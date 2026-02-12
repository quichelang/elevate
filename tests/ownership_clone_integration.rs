use elevate::compile_source;
use std::env;
use std::fs;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

struct CloneCase<'a> {
    name: &'a str,
    source: &'a str,
    required_snippets: &'a [&'a str],
    forbidden_snippets: &'a [&'a str],
}

#[test]
fn integration_clone_regressions_cover_nested_and_convoluted_ownership_paths() {
    let cases = [
        CloneCase {
            name: "reused owned string argument",
            source: r#"
                fn consume_text(text: String) -> i64 {
                    std::mem::drop(text);
                    return 1;
                }

                fn run(text: String) -> i64 {
                    const first = consume_text(text);
                    const second = consume_text(text);
                    return first + second;
                }
            "#,
            required_snippets: &["consume_text(text.clone())", "consume_text(text)"],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "deep nested struct field reused",
            source: r#"
                pub struct Layer3 { word: String, }
                pub struct Layer2 { inner: Layer3, }
                pub struct Layer1 { inner: Layer2, }

                fn consume_text(text: String) -> i64 {
                    std::mem::drop(text);
                    return 1;
                }

                fn run(state: Layer1) -> i64 {
                    const first = consume_text(state.inner.inner.word);
                    const second = consume_text(state.inner.inner.word);
                    return first + second;
                }
            "#,
            required_snippets: &[
                "consume_text(state.inner.inner.word.clone())",
                "consume_text(state.inner.inner.word)",
            ],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "nested vec tuple payload reused",
            source: r#"
                fn consume_pairs(pairs: Vec<(String, String)>) -> i64 {
                    std::mem::drop(pairs);
                    return 1;
                }

                fn run(pairs: Vec<(String, String)>) -> i64 {
                    const first = consume_pairs(pairs);
                    const second = consume_pairs(pairs);
                    return first + second;
                }
            "#,
            required_snippets: &["consume_pairs(pairs.clone())", "consume_pairs(pairs)"],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "for loop consumes iterable then value reused",
            source: r#"
                fn consume_words(words: Vec<String>) -> i64 {
                    std::mem::drop(words);
                    return 1;
                }

                fn run(words: Vec<String>) -> i64 {
                    for word in words {
                        std::mem::drop(word);
                    }
                    return consume_words(words);
                }
            "#,
            required_snippets: &["for word in words.clone()", "consume_words(words)"],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "hashmap reused in two owned calls",
            source: r#"
                use std::collections::HashMap;

                fn consume_map(map: HashMap<String, i64>) -> i64 {
                    std::mem::drop(map);
                    return 1;
                }

                fn run(map: HashMap<String, i64>) -> i64 {
                    const first = consume_map(map);
                    const second = consume_map(map);
                    return first + second;
                }
            "#,
            required_snippets: &["consume_map(map.clone())", "consume_map(map)"],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "branching owned calls with post-branch reuse",
            source: r#"
                fn consume_text(text: String) -> i64 {
                    std::mem::drop(text);
                    return 1;
                }

                fn run(flag: bool, text: String) -> i64 {
                    if flag {
                        std::mem::drop(consume_text(text));
                    } else {
                        std::mem::drop(consume_text(text));
                    }
                    return consume_text(text);
                }
            "#,
            required_snippets: &[
                "std::mem::drop(consume_text(text.clone()))",
                "return consume_text(text);",
            ],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "nested vec struct reused in helper chain",
            source: r#"
                pub struct Cell { token: String, }

                fn consume_cells(cells: Vec<Cell>) -> i64 {
                    std::mem::drop(cells);
                    return 1;
                }

                fn fanout(cells: Vec<Cell>) -> i64 {
                    const left = consume_cells(cells);
                    const right = consume_cells(cells);
                    return left + right;
                }

                fn run(cells: Vec<Cell>) -> i64 {
                    return fanout(cells) + fanout(cells);
                }
            "#,
            required_snippets: &[
                "consume_cells(cells.clone())",
                "fanout(cells.clone()) + fanout(cells)",
            ],
            forbidden_snippets: &[],
        },
        CloneCase {
            name: "deep path element in loop body reused after loop",
            source: r#"
                pub struct Inner { values: Vec<String>, }
                pub struct Outer { inner: Inner, }

                fn consume_words(words: Vec<String>) -> i64 {
                    std::mem::drop(words);
                    return 1;
                }

                fn run(outer: Outer) -> i64 {
                    for word in outer.inner.values {
                        std::mem::drop(word);
                    }
                    return consume_words(outer.inner.values);
                }
            "#,
            required_snippets: &[
                "for word in outer.inner.values.clone()",
                "return consume_words(outer.inner.values);",
            ],
            forbidden_snippets: &[],
        },
    ];

    for case in cases {
        let output = compile_source(case.source)
            .unwrap_or_else(|err| panic!("case `{}` failed to compile: {err}", case.name));
        for snippet in case.required_snippets {
            assert!(
                output.rust_code.contains(snippet),
                "case `{}` missing required snippet `{}`\n{}",
                case.name,
                snippet,
                output.rust_code
            );
        }
        for snippet in case.forbidden_snippets {
            assert!(
                !output.rust_code.contains(snippet),
                "case `{}` contained forbidden snippet `{}`\n{}",
                case.name,
                snippet,
                output.rust_code
            );
        }
        assert!(
            !output.ownership_notes.is_empty(),
            "case `{}` should emit ownership notes when clone insertion is required",
            case.name
        );
        assert_rust_code_compiles(&output.rust_code);
    }
}

fn assert_rust_code_compiles(code: &str) {
    let rustc_available = Command::new("rustc").arg("--version").output().is_ok();
    if !rustc_available {
        return;
    }

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time must be after epoch")
        .as_nanos();
    let base = env::temp_dir().join(format!("elevate-integration-test-{nanos}"));
    fs::create_dir_all(&base).expect("temp dir create should succeed");
    let source_path = base.join("generated.rs");
    let output_path = base.join("generated.rlib");

    fs::write(&source_path, code).expect("write generated source should succeed");
    let result = Command::new("rustc")
        .arg("--crate-type=lib")
        .arg("--edition=2024")
        .arg(&source_path)
        .arg("-o")
        .arg(&output_path)
        .output()
        .expect("rustc invocation should run");

    if !result.status.success() {
        let stderr = String::from_utf8_lossy(&result.stderr);
        panic!("generated Rust failed to compile:\n{stderr}");
    }
}
