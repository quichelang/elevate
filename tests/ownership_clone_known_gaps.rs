use elevate::compile_source;

// These tests encode desired ownership/clone behavior for scenarios that are
// currently brittle or not fully implemented. They are intentionally ignored so
// CI stays green while we track compiler hardening work.

#[test]
#[ignore = "known gap: closure capture + repeated owned consumption"]
fn gap_closure_capture_reuse_should_clone_captured_vec() {
    let source = r#"
        fn consume_words(words: Vec<String>) -> i64 {
            std::mem::drop(words);
            return 1;
        }

        fn run(words: Vec<String>) -> i64 {
            const eval = || -> i64 {
                return consume_words(words);
            };
            const first = eval();
            const second = consume_words(words);
            return first + second;
        }
    "#;

    let output = compile_source(source).expect("should compile once closure capture cloning is fixed");
    assert!(output.rust_code.contains("consume_words(words.clone())"));
}

#[test]
#[ignore = "known gap: nested match + consuming calls should preserve post-match reuse"]
fn gap_match_arms_then_post_match_reuse_should_clone_once() {
    let source = r#"
        fn consume_text(text: String) -> i64 {
            std::mem::drop(text);
            return 1;
        }

        fn run(flag: bool, text: String) -> i64 {
            match flag {
                true => {
                    std::mem::drop(consume_text(text));
                }
                false => {
                    std::mem::drop(consume_text(text));
                }
            }
            return consume_text(text);
        }
    "#;

    let output = compile_source(source).expect("should compile with arm-aware clone planning");
    assert!(output.rust_code.contains("consume_text(text.clone())"));
    assert!(output.rust_code.contains("return consume_text(text);"));
}

#[test]
#[ignore = "known gap: for-loop over nested projection in helper closure"]
fn gap_loop_projection_capture_should_clone_projection_root() {
    let source = r#"
        pub struct Inner { values: Vec<String>; }
        pub struct Outer { inner: Inner; }

        fn consume_words(words: Vec<String>) -> i64 {
            std::mem::drop(words);
            return 1;
        }

        fn run(outer: Outer) -> i64 {
            const walk = || -> i64 {
                for word in outer.inner.values {
                    std::mem::drop(word);
                }
                return 0;
            };
            std::mem::drop(walk());
            return consume_words(outer.inner.values);
        }
    "#;

    let output = compile_source(source).expect("should compile once projection capture is clone-safe");
    assert!(output.rust_code.contains("for word in outer.inner.values.clone()"));
    assert!(output.rust_code.contains("consume_words(outer.inner.values)"));
}

#[test]
#[ignore = "known gap: nested container through helper return path"]
fn gap_nested_container_return_chain_should_clone_at_call_site() {
    let source = r#"
        pub struct Deck { words: Vec<Vec<String>>; }

        fn consume_deck(words: Vec<Vec<String>>) -> i64 {
            std::mem::drop(words);
            return 1;
        }

        fn pass(words: Vec<Vec<String>>) -> i64 {
            return consume_deck(words);
        }

        fn run(deck: Deck) -> i64 {
            const a = pass(deck.words);
            const b = pass(deck.words);
            return a + b;
        }
    "#;

    let output = compile_source(source).expect("should compile with nested return-chain clone planning");
    assert!(output.rust_code.contains("pass(deck.words.clone())"));
    assert!(output.rust_code.contains("pass(deck.words)"));
}

#[test]
#[ignore = "known gap: tuple-destructure binding reused with post-destructure ownership move"]
fn gap_tuple_destructure_then_reuse_should_insert_clone() {
    let source = r#"
        fn consume_pair(pair: (String, String)) -> i64 {
            std::mem::drop(pair);
            return 1;
        }

        fn run(pair: (String, String)) -> i64 {
            const (left, right) = pair;
            std::mem::drop(left);
            std::mem::drop(right);
            return consume_pair(pair);
        }
    "#;

    let output = compile_source(source).expect("should compile with tuple-destructure-aware clone insertion");
    assert!(output.rust_code.contains("let (left, right) = pair.clone();"));
    assert!(output.rust_code.contains("consume_pair(pair)"));
}

#[test]
#[ignore = "known gap: map iteration via keys with later owned consumption"]
fn gap_map_keys_iteration_then_owned_call_should_clone_map() {
    let source = r#"
        use std::collections::HashMap;

        fn consume_map(map: HashMap<String, i64>) -> i64 {
            std::mem::drop(map);
            return 1;
        }

        fn run(map: HashMap<String, i64>) -> i64 {
            for key in map.keys() {
                std::mem::drop(key);
            }
            return consume_map(map);
        }
    "#;

    let output = compile_source(source).expect("should compile with iterator/ownership reconciliation");
    assert!(output.rust_code.contains("for key in map.keys()"));
    assert!(output.rust_code.contains("consume_map(map)"));
}
