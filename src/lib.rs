pub mod ast;
pub mod codegen;
pub mod crate_builder;
pub mod diag;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod passes;
pub mod source;

use std::fmt;

use ast::Module;
use diag::Diagnostic;
use ir::lowered::RustModule;
use ir::typed::TypedModule;

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub typed: TypedModule,
    pub lowered: RustModule,
    pub rust_code: String,
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub diagnostics: Vec<Diagnostic>,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, diagnostic) in self.diagnostics.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            write!(f, "{diagnostic}")?;
        }
        Ok(())
    }
}

pub fn compile_source(source: &str) -> Result<CompilerOutput, CompileError> {
    let tokens = lexer::lex(source).map_err(|diagnostics| CompileError { diagnostics })?;
    let module = parser::parse_module(tokens).map_err(|diagnostics| CompileError { diagnostics })?;
    compile_ast(&module)
}

pub fn compile_ast(module: &Module) -> Result<CompilerOutput, CompileError> {
    let typed = passes::lower_to_typed(module).map_err(|diagnostics| CompileError { diagnostics })?;
    let lowered = passes::lower_to_rust(&typed);
    let rust_code = codegen::emit_rust_module(&lowered);

    Ok(CompilerOutput {
        typed,
        lowered,
        rust_code,
    })
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::fs;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::compile_source;

    #[test]
    fn compile_smoke_test() {
        let source = r#"
            pub struct Point { x: i64; y: i64; }

            pub fn id(v: i64) -> i64 {
                return v;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub struct Point"));
        assert!(output.rust_code.contains("pub fn id"));
    }

    #[test]
    fn compile_supports_result_try_and_rust_use_imports() {
        let source = r#"
            rust use std::num::ParseIntError;

            fn parse_i64(input: String) -> Result<i64, ParseIntError> {
                return Result::Ok(1);
            }

            fn parse_value(input: String) -> Result<i64, ParseIntError> {
                const value = parse_i64(input)?;
                return Result::Ok(value);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("use std::num::ParseIntError;"));
        assert!(output.rust_code.contains("parse_i64(input)?"));
    }

    #[test]
    fn compile_rejects_option_try_outside_option_return() {
        let source = r#"
            fn bad(v: Option<i64>) -> i64 {
                return v?;
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("The `?` operator on Option requires the function to return Option")
        );
    }

    #[test]
    fn compile_infers_return_type_when_annotation_is_missing() {
        let source = r#"
            pub fn id(v: i64) {
                return v;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub fn id(v: i64) -> i64"));
    }

    #[test]
    fn compile_rejects_try_when_return_type_is_not_declared() {
        let source = r#"
            fn parse_i64(input: String) -> Result<i64, String> {
                return Result::Ok(1);
            }

            fn read(input: String) {
                return parse_i64(input)?;
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Functions using `?` must declare an Option/Result return type")
        );
    }

    #[test]
    fn compile_allows_external_rust_path_calls() {
        let source = r#"
            rust use std::mem::drop;

            fn cleanup(value: String) {
                std::mem::drop(value);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("use std::mem::drop;"));
        assert!(output.rust_code.contains("std::mem::drop(value);"));
    }

    #[test]
    fn compile_inserts_clone_for_reused_string_call_argument() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                consume(text);
                consume(text);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("consume(text.clone());"));
        assert!(output.rust_code.contains("consume(text);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_keeps_copy_types_without_clone_in_call_arguments() {
        let source = r#"
            fn use_i64(v: i64) {
                std::mem::drop(v);
                return;
            }

            fn demo(x: i64) {
                use_i64(x);
                use_i64(x);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(!output.rust_code.contains("use_i64(x.clone());"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_for_str_len_calls() {
        let source = r#"
            fn demo(text: String) {
                std::mem::drop(str::len(text));
                std::mem::drop(str::len(text));
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("str::len(&text)"));
        assert!(!output.rust_code.contains("str::len(text)"));
        assert!(!output.rust_code.contains("text.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_borrow_then_consume_keeps_final_move() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                std::mem::drop(str::len(text));
                consume(text);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("str::len(&text)"));
        assert!(output.rust_code.contains("consume(text);"));
        assert!(!output.rust_code.contains("consume(text.clone());"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_rejects_unknown_inferred_return() {
        let source = r#"
            fn maybe_value() {
                return None;
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("return type could not be fully inferred")
        );
    }

    #[test]
    fn generated_rust_compiles_for_result_try_flow() {
        let source = r#"
            rust use std::num::ParseIntError;

            fn parse_i64(input: String) -> Result<i64, ParseIntError> {
                return Result::Ok(1);
            }

            fn parse_value(input: String) -> Result<i64, ParseIntError> {
                const parsed = parse_i64(input)?;
                std::mem::drop(parsed);
                return Result::Ok(1);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_match_on_enums() {
        let source = r#"
            enum Maybe {
                Some(i64);
                None;
            }

            fn unwrap_or_zero(value: Maybe) -> i64 {
                return match value {
                    Maybe::Some(inner) => inner;
                    Maybe::None => 0;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match value"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_visibility_if_and_while() {
        let source = r#"
            pub fn choose(flag: bool) -> i64 {
                if flag {
                    return 1;
                } else {
                    return 0;
                }
            }

            fn spin(flag: bool) -> i64 {
                while flag {
                    return 1;
                }
                return 0;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub fn choose"));
        assert!(output.rust_code.contains("\nfn spin("));
        assert!(output.rust_code.contains("if flag"));
        assert!(output.rust_code.contains("while flag"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_impl_methods() {
        let source = r#"
            pub struct Point { x: i64; }

            impl Point {
                pub fn get_x(p: Point) -> i64 {
                    return p.x;
                }
            }

            pub fn read_x(p: Point) -> i64 {
                return Point::get_x(p);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("impl Point"));
        assert!(output.rust_code.contains("pub fn get_x"));
        assert!(output.rust_code.contains("Point::get_x(p)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_boolean_comparisons_and_tail_return() {
        let source = r#"
            pub fn check(a: bool, b: bool, x: i64, y: i64) -> bool {
                (not a and b) or (x <= y and x != y)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("(!a && b)"));
        assert!(output.rust_code.contains("(x <= y)"));
        assert!(output.rust_code.contains("(x != y)"));
        assert!(output.rust_code.contains("return"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_ranges_and_tuple_destructure() {
        let source = r#"
            pub fn f() -> i64 {
                const (a, b) = (1, 2);
                const r1 = a..b;
                const r2 = a..=b;
                const r3 = ..b;
                const r4 = a..;
                std::mem::drop(r1);
                std::mem::drop(r2);
                std::mem::drop(r3);
                std::mem::drop(r4);
                a
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("let (a, b) = (1, 2);"));
        assert!(output.rust_code.contains("a..b"));
        assert!(output.rust_code.contains("a..=b"));
        assert!(output.rust_code.contains("..b"));
        assert!(output.rust_code.contains("a.."));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_advanced_match_patterns() {
        let source = r#"
            enum Maybe {
                Some(i64);
                None;
            }

            pub fn choose(a: i64, b: i64, m: Maybe) -> i64 {
                const from_tuple = match (a, b) {
                    (0, n) => n;
                    (x, 0) => x;
                    _ => a;
                };

                return match m {
                    Maybe::Some(0) => from_tuple;
                    Maybe::Some(v) => v;
                    Maybe::None => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match (a, b)"));
        assert!(output.rust_code.contains("Maybe::Some(0)"));
        assert!(output.rust_code.contains("Maybe::Some(v)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_closures_and_closure_calls() {
        let source = r#"
            pub fn use_closure(x: i64) -> i64 {
                const pick = |y: i64| -> i64 { x };
                const id = |z: i64| -> i64 { z };
                const from_literal = (|k: i64| -> i64 { k })(x);
                return id(pick(from_literal));
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("let pick = |y: i64| -> i64"));
        assert!(output.rust_code.contains("let id = |z: i64| -> i64"));
        assert!(output.rust_code.contains("(|k: i64| -> i64"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_comments_and_raw_multiline_strings() {
        let source = r##"
            /* docs-like block comment */
            pub fn banner() -> String {
                // runtime banner text
                const msg = r#"hello
"elevate"
world"#;
                return msg;
            }
        "##;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub fn banner() -> String"));
        assert!(output.rust_code.contains("\\n"));
        assert!(output.rust_code.contains("\\\"elevate\\\""));
        assert_rust_code_compiles(&output.rust_code);
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
        let base = env::temp_dir().join(format!("elevate-test-{nanos}"));
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
}
