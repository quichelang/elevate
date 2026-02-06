pub mod ast;
pub mod codegen;
pub mod crate_builder;
pub mod diag;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod passes;
pub mod source;
pub mod test_runner;

use std::fmt;

use ast::Module;
use diag::Diagnostic;
use ir::lowered::RustModule;
use ir::typed::TypedModule;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExperimentFlags {
    pub move_mut_args: bool,
    pub infer_local_bidi: bool,
    pub effect_rows_internal: bool,
    pub infer_principal_fallback: bool,
}

impl ExperimentFlags {
    fn active_names(&self) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.move_mut_args {
            out.push("exp_move_mut_args");
        }
        if self.infer_local_bidi {
            out.push("exp_infer_local_bidi");
        }
        if self.effect_rows_internal {
            out.push("exp_effect_rows_internal");
        }
        if self.infer_principal_fallback {
            out.push("exp_infer_principal_fallback");
        }
        out
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CompileOptions {
    pub experiments: ExperimentFlags,
}

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub typed: TypedModule,
    pub lowered: RustModule,
    pub rust_code: String,
    pub ownership_notes: Vec<String>,
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
    compile_source_with_options(source, &CompileOptions::default())
}

pub fn compile_source_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<CompilerOutput, CompileError> {
    let tokens = lexer::lex(source).map_err(|diagnostics| CompileError { diagnostics })?;
    let module =
        parser::parse_module(tokens).map_err(|diagnostics| CompileError { diagnostics })?;
    compile_ast_with_options(&module, options)
}

pub fn compile_ast(module: &Module) -> Result<CompilerOutput, CompileError> {
    compile_ast_with_options(module, &CompileOptions::default())
}

pub fn compile_ast_with_options(
    module: &Module,
    options: &CompileOptions,
) -> Result<CompilerOutput, CompileError> {
    let typed =
        passes::lower_to_typed(module).map_err(|diagnostics| CompileError { diagnostics })?;
    let mut lowered = passes::lower_to_rust(&typed);
    for name in options.experiments.active_names() {
        lowered
            .ownership_notes
            .push(format!("experimental flag enabled: {name}"));
    }
    let ownership_notes = lowered.ownership_notes.clone();
    let rust_code = codegen::emit_rust_module(&lowered);

    Ok(CompilerOutput {
        typed,
        lowered,
        rust_code,
        ownership_notes,
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
    fn compile_assignment_and_add_assign_statements() {
        let source = r#"
            struct Counter { value: i64; }

            fn tick(n: i64, counter: Counter) -> i64 {
                n += 1;
                counter.value = n + 1;
                return counter.value;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("n += 1;"));
        assert!(output.rust_code.contains("counter.value = (n + 1);"));
    }

    #[test]
    fn compile_supports_assert_functions_without_macro_syntax() {
        let source = r#"
            fn test_asserts() {
                assert(true);
                assert_eq(1 + 1, 2);
                assert_ne(1, 2);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("assert!(true);"));
        assert!(output.rust_code.contains("assert_eq!((1 + 1), 2);"));
        assert!(output.rust_code.contains("assert_ne!(1, 2);"));
        assert_rust_code_compiles(&output.rust_code);
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
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("auto-clone inserted"))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_clones_reused_user_defined_types() {
        let source = r#"
            pub struct Packet { id: i64; }

            fn consume(packet: Packet) {
                std::mem::drop(packet);
                return;
            }

            fn demo(packet: Packet) {
                consume(packet);
                consume(packet);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("consume(packet.clone());"));
        assert!(output.rust_code.contains("consume(packet);"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`packet` of type `Packet`"))
        );
        assert!(
            output
                .rust_code
                .contains("// ownership-note: auto-clone inserted")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_clones_reused_imported_nominal_types() {
        let source = r#"
            rust use external::Token;

            fn consume(token: Token) {
                std::mem::drop(token);
                return;
            }

            fn demo(token: Token) {
                consume(token);
                consume(token);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("consume(token.clone());"));
        assert!(output.rust_code.contains("consume(token);"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`token` of type `Token`"))
        );
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
        assert!(output.rust_code.contains("__elevate_shim_str_len(&text)"));
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_len(__arg0: &str) -> usize")
        );
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
        assert!(output.rust_code.contains("__elevate_shim_str_len(&text)"));
        assert!(output.rust_code.contains("consume(text);"));
        assert!(!output.rust_code.contains("consume(text.clone());"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_clones_reused_method_receiver_for_owned_calls() {
        let source = r#"
            fn demo(text: String) {
                std::mem::drop(text.into_bytes());
                std::mem::drop(text.into_bytes());
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("text.clone().into_bytes()"));
        assert!(output.rust_code.contains("text.into_bytes()"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`text` of type `String`"))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_method_call_arguments_for_string_contains() {
        let source = r#"
            fn demo(text: String, needle: String) -> bool {
                return text.contains(needle);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("text.contains(&needle)"));
        assert!(!output.rust_code.contains("needle.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_keeps_borrowed_method_receiver_without_clone() {
        let source = r#"
            fn demo(text: String) {
                std::mem::drop(text.len());
                std::mem::drop(text.len());
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(!output.rust_code.contains("text.clone().len()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_for_str_contains_calls() {
        let source = r#"
            fn demo(text: String, needle: String) {
                std::mem::drop(str::contains(text, needle));
                std::mem::drop(str::contains(text, needle));
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("__elevate_shim_str_contains(&text, &needle)")
        );
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_contains(__arg0: &str, __arg1: &str) -> bool")
        );
        assert!(!output.rust_code.contains("str::contains(text, needle)"));
        assert!(!output.rust_code.contains("text.clone()"));
        assert!(!output.rust_code.contains("needle.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_for_str_prefix_suffix_calls() {
        let source = r#"
            fn demo(text: String, prefix: String, suffix: String) -> bool {
                std::mem::drop(str::starts_with(text, prefix));
                return str::ends_with(text, suffix);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("__elevate_shim_str_starts_with(&text, &prefix)")
        );
        assert!(
            output
                .rust_code
                .contains("__elevate_shim_str_ends_with(&text, &suffix)")
        );
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_starts_with(__arg0: &str, __arg1: &str) -> bool")
        );
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_ends_with(__arg0: &str, __arg1: &str) -> bool")
        );
        assert!(!output.rust_code.contains("text.clone()"));
        assert!(!output.rust_code.contains("prefix.clone()"));
        assert!(!output.rust_code.contains("suffix.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_string_known_shims_with_owned_returns() {
        let source = r#"
            fn demo(text: String) -> String {
                const rest = str::strip_prefix_known(text, "--");
                std::mem::drop(str::split_once_known(rest, "="));
                return rest;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("__elevate_shim_str_strip_prefix_known(&text")
        );
        assert!(
            output
                .rust_code
                .contains("__elevate_shim_str_split_once_known(&rest")
        );
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_strip_prefix_known(__arg0: &str, __arg1: &str) -> String")
        );
        assert!(output.rust_code.contains("str::strip_prefix(__arg0, __arg1)"));
        assert!(output.rust_code.contains(".unwrap().to_string()"));
        assert!(
            output
                .rust_code
                .contains("fn __elevate_shim_str_split_once_known(__arg0: &str, __arg1: &str) -> (String, String)")
        );
        assert!(output.rust_code.contains("str::split_once(__arg0, __arg1)"));
        assert!(output.rust_code.contains("let (__left, __right) ="));
        assert!(!output.rust_code.contains("text.clone()"));
        assert!(!output.rust_code.contains("rest.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_string_option_result_associated_calls() {
        let source = r#"
            fn demo(text: String, v: Option<String>, r: Result<String, String>) -> bool {
                std::mem::drop(String::len(text));
                std::mem::drop(Option::is_some(v));
                std::mem::drop(Result::is_ok(r));
                return String::is_empty(text) and Option::is_none(v) and Result::is_err(r);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("String::len(&text)"));
        assert!(output.rust_code.contains("Option::is_some(&v)"));
        assert!(output.rust_code.contains("Result::is_ok(&r)"));
        assert!(output.rust_code.contains("String::is_empty(&text)"));
        assert!(output.rust_code.contains("Option::is_none(&v)"));
        assert!(output.rust_code.contains("Result::is_err(&r)"));
        assert!(!output.rust_code.contains("text.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_vec_associated_calls() {
        let source = r#"
            fn demo(values: Vec<i64>) -> bool {
                std::mem::drop(Vec::len(values));
                return Vec::is_empty(values);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Vec::len(&values)"));
        assert!(output.rust_code.contains("Vec::is_empty(&values)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_hashmap_btreemap_associated_calls() {
        let source = r#"
            rust use std::collections::HashMap;
            rust use std::collections::BTreeMap;

            fn demo(h: HashMap<String, i64>, b: BTreeMap<String, i64>, key: String) -> bool {
                std::mem::drop(HashMap::len(h));
                std::mem::drop(BTreeMap::is_empty(b));
                return HashMap::contains_key(h, key) or BTreeMap::contains_key(b, key);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("HashMap::len(&h)"));
        assert!(output.rust_code.contains("BTreeMap::is_empty(&b)"));
        assert!(output.rust_code.contains("HashMap::contains_key(&h, &key)"));
        assert!(
            output
                .rust_code
                .contains("BTreeMap::contains_key(&b, &key)")
        );
        assert!(!output.rust_code.contains("key.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_borrows_hashset_btreeset_contains_calls() {
        let source = r#"
            rust use std::collections::HashSet;
            rust use std::collections::BTreeSet;

            fn demo(hs: HashSet<String>, bs: BTreeSet<String>, key: String) -> bool {
                std::mem::drop(HashSet::contains(hs, key));
                return BTreeSet::contains(bs, key);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("HashSet::contains(&hs, &key)"));
        assert!(output.rust_code.contains("BTreeSet::contains(&bs, &key)"));
        assert!(!output.rust_code.contains("key.clone()"));
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
    fn compile_supports_generic_function_inference() {
        let source = r#"
            fn id<T>(value: T) -> T {
                value
            }

            fn run() -> i64 {
                id(7)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("fn id<T>(value: T) -> T"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_generic_reuse_mismatch() {
        let source = r#"
            fn same<T>(left: T, right: T) -> T {
                left
            }

            fn bad() -> i64 {
                same(1, "oops")
            }
        "#;

        let error = compile_source(source).expect_err("expected generic mismatch");
        assert!(
            error
                .to_string()
                .contains("Arg 2 for `same`: expected `T`, got `String`")
        );
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
    fn compile_supports_multi_payload_enum_variants() {
        let source = r#"
            enum PairOrNone {
                Pair(i64, i64);
                None;
            }

            fn make_pair(a: i64, b: i64) -> PairOrNone {
                PairOrNone::Pair(a, b)
            }

            fn sum_or_zero(value: PairOrNone) -> i64 {
                return match value {
                    PairOrNone::Pair(left, right) => left + right;
                    PairOrNone::None => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Pair(i64, i64)"));
        assert!(output.rust_code.contains("PairOrNone::Pair(left, right)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_multi_payload_enum_arity_mismatch() {
        let source = r#"
            enum Pair {
                Two(i64, i64);
            }

            fn bad() -> Pair {
                Pair::Two(1)
            }
        "#;

        let error = compile_source(source).expect_err("expected arity error");
        assert!(
            error
                .to_string()
                .contains("Enum variant `Pair::Two` expects 2 argument(s), got 1")
        );
    }

    #[test]
    fn compile_supports_match_range_patterns() {
        let source = r#"
            fn classify(v: i64) -> i64 {
                return match v {
                    0..10 => 1;
                    10..=20 => 2;
                    ..0 => 3;
                    _ => 4;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("0..10"));
        assert!(output.rust_code.contains("10..=20"));
        assert!(output.rust_code.contains("..0"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_match_binding_at_patterns() {
        let source = r#"
            fn classify(v: i64) -> i64 {
                return match v {
                    n @ 0..=10 => n;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("n @ 0..=10"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_match_struct_patterns() {
        let source = r#"
            struct Point { x: i64; y: i64; }
            fn classify(p: Point) -> i64 {
                return match p {
                    Point { x, y: 0 } => x;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Point { x, y: 0 }"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_match_struct_rest_patterns() {
        let source = r#"
            struct Point { x: i64; y: i64; z: i64; }
            fn classify(p: Point) -> i64 {
                return match p {
                    Point { x, .. } => x;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Point { x, .. }"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_match_struct_missing_fields_without_rest() {
        let source = r#"
            struct Point { x: i64; y: i64; }
            fn classify(p: Point) -> i64 {
                return match p {
                    Point { x } => x;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Struct pattern `Point` is missing field(s): y")
        );
    }

    #[test]
    fn compile_supports_match_slice_rest_patterns() {
        let source = r#"
            fn classify(v: Vec<i64>) -> i64 {
                return match v {
                    [head, ..tail] => head;
                    [single] => single;
                    [] => 0;
                    _ => 1;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("[head, tail @ ..]"));
        assert!(output.rust_code.contains("[single]"));
        assert!(output.rust_code.contains("[]"));
    }

    #[test]
    fn compile_reports_non_exhaustive_bool_match() {
        let source = r#"
            fn classify(flag: bool) -> i64 {
                return match flag {
                    true => 1;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(error.to_string().contains("Non-exhaustive match on `bool`"));
    }

    #[test]
    fn compile_reports_non_exhaustive_bool_tuple_match() {
        let source = r#"
            fn classify() -> i64 {
                const flags = (true, false);
                return match flags {
                    (true, true) => 1;
                    (false, false) => 0;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(error.to_string().contains("Non-exhaustive match on tuple bool pattern"));
        assert!(error.to_string().contains("(true, false)"));
        assert!(error.to_string().contains("(false, true)"));
    }

    #[test]
    fn compile_accepts_exhaustive_bool_tuple_match() {
        let source = r#"
            fn classify() -> i64 {
                const flags = (true, false);
                return match flags {
                    (true, true) => 3;
                    (true, false) => 2;
                    (false, true) => 1;
                    (false, false) => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match flags"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_non_exhaustive_enum_match() {
        let source = r#"
            enum Maybe {
                Some(i64);
                None;
            }

            fn classify(v: Maybe) -> i64 {
                return match v {
                    Maybe::Some(_) => 1;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(error.to_string().contains("Non-exhaustive match on `Maybe`"));
        assert!(error.to_string().contains("None"));
    }

    #[test]
    fn compile_reports_non_exhaustive_finite_tuple_match() {
        let source = r#"
            enum Switch {
                Left;
                Right;
            }

            fn classify() -> i64 {
                const input = (false, Switch::Left);
                return match input {
                    (true, Switch::Left) => 1;
                    (true, Switch::Right) => 2;
                    (false, Switch::Right) => 3;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(error.to_string().contains("Non-exhaustive match on finite tuple domain"));
        assert!(error.to_string().contains("(false, Switch::Left)"));
    }

    #[test]
    fn compile_accepts_exhaustive_finite_tuple_match() {
        let source = r#"
            enum Switch {
                Left;
                Right;
            }

            fn classify() -> i64 {
                const input = (false, Switch::Left);
                return match input {
                    (true, Switch::Left) => 1;
                    (true, Switch::Right) => 2;
                    (false, Switch::Right) => 3;
                    (false, Switch::Left) => 4;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match input"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_skips_finite_tuple_exhaustiveness_for_payload_enums() {
        let source = r#"
            enum Maybe {
                Some(i64);
                None;
            }

            fn classify() -> i64 {
                const input = (false, Maybe::None);
                return match input {
                    (true, Maybe::Some(_)) => 1;
                    (true, Maybe::None) => 2;
                    (false, Maybe::Some(_)) => 3;
                    _ => 4;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match input"));
    }

    #[test]
    fn compile_emits_multi_value_variant_payload_patterns() {
        let source = r#"
            rust use crate::Pair;

            fn classify(value: Pair) -> i64 {
                return match value {
                    Pair::Both(left, right) => left;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Pair::Both(left, right)"));
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
    fn compile_supports_loop_break_continue() {
        let source = r#"
            fn spin(flag: bool) -> i64 {
                loop {
                    if flag {
                        break;
                    }
                    continue;
                }
                return 0;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("loop {"));
        assert!(output.rust_code.contains("break;"));
        assert!(output.rust_code.contains("continue;"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_in_loops() {
        let source = r#"
            fn drive(n: i64) -> i64 {
                for i in 0..n {
                    std::mem::drop(i);
                }
                return n;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for i in 0..n"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_tuple_destructure_bindings() {
        let source = r#"
            rust use crate::PairIter;

            fn sum_pairs(pairs: PairIter) -> i64 {
                for (left, right) in pairs {
                    std::mem::drop(left);
                    std::mem::drop(right);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for (left, right) in pairs"));
    }

    #[test]
    fn compile_supports_slice_destructure_bindings() {
        let source = r#"
            fn head(values: Vec<i64>) -> i64 {
                const [first, ..rest] = values;
                std::mem::drop(rest);
                first
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("let [first, rest @ ..] = values.as_slice() else")
        );
        assert!(output.rust_code.contains("let first = (*first).clone();"));
        assert!(output.rust_code.contains("let rest = rest.to_vec();"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_slice_destructure_bindings() {
        let source = r#"
            fn drive() -> i64 {
                const out = 0;
                for [left, right] in [[1, 2], [3, 4]] {
                    std::mem::drop(left);
                    std::mem::drop(right);
                }
                out
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for __item in vec![vec![1, 2], vec![3, 4]]"));
        assert!(
            output
                .rust_code
                .contains("let [left, right] = __item.as_slice() else")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_slice_destructure_type_mismatch() {
        let source = r#"
            fn bad(v: i64) -> i64 {
                const [a, b] = v;
                a
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(error.to_string().contains("Slice destructure requires Vec value"));
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
    fn compile_supports_impl_methods_on_enums() {
        let source = r#"
            pub enum Status {
                Ok;
                Err(String);
            }

            impl Status {
                pub fn is_ok(value: Status) -> bool {
                    return match value {
                        Status::Ok => true;
                        Status::Err(_) => false;
                    };
                }
            }

            pub fn check(value: Status) -> bool {
                return Status::is_ok(value);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("impl Status"));
        assert!(output.rust_code.contains("pub fn is_ok"));
        assert!(output.rust_code.contains("Status::is_ok(value)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_impl_self_param_and_infers_mut_self() {
        let source = r#"
            pub struct Point { x: i64; }

            impl Point {
                pub fn bump(self, n: i64) -> Point {
                    self.x += n;
                    self
                }
            }

            pub fn run(p: Point) -> Point {
                Point::bump(p, 1)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("impl Point"));
        assert!(output.rust_code.contains("pub fn bump(mut self: Point, n: i64) -> Point"));
        assert!(output.rust_code.contains("self.x += n;"));
        assert!(output.rust_code.contains("Point::bump(p, 1)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_impl_self_return_alias() {
        let source = r#"
            pub struct Point { x: i64; }

            impl Point {
                pub fn new(x: i64) -> Self {
                    Point { x: x; }
                }
            }

            pub fn make() -> Point {
                Point::new(7)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub fn new(x: i64) -> Point"));
        assert!(output.rust_code.contains("Point::new(7)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_struct_literals() {
        let source = r#"
            pub struct Pair { left: i64; right: i64; }

            fn make_pair(v: i64) -> Pair {
                return Pair {
                    left: v;
                    right: v;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Pair { left: v, right: v }"));
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
    fn compile_supports_vec_index_expressions() {
        let source = r#"
            pub fn first(values: Vec<i64>) -> i64 {
                values[0]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("values[(0) as usize]"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_array_literals() {
        let source = r#"
            pub fn values() -> Vec<i64> {
                [1, 2, 3]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("vec![1, 2, 3]"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_heterogeneous_tuple_type_annotations() {
        let source = r#"
            pub fn pair(left: i64, right: String) -> (i64, String) {
                (left, right)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("-> (i64, String)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_emits_vec_slice_range_expressions() {
        let source = r#"
            pub fn view(values: Vec<i64>) -> i64 {
                const middle = values[1..3];
                std::mem::drop(middle);
                values[0]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("values[1..3]"));
    }

    #[test]
    fn compile_supports_tuple_assignment_targets() {
        let source = r#"
            pub fn swap(a: i64, b: i64) -> i64 {
                (a, b) = (b, a);
                a
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("(a, b) = (b, a);"));
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
    fn compile_supports_match_arm_block_expressions() {
        let source = r#"
            fn classify(v: i64) -> i64 {
                return match v {
                    0 => { 1 };
                    _ => { 2 };
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match v"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_match_or_patterns_and_guards() {
        let source = r#"
            fn classify(v: i64) -> i64 {
                return match v {
                    0 | 1 if v == 1 => 10;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("0 | 1 if (v == 1) => 10"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_matching_imported_rust_enums() {
        let source = r#"
            rust use std::cmp::Ordering;

            fn classify(v: Ordering) -> i64 {
                return match v {
                    Ordering::Less => 0;
                    Ordering::Equal => 1;
                    Ordering::Greater => 2;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Ordering::Less"));
        assert!(output.rust_code.contains("Ordering::Equal"));
        assert!(output.rust_code.contains("Ordering::Greater"));
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
