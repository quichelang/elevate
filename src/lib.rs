pub mod ast;
pub mod codegen;
pub mod crate_builder;
pub mod data;
pub mod diag;
pub mod ir;
pub mod lexer;
pub mod ownership_planner;
pub mod parser;
pub mod passes;
mod rustdex_adapter;
pub mod rustdex_backend;
pub mod source;
pub mod source_map;
pub mod test_runner;

use std::fmt;

use ast::{Item, Module};
use diag::Diagnostic;
use ir::lowered::RustModule;
use ir::typed::TypedModule;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExperimentFlags {
    pub move_mut_args: bool,
    pub type_system: bool,
}

impl Default for ExperimentFlags {
    fn default() -> Self {
        Self {
            move_mut_args: false,
            type_system: true, // on by default; --strict disables
        }
    }
}

impl ExperimentFlags {
    fn type_system_enabled(&self) -> bool {
        self.type_system
    }

    fn active_names(&self) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.move_mut_args {
            out.push("exp_move_mut_args");
        }
        if self.type_system_enabled() {
            out.push("exp_type_system");
        }
        out
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CompileOptions {
    pub experiments: ExperimentFlags,
    pub direct_borrow_hints: Vec<DirectBorrowHint>,
    pub forced_clone_places: Vec<String>,
    pub fail_on_hot_clone: bool,
    pub allow_hot_clone_places: Vec<String>,
    pub warn_missing_types: bool,
    pub source_name: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DirectBorrowHint {
    pub path: String,
    pub borrowed_arg_indexes: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub typed: TypedModule,
    pub lowered: RustModule,
    pub rust_code: String,
    pub ownership_notes: Vec<String>,
    pub warnings: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub diagnostics: Vec<Diagnostic>,
    pub source_name: Option<String>,
    pub source_text: Option<String>,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, diagnostic) in self.diagnostics.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            write!(
                f,
                "{}",
                source_map::render_diagnostic(
                    diagnostic,
                    self.source_name.as_deref(),
                    self.source_text.as_deref()
                )
            )?;
        }
        Ok(())
    }
}

impl CompileError {
    fn from_diagnostics(
        diagnostics: Vec<Diagnostic>,
        source_name: Option<String>,
        source_text: Option<String>,
    ) -> Self {
        Self {
            diagnostics,
            source_name,
            source_text,
        }
    }
}

pub fn compile_source(source: &str) -> Result<CompilerOutput, CompileError> {
    compile_source_with_options(source, &CompileOptions::default())
}

pub fn compile_source_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<CompilerOutput, CompileError> {
    let source_name = options.source_name.clone();
    let source_text = Some(source.to_string());
    let tokens = lexer::lex(source).map_err(|diagnostics| {
        CompileError::from_diagnostics(diagnostics, source_name.clone(), source_text.clone())
    })?;
    let module = parser::parse_module(tokens).map_err(|diagnostics| {
        CompileError::from_diagnostics(diagnostics, source_name.clone(), source_text.clone())
    })?;
    compile_ast_with_options(&module, options).map_err(|mut error| {
        if error.source_name.is_none() {
            error.source_name = source_name;
        }
        if error.source_text.is_none() {
            error.source_text = source_text;
        }
        error
    })
}

pub fn compile_ast(module: &Module) -> Result<CompilerOutput, CompileError> {
    compile_ast_with_options(module, &CompileOptions::default())
}

pub fn compile_ast_with_options(
    module: &Module,
    options: &CompileOptions,
) -> Result<CompilerOutput, CompileError> {
    if let Err(message) = validate_experiment_flags(&options.experiments) {
        return Err(CompileError::from_diagnostics(
            vec![Diagnostic::new(message, diag::Span::new(0, 0))],
            options.source_name.clone(),
            None,
        ));
    }
    let warnings = collect_compile_warnings(module, options);
    let type_system_enabled = options.experiments.type_system_enabled();
    let rustdex_ready = if type_system_enabled {
        match rustdex_backend::preflight_required() {
            Ok(_session) => true,
            Err(err) => {
                return Err(CompileError::from_diagnostics(
                    vec![Diagnostic::new(
                        format!(
                            "E_RUSTDEX_UNAVAILABLE: rustdex unavailable: capability resolution requires rustdex index metadata ({err:?})"
                        ),
                        diag::Span::new(0, 0),
                    )],
                    options.source_name.clone(),
                    None,
                ));
            }
        }
    } else {
        false
    };
    let typed = passes::lower_to_typed_with_options(
        module,
        &passes::TypecheckOptions {
            type_system: type_system_enabled,
            rustdex_ready,
        },
    )
    .map_err(|diagnostics| {
        CompileError::from_diagnostics(diagnostics, options.source_name.clone(), None)
    })?;
    let mut lowered = passes::lower_to_rust_with_hints(
        &typed,
        &options.direct_borrow_hints,
        &options.forced_clone_places,
    );
    for name in options.experiments.active_names() {
        lowered
            .ownership_notes
            .push(format!("experimental flag enabled: {name}"));
    }
    enforce_hot_clone_policy(&lowered.ownership_notes, options).map_err(|diagnostics| {
        CompileError::from_diagnostics(diagnostics, options.source_name.clone(), None)
    })?;
    let ownership_notes = lowered.ownership_notes.clone();
    let rust_code = codegen::emit_rust_module(&lowered);

    Ok(CompilerOutput {
        typed,
        lowered,
        rust_code,
        ownership_notes,
        warnings,
    })
}

pub fn validate_experiment_flags(experiments: &ExperimentFlags) -> Result<(), String> {
    let _ = experiments;
    Ok(())
}

fn collect_compile_warnings(module: &Module, options: &CompileOptions) -> Vec<Diagnostic> {
    if !options.warn_missing_types {
        return Vec::new();
    }
    let mut warnings = Vec::new();
    for item in &module.items {
        collect_item_warnings(item, &mut warnings);
    }
    warnings
}

fn collect_item_warnings(item: &Item, warnings: &mut Vec<Diagnostic>) {
    match item {
        Item::Function(function) => collect_function_warnings(function, warnings),
        Item::Impl(imp) => {
            for method in &imp.methods {
                collect_function_warnings(method, warnings);
            }
        }
        Item::Const(constant) => {
            if constant.ty.is_none() {
                warnings.push(Diagnostic::new(
                    format!(
                        "Missing explicit type for const `{}`. Add an explicit type annotation to avoid inference-only behavior.",
                        constant.name
                    ),
                    warning_span(constant.span),
                ));
            }
        }
        _ => {}
    }
}

fn collect_function_warnings(function: &ast::FunctionDef, warnings: &mut Vec<Diagnostic>) {
    for param in &function.params {
        if is_placeholder_type(&param.ty) {
            warnings.push(Diagnostic::new(
                format!(
                    "Missing explicit type for parameter `{}` in function `{}`. Add an explicit type annotation or enable `--exp-type-system`.",
                    param.name, function.name
                ),
                warning_span(function.span),
            ));
        }
    }
    if function.return_type.is_none() {
        warnings.push(Diagnostic::new(
            format!(
                "Missing explicit return type for function `{}`. Add an explicit return type annotation or enable `--exp-type-system`.",
                function.name
            ),
            warning_span(function.span),
        ));
    }
}

fn is_placeholder_type(ty: &ast::Type) -> bool {
    ty.path.len() == 1 && ty.path[0] == "_"
}

fn warning_span(span: Option<diag::Span>) -> diag::Span {
    span.unwrap_or(diag::Span::new(0, 0))
}

fn enforce_hot_clone_policy(
    notes: &[String],
    options: &CompileOptions,
) -> Result<(), Vec<Diagnostic>> {
    if !options.fail_on_hot_clone {
        return Ok(());
    }
    let mut diagnostics = Vec::new();
    for note in notes {
        if !note.starts_with("hot-clone:auto ") {
            continue;
        }
        let place = extract_hot_clone_place(note).unwrap_or_else(|| "<unknown>".to_string());
        let is_allowed = options
            .allow_hot_clone_places
            .iter()
            .any(|allowed| note.contains(&format!("place=`{}`", allowed)));
        if is_allowed {
            continue;
        }
        diagnostics.push(Diagnostic::new(
            format!(
                "Hot-path auto-clone rejected by policy at `{place}`. Add an explicit override with `--allow-hot-clone-place {place}` if this clone is intentional."
            ),
            diag::Span::new(0, 0),
        ));
    }
    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(diagnostics)
    }
}

fn extract_hot_clone_place(note: &str) -> Option<String> {
    let marker = "place=`";
    let start = note.find(marker)? + marker.len();
    let rest = &note[start..];
    let end = rest.find('`')?;
    Some(rest[..end].to_string())
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::fs;
    use std::process::Command;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{CompileOptions, compile_ast, compile_source, compile_source_with_options};
    static TEST_TEMP_SEQ: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn compile_smoke_test() {
        let source = r#"
            pub struct Point { x: i64, y: i64, }

            pub fn id(v: i64) -> i64 {
                return v;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub struct Point"));
        assert!(output.rust_code.contains("pub fn id"));
    }

    #[test]
    fn compile_supports_result_try_and_use_imports() {
        let source = r#"
            use std::num::ParseIntError;

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
    fn compile_error_includes_source_name_and_line_col_for_parser_errors() {
        let source = "fn broken( { return 1; }";
        let mut options = CompileOptions::default();
        options.source_name = Some("examples/broken.ers".to_string());
        let error =
            compile_source_with_options(source, &options).expect_err("expected parse error");
        let rendered = error.to_string();
        assert!(rendered.contains("examples/broken.ers:"));
        assert!(rendered.contains("Expected"));
    }

    #[test]
    fn compile_error_marks_spanless_diagnostics_as_location_unavailable() {
        let source = r#"
            fn run() {
                return missing_call();
            }
        "#;
        let mut options = CompileOptions::default();
        options.source_name = Some("examples/missing.ers".to_string());
        let error =
            compile_source_with_options(source, &options).expect_err("expected unknown function");
        let rendered = error.to_string();
        assert!(rendered.contains("examples/missing.ers:"));
        assert!(rendered.contains("Unknown function `missing_call`"));
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
            struct Counter { value: i64, }

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
    fn compile_supports_negative_and_mul_div_rem_operators() {
        let source = r#"
            fn calc(a: i64, b: i64, c: i64) -> i64 {
                return -a + b * c / 2 % 5 - 1;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("% 5"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_index_assignment_targets() {
        let source = r#"
            fn update(values: Vec<i64>) -> i64 {
                values[1] = values[1] % 3;
                return values[1];
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn update(mut values: Vec<i64>) -> i64")
        );
        assert!(output.rust_code.contains("values["));
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert!(output.rust_code.contains("% 3"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_let_reassignment_style() {
        let source = r#"
            fn update(v: i64) -> i64 {
                let next = v;
                next = next + 1;
                next = next * 2;
                return next;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("let mut next: i64 = v;"));
        assert!(output.rust_code.contains("next = (next + 1);"));
        assert!(output.rust_code.contains("next = (next * 2);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_nested_index_assignment_targets() {
        let source = r#"
            fn update(board: Vec<Vec<i64>>) -> i64 {
                board[1][2] = board[1][2] + 1;
                return board[1][2];
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn update(mut board: Vec<Vec<i64>>) -> i64")
        );
        assert!(output.rust_code.contains("board["));
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert!(output.rust_code.contains("+ 1"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_deep_nested_index_assignment_targets() {
        let source = r#"
            fn update(hyper: Vec<Vec<Vec<Vec<i64>>>>) -> i64 {
                hyper[0][1][2][3] = hyper[0][1][2][3] + 1;
                return hyper[0][1][2][3];
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn update(mut hyper: Vec<Vec<Vec<Vec<i64>>>>) -> i64")
        );
        assert!(output.rust_code.contains("hyper["));
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert!(output.rust_code.contains("+ 1"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_coerces_numeric_types_in_arithmetic() {
        let source = r#"
            fn measure(base: usize, delta: i64) -> i64 {
                return base + delta;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("((base as i64) + delta)"));
        assert_rust_code_compiles(&output.rust_code);
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
            use std::mem::drop;

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
            pub struct Packet { id: i64, }

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
            use external::Token;

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
    fn compile_auto_clones_reused_consuming_receiver_in_single_chain_expression() {
        let source = r#"
            fn demo(text: String) -> usize {
                return text.into_bytes().len() + text.into_bytes().len();
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("text.clone().into_bytes().len() + text.into_bytes().len()")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_respects_forced_clone_place_hints() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                consume(text);
                return;
            }
        "#;
        let mut options = CompileOptions::default();
        options.forced_clone_places.push("text".to_string());
        let output = compile_source_with_options(source, &options).expect("expected compile");
        assert!(output.rust_code.contains("consume(text.clone());"));
    }

    #[test]
    fn compile_auto_clones_owned_args_inside_loops() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                for i in 0..3 {
                    std::mem::drop(i);
                    consume(text);
                }
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for i in 0..3"));
        assert!(output.rust_code.contains("consume(text.clone());"));
        assert!(!output.rust_code.contains("consume(text);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_preserves_move_chain_rebinding_without_clone_in_loops() {
        let source = r#"
            pub struct Canvas { id: i64, }

            impl Canvas {
                fn put(self: Self, x: i64) -> Self {
                    std::mem::drop(x);
                    self
                }
            }

            fn render(canvas: Canvas) -> Canvas {
                for i in 0..10 {
                    canvas = Canvas::put(canvas, i);
                }
                canvas
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("canvas = Canvas::put(canvas, i);")
        );
        assert!(!output.rust_code.contains("Canvas::put(canvas.clone(), i)"));
        assert!(
            output
                .ownership_notes
                .iter()
                .all(|note| !note.contains("`canvas`"))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_fail_on_hot_clone_policy_rejects_expensive_loop_clones() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                for i in 0..3 {
                    std::mem::drop(i);
                    consume(text);
                }
                return;
            }
        "#;
        let mut options = CompileOptions::default();
        options.fail_on_hot_clone = true;

        let error =
            compile_source_with_options(source, &options).expect_err("expected policy failure");
        assert!(
            error
                .to_string()
                .contains("Hot-path auto-clone rejected by policy")
        );
        assert!(error.to_string().contains("--allow-hot-clone-place text"));
    }

    #[test]
    fn compile_allows_hot_clone_when_place_is_explicitly_approved() {
        let source = r#"
            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(text: String) {
                for i in 0..3 {
                    std::mem::drop(i);
                    consume(text);
                }
                return;
            }
        "#;
        let mut options = CompileOptions::default();
        options.fail_on_hot_clone = true;
        options.allow_hot_clone_places.push("text".to_string());

        let output =
            compile_source_with_options(source, &options).expect("expected compile success");
        assert!(output.rust_code.contains("consume(text.clone());"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.starts_with("hot-clone:auto "))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_read_views_for_looped_struct_reads() {
        let source = r#"
            pub struct Game {
                title: String,
                rounds: Vec<String>,
            }

            fn score(game: Game) -> usize {
                const snapshot = view(game);
                for round in snapshot.rounds.iter() {
                    std::mem::drop(round);
                    std::mem::drop(snapshot.title.len());
                }
                return snapshot.rounds.len();
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("let snapshot: &Game = &game;"));
        assert!(output.rust_code.contains("snapshot.rounds.iter()"));
        assert!(output.rust_code.contains("snapshot.title.len()"));
        assert!(!output.rust_code.contains("game.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_clones_reused_struct_field_call_argument() {
        let source = r#"
            pub struct State { text: String, }

            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(state: State) {
                consume(state.text);
                std::mem::drop(state.text);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("consume(state.text.clone());"));
        assert!(output.rust_code.contains("std::mem::drop(state.text);"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`state.text`"))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_auto_clones_reused_struct_field_owned_method_receiver() {
        let source = r#"
            pub struct State { text: String, }

            fn demo(state: State) {
                std::mem::drop(state.text.into_bytes());
                std::mem::drop(state.text.into_bytes());
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("state.text.clone().into_bytes()"));
        assert!(output.rust_code.contains("state.text.into_bytes()"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`state.text`"))
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_avoids_clone_for_disjoint_struct_fields() {
        let source = r#"
            pub struct Pair { left: String, right: String, }

            fn consume(value: String) {
                std::mem::drop(value);
                return;
            }

            fn demo(pair: Pair) {
                consume(pair.left);
                consume(pair.right);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("consume(pair.left);"));
        assert!(output.rust_code.contains("consume(pair.right);"));
        assert!(!output.rust_code.contains("pair.left.clone()"));
        let real_notes: Vec<_> = output
            .ownership_notes
            .iter()
            .filter(|n| !n.starts_with("experimental flag"))
            .collect();
        assert!(
            real_notes.is_empty(),
            "unexpected ownership notes: {:?}",
            real_notes
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_clones_field_when_whole_struct_is_used_later() {
        let source = r#"
            pub struct Pair { left: String, right: String, }

            fn consume_text(value: String) {
                std::mem::drop(value);
                return;
            }

            fn consume_pair(value: Pair) {
                std::mem::drop(value);
                return;
            }

            fn demo(pair: Pair) {
                consume_text(pair.left);
                consume_pair(pair);
                return;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("consume_text(pair.left.clone());")
        );
        assert!(output.rust_code.contains("consume_pair(pair);"));
        assert!(
            output
                .ownership_notes
                .iter()
                .any(|note| note.contains("`pair.left`"))
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
    fn compile_supports_string_push_str_method_with_auto_borrow() {
        let source = r#"
            fn append(text: String, suffix: String) -> String {
                text.push_str(suffix);
                text
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("text.push_str(&suffix);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_string_concat_chains_without_manual_borrows() {
        let source = r#"
            fn join3(a: String, b: String, c: String) -> String {
                a + b + c
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("a + &b"));
        assert!(output.rust_code.contains("+ &c"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_does_not_auto_borrow_known_by_value_associated_calls() {
        let source = r#"
            struct Catalog {}

            impl Catalog {
                fn get(self: Self, idx: i64) -> i64 {
                    idx
                }
            }

            fn demo(catalog: Catalog) -> i64 {
                Catalog::get(catalog, 7)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Catalog::get(catalog, 7)"));
        assert!(!output.rust_code.contains("Catalog::get(&catalog, 7)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_heuristically_borrows_third_party_method_calls() {
        let source = r#"
            rust {
                pub struct Foreign { pub text: String }
                impl Foreign {
                    pub fn has_piece(&self, needle: &str) -> bool {
                        self.text.contains(needle)
                    }
                }
            }

            fn demo(foreign: Foreign, needle: String) -> bool {
                std::mem::drop(foreign.has_piece(needle));
                return foreign.has_piece(needle);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("foreign.has_piece(&needle)"));
        assert!(!output.rust_code.contains("needle.clone()"));
        assert!(!output.rust_code.contains("foreign.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_heuristically_borrows_third_party_associated_query_calls() {
        let source = r#"
            rust {
                pub struct Foreign;
                impl Foreign {
                    pub fn has_prefix(text: &str, prefix: &str) -> bool {
                        text.starts_with(prefix)
                    }
                }
            }

            fn demo(text: String, prefix: String) -> bool {
                std::mem::drop(Foreign::has_prefix(text, prefix));
                return Foreign::has_prefix(text, prefix);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("Foreign::has_prefix(&text, &prefix)")
        );
        assert!(!output.rust_code.contains("text.clone()"));
        assert!(!output.rust_code.contains("prefix.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_heuristically_borrows_draw_like_external_path_calls() {
        let source = r#"
            rust {
                pub fn runtime_draw_scene(cells: &[i64], fixed: &[bool]) -> bool {
                    !cells.is_empty() && !fixed.is_empty()
                }
            }

            fn demo(cells: Vec<i64>, fixed: Vec<bool>) -> bool {
                const drawn = runtime_draw_scene(cells, fixed);
                if drawn {
                    return runtime_draw_scene(cells, fixed);
                }
                runtime_draw_scene(cells, fixed)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("runtime_draw_scene(&cells, &fixed)")
        );
        assert!(
            output
                .rust_code
                .contains("return runtime_draw_scene(&cells, &fixed);")
        );
        assert!(!output.rust_code.contains("cells.clone()"));
        assert!(!output.rust_code.contains("fixed.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_borrowed_external_call_arg_preserves_followup_move() {
        let source = r#"
            rust {
                pub fn has_values(values: &[i64]) -> bool { !values.is_empty() }
                pub fn consume(values: Vec<i64>) -> i64 { values.len() as i64 }
            }

            fn demo(values: Vec<i64>) -> i64 {
                std::mem::drop(has_values(values));
                consume(values)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("has_values(&values)"));
        assert!(output.rust_code.contains("consume(values)"));
        assert!(!output.rust_code.contains("consume(values.clone())"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_does_not_auto_borrow_owned_string_for_draw_prefixed_calls() {
        let source = r#"
            rust {
                pub fn draw_footer_status(message: String) -> bool { !message.is_empty() }
            }

            fn demo(message: String) -> bool {
                draw_footer_status(message)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("draw_footer_status(message)"));
        assert!(!output.rust_code.contains("draw_footer_status(&message)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_does_not_auto_borrow_by_value_associated_calls_with_nominal_args() {
        let source = r#"
            rust {
                pub struct Board;
                impl Board {
                    pub fn is_complete(board: Board) -> bool {
                        let _ = board;
                        true
                    }
                }
            }

            fn demo(board: Board) -> bool {
                Board::is_complete(board)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Board::is_complete(board)"));
        assert!(!output.rust_code.contains("Board::is_complete(&board)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn benchmark_regression_ownership_profiles() {
        struct Case {
            name: &'static str,
            source: &'static str,
            max_clone_calls: usize,
            max_hot_clone_notes: usize,
            required_snippets: &'static [&'static str],
            forbidden_snippets: &'static [&'static str],
        }

        let cases = vec![
            Case {
                name: "runtime_draw_scene borrow",
                source: r#"
                    rust {
                        pub fn runtime_draw_scene(cells: &[i64], fixed: &[bool]) -> bool {
                            !cells.is_empty() && !fixed.is_empty()
                        }
                    }

                    fn demo(cells: Vec<i64>, fixed: Vec<bool>) -> bool {
                        const drawn = runtime_draw_scene(cells, fixed);
                        if drawn {
                            std::mem::drop(runtime_draw_scene(cells, fixed));
                        }
                        runtime_draw_scene(cells, fixed)
                    }
                "#,
                max_clone_calls: 0,
                max_hot_clone_notes: 0,
                required_snippets: &["runtime_draw_scene(&cells, &fixed)"],
                forbidden_snippets: &["cells.clone()", "fixed.clone()"],
            },
            Case {
                name: "draw prefixed string by-value",
                source: r#"
                    rust {
                        pub fn draw_footer_status(message: String) -> bool { !message.is_empty() }
                    }

                    fn demo(message: String) -> bool {
                        draw_footer_status(message)
                    }
                "#,
                max_clone_calls: 0,
                max_hot_clone_notes: 0,
                required_snippets: &["draw_footer_status(message)"],
                forbidden_snippets: &["draw_footer_status(&message)", "message.clone()"],
            },
            Case {
                name: "nominal associated by-value",
                source: r#"
                    rust {
                        pub struct Board;
                        impl Board {
                            pub fn is_complete(board: Board) -> bool {
                                let _ = board;
                                true
                            }
                        }
                    }

                    fn demo(board: Board) -> bool {
                        Board::is_complete(board)
                    }
                "#,
                max_clone_calls: 0,
                max_hot_clone_notes: 0,
                required_snippets: &["Board::is_complete(board)"],
                forbidden_snippets: &["Board::is_complete(&board)", "board.clone()"],
            },
            Case {
                name: "loop clone budget",
                source: r#"
                    rust { pub fn consume(values: Vec<i64>) { std::mem::drop(values.len()); } }

                    fn demo(values: Vec<i64>) {
                        for _ in 0..3 {
                            consume(values);
                        }
                        return;
                    }
                "#,
                max_clone_calls: 3,
                max_hot_clone_notes: 1,
                required_snippets: &["consume(values.clone())"],
                forbidden_snippets: &[],
            },
        ];

        let mut failures = Vec::new();
        for case in cases {
            let output =
                compile_source(case.source).expect("benchmark regression case should compile");
            let clone_calls = output.rust_code.match_indices(".clone()").count()
                + output.rust_code.match_indices(".clone(").count();
            let hot_clone_notes = output
                .ownership_notes
                .iter()
                .filter(|note| note.starts_with("hot-clone:auto "))
                .count();

            if clone_calls > case.max_clone_calls {
                failures.push(format!(
                    "{}: clone_calls={} exceeds budget {}",
                    case.name, clone_calls, case.max_clone_calls
                ));
            }
            if hot_clone_notes > case.max_hot_clone_notes {
                failures.push(format!(
                    "{}: hot_clone_notes={} exceeds budget {}",
                    case.name, hot_clone_notes, case.max_hot_clone_notes
                ));
            }
            for snippet in case.required_snippets {
                if !output.rust_code.contains(snippet) {
                    failures.push(format!("{}: missing snippet `{snippet}`", case.name));
                }
            }
            for snippet in case.forbidden_snippets {
                if output.rust_code.contains(snippet) {
                    failures.push(format!("{}: unexpected snippet `{snippet}`", case.name));
                }
            }

            assert_rust_code_compiles(&output.rust_code);
        }

        if !failures.is_empty() {
            panic!(
                "ownership benchmark regressions detected:\n{}",
                failures.join("\n")
            );
        }
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
        assert!(output.rust_code.contains(
            "fn __elevate_shim_str_strip_prefix_known(__arg0: &str, __arg1: &str) -> String"
        ));
        assert!(
            output
                .rust_code
                .contains("str::strip_prefix(__arg0, __arg1)")
        );
        assert!(output.rust_code.contains(".unwrap().to_string()"));
        assert!(output.rust_code.contains(
            "fn __elevate_shim_str_split_once_known(__arg0: &str, __arg1: &str) -> (String, String)"
        ));
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
    fn compile_supports_vec_push_method_with_mut_inference() {
        let source = r#"
            fn demo(values: Vec<i64>, item: i64) -> usize {
                values.push(item);
                return Vec::len(values);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn demo(values: &mut Vec<i64>, item: i64) -> usize")
        );
        assert!(output.rust_code.contains("values.push(item);"));
        assert!(output.rust_code.contains("Vec::len(&values)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_local_let_vec_push_mut_inference() {
        let source = r#"
            fn demo() -> usize {
                let values: Vec<i64> = [];
                values.push(1);
                return Vec::len(values);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("let mut values: Vec<i64> = vec![];")
        );
        assert!(output.rust_code.contains("values.push(1);"));
        assert!(output.rust_code.contains("Vec::len(&values)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_rejects_const_mutating_method_calls() {
        let source = r#"
            fn demo() {
                const values: Vec<i64> = [];
                values.push(1);
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Cannot mutate immutable const `values` via method `push`")
        );
    }

    #[test]
    fn compile_rejects_shadowing_const_with_let() {
        let source = r#"
            fn demo() -> i64 {
                const value = 1;
                let value = 2;
                value
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Cannot redeclare `value` because it is an immutable const in scope")
        );
    }

    #[test]
    fn compile_rejects_shadowing_const_in_nested_block() {
        let source = r#"
            fn demo() -> i64 {
                const value = 1;
                if true {
                    let value = 2;
                    return value;
                }
                value
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Cannot redeclare `value` because it is an immutable const in scope")
        );
    }

    #[test]
    fn compile_auto_borrows_hashmap_btreemap_associated_calls() {
        let source = r#"
            use std::collections::HashMap;
            use std::collections::BTreeMap;

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
            use std::collections::HashSet;
            use std::collections::BTreeSet;

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
    fn compile_rejects_unknown_function_call() {
        let source = r#"
            fn run() -> i64 {
                return missing_call(1, 2);
            }
        "#;

        let error = compile_source(source).expect_err("expected unknown function diagnostic");
        assert!(
            error
                .to_string()
                .contains("Unknown function `missing_call`")
        );
    }

    #[test]
    fn compile_infers_option_return_with_bidi_when_none_precedes_some() {
        let source = r#"
            fn maybe(flag: bool) {
                if flag {
                    return None;
                }
                return Some(1);
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi inference should compile");
        assert!(
            output
                .rust_code
                .contains("fn maybe(flag: bool) -> Option<i64>")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn bidi_mode_accepts_unknown_binding_from_namespaced_call() {
        let source = r#"
            fn run() -> i64 {
                const left = math::left();
                return left + 2;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi mode should compile");
        assert!(output.rust_code.contains("left + 2"));
    }

    #[test]
    fn compile_infers_result_return_with_bidi_from_ok_and_err_paths() {
        let source = r#"
            fn parse(flag: bool) {
                if flag {
                    return Result::Ok(1);
                }
                return Result::Err("bad");
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi inference should compile");
        assert!(
            output
                .rust_code
                .contains("fn parse(flag: bool) -> Result<i64, String>")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_infers_match_option_return_with_bidi() {
        let source = r#"
            fn choose(flag: bool) {
                return match flag {
                    true => None;
                    false => Some(9);
                };
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi inference should compile");
        assert!(
            output
                .rust_code
                .contains("fn choose(flag: bool) -> Option<i64>")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_infers_nested_result_inside_option_with_bidi() {
        let source = r#"
            fn nested(flag: bool) {
                if flag {
                    return Some(Result::Ok(1));
                }
                return Some(Result::Err("bad"));
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi inference should compile");
        assert!(
            output
                .rust_code
                .contains("fn nested(flag: bool) -> Option<Result<i64, String>>")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_infers_tuple_of_options_with_bidi() {
        let source = r#"
            fn pair(flag: bool) {
                if flag {
                    return (Some(1), None);
                }
                return (None, Some(2));
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("bidi inference should compile");
        assert!(
            output
                .rust_code
                .contains("fn pair(flag: bool) -> (Option<i64>, Option<i64>)")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_still_rejects_incompatible_return_families_with_bidi() {
        let source = r#"
            fn bad(flag: bool) {
                if flag {
                    return Some(1);
                }
                return Result::Err("nope");
            }
        "#;

        let mut options = CompileOptions::default();
        let error =
            compile_source_with_options(source, &options).expect_err("expected mismatch error");
        assert!(
            error
                .to_string()
                .contains("Cannot infer single return type for `bad`")
        );
    }

    #[test]
    fn compile_principal_fallback_flag_emits_targeted_function_hint() {
        let source = r#"
            fn maybe_value() {
                return None;
            }
        "#;

        let mut options = CompileOptions::default();
        let error = compile_source_with_options(source, &options).expect_err("expected fallback");
        let rendered = error.to_string();
        assert!(rendered.contains("Principal fallback:"));
        assert!(rendered.contains("function `maybe_value`"));
    }

    #[test]
    fn compile_principal_fallback_flag_emits_targeted_const_hint() {
        let source = r#"
            const EMPTY = None;
        "#;

        let mut options = CompileOptions::default();
        let error = compile_source_with_options(source, &options).expect_err("expected fallback");
        let rendered = error.to_string();
        assert!(rendered.contains("Principal fallback:"));
        assert!(rendered.contains("const `EMPTY`"));
    }

    #[test]
    fn generated_rust_compiles_for_result_try_flow() {
        let source = r#"
            use std::num::ParseIntError;

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
    fn compile_supports_generic_struct_and_enum_definitions() {
        let source = r#"
            struct Wrapper<T> {
                value: T,
            }

            enum Maybe<T> {
                Some(T),
                None,
            }

            fn wrap(v: i64) -> Wrapper<i64> {
                Wrapper { value: v }
            }

            fn choose(flag: bool, value: i64) -> Maybe<i64> {
                if flag {
                    return Maybe::Some(value);
                }
                Maybe::None
            }
        "#;

        let output = compile_source(source).expect("expected generic type defs support");
        assert!(output.rust_code.contains("struct Wrapper<T>"));
        assert!(output.rust_code.contains("enum Maybe<T>"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_generic_impl_block_methods() {
        let source = r#"
            struct Wrapper<T> {
                value: T,
            }

            impl<T> Wrapper<T> {
                fn new(value: T) -> Self {
                    Wrapper { value: value }
                }

                fn get(self) -> T {
                    self.value
                }
            }

            fn run() -> i64 {
                Wrapper::new(7).get()
            }
        "#;

        let output = compile_source(source).expect("expected generic impl support");
        assert!(output.rust_code.contains("impl<T> Wrapper<T>"));
        assert!(output.rust_code.contains("fn get(self: Wrapper<T>) -> T"));
        assert!(output.rust_code.contains("fn run() -> i64"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_generic_trait_impl_block_targets() {
        let source = r#"
            trait Named {
                fn name(self: Self) -> String;
            }

            struct Wrapper<T> {
                value: T,
            }

            impl<T> Named for Wrapper<T> {
                fn name(self) -> String {
                    "wrapped"
                }
            }

            fn run(v: Wrapper<i64>) -> String {
                v.name()
            }
        "#;

        let output = compile_source(source).expect("expected generic trait impl target support");
        assert!(output.rust_code.contains("impl<T> Named for Wrapper<T>"));
        assert!(
            output
                .rust_code
                .contains("fn name(self: Wrapper<T>) -> String")
        );
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
        let rendered = error.to_string();
        assert!(
            rendered.contains("Arg 2 for `same`: expected `T`, got `String`")
                || rendered.contains("Arg 2 for `same`: expected `i64`, got `String`")
        );
    }

    #[test]
    fn compile_supports_generic_bounds_syntax() {
        let source = r#"
            fn keep<T: Clone + Copy>(value: T) -> T {
                value
            }

            fn run() -> i64 {
                keep(7)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn keep<T: Clone + Copy>(value: T) -> T")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_generic_bound_violation() {
        let source = r#"
            fn keep<T: Copy>(value: T) -> T {
                value
            }

            fn run() -> String {
                keep("hello")
            }
        "#;

        let error = compile_source(source).expect_err("expected bound violation");
        assert!(error.to_string().contains("does not satisfy bound `Copy`"));
    }

    #[test]
    fn compile_supports_generic_default_ord_bounds() {
        let source = r#"
            fn ordered<T: Default + Ord>(left: T, right: T) -> bool {
                const zero = T::default();
                std::mem::drop(zero);
                return left < right;
            }

            fn run() -> bool {
                ordered(1, 2)
            }
        "#;

        let output = compile_source(source).expect("expected generic bounds support");
        assert!(
            output
                .rust_code
                .contains("fn ordered<T: Default + Ord>(left: T, right: T) -> bool")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_generic_partial_eq_bound_violation_for_user_struct() {
        let source = r#"
            struct User { id: i64, }

            fn eq_user<T: PartialEq>(left: T, right: T) -> bool {
                return left == right;
            }

            fn run(left: User, right: User) -> bool {
                eq_user(left, right)
            }
        "#;

        let error = compile_source(source).expect_err("expected PartialEq bound violation");
        assert!(
            error
                .to_string()
                .contains("does not satisfy bound `PartialEq`")
        );
    }

    #[test]
    fn compile_reports_generic_hash_bound_violation_for_user_struct() {
        let source = r#"
            struct User { id: i64, }

            fn keep<T: Hash>(value: T) -> T {
                value
            }

            fn run(value: User) -> User {
                keep(value)
            }
        "#;

        let error = compile_source(source).expect_err("expected Hash bound violation");
        assert!(error.to_string().contains("does not satisfy bound `Hash`"));
    }

    #[test]
    fn compile_supports_structural_method_polymorphism_via_specialization() {
        let source = r#"
            struct Label { text: String, }

            impl Label {
                fn draw_label(self: Self, prefix: String) -> String {
                    return prefix + self.text;
                }
            }

            fn render<T>(value: T) -> String {
                return value.draw_label("> ");
            }

            fn run(label: Label) -> String {
                return render(label);
            }
        "#;

        let output = compile_source(source).expect("expected structural method specialization");
        assert!(output.rust_code.contains("__elevate_row_render_"));
        assert!(!output.rust_code.contains("fn render<T>"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_structural_field_polymorphism_via_specialization() {
        let source = r#"
            struct Entry { count: i64, }

            fn read<T>(value: T) -> i64 {
                return value.count;
            }

            fn run(item: Entry) -> i64 {
                return read(item);
            }
        "#;

        let output = compile_source(source).expect("expected structural field specialization");
        assert!(output.rust_code.contains("__elevate_row_read_"));
        assert!(!output.rust_code.contains("fn read<T>"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_structural_specialization_for_generic_impl_methods() {
        let source = r#"
            struct Point<T> {
                x: T,
                y: T,
            }

            impl<T> Point<T> {
                fn new(x: T, y: T) -> Point<T> {
                    Point { x: x, y: y, }
                }

                fn label(self) -> String {
                    return self.x.to_string();
                }
            }

            fn run() -> String {
                const p = Point::new(5, 6);
                return p.label();
            }
        "#;

        let output = compile_source(source)
            .expect("expected structural specialization for generic impl methods");
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_allows_structural_trait_methods_for_generic_impl_calls_with_bounds() {
        let source = r#"
            use std::fmt::Display;

            struct Point<T> {
                x: T,
                y: T,
            }

            impl<T> Point<T> {
                fn new(x: T, y: T) -> Point<T> {
                    Point { x: x, y: y, }
                }

                fn label(self) -> String {
                    return self.x.to_string();
                }
            }

            fn describe<T: Display>(point: Point<T>) -> String {
                return point.label();
            }

            fn run() -> String {
                return describe(Point::new(5, 6));
            }
        "#;

        let output = compile_source(source)
            .expect("expected structural trait method checks to honor inferred Display bound");
        assert!(
            output
                .rust_code
                .contains("fn describe<T: Display>(point: Point<T>) -> String")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_does_not_emit_unresolved_structural_impl_specializations() {
        let source = r#"
            use std::fmt::Display;

            struct Point<T> {
                x: T,
                y: T,
            }

            impl<T> Point<T> {
                fn new(x: T, y: T) -> Point<T> {
                    Point { x: x, y: y, }
                }

                fn label(self) -> String {
                    return self.x.to_string();
                }
            }

            fn describe<T: Display>(point: Point<T>) -> String {
                return point.label();
            }

            fn run() -> String {
                const p = Point::new(5, 6);
                return describe(p);
            }
        "#;

        let output = compile_source(source)
            .expect("expected concrete structural specialization without leaked unresolved impl");
        assert!(
            !output.rust_code.contains("impl Point<T> {"),
            "unexpected leaked unresolved impl specialization:\n{}",
            output.rust_code
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_missing_structural_member_for_generic_impl_methods() {
        let source = r#"
            struct Opaque {}

            struct Point<T> {
                x: T,
                y: T,
            }

            impl<T> Point<T> {
                fn label(self) -> String {
                    self.x.to_string()
                }
            }

            fn run(v: Opaque) -> String {
                const p = Point { x: v, y: Opaque {} };
                return p.label();
            }
        "#;

        let error = compile_source(source)
            .expect_err("expected missing method diagnostic for impl-method structural call");
        let rendered = error.to_string();
        assert!(
            rendered.contains("does not provide required method `to_string`"),
            "expected structural-method diagnostic, got:\n{rendered}"
        );
    }

    #[test]
    fn compile_reports_missing_structural_member_at_call_site() {
        let source = r#"
            struct Entry { count: i64, }
            struct Empty { id: i64, }

            fn read<T>(value: T) -> i64 {
                return value.count;
            }

            fn run(item: Empty) -> i64 {
                return read(item);
            }
        "#;

        let error = compile_source(source).expect_err("expected missing structural field");
        assert!(
            error
                .to_string()
                .contains("does not provide required field `count`")
        );
    }

    #[test]
    fn compile_enforces_user_trait_bounds_at_elevate_level() {
        let source = r#"
            trait Renderable {
                fn render(self: Self) -> String;
            }

            struct Card { id: i64, }

            fn print_it<T: Renderable>(value: T) -> String {
                value.render()
            }

            fn run(card: Card) -> String {
                print_it(card)
            }
        "#;

        let error = compile_source(source).expect_err("expected user-trait bound violation");
        assert!(
            error
                .to_string()
                .contains("does not satisfy bound `Renderable`")
        );
    }

    #[test]
    fn compile_enforces_user_supertrait_bounds_at_elevate_level() {
        let source = r#"
            trait Renderable {
                fn render(self: Self) -> String;
            }

            trait Fancy: Renderable {
            }

            struct Card { id: i64, }

            impl Fancy for Card {
            }

            fn print_it<T: Renderable>(value: T) -> String {
                value.render()
            }

            fn run(card: Card) -> String {
                print_it(card)
            }
        "#;

        let output = compile_source(source).expect("expected supertrait closure to satisfy bound");
        assert!(
            output
                .rust_code
                .contains("fn print_it<T: Renderable>(value: T) -> String")
        );
    }

    #[test]
    fn compile_infers_omitted_param_and_return_from_numeric_body_with_bidi() {
        let source = r#"
            fn add(a: i64, b) {
                return a + b;
            }

            fn run() {
                x = add(3, 4);
                return;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("expected bidi inference");
        assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
        assert!(output.rust_code.contains("let x: i64 = add(3, 4);"));
    }

    #[test]
    fn compile_infers_omitted_params_without_return_annotation_from_use_site_with_bidi() {
        let source = r#"
            fn add(a, b) {
                return a + b;
            }

            fn run() {
                y = add(10, 32);
                return;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("expected bidi inference");
        assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
        assert!(output.rust_code.contains("let y: i64 = add(10, 32);"));
    }

    #[test]
    fn compile_effect_rows_internal_rejects_unbounded_generic_method_use() {
        let source = r#"
            trait Displayish {
                fn render(self: Self) -> String;
            }

            fn bad<T>(value: T) -> String {
                value.render()
            }
        "#;

        let mut options = CompileOptions::default();
        let error =
            compile_source_with_options(source, &options).expect_err("expected capability error");
        let rendered = error.to_string();
        assert!(rendered.contains("missing capability `method::render`"));
        assert!(rendered.contains("add a bound like `T: Displayish`"));
    }

    #[test]
    fn compile_effect_rows_internal_accepts_supertrait_method_capability() {
        let source = r#"
            trait Displayish {
                fn render(self: Self) -> String;
            }

            trait Pretty: Displayish {
            }

            fn ok<T: Pretty>(value: T) -> String {
                value.render()
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("expected capability support");
        assert!(
            output
                .rust_code
                .contains("fn ok<T: Pretty>(value: T) -> String")
        );
    }

    #[test]
    fn compile_effect_rows_surface_accepts_declared_capabilities() {
        let source = r#"
            fn run(value: i64) ![call::std::mem::drop] {
                std::mem::drop(value);
                return;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("expected effect row acceptance");
        assert!(output.rust_code.contains("fn run(value: i64) -> ()"));
    }

    #[test]
    fn compile_effect_rows_surface_rejects_missing_declared_capabilities() {
        let source = r#"
            fn run(value: i64) ![] {
                std::mem::drop(value);
                return;
            }
        "#;

        let mut options = CompileOptions::default();
        let error = compile_source_with_options(source, &options)
            .expect_err("expected effect row mismatch");
        let rendered = error.to_string();
        assert!(rendered.contains("missing capability `call::std::mem::drop`"));
    }

    #[test]
    fn compile_effect_rows_surface_allows_open_rows() {
        let source = r#"
            fn run(value: i64) ![..r] {
                std::mem::drop(value);
                return;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("expected open effect row");
        assert!(output.rust_code.contains("fn run(value: i64) -> ()"));
    }

    #[test]
    fn compile_supports_match_on_enums() {
        let source = r#"
            enum Maybe {
                Some(i64),
                None,
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
                Pair(i64, i64),
                None,
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
    fn compile_supports_named_payload_enum_variants() {
        let source = r#"
            enum Message {
                Move { x: i64, y: i64 },
                Quit,
            }

            fn make_move() -> Message {
                Message::Move { x: 1, y: 2, }
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("Move { x: i64, y: i64 }"));
        assert!(output.rust_code.contains("Message::Move { x: 1, y: 2 }"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_reports_named_payload_enum_unknown_field() {
        let source = r#"
            enum Message {
                Move { x: i64, y: i64 },
            }

            fn make_move() -> Message {
                Message::Move { x: 1, z: 2, }
            }
        "#;

        let error = compile_source(source).expect_err("expected field error");
        assert!(
            error
                .to_string()
                .contains("Unknown enum variant field `z` in literal")
        );
    }

    #[test]
    fn compile_reports_named_payload_enum_missing_field() {
        let source = r#"
            enum Message {
                Move { x: i64, y: i64 },
            }

            fn make_move() -> Message {
                Message::Move { x: 1, }
            }
        "#;

        let error = compile_source(source).expect_err("expected missing field error");
        assert!(error.to_string().contains("Missing enum variant field `y`"));
    }

    #[test]
    fn compile_reports_multi_payload_enum_arity_mismatch() {
        let source = r#"
            enum Pair {
                Two(i64, i64),
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
            struct Point { x: i64, y: i64, }
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
            struct Point { x: i64, y: i64, z: i64, }
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
            struct Point { x: i64, y: i64, }
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
        assert!(
            error
                .to_string()
                .contains("Non-exhaustive match on tuple bool pattern")
        );
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
                Some(i64),
                None,
            }

            fn classify(v: Maybe) -> i64 {
                return match v {
                    Maybe::Some(_) => 1;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Non-exhaustive match on `Maybe`")
        );
        assert!(error.to_string().contains("None"));
    }

    #[test]
    fn compile_reports_non_exhaustive_finite_tuple_match() {
        let source = r#"
            enum Switch {
                Left,
                Right,
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
        assert!(
            error
                .to_string()
                .contains("Non-exhaustive match on finite tuple domain")
        );
        assert!(error.to_string().contains("(false, Switch::Left)"));
    }

    #[test]
    fn compile_accepts_exhaustive_finite_tuple_match() {
        let source = r#"
            enum Switch {
                Left,
                Right,
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
    fn compile_reports_non_exhaustive_option_tuple_match() {
        let source = r#"
            fn classify() -> i64 {
                const input = (false, Option::None);
                return match input {
                    (true, Option::Some(_)) => 1;
                    (true, Option::None) => 2;
                    (false, Option::Some(_)) => 3;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("Non-exhaustive match on finite tuple domain")
        );
        assert!(error.to_string().contains("(false, Option::None)"));
    }

    #[test]
    fn compile_skips_finite_tuple_exhaustiveness_for_payload_enums() {
        let source = r#"
            enum Maybe {
                Some(i64),
                None,
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
            use crate::Pair;

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
    fn compile_accepts_strict_implicit_tail_if_else_return() {
        let source = r#"
            fn choose(flag: bool) -> i64 {
                if flag {
                    1
                } else {
                    0
                }
            }
        "#;

        let output = compile_source(source).expect("expected implicit tail if/else return");
        assert!(output.rust_code.contains("return match flag"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_rejects_if_else_statement_with_trailing_semicolon_when_value_expected() {
        let source = r#"
            fn choose(flag: bool) -> i64 {
                if flag {
                    1
                } else {
                    0
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected trailing semicolon parse error");
        assert!(error.to_string().contains("Expected expression"));
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
            use crate::PairIter;

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
        assert!(
            output
                .rust_code
                .contains("for __item in vec![vec![1, 2], vec![3, 4]]")
        );
        assert!(
            output
                .rust_code
                .contains("let [left, right] = __item.as_slice() else")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_vec_into_iter() {
        let source = r#"
            fn sum(values: Vec<i64>) -> i64 {
                for value in values.into_iter() {
                    std::mem::drop(value);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for value in values.into_iter()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_vec_iter() {
        let source = r#"
            fn sum(values: Vec<i64>) -> i64 {
                for value in values.iter() {
                    std::mem::drop(value);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for value in values.iter()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_clones_for_loop_iterable_when_reused_after_loop() {
        let source = r#"
            fn count_and_probe(values: Vec<i64>) -> i64 {
                for value in values {
                    std::mem::drop(value);
                }
                std::mem::drop(values.len());
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for value in values.clone()"));
        assert!(output.rust_code.contains("values.len()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_string_chars() {
        let source = r#"
            fn walk(text: String) -> i64 {
                for ch in text.chars() {
                    std::mem::drop(ch);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for ch in text.chars()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_vec_first_last_get_methods() {
        let source = r#"
            fn probe(values: Vec<i64>) -> bool {
                const has_first = values.first().is_some();
                const has_last = values.last().is_some();
                const has_idx0 = values.get(0).is_some();
                has_first and has_last and has_idx0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains(".first().is_some()"));
        assert!(output.rust_code.contains(".last().is_some()"));
        // Integer literal 0 is converted via Elevate's i64→usize coercion
        assert!(output.rust_code.contains(".get(") && output.rust_code.contains(").is_some()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_hashmap_keys() {
        let source = r#"
            use std::collections::HashMap;

            fn walk_keys(map: HashMap<String, i64>) -> i64 {
                for key in map.keys() {
                    std::mem::drop(key);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("for key in map.keys()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_hashmap_into_iter_tuple() {
        let source = r#"
            use std::collections::HashMap;

            fn walk_pairs(map: HashMap<String, i64>) -> i64 {
                for (key, value) in map.into_iter() {
                    std::mem::drop(key);
                    std::mem::drop(value);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("for (key, value) in map.into_iter()")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_for_loop_option_into_iter() {
        let source = r#"
            fn walk_one() -> i64 {
                for value in Option::Some(1).into_iter() {
                    std::mem::drop(value);
                }
                0
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("for value in Option::Some(1).into_iter()")
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
        assert!(
            error
                .to_string()
                .contains("Slice destructure requires Vec value")
        );
    }

    #[test]
    fn compile_supports_impl_methods() {
        let source = r#"
            pub struct Point { x: i64, }

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
                Ok,
                Err(String),
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
            pub struct Point { x: i64, }

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
        assert!(
            output
                .rust_code
                .contains("pub fn bump(mut self: Point, n: i64) -> Point")
        );
        assert!(output.rust_code.contains("self.x += n;"));
        assert!(output.rust_code.contains("Point::bump(p, 1)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_impl_self_return_alias() {
        let source = r#"
            pub struct Point { x: i64, }

            impl Point {
                pub fn new(x: i64) -> Self {
                    Point { x: x, }
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
    fn compile_supports_trait_declarations_with_supertraits() {
        let source = r#"
            use std::fmt::Debug;
            use std::fmt::Display;

            pub trait Renderable: Debug + Display {
                fn render(self: Self) -> String;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("pub trait Renderable: Debug + Display")
        );
        assert!(
            output
                .rust_code
                .contains("fn render(self: Self) -> String;")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_trait_impl_blocks() {
        let source = r#"
            pub trait Greeter {
                fn greet(self: Self) -> String;
            }

            pub struct User {
                name: String,
            }

            impl Greeter for User {
                pub fn greet(self) -> String {
                    format!("hi {}", self.name)
                }
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("impl Greeter for User"));
        assert!(output.rust_code.contains("fn greet(self: User) -> String"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_infers_std_trait_method_signature_modifiers() {
        let source = r#"
            use std::fmt;
            use std::fmt::Display;

            pub struct Token {
                value: i64,
            }

            rust {
                fn __fmt_ok() -> std::fmt::Result {
                    Ok(())
                }
            }

            impl Display for Token {
                fn fmt(self, f) -> std::fmt::Result {
                    return __fmt_ok();
                }
            }
        "#;

        let output = compile_source(source).expect("expected std trait signature inference");
        assert!(output.rust_code.contains("impl Display for Token"));
        assert!(
            output.rust_code.contains(
                "fn fmt(self: &Token, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result"
            )
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_infers_dyn_for_trait_object_shorthand_types() {
        let source = r#"
            pub trait Printable {}

            rust {
                impl Printable for String {
                }

                pub fn boxed_printable(v: String) -> Box<dyn Printable> {
                    Box::new(v)
                }
            }

            fn read(value: Printable) -> i64 {
                std::mem::drop(value);
                0
            }

            fn wrap(input: String) -> Printable {
                boxed_printable(input)
            }

            fn run(input: String) -> i64 {
                read(input)
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(
            output
                .rust_code
                .contains("fn read(value: &dyn Printable) -> i64")
        );
        assert!(
            output
                .rust_code
                .contains("fn wrap(input: String) -> Box<dyn Printable>")
        );
        assert!(output.rust_code.contains("return read(&input);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_instance_method_call_syntax_for_impls() {
        let source = r#"
            pub struct Counter { value: i64, }

            impl Counter {
                pub fn inc(self: Self) -> Counter {
                    Counter { value: self.value + 1, }
                }

                pub fn get(self: Self) -> i64 {
                    self.value
                }
            }

            pub fn run(counter: Counter) -> i64 {
                counter.inc().get()
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("counter.inc().get()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_struct_literals() {
        let source = r#"
            pub struct Pair { left: i64, right: i64, }

            fn make_pair(v: i64) -> Pair {
                return Pair {
                    left: v,
                    right: v,
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
        assert!(output.rust_code.contains("values["));
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_vec_index_expressions_with_integer_index_types() {
        let source = r#"
            pub fn at(values: Vec<i64>, idx: u32) -> i64 {
                values[idx]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("values[(idx as usize)]"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_vec_index_expressions_with_signed_index_types() {
        let source = r#"
            pub fn at(values: Vec<i64>, idx: i32) -> i64 {
                values[idx]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("saturating_abs()"));
        assert!(output.rust_code.contains("idx as i32"));
        assert!(output.rust_code.contains("as usize"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_rejects_vec_index_expressions_with_float_index() {
        let source = r#"
            pub fn at(values: Vec<i64>, idx: f64) -> i64 {
                values[idx]
            }
        "#;

        let error = compile_source(source).expect_err("float index should be rejected");
        assert!(
            error
                .to_string()
                .contains("Vector indexing expects integer index (or range), got `f64`")
        );
    }

    #[test]
    fn compile_supports_hashmap_subscript_via_get_option_semantics() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score(scores: HashMap<String, i64>) -> Option<i64> {
                scores["Alice"]
            }
        "#;

        let output = compile_source(source).expect("hash map subscript should lower to get");
        assert!(output.rust_code.contains("scores.get("));
        assert!(output.rust_code.contains("Option<i64>"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_hashmap_from_pairs_then_subscript() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score() -> Option<i64> {
                const pairs = [("Alice", 100), ("Bob", 85)];
                const scores = HashMap::from(pairs);
                return scores["Alice"];
            }
        "#;

        let output = compile_source(source).expect("HashMap::from should preserve map typing");
        assert!(output.rust_code.contains("HashMap::from_iter"));
        assert!(output.rust_code.contains("scores.get("));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_hashmap_from_pairs_then_subscript_in_type_system_mode() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score() -> Option<i64> {
                const pairs = [("Alice", 100), ("Bob", 85)];
                const scores = HashMap::from(pairs);
                return scores["Alice"];
            }
        "#;

        let mut options = CompileOptions::default();
        let output = compile_source_with_options(source, &options)
            .expect("type-system mode should resolve map subscript after HashMap::from");
        assert!(output.rust_code.contains("HashMap::from_iter"));
        assert!(output.rust_code.contains("scores.get("));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_hashmap_get_method() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score(scores: HashMap<String, i64>, key: String) -> Option<i64> {
                return scores.get(key);
            }
        "#;

        let output = compile_source(source).expect("hash map get should compile");
        assert!(output.rust_code.contains("scores.get("));
        assert!(output.rust_code.contains("&key"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_hashmap_subscript_uses_copied_for_copy_values() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score(scores: HashMap<String, i64>, key: String) -> Option<i64> {
                return scores[key];
            }
        "#;

        let output = compile_source(source).expect("hash map subscript should compile");
        assert!(output.rust_code.contains(".get(&key).copied()"));
        assert!(!output.rust_code.contains(".get(&key).cloned()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_hashmap_subscript_uses_cloned_for_non_copy_values() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score(scores: HashMap<String, String>, key: String) -> Option<String> {
                return scores[key];
            }
        "#;

        let output = compile_source(source).expect("hash map subscript should compile");
        assert!(output.rust_code.contains(".get(&key).cloned()"));
        assert!(!output.rust_code.contains(".get(&key).copied()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_custom_get_like_index_capability() {
        let source = r#"
            pub struct Lookup {
                value: i64,
            }

            impl Lookup {
                pub fn get(self, key: String) -> Option<i64> {
                    std::mem::drop(key);
                    return Some(self.value);
                }
            }

            pub fn pick(store: Lookup) -> Option<i64> {
                return store["Alice"];
            }
        "#;

        let output =
            compile_source(source).expect("custom get-like index capability should compile");
        assert!(output.rust_code.contains("Lookup::get("));
        assert!(!output.rust_code.contains("store.get("));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_custom_get_like_index_with_enum_key_without_clone() {
        let source = r#"
            pub enum Key {
                A,
                B,
            }

            pub struct Lookup {
                value: i64,
            }

            impl Lookup {
                pub fn get(self, key: Key) -> Option<i64> {
                    return match key {
                        Key::A => Some(self.value);
                        Key::B => Some(0);
                    };
                }
            }

            pub fn pick(store: Lookup) -> Option<i64> {
                return store[Key::A];
            }
        "#;

        let output = compile_source(source)
            .expect("custom get-like enum key indexing should compile without forced key clone");
        assert!(output.rust_code.contains("Lookup::get("));
        assert!(!output.rust_code.contains("Key::A.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_custom_get_like_index_with_struct_key() {
        let source = r#"
            pub struct Key {
                id: i64,
            }

            pub struct Lookup {
                value: i64,
            }

            impl Lookup {
                pub fn get(self, key: Key) -> Option<i64> {
                    if key.id > 0 {
                        return Some(self.value);
                    }
                    return None;
                }
            }

            pub fn pick(store: Lookup) -> Option<i64> {
                return store[Key { id: 3 }];
            }
        "#;

        let output = compile_source(source)
            .expect("custom get-like struct key indexing should infer key type");
        assert!(output.rust_code.contains("Lookup::get("));
        assert!(output.rust_code.contains("Key { id: 3 }"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_custom_get_like_index_autoclones_reused_owned_key() {
        let source = r#"
            pub struct Lookup {
                value: i64,
            }

            impl Lookup {
                pub fn get(self, key: String) -> Option<i64> {
                    std::mem::drop(key);
                    return Some(self.value);
                }
            }

            pub fn pick(store: Lookup, key: String) -> (Option<i64>, usize) {
                const first = store[key];
                return (first, key.len());
            }
        "#;

        let output = compile_source(source)
            .expect("reused owned key should auto-clone for custom get-like indexing");
        assert!(output.rust_code.contains("Lookup::get("));
        assert!(output.rust_code.contains("key.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_custom_get_like_index_reused_copy_key_does_not_clone() {
        let source = r#"
            pub struct Lookup {
                value: i64,
            }

            impl Lookup {
                pub fn get(self, key: i64) -> Option<i64> {
                    return Some(self.value + key);
                }
            }

            pub fn pick(store: Lookup, key: i64) -> (Option<i64>, i64) {
                const first = store[key];
                return (first, key + 1);
            }
        "#;

        let output = compile_source(source)
            .expect("reused copy key should not require clone for custom get-like indexing");
        assert!(output.rust_code.contains("Lookup::get("));
        assert!(!output.rust_code.contains("key.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_custom_direct_index_autoclones_reused_owned_key() {
        let source = r#"
            pub struct Lookup {}

            impl Lookup {
                pub fn index(self, key: String) -> usize {
                    return key.len();
                }
            }

            pub fn pick(store: Lookup, key: String) -> (usize, usize) {
                const first = store[key];
                return (first, key.len());
            }
        "#;

        let output = compile_source(source)
            .expect("reused owned key should auto-clone for custom direct indexing");
        assert!(output.rust_code.contains("Lookup::index("));
        assert!(output.rust_code.contains("key.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_custom_direct_index_reused_copy_key_does_not_clone() {
        let source = r#"
            pub struct Lookup {}

            impl Lookup {
                pub fn index(self, key: i64) -> i64 {
                    return key;
                }
            }

            pub fn pick(store: Lookup, key: i64) -> (i64, i64) {
                const first = store[key];
                return (first, key + 1);
            }
        "#;

        let output = compile_source(source)
            .expect("reused copy key should not require clone for custom direct indexing");
        assert!(output.rust_code.contains("Lookup::index("));
        assert!(!output.rust_code.contains("key.clone()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_hashmap_subscript_with_enum_key_and_borrowed_lookup() {
        let source = r#"
            rust {
                #[derive(Clone, Debug, Eq, PartialEq, Hash)]
                pub enum Key {
                    Primary,
                    Secondary,
                }
            }

            use std::collections::HashMap;

            pub fn pick(scores: HashMap<Key, i64>) -> Option<i64> {
                return scores[Key::Primary];
            }
        "#;

        let output = compile_source(source).expect("enum-key hashmap subscript should compile");
        assert!(output.rust_code.contains(".get(&Key::Primary)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_custom_direct_index_capability_with_struct_key() {
        let source = r#"
            pub struct Key {
                id: i64,
            }

            pub struct Lookup {}

            impl Lookup {
                pub fn index(self, key: Key) -> i64 {
                    return key.id;
                }
            }

            pub fn pick(store: Lookup) -> i64 {
                return store[Key { id: 9 }];
            }
        "#;

        let output = compile_source(source).expect("custom direct index capability should compile");
        assert!(output.rust_code.contains("Lookup::index("));
        assert!(output.rust_code.contains("Key { id: 9 }"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_rejects_hashmap_subscript_key_type_mismatch() {
        let source = r#"
            use std::collections::HashMap;

            pub fn score(scores: HashMap<String, i64>) -> Option<i64> {
                scores[1]
            }
        "#;

        let error = compile_source(source).expect_err("mismatched hashmap key should fail");
        assert!(
            error
                .to_string()
                .contains("key type mismatch: expected `String`, got `i64`")
        );
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
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert!(output.rust_code.contains(".to_vec()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_vec_slice_range_with_i64_bounds() {
        let source = r#"
            pub fn view(values: Vec<i64>, start: i64, end: i64) -> i64 {
                const middle = values[start..end];
                std::mem::drop(middle);
                values[0]
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("start as i64"));
        assert!(output.rust_code.contains("end as i64"));
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert_rust_code_compiles(&output.rust_code);
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
                Some(i64),
                None,
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
    fn compile_counts_true_guards_for_exhaustiveness() {
        let source = r#"
            fn classify(flag: bool) -> i64 {
                return match flag {
                    true if true => 1;
                    false => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("true => 1"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_ignores_false_guards_for_exhaustiveness() {
        let source = r#"
            fn classify(flag: bool) -> i64 {
                return match flag {
                    true if false => 1;
                    false => 0;
                };
            }
        "#;

        let error = compile_source(source).expect_err("expected non-exhaustive error");
        assert!(error.to_string().contains("Non-exhaustive match on `bool`"));
    }

    #[test]
    fn compile_supports_matching_imported_rust_enums() {
        let source = r#"
            use std::cmp::Ordering;

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

    #[test]
    fn compile_supports_string_escape_sequences() {
        let source = r#"
            pub fn frame() -> String {
                const head = "\x1b[H\n";
                head
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("\\u{1b}[H\\n"));
        assert!(!output.rust_code.contains("\\\\x1b"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_char_literals_in_match() {
        let source = r#"
            fn classify(ch: char) -> i64 {
                return match ch {
                    'a' => 1;
                    'b' => 2;
                    _ => 0;
                };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("match ch"));
        assert!(output.rust_code.contains("'a' => 1"));
        assert!(output.rust_code.contains("'b' => 2"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_iter_map_collect_and_fold_chains() {
        let source = r#"
            fn doubled(values: Vec<i64>) -> Vec<i64> {
                return values.into_iter().map(|x: i64| -> i64 { x * 2 }).collect();
            }

            fn total(values: Vec<i64>) -> i64 {
                return values.into_iter().fold(0, |acc: i64, x: i64| -> i64 { acc + x });
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains(".into_iter().map("));
        assert!(output.rust_code.contains(".collect()"));
        assert!(output.rust_code.contains(".fold("));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_block_expressions() {
        let source = r#"
            fn doubled(x: i64) -> i64 {
                return { const y = x + x; y };
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("return {\n"));
        assert!(output.rust_code.contains("let y: i64 = (x + x);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_to_string_on_non_string_values() {
        let source = r#"
            fn render(v: i64) -> String {
                return v.to_string();
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("v.to_string()"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_numeric_coercion_flag_allows_cross_numeric_calls() {
        let source = r#"
            fn need_i32(v: i32) -> i32 {
                return v;
            }

            fn call() -> i32 {
                return need_i32(0);
            }
        "#;

        let options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("numeric coercion should compile");
        assert!(output.rust_code.contains("need_i32(0)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_numeric_coercion_inserts_widening_cast_for_call_args() {
        let source = r#"
            fn need_u64(v: u64) -> u64 {
                return v;
            }

            fn run(x: u32) -> u64 {
                return need_u64(x);
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("numeric coercion should compile");
        assert!(output.rust_code.contains("need_u64((x as u64))"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_numeric_coercion_uses_saturating_abs_for_signed_to_unsigned() {
        let source = r#"
            fn run(x: i32) -> u32 {
                const y: u32 = x;
                return y;
            }
        "#;

        let mut options = CompileOptions::default();
        let output =
            compile_source_with_options(source, &options).expect("signed->unsigned should compile");
        assert!(output.rust_code.contains("saturating_abs()"));
        assert!(output.rust_code.contains("x as i32"));
        assert!(output.rust_code.contains("as u32"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_numeric_coercion_rejects_narrowing_unsigned_assignment() {
        let source = r#"
            fn run(x: u64) -> u32 {
                const y: u32 = x;
                return y;
            }
        "#;

        let mut options = CompileOptions::default();
        let error = compile_source_with_options(source, &options)
            .expect_err("narrowing unsigned conversion should be rejected");
        assert!(
            error
                .to_string()
                .contains("target `u32` is narrower than `u64`")
        );
    }

    #[test]
    fn compile_ast_supports_explicit_cast_variant() {
        use crate::ast;

        let module = ast::Module {
            items: vec![ast::Item::Function(ast::FunctionDef {
                visibility: ast::Visibility::Private,
                name: "cast_it".to_string(),
                type_params: vec![],
                params: vec![],
                return_type: Some(ast::Type {
                    path: vec!["i32".to_string()],
                    args: vec![],
                    trait_bounds: vec![],
                }),
                effect_row: None,
                span: None,
                body: ast::Block {
                    statements: vec![ast::Stmt::Return(Some(ast::Expr::Cast {
                        expr: Box::new(ast::Expr::Int(7)),
                        target_type: ast::Type {
                            path: vec!["i32".to_string()],
                            args: vec![],
                            trait_bounds: vec![],
                        },
                    }))],
                },
            })],
        };

        let output = compile_ast(&module).expect("cast AST should compile");
        assert!(output.rust_code.contains("return (7 as i32);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_ast_supports_legacy_quiche_cast_macro() {
        use crate::ast;

        let module = ast::Module {
            items: vec![ast::Item::Function(ast::FunctionDef {
                visibility: ast::Visibility::Private,
                name: "cast_macro".to_string(),
                type_params: vec![],
                params: vec![ast::Param {
                    name: "v".to_string(),
                    ty: ast::Type {
                        path: vec!["i64".to_string()],
                        args: vec![],
                        trait_bounds: vec![],
                    },
                }],
                return_type: Some(ast::Type {
                    path: vec!["i32".to_string()],
                    args: vec![],
                    trait_bounds: vec![],
                }),
                effect_row: None,
                span: None,
                body: ast::Block {
                    statements: vec![ast::Stmt::Return(Some(ast::Expr::MacroCall {
                        path: vec!["__quiche_as".to_string()],
                        args: vec![
                            ast::Expr::Path(vec!["v".to_string()]),
                            ast::Expr::Path(vec!["i32".to_string()]),
                        ],
                    }))],
                },
            })],
        };

        let output = compile_ast(&module).expect("legacy quiche cast macro should compile");
        assert!(output.rust_code.contains("return (v as i32);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_ast_closure_last_expr_stmt_is_value_return() {
        use crate::ast;

        let module = ast::Module {
            items: vec![ast::Item::Function(ast::FunctionDef {
                visibility: ast::Visibility::Private,
                name: "run".to_string(),
                type_params: vec![],
                params: vec![],
                return_type: Some(ast::Type {
                    path: vec!["i64".to_string()],
                    args: vec![],
                    trait_bounds: vec![],
                }),
                effect_row: None,
                span: None,
                body: ast::Block {
                    statements: vec![
                        ast::Stmt::Const(ast::ConstDef {
                            visibility: ast::Visibility::Private,
                            name: "inc".to_string(),
                            ty: None,
                            value: ast::Expr::Closure {
                                params: vec![ast::Param {
                                    name: "x".to_string(),
                                    ty: ast::Type {
                                        path: vec!["i64".to_string()],
                                        args: vec![],
                                        trait_bounds: vec![],
                                    },
                                }],
                                return_type: Some(ast::Type {
                                    path: vec!["i64".to_string()],
                                    args: vec![],
                                    trait_bounds: vec![],
                                }),
                                body: ast::Block {
                                    statements: vec![ast::Stmt::Expr(ast::Expr::Binary {
                                        op: ast::BinaryOp::Add,
                                        left: Box::new(ast::Expr::Path(vec!["x".to_string()])),
                                        right: Box::new(ast::Expr::Int(1)),
                                    })],
                                },
                            },
                            is_const: false,
                            span: None,
                        }),
                        ast::Stmt::Return(Some(ast::Expr::Call {
                            callee: Box::new(ast::Expr::Path(vec!["inc".to_string()])),
                            args: vec![ast::Expr::Int(4)],
                        })),
                    ],
                },
            })],
        };

        let output = compile_ast(&module).expect("closure Expr stmt should return value");
        assert!(output.rust_code.contains("|x: i64| -> i64"));
        assert!(output.rust_code.contains("return (x + 1);"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_ast_tolerates_duplicate_impl_self_param_from_bridge() {
        use crate::ast;

        let self_ty = ast::Type {
            path: vec!["Self".to_string()],
            args: vec![],
            trait_bounds: vec![],
        };

        let module = ast::Module {
            items: vec![
                ast::Item::Struct(ast::StructDef {
                    visibility: ast::Visibility::Public,
                    name: "Counter".to_string(),
                    type_params: vec![],
                    span: None,
                    fields: vec![ast::Field {
                        name: "value".to_string(),
                        ty: ast::Type {
                            path: vec!["i64".to_string()],
                            args: vec![],
                            trait_bounds: vec![],
                        },
                    }],
                }),
                ast::Item::Impl(ast::ImplBlock {
                    type_params: vec![],
                    target: "Counter".to_string(),
                    target_args: vec![],
                    trait_target: None,
                    span: None,
                    methods: vec![ast::FunctionDef {
                        visibility: ast::Visibility::Public,
                        name: "get".to_string(),
                        type_params: vec![],
                        params: vec![
                            ast::Param {
                                name: "self".to_string(),
                                ty: self_ty.clone(),
                            },
                            ast::Param {
                                name: "self".to_string(),
                                ty: self_ty,
                            },
                        ],
                        return_type: Some(ast::Type {
                            path: vec!["i64".to_string()],
                            args: vec![],
                            trait_bounds: vec![],
                        }),
                        effect_row: None,
                        span: None,
                        body: ast::Block {
                            statements: vec![ast::Stmt::Return(Some(ast::Expr::Field {
                                base: Box::new(ast::Expr::Path(vec!["self".to_string()])),
                                field: "value".to_string(),
                            }))],
                        },
                    }],
                }),
                ast::Item::Function(ast::FunctionDef {
                    visibility: ast::Visibility::Private,
                    name: "run".to_string(),
                    type_params: vec![],
                    params: vec![ast::Param {
                        name: "counter".to_string(),
                        ty: ast::Type {
                            path: vec!["Counter".to_string()],
                            args: vec![],
                            trait_bounds: vec![],
                        },
                    }],
                    return_type: Some(ast::Type {
                        path: vec!["i64".to_string()],
                        args: vec![],
                        trait_bounds: vec![],
                    }),
                    effect_row: None,
                    span: None,
                    body: ast::Block {
                        statements: vec![ast::Stmt::Return(Some(ast::Expr::Call {
                            callee: Box::new(ast::Expr::Field {
                                base: Box::new(ast::Expr::Path(vec!["counter".to_string()])),
                                field: "get".to_string(),
                            }),
                            args: vec![],
                        }))],
                    },
                }),
            ],
        };

        let output = compile_ast(&module).expect("duplicate bridge self param should be tolerated");
        assert!(output.rust_code.contains("fn get(self: Counter) -> i64"));
        assert!(output.rust_code.contains("return counter.get();"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_ast_allows_vec_index_with_usize() {
        use crate::ast;

        let module = ast::Module {
            items: vec![ast::Item::Function(ast::FunctionDef {
                visibility: ast::Visibility::Private,
                name: "pick".to_string(),
                type_params: vec![],
                params: vec![ast::Param {
                    name: "values".to_string(),
                    ty: ast::Type {
                        path: vec!["Vec".to_string()],
                        args: vec![ast::Type {
                            path: vec!["i64".to_string()],
                            args: vec![],
                            trait_bounds: vec![],
                        }],
                        trait_bounds: vec![],
                    },
                }],
                return_type: Some(ast::Type {
                    path: vec!["i64".to_string()],
                    args: vec![],
                    trait_bounds: vec![],
                }),
                effect_row: None,
                span: None,
                body: ast::Block {
                    statements: vec![ast::Stmt::Return(Some(ast::Expr::Index {
                        base: Box::new(ast::Expr::Path(vec!["values".to_string()])),
                        index: Box::new(ast::Expr::Cast {
                            expr: Box::new(ast::Expr::Int(0)),
                            target_type: ast::Type {
                                path: vec!["usize".to_string()],
                                args: vec![],
                                trait_bounds: vec![],
                            },
                        }),
                    }))],
                },
            })],
        };

        let output = compile_ast(&module).expect("usize indexing should compile");
        assert!(output.rust_code.contains("(0 as usize)"));
        assert!(output.rust_code.contains("values["));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_supports_from_iter_associated_calls() {
        let source = r#"
            use std::collections::HashMap;

            fn build_map(pairs: Vec<(String, i64)>) -> HashMap<String, i64> {
                return HashMap::from_iter(pairs.into_iter());
            }
        "#;

        let output = compile_source(source).expect("from_iter associated call should compile");
        assert!(
            output
                .rust_code
                .contains("HashMap::from_iter(pairs.into_iter())")
        );
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_lowers_strcat_calls_to_runtime_macro() {
        let source = r#"
            fn message(name: String) -> String {
                return strcat("hi ", name);
            }
        "#;

        let output = compile_source(source).expect("strcat calls should type-check");
        assert!(output.rust_code.contains("crate::strcat!("));
    }

    #[test]
    fn compile_preserves_usize_for_add_with_int_literal() {
        let source = r#"
            fn bump(count: usize) -> usize {
                return count + 1;
            }
        "#;

        let output = compile_source(source).expect("usize + int literal should compile");
        assert!(output.rust_code.contains("saturating_abs() as usize"));
        assert!(!output.rust_code.contains("(count as i64)"));
        assert_rust_code_compiles(&output.rust_code);
    }

    #[test]
    fn compile_strips_dead_string_expression_statements() {
        let source = r#"
            fn value() -> i64 {
                "this should be stripped";
                return 1;
            }
        "#;

        let output = compile_source(source).expect("dead string expression should be allowed");
        assert!(!output.rust_code.contains("this should be stripped"));
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
        let seq = TEST_TEMP_SEQ.fetch_add(1, Ordering::Relaxed);
        let base = env::temp_dir().join(format!("elevate-test-{nanos}-{seq}"));
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
