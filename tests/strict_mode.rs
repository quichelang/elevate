use elevate::{CompileOptions, ExperimentFlags, compile_source_with_options};

fn compile_strict(source: &str) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let options = CompileOptions {
        experiments: ExperimentFlags {
            type_system: false,
            ..Default::default()
        },
        ..Default::default()
    };
    compile_source_with_options(source, &options)
}

#[test]
fn strict_mode_reports_unknown_binding_and_suppresses_operator_cascade() {
    let source = r#"
        fn run() -> i64 {
            const left = math::left();
            return left + 2;
        }
    "#;

    let error = compile_strict(source).expect_err("expected strict inference error");
    let rendered = error.to_string();
    assert!(rendered.contains("Cannot infer type for `left` in strict mode"));
    assert!(rendered.contains("--exp-type-system"));
    assert!(!rendered.contains("`+` expects numeric operands"));
    assert_eq!(
        rendered
            .matches("Cannot infer type for `left` in strict mode")
            .count(),
        1
    );
}

#[test]
fn strict_rejects_unknown_inferred_return() {
    let source = r#"
        fn maybe_value() {
            return None;
        }
    "#;

    let error = compile_strict(source).expect_err("expected compile error");
    assert!(
        error
            .to_string()
            .contains("return type could not be fully inferred")
    );
}

#[test]
fn strict_rejects_non_principal_option_return_without_bidi() {
    let source = r#"
        fn maybe(flag: bool) {
            if flag {
                return None;
            }
            return Some(1);
        }
    "#;

    let error = compile_strict(source).expect_err("expected inference error");
    assert!(
        error
            .to_string()
            .contains("return type could not be fully inferred")
    );
}

#[test]
fn strict_rejects_non_principal_result_return_without_bidi() {
    let source = r#"
        fn parse(flag: bool) {
            if flag {
                return Result::Ok(1);
            }
            return Result::Err("bad");
        }
    "#;

    let error = compile_strict(source).expect_err("expected inference error");
    assert!(
        error
            .to_string()
            .contains("return type could not be fully inferred")
    );
}

#[test]
fn strict_numeric_coercion_rejects_cross_numeric_calls() {
    let source = r#"
        fn need_i32(v: i32) -> i32 {
            return v;
        }

        fn call() -> i32 {
            return need_i32(0);
        }
    "#;

    let strict_error = compile_strict(source).expect_err("strict typing should reject");
    assert!(
        strict_error
            .to_string()
            .contains("expected `i32`, got `i64`")
    );
}

#[test]
fn strict_effect_rows_internal_is_flag_gated() {
    let source = r#"
        fn maybe<T>(value: T) -> String {
            value.render()
        }
    "#;

    let output = compile_strict(source).expect("expected gated behavior");
    assert!(output.rust_code.contains("fn maybe<T>(value: T) -> String"));
}
