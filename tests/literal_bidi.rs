use elevate::{CompileOptions, compile_source, compile_source_with_options};

fn compile_with_literal_bidi(
    source: &str,
) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.infer_literal_bidi = true;
    compile_source_with_options(source, &options)
}

#[test]
fn strict_mode_rejects_u64_function_arg_from_default_int_literal() {
    let source = r#"
        fn takes_u64(v: u64) -> u64 {
            return v;
        }

        fn run() -> u64 {
            return takes_u64(7);
        }
    "#;

    let error =
        compile_source(source).expect_err("strict mode should reject i64 literal for u64 arg");
    assert!(error.to_string().contains("expected `u64`, got `i64`"));
}

#[test]
fn literal_bidi_accepts_u64_function_arg_from_default_int_literal() {
    let source = r#"
        fn takes_u64(v: u64) -> u64 {
            return v;
        }

        fn run() -> u64 {
            return takes_u64(7);
        }
    "#;

    let output =
        compile_with_literal_bidi(source).expect("literal bidi should allow literal arg typing");
    assert!(
        output
            .ownership_notes
            .iter()
            .any(|note| note.contains("exp_literal_bidi"))
    );
}

#[test]
fn literal_bidi_accepts_u64_const_from_default_int_literal() {
    let source = r#"
        const LIMIT: u64 = 98;

        fn run() -> u64 {
            return LIMIT;
        }
    "#;

    let _ = compile_with_literal_bidi(source)
        .expect("literal bidi should allow const literal typing by declaration");
}

#[test]
fn result_unwrap_or_helper_accepts_literal_default_with_literal_bidi() {
    let source = r#"
        fn fallback(value: Result<u64, String>) -> u64 {
            return value.unwrap_or(0);
        }
    "#;

    let output = compile_with_literal_bidi(source)
        .expect("Result::unwrap_or should be available with literal default typing");
    assert!(output.rust_code.contains("value.unwrap_or(0)"));
}

#[test]
fn associated_try_from_chain_supports_result_unwrap_or() {
    let source = r#"
        fn fallback(value: i64) -> u64 {
            return u64::try_from(value).unwrap_or(u64::MIN);
        }
    "#;

    let output = compile_source(source)
        .expect("try_from(...).unwrap_or(...) should type-check as Result helper chain");
    assert!(
        output
            .rust_code
            .contains("u64::try_from(value).unwrap_or(u64::MIN)")
    );
}

#[test]
fn bidi_flags_are_mutually_exclusive_in_api_options() {
    let source = r#"
        fn run() -> i64 {
            return 1;
        }
    "#;
    let mut options = CompileOptions::default();
    options.experiments.infer_local_bidi = true;
    options.experiments.infer_literal_bidi = true;

    let error = compile_source_with_options(source, &options)
        .expect_err("mutually exclusive bidi options must be rejected");
    assert!(error.to_string().contains("mutually exclusive"));
}
