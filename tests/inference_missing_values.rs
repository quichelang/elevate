use elevate::{CompileOptions, compile_source_with_options};

fn compile_with_bidi(source: &str) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = true;
    compile_source_with_options(source, &options)
}

fn compile_with_fallback(source: &str) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = true;
    compile_source_with_options(source, &options)
}

fn compile_strict(source: &str) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = false;
    compile_source_with_options(source, &options)
}

#[test]
fn strict_mode_rejects_array_none_some_without_annotation() {
    let source = r#"
        fn values() {
            return [None, Some(1)];
        }
    "#;
    let error = compile_strict(source).expect_err("strict mode should reject");
    assert!(
        error
            .to_string()
            .contains("return type could not be fully inferred")
    );
}

#[test]
fn bidi_infers_array_none_some_to_vec_option_i64() {
    let source = r#"
        fn values() {
            return [None, Some(1)];
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(output.rust_code.contains("fn values() -> Vec<Option<i64>>"));
}

#[test]
fn bidi_infers_local_binding_with_missing_array_element_types() {
    let source = r#"
        fn values() {
            const xs = [None, Some(1)];
            return xs;
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(output.rust_code.contains("fn values() -> Vec<Option<i64>>"));
    assert!(
        output
            .rust_code
            .contains("let xs: Vec<Option<i64>> = vec![None, Some(1)];")
    );
}

#[test]
fn bidi_infers_result_array_from_ok_err() {
    let source = r#"
        fn values() {
            return [Result::Ok(1), Result::Err("bad")];
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(
        output
            .rust_code
            .contains("fn values() -> Vec<Result<i64, String>>")
    );
}

#[test]
fn bidi_infers_nested_option_result_array() {
    let source = r#"
        fn values() {
            return [Some(Result::Ok(1)), Some(Result::Err("bad"))];
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer nested type");
    assert!(
        output
            .rust_code
            .contains("fn values() -> Vec<Option<Result<i64, String>>>")
    );
}

#[test]
fn bidi_infers_reordered_option_branches() {
    let source = r#"
        fn choose(flag: bool) {
            if flag {
                return None;
            }
            return Some(1);
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(
        output
            .rust_code
            .contains("fn choose(flag: bool) -> Option<i64>")
    );
}

#[test]
fn bidi_infers_reordered_result_branches() {
    let source = r#"
        fn choose(flag: bool) {
            if flag {
                return Result::Err("bad");
            }
            return Result::Ok(1);
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(
        output
            .rust_code
            .contains("fn choose(flag: bool) -> Result<i64, String>")
    );
}

#[test]
fn bidi_infers_option_through_match_arms() {
    let source = r#"
        fn choose(flag: bool) {
            return match flag {
                true => None;
                false => Some(1);
            };
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer");
    assert!(
        output
            .rust_code
            .contains("fn choose(flag: bool) -> Option<i64>")
    );
}

#[test]
fn bidi_rejects_all_none_without_signal() {
    let source = r#"
        fn values() {
            return [None, None];
        }
    "#;
    let error = compile_with_bidi(source).expect_err("should still require annotation");
    let rendered = error.to_string();
    assert!(rendered.contains("Principal fallback:"));
    assert!(rendered.contains("function `values`"));
}

#[test]
fn bidi_rejects_err_only_without_ok_type_signal() {
    let source = r#"
        fn values() {
            return [Result::Err("bad")];
        }
    "#;
    let error = compile_with_bidi(source).expect_err("should still require annotation");
    let rendered = error.to_string();
    assert!(rendered.contains("Principal fallback:"));
    assert!(rendered.contains("function `values`"));
}

#[test]
fn bidi_rejects_incompatible_missing_type_families() {
    let source = r#"
        fn values(flag: bool) {
            if flag {
                return Some(1);
            }
            return Result::Err("bad");
        }
    "#;
    let error = compile_with_bidi(source).expect_err("should reject incompatible families");
    assert!(
        error
            .to_string()
            .contains("Cannot infer single return type for `values`")
    );
}

#[test]
fn principal_fallback_points_to_function_annotation() {
    let source = r#"
        fn maybe_value() {
            return None;
        }
    "#;
    let error = compile_with_fallback(source).expect_err("expected fallback guidance");
    let rendered = error.to_string();
    assert!(rendered.contains("Principal fallback:"));
    assert!(rendered.contains("function `maybe_value`"));
}

#[test]
fn principal_fallback_points_to_const_annotation() {
    let source = r#"
        const EMPTY = None;
    "#;
    let error = compile_with_fallback(source).expect_err("expected fallback guidance");
    let rendered = error.to_string();
    assert!(rendered.contains("Principal fallback:"));
    assert!(rendered.contains("const `EMPTY`"));
}

#[test]
fn strict_mode_rejects_implicit_assignment_binding() {
    let source = r#"
        fn add(a: i64, b: i64) -> i64 {
            return a + b;
        }

        fn main() {
            x = add(3, 4);
            return;
        }
    "#;
    let error = compile_strict(source).expect_err("strict mode should reject undeclared target");
    assert!(error.to_string().contains("Unknown assignment target `x`"));
}

#[test]
fn bidi_mode_accepts_implicit_assignment_binding() {
    let source = r#"
        fn add(a: i64, b: i64) -> i64 {
            return a + b;
        }

        fn main() {
            x = add(3, 4);
            return;
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer implicit local");
    assert!(output.rust_code.contains("let x: i64 = add(3, 4);"));
}

#[test]
fn strict_mode_rejects_placeholder_param_in_arithmetic() {
    let source = r#"
        fn add(a: _, b: i64) -> i64 {
            return a + b;
        }

        fn main() -> i64 {
            return add(3, 4);
        }
    "#;
    let error = compile_strict(source).expect_err("strict mode should reject unresolved param");
    assert!(
        error
            .to_string()
            .contains("Cannot infer type for `a` in strict mode")
    );
}

#[test]
fn bidi_mode_infers_placeholder_param_from_arithmetic_context() {
    let source = r#"
        fn add(a: _, b: i64) -> i64 {
            return a + b;
        }

        fn main() -> i64 {
            return add(3, 4);
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer param");
    assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
}

#[test]
fn bidi_mode_infers_multiple_placeholder_params() {
    let source = r#"
        fn add(a: _, b: _) -> i64 {
            return a + b;
        }

        fn main() -> i64 {
            return add(3, 4);
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer both params");
    assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
}

#[test]
fn bidi_mode_infers_omitted_param_types() {
    let source = r#"
        fn add(a, b) -> i64 {
            return a + b;
        }

        fn main() -> i64 {
            return add(3, 4);
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer omitted param types");
    assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
}

#[test]
fn bidi_mode_infers_params_and_return_without_return_annotation() {
    let source = r#"
        fn add(a, b) {
            return a + b;
        }

        fn main() {
            x = add(3, 4);
            return;
        }
    "#;
    let output = compile_with_bidi(source).expect("bidi mode should infer missing function types");
    assert!(output.rust_code.contains("fn add(a: i64, b: i64) -> i64"));
    assert!(output.rust_code.contains("let x: i64 = add(3, 4);"));
    assert!(
        !output
            .rust_code
            .contains("fn add(a: String, b: String) -> String")
    );
}

#[test]
fn strict_mode_rejects_missing_add_types_without_bidi() {
    let source = r#"
        fn add(a, b) {
            return a + b;
        }

        fn main() {
            return;
        }
    "#;
    let error =
        compile_strict(source).expect_err("strict mode should reject unresolved add signature");
    assert!(
        error
            .to_string()
            .contains("Cannot infer type for `a` in strict mode")
    );
}
