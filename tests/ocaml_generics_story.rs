use elevate::{CompileOptions, compile_source, compile_source_with_options};

fn compile_with_ocaml_profile(
    source: &str,
) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    // OCaml-like inference profile:
    // - local bidirectional inference
    // - principal fallback diagnostics
    // - numeric coercion for expected-type literal steering
    // - effect-row analysis (surface + internal)
    options.experiments.type_system = true;
    compile_source_with_options(source, &options)
}

// Checklist Story: Section 1
// Goal: explicit type application in expression position.
#[test]
fn explicit_type_application_supports_generic_function_call() {
    let source = r#"
        fn id<T>(value: T) -> T {
            value
        }

        fn run() -> i64 {
            const a = id<i64>(7);
            return a;
        }
    "#;

    let output = compile_with_ocaml_profile(source).expect("explicit type application should compile");
    assert!(output.rust_code.contains("fn id<T>(value: T) -> T"));
}

#[test]
fn explicit_type_application_supports_associated_constructor_calls() {
    let source = r#"
        use std::fmt::Display;

        struct Point<T> {
            x: T;
            y: T;
        }

        impl<T: Display> Point<T> {
            fn new(x: T, y: T) -> Self {
                Point { x: x; y: y; }
            }

            fn render(self) -> String {
                format!("({}, {})", self.x, self.y)
            }
        }

        fn run() -> String {
            const i = Point<i32>::new(5, 6);
            const f = Point<f64>::new(2.5, 3.5);
            return i.render() + f.render();
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("explicit associated generic calls should compile");
    assert!(output.rust_code.contains("Point::new(5, 6)"));
    assert!(output.rust_code.contains("Point::new(2.5, 3.5)"));
}

#[test]
fn explicit_type_application_reports_arity_mismatches() {
    let source = r#"
        fn id<T>(value: T) -> T {
            value
        }

        fn run() -> i64 {
            return id<i64, i32>(7);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("wrong explicit type-arg arity should be rejected");
    assert!(
        error
            .to_string()
            .contains("expects 1 explicit type argument(s), got 2")
    );
}

// Checklist Story: Section 2
// Goal: implicit polymorphism across competing concrete types.
#[test]
fn implicit_polymorphism_handles_int_uint_bool_and_vectors() {
    let source = r#"
        fn keep<T>(value: T) -> T {
            value
        }

        fn run() {
            const a = keep(-7);
            const b = keep(u64::from(9));
            const c = keep(true);
            const d = keep([1, 2, 3]);
            const e = keep([[1, 2], [3, 4]]);
            std::mem::drop(a);
            std::mem::drop(b);
            std::mem::drop(c);
            std::mem::drop(d);
            std::mem::drop(e);
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("polymorphic keep<T> should handle diverse call sites");
    assert!(output.rust_code.contains("fn keep<T>(value: T) -> T"));
}

#[test]
fn structural_polymorphism_handles_nested_object_properties() {
    let source = r#"
        struct Child {
            values: Vec<i64>;
        }

        struct Parent {
            child: Child;
            flags: Vec<bool>;
        }

        fn first_value(obj: Parent) -> i64 {
            return obj.child.values[0];
        }

        fn has_flag(obj: Parent) -> bool {
            return obj.flags[0];
        }

        fn run() -> bool {
            const p = Parent {
                child: Child { values: [3, 9] };
                flags: [true, false];
            };
            return has_flag(p) and first_value(p) > 0;
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("structural generic access should compile for nested fields");
    assert!(output.rust_code.contains("fn first_value(obj: Parent) -> i64"));
    assert!(output.rust_code.contains("fn has_flag(obj: Parent) -> bool"));
}

#[test]
fn structural_generic_nested_object_access_compiles_in_profile_mode() {
    let source = r#"
        struct Child {
            values: Vec<i64>;
        }

        struct Parent {
            child: Child;
        }

        fn first_value<T>(obj: T) -> i64 {
            return obj.child.values[0];
        }

        fn run() -> i64 {
            const p = Parent { child: Child { values: [3, 9] } };
            return first_value(p);
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("nested structural generic field access should compile in profile mode");
    assert!(output.rust_code.contains("__elevate_row_first_value_"));
}

// Checklist Story: Section 3
// Goal: bidirectional inference should use expected types in both strict and profile comparisons.
#[test]
fn strict_mode_rejects_expected_u64_without_profile() {
    let source = r#"
        fn take_u64(v: u64) -> u64 {
            return v;
        }

        fn run() -> u64 {
            return take_u64(7);
        }
    "#;

    let error = compile_source(source).expect_err("strict mode should reject default i64 literal");
    assert!(error.to_string().contains("expected `u64`, got `i64`"));
}

#[test]
fn ocaml_profile_accepts_expected_u64_with_bidirectional_numeric_inference() {
    let source = r#"
        fn take_u64(v: u64) -> u64 {
            return v;
        }

        fn run() -> u64 {
            return take_u64(7);
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("profile should steer literal to expected u64");
    assert!(
        output
            .ownership_notes
            .iter()
            .any(|note| note.contains("exp_type_system"))
    );
}

#[test]
fn ocaml_profile_propagates_expected_type_through_match_arms() {
    let source = r#"
        fn choose(flag: bool) {
            return match flag {
                true => None;
                false => Some(1);
            };
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("profile should infer Option<i64> across match arms");
    assert!(output.rust_code.contains("fn choose(flag: bool) -> Option<i64>"));
}

// Checklist Story: Section 4
// Goal: numeric defaulting should be deterministic under expected-type pressure.
#[test]
fn ocaml_profile_allows_explicit_unsigned_targeting_in_nested_calls() {
    let source = r#"
        fn lift(v: u64) -> u64 {
            return v + 1;
        }

        fn run() -> u64 {
            const xs = [1, 2, 3];
            return lift(xs[0]);
        }
    "#;

    let _ = compile_with_ocaml_profile(source)
        .expect("profile should coerce nested literal/index flow to u64 target");
}

// Checklist Story: Section 5
// Goal: explicit and implicit polymorphism should compose in one program.
#[test]
fn mixed_explicit_and_implicit_polymorphism_compose() {
    let source = r#"
        fn id<T>(value: T) -> T {
            value
        }

        fn pair<T>(left: T, right: T) -> (T, T) {
            return (left, right);
        }

        fn run() {
            const a = id<i64>(9);
            const b = id(true);
            const c = pair([1, 2], [3, 4]);
            std::mem::drop(a);
            std::mem::drop(b);
            std::mem::drop(c);
        }
    "#;

    let _ = compile_with_ocaml_profile(source)
        .expect("mixed explicit/implicit polymorphism should compile");
}

// Checklist Story: Section 6
// Goal: principal fallback should still guide users when inference has no signal.
#[test]
fn ocaml_profile_preserves_principal_fallback_guidance() {
    let source = r#"
        fn unresolved() {
            return None;
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("principal fallback should still report unresolved holes");
    assert!(error.to_string().contains("Principal fallback:"));
}

// Checklist Story: Section 7
// Goal: integrated scenario with nested vectors + objects + explicit type app.
#[test]
fn integrated_story_case_exercises_multiple_ocaml_like_patterns() {
    let source = r#"
        use std::fmt::Display;

        struct Boxed<T> {
            value: T;
        }

        struct Scene {
            points: Vec<Vec<i64>>;
            active: bool;
        }

        impl<T: Display> Boxed<T> {
            fn new(value: T) -> Self {
                Boxed { value: value; }
            }

            fn show(self) -> String {
                format!("{}", self.value)
            }
        }

        fn pick_first(scene: Scene) -> i64 {
            return scene.points[0][0];
        }

        fn run() -> String {
            const scene = Scene {
                points: [[1, 2], [3, 4]];
                active: true;
            };
            const left = Boxed<i64>::new(pick_first(scene));
            const right = Boxed::new(u64::from(7));
            return left.show() + right.show();
        }
    "#;

    let _ = compile_with_ocaml_profile(source)
        .expect("integrated OCaml-style story case should compile");
}
