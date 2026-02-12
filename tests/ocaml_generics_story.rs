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

    let output =
        compile_with_ocaml_profile(source).expect("explicit type application should compile");
    assert!(output.rust_code.contains("fn id<T>(value: T) -> T"));
}

#[test]
fn explicit_type_application_supports_associated_constructor_calls() {
    let source = r#"
        use std::fmt::Display;

        struct Point<T> {
            x: T,
            y: T,
        }

        impl<T: Display> Point<T> {
            fn new(x: T, y: T) -> Self {
                Point { x: x, y: y, }
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
            values: Vec<i64>,
        }

        struct Parent {
            child: Child,
            flags: Vec<bool>,
        }

        fn first_value(obj: Parent) -> i64 {
            return obj.child.values[0];
        }

        fn has_flag(obj: Parent) -> bool {
            return obj.flags[0];
        }

        fn run() -> bool {
            const p = Parent {
                child: Child { values: [3, 9] },
                flags: [true, false],
            };
            return has_flag(p) and first_value(p) > 0;
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("structural generic access should compile for nested fields");
    assert!(
        output
            .rust_code
            .contains("fn first_value(obj: Parent) -> i64")
    );
    assert!(
        output
            .rust_code
            .contains("fn has_flag(obj: Parent) -> bool")
    );
}

#[test]
fn structural_generic_nested_object_access_compiles_in_profile_mode() {
    let source = r#"
        struct Child {
            values: Vec<i64>,
        }

        struct Parent {
            child: Child,
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

    let output =
        compile_with_ocaml_profile(source).expect("profile should steer literal to expected u64");
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
    assert!(
        output
            .rust_code
            .contains("fn choose(flag: bool) -> Option<i64>")
    );
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
            value: T,
        }

        struct Scene {
            points: Vec<Vec<i64>>,
            active: bool,
        }

        impl<T: Display> Boxed<T> {
            fn new(value: T) -> Self {
                Boxed { value: value, }
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
                points: [[1, 2], [3, 4]],
                active: true,
            };
            const left = Boxed<i64>::new(pick_first(scene));
            const right = Boxed::new(u64::from(7));
            return left.show() + right.show();
        }
    "#;

    let _ = compile_with_ocaml_profile(source)
        .expect("integrated OCaml-style story case should compile");
}

// Checklist Story: Section 8
// Goal: numeric policy matrix should stay explicit for non-literals while keeping literal ergonomics.
#[test]
fn ocaml_profile_accepts_contextual_uint_literals_across_widths() {
    let source = r#"
        fn need_u8(v: u8) -> u8 { return v; }
        fn need_u16(v: u16) -> u16 { return v; }
        fn need_u32(v: u32) -> u32 { return v; }
        fn need_u64(v: u64) -> u64 { return v; }
        fn need_usize(v: usize) -> usize { return v; }

        fn run() -> usize {
            const a = need_u8(7);
            const b = need_u16(9);
            const c = need_u32(11);
            const d = need_u64(13);
            const e = need_usize(15);
            std::mem::drop(a);
            std::mem::drop(b);
            std::mem::drop(c);
            std::mem::drop(d);
            return e;
        }
    "#;

    let _ = compile_with_ocaml_profile(source)
        .expect("contextual typing for uint literal arguments should work in profile mode");
}

#[test]
fn ocaml_profile_rejects_implicit_int_to_float_argument_coercion() {
    let source = r#"
        fn need_f64(v: f64) -> f64 {
            return v;
        }

        fn run() -> f64 {
            return need_f64(7);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("whole-number int should not auto-coerce to float");
    let rendered = error.to_string();
    assert!(rendered.contains("expected `f64`"));
    assert!(rendered.contains("got `i64`"));
}

#[test]
fn strict_mode_rejects_bool_where_uint_is_expected() {
    let source = r#"
        fn need_u64(v: u64) -> u64 {
            return v;
        }

        fn run() -> u64 {
            return need_u64(true);
        }
    "#;

    let error = compile_source(source).expect_err("bool should not coerce to uint");
    let rendered = error.to_string();
    assert!(rendered.contains("expected `u64`"));
    assert!(rendered.contains("got `bool`"));
}

#[test]
fn strict_mode_rejects_vector_member_type_mismatch_for_uint_parameter() {
    let source = r#"
        fn first_u64(values: Vec<u64>) -> u64 {
            return values[0];
        }

        fn run() -> u64 {
            const flags = [true, false];
            return first_u64(flags);
        }
    "#;

    let error = compile_source(source).expect_err("Vec<bool> should not pass as Vec<u64>");
    let rendered = error.to_string();
    assert!(rendered.contains("expected `Vec<u64>`"));
    assert!(rendered.contains("got `Vec<bool>`"));
}

#[test]
fn strict_mode_rejects_struct_member_bool_for_uint_field() {
    let source = r#"
        struct Metrics {
            count: u64,
        }

        fn run() -> u64 {
            const m = Metrics { count: true };
            return m.count;
        }
    "#;

    let error = compile_source(source).expect_err("bool should not satisfy struct uint field");
    let rendered = error.to_string();
    assert!(rendered.contains("Struct field `count` expected `u64`, got `bool`"));
}

#[test]
fn explicit_conversion_try_from_accepts_non_literal_signed_value() {
    let source = r#"
        fn to_u64(value: i64) -> u64 {
            return u64::try_from(value).unwrap_or(u64::MIN);
        }
    "#;

    let output = compile_source(source).expect("explicit conversion path should compile");
    assert!(
        output
            .rust_code
            .contains("u64::try_from(value).unwrap_or(u64::MIN)")
    );
}

#[test]
fn explicit_conversion_try_from_accepts_struct_member_value() {
    let source = r#"
        struct Input {
            raw: i64,
        }

        fn to_u64(input: Input) -> u64 {
            return u64::try_from(input.raw).unwrap_or(u64::MIN);
        }
    "#;

    let output = compile_source(source).expect("struct member conversion should compile");
    assert!(
        output
            .rust_code
            .contains("u64::try_from(input.raw).unwrap_or(u64::MIN)")
    );
}

#[test]
fn explicit_conversion_try_from_accepts_vector_member_value() {
    let source = r#"
        fn first_u64(values: Vec<i64>) -> u64 {
            return u64::try_from(values[0]).unwrap_or(u64::MIN);
        }
    "#;

    let output = compile_source(source).expect("vector member conversion should compile");
    assert!(output.rust_code.contains("u64::try_from(values["));
    assert!(output.rust_code.contains("saturating_abs() as usize"));
    assert!(output.rust_code.contains("unwrap_or(u64::MIN)"));
}

#[test]
fn ocaml_profile_rejects_bool_vector_index_for_get() {
    let source = r#"
        fn run(values: Vec<i64>) -> Option<i64> {
            return values.get(true);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("bool index should be rejected even in profile mode");
    assert!(
        error
            .to_string()
            .contains("Method `Vec::get` expects integer index, got `bool`")
    );
}

#[test]
fn ocaml_profile_rejects_bool_direct_indexing() {
    let source = r#"
        fn run(values: Vec<i64>) -> i64 {
            return values[true];
        }
    "#;

    let error =
        compile_with_ocaml_profile(source).expect_err("bool index should be rejected in [] form");
    assert!(
        error
            .to_string()
            .contains("Vector indexing expects integer index (or range), got `bool`")
    );
}

// Checklist Story: Section 9
// Goal: whole-number coercion should work beyond index expressions, with safe widening rules.
#[test]
fn ocaml_profile_accepts_widening_integral_conversions_in_variables_and_calls() {
    let source = r#"
        fn need_u64(v: u64) -> u64 { return v; }
        fn need_i128(v: i128) -> i128 { return v; }

        fn run(u: u32, s: i16, idx: i32, values: Vec<i64>) -> (u64, i128, i64) {
            const a: u64 = u;
            const b: i128 = s;
            const c: usize = idx;
            return (need_u64(a), need_i128(b), values[c]);
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("widening whole-number conversions should compile in profile mode");
    assert!(output.rust_code.contains("saturating_abs"));
    assert!(output.rust_code.contains("as usize"));
}

#[test]
fn ocaml_profile_rejects_narrowing_unsigned_target() {
    let source = r#"
        fn run(v: u64) -> u32 {
            const out: u32 = v;
            return out;
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("narrowing whole-number conversion should be rejected");
    let rendered = error.to_string();
    assert!(rendered.contains("expected `u32`"));
    assert!(rendered.contains("got `u64`"));
    assert!(rendered.contains("narrower"));
}

#[test]
fn ocaml_profile_rejects_equal_width_unsigned_to_signed() {
    let source = r#"
        fn need_i64(v: i64) -> i64 {
            return v;
        }

        fn run(v: u64) -> i64 {
            return need_i64(v);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("u64 to i64 should require explicit conversion");
    let rendered = error.to_string();
    assert!(rendered.contains("expected `i64`"));
    assert!(rendered.contains("got `u64`"));
    assert!(rendered.contains("wider signed target"));
}

#[test]
fn ocaml_profile_rejects_implicit_float_int_binary_mix() {
    let source = r#"
        fn run(a: i64, b: f64) -> f64 {
            return a + b;
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("implicit float/int arithmetic should require explicit cast");
    assert!(error.to_string().contains("expects numeric operands"));
}

// Checklist Story: Section 10 (Pending)
// Goal: diagnostics should suggest abs(i*) guidance for signed->uint/index-intent paths.
#[test]
#[ignore = "pending: add abs(i*) guidance in numeric mismatch diagnostics"]
fn pending_diagnostic_mentions_abs_for_signed_to_unsigned_assignment() {
    let source = r#"
        fn need_u64(v: u64) -> u64 {
            return v;
        }

        fn run(x: i64) -> u64 {
            return need_u64(x);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("pending abs(i*) diagnostics should reject signed-to-unsigned mismatch");
    assert!(error.to_string().contains("abs("));
}

#[test]
#[ignore = "pending: add abs(i*) guidance for index-intent coercion hints"]
fn pending_diagnostic_mentions_abs_for_index_intent_assignment() {
    let source = r#"
        fn take_usize(v: usize) -> usize {
            return v;
        }

        fn run(values: Vec<i64>) -> usize {
            const idx = values[0];
            return take_usize(idx);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("pending abs(i*) diagnostics should reject index-intent signed mismatch");
    assert!(error.to_string().contains("abs("));
}

// Checklist Story: Section 11 (Capability Story)
// Goal: capability-driven map/method/index behavior composes with OCaml-profile inference.
#[test]
fn capability_story_map_subscript_returns_option_and_composes_with_nested_vectors() {
    let source = r#"
        use std::collections::HashMap;

        fn pick(store: HashMap<String, Vec<Vec<i64>>>) -> Option<Vec<Vec<i64>>> {
            return store["board"];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("map subscript should lower through get-like option capability");
    assert!(output.rust_code.contains(".get("));
    assert!(output.rust_code.contains("Option<Vec<Vec<i64>>>"));
}

#[test]
fn capability_story_map_get_reports_key_type_mismatch_with_expected_actual() {
    let source = r#"
        use std::collections::HashMap;

        fn pick(scores: HashMap<String, i64>) -> Option<i64> {
            return scores.get(true);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("bool key should fail against String-key map capability");
    let rendered = error.to_string();
    assert!(rendered.contains("Arg 1 for method `get`"));
    assert!(rendered.contains("expected `String`"));
    assert!(rendered.contains("got `bool`"));
}

#[test]
fn capability_story_mixed_generics_map_vector_numeric_coercion_and_ownership() {
    let source = r#"
        use std::collections::HashMap;

        fn pick(cache: HashMap<String, Vec<i64>>, idx: i32) -> Option<i64> {
            const selected = match cache.get("nums") {
                Some(values) => Some(values[idx]);
                None => Some(0);
            };
            return selected;
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("mixed map/vector/index coercion path should compile in profile mode");
    assert!(output.rust_code.contains(".get("));
    assert!(output.rust_code.contains("as usize"));
}

#[test]
fn capability_story_custom_type_method_without_capability_reports_elevate_error() {
    let source = r#"
        struct Bag {
            value: i64,
        }

        fn run(bag: Bag) -> i64 {
            return bag.get(0);
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("custom type without get capability should fail at Elevate layer");
    assert!(
        error
            .to_string()
            .contains("Capability resolution failed for method `get` on `Bag`")
    );
}

#[test]
fn capability_story_inferred_map_via_from_iter_supports_get_method_path() {
    let source = r#"
        use std::collections::HashMap;

        fn run() -> Option<i64> {
            const pairs = [("Alice", 100), ("Bob", 85)];
            const scores = HashMap::from_iter(pairs.into_iter());
            return scores.get("Alice");
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("inferred map value via from_iter should still resolve get capability");
    assert!(output.rust_code.contains("HashMap::from_iter"));
    assert!(output.rust_code.contains(".get("));
}

// Checklist Story: Section 12 (Strict Mode Robustness)
// Goal: strict mode (no --exp-type-system) must keep auto-borrow/auto-clone behavior robust.
#[test]
fn strict_mode_still_autoborrows_string_contains() {
    let source = r#"
        fn probe(text: String, needle: String) -> bool {
            return text.contains(needle);
        }
    "#;

    let output =
        compile_source(source).expect("strict mode should keep string method autoborrow behavior");
    assert!(output.rust_code.contains("text.contains(&needle)"));
}

#[test]
fn strict_mode_still_autoclones_reused_owned_receiver_chain() {
    let source = r#"
        fn measure(text: String) -> usize {
            return text.into_bytes().len() + text.into_bytes().len();
        }
    "#;

    let output = compile_source(source)
        .expect("strict mode should keep auto-clone behavior for reused owned receivers");
    assert!(output.rust_code.contains("into_bytes()"));
    assert!(
        output
            .ownership_notes
            .iter()
            .any(|note| note.contains("auto-clone") || note.contains("hot-clone"))
    );
}

// Checklist Story: Section 13 (Advanced Index Key Inference)
// Goal: map/custom indexing should infer key types for non-primitive and enum keys.
#[test]
fn capability_story_hashmap_enum_key_index_infers_and_borrows_key() {
    let source = r#"
        rust {
            #[derive(Clone, Debug, Eq, PartialEq, Hash)]
            pub enum Key {
                Primary,
                Secondary,
            }
        }

        use std::collections::HashMap;

        fn pick(scores: HashMap<Key, i64>) -> Option<i64> {
            return scores[Key::Primary];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("HashMap enum key subscript should infer and borrow key");
    assert!(output.rust_code.contains(".get(&Key::Primary)"));
}

#[test]
fn capability_story_hashmap_struct_key_index_infers_and_borrows_key() {
    let source = r#"
        rust {
            #[derive(Clone, Debug, Eq, PartialEq, Hash)]
            pub struct Key {
                pub id: i64,
            }
        }

        use std::collections::HashMap;

        fn pick(scores: HashMap<Key, i64>) -> Option<i64> {
            return scores[Key { id: 1 }];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("HashMap struct key subscript should infer and borrow key");
    assert!(output.rust_code.contains(".get(&Key"));
    assert!(output.rust_code.contains("id: 1"));
}

#[test]
fn capability_story_custom_get_index_with_enum_key_infers_key_type() {
    let source = r#"
        enum Key {
            A,
            B,
        }

        struct Lookup {
            value: i64,
        }

        impl Lookup {
            fn get(self, key: Key) -> Option<i64> {
                return match key {
                    Key::A => Some(self.value);
                    Key::B => Some(0);
                };
            }
        }

        fn pick(store: Lookup) -> Option<i64> {
            return store[Key::A];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("custom get-like index should infer enum key type");
    assert!(output.rust_code.contains("Lookup::get("));
    assert!(output.rust_code.contains("Key::A"));
    assert!(!output.rust_code.contains("Key::A.clone()"));
}

#[test]
fn capability_story_custom_get_index_with_struct_key_infers_key_type() {
    let source = r#"
        struct Key {
            id: i64,
        }

        struct Lookup {
            value: i64,
        }

        impl Lookup {
            fn get(self, key: Key) -> Option<i64> {
                if key.id > 0 {
                    return Some(self.value);
                }
                return None;
            }
        }

        fn pick(store: Lookup) -> Option<i64> {
            return store[Key { id: 2 }];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("custom get-like index should infer struct key type");
    assert!(output.rust_code.contains("Lookup::get("));
    assert!(output.rust_code.contains("Key { id: 2 }"));
}

#[test]
fn capability_story_custom_direct_index_with_struct_key_infers_key_type() {
    let source = r#"
        struct Key {
            id: i64,
        }

        struct Lookup {}

        impl Lookup {
            fn index(self, key: Key) -> i64 {
                return key.id;
            }
        }

        fn pick(store: Lookup) -> i64 {
            return store[Key { id: 7 }];
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("custom direct index should infer struct key type");
    assert!(output.rust_code.contains("Lookup::index("));
    assert!(output.rust_code.contains("Key { id: 7 }"));
}

#[test]
fn capability_story_custom_get_index_reused_owned_key_triggers_clone_support() {
    let source = r#"
        struct Lookup {
            value: i64,
        }

        impl Lookup {
            fn get(self, key: String) -> Option<i64> {
                std::mem::drop(key);
                return Some(self.value);
            }
        }

        fn pick(store: Lookup, key: String) -> (Option<i64>, usize) {
            const first = store[key];
            return (first, key.len());
        }
    "#;

    let output = compile_with_ocaml_profile(source)
        .expect("reused owned key should compile with clone support for custom get-like indexing");
    assert!(output.rust_code.contains("Lookup::get("));
    assert!(output.rust_code.contains("key.clone()"));
}

#[test]
fn ocaml_profile_reports_constraint_groups_for_conflicting_return_types() {
    let source = r#"
        fn choose(flag: bool) {
            if flag {
                return 1;
            }
            return "x";
        }
    "#;

    let error = compile_with_ocaml_profile(source)
        .expect_err("conflicting return types should emit constraint partition diagnostics");
    let rendered = error.to_string();
    assert!(rendered.contains("Cannot infer single return type for `choose`"));
    assert!(rendered.contains("Constraint groups for return type for `choose` are incompatible"));
    assert!(rendered.contains("Principal fallback:"));
    assert!(rendered.contains("Principal fallback selected `"));
}
