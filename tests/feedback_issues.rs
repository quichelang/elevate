use elevate::compile_source;
use std::env;
use std::fs;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

// ═══════════════════════════════════════════════════════════════════════════
// Issue #1: Vec<String> Indexing Generates Move Instead of Clone (E0507)
//
// Any Vec<NonCopy> subscript should emit .clone() on the element, since
// indexing a Vec returns a reference in Rust — you can't move out of it.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #1: Vec<String> subscript emits move instead of clone"]
fn issue1_vec_string_subscript_should_clone() {
    let source = r#"
        fn run(values: Vec<String>) -> String {
            const val: String = values[0];
            return val;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "expected .clone() on Vec<String> subscript\n{}",
        output.rust_code
    );
}

#[test]
#[ignore = "feedback #1: Vec<Vec<i64>> subscript should clone inner vec"]
fn issue1_vec_of_vec_subscript_should_clone() {
    let source = r#"
        fn run(rows: Vec<Vec<i64>>) -> Vec<i64> {
            const row: Vec<i64> = rows[0];
            return row;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "expected .clone() on Vec<Vec<i64>> subscript\n{}",
        output.rust_code
    );
}

#[test]
#[ignore = "feedback #1: struct field Vec<String> subscript should clone"]
fn issue1_struct_field_vec_string_subscript_should_clone() {
    let source = r#"
        pub struct Names { items: Vec<String>, }

        fn run(names: Names) -> String {
            const first: String = names.items[0];
            return first;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "expected .clone() on struct member Vec<String> subscript\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #1: nested struct Vec subscript should clone"]
fn issue1_nested_struct_vec_subscript_should_clone() {
    let source = r#"
        pub struct Inner { labels: Vec<String>, }
        pub struct Outer { inner: Inner, }

        fn run(outer: Outer) -> String {
            const label: String = outer.inner.labels[0];
            return label;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "expected .clone() on nested struct Vec subscript\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
fn issue1_vec_i64_subscript_should_not_clone() {
    // Copy types should NOT get .clone() — this should already work
    let source = r#"
        fn run(values: Vec<i64>) -> i64 {
            const val: i64 = values[0];
            return val;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    // i64 is Copy, so subscript should NOT produce .clone()
    assert!(
        !output.rust_code.contains("values[0].clone()")
            && !output.rust_code.contains("values.clone()"),
        "Copy type Vec<i64> subscript should NOT clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #2: Integer Literals Default to i64, Breaking Explicit Annotations
//
// Function arguments with explicit i32/f64 types should accept unadorned
// literals via type inference from the call context.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #2: i32 function params should accept integer literals"]
fn issue2_i32_function_params_accept_int_literals() {
    let source = r#"
        fn add(x: i32, y: i32) -> i32 {
            return x + y;
        }

        fn run() -> i32 {
            return add(1, 2);
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #2: f64 function params should accept float literals"]
fn issue2_f64_function_params_accept_float_literals() {
    let source = r#"
        fn scale(factor: f64, value: f64) -> f64 {
            return factor * value;
        }

        fn run() -> f64 {
            return scale(2.5, 10.0);
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #2: usize function params should accept integer literals"]
fn issue2_usize_params_accept_int_literals() {
    let source = r#"
        fn at_index(values: Vec<i64>, idx: usize) -> i64 {
            return values[idx];
        }

        fn run() -> i64 {
            const v: Vec<i64> = vec![10, 20, 30];
            return at_index(v, 1);
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #2: i32 let binding should infer from type annotation"]
fn issue2_i32_let_binding_infers_from_annotation() {
    let source = r#"
        fn run() -> i32 {
            const x: i32 = 42;
            return x;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #2: f64 enum variant payload should accept float literal"]
fn issue2_enum_variant_f64_accepts_float_literal() {
    let source = r#"
        enum Shape {
            Circle(f64),
            Square(f64),
        }

        fn run() -> f64 {
            const s: Shape = Shape::Circle(10.0);
            match s {
                Shape::Circle(r) => return r;
                Shape::Square(side) => return side;
            }
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #2: mixed numeric types in struct fields"]
fn issue2_struct_with_mixed_numeric_field_types() {
    let source = r#"
        pub struct Measurement {
            count: i32,
            weight: f64,
            label_idx: usize,
        }

        fn run() -> i32 {
            const m: Measurement = Measurement { count: 5, weight: 7.3, label_idx: 0 };
            return m.count;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #3: Several Vec Methods Not Resolved by Capability System
//
// Common Vec methods like pop(), clear(), remove(), insert(), sort(),
// reverse() should all resolve correctly.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #3: Vec.pop() should resolve"]
fn issue3_vec_pop_i64() {
    let source = r#"
        fn run() -> Option<i64> {
            let v: Vec<i64> = vec![1, 2, 3];
            return v.pop();
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.pop() must resolve");
    assert!(output.rust_code.contains(".pop()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.pop() on Vec<String> should resolve"]
fn issue3_vec_pop_string() {
    let source = r#"
        fn run() -> Option<String> {
            let v: Vec<String> = vec![String::from("hello")];
            return v.pop();
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec<String>.pop() must resolve");
    assert!(output.rust_code.contains(".pop()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.clear() should resolve"]
fn issue3_vec_clear() {
    let source = r#"
        fn run() {
            let v: Vec<i64> = vec![1, 2, 3];
            v.clear();
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.clear() must resolve");
    assert!(output.rust_code.contains(".clear()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.remove() should resolve"]
fn issue3_vec_remove() {
    let source = r#"
        fn run() -> String {
            let v: Vec<String> = vec![String::from("a"), String::from("b"), String::from("c")];
            return v.remove(1);
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.remove() must resolve");
    assert!(output.rust_code.contains(".remove("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.sort() should resolve"]
fn issue3_vec_sort() {
    let source = r#"
        fn run() -> Vec<i64> {
            let v: Vec<i64> = vec![3, 1, 2];
            v.sort();
            return v;
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.sort() must resolve");
    assert!(output.rust_code.contains(".sort()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.reverse() should resolve"]
fn issue3_vec_reverse() {
    let source = r#"
        fn run() -> Vec<i64> {
            let v: Vec<i64> = vec![1, 2, 3];
            v.reverse();
            return v;
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.reverse() must resolve");
    assert!(output.rust_code.contains(".reverse()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.extend() should resolve"]
fn issue3_vec_extend() {
    let source = r#"
        fn run() -> Vec<i64> {
            let v: Vec<i64> = vec![1, 2];
            v.extend(vec![3, 4]);
            return v;
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.extend() must resolve");
    assert!(output.rust_code.contains(".extend("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec.truncate() should resolve"]
fn issue3_vec_truncate() {
    let source = r#"
        fn run() -> Vec<i64> {
            let v: Vec<i64> = vec![1, 2, 3, 4, 5];
            v.truncate(3);
            return v;
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec.truncate() must resolve");
    assert!(output.rust_code.contains(".truncate("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #3: Vec of structs — pop should resolve"]
fn issue3_vec_of_structs_pop() {
    let source = r#"
        pub struct Task { name: String, priority: i64, }

        fn run() -> Option<Task> {
            let tasks: Vec<Task> = vec![
                Task { name: String::from("build"), priority: 1 },
            ];
            return tasks.pop();
        }
    "#;

    let output = compile_source(source).expect("should compile — Vec<Task>.pop() must resolve");
    assert!(output.rust_code.contains(".pop()"));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issues #4-5: Match Arms Generate Closures / Don't Satisfy Return Checker
//
// Match with block-bodied arms should NOT wrap in closure IIFEs.
// Exhaustive match arms with returns should satisfy the return checker.
// ═══════════════════════════════════════════════════════════════════════════

// ---- Phase 1 fix: block-bodied match arms as value-producing expressions ----

#[test]
fn issue4_match_return_i64() {
    // Match as expression with inline arms producing i64 values
    let source = r#"
        fn classify(n: i64) -> i64 {
            return match n {
                1 => 10;
                2 => 20;
                _ => 0;
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — match arms produce i64");
    assert!(!output.rust_code.contains("Closure"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
fn issue4_match_return_string() {
    // Match as expression with block arms returning String via tail expression
    let source = r#"
        fn describe(n: i64) -> String {
            return match n {
                1 => "one";
                _ => "other";
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — match arms return String");
    assert!(!output.rust_code.contains("Closure"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
fn issue4_match_block_arms_return_values() {
    // Match with block-bodied arms — the core fix in Phase 1
    let source = r#"
        fn compute(flag: bool) -> i64 {
            return match flag {
                true => {
                    const x: i64 = 42;
                    x
                }
                false => {
                    0
                }
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — block arms produce values");
    assert!(
        !output.rust_code.contains("Closure"),
        "should NOT use closure wrapping\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #4-5: enum match block arms — needs enum comma syntax support"]
fn issue4_match_enum_block_arms() {
    // Enum with block arms — also needs correct Elevate enum syntax
    let source = r#"
        enum Color { Red, Green, Blue }

        fn name(c: Color) -> String {
            return match c {
                Color::Red => {
                    "red"
                }
                Color::Green => {
                    "green"
                }
                Color::Blue => {
                    "blue"
                }
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — enum match block arms");
    assert!(!output.rust_code.contains("Closure"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
fn issue4_match_returning_struct() {
    // Match returning struct values from block arms
    let source = r#"
        pub struct Point { x: i64, y: i64, }

        fn origin_or_unit(flag: bool) -> Point {
            return match flag {
                true => {
                    Point { x: 0, y: 0 }
                }
                false => {
                    Point { x: 1, y: 1 }
                }
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — match returning structs");
    assert!(!output.rust_code.contains("Closure"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #4-5: match returning Vec from block arms — needs vec macro support"]
fn issue4_match_returning_vec() {
    // Match returning Vec — requires vec![] macro support
    let source = r#"
        fn make_list(kind: i64) -> Vec<i64> {
            return match kind {
                1 => {
                    [1, 2, 3]
                }
                2 => {
                    [10, 20]
                }
                _ => {
                    []
                }
            };
        }
    "#;

    let output = compile_source(source).expect("should compile — match returning Vec");
    assert!(!output.rust_code.contains("Closure"));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #7: Option.unwrap() Not Resolved
//
// Common Option methods beyond is_some/is_none should resolve.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #7: Option<i64>.unwrap() should resolve"]
fn issue7_option_unwrap_i64() {
    let source = r#"
        fn run() -> i64 {
            const x: Option<i64> = Some(42);
            return x.unwrap();
        }
    "#;

    let output = compile_source(source).expect("should compile — Option<i64>.unwrap()");
    assert!(output.rust_code.contains(".unwrap()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #7: Option<String>.unwrap() should resolve"]
fn issue7_option_unwrap_string() {
    let source = r#"
        fn run() -> String {
            const x: Option<String> = Some(String::from("hello"));
            return x.unwrap();
        }
    "#;

    let output = compile_source(source).expect("should compile — Option<String>.unwrap()");
    assert!(output.rust_code.contains(".unwrap()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #7: Option.map() should resolve"]
fn issue7_option_map() {
    let source = r#"
        fn run() -> Option<i64> {
            const x: Option<i64> = Some(10);
            return x.map(|v| -> i64 { return v * 2; });
        }
    "#;

    let output = compile_source(source).expect("should compile — Option.map()");
    assert!(output.rust_code.contains(".map("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #7: Option.and_then() should resolve"]
fn issue7_option_and_then() {
    let source = r#"
        fn safe_div(a: i64, b: i64) -> Option<i64> {
            if b == 0 {
                return None;
            }
            return Some(a / b);
        }

        fn run() -> Option<i64> {
            const x: Option<i64> = Some(10);
            return x.and_then(|v| -> Option<i64> { return safe_div(v, 2); });
        }
    "#;

    let output = compile_source(source).expect("should compile — Option.and_then()");
    assert!(output.rust_code.contains(".and_then("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #7: Option<Vec<i64>>.unwrap() should resolve"]
fn issue7_option_unwrap_vec() {
    let source = r#"
        fn run() -> Vec<i64> {
            const x: Option<Vec<i64>> = Some(vec![1, 2, 3]);
            return x.unwrap();
        }
    "#;

    let output = compile_source(source).expect("should compile — Option<Vec<i64>>.unwrap()");
    assert!(output.rust_code.contains(".unwrap()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #7: Option on struct should resolve unwrap"]
fn issue7_option_unwrap_struct() {
    let source = r#"
        pub struct Config { verbose: bool, }

        fn run() -> Config {
            const c: Option<Config> = Some(Config { verbose: true });
            return c.unwrap();
        }
    "#;

    let output = compile_source(source).expect("should compile — Option<Config>.unwrap()");
    assert!(output.rust_code.contains(".unwrap()"));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #8: HashMap Method Resolution Incomplete
//
// HashMap.insert(), .remove(), .entry() should resolve.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #8: HashMap.insert(String, i64) should resolve"]
fn issue8_hashmap_insert_string_i64() {
    let source = r#"
        use std::collections::HashMap;

        fn run() {
            let d: HashMap<String, i64> = HashMap::new();
            d.insert(String::from("key"), 42);
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — HashMap.insert()");
    assert!(output.rust_code.contains(".insert("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #8: HashMap.remove(String) should resolve"]
fn issue8_hashmap_remove() {
    let source = r#"
        use std::collections::HashMap;

        fn run() {
            let d: HashMap<String, i64> = HashMap::new();
            d.insert(String::from("key"), 42);
            d.remove(String::from("key"));
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — HashMap.remove()");
    assert!(output.rust_code.contains(".remove("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #8: HashMap<i64, String>.insert() should resolve"]
fn issue8_hashmap_insert_i64_string() {
    let source = r#"
        use std::collections::HashMap;

        fn run() {
            let d: HashMap<i64, String> = HashMap::new();
            d.insert(1, String::from("hello"));
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — HashMap<i64, String>.insert()");
    assert!(output.rust_code.contains(".insert("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #8: HashMap<String, Vec<i64>>.insert() should resolve"]
fn issue8_hashmap_insert_nested_value() {
    let source = r#"
        use std::collections::HashMap;

        fn run() {
            let d: HashMap<String, Vec<i64>> = HashMap::new();
            d.insert(String::from("nums"), vec![1, 2, 3]);
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — HashMap with nested value type");
    assert!(output.rust_code.contains(".insert("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #8: HashMap with struct values — insert should resolve"]
fn issue8_hashmap_insert_struct_value() {
    let source = r#"
        use std::collections::HashMap;

        pub struct Score { value: i64, label: String, }

        fn run() {
            let d: HashMap<String, Score> = HashMap::new();
            d.insert(String::from("test"), Score { value: 100, label: String::from("perfect") });
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile — HashMap<String, Score>.insert()");
    assert!(output.rust_code.contains(".insert("));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #9: String Method Resolution Limited
//
// Common String methods like to_uppercase(), to_lowercase(), trim(),
// replace(), split() should all resolve.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #9: String.to_uppercase() should resolve"]
fn issue9_string_to_uppercase() {
    let source = r#"
        fn run() -> String {
            const s: String = String::from("hello");
            return s.to_uppercase();
        }
    "#;

    let output = compile_source(source).expect("should compile — String.to_uppercase()");
    assert!(output.rust_code.contains(".to_uppercase()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #9: String.to_lowercase() should resolve"]
fn issue9_string_to_lowercase() {
    let source = r#"
        fn run() -> String {
            const s: String = String::from("HELLO");
            return s.to_lowercase();
        }
    "#;

    let output = compile_source(source).expect("should compile — String.to_lowercase()");
    assert!(output.rust_code.contains(".to_lowercase()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #9: String.trim() should resolve"]
fn issue9_string_trim() {
    let source = r#"
        fn run() -> String {
            const s: String = String::from("  hello  ");
            return s.trim().to_string();
        }
    "#;

    let output = compile_source(source).expect("should compile — String.trim()");
    assert!(output.rust_code.contains(".trim()"));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #9: String.replace() should resolve"]
fn issue9_string_replace() {
    let source = r#"
        fn run() -> String {
            const s: String = String::from("hello world");
            return s.replace(String::from("world"), String::from("rust"));
        }
    "#;

    let output = compile_source(source).expect("should compile — String.replace()");
    assert!(output.rust_code.contains(".replace("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #9: String.split() should resolve"]
fn issue9_string_split() {
    let source = r#"
        fn run() -> Vec<String> {
            const s: String = String::from("a,b,c");
            return s.split(String::from(",")).map(|x| -> String { return x.to_string(), }).collect();
        }
    "#;

    let output = compile_source(source).expect("should compile — String.split()");
    assert!(output.rust_code.contains(".split("));
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #9: struct field String method should resolve"]
fn issue9_struct_field_string_method() {
    let source = r#"
        pub struct User { name: String, }

        fn run(user: User) -> String {
            return user.name.to_uppercase();
        }
    "#;

    let output =
        compile_source(source).expect("should compile — struct field String.to_uppercase()");
    assert!(output.rust_code.contains(".to_uppercase()"));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #10: Mutable References Not Inserted for Function Arguments
//
// When a function mutates its argument (push, insert, field assign), the
// caller should pass &mut, not a clone.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #10: function that mutates Vec arg should get &mut"]
fn issue10_mutating_vec_arg_should_pass_mut_ref() {
    let source = r#"
        fn add_item(items: Vec<i64>) {
            items.push(42);
            return;
        }

        fn run() -> i64 {
            let numbers: Vec<i64> = vec![1, 2, 3];
            add_item(numbers);
            return numbers.len();
        }
    "#;

    let output = compile_source(source).expect("should compile");
    // The function should NOT clone — it should pass by mutable reference
    // or the mutation should be visible to the caller
    assert!(
        !output.rust_code.contains("numbers.clone()"),
        "mutating call should not silently clone — mutation would be lost\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #10: function that mutates Vec<String> arg"]
fn issue10_mutating_vec_string_arg() {
    let source = r#"
        fn append(items: Vec<String>) {
            items.push(String::from("added"));
            return;
        }

        fn run() -> i64 {
            let words: Vec<String> = vec![String::from("hello")];
            append(words);
            return words.len();
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        !output.rust_code.contains("words.clone()"),
        "mutating Vec<String> call should not clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #10: function mutates struct field arg"]
fn issue10_mutating_struct_field_arg() {
    let source = r#"
        pub struct Counter { value: i64, }

        fn increment(c: Counter) {
            c.value = c.value + 1;
            return;
        }

        fn run() -> i64 {
            let c: Counter = Counter { value: 0 };
            increment(c);
            return c.value;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        !output.rust_code.contains("c.clone()"),
        "mutating struct call should not clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #10: function mutates HashMap arg via insert"]
fn issue10_mutating_hashmap_arg() {
    let source = r#"
        use std::collections::HashMap;

        fn populate(map: HashMap<String, i64>) {
            map.insert(String::from("key"), 42);
            return;
        }

        fn run() -> i64 {
            let m: HashMap<String, i64> = HashMap::new();
            populate(m);
            return m.len();
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        !output.rust_code.contains("m.clone()"),
        "mutating HashMap call should not clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #10: nested struct mutation through function arg"]
fn issue10_nested_struct_mutation() {
    let source = r#"
        pub struct Inner { count: i64, }
        pub struct Outer { inner: Inner, }

        fn bump(o: Outer) {
            o.inner.count = o.inner.count + 1;
            return;
        }

        fn run() -> i64 {
            let x: Outer = Outer { inner: Inner { count: 0 } };
            bump(x);
            return x.inner.count;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        !output.rust_code.contains("x.clone()"),
        "nested struct mutation should not clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Issue #16: Generic Functions Missing Trait Bounds
//
// Generic functions using operators should infer the needed trait bounds.
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "feedback #16: generic == should infer PartialEq bound"]
fn issue16_generic_eq_infers_partial_eq() {
    let source = r#"
        fn check_eq<T>(a: T, b: T) -> bool {
            return a == b;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains("PartialEq"),
        "generic == should produce PartialEq bound\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #16: generic < should infer PartialOrd bound"]
fn issue16_generic_lt_infers_partial_ord() {
    let source = r#"
        fn is_less<T>(a: T, b: T) -> bool {
            return a < b;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains("PartialOrd"),
        "generic < should produce PartialOrd bound\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #16: generic + should infer Add bound"]
fn issue16_generic_add_infers_add() {
    let source = r#"
        fn sum<T>(a: T, b: T) -> T {
            return a + b;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains("Add"),
        "generic + should produce Add bound\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #16: generic function with multiple operators"]
fn issue16_generic_multiple_ops() {
    let source = r#"
        fn clamp<T>(value: T, min: T, max: T) -> T {
            if value < min {
                return min;
            }
            if value > max {
                return max;
            }
            return value;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains("PartialOrd"),
        "generic comparison should produce PartialOrd bound\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "feedback #16: generic Debug formatting should infer Debug bound"]
fn issue16_generic_debug_infers_debug() {
    let source = r#"
        fn show<T>(value: T) {
            println!("{:?}", value);
            return;
        }
    "#;

    let output = compile_source(source).expect("should compile");
    assert!(
        output.rust_code.contains("Debug") || output.rust_code.contains("fmt::Debug"),
        "generic println!(:?) should produce Debug bound\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Cross-cutting: Ownership + Capability combinations that stress multiple
// issues simultaneously
// ═══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "cross-cutting: match arm + Vec subscript clone + struct member"]
fn cross_match_arm_vec_subscript_struct() {
    let source = r#"
        pub struct Bag { words: Vec<String>, }

        fn run(bag: Bag, idx: i64) -> String {
            match idx {
                0 => {
                    return bag.words[0];
                }
                _ => {
                    return bag.words[1];
                }
            }
        }
    "#;

    let output = compile_source(source).expect("should compile — match + subscript + struct");
    assert!(
        output.rust_code.contains(".clone()"),
        "Vec<String> subscript in match arm should clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "cross-cutting: closure capture + Vec method + reuse"]
fn cross_closure_vec_pop_reuse() {
    let source = r#"
        fn run() -> i64 {
            let items: Vec<i64> = vec![1, 2, 3];
            const popper = || -> Option<i64> {
                return items.pop();
            };
            std::mem::drop(popper());
            return items.len();
        }
    "#;

    let output = compile_source(source).expect("should compile — closure + pop + reuse");
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "cross-cutting: HashMap insert + Vec subscript + struct field"]
fn cross_hashmap_insert_vec_subscript_struct() {
    let source = r#"
        use std::collections::HashMap;

        pub struct Row { cells: Vec<String>, }

        fn run(row: Row) {
            let index: HashMap<String, i64> = HashMap::new();
            const key: String = row.cells[0];
            index.insert(key, 42);
            return;
        }
    "#;

    let output = compile_source(source)
        .expect("should compile — HashMap.insert() + Vec<String> subscript + struct");
    assert!(output.rust_code.contains(".insert("));
    assert!(
        output.rust_code.contains(".clone()"),
        "Vec<String> subscript should clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "cross-cutting: nested Vec<Vec<String>> subscript + reuse"]
fn cross_nested_vec_subscript_reuse() {
    let source = r#"
        fn consume(rows: Vec<Vec<String>>) -> i64 {
            std::mem::drop(rows);
            return 1;
        }

        fn run(rows: Vec<Vec<String>>) -> i64 {
            const row: Vec<String> = rows[0];
            const cell: String = row[0];
            std::mem::drop(cell);
            return consume(rows);
        }
    "#;

    let output = compile_source(source)
        .expect("should compile — nested subscript + reuse should clone appropriately");
    assert!(
        output.rust_code.contains(".clone()"),
        "nested Vec subscript with reuse should clone\n{}",
        output.rust_code
    );
    assert_rust_code_compiles(&output.rust_code);
}

#[test]
#[ignore = "cross-cutting: tuple destructure + struct field + Vec method"]
fn cross_destructure_struct_vec_method() {
    let source = r#"
        pub struct Pair { left: Vec<i64>, right: Vec<i64>, }

        fn run(pair: Pair) -> i64 {
            let combined: Vec<i64> = pair.left;
            combined.extend(pair.right);
            return combined.len();
        }
    "#;

    let output = compile_source(source).expect("should compile — struct destructure + extend");
    assert!(output.rust_code.contains(".extend("));
    assert_rust_code_compiles(&output.rust_code);
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper
// ═══════════════════════════════════════════════════════════════════════════

fn assert_rust_code_compiles(code: &str) {
    let rustc_available = Command::new("rustc").arg("--version").output().is_ok();
    if !rustc_available {
        return;
    }

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time must be after epoch")
        .as_nanos();
    let base = env::temp_dir().join(format!("elevate-feedback-test-{nanos}"));
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
