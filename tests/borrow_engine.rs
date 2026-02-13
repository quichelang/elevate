//! Integration tests for `--exp-optimistic-move` and `--exp-polonius` flags.
//!
//! These verify that the `BorrowEngine` integration in the lowering pass
//! produces correct clone decisions under each engine.

use elevate::{CompileOptions, compile_source_with_options};

fn compile_with_optimistic_move(source: &str) -> elevate::CompilerOutput {
    let mut options = CompileOptions::default();
    options.experiments.optimistic_move = true;
    compile_source_with_options(source, &options).expect("optimistic-move compile should succeed")
}

fn compile_with_polonius(source: &str) -> elevate::CompilerOutput {
    let mut options = CompileOptions::default();
    options.experiments.polonius = true;
    compile_source_with_options(source, &options).expect("polonius compile should succeed")
}

fn compile_default(source: &str) -> elevate::CompilerOutput {
    let options = CompileOptions::default();
    compile_source_with_options(source, &options).expect("default compile should succeed")
}

#[allow(dead_code)]
fn compile_strict(source: &str) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = false;
    compile_source_with_options(source, &options)
}

fn compile_strict_with_optimistic_move(
    source: &str,
) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = false;
    options.experiments.optimistic_move = true;
    compile_source_with_options(source, &options)
}

fn compile_strict_with_polonius(
    source: &str,
) -> Result<elevate::CompilerOutput, elevate::CompileError> {
    let mut options = CompileOptions::default();
    options.experiments.type_system = false;
    options.experiments.polonius = true;
    compile_source_with_options(source, &options)
}

// ---------------------------------------------------------------------------
// BranchAwarePlan (--exp-optimistic-move)
// ---------------------------------------------------------------------------

#[test]
fn optimistic_move_avoids_clone_in_disjoint_branches() {
    // `data` is used in two disjoint if/else branches — the optimistic engine
    // should recognise they can't co-execute and skip the clone.
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>, flag: bool) -> usize {
            if flag {
                return consume(data);
            } else {
                return consume(data);
            }
        }
    "#;
    let output = compile_with_optimistic_move(source);
    // With optimistic analysis, no clone should be needed because both uses
    // are in mutually exclusive branches.
    assert!(
        !output.rust_code.contains(".clone()"),
        "optimistic-move should avoid cloning data in disjoint branches.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn optimistic_move_still_clones_sequential_uses() {
    // `data` used twice sequentially — even optimistic analysis must clone.
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>) -> usize {
            std::mem::drop(consume(data));
            return consume(data);
        }
    "#;
    let output = compile_with_optimistic_move(source);
    assert!(
        output.rust_code.contains(".clone()"),
        "optimistic-move should still clone sequential reuse.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn optimistic_move_notes_flag_active() {
    let source = r#"
        fn id(v: i64) -> i64 { v }
    "#;
    let output = compile_with_optimistic_move(source);
    assert!(
        output
            .ownership_notes
            .iter()
            .any(|n| n.contains("exp_optimistic_move")),
        "expected ownership note for exp_optimistic_move flag"
    );
}

// ---------------------------------------------------------------------------
// PoloniusEngine (--exp-polonius) — conservative stub
// ---------------------------------------------------------------------------

#[test]
fn polonius_produces_same_output_as_default() {
    // The Polonius stub is conservative (flat counting), so output should be
    // identical to default compilation.
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>) -> usize {
            std::mem::drop(consume(data));
            return consume(data);
        }
    "#;
    let default_output = compile_default(source);
    let polonius_output = compile_with_polonius(source);

    // The actual Rust code (minus ownership notes) should be equivalent
    let default_code = strip_ownership_comments(&default_output.rust_code);
    let polonius_code = strip_ownership_comments(&polonius_output.rust_code);
    assert_eq!(
        default_code, polonius_code,
        "polonius stub should produce identical code to default"
    );
}

#[test]
fn polonius_still_clones_sequential_uses() {
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>) -> usize {
            std::mem::drop(consume(data));
            return consume(data);
        }
    "#;
    let output = compile_with_polonius(source);
    assert!(
        output.rust_code.contains(".clone()"),
        "polonius stub should clone sequential reuse.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn polonius_notes_flag_active() {
    let source = r#"
        fn id(v: i64) -> i64 { v }
    "#;
    let output = compile_with_polonius(source);
    assert!(
        output
            .ownership_notes
            .iter()
            .any(|n| n.contains("exp_polonius")),
        "expected ownership note for exp_polonius flag"
    );
}

// ---------------------------------------------------------------------------
// Both flags enabled: polonius takes precedence
// ---------------------------------------------------------------------------

#[test]
fn polonius_takes_precedence_when_both_flags_set() {
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>, flag: bool) -> usize {
            if flag {
                return consume(data);
            } else {
                return consume(data);
            }
        }
    "#;
    let mut options = CompileOptions::default();
    options.experiments.optimistic_move = true;
    options.experiments.polonius = true;
    let output = compile_source_with_options(source, &options).expect("compile should succeed");
    // Polonius is conservative — it should clone even in disjoint branches,
    // proving it took precedence over the optimistic engine.
    assert!(
        output.rust_code.contains(".clone()"),
        "polonius should take precedence and clone in disjoint branches.\nGenerated:\n{}",
        output.rust_code,
    );
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

fn strip_ownership_comments(code: &str) -> String {
    code.lines()
        .filter(|line| !line.trim_start().starts_with("// ownership-note:"))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// Strict mode + BorrowEngine
// ---------------------------------------------------------------------------

#[test]
fn strict_optimistic_move_clones_sequential_uses() {
    // Fully annotated source works in strict mode with optimistic-move.
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>) -> usize {
            std::mem::drop(consume(data));
            return consume(data);
        }
    "#;
    let output = compile_strict_with_optimistic_move(source)
        .expect("strict + optimistic-move should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "strict + optimistic-move should still clone sequential reuse.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn strict_optimistic_move_avoids_clone_in_disjoint_branches() {
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>, flag: bool) -> usize {
            if flag {
                return consume(data);
            } else {
                return consume(data);
            }
        }
    "#;
    let output = compile_strict_with_optimistic_move(source)
        .expect("strict + optimistic-move should compile");
    assert!(
        !output.rust_code.contains(".clone()"),
        "strict + optimistic-move should avoid clone in disjoint branches.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn strict_polonius_clones_sequential_uses() {
    let source = r#"
        rust { pub fn consume(v: Vec<i64>) -> usize { v.len() } }
        fn demo(data: Vec<i64>) -> usize {
            std::mem::drop(consume(data));
            return consume(data);
        }
    "#;
    let output = compile_strict_with_polonius(source).expect("strict + polonius should compile");
    assert!(
        output.rust_code.contains(".clone()"),
        "strict + polonius should clone sequential reuse.\nGenerated:\n{}",
        output.rust_code,
    );
}

#[test]
fn strict_mode_rejection_not_affected_by_optimistic_move() {
    // Missing type annotation — strict mode should still reject, even with
    // optimistic-move enabled.
    let source = r#"
        fn add(a, b) {
            return a + b;
        }
    "#;
    let error = compile_strict_with_optimistic_move(source)
        .expect_err("strict mode should reject unresolved types");
    assert!(
        error.to_string().contains("strict mode"),
        "error should mention strict mode:\n{}",
        error,
    );
}

#[test]
fn strict_mode_rejection_not_affected_by_polonius() {
    let source = r#"
        fn add(a, b) {
            return a + b;
        }
    "#;
    let error = compile_strict_with_polonius(source)
        .expect_err("strict mode should reject unresolved types");
    assert!(
        error.to_string().contains("strict mode"),
        "error should mention strict mode:\n{}",
        error,
    );
}
