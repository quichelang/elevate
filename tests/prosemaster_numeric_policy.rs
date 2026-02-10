use elevate::{CompileOptions, crate_builder};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static TEST_TEMP_SEQ: AtomicU64 = AtomicU64::new(0);

#[test]
fn prosemaster_transpile_uses_typed_results_and_u64_counters() {
    let root = copy_example_to_temp("prosemaster");
    let mut options = CompileOptions::default();
    options.experiments.infer_local_bidi = true;

    let summary = crate_builder::transpile_ers_crate_with_options(&root, &options)
        .expect("prosemaster should transpile with bidi inference enabled");

    let runtime_rs = fs::read_to_string(summary.generated_root.join("src/runtime.rs"))
        .expect("generated runtime.rs should exist");

    assert!(runtime_rs.contains("pub enum PlaceFromRackResult"));
    assert!(runtime_rs.contains("pub enum SubmitTurnResult"));
    assert!(runtime_rs.contains("bag_count: u64"));
    assert!(runtime_rs.contains("score_total: u64"));
    assert!(runtime_rs.contains("last_turn_score: u64"));
    assert!(runtime_rs.contains("turn_number: u64"));
    assert!(runtime_rs.contains("pub fn submit_turn"));
    assert!(runtime_rs.contains("SubmitTurnResult"));
    assert!(runtime_rs.contains("pub fn place_from_rack"));
    assert!(runtime_rs.contains("PlaceFromRackResult"));
    assert!(runtime_rs.contains("host::runtime_start_mode(mode)"));
}

#[test]
fn prosemaster_numeric_policy_separates_wrapping_rng_from_saturating_counters() {
    let root = copy_example_to_temp("prosemaster");
    let mut options = CompileOptions::default();
    options.experiments.infer_local_bidi = true;

    let summary = crate_builder::transpile_ers_crate_with_options(&root, &options)
        .expect("prosemaster should transpile with bidi inference enabled");

    let numeric_rs = fs::read_to_string(summary.generated_root.join("src/numeric.rs"))
        .expect("generated numeric.rs should exist");

    assert!(numeric_rs.contains("pub fn prng_step(seed: u64) -> u64"));
    assert!(numeric_rs.contains("u64::wrapping_mul(seed, 1103515245)"));
    assert!(numeric_rs.contains("u64::wrapping_add(u64::wrapping_mul(seed, 1103515245), 12345)"));
    assert!(numeric_rs.contains("pub fn score_add(value: u64, delta: u64) -> u64"));
    assert!(numeric_rs.contains("u64::saturating_add(value, delta)"));
    assert!(numeric_rs.contains("pub fn count_dec(value: u64) -> u64"));
    assert!(numeric_rs.contains("u64::saturating_sub(value, 1)"));
}

fn copy_example_to_temp(example_name: &str) -> PathBuf {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source = workspace.join("examples").join(example_name);
    let target = temp_dir(&format!("elevate-{example_name}-numeric-policy"));
    copy_dir_filtered(&source, &target);
    fs::canonicalize(&target).unwrap_or(target)
}

fn copy_dir_filtered(source: &Path, target: &Path) {
    fs::create_dir_all(target).expect("target dir should be created");
    let entries = fs::read_dir(source).expect("source dir should be readable");
    for entry in entries {
        let entry = entry.expect("directory entry should be readable");
        let path = entry.path();
        let name = entry.file_name();
        if name == "target" || name == ".git" {
            continue;
        }
        let next = target.join(name);
        if path.is_dir() {
            copy_dir_filtered(&path, &next);
        } else {
            fs::copy(&path, &next).unwrap_or_else(|error| {
                panic!(
                    "failed to copy {} to {}: {error}",
                    path.display(),
                    next.display()
                )
            });
        }
    }
}

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time should be valid")
        .as_nanos();
    let seq = TEST_TEMP_SEQ.fetch_add(1, Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!("{prefix}-{nanos}-{seq}"));
    fs::create_dir_all(&path).expect("temp dir should be created");
    path
}
