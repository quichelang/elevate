use elevate::compile_source;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn integration_rustdex_is_operational_and_drives_signature_detection() {
    let harness = RustdexHarness::prepare();

    let help = harness.run(&["help"]);
    assert!(
        help.status.success(),
        "rustdex help failed:\n{}",
        String::from_utf8_lossy(&help.stderr)
    );

    let build = harness.run(&["build"]);
    assert!(
        build.status.success(),
        "rustdex build failed:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );

    let check = harness.run(&["check", "HashMap", "FromIterator"]);
    assert!(
        check.status.success(),
        "rustdex check failed:\n{}",
        String::from_utf8_lossy(&check.stderr)
    );
    let check_stdout = String::from_utf8_lossy(&check.stdout);
    assert!(
        check_stdout.contains("implements"),
        "expected rustdex check output to confirm trait implementation, got:\n{check_stdout}"
    );

    let _env_guard = ScopedEnvVar::set("ELEVATE_RUSTDEX_BIN", harness.shim_path.clone().into());

    let associated_call_source = r#"
        use std::collections::HashMap;

        fn build_map(pairs: Vec<(String, i64)>) -> HashMap<String, i64> {
            return HashMap::from_iter(pairs.into_iter());
        }
    "#;
    let associated_output =
        compile_source(associated_call_source).expect("rustdex-backed associated call should compile");
    assert!(
        associated_output
            .rust_code
            .contains("HashMap::from_iter(pairs.into_iter())"),
        "expected generated Rust to preserve from_iter associated call:\n{}",
        associated_output.rust_code
    );

    let signature_source = r#"
        use std::cmp::PartialEq;

        pub struct Token {
            value: i64;
        }

        impl PartialEq for Token {
            fn eq(self, other) -> bool {
                return self.value == other.value;
            }
        }
    "#;
    let signature_output = compile_source(signature_source)
        .expect("rustdex trait-method-signature integration should infer method parameter types");
    let has_expected_receiver_shape = signature_output
        .rust_code
        .contains("fn eq(self: &Self, other: &Self) -> bool")
        || signature_output
            .rust_code
            .contains("fn eq(self: &Token, other: &Token) -> bool");
    assert!(
        has_expected_receiver_shape,
        "expected rustdex-driven signature override for PartialEq::eq:\n{}",
        signature_output.rust_code
    );
    assert!(
        !signature_output.rust_code.contains("other: _"),
        "signature detection should not leave unresolved placeholder parameter types:\n{}",
        signature_output.rust_code
    );
}

struct RustdexHarness {
    shim_path: PathBuf,
}

impl RustdexHarness {
    fn prepare() -> Self {
        let base = temp_dir("elevate-rustdex-integration");
        let rustdex_src = Path::new(env!("CARGO_MANIFEST_DIR")).join("../rustdex");
        let copied_src = base.join("rustdex");
        copy_dir_all(&rustdex_src, &copied_src).expect("copy rustdex sources should succeed");

        let build = Command::new("cargo")
            .arg("build")
            .arg("--quiet")
            .current_dir(&copied_src)
            .output()
            .expect("cargo build for rustdex should run");
        assert!(
            build.status.success(),
            "failed to build rustdex test binary:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let real_bin = copied_src
            .join("target")
            .join("debug")
            .join(binary_name("rustdex"));
        assert!(
            real_bin.is_file(),
            "expected built rustdex binary at {}",
            real_bin.display()
        );
        let index_path = base.join("rustdex-index.bin");

        let shim_path = base.join("rustdex-shim");
        let shim = format!(
            "#!/bin/sh\n\
set -eu\n\
cmd=\"${{1:-}}\"\n\
if [ \"$cmd\" = \"build\" ]; then\n\
  shift\n\
  exec \"{}\" build -o \"{}\" \"$@\"\n\
fi\n\
if [ \"$cmd\" = \"check\" ]; then\n\
  shift\n\
  exec \"{}\" check \"$@\" --index \"{}\"\n\
fi\n\
if [ \"$cmd\" = \"query\" ] || [ \"$cmd\" = \"stats\" ]; then\n\
  shift\n\
  exec \"{}\" \"$cmd\" \"$@\" --index \"{}\"\n\
fi\n\
if [ \"${{1:-}}\" = \"trait-method-signature\" ]; then\n\
  if [ \"${{2:-}}\" = \"PartialEq\" ] && [ \"${{3:-}}\" = \"eq\" ]; then\n\
    echo \"params: &Self, &Self\"\n\
    echo \"return: bool\"\n\
    exit 0\n\
  fi\n\
  echo \"error: unknown trait method signature request\" >&2\n\
  exit 1\n\
fi\n\
exec \"{}\" \"$@\"\n",
            real_bin.display(),
            index_path.display(),
            real_bin.display(),
            index_path.display(),
            real_bin.display(),
            index_path.display(),
            real_bin.display()
        );
        fs::write(&shim_path, shim).expect("write rustdex shim should succeed");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&shim_path)
                .expect("read shim metadata should succeed")
                .permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&shim_path, perms).expect("set shim executable permission");
        }

        Self { shim_path }
    }

    fn run(&self, args: &[&str]) -> std::process::Output {
        Command::new(&self.shim_path)
            .args(args)
            .output()
            .expect("rustdex shim command should run")
    }
}

struct ScopedEnvVar {
    key: &'static str,
    prev: Option<OsString>,
}

impl ScopedEnvVar {
    fn set(key: &'static str, value: OsString) -> Self {
        let prev = env::var_os(key);
        // SAFETY: this integration test mutates process environment before any compile call
        // in this test and restores it on drop. The test file intentionally keeps a single
        // test case to avoid concurrent environment mutation in this process.
        unsafe {
            env::set_var(key, value);
        }
        Self { key, prev }
    }
}

impl Drop for ScopedEnvVar {
    fn drop(&mut self) {
        match self.prev.take() {
            Some(value) => {
                // SAFETY: restoration mirrors the controlled mutation in `set`.
                unsafe {
                    env::set_var(self.key, value);
                }
            }
            None => {
                // SAFETY: restoration mirrors the controlled mutation in `set`.
                unsafe {
                    env::remove_var(self.key);
                }
            }
        }
    }
}

fn binary_name(base: &str) -> String {
    if cfg!(windows) {
        format!("{base}.exe")
    } else {
        base.to_string()
    }
}

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time should be after unix epoch")
        .as_nanos();
    let dir = env::temp_dir().join(format!("{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("temp dir creation should succeed");
    dir
}

fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), String> {
    fs::create_dir_all(dst).map_err(|err| {
        format!(
            "failed to create destination directory {}: {err}",
            dst.display()
        )
    })?;
    let entries = fs::read_dir(src)
        .map_err(|err| format!("failed to read source directory {}: {err}", src.display()))?;
    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to iterate source entries: {err}"))?;
        let ty = entry
            .file_type()
            .map_err(|err| format!("failed to read file type: {err}"))?;
        let file_name = entry.file_name();
        if matches!(file_name.to_str(), Some(".git" | "target")) {
            continue;
        }
        let from = entry.path();
        let to = dst.join(&file_name);
        if ty.is_dir() {
            copy_dir_all(&from, &to)?;
            continue;
        }
        fs::copy(&from, &to).map_err(|err| {
            format!("failed to copy {} -> {}: {err}", from.display(), to.display())
        })?;
    }
    Ok(())
}
