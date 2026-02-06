use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildSummary {
    pub source_root: PathBuf,
    pub generated_root: PathBuf,
    pub transpiled_files: usize,
    pub copied_files: usize,
}

pub fn build_ers_crate(crate_root: &Path, release: bool) -> Result<BuildSummary, String> {
    let summary = transpile_ers_crate(crate_root)?;
    run_generated_cargo_build(&summary.source_root, &summary.generated_root, release)?;
    Ok(summary)
}

pub fn transpile_ers_crate(crate_root: &Path) -> Result<BuildSummary, String> {
    let source_root = canonicalize(crate_root)?;
    let source_manifest = source_root.join("Cargo.toml");
    let source_src = source_root.join("src");

    if !source_manifest.is_file() {
        return Err(format!(
            "expected crate manifest at {}",
            source_manifest.display()
        ));
    }
    if !source_src.is_dir() {
        return Err(format!(
            "expected source directory at {}",
            source_src.display()
        ));
    }

    let generated_root = source_root.join("target").join("elevate-gen");
    if generated_root.exists() {
        fs::remove_dir_all(&generated_root).map_err(|error| {
            format!(
                "failed to clear generated output {}: {error}",
                generated_root.display()
            )
        })?;
    }
    fs::create_dir_all(generated_root.join("src")).map_err(|error| {
        format!(
            "failed to create generated source root {}: {error}",
            generated_root.join("src").display()
        )
    })?;

    fs::copy(&source_manifest, generated_root.join("Cargo.toml")).map_err(|error| {
        format!(
            "failed to copy Cargo.toml to generated crate {}: {error}",
            generated_root.join("Cargo.toml").display()
        )
    })?;

    let mut summary = BuildSummary {
        source_root,
        generated_root: generated_root.clone(),
        transpiled_files: 0,
        copied_files: 0,
    };

    process_src_dir(
        &source_src,
        &source_src,
        &generated_root.join("src"),
        &mut summary,
    )?;
    Ok(summary)
}

fn run_generated_cargo_build(
    source_root: &Path,
    generated_root: &Path,
    release: bool,
) -> Result<(), String> {
    let manifest = generated_root.join("Cargo.toml");
    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--manifest-path")
        .arg(&manifest)
        .arg("--target-dir")
        .arg(source_root.join("target"));
    if release {
        cmd.arg("--release");
    }

    let status = cmd.status().map_err(|error| {
        format!(
            "failed to run cargo build for generated crate {}: {error}",
            manifest.display()
        )
    })?;
    if !status.success() {
        return Err(format!(
            "cargo build failed for generated crate {}",
            manifest.display()
        ));
    }
    Ok(())
}

fn process_src_dir(
    root_src: &Path,
    current: &Path,
    generated_src: &Path,
    summary: &mut BuildSummary,
) -> Result<(), String> {
    let entries = fs::read_dir(current)
        .map_err(|error| format!("failed to read directory {}: {error}", current.display()))?;

    for entry in entries {
        let entry = entry.map_err(|error| {
            format!(
                "failed to read directory entry in {}: {error}",
                current.display()
            )
        })?;
        let path = entry.path();
        let rel = path
            .strip_prefix(root_src)
            .map_err(|error| format!("failed to compute relative source path: {error}"))?;
        let target = generated_src.join(rel);

        if path.is_dir() {
            fs::create_dir_all(&target).map_err(|error| {
                format!(
                    "failed to create generated directory {}: {error}",
                    target.display()
                )
            })?;
            process_src_dir(root_src, &path, generated_src, summary)?;
            continue;
        }

        if path.extension() == Some(OsStr::new("ers")) {
            let source = fs::read_to_string(&path)
                .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
            let output = crate::compile_source(&source)
                .map_err(|error| format!("failed to compile {}: {error}", path.display()))?;
            let mut out_path = target.clone();
            out_path.set_extension("rs");
            if out_path.exists() {
                return Err(format!(
                    "output path collision while generating Rust: {}",
                    out_path.display()
                ));
            }
            fs::write(&out_path, output.rust_code.as_bytes()).map_err(|error| {
                format!(
                    "failed to write generated file {}: {error}",
                    out_path.display()
                )
            })?;
            summary.transpiled_files += 1;
            continue;
        }

        if target.exists() {
            return Err(format!(
                "output path collision while copying file: {}",
                target.display()
            ));
        }
        fs::copy(&path, &target).map_err(|error| {
            format!(
                "failed to copy source file {} to {}: {error}",
                path.display(),
                target.display()
            )
        })?;
        summary.copied_files += 1;
    }

    Ok(())
}

fn canonicalize(path: &Path) -> Result<PathBuf, String> {
    fs::canonicalize(path).map_err(|error| format!("failed to access {}: {error}", path.display()))
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::transpile_ers_crate;

    #[test]
    fn transpile_multi_file_crate_preserves_layout() {
        let root = create_temp_dir("elevate-crate-layout");
        fs::create_dir_all(root.join("src/net")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(root.join("src/lib.ers"), "const VALUE: i64 = 1;")
            .expect("write lib.ers should succeed");
        fs::write(
            root.join("src/net/http.ers"),
            "fn status() -> i64 { return 200; }",
        )
        .expect("write nested ers should succeed");
        fs::write(root.join("src/notes.txt"), "preserve me")
            .expect("write non-ers file should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        assert_eq!(summary.transpiled_files, 2);
        assert_eq!(summary.copied_files, 1);

        let generated = summary.generated_root.join("src");
        assert!(generated.join("lib.rs").exists());
        assert!(generated.join("net/http.rs").exists());
        assert!(generated.join("notes.txt").exists());
    }

    fn create_temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be valid")
            .as_nanos();
        let path = env::temp_dir().join(format!("{prefix}-{nanos}"));
        fs::create_dir_all(&path).expect("temp dir create should succeed");
        path
    }
}
