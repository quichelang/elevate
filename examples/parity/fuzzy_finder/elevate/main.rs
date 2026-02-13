use std::env;
use std::io::ErrorKind;
use std::path::Path;
use std::path::PathBuf;
use std::process::{self, Command};

fn main() {
    let source_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let release = false;

    run_checked(build_command(&source_root, release), "elevate build")
        .unwrap_or_else(|error| exit_with_error(error));
    eprintln!("Build succeeded.");
}

fn build_command(source_root: &Path, release: bool) -> Command {
    if let Some(manifest) = detect_local_elevate_manifest(source_root) {
        let mut fallback = Command::new("cargo");
        fallback
            .arg("run")
            .arg("--manifest-path")
            .arg(manifest)
            .arg("--")
            .arg("build")
            .arg(source_root);
        if release {
            fallback.arg("--release");
        }
        return fallback;
    }
    let mut build = Command::new("elevate");
    build.arg("build").arg(source_root);
    if release {
        build.arg("--release");
    }
    build
}

fn run_checked(mut cmd: Command, label: &str) -> Result<(), String> {
    let status = cmd.status().map_err(|error| {
        if error.kind() == ErrorKind::NotFound {
            return format!(
                "failed to run {label}: command not found (install `elevate` or run inside the Elevate repository)"
            );
        }
        format!("failed to run {label}: {error}")
    })?;
    if status.success() {
        return Ok(());
    }
    let code = status.code().unwrap_or(1);
    Err(format!("{label} failed with exit code {code}"))
}

fn detect_local_elevate_manifest(source_root: &Path) -> Option<PathBuf> {
    let mut dir = source_root.to_path_buf();
    loop {
        let manifest = dir.join("Cargo.toml");
        if manifest.is_file() {
            let content = std::fs::read_to_string(&manifest).ok()?;
            if content.contains("name = \"elevate\"") {
                return Some(manifest);
            }
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

fn exit_with_error(message: String) -> ! {
    eprintln!("{message}");
    process::exit(1);
}
