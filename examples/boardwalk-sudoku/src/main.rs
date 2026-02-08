use std::env;
use std::io::ErrorKind;
use std::path::Path;
use std::path::PathBuf;
use std::process::{self, Command};

fn main() {
    let source_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let run_args = env::args().skip(1).collect::<Vec<_>>();
    let release = running_release_profile();

    run_checked(build_command(&source_root, release), "elevate build")
        .unwrap_or_else(|error| exit_with_error(error));

    let mut run = Command::new("cargo");
    run.arg("run")
        .arg("--manifest-path")
        .arg(
            source_root
                .join("target")
                .join("elevate-gen")
                .join("Cargo.toml"),
        )
        .arg("--target-dir")
        .arg(source_root.join("target"));
    if release {
        run.arg("--release");
    }
    if !run_args.is_empty() {
        run.arg("--");
        run.args(&run_args);
    }

    run_checked(run, "cargo run (generated crate)")
        .unwrap_or_else(|error| exit_with_error(error));
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

fn running_release_profile() -> bool {
    let Ok(path) = env::current_exe() else {
        return false;
    };
    path.components().any(|component| component.as_os_str() == "release")
}

fn exit_with_error(message: String) -> ! {
    eprintln!("{message}");
    process::exit(1);
}
