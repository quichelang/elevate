use std::env;
use std::io::ErrorKind;
use std::path::Path;
use std::path::PathBuf;
use std::process::{self, Command};

fn main() {
    let source_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let cli_args = env::args().skip(1).collect::<Vec<_>>();
    let invocation = parse_invocation(&source_root, &cli_args);
    let release = running_release_profile();

    run_checked(
        build_command(&source_root, release, &invocation.build_args),
        "elevate build",
    )
        .unwrap_or_else(|error| exit_with_error(error));
    if invocation.build_only {
        return;
    }

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
    if !invocation.run_args.is_empty() {
        run.arg("--");
        run.args(&invocation.run_args);
    }

    run_checked(run, "cargo run (generated crate)")
        .unwrap_or_else(|error| exit_with_error(error));
}

fn build_command(source_root: &Path, release: bool, build_args: &[String]) -> Command {
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
        fallback.args(build_args);
        return fallback;
    }
    let mut build = Command::new("elevate");
    build.arg("build").arg(source_root);
    if release {
        build.arg("--release");
    }
    build.args(build_args);
    build
}

#[derive(Debug, Default)]
struct Invocation {
    build_only: bool,
    build_args: Vec<String>,
    run_args: Vec<String>,
}

fn parse_invocation(source_root: &Path, args: &[String]) -> Invocation {
    if args
        .first()
        .is_some_and(|arg| arg == "build" || arg == "run")
    {
        let build_only = args.first().is_some_and(|arg| arg == "build");
        let mut build_args = Vec::new();
        let mut run_args = Vec::new();
        let mut parsing_run_args = false;
        let source_root_text = source_root.to_string_lossy();
        let mut index = 1usize;
        while index < args.len() {
            let arg = &args[index];
            if arg == "--" {
                parsing_run_args = true;
                index += 1;
                continue;
            }
            if parsing_run_args {
                run_args.push(arg.clone());
                index += 1;
                continue;
            }
            if arg == "." || arg == source_root_text.as_ref() {
                index += 1;
                continue;
            }
            if !arg.starts_with('-') {
                index += 1;
                continue;
            }
            build_args.push(arg.clone());
            if build_flag_takes_value(arg)
                && let Some(value) = args.get(index + 1)
                && !value.starts_with('-')
            {
                build_args.push(value.clone());
                index += 1;
            }
            index += 1;
        }
        return Invocation {
            build_only,
            build_args,
            run_args,
        };
    }

    Invocation {
        build_only: false,
        build_args: Vec::new(),
        run_args: args.to_vec(),
    }
}

fn build_flag_takes_value(flag: &str) -> bool {
    matches!(
        flag,
        "--allow-hot-clone-place" | "--force-clone-place" | "--source-name"
    )
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
