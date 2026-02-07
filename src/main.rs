use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process;

use elevate::{CompileOptions, ExperimentFlags};

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        usage();
        process::exit(2);
    }

    match args[0].as_str() {
        "build" => run_build(&args[1..]),
        "test" => run_test(&args[1..]),
        "init" => run_init(&args[1..]),
        _ => run_compile(&args),
    }
}

fn run_build(args: &[String]) {
    if args.is_empty() {
        eprintln!("usage: elevate build <crate-root> [--release] [experiment flags]");
        process::exit(2);
    }
    let crate_root = PathBuf::from(&args[0]);
    let mut release = false;
    let mut non_flag_args = Vec::new();
    for arg in &args[1..] {
        if arg == "--release" {
            release = true;
            continue;
        }
        non_flag_args.push(arg.clone());
    }
    let options = parse_compile_options(&non_flag_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    }
    );

    match elevate::crate_builder::build_ers_crate_with_options(&crate_root, release, &options) {
        Ok(summary) => {
            println!(
                "built generated crate at {} (transpiled {}, copied {}), artifacts in {}",
                summary.generated_root.display(),
                summary.transpiled_files,
                summary.copied_files,
                summary.source_root.join("target").display()
            );
        }
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    }
}

fn run_test(args: &[String]) {
    if args.is_empty() {
        eprintln!("usage: elevate test <crate-root> [experiment flags]");
        process::exit(2);
    }
    let crate_root = PathBuf::from(&args[0]);
    let options = parse_compile_options(&args[1..]).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });

    match elevate::test_runner::test_ers_crate(&crate_root, &options) {
        Ok(summary) => {
            println!(
                "tests passed for {} (discovered {} test(s)); generated crate at {}",
                summary.source_root.display(),
                summary.discovered_tests,
                summary.generated_root.display()
            );
        }
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    }
}

fn run_init(args: &[String]) {
    if args.is_empty() {
        eprintln!("usage: elevate init <crate-root> [cargo init flags]");
        process::exit(2);
    }
    let crate_root = parse_init_root(args).unwrap_or_else(|| {
        eprintln!("usage: elevate init <crate-root> [cargo init flags]");
        process::exit(2);
    });
    let mut init = Command::new("cargo");
    init.arg("init");
    init.args(args);
    let status = init.status().unwrap_or_else(|error| {
        eprintln!("failed to run cargo init: {error}");
        process::exit(1);
    });
    if !status.success() {
        process::exit(status.code().unwrap_or(1));
    }

    apply_elevate_templates(&crate_root).unwrap_or_else(|error| {
        eprintln!("{error}");
        process::exit(1);
    });
    println!(
        "initialized elevate crate at {} with transparent bootstrap runner",
        crate_root.display()
    );
}

fn run_compile(args: &[String]) {
    let input_path = PathBuf::from(&args[0]);
    let mut emit_target = EmitTarget::Stdout;
    let mut options_args = Vec::new();
    let mut idx = 1usize;
    while idx < args.len() {
        if args[idx] == "--emit-rust" {
            if idx + 1 < args.len() && !args[idx + 1].starts_with("--") {
                emit_target = EmitTarget::File(PathBuf::from(&args[idx + 1]));
                idx += 2;
            } else {
                emit_target = EmitTarget::Stdout;
                idx += 1;
            }
            continue;
        }
        options_args.push(args[idx].clone());
        idx += 1;
    }
    let options = parse_compile_options(&options_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });

    let source = match elevate::source::load_file(&input_path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    };
    let output = match elevate::compile_source_with_options(&source, &options) {
        Ok(output) => output,
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    };

    match emit_target {
        EmitTarget::Stdout => println!("{}", output.rust_code),
        EmitTarget::File(path) => {
            if let Err(error) = std::fs::write(&path, output.rust_code.as_bytes()) {
                eprintln!("failed to write {}: {error}", path.display());
                process::exit(1);
            }
        }
    }
}

fn usage() {
    eprintln!("usage:");
    eprintln!("  elevate <input-file.ers> [--emit-rust [output-file]] [experiment flags]");
    eprintln!("  elevate build <crate-root> [--release] [experiment flags]");
    eprintln!("  elevate test <crate-root> [experiment flags]");
    eprintln!("  elevate init <crate-root> [cargo init flags]");
    eprintln!("experiment flags:");
    eprintln!("  --exp-move-mut-args");
    eprintln!("  --exp-infer-local-bidi");
    eprintln!("  --exp-effect-rows-internal");
    eprintln!("  --exp-infer-principal-fallback");
    eprintln!("  --fail-on-hot-clone");
    eprintln!("  --allow-hot-clone-place <place>");
    eprintln!("  --force-clone-place <place>");
}

fn parse_init_root(args: &[String]) -> Option<PathBuf> {
    let mut index = 0usize;
    while index < args.len() {
        let arg = &args[index];
        if arg.starts_with('-') {
            if matches!(arg.as_str(), "--name" | "--vcs") {
                index += 1;
            }
            index += 1;
            continue;
        }
        return Some(PathBuf::from(arg));
    }
    None
}

fn apply_elevate_templates(crate_root: &Path) -> Result<(), String> {
    let manifest_path = crate_root.join("Cargo.toml");
    let src_dir = crate_root.join("src");
    if !manifest_path.is_file() {
        return Err(format!(
            "failed to initialize templates: missing {}",
            manifest_path.display()
        ));
    }
    fs::create_dir_all(&src_dir).map_err(|error| {
        format!(
            "failed to create source directory {}: {error}",
            src_dir.display()
        )
    })?;

    let mut manifest = fs::read_to_string(&manifest_path)
        .map_err(|error| format!("failed to read {}: {error}", manifest_path.display()))?;
    ensure_bootstrap_bin_target(&mut manifest);
    fs::write(&manifest_path, manifest.as_bytes())
        .map_err(|error| format!("failed to write {}: {error}", manifest_path.display()))?;

    fs::write(
        src_dir.join("main.rs"),
        include_str!("templates/init_bootstrap_main.rs").as_bytes(),
    )
    .map_err(|error| format!("failed to write {}: {error}", src_dir.join("main.rs").display()))?;
    let main_ers = src_dir.join("main.ers");
    if !main_ers.exists() {
        fs::write(&main_ers, include_str!("templates/init_main.ers").as_bytes())
            .map_err(|error| format!("failed to write {}: {error}", main_ers.display()))?;
    }
    Ok(())
}

fn ensure_bootstrap_bin_target(manifest: &mut String) {
    if manifest.contains("[[bin]]") && manifest.contains("path = \"src/main.rs\"") {
        return;
    }
    let name = parse_manifest_package_name(manifest).unwrap_or_else(|| "app".to_string());
    if !manifest.ends_with('\n') {
        manifest.push('\n');
    }
    manifest.push('\n');
    manifest.push_str("[[bin]]\n");
    manifest.push_str(&format!("name = \"{name}\"\n"));
    manifest.push_str("path = \"src/main.rs\"\n");
}

fn parse_manifest_package_name(manifest: &str) -> Option<String> {
    let mut in_package = false;
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_package = trimmed == "[package]";
            continue;
        }
        if !in_package {
            continue;
        }
        if let Some(raw) = trimmed.strip_prefix("name =") {
            let quoted = raw.trim().trim_matches('"');
            if !quoted.is_empty() {
                return Some(quoted.to_string());
            }
        }
    }
    None
}

enum EmitTarget {
    Stdout,
    File(PathBuf),
}

fn apply_experiment_flag(flag: &str, experiments: &mut ExperimentFlags) -> bool {
    match flag {
        "--exp-move-mut-args" => {
            experiments.move_mut_args = true;
            true
        }
        "--exp-infer-local-bidi" => {
            experiments.infer_local_bidi = true;
            true
        }
        "--exp-effect-rows-internal" => {
            experiments.effect_rows_internal = true;
            true
        }
        "--exp-infer-principal-fallback" => {
            experiments.infer_principal_fallback = true;
            true
        }
        _ => false,
    }
}

fn parse_compile_options(args: &[String]) -> Result<CompileOptions, String> {
    let mut options = CompileOptions::default();
    let mut index = 0usize;
    while index < args.len() {
        let arg = &args[index];
        if apply_experiment_flag(arg, &mut options.experiments) {
            index += 1;
            continue;
        }
        match arg.as_str() {
            "--fail-on-hot-clone" => {
                options.fail_on_hot_clone = true;
                index += 1;
            }
            "--allow-hot-clone-place" => {
                if index + 1 >= args.len() {
                    return Err("`--allow-hot-clone-place` expects a place name".to_string());
                }
                options
                    .allow_hot_clone_places
                    .push(args[index + 1].clone());
                index += 2;
            }
            "--force-clone-place" => {
                if index + 1 >= args.len() {
                    return Err("`--force-clone-place` expects a place name".to_string());
                }
                options.forced_clone_places.push(args[index + 1].clone());
                index += 2;
            }
            _ => return Err(format!("unknown argument '{arg}'")),
        }
    }
    Ok(options)
}
