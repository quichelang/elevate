use std::env;
use std::path::PathBuf;
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
    eprintln!("experiment flags:");
    eprintln!("  --exp-move-mut-args");
    eprintln!("  --exp-infer-local-bidi");
    eprintln!("  --exp-effect-rows-internal");
    eprintln!("  --exp-infer-principal-fallback");
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
    for arg in args {
        if !apply_experiment_flag(arg, &mut options.experiments) {
            return Err(format!("unknown argument '{arg}'"));
        }
    }
    Ok(options)
}
