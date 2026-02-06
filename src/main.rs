use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        eprintln!("usage:");
        eprintln!("  elevate <input-file.ers> [--emit-rust <output-file>]");
        eprintln!("  elevate build <crate-root>");
        process::exit(2);
    }

    if args[0] == "build" {
        if args.len() < 2 || args.len() > 3 {
            eprintln!("usage: elevate build <crate-root> [--release]");
            process::exit(2);
        }
        let crate_root = PathBuf::from(&args[1]);
        let release = if args.len() == 3 {
            if args[2] != "--release" {
                eprintln!("error: unknown argument '{}'", args[2]);
                process::exit(2);
            }
            true
        } else {
            false
        };
        match elevate::crate_builder::build_ers_crate(&crate_root, release) {
            Ok(summary) => {
                println!(
                    "built generated crate at {} (transpiled {}, copied {}), artifacts in {}",
                    summary.generated_root.display(),
                    summary.transpiled_files,
                    summary.copied_files,
                    summary.source_root.join("target").display()
                );
                process::exit(0);
            }
            Err(error) => {
                eprintln!("{error}");
                process::exit(1);
            }
        }
    }

    let input_path = PathBuf::from(&args[0]);
    let mut emit_path: Option<PathBuf> = None;
    let mut idx = 1;
    while idx < args.len() {
        if args[idx] == "--emit-rust" {
            if idx + 1 >= args.len() {
                eprintln!("error: --emit-rust requires an output file path");
                process::exit(2);
            }
            emit_path = Some(PathBuf::from(&args[idx + 1]));
            idx += 2;
            continue;
        }
        eprintln!("error: unknown argument '{}'", args[idx]);
        process::exit(2);
    }

    let source = match elevate::source::load_file(&input_path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    };

    let output = match elevate::compile_source(&source) {
        Ok(output) => output,
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    };

    if let Some(path) = emit_path {
        if let Err(error) = std::fs::write(&path, output.rust_code.as_bytes()) {
            eprintln!("failed to write {}: {error}", path.display());
            process::exit(1);
        }
    } else {
        println!("{}", output.rust_code);
    }
}
