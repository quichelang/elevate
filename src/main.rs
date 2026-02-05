use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        eprintln!("usage: elevate <input-file> [--emit-rust <output-file>]");
        process::exit(2);
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
