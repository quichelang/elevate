use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::process::Command;
use std::time::Instant;
use std::time::{SystemTime, UNIX_EPOCH};

use elevate::{CompileOptions, ExperimentFlags};

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        usage();
        process::exit(2);
    }

    match args[0].as_str() {
        "build" => run_build(&args[1..]),
        "run" => run_run(&args[1..]),
        "test" => run_test(&args[1..]),
        "bench" => run_bench(&args[1..]),
        "init" => run_init(&args[1..]),
        _ => run_compile(&args),
    }
}

fn run_build(args: &[String]) {
    if args.is_empty() {
        eprintln!(
            "usage: elevate build <crate-root|script.ers> [--release] [--out <output-file>] [experiment flags]"
        );
        process::exit(2);
    }
    let target_path = PathBuf::from(&args[0]);
    let mut release = false;
    let mut out: Option<PathBuf> = None;
    let mut options_args = Vec::new();
    let mut index = 1usize;
    while index < args.len() {
        match args[index].as_str() {
            "--release" => {
                release = true;
                index += 1;
            }
            "--out" => {
                if index + 1 >= args.len() {
                    eprintln!("error: `--out` expects a file path");
                    process::exit(2);
                }
                out = Some(PathBuf::from(&args[index + 1]));
                index += 2;
            }
            _ => {
                options_args.push(args[index].clone());
                index += 1;
            }
        }
    }
    let options = parse_compile_options(&options_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });

    match detect_target_kind(&target_path).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    }) {
        TargetKind::Crate => {
            if out.is_some() {
                eprintln!("error: `--out` is only supported when building a single .ers script");
                process::exit(2);
            }
            match elevate::crate_builder::build_ers_crate_with_options(
                &target_path,
                release,
                &options,
            ) {
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
        TargetKind::Script => {
            let output_path = out.unwrap_or_else(|| default_script_binary_path(&target_path));
            build_script_binary(&target_path, &output_path, release, &options).unwrap_or_else(
                |error| {
                    eprintln!("{error}");
                    process::exit(1);
                },
            );
            println!(
                "built script binary {} -> {}",
                target_path.display(),
                output_path.display()
            );
        }
    }
}

fn run_run(args: &[String]) {
    if args.is_empty() {
        eprintln!(
            "usage: elevate run <crate-root|script.ers> [--release] [experiment flags] [-- <program args>]"
        );
        process::exit(2);
    }
    let target_path = PathBuf::from(&args[0]);
    let mut release = false;
    let mut options_args = Vec::new();
    let mut program_args = Vec::new();
    let mut index = 1usize;
    while index < args.len() {
        if args[index] == "--" {
            program_args.extend_from_slice(&args[index + 1..]);
            break;
        }
        if args[index] == "--release" {
            release = true;
            index += 1;
            continue;
        }
        options_args.push(args[index].clone());
        index += 1;
    }
    let options = parse_compile_options(&options_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });

    match detect_target_kind(&target_path).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    }) {
        TargetKind::Crate => {
            let summary = elevate::crate_builder::build_ers_crate_with_options(
                &target_path,
                release,
                &options,
            )
            .unwrap_or_else(|error| {
                eprintln!("{error}");
                process::exit(1);
            });
            let mut cmd = Command::new("cargo");
            cmd.arg("run")
                .arg("--manifest-path")
                .arg(summary.generated_root.join("Cargo.toml"))
                .arg("--target-dir")
                .arg(summary.source_root.join("target"));
            if release {
                cmd.arg("--release");
            }
            if !program_args.is_empty() {
                cmd.arg("--");
                cmd.args(program_args.iter());
            }
            let status = cmd.status().unwrap_or_else(|error| {
                eprintln!(
                    "failed to run cargo for {}: {error}",
                    summary.generated_root.display()
                );
                process::exit(1);
            });
            if !status.success() {
                process::exit(status.code().unwrap_or(1));
            }
        }
        TargetKind::Script => {
            run_script(&target_path, release, &options, &program_args).unwrap_or_else(|error| {
                eprintln!("{error}");
                process::exit(1);
            });
        }
    }
}

fn run_script(
    script_path: &Path,
    release: bool,
    options: &CompileOptions,
    program_args: &[String],
) -> Result<(), String> {
    let rust_code = compile_script_to_rust(script_path, options)?;
    let rust_path = unique_temp_path("elevate-script-runner", "rs");
    let binary_path = unique_temp_path("elevate-script-runner", binary_ext());
    if let Some(parent) = rust_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create temp directory {}: {error}",
                parent.display()
            )
        })?;
    }
    fs::write(&rust_path, rust_code.as_bytes())
        .map_err(|error| format!("failed to write {}: {error}", rust_path.display()))?;
    compile_rust_to_binary(&rust_path, &binary_path, release)?;
    let mut cmd = Command::new(&binary_path);
    cmd.args(program_args.iter());
    let status = cmd
        .status()
        .map_err(|error| format!("failed to execute {}: {error}", binary_path.display()))?;
    let _ = fs::remove_file(&rust_path);
    let _ = fs::remove_file(&binary_path);
    if !status.success() {
        process::exit(status.code().unwrap_or(1));
    }
    Ok(())
}

fn build_script_binary(
    script_path: &Path,
    output_path: &Path,
    release: bool,
    options: &CompileOptions,
) -> Result<(), String> {
    let rust_code = compile_script_to_rust(script_path, options)?;
    let rust_path = unique_temp_path("elevate-script-build", "rs");
    if let Some(parent) = rust_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create temp directory {}: {error}",
                parent.display()
            )
        })?;
    }
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create output directory {}: {error}",
                parent.display()
            )
        })?;
    }
    fs::write(&rust_path, rust_code.as_bytes())
        .map_err(|error| format!("failed to write {}: {error}", rust_path.display()))?;
    let result = compile_rust_to_binary(&rust_path, output_path, release);
    let _ = fs::remove_file(&rust_path);
    result
}

fn compile_script_to_rust(script_path: &Path, options: &CompileOptions) -> Result<String, String> {
    let source = elevate::source::load_file(script_path)?;
    let mut compile_options = options.clone();
    compile_options.source_name = Some(script_path.display().to_string());
    let output = elevate::compile_source_with_options(&source, &compile_options)
        .map_err(|error| error.to_string())?;
    for warning in &output.warnings {
        eprintln!(
            "warning: {}",
            elevate::source_map::render_diagnostic(
                warning,
                compile_options.source_name.as_deref(),
                Some(&source)
            )
        );
    }
    Ok(output.rust_code)
}

fn compile_rust_to_binary(
    rust_path: &Path,
    output_path: &Path,
    release: bool,
) -> Result<(), String> {
    let mut cmd = Command::new("rustc");
    cmd.arg("--edition=2021")
        .arg(rust_path)
        .arg("-o")
        .arg(output_path);
    if release {
        cmd.arg("-O");
    }
    let output = cmd
        .output()
        .map_err(|error| format!("failed to run rustc for {}: {error}", rust_path.display()))?;
    if !output.status.success() {
        return Err(format!(
            "rustc failed for {} (status: {})\n{}",
            rust_path.display(),
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(())
}

fn default_script_binary_path(script_path: &Path) -> PathBuf {
    let stem = script_path
        .file_stem()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .unwrap_or("app");
    script_path.with_file_name(format!("{stem}{}", binary_ext()))
}

fn unique_temp_path(prefix: &str, ext: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    let pid = process::id();
    let suffix = if ext.is_empty() {
        format!("{prefix}-{pid}-{nanos}")
    } else {
        format!("{prefix}-{pid}-{nanos}.{ext}")
    };
    env::temp_dir().join(suffix)
}

fn binary_ext() -> &'static str {
    if cfg!(windows) { "exe" } else { "" }
}

enum TargetKind {
    Crate,
    Script,
}

fn detect_target_kind(path: &Path) -> Result<TargetKind, String> {
    if path.is_dir() {
        return Ok(TargetKind::Crate);
    }
    if path
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext == "ers")
    {
        return Ok(TargetKind::Script);
    }
    if path.is_file() {
        return Err(format!(
            "expected a crate directory or .ers script path, got file {}",
            path.display()
        ));
    }
    Err(format!(
        "path does not exist or is unsupported: {} (expected crate directory or .ers script)",
        path.display()
    ))
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
    let mut options = parse_compile_options(&options_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });
    options.source_name = Some(input_path.display().to_string());

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
    for warning in &output.warnings {
        eprintln!(
            "warning: {}",
            elevate::source_map::render_diagnostic(
                warning,
                options.source_name.as_deref(),
                Some(&source)
            )
        );
    }

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

fn run_bench(args: &[String]) {
    let mut iterations = 30usize;
    let mut warmup = 3usize;
    let mut out: Option<PathBuf> = None;
    let mut compare: Option<PathBuf> = None;
    let mut fail_on_regression = false;
    let mut options_args = Vec::new();
    let mut idx = 0usize;
    while idx < args.len() {
        match args[idx].as_str() {
            "--iters" => {
                if idx + 1 >= args.len() {
                    eprintln!("error: --iters expects a positive integer");
                    process::exit(2);
                }
                iterations = args[idx + 1].parse::<usize>().unwrap_or_else(|_| {
                    eprintln!("error: invalid --iters value `{}`", args[idx + 1]);
                    process::exit(2);
                });
                idx += 2;
            }
            "--warmup" => {
                if idx + 1 >= args.len() {
                    eprintln!("error: --warmup expects a non-negative integer");
                    process::exit(2);
                }
                warmup = args[idx + 1].parse::<usize>().unwrap_or_else(|_| {
                    eprintln!("error: invalid --warmup value `{}`", args[idx + 1]);
                    process::exit(2);
                });
                idx += 2;
            }
            "--out" => {
                if idx + 1 >= args.len() {
                    eprintln!("error: --out expects a file path");
                    process::exit(2);
                }
                out = Some(PathBuf::from(&args[idx + 1]));
                idx += 2;
            }
            "--compare" => {
                if idx + 1 >= args.len() {
                    eprintln!("error: --compare expects a CSV file path");
                    process::exit(2);
                }
                compare = Some(PathBuf::from(&args[idx + 1]));
                idx += 2;
            }
            "--fail-on-regression" => {
                fail_on_regression = true;
                idx += 1;
            }
            other => {
                options_args.push(other.to_string());
                idx += 1;
            }
        }
    }
    if iterations == 0 {
        eprintln!("error: --iters must be greater than zero");
        process::exit(2);
    }
    let options = parse_compile_options(&options_args).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(2);
    });

    let mut results = Vec::new();
    let cases = benchmark_cases();
    for case in cases {
        for _ in 0..warmup {
            if let Err(error) = elevate::compile_source_with_options(case.source, &options) {
                eprintln!("benchmark warmup failed for `{}`: {error}", case.name);
                process::exit(1);
            }
        }
        let mut timings = Vec::with_capacity(iterations);
        let mut latest = None;
        for _ in 0..iterations {
            let started = Instant::now();
            let output = elevate::compile_source_with_options(case.source, &options)
                .unwrap_or_else(|error| {
                    eprintln!("benchmark compile failed for `{}`: {error}", case.name);
                    process::exit(1);
                });
            timings.push(started.elapsed().as_secs_f64() * 1000.0);
            latest = Some(output);
        }
        let output = latest.expect("at least one iteration");
        timings.sort_by(|a, b| a.total_cmp(b));
        let median_ms = percentile_ms(&timings, 0.50);
        let p95_ms = percentile_ms(&timings, 0.95);
        let min_ms = *timings.first().unwrap_or(&0.0);
        let max_ms = *timings.last().unwrap_or(&0.0);
        let clone_calls = output.rust_code.match_indices(".clone(").count()
            + output.rust_code.match_indices(".clone()").count();
        let auto_clone_notes = output
            .ownership_notes
            .iter()
            .filter(|note| note.starts_with("auto-clone inserted"))
            .count();
        let hot_clone_notes = output
            .ownership_notes
            .iter()
            .filter(|note| note.starts_with("hot-clone:auto "))
            .count();
        results.push(BenchResult {
            name: case.name.to_string(),
            iterations,
            warmup,
            median_ms,
            p95_ms,
            min_ms,
            max_ms,
            clone_calls,
            auto_clone_notes,
            hot_clone_notes,
            rust_bytes: output.rust_code.len(),
        });
    }

    if let Some(compare_path) = compare.as_ref() {
        let baseline = read_benchmark_csv(compare_path).unwrap_or_else(|error| {
            eprintln!(
                "failed to read benchmark baseline {}: {error}",
                compare_path.display()
            );
            process::exit(1);
        });
        let regressions = print_comparison_table(&results, &baseline);
        if regressions > 0 {
            eprintln!("benchmark regressions detected: {regressions}");
            if fail_on_regression {
                process::exit(1);
            }
        }
    } else {
        print_benchmark_table(&results);
    }

    if let Some(path) = out {
        let mut csv = String::from(
            "case,iterations,warmup,median_ms,p95_ms,min_ms,max_ms,clone_calls,auto_clone_notes,hot_clone_notes,rust_bytes\n",
        );
        for row in &results {
            csv.push_str(&format!(
                "{},{},{},{:.3},{:.3},{:.3},{:.3},{},{},{},{}\n",
                row.name,
                row.iterations,
                row.warmup,
                row.median_ms,
                row.p95_ms,
                row.min_ms,
                row.max_ms,
                row.clone_calls,
                row.auto_clone_notes,
                row.hot_clone_notes,
                row.rust_bytes
            ));
        }
        if let Err(error) = fs::write(&path, csv.as_bytes()) {
            eprintln!(
                "failed to write benchmark report {}: {error}",
                path.display()
            );
            process::exit(1);
        }
        println!("wrote benchmark report to {}", path.display());
    }
}

fn print_benchmark_table(results: &[BenchResult]) {
    println!(
        "{:<34} {:>9} {:>9} {:>9} {:>9} {:>8} {:>8} {:>8}",
        "case", "median", "p95", "min", "max", "clone()", "auto", "hot"
    );
    for row in results {
        println!(
            "{:<34} {:>9.2} {:>9.2} {:>9.2} {:>9.2} {:>8} {:>8} {:>8}",
            row.name,
            row.median_ms,
            row.p95_ms,
            row.min_ms,
            row.max_ms,
            row.clone_calls,
            row.auto_clone_notes,
            row.hot_clone_notes
        );
    }
}

fn print_comparison_table(
    current: &[BenchResult],
    baseline: &HashMap<String, BenchResultSnapshot>,
) -> usize {
    println!(
        "{:<34} {:>9} {:>9} {:>9} {:>8} {:>8} {:>11} {:>11}",
        "case", "median", "p95", "clone()", "auto", "hot", "delta-median", "status"
    );
    let mut regressions = 0usize;
    for row in current {
        let Some(base) = baseline.get(&row.name) else {
            println!(
                "{:<34} {:>9.2} {:>9.2} {:>9} {:>8} {:>8} {:>11} {:>11}",
                row.name,
                row.median_ms,
                row.p95_ms,
                row.clone_calls,
                row.auto_clone_notes,
                row.hot_clone_notes,
                "n/a",
                "NEW"
            );
            continue;
        };

        let mut row_regression = false;
        let mut row_improvement = false;

        let median_delta = percent_delta(row.median_ms, base.median_ms);
        let p95_delta = percent_delta(row.p95_ms, base.p95_ms);

        if is_timing_regression(row.median_ms, base.median_ms)
            || is_timing_regression(row.p95_ms, base.p95_ms)
        {
            row_regression = true;
        } else if is_timing_improvement(row.median_ms, base.median_ms)
            || is_timing_improvement(row.p95_ms, base.p95_ms)
        {
            row_improvement = true;
        }

        if row.clone_calls > base.clone_calls
            || row.auto_clone_notes > base.auto_clone_notes
            || row.hot_clone_notes > base.hot_clone_notes
        {
            row_regression = true;
        } else if row.clone_calls < base.clone_calls
            || row.auto_clone_notes < base.auto_clone_notes
            || row.hot_clone_notes < base.hot_clone_notes
        {
            row_improvement = true;
        }

        let status = if row_regression {
            regressions += 1;
            "REGRESSION"
        } else if row_improvement {
            "IMPROVED"
        } else {
            "UNCHANGED"
        };
        println!(
            "{:<34} {:>9.2} {:>9.2} {:>9} {:>8} {:>8} {:>10.1}% {:>11}",
            row.name,
            row.median_ms,
            row.p95_ms,
            row.clone_calls,
            row.auto_clone_notes,
            row.hot_clone_notes,
            median_delta,
            status
        );

        if status == "REGRESSION" {
            println!(
                "  note: p95 delta={:+.1}%, clone delta={:+}, auto delta={:+}, hot delta={:+}",
                p95_delta,
                row.clone_calls as isize - base.clone_calls as isize,
                row.auto_clone_notes as isize - base.auto_clone_notes as isize,
                row.hot_clone_notes as isize - base.hot_clone_notes as isize
            );
        }
    }
    regressions
}

fn percent_delta(current: f64, baseline: f64) -> f64 {
    if baseline <= 0.0 {
        return 0.0;
    }
    ((current - baseline) / baseline) * 100.0
}

fn is_timing_regression(current: f64, baseline: f64) -> bool {
    if baseline <= 0.0 {
        return false;
    }
    current > (baseline * 1.10) && (current - baseline) > 0.05
}

fn is_timing_improvement(current: f64, baseline: f64) -> bool {
    if baseline <= 0.0 {
        return false;
    }
    current < (baseline * 0.90) && (baseline - current) > 0.05
}

fn read_benchmark_csv(path: &Path) -> Result<HashMap<String, BenchResultSnapshot>, String> {
    let content = fs::read_to_string(path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let mut rows = HashMap::new();
    for (index, line) in content.lines().enumerate() {
        if index == 0 {
            continue;
        }
        if line.trim().is_empty() {
            continue;
        }
        let columns = line.split(',').map(|cell| cell.trim()).collect::<Vec<_>>();
        if columns.len() != 11 {
            return Err(format!(
                "invalid benchmark row {} in {}: expected 11 columns, got {}",
                index + 1,
                path.display(),
                columns.len()
            ));
        }
        let parse_usize = |text: &str, label: &str| -> Result<usize, String> {
            text.parse::<usize>().map_err(|_| {
                format!(
                    "invalid {label} `{text}` in row {} of {}",
                    index + 1,
                    path.display()
                )
            })
        };
        let parse_f64 = |text: &str, label: &str| -> Result<f64, String> {
            text.parse::<f64>().map_err(|_| {
                format!(
                    "invalid {label} `{text}` in row {} of {}",
                    index + 1,
                    path.display()
                )
            })
        };
        let name = columns[0].to_string();
        rows.insert(
            name,
            BenchResultSnapshot {
                median_ms: parse_f64(columns[3], "median_ms")?,
                p95_ms: parse_f64(columns[4], "p95_ms")?,
                clone_calls: parse_usize(columns[7], "clone_calls")?,
                auto_clone_notes: parse_usize(columns[8], "auto_clone_notes")?,
                hot_clone_notes: parse_usize(columns[9], "hot_clone_notes")?,
            },
        );
    }
    Ok(rows)
}

fn percentile_ms(sorted_ms: &[f64], quantile: f64) -> f64 {
    if sorted_ms.is_empty() {
        return 0.0;
    }
    let last = sorted_ms.len() - 1;
    let idx = ((last as f64) * quantile).round() as usize;
    sorted_ms[idx.min(last)]
}

struct BenchCase {
    name: &'static str,
    source: &'static str,
}

struct BenchResult {
    name: String,
    iterations: usize,
    warmup: usize,
    median_ms: f64,
    p95_ms: f64,
    min_ms: f64,
    max_ms: f64,
    clone_calls: usize,
    auto_clone_notes: usize,
    hot_clone_notes: usize,
    rust_bytes: usize,
}

struct BenchResultSnapshot {
    median_ms: f64,
    p95_ms: f64,
    clone_calls: usize,
    auto_clone_notes: usize,
    hot_clone_notes: usize,
}

fn benchmark_cases() -> Vec<BenchCase> {
    vec![
        BenchCase {
            name: "strings: read-only borrow",
            source: r#"
                fn demo(text: String, needle: String) -> bool {
                    std::mem::drop(str::contains(text, needle));
                    return str::contains(text, needle);
                }
            "#,
        },
        BenchCase {
            name: "strings: mixed move chain",
            source: r#"
                rust { pub fn consume(bytes: Vec<u8>) -> usize { bytes.len() } }
                fn demo(text: String) -> usize {
                    std::mem::drop(text.len());
                    consume(text.into_bytes())
                }
            "#,
        },
        BenchCase {
            name: "vec: loop ownership",
            source: r#"
                rust { pub fn consume(values: Vec<i64>) { let _ = values.len(); } }
                fn demo(values: Vec<i64>) {
                    for _ in 0..3 {
                        consume(values);
                    }
                    return;
                }
            "#,
        },
        BenchCase {
            name: "interop: runtime_draw_scene",
            source: r#"
                rust {
                    pub fn runtime_draw_scene(cells: &[i64], fixed: &[bool]) -> bool {
                        !cells.is_empty() && !fixed.is_empty()
                    }
                }
                fn demo(cells: Vec<i64>, fixed: Vec<bool>) -> bool {
                    const drawn = runtime_draw_scene(cells, fixed);
                    if drawn {
                        return runtime_draw_scene(cells, fixed);
                    }
                    runtime_draw_scene(cells, fixed)
                }
            "#,
        },
        BenchCase {
            name: "nominal: by-value associated",
            source: r#"
                rust {
                    pub struct Board;
                    impl Board {
                        pub fn is_complete(board: Board) -> bool {
                            let _ = board;
                            true
                        }
                    }
                }
                fn demo(board: Board) -> bool {
                    Board::is_complete(board)
                }
            "#,
        },
        BenchCase {
            name: "contrived: nested containers",
            source: r#"
                rust { pub fn observe(data: &[Vec<String>]) -> bool { !data.is_empty() } }
                fn demo(data: Vec<Vec<String>>) -> bool {
                    std::mem::drop(observe(data));
                    observe(data)
                }
            "#,
        },
    ]
}

fn usage() {
    eprintln!("usage:");
    eprintln!("  elevate <input-file.ers> [--emit-rust [output-file]] [experiment flags]");
    eprintln!(
        "  elevate build <crate-root|script.ers> [--release] [--out <output-file>] [experiment flags]"
    );
    eprintln!(
        "  elevate run <crate-root|script.ers> [--release] [experiment flags] [-- <program args>]"
    );
    eprintln!("  elevate test <crate-root> [experiment flags]");
    eprintln!(
        "  elevate bench [--iters N] [--warmup N] [--out report.csv] [--compare baseline.csv] [--fail-on-regression] [experiment flags]"
    );
    eprintln!("  elevate init <crate-root> [cargo init flags]");
    eprintln!("experiment flags:");
    eprintln!("  --exp-move-mut-args");
    eprintln!("  --exp-infer-local-bidi");
    eprintln!("  --exp-literal-bidi");
    eprintln!("  --exp-effect-rows");
    eprintln!("  --exp-effect-rows-internal");
    eprintln!("  --exp-infer-principal-fallback");
    eprintln!("  --exp-numeric-coercion");
    eprintln!("  --warn-missing-types");
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
    .map_err(|error| {
        format!(
            "failed to write {}: {error}",
            src_dir.join("main.rs").display()
        )
    })?;
    let main_ers = src_dir.join("main.ers");
    if !main_ers.exists() {
        fs::write(
            &main_ers,
            include_str!("templates/init_main.ers").as_bytes(),
        )
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
        "--exp-literal-bidi" => {
            experiments.infer_literal_bidi = true;
            true
        }
        "--exp-effect-rows" => {
            experiments.effect_rows = true;
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
        "--exp-numeric-coercion" => {
            experiments.numeric_coercion = true;
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
            "--warn-missing-types" => {
                options.warn_missing_types = true;
                index += 1;
            }
            "--allow-hot-clone-place" => {
                if index + 1 >= args.len() {
                    return Err("`--allow-hot-clone-place` expects a place name".to_string());
                }
                options.allow_hot_clone_places.push(args[index + 1].clone());
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
    elevate::validate_experiment_flags(&options.experiments)?;
    Ok(options)
}

#[cfg(test)]
mod tests {
    use super::{is_timing_improvement, is_timing_regression, read_benchmark_csv};
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn timing_thresholds_apply_noise_guard() {
        assert!(!is_timing_regression(10.04, 10.0));
        assert!(!is_timing_regression(10.9, 10.0));
        assert!(is_timing_regression(11.2, 10.0));

        assert!(!is_timing_improvement(9.96, 10.0));
        assert!(!is_timing_improvement(9.2, 10.0));
        assert!(is_timing_improvement(8.9, 10.0));
    }

    #[test]
    fn benchmark_csv_parser_reads_expected_columns() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("elevate-bench-{nanos}.csv"));
        let content = "case,iterations,warmup,median_ms,p95_ms,min_ms,max_ms,clone_calls,auto_clone_notes,hot_clone_notes,rust_bytes\ninterop: runtime_draw_scene,20,3,1.100,1.900,0.900,2.100,0,0,0,1234\n";
        fs::write(&path, content).expect("write csv fixture should succeed");
        let parsed = read_benchmark_csv(&path).expect("csv should parse");
        let row = parsed
            .get("interop: runtime_draw_scene")
            .expect("expected case row");
        assert_eq!(row.clone_calls, 0);
        assert_eq!(row.auto_clone_notes, 0);
        assert_eq!(row.hot_clone_notes, 0);
        assert!((row.median_ms - 1.1).abs() < 0.0001);
        assert!((row.p95_ms - 1.9).abs() < 0.0001);
    }
}
