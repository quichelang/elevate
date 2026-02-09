use crate::CompileOptions;
use crate::ast::{Block, Expr, Item, Module, Stmt, StructLiteralField};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{collections::BTreeSet, fmt::Write as _};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildSummary {
    pub source_root: PathBuf,
    pub generated_root: PathBuf,
    pub transpiled_files: usize,
    pub copied_files: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct InteropContract {
    allowed_paths: BTreeSet<String>,
    adapters: Vec<InteropAdapter>,
    runtime_lines: Vec<String>,
    runtime_presets: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InteropAdapter {
    alias: String,
    target: String,
    params: Vec<String>,
    return_type: String,
    transform: InteropTransform,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InteropTransform {
    Direct,
    UnwrapToOwnedString,
    UnwrapSplitOnceToOwnedTuple,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContractType {
    I64,
    Bool,
    String,
    StrRef,
    Unknown,
}

pub fn build_ers_crate(crate_root: &Path, release: bool) -> Result<BuildSummary, String> {
    build_ers_crate_with_options(crate_root, release, &CompileOptions::default())
}

pub fn build_ers_crate_with_options(
    crate_root: &Path,
    release: bool,
    options: &CompileOptions,
) -> Result<BuildSummary, String> {
    const MAX_BUILD_FEEDBACK_RETRIES: usize = 2;

    let mut adaptive_options = options.clone();
    let mut summary = transpile_ers_crate_with_options(crate_root, &adaptive_options)?;
    let mut retries_left = MAX_BUILD_FEEDBACK_RETRIES;
    loop {
        match run_generated_cargo_build(&summary.source_root, &summary.generated_root, release) {
            Ok(()) => return Ok(summary),
            Err(error) => {
                let inferred = infer_direct_borrow_hints_from_cargo_error(
                    &error.stderr,
                    &summary.generated_root,
                );
                let added_borrow = merge_direct_borrow_hints(&mut adaptive_options, inferred);
                let inferred_clones = infer_forced_clone_places_from_cargo_error(
                    &error.stderr,
                    &summary.generated_root,
                );
                let added_clone = merge_forced_clone_places(&mut adaptive_options, inferred_clones);
                if (added_borrow + added_clone) == 0 || retries_left == 0 {
                    return Err(format!(
                        "cargo build failed for generated crate {} (status: {})\n{}",
                        error.manifest.display(),
                        error.status,
                        error.stderr.trim()
                    ));
                }
                retries_left -= 1;
                summary = transpile_ers_crate_with_options(crate_root, &adaptive_options)?;
            }
        }
    }
}

pub fn transpile_ers_crate(crate_root: &Path) -> Result<BuildSummary, String> {
    transpile_ers_crate_with_options(crate_root, &CompileOptions::default())
}

pub fn transpile_ers_crate_with_options(
    crate_root: &Path,
    options: &CompileOptions,
) -> Result<BuildSummary, String> {
    let source_root = canonicalize(crate_root)?;
    let source_manifest = source_root.join("Cargo.toml");
    let source_src = source_root.join("src");
    let interop_contract = load_interop_contract(&source_root)?;

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

    let mut transpiled_ers = Vec::new();
    process_src_dir(
        &source_src,
        &source_src,
        &generated_root.join("src"),
        &mut summary,
        &interop_contract,
        options,
        &mut transpiled_ers,
    )?;
    inject_generated_module_declarations(&generated_root.join("src"), &transpiled_ers)?;
    emit_interop_adapter_module(&generated_root.join("src"), &interop_contract)?;
    Ok(summary)
}

#[derive(Debug, Clone)]
struct CargoBuildError {
    manifest: PathBuf,
    status: i32,
    stderr: String,
}

fn run_generated_cargo_build(
    source_root: &Path,
    generated_root: &Path,
    release: bool,
) -> Result<(), CargoBuildError> {
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

    let output = cmd.output().map_err(|error| CargoBuildError {
        manifest: manifest.clone(),
        status: -1,
        stderr: format!(
            "failed to run cargo build for generated crate {}: {error}",
            manifest.display()
        ),
    })?;
    if !output.status.success() {
        return Err(CargoBuildError {
            manifest,
            status: output.status.code().unwrap_or(-1),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        });
    }
    Ok(())
}

fn merge_direct_borrow_hints(
    options: &mut CompileOptions,
    hints: Vec<crate::DirectBorrowHint>,
) -> usize {
    let mut merged = options
        .direct_borrow_hints
        .iter()
        .map(|hint| {
            (
                hint.path.clone(),
                hint.borrowed_arg_indexes
                    .iter()
                    .copied()
                    .collect::<BTreeSet<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    let mut added = 0usize;
    for hint in hints {
        let entry = merged.entry(hint.path).or_default();
        for index in hint.borrowed_arg_indexes {
            if entry.insert(index) {
                added += 1;
            }
        }
    }
    let mut flattened = merged
        .into_iter()
        .map(|(path, indexes)| crate::DirectBorrowHint {
            path,
            borrowed_arg_indexes: indexes.into_iter().collect(),
        })
        .collect::<Vec<_>>();
    flattened.sort_by(|left, right| left.path.cmp(&right.path));
    options.direct_borrow_hints = flattened;
    added
}

fn merge_forced_clone_places(options: &mut CompileOptions, places: Vec<String>) -> usize {
    let mut set = options
        .forced_clone_places
        .iter()
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut added = 0usize;
    for place in places {
        if set.insert(place) {
            added += 1;
        }
    }
    options.forced_clone_places = set.into_iter().collect();
    added
}

fn infer_direct_borrow_hints_from_cargo_error(
    stderr: &str,
    generated_root: &Path,
) -> Vec<crate::DirectBorrowHint> {
    let mut source_cache = HashMap::<PathBuf, Vec<String>>::new();
    let mut current_location: Option<(PathBuf, usize)> = None;
    let mut hinted = HashMap::<String, BTreeSet<usize>>::new();
    let lines = stderr.lines().collect::<Vec<_>>();
    let mut index = 0usize;
    while index < lines.len() {
        let line = strip_ansi(lines[index]);
        if let Some(location) = parse_diagnostic_location(&line, generated_root) {
            current_location = Some(location);
        }
        if line.contains("help: consider borrowing here") {
            let mut probe = index + 1;
            while probe < lines.len() {
                let candidate = strip_ansi(lines[probe]);
                if let Some((snippet_line, suggested_code)) = parse_snippet_line(&candidate) {
                    if let Some((path, fallback_line)) = &current_location {
                        let line_no = if snippet_line > 0 {
                            snippet_line
                        } else {
                            *fallback_line
                        };
                        if let Some(original) = source_line(path, line_no, &mut source_cache)
                            && let Some((callee, borrowed_indexes)) =
                                infer_borrow_indexes_from_suggestion(&original, &suggested_code)
                        {
                            let entry = hinted.entry(callee).or_default();
                            for borrow_index in borrowed_indexes {
                                entry.insert(borrow_index);
                            }
                        }
                    }
                    break;
                }
                if candidate.trim_start().starts_with("-->")
                    || candidate.trim_start().starts_with("error")
                    || candidate.trim_start().starts_with("warning")
                    || candidate.trim_start().starts_with("note")
                    || candidate.trim_start().starts_with("help:")
                {
                    break;
                }
                probe += 1;
            }
            index = probe;
            continue;
        }
        index += 1;
    }

    hinted
        .into_iter()
        .filter_map(|(path, indexes)| {
            if path.contains('.') || !path.contains("::") {
                return None;
            }
            Some(crate::DirectBorrowHint {
                path,
                borrowed_arg_indexes: indexes.into_iter().collect(),
            })
        })
        .collect()
}

fn infer_forced_clone_places_from_cargo_error(stderr: &str, generated_root: &Path) -> Vec<String> {
    let mut source_cache = HashMap::<PathBuf, Vec<String>>::new();
    let mut current_location: Option<(PathBuf, usize)> = None;
    let mut hinted = BTreeSet::<String>::new();
    let lines = stderr.lines().collect::<Vec<_>>();
    let mut index = 0usize;
    while index < lines.len() {
        let line = strip_ansi(lines[index]);
        if let Some(location) = parse_diagnostic_location(&line, generated_root) {
            current_location = Some(location);
        }
        if line.contains("consider cloning the value")
            || line.contains("consider cloning this value")
            || line.contains("consider cloning")
        {
            let mut probe = index + 1;
            while probe < lines.len() {
                let candidate = strip_ansi(lines[probe]);
                if let Some((snippet_line, suggested_code)) = parse_snippet_line(&candidate) {
                    if let Some((path, fallback_line)) = &current_location {
                        let line_no = if snippet_line > 0 {
                            snippet_line
                        } else {
                            *fallback_line
                        };
                        if let Some(original) = source_line(path, line_no, &mut source_cache) {
                            for place in
                                infer_clone_places_from_suggestion(&original, &suggested_code)
                            {
                                hinted.insert(place);
                            }
                        }
                    }
                    break;
                }
                if candidate.trim_start().starts_with("-->")
                    || candidate.trim_start().starts_with("error")
                    || candidate.trim_start().starts_with("warning")
                    || candidate.trim_start().starts_with("note")
                    || candidate.trim_start().starts_with("help:")
                {
                    break;
                }
                probe += 1;
            }
            index = probe;
            continue;
        }
        index += 1;
    }
    hinted.into_iter().collect()
}

fn strip_ansi(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && chars.peek() == Some(&'[') {
            chars.next();
            while let Some(next) = chars.next() {
                if next.is_ascii_alphabetic() {
                    break;
                }
            }
            continue;
        }
        out.push(ch);
    }
    out
}

fn parse_diagnostic_location(line: &str, generated_root: &Path) -> Option<(PathBuf, usize)> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("-->") {
        return None;
    }
    let rest = trimmed.trim_start_matches("-->").trim();
    let mut parts = rest.rsplitn(3, ':');
    let _col = parts.next()?;
    let line_no = parts.next()?.parse::<usize>().ok()?;
    let path_part = parts.next()?.trim();
    let path = PathBuf::from(path_part);
    let normalized = if path.is_absolute() {
        path
    } else {
        generated_root.join(path)
    };
    Some((normalized, line_no))
}

fn parse_snippet_line(line: &str) -> Option<(usize, String)> {
    let trimmed = line.trim_start();
    let mut cursor = 0usize;
    let bytes = trimmed.as_bytes();
    while cursor < bytes.len() && bytes[cursor].is_ascii_digit() {
        cursor += 1;
    }
    if cursor == 0 {
        return None;
    }
    let line_no = trimmed[..cursor].parse::<usize>().ok()?;
    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
        cursor += 1;
    }
    if cursor >= bytes.len() || bytes[cursor] != b'|' {
        return None;
    }
    let code = trimmed[cursor + 1..].trim().to_string();
    Some((line_no, code))
}

fn source_line(
    path: &Path,
    line_no: usize,
    cache: &mut HashMap<PathBuf, Vec<String>>,
) -> Option<String> {
    if line_no == 0 {
        return None;
    }
    if !cache.contains_key(path) {
        let content = fs::read_to_string(path).ok()?;
        let lines = content
            .lines()
            .map(|line| line.to_string())
            .collect::<Vec<_>>();
        cache.insert(path.to_path_buf(), lines);
    }
    cache
        .get(path)
        .and_then(|lines| lines.get(line_no - 1))
        .cloned()
}

fn infer_borrow_indexes_from_suggestion(
    original_line: &str,
    suggested_line: &str,
) -> Option<(String, Vec<usize>)> {
    let (orig_callee, orig_args) = parse_first_call(original_line)?;
    let (suggested_callee, suggested_args) = parse_first_call(suggested_line)?;
    if orig_callee != suggested_callee || orig_args.len() != suggested_args.len() {
        return None;
    }

    let mut borrowed_indexes = Vec::new();
    for (index, (orig_arg, suggested_arg)) in
        orig_args.iter().zip(suggested_args.iter()).enumerate()
    {
        let orig_trimmed = orig_arg.trim();
        let suggested_trimmed = suggested_arg.trim();
        if suggested_trimmed.starts_with('&')
            && !orig_trimmed.starts_with('&')
            && suggested_trimmed.trim_start_matches('&').trim() == orig_trimmed
        {
            borrowed_indexes.push(index);
        }
    }
    if borrowed_indexes.is_empty() {
        None
    } else {
        Some((orig_callee, borrowed_indexes))
    }
}

fn infer_clone_places_from_suggestion(original_line: &str, suggested_line: &str) -> Vec<String> {
    let mut places = Vec::new();
    for token in extract_clone_tokens(suggested_line) {
        let bare = token.trim_end_matches(".clone()");
        if bare.is_empty() {
            continue;
        }
        if !original_line.contains(bare) {
            continue;
        }
        if !original_line.contains(&token) {
            places.push(bare.to_string());
        }
    }
    places.sort();
    places.dedup();
    places
}

fn extract_clone_tokens(line: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let needle = ".clone()";
    let mut search_from = 0usize;
    while let Some(rel_idx) = line[search_from..].find(needle) {
        let clone_start = search_from + rel_idx;
        let mut start = clone_start;
        while start > 0 {
            let prev = line[..start].chars().last().unwrap_or(' ');
            if prev.is_ascii_alphanumeric() || matches!(prev, '_' | '.' | '[' | ']') {
                start -= prev.len_utf8();
            } else {
                break;
            }
        }
        if start < clone_start {
            tokens.push(line[start..clone_start + needle.len()].to_string());
        }
        search_from = clone_start + needle.len();
    }
    tokens
}

fn parse_first_call(line: &str) -> Option<(String, Vec<String>)> {
    let chars = line.char_indices().collect::<Vec<_>>();
    for (offset, ch) in chars {
        if ch != '(' {
            continue;
        }
        let mut end = offset;
        while end > 0
            && line[..end]
                .chars()
                .last()
                .is_some_and(|c| c.is_ascii_whitespace())
        {
            end -= line[..end].chars().last()?.len_utf8();
        }
        let mut start = end;
        while start > 0 {
            let prev = line[..start].chars().last()?;
            if prev.is_ascii_alphanumeric() || matches!(prev, '_' | ':' | '.') {
                start -= prev.len_utf8();
            } else {
                break;
            }
        }
        if start == end {
            continue;
        }
        let callee = line[start..end].trim().to_string();
        if callee.is_empty() {
            continue;
        }
        let mut depth = 1usize;
        let mut close = None;
        for (idx, c) in line[offset + 1..].char_indices() {
            match c {
                '(' => depth += 1,
                ')' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        close = Some(offset + 1 + idx);
                        break;
                    }
                }
                _ => {}
            }
        }
        let close = close?;
        let args = split_top_level_call_args(&line[offset + 1..close])
            .into_iter()
            .map(|arg| arg.trim().to_string())
            .collect::<Vec<_>>();
        return Some((callee, args));
    }
    None
}

fn split_top_level_call_args(input: &str) -> Vec<&str> {
    let mut args = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut angle_depth = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '{' => brace_depth += 1,
            '}' => brace_depth = brace_depth.saturating_sub(1),
            '<' => angle_depth += 1,
            '>' => angle_depth = angle_depth.saturating_sub(1),
            ',' if paren_depth == 0
                && bracket_depth == 0
                && brace_depth == 0
                && angle_depth == 0 =>
            {
                args.push(input[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    let tail = input[start..].trim();
    if !tail.is_empty() {
        args.push(tail);
    }
    args
}

fn process_src_dir(
    root_src: &Path,
    current: &Path,
    generated_src: &Path,
    summary: &mut BuildSummary,
    interop_contract: &InteropContract,
    options: &CompileOptions,
    transpiled_ers: &mut Vec<PathBuf>,
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
            process_src_dir(
                root_src,
                &path,
                generated_src,
                summary,
                interop_contract,
                options,
                transpiled_ers,
            )?;
            continue;
        }

        if path.extension() == Some(OsStr::new("ers")) {
            let source = fs::read_to_string(&path)
                .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
            validate_interop_contract_for_source(&source, &path, interop_contract)?;
            let output =
                compile_source_with_interop_contract(&source, interop_contract, options)
                    .map_err(|error| format_compile_error_with_context(&path, &source, &error))?;
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
            transpiled_ers.push(rel.to_path_buf());
            continue;
        }

        if should_skip_source_copy_because_ers_twin(&path) {
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

fn inject_generated_module_declarations(
    generated_src: &Path,
    transpiled_ers: &[PathBuf],
) -> Result<(), String> {
    let mut children_by_dir: HashMap<PathBuf, BTreeSet<String>> = HashMap::new();
    for rel in transpiled_ers {
        if rel.extension() != Some(OsStr::new("ers")) {
            continue;
        }
        let mut rel_no_ext = rel.clone();
        rel_no_ext.set_extension("");
        let parts = rel_no_ext
            .iter()
            .map(|part| part.to_string_lossy().to_string())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            continue;
        }
        if parts.len() == 1 && matches!(parts[0].as_str(), "lib" | "main") {
            continue;
        }
        let mut dir = PathBuf::new();
        for part in parts {
            children_by_dir
                .entry(dir.clone())
                .or_default()
                .insert(part.clone());
            dir.push(part);
        }
    }

    if children_by_dir.is_empty() {
        return Ok(());
    }

    let root_target = if generated_src.join("lib.rs").is_file() {
        Some(generated_src.join("lib.rs"))
    } else if generated_src.join("main.rs").is_file() {
        Some(generated_src.join("main.rs"))
    } else {
        None
    };
    if let Some(root_target) = root_target
        && let Some(root_children) = children_by_dir.get(&PathBuf::new())
    {
        inject_module_declarations_into_file(&root_target, root_children)?;
    }

    let mut dirs = children_by_dir.keys().cloned().collect::<Vec<_>>();
    dirs.sort_by_key(|path| path.components().count());
    for dir in dirs {
        if dir.as_os_str().is_empty() {
            continue;
        }
        let Some(children) = children_by_dir.get(&dir) else {
            continue;
        };
        let mut module_file = generated_src.join(&dir);
        module_file.set_extension("rs");
        let target = if module_file.is_file() {
            module_file
        } else {
            generated_src.join(&dir).join("mod.rs")
        };
        inject_module_declarations_into_file(&target, children)?;
    }

    Ok(())
}

fn inject_module_declarations_into_file(
    path: &Path,
    modules: &BTreeSet<String>,
) -> Result<(), String> {
    if modules.is_empty() {
        return Ok(());
    }
    let existing = if path.is_file() {
        fs::read_to_string(path)
            .map_err(|error| format!("failed to read {}: {error}", path.display()))?
    } else {
        String::new()
    };
    let missing = modules
        .iter()
        .filter(|name| !source_contains_module_decl(&existing, name))
        .cloned()
        .collect::<Vec<_>>();
    if missing.is_empty() {
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
    }
    let mut out = String::new();
    out.push_str("// auto-generated module declarations for transpiled .ers files\n");
    for name in &missing {
        out.push_str(&format!("pub mod {name};\n"));
    }
    out.push('\n');
    out.push_str(&existing);
    fs::write(path, out.as_bytes())
        .map_err(|error| format!("failed to write {}: {error}", path.display()))?;
    Ok(())
}

fn source_contains_module_decl(source: &str, module_name: &str) -> bool {
    source.lines().any(|line| {
        let trimmed = line.trim();
        trimmed == format!("mod {module_name};") || trimmed == format!("pub mod {module_name};")
    })
}

fn should_skip_source_copy_because_ers_twin(path: &Path) -> bool {
    if path.extension() != Some(OsStr::new("rs")) {
        return false;
    }
    let mut twin = path.to_path_buf();
    twin.set_extension("ers");
    twin.is_file()
}

fn compile_source_with_interop_contract(
    source: &str,
    contract: &InteropContract,
    options: &CompileOptions,
) -> Result<crate::CompilerOutput, crate::CompileError> {
    if contract.adapters.is_empty() {
        return crate::compile_source_with_options(source, options);
    }

    let tokens =
        crate::lexer::lex(source).map_err(|diagnostics| crate::CompileError {
            diagnostics,
            source_name: options.source_name.clone(),
            source_text: Some(source.to_string()),
        })?;
    let mut module = crate::parser::parse_module(tokens)
        .map_err(|diagnostics| crate::CompileError {
            diagnostics,
            source_name: options.source_name.clone(),
            source_text: Some(source.to_string()),
        })?;
    let validation = validate_adapter_callsites(&module, contract);
    if !validation.is_empty() {
        return Err(crate::CompileError {
            diagnostics: validation,
            source_name: options.source_name.clone(),
            source_text: Some(source.to_string()),
        });
    }
    rewrite_module_adapter_calls(&mut module, contract);
    crate::compile_ast_with_options(&module, options)
}

fn validate_adapter_callsites(
    module: &Module,
    contract: &InteropContract,
) -> Vec<crate::diag::Diagnostic> {
    if contract.adapters.is_empty() {
        return Vec::new();
    }
    let alias_arity = contract
        .adapters
        .iter()
        .map(|adapter| (adapter.alias.clone(), adapter.params.len()))
        .collect::<HashMap<_, _>>();
    let alias_param_types = contract
        .adapters
        .iter()
        .map(|adapter| {
            (
                adapter.alias.clone(),
                adapter
                    .params
                    .iter()
                    .map(|param| parse_contract_type(param))
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    let mut diagnostics = Vec::new();
    for item in &module.items {
        validate_item_adapter_calls(item, &alias_arity, &alias_param_types, &mut diagnostics);
    }
    diagnostics
}

fn validate_item_adapter_calls(
    item: &Item,
    alias_arity: &HashMap<String, usize>,
    alias_param_types: &HashMap<String, Vec<ContractType>>,
    diagnostics: &mut Vec<crate::diag::Diagnostic>,
) {
    match item {
        Item::Function(def) => {
            validate_block_adapter_calls(&def.body, alias_arity, alias_param_types, diagnostics)
        }
        Item::Impl(def) => {
            for method in &def.methods {
                validate_block_adapter_calls(
                    &method.body,
                    alias_arity,
                    alias_param_types,
                    diagnostics,
                );
            }
        }
        Item::Const(def) => {
            validate_expr_adapter_calls(&def.value, alias_arity, alias_param_types, diagnostics)
        }
        Item::Static(def) => {
            validate_expr_adapter_calls(&def.value, alias_arity, alias_param_types, diagnostics)
        }
        Item::Struct(_)
        | Item::Enum(_)
        | Item::Trait(_)
        | Item::RustUse(_)
        | Item::RustBlock(_) => {}
    }
}

fn validate_block_adapter_calls(
    block: &Block,
    alias_arity: &HashMap<String, usize>,
    alias_param_types: &HashMap<String, Vec<ContractType>>,
    diagnostics: &mut Vec<crate::diag::Diagnostic>,
) {
    for stmt in &block.statements {
        validate_stmt_adapter_calls(stmt, alias_arity, alias_param_types, diagnostics);
    }
}

fn validate_stmt_adapter_calls(
    stmt: &Stmt,
    alias_arity: &HashMap<String, usize>,
    alias_param_types: &HashMap<String, Vec<ContractType>>,
    diagnostics: &mut Vec<crate::diag::Diagnostic>,
) {
    match stmt {
        Stmt::Const(def) => {
            validate_expr_adapter_calls(&def.value, alias_arity, alias_param_types, diagnostics)
        }
        Stmt::DestructureConst { value, .. } => {
            validate_expr_adapter_calls(value, alias_arity, alias_param_types, diagnostics)
        }
        Stmt::Assign { target, value, .. } => {
            validate_assign_target_adapter_calls(
                target,
                alias_arity,
                alias_param_types,
                diagnostics,
            );
            validate_expr_adapter_calls(value, alias_arity, alias_param_types, diagnostics);
        }
        Stmt::Return(Some(expr)) => {
            validate_expr_adapter_calls(expr, alias_arity, alias_param_types, diagnostics)
        }
        Stmt::Return(None) => {}
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            validate_expr_adapter_calls(condition, alias_arity, alias_param_types, diagnostics);
            validate_block_adapter_calls(then_block, alias_arity, alias_param_types, diagnostics);
            if let Some(else_block) = else_block {
                validate_block_adapter_calls(
                    else_block,
                    alias_arity,
                    alias_param_types,
                    diagnostics,
                );
            }
        }
        Stmt::While { condition, body } => {
            validate_expr_adapter_calls(condition, alias_arity, alias_param_types, diagnostics);
            validate_block_adapter_calls(body, alias_arity, alias_param_types, diagnostics);
        }
        Stmt::For { iter, body, .. } => {
            validate_expr_adapter_calls(iter, alias_arity, alias_param_types, diagnostics);
            validate_block_adapter_calls(body, alias_arity, alias_param_types, diagnostics);
        }
        Stmt::Loop { body } => {
            validate_block_adapter_calls(body, alias_arity, alias_param_types, diagnostics)
        }
        Stmt::Break | Stmt::Continue | Stmt::RustBlock(_) => {}
        Stmt::Expr(expr) | Stmt::TailExpr(expr) => {
            validate_expr_adapter_calls(expr, alias_arity, alias_param_types, diagnostics)
        }
    }
}

fn validate_expr_adapter_calls(
    expr: &Expr,
    alias_arity: &HashMap<String, usize>,
    alias_param_types: &HashMap<String, Vec<ContractType>>,
    diagnostics: &mut Vec<crate::diag::Diagnostic>,
) {
    match expr {
        Expr::Call { callee, args } => {
            if let Expr::Path(path) = callee.as_ref() {
                let alias = path.join("::");
                if let Some(expected) = alias_arity.get(&alias)
                    && args.len() != *expected
                {
                    diagnostics.push(crate::diag::Diagnostic::new(
                        format!(
                            "interop adapter `{alias}` expects {expected} args, got {}",
                            args.len()
                        ),
                        crate::diag::Span::new(0, 0),
                    ));
                }
                if let Some(expected_types) = alias_param_types.get(&alias) {
                    for (index, (arg, expected_ty)) in args.iter().zip(expected_types).enumerate() {
                        let actual_ty = infer_contract_expr_type(arg);
                        if !contract_types_compatible(actual_ty, *expected_ty) {
                            diagnostics.push(crate::diag::Diagnostic::new(
                                format!(
                                    "interop adapter `{alias}` arg {} expected `{}`, got `{}`",
                                    index + 1,
                                    format_contract_type(*expected_ty),
                                    format_contract_type(actual_ty)
                                ),
                                crate::diag::Span::new(0, 0),
                            ));
                        }
                    }
                }
            }
            validate_expr_adapter_calls(callee, alias_arity, alias_param_types, diagnostics);
            for arg in args {
                validate_expr_adapter_calls(arg, alias_arity, alias_param_types, diagnostics);
            }
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                validate_expr_adapter_calls(arg, alias_arity, alias_param_types, diagnostics);
            }
        }
        Expr::Field { base, .. } => {
            validate_expr_adapter_calls(base, alias_arity, alias_param_types, diagnostics)
        }
        Expr::Index { base, index } => {
            validate_expr_adapter_calls(base, alias_arity, alias_param_types, diagnostics);
            validate_expr_adapter_calls(index, alias_arity, alias_param_types, diagnostics);
        }
        Expr::Match { scrutinee, arms } => {
            validate_expr_adapter_calls(scrutinee, alias_arity, alias_param_types, diagnostics);
            for arm in arms {
                validate_expr_adapter_calls(
                    &arm.value,
                    alias_arity,
                    alias_param_types,
                    diagnostics,
                );
            }
        }
        Expr::Unary { expr, .. } | Expr::Try(expr) => {
            validate_expr_adapter_calls(expr, alias_arity, alias_param_types, diagnostics)
        }
        Expr::Cast { expr, .. } => {
            validate_expr_adapter_calls(expr, alias_arity, alias_param_types, diagnostics)
        }
        Expr::Binary { left, right, .. } => {
            validate_expr_adapter_calls(left, alias_arity, alias_param_types, diagnostics);
            validate_expr_adapter_calls(right, alias_arity, alias_param_types, diagnostics);
        }
        Expr::Array(items) | Expr::Tuple(items) => {
            for item in items {
                validate_expr_adapter_calls(item, alias_arity, alias_param_types, diagnostics);
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for StructLiteralField { value, .. } in fields {
                validate_expr_adapter_calls(value, alias_arity, alias_param_types, diagnostics);
            }
        }
        Expr::Block(body) => {
            validate_block_adapter_calls(body, alias_arity, alias_param_types, diagnostics)
        }
        Expr::Closure { body, .. } => {
            validate_block_adapter_calls(body, alias_arity, alias_param_types, diagnostics)
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                validate_expr_adapter_calls(start, alias_arity, alias_param_types, diagnostics);
            }
            if let Some(end) = end {
                validate_expr_adapter_calls(end, alias_arity, alias_param_types, diagnostics);
            }
        }
        Expr::Path(_) | Expr::Int(_) | Expr::Bool(_) | Expr::Char(_) | Expr::String(_) => {}
    }
}

fn validate_assign_target_adapter_calls(
    target: &crate::ast::AssignTarget,
    alias_arity: &HashMap<String, usize>,
    alias_param_types: &HashMap<String, Vec<ContractType>>,
    diagnostics: &mut Vec<crate::diag::Diagnostic>,
) {
    match target {
        crate::ast::AssignTarget::Path(_) => {}
        crate::ast::AssignTarget::Field { base, .. } => {
            validate_expr_adapter_calls(base, alias_arity, alias_param_types, diagnostics)
        }
        crate::ast::AssignTarget::Index { base, index } => {
            validate_expr_adapter_calls(base, alias_arity, alias_param_types, diagnostics);
            validate_expr_adapter_calls(index, alias_arity, alias_param_types, diagnostics);
        }
        crate::ast::AssignTarget::Tuple(items) => {
            for item in items {
                validate_assign_target_adapter_calls(
                    item,
                    alias_arity,
                    alias_param_types,
                    diagnostics,
                );
            }
        }
    }
}

fn parse_contract_type(raw: &str) -> ContractType {
    let normalized = raw.trim().replace(' ', "");
    match normalized.as_str() {
        "i64" => ContractType::I64,
        "bool" => ContractType::Bool,
        "String" => ContractType::String,
        "&str" => ContractType::StrRef,
        _ => ContractType::Unknown,
    }
}

fn infer_contract_expr_type(expr: &Expr) -> ContractType {
    match expr {
        Expr::Int(_) => ContractType::I64,
        Expr::Bool(_) => ContractType::Bool,
        Expr::Char(_) => ContractType::Unknown,
        Expr::String(_) => ContractType::String,
        _ => ContractType::Unknown,
    }
}

fn contract_types_compatible(actual: ContractType, expected: ContractType) -> bool {
    match (actual, expected) {
        (_, ContractType::Unknown) | (ContractType::Unknown, _) => true,
        (ContractType::String, ContractType::StrRef) => true,
        _ => actual == expected,
    }
}

fn format_contract_type(ty: ContractType) -> &'static str {
    match ty {
        ContractType::I64 => "i64",
        ContractType::Bool => "bool",
        ContractType::String => "String",
        ContractType::StrRef => "&str",
        ContractType::Unknown => "_",
    }
}

fn rewrite_module_adapter_calls(module: &mut Module, contract: &InteropContract) {
    let adapter_map = contract
        .adapters
        .iter()
        .map(|adapter| {
            (
                adapter.alias.clone(),
                vec![
                    "elevate_interop".to_string(),
                    adapter_fn_name(&adapter.alias),
                ],
            )
        })
        .collect::<HashMap<_, _>>();

    for item in &mut module.items {
        rewrite_item_adapter_calls(item, &adapter_map);
    }
}

fn rewrite_item_adapter_calls(item: &mut Item, adapter_map: &HashMap<String, Vec<String>>) {
    match item {
        Item::Function(def) => rewrite_block_adapter_calls(&mut def.body, adapter_map),
        Item::Impl(def) => {
            for method in &mut def.methods {
                rewrite_block_adapter_calls(&mut method.body, adapter_map);
            }
        }
        Item::Const(def) => rewrite_expr_adapter_calls(&mut def.value, adapter_map),
        Item::Static(def) => rewrite_expr_adapter_calls(&mut def.value, adapter_map),
        Item::Struct(_)
        | Item::Enum(_)
        | Item::Trait(_)
        | Item::RustUse(_)
        | Item::RustBlock(_) => {}
    }
}

fn rewrite_block_adapter_calls(block: &mut Block, adapter_map: &HashMap<String, Vec<String>>) {
    for stmt in &mut block.statements {
        rewrite_stmt_adapter_calls(stmt, adapter_map);
    }
}

fn rewrite_stmt_adapter_calls(stmt: &mut Stmt, adapter_map: &HashMap<String, Vec<String>>) {
    match stmt {
        Stmt::Const(def) => rewrite_expr_adapter_calls(&mut def.value, adapter_map),
        Stmt::DestructureConst { value, .. } => rewrite_expr_adapter_calls(value, adapter_map),
        Stmt::Assign { target, value, .. } => {
            rewrite_assign_target_adapter_calls(target, adapter_map);
            rewrite_expr_adapter_calls(value, adapter_map);
        }
        Stmt::Return(Some(expr)) => rewrite_expr_adapter_calls(expr, adapter_map),
        Stmt::Return(None) => {}
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            rewrite_expr_adapter_calls(condition, adapter_map);
            rewrite_block_adapter_calls(then_block, adapter_map);
            if let Some(else_block) = else_block {
                rewrite_block_adapter_calls(else_block, adapter_map);
            }
        }
        Stmt::While { condition, body } => {
            rewrite_expr_adapter_calls(condition, adapter_map);
            rewrite_block_adapter_calls(body, adapter_map);
        }
        Stmt::For { iter, body, .. } => {
            rewrite_expr_adapter_calls(iter, adapter_map);
            rewrite_block_adapter_calls(body, adapter_map);
        }
        Stmt::Loop { body } => {
            rewrite_block_adapter_calls(body, adapter_map);
        }
        Stmt::Break | Stmt::Continue | Stmt::RustBlock(_) => {}
        Stmt::Expr(expr) | Stmt::TailExpr(expr) => rewrite_expr_adapter_calls(expr, adapter_map),
    }
}

fn rewrite_expr_adapter_calls(expr: &mut Expr, adapter_map: &HashMap<String, Vec<String>>) {
    match expr {
        Expr::Path(path) => {
            let alias = path.join("::");
            if let Some(rewrite) = adapter_map.get(&alias) {
                *path = rewrite.clone();
            }
        }
        Expr::Call { callee, args } => {
            rewrite_expr_adapter_calls(callee, adapter_map);
            for arg in args {
                rewrite_expr_adapter_calls(arg, adapter_map);
            }
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                rewrite_expr_adapter_calls(arg, adapter_map);
            }
        }
        Expr::Field { base, .. } => rewrite_expr_adapter_calls(base, adapter_map),
        Expr::Index { base, index } => {
            rewrite_expr_adapter_calls(base, adapter_map);
            rewrite_expr_adapter_calls(index, adapter_map);
        }
        Expr::Match { scrutinee, arms } => {
            rewrite_expr_adapter_calls(scrutinee, adapter_map);
            for arm in arms {
                rewrite_expr_adapter_calls(&mut arm.value, adapter_map);
            }
        }
        Expr::Unary { expr, .. } | Expr::Try(expr) => rewrite_expr_adapter_calls(expr, adapter_map),
        Expr::Cast { expr, .. } => rewrite_expr_adapter_calls(expr, adapter_map),
        Expr::Binary { left, right, .. } => {
            rewrite_expr_adapter_calls(left, adapter_map);
            rewrite_expr_adapter_calls(right, adapter_map);
        }
        Expr::Array(items) | Expr::Tuple(items) => {
            for item in items {
                rewrite_expr_adapter_calls(item, adapter_map);
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for StructLiteralField { value, .. } in fields {
                rewrite_expr_adapter_calls(value, adapter_map);
            }
        }
        Expr::Block(body) => rewrite_block_adapter_calls(body, adapter_map),
        Expr::Closure { body, .. } => rewrite_block_adapter_calls(body, adapter_map),
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                rewrite_expr_adapter_calls(start, adapter_map);
            }
            if let Some(end) = end {
                rewrite_expr_adapter_calls(end, adapter_map);
            }
        }
        Expr::Int(_) | Expr::Bool(_) | Expr::Char(_) | Expr::String(_) => {}
    }
}

fn rewrite_assign_target_adapter_calls(
    target: &mut crate::ast::AssignTarget,
    adapter_map: &HashMap<String, Vec<String>>,
) {
    match target {
        crate::ast::AssignTarget::Path(_) => {}
        crate::ast::AssignTarget::Field { base, .. } => {
            rewrite_expr_adapter_calls(base, adapter_map)
        }
        crate::ast::AssignTarget::Index { base, index } => {
            rewrite_expr_adapter_calls(base, adapter_map);
            rewrite_expr_adapter_calls(index, adapter_map);
        }
        crate::ast::AssignTarget::Tuple(items) => {
            for item in items {
                rewrite_assign_target_adapter_calls(item, adapter_map);
            }
        }
    }
}

fn canonicalize(path: &Path) -> Result<PathBuf, String> {
    fs::canonicalize(path).map_err(|error| format!("failed to access {}: {error}", path.display()))
}

const INTEROP_CONTRACT_FILE: &str = "elevate.interop";

fn load_interop_contract(source_root: &Path) -> Result<InteropContract, String> {
    let path = source_root.join(INTEROP_CONTRACT_FILE);
    if !path.is_file() {
        return Ok(InteropContract::default());
    }

    let content = fs::read_to_string(&path).map_err(|error| {
        format!(
            "failed to read interop contract {}: {error}",
            path.display()
        )
    })?;
    parse_interop_contract(&content, &path)
}

fn parse_interop_contract(content: &str, path: &Path) -> Result<InteropContract, String> {
    let mut contract = InteropContract::default();
    let mut seen_aliases = BTreeSet::new();
    let lines = content.lines().collect::<Vec<_>>();
    let mut cursor = 0usize;
    while cursor < lines.len() {
        let raw_line = lines[cursor];
        let line_no = cursor + 1;
        let line = raw_line.trim();
        cursor += 1;
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line == "runtime {" {
            let mut closed = false;
            while cursor < lines.len() {
                let body_raw = lines[cursor];
                let body_no = cursor + 1;
                let body_line = body_raw.trim();
                cursor += 1;
                if body_line == "runtime }" {
                    closed = true;
                    break;
                }
                if body_line.starts_with("runtime ") {
                    return Err(format!(
                        "nested runtime directive is not allowed inside runtime block at {}:{}",
                        path.display(),
                        body_no
                    ));
                }
                contract.runtime_lines.push(body_raw.trim_end().to_string());
            }
            if !closed {
                return Err(format!(
                    "unterminated runtime block at {}:{}: expected closing `runtime }}`",
                    path.display(),
                    line_no
                ));
            }
            continue;
        }
        if let Some(rest) = line.strip_prefix("allow ") {
            let allowed = rest.trim();
            if !is_valid_allow_pattern(allowed) {
                return Err(format!(
                    "invalid interop contract path at {}:{}: `{}`",
                    path.display(),
                    line_no,
                    allowed
                ));
            }
            contract.allowed_paths.insert(allowed.to_string());
            continue;
        }
        if let Some(rest) = line.strip_prefix("adapter ") {
            let adapter = parse_adapter_contract_line(rest, path, line_no)?;
            if !seen_aliases.insert(adapter.alias.clone()) {
                return Err(format!(
                    "duplicate adapter alias `{}` at {}:{}",
                    adapter.alias,
                    path.display(),
                    line_no
                ));
            }
            contract.adapters.push(adapter);
            continue;
        }
        if let Some(rest) = line.strip_prefix("runtime ") {
            let runtime_line = rest.trim();
            if runtime_line.is_empty() {
                return Err(format!(
                    "invalid runtime directive at {}:{}: missing Rust item",
                    path.display(),
                    line_no
                ));
            }
            contract.runtime_lines.push(runtime_line.to_string());
            continue;
        }
        if let Some(rest) = line.strip_prefix("runtime_preset ") {
            let preset = rest.trim();
            if preset.is_empty() {
                return Err(format!(
                    "invalid runtime_preset directive at {}:{}: missing preset name",
                    path.display(),
                    line_no
                ));
            }
            if !matches!(preset, "parser_state") {
                return Err(format!(
                    "unknown runtime preset `{}` at {}:{}",
                    preset,
                    path.display(),
                    line_no
                ));
            }
            contract.runtime_presets.insert(preset.to_string());
            continue;
        }
        return Err(format!(
            "unknown interop contract directive at {}:{}: `{}`",
            path.display(),
            line_no,
            line
        ));
    }
    contract.adapters.sort_by(|a, b| a.alias.cmp(&b.alias));
    Ok(contract)
}

fn parse_adapter_contract_line(
    line: &str,
    path: &Path,
    line_no: usize,
) -> Result<InteropAdapter, String> {
    let (signature, transform) = if let Some((lhs, rhs)) = line.rsplit_once(" using ") {
        let transform = match rhs.trim() {
            "direct" => InteropTransform::Direct,
            "unwrap_to_owned_string" => InteropTransform::UnwrapToOwnedString,
            "unwrap_split_once_to_owned_tuple" => InteropTransform::UnwrapSplitOnceToOwnedTuple,
            other => {
                return Err(format!(
                    "unknown interop adapter transform `{}` at {}:{}",
                    other,
                    path.display(),
                    line_no
                ));
            }
        };
        (lhs.trim(), transform)
    } else {
        (line.trim(), InteropTransform::Direct)
    };

    let (alias, rhs) = signature.split_once("=>").ok_or_else(|| {
        format!(
            "invalid adapter syntax at {}:{}: expected `adapter <alias> => <target> (<params>) -> <ret>`",
            path.display(),
            line_no
        )
    })?;
    let alias = alias.trim();
    if !is_valid_rust_path(alias) {
        return Err(format!(
            "invalid adapter alias path at {}:{}: `{}`",
            path.display(),
            line_no,
            alias
        ));
    }

    let (target_and_params, return_type) = rhs.split_once("->").ok_or_else(|| {
        format!(
            "invalid adapter signature at {}:{}: missing `->` return type",
            path.display(),
            line_no
        )
    })?;
    let return_type = return_type.trim();
    if return_type.is_empty() {
        return Err(format!(
            "invalid adapter return type at {}:{}: return type cannot be empty",
            path.display(),
            line_no
        ));
    }

    let target_and_params = target_and_params.trim();
    let open = target_and_params.find('(').ok_or_else(|| {
        format!(
            "invalid adapter signature at {}:{}: expected parameter list `( ... )`",
            path.display(),
            line_no
        )
    })?;
    if !target_and_params.ends_with(')') {
        return Err(format!(
            "invalid adapter signature at {}:{}: parameter list must end with `)`",
            path.display(),
            line_no
        ));
    }
    let target = target_and_params[..open].trim();
    if !is_valid_rust_path(target) {
        return Err(format!(
            "invalid adapter target path at {}:{}: `{}`",
            path.display(),
            line_no,
            target
        ));
    }
    let params_body = &target_and_params[open + 1..target_and_params.len() - 1];
    let params = split_signature_list(params_body)
        .into_iter()
        .map(|part| part.to_string())
        .collect();

    Ok(InteropAdapter {
        alias: alias.to_string(),
        target: target.to_string(),
        params,
        return_type: return_type.to_string(),
        transform,
    })
}

fn split_signature_list(input: &str) -> Vec<&str> {
    let input = input.trim();
    if input.is_empty() {
        return Vec::new();
    }
    let mut items = Vec::new();
    let mut start = 0usize;
    let mut depth_angle = 0usize;
    let mut depth_paren = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '<' => depth_angle += 1,
            '>' => depth_angle = depth_angle.saturating_sub(1),
            '(' => depth_paren += 1,
            ')' => depth_paren = depth_paren.saturating_sub(1),
            ',' if depth_angle == 0 && depth_paren == 0 => {
                let item = input[start..idx].trim();
                if !item.is_empty() {
                    items.push(item);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    let tail = input[start..].trim();
    if !tail.is_empty() {
        items.push(tail);
    }
    items
}

fn validate_interop_contract_for_source(
    source: &str,
    source_path: &Path,
    contract: &InteropContract,
) -> Result<(), String> {
    if contract.allowed_paths.is_empty() {
        return Ok(());
    }

    let mut violations = Vec::new();
    for (line_idx, line) in source.lines().enumerate() {
        let Some((path, col)) = parse_use_import_line(line) else {
            continue;
        };
        if !path_allowed_by_contract(&path, &contract.allowed_paths) {
            violations.push((line_idx + 1, col, path));
        }
    }

    if violations.is_empty() {
        return Ok(());
    }

    let mut message = format!(
        "interop contract violation in {} ({}):",
        source_path.display(),
        INTEROP_CONTRACT_FILE
    );
    for (line, col, path) in violations {
        let _ = write!(
            &mut message,
            "\n  - `use {}` is not allowed (line {}, col {}). add `allow {}` to {}",
            path, line, col, path, INTEROP_CONTRACT_FILE
        );
    }
    Err(message)
}

fn parse_use_import_line(line: &str) -> Option<(String, usize)> {
    let indent = line.len() - line.trim_start().len();
    let trimmed = line.trim_start();
    let (rest, prefix_len) = (trimmed.strip_prefix("use ")?, "use ".len());
    let path = rest.strip_suffix(';')?.trim();
    if !is_valid_rust_path(path) {
        return None;
    }
    Some((path.to_string(), indent + prefix_len + 1))
}

fn path_allowed_by_contract(path: &str, allowed: &BTreeSet<String>) -> bool {
    if allowed.contains(path) {
        return true;
    }
    for pattern in allowed {
        if let Some(prefix) = pattern.strip_suffix("::*")
            && (path == prefix || path.starts_with(&format!("{prefix}::")))
        {
            return true;
        }
    }
    false
}

fn is_valid_rust_path(path: &str) -> bool {
    if path.is_empty() {
        return false;
    }
    for segment in path.split("::") {
        if segment.is_empty() {
            return false;
        }
        let mut chars = segment.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if !(first == '_' || first.is_ascii_alphabetic()) {
            return false;
        }
        if chars.any(|ch| !(ch == '_' || ch.is_ascii_alphanumeric())) {
            return false;
        }
    }
    true
}

fn is_valid_allow_pattern(pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix("::*") {
        return !prefix.is_empty() && is_valid_rust_path(prefix);
    }
    is_valid_rust_path(pattern)
}

fn emit_interop_adapter_module(
    generated_src: &Path,
    contract: &InteropContract,
) -> Result<(), String> {
    if contract.adapters.is_empty()
        && contract.runtime_lines.is_empty()
        && contract.runtime_presets.is_empty()
    {
        return Ok(());
    }

    let module_path = generated_src.join("elevate_interop.rs");
    let mut out = String::new();
    out.push_str("// Generated by elevate interop contract.\n");
    out.push_str("// Do not edit generated output manually.\n");
    out.push_str("#![allow(dead_code)]\n\n");
    for line in &contract.runtime_lines {
        out.push_str(line);
        out.push('\n');
    }
    for preset in &contract.runtime_presets {
        out.push_str(runtime_preset_source(preset));
        if !out.ends_with('\n') {
            out.push('\n');
        }
    }
    if !contract.runtime_lines.is_empty() || !contract.runtime_presets.is_empty() {
        out.push('\n');
    }
    for adapter in &contract.adapters {
        emit_adapter_fn(adapter, &mut out);
        out.push('\n');
    }
    fs::write(&module_path, out.as_bytes()).map_err(|error| {
        format!(
            "failed to write generated interop adapter module {}: {error}",
            module_path.display()
        )
    })?;

    inject_interop_module_decl(generated_src)?;
    Ok(())
}

fn emit_adapter_fn(adapter: &InteropAdapter, out: &mut String) {
    let fn_name = adapter_fn_name(&adapter.alias);
    let params = adapter
        .params
        .iter()
        .enumerate()
        .map(|(index, ty)| format!("__arg{index}: {}", emitted_adapter_param_type(ty)))
        .collect::<Vec<_>>()
        .join(", ");
    let args = adapter
        .params
        .iter()
        .enumerate()
        .map(|(index, ty)| emitted_adapter_call_arg(index, ty))
        .collect::<Vec<_>>()
        .join(", ");
    let _ = writeln!(out, "// adapter {} => {}", adapter.alias, adapter.target);
    let _ = writeln!(
        out,
        "pub fn {fn_name}({params}) -> {} {{",
        adapter.return_type
    );
    match adapter.transform {
        InteropTransform::Direct => {
            let _ = writeln!(out, "    return {}({args});", adapter.target);
        }
        InteropTransform::UnwrapToOwnedString => {
            let _ = writeln!(
                out,
                "    return {}({args}).expect(\"interop adapter unwrap failed\").to_string();",
                adapter.target
            );
        }
        InteropTransform::UnwrapSplitOnceToOwnedTuple => {
            let _ = writeln!(
                out,
                "    let (__left, __right) = {}({args}).expect(\"interop adapter unwrap failed\");",
                adapter.target
            );
            let _ = writeln!(out, "    return (__left.to_string(), __right.to_string());");
        }
    }
    out.push_str("}\n");
}

fn emitted_adapter_param_type(param: &str) -> String {
    match param.trim() {
        "&str" => "String".to_string(),
        other => other.to_string(),
    }
}

fn emitted_adapter_call_arg(index: usize, param: &str) -> String {
    match param.trim() {
        "&str" => format!("&__arg{index}"),
        _ => format!("__arg{index}"),
    }
}

fn adapter_fn_name(alias: &str) -> String {
    let mut out = "__elevate_adapter_".to_string();
    for ch in alias.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    out
}

fn inject_interop_module_decl(generated_src: &Path) -> Result<(), String> {
    for entry in ["lib.rs", "main.rs"] {
        let file = generated_src.join(entry);
        if !file.is_file() {
            continue;
        }
        let content = fs::read_to_string(&file)
            .map_err(|error| format!("failed to read {}: {error}", file.display()))?;
        if content.contains("mod elevate_interop;") {
            continue;
        }
        let rewritten = format!("mod elevate_interop;\n\n{content}");
        fs::write(&file, rewritten.as_bytes())
            .map_err(|error| format!("failed to rewrite {}: {error}", file.display()))?;
    }
    Ok(())
}

fn runtime_preset_source(name: &str) -> &'static str {
    match name {
        "parser_state" => include_str!("templates/runtime_preset_parser_state.rs"),
        _ => "",
    }
}

fn format_compile_error_with_context(
    path: &Path,
    source: &str,
    error: &crate::CompileError,
) -> String {
    let mut out = format!("failed to compile {}:", path.display());
    for diagnostic in &error.diagnostics {
        let (line, col) = crate::source_map::byte_to_line_col_clamped(source, diagnostic.span.start);
        out.push_str(&format!(
            "\n  - {} (line {}, col {}, bytes {}..{})",
            diagnostic.message, line, col, diagnostic.span.start, diagnostic.span.end
        ));
        if let Some(name) = extract_function_name(&diagnostic.message)
            && let Some((decl_line, decl_col)) = find_function_decl(source, &name)
        {
            out.push_str(&format!(
                "\n    hint: `{}` is declared at line {}, col {}",
                name, decl_line, decl_col
            ));
        }
    }
    out
}

fn extract_function_name(message: &str) -> Option<String> {
    extract_quoted_symbol(message, "Function").or_else(|| extract_quoted_symbol(message, "Method"))
}

fn extract_quoted_symbol(message: &str, label: &str) -> Option<String> {
    let marker = format!("{label} `");
    let rest = message.strip_prefix(&marker)?;
    let end = rest.find('`')?;
    Some(rest[..end].to_string())
}

fn find_function_decl(source: &str, name: &str) -> Option<(usize, usize)> {
    let needle = format!("fn {name}(");
    for (idx, line) in source.lines().enumerate() {
        if let Some(col) = line.find(&needle) {
            return Some((idx + 1, col + 1));
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{
        infer_borrow_indexes_from_suggestion, infer_clone_places_from_suggestion,
        merge_direct_borrow_hints, merge_forced_clone_places, parse_diagnostic_location,
        transpile_ers_crate,
    };
    use crate::{CompileOptions, DirectBorrowHint};

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

    #[test]
    fn transpile_auto_injects_root_module_decls_for_ers_siblings() {
        let root = create_temp_dir("elevate-crate-auto-mods-root");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(root.join("src/lib.ers"), "pub fn ping() -> i64 { 1 }\n")
            .expect("write lib.ers should succeed");
        fs::write(root.join("src/app.ers"), "pub fn run() -> i64 { 7 }\n")
            .expect("write app.ers should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_lib = fs::read_to_string(summary.generated_root.join("src/lib.rs"))
            .expect("generated lib should exist");
        assert!(generated_lib.contains("pub mod app;"));
        assert!(summary.generated_root.join("src/app.rs").is_file());
    }

    #[test]
    fn transpile_auto_injects_nested_module_tree_for_ers_files() {
        let root = create_temp_dir("elevate-crate-auto-mods-nested");
        fs::create_dir_all(root.join("src/net")).expect("create nested src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(root.join("src/lib.ers"), "pub fn ping() -> i64 { 1 }\n")
            .expect("write lib.ers should succeed");
        fs::write(
            root.join("src/net/http.ers"),
            "pub fn code() -> i64 { 200 }\n",
        )
        .expect("write nested http.ers should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_lib = fs::read_to_string(summary.generated_root.join("src/lib.rs"))
            .expect("generated lib should exist");
        let generated_net_mod = fs::read_to_string(summary.generated_root.join("src/net/mod.rs"))
            .expect("generated net/mod.rs should exist");
        assert!(generated_lib.contains("pub mod net;"));
        assert!(generated_net_mod.contains("pub mod http;"));
        assert!(summary.generated_root.join("src/net/http.rs").is_file());
    }

    #[test]
    fn transpile_prefers_ers_source_over_rs_twin() {
        let root = create_temp_dir("elevate-crate-ers-precedence");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(root.join("src/lib.ers"), "pub fn source() -> i64 { 7 }\n")
            .expect("write lib.ers should succeed");
        fs::write(root.join("src/lib.rs"), "pub fn source() -> i64 { 999 }\n")
            .expect("write lib.rs twin should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated = fs::read_to_string(summary.generated_root.join("src/lib.rs"))
            .expect("generated lib should exist");
        assert!(generated.contains("pub fn source() -> i64"));
        assert!(generated.contains("7"));
        assert!(!generated.contains("999"));
    }

    #[test]
    fn transpile_rejects_use_not_allowed_by_interop_contract() {
        let root = create_temp_dir("elevate-interop-contract");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "use std::mem::drop;\nfn cleanup(v: String) {\n    std::mem::drop(v);\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(root.join("elevate.interop"), "allow std::collections::*\n")
            .expect("write interop contract should succeed");

        let error = transpile_ers_crate(&root).expect_err("expected contract validation failure");
        assert!(error.contains("interop contract violation"));
        assert!(error.contains("use std::mem::drop"));
        assert!(error.contains("line 1"));
        assert!(error.contains("allow std::mem::drop"));
    }

    #[test]
    fn transpile_generates_interop_adapter_module_from_contract() {
        let root = create_temp_dir("elevate-interop-adapter");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn identity(v: i64) -> i64 { v }\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "adapter str::len_alias => std::str::len (&str) -> usize\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let adapter_module = fs::read_to_string(generated_src.join("elevate_interop.rs"))
            .expect("adapter module should exist");
        assert!(adapter_module.contains("pub fn __elevate_adapter_str__len_alias"));
        assert!(adapter_module.contains("__arg0: String"));
        assert!(adapter_module.contains("std::str::len(&__arg0)"));

        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(generated_lib.contains("mod elevate_interop;"));
    }

    #[test]
    fn transpile_generates_runtime_support_lines_from_contract() {
        let root = create_temp_dir("elevate-interop-runtime");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn identity(v: i64) -> i64 { v }\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "runtime use std::sync::OnceLock;\nruntime static REGISTRY: OnceLock<i64> = OnceLock::new();\nruntime pub fn registry() -> &'static OnceLock<i64> { &REGISTRY }\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let interop_module = fs::read_to_string(generated_src.join("elevate_interop.rs"))
            .expect("interop module should exist");
        assert!(interop_module.contains("use std::sync::OnceLock;"));
        assert!(interop_module.contains("static REGISTRY: OnceLock<i64> = OnceLock::new();"));
        assert!(
            interop_module.contains("pub fn registry() -> &'static OnceLock<i64> { &REGISTRY }")
        );

        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(generated_lib.contains("mod elevate_interop;"));
    }

    #[test]
    fn transpile_generates_runtime_block_lines_from_contract() {
        let root = create_temp_dir("elevate-interop-runtime-block");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn identity(v: i64) -> i64 { v }\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "runtime {\nuse std::sync::OnceLock;\nstatic REG: OnceLock<i64> = OnceLock::new();\nruntime }\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let interop_module = fs::read_to_string(generated_src.join("elevate_interop.rs"))
            .expect("interop module should exist");
        assert!(interop_module.contains("use std::sync::OnceLock;"));
        assert!(interop_module.contains("static REG: OnceLock<i64> = OnceLock::new();"));
    }

    #[test]
    fn transpile_generates_runtime_preset_parser_state() {
        let root = create_temp_dir("elevate-interop-runtime-preset");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn identity(v: i64) -> i64 { v }\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "runtime_preset parser_state\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let interop_module = fs::read_to_string(generated_src.join("elevate_interop.rs"))
            .expect("interop module should exist");
        assert!(interop_module.contains("struct ParserState"));
        assert!(
            interop_module
                .contains("static REGISTRY: OnceLock<Mutex<Registry>> = OnceLock::new();")
        );
        assert!(interop_module.contains("pub fn take_next_arg(handle: i64) -> Option<String>"));
    }

    #[test]
    fn transpile_routes_alias_calls_to_generated_interop_adapters() {
        let root = create_temp_dir("elevate-interop-routing");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn pick(a: i64, b: i64) -> i64 {\n    const out: i64 = elevate::max_i64(a, b);\n    out\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "adapter elevate::max_i64 => std::cmp::max (i64, i64) -> i64\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(
            generated_lib.contains("elevate_interop::__elevate_adapter_elevate__max_i64(a, b)")
        );
    }

    #[test]
    fn transpile_routes_alias_paths_used_as_values() {
        let root = create_temp_dir("elevate-interop-routing-path-value");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn pick(a: i64, b: i64) -> i64 {\n    const pick_fn = elevate::max_i64;\n    const out: i64 = pick_fn(a, b);\n    out\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "adapter elevate::max_i64 => std::cmp::max (i64, i64) -> i64\n",
        )
        .expect("write interop contract should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(
            generated_lib
                .contains("let pick_fn = elevate_interop::__elevate_adapter_elevate__max_i64;")
        );
        assert!(generated_lib.contains("let out: i64 = pick_fn(a, b);"));
    }

    #[test]
    fn transpile_reports_adapter_call_arity_mismatch() {
        let root = create_temp_dir("elevate-interop-routing-arity");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn pick(a: i64, b: i64) -> i64 {\n    elevate::max_i64(a)\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "adapter elevate::max_i64 => std::cmp::max (i64, i64) -> i64\n",
        )
        .expect("write interop contract should succeed");

        let error = transpile_ers_crate(&root).expect_err("expected arity validation failure");
        assert!(error.contains("interop adapter `elevate::max_i64` expects 2 args, got 1"));
    }

    #[test]
    fn transpile_reports_adapter_call_type_mismatch_for_literals() {
        let root = create_temp_dir("elevate-interop-routing-types");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn run() -> bool {\n    elevate::is_enabled(1)\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(
            root.join("elevate.interop"),
            "adapter elevate::is_enabled => crate::is_enabled (bool) -> bool\n",
        )
        .expect("write interop contract should succeed");

        let error = transpile_ers_crate(&root).expect_err("expected type validation failure");
        assert!(
            error
                .contains("interop adapter `elevate::is_enabled` arg 1 expected `bool`, got `i64`")
        );
    }

    #[test]
    fn transpile_crate_with_tuple_assignment_compiles_with_cargo() {
        let root = create_temp_dir("elevate-tuple-assign-build");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "pub fn swap(a: i64, b: i64) -> i64 {\n    (a, b) = (b, a);\n    a\n}\n",
        )
        .expect("write lib.ers should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(generated_lib.contains("(a, b) = (b, a);"));

        let output = std::process::Command::new("rustc")
            .arg("--crate-type=lib")
            .arg(generated_src.join("lib.rs"))
            .arg("-o")
            .arg(summary.generated_root.join("libtuple_test.rlib"))
            .output()
            .expect("rustc should execute");
        assert!(
            output.status.success(),
            "rustc compile failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn transpile_crate_with_read_views_and_loop_ownership_compiles_with_rustc() {
        let root = create_temp_dir("elevate-read-view-loop-build");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            r#"
                pub struct Game {
                    title: String;
                    rounds: Vec<String>;
                }

                fn consume(value: String) {
                    std::mem::drop(value);
                    return;
                }

                pub fn score(game: Game, text: String) -> usize {
                    const snapshot = view(game);
                    for round in snapshot.rounds.iter() {
                        std::mem::drop(round);
                        std::mem::drop(snapshot.title.len());
                        consume(text);
                    }
                    return snapshot.rounds.len();
                }
            "#,
        )
        .expect("write lib.ers should succeed");

        let summary = transpile_ers_crate(&root).expect("transpile should succeed");
        let generated_src = summary.generated_root.join("src");
        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(generated_lib.contains("let snapshot: &Game = &game;"));
        assert!(generated_lib.contains("consume(text.clone());"));

        let output = std::process::Command::new("rustc")
            .arg("--crate-type=lib")
            .arg(generated_src.join("lib.rs"))
            .arg("-o")
            .arg(summary.generated_root.join("libreadview_test.rlib"))
            .output()
            .expect("rustc should execute");
        assert!(
            output.status.success(),
            "rustc compile failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn transpile_crate_reports_extended_generic_bound_violation() {
        let root = create_temp_dir("elevate-generic-bound-violation");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            r#"
                struct User { id: i64; }

                fn eq_user<T: PartialEq>(left: T, right: T) -> bool {
                    return left == right;
                }

                pub fn run(left: User, right: User) -> bool {
                    eq_user(left, right)
                }
            "#,
        )
        .expect("write lib.ers should succeed");

        let error = transpile_ers_crate(&root).expect_err("expected bound violation");
        assert!(error.contains("does not satisfy bound `PartialEq`"));
    }

    #[test]
    fn parse_location_supports_relative_generated_paths() {
        let generated_root = PathBuf::from("/tmp/generated");
        let line = " --> src/lib.rs:13:12";
        let (path, line_no) =
            parse_diagnostic_location(line, &generated_root).expect("location should parse");
        assert_eq!(path, generated_root.join("src/lib.rs"));
        assert_eq!(line_no, 13);
    }

    #[test]
    fn infer_borrow_indexes_detects_new_ampersands() {
        let original = "return Foreign::has_prefix(text, prefix);";
        let suggested = "return Foreign::has_prefix(&text, &prefix);";
        let (callee, indexes) = infer_borrow_indexes_from_suggestion(original, suggested)
            .expect("borrow suggestion should be inferred");
        assert_eq!(callee, "Foreign::has_prefix");
        assert_eq!(indexes, vec![0, 1]);
    }

    #[test]
    fn infer_borrow_indexes_detects_ampersands_on_cloned_args() {
        let original =
            "let drawn = host::runtime_draw_scene(board.cells.clone(), board.fixed.clone(), row);";
        let suggested = "let drawn = host::runtime_draw_scene(&board.cells.clone(), &board.fixed.clone(), row);";
        let (callee, indexes) = infer_borrow_indexes_from_suggestion(original, suggested)
            .expect("borrow suggestion should be inferred");
        assert_eq!(callee, "host::runtime_draw_scene");
        assert_eq!(indexes, vec![0, 1]);
    }

    #[test]
    fn merge_borrow_hints_unions_indexes_per_path() {
        let mut options = CompileOptions {
            direct_borrow_hints: vec![DirectBorrowHint {
                path: "Foreign::has_prefix".to_string(),
                borrowed_arg_indexes: vec![0],
            }],
            ..CompileOptions::default()
        };
        let added = merge_direct_borrow_hints(
            &mut options,
            vec![
                DirectBorrowHint {
                    path: "Foreign::has_prefix".to_string(),
                    borrowed_arg_indexes: vec![1],
                },
                DirectBorrowHint {
                    path: "Other::query".to_string(),
                    borrowed_arg_indexes: vec![0],
                },
            ],
        );
        assert_eq!(added, 2);
        assert!(
            options
                .direct_borrow_hints
                .iter()
                .any(|hint| hint.path == "Foreign::has_prefix"
                    && hint.borrowed_arg_indexes == vec![0, 1])
        );
        assert!(
            options
                .direct_borrow_hints
                .iter()
                .any(|hint| hint.path == "Other::query" && hint.borrowed_arg_indexes == vec![0])
        );
    }

    #[test]
    fn infer_clone_places_detects_added_clone_calls() {
        let original = "return state.text.into_bytes();";
        let suggested = "return state.text.clone().into_bytes();";
        let places = infer_clone_places_from_suggestion(original, suggested);
        assert_eq!(places, vec!["state.text".to_string()]);
    }

    #[test]
    fn merge_forced_clone_places_deduplicates() {
        let mut options = CompileOptions {
            forced_clone_places: vec!["state.text".to_string()],
            ..CompileOptions::default()
        };
        let added = merge_forced_clone_places(
            &mut options,
            vec!["state.text".to_string(), "packet".to_string()],
        );
        assert_eq!(added, 1);
        assert_eq!(
            options.forced_clone_places,
            vec!["packet".to_string(), "state.text".to_string()]
        );
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
