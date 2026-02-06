use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{collections::BTreeSet, fmt::Write as _};
use crate::ast::{Block, Expr, Item, Module, Stmt, StructLiteralField};

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

pub fn build_ers_crate(crate_root: &Path, release: bool) -> Result<BuildSummary, String> {
    let summary = transpile_ers_crate(crate_root)?;
    run_generated_cargo_build(&summary.source_root, &summary.generated_root, release)?;
    Ok(summary)
}

pub fn transpile_ers_crate(crate_root: &Path) -> Result<BuildSummary, String> {
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

    process_src_dir(
        &source_src,
        &source_src,
        &generated_root.join("src"),
        &mut summary,
        &interop_contract,
    )?;
    emit_interop_adapter_module(&generated_root.join("src"), &interop_contract)?;
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
    interop_contract: &InteropContract,
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
            process_src_dir(root_src, &path, generated_src, summary, interop_contract)?;
            continue;
        }

        if path.extension() == Some(OsStr::new("ers")) {
            let source = fs::read_to_string(&path)
                .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
            validate_interop_contract_for_source(&source, &path, interop_contract)?;
            let output = compile_source_with_interop_contract(&source, interop_contract).map_err(|error| {
                format_compile_error_with_context(&path, &source, &error)
            })?;
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

fn compile_source_with_interop_contract(
    source: &str,
    contract: &InteropContract,
) -> Result<crate::CompilerOutput, crate::CompileError> {
    if contract.adapters.is_empty() {
        return crate::compile_source(source);
    }

    let tokens = crate::lexer::lex(source).map_err(|diagnostics| crate::CompileError { diagnostics })?;
    let mut module = crate::parser::parse_module(tokens)
        .map_err(|diagnostics| crate::CompileError { diagnostics })?;
    rewrite_module_adapter_calls(&mut module, contract);
    crate::compile_ast(&module)
}

fn rewrite_module_adapter_calls(module: &mut Module, contract: &InteropContract) {
    let adapter_map = contract
        .adapters
        .iter()
        .map(|adapter| {
            (
                adapter.alias.clone(),
                vec!["__elevate_interop".to_string(), adapter_fn_name(&adapter.alias)],
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
        Item::Struct(_) | Item::Enum(_) | Item::RustUse(_) => {}
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
        Stmt::Expr(expr) | Stmt::TailExpr(expr) => rewrite_expr_adapter_calls(expr, adapter_map),
    }
}

fn rewrite_expr_adapter_calls(expr: &mut Expr, adapter_map: &HashMap<String, Vec<String>>) {
    match expr {
        Expr::Call { callee, args } => {
            rewrite_expr_adapter_calls(callee, adapter_map);
            for arg in args {
                rewrite_expr_adapter_calls(arg, adapter_map);
            }
            if let Expr::Path(path) = callee.as_mut() {
                let alias = path.join("::");
                if let Some(rewrite) = adapter_map.get(&alias) {
                    *path = rewrite.clone();
                }
            }
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                rewrite_expr_adapter_calls(arg, adapter_map);
            }
        }
        Expr::Field { base, .. } => rewrite_expr_adapter_calls(base, adapter_map),
        Expr::Match { scrutinee, arms } => {
            rewrite_expr_adapter_calls(scrutinee, adapter_map);
            for arm in arms {
                rewrite_expr_adapter_calls(&mut arm.value, adapter_map);
            }
        }
        Expr::Unary { expr, .. } | Expr::Try(expr) => rewrite_expr_adapter_calls(expr, adapter_map),
        Expr::Binary { left, right, .. } => {
            rewrite_expr_adapter_calls(left, adapter_map);
            rewrite_expr_adapter_calls(right, adapter_map);
        }
        Expr::Tuple(items) => {
            for item in items {
                rewrite_expr_adapter_calls(item, adapter_map);
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for StructLiteralField { value, .. } in fields {
                rewrite_expr_adapter_calls(value, adapter_map);
            }
        }
        Expr::Closure { body, .. } => rewrite_block_adapter_calls(body, adapter_map),
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                rewrite_expr_adapter_calls(start, adapter_map);
            }
            if let Some(end) = end {
                rewrite_expr_adapter_calls(end, adapter_map);
            }
        }
        Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Path(_) => {}
    }
}

fn rewrite_assign_target_adapter_calls(
    target: &mut crate::ast::AssignTarget,
    adapter_map: &HashMap<String, Vec<String>>,
) {
    if let crate::ast::AssignTarget::Field { base, .. } = target {
        rewrite_expr_adapter_calls(base, adapter_map);
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
    for (line_index, raw_line) in content.lines().enumerate() {
        let line_no = line_index + 1;
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
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
        let Some((path, col)) = parse_rust_use_line(line) else {
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
            "\n  - `rust use {}` is not allowed (line {}, col {}). add `allow {}` to {}",
            path,
            line,
            col,
            path,
            INTEROP_CONTRACT_FILE
        );
    }
    Err(message)
}

fn parse_rust_use_line(line: &str) -> Option<(String, usize)> {
    let indent = line.len() - line.trim_start().len();
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("rust use ")?;
    let path = rest.strip_suffix(';')?.trim();
    if !is_valid_rust_path(path) {
        return None;
    }
    Some((path.to_string(), indent + "rust use ".len() + 1))
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

fn emit_interop_adapter_module(generated_src: &Path, contract: &InteropContract) -> Result<(), String> {
    if contract.adapters.is_empty() {
        return Ok(());
    }

    let module_path = generated_src.join("__elevate_interop.rs");
    let mut out = String::new();
    out.push_str("// Generated by elevate interop contract.\n");
    out.push_str("// Do not edit generated output manually.\n");
    out.push_str("#![allow(dead_code)]\n\n");
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
        .map(|(index, ty)| format!("__arg{index}: {ty}"))
        .collect::<Vec<_>>()
        .join(", ");
    let args = (0..adapter.params.len())
        .map(|index| format!("__arg{index}"))
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
            let _ = writeln!(
                out,
                "    return (__left.to_string(), __right.to_string());"
            );
        }
    }
    out.push_str("}\n");
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
        let content =
            fs::read_to_string(&file).map_err(|error| format!("failed to read {}: {error}", file.display()))?;
        if content.contains("mod __elevate_interop;") {
            continue;
        }
        let rewritten = format!("mod __elevate_interop;\n\n{content}");
        fs::write(&file, rewritten.as_bytes())
            .map_err(|error| format!("failed to rewrite {}: {error}", file.display()))?;
    }
    Ok(())
}

fn format_compile_error_with_context(path: &Path, source: &str, error: &crate::CompileError) -> String {
    let mut out = format!("failed to compile {}:", path.display());
    for diagnostic in &error.diagnostics {
        let (line, col) = byte_to_line_col(source, diagnostic.span.start);
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

fn byte_to_line_col(source: &str, byte_offset: usize) -> (usize, usize) {
    let clamped = byte_offset.min(source.len());
    let mut line = 1usize;
    let mut line_start = 0usize;
    for (idx, ch) in source.char_indices() {
        if idx >= clamped {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = idx + 1;
        }
    }
    let col = source[line_start..clamped].chars().count() + 1;
    (line, col)
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

    #[test]
    fn transpile_rejects_rust_use_not_allowed_by_interop_contract() {
        let root = create_temp_dir("elevate-interop-contract");
        fs::create_dir_all(root.join("src")).expect("create src tree should succeed");
        fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("write manifest should succeed");
        fs::write(
            root.join("src/lib.ers"),
            "rust use std::mem::drop;\nfn cleanup(v: String) {\n    std::mem::drop(v);\n}\n",
        )
        .expect("write lib.ers should succeed");
        fs::write(root.join("elevate.interop"), "allow std::collections::*\n")
            .expect("write interop contract should succeed");

        let error = transpile_ers_crate(&root).expect_err("expected contract validation failure");
        assert!(error.contains("interop contract violation"));
        assert!(error.contains("rust use std::mem::drop"));
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
        let adapter_module = fs::read_to_string(generated_src.join("__elevate_interop.rs"))
            .expect("adapter module should exist");
        assert!(adapter_module.contains("pub fn __elevate_adapter_str__len_alias"));
        assert!(adapter_module.contains("std::str::len(__arg0)"));

        let generated_lib =
            fs::read_to_string(generated_src.join("lib.rs")).expect("generated lib should exist");
        assert!(generated_lib.contains("mod __elevate_interop;"));
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
            generated_lib.contains("__elevate_interop::__elevate_adapter_elevate__max_i64(a, b)")
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
