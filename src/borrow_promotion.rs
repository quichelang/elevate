//! Post-lowering borrow promotion pass.
//!
//! Runs on a fully-lowered `RustModule` and promotes non-mutated, non-consumed
//! parameters from `T` to `&T`.  This eliminates cloning at call sites that
//! only need to read a value.
//!
//! ## Pipeline position
//!
//! ```text
//! lower items → RustModule ─► apply_borrow_promotions ─► codegen
//! ```
//!
//! ## Design insight
//!
//! The body does NOT need rewriting after promotion.  If `param: &T`:
//! - `param.borrow()` → problematic (`.borrow()` on `&T` produces `&&T`)
//! - `param.clone()`  → produces `&T` (cheap ref copy), may mismatch callee
//!
//! We therefore **block** promotion for any param that has `.borrow()`,
//! `.clone()`, or `&param` in the lowered body.  Only params used purely via
//! field access, indexing, or method calls that auto-deref are eligible.
//!
//! Call sites of promoted functions get `BorrowCall` wrappers on their args.

use std::collections::HashMap;

use crate::codegen::collect_mutated_paths_in_stmts;
use crate::ir::lowered::{RustExpr, RustFunction, RustItem, RustModule, RustStmt};
use crate::passes::{is_copy_primitive_type, param_is_consumed_in_body, split_type_head_and_args};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Apply read-only borrow promotion across an entire lowered module.
pub fn apply_borrow_promotions(module: &mut RustModule) {
    let promotions = collect_all_promotions(module);
    if promotions.is_empty() {
        return;
    }

    // Promote signatures.
    for item in &mut module.items {
        match item {
            RustItem::Function(func) => {
                if let Some(indexes) = promotions.get(&func.name) {
                    promote_signature(func, indexes);
                }
            }
            RustItem::Impl(imp) => {
                for method in &mut imp.methods {
                    let key = format!("{}::{}", imp.target, method.name);
                    if let Some(indexes) = promotions.get(&key) {
                        promote_signature(method, indexes);
                    }
                }
            }
            _ => {}
        }
    }

    // Rewrite call sites in ALL function bodies.
    // Pass the current function's own promoted param names so we can
    // avoid double-wrapping self-recursive calls.
    for item in &mut module.items {
        match item {
            RustItem::Function(func) => {
                let own_promoted = own_promoted_names(func, &promotions);
                rewrite_callsites_in_body(&mut func.body, &promotions, &own_promoted);
            }
            RustItem::Impl(imp) => {
                for method in &mut imp.methods {
                    let key = format!("{}::{}", imp.target, method.name);
                    let own_promoted = own_promoted_names_by_key(method, &key, &promotions);
                    rewrite_callsites_in_body(&mut method.body, &promotions, &own_promoted);
                }
            }
            _ => {}
        }
    }
}

use std::collections::HashSet;

/// Collect param names promoted for a free function.
fn own_promoted_names(
    func: &RustFunction,
    promotions: &HashMap<String, Vec<usize>>,
) -> HashSet<String> {
    own_promoted_names_by_key(func, &func.name, promotions)
}

/// Collect param names promoted for a function/method by key.
fn own_promoted_names_by_key(
    func: &RustFunction,
    key: &str,
    promotions: &HashMap<String, Vec<usize>>,
) -> HashSet<String> {
    let mut names = HashSet::new();
    if let Some(indexes) = promotions.get(key) {
        for &idx in indexes {
            names.insert(func.params[idx].name.clone());
        }
    }
    names
}

// ---------------------------------------------------------------------------
// Step 1: Determine promotion candidates
// ---------------------------------------------------------------------------

fn collect_all_promotions(module: &RustModule) -> HashMap<String, Vec<usize>> {
    let mut promotions = HashMap::new();
    for item in &module.items {
        match item {
            RustItem::Function(func) => {
                let indexes = find_promotable_params(func, false);
                if !indexes.is_empty() {
                    promotions.insert(func.name.clone(), indexes);
                }
            }
            RustItem::Impl(imp) => {
                if imp.trait_target.is_some() {
                    continue;
                }
                for method in &imp.methods {
                    let indexes = find_promotable_params(method, true);
                    if !indexes.is_empty() {
                        let key = format!("{}::{}", imp.target, method.name);
                        promotions.insert(key, indexes);
                    }
                }
            }
            _ => {}
        }
    }
    promotions
}

fn find_promotable_params(func: &RustFunction, is_method: bool) -> Vec<usize> {
    if !func.type_params.is_empty() {
        return Vec::new();
    }

    let mutated = collect_mutated_paths_in_stmts(&func.body);

    func.params
        .iter()
        .enumerate()
        .filter_map(|(idx, param)| {
            if param.name == "self" {
                return None;
            }
            if param.ty.trim_start().starts_with('&') {
                return None;
            }
            let (head, _) = split_type_head_and_args(param.ty.trim());
            if is_copy_primitive_type(head) {
                return None;
            }
            if mutated.contains(&param.name) {
                return None;
            }
            if param_is_consumed_in_body(&param.name, &func.body) {
                return None;
            }
            // Block if the body has .clone(), .borrow(), BorrowCall, or &param
            // on this param — these patterns assume owned semantics.
            if param_has_owned_wrappers(&param.name, &func.body) {
                return None;
            }
            if is_method && idx == 0 {
                return None;
            }
            Some(idx)
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Guard: detect clone/borrow/& wrappers on a param
// ---------------------------------------------------------------------------

fn param_has_owned_wrappers(name: &str, stmts: &[RustStmt]) -> bool {
    stmts.iter().any(|s| stmt_has_wrapper(name, s))
}

fn stmt_has_wrapper(name: &str, stmt: &RustStmt) -> bool {
    match stmt {
        RustStmt::Const(c) => expr_has_wrapper(name, &c.value),
        RustStmt::DestructureConst { value, .. } => expr_has_wrapper(name, value),
        RustStmt::Assign { value, .. } => expr_has_wrapper(name, value),
        RustStmt::Return(Some(expr)) => expr_has_wrapper(name, expr),
        RustStmt::Return(None) | RustStmt::Break | RustStmt::Continue | RustStmt::Raw(_) => false,
        RustStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            expr_has_wrapper(name, condition)
                || param_has_owned_wrappers(name, then_body)
                || else_body
                    .as_ref()
                    .map_or(false, |eb| param_has_owned_wrappers(name, eb))
        }
        RustStmt::While { condition, body } => {
            expr_has_wrapper(name, condition) || param_has_owned_wrappers(name, body)
        }
        RustStmt::For { iter, body, .. } => {
            expr_has_wrapper(name, iter) || param_has_owned_wrappers(name, body)
        }
        RustStmt::Loop { body } => param_has_owned_wrappers(name, body),
        RustStmt::Expr(expr) => expr_has_wrapper(name, expr),
    }
}

/// Check if an expression contains `.clone()`, `.borrow()`, `BorrowCall(param)`,
/// or `&param` where the target is the named param.
fn expr_has_wrapper(name: &str, expr: &RustExpr) -> bool {
    match expr {
        // param.clone() or param.borrow()
        RustExpr::Call {
            callee,
            args,
            mutates_receiver: false,
        } if args.is_empty() => {
            if let RustExpr::Field { base, field } = callee.as_ref() {
                if (field == "clone" || field == "borrow")
                    && matches!(base.as_ref(), RustExpr::Path(p) if p.len() == 1 && p[0] == name)
                {
                    return true;
                }
            }
            expr_has_wrapper(name, callee) || args.iter().any(|a| expr_has_wrapper(name, a))
        }
        // BorrowCall(param)
        RustExpr::BorrowCall(inner) | RustExpr::BorrowMutCall(inner) => {
            if matches!(inner.as_ref(), RustExpr::Path(p) if p.len() == 1 && p[0] == name) {
                return true;
            }
            expr_has_wrapper(name, inner)
        }
        // &param
        RustExpr::Borrow(inner) => {
            if matches!(inner.as_ref(), RustExpr::Path(p) if p.len() == 1 && p[0] == name) {
                return true;
            }
            expr_has_wrapper(name, inner)
        }
        // Recurse into all other children
        RustExpr::Call { callee, args, .. } => {
            expr_has_wrapper(name, callee) || args.iter().any(|a| expr_has_wrapper(name, a))
        }
        RustExpr::Field { base, .. } => expr_has_wrapper(name, base),
        RustExpr::Index { base, index } => {
            expr_has_wrapper(name, base) || expr_has_wrapper(name, index)
        }
        RustExpr::Binary { left, right, .. } => {
            expr_has_wrapper(name, left) || expr_has_wrapper(name, right)
        }
        RustExpr::Unary { expr: inner, .. }
        | RustExpr::Cast { expr: inner, .. }
        | RustExpr::Try(inner)
        | RustExpr::MutBorrow(inner) => expr_has_wrapper(name, inner),
        RustExpr::Tuple(items) | RustExpr::Array(items) => {
            items.iter().any(|i| expr_has_wrapper(name, i))
        }
        RustExpr::Match { scrutinee, arms } => {
            expr_has_wrapper(name, scrutinee)
                || arms.iter().any(|arm| expr_has_wrapper(name, &arm.value))
        }
        RustExpr::Block { body, tail } => {
            param_has_owned_wrappers(name, body)
                || tail.as_ref().map_or(false, |t| expr_has_wrapper(name, t))
        }
        RustExpr::StructLiteral { fields, .. } => {
            fields.iter().any(|f| expr_has_wrapper(name, &f.value))
        }
        RustExpr::MacroCall { args, .. } => args.iter().any(|a| expr_has_wrapper(name, a)),
        RustExpr::Closure { body, .. } => param_has_owned_wrappers(name, body),
        RustExpr::Range { start, end, .. } => {
            start.as_ref().map_or(false, |s| expr_has_wrapper(name, s))
                || end.as_ref().map_or(false, |e| expr_has_wrapper(name, e))
        }
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Step 2: Promote signatures
// ---------------------------------------------------------------------------

fn promote_signature(func: &mut RustFunction, indexes: &[usize]) {
    for &idx in indexes {
        func.params[idx].ty = format!("&{}", func.params[idx].ty);
    }
}

// ---------------------------------------------------------------------------
// Step 3: Rewrite call sites
// ---------------------------------------------------------------------------

fn rewrite_callsites_in_body(
    body: &mut Vec<RustStmt>,
    promotions: &HashMap<String, Vec<usize>>,
    own_promoted: &HashSet<String>,
) {
    for stmt in body.iter_mut() {
        rewrite_callsites_in_stmt(stmt, promotions, own_promoted);
    }
}

fn rewrite_callsites_in_stmt(
    stmt: &mut RustStmt,
    promotions: &HashMap<String, Vec<usize>>,
    own_promoted: &HashSet<String>,
) {
    match stmt {
        RustStmt::Const(c) => rewrite_callsites_in_expr(&mut c.value, promotions, own_promoted),
        RustStmt::DestructureConst { value, .. } => {
            rewrite_callsites_in_expr(value, promotions, own_promoted)
        }
        RustStmt::Assign { value, .. } => {
            rewrite_callsites_in_expr(value, promotions, own_promoted)
        }
        RustStmt::Return(Some(expr)) => rewrite_callsites_in_expr(expr, promotions, own_promoted),
        RustStmt::Return(None) | RustStmt::Break | RustStmt::Continue | RustStmt::Raw(_) => {}
        RustStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            rewrite_callsites_in_expr(condition, promotions, own_promoted);
            rewrite_callsites_in_body(then_body, promotions, own_promoted);
            if let Some(eb) = else_body {
                rewrite_callsites_in_body(eb, promotions, own_promoted);
            }
        }
        RustStmt::While { condition, body } => {
            rewrite_callsites_in_expr(condition, promotions, own_promoted);
            rewrite_callsites_in_body(body, promotions, own_promoted);
        }
        RustStmt::For { iter, body, .. } => {
            rewrite_callsites_in_expr(iter, promotions, own_promoted);
            rewrite_callsites_in_body(body, promotions, own_promoted);
        }
        RustStmt::Loop { body } => rewrite_callsites_in_body(body, promotions, own_promoted),
        RustStmt::Expr(expr) => rewrite_callsites_in_expr(expr, promotions, own_promoted),
    }
}

fn rewrite_callsites_in_expr(
    expr: &mut RustExpr,
    promotions: &HashMap<String, Vec<usize>>,
    own_promoted: &HashSet<String>,
) {
    // Recurse into children first
    match expr {
        RustExpr::Call { callee, args, .. } => {
            rewrite_callsites_in_expr(callee, promotions, own_promoted);
            for arg in args {
                rewrite_callsites_in_expr(arg, promotions, own_promoted);
            }
        }
        RustExpr::MacroCall { args, .. } => {
            for arg in args {
                rewrite_callsites_in_expr(arg, promotions, own_promoted);
            }
        }
        RustExpr::Field { base, .. } => rewrite_callsites_in_expr(base, promotions, own_promoted),
        RustExpr::Index { base, index } => {
            rewrite_callsites_in_expr(base, promotions, own_promoted);
            rewrite_callsites_in_expr(index, promotions, own_promoted);
        }
        RustExpr::Match { scrutinee, arms } => {
            rewrite_callsites_in_expr(scrutinee, promotions, own_promoted);
            for arm in arms {
                rewrite_callsites_in_expr(&mut arm.value, promotions, own_promoted);
                if let Some(guard) = &mut arm.guard {
                    rewrite_callsites_in_expr(guard, promotions, own_promoted);
                }
            }
        }
        RustExpr::Unary { expr: inner, .. } | RustExpr::Cast { expr: inner, .. } => {
            rewrite_callsites_in_expr(inner, promotions, own_promoted);
        }
        RustExpr::Binary { left, right, .. } => {
            rewrite_callsites_in_expr(left, promotions, own_promoted);
            rewrite_callsites_in_expr(right, promotions, own_promoted);
        }
        RustExpr::Array(items) | RustExpr::Tuple(items) => {
            for item in items {
                rewrite_callsites_in_expr(item, promotions, own_promoted);
            }
        }
        RustExpr::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_callsites_in_expr(&mut field.value, promotions, own_promoted);
            }
        }
        RustExpr::Block { body, tail } => {
            rewrite_callsites_in_body(body, promotions, own_promoted);
            if let Some(tail) = tail {
                rewrite_callsites_in_expr(tail, promotions, own_promoted);
            }
        }
        RustExpr::Closure { body, .. } => rewrite_callsites_in_body(body, promotions, own_promoted),
        RustExpr::Borrow(inner)
        | RustExpr::MutBorrow(inner)
        | RustExpr::BorrowCall(inner)
        | RustExpr::BorrowMutCall(inner)
        | RustExpr::Try(inner) => {
            rewrite_callsites_in_expr(inner, promotions, own_promoted);
        }
        RustExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                rewrite_callsites_in_expr(s, promotions, own_promoted);
            }
            if let Some(e) = end {
                rewrite_callsites_in_expr(e, promotions, own_promoted);
            }
        }
        _ => {}
    }

    // After recursing, wrap args to promoted functions in &arg
    if let RustExpr::Call { callee, args, .. } = expr {
        if let Some(func_name) = extract_callee_name(callee) {
            if let Some(indexes) = promotions.get(&func_name) {
                for &idx in indexes {
                    if let Some(arg) = args.get_mut(idx) {
                        // Skip if already borrowed
                        if is_already_borrowed(arg) {
                            continue;
                        }
                        // Skip if this arg is a promoted param of the current
                        // function — it's already &T, wrapping again would
                        // produce &&T.
                        if is_own_promoted_param(arg, own_promoted) {
                            continue;
                        }
                        let owned = std::mem::replace(arg, RustExpr::Bool(false));
                        *arg = RustExpr::Borrow(Box::new(owned));
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Check if an argument is a simple `Path(["name"])` where `name` is in the
/// current function's promoted params.  Such args are already `&T` and
/// should NOT be wrapped in `&` again.
fn is_own_promoted_param(expr: &RustExpr, own_promoted: &HashSet<String>) -> bool {
    matches!(expr, RustExpr::Path(p) if p.len() == 1 && own_promoted.contains(&p[0]))
}

fn extract_callee_name(callee: &RustExpr) -> Option<String> {
    match callee {
        RustExpr::Path(p) if p.len() == 1 => Some(p[0].clone()),
        RustExpr::Path(p) if p.len() == 2 => Some(format!("{}::{}", p[0], p[1])),
        RustExpr::Field { base, field } => {
            if let RustExpr::Path(p) = base.as_ref() {
                if p.len() == 1 {
                    return Some(format!("{}::{}", p[0], field));
                }
            }
            None
        }
        _ => None,
    }
}

fn is_already_borrowed(expr: &RustExpr) -> bool {
    matches!(
        expr,
        RustExpr::BorrowCall(_) | RustExpr::Borrow(_) | RustExpr::BorrowMutCall(_)
    )
}
