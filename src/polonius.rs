/// Polonius-based borrow analysis engine.
///
/// This module implements the `BorrowEngine` trait by delegating to the
/// Polonius borrow checker for precise, flow-sensitive ownership analysis.
///
/// Enabled via `--exp-polonius`. When active, the lowering pass uses this
/// engine instead of the default flat counter or the branch-aware plan.
///
/// # Architecture
///
/// Polonius uses a "facts-based" approach:
/// 1. We emit Polonius input facts (loan origins, region constraints)
///    from the typed AST during a pre-scan pass.
/// 2. Polonius computes which loans are live at each program point.
/// 3. We query the output to determine if a variable can be moved or
///    must be cloned.
///
/// Currently this is a skeleton that falls back to conservative behavior
/// (all uses conflict). The full Polonius integration will be wired up
/// once the `polonius-engine` crate is added as a dependency.
use crate::ir::typed::TypedStmt;
use crate::liveness::BorrowEngine;
use std::collections::HashMap;

/// Polonius-backed borrow engine.
///
/// Uses Polonius output facts to determine whether a variable is still
/// live (and therefore needs a clone) or is dead after a given use.
#[derive(Debug, Default)]
pub struct PoloniusEngine {
    /// Flat use counts — conservative fallback until Polonius is fully wired.
    place_use_counts: HashMap<String, usize>,
    consumed: HashMap<String, usize>,
}

impl BorrowEngine for PoloniusEngine {
    fn remaining_conflicting(&self, place: &str) -> usize {
        let total = self.place_use_counts.get(place).copied().unwrap_or(0);
        let consumed = self.consumed.get(place).copied().unwrap_or(0);
        total.saturating_sub(consumed + 1)
    }

    fn consume(&mut self, place: &str) {
        let counter = self.consumed.entry(place.to_string()).or_insert(0);
        *counter += 1;
    }
}

impl PoloniusEngine {
    /// Build from typed statements — currently just counts uses (conservative).
    ///
    /// TODO: Generate Polonius input facts and compute precise liveness.
    pub fn from_stmts(stmts: &[TypedStmt]) -> Self {
        let mut engine = PoloniusEngine::default();
        for stmt in stmts {
            engine.collect_stmt(stmt);
        }
        engine
    }

    fn collect_stmt(&mut self, stmt: &TypedStmt) {
        match stmt {
            TypedStmt::Const(c) => self.collect_expr(&c.value),
            TypedStmt::DestructureConst { value, .. } => self.collect_expr(value),
            TypedStmt::Assign { value, .. } => self.collect_expr(value),
            TypedStmt::Return(Some(expr)) => self.collect_expr(expr),
            TypedStmt::If {
                condition,
                then_body,
                else_body,
            } => {
                self.collect_expr(condition);
                for s in then_body {
                    self.collect_stmt(s);
                }
                if let Some(else_stmts) = else_body {
                    for s in else_stmts {
                        self.collect_stmt(s);
                    }
                }
            }
            TypedStmt::While { condition, body } => {
                self.collect_expr(condition);
                for s in body {
                    self.collect_stmt(s);
                }
            }
            TypedStmt::For { iter, body, .. } => {
                self.collect_expr(iter);
                for s in body {
                    self.collect_stmt(s);
                }
            }
            TypedStmt::Loop { body } => {
                for s in body {
                    self.collect_stmt(s);
                }
            }
            TypedStmt::Expr(expr) => self.collect_expr(expr),
            _ => {}
        }
    }

    fn collect_expr(&mut self, expr: &crate::ir::typed::TypedExpr) {
        use crate::ir::typed::TypedExprKind;

        // Record place use.
        if let Some(place) = place_for_expr(expr) {
            *self.place_use_counts.entry(place).or_insert(0) += 1;
        }

        // Recurse.
        match &expr.kind {
            TypedExprKind::Call { callee, args } => {
                self.collect_expr(callee);
                for a in args {
                    self.collect_expr(a);
                }
            }
            TypedExprKind::MacroCall { args, .. } => {
                for a in args {
                    self.collect_expr(a);
                }
            }
            TypedExprKind::Field { base, .. } => self.collect_expr(base),
            TypedExprKind::Index { base, index, .. } => {
                self.collect_expr(base);
                self.collect_expr(index);
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.collect_expr(scrutinee);
                for arm in arms {
                    if let Some(g) = &arm.guard {
                        self.collect_expr(g);
                    }
                    self.collect_expr(&arm.value);
                }
            }
            TypedExprKind::Binary { left, right, .. } => {
                self.collect_expr(left);
                self.collect_expr(right);
            }
            TypedExprKind::Unary { expr: e, .. } => self.collect_expr(e),
            TypedExprKind::Tuple(es) | TypedExprKind::Array(es) => {
                for e in es {
                    self.collect_expr(e);
                }
            }
            TypedExprKind::StructLiteral { fields, .. } => {
                for f in fields {
                    self.collect_expr(&f.value);
                }
            }
            TypedExprKind::Block { body, tail } => {
                for s in body {
                    self.collect_stmt(s);
                }
                if let Some(t) = tail {
                    self.collect_expr(t);
                }
            }
            TypedExprKind::Closure { body, .. } => {
                for s in body {
                    self.collect_stmt(s);
                }
            }
            TypedExprKind::Cast { expr: e, .. } | TypedExprKind::Try(e) => self.collect_expr(e),
            TypedExprKind::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.collect_expr(s);
                }
                if let Some(e) = end {
                    self.collect_expr(e);
                }
            }
            _ => {}
        }
    }
}

fn place_for_expr(expr: &crate::ir::typed::TypedExpr) -> Option<String> {
    use crate::ir::typed::TypedExprKind;
    match &expr.kind {
        TypedExprKind::Path(segments) if segments.len() == 1 => Some(segments[0].clone()),
        TypedExprKind::Field { base, field } => {
            let base_place = place_for_expr(base)?;
            Some(format!("{base_place}.{field}"))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conservative_counts_all_uses() {
        let mut engine = PoloniusEngine::default();
        engine.place_use_counts.insert("x".to_string(), 3);

        assert_eq!(engine.remaining_conflicting("x"), 2);
        engine.consume("x");
        assert_eq!(engine.remaining_conflicting("x"), 1);
        engine.consume("x");
        assert_eq!(engine.remaining_conflicting("x"), 0);
    }

    #[test]
    fn unknown_place_returns_zero() {
        let engine = PoloniusEngine::default();
        assert_eq!(engine.remaining_conflicting("nonexistent"), 0);
    }
}
