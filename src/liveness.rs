/// Borrow engine abstraction and liveness-based implementation.
///
/// This module defines the `BorrowEngine` trait — a facade that the lowering
/// pass queries to decide whether a variable use requires a clone. Two
/// implementations are provided:
///
/// 1. **`BranchAwarePlan`** (this module) — lightweight branch-aware liveness
///    analysis on the typed AST. Tracks variable uses with their control-flow
///    context so that uses in disjoint branches (if/else, match arms) don't
///    count as conflicting. Enabled via `--exp-optimistic-move`.
///
/// 2. **`PoloniusEngine`** (in `polonius.rs`) — delegates to the Polonius
///    borrow checker for precise dataflow analysis. Enabled via `--exp-polonius`.
use crate::ir::typed::{TypedExpr, TypedExprKind, TypedStmt};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// BorrowEngine trait — the facade
// ---------------------------------------------------------------------------

/// Unified interface for borrow/ownership analysis engines.
///
/// The lowering pass uses this trait to query remaining conflicting uses
/// and to mark uses as consumed, without knowing which engine is active.
pub trait BorrowEngine {
    /// Returns the number of remaining uses of `place` that can co-execute
    /// with the current (next unconsumed) use.
    fn remaining_conflicting(&self, place: &str) -> usize;

    /// Mark one use of the given place as consumed.
    fn consume(&mut self, place: &str);
}

// ---------------------------------------------------------------------------
// Branch path representation
// ---------------------------------------------------------------------------

/// A branch path identifies a unique control-flow position in the AST.
/// Two uses conflict only if their branch paths are "compatible" — meaning
/// they can both execute in the same program run.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BranchPath(Vec<BranchSegment>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BranchSegment {
    /// Unique ID for the branch point (if/match).
    branch_id: u32,
    /// Which arm of the branch point this is.
    arm_index: u32,
}

impl BranchPath {
    fn root() -> Self {
        BranchPath(Vec::new())
    }

    fn push(&self, branch_id: u32, arm_index: u32) -> Self {
        let mut path = self.clone();
        path.0.push(BranchSegment {
            branch_id,
            arm_index,
        });
        path
    }
}

/// Two branch paths can co-execute if they don't diverge at any branch point.
fn branches_can_coexecute(a: &BranchPath, b: &BranchPath) -> bool {
    for (sa, sb) in a.0.iter().zip(b.0.iter()) {
        if sa.branch_id == sb.branch_id && sa.arm_index != sb.arm_index {
            return false;
        }
    }
    true
}

// ---------------------------------------------------------------------------
// Place use record
// ---------------------------------------------------------------------------

type Place = String;

#[derive(Debug, Clone)]
struct PlaceUse {
    branch_path: BranchPath,
}

// ---------------------------------------------------------------------------
// BranchAwarePlan — internal liveness engine
// ---------------------------------------------------------------------------

/// Branch-aware ownership plan that tracks variable uses with their
/// control-flow context to avoid false-positive clones.
#[derive(Debug, Default)]
pub struct BranchAwarePlan {
    place_uses: HashMap<Place, Vec<PlaceUse>>,
    consumed: HashMap<Place, usize>,
    next_branch_id: u32,
}

impl BorrowEngine for BranchAwarePlan {
    fn remaining_conflicting(&self, place: &str) -> usize {
        let Some(uses) = self.place_uses.get(place) else {
            return 0;
        };
        let consumed = self.consumed.get(place).copied().unwrap_or(0);
        if consumed >= uses.len() {
            return 0;
        }
        let current_branch = &uses[consumed].branch_path;
        uses[consumed + 1..]
            .iter()
            .filter(|u| branches_can_coexecute(current_branch, &u.branch_path))
            .count()
    }

    fn consume(&mut self, place: &str) {
        let counter = self.consumed.entry(place.to_string()).or_insert(0);
        *counter += 1;
    }
}

impl BranchAwarePlan {
    /// Build a branch-aware plan from typed statements.
    pub fn from_stmts(stmts: &[TypedStmt]) -> Self {
        let mut plan = BranchAwarePlan::default();
        let branch_path = BranchPath::root();
        plan.collect_stmts(stmts, &branch_path);
        plan
    }

    fn fresh_branch_id(&mut self) -> u32 {
        let id = self.next_branch_id;
        self.next_branch_id += 1;
        id
    }

    // -----------------------------------------------------------------------
    // AST walking — uses actual TypedStmt / TypedExprKind variants
    // -----------------------------------------------------------------------

    fn collect_stmts(&mut self, stmts: &[TypedStmt], branch_path: &BranchPath) {
        for stmt in stmts {
            self.collect_stmt(stmt, branch_path);
        }
    }

    fn collect_stmt(&mut self, stmt: &TypedStmt, branch_path: &BranchPath) {
        match stmt {
            TypedStmt::Const(c) => {
                self.collect_expr(&c.value, branch_path);
            }
            TypedStmt::DestructureConst { value, .. } => {
                self.collect_expr(value, branch_path);
            }
            TypedStmt::Assign { value, .. } => {
                self.collect_expr(value, branch_path);
            }
            TypedStmt::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.collect_expr(expr, branch_path);
                }
            }
            TypedStmt::If {
                condition,
                then_body,
                else_body,
            } => {
                self.collect_expr(condition, branch_path);
                let bid = self.fresh_branch_id();
                let then_path = branch_path.push(bid, 0);
                self.collect_stmts(then_body, &then_path);
                if let Some(else_stmts) = else_body {
                    let else_path = branch_path.push(bid, 1);
                    self.collect_stmts(else_stmts, &else_path);
                }
            }
            TypedStmt::While { condition, body } => {
                self.collect_expr(condition, branch_path);
                self.collect_stmts(body, branch_path);
            }
            TypedStmt::For { iter, body, .. } => {
                self.collect_expr(iter, branch_path);
                self.collect_stmts(body, branch_path);
            }
            TypedStmt::Loop { body } => {
                self.collect_stmts(body, branch_path);
            }
            TypedStmt::Expr(expr) => {
                self.collect_expr(expr, branch_path);
            }
            TypedStmt::Break | TypedStmt::Continue | TypedStmt::RustBlock(_) => {}
        }
    }

    fn collect_expr(&mut self, expr: &TypedExpr, branch_path: &BranchPath) {
        // Record a use if the expression refers to a place.
        if let Some(place) = place_for_typed_expr(expr) {
            self.place_uses.entry(place).or_default().push(PlaceUse {
                branch_path: branch_path.clone(),
            });
        }

        // Recurse into sub-expressions, splitting at branch points.
        match &expr.kind {
            TypedExprKind::Match { scrutinee, arms } => {
                self.collect_expr(scrutinee, branch_path);
                let bid = self.fresh_branch_id();
                for (i, arm) in arms.iter().enumerate() {
                    let arm_path = branch_path.push(bid, i as u32);
                    if let Some(guard) = &arm.guard {
                        self.collect_expr(guard, &arm_path);
                    }
                    self.collect_expr(&arm.value, &arm_path);
                }
            }
            TypedExprKind::Block { body, tail } => {
                self.collect_stmts(body, branch_path);
                if let Some(tail_expr) = tail {
                    self.collect_expr(tail_expr, branch_path);
                }
            }
            TypedExprKind::Call { callee, args } => {
                self.collect_expr(callee, branch_path);
                for arg in args {
                    self.collect_expr(arg, branch_path);
                }
            }
            TypedExprKind::MacroCall { args, .. } => {
                for arg in args {
                    self.collect_expr(arg, branch_path);
                }
            }
            TypedExprKind::Field { base, .. } => {
                self.collect_expr(base, branch_path);
            }
            TypedExprKind::Index { base, index, .. } => {
                self.collect_expr(base, branch_path);
                self.collect_expr(index, branch_path);
            }
            TypedExprKind::Binary { left, right, .. } => {
                self.collect_expr(left, branch_path);
                self.collect_expr(right, branch_path);
            }
            TypedExprKind::Unary { expr: operand, .. } => {
                self.collect_expr(operand, branch_path);
            }
            TypedExprKind::Tuple(elems) | TypedExprKind::Array(elems) => {
                for elem in elems {
                    self.collect_expr(elem, branch_path);
                }
            }
            TypedExprKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.collect_expr(&field.value, branch_path);
                }
            }
            TypedExprKind::Closure { body, .. } => {
                self.collect_stmts(body, branch_path);
            }
            TypedExprKind::Cast { expr: inner, .. } => {
                self.collect_expr(inner, branch_path);
            }
            TypedExprKind::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.collect_expr(s, branch_path);
                }
                if let Some(e) = end {
                    self.collect_expr(e, branch_path);
                }
            }
            TypedExprKind::Try(inner) => {
                self.collect_expr(inner, branch_path);
            }
            // Leaf expressions: Path, Int, Float, Bool, Char, String
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Place extraction from TypedExpr
// ---------------------------------------------------------------------------

fn place_for_typed_expr(expr: &TypedExpr) -> Option<String> {
    match &expr.kind {
        TypedExprKind::Path(segments) => {
            if segments.len() == 1 {
                Some(segments[0].clone())
            } else {
                None
            }
        }
        TypedExprKind::Field { base, field } => {
            let base_place = place_for_typed_expr(base)?;
            Some(format!("{base_place}.{field}"))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disjoint_branches_do_not_conflict() {
        let mut plan = BranchAwarePlan::default();
        let root = BranchPath::root();
        let bid = plan.fresh_branch_id();
        let then_path = root.push(bid, 0);
        let else_path = root.push(bid, 1);

        plan.place_uses
            .entry("x".to_string())
            .or_default()
            .push(PlaceUse {
                branch_path: then_path,
            });
        plan.place_uses
            .entry("x".to_string())
            .or_default()
            .push(PlaceUse {
                branch_path: else_path,
            });

        assert_eq!(plan.remaining_conflicting("x"), 0);
    }

    #[test]
    fn same_branch_uses_do_conflict() {
        let mut plan = BranchAwarePlan::default();
        let root = BranchPath::root();

        for _ in 0..2 {
            plan.place_uses
                .entry("x".to_string())
                .or_default()
                .push(PlaceUse {
                    branch_path: root.clone(),
                });
        }

        assert_eq!(plan.remaining_conflicting("x"), 1);
    }

    #[test]
    fn nested_branch_preserves_parent_context() {
        let mut plan = BranchAwarePlan::default();
        let root = BranchPath::root();
        let bid = plan.fresh_branch_id();
        let inner_arm = root.push(bid, 0);

        plan.place_uses
            .entry("x".to_string())
            .or_default()
            .push(PlaceUse {
                branch_path: root.clone(),
            });
        plan.place_uses
            .entry("x".to_string())
            .or_default()
            .push(PlaceUse {
                branch_path: inner_arm,
            });

        assert_eq!(plan.remaining_conflicting("x"), 1);
    }

    #[test]
    fn consume_reduces_conflicts() {
        let mut plan = BranchAwarePlan::default();
        let root = BranchPath::root();

        for _ in 0..3 {
            plan.place_uses
                .entry("x".to_string())
                .or_default()
                .push(PlaceUse {
                    branch_path: root.clone(),
                });
        }

        assert_eq!(plan.remaining_conflicting("x"), 2);
        plan.consume("x");
        assert_eq!(plan.remaining_conflicting("x"), 1);
        plan.consume("x");
        assert_eq!(plan.remaining_conflicting("x"), 0);
    }

    #[test]
    fn branches_coexecute_same_arm() {
        let root = BranchPath::root();
        let a = root.push(0, 0);
        let b = root.push(0, 0);
        assert!(branches_can_coexecute(&a, &b));
    }

    #[test]
    fn branches_cannot_coexecute_different_arms() {
        let root = BranchPath::root();
        let a = root.push(0, 0);
        let b = root.push(0, 1);
        assert!(!branches_can_coexecute(&a, &b));
    }

    #[test]
    fn three_match_arms_pairwise_disjoint() {
        let mut plan = BranchAwarePlan::default();
        let root = BranchPath::root();
        let bid = plan.fresh_branch_id();

        for arm in 0..3 {
            plan.place_uses
                .entry("x".to_string())
                .or_default()
                .push(PlaceUse {
                    branch_path: root.push(bid, arm),
                });
        }

        // At arm 0, arms 1 and 2 are in different branches — 0 conflicts.
        assert_eq!(plan.remaining_conflicting("x"), 0);
    }
}
