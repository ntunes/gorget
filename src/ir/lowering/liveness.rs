//! Last-use analysis for auto-move optimization (Phase 1f).
//!
//! Determines, for each variable, whether it is live (used on a future path)
//! after a given statement. Variables that are dead after a use point can be
//! moved (zero-cost) instead of cloned at ownership boundaries.
//!
//! Algorithm: reverse walk with branch union and two-pass loops.
//! Replaces the unsound `is_single_use` (global count across all branches).

use crate::parser::ast::*;
use crate::span::Spanned;
use rustc_hash::FxHashSet;

/// Compute the set of variable names that are live at the entry of a block.
/// The `live` set on entry represents names used after the block.
pub fn live_before_block(body: &[Spanned<Stmt>]) -> FxHashSet<String> {
    let mut live = FxHashSet::default();
    walk_block_reverse(body, &mut live);
    live
}

/// Check if a variable is live after a specific statement index in a block.
/// This re-walks the suffix — suitable for one-off queries during lowering.
pub fn is_live_after(body: &[Spanned<Stmt>], stmt_idx: usize, name: &str) -> bool {
    let mut live = FxHashSet::default();
    for stmt in body[stmt_idx + 1..].iter().rev() {
        walk_stmt(&stmt.node, &mut live);
    }
    live.contains(name)
}

/// Walk a block in reverse, collecting live variable names.
fn walk_block_reverse(stmts: &[Spanned<Stmt>], live: &mut FxHashSet<String>) {
    for stmt in stmts.iter().rev() {
        walk_stmt(&stmt.node, live);
    }
}

fn walk_stmt(stmt: &Stmt, live: &mut FxHashSet<String>) {
    match stmt {
        Stmt::VarDecl { pattern, value, .. } => {
            kill_pattern(pattern, live);
            uses_in_expr(&value.node, live);
        }
        Stmt::Assign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                live.remove(name);
            }
            uses_in_expr(&value.node, live);
            uses_in_target_subexprs(&target.node, live);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            uses_in_expr(&value.node, live);
            uses_in_expr(&target.node, live);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) => {
            uses_in_expr(&expr.node, live);
        }
        Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
            let saved = live.clone();
            let mut live_else = saved.clone();
            if let Some(b) = else_body { walk_block_reverse(&b.stmts, &mut live_else); }
            let mut live_then = saved.clone();
            walk_block_reverse(&then_body.stmts, &mut live_then);
            // Union all branches
            *live = live_then;
            live.extend(live_else);
            for (cond, body) in elif_branches.iter().rev() {
                let mut live_elif = saved.clone();
                walk_block_reverse(&body.stmts, &mut live_elif);
                uses_in_expr(&cond.node, &mut live_elif);
                live.extend(live_elif);
            }
            uses_in_expr(&condition.node, live);
        }
        Stmt::While { condition, body, else_body, .. } => {
            // Pass 1
            let mut live_body = live.clone();
            if let Some(eb) = else_body { walk_block_reverse(&eb.stmts, &mut live_body); }
            walk_block_reverse(&body.stmts, &mut live_body);
            uses_in_expr(&condition.node, &mut live_body);
            // Merge (loop may repeat)
            live.extend(live_body);
            // Pass 2
            walk_block_reverse(&body.stmts, live);
            uses_in_expr(&condition.node, live);
        }
        Stmt::For { pattern, iterable, body, else_body, .. } => {
            let mut live_body = live.clone();
            if let Some(eb) = else_body { walk_block_reverse(&eb.stmts, &mut live_body); }
            walk_block_reverse(&body.stmts, &mut live_body);
            kill_pattern(pattern, &mut live_body);
            live.extend(live_body);
            // Pass 2
            walk_block_reverse(&body.stmts, live);
            kill_pattern(pattern, live);
            uses_in_expr(&iterable.node, live);
        }
        Stmt::Match { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union = FxHashSet::default();
            for item in arms {
                if let MatchItem::Arm(arm) = item {
                    let mut a = saved.clone();
                    uses_in_expr(&arm.body.node, &mut a);
                    if let Some(g) = &arm.guard { uses_in_expr(&g.node, &mut a); }
                    kill_pattern(&arm.pattern, &mut a);
                    union.extend(a);
                }
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                walk_block_reverse(&b.stmts, &mut a);
                union.extend(a);
            }
            *live = union;
            uses_in_expr(&scrutinee.node, live);
        }
        Stmt::Assert { condition, message, .. } => {
            if let Some(m) = message { uses_in_expr(&m.node, live); }
            uses_in_expr(&condition.node, live);
        }
        _ => {}
    }
}

/// Collect variable names used in an expression.
fn uses_in_expr(expr: &Expr, live: &mut FxHashSet<String>) {
    match expr {
        Expr::Identifier(name) => { live.insert(name.clone()); }
        Expr::Call { callee, args, .. } => {
            uses_in_expr(&callee.node, live);
            for a in args { uses_in_expr(&a.node.value.node, live); }
        }
        Expr::MethodCall { receiver, args, .. } => {
            uses_in_expr(&receiver.node, live);
            for a in args { uses_in_expr(&a.node.value.node, live); }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_in_expr(&object.node, live);
        }
        Expr::Index { object, index, .. } => {
            uses_in_expr(&object.node, live);
            uses_in_expr(&index.node, live);
        }
        Expr::BinaryOp { left, right, .. } => {
            uses_in_expr(&left.node, live);
            uses_in_expr(&right.node, live);
        }
        Expr::UnaryOp { operand, .. } | Expr::Move { expr: operand } => {
            uses_in_expr(&operand.node, live);
        }
        Expr::Block(block) => {
            let mut block_live = live.clone();
            walk_block_reverse(&block.stmts, &mut block_live);
            live.extend(block_live);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            uses_in_expr(&condition.node, live);
            uses_in_expr(&then_branch.node, live);
            for (c, b) in elif_branches { uses_in_expr(&c.node, live); uses_in_expr(&b.node, live); }
            if let Some(b) = else_branch { uses_in_expr(&b.node, live); }
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            uses_in_expr(&scrutinee.node, live);
            for arm in arms { uses_in_expr(&arm.body.node, live); }
            if let Some(b) = else_arm { uses_in_expr(&b.node, live); }
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            uses_in_expr(&body.node, live);
        }
        Expr::StringLiteral(_) => {} // f-string interpolation — conservative, no effect
        _ => {} // literals, self, etc.
    }
}

/// Uses in assignment target sub-expressions (field access, index) but not the top identifier.
fn uses_in_target_subexprs(expr: &Expr, live: &mut FxHashSet<String>) {
    match expr {
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_in_expr(&object.node, live);
        }
        Expr::Index { object, index, .. } => {
            uses_in_expr(&object.node, live);
            uses_in_expr(&index.node, live);
        }
        _ => {}
    }
}

fn kill_pattern(pat: &Spanned<Pattern>, live: &mut FxHashSet<String>) {
    match &pat.node {
        Pattern::Binding(name) => { live.remove(name); }
        Pattern::Constructor { fields, .. } => {
            for f in fields { kill_pattern(f, live); }
        }
        Pattern::Tuple(elems) => {
            for e in elems { kill_pattern(e, live); }
        }
        _ => {}
    }
}
