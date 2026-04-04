//! Last-use analysis for auto-move optimization (Phase 1f).
//!
//! Full-function reverse walk that tags each variable use as "last use" or not.
//! A use is "last" if the variable is not used on any future execution path.
//!
//! Algorithm: reverse AST walk with branch union and two-pass loops.
//! Result: a set of span positions where the identifier is the last use.

use crate::parser::ast::*;
use crate::span::Spanned;
use rustc_hash::FxHashSet;

/// Result of full-function liveness analysis.
/// Contains the span start positions of identifier uses that are the last use
/// of that variable on all reachable paths.
pub struct LivenessResult {
    /// Set of span.start values for Expr::Identifier uses that are last uses.
    pub last_use_spans: FxHashSet<usize>,
}

/// Compute last-use information for an entire function body.
pub fn compute_function_liveness(body: &[Spanned<Stmt>]) -> LivenessResult {
    let mut live = FxHashSet::default();
    let mut last_use_spans = FxHashSet::default();
    walk_block(body, &mut live, &mut last_use_spans);
    LivenessResult { last_use_spans }
}

fn walk_block(
    stmts: &[Spanned<Stmt>],
    live: &mut FxHashSet<String>,
    last_uses: &mut FxHashSet<usize>,
) {
    for stmt in stmts.iter().rev() {
        walk_stmt(&stmt.node, live, last_uses);
    }
}

fn walk_stmt(
    stmt: &Stmt,
    live: &mut FxHashSet<String>,
    lu: &mut FxHashSet<usize>,
) {
    match stmt {
        Stmt::VarDecl { pattern, value, .. } => {
            kill_pattern(pattern, live);
            uses_expr(&value.node, value.span.start, live, lu);
        }
        Stmt::Assign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                live.remove(name);
            }
            uses_expr(&value.node, value.span.start, live, lu);
            uses_target_sub(&target.node, live, lu);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            uses_expr(&value.node, value.span.start, live, lu);
            uses_expr(&target.node, target.span.start, live, lu);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) => {
            uses_expr(&expr.node, expr.span.start, live, lu);
        }
        Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
            let saved = live.clone();
            let mut live_else = saved.clone();
            if let Some(b) = else_body { walk_block(&b.stmts, &mut live_else, lu); }
            let mut live_then = saved.clone();
            walk_block(&then_body.stmts, &mut live_then, lu);
            *live = live_then;
            live.extend(live_else);
            for (cond, body) in elif_branches.iter().rev() {
                let mut live_elif = saved.clone();
                walk_block(&body.stmts, &mut live_elif, lu);
                uses_expr(&cond.node, cond.span.start, &mut live_elif, lu);
                live.extend(live_elif);
            }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Stmt::While { condition, body, else_body, .. } => {
            let mut live_body = live.clone();
            if let Some(eb) = else_body { walk_block(&eb.stmts, &mut live_body, lu); }
            walk_block(&body.stmts, &mut live_body, lu);
            uses_expr(&condition.node, condition.span.start, &mut live_body, lu);
            live.extend(live_body);
            // Pass 2
            walk_block(&body.stmts, live, lu);
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Stmt::For { pattern, iterable, body, else_body, .. } => {
            let mut live_body = live.clone();
            if let Some(eb) = else_body { walk_block(&eb.stmts, &mut live_body, lu); }
            walk_block(&body.stmts, &mut live_body, lu);
            kill_pattern(pattern, &mut live_body);
            live.extend(live_body);
            walk_block(&body.stmts, live, lu);
            kill_pattern(pattern, live);
            uses_expr(&iterable.node, iterable.span.start, live, lu);
        }
        Stmt::Match { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union = FxHashSet::default();
            for item in arms {
                if let MatchItem::Arm(arm) = item {
                    let mut a = saved.clone();
                    uses_expr(&arm.body.node, arm.body.span.start, &mut a, lu);
                    if let Some(g) = &arm.guard { uses_expr(&g.node, g.span.start, &mut a, lu); }
                    kill_pattern(&arm.pattern, &mut a);
                    union.extend(a);
                }
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                walk_block(&b.stmts, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            uses_expr(&scrutinee.node, scrutinee.span.start, live, lu);
        }
        Stmt::Assert { condition, message, .. } => {
            if let Some(m) = message { uses_expr(&m.node, m.span.start, live, lu); }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        _ => {}
    }
}

/// Process an expression: for each Identifier, check if it's a last use.
fn uses_expr(
    expr: &Expr,
    span_start: usize,
    live: &mut FxHashSet<String>,
    lu: &mut FxHashSet<usize>,
) {
    match expr {
        Expr::Identifier(name) => {
            if !live.contains(name) {
                // Not in live set → this is the last use → record span
                lu.insert(span_start);
            }
            live.insert(name.clone());
        }
        Expr::Call { callee, args, .. } => {
            // Process args right-to-left (reverse of evaluation order)
            for a in args.iter().rev() {
                uses_expr(&a.node.value.node, a.node.value.span.start, live, lu);
            }
            uses_expr(&callee.node, callee.span.start, live, lu);
        }
        Expr::MethodCall { receiver, args, .. } => {
            for a in args.iter().rev() {
                uses_expr(&a.node.value.node, a.node.value.span.start, live, lu);
            }
            uses_expr(&receiver.node, receiver.span.start, live, lu);
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::Index { object, index, .. } => {
            uses_expr(&index.node, index.span.start, live, lu);
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::BinaryOp { left, right, .. } => {
            uses_expr(&right.node, right.span.start, live, lu);
            uses_expr(&left.node, left.span.start, live, lu);
        }
        Expr::UnaryOp { operand, .. } | Expr::Move { expr: operand } => {
            uses_expr(&operand.node, operand.span.start, live, lu);
        }
        Expr::Block(block) => {
            walk_block(&block.stmts, live, lu);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            let saved = live.clone();
            let mut live_else = saved.clone();
            if let Some(b) = else_branch { uses_expr(&b.node, b.span.start, &mut live_else, lu); }
            let mut live_then = saved;
            uses_expr(&then_branch.node, then_branch.span.start, &mut live_then, lu);
            *live = live_then;
            live.extend(live_else);
            for (c, b) in elif_branches.iter().rev() {
                uses_expr(&b.node, b.span.start, live, lu);
                uses_expr(&c.node, c.span.start, live, lu);
            }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union = FxHashSet::default();
            for arm in arms {
                let mut a = saved.clone();
                uses_expr(&arm.body.node, arm.body.span.start, &mut a, lu);
                kill_pattern(&arm.pattern, &mut a);
                union.extend(a);
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                uses_expr(&b.node, b.span.start, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            uses_expr(&scrutinee.node, scrutinee.span.start, live, lu);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            // Closures capture — treat as uses at the closure creation site.
            uses_expr(&body.node, span_start, live, lu);
        }
        Expr::StringLiteral(_) => {}
        _ => {} // Literals etc.
    }
}

fn uses_target_sub(expr: &Expr, live: &mut FxHashSet<String>, lu: &mut FxHashSet<usize>) {
    match expr {
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::Index { object, index, .. } => {
            uses_expr(&index.node, index.span.start, live, lu);
            uses_expr(&object.node, object.span.start, live, lu);
        }
        _ => {}
    }
}

fn kill_pattern(pat: &Spanned<Pattern>, live: &mut FxHashSet<String>) {
    match &pat.node {
        Pattern::Binding(name) => { live.remove(name); }
        Pattern::Constructor { fields, .. } => { for f in fields { kill_pattern(f, live); } }
        Pattern::Tuple(elems) => { for e in elems { kill_pattern(e, live); } }
        _ => {}
    }
}
