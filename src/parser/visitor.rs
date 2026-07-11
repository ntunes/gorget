//! AST visitor trait with exhaustive default walks.
//!
//! Every `Expr` and `Stmt` variant is explicitly matched in `walk_expr` /
//! `walk_stmt`.  Adding a new variant to either enum causes a compile error
//! here, ensuring every walker is updated.
//!
//! Implementors override the `visit_*` methods they care about and call the
//! corresponding `walk_*` function for default recursion.  The pattern follows
//! rustc's own visitor infrastructure.
//!
//! The trait passes `Spanned<Expr>` and `Spanned<Stmt>` so that visitors
//! needing source location info (e.g. resolution-map lookups) have it.

use crate::lexer::token::StringSegment;
use crate::span::{Span, Spanned};

use super::ast::*;

/// Trait for walking the Gorget AST.
///
/// Default implementations recurse into all children via the `walk_*`
/// functions.  Override a method to intercept a node, and call the
/// corresponding `walk_*` from your override to continue recursion.
pub trait ExprVisitor {
    /// Visit an expression.  Default: recurse via [`walk_expr`].
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        walk_expr(self, expr);
    }

    /// Visit a statement.  Default: recurse via [`walk_stmt`].
    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
        walk_stmt(self, stmt);
    }

    /// Visit a block (sequence of statements).  Default: visit each statement.
    fn visit_block(&mut self, block: &Block) {
        walk_block(self, block);
    }
}

// ─── Default walk functions ──────────────────────────────────

/// Recursively visit all child expressions/statements of `expr`.
///
/// **Exhaustive:** every `Expr` variant is matched.  A new variant that is
/// not added here will cause a compile error.
pub fn walk_expr<V: ExprVisitor + ?Sized>(v: &mut V, expr: &Spanned<Expr>) {
    match &expr.node {
        // ── Leaves (no child expressions) ──
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::Identifier(_)
        | Expr::SelfExpr
        | Expr::Path { .. }
        | Expr::It => {}

        // ── String literal (may contain interpolations) ──
        Expr::StringLiteral(s, _) => {
            for seg in &s.segments {
                if let StringSegment::Interpolation(name, _) = seg {
                    // Treat interpolated name as an identifier reference.
                    // Use the string literal's span as a fallback — visitors
                    // needing precise interpolation spans should override.
                    let fake = Spanned {
                        node: Expr::Identifier(name.clone()),
                        span: Span::dummy(),
                    };
                    v.visit_expr(&fake);
                }
            }
        }

        // ── Unary ──
        Expr::UnaryOp { operand, .. } => {
            v.visit_expr(operand);
        }

        // ── Binary ──
        Expr::BinaryOp { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        // ── Calls ──
        Expr::Call { callee, args, .. } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&arg.node.value);
            }
        }
        Expr::MethodCall {
            receiver, args, ..
        } => {
            v.visit_expr(receiver);
            for arg in args {
                v.visit_expr(&arg.node.value);
            }
        }

        // ── Access ──
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            v.visit_expr(object);
        }
        Expr::Index { object, index } => {
            v.visit_expr(object);
            v.visit_expr(index);
        }

        // ── Range ──
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                v.visit_expr(s);
            }
            if let Some(e) = end {
                v.visit_expr(e);
            }
        }

        // ── Optional chaining / default operator ──
        Expr::OptionalChain { object, .. } => {
            v.visit_expr(object);
        }
        Expr::DefaultOp { lhs, rhs } => {
            v.visit_expr(lhs);
            v.visit_expr(rhs);
        }

        // ── Wrapper expressions ──
        Expr::Move { expr }
        | Expr::MutableBorrow { expr }
        | Expr::Deref { expr }
        | Expr::Await { expr }
        | Expr::Spawn { expr, .. }
        | Expr::SpawnBlocking { expr, .. }
        | Expr::As { expr, .. } => {
            v.visit_expr(expr);
        }

        // ── If expression ──
        Expr::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
        } => {
            v.visit_expr(condition);
            v.visit_expr(then_branch);
            for (cond, body) in elif_branches {
                v.visit_expr(cond);
                v.visit_expr(body);
            }
            if let Some(eb) = else_branch {
                v.visit_expr(eb);
            }
        }

        // ── Match expression ──
        Expr::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    v.visit_expr(guard);
                }
                v.visit_expr(&arm.body);
            }
            if let Some(eb) = else_arm {
                v.visit_expr(eb);
            }
        }

        // ── Block / Do ──
        Expr::Block(block) | Expr::Do { body: block } => {
            v.visit_block(block);
        }

        // ── Closures ──
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            v.visit_expr(body);
        }

        // ── Comprehensions ──
        Expr::ListComprehension {
            expr: comp_expr,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(iterable);
            v.visit_expr(comp_expr);
            if let Some(cond) = condition {
                v.visit_expr(cond);
            }
        }
        Expr::DictComprehension {
            key,
            value,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(iterable);
            v.visit_expr(key);
            v.visit_expr(value);
            if let Some(cond) = condition {
                v.visit_expr(cond);
            }
        }
        Expr::SetComprehension {
            expr: comp_expr,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(iterable);
            v.visit_expr(comp_expr);
            if let Some(cond) = condition {
                v.visit_expr(cond);
            }
        }

        // ── Collection literals ──
        Expr::ArrayLiteral(items) | Expr::TupleLiteral(items) => {
            for item in items {
                v.visit_expr(item);
            }
        }
        Expr::DictLiteral(pairs) => {
            for (k, val) in pairs {
                v.visit_expr(k);
                v.visit_expr(val);
            }
        }

        // ── Struct literal ──
        Expr::StructLiteral { args, .. } => {
            for arg in args {
                v.visit_expr(arg);
            }
        }

        // ── Is pattern test ──
        Expr::Is { expr, .. } => {
            v.visit_expr(expr);
        }

        // ── Dot-shorthand variant ──
        Expr::DotShorthand { args, .. } => {
            for arg in args {
                v.visit_expr(&arg.node.value);
            }
        }
        // ── Meta op ──
        Expr::MetaOpInfix { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, transform, .. } => {
            v.visit_expr(expr);
            v.visit_expr(transform);
        }
        Expr::Catch { expr, recovery, .. } => {
            v.visit_expr(expr);
            v.visit_expr(recovery);
        }
        Expr::FaultCatch { expr, handler, .. } => {
            v.visit_expr(expr);
            v.visit_expr(handler);
        }
    }
}

/// Recursively visit all child expressions/blocks of `stmt`.
///
/// **Exhaustive:** every `Stmt` variant is matched.
pub fn walk_stmt<V: ExprVisitor + ?Sized>(v: &mut V, stmt: &Spanned<Stmt>) {
    match &stmt.node {
        Stmt::VarDecl { value, .. } => {
            v.visit_expr(value);
        }
        Stmt::Expr(expr) => {
            v.visit_expr(expr);
        }
        Stmt::Assign { target, value } => {
            v.visit_expr(target);
            v.visit_expr(value);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            v.visit_expr(target);
            v.visit_expr(value);
        }
        Stmt::Return(Some(expr)) | Stmt::Throw(expr) => {
            v.visit_expr(expr);
        }
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::Pass => {}
        Stmt::For {
            iterable,
            body,
            else_body,
            ..
        } => {
            v.visit_expr(iterable);
            v.visit_block(body);
            if let Some(eb) = else_body {
                v.visit_block(eb);
            }
        }
        Stmt::While {
            condition,
            body,
            else_body,
        } => {
            v.visit_expr(condition);
            v.visit_block(body);
            if let Some(eb) = else_body {
                v.visit_block(eb);
            }
        }
        Stmt::Loop { body } => {
            v.visit_block(body);
        }
        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => {
            v.visit_expr(condition);
            v.visit_block(then_body);
            for (cond, body) in elif_branches {
                v.visit_expr(cond);
                v.visit_block(body);
            }
            if let Some(eb) = else_body {
                v.visit_block(eb);
            }
        }
        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            v.visit_expr(scrutinee);
            for arm in arms.iter().filter_map(|i| i.arm()) {
                if let Some(guard) = &arm.guard {
                    v.visit_expr(guard);
                }
                v.visit_expr(&arm.body);
            }
            if let Some(eb) = else_arm {
                v.visit_block(eb);
            }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                match &arm.op {
                    SelectOp::Recv { channel, .. } => v.visit_expr(channel),
                    SelectOp::Send { channel, value } => {
                        v.visit_expr(channel);
                        v.visit_expr(value);
                    }
                }
                v.visit_block(&arm.body);
            }
            if let Some(eb) = else_arm { v.visit_block(eb); }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                v.visit_expr(&binding.expr);
            }
            v.visit_block(body);
        }
        Stmt::Unsafe { body } => {
            v.visit_block(body);
        }
        Stmt::NamedScope { body, .. } => {
            v.visit_block(body);
        }
        Stmt::Assert {
            condition,
            message,
        }
        | Stmt::AssertReturn {
            condition,
            message,
        } => {
            v.visit_expr(condition);
            if let Some(msg) = message {
                v.visit_expr(msg);
            }
        }
        Stmt::Snapshot { value, .. } => {
            v.visit_expr(value);
        }
        Stmt::Item(_) => {
            // Nested items are not walked — they are separate compilation units.
        }
        Stmt::MetaIf {
            condition,
            then_body,
            elif_branches,
            else_body,
            ..
        } => {
            v.visit_expr(condition);
            v.visit_block(then_body);
            for (cond, body) in elif_branches {
                v.visit_expr(cond);
                v.visit_block(body);
            }
            if let Some(eb) = else_body {
                v.visit_block(eb);
            }
        }
        Stmt::MetaFor { range, body, .. } => {
            v.visit_expr(range);
            v.visit_block(body);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            v.visit_expr(scrutinee);
            for (case_expr, body) in arms {
                v.visit_expr(case_expr);
                v.visit_block(body);
            }
            if let Some(eb) = else_arm {
                v.visit_block(eb);
            }
        }
        Stmt::MetaWhile { condition, body, .. } => {
            v.visit_expr(condition);
            v.visit_block(body);
        }
        Stmt::MetaConst { value, .. } => {
            v.visit_expr(value);
        }
        Stmt::MetaLog { args, .. } => {
            for arg in args { v.visit_expr(arg); }
        }
        Stmt::OnError { body } => {
            v.visit_block(body);
        }
    }
}

/// Walk each statement in a block.
pub fn walk_block<V: ExprVisitor + ?Sized>(v: &mut V, block: &Block) {
    for stmt in &block.stmts {
        v.visit_stmt(stmt);
    }
}
