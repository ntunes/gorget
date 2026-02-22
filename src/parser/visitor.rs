//! AST visitor trait with exhaustive default walks.
//!
//! Every `Expr` and `Stmt` variant is explicitly matched in `walk_expr` /
//! `walk_stmt`.  Adding a new variant to either enum causes a compile error
//! here, ensuring every walker is updated.
//!
//! Implementors override the `visit_*` methods they care about and call the
//! corresponding `walk_*` function for default recursion.  The pattern follows
//! rustc's own visitor infrastructure.

use crate::lexer::token::StringSegment;

use super::ast::*;

/// Trait for walking the Gorget AST.
///
/// Default implementations recurse into all children via the `walk_*`
/// functions.  Override a method to intercept a node, and call the
/// corresponding `walk_*` from your override to continue recursion.
pub trait ExprVisitor {
    /// Visit an expression.  Default: recurse via [`walk_expr`].
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    /// Visit a statement.  Default: recurse via [`walk_stmt`].
    fn visit_stmt(&mut self, stmt: &Stmt) {
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
pub fn walk_expr<V: ExprVisitor + ?Sized>(v: &mut V, expr: &Expr) {
    match expr {
        // ── Leaves (no child expressions) ──
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::CharLiteral(_)
        | Expr::NoneLiteral
        | Expr::Identifier(_)
        | Expr::SelfExpr
        | Expr::Path { .. }
        | Expr::It => {}

        // ── String literal (may contain interpolations) ──
        Expr::StringLiteral(s) => {
            for seg in &s.segments {
                if let StringSegment::Interpolation(name) = seg {
                    // Treat interpolated name as an identifier reference.
                    let fake = Expr::Identifier(name.clone());
                    v.visit_expr(&fake);
                }
            }
        }

        // ── Unary ──
        Expr::UnaryOp { operand, .. } => {
            v.visit_expr(&operand.node);
        }

        // ── Binary ──
        Expr::BinaryOp { left, right, .. } => {
            v.visit_expr(&left.node);
            v.visit_expr(&right.node);
        }

        // ── Calls ──
        Expr::Call { callee, args, .. } => {
            v.visit_expr(&callee.node);
            for arg in args {
                v.visit_expr(&arg.node.value.node);
            }
        }
        Expr::MethodCall {
            receiver, args, ..
        } => {
            v.visit_expr(&receiver.node);
            for arg in args {
                v.visit_expr(&arg.node.value.node);
            }
        }

        // ── Access ──
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            v.visit_expr(&object.node);
        }
        Expr::Index { object, index } => {
            v.visit_expr(&object.node);
            v.visit_expr(&index.node);
        }

        // ── Range ──
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                v.visit_expr(&s.node);
            }
            if let Some(e) = end {
                v.visit_expr(&e.node);
            }
        }

        // ── Optional chaining / nil coalescing ──
        Expr::OptionalChain { object, .. } => {
            v.visit_expr(&object.node);
        }
        Expr::NilCoalescing { lhs, rhs } => {
            v.visit_expr(&lhs.node);
            v.visit_expr(&rhs.node);
        }

        // ── Wrapper expressions ──
        Expr::Try { expr }
        | Expr::Move { expr }
        | Expr::MutableBorrow { expr }
        | Expr::Deref { expr }
        | Expr::Await { expr }
        | Expr::Spawn { expr }
        | Expr::TryCapture { expr }
        | Expr::As { expr, .. } => {
            v.visit_expr(&expr.node);
        }

        // ── If expression ──
        Expr::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
        } => {
            v.visit_expr(&condition.node);
            v.visit_expr(&then_branch.node);
            for (cond, body) in elif_branches {
                v.visit_expr(&cond.node);
                v.visit_expr(&body.node);
            }
            if let Some(eb) = else_branch {
                v.visit_expr(&eb.node);
            }
        }

        // ── Match expression ──
        Expr::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            v.visit_expr(&scrutinee.node);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    v.visit_expr(&guard.node);
                }
                v.visit_expr(&arm.body.node);
            }
            if let Some(eb) = else_arm {
                v.visit_expr(&eb.node);
            }
        }

        // ── Block / Do ──
        Expr::Block(block) | Expr::Do { body: block } => {
            v.visit_block(block);
        }

        // ── Closures ──
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            v.visit_expr(&body.node);
        }

        // ── Comprehensions ──
        Expr::ListComprehension {
            expr: comp_expr,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(&iterable.node);
            v.visit_expr(&comp_expr.node);
            if let Some(cond) = condition {
                v.visit_expr(&cond.node);
            }
        }
        Expr::DictComprehension {
            key,
            value,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(&iterable.node);
            v.visit_expr(&key.node);
            v.visit_expr(&value.node);
            if let Some(cond) = condition {
                v.visit_expr(&cond.node);
            }
        }
        Expr::SetComprehension {
            expr: comp_expr,
            iterable,
            condition,
            ..
        } => {
            v.visit_expr(&iterable.node);
            v.visit_expr(&comp_expr.node);
            if let Some(cond) = condition {
                v.visit_expr(&cond.node);
            }
        }

        // ── Collection literals ──
        Expr::ArrayLiteral(items) | Expr::TupleLiteral(items) => {
            for item in items {
                v.visit_expr(&item.node);
            }
        }
        Expr::DictLiteral(pairs) => {
            for (k, val) in pairs {
                v.visit_expr(&k.node);
                v.visit_expr(&val.node);
            }
        }

        // ── Struct literal ──
        Expr::StructLiteral { args, .. } => {
            for arg in args {
                v.visit_expr(&arg.node);
            }
        }

        // ── Is pattern test ──
        Expr::Is { expr, .. } => {
            v.visit_expr(&expr.node);
        }
    }
}

/// Recursively visit all child expressions/blocks of `stmt`.
///
/// **Exhaustive:** every `Stmt` variant is matched.
pub fn walk_stmt<V: ExprVisitor + ?Sized>(v: &mut V, stmt: &Stmt) {
    match stmt {
        Stmt::VarDecl { value, .. } => {
            v.visit_expr(&value.node);
        }
        Stmt::Expr(expr) => {
            v.visit_expr(&expr.node);
        }
        Stmt::Assign { target, value } => {
            v.visit_expr(&target.node);
            v.visit_expr(&value.node);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            v.visit_expr(&target.node);
            v.visit_expr(&value.node);
        }
        Stmt::Return(Some(expr)) | Stmt::Throw(expr) | Stmt::Break(Some(expr)) => {
            v.visit_expr(&expr.node);
        }
        Stmt::Return(None) | Stmt::Break(None) | Stmt::Continue | Stmt::Pass => {}
        Stmt::For {
            iterable,
            body,
            else_body,
            ..
        } => {
            v.visit_expr(&iterable.node);
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
            v.visit_expr(&condition.node);
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
            v.visit_expr(&condition.node);
            v.visit_block(then_body);
            for (cond, body) in elif_branches {
                v.visit_expr(&cond.node);
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
            v.visit_expr(&scrutinee.node);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    v.visit_expr(&guard.node);
                }
                v.visit_expr(&arm.body.node);
            }
            if let Some(eb) = else_arm {
                v.visit_block(eb);
            }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                v.visit_expr(&binding.expr.node);
            }
            v.visit_block(body);
        }
        Stmt::Unsafe { body } => {
            v.visit_block(body);
        }
        Stmt::Assert {
            condition,
            message,
        } => {
            v.visit_expr(&condition.node);
            if let Some(msg) = message {
                v.visit_expr(&msg.node);
            }
        }
        Stmt::Item(_) => {
            // Nested items are not walked — they are separate compilation units.
        }
    }
}

/// Walk each statement in a block.
pub fn walk_block<V: ExprVisitor + ?Sized>(v: &mut V, block: &Block) {
    for stmt in &block.stmts {
        v.visit_stmt(&stmt.node);
    }
}
