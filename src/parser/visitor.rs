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
        | Expr::ReturnValue
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
        | Expr::Propagate { expr }
        | Expr::MutableBorrow { expr }
        | Expr::Deref { expr }
        | Expr::Await { expr, .. }
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
        Expr::Block(block) | Expr::Do { body: block, .. } => {
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
        Expr::ArrayLiteral(items, _) | Expr::TupleLiteral(items) => {
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

// ─── Child enumeration — THE one source of truth for "what are an
//     expression's sub-expressions" (devbook/24 Layering rule 3) ─────────
//
// R44 Track E. Every pass whose contract is to REACH EVERY expression
// position — a rewriter, an instance collector, a "does any sub-expression
// have property P" predicate — routes its recursion through here instead of
// hand-rolling a `match` that ends in `_ => {}`. A hand-rolled walker that
// silently skips a position is the defect class this replaces: the skipped
// position produces a MISSING diagnostic, a lost generic instance, or a
// silent under-capture, and nothing in the type system notices.
//
// THREE STRUCTURAL GUARDS, all plain compile errors, no lint needed:
//   1. EVERY VARIANT IS MATCHED — no `_ =>` arm, so adding an `Expr` variant
//      fails to compile here.
//   2. EVERY FIELD IS NAMED — no `..` in any pattern, so adding a *field* to
//      an existing variant fails to compile here.
//   3. EVERY BOUND FIELD IS USED — `#[deny(unused_variables)]`, so a child
//      field that is destructured but never forwarded fails to compile.
//
// Fields that genuinely are not child expressions are written `field: _`,
// which is an explicit acknowledgement rather than a silent skip.
//
// ⚠ WHAT THESE GUARDS DO NOT CATCH (state it, do not pretend otherwise):
//   - a field whose type CHANGES from non-`Expr` to `Expr`-bearing while its
//     pattern stays `field: _`;
//   - a child forwarded to the wrong callback, or forwarded under a
//     condition;
//   - a `Vec` child iterated partially;
//   - anything on the `Stmt`, `Pattern` or `Type` axes. `Expr` and `Stmt` are
//     mutually recursive, so a gap in `Stmt` loses positions just as surely.
//     `Type` is not closed over `Expr` here either — and that population is
//     not theoretical: `Type::Array`'s `size` is a real `Expr` behind a
//     `Type`, and it carries a live defect (see TODO.md, `types.rs` `_ => 0`).
// Those need the positional fixture net, not a type-level guard.

/// Apply `on_expr` to every DIRECT child expression of `e`, and `on_block` to
/// every [`Block`] it directly owns. Does NOT recurse — the caller decides
/// whether to descend, which keeps this usable by both full traversals and
/// depth-limited ones.
///
/// ⚠ The explicit `<'a>` on the callbacks is LOAD-BEARING: it is what lets a
/// caller COLLECT the children into a `Vec` and walk them afterwards. An
/// elided-lifetime signature fails to compile at such a call site (`E0521`),
/// and collect-then-walk is the only spelling available to a caller that
/// needs `&mut` state in both callbacks (two closures over the same `&mut`
/// state is `E0524`). Do not "simplify" the lifetimes away.
#[deny(unused_variables)]
pub fn visit_expr_children<'a>(
    e: &'a Expr,
    on_expr: &mut dyn FnMut(&'a Spanned<Expr>),
    on_block: &mut dyn FnMut(&'a Block),
) {
    match e {
        // ── Leaves ──
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::Identifier(_)
        | Expr::SelfExpr
        | Expr::ReturnValue
        | Expr::It
        | Expr::MetaOpToken(_) => {}
        Expr::Path { segments: _ } => {}

        // ── Single-operand wrappers ──
        Expr::Move { expr }
        | Expr::Propagate { expr }
        | Expr::MutableBorrow { expr }
        | Expr::Deref { expr }
        | Expr::ImplicitClosure { body: expr } => on_expr(expr),
        Expr::As { expr, type_: _ } => on_expr(expr),
        Expr::Await { expr, prefix_form: _ } => on_expr(expr),
        Expr::Spawn { expr, unchecked: _ } => on_expr(expr),
        Expr::SpawnBlocking { expr, unchecked: _ } => on_expr(expr),
        Expr::Is { expr, negated: _, pattern: _ } => on_expr(expr),
        Expr::UnaryOp { op: _, operand } => on_expr(operand),
        Expr::FieldAccess { object, field: _ } => on_expr(object),
        Expr::OptionalChain { object, field: _ } => on_expr(object),
        Expr::TupleFieldAccess { object, index: _ } => on_expr(object),
        Expr::Closure { is_move: _, is_async: _, params: _, body } => on_expr(body),

        // ── Two-operand ──
        Expr::BinaryOp { left, op: _, right } => {
            on_expr(left);
            on_expr(right);
        }
        Expr::MetaOpInfix { left, op_name: _, right } => {
            on_expr(left);
            on_expr(right);
        }
        Expr::DefaultOp { lhs, rhs } => {
            on_expr(lhs);
            on_expr(rhs);
        }
        Expr::Index { object, index } => {
            on_expr(object);
            on_expr(index);
        }
        Expr::Catch { expr, error_binding: _, recovery } => {
            on_expr(expr);
            on_expr(recovery);
        }
        Expr::Rethrow { expr, error_binding: _, transform } => {
            on_expr(expr);
            on_expr(transform);
        }

        // ── Calls ──
        Expr::Call { callee, generic_args: _, args } => {
            on_expr(callee);
            for a in args {
                on_expr(&a.node.value);
            }
        }
        Expr::MethodCall { receiver, method: _, generic_args: _, args } => {
            on_expr(receiver);
            for a in args {
                on_expr(&a.node.value);
            }
        }
        // ⚠ NOT a leaf: `.Ok(expr)` carries real argument expressions.
        Expr::DotShorthand { variant: _, args } => {
            for a in args {
                on_expr(&a.node.value);
            }
        }

        // ── Literals with children ──
        Expr::ArrayLiteral(items, _) => {
            for i in items {
                on_expr(i);
            }
        }
        Expr::TupleLiteral(items) => {
            for i in items {
                on_expr(i);
            }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                on_expr(k);
                on_expr(v);
            }
        }
        Expr::StructLiteral { name: _, generic_args: _, args } => {
            for a in args {
                on_expr(a);
            }
        }
        // ⚠ The pre-parsed interpolation expressions are the REAL children.
        // `walk_expr` above synthesises fake `Identifier` nodes from
        // `lit.segments` with `Span::dummy()` instead, so a visitor built on
        // it cannot see a method call inside `f"{...}"`. That is the
        // "arm exists but under-recurses" hole a no-catch-all `match` cannot
        // detect on its own — which is why guard 3 exists.
        Expr::StringLiteral(_, interps) => {
            for i in interps {
                on_expr(i);
            }
        }

        // ── Control-flow expressions ──
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            on_expr(condition);
            on_expr(then_branch);
            for (c, b) in elif_branches {
                on_expr(c);
                on_expr(b);
            }
            if let Some(eb) = else_branch {
                on_expr(eb);
            }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            on_expr(scrutinee);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    on_expr(g);
                }
                on_expr(&arm.body);
            }
            if let Some(eb) = else_arm {
                on_expr(eb);
            }
        }
        Expr::Range { start, end, inclusive: _, colon: _ } => {
            if let Some(s) = start {
                on_expr(s);
            }
            if let Some(en) = end {
                on_expr(en);
            }
        }

        // ── Comprehensions ──
        Expr::ListComprehension { expr, variable: _, ownership: _, iterable, condition } => {
            on_expr(iterable);
            on_expr(expr);
            if let Some(c) = condition {
                on_expr(c);
            }
        }
        Expr::SetComprehension { expr, variable: _, iterable, condition } => {
            on_expr(iterable);
            on_expr(expr);
            if let Some(c) = condition {
                on_expr(c);
            }
        }
        Expr::DictComprehension { key, value, variables: _, iterable, condition } => {
            on_expr(iterable);
            on_expr(key);
            on_expr(value);
            if let Some(c) = condition {
                on_expr(c);
            }
        }

        // ── Block-bearing ──
        Expr::Block(block) => on_block(block),
        Expr::Do { body, author_spelled: _ } => on_block(body),
    }
}
