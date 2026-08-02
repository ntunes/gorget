//! Semantic-root type utilities shared across sub-modules (typecheck +
//! safety). Lives at the semantic root, above `safety/`, because it is
//! visible to BOTH the typechecker (which authoritatively rejects the
//! borrow-bind at D10(a)) AND the borrow checker (which needs to know
//! when D10(a) has already fired so it can suppress the mirror-walker
//! `E_AmpInOperandPosition` on the SAME syntax — the compound-shape
//! branches/arms/tails from Round XXV Track D §D-3).
//!
//! Round XXV Track D chose path (a) — create at the semantic root rather
//! than widen `safety::type_utils` — for cleaner layering: `safety`
//! depends on `semantic`, not the other way around.

use crate::parser::ast::{Block, Expr, MatchItem, Stmt};

/// Returns true when `expr` is (or bottoms out in a compound tail that
/// yields) an `Expr::MutableBorrow` — i.e. a syntactic borrow-bind. Used
/// at the D10(a) `E_LocalBorrowBind` decision at VarDecl inits,
/// Assign RHS, and StaticDecl initializers to reject `auto x = &b`,
/// `x = &b`, and the compound tails (`if c: &a else: &b`, `match e:
/// case P: &y`, `do: &a`, `{ ...; &a }`).
///
/// Deliberately NOT a deep walk: `&x` nested in a call (`f(&x)`) is the
/// legal call-arg form and is never visited here — this helper is only
/// invoked on VarDecl inits, assignment RHS, and static initializers.
///
/// Round XXV Track D lifted this from `TypeChecker` (was
/// `TypeChecker::expr_is_borrow_bind`) to a semantic-root helper so both
/// `TypeChecker` (authoritative D10(a) rejector) and `BorrowChecker`
/// (mirror-walker suppressor) can read the same predicate without
/// duplicating logic.
pub(crate) fn expr_is_borrow_bind(expr: &Expr) -> bool {
    match expr {
        Expr::MutableBorrow { .. } => true,
        Expr::If {
            then_branch,
            elif_branches,
            else_branch,
            ..
        } => {
            expr_is_borrow_bind(&then_branch.node)
                || elif_branches
                    .iter()
                    .any(|(_, b)| expr_is_borrow_bind(&b.node))
                || else_branch
                    .as_ref()
                    .map_or(false, |b| expr_is_borrow_bind(&b.node))
        }
        Expr::Match { arms, else_arm, .. } => {
            arms.iter()
                .any(|arm| expr_is_borrow_bind(&arm.body.node))
                || else_arm
                    .as_ref()
                    .map_or(false, |b| expr_is_borrow_bind(&b.node))
        }
        Expr::Do { body } => block_tail_is_borrow_bind(body),
        Expr::Block(block) => block_tail_is_borrow_bind(block),
        _ => false,
    }
}

/// The value of a `do:` / block expression is its TAIL statement (Gorget
/// blocks have no separate tail field — the value is the last statement
/// when it produces one). A tail whose value is a `&expr` makes the block
/// a borrow-bind. The tail may be a plain expression (`Stmt::Expr`) OR a
/// STATEMENT-FORM `if`/`match` used in value position (`do:` newline
/// `if c: &a else: &b`) — those still yield the block's value through
/// their branches/arms, so recurse them too (else `do: <stmt-match &a>`
/// dodges the check and garbage-links, `undefined reference to
/// int64_t__push`). A block whose last statement is not value-producing
/// (`return`, an assignment, a loop, …) yields no bindable borrow → false.
pub(crate) fn block_tail_is_borrow_bind(block: &Block) -> bool {
    match block.stmts.last() {
        Some(last) => match &last.node {
            Stmt::Expr(e) => expr_is_borrow_bind(&e.node),
            Stmt::If {
                then_body,
                elif_branches,
                else_body,
                ..
            } => {
                block_tail_is_borrow_bind(then_body)
                    || elif_branches
                        .iter()
                        .any(|(_, b)| block_tail_is_borrow_bind(b))
                    || else_body
                        .as_ref()
                        .map_or(false, |b| block_tail_is_borrow_bind(b))
            }
            Stmt::Match { arms, else_arm, .. } => {
                arms.iter().any(|item| {
                    let arm = match item {
                        MatchItem::Arm(a) => a,
                        MatchItem::MetaFor { arm_template, .. } => arm_template,
                    };
                    expr_is_borrow_bind(&arm.body.node)
                }) || else_arm
                    .as_ref()
                    .map_or(false, |b| block_tail_is_borrow_bind(b))
            }
            _ => false,
        },
        None => false,
    }
}
