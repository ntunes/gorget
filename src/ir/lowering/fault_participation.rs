//! Cross-frame fault-propagation participation analysis (error-model.md §11,
//! Increment 2.1a — C backend, single hop, `Fault.Overflow`).
//!
//! A function PARTICIPATES in cross-frame fault propagation iff:
//!   (a) its body has a reachable-uncaught faultable arithmetic op (an
//!       integer Add/Sub/Mul/Div/Rem NOT lexically inside a local
//!       `FaultCatch` that catches the fault), AND
//!   (b) it is directly called from inside a `FaultCatch` scope that catches
//!       the fault (so a deep fault would actually need to reach a handler).
//!
//! The INTERSECTION (a) ∩ (b) is the participating set. This is what bounds
//! the blast radius of the uniform-signature design (D5): a participating
//! callee gets a synthesized trailing `MutPtr<i32>` fault-slot param, and
//! EVERY direct caller must pass the trailing arg. Marking every
//! arithmetic-bearing function would change thousands of signatures across
//! the suite; the intersection limits it to exactly the deep-catch callees.
//!
//! 2.1a is single-hop and conservative: condition (b) only follows DIRECT
//! calls inside the catch (no transitive closure — that is 2.2). The detector
//! over-approximates (a) (any uncaught arithmetic op, on any integer type)
//! which is sound: a function flagged but never deep-caught simply isn't in
//! the intersection.
//!
//! This is a TYPED flag set at the source (devbook/24 rule 2), stored in
//! `LoweringContext::participating_fault_fns` and read via
//! `participates_in_fault` — never re-derived from a name.

use rustc_hash::FxHashSet;

use crate::parser::ast::{
    BinaryOp, Block, Expr, FaultCatchPattern, FunctionBody, FunctionDef, Item,
};
use crate::parser::visitor::{walk_expr, ExprVisitor};
use crate::span::Spanned;

/// Does this `FaultCatch` pattern catch `Fault.Overflow`?
///
/// `catch Fault.Overflow:` (variant form) catches exactly Overflow.
/// `catch f:` (binding form) catches ANY fault — including Overflow.
/// Any other variant (`Fault.DivByZero`, `Fault.Bounds`) does NOT catch
/// Overflow, so an overflow inside it stays uncaught (2.1a ships Overflow
/// only; DivByZero/Bounds deep propagation is 2.1c).
fn pattern_catches_overflow(pattern: &FaultCatchPattern) -> bool {
    match pattern {
        FaultCatchPattern::Variant { variant, .. } => variant.node == "Overflow",
        FaultCatchPattern::Binding(_) => true,
    }
}

/// Is this binary op a faultable arithmetic op (can overflow)?
/// Add/Sub/Mul/Div/Rem on integers trap on overflow. The `+%`/`-%`/`*%` wrap
/// ops, Mod, bitwise, shifts, and comparisons never overflow-trap.
fn is_faultable_arith(op: BinaryOp) -> bool {
    matches!(
        op,
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem
    )
}

/// Visitor (a): does a function body contain an arithmetic op NOT inside a
/// local `FaultCatch`-Overflow scope? `catch_depth` counts enclosing
/// overflow-catching scopes; an arithmetic op at depth 0 is uncaught.
struct UncaughtArithDetector {
    catch_depth: usize,
    found: bool,
}

impl ExprVisitor for UncaughtArithDetector {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        if self.found {
            return; // short-circuit
        }
        match &expr.node {
            Expr::BinaryOp { op, left, right } if is_faultable_arith(*op) => {
                if self.catch_depth == 0 {
                    self.found = true;
                    return;
                }
                // Operands themselves are evaluated in the same scope.
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::FaultCatch { expr: inner, pattern, handler } => {
                // The wrapped expr is inside the catch scope IFF the pattern
                // catches Overflow; the handler runs OUTSIDE the caught scope.
                if pattern_catches_overflow(pattern) {
                    self.catch_depth += 1;
                    self.visit_expr(inner);
                    self.catch_depth -= 1;
                } else {
                    self.visit_expr(inner);
                }
                self.visit_expr(handler);
            }
            _ => walk_expr(self, expr),
        }
    }
}

/// Visitor (b): collect direct-call callee names that appear inside a
/// `FaultCatch`-Overflow scope. Only `Expr::Call` on a bare
/// `Expr::Identifier` callee is a "direct call" (method/indirect calls are
/// 2.3/2.3b — out of scope).
struct DeepCatchCalleeCollector {
    catch_depth: usize,
    callees: FxHashSet<String>,
}

impl ExprVisitor for DeepCatchCalleeCollector {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::FaultCatch { expr: inner, pattern, handler } => {
                if pattern_catches_overflow(pattern) {
                    self.catch_depth += 1;
                    self.visit_expr(inner);
                    self.catch_depth -= 1;
                } else {
                    self.visit_expr(inner);
                }
                // The handler is OUTSIDE the caught scope.
                self.visit_expr(handler);
            }
            Expr::Call { callee, args, .. } => {
                if self.catch_depth > 0 {
                    if let Expr::Identifier(name) = &callee.node {
                        self.callees.insert(name.clone());
                    }
                }
                // Recurse into callee + args (nested calls / fault-catches).
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.node.value);
                }
            }
            _ => walk_expr(self, expr),
        }
    }
}

/// Walk a function body with `visitor`.
fn walk_body<V: ExprVisitor>(visitor: &mut V, body: &FunctionBody) {
    match body {
        FunctionBody::Block(block) => walk_block_with(visitor, block),
        FunctionBody::Expression(e) => visitor.visit_expr(e),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn walk_block_with<V: ExprVisitor>(visitor: &mut V, block: &Block) {
    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }
}

/// Compute the participating-function set over all non-generic functions in
/// the AST module. Returns the intersection of (a) functions with an
/// uncaught faultable arithmetic op and (b) functions directly called from a
/// `FaultCatch`-Overflow scope anywhere in the module.
///
/// Generic functions are EXCLUDED (generics are 2.3 — their monomorphized
/// instances aren't named at AST scan time anyway).
pub fn compute_participating_fault_fns(
    items: &[Spanned<Item>],
) -> FxHashSet<String> {
    // Phase (b): collect every direct callee inside a Fault.Overflow catch.
    let mut deep_callees = DeepCatchCalleeCollector {
        catch_depth: 0,
        callees: FxHashSet::default(),
    };
    for item in items {
        if let Item::Function(func) = &item.node {
            walk_body(&mut deep_callees, &func.body);
        }
    }
    if deep_callees.callees.is_empty() {
        // No deep-catch call site anywhere → nothing participates. Fast path:
        // the entire existing suite + self-host take this branch (they have no
        // `catch Fault.X` over a user CALL), so signatures are unchanged.
        return FxHashSet::default();
    }

    // Phase (a) ∩ (b): a candidate participates only if its OWN body has an
    // uncaught faultable arithmetic op.
    let mut participating = FxHashSet::default();
    for item in items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // generics are 2.3
            }
            let name = &func.name.node;
            if !deep_callees.callees.contains(name) {
                continue;
            }
            if function_has_uncaught_arith(func) {
                participating.insert(name.clone());
            }
        }
    }
    participating
}

/// Whether a single function body contains a reachable-uncaught faultable
/// arithmetic op (condition (a)).
fn function_has_uncaught_arith(func: &FunctionDef) -> bool {
    let mut detector = UncaughtArithDetector {
        catch_depth: 0,
        found: false,
    };
    walk_body(&mut detector, &func.body);
    detector.found
}
