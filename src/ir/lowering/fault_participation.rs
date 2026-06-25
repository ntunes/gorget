//! Cross-frame fault-propagation participation analysis (error-model.md §11,
//! Increment 2.1a/2.1c/2.1d — C + LLVM, single hop, faults
//! `Fault.Overflow` + `Fault.DivByZero` + `Fault.Bounds`).
//!
//! A function PARTICIPATES in cross-frame fault propagation iff:
//!   (a) its body has a reachable-uncaught faultable op (an integer
//!       Add/Sub/Mul/Div/Rem OR an array index read `v[i]`, NOT lexically
//!       inside a local `FaultCatch` that catches the fault), AND
//!   (b) it is directly called from inside a `FaultCatch` scope that catches
//!       the fault (so a deep fault would actually need to reach a handler).
//!
//! The INTERSECTION (a) ∩ (b) is the participating set. This is what bounds
//! the blast radius of the uniform-signature design (D5): a participating
//! callee gets a synthesized trailing `MutPtr<i32>` fault-slot param, and
//! EVERY direct caller must pass the trailing arg. Marking every
//! fault-bearing function would change thousands of signatures across
//! the suite; the intersection limits it to exactly the deep-catch callees.
//!
//! 2.1a is single-hop and conservative: condition (b) only follows DIRECT
//! calls inside the catch (no transitive closure — that is 2.2). The detector
//! over-approximates (a) (any uncaught faultable op — arithmetic or array
//! index read) which is sound: a function flagged but never deep-caught
//! simply isn't in the intersection.
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

/// Does this `FaultCatch` pattern catch any PROPAGATABLE fault (`Fault.Overflow`,
/// `Fault.DivByZero`, or `Fault.Bounds`)?
///
/// `catch Fault.Overflow:` / `catch Fault.DivByZero:` / `catch Fault.Bounds:`
/// (variant form) each catch exactly their category. `catch f:` (binding form)
/// catches ANY fault — all three categories. A catch scope makes a callee
/// participate iff that callee can raise a fault the scope catches; the
/// per-category op detector (below) is what discriminates which fault.
fn pattern_catches_fault(pattern: &FaultCatchPattern) -> bool {
    match pattern {
        FaultCatchPattern::Variant { variant, .. } => {
            matches!(variant.node.as_str(), "Overflow" | "DivByZero" | "Bounds")
        }
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

/// Visitor (a): does a function body contain a faultable op (an arithmetic
/// Add/Sub/Mul/Div/Rem OR an `array[index]` read) NOT inside a local
/// `FaultCatch` scope? `catch_depth` counts enclosing fault-catching scopes
/// (Overflow / DivByZero / Bounds, or a binding catch); a faultable op at
/// depth 0 is uncaught. Over-approximates: a flagged fn that is never
/// deep-caught simply isn't in the intersection (sound — the gate's
/// per-category resolution re-panics any uncaught category, §3).
struct UncaughtFaultDetector {
    catch_depth: usize,
    found: bool,
}

impl ExprVisitor for UncaughtFaultDetector {
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
            // An `object[index]` read is a faultable op (Bounds) — it may be
            // out of range. (The cross-frame Bounds mechanism, 2.1d, routes the
            // callee's OOB to its bounds-return block.) Conservatively counts
            // ANY index read; the GIR `bounds_handler_for` gate narrows the
            // ACTUAL faultable lowering to ARRAY element reads at the
            // type-resolved site (dict/string/range index never lowers to a
            // `FaultableIndexLoad`), so over-flagging a non-array index here is
            // harmless (an unused slot/return block, DCE'd).
            Expr::Index { object, index } => {
                if self.catch_depth == 0 {
                    self.found = true;
                    return;
                }
                self.visit_expr(object);
                self.visit_expr(index);
            }
            Expr::FaultCatch { expr: inner, pattern, handler } => {
                // The wrapped expr is inside the catch scope IFF the pattern
                // catches a fault; the handler runs OUTSIDE the caught scope.
                if pattern_catches_fault(pattern) {
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
/// `FaultCatch` scope (catching any propagatable fault). Only `Expr::Call` on a
/// bare `Expr::Identifier` callee is a "direct call" (method/indirect calls are
/// 2.3/2.3b — out of scope).
struct DeepCatchCalleeCollector {
    catch_depth: usize,
    callees: FxHashSet<String>,
}

impl ExprVisitor for DeepCatchCalleeCollector {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::FaultCatch { expr: inner, pattern, handler } => {
                if pattern_catches_fault(pattern) {
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
/// uncaught faultable op (arithmetic or array index read) and (b) functions
/// directly called from a `FaultCatch` scope (Overflow, DivByZero, or Bounds)
/// anywhere in the module.
///
/// Generic functions are EXCLUDED (generics are 2.3 — their monomorphized
/// instances aren't named at AST scan time anyway).
pub fn compute_participating_fault_fns(
    items: &[Spanned<Item>],
) -> FxHashSet<String> {
    // Phase (b): collect every direct callee inside a Fault catch
    // (Fault.Overflow, Fault.DivByZero, or Fault.Bounds).
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
    // uncaught faultable op (arithmetic or array index read).
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
            if function_has_uncaught_fault(func) {
                participating.insert(name.clone());
            }
        }
    }
    participating
}

/// Whether a single function body contains a reachable-uncaught faultable op
/// (an arithmetic op OR an `array[index]` read — condition (a)).
fn function_has_uncaught_fault(func: &FunctionDef) -> bool {
    let mut detector = UncaughtFaultDetector {
        catch_depth: 0,
        found: false,
    };
    walk_body(&mut detector, &func.body);
    detector.found
}
