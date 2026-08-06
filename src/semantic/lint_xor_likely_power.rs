//! D28 XOR-fix-it lint (`W_XorLikelyPower`).
//!
//! Detects the newcomer trap where a user writes `2 ^ 10` intending
//! `2 ** 10` — `^` is XOR (bitwise) in Gorget, not power. The warning is
//! narrowed to GCC-12's `-Wxor-used-as-pow` shape per ledger
//! `docs/define-gorget/decisions.md:959-960` — the broader
//! `literal ^ literal` form is EXPLICITLY REJECTED because it false-fires
//! on canonical XOR fixtures like `0xff ^ 0x0f`, `3 ^ 5`, `7 ^ 11`.
//!
//! Fire only when ALL of:
//! - LHS is `Expr::IntLiteral` with value in the canonical-power-base set
//!   `{2, 10}` (no one writes `3 ^ 5` meaning power).
//! - RHS is `Expr::IntLiteral` with value in the plausible-exponent
//!   range `0..=63` (a `2 ^ 100` might be an intentional bitmask against
//!   a large sentinel).
//! - Both operand literals are non-negative.
//!
//! Non-fatal warning — the code is legitimate XOR too.

use crate::parser::ast::{BinaryOp, Expr, FunctionBody, Item, Module};
use crate::parser::visitor::{walk_expr, ExprVisitor};
use crate::semantic::errors::{SemanticWarning, SemanticWarningKind};
use crate::span::Spanned;

/// Run the XOR-fix-it lint over every expression in `module`.
pub fn check_module(module: &Module, warnings: &mut Vec<SemanticWarning>) {
    let mut visitor = XorVisitor { warnings };
    for item in &module.items {
        visit_item(&mut visitor, &item.node);
    }
}

fn visit_item(visitor: &mut XorVisitor<'_>, item: &Item) {
    match item {
        Item::Function(f) => visit_function(visitor, f),
        Item::Equip(eb) => {
            for m in &eb.items {
                visit_function(visitor, &m.node);
            }
        }
        Item::Module { items, .. } => {
            for m in items {
                visit_item(visitor, &m.node);
            }
        }
        _ => {}
    }
}

fn visit_function(visitor: &mut XorVisitor<'_>, func: &crate::parser::ast::FunctionDef) {
    match &func.body {
        FunctionBody::Block(block) => {
            for stmt in &block.stmts {
                visitor.visit_stmt(stmt);
            }
        }
        FunctionBody::Expression(expr) => {
            visitor.visit_expr(expr);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

struct XorVisitor<'a> {
    warnings: &'a mut Vec<SemanticWarning>,
}

impl<'a> ExprVisitor for XorVisitor<'a> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        if let Expr::BinaryOp { op: BinaryOp::BitXor, left, right } = &expr.node {
            if let (Expr::IntLiteral(l), Expr::IntLiteral(r)) = (&left.node, &right.node) {
                if is_narrow_xor_likely_power(*l, *r) {
                    self.warnings.push(SemanticWarning {
                        kind: SemanticWarningKind::XorLikelyPower { lhs: *l, rhs: *r },
                        span: expr.span,
                    });
                }
            }
        }
        // Recurse into sub-expressions.
        walk_expr(self, expr);
    }
}

/// Narrow to GCC-12 `-Wxor-used-as-pow` shape (ledger `:959-960`).
fn is_narrow_xor_likely_power(lhs: i64, rhs: i64) -> bool {
    // LHS must be a canonical power base and RHS a plausible exponent, both
    // non-negative — otherwise it's legitimate XOR.
    (lhs == 2 || lhs == 10) && (0..=63).contains(&rhs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn narrow_shape_fires() {
        assert!(is_narrow_xor_likely_power(2, 10));  // classic
        assert!(is_narrow_xor_likely_power(10, 3));  // 10-base
        assert!(is_narrow_xor_likely_power(2, 0));   // exp = 0
        assert!(is_narrow_xor_likely_power(2, 63));  // exp = 63
    }

    #[test]
    fn broad_shape_does_not_fire() {
        // Non-canonical bases — ledger `:959-960` REJECTS the broad form.
        assert!(!is_narrow_xor_likely_power(3, 5));
        assert!(!is_narrow_xor_likely_power(7, 11));
        assert!(!is_narrow_xor_likely_power(0xff, 0x0f)); // canonical XOR
        // Exponent out of range.
        assert!(!is_narrow_xor_likely_power(2, 100));
        assert!(!is_narrow_xor_likely_power(2, 64));
        // Negative operands.
        assert!(!is_narrow_xor_likely_power(2, -1));
        assert!(!is_narrow_xor_likely_power(-2, 10));
    }
}
