//! The typed author-grouping-paren trivia table.
//!
//! A grouping paren the author wrote — `(y % 4) == 0`, `(a + b) + c` — carries
//! no semantics: `parse_paren_expr` returns the INNER expression unchanged, so
//! by the time any pass sees the AST the paren is gone. That makes the parser
//! the only place in the compiler that knows it was ever there, and this table
//! the way it says so.
//!
//! **Sideband, not an AST node.** An `Expr::Paren` wrapper would have to be
//! seen through by every semantic pass, and the one pass that forgot would
//! mis-analyse the expression underneath it. The same reasoning that keeps
//! comments out of the AST (see [`super::comments`]) keeps parens out: the
//! formatter is the only consumer.
//!
//! **What the key can and cannot collide with.** Entries are keyed by the
//! wrapped expression's span, and a span is unique to a source range within
//! one parse — with one apparent exception, f-string interpolation
//! expressions, whose spans are synthetic. Those cannot enter this table at
//! all: interpolation bodies are parsed by a SEPARATE `Parser` built with
//! [`super::Parser::new_with_offset`], whose own table is dropped with it. The
//! synthetic offset space (`interp_base`, at `1 << 40`) is the second fence,
//! disjoint from every real source offset by construction. Structural parens —
//! call-argument lists, tuples, closure parameter lists, patterns, types —
//! never reach the recording site, which is `parse_paren_expr`'s
//! parenthesized-EXPRESSION branch alone.

use std::collections::HashMap;

use crate::span::Span;

/// How many grouping-paren layers the author wrote around each expression.
///
/// Built ONCE, at the end of a parse, from the parser's push-log. The log is a
/// `Vec` rather than this map precisely because speculation must be able to
/// unwind it: `Parser::try_parse` truncates the log back to its saved length
/// when a speculative parse is abandoned, and a count map cannot be truncated
/// to a length.
#[derive(Debug, Clone, Default)]
pub struct AuthorParenTable {
    layers: HashMap<(usize, usize), usize>,
}

impl AuthorParenTable {
    /// The table a formatter gets when there is no parse behind it.
    pub fn empty() -> Self {
        Self {
            layers: HashMap::new(),
        }
    }

    /// Fold the parser's push-log into per-span layer counts.
    ///
    /// Nested parens push the SAME inner span once per layer (`((a))` pushes
    /// `a`'s span twice), so the layer count falls out of the multiplicity —
    /// no separate depth bookkeeping.
    pub fn build(spans: &[Span]) -> Self {
        let mut layers: HashMap<(usize, usize), usize> = HashMap::new();
        for span in spans {
            *layers.entry((span.start, span.end)).or_insert(0) += 1;
        }
        Self { layers }
    }

    /// How many grouping-paren layers the author wrote around the node at
    /// `span`. Zero for every span the parser did not record — which includes
    /// every synthetic span and every paren that belonged to a call, a tuple,
    /// or a closure parameter list.
    pub fn layers(&self, span: Span) -> usize {
        self.layers
            .get(&(span.start, span.end))
            .copied()
            .unwrap_or(0)
    }

    /// Number of distinct spans carrying at least one layer.
    pub fn len(&self) -> usize {
        self.layers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }
}
