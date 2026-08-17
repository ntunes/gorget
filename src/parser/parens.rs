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
    ends_at: HashMap<usize, usize>,
}

impl AuthorParenTable {
    /// The table a formatter gets when there is no parse behind it.
    pub fn empty() -> Self {
        Self {
            layers: HashMap::new(),
            ends_at: HashMap::new(),
        }
    }

    /// Fold the parser's push-log into per-span layer counts.
    ///
    /// Nested parens push the SAME inner span once per layer (`((a))` pushes
    /// `a`'s span twice), so the layer count falls out of the multiplicity —
    /// no separate depth bookkeeping.
    ///
    /// The second index is by END offset alone, for the consumer that has a
    /// position rather than a span (see [`Self::layers_ending_at`]). It is
    /// built here rather than derived on demand so the table stays the single
    /// source of truth for both questions.
    pub fn build(spans: &[Span]) -> Self {
        let mut layers: HashMap<(usize, usize), usize> = HashMap::new();
        let mut ends_at: HashMap<usize, usize> = HashMap::new();
        for span in spans {
            *layers.entry((span.start, span.end)).or_insert(0) += 1;
            *ends_at.entry(span.end).or_insert(0) += 1;
        }
        Self { layers, ends_at }
    }

    /// How many grouping-paren layers CLOSE at byte offset `end` — i.e. how
    /// many author `)` sit (past whitespace and comments) immediately after it.
    ///
    /// Distinct from [`Self::layers`], which is keyed by the EXACT span of the
    /// wrapped node, and the distinction is load-bearing for any consumer that
    /// holds a POSITION rather than the wrapped node's span. Two shapes make
    /// the exact-span lookup return 0 while an author `)` is still there:
    ///
    ///  * a BINARY node is spanned `lhs.merge(paren-stripped rhs)`, so for
    ///    `x + (y)` the whole-node key is `[x.start, y.end]` while the recorded
    ///    key is `[y.start, y.end]` — different keys, same END;
    ///  * a CALL ARGUMENT is a `Spanned<CallArg>` spanned from the argument's
    ///    own start, which for `f(1, (2 + 3))` is the author's `(` — again a
    ///    different key with the same END.
    ///
    /// Summing across DIFFERENT spans that share an end is correct, not an
    /// over-count: same-offset keys NEST. `(a + (b))` records `[b.start,
    /// b.end]` for the inner paren and `[a.start, b.end]` for the outer, and
    /// the source does carry two `)` after `b`.
    pub fn layers_ending_at(&self, end: usize) -> usize {
        self.ends_at.get(&end).copied().unwrap_or(0)
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
