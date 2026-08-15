//! The typed comment-attachment table.
//!
//! Comments never reach the AST — the parser partitions them into a side
//! table that the formatter walks with a forward-only cursor. What that
//! sideband carries is the subject of this module.
//!
//! **One producer, typed fields, read through accessors.** Each entry's
//! LEXICAL facts (where the `#` is, what column it sits at, whether code
//! precedes it on its line) are written by the lexer, which has them in hand
//! at scan time; the one CROSS-ENTRY fact — whether an entry continues the
//! trailing comment above it — is derived once here, at the collection site,
//! because it is a relation between neighbours and no single token knows it.
//! Nothing downstream re-derives either kind by scanning source bytes.

use crate::lexer::token::{CommentPlacement, CommentToken};
use crate::span::Span;

/// One comment, with the attachment facts a reader needs.
#[derive(Debug, Clone)]
pub struct CommentEntry {
    /// The comment text, `#` included, to end of line.
    pub text: String,
    /// `span.start` is the `#` and `span.end` is the line's `\n` (or EOF).
    /// Both producers normalize the start, so the position means one thing.
    pub span: Span,
    /// Byte offset of this comment's own line start.
    pub line_start: usize,
    /// Character column of the `#` on its line.
    pub hash_col: usize,
    pub placement: CommentPlacement,
    /// Index of the HEAD of the multi-line trailing run this entry continues,
    /// or `None` when this entry is not a continuation.
    ///
    /// A trailing comment that runs onto the following lines — each aligned
    /// under the first `#`, no blank and no code between — is ONE logical
    /// comment. `continues` IS that concept: it is defined here, once, and
    /// every consumer asks the table instead of re-deciding.
    ///
    /// A DOC comment can never join a run: an own-line `#/` lexes as
    /// `Token::DocComment`, which is not collected into this table at all, so
    /// it breaks the line-adjacency test between the head and anything below
    /// it.
    pub continues: Option<usize>,
}

impl CommentEntry {
    /// Byte offset of the `#` itself.
    pub fn hash_pos(&self) -> usize {
        self.span.start
    }

    /// True iff nothing but whitespace precedes the `#` on its source line.
    pub fn is_own_line(&self) -> bool {
        self.placement == CommentPlacement::OwnLine
    }
}

/// The parser's comment sideband: every `Token::Comment`, in source order.
#[derive(Debug, Clone, Default)]
pub struct CommentTable {
    entries: Vec<CommentEntry>,
}

impl CommentTable {
    /// The sideband a sub-render gets: no comments to interleave.
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Build the table from the lexer's comment tokens, in source order, and
    /// derive the continuation runs.
    ///
    /// A comment CONTINUES the one above it when all four hold:
    ///   * it is an OWN-LINE comment (a trailing comment starts its own run);
    ///   * the entry above is a trailing head, or a continuation of one;
    ///   * its `#` sits at the head's `#` column;
    ///   * its line starts exactly one byte past the previous entry's end —
    ///     i.e. DIRECTLY below, with no blank line and no code line between.
    ///     A comment token's span ends AT the newline, so `line_start ==
    ///     prev.span.end + 1` is that test, spelled in typed fields.
    pub fn build(tokens: Vec<(CommentToken, Span)>) -> Self {
        let mut entries: Vec<CommentEntry> = tokens
            .into_iter()
            .map(|(c, span)| CommentEntry {
                text: c.text,
                span,
                line_start: c.line_start,
                hash_col: c.hash_col,
                placement: c.placement,
                continues: None,
            })
            .collect();
        for i in 1..entries.len() {
            if !entries[i].is_own_line() {
                continue;
            }
            let head_idx = match entries[i - 1].placement {
                CommentPlacement::Trailing => i - 1,
                CommentPlacement::OwnLine => match entries[i - 1].continues {
                    Some(h) => h,
                    None => continue,
                },
            };
            if entries[i].hash_col != entries[head_idx].hash_col {
                continue;
            }
            if entries[i].line_start != entries[i - 1].span.end + 1 {
                continue;
            }
            entries[i].continues = Some(head_idx);
        }
        Self { entries }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn get(&self, i: usize) -> Option<&CommentEntry> {
        self.entries.get(i)
    }

    /// The entries from `from` onward — the unemitted tail, for the callers
    /// that ask a question about "any comment not yet claimed".
    pub fn from(&self, from: usize) -> &[CommentEntry] {
        &self.entries[from.min(self.entries.len())..]
    }

    pub fn iter(&self) -> std::slice::Iter<'_, CommentEntry> {
        self.entries.iter()
    }
}

impl std::ops::Index<usize> for CommentTable {
    type Output = CommentEntry;

    fn index(&self, i: usize) -> &CommentEntry {
        &self.entries[i]
    }
}
