//! Provenance trace events (RFC §2.7).
//!
//! Every ownership-relevant event during evaluation emits one of these. They
//! are the raw material for `gg explain` (phase 2) and for divergence
//! debugging, and — per RFC §2.3 — they accompany **all four** evaluator
//! outcomes (Value/Trap/IllFormed/FuelExhausted), not just successful runs.
//!
//! The six event kinds map one-to-one onto the §2.2 mechanics:
//!   * `BindCopy`      — a live-place copy at an implicit-copy position.
//!   * `Move`          — ownership transferred out of a place (source killed),
//!                       or a fresh temp moved into a slot (structural move).
//!   * `ExplicitClone` — an explicit `.clone()` deep copy.
//!   * `Materialize`   — first write through a `Borrow` binding privatised it.
//!   * `Write`         — a write landed on a place (assign / mutating method).
//!   * `Drop`          — a scope-exit drop (reverse declaration order).
//!
//! The place is recorded as a human-readable path string (e.g. `b`, `a.text`,
//! `v[0]`). A richer structured provenance can come later; a string is enough
//! to read a JSONL trace and to unit-test drop order.

use gorget::span::Span;

/// One ownership-relevant event, with its source span and place provenance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceEvent {
    /// A live-place value was copied at an implicit-copy position.
    BindCopy { place: String, span: Span },
    /// Ownership moved out of `from` (source now dead), or a fresh temp moved
    /// into a binding (`from` names the destination for a structural move).
    Move { from: String, span: Span },
    /// An explicit `.clone()` produced a deep copy.
    ExplicitClone { place: String, span: Span },
    /// The first write through a `Borrow` binding privatised it.
    Materialize { place: String, span: Span },
    /// A write landed on a place.
    Write { place: String, span: Span },
    /// A scope-exit drop.
    Drop { place: String, span: Span },
}

impl TraceEvent {
    /// The event kind, as a stable lowercase tag (used in the JSONL output).
    pub fn kind(&self) -> &'static str {
        match self {
            TraceEvent::BindCopy { .. } => "bind_copy",
            TraceEvent::Move { .. } => "move",
            TraceEvent::ExplicitClone { .. } => "explicit_clone",
            TraceEvent::Materialize { .. } => "materialize",
            TraceEvent::Write { .. } => "write",
            TraceEvent::Drop { .. } => "drop",
        }
    }

    /// The place / source path this event concerns.
    pub fn place(&self) -> &str {
        match self {
            TraceEvent::BindCopy { place, .. }
            | TraceEvent::ExplicitClone { place, .. }
            | TraceEvent::Materialize { place, .. }
            | TraceEvent::Write { place, .. }
            | TraceEvent::Drop { place, .. } => place,
            TraceEvent::Move { from, .. } => from,
        }
    }

    fn span(&self) -> Span {
        match self {
            TraceEvent::BindCopy { span, .. }
            | TraceEvent::Move { span, .. }
            | TraceEvent::ExplicitClone { span, .. }
            | TraceEvent::Materialize { span, .. }
            | TraceEvent::Write { span, .. }
            | TraceEvent::Drop { span, .. } => *span,
        }
    }

    /// Render this event as a single JSON object (one JSONL line).
    ///
    /// Hand-rolled rather than via `serde` to honour the "no new deps" rule —
    /// the shape is trivial and the strings are short.
    pub fn to_json(&self) -> String {
        let span = self.span();
        format!(
            "{{\"event\":\"{}\",\"place\":\"{}\",\"span\":[{},{}]}}",
            self.kind(),
            escape_json(self.place()),
            span.start,
            span.end,
        )
    }
}

/// Escape a string for embedding in a JSON string literal.
fn escape_json(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out
}
