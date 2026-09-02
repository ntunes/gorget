# WriteThrough places and Move

> **STUB (phase 0).** Rule stated; full prose is phase 1.

**Rule (RFC §2.2, bullet 3).** **`WriteThrough`** places (the `&` sigil) **alias
the owner**; a write lands directly on the owner with no materialize (materialize
is a no-op for an already-owning-through binding). **`Move`** (the `^` sigil;
`!` is the pre-D27 alias) **transfers** the value and **kills the source**: the
source slot becomes logically dead, and any later read of it is `IllFormed`
(RFC §2.3) — the statically-ill-formed "read of a moved-out slot" detected
dynamically. A `^` applies only to a **whole value** (`^m`, `^self`). A field
or index place (`^m.a`, `^v[i]`, `^self.items`) is `E_PartialMove` — a live
value has no holes, and consuming every field in one call is not an exception.

Modes are **elaboration-resolved tags** from syntax (bare → `Borrow`, `&` →
`WriteThrough`, `!` → `Move`); **GGC never re-infers a mode**. A `Move` emits a
`Move` event (source killed); a write through a `WriteThrough` binding emits a
`Write` event on the owner (RFC §2.7).

<!-- cites: eval.rs::resolve_write -->
<!-- cites: eval.rs::kill_place -->
<!-- cites: eval.rs::eval_source_to_slot -->

**Related:** decisions.md C3 (`&` of an owned root writes through with no
materialize); the two smith adjudications exercise `Move` (`String !p` concat →
`ablog`) and the fresh-temp/aliasing split.
