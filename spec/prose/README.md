# `spec/prose/` — the spec prose leg of the triad

Section-per-construct prose that cross-cites `ggdef`'s `eval.rs` (HaMLet-style:
the prose states the rule; the `<!-- cites: eval.rs::<fn> -->` comment pins the
exact code that IS the rule). The prose and `ggdef` are merge-gated together
(RFC §5.1) so they can never drift.

> **Phase-0 status:** these are **stubs** (RFC §6, Increment C deliverable 3).
> Each file states its §2.2 rule in one paragraph and cross-cites the governing
> `eval.rs` / elaboration function. Full normative prose — the formatting
> appendix, the trap-normalization text, worked examples, the diagnostic-code
> registry — is phase 1.

## The §2.2 ownership model, one file per bullet

| File | §2.2 bullet | Primary cite |
|---|---|---|
| [`01-implicit-copy-positions.md`](01-implicit-copy-positions.md) | implicit-copy positions (closed set) + fresh-temp Move | `eval.rs::eval_source_to_slot` |
| [`02-borrow-and-materialize-on-write.md`](02-borrow-and-materialize-on-write.md) | Borrow = view on read; write MATERIALISES; `self`/match/for are bare | `eval.rs::resolve_write` |
| [`03-writethrough-and-move.md`](03-writethrough-and-move.md) | `WriteThrough` aliases the owner; `Move` kills the source | `eval.rs::resolve_write` |
| [`04-drop-purity.md`](04-drop-purity.md) | D4 drop-taint: reject the six positions; custom drops in reverse order | `elaborate::reject_if_tainted_live_place` |
| [`05-resource-exhaustion.md`](05-resource-exhaustion.md) | stack/OOM impl-defined; `ggdef` total via fuel | `eval.rs::run` |

Related normative pieces that get their own prose sections in phase 1: the four
evaluator outcomes (RFC §2.3), the formatting appendix (D8 shortest round-trip),
and trap normalization (RFC §4).

The **diagnostic-code registry** (RFC §5.5) is now authored:
[`diagnostic-codes.md`](diagnostic-codes.md) maps each stable `E_`/`W_` code to
its diagnostic kind and prose section.

The **trap-code registry** (RFC §4; D11 trap normalization) is now authored:
[`trap-codes.md`](trap-codes.md) maps each stable `T_` code to its trap class.
All traps are uncatchable — they render `trap[T_X]: detail at file:line:col` and
exit 101; conformance compares the `T_` code + exit only.
