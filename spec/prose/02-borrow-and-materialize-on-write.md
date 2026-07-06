# Borrow positions and materialize-on-write

> **STUB (phase 0).** Rule stated; full prose is phase 1.

**Rule (RFC §2.2, bullet 2).** `Borrow` positions — **bare params, reads,
receivers** — are **non-owning views for ALL types on READ**: no copy, no drop in
the borrower, for reading. **A WRITE through a `Borrow` binding MATERIALIZES**: at
the first write, the binding becomes a **persistent private copy** holding the
pre-write value with the write applied; all subsequent reads AND writes through
that binding see the copy; **the owner is untouched**; the copy drops in the
borrower's scope. This is the language's core CoW rule (language-design §3.1)
stated as eager value semantics — it is what makes the `deadwrite_*` family and
D2 evaluable.

**`self` is a bare binding (D2):** a write through plain `self` materializes
exactly as above; `&self` is the write-through opt-in (see
[`03`](03-writethrough-and-move.md)). **Match pattern bindings and `for`-loop
variables are Borrow-mode bindings** (views of the scrutinee / element;
materialize-on-write applies).

The first materializing write emits a `Materialize` event; the write itself emits
a `Write` event (RFC §2.7).

<!-- cites: eval.rs::resolve_write -->
<!-- cites: eval.rs::resolve_read -->
<!-- cites: eval.rs::push_pattern_bindings -->

**Related:** decisions.md D2 (plain-`self` = uniform CoW), C4 (mutating method on
`param.field` via bare param materializes the root). ⚠ Production holes surfaced
by `ggdef` here (compound-assign write-through; materialize not persisting across
loop iterations) are filed invariant-#8 findings — see
[`../ggdef/reports/phase0_completion.md`](../ggdef/reports/phase0_completion.md).
