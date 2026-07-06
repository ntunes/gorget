# `spectests/annexe/` — the copy-guarantees annexe (D1)

The normative counterpart to value semantics: a small **CLOSED** list of
MUST-NOT-ALLOCATE positions (bare bind / read / param-pass, borrow field/element
read — README:50). These fixtures make the language's zero-copy promises testable
spec (D1's second half).

Tested **implementation-side**, not by the value-semantics evaluator: under
`--clones=stats` / a counting allocator, an annexe fixture asserts a clone/alloc
count, so an eager-copy-everything implementation is nonconformant by
construction. `ggdef`'s side is the trace: it tags the annexe positions as
no-copy events (RFC §2.7, §5.4).

v1 gates the **C backend** only (LLVM rejects `--clones=stats`, self-host has only
transient instrumentation — both are floor-tracked debt).

**Empty in phase 0** (the W3a–d String-shape clone-count fixtures land here — RFC §6).
