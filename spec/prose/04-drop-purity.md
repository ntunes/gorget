# Drop-purity (D4)

> **STUB (phase 0).** Rule stated; full prose is phase 1.

**Rule (RFC §2.2, bullet 4; decisions.md D4).** Types carry a **transitive
`drop_tainted` bit** computed at elaboration: a type is tainted iff it has a
custom `Drop` anywhere in its field / element graph. **Elaboration REJECTS all
SIX implicit-copy positions** (bind, ctor/field-init, collection put, return,
closure capture, and materialize-on-write — see
[`01`](01-implicit-copy-positions.md)) for tainted types **when the source is a
LIVE PLACE**. Fresh temps still move and are never rejected (the ledger's
"Box-identical" pin). The rejection is the `E_MoveWithoutOperator` family, with
the `!`-move / `.clone()` / `&` fix-it — so `ggdef` never implicitly copies a
tainted value.

**Custom drops run at scope exit in reverse declaration order**; drop count /
order for tainted types is **normative and byte-tested**. A pleasing consequence:
because tainted materialize is rejected, any binding that DOES conditionally
materialize is drop-pure, so its scope-exit drop is unobservable — the observable
semantics needs no dynamic drop-flags.

A scope-exit drop emits a `Drop` event (RFC §2.7). The rejection is centralized
in ONE elaboration helper called at all six positions.

<!-- cites: elaborate::reject_if_tainted_live_place -->
<!-- cites: elaborate::compute_taint -->
<!-- cites: elaborate::ty_tainted -->
<!-- cites: eval.rs::drop_scope -->
<!-- cites: eval.rs::run_custom_drop -->

**Related:** decisions.md D4; RFC §2.2's prerequisite note (collection-element
custom-Drop loss on named-local push — a production HIGH). ⚠ `ggdef`'s
`run_custom_drop` is currently **top-level only** (does not enumerate droppable
FIELDS / COLLECTION ELEMENTS of the dropped value) — a filed phase-1 MUST before
drop-count spectests can gate implementations; see
[`../ggdef/reports/phase0_completion.md`](../ggdef/reports/phase0_completion.md).
