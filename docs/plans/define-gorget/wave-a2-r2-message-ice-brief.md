# Wave A2-R2 brief — D12 riders: position-aware diagnostics + the compound-assign ICE fix

> **Split from A2-R1 by pass-5's ruling (2026-07-11).** Two un-prototyped items in
> zones DISJOINT from A2-R1's semantic-safety core. Sequenced AFTER A2-R1 lands.
> **Status:** v0 STUB — needs its own scout (the ICE rider's lowering write-site
> re-verify) → brief completion → ≥3 review passes → executor.

## Item 1 — the position/shape-aware `E_MoveWithoutOperator` message (A2-R brief pin-4)

Extend the `MoveWithoutOperator` variant (`errors.rs:438`, currently
`{ name: String }`) with reason/position(+shape) fields, rendered under the SAME
E_ code. Update ALL construction sites (8-9 post-A2-R1 — enumerate at execution;
baseline 2 = `check_expr.rs:33`, `check_stmt.rs:1457`), the Display arm
(`errors.rs:965` — REMOVE the dead `` `move` `` alternative, confirmed present),
`errors.rs:1217`, and the `safety/tests.rs` matchers. Message content per the
A2-R1 brief's pin-4: name the drop-taint cause; whole-identifier places →
`!x`/`.clone()`; field/index places → `.clone()` ONLY; captures → pass-as-arg /
`Shared[T]` ONLY. **GATE: the rendered capture-position message contains no `!`.**

## Item 2 — the compound-assign ICE fix (`v[i] += x` moves the dead element)

**CORRECTED ANCHORS (pass-5 verified each against source + reproduced the ICE —
the earlier citations pointed at non-existent files):**
- `lower_compound_assign` = `src/ir/lowering/stmts/assigns.rs:1148` (NOT
  `src/ir/lowering/assigns.rs`).
- `index_load_borrow` = defined at `src/ir/builder.rs:258` (NOT `validate.rs`).
- The live panic = `src/ir/lowering/mod.rs:1763` ("shallow copy of resource _8 :
  Acc") — reproduced with the A2-R1 prototype applied (`gg check` passes,
  `gg build` panics; the taint check does NOT mask it, correctly, matching
  ggdef's accept of owned-local compound writes).
- The operator-overload sibling panics: `stmts/assigns.rs:1129` and `:1775`.

Design (from the A2 scout, unchanged): replace the shared vector/dict
clone-read+shallow-assign branch with `index_load_borrow` for the `self` borrow;
`__set`'s pre-drop gives drop-once. Closes the two TODO entries: the 🐛💥
Compound-assign resource-element ICE + the op-overload validator-panic sibling.
Do NOT close the D12 parent entry (A2-S remains). Gates: a NEW tainted-compound
fixture + ASan + ALL non-tainted compound fixtures byte-identical
(`ls tests/fixtures | grep compound`, ~20 files, both binaries).
