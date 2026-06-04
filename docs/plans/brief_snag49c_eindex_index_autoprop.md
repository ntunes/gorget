# Brief — snag49c: EIndex-INDEX auto-propagation — +1, RUN-VERIFIED

A RUN-verify scout built + ran + measured this end-to-end: **379 → 380 (+1)**, exactly one fixture flips
(`snag49c_throws_index` CC-FAIL → MATCH, output `got: 30` == oracle), **ZERO regressions**, clean diff.
Trivial, idempotent, zero-risk.

## Root
The `lower.gg` EIndex value-read arm (`case EIndex(base_box, idx_box)`, ~:5895, the region the 3g fix just
touched) lowers the index expression but does NOT call `maybe_auto_propagate()` on it. So a THROWING index
`v[pick()]` (where `pick()` returns `Result[int, E]` and the enclosing fn throws) leaves the Result struct
un-propagated — its bytes are then read as the int index. `maybe_auto_propagate` is the proven auto-prop
hook already wired at 5 consuming sites (EIf cond ~:4502, call args ~:6736, EWhile ~:7690, EFor iterable
~:8988); the EIndex arm + the index-assign LHS twin were the two sites that missed it.

## The fix (3 lines, `lower.gg` only — the scout's exact RUN-verified prototype)
1. **EIndex value-read (~:5901)** — after `int eix_idx = lower_expr(...)`, add:
   `eix_idx = maybe_auto_propagate(&ctx, eix_idx, &gmod)` (re-pin by symbol — the var holding the lowered
   index operand in the EIndex arm).
2. **`lower_index_assign` LHS-write twin (~:7749/7750)** — add `idx = maybe_auto_propagate(&ctx, idx, &gmod)`
   AND `val = maybe_auto_propagate(&ctx, val, &gmod)`.

**Keep all 3** (the RUN-verified configuration that measured +1 / zero-regress). Rationale: the LHS-write
site `arr[pick()] = x` has the IDENTICAL latent bug; fixing the whole EIndex-index-auto-prop *class*
(read + write) is the principled fix (CLAUDE.md "generic solutions, not just the symptom"), and the calls
are IDEMPOTENT — `maybe_auto_propagate` is a no-op unless the operand IS a Result AND the enclosing fn
throws, so the extra calls cannot misfire (the scout confirmed zero regressions with all 3 in place).

## Reviewers verify
1. The root: the EIndex arm lowers the index but never propagates it; mirror the 5 proven sites exactly
   (`maybe_auto_propagate(&ctx, <operand>, &gmod)`, reassigning the operand var). Confirm the var names
   (`eix_idx` in the read arm; `idx`/`val` in `lower_index_assign`) against current source.
2. Idempotency: `maybe_auto_propagate` is a no-op unless operand is a Result in a throwing fn — so the
   added calls are safe on non-throwing/non-Result indices (the common case). No regression risk.
3. Scoped: edits only the EIndex-read index operand + the index-assign idx/val operands; the base operand
   and the 3g by-value struct-read machinery are UNTOUCHED.

## Gates (executor; RUN-verified baseline 379)
- Force-rebuild driver → `self_host_runtime` lock-in **380/0** (1 new snapshot `snag49c_throws_index.out`,
  NO existing snapshot changes); `runtime_diff` 379→**380** (only `snag49c_throws_index` flips, zero other
  flips/regressions); `lowerer_comparison` 960 + `c_emit_comparison` 891 (expected unchanged — adds a
  propagate call, not a new fn); `bootstrap_fixed_point` GREEN; `cargo test --lib` 1072/0.
- Stage ONLY `tests/fixtures/self_host_lowerer/lower.gg` + `tests/fixtures/runtime_snapshots/snag49c_throws_index.out`.

## Out of scope
This is the EIndex-INDEX (the `[i]` operand) auto-prop. Distinct from the landed 3g EIndex-VALUE (the
struct element read). Both now correct.
