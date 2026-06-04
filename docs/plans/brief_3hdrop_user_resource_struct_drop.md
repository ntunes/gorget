# Brief — 3h-drop: register user resource-containing structs for drop — +4, RUN-VERIFIED

A scope scout RUN-verified this end-to-end: **380 → 384 (+4)**, ZERO regressions, `bootstrap_fixed_point`
GREEN (stage1==stage2 byte-identical). 2-line core + 1 paired guard + a docstring correction.

## Root (corrects a STALE TODO premise)
The TODO framed `leak_method_return_loop` as blocked on the unshipped Phase-2c COMMIT-3 drop-emission flip
("emit_scope_drops/flush_drop_queue are no-op stubs"). **That premise is STALE** — the scout proved the drop
machinery is ACTIVE: `emit_scope_drops` (`lower.gg:1110`) emits `GIDropIfAlive` unconditionally,
`pop_drop_scope` emits on while-loop fall-through, and a runtime-resource (String) returned-from-fn-and-
bound-in-a-loop IS correctly dropped per-iteration today.

The actual gap: `is_droppable_type` (`lower.gg:1174`) returns true ONLY when `resource_meta_for` returns
`Some` — i.e. for RUNTIME resources (String/Vector/Dict/Box…). For a USER struct that CONTAINS resources
(e.g. `Def`, `Stage`), `resource_meta_for` returns `None` → `is_droppable_type` false →
`register_local_for_drop` skips it → it never enters the drop scope → never dropped per-iteration → LEAK.
The function's docstring (`lower.gg:1160-1169`) claims user types are "deliberately EXCLUDED" because of a
"0 `<Type>__drop` definitions" gap — **that justification is FALSE NOW**: `Def__drop`/`Stage__drop` ARE
emitted (`populate_drop_metadata` + `recursive_drop_structs` + `drop_fn_for_type` all work; verified in the
emitted C). Baseline behavior: `leak_method_return_loop` oracle `leaked=false`, self-host `leaked=true`.

## The fix (2-line core + 1 paired guard + docstring — the scout's exact RUN-verified prototype)
1. **`is_droppable_type` (`lower.gg:~1174`):** after the `resource_meta_for` → `None` arm, add
   `if gmod.resource_types.contains(tname): return true` — so a user struct registered as a resource type
   (i.e. it transitively contains a resource) becomes droppable. (Re-pin `tname`/the `gmod.resource_types`
   accessor against current source.)
2. **PAIRED GUARD (load-bearing — prevents a double-free regression) at the Branch C-pre `.get().unwrap()`
   clone (`lower.gg:~7116-7144`):** that CloneAndMove path deref-CLONES a `GtPtr`/`GtMutPtr` element-address
   into an owned local where **the oracle BORROWS it** (no clone, no drop). Without this guard, fix #1 makes
   that clone droppable → an EXTRA drop → `drop_struct_collection_fields` double-prints (`wrapped`/`new`/
   `first`). Fix: SKIP `register_local_for_drop` when the clone source was a pointer (`GtPtr`/`GtMutPtr`)
   element-address. The scout measured: with guard #2, `drop_struct_collection_fields` stays MATCH (no
   regression); without it, −1.
3. **Docstring correction (`lower.gg:~1160-1169`):** the stale "0 `<Type>__drop` definitions / deliberately
   EXCLUDED" justification must be REWRITTEN to reflect that user resource-containing structs ARE now
   droppable (their `__drop` fns are emitted). Per CLAUDE.md self-host-showcase: a false historical record
   in a comment misleads — fix the comment, not just the code.

## Reviewers verify (load-bearing — this touches drop registration → double-free risk)
1. **Fix #1 scope:** `gmod.resource_types.contains(tname)` is true exactly for transitively-resource user
   structs (so we register THEIR drop), and does NOT newly-register plain POD structs (no resources → not in
   `resource_types` → still skipped). Confirm `resource_types` membership is the right discriminator (it's
   the set that drives `recursive_drop_structs`/`__drop` emission) and that the order (after the
   `resource_meta_for` None arm) doesn't double-count runtime resources.
2. **Fix #2 is REQUIRED + correctly scoped:** trace the Branch C-pre clone (`.get().unwrap()` element read
   that deref-clones a `GtPtr`/`GtMutPtr` to an owned local). Confirm the oracle BORROWS (doesn't drop) this
   element, so registering it for drop is a DOUBLE-free of the collection's element. Confirm the guard
   (skip-register-when-source-was-pointer) fires for exactly that path and NOT for legitimately-owned user
   structs (which MUST stay droppable for fix #1's +4). The scout saw `drop_struct_collection_fields`
   double-print without the guard → confirm it's clean WITH it.
3. **No double-free / no missed-drop:** a per-iteration loop-bound user struct gets exactly ONE drop per
   iteration (the +4 leak fixtures); a borrowed/aliased element gets ZERO (guard #2); `bootstrap_fixed_point`
   GREEN is the strongest signal (the driver self-compiles its own drop logic + re-converges byte-identical).

## Gates (executor; force-rebuild driver; baseline 380)
- `self_host_runtime` lock-in **384/0** (4 new snapshots: `leak_method_return_loop`, `drop_block_scope`,
  `drop_early_return`, `drop_loop_reinit`; NO existing snapshot changes — esp. `drop_struct_collection_fields`
  UNCHANGED); `runtime_diff` 380→**384** (exactly those 4 flip, ZERO regressions).
- `lowerer_comparison` / `c_emit_comparison` — report (drop registration adds `GIDropIfAlive` call sites,
  not new fns, so fn-count is EXPECTED unchanged at 960/891 — but CONFIRM, and if it changes, explain why).
- `bootstrap_fixed_point` GREEN; `cargo test --lib` 1072/0.
- Stage ONLY `tests/fixtures/self_host_lowerer/lower.gg` + the 4 new `runtime_snapshots/*.out`.

## Out of scope (log to TODO)
This +4 is the clean subset. The scout found a ~14-fixture drop/leak backlog tail with the SAME root family
(`leak_collection_elements`/`_comprehensive`/`_match_resource`/`_match_struct`/`_result_*`, `drop_flag_*`,
`drop_*reassign*`, match-arm-destructure drops) that needs ADDITIONAL sub-fixes (match-arm-destructure drops,
reassign-drops, the `.get().unwrap()` borrow done properly) — a follow-on chain, NOT this one. Log it.
