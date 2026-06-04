# Brief v2 — 3h-drop: register user resource-containing structs for drop — +4, RUN-VERIFIED (narrative corrected)

A scope scout RUN-verified end-to-end: **380 → 384 (+4)**, ZERO regressions, `bootstrap_fixed_point` GREEN
(stage1==stage2 byte-identical). A fresh brief-review CONFIRMED the fix is correctly targeted + scoped and
the +4 real, but raised a BLOCKING narrative correction (the fix-#2 regression is NOT a "double-free") + 3
non-blocking folds, all incorporated here.

## Root (corrects a STALE TODO premise)
The TODO framed `leak_method_return_loop` as blocked on Phase-2c COMMIT-3 ("emit_scope_drops/flush_drop_queue
are no-op stubs"). **STALE** — `emit_scope_drops` (`lower.gg:1110-1116`) emits `GIDropIfAlive(entry.local_id)`
UNCONDITIONALLY in LIFO order; the machinery is ACTIVE (a String returned-from-fn-and-bound-in-a-loop IS
dropped per-iteration today). The +4 (leak true→false) would be impossible if the `__drop` fns didn't link.

Actual gap: `is_droppable_type` (`lower.gg:1174-1200`) returns true ONLY when `resource_meta_for` returns
`Some` (`:1196-1197`) — i.e. RUNTIME resources (String/Vector/Dict/Box). A USER struct that transitively
CONTAINS resources (e.g. `Def`, `Stage`) returns `None` (`:1198-1199`) → not droppable →
`register_local_for_drop` skips it → never dropped per-iteration → LEAK (`leak_method_return_loop`: oracle
`leaked=false`, self `leaked=true`). The docstring (`:1160-1169`) claims user types are "deliberately
EXCLUDED" for a "0 `<Type>__drop` definitions" gap — **FALSE NOW**: `populate_drop_metadata`
(`lir_lower.gg:3658`) registers `recursive_drop_structs`+`type_drop_fns` for every resource struct
(`:3684-3687`); the `__imported_type__` skip that caused "0 defs" was removed 2026-05-24 (`:3668-3674`). So
`Def__drop`/`Stage__drop`/`Container__drop` ARE emitted.

## The fix (3 parts, `lower.gg` — the scout's exact RUN-verified prototype)
1. **`is_droppable_type` (`:1174-1200`):** after the `resource_meta_for` → `None` arm (the trailing
   `return false` at `:1198-1200`), guard it with `if gmod.resource_types.contains(tname): return true`. So a
   user struct in `gmod.resource_types` (`Dict[String,bool]`, `gir.gg:378`; `.contains` already used at
   `lower.gg:8011`) — the fixpoint set of structs/enums transitively containing a resource
   (`lower.gg:11779-11809`) — becomes droppable. Runtime resources already hit `return true` at `:1197`
   first, so no double-count; plain POD structs aren't in `resource_types` → still skipped (no spurious drop).
2. **PAIRED GUARD at the `CloneAndMove` emission arm (`:7091-7141`):** the source-pointer inspection at
   `:7115-7133` already matches `GtPtr(cam_inner)`/`GtMutPtr(cam_inner)`. Set `bool source_was_ptr` true in
   those arms, then gate `:7141` with `if not source_was_ptr: register_local_for_drop(...)`. This is the
   Branch C-pre `.get().unwrap()` shape: it deref-CLONES a `GtPtr`/`GtMutPtr` collection element-address into
   an owned local via a DEEP `Container__clone` (`resolve_payload_clone_fn`, `:7983-8013`).
   ⚠ **CORRECTED NARRATIVE (review reservation 1 — DO NOT call this a "double-free"):** the clone owns its OWN
   independent heap, so dropping it is a memory-SAFE *extra* drop — NOT a double-free of the collection's
   element. The bug is purely a STDOUT divergence: the ORACLE borrows the element (no clone, no drop) and
   prints each label once; without the guard, fix #1 makes the self-host's deref-clone droppable → a SECOND
   drop → `drop_struct_collection_fields` double-PRINTS. The guard matches the oracle's borrow-no-drop stdout
   **at the cost of LEAKING the deref-clone** (the clone's heap is never freed; invisible to the fixture, which
   has no `mem_live()` check). **The executor's code comment MUST say "suppress the extra drop of a deep
   deref-clone to match the oracle's borrow-no-drop output (the clone leaks — removable once Branch C-pre
   borrows instead of clones, see TODO)" — NOT "prevent double-free".** (Self-host-showcase: an inaccurate
   comment is a false historical record.)
   ⚠ The guard covers BOTH `GtPtr` and `GtMutPtr`. The `GtMutPtr` arm also includes the `T x = &param` clone
   shape (`:973-977`/`:7120-7131`), which produces a genuinely-owned fresh value that in principle SHOULD be
   dropped — suppressing it would LEAK (not double-free). No current fixture hits a resource-struct
   `&param`-clone (scout's +4/0-regress + bootstrap-GREEN is the evidence). **Executor: confirm no snapshot
   exercises a resource-containing-struct `&param`-clone that this guard now leaks** (grep/inspect the
   GtMutPtr-clone fixtures); if one appears, the guard is too broad and the principled fix (below) is required.
3. **Docstring corrections (self-host-showcase / honest record):** rewrite BOTH stale blocks —
   (a) `:1160-1169` (the "0 `<Type>__drop` defs / deliberately EXCLUDED" justification on `is_droppable_type`),
   AND (b, review reservation 4) the LARGER stale block at `:1066-1109` on `emit_scope_drops` itself, which
   flatly states "emission is DISABLED / stays a no-op / any GIDropIfAlive emitted would double-free at
   runtime" — ALL now FALSE (the body emits unconditionally and this fix relies on it). Leaving 1066-1109 is a
   worse false-record than 1160-1169.

## Reviewers verify (load-bearing — touches drop registration)
1. **Fix #1 discriminator:** `gmod.resource_types.contains(tname)` is true EXACTLY for transitively-resource
   user structs; placement after the `None` arm doesn't double-count runtime resources (they returned true at
   `:1197`); POD structs stay non-droppable.
2. **Fix #2 scoped + the corrected narrative:** the guard fires for EXACTLY the Branch C-pre deref-clone
   (source `GtPtr`/`GtMutPtr`) and NOT for the MoveDirect value-owned path (`:7142-7149`) that the +4 fixtures
   use (they bind value-typed owned structs from a user-method return / direct construction — confirm
   `leak_method_return_loop`'s `auto sd = …` is a value return, not a collection element-address). Confirm the
   regression it prevents is an EXTRA-DROP/double-PRINT (clone-leak), NOT a double-free, and that the
   executor's comment reflects that.
3. **Net:** loop-bound owned user struct → exactly ONE drop/iteration (+4); borrowed/deref-cloned element →
   ZERO (guard #2, accepting the clone-leak). `bootstrap_fixed_point` GREEN is the strongest signal.

## Gates (executor; force-rebuild driver; baseline 380)
- `self_host_runtime` lock-in **384/0** (4 new snapshots: `leak_method_return_loop`, `drop_block_scope`,
  `drop_early_return`, `drop_loop_reinit`; NO existing snapshot changes — esp. `drop_struct_collection_fields`
  UNCHANGED); `runtime_diff` 380→**384** (exactly those 4 flip, ZERO regressions).
- `lowerer_comparison` 960 / `c_emit_comparison` 891 — EXPECTED unchanged (drop registration adds
  `GIDropIfAlive` call sites, not new fns); CONFIRM, and if changed, explain.
- `bootstrap_fixed_point` GREEN; `cargo test --lib` 1072/0.
- Stage ONLY `tests/fixtures/self_host_lowerer/lower.gg` + the 4 new `runtime_snapshots/*.out`.

## Out of scope (log to TODO with the causal link)
- **The guard #2 is a stdout-parity STOPGAP (review reservation 2).** The real bug: Branch C-pre CLONES where
  the oracle BORROWS (`lower.gg:925-979`). **Once Branch C-pre borrows-not-clones, the guard becomes removable**
  AND the deref-clone leak disappears. Log with this explicit causal link (not a vague backlog mention).
- **The ~14-fixture drop/leak backlog tail** with the SAME root family (`leak_collection_elements`/
  `_comprehensive`/`_match_resource`/`_match_struct`/`_result_*`, `drop_flag_*`, `drop_*reassign*`,
  match-arm-destructure drops) needs ADDITIONAL sub-fixes (match-arm-destructure drops, reassign-drops). A
  follow-on chain, NOT this one.
