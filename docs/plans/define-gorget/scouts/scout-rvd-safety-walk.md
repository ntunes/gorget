# RV-D SCOUT REPORT — self-host unified safety-walk soundness cluster

All findings in `tests/fixtures/self_host_typechecker/typecheck.gg` (symlinked into
`self_host_lowerer/` and `self_host_check/` drivers). Prototype patch:
`/tmp/recover_rvd_proto.patch`. Repro fixtures: `/tmp/rvd/`.

Zone note: last commit touching typecheck.gg = `c082ae96` (coarse-kind diagnostic
split — EMIT sites). The RV-D hunks touch place_projection_path, arg_place_is_copy,
safety_branch/safety_commit + the SIf/SMatch/EIf/EMatch/SMetaIf callers, EClosure,
and the three comprehension arms — NO overlap with the coarse-kind EMIT sites.

## Verified premise table (file:line against CURRENT source)

| # | Hole | self-host site | production model | ggdef |
|---|------|----------------|-----------------|-------|
| 6 | EClosure body walked on FRESH EMPTY SafetyState | typecheck.gg:1711 | check_expr.rs:1034-1050 (save→check→restore, captures visible) | check_use captures |
| 7 | comprehension arms run on discarded snapshot, no loop_depth bump | typecheck.gg:1775-1793 | check_expr.rs:1073-1133 (loop_depth+1 + push loop_locals) | loop context |
| 8 | place_projection_path carves out range-index → slice not a place | typecheck.gg:784-793 | helpers.rs:509-511 (Index recurses to object, no range check) | recurse Index |
| 9 | branch-join merge ADD-only → reinit-in-all-arms falsely rejected | typecheck.gg:1324-1327 | origins.rs:568-617 merge_branch_states (REPLACE w/ union of live end-states) + before-as-fallthrough (check_stmt.rs:1168-1170/1221-1223) | union_all REPLACE |
| 5 | Copy axis scalar-primitive-only vs is_copy_type | typecheck.gg:833 | type_utils.rs:17-88 is_copy_type (scalar/tuple/ref/handle/struct-of-copy) | — |

Docs grounding: reference §9.5 Branch Merging ("moved in any branch → moved after"),
§9.4 (Copy-read exemption + UAM-precedes-aliasing ordering), D10(b) ADDENDUM
(place-overlap ranges over LIVE ALIASES; Copy reads participate in no overlap),
D12 six positions.

## Measured before/after (driver vs production `gg check`)

Driver invoked as `driver <fixture> lib --lir-c` (accept = exit 0 + emits C;
reject = non-zero + codespan diagnostic).

| Fixture | Hole | production | self-host BEFORE | self-host AFTER |
|---------|------|-----------|------------------|-----------------|
| repro_6_closure | #6 | REJECT E_UseAfterMove | ACCEPT (UAF) | **REJECT E_UseAfterMove** ✓ |
| repro_7_comprehension | #7 | REJECT E_MoveInLoop | ACCEPT (per-iter UAF) | **REJECT E_MoveInLoop** ✓ |
| repro_8_slice_alias (Vec[String]) | #8 | REJECT E_BorrowConflict | ACCEPT (dangling view) | **REJECT E_BorrowConflict** ✓ |
| repro_8b_slice_int (Vec[int], Copy elem) | #8 | ACCEPT (RV-E bug) | ACCEPT | **ACCEPT** ✓ (parity kept) |
| repro_9_reinit_both | #9 | ACCEPT | REJECT E_DoubleMove (over-reject) | **ACCEPT** ✓ |
| repro_5_copy_struct | #5 | ACCEPT | REJECT E_BorrowConflict | REJECT (unchanged — struct case needs compute_is_copy) |

All 9 over-rejection/true-positive guards + 3 divergence/nesting edges match
production AFTER the fix:
- g6_closure_live=ACCEPT, g7_comp_plain=ACCEPT, g7_comp_read_enclosing=ACCEPT
- g9_move_one_arm / g9_move_both_arms / g9_reinit_one_arm / g9_match_move_all = REJECT E_DoubleMove
- g9_reinit_both_arms / g9_match_reinit_else = ACCEPT
- e9_diverge_reinit=ACCEPT, e9_nested_move=REJECT, e9_all_diverge=ACCEPT

Net: holes #6/#7/#8/#9 CLOSED and parity-clean; #8b stays at production parity
(the RV-E Copy-element bug is deliberately NOT fixed here — that is RV-E's job in
BOTH compilers). #5's struct-of-scalars case unchanged (subset only).

## Driver-lane gates (true-positive preservation + over-rejection guards)

`cargo test --test integration --release self_host_driver` (rebuilds the driver
from the patched typecheck.gg): **23/23 self_host_driver_* PASS, 0 FAILED** (191s),
including the over-rejection guards `accepts_liveness`, `accepts_d10b_place_overlap`,
`accepts_d12_legal` AND the true-positive lanes `rejects_liveness`,
`rejects_d10b_place_overlap`, `rejects_d12_drop_purity`, `rejects_invalid_program`.
No existing reject flipped to accept; no existing accept flipped to reject.

## Bootstrap-risk assessment (the executor runs its OWN bootstrap)

DIRECTION IS GREEN. The three TIGHTENING fixes (#6/#7/#8 — the ones that could
reject valid self-host source) were probed end-to-end:

- **Whole-frontend compile**: `driver self_host_lowerer/driver.gg lib --lir-c`
  compiles the ENTIRE frontend (driver.gg + all ~18K lines of imports) through the
  new walk → **exit 0, 37.4 MB C emitted, ZERO error diagnostics.** No pattern in
  the actual self-host source is over-rejected by the tighter closure/comprehension/
  slice checks.
- The two LOOSENING fixes (#9 REPLACE-with-union; #5 Copy subset) can only ACCEPT
  MORE, so they carry no over-tightening risk; their true-positives are pinned by
  g9_move_* / g9_match_move_all / rejects_liveness (all still REJECT).

Residual risk is LOW but non-zero: the frontend probe exercises the source that
EXISTS, not every shape. The executor's `self_host_bootstrap_fixed_point` +
`*_comparison` + full integration sweep are the definitive gate. No under-rejection
path was found (every true-positive guard fires; #9's REPLACE never drops a key
that a reaching path still holds — verified by g9_move_one_arm / reinit_one_arm /
nested / diverge edges).

## Executor plan

Single coherent change-set over the walk (patch = `/tmp/recover_rvd_proto.patch`,
159 ins / 46 del, ALL in `tests/fixtures/self_host_typechecker/typecheck.gg`):

1. **#8** `place_projection_path` EIndex arm — drop the range carve-out, recurse to
   the collection root for scalar AND range index (mirrors
   `find_root_def_id_with_path`). Do NOT touch `expr_is_place` (:603) — production's
   `expr_is_place` ALSO carves out ranges; the asymmetry is deliberate.
2. **#6** EClosure — check the body against `safety_snapshot(state)` with loop ctx
   reset (loop_depth 0, fresh loop_locals, rebind -1) then discard; captures now
   visible so a moved capture read → UAM.
3. **#7** EListComp/ESetComp/EDictComp — snapshot + `loop_depth+1` + fresh
   loop_locals (iterable still checked in the enclosing state; watch the EListComp
   iterable line — easy to drop, see the fix).
4. **#9** `safety_branch` returns `bool reached`; `safety_commit(acc, has_fallthrough,
   any_reached, &state)` builds `merged = acc.clone()` (∪ pre-branch state when
   `has_fallthrough`) and REPLACES `state.moved`. `has_fallthrough` from
   `not elsebranches_have_uncond(...)` (if/elif) / `not match_has_catch_all(arms)`
   (match). Callers: SIf, SMatch, EIf, EMatch, SMetaIf. Do NOT remove the
   OWNER-RULED-KEEP `live_reinit` workaround (:1186-1190) — untouched here.
5. **#5 (subset)** `arg_place_is_copy` → recursive `resolved_type_is_copy` covering
   scalar/tuple-of-copy/RTRef/handle-generics. STRUCT/ENUM case still conservative.

### #5 completion (optional, separable — recommend deciding at integration)

The struct-of-scalars case (repro_5) needs `is_copy_type`'s "not drop-tainted AND
all fields Copy" recursion, which needs struct field types reachable from
`arg_place_is_copy`. The self-host's DefInfo has NO field_types (production's does);
`compute_drop_taint` walks the module AST. Reference-grade fix = add a
`compute_is_copy` pass mirroring `compute_drop_taint` (typecheck.gg:550) that sets a
new `DefInfo.is_copy` flag, then read it in `resolved_type_is_copy`'s RTDefined arm.
COST: new DefInfo field → updates the 3 `DefInfo(...)` constructors (scope.gg:216/
255/271) + a `set_def_is_copy` setter + pipeline wiring after compute_drop_taint.
PRIORITY: LOWEST of the cluster — #5 is an OVER-rejection (sound; never a UAF) that
is DORMANT (the frontend probe shows self-host source never trips it). Safe to land
the subset now and FILE the struct case, OR fold the pass in if the executor wants
full is_copy_type parity. Either is defensible; it is NOT a soundness or bootstrap
blocker.

### Gates for the executor
- `cargo build` (Rust unaffected — change is a .gg fixture).
- Rebuild driver: `rm driver driver.c; GG_BUILD_TIMEOUT_SECS=600 gg build
  self_host_lowerer/driver.gg`.
- `self_host_driver` lanes (22/22) + the RV-D repros/guards in `/tmp/rvd/`.
- `type_comparison` — confirm mismatched count does NOT rise above the 85 baseline
  (c082ae96); a rise = a corpus over-rejection the frontend probe missed.
- `self_host_bootstrap_fixed_point` (stage-2==3==4) — the definitive over-tightening
  gate. Add exercising fixtures: promote the 6 repros (esp. #6/#7/#8/#9) into
  `self_host_driver_rejects_*` / `accepts_*` lanes so the class is guarded.
- Full integration sweep (owner-required at round close).

## Pre-existing bugs found (file, do NOT fix here)
- **RV-E CONFIRMED as the production twin of #8**: `gg check f(&v, v[0..2])` on
  `Vector[int]` (Copy element) → ACCEPT (should reject — the slice is a live view).
  Production `lvalue_value_type` (helpers.rs:900-910) returns `args.last()` (element
  int) for a range index → Copy exemption fires. RV-D's #8 keeps the self-host at
  PARITY with this (repro_8b ACCEPT on both); the fix belongs in RV-E, applied to
  BOTH compilers (self-host twin: force a range-index arg non-Copy in
  `arg_place_is_copy`). Already filed as RV-E; no NEW bug beyond it.
- No other new defect class surfaced.
