# FIDELITY brief — field-store consumes its owned struct base (self-host)

## Goal
`x.field = value` where `x` is an **owned (`LoOwned`) struct local** mis-lowers:
the self-host emits `OpMove(base)` for the base, so lir_lower materializes a
**whole-struct clone** of the base, writes the field into the throwaway clone,
and discards it — the original struct is never mutated. Downstream reads of
`x.field` see the un-mutated (often empty/NULL) value → wrong output or a
SEGV (`Vector[String]` field → `safe_get` → `Some(NULL)` → memcpy-from-NULL).
A field-STORE must mutate its base IN PLACE; it never consumes the base.

## ⚠ This CORRECTS the TODO's "bug-3 / snag-#8" diagnosis (verified by RUNNING)
The TODO framed the `move_across_branches`/`field_store_auto_clones_live_source`
crashes as snag-#8 (per-branch move-flag pollution; "port Rust's
snapshot/restore to `lower_if`/`lower_match_stmt`"). **That is WRONG** — the
scout verified:
- The self-host ALREADY has `snap_moved_state`/`restore_moved`/`union_moved`
  (`lower.gg:~1221/1245/1265`) but they are **DEAD CODE (zero call sites)**.
- The self-host decides move-vs-clone in a separate CFG post-pass
  `wire_liveness_into_modes` (`lower.gg:~2487`) driven by flow-sensitive
  liveness (`liveness.last_use_of_op`), NOT by sequential `mark_local_moved`
  flag pollution across branches. `mark_local_moved` is only called from that
  post-pass (`lower.gg:~2595`), long after `lower_if` runs.
- The crash reproduces with **NO branches at all**: `after_use()` (a plain
  sequence, no if/match) clones-and-discards the base. So it is not a branch
  bug — it is the field-store base-ownership bug below.
Do NOT port the snapshot/restore (no-op). Fix the base ownership.

## Root cause (writer-site, verified in emitted C)
`tests/fixtures/self_host_lowerer/lower.gg`:
- `lower_field_write` (~:7099, the store emit ~:7120-7126) and
- `emit_field_write_from_local` (~:7132, ~:7152-7157)

emit `OpMove(base)` for a non-borrowed (`LoOwned`) struct base. The OpMove-of-
resource path in `lir_lower` (~:2349-2521) then clones the whole base struct,
writes the field into the clone, and the clone is discarded. Verified C from
`after_use()`: `Foo__clone(&__s5) → __s22; memcpy(&__s22->a, items_clone); /*
__s22 discarded */` then `f.a` is read from the un-mutated `__s5` → empty.

## Rust reference (the oracle — MIRROR THE SEMANTICS)
`src/ir/lowering/stmts/assigns.rs:~395-458`: a field-store builds a
`Place{ local: f, projections:[Field(a)] }` and writes IN PLACE — the base is an
**lvalue**, never moved or cloned. (Mirror the *semantics*, not the Rust Place
machinery; the self-host's equivalent is to pass the base by address/borrow.)

## The fix (executor: implement + RE-VERIFY each candidate by RUNNING)
A field-store never consumes its base. In BOTH `lower_field_write` and
`emit_field_write_from_local`, pass the base as a **borrow / address**
(`OpBorrow(base)`) instead of `OpMove(base)` for the `LoOwned` case (the
`LoBorrowed` case — e.g. a `&mut`/`!` param base — is ALREADY a borrow and
correct; leave it). This matches `needs_ptr_arg("__field_write_", 0) == true`
(`lir_lower.gg:~1890`) — the runtime field-write helper already expects the base
by pointer — and the Rust in-place Place store.
- ⚠ Confirm `OpBorrow(base)` lowers to a `SlotAddr` pointer the `__field_write_`
  codegen (`lir_codegen.gg:~3717`) consumes, and that the OpMove→clone path in
  `lir_lower` is NO LONGER reached for the base.
- ⚠ Hot path — `x.field = …` is everywhere. RE-PIN line numbers by CONTENT.
  Regression-check these shapes specifically (RUN them): plain `f.field = x`;
  `f.field += x` / compound-assign (reads-then-writes the base — must still
  read correctly); `self.field = x` (base already a borrow); nested
  `a.b.c = x` (chained field write — the OUTER base is the place, inner accesses
  are reads); a field-store inside an `equip` method.
- ⚠ Do NOT change the RHS (`value`) ownership — that side correctly clones/moves
  per CoW (the borrowed-String + the 3-way rule). ONLY the BASE changes.
- Do NOT reshape any fixture.

## Expected flips (RE-RUN each — count only WHOLE-stdout MATCH)
HIGH confidence (plain field-store crashes): `field_store_auto_clones_live_source`,
`move_across_branches`, `option_result_field_store`. PROBABLE (may carry a
secondary diff — verify): `cow_nested_field_mutation`, `empty_literal_struct_field`,
`string_struct_complex`. Realistic **+3 to +6**.

## File zone
ONLY `tests/fixtures/self_host_lowerer/lower.gg` (`lower_field_write` ~:7099,
`emit_field_write_from_local` ~:7132). **Same FILE as the in-flight
method-resolution chain (which edits `infer_method_return_type`/
`is_string_view_method`/`is_owning_mutator_arg` at ~:523/:574/:3547) — but
HUNK-DISJOINT (line ~7100 vs ~500-3500).** Run in an isolated worktree; the
orchestrator cherry-picks and resolves any overlap at merge (expected clean).

## Gates (force-rebuild driver: `rm tests/fixtures/self_host_lowerer/driver{,.c}`)
- The 3 high-confidence fixtures → MATCH (whole stdout). Re-run the 3 probables;
  report which flip.
- `runtime_diff` parity ≥ 330; no MATCH→worse (⚠ this fix touches a HOT path —
  carefully confirm NO previously-MATCH fixture regressed, esp. struct/CoW/equip
  families).
- `self_host_runtime` regressed=0 → regen → new passing set (report new count +
  that only new `.out` files were added / 0 modified).
- `lowerer_comparison` ≥958, `c_emit_comparison` ≥887 (a lowering-shape change —
  these MIGHT move slightly if the clone is elided; report before→after, and if
  they DROP investigate, if they RISE it's the elided clone — confirm benign).
- `bootstrap_fixed_point` GREEN (the driver does `x.field = …` extensively, so
  this is a REAL neutrality signal here, not just a guard — high-value gate).

## Worktree discipline (executor preamble)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree,
NEVER `/workspace/gorget-1`. Open with `git merge --ff-only gorget-1`. Stage
ONLY `tests/fixtures/self_host_lowerer/lower.gg` + the new
`tests/fixtures/runtime_snapshots/*.out` (NEVER `git add -A`). Run `cargo build`
+ the targeted gates above; do NOT run the full integration suite (orchestrator
owns it). If any regression-shape (compound/nested/self/equip) breaks, STOP and
report — do NOT broaden the fix to paper over it.
