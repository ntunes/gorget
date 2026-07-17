# EXECUTOR BRIEF — Root A: self-host iterator-receiver field-place borrow (3 spin fixtures flip)

**Status:** DRAFT — in the ≥3-fresh-pass review gauntlet. Do not execute until a clean pass.
**Scout evidence (THE measured spec):** `docs/plans/roota-scout.md` — GO, fully validated.
Prototype: `/tmp/roota_proto.patch` (197 lines, 4 files: `lower_expr.gg` + `tests/integration.rs`
+ 2 new fixtures; applies cleanly, round-trip verified; backup `/tmp/recover_roota_proto.patch`).
Census context: `docs/plans/hang-census-2026-07-16.md` §Root A.

## Mission

Land the write-site fix for the hang census's Root A: the self-host EMethodCall receiver
lowering **byte-copies a `&self`/bare-local receiver that is a struct-FIELD place** when the
field's type is not resource-classified (`lower_place_base` has no `EFieldAccess` arm →
falls to `lower_expr`, whose field-load is Ptr-typed only for `is_resource_type_name`,
`lower_expr.gg:~4718`). `SetIter`/`DictKeysIter`/`DictValuesIter` hold a `Ref` (not a
resource) → the lazy adapters advanced a discarded copy of the cursor → infinite re-yield.
The fix: `lower_recv_place` at the EMethodCall receiver site — for an `EFieldAccess`
receiver on a bare-local / `ESelfExpr` base whose field is a plain struct (`>= PRIM_COUNT`,
not resource, not `Ref__`/`MutRef__`), emit a Ptr-typed `GIFieldLoad` (borrow the field
place, the exact 1B/VectorIter shape; mirrors Rust `methods.rs:2037-2064`). Every other
shape delegates to `lower_place_base` UNCHANGED (the scout verified byte-identical).
Known gotcha (already in the patch): the self receiver arrives as `ESelfExpr`, NOT
`EIdentifier("self")`.

Implementation-internal per Core #9 (the self-host lane catching up to Rust's correct
behavior; the census fixtures + 2 new regression fixtures are the pins).

## Scope (same commit)

1. The patch (or re-derive on drift — STOP-AND-REPORT if it stops being mechanical).
2. The 2 new regression fixtures (`set_filter_count` → `3`; `set_take_values` → `10,20`) —
   scout-verified 3-lane MATCHes; wire `run_gg`.
3. **The EXPECTED_HANGS no-new-hangs guard** (the census recommendation, harness follow-ups
   TODO entry item (iii)): in `self_host_runtime_diff`, after the CRASH print, assert the
   set of fixtures whose label contains `timed out`/`runaway output` ⊆ `EXPECTED_HANGS` —
   **listing ONLY `async_select`** (this landing removes the other three, so the list is
   born correct). A new hang FAILS loudly; a fixed one FAILS asking to shrink. Shrink-only
   allowlist, the MATCH-floor idiom; place it beside `RUNTIME_DIFF_MATCH_FLOOR` with the
   same comment discipline. Update the harness follow-ups TODO entry: item (iii) lands here;
   items (i)/(ii)/(iv) remain.
4. **Parity regen + floor:** run the full regen FOREGROUND
   (`GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 <test binary> self_host_runtime_diff
   --nocapture`, ~200s — do NOT set GG_TEST_TIMEOUT_SECS; the harness is FIXED, the
   scout-report's "OOMs solo" line quoted a stale MEMORY note). Expect MATCH 1166→1169
   (the 3 CRASH→MATCH) + the 2 new fixtures entering as MATCHes → likely 1171/1242-ish;
   seed `RUNTIME_DIFF_MATCH_FLOOR` = fresh MATCH − 5 per the documented formula, and record
   the full `PARITY =` line in your report. Any regression in the table = STOP-AND-REPORT.

## Gates (FOREGROUND; self-host builds `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`;
chunk >10min; NEVER background a final gate)

1. `cargo build` + `cargo test --lib` + the integration iterator filter (scout: 9 tests).
2. The 3 census fixtures end-to-end vs the Rust oracle (record stdout both lanes:
   `stdlib_iter_set` `10,20,30,40,--,10,20,--,30,40` · `dict_keys_lazy` `60,3,2` ·
   `dict_values_lazy` `100,2`) + the 2 new fixtures on C + LLVM + self-host driver.
3. The blast-radius set: re-run the scout's 76-fixture field-access-receiver population
   pre/post status compare (byte-identical statuses; the scout's list is in its report —
   re-derive the grep if the report's list is unavailable).
4. ASan on the touched fixtures + a Vector-adapter control.
5. `self_host_bootstrap_fixed_point` (lower_expr.gg is on the bootstrap path; budget 900s,
   chunked foreground; scout measured 583s).
6. The parity regen (scope item 4) — this doubles as the guard's first live run.

## Bookkeeping (same commit)
- TODO: the Root-A HIGH entry retires to DONE (datestamped, with the mechanism + the
  measured flips); the harness follow-ups entry updated per scope item 3; the two
  scout-filed limitations (deeper-chain LOW; the DictIter chained-adapter MED) STAY filed —
  do not touch. MEMORY's parity line is the PARENT's to update — report your fresh number,
  don't edit memory.
- Stage EXPLICITLY: `tests/fixtures/self_host_lowerer/lower_expr.gg tests/integration.rs
  tests/fixtures/set_filter_count.gg tests/fixtures/set_take_values.gg TODO.md DONE.md`
  (adjust to actual). Trailers:

      Co-Authored-By: Claude Opus <noreply@anthropic.com>
      Claude-Session: https://claude.ai/code/session_01TYkkHveF8WhhTVX4DjbCTN

- Checkpoint `/tmp/roota_exec_progress.md` after every gate. STOP-AND-REPORT on any
  conflict, gate failure, or surprise.

## Zones
Your write zone: `tests/fixtures/self_host_lowerer/lower_expr.gg` + `tests/integration.rs`
(the runtime_diff region for the guard — NOT the `run_with_deadline` region) + fixtures +
TODO/DONE. The RV-B track will land AFTER you in `lower_expr.gg` (its AST-shape change
rebases on your landing — keep your diff clean and minimal there). A concurrent RV-B
brief-reviewer is READ-ONLY in your zone.

Final message: commit hash + branch, the 3-fixture flip outputs both lanes, the fresh
`PARITY =` line + new floor, every gate count verbatim, the guard's first-run evidence,
staged list, smells.
