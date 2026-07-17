# EXECUTOR BRIEF — Root A: self-host iterator-receiver field-place borrow (3 spin fixtures flip)

**Status:** DRAFT v3 — in the ≥3-fresh-pass review gauntlet. Pass 1 (core patch verified
clean end-to-end; 4 reservations folded: guard gating + positive control, comparison-net
facts, TODO retire disambiguation + Ref__ residual) → v2. Pass 2 (all folds re-verified
independently incl. the load-bearing rp_is_ref exclusion at the LIR alias crux; ONE
SUBSTANTIVE catch: the run_gg wiring pins only the RUST lane — for this self-host-only bug
NO default-CI test would fail on revert; scope 2b now seeds the 5 runtime snapshots into
the default `self_host_runtime` net with a revert-verify, per the CoW-1B/1C practice; +m1
ggdef standing-exclusion citation, m2 sub-bullet promotion, m3 the second-regen cost)
→ this v3. Do not execute until a clean pass.
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
2b. **DEFAULT-CI LOCK-IN for the SELF-HOST lane (pass-2 R1 — without this, reverting
   `lower_recv_place` fails NO default test):** `run_gg` builds with RUST gg, and Root A is
   a self-host-only bug — so the run_gg wiring alone does not exercise what it guards, and
   the floor + EXPECTED_HANGS guard live inside the env-gated `runtime_diff`. Seed the
   default-running `self_host_runtime` lock-in net: `GG_REGEN_RUNTIME_SNAPSHOT=1 …
   self_host_runtime`, then stage ONLY the relevant new `.out` snapshot files (the 3 census
   fixtures + the 2 new ones — 5 snapshots; enumerate them in your staged list; do NOT
   sweep in unrelated regenerated snapshots). This is the established practice for parity
   flips (CoW-1B `565392d8`, CoW-1C `99a52094` both seeded snapshots). Verify the lock-in:
   with the fix reverted locally, at least one seeded snapshot test must FAIL; restore.
3. **The EXPECTED_HANGS no-new-hangs guard** (the census recommendation, harness follow-ups
   TODO entry item (iii)): in `self_host_runtime_diff`, after the CRASH print, assert the
   set of fixtures whose label contains `timed out`/`runaway output` ⊆ `EXPECTED_HANGS` —
   **listing ONLY `async_select`** (this landing removes the other three, so the list is
   born correct). A new hang FAILS loudly; a fixed one FAILS asking to shrink. Shrink-only
   allowlist, the MATCH-floor idiom; place it beside `RUNTIME_DIFF_MATCH_FLOOR` with the
   same comment discipline — **and inside the SAME gating structure** (pass-1 R1): skipped
   under `cfg!(debug_assertions)` and gated on `parity_floor_active(...)` exactly like the
   floor (`integration.rs:~22287-22292`), because the floor's 5-jitter discount exists
   precisely for transient MATCH→CRASH timeout flips and a SET guard cannot discount jitter
   — its comment must state that a transient red = re-run (same triage as a floor red).
   **POSITIVE CONTROL required (pass-1 R4, per the leak-detection-needs-positive-control
   rule): before shipping, prove the guard CAN FIRE** — invert/empty the allowlist (or
   point the substring filter at a label you know is present) and confirm the test FAILS
   with the intended message, then restore; record the failing output in your report. A
   guard whose only validation is "the assert passed" may be a substring typo matching
   nothing. COST NOTE (pass-2 m3): the positive control is a SEPARATE ~200s release regen
   on top of gate 6's passing regen — two full runs total; budget for it, foreground both.
   Update the harness follow-ups TODO entry: item (iii) lands here; items (i)/(ii)/(iv)
   remain.
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
6. The parity regen (scope item 4) — this doubles as the guard's first live PASSING run
   (the positive control in scope item 3 is what proves it can FAIL).
7. Comparison-net facts (pass-1 R2 — state, don't re-run beyond the listed): `type_comparison`
   / `check_comparison` build the TYPECHECKER driver, which has no `lower_expr.gg` — provably
   untouched by this diff; `lowerer_comparison` + `c_emit_comparison` DO build from the
   patched tree but are structurally insensitive to a borrow-vs-copy change (`c_emit`'s
   metric is user_fn_count; the fix adds no functions) — they are the parent's-sweep
   backstop; the 2 new fixtures raise c_emit's matched count by +2 (floor-safe; optionally
   reseed `C_EMIT_MATCH_FLOOR` +2 with the same-commit ratchet discipline, reporting the
   fresh count).

## Bookkeeping (same commit)
- TODO: **the retire target is EXACTLY the "ROOT A of the hang census 2026-07-16 …
  SCOUTED 2026-07-17 — GO" HIGH entry (~:1021)** — datestamped to DONE with the mechanism +
  measured flips. ⚠ DISAMBIGUATION (pass-1 R3): TWO OTHER entries also say "Root A" and are
  DIFFERENT, OLDER tracks that this patch does NOT resolve — the "#4 Ref[T] / lazy-iterator
  read-side deref CRASH" deep track (~:195) and the "Ref/MutRef write-site port" entry
  (~:214). BOTH STAY FILED untouched. Additionally: `lower_recv_place` ADDS a
  `starts_with("Ref__")/("MutRef__")` name-match (`rp_is_ref`) — an ACCEPTED residual
  mirroring the established `lir_lower.gg:~5143` idiom, but it joins line ~:214's debt
  census: append it to that entry so the census stays complete and the output-reviewer
  doesn't mis-flag it as a fresh layering violation. **Retiring the :1021 parent must NOT
  orphan its nested sub-bullets (pass-2 m2): PROMOTE the two scout-filed limitations
  (deeper-chain LOW; DictIter chained-adapter MED) to STANDALONE entries** — they stay
  pending. The harness follow-ups entry updates per scope item 3. ggdef subset note (pass-2
  m1): generic stdlib iterators are a PRE-EXISTING structural ggdef exclusion, not a new
  gap — cite the standing exclusion in the commit note; file a new gap ONLY if you find the
  standing one is not actually recorded. MEMORY's parity line is the PARENT's to update —
  report your fresh number, don't edit memory.
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
