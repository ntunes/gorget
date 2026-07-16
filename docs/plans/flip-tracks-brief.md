# EXECUTOR BRIEF — self-host FLIP TRACKS: callable-init class (+4) + Copy-axis struct (+1) → SELFHOST floor 214

**Status:** DRAFT — in the ≥3-fresh-pass review gauntlet. Do not execute until a clean pass.
**Scout evidence (THE spec's measurements):** `docs/plans/flip-tracks-scout.md` — GO, fully
prototyped + measured. Prototype: `/tmp/flip_proto.patch` (293 lines, backup
`/tmp/recover_flip_proto.patch`; touches ONLY `tests/fixtures/self_host_typechecker/scope.gg`
+ `typecheck.gg`). Verified in the scout's worktree: all 4 callable fixtures flip
ACCEPT→REJECT with `error[E_MoveWithoutOperator]`; `copy_struct_field_borrow_ok.gg` flips
REJECT(`E_BorrowConflict`)→ACCEPT (runs, prints `4`, exit 0); full self-host conformance lane
**214/214** (was 209/4/1); bootstrap fixed-point ok (504.6s); blast radius: 11 at-risk ACCEPT
fixtures verified unchanged; `move_without_operator_error.gg` (`Box[int] b = a`) now rejects
identically to production — a bonus parity alignment.

## Mission

Land the scout's prototype as the production self-host state, closing the FIVE by-design
conformance misses filed at the RV-F landing (DONE.md "RV-F — the four ggdef ORACLE
divergences CLOSED"):

- **Fix A — Copy-axis struct extension:** `DefInfo.is_copy` (`scope.gg`) + `compute_is_copy`
  fixpoint (dual of `compute_drop_taint`, runs right after it) + the `RTDefined` arm in
  `resolved_type_is_copy` (`not drop_tainted and is_copy`). Mirrors production `is_copy_type`
  (`src/semantic/safety/type_utils.rs:49-80`) and ggdef `ty_is_copy`
  (`spec/ggdef/src/elaborate/mod.rs:546`).
- **Fix B — the single-owner Callable INIT class (#15 twin):** `is_single_owner_type` (the 5
  callable ResolvedType variants + Box/Task/TaskGroup/Guard generics; drop-taint excluded —
  disjoint axes, no double-diagnosis) + `single_owner_message` (mirrors `errors.rs:1045`) +
  `reject_single_owner_init` wired at production's 5 sites: bind + reassign (PARAM-EXEMPT,
  mirroring `check_stmt.rs:1464`) and ECall-ctor + struct-literal + enum-variant shorthand
  (params REJECTED, mirroring `check_expr.rs:45`).

This is a lane-catch-up landing (self-host adopting semantics production + ggdef already
enforce — Core #9's implementation-internal category; the cross-lane fixtures already exist
and are exactly the 5 flipping).

## The work

1. Apply `/tmp/flip_proto.patch`; read it against the scout's design section — you own the
   result, not the patch (if anything in it contradicts this brief, STOP-AND-REPORT).
2. Same commit riders:
   - `tests/spec_conformance.rs`: `SELFHOST_MATCH_FLOOR` 209→214; REWRITE the by-design-miss
     comments (module doc ~:49-67 + inline ~:117-163) — the five-miss list is GONE; do not
     leave stale prose.
   - The documented asymmetry note (scout's "KNOWN MINOR ASYMMETRY"): the self-host
     struct-literal arm lacks production's ref-typed-field skip (`check_expr.rs:1183`
     `!target_is_ref`) — over-reject-only, unreachable in today's corpus. Put the one-line
     note as a code comment at the EStructLiteral arm + a TODO entry (LOW) so the gap is
     findable — do NOT implement the heavier per-field ref-flag mirror in this track.
   - TODO/DONE: move the two flip obligations (TODO ~:257 callable-init class; ~:888
     Copy-axis) to DONE.md with datestamps. TODO stays pending-only; no landed breadcrumbs.
3. Gates (FOREGROUND; self-host commands get `GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=600`; chunk >10min gates by test name; NEVER background a final gate):
   a. `cargo build` + `cargo test --lib`.
   b. The 5 fixtures through the self-host driver (force-rebuild the driver first): 4 REJECT
      with `error[E_MoveWithoutOperator]`, copy_struct ACCEPTs → runs → `4`.
   c. The full `spec_conformance` suite: SELFHOST 214/214; C/LLVM/GGDEF unchanged (zero
      Rust/ggdef source edits in this diff — verify by inspecting your own staged list).
   d. The 11 at-risk ACCEPT fixtures + `dict_box_callable` + `callable_param_rebind_ok`
      (the param-exemption load-bearing proof, expect `20`) re-run unchanged.
   e. `self_host_bootstrap_fixed_point` (chunked foreground; scout measured 504.6s — budget
      900s).
   f. Targeted `*_comparison` net for the typechecker area.
4. Commit: stage EXPLICITLY (`git add tests/fixtures/self_host_typechecker/scope.gg
   tests/fixtures/self_host_typechecker/typecheck.gg tests/spec_conformance.rs TODO.md
   DONE.md` — adjust to actual); message with the measured flip matrix; trailers:

       Co-Authored-By: Claude Opus <noreply@anthropic.com>
       Claude-Session: https://claude.ai/code/session_01TYkkHveF8WhhTVX4DjbCTN

## Constraints & zones

- Your write zone: `tests/fixtures/self_host_typechecker/**` + `tests/spec_conformance.rs` +
  TODO/DONE. Another executor is working the self-host LOWERER (`lower_loops.gg` /
  `lower_expr.gg`) + Rust `for_loops.rs`/`collections.rs` + tests/integration.rs — do NOT
  touch those (NOTE: parser/ast under the typechecker dir are SYMLINKED to the lowerer's —
  the scout's patch avoids them; keep it that way, and if the patch fails to apply because of
  drift from the other executor's landing, STOP-AND-REPORT rather than hand-merging).
- Parity regen is NOT yours — the parent runs it at integration (the harness is fixed and
  fast; the scout report's "OOMs solo" line is STALE — capped-drain landed `355a73ea`).
- Checkpoint `/tmp/flip_exec_progress.md` after every gate. STOP-AND-REPORT on any conflict,
  gate failure, or surprise. Worktree rules: standard (verify pwd on entry; relative paths;
  no stash; explicit staging).

Final message: commit hash + branch, the 5-fixture before/after matrix, every gate result
verbatim (counts, not adjectives), the staged file list, and anything smelly.
