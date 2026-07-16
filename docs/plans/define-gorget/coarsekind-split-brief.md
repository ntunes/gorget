# Executor brief: coarse-kind diagnostic split (DkTypeMismatch + DkControlFlow → per-code kinds)

> **Status:** v2 — ✅ PASS 3 SIGNED OFF CLEAN (2026-07-16; gauntlet 3→4→0 reservations) —
> **EXECUTING**. Pass-3 extras: the double-await self-host path is structurally proven but was
> never driver-run — the 19/0 gate is its empirical confirmation; if an async-handling crash
> pre-empts the walk, that is a pre-existing bug to REPORT. Pass-2 folded: patch path
> repo-root-relative; return-outside SETTLED unreachable (reserved slot; the typecheck gate is
> structurally dead — in_function_body is never false at a reachable SReturn); gates 19/0; M3
> five tests. Pass-1 folded: M2 count 7→10 enumerated; M3 reuse-4; scout overclaim corrected.
> **Scout basis (read both FIRST):**
> `docs/plans/define-gorget/scouts/scout-coarsekind-split.md` (verified emit-site table, measured
> before/after, blast radius; ⚠ its headline "PROVEN end-to-end (all 12 codes)" is an overclaim —
> the driver-run table covers 10 codes) and the proven patch at
> `docs/plans/define-gorget/scouts/patches/coarsekind_split_proto.patch` (repo-root-relative
> path — use it verbatim from your worktree root; pass-1 verified blob-hash-exact clean apply,
> pass-2 re-verified: 3 files, diagnostic.gg +74/−16 · infer.gg +1/−1 · typecheck.gg +14/−14,
> zero spectests/).
> **Settled by pass-2 (empirical, production compiler built + probed):**
> **E_DoubleAwait IS reachable** (`async int f(): auto v = await await g()` → production
> `error[E_DoubleAwait]`; self-host emit at typecheck.gg:2286) — write its driver test.
> **E_ReturnOutsideFunction is UNREACHABLE** via any parseable program (top-level `return` is a
> PARSE error — parse_item has no Return arm; the typecheck gate `not ctx.in_function_body`
> [typecheck.gg:2888] is only false at module scope, which the parser cannot produce; the
> existing fixture's own comment agrees) — it is a RESERVED-CODED SLOT like E_TypeMismatch: the
> split still wires the code + emit site; do NOT author a fixture for it.
> **Model policy:** executor + brief-reviews on Opus; the output-review before integration on Fable
> (owner 2026-07-16).

## Objective

The self-host's two remaining COARSE diagnostic kinds multiplex several registry codes each and
were left codeless by the reject-diagnostic landing (`cbb21f28`) — they print bare `error:`.
Split them 1:1 with the registry so every reject from these families emits
`error[E_<code>]: <detail>` (verdict-triple shape: empty stdout, stderr diagnostic, exit 1).

Corrected spec (scout-verified against production; the TODO's original entry had 3 wrong code
names and a phantom 6th site):

- **DkTypeMismatch → 5 live per-code kinds**: E_ValueOutOfRange, E_StringIndexAssign,
  E_MainThrowsNonInt, E_DefaultOpNonOptional, E_DerefNonBox. The self-host never emits plain
  `E_TypeMismatch`; keep `DkTypeMismatch` itself mapped 1:1 to `E_TypeMismatch` as a
  reserved-but-coded slot (do NOT delete the kind).
- **DkControlFlow → 7 per-code kinds**: E_BreakOutsideLoop, E_ContinueOutsideLoop,
  E_ReturnOutsideFunction, E_ThrowInNonThrowingFunction, E_PositionalAfterNamed,
  E_RequiredAfterDefault, E_DoubleAwait.
- All 13 emit sites live in `tests/fixtures/self_host_typechecker/typecheck.gg` (table with
  file:line in the scout report). The parser/resolver/lexer `diagnostic.gg` copies are
  independent minimal enums that never emit these kinds — verified no-change; do not touch them.

## Milestones

1. **M1 — apply the proven split** from
   `docs/plans/define-gorget/scouts/patches/coarsekind_split_proto.patch` (repo-root-relative —
   run `git apply --check <that path>` from your worktree root first) onto the current tip
   (re-read each hunk if it has drifted). 3 files. Includes the `infer.gg:24` dead-import fix
   (DkControlFlow/DkTypeMismatch imports that nothing uses).
2. **M2 — tighten the TEN existing coarse-family driver reject tests** (because every coarse
   code still renders `"error"`, a missed upgrade passes the gates silently — upgrade ALL of
   these from `contains("error")` to the exact `error[E_<code>]`; codes spelled per pass-2):
   `invalid_program` (:~18563 → E_ThrowInNonThrowingFunction) ·
   `positional_after_named` (:~19039 → E_PositionalAfterNamed) ·
   `positional_after_named_method` (:~19106 → E_PositionalAfterNamed) ·
   `default_op_non_optional` (:~19173 → E_DefaultOpNonOptional) ·
   `default_op_non_optional_nested` (:~19237 → E_DefaultOpNonOptional) ·
   `required_after_default` (:~19303 → E_RequiredAfterDefault) ·
   `trait_required_after_default` (:~19444 → E_RequiredAfterDefault) ·
   `value_out_of_range` (:~19522 → E_ValueOutOfRange) ·
   `string_index_assign` (:~19601 → E_StringIndexAssign) ·
   `string_index_compound_assign` (:~19671 → E_StringIndexAssign).
   (The other 4 `self_host_driver_rejects_*` are non-coarse — d12/d10b/liveness/duplicate-field —
   leave them.)
3. **M3 — add FIVE new driver reject tests** covering the reachable codes no driver test
   exercises:
   - Four REUSE existing committed fixtures (do NOT author duplicates):
     `tests/fixtures/deref_non_box_rejected.gg` (→ E_DerefNonBox),
     `main_throws_non_int_error.gg` (→ E_MainThrowsNonInt),
     `break_outside_loop_error.gg` (→ E_BreakOutsideLoop),
     `continue_outside_loop_error.gg` (→ E_ContinueOutsideLoop) — each already consumed by a
     production `check_gg_fails` test (integration.rs ~:7954/:27300/:27332/:27356); no
     `.expected` companions needed.
   - One NEW fixture: **double-await** (`await await g()` inside an async fn — the proven
     reachable shape; production rejects `error[E_DoubleAwait]`). Name it with the sibling
     `*_error.gg` convention; run it through `gg fmt` BEFORE committing (top-level fixtures are
     auto-swept by glob harnesses — `fmt_idempotent` asserts formatter fixpoint on every
     fixture and runs in the PARENT's sweep, so a non-converging fixture bounces late);
     verify it's not gitignore-hidden (`git status` shows it).
   - **E_ReturnOutsideFunction: reserved-coded slot** (settled unreachable — see header). No
     fixture, no test; the split wires the code + emit site and that is the whole deliverable
     for it.
4. **M4 — gates (all FOREGROUND, generous timeouts; chunk any >600s gate by test name)**:
   self-host driver rebuild (`GG_BUILD_TIMEOUT_SECS=600`) · `self_host_driver_rejects_*`
   (expect 14/0 pre-M3, **19/0** after M3) · `self_host_driver_accepts_*` 3/0 ·
   `type_comparison` diagnostic run (print the counts) · `gg fmt` idempotence on the new
   fixture · `cargo test --lib` · `cargo test --test lints` · `cargo test -p ggdef` (cheap
   insurance; no expectations flip — the split is floor-neutral, `spec_conformance` floors stay
   at their current value; do NOT touch spectests/).

## Out of scope (do NOT do these)

- The four-lane conformance migration of these reject families — **blocked** on the
  ggdef-elaborate axis extension (filed HIGH in TODO; separate track). No spectests/ changes.
- Any change to the may-move/liveness reject family (already coded + migrated).
- Registry/prose changes (all 12 codes already exist in the registry — the scout verified the
  production emit for each; if you find one missing, STOP and report, do not add codes).
- The ggdef under-rejection gap (filed separately).

## Process contract (non-negotiable)

Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree.
NEVER touch `/workspace/gorget` (main) or `/workspace/gorget-1`; worktree-relative paths only
(your worktree nests UNDER the main checkout — an absolute `/workspace/gorget/...` path writes
into MAIN). NEVER `git stash`; checkpoint via `git diff > /tmp/recover_coarsekind_exec_<n>.patch`
after every milestone. Stage by EXPLICIT file name only (`git add <file>...`; never `-a`/`.`).
On an Edit-tool desync, re-Read and retry the Edit tool — never a shell heredoc with an absolute
path; after any non-Edit-tool write, run `git -C <main> status` — if it shows changes, STOP and
report. Commit when green with a `feat(self-host):` message; the parent runs bootstrap
fixed-point + full C/LLVM sweeps + integration (not you). Report any NEW pre-existing bug you
find (file:line + repro) — do not fix it in this track.

## Acceptance

Every coarse-kind reject prints `error[E_<code>]:` with the correct code (scout table), exit 1,
empty stdout; **11 of 12 codes exercised by a driver test** (E_ReturnOutsideFunction is the one
documented reserved-coded slot, settled unreachable); **19/0 rejects** · 3/0 accepts ·
lib/lints/ggdef green; zero spectests/floor movement; zero changes outside the 3 self-host
files + tests/integration.rs (+ the 1 new double-await fixture).
