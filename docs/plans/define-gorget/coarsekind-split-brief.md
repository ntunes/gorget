# Executor brief: coarse-kind diagnostic split (DkTypeMismatch + DkControlFlow → per-code kinds)

> **Status:** v0 — awaiting ≥3 sequential fresh brief-reviews (fold after each; stop only on a clean pass).
> **Scout basis (read both FIRST):** `scouts/scout-coarsekind-split.md` (verified emit-site table,
> measured before/after, blast radius) + `scouts/patches/coarsekind_split_proto.patch` (the PROVEN
> full split — 14/0 driver rejects emitting contiguous `error[E_<code>]:`, floor-neutral).
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

1. **M1 — apply the proven split** from `scouts/patches/coarsekind_split_proto.patch` onto the
   current tip (re-read each hunk if it has drifted; the patch was proven on `b57cf993`-era
   source). 3 files. Includes the `infer.gg:24` dead-import fix (DkControlFlow/DkTypeMismatch
   imports that nothing uses).
2. **M2 — tighten the driver tests**: upgrade the 7 existing `self_host_driver_rejects_*`
   assertions that cover these families from `contains("error")` to asserting the exact
   `error[E_<code>]` (each code per the scout's table).
3. **M3 — add 4 new driver reject tests** for the previously-uncovered codes: deref-non-box,
   main-throws-non-int, break-outside-loop, continue-outside-loop (minimal .gg reject fixtures +
   assertions on the exact code; follow the existing reject-fixture layout and allowlist stanzas —
   fixtures must not be gitignore-hidden, verify with `git status` after adding).
4. **M4 — gates (all FOREGROUND, generous timeouts; chunk any >600s gate by test name)**:
   self-host driver rebuild (`GG_BUILD_TIMEOUT_SECS=600`) · `self_host_driver_rejects_*`
   (expect 14/0 pre-M3, 18/0 after) · `self_host_driver_accepts_*` 3/0 · `type_comparison`
   diagnostic run (print the counts) · `cargo test --lib` · `cargo test --test lints` ·
   `cargo test -p ggdef` (cheap insurance; no expectations flip — the split is floor-neutral,
   `spec_conformance` floors stay at their current value; do NOT touch spectests/).

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
empty stdout; 18/0 rejects · 3/0 accepts · lib/lints/ggdef green; zero spectests/floor movement;
zero changes outside the 3 self-host files + tests/integration.rs (+ new fixture files).
