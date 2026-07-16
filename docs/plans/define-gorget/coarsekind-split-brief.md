# Executor brief: coarse-kind diagnostic split (DkTypeMismatch + DkControlFlow → per-code kinds)

> **Status:** v1 — pass-1 review folded (3 reservations: M2's count corrected 7→10 and enumerated;
> M3 rewritten to REUSE the 4 existing fixtures + author 2 NEW ones for the uncovered codes →
> 6 new driver tests; scout's "all 12 PROVEN" corrected to 10-driver-verified). Awaiting the next
> fresh pass.
> **Scout basis (read both FIRST):** `scouts/scout-coarsekind-split.md` (verified emit-site table,
> measured before/after, blast radius; ⚠ its headline "PROVEN end-to-end (all 12 codes)" is an
> overclaim — the driver-run table covers 10 codes; E_ReturnOutsideFunction + E_DoubleAwait were
> never driven) + `scouts/patches/coarsekind_split_proto.patch` (the proven split — 14/0 driver
> rejects emitting contiguous `error[E_<code>]:`, floor-neutral; pass-1 verified it still applies
> clean, blob-hash exact).
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
2. **M2 — tighten the TEN existing coarse-family driver reject tests** (pass-1 enumerated; the
   scout's "7" was an undercount — and because every coarse code still renders `"error"`, a
   missed upgrade passes the gates silently, so upgrade ALL of these from `contains("error")`
   to the exact `error[E_<code>]`): `invalid_program` (:~18563, E_ThrowInNonThrowingFunction) ·
   `positional_after_named` (:~19039) · `positional_after_named_method` (:~19106) ·
   `default_op_non_optional` (:~19173) · `default_op_non_optional_nested` (:~19237) ·
   `required_after_default` (:~19303) · `trait_required_after_default` (:~19444) ·
   `value_out_of_range` (:~19522) · `string_index_assign` (:~19601) ·
   `string_index_compound_assign` (:~19671). (The other 4 `self_host_driver_rejects_*` are
   non-coarse — d12/d10b/liveness/duplicate-field — leave them.)
3. **M3 — add SIX new driver reject tests** covering the codes no driver test exercises:
   - Four REUSE existing committed fixtures (do NOT author duplicates):
     `tests/fixtures/deref_non_box_rejected.gg`, `main_throws_non_int_error.gg`,
     `break_outside_loop_error.gg`, `continue_outside_loop_error.gg` (each already consumed by a
     production `check_gg_fails` test — integration.rs ~:7954/:27300/:27332/:27356).
   - Two need NEW fixtures (none exist anywhere): **return-outside-function** and
     **double-await** (production's double-await coverage is a Rust unit test only). Author
     minimal .gg reject fixtures; verify they're not gitignore-hidden (`git status` shows them);
     confirm the shape actually reaches the typecheck emit site on BOTH compilers (production
     rejects with the same code). ⚠ If a shape cannot reach the emit site (e.g. the parser
     rejects `return` at top level before typecheck), STOP on that code, document it as a
     reserved-coded slot (like E_TypeMismatch), and report — do not force an artificial shape.
4. **M4 — gates (all FOREGROUND, generous timeouts; chunk any >600s gate by test name)**:
   self-host driver rebuild (`GG_BUILD_TIMEOUT_SECS=600`) · `self_host_driver_rejects_*`
   (expect 14/0 pre-M3, **20/0** after — or 18-19/0 with documented reserved slots per M3) ·
   `self_host_driver_accepts_*` 3/0 · `type_comparison` diagnostic run (print the counts) ·
   `cargo test --lib` · `cargo test --test lints` · `cargo test -p ggdef` (cheap insurance; no
   expectations flip — the split is floor-neutral, `spec_conformance` floors stay at their
   current value; do NOT touch spectests/).

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
empty stdout; **all 12 codes exercised by a driver test** (or explicitly documented as
reserved-coded slots with the reachability evidence); 20/0 rejects (or per M3's documented
slots) · 3/0 accepts · lib/lints/ggdef green; zero spectests/floor movement; zero changes
outside the 3 self-host files + tests/integration.rs (+ the 2 new fixture files).
