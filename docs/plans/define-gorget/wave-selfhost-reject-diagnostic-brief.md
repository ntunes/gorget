# Executor Brief — self-host reject-diagnostic rendering + the four-lane reject migration

**Track:** self-host reject-diagnostic (owner-ruled HIGH; the four-lane-green completion of the ggdef
verdict-triple landing). **Base:** main (re-check the patch applies). **BOOTSTRAP-GATED** (self-host SOURCE:
`diagnostic.gg`/`typecheck.gg`). Zone: `tests/fixtures/self_host_typechecker/{diagnostic,typecheck}.gg` ·
`spectests/run/*.gg` · `tests/spec_conformance.rs` · `spec/ggdef/tests/spec_conformance_ggdef.rs` (the
cross-zone GGDEF floor) · `tests/integration.rs` (the liveness driver-list + the stale comments). Parent gate =
full C+LLVM sweep + `self_host_bootstrap_fixed_point`.

## 0. WORKTREE PREAMBLE (non-negotiable)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch `/workspace/gorget`
(main) / `/workspace/gorget-1`. Worktree-RELATIVE paths only. Stage by file name (NEVER `git add -a`/
`commit -a`). NEVER `git stash` — `git diff > /tmp/shdiag_<name>.patch`. Checkpoint to /tmp EARLY. FINAL gates
FOREGROUND. On an Edit desync, re-Read + retry — never a shell heredoc with an absolute path. **A concurrent
track (is-scrutinee single-eval) also edits `tests/integration.rs` — but a DIFFERENT region (a new fixture near
`is_bindings`); you edit the LIVENESS driver-list + the stale comments. Stay in your region.**

## 1. GROUND IN THE SCOUT (read first)
- Scout report: `docs/plans/define-gorget/scouts/scout-selfhost-reject-diagnostic.md`. The self-host REJECTS
  use-after-move correctly but rendered a BARE `error:` headline (`diagnostic.gg:293`), not the ratified
  `error[E_<code>]:`. **Design crux:** `DkUseAfterMove` was OVERLOADED for use-after-move AND double-move → the
  scout split off a new `DkDoubleMove` variant (a typed `DiagKind→E_code` map can't name two codes from one
  kind — layering rule 2; and it aligns the self-host with production's E_UseAfterMove/E_DoubleMove
  distinction). Registry source of truth: `spec/prose/diagnostic-codes.md` + `src/semantic/errors.rs`.
- The move/liveness reject family is ALL coded; the COARSE kinds `DkTypeMismatch`/`DkControlFlow` multiplex 6-7
  codes each and stay CODELESS (bare `error:`) — that split is a SEPARATE filed HIGH follow-up. Do NOT migrate
  type/control-flow reject fixtures in this landing.

## 2. APPLY THE PROVEN PATCH (verify, don't re-derive)
`git apply docs/plans/define-gorget/scouts/patches/selfhost-reject-diagnostic-proto.patch` (4 files, +114/-29,
applies CLEAN). It: adds `DkDoubleMove` + the typed `diag_kind_code` map + the `error[E_<code>]:` headline for
`SevError` in `diagnostic.gg`; retargets the double-move site `typecheck.gg:1207` `DkUseAfterMove→DkDoubleMove`;
adds `spectests/run/reject_double_move.gg` (four-lane, ggdef-`gen`'d expect `reject: E_DoubleMove`); sets
`C/LLVM/SELFHOST/MIN_FIXTURES` floors to 198 in `tests/spec_conformance.rs`.
VERIFY (measured): build the self-host driver (`GG_BUILD_TIMEOUT_SECS=600`); run it on the liveness reject
fixtures → confirm `error[E_UseAfterMove]:`, `error[E_DoubleMove]:`, `error[E_MoveInLoop]:` on stderr (paste);
`cargo test --test spec_conformance` self-host lane → `reject_use_after_move.gg` MATCH (was the held MISMATCH).

## 3. THE CROSS-ZONE GGDEF FLOOR (the one thing the scout's zone left undone)
Adding a `spectests/run/` fixture bumps ALL FOUR lane floors, but the ggdef lane floor lives at
`spec/ggdef/tests/spec_conformance_ggdef.rs:~47` (`GGDEF_MATCH_FLOOR`, currently 197) — OUTSIDE the scout's
zone. The suite is green as-is (ggdef actual ≥ floor) but the gain isn't LOCKED. Bump `GGDEF_MATCH_FLOOR` to the
OBSERVED ggdef `total`/`MATCH` (regenerate — after this patch it's 198; after the §4 migration it rises per
fixture). No collision — the concurrent is-scrutinee track does NOT touch `spec/ggdef/`.

## 4. THE BULK REJECT-FIXTURE MIGRATION (four-lane-green — now that the self-host emits codes + ggdef-elaborate rejects them)
The 4 remaining reject fixtures are currently DRIVER-ONLY in `self_host_driver_rejects_liveness`
(`tests/integration.rs:~18886`). Migrate each to a four-lane conformance spectest:
| new `spectests/run/` fixture | source driver fixture | code | ggdef rejects via |
|---|---|---|---|
| `reject_move_in_loop.gg` | `move_in_loop_reject` | E_MoveInLoop | `liveness.rs` may-move |
| `reject_use_after_move_branch.gg` | `use_after_move_branch_reject` (conditional-move-then-use) | E_UseAfterMove | elaborate union (the elab∘eval win) |
| `reject_consuming_self_use_after_move.gg` | `consuming_self_use_after_move_reject` | E_UseAfterMove | may-move |
| `reject_consume_callable_double.gg` | `consume_callable_double_reject` | E_DoubleMove | consume-call kill |
For EACH: copy the `.gg` source into `spectests/run/` **AND wrap it in the `#!spectest … #!end` frontmatter
scaffold** (mode/adjudicator/features/doc + an EMPTY `expect:`) using the patch's `reject_double_move.gg` as the
TEMPLATE — `ggdef gen` FILLS `expect:` but does NOT scaffold the fence (a bare fixture errors "no #!spectest …
#!end frontmatter fence"). THEN `cargo run -p ggdef -- gen spectests/run/<f>.gg` to fill the `expect:` block
(confirm it records `exit: 1` / `stdout: ""` / `reject: E_<code>`, NOT the message); run all four lanes and
confirm **four-lane MATCH** (ggdef + C + LLVM + self-host all reject with the right code — mechanism-confirmed
via ggdef's `liveness.rs` may-move pass, but VERIFY per-fixture; if any fixture does NOT reject four-lane on
ggdef, **FILE it** ("Don't redesign around compiler gaps") rather than force-remove it); then REMOVE the
four-lane-verified entries from the `self_host_driver_rejects_liveness` `reject_fixtures` list (now covered
stronger cross-lane) — **update the array-size annotation `[(&str, &str); 9]` → the ACTUAL remaining count**
(`; 5]` if all 4 migrate; a hard compile error if the number is wrong) — and KEEP the `accepts_liveness` list
untouched. **FLOORS: regenerate, do NOT hardcode** — each
four-lane-MATCH fixture bumps `MIN_FIXTURES` + all four lane floors (both files) by 1; run each lane with the
floor-off env (`GG_PARITY_FLOOR_OFF=1` / `GG_GGDEF_CONFORMANCE_FLOOR_OFF=1`), read the printed `total=`, set the
floors to it in the SAME commit. **AND update the floor-block DOC-COMMENT NARRATIVES in BOTH files to the
regenerated total + the full migrated-reject set** — `tests/spec_conformance.rs:~105-112` (the `MIN_FIXTURES`
"= N (…enumerated reject fixtures…)" narrative + the C/LLVM/SELFHOST floor comment — the patch already rewrote
this for the 198 step; EXTEND it to the final total and name the newly-migrated rejects) AND
`spec/ggdef/tests/spec_conformance_ggdef.rs:~33-42` (the sample-run "→ total=N · MATCH=N" + the fixture
enumeration above `GGDEF_MATCH_FLOOR`). Leaving these prose narratives stale MANUFACTURES exactly the
false-historical-record the §5 sweep exists to remove — treat them as part of §5.

## 5. THE STALE-COMMENT SWEEP (false historical record — fix it, per CLAUDE.md)
`tests/integration.rs:~18602-18603, 18731, 18860, 18873` still assert in PROSE "the self-host renderer has no
`error[E_…]` codes" / "emits a bare `error:` headline." After this landing that is FALSE (d10b→
`error[E_BorrowConflict]`, d12→`error[E_MoveWithoutOperator]`, liveness→the codes). Rewrite these comments to
match reality (the self-host now emits the `error[E_<code>]:` family for coded `SevError` kinds; only the coarse
`DkTypeMismatch`/`DkControlFlow` kinds remain codeless — cite the filed follow-up). Do this when you rewrite the
liveness reject-list anyway.

## 6. GATES + REPORT
**Executor FOREGROUND gates:** `cargo test --test spec_conformance` (all lanes green at the REGENERATED floors —
paste the per-lane `total/MATCH/MISMATCH`) · `cargo test -p ggdef` (the ggdef lane + its bumped floor; the
`gen`-recorded reject codes) · the TARGETED `self_host_driver_accepts_liveness` + `self_host_driver_rejects_liveness`
integration tests (`--release` to dodge DEBUG-concurrency timeouts) · `cargo test --lib`. **Do NOT run the full
`cargo test --test integration` sweep or the bootstrap fixed-point — those are the PARENT's gates.** No
`LANDED`/`DONE` breadcrumb in `TODO.md`.
**Report:** commit hash; the self-host `error[E_<code>]:` headlines (paste); the four-lane MATCH for EVERY
migrated fixture (per-lane counts, REGENERATED); the regenerated floors (both files, incl. GGDEF); confirm the
`self_host_driver_rejects_liveness` entries were removed for the migrated fixtures + `accepts_liveness` kept;
confirm the stale comments are rewritten; confirm the coarse `DkTypeMismatch`/`DkControlFlow` kinds stay codeless
(NOT wrongly coded); `git -C /workspace/gorget status` CLEAN.
**PARENT at integrate (NOT the executor):** the full C+LLVM integration sweep (`--release`) +
`self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — bootstrap-gated (self-host source).
**SERIALIZE the bootstrap with the is-scrutinee track** (both change self-host source: `diagnostic.gg`/
`typecheck.gg` here, `resolve.gg` there): land one, re-establish the fixed-point + sweep, rebase the other, land
it. Then move the self-host reject-diagnostic TODO entry to `DONE.md`.
