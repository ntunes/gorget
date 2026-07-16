# Scout Report — self-host reject-diagnostic rendering (four-lane-green completion)

Agent `a297e426136bd0eca`, 2026-07-16. Prototype `scouts/patches/selfhost-reject-diagnostic-proto.patch`
(4 files, +114/-29; applies CLEAN to main).

## VERDICT: FEASIBLE — render + conformance + one-fixture migration MEASURED green; blast radius ZERO
The self-host now emits the ratified `error[E_<code>]:` family off the TYPED `DiagKind`. `reject_use_after_move.gg`
flips MISMATCH→MATCH on the self-host lane; a migrated `reject_double_move.gg` is four-lane green.

## Root cause + the DESIGN CRUX (the DkDoubleMove split)
- `tests/fixtures/self_host_typechecker/diagnostic.gg:293` built the headline as `severity_str(sev) + ": " +
  message` — no `[E_<code>]` segment → bare `error:`. (`diagnostic.gg` lives in `self_host_typechecker/`,
  symlinked into `self_host_lowerer/`; the parser has a separate copy with no `render_diagnostic`.)
- **Crux:** `DkUseAfterMove` was OVERLOADED for BOTH use-after-move (`typecheck.gg:1178`) AND double-move
  (`typecheck.gg:1207`) — one kind, two registry codes. A typed map (layering rule 2) can't name two codes from
  one kind → the scout SPLIT off a new **`DkDoubleMove`** variant (`typecheck.gg:1207` → `DkDoubleMove`). This
  ALSO aligns the self-host with production, which distinguishes E_UseAfterMove vs E_DoubleMove — a correctness
  improvement, not just cosmetics. Registry source of truth: `spec/prose/diagnostic-codes.md` +
  `src/semantic/errors.rs:715`.

## The DiagKind→E_code enumeration (the design decision — typed map, SevError only)
CODED 1:1: DkUseAfterMove→E_UseAfterMove · **DkDoubleMove(NEW)→E_DoubleMove** · DkMoveInLoop→E_MoveInLoop ·
DkMoveWithoutOperator→E_MoveWithoutOperator · DkBorrowConflict→E_BorrowConflict · DkLocalBorrowBind→
E_LocalBorrowBind · DkPrimitiveTraitImpl→E_PrimitiveTraitImpl · DkUndefinedName→E_UndefinedName ·
DkDuplicateDefinition→E_DuplicateDefinition · DkNotAType→E_NotAType.
**CODELESS (bare `error:`, deliberate — multiplex multiple codes, so emitting one would be WRONG):**
`DkTypeMismatch` (multiplexes E_TypeMismatch/E_ValueOutOfRange/E_StringIndexAssign/E_MainThrowsNonInt/
E_DefaultOpNonOptional/E_DerefNonBox) · `DkControlFlow` (multiplexes E_Break/Continue/ReturnOutsideLoop/
E_ThrowInNonThrowingFunction/E_PositionalAfterNamed/E_RequiredAfterDefault/E_DoubleAwait) · DkParseError/
DkLexError (parse/lex families not yet coded) · DkUnreachable (reserved). Codes render ONLY for `SevError`
(warnings/notes stay bare, per registry).

## Measured evidence
- Self-host stderr headlines (driver on the liveness reject fixtures, ANSI-stripped):
  `error[E_UseAfterMove]: use of \`s\` after it was moved` · `error[E_DoubleMove]: \`s\` moved more than once
  (double move)` · `error[E_MoveInLoop]: cannot move \`n\` out of an enclosing scope inside a loop …`. Raw bytes
  keep `error[E_UseAfterMove]` CONTIGUOUS (`\x1b[38;5;9merror[E_UseAfterMove]\x1b[0m`), so the harness's
  `extract_reject_code`'s `find("error[")` reads it.
- Self-host lane conformance: `spec_conformance_selfhost` → **total=198 · MATCH=198 · MISMATCH=0**;
  `reject_use_after_move.gg` = MATCH (was the held MISMATCH).
- **Blast radius ZERO (measured, not reasoned):** `cargo test --test integration self_host_driver` → 18
  passed / 0 (incl. all reject/accept driver tests, d12→E_MoveWithoutOperator, d10b→E_BorrowConflict); the
  codeless kinds still render bare. `type_comparison` passed (the DkDoubleMove split is clean). Swept all
  `tests/*.rs`: NO assertion checks exact stderr equality, code-absence, or a bare self-host `error:`; no golden
  snapshot. Nothing needed its expectation fixed.
- Migrated `spectests/run/reject_double_move.gg` (ggdef-`gen`'d: exit 1 / stdout "" / reject E_DoubleMove):
  ggdef 198/198 · C 198/198 · LLVM 198/198 · self-host 198/198 — **four-lane green.**

## Patch (`scouts/patches/selfhost-reject-diagnostic-proto.patch`, 4 files +114/-29)
`spectests/run/reject_double_move.gg` (new) · `diagnostic.gg` (DkDoubleMove + `diag_kind_code` map + headline)
· `typecheck.gg:1207` (DkUseAfterMove→DkDoubleMove) · `tests/spec_conformance.rs` (C/LLVM/SELFHOST/MIN_FIXTURES
floors → 198 observed + narrative). Bootstrap-gated (self-host source; symlinked so the lowerer driver picks it
up automatically — single source of truth).

## Bulk-migration plan (executor) — the remaining driver-only reject fixtures
For each in `self_host_driver_rejects_liveness` (`integration.rs:18886`): create `spectests/run/reject_<name>.gg`,
`ggdef gen` its expect, verify four-lane MATCH, bump every floor to the re-run observed total (never hardcode):
`reject_move_in_loop.gg` (E_MoveInLoop) · `reject_use_after_move_branch.gg` (conditional-move-then-use,
E_UseAfterMove) · `reject_consuming_self_use_after_move.gg` (E_UseAfterMove) · `reject_consume_callable_double.gg`
(E_DoubleMove via ConsumeCallable). Then remove those entries from `self_host_driver_rejects_liveness` (now
covered stronger) + fix the stale comments (§ snags). Regenerate floors with `GG_PARITY_FLOOR_OFF=1` /
`GG_GGDEF_CONFORMANCE_FLOOR_OFF=1`, read the printed `total=`, set floors to it.

## SNAGS / DESIGN FORKS
1. **CROSS-ZONE FLOOR — `GGDEF_MATCH_FLOOR`.** Any `spectests/run/` fixture bumps ALL FOUR lane floors, but the
   ggdef floor is at `spec/ggdef/tests/spec_conformance_ggdef.rs:47` (`=197`) — the scout's brief FORBADE that
   zone. Suite is GREEN (ggdef actual 198 ≥ 197) but the gain isn't LOCKED. The executor (NOT zone-restricted the
   way the scout was — no collision, the codegen track doesn't touch spec/ggdef) must bump `GGDEF_MATCH_FLOOR`
   197→198 (and per-fixture thereafter) to lock it.
2. **Design fork — coarse codeless kinds → separate follow-up (FILED).** `DkTypeMismatch` + `DkControlFlow` each
   multiplex 5-7 production codes → left CODELESS (emitting one would be WRONG). Self-host rejects for those kinds
   can't be E_-code-conformance-compared until the self-host SPLITS those kinds 1:1 with the registry. NOT needed
   for the move/liveness migration set (all coded). Filed to TODO.
3. **Stale comments now factually false** (not assertions — false historical record): `integration.rs:18602-18603,
   18731, 18860, 18873` still say "the self-host renderer has no error[E_…] codes" / "emits a bare error:". After
   this patch d10b→E_BorrowConflict, d12→E_MoveWithoutOperator, liveness→codes. Left OUT of the minimal patch;
   the executor sweeps them during the migration (when the liveness list is rewritten anyway).

## Bootstrap gate
`self_host_bootstrap_fixed_point` launched DEBUG but KILLED incomplete (~15+ min under contention; freed CPU for
the concurrent agent). The driver BUILT + RAN correctly with the patch (produced all three E_ headlines) → the
self-host source compiles. The byte-identical fixed-point is the PARENT's authoritative `--release` integrate gate.
