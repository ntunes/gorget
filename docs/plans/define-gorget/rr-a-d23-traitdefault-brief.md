# R-A brief — D23 trait-default `throws`: trait-registry keying + throws substitution

> **Round:** review-residuals (xhigh review of `f42eea96..7aad1844`, TODO High entry
> "D11/D23-wave RESIDUALS" item (a)). **Zone:** `src/semantic/` (`typecheck.rs`,
> `traits.rs`) + new fixtures under `tests/fixtures/` + one new lint in
> `tests/lints.rs` (coordinate: R-D also edits `tests/lints.rs` — DISJOINT functions,
> R-D rewrites `trap_kind_parity_prod_vs_ggdef` only).
> **Scout:** report `/tmp/scout_rr_a_report.md`, prototype `/tmp/scout_rr_a_prototype.patch`
> (2 files, +93/−28), measured end-to-end at `cab529cd`.
> **Status:** v1 — awaiting sequential fresh-review passes.

## The corrected root-cause picture (the scout REFUTED the review's mechanism for mode 1)

The review filed two reproduced failures. Both outcomes CONFIRMED; the mode-1
mechanism was one layer off (CLAUDE.md debugging heuristic: the write site was
lossy, not the read site):

- **Mode 1 (cross-module silent miscompile): the D23 gate is never reached.**
  Instrumentation shows `resolve_throws_method_ret` gets ZERO hits for the
  cross-module program. Root: `collect_trait` keys the trait registry with
  value-first `scopes.lookup` (`src/semantic/traits.rs:868`) while `process_impl`
  resolves the equip's trait via `lookup_type` (`traits.rs:989`). An import
  placeholder registers in BOTH namespaces (`scope.rs:71-92`) but
  `export_non_private` (`scope.rs:686-707`) overwrites only the TYPE namespace —
  the stale value-namespace placeholder wins the value-first lookup
  (`scope.rs:311-313`). Measured: registry key = DefId(52) (kind=Import) vs the
  equip's DefId(58) (kind=Trait) → `resolve_method`/`resolve_method_by_name` miss
  → the call types as `error_id` and unifies with anything.
  **Blast radius is bigger than throws: EVERY cross-module trait-default method is
  currently invisible to typecheck** (no arg checks, no return type;
  `validate_trait_impls` keyed dead). Single-module control rejects correctly.
- **Mode 2 (wrong-type resolution): CONFIRMED as filed.** The trait-generic name in
  `throws E` resolves in the CALLER's scope: a colliding `struct E` produces a
  spurious `E_UnconvertibleErrorPropagation` on valid propagation; a non-colliding
  unknown name resolves to `Ok(error_id)` (`types.rs:466-470`) and renders
  `` throws `<error>` ``. The `.ok()` swallow (`typecheck.rs:5338`) fires only on
  `NotAType`-class errors — real, but narrower than the review claimed.

## Design (prototyped; registration-time resolution REJECTED)

**Fix 1 — registry keying at the write site.** `lookup` → `lookup_type` in the
traits.rs registration paths: the registry key (`:868`), `extends` (`:909`), and
the builtin registrations (`:831`, `:850`, `:853`). This aligns the whole class
with `process_impl`'s `lookup_type` and the EXISTING D23 gate then fires through
the normal dispatch path — no read-site patch (CLAUDE.md invariant #1, devbook/24).

**Fix 2 — `throws_ast` rides the established substitution mechanism.** Factor
`default_sig_bindings()` out of `substitute_default_method_sig`;
`resolve_throws_method_ret` gains `receiver_type_id` and substitutes `throws_ast`
through the SAME Self/trait-param bindings as return/params, resolving with
`.unwrap_or(error_id)` — **resolution failure can never read as non-throws**
(worst case is `error_id`, which either renders `<error>` on the unhandled path or
is accepted as unsettled on the propagating path — never a silent pass-through).

**Why resolve-at-registration loses** (the orchestrator's original hypothesis —
evaluated and rejected): (1) the trait's defining scope is flattened away by
`build_registry` (`traits.rs:443-469`) — real plumbing to keep it; (2) `throws E`
must stay symbolic anyway: one default serves many equips (`Risky[String]`,
`Risky[int]`), so per-receiver binding = substitution is needed regardless, and
compound types (`throws MyErr[E]`) force a resolved-AST structure; (3) it creates
a parallel mechanism for ONE axis of a sig whose other axes ride call-site
substitution — the sidecar drift devbook/24 rule 3 bans.

**W3 — sibling-drift lint (invariant #6).** A new `tests/lints.rs` lint pinning
the traits.rs registration paths to `lookup_type` (the 5 sites above), so the
next registration path can't reintroduce the value-first read. Mutation-test it
(flip one site back, watch the lint fail, revert).

**Doc grounding:** D23 LOG `decisions.md:262-276` (totality invariant + diagnostic
contract); `docs/language-reference.md` §10.1 ("uniformly across all positions …
free functions and methods"); `t3a-d23-enforcement-brief.md:94-103` (these exact
dispatch paths); CLAUDE.md layering rules 1/3/4.

## Measured before/after (scout, all six cases)

| Case | BEFORE | AFTER |
|---|---|---|
| mode1 xmod unhandled | `OK` + runtime `281474439764913` | `` E_UnhandledThrows … throws `CalcError` `` |
| mode1 without importing the error type | same silent pass | `` E_UnhandledThrows … `CalcError` `` |
| mode1 handled/propagated | — | OK; runs print `7` / `6` (correct) |
| mode2 collide propagation | spurious `E_UnconvertibleErrorPropagation` | `OK` |
| mode2 no-collide unhandled | `` throws `<error>` `` | `` throws `String` `` |
| mode2 collide unhandled | `<error>`/collision family | `` throws `String` `` (binding beats struct `E`) |

Scout gates: lib 1105/0 · d23_ 11/0 · throws 36/0 · lints 52/0 · module 14/0 ·
trait 40/1 · import 14/1 — the two failures are the SAME self-host-driver
120s-timeout flake under box contention; both PASS at 600s (146.56s/148.57s
measured). Executor MUST run those suites with `GG_BUILD_TIMEOUT_SECS=600`.

## Fixtures to add (all assert CONCRETE names — no `<error>`, no `Result[` leak)

1. `d23_unhandled_method_traitdefault_xmod/` — negative, asserts `CalcError` in stderr.
2. Its handled/propagated positive twin (runtime output `7`/`6`).
3. Generic-throws positive (`Risky[E]` + `equip … Risky[String]`, valid propagation).
4. Collision positive + collision-unhandled negative asserting `String`.
5. Optional: cross-module `extends` default-method sibling (covers the `:909` site).

## Risks / pins (first-class — read before executing)

- **Fix 1 AWAKENS cross-module trait-default dispatch + `validate_trait_impls` for
  ALL traits, by design.** Previously-accepted programs that only "passed" because
  the method typed as `error_id` may now be properly checked — new rejections are
  CORRECT (invariant #8) but must be REPORTED, not downgraded or dodged. Targeted
  suites are green; **the real gate is the parent's full both-backend sweep +
  `self_host_bootstrap_fixed_point`** — the executor does NOT run those (rule 4),
  the parent does at integration.
- **Pin: no name-based registry fallback at the read site.** The fix is the write
  site key; if some path still misses, trace the writer — do not add a
  `resolve_method_by_name` band-aid.
- **Accepted residual (file in TODO, small follow-up):** bindings-unavailable +
  trait-param throws degrades to `error_id` → `<error>` rendering on the unhandled
  path / `is_unsettled` accept on the propagating path (`typecheck.rs:5619-5623`)
  — never silently-non-throws. Name-rendering polish from the AST is the follow-up.
- **Self-host is the other half (note-only here, separate track):** the self-host
  typechecker LACKS the D23 gate entirely (no `UnhandledThrows` analog;
  `typecheck.gg:751-769`, `:1039-1045`; `traits.gg` has zero throws handling
  despite mirroring `substitute_default_method_sig` at `traits.gg:532`). Both
  compilers accept the mode-1 program → a Core-#8 both-wrong pair. This track
  fixes the Rust half; FILE the self-host D23-enforcement port (routed through the
  same `traits.gg:532` helper — the Fix-2 shape) as its own HIGH track.

## Executor protocol (multi-agent rules in full)

Worktree-isolated; worktree-relative paths only; no `git stash`; checkpoint diff
to /tmp after each work item; stage by explicit file name; final gates FOREGROUND
with generous timeouts. Base: apply `/tmp/scout_rr_a_prototype.patch`, re-derive
judgment hunk by hunk (you own it), then add the fixtures + the W3 lint.

## Gate list (executor, foreground, tee'd)

1. `cargo build`
2. `cargo test --lib` — 1105/0
3. `cargo test --test integration d23_ -- --test-threads=4` — 11 pre-existing + new fixtures /0
4. `cargo test --test integration throws -- --test-threads=4` — 36/0
5. `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration trait -- --test-threads=4`
   and same for `import`, `module` — expect the scout's counts, NO new failures
6. `cargo test --test lints` — 52 + 1 new /0; paste the W3 mutation evidence
7. Before/after transcript of all six measured cases (build the repro programs,
   paste real output)

Parent (NOT executor): full `cargo test --test integration` both backends +
`self_host_bootstrap_fixed_point` + spec_conformance at integration.
