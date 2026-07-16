# Executor brief: RV-G — gen⇄parse frontmatter biconditional (gen refuses codeless exit-1)

> **Status:** v4 — pass-5 folded (2 LOW, exact-fix: the stale 'rendered block' wording in the pass-3 summary; UnparseableRender payload pinned to FrontmatterError). Pass-5 PROTOTYPE-PROVED the class upgrade: compiles clean, 140/0, backstop unreachable on all four generatable arms, exhaustive witness compiles. Pass-4 folded (2: the self-validation mechanism CORRECTED to parse the SPLICED would-be file per migrate's idiom — the bare rendered block would NoFence-fail every call; the phantom '+N' count hedge dropped — exactly 140/0). Pass-3 folded (2 LOW: the stale status clause removed; the CLASS upgrade
> below — gen SELF-VALIDATES its SPLICED would-be output through `parse_frontmatter` before writing, so
> the biconditional holds by construction for any future Outcome variant, plus an exhaustive
> `match Outcome` witness in the test). Pass-1 and pass-2 signed off zero-reservations (pass-2
> closed the writer-class question: gen_frontmatter is the SOLE machine writer). Awaiting the
> confirming pass.
> **Scout basis (read both FIRST):**
> `docs/plans/define-gorget/scouts/scout-rvg-frontmatter.md` (brick repro + design trade-off
> table; ⚠ its §6 cites a stale `/tmp/recover_rvg_proto.patch` path — the DURABLE copy is the
> one below) and the proven patch
> `docs/plans/define-gorget/scouts/patches/rvg_frontmatter_proto.patch`
> (2 files, +124; measured 139/0 → 140/0). **Model policy:** executor + brief-reviews Opus;
> output-review on Fable.

## Objective

Close the writer⇄reader asymmetry that can brick every conformance lane: `ggdef gen` on a
codeless eval-internal `IllFormed` program (no `main`; eval defense-in-depth catches like the
RV-H while-condition moved-read) writes `#   exit: 1` with no `#   reject:` line and exits 0 —
and `parse_frontmatter`'s ratified biconditional (`frontmatter.rs:267-274`,
`RejectExitWithoutCode`) then hard-fails that file for ALL four lanes (`run_lane` asserts
`frontmatter_errs` empty, floor-off notwithstanding).

**The ratified fix shape is (b): gen REFUSES.** Grounded in `verdict = elaborate ∘ eval`
(decisions.md): elaborate owns every ratified static rejection and emits its E_ code; eval's
own IllFormed is defense-in-depth with NO ratified code — therefore NOT a generatable
conformance outcome (the production lanes cannot reproduce it as a coded verdict). Rejected
alternatives, for the record: (a) fake E_ codes for eval-internal states (unsound; papers over
RV-H-class gate holes), (c) a new exit code (re-fragments the just-ratified 0/1/2/101/103
taxonomy), (d) an `illformed:` frontmatter marker (adds an uncomparable channel; enables
permanently-red fixtures). (b) keeps the verdict triple and exit-code scheme byte-unchanged,
makes writer⇄reader agree by construction, and keeps gate holes VISIBLE via a loud refusal.

## Milestones

1. **M1 — apply the proven patch, THEN add the pass-3 class upgrade** (`git apply --check`
   first; repo-root-relative path above):
   `spec/ggdef/src/lib.rs` — new `GenError::CodelessIllFormed(String)` + Display + new `GenError::UnparseableRender(FrontmatterError)` + its Display arm (the class backstop's error type) + the guard in
   `gen_frontmatter` (refuse BEFORE rendering; the target file must remain unchanged on
   refusal); `spec/ggdef/src/tests.rs` — the `gen_output_always_parses_round_trip` guard
   (Core #6): every generatable outcome (Value / Trap+T_ / elaborate-reject+E_ /
   FuelExhausted@103) round-trips gen→parse; both codeless shapes assert refusal. The
   while-move arm must stay RV-H-INDEPENDENT (it accepts a future coded reject once RV-H
   lands — assert "refuses OR parses", never pin the current hole).
   **Class upgrade (pass-3, mechanism CORRECTED by pass-4 — ~10 lines beyond the patch):**
   (i) in `gen_frontmatter`, SPLICE first, then self-validate the SPLICED would-be file —
   NOT the bare rendered block (`parse_frontmatter` requires the full `#!spectest`…`#!end`
   fenced source; its first act is `fence_body`, so a bare block would `NoFence`-fail on
   EVERY call). Shape: `let spliced = splice_expect(source, &block)?;
   parse_frontmatter(&spliced).map_err(|e| GenError::UnparseableRender(..))?; Ok(spliced)` —
   the exact idiom `migrate` already uses (`lib.rs:325-329`, the proven model). Keep the
   specific `CodelessIllFormed` guard FIRST (it owns the cause-naming error message); the
   spliced-parse check is the class backstop and must stay UNREACHABLE for the current
   taxonomy (no extra assert needed — pass-4 verified the round-trip test transitively pins
   both: a spurious backstop fires the generatable arms' unwraps; a mis-ordered guard trips
   the codeless arms' panic). (ii) in the round-trip test, add an exhaustive `match` witness
   over `Outcome` (no wildcard; structural exhaustiveness compile-forces a 5th variant even
   over a fabricated scrutinee). The prototype's wildcard arms in `render_expect_block` stay
   as-is (the spliced-parse now covers them).
2. **M2 — gates (foreground, ggdef-scoped; NOT bootstrap-gated):** `cargo test -p ggdef --lib`
   (expect 140/0) · `cargo test -p ggdef` full (all sub-suites incl. `gen_idempotent` +
   `converter_agreement`) · `cargo test --test spec_conformance --no-run` (compile-check) ·
   manual probe: `ggdef gen` on a /tmp no-main program refuses loudly (nonzero exit, file
   untouched) and still generates a normal Value fixture cleanly.

## Out of scope

- The RV-H gate hole itself (while-condition moves — own three-lane track; the refusal message
  may NAME the shape, the fix must not mask it).
- Any spectests/ regeneration (blast radius is ZERO on the committed corpus — scout verified
  all 202 seeds parse and none is codeless; if you find one that regenerates differently,
  STOP and report).
- The exit code of the refusal itself (`EXIT_USAGE`=2, scout-noted as defensible) — keep as
  the prototype has it.

## Process contract (non-negotiable)

Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your
worktree. NEVER touch `/workspace/gorget` (main) or `/workspace/gorget-1`; worktree-relative
paths only (your worktree nests UNDER the main checkout). NEVER `git stash`; checkpoint to
/tmp after each milestone. Stage by EXPLICIT file name (`spec/ggdef/src/lib.rs`,
`spec/ggdef/src/tests.rs` only). On an Edit-tool desync, re-Read and retry the Edit tool.
Under multi-agent cargo contention a TRANSIENT dependency-compile error (e.g. regex
E0463/E0282) can appear once and vanish on retry — retry before diagnosing (the documented
worktree-launch-reset pattern).
Commit when green (`fix(ggdef): gen refuses codeless eval-internal IllFormed — gen⇄parse
round-trip guaranteed (RV-G)`), trailers: Co-Authored-By Claude Opus + the Claude-Session
line. Report any NEW pre-existing bug (file-don't-fix).

## Acceptance

The brick is impossible by construction: gen SELF-VALIDATES the SPLICED would-be file through
`parse_frontmatter` before writing (the class guard, migrate's proven idiom) and refuses
codeless IllFormed with the cause-naming message (the instance guard); the round-trip test's
exhaustive `match Outcome` witness compile-forces coverage of future variants; ggdef
**exactly 140/0** (the class upgrade adds match arms and assertions inside the existing test,
not a new test fn — the count does not move); conformance compile-clean; zero corpus churn;
two files changed.
