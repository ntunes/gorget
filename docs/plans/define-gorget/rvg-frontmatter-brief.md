# Executor brief: RV-G — gen⇄parse frontmatter biconditional (gen refuses codeless exit-1)

> **Status:** v1 — pass-1 AND pass-2 both SIGNED OFF, zero reservations (pass-2 additionally closed the writer-CLASS question: gen_frontmatter is the SOLE machine writer — migrate routes through it, smith is in-process, hand-authored blocks are the reader's guard; and probed the refusal is not over-broad). Pass 3 = the final ≥3-rule pass. (pass-1: brick + design grounding + patch +
> gates + blast-zero all independently re-verified, incl. a live refusal probe leaving the file
> byte-identical). Two non-blocking notes folded below. Passes 2-3 still required (≥3 rule).
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

1. **M1 — apply the proven patch** (`git apply --check` first; repo-root-relative path above):
   `spec/ggdef/src/lib.rs` — new `GenError::CodelessIllFormed(String)` + Display + the guard in
   `gen_frontmatter` (refuse BEFORE rendering; the target file must remain unchanged on
   refusal); `spec/ggdef/src/tests.rs` — the `gen_output_always_parses_round_trip` guard
   (Core #6): every generatable outcome (Value / Trap+T_ / elaborate-reject+E_ /
   FuelExhausted@103) round-trips gen→parse; both codeless shapes assert refusal. The
   while-move arm must stay RV-H-INDEPENDENT (it accepts a future coded reject once RV-H
   lands — assert "refuses OR parses", never pin the current hole).
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

The brick is impossible by construction: everything gen writes, parse accepts (round-trip
guard green); codeless IllFormed programs refuse loudly at gen time with the file untouched;
ggdef 140/0; conformance compile-clean; zero corpus churn; two files changed.
