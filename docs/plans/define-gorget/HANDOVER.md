# Define Gorget — Orchestration Handover

> **Purpose:** let ANY fresh session (including a less-context-heavy model, e.g. Opus) pick up
> this project mid-stride without reconstructing the reasoning. Per repo rules this file holds
> INVARIANTS AND COMMANDS, not numbers — regenerate every count before quoting it.
> **Read order for a fresh session:** this file → [`decisions.md`](decisions.md) (D1–D8 +
> directives + open queue) → [`rfc-ggc-ggdef.md`](rfc-ggc-ggdef.md) (APPROVED — the normative
> architecture) → [`phase0-brief.md`](phase0-brief.md) (the current executable work item) →
> CLAUDE.md's "Multi-agent orchestration" + "Review … with a fresh agent" sections (the
> process you MUST run).

## What this project is (one paragraph)

Gorget's ownership/CoW/drop semantics were never formally defined — four implementations
(Rust gg → C, Rust gg → LLVM, self-host, sim) disagree in known places and there is no oracle
that says who is right. This project builds the executable definition: **`ggdef`**, a small
eager-value-semantics interpreter that IS the meaning (production stays borrows+lazy-CoW — an
optimization with an observational-equivalence obligation, mandated by the D1 owner note), a
conformance suite (**`spectests/`**) whose expectations are GENERATED from `ggdef`, and
continuous differential verification. Owner-approved 2026-07-05; RFC cleared a 5-pass
sequential fresh-review gauntlet.

## Current state (update this section IN PLACE as work lands)

- **Decisions D1–D8**: all recorded in `decisions.md` with rationale. Do not relitigate; do
  bring NEW decision needs to the owner as option-questions (owner directive: ask along the
  way, with recommendations and previews).
- **RFC**: APPROVED (status line at top of `rfc-ggc-ggdef.md`). §2.2 is the semantic core.
- **Phase 0**: brief at `phase0-brief.md` (3 increments; its STATUS header is the live
  tracker). **Increments A + B1: LANDED + MERGED** (A: 26/26; B1: 75/75 non-equip gate,
  28 REPORT-ONLY recorded). **ggdef has surfaced 4 production bugs** (3 bare-param
  materialize holes + ctor named-args — TODO HIGH entries, ggdef-adjudicated expected
  outputs in spec/ggdef/reports/increment_b1.md's CORRECTION table). Call-side named args
  are REJECTED in ggdef pending B2's reorder. **Next: B2** (equip/Drop/D4/receiver-type
  inference — its brief section is review-confirmed; fold any new items, launch executor).
  The elemdrop production-fix track runs in parallel (docs/plans/elemdrop-fix-brief.md,
  signed off, executor launched 2026-07-06).
- **Known prerequisite bug** (do not lose): collection-element custom-Drop lost on named-local
  push — TODO.md HIGH entry (grep `custom Drop LOST`) — must be fixed before D4 drop-count
  spectests can gate implementations. Its fix is a SEPARATE track (src/ir zone; disjoint from
  phase 0's spec/ zone; parallelizable).
- **Parallel HIGH bugs filed this project** (each own scout→brief→reviews, disjoint from
  spec/): dead-branch alias-bind SIGSEGV; `String !p`+concat invalid-C; plain-`self`
  write-through (= the D2 implementation track); Option[T] unknown-method link-explosion.
  All in TODO.md's STRATEGIC ASSESSMENT block.

## The process you must run (non-negotiable, from CLAUDE.md)

For every work item: **scout (verify premises, file:line, measure end-to-end) → brief →
≥3 SEQUENTIAL fresh-agent review passes folding after each, until a CLEAN pass → executor in
a WORKTREE (`isolation: "worktree"`, the full preamble from CLAUDE.md rule 2) → fresh
output-review of the diff → orchestrator merges + runs the full integration sweep → TODO/DONE
bookkeeping → worktree/scratch cleanup.** Never stop on a pass that raised reservations.
Reviewers verify against CURRENT source with file:line; brief them to SIGN OFF or cite
specific reservations, never rubber-stamp. If the owner's tokens are constrained, pass
`model: "opus"` on Agent calls — the briefs in this project are written to be executable by
Opus-class agents; if an Opus agent stalls on a brief, that is a BRIEF DEFECT to fix, not an
agent failure to push through.

## Standing rules specific to this project

1. **Docs write-through**: a decision/rule lands only when `ggdef` + spectests + prose +
   `language-design.md`/book/devbook agree. The ledger tracks write-through debt.
2. **Expectations flow FROM the definition**: `ggdef -- gen` produces them; humans review the
   diff; never copy a backend's output in as truth. A spec change is justified by design
   intent, never "matches the implementation" (invariant #8).
3. **`gg sim` is permanently disqualified as the definition** (it consumes GIR) and is NOT a
   conformance lane; its disposition is a separate owner decision (TODO "gg sim disposition").
4. **Dogfood findings feed the ledger** (owner directive): ugly real-Gorget patterns are
   language-design findings, filed against `decisions.md`.
5. **The import ratchet is the fence**: the ggdef crate may import lexer/parser/AST/span ONLY —
   never `src/ir/` or `src/semantic/`. The lint lands in Increment A, before any evaluator code.

## What comes after phase 0 (do not start early)

Phase 1 (coverage completion + conformance floors + smith verdict lane + D4/D5/D6 rejections
in BOTH compilers + diagnostic-code registry) → Phase 2 (annexe probes + `gg explain` v0 +
context pack + the LLM-correctness KPI harness) → Phase 3 (sharing/concurrency). Full phase
definitions: RFC §6. Decision queue for the next owner batches: `decisions.md` "OPEN — queue"
(A5 get_or ownership, A10 allocator lifetimes, A12 book contracts are the likely batch 4).

## Commands (regenerate, never trust cached numbers)

```bash
cargo build && cargo test --lib && cargo test --test lints        # base gates
cargo test --test integration -- --test-threads=4                  # full sweep (parent-only)
GG_SMITH_SEEDS=1..200 cargo test --test smith -- --nocapture       # fuzzer batch
cargo run -p ggdef -- run <file.gg>                                # once phase 0 lands
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture   # parity (separate track)
```
