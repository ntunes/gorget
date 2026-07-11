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

- **⚡ ENFORCEMENT WAVE — IN PROGRESS (started 2026-07-10).** Order: trap-normalization+D23 →
  D12 → D10 → D13/D14/D17 → riders. Trap-normalization is SLICED **T1 (ggdef definition) →
  T2a (production registry+emit) → T2b (runtime-lib fold + bounds REAL locations)**; D23 is the
  DISJOINT parallel track **T3a (diagnostic+method+expr-body) + T3b (smith tier)**.
  - **T1 LANDED** (merge `d412990a`): ggdef `Fault`→closed 8-variant `TrapKind` (`code()`→`T_<Variant>`
    exhaustive-ratchet + `is_catchable()` = §10.9 subset), the 3 missing classes (assert/panic/
    unwrap_error) as typed GGC nodes, `trap:` frontmatter (`trap ⟺ exit 101`), `adjudicate` compares
    **T_ code + exit 101 ONLY** (detail impl-defined), `spec/prose/trap-codes.md`, 8 ggdef-generated
    fixtures. Production lanes MISMATCH by design until T2 (floors STAY 187). Regenerate:
    `cargo test -p ggdef` + `GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1 --nocapture`.
  - **OWNER RULINGS this wave:** bounds traps get REAL source locations (T2b, not `<unknown>:0:0`);
    conformance compares code+exit only; ggdef DUPLICATES the registry (parity lint in T2a).
  - **NEXT:** T2a (new `src/trap.rs` closed `TrapKind` mirroring ggdef's + `gorget_trap_at` runtime
    entry + reroute compiler-emit sites + both-backend `gorget_panic`→`gorget_trap_at` rewrite +
    parity lint) → T2b. T3a executing in parallel. Then D12 blast-radius scout.
  - **T3a (D23) LANDED** (merge `9d9a6d83`): unhandled `throws` in every position → clean
    `E_UnhandledThrows`; closed the 3 modes (leak / swallow / throws-METHOD silent-miscompile incl.
    a live trait-default hole) at one producer helper; expr-body widened to match block-body.
    Follow-ons: T3b (smith throws tier), 2 minor items filed in TODO (generic-error trait-default
    message substitution; general must-use-on-Result).
  - **T2a-rust LANDED** (merge `82d50b0f`): both Rust backends emit `trap[T_<Code>]: … + exit 101`;
    7/8 fixtures MATCH on C+LLVM (floors 187→194; bounds=T2b, self-host=T2a-selfhost). `src/trap.rs`
    mirrors ggdef (parity-lint-pinned); shift→`T_Overflow` + new LLVM shift check (owner ruling); a
    block_exit_labels twin-drift phi bug the LLVM shift check exposed was caught ONLY by the bootstrap
    (invariant #7) + fixed + pinned. Regenerate: `GG_BUILD_TIMEOUT_SECS=600 cargo test --test
    spec_conformance -- --test-threads=1 --nocapture` (expect C/LLVM 194, self-host 187, bounds the
    sole per-lane MISMATCH). Assert real-span (Q-D) deferred LOW = a deeper pre-existing branch-block
    span gap (impl-defined, not conformance-compared).
  - **T2a-selfhost LANDED** (merge `9bb33ec6`): self-host lane emits `trap[T_<Code>]` at the 7 direct
    sites → `spec_conformance_selfhost` 194/1 (only `trap_bounds`); floor 187→194; `self_host_trap_code_parity`
    lint pins the hand-spelled codes. Cross-frame repanic siblings LEFT as `gorget_panic` (matches
    un-rerouted Rust; both-compiler reroute filed). **T3b LANDED** (merge `d70fefe1`): D23 smith
    rejection-oracle fuzz tier (`GG_SMITH_TIER=1`, canary-proven the gate blocks a slip); D23 enforcement
    now complete (diagnostic + fuzz). **NOW 3/4 lanes emit the trap format; only `trap_bounds` remains
    on ALL lanes.**
  - **T2b LANDED — 🎯 D11 COMPLETE** (merge `c3962cd2`): `trap_bounds` flips on all lanes →
    `spec_conformance` **C/LLVM/self-host each 195/0/0 = MIN_FIXTURES**. The flagship `v[i]` bounds
    trap carries a REAL location (byte-identical C↔LLVM, owner ruling); self-host flipped FOR FREE
    (shared runtime helper). Fault re-panic descoped to a both-compiler follow-up. Output-review caught
    a blocking LLVM `trap_counter` twin-drift build-fail (2nd instance of that class) — fixed `feee30d3`.
    **All 4 implementation lanes now emit the ratified `trap[T_X]: … at file:line:col` + exit 101.**
  - **✅ THE ROUND IS DONE (2026-07-10).** This round landed T1 · T2a-rust · T2a-selfhost · T2b (D11
    complete) + T3a · T3b (D23 enforcement complete).
  - **✅ REVIEW-RESIDUALS ROUND DONE (2026-07-10, same day — owner-directed).** An xhigh 30-agent
    adversarial review of the D11/D23 wave (`f42eea96..7aad1844`) found 15 verified defects; ALL
    fixable ones are now LANDED via 4 gauntlet tracks + an inline slice (see DONE.md entries R-A/R-B/
    R-C/R-D + "Review-residuals INLINE slice"): R-D guard-tightening `874b6371` · R-A trait-registry
    keying + trait-default throws substitution `6d12c5ad` (the review's mechanism was one layer OFF —
    every cross-module trait-default method was invisible to typecheck; fixed at the write site) ·
    R-B trap-detail lifetime + `gorget_trap_fmt` 27-site dedup `6e51fd18` · R-C LLVM
    `T_UnwrapErrorOnOk` combinator guard + happy-path repair `dd05ebb8` · inline slice `38d28727`
    (sim `gorget_trap` arm, shift→T_Overflow registry doc rows, ggdef location-suffixed trap render).
    **Round-close verification (all regenerated 2026-07-10): full C sweep 1579/0/7ign · full LLVM
    sweep 1579/0/7ign · spec_conformance C/LLVM/self-host each 195/195 MATCH 0 MISMATCH 0 BUILD-FAIL ·
    lib 1105/0 · lints 53/0 · ggdef 104/0 · parity regen 1147/1219 = 94.1%** (MATCH +37 abs vs
    post-R43; denom grew 1177→1219 with the waves' fixtures; WRONG 10 · CC-FAIL 51 · CRASH 11 —
    2 of those are R-C's new combinator fixtures crashing on the SELF-HOST lane, filed (c3)).
    **The gauntlet surfaced SIX new pre-existing defect classes, all FILED with measured repros, none
    fixed this round: (a2) self-host lacks the D23 gate entirely · (a3) supertrait defaults un-gated
    (no extends walk) · (a4) generic-trait-default `throws E` bodies lower unsubstituted → silent
    wrong values · (c2) unwrap-family on static receivers returns garbage BOTH backends · (c3) the
    self-host combinator-lane crash · the `ast_type_to_resolved` Import-placeholder cousin.** These +
    the wave-2 (f) cleanup residue (cstr-marshal dedup, typed CallExtern routing, unwrap name-match
    fallback) + the escalated `block_exit_labels` fix-(b) track (measured NOT drop-in, ~44-site
    audit, 4 known instances) are the review-residuals backlog — each needs its own gauntlet.
    PROCESS NOTE: FOUR rule-9 stalls this round, all on >600s gates; the sanctioned recovery is the
    CHUNKED-FOREGROUND pattern (split suites by test name) — brief it explicitly in future executor
    prompts. **Per the owner directive, the enforcement wave continues with a fresh agent's round.**
    Do NOT start D12/D10/D13-17/riders — those are the next round's work.
  - **⚡ DESIGN SESSION 2026-07-11 (owner + orchestrator, post-round): FOUR new rulings + the wave
    RE-SEQUENCED.** The A33+fault-model design scout ran (mandate
    `a33-fault-model-scout-mandate.md`, report preserved at `scouts/scout-a33-fault-model.md` —
    headline: ZERO organic fault-catch uses anywhere; §10.5 is STALE, single-call-DEEP catch is
    shipped; ~2,000-line machinery; Pony prior art for fallible operators). Ratified in-discussion:
    **D27** (sigil economy: `^`=move [was `!`], `!`=error channel, `?`=optionals) + **D28** (`**`
    power operator, full package). RECOMMENDED-pending-formal-ratification: **D24** (Task-join-only
    supervised boundary, `TaskFault` all-8-codes) · **D25** (REMOVE fault-catch, Swift model, gated
    on D26) · **D26** (fallible operators `+! -! *! /! %!` + `**!`, prelude `ArithError`). Deep/
    dynamic catch REJECTED on the merits (ledger LOG + the scout's "why not dynamic exceptions"
    appendix). **THE CENSUS RAN AND THE PLAN IS RATIFIED (2026-07-11, same day):** report at
    `scouts/scout-wave-census.md`; **D24 + D25 + D26 ratified in the packet review — DECISION
    BATCH 5 CLOSED (D24-D28)**. **THE LIVE EXECUTION PLAN is TODO.md's "RATIFIED
    ENFORCEMENT-WAVE PLAN" entry: Batch A (D19 + D12 straight-to-error + D10(a)) → Batch B
    (D10(b) place-overlap + the in-repo hand-hoists) → Batch C (C1 D26+D28 operators → C2 D25
    fault-catch removal [~2,000-line machinery deletion; ships the D24 spec prose + §10.5/§10.9
    rewrite] → C3 the composed one-fmt-pass sweep: ~1,114 sigils + 208 `.slice()` + 7 `pow()`
    per IN-REPO corpus). gorget-js/arena/gglox/gorget-conformance migrate in a LATER
    coordination round (owner ruling). Every track: brief → ≥3 sequential fresh reviews →
    worktree executor → output-review → integrate; surprises are REPORTS, not downgrades.**
    After Batch C: the out-of-repo coordination round → D13/D14/D17 → remaining riders;
    A31/A32 design scouts interleave.
  - **⚡ LIVE BATCH-A STATE (update IN PLACE at every track state-change — owner directive
    2026-07-11: subagents run `model:"opus"`; Fable reserved for the orchestrator; this block
    exists so ANY model can resume mid-batch):**
    - **A1 (D19 break-value removal): EXECUTED, awaiting output-review.** Brief
      `wave-a1-break-value-removal-brief.md` v3 (3-pass gauntlet clean). Executor commit
      `68e21f79` on its worktree branch (`worktree-agent-ab50b8851fdd6bd3d`); final patch
      `/tmp/wA1_exec_final.patch`; all 17 gates exact (incl. bootstrap fixed-point green,
      5 comparison suites byte-identical, 3 residue greps empty). NEXT: output-review
      (in flight) → parent cherry-picks onto gorget-1 → full sweep at batch close.
    - **A2-R (D12 drop-purity, Rust half): brief v4 in gauntlet, pass 4 in flight
      (Opus).** Brief `wave-a2-drop-purity-brief.md` v4 = durable prototype
      (`scouts/patches/scout_wA2_prototype.patch`) + 19 folds across 3 passes. Pass-3
      found the BLOCKING place-shape hole (field/index places dodged the check →
      MEASURED double-drop on `R c = hh.r`; `expr_types` is sparse) — now a mandated
      writer-side work item + ggdef field-place tests. Executor builds SIX items beyond
      the prototype (expr-body hunk · closure+ImplicitClosure shared helper ·
      place-shape fix · position-aware message mechanism · ICE rider · ggdef
      Option/closure/field tests). Pass-4 (Opus) is executing-or-refuting the
      place-shape spec + assessing whether to SPLIT the track. NEXT: pass-4 verdict →
      fold-or-launch executor (`model:"opus"`), or split per its recommendation.
      Filed en route: Shared[R]-payload-drop bug (HIGH) · builtin-handle name-list
      debt (MEDIUM) · generics-T-blind dodge (MEDIUM).
    - **A3 (D10a &-bind rejection): brief v2 in gauntlet, pass 2 in flight.** Brief
      `wave-a3-amp-bind-rejection-brief.md` v2 (pass-1's BLOCKING fold applied: the
      expr-position class-hole — extend `expr_is_borrow_bind` to Match/Do/Block +2
      fixtures + re-sweep; zone completed; framing corrected). Prototype DURABLE at
      `scouts/patches/scout_wA3_prototype.patch` (all 3 scout prototypes staged there).
      Discoveries filed: comprehension-over-`&a` empty vector (HIGH) + the no-op-`&`
      value-position family (MEDIUM). NEXT: pass-2 verdict → fold → pass 3 → executor
      (`model:"opus"`).
    - **Batch-A discoveries FILED during the gauntlets** (all in TODO High): self-host
      parse-error surfacing (check/lowerer lanes drop ALL parse errors) · `return &v` of a
      `&`-param double-free · `Shared[R]` payload custom-drop never runs · builtin-handle
      name-list typed-marker debt · `Parser::error_at` prose-mangle (LOW) · decl-sigil
      swallow noted in C3's mandate.
    - **Integration order at batch close (parent):** cherry-pick A1 → A2-R → A3
      sequentially (same-file disjoint hunks in src/semantic — resolve textual adjacency);
      then full both-backend sweep + bootstrap + conformance + parity regen; then Batch B. Open non-fault follow-ups in TODO (each own gauntlet):
    rarer-bounds-sites real locations; self-host shift-parity guard; the block_exit_labels
    structural guard (≥2 instances — escalated); D23's T3c positive-throws smith tier. (The
    former fault-re-panic normalization + dead-catch-lint follow-ups are CANCELLED by D25 —
    that machinery is deleted in C2.) **Regenerate before quoting any number:**
    `cargo test -p ggdef` + `GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance --
    --test-threads=1 --nocapture` (expect 195/195/195/195) + `cargo test --lib` + `--test lints` +
    `self_host_bootstrap_fixed_point`.
- **Decisions D1–D8**: all recorded in `decisions.md` with rationale. Do not relitigate; do
  bring NEW decision needs to the owner as option-questions (owner directive: ask along the
  way, with recommendations and previews).
- **RFC**: APPROVED (status line at top of `rfc-ggc-ggdef.md`). §2.2 is the semantic core.
- **Phase 1 INFRA: ✅ COMPLETE 2026-07-06** — all six `phase1-infra-brief.md` tracks landed
  (P1-A throws desugar + §10.3 capture; P1-B frontmatter reader + ggdef lane; P1-C C/LLVM/
  self-host lanes; P1-D/D1 the 182-fixture migration, corpus 5→187; P1-E smith ggdef verdict
  lane; P1-G E_/W_ diagnostic registry). **Conformance state: 4 lanes over 187 fixtures,
  ALL FULL — ggdef · C · LLVM · self-host each 187/187, zero BUILD-FAIL, every floor pinned
  at MIN_FIXTURES=187 (the formerly held-open `String !p`+concat BUILD-FAIL was FIXED and
  landed 2026-07-06, which earned the floor bump). ggdef and both production backends are
  BYTE-IDENTICAL on all 182 migrated fixtures.** Regenerate: `cargo test -p ggdef` + `GG_BUILD_TIMEOUT_SECS=600 cargo test --test
  spec_conformance -- --test-threads=1 --nocapture`. Remaining RFC-§6 phase-1 scope (NOT in
  the infra brief): D4/D5/D6 rejections in BOTH production compilers (now framed as the D12
  mandate in `decision-batch-4-proposal.md`, awaiting owner ruling), the float chain (HELD
  behind its 3 filed prerequisites), and P1-D/D2 (production-v1 bulk — own scout, filed in
  TODO with the detonation census).
- **Decision batch 4: ✅ CLOSED — D10–D23 ALL RATIFIED** (D10–D21 on 2026-07-06; D22
  colon-slice and D23 throws-totality on 2026-07-06/07; each ruling + rationale in
  `decisions.md` LOG + the annotated `decision-batch-4-proposal.md`; implementation
  tracks filed in TODO.md, each requiring its own scout→brief→gauntlet).
  The big ones: D10 exclusivity (one place-overlap rule; local `&`-binds REMOVED),
  D11 trap normalization (ONE TrapKind registry, T_ codes, exit 101, `Fault` = the
  catchable subset), D12 D4-production-enforcement (straight to error), D13 allocators
  two-step, D14 get_or views, D15 slices-are-values + `int[]` removal, D17 read_file
  throws + the stdlib fallibility principle, D21 sim retired, D22 colon-slice `v[a:b]`
  canonical + `.slice()` removed (combined with D15 as ONE slice-surface track), D23
  throws TOTALITY INVARIANT + diagnostic contract + the A33 rider (faults enter the
  error/value world only via explicit conversion points). Design queue after this
  batch: A31 inferred error sets, A32 HOF effect-polymorphism, A33 boundary hook spec.
  ENFORCEMENT-WAVE ORDER (the unwrap + strmove production fixes both MERGED 2026-07-06):
  trap-normalization (unblocks exact-code conformance + D2 panics) → D12 (scout measures
  blast radius first) → D10 tracks (bind-removal is bootstrap-gated) → D13/D14/D17 →
  the small riders (D15+D22 slice-surface, D18, D19, D20, D21 salvage-scan deletion).
- **Phase 0: ✅ COMPLETE 2026-07-06** (A 26/26 · B1 75/75 · B2 87/87 full corpus · C
  adjudications `9`/`ablog`/`hello` + spectests skeleton + gen + prose stubs; RFC §6(a)(b)(c)
  satisfied per independent output-review re-runs). `phase0-brief.md` is CLOSED/sealed.
  Definition scorecard so far: 4 production bugs surfaced+filed, 3 ratified expectations
  production can't yet meet, 1 production memory-safety fix landed (elemdrop), EMove settled. **ggdef has surfaced 4 production bugs** (3 bare-param
  materialize holes + ctor named-args — TODO HIGH entries, ggdef-adjudicated expected
  outputs in spec/ggdef/reports/increment_b1.md's CORRECTION table). Call-side named args
  are REJECTED in ggdef pending B2's reorder. **Next: PHASE 1** — scope per RFC §6: coverage completion (§2.6 rows-1-2
  remainder + statics), frontmatter migration of the ~1,218 harness pairs (float fixtures
  unblocked by D8), per-impl conformance reports + monotone floors, ggdef as smith's VERDICT
  lane, D4/D5/D6 rejections in BOTH production compilers + negative fixtures, ggdef
  transitive-drop completion (filed HIGH — spectest prerequisite), diagnostic-code registry.
  START with a phase-1 scout+brief (new file, e.g. phase1-brief.md) through the full
  gauntlet; the elemdrop production fix already LANDED (P1+P2 closed).
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
(batch 4 CLOSED; the live queue is A31 inferred error sets, A32 HOF effect-polymorphism,
A33 supervised-boundary hook spec).

## Commands (regenerate, never trust cached numbers)

```bash
cargo build && cargo test --lib && cargo test --test lints        # base gates
cargo test --test integration -- --test-threads=4                  # full sweep (parent-only)
GG_SMITH_SEEDS=1..200 cargo test --test smith -- --nocapture       # fuzzer batch
cargo run -p ggdef -- run <file.gg>                                # once phase 0 lands
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture   # parity (separate track)
```
