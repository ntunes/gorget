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
  - **🛑 OWNER DIRECTIVE (2026-07-12, BINDING): STOP AFTER A2-R1.** When A2-R1 is
    landed + integrated + the BATCH-A CLOSE VERIFICATION passes (full C+LLVM sweeps,
    4-lane spec_conformance, parity regen, bookkeeping, worktree sweep), this session
    ENDS THE WAVE'S WORK — do NOT start A2-R2's scout, A2-S, Batch B, or anything else.
    **The baton passes to a FRESH ORCHESTRATOR RUNNING OPUS** (Fable quota preserved);
    the owner syncs main and continues from there. The "NEXT ORCHESTRATOR START HERE"
    section below is that agent's entry point. All subagents stay on `model:"opus"`.
  - **⚡ LIVE BATCH-A STATE (update IN PLACE at every track state-change — owner directive
    2026-07-11: subagents run `model:"opus"`; Fable reserved for the orchestrator; this block
    exists so ANY model can resume mid-batch):**
    - **A1 (D19 break-value removal): ✅ LANDED `d59605fc` (2026-07-11)** — output-review
      SIGN OFF (diff content-identical to the reviewed prototype); post-integration
      quick gates green (build, lib 1105/0, break 5/0, lints 53/0). Full sweep at batch
      close covers it. Detail: DONE.md.
    - **A2 (D12 drop-purity): SPLIT per pass-5 (Opus) — A2-R1 EXECUTING (Opus), A2-R2 queued.**
      Gauntlet: 5 passes, 22 folds; pass-5 BUILT the two riskiest items and ruled the split.
      A2-R1 (brief `wave-a2-drop-purity-brief.md`, SPLIT header = scope): taint pass + six
      positions (incl. expr-body `check_stmt.rs:1747` + closure tails with capture-rooted
      skip) + `lvalue_value_type` place-shape reroute + full probe suite + ggdef parity
      (Option gap, closure-tail, field-place rejection tests) + docs. Executor #1 DIED silently (no resume
      possible — no tool rounds left); its M1-M5 work RECOVERED and staged DURABLY:
      `scouts/patches/wA2R1_partial_m1-m5.patch` (550-line semantic core) +
      `wA2R1_fixtures_m5.tgz` (19 probe fixtures — ⚠ gitignore-hidden until the
      allowlist stanzas land, which executor #1 never added). TAKE-2 EXECUTOR (Opus)
      COMPLETED: commit `a79fda44` (36 files +1010/−31; all probes verified; ggdef
      gains Ty::Option/Result payload taint + 6 tests → lib 110/0; docs done; NEW
      pre-existing bug DISCOVERED+to-file: closure-returned owned Drop temp not
      drop-registered — `R b = f()` build-panics, `use(f())` silently loses the
      drop). OUTPUT-REVIEW SIGNED OFF →
      **✅ LANDED on gorget-1 2026-07-12** (post-integration gates green: d12 19/0+1ign,
      ggdef 110/0, lints 53/0, lib 1105/0) — BATCH A: ALL THREE TRACKS LANDED. ⚠ SEPARATE A3-REGRESSION FOUND at
      integration-check: ggdef `corpus_a_all_match` RED on the tip — A3 flipped
      `cow_amp_bind_ref*` to expect `E_LocalBorrowBind` but ggdef never got the
      D10(a) rule (every A3 reviewer + the brief missed the DEFINITION lane); a
      dedicated ggdef-fix executor (Opus) IS RUNNING (mirror the landed rejection
      class in elaborate; corpus_a + spec_conformance ggdef-lane green as gates).
      LESSON for the playbook: any track that FLIPS fixture expectations must carry
      the FULL ggdef suite in its gate list. Dead worktree removed post-capture.
      Commit lands with the current generic message (staged, Core-#8 clean).
      A2-R2 (stub `wave-a2-r2-message-ice-brief.md`): position/shape-aware message (no-`!`
      capture gate; dead `move` suggestion dies) + the compound-assign ICE fix (CORRECTED
      anchors: `stmts/assigns.rs:1148`, `ir/builder.rs:258`, panic `mod.rs:1763`) — own
      scout→gauntlet AFTER A2-R1. Filed en route: Shared[R]-payload-drop (HIGH) ·
      builtin-handle name-list · generics-T-blind · it-lambda tail dodge (MEDIUM).
    - **A3 (D10a &-bind rejection): ✅ LANDED `414e652a` (2026-07-11)** — output-review
      SIGN OFF (zones verified disjoint from A2-R1, so the old integrate-after-A2-R1
      constraint was dropped); post-integration gates green (amp_bind 10/0, lints 53/0,
      lib 1105/0). Commit message's decl-sigil overclaim corrected at cherry-pick
      (only `&` rejects; `!`/`move` swallowing = C3's mandate). Detail: DONE.md.
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

## 🎯 NEXT ORCHESTRATOR START HERE (written 2026-07-12 for the Opus successor)

You are a fresh orchestrator (Opus) taking the enforcement wave mid-stride. The owner
syncs main from gorget-1; **work on gorget-1 only**. Read order: this file top-to-bottom
→ `decisions.md` (D1-D28 + the 2026-07-11 LOG entries) → TODO.md's "RATIFIED
ENFORCEMENT-WAVE PLAN" entry (the live execution plan) → CLAUDE.md's multi-agent +
review sections (the gauntlet you MUST run). Regenerate every number before quoting.

**State you inherit (verify, don't trust):** Batch A of the wave — A1 (`d59605fc`) and
A3 (`414e652a`) LANDED; A2-R1's status is in the LIVE BATCH-A STATE block above (it was
in executor-recovery when this section was written — if unfinished, YOUR first job is
completing it: the brief is `wave-a2-drop-purity-brief.md` [SPLIT header = scope], the
base prototype is `scouts/patches/scout_wA2_prototype.patch`, partial work may be
captured at `/tmp/recover_wA2R1_partial.patch` [550 lines, M1-M5: semantic core + 19
`d12_drop_purity/` probe fixtures] — apply, finish M6-M9 per the brief, gauntlet the
output). If A2-R1 landed and the batch-close verification passed, start at step 2.

**Your work queue, in order (each item = scout→brief→≥3 sequential fresh reviews→
worktree executor→fresh output-review→integrate; ALL subagents `model:"opus"`):**
1. Finish/verify A2-R1 + the BATCH-A CLOSE: full C+LLVM sweeps
   (`GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 cargo test --test integration
   -- --test-threads=4`, both backends), 4-lane `spec_conformance` (expect 195×3 +
   the new negative fixtures outside spectests), parity regen (the standard command in
   TODO's north-star block; expect denominator movement from the batch's new
   fixtures — floors are min-counts, safe), DONE/TODO reconciliation, worktree sweep.
2. **A2-R2** (`wave-a2-r2-message-ice-brief.md` — a v0 STUB): scout first (the ICE
   rider's lowering write-site re-verify), complete the brief, gauntlet, execute.
3. **A2-S** (self-host drop-purity port — TODO High entry): own scout→gauntlet;
   bootstrap-gated; ~250-400 .gg lines; ggdef's 9-test suite is the model.
4. **Batch B** (D10(b) place-overlap + in-repo hand-hoists — the wave-plan entry has
   the site list): the scout MUST first evaluate building the TYPED BORROW-PROVENANCE
   bit (TODO: grep "typed is-borrow signal") — A3's gauntlet identified it as the root
   cause of the syntactic-walk fragility D10(b) would otherwise inherit.
5. **Batch C** per the wave plan: C1 operators (D26+D28; wire every new token into
   self-host `map_binop` + the anti-OP_ADD ratchet) → C2 fault-catch removal (D25;
   ~2,000-line both-compiler deletion; ships D24 spec prose + §10.5/§10.9 rewrite) →
   C3 the composed fmt sweep (D27 sigils ~1,114 + D22 slices 208 + D28 pows 7; ONE
   fmt pass per in-repo corpus). Then the deferred out-of-repo coordination round.

**Operational playbook (hard-won this wave):** every agent worktree-isolated with the
CLAUDE.md preamble; briefs + prototypes live DURABLY in `scouts/patches/` (never
/tmp-only); >600s gates are CHUNKED-FOREGROUND by test name (bootstrap per-stage
~150-170s — five agents stalled by backgrounding; the SendMessage nudge-resume cures
it; if an executor dies silently, capture-first then parent-takeover per the R40 T-C
precedent); update THIS file's live block at every track state-change; TODO holds
pending-only work; discoveries get filed with measured repros before moving on.

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
