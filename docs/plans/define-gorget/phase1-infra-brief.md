# EXECUTOR BRIEF: Define Gorget PHASE 1 — conformance infrastructure (P1-A..P1-G)

> **STATUS: v3 — passes 1 (10 res) + 2 (2 blocking + 4 minor) folded; 0 clean of >=3. Pass 3 (Opus) = SIGN OFF FOR WAVE 1. WAVE 1 = {P1-A, P1-E, P1-G} — LAUNCHED 2026-07-06.**
> **P1-E: ✅ LANDED 2026-07-06** (merged to main; output-review SIGN OFF w/ lane-reversal causality proof; post-merge smith smoke 16 seeds = 14 MATCH / 1 pre-existing CRASH s103 / 1 SPEC-DIVERGE s110 / 0 GGDEF-SKIP).
> **P1-G: ✅ LANDED 2026-07-06** (merged; review SIGN OFF conditional on 3 follow-ups — ALL FILED in TODO: full-code-list pin test M, self-host header parity M, lex/parse codes L; "never churn" wording hardened in the registry at integration).
> **P1-A: ✅ LANDED 2026-07-06** (merge cf9c7399 = 5c5c6d99 + fold 2afb452b; pass-1 review raised BLOCKING §10.3 silent-wrong → folded (CaptureCtx typed-dest capture, 11 s103 pinning tests, floors 182/341/277) → pass-2 SIGN OFF w/ full fold-defect hunt. 2 production findings filed from the fold: nested-Result garbage HIGH + return/expr-body asymmetry decision; closure-throws ggdef hardening L filed from pass-2. Corpus count guards bumped 118/105 at integration for matcluster's +2 fixtures). **WAVE 1 COMPLETE. P1-D's throw-drop gate is CLEARED.**
> **P1-B: ✅ LANDED 2026-07-06** (pre-launch brief-review SIGN OFF w/ premise digest → executor cb74777e → output-review SIGN OFF w/ 2 LOW forward-compat seams FILED (block-shaped unknown keys; run-tier-only Expect) + doc-record corrected at integration. `parse_frontmatter` + `spec_conformance_ggdef` lane, floor=5, ggdef lib 89/0). **Next: {P1-C, P1-D} in parallel (disjoint zones: root tests/ vs spectests/+migration tool) — pre-launch brief-reviews first.**
> **P1-C: ✅ LANDED 2026-07-06** (executor 14e85b34 → output-review SIGN OFF, 4 behavior probes; merged; parent gates green on main — 3/3 lanes, floors C=4/LLVM=4/selfhost=5 live; 2 follow-ups filed: dormant exit≠0 branch = D1 merge hazard, CI wiring). Was: LAUNCHED 2026-07-06 (pre-launch review SIGN OFF w/ MEASURED per-lane floors C=4 / LLVM=4 / self-host=5 — the C/LLVM non-MATCH on smith_move_param_concat is the filed `String !p`+concat HIGH surfacing as intended, invariant #8; digest: duplicate-helpers mechanism per the smith precedent, lanes pin their own backends, exit-mapping rule 0→exact / non-zero→trap+prefix deferred-code, stdout-trim trap, OnceLock driver build).
> **P1-D/D1: ✅ LANDED 2026-07-06** (executor 01f3d39b+c2edb2a3 → output-review SIGN OFF w/ R4 lane measurement C 186/187 · LLVM 186/187 · selfhost 187/187, zero whitespace mismatches; merged; root-lane floors re-seeded 186/186/187, MIN_FIXTURES 187; D2 filed in TODO w/ corrected detonation census). Was: LAUNCHED after FOUR sequential passes (pass 1 RES(6) → FOLD D1/D2 split; pass 2 RES(R1 blocking: selection mechanism) → FOLD-2 classification hoist; pass 3 RES(R1: dedup-scope inconsistency) → FOLD-3 option A; pass 4 = CLEAN SIGN OFF w/ notes N1-N3 folded into the executor prompt). D2 = deferred, own scout (expect-source fork first). Executor zone: spec/ggdef + spectests/run.
> Scout artifacts: /tmp/recover_p1infra/ (findings, prototypes, probes). Prerequisite HIGH filed in
> TODO: ggdef throw-drop + native-recursion (P1-A must close them before P1-D's converter runs).
> RFC §4 amendment noted in ledger: conformance floors are INLINE dynamic floors per runner (the
> c_emit_comparison precedent), not tests/lints.rs static ratchets.

## ⚡ REVIEW PASS-1 FOLDS (2026-07-06) — these OVERRIDE the corresponding draft text below

- **R1 (P1-A gate)**: the scout probes are /tmp-only. P1-A PROMOTES them to committed tests:
  `spec/ggdef/tests/coverage_histogram.rs` + `converter_agreement.rs` (from
  /tmp/recover_p1infra/scout_probe.rs) — the coverage/agreement gates cite THOSE, not /tmp.
- **R2 (P1-E taxonomy)**: full ggdef-outcome map — Value-agree→MATCH; Value-disagree→
  SPEC-DIVERGE; **IllFormed where gg-check ACCEPTED→SPEC-DIVERGE** (the flagship class);
  Trap→compare exits (agree→MATCH else SPEC-DIVERGE); FuelExhausted→GGDEF-SKIP;
  ElabError/ParseError→GGDEF-SKIP.
- **R3 (P1-E eval/compare split)**: EVALUATE ggdef after classify() step 1 (cheap; the
  IllFormed-vs-check-accepts verdict can fire there); the Value-vs-C-stdout COMPARISON lands
  after step 3 (needs `c_out`).
- **R4 (SAFETY — gates P1-E)**: ggdef runs IN-PROCESS in smith; a deep-recursion seed SIGABRTs
  the whole run (uncatchable). The ggdef native-recursion fix (TODO, filed w/ throw-drop) is a
  P1-E prerequisite TOO (not just P1-D) — or P1-E must subprocess-isolate/depth-bound eval.
- **R5**: the 3 float prerequisites are FILED in TODO as of this fold (D8 formatting appendix;
  ggdef format_value D8-compliance incl. 3.0→"3.0" decision; production print+float_to_str fix
  both backends). Float migration stays HELD behind all three.
- **R6**: P1-G threading site = `src/errors.rs:275` (`report_semantic_error`), not :205.
- **R7**: the ggdef lane (spec/ggdef/tests/) gets its OWN inline floor + escape env (cannot call
  root `parity_floor_active`; doesn't need its carve-outs). P1-C note: under GG_BACKEND=llvm the
  floor is diagnostic-only (parity_floor_active→false) — matches the c_emit precedent, expected.
- **R8**: the ggdef root dev-dep is NOT committed (apply /tmp/recover_p1infra/
  scout_cargo_devdep.patch in P1-E); P1-C's root-crate `parse_frontmatter` use depends on it →
  explicit ordering: the shared reader lands with P1-B (spec/ggdef), P1-C consumes it via the
  dev-dep, so **P1-E's Cargo change (or an equivalent dep commit) precedes P1-C**.
- **R9 (zones)**: `src/semantic/errors.rs` is SHARED with the concurrent materialize-cluster
  track (DeadBareParamWrite) — P1-G adds a code() method only, coordinate at merge; P1-C lives
  in a NEW `tests/spec_conformance.rs` (never appended to integration.rs).
- **R10**: `parse_frontmatter` tolerates the `doc: |` multiline block scalar + unknown keys
  (present in committed seeds).

### Pass-2 folds (override precedence over BOTH the draft and pass-1 where they conflict)
- **R-a (P1-E wave-1 status)**: P1-E runs ggdef IN-PROCESS for wave 1 — tier-0 is provably
  shallow (fixed helper set; 200 seeds, 0 FuelExhausted at fuel=2,000,000 — USE THAT CONSTANT).
  The native-recursion fix (TODO) is a HARD ordering prerequisite before the smith generator
  widens past tier-0 — NOT a P1-E deliverable; the "depth-bound eval" option is DROPPED for
  P1-E (out of its zone; collides with P1-A).
- **R-b (P1-A scope)**: items 8 (transitive-drop) + 9 (native-recursion) are STRIPPED from
  P1-A's work list — separate tracks (TODO HIGHs), coordinate, do not do there. P1-A item 1
  (the silent-wrong→loud-ElabError safety audit, throw-drop foremost) is the MANDATORY
  deliverable that GATES P1-D; items 2-7 are "climb the coverage histogram as far as fits"
  (gate = monotone improvement via the promoted coverage_histogram test, not 100%).
- **R-c**: P1-G step 2's ":205" in the draft body is STALE — the semantic-error threading site
  is src/errors.rs:275 (report_semantic_error).
- **R-d**: the ggdef root dev-dep lands as a STANDALONE prerequisite commit (1-line, additive)
  before BOTH P1-E and P1-C — decouples P1-C from P1-E completion.
- **R-e**: warning-code rendering (report_semantic_warning:281; 13 check_gg_warns) is DEFERRED
  — P1-G threads the error path only; SemanticWarningKind::code() may exist but is not rendered
  in phase 1 (state in the registry prose).
- **R-f**: pass-1 R2 EXTENDS the draft's P1-E step-1 outcome list (additive); "Value-agree" is
  not a smith verdict — it means "no SPEC-DIVERGE, continue the lanes".

### ⚡ P1-D PRE-LAUNCH FOLD (2026-07-06, from the RESERVATIONS review pass) — OVERRIDES §P1-D below

The §P1-D draft's "~1218 pairs in one run" scope is a BRIEF DEFECT. P1-D SPLITS:

**D1 — the launchable increment (zone: spec/ggdef/ + spectests/run/; nothing in tests/):**
1. Migrate ONLY the ggdef-adjudicated AGREE set (regenerate the list:
   `cargo test -p ggdef --test converter_agreement -- --nocapture`; 182 at fold time).
   EXCLUDE: `cow_*`/`deadwrite_*` (the 122-fixture phase-0 corpus), all float fixtures
   (predicate `has_decimal_number` on EITHER side, converter_agreement.rs:189-197 — float
   HOLD still binding, D8 production fix NOT landed, only the D9 decision is), and the
   OTHER-mismatch triage names (12 at fold time, printed by the same run — real ggdef gaps
   routed to the P1-A coverage list, e.g. core_traits = user-Display, NEVER migrated blind).
2. Tool = `pub fn migrate(...)` in spec/ggdef/src/lib.rs (must reach the private
   `render_expect_block`/`json_escape`/`splice_expect`), dispatched from the existing
   `main.rs` arg match; call `gen_frontmatter` IN-PROCESS per fixture (never shell out N×);
   `expect:` stays LAST in the fence (lib.rs:141 convention); flat layout in spectests/run/
   (both lanes glob flat — do NOT change globs); add a `render_expect_block_from(exit,
   stdout)` seam now (D2 will need it; keeps D2's serialization on the SAME json_escape the
   reader inverts).
3. **`gen_idempotent` MUST become adjudicator-aware IN D1** — today gen_idempotent.rs:40-42
   panics on any fixture ggdef can't gen (production-v1 landing in D2 would detonate the
   gate); assert idempotence only for `adjudicator: ggdef`, count-and-report the rest.
4. Bump `GGDEF_MATCH_FLOOR` (spec_conformance_ggdef.rs:40) 5 → the REGENERATED in-worktree
   count (expected ≈ 5 seeds + AGREE set; never quote a cached number). No seed collisions
   (verified: the 5 seeds don't exist under tests/fixtures/).
5. Transition stays ADDITIVE: integration.rs literal pairs untouched; retirement = separate
   later track.

**D2 — DEFERRED, needs its own scout→brief→gauntlet (do NOT fold into D1):**
- The production-v1 bulk (~900 single-file run_gg where ggdef ElabErrors) — blocked on the
  expect-source fork: grammar-complete literal extraction (+ per-fixture round-trip
  validation; current parse_rust_str_lit drops raw strings) VS production-run capture
  (crosses into tests/ zone — coordinate with P1-C). Resolve by scout, not hand-wave.
- args/stdin families (reader ignores unknown inline keys but run_source can't consume
  them), dir/multi-file (`files:` is block-shaped — reader errors, filed LIMIT seam),
  `gg test`/bench families (fragments, not run-tier).
- ALL panics families (25 call sites): `Expect` has no stderr field; migrating would DROP
  the panic-message assertion (a "don't redesign around a gap" violation). Wait for RFC §4
  trap-normalization + a stderr/code field (tier-discriminated Expect, filed TODO).
- Corrected corpus census (call sites, fn-def lines excluded): run_gg=1216,
  with_args=1, with_stdin=1, dir=20, bench=1, panics=19, panics_with_stdout=5,
  panics_with_flags=1, test=14, test_with_flags=4, test_with_tags=2.

**⚡ D1 FOLD-2 (2026-07-06, from the re-review pass — R1 was BLOCKING; these override D1 items above where they conflict):**
1. **R1 (the selection mechanism):** the converter_agreement run prints only the AGREE *count*,
   never the 182 names, and the classification predicate (`parse_rust_str_lit`,
   `has_decimal_number`, the run_gg-needle extractor, the EXCLUDE list, fuel=3_000_000,
   `.trim()` compare) lives ONLY in tests/ binaries (already copy-pasted 4×) — unreachable from
   a lib.rs `migrate`. D1 therefore HOISTS the classification into `spec/ggdef/src/lib.rs` (or a
   new lib module) as pub helpers + a `classify_fixture(...) -> Agree|Float|Other|NotValue|NoPair`,
   and BOTH converter_agreement.rs AND `migrate` call them. **Scope of the dedup (FOLD-3, pass-3
   R1 option A): delete ONLY converter_agreement.rs's private copies and route it + `migrate`
   through the hoisted helpers; corpus_a/b/b1's three private copies are OUT of D1's critical
   path (corpus_a's is a DIVERGENT implementation shape — rewiring it needs an equivalence
   proof) — their dedup is a FILED follow-up, do NOT rewire them this round.** Migrate selects
   exactly the `Agree` fixtures via the SAME predicate; the test's AGREE count becomes the
   cross-check assertion, not the list source. **The hoist is a PURE CODE-MOVE — no
   classification-logic change; after it, `cargo test -p ggdef --test converter_agreement --
   --nocapture` MUST still print AGREE=182 (floor unchanged).**
2. **R5 (set arithmetic):** the `Agree` bucket IS the exact migration set — the D1 EXCLUDE list
   above is DESCRIPTIVE of what Agree already omits (cow_*/deadwrite_* are outside the walk
   universe; floats/OTHER are disjoint buckets). Do NOT apply it as a second filter.
3. **R3 (gate + zone-lock):** D1's gate = `cargo test -p ggdef` (all ggdef suites incl. the
   frontmatter unit tests over the grown corpus) **PLUS `cargo test --test lints
   ggdef_import_ratchet` (FOLD-3 pass-3 R4: D1 adds code to the ratchet's scanned dir — cheap,
   run it before declaring done)** — explicitly NOT the root spec_conformance_c/
   _llvm/_selfhost lanes (P1-C's zone). `git add` named spec/ggdef + spectests/run files only;
   TODO.md/DONE.md are parent-only.
4. **R4 (orchestrator note, not an executor task):** P1-C lanes READ spectests/run/; whoever
   merges second re-seeds the other track's floors from the merged corpus, and the parent runs
   the root lanes over the merged set. Expected clean EXCEPT trailing-whitespace-only AGREE
   fixtures (converter_agreement compares `.trim()`ed; the lanes compare exact) — any such
   MISMATCH is a reference-grade FINDING to triage (invariant #8), never a floor to lower.
5. **R6:** `render_expect_block_from(exit, stdout)` ships WITH a lib unit test (round-trip vs
   json_escape/parse_json_string) so the D2 seam isn't untested dead code. **(FOLD-3 pass-3 R3:
   `parse_json_string` is frontmatter.rs-private — make it `pub(crate)` or site the round-trip
   test inside frontmatter.rs's own `#[cfg(test)]` mod so escape + inverse are co-reachable.)** D2-scout breadcrumb:
   enumerate ALL adjudicator-assuming gates before landing production-v1 seeds —
   `gen_idempotent.rs:40-42` AND `frontmatter.rs:401` (`all_committed_seeds_parse`) both assume
   `adjudicator: ggdef` today.


# DRAFT executor brief — Define Gorget PHASE 1, conformance-infrastructure track

> Status: SCOUT DRAFT for the orchestrator's ≥3-pass fresh-review gauntlet. Numbers are
> regenerate-before-quote (commands inline). Premises verified against source this session
> (2026-07-06) with file:line. This is partitioned into launchable increments; each is a
> separate worktree agent with a disjoint zone.

## Prime directives for every executor in this track
- Worktree preamble (CLAUDE.md "Multi-agent orchestration" rule 2). `git add <named files>` only.
- Expectations flow FROM the definition (RFC §4). NEVER copy a backend's output as truth.
- A ggdef-vs-production divergence is a FINDING to triage (invariant #8), never silently accepted.
- `adjudicator: ggdef` is assigned ONLY when ggdef stdout AGREES with production (modulo D8 floats).
  Elaborates-but-disagrees ⇒ triage: production bug (file) OR ggdef gap (coverage). NEVER blind-migrate.

## Dependency graph (waves; disjoint zones can run in parallel)
- WAVE 1 (parallel): **P1-A** coverage (zone: spec/ggdef/) · **P1-E** smith lane (zone: tests/smith + Cargo.toml) · **P1-G** diagnostic registry (zone: src/semantic + src/errors.rs + spec/prose).
- WAVE 2: **P1-B** frontmatter-reader + ggdef conformance lane (zone: spec/ggdef/) — sequence after/with P1-A (same crate). Then **P1-C** C/LLVM/self-host lanes (zone: tests/).
- WAVE 3: **P1-D** converter (zone: spectests/ + migration tool) — after P1-A matured + P1-B/C lanes exist.
- Cross-track: D4/D5/D6 production+elaboration rejections and the ggdef transitive-drop fix are SEPARATE tracks (HANDOVER) but P1-A/P1-D depend on the transitive-drop fix for drop-count fixtures.

---

## P1-A — ggdef coverage completion (zone: spec/ggdef/ ONLY)
**Why.** MEASURED (scout_probe.rs, `cargo test -p ggdef --test scout_probe scout_coverage_histogram`):
of a ~40% sample of the 1363 non-cow/deadwrite fixtures, only **25.5% elaborate+run** today; 74%
ElabError. The converter's ggdef-adjudicated set is capped by this.
**Work, in priority order (top ElabError buckets):**
1. **SAFETY FIRST — make every unimplemented construct a LOUD ElabError, never silent-wrong.**
   Bonus finding 2: throws-based fixtures (snag43, snag49a-d, void_throws) currently elaborate to a
   Value with EMPTY stdout (the throw effect is silently dropped) while OTHER throw forms correctly
   ElabError (Q2 bucket `statement throw`=16). A silent-wrong Value would auto-migrate to
   adjudicator:ggdef and lock in garbage. Audit eval/elaborate for every "partially handled →
   wrong Value" path; convert to ElabError. GATE this before P1-D runs.
2. **throws→Result desugar** (RFC §2.6 row 1 says IN). Biggest correctness gap.
3. **statics** (RFC §2.6 row 3; Q2 bucket `item kind static`=16): const locals + module static globals
   (init order, mutation, program-exit drops) — needs GGC store rules.
4. **traits/generics/trait-objects** (Q2 `item kind trait`=13; `generic_equip_method` disagrees 0 vs 3).
5. **method long tail** (.iter/.map/.contains/.is_empty/.is_some/.ok/.to_upper — the stdlib-free shims).
6. **fstring format specs** (`:b` binary etc. — `fstring_binary_spec_leak` ggdef `bin=10` vs `1010`).
7. **the `expression unsupported` catch-all** (Q2=73) — drill down; likely several distinct constructs.
8. **ggdef transitive custom-drop** (already filed HIGH) — drop-count spectests can't gate until fixed.
9. **ggdef native-recursion guard** (bonus finding 1): `stack_guard_deep_recursion.gg` → SIGABRT, not
   FuelExhausted (fuel bounds steps, not eval/elaborate recursion DEPTH). RFC §2.2 claims "never
   exhausts a real stack" — currently FALSE. Either add a depth guard → Trap, OR (cheaper) soften the
   RFC text and keep this fixture in the RFC-C11 implementation-defined exclusion set. Owner call.
**Do NOT** pull in out-of-spec-v1 surface (tensor/p2p/allocators/FFI/concurrency/meta — §2.6). Those
stay ElabError (loud) → production-v1 adjudicated or excluded.
**Gate.** `cargo test -p ggdef`; re-run scout_coverage_histogram (climbs) + scout_converter_agreement
(OTHER-mismatch → only confirmed-production-bug entries remain). `cargo test --test lints ggdef_import_ratchet`.

## P1-B — frontmatter reader + ggdef conformance lane + floor (zone: spec/ggdef/)
**Why.** No frontmatter READER exists (gen WRITES via splice, lib.rs:141; nothing parses back).
All 4 lanes need identical parsing → ONE shared reader.
**Work.**
1. `pub fn parse_frontmatter(&str) -> Result<Front, _>` in ggdef lib: mode, adjudicator, expect{exit,stdout},
   and the phase-1 keys args/stdin/files/nondet-seeds/since/features. (Prototype: scout_runner.rs parse_front.)
2. `spec_conformance_ggdef` test over spectests/run/: run_source in-process, compare exit+stdout,
   print always-pass MATCH table, GGDEF-SKIP on ElabError/Parse (out of surface), INLINE monotone floor.
   (Prototype: scout_runner.rs — 5/5 MATCH in 1.8ms.)
**ARCHITECTURE DECISION for the orchestrator/owner:** RFC §4 + README say floors live "in tests/lints.rs".
But lints.rs floors are STATIC (grep arm-counts from source); a conformance MATCH-count is DYNAMIC
(requires running). It CANNOT be a lints.rs static ratchet. It must be an INLINE floor in each runner
(exactly the c_emit_comparison precedent, integration.rs:16547, gated by parity_floor_active). Recommend
reading "floors in tests/lints.rs" as "house-ratchet pattern, floor inline per runner". Confirm with owner.
**Gate.** `cargo test -p ggdef spec_conformance_ggdef`.

## P1-C — C / LLVM / self-host conformance lanes + floors (zone: tests/)
**Why.** RFC §4: each impl runs the suite via a thin adapter, prints `spec_conformance_<impl>` always-pass
+ monotone floor. These need `gg build` subprocess + cc + the self-host driver — root-crate territory.
**Work.** `spec_conformance_c`, `_llvm`, `_selfhost` (new tests/spec_conformance.rs OR appended to
integration.rs): for each spectests/run fixture, build+run via the EXISTING harness (`gg_command`,
`build_with_timeout`, `self_host_emit_cc_run` at integration.rs:~19388), compare to the SHARED
parse_frontmatter expect:, print table + inline floor. Respect `parity_floor_active` carve-outs
(linux-only, C-backend-only, GG_PARITY_FLOOR_OFF). Skip production-v1/prose-adjudicated fixtures whose
mode isn't `run` for the run lanes.
**Non-goal.** The static-error/parse-error/annexe tiers' runners (fill as those tiers populate).
**Gate.** `cargo test --test <target> spec_conformance_c`; LLVM under `GG_BACKEND=llvm`. Seed floors
from a regenerated run in-worktree (never a cached number).

## P1-D — the converter (zone: spectests/ + a migration tool)
**[SUPERSEDED by the ⚡ P1-D FOLD + FOLD-2 above: census corrected there; D1 = AGREE-set only.]**
**Why.** Migrate the literal harness pairs (census: see the fold) → spectests frontmatter fixtures,
expectations regenerated via `ggdef -- gen`.
**Work.**
1. A migration tool: `pub fn migrate` in spec/ggdef/src/lib.rs dispatched via `ggdef -- migrate`
   [FOLD-2 R1 — the "OR a tests/-adjacent script" option is STRUCK: out of D1's zone, collides with
   P1-C]; per fixture: copies the .gg into spectests/run/, prepends frontmatter, assigns
   `adjudicator:` per the rule below, and runs gen IN-PROCESS on the ggdef set.
2. **adjudicator assignment (MEASURED split, scout_converter_agreement):**
   - **ggdef** — ggdef elaborates to Value AND stdout AGREES with the committed run_gg string (modulo
     float rendering). (Agree=174 of the sampled in-surface set.)
   - **production-v1** — ggdef ElabErrors (out of surface) OR is out-of-spec-v1: expect: filled from the
     EXTRACTED run_gg string. Retired to ggdef as P1-A coverage lands. (This is the majority — the ~75%.)
     **[D2 ONLY — D1 migrates ONLY adjudicator:ggdef AGREE fixtures, per the fold.]**
   - **HOLD (do NOT migrate this round)** — float-output fixtures (~60-79; MEASURED
     scout_float_output_prevalence: 63 fixed-6 + noise). BLOCKED on the D8 production fix landing in
     BOTH backends. See "Float sequencing" below.
   - **triage, don't migrate** — ggdef elaborates but DISAGREES (non-float): production bug (file per
     invariant #8) OR ggdef gap (P1-A). (OTHER-mismatch=19 in the sample; several are real gaps.)
3. **Human-review diff.** Every ggdef-gen expectation differing from the committed run_gg string is
   surfaced for review: float→HOLD, else a FINDING.
4. **Transition policy (decision point for the brief-review):** keep integration.rs's literal pairs
   IN PLACE (spectests ADDITIVE) during transition; retiring the ~1218 pairs is a later, separate,
   risky delete — do NOT couple it to the migration.
**Gate.** [D1: `cargo test -p ggdef` ONLY, per FOLD-2 R3 — the root spec_conformance_c/_llvm/_selfhost
lanes are P1-C's zone and run at the parent's merge.] D2-era: spec_conformance_* lanes green on the
migrated set; `cargo test -p ggdef gen_idempotent`.

### Float sequencing (Q1 recommendation — OPTION A: migrate non-float first, HOLD floats)
CONFIRMED: ggdef prints floats via Rust `format!("{f}")` (eval.rs:1576 — shortest round-trip, D8's
target); production prints `%f` fixed-6. They diverge on EVERY float. The conformance floor is a MONOTONE
MATCH-count: migrating floats now makes every float fixture an instant MISMATCH on C/LLVM/self-host —
60-79 permanent per-fixture carve-outs (an allow-list the house style rejects). HOLDING floats keeps
every migrated fixture MATCHing on ALL lanes → floor = full set, zero exceptions. Float fixtures stay in
the legacy literal harness until the D8 production fix lands, then migrate in ONE ggdef-gen batch. Float
migration has THREE prerequisites (all separate tracks): (1) D8 formatting-appendix authored — must pin
whether `3.0` prints "3.0" or "3" (ggdef's provisional `{f}` prints "3", NOT strictly D8-compliant);
(2) ggdef format_value updated to match the appendix; (3) production print + float_to_str fixed in BOTH
backends. Option A is operationally cleaner than "migrate all + known-mismatch allowances".

## P1-E — ggdef as smith's verdict lane (zone: tests/smith + Cargo.toml)
**Why.** RFC §4: ggdef joins smith as the verdict lane; tri-state triage. MEASURED (scout probe over 200
tier-0 seeds): **100% ggdef-adjudicable** (197 Value + 3 IllFormed, 0 out-of-surface), **0.316ms/seed**
(negligible). Wired `ggdef = { path = "spec/ggdef" }` into root [dev-dependencies] already (scout).
**Work.**
1. Insert a ggdef lane in classify() (main.rs:426) AFTER gg-check (step 1). ggdef Value becomes an ORACLE:
   compare to the C-run oracle. AGREE → continue. DISAGREE → new verdict `SPEC-DIVERGE` (needs tri-state
   sub-classification: impl-bug / spec-bug / spec-silent). ElabError/Parse → `GGDEF-SKIP` (fall back to
   today's C-oracle differential). Keep the C/self-host/LLVM cross-impl diff AS-IS — ggdef ADDS an oracle.
2. **GGDEF-SKIP is defensive-mandatory** even at 0% today: tier-1/2 generator widening (future) WILL
   produce out-of-surface programs; the skip must exist before then.
3. **Triage the 3 known findings** (seeds 110/142/195): ggdef flags "read of moved-out value"; `gg check`
   ACCEPTS them (exit 0). Seed 110: `fn1(w0, r1, !r1)` reads `r1` as a borrow arg AND moves it in the same
   call — the A29 same-call-aliasing hole (decisions.md OPEN A29). Per RFC §2.3 this is a both-compiler
   finding (production should reject). File; owner decides A29.
**Gate.** `cargo test --test smith generator_determinism`; `GG_SMITH_SEEDS=1..200 cargo test --test smith
-- --nocapture` shows the new lane; the 3 findings surface as SPEC-DIVERGE.

## P1-G — diagnostic-code (E_) registry (zone: src/semantic/errors.rs + src/errors.rs + spec/prose)
**Why.** 110 diagnostic kinds today (93 SemanticErrorKind + 17 SemanticWarningKind, errors.rs:198/:18),
NO stable codes; negative fixtures assert on 45 distinct message SUBSTRINGS across 160 check_gg_fails.
**Work (no ocean-boiling).**
1. `pub fn code(&self) -> &'static str` on SemanticErrorKind/WarningKind — a match mirroring the Display
   impl's arm structure (errors.rs:590, 93 arms, NO catch-all `_`) ⇒ compiler-enforced EXHAUSTIVE: a new
   variant without a code is a build error = built-in ratchet (invariant #6). Symbolic names
   (`E_MOVE_WITHOUT_OPERATOR`, `E_USE_AFTER_MOVE`), NOT sequential numbers (RFC §8.3 numbering bikeshed —
   sidestep with names; numbers churn/collide across branches).
2. Thread into rendered output: `diag.with_code(err.kind.code())` at src/errors.rs:205 (codespan-reporting
   Diagnostic has native `.with_code`).
3. Registry `spec/prose/diagnostics.md`: code → prose section → fixtures.
4. **Incremental adoption:** start with the D4/D5/D6 rejection codes + the ~45 substrings the negative
   fixtures already assert. static-error/parse-error fixtures reference codes via `expect: code: E_...`.
   A lint (LATER, own increment) requires every static-error fixture to name a registered code. Migrate
   check_gg_fails substring asserts → code asserts opportunistically, not big-bang.
**Gate.** `cargo test --lib` + `cargo test --test integration` (no message drift); registry present.

## Track-wide non-goals
- Float fixtures (D8 production fix pending) — HOLD (P1-D).
- Out-of-spec-v1 surface (tensor/p2p/allocators/FFI/concurrency/meta/slices) — stay production-v1 or
  excluded from the ggdef lane; kept in C/LLVM/self-host lanes.
- Retiring the ~1218 literal run_gg pairs from integration.rs — dual-run during transition; cutover is a
  later, separate, risky delete.
- v1.5 executable well-formedness checker (retires adjudicator: production-v1) — post-phase-3.
- static-error / parse-error / annexe tier runners beyond the ggdef+run lanes — fill as tiers populate.

## Scout prototypes (in /tmp, reusable by executors)
- /tmp/scout_probe.rs — coverage histogram + float prevalence + converter-agreement (spec/ggdef/tests/).
- /tmp/scout_runner.rs — ggdef conformance-lane prototype (spec/ggdef/tests/).
- /tmp/scout_smith_probes.rs — smith-generator ggdef surface + illformed-seed + dump-seed probes.
- /tmp/scout_cargo_devdep.patch — the ggdef dev-dep wiring.
- /tmp/scout_seed110.gg — the A29 same-call-aliasing repro.
