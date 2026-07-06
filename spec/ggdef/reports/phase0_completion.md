# Phase-0 completion report — the `ggdef` walking skeleton (A + B1 + B2 + C)

**Scope.** Phase 0 (RFC §6) is DONE: the elaborator + evaluator for the honest
subset the target fixtures use, the conformance evidence over the full cow_* /
deadwrite_* corpus, and — this increment (C) — the three adjudications from the
definition, the `spectests/` skeleton, the `spec/prose/` stubs, and the
`ggdef -- gen` expectation generator. This report is the phase-0 gate summary +
the divergence ledger + the filed-findings list + the phase-1 punch list.

**Numbers are regenerated, never quoted.** Every count below was regenerated on
2026-07-06 by the command beside it; the `*_comparison`/corpus gates are
diagnostic-always-pass, so only the freshly-printed counts mean anything. Re-run
before relying on any figure.

## Regenerate every gate

```
cargo build --workspace                                  # clean
cargo test -p ggdef                                      # lib + corpus_a/b1/b + gen_idempotent
cargo test -p ggdef --test corpus_a  -- --nocapture      # A table
cargo test -p ggdef --test corpus_b1 -- --nocapture      # B1 table
cargo test -p ggdef --test corpus_b  -- --nocapture      # B2 table (full corpus)
cargo test -p ggdef --test gen_idempotent                # gen idempotence gate
cargo test --test lints ggdef_import_ratchet             # the import fence
cargo test --test lints                                  # all lints
cargo test --lib                                         # root package — unaffected
# the three adjudications (see adjudications.md for the trace justifications):
cargo run -p ggdef -- run tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg   # 9
cargo run -p ggdef -- run tests/fixtures/known_gaps/move_param_concat.gg            # ablog
cargo run -p ggdef -- run spectests/run/emove_lazy_bind_witness.gg                  # hello
```

## Gate summary (regenerated 2026-07-06)

| Increment | Gate test | Set | MATCH-gated | REPORT-ONLY | Result |
|---|---|---|---|---|---|
| **A** | `corpus_a` | 26 hardcoded A-clean cow_* | **26 / 26** | — | green |
| **B1** | `corpus_b1` | 103 non-equip cow_*/deadwrite_* | **75 / 75** | 28 | green |
| **B2** | `corpus_b` | 116 full corpus (−4 exclusions) | **87 / 87** | 29 | green |
| **C** | `gen_idempotent` | 5 `run/` seeds | idempotent (gen==committed) | — | green |

- **Unit tests:** `cargo test -p ggdef --lib` = **64** (regenerated), of which the
  4 `gen_*` tests are new this increment (insert-when-missing, idempotent-replace,
  no-fence-error, multiline-escape); the rest cover the §2.2 bullets + the four
  outcomes + the equip/D4 surface.
- **Import fence:** `ggdef_import_ratchet` green (use-line scan + the B2 F1
  full-source scan for inline `gorget::(ir|semantic|lir|bir|backend)::` paths).
- **Full lints:** `cargo test --test lints` green.
- **Adjudications:** all three correct — `9` / `ablog` / `hello` (Value outcomes;
  no custom-`Drop`, so the transitive-drop gap does not touch them). Details +
  trace justifications in [`adjudications.md`](adjudications.md).

**Standing exclusions (RFC §6, hardcoded — frontmatter `features:` is phase 1):**
the 3 generic-equip cow fixtures (`cow_element_borrow_alias_mutate`,
`cow_p3_alias_chain_mutate`, `cow_p3_index_mutate` — generic-equip-on-builtin is
optional in phase 0) and `deadwrite_ok_atomic_add` (std.sync atomics are phase 3).

## Divergence categories (the ledger the divergence table produced)

Every corpus fixture falls into exactly one bucket. `Findings = 0`,
`STOP-and-report = 0` — no un-triaged divergence.

- **expected-D2 (plain-`self` write-through):** none in the corpus (the equip
  methods are `&self` or plain-`self`-without-a-self-write). The D2 rule (a write
  through plain `self` MATERIALIZES) is exercised by the unit test
  `equip_plain_self_write_materializes_d2`.
- **expected-D1 / EMove:** none as a divergence — `cow_lazy_move_bind` /
  `cow_lazy_move_reassign` MATCH the committed pre-mutation value; production's
  `Expr::Move` read-through bug was already fixed and both sides agree.
- **the two smith bugs (acceptance (c)):** `cow_dead_branch_alias_bind` → `9`,
  `move_param_concat` → `ablog`. `ggdef` produces the correct value; production
  still diverges (filed below). See [`adjudications.md`](adjudications.md).
- **the 3 PRE-ADJUDICATED deadwrite deltas** (RFC §2.2 bare-param materialize;
  production bugs filed; all REPORT-ONLY / stderr-wired so they do not fail the
  MATCH gate — do NOT re-derive, do NOT patch `eval.rs`):

  | Fixture | ggdef (correct) | production | production defect |
  |---|---|---|---|
  | `deadwrite_warn_compound` | `10` | `11` | bare-param `xs[0] += 1` WRITES THROUGH (compound-assign bypasses materialize) |
  | `deadwrite_ok_loop_read_before_write` | `1,2,3,1` | `1,1,1,1` | materialize does not persist across loop iterations |
  | `deadwrite_ok_rebind` | `3,1` | CC-FAIL | bare-param full-rebind emits invalid C (latent; stderr-only fixture) |

  Full table + governing rule in
  [`deadwrite_spec_expectations.md`](deadwrite_spec_expectations.md).

## Filed findings — the definition's catches (invariant #8)

Phase 0 surfaced defects in **both** directions (the reference is not sacrosanct).
All are filed in the root `TODO.md`; regenerate their status from there, not from
this snapshot.

### 4 PRODUCTION bugs (Rust gg wrong; `ggdef` correct per RFC §2.2)

1. **compound-assign bypasses materialize** — `xs[0] += 1` through a bare param
   writes through to the caller (`deadwrite_warn_compound` → 11 vs ggdef 10);
   sibling-site drift, the compound-assign lowering missed the cow_before_mutation
   hook (also falsifies that program's `DeadBareParamWrite` warning text).
2. **materialize does not PERSIST across loop iterations** —
   `deadwrite_ok_loop_read_before_write` → 1,1,1,1 vs model 1,2,3,1 (the fixture's
   own comment says the private copy persists).
3. **bare-param full-rebind emits invalid C** — `xs = [9,9]` →
   `incompatible types … GorgetArray from void*` (latent; `deadwrite_ok_rebind`
   is gg-checked only; ggdef: 3,1). Fix (1)-(3) as ONE class at the materialize
   writer-site (Core invariant #4).
4. **struct/enum-ctor named args bind POSITIONALLY** — `Point(y=200, x=100)` →
   production x=200,y=100; correct (and ggdef) x=100,y=200. The free-fn/method
   named-reorder landed earlier but the ctor paths were left positional and never
   filed until B1's output-review probed it.

### `ggdef` phase-1 items (one track; disclosed in the B2 CORRECTION)

- **transitive custom-drop execution** (HIGH): `run_custom_drop` runs the type's
  OWN drop body only — it does not enumerate droppable FIELDS or COLLECTION
  ELEMENTS of the dropped value. RFC §2.2 makes drop count/order normative, so
  drop-count spectests cannot gate implementations until the definition itself is
  transitively correct. Not exercised by the phase-0 corpus (the only Drop fixture
  has a scalar field + `pass` body) → the gate is legitimately green.
- **D4 position-6 plain-`self`-write detection** needs method-body write analysis
  (the `&self` sibling was closed post-B2-review with pinning tests).
- **position-5 (capture) consolidation** — uses an inline taint check; should
  route through the centralized `reject_if_tainted_live_place` helper.
- **CLI exit codes on frontend errors** — provisional; the trap-normalization spec
  text (RFC §4) will pin the final scheme. (Today `run`/`trace`/`gen` already exit
  non-zero — `EXIT_USAGE`=2 — on parse/elaborate errors.)

## Phase-1 punch list (RFC §6)

- **Coverage completion:** the §2.6 rows-1-2 remainder beyond the phase-0 subset —
  closures with D5 capture lists, traits/generics/trait objects, comprehensions,
  the long tail of stdlib-free constructs; row-3 statics (`const` locals, module
  `static` globals).
- **Fix the 4 production bugs** above in BOTH backends + negative/positive
  fixtures; fix the `ggdef` transitive-drop gap so drop-count spectests can gate.
- **Frontmatter migration:** the converter from the ~1,218 literal harness
  expectation pairs → `ggdef -- gen` regeneration → human-reviewed diff (blocked
  for float-printing fixtures on the D8 shortest-round-trip appendix landing).
  The `spectests/` skeleton (this increment) + `gen` are the migration target.
- **Per-impl conformance reports + monotone floors** in `tests/lints.rs`
  (`spec_conformance_<impl>`); `ggdef` verdict lane in **smith** (tri-state triage).
- **D4/D5/D6 rejections** in elaboration + production + negative fixtures; the
  static-error / parse-error / annexe tiers fill in (`adjudicator:` provenance per
  RFC §4); the diagnostic-code (`E_`) registry.
- **Prose completion:** the `spec/prose/` stubs → full normative prose (formatting
  appendix D8, trap normalization, the four-outcome section, worked examples), each
  merge-gated with its `eval.rs` cite.

## What Increment C landed

- **`ggdef -- gen`** (new CLI mode + `lib::gen_frontmatter`): runs a `spectests/`
  fixture and writes the observed outcome into its frontmatter `expect:` block, in
  place; idempotent (`gen(committed) == committed`), gated by
  `tests/gen_idempotent.rs` + 4 lib unit tests.
- **`spectests/` skeleton:** the RFC §3 tier layout (`run/ static-error/
  parse-error/ annexe/ staging/`), a README quoting the §4 frontmatter schema, a
  README per tier, and 5 seed fixtures in `run/` (the two smith repros, the EMove
  witness, and two core-CoW showcases) with full frontmatter + `adjudicator: ggdef`
  + generated `expect:` blocks.
- **`spec/prose/` stubs:** one file per §2.2 bullet, each stating its rule + a
  `<!-- cites: eval.rs::<fn> -->` cross-cite, plus an index README.
- **Reports:** [`adjudications.md`](adjudications.md) (the three verdicts + trace
  justifications) and this file.
