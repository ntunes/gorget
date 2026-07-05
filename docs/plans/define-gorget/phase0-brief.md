# EXECUTOR BRIEF: ggdef phase 0 — the walking skeleton (Increments A/B/C)

> **STATUS: v1 — review passes cleared: 0 of ≥3 (update this line per pass).**
> **Executor launches: A: not launched · B: not launched · C: not launched** (update in place).
> Normative sources: [`rfc-ggc-ggdef.md`](rfc-ggc-ggdef.md) (APPROVED — §2 is the semantics,
> §3 layout, §6 phase-0 scope/acceptance), [`decisions.md`](decisions.md) (D1–D8).
> This brief is deliberately self-contained enough for an Opus-class executor: where the RFC
> states WHAT, this brief adds the operational HOW plus the judgment calls pre-made.

## Shape of the work: three increments, one executor run each

Run each increment as its own worktree executor with a fresh output-review before the next.
Commit per increment. An increment's gates must be green before the next launches.

---

## Increment A — crate, fence, core evaluator, first 20 fixtures

**Deliverables:**
1. **Workspace conversion + `spec/ggdef` crate.** Root `Cargo.toml` gains a `[workspace]`
   with members `[".", "spec/ggdef"]` (the root package keeps building exactly as before —
   verify `cargo build` + `cargo test --lib` are unaffected). The ggdef crate replicates
   `[lints.rust] warnings = "deny"`. Depends on the root crate (path dep) for lexer/parser/AST
   ONLY.
2. **The import ratchet lint FIRST** (before any evaluator code): a new test in
   `tests/lints.rs` following the existing ratchet pattern (see `no_growth_in_name_prefix_routing`
   for style): scan `spec/ggdef/src/**/*.rs` for `use` lines; FAIL if any path resolves into
   `ir`, `semantic`, `lir`, `bir`, or `backend` modules of the root crate. Allowlist: `lexer`,
   `parser` (incl. AST), `span`, plus std. Budget = 0 violations, fatal from day one.
3. **`src/ggc.rs`** — the GGC data types per RFC §2.1, for the Increment-A subset: scalar
   values (int64/bool/float64 only in A; full sized-int matrix in B), String, Vector, struct,
   tuple; places (local/field/element); the three mode tags (Borrow/WriteThrough/Move);
   expressions/statements for: bind, read, write-to-place, call, construct, index/field
   access, if/while/loop/return/break/continue, explicit clone, scope-drop markers, print.
4. **`src/eval.rs`** — ONE fuel-indexed functional-big-step function per RFC §2.3/§2.7:
   `eval(fuel, state, node) -> Outcome` with the four outcomes (Value/Trap/IllFormed/
   FuelExhausted); trace events on ALL outcomes (`src/trace.rs`: BindCopy, Move, ExplicitClone,
   Materialize, Write, Drop — each with span + place provenance). Implement §2.2 EXACTLY:
   live-place copies at the five positions; fresh-temp structural Move; Borrow = view on read;
   **materialize-on-first-write** (persistent private copy in the binding; owner untouched;
   copy drops in borrower scope); WriteThrough aliases; Move kills the source (later read =
   IllFormed); scope-exit drops in reverse declaration order. Checked arithmetic → Trap(Fault).
5. **`src/elaborate/`** — for the A-subset only: reuse the production lexer+parser; own tiny
   resolver (locals + function names; no generics yet); mode-tag resolution from syntax
   (bare/&/!); desugar: f-strings with int/string interpolations → concat/print forms;
   method→call for the builtin Vector/String methods in the A fixture set (len/push/get/
   unwrap on Option in B — in A avoid fixtures needing Option); for→explicit loop.
6. **CLI**: `cargo run -p ggdef -- run file.gg` (prints program stdout; exit code per
   outcome: Value→0, Trap→101, IllFormed→102, FuelExhausted→103 — pin these in a const with
   a doc comment; they are provisional until the trap-normalization spec text lands in B) and
   `-- trace file.gg` (events as JSONL to stdout after the program output, separated by a
   `---trace---` line).
7. **First conformance evidence**: a script or test (`spec/ggdef/tests/corpus_a.rs`) that runs
   ggdef over a HARDCODED list of ~20 Increment-A-compatible `cow_*` fixtures (pick fixtures
   using only A-subset constructs — candidates: `cow_borrow_basic`, `cow_struct_sever_on_mutation`,
   `cow_amp_owned_writethrough`, `cow_lazy_d1_alias_deadpath`, plus ~16 more the executor
   selects by READING the fixtures; document the list + why each qualifies) and compares
   stdout to the expected outputs extracted from `tests/integration.rs`'s literal `run_gg`
   pairs. **Acceptance A: all listed fixtures MATCH.** Any mismatch: STOP, do not "fix" ggdef
   to match without checking the divergence table below — it may be an expected divergence or
   a genuine spec finding to report.

**The divergence decision table (memorize; applies to all increments):**

| ggdef differs from the committed fixture expectation because… | Action |
|---|---|
| the fixture relies on plain-`self` write-through | EXPECTED (D2): record in the increment report; the fixture's spec-expectation regenerates under D2; do NOT change ggdef |
| the fixture is an EMove lazy-read shape (post-mutation value) | EXPECTED (D1): ggdef's pre-mutation value is correct; Rust gg is the bug |
| the fixture is one of the two smith bugs (`cow_dead_branch_alias_bind` → 9, `move_param_concat` → ablog) | EXPECTED: ggdef MUST produce the correct value — this is acceptance (c) |
| anything else | POTENTIAL GGDEF BUG or spec finding: STOP on that fixture, write a minimal repro, report it in the increment report for orchestrator triage. Never silently patch eval.rs to match production output |

**Gates A** (foreground, tee to /tmp/ggdef_a_*_$RANDOM.log): `cargo build` (workspace) clean;
`cargo test --lib` unchanged-green; `cargo test --test lints` green INCLUDING the new ratchet;
`cargo test -p ggdef` green (unit tests for eval — minimum: one test per §2.2 bullet,
including materialize-then-read-sees-copy, owner-untouched, move-then-read=IllFormed,
fresh-temp-move, drop order); the corpus_a run with its MATCH list printed.

---

## Increment B — the full phase-0 surface

**Deliverables:** extend ggc/eval/elaborate to the FULL phase-0 subset (RFC §6 phase 0,
verbatim list): Dict/Set values (insertion-ordered); Option/Result as ordinary enums +
`.unwrap()`/`.unwrap_or()` (Trap on unwrap-None with the normalized panic shape); match +
user payload enums + pattern bindings (Borrow-mode per §2.2); concrete `equip` method→call
including `equip T with Drop` (custom drops run per §2.2; the `drop_tainted` bit computed
transitively; D4 rejections at all six positions with live-place sources — emit the
`E_MoveWithoutOperator`-family error text); full sized-int matrix + `as`-cast saturation
rules; ranges + string slices `s[a..b]` (the W3c view shapes — values per D1); named-arg
construction; `with expr as name:` (scoped bind via fresh-temp Move + drop-at-exit); while
loops; the v1 shim list (`std.collections.{Vector,Set,Dict}` import mapping +
`std.conv.int_to_str` as a GGC intrinsic); by-value closures (no capture lists — D5 capture
lists are a phase-1/production item; bare closures capture by value at creation).

**Gates B:** everything from A, plus `spec/ggdef/tests/corpus_b.rs` running the **entire
cow_* family minus the RFC's 4-fixture exclusion list** and the **deadwrite_* programs minus
`deadwrite_ok_atomic_add`**. Acceptance per RFC §6(a)+(b) with the divergence table applied;
the increment report MUST list every fixture in each divergence category (expected-D2,
expected-D1/EMove, smith, findings). For the deadwrite programs the executor generates a
`deadwrite_spec_expectations.md` (program → ggdef stdout + which D-decision explains any
delta vs production) for orchestrator/owner review — these become the newly-ratified
spec expectations.

---

## Increment C — adjudication + spectests skeleton + prose stubs

**Deliverables:**
1. **The three adjudications, from the definition** (RFC §6(c)): run ggdef on
   `tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg` (expect `9`),
   `tests/fixtures/known_gaps/move_param_concat.gg` (expect `ablog`), and a minimal EMove
   witness (write it: move a vector, bind an element-derived String BEFORE a mutation through
   the new owner, print — ggdef must print the PRE-mutation value). Produce
   `adjudications.md` recording each verdict with the trace excerpt that justifies it.
2. **`spectests/` skeleton**: directory layout per RFC §3; move NOTHING yet (migration is
   phase 1) — just the layout, a README quoting the frontmatter schema from RFC §4, and
   3-5 seed fixtures (the two smith repros + the EMove witness) in `spectests/run/` with
   full frontmatter and `adjudicator: ggdef`, expectations generated via a new
   `-- gen file.gg` CLI mode (emits/updates the frontmatter `expect:` block in place).
3. **`spec/prose/` stubs**: one file per §2.2 bullet with the rule stated and a
   `<!-- cites: eval.rs::<fn> -->` comment — full prose is phase 1; the skeleton pins the
   structure (HaMLet-style code↔prose cross-citation).
4. **Increment report** = the phase-0 completion report: MATCH tables, divergence categories,
   findings list, and the phase-1 punch list.

**Gates C:** all prior gates; the three adjudications correct; `-- gen` idempotent (second
run = no diff); full-workspace `cargo build`/`--lib`/`lints` green. The parent (orchestrator)
runs the full integration sweep after merge — executors never do.

---

## Operational rules (all increments — the CLAUDE.md digest)

Worktree preamble verbatim (run FIRST): `pwd` + `git rev-parse --show-toplevel` must point
inside YOUR worktree; never touch `/workspace/gorget` or `/workspace/gorget-1`; worktree-
RELATIVE paths only; never `git stash` (checkpoint via `git diff > /tmp/<name>.patch`); on
Edit-tool desync re-Read and retry, never heredoc-with-absolute-path; after any non-Edit
write, check `git -C /workspace/gorget status` and STOP on unexpected MAIN changes.
Zones: `spec/**`, `spectests/**`, root `Cargo.toml` ([workspace] only), `tests/lints.rs`
(additive), `spec/ggdef/tests/**`. NEVER: `src/**`, `TODO.md`, `docs/**` (orchestrator owns),
`tests/integration.rs`, `tests/fixtures/**` (read-only). Commit per increment with EXPLICIT
file lists; message prefix `feat(ggdef):`. Checkpoint diffs to /tmp early and often; final
gates FOREGROUND with generous timeouts, teed. If blocked on anything this brief doesn't
answer: STOP and report — do not improvise semantics; the RFC §2.2 + decision table are the
only sources of truth, and a gap in them is a finding, not a license.
