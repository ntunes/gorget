# EXECUTOR BRIEF: ggdef phase 0 — the walking skeleton (Increments A/B/C)

> **STATUS: v5 (v4 + B1/B2 split fold, confirmed 2026-07-06) — passes 1 (5 res) + 2 (3 res) folded; pass 3 (Opus) = SIGN OFF FOR INCREMENT A
> (2 non-blocking findings folded into v4). B/C need one confirming pass on their sections
> before launching (the F2 report-homes fix landed post-sign-off).**
> **Executor launches: A: ✅ LANDED + MERGED (Opus; 26/26 MATCH; output-review SIGN OFF) ·
> B: SPLIT into B1/B2 per the B/C confirming pass (6 reservations folded 2026-07-06);
> B1: ✅ LANDED + MERGED 2026-07-06 (75/75 gate MATCH; output-review folds applied: report corrected, call-side named args rejected pending B2 reorder; ggdef surfaced 4 PRODUCTION bugs — filed) ·
> B2: after B1 lands · C: after B2.**
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
   at tests/lints.rs:178 for style): scan `spec/ggdef/src/**/*.rs` for `use` lines; **DENYLIST
   semantics** — FAIL if any path resolves into `ir`, `semantic`, `lir`, `bir`, or `backend`
   modules of the root crate. (Typical legal imports, illustrative not exhaustive: `lexer`,
   `parser` incl. AST, `span`, `errors`, `intern`, `compiler_data`, std.) Budget = 0, fatal
   from day one. NOTE for the executor: this is a SOURCE-discipline fence, not a link fence —
   ggdef links the whole gorget lib, and root-crate modules like `src/errors.rs` internally
   reference `crate::semantic` (that is fine and not your concern); the fence applies only to
   ggdef's OWN `use` lines.
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
   (bare/&/!); desugar: **collection literals `[a, b, c]` → Vector construct + pushes**
   (without this only ~16 cow fixtures qualify for A; with it ~34 — measured in review
   pass 1); f-strings with int/string interpolations → concat/print forms; method→call for
   the builtin Vector/String methods the A fixture set uses (len/push/set/index; Option and
   `.unwrap()` are B — A avoids fixtures needing Option); for→explicit loop;
   **operators → intrinsic calls per C9** (int/bool comparisons → bool-returning intrinsics —
   the for-desugar's bound check `i < n` needs this; String `+` → concat intrinsic; int
   arithmetic already covered by the checked ops); **`from std.collections import ...` lines are a
   parse-and-DISCARD NO-OP in A, name-agnostic** (e.g. `from std.collections import Vector`,
   which 5 of the 8 named fixtures carry; those types are prelude-available per RFC §2.6 —
   the full shim mechanism, incl. `std.conv.int_to_str`, stays B).
6. **CLI**: `cargo run -p ggdef -- run file.gg` (prints program stdout; exit code per
   outcome: Value→0, Trap→101, IllFormed→102, FuelExhausted→103 — pin these in a const with
   a doc comment; they are provisional until the trap-normalization spec text lands in B) and
   `-- trace file.gg` (events as JSONL to stdout after the program output, separated by a
   `---trace---` line).
7. **First conformance evidence**: a script or test (`spec/ggdef/tests/corpus_a.rs`) that runs
   ggdef over a HARDCODED list of ~20 Increment-A-compatible `cow_*` fixtures (pick fixtures
   using only A-subset constructs — VERIFIED-A-clean candidates from review pass 1:
   `cow_struct_sever_on_mutation`, `cow_amp_owned_writethrough`, `cow_transitive_alias`,
   `cow_index_proj_alias`, `cow_collection_element_mutate`, `cow_loop_borrow_propagation`,
   `cow_scope_exit_alias`, `cow_fieldpath_double_fire`; select the rest by READING the
   fixtures — the A-clean pool is ~34 with list-literals in A; do NOT pick Option/`.unwrap()`
   users like `cow_borrow_basic` or `cow_lazy_d1_alias_deadpath`, those are B corpus;
   document the list + why each qualifies) and compares stdout to the expected outputs
   extracted from `tests/integration.rs`'s literal `run_gg` pairs (parse the Rust string
   literal — including the `\`-continuation multi-line form, e.g. integration.rs:4895-4900 —
   never retype it). **Acceptance A: all listed fixtures MATCH.** Any mismatch: STOP, do not "fix" ggdef
   to match without checking the divergence table below — it may be an expected divergence or
   a genuine spec finding to report.

**The divergence decision table (memorize; applies to all increments):**

| ggdef differs from the committed fixture expectation because… | Action |
|---|---|
| the fixture relies on plain-`self` write-through | EXPECTED (D2): record in the increment report; the fixture's spec-expectation regenerates under D2; do NOT change ggdef |
| the fixture is an EMove lazy-read shape (post-mutation value) | EXPECTED (D1): ggdef's pre-mutation value is correct; Rust gg is the bug |
| the fixture is one of the two smith bugs (`cow_dead_branch_alias_bind` → 9, `move_param_concat` → ablog) | EXPECTED: ggdef MUST produce the correct value — this is acceptance (c) |
| anything else | POTENTIAL GGDEF BUG or spec finding: STOP on that fixture, write a minimal repro, report it in the increment report for orchestrator triage. Never silently patch eval.rs to match production output |

**Gates A** (foreground, tee to /tmp/ggdef_a_*_$RANDOM.log): `cargo build --workspace` clean
(bare `cargo build` only builds the root package — ggdef needs `--workspace` or `-p ggdef`);
`cargo test --lib` unchanged-green (root package — bare form is correct here); `cargo test --test lints` green INCLUDING the new ratchet;
`cargo test -p ggdef` green (unit tests for eval — minimum: one test per §2.2 bullet,
including materialize-then-read-sees-copy, owner-untouched, move-then-read=IllFormed,
fresh-temp-move, drop order); the corpus_a run with its MATCH list printed.

---

## Increment B — the full phase-0 surface (SPLIT into B1/B2 per the B/C confirming pass;
## sequential — B2 depends on B1)

### Increment B1 — the non-equip surface (~103 gate fixtures)

**Deliverables:** extend ggc/eval/elaborate to: Option/Result as ordinary enums +
`.get()`/`.unwrap()`/`.unwrap_or()` (Trap on unwrap-None with the normalized panic shape —
the single biggest feature, ~47 fixtures); match + user payload enums + pattern bindings
(new `Proj::Payload`; bindings are Borrow-mode per §2.2); Dict/Set values (insertion-ordered);
ranges + string slices `s[a..b]` (values per D1); named-arg construction; the v1 shim list
(`std.collections` import mapping + `std.conv.int_to_str` as a GGC intrinsic); **the corpus's
full builtin-method set — including `fill`, `pop`, `clear`, `trim`, `substring`** (measured
in the confirming pass; the gate + stop-and-report force any stragglers); full sized-int
matrix + `as`-cast saturation rules (**unit-tested only — zero corpus fixtures**); by-value
closures (bare closures capture by value at creation; D5 capture lists are phase-1 —
**gated by whichever corpus fixtures use them, else unit-tested only**).

**Gate B1:** every cow_*/deadwrite_* corpus fixture **without an `equip` block** MATCHes
(minus the standing exclusions), via `spec/ggdef/tests/corpus_b1.rs`; divergence table
applied; all prior gates green.

### Increment B2 — equip, Drop, and the D4 rejections (~13 equip fixtures + full corpus)

**Deliverables:** **receiver-type inference** — called out explicitly because A has ZERO type
inference and name-matching dispatch is IMPOSSIBLE (the corpus contains
`cow_named_recv_gate_name_collision`/`_projected`, whose user `get(&self)` collides with the
builtin `.get()`): the elaborator gains a per-function type environment (locals are typed at
bind sites; GGC is monomorphic, so this is propagation, not unification); concrete `equip`
method→call with self-mode handling (D2: plain `self` = bare binding); `equip T with Drop`
with custom-drop EXECUTION (mechanical note from the confirming pass: `drop_scope` must
thread `Ctx` and become `Result<(), Halt>` — a custom drop can Trap/recurse/exhaust fuel);
transitive `drop_tainted` computation; **D4 rejections at all six implicit-copy positions
with LIVE-PLACE sources** (fresh temps move, never rejected — e.g.
`cow_element_borrow_source_mutate_with`'s `with Res(1) as r:` is a fresh-temp Move and must
keep running) — emit the `E_MoveWithoutOperator`-family error text; **B's D4 testing =
ggdef unit tests** (production-side rejections + negative conformance fixtures are phase 1
per RFC §6); `with expr as name:` (scoped bind via fresh-temp Move + drop-at-exit); (F1)
ratchet hardening — second scan over FULL source text for bare
`gorget::(ir|semantic|lir|bir|backend)::` path segments (use-line-only scan is bypassable,
confirmed); (F2) emit a structural-move trace event for fresh-temp binds + update the
`fresh_temp_bind_is_a_move_not_a_copy` unit test (preferred over comment-fixing; provenance
completeness); **(from B1 output-review R2) call-side named-arg REORDER** — replace the
B1-interim elaboration rejection (`reject_named_args`) for ORDINARY function calls with the
proper reorder keyed on param names (mirror `struct_ctor_args`); enum-variant/collection-ctor
positions may keep the rejection (owner call in review if contested); + unit tests both ways.

**Gate B2:** the **entire corpus** — cow_* minus the 3 generic-equip exclusions
(`cow_element_borrow_alias_mutate`, `cow_p3_alias_chain_mutate`, `cow_p3_index_mutate`) and
deadwrite_* minus `deadwrite_ok_atomic_add` — via `spec/ggdef/tests/corpus_b.rs`. Acceptance
per RFC §6(a)+(b) with the divergence table applied; the increment report MUST list every
fixture per divergence category (expected-D2, expected-D1/EMove, smith, findings). The
executor generates `spec/ggdef/reports/deadwrite_spec_expectations.md` (program → ggdef
stdout + which D-decision explains any delta vs production) for orchestrator/owner review —
these become the newly-ratified spec expectations.

---

## Increment C — adjudication + spectests skeleton + prose stubs

**Deliverables:**
1. **The three adjudications, from the definition** (RFC §6(c)): run ggdef on
   `tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg` (expect `9`),
   `tests/fixtures/known_gaps/move_param_concat.gg` (expect `ablog`), and a minimal EMove
   witness. **The EMove shape is precise (devbook/11:716-733; fixtures `cow_lazy_move_bind.gg`
   / `cow_lazy_move_reassign.gg`): (1) bind `String s = v.get(0).unwrap()` from the PRE-move
   name `v`; (2) THEN `Vector[String] w = !v`; (3) THEN mutate through the POST-move name
   (`w.set(0, "mutated")`); (4) THEN print `s`. The bind-source name MUST differ from the
   mutation-target name — that asymmetry IS the EMove phenomenon (the lazy binding is keyed
   by `v` but the mutation goes through `w`). A same-name variant (`s = w[0]` after the move)
   is degenerate and does NOT witness it.** ggdef must print the PRE-mutation value. Produce
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

## Report homes (all increments)

Increment reports, B's `deadwrite_spec_expectations.md`, and C's `adjudications.md` live in
**`spec/ggdef/reports/`** (in-zone: `spec/**`; `docs/**` is orchestrator-owned). Increment A's
report = the agent's final message + the fixture-rationale doc-comment in `corpus_a.rs`.

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
