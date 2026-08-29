# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

**Who this binds.** Everything down to and including § Task Continuity binds **every agent** — orchestrator, scout, executor, reviewer alike. The last three sections (§ Multi-agent orchestration, § Review …, § Round lifecycle) bind the **orchestrator**, plus whatever a brief passes on to the agent it launches. Read the part that binds you, and obey the Core invariants regardless.

## Core invariants (read first)

The sections below are the spec; these are the load-bearing rules they reduce to.

**How a rule lands here — a fresh owner ruling included.** Write ONE compact imperative in the present tense; its provenance, measurement and war-story go to [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md) (engineering) or [`docs/devbook/30`](docs/devbook/30-excellence-system.md) (the excellence system); unratified owner open-thinking goes to devbook/30, marked as such. Sharpening an existing rule EDITS that rule in place — never a second dated copy beside it. **Four lints hold the line** (`tests/lints.rs`), and none of them is total — their measured reach is `todo/t0714`. **So the diff is the guard: after editing a rule, re-read the whole rule**, not just the clause you changed. State every excellence-system rule HERE, **never only in one harness's private memory**.

1. **Fix at the write site, not the read site.** A complex read-side fix (save/restore, phi repair, per-case rules) means a writer one layer up dropped a typed invariant. (→ Layering discipline)
2. **Typed metadata, never name-matching.** No `name.starts_with("Vector__")` to decide *meaning* — flag the typed decl, set at the source, read via an accessor. (→ Layering discipline)
3. **Register ownership at the value's birth.** Every freshly-materialized owned, droppable value is registered for drop (or provably moved) at the producer; the leak/double-free class is always a missing or mis-typed ownership tag. (→ Ownership at Consuming Positions)
4. **One fix, all siblings.** Fix the enumerated *class*, not the instance; centralize at the producer; add an arm-count lint. (→ Layering discipline)
5. **Re-verify every premise; regenerate every number.** No dated figure enters a plan/brief/commit/handover unless you regenerated it this session. (→ Solution Quality)
6. **Convert a recurring bug class into an executable guard** (validator or `tests/lints.rs` ratchet: env-gate → burn down → fatal). Prose rots; guards don't. When passes or rounds keep finding ONE class in new costumes, the round's output owes the class-retiring guard. (→ `docs/devbook/25-structural-guards.md`)
7. **Gate on the bootstrap and the sanitizer**, not just a green suite — `self_host_bootstrap_fixed_point` + ASan catch what `cargo test` and the always-pass `*_comparison` diagnostics miss. (→ Build & Test)
8. **Reference-grade is the bar, not parity with a possibly-wrong reference.** "Matches Rust gg" / "both backends agree" / "only fails on programs that are UB on both" is *necessary, not sufficient*. If the agreed-on behavior is itself wrong, that is ≥2 bugs to fix in BOTH compilers — most often by making the language *reject* it. "Benign because both backends are UB" is a red flag, never a pass; the output-review must refuse to ship a known defect. (→ Review … fresh agent)
9. **A SEMANTIC change lands on every lane in the same round** — ggdef (in subset), Rust gg (C+LLVM), self-host — pinned by a cross-lane fixture, never a promise. Anything altering accept/reject ships with the conformance fixture encoding the intended FINAL state; a lagging lane is a red lane or an explicit `#[ignore]`+citation; out-of-subset shapes get a note + a filed subset gap. Implementation-internal fixes (one backend's codegen) are exempt. A track flipping fixture expectations carries the FULL ggdef suite. A round's OWN new fixtures must COMPILE + MATCH on the self-host lane the SAME ROUND; only PRE-EXISTING non-MATCH are exempt from `RUNTIME_DIFF_NONMATCH_CEILING`, and raising it for your OWN inflow is forbidden.
10. **Lower-or-reject — never silently drop user syntax.** Every lowering arm either lowers the construct or emits a check-time rejection; a `_ =>` fall-through that discards a write is a miscompile-class defect. Enforcement: the silent-fallthrough allowlist ratchet.
11. **Every fix ships wide, genuinely-exercising regression fixtures, same round.** Exercise the bug on the *real* path — non-constant operands, wired to RUN, one per sibling for a class, wide enough that a partial regression trips them (costumes · shapes · POS+NEG · lane pins). A single existing NEG with a thin harness pin is a floor, not the bar. The fixture lands WITH the fix, never "later", on every touch.
12. **A regression fixture is not coverage until it has been seen to FAIL — and a fixture set that samples one value of a typed axis is an anecdote, not a net.**
    - **RED-verify.** Run every new fixture against the PRE-fix compiler and record the failure. A fixture green before *and* after the fix tests nothing, and is worse than none — it reads as coverage.
    - ⚠ **GREEN ON ARRIVAL IS NOT COVERAGE** — RED-verify binds EVERY new fixture. For a shipped feature, break the mechanism it claims to guard, confirm RED, restore. If neither red is possible, say so in the header and state what it pins instead.
    - **A fixture's NAME is a claim about SCOPE** — make it true or narrow it; record which CELL of which axis it samples.
    - **Axis-complete.** Where behaviour depends on a typed axis — field type · receiver/root shape · backend · lane · element type — the net must cover every value of that axis, or name each omitted cell and why. ENUMERATE its axes first, and check what each fixture *actually* exercises. Go TYPE-first on a partial audit.
13. **Verify the verifier — and pick an instrument that can SEE the failure class.**
    - **Demonstrate a red.** Before reporting "gates green", show at least one gate going RED on a deliberately broken variant. A gate that has never been seen to fail is not evidence.
    - **Ask ggdef FIRST — a TRIAGE instrument, not just a round-close gate.** Run the shape through the oracle during triage and treat disagreement as the finding. But ggdef adjudicates VALUE SEMANTICS and is STRUCTURALLY BLIND to memory-invalidation — it accepts live heap-UAFs. ASan on the real backends adjudicates memory validity. ggdef can LAG a ratified decision — or be WRONG: it IMPLEMENTS the definition, it is not the definition. A BOTH-WRONG row is an OWNER ASK only if the semantics are UNRATIFIED; where the ledger rules, fix ggdef.
14. **An invariant-asserting comment needs an enforcing guard, or it gets DELETED.** "This is unreachable", "the only consumer is X" — either a `debug_assert!`/lint/typed guard, or rot that will mislead a reader who trusts it (Core #6 applied to prose). When you touch code near such a comment, verify it or delete it — never inherit it.
15. **Make rigor MECHANICAL, not clever — the gauntlet must still work with a weaker reviewer.**
    - **(a) Every load-bearing claim in a brief carries its VERIFICATION COMMAND.** A claim with no command is not a claim, it is a hope.
    - **(b) Scope over a SET → present the SET:** the total enumeration with a disposition per row (LAND / DEFER / NEVER + reason), never a selection.
    - **(c) FOLD AT THE GRANULARITY OF THE DEFECT — VERIFY AT THE GRANULARITY OF THE SECTION.** One clause wrong ⇒ edit that clause; then re-read the WHOLE enclosing section — heading, both neighbouring paragraphs, and the comments inside its examples. On a rewrite, ask "what did the old text stop saying".
    - **(d) Fold a correction → GREP for the thing it corrects**, in its *instruction form* (`old`→`new`), grepping the SHORTEST DISTINCTIVE TOKEN, never the sentence. Instruction form, because explanatory prose legitimately mentions the old value. Edit-asserts catch a MISSING fold; only a grep catches a SURVIVING CONTRADICTION.
    - **(e) Fixed procedures for the recurring claim types**, run without judgement: *"fixture F guards mechanism M"* → break M, run F, confirm RED · *"X is filed"* → grep the record · *"there are N sites"* → run the census command and compare · *"shape S behaves B"* → build and run on C AND LLVM, plus ggdef when in-subset. MIND THE PROBE: never test accept/reject inside an f-string, never read a crash off a PIPELINE · *"the gates are green"* → make one go RED, once · *"line L says X"* → read L at HEAD.

**A round runs SEVERAL TRACKS IN PARALLEL, each on ITS OWN WORKTREE, and every track runs its own gauntlet:** scout → brief → ≥3 fresh brief-reviews → launch (worktree) → fresh output-review → integrate (→ Review), inside the Round lifecycle. **Only that track's EXECUTOR changes code** — scouts prototype and throw away, reviewers propose without implementing, and the orchestrator coordinates (Multi-agent rule 0).

## Build & Test

```bash
cargo build                                          # build the compiler
cargo test --lib                                     # unit tests (~1027)
scripts/run_integration.sh                  # integration tests (autoscaled)
cargo test                                           # all tests
```

**Always pipe integration tests through `tee`** with a random filename — parallel agents collide on fixed names:

```bash
scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log
```

**LLVM backend.** Set `GG_BACKEND=llvm` to append `--backend=llvm` to every `gg build` (all-or-nothing per run; `tests/integration.rs:52-103`). Full sweeps autoscale via `scripts/run_integration.sh`; no `--test-threads=1`.

```bash
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 scripts/run_integration.sh --release 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

**Backends should be at parity**; a regression on one but not the other means the change touched a backend-specific path, not shared LIR.

**Timeouts** (override on loaded hosts): `GG_BUILD_TIMEOUT_SECS` (outer `gg build`; default 120/180; bump to 600 on multi-agent boxes for DEBUG self-host builds), `GG_TEST_TIMEOUT_SECS` (per-test binary; default 30; bump for `stress_*` / p2p / gorget-arena).

## Documentation

- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience, not Gorget experience)
- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale
- `docs/devbook/` — [Compiler Internals Book](docs/devbook/README.md): contributor-facing pipeline and design docs

**`docs/book/` and `docs/devbook/` read like a published book** — timeless present-tense design narrative faithful to INTENDED behavior, never a fix-log. No dates, commit hashes, `Snag #N` labels, or parity/perf "win" numbers in design chapters — those belong in `DONE.md` and the playbook chapters (`docs/devbook/29`–`30`). A round that changes behavior owes a doc-write-through; book-ifying a chapter that has rotted into changelog style is its own DOC track, reviewed like any work.

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, borrow checking
- `src/ir/` — Intermediate representation and lowering from AST (monomorphization, drop insertion, closures)
- `src/lir/` + `src/backend/c_lir/` — SSA-based LIR; `src/bir/` — BIR lowers canonical ops before backend emit
- `src/backend/llvm/` — LLVM IR backend (`--backend=llvm`)
- `src/backend/c/` — C runtime library and SQLite amalgamation
- `src/formatter/` — Source formatter (`gg fmt`)
- `src/loader.rs`, `src/lockfile.rs`, `src/manifest.rs` — Package management
- `src/report.rs` — Test report generation
- `tests/fixtures/*.gg` — Integration test programs with deterministic stdout
- `tests/integration.rs` — Integration test harness: builds fixtures via `cargo run -- build`, executes, asserts stdout

## Language Syntax (Quick Reference)

- Indentation-based blocks (Python-style), type-first declarations: `int x = 5`, `String name = "hello"`
- Functions: `int add(int a, int b): return a + b` / expression-body: `int double(int x): x * 2`
- Closures: `(int x): x * 2` / function types: `int(int, int)` (return type first)
- Match uses `case`: `match x: case 1: ... else: ...`
- **Enum variants are qualified**: `Color.Red()` not `Red()` (prelude variants `Ok`, `Error`, `Some`, `None` stay bare)
- `meta` keyword for compile-time evaluation — see `docs/language-reference.md` for full builtin list
- Mutable borrow (`&`) / move (`^`) sigils go in the name's slot — before the name, or alone if unnamed (D35). Never before the type:
  `void modify(Message &msg)` ✓ — `void modify(&Message msg)` ✗
  **`void consume(Message ^msg)` ✓** / `Callable[void(int &)]` ✓ — `^Message msg`/`(&int)` ✗

**Always use type-first Gorget syntax** in code, plans, and examples: `int x = 5`, `String greet(String name)`. The only string type is `String` — `str` is not a keyword.

## Ownership at Consuming Positions (push/put/set/insert/send, constructors, returns, captures)

CoW's default everywhere is **borrow** — bare-identifier assignments
(`Spanned b = a`), call args, match scrutinees and collection reads all
propagate Ptr aliases at zero cost. Clones happen
**only at ownership boundaries, where the destination must own**
(collection puts, constructor / struct / enum field init like
`S(name)` / `Some(name)`, returns, closure captures). The rule is
uniform — there is no push-vs-constructor split:
clone-if-the-source-is-live, move-if-it-is-dead. Even at the boundary,
**the compiler prefers move when liveness allows it**.

**The carve-outs to CoW-default-borrow are**: closures / `Callable[T]`,
`Owned[T]`, `Box[T]`, `Task`, `TaskGroup`, `Guard` — these are
**single-owner-by-design (no clone path in the lowering)**, so the safety pass
emits `MoveWithoutOperator` (E_MoveWithoutOperator)
**at bare-assign sites AND at constructor / struct / enum-init sites**, forcing
**the user to write `^source` or `source.clone()`**. (At a plain function / method call these types are simply borrowed, so no operator is needed.)

At each consuming position (`push`, `put`, `set`, `insert`, `send`,
`v[i] = x`) the collection must own. The compiler **picks per-arg from
typed ownership state** (Phase D's `LocalOwnership`):

| Source                                            | Action                |
|---------------------------------------------------|-----------------------|
| Owns AND dead at this call                        | move after call       |
| Borrow, OR owned but live past this call          | clone before call     |
| Static literal                                    | runtime *_materialize |

**The three move-eligible shapes are**: `^arg` (user opt-in), expression
temp (last-use + owning by construction), and named local at last use
bound to an owned value (not from `.get()`, a view-returning method,
or a parameter — those bind borrows).

On a valid move the source slot becomes logically dead (IR `MoveZero`; **the
backend zeros the source only when drop-tracking would otherwise re-drop it**).
The clone case is required, not a fallback: a borrowed or still-live source
would be a use-after-free if moved. The decision is mechanical, not heuristic.

**This is the compiler contract — not a suggestion.** Full spec:
[`docs/devbook/11-copy-on-write.md`](docs/devbook/11-copy-on-write.md#materialization-points--the-enforced-boundary-set).

## Solution Quality

- **Prefer robust, architecturally sound solutions over quick fixes.** When the trade-off is unclear, discuss both approaches and ask before proceeding.
- **Aim for generic solutions that solve classes of problems.** Be resourceful — read code, search the web, study how other compilers solve it. Exhaust every avenue before concluding something can't be done.
- **Flag code smells and structural issues** even when unrelated to the task. Log non-trivial findings to `todo/`.
- **You are allowed an opinion.** If the user is proposing something dumb, call him out.
- **You are allowed to swear if opportune.** But if something deserves a 'holy shit', use it!
- **Performance work measures MEMORY, not just time.** Every perf fix tracks peak RSS + alloc/clone counts (`--clones=stats` → the `[clone-stats] array_clone=N` line, `/usr/bin/time -v`, `scripts/self_host_mem_baseline.sh`) alongside wall-clock — a memory balloon is as blocking as a time regression.
- **Re-verify a premise against CURRENT source/tests before acting on it (Core #5)** — re-run the test, re-read the cited source, check the actual current code shape. No un-regenerated numbers: a figure you did not regenerate this session enters no plan, brief, commit, handover, or statement to the owner — quote the *command*, not the stale value (the `*_comparison` tests are always-pass; only freshly-printed counts mean anything).
- **Consult history before proposing a design** or briefing a design-heavy task: grep `DONE.md`, `todo/`, `git log`, AND the Rust impl in `src/`. Don't wait to be asked. Skip only for mechanical/greenfield changes.

## Layering discipline

How information crosses IR layer boundaries (AST → GIR → LIR → backend). Full rules in [`docs/devbook/24-layering-discipline.md`](docs/devbook/24-layering-discipline.md); four-line summary:

1. **Lossless on invariants, lossy on syntax.** A layer may resolve abstractions and add information; it may not drop semantic invariants (ownership, drop strategy, view-vs-owned, ABI, copy semantics, borrow provenance). Invariants accumulate; abstractions evaporate.
2. **Typed metadata, not name-matched.** Facts cross as typed fields on structs — never name prefixes, sentinel values, or runtime-symbol conventions. (See "No name matching" below.)
3. **One source of truth per axis.** Exactly one piece of metadata at one location, read through one accessor. No parallel sidecar maps.
4. **Resolve once, write through.** A resolved abstraction writes into the next layer's typed metadata. Downstream doesn't redo the work and doesn't get to disagree.

**Litmus test:** if a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong. The fix is always upstream. Cite the doc in PRs that touch IR layer boundaries.

### No name matching (rule 2 at the runtime-symbol boundary)

**Do not pattern-match on function names, type names, runtime-symbol prefixes**, or any identifier string to make a semantic decision. Writing `matches!(name, "gorget_str_trim" | ...)` or `if name.starts_with("Vector__")` to decide what something *means* — stop. The metadata you need is missing one layer up.

Symptoms: parallel lists in different files kept in sync by hand; new methods silently misbehaving because a name list wasn't updated; `// keep both lists in sync` comments.

The fix: put the semantic flag on the typed declaration (`BuiltinMethodDecl.returns_view`, `Inst::CallRuntime` sidecar), set once at the source, read via typed accessors. If the metadata doesn't exist yet, **add it** rather than fishing for the answer in a name.

Exception: at the C-emit boundary you have to spell the runtime symbol (the name *is* the contract with the runtime). Even there, drive the spelling from a typed registry — never route on `if name == "..."`.

### Debugging heuristic — fix complexity as a signal of wrong layer

When the fix you're sketching is *intrinsically complex* — save/restore around branches, phi insertion at merges, scope-tracking name maps, manual SSA repair — stop. That complexity is a tell that you're patching a *symptom*.

1. **Trace the data the buggy site is reading.** *Where was it last written?*
2. Look at the writer. *Did it respect all the typed metadata available?* Or did it default / hardcode / collapse cases the upstream had distinguished?
3. **Writer was lossy → fix at the source**; the downstream "complex fix" evaporates.
4. **Writer was faithful → trace one more layer up.** Repeat.

Every layer hop without finding the bug should make you *more* suspicious of your diagnosis, not less.

Worked examples (Snag #17, Snag #13): [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#the-debugging-heuristic-fix-complexity-is-a-signal-of-the-wrong-layer).

### Sibling-site drift — fix the class, not the instance

Fixing a bug at one position in an *enumerated set* — consume positions (`push`/`put`/`set`/`insert`/`send`/ctor/return/capture), tail-value dispatchers, container-literal arms, registration paths — fix the **class**, not the instance:

1. **Grep for the siblings before you commit.**
2. **Prefer centralizing at the producer** over patching each consumer (e.g. `maybe_auto_propagate` hoisted to the `lower_expr` exit).
3. **Add an arm-count lint** (like `container_literal_arms_count`) so the next sibling is forced through the shared path.

**Litmus test:** if the fix is "add the missing call to site N", ask "how many sites are there, and what stops site N+1 from the same hole?"

Sagas: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#sibling-site-drift-fix-the-class-not-the-instance).

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the expected output must reflect what the language *should* do.

**Forbidden: reshaping the surrounding code** (tests, fixtures, examples, production code) to avoid the gap. Even commented, this buries the bug — the wired-in expected output is the load-bearing artifact, not the comment.

Worked examples: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#dont-redesign-around-compiler-gaps).

**Litmus test:** a fixture using a more complex shape than necessary, or a workaround comment citing a bug, is likely a dodged gap. Verify the bug still exists before treating the workaround as canonical.

**This rule outranks Task Continuity's** "Never silently work around a bug" — that one is satisfied by recording the bug; this one is not.

## Self-host as the elegance showcase

The self-host frontend (`tests/fixtures/self_host_*/`) is the language's reference-grade demonstration. It must be written in **idiomatic Gorget**, not the way it had to be written to dodge an old compiler bug. It is at once a stress test for the compiler, a regression net (via `*_comparison` and `bootstrap_fixed_point`) and a showcase. The third role is non-negotiable.

**The succession plan.** The self-host REPLACES Rust gg as the primary reference at ~100% runtime parity, so a "reference lags the self-host" finding is a milestone: file it, fix the Rust side as oracle hygiene, never dumb the self-host down to match. As agreement-with-Rust loses meaning, ggdef adjudication is the truth axis.

Defensive code accumulated for past compiler gaps is technical debt with a stale justification.

Fossils already burned in, with the concrete list: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#self-host-as-the-elegance-showcase--and-retiring-fossils).

Rules:
1. **No defensive code without a live, cited bug.** Finding a workaround comment ("parallel because…", "wrapper to avoid…"), verify the bug still exists; if it doesn't, delete the workaround and use the idiomatic shape.
2. **Self-host code reads like the user manual.** If you wouldn't recommend the pattern in `docs/book/`, don't write it in self-host.
3. **When you fix a compiler gap, also retire the workarounds.** Search for the workaround pattern across all self-host directories before declaring the fix shipped.
4. **Periodically audit.** Treat the self-host as a living document and prune. The `*_comparison` and `bootstrap_fixed_point` tests will catch regressions.

**This rule pairs with "Don't redesign around compiler gaps"** — that one is about not creating new dodges; this one retires old ones.

## Task Continuity

**Work items live ONE PER FILE in `todo/`**: TOML front matter, `+++`, prose verbatim — spec in `scripts/todo_index.py`. A field the item's text does not state stays EMPTY. `TODO.md` keeps the handover, the invariants, and a GENERATED index (`scripts/todo_index.py --write`; lint `todo_index_is_current`).

**Cardinal rule:** any deferred work — a discovered bug, a remaining sub-task, a blocked feature — must be filed as a `todo/<id>.md` item before moving on. Nothing falls through the cracks.

- **Adding work:** one new `todo/<id>.md`, then regenerate the index. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** `git rm` the item file and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Closure IS removal — never a `status` field; git keeps the item's whole life.
- **Before overwriting your plan:** check if there are incomplete items from the previous plan and file them.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, file it and move on. Never silently work around a bug — either fix it or record it.
- **Every filed reproducible bug/gap ships a DURABLE `known_gaps` repro**, never a `/tmp` scratch file. Commit a minimal reproducer to `tests/fixtures/known_gaps/` and wire an `#[ignore]`d test asserting the **CORRECT/intended** output (or an ASan/`security_safe_no_leak` fixture for a leak/UAF), cited from the item's `repro`. Keep the exact shape (leaks need a *heap-forced* value, not a literal); it graduates to a live regression fixture the same round the bug is fixed. This is the one exception to "scouts/briefs are `/tmp`-only" — the triage *paperwork* is `/tmp`, the *repro* is committed. ⊕ **A repro CITED by an item is that item's EVIDENCE, not a second filing**; an UNCITED gap fixture still counts on its own, and non-reproducible items (design notes, refactors, perf without a repro) are naturally exempt.
- **GREP `todo/` BEFORE YOU FILE** — the symptom AND the mechanism. When the defect joins a family, state what DISCRIMINATES it (panic site, lane, axis cell) and name the WHOLE family.
- **Never delete `TODO.md` or bulk-delete `todo/`** — only move completed items out.
- **The handover stores invariants and commands, not numbers.** Record *what to run to get the current number* and what it means, not the number itself.
- **Commit autonomously when green.** Once `cargo test --lib` and the round's relevant integration tests pass, commit without asking — this waiver overrides the harness default of "commit only when the user asks". It covers `git commit` only: still ask before push / force-push / `reset --hard` / `branch -D` / `rm -rf` / amend / rebasing onto a shared branch / opening or closing PRs. Never commit red or skipped.
- **Stale-pending scan.** Move completed items to `DONE.md` every session and stale-scan pending ones — verify the cited bug/stub still exists. Keep items short and scannable, and keep the pending set small.

## Multi-agent orchestration

When you launch sub-agents via the `Agent` tool, the following rules are **non-negotiable**:

0. **THE ORCHESTRATOR DOES NOT TOUCH THE CODE — IT LAUNCHES TRACKS THAT DO.** Its job:
   - **Verify the streak.**
   - **Update the brief.** Per finding: INCORPORATE into the track's scope BY DEFAULT — the TRACK fixes what its own passes discover — and FILE only when genuinely disjoint. Fold verbatim; keep the precedence stack straight.
   - **Coordinate parallel tracks** so they don't collide.
   - **CRITICISE the final form — and hand the criticism to a fresh agent.** The cross-check yields a FINDING RETURNED TO THE AGENT, never an edit the orchestrator makes.

   ⊕ **The orchestrator's OWN hands (owner 2026-08-29): fix a typo, a stale figure or a one-line correction in place** proactively or at the owner's request — **never on a TRACK's behalf**: inside a track's lifecycle the TRACK fixes what its passes find, through its executor. Proposing the fix is the REVIEW AGENT's job; implementing it is the EXECUTOR's. *"Fix inline unless really disjoint"* binds the **ROUND**, not the orchestrator's hands. The parent still drives the integration battery (rule 4).

0b. **Orchestrator is branch-agnostic.** Stay in the launch worktree — that *is* the session integration branch. Never hardcode a branch name and never check a track branch out there. Subagents always get their own worktree; parent integrates back.

1. **Always pass `isolation: "worktree"`.** No exceptions; applies to NESTED forks too — an agent loose in the main worktree sweeps the parent's uncommitted work into limbo. **Every track runs on its own worktree** — its EXECUTOR's, the branch the output-review reads and the parent integrates. Other agents on the track get throwaway worktrees and ship no diff.

2. **Brief the agent to verify its worktree on entry.** Open every agent prompt with:
   > **Run `pwd` and `git rev-parse --show-toplevel` FIRST** and confirm both point inside your worktree. NEVER touch the main checkout or the orchestrator worktree — every file op, `cargo`, and `git` command runs in your worktree path. Do NOT `cd` into either. Do NOT use absolute paths into main or the orchestrator worktree (worktrees nest UNDER main, so those write into MAIN — see rule 7). If `pwd` is main or the orchestrator worktree, STOP and report it. (Concrete paths live in the session handover.)

3. **Stage explicitly by file name.** Brief every agent: `git add <specific files>` only — NEVER `git add -a`, `git add .`, or `git commit -a`. A sweeping stage clobbers other agents' uncommitted work.

4. **Parent drives the integration sweep, not agents.** Agents run `cargo build` + `--lib` + targeted tests only; the 15-20 min full sweep is the parent's job.

5. **Brief file zones when running agents in parallel — disjointness is cheap insurance, NOT a hard requirement.** Tell each agent which files the others are touching. Do NOT defer or reshape a worthwhile parallel track to avoid overlap. When two tracks must touch one file, brief EACH on the other's exact edit regions so the diffs stay mergeable. Scout the overlap first so integration is planned, not discovered.

6. **Clean up scratch and worktrees once integrated or abandoned** — they do NOT dispose of themselves. Closing step of every round: **`scripts/round_cleanup.sh`** (dry-runs by default, `--yes` applies). Read the dry-run first. ⚠ **It prunes EVERY `agent-*` worktree**: right at round CLOSE, WRONG mid-round — an unmerged deliverable or a live agent needs an explicit keep-list. ⚠ **Also sweep `/tmp`** — stale `gg build` scratch and prior-round cargo targets dwarf the worktrees. Not "later" — "later" is when the disk is already full.

7. **Worktree-RELATIVE paths only — agent worktrees nest UNDER main.** They live at `<main-checkout>/.claude/worktrees/agent-*`, *inside* the main checkout, so an unqualified absolute path writes into MAIN. Brief every agent: all file ops use paths RELATIVE to its worktree; on an Edit-tool desync, re-Read and retry the Edit tool and never fall back to a shell heredoc with an absolute path; after any non-Edit-tool write, run `git -C <main-checkout> status` and STOP if it shows changes. Worktree isolation is necessary but NOT sufficient when the worktrees are children of the thing they must not touch. (The concrete main-checkout path for the current environment is in the session handover.)

8. **NEVER `git stash` in agents — the stash stack is repo-GLOBAL across all worktrees.** Brief every agent: save/restore state with `git add <new files>` + `git diff HEAD > /tmp/<name>.patch` + `git apply` — a plain `git diff` LOSES untracked files.

9. **Checkpoint scout prototypes to /tmp EARLY; run final gates FOREGROUND.** Agents are killable at any moment. Brief agents to checkpoint to `/tmp/recover_*.patch` after every meaningful step and to run FINAL validation gates as foreground commands with generous timeouts.

## Review with a fresh agent — the gauntlet

A **fresh** agent must review any non-trivial artifact before it's acted on, folding each pass's findings, until a fresh pass raises no reservations. ⚠ THE GAUNTLET VERIFIES WORK; IT DOES NOT DEFER IT — fresh passes exist so one pass cannot quietly break something, NOT as a queue for handing fixes to the next agent or the next round; the round that surfaced it still owes the fix. Use a *new* agent each pass — a reused one anchors on its prior conclusions. Brief every reviewer to verify each load-bearing claim against source with `file:line` and return SIGN OFF or cited reservations; cross-check them yourself — a reviewer can be wrong too.

⚠ **THE GAUNTLET SIGNS OFF THE DESIGN; ITS OBJECT IS TO REACH THE EXECUTOR.** Each pass asks whether the design makes sense and serves the project's objectives; the executor still owns the solution, reviewed in turn by the post-execution pass. ⚠ **SCOPE MAKES IT TERMINATE**: a finding resets the streak ONLY when it invalidates the DESIGN — wrong root cause, wrong layer, a Core invariant fought. **A finding that is merely more WORK inside a sound design — another sibling site, a nearby bug, a typo — GROWS THE TRACK'S SCOPE, ships to the executor in the brief, and does NOT reset the streak.** Never file such a finding as a `todo/` item for a later round, and never let it delay the launch. A TREE defect found while reviewing, and an ORCHESTRATOR fold/guard defect, are not the track's. Reviewers still hunt freely, and disposition belongs to Multi-agent rule 0. Terminal-pass minors fold as MARKED ERRATA, never woven into the body; the executor treats errata as spec.

**The reviewer's checklist is DESIGN-SOUNDNESS, not just premise-accuracy: a brief or diff that violates a Core invariant is a blocking reservation *even when the code works and every premise checks out*.** Brief every reviewer to test the artifact's DESIGN against the Core invariants + Layering discipline and raise any violation as a cited reservation. "Correct and premise-accurate" is NOT a SIGN OFF if the design fights an invariant — the reviewer names the invariant and the reference-grade shape instead.

**Scout before you brief.** This tree's most expensive mistakes were briefs built on stale premises. Before writing a brief — and before committing to any non-trivial plan — run a scout: a read-only probe/audit (often a delegated `Explore` agent) that verifies every load-bearing premise against CURRENT source with `file:line`, confirms the bug still reproduces, and where a yield is claimed prototypes it end-to-end and MEASURES the real result — in a throwaway worktree, shipping no diff. Killing an unsound plan after a one-agent scout is a win. Scout yield estimates MUST be end-to-end-verified — compile AND run AND diff whole output, never source-read.

**Ground the scout's design in the docs, not just the code.** Every scout brief MUST tell the agent to consult the relevant documentation FIRST — `docs/language-design.md`, `docs/book/`, `docs/devbook/`, `docs/internals/` — and base the design on it, citing the sections it rests on. The code shows what IS; the docs show what's INTENDED, and a code-only design reproduces whatever fossil is there. ⚠ EXCEPT `docs/language-reference.md`, written AFTER the implementation, a reference-vs-code conflict is an OPEN QUESTION, not doc-wins, and a load-bearing one is an OWNER ASK.

**The passes are SEQUENTIAL, not parallel**; a blocking pass always gets a confirming fresh pass after the fold. ≥3 passes is the FLOOR; there is NO upper bound on passes that keep finding DESIGN defects — consecutive blocking passes are the gauntlet CONVERGING, not failing. **Launch the executor as soon as a fresh pass signs off the DESIGN.**

**Convergence gate — the READINESS CHECKLIST.** A track is ready when the brief satisfies all FIVE, each binary and checkable without judgement: (1) every measurement carries a FIRE COUNT proving the mechanism executed; (2) every enumeration cites an INDEPENDENT witness (rustc exhaustiveness, a repo lint table — never the enumerator's own list); (3) `|pinned cells| == |changed cells|`; (4) the GUARD FAILS when the fix is reverted; (5) every load-bearing figure REGENERATED at current HEAD. ⚠ **The FIVE are CAPPED — a new class RETIRES a row or becomes a guard (Core #6), never a sixth.**

**FOLD VERBATIM, NEVER SUMMARISED; STACK FOLDS AS PRECEDENCE-ORDERED ADDENDA.** A summarised fold introduces errors of its own — that is what this rule was bought with, and it is why no reviewer's summary of a finding replaces the finding. Each fold generation is its own marked addendum with an explicit precedence line (later > earlier > body); never rewrite the body silently. ⚠ **This binds the ORCHESTRATOR'S OWN directives too** — an addendum may DECIDE (scope, choice, retraction), never RESTATE; a restatement compresses, and OUTRANKS the verbatim text it compressed. Not narrowing ⇒ cite *operative text: pass-N §X, unchanged*; overriding PART ⇒ NAME the part; errata are RESTATED, never pointed at. After each fold re-read the enclosing SECTION and grep the correction.

Rationale + D45: devbook/30 §12; examples: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#scout-before-you-brief-review-in-sequential-fresh-passes).

**The SIX QUESTIONS no runbook generates.** (a)-(e) of Core #15 mechanise procedure, not taste; ask these of every brief and every "defect" before acting on it:

1. **Is this asymmetry a DEFECT, or two positions with different RATIFIED semantics?** Check the design record before calling an accept/reject asymmetry a bug.
2. **Can this guard catch its OWN class?** A guard that green-lights the class it was written to retire is worse than none.
3. **Is this enumeration TOTAL, or a selection?** A selection cannot show you what it omits.
4. **Does this rule's SUBJECT actually cover the case** — or is there a case with no subject at all, which no widening of the rule fixes?
5. **Am I reasoning about emission, or emission ORDER?** When a thing happens relative to its siblings is only visible in the IR.
6. **Is this passing case ACCIDENTALLY correct?** A green cell may be green for a reason unrelated to what you think it tests.

⚠ Plus one about the record itself: *is this premise still TRUE, or a filed fact that decayed?* A considered decision in a scratch file outranks a stale one in the ledger, and the fix is to **file it properly**, not to discount it. The signal that the process has thinned: reviews finding only compression errors, never a design defect.

**One track, one agent, clean context — NO pack reviews.** A reused context anchors on its own prior conclusions and divides attention across the tracks sharing it. Forbidden: a single "pack" reviewer reading N track briefs (or N executor diffs) in one conversation and signing them off together. Required: per track, ≥3 sequential fresh brief-review agents each seeing *only that track's brief* (N tracks ⇒ N×≥3 agents); one executor per track (worktree); one fresh output-review per track's diff before it integrates. Pass *k* may run in parallel *across* tracks; within* a track passes stay sequential. Parallelism is *across tracks*, not *across roles for the same track*.

**Model allocation (harness-agnostic).** EVERY agent — scout, executor, every review pass — runs the STRONGEST available model. A rationing harness keeps it LAST at: (a) the FIRST review pass on a fresh artifact — first contact catches the structural defects while folding is cheapest; (b) the FINAL pre-integration output-review, for consequence and model diversity; (c) ad-hoc arbitration when two agents disagree. Mandate quality still dominates model strength.

This applies to four kinds of artifact:

1. **Plans / TODO items** — review before you start implementing.
2. **Agent briefs (≥3 fresh passes)** — a brief is a spec; review it *before launching*. A wrong brief wastes the whole execute + validate cycle.
3. **Agent output** — when the executor finishes, a fresh agent reviews its diff/commits *before you integrate or run expensive validation. Three gates: the breadcrumb-check — no completed-status entries (`LANDED`/`FIXED`/`RESOLVED`/`DONE`/`SHIPPED`/`✅`) added to `todo/`; those are completed work to MOVE to `DONE.md` or follow-ups to REPHRASE as the work that remains, since `todo/` holds pending work only. the **fixture-coverage gate** (Core #11/#12) — SIGN OFF requires wide, genuinely-exercising, RED-verified regression nets, not a single thin pin if siblings exist. the **reference-grade gate** (Core #8) — the acceptance bar is *correct/principled*, and a KNOWN DEFECT left in place is a reservation even when it reproduces identically in Rust gg — "both backends agree on the wrong answer" / "benign because both are UB" is the exact phrasing that must trip the gate. The orchestrator must not accept it either: pushing the defect to a 'benign, filed' follow-up is the same failure.
4. **Session-handover / state snapshots** — a stale one misleads the next session exactly as a wrong brief misleads an executor. A fresh agent verifies every load-bearing claim against ACTUAL state — commit hashes resolve, scores re-confirmed from the `*_comparison` tests, durable artifacts present at cited paths.

A multi-track round is N independent per-track loops (scout → brief → ≥3 reviews → executor → output-review → integrate), parallel across tracks, never one pack loop. You hold the full context, brief each reviewer/executor with only the artifact they own, and keep them honest.

**Scouts, briefs, and review checkpoints** — scout reports, executor briefs, census reports, review notes — are `/tmp`-only — never `git add` them. Durable content goes to its official home (`docs/language-design.md` / the define-gorget ledger / book / devbook); `todo/` items are written **self-contained**, findings inline, never "see the scout file". **The single session-state doc is `TODO.md`'s handover block.** Round close `git rm`s any scout/brief that slipped into the repo, guarded by the shrink-only allowlist lint `docs_plans_removed_and_define_gorget_is_ledger_only`; moving durable content out and deleting a completed plan is itself a reviewed change.

**Fold/patch scripts MUST assert their replace targets matched.** A stale target silently dropped wastes the entire pass, so every fold asserts the old text was found and the new text landed (a `must_replace` helper), then greps a distinctive fragment of the new text — or just use the Edit tool, which errors on no-match. `str.replace` silently no-ops.

## Round lifecycle

The delegated-task pipeline (→ Review) is the atom; a **round** is the unit the orchestrator works in. By default rounds run back-to-back, autonomously, until the owner stops them.

1. **Open a round around a headline theme — parallel tracks welcome, and a round normally carries more than one.** Pick the next headline from `TODO.md`'s handover block to give the round its identity and its `DONE.md` record. Multiple items/tracks may run IN PARALLEL within the round — disjoint file zones, per Multi-agent orchestration rule 5. "One campaign" is about the round's *theme*, NOT a limit on concurrency. The one thing to avoid is PRE-WARMING a FUTURE round's campaign: the round boundary is a landing gate.

   **Convergence lens:** expected NET items closed is one selection axis among several — bias class-fix (Core #4) and bulk-graduation over instance-fixes-with-follow-ups. It informs SELECTION; it no longer gates CLOSING.
2. **Run the delegated pipeline** (→ Review), opening with a scout (verify premises + measure end-to-end); the passes stay sequential. A semantic change lands on every lane the same round (Core #9), each with its exercising fixture (Core #11).
3. **Commit as the chains land** (→ Task Continuity, "Commit autonomously when green").
4. **Round-close gate — the FULL local battery**, matching CI's target set, with the round's commits on the integration branch:
   - **C sweep.** `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log` — **both** knobs; omitting the second false-reds `lowerer_comparison` on a loaded box. Use the wrapper, never a hand-rolled thread count — it autoscales on free cores AND RAM.
   - **Then the LLVM sweep, SEQUENTIALLY, never in parallel** (they thrash the toolchain, and `self_host_*` share fixed `/tmp` scratch), plus the bootstrap / parity-split gates the change touched.
   - **AND the separate `cargo` targets `--test integration` never touches:** `-p ggdef`, `--test spec_conformance` (3-lane C/LLVM/SH adjudication), `--test security` (ASan), `--test lints`, `--lib`.
   - **⊕ And `scripts/sanitize_sweep.sh`** (~25 min, ASan leak + corruption allowlists) — being in no gate list is exactly how it drifted RED unnoticed.
   - **⊕ Also run `python3 scripts/robustness_map.py`** — the beginner-code robustness map (700+ cells on five lanes: C · LLVM · self-host · ASan · ggdef). It reports the WORKS share per topic and fails on any WORKS→broken regression; `--accept` folds genuine progress into the baseline as a reviewed change. It measures what the main suite structurally cannot. Never edit an expectation to match what the compiler prints. Every hang/spin/timeout gets root-caused into a census row, never merely killed. Prefer a no-new-hangs executable guard (CRASH-count ratchet / shrinking `EXPECTED_HANGS`).

   The full battery covers every target CI runs, so local-green IS the round-close sign-off; autonomous rounds do NOT wait for CI. A CI-*config* failure is separate CI-hygiene, glanced at periodically, NEVER a per-round blocker. Targeted and self-host gates are necessary, not sufficient (Core #7).
5. **Records + convergence RECORD.** Add the round's `DONE.md` entry (date-stamped); update `TODO.md`'s handover block IN PLACE (pending-only, no completed breadcrumbs, invariants+commands not numbers).
   - Every DONE round entry ends with the `Convergence:` line QUOTED from `scripts/convergence.sh <prev_kg> <prev_todo> <filed>` — a MEASUREMENT, NOT A GATE: no ratio to meet, and a round never stays open on the arithmetic.
   - **What SURVIVES, as an owner ruling in its own right: FIX INLINE unless the defect is REALLY DISJOINT** — the TRACK fixes what its own passes discover, via its executor; a stream of new `todo/` items instead of a slightly larger scope is the anti-pattern.
   - ⚠ **DISJOINT MEANS A DIFFERENT *CLASS*, NOT A DIFFERENT SITE.** A sibling site in the same enumerated class — same helper, same axis, same fix shape — is Core #4's subject. Only a genuinely different class is FILED, with its durable `known_gaps` repro.
   - **A round whose commit log never touches `src/` has stopped, not discovered** — check `git log --oneline <round-open>..HEAD -- src/ | wc -l` BEFORE close.
   - **File follow-ups as `todo/` items, never into the handover.** Phased work is filed as ONE ITEM PER DECLARED PHASE.
   - **A red battery is still NEVER waivable.**
6. **Docs + hygiene.** Doc-write-through for behavior changes (→ Documentation); prune completed plans/briefs (`git rm`); capture-then-prune agent worktrees, `/tmp` scratch and any stray stash (→ Multi-agent rule 6).
7. **Open the next round autonomously.** STOP and ask the owner for exactly TWO things: (i) a genuine DESIGN decision — language semantics, a scope/sequencing trade-off, a knob such as error-vs-silent-no-op, retiring a feature; (ii) an UNRATIFIED semantics question, including any lane divergence whose correct direction is not already settled (Core #9). Both are owner CALLS, not process questions. Never stop for the discipline — gauntlet, review passes, battery and parity regen all run AUTONOMOUSLY — nor for a choice whose reference-grade answer is clear. The owner may suspend autonomy for a stretch; that is a live override of this default.
