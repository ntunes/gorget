# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file, so other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

**Who this binds.** Everything down to and including § Task Continuity binds **every agent**. The last three sections (§ Multi-agent orchestration, § Review …, § Round lifecycle) bind the **orchestrator**, plus whatever a brief passes on. Read the part that binds you, and obey the Core invariants regardless.

## Core invariants (read first)

The sections below are the spec.

**How a rule lands here — a fresh owner ruling included.** Write ONE compact imperative in the present tense (title + at most one intent sentence); its provenance, measurement and war-story go to [devbook/29](docs/devbook/29-contributor-playbook.md) (engineering) or [devbook/30](docs/devbook/30-excellence-system.md) (the excellence system); unratified owner open-thinking goes to devbook/30, marked as such. Sharpening an existing rule EDITS that rule in place — never a second dated copy beside it. **Size ceiling + heading-id inventory** hold the line (`tests/lints.rs`); clause-level probes are retired. **So the diff is the guard: after editing a rule, re-read the whole rule**, not just the clause you changed. State every excellence-system rule HERE, **never only in one harness's private memory**.

1. **Fix at the write site, not the read site.** A complex read-side fix (save/restore, phi repair, per-case rules) means a writer one layer up dropped a typed invariant. (→ Layering discipline)
2. **Typed metadata, never name-matching.** No `name.starts_with("Vector__")` to decide *meaning* — flag the typed decl, set at the source, read via an accessor. (→ Layering discipline)
3. **Register ownership at the value's birth.** Every freshly-materialized owned, droppable value is registered for drop (or provably moved) at the producer; the leak/double-free class is always a missing or mis-typed ownership tag. (→ Ownership at Consuming Positions)
4. **One fix, all siblings.** Fix the enumerated *class*, not the instance; centralize at the producer; add an arm-count lint. (→ Layering discipline)
5. **Re-verify every premise; regenerate every number.** No dated figure enters a plan/brief/commit/handover unless you regenerated it this session. ⚠ **AND THE BINARY MUST MATCH THE COMMIT** — `cargo test` REBUILDS `target/debug/gg`, so a sweep run after a test on a dirty tree measures a compiler that no longer exists; rebuild from committed source before any measurement you will quote. (→ Solution Quality)
6. **Convert a recurring bug class into an executable guard** (validator or `tests/lints.rs` ratchet: env-gate → burn down → fatal). Prose rots; guards don't — a ratchet needs BOTH directions, or a tolerance band greens every step of its own drift. ⊕ **KEEP THE EXACT `==`; SLACK IS WHAT ROTS** (owner 2026-09-08). An improvement lowers the pin by **auto-lowering write-back (`--bless`, LOWER-ONLY)** — never by hand, never by widening to `<=`. **Terminus: burn the debt to 0 and assert `== 0`** — a zero never needs editing again. ⭐ **Owner directive: burn ALL known failures to 0.** (→ `docs/devbook/25-structural-guards.md`)
7. **Gate on the bootstrap and the sanitizer**, not just a green suite — `self_host_bootstrap_fixed_point` + ASan catch what `cargo test` and the always-pass `*_comparison` diagnostics miss. (→ Build & Test)
8. **Reference-grade is the bar, not parity with a possibly-wrong reference.** "Matches Rust gg" / "both backends agree" is *necessary, not sufficient*; if the agreed-on behavior is wrong, fix BOTH compilers — most often by making the language *reject* it. "Benign because both backends are UB" is a red flag; the output-review must refuse to ship a known defect.
9. **A SEMANTIC change lands on every lane in the same round** — ggdef (in subset), Rust gg (C+LLVM), self-host — pinned by a cross-lane fixture, never a promise. Accept/reject changes ship the conformance fixture for the FINAL state; a lagging lane is red or `#[ignore]`+citation; out-of-subset shapes get a note + a filed subset gap; implementation-internal codegen is exempt. Own new fixtures must COMPILE + MATCH on self-host the SAME ROUND — raising `RUNTIME_DIFF_NONMATCH_CEILING` for your own inflow is forbidden.
10. **Lower-or-reject — never silently drop user syntax.** Every lowering arm either lowers the construct or emits a check-time rejection; a `_ =>` fall-through that discards a write is a miscompile-class defect. Enforcement: the silent-fallthrough allowlist ratchet.
11. **Every fix ships wide, genuinely-exercising regression fixtures, same round.** Exercise the bug on the *real* path — non-constant operands, wired to RUN, one per sibling for a class, wide enough that a partial regression trips them. The fixture lands WITH the fix, never "later".
12. **A regression fixture is not coverage until it has been seen to FAIL — and a fixture set that samples one value of a typed axis is an anecdote, not a net.** RED-verify every new fixture against the PRE-fix compiler (green on arrival is not coverage); a fixture's NAME is a claim about SCOPE; where behaviour depends on a typed axis, cover every value or name each omitted cell.
13. **Verify the verifier — and pick an instrument that can SEE the failure class.** Show at least one gate going RED on a deliberately broken variant; ask ggdef first for value semantics (it is STRUCTURALLY BLIND to memory-invalidation — ASan adjudicates memory); ggdef IMPLEMENTS the definition, it is not the definition. ⚠ **ANCHOR A DELIBERATE BREAK BY LINE, NEVER BY SUBSTRING** — an identically-spelled sibling site absorbs it and you conclude the guard is inert or the brief is wrong when neither is true.
14. **An invariant-asserting comment needs an enforcing guard, or it gets DELETED.** "This is unreachable", "the only consumer is X" — either a `debug_assert!`/lint/typed guard, or rot; when you touch code near such a comment, verify it or delete it — never inherit it.
15. **Make rigor MECHANICAL, not clever — the gauntlet must still work with a weaker reviewer.** (a) every load-bearing claim carries its VERIFICATION COMMAND — ⚠ **a `file:line` IS ONE: cite the GREP THAT REGENERATES IT, never a bare number. A sibling track integrating mid-gauntlet rots every line in the brief, and Core #13's *anchor the break BY LINE* is then unusable;** (b) scope over a SET → present the SET with a disposition per row; (c) fold at the defect, then re-read the whole enclosing section; (d) grep the correction in instruction form (`old`→`new`); (e) run the fixed procedures for recurring claim types without judgement.

**A round runs SEVERAL TRACKS IN PARALLEL, each on ITS OWN WORKTREE, and every track runs its own gauntlet:** scout → brief → ≥3 fresh brief-reviews → launch (worktree) → fresh output-review → integrate (→ Review), inside the Round lifecycle. **Only that track's EXECUTOR changes code** — scouts prototype and throw away, reviewers propose without implementing, and the orchestrator coordinates (Multi-agent rule 0).

## Build & Test

```bash
cargo build  # build the compiler
cargo test --lib  # unit tests
scripts/run_integration.sh  # integration tests (autoscaled)
cargo test  # all tests
```

**Pipe integration tests through `tee`** with a random filename — parallel agents collide on fixed names.

```bash
scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log
```

**LLVM backend.** Set `GG_BACKEND=llvm` to append `--backend=llvm` to every `gg build` (all-or-nothing per run). Sweeps autoscale via `scripts/run_integration.sh`, no `--test-threads=1`.

```bash
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 scripts/run_integration.sh --release 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

**Backends should be at parity**; a regression on one and not the other means the change touched a backend-specific path.

⛔ **THE WORKING TREE IS A GATE *INPUT*, NOT JUST A BUILD INPUT — DO NOT EDIT `src/`, `lib/`, `compiler/` OR DRIVER SOURCES WHILE A BOOTSTRAP OR SWEEP IS IN FLIGHT, COMMENTS INCLUDED.** `driver.gg` bakes every `src/backend/c/runtime/*.c` in via `embed_file`, so a one-word comment edit mid-run makes two generations embed different bytes and **convergence slips exactly one generation** — indistinguishable from a real ratchet breach. ⚠ *"Convergence stage is deterministic, so load cannot move it"* is TRUE about load and **FALSE as a licence**: determinism holds only if the INPUTS hold still. Re-run at the same commit before concluding anything. (Measured R50: same commit, stage-3 then stage-2.) ⊕ **AND THAT INCLUDES THE SWEEP SCRIPT ITSELF — `bash` READS A SCRIPT INCREMENTALLY, so editing a running one corrupts its own tail** (measured R51: `error reading input file`, **exit 0**, gate blocks silently absent).

**Timeouts** (override on loaded hosts): `GG_BUILD_TIMEOUT_SECS` (outer `gg build`; default 120/180; bump to 600 on multi-agent boxes), `GG_TEST_TIMEOUT_SECS` (per-test binary; default 30; bump for `stress_*` / p2p / arena). ⚠ **The BOOTSTRAP stages obey NEITHER — they read `GG_STAGE1_TIMEOUT_SECS` (default 600), and its load auto-adjust samples `/proc/loadavg` ONCE at test start, so it cannot protect a 20-min stage on a box that loads up later. Set it to 1800 on a multi-agent box; a stage timeout is NOT a regression until compared at HEAD.**

## Documentation

- `docs/define-gorget/decisions.md` — **the RATIFIED OWNER DECISIONS. OUTRANKS every doc below** — read FIRST. ⛔ **NO AGENT EDITS IT — owner ask only.**
- `docs/language-design.md` — Design philosophy, safety, rationale
- `docs/language-reference.md` — Full syntax and semantics, written AFTER the code
- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience)
- `docs/devbook/` — [Compiler Internals](docs/devbook/README.md): pipeline and design docs

**`docs/book/` and `docs/devbook/` read like a published book** — timeless present-tense design narrative faithful to INTENDED behavior, never a fix-log. No dates, commit hashes, `Snag #N` labels or perf "win" numbers in design chapters — those belong in `DONE.md` and `docs/devbook/29`–`30`. A round that changes behavior owes a doc-write-through.

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
`Owned[T]`, `Box[T]`, `Task`, `TaskGroup`, `Guard`, `Mutex`/`RWLock` (D53) —
**single-owner-by-design — no IMPLICIT-copy path in the lowering** (an explicit
`.clone()` exists for SOME members and not others; check `clone_fn`, never
assume). `E_MoveWithoutOperator`
**at bare-assign sites AND at constructor / struct / enum-init sites** + consume;
**require the user to write `^source` or `source.clone()`** (unique locks: `Shared[Mutex[T]]`, never `.clone()`). (At a plain function / method call these types are simply borrowed.)

### Reason about storage and liveness, never about what the value IS

**At an ownership boundary the question is what these two access paths do to the same storage over time — does it overlap, do their live ranges intersect, can either one write — never what type family the value belongs to.** ⚠ **A type family says what a value is CAPABLE of; it never says what THESE paths do to THIS storage.** `clone_fn` presence proves a source *can* be duplicated and says nothing about whether the boundary transfers ownership of the duplicate — **so a property of the SOURCE never settles a question about the BOUNDARY.** ⊕ Ratified carve-outs remain carve-outs: the single-owner set is a real type-family rule, and it tells you a value has no implicit-copy path — it does **not** tell you whether a given position conflicts. Full rule: [`docs/language-design.md` §3.5](docs/language-design.md) — *"Overlap is about storage, not spelling"*, *"The test is ability to write, not the sigil."*

At each consuming position (`push`, `put`, `set`, `insert`, `send`,
`v[i] = x`) the collection must own — **the POSITION is the rule; the receiver's
spelling (`fns` vs `self.routes`) is not part of it**. The compiler **picks per-arg from
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
The clone case is required, not a fallback. The decision is mechanical, not heuristic.

**This is the compiler contract — not a suggestion.** Full spec:
[`docs/devbook/11-copy-on-write.md`](docs/devbook/11-copy-on-write.md#materialization-points--the-enforced-boundary-set).

## Solution Quality

- **Prefer robust, architecturally sound solutions over quick fixes.** When the trade-off is unclear, discuss both approaches and ask before proceeding.
- **Aim for generic solutions that solve classes of problems.** Be resourceful — read code, search the web, study how other compilers solve it. Exhaust every avenue before concluding something can't be done.
- **Flag code smells and structural issues** even when unrelated to the task. Log non-trivial findings to `todo/`.
- **You are allowed an opinion.** If the user is proposing something dumb, call him out.
- **You are allowed to swear if opportune.** But if something deserves a 'holy shit', use it!
- **Performance work measures MEMORY, not just time.** Track peak RSS + alloc/clone counts (`--clones=stats`, `/usr/bin/time -v`) alongside wall-clock — a memory balloon is as blocking as a time regression.
- **Re-verify a premise against CURRENT source/tests before acting on it (Core #5)** — re-run the test, re-read the cited source, check the actual current code shape. No un-regenerated numbers: quote the *command*, not the stale value (the `*_comparison` tests are always-pass).
- **Consult history before proposing a design** or briefing a design-heavy task: grep `DONE.md`, `todo/`, `git log`, AND the Rust impl in `src/`. Don't wait to be asked. Skip only for mechanical/greenfield changes.
- ⛔ **AND BEFORE ANY OWNER ASK — THE LEDGER FIRST, THEN A PROBE.** An ask is a design proposal with the owner's time attached, so it carries the *strictest* form of the rule above: search `docs/define-gorget/decisions.md` for the question AND for its inverse, then measure the hinge. ⚠ **A claim of the form *"the ratified X forbids Y"* is not checkable by reading X — check whether Y ONCE EXISTED AND WAS DELIBERATELY REMOVED, and whether a general rule is overridden by a carve-out.** Both R50 asks dissolved on this: one was answered by a two-line probe showing a sibling position already rejects, the other by a ledger line collapsing the very distinction it wanted to add. **Bring the owner what the record cannot settle — never what you have not looked up.**

## Layering discipline

AST → GIR → LIR → backend. Full rules in [`docs/devbook/24-layering-discipline.md`](docs/devbook/24-layering-discipline.md):

1. **Lossless on invariants, lossy on syntax.** A layer may resolve abstractions and add information; it may not drop semantic invariants (ownership, drop strategy, view-vs-owned, ABI, copy semantics, borrow provenance). Invariants accumulate; abstractions evaporate.
2. **Typed metadata, not name-matched.** Facts cross as typed fields on structs — never name prefixes, sentinel values, or runtime-symbol conventions. (See "No name matching" below.)
3. **One source of truth per axis.** Exactly one piece of metadata at one location, read through one accessor. No parallel sidecar maps.
4. **Resolve once, write through.** A resolved abstraction writes into the next layer's typed metadata. Downstream doesn't redo the work and doesn't get to disagree.

**Litmus test:** if a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong. The fix is always upstream. Cite the doc in PRs that touch IR layer boundaries.

### No name matching (rule 2 at the runtime-symbol boundary)

**Do not pattern-match on function names, type names, runtime-symbol prefixes**, or any identifier string to make a semantic decision. Writing `matches!(name, "gorget_str_trim" | ...)` or `if name.starts_with("Vector__")` to decide what something *means* — stop. Put the flag on the typed declaration and read it via an accessor; if the metadata does not exist yet, **add it**. Exception: at the C-emit boundary the runtime symbol *is* the contract — drive the spelling from a typed registry, never `if name == "..."`.

### Debugging heuristic — fix complexity as a signal of wrong layer

When the fix you're sketching is *intrinsically complex* — save/restore around branches, phi insertion at merges, scope-tracking name maps, manual SSA repair — stop: you're patching a *symptom*.

1. **Trace the data the buggy site is reading.** *Where was it last written?*
2. Look at the writer. *Did it respect all the typed metadata available?* Or did it default / hardcode / collapse cases the upstream had distinguished?
3. **Writer was lossy → fix at the source**; the downstream "complex fix" evaporates.
4. **Writer was faithful → trace one more layer up.** Repeat.

Worked examples: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#the-debugging-heuristic-fix-complexity-is-a-signal-of-the-wrong-layer).

### Sibling-site drift — fix the class, not the instance

Fixing a bug at one position in an *enumerated set* — consume positions (`push`/`put`/`set`/`insert`/`send`/ctor/return/capture), tail-value dispatchers, container-literal arms, registration paths — fix the **class**, not the instance:

1. **Grep for the siblings before you commit.**
2. **Prefer centralizing at the producer** over patching each consumer (e.g. `maybe_auto_propagate` hoisted to the `lower_expr` exit).
3. **Add an arm-count lint** (like `container_literal_arms_count`) so the next sibling is forced through the shared path.

**Litmus test:** if the fix is "add the missing call to site N", ask how many sites there are and what stops site N+1. [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#sibling-site-drift-fix-the-class-not-the-instance).

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the expected output must reflect what the language *should* do.

**Forbidden: reshaping the surrounding code** to avoid the gap. The wired-in expected output is the load-bearing artifact, not the comment. [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#dont-redesign-around-compiler-gaps).

**Litmus test:** a more-complex-than-necessary fixture, or a workaround comment citing a bug, is likely a dodged gap. This rule outranks Task Continuity's "Never silently work around a bug" — recording the bug is not enough.

## Self-host as the elegance showcase

The self-host frontend (`tests/fixtures/self_host_*/`) is the language's reference-grade demonstration. It must be written in **idiomatic Gorget**, not the way it had to be written to dodge an old compiler bug — stress test, regression net, and showcase; the third role is non-negotiable.

**The succession plan.** The self-host REPLACES Rust gg as the primary reference at ~100% runtime parity: file a "reference lags the self-host" finding, fix the Rust side as oracle hygiene, never dumb the self-host down to match. ggdef adjudication is the truth axis.

Fossils: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#self-host-as-the-elegance-showcase--and-retiring-fossils).

Rules:
1. **No defensive code without a live, cited bug.** Finding a workaround comment ("parallel because…", "wrapper to avoid…"), verify the bug still exists; if it doesn't, delete the workaround and use the idiomatic shape.
2. **Self-host code reads like the user manual.** If you wouldn't recommend the pattern in `docs/book/`, don't write it in self-host.
3. **When you fix a compiler gap, also retire the workarounds.** Search for the workaround pattern across all self-host directories before declaring the fix shipped.
4. **Periodically audit.** Treat the self-host as a living document and prune. The `*_comparison` and `bootstrap_fixed_point` tests will catch regressions.

## Task Continuity

**Work items live ONE PER FILE in `todo/`**: TOML front matter, `+++`, prose verbatim — spec in `scripts/todo_index.py`. A field the item's text does not state stays EMPTY. `TODO.md` keeps the handover, the invariants, and a GENERATED index (`scripts/todo_index.py --write`; lint `todo_index_is_current`).

**Cardinal rule:** any deferred work — a discovered bug, a remaining sub-task, a blocked feature — must be filed as a `todo/<id>.md` item before moving on. Nothing falls through the cracks.

- **Adding work:** one new `todo/<id>.md`, then regenerate the index. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** `git rm` the item file and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Closure IS removal — never a `status` field; git keeps the item's whole life.
- **Before overwriting your plan:** check if there are incomplete items from the previous plan and file them.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, file it and move on. Never silently work around a bug — either fix it or record it.
- **Every filed reproducible bug/gap ships a DURABLE `known_gaps` repro.** Commit a minimal reproducer to `tests/fixtures/known_gaps/` and wire an `#[ignore]`d test asserting the **CORRECT/intended** output, cited from the item's `repro`. Keep the exact shape (leaks need a *heap-forced* value). A cited repro is that item's evidence, not a second filing; non-reproducible items are exempt.
- **GREP `todo/` BEFORE YOU FILE** — the symptom AND the mechanism AND **the CITED SOURCE FILES**. When the defect joins a family, state what DISCRIMINATES it (panic site, lane, axis cell) and name the WHOLE family. ⚠ **A DUPLICATE BORN OF ONE SOURCE REPORT IS INVISIBLE TO A SYMPTOM GREP** — both filings use that report's own words, so only the `cites` overlap shows it. Measured: two filings of one probe's finding, four hours apart, by the same author.
- **Never delete `TODO.md` or bulk-delete `todo/`** — only move completed items out.
- **The handover stores invariants and commands, not numbers.** Record *what to run to get the current number* and what it means, not the number itself.
- **Commit autonomously when green.** Once `cargo test --lib` and the round's relevant integration tests pass, commit without asking. Covers `git commit` only: still ask before push / force-push / `reset --hard` / `branch -D` / `rm -rf` / amend / rebase onto a shared branch / opening or closing PRs. Never commit red or skipped.
- **Stale-pending scan.** Move completed items to `DONE.md` every session and stale-scan pending ones — verify the cited bug/stub still exists. Keep items short and scannable, and keep the pending set small.

## Multi-agent orchestration

When you launch sub-agents via the `Agent` tool, the following rules are **non-negotiable**:

0. **THE ORCHESTRATOR DOES NOT TOUCH THE CODE — IT LAUNCHES TRACKS THAT DO.** Verify the streak; write and update the brief (it is the orchestrator's deliverable — incorporate findings into the track's scope by default, file only when genuinely disjoint); coordinate parallel tracks; criticise the final form and hand the criticism to a fresh agent (a FINDING RETURNED TO THE AGENT, never an orchestrator edit).

   ⊕ **The orchestrator's OWN hands:** fix a typo, a stale figure or a one-line correction in place, never on a TRACK's behalf. Inside a track the TRACK fixes what its passes find, through its executor. *"Fix inline unless really disjoint"* binds the **ROUND**, not the orchestrator's hands.

0b. **Orchestrator is branch-agnostic. WORK ALWAYS LANDS ON THE WORKTREE CLAUDE WAS INVOKED FROM** (owner 2026-09-03) — that *is* the session integration branch. Never hardcode a branch name, never check a track branch out there, and never commit to `main`: `main` moving is the OWNER's sync, not a place to land. Subagents always get their own worktree; parent integrates back. ⚠ **A handover bullet contradicting a rule in this file is STALE BY CONSTRUCTION — this file wins.**

1. **Always pass `isolation: "worktree"`.** No exceptions; applies to NESTED forks too. **Every track runs on its own worktree** — its EXECUTOR's. Other agents on the track get throwaway worktrees and ship no diff.

2. **Brief the agent to verify its worktree on entry.** Open every agent prompt with:
   > **Run `pwd` and `git rev-parse --show-toplevel` FIRST** and confirm both point inside your worktree. NEVER touch the main checkout or the orchestrator worktree — every file op, `cargo`, and `git` command runs in your worktree path. Do NOT `cd` into either. Do NOT use absolute paths into main or the orchestrator worktree (worktrees nest UNDER main, so those write into MAIN — see rule 7). If `pwd` is main or the orchestrator worktree, STOP and report it. (Concrete paths live in the session handover.)

3. **Stage explicitly by file name.** Brief every agent: `git add <specific files>` only — NEVER `git add -a`, `git add .`, or `git commit -a`. A sweeping stage clobbers other agents' uncommitted work. ⛔ **AND `TODO.md`'s HANDOVER BLOCK IS NEVER A TRACK'S TO WRITE** — a worktree is frozen at spawn, so a track's snapshot of it reverts every handover write since. The GENERATED INDEX in the same file *is* the track's to regenerate (`scripts/todo_index.py --write`, lint `todo_index_is_current`). **Brief every agent which files are NOT its own** — naming the base commit tells it what it HAS, not what it may TOUCH.

3b. **THE ORCHESTRATOR ALLOCATES `todo/` IDs; a track never picks its own.** Issue each executor a private, disjoint ID BLOCK in its brief and extend it on request — concurrent tracks otherwise collide on the same next-free id (owner 2026-09-03). ⛔ **AND THE ALLOCATOR NEEDS AN ALLOCATION TOO — THIS RULE'S SUBJECT DID NOT COVER ITS OWN AUTHOR.** ⇒ **the orchestrator files from a block it has ISSUED TO ITSELF.** A collided block is RETIRED WHOLE and re-issued; reasoning about which half is still safe costs more than the ids.

4. **Parent drives the integration sweep, not agents.** Agents run `cargo build` + `--lib` + targeted tests only; the 15-20 min full sweep is the parent's job.

5. **Brief file zones when running agents in parallel — disjointness is cheap insurance, NOT a hard requirement.** Tell each agent which files the others are touching; do not defer a worthwhile parallel track to avoid overlap. When two tracks must touch one file, brief EACH on the other's exact edit regions. Scout the overlap first. ⚠ **BUT FILE-ZONE DISJOINTNESS IS ABOUT EDIT COLLISION AND SAYS NOTHING ABOUT INVARIANT COUPLING** — two halves in non-adjacent regions still share who OWNS an allocation. **A diff of two PROGRAMS cannot show whether two PATCHES compose.** ⇒ **before cutting a split, APPLY EACH HALF ALONE TO PRISTINE HEAD AND RUN THE OTHER HALF'S FIXTURES** plus the committed suite; one build each. ⊕ **AND A SHARED COUNTER IS INVISIBLE TO THAT TEST TOO** — a corpus pin both halves bump is CORRECT in each half alone and RED together, because each half measured base+itself and the other half's fixture does not exist in its tree. ⇒ **the INTEGRATING PARENT RE-MEASURES every corpus pin, floor and census row from the MERGED tree and sets it from the measured output — never by adding one** (Core #5), and a fixture-adding brief says the track's bump is PROVISIONAL.

6. **Prune a track's worktrees the moment it INTEGRATES — not before, not at round close.** ⛔ **NOT BEFORE: an output-review returns FINDINGS, and the executor needs its worktree to fix them** (measured: pruned at executor-done, recreated one heartbeat later cold). They do not dispose of themselves; prune only when CLEAN (`git status --porcelain`); `scripts/round_cleanup.sh` dry-run first (a live agent needs a keep-list); sweep `/tmp`.

7. **Worktree-RELATIVE paths only — agent worktrees nest UNDER main.** An unqualified absolute path writes into MAIN. Brief every agent: paths RELATIVE to its worktree; on an Edit-tool desync, re-Read and retry (never a shell heredoc with an absolute path); after any non-Edit-tool write, `git -C <main-checkout> status` and STOP if it shows changes.

8. **NEVER `git stash` in agents — the stash stack is repo-GLOBAL across all worktrees.** ⛔ **AND NEVER `git checkout <ref> -- <path>` TO SET UP A REVERT — IT SILENTLY CLOBBERS UNCOMMITTED WORK UNDER THAT PATH — same family as the stash prohibition, a whole-subtree overwrite with no prompt.** ⇒ **RED-verify with a PATCH you can re-apply** (`git diff HEAD > /tmp/<agent-id>.patch`, break, measure, `git apply`), never with a checkout. Brief every agent: save/restore state with `git add <new files>` + `git diff HEAD > /tmp/<name>.patch` + `git apply` — a plain `git diff` LOSES untracked files. ⛔ **AND A WHOLE-FILE SNAPSHOT PRESCRIBED AS A FIX IS THE SAME FAMILY: IT SILENTLY REVERTS WHATEVER A SIBLING TRACK LANDED IN THAT FILE, WITH NO CONFLICT.** ⇒ **prescribe DIFFS, never a copy — but `patch` DEFAULTS TO FUZZ AND SUCCEEDS ON DRIFT: measured, a reworded context line applies with `fuzz 1` at rc 0, the only signal a word in stdout. ⛔ `patch -p0 -F0` IS THE PRESCRIPTION** (it then rc 1s with rejects). Re-derive against the executor's ACTUAL HEAD, and order the track after the sibling that shares the file.

9. **Checkpoint scout prototypes to /tmp EARLY; run final gates FOREGROUND.** Agents are killable at any moment. Brief agents to checkpoint to `/tmp/recover_*.patch` after every meaningful step and to run FINAL validation gates as foreground commands with generous timeouts. ⛔ **AND THE PRUNE TEST IS *"DOES A LIVE CLAIM REST ON THIS"*, NOT *"DOES A LIVE FILE NAME IT"***. ⚠ **NAMESPACE EVERY `/tmp` ARTIFACT BY AGENT** (`recover_<agentid>_*`) — `/tmp` is shared across all agents on the box, and two agents on one track will pick the same mnemonic prefix; a clobber there is SILENT. ⊕ **THE SAME HAZARD EXISTS IN PROCESS SPACE: `pgrep -f <shared script name>` IS A FIXED `/tmp` FILENAME BY ANOTHER NAME.** A watchdog spinning on `until ! pgrep -f 'sweep.sh'` matches EVERY agent's run, so two agents each hold the other forever. **A STATUS PROBE HAS THE IDENTICAL FLAW, and the shell running the probe embeds the string too — so `pgrep` FALSE-POSITIVES on your own waiter and FALSE-NEGATIVES once you filter `eval` out. ⇒ ASK THE ARTIFACT, NOT THE PROCESS TABLE: a log's line-count GROWTH or its mtime cannot be impersonated by another process.** Reap orphaned wait-loops at round close.

## Review with a fresh agent — the gauntlet

A **fresh** agent must review any non-trivial artifact before it's acted on, folding each pass's findings, until a fresh pass raises no reservations. ⚠ THE GAUNTLET VERIFIES WORK; IT DOES NOT DEFER IT — the round that surfaced a finding still owes the fix. Use a *new* agent each pass; brief every reviewer to verify each load-bearing claim against source with `file:line` and return SIGN OFF or cited reservations.

⚠ **THE GAUNTLET SIGNS OFF THE DESIGN; ITS OBJECT IS TO REACH THE EXECUTOR.** A finding resets the streak ONLY when it invalidates the DESIGN — wrong root cause, wrong layer, a Core invariant fought. More WORK inside a sound design GROWS THE TRACK'S SCOPE, ships to the executor in the brief, and does NOT reset the streak. Terminal-pass minors fold as MARKED ERRATA.

**The reviewer's checklist is DESIGN-SOUNDNESS, not just premise-accuracy: a brief or diff that violates a Core invariant is a blocking reservation *even when the code works and every premise checks out*.** "Correct and premise-accurate" is NOT a SIGN OFF if the design fights an invariant — name the invariant and the reference-grade shape instead.

**Scout before you brief.** Run a read-only probe that verifies every load-bearing premise against CURRENT source with `file:line`, confirms the bug still reproduces, and where a yield is claimed prototypes it end-to-end and MEASURES the real result — in a throwaway worktree, shipping no diff. Yield estimates MUST be compile AND run AND diff whole output, never source-read.

**Ground the scout's design in the docs, not just the code.** Every scout brief MUST tell the agent to consult FIRST — **`docs/define-gorget/decisions.md` (RATIFIED, outranks the rest)**, `docs/language-design.md`, `docs/book/`, `docs/devbook/`, `docs/internals/` — and cite what it rests on. ⚠ EXCEPT `docs/language-reference.md`, written AFTER the implementation, a reference-vs-code conflict is an OPEN QUESTION, not doc-wins.

**The passes are SEQUENTIAL, not parallel**; a blocking pass always gets a confirming fresh pass after the fold. ≥3 passes is the FLOOR; there is NO upper bound on passes that keep finding DESIGN defects. **Launch the executor as soon as a fresh pass signs off the DESIGN.** ⚠ **A track that cannot get its design signed off is REBUILT or SPLIT, never reviewed harder.** ⊕ **SPLIT also when the scope genuinely grows too much** — each half becomes its own track in the SAME round; a split is division, never deferral.

**Convergence gate — the READINESS CHECKLIST.** A track is ready when the brief satisfies all FIVE, each binary and checkable without judgement: (1) every measurement carries a FIRE COUNT proving the mechanism executed; (2) every enumeration cites an INDEPENDENT witness (rustc exhaustiveness, a repo lint table — never the enumerator's own list); ⚠ **AND A FILTER OVER A HAND-BUILT CORPUS IS THE ENUMERATOR'S OWN LIST IN A MECHANICAL COSTUME** — a guard's fire-delta only ranks cells someone already wrote, so it fails in BOTH directions: measured, it SELECTED an unchanged cell (accidentally correct) and MISSED a changed one; (3) `|pinned cells| == |changed cells|`; (4) the GUARD FAILS when the fix is reverted — **and a fixture set is complete only when EVERY PARTIAL REVERT turns a row RED: enumerate the reverts, name the row that pins each**; (5) every load-bearing figure REGENERATED at current HEAD. ⚠ **The FIVE are CAPPED — a new class RETIRES a row or becomes a guard (Core #6), never a sixth.**

**FOLD VERBATIM, NEVER SUMMARISED; STACK FOLDS AS PRECEDENCE-ORDERED ADDENDA.** Each fold generation is its own marked addendum with an explicit precedence line (later > earlier > body); never rewrite the body silently. ⚠ **This binds the ORCHESTRATOR'S OWN directives too** — an addendum may DECIDE (scope, choice, retraction), never RESTATE. After each fold re-read the enclosing SECTION and grep the correction. ⛔ **A fold that prescribes a WRITE SITE or ships CODE cites the reviewer's MEASURED PROTOTYPE (`/tmp/recover_*.patch`), never a retyped snippet** — folding an ILLUSTRATION as a PRESCRIPTION is how three R49 folds shipped defects, incl. a placement that fixed 0 of 6 and a call whose wrong arity resolved to the silent-wrong sibling. ⛔ **A FOLD MAY ONLY ASSERT WHAT A COMMAND IN THAT SAME FOLD REGENERATES** — a figure quoted from a scout is NOT exempt, and the instrument must be able to SEE the class it is asked about. ⛔ **AND AN ADDENDUM THAT REPLACES A *DESIGN* MUST STRIKE ITS ARTIFACT POINTERS **BY NAME** — PRECEDENCE ORDERS *CLAIMS*, IT DOES NOT RETRACT *CITATIONS*.** ⊕ **A RETRACTION QUOTES THE RETRACTED CLAIM'S OWN SCOPE**: retract wider and you delete a true claim; retract narrower and the false one still stands.

**The SIX QUESTIONS no runbook generates.** Ask these of every brief and every "defect" before acting on it:

1. **Is this asymmetry a DEFECT, or two positions with different RATIFIED semantics?** Check the design record before calling an accept/reject asymmetry a bug.
2. **Can this guard catch its OWN class?** A guard that green-lights the class it was written to retire is worse than none. ⛔ **AND THE SILENCING EDIT IS OFTEN INSIDE YOUR OWN BRIEF: a guard whose SUBJECT is the DECLARATION goes legitimately GREEN over a value that is still wrong.** ⇒ **APPLY EVERY OTHER IN-SCOPE EDIT, THEN RE-CHECK THE GUARD IS STILL RED.** A `=fatal` landing that survives that is real; one that does not was fatal over a live defect.
3. **Is this enumeration TOTAL, or a selection?** A selection cannot show you what it omits.
4. **Does this rule's SUBJECT actually cover the case** — or is there a case with no subject at all, which no widening of the rule fixes?
5. **Am I reasoning about emission, or emission ORDER?** When a thing happens relative to its siblings is only visible in the IR.
6. **Is this passing case ACCIDENTALLY correct?** A green cell may be green for a reason unrelated to what you think it tests.

⚠ Plus one about the record itself: *is this premise still TRUE, or a filed fact that decayed?* File it properly; do not discount a considered scratch decision because the ledger is stale.

**One track, one agent, clean context — NO pack reviews.** Per track: ≥3 sequential fresh brief-review agents each seeing *only that track's brief*; one executor; one fresh output-review of the diff. Parallelism is *across tracks*, not *across roles for the same track*. A multi-track round is N independent per-track loops, never one pack loop.

**Model allocation (harness-agnostic).** EVERY agent runs the STRONGEST available model. A rationing harness keeps it LAST at: (a) the FIRST review pass on a fresh artifact; (b) the FINAL pre-integration output-review; (c) ad-hoc arbitration when two agents disagree.

This applies to four kinds of artifact:

1. **Plans / TODO items** — review before you start implementing.
2. **Agent briefs (≥3 fresh passes)** — a brief is a spec; review it *before launching*.
3. **Agent output** — a fresh agent reviews the diff *before you integrate*. Three gates: no completed-status entries (`LANDED`/`FIXED`/`RESOLVED`/`DONE`/`SHIPPED`/`✅`) in `todo/`; the **fixture-coverage gate** (Core #11/#12); the **reference-grade gate** (Core #8) — "both backends agree on the wrong answer" / "benign because both are UB" must trip it.
4. **Session-handover / state snapshots** — a stale one misleads the next session exactly as a wrong brief misleads an executor. Verify every load-bearing claim against ACTUAL state.

**Scouts, briefs, and review checkpoints** are `/tmp`-only — never `git add` them. Durable content goes to its official home; `todo/` items are written **self-contained**. **The single session-state doc is `TODO.md`'s handover block.** Round close `git rm`s any scout/brief that slipped into the repo. ⚠ **A NAMED OMISSION IS DURABLE CONTENT** — *"not measured"*, *"scoped enumeration"*, *"lane not run"* noted only in a brief dies WITH the brief at round close, and *nothing recorded what was never run* is how a family gets declared closed. File it, or land it in the item it qualifies.

**Fold/patch scripts MUST assert their replace targets matched.** `str.replace` silently no-ops; use a `must_replace` helper or the Edit tool.

## Round lifecycle

The delegated-task pipeline (→ Review) is the atom; a **round** is the unit the orchestrator works in. By default rounds run back-to-back, autonomously, until the owner stops them.

1. **Open a round around a headline theme — parallel tracks welcome, and a round normally carries more than one.** Pick the next headline from `TODO.md`'s handover block. "One campaign" is about the round's *theme*, NOT a limit on concurrency; the thing to avoid is PRE-WARMING a FUTURE round's campaign. Bias class-fix (Core #4) and bulk-graduation over instance-fixes-with-follow-ups.

2. **Run the delegated pipeline** (→ Review), opening with a scout (verify premises + measure end-to-end). A semantic change lands on every lane the same round (Core #9), each with its exercising fixture (Core #11).
3. **Commit as the chains land** (→ Task Continuity, "Commit autonomously when green").
4. **Round-close gate — the FULL local battery**, matching CI's target set, with the round's commits on the integration branch:
   - **C sweep.** `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log` — **both** knobs. Use the wrapper, never a hand-rolled thread count.
   - **Then the LLVM sweep, SEQUENTIALLY, never in parallel** — `GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 scripts/run_integration.sh --release` — plus the bootstrap / parity-split gates the change touched.
   - **AND the separate `cargo` targets `--test integration` never touches:** `-p ggdef`, `--test spec_conformance`, `--test security`, `--test lints`, `--test c_runtime`, `--lib`.
   - **⊕ And the LLVM-lane security run, its OWN CI step** — neither sweep reaches it: `GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 cargo test --test security --release -- --test-threads=1`.
   - **⊕ And the three SCRIPT gates no `cargo` target reaches:** `scripts/known_gaps_census.sh --check`, `GG_STAGING_MOVE_GUARD=fatal scripts/staging_move_burndown.sh --check` and `GG_BOX_RECEIVER_GUARD=count scripts/box_receiver_burndown.sh --check`. ⚠ **Read every rc off the BARE command** — `script | tail` reports the PIPE's status, which has greened a red gate three times in one session.
   - **⊕ And `scripts/sanitize_sweep.sh`** (~25 min, ASan leak + corruption allowlists).
   - **⊕ Also run `python3 scripts/robustness_map.py --lanes all`** — five lanes (C · LLVM · self-host · ASan · ggdef), which is what CI gates on; fails on any WORKS→broken regression. ⚠ **The BARE command is `--lanes c` (the `--lanes` default) — one lane, not five**; it was written here as if it were five for two rounds. Never edit an expectation to match what the compiler prints.

   **This list is RECONCILED AGAINST `.github/workflows/ci.yml` BY A LINT** (`round_close_battery_covers_ci_steps`), because the sentence that used to stand here — *"the full battery covers every target CI runs"* — was measured FALSE: three CI steps were absent, so a round could run the whole documented battery green while CI was red, and did. Local-green IS the round-close sign-off only for as long as that lint holds it true. A CI-*config* failure is NEVER a per-round blocker. Targeted and self-host gates are necessary, not sufficient (Core #7).
5. **Records + convergence RECORD.** Add the round's `DONE.md` entry; update `TODO.md`'s handover block IN PLACE (pending-only, invariants+commands not numbers). Every DONE round entry ends with the `Convergence:` line AND the **DEBT LEDGER** table QUOTED from `scripts/convergence.sh` — a MEASUREMENT, NOT A GATE, so **no debt total ever blocks a close**. The ledger shows every axis's ACTUAL count (a label that names a bigger thing than it counts is worse than no number) plus its three movements: **DISCOVERED (+found) · FIXED (−fixed) · CARRIED**. Every figure is DERIVED, never pinned; the baseline is AUTO-WRITTEN (`scripts/convergence.sh --bless`) and **never hand-edited**. ⭐ **DISCOVERED ≠ INTRODUCED.** A failure your round EXPOSED — better instrument, honest work — becomes KNOWN: fix it this round, SPLITTING the track if that is what it takes, else file it. **A failure your round INTRODUCED is fixed BEFORE close; introducing known issues is not allowed** — this is Core #9's *"raising `RUNTIME_DIFF_NONMATCH_CEILING` for your own inflow is forbidden"*, generalised from one axis to all of them. NAME every wrong-way row in the round entry with its verdict (`--ledger-diff <axis>` names the members). **FIX INLINE unless the defect is REALLY DISJOINT** — DISJOINT MEANS A DIFFERENT *CLASS*, NOT A DIFFERENT SITE. A round whose commit log never touches `src/` has stopped, not discovered. File follow-ups as `todo/` items, never into the handover. **A red battery is still NEVER waivable.**
6. **Docs + hygiene.** Doc-write-through for behavior changes (→ Documentation); prune completed plans/briefs (`git rm`); capture-then-prune worktrees, `/tmp` and any stray stash (→ Multi-agent rule 6).
7. **Open the next round autonomously.** STOP and ask the owner for exactly TWO things: (i) a genuine DESIGN decision; (ii) an UNRATIFIED semantics question, including any lane divergence whose correct direction is not already settled (Core #9). Never stop for the discipline. The owner may suspend autonomy for a stretch; that is a live override of this default.
