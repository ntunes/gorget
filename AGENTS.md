# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

## Build & Test

```bash
cargo build                                          # build the compiler
cargo test --lib                                     # unit tests (~1027)
cargo test --test integration -- --test-threads=4    # integration tests (~1069, ~3 min)
cargo test                                           # all tests
```

**Always pipe integration tests through `tee`** with a random filename — these can be long, save output so you don't have to re-run to find which test failed, and parallel agents collide on fixed names:

```bash
cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-$RANDOM.log
```

**LLVM backend.** Set `GG_BACKEND=llvm` to append `--backend=llvm` to every `gg build` (all-or-nothing per run; see `tests/integration.rs:29-48`). Use `--test-threads=1` for full sweeps — the parallel runner hits cargo-level rebuild races (~28 min sequential vs ~13 min parallel for C). Single-test runs with the default `--test-threads=4` are fine.

```bash
GG_BACKEND=llvm cargo test --test integration --release -- --test-threads=1 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

Backends should be at parity; a regression on one but not the other usually means the change touched a backend-specific path rather than shared LIR.

**Timeouts** (override on loaded hosts): `GG_BUILD_TIMEOUT_SECS` (outer `gg build`; default 120/180; bump to 600 on multi-agent boxes for DEBUG self-host builds), `GG_TEST_TIMEOUT_SECS` (per-test binary; default 30; bump for `stress_*` / p2p / gorget-arena).

## Documentation

- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience, not Gorget experience)
- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale
- `docs/devbook/` — [Compiler Internals Book](docs/devbook/README.md): contributor-facing pipeline and design docs

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, borrow checking
- `src/ir/` — Intermediate representation and lowering from AST (monomorphization, drop insertion, closures)
- `src/lir/` + `src/backend/c_lir/` — SSA-based LIR; `src/bir/` — BIR lowers canonical ops before backend emit
- `src/backend/llvm/` — LLVM IR backend (`--backend=llvm`)
- `src/backend/c/` — C runtime library and SQLite amalgamation
- `src/formatter/` — Source formatter (`gg fmt`)
- `src/sim/` — Interpreter / simulation runtime
- `src/loader.rs`, `src/lockfile.rs`, `src/manifest.rs` — Package management
- `src/report.rs` — Test report generation
- `tests/fixtures/*.gg` — Integration test programs with deterministic stdout
- `tests/integration.rs` — Integration test harness: builds fixtures via `cargo run -- build`, executes, asserts stdout

## Language Syntax (Quick Reference)

- Indentation-based blocks (Python-style), type-first declarations: `int x = 5`, `String name = "hello"`
- Functions: `int add(int a, int b): return a + b` / expression-body: `int double(int x): x * 2`
- Closures: `(int x): x * 2` / function types: `int(int, int)` (return type first)
- Match uses `case`: `match x: case 1: ... else: ...`
- Enum variants are qualified: `Color.Red()` not `Red()` (prelude variants `Ok`, `Error`, `Some`, `None` stay bare)
- `meta` keyword for compile-time evaluation — see `docs/language-reference.md` for full builtin list
- Mutable borrow (`&`) and move (`!`) sigils go immediately before the argument name, not before the type:
  `void modify(Message &msg)` ✓ — `void modify(&Message msg)` ✗
  `void consume(Message !msg)` ✓ — `void consume(!Message msg)` ✗

**Always use type-first Gorget syntax** in code, plans, and examples: `int x = 5`, `String greet(String name)`. The only string type is `String` — `str` is not a keyword.

## Ownership at Consuming Positions (push/put/set/insert/send)

CoW's default everywhere is **borrow** — bare-identifier assignments
(`Spanned b = a`), regular function call args, match scrutinees,
collection reads all propagate Ptr aliases at zero cost. Clones happen
only at ownership boundaries, where the destination must own
(collection puts, returns, struct/enum field init, closure captures).
Even there, the compiler prefers move when liveness allows it.

The carve-outs to CoW-default-borrow on bare-assign are: closures /
`Callable[T]`, `Owned[T]`, `Box[T]`, `Task`, `TaskGroup`, `Guard`.
These are single-owner-by-design — the safety pass still emits
`MoveWithoutOperator` (E_MoveWithoutOperator) at bare-assign sites for
these, forcing the user to write `!source` or `source.clone()`.

At each consuming position (`push`, `put`, `set`, `insert`, `send`,
`v[i] = x`) the collection must own. The compiler picks per-arg from
typed ownership state (Phase D's `LocalOwnership`):

| Source                                            | Action                |
|---------------------------------------------------|-----------------------|
| Owns AND dead at this call                        | move after call       |
| Borrow, OR owned but live past this call          | clone before call     |
| Static literal                                    | runtime *_materialize |

The three move-eligible shapes are: `!arg` (user opt-in), expression
temp (last-use + owning by construction), and named local at last use,
bound to an owned value (not from `.get()`, a view-returning method,
or a parameter — those bind borrows).

On a valid move, the source slot becomes logically dead. The IR
instruction is `MoveZero`; the backend zeros the source only when
drop-tracking would otherwise re-drop the value, and elides the
zero when liveness proves it unobservable. The zero is a backend
optimization for drop correctness, not part of the move semantics.

The clone case is required, not a fallback: the source either doesn't
own its data or must stay valid past the call; move would be a
use-after-free. The decision is mechanical, not heuristic.

**This is the compiler contract — not a suggestion.** Post-call
zeroing (when emitted) is correct only for the move-eligible shapes.
See [`docs/devbook/11-copy-on-write.md`](docs/devbook/11-copy-on-write.md#materialization-points--the-six-vs-seven-finding)
for the full specification.

## Solution Quality

- Prefer robust, architecturally sound solutions over quick fixes. When the trade-off is unclear, discuss both approaches and ask before proceeding.
- Aim for generic solutions that solve classes of problems, not just the immediate symptom. Be resourceful — read code, search the web, study how other compilers solve the same problem. Exhaust every avenue before concluding something can't be done.
- Flag code smells and structural issues you encounter, even if unrelated to the current task. Log non-trivial findings to `TODO.md`.
- You are allowed an opinion. If the user is proposing something dumb, call him out.
- You are allowed to swear if opportune. Don't over do it, but if something deserves a 'holy shit', use it!
- **Performance work measures MEMORY, not just time.** Every perf investigation/fix tracks peak RSS + alloc/clone counts (`--clone-stats` build flag → the `[clone-stats] array_clone=N` line, `/usr/bin/time -v`, `scripts/self_host_mem_baseline.sh`) alongside wall-clock — a memory balloon is as blocking as a time regression. Worked example: a `self_host_stage` process ballooning to ~4GB RSS (a 1.5-billion-array_clone get-mutate-set clone-bomb) went undiagnosed because perf chains only timed ms; the long compile times were a *symptom* of the memory thrash, not the disease.
- **Re-verify a premise against CURRENT source/tests before acting on it.** Diagnoses, plans, comparison scores, and dated TODO/memory notes go stale — confirm the load-bearing fact still holds (re-run the `*_comparison` test for a score; re-read the cited source for a "bug"; check the actual current code shape, not a remembered one). This codebase has repeatedly burned cycles on stale premises: a "resolver at 57%" that was actually 96%, an "unshipped f-string port" already shipped, a "live function-type bug" already fixed, a "cleanup target" whose fossils were already retired, a fix-brief that misread a workaround's *current* state as its *proposed* state. Don't trust dated figures or an agent's unverified conclusion (an agent claimed "multi-agent load" when it was the only agent running, and "Rust codegen bug" without a repro); cross-check first.

## Layering discipline

How information crosses IR layer boundaries (AST → GIR → LIR → backend). Full rules in [`docs/devbook/24-layering-discipline.md`](docs/devbook/24-layering-discipline.md); four-line summary:

1. **Lossless on invariants, lossy on syntax.** Each layer may resolve abstractions (generics, methods, traits) and add information (control flow, SSA). It may not drop semantic invariants (ownership, drop strategy, view-vs-owned, ABI, copy semantics, borrow provenance). Invariants accumulate; abstractions evaporate.
2. **Typed metadata, not name-matched.** Facts cross boundaries as typed fields on structs — never as name prefixes, sentinel values, or runtime-symbol conventions. (See "No name matching" below.)
3. **One source of truth per axis.** For each kind of information, exactly one piece of metadata at exactly one location, read through one accessor. No parallel sidecar maps.
4. **Resolve once, write through.** When a pass resolves an abstraction, the result writes into the next layer's typed metadata. Downstream doesn't redo the work and doesn't get to disagree.

**Litmus test:** if a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong. The fix is always upstream — add the field, write it at the source, read it at the consumer. Cite the doc in PRs that touch IR layer boundaries.

### No name matching (rule 2 at the runtime-symbol boundary)

Do not pattern-match on function names, type names, runtime-symbol prefixes, or any other identifier string to make a semantic decision. If you're writing `matches!(name, "gorget_str_trim" | ...)` or `if name.starts_with("Vector__")` to decide what something *means* — stop. The metadata you need is missing one layer up.

Symptoms: parallel lists in different files kept in sync by hand; new methods silently misbehaving because a name list wasn't updated; `// keep both lists in sync` comments; lowering/backend decisions spelled as substring tests on identifiers.

The fix: put the semantic flag on the typed declaration (`BuiltinMethodDecl.returns_view`, `Inst::CallRuntime` sidecar, etc.), set once at the source, propagated as typed fields, read via typed accessors. If the metadata genuinely doesn't exist yet, **add it** rather than fishing for the answer in a name.

Exception: at the C-emit boundary you have to spell the runtime symbol (the name *is* the contract with the runtime). Even there, drive the spelling from a typed registry — never make a routing decision based on `if name == "..."`.

### Debugging heuristic — fix complexity as a signal of wrong layer

When you've localized a bug and the fix you're sketching is *intrinsically complex* — save/restore around branches, phi insertion at merges, scope-tracking name maps, manual SSA repair — stop. That complexity is almost always a tell that you're patching a *symptom*. Real bugs in well-layered compilers are usually one-line oversights at a **write** site, not multi-case rules at the **read** site.

1. Trace the data the buggy site is reading. *Where was it last written?*
2. Look at the writer. *Did it respect all the typed metadata available?* Or did it default / hardcode / collapse cases the upstream had distinguished?
3. Writer was lossy → fix at the source; the downstream "complex fix" evaporates.
4. Writer was faithful → trace one more layer up. Repeat.

Every layer hop without finding the bug should make you *more* suspicious of your diagnosis, not less.

Worked examples:
- **Snag #17** (chained `text.substring(...)` corrupting later `parse_float(text)`): symptom looked like `cow_materialize_alias` rebinding across CF merges (50+ line fix candidates). Real bug: `resolve_builtin_method_return_type` ignored the protocol's `self_conv` flag, triggering bogus materialization. 5-line fix at the writer; rebind path now never-taken.
- **Snag #13** (Box-recursive enum links to undefined `__gorget_box_alloc_<T>`): tempting fix was scanning recursive-drop tables for `Box__X__drop` — name-matching. Real bug: `StructDef` for `Box[T]` had typed inner-type info at registration but didn't expose it to the C backend. Fix: add `box_inner_type: Option<String>`, set at registration, read at emit.

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the fixture's expected output must reflect what the language *should* do, not what it currently does.

Forbidden: reshaping the surrounding code (tests, fixtures, examples, even production code) to avoid the gap. Even when commented, this buries the bug. The wired-in expected output (or the surviving workaround idiom) becomes the load-bearing artifact, and "passing" tests lock in buggy behavior as canonical.

Worked examples from this codebase:
- **Tier E §8.1 drop-flag**: agent dodged a universal `!`-param drop-at-exit leak by rewriting the canonical fixture to use locals instead of `!` params. Bug stayed hidden a day; three masked-leak tests needed expected-output updates once it was fixed.
- **`Dict.len()`**: workaround `scores.keys().len()` was documented in a fixture comment for ~8 weeks past the silent fix. The redesign outlived its justification.
- **Phase A `collection_runtime_type`**: stale TODO that had already been resolved as a side-effect of foundation commits — refusing to manufacture migration work to fit it is itself an instance of this rule.

**Litmus test:** if a fixture uses a more complex shape than seems necessary, OR a workaround comment cites a bug, ask why. Patterns like "uses locals instead of `!` params" or "passes an extra explicit arg the language should default" are smells — likely a gap was dodged. Verify the bug still exists before treating the workaround as canonical.

Stronger than "never silently work around a bug" — the workaround need not be silent to harm. Commented redesigns harm too, because the wired-in expected output is the load-bearing artifact, not the comment.

## Self-host as the elegance showcase

The self-host frontend (`tests/fixtures/self_host_*/`) is the language's reference-grade demonstration. It must be written in **idiomatic Gorget** — the way the language is meant to look when it's working — not the way it had to be written to dodge a compiler bug six months ago. The self-host serves three roles simultaneously: a stress test for the compiler, a regression net (via `*_comparison` and `bootstrap_fixed_point` tests), AND a showcase for the language. The third role is non-negotiable.

Defensive code accumulated for past compiler gaps is **technical debt with a stale justification.** The bug was fixed; the workaround stayed; the comment explaining "why the parallel-vector / extra clone / wrapper function" became a false historical record. New contributors read the workaround as canonical style, copy it, and the rot spreads.

Examples already burned into the codebase:
- `StructRegistry`: parallel `Vector[String]` + `Vector[int]` with O(n) linear scan in `lir_lower.gg`, kept "because callers iterate in insertion order at emission time" — a workaround for a Dict-ordering bug fixed 2026-05-08.
- `type_info_keys_safe`: a wrapper function around `Dict.keys()` whose entire purpose is to dodge a state-loss bug that no longer exists.
- Comments containing `# parallel storage to dodge Dict[String, _] state-loss` scattered across `lower.gg`.

Rules:
1. **No defensive code without a live, cited bug.** If you find a workaround comment ("parallel because…", "wrapper to avoid…", "rebuild instead of mutate…"), verify the bug still exists. If it doesn't, delete the workaround and use the idiomatic shape.
2. **Self-host code reads like the user manual.** If you wouldn't recommend this pattern in `docs/book/`, don't write it in self-host.
3. **When you fix a compiler gap, also retire the workarounds.** A fix is incomplete until the dodge it enabled in self-host is gone. Search for the workaround pattern across all self-host directories before declaring the fix shipped.
4. **Periodically audit.** Compiler gaps that get fixed leave fossils. Treat the self-host as a living document and prune. The `*_comparison` and `bootstrap_fixed_point` tests will catch regressions.

This rule pairs with "Don't redesign around compiler gaps" — that one is about not creating new dodges; this one is about retiring old ones.

## Multi-agent orchestration

When you launch sub-agents via the `Agent` tool in this project, the following rules are **non-negotiable** — past sessions have repeatedly lost work because they were treated as suggestions:

1. **Always pass `isolation: "worktree"`.** No exceptions. Omitting it means the agent runs in the main worktree and any `git stash` / `git reset --hard` / `git commit -a` it performs sweeps the parent conversation's uncommitted work into limbo. Even if past sessions observed worktree enforcement as "advisory" (the agent ignored it and ran in `/workspace/...` anyway), the explicit flag is still the first line of defense. Never skip it.

2. **Brief the agent to verify its worktree on entry.** Open every agent prompt with:
   > Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree. NEVER touch `/workspace/gorget-1` directly — every file operation, `cargo` command, and `git` command runs in your worktree path. Do NOT `cd` into `/workspace/gorget-1`. Do NOT use absolute paths starting with `/workspace/gorget-1/...`. If your `pwd` reports `/workspace/gorget-1`, STOP and report it back.

3. **Stage explicitly by file name.** Brief every agent: `git add <specific files>` only — NEVER `git add -a`, `git add .`, or `git commit -a`. Other agents (and the parent) may have uncommitted work in the tree; a sweeping stage clobbers it.

4. **Parent drives the integration sweep, not agents.** Brief every agent to run `cargo build` + `cargo test --lib` + targeted integration tests only. The 15-20 minute full `cargo test --test integration` is the parent's job; agents that try to wait for it stall and may be terminated mid-commit.

5. **Brief disjoint file zones when running agents in parallel.** Tell each agent which files the other agents are touching. Even with worktree isolation, telling the agent to stay away from a specific area is cheap insurance.

The failure mode when these rules slip is recoverable but ugly: working trees get contaminated, stashes accumulate mixed ownership, edits disappear into `stash@{N}` entries the parent can't easily attribute. The cost of fixing it after the fact is far higher than the cost of doing it right at launch.

## Review plans, TODO items, AND agent briefs/outputs with a fresh agent

A **fresh** agent must review any non-trivial artifact before it's acted on, iterating — folding each pass's findings — until a fresh agent raises **no reservations**. Use a *new* agent each pass: a reused one anchors on its prior conclusions, while a fresh one re-derives from the code and catches what the artifact baked in (more than once the second pass found the *first pass's own recommendation* was unsound). Brief every reviewer to verify each load-bearing claim against source with `file:line` and return either SIGN OFF or specific cited reservations — not to rubber-stamp, and not to invent reservations to avoid signing off. The reviewer may build or run minimal checks to verify.

**The passes are SEQUENTIAL, not parallel.** Each pass reviews the artifact *after* the previous pass's findings have been folded in — pass 2 sees the corrected v2, pass 3 sees v3, and so on. Do **not** fan out N reviewers concurrently against the same version: that gives you N opinions on v1, misses defects introduced *by* a correction, and forfeits the point — a fresh agent re-deriving against the *latest* artifact (which may include another reviewer's now-folded suggestion that itself turns out unsound). The loop is strictly: review → fold → fresh review of the corrected artifact → fold → … → a fresh pass raises no reservations. "≥3 fresh passes" means ≥3 such sequential rounds, not 3 simultaneous reads.

**Never launch after a pass that raised reservations — the NEXT pass, on the corrected artifact, must itself come back clean.** A fold can leave a stale remnant *or introduce a new defect*, and only a fresh pass catches it. Worked examples from this codebase: a brief whose reservation-1 fold updated the "typed shape" section but left the "consumer rewrites" section describing the *pre-fold* design — a direct contradiction caught only on the next pass; and a plan that needed **four** passes because passes 2 and 3 each surfaced a fresh stale-remnant of the same class. Do **not** rationalize skipping the confirming pass with "the fix was mechanical" or "the design is source-verified" — the fold itself is exactly what the next pass exists to verify. If a pass is *blocking* (not just nit-level), you are not done; fold and run another.

This applies to four kinds of artifact:

1. **Plans / TODO items** — review before you start implementing.
2. **Agent briefs (≥3 fresh passes)** — a brief you hand a delegated `Agent` is a spec; review it *before launching* the agent. A wrong brief wastes the whole execution + validation cycle, so a cheap fresh-agent pass is the bargain — and these passes routinely catch a mis-identified root cause, a fix aimed at the wrong layer, or a "fix" that's already implemented.
3. **Agent output** — when the execution agent finishes, a fresh agent reviews its diff/commits *before you integrate or run expensive validation*, to confirm correctness and catch regressions.
4. **Session-handover / state snapshots** — the in-flight-state doc a fresh session resumes from (the `TODO.md` handover block, persisted resumption artifacts, the `MEMORY.md` north-star/scores) is a *spec the next session executes from*; a stale, wrong, or incomplete one misleads it exactly as a wrong brief misleads an execution agent — and it's the highest-leverage artifact to get right, since every downstream action inherits its errors. Before relying on it for a handover, a fresh agent verifies every load-bearing claim against ACTUAL state: commit hashes resolve, worktree progress is as described, comparison scores re-confirmed from the `*_comparison` tests (not quoted from memory), durable artifacts (patches/plans) are present at the cited paths, and nothing is stale, contradictory, or missing. This is the same trap as "re-verify a premise" — a handover written from memory/agent-reports is dated the moment it's written.

So a delegated task runs: **write brief → ≥3 fresh brief-reviews (until no reservations) → launch agent (worktree, per "Multi-agent orchestration") → fresh review of its output → integrate.** A session handover runs the same loop on the state snapshot before the baton is passed. You (the orchestrator) hold the full context and brief the reviewers with it — and cross-check their load-bearing claims against source, keeping the reviewers honest too. Skipping the brief-review or the output-review is how wrong diagnoses and unreviewed regressions slip through.

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and add them to TODO.md.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
