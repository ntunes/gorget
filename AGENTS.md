# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

## Core invariants (read first)

The sections below are the spec; these are the load-bearing rules they reduce to. **New lessons go here as a one-line rule; the war-story goes in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md) — that split is how this file stays lean.** **Significant excellence-system rules (process, gates, gauntlet shapes, model allocation) live in THIS file — never only in one harness's private memory — so any external agent harness can replicate the system. (Owner 2026-07-18.)**

1. **Fix at the write site, not the read site.** A complex read-side fix (save/restore, phi repair, per-case rules) means a writer one layer up dropped a typed invariant. (→ Layering discipline)
2. **Typed metadata, never name-matching.** No `name.starts_with("Vector__")` to decide *meaning* — put the flag on the typed decl, set at the source, read via an accessor. (→ Layering discipline)
3. **Register ownership at the value's birth.** Every freshly-materialized owned, droppable value is registered for drop (or provably moved) at the producer; the leak/double-free class is always a missing or mis-typed ownership tag. (→ Ownership at Consuming Positions)
4. **One fix, all siblings.** Fix the enumerated *class* (every consume/dispatch site), not the instance; centralize at the producer; add an arm-count lint. (→ Layering discipline)
5. **Re-verify every premise; regenerate every number.** No dated figure enters a plan/brief/commit/handover unless you regenerated it this session. (→ Solution Quality)
6. **Convert a recurring bug class into an executable guard** (validator or `tests/lints.rs` ratchet: env-gate → burn down → fatal). Prose rots; guards don't. When review passes or successive rounds keep finding ONE class in new syntactic costumes, the round's output owes the class-retiring guard — not just the instance fixes (owner 2026-07-18; the 2G loop/branch/comprehension materialize family is the type case). (→ `docs/devbook/25-structural-guards.md`)
7. **Gate on the bootstrap and the sanitizer**, not just a green suite — `self_host_bootstrap_fixed_point` + ASan catch what `cargo test` and the always-pass `*_comparison` diagnostics miss. (→ Build & Test)
8. **Reference-grade is the bar, not parity with a possibly-wrong reference.** "Matches Rust gg" / "both backends agree" / "only fails on programs that are UB on both" is *necessary, not sufficient*. If the agreed-on behavior is itself wrong (garbage, crash, silent miscompile, or a program that *should* be rejected but isn't), that is **≥2 bugs to fix in BOTH compilers** — most often by making the language *reject* the program (a typecheck error + a negative fixture). A "benign because both backends are UB" review verdict is a **red flag, never a pass**; the final output-review must refuse to ship a known defect. (→ Review … fresh agent; pairs with "rust is not sacrosanct" + "Don't redesign around compiler gaps")
9. **A SEMANTIC change lands on every lane in the same round — ggdef (within its subset), Rust gg (C+LLVM), and the self-host — pinned by a cross-lane fixture, never by a promise.** Anything that alters accept/reject or what accepted programs do ships with the conformance fixture (or per-lane driver tests) encoding the intended FINAL state; a lagging lane is a red lane or an explicit `#[ignore]`+citation — never a silent gap; out-of-ggdef-subset shapes get an explicit note + a filed subset gap. Implementation-internal fixes (one backend's codegen) are exempt: lanes share semantics, not implementation. The round does not close with an undocumented lane divergence. (Owner 2026-07-16; unifies docs-write-through, the Batch-A ggdef-lane lesson, and the A2-S port pattern — 9 of the 15 findings in the 2026-07-16 xhigh review were single-lane landings drifting.)
10. **Lower-or-reject — never silently drop user syntax.** Every lowering arm either lowers the construct or emits a check-time rejection; a `_ =>` fall-through (or missing arm) that discards a write or expression the user wrote is a miscompile-class defect (`xs.0 = v` silently discarding the assignment, found live 2026-07-18), not a "not yet supported" comment. Enforcement: the `tests/lints.rs` silent-fallthrough allowlist ratchet (env-gate → burn down → fatal). (Owner 2026-07-18.)
11. **Every fix ships a genuinely-exercising regression fixture, same round.** A bug fix isn't done until a fixture exercises the bug on the *real* path — non-constant operands (so const-fold can't elide it), wired to RUN (a `run_gg` snapshot / integration fixture, not just compile), one per sibling for a class. The fixture lands WITH the fix, never "later". (Distinct from #6: #6 is the executable guard that retires a recurring *class*; this is the baseline per-fix regression net.)

Delegated work runs **scout → brief → ≥3 fresh brief-reviews → launch (worktree) → fresh output-review → integrate** (→ Review … with a fresh agent), inside the **Round lifecycle** (see that section for how a round opens, closes, and chains).

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

**LLVM backend.** Set `GG_BACKEND=llvm` to append `--backend=llvm` to every `gg build` (all-or-nothing per run; the backend dispatch is `gg_backend`/`gg_command`, `tests/integration.rs:52-103`). Full sweeps run fine at `--test-threads=4` (measured 2026-06-15: 1289/0 in ~5 min vs ~18 min sequential). The former `--test-threads=1` requirement was stale: it predated the harness switching to invoke the pre-built `gg` binary directly via `CARGO_BIN_EXE_gg` (`tests/integration.rs:84`), which eliminated the `cargo run` build-lock contention that used to race under parallelism.

```bash
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release -- --test-threads=4 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

Backends should be at parity; a regression on one but not the other usually means the change touched a backend-specific path rather than shared LIR.

**Timeouts** (override on loaded hosts): `GG_BUILD_TIMEOUT_SECS` (outer `gg build`; default 120/180; bump to 600 on multi-agent boxes for DEBUG self-host builds), `GG_TEST_TIMEOUT_SECS` (per-test binary; default 30; bump for `stress_*` / p2p / gorget-arena).

## Documentation

- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience, not Gorget experience)
- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale
- `docs/devbook/` — [Compiler Internals Book](docs/devbook/README.md): contributor-facing pipeline and design docs

**`docs/book/` and `docs/devbook/` read like a published book — timeless present-tense design narrative faithful to INTENDED behavior, never a fix-log.** No dates, commit hashes, `Snag #N`/`Root #N`/`Fix C` labels, or parity/perf "win" numbers in the design chapters — those belong in `DONE.md` and the contributor playbook (`docs/devbook/29`). A round that changes behavior owes a doc-write-through (Core #9 spans docs too); book-ifying a chapter that has rotted into changelog style is its own recurring DOC track, reviewed like any work (→ Round lifecycle).

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

## Ownership at Consuming Positions (push/put/set/insert/send, constructors, returns, captures)

CoW's default everywhere is **borrow** — bare-identifier assignments
(`Spanned b = a`), regular function call args, match scrutinees,
collection reads all propagate Ptr aliases at zero cost. Clones happen
only at ownership boundaries, where the destination must own
(collection puts, **constructor / struct / enum field init** like
`S(name)` / `Some(name)`, returns, closure captures). The rule is
**uniform across all of them** — there is no push-vs-constructor split:
clone-if-the-source-is-live, move-if-it-is-dead. Even at the boundary,
the compiler prefers move when liveness allows it.

The carve-outs to CoW-default-borrow are: closures / `Callable[T]`,
`Owned[T]`, `Box[T]`, `Task`, `TaskGroup`, `Guard`. These are
single-owner-by-design (no clone path in the lowering) — the safety pass
emits `MoveWithoutOperator` (E_MoveWithoutOperator) for these at
bare-assign sites AND at constructor / struct / enum-init sites, forcing
the user to write `!source` or `source.clone()`. (At a plain function /
method call these types are simply borrowed, so no operator is needed.)

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

On a valid move the source slot becomes logically dead; the IR instruction is
`MoveZero` (the backend zeros the source only when drop-tracking would otherwise
re-drop it, eliding the zero when liveness proves it unobservable — a drop-correctness
optimization, not part of the move semantics). The clone case is required, not a
fallback: a borrowed or still-live source would be a use-after-free if moved. The
decision is mechanical, not heuristic.

**This is the compiler contract — not a suggestion.** Full spec:
[`docs/devbook/11-copy-on-write.md`](docs/devbook/11-copy-on-write.md#materialization-points--the-enforced-boundary-set).

## Solution Quality

- Prefer robust, architecturally sound solutions over quick fixes. When the trade-off is unclear, discuss both approaches and ask before proceeding.
- Aim for generic solutions that solve classes of problems, not just the immediate symptom. Be resourceful — read code, search the web, study how other compilers solve the same problem. Exhaust every avenue before concluding something can't be done.
- Flag code smells and structural issues you encounter, even if unrelated to the current task. Log non-trivial findings to `TODO.md`.
- You are allowed an opinion. If the user is proposing something dumb, call him out.
- You are allowed to swear if opportune. Don't over do it, but if something deserves a 'holy shit', use it!
- **Performance work measures MEMORY, not just time.** Every perf investigation/fix tracks peak RSS + alloc/clone counts (`--clones=stats` build flag → the `[clone-stats] array_clone=N` line, `/usr/bin/time -v`, `scripts/self_host_mem_baseline.sh`) alongside wall-clock — a memory balloon is as blocking as a time regression (a ~4GB-RSS clone-bomb once hid behind ms-only timing; the slow compile was the *symptom*, not the disease).
- **Re-verify a premise against CURRENT source/tests before acting on it.** Diagnoses, plans, comparison scores, and dated TODO/memory notes go stale — confirm the load-bearing fact still holds (re-run the `*_comparison` test for a score; re-read the cited source for a "bug"; check the actual current code shape, not a remembered one). Don't trust dated figures or an agent's unverified conclusion; cross-check first. **No un-regenerated numbers:** a figure you did not regenerate this session does not enter a plan, brief, commit message, TODO/handover, or statement to the owner — quote the *command*, not the stale value (the `*_comparison` tests are diagnostic-always-pass, so only the freshly-printed counts mean anything). Burned-cycle incidents (resolver "57%"→96%, already-shipped ports, retired fossils) in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#re-verify-a-premise-against-current-source-before-acting).
- **Consult history before proposing a design or briefing a design-heavy task.** Before an architectural change, a diverge-vs-mirror-Rust call, a recommendation, or a design-heavy brief: grep `DONE.md`, `TODO.md`, `git log`, AND the Rust impl in `src/` (the blueprint for self-host work). Don't wait to be asked. Skip only for mechanical/greenfield changes — reinventing a rejected approach, or misframing "alignment with the existing design" as a "departure", burns real cycles.

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

Worked examples (Snag #17 — `self_conv` ignored → bogus materialization, 5-line writer fix; Snag #13 — Box inner-type dropped at the layer boundary, fixed with a typed `box_inner_type` field): [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#the-debugging-heuristic-fix-complexity-is-a-signal-of-the-wrong-layer).

### Sibling-site drift — fix the class, not the instance

When you fix a bug at one position in an *enumerated set* — consume positions (`push`/`put`/`set`/`insert`/`send`/ctor/return/capture), tail-value dispatchers, container-literal arms, registration paths — fix the **class**, not the instance:

1. **Grep for the siblings before you commit.**
2. **Prefer centralizing at the producer** over patching each consumer (e.g. `maybe_auto_propagate` hoisted to the `lower_expr` exit; `builder.set_terminator` made a no-op when already terminated — one line that killed a whole class).
3. **Add an arm-count lint** (`tests/lints.rs`, like `container_literal_arms_count`) so the next sibling is forced through the shared path — as part of the fix, not after the next regression.

**Litmus test:** if your fix is "add the missing call to site N", ask "how many sites are there, and what stops site N+1 from the same hole?" If nothing does, you fixed the instance, not the class. Sagas (auto-prop #43→#49; tail-value #8→#51) in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#sibling-site-drift-fix-the-class-not-the-instance).

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the fixture's expected output must reflect what the language *should* do, not what it currently does.

Forbidden: reshaping the surrounding code (tests, fixtures, examples, even production code) to avoid the gap. Even when commented, this buries the bug. The wired-in expected output (or the surviving workaround idiom) becomes the load-bearing artifact, and "passing" tests lock in buggy behavior as canonical.

Worked examples (Tier E `!`-param drop-flag dodged via a rewritten fixture; `Dict.len()` workaround outliving its bug ~8 weeks; Phase A stale-but-already-resolved TODO): [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#dont-redesign-around-compiler-gaps).

**Litmus test:** if a fixture uses a more complex shape than seems necessary, OR a workaround comment cites a bug, ask why. Patterns like "uses locals instead of `!` params" or "passes an extra explicit arg the language should default" are smells — likely a gap was dodged. Verify the bug still exists before treating the workaround as canonical.

Stronger than "never silently work around a bug" — the workaround need not be silent to harm. Commented redesigns harm too, because the wired-in expected output is the load-bearing artifact, not the comment.

## Self-host as the elegance showcase

The self-host frontend (`tests/fixtures/self_host_*/`) is the language's reference-grade demonstration. It must be written in **idiomatic Gorget** — the way the language is meant to look when it's working — not the way it had to be written to dodge a compiler bug six months ago. The self-host serves three roles simultaneously: a stress test for the compiler, a regression net (via `*_comparison` and `bootstrap_fixed_point` tests), AND a showcase for the language. The third role is non-negotiable.

**The succession plan (owner 2026-07-18).** The endgame: the self-host becomes so good it REPLACES Rust gg as the primary reference once runtime parity reaches ~100% across fixtures and dogfood apps. Consequences: a "reference lags the self-host" finding (the self-host correct where Rust gg is buggy) is a **succession milestone, not an embarrassment** — file it, fix the Rust side as **oracle hygiene** (Rust gg is the parity harness's measuring instrument until succession; a wrong oracle poisons measurements), and never dumb the self-host down to match. And because agreement-with-Rust loses meaning as the self-host overtakes, **ggdef adjudication is the truth axis that makes the succession decision safe** — subset expansion and the adjudicated-parity split rise in priority accordingly.

**Post-succession leaning (owner 2026-07-18, open thinking — not ratified):** KEEP the Rust implementation even after full parity, for three reasons. (1) **Triangulation** — an odd number of implementations disambiguates (the adjudication split's first reading proved it: 13 two-compiler agreements were overturned by the third opinion); caveat: Rust gg and the self-host share semantic lineage, so ggdef remains the only structurally independent member — the triad is a diversity portfolio, not three equal voters. (2) **ggdef must stay SMALL** — its authority derives from being a readable definition; grown to full coverage it just becomes another compiler, so the full-coverage third opinion has to be Rust gg. The out-of-subset count should shrink toward "every semantically load-bearing fixture adjudicated" and deliberately stop there, not trend to zero. (3) **Bootstrap trust** — a self-hosted compiler alone has the trusting-trust problem; an independent implementation built by a foreign toolchain is the trust anchor (cross-build the self-host from Rust gg and diff — diverse double-compiling). Likely end-state roles: ggdef = the norm (bounded subset) · Rust gg = independent juror + trust anchor, frozen into a conformance implementation (maintained for correctness, not perf/features — which is what makes keeping it affordable) · self-host = the product and showcase.

Defensive code accumulated for past compiler gaps is **technical debt with a stale justification.** The bug was fixed; the workaround stayed; the comment explaining "why the parallel-vector / extra clone / wrapper function" became a false historical record. New contributors read the workaround as canonical style, copy it, and the rot spreads.

Fossils already burned in (`StructRegistry` parallel `Vector[String]`+`Vector[int]` scan; `type_info_keys_safe` wrapper around `Dict.keys()`; `# parallel storage to dodge Dict[String, _] state-loss` comments) — all dodging bugs since fixed: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#self-host-as-the-elegance-showcase--and-retiring-fossils).

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
   > Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree. NEVER touch the main checkout or the orchestrator's working-branch checkout directly — every file operation, `cargo` command, and `git` command runs in your worktree path. Do NOT `cd` into either. Do NOT use absolute paths into the main checkout or the working-branch checkout (your worktree nests UNDER the main checkout, so an absolute path there writes into MAIN — see rule 7). If your `pwd` reports the main checkout or the working-branch checkout rather than your own worktree, STOP and report it back. (The concrete paths and branch name for the current environment are in the session handover.)

3. **Stage explicitly by file name.** Brief every agent: `git add <specific files>` only — NEVER `git add -a`, `git add .`, or `git commit -a`. Other agents (and the parent) may have uncommitted work in the tree; a sweeping stage clobbers it.

4. **Parent drives the integration sweep, not agents.** Brief every agent to run `cargo build` + `cargo test --lib` + targeted integration tests only. The 15-20 minute full `cargo test --test integration` is the parent's job; agents that try to wait for it stall and may be terminated mid-commit.

5. **Brief disjoint file zones when running agents in parallel.** Tell each agent which files the other agents are touching. Even with worktree isolation, telling the agent to stay away from a specific area is cheap insurance.

6. **Clean up scratch and worktrees once the work is integrated (or abandoned).** Agent worktrees and `gg`/`cargo` build scratch are disposable, but they do NOT dispose of themselves — they accumulate until the disk fills and a session dies mid-task with "no space left" (this happened: ~475K stale `/tmp/.tmp*` / `tmp.*` `gg`-build-scratch dirs filled the volume). So, as the closing step of every delegated round, after a track has landed on the integration branch (or been killed): (a) **capture first, prune second** — committed work survives on its branch through `git worktree remove`, but UNCOMMITTED work does not, so save any diff you still want (`git -C <wt> diff > /tmp/recover_<x>.patch`) BEFORE removing; (b) `git worktree remove --force <path>` the round's agent worktrees (unlock locked ones first; never touch the persistent long-lived dev worktrees — only ever the round's `agent-*` worktrees), then `git worktree prune`; (c) clear the stale `gg`-build-scratch — `find /tmp /tmp/claude-1000 -maxdepth 1 \( -name 'tmp.*' -o -name '.tmp*' -o -name 'clone_attr.*' -o -name 'bench_stages.*' \) -type d -mtime +1 -exec rm -rf {} +` plus the named per-track scratch dirs (`/tmp/gg_*`, `/tmp/sh_*`, `/tmp/gg_runtime_diff_*`), keeping only the `/tmp/recover_*` captures and anything touched today. (The `clone_attr.*`/`bench_stages.*` dirs are the measurement scripts' deliberately-kept work dirs — hundreds of MB each, driver-scale `.c` + O0 binaries.) (d) **check `git stash list` and capture+clear any stray stash** — the stash stack is repo-GLOBAL (rule 8) and any session that slipped the no-stash discipline leaves cruft a future `stash pop` could grab; `for i in $(seq 0 N); do git stash show -p "stash@{$i}" > /tmp/recover_stash_$i.patch; done` then `git stash clear` (owner preference 2026-07-03: agents must NEVER stash — save to `/tmp` — and the parent clears strays at close; the stack should be empty). Verify with `df -h /`, `git worktree list`, and `git stash list`. Do this as part of closing the round — not "later" — because "later" is when the disk is already full.

7. **Worktree-RELATIVE paths only — agent worktrees nest UNDER main.** Agent worktrees live at `<main-checkout>/.claude/worktrees/agent-*`, *inside* the main checkout. So an unqualified absolute path into the main checkout — or a `python`/`sed`/heredoc fallback after an Edit-tool disk-desync — writes into MAIN, not the worktree (this happened: a heredoc fallback dropped 20 files into the main checkout, a pure duplicate of already-committed work, caught only because the owner noticed). Brief every agent: all file ops use paths RELATIVE to its worktree; on an Edit-tool desync, re-Read and retry the Edit tool — never fall back to a shell heredoc with an absolute path; and after any non-Edit-tool write, run `git -C <main-checkout> status` and STOP if it shows changes. Worktree isolation is necessary but NOT sufficient when the worktrees are children of the thing they must not touch. (The concrete main-checkout path for the current environment is in the session handover.)

8. **NEVER `git stash` in agents — the stash stack is repo-GLOBAL across all worktrees.** Two concurrent round-32 scouts stashed "their" work and popped each other's: one pop grabbed a foreign 14-file diff, the other's entry went dangling (both fully recovered, but only via reflog surgery). Brief every agent: save/restore state with `git diff > /tmp/<name>.patch` + `git apply` — never stash. (→ war story in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md))

9. **Checkpoint scout prototypes to /tmp EARLY; run final gates FOREGROUND.** Agents are killable at any moment (session limits) — a scout that keeps its prototype only in its worktree until the final report loses everything (a round-32 scout lost 26 min of unsaved work; its relaunch checkpointed after every step and survived a second kill). And an agent whose last act is a *backgrounded* long run can stall indefinitely when the completion handoff is lost (three round-32 agents each stalled this way and needed manual nudges): brief agents to run their FINAL validation gates as foreground commands with explicit generous timeouts.

The failure mode when these rules slip is recoverable but ugly: working trees get contaminated, stashes accumulate mixed ownership, edits disappear into `stash@{N}` entries the parent can't easily attribute. The cost of fixing it after the fact is far higher than the cost of doing it right at launch.

## Review plans, TODO items, AND agent briefs/outputs with a fresh agent

A **fresh** agent must review any non-trivial artifact before it's acted on, folding each pass's findings, until a fresh pass raises **no reservations**. Use a *new* agent each pass (a reused one anchors on its prior conclusions; a fresh one re-derives from the code and catches what the artifact baked in). Brief every reviewer to verify each load-bearing claim against source with `file:line` and return SIGN OFF or specific cited reservations — not to rubber-stamp, not to invent reservations — and cross-check their claims yourself; a reviewer can be wrong too.

**Scout before you brief.** A brief is only as good as its premises, and this tree's most expensive mistakes were briefs built on stale ones. Before writing a brief — and before committing to any non-trivial plan — run a scout: a read-only probe/audit (often a delegated `Explore`/`general-purpose` agent) that verifies every load-bearing premise against CURRENT source with `file:line`, confirms the bug still reproduces, and where the plan claims a yield, **prototypes it end-to-end and MEASURES the real result.** Killing an unsound plan after a one-agent scout is a win. **Scout yield estimates MUST be end-to-end-verified — compile AND run AND diff whole output, never source-read** (three estimates in this tree were ~0 real because they were source-read).

**Ground the scout's design in the docs, not just the code.** Every scout brief MUST tell the agent to consult the relevant documentation FIRST and base its proposed design on it: `docs/language-design.md` (design rationale/philosophy + the intended semantics), `docs/book/` (the user-facing mental model — how the feature is *meant* to look), `docs/devbook/` (the compiler-internals/pipeline/layering design), and `docs/internals/` where present. The code shows what IS; the docs show what's INTENDED. A scout that designs only from current code faithfully reproduces whatever bug or fossil is already there and misses the reference-grade shape — the same trap as "Don't redesign around compiler gaps" (design toward what the language *should* do) and "Self-host as the elegance showcase". Cite the doc sections the design rests on in the scout's deliverable.

**The passes are SEQUENTIAL, not parallel** (fanning out N reviewers at v1 gives N opinions on v1 and misses defects introduced *by* a fold), and you **never stop on a pass that raised reservations** — a fold can leave a stale remnant or introduce a fresh defect, and only the next pass catches it. Do not rationalize skipping the confirming pass with "the fix was mechanical". The loop is: review → fold → fresh review of the corrected artifact → … → a clean pass. The why + worked examples (a fold that left a *contradicting* section; a plan that needed four passes) are in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#scout-before-you-brief-review-in-sequential-fresh-passes).

**Model allocation at the gates (harness-agnostic; owner 2026-07-18).** Put the strongest available model where finding-rate and consequence are highest: (a) the FIRST review pass on a fresh artifact — first contact catches the structural defects while folding is cheapest; (b) the FINAL pre-integration output-review — maximum consequence, plus model diversity against the executor's blind spots; (c) ad-hoc arbitration when two agents disagree on a load-bearing conclusion. Standard-strength models run everything else (scouts, executors, middle review passes) — the last clean pass definitionally finds nothing, so don't spend the scarce model there. Mandate quality still dominates model strength: the reviewer's checklist is what catches lane-shaped misses. (Measured 2026-07-18: the two strongest-model first-pass reviews contributed 10 of a 2G round's 22 folded reservations, including two empirically-proven new bug classes; a standard-model pass also overturned a strongest-model pass's directionally-wrong claim — every pass gets cross-checked regardless of model.)

This applies to four kinds of artifact:

1. **Plans / TODO items** — review before you start implementing.
2. **Agent briefs (≥3 fresh passes)** — a brief you hand a delegated `Agent` is a spec; review it *before launching*. A wrong brief wastes the whole execution + validation cycle, and these passes routinely catch a mis-identified root cause, a fix aimed at the wrong layer, or a "fix" that's already implemented.
3. **Agent output** — when the executor finishes, a fresh agent reviews its diff/commits *before you integrate or run expensive validation*. This includes the **breadcrumb-check**: verify no completed-status entries (`LANDED`/`FIXED`/`RESOLVED`/`DONE`/`SHIPPED`/`✅`) were added to `TODO.md` — those are either completed work to MOVE to `DONE.md` or pending follow-ups to REPHRASE as the work that remains. `TODO.md` holds pending work only. **It also includes the reference-grade gate (Core invariant #8): the reviewer's acceptance bar is *correct/principled*, not "matches the reference." If the change leaves a KNOWN DEFECT — even one that reproduces identically in Rust gg, or only manifests on a program that is UB/garbage/crash on *both* backends — that is NOT a SIGN OFF; it is a finding that the reference is also wrong (≥2 bugs). The reviewer states it as a reservation, and the round is not done until the language does the right thing (usually: *reject* the program with a typecheck error + negative fixture) in BOTH compilers. "Both backends agree on the wrong answer" / "benign because both are UB" is the exact phrasing that must trip the gate. The orchestrator must not accept it either — pushing the defect to a 'benign, filed' follow-up is the same failure.**
4. **Session-handover / state snapshots** — the in-flight-state doc a fresh session resumes from (the `TODO.md` handover block, the `MEMORY.md` north-star/scores) is a *spec the next session executes from*; a stale one misleads it exactly as a wrong brief misleads an executor. Before relying on it, a fresh agent verifies every load-bearing claim against ACTUAL state: commit hashes resolve, scores re-confirmed from the `*_comparison` tests (not memory), durable artifacts present at cited paths, nothing stale or contradictory. Same trap as "re-verify a premise".

So a delegated task runs: **scout (verify premises + measure yield end-to-end) → write brief → ≥3 fresh brief-reviews (until no reservations) → launch agent (worktree, per "Multi-agent orchestration") → fresh output-review → integrate.** A session handover runs the same loop on the state snapshot before the baton is passed. You (the orchestrator) hold the full context, brief the reviewers with it, and keep them honest.

**Scouts, briefs, and review checkpoints are `/tmp`-only — never `git add` them.** The gauntlet's paperwork (scout reports, executor briefs, census reports, review notes) is exhaust: it lives in `/tmp`. Durable content goes to its official home (`docs/language-design.md` / the define-gorget ledger / book / devbook); `TODO.md` entries are written **self-contained** — findings inline, never "see the scout file". The single session-state doc is `TODO.md`'s handover block. Round close `git rm`s any scout/brief that slipped into the repo (git-recoverable), guarded by a shrink-only docs/plans allowlist test; moving durable content out and deleting a completed plan is itself a reviewed change, not a silent bulk delete.

**Fold/patch scripts MUST assert their replace targets matched.** When you fold review findings across the sequential passes, a stale target silently dropped wastes the entire pass. Every fold asserts the old text was found and the new text landed (a `must_replace` helper) then greps the file for a distinctive fragment of the new text — or just use the Edit tool, which errors on no-match. `str.replace` silently no-ops on a stale target; a "folded" print is not verification.

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and add them to TODO.md.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
- **The handover stores invariants and commands, not numbers.** The `TODO.md` handover block and any state snapshot are specs the next session executes from; a dated number in them is a stale premise waiting to happen. Record *what to run to get the current number* and *what it means*, not the number itself.
- **Commit autonomously when green.** Once `cargo test --lib` and the round's relevant integration tests pass, commit without asking — this waiver **overrides the harness default of "commit only when the user asks".** The waiver covers `git commit` only: still ask before push / force-push / `reset --hard` / `branch -D` / `rm -rf` / amend / rebasing onto a shared branch / opening or closing PRs. Never commit red or skipped.
- **Stale-pending scan.** Aggressively move completed items to `DONE.md` every session, and periodically stale-scan pending items — verify the cited bug/stub still exists in current source before keeping one. Keep entries short and scannable, and keep `TODO.md` small.

## Round lifecycle

The delegated-task pipeline (→ Review) is the atom; a **round** is the unit the orchestrator works in. By default rounds run back-to-back, autonomously, until the owner stops them.

1. **Open a round around a headline theme — parallel tracks welcome.** Pick the next headline from `TODO.md`'s handover block to give the round its identity and its `DONE.md` record. Multiple items/tracks may run IN PARALLEL within the round — disjoint file zones, per Multi-agent orchestration rule 5 (e.g. a ggdef-oracle track ∥ a wrong-code track, or an eight-track wave). "One campaign" is about the round's *theme*, NOT a limit on concurrency. The one thing to avoid is PRE-WARMING a FUTURE round's campaign: don't start the next headline's scouts/briefs while the current round's chains are still executing — the round boundary is a landing gate. Focus means not fragmenting attention across rounds, not serializing work within one.
2. **Run the delegated pipeline** (→ Review): scout (verify premises + measure end-to-end) → brief → ≥3 fresh sequential brief-reviews → launch (worktree, → Multi-agent orchestration) → fresh output-review → integrate. A semantic change lands on every lane the same round (Core #9), each with its exercising fixture (Core #11).
3. **Commit as the chains land** (→ Task Continuity, "Commit autonomously when green").
4. **Round-close gate — the FULL integration sweep on BOTH backends (owner-required 2026-06-20).** After the round's commits are on the integration branch, run the full C sweep (`GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-$RANDOM.log`) and the LLVM sweep, plus the bootstrap / parity-split gates the change touched. A green FULL sweep is the sign-off — targeted and self-host gates are necessary, not sufficient (Core #7). Every fixture that hangs / spins / times out gets root-caused into a census row (lane · spin-vs-block · minimal probe · mechanism · TODO filing) — never merely killed; a both-lane hang is still ≥2 bugs (Core #8). Prefer a no-new-hangs executable guard (CRASH-count ratchet / shrinking `EXPECTED_HANGS`).
5. **Records.** Add the round's `DONE.md` entry (date-stamped); update `TODO.md`'s handover block IN PLACE (pending-only, no completed breadcrumbs, invariants+commands not numbers); refresh any state snapshot.
6. **Docs + hygiene.** Doc-write-through for behavior changes (→ Documentation); prune completed plans/briefs (`git rm`, git-recoverable); capture-then-prune agent worktrees, `/tmp` scratch, and any stray stash (→ Multi-agent orchestration rule 6).
7. **Open the next round autonomously.** STOP and ask the owner ONLY for a genuine DESIGN decision — language semantics, a scope/sequencing trade-off, a knob such as error-vs-silent-no-op, retiring a feature. Never stop for the discipline itself, and never for a choice whose reference-grade answer is clear (implement it, note it, and proceed). The owner may suspend autonomy for a stretch ("don't start the next round"); that is a live override of this default, not a change to it.
