# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

## Build & Test

```bash
cargo build              # build the compiler
cargo test --lib         # unit tests (currently ~970)
cargo test --test integration -- --test-threads=4  # integration tests (currently ~928, parallel with serial_test groups for fixture conflicts)
cargo test               # all tests
```

**Always pipe integration tests through `tee`** — they take ~3 minutes and failure diffs can be long. Save output so you don't have to re-run to find which test failed. Use a random filename to avoid collisions with parallel agents:

```bash
cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-$RANDOM.log
```

**Testing the LLVM backend.** The harness reads `GG_BACKEND=llvm` and appends `--backend=llvm` to every fixture's `gg build` invocation (see `tests/integration.rs:29-48`). All-or-nothing per `cargo test` run; there's no per-test override.

```bash
# LLVM full sweep (sequential — see note below)
GG_BACKEND=llvm cargo test --test integration --release -- --test-threads=1 2>&1 | tee /tmp/llvm-$RANDOM.log

# LLVM single test
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

**Use `--test-threads=1` for LLVM full sweeps.** The parallel runner hits `cargo`-level rebuild races where the integration test binary gets recompiled mid-run, producing false-positive failures that vanish on rerun. Sequential is ~28 min for 1047 tests vs ~13 min parallel for C. Single-test invocations and small subsets are fine to run with the default `--test-threads=4`.

Both backends should be at parity (1047/1047 as of 2026-04-30); a regression on one but not the other usually means the change touched a backend-specific path rather than shared LIR.

**Build / binary timeouts** (override on shared / loaded hosts):

- `GG_BUILD_TIMEOUT_SECS` — outer `gg build` deadline. Default 120 (integration suite) / 180 (security suite). On a multi-agent box where DEBUG `cargo run -- build --backend=llvm self_host_lowerer/driver.gg` can drift past 5 minutes, set this generously: `GG_BUILD_TIMEOUT_SECS=600`.
- `GG_TEST_TIMEOUT_SECS` — per-test-binary deadline. Default 30. Bump for slow stress fixtures (`stress_*`, p2p, gorget-arena builds).

## Documentation

- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience, not Gorget experience)
- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale
- `docs/internals/` — [Compiler Internals](docs/internals/README.md): contributor-facing pipeline and design docs

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, provenance inference, borrow checking
- `src/ir/` — Intermediate representation and lowering from AST (monomorphization, drop insertion, closures)
- `src/lir/` + `src/backend/c_lir/` — SSA-based LIR backend (sole production backend)
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

CoW's default everywhere is **borrow** — assignments, regular function
call args, collection reads all propagate Ptr aliases at zero cost.
Clones happen only at ownership boundaries, where the destination must
own (collection puts, returns, struct/enum field init, closure
captures). Even there, the compiler prefers move when liveness allows
it.

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
See `docs/internals/copy-on-write.md` Phase 3 for the full
specification.

## Solution Quality

- Prefer robust, architecturally sound solutions over quick fixes. When the trade-off is unclear, discuss both approaches and ask before proceeding.
- Aim for generic solutions that solve classes of problems, not just the immediate symptom. Be resourceful — read code, search the web, study how other compilers solve the same problem. Exhaust every avenue before concluding something can't be done.
- Flag code smells and structural issues you encounter, even if unrelated to the current task. Log non-trivial findings to `TODO.md`.
- You are allowed an opinion. If the user is proposing something dumb, call him out.
- You are allowed to swear if opportune. Don't over do it, but if something deserves a 'holy shit', use it!

## No name matching

Do not pattern-match on function names, type names, runtime-symbol prefixes, or any other identifier string to make a semantic decision. If you find yourself writing `matches!(name, "gorget_str_trim" | "gorget_str_substring" | ...)` or `if name.starts_with("Vector__")` to decide what something *means* — stop. The metadata you need is missing one layer up.

Symptoms that this rule is being violated:

- Two parallel lists in different files that have to be kept in sync (e.g. a `BuiltinMethodDecl.returns_view: bool` registry AND a separate `is_view_returning_string_runtime` name list).
- A new method/type starts behaving wrong silently when added, because the name list wasn't updated.
- Comments like `// keep both lists in sync`.
- Decisions made in lowering or backend code that look like business logic ("this string is a view", "this collection is shared") but are spelled as substring tests on identifiers.

The right shape:

- The semantic flag belongs on the typed declaration (`BuiltinMethodDecl`, `TypeDef.metadata`, `Inst::CallRuntime` sidecar, etc.) — set once at the source of truth.
- Propagate that flag through the IR/LIR via typed fields, not by re-deriving from names downstream.
- Consumers read the flag via a typed accessor (`decl.returns_view`, `inst.abi_kind`), never by inspecting a string.

The exception: at the C-emit boundary you have to spell the runtime symbol (the name *is* the contract with the runtime). Even there, drive the spelling from a typed registry — never make a routing decision based on `if name == "..."`.

If the metadata genuinely doesn't exist yet, **add it** rather than fishing for the answer in a name. CoW, ownership boundaries, ABI kinds, view-vs-owned, fresh-vs-borrowed: these are language rules — they should work declaratively, not by enumerating call sites.

## Layering discipline

"No name matching" is one rule of a broader discipline that governs how information crosses IR layer boundaries (AST → GIR → LIR → backend). The full rules live in [`docs/internals/layering-discipline.md`](docs/internals/layering-discipline.md); the four-line summary:

1. **Lossless on invariants, lossy on syntax.** Each layer may resolve abstractions (generics, methods, traits) and add information (control flow, SSA). It may not drop semantic invariants (ownership, drop strategy, view-vs-owned, ABI, copy semantics, borrow provenance). Invariants accumulate; abstractions evaporate.
2. **Typed metadata, not name-matched.** Facts cross boundaries as typed fields on structs — never as name prefixes, sentinel values, or runtime-symbol conventions. ("No name matching" is this rule applied at the runtime-symbol boundary.)
3. **One source of truth per axis.** For each kind of information, exactly one piece of metadata at exactly one location, read through one accessor. No parallel sidecar maps.
4. **Resolve once, write through.** When a pass resolves an abstraction, the result writes into the next layer's typed metadata. Downstream doesn't redo the work and doesn't get to disagree.

**Litmus test:** if a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong. The fix is always upstream — add the field, write it at the source, read it at the consumer. Cite the doc in PRs that touch IR layer boundaries.

**Debugging heuristic — fix complexity as a signal of wrong layer.** When you've localized a bug and the fix you're sketching is *intrinsically complex* — save/restore state around branches, phi insertion at merges, scope-tracking name maps, manual SSA repair, ad-hoc invalidation, "rebind correctly across CF merges" — stop. That complexity is almost always a tell that you're patching a *symptom*, not the cause. Real bugs in well-layered compilers are usually one-line oversights at a **write** site, not multi-case rules at the **read** site. Before committing to the complex fix:

1. Trace the data the buggy site is reading. *Where was it last written?*
2. Look at the writer. *Did it respect all the typed metadata available at that point?* Or did it default / hardcode / collapse cases that the upstream had distinguished?
3. If yes (writer was lossy), the bug lives there — fix it at the source. The downstream "complex fix" usually evaporates because the wrong assumption never gets propagated.
4. If no (writer was faithful), then trace one more layer up. Repeat.

Rule of thumb: every layer hop you climb without finding the bug should make you *more* suspicious of your initial diagnosis, not less. A multi-line fix at the symptom layer that requires you to reason about every interleaving of branches is the universe trying to tell you the bug is elsewhere.

Worked examples:
- Snag #17 (chained `text.substring(...)` corrupting later `parse_float(text)`): symptom was `cow_materialize_alias` rebinding a variable name across CF merges. Fix candidates at that layer: don't rebind / save-restore / phi insertion — all 50+ lines, all with judgment-call tradeoffs. Real bug: `resolve_builtin_method_return_type` always wrote `MutPtr(self_type)` to `fn_sigs` regardless of the protocol's `self_conv` flag, so `lower_method_call`'s `needs_mut` check fired on non-mutating methods, *triggering* the materialization that then exposed the rebind. Fix at the writer: 5 lines dispatching on `self_conv`. The rebind path is now correctly never-taken; the SSA-correctness question evaporates.
- Snag #13 (Box-recursive enum returned by value links to undefined `__gorget_box_alloc_<T>`): symptom was missing helper declarations. Tempting fix: scan recursive-drop tables for `Box__X__drop` strings and synthesize helpers — but that's name-matching and adds a parallel inference. Real bug: the LIR `StructDef` for `Box[T]` had typed inner-type info available at registration time but didn't expose it to the C backend. Fix at the writer: add `box_inner_type: Option<String>` to `StructDef`, set at the regular-Box registration site, read at the helper-emitter. Consumer is one `for sd in &module.structs` loop; no name parsing.

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the fixture's expected output must reflect what the language *should* do, not what it currently does.

Forbidden: reshaping the surrounding code (tests, fixtures, examples, even production code) to avoid the gap. Even when commented, this buries the bug. The wired-in expected output (or the surviving workaround idiom) becomes the load-bearing artifact, and "passing" tests lock in buggy behavior as canonical.

Worked examples from this codebase:
- The Tier E §8.1 drop-flag agent hit the universal `!`-param drop-at-exit leak, redesigned the canonical `drop_flag_param_seed.gg` fixture around it (using locals instead of `!` params), and wired it in with `consume ck\nck-done` and no `drop ck` between them as expected output. The bug stayed hidden for a day until a deliberate scope-correction reproduced it; three masked-leak tests needed expected-output updates when the bug was fixed.
- The `Dict.len()` codegen bug had `scores.keys().len()` documented as a workaround in a fixture comment for ~8 weeks; the bug was silently fixed, but the workaround idiom survived. Same shape: the redesign acquires inertia long after its justification disappears.
- The Phase A `collection_runtime_type` migration TODO was filed 2026-05-02; the work shipped naturally as a side-effect of foundation commits over the next 3 days, but nobody updated the TODO. The right move when the agent saw it was to refuse to manufacture migration work to fit the stale entry — recursive instance of this rule.

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

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and add them to TODO.md.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
