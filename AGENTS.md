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

Collections always own their items. The compiler decides per-arg at each
consuming position (`push`, `put`, `set`, `insert`, `send`, `v[i] = x`):

| Argument shape        | Action              | Rationale                          |
|-----------------------|---------------------|------------------------------------|
| `!arg` (explicit)     | move_zero after call| Caller opted in — source is dead   |
| expression temp       | move_zero after call| Always last-use by construction    |
| named local, last use | move_zero after call| Zero-cost transfer                 |
| bare param            | clone before call   | Caller still owns it               |
| borrow (Ptr/Ref/CoW)  | clone before call   | Source stays live                   |
| non-last-use local    | clone before call   | Caller needs its value             |
| static literal        | runtime materialize | cap==0 clone                       |

**This is the compiler contract — not a suggestion.** If an arg is non-last-use,
it MUST be cloned before the call, not zeroed after it. Post-call zeroing is only
correct for the three move cases (explicit `!`, expression temp, named last-use).
See `docs/internals/copy-on-write.md` Phase 3 for the full specification.

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

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and add them to TODO.md.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
