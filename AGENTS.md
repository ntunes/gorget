# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a Python-like language that compiles to C via transpilation.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → C codegen → cc → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

## Build & Test

```bash
cargo build              # build the compiler
cargo test --lib         # 177 unit tests
cargo test --test integration -- --test-threads=1  # 30 integration tests (run serially to avoid cargo lock contention)
cargo test               # all tests (use --test-threads=1 if integration tests hang)
```

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, borrow checking
- `src/codegen/` — C code generation (uses GCC extensions: statement expressions, `__typeof__`)
- `tests/fixtures/*.gg` — Integration test programs with deterministic stdout
- `tests/integration.rs` — Integration test harness: builds fixtures via `cargo run -- build`, executes, asserts stdout

## Language Syntax

- Indentation-based blocks (Python-style)
- Functions: `int add(int a, int b): return a + b`
- Expression-body functions: `int double(int x) = x * 2`
- Enum variants constructed as bare calls: `Red()` not `Color.Red()`
- Closures: `(params): body` syntax
- Function types: `int(int, int)` (return type first)
- Generic structs need explicit type args: `Pair[int, int](10, 20)`
- String interpolation: `print("{variable}")`
- Match uses `case` keyword: `match x: case 1: ... else: ...`

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

- **Starting a multi-step plan:** Write all high-level tasks to `TODO.md` (append, never replace existing items).
- **Completing a task:** **Remove** the item from `TODO.md` and add it to `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Do NOT mark items as done (e.g. `[x]`) in `TODO.md` — completed items must be deleted from TODO and only appear in DONE.
- **Before replacing a plan:** Always read `TODO.md` first. Carry forward any incomplete items — they must remain in `TODO.md`.
- **Quick-fix interruptions:** After finishing a tangential fix, remind the user about remaining `TODO.md` items.
- **New conversation start:** Read `TODO.md` at the start to restore context on pending work.
- **Stale items:** If a `TODO.md` item is older than 7 days, remind the user and ask whether to implement or drop it.
- **Discovered issues:** If you encounter unrelated or tangential bugs, limitations, or issues while implementing a plan: fix them inline if they are small and straightforward. If they are too large or would require a separate plan, suggest the user add them as new tasks to `TODO.md` so they aren't forgotten.
- **Partial completion:** If a task is only partially implemented, reassess whether the remaining work deserves the same priority. The unfinished portion may be less urgent than the original task and could be downgraded to a lower priority tier in `TODO.md`.
- **Never delete `TODO.md`** — only move completed items out of it.
