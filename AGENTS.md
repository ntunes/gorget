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

## Documentation

- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale

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

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
