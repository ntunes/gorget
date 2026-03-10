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
cargo test --lib         # unit tests (currently ~670)
cargo test --test integration -- --test-threads=1  # integration tests (currently ~493, run serially to avoid cargo lock contention)
cargo test               # all tests (use --test-threads=1 if integration tests hang)
```

## Documentation

- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, borrow checking
- `src/ir/` — Intermediate representation and lowering from AST (monomorphization, drop insertion, closures)
- `src/backend/` — Code generation backends (`c/` is the current backend)
- `src/formatter/` — Source formatter (`gg fmt`)
- `src/sim/` — Interpreter / simulation runtime
- `src/loader.rs`, `src/lockfile.rs`, `src/manifest.rs` — Package management
- `src/report.rs` — Test report generation
- `tests/fixtures/*.gg` — Integration test programs with deterministic stdout
- `tests/integration.rs` — Integration test harness: builds fixtures via `cargo run -- build`, executes, asserts stdout

## Language Syntax

- Indentation-based blocks (Python-style)
- Type-first declarations: `int x = 5`, `str name = "hello"`, `Vector[int] items = Vector[int]()`
- Functions: `int add(int a, int b): return a + b`
- Expression-body functions: `int double(int x) = x * 2`
- User-defined enum variants require qualified access: `Color.Red()` not `Red()`. Prelude variants (`Ok`, `Error`, `Some`, `None`) stay bare.
- Dot-shorthand: `.Red()` desugars to `Color.Red()` when the expected type is known from context (VarDecl, assignment, return, function arg, match pattern).
- Glob import brings variants into bare scope: `from gg.log import LogLevel.*` → `Info()`, `Debug()` etc.
- Generic enum variants remain bare (e.g. `Some(42)`, `Just(x)`) since qualified generic syntax is not supported.
- Closures: `(params): body` syntax
- Function types: `int(int, int)` (return type first)
- Generic structs need explicit type args: `Pair[int, int](10, 20)`
- String interpolation: `print("{variable}")`
- Match uses `case` keyword: `match x: case 1: ... else: ...`

- Meta / compile-time evaluation uses the `meta` keyword:
  - `meta const name = expr` — compile-time constant binding (valid inside generic function bodies)
  - `meta T is Category` — type predicate: `meta T is Numeric`, `meta T is Struct`, etc.
  - `meta log(expr)` — compile-time debug output (printed during compilation, not at runtime)
  - `meta op` parameter — accepts a binary operator token at call site; use `a meta[op] b` in body
  - `meta for vname, T in variant_payloads(E): case vname(c): ...` — expand match arms from enum variants
  - Phase-0 builtins: `platform()`, `arch()`, `arch_word_bits()`, `feature(str)`, `debug()`, `sizeof(Type)`, `alignof(Type)`, `typename(Type)`, `embed_file(str)`
  - Delayed (generic body) builtins — type: `typename(T)`, `typeof(T)`, `sizeof(T)`, `bitwidth(T)`, `min_val(T)`, `max_val(T)`, `implements(T, str)`
  - Delayed — struct: `fields(T)`, `field_names(T)`, `field_count(T)`, `has_field(T, str)`, `field_type(T, str)`, `field_value(val, fname)`, `field_set(obj, fname, value)`
  - Delayed — enum: `variant_names(T)`, `variant_count(T)`, `variant_payloads(T)`, `enum_ordinal(T, str)`, `enum_from_ordinal(T, n)`, `make_variant(T, str)`

**Always use type-first native Gorget syntax** when generating code, writing plans, or providing examples. Write `int x = 5` not `x: int = 5` or `let x = 5`. Write `str greet(str name)` not `fn greet(name: str) -> str`.

## Solution Quality

Always prefer sound, solid, and architecturally elegant solutions over quick fixes. This is a compiler — shortcuts compound into technical debt that becomes increasingly painful to unwind. When faced with a choice between a simpler approach and a more robust one, default to the solution that will age well. If the trade-off is unclear, discuss it: explain both approaches, compare their long-term implications, and ask before proceeding. A good solution implemented once is worth more than a fast solution revisited three times.

Push on creativity: aim for generic, elegant solutions that solve entire classes of problems rather than patching individual symptoms. The best fix is the one that makes the next five bugs impossible.

Be infinitely resourceful, read code, search the web, innovate. When a path is blocked, find another. When the obvious approach falls short, dig deeper — read more code, explore adjacent systems, study how other compilers solve the same problem. Exhaust every avenue before concluding something can't be done.

If you encounter code smells, structural weaknesses, or questionable patterns while exploring the codebase — even when unrelated to the current task — flag them. Don't hold back: recommending a significant refactor or a design change is welcome if it leads to a better codebase. Mention findings in your response and log anything non-trivial to `TODO.md` so it doesn't get forgotten.

## Task Continuity

Maintain `TODO.md` and `DONE.md` at the project root to track work across plans and conversations.

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be written to `TODO.md` before moving on. Nothing falls through the cracks.

- **Adding work:** Append new items to `TODO.md`. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** Delete the item from `TODO.md` and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Never mark items as done in `TODO.md` — completed items only live in `DONE.md`.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and add them to TODO.md.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, add it to `TODO.md` and move on. Never silently work around a bug — either fix it or record it.
- **Never delete `TODO.md`** — only move completed items out of it.
