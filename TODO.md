# TODO

## High Priority — Self-hosting blockers
- Enum destructuring in match: `case Generic(def_id, args):` must bind payload fields to variables [added: 2026-02-11]
- Interior mutability (RefCell equivalent): codegen uses RefCell for mutation from immutable contexts — could redesign to avoid need [added: 2026-02-11]
- Closures capturing mutable references: compiler passes use `&mut self` heavily [added: 2026-02-11]
- `@derive` macro expansion: reduces boilerplate for Debug, Clone, etc. [added: 2026-02-10]

## Medium Priority — Language ergonomics & tooling
- `via` delegation in equip blocks: auto-forward trait methods through a struct field [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
