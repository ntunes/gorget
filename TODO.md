# TODO

## Critical — Stdlib (benefits everyone, unblocks self-hosting)
- Type-safe generic HashMap[K,V]: current Dict is type-erased (void*) — need proper generics over hash maps [added: 2026-02-11]
- Vec capacity and slicing: `reserve(n)`, `with_capacity(n)`, `vec[start..end]` [added: 2026-02-11]
- Iterator adapters: `.map()`, `.filter()`, `.collect()`, `.fold()` on iterators [added: 2026-02-11]
- `?` operator for Result propagation: ergonomic error handling without exceptions [added: 2026-02-11]

## High Priority — Self-hosting (write the compiler in Gorget, still targeting C)
- Cross-module name resolution: module system loads files but resolution across modules is incomplete [added: 2026-02-11]
- `@derive` macro expansion: reduces boilerplate for Debug, Clone, etc. [added: 2026-02-10]
- Interior mutability (RefCell equivalent): codegen uses RefCell for mutation from immutable contexts — could redesign to avoid need [added: 2026-02-11]

## Medium Priority — Language ergonomics & tooling
- `via` delegation in equip blocks: auto-forward trait methods through a struct field [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)
