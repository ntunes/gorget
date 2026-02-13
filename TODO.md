# TODO

## High Priority — Self-hosting blockers
(none currently)

## Medium Priority — Language ergonomics & tooling
- `@derive` for generic structs/enums: needs type parameter forwarding in generated equip blocks [added: 2026-02-13]
- `via` delegation in equip blocks: auto-forward trait methods through a struct field [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Showcase examples
- Mutable borrow parameters in free functions: `Type &name` params in non-method free functions generate by-value C code instead of pointer params — mutations don't propagate to caller; workaround: use `&self` methods instead [added: 2026-02-13]
- `DenseStore[T]` / archetype ECS storage: needs Default trait for generic slot initialization [added: 2026-02-13]
- Type erasure / `any` type: allows fully generic World without user-defined struct [added: 2026-02-13]
- Query builder API for ECS: `world.query[Position, Health]()` returning iterator — needs variadic generics or macro system [added: 2026-02-13]
- [showcase] `examples/collections/` — Custom generic data structure: generic Stack[T] backed by Vector, Iterator[T] impl, ownership moves [added: 2026-02-12]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
