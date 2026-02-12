# TODO

## High Priority — Self-hosting blockers
- Pattern-bound variable type inference in `print()`: destructured bindings from `case Error(e):` etc. don't have type_id in the resolution map, so `print("{e}")` defaults to `%lld` instead of the correct format specifier. Workaround: use `print(e)` for non-int types. [added: 2026-02-12]
- Interior mutability (RefCell equivalent): codegen uses RefCell for mutation from immutable contexts — could redesign to avoid need [added: 2026-02-11]
- Closures capturing mutable references: compiler passes use `&mut self` heavily [added: 2026-02-11]
- `@derive` macro expansion: reduces boilerplate for Debug, Clone, etc. [added: 2026-02-10]
- Box[T] codegen uses `int64_t` instead of actual inner type T: `Box.new(expr)` where expr is a struct/enum emits `int64_t*` malloc instead of `T*` malloc [added: 2026-02-12]
- Enum match destructuring doesn't dereference Box payloads: `case Add(a, b):` binds `a` as `Expr*` pointer, but `*a` (Gorget deref) isn't compiled to C dereference — `eval(*a)` passes pointer instead of value [added: 2026-02-12]

## Medium Priority — Language ergonomics & tooling
- `via` delegation in equip blocks: auto-forward trait methods through a struct field [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Showcase examples
- [showcase] `examples/calculator/` — Enums as AST + recursive evaluation: Expr enum with Box payloads, recursive match-based eval, multi-module (blocked by Box[T] codegen bugs above) [added: 2026-02-12]
- [showcase] `examples/todo_app/` — Data modeling + error handling: Task struct, Priority enum, Displayable trait, TaskStore with Vector, filter/map [added: 2026-02-12]
- [showcase] `examples/pipeline/` — Generics + closures + iterators: generic Pipeline[T] struct with chainable map/filter/collect, implicit it [added: 2026-02-12]
- [showcase] `examples/ecs/` — Entity-component system: trait-based components, Option[T] fields, system iteration with pattern matching [added: 2026-02-12]
- [showcase] `examples/collections/` — Custom generic data structure: generic Stack[T] backed by Vector, Iterator[T] impl, ownership moves [added: 2026-02-12]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
