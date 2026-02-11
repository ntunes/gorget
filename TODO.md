# TODO

## High Priority — Parser bugs
- Index after field access (`self.values[i]`) fails to parse: the `[` triggers generic-type-arg disambiguation which succeeds (parsing `i` as type), then backtracks, but error recovery corrupts parser state. Workaround: use `.get(i)` method instead. [added: 2026-02-11]

## High Priority — Unblocked
- Linked list example (`examples/linked_list.gg`): arena-backed linked list with Iterator support. Unblocked by struct-field codegen fixes. [added: 2026-02-11]

## High Priority — Self-hosting blockers
- Cross-module name resolution: module system loads files but resolution across modules is incomplete [added: 2026-02-11]
- String concatenation (`+` on strings or StringBuilder): compiler needs to build C source strings, mangle names, compose error messages [added: 2026-02-11]
- Nested generics (`Vector[Vector[int]]`): compiler data structures use nested collections extensively [added: 2026-02-11]
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
