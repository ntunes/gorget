# TODO

## Critical — Codegen bugs (blocks struct-with-collection patterns)
- Method calls on struct fields generate wrong type: `infer_receiver_type` returns `"Unknown"` for `FieldAccess` expressions (e.g. `list.values.push(x)` → `Unknown__push`). Need to resolve the object's struct type, look up the field's declared type, and return it. [added: 2026-02-11]
- Silent data loss on mutating struct field collections: `needs_temp` wraps `self.field` and `obj.field` in a temporary copy, so `self.values.push(x)` pushes to a discarded copy. `FieldAccess` on identifiers/self is an lvalue and should use `&obj.field` directly. [added: 2026-02-11]

## On hold — waiting for bug fixes above
- Linked list example (`examples/linked_list.gg`): arena-backed linked list with Iterator support. Blocked by the two struct-field codegen bugs above. [added: 2026-02-11]

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
