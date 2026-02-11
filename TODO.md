# TODO

## High Priority (self-hosting blockers)
- String slicing and indexing: `str[i]` for char access, `str[start..end]` for substrings, char-by-char iteration — needed for lexer/parser [added: 2026-02-11]
- Type-safe generic HashMap[K,V]: current Dict is type-erased (void*) — compiler needs ~90 hash map uses for symbol tables, resolution maps, type tables [added: 2026-02-11]
- Vec capacity and slicing: `reserve(n)`, `with_capacity(n)`, `vec[start..end]` — needed for token streams and AST node lists [added: 2026-02-11]
- Iterator adapters: `.map()`, `.filter()`, `.collect()`, `.fold()` on iterators — compiler uses iterator chains pervasively for AST traversal [added: 2026-02-11]
- String builder / format function: `format(...)` or equivalent for building strings without print — codegen builds thousands of C strings [added: 2026-02-11]

## Medium Priority
- Wrapping operators `+%`/`-%`/`*%`: new lexer tokens + parser + codegen for intentional wrapping arithmetic (pairs with overflow checking) [added: 2026-02-10]
- `@derive` macro expansion [added: 2026-02-10]
- `via` delegation in equip blocks: auto-forward trait methods through a struct field (depends on default methods + trait inheritance) [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Cross-module name resolution: module system loads files but resolution across modules is incomplete — needed to split compiler into multiple files [added: 2026-02-11]
- `?` operator for Result propagation: currently only `throws`/`try` exists — compiler needs ergonomic error handling without exceptions [added: 2026-02-11]

## Low Priority
- Basic orphan rule: equip block must be in the module that defines the trait or the type (implement when module system matures) [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type (implement after trait system stabilizes) [added: 2026-02-10]
- Interior mutability (RefCell equivalent): codegen uses RefCell for mutation from immutable contexts — could redesign to avoid need [added: 2026-02-11]
