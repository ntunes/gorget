# TODO

## High Priority

## Medium Priority
- Integer overflow checking on `+`/`-`/`*`: only div-by-zero guard exists today — add overflow detection on add/sub/mul that panics by default, plus `--overflow=wrap` compiler flag [added: 2026-02-10]
- Wrapping operators `+%`/`-%`/`*%`: new lexer tokens + parser + codegen for intentional wrapping arithmetic (pairs with overflow checking) [added: 2026-02-10]
- `@derive` macro expansion [added: 2026-02-10]
- `via` delegation in equip blocks: auto-forward trait methods through a struct field (depends on default methods + trait inheritance) [added: 2026-02-10]
- Raw strings, multi-line strings [added: 2026-02-10]
- `forge` package manager basics [added: 2026-02-10]

## Low Priority
- Basic orphan rule: equip block must be in the module that defines the trait or the type (implement when module system matures) [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type (implement after trait system stabilizes) [added: 2026-02-10]
