# TODO

## High Priority
- Higher-order collection methods: `.filter()`, `.fold()`, `.reduce()` on Vector/Dict/Set [added: 2026-02-10]

## Medium Priority
- Chained method calls on trait methods: `obj.trait_method().other_method()` fails because `infer_receiver_type` doesn't handle `MethodCall` — need to resolve intermediate return types through trait registry [added: 2026-02-10]
- Named args / default params (semantic validation) [added: 2026-02-10]
- `@derive` macro expansion [added: 2026-02-10]
- `via` delegation in equip blocks: auto-forward trait methods through a struct field (depends on default methods + trait inheritance) [added: 2026-02-10]
- Raw strings, multi-line strings [added: 2026-02-10]
- `forge` package manager basics [added: 2026-02-10]

## Low Priority
- Basic orphan rule: equip block must be in the module that defines the trait or the type (implement when module system matures) [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type (implement after trait system stabilizes) [added: 2026-02-10]
