# TODO

## High Priority — Self-hosting blockers
(none currently)

## Medium Priority — Codegen cleanup (discuss before fixing)
- **tuple_typedefs linear dedup** (c_item.rs:1679): `register_tuple_typedef` scans the whole vec with `.iter().any()` on every call — O(n) per registration. A parallel `HashSet<String>` for O(1) dedup would be trivial but adds a field to CodegenContext. Discuss whether tuple counts are high enough to matter. [added: 2026-02-13]
- **Closure env memory leak** (c_expr.rs ~3140): Capturing closures emit `malloc(sizeof(env))` but never `free` the env. No GC or ref-counting exists. Fixing properly requires either registering closure envs in the drop scope system or adding reference counting. Fine for V0.3 MVP but should be addressed before any long-running program showcase. Discuss design approach. [added: 2026-02-13]
- **scan_for_generics / scan_for_tuples duplication** (c_item.rs ~860-1044 vs ~1717-1879): Nearly identical AST traversal code exists for generic discovery and tuple discovery — two ~200-line walkers doing the same recursive descent with different leaf actions. A shared visitor pattern could consolidate this but it's a bigger refactor. Discuss whether it's worth the abstraction. [added: 2026-02-13]
- **No cycle detection in collect_all_trait_methods** (c_item.rs:1905): Recursive trait method collection via `extends` assumes acyclic graph. If a trait accidentally extends itself (directly or transitively), this infinite-loops. Semantic analysis likely prevents it, but a `visited: HashSet` would be cheap insurance. Discuss whether to fix here or in semantic. [added: 2026-02-13]

## Medium Priority — Stdlib additions
- **`std.conv` additions**: `parse_float(str) -> float`, `to_str(int) -> str`, `to_str(float) -> str` [added: 2026-02-13]
- **`std.random.rand_range(int, int) -> int`**: random integer in a range [added: 2026-02-13]
- **`std.os` additions**: `getcwd() -> str`, `setenv(str, str)`, `platform() -> str` [added: 2026-02-13]
- **`std.time.time_ms`**: millisecond-precision timestamp for benchmarking [added: 2026-02-13]
- **`std.fmt`** module: `Displayable` trait, `format` function — referenced in formatter tests but not implemented [added: 2026-02-13]

## Medium Priority — Language ergonomics & tooling
- `via` delegation in equip blocks: auto-forward trait methods through a struct field [added: 2026-02-10]
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Showcase examples
- `DenseStore[T]` / archetype ECS storage: needs Default trait for generic slot initialization [added: 2026-02-13]
- Type erasure / `any` type: allows fully generic World without user-defined struct [added: 2026-02-13]
- Query builder API for ECS: `world.query[Position, Health]()` returning iterator — needs variadic generics or macro system [added: 2026-02-13]
- [showcase] `examples/collections/` — Custom generic data structure: generic Stack[T] backed by Vector, Iterator[T] impl, ownership moves [added: 2026-02-12]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
