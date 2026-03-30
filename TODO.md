# TODO

## High

- **Remaining 50 leaks in yaml_parse**: Down from 374→50 (-87%). Three categories: (1) String temps unregistered by `unregister_gorget_string_args` (12 leaks) — owned string temps passed to functions returning struct/enum. `owned_locals` skip helps for direct call results but misses VarDecl/assign chains. (2) Yaml anchor merge intermediates (8 leaks) — merge_into destructures `!yaml_source`, puts values into target Dict, but source Dict values leak on drop. (3) cstr-returning functions (10 leaks) — gorget_string_adopt wraps correctly but GIR type is still StringView → return path adds redundant gorget_string_from_str clone. Remaining 20: str_cat, string_clone, misc. [updated: 2026-03-30]

- **`is_resource_type = needs_drop` (deferred)**: The correct architectural unification — all droppable types get uniform CoW treatment. Eliminates the string special-casing and `unregister_gorget_string_args` workaround. `string_param_locals` already removed (replaced with type-based check). 110 failures from ABI change (String params become Ptr). Needs ~100 deref site fixes. Deferred until current ownership model is stable. [updated: 2026-03-30]

- **`!` string params: C backend Ptr→Str clone**: `!` string params pass via MutPtr. C backend's Ptr→Str SlotStore clones (`gorget_string_clone`) instead of memcpy. Should be zero-cost move. Needs C backend fix: when is_move=true in SlotStore for string types, use memcpy not clone. [added: 2026-03-30]

- **CoW Phase 1f: multi-use clone needs liveness analysis**: Single-use auto-move works for push/put/set and enum/struct constructors. Multi-use clone attempted but `is_single_use` over-counts across branches (50 failures). Needs per-path liveness analysis. Phase 2a Step 5 (unified Type__drop cleanup) blocked on this. [updated: 2026-03-30]

- **Explicit clone roadmap (Phase 2 remaining)**: `.clone()` works on all types. `directive explicit-clone` promotes warnings to errors. Remaining: fix 5 false-positive implicit clone warnings in p2p.gg, add `Cloneable` trait to type system. [updated: 2026-03-25]

- **Recursive/Custom elem_drop — 2 remaining fixes**: (1) Option[Ref_T].unwrap() must auto-clone Ptr→T. (2) C backend Option wrapping must CLONE for Recursive/Custom elements. Both needed for full self-cleaning collections. [updated: 2026-03-28]

- **LIR backend: Phase 3 — multi-file project support (gorget-arena)**: 0 C compilation errors, 0 linker errors, 0 C warnings. Phase 4 stdlib name mapping and cross-module type registration complete. [updated: 2026-03-21]

- **Trait-bounded generic functions don't monomorphize**: `void print_sum[Summable T](T val)` — linker error. Core language feature gap. [added: 2026-03-23]

## Medium

- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **IndexLoad reference semantics (target design)**: `v[i]` should return `&T`, auto-clone at `T` boundaries. Enables zero-cost reads and in-place mutation. [added: 2026-03-22]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]

- **Closure return type inference**: Multi-line closures with `return` typed as void. Need to propagate return type in typecheck.rs. [added: 2026-03-21]

- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

## Low

- **`char` type backend bugs**: `char as int` gives garbage, char `==`/`!=` uses `gorget_str_eq`. [added: 2026-03-21]

- **Self-host parser: 3 remaining mismatches**: null byte, str alias, float precision — all unfixable at self-host level. [updated: 2026-03-21]

- **Self-host comparison regression (35/858 resolver, was 797/797)**: After CoW work, the Rust compiler registers new builtins/keywords that the self-host resolver doesn't know about, shifting all DEF IDs. The dangling string view bug is fixed (owned_slice in lexer). Remaining work: sync the self-host resolver's builtin trait list, keyword set, and AST nodes with the current Rust compiler. Similar sync needed for parser/typechecker comparisons. [added: 2026-03-30]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Multi-line closures with return typed as void**: `Expr::Block(_) => UNIT_TYPE` hardcoded. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]
