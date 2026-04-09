# TODO

## High

- **Phase 1/2 Option/Result drops crash self-host driver**: The Option/Result Recursive drop infrastructure (Phase 1) and scope-exit drops (Phase 2) cause segfaults in the self-host driver binary. Root cause: Phase 2's `move_zero_and_mark` doesn't cover all consumption paths in complex programs (match extraction on Vector elements, nested struct field access). Integration tests pass (948/948) but the self-host driver crashes on any input with String types. Workaround: build driver with pre-Phase-1 compiler. Fix requires completing MoveZero coverage for all Option/Result extraction paths. [added: 2026-04-09]

- **Remove .clone() from Json.get()/at()**: Fix A (Ptr-scrutinee extraction widened to `is_resource_type` minus Box — done) and Fix B (`Option[Ptr(T)]` → `Option[T]` return conversion with deref+clone — done). However, `Ptr(Dict).get(key)` from pattern extraction on `&self` doesn't dispatch correctly — method calls on pattern-extracted Ptr locals produce wrong results. `.clone()` workaround is correct and safe. [updated: 2026-04-08]

- **Clone observability + Cloneable trait**: `.clone()` works on all types. `gg build --show-clones` done. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-05]


- **Extern module ABI — remaining structural whitelist**: `is_cstr_returning_fn` and `takes_cstr_for_str_param` deleted (all entries eliminated). `runtime_arg_by_ptr` is structural (collection/string self-deref + string clone/free) — cannot be removed without changing how the LIR represents method dispatch. See `docs/internals/extern-modules.md`. [updated: 2026-04-03]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

- **GIR call resolution confuses functions with similar first-param types**: Reported but could NOT reproduce with targeted multi-file test cases. Original symptom (ownership mismatch for variable not in scope) disappeared after code restructuring. May have been a transient issue from specific code patterns. Keep as low-priority — reopen if it recurs with a reproducible case. Root cause may be in `fn_sigs` lookup or borrow checker scope handling in multi-file compilation. [updated: 2026-04-08]

- **CoW: scope-escape materialization boundary**: When a CoW borrow escapes its scope (stored in a struct field, returned from function, pushed to collection), materialize to owned at the boundary. This is the generalization of the existing return-clone and field-store-from-borrow materializations. Once all escape boundaries are covered, strings no longer need an allocator pointer in every struct — the CoW system guarantees that all live references are independently owned at scope boundaries. Enables removing the allocator field from `GorgetString`, reducing every string from 32 to 24 bytes. [added: 2026-04-07]

- **MutationWhileBorrowed: extend to implicit CoW borrows**: The `is_ref_type` filter at `check_expr.rs:353` skips non-Ref-type origins. This means `auto x = vec.get(0).unwrap()` followed by `vec.push(y)` is not caught for non-Ref element types. The GIR CoW system handles this via `cow_collection_refs`, but extending the borrow checker to also catch it would add defense-in-depth. [added: 2026-04-05]

- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Borrow checker: reject multi-use `!` on strings**: `!` on strings now triggers real MoveZero (pragmatic skip removed). Borrow checker should catch use-after-move for `!key` in loops. [updated: 2026-04-08]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **IndexLoad reference semantics — borrow checker integration**: GIR already produces `Ptr(T)` + `CollectionRef` for resource-type IndexLoads; CoW materialization handles correctness; struct clone now uses `clone_to_owned`. Remaining: extend borrow checker to track IndexLoad borrows and report `MutationWhileBorrowed`, borrow propagation through fields/destructuring. [updated: 2026-04-07]


- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]

- **InlineC instruction — remaining 13**: 5 in for_loops.rs (typed key/value extraction needing C pointer cast), 8 in stmts/mod.rs (snapshot writes + assert formatting needing C globals/formatting). The 8 scalar iterator accessors (cap, states, order) now use proper CallExtern via runtime functions. [updated: 2026-04-06]

- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **`char` type backend bugs**: `char as int` gives garbage, char `==`/`!=` uses `gorget_str_eq`. [added: 2026-03-21]

- **Self-host comparison — 16 type checker mismatches**: At 861 fixtures (2026-04-07). Parser: 856/861 (99.4%). Resolver: 854/861 (99.2%). Type checker: 845/861 (98.1%), **0 crashes**. GIR Lowerer: 586/901 (65.0%), **0 crashes** — remaining: ~105 imported functions (equip methods from library modules, transitive import functions), ~76 error tests, ~35 `*mut` generic String params, ~26 trait `*unit` self, ~73 other. Import loading + module-path mangling + name-based filtering done. Full BFS reachability blocked by CoW segfault on instruction iteration. 16 type checker mismatches remain (type var numbering, closure param inference, Gorget-more-correct). [updated: 2026-04-08]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]
