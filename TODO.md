# TODO

## High

- **Leak reduction — status**: Error path fixed (emit_enum_init_owned). StringView call results upgraded to GorgetString in call_tracked. Match pattern strings clone-on-extract. Struct field strings now Ptr(GorgetString) references (matching collection pattern). LIR FieldLoad cap/alloc zeroing blocks deleted. [updated: 2026-04-05]


- **Clone observability + Cloneable trait**: `.clone()` works on all types. Remaining: `gg build --show-clones` structured report (JSON/human-readable) of all CoW materialization points with source line, type, reason. `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-05]

- **Struct clone ownership masking**: `emit_recursive_struct_clones` uses `gorget_string_clone` (view-preserving) which masks ownership bugs in code paths where struct values are passed to functions without move-zeroing the source. Example: `TaskStore__add` takes a Task by pointer, memcpys it into an array, but the caller's copy isn't zeroed — both copies share the title pointer. With `gorget_string_clone_to_owned` this manifests as double-free. Root cause: LIR doesn't emit move-zero after passing struct values to consuming functions. Fix: ensure consuming function calls zero the source, then upgrade all clone generators to `gorget_string_clone_to_owned`. [added: 2026-04-05]

- **Recursive/Custom elem_drop — 1 remaining fix**: C backend Option wrapping must CLONE for Recursive/Custom elements when payload is NOT Ptr (consuming methods like gorget_array_safe_pop). The fragile struct-name lookup may miss types. Option[Ref_T].unwrap() returns Ptr (CoW borrow) — clone fires at ownership boundaries. [updated: 2026-04-05]

- **LIR backend: Phase 3 — multi-file project support (gorget-arena)**: 0 C compilation errors, 0 linker errors, 0 C warnings. Phase 4 stdlib name mapping and cross-module type registration complete. [updated: 2026-03-21]

- **Extern module ABI — remaining structural whitelist**: `is_cstr_returning_fn` and `takes_cstr_for_str_param` deleted (all entries eliminated). `runtime_arg_by_ptr` is structural (collection/string self-deref + string clone/free) — cannot be removed without changing how the LIR represents method dispatch. See `docs/internals/extern-modules.md`. [updated: 2026-04-03]

- **Trait-bounded generic functions don't monomorphize**: `void print_sum[Summable T](T val)` — linker error. Core language feature gap. [added: 2026-03-23]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

- **LoweringContext per-function state isolation**: `LoweringContext` is a god struct shared across all function lowering. Per-function transients (`expected_type`, `current_throws_result_type`, `closure_param_type_hints`, `loop_stack`, `pattern_must_clone_strings`, etc.) leak across function boundaries when arm body lowering triggers monomorphization. Fix: split into `FunctionLoweringState` struct pushed/popped on a stack at function entry/exit. Module-wide state (`fn_sigs`, `type_registry`, `enum_variants`, `runtime_callees`) stays on LoweringContext. Unblocks: conditional match-pattern clone optimization (skip clone when scrutinee is dead in arm body). [added: 2026-04-05]

- **CoW map consolidation**: Replace 8 tracking maps (`cow_alias_sources`, `cow_alias_targets`, `cow_ptr_params`, `cow_collection_refs`, `ref_locals`, `owned_locals`, etc.) with unified `LocalOwnershipState` enum per local. ~40% conditional reduction. The reverse map (`cow_alias_targets`) becomes a derived query, not stored state. [added: 2026-04-05]

- **Flow-sensitive prescan**: Track which basic blocks reassign each name, not just function-wide. Reduces conservative clones when only one branch mutates. [added: 2026-04-05]

- **Mutation trait on method declarations**: Replace hardcoded `is_mutating_collection_method` name list with `mutates_self` flag on `BuiltinMethodDecl`. Derive the check from the protocol table instead of hardcoding method names. [added: 2026-04-05]

- **CoW: nested field mutation gap**: `s.v.push(x)` goes through `field_place_info` path in `methods.rs:1030` which skips `cow_before_mutation`. If `s.v[0]` previously created a `cow_collection_refs` entry keyed on a FieldLoad temp, the ref won't be cloned out before the push. Only affects resource-type elements (e.g., `Vector[Vector[int]]` inside a struct), not strings or primitives. The borrow checker only catches explicit `T &` refs, not implicit CoW borrows. Fix requires tracking FieldLoad provenance through to `cow_collection_refs` — non-trivial. [added: 2026-04-05]

- **MutationWhileBorrowed: extend to implicit CoW borrows**: The `is_ref_type` filter at `check_expr.rs:353` skips non-Ref-type origins. This means `auto x = vec.get(0).unwrap()` followed by `vec.push(y)` is not caught for non-Ref element types. The GIR CoW system handles this via `cow_collection_refs`, but extending the borrow checker to also catch it would add defense-in-depth. [added: 2026-04-05]

- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Borrow checker: reject multi-use `!` on strings**: After string unification, `!` on a string triggers real MoveZero (ownership transfer). Library code uses `!key` in loops — borrow checker should catch this as use-after-move. Currently `!` on named string locals is pragmatically skipped (no-op, matches pre-unification). [added: 2026-03-30]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **IndexLoad reference semantics**: `v[i]` / `.get()` returns `&T` (mutable borrow). Borrows propagate through fields and destructuring. `.clone()` for ownership. Subsumes the collection double-free fix. [updated: 2026-04-03]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]

- **Closure return type inference**: Multi-line closures with `return` typed as void. Need to propagate return type in typecheck.rs. [added: 2026-03-21]

- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

## Low

- **`char` type backend bugs**: `char as int` gives garbage, char `==`/`!=` uses `gorget_str_eq`. [added: 2026-03-21]

- **Self-host comparison (858 fixtures)**: Parser 761 matched / 3 mismatch / 94 crash. Remaining 3 mismatches all unfixable: null byte (chars.gg), float precision (fstring_format.gg), catch-do-block (error_catch_in_loop.gg). 94 crashes from unsupported AST nodes (traits/generics/meta). Resolver 35 matched / 729 mismatch / 94 crash — builtin/keyword DEF ID shift. [updated: 2026-03-30]

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
