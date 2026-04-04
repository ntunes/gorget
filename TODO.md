# TODO

## High

- **118 leaks in yaml_parse (14KB)**: Down from 374 (-68%). Root cause fully characterized. Four coordinated changes needed: (1) VarDecl clone type → GorgetString, (2) deferred drop for reassignment, (3) saved_old lifecycle, (4) owned_locals tracking. Each validated individually but combining them increases leaks to 162 due to extra intermediate copies in the struct init path. The GIR creates redundant clones when string locals are properly typed — needs holistic redesign of string ownership flow through VarDecl + struct init + ensure_owned_string. [updated: 2026-04-04]


- **CoW Phase 1f: DONE — liveness analysis implemented**: Full-function span-based reverse walk with branch union and two-pass loops. is_last_use_at(name, span) provides precise per-use last-use queries. Integrated at push/put and struct-init call sites. 905/905 tests pass. Phase 2a Step 5 (unified Type__drop) already partially done (emit_type_drop_fns exists, Type__drop called at scope exit). [updated: 2026-04-04]

- **Explicit clone roadmap (Phase 2 remaining)**: `.clone()` works on all types. `directive explicit-clone` to be deprecated (incompatible with CoW). Remaining: runtime clone counters for observability (`gg run --clone-stats`), `Cloneable` trait. [updated: 2026-04-01]

- **CoW borrow semantics — 35 tests remaining**: All 6 materialization points implemented (assign, mutating method, struct init, push/put, return, move). VarDecl borrow propagation active. cow_borrow_basic fixture passes. 35 integration tests fail from: (1) consuming methods misclassified as borrowing (heap pop/peek), (2) code paths passing borrows through function calls without materialization, (3) library code patterns that need analysis. [updated: 2026-04-04]

- **Recursive/Custom elem_drop — 2 remaining fixes**: (1) Option[Ref_T].unwrap() must auto-clone Ptr→T. (2) C backend Option wrapping must CLONE for Recursive/Custom elements. Both needed for full self-cleaning collections. [updated: 2026-03-28]

- **LIR backend: Phase 3 — multi-file project support (gorget-arena)**: 0 C compilation errors, 0 linker errors, 0 C warnings. Phase 4 stdlib name mapping and cross-module type registration complete. [updated: 2026-03-21]

- **Extern module ABI — remaining structural whitelist**: `is_cstr_returning_fn` and `takes_cstr_for_str_param` deleted (all entries eliminated). `runtime_arg_by_ptr` is structural (collection/string self-deref + string clone/free) — cannot be removed without changing how the LIR represents method dispatch. See `docs/internals/extern-modules.md`. [updated: 2026-04-03]

- **Trait-bounded generic functions don't monomorphize**: `void print_sum[Summable T](T val)` — linker error. Core language feature gap. [added: 2026-03-23]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

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
