# TODO

## High

- **Leak reduction — status**: Error path fixed (emit_enum_init_owned). StringView call results upgraded to GorgetString in call_tracked. Match pattern strings clone-on-extract. Struct field strings now Ptr(GorgetString) references (matching collection pattern). LIR FieldLoad cap/alloc zeroing blocks (lir/lower.rs:1631-1656, 2097-2123) are now dead code for GorgetString fields — `is_str_dst` matches GorgetStringView but field loads produce Ptr(GorgetString). Safe to remove once verified no other path produces StringView dst. [updated: 2026-04-05]


- **CoW Phase 1f: DONE — liveness analysis implemented**: Full-function span-based reverse walk with branch union and two-pass loops. is_last_use_at(name, span) provides precise per-use last-use queries. Integrated at push/put and struct-init call sites. 905/905 tests pass. Phase 2a Step 5 (unified Type__drop) already partially done (emit_type_drop_fns exists, Type__drop called at scope exit). [updated: 2026-04-04]

- **Explicit clone roadmap (Phase 2 remaining)**: `.clone()` works on all types. `directive explicit-clone` to be deprecated (incompatible with CoW). Remaining: runtime clone counters for observability (`gg run --clone-stats`), `Cloneable` trait. [updated: 2026-04-01]

- **StringView removal — the ONE fix for all remaining issues**: Unify StringView + GorgetString into a single String type. This eliminates: void* slot mismatch (leaks), provenance cap/alloc zeroing (csv_basic), ensure_owned_string workaround, deferred-drop complexity. Attempted: type unification alone causes 20 regressions because ensure_owned_string and other code paths depend on the type distinction. Needs coordinated update of ALL StringView consumers in one commit. [updated: 2026-04-04]

- **Recursive/Custom elem_drop — 1 remaining fix**: Option[Ref_T].unwrap() auto-clone DONE. Remaining: C backend Option wrapping must CLONE for Recursive/Custom elements when payload is NOT Ptr (non-borrow collection reads like gorget_array_safe_pop). The fragile struct-name lookup (c_lir:5080-5089) may miss types. [updated: 2026-04-05]

- **LIR backend: Phase 3 — multi-file project support (gorget-arena)**: 0 C compilation errors, 0 linker errors, 0 C warnings. Phase 4 stdlib name mapping and cross-module type registration complete. [updated: 2026-03-21]

- **Extern module ABI — remaining structural whitelist**: `is_cstr_returning_fn` and `takes_cstr_for_str_param` deleted (all entries eliminated). `runtime_arg_by_ptr` is structural (collection/string self-deref + string clone/free) — cannot be removed without changing how the LIR represents method dispatch. See `docs/internals/extern-modules.md`. [updated: 2026-04-03]

- **Trait-bounded generic functions don't monomorphize**: `void print_sum[Summable T](T val)` — linker error. Core language feature gap. [added: 2026-03-23]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

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
