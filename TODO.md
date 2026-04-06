# TODO

## High

- **Leak reduction — 2 remaining patterns**: Strings, Dicts, Closures, Vectors all leak-free (5/6 stress tests pass). Remaining: (1) **Option/enum scope-exit drop doesn't free resource payloads** — `Option[Vector[int]]` leaks 128 bytes per `Some` (the inner Vector is not freed). Enum types have `drop: None`. (2) **Collection reassignment skips drop** — `v = !new_v` doesn't free old `v`. Fix blocked on self-referential detection (naively dropping before assign causes UAF for `v = v.slice(...)`). [updated: 2026-04-06]

- **Clone observability + Cloneable trait**: `.clone()` works on all types. `gg build --show-clones` done. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-05]

- **Struct clone ownership masking**: `emit_recursive_struct_clones` uses `gorget_string_clone` (view-preserving) which masks ownership bugs in code paths where struct values are passed to functions without move-zeroing the source. Example: `TaskStore__add` takes a Task by pointer, memcpys it into an array, but the caller's copy isn't zeroed — both copies share the title pointer. With `gorget_string_clone_to_owned` this manifests as double-free. Root cause: LIR doesn't emit move-zero after passing struct values to consuming functions. Fix: ensure consuming function calls zero the source, then upgrade all clone generators to `gorget_string_clone_to_owned`. [added: 2026-04-05]

- **Recursive/Custom elem_drop — 1 remaining fix**: C backend Option wrapping must CLONE for Recursive/Custom elements when payload is NOT Ptr (consuming methods like gorget_array_safe_pop). The fragile struct-name lookup may miss types. Option[Ref_T].unwrap() returns Ptr (CoW borrow) — clone fires at ownership boundaries. [updated: 2026-04-05]

- **Extern module ABI — remaining structural whitelist**: `is_cstr_returning_fn` and `takes_cstr_for_str_param` deleted (all entries eliminated). `runtime_arg_by_ptr` is structural (collection/string self-deref + string clone/free) — cannot be removed without changing how the LIR represents method dispatch. See `docs/internals/extern-modules.md`. [updated: 2026-04-03]

- **Trait-bounded generic functions don't monomorphize**: `void print_sum[Summable T](T val)` — linker error. Core language feature gap. [added: 2026-03-23]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium


- **Flow-sensitive prescan**: Track which basic blocks reassign each name, not just function-wide. Reduces conservative clones when only one branch mutates. [added: 2026-04-05]

- **Borrow checker: early return doesn't reset move state**: INVESTIGATED — the divergent-branch filtering in `merge_branch_states()` already works correctly. Moves in branches that return/throw/break are excluded from the join-point merge. Added regression test `move_in_divergent_branch_ok`. The self-host `scope.gg` defensive clones may have been needed before the StringView removal. Review if they can now be removed. [updated: 2026-04-06]


- **CoW: nested field mutation gap**: `s.v.push(x)` goes through `field_place_info` path in `methods.rs:1030` which skips `cow_before_mutation`. If `s.v[0]` previously created a `cow_collection_refs` entry keyed on a FieldLoad temp, the ref won't be cloned out before the push. Only affects resource-type elements (e.g., `Vector[Vector[int]]` inside a struct), not strings or primitives. The borrow checker only catches explicit `T &` refs, not implicit CoW borrows. Fix requires tracking FieldLoad provenance through to `cow_collection_refs` — non-trivial. [added: 2026-04-05]

- **MutationWhileBorrowed: extend to implicit CoW borrows**: The `is_ref_type` filter at `check_expr.rs:353` skips non-Ref-type origins. This means `auto x = vec.get(0).unwrap()` followed by `vec.push(y)` is not caught for non-Ref element types. The GIR CoW system handles this via `cow_collection_refs`, but extending the borrow checker to also catch it would add defense-in-depth. [added: 2026-04-05]

- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Borrow checker: reject multi-use `!` on strings**: After string unification, `!` on a string triggers real MoveZero (ownership transfer). Library code uses `!key` in loops — borrow checker should catch this as use-after-move. Currently `!` on named string locals is pragmatically skipped (no-op, matches pre-unification). [added: 2026-03-30]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **IndexLoad reference semantics**: `v[i]` / `.get()` returns `&T` (mutable borrow). Borrows propagate through fields and destructuring. `.clone()` for ownership. Subsumes the collection double-free fix. [updated: 2026-04-03]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]

- **DSE may leak droppable values** (`ir/transforms/optimize.rs`): Dead store elimination removes `_1 = Copy(resource_local)` when `_1` is overwritten later, but the first value may need dropping. Investigated: DSE runs post-drop-insertion so scope-exit drops already cover this. Added safety comment. Still worth auditing for edge cases where DropElaborator doesn't insert intermediate drops at overwrites. [updated: 2026-04-06]

- **Hardcoded type size database — remaining**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]

- **InlineC instruction — remaining 13**: 5 in for_loops.rs (typed key/value extraction needing C pointer cast), 8 in stmts/mod.rs (snapshot writes + assert formatting needing C globals/formatting). The 8 scalar iterator accessors (cap, states, order) now use proper CallExtern via runtime functions. [updated: 2026-04-06]

- **Closure return type inference**: Multi-line closures with `return` typed as void (`Expr::Block(_) => UNIT_TYPE` hardcoded). Need to propagate return type in typecheck.rs. [added: 2026-03-21]

- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

## Low

- **`char` type backend bugs**: `char as int` gives garbage, char `==`/`!=` uses `gorget_str_eq`. [added: 2026-03-21]

- **Self-host comparison — stale numbers**: Last measured at 797 fixtures (2026-03-21): parser 99.6% (3 unfixable mismatches), resolver 100%, type checker 99.2%. Needs re-measurement at current fixture count (~890). [updated: 2026-04-06]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]
