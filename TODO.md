# TODO

## High


- **Cloneable trait + runtime clone counters**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-10]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 6 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks, struct+enum init). All work correctly — this is cleanup, not a bug. [demoted from High: 2026-04-12]

- **LIR value/slot split loses GIR MoveZero for consuming args**: Fixed. GIR lowering marks consuming call args as `Operand::Move` (Rust-style); LIR emits generic post-call zeroing. Hardcoded function-name fallback removed — `ensure_owned_at_consuming_arg` now correctly clones non-last-use args (including Ptr-typed named locals from `.get().unwrap()` chains). [updated: 2026-04-13]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Borrow checker: reject multi-use `!` on strings**: `!` on strings now triggers real MoveZero (pragmatic skip removed). Borrow checker should catch use-after-move for `!key` in loops. [updated: 2026-04-08]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Remove GIR MoveZero emissions for borrow-wrapped call args**: C runtime safety nets and `arg_owners` removed. GIR MoveZero retained for args behind borrow ptrs (field loads, MutPtr params) — the LIR `emit_post_call_zeros` only reaches direct `Operand::Move` args. Removing these requires either flattening the borrow indirection or a drop elaboration pass. [updated: 2026-04-13]

- **Drop elaboration — remaining 24 Memsets**: V7 reduced to 24 across 17 fixtures (from 872). These are genuinely necessary: IndexLoad element zeroing (zeroing inside collection data arrays after move-out) and projected Deref/Field MoveZero (field-level ownership transfer through pointers). Could be eliminated with: (1) element drop flags on collections, (2) `MoveField { slot, field }` instruction. Low priority — these are rare hot-path operations. [updated: 2026-04-14]

- **LLVM backend test results (2026-04-13, 831 tested / 913 total)**:
  - PASS: 669 (80.5% of tested); file_io fixed 2026-04-14 (gorget_file_read_all ABI mismatch)
  - FAIL: 40 (output mismatch — see categories below)
  - CRASH: 73 (segfault/abort — mostly dataframe, p2p, toml, yaml, xml, httpserver, threads)
  - BUILD_FAIL: 47 (LLVM IR/linker errors — see categories below)
  - SKIP: 82 (C backend also fails — error programs, benches, etc.)
  - Build fail categories: 17x gorget_task_group_submit undefined, 10x LLC type mismatch (ptr/struct type confusion), 9x LLC forward-ref / undefined-value, 9x other undefined refs (__adapt_double, gorget_reactor_sleep_seconds, etc.), 2x TLS runtime missing
  - Fail categories: 8x tensor (float stored as i64?), 6x leak/alloc tracking, 4x CoW/Shared issues, 3x result/coroutine combinators, 3x process/socket (networking), 5x drop/destructor ordering, 5x test framework divergence, others
  - Key bug patterns: float fields in structs read as 0.0 (shared_float, tensor_*), memory not being freed (stress_alloc_*, leak_*), drop order wrong (drop_collections, drop_struct_collection_fields), Shared[T] write not visible (shared_struct/weak), __adapt_double / gorget_task_group_submit missing from LLVM wrapper generation [updated: 2026-04-14]
  - Root cause pattern: C runtime fns returning GorgetString directly but LLVM IR expects Result — gorget_file_read_all fixed; may affect other fns [added: 2026-04-14]

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **Self-host LIR backend**: 3,906 lines across 4 files. hello.gg compiles+runs end-to-end through self-host pipeline. 0/26 testable fixtures pass C compilation (all produce C output but fail cc). Main blockers: (1) void-return functions assigned to values, (2) value type gaps from ICallExtern return types, (3) SSA value ID gaps from block param numbering, (4) Ptr-to-struct return type coercion. C runtime embedding still manual (prepend extracted runtime header). [updated: 2026-04-14]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]
