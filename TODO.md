# TODO

## High


- **Cloneable trait + runtime clone counters**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-10]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

- **C backend: migrate val_types to func.value_types**: Shared `compute_module_value_types()` runs after LIR optimization and populates `func.value_types`. The LLVM backend already reads from it. The C backend still uses its own single-pass `infer_inst_type` because its multi-phase fixups (guard accessor inference, CallExtern→SlotStore mismatch, cross-type map combinator) depend on `ptr_pointee` context computed in the same pass. Next step: seed the C backend's val_types from `func.value_types` and reduce the fixup phases. [updated: 2026-04-14]

- **LIR value origin metadata — enable Store/SlotStore/Call lifts**: The C backend maintains 5 origin bitmaps (`str_lit_vals`, `null_vals`, `cstr_vals`, `ptr_pointee`, `func_addr_targets`) beyond type info. These track value provenance needed for ~37 emit-decision sites. The type metadata (`func.value_types`) is now shared; origin metadata remains backend-local. Fix: attach origin tags to LIR values (e.g. `StrLit` → string-literal flag, `NullPtr` → null flag, `FuncAddr` → FuncId). Unblocks lifting Store routing (~50 lines), SlotStore string/cstr coercion (~22 lines), and Call/CallPtr ABI coercion (~100 lines). [updated: 2026-04-14]

- **Decompose emit_call_extern.rs (~1,850 lines)**: Tier 1-3 lifts complete — ~490 lines of inline expansion removed. Remaining: HOF inlining (map/filter/each/fold ~590 lines), printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-04-15]

## Medium

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 6 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks, struct+enum init). All work correctly — this is cleanup, not a bug. [demoted from High: 2026-04-12]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Borrow checker: reject multi-use `!` on strings**: `!` on strings now triggers real MoveZero (pragmatic skip removed). Borrow checker should catch use-after-move for `!key` in loops. [updated: 2026-04-08]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

- **LLVM backend test results (2026-04-15, 815 tested / 913 total)**:
  - PASS: 730 (89.6% of tested); up from 669 (80.5%) start of 2026-04-14
  - FAIL: 36 (output mismatch — see categories below)
  - CRASH: 40 (segfault/abort/timeout)
  - BUILD_FAIL: 9 (LLC type mismatch / forward-ref / missing symbols)
  - SKIP: 108 (C backend also fails — error programs, benches, etc.)
  - Remaining BUILD_FAIL (9): 4x LLC forward-ref type mismatch (phi i64 vs i32), 1x conv_stdlib ptr vs i64, 1x shared_iterator_invalidation ptr vs GorgetArray, 1x print_trait_object struct init, 1x string_enum_variants i8 vs i64, 1x sqlite undefined ref
  - Remaining CRASH (40): 13x p2p (signal 11), 3x httpserver, 2x crypto, 3x drop/box (signal 6 — double free from ptr-slot free(stack_addr)), 2x leak (signal 6), toml_stringify, serializable, deserializable, socket_connect, sync_condvar, thread_mutex, test_hashset_all, test_set_string, etc.
  - Remaining FAIL (36): ~7x json/xml/serialize (int values read as 0 from enum payload — ptr-slot memcpy overflow), ~5x leak/stress_alloc (leaked=true), ~3x drop ordering, ~3x result combinators, ~3x process/socket, ~2x ecs (field reads 0), ~2x coroutine, ~11x others
  - **Root cause: struct inter-field alignment mismatch** — non-union structs containing aggregate fields (e.g., `Option__Json = {i32, Json}`) get different layout in C vs LLVM. C pads i32 tag to 8 before Json (which has 8-byte alignment from its union), giving offset 8. LLVM puts Json at offset 4 (because `%Json = {i32, i32, [N x i8]}` has apparent alignment 4). Fix needs `computed_c_align` on StructDef so the LLVM backend can insert correct padding. Naive "all aggregates align to 8" breaks traits/vtables.
  - **Double-free crashes** (drop_raii, box_heap): `free(slot_addr)` frees the stack address instead of the heap pointer. Ptr-typed slots need `load ptr` before `free`.
  - **Ptr-typed LIR slots holding struct data**: GIR types `Ptr(Named("X"))` → `alloca ptr` (8 bytes) but receives struct memcpy. Fix in GIR: locals that hold copies should be typed as the struct, not as Ptr.

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **Self-host LIR backend**: ~4,200 lines across 4 files. 205/923 fixtures compile through self-host LIR codegen pipeline (up from 0/26). Codegen fixes: void-return suppression, I64 fallback for unknown CallExtern types, struct constructor type inference, aggregate string comparison (gorget_str_eq/cmp), mono wrapper typedefs, monomorphized method→runtime name mapping. Remaining blockers: (1) self-host LIR lowerer uses wrong function names (int64_t__get instead of gorget_array_get — ~165 linker errors), (2) gorget_dict_new called with wrong arg count (~33 errors), (3) Option/Result variant construction doesn't match parent enum type, (4) SSA value gaps in closure bodies. C runtime embedding still manual. [updated: 2026-04-15]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]
