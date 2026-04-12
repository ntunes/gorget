# TODO

## High

- **C backend Ptr(Str) auto-deref in CallExtern**: emit_call_extern.rs line ~2014 has a fallback that dereferences Ptr(Str) args to pass Str by value. Correct for runtime functions expecting Str by value (sqlite wrappers) but wrong when callee expects void*. Can't simply check LIR extern param type — LIR uses Ptr for struct-by-address too. Needs: either C-level param type annotation on externs, or distinguishing "real Ptr" from "by-address Ptr" in LIR. The `len()` free function case is fixed (resolved at IR level). [updated: 2026-04-10]

- **Cloneable trait + runtime clone counters**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-10]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

## Medium

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 6 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks, struct+enum init). All work correctly — this is cleanup, not a bug. [demoted from High: 2026-04-12]

- **LIR value/slot split loses GIR MoveZero for consuming args**: Runtime safety net covers `gorget_array_set` and `gorget_array_insert`. `gorget_map_put` can't use it (called internally by put_cloned/grow/rehash — zeroing corrupts source). `gorget_set_add` delegates to map_put. The dict[key].push() CoW sever fix handles the dict double-free case at the GIR level. Proper LIR fix: value→slot ownership propagation. [updated: 2026-04-12]

- **Borrow checker: reject multi-use `!` on strings**: `!` on strings now triggers real MoveZero (pragmatic skip removed). Borrow checker should catch use-after-move for `!key` in loops. [updated: 2026-04-08]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]

- **InlineC instruction — remaining 13**: 5 in for_loops.rs (typed key/value extraction needing C pointer cast), 8 in stmts/mod.rs (snapshot writes + assert formatting needing C globals/formatting). The 8 scalar iterator accessors (cap, states, order) now use proper CallExtern via runtime functions. [updated: 2026-04-06]

- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

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
