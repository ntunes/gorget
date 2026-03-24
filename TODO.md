# TODO

## High

- **C backend: collection drop — remaining gaps**: Phase 6 core + borrow checker restriction done (2026-03-22). Collection/string call temps auto-registered via `call_tracked`/`call_extern_tracked` (2026-03-24); field assignment drop-before-reassign works; collection field loads return Ptr(T). **Remaining:** (1) named collection variables passed by value to functions that store the buffer — no deep-clone on pass, shared buffer means callee and caller alias the same data; (2) consuming methods (`pop`/`remove`) still clone resource payloads; (3) user structs with Resource fields need recursive drop (auto-Recursive upgrade tags them but C backend doesn't yet emit field-by-field cleanup); (4) `needs_drop_for_temp` excludes Recursive — call result temps of user struct type aren't tracked. [added: 2026-03-14, updated: 2026-03-24]

- **String variable leak: `unregister_gorget_string_args` too coarse**: Named String variables (f-string results, concat results) passed to non-void-returning functions get unregistered from drop tracking → leaked. Root cause: `should_unregister_string_args` returns true for ANY non-void callee. Attempted fix (skip unregister for named locals) exposes a **LIR local type ordering fragility**: changing which temps are registered changes SSA variable creation order, which cascades into wrong types (int64_t↔void* swaps in generated C). This causes yaml/p2p test failures (5529 lines of C diff from a 4-line Rust change). **Prerequisite:** fix LIR local type assignment to be deterministic regardless of drop registration order. Then the named-local fix can be safely applied. [added: 2026-03-23, updated: 2026-03-24]

- **LIR local type ordering fragility**: Changing drop registration order (which temps get registered) causes cascading changes in SSA variable numbering, which leads to type mismatches (int64_t assigned where void* is needed) in the generated C. Discovered when attempting to fix `should_unregister_string_args`. The LIR lowering should produce stable local types regardless of GIR-level drop elaboration changes. [added: 2026-03-24]

- **LIR backend: Phase 3 — multi-file project support (gorget-arena)**: 0 C compilation errors, 0 linker errors, 0 C warnings. Phase 4 stdlib name mapping and cross-module type registration complete. [updated: 2026-03-21]


- **IndexLoad reference semantics (target design)**: `v[i]` should return a mutable borrow (`&T`), not a value. Auto-clone when assigned to an owned `T` variable; mutate-in-place when calling methods through the borrow. **Current state (2026-03-22):** LIR IndexLoad clones collection-type elements (Vector, Dict, Set, Str) via `gorget_*_clone()` and move+zeroes other resource types (Task, user structs). This is a pragmatic workaround — the clone happens unconditionally in LIR, not conditionally at the semantic/IR level. **Target:** implement proper reference-returning IndexLoad in the GIR layer, with auto-clone inserted by the semantic layer only when assigning `&T` to `T`. This enables zero-cost reads (e.g., `print(matrix[0].len())` without cloning) and in-place mutation (`matrix[0].push(4)`). Also unifies with Dict read semantics (currently Dict.get() clones resource-type values separately). Documented in language-design.md §24, language-reference.md §7.9, book ch.5. [added: 2026-03-22]

- **LIR backend: remaining clone-on-read for Dict resource-type values**: Phase 3b eliminates clones for Vector borrowing reads on resource-type elements. Dict.get() still clones resource-type values (GorgetArray/GorgetMap/GorgetString). **Dict Ref attempted and reverted (2026-03-21):** Extending Phase 3b to Dict via `Option__Ref_V` breaks the common write-back pattern (`get → modify → set`): `gorget_map_put` receives a Ptr (8 bytes) where it expects a full struct (32+ bytes) → buffer overread. Fix requires either: (a) deref Ptr back to value before set/put calls, (b) detect and skip Ref for Dict values that are written back, or (c) a fundamentally different approach. Subsumed by IndexLoad reference semantics above. [added: 2026-03-17, updated: 2026-03-22]

- **Self-host parser: 794/797 matched, 3 mismatched, 0 crashed** — Remaining 3: null byte in chars.gg (1), str/String source alias in dataframe_nulls.gg (1), float literal precision in fstring_format.gg (1). All unfixable at self-host level. [updated: 2026-03-21]

- **`char` type backend bugs**: `char as int` gives garbage values, and char comparison with `==`/`!=` generates `gorget_str_eq` calls. [added: 2026-03-21]

## Medium

- **Closure return type inference incomplete in semantic layer** — `infer_closure_return_type` in GIR lowering now walks block bodies for `return` statements (2026-03-21), but the semantic type checker's `check_block` only returns the tail expression type, not the type from explicit `return` statements inside closures. Multi-line closures with `return <value>` get type-checked as void and fail with "type mismatch: expected void, found int". Fix: propagate `current_return_type` for closure bodies in `typecheck.rs` and unify return expression types against it. [added: 2026-03-21]

- **`@[no_alloc]` function annotation** — Compiler error if a function body contains any allocating operation (string concatenation, collection construction, f-strings, etc.). Useful for hot loops and real-time code paths where implicit allocation must be prevented. Requires cataloguing which AST expressions/calls allocate. Could also support `@[no_alloc]` on blocks for finer granularity. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived** — A closure spawned after an await can capture a variable derived from a shared binding that is now stale. `check_spawn_closure_captures` checks `has_borrowed_origin` but doesn't intersect captured DefIds against `stale_shared_derived`. The spawned task silently uses pre-await data. Fix: in check_spawn_closure_captures, check if captured def_id is in stale_shared_derived and warn/error. [added: 2026-03-18]


- **Replace auto-borrow with explicit reference semantics** — **Phase 1 done (2026-03-20):** `const_params` field added to `LirFunction`; LIR lowering populates it from `GirType::Ptr` (bare borrow); C backend emits `const void*` for const params in both forward declarations and definitions. 843/844 integration tests pass (bench_basic pre-existing). **Phase 2 (const value propagation):** Track constness through intermediate values so the C compiler catches mutations through copies of const params. Not yet started. **Phase 3 blocked on collection reference semantics** (see below). [added: 2026-03-13, updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps** — Phase 6 + borrow checker restriction done (2026-03-22). Borrow checker rejects struct/variant/return storage of borrowed resource params. **Still leaking:** (1) collections passed by value to functions (unregistered from drop — borrow checker doesn't cover field assignment or method-call ownership transfer paths); (2) collections reassigned without drop-before-reassign. **Next step:** extend borrow checker to also reject field assignments (`self.field = borrowed_param`) and method calls that store borrowed params. Then remove the IR unregister workaround. [added: 2026-03-15, updated: 2026-03-22]

- **`meta is_pure(fn_name)` builtin** — Purity inference is now computed in Pass 5b½ (before borrow check, after resolve/typecheck) but `meta` evaluation happens earlier (Pass 0). Chicken-and-egg problem. Options: (1) move meta evaluation after borrow checking for `is_pure` only, (2) two-phase meta where purity queries are deferred, (3) expose purity only at IR/codegen level (current state). [added: 2026-03-14, updated: 2026-03-14]


- **If-expression `elif` branches: parser limitation**: The parser doesn't support `elif` in inline if-expressions (only `if cond: expr else: expr`). The IR lowering now handles elif branches if the parser passes them through. Workaround: use nested `else: if`: `if a == 1: 10 else: if a == 2: 20 else: 30`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock under full test suite**: The fixture runs fine standalone and in isolation but occasionally hangs (infinite CPU loop) when run as part of the full 670+ test suite. Timing-dependent — likely a contention issue in the coroutine runtime's shared counter lock release/reacquire path. Reproduce: `cargo test --test integration -- --test-threads=1` and wait for `shared_stress_yield`. [added: 2026-03-11]


- **Multi-line closures with return always typed as void**: `infer_closure_return_type` in `src/ir/lowering/closures.rs:664` hardcodes `Expr::Block(_) => UNIT_TYPE`. Multi-line closures like `(int x): int y = x + 1; return y * 2` fail type checking because the return type is inferred as void instead of int. Only single-expression closures and void (side-effect) closures work. Fix: walk the block's return statements to infer the actual return type. [added: 2026-03-11]


- **Inline `None()` without typed variable produces garbage**: Using bare `None()` in expressions without first binding to a typed `Option[T]` variable causes uninitialized variable warnings and garbage output in the C backend. Workaround: `Option[int] n = None()` then use `n`. [added: 2026-03-11]






- **C backend: `compute_type_overrides` should use TypeIds, not string manipulation**: The type override system in `src/backend/c/mod.rs` infers C types for GIR locals by string-matching formatted type names — stripping `*` suffixes, matching `"const "` prefixes, looking up struct fields by string name. This is fragile (e.g., the `dst.projections` bug where writing a float to a struct field through a borrow corrupted the borrow's type override to `"double"`). Refactor to track `TypeId`s from the GIR type registry throughout, only formatting to C strings at declaration time. [added: 2026-03-14]

- **C backend: uninitialized return variable in some functions**: At least one generated function has `_0` used uninitialized as a return value. The codegen should ensure all return locals are zero-initialized. Currently suppressed with `-Wuninitialized` not being fatal. [added: 2026-03-13]

- **Metal runtime: ObjC method signature for drawIndexedPrimitives with indirect buffer**: `drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:indirectBuffer:indirectBufferOffset:` not found by clang. Either wrong selector name or missing protocol cast on encoder. Could crash if called. Suppressed with `-Wno-objc-method-access`. [added: 2026-03-13]


- **Metal runtime: update deprecated APIs**: `sampleCount` → `rasterSampleCount`, `useResource:usage:` → `useResource:usage:stages:`, `useHeap:` → `useHeap:stages:`. Still functional but deprecated since macOS 13.0. [added: 2026-03-13]

- **`shared static` support**: `public static shared int counter = 0` — thread-safe module-level statics. Requires adding `SharedKind` field to `StaticDecl`, atomic/mutex global codegen in C backend (atomic globals, constructor-initialized mutexes), and wiring lock/unlock into `GlobalAssign` emission. Workaround: use explicit `Mutex[int]` or `Atomic[int]` as the static type. [added: 2026-03-10]

- **IR: computed callees silently return Unit** — Non-identifier, non-closure callees (e.g., `array[i](args)`) in `lowering/exprs/calls.rs:852` silently produce `Constant::Unit` instead of an error or actual call. [added: 2026-03-17]

- **IR: silent I64_TYPE fallbacks throughout lowering** — ~10 locations default to `I64_TYPE` when type resolution fails, with no diagnostic. Key sites: closure return inference (`closures.rs:597,604,652,665`), local type lookup (`exprs/mod.rs:180`), fn_sigs miss (`calls.rs:786`). Should at minimum `debug_assert!` or log. [added: 2026-03-17]


- **IR: shared AST visitor for closure analysis** — `collect_free_vars()` and `detect_mutations()` in `closures.rs:335-482` duplicate ~150 lines of identical tree-walking code. Extract shared visitor trait. [added: 2026-03-17]

- **IR: split `lower_module()` into named sub-passes** — Currently ~1,056 lines with ~15 phases inlined. Each phase should be a separate function. [added: 2026-03-17]

- **IR refactor: Continue `exprs/` split — Phase 4**: Phases 1-3 done (8 files, mod.rs 2,515 lines). Remaining: control-flow exprs, struct literals, field access — tightly coupled to `lower_expr_inner`. Diminishing returns. Revisit if mod.rs grows again. [updated: 2026-03-07]

- **IR refactor: Continue `LoweringContext` decomposition (Phase 3)**: Phases 1-2 done. 24 fields remain. fn_sigs has 116 uses — high churn for modest readability gain. Diminishing returns. [updated: 2026-03-07]

- **IR: Migrate key call sites to `try_map_ast_type`**: Phase 1 done — `try_map_ast_type() -> Option<TypeId>` added alongside existing `map_ast_type()`. Callers can now distinguish "genuinely void" from "unknown type." Remaining: convert critical call sites (function params, return types) to use `try_map_ast_type` with diagnostics. Low priority — existing pipeline ordering prevents bugs in practice. [updated: 2026-03-07]

- **IR: Enhance `validate.rs` further**: Phases 1-3 done + span_map consistency check added (Phase 4). 19 validator tests. Remaining: cross-block use-after-move (requires full dataflow framework — reaching definitions or gen/kill bit-vector analysis). Unreachable block check deferred — overlaps with dead block elimination and would fire on normal pre-optimization IR. [updated: 2026-03-07]

- **IR: Embed `DropStrategy` in Drop instruction**: Currently the backend reconstructs the strategy via `lookup_drop_strategy()`. Embedding it in `Instruction::Drop { place, strategy }` would make the instruction self-contained. ~20 match sites to update. Contract is now documented (types.rs, drops.rs) and validated (validate.rs). Low priority. [updated: 2026-03-07]

- **IR: Continue `generics/` split**: Phase 1 done (substitute.rs extracted, 331 lines). `mod.rs` still 1,236 lines. Remaining: monomorphization helpers (~188 lines) could move to `monomorphize.rs`, but they're tightly coupled to GenericCollector::emit(). Consider done unless mod.rs grows. [updated: 2026-03-07, from: IR code review]


- **Async Channel: rendezvous (capacity=0) poll_send ack**: Current poll_send for rendezvous channels treats deposit-into-slot as completion (no ack wait). True rendezvous semantics require two-phase state machine (deposit → wait for count==0). Low priority — buffered channels work correctly. [added: 2026-03-07]




- **`gg sim` aliasing model — Tree Borrows tracking**: sim.md identifies this as the "core differentiator from naive interpreters." Implement borrow-level tracking to catch aliasing violations. Add `--tree-borrows` (default) and `--strict-aliasing` flags (stricter stacked-borrows model). Currently sim detects UB (bounds, uninit, etc.) but does not track borrow validity. [added: 2026-03-05]

- **`gg.httpserver` V2 — non-blocking sockets + async handlers**: Reactor fd readiness (epoll/kqueue) and async socket ops done. `nb_read`/`nb_write`/`nb_accept` methods on Socket/ServerSocket. Coroutine yield kinds for socket I/O working. Remaining: wire async handlers into httpserver dispatch loop, connection keep-alive. [updated: 2026-03-08]



- **`gg.httpserver` V2 — keep-alive / connection reuse**: Current V1 sends `Connection: close` after every response. Future: parse `Connection: keep-alive` + `Keep-Alive: timeout=N`, loop parse→handle→write on the same socket, close on timeout or `Connection: close`. Blocked on async handler signatures (above). [added: 2026-03-03]

- **Module namespaces Phase 6 — remaining prefix cleanup**: File-based modules done (uuid, log, csv, cli, gfx, sqlite, json, xml, http, influx, yaml, toml, ssh, tensor). Remaining: synthetic stdlib modules in `src/stdlib.rs` (crypto, bytes, path etc.) still use `crypto_sha256()`, `bytes_from_str()` style. Lower priority since these are deeply wired into codegen dispatch tables. [added: 2026-02-26, updated: 2026-03-07]


- **`std.alloc`: per-thread scratch arenas**: `thread_scratch()` returns a thread-local `Arena` reset automatically between calls (double-buffered to allow two scratch frames per thread concurrently). Pattern from stb/handmade: zero-overhead scratch without explicit `with` blocks. [added: 2026-03-03]

- **Inline bounds follow-up — find new syntax for `outlives` to fully remove `where`**: The `where` keyword is now only used for `where a outlives b`. Options: (1) inline on the lifetime param `live(a outlives b)`, (2) a dedicated `outlives` section, (3) lifetime annotations on the param itself. Survey and decide before removing `where` entirely. [added: 2026-03-02]


- **Selective token hold across await (optimization)**: CFA could prove that an awaited task doesn't touch a given shared variable, allowing the token to be held across await instead of released. `with x:` auto-refresh now mitigates the ergonomic impact (§3.4 stale warnings suggest `with` pattern). True selective hold remains an optimization — eliminates unnecessary release/reacquire overhead. Requires transitive closure over spawn chains; conservative for opaque callables. [updated: 2026-03-07]



- **Async/await — `await` on vector-indexed tasks with multiple spawn functions**: Type-based await dispatch now works when exactly one function produces tasks of a given type. When multiple functions produce the same `Task__T` type (e.g., two functions both returning `int`), the type-based fallback can't disambiguate. Fix: embed a function dispatch pointer in the `Task__T` struct or use a tag field. [updated: 2026-03-07]

- **Self-hosting parser: 794/797 (99.6%) on LIR backend** — Exceeds GIR backend (595/596). 3 remaining: null byte, str alias, float precision — all unfixable at self-host level. [updated: 2026-03-21]

- **Self-hosting resolver: 797/797 (100%) on LIR backend** — COMPLETE. [updated: 2026-03-21]

- **Self-hosting type checker: 791/797 (99.2%) on LIR backend** — Exceeds GIR backend (595/595). 6 remaining: type var numbering (3), closure param inference (2), Gorget-more-correct (1). [updated: 2026-03-21]


- **`Into[T]` conversion trait**: Counterpart to `From[T]` requiring explicit type args (`value.into[Celsius]()`) or return-type inference. Adds complexity (equipping primitives, potential blanket impl pattern). [added: 2026-02-17]

- **`TryInto[T]` conversion trait**: Fallible counterpart to `Into[T]`, same complexity issues (explicit type args or return-type inference). Track alongside `Into[T]`. [added: 2026-02-18]



- **Extract serialization traits to `std.serialize` module**: When adding TOML/YAML serializers, move `Serializer` and `Serializable` traits to a shared `std.serialize` module. `std.json`, `std.toml`, `std.yaml` would each provide their own backend. [added: 2026-02-17]


- **Hot-reload: multi-file watch**: When a hot-reloadable program imports other modules, all imported .gg files should be watched for changes (currently only watches the main file). Need to pass import file list from loader to codegen. [added: 2026-02-16]

- **Hot-reload: state migration hooks**: Currently, State struct layout changes trigger full reinitialization via `init()`. Future: additive migration (fields added at end keep existing data), explicit migration hooks (`upgrade from v1 to v2`). [added: 2026-02-16]

- **Hot-reload: trait objects / closures in State**: Trait object vtable pointers and closure function pointers become invalid after dlclose. The `reload()` hook can reconstruct them, but compiler-assisted fixup would be better. [added: 2026-02-16]









- **Inconsistent function naming across synthetic stdlib modules**: File-based modules now use bare names (gg.http `get()`/`post()`, gg.yaml `parse()`/`stringify()`). Synthetic modules still use prefixed names: `crypto_sha256()`, `bytes_from_str()`, `path_join()`. Aligning these requires updating codegen dispatch tables in stdlib.rs — more invasive than file-based renames. [added: 2026-02-16, updated: 2026-03-07]




- **Struct destructuring in VarDecl/for-loop**: Tuple destructuring now works in VarDecl, for-loop, match, and comprehensions. Struct field destructuring (`auto Point { x, y } = point`) still not implemented — would need named-field pattern parsing + codegen. [from roadmap, added: 2026-02-16, updated: 2026-02-20]

- **Const generics**: Partially parsed but not validated or monomorphized. E.g., `struct Array[T, N: int]`. [from roadmap, added: 2026-02-16]

- **Smart pointers — remaining**: `Shared[T]` (Arc-pattern), `Weak[T]`, and `Mutex[T]` are DONE. Remaining: (1) `Box[Trait]` trait object generalization — add `SmartPtrKind` (Box/Rc/Arc/Weak) to `ResolvedType::TraitObject`, extend the `name.node == "Box"` check in `types.rs:367` to a set of known smart pointer names. Vtable dispatch is identical across wrappers; only construction/clone/drop differs. (2) Future: `Arc` vs `Rc` naming alignment if single-threaded variant ever needed. [from roadmap, added: 2026-02-16, updated: 2026-03-02]

- **SSH library enhancements**: Public key authentication (IdentityFile), host key verification against known_hosts, ProxyJump/ProxyCommand support from ssh_config. [added: 2026-02-15]





- **Demand-driven refinement for borrow analysis**: When `return_borrows_from` conservatively unions multiple branches (e.g., function returns from two branches with different parameter origins), this can cause false positives at specific call sites. Per-call-site re-analysis would only activate when the conservative summary causes a rejection, then trace the specific call arguments through the callee body. Currently zero false positives across 466 unit + 228 integration tests — implement when actual false-positive reports arise. [added: 2026-02-18]

- **`@guarded` annotation for opt-in self-referential structs**: If self-referential structs are ever needed, consider an explicit `@guarded` annotation that adds runtime scope-token checks to specific fields. Opt-in (not automatic) to preserve zero-cost default. Each guarded field would carry a scope token that invalidates when the source field is mutated. Requires: field-granularity mutation tracking, fat pointer layout for guarded fields, instrumentation of field writes. Philosophy: compile error by default for unsafe self-references; `@guarded` as explicit escape hatch with documented runtime cost. [added: 2026-02-18]

- **`std.regex` deferred features**: (1) `replace_with(self, str subject, Callable[Match, str] fn)` — callback replacement (requires C→Gorget closure call for user-defined replacement logic). (2) `named_groups(self) -> Dict[str, str]` — requires building a Gorget Dict from C. [added: 2026-02-19]

- **ECS `(int, T)` pair iteration / `items()` method**: `Iterable[int]` only yields entity IDs, forcing immediate `get(eid)` in every loop. An `items()` method yielding `(int, T)` tuples would eliminate boilerplate. Blocked: tuple return from generic equip methods is untested codegen territory. [added: 2026-02-22]

- **ECS iter() copies entire entity_ids vector**: `SparseSet[T].iter()` allocates a fresh `Vector[Entity]` and copies all entity IDs. O(n) allocation just to start iteration. Language limitation: `SparseSetIter` can't hold a reference (no lifetime-annotated struct fields). Could improve with index+length snapshot if struct references become available. [added: 2026-02-22]

- **gg.jsonpath Phase 2 — function argument auto-propagation**: `query_all(doc, "friends.#(age>30).name")` — chaining filter + key access in a single path. Currently filter returns matching objects but subsequent segments after filter aren't applied. Need to feed filter results back into the segment pipeline. [added: 2026-03-09]

- **gg.xpath — XPath-style queries for XmlNode**: Move `find`/`find_all` from `gg.xml` to `gg.xpath`. Add path query support consistent with `gg.jsonpath` interface. [added: 2026-03-09]

- **gorget-db — JSON document store**: MongoDB-lite REST API using gg.httpserver, gg.json, gg.jsonpath, std.signal, std.fs. POST/GET/DELETE/PUT/PATCH on `/db/{collection}/{id}` with query support. [added: 2026-03-09]

- **Borrow checker: bare param mutation via nested field method call** — `w.c.increment()` where `w` is a bare param and `increment` takes `&self` is not caught. Direct `c.increment()` IS caught. Root cause: `FieldAccess` returns `error_id` instead of the real field type → method resolution falls back to name-based lookup → `function_info` miss. Proper fix requires FieldAccess type inference (returning real field types), which has cascading effects on exhaustiveness checking and borrow origin computation. Needs careful rollout. [added: 2026-03-18, updated: 2026-03-18]

- **Type checker: FieldAccess returns error_id** — `Expr::FieldAccess` always returns `self.types.error_id` instead of the actual field type. This prevents downstream type checking from working correctly on field access chains (method resolution, exhaustiveness, type inference). Fix prototype worked but caused 6+ test regressions from newly-enforced checks. Needs phased rollout with fixture updates. [added: 2026-03-18]

- **Resolver: nested `Stmt::Item` orphans FunctionInfo** — `resolve_stmt` at line 1108 creates a fresh `ResolveContext` for nested function definitions. `FunctionInfo` entries created there are immediately dropped — the type checker never sees them. Fix requires threading `function_info`/`function_body_scopes` through `resolve_block` (29 call sites) and `resolve_stmt` (1 call site). Nested function definitions are rare, so low urgency. [added: 2026-03-17]

- **Resolver: `Expr::Block`/`Expr::Do` throwaway TypeTable** — Lines 1379/1385 pass `TypeTable::new()` to `resolve_block`. Types created inside expression blocks are discarded. Would need adding `types` to `resolve_expr` (84+ call sites). Expression blocks with type declarations are extremely rare. [added: 2026-03-17]

- **Resolver: double equip block scope creation** — Pass 1 creates an EquipBlock scope and defines method names. Pass 2 creates another EquipBlock scope for body resolution. Wasteful but functionally correct. [added: 2026-03-17]

- **Resolver: `validate_str_param_modes` for type aliases** — Only checks bare `Type::Primitive(Str)`, not `type MyStr = str`. Would need type resolution which isn't available in Pass 1. Low impact. [added: 2026-03-17]

- **Semantic: scope lookup performance (O(n) → O(1))** — Linear scope lookup could use index-based optimization. Needs profiling first to confirm it's a bottleneck. [added: 2026-03-17]

- **Semantic: type alias circular dependency detection** — No check for circular type aliases (`type A = B`, `type B = A`). Needs design. [added: 2026-03-17]

- **Semantic: conflicting trait method name detection** — No check when two traits with same-named methods are both implemented for a type. Needs design. [added: 2026-03-17]

- **Semantic: via delegation full validation** — Incomplete validation of `via` delegation. Needs deeper analysis. [added: 2026-03-17]

- **Negative test fixtures — remaining gaps**: 29 `*_error.gg` fixtures added (2026-03-15), plus 4 new diagnostics: `no_field` (Defined structs only), `not_a_function` (Variable/Const/Static), `break_outside_loop`, `unknown_directive` (item-level @attrs). `no_method` now enabled for types with inherent-only equip blocks. Still missing: `underivable_trait` (deferred — derive expansion runs before trait registry), `return_outside_function` (parse error only), `spawn sync_fn()` (not caught), `private_in_public` (warning only), `borrow_on_copy` (not caught). [updated: 2026-03-17]


## Low

- **Gorget Arena: deformVertexes AutoSprite/AutoSprite2** — DeformWave, DeformBulge, DeformMove are implemented (CPU vertex modification via glBufferSubData). AutoSprite/AutoSprite2 remain — they require camera-facing billboard orientation logic per vertex quad. [added: 2026-03-21]

- **Multi-file loader: direct synthetic module imports crash on macOS** — Importing `gg.gl` functions directly into `backend.gg` (`from gg.gl import gl_bind_buffer`) produces a binary that crashes on macOS (reads wrong value for GL function args). Workaround: route through `gpu_backend.gg` which already imports `gg.gl` successfully. Likely a scoping/symbol resolution issue in the multi-file synthetic module loader. [added: 2026-03-21]

- **Named/labeled arguments at call sites**: Allow `=` syntax for labeling arguments: `str.slice(from=2, to=5)`, `transfer(buf=!owned)`, `process(data=&vec)`. Pure sugar — desugars to positional args by matching label to parameter name. Sigils (`&`/`!`) go on the value side only (mandatory, as with positional calls); labels are bare names. Labeled args freely mixable with positional: `connect("localhost", port=8080)`. Parser needs: `IDENT '=' Expr` as an argument form; semantic check maps labels to positions and rejects duplicates/unknowns. [added: 2026-03-11]

- **GIR Phase 6 — LLVM backend**: Implement `src/backend/llvm/` to emit LLVM IR (text or bitcode) from GIR. Use `llc` or Rust `inkwell`/`llvm-sys` crate for compilation. Construct SSA via alloca+mem2reg pattern. Emit debug info from GIR source locations. Wire as `gg build --backend=llvm`. Removes C compiler dependency for native compilation. [added: 2026-03-05]

- **GIR Phase 7 — WASM backend**: Add WebAssembly support — either via LLVM backend with `--target=wasm32-wasi` (Option A, minimal code) or direct WASM emission (Option B, more control). Start with Option A, defer Option B if LLVM quality is sufficient. Wire as `gg build --backend=wasm`. [added: 2026-03-05]

- **GIR Phase 8 — GPU/compute backend**: Implement `@kernel` and `@parallel` annotation support in GIR lowering. Add validation pass for GPU constraints (no heap alloc, recursion, virtual dispatch). Implement SPIR-V emission (most portable GPU target). Generate host-side dispatch code (buffer setup, kernel launch, readback). Wire as `gg build --backend=gpu`. [added: 2026-03-05]

- **`gg sim` cross-target interpretation**: `gg sim run --target aarch64-unknown-gorget-elf` to catch byte-layout bugs by interpreting code as a different architecture/endianness. Test struct layout assumptions portably without recompiling. [added: 2026-03-05]

- **`gg sim` `cfg(sim)` gating**: Support `cfg(sim)` or `cfg(gg_sim)` conditional compilation to gate code that behaves differently under simulation or skip problematic tests. Allows tests to adapt to simulated environment constraints. [added: 2026-03-05]

- **`gg sim` data race detection**: Single-threaded deterministic model + weak memory exploration for detecting races in multi-threaded programs. Requires work-stealing executor + weak memory models. [added: 2026-03-05]


- **`uuid_parse(str) -> Result[UUID, str]`**: Parse UUID strings in the standard `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` format. `UUID` would be a struct `{ uint64_t hi; uint64_t lo; }` with `to_string()` and `eq()` equip. C backend via sscanf or manual hex parsing. [added: 2026-03-02]

- **`Heap[T]` max-heap variant**: Current `Heap[T]` is a min-heap. Add a max-heap variant (either a `MaxHeap[T]` type or a `Heap[T](reverse=true)` constructor parameter). [added: 2026-03-02]


- **Package management phase 2 (`gg update`, registry)**: Semver-aware resolution, central registry, `gg publish`, workspaces. [added: 2026-02-15]

- **Data-driven stdlib call dispatch**: Large match blocks in the GIR C backend (`src/backend/c/mod.rs:3397, 3534, 6543`) where each arm follows the same pattern (check name, emit C call). Replace with a table of (name, arity, C template). [added: 2026-02-16, updated: 2026-03-03]


- **`gg info` command**: show fields, methods, traits, memory layout for a type. [added: 2026-02-10]


- **Associated type validation**: Associated types are parsed but not validated or resolved in semantic analysis. [from roadmap, added: 2026-02-16]


- **`gg fmt` remaining polish**: All phases complete. Future: `from X import (a, b, c)` parenthesized import syntax for wrapping long `from` imports (requires parser change). [from roadmap, updated: 2026-03-08]

- **LSP server**: Language Server Protocol for IDE integration (completions, diagnostics, go-to-definition). [from roadmap, added: 2026-02-16]

- **`gg doc` (documentation generator)**: Generate HTML docs from doc comments. [from roadmap, added: 2026-02-16]

- **`--watch` mode**: `gg run --watch` and `gg test --watch` for recompile-and-rerun on file changes. [from roadmap, added: 2026-02-16]


- **Incremental compilation**: Only recompile changed modules. [from roadmap, added: 2026-02-16]


- **TOML DateTime structured type**: `DateTime(str)` currently stores unparsed text (documented). Future enhancement: add a structured `TomlDateTime` type with year/month/day fields. (`toml.gg:21, 378-427`) [added: 2026-02-16]




- **Serial port library (`std.io.serial`)**: `Port` struct, `.write()`, `.read_until()`, timeout support. C backend via termios/POSIX. [added: 2026-02-14]

- **File system utilities (`std.io.fs`)**: temp directory management, content assertions. [added: 2026-02-14]

- **`--parallel` test execution: V2 improvements**: Current: CLI-level process parallelism with stride-based distribution. Future: shared-memory approach for better load balancing, progress reporting across workers. [updated: 2026-03-10]

- **`directive test_suite "name"`**: suite naming for report grouping. [added: 2026-02-14]

- **`with` clause on suite directive**: `directive test_suite "name" with Resource(...) as r:` for suite-level resource management. [added: 2026-02-14]


- **HTML report: search, filter, expand-all**: for large test suites — filter by name/status, search within traces, expand/collapse all nodes. [added: 2026-02-14]

- **HTML report: source file/line context**: trace nodes show source text but no file path or line number. [added: 2026-02-14]

- **HTML report: timing breakdown per function**: call/return pairs contain the data but the report doesn't surface it. [added: 2026-02-14]

- **`directive implicit-auto`**: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Trade-off: more Pythonic but typos silently create new variables. [added: 2026-02-11]


- **`c_runtime.rs` monolithic string constant**: ~5,200-line single string constant is hard to navigate and edit. Split into separate const blocks or `.c` files. [added: 2026-02-16, updated: 2026-02-25]

## Best Effort

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS; type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Default trait: enum derive**: `@derive(Default)` for enums — which variant is default? Could use first variant, `@default` attribute, or unit-only constraint. Currently not derivable. [added: 2026-02-17]

- **Default trait: replace TryCapture memset with Default call**: TryCapture codegen uses `memset(&var, 0, sizeof(var))` for zero-default on error. Could call `Default_for_Type__default()` instead for proper initialization (e.g., empty strings vs null pointers). Needs type info at the TryCapture site. [added: 2026-02-17]



- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
