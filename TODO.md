# TODO

## High

(No high-priority items at this time.)

## Medium

- **IR refactor: Continue `exprs/` split — Phase 4**: Phases 1-3 done (8 files, mod.rs 2,515 lines). Remaining: control-flow exprs, struct literals, field access — tightly coupled to `lower_expr_inner`. Diminishing returns. Revisit if mod.rs grows again. [updated: 2026-03-07]

- **IR refactor: Continue `LoweringContext` decomposition (Phase 3)**: Phases 1-2 done. 24 fields remain. fn_sigs has 116 uses — high churn for modest readability gain. Diminishing returns. [updated: 2026-03-07]

- **IR: Migrate key call sites to `try_map_ast_type`**: Phase 1 done — `try_map_ast_type() -> Option<TypeId>` added alongside existing `map_ast_type()`. Callers can now distinguish "genuinely void" from "unknown type." Remaining: convert critical call sites (function params, return types) to use `try_map_ast_type` with diagnostics. Low priority — existing pipeline ordering prevents bugs in practice. [updated: 2026-03-07]

- **IR: Enhance `validate.rs` further**: Phases 1-3 done (StructInit/EnumInit field counts, Drop on non-droppable, local TypeId validity, drop metadata consistency, return type _0 consistency, intra-block use-after-move detection). Remaining: cross-block use-after-move (requires full dataflow framework — reaching definitions or gen/kill bit-vector analysis). Unreachable block check deferred — overlaps with dead block elimination and would fire on normal pre-optimization IR. [updated: 2026-03-07]

- **IR: Embed `DropStrategy` in Drop instruction**: Currently the backend reconstructs the strategy via `lookup_drop_strategy()`. Embedding it in `Instruction::Drop { place, strategy }` would make the instruction self-contained. ~20 match sites to update. Contract is now documented (types.rs, drops.rs) and validated (validate.rs). Low priority. [updated: 2026-03-07]

- **IR: Continue `generics/` split**: Phase 1 done (substitute.rs extracted, 331 lines). `mod.rs` still 1,236 lines. Remaining: monomorphization helpers (~188 lines) could move to `monomorphize.rs`, but they're tightly coupled to GenericCollector::emit(). Consider done unless mod.rs grows. [updated: 2026-03-07, from: IR code review]

- **Async `.lock()` / `.read()` / `.write()` for explicit Mutex/RwLock**: Currently these are synchronous (`pthread_mutex_lock`/`pthread_rwlock_rdlock`), blocking the OS thread. In async code on the M:N scheduler, this ties up a worker thread under contention. Should use trylock + waker-queue protocol: try to acquire, if contended register task's waker on the sync primitive's wait queue and return Pending, wake one waiter on guard drop. The `shared` keyword path manages this internally, but explicit `Mutex[T]` in async functions needs it for correct M:N behavior. Requires: waker queue field on `gorget_mutex_t`/`gorget_rwlock_t`, async-aware lock methods that return `Future[Guard[T]]`, integration with executor's poll loop. [added: 2026-03-06]

- **C backend: emit typed constant literals**: When emitting `Constant::I64(v)` in C, use `(int64_t)(v)` or `vLL` suffix instead of bare integer literal. Currently bare `-5` is `int` in C, which breaks `printf("%lld", -5)` format specifiers. Blocks full re-propagation of constants into instruction operands. [added: 2026-03-07]

- **GIR Phase 5 — Copy propagation**: Constant folding and drop elision are DONE. Copy propagation (`_N = Copy(_M)` → replace uses of `_N` with `_M`) requires SSA form or reaching-definitions analysis to be safe — GIR's non-SSA form means sources can be reassigned in loops or modified through pointers (BorrowMut). Attempted and reverted: even "source never written to" isn't sufficient due to pointer aliasing. Prerequisite: either convert GIR to SSA (phi nodes) or implement per-block dominator-aware copy prop. [added: 2026-03-05, updated: 2026-03-07]

- **`gg sim` aliasing model — Tree Borrows tracking**: sim.md identifies this as the "core differentiator from naive interpreters." Implement borrow-level tracking to catch aliasing violations. Add `--tree-borrows` (default) and `--strict-aliasing` flags (stricter stacked-borrows model). Currently sim detects UB (bounds, uninit, etc.) but does not track borrow validity. [added: 2026-03-05]

- **`gg.httpserver` V2 — non-blocking sockets + async handlers**: Blocked on `GorgetSocket` having no `poll`/`epoll`/`kqueue` integration — reads and writes block the calling thread. Needs `gorget_socket_set_nonblocking()` + fd registration with the existing reactor (epoll fd on Linux, kqueue on macOS), and a `GorgetWaker` protocol extension for readable/writable events. API impact: handler type becomes `async Callable[HttpServerResponse(HttpRequest)]` — one-word change for users. [added: 2026-03-03]



- **`gg.httpserver` V2 — keep-alive / connection reuse**: Current V1 sends `Connection: close` after every response. Future: parse `Connection: keep-alive` + `Keep-Alive: timeout=N`, loop parse→handle→write on the same socket, close on timeout or `Connection: close`. Blocked on async handler signatures (above). [added: 2026-03-03]

- **Module namespaces Phase 6 — lib prefix cleanup**: Compiler phases 1–5 are all done. Phase 6: remove manual C-style prefixes from all 22 library files (e.g., `csv_parse_field` → `parse_field` in `lib/gg/csv.gg`). Add `private` to internal helpers. Update all import statements and test fixtures. Start with smallest modules (gg.uuid, gg.log), validate pattern, then tackle yaml.gg. [added: 2026-02-26, updated: 2026-03-04]


- **`std.alloc`: per-thread scratch arenas**: `thread_scratch()` returns a thread-local `Arena` reset automatically between calls (double-buffered to allow two scratch frames per thread concurrently). Pattern from stb/handmade: zero-overhead scratch without explicit `with` blocks. [added: 2026-03-03]

- **Inline bounds follow-up — find new syntax for `outlives` to fully remove `where`**: The `where` keyword is now only used for `where a outlives b`. Options: (1) inline on the lifetime param `live(a outlives b)`, (2) a dedicated `outlives` section, (3) lifetime annotations on the param itself. Survey and decide before removing `where` entirely. [added: 2026-03-02]

- **M:N Scheduler Phase 6 (blocking thread pool + I/O integration)**: Phases 1-5 are DONE (executor pool, coroutine state machines, cooperative yield). Phase 5.5 DONE: work-stealing in blocking waits (TaskGroup.join, Task.await, spawn_drop) prevents pool starvation for nested spawn patterns. Remaining: **Phase 6** — blocking pool: when async code calls truly blocking operations (file I/O, network, sleep), they must not monopolize a worker thread. Approach: dedicated expandable "blocking pool" thread + reactor integration for timers. `gorget_reactor_register_timer` already exists in `REACTOR_RUNTIME`. `sleep(n)` should return a future that registers a timer and yields. [updated: 2026-03-06]

- **Selective token hold across await (optimization)**: CFA could prove that an awaited task doesn't touch a given shared variable, allowing the token to be held across await instead of released. This would eliminate stale-condition warnings for disjoint shared state. Requires transitive closure over spawn chains; must conservatively release for opaque callables/indirection. Deadlock risk on false negatives makes this a "nice to have" — current release-on-await + stale warning is safe and the user fix is one line (re-read after await). Implement only if stale warnings become noisy in practice. [added: 2026-03-06]

- **Coroutine codegen — collection method calls**: Collection method calls (Vector push/get, Dict put/get, etc.) in coroutine poll functions are emitted as raw `fn_name(args)` without the collection method rewriting that the normal path does via `try_rewrite_collection_method`/`emit_collection_method_call`. Functions that would hit this fall back to blocking path (correct but no cooperative yield). Fix requires adapting the collection method dispatch to work with poll-context place formatting (`f->_N` instead of `_N`). [added: 2026-03-06]

- **Async/await — `await` on vector-indexed tasks**: `await tasks[j]` or `tasks[j].await()` currently fails (codegen emits the Task value directly instead of calling `__gorget_await_fn`). The spawn-origin tracking in `spawn_result_locals` only covers direct task locals from `spawn` expressions, not collections. Fix: add an `await` function pointer to the `Task__T` struct (generic dispatch via void*), or use a type-level reverse-lookup when exactly one spawned function produces a given return type. Needed for concurrent-spawn patterns (spawn N tasks, collect in vector, await all). [added: 2026-03-06]

- **Self-hosting parser: 2 remaining comparison mismatches (235/237)**: (1) `math_constants.gg` — C's `%g` float formatting outputs `1e+06` / `3.14159e+06` while Rust's `Display` outputs `1000000.0` / `3141592.0`. Would need custom float-to-string in format.gg. (2) `name_first.gg` — `directive name-first` enables a completely different parsing mode (identifier-first declarations like `x int` instead of `int x`). The self-hosting parser only handles the default type-first mode. [added: 2026-02-21, updated: 2026-03-03]

- **Self-hosting resolver: 28 remaining comparison mismatches (295/323)**: Remaining categories: (1) Parse failures causing misidentified statics — bare tuple return types, `Mutex[int]` param types, dot-shorthand patterns (5 fixtures). (2) `implicit it` — needs `ImplicitClosure` AST variant in Gorget parser (1 fixture). (3) Test `with` clause bindings not in AST (1 fixture). (4) Tuple destructuring in VarDecl (`auto (a, b) = ...`) — name is string not pattern (1 fixture). (5) `directive name-first` not supported (1 fixture). (6) Various match/pattern/enum resolution divergences — is_bindings, async_match, derive, enums, option_box_enum, recursive_enum, etc. (12 fixtures). (7) Other: shared_weak, mutex_basic, trait_bounds, namespace_basic (7 fixtures). SCOPE lines excluded — Rust `Expr::Block` creates extra scopes absent in Gorget AST. DEF spans excluded — Gorget AST doesn't store name spans. [updated: 2026-03-02]

- **`Into[T]` conversion trait**: Counterpart to `From[T]` requiring explicit type args (`value.into[Celsius]()`) or return-type inference. Adds complexity (equipping primitives, potential blanket impl pattern). [added: 2026-02-17]

- **`TryInto[T]` conversion trait**: Fallible counterpart to `Into[T]`, same complexity issues (explicit type args or return-type inference). Track alongside `Into[T]`. [added: 2026-02-18]



- **Extract serialization traits to `std.serialize` module**: When adding TOML/YAML serializers, move `Serializer` and `Serializable` traits to a shared `std.serialize` module. `std.json`, `std.toml`, `std.yaml` would each provide their own backend. [added: 2026-02-17]


- **Hot-reload: multi-file watch**: When a hot-reloadable program imports other modules, all imported .gg files should be watched for changes (currently only watches the main file). Need to pass import file list from loader to codegen. [added: 2026-02-16]

- **Hot-reload: state migration hooks**: Currently, State struct layout changes trigger full reinitialization via `init()`. Future: additive migration (fields added at end keep existing data), explicit migration hooks (`upgrade from v1 to v2`). [added: 2026-02-16]

- **Hot-reload: trait objects / closures in State**: Trait object vtable pointers and closure function pointers become invalid after dlclose. The `reload()` hook can reconstruct them, but compiler-assisted fixup would be better. [added: 2026-02-16]









- **Inconsistent function naming across synthetic stdlib modules**: Modules use different prefixing: `crypto_sha256()`, `bytes_from_str()`, `path_join()`, but HTTP uses bare `get()`/`post()`. Crypto has `crypto_random_bytes()` while bytes has `random_bytes()`. Adopt consistent `module_verb()` convention. (`stdlib.rs`) [added: 2026-02-16]




- **Struct destructuring in VarDecl/for-loop**: Tuple destructuring now works in VarDecl, for-loop, match, and comprehensions. Struct field destructuring (`auto Point { x, y } = point`) still not implemented — would need named-field pattern parsing + codegen. [from roadmap, added: 2026-02-16, updated: 2026-02-20]

- **Const generics**: Partially parsed but not validated or monomorphized. E.g., `struct Array[T, N: int]`. [from roadmap, added: 2026-02-16]

- **Smart pointers — remaining**: `Shared[T]` (Arc-pattern), `Weak[T]`, and `Mutex[T]` are DONE. Remaining: (1) `Box[Trait]` trait object generalization — add `SmartPtrKind` (Box/Rc/Arc/Weak) to `ResolvedType::TraitObject`, extend the `name.node == "Box"` check in `types.rs:367` to a set of known smart pointer names. Vtable dispatch is identical across wrappers; only construction/clone/drop differs. (2) Future: `Arc` vs `Rc` naming alignment if single-threaded variant ever needed. [from roadmap, added: 2026-02-16, updated: 2026-03-02]

- **SSH library enhancements**: Public key authentication (IdentityFile), host key verification against known_hosts, ProxyJump/ProxyCommand support from ssh_config. [added: 2026-02-15]


- **Fixture system for tests**: suite setup/teardown (done) → `with` clause (done) → fixture injection. Named, composable, scoped resources injected into test signatures. Design questions: yield semantics (Drop-based vs explicit teardown), scope model (test/suite), composability (fixture graphs). [added: 2026-02-14]

- **Demand-driven refinement for borrow analysis**: When `return_borrows_from` conservatively unions multiple branches (e.g., function returns from two branches with different parameter origins), this can cause false positives at specific call sites. Per-call-site re-analysis would only activate when the conservative summary causes a rejection, then trace the specific call arguments through the callee body. Currently zero false positives across 466 unit + 228 integration tests — implement when actual false-positive reports arise. [added: 2026-02-18]

- **`@guarded` annotation for opt-in self-referential structs**: If self-referential structs are ever needed, consider an explicit `@guarded` annotation that adds runtime scope-token checks to specific fields. Opt-in (not automatic) to preserve zero-cost default. Each guarded field would carry a scope token that invalidates when the source field is mutated. Requires: field-granularity mutation tracking, fat pointer layout for guarded fields, instrumentation of field writes. Philosophy: compile error by default for unsafe self-references; `@guarded` as explicit escape hatch with documented runtime cost. [added: 2026-02-18]

- **`std.regex` deferred features**: (1) `replace_with(self, str subject, Callable[Match, str] fn)` — callback replacement (requires C→Gorget closure call for user-defined replacement logic). (2) `named_groups(self) -> Dict[str, str]` — requires building a Gorget Dict from C. [added: 2026-02-19]

- **ECS `(int, T)` pair iteration / `items()` method**: `Iterable[int]` only yields entity IDs, forcing immediate `get(eid)` in every loop. An `items()` method yielding `(int, T)` tuples would eliminate boilerplate. Blocked: tuple return from generic equip methods is untested codegen territory. [added: 2026-02-22]

- **ECS iter() copies entire entity_ids vector**: `SparseSet[T].iter()` allocates a fresh `Vector[Entity]` and copies all entity IDs. O(n) allocation just to start iteration. Language limitation: `SparseSetIter` can't hold a reference (no lifetime-annotated struct fields). Could improve with index+length snapshot if struct references become available. [added: 2026-02-22]

## Low

- **GIR Phase 6 — LLVM backend**: Implement `src/backend/llvm/` to emit LLVM IR (text or bitcode) from GIR. Use `llc` or Rust `inkwell`/`llvm-sys` crate for compilation. Construct SSA via alloca+mem2reg pattern. Emit debug info from GIR source locations. Wire as `gg build --backend=llvm`. Removes C compiler dependency for native compilation. [added: 2026-03-05]

- **GIR Phase 7 — WASM backend**: Add WebAssembly support — either via LLVM backend with `--target=wasm32-wasi` (Option A, minimal code) or direct WASM emission (Option B, more control). Start with Option A, defer Option B if LLVM quality is sufficient. Wire as `gg build --backend=wasm`. [added: 2026-03-05]

- **GIR Phase 8 — GPU/compute backend**: Implement `@kernel` and `@parallel` annotation support in GIR lowering. Add validation pass for GPU constraints (no heap alloc, recursion, virtual dispatch). Implement SPIR-V emission (most portable GPU target). Generate host-side dispatch code (buffer setup, kernel launch, readback). Wire as `gg build --backend=gpu`. [added: 2026-03-05]

- **`gg sim` cross-target interpretation**: `gg sim run --target aarch64-unknown-gorget-elf` to catch byte-layout bugs by interpreting code as a different architecture/endianness. Test struct layout assumptions portably without recompiling. [added: 2026-03-05]

- **`gg sim` `cfg(sim)` gating**: Support `cfg(sim)` or `cfg(gg_sim)` conditional compilation to gate code that behaves differently under simulation or skip problematic tests. Allows tests to adapt to simulated environment constraints. [added: 2026-03-05]

- **`gg sim` data race detection**: Single-threaded deterministic model + weak memory exploration for detecting races in multi-threaded programs. Requires work-stealing executor + weak memory models. [added: 2026-03-05]

- **`gg sim bench`**: Interpret benchmarks under the simulator (with warmup). Phase 2+ extension from sim.md design. Useful for detecting memory/aliasing issues in performance-critical code. [added: 2026-03-05]

- **`uuid_parse(str) -> Result[UUID, str]`**: Parse UUID strings in the standard `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` format. `UUID` would be a struct `{ uint64_t hi; uint64_t lo; }` with `to_string()` and `eq()` equip. C backend via sscanf or manual hex parsing. [added: 2026-03-02]

- **`Heap[T]` max-heap variant**: Current `Heap[T]` is a min-heap. Add a max-heap variant (either a `MaxHeap[T]` type or a `Heap[T](reverse=true)` constructor parameter). [added: 2026-03-02]


- **Package management phase 2 (`gg update`, registry)**: Semver-aware resolution, central registry, `gg publish`, workspaces. [added: 2026-02-15]

- **Data-driven stdlib call dispatch**: Large match blocks in the GIR C backend (`src/backend/c/mod.rs:3397, 3534, 6543`) where each arm follows the same pattern (check name, emit C call). Replace with a table of (name, arity, C template). [added: 2026-02-16, updated: 2026-03-03]


- **`gg info` command**: show fields, methods, traits, memory layout for a type. [added: 2026-02-10]

- **Docs: document borrow checker inference pipeline for contributors**: Explain the multi-phase architecture: Pass 5a computes `return_borrows_from` per function (body analysis with local alias tracing → elision fallback → explicit `live`), Pass 5b validates at call sites using `var_origins` + `return_borrows_from`. Document how the two systems interact — 5b depends on 5a's metadata for cross-function analysis. Cover `BorrowOrigin` variants, `compute_expr_origin()`, and the `LocalAliasMap` that traces through local variables and calls. [added: 2026-02-22, updated: 2026-02-22]

- **Associated type validation**: Associated types are parsed but not validated or resolved in semantic analysis. [from roadmap, added: 2026-02-16]


- **`gg fmt` (code formatter)**: Auto-formatter for `.gg` source files. [from roadmap, added: 2026-02-16]

- **LSP server**: Language Server Protocol for IDE integration (completions, diagnostics, go-to-definition). [from roadmap, added: 2026-02-16]

- **`gg doc` (documentation generator)**: Generate HTML docs from doc comments. [from roadmap, added: 2026-02-16]

- **`--watch` mode**: `gg run --watch` and `gg test --watch` for recompile-and-rerun on file changes. [from roadmap, added: 2026-02-16]


- **Incremental compilation**: Only recompile changed modules. [from roadmap, added: 2026-02-16]


- **TOML DateTime structured type**: `DateTime(str)` currently stores unparsed text (documented). Future enhancement: add a structured `TomlDateTime` type with year/month/day fields. (`toml.gg:21, 378-427`) [added: 2026-02-16]




- **Serial port library (`std.io.serial`)**: `Port` struct, `.write()`, `.read_until()`, timeout support. C backend via termios/POSIX. [added: 2026-02-14]

- **File system utilities (`std.io.fs`)**: temp directory management, content assertions. [added: 2026-02-14]

- **`--parallel` test execution**: run tests concurrently. [added: 2026-02-14]

- **`directive test_suite "name"`**: suite naming for report grouping. [added: 2026-02-14]

- **`with` clause on suite directive**: `directive test_suite "name" with Resource(...) as r:` for suite-level resource management. [added: 2026-02-14]

- **Table-driven test support**: subtesting/sub-case reporting (for-loops already work for the basic case). [added: 2026-02-14]

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
