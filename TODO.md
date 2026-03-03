# TODO

## High

## Medium

- **`path_normalize(str)` and `path_absolute(str)` stdlib functions**: Missing from `std.fs`. `path_normalize` should resolve `.`/`..` components without touching the filesystem; `path_absolute` should canonicalize relative to `getcwd()`. Both return `str`. [added: 2026-03-02]

- **Inline bounds follow-up — find new syntax for `outlives` to fully remove `where`**: The `where` keyword is now only used for `where a outlives b`. Options: (1) inline on the lifetime param `live(a outlives b)`, (2) a dedicated `outlives` section, (3) lifetime annotations on the param itself. Survey and decide before removing `where` entirely. [added: 2026-03-02]

- **Async/await — architectural roadmap**: **Settled design decisions**: (a) Colored async: `async fn` returns `Future[T]`, caller must `await`. Matches Gorget's explicit type philosophy. (b) Ban borrows across await for V1: no `&T` or `&mut T` live across await points. Owned + Copy types only. Eliminates self-referential state structs. Revisit later. (c) ~~Thread-pool executor~~ DONE. (d) ~~Channels for inter-task communication~~ DONE. (e) Deferred: work-stealing. **Prerequisites**: ~~(1) fix closure body scope leakage~~ DONE, ~~(2) extract ExprVisitor trait~~ DONE. ~~**Phase 1 — Type system**~~ DONE. ~~**Phase 2 — Borrow checker suspension-point tracking**~~ DONE. ~~**Phase 3 — Codegen: state machine transformation**~~ DONE. ~~**Phase 4 — Runtime: thread-pool executor**~~ DONE. ~~**Phase 5 — Thread-local error handling + std.channel**~~ DONE. ~~**Phase 6 — Await inside control flow (while/loop/if/elif/else)**~~ DONE. ~~**Phase 7 — RAII for async state structs**~~ DONE. ~~**Phase 8 — Await inside for-loops (range)**~~ DONE. ~~**Phase 9 — Await inside match statements**~~ DONE. ~~**Phase 10 — Await inside non-range for-loops**~~ DONE. ~~**Phase 11 — Expression-position await**~~ DONE. ~~**Phase 12 — Postfix `.await()` syntax**~~ DONE. ~~**Phase 13 — Waker protocol + event-driven executor + non-blocking task-await**~~ DONE. **Remaining items**: ~~expression-position task-await~~ DONE, ~~async channels (waker-driven send/recv instead of condvar blocking)~~ DONE, ~~timer/sleep primitive (first I/O-like waker consumer)~~ DONE (`std.async.async_sleep`), ~~I/O reactor (epoll/kqueue integration)~~ DONE (`REACTOR_RUNTIME`, timerfd+epoll on Linux, kqueue on macOS), ~~worker thread wakers~~ DONE, await inside Iterable/Iterator-based for-loops (busy-poll fallback), async closures, sub-future state cleanup, ~~ConsumeCallable single-call enforcement~~ DONE, ~~`select` for multiplexing multiple channels~~ DONE, ~~unbuffered channels (`Channel[T](0)`)~~ DONE, ~~for/else with break-flag in async for-loops~~ DONE, ~~await in `if`/`while` conditions~~ DONE, ~~await in for-loop iterable/range bounds~~ DONE. **V1 limitations**: thread-pool deadlock if all workers block on `await Task`; ~~spawned-but-never-awaited tasks leak memory~~ DONE (join-on-drop); `await Task` only in async functions; ~~no `Channel.free()` called automatically (manual close only)~~ DONE (refcount RAII). **I/O reactor Phase 2**: socket/file readability events (epoll EPOLLIN on socket fd), completing the async I/O story. [added: 2026-02-21, updated: 2026-03-02]



- **Self-hosting parser: 3 remaining comparison mismatches (234/237)**: (1) `chars.gg` — `'\0'` null character literal truncates the C string at the null byte, so the Gorget parser outputs `''` instead of `'\0'`. Fundamental C string limitation. (2) `math_constants.gg` — C's `%g` float formatting outputs `1e+06` / `3.14159e+06` while Rust's `Display` outputs `1000000.0` / `3141592.0`. Would need custom float-to-string in format.gg. (3) `name_first.gg` — `directive name-first` enables a completely different parsing mode (identifier-first declarations like `x int` instead of `int x`). The self-hosting parser only handles the default type-first mode. [added: 2026-02-21]

- **Self-hosting resolver: 28 remaining comparison mismatches (295/323)**: Remaining categories: (1) Parse failures causing misidentified statics — bare tuple return types, `Mutex[int]` param types, dot-shorthand patterns (5 fixtures). (2) `implicit it` — needs `ImplicitClosure` AST variant in Gorget parser (1 fixture). (3) Test `with` clause bindings not in AST (1 fixture). (4) Tuple destructuring in VarDecl (`auto (a, b) = ...`) — name is string not pattern (1 fixture). (5) `directive name-first` not supported (1 fixture). (6) Various match/pattern/enum resolution divergences — is_bindings, async_match, derive, enums, option_box_enum, recursive_enum, etc. (12 fixtures). (7) Other: shared_weak, mutex_basic, trait_bounds, namespace_basic (7 fixtures). SCOPE lines excluded — Rust `Expr::Block` creates extra scopes absent in Gorget AST. DEF spans excluded — Gorget AST doesn't store name spans. [updated: 2026-03-02]


- **`Into[T]` conversion trait**: Counterpart to `From[T]` requiring explicit type args (`value.into[Celsius]()`) or return-type inference. Adds complexity (equipping primitives, potential blanket impl pattern). [added: 2026-02-17]

- **`TryInto[T]` conversion trait**: Fallible counterpart to `Into[T]`, same complexity issues (explicit type args or return-type inference). Track alongside `Into[T]`. [added: 2026-02-18]



- **Extract serialization traits to `std.serialize` module**: When adding TOML/YAML serializers, move `Serializer` and `Serializable` traits to a shared `std.serialize` module. `std.json`, `std.toml`, `std.yaml` would each provide their own backend. [added: 2026-02-17]


- **Hot-reload: multi-file watch**: When a hot-reloadable program imports other modules, all imported .gg files should be watched for changes (currently only watches the main file). Need to pass import file list from loader to codegen. [added: 2026-02-16]

- **Hot-reload: state migration hooks**: Currently, State struct layout changes trigger full reinitialization via `init()`. Future: additive migration (fields added at end keep existing data), explicit migration hooks (`upgrade from v1 to v2`). [added: 2026-02-16]

- **Hot-reload: trait objects / closures in State**: Trait object vtable pointers and closure function pointers become invalid after dlclose. The `reload()` hook can reconstruct them, but compiler-assisted fixup would be better. [added: 2026-02-16]



- **Borrow checker: no capture set tracking**: The checker knows a closure's origin (which references flow in) but not its capture set (which variables it closes over). Without this, can't enforce aliasing between closures and their captures, can't detect two closures mutably capturing the same variable, and scope isolation (above) can't be implemented properly. Codegen computes free vars independently — should share this analysis. [added: 2026-02-21]

- **Borrow checker: no MutCallable aliasing enforcement**: A `MutCallable` closure holds `&mut` to captured variables, but the checker doesn't prevent simultaneous reads of those variables while the closure exists. Works in C (raw pointers) but would be unsound with stricter backends. [added: 2026-02-21]






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

- **`uuid_parse(str) -> Result[UUID, str]`**: Parse UUID strings in the standard `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` format. `UUID` would be a struct `{ uint64_t hi; uint64_t lo; }` with `to_string()` and `eq()` equip. C backend via sscanf or manual hex parsing. [added: 2026-03-02]

- **`gg.log` level filtering (`Logger.set_level`)**: Currently `Logger` emits all messages regardless of level. Add `Logger.set_level(LogLevel)` to suppress messages below the configured threshold. The log level should be stored as a thread-local or global static in the C runtime. [added: 2026-03-02]

- **`Heap[T]` max-heap variant**: Current `Heap[T]` is a min-heap. Add a max-heap variant (either a `MaxHeap[T]` type or a `Heap[T](reverse=true)` constructor parameter). [added: 2026-03-02]

- **Simulator dispatch for basic File I/O**: `File__create`, `File__open`, `File__read_all`, `File__write_str` have no simulator dispatch — programs that use file I/O can't be run under `gg check`. Add in-memory file table to `src/sim/runtime.rs`. [added: 2026-03-02]


- **Package management phase 2 (`gg update`, registry)**: Semver-aware resolution, central registry, `gg publish`, workspaces. [added: 2026-02-15]

- **Data-driven stdlib call dispatch**: Large match blocks in the GIR C backend (`src/backend/c/mod.rs:3397, 3534, 6543`) where each arm follows the same pattern (check name, emit C call). Replace with a table of (name, arity, C template). [added: 2026-02-16, updated: 2026-03-03]

- **`gg info` command**: show fields, methods, traits, memory layout for a type. [added: 2026-02-10]

- **Docs: document borrow checker inference pipeline for contributors**: Explain the multi-phase architecture: Pass 5a computes `return_borrows_from` per function (body analysis with local alias tracing → elision fallback → explicit `live`), Pass 5b validates at call sites using `var_origins` + `return_borrows_from`. Document how the two systems interact — 5b depends on 5a's metadata for cross-function analysis. Cover `BorrowOrigin` variants, `compute_expr_origin()`, and the `LocalAliasMap` that traces through local variables and calls. [added: 2026-02-22, updated: 2026-02-22]

- **Associated type validation**: Associated types are parsed but not validated or resolved in semantic analysis. [from roadmap, added: 2026-02-16]

- **Const evaluation**: No compile-time expression evaluation. Needed for const declarations, array sizes, and const generics. [from roadmap, added: 2026-02-16]


- **`gg fmt` (code formatter)**: Auto-formatter for `.gg` source files. [from roadmap, added: 2026-02-16]

- **LSP server**: Language Server Protocol for IDE integration (completions, diagnostics, go-to-definition). [from roadmap, added: 2026-02-16]

- **`gg doc` (documentation generator)**: Generate HTML docs from doc comments. [from roadmap, added: 2026-02-16]

- **`--watch` mode**: `gg run --watch` and `gg test --watch` for recompile-and-rerun on file changes. [from roadmap, added: 2026-02-16]


- **Incremental compilation**: Only recompile changed modules. [from roadmap, added: 2026-02-16]

- **Async/await**: Moved to Medium with structured roadmap. See Medium section. [from roadmap, added: 2026-02-16, promoted: 2026-02-21]


- **TOML DateTime is just a raw string**: `DateTime` variants store unparsed text. Users can't extract year/month/day components. Document limitation or add a structured `TomlDateTime` type. (`toml.gg:21, 378-427`) [added: 2026-02-16]




- **Vector/List/Array declared identically in collections module**: Three collection types declared with identical representations. Either an intentional alias system (document it) or placeholder for future differentiation. (`stdlib.rs:246`) [added: 2026-02-16]

- **Serial port library (`std.io.serial`)**: `Port` struct, `.write()`, `.read_until()`, timeout support. C backend via termios/POSIX. [added: 2026-02-14]

- **File system utilities (`std.io.fs`)**: temp directory management, content assertions. [added: 2026-02-14]

- **`--parallel` test execution**: run tests concurrently. [added: 2026-02-14]

- **`directive test_suite "name"`**: suite naming for report grouping. [added: 2026-02-14]

- **`with` clause on suite directive**: `directive test_suite "name" with Resource(...) as r:` for suite-level resource management. [added: 2026-02-14]

- **Table-driven test support**: subtesting/sub-case reporting (for-loops already work for the basic case). [added: 2026-02-14]

- **HTML report: search, filter, expand-all**: for large test suites — filter by name/status, search within traces, expand/collapse all nodes. [added: 2026-02-14]

- **HTML report: source file/line context**: trace nodes show source text but no file path or line number. [added: 2026-02-14]

- **HTML report: timing breakdown per function**: call/return pairs contain the data but the report doesn't surface it. [added: 2026-02-14]

- **`build_tree` silently absorbs malformed events**: depth jumps and unmatched Return/StmtEnd are silently dropped. Should log a diagnostic. [added: 2026-02-14]

- **`directive implicit-auto`**: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Trade-off: more Pythonic but typos silently create new variables. [added: 2026-02-11]

- **Topological sort silent fallback for cycles**: `src/backend/c/mod.rs:1092-1097` comment says "shouldn't happen without cycles" but code silently appends remaining types in original order. Should `debug_assert!` or warn. [added: 2026-02-16, updated: 2026-03-03]


- **`c_runtime.rs` monolithic string constant**: ~5,200-line single string constant is hard to navigate and edit. Split into separate const blocks or `.c` files. [added: 2026-02-16, updated: 2026-02-25]

## Best Effort

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); evaluate `str.is_alpha()`/`is_digit()` on `str` (currently only on `char`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS; type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Default trait: enum derive**: `@derive(Default)` for enums — which variant is default? Could use first variant, `@default` attribute, or unit-only constraint. Currently not derivable. [added: 2026-02-17]

- **Default trait: replace TryCapture memset with Default call**: TryCapture codegen uses `memset(&var, 0, sizeof(var))` for zero-default on error. Could call `Default_for_Type__default()` instead for proper initialization (e.g., empty strings vs null pointers). Needs type info at the TryCapture site. [added: 2026-02-17]



- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
