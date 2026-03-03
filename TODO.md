# TODO

## High

## Medium

- **`gg.httpserver` V2 — non-blocking sockets + async handlers**: Blocked on `GorgetSocket` having no `poll`/`epoll`/`kqueue` integration — reads and writes block the calling thread. Needs `gorget_socket_set_nonblocking()` + fd registration with the existing reactor (epoll fd on Linux, kqueue on macOS), and a `GorgetWaker` protocol extension for readable/writable events. API impact: handler type becomes `async Callable[HttpServerResponse(HttpRequest)]` — one-word change for users. [added: 2026-03-03]

- **`gg.httpserver` V2 — keep-alive / connection reuse**: Current V1 sends `Connection: close` after every response. Future: parse `Connection: keep-alive` + `Keep-Alive: timeout=N`, loop parse→handle→write on the same socket, close on timeout or `Connection: close`. Blocked on async handler signatures (above). [added: 2026-03-03]

- **`std.datetime`: `dt_days_from_epoch` pre-epoch bug**: The while loop `while dt_y < dt_year` in `lib/std/datetime.gg:50` never executes for years < 1970, so `dt_days_from_epoch(1969, 12, 31)` returns 364 instead of -1. The `add_days(-1)` test in `datetime_basic.gg` works correctly because it goes through `to_epoch` → C's `gmtime_r` (which handles negative offsets correctly), not through `dt_days_from_epoch` directly. Fix: use signed arithmetic for the loop counter. [added: 2026-03-03]

- **Module-scoped names (proper namespaces)**: File-based modules currently merge all top-level names into a flat namespace, requiring manual prefixing (`md_`, `csv_`, `enc_`, `xml_`, `yaml_`) to avoid collisions. Every stdlib module does this — it's the C convention. The fix: each file-based module should keep its own scope. `from X import Y` pulls specific names into the caller's scope; `import X` enables qualified access (`X.name`); private helpers (no `public`) stay hidden. The compiler already has `from X import Y` syntax — the missing piece is that the merged AST flattens everything rather than preserving module boundaries. Scope resolution, name resolution, and possibly codegen for qualified access would need changes. Once implemented, all stdlib module prefixes (`md_`, `csv_`, `enc_`, etc.) can be mechanically removed in one pass. [added: 2026-02-26]

- **`std.alloc`: arena checkpoint / restore**: `Arena.checkpoint() -> ArenaCheckpoint` that captures `bytes_used` and `Arena.restore(checkpoint)` that bumps `used` back to the saved value. Useful for transient scratch work without a full `reset()`. Implement as a thin value type wrapping `(Arena*, size_t)`. [added: 2026-03-03]

- **`std.alloc`: per-thread scratch arenas**: `thread_scratch()` returns a thread-local `Arena` reset automatically between calls (double-buffered to allow two scratch frames per thread concurrently). Pattern from stb/handmade: zero-overhead scratch without explicit `with` blocks. [added: 2026-03-03]

- **Inline bounds follow-up — find new syntax for `outlives` to fully remove `where`**: The `where` keyword is now only used for `where a outlives b`. Options: (1) inline on the lifetime param `live(a outlives b)`, (2) a dedicated `outlives` section, (3) lifetime annotations on the param itself. Survey and decide before removing `where` entirely. [added: 2026-03-02]

- **Async/await — remaining items**: Core async system (Phases 1–13) is complete. Remaining: (1) await inside Iterable/Iterator-based for-loops (busy-poll fallback), (2) async closures, (3) sub-future state cleanup, (4) work-stealing executor, (5) I/O reactor Phase 2 — socket/file readability events (epoll EPOLLIN on socket fd). **V1 limitations**: thread-pool deadlock if all workers block on `await Task`; `await Task` only in async functions. [added: 2026-02-21, updated: 2026-03-03]

- **Self-hosting parser: 2 remaining comparison mismatches (235/237)**: (1) `math_constants.gg` — C's `%g` float formatting outputs `1e+06` / `3.14159e+06` while Rust's `Display` outputs `1000000.0` / `3141592.0`. Would need custom float-to-string in format.gg. (2) `name_first.gg` — `directive name-first` enables a completely different parsing mode (identifier-first declarations like `x int` instead of `int x`). The self-hosting parser only handles the default type-first mode. [added: 2026-02-21, updated: 2026-03-03]

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


- **Delayed meta (Zig-style comptime in generic bodies)**: Meta currently evaluates before semantic analysis, so it cannot branch on generic type params. Future exploration: a second meta evaluation pass at monomorphization time that would allow `meta if typename(T) == "int":` inside generic function bodies. Would enable Zig/Odin-style duck typing without traits. [added: 2026-03-03]

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

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); evaluate `str.is_alpha()`/`is_digit()` on `str` (currently only on `char`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS; type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Default trait: enum derive**: `@derive(Default)` for enums — which variant is default? Could use first variant, `@default` attribute, or unit-only constraint. Currently not derivable. [added: 2026-02-17]

- **Default trait: replace TryCapture memset with Default call**: TryCapture codegen uses `memset(&var, 0, sizeof(var))` for zero-default on error. Could call `Default_for_Type__default()` instead for proper initialization (e.g., empty strings vs null pointers). Needs type info at the TryCapture site. [added: 2026-02-17]



- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
