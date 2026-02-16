# TODO

## Critical

- **HTTP Response type is opaque with no accessors**: `Response` struct declared as opaque (zero fields) with no equip block or helper functions, but `c_runtime.rs` already has `gorget_http_response_status()`, `gorget_http_response_body()`, `gorget_http_response_header()`. Users cannot use an HTTP response after receiving it. (`stdlib.rs:776`) [added: 2026-02-16]

## High




- **`format_for_c_type` silently casts structs to int**: Unknown types in string interpolation default to `(long long)` cast, corrupting the value. Should error on non-formattable types. (`c_types.rs:284`) [added: 2026-02-16]

- **Unicode escapes silently dropped in JSON and TOML parsers**: `\uXXXX` in JSON skips hex digits and produces empty string; TOML returns literal `"?"`. Silent data loss — `\u0041` should produce `'A'`. Either decode the codepoint or reject with an explicit error. (`json.gg:97-102`, `toml.gg:212-220`) [added: 2026-02-16]

- **HTTP `Client` struct declared but unusable**: `Client` struct declared in synthetic stdlib alongside `Response` but has no constructor, no methods, no way to instantiate. Remove or complete. (`stdlib.rs:777`) [added: 2026-02-16]

- **JSON/TOML accessors fail silently**: `get()`, `at()`, `as_int()` etc. return `Null()` or default values on type mismatch / missing keys with no way to distinguish "key absent" from "value is null." Need `has()`/`contains()` methods or `Result` return types. (`json.gg:350-458`, `toml.gg:837-951`) [added: 2026-02-16]

- **Crypto module error handling is partial**: `crypto_rsa_load_public()` returns `Result[RSAKey, str]` but other fallible ops like `crypto_aes_ctr_new()` return bare types. C runtime has `gorget_crypto_last_error()` but no Gorget API exposes it. (`stdlib.rs:713`) [added: 2026-02-16]

- **Method calls in generic function bodies use wrong mangled name**: Inside monomorphized generic functions, `infer_receiver_mangled_type` fails because the semantic type arg resolves to `Error` instead of `Defined(GenericParam)`. E.g., `c.get()` where `c: Container[T]` emits `Container__/*error*/int64_t__get` instead of `Container__int64_t__get`. The `type_id_to_c_substituted` fallback produces garbage. (`c_expr_call.rs:1050, c_expr_generic.rs:375`) [added: 2026-02-16]

## Medium

- **Hot-reload: inotify file watching for Linux**: Current hot-reload file watcher is macOS-only (kqueue). Need `inotify` implementation in `HOT_RELOAD_RUNTIME` for Linux support. The Linux stub is in place, just needs implementation. [added: 2026-02-16]

- **Hot-reload: multi-file watch**: When a hot-reloadable program imports other modules, all imported .gg files should be watched for changes (currently only watches the main file). Need to pass import file list from loader to codegen. [added: 2026-02-16]

- **Hot-reload: state migration hooks**: Currently, State struct layout changes trigger full reinitialization via `init()`. Future: additive migration (fields added at end keep existing data), explicit migration hooks (`upgrade from v1 to v2`). [added: 2026-02-16]

- **Hot-reload: trait objects / closures in State**: Trait object vtable pointers and closure function pointers become invalid after dlclose. The `reload()` hook can reconstruct them, but compiler-assisted fixup would be better. [added: 2026-02-16]

- **Closures step 2 — FnMut/FnOnce closure traits**: Define `FnMut[Args -> Ret]`, `FnOnce[Args -> Ret]` traits with `call` method. `Fn[sig]` is implemented as a compiler-magic callable type (see DONE.md). Auto-implement based on capture mode: `ByMutRef` → `FnMut`, move → `FnOnce`. Requires per-closure types from step 3 for proper trait dispatch. [updated: 2026-02-16]

- **Closures step 3 — Embed captures in closure struct**: Replace the current two-pointer `GorgetClosure { void* fn_ptr; void* env; }` with per-closure structs that embed captures directly: `struct Closure_N { RetType (*fn_ptr)(Closure_N*, args...); field1; field2; ... }`. This makes closures monomorphized types whose size and layout are known at compile time. The closure function receives `self` instead of `void* env`, eliminating the unsafe cast. **Key files:** `c_runtime.rs` (remove generic GorgetClosure), `c_item.rs:886` (emit_lifted_closures — generate per-closure struct typedefs), `c_expr.rs:863` (closure invocation — pass closure struct pointer instead of env). **Depends on:** step 2 (Fn traits, so each closure struct can implement its trait). Unblocks: type-safe closure passing, proper monomorphization, step 4 (trait objects need known struct layout). [added: 2026-02-14]

- **ByMutRef captures in escaping closures**: `&count` captures still point to the caller's stack even when the env is heap-allocated (step 1). Needs boxing the captured variable itself — separate fix from env heap-allocation. [added: 2026-02-15]

- **Restrict `str` from `&`/`!` parameter modes**: `str` is Copy and pointer-sized — always pass by value. Mutable/moving borrows are meaningless for an immutable view type. The borrow checker should reject `&str` and `!str` parameter modes. [added: 2026-02-16]


- **String interpolation only works in print() context**: `"{n}"` outside of `print()` generates literal `"%s"` instead of formatting the value. Stdlib modules must use explicit `int_to_str()`, `float_to_str()`, `char_to_str()` etc. [added: 2026-02-16]

- **For-loop range bounds validation**: `for n in 0..256` with a `uint8` loop variable silently overflows. Codegen hardcodes `int64_t` for range loop variables (`c_stmt.rs:1210`) — should use the declared type. [added: 2026-02-14]

- **Codegen panics instead of semantic errors**: Several codegen paths panic on invalid input that should be caught earlier — string interpolation of non-primitive types (`c_expr_print.rs:394`), `in` operator fallthrough (`c_expr_call.rs:1225`). Move these checks to semantic analysis or use `unreachable!()`. [added: 2026-02-16]

- **Basic orphan rule**: equip block must be in the module that defines the trait or the type. Prevents incoherent trait implementations across modules. [added: 2026-02-10]

- **XML entity handling incomplete**: Only the 5 predefined entities decoded (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`). Numeric character references (`&#NNN;`, `&#xHH;`) and all other named entities silently pass through as literal `&`. (`xml.gg:26-52`) [added: 2026-02-16]

- **No SDL/GFX error handling**: `sdl_create_window` and `sdl_create_renderer` can return null but return values are used directly without checks. Null window passed to `sdl_create_renderer` segfaults. (`gfx.gg:31-40`) [added: 2026-02-16]

- **SSH Session initialization is fragile**: Constructor takes 11 positional arguments, several initialized with dummy values (`bytes_from_hex("")`, dummy crypto contexts). Error-prone — needs builder pattern or named-parameter constructor. (`ssh.gg:635`) [added: 2026-02-16]

- **ECS `get()` has no bounds check**: Calls `self.sparse.get(id)` without checking `id < sparse.len()` while the adjacent `has()` method does check. Out-of-bounds panic for callers who forget `has()`. (`ecs.gg:74-76`) [added: 2026-02-16]

- **Parameter ownership uniformly `Borrow` in synthetic modules**: Every function parameter declared `Ownership::Borrow` regardless of whether move semantics would be more appropriate for collections. Either document the design decision or add per-parameter ownership. (`stdlib.rs:549`) [added: 2026-02-16]

- **Inconsistent function naming across synthetic stdlib modules**: Modules use different prefixing: `crypto_sha256()`, `bytes_from_str()`, `path_join()`, but HTTP uses bare `get()`/`post()`. Crypto has `crypto_random_bytes()` while bytes has `random_bytes()`. Adopt consistent `module_verb()` convention. (`stdlib.rs`) [added: 2026-02-16]

- **Synthetic vs file-based module split is undocumented**: No comment explaining why some modules are synthetic (Rust-generated AST) vs file-based (parsed `.gg`). Contributors can't make the right choice when adding new modules. (`stdlib.rs:27-59`) [added: 2026-02-16]

- **Operator overloading (via traits)**: Allow user-defined types to implement operators (`+`, `-`, `==`, `<`, `[]`, etc.) through trait equip blocks. [from roadmap, added: 2026-02-16]

- **Struct destructuring**: Tuple destructuring works but struct destructuring does not. E.g., `auto Point { x, y } = point` or `case Point { x, y }:` in match. [from roadmap, added: 2026-02-16]

- **Lifetime inference**: No lifetime system exists yet. Needed for references that outlive their scope, return references from functions, and store references in structs. [from roadmap, added: 2026-02-16]

- **Const generics**: Partially parsed but not validated or monomorphized. E.g., `struct Array[T, N: int]`. [from roadmap, added: 2026-02-16]

- **Smart pointers (Rc[T], Arc[T])**: `Box[Trait]` exists for trait objects but general reference-counted (`Rc[T]`) and atomic reference-counted (`Arc[T]`) pointers are missing. Also `Cell`, `RefCell`, `Mutex`. [from roadmap, added: 2026-02-16]

- **Pattern-bound variable type inference in print**: Bindings from `case Error(e):` use `__typeof__()` for declaration but their TypeId isn't in the resolution map. `print("{e}")` defaults to `%lld` format. Workaround: use `print(e)` for non-int pattern-bound vars. [from codegen-notes, added: 2026-02-16]

- **SSH library enhancements**: Public key authentication (IdentityFile), host key verification against known_hosts, ProxyJump/ProxyCommand support from ssh_config. [added: 2026-02-15]

- **Native data format parsers (pure Gorget)**: Replace vendored C libraries with pure `.gg` implementations. All use recursive enum value trees, recursive descent parsers, and `equip`-based method APIs. Implement in order: [added: 2026-02-15]

  1. **std.yaml** — YAML 1.2 (JSON schema) parser. Value enum: `enum YamlValue: Null, Bool(bool), Int(int), Float(float), Str(str), Seq(Vector[YamlValue]), Map(Dict[str, YamlValue])`. MVP scope: block mappings, block sequences, flow mappings `{}`, flow sequences `[]`, quoted and plain scalars, `#` comments, `---` document markers. Skip: anchors/aliases, tags, multi-document streams, complex keys. Free functions: `yaml_parse(str) -> Result[YamlValue, str]`, `yaml_stringify(YamlValue) -> str`. File: `lib/std/yaml.gg`. YAML's indentation sensitivity makes this the hardest parser.


- **Fixture system for tests**: suite setup/teardown (done) → `with` clause (done) → fixture injection. Named, composable, scoped resources injected into test signatures. Design questions: yield semantics (Drop-based vs explicit teardown), scope model (test/suite), composability (fixture graphs). [added: 2026-02-14]

## Low

- **Package management phase 2 (`gg update`, registry)**: Semver-aware resolution, central registry, `gg publish`, workspaces. [added: 2026-02-15]

- **Closures step 4 — `dyn Fn` / `Box[Fn]` trait objects for closures**: Allow closures to be type-erased via trait objects: `auto callback: dyn Fn[int -> int] = my_closure`. Requires a vtable with the `call` method pointer. `Box[dyn Fn[int -> int]]` provides owned, heap-allocated trait objects. **Implementation:** Generate a vtable struct with `call` function pointer for each Fn trait instantiation. `dyn Fn` is a fat pointer `{ void* data; VTable* vtable; }`. Calling through `dyn Fn` does `vtable->call(data, args...)`. **Depends on:** step 2 (Fn traits) + step 3 (embedded captures, so the data pointer points to a self-contained struct). Unblocks: heterogeneous closure collections (`Vector[dyn Fn[int -> int]]`), callback registries, event handler maps, strategy pattern. [added: 2026-02-14]

- **Consolidate type inference functions**: 6+ scattered inference functions (`infer_c_type_from_expr`, `infer_receiver_type`, `infer_receiver_c_type`, `infer_receiver_mangled_type`, `infer_vector_elem_type`, `infer_closure_body_c_type`) with no caching and redundant re-computation. Consolidate into a single type resolver module. [added: 2026-02-16]

- **Extract `CodegenContext` sub-contexts**: 40+ fields mixing tracing, closures, generics, ownership, and test mode. Extract `ClosureContext`, `TraceContext`, `TestContext` for single-responsibility. [added: 2026-02-16]

- **Data-driven stdlib call dispatch**: ~400-line match block in `c_expr_call.rs` (`gen_call`) where each arm follows the same pattern (extract args, format C call). Replace with a table of (name, arity, C template). [added: 2026-02-16]


- **Deduplicate `trait_defs` collection**: Same `HashMap<String, &TraitDef>` built identically in `emit_function_definitions` and `emit_vtable_instances` (`c_item.rs:402-407, 926-930`). Factor into module-level helper. [added: 2026-02-16]

- **Deduplicate Result constructor pattern**: Same `mangle_generic("Result", ...) + mangle_variant("Ok"/"Error")` pattern repeated 3+ times for HTTP, Socket, RSA (`c_expr_call.rs:575, 607, 717`, `c_expr_methods.rs:1050`). Extract `gen_result_binding()`. [added: 2026-02-16]

- **Inconsistent string type checking**: `is_string_expr()` exists but isn't used everywhere. Some places check `resolve_expr_type_id`, others check `Expr::StringLiteral` (`c_expr.rs:136-137`, `c_expr_generic.rs:454-475`). Unify. [added: 2026-02-16]

- **`gg info` command**: show fields, methods, traits, memory layout for a type. [added: 2026-02-10]

- **Byte strings (`b"..."`)**: Not yet parsed or supported. Needed for binary data handling. [from roadmap, added: 2026-02-16]

- **Associated type validation**: Associated types are parsed but not validated or resolved in semantic analysis. [from roadmap, added: 2026-02-16]

- **Const evaluation**: No compile-time expression evaluation. Needed for const declarations, array sizes, and const generics. [from roadmap, added: 2026-02-16]

- **Conditional compilation (`@cfg`)**: Platform/feature-gated code blocks. [from roadmap, added: 2026-02-16]

- **`gg fmt` (code formatter)**: Auto-formatter for `.gg` source files. [from roadmap, added: 2026-02-16]

- **LSP server**: Language Server Protocol for IDE integration (completions, diagnostics, go-to-definition). [from roadmap, added: 2026-02-16]

- **`gg doc` (documentation generator)**: Generate HTML docs from doc comments. [from roadmap, added: 2026-02-16]

- **`--watch` mode**: `gg run --watch` and `gg test --watch` for recompile-and-rerun on file changes. [from roadmap, added: 2026-02-16]

- **REPL**: Interactive read-eval-print loop for Gorget. [from roadmap, added: 2026-02-16]

- **Incremental compilation**: Only recompile changed modules. [from roadmap, added: 2026-02-16]

- **Async/await, concurrency, threads**: Not started. Requires runtime design (green threads vs OS threads, event loop). [from roadmap, added: 2026-02-16]

- **`json_stringify` / `json_pretty_internal` duplicate logic**: Nearly identical functions — only difference is whitespace insertion. Merge into single `json_stringify_internal(Json, bool pretty, int indent)` helper. (`json.gg:257-343`) [added: 2026-02-16]

- **TOML DateTime is just a raw string**: `DateTime` variants store unparsed text. Users can't extract year/month/day components. Document limitation or add a structured `TomlDateTime` type. (`toml.gg:21, 378-427`) [added: 2026-02-16]

- **SSH hardcoded magic numbers**: Channel window size `2097152` and max packet size `32768` as bare literals. Should be named constants at module top. (`ssh.gg:441, 502`) [added: 2026-02-16]

- **O(n²) string concatenation in XML stringifier**: `xml_out = xml_out + xml_stringify(child)` in loop creates quadratic allocation for large documents. Future `StringBuffer` type would fix this across stdlib. (`xml.gg:318-343`) [added: 2026-02-16]

- **Variable naming collision risk in TOML module**: Variables like `toml_result`, `arr_out`, `tbl_out`, `sec_out` are short/generic enough to risk collisions with user code via `lookup_by_name_anywhere`. JSON and XML already use safer prefixed names. (`toml.gg`) [added: 2026-02-16]

- **SDL local closure duplicates global `opaque_struct()`**: `gen_sdl_module()` defines its own `opaque_struct` closure identical to the global helper function. Just call the global one. (`stdlib.rs:291-303 vs 632`) [added: 2026-02-16]

- **Missing math constants in stdlib**: Math module provides `sin`, `cos`, `sqrt` etc. but no `PI`, `E`, or `INFINITY` constants. Table-stakes for a math library. (`stdlib.rs` math module) [added: 2026-02-16]

- **Missing little-endian byte helpers**: Only big-endian `bytes_read_u32_be` / `bytes_write_u32_be` provided. Little-endian variants expected for general-purpose bytes module. (`stdlib.rs` bytes module) [added: 2026-02-16]

- **Vector/List/Array declared identically in collections module**: Three collection types declared with identical representations. Either an intentional alias system (document it) or placeholder for future differentiation. (`stdlib.rs:246`) [added: 2026-02-16]

- **ECS `self` vs `&self` receiver inconsistency**: Mutating methods use `&self`, read-only use `self` — correct but surprising given Gorget convention. Needs doc comment explaining receiver semantics. (`ecs.gg`) [added: 2026-02-16]

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

- **Topological sort silent fallback for cycles**: `c_item.rs:166-172` comment says "cycles — shouldn't happen" but code silently handles them. Should `debug_assert!` or warn. [added: 2026-02-16]


- **`c_runtime.rs` monolithic string constant**: 2,505-line single string constant is hard to navigate and edit. Split into separate const blocks or `.c` files. [added: 2026-02-16]

## Best Effort

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); evaluate `str.is_alpha()`/`is_digit()` on `str` (currently only on `char`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS (needs Default trait); type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Consistent C code emission style**: `c_item.rs` uses `emitter.emit_line()` with proper indentation, `c_expr.rs` uses long `format!()` strings with embedded `\n`. Statement expression formatting varies across `c_expr.rs:42, 125, 184, 275`. Standardize on emitter usage. [added: 2026-02-16]

- **VTable method slot duplication**: `emit_vtable_method_slot()` and the vtable instance assignment loop (`c_item.rs:852-877, 962-993`) reconstruct the same logic independently. Reuse slot generation. [added: 2026-02-16]


- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
