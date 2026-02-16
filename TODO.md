# TODO

## Critical

(none)

## High



- **Generic equip blocks: declarations still skipped**: Generic equip blocks (`equip Foo[T] with Trait`) now emit vtable instances and default method bodies per-instantiation, but forward declarations are still skipped for generic equip methods (`c_item.rs:335`). Works because `emit_monomorphized_equip_method` emits its own prototype, but if any code references the function before step 3b, it would fail. Also, `discover_generic_usages` silently omits instantiations it doesn't discover. [updated: 2026-02-16]


- **`format_for_c_type` silently casts structs to int**: Unknown types in string interpolation default to `(long long)` cast, corrupting the value. Should error on non-formattable types. (`c_types.rs:284`) [added: 2026-02-16]

- **Method calls in generic function bodies use wrong mangled name**: Inside monomorphized generic functions, `infer_receiver_mangled_type` fails because the semantic type arg resolves to `Error` instead of `Defined(GenericParam)`. E.g., `c.get()` where `c: Container[T]` emits `Container__/*error*/int64_t__get` instead of `Container__int64_t__get`. The `type_id_to_c_substituted` fallback produces garbage. (`c_expr_call.rs:1050, c_expr_generic.rs:375`) [added: 2026-02-16]

## Medium

- **Hot-reload: inotify file watching for Linux**: Current hot-reload file watcher is macOS-only (kqueue). Need `inotify` implementation in `HOT_RELOAD_RUNTIME` for Linux support. The Linux stub is in place, just needs implementation. [added: 2026-02-16]

- **Hot-reload: multi-file watch**: When a hot-reloadable program imports other modules, all imported .gg files should be watched for changes (currently only watches the main file). Need to pass import file list from loader to codegen. [added: 2026-02-16]

- **Hot-reload: state migration hooks**: Currently, State struct layout changes trigger full reinitialization via `init()`. Future: additive migration (fields added at end keep existing data), explicit migration hooks (`upgrade from v1 to v2`). [added: 2026-02-16]

- **Hot-reload: trait objects / closures in State**: Trait object vtable pointers and closure function pointers become invalid after dlclose. The `reload()` hook can reconstruct them, but compiler-assisted fixup would be better. [added: 2026-02-16]

- **Closures step 2 — Fn/FnMut/FnOnce closure traits**: Define built-in traits `Fn[Args -> Ret]`, `FnMut[Args -> Ret]`, `FnOnce[Args -> Ret]` with a `call` method. The compiler auto-implements the appropriate trait based on capture mode: `ByValue` + no mutation → `Fn`, `ByMutRef` → `FnMut`, move semantics (future) → `FnOnce`. Currently `CaptureMode` (`codegen/mod.rs`) has `ByValue` and `ByMutRef`; this step adds the trait-level classification. **Key design:** Whether these are real traits in the trait registry or compiler-magic marker traits. Gorget's `equip` system could work: `equip Fn[int -> int] for ClosureType_N`. **Depends on:** nothing (but benefits from step 1 for escaping closures). Unblocks: generic functions accepting closures (`fn apply[F: Fn[int -> int]](f: F)`), step 3 (embedded captures), step 4 (trait objects). [added: 2026-02-14]

- **Closures step 3 — Embed captures in closure struct**: Replace the current two-pointer `GorgetClosure { void* fn_ptr; void* env; }` with per-closure structs that embed captures directly: `struct Closure_N { RetType (*fn_ptr)(Closure_N*, args...); field1; field2; ... }`. This makes closures monomorphized types whose size and layout are known at compile time. The closure function receives `self` instead of `void* env`, eliminating the unsafe cast. **Key files:** `c_runtime.rs` (remove generic GorgetClosure), `c_item.rs:886` (emit_lifted_closures — generate per-closure struct typedefs), `c_expr.rs:863` (closure invocation — pass closure struct pointer instead of env). **Depends on:** step 2 (Fn traits, so each closure struct can implement its trait). Unblocks: type-safe closure passing, proper monomorphization, step 4 (trait objects need known struct layout). [added: 2026-02-14]

- **ByMutRef captures in escaping closures**: `&count` captures still point to the caller's stack even when the env is heap-allocated (step 1). Needs boxing the captured variable itself — separate fix from env heap-allocation. [added: 2026-02-15]

- **Restrict `str` from `&`/`!` parameter modes**: `str` is Copy and pointer-sized — always pass by value. Mutable/moving borrows are meaningless for an immutable view type. The borrow checker should reject `&str` and `!str` parameter modes. [added: 2026-02-16]


- **String interpolation only works in print() context**: `"{n}"` outside of `print()` generates literal `"%s"` instead of formatting the value. Stdlib modules must use explicit `int_to_str()`, `float_to_str()`, `char_to_str()` etc. [added: 2026-02-16]

- **For-loop range bounds validation**: `for n in 0..256` with a `uint8` loop variable silently overflows. Codegen hardcodes `int64_t` for range loop variables (`c_stmt.rs:1210`) — should use the declared type. [added: 2026-02-14]

- **Codegen panics instead of semantic errors**: Several codegen paths panic on invalid input that should be caught earlier — string interpolation of non-primitive types (`c_expr_print.rs:394`), `in` operator fallthrough (`c_expr_call.rs:1225`). Move these checks to semantic analysis or use `unreachable!()`. [added: 2026-02-16]

- **Basic orphan rule**: equip block must be in the module that defines the trait or the type. Prevents incoherent trait implementations across modules. [added: 2026-02-10]

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

- **Silent catch-all in `scan_stmt_for_generics`**: `_ => {}` at `c_stmt.rs:1177` silently ignores unhandled statement types without documenting which are intentionally excluded. [added: 2026-02-16]

- **`c_runtime.rs` monolithic string constant**: 2,505-line single string constant is hard to navigate and edit. Split into separate const blocks or `.c` files. [added: 2026-02-16]

## Best Effort

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); evaluate `str.is_alpha()`/`is_digit()` on `str` (currently only on `char`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS (needs Default trait); type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Consistent C code emission style**: `c_item.rs` uses `emitter.emit_line()` with proper indentation, `c_expr.rs` uses long `format!()` strings with embedded `\n`. Statement expression formatting varies across `c_expr.rs:42, 125, 184, 275`. Standardize on emitter usage. [added: 2026-02-16]

- **VTable method slot duplication**: `emit_vtable_method_slot()` and the vtable instance assignment loop (`c_item.rs:852-877, 962-993`) reconstruct the same logic independently. Reuse slot generation. [added: 2026-02-16]

- **Generic instance registration duplicated**: Dict/HashMap registration appears twice in `c_item.rs:1191-1207` and `c_item.rs:1263-1276`. [added: 2026-02-16]

- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
