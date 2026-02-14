# TODO

## Critical

(none)

## High

- **Rust-model closures**: Escaping closures (returned from functions, stored in structs) are currently UB because the env is stack-allocated. Embed captures inside the closure struct so lifetime matches the value. Depends on: closure traits (`Fn`/`FnMut`/`FnOnce`), monomorphization of closure types, trait object support for closures. Unblocks: callbacks, event handlers, builder patterns, any higher-order API that stores closures. [added: 2026-02-13]

- **HTTP client library (`std.net.http`)**: `Client` struct, `.get()`, `.post()`, response status/body. C backend via libcurl or minimal HTTP client. Unblocks: API consumption, web scraping, webhook integration, downloading resources. [added: 2026-02-14]

- **Package management (`gg new`, `gg add`, `gg update`)**: Project scaffolding, dependency resolution, registry. Unblocks: code reuse across projects, ecosystem growth. [added: 2026-02-10]

## Medium

- **For-loop range bounds validation**: `for n in 0..256` with a `uint8` loop variable silently overflows. Codegen hardcodes `int64_t` for range loop variables (`c_stmt.rs:1210`) — should use the declared type. [added: 2026-02-14]

- **Basic orphan rule**: equip block must be in the module that defines the trait or the type. Prevents incoherent trait implementations across modules. [added: 2026-02-10]

- **SSH library (`std.net.ssh`)**: `Session` struct, `.run(cmd) -> str`, `.close()`, automatic teardown. C backend via libssh2 or `popen("ssh ...")`. Unblocks: remote automation, deployment scripts. [added: 2026-02-14]

- **Fixture system for tests**: suite setup/teardown (done) → `with` clause (done) → fixture injection. Named, composable, scoped resources injected into test signatures. Design questions: yield semantics (Drop-based vs explicit teardown), scope model (test/suite), composability (fixture graphs). [added: 2026-02-14]

## Low

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

## Best Effort

- **API consistency**: review `trim()` vs `strip()` overlap; string method naming audit (Python `upper`/`lower`/`startswith` vs current `to_upper`/`to_lower`/`starts_with`); evaluate `str.is_alpha()`/`is_digit()` on `str` (currently only on `char`); review `Vector.get(i)` vs `v[i]` overlap. [added: 2026-02-14]

- **fprintf trace performance**: each trace event produces many small `fprintf` calls. Consider `setvbuf` with a large buffer or batching events in memory. [added: 2026-02-14]

- **HTML report: keyboard/accessibility**: tree nodes use `<span>` with `onclick` — not focusable, no `aria-*`, no `tabindex`. Should use `<button>` elements. [added: 2026-02-14]

- **Showcase examples**: `DenseStore[T]` / archetype ECS (needs Default trait); type erasure / `any` type; ECS query builder (needs variadic generics); `examples/collections/` generic Stack[T]. [added: 2026-02-12]

- **Native backend**: LLVM, QBE, or cranelift — after language stabilizes. [added: 2026-02-10]
