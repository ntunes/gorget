# TODO

## Critical

(none)

## High

- **Package management (`gg new`, `gg add`, `gg update`)**: Project scaffolding, dependency resolution, registry. Unblocks: code reuse across projects, ecosystem growth. [added: 2026-02-10]

## Medium

- **Closures step 2 — Fn/FnMut/FnOnce closure traits**: Define built-in traits `Fn[Args -> Ret]`, `FnMut[Args -> Ret]`, `FnOnce[Args -> Ret]` with a `call` method. The compiler auto-implements the appropriate trait based on capture mode: `ByValue` + no mutation → `Fn`, `ByMutRef` → `FnMut`, move semantics (future) → `FnOnce`. Currently `CaptureMode` (`codegen/mod.rs`) has `ByValue` and `ByMutRef`; this step adds the trait-level classification. **Key design:** Whether these are real traits in the trait registry or compiler-magic marker traits. Gorget's `equip` system could work: `equip Fn[int -> int] for ClosureType_N`. **Depends on:** nothing (but benefits from step 1 for escaping closures). Unblocks: generic functions accepting closures (`fn apply[F: Fn[int -> int]](f: F)`), step 3 (embedded captures), step 4 (trait objects). [added: 2026-02-14]

- **Closures step 3 — Embed captures in closure struct**: Replace the current two-pointer `GorgetClosure { void* fn_ptr; void* env; }` with per-closure structs that embed captures directly: `struct Closure_N { RetType (*fn_ptr)(Closure_N*, args...); field1; field2; ... }`. This makes closures monomorphized types whose size and layout are known at compile time. The closure function receives `self` instead of `void* env`, eliminating the unsafe cast. **Key files:** `c_runtime.rs` (remove generic GorgetClosure), `c_item.rs:886` (emit_lifted_closures — generate per-closure struct typedefs), `c_expr.rs:863` (closure invocation — pass closure struct pointer instead of env). **Depends on:** step 2 (Fn traits, so each closure struct can implement its trait). Unblocks: type-safe closure passing, proper monomorphization, step 4 (trait objects need known struct layout). [added: 2026-02-14]

- **ByMutRef captures in escaping closures**: `&count` captures still point to the caller's stack even when the env is heap-allocated (step 1). Needs boxing the captured variable itself — separate fix from env heap-allocation. [added: 2026-02-15]

- **For-loop range bounds validation**: `for n in 0..256` with a `uint8` loop variable silently overflows. Codegen hardcodes `int64_t` for range loop variables (`c_stmt.rs:1210`) — should use the declared type. [added: 2026-02-14]

- **Basic orphan rule**: equip block must be in the module that defines the trait or the type. Prevents incoherent trait implementations across modules. [added: 2026-02-10]

- **SSH library (`std.net.ssh`)**: `Session` struct, `.run(cmd) -> str`, `.close()`, automatic teardown. C backend via libssh2 or `popen("ssh ...")`. Unblocks: remote automation, deployment scripts. [added: 2026-02-14]

- **Fixture system for tests**: suite setup/teardown (done) → `with` clause (done) → fixture injection. Named, composable, scoped resources injected into test signatures. Design questions: yield semantics (Drop-based vs explicit teardown), scope model (test/suite), composability (fixture graphs). [added: 2026-02-14]

## Low

- **Closures step 4 — `dyn Fn` / `Box[Fn]` trait objects for closures**: Allow closures to be type-erased via trait objects: `auto callback: dyn Fn[int -> int] = my_closure`. Requires a vtable with the `call` method pointer. `Box[dyn Fn[int -> int]]` provides owned, heap-allocated trait objects. **Implementation:** Generate a vtable struct with `call` function pointer for each Fn trait instantiation. `dyn Fn` is a fat pointer `{ void* data; VTable* vtable; }`. Calling through `dyn Fn` does `vtable->call(data, args...)`. **Depends on:** step 2 (Fn traits) + step 3 (embedded captures, so the data pointer points to a self-contained struct). Unblocks: heterogeneous closure collections (`Vector[dyn Fn[int -> int]]`), callback registries, event handler maps, strategy pattern. [added: 2026-02-14]

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
