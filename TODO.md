# TODO

## High Priority — Test Framework Enhancements (Phase 2)
- `with` clause on test blocks — per-test resource management with automatic teardown. Syntax: `test "name" with Resource(...) as r:`. Resource created before test body, closed/dropped automatically after (even on failure). Multiple resources: `test "name" with (A(...) as a, B(...) as b):`. Leverages existing Drop trait + cleanup stack. [added: 2026-02-14]
- `--filter` name-based test filtering — `gg test file.gg --filter "fibonacci"` runs only tests whose name contains the substring. Complement to existing `--tag` attribute-based filtering. [added: 2026-02-14]
- `--exclude-tag` flag — `gg test file.gg --exclude-tag slow` skips tests tagged "slow". Complement to existing `--tag` inclusion filter. If both `--tag` and `--exclude-tag` are specified, exclusion wins. [added: 2026-02-14]
- Console reporter improvements — per-test duration (milliseconds), test count header (`Running N tests...`). [added: 2026-02-14]
- `should_panic` test attribute — `test "division by zero" should_panic:` expects a panic. Inverts pass/fail logic: test passes if gorget_panic is called. [added: 2026-02-14]

## High Priority — Language completeness
- String method `.strip()` (alias for `.trim()` or strip-prefix/suffix variant) [added: 2026-02-13]
- Vector higher-order methods: `.map()`, `.filter()`, `.reduce()` [added: 2026-02-13]
- Real example program: a small but complete CLI tool or parser that exercises the language end-to-end [added: 2026-02-13]

## Medium Priority — Reporting (Phase 3)
- `--trace` integration for test runs — trace each test execution to JSONL. Per-test trace events: `test_start`, `test_end` with status/duration. Leverages existing `directive trace` infrastructure. [added: 2026-02-14]
- HTML report generation — Robot Framework-style `report.html` from trace JSONL. Test pass/fail summary page, expandable execution trace per test (function calls, return values). `gg test --report html` flag. [added: 2026-02-14]

## Medium Priority — General-purpose I/O Libraries (Phase 4)
- SSH library (`std.net.ssh`) — `Session` struct, `.run(cmd) -> str`, `.close()`, automatic teardown. C backend via libssh2 or popen("ssh ..."). [added: 2026-02-14]
- HTTP client library (`std.net.http`) — `Client` struct, `.get()`, `.post()`, response status/body. C backend via libcurl or minimal HTTP client. [added: 2026-02-14]
- Serial port library (`std.io.serial`) — `Port` struct, `.write()`, `.read_until()`, timeout support. C backend via termios/POSIX. [added: 2026-02-14]
- File system utilities (`std.io.fs`) — temp directory management, file existence checks, content assertions. [added: 2026-02-14]

## Medium Priority — Language ergonomics & tooling
- `gg` package management subcommands (`gg new`, `gg add`, `gg update`, `gg publish`, etc.) [added: 2026-02-10]
- Basic orphan rule: equip block must be in the module that defines the trait or the type [added: 2026-02-10]
- `gg info` command: show fields, methods, traits, memory layout for a type [added: 2026-02-10]

## Low Priority — Other Test Features
- `--parallel` test execution — run tests concurrently. [added: 2026-02-14]
- `with` clause on suite directive — `directive test_suite "name" with Resource(...) as r:` for suite-level resource management. [added: 2026-02-14]
- `directive test_suite "name"` — suite naming for report grouping. [added: 2026-02-14]
- Table-driven test support — subtesting/sub-case reporting (for-loops already work for the basic case). [added: 2026-02-14]

## Low Priority — Showcase examples
- `DenseStore[T]` / archetype ECS storage: needs Default trait for generic slot initialization [added: 2026-02-13]
- Type erasure / `any` type: allows fully generic World without user-defined struct [added: 2026-02-13]
- Query builder API for ECS: `world.query[Position, Health]()` returning iterator — needs variadic generics or macro system [added: 2026-02-13]
- [showcase] `examples/collections/` — Custom generic data structure: generic Stack[T] backed by Vector, Iterator[T] impl, ownership moves [added: 2026-02-12]

## Low Priority — Long-term design changes
- **Rust-model closures**: embed captures inside the closure struct (not a separate env allocation). Each closure becomes a unique anonymous type; polymorphic dispatch via `Fn`/`FnMut`/`FnOnce` traits + monomorphization, `dyn Fn` for trait objects. Eliminates the env pointer indirection and makes closure lifetime identical to the closure value's lifetime. Depends on: closure traits, monomorphization of closure types, trait object support for closures. [added: 2026-02-13]

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
