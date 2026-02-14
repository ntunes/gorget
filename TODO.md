# TODO

## Medium Priority — Type safety

- **For-loop range bounds validation**: `for n in 0..256` with a `uint8` loop variable silently overflows. The loop variable's declared type should constrain the range bounds. Also, codegen hardcodes `int64_t` for range loop variables (`c_stmt.rs:1210`) — should use the declared type. [added: 2026-02-14]

## Low Priority — Trace / HTML report

- **No trace for non-test mode**: `gg run --trace` produces a trace file but `gg report` expects `test_start`/`test_end` framing. Events outside test boundaries are silently dropped, producing an empty report. Either support a non-test trace view or document the limitation. [added: 2026-02-14]

- **fprintf performance in hot loops**: Each trace event produces many small `fprintf` calls (e.g. `stmt_start` with 3 vars = ~9 calls). Consider `setvbuf` with a large buffer on `__gorget_trace_fp`, or batch events in a memory buffer and flush periodically. [added: 2026-02-14]

- **HTML report: no keyboard/accessibility**: Tree nodes use `<span>` with `onclick` — not focusable, no `aria-*` attributes, no `tabindex`. The global `event` variable in `ttoggle()` is deprecated. Buttons should be `<button>` elements with keyboard handlers. [added: 2026-02-14]

- **HTML report: no search, filter, or expand-all**: For large test suites there's no way to filter by name/status, search within traces, or expand/collapse all nodes at once. [added: 2026-02-14]

- **HTML report: no source file/line context**: Trace nodes show source text but no file path or line number. Adding `file:line` would make navigation much more useful. [added: 2026-02-14]

- **HTML report: no timing breakdown per function**: Call/return pairs contain the data to compute cumulative time per function, but the report doesn't surface it. [added: 2026-02-14]

- **HTML report: dead CSS rules and dark-theme-only**: `.bar-has-fail` applies a solid-green gradient identical to `.bar-fill` (leftover from a split-bar attempt). `.tree-row.leaf {}` is empty. No light-mode support — consider `prefers-color-scheme` media queries. [added: 2026-02-14]

- **`build_tree` silently absorbs malformed events (`report.rs:383-466`)**: Depth jumps (0→3 without intermediates) get silently absorbed. Unmatched `Return`/`StmtEnd` against the root frame are dropped with no warning. Should at minimum log a diagnostic. [added: 2026-02-14]

- **Broken doc comments in `c_item.rs:551,560`**: Lines start with `/ ` instead of `// ` — appears to be a typo where one `/` was lost. [added: 2026-02-14]

## Medium Priority — Stdlib gaps
- `std.fs`: `mkdir`, `rmdir`, `rename`, `copy_file`, `file_size`, `is_dir` [added: 2026-02-14]

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

## Low Priority — API consistency revisit
- Review `trim()` vs `strip()` overlap — consider deprecating one for consistency [added: 2026-02-14]
- Evaluate Python `in` operator support for collections (currently have `.contains()` only) [added: 2026-02-14]
- String method naming audit: Python names (`upper`/`lower`/`startswith`) vs current (`to_upper`/`to_lower`/`starts_with`) — pick a consistent convention [added: 2026-02-14]
- Evaluate whether `str.is_alpha()`/`is_digit()`/`is_alnum()` should exist on `str` (currently only on `char`) [added: 2026-02-14]
- Review `Vector.get(i)` vs indexing syntax `v[i]` overlap [added: 2026-02-14]

## Low Priority — Showcase examples
- `DenseStore[T]` / archetype ECS storage: needs Default trait for generic slot initialization [added: 2026-02-13]
- Type erasure / `any` type: allows fully generic World without user-defined struct [added: 2026-02-13]
- Query builder API for ECS: `world.query[Position, Health]()` returning iterator — needs variadic generics or macro system [added: 2026-02-13]
- [showcase] `examples/collections/` — Custom generic data structure: generic Stack[T] backed by Vector, Iterator[T] impl, ownership moves [added: 2026-02-12]

## Low Priority — Long-term design changes
- **Rust-model closures**: embed captures inside the closure struct (not a separate env allocation). Each closure becomes a unique anonymous type; polymorphic dispatch via `Fn`/`FnMut`/`FnOnce` traits + monomorphization, `dyn Fn` for trait objects. Eliminates the env pointer indirection and makes closure lifetime identical to the closure value's lifetime. Depends on: closure traits, monomorphization of closure types, trait object support for closures. [added: 2026-02-13]

### Future: Fixture System
- **Evolutionary path**: suite setup/teardown (done) → `with` clause for per-test resources (done) → fixture injection (long-term). Each stage builds on the previous; fixtures are the final generalization where named, composable, scoped resources are injected into test signatures automatically. [added: 2026-02-14]
- **Design questions to resolve**:
  - *Yield semantics*: pytest fixtures use `yield` for co-located setup/teardown. In a C-targeting language without coroutines, alternatives include Drop-based teardown (leverage existing cleanup stack), explicit `setup`/`teardown` blocks within the fixture definition, or a callback/closure pattern.
  - *Keyword choice*: `fixture` as a new keyword is the most explicit option. `equip` was considered and rejected — it's the wrong abstraction (`equip` means "implement a trait for a type", not "inject a dependency into a test").
  - *Scope model*: pytest supports `function`, `class`, `module`, `session` scopes. Gorget could start with `test` (per-test) and `suite` (per-suite) scopes, matching existing setup/teardown granularity.
  - *Composability*: fixtures that depend on other fixtures (fixture graphs). Requires a resolution/ordering pass at compile time.
  - *Drop-based vs explicit teardown*: if a fixture's type implements Drop, teardown is automatic. Otherwise, need explicit teardown logic — possibly via the same `with`-style cleanup stack used by the `with` clause.

## Low Priority — Native backend (LLVM, QBE, or cranelift — after language stabilizes)
- (no items yet — depends on earlier phases)

## Very Low Priority — Still pondering
- `directive implicit-auto`: Python-style implicit variable declarations (`x = 1` instead of `auto x = 1`). Semantic pass promotes first-use assignments to declarations. Trade-off: more Pythonic for quick scripts, but typos silently create new variables instead of erroring. Might pair with unused variable warnings as a safety net. [added: 2026-02-11]
