# TODO

## High Priority — Bugs
- Trace source text `%%` bug: `c_string_escape` doubles `%` for printf format strings, but source text is passed to `__gorget_trace_json_str` (which uses `fputc`, not printf). So `a % b` appears as `a %% b` in trace output and HTML reports. Fix: don't apply `%%` escaping for trace source strings, or use a separate escape function. [added: 2026-02-14]

## High Priority — Language completeness
- Real example program: a small but complete CLI tool or parser that exercises the language end-to-end [added: 2026-02-13]


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
