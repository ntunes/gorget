# TODO

## High Priority — Language completeness
- Real example program: a small but complete CLI tool or parser that exercises the language end-to-end [added: 2026-02-13]


## High Priority — Trace / HTML report

- **Fragile hand-rolled JSON parser in `src/report.rs`**: The ~300-line parser (`extract_str`, `extract_int`, `extract_args`, `extract_vars_raw`, `parse_vars_pairs`) uses naive string scanning that will break on nested objects, values containing key-like substrings, or any non-trivial JSON. `extract_args` assumes no nested `{}` in args. `parse_vars_pairs` uses a heuristic to guess if values are strings. `extract_args` and `parse_vars_pairs` are near-duplicate state machines. Either switch to `serde_json` or consolidate into a single robust extractor. [added: 2026-02-14]

- **`Stmt::Expr` not traced (`c_stmt.rs:304-307`)**: Bare expression statements (e.g. `print("hello")`, standalone function calls) get zero trace instrumentation — no `stmt_start`/`stmt_end` wrapper. This creates gaps in the execution timeline where a function call event appears with no enclosing statement. [added: 2026-02-14]

## Medium Priority — Trace / HTML report

- **Duplicated branch trace emission (`c_stmt.rs:439-506`)**: The trace code for `if`, `elif`, and `else` branches is copy-pasted ~60 lines each, differing only in the `kind` field. Extract into a helper like `emit_branch_trace(kind, condition_span, condition_expr, emitter)`. Same duplication exists for while-loop trace between the `while-else` and plain `while` paths. [added: 2026-02-14]

- **Inconsistent return event format**: Void-function implicit returns (`c_item.rs:482`) emit `"value":null`, expression-body returns (`c_item.rs:511-523`) emit a typed `"value"` field, and explicit `return` statements (`c_stmt.rs:89-91`) emit no `value` field at all. The report parser ignores the field entirely. Either commit to capturing return values and displaying them, or remove the `value` field from all return events for consistency. [added: 2026-02-14]

- **Non-ASCII bug in `substitute_vars` (`report.rs:234-261`)**: Variable substitution indexes by byte position and reconstructs with `result_bytes[i] as char`, which truncates any byte > 127. Source lines with unicode (accented comments, unicode string literals) will produce corrupted substitution text. Needs proper UTF-8 handling via `char_indices()` or similar. [added: 2026-02-14]

- **`VarDecl` with pattern destructuring not traced (`c_stmt.rs:850-860`)**: Only `Pattern::Binding` declarations get `stmt_start`/`stmt_end`. Tuple patterns (`let (a, b) = get_pair()`) and wildcard patterns are invisible in the trace. [added: 2026-02-14]

- **Crash-resilient report generation**: If the test binary crashes (segfault in unsafe code), `atexit` handlers don't run and the trace file is truncated. `parse_trace_file` silently produces a partial report. Should warn when `test_start` events have no matching `test_end`, indicating a probable crash. [added: 2026-02-14]

- **`generate_c` has 11 parameters (`codegen/mod.rs:197`)**: Trace-related params (`trace`, `trace_filename`) plus `strip_asserts`, `overflow_wrap`, test filters, etc. should be bundled into a `CodegenOptions` struct. [added: 2026-02-14]

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
