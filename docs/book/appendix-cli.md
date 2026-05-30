# Appendix C — CLI Reference

The `gg` command is the Gorget compiler, runner, formatter, and package manager.

---

## Quick Start

```bash
gg run app.gg              # build and run
gg build app.gg            # compile to binary
gg test app.gg             # run tests
gg fmt app.gg -i           # format in place
```

Shorthand: `gg app.gg` is equivalent to `gg run app.gg`.

Running `gg` with no arguments launches the interactive TUI.

---

## Compilation Commands

### `gg build <file.gg>`

Compile a `.gg` source file to a native binary.

```bash
gg build app.gg                    # produces ./app
gg build app.gg --sanitize         # with AddressSanitizer + UBSan
gg build app.gg --shared -o lib.so # shared library
gg build app.gg --hot-reload       # host binary + guest dylib
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--strip-asserts` | Remove all `assert` statements |
| `--no-strip-asserts` | Keep asserts (overrides source directive) |
| `--overflow=wrap` | Wrapping arithmetic on overflow |
| `--overflow=checked` | Panic on overflow (default) |
| `--trace` | Enable execution tracing (outputs `.trace.jsonl`) |
| `--no-trace` | Disable tracing (overrides source directive) |
| `--hot-reload` | Build for hot code reloading |
| `--shared` | Build as shared library |
| `-o <path>` | Output path for binary or library |
| `--sanitize` | Enable AddressSanitizer + UBSan |
| `--feature=<name>` | Enable compile-time feature flag (repeatable) |
| `--scheduler=<mode>` | Async scheduler: `pool`, `thread`, `inline`, `single` |

### `gg run <file.gg>`

Build and execute immediately. Accepts all `build` flags.

```bash
gg run server.gg --feature debug_logging
```

### `gg check <file.gg>`

Run semantic analysis without generating code. Reports type errors, borrow
violations, and unresolved names.

```bash
gg check app.gg
gg check app.gg --show-borrows    # print borrow checker summary
gg build app.gg --show-clones     # print implicit clone report (including CoW materializations)
```

---

## Inspection Commands

### `gg lex <file.gg>`

Print the token stream with source spans. Useful for debugging lexer issues.

### `gg parse <file.gg>`

Print the parsed AST. Useful for verifying how syntax is interpreted.

### IR Dump Flags

These flags work with `build` and `run`:

| Flag | Output |
|------|--------|
| `--emit-lir` | Low-level SSA IR |
| `--emit-c-lir` | Generated C code |
| `--show-clones` | Implicit clone report (ownership-boundary clones + CoW materializations) |
| `--show-borrows` | Borrow checker analysis summary |

---

## Testing

### `gg test <file.gg>`

Run all `test` blocks in the file.

```bash
gg test app.gg                       # run all tests
gg test app.gg --filter "query"      # tests matching substring
gg test app.gg --tag smoke           # only @tag("smoke") tests
gg test app.gg --exclude-tag slow    # skip @tag("slow") tests
gg test app.gg --timeout 5s          # global 5-second timeout
gg test app.gg --parallel 4          # run across 4 worker processes
gg test app.gg --failed-only         # re-run only previously failed tests
gg test app.gg --failed-first        # run failed tests first, then the rest
gg test app.gg --bench               # run benchmarks instead of tests
gg test app.gg --report html         # generate HTML report
gg test app.gg --snapshot save "v1"  # run tests and save snapshots as "v1"
gg test app.gg --snapshot diff "v1" "v2"  # compare two snapshot versions
gg test app.gg --snapshot list       # list saved snapshot versions
gg test app.gg --snapshot show "v1"  # print snapshot contents
gg test app.gg --snapshot delete "v1"  # remove a saved version
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--filter <pattern>` | Run tests with names matching the pattern |
| `--tag <name>` | Run only tests with this tag (repeatable) |
| `--exclude-tag <name>` | Skip tests with this tag (repeatable) |
| `--timeout <value>` | Global timeout (`5s`, `500ms`, or `5000` for ms) |
| `--parallel <N>` | Run tests across N worker processes |
| `--failed-only` | Re-run only previously failed tests |
| `--failed-first` | Run failed tests first, then the rest |
| `--bench` | Run `bench` blocks instead of `test` blocks |
| `--snapshot <cmd>` | Snapshot subcommand: `save`, `diff`, `list`, `show`, `delete` |
| `--report html` | Generate an HTML test report |

---

## Formatting

### `gg fmt <file.gg>`

Format source code to canonical style.

```bash
gg fmt app.gg                # print formatted output to stdout
gg fmt app.gg --in-place     # modify file in place
gg fmt app.gg --check        # exit 1 if not formatted (CI use)
```

**Flags:**

| Flag | Short | Description |
|------|-------|-------------|
| `--in-place` | `-i` | Overwrite the file with formatted output |
| `--check` | `-c` | Check formatting without modifying |

---

## Simulator

### `gg sim <file.gg>`

Run the program in the GIR interpreter instead of compiling to native code.
Useful for debugging and UB detection.

```bash
gg sim app.gg
gg sim app.gg --ub-checks           # detect undefined behavior
gg sim app.gg --seed=42             # seed the RNG for reproducibility
```

### `gg sim test <file.gg>`

Run tests in the simulator with optional UB checking.

```bash
gg sim test app.gg --ub-checks
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--ub-checks` | Enable undefined behavior detection |
| `--seed=<n>` | Seed the RNG with N for reproducible runs |
| `--many-seeds=<from>..<to>` | Run with each seed in `[from, to)` to find non-deterministic bugs |
| `--ignore-leaks` | Suppress leak reports |
| `--disable-isolation` | Allow real I/O (otherwise sandboxed) |
| `--backtrace=<0\|1\|full>` | Error detail level |
| `--overflow=<mode>` | Overflow behavior (`wrap` or `checked`) |

---

## Package Management

### `gg init`

Create a `gorget.toml` manifest in the current directory.

### `gg new <name>`

Create a new project directory with a template `gorget.toml` and `main.gg`.

### `gg add <name>`

Add a dependency.

```bash
gg add utils --git https://github.com/user/utils.git
gg add utils --git https://github.com/user/utils.git --tag v1.0
gg add utils --git https://github.com/user/utils.git --branch dev
gg add local-lib --path ../local-lib
```

### `gg remove <name>`

Remove a dependency from `gorget.toml`.

---

## Reporting

### `gg report <file.trace.jsonl>`

Generate an HTML report from an execution trace file.

```bash
gg build app.gg --trace         # produces app.trace.jsonl
gg report app.trace.jsonl       # produces app.report.html
gg report app.trace.jsonl --output=report.html
```

---

## Global Flags

| Flag | Description |
|------|-------------|
| `--help` / `-h` | Print help |
| `--version` | Print compiler version |
