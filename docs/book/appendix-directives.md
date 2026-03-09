# Appendix D — Directives

Directives are per-file compiler settings declared at the top of a `.gg` file.
They configure compilation behavior without changing the language semantics.

---

## Syntax

```gorget
directive <name>
directive <name>=<value>
```

Directives must appear before any code in the file.

---

## Available Directives

### `directive strip-asserts`

Remove all `assert` statements from the compiled output. Assertions are kept
in all builds by default — this directive strips them for performance-critical
code.

```gorget
directive strip-asserts

void main():
    assert 1 + 1 == 2    # removed at compile time
    print("running")
```

**CLI override:** `--strip-asserts` / `--no-strip-asserts`

---

### `directive overflow=<mode>`

Control arithmetic overflow behavior.

| Mode | Behavior |
|------|----------|
| `checked` | Panic on overflow (default) |
| `wrap` | Wrapping arithmetic (modular) |

```gorget
directive overflow=wrap

void main():
    int8 x = 127
    x += 1          # wraps to -128 instead of panicking
```

**CLI override:** `--overflow=wrap` / `--overflow=checked`

---

### `directive trace`

Enable execution tracing. The compiled program writes a `.trace.jsonl` file
recording function entries, exits, variable assignments, and loop iterations.

```gorget
directive trace

void main():
    int x = 5       # recorded in trace
    print("{x}")
```

Use `gg report app.trace.jsonl` to generate an HTML visualization.

**CLI override:** `--trace` / `--no-trace`

---

### `directive hot-reload`

Build the file for hot code reloading. The compiler produces a host binary and
a guest dynamic library. The guest can be rebuilt and reloaded at runtime.

```gorget
directive hot-reload

void update():
    # This function can be modified and reloaded
    print("version 1")
```

```bash
gg build app.gg --hot-reload    # produces host + guest.dylib
```

Useful for game development, UI iteration, and live-coding workflows.

**CLI override:** `--hot-reload`

---

### `directive scheduler=<mode>`

Select the async task scheduler.

| Mode | Behavior |
|------|----------|
| `pool` | Thread pool (default) |
| `thread` | New OS thread per task |
| `inline` | Inline execution (no threading) |
| `single` | Single-threaded event loop |

```gorget
directive scheduler=inline

async void main():
    spawn do_work()    # runs inline, no threads
```

**CLI override:** `--scheduler=<mode>`

---

### `directive name-first`

Switch to name-before-type declaration syntax for the entire file. This provides
a Rust/Python-style syntax alternative.

```gorget
directive name-first

fn add(a: int, b: int) -> int:
    return a + b

fn main():
    let x: int = add(3, 4)
    print("{x}")
```

**Comparison:**

| Standard (type-first) | With `directive name-first` |
|-----------------------|----------------------------|
| `int x = 5` | `let x: int = 5` |
| `int add(int a, int b):` | `fn add(a: int, b: int) -> int:` |
| `str greet(str name):` | `fn greet(name: str) -> str:` |

The AST is identical regardless of syntax mode. Both forms can coexist in
different files of the same project.

No CLI override — source-only.

---

### `directive immutable-by-default`

Make plain variables immutable. Mutation requires the `mutable` keyword.

```gorget
directive immutable-by-default

void main():
    int x = 5          # immutable
    mutable int y = 0  # mutable
    y += 1              # ok
    x += 1              # compile error: x is immutable
```

**Three-tier mutability with this directive:**

| Declaration | Mutable? |
|-------------|----------|
| `const x = 5` | Never (compile-time constant) |
| `int x = 5` | No (immutable binding) |
| `mutable int x = 5` | Yes |

Without this directive, all non-`const` variables are mutable by default.

No CLI override — source-only.

---

## CLI Override Rules

When both a source directive and a CLI flag are present, the CLI flag wins:

| Source | CLI | Result |
|--------|-----|--------|
| `directive strip-asserts` | `--no-strip-asserts` | Asserts kept |
| `directive overflow=wrap` | `--overflow=checked` | Checked mode |
| `directive trace` | `--no-trace` | No tracing |
| (none) | `--strip-asserts` | Asserts stripped |
| (none) | `--trace` | Tracing enabled |

This lets you override per-file settings without editing source code — useful
for CI, profiling, and debugging.

---

## Summary

| Directive | Values | CLI Override | Purpose |
|-----------|--------|:------------:|---------|
| `strip-asserts` | — | Yes | Remove assertions |
| `overflow` | `wrap`, `checked` | Yes | Overflow behavior |
| `trace` | — | Yes | Execution tracing |
| `hot-reload` | — | Yes | Hot code reloading |
| `scheduler` | `pool`, `thread`, `inline`, `single` | Yes | Async scheduler |
| `name-first` | — | No | Name-before-type syntax |
| `immutable-by-default` | — | No | Immutable variables |
