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
    print(f"{x}")
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
