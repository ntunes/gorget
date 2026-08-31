# Appendix D — Directives

Directives are per-file compiler settings declared at the top of a `.gg` file.
They configure compilation behavior without changing the language semantics.

---

## Syntax

```gorget
directive <name>
directive <name>=<value>
```

Directives must appear before any code in the file. The name and the value are
both identifiers, and `=` is the only way to attach a value — `directive
scheduler single` is a syntax error, not a second spelling of
`directive scheduler=single`.

**The set below is closed.** A name that is not on it is rejected, and so is a
value outside the set the directive admits: `directive frobnicate`,
`directive trace=yes` and `directive scheduler=Pool` are all errors (the
scheduler modes are lower-case). A compiler that quietly ignored an unrecognised
directive would turn a typo into a silent change of behaviour — the scheduler
one in particular decides how an `async` program interleaves. Lints do not go
here either: they have their own `lint` keyword.

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

> **Note:** there is no overflow directive. Plain `+`/`-`/`*` always check
> overflow (trap uncatchably on fault); use the per-operator `+%`/`-%`/`*%`
> forms for explicit wrapping, or the fallible `+!`/`-!`/`*!` forms to capture
> the failure as `Result[T, ArithError]`. Wrapping is per-expression by
> design — there is no global mode.

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
| `trace` | — | Yes | Execution tracing |
| `hot-reload` | — | Yes | Hot code reloading |
| `scheduler` | `pool`, `thread`, `inline`, `single` | Yes | Async scheduler |
