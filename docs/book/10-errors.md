# Error Handling

Every program encounters situations it cannot handle: a file that doesn't exist, a network
request that times out, user input that doesn't parse. How a language deals with these
situations shapes how reliable your code can be.

Gorget sorts failures into two broad categories:

- **Expected failures** — things that can go wrong in normal operation: I/O errors, invalid
  input, missing data. The compiler ensures you handle them.

- **Programmer errors** — bugs: array out of bounds, integer overflow, calling `unwrap()`
  on `None`. By default these **panic immediately**, because continuing with corrupted state
  is usually worse than stopping.

A small, closed set of programmer errors — integer overflow, division by zero, and
out-of-bounds indexing — are **faults**. They panic **uncatchably**: there is no lexical
recovery form for them. Programs that need to *recover* from an arithmetic fault opt in via
the fallible arithmetic operators (`+!`, `-!`, `*!`, `/!`, `%!`, `<<!`, `>>!`), which
surface the fault into the ordinary `throws` / `Result[T, E]` channel — see the "Faults
Panic Uncatchably" section at the end of the chapter.

This chapter covers all of it, starting with expected failures — the kind that need a design.

The previous chapter introduced `Result[T, E]` and `Option[T]` as data types with methods
like `map`, `and_then`, and `unwrap_or`. This chapter builds on that foundation with
Gorget's `throws` model — syntactic sugar that makes the happy path read like straight-line
code.

---

## Throwing and Catching Errors

Gorget's primary error handling mechanism is `throws`. A function that can fail declares it
in its signature. The caller either lets the error propagate or captures it as a `Result`.
The happy path reads like straight-line code.

### Declaring a Throwing Function

Add `throws` after the parameter list, with the error type:

```gorget
int parse_port(String input) throws String:
    if input.is_empty():
        throw "empty input"
    Option[int] n = int.parse(input)
    match n:
        case Some(val):
            if val < 1 or val > 65535:
                throw f"port out of range: {val}"
            return val
        case None:
            throw f"not a number: {input}"
```

The function returns `int` on success. On failure, it `throw`s a `String` describing what
went wrong. The caller sees a clean signature: "give me a string, I'll give you an int
or tell you why I can't."

### Throwing an Error

The `throw` keyword raises an error. It can only be used inside a `throws` function —
using it elsewhere is a compile error:

```gorget
void helper():
    throw "oops"    # ERROR: throw in non-throwing function
```

You can throw any expression that matches the declared error type:

```gorget
int divide(int a, int b) throws String:
    if b == 0:
        throw "division by zero"
    return a / b
```

### The fallible mark (`!`) and auto-propagation

Fallible calls (`throws` callees and functions that return `Result[T, E]`) do **not**
silently enter the error channel. You activate the channel with a **postfix** `!` on the
call (D29 — visible error propagation). Inside a `throws` function (or a function that
returns `Result`), a marked call peels to the success type and propagates on failure:

```gorget
Config load_config(String path) throws String:
    String content = read_file(path)!     # mark: propagate if this fails
    Config cfg = parse_config(content)!   # same here
    return cfg
```

Three exits for a fallible call (the compiler rejects silent bare discards):

| Form | Meaning |
|------|---------|
| `f()!` | Activate the channel — propagate in a `throws`/`Result` function |
| `f()! catch (e): …` / `f()! rethrow …` | Activate and handle / transform |
| `Result[T, E] r = f()` | **Capture** — explicit Result annotation, **no** mark |

Mark + Result capture together is an error (remove the `!`). Marking a non-fallible
expression (`5!`, `pure()!`) is also an error. A mark in a function that cannot
propagate and has no handler is `E_UnhandledThrows`.

This is similar to exceptions in other languages, but with two critical differences:

1. **It's in the type signature.** A `throws` function declares its error type. You can
   see at a glance which functions can fail and what they fail with.

2. **It's checked and marked.** You cannot call a fallible function without either a mark
   (channel activation) or an explicit Result capture. The compiler forces the issue.

Marked propagation also works in functions that explicitly return `Result`:

```gorget
Result[int, String] double_parsed(String s):
    int val = parse_int(s)!     # peels Ok / propagates Error
    return Ok(val * 2)
```

### Capturing as a Result

Sometimes you don't want an error to propagate — you want the full `Result` value.
When the destination is annotated `Result[T, E]`, leave the call **unmarked**:

```gorget
void main():
    Result[int, String] result = parse_port("8080")
    match result:
        case Ok(port):
            print(f"using port {port}")
        case Error(msg):
            print(f"bad port: {msg}")
```

Without the `Result` type on the destination, calling a `throws` function from a
non-`throws` function is a compile error. The `Result` type is the bridge: it tells
the compiler you want to inspect the result with all the methods from the previous
chapter.

### Quick Recovery

When you just need a default value if something fails:

```gorget
void main():
    Result[int, String] port_result = parse_port(input)
    int port = port_result.unwrap_or(8080)
    print(f"listening on {port}")
```

Or with pattern matching for more nuanced recovery:

```gorget
void main():
    Result[Connection, String] result = connect(host, port)
    match result:
        case Ok(conn):
            handle(conn)
        case Error(e):
            print(f"connection failed: {e}, using fallback")
            handle(fallback_conn())
```

### Intercepting Errors in a Throwing Function

You can use Result capture inside a `throws` function too, when you want to intercept
an error rather than let it propagate:

```gorget
Config load_with_fallback(String path) throws String:
    Result[Config, String] result = load_config(path)
    match result:
        case Ok(cfg):
            return cfg
        case Error(e):
            print(f"warning: {e}, using defaults")
            return Config.default()
```

Without the `Result` type, the error from `load_config` would auto-propagate. With
Result capture, you handle it locally and decide what to do.

### Recovering from Errors with `catch`

Sometimes you want to recover from an error with a fallback value rather than propagate
it. The `catch` keyword is the recovery counterpart to `rethrow` — where `rethrow`
transforms an error and re-throws it, `catch` handles the error and produces a recovery
value. The overall expression always succeeds:

```gorget
void main():
    int port = parse_port(input) catch (e): 8080
    print(f"using port {port}")
```

On success, `parse_port` returns normally and the value passes through. On failure, the
error is bound to `e` and the recovery expression (`8080`) becomes the value. The
recovery expression must produce the same type as the success value.

Because `catch` fully handles the error, it does **not** require the enclosing function
to declare `throws`. You can use it anywhere — including `main` and other non-throwing
functions:

```gorget
void main():
    String content = read_file("config.json") catch (e): "{}"
    Config cfg = parse_config(content) catch (e): Config.default()
    serve(cfg)
```

### Transforming Errors with `rethrow`

Often you want to add context or convert between error types as an error propagates.
The `rethrow` keyword does this concisely:

```gorget
Config load_config(String path) throws ConfigError:
    String content = read_file(path) rethrow (String e): ConfigError.Io(f"reading {path}: {e}")
    Config cfg = parse(content) rethrow (String e): ConfigError.Parse(e)
    return cfg
```

When you don't need the original error, use the **bare form**:

```gorget
void main() throws int:
    Json doc = json_parse(input) rethrow 1
    Data d = load(doc) rethrow 2
    process(d)
```

`rethrow` is a postfix modifier. On success, the expression's value passes through
unchanged. On error, the transform expression is evaluated and thrown. In the binding
form `(Type name): expr`, the original error is available to the transform. In the bare
form, it is discarded.

### `throws int` on Main

`main()` can declare `throws int`, where the thrown integer becomes the process exit
code:

```gorget
void main() throws int:
    Config cfg = load("config.json") rethrow 1
    serve(cfg)
    # implicit success → exit 0
```

If `main` throws, the process exits with that code. If it completes normally, exit 0.
This is the cleanest way to map application errors to OS exit codes — `rethrow` at each
call site converts domain errors into the appropriate code. `main` can only throw `int`
(any other type is a compile error).

### Error-Path Cleanup with `on error`

Sometimes you need cleanup code that only runs when a function exits via error — for
example, closing a file you opened before the error occurred:

```gorget
File open_and_process(String path) throws String:
    File f = File.open(path)
    on error:
        f.close()
    String content = f.read_all()
    return process(content)
```

If `read_all()` throws, the `on error` block runs and `f` is closed before the error
propagates. If everything succeeds, the block is skipped entirely.

For single-statement cleanup, use the **inline form** — no colon, no indented block:

```gorget
File open_and_process(String path) throws String:
    File f = File.open(path)
    on error f.close()
    String content = f.read_all()
    return process(content)
```

Multiple `on error` statements run in **reverse order** (last declared, first executed):

```gorget
void setup() throws String:
    Resource a = acquire_a()
    on error:
        release_a(a)
    Resource b = acquire_b()
    on error:
        release_b(b)
    use(a, b)
```

If `use(a, b)` throws, `release_b` runs first, then `release_a` — matching the
acquisition order in reverse, just like destructors.

---

## The Escalation Ladder

Gorget's error handling forms a natural progression. Start simple; add complexity only
where needed:

1. **Auto-propagation** — do nothing. Errors flow through automatically. This is the
   default and covers most call sites.

2. **`catch`** — recover from an error with a fallback value. Use when a sensible
   default exists and you want to exit error land in one line.

3. **`rethrow`** — add context or convert error types in one line. Use when crossing
   module boundaries or when errors need more information.

4. **`on error`** — add cleanup that only runs on the error path. Use when you've
   acquired resources that need releasing.

5. **Result capture** — drop to full manual control. Declare a `Result[T, E]` variable
   to capture the result, then handle it with pattern matching, combinators, or any
   logic you need.

Most functions only need step 1. A few need steps 2 or 3. Steps 4 and 5 appear at
natural boundaries — module edges, resource management, top-level handlers.

---

## Defining Error Types

For simple cases, `String` is a perfectly fine error type. For larger programs, define an
enum:

```gorget
enum AppError:
    Io(String)
    Parse(String)
    NotFound(String)
    InvalidState(String)
```

This gives callers the ability to match on the *kind* of error and respond differently:

```gorget
void handle_request(String path) throws AppError:
    Result[Data, AppError] result = load_resource(path)
    match result:
        case Ok(data):
            respond(data)
        case Error(AppError.NotFound(msg)):
            respond_404(msg)
        case Error(AppError.Io(msg)):
            # retry once
            Result[Data, AppError] retry = load_resource(path)
            match retry:
                case Ok(data):
                    respond(data)
                case Error(e):
                    throw e
        case Error(e):
            throw e
```

### Error Type Design

A few guidelines:

**Keep error types specific to the layer.** A database module has `DbError`; an HTTP
module has `HttpError`. Don't create one global error enum for the whole program — it
couples everything.

**Include enough context.** `Error("failed")` is useless. `Error("failed to read
/etc/config.toml: permission denied")` tells you what to fix.

**Payload variants are cheap.** Each variant can carry different data:

```gorget
enum DbError:
    ConnectionFailed(String host, int port)
    QueryFailed(String query, String reason)
    Timeout(int elapsed_ms)
```

### The I/O Error Channel

`std.io` ships a canonical `IoError` enum — `NotFound`,
`PermissionDenied`, `BrokenPipe`, `ConnectionReset`, `TimedOut`,
`UnexpectedEof`, `Utf8Invalid(offset)`, `Other(String)`, and other
common categories. All byte-shaped Writer/Reader methods return
`Result[T, IoError]`, so I/O callers pattern-match on category
instead of parsing message strings:

```gorget
from std.io import IoError

match socket.write(request.bytes()):
    case Ok(n):
        print(f"wrote {n} bytes")
    case Error(IoError.BrokenPipe()):
        reconnect()
    case Error(IoError.TimedOut()):
        retry_with_backoff()
    case Error(e):
        log.error(e.display())
```

If a stdlib function you need still returns `Result[T, String]`, the
`from_string_error[T]` helper wraps the message in `IoError.Other`
so the call site can start pattern-matching immediately.

### The Parse Error Channel

Parsing and I/O are two different failure categories, so `std.conv`
ships a separate `ParseError` enum used by `parse_int`,
`parse_float`, `json_parse`, `toml.parse`, `yaml.parse`,
`xml_parse`, `url_decode`, and `form_decode`. Variants:
`Empty`, `InvalidNumber(String)`, `OutOfRange(String)`,
`InvalidSyntax(int byte_offset, String message)`, and
`Other(String)`.

```gorget
from std.conv import parse_int, ParseError

match parse_int(input):
    case Ok(n):
        use(n)
    case Error(ParseError.Empty()):
        return default_value()
    case Error(ParseError.OutOfRange(s)):
        log.warn(f"overflow in {s}")
    case Error(e):
        log.error(e.display())
```

The two channels are disjoint on purpose: a typed parser layered on
top of a Reader surfaces a `ParseError` for "bad input" (format
fault) and an `IoError` for "bad transport" (network/disk fault).
Callers can handle them independently.

### The `Error` Trait

Both `IoError` and `ParseError` implement the narrow `Error` trait
(defined in `std.io`):

```gorget
trait Error extends Displayable & Debuggable:
    Option[String] source(&self)
```

Every well-behaved stdlib error type implements `Error`, which means
a generic helper can receive any error uniformly:

```gorget
from std.io import Error

void log_error[Error E](E e):
    print(e.display())            # human message
    print(e.debug())              # developer view
    match e.source():
        case Some(cause):
            print(f"  caused by: {cause}")
        case None:
            pass
```

The `Error` trait coexists with the `Result.Error(x)` variant: the
first lives in the type namespace (trait bound / equip target), the
second in the value namespace (call / pattern). Use-site resolution
picks the right one by context.

---

## throws vs Result — Under the Hood

`throws` is syntactic sugar for `Result`. A function declared as:

```gorget
int parse_port(String input) throws String:
```

compiles to a function that returns `Result[int, String]`. The `throw` keyword becomes an
early return of `Error(...)`. Auto-propagation becomes automatic unwrapping of `Ok` or
early return of `Error`. Type-directed Result capture is the inverse — when the
destination type is `Result[T, E]`, the compiler skips the auto-unwrap and gives you
the full `Result` value.

This means `throws` functions and `Result`-returning functions are interchangeable from
the caller's perspective. Auto-propagation works with both. You can call a library
function that returns `Result[Config, String]` from a function that `throws String`, and the
error propagates automatically — no conversion needed.

**When to use which:** Use `throws` when you're writing application code and want clean
signatures. Use explicit `Result` returns when you're writing library code that needs to
be explicit about its return types, or when you want to use combinators like `map` and
`and_then` to transform results in a pipeline.

---

## Panics: When Not to Use Any of This

Not every failure should be caught and handled. Some failures indicate bugs — continuing
would be worse than crashing.

**Panics are for programmer errors:**

- Array index out of bounds
- Integer overflow (by default — see below)
- Calling `unwrap()` on `None` or `Error`
- Failed assertions

**`throws` and `Result` are for environmental failures:**

- File not found
- Network timeout
- Invalid user input
- Parse failure

The distinction: if fixing the failure means changing the *code* (a bug), panic. If fixing
it means changing the *environment or the input* (a configuration, a network, a user), use
`throws`.

### Assertions

`assert` checks a condition and panics with a message if it fails:

```gorget
void process(Vector[int] items):
    assert items.len() > 0, "items must not be empty"
    # ... proceed knowing items is non-empty ...
```

Assertions run in both debug and release builds. If a condition is worth checking, it's
worth checking in production. If the check is expensive (e.g., validating an entire data
structure), use a debug-only guard:

```gorget
# This only runs in debug builds
meta if debug():
    assert validate_tree(root), "tree invariant violated"
```

### Integer Overflow

By default, integer overflow panics:

```gorget
int x = 9223372036854775807    # int max
int y = x + 1                  # PANIC: integer overflow
```

This catches subtle bugs that silently corrupt data in C. When you intentionally want
wrapping arithmetic, use the per-operator wrapping operators: `+%`, `-%`, `*%`. Wrapping
is per-expression — plain `+`/`-`/`*` always check overflow.

---

## Faults Panic Uncatchably

A small, closed set of runtime error conditions — integer overflow, division or remainder by
zero, out-of-bounds indexing of an array-backed collection — are **faults**. They **panic
uncatchably**: there is no lexical recovery form. An uncaught fault renders a diagnostic on
stderr and exits, exactly like any other trap (`.unwrap()` on `None`/`Error`, a failing
`assert`, `panic`).

```gorget
void main():
    int big = 9223372036854775807
    int r = big * 2         # trap[T_Overflow]: integer overflow → exit 101
```

Programs that need to *recover* from an arithmetic fault opt in via the fallible arithmetic
operators (`+!`, `-!`, `*!`, `/!`, `%!`, `<<!`, `>>!` — see [Chapter 2 §Fallible arithmetic](02-types.md)),
which surface the fault into the ordinary `throws` / `Result[T, E]` channel:

```gorget
void main():
    int big = 9223372036854775807
    Result[int, ArithError] r = big +! 2       # Error(ArithError.Overflow)
    match r:
        case Ok(v):    print(f"{v}")
        case Error(e): print("overflow")

    # Inside a `throws ArithError` function, `+!` peels to T and auto-propagates:
    int total = sum_or_throw([1, 2, 3]) catch (_): 0
    print(f"{total}")                                # 6

int sum_or_throw(Vector[int] xs) throws ArithError:  # auto-inferred from +!
    int acc = 0
    for x in xs:
        acc = acc +! x                                # auto-propagates on overflow
    return acc
```

For bounds-safety, use `.get(i)` (returns `Option[T]`) or check the index yourself before
indexing:

```gorget
void main():
    Vector[int] xs = [10, 20, 30]
    Option[int] r = xs.get(10)                      # None, no trap
    match r:
        case Some(v): print(f"{v}")
        case None:    print("missing")
```

Out-of-bounds indexing with `xs[i]` itself has no recovery form — it traps uncatchably.

---

## Putting It Together

Here's a realistic example that combines `throws`, Result capture, custom error types,
and `Option`:

```gorget
enum ConfigError:
    FileNotFound(String)
    ParseFailed(String)
    MissingField(String)

struct Config:
    String host
    int port
    String database

Config parse_config(String content) throws ConfigError:
    # Parse key=value pairs from a config file
    Dict[String, String] pairs = Dict[String, String]()
    for line in content.split("\n"):
        if line.is_empty():
            continue
        Option[int] eq = line.index_of("=")
        match eq:
            case Some(pos):
                String key = line.substring(0, pos).trim()
                String val = line.substring(pos + 1, line.len()).trim()
                pairs.put(key, val)
            case None:
                throw ConfigError.ParseFailed(f"invalid line: {line}")

    String host = pairs.get("host") ?? throw ConfigError.MissingField("host")
    String port_str = pairs.get("port") ?? throw ConfigError.MissingField("port")
    String db = pairs.get("database") ?? throw ConfigError.MissingField("database")

    Option[int] port = int.parse(port_str)
    match port:
        case Some(p):
            return Config(host, p, db)
        case None:
            throw ConfigError.ParseFailed(f"invalid port: {port_str}")

Config load_config(String path) throws ConfigError:
    String content = read_file(path) rethrow (String e): ConfigError.FileNotFound(path)
    return parse_config(content)

void main():
    Result[Config, ConfigError] result = load_config("app.conf")
    match result:
        case Ok(cfg):
            print(f"connecting to {cfg.host}:{cfg.port}/{cfg.database}")
        case Error(ConfigError.FileNotFound(path)):
            print(f"config file not found: {path}")
            print("using defaults")
        case Error(ConfigError.ParseFailed(msg)):
            print(f"config parse error: {msg}")
        case Error(ConfigError.MissingField(field)):
            print(f"missing required field: {field}")
```

The structure is clear: `parse_config` throws on any parsing issue with a specific
reason. `load_config` uses `rethrow` to wrap file I/O errors into the same error type
in one line. `main` uses Result capture to catch everything and respond to each error
kind differently.

---

## Summary

| Mechanism | Purpose | Where to use |
|-----------|---------|-------------|
| `throws E` | Declare a function can fail | Function signature |
| `throw expr` | Raise an error | Inside `throws` function |
| Auto-propagation | Errors propagate without syntax | `throws` or `Result`-returning functions |
| `rethrow expr` | Replace error with a different value | `throws int` main, simple error mapping |
| `rethrow (T e): expr` | Transform and re-throw with context | Adding context, converting error types |
| `catch (e): expr` | Recover from error with fallback | Default values, graceful degradation |
| `+!` / `-!` / `*!` / `/!` / `%!` / `<<!` / `>>!` | Fallible arithmetic (returns `Result[T, ArithError]`) | Recoverable arithmetic |
| `throws int` on main | Exit code on error | Process-level error handling |
| `on error: block` | Cleanup on error exit only | Resource cleanup |
| `Result[T, E] x = expr` | Capture a throwing call as `Result` | When you want to handle, not propagate |
| `assert` | Panic if condition is false | Invariant checks (runs in all builds) |
| Panic | Crash on programmer error | Bugs, not environmental failures |
