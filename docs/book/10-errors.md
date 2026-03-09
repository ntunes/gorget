# Error Handling

Every program encounters situations it cannot handle: a file that doesn't exist, a network
request that times out, user input that doesn't parse. How a language deals with these
situations shapes how reliable your code can be.

Gorget divides failures into two categories:

- **Expected failures** — things that can go wrong in normal operation: I/O errors, invalid
  input, missing data. The compiler ensures you handle them.

- **Programmer errors** — bugs: array out of bounds, integer overflow, calling `unwrap()`
  on `None`. These panic immediately, because continuing with corrupted state is worse than
  stopping.

This chapter covers both, starting with expected failures — the kind that need a design.

The previous chapter introduced `Result[T, E]` and `Option[T]` as data types with methods
like `map`, `and_then`, and `unwrap_or`. This chapter builds on that foundation with
Gorget's `throws` model — syntactic sugar that makes the happy path read like straight-line
code.

---

## Throwing and Catching Errors

Gorget's primary error handling mechanism is `throws`. A function that can fail declares it
in its signature. The caller either lets the error propagate or catches it with `try`. The
happy path reads like straight-line code.

### Declaring a Throwing Function

Add `throws` after the parameter list, with the error type:

```gorget
int parse_port(str input) throws str:
    if input.is_empty():
        throw "empty input"
    Option[int] n = int.parse(input)
    match n:
        case Some(val):
            if val < 1 or val > 65535:
                throw "port out of range: {val}"
            return val
        case None:
            throw "not a number: {input}"
```

The function returns `int` on success. On failure, it `throw`s a `str` describing what
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
int divide(int a, int b) throws str:
    if b == 0:
        throw "division by zero"
    return a / b
```

### Auto-Propagation

Here's the key feature: inside a `throws` function, calls to other `throws` functions
automatically propagate errors. If the callee fails, the caller immediately returns that
error — no extra syntax needed.

```gorget
Config load_config(str path) throws str:
    String content = read_file(path)      # if this throws, we throw too
    Config cfg = parse_config(content)    # same here
    return cfg
```

If `read_file` throws `"file not found"`, then `load_config` immediately returns that
same error. If `parse_config` throws `"invalid syntax"`, same thing. The happy path
reads like straight-line code — no error checks between each call.

This is similar to exceptions in other languages, but with two critical differences:

1. **It's in the type signature.** A `throws` function declares its error type. You can
   see at a glance which functions can fail and what they fail with.

2. **It's checked.** You cannot call a `throws` function from a non-`throws` function
   without handling the error. The compiler forces the issue.

Auto-propagation also works in functions that explicitly return `Result`:

```gorget
Result[int, str] double_parsed(str s):
    int val = parse_int(s)      # auto-propagates Error if parse fails
    return Ok(val * 2)
```

### Catching with `try`

Sometimes you don't want an error to propagate — you want to handle it locally. The
`try` keyword captures a throwing call as a `Result` value:

```gorget
void main():
    Result[int, str] result = try parse_port("8080")
    match result:
        case Ok(port):
            print("using port {port}")
        case Error(msg):
            print("bad port: {msg}")
```

Without `try`, calling a `throws` function from a non-`throws` function is a compile
error. `try` is the bridge: it converts the throwing call into a `Result` you can
inspect with all the methods from the previous chapter.

### Quick Recovery

When you just need a default value if something fails:

```gorget
void main():
    int port = try parse_port(input).unwrap_or(8080)
    print("listening on {port}")
```

Or with pattern matching for more nuanced recovery:

```gorget
void main():
    auto result = try connect(host, port)
    match result:
        case Ok(conn):
            handle(conn)
        case Error(e):
            print("connection failed: {e}, using fallback")
            handle(fallback_conn())
```

### Intercepting Errors in a Throwing Function

You can use `try` inside a `throws` function too, when you want to intercept an error
rather than let it propagate:

```gorget
Config load_with_fallback(str path) throws str:
    auto result = try load_config(path)
    match result:
        case Ok(cfg):
            return cfg
        case Error(e):
            print("warning: {e}, using defaults")
            return Config.default()
```

Without `try`, the error from `load_config` would auto-propagate. With `try`, you catch
it locally and decide what to do.

### Transforming Errors with `rethrow`

Often you want to add context or convert between error types as an error propagates.
The `rethrow` keyword does this concisely:

```gorget
Config load_config(str path) throws ConfigError:
    str content = read_file(path) rethrow (str e): ConfigError.Io(f"reading {path}: {e}")
    Config cfg = parse(content) rethrow (str e): ConfigError.Parse(e)
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
File open_and_process(str path) throws str:
    File f = File.open(path)
    on error:
        f.close()
    str content = f.read_all()
    return process(content)
```

If `read_all()` throws, the `on error` block runs and `f` is closed before the error
propagates. If everything succeeds, the block is skipped entirely.

For single-statement cleanup, use the **inline form** — no colon, no indented block:

```gorget
File open_and_process(str path) throws str:
    File f = File.open(path)
    on error f.close()
    str content = f.read_all()
    return process(content)
```

Multiple `on error` statements run in **reverse order** (last declared, first executed):

```gorget
void setup() throws str:
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

2. **`rethrow`** — add context or convert error types in one line. Use when crossing
   module boundaries or when errors need more information.

3. **`on error`** — add cleanup that only runs on the error path. Use when you've
   acquired resources that need releasing.

4. **`try`** — drop to full manual control. Capture the `Result` and handle it with
   pattern matching, combinators, or any logic you need.

Most functions only need step 1. A few need step 2. Steps 3 and 4 appear at natural
boundaries — module edges, resource management, top-level handlers.

---

## Defining Error Types

For simple cases, `str` is a perfectly fine error type. For larger programs, define an
enum:

```gorget
enum AppError:
    Io(str)
    Parse(str)
    NotFound(str)
    InvalidState(str)
```

This gives callers the ability to match on the *kind* of error and respond differently:

```gorget
void handle_request(str path) throws AppError:
    auto result = try load_resource(path)
    match result:
        case Ok(data):
            respond(data)
        case Error(AppError.NotFound(msg)):
            respond_404(msg)
        case Error(AppError.Io(msg)):
            # retry once
            auto retry = try load_resource(path)
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
    ConnectionFailed(str host, int port)
    QueryFailed(str query, str reason)
    Timeout(int elapsed_ms)
```

---

## throws vs Result — Under the Hood

`throws` is syntactic sugar for `Result`. A function declared as:

```gorget
int parse_port(str input) throws str:
```

compiles to a function that returns `Result[int, str]`. The `throw` keyword becomes an
early return of `Error(...)`. Auto-propagation becomes automatic unwrapping of `Ok` or
early return of `Error`. The `try` keyword is the inverse — it wraps the call so you get
the raw `Result` back.

This means `throws` functions and `Result`-returning functions are interchangeable from
the caller's perspective. Auto-propagation works with both. You can call a library
function that returns `Result[Config, str]` from a function that `throws str`, and the
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

This catches subtle bugs that silently corrupt data in C. If you intentionally want
wrapping arithmetic, use the directive:

```gorget
directive overflow=wrap
```

Or use wrapping operators for specific expressions: `+%`, `-%`, `*%`.

---

## Putting It Together

Here's a realistic example that combines `throws`, `try`, custom error types, and
`Option`:

```gorget
enum ConfigError:
    FileNotFound(str)
    ParseFailed(str)
    MissingField(str)

struct Config:
    str host
    int port
    str database

Config parse_config(str content) throws ConfigError:
    # Parse key=value pairs from a config file
    Dict[str, str] pairs = Dict[str, str]()
    for line in content.split("\n"):
        if line.is_empty():
            continue
        Option[int] eq = line.index_of("=")
        match eq:
            case Some(pos):
                str key = line.substring(0, pos).trim()
                str val = line.substring(pos + 1, line.len()).trim()
                pairs.put(key, val)
            case None:
                throw ConfigError.ParseFailed("invalid line: {line}")

    str host = pairs.get("host") ?? throw ConfigError.MissingField("host")
    str port_str = pairs.get("port") ?? throw ConfigError.MissingField("port")
    str db = pairs.get("database") ?? throw ConfigError.MissingField("database")

    Option[int] port = int.parse(port_str)
    match port:
        case Some(p):
            return Config(host, p, db)
        case None:
            throw ConfigError.ParseFailed("invalid port: {port_str}")

Config load_config(str path) throws ConfigError:
    str content = read_file(path) rethrow (str e): ConfigError.FileNotFound(path)
    return parse_config(content)

void main():
    auto result = try load_config("app.conf")
    match result:
        case Ok(cfg):
            print("connecting to {cfg.host}:{cfg.port}/{cfg.database}")
        case Error(ConfigError.FileNotFound(path)):
            print("config file not found: {path}")
            print("using defaults")
        case Error(ConfigError.ParseFailed(msg)):
            print("config parse error: {msg}")
        case Error(ConfigError.MissingField(field)):
            print("missing required field: {field}")
```

The structure is clear: `parse_config` throws on any parsing issue with a specific
reason. `load_config` uses `rethrow` to wrap file I/O errors into the same error type
in one line. `main` uses `try` to catch everything and respond to each error kind
differently.

---

## Summary

| Mechanism | Purpose | Where to use |
|-----------|---------|-------------|
| `throws E` | Declare a function can fail | Function signature |
| `throw expr` | Raise an error | Inside `throws` function |
| Auto-propagation | Errors propagate without syntax | `throws` or `Result`-returning functions |
| `rethrow expr` | Replace error with a different value | `throws int` main, simple error mapping |
| `rethrow (T e): expr` | Transform and re-throw with context | Adding context, converting error types |
| `throws int` on main | Exit code on error | Process-level error handling |
| `on error: block` | Cleanup on error exit only | Resource cleanup |
| `try expr` | Catch a throwing call as `Result` | When you want to handle, not propagate |
| `assert` | Panic if condition is false | Invariant checks (runs in all builds) |
| Panic | Crash on programmer error | Bugs, not environmental failures |
