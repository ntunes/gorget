# Option and Result

Two types appear everywhere in Gorget: `Option[T]` for values that might be absent,
and `Result[T, E]` for operations that might fail. They are the foundation of safe
null handling and error handling. If you understand these two types, you understand
how Gorget prevents an entire class of bugs at compile time.

---

## Option — A Value That Might Be Absent

Not every missing value is an error. A dictionary lookup might miss. A configuration
field might be optional. A search might find nothing. In other languages, these
situations return `null` — and forgetting to check for `null` causes crashes.

Gorget has no `null`. Instead, it has `Option[T]`:

```gorget
Option[int] found = Some(42)
Option[int] missing = None()
```

`Option[T]` is a generic enum with two variants: `Some(T)` holds a value, `None`
holds nothing. The compiler forces you to handle both cases — you cannot accidentally
use a `None` as if it were a value.

### Pattern Matching

The most explicit way to handle an `Option`:

```gorget
Option[int] result = find_user_age("Alice")
match result:
    case Some(age):
        print(f"Alice is {age}")
    case None:
        print("Alice not found")
```

The `is` keyword works for quick checks:

```gorget
if result is Some(age):
    print(f"found: {age}")
```

### Common Methods

```gorget
Option[int] x = Some(42)
Option[int] y = None()

x.unwrap()              # 42 — panics if None
x.expect("need a value") # 42 — panics with message if None
x.unwrap_or(0)          # 42 — eager: default always evaluated
y.unwrap_or(0)          # 0
y.unwrap_or_else((): 42) # 42 — lazy: closure only called on None
x.is_some()             # true
y.is_none()             # true
```

`unwrap()` is a sharp tool — use it only when you're certain the value is `Some`, or
when a panic is the right response (tests, prototyping). In production code, prefer
`unwrap_or`, pattern matching, or the operators below.

### Transforming Options

`map` applies a function to the inner value if present:

```gorget
Option[int] some = Some(42)
Option[int] doubled = some.map((int x): x * 2)    # Some(84)
Option[int] none = None()
Option[int] still_none = none.map((int x): x * 2) # None
```

`and_then` (flat-map) chains operations that themselves return `Option`:

```gorget
Option[int] chained = some.and_then((int x): Some(x + 1))   # Some(43)
Option[int] short = none.and_then((int x): Some(x + 1))     # None
```

`filter` keeps the value only if a predicate is true:

```gorget
Option[int] kept = some.filter((int x): x > 40)       # Some(42)
Option[int] rejected = some.filter((int x): x > 50)   # None
```

`or` and `or_else` provide fallback alternatives:

```gorget
Option[int] alt = Some(77)
Option[int] from_some = some.or(alt)    # Some(42) — already has a value
Option[int] from_none = none.or(alt)    # Some(77) — falls back to alt
Option[int] lazy = none.or_else((): Some(99))  # Some(99)
```

`flatten` unwraps one layer of nesting:

```gorget
Option[Option[int]] nested = Some(Some(100))
Option[int] flat = nested.flatten()    # Some(100)

Option[Option[int]] outer_none = None()
Option[int] still_none = outer_none.flatten()    # None

Option[Option[int]] inner_none = Some(None())
Option[int] also_none = inner_none.flatten()     # None
```

### Full Method Table

| Method | Description | Eager/Lazy |
|--------|-------------|------------|
| `unwrap()` | Extract value, panic if `None` | — |
| `expect(msg)` | Extract value, panic with message if `None` | — |
| `unwrap_or(default)` | Extract value or return default | Eager |
| `unwrap_or_else(f)` | Extract value or compute default | Lazy |
| `is_some()` | True if `Some` | — |
| `is_none()` | True if `None` | — |
| `map(f)` | Transform inner value | Lazy |
| `and_then(f)` | Flat-map — chain `Option`-returning operations | Lazy |
| `or(alt)` | Return self if `Some`, otherwise `alt` | Eager |
| `or_else(f)` | Return self if `Some`, otherwise compute fallback | Lazy |
| `filter(pred)` | Keep value if predicate is true | Lazy |
| `flatten()` | Unwrap `Option[Option[T]]` to `Option[T]` | — |

---

## Optional Chaining (`?.`)

The `?.` operator short-circuits a chain of field accesses when any step produces
`None`:

```gorget
Option[String] city = user?.address?.city
```

If `user` is `None`, the whole expression is `None`. If `user` is `Some` but
`user.address` is `None`, same thing. Only if every step succeeds do you get
`Some(city)`.

Without `?.`, you'd need nested matches:

```gorget
# Without optional chaining — verbose
Option[String] city = None()
match user:
    case Some(u):
        match u.address:
            case Some(addr):
                city = addr.city
            case None:
                pass
    case None:
        pass
```

`?.` makes the common case readable.

---

## The Default Operator (`??`)

The `??` operator provides a default when an `Option` is `None`. The right-hand side
is **lazy** — it is only evaluated when the left-hand side is `None`:

```gorget
String name = user?.name ?? "anonymous"
int port = config?.port ?? 8080
```

This reads naturally: "use this value, or if it's absent, use that default." It
composes beautifully with `?.`:

```gorget
String greeting = user?.profile?.greeting ?? "hello"
```

The difference from `unwrap_or`: the `??` operator works directly on `Option` values
in expressions, while `unwrap_or` is a method call. Use whichever reads better in
context.

---

## Result — A Value or an Error

`Option` answers "is there a value?" `Result` answers "did the operation succeed?"

```gorget
Result[int, String] success = Ok(42)
Result[int, String] failure = Error("something went wrong")
```

`Result[T, E]` is a generic enum: `T` is the success type, `E` is the error type.
Like `Option`, the compiler forces you to handle both variants.

### Pattern Matching

```gorget
Result[int, String] r = parse_number(input)
match r:
    case Ok(value):
        print(f"parsed: {value}")
    case Error(msg):
        print(f"error: {msg}")
```

The `is` keyword works here too:

```gorget
if r is Ok(value):
    print(f"got {value}")
if r is Error(msg):
    print(f"failed: {msg}")
```

### Common Methods

```gorget
Result[int, String] ok = Ok(10)
Result[int, String] err = Error("fail")

ok.unwrap()             # 10 — panics if Error
ok.unwrap_or(0)         # 10 — eager: default always evaluated
err.unwrap_or(99)       # 99
err.unwrap_or_else((String e): 0)  # 0 — lazy: closure only called on Error

ok.is_ok()              # true
err.is_error()          # true
err.unwrap_error()      # "fail" — extract error, panics if Ok
```

### Transforming Results

`map` transforms the success value, leaving errors untouched:

```gorget
Result[int, String] doubled = ok.map((int x): x * 2)      # Ok(20)
Result[int, String] still_err = err.map((int x): x * 2)   # Error("fail")
```

`map_err` transforms the error value, leaving successes untouched:

```gorget
Result[int, int] coded = err.map_err((String e): e.len())   # Error(4)
```

`and_then` chains operations that themselves return `Result`:

```gorget
Result[int, String] chained = ok.and_then((int x): Ok(x + 1))   # Ok(11)
```

`or` and `or_else` provide fallback results:

```gorget
Result[int, String] alt = Ok(77)
Result[int, String] from_ok = ok.or(alt)     # Ok(10) — already succeeded
Result[int, String] from_err = err.or(alt)   # Ok(77) — falls back to alt
```

### Full Method Table

| Method | Description | Eager/Lazy |
|--------|-------------|------------|
| `unwrap()` | Extract value, panic if `Error` | — |
| `expect(msg)` | Extract value, panic with message if `Error` | — |
| `unwrap_or(default)` | Extract value or return default | Eager |
| `unwrap_or_else(f)` | Extract value or compute default from error | Lazy |
| `unwrap_error()` | Extract error, panic if `Ok` | — |
| `is_ok()` | True if `Ok` | — |
| `is_error()` | True if `Error` | — |
| `map(f)` | Transform success value | Lazy |
| `map_err(f)` | Transform error value | Lazy |
| `and_then(f)` | Flat-map on success | Lazy |
| `or(alt)` | Return self if `Ok`, otherwise `alt` | Eager |
| `or_else(f)` | Return self if `Ok`, otherwise compute fallback | Lazy |

---

## Result Under the Hood

Here's a secret that connects this chapter to the next: Gorget's `throws` keyword
is syntactic sugar for `Result`. A function declared as:

```gorget
int parse_port(String input) throws String:
```

compiles to a function that returns `Result[int, String]`. The `throw` keyword becomes
an early return of `Error(...)`. When the destination variable is typed as `Result[T, E]`,
the compiler captures the full `Result` instead of auto-unwrapping.

You don't need to think about this to use `throws` — the next chapter covers that
model in full. But knowing the connection means you can move freely between the two
styles. A function that returns `Result[int, String]` and a function that `throws String`
are interchangeable from the caller's perspective.

**When to use which:** Use `throws` when you're writing application code and want
clean signatures. Use explicit `Result` returns when you're writing library code that
wants to be explicit about its return types, or when you're working with combinators
like `map` and `and_then` to transform results in a pipeline.

---

## Option vs Result — When to Use Which

The distinction is simple:

- **Option** — the value might not exist, and that's *normal*. A lookup that misses,
  an optional field, a search that finds nothing.

- **Result** — the operation tried to do something and might have *failed*. File I/O,
  parsing, network requests, validation.

A few examples:

```gorget
# Option — absence is expected and normal
Option[String] middle_name = user.middle_name       # many people don't have one
Option[int] index = text.index_of("needle")      # the text might not contain it
Option[User] found = users.find((User u): u.id == target_id)

# Result — failure means something went wrong
Result[File, String] file = File.open("/etc/config")
Result[int, String] parsed = int.try_parse(input)
Result[Response, HttpError] response = http.get(url)
```

When in doubt: if the caller needs to know *why* something didn't work, use `Result`.
If "it's not there" is the only information needed, use `Option`.

---

## Summary

| Mechanism | Purpose | Key Operations |
|-----------|---------|----------------|
| `Option[T]` | Value that might be absent | `unwrap`, `map`, `and_then`, `filter`, `flatten` |
| `Some(value)` | Wraps a present value | Pattern matching, `is` checks |
| `None()` | Represents absence | — |
| `?.` | Optional chaining | Short-circuit on `None` |
| `??` | Default operator (lazy) | Provide fallback value |
| `Result[T, E]` | Success or typed error | `unwrap`, `map`, `map_err`, `and_then` |
| `Ok(value)` | Wraps a success | Pattern matching, `is` checks |
| `Error(reason)` | Wraps an error | `unwrap_error`, `map_err` |
| `unwrap_or(v)` | Default on failure (eager) | Both `Option` and `Result` |
| `unwrap_or_else(f)` | Default on failure (lazy) | Both `Option` and `Result` |
