# Appendix B — Built-in Traits Reference

Gorget provides built-in traits for common operations. Types can implement them
manually with `equip` or automatically with `@derive`.

---

## Display and Equality

### Displayable

```gorget
trait Displayable:
    String display(self)
```

Enables string interpolation (`f"{value}"`) and `print`. All primitive types
implement `Displayable`.

### Debuggable

```gorget
trait Debuggable:
    String debug(self)
```

Developer-facing, round-trip-ish rendering — the dual of `Displayable`.
Where `display()` is hand-written for humans ("3.14"), `debug()` is
derivable for logs, panics, and inspection: struct fields show as
`TypeName { field: <field.debug()>, … }`; strings quote and escape
(`"hi\nworld"`); all primitives implement `Debuggable` intrinsically.

### Equatable

```gorget
trait Equatable:
    bool eq(self, Self other)
```

Enables `==` and `!=` operators. Inequality is derived automatically from `eq`.

### Comparable

```gorget
trait Comparable:
    int compare(self, Self other)
```

Enables `<`, `>`, `<=`, `>=` operators. Returns `-1`, `0`, or `1`.

### Hashable

```gorget
trait Hashable:
    void hash[Hasher H](self, H &h)
```

Required for use as `Dict` keys or `Set` elements. Hashable is
state-based and generic over the `Hasher`: an implementation forwards
each field into the caller's `H` rather than producing a standalone
`int`. Struct implementations compose automatically — `self.x.hash(&h);
self.y.hash(&h)` — with no combine logic to maintain, and the choice
of hashing algorithm lives at the consumer. `std.hash` ships
`FxHasher` as the default; for one-shot callers, `hash_of[T](v)`
uses it directly. To pick a different Hasher, write:

```gorget
SipHasher h = SipHasher.new(key)
v.hash[SipHasher](&h)
int hv = h.finish()
```

### Hasher

```gorget
trait Hasher:
    void write_int(&self, int v)
    void write_bytes(&self, Vector[byte] bytes)
    void write_string(&self, String s)
    int finish(self)
```

Accumulates hash state. `std.hash` ships `FxHasher` as the default
state machine — a simple multiplicative mix suitable for in-process
keyed collections. Other `Hasher` implementations slot in
transparently because `Hashable.hash` is generic over the Hasher; a
user-defined `SipHasher` works the same way.

---

## Copying and Cleanup

### Cloneable

```gorget
trait Cloneable:
    Self clone(self)
```

Deep copy. Called explicitly with `.clone()`.

### Drop

```gorget
trait Drop:
    void drop(^self)
```

Resource cleanup. Called automatically when a value goes out of scope.
The `^self` parameter means `drop` consumes the value.

### Default

```gorget
trait Default:
    Self default()
```

Factory for zero/empty values. `int.default()` is `0`, `String.default()` is `""`.

---

## Arithmetic Operators

Each arithmetic operator maps to a trait:

| Trait | Method | Operator |
|-------|--------|----------|
| `Add[Out]` | `Out add(self, Self rhs)` | `+` |
| `Sub[Out]` | `Out sub(self, Self rhs)` | `-` |
| `Mul[Out]` | `Out mul(self, Self rhs)` | `*` |
| `Div[Out]` | `Out div(self, Self rhs)` | `/` |
| `Rem[Out]` | `Out rem(self, Self rhs)` | `%` |
| `Mod[Out]` | `Out mod(self, Self rhs)` | `.mod()` |
| `Neg[Out]` | `Out neg(self)` | unary `-` |

The `Out` parameter controls the return type. For most types, `Out` equals `Self`.

### Numeric

```gorget
trait Numeric extends Add, Sub, Mul, Div, Rem, Mod, Neg, Comparable, Default, One
```

Composite trait for numeric types. All integer and float types implement `Numeric`.

### One

```gorget
trait One:
    Self one()
```

Multiplicative identity factory. `int.one()` is `1`, `float.one()` is `1.0`.

---

## Indexing

### Index[K, V]

```gorget
trait Index[K, V]:
    V get(self, K key)
```

Enables read access with `[]`: `value[key]` calls `get(key)`.

### IndexMut[K, V]

```gorget
trait IndexMut[K, V]:
    void set(&self, K key, V value)
```

Enables write access with `[]=`: `container[key] = value` calls `set(key, value)`.
Takes `&self` (mutable borrow).

---

## Iteration

### Iterator[T]

```gorget
trait Iterator[T]:
    Option[T] next(&self)
```

The core iteration protocol. Returns `Some(value)` for each element, `None`
when exhausted. Takes `&self` (mutable borrow) to advance internal state.

### Iterable[T]

```gorget
trait Iterable[T]:
    Iterator[T] iter(&self)
```

`for x in collection` calls `collection.iter()` to get an `Iterator[T]`,
then calls `next()` repeatedly. Dispatch is name-based — the for-loop
fast path doesn't go through trait-vtable lookup, so chains compose at
monomorphisation without virtual dispatch. The trait provides the
contract for `[Iterable T]` generic bounds and documents what `iter()`
returns. `Vector[T]`, `Set[T]`, `Dict[K, V]`, and every lazy adapter
(`TakeIter`, `MapIter`, …) implement `Iterator[T]` (which is itself
walkable in a for-loop without going through Iterable).

> Types that implement `Iterator[T]` directly (no separate iterator
> struct) can also be used in a `for`-loop — the compiler treats the
> value as its own iterator when it has `next()` but no `iter()`.

---

## Byte-shaped I/O

### Writer

```gorget
trait Writer:
    Result[int, IoError] write(&self, Vector[byte] buf)
    Result[int, IoError] flush(&self):   # default: no-op
        return Ok(0)
```

Narrow output interface. Returns the byte count actually written; a
write may be short (sockets, pipes, compression streams). Input is
raw bytes, not UTF-8 — binary protocols, TLS, compression all push
arbitrary byte sequences. Callers with a `String` source convert via
`.bytes()` at the boundary. `write_all(w, buf)` from `std.io` wraps
this with a loop that guarantees completion; `write_str(w, s)` and
`write_display(w, v)` are the text/Displayable convenience adapters.

`flush()` has a default no-op body — in-memory writers (`String`,
`Vector[byte]`) have nothing to flush. `File` overrides to push the
underlying stdio buffer, so progress bars and REPL prompts can force
output before the buffer fills.

### Reader

```gorget
trait Reader:
    Result[int, IoError] read(&self, Vector[byte] &buf)
```

Narrow input interface. Reader fills the caller's buffer through a
mutable borrow and returns the byte count read. `Ok(0)` indicates EOF.
`reader_drain(r)` and `read_exact(r, n)` from `std.io` derive
read-to-end and fill-exactly-n on top.

### IoError

```gorget
enum IoError:
    NotFound
    PermissionDenied
    AlreadyExists
    BrokenPipe
    ConnectionRefused
    ConnectionReset
    ConnectionAborted
    NotConnected
    AddrInUse
    TimedOut
    WouldBlock
    Interrupted
    UnexpectedEof
    InvalidInput
    InvalidData
    Utf8Invalid(int)      # byte offset
    Other(String)
```

The typed error channel for all I/O. Pattern-match by category instead
of parsing error strings. `IoError.Other(msg)` is the escape hatch;
prefer a named variant when possible.

`equip IoError with Displayable & Debuggable & Error`.

### ParseError

```gorget
enum ParseError:
    Empty
    InvalidNumber(String)
    OutOfRange(String)
    InvalidSyntax(int byte_offset, String message)
    Other(String)
```

Defined in `std.conv`. Used by `parse_int`, `parse_float`,
`json_parse`, `toml.parse`, `yaml.parse`, `xml_parse`, `url_decode`,
`form_decode`, and future parsers. Kept separate from `IoError`
because "bad input" (format fault) is categorically different from
"bad I/O" (transport fault) — a parser built on top of a `Reader`
surfaces both distinctly.

`equip ParseError with Displayable & Debuggable & Error`.

### Error

```gorget
trait Error extends Displayable & Debuggable:
    Option[String] source(&self)
```

The narrow contract every well-behaved error type implements. Gives
generic helpers a single bound that covers display, debug, and an
optional underlying-cause message. Both `IoError` and `ParseError`
implement it, and generic error-handling code can accept any error:

```gorget
void log_error[Error E](E e):
    print(e.display())
    print(e.debug())
```

Coexists with the `Result.Error(x)` variant via type/value namespace
separation — the trait lives in the type namespace, the variant in
the value namespace, and context picks the right one.

### print — infallible builtin / Writer primitives — typed-error

The compiler builtin `print(v, terminator="\n", file=stdout)` is the
script ergonomic for stdout writes; it panics on failure (rare for
stdout). For typed-error callers, write directly on the `Writer`
primitives:

```gorget
from std.io import stdout, stderr, IoError, write_display, write_str, write_all

Result[int, IoError] r1 = write_display[File, int](&stdout, 42)
Result[int, IoError] r2 = write_str[File](&stderr, "oops\n")
Result[int, IoError] r3 = write_all[File](&stdout, "raw bytes\n".bytes())
```

`write_display` / `write_str` / `write_all` compose with any `Writer`,
not just stdout — files, sockets, in-memory `String` builders all
plug in via the same primitives.

### Typed file I/O

Whole-file convenience helpers in `std.io`:

```gorget
Result[File, IoError]          file_open(String path, String mode)
Result[String, IoError]        read_to_string(String path)
Result[Vector[byte], IoError]  read_all_bytes(String path)
Result[int, IoError]           write_string(String path, String content)
Result[int, IoError]           write_all_bytes(String path, Vector[byte] buf)
```

These compose `file_open` + the `Reader` / `Writer` machinery so
callers get structured errors for common operations. The infallible
`std.fs.read_file` / `write_file` stay as convenience shortcuts that
panic on failure.

---

## Conversion

### From[T]

```gorget
trait From[T]:
    Self from(T value)
```

Infallible conversion. Example: `float.from(42)` produces `42.0`.

### TryFrom[T]

```gorget
trait TryFrom[T]:
    Result[Self, String] try_from(T value)
```

Fallible conversion. Returns `Error` if the conversion is invalid.

### Parseable

```gorget
trait Parseable:
    Option[Self] parse(String s)
```

Parse a value from a string. Returns `None` on invalid input.

---

## Size

### Measurable

```gorget
trait Measurable:
    int len(self)
```

Returns the number of elements. Implemented by `Vector`, `Dict`, `Set`, `String`.

---

## Serialization

### Serializable / Deserializable

```gorget
trait Serializable:
    String serialize(self)

trait Deserializable:
    Self deserialize(String data)
```

JSON serialization. Use `@derive(Serializable, Deserializable)` for automatic
implementation based on struct fields.

---

## Derivable Traits

The `@derive` attribute generates trait implementations automatically:

```gorget
@derive(Equatable, Hashable, Displayable, Cloneable)
struct Point:
    int x
    int y
```

Derivable traits: `Equatable`, `Hashable`, `Displayable`, `Debuggable`,
`Cloneable`, `Default`, `Serializable`, `Deserializable`.

The generated implementation operates field-by-field. For `Equatable`, all fields
must be equal. For `Hashable`, all fields are combined into the hash. For
`Displayable`, fields are printed as `TypeName(field1, field2, ...)`. For
`Debuggable`, structs render as `TypeName { field1: …, field2: … }` where each
field's `debug()` is called recursively (strings get quoted and escaped); enum
variants render as `Variant` for unit variants and `Variant(arg1, arg2, …)`
for tuple variants.

---

## Quick Reference

| Trait | Method(s) | Enables | Derivable |
|-------|-----------|---------|:---------:|
| Displayable | `display` | `f"{val}"`, `print` | Yes |
| Debuggable | `debug` | Developer logs, panics | Yes |
| Equatable | `eq` | `==`, `!=` | Yes |
| Comparable | `compare` | `<`, `>`, `<=`, `>=` | No |
| Hashable | `hash` | Dict keys, Set elements | Yes |
| Cloneable | `clone` | `.clone()` | Yes |
| Drop | `drop` | Automatic cleanup | No |
| Default | `default` | `.default()` | Yes |
| Add/Sub/Mul/Div/Rem/Mod/Neg | operator methods | Arithmetic | No |
| Numeric | (composite) | Generic numeric code | No |
| Index[K,V] | `get` | `val[key]` | No |
| IndexMut[K,V] | `set` | `val[key] = x` | No |
| Iterator[T] | `next` | Manual iteration, `for x in val` | No |
| Iterable[T] | `iter` | `for x in val` (with iter struct) | No |
| Writer | `write` | Byte-shaped output | No |
| Reader | `read` | Byte-shaped input | No |
| From[T] | `from` | Type conversion | No |
| TryFrom[T] | `try_from` | Fallible conversion | No |
| Measurable | `len` | `.len()` | No |
| Parseable | `parse` | `.parse(String)` | No |
| Serializable | `serialize` | JSON output | Yes |
| Deserializable | `deserialize` | JSON input | Yes |
