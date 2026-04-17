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
    int hash(self)
```

Required for use as `Dict` keys or `Set` elements.

### Ordinal

```gorget
trait Ordinal:
    int ordinal(self)
```

Returns the zero-based positional index of an enum variant. Only derivable for
enums. The first variant is 0, the second is 1, and so on. Payload values are
ignored.

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
    void drop(!self)
```

Resource cleanup. Called automatically when a value goes out of scope.
The `!self` parameter means `drop` consumes the value.

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
    IterType iter(&self)
```

Creates an iterator. `for x in collection` calls `collection.iter()` to get
an `Iterator`, then calls `next()` repeatedly.

> Types that implement `Iterator[T]` directly (no separate iterator struct)
> can also be used in a `for`-loop — the compiler treats the value as its
> own iterator when it has `next()` but no `iter()`.

---

## Byte-shaped I/O

### Writer

```gorget
trait Writer:
    Result[int, IoError] write_bytes(&self, String bytes)
```

Narrow output interface. Returns the byte count actually written; a
write may be short (sockets, pipes, compression streams).
`write_all(w, bytes)` from `std.io` wraps this with a loop that
guarantees completion.

### Reader

```gorget
trait Reader:
    Result[int, IoError] read_bytes(&self, Vector[uint8] &buf)
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

`equip IoError with Displayable` for human-readable output.

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
`Cloneable`, `Comparable`, `Default`, `Ordinal` (enums only),
`Serializable`, `Deserializable`.

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
| Comparable | `compare` | `<`, `>`, `<=`, `>=` | Yes |
| Hashable | `hash` | Dict keys, Set elements | Yes |
| Ordinal | `ordinal` | Variant index (enums) | Yes |
| Cloneable | `clone` | `.clone()` | Yes |
| Drop | `drop` | Automatic cleanup | No |
| Default | `default` | `.default()` | Yes |
| Add/Sub/Mul/Div/Rem/Mod/Neg | operator methods | Arithmetic | No |
| Numeric | (composite) | Generic numeric code | No |
| Index[K,V] | `get` | `val[key]` | No |
| IndexMut[K,V] | `set` | `val[key] = x` | No |
| Iterator[T] | `next` | Manual iteration, `for x in val` | No |
| Iterable[T] | `iter` | `for x in val` (with iter struct) | No |
| Writer | `write_bytes` | Byte-shaped output | No |
| Reader | `read_bytes` | Byte-shaped input | No |
| From[T] | `from` | Type conversion | No |
| TryFrom[T] | `try_from` | Fallible conversion | No |
| Measurable | `len` | `.len()` | No |
| Parseable | `parse` | `.parse(String)` | No |
| Serializable | `serialize` | JSON output | Yes |
| Deserializable | `deserialize` | JSON input | Yes |
