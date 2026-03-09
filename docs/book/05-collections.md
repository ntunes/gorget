# Strings and Collections

This chapter covers Gorget's built-in data structures: strings, vectors, dicts,
sets, tuples, and comprehensions.

---

## Strings

### str vs String

Gorget has two string types with different ownership semantics:

- **`str`** — an immutable, borrowed string slice. Zero-cost to pass around. Cannot
  be grown or modified.
- **`String`** — an owned, heap-allocated string. Can be concatenated, modified,
  and built up.

```gorget
String owned = "hello"        # owned String
str borrowed = "world"        # borrowed slice
```

String literals adapt to context: in a `String` declaration they produce a `String`;
as a function argument expecting `str` they produce a `str`. You usually don't need
to think about it.

### Concatenation

```gorget
String a = "hello"
String b = " world"
String c = a + b              # "hello world"
a += "!"                      # "hello!"
print(a)
```

### String Interpolation

Embed any expression inside `{}`:

```gorget
str name = "Alice"
int age = 30
print("Name: {name}, Age: {age}")
```

For literal braces, double them: `"{{x}}"` prints `{x}`.

### Common String Methods

```gorget
str s = "Hello, World!"

s.starts_with("Hello")        # true
s.ends_with("World!")         # true
s.contains("World")          # true
s.is_empty()                 # false

s.to_upper()                 # "HELLO, WORLD!"
s.to_lower()                 # "hello, world!"
s.trim()                     # remove leading/trailing whitespace
s.replace("World", "Gorget") # "Hello, Gorget!"

s.len()                      # character count
s.byte_len()                 # byte count (may differ for Unicode)

Vector[str] parts = "a,b,c".split(",")   # ["a", "b", "c"]
Option[int] pos = s.index_of(",")        # Some(5)
str sub = s.substring(0, 5)              # "Hello"
```

### Raw Strings

Raw strings disable escape sequences:

```gorget
String raw = r"no \n escape here"
print(raw)    # no \n escape here
```

---

## Vectors

`Vector[T]` is a dynamic, growable array — the workhorse collection.

### Creation

```gorget
auto v = [10, 20, 30]                # literal — inferred as Vector[int]
Vector[int] empty = Vector[int]()    # empty vector with explicit type
Vector[str] names = Vector[str]()
```

### Basic Operations

```gorget
auto v = [10, 20, 30]

v.push(40)            # append
int len = v.len()     # 4
int first = v[0]      # subscript read: 10
v[1] = 99             # subscript write

for x in v:
    print("{x}")      # iterate
```

### Membership

```gorget
if 99 in v:
    print("found")
```

### Concatenation

```gorget
auto a = [1, 2, 3]
auto b = [4, 5]
auto c = a + b        # [1, 2, 3, 4, 5]
```

---

## Dicts

`Dict[K, V]` is an ordered hash map — insertion order is preserved.

### Creation

```gorget
auto d = {"x": 10, "y": 20}              # literal — Dict[str, int]
Dict[str, int] empty = Dict[str, int]()   # explicit type
```

### Basic Operations

```gorget
auto d = {"x": 10, "y": 20}

int val = d["x"]      # subscript read: 10
d["z"] = 30           # subscript write (insert)
d["x"] = 99           # subscript write (update)
int len = d.len()     # 3
```

### Iteration

```gorget
Dict[str, int] ages = Dict[str, int]()
ages.put("Alice", 30)
ages.put("Bob", 25)

for k, v in ages:
    print("{k}: {v}")
```

Iteration follows insertion order.

### Lookup with Option

```gorget
Option[int] result = ages.get("Alice")   # Some(30)
Option[int] missing = ages.get("Eve")    # None

int age = ages.get("Alice") ?? 0         # 30, or 0 if missing
```

---

## Sets

`Set[T]` is an unordered collection of unique elements.

```gorget
from std.collections import Set

Set[int] s = Set[int]()
s.add(1)
s.add(2)
s.add(2)           # no effect — already present
print("{s.len()}")  # 2

if 1 in s:
    print("found")
```

---

## Tuples

Tuples group a fixed number of values with potentially different types:

```gorget
auto pair = (10, "hello")
auto triple = (1, 2.0, true)
```

### Field Access

Access fields by index with `._0`, `._1`, etc.:

```gorget
auto pair = (10, 20)
print("{pair._0}")    # 10
print("{pair._1}")    # 20
```

### Tuple Unpacking

```gorget
auto x, y = (10, 20)
print("{x}")    # 10
print("{y}")    # 20
```

Works with function return values:

```gorget
str, int parse_header(str line):
    return "Content-Type", 200

auto name, code = parse_header("...")
```

### Nested Tuples

```gorget
auto nested = (1, (2, 3))
print("{nested._0}")       # 1
print("{nested._1._0}")    # 2
```

---

## Comprehensions

Comprehensions build collections from expressions:

### List Comprehension

```gorget
Vector[int] squares = [x * x for x in 0..5]
# [0, 1, 4, 9, 16]

Vector[int] evens = [x for x in 0..20 if x % 2 == 0]
# [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
```

### Set Comprehension

```gorget
Set[int] even_set = {x for x in 0..10 if x % 2 == 0}
```

### Dict Comprehension

```gorget
Dict[int, int] doubled = {x: x * 2 for x in 0..5}
# {0: 0, 1: 2, 2: 4, 3: 6, 4: 8}
```

All three forms support the optional `if` filter.

---

## Summary

| Type | Literal | Key Operations |
|------|---------|----------------|
| `str` | `"text"` | Borrowed, immutable, zero-cost |
| `String` | `"text"` (owned context) | Owned, concatenation with `+` |
| `Vector[T]` | `[1, 2, 3]` | `push`, `[]`, `len`, `for` |
| `Dict[K, V]` | `{"k": v}` | `[]`, `put`, `get`, `len`, ordered |
| `Set[T]` | (constructor) | `add`, `in`, `len`, unique elements |
| Tuple | `(a, b, c)` | `._0`, `._1`, unpacking |
| Comprehension | `[expr for x in items if cond]` | List, set, and dict forms |
