# Strings and Collections

This chapter covers Gorget's built-in data structures: strings, vectors, dicts,
hash maps, sets, arrays, tuples, and comprehensions.

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

Use f-strings to embed any expression inside `{}`:

```gorget
str name = "Alice"
int age = 30
print(f"Name: {name}, Age: {age}")
```

The `f` prefix is required — without it, `{name}` would be literal text.
For literal braces, double them: `f"{{x}}"` prints `{x}`.

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
    print(f"{x}")      # iterate
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
    print(f"{k}: {v}")
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
print(f"{s.len()}")  # 2

if 1 in s:
    print("found")
```

### Set Operations

```gorget
Set[int] a = Set[int]()
a.add(1)
a.add(2)
a.add(3)

Set[int] b = Set[int]()
b.add(2)
b.add(3)
b.add(4)

Set[int] both = a.intersection(b)     # {2, 3}
Set[int] either = a.union(b)          # {1, 2, 3, 4}
Set[int] only_a = a.difference(b)     # {1}
bool sub = a.is_subset(b)             # false
```

---

## HashMap and HashSet

`Dict` preserves insertion order. When order doesn't matter and you want maximum
performance, use `HashMap[K, V]` instead. It has the same API as `Dict`:

```gorget
from std.collections import HashMap

HashMap[str, int] counts = HashMap[str, int]()
counts.put("apple", 3)
counts.put("banana", 5)
int c = counts.get("apple") ?? 0      # 3
```

Similarly, `HashSet[T]` is the unordered counterpart to `Set[T]`:

```gorget
from std.collections import HashSet

HashSet[int] seen = HashSet[int]()
seen.add(10)
seen.add(20)
if 10 in seen:
    print("found")
```

**When to use which:** Use `Dict`/`Set` when you need deterministic iteration
order (tests, serialization, user-facing output). Use `HashMap`/`HashSet` when
order is irrelevant and raw throughput matters.

---

## Arrays

For fixed-size data where the length is known at compile time, use arrays:

```gorget
int[5] arr = [1, 2, 3, 4, 5]     # fixed C-level array
int first = arr[0]                 # subscript read: 1
arr[2] = 99                       # subscript write
```

Array size must be a compile-time constant. Arrays are stack-allocated and have
no runtime overhead.

**Arrays vs Vectors:** Use `auto` with a literal to get a `Vector[T]` (dynamic,
supports `push`/`pop`/`len`). Use an explicit array type like `int[5]` when you
need a fixed-size, stack-allocated buffer:

```gorget
auto dynamic = [1, 2, 3]          # Vector[int] — growable
int[3] fixed = [1, 2, 3]          # int[3] — fixed size, stack-allocated
```

### Slices

A slice is a borrowed view into contiguous memory — an array or vector section
without copying:

```gorget
int[5] arr = [1, 2, 3, 4, 5]
int[] middle = arr[1..4]           # borrowed view: [2, 3, 4]
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
print(f"{pair._0}")    # 10
print(f"{pair._1}")    # 20
```

### Tuple Unpacking

```gorget
auto x, y = (10, 20)
print(f"{x}")    # 10
print(f"{y}")    # 20
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
print(f"{nested._0}")       # 1
print(f"{nested._1._0}")    # 2
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
| `HashMap[K, V]` | (constructor) | Same API as `Dict`, unordered, faster |
| `Set[T]` | (constructor) | `add`, `in`, `len`, unique elements |
| `HashSet[T]` | (constructor) | Same API as `Set`, unordered, faster |
| `T[N]` (array) | `int[5] a = [...]` | Fixed-size, stack-allocated |
| `T[]` (slice) | `arr[1..4]` | Borrowed view into contiguous memory |
| Tuple | `(a, b, c)` | `._0`, `._1`, unpacking |
| Comprehension | `[expr for x in items if cond]` | List, set, and dict forms |
