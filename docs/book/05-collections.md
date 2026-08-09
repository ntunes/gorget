# Strings and Collections

This chapter covers Gorget's built-in data structures: strings, vectors, dicts,
hash maps, sets, arrays, tuples, and comprehensions.

---

## Strings

### String

Gorget has a single string type: **`String`** — a 32-byte value with copy-on-write
semantics. Literals and slicing operations are zero-allocation **views**; concatenation
and f-strings produce **owned** copies. The compiler auto-materializes views when the
source is mutated — you just write `String` everywhere.

```gorget
String greeting = "hello"     # literal — view (no allocation)
String combined = "a" + "b"   # concatenation — owned (heap-allocated)
```

String literals, function parameters, and for-loop bindings are typically inferred
as views (zero-cost, no allocation). Concatenation, f-strings, and methods like
`to_upper()` produce owned strings.

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
String name = "Alice"
int age = 30
print(f"Name: {name}, Age: {age}")
```

The `f` prefix is required — without it, `{name}` would be literal text.
For literal braces, double them: `f"{{x}}"` prints `{x}`.

### Common String Methods

```gorget
String s = "Hello, World!"

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

Vector[String] parts = "a,b,c".split(",")   # ["a", "b", "c"]
Option[int] pos = s.index_of(",")           # Some(5)
String sub = s.substring(0, 5)              # "Hello"
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
Vector[String] names = Vector[String]()

# Pre-allocate when you know the size — avoids reallocation during growth.
Vector[int] big = Vector[int](cap=10_000)
```

### Basic Operations

```gorget
auto v = [10, 20, 30]

v.push(40)            # append
int len = v.len()     # 4
int cap = v.capacity()  # current allocated capacity
int first = v[0]      # subscript read: 10
v[1] = 99             # subscript write

for x in v:
    print(f"{x}")      # iterate
```

### Sort

```gorget
auto v = [3, 1, 4, 1, 5, 9, 2, 6]

v.sort()                            # in place, natural order
Vector[int] sorted = v.sorted()     # returns new sorted vector

# Custom comparator (sort / sorted take an optional closure).
v.sort((a, b): b - a)               # descending

# Sort by key — avoids recomputing the key per comparison.
Vector[String] words = ["banana", "fig", "apple"]
words.sort_by_key((s): s.len())     # by length

Vector[String] sorted_by_len = words.sorted_by_key((s): s.len())
```

### Windows and Chunks

```gorget
auto v = [1, 2, 3, 4, 5]

# Fixed-width sliding windows (size 3): [[1,2,3], [2,3,4], [3,4,5]]
Vector[Vector[int]] wins = v.windows(3)

# Non-overlapping chunks (size 2): [[1,2], [3,4], [5]]
Vector[Vector[int]] chs = v.chunks(2)
```

### Swap and Fill

```gorget
Vector[int] v = [10, 20, 30, 40, 50]

# swap(i, j) — in-place swap.
v.swap(0, 4)                # [50, 20, 30, 40, 10]

# swap_remove(i) — O(1) removal by moving the last element into the
# hole. Order-destroying. Prefer over `remove(i)` when order doesn't
# matter — saves the N-i shift.
v.swap_remove(1)            # [50, 10, 30, 40] (10 filled the hole)

# fill(n, v) — replace contents with n copies of v. Drops existing.
Vector[int] zeros = Vector[int]()
zeros.fill(5, 0)            # [0, 0, 0, 0, 0]
```

### Index Access and Ownership

`v[i]` borrows the element — it doesn't remove it from the vector. For simple types like `int` or `bool`, the value is just copied. For resource types like `Vector` or `String`, a bare-assign borrows (whether `auto` or explicitly typed); the borrow CoW-severs on the first mutation. For an independent owned copy up front, call `.clone()`:

```gorget
Vector[Vector[int]] matrix = [[1, 2, 3], [4, 5, 6]]
Vector[int] row = matrix[0]    # borrow — CoW-severs on mutation; matrix still has both rows
print(f"{row.len()}")          # 3
print(f"{matrix.len()}")       # still 2

Vector[int] owned = matrix[0].clone()  # independent owned copy (explicit)
```

To move an element out (no clone), use a consuming method:

```gorget
Vector[int] first = matrix.remove(0).unwrap()   # removes row 0, shifts row 1 down
print(f"{matrix.len()}")               # now 1
```

Other consuming methods: `pop()` removes the last element, `insert(i, !val)` inserts at a position.

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

### Iteration with Adapters

`for x in v` is the everyday form. For chained transformations, every
collection's `.iter()` returns a concrete `Iterator[T]` state machine:
`VectorIter[T]` for `Vector`, `SetIter[T]` for `Set`, `DictIter[K, V]`
for `Dict`. Adapter methods (`take` / `skip` / `map` / `filter` /
`enumerate` / …) are inherited from the `Iterator[T]` trait and return
nested adapter structs (`TakeIter[VectorIter[int], int]` etc.) — each
adapter is stored by value, no boxing, no trait-object dispatch, and
the whole chain fuses at monomorphization.

```gorget
Vector[int] v = [10, 20, 30, 40, 50]

# `.iter()` yields a VectorIter[T]; adapter methods come from the
# Iterator[T] trait. Inferred return types let chains compose freely.
for x in v.iter().take(3):
    print(x)                # 10 20 30

for x in v.iter().skip(2):
    print(x)                # 30 40 50

Vector[int] w = [100, 200]
for x in v.iter().chain(w.iter()):    # chain is VectorIter-specific
    print(x)                # 10 20 30 40 50 100 200
```

Most callers don't have to spell the adapter chain type — `auto` and
`for-in` infer it. When you do need an explicit type, write the full
nested form:

```gorget
TakeIter[VectorIter[int], int] prefix = v.iter().take(3)
for x in prefix:
    print(x)
```

**Eager terminals** are inherited from `Iterator[T]` as default
methods — call them at the end of the chain:

```gorget
int n          = v.iter().count()                    # 5
Vector[int] xs = v.iter().take(3).collect()          # [10, 20, 30]
int folded     = v.iter().fold(1, (acc, x): acc * x) # product via fold
bool has_big   = v.iter().any((x): x > 100)
Option[int] hit = v.iter().find((x): x > 25)         # Some(30)
```

Bound-needing terminals (`min`, `max`, `contains`, `sum`, `product`,
`join`) all ship as `Iterator[T]` defaults too. The compiler's
demand-gate only specialises each terminal for `T`s that satisfy
its bound (Comparable / Equatable / Numeric / Displayable):

```gorget
int total       = v.iter().sum()              # 150
int prod        = v.iter().product()          # element product
Option[int] lo  = v.iter().min()              # Some(10)
Option[int] hi  = v.iter().max()              # Some(50)
bool has50      = v.iter().contains(50)       # true
String csv      = v.iter().join(", ")         # "10, 20, 30, 40, 50"
```

`collect()` infers its target from the LHS binding type — Vector, Set,
or Dict (when the iterator yields tuples):

```gorget
Vector[int] dups = [1, 1, 2, 3, 3, 3]
Set[int] uniq    = dups.iter().collect()                  # → Set[int]
Vector[(int, int)] pairs = [(1, 10), (2, 20)]
Dict[int, int] d = pairs.iter().collect()                 # → Dict[int, int]
```

`Set.iter()` and `Dict.iter()` are **lazy** bucket walks — no
materialisation of `.items()` first:

```gorget
Set[int] s = Set[int]()
s.add(1); s.add(2); s.add(3)
for x in s.iter().take(2):
    print(x)                # walks two buckets, stops

Dict[String, int] ages = Dict[String, int]()
ages.put("Alice", 30)
ages.put("Bob", 25)
for p in ages.iter():
    print(f"{p.0}: {p.1}")  # yields (K, V) tuples lazily
```

---

## Dicts

`Dict[K, V]` is an ordered hash map — insertion order is preserved.

### Creation

```gorget
auto d = {"x": 10, "y": 20}              # literal — Dict[String, int]
Dict[String, int] empty = Dict[String, int]()   # explicit type
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
Dict[String, int] ages = Dict[String, int]()
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

`Set[T]` is a collection of unique elements that remembers insertion order — the
same pairing as `Dict` and `HashMap`, one step down: `Set` keeps order, `HashSet`
trades it for speed. If you are coming from Python, note the difference: Python's
`dict` is ordered but its `set` is not, and Gorget closes that gap so the ordered
and unordered choice is yours for both maps and sets.

Order is not the same as indexing, though. `s[0]` does not compile, because a
set's elements *are* its keys — over a `Set[int]`, `s[0]` could equally mean "the
first element" or "the element `0`", so Gorget declines to guess. Reach for
`s.items()[0]` when you want a position.

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

HashMap[String, int] counts = HashMap[String, int]()
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

Slice syntax `v[a:b]` (canonical) produces a semantically OWNED sub-sequence
value for both `String` and `Vector[T]` receivers. The runtime may back a
`String` slice with a `cap == 0` view as an invisible optimisation — you
cannot observe the distinction from source. Bounds CLAMP Python-style:

```gorget
String text = "hello"
String sub = text[1:4]             # "ell" (String slice, view-backed at runtime)
String tail = text[2:]             # "llo" — implicit end = len
String head = text[:3]             # "hel" — implicit start = 0
String full = text[:]              # "hello" — full copy

Vector[int] v = [1, 2, 3, 4, 5]
Vector[int] mid = v[1:4]           # [2, 3, 4] — independent OWNED copy
Vector[int] safe = v[0:1000]       # [1..5] — end clamps to v.len(); no fault
Vector[int] empty = v[3:1]         # [] — start > end yields empty
```

Four accept-forms: `v[a:b]`, `v[a:]`, `v[:b]`, `v[:]`. Two forms are
DEFERRED and rejected at parse time:

- `v[-1:b]` — negative-literal indices (`E_NegativeSliceIndex`). Write
  `v[v.len() - 1:b]` for an offset-from-end index.
- `v[a:b:c]` — step (`E_SliceStepDeferred`). Use `v.reversed()` for reverse
  iteration.

Runtime-negative values (a variable that goes negative at run time) still
clamp — a slice never faults at runtime.

The general `T[]` slice type exists in the grammar but is **not yet
implemented** for arrays and vectors as a distinct return type; `v[a:b]`
returns a fresh `Vector[T]` copy instead.

The `..` range spelling (`v[1..4]`) is still accepted during the D22
migration window and is identical in meaning; `gg fmt` preserves whichever
form you wrote.

---

## Tuples

Tuples group a fixed number of values with potentially different types:

```gorget
auto pair = (10, "hello")
auto triple = (1, 2.0, true)
```

### Field Access

Access fields by index. Both the bare-int form (`pair.0`) and the underscore
alias (`pair._0`) work:

```gorget
auto pair = (10, 20)
print(f"{pair.0}")     # 10
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
String, int parse_header(String line):
    return "Content-Type", 200

auto name, code = parse_header("...")
```

### Nested Tuples

Nested access composes with either form (`nested.1.0` or `nested._1._0`):

```gorget
auto nested = (1, (2, 3))
print(f"{nested.0}")        # 1
print(f"{nested.1.0}")      # 2
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

## Higher-Order Methods

Vectors, dicts, sets, and hash maps all support functional-style operations. These
methods take closures and return new collections — the originals are not modified.

### Vector: map, filter, fold, reduce

```gorget
auto numbers = [1, 2, 3, 4, 5]

Vector[int] doubled = numbers.map((int x): x * 2)     # [2, 4, 6, 8, 10]
Vector[int] evens = numbers.filter((int x): x % 2 == 0) # [2, 4]
int total = numbers.fold(0, (int acc, int x): acc + x) # 15
int product = numbers.reduce((int a, int b): a * b)    # 120
```

`fold` takes an initial accumulator value. `reduce` uses the first element as the
initial value — it panics on an empty vector.

### Vector: any, all

```gorget
auto numbers = [1, 2, 3, 4, 5]

bool has_even = numbers.any((int x): x % 2 == 0)     # true
bool all_positive = numbers.all((int x): x > 0)      # true
```

### Vector: sort, sorted

```gorget
auto v = [3, 1, 4, 1, 5]

v.sort()                       # in-place: [1, 1, 3, 4, 5]
auto copy = v.sorted()         # new sorted copy, original unchanged
```

Elements must implement the `Comparable` trait. All primitive types do.

### Implicit `it` Closures

For single-parameter closures, you can use the implicit `it` parameter:

```gorget
auto names = ["Alice", "Bob", "Charlie"]
auto lengths = names.map(it.len())           # [5, 3, 7]
auto long = names.filter(it.len() > 3)      # ["Alice", "Charlie"]
```

### Dict and Set

Dicts support direct named HOFs (`any` / `all` / `each` / `find` / `fold` /
`filter`). The closure takes key and value as two separate arguments — the
natural shape for a Dict:

```gorget
auto scores = {"Alice": 90, "Bob": 75, "Carol": 85}

bool any_high  = scores.any((String k, int v): v >= 90)
bool all_pass  = scores.all((String k, int v): v >= 60)
auto passing   = scores.filter((String k, int v): v >= 80)
int  total     = scores.fold(0, (int acc, String k, int v): acc + v)
```

When you need to **compose** transformations (filter → map → take → ...),
use the iterator chain. `d.iter()` yields `(K, V)` tuples, and closure
tuple destructuring binds the components as named locals:

```gorget
# Filter, transform, then aggregate — direct .filter / .any can't chain like this:
int total_high = scores
    .iter()
    .filter(((String k, int v)): v >= 80)
    .fold(0, (int acc, (String k, int v)): acc + v)

# Project to keys via destructuring; underscore ignores the value:
Vector[String] high_names = scores
    .iter()
    .filter(((String k, int v)): v >= 80)
    .map(((String k, int _v)): k)
    .collect()
```

Both shapes coexist by use case:

| Use case                                            | Use            |
|-----------------------------------------------------|----------------|
| One-shot predicate / each / fold on a Dict          | Direct named HOF |
| Filter / map / take / zip / chain in the middle     | Iterator chain   |

Sets support `filter`, `fold`, `any`, and `all` with single-argument closures:

```gorget
from std.collections import Set

Set[int] s = Set[int]()
s.add(1)
s.add(2)
s.add(3)

bool has_even = s.any((int x): x % 2 == 0)   # true
```

---

## Built-in Functions for Collections

Several functions are available without any import. They work with any collection
that implements the `Iterable` trait.

### range

Creates a range of integers. Used in `for` loops and comprehensions:

```gorget
for i in range(5):
    print(f"{i}")          # 0, 1, 2, 3, 4

for i in range(2, 8):
    print(f"{i}")          # 2, 3, 4, 5, 6, 7
```

The range syntax `0..5` is equivalent to `range(0, 5)`.

### enumerate

Iterates with an index:

```gorget
auto names = ["Alice", "Bob", "Carol"]

for i, name in enumerate(names):
    print(f"{i}: {name}")
# 0: Alice
# 1: Bob
# 2: Carol
```

### zip

Combines two collections element-by-element:

```gorget
auto names = ["Alice", "Bob"]
auto ages = [30, 25]

for name, age in zip(names, ages):
    print(f"{name} is {age}")
# Alice is 30
# Bob is 25
```

Stops at the shorter collection.

### map and filter (free functions)

`map` and `filter` are also available as free functions (in addition to methods):

```gorget
auto doubled = map([1, 2, 3], (int x): x * 2)
auto evens = filter([1, 2, 3, 4], (int x): x % 2 == 0)
```

### type

Returns the runtime type name of any value:

```gorget
int x = 42
print(type(x))             # "int"

auto v = [1, 2, 3]
print(type(v))             # "Vector[int]"
```

---

## Summary

| Type | Literal | Key Operations |
|------|---------|----------------|
| `String` | `"text"` | Provenance-inferred (view or owned) |
| `Vector[T]` | `[1, 2, 3]` | `push`, `[]`, `len`, `for` |
| `Dict[K, V]` | `{"k": v}` | `[]`, `put`, `get`, `len`, ordered |
| `HashMap[K, V]` | (constructor) | Same API as `Dict`, unordered, faster |
| `Set[T]` | (constructor) | `add`, `in`, `len`, unique elements |
| `HashSet[T]` | (constructor) | Same API as `Set`, unordered, faster |
| `T[N]` (array) | `int[5] a = [...]` | Fixed-size, stack-allocated |
| Colon-slice (D22) | `s[1:4]`, `v[1:]`, `v[:3]`, `v[:]` | Owned sub-sequence for `String` + `Vector[T]`; bounds CLAMP; four accept-forms; negatives + step deferred at parse. `s[1..4]` still accepted during migration. |
| Tuple | `(a, b, c)` | `._0`, `._1`, unpacking |
| Comprehension | `[expr for x in items if cond]` | List, set, and dict forms |
