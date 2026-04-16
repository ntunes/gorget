# Standard Library Design: Narrow Waist Architecture

> Design document for Gorget's standard library API philosophy, trait layering,
> and method consolidation. Approved 2026-04-16.

## 1. Design Philosophy

### The Narrow Waist Principle

Gorget's standard library follows the **narrow waist** pattern: a small set of
orthogonal traits in the middle, with M producers on one side and N consumers
on the other. Cost is M+N, not M×N.

The Unix file descriptor interface is the model. Five operations —
`open`, `read`, `write`, `close`, `ioctl` — and suddenly files, pipes, sockets,
devices, and `/proc` entries are all interchangeable.

**Rules of thumb:**

1. **Very few methods per trait** — ideally one or two. Each additional method
   halves the number of types that can implement the interface.
2. **Parameters for variation, not new methods** — `find(pattern, reverse: true)`
   not `find()` + `rfind()`. The method count stays fixed as capabilities grow.
3. **Layers compose** — a buffered reader wraps a reader. A compressor wraps a
   writer. Neither knows what's underneath.
4. **Don't break POLA** — if users universally expect `contains()` on String,
   provide it. But implement it as a thin wrapper over the primitive (`find`).

### The Layered Stack

```
┌─────────────────────────────────────────────────┐
│  xtd.*                                          │
│  Domain libraries (JSON, HTTP, Regex, DB, ...)  │
│  Consumes traits; builds on std                 │
├─────────────────────────────────────────────────┤
│  std.*                                          │
│  System interfaces (fs, io, net, conv, ...)     │
│  Implements Reader/Writer/Iterable              │
├─────────────────────────────────────────────────┤
│  Derived power (equip on traits)                │
│  map, filter, fold, find, min, max, sum,        │
│  partition, group_by, zip, take, drop, join ... │
│  Free for all types that implement Iterator     │
├═════════════════════════════════════════════════╡ ← NARROW WAIST
│  Core traits (1-2 methods each)                 │
│  Iterator[T], Iterable[T], Writer, Reader,      │
│  Displayable, Equatable, Comparable, Hashable,  │
│  Cloneable, Default                             │
├─────────────────────────────────────────────────┤
│  Core types                                     │
│  String, Vector, Dict, Set, Option, Result      │
│  Minimal mutation + type-specific methods       │
└─────────────────────────────────────────────────┘
```

## 2. Trait Naming Convention

Two naming patterns for two kinds of traits:

| Kind | Suffix | Test | Examples |
|------|--------|------|----------|
| **Capability** — a property of a type that has other purposes | `-able` / `-ible` | "this type **is** X" | Equatable, Comparable, Hashable, Displayable, Cloneable, Iterable, Serializable |
| **Role** — the primary purpose of a type | `-er` / `-or` | "this type **is a** X" | Iterator, Writer, Reader, Formatter, Handler |
| **Operator / conversion** | bare verb/noun | standard math/conversion | Add, Sub, From, Into, Index, Default |
| **Domain** | bare noun | natural concept name | Shape, Collection |

**Updated guidance:** I/O traits use the -er pattern: `Writer`, `Reader` (not
`Writable`, `Readable`). This aligns with Go (`io.Reader`/`io.Writer`), Rust
(`Read`/`Write`), Java (`Reader`/`Writer`), and Gorget's existing `Iterator`.

## 3. Core Traits (The Narrow Waist)

### Role Traits (-er)

```gorget
trait Iterator[T]:
    Option[T] next(&self)

trait Writer:
    void write(String s, &self)

trait Reader:
    Result[String, String] read(&self, int n = 0)   # 0 = read all
```

### Capability Traits (-able)

```gorget
trait Iterable[T]:
    Iterator[T] iter(&self)

trait Equatable:
    bool eq(self, Self other)

trait Comparable:
    int compare(self, Self other)

trait Hashable:
    int hash(self)

trait Displayable:
    String display(self)

trait Cloneable:
    Self clone(self)

trait Default:
    Self default()
```

### Relationship: Iterable vs Iterator

`Iterable` can produce **multiple** iterators (you can iterate a Vector many
times). `Iterator` has cursor state and is consumed. Vector is Iterable. The
thing `iter()` returns is an Iterator. These are distinct concepts.

## 4. Iterator: The M+N Payoff

All higher-order operations are defined **once** on Iterator[T] via equip.
Any type that implements Iterable gets them all for free.

### Lazy by Default

Iterator adapter methods return Iterator (lazy). Call `collect()` to
materialize. This enables single-pass fusion:

```gorget
# Single pass, no intermediate allocations:
Vector[String] names = people.iter()
    .filter((p): p.age > 18)
    .map((p): p.name)
    .take(10)
    .collect()
```

### Eager Convenience on Collections

Vector/Dict/Set provide convenience methods that wrap iter + collect:

```gorget
equip Vector[T]:
    # Eager convenience — thin wrapper over lazy primitive:
    Vector[U] map[U](self, U(T) f):
        self.iter().map(f).collect()

    Vector[T] filter(self, bool(T) f):
        self.iter().filter(f).collect()
```

This gives both calling styles:

```gorget
# Eager (what Kotlin/Python users expect):
Vector[String] names = people.map((p): p.name)

# Lazy chain (what Rust users expect):
Vector[String] names = people.iter().filter(f).map(g).collect()
```

### Iterator Equip Methods

The full set of derived operations on Iterator[T]:

**Transformation (lazy, return Iterator):**
- `map[U](U(T) f)` — transform each element
- `filter(bool(T) f)` — keep elements matching predicate
- `filter_map[U](Option[U](T) f)` — transform + remove None
- `flat_map[U](Iterable[U](T) f)` — transform + flatten
- `enumerate()` — yield (int, T) pairs
- `zip[U](Iterable[U] other)` — pair elements from two iterators
- `chain(Iterable[T] other)` — concatenate two iterators
- `take(int n)` — first n elements
- `drop(int n)` — skip first n elements
- `take_while(bool(T) f)` — take while predicate holds
- `drop_while(bool(T) f)` — skip while predicate holds
- `inspect(void(T) f)` — side-effect, pass through (debugging)

**Aggregation (eager, consume the iterator):**
- `fold[U](U init, U(U, T) f)` — left fold
- `reduce(T(T, T) f)` — fold without initial value
- `any(bool(T) f)` — short-circuits on true
- `all(bool(T) f)` — short-circuits on false
- `count()` / `count(bool(T) f)` — total or matching count
- `sum()` — numeric sum (where T: Numeric)
- `min()` / `max()` — extrema (where T: Comparable)
- `min_by(int(T, T) f)` / `max_by(int(T, T) f)` — custom comparator

**Search (eager, may short-circuit):**
- `find(bool(T) f)` — first matching element
- `find_index(bool(T) f)` — index of first match
- `contains(T val)` — membership (where T: Equatable)

**Collection (eager, materialize):**
- `collect()` — to Vector[T]
- `to_set()` — to Set[T] (where T: Hashable)
- `join(String sep)` — to String (where T: Displayable)
- `partition(bool(T) f)` — split into (Vector[T], Vector[T])
- `group_by[K](K(T) f)` — to Dict[K, Vector[T]]
- `for_each(void(T) f)` — side-effecting iteration

### Implementation Phasing

1. **Phase 1 (now):** Define traits, implement Iterator methods as eager
   (returning Vector internally). The API surface is final; only the
   implementation changes later.
2. **Phase 2:** Add lazy iterator structs (compiler generates state machines
   for each adapter). Swap implementations — zero API change.
3. **Phase 3:** Advanced adapters (scan, intersperse, cycle) as demand arises.

## 5. Core Type APIs

### Constructors: Capacity via Optional Parameter

All collection types accept an optional `cap` parameter. No `with_capacity`
constructor — one constructor, one optional parameter:

```gorget
String s = String()                         # empty
String s = String(cap: 256)                 # preallocated builder

Vector[int] v = Vector[int]()              # default
Vector[int] v = Vector[int](cap: 1000)     # preallocated

Dict[String, int] d = Dict[String, int]()           # default
Dict[String, int] d = Dict[String, int](cap: 64)    # preallocated buckets

Set[String] s = Set[String]()              # default
Set[String] s = Set[String](cap: 32)       # preallocated buckets
```

### String

**Primitives (carry the weight):**

| Method | Signature | Notes |
|--------|-----------|-------|
| `find(pattern, from, reverse)` | `(String, int = 0, bool = false) -> Option[int]` | Unified search. The ONE search primitive. |
| `trim(side, chars)` | `(Side = Side.Both, String = " \t\n\r") -> String` | Unified trim. Side enum: Left, Right, Both. |
| `split(sep, limit)` | `(String, int = 0) -> Vector[String]` | limit=0 means unlimited. limit=2 is split_once. |
| `replace(old, new, limit)` | `(String, String, int = 0) -> String` | limit=0 means all. limit=1 is replace_first. |
| `push(s)` | `(String) -> void` | Mutable append (amortized O(1)). The builder primitive. |
| `push(b)` | `(byte) -> void` | Mutable append byte. |

**POLA convenience (thin wrappers, call primitives internally):**

| Method | Implementation | Why keep it |
|--------|---------------|-------------|
| `contains(s)` | `find(s).is_some()` | Universally expected |
| `starts_with(s)` | `find(s) == Some(0)` | Universally expected |
| `ends_with(s)` | `find(s, reverse: true)` + position check | Universally expected |
| `index_of(s)` | `find(s)` | Backward compat, familiar name |
| `trim_left(chars)` | `trim(side: Side.Left, chars)` | Discoverable alias |
| `trim_right(chars)` | `trim(side: Side.Right, chars)` | Discoverable alias |
| `lines()` | Specialized (handles `\n`, `\r\n`, `\r`) | Different semantics from split |

**Other methods (no change):**

`len`, `byte_len`, `is_empty`, `substring`, `byte_slice`, `byte_at`,
`to_upper`, `to_lower`, `repeat`, `pad_left`, `pad_right`,
`removeprefix`, `removesuffix`, `join`, `hash`,
`chars`, `bytes`, `codepoints`, `split`,
`is_alpha`, `is_digit`, `is_alphanumeric`, `is_whitespace`,
`is_upper`, `is_lower`, `is_hex_digit`, `is_ascii`,
`clear`, `capacity`.

**Removed:**

| Method | Replacement |
|--------|------------|
| `strip()` | `trim()` — was a synonym |
| `lstrip()` | `trim_left()` — renamed for clarity |
| `rstrip()` | `trim_right()` — renamed for clarity |
| `char_at()` | `byte_at()` — was deprecated |

**String implements:** Iterable[String] (iterate codepoints), Writer (push is write), Displayable, Equatable, Comparable, Hashable, Cloneable, Default.

### Vector[T]

**Mutation (type-specific, can't be derived from Iterator):**

| Method | Signature | Notes |
|--------|-----------|-------|
| `push(val)` | `(T) -> void` | Append |
| `pop()` | `() -> Option[T !]` | Remove + return last |
| `get(i)` | `(int) -> Option[T &]` | Safe indexed access |
| `set(i, val)` | `(int, T) -> void` | Replace at index |
| `insert(i, val)` | `(int, T) -> void` | Insert at index |
| `remove(i)` | `(int) -> Option[T !]` | Remove + return at index |
| `swap_remove(i)` | `(int) -> Option[T !]` | O(1) remove (swap with last) |
| `first()` | `() -> Option[T &]` | Borrow first |
| `last()` | `() -> Option[T &]` | Borrow last |
| `extend(other)` | `(Vector[T]) -> void` | Append all |
| `reserve(n)` | `(int) -> void` | Pre-allocate capacity |
| `clear()` | `() -> void` | Remove all, keep capacity |
| `retain(f)` | `(bool(T) f) -> void` | In-place filter |
| `fill(val)` | `(T) -> void` | Set all elements |
| `swap(i, j)` | `(int, int) -> void` | Swap two elements |
| `sort(by)` | `(int(T, T) = None) -> void` | In-place sort, optional comparator |
| `reverse()` | `() -> void` | In-place reverse |

**Non-mutating (type-specific):**

| Method | Signature | Notes |
|--------|-----------|-------|
| `len()` | `() -> int` | Element count |
| `is_empty()` | `() -> bool` | len() == 0 |
| `contains(val)` | `(T) -> bool` | Linear search |
| `index_of(val)` | `(T) -> Option[int]` | First occurrence |
| `binary_search(val)` | `(T) -> int` | Requires sorted |
| `sorted(by)` | `(int(T, T) = None) -> Vector[T]` | New sorted copy |
| `reversed()` | `() -> Vector[T]` | New reversed copy |
| `unique()` | `() -> Vector[T]` | New deduplicated copy |
| `slice(start, end)` | `(int, int) -> Vector[T]` | New sub-vector |

**From Iterator (eager convenience, thin wrappers):**

| Method | Wraps | Notes |
|--------|-------|-------|
| `map(f)` | `iter().map(f).collect()` | Eager transform |
| `filter(f)` | `iter().filter(f).collect()` | Eager filter |
| `flat_map(f)` | `iter().flat_map(f).collect()` | Eager transform + flatten |
| `filter_map(f)` | `iter().filter_map(f).collect()` | Eager map + remove None |
| `fold(init, f)` | `iter().fold(init, f)` | Aggregate |
| `reduce(f)` | `iter().reduce(f)` | Aggregate without init |
| `any(f)` | `iter().any(f)` | Short-circuit predicate |
| `all(f)` | `iter().all(f)` | Short-circuit predicate |
| `find(f)` | `iter().find(f)` | First matching element |
| `min()` / `max()` | `iter().min()` / `iter().max()` | Extrema |
| `sum()` | `iter().sum()` | Numeric sum |
| `partition(f)` | `iter().partition(f)` | Split by predicate |
| `group_by(f)` | `iter().group_by(f)` | Group by key |
| `join(sep)` | `iter().join(sep)` | Join to String |
| `take(n)` / `drop(n)` | `iter().take(n).collect()` | Prefix/suffix |
| `zip(other)` | `iter().zip(other).collect()` | Pair elements |
| `enumerate()` | `iter().enumerate().collect()` | Add indices |

**Vector implements:** Iterable[T], Equatable (where T: Equatable), Cloneable, Default, Displayable (where T: Displayable), Measurable.

### Dict[K, V]

**Mutation:**

| Method | Signature | Notes |
|--------|-----------|-------|
| `put(key, val)` | `(K, V) -> void` | Insert or update. THE one insert method. |
| `remove(key)` | `(K) -> Option[V !]` | Remove + return value. Changed from bool. |
| `get_or(key, default)` | `(K, V) -> V` | Eager default |
| `get_or_put(key, val)` | `(K, V) -> V` | Insert if absent, return |
| `clear()` | `() -> void` | Remove all |

**Access:**

| Method | Signature | Notes |
|--------|-----------|-------|
| `get(key)` | `(K) -> Option[V &]` | Borrow value |
| `contains(key)` | `(K) -> bool` | Key present. THE one name (not `has`). |
| `keys()` | `() -> Vector[K]` | All keys |
| `values()` | `() -> Vector[V]` | All values |
| `items()` | `() -> Vector[(K, V)]` | All pairs |
| `len()` | `() -> int` | Entry count |
| `is_empty()` | `() -> bool` | len() == 0 |

**Removed synonyms:**

| Removed | Replacement |
|---------|------------|
| `update(k, v)` | `put(k, v)` |
| `set(k, v)` | `put(k, v)` |
| `has(k)` | `contains(k)` |

**From Iterator:** `filter`, `fold`, `map_values`, `map_keys` via
`iter().method()` patterns.

**Dict implements:** Iterable[(K, V)], Equatable, Cloneable, Default, Measurable.

### Set[T]

**Mutation:**

| Method | Signature | Notes |
|--------|-----------|-------|
| `add(val)` | `(T) -> void` | Add element |
| `remove(val)` | `(T) -> bool` | Remove, returns true if existed |
| `clear()` | `() -> void` | Remove all |

**Access / Set Algebra:**

| Method | Signature | Notes |
|--------|-----------|-------|
| `contains(val)` | `(T) -> bool` | Membership |
| `union(other)` | `(Set[T]) -> Set[T]` | A ∪ B |
| `intersection(other)` | `(Set[T]) -> Set[T]` | A ∩ B |
| `difference(other)` | `(Set[T]) -> Set[T]` | A \ B |
| `symmetric_difference(other)` | `(Set[T]) -> Set[T]` | A △ B (new) |
| `is_subset(other)` | `(Set[T]) -> bool` | A ⊆ B |
| `is_superset(other)` | `(Set[T]) -> bool` | A ⊇ B |
| `is_disjoint(other)` | `(Set[T]) -> bool` | A ∩ B = ∅ (new) |
| `len()` | `() -> int` | Element count |
| `is_empty()` | `() -> bool` | len() == 0 |

**Set implements:** Iterable[T], Equatable, Cloneable, Default, Measurable.

### Option[T]

| Method | Signature | Notes |
|--------|-----------|-------|
| `unwrap()` | `() -> T` | Panic on None |
| `expect(msg)` | `(String) -> T` | Panic with message |
| `unwrap_or(default)` | `(T) -> T` | Eager default |
| `unwrap_or_else(f)` | `(T() f) -> T` | Lazy default |
| `unwrap_or_default()` | `() -> T` | Where T: Default (new) |
| `is_some()` | `() -> bool` | |
| `is_none()` | `() -> bool` | |
| `map(f)` | `(U(T) f) -> Option[U]` | Transform |
| `and_then(f)` | `(Option[U](T) f) -> Option[U]` | Monadic bind |
| `or_else(f)` | `(Option[T]() f) -> Option[T]` | Lazy fallback |
| `or(alt)` | `(Option[T]) -> Option[T]` | Eager fallback |
| `filter(f)` | `(bool(T) f) -> Option[T]` | Conditional None |
| `flatten()` | `() -> Option[T]` | Unwrap nested |
| `ok_or(err)` | `(E) -> Result[T, E]` | Convert to Result (new) |

### Result[T, E]

| Method | Signature | Notes |
|--------|-----------|-------|
| `unwrap()` | `() -> T` | Panic on Error |
| `expect(msg)` | `(String) -> T` | Panic with message |
| `unwrap_or(default)` | `(T) -> T` | Eager default |
| `unwrap_or_else(f)` | `(T(E) f) -> T` | Lazy default |
| `unwrap_error()` | `() -> E` | Panic on Ok |
| `is_ok()` | `() -> bool` | |
| `is_error()` | `() -> bool` | |
| `map(f)` | `(U(T) f) -> Result[U, E]` | Transform success |
| `map_err(f)` | `(F(E) f) -> Result[T, F]` | Transform error |
| `and_then(f)` | `(Result[U, E](T) f) -> Result[U, E]` | Chain |
| `or_else(f)` | `(Result[T, F](E) f) -> Result[T, F]` | Fallback |
| `or(alt)` | `(Result[T, F]) -> Result[T, F]` | Eager fallback |
| `ok()` | `() -> Option[T]` | Extract success as Option (new) |
| `err()` | `() -> Option[E]` | Extract error as Option (new) |

## 6. Writer / Reader Traits

### Writer — The Narrow Waist for Output

```gorget
trait Writer:
    void write(String s, &self)
```

One method. Implementors:

| Type | Behavior |
|------|----------|
| String | `push(s)` — mutable append to buffer |
| File | Write to file descriptor |
| Socket / TlsSocket | Write to network |
| stdout / stderr | Write to standard streams |

A function that produces text doesn't need to know the destination:

```gorget
void render_html(Writer &out, Page page):
    out.write("<html>")
    out.write(page.body)
    out.write("</html>")

# Works with any Writer:
String buf = String(cap: 4096)
render_html(&buf, page)

File f = open("out.html")
render_html(&f, page)
```

### Reader — The Narrow Waist for Input

```gorget
trait Reader:
    Result[String, String] read(&self, int n = 0)   # 0 = read all
```

One method. Implementors: File, Socket, TlsSocket, stdin.

### Displayable vs Writer

These are orthogonal:

- **Displayable** = "what" — converts a value to String (`String display(self)`)
- **Writer** = "where" — receives strings (`void write(String s, &self)`)

`print(x)` is: `stdout.write(x.display() + "\n")`. Takes Displayable, writes to
stdout (a Writer).

## 7. The Side Enum

Used by String.trim and potentially other methods:

```gorget
enum Side:
    Left
    Right
    Both
```

Defined in the prelude. Available without import.

## 8. Implementation Plan

### Phase 1: Foundation (constructors + String builder + trait rename)

1. Add `cap` parameter to String/Vector/Dict/Set constructors
2. Wire up String mutation methods (`push`, `push_char`, `clear`, `capacity`)
   in type checker and GIR lowering — runtime already has them
3. Add `find(pattern, from, reverse)` as String primitive
4. Consolidate `trim`/`lstrip`/`rstrip`/`strip` into `trim(side, chars)`
   with `trim_left`/`trim_right` aliases
5. Add `limit` parameter to `split` and `replace`
6. Add `lines()` method on String
7. Remove Dict synonyms (`update`, `set`, `has`)
8. Change Dict.remove to return `Option[V]`
9. Add `sort(by)` / `sorted(by)` optional comparator parameter
10. Add Set.symmetric_difference and Set.is_disjoint

### Phase 2: Iterator equip + HOFs

1. Define Iterator[T] equip with all derived methods (eager initially)
2. Add Vector convenience wrappers for common HOFs
3. Add new methods: `flat_map`, `filter_map`, `min`, `max`, `sum`,
   `partition`, `group_by`, `take`, `drop`, `zip`, `join` on Vector
4. Add `swap_remove`, `retain`, `fill`, `swap` on Vector

### Phase 3: Writer/Reader traits

1. Define Writer and Reader traits
2. Implement Writer on String, File, Socket, stdout, stderr
3. Implement Reader on File, Socket, stdin
4. Refactor `print()` to use Displayable

### Phase 4: Lazy iterators

1. Compiler support for iterator state machine structs
2. Convert Iterator adapter methods to return lazy iterators
3. Add `collect()` terminal
4. No API change — call sites remain identical

### Phase 5: Documentation updates

1. Update `docs/language-design.md` §4.4.1 — add Writer/Reader to -er examples
2. Update `docs/language-reference.md` §15.2 — all method signature changes
3. Update `docs/book/05-collections.md` — new methods, consolidated API
4. Update `docs/book/appendix-traits.md` — add Writer, Reader; note convention
5. Update `docs/book/19-stdlib.md` — reflect new std/xtd layering

## 9. Migration / Backward Compatibility

| Change | Impact | Migration |
|--------|--------|-----------|
| `strip()` removed | Low — synonym for `trim()` | Find-replace |
| `lstrip()`/`rstrip()` renamed | Low | `lstrip()` → `trim_left()`, `rstrip()` → `trim_right()` |
| `has()` removed from Dict | Low — synonym for `contains()` | Find-replace |
| `update()`/`set()` removed from Dict | Medium | `update(k,v)` → `put(k,v)`, `set(k,v)` → `put(k,v)` |
| `char_at()` removed | Low — was deprecated | Use `byte_at()` or `substring()` |
| Dict.remove returns Option[V] | Medium | Code expecting bool needs update |
| `find()` on String (was removed) | Restored | New unified search with parameters |

All removals can have a deprecation period where the old name emits a
compiler warning pointing to the replacement.
