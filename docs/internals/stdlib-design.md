# Standard Library Design: Narrow Waist Architecture

> Design document for Gorget's standard library API philosophy, trait layering,
> and method consolidation. Approved 2026-04-16. **Revised 2026-04-17** after
> independent review — concrete-type Iterator returns, sigil-based ownership on
> iterated elements, byte-shaped Writer/Reader, typed IoError, Debuggable trait,
> Hasher-state Hashable, spawn-boundary concurrency model.

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
| **Capability** — a property of a type that has other purposes | `-able` / `-ible` | "this type **is** X" | Equatable, Comparable, Hashable, Displayable, **Debuggable**, Cloneable, Iterable, IntoIterable, Serializable |
| **Role** — the primary purpose of a type | `-er` / `-or` | "this type **is a** X" | Iterator, Writer, Reader, Hasher, Formatter, Handler |
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
    # T carries its own ownership sigil (see §4.2):
    #   Iterator[Foo &]  — yields mutable borrows (source survives)
    #   Iterator[Foo !]  — yields owned moves (source is consumed)

trait Writer:
    Result[int, IoError] write(&self, Str bytes)
    # Returns bytes actually written — may be less than bytes.len
    # for sockets, pipes, and other short-write destinations.
    # Use the derived `write_all` for "must complete or fail" semantics.

trait Reader:
    Result[int, IoError] read(&self, Vector[byte] &buf)
    # Fills `buf` up to its capacity; returns bytes read (0 = EOF).
    # `buf` is borrowed — the Reader writes into the caller's buffer.

trait Hasher:
    void write(&self, Str bytes)
    int finish(self)
    # Hash state. Hashable types forward field bytes into a Hasher; the
    # concrete algorithm (SipHash, FxHash, ...) is chosen by the consumer.
```

### Capability Traits (-able / -ible)

```gorget
trait Iterable[T]:
    # Non-consuming iteration. Default; `for x in v` desugars to this.
    # The returned iterator type is concrete (monomorphized per Self),
    # not a trait object — see §4.1.
    type Iter: Iterator[T &]
    Self::Iter iter(&self)

trait IntoIterable[T]:
    # Consuming iteration. Source is moved into the iterator.
    type IntoIter: Iterator[T !]
    Self::IntoIter into_iter(!self)

trait Equatable:
    bool eq(self, Self other)

trait Comparable:
    int compare(self, Self other)

trait Hashable:
    # State-based hashing: composes — struct impls forward field hashes
    # into the same Hasher without re-inventing combine logic.
    void hash(self, Hasher &h)

trait Displayable:
    # User-facing representation. Hand-written, not derived.
    String display(self)

trait Debuggable:
    # Developer-facing representation. Derivable via @derive(Debug).
    # {v} in f-strings calls display(); {v:?} calls debug().
    String debug(self)

trait Cloneable:
    Self clone(self)

trait Default:
    Self default()
```

### Relationship: Iterable vs IntoIterable vs Iterator

- **Iterable** — can produce many iterators; source survives. `v.iter()`
  yields `T &` (mut borrow). This is what `for x in v` desugars to.
- **IntoIterable** — single consuming iteration; source is moved. `v.into_iter()`
  yields `T !` (owned). Use when you want to transfer elements into a new
  collection without cloning.
- **Iterator** — cursor state, generic over what it yields. A single
  `Iterator[T]` trait backs both iteration modes; the ownership story lives
  in `T`'s sigil, not in a separate trait.

### Concrete Return, Not Trait Object

`iter()` and `into_iter()` return a **concrete type** (specific to the collection
and any adapter chain), not a trait object. This matters:

- `.iter().filter(f).map(g).take(10).collect()` must monomorphize so closures
  inline and the adapter chain fuses into a single loop.
- Trait-object iteration is virtual-dispatch per element, defeats inlining,
  and makes §4's lazy fusion structurally impossible.

The cost is type-signature verbosity (`Map[Filter[VectorIter[T], F], G]`).
Library authors see these; users mostly don't. An `impl Iterator[T]`–style
abbreviation may follow once the system is in place.

## 4. Iterator: The M+N Payoff

All higher-order operations are defined **once** on `Iterator[T]` via equip.
Any type that implements `Iterable` or `IntoIterable` gets them all for free.

### 4.1 Lazy by Default (No Eager Transition)

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

**Design change from the original plan:** we do **not** ship an "eager
interim" implementation where adapters allocate intermediate Vectors. An
eager `iter()` slower than direct `vec.map(f)` trains users to stay on the
eager Vector API permanently, so lazy `Iterator` never actually gets used.
Better to delay Phase 2 until the state-machine infrastructure is in place.

### 4.2 Ownership Sigils: `T &` vs `T !`

`Iterator[T]`'s parameter `T` carries an ownership sigil that controls
what each element yields. Gorget supports two tiers:

| Instantiation | Yields | Source after | Typical producer |
|---------------|--------|--------------|------------------|
| `Iterator[T &]` | mutable borrow | alive | `Iterable.iter()` |
| `Iterator[T !]` | owned move | consumed | `IntoIterable.into_iter()` |

One trait, two instantiations. HOFs (`map`, `filter`, `fold`, …) are defined
once on `Iterator[T]` and work for both — closures receive whatever `T`
resolves to.

```gorget
Vector[String] names = load_names()

# Non-consuming iteration — `names` survives:
for name in names.iter():           # yields String & (mut borrow)
    name.push("!")                  # callee may mutate through &
process(names)                      # still alive

# Consuming iteration — move elements out, `names` is gone:
Set[String] unique = Set[String]()
for name in !names.into_iter():     # yields String ! (move)
    unique.add(!name)               # transfer ownership, no clone
# `names` is moved; using it again is a compile error

# Functional chain — whole pipeline monomorphizes:
Vector[int] lens = names.iter().map((s &): s.len()).collect()
Set[String] set = names.into_iter().collect()
```

**The read-only / const-borrow tier (`Iterator[T]` with bare `T`) is
deferred.** Gorget's `&` is not Rust-style exclusive access (see §8), so
a signature-level "no mutation" contract is weaker than in Rust and not
worth the extra surface in v1. Bare-`T` remains reserved; the third tier
can be added later purely additively.

#### Type-System Prerequisites

Sigils currently appear only at argument-name positions (`Type &name`,
`Type !name`). Iterator requires them at **type-argument positions**
(`Iterator[String &]`). That's a genuine grammar extension — the parser,
resolver, type inference, monomorphizer, and borrow checker all need to
accept sigils inside `[...]`. Call this out in Phase 2's scope: it is not
"purely additive" — it touches every type-system pass.

### 4.3 Copy-Type Interaction

For Copy types (integers, floats, bools, small POD structs), `Iterator[int &]`,
`Iterator[int !]`, and (eventually) `Iterator[int]` are **distinct at the
type level but runtime-identical** — a 4-byte int is copied regardless of
sigil. Design decision:

- **Types stay distinct** for grammar uniformity — no special-casing of
  Copy in the type system.
- **The borrow check and deref are no-ops on Copy**, and the backend
  emits the same code for all three forms.
- **Implicit auto-deref** at the call site: passing `int &` where `int`
  is expected (or vice versa) is accepted for Copy types only. Callers
  don't need to write `*iter.next()` for `int`.

This keeps the grammar clean and avoids every generic iterator consumer
having to special-case primitives.

### 4.4 Iterator Equip Methods

The full set of derived operations on `Iterator[T]`:

**Transformation (lazy, return Iterator):**
- `map[U](U(T) f)` — transform each element
- `filter(bool(T) f)` — keep elements matching predicate
- `filter_map[U](Option[U](T) f)` — transform + remove None
- `flat_map[U](Iterable[U](T) f)` — transform + flatten
- `enumerate()` — yield `(int, T)` pairs
- `zip[U](Iterable[U] other)` — pair elements from two iterators
- `chain(Iterable[T] other)` — concatenate two iterators
- `take(int n)` / `drop(int n)` — prefix / skip first n
- `take_while(bool(T) f)` / `drop_while(bool(T) f)` — conditional prefix / skip
- `windows(int n)` / `chunks(int n)` — sliding / non-overlapping slices
- `inspect(void(T) f)` — side-effect, pass through (debugging)

**Aggregation (eager, consume the iterator):**
- `fold[U](U init, U(U, T) f)` — left fold
- `reduce(T(T, T) f)` — fold without initial value
- `any(bool(T) f)` / `all(bool(T) f)` — short-circuit predicates
- `count()` / `count(bool(T) f)` — total or matching count
- `sum()` — numeric sum (where T: Numeric)
- `min()` / `max()` — extrema (where T: Comparable)
- `min_by(int(T, T) f)` / `max_by(int(T, T) f)` — custom comparator
- `min_by_key[K: Comparable](K(T) f)` / `max_by_key[K: Comparable](K(T) f)` — key-function extrema

**Search (eager, may short-circuit):**
- `find(bool(T) f)` — first matching element
- `find_index(bool(T) f)` — index of first match
- `contains(T val)` — membership (where T: Equatable)

**Collection (eager, materialize) — all go through one inferred `collect()`:**
- `collect()` — target type inferred from the binding / turbofish:
    - `Vector[int] v = it.collect()` — to Vector
    - `Set[int] s = it.collect()` — to Set (where T: Hashable)
    - `Dict[K, V] d = it_of_pairs.collect()` — to Dict (pair iterators)
    - `String s = it.collect()` — to String (where T: Displayable)
- `join(String sep)` — to String (where T: Displayable). This is the ONE
  place `join` lives; no `String.join(vec)` duplicate.
- `partition(bool(T) f)` — split into `(Vector[T], Vector[T])`
- `group_by[K: Hashable](K(T) f)` — to `Dict[K, Vector[T]]`
- `for_each(void(T) f)` — side-effecting iteration

### 4.5 Trait Contract (Algebraic Laws)

Implementations and adapter chains must preserve:

- `iter().count() == self.len()` (for sized Iterables)
- After `v.sort()`: `v.iter().is_sorted()` and `v.len()` unchanged
- `iter().filter(f).all(f) == true`
- `iter().chain(other).count() == self.len() + other.len()`
- `iter().rev().rev()` ≡ `iter()` for DoubleEndedIterator
- `iter().map(f).collect()` preserves order for ordered containers

These are property-test targets, not hand-checked invariants.

### 4.6 Implementation Phasing

See §10 for the full plan. Summary:

1. **Phase 2a (prerequisite):** Grammar + type-system work for sigils in
   type-argument positions (`Iterator[T &]`).
2. **Phase 2b:** Compiler support for concrete iterator return types
   (monomorphized adapter chains).
3. **Phase 2c:** Define `Iterator`/`Iterable`/`IntoIterable` traits and all
   equip methods listed above — **lazy from day one**.
4. **Phase 2d:** Vector/Dict/Set convenience wrappers (`v.map(f)` ≡
   `v.iter().map(f).collect()`).
5. **Phase 2e:** Advanced adapters (scan, intersperse, cycle, peekable) as
   demand arises.

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

**Trait bounds on collections (explicit):**

- `Dict[K, V]` requires `K: Hashable + Equatable`
- `Set[T]` requires `T: Hashable + Equatable`
- `Vector[T]`, `Deque[T]` — no bounds on T

Bounds state at the collection type, not on each method.

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
`removeprefix`, `removesuffix`, `hash`,
`chars`, `bytes`, `codepoints`, `split`,
`is_alpha`, `is_digit`, `is_alphanumeric`, `is_whitespace`,
`is_upper`, `is_lower`, `is_hex_digit`, `is_ascii`,
`clear`, `capacity`.

**Note on `join`:** dropped from String. `"-".join(vec)` is expressible as
`vec.iter().join("-")` which subsumes the transform-then-join, filter-then-join,
and non-string-element cases uniformly. One `join`, lives on Iterator.

**Removed:**

| Method | Replacement |
|--------|------------|
| `strip()` | `trim()` — was a synonym |
| `lstrip()` | `trim_left()` — renamed for clarity |
| `rstrip()` | `trim_right()` — renamed for clarity |
| `char_at()` | `byte_at()` — was deprecated |

**String implements:** `Iterable[String]` (iterate codepoints), `IntoIterable[String]`, `Writer` (append bytes to buffer), `Displayable` (identity), `Debuggable` (quoted + escaped), `Equatable`, `Comparable`, `Hashable`, `Cloneable`, `Default`.

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
| `sort()` | `() -> void` | In-place sort (T: Comparable) |
| `sort_by(cmp)` | `(int(T, T)) -> void` | In-place sort with comparator |
| `sort_by_key(f)` | `[K: Comparable](K(T) f) -> void` | Sort by key function (90% case) |
| `reverse()` | `() -> void` | In-place reverse |
| `capacity()` | `() -> int` | Current allocation size |

**Non-mutating (type-specific):**

| Method | Signature | Notes |
|--------|-----------|-------|
| `len()` | `() -> int` | Element count |
| `is_empty()` | `() -> bool` | len() == 0 |
| `contains(val)` | `(T) -> bool` | Linear search |
| `index_of(val)` | `(T) -> Option[int]` | First occurrence |
| `binary_search(val)` | `(T) -> int` | Requires sorted |
| `sorted()` | `() -> Vector[T]` | New sorted copy (T: Comparable) |
| `sorted_by(cmp)` | `(int(T, T)) -> Vector[T]` | New sorted copy with comparator |
| `sorted_by_key(f)` | `[K: Comparable](K(T) f) -> Vector[T]` | New sorted copy by key |
| `reversed()` | `() -> Vector[T]` | New reversed copy |
| `unique()` | `() -> Vector[T]` | New deduplicated copy |
| `slice(start, end)` | `(int, int) -> Vector[T]` | New sub-vector |
| `windows(n)` | `(int) -> Iterator[Vector[T] &]` | Sliding slices of size n |
| `chunks(n)` | `(int) -> Iterator[Vector[T] &]` | Non-overlapping slices of size n |

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

**Vector implements:** `Iterable[T]`, `IntoIterable[T]`, `Equatable` (where `T: Equatable`), `Cloneable` (where `T: Cloneable`), `Default`, `Displayable` (where `T: Displayable`), `Debuggable` (where `T: Debuggable`), `Measurable`.

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

**Dict implements:** `Iterable[(K, V)]`, `IntoIterable[(K, V)]`, `Equatable` (where `K, V: Equatable`), `Cloneable` (where `K, V: Cloneable`), `Default`, `Debuggable` (where `K, V: Debuggable`), `Measurable`. Requires `K: Hashable + Equatable`.

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

**Set implements:** `Iterable[T]`, `IntoIterable[T]`, `Equatable`, `Cloneable` (where `T: Cloneable`), `Default`, `Debuggable` (where `T: Debuggable`), `Measurable`. Requires `T: Hashable + Equatable`.

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

## 6. Writer / Reader Traits — Byte-Shaped I/O

**Design note:** I/O primitives are byte-shaped, not text-shaped. Binary files,
TLS sockets, compression streams, and raw network protocols are not UTF-8. Every
serious stdlib (Rust `io::Read/Write`, Go `io.Reader/Writer`, Java
`InputStream/OutputStream`) operates on bytes. Text is a convenience layer on
top.

### 6.1 Writer — The Narrow Waist for Output

```gorget
trait Writer:
    Result[int, IoError] write(&self, Bytes buf)
    # Returns bytes actually written. May be less than buf.len() for
    # sockets, pipes, or any short-write destination. Use write_all for
    # "must complete or fail" semantics.
```

The input is `Bytes` (= `Vector[uint8]`, see §6.3), **not** `String` / `Str`.
Writer is the narrow waist for *any* byte stream — binary files, TLS,
compression, encrypted protocols — none of which are UTF-8. Callers with
a `String` source convert via `.bytes()` at the boundary; callers with
raw bytes pass them directly with no pretence of text.

**Derived convenience (once per Writer):**

```gorget
equip Writer:
    # Guarantee all bytes written, or fail.
    Result[int, IoError] write_all(&self, Bytes buf):
        int total = 0
        int len = buf.len()
        while total < len:
            # slice() allocates a fresh Vector each call — acceptable for
            # the short-write path, which is rare for buffered Writers.
            # A future zero-cost byte-view type would make this O(1).
            match self.write(buf.slice(total, len)):
                case Ok(0): return Error(IoError.BrokenPipe())
                case Ok(n): total = total + n
                case Error(!e): return Error(!e)
        return Ok(total)

    # Text convenience — route a String through .bytes() then write_all.
    Result[int, IoError] write_str(&self, String s):
        self.write_all(s.bytes())

    # Formatted write — delegates to Displayable.
    Result[int, IoError] write_display(&self, Displayable v):
        self.write_all(v.display().bytes())
```

Implementors:

| Type | Behavior |
|------|----------|
| String | Append bytes to buffer (UTF-8 validated on `write_str`) |
| File | Write to file descriptor |
| Socket / TlsSocket | Write to network (short-writes possible) |
| stdout / stderr | Write to standard streams |

### 6.2 Reader — The Narrow Waist for Input

```gorget
trait Reader:
    Result[int, IoError] read(&self, Bytes &buf)
    # Fills buf up to its capacity; returns bytes read.
    # 0 means EOF. buf is a mutable borrow — Reader writes into
    # the caller's buffer.
```

**Derived convenience:**

```gorget
equip Reader:
    # Read until EOF into a new Vector.
    Result[Bytes, IoError] read_all(&self):
        Bytes out = Bytes(cap: 4096)
        Bytes chunk = Bytes(cap: 4096)
        loop:
            match self.read(&chunk):
                case Ok(0): return Ok(!out)
                case Ok(_): out.extend(!chunk); chunk = Bytes(cap: 4096)
                case Error(!e): return Error(!e)

    # Read until EOF, validate UTF-8, return String.
    Result[String, IoError] read_all_str(&self):
        Bytes bytes = self.read_all()?
        String.from_utf8(bytes)   # returns Result[String, IoError.Utf8Invalid]

    # Read exactly n bytes or fail.
    Result[Bytes, IoError] read_exact(&self, int n):
        Bytes out = Bytes(cap: n)
        while out.len() < n:
            match self.read(&out):
                case Ok(0): return Error(IoError.UnexpectedEof())
                case Ok(_): pass
                case Error(!e): return Error(!e)
        return Ok(!out)
```

Implementors: `File`, `Socket`, `TlsSocket`, `stdin`, `BufReader`.

### 6.3 Bytes: A Thin Vector[uint8] Alias

For API clarity, `Bytes` is the canonical owned byte-buffer type,
aliasing `Vector[uint8]`:

```gorget
type Bytes = Vector[uint8]
```

Exported from `std.io` — the Writer/Reader signatures above use it, so
anyone already importing `Writer` / `Reader` picks up `Bytes` on the
same line.

Functions that traffic in raw bytes take `Bytes` / `Str` (view over bytes);
text-oriented code takes `String` / `Str` (UTF-8-validated).

### 6.4 Displayable vs Writer

Orthogonal:

- **Displayable** = "what" — `String display(self)`
- **Writer** = "where" — `Result[int, IoError] write(&self, Bytes buf)`

`print(x)` is: `stdout.write_display(x); stdout.write_str("\n")`.

## 7. The Side Enum

Used by String.trim and potentially other methods:

```gorget
enum Side:
    Left
    Right
    Both
```

Defined in the prelude. Available without import.

## 8. Concurrency Model

Gorget is not Rust. `&` is **not** exclusive access — multiple `&` borrows of
the same variable can coexist within a single thread. The language trusts the
programmer to avoid overlapping mutations where the code stays single-threaded.

The place where races actually happen is at thread boundaries. That's where
Gorget draws its safety line.

### 8.1 The Ownership Sigils

| Sigil | Meaning | Aliasing | Cost | Cross-thread? |
|-------|---------|----------|------|---------------|
| `T` (bare) | owned value | N/A | None | by move only |
| `Ptr(T)` | shared immutable borrow | OK | None | no |
| `T &` | local mutable borrow | OK (single-threaded) | None | **no** |
| `T !` | move / consume | source dies | None | yes (by move) |
| `shared T` | thread-safe aliased mutable access | OK across threads | atomic / lock | yes |

### 8.2 Spawn Boundaries Require `shared`

A `&` borrow cannot escape a `spawn`. If you want mutable aliased state
across tasks, it must be `shared`:

```gorget
# OK — shared is designed for cross-thread mutable access:
shared int counter = 0
Task[void] t = spawn increment(counter)   # counter auto-threads through

# REJECTED — plain `&` cannot cross a spawn boundary:
int local = 0
Task[void] t = spawn ((): local += 1)()   # compile error:
                                          # `&local` cannot cross spawn boundary;
                                          # capture via `shared` or use
                                          # `spawn unchecked`.
```

This closes the principal data-race surface without charging a Rust-style
borrow-checker tax on single-threaded code. `&` stays ergonomic locally;
`shared` is mandatory for the case where races happen.

### 8.3 Escape Hatch: `spawn unchecked`

Sometimes the programmer has already synchronized access manually — via a
hand-rolled mutex, an external lock, a lockfree scheme, a single-reader /
single-writer invariant, or by pinning the task to a thread. For these
cases the boundary check is suppressed at the spawn site:

```gorget
# I have manually synchronized access to `x`. Trust me.
Task[void] t = spawn unchecked manually_synchronized_mutate(&x)

# Also valid with inline closure:
Task[void] t = spawn unchecked ((): manually_synchronized_mutate(&x))()
```

(Final surface syntax — `spawn unchecked fn()` prefix form, `spawn @unchecked`
attribute form, or something else — is a bikeshed. The semantic is: one
explicit local opt-out per spawn site.)

Rules:

- **`unchecked` is a per-spawn opt-out**, not a function-level or
  module-level setting. It is local, grep-able, and obvious in review.
- **No implicit propagation.** A function called from an `unchecked`
  spawn does not inherit the opt-out; if it internally spawns without
  `shared`, that inner spawn also needs its own `unchecked`.
- **Document why.** If you reach for `unchecked`, you have taken
  responsibility for correctness the compiler cannot verify. Leave a
  comment explaining which invariant saves you.

Design intent: `shared` is the answer 99% of the time; `unchecked` exists
so that specialized code (async runtimes, lockfree structures, pinned
workers, FFI callbacks) doesn't have to contort its types to satisfy a
check it has already manually discharged.

### 8.4 Why Not Go Full Rust-Exclusive?

Worth stating explicitly: making `&` Rust-style exclusive **was considered
and rejected**. The gain (compile-time data-race freedom, signature-level
mutation contracts) is real, but the cost is Rust-tier ergonomics — every
existing closure capture, self-referential struct, observer pattern, and
"mutate a vec while iterating its view" idiom would need auditing, plus a
`Cell` / `RefCell` escape-hatch gallery. Gorget advertises Python-shaped
syntax; Rust-exclusive semantics would break that contract.

The spawn-boundary model captures ~95% of the practical safety benefit
(data races are ~entirely at thread boundaries) at ~5% of the ergonomic
cost. It's the right tradeoff for Gorget's target audience.

## 9. Error Design

### 9.1 Typed IoError — Not `Result[T, String]`

String errors are for humans, not programs. Using `Result[T, String]` in the
stdlib forecloses:

- Pattern matching on category (`NotFound` vs `PermissionDenied` vs
  `UnexpectedEof`)
- Error chaining (cause / source)
- Programmatic recovery (retry on `Interrupted`, escalate on `OutOfMemory`)

Stdlib I/O uses a typed `IoError` enum:

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
    Utf8Invalid(int byte_offset)
    Other(String message)    # escape hatch — prefer a named variant
```

Defined in `std.io`. Every producer or consumer of `IoError` also imports
from `std.io` (Writer/Reader traits, `write_all` helpers, I/O functions),
so the type is naturally in scope on the same `from std.io import …`
line — no prelude bloat needed. (Earlier revisions of this doc proposed
prelude placement; we walked that back when it became clear callers
would always co-import other std.io items.)

### 9.2 The `Error` Trait

For domain-specific errors (JSON parse, SQL, HTTP, etc.), a narrow trait
enables uniform handling:

```gorget
trait Error: Displayable + Debuggable:
    Option[Box[Error]] source(&self)   # optional cause chain
```

Domain enums implement `Error`; `?`-propagation uses `From[Source, Target]`
conversions where applicable.

### 9.3 Once you ship strings, you're stuck

`Result[T, String]` in the stdlib is a one-way door. Downstream code
`.map_err(|e| e.to_string())` to paper over mismatches, and retrofitting
typed errors breaks every signature that touched the stdlib. Decide now.

## 10. Implementation Plan

Phases are re-ordered around **dependencies**, not chronological preference.
Type-system prerequisites gate the Iterator work; Iterator gates Writer/Reader
convenience layers.

### Phase 1: Foundation — **DONE 2026-04-16**

Constructors, String builder, method consolidation.

1. Add `cap` parameter to String/Vector/Dict/Set constructors ✓
2. Wire up String mutation methods (`push`, `push_char`, `clear`, `capacity`)
   in type checker and GIR lowering — runtime already has them ✓
3. Add `find(pattern, from, reverse)` as String primitive ✓
4. Consolidate `trim`/`lstrip`/`rstrip`/`strip` into `trim(side, chars)`
   with `trim_left`/`trim_right` aliases ✓
5. Add `limit` parameter to `split` and `replace` ✓
6. Add `lines()` method on String ✓
7. Remove Dict synonyms (`update`, `set`, `has`) ✓ (kept as compat aliases;
   not in recommended API)
8. Change Dict.remove to return `Option[V]` ✓ (backing runtime fn
   `gorget_map_remove_opt` uses a thread-local buffer)
9. Add `sort(by)` / `sorted(by)` optional comparator parameter ✓ (arg-count
   routes to `sort_by`/`sorted_by`; TLS closure pointer + qsort)
10. Add Set.symmetric_difference and Set.is_disjoint ✓

### Phase 1.5: Review-driven additions (pre-Phase 2 quick wins)

Folded in before Phase 2 starts — cheap items that don't depend on type-system
work:

1. Add `sort_by_key(f)` / `sorted_by_key(f)` — wraps existing sort_by
   infrastructure; closure returns `K: Comparable` instead of `int`.
2. Add `Debuggable` trait + `@derive(Debug)` — @derive machinery already
   handles 10 struct + 6 enum traits; Debug is a mechanical addition.
3. Drop `String.join(vec)` from the recommended surface — `vec.iter().join(sep)`
   subsumes it. Keep a back-compat shim that emits a deprecation warning.
4. Add explicit trait bounds on collection type parameters:
   `Dict[K: Hashable + Equatable, V]`, `Set[T: Hashable + Equatable]`.
5. Vector.capacity() — currently exists only on String.
6. Minor: `windows(n)` / `chunks(n)` on Vector (eager Vector[Vector[T]]
   version; lazy Iterator version lands in Phase 2).

### Phase 2: Iterator, Iterable, IntoIterable (depends on type-system work)

This phase has **real type-system prerequisites** — see §4.2, §4.3, and §3.

#### Phase 2a: Type-system prerequisites

1. **Grammar extension** — accept ownership sigils (`&`, `!`) at type-argument
   positions, e.g., `Iterator[String &]`, `Option[T !]`, `Vector[Foo !]`.
   Parser work; additive in the sense that previously-invalid syntax becomes
   valid, no existing parses change meaning.
2. **Type checker** — treat `T`, `T &`, `T !` as distinct-but-related generic
   arguments; propagate the sigil through closure signatures in HOFs; implement
   Copy-type auto-deref (§4.3).
3. **Borrow checker** — the existing provenance tracking covers references-in-
   generic-position once the parser accepts them; mostly wiring, no new logic.
4. **Monomorphization** — distinct instantiations per sigil (no coalescing
   even when runtime-identical on Copy types); emits same code where
   appropriate.
5. **Associated types on traits** — `type Iter: Iterator[T &]` inside
   `Iterable[T]`. If Gorget doesn't yet have associated types, this adds
   them; if it does, just use them here.

#### Phase 2b: Concrete iterator returns

1. Compiler support for concrete iterator state-machine structs (one per
   adapter: `Filter`, `Map`, `Take`, `Chain`, …).
2. Monomorphized adapter chains — `vec.iter().filter(f).map(g)` produces
   a single fused loop after inlining.
3. Optional ergonomic sugar: `impl Iterator[T]`-style abbreviation in
   return-type position (can defer; library authors can write the explicit
   types until then).

#### Phase 2c: Trait definitions + equip methods (LAZY FROM DAY ONE)

1. Define `Iterator[T]`, `Iterable[T]`, `IntoIterable[T]` per §3.
2. Implement all equip methods from §4.4 on `Iterator[T]` — lazy from the
   start, no eager Vector-intermediate implementation. (An eager interim
   trains users onto the old Vector API permanently.)
3. Vector/Dict/Set convenience wrappers (`v.map(f)` ≡
   `v.iter().map(f).collect()`). Cheap shells over the Iterator methods.
4. Add `swap_remove`, `retain`, `fill`, `swap` on Vector.
5. Single inferred `collect()` — infers target from binding/turbofish.
   Drop `to_set()` / `to_dict()` from the surface; they all go through `collect()`.

#### Phase 2d: Advanced adapters

`scan`, `intersperse`, `cycle`, `peekable`, `DoubleEndedIterator` — driven by
demand, not shipped speculatively.

### Phase 3: Writer/Reader (depends on Phase 2 for `Iterator[byte]`)

1. Define the `Writer` trait (byte-shaped, `Result[int, IoError]`).
2. Define the `Reader` trait (`Result[int, IoError] read(&self, Vector[byte] &buf)`).
3. Define `IoError` enum in `std.io` (see §9.1 — callers co-import with Writer/Reader).
4. Define the `Error` trait (see §9.2).
5. Implement Writer on String (byte append), File, Socket, TlsSocket,
   stdout, stderr, Bytes.
6. Implement Reader on File, Socket, TlsSocket, stdin, BufReader.
7. Derived methods: `write_all`, `write_str`, `write_display`, `read_all`,
   `read_all_str`, `read_exact` — equip on trait.
8. Refactor `print()` to use `Displayable` + `stdout.write_str`.
9. Migrate existing `Result[T, String]` stdlib signatures to
   `Result[T, IoError]` where they cross the I/O boundary — breaking,
   but contained to the I/O surface.

### Phase 4: Concurrency-model enforcement

1. Type-checker pass: reject `&` captures crossing `spawn` boundaries;
   require `shared T` or `spawn unchecked`.
2. Parser/syntax for `spawn unchecked` per §8.3.
3. Audit existing fixtures and `xtd` libraries for patterns that silently
   relied on `&` escaping spawns; migrate to `shared` or `unchecked` as
   appropriate.
4. Hashable migration — switch trait from `int hash(self)` to
   `void hash(self, Hasher &h)`. Update the `@derive(Hashable)` generator;
   Dict/Set internals reimplemented against `Hasher`.

### Phase 5: Documentation

1. Update `docs/language-design.md` §4.4.1 — add Writer/Reader/Hasher/Debuggable to -er/-able examples.
2. Update `docs/language-reference.md` §15.2 — all method signature changes.
3. Update `docs/book/05-collections.md` — new methods, consolidated API, Iterator examples.
4. Update `docs/book/appendix-traits.md` — add Writer, Reader, Debuggable, Error, Hasher; note naming convention.
5. Update `docs/book/19-stdlib.md` — reflect new std/xtd layering and concurrency model.
6. New: `docs/book/XX-concurrency.md` — the `&`/`shared`/`unchecked` story with worked examples.

## 11. Migration / Backward Compatibility

| Change | Phase | Impact | Migration |
|--------|-------|--------|-----------|
| `strip()` removed | 1 ✓ | Low — synonym for `trim()` | Find-replace |
| `lstrip()`/`rstrip()` renamed | 1 ✓ | Low | `lstrip()` → `trim_left()`, `rstrip()` → `trim_right()` |
| `has()` removed from Dict | 1 ✓ | Low — synonym for `contains()` | Find-replace |
| `update()`/`set()` removed from Dict | 1 ✓ | Medium | `update(k,v)` → `put(k,v)`, `set(k,v)` → `put(k,v)` |
| `char_at()` removed | 1 ✓ | Low — was deprecated | Use `byte_at()` or `substring()` |
| `Dict.remove` returns `Option[V]` | 1 ✓ | Medium | Code expecting `bool` needs update |
| `find()` on String restored | 1 ✓ | None | New unified search with parameters |
| `String.join(vec)` deprecated | 1.5 | Low | `vec.iter().join(sep)` |
| `Hashable: int hash(self)` → `void hash(Hasher &h)` | 4 | **High** | `@derive(Hashable)` works unchanged; hand-written impls need rewrite |
| `Writer.write` returns `Result[int, IoError]` | 3 | Medium | `Writer` was not yet shipped; only internal implementors affected |
| `Reader.read` is byte-shaped | 3 | Medium | Same — `Reader` was not yet shipped |
| `Result[T, String]` → `Result[T, IoError]` on I/O paths | 3 | **High** | Breaking at the I/O boundary; downstream `?`-propagation needs `From` impls or explicit conversion |
| `&` captures banned across `spawn` | 4 | Medium–High | Existing code using `&` across `spawn` must switch to `shared` or `spawn unchecked` |
| `to_set()` / `to_dict()` removed | 2 | Low | `iter.collect()` infers target from binding type |

All removals can have a deprecation period where the old name emits a
compiler warning pointing to the replacement. The four **High**-impact
changes (Hashable signature, Result[_, String] → IoError, spawn boundary
enforcement) are the moments that hurt most — schedule them with code-mod
tooling and a migration guide.
