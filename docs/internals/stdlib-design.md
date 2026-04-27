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
| **Capability** — a property of a type that has other purposes | `-able` / `-ible` | "this type **is** X" | Equatable, Comparable, Hashable, Displayable, **Debuggable**, Cloneable, Iterable, Drainable, Serializable |
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
    void write_int(&self, int v)
    void write_bytes(&self, Vector[byte] bytes)
    void write_string(&self, String s)
    int finish(self)
    # Hash state. Hashable types forward field bytes into a Hasher; the
    # concrete algorithm (SipHash, FxHash, ...) is chosen by the consumer.
    # Typed write methods (write_int / write_string) let primitives feed
    # state without a Vector[byte] round-trip.
```

### Capability Traits (-able / -ible)

```gorget
trait Iterable[T]:
    # Non-consuming iteration. `for x in v` desugars to name-based
    # dispatch on `iter()`; the trait provides the contract and
    # enables `[Iterable T]` generic bounds. The returned iterator
    # type is concrete (monomorphized per Self), not a trait object
    # — see §4.1. Real declaration ships in `lib/std/iter.gg`;
    # equipped by Vector / Set / Dict.
    Iterator[T] iter(&self)

trait Drainable[T]:
    # Consuming iteration. Source is moved into the iterator and the
    # caller can no longer use it after `drain()` returns. "Drain" is
    # canonical collection-API vocabulary for "iterate by emptying."
    # Sibling to Iterable (not extends): some types support drain
    # but not borrow-iterate (one-shot streams), some support both
    # (collections), implementors opt in independently.
    #
    # **Status (2026-04-27):** trait declared in `lib/std/iter.gg`,
    # `Vector[T]` equipped via O(n) reverse + pop. `Set[T]` /
    # `Dict[K, V]` drain not yet shipped — they need a
    # `gorget_map_drain_entry` runtime helper or tombstone-walk
    # machinery. Today users wanting drain on those collections
    # call `.iter().clone_each()` or build their own drain iterator
    # over the underlying GorgetMap bucket array.
    type DrainIter: Iterator[T !]
    Self::DrainIter drain(!self)

trait Equatable:
    bool eq(self, Self other)

trait Comparable:
    int compare(self, Self other)

trait Hashable:
    # State-based hashing: composes — struct impls forward field hashes
    # into the same Hasher without re-inventing combine logic.
    # Generic over the Hasher implementation; the consumer picks the
    # algorithm (FxHash for in-process, SipHash for DoS-resistance,
    # …). `std.hash` ships `FxHasher` as the default.
    void hash[Hasher H](self, H &h)

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

### Relationship: Iterable vs Drainable vs Iterator

- **Iterable** — can produce many iterators; source survives. `v.iter()`
  yields `T &` (mut borrow). This is what `for x in v` desugars to.
- **Drainable** — single consuming iteration; source is moved. `v.drain()`
  yields `T !` (owned). Use when you want to transfer elements into a new
  collection without cloning.
- **Iterator** — cursor state, generic over what it yields. A single
  `Iterator[T]` trait backs both iteration modes; the ownership story lives
  in `T`'s sigil, not in a separate trait.

### Concrete Return, Not Trait Object

`iter()` and `drain()` return a **concrete type** (specific to the collection
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
Any type that implements `Iterable` or `Drainable` gets them all for free.

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
| `Iterator[T !]` | owned move | consumed | `Drainable.drain()` |

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
for name in names.drain():          # yields String ! (move)
    unique.add(!name)               # transfer ownership, no clone
# `names` is moved (drain consumed it); using it again is a compile error

# Functional chain — whole pipeline monomorphizes:
Vector[int] lens = names.iter().map((s &): s.len()).collect()
Set[String] set = names.drain().collect()
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
- `join(String sep)` — to String (where T: Displayable). Iterator-
  chain primitive. `String.join(vec)` stays as a Python-shaped
  script shortcut — see §5 String → "Note on `join`" for rationale.
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
3. **Phase 2c:** Define `Iterator`/`Iterable`/`Drainable` traits and all
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

**Note on `join`:** both forms coexist. `"-".join(vec)` is the Python-
shaped script ergonomic (stays); `join_iter[Iter, Displayable T]
(iter, "-")` in `std.iter` is the narrow-waist primitive for
iterator chains with transform-then-join, filter-then-join, or
non-string element types (routes through `Displayable.display()`).
Same rationale as `std.fs.read_file` / `write_file` vs the
Writer/Reader primitives: keep the ergonomic shortcut for the
common case; the narrow waist sits underneath.

**Removed:**

| Method | Replacement |
|--------|------------|
| `strip()` | `trim()` — was a synonym |
| `lstrip()` | `trim_left()` — renamed for clarity |
| `rstrip()` | `trim_right()` — renamed for clarity |
| `char_at()` | `byte_at()` — was deprecated |

**String implements:** `Iterable[String]` (iterate codepoints), `Drainable[String]`, `Writer` (append bytes to buffer), `Displayable` (identity), `Debuggable` (quoted + escaped), `Equatable`, `Comparable`, `Hashable`, `Cloneable`, `Default`.

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

**Vector implements:** `Iterable[T]`, `Drainable[T]`, `Equatable` (where `T: Equatable`), `Cloneable` (where `T: Cloneable`), `Default`, `Displayable` (where `T: Displayable`), `Debuggable` (where `T: Debuggable`), `Measurable`.

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

**Dict implements:** `Iterable[(K, V)]`, `Drainable[(K, V)]`, `Equatable` (where `K, V: Equatable`), `Cloneable` (where `K, V: Cloneable`), `Default`, `Debuggable` (where `K, V: Debuggable`), `Measurable`. Requires `K: Hashable + Equatable`.

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

**Set implements:** `Iterable[T]`, `Drainable[T]`, `Equatable`, `Cloneable` (where `T: Cloneable`), `Default`, `Debuggable` (where `T: Debuggable`), `Measurable`. Requires `T: Hashable + Equatable`.

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
    Result[int, IoError] write(&self, Vector[byte] buf)
    # Returns bytes actually written. May be less than buf.len() for
    # sockets, pipes, or any short-write destination. Use write_all for
    # "must complete or fail" semantics.
```

The input is `Vector[byte]` — a raw byte buffer — **not** `String`/`Str`.
`byte` is a lexer-level alias for `uint8` (same type, no conversion
cost, works across modules without alias-resolution hiccups).
Writer is the narrow waist for *any* byte stream — binary files, TLS,
compression, encrypted protocols — none of which are UTF-8. Callers with
a `String` source convert via `.bytes()` at the boundary; callers with
raw bytes pass them directly with no pretence of text.

**Derived convenience (once per Writer):**

```gorget
equip Writer:
    # Guarantee all bytes written, or fail.
    Result[int, IoError] write_all(&self, Vector[byte] buf):
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
    Result[int, IoError] read(&self, Vector[byte] &buf)
    # Fills buf up to its capacity; returns bytes read.
    # 0 means EOF. buf is a mutable borrow — Reader writes into
    # the caller's buffer.
```

**Derived convenience:**

```gorget
equip Reader:
    # Read until EOF into a new Vector.
    Result[Vector[byte], IoError] read_all(&self):
        Vector[byte] out = Vector[byte](cap: 4096)
        Vector[byte] chunk = Vector[byte](cap: 4096)
        loop:
            match self.read(&chunk):
                case Ok(0): return Ok(!out)
                case Ok(_): out.extend(!chunk); chunk = Vector[byte](cap: 4096)
                case Error(!e): return Error(!e)

    # Read until EOF, validate UTF-8, return String.
    Result[String, IoError] read_all_str(&self):
        Vector[byte] bytes = self.read_all()?
        String.from_utf8(bytes)   # returns Result[String, IoError.Utf8Invalid]

    # Read exactly n bytes or fail.
    Result[Vector[byte], IoError] read_exact(&self, int n):
        Vector[byte] out = Vector[byte](cap: n)
        while out.len() < n:
            match self.read(&out):
                case Ok(0): return Error(IoError.UnexpectedEof())
                case Ok(_): pass
                case Error(!e): return Error(!e)
        return Ok(!out)
```

Implementors: `File`, `Socket`, `TlsSocket`, `stdin`, `BufReader`.

### 6.3 The `byte` / `uint8` Type Alias

`byte` is a lexer-level alias for `uint8` — the same type at the AST
level, zero conversion cost, works identically across module
boundaries. `Vector[byte]` is the canonical name for "a byte buffer,
not a string"; it reads honestly and costs nothing over the raw
`Vector[uint8]` form.

No nominal `Bytes` wrapper is introduced. Rust's std doesn't have one
(`&[u8]` is the interface); Go uses `[]byte`. A dedicated newtype
would force wrap/unwrap ceremony at every boundary without enabling
anything the free functions in `std.bytes` (hex, base64, endian
helpers) don't already do over `Vector[byte]`.

Functions that traffic in raw bytes take `Vector[byte]`;
text-oriented code takes `String` / `Str` (UTF-8-intended).

### 6.4 Displayable vs Writer

Orthogonal:

- **Displayable** = "what" — `String display(self)`
- **Writer** = "where" — `Result[int, IoError] write(&self, Vector[byte] buf)`

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
3. ~~Drop `String.join(vec)` from the recommended surface~~ — **revised
   2026-04-19**: keep it. `join_iter[Iter, Displayable T](iter, sep)`
   ships in `std.iter` for iterator-chain callers, but `",".join(v)`
   stays as a Python-style script ergonomic. Rationale parallels
   `std.fs.read_file` / `write_file`: a convenient shortcut for the
   common case is worth the surface-area cost when the narrow-waist
   primitive exists alongside. Deprecating would punish every
   script-style caller (20+ fixtures use `sep.join(v)`) with no
   correctness gain.
4. Add explicit trait bounds on collection type parameters:
   `Dict[K: Hashable + Equatable, V]`, `Set[T: Hashable + Equatable]`.
5. Vector.capacity() — currently exists only on String.
6. Minor: `windows(n)` / `chunks(n)` on Vector (eager Vector[Vector[T]]
   version; lazy Iterator version lands in Phase 2).

### Phase 2: Iterator, Iterable, Drainable (depends on type-system work)

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

**Status as of 2026-04-25:** Substantially shipped. Trait declared,
all eager terminals on Iterator as default-method bodies, all 9
adapter structs generic over a source `Iter` parameter, all four
"missing lazy adapters" shipped. Self / trait-T substitution in
default-method sigs + adapter-constructor defaults both landed
2026-04-21; chain-past-one-step fixture green. **Dict / Set
lazy iterators landed 2026-04-25** with two compiler fixes
(borrow-field construction passes Ptr through; default-return-type
discovery binds trait-T) — Set finally has `.iter()`, Dict.iter()
yields `(K, V)` tuples lazily without materialising `.items()`.
Three compiler items remain — each blocked on a specific compiler
feature itemised below the checklist.

##### Shipped

1. ✅ Define `Iterator[T]` in `lib/std/iter.gg`. Builtin entry at
   `src/semantic/traits.rs:421-431` deleted; the placeholder DefId
   reserved at `resolve.rs:109` lets equip blocks parse before
   the user-space declaration loads.
2. ✅ Default-body terminals on `Iterator[T]` — `count` / `collect` /
   `last` / `nth` / `any[F]` / `all[F]` / `find[F]` /
   `find_index[F]` / `for_each[F]` / `fold[A, F]`. Method-level-
   generic defaults flow through per-call-site mono via a new
   `find_default_trait_method` helper in the generic collector.
3. ✅ All 9 adapter structs generic over source `Iter`:
   `TakeIter[Iter, T]`, `SkipIter[Iter, T]`,
   `ChainIter[IterA, IterB, T]`, `MapIter[Iter, T, U, F]`,
   `FilterIter[Iter, T, F]`, `TakeWhileIter[Iter, T, F]`,
   `DropWhileIter[Iter, T, F]`, `FilterMapIter[Iter, T, U, F]`,
   `InspectIter[Iter, T, F]`.
4. ✅ Lazy adapters: `EnumerateIter[Iter, T]`,
   `ZipIter[IterA, IterB, A, B]`, `WindowsIter[Iter, T]`,
   `ChunksIter[Iter, T]`. `Option[(int, T)]` tuple lowering verified
   end-to-end before commit.
5. ✅ Compiler infrastructure surfaced and fixed along the way:
   `Self → equipping_type` substitution for default trait methods,
   default-method per-call-site mono, body pre-substitution in
   `lower_method_instance` (handles nested generic type-arg lists
   like `MapIter[VectorIter[T], T, U, F]` that the post-mangling
   string substitution couldn't), `error_id`-aware
   `validate_trait_impls` for user-space generic trait sigs,
   default-method fallback in `TraitRegistry::resolve_method`.
6. ✅ **Self / trait-T substitution in default-method sigs at call
   site** — 2026-04-21 (commit `2f9a5d01`). `TraitInfo` gained
   `trait_generic_params` + `default_method_sigs` (AST-level return
   / param types). `EquipInfo` gained `self_type_ast` +
   `impl_generic_params`. On a trait-default hit in
   `resolve_method` / `resolve_method_by_name`, typecheck walks the
   impl's self_type AST against the receiver's concrete type to
   bind impl locals, substitutes trait generic args, binds `Self →
   receiver`, and rebuilds an owned `FunctionSig`. Name-based
   default resolution now runs before the hardcoded
   `try_iterator_adapter_type` Vector-adapter shortcut so real
   defaults win for types that actually impl `Iterator[T]`.
   IR-side mirror landed in the same commit:
   `register_equip_sigs_with_defaults` /
   `register_method_instance_sigs` / `lower_method_instance` all
   bind `("Self", substituted_equipped)` in their subs.
7. ✅ **Adapter constructor defaults on `Iterator[T]`** — 2026-04-21
   (commit `86999a37`). `take(n)` / `skip(n)` / `map[U,F](f)` /
   `filter[F](p)` / `take_while[F]` / `drop_while[F]` /
   `filter_map[U,F]` / `inspect[F]` / `enumerate()` now return
   `TakeIter[Self, T]` / `MapIter[Self, T, U, F]` etc. Every
   iterator implementor inherits the adapter surface; chains past
   one step work (`v.iter().take(4).filter(is_even).map(double)
   .collect()`). Demand-driven plumbing keeps the mono cost
   linear:
   - `try_register_default_return_type` in the generic collector's
     MethodCall walk: substitute + `scan_type` the default's
     return type so only call-site-reachable adapter instances
     register. Doesn't cascade into the returned instance's own
     trait defaults — that's what the earlier eager-scan attempt
     did and it blew up with `TakeIter[TakeIter[..], int]`
     unbounded growth.
   - `try_register_method_instance` binds Self in its scan subs so
     method-generic defaults like `FilterIter[Self, T, F]
     filter[F](self, F p)` register
     `FilterIter[VectorIter[int], int, bool(int)]` rather than
     `FilterIter[SelfType, ...]`.
   - `lower_generic_equip_methods_with_defaults` demand-gates
     emission via `all_return_nominals_registered`: skip emitting
     a default whose substituted return type mentions a nominal
     that no call site registered. Prevents every Iterator
     implementor from speculatively emitting every adapter.
   - Rewrite pass (`rewrite_struct_calls`) falls back to
     `scopes.lookup(name)` when the resolution map has no entry,
     so trait default-method bodies (which the resolver doesn't
     walk) still convert `TakeIter[Self, T](self, n)` from
     `Expr::Call` to `Expr::StructLiteral`.
   - `infer_expr_ast_type` falls back to trait defaults when the
     equip block lacks the method — enables chain inference
     through the lifted defaults. Fixture:
     `iter_chain_past_one_step.gg`.
   `chain` / `zip` stay as VectorIter-specific methods because
   their `other` parameter is iterator-specific; making them
   iterator-generic (`chain[Other](self, Other other)`) needs a
   Shape-2 variant of method-generic inference that threads the
   other iterator's concrete type through the adapter struct's
   field. `lazy_windows` / `lazy_chunks` also stay on VectorIter
   (their bodies construct `Vector[T]` locals and keeping them
   there avoids repeated Vector[T] instantiation scans across
   every Iterator implementor).
8. ✅ **Lazy `Dict.iter()` / `Set.iter()`** — 2026-04-25 (commit
   `21af33cf`). `DictIter[K, V]` / `SetIter[T]` walk the
   `GorgetMap` bucket array in place via a `Ref[Dict[K, V]]` /
   `Ref[Set[T]]` borrow field — no `.items()` materialisation.
   Yielded `(K, V)` pairs / `T` values come out of the
   `gorget_map_iter_key` / `_value` accessors, which clone
   resource-typed K/V via the map's `key_clone` / `val_clone`
   hooks so callers may freely drop the element without
   disturbing the source. The runtime fns are declared as
   generic externs (`extern int __dict_iter_order_len[K, V]
   (Ref[Dict[K, V]] m) = "gorget_map_iter_order_len"`) — generic
   mono preserves the C symbol, so a single runtime function
   serves every Dict / Set instantiation. Two compiler fixes
   were needed for the new equipping types to compose with the
   `Iterator[T]` adapter chain (`take`, `skip`, `fold`, …):
   - **Borrow-field construction passes Ptr through** — struct-
     literal lowering (`lower_struct_literal` in
     `src/ir/lowering/exprs/mod.rs`) used to call
     `ensure_owned_at_boundary` on every field operand, which
     cloned `Ptr(T)` operands and stored the address of a
     stack-local clone. For `Ref[T]` / `MutRef[T]` borrow fields
     the field semantics is "alias the source", so the boundary
     now skips Ptr-typed fields. Without this, `Dict.iter()`
     would clone the dict inside the iter() body, store the
     address of a stack-local clone in `DictIter.source`, and
     return a dangling pointer (the `borrow_field_basic.gg` /
     `borrow_field_lazy_dict_iter.gg` fixtures had been passing
     by accident — stack-local addresses happened to remain
     valid for the iteration window).
   - **Trait-default discovery binds trait-T to equip's trait
     args** — `try_register_default_return_type` (the
     non-generic-method-default complement to
     `try_register_method_instance`) didn't bind the trait's
     own generic params. For `equip [K, V] DictIter[K, V] with
     Iterator[(K, V)]:`, the trait's `T` should bind to
     `(K, V)`, so `TakeIter[Self, T] take(self, int n)`
     substitutes to `TakeIter[DictIter[int,int], (int,int)]`.
     Without the binding `T` stayed unresolved, the
     registration scan missed the concrete `TakeIter` instance,
     and the body-emission demand-gate skipped `take` for the
     equipping type — manifesting as `undefined reference to
     DictIter__int64_t__int64_t__take` at link time. The fix
     (mirroring the binding logic already in
     `register_equip_sigs_with_defaults` and
     `lower_generic_equip_methods_with_defaults`) lets any
     `equip MyType[A, B] with SomeTrait[(A, B)]` impl with
     renamed trait params inherit the full adapter surface.
   Fixtures: `tests/fixtures/stdlib_iter_dict.gg` (lazy
   bucket-walk over `Dict[int, int]`), `stdlib_iter_set.gg`
   (lazy `SetIter[int]`), plus the existing `borrow_field_*`
   fixtures that exercise `Ref[T]` field construction without
   the dangling-stack-address bug. Both fixtures' explicit
   `TakeIter[VectorIter[…], …]` annotations migrated to
   `TakeIter[DictIter[…]]` / `TakeIter[SetIter[…]]`.
   `chain` test dropped from `stdlib_iter_set.gg` — `chain`
   stays VectorIter-specific (see item 7 above); coverage
   remains in `stdlib_vector_iter.gg`.

##### Deferred (each blocked on a specific compiler feature)

| Item | Blocked on | Plan doc |
|---|---|---|
| Dict-flavoured convenience wrappers — **Vector wrappers shipped 2026-04-21** (`v.each`, `v.for_each`, `v.any`, `v.all`, `v.find`, `v.find_index`, `v.fold`, `v.map`, `v.filter`). **Set wrappers also ship today** (`s.each` / `s.for_each` / `s.any` / `s.all` / `s.find` / `s.find_index` / `s.fold`) — same shape as Vector, delegating through `s.iter()` which now returns the lazy `SetIter[T]` (item 8). **Dict wrappers still deferred** — design hold, not a compiler limitation. The existing builtin `Dict.any(K, V)` / `.all(K, V)` / `.each(K, V)` / `.fold(A, K, V)` methods take key and value as TWO separate closure args; iterator wrappers would take a single `(K, V)` tuple arg. Picking either shape breaks the other set of callers. Users who want tuple semantics today write it explicitly (`d.iter().any(p)` / `d.iter().fold(0, f)`); the public-API decision can wait for a deliberate breaking-change pass. `v.count(p)` / `v.reduce(f)` skipped (different sig from Iterator counterpart). `to_set()` / `to_dict()` drop blocked on inferred `collect()` (row 2) | Dict tuple-vs-2-args API decision | `docs/internals/method-level-inference.md` |
| Comparable-bounded defaults (`min` / `max` / `sum` / `product` / `join` / `contains` as Iterator defaults) | per-method trait-bound declarations (e.g. `where T: Comparable`) + bulk-emission skip logic for impls that fail the bound; without this, default-method emission specialises Iterator[T] for self-host driver Ts that don't satisfy `<` / `+` / `.display()` etc. and emits broken codegen — verified by self_host_bootstrap regression on 2026-04-20 | _to be written_ |
| Single inferred `collect()` (drop `to_set()` / `to_dict()` from the surface) — **all three targets shipped 2026-04-22/23**. Vector (`Vector[T] v = it.collect()`): default-method sig registration in `register_trait_equip_sigs` + demand-gated bulk emission produce a concrete `X__collect` returning `Vector[T]` per iterator impl. Set (`Set[T] s = it.collect()`): Pass 2.6 AST rewrite (`apply_collect_target_rewrites` in `src/semantic/typecheck.rs`) swaps `.collect()` → `.to_set()` when the VarDecl's declared type is `Set[_]`, routing through a new `Iterator[T]::to_set(&self)` trait default. Dict (`Dict[K, V] d = pairs.collect()`): same rewrite splices K/V from the LHS into the method's generic args and swaps to `.to_dict[K, V]()`, routing through a new `Iterator[T]::to_dict[K, V](&self)` trait default that `.put(x.0, x.1)`s tuple elements from the iterator. Non-tuple `T`s fail at mono emission since the body reaches for `x.0`/`x.1` — users who try `Dict[K, V] = non_pairs.iter().collect()` get a compile error. Fixtures: `tests/fixtures/iter_collect_set.gg`, `iter_collect_dict.gg`. Still deferred (low priority): turbofish form `it.collect[Set[int]]()` would require `.collect()` itself to become method-generic; today explicit turbofish routes through `.to_set[T]()` / `.to_dict[K, V]()` directly. | — | — |
| Auto-import std.iter via the loader — **SHIPPED 2026-04-23** (commit a5b3ba7a). `v.iter().map(f).filter(p).collect()` in a scratch file compiles and runs without any `from std.iter import ...` boilerplate. The heuristic fires when the entry module references `Iterator`/`Iterable`/`Drainable`/adapter-struct names or calls `.iter()` anywhere, subject to shadowing (e.g. `vector_iter_userdef.gg` defines its own `VectorIter`) and existing-import checks. Turn-on required two prerequisites landed in the same commit: (1) trait-T binding in per-call-site mono (`try_register_method_instance` + `lower_method_instance` push trait-generic-name → substituted-trait-arg BEFORE Self + impl subs so trait-body refs win when names collide, e.g. `equip CounterIter with Iterator[int]:`'s inherited `fold[A, F]` specialises correctly); (2) non-generic-equip registration in `equip_templates` when the trait has defaults OR is an iterator-protocol name, so chain inference via `infer_expr_ast_type` can resolve `Counter(0, 5).iter()` → `CounterIter`. Fixture migrations: `iterator_adapters.gg` + `linked_list.gg` + `examples/iterator_demo.gg` + `examples/linked_list.gg` now call `.collect()` explicitly where they previously relied on the eager `try_lower_iterator_adapter` shortcut (that path stays as a fallback when the trait default isn't in scope). | — | — |

Vector's `swap_remove` / `retain` / `fill` / `swap` method-level
bindings and the `lazy_windows` / `lazy_chunks` rename to drop the
`lazy_` prefix both depend on Vector's eager `.windows(n)` /
`.chunks(n)` becoming thin shells — which itself blocks on the
inferred `collect()` row above (eager `.windows` wants
`iter().windows(n).collect()`). Sequencing: inferred collect →
Vector wrappers → rename.

**Bound-needing terminals still shipped as free functions in
`std.iter`** (kept as a working alternative for callers that can't
go through the trait-default path until per-method trait bounds
land):

- Aggregation: `sum_iter`, `product_iter`, `min_iter` (int),
  `max_iter` (int).
- Collection: `join_iter` (Displayable-based).

The unbound counterparts (`collect_vec`, `count_iter`, `fold_iter`,
`any_iter`, `all_iter`, `find_iter`, `find_index_iter`, `last_iter`,
`nth_iter`, `for_each_iter`) were retired in the Phase 2c
convenience-wrapper migration (2026-04-21). All of those terminals
live as default-method bodies on `Iterator[T]`; callers use the
method form (`v.iter().count()` / `v.count()` via the Vector
wrapper / `take_iter.collect()` etc.) and the four
`stdlib_iter_*.gg` fixtures were rewritten accordingly.

#### Phase 2d: Advanced adapters

`scan`, `intersperse`, `cycle`, `peekable`, `DoubleEndedIterator` — driven by
demand, not shipped speculatively.

### Phase 3: Writer/Reader (depends on Phase 2 for `Iterator[byte]`)

1. Define the `Writer` trait (byte-shaped, `Result[int, IoError]`).
2. Define the `Reader` trait (`Result[int, IoError] read(&self, Vector[byte] &buf)`).
3. Define `IoError` enum in `std.io` (see §9.1 — callers co-import with Writer/Reader).
4. Define the `Error` trait (see §9.2).
5. Implement Writer on String (byte append), File, Socket, TlsSocket,
   stdout, stderr, `Vector[byte]`.
6. Implement Reader on File, Socket, TlsSocket, stdin, BufReader.
7. Derived methods: `write_all`, `write_str`, `write_display`, `read_all`,
   `read_all_str`, `read_exact` — equip on trait.
8. Refactor `print()` to use `Displayable` + `stdout.write_str`.
9. Migrate existing `Result[T, String]` stdlib signatures to
   `Result[T, IoError]` where they cross the I/O boundary — breaking,
   but contained to the I/O surface.

**Phase 3 follow-ups (post-initial-ship):**

- ✅ **Rename `write_bytes` / `read_bytes` → `write` / `read`**
  (2026-04-19). All legacy name-collision shims retired: `File.write
  (String)` + `Socket.write(Vector)` / `Socket.read(int)` +
  `TlsSocket.write(Vector)` / `TlsSocket.read(int)` all removed. The
  four `xtd` callers (ssh.gg:243/245, http.gg:117/158) migrated to
  Writer/Reader trait methods. `Writer.write(&self, Vector[byte] buf)`
  and `Reader.read(&self, Vector[byte] &buf)` are now the final
  narrow-waist names.
- ✅ **Retired `println` / `writeln` / `println_str`** (2026-04-19).
  `print` stays as the infallible compiler builtin with `terminator=` /
  `file=` kwargs (script ergonomics). Typed-error callers use
  `write_display[File, D](&stdout, v)` / `write_str[File](&stdout, s)`
  / `write_all[File](&stdout, buf)` directly on the Writer primitives.
  Old fixture `stdlib_io_println.gg` replaced by
  `stdlib_io_stdout_typed.gg` exercising the same surface through the
  Writer primitives.
- ✅ **Upgraded `print`'s signature with `terminator=String` kwarg**
  (2026-04-19). Default `"\n"`. Covers TSV / CSV / custom-separator
  output without pre-embedding the separator in the string; pass `""`
  to suppress the newline. The old `newline=bool` sugar was dropped
  in the same change — terminator-as-String subsumes it and having
  two kwargs for the same intent adds noise. Fixture
  `print_terminator.gg`.
- ✅ **Added `Writer.flush(&self) -> Result[int, IoError]` with a
  default no-op body** (2026-04-19). In-memory writers (`String`,
  `Vector[byte]`) inherit the default; `File` overrides via
  `gorget_file_flush` to push the stdio buffer. Return type is
  `Result[int, IoError]` matching `write`'s shape; the int is
  unused on success (always `0`). Socket / TlsSocket could add an
  SSL-flush override later but TCP writes go straight to the kernel
  send buffer so the default is correct today.

### Phase 4: Concurrency-model enforcement — **DONE 2026-04-26**

All four items shipped; fixtures wired and green.

1. ✅ **Type-checker pass rejects `&` captures across `spawn`**.
   `check_spawn_args` (in `src/semantic/safety/helpers.rs`) checks
   each arg's borrow origin — non-`shared` borrowed locals are
   rejected with `SpawnWithBorrowedRef`. `check_spawn_closure_captures`
   walks the closure's `CaptureSet` and emits
   `SpawnClosureCaptureShared` / `SpawnClosureCaptureBorrowed` /
   `SpawnClosureCaptureMutable` for the three failure modes.
   Function-call, closure-variable-call, inline-closure, and
   method-call spawn forms all route through `check_expr.rs` Spawn
   handling. Fixture: `shared_closure_capture_error.gg`.
2. ✅ **`spawn unchecked` parser + safety bypass**. Parsed at
   `src/parser/expr.rs:465` (also `spawn blocking unchecked` /
   `spawn unchecked blocking` form). The Spawn handler in
   `check_expr.rs` short-circuits the capture check when
   `unchecked` is set — only the inner expression's normal
   move/borrow rules still apply. Fixtures: `spawn_unchecked.gg`,
   `spawn_unchecked_bypasses_check.gg`.
3. ✅ **Fixture / `xtd` audit**. No `xtd` consumer relies on `&`
   escaping spawns (verified 2026-04-25); the existing spawn
   fixtures (`spawn_blocking_*`, `spawn_closure_*`, `spawn_method_*`,
   `spawn_coroutine_*`) all use `shared` for cross-task mutable
   state or pass owned values through. Migration was effectively
   a no-op because the enforcement landed alongside the fixtures.
4. ✅ **Hashable migration to `void hash[Hasher H](self, H &h)`**.
   `lib/std/hash.gg` defines the `Hasher` trait
   (`write_int` / `write_bytes` / `write_string` / `finish`) and
   `FxHasher` as the default state machine. The `Hashable` builtin
   sig in `src/semantic/traits.rs:529` requires
   `void hash[Hasher H](self, H &h)` — generic over the Hasher
   implementation. `@derive(Hashable)` emits the generic body so
   field-by-field forwarding (`self.x.hash(&h)`) routes to whatever
   `H` the caller picked. The one-shot `int hash_of[Hashable T](T v)`
   uses `FxHasher` as the default; user code that wants a different
   Hasher writes its own
   `int my_hash[Hashable T](T v): MyHasher h = MyHasher(0); v.hash[MyHasher](&h); return h.finish()`.
   Dict / Set internals route through `FxHasher` for now (the runtime
   `gorget_map_hash_*` helpers are FxHasher-typed). Three compiler
   fixes were needed to unlock the generic form:
   - **Bound-method-generic body dispatch**. Per-call-site
     monomorphization of free functions like
     `apply_hash[Hashable T, Hasher H](T v, H &h): v.hash[H](&h)`
     never re-discovered method-call instances after substitution.
     Added a `walk_fn_body_for_method_calls` pass at the end of
     `discover_transitive`'s Function arm so the substituted body
     registers `Point__hash__MyHasher` as a method instance.
   - **Targ name substitution at call-site mangling**. The dispatch
     in `lower_method_call` mangled the call's targ AST as-is —
     `v.hash[H](&h)` produced `Point__hash__H` instead of
     `Point__hash__MyHasher` because the `H` AST node wasn't
     resolved through `generic_param_ast_types`. Added explicit
     substitution at the dispatch's targ-mangling step.
   - **Generic primitive-hash dispatch**. The IR-side fast path for
     `x.hash(&h)` on int/bool/String hardcoded `FxHasher__write_int`
     / `FxHasher__write_string`. Replaced with a runtime lookup of
     the hasher arg's actual type — falls back to
     `Hasher_for_<H>__write_int` when the user-defined Hasher only
     provides the `equip H with Hasher:` block. Verified with a
     fixture using both FxHasher and a user-defined `DjbHasher` on
     the same `@derive(Hashable)` struct.
   - Existing `Hashable` impls (e.g. `trait_bound_transitive.gg`)
     migrated to the generic shape via codemod-shaped find+replace.
   Fixtures: `derive_hashable.gg`, `dict_user_key_hashable.gg`,
   `set_user_key_hashable.gg`, `trait_bound_transitive.gg`. Status:
   **closed 2026-04-26**. Phase 4 fully done.

### Phase 5: Documentation — **DONE 2026-04-25**

1. ✅ `docs/language-design.md` §4.4.1 carries Writer / Reader /
   Hasher / Debuggable in the -er/-able convention tables, plus
   explainer blocks for the I/O role pair, the hashing
   capability/role split, and Debuggable vs Displayable.
2. ✅ `docs/language-reference.md` §15.2 method signatures synced
   to current API: String table now includes `find(pattern, from?,
   reverse?)`, `lines()`, `byte_at()`, extended `replace`/`split`
   `limit` forms, `trim_left`/`trim_right` aliases, and the
   state-based `hash(h)`. UTF-8 boundary table uses
   `Result[String, IoError]` with `IoError.Utf8Invalid(offset)`.
   Dict/HashMap `remove(key)` returns `Option[V]`. Set has
   `is_disjoint`. Dict/Set show `iter()` returning `DictIter[K, V]`
   / `SetIter[T]`. File API replaced with the `std.io` Writer/Reader
   primitives + typed-error helpers.
3. ✅ `docs/book/05-collections.md` Iteration with Adapters
   section rewritten 2026-04-25 — concrete iterator types per
   collection, nested adapter chain, eager terminals from
   `Iterator[T]` defaults, bound-needing free-function terminals,
   inferred `collect()`, lazy Set/Dict iteration. All snippets
   verified end-to-end.
4. ✅ `docs/book/appendix-traits.md` covers Writer, Reader,
   Debuggable, Error, IoError, ParseError, and the FxHasher /
   Hashable shape. Aspirational "swap in a different Hasher" note
   trimmed; outdated `trait Iterable[T]` declaration replaced with
   the actual name-based iteration convention; retired
   `println` / `writeln` / `println_str` block replaced with the
   surviving `print` builtin + Writer-primitive helpers.
5. ✅ `docs/book/19-stdlib.md` Lazy Iterators block updated —
   Set/Dict lazy bucket-walk, inferred `collect()` examples,
   `.keys()`/`.values()`/`.items()` still eager for callers that
   want the materialised form.
6. ✅ `docs/book/14-concurrency.md` carries the complete
   `&`/`shared`/`spawn`/`spawn unchecked` story (the chapter that
   was tentatively planned as `XX-concurrency.md`). Sigil-before-
   type typo fixed in §3.9.

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
| `String.join(vec)` kept (2026-04-19 revision) | 1.5 ✓ | None | Iterator alternative `join_iter[Iter, Displayable T](iter, sep)` added; both coexist |
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
