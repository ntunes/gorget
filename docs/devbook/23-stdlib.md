# Chapter 23 — The standard library narrow waist

Gorget's standard library is split across two namespaces: `std.*` (lean,
stable building blocks) and `xtd.*` (batteries — JSON, HTTP, regex, SDL,
…). Almost none of it is hard-coded in the compiler: every module is a
real `.gg` source file under `lib/std/` or `lib/xtd/`, embedded into the
`gg` binary via `include_str!` and merged into the user's module by the
loader before semantic analysis runs. This chapter covers the *narrow-waist*
architecture (a small set of orthogonal traits in the middle, M producers
and N consumers on the sides), the core `Iterator`/`Iterable`/`Drainable`
and `Writer`/`Reader` traits, capacity constructors, the lazy-iterator
adapter machinery, and how the compiler registers and types the parts that
*can't* be pure library code.

The design rationale (formerly the `stdlib-design.md` deep-dive, approved
2026-04-16, revised through Phase 5 in 2026-04) is folded into this chapter and
Chapter 22. This chapter re-derives the
*shipped* shape from current source — most of what that doc described in
present/future tense is now implemented in `lib/std/`, and the residual
roadmap (advanced adapters, the third const-borrow iterator tier) is noted
where relevant but is a TODO, not a description of today's code.

## 23.1 Module registration and loading

A built-in module is recognised by `is_builtin_module` in
`src/stdlib.rs:28`, which whitelists the legal `std.*` / `xtd.*` /
`gg.*` import segment paths. The actual source bytes come from
`builtin_module_source` (`src/stdlib.rs:64`), a flat `match` that maps
each segment path to an `include_str!("../lib/...")`. `generate_builtin_module`
(`src/stdlib.rs:55`) is a vestigial hook that always returns `None` — all
modules are now file-based; nothing is synthesised in Rust anymore.

Adding a module is mechanical and the steps are documented inline at
`src/stdlib.rs:18-24`: create the `.gg` file, add the name to
`is_builtin_module`, add a `None` arm to `generate_builtin_module`, add the
`include_str!` to `builtin_module_source`, add unit tests. The loader reads
the embedded source, parses it, recursively resolves *its* imports, and
merges the resulting AST into the main module; name resolution, type
checking, and borrow checking then run on the merged whole. There is no
separate "stdlib is pre-typed" fast path — the standard library is typed
the same way user code is, every compile.

Modules declare their C runtime bindings with `extern "C":` /
`extern "Gorget":` blocks carrying explicit `= "c_symbol"` annotations (see
`lib/std/io.gg:55-61` for the standard-handle externs, `lib/std/iter.gg:48-69`
for the generic map-iteration externs). ABI marshalling (cstr conversion,
`blocking` qualifier for shared-variable lock release) is derived from the
extern block's ABI string and the explicit FFI parameter types — not from
name matching.

## 23.2 The narrow-waist principle

The architecture is the Unix-file-descriptor model applied to a standard
library: a small core of orthogonal traits (1–2 methods each) in the middle,
producers below and consumers above, so the cost of N producers × M consumers
collapses to N + M. The layering, top to bottom:

- **`xtd.*` domain libraries** — consume the core traits, build on `std`.
- **`std.*` system interfaces** — implement `Reader`/`Writer`/`Iterable`.
- **Derived power** — `map`/`filter`/`fold`/`collect`/… defined *once* on
  `Iterator[T]`, free for every type that can produce an iterator.
- **The narrow waist** — the core traits themselves.
- **Core types** — `String`, `Vector`, `Dict`, `Set`, `Option`, `Result`
  with minimal type-specific mutation methods.

Two design rules drive the API surface and are visible throughout `lib/std/`:
**parameters for variation, not new methods** (`String.find(pattern, from,
reverse)` is one primitive; `index_of`/`rfind` are thin wrappers), and
**thin POLA wrappers over primitives** (`contains(s)` is `find(s).is_some()`).
The method *count* per trait stays small even as capability grows.

## 23.3 Core trait taxonomy

Naming follows two conventions: **role traits** use `-er`/`-or` ("is a X":
`Iterator`, `Writer`, `Reader`, `Hasher`) and **capability traits** use
`-able`/`-ible` ("is X": `Equatable`, `Comparable`, `Hashable`,
`Displayable`, `Debuggable`, `Cloneable`, `Iterable`, `Drainable`,
`Serializable`).

The core traits are registered in **two different ways** depending on whether
they need a hand-written Rust signature or can be ordinary library code:

1. **Built-in traits with Rust-side signatures** — registered by
   `register_builtin_traits` (`src/semantic/traits.rs:505`), a `Vec` of
   `(name, method-sig-map)` pairs covering `Displayable`, `Debuggable`,
   `Equatable`, `Comparable`, `Cloneable`, `Hashable`, `Hasher`, `Ordinal`,
   `Drop`, `Iterable`, plus the operator/conversion traits. `build_registry`
   (`src/semantic/traits.rs:436`) calls it before walking user traits.
   Where a parameter type can't be named at registration time (e.g. `Self`,
   a method-level generic, or a type whose `TypeId` doesn't exist yet) the
   slot is filled with `types.error_id` as a placeholder and impl validation
   reconciles it by *shape matching* rather than exact-type comparison.

2. **Trait-name reservations resolved against the real declaration** —
   `collect_top_level` (`src/semantic/resolve.rs:126-132`) `define`s the
   core trait *names* (`Iterator`, `Iterable`, `Hasher`, …) as `DefKind::Trait`
   placeholders so that `equip X with Iterator[T]:` blocks parse *before* the
   user-space declaration in `lib/std/iter.gg` loads. When the real trait
   declaration arrives it shadows the placeholder.

`Iterator[T]` is the load-bearing example of the second strategy: it is
**not** a built-in trait at all. `register_builtin_traits` deliberately omits
it (`src/semantic/traits.rs:624-628` is a comment explaining why) — declaring
it in `lib/std/iter.gg:81` as ordinary Gorget lets its default-method bodies
ride the standard `register_equip_sigs_with_defaults` machinery instead of
needing bespoke compiler support. Only a placeholder `DefId` is reserved by
`collect_top_level` (`src/semantic/resolve.rs:118`).

### Hashable / Hasher signature shape

`Hashable` is **purely compiler-registered** — there is no user-space
`trait Hashable` declaration anywhere in `lib/`. `register_builtin_traits`
registers it as `void hash(self, <error>)`
(`src/semantic/traits.rs:561-570`). The intended generic shape,
`void hash[Hasher H](self, H &h)` — state-based hashing generic over the
hasher — is documented as a prose comment (`lib/std/hash.gg:10`) and is the
signature both the `equip FxHasher with Hasher:` block and
`hash_of[Hashable T]` (`lib/std/hash.gg:93`) target, but it is never written
out as a `trait` declaration. The `H &h` parameter is erased to `error_id` at
registration because `H` is a method-level generic resolved at the call site
and the concrete `FxHasher` `TypeId` isn't available yet; impl validation
accepts the generic form by shape matching. `Hasher` itself (`src/semantic/traits.rs:574-601`)
carries `write_int`/`write_bytes`/`write_string`/`finish`, with `write_bytes`'
`Vector[byte]` parameter likewise placeholder-erased. The concrete `FxHasher`
state machine and the one-shot `hash_of[Hashable T]` lives entirely in
`lib/std/hash.gg` — `FxHasher` is a multiplicative mix (`state *% 31 +% v`,
`lib/std/hash.gg:44`), declared both as an inherent `equip FxHasher:` block
(the names the compiler emits for primitive `.hash(&h)` lowerings, e.g.
`FxHasher__write_int`) and an `equip FxHasher with Hasher:` block for
trait-vtable dispatch (`lib/std/hash.gg:42-84`).

## 23.4 Capacity constructors

There is no `with_capacity`. Every collection constructor takes an optional
`cap` named argument: `Vector[int](cap=1000)`, `Dict[String, int](cap=64)`,
`Set[String](cap=32)`, `String(cap=256)`. (Named arguments use `=`, never
`:` — the colon form is a parse error.) The compiler handles `cap` in
two places:

- **Typecheck** validates that `cap` / `alloc` are the only legal named args
  on the builtin constructors (`Vector`, `Dict`, `HashMap`, `Set`, `HashSet`,
  `Channel`, `String`, allocators). `src/semantic/typecheck.rs:1514-1646`:
  any other named arg is an `UnknownNamedArg` error; `alloc=` is checked to be
  an allocator type, and `cap=` must be an integer of any width
  (`is_integer_type`, round-33 — a non-int cap used to be silently deferred
  to lowering, where it ICE'd the backend, died as a cc error, or silently
  wrong-accepted via C implicit conversion; see
  `tests/fixtures/ctor_cap_arg_error.gg`).
  The same block rejects a positional 1-arg `String(x)` whose arg is neither
  an integer capacity nor String content (round-32, Core #8 — non-string args
  used to reach `gorget_string_from_str` and die as a cc/llc internal error).

- **GIR lowering** turns `Vector[T](cap=n)` into a `…__new` extern call
  followed by a `…__reserve` call. `src/ir/lowering/exprs/calls.rs:967-1026`:
  it finds the `cap` arg, lowers the fresh-allocation call, takes a mutable
  borrow of the result, and emits `{mangled}__reserve(ptr, cap)`. The `alloc=`
  path does the same inside a `push_allocator`/`pop_allocator` bracket. The
  `String(capacity)` special case routes to `gorget_string_with_capacity` for
  every integer width (shared `is_int_type_id` predicate, `src/ir/types.rs`;
  the named-arg site is `src/ir/lowering/exprs/calls.rs:675-700`, the
  positional sibling `src/ir/lowering/exprs/mod.rs:1651-1667`).

### Collection trait bounds

Bounds are stated once at the collection type, not per method.
`Dict[K, V]` and `HashMap[K, V]` require `K: Hashable + Equatable`;
`Set[T]` and `HashSet[T]` require `T: Hashable + Equatable`. These are
registered in `collect_top_level` at `src/semantic/resolve.rs:159-171` by
inserting into `struct_generic_bounds`, and enforced by `check_trait_bounds`
(`src/semantic/typecheck.rs:5149`) / `check_struct_type_bounds`
(`src/semantic/typecheck.rs:5204`), which emit `UnsatisfiedTraitBound`.
Numeric primitives, `bool`, `char`, and `String` satisfy
`Hashable`/`Equatable` intrinsically — see `is_hashable_primitive` /
`is_hashable_trait` (`src/semantic/traits.rs:420-426`), so the common
`Dict[String, int]` needs no user impl.

## 23.5 Core-type method typing

The hot collection methods (`Vector.push`, `Dict.get`, `Set.contains`, …)
are typed in Rust by `builtin_method_type` (`src/semantic/typecheck.rs:4771`),
not by a `.gg` declaration — they are too central and too tied to runtime
calls to round-trip through library typing. The function dispatches on the
receiver's base-type name and the method name, returning the result `TypeId`.

The ownership-aware return shapes encode the CoW borrow/own discipline at
the type level:

- Borrowing accessors return `Option[T &]`: `Vector.get`/`first`/`last`
  build `Option[Ref(elem)]` (`src/semantic/typecheck.rs:4812-4819`),
  `Dict.get` builds `Option[Ref(val)]` (`:4855-4862`).
- Consuming accessors return `Option[T !]`: `Vector.pop`/`remove` build
  `Option[Owned(elem)]` (`:4821-4828`), `Dict.remove` returns
  `Option[Owned(val)]` (`:4866-4874`) — note `remove` returns the removed
  value, not a bool.
- `Dict.keys`/`values` return materialised `Vector[K]`/`Vector[V]`
  (`:4877-4890`); `Vector.windows`/`chunks` return eager `Vector[Vector[T]]`
  (`:4841-4849`).

Method names that were consolidated away still resolve as compat aliases
here — e.g. `Dict.put`/`set`/`update` all map to `void`
(`src/semantic/typecheck.rs:4854`) and `contains`/`has`/`has_key`/`contains_key`
all map to `bool` (`:4864`) — but only the consolidated name is the
recommended surface.

## 23.6 Iterator: the M+N payoff

`Iterator[T]` (`lib/std/iter.gg:81`) has one required method,
`Option[T] next(&self)`. Everything else — every higher-order operation — is
a **default-method body on the trait**, so any type that equips `Iterator[T]`
inherits the entire surface for free. This is where M+N pays off: the
adapters and terminals are written once.

### Eager terminals (default bodies)

The unbound terminals are plain default methods that drive the for-loop
protocol on `self`: `count` (`lib/std/iter.gg:96`), `collect`
(`:102`, materialises a `Vector[T]`), `last`/`nth`, the method-generic
`any[F]`/`all[F]`/`find[F]`/`find_index[F]`/`for_each[F]`/`fold[A, F]`
(`:153-187`). Because they iterate via `for x in self`, each equipping type
gets a specialised loop with no per-method-per-type duplication — the loop
dispatches `next(&self)` through ordinary name-based resolution.

The **bound-needing** terminals — `min`/`max` (use `<`/`>`), `sum`
(uses `T.default()` + `+`), `product` (uses `T.one()` + `*`), `contains`
(uses `==`), `join` (uses `x.display()`) — are also defaults on `Iterator[T]`
(`lib/std/iter.gg:205-265`), operating on the abstract `T` directly. They are
emitted per-impl only when a call site reaches for them, gated by the
demand-gate `all_return_nominals_registered`, so an `Iterator[T]` whose `T`
isn't Comparable/Numeric/Equatable/Displayable never has these monomorphised
and the body's operators never need to resolve. The free-function variants
(`min_iter`, `sum_iter`, …) were retired once the defaults shipped; method
form is canonical.

### Lazy adapters (concrete return, not trait object)

Adapter methods return a **concrete state-machine struct**, not a trait
object, so `.iter().filter(f).map(g).take(10).collect()` monomorphises into
a single fused loop with closures inlined. Trait-object iteration would force
virtual dispatch per element and make fusion impossible. The cost is
type-signature verbosity (`MapIter[FilterIter[VectorIter[T], F], T, U, G]`),
which library authors see but users rarely do.

Each adapter is a generic struct over its source iterator, equipped with
`Iterator`:

| Adapter | Struct | Equips |
|---|---|---|
| `take`/`skip` | `TakeIter[Iter, T]` / `SkipIter[Iter, T]` | `Iterator[T]` |
| `map` | `MapIter[Iter, T, U, F]` | `Iterator[U]` |
| `filter` | `FilterIter[Iter, T, F]` | `Iterator[T]` |
| `filter_map` | `FilterMapIter[Iter, T, U, F]` | `Iterator[U]` |
| `take_while`/`drop_while` | `TakeWhileIter`/`DropWhileIter[Iter, T, F]` | `Iterator[T]` |
| `inspect` | `InspectIter[Iter, T, F]` | `Iterator[T]` |
| `enumerate` | `EnumerateIter[Iter, T]` | `Iterator[(int, T)]` |
| `zip` | `ZipIter[IterA, IterB, A, B]` | `Iterator[(A, B)]` |
| `chain` | `ChainIter[IterA, IterB, T]` | `Iterator[T]` |
| `windows`/`chunks` | `WindowsIter`/`ChunksIter[Iter, T]` | `Iterator[Vector[T]]` |

(See the struct + `equip` definitions, `lib/std/iter.gg:344-768`.) The
adapter *constructors* are themselves defaults on `Iterator[T]` returning
`AdapterIter[Self, T, …]` (`lib/std/iter.gg:283-308`), so chains compose
past one step: every iterator implementor inherits `.take`/`.map`/`.filter`/…
`chain`/`zip`/`lazy_windows`/`lazy_chunks` stay specific to `VectorIter`
(`lib/std/iter.gg:449-460`) because their `other` parameter is
iterator-typed — lifting them to `Iterator[T]` needs threading a second
iterator's concrete type through the adapter field, which the current
method-generic inference doesn't do.

### The adapter contract — laws, not hand-checked invariants

Every implementor and every adapter chain preserves a small set of algebraic
identities. They are stated as laws rather than prose because they are meant to
be *property-test* targets: a chain that breaks one is wrong even when each
adapter looks right in isolation.

- `iter().count() == len()` for sized iterables — an adapter may not lose or
  invent elements.
- `iter().filter(f).all(f)` holds — filtering is total, not best-effort.
- `iter().map(f).collect()` preserves order for ordered containers, so a
  `Vector` round-trips through a map chain in the same sequence.
- `a.iter().chain(b).count() == a.len() + b.len()` — concatenation is additive
  (`VectorIter`-scoped today, per the note above).

Two further laws are stated but not yet testable, because the operations they
quantify over do not exist: an involution law for `rev()` (which needs a
double-ended iterator concept) and a sortedness law pairing `sort()` with an
`is_sorted()` predicate. Both are recorded with the iterator-surface work in
`TODO.md` rather than here, since this chapter describes what ships.

### Lazy by default — no eager interim

Adapters are lazy from day one. There is deliberately *no* eager
implementation where adapters allocate intermediate Vectors: an eager
`iter()` slower than direct `vec.map(f)` would train users to stay on the
eager Vector API permanently. Eager convenience wrappers (`v.map(f)`,
`v.filter(p)`, …) exist as thin shells over `self.iter().method().collect()`
in the `equip [T] Vector[T]:` block (`lib/std/iter.gg:386-438`); Set has the
terminal wrappers (`each`/`for_each`/`any`/`all`/`find`/`find_index`/`fold`,
`lib/std/iter.gg:818-840`).

### `collect()` and the collect-target rewrite

`collect()` is a single default returning `Vector[T]` (`lib/std/iter.gg:102`).
To target a `Set` or `Dict` *from the binding type*, a post-typecheck AST pass
`apply_collect_target_rewrites` (`src/semantic/typecheck.rs:5896`) rewrites the
method name in place: if a `VarDecl`'s declared type is `Set[T]` it swaps
`.collect()` → `.to_set()`; if `Dict[K, V]` it swaps to `.to_dict[K, V]()`
and lifts the K/V generic args straight from the LHS
(`src/semantic/typecheck.rs:5938-5947`). The pass only fires on a `MethodCall`
named exactly `collect` — any other RHS shape is left alone. `to_set`
(`lib/std/iter.gg:115`) and `to_dict[K, V]` (`:131`) are themselves
`Iterator[T]` defaults; `to_dict`'s body reaches for tuple fields `x.0`/`x.1`,
so non-tuple `T` instances fail at mono emission rather than silently
producing garbage.

### Ownership sigils on the element type

`Iterator[T]`'s `T` is meant to carry an ownership sigil — `Iterator[T &]`
yields mutable borrows (the source survives, produced by `Iterable.iter()`),
`Iterator[T !]` yields owned moves (the source is consumed, produced by
`Drainable.drain()`). One trait, two instantiations; the HOFs are written once
and work for both. The third, bare-`T` const-borrow tier is **deferred** —
Gorget's `&` is not Rust-style exclusive access (see §23.8), so a signature-
level no-mutation contract buys less than it does in Rust. Sigils at
type-argument positions (inside `[...]`) are a grammar/type-system extension
that touches the parser, resolver, inference, monomorphizer, and borrow
checker; this remains roadmap rather than fully shipped, and the deferred
const-borrow tier is a TODO, not present behaviour.

### Iterable / Drainable

`Iterable[T]` (`lib/std/iter.gg:326`) is the capability "can be iterated
non-consumingly" — `Iterator[T] iter(&self)`. `for x in v` desugars to
name-based dispatch on `iter()`; the trait provides the contract and enables
`[Iterable T]` bounds but the for-loop fast path does not route through a
trait vtable. `Drainable[T]` (`:341`) is the consuming sibling —
`Iterator[T] drain(^self)` moves the source in and yields owned elements.

`Vector` equips both: `iter()` returns `VectorIter[T]`
(`lib/std/iter.gg:356-358`); `drain()` is an O(n) reverse + repeated O(1)
`pop()` via a `VectorDrain[T]` whose custom `Drop` reverses the unconsumed
tail back so early-break elements still drop in insertion order
(`lib/std/iter.gg:360-384`). `Set.iter()` returns `SetIter[T]` and
`Dict.iter()` returns `DictIter[K, V]` — both walk the underlying `GorgetMap`
bucket array in place through a `Ref[Set[T]]` / `Ref[Dict[K, V]]` borrow field,
with no `.items()` materialisation (`lib/std/iter.gg:799-882`). The bucket walk
uses generic externs (`__dict_iter_order_len[K, V]`, `__dict_iter_key`, …,
`lib/std/iter.gg:48-56`) that share a single C symbol across all
instantiations — generic monomorphisation preserves the symbol, so one runtime
function serves every `Dict`/`Set` shape. The accessors clone resource-typed
K/V through the map's `key_clone`/`val_clone` hooks so the consumer can drop
yielded elements without disturbing the source.

## 23.7 Writer / Reader — byte-shaped I/O

I/O primitives are byte-shaped, not text-shaped: binary files, TLS sockets,
and compression streams are not UTF-8. The two role traits live in
`lib/std/io.gg`:

```
trait Writer:
    Result[int, IoError] write(&self, Vector[byte] buf)   # lib/std/io.gg:242
    Result[int, IoError] flush(&self): return Ok(0)        # default no-op

trait Reader:
    Result[int, IoError] read(&self, Vector[byte] &buf)   # lib/std/io.gg:257
```

`write` returns the byte count *actually* written (may be short for sockets
/ pipes); `read` fills the caller's buffer through a mutable borrow and
returns the count read, with `Ok(0)` meaning EOF. `byte` is a lexer-level
alias for `uint8` — the same type at the AST level, zero conversion cost
(`src/lexer/token.rs:471`). There is no nominal `Bytes` wrapper; `Vector[byte]`
*is* the byte-buffer interface, matching Rust's `&[u8]` and Go's `[]byte`.

Derived convenience (`write_all`, `write_str`, `write_display`,
`reader_drain`, `read_exact`) is written once as **generic free functions**
over the concrete Writer/Reader type, not as `equip`-on-trait default methods:
`Result[int, IoError] write_all[W](W &w, Vector[byte] buf)`
(`lib/std/io.gg:428`), `write_str[W]` (`:444`),
`write_display[W, Displayable D]` (`:449`), `reader_drain[R]` (`:464`,
the EOF-drain loop), `read_exact[R]` (`:495`). Monomorphising over `W`/`R`
avoids trait-object dispatch — callers write `write_all[Sink](&w, buf)`. The
`Writer`/`Reader` traits themselves carry only the one required method
(`write` / `read`) plus `Writer`'s no-op `flush` default, so in-memory writers
inherit `flush` for free while `File` overrides via `gorget_file_flush`.
Implementors: `String` (append bytes to its builder via `__bytes_to_str_raw`,
`lib/std/io.gg:266-271`), `File` (`equip File with Writer`/`Reader` wrapping
the negative-errno C helpers, `:318-341`), plus Socket / TlsSocket / stdout /
stderr / stdin in their respective modules. (`File` also carries its own
`read_all` extern bound to `gorget_file_read_all` at `lib/std/io.gg:31-32` —
that is a File-specific convenience, not the generic Reader drain helper.)

### Typed errors, not stringly-typed

Stdlib I/O reports failure through a typed `IoError` enum
(`lib/std/io.gg:100-120`) — `NotFound`, `PermissionDenied`, `BrokenPipe`,
`UnexpectedEof`, `Utf8Invalid(int)`, `Other(String)` as the escape hatch, …
This enables pattern-matching on category and programmatic recovery that
`Result[T, String]` forecloses, and `Result[T, String]` is a one-way door
(downstream code papers over mismatches with `.map_err(to_string)`). A
private `_errno_to_io_error` (`lib/std/io.gg:280`) maps Linux errno values to
variants so every File/Socket impl funnels through the same mapping. The
broader `Error` trait (`lib/std/io.gg:223`, `extends Displayable & Debuggable`)
is the cross-domain contract every stdlib error type should implement.
`File` itself was moved from `collections` to `std.io` because it is an I/O
concept (`lib/std/collections.gg:21-22`); `collections.gg` now holds only the
empty-bodied `struct Vector/Dict/HashMap/Set/HashSet/Box` placeholders whose
real methods are compiler-typed (§23.5).

## 23.8 Concurrency boundary (sigil enforcement)

The standard library's safety story is: Gorget's `&` is **not** Rust-style
exclusive access — multiple `&` borrows can coexist single-threaded. Races
happen at thread boundaries, so that is where the line is drawn: a plain `&`
borrow cannot escape a `spawn`; cross-thread mutable aliasing must be
`shared`. The enforcement is a type-checker pass (`check_spawn_args` /
`check_spawn_closure_captures` in `src/semantic/safety/helpers.rs`) with a
local `spawn unchecked` opt-out parsed in the expression grammar. This is a
compiler concern rather than a library one, covered in the safety/borrow
chapters; it is relevant here only because `Iterator[T !]` (drain) and the
`shared` sigil are what make the M+N iterator surface safe to use across
tasks without a Rust-tier borrow tax. (Full detail was in the former
`stdlib-design.md` §8, now folded into this chapter.)

## 23.9 Roadmap (not yet shipped)

For completeness, items the design doc lists that are *not* present today and
remain TODOs:

- **Const-borrow iterator tier** (`Iterator[T]` with bare `T`) and sigils at
  type-argument positions as a fully-landed grammar feature (§23.6).
- **Advanced adapters** — `scan`, `intersperse`, `cycle`, `peekable`,
  `DoubleEndedIterator` — deferred, demand-driven.
- **Lifting `chain`/`zip`/`lazy_windows`/`lazy_chunks` off `VectorIter`**
  to all iterators — these stay `VectorIter`-specific (`lib/std/iter.gg:449-460`)
  because their `other` parameter is iterator-typed and the current
  method-generic inference can't thread a second iterator's concrete type
  through the adapter field. (Set/Dict `Drainable` itself is *shipped* —
  `equip Set with Drainable[T]` at `lib/std/iter.gg:947` and
  `equip Dict with Drainable[(K, V)]` at `:968`, driving the
  `__set_drain_entry`/`__dict_drain_entry` runtime helpers.)
- **Turbofish `collect[Set[int]]()`** — today the binding-type rewrite
  (§23.6) covers the inference case; explicit turbofish routes through
  `.to_set[T]()` / `.to_dict[K, V]()` directly.

## 23.10 Collection layout — Dict/Set dense index-map + StableMap/StableSet

`Dict`/`Set` use the dense index-map layout — a compact insertion-order `entries` array (packed keys and, for `Dict`, packed values) plus a hash `indices` table pointing into it — matching the shape used by Rust's `indexmap`, Zig's `ArrayHashMap`, Python's compact dict, Swift's `OrderedSet`, and .NET 9's `OrderedDictionary`. `HashMap`/`HashSet` stay open-addressed with linear probing and tombstone reuse — that combination *is* their reason to exist and dense would erase it. The prior tombstone layout survives as `StableMap`/`StableSet` in `lib/std/stablemap.gg` (Phase B, forward-referenced), following the `std.slotmap` precedent for specialist collections, which tiers them out of `std.collections` and out of the book's collections chapter.

### Cost table

| Operation | `Dict`/`Set` (dense) | `HashMap`/`HashSet` (open-addressed) |
|---|---|---|
| `get(k)` / `contains(k)` | O(1) | O(1) |
| `put(k, v)` / `add(k)` | O(1) amortised | O(1) amortised |
| `remove(k)` (order-preserving) | O(n) — shifts entries + decrements indices | O(1) — tombstone the slot |
| `swap_remove(k)` (order-destroying) | O(1) — swap last entry into the freed slot | O(1) — equivalent to `remove` (order is unspecified) |
| Iteration | O(n) dense walk | O(cap) bucket walk |
| Ordinal access `.nth(i)` / `.key_at(i)` (Phase C, forward-referenced) | O(1) | N/A — not defined |

### The remove/rank tradeoff — why O(1)/O(1) is impossible

Dense buys O(1) key lookup, O(1) ordinal rank, and O(1) `swap_remove`. What it costs is order-preserving `remove`: shifting the entries array on middle deletions is O(n), and the reference promises insertion-order iteration as a global invariant so a swap-style default would break it silently. That trade is not arbitrary — it is forced. Order-preserving removal decrements the rank of every subsequent element, so any structure answering "the k-th live element" in O(1) must materialise ranks, and one removal invalidates O(n) of them. This is the dynamic partial-sums / list-indexing problem, and Fredman–Saks (cell-probe model) puts a logarithmic-ish lower bound on it: no data structure can beat that on the RAM machine.

### The theoretical-optimum shape (named fallback, not shipped)

The Fenwick tree over live flags saturates the Fredman–Saks bound: an implicit binary indexed tree layered over the tombstone-retaining slot array gives O(log n) rank *and* O(log n) removal, with tombstones retained so nothing relocates and address stability survives. Layout: the entries array retains its slot indices exactly as `StableMap`/`StableSet` do; alongside it, a same-length `int[]` Fenwick tree stores per-slot live flags (1 = live, 0 = dead), and rank queries reduce to a prefix sum through the tree. Insertion appends and increments one Fenwick prefix; `remove(k)` marks the flag dead and decrements one prefix; iteration walks the raw slot array skipping dead flags. The important structural property — nothing else relocates on a middle removal — is what makes it address-stable.

This is the *named fallback* for remove-heavy ordered workloads, not the default. Two reasons: measurement-driven — the workload has to earn it, the default should be the shape that predicts its dominant cost — and cognitive — O(log n) `[]` invites the same "is this cheap?" confusion O(n) does, and the whole point of splitting `Dict`/`Set` from `StableMap`/`StableSet` is that the cost of the operator is legible at the type. The moment measurement shows this shape is worth landing it belongs on `StableMap`/`StableSet` (they already carry the "removal-tolerant" signal), and the choice between the plain O(1)/O(n) `StableMap` and a Fenwick-backed O(log n)/O(log n) `StableMap` becomes an implementation swap that does not touch the public API.

### Removal-naming discipline

`remove` is order-preserving on `Vector` (`remove(index)` shifts subsequent elements, O(n)); `swap_remove` is the marked O(1) opt-in that moves the last element into the hole and destroys order. `Dict`/`Set` carry the exact pair unchanged — same operator names, same asymmetry. `fast_remove` was rejected: name the hazard, not the reward — the dangerous property is that it silently reorders the collection, which `swap_remove` predicts and `fast_remove` conceals; "fast" is a relative claim that rots as the cost table changes; and the `Vector` pair is the dominant precedent (Rust `Vec`/`indexmap`, Zig `ArrayList`/`ArrayHashMap`, the closest existing analogue to Gorget's situation). The asymmetry is deliberate: the safe operation keeps the unmarked name; the hazardous one carries the qualifier.

Return-type mirrors each receiver's own `remove` — `Dict.swap_remove(key)` returns `Option[V]` (the removed value or `None`), `Set.swap_remove(elem)` returns `bool` (was the element present?). `void` was rejected on key-based receivers because a silent no-op on a missing key is a footgun the caller cannot detect. `HashMap`/`HashSet` accept `swap_remove` under the same signatures, but since their iteration order is unspecified they are semantically equivalent to `remove` there; the pair is exposed for source-level parity across the ordered/unordered split rather than for a performance win.

Mid-iteration mutation (`for k in d.keys(): d.swap_remove(k)`) leaves the iteration in an unspecified state — the visitor may see the new state, the old state, or skip/duplicate elements. Callers materialise first (`for k in d.keys():` collects a fresh `Vector[K]`) or use the drain family. This convention is language-wide and does not depend on the layout choice.

### Ordinal access

`[]` on `Dict`/`Set` means key lookup — `d[k]` for `Dict[K, V]`, undefined for `Set` since a set's elements *are* its keys (D38). Ordinal access is a **named accessor**: `.nth(i)` on sets, `.key_at(i)` / `.entry_at(i)` on maps, O(1) once dense lands, symmetric across `Set` and `Dict` and unambiguous at every key type. ⚠ Do not name the accessor `.ordinal(…)` or hang it off an `Ordinal`-ish trait — `Ordinal` is already taken (`src/semantic/traits.rs:627-637`, `int ordinal(self)`) and runs in the inverse direction (thing → position); its true analogue is the existing `enum_from_ordinal` meta builtin.

## In the self-host

n/a — the self-host frontend (`tests/fixtures/self_host_*`) reimplements the
lexer, parser, resolver, type checker, and GIR lowerer, but **not** the
standard library itself. The stdlib is plain `.gg` source compiled by the
frontend like any other program; there is no separate self-host
re-implementation of `lib/std/` or `lib/xtd/`, and the `*_comparison` tests
do not cover a self-host stdlib because there isn't one. The relevant parity
question — "does the self-host frontend *compile* the stdlib the same way
Rust `gg` does?" — is exercised transitively whenever a fixture imports a
std module, but the narrow-waist library code is shared, not forked.
