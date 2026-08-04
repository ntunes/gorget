# 18 — The runtime & the backend ABI contract

This chapter documents the contract between Gorget's compiled output and its C
runtime library: the byte layout of the runtime value types
(`GorgetString`, `GorgetArray`, `GorgetMap`, `GorgetSet`), the view/owned
discriminator that those layouts share, the clone/materialize ABI for
copy-on-write, and the *single source of truth* for runtime-symbol signatures —
the `RuntimeFn` enum plus the data-driven resource table. The runtime itself
lives as an embedded C string in `src/backend/c/c_runtime.rs`; the typed view of
its API surface lives in `src/lir/runtime.rs` (`RuntimeFn`) and
`compiler/data/resources.gg` (the resource table). Everything here is "the
contract", not a suggestion — a frontend that lowers a call with the wrong ABI
tag, or a layout change that doesn't update both sides, produces silent memory
corruption rather than a compile error.

## The runtime value types share a layout prefix

The four heap-backed runtime types are plain C structs defined together in
`c_runtime.rs`. Their full field sets differ, but they were deliberately laid
out so the first two machine words coincide:

- `GorgetArray` — `{ void* data; size_t cap; size_t len; size_t elem_size; ... }`
  (`src/backend/c/runtime/runtime_preamble.c:345`). The struct carries three trailing function
  pointers (`elem_drop`, `elem_clone`, `elem_materialize`) for resource-typed
  elements.
- `GorgetMap` — `{ void* keys; size_t cap; void* values; ... }`
  (`src/backend/c/runtime/runtime_preamble.c:358`). `GorgetSet` is a typedef alias of
  `GorgetMap` (`src/backend/c/runtime/runtime_preamble.c:386`).
- `Str` / `GorgetString` — `{ char* data; size_t cap; size_t len; GorgetAllocator* alloc; }`
  (`src/backend/c/runtime/runtime_string.c:22`, with `typedef Str GorgetString;` at
  `:30`). `data` is `char*` rather than `void*` so it can be used directly as a
  C string.

The common invariant is: the pointer/handle is at offset 0, and **`cap` is the
field at offset +8** (word index 1) in every one of them. The runtime header
calls this out explicitly at each definition ("offset +8: generic view
discriminator (0 = view)") and the `Str` block spells out the byte offsets
(`data`@0, `cap`@8, `len`@16, `alloc`@24 — `src/backend/c/runtime/runtime_string.c:6`–`9`).

### Why the abstract type is narrow

The compiler's typed view of these structs (`CRuntimeType` in
`src/lir/runtime.rs:42`) is deliberately *narrower* than the full `LirType`: only
`Str`, `Array`, `Map`, `Set` name a runtime struct; everything else is a scalar,
a `Ptr`, a `VoidElem` (collection-element pointer), or a `CStr`. The reason is
that `CRuntimeType` must be `const`-buildable for the static signature table,
while `LirType::Struct(StructId)` needs a runtime-allocated id — so
`CRuntimeType::to_lir_type` resolves the struct types through the per-module
`StructRegistry` at call time, degrading to `LirType::Ptr` when the module never
registered that struct (`src/lir/runtime.rs:80`–`103`). This is the "narrow
waist": a handful of named structs cross the runtime boundary; the rest of the
LIR type lattice never reaches the runtime ABI.

## The view discriminator: `cap == 0` means "view"

The shared offset-+8 `cap` field is a **view discriminator**, not just a
capacity. For `Str`:

- `cap == 0` ⟺ **view** — `data` borrows a buffer the `Str` does not own (a
  `.rodata` literal, or a slice into another `Str`'s buffer). Drop is a no-op.
- `cap > 0` ⟺ **owned** — drop frees `data` via `alloc->dealloc(data, cap)`.
- `len` is authoritative in both cases.

(`src/backend/c/runtime/runtime_string.c:7`–`13`.) String literals lower to static
view structs — `gorget_string_free` short-circuits on a view
(`if (s->cap == 0) { *s = (Str){0}; return; }`, `src/backend/c/runtime/runtime_string.c:121`),
and the `GORGET_SLIT` macro builds a zero-alloc compound literal with `.cap = 0`
(`src/backend/c/runtime/runtime_string.c:60`).

Because the field is at the *same* offset across all four types, the runtime can
test view-ness generically without knowing which struct it holds:

```c
// src/backend/c/c_runtime.rs:1458
static inline bool gorget_is_view(const void* resource) {
    return ((const size_t*)resource)[1] == 0;   // word index 1 == cap
}
```

This is the layout fact the backend depends on, and the reason the field order
was chosen — `{data, cap, len, alloc}` puts `cap` where `GorgetArray`/`GorgetMap`
already had it (`src/backend/c/runtime/runtime_string.c:20`).

## Cover structs: under-declared Gorget layout over a larger C struct

Some Gorget structs deliberately **under-declare** their layout to "cover" a
larger C runtime struct. The Gorget-visible declaration carries fewer (or
smaller) fields than the real runtime ABI struct, and the compiler treats the
Gorget value as an opaque handle of the runtime's size. Examples:

- `struct File: int handle` (`lib/std/io.gg:23`) covers the 16-byte
  `GorgetFile` `{ int handle; bool owned; ... }`. The Gorget-visible 8-byte
  `int` spans the C struct's first word; the runtime owns both halves.
- `struct TlsSocket: int _handle` (`lib/std/tls.gg:8`) covers the 24-byte
  `GorgetTlsSocket` `{ int64_t fd; SSL_CTX* ctx; SSL* ssl }`.
- `struct ArenaCheckpoint` (`lib/std/alloc.gg:6`) covers a 16-byte
  `{ GorgetArenaBlock* block; size_t used }`.
- Zero-field opaque handles (`TaskGroup`, the allocator handles) cover an
  8-byte pointer.

The raw field-sum of a cover struct is therefore **smaller than its real
runtime size**, and that wrong-small size is poison if it reaches an ABI
decision: the sret-vs-register return choice, a move-out `memcpy` width, or a
trailing-pad calculation all read `computed_c_size`, and a size that is too
small returns the struct through the wrong path or copies only part of it →
SIGSEGV or a truncated value.

The fix is **one source of truth per axis** (`docs/devbook/24-layering-discipline.md`
Rule 3): a cover struct's `computed_c_size` is fixed ONCE, at registration, in
`compute_struct_sizes` (`src/lir/mod.rs`):

```rust
let size = match lower::types::opaque_runtime_size(&self.structs[i].name) {
    Some(rt) => field_sum.max(rt),
    None     => field_sum,
};
```

`opaque_runtime_size` (`src/lir/lower/types.rs:358`) is the runtime-ABI floor:
it returns `None` for ordinary user structs (no change), `Some(==field_sum)` for
already-agreeing runtime singletons (no-op), and `Some(>field_sum)` exactly for
cover structs — where the `max()` lifts the cached size to the real layout. Once
`computed_c_size` is set this way, **every downstream ABI consumer reads it** and
none re-derives the size from the field list. Re-deriving the size at a read site
is the bug: it discards the floor the writer applied.

This class has bitten three times — each a cover struct whose `computed_c_size`
fell to its field-sum and leaked into an ABI decision: (1) `091faaef`,
zero-field `TaskGroup` (`computed_c_size = 0` vs `opaque_runtime_size = 8`);
(2,3), one-field `TlsSocket` (8 vs 24, `GorgetTlsSocket`) and `File` (8 vs 16,
`GorgetFile`), fixed by the `max()` in `2d720077`. Per core-invariant #6
("convert a recurring bug class into an executable guard"), the invariant is now
enforced by a runtime guard — `cover_struct_size_never_below_runtime_abi` in
`src/lir/mod.rs`'s test module builds a minimal cover struct under each
cover-struct name, runs `compute_struct_sizes`, and asserts the cached size never
drops below `opaque_runtime_size`. A reverted/weakened `max()` fails it. The
war-story lives in `docs/devbook/29-contributor-playbook.md`.

## The clone / copy / materialize ABI

Copy-on-write is realized through three runtime entry points whose ABI is part
of this contract (all take a `const T*` or `void*` pointer to the in-slot value):

- **clone** — deep copy. The collection clones `gorget_array_clone` /
  `gorget_map_clone` / `gorget_set_clone` deep-clone every element into a fresh
  collection and are tagged `sig_fresh` (`returns_fresh: true` — see below)
  because they always return an independently-allocated value that aliases no
  input (`src/lir/runtime.rs:366`, `:405`, `:431`). The string clones
  `gorget_string_clone` / `gorget_string_clone_to_owned`
  (`src/backend/c/runtime/runtime_string.c:192`, `:206`) are tagged with **plain `sig`**
  (`returns_fresh: false` — `src/lir/runtime.rs:349`–`350`): for non-empty input
  they `str_alloc_copy` a fresh owned buffer, but for empty input they return the
  shared static `GORGET_EMPTY_STR` view (`if (src->len == 0) return
  GORGET_EMPTY_STR;`, `src/backend/c/runtime/runtime_string.c:193`), and `GORGET_EMPTY_STR` is a `cap == 0` static
  (`src/backend/c/runtime/runtime_string.c:56`) — so the result can alias a static rather
  than always being a fresh non-aliasing buffer, which is exactly why these are
  not `sig_fresh`.
- **copy-on-write** — `gorget_string_copy_cow` (`src/backend/c/runtime/runtime_string.c:215`)
  branches on the discriminator: a view (`cap == 0`) is a 32-byte struct copy
  with **zero allocation**; an owned string is deep-cloned. This is the runtime
  half of the `String t = s` Copy path.
- **materialize** — `gorget_string_materialize_inplace`
  (`src/backend/c/runtime/runtime_string.c:250`) upgrades an in-slot *view* to owned
  in-place (`if (s->cap == 0 && s->len > 0) { ... str_alloc_copy ... }`), and is
  a no-op on an already-owned value. It is invoked through the per-element
  function-pointer hooks (`elem_materialize` / `val_materialize` /
  `key_materialize`) that a collection carries, *after* the caller memcpys a
  value into a slot — this is lazy CoW: a borrowed view crossing into owning
  storage gets upgraded on the fly.

The element-level hooks live at fixed struct offsets that the *LIR lowering*
emits as `Store` insts (via `emit_collection_fn_ptr_stores`) when constructing a
collection of resource-typed elements — not a write done by the C backend. The
offsets are computed in `infer_fn_ptr_stores_from_types`: `GorgetArray`'s
`elem_drop`/`elem_clone`/`elem_materialize` at 40/48/56
(`src/lir/lower/insts.rs:2092`), and `GorgetMap`'s
`val_clone`/`key_clone`/`val_materialize` at 112/128/136
(`src/lir/lower/insts.rs:2112`). The stores are emitted as
`NamedFuncAddr` + `ElemPtr` + `Store` against the slot address
(`src/lir/lower/insts.rs:4086`–`4097`); both backends compile those LIR insts
uniformly (`src/backend/c_lir/emit_call_extern.rs:887`–`897`). The per-type
clone hook is named
`{Type}__clone_inplace` and the by-value deep-clone is `{Type}__clone(ptr)`
(`src/lir/lower/insts.rs:1067`). The `*__clone` ABI is "pointer in" — the LIR
forces `LirType::Ptr` params for these so they match the runtime convention (see
the clone-function ABI note in the LIR backend memory and `insts.rs:1069`).

## The runtime declaration table: one source of truth for signatures

A runtime call must agree with the runtime on parameter types, return type, and
per-argument ABI marshalling. Getting any of those wrong is undiagnosable at the
C level (it compiles, then corrupts). Gorget makes the agreement structural with
two cooperating tables.

### `RuntimeFn` — the typed call boundary

`src/lir/runtime.rs` defines an enum whose variants are the *only* legal way to
name a runtime function inside `Inst::CallRuntime`. The enum and its parallel
signature `REGISTRY` slice are generated together by the `runtime_table!` macro
(`src/lir/runtime.rs:213`), one line per function:

```rust
// src/lir/runtime.rs:285
StrCat => "gorget_str_cat",
    sig_fresh(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
```

Because the macro emits the enum variant and the registry entry from the same
line, the variant's `as usize` ordinal *is* its registry index — drift between
the two is structurally impossible. Accessors hang off the enum:
`c_name()` (`:598`), `signature()` (`:604`), `resolve_lir_sig(sr)` which
resolves the abstract signature to concrete `LirType`s + per-param `AbiKind`s
through the module's `StructRegistry` (`:611`), `from_c_name()` — a
`OnceLock<FxHashMap>` reverse lookup (`:630`), and `may_mutate()` (`:657`).
`RuntimeFn::count()` reports the table size. (Run `cargo test -p <crate> -- runtime`
or read `REGISTRY` for the current count; do not transcribe a figure here.)

Each entry carries a `RuntimeSig` (`src/lir/runtime.rs:159`): `params` as
`(CRuntimeType, AbiKind)` pairs, `ret`, a `SideEffects` tag, and
`returns_fresh`. `SideEffects` (`:125`) is a coarse classification — `Pure`,
`ReadOnly`, `Allocates`, `Mutates`, `Io`, `Aborts`, `Concurrent`, `Unknown` —
read by the optimizer to gate CSE / DCE / hoisting via `may_mutate()` (`:150`).
`returns_fresh` (`:163`) is the *sharper* signal: true iff the call ALWAYS
returns an independently heap-allocated buffer that aliases no input (distinct
from the coarse `Allocates`, which also covers view-returners that build `cap=0`
views). The CoW machinery in IR lowering reads `returns_fresh` to skip the
self-referential-reassignment clone guard and the return-clone-elision check.
The two constructors `sig` and `sig_fresh` (`:183`, `:193`) differ only in that
flag.

A module pass, `promote_runtime_calls` (`src/lir/runtime.rs:675`), walks every
`Inst::CallExtern` and rewrites it to `Inst::CallRuntime` when the name matches a
`RuntimeFn`. It runs after collection-bridge wiring and before BIR lowering
(which rewrites `CallRuntime` back to `CallExtern` for backend uniformity); the
window between is where the typed form is visible to the validator and
optimizer. The pass is idempotent and also resolves the strip-family arity
overload here (`s.strip()` → `StrTrim`, `s.strip(chars)` → `StrStrip`) so the
chosen variant's signature matches the call shape (`:688`).

### `AbiKind` is read, not guessed, at the C-emit boundary

Each param's `AbiKind` survives into the `CallExtern`/`CallRuntime`'s `arg_abis`
and drives how the C backend marshals the argument. The dispatch in
`src/backend/c_lir/emit_types.rs` is a `match` on the tag, not a name heuristic:
`GorgetString` derefs a pointer to `*(Str*)val` or passes a struct through
(`:751`); `Ptr` takes the address of a struct (`&val`, `:777`); `VoidElem`
wraps a scalar element as `&(Type){val}` for memcpy (`:796`); `Opaque`/`Scalar`
pass through (`:792`). The backend keeps a whitelist of by-ref runtime fns only
as a safety net for the case where `arg_abis` is empty — tagged calls use
`arg_abis` directly (`src/backend/c_lir/helpers.rs:679`, `:747`).

This is the layering-discipline payoff (Chapter on layering / the project's
"No name matching" rule): the ABI marshalling decision is a typed field set once
at the source (`RuntimeFn::resolve_lir_sig`) and read by the consumer — not a
substring test on the symbol name at emit time.

### The resource table — data-driven, in Gorget

The *type*-side companion to `RuntimeFn` is a declarative table written in
idiomatic Gorget at `compiler/data/resources.gg`, baked into the binary via
`include_str!` (`src/compiler_data.rs:14`). At first use the loader parses it
*with the compiler's own parser*, walks the literal-only AST, and produces a
typed `ResourceTable` (`src/resources.rs:38`–`73`); subsequent calls are O(1)
through a `OnceLock`. A `GORGET_RESOURCES_PATH` env var overrides the embedded
copy so the table can be edited without recompiling Rust
(`src/resources.rs:44`).

The schema is declared in `compiler/data/schema.gg` and mirrored, by hand, into
typed Rust structs in `src/resource_schema.rs`. The two are kept in lockstep
by a `SCHEMA_VERSION` integer that the loader asserts against
`SCHEMA_VERSION_EXPECTED` (`src/resources.rs:28`, `:65`) — a field-shape change
that forgets to bump the version fails the build loudly rather than corrupting
silently. The Rust mirror exists only so Rust consumers get typed values; it is
documented to retire when self-host replaces Rust as the canonical compiler
(`src/resource_schema.rs:8`).

Each `ResourceEntry` (`src/resource_schema.rs:79`) is a list of `MatchKind`
keys (`Exact` / `Prefix`) plus a `ResourceMetadata` payload. `lookup(name)`
returns the first entry whose `match_on` list matches, in declaration order
(`src/resource_schema.rs:144`). The metadata is the type-axis truth:
`runtime_name`, `size_bytes`, `drop_fn`/`clone_fn`/`materialize_fn`,
`copy_semantics`, `collection_kind`, `box_kind`, `opaque_handle`,
`method_prefix`, `c_typedef_name`, `is_typed_constructor`. For example
`GorgetString` carries `size_bytes = 32`, `CsResource`, materialize_fn
`gorget_string_materialize_inplace`, and `method_prefix = "gorget_str"`
(`compiler/data/resources.gg:64`–`73`); `GorgetArray` is `size_bytes = 64`
(`:79`).

The classic consumer is method-name routing. `map_monomorphized_to_runtime` in
`src/lir/lower/calls.rs:270` does **not** prefix-match the symbol to decide the
family — it reads the typed `method_prefix` field from the table:

```rust
// src/lir/lower/calls.rs:280
let family = crate::ir::resources::table().lookup(name)
    .and_then(|m| m.method_prefix.as_deref());
```

The within-arm dispatch (constructor detection, higher-order inline returns) then
keys off the method name, but "which family?" — the part that used to be a
`starts_with("Vector__")` — is now a typed lookup.

## Future direction: `RUNTIME_DECLS` / a single canonical source file

The former `unified-resource-model.md` §3.6 (now folded into this chapter)
sketched a `RUNTIME_DECLS` const
generated, along with the C runtime header and a self-host Gorget form, from one
canonical `resources.toml` at `build.rs` time — so the C backend's `extern`s,
an LLVM backend's `declare`s, and a future WASM backend's `(import …)`s, plus the
hand-written C header, can never disagree on a signature. **This pipeline is
unshipped** — `RUNTIME_DECLS`, `resources.toml`, and the `build.rs` emitters do
not exist in the tree: there is no `build.rs`, no `RUNTIME_DECLS` const in `src/`,
and no `resources.toml` file. (`RUNTIME_DECLS` appears only in docs; the string
`resources.toml` appears as a plan-name reference in a few source comments —
`src/compiler_data.rs:11`, `src/resources.rs:428`,
`compiler/data/resources.gg:13` — but no such file is ever read or written.)

What ships *today* is strictly the better half of that idea, just split across
two homes and one rung higher than the doc proposed: the `RuntimeFn`
enum-as-index registry (function signatures, generated by `runtime_table!`) and
the `compiler/data/resources.gg` data file (resource-type metadata, loaded into
`ResourceTable`). The doc's flat `RUNTIME_DECLS` slice would be a regression from
the macro's ordinal-as-index guarantee for the function side. The remaining open
work — tracked as a `TODO.md` roadmap item, not a gap in the shipped code — is to
unify both axes under one hand-edited canonical source that *also* generates the
C header and the self-host form, closing the latent "frontend/runtime/self-host
signature drift" bug class and de-duplicating the hand-written Rust mirror in
`src/resource_schema.rs`. The design lived in the former
`unified-resource-model.md` §3.6 / §9.2 / §13, now folded into this chapter.

> Note: the citation in `src/lir/runtime.rs:6` formerly pointed at a
> `lir-correctness-roadmap.md` that never existed — its content was
> superseded by the `unified-resource-model.md` deep-dive. The `RuntimeFn`-enum
> design that comment describes is documented in this chapter, where the
> citation now points.

## In the self-host

n/a — the self-host frontend (`tests/fixtures/self_host_*`) covers the lexer,
parser, resolver, type checker, and GIR lowerer, but not the C runtime or the
backend ABI; there is no self-host backend, so this subsystem has no self-host
counterpart to compare against today. The `compiler/data/schema.gg` /
`resources.gg` files are written so the self-host *could* consume them directly
(`from compiler.data.schema import …`, `compiler/data/schema.gg:5`), which is the
long-term plan in §9.2 of the unified-resource-model doc — but that consumption
is not wired up. The Rust mirror at `src/resource_schema.rs` is the only live
reader.
