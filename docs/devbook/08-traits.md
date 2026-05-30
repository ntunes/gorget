# 08 — Traits & the impl registry (Pass 3)

The trait registry is the single source of truth for "which types implement
which traits, and what does each trait method's signature look like". It is
built once, after name resolution and before type checking, by
`build_registry` in `src/semantic/traits.rs:436`. Everything downstream —
method-call resolution in the type checker, vtable construction in IR
lowering, `@derive` validation — reads `TraitRegistry`; nothing rebuilds it.

This chapter covers the registry's data shape, how it is populated across
its four sub-passes (built-ins → traits → impls → validation), how default
methods / inheritance / `via` delegation / `@derive` feed into it, and where
the self-host typechecker mirrors the same structure.

## Where it sits in the pipeline

`analyze` (`src/semantic/mod.rs:90`) is a one-line delegate to
`analyze_with_source_dir` (signature `:96-101`, body `:102+`), which runs the
passes in order. The trait-relevant ones:

- **`@derive` expansion** (`src/semantic/mod.rs:113`) — *before* Pass 1.
  `derive::expand_derives` synthesizes `equip` blocks as AST so the rest of
  the pipeline sees them as ordinary user impls (see [§@derive-generated
  impls](#derive-generated-impls)).
- **Pass 3 — `build_registry`** (`src/semantic/mod.rs:271`). The subject of
  this chapter.
- **Pass 3.5 — `validate_derive_field_traits`** (`src/semantic/mod.rs:276`)
  — runs *after* the registry exists, because it needs to ask the registry
  whether a field type implements a derived trait.
- **Pass 4 — type check** (`src/semantic/mod.rs:290`) reads the registry via
  `resolve_method` / `resolve_method_by_name` / the `*_shape` lookups.

The registry is stored on `AnalysisResult.traits`
(`src/semantic/mod.rs:46`) and threaded into the type checker as
`&'a TraitRegistry` (`src/semantic/typecheck.rs:389`).

## The registry data structures

`TraitRegistry` (`src/semantic/traits.rs:116`) holds two owning collections
and four lookup indices:

- `traits: FxHashMap<DefId, TraitInfo>` — every trait, built-in and
  user-defined.
- `impls: Vec<EquipInfo>` — every `equip` block, inherent and trait. This
  `Vec` is the **source of truth**; everything else indexes into it by
  position (`usize`).
- `inherent_impls: FxHashMap<TypeId, Vec<usize>>` — inherent `equip T:`
  blocks, by self type.
- `trait_impls_by_type: FxHashMap<TypeId, Vec<usize>>` — trait `equip T with
  Tr:` blocks, by self type. Lets `resolve_method` visit only the impls
  whose self type matches the receiver instead of scanning all impls
  (`src/semantic/traits.rs:120-126`).
- `impls_by_name: FxHashMap<String, Vec<usize>>` — same shape, keyed by the
  impl's *self-type name string*. Drives the name-based fallback paths used
  for cross-module equips where `TypeId`s don't line up, and for `meta if
  implements(...)` (`src/semantic/traits.rs:127-137`). The header comment is
  explicit that this is not a new name-matching axis — `self_type_name` is
  already name-keyed at the source, so this only indexes existing data.
- `trait_impls: FxHashMap<(DefId, TypeId, Vec<TypeId>), usize>` — keyed by
  `(trait, self type, resolved trait generic args)` so that multiple impls
  of the same parameterized trait coexist, e.g. `From[int]` and `From[str]`
  on one type (`src/semantic/traits.rs:138-141`). Also the duplicate-impl
  detector.

### TraitInfo

`TraitInfo` (`src/semantic/traits.rs:66`) carries:

- `methods: FxHashMap<String, FunctionSig>` — resolved signatures.
- `has_default_body: FxHashMap<String, bool>` — does this method have a body
  in the trait declaration (a default), or is it just a signature?
- `extends: Vec<DefId>` — supertrait DefIds (see
  [§Inheritance](#trait-inheritance)).
- `trait_generic_params: Vec<String>` — the trait's own generic names, e.g.
  `["T"]` for `trait Iterator[T]` (`src/semantic/traits.rs:892`).
- `method_shapes` and `default_method_sigs` — AST-level shapes preserved for
  call-site generic inference; see
  [§Why two signature representations](#why-two-signature-representations).

### EquipInfo

`EquipInfo` (`src/semantic/traits.rs:89`) describes one `equip` block:

- `self_type: TypeId` / `self_type_name: String` — the implementing type,
  kept in both keyed forms.
- `trait_: Option<DefId>` / `trait_name: Option<String>` — `None` for an
  inherent `equip T:`.
- `methods: FxHashMap<String, (DefId, FunctionSig)>` — the methods the block
  actually defines (the `DefId` is the function's own def, used by IR
  lowering for mangled-name dispatch).
- `trait_generic_args: Vec<Type>` — the AST type args off the trait, e.g.
  `[int]` for `with Iterator[int]` (`src/semantic/traits.rs:1076`).
- `via_field: Option<String>` — the delegation field (see [§via
  delegation](#via-delegation)).
- `self_type_ast: Type` + `impl_generic_params: Vec<String>` — the AST self
  type and the impl's own generics (e.g. `["T"]` for `equip [T]
  VectorIter[T]`), used to bind impl locals to the receiver's concrete args
  at a call site (`src/semantic/traits.rs:1086-1094`).

### FunctionSig

`FunctionSig` (`src/semantic/traits.rs:14`) is the resolved-TypeId view:
`params: Vec<TypeId>`, `return_type: TypeId`, `has_self: bool`,
`self_ownership: Option<Ownership>`. It is built by `build_function_sig`
(`src/semantic/traits.rs:1629`), which resolves every AST type via
`types::ast_type_to_resolved`, dropping the `self` param into `has_self` /
`self_ownership` and wrapping the return type in `Future[T]` for `async`
methods (`src/semantic/traits.rs:1640`).

## Why two signature representations

`FunctionSig` resolves types eagerly, which is lossy for generics: a
method-level generic param's def isn't in scope at registration time, so it
erases to `error_id` (the comment at `src/semantic/traits.rs:21-32`
documents this). `Self` likewise erases to `error_id`, and a trait's own `T`
resolves to a placeholder. That's fine for *validation* but useless for
*call-site inference*, which needs to know which named generic sat in which
slot.

So the registry keeps two parallel AST-level shapes:

- **`MethodSigShape`** (`src/semantic/traits.rs:34`) — for any method with a
  non-empty `generic_params` (method-level generics like `any[F]`). Built by
  `build_method_sig_shape` (`src/semantic/traits.rs:1528`), which returns
  `None` for non-generic methods to keep the registry small. Consumed by
  `resolve_method_shape` / `resolve_method_shape_by_name`
  (`src/semantic/traits.rs:211`, `:247`) during the type checker's
  method-generic inference (`src/semantic/typecheck.rs:1784`).
- **`DefaultMethodSig`** (`src/semantic/traits.rs:50`) — for any trait method
  with a body (a default). Captures param types, ownerships, return type,
  and `Self`/method-generic names at AST level so the type checker can
  substitute `Self` and trait-`T` against a concrete receiver when a call
  falls through to a trait default
  (`substitute_default_method_sig`, `src/semantic/typecheck.rs:4032`).

The substitution kernel is `substitute_ast_type`
(`src/semantic/traits.rs:1557`): a recursive walk that replaces every
bare `Type::Named` whose name is a binding key, recursing through arrays,
slices, tuples, function types, refs/owned/pointers. Notably it treats
`Type::SelfType` as a *bindable* placeholder when the bindings contain a
`"Self"` entry (`src/semantic/traits.rs:1612-1623`) — needed for default
sigs like `TakeIter[Self, T] take(self, int n)` on `Iterator[T]`.

This is rule 2 of [layering discipline](24-layering-discipline.md#rule-2--typed-metadata-not-name-matched)
in microcosm: the registry doesn't reconstruct generic structure from names
downstream — it carries the typed AST shape forward and substitutes once.

## Built-in traits

`register_builtin_traits` (`src/semantic/traits.rs:505`) runs first so
user impls can `equip T with Equatable:` without a `trait` declaration. It
hand-builds a `FunctionSig` for each core trait's methods. The candidate set
is the `builtin_traits` vec (`src/semantic/traits.rs:510-821`): `Displayable`,
`Debuggable`, `Equatable`, `Cloneable`, `Hashable`, `Hasher`, `Drop`,
`Iterable`, the arithmetic operator traits (`Add`/`Sub`/`Mul`/`Div`/`Rem`/
`Mod`/`Neg`), `Comparable`, `Index`/`IndexMut`, `Default`, `From`/`TryFrom`,
`Measurable`, `Parseable`, `One`, and the composite `Numeric`.

Crucially, **listing a trait in the vec does not register it.** Each entry is
installed only if the resolver pre-defined the name — the loop at
`src/semantic/traits.rs:824` is gated on `scopes.lookup(name)`, and the
placeholder defs come from the resolver's built-in trait list
(`src/semantic/resolve.rs:127`). A vec entry whose name the resolver never
reserves is silently skipped. `Self`/generic param slots are stored as
`error_id` (e.g. `Cloneable::clone` returns `error_id`,
`src/semantic/traits.rs:549`), and validation later treats `error_id` as a
wildcard.

**`Ordinal` is a dead entry.** The vec carries an `("Ordinal", …)` entry
with the documented `int ordinal(self)` signature
(`src/semantic/traits.rs:603`), **but it is never registered**: `Ordinal` is
absent from the resolver's reservation list (`src/semantic/resolve.rs:127`)
and no library declares `trait Ordinal`, so `scopes.lookup("Ordinal")` at
`:824` returns `None` and the entry is dropped. The consequence is a latent
validation gap: `equip X with Ordinal:` (and `@derive(Ordinal)`, which
desugars to one) finds no trait def, falls through to the inherent-impl path
(`trait_def_id = None`), and its `ordinal` signature is **never checked**
against the `int ordinal(self)` contract — a wrong-signature
`String ordinal(self)` equip compiles clean, whereas the same trick on a
genuinely-registered built-in like `Comparable` is correctly rejected with
`MethodSignatureMismatch`. The `ordinal` method itself still works as an
inherent method; only the trait-contract enforcement is missing. (Tracked in
`TODO.md`.)

Two deliberate non-built-ins:

- **`Iterator[T]` is user-space**, declared in `lib/std/iter.gg`, so its
  default-method bodies (`count`/`map`/`filter`/`fold`/…) ride the ordinary
  `collect_trait` path and become real `DefaultMethodSig`s. The resolver
  reserves the name as a placeholder so equip blocks parse before
  `iter.gg` loads (`src/semantic/traits.rs:624-628`,
  `src/semantic/resolve.rs:127`).
- **`Numeric` has an empty method map**; it is purely a composite. Its
  `extends` is wired by hand to the ten operator/identity parents
  (`src/semantic/traits.rs:842-851`).

### Intrinsic satisfaction

Primitives satisfy certain traits without any `equip`. `has_trait_impl_by_name`
(`src/semantic/traits.rs:331`) short-circuits: numeric primitives satisfy the
numeric traits (`is_numeric_primitive`/`is_numeric_trait`,
`src/semantic/traits.rs:402`,`:411`); hashable primitives satisfy
`Hashable`/`Equatable` (`:420`,`:425`); all primitives + `String` are
`Cloneable`/`Displayable`/`Debuggable` (`:431`, line `:350`). This is why
you can `@derive(Hashable)` a struct of `int`s with no further work.

## Building the registry

`build_registry` (`src/semantic/traits.rs:436`) is four sub-passes:

1. `register_builtin_traits` (above).
2. `collect_traits_from_items` (`:450`) → `collect_trait` (`:854`) for every
   `Item::Trait`, recursing into `Item::Module` wrappers so imported-module
   traits land in the registry too.
3. `process_impls_from_items` (`:453`) → `process_impl` (`:958`) for every
   `Item::Equip`, same recursion.
4. `validate_trait_cycles` (`:456`) then `validate_trait_impls` (`:459`).

### collect_trait

For each `TraitItem::Method` (`src/semantic/traits.rs:870`): validate
default-param ordering, build the `FunctionSig`, record whether it has a
body (anything not `Declaration`/`Extern` is a default,
`src/semantic/traits.rs:874`), and populate `method_shapes` /
`default_method_sigs` as applicable. Then it extracts
`trait_generic_params` and resolves `extends` DefIds via `scopes.lookup`.

### process_impl

`process_impl` (`src/semantic/traits.rs:958`) is where most validation
*gating* lives — note it returns early on each fatal condition, so a bad
equip never makes it into the indices:

- **Trait lookup uses the type namespace** (`scopes.lookup_type`,
  `:984`) so `equip IoError with Error:` finds `trait Error`, not the
  `Result.Error` value-namespace variant of the same name.
- **`via` without a trait** is `ViaWithoutTrait`
  (`src/semantic/traits.rs:997`).
- **Duplicate impl**: `(trait, self type, resolved trait args)` already in
  `trait_impls` → `DuplicateImpl` (`:1020`). The trait-arg tuple is what lets
  `From[int]` and `From[str]` coexist.
- **Orphan rule** (`:1035`): when a trait is named, at least one of (trait,
  type) must be *local* — a real span and not a `DefKind::Import`
  (`:1039`). Built-ins have `Span::dummy()` and count as foreign, so
  `equip int with Displayable:` is an `OrphanImpl`, but a local type with a
  built-in trait, or a local trait with a foreign type, is fine (tests at
  `src/semantic/traits.rs:2080-2156`).

It then collects the block's methods (with their `DefId`s), the
`trait_generic_args`, and `impl_generic_params`, pushes the `EquipInfo` into
`impls`, and updates the indices. The split for inherent vs trait impls is at
`src/semantic/traits.rs:1119`:

- **Trait impl** → insert into `trait_impls` and `trait_impls_by_type`.
- **Inherent impl** → multiple `equip T:` blocks are allowed and just
  accumulate methods, *but* two blocks declaring the *same* method name is a
  `DuplicateImpl` with a `(inherent method ...)` marker
  (`src/semantic/traits.rs:1131-1146`) — matching Rust's rule.

## Impl validation

`validate_trait_impls` (`src/semantic/traits.rs:1157`) checks, for every
trait impl:

1. **`via` field validity** (`validate_via_field`, `:1369`).
2. **All required methods present.** `collect_all_required_methods`
   (`:1488`) walks `extends` recursively (with a visited-set cycle guard,
   `:1501`) and returns `(method_name, has_default, source_trait_name)`. A
   method that's missing *and* has no default → `MissingTraitMethod`
   (`:1204`) — **unless** `via` is active (auto-forwarded, `:1186`) or it's
   *satisfied elsewhere*: a sibling equip block on the same self-type name
   that provides the inherited method (split-equip satisfaction,
   `src/semantic/traits.rs:1196-1203`).
3. **Signature shape match** for present methods → `MethodSignatureMismatch`:
   return type (`:1232`), param count (`:1254`), per-param types (`:1268`),
   `has_self` (`:1310`), and `self_ownership` (`:1328`).

Two escape hatches make validation tolerant of the registry's lossiness:

- **`error_id` is a wildcard.** Any trait param or return that *is* or
  *contains* `error_id` is skipped — that slot was a `Self`/generic
  placeholder. `type_contains_error` (`:1352`) recurses through
  `Generic`/`Tuple` args, so `Option[<error>]` from a generic trait sig
  doesn't spuriously clash with the impl's concrete `Option[int]`.
- **Display-form fallback.** The trait registry interns types separately
  from the impl builder, so legitimately-equal types (notably trait objects
  rendered `<trait object>`) can differ at the `TypeId` level. Before
  flagging a mismatch, both sides are compared by `types.display(...)`
  (`:1288-1291`, return type at `:1238`).
- **`self_ownership: None` is a wildcard** — built-in traits that don't pin
  an ownership mode skip the check (`:1328`).

### Cycle detection

`validate_trait_cycles` (`src/semantic/traits.rs:1433`) runs a DFS over the
`extends` graph (`dfs_detect_cycle`, `:1445`) emitting `TraitCycle` with the
`A → B → A` path. It runs *before* `validate_trait_impls` specifically to
avoid the unbounded recursion that `collect_all_required_methods` would hit
on a cyclic graph (`src/semantic/traits.rs:455`); the latter still carries
its own visited-set guard as defense in depth.

## Method resolution

`resolve_method` (`src/semantic/traits.rs:157`) is the hot path used by the
type checker. Order: **inherent impls first**, then trait impls, then trait
defaults. The trait-impl scan is a single pass over `trait_impls_by_type[T]`
(`:175`): an impl-supplied override wins immediately; otherwise it remembers
the first trait that has a *default body* for the method and returns that
trait's sig on fallthrough (`:182-201`). `resolve_method_shape` (`:211`)
mirrors this for `MethodSigShape`, and `resolve_method_by_name` (`:273`) is
the name-keyed fallback for cross-module receivers. The type checker calls
all of these from its `MethodCall` arm (`src/semantic/typecheck.rs:1784`,
`:1813`, `:1871`).

## Trait inheritance

Supertraits are stored as `TraitInfo.extends: Vec<DefId>`, populated in
`collect_trait` from `trait_def.extends` (`src/semantic/traits.rs:899-905`)
and, for `Numeric`, hand-wired (`:842`). Inheritance affects three things:

- **Required-method collection** flattens parents
  (`collect_all_required_methods`), so `equip Foo with Child:` must satisfy
  `Base`'s non-default methods too (test `src/semantic/traits.rs:1943`).
- **`trait_satisfies`** (`:370`) answers "does holding `Child` satisfy a
  `Base` bound?" by checking same-name or a one-level `extends` membership.
- **Cycle detection** walks the same edges.

## via delegation

`equip Outer with Tr via field:` auto-forwards every un-overridden `Tr`
method through `Outer.field`. The registry side: `via_field` is recorded on
`EquipInfo` (`src/semantic/traits.rs:996`), `MissingTraitMethod` is
suppressed for via impls (`:1186`), and `validate_via_field`
(`:1369`) checks the named field exists on the struct and that its type
implements the target trait — emitting `ViaFieldNotFound` (`:1394`) or
`ViaFieldTypeMissingTrait` (`:1412`) otherwise. (Non-struct self types skip
field validation, `:1386`.)

The actual forwarding is *not* in the semantic layer — it's synthesized at
IR lowering. `src/ir/lowering/traits.rs:552` detects `equip.via_field` and
calls `emit_via_forwarding_function` (`:726`), which generates a vtable
thunk `Tr_for_Outer__method` that casts `self`, takes the address of the
field, and tail-calls `Tr_for_Inner__method` (the worked C shape is in the
doc comment at `src/ir/lowering/traits.rs:716-724`). This is a clean
layering split: the semantic pass *validates and records intent*; lowering
*realizes* it. The self-host has no backend, so it covers the registry side
of `via` (recording `via_field`) but not the forwarding codegen.

## @derive-generated impls

`@derive(...)` is desugaring, not a registry concept: `derive::expand_derives`
(`src/semantic/derive.rs:54`) runs before name resolution and *generates
Gorget source* for an `equip` block per derived trait, parses it, and appends
the resulting AST items to the module (`parse_and_collect_derived_items`,
called from `:143`/`:241`). By Pass 3 those equips are indistinguishable from
hand-written ones — `process_impl` ingests them normally.

- Struct-derivable traits: `DERIVABLE_STRUCT_TRAITS`
  (`src/semantic/derive.rs:85`) — `Equatable`, `Displayable`, `Debuggable`,
  `Cloneable`, `Hashable`, `Serializable`, `Default`, `Deserializable`,
  `From`, `TryFrom`, `FromRow`. Enum-derivable: `DERIVABLE_ENUM_TRAITS`
  (`:87`) — same minus the field-shaped ones, plus `Ordinal`.
- A non-derivable trait → `UnderivableTrait` (`:101`); `@derive(From)` /
  `TryFrom` on a multi-field struct → `DeriveFromRequiresSingleField`
  (`:111`).
- A per-trait dispatcher `generate_struct_derive`
  (`src/semantic/derive.rs:307`) routes to one generator each
  (`generate_struct_equatable` at `:329`, etc.), all emitting idiomatic
  Gorget; e.g. `Hashable` generates `void hash[Hasher H](self, H &h):` — kept
  generic over the `Hasher`, not concretized to `FxHasher`
  (`src/semantic/derive.rs:414`).

**Field-trait validation runs after the registry exists.**
`collect_struct_derives` records a `DeriveRecord` per derive (`:135`), and
Pass 3.5 `validate_derive_field_traits` (`src/semantic/derive.rs:171`)
checks — for the field-requiring traits `Hashable`/`Equatable`/`Cloneable`
(`FIELD_REQUIRING_TRAITS`, `:148`) — that every field type satisfies the
derived trait, via `primitive_satisfies` (`:151`), the registry's
`has_trait_impl_by_name`, or another field's own derive record (cross-module
case, `:201`). Failure → `FieldMissingDerivedTrait` (`:208`). It must run
after Pass 3 precisely because it asks the live registry, illustrating why
`@derive`'s two halves straddle the registry build.

## In the self-host

The self-host typechecker reimplements the registry in
`tests/fixtures/self_host_typechecker/traits.gg`, with the data types
(`TraitInfo`, `EquipInfo`, `FunctionSig`, `MethodSigShape`, `TraitRegistry`,
`MethodResolution`) defined in `types.gg` and imported at
`traits.gg:25-26`. The structure tracks the Rust side closely:

- `build_trait_registry` (`traits.gg:482`) is the analog of `build_registry`:
  collect user traits, process equips, then `register_builtin_traits`
  (`traits.gg:387`) — with the explicit note that user TraitInfos collected
  first "win on the same DefId" (`traits.gg:501`). `register_builtin_method`
  (`traits.gg:333`) installs each core trait method.
- The built-in set and intrinsic-satisfaction logic mirror Rust's
  (`traits.gg:399-446`).
- `resolve_method_full` (`traits.gg:632`) / `resolve_method_by_name`
  (`traits.gg:768`), consumed at `typecheck.gg:705`,`:717`, return a
  `MethodResolution`. The default-method substitution that Rust does in
  `typecheck.rs` lives in `substitute_default_return` (`traits.gg:514`) and
  the generic-impl variant `substitute_default_return_generic`
  (`traits.gg:549`), both binding `Self → impl.self_type` and trait-`T` →
  resolved trait args.
- The `IEquip` arm of `type_check_function` (`typecheck.gg:1204`) handles the
  trait-bounded vs target-implicit equip generic-param distinction via
  `trait_equip_scope_id` (`typecheck.gg:1226-1231`), the same asymmetry the
  Rust type checker applies.

Two structural divergences:

- One `Dict` per index rather than Rust's mix of maps; `resolve_method_full`
  takes the inherent and trait impls from disjoint Dicts
  (`traits.gg:266`,`:42`).
- **No backend**, so `via` *forwarding codegen* and the vtable thunks have no
  self-host counterpart — only the registry-side recording of `via_field`
  exists. `@derive` is mirrored in `derive.gg`.

**Parity is diagnostic, not asserted.** To read the current trait-area
parity, run the comparison test and read its printed matched-count — the
`*_comparison` tests always pass regardless:

```bash
cargo test --test integration type_comparison -- --nocapture
cargo test --test integration check_comparison -- --nocapture
```

(`type_comparison` at `tests/integration.rs:12997`, `check_comparison` at
`:13193`.) Trait/equip handling feeds the type-checker output both compare,
so a regression in the registry surfaces as a drop in their matched counts —
never trust a remembered figure; re-derive it from the `--nocapture` output.
