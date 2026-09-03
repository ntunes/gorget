# 7. Name resolution & scopes (Pass 1–2)

Name resolution turns the AST's bare identifier *strings* into stable
`DefId`s — answering "which declaration does this use of `foo` refer to?".
It lives almost entirely in `src/semantic/resolve.rs` (the two passes and
the AST walk) and `src/semantic/scope.rs` (the scope tree and the lookup
machinery). It runs as Pass 1 and Pass 2 of semantic analysis, after meta
and derive expansion and before trait-registry building and type checking
(see the pass order in `src/semantic/mod.rs:234-251`). Its output is the
**resolution map** — `FxHashMap<usize, DefId>` keyed by name-use span
start (`resolve.rs:32`) — plus a populated `ScopeTable` (the definition
table) and a handful of side tables that downstream passes consume.

> *Verified against the source as of the commit this chapter was written
> against. Re-derive line numbers if they've drifted.*

## What "resolution" does and does not do here

This pass is deliberately shallow. It binds **value- and type-namespace
names to DefIds and reports undefined names**; it does *not* assign types,
resolve methods, or resolve generic type arguments. Concretely:

- Method names are **not** resolved here — the `MethodCall` arm resolves
  only the receiver and arg expressions and leaves the method to the
  typechecker (`resolve.rs:1450-1458`, `// Method name is resolved during
  type checking`).
- **Call-site type arguments are not resolved.** `Expr::Call` carries a
  `generic_args: Option<Vec<Spanned<Type>>>` field (`ast.rs:523-527`), but
  the resolver's `Call` arm destructures the node with `..` and only walks
  `callee` and `args` (`resolve.rs:1418-1448`). The same holds for
  `MethodCall`'s `generic_args`. This is intentional: putting a resolvable
  name inside `targs` and expecting the resolver to bind it will silently
  do nothing (a recurring footgun — see the self-host notes below).
- Dot-shorthand (`.Variant(...)`) and the bare-variant constructor path
  defer enum-name resolution to type-check time, where the expected type
  disambiguates (`resolve.rs:1677-1682`).

DefId assignment is monotonic: `define`/`alloc_def` allocate
`DefId(self.definitions.len() as u32)` (a `DefId` wraps a `u32`,
`ids.rs:3`) and push a `DefInfo` (`scope.rs:193-210`, `270-298`). DefIds
are never reused, so a DefId is a stable handle into
`ScopeTable::definitions` for the rest of compilation.

## The scope tree and the two namespaces

A `Scope` (`scope.rs:60-65`) holds a `parent` link, a `ScopeKind`, and —
crucially — **two disjoint name maps**: `types: FxHashMap<String, DefId>`
and `values: FxHashMap<String, DefId>`. Gorget keeps separate type and
value namespaces so the same spelling can mean different things in
different positions. The canonical case is `Error`: a user `trait Error`
lives in the type namespace while the `Result.Error` variant constructor
lives in the value namespace, and both coexist (`scope.rs:49-59`,
duplicate check at `scope.rs:220-231`).

`def_namespace(kind)` (`scope.rs:70-92`) classifies each `DefKind`:

- **Type-only**: `Trait`, `TypeAlias`, `GenericParam`.
- **Value-only**: `Function`, `Variant`, `Variable`, `Const`, `Static`.
- **Both**: `Struct`, `Enum`, `Newtype` (dual-role — a struct name is both
  a type `Vector[int] v` and a constructor/path-head `Vector[int]()`), and
  `Import` (ambiguous until the source is known).

`ScopeKind` (`scope.rs:101-113`) distinguishes `Module`, `FileModule {
path }` (per imported module wrapper), `Function`, `Block`, `EquipBlock {
self_type }`, `TraitDef`, and `ForLoop`. The kind matters downstream:
`enclosing_function_scope` and `is_in_loop` walk the chain looking for
specific kinds (`scope.rs:459-482`).

### Lookup and the parent walk

`lookup(name)` walks the current scope up the parent chain, checking the
**value namespace first, then the type namespace** (`scope.rs:311-313`).
Callers that care which meaning they want use `lookup_value` /
`lookup_type` (`scope.rs:328-339`, `316-326`); the generic `lookup` is for
ambiguous positions. In practice the namespace-specific lookups have a
single consumer in the whole pipeline: the trait-registry builder
(`traits.rs:981`, `scopes.lookup_type(&name.node)`). The resolver itself
uses only the generic `lookup`/`lookup_from_scope` (no `lookup_type` /
`lookup_value` calls in `resolve.rs`), and the typechecker uses neither —
the value-first fallback in `lookup` covers their needs.

### The name index (performance)

Every `define`/`alloc_def` also pushes the new DefId onto a reverse index
`name_index: FxHashMap<String, Vec<DefId>>` (`scope.rs:120-128`,
maintained at `scope.rs:195`, `271`). This turns the O(N_defs) linear
scans in `lookup_within_function`, `is_global_def`, `lookup_def_by_span`,
and `is_known_variant_name` into O(K) lookups over the (typically 1–5)
defs sharing a name. The comment at `scope.rs:120-127` records why: at
self-host-lowerer scale the safety pass calls these thousands of times
across ~10K defs, and the old linear scan was quadratic-in-module-size and
dominated the semantic phase.

`lookup_within_function` (`scope.rs:380-395`) is a general within-function
name lookup: it searches a function's whole scope subtree (plus
ancestors), returning the highest matching DefId among
`Variable`/`Const`/`Function` defs that are descendants of the function
scope, falling back to the ancestor walk for module-scope names. The
resolver does not call it; its consumers are downstream passes that need
to resolve a name relative to the function currently being checked — the
safety pass's `find_def_by_name` (`safety/helpers.rs:350-356`) and the
typechecker's `resolve_name` fallback and f-string interpolation lookup
(`typecheck.rs:551`, `typecheck.rs:1052`).

## Pass 1 — top-level collection (`collect_top_level`)

`collect_top_level` (`resolve.rs:118-302`) seeds the module scope and
collects every top-level definition into it, so that **forward references
resolve** (a function can call one declared later — the
`forward_reference` test at `resolve.rs:2051-2061`).

### Prelude seeding

Before walking user items, the pass registers the built-ins
(`resolve.rs:125-171`):

- Core traits (`Displayable`, `Cloneable`, `Hashable`, `Iterator`, the
  operator traits, …) as `DefKind::Trait` with dummy spans.
- The `String` constructor as a `Function`.
- Collection / concurrency types (`Vector`, `Dict`, `Box`, `Task`,
  `Channel`, `Mutex`, …) as `DefKind::Import` *placeholders* — real
  definitions from `std.collections` replace these when imported.
- `Option`/`Result` enums with their variants (`Some`/`None`,
  `Ok`/`Error`), recording an `EnumVariantInfo` per enum.
- Built-in generic trait bounds for `Dict`/`HashMap` (K: Hashable +
  Equatable) and `Set`/`HashSet` (T: Hashable + Equatable), into
  `struct_generic_bounds`.

All of these use `Span::dummy()`, which matters for the replacement rule
below.

### `collect_item` — one match arm per item kind

`collect_item` (`resolve.rs:335-761`) defines the item's name and stashes
the side-table info downstream passes need. The interesting cases:

- **Function** (`resolve.rs:343-418`): defines the name, eagerly resolves
  the return type, param types, and `throws` type via
  `types::ast_type_to_resolved`, validates default-param ordering, and
  records a `FunctionInfo` (the big struct at `resolve.rs:35-73`). Async
  functions get their return type wrapped to `Future[T]`
  (`resolve.rs:355-363`). The `scope_id` recorded here is the *module*
  scope; Pass 2 overwrites it with the real body scope.
- **Struct** (`resolve.rs:420-438`): records field `(name, span)` pairs and
  any generic bounds.
- **Enum** (`resolve.rs:440-499`): the subtle one. **Non-generic** enum
  variants are allocated with `alloc_def` — given a DefId and recorded in
  `name_index` but **not inserted into any scope's value namespace**
  (`resolve.rs:461-468`). They are reachable only via qualified paths
  (`Color.Red()`). **Generic** enum variants stay in scope as bare names
  (`resolve.rs:451-460`) because there's no parseable qualified syntax for
  them (`Maybe[int].Just(42)` doesn't parse). The collect tests assert
  exactly this (`resolve.rs:2014-2025`).
- **Equip block** (`resolve.rs:536-614`): pushes a temporary `EquipBlock`
  scope just to give each method a unique DefId and a `FunctionInfo`
  (needed by the borrow checker), then pops it. The bodies are resolved in
  Pass 2.
- **ExternBlock** (`resolve.rs:616-676`): like functions but
  `has_body: false` and `return_origin_is_static: true`.
- **Module** (`resolve.rs:691-759`): pushes a `FileModule` scope, computes
  the set of explicitly-`private` names, collects every item (public and
  private) into the module scope, then **promotes non-private names to the
  parent** via `export_non_private`. This makes public items visible
  globally while keeping private ones module-local.
- **Meta items** (`resolve.rs:686-689`) are skipped — they're resolved
  during meta evaluation, and directives are codegen-time.

### The `define` replacement / duplicate rule

`define_with_mutability` (`scope.rs:213-299`) is the gate. It checks for an
existing name **in the same namespace** and errors `DuplicateDefinition`
unless the existing entry is a *placeholder* that may be replaced
(`scope.rs:254-257`):

1. a dummy-span `Import` (built-in collection placeholder or prelude
   entry) — replaceable by anything;
2. a dummy-span `Trait` (prelude placeholder) — shadowable;
3. a dummy-span `Variant` of matching kind;
4. any user `import` may shadow a dummy-span prelude entry.

Real user declarations never silently replace each other. The asymmetry
fix in Snag #29 follow-up #2 (`scope.rs:244-253`) made user-def-then-import
and import-then-user-def both error consistently — a real user import is
no longer silently clobbered by a same-named def.

### Cross-module export and collisions

`export_non_private` (`scope.rs:649-721`) copies non-private names from the
FileModule scope into its parent. It returns **type-namespace collisions**
as `(name, existing, new)` so `collect_item` can emit a
`DuplicateDefinition` citing both spans (`resolve.rs:740-751`). This is
load-bearing: the C type-mangling layer is currently flat across modules,
so two user types named `ParseError` from different modules would collapse
to one C struct and break at link time. Value-namespace collisions are
*not* reported (`scope.rs:697-705`) — multiple stdlib modules legitimately
re-declare the same extern bound to the same C symbol, and function call
sites resolve through their call-site type rather than by name alone.

### Import fixups (the multi-pass tail)

After `collect_top_level_inner`, `collect_top_level` runs several fixup
passes over the merged module (`resolve.rs:175-299`):

1. **Glob imports** `from X import EnumName.*` — bring each variant into
   scope as a bare name now that all enums are defined
   (`resolve.rs:178-193`).
2. **Aliased imports** `from X import Y as Z` — `rebind_alias`
   (`scope.rs:585-598`) points the placeholder `Z` at `Y`'s real DefId,
   and `Z → Y` is recorded in `ctx.import_aliases` so a later AST rewrite
   (`mod.rs:244-246`, Pass 1.5) renames `Z` back to `Y` (the IR backend
   lowers by surface name) (`resolve.rs:204-213`).
3. **Wildcard imports** `from X import *` — re-bind each public name via
   `bind_wildcard` (`resolve.rs:227-243`); mostly a no-op today since
   `export_non_private` already made them visible.
4. **Private-import validation** (`resolve.rs:249-268`) — any remaining
   user `Import` placeholder whose name is in some module's private set is
   a `PrivateImport` error.
5. **Return-type re-resolution** (`resolve.rs:274-299`) — in cross-module
   builds, an entry-file function whose return type is an *imported* type
   resolves to `None` on the first pass (the type isn't in scope yet);
   this pass retries now that everything is collected.

## Pass 2 — body resolution (`resolve_bodies`)

`resolve_bodies` (`resolve.rs:807-823`) walks each top-level item and
resolves the names *inside* bodies, building the `ResolutionMap` it
returns. `resolve_item_body` (`resolve.rs:825-886`) dispatches:
functions and equip methods to `resolve_function`, const/static
initializers to `resolve_expr`, test/bench/suite bodies into fresh
function scopes, and nested `Module` items by **re-entering** the saved
FileModule scope (`resolve.rs:869-882`, via `enter_scope`/`restore_scope`)
so private names are visible during body resolution.

### `resolve_function` — scopes, generics, params

`resolve_function` (`resolve.rs:888-963`):

1. Pushes a `Function` scope and records it in `function_body_scopes`,
   keyed by `(name, span_start)` to avoid cross-file span collisions
   (`resolve.rs:897-901`).
2. **Backpatches the FunctionInfo's `scope_id`** from the module scope (set
   during collection) to this real body scope, looked up by *span* via
   `lookup_def_by_span` to survive shadowing (`resolve.rs:904-908`).
3. Defines generic type params (`DefKind::GenericParam`) and const generics
   (`DefKind::Const`) (`resolve.rs:910-930`).
4. Defines parameters as **mutable Variables** with `is_param = true` and
   their ownership annotation (`resolve.rs:932-949`).
5. Resolves the body block or expression, then pops the scope.

Equip blocks (`resolve.rs:965-995`) push an `EquipBlock` scope, define the
impl's generic params, and resolve each method via `resolve_function`.

### Statement walk and scope pushes

`resolve_stmt` (`resolve.rs:1009-1316`) is a large match. Two recurring
patterns are worth internalizing:

- **VarDecl resolves the initializer *before* defining the binding**
  (`resolve.rs:1018-1039`), so `int x = x` refers to the *outer* `x`. The
  binding kind is `Const` (immutable) or `Variable`; `shared` bindings get
  their `SharedKind` stamped on the DefInfo and an entry in the resolution
  map for IR lowering.
- **Every block-introducing construct pushes its own scope.** `for`/`loop`
  use `ForLoop`; `if`/`while`/`match`-arm/`with`/`named scope`/`select` use
  `Block`. `if`/`while` push the body scope *before* resolving the
  condition so that compound `is`-pattern bindings (`a is Some(x) and x >
  10`) are visible to the guard — handled by `resolve_is_condition`
  (`resolve.rs:1744-1762`), which defines pattern bindings left-to-right.

Meta statements (`MetaIf`/`MetaFor`/`MetaMatch`/`MetaWhile`) resolve their
**bodies** as real code but **skip their conditions/ranges/scrutinees**,
which are meta expressions evaluated at monomorphization time, not runtime
(`resolve.rs:1253-1308`).

Nested items inside a body (`Stmt::Item`, `resolve.rs:1246-1251`) get a
throwaway `ResolveContext`: they're collected and then body-resolved
inline.

### Expression walk and identifier resolution

`resolve_expr` (`resolve.rs:1318-1727`) is the heart. The two binding
sites that populate the resolution map:

- **`Expr::Identifier`** (`resolve.rs:1351-1385`): `scopes.lookup(name)` →
  insert `(span.start → def_id)`. On a miss it reports `UndefinedName`
  (with an edit-distance `suggest_name`) **unless** the name is a built-in
  (`is_builtin`, `resolve.rs:1961-1968` — `print`, `format`, `len`, the
  numeric type constructors, …), the synthetic `__return__`, or a *known
  variant name* (`is_known_variant_name`, `scope.rs:431-439`). The
  variant escape hatch exists because the loader's pre-merge variant
  qualifier can drop an ambiguous bare name from its rewrite map, leaving a
  bare `Identifier` that the typechecker resolves later via the
  expected-type hint — see the long comment at `resolve.rs:1356-1374`.
- **`Expr::Path`** (`resolve.rs:1387-1407`): resolves only the *first*
  segment; later segments are field/variant/method accesses resolved
  downstream.
- **`Expr::StructLiteral`** (`resolve.rs:1648-1667`): resolves the struct
  name (real `UndefinedName` if missing) plus arg expressions.

Binding-introducing expressions push scopes and define names: closures
(`resolve.rs:1549-1564`), the comprehensions
(`resolve.rs:1576-1633`), inline `match` (`resolve.rs:1517-1535`), and the
error-binding forms `Rethrow`/`Catch` (`resolve.rs:1688-1725`, Snag #37 —
the resolver registers the bound name so lookups in the transform/recovery
expression succeed, leaving its *type* to the typechecker).

Three call-shaped builtins are special-cased to avoid spurious
`UndefinedName`: `field_value`/`field_set`/`make_variant`
(`resolve.rs:1418-1443`) — their field-name / type-name args are
meta-loop variables or string literals, not runtime identifiers, and are
resolved at meta-substitution or rewrite time instead.

F-string interpolation segments (`Expr::StringLiteral(_, interp_exprs)`,
`resolve.rs:1333-1349`) are resolved into a *discarded* error sink:
closure params inside `f"{...}"` need DefIds, but errors are suppressed
because meta-for loop variables only materialize at monomorphization. Real
undefined names still surface when the same expression is used outside an
f-string.

### Pattern bindings

`define_pattern_bindings_with_kind` (`resolve.rs:1861-1917`) recurses
through `Binding`/`Constructor`/`Tuple`/`Or`/`DotShorthand`, defining each
`Binding` name. Or-patterns validate that **all alternatives bind the same
name set** (`OrPatternBindingMismatch`, `resolve.rs:1885-1907`) and then
bind from the first alternative.

`define_match_arm_pattern` (`resolve.rs:1810-1858`) adds one rule on top:
a top-level `Pattern::Binding(name)` that resolves to an outer
`Const`/`Static` is treated as a **value comparison** (`case FOO:` ≡
`case <FOO's value>:`), recorded in the resolution map rather than bound as
a fresh shadowing variable (Snag 2026-05-13 — without this, `match x: case
CONST:` shadowed the constant and routed every input to the first arm).

## What gets handed downstream

Pass 2's `ResolutionMap` is extended with `resolve_ctx.resolution_map`
(`mod.rs:252-253`) and flows into the `AnalysisResult`. That merge is
**presently a no-op**: Pass 1 (`collect_top_level`) records its output
only in side tables, and `ctx.resolution_map` is initialized empty
(`resolve.rs:105`) and never written during collection — every
`resolution_map.insert` in `resolve.rs` is inside a Pass-2 function
(`resolve_stmt`/`resolve_expr`/`define_match_arm_pattern`). The `extend`
is kept as a forward-compatible seam, not because Pass 1 currently
contributes resolutions. The resolution map flows into the
`AnalysisResult`
(`mod.rs:361-378`), alongside the `ScopeTable`, the `struct_fields` /
`enum_variants` / `function_info` side tables, the
`function_body_scopes` map, and `struct_generic_bounds`. Between the two
resolution passes and the typechecker run several small rewrites
(`mod.rs:255-289`): struct-constructor calls are rewritten to
`StructLiteral` nodes now that the resolver knows which identifiers are
structs (Pass 2.5), `.collect()` targets are selected, and the trait
registry is built. Field types on `DefInfo` are populated *after* the
trait registry (`populate_def_field_types`, `mod.rs:280-283`,
`397-455`) — the resolver leaves `field_types`/`variant_field_types` as
`None`.

## In the self-host

The Gorget self-host resolver lives in
`tests/fixtures/self_host_resolver/` — `resolve.gg`, `scope.gg`,
`ast.gg`, `types.gg`, plus `format_resolve.gg`/`format.gg` for the
canonical dump and `driver.gg`/`main.gg` to run it. It mirrors the
Rust two-pass structure and emits the same `DEF` / `SCOPE` / `RES` line
format that `format_resolution_canonical` produces on the Rust side
(`tests/integration.rs:12608-12647`).

The `resolver_comparison` test (`tests/integration.rs:12683`) builds the
Gorget driver, runs both resolvers over every `tests/fixtures/*.gg`, and
diffs the normalized output. The comparison strips `DEF` spans (the
self-host AST doesn't store name spans) and skips `SCOPE` lines
(structural AST differences — e.g. Rust's `Expr::Block` creates extra
scopes); **`RES` lines are the load-bearing exact-match correctness check**
(`tests/integration.rs:12650-12673`).

To read current parity, run:

```bash
cargo test --test integration resolver_comparison -- --nocapture
```

and read the printed matched-count. **Do not quote a fixed number** — the
`*_comparison` tests are diagnostic-always-pass (no assertion on the
score), so a green `cargo test` says nothing about parity; only the
printed counts do.

The dominant historical divergence has been **f-string interpolation
RES entries**: the Rust parser pre-parses interpolation segments into a
sidecar on `StringLiteral`, so its resolver emits `RES` entries (with very
large span keys) for names inside `f"{...}"` (`resolve.rs:1333-1349`);
where the self-host parser hasn't pre-parsed those segments, those entries
are absent. The remaining mismatches cluster around definition-ordering,
import/enum edge cases, and that f-string tail — re-derive the exact
breakdown from the `--nocapture` output rather than trusting any cached
figure.
