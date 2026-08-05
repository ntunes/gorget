# 12. GIR & lowering: monomorphization, drops, closures

GIR ("Gorget IR") is the mid-level, control-flow-explicit IR that sits
between the type-checked AST and the SSA-based LIR backend. Lowering is
the pass that consumes the resolved + type-checked AST and produces a
`Module` of `Function`s, each a CFG of `BasicBlock`s over a flat
`Vec<Local>` slot array. This is where the language's *abstractions
evaporate and its invariants accumulate*:
generics get monomorphized into concrete instances, methods/traits get
resolved to free functions, closures get lifted into structs + call
functions, and ownership decisions (drops, moves, clones, borrows) get
made explicit as instructions. The IR data structures live in
`src/ir/` (`mod.rs`, `types.rs`, `instructions.rs`); the lowering pass
lives in `src/ir/lowering/`, entered through
`lower_module` (`src/ir/lowering/mod.rs:77`).

> *Verified against source at the commit this chapter was written
> against. Re-derive any line numbers that have drifted — and treat the
> numbers in the former `unified-resource-model.md` roadmap (since folded
> into this chapter and chapters 13/15/18/25) as stale (it was a roadmap,
> not a status record).*

## What GIR looks like

A complete module is `Module` (`src/ir/mod.rs:244`): a `TypeRegistry`,
a `Vec<Function>`, globals, externs, plus a large pile of sidecar
metadata (`RuntimeFeatures`, ABI tables, purity, clone warnings). A
`Function` (`src/ir/mod.rs:383`) carries `params: Vec<TypeId>`,
`return_type`, a `Vec<Local>`, and `blocks: Vec<BasicBlock>`. The local
convention is fixed: `_0` is the return place, `_1..=_N` are the params,
and everything after is user variables and compiler temps
(`src/ir/mod.rs:387`).

A `Local` (`src/ir/mod.rs:663`) is a `type_id`, an optional `name_hint`,
and — load-bearing for everything in this chapter — an `ownership:
LocalOwnership` field, a `slot_kind: SlotKind`, and an `is_owning_param:
bool`. These three carry the per-value ownership/borrow decision through
the GIR→LIR boundary so backends never re-derive it (see
[§ Ownership state on locals](#ownership-state-on-locals)).

`Instruction` (`src/ir/instructions.rs:141`) is the non-control-flow
vocabulary: `Assign`, `FieldLoad`, `IndexLoad`, `LoadRef`/`StoreRef`,
arithmetic/compare, `StructInit`/`EnumInit`/`TupleInit`, `Call`/
`CallExtern`, the ownership ops `MoveZero`/`Borrow`/`BorrowMut`/`Drop`/
`DropIfAlive`, allocator ops, and `InlineC`. Control flow is carried
separately by `Terminator` (`src/ir/instructions.rs:384`): `Return`,
`Jump`, `Branch`, `Switch`, `Invoke` (the `try`/error-propagation edge),
`Unreachable`. An operand is `Copy(Place)` / `Move(Place)` /
`Constant(_)` (`src/ir/instructions.rs:40`); a `Place` is a `LocalId`
plus a projection path of `Field`/`Index`/`Deref`
(`src/ir/instructions.rs:5`,`28`).

The reads that touch a *place* carry a `ReadMode`
(`src/ir/instructions.rs:91`): `Copy`, `Move`, `Clone`, `Borrow`. This
is the single source of truth for "how does this value flow out of its
source", and `AssignMode` is literally a type alias for it
(`src/ir/instructions.rs:117`). `IndexLoad.read` and the
`EnumFieldLoadMode` (`src/ir/instructions.rs:133`) are typed views of
the same decision. This is the unified read-mode discipline that
replaced four previously-parallel per-instruction enums; the rationale
is folded from `unified-resource-model.md` §6.4 below.

A GIR module is printable — `print_module` (`src/ir/printer.rs:8`) is
what `gg build --emit-gir` dumps, and it's exactly the textual form the
self-host comparison test diffs against (each function prints a `fn …`
line; see [§ In the self-host](#in-the-self-host)).

## The lowering pass: order of operations

`lower_module` (`src/ir/lowering/mod.rs:77`) runs a long fixed sequence
of sub-passes (instrumented with a `time_pass!` macro that records
per-pass wall-clock into `Module::gir_lower_pass_times` for `gg
profile`). The spine, in order:

1. **Flatten + mangle.** `Item::Module` wrappers from `merge_modules()`
   are flattened so all subsequent passes see one item list; module-path
   manglings (`seg1__seg2___fn`) are pre-computed to avoid C linker
   collisions (`src/ir/lowering/mod.rs:86-150`).
2. **Two-pass type registration** (see next section).
3. **Drop-strategy upgrade** — `upgrade_types_from_fields`
   (the single call site is `src/ir/lowering/mod.rs:273`, right after
   type registration, the allocator-type registration, and the
   `equip … with Drop` metadata scan) walks types and upgrades any
   aggregate that transitively contains a droppable field to
   `Resource` + `Recursive`, to a fixed point. It runs **at module
   start, before monomorphization** — the substitute body lowering and
   `generics/mod.rs:2448` both rely on the upgrade having already run.
4. **Generic monomorphization** — collect templates, discover concrete
   instantiations, generate monomorphized `TypeDef`s and `Function`s
   (the pass begins at `src/ir/lowering/mod.rs:375`).
5. **Function-body lowering** — non-generic functions, then
   monomorphized generic instances, then equip-method instances
   (`lower_function`, `src/ir/lowering/functions.rs:583`).
6. **Closure emission** — lifted closures collected during body lowering
   are emitted as their own functions.

The two big "abstractions evaporate" transforms — monomorphization and
closure lowering — are detailed below, bracketed by the type-registration
and drop-insertion machinery they depend on.

## Two-pass type registration

The forward-reference problem: a struct `A` can have a field of type `B`
where `B` is declared *later* in the file. If field-type resolution ran
in one pass, `B` would resolve to `UNIT_TYPE` (the "unknown/placeholder"
type), and `A`'s `B`-typed field would silently get the wrong layout —
worse, a `UNIT_TYPE` field marks the type as a generic-template
placeholder and suppresses its C output.

The fix is two passes over the item list
(`src/ir/lowering/mod.rs:167-227`):

- **Pass 1** (`mod.rs:172-197`): register every non-generic struct/enum/
  newtype *name* as an empty `GirType::Named(name)` in the registry, with
  no fields. After this pass, every user type name resolves to a stable
  `TypeId`, even though the bodies are still empty.
- **Pass 2** (`mod.rs:199-227`): call `register_struct_type` /
  `register_enum_type` / `register_newtype` to fill in the actual fields.
  Because every name already resolves from Pass 1, a field referencing a
  later-declared type gets the correct `TypeId` instead of `UNIT_TYPE`.

The comment at `src/ir/lowering/mod.rs:167-171` spells out the bug this
prevents. Note this is the *GIR-side* two-pass registration; the LIR/C
backend does its own analogous pre-registration for C struct emission
(topological sort + placeholder structs — covered in the backend
chapter). They solve the same forward-reference class at different
layers.

A handful of runtime-only types (allocators like `Arena`,
`PoolAllocator`) are registered as `MutPtr(Named(GorgetArena))` right
after, since they're opaque C handles never declared in `.gg` source
(`src/ir/lowering/mod.rs:233-248`).

## Monomorphization

Gorget has no runtime generics — every `Pair[int, String]` becomes a
distinct concrete `TypeDef`, every `identity[T]` call becomes a call to
a distinct concrete `Function`. The machinery is `GenericCollector`
(`src/ir/lowering/generics/mod.rs:51`), which runs in phases:

1. **Collect templates** — `collect_templates`
   (`generics/mod.rs:127`) harvests every generic struct/enum/function/
   equip-block AST node into per-base-name maps, plus trait defs (so
   equip blocks can find default-method bodies).
2. **Discover usages** — `discover_usages` (`generics/mod.rs:234`) walks
   the module for concrete instantiation sites and records `(base_name,
   concrete_type_args, mangled_name, kind)` tuples, deduplicated by
   mangled name.
3. **Transitive discovery** — `discover_transitive`
   (`generics/mod.rs:783`) is a **worklist fixpoint**: it indexes through
   the growing `instances` vector, and for each instance, substitutes
   the concrete type args into the template body and *re-scans* it. A
   `Vector[Pair[int, String]]` instantiation discovered inside a generic
   function body only becomes concrete once the outer function is
   specialized, so the loop keeps going until no new instances appear
   (`generics/mod.rs:784-786`, `while i < self.instances.len()`). The
   code carries a sharp warning about *not* speculatively expanding
   trait-default adapter surfaces here, because that produced an infinite
   loop (`TakeIter[TakeIter[…]]` ad infinitum — `generics/mod.rs:841-858`).
4. **Method-level-generic instances** — `discover_method_instances`
   (`generics/mod.rs:886`) handles the harder case of a method like
   `equip [T] VectorIter[T]: U map[U, F](F f)` where the method has its
   *own* generic params (`U`, `F`) on top of the receiver's (`T`). Each
   call site produces a `MethodInstance` (`generics/mod.rs:34`) carrying
   the *merged* substitution and a fully mangled symbol like
   `VectorIter__int64_t__map__int64_t__GorgetClosure`.
5. **Emit** — `monomorphize_types` (`generics/mod.rs:1621`) creates the
   monomorphized `TypeDef`s; the function/method instances are lowered as
   ordinary functions later in `lower_module`.

Type substitution is in `generics/substitute.rs`
(`substitute_type` at `:29`, `substitute_function_body` at `:103`) — a
straightforward AST rewrite replacing generic-param names with concrete
`Type` nodes.

### Name mangling

The contract between monomorphized types and the rest of the pipeline is
the mangled name. `mangle_generic_name` (`src/ir/lowering/types.rs:1016`)
maps `Vector[int]` → `Vector__int64_t`; `mangle_type_for_name`
(`types.rs:1084`) is the per-arg spelling (`int`/`int64` →
`"int64_t"`, etc.). Crucially, **mangling is a name-encoding for
codegen, not a semantic decision** — downstream passes that need to know
"is this a closure type" read typed metadata, not the `GorgetClosure`
substring in the name (per CLAUDE.md "No name matching"). For example
`Callable[T]` mangles to `Callable__GorgetClosure` in collection
positions (`types.rs:84-91`), but whether a value needs dropping is
decided by `needs_drop` reading `TypeDef` metadata, not by spotting that
substring (with one residual `FnPtr`-shape special-case — see below).

## Ownership state on locals

Every `Local` carries a typed `ownership: LocalOwnership` field
(`src/ir/mod.rs:670`, on the `Local` struct; the enum it holds is
defined at `src/ir/mod.rs:450`). This is the single field that replaced
a 7-variant `LocalOwnershipState` and a 3-variant `OwnershipState` plus
roughly six parallel sidecar `FxHashMap`s in the lowering context (the
consolidation known as **Phase D**; the enum at `src/ir/mod.rs:450` is the
result). The variants:

- `Untracked` (the `#[default]`) — "no ownership decision recorded yet",
  preserving the legacy "absent from the hashmap" semantics so readers
  don't treat a fresh local as `Owned` (`src/ir/mod.rs:457`).
- `Owned` / `FreshOwned` — owns its data and is drop-registered.
  `FreshOwned` is the strictly-stronger "provably no other local aliases
  this buffer" case, set when a runtime callee returns a fresh allocation;
  it powers return-clone elision (`src/ir/mod.rs:463-471`).
- `Borrowed { origin: BorrowOrigin, mutability }` — does *not* drop;
  carries provenance.
- `View { source }` — a runtime view (cap=0 string slice today); drop is
  a no-op until materialized (`src/ir/mod.rs:475-480`).
- `SharedHeap { source }` — the `String b = a` value-aliasing case: the
  local IS its own 32-byte owned slot, but its heap data is shared with
  `source` (`src/ir/mod.rs:481-491`).
- `MaybeOwned` — started borrowed, may have been materialized on some
  paths; conditional drop guard.

`BorrowOrigin` (`src/ir/mod.rs:582`) is *where* a borrow points:
`Param`, `CollectionElement`, `Field`, `Alias`, `RuntimeView`,
`FieldPath`, `CowBorrowPending`, `TupleElement`. This is the provenance
that lets the compiler answer "if I mutate collection X, which borrows
must I materialize first?" without lifetime annotations — it is, per the
design note, the actual Gorget invention that buys "no lifetimes" without
losing the safety guarantee.

`SlotKind` (`src/ir/mod.rs:641`) is the orthogonal *layout* axis read by
the LIR slot-routing sites: `Value` (slot holds the value),
`OwnedPtr` (slot holds a pointer this local owns), `BorrowedPtr` (slot
holds a non-owning view pointer, deref-free, no drop). `is_owning_param`
(`src/ir/mod.rs:688`) is the one extra bit that distinguishes a `!`-sigil
resource param (owns its pointee, must drop at exit) from a `&` param
(borrows, never drops) — both otherwise share `Borrowed { Param(self),
Unique }` + `BorrowedPtr`.

## Drop insertion

Drops are inserted by the `DropElaborator` (`src/ir/lowering/drops.rs:22`).
Its contract is sharp: **the elaborator decides *when* to drop** (by
emitting `Drop` / `DropIfAlive` instructions at scope boundaries); **the
backend decides *how*** (by looking up the type's `DropStrategy` from the
registry). That split is documented at `drops.rs:11-21`.

The elaborator maintains a stack of `DropScope`s
(`drops.rs:31`), one per owning language construct
(`DropScopeKind::{Function, Loop, Block}` — `drops.rs:39`). Each scope
records its registered locals in declaration order; **drops fire in
reverse (LIFO)** at scope exit (`drops.rs:34`). `lower_function` pushes a
`Function` scope at entry (`functions.rs:684`), registers params via
`register_param` (`functions.rs:696`), and pops it — emitting drops — at
the natural exit (`functions.rs:785`, `pop_scope`). Explicit `return`
paths emit their drops eagerly via `emit_early_exit_drops` and then pop
*without* re-emitting (`functions.rs:813`, `pop_scope_no_emit`).

What gets registered: `register_local` registers any local whose type
`needs_drop` (`drops.rs:107`), i.e. `CopySemantics::Resource` OR a
non-`None` `DropStrategy` (`src/ir/types.rs:405-427`,
`TypeRegistry::needs_drop`). The one name-shape special case left is
`GirType::FnPtr` — a bare `Callable` local carries a heap-alloc'd env so
it always needs drop (`types.rs:414-416`); this is the documented "Phase
A residual #1".

Drop *ordering* across borrows: `emit_scope_drops_ordered`
(`drops.rs:414`) is normally plain LIFO, but when a scope contains borrow
dependencies (registered via `add_borrow_dep`, `drops.rs:77`) it runs a
**Kahn's-algorithm topological sort with a LIFO tiebreaker** so a
borrower is always dropped before its source (`drops.rs:457-485`),
preventing use-after-free at scope exit.

A deliberate conservatism: every scope-exit drop emits `DropIfAlive`, not
`Drop`, *defensively* (`drops.rs:506`). The reasoning (Snag #30,
`drops.rs:495-504`): the GIR-level `maybe_moved` tracking produced a
false negative across nested-match + early-return joins, yielding an
unconditional drop and a double-free. So GIR always emits the conditional
form, and the LIR `drop_elab` pass statically elides the runtime
liveness check when slot-init is provably unconditional — no codegen
quality lost. This is the same drop-flag dataflow the `!`-owning-param
path relies on (`drops.rs:56-64`).

### The drop-strategy upgrade fixpoint

`upgrade_types_from_fields` (`src/ir/lowering/mod.rs:3019`) is the pass
that makes "a struct containing a `String` field is itself droppable"
work. It loops to a fixed point (`mod.rs:3025`): each iteration
re-collects the set of currently-droppable type names, scans every
struct/enum for a field/variant-payload whose type is in that set (or is
`GorgetString` / a collection), and upgrades any match to
`DropStrategy::Recursive` + `CopySemantics::Resource`
(`mod.rs:3083-3093`). The fixpoint is required because upgrading
`Option[String]` to droppable can in turn make a struct containing
`Option[String]` need upgrading (`mod.rs:3022-3024`). The backend then
walks fields and emits per-field cleanup for `Recursive` types.

The valid `(CopySemantics, DropStrategy)` combinations and what each
encodes are tabulated in the `TypeMetadata` doc comment
(`src/ir/types.rs:118-141`).

## Global materialization: rooting a `Place` at a local

A `Place` roots only at a `LocalId` plus a projection path
(`src/ir/instructions.rs:5`); there is no `Place` base for a module-level
`static`. So a global has no place to *project into* — and a naive
field access or index read on one degrades silently. Field reads on a
static struct fall through to `const unit`/0 and field *stores* emit zero
instructions (the write is silently dropped); an index read on a static
const-folds to 0. The bug is invisible at the surface: `P.x` returns
garbage, `P.x = 99` does nothing, and nothing errors.

The fix is a single pattern — **materialize the global identifier into an
addressable pointer local, then let the existing pointer-deref place path
project through it.** The helper `materialize_global_field_base`
(`src/ir/lowering/exprs/mod.rs:2321`) detects an `Expr::Identifier`
naming a global, emits `&NAME` via `Constant::GlobalRefPtr` (a real
`*mut <T>`) into a fresh local typed `MutPtr(<struct>)` through
`register_mut_ptr_type`, and returns `Operand::Copy` of that pointer
local. The existing field path then appends a `Projection::Deref` and
walks through it, so reads, resource-field borrows, and write-through
stores all work unchanged. Crucially the pointer local is typed
`MutPtr(base)` — *not* `GlobalRefPtr`'s own type inference, which returns
the bare base type — because the typed pointee is what drives the
downstream `Deref` projection.

This mirrors the index-load precedent (`lower_index_access`,
`src/ir/lowering/exprs/methods.rs:3272-3282`), which materializes a
`GlobalRef`-typed index base into a local before the place path emits the
real `index_load`. But the two diverge deliberately on read mode: the
index path materializes with `AssignMode::Borrow`/`Copy` (a read of a
value local — a resource collection borrows zero-cost, a value type
copies), whereas the field path uses `MutPtr`+`Deref` because the *store*
path (`P.x = 99`) must write **through** to the global, not to a stack
copy. Read-only materialization can copy; a mutable place root cannot.

Because the same defect lives at every field entry point, the helper is
wired into all three (sibling-site discipline, Ch. 24): the place
resolver `try_resolve_field_place` (`exprs/mod.rs:2362`, which covers the
field-store callers and the nested-recursion case),
`lower_field_access` (`exprs/mod.rs:2069`), and the field-store fallback
in `assigns.rs` (`src/ir/lowering/stmts/assigns.rs:638`). One helper,
three call sites, so a future fourth field entry point is forced through
the shared path rather than re-growing the silent-drop hole.

## Closure lowering and capture

> Note: an earlier internals doc `closure-capture.md`
> was referenced from source comments (`src/ir/validate.rs:1943`,
> `src/ir/lowering/closures.rs:149`) but never existed. Those
> citations now point here; this section is the authoritative
> closure-lowering reference.

Closures don't exist in GIR — they're *lifted* into a struct + a
function. `lower_closure` (`src/ir/lowering/closures.rs:79`) transforms
an `Expr::Closure` into three things (`closures.rs:1-6`):

1. A `__Closure_N` **env struct** holding the captured variables.
2. A `__Closure_N__call` **function** taking an env pointer + the closure
   params.
3. A `StructInit` at the creation site that builds the env.

### Capture analysis

Free variables are found by `collect_free_vars`
(`closures.rs:604`): a `FreeVarCollector` walks the body, and any
identifier that is neither a closure param nor a body-local, but *does*
resolve to a local in the enclosing scope, is a capture
(`closures.rs:631-640`). Each capture's mode is decided by
`detect_mutations` (`closures.rs:781`): a captured variable that the
closure body *mutates* is captured `ByMutRef`; otherwise `ByValue`
(`closures.rs:101-118`). A `move`-closure forces everything `ByValue`
(`closures.rs:102-104`).

The env struct's field types follow from the mode (`closures.rs:124-138`):
`ByValue` captures get the (CoW-resolved, `Ptr(T)` → `T`) value type;
`ByMutRef` captures get a `MutPtr(T)` field so the closure can write
through to the outer variable.

### Capture-by-value is a clone (or a move), never a shallow alias

At the creation site (`closures.rs:280-335`), each `ByValue` field
operand is produced carefully, because a closure outlives the stack frame
it was created in:

- If the capture is a CoW `Ptr(T)` alias at its **last use**, the source
  is moved into the struct (auto-deref + `move_zero_and_mark`,
  `closures.rs:288-297`).
- If it's an owned by-value resource at **last use**, the `MoveZero` is
  *deferred* to after the `StructInit` reads it (so the field init can
  still read the source), then the source slot is zeroed
  (`closures.rs:307-315`, `338-343`). Without this, the source's
  scope-exit drop would free a buffer the closure env still owns — a
  heap-UAF on closure invocation (`closures.rs:274-278`).
- Otherwise, `ensure_owned_at_boundary` inserts a deep **clone**
  (`closures.rs:319-324`, `ImplicitCloneReason::ClosureCapture`).

This is the consuming-position contract from CLAUDE.md applied to a
capture boundary: the closure env must *own* its captured resource data,
so the compiler either moves (when the source is dead) or clones (when
it's a borrow or stays live). A shallow alias would double-free.

### The env struct is tagged `is_closure_env`

The lifted struct's `TypeMetadata` sets `is_closure_env: true`
(`closures.rs:150`). This is read in two validator carve-outs in
`src/ir/validate.rs`:

- The **consume-site validator** skips `StructInit` fields whose
  destination is a closure-env struct, so the deliberate bitwise-copy of
  a captured value at non-last-use doesn't trip `OwnedLiveSourceConsumed`
  (the `Instruction::StructInit` arm at `validate.rs:2430-2440`: the
  `is_closure_env` read at `validate.rs:2438`, `if is_closure { continue; }`
  at `:2440`).
- The **type-metadata coherence validator** skips closure-env structs
  entirely (`validate.rs:1944`): a closure that captures at non-last-use
  holds *lifetime-tied aliases* it doesn't independently own, so the env
  struct stays `(DropStrategy::None, CopySemantics::Trivial)` and
  scope-exit doesn't double-free the captured values (the outer-scope
  drops handle them). The explanatory comment is at
  `validate.rs:1935-1943` (it still cites the dead `closure-capture.md`;
  it means this section).

### The call function

`emit_closure_call_function` (`closures.rs:349`) builds
`__Closure_N__call(env_ptr, params…)`. The env pointer is `_1`; params
are `_2..` (`closures.rs:430`). The body re-loads captures out of the env
struct via `FieldLoad`: a resource-typed `ByValue` capture loads as a
`Ptr(T)` *borrow into the env's storage* — the env owns the data across
calls, the body reads through the borrow, and auto-clone fires only if
the value crosses an ownership boundary (`closures.rs:386-411`,
`set_field_borrow`). A `ByMutRef` capture loads the `MutPtr` and reads/
writes through `Deref` projections (`closures.rs:413-425`).

The call function gets its own fresh `DropElaborator` and a `Function`
drop scope (`closures.rs:376-377`), so locals registered during body
lowering land in the closure's scope, not the outer function's.

### ABI and the runtime boundary

A closure value at the C ABI is a `GorgetClosure` `{fn_ptr, env}` pair.
The lifted struct param ABI is registered with the env passed `ByPtr`
(`closures.rs:233`), and `register_closure_info` (`closures.rs:249`)
records the struct/call-fn/captures for call dispatch and for the
spawn-wrapper path (only `ByValue` captures are spawnable across thread
boundaries — `ByMutRef` pointers can't cross — `closures.rs:238-248`).
The mapping of the lifted `__Closure_N` struct and the
`Callable__GorgetClosure` mangled form onto the runtime `GorgetClosure`
struct happens at the C backend boundary (`src/backend/c_lir/`), which
spells the runtime symbol — the one place name-spelling is the contract
(per CLAUDE.md's C-emit exception).

## In the self-host

The self-host lowerer lives in `tests/fixtures/self_host_lowerer/`,
driven by `lower.gg` (~9,900 lines). It is a real AST→GIR lowerer written
in idiomatic Gorget: it imports a `GirModule`/`GirFunction`/`GirLocal`/
`BasicBlock`/`Instruction` model from `gir.gg` and emits the same `fn …`
GIR textual form the Rust `print_module` does. Its `ast.gg`, `parser.gg`,
`resolve.gg`, `typecheck.gg`, `types.gg` etc. are **symlinks** into
`self_host_typechecker/` (confirm with `ls -la
tests/fixtures/self_host_lowerer/*.gg`); only `lower.gg`, `gir.gg`,
`driver.gg`, and the LIR-stage files are independent.

It mirrors the Rust design closely: it has a `DropEntry`/`DropScope`/
drop-elaborator structure explicitly modeled on Rust's
`src/ir/lowering/drops.rs` (`lower.gg` header comments at lines ~45-78),
including the LIFO scope stack, the early-exit walk, and the Snag #30
"always emit unconditional/`DropIfAlive`-shape and let LIR elide" rule
(lower.gg ~52-55). It carries a `ResourceMetadata` table (imported from
`gir.gg`) as its typed-metadata answer to "is this a resource, what drops
it", and it handles generic monomorphization mangling
(`Vector[int]` → `Vector__int64_t`) and closure/`__callable_N` shapes
(`lower.gg` has ~160 closure-related and ~150 generic/monomorph
mentions).

**Parity is measured, not fixed.** The `lowerer_comparison` test
(`tests/integration.rs:13390`) builds the self-host driver, runs both the
Rust `gg build --emit-gir` and the self-host lowerer over every top-level
`.gg` fixture, and compares the count of `fn ` lines in each output
(`integration.rs:13438-13441`). It is **diagnostic-always-pass** — a
green `cargo test` says nothing; only the printed matched/mismatched
counts do. To read current parity:

```bash
cargo test --test integration lowerer_comparison -- --nocapture
```

and read the matched-count it prints. Note the metric is *function-count
shape* parity, not byte-identical GIR — true structural parity is a
stricter bar than the number suggests. (The companion `c_emit_comparison`
and `self_host_bootstrap_fixed_point` tests reuse the same cached driver
build.)
