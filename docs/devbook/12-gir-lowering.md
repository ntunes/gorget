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
(`closures.rs:692`): a `FreeVarCollector` walks the body, and any
identifier that is neither a closure param nor a body-local, but *does*
resolve to a local in the enclosing scope, is a capture. Each capture's
mode is decided by `detect_mutations` (`closures.rs:877`): a captured
variable that the closure body *mutates* is captured `ByMutRef`;
otherwise `ByValue`. A `move`-closure forces everything `ByValue`.

The collector walks `Spanned<Expr>`, not bare `Expr`, so a `CaptureInfo`
(`closures.rs:31`) records the **span of the capture's first occurrence
inside the body** alongside its name, type and local. That span is what
makes the ownership decision below answerable: `is_last_use_at` is keyed
on a position, and it answers a conservative `false` for any *enclosing*
span — so the whole-closure span can never distinguish a source that is
dead after the capture from one that is still live. Both would read as
"still live", and every capture would clone.

One capture shape has no occurrence span to record: an identifier whose
only mention is inside an f-string interpolation. `StringSegment::Interpolation`
carries no span, and the collector recovers the name by splitting the
interpolation text, so that capture falls back to the enclosing literal's
span and takes the conservative answer.

The env struct's field types follow from the mode (`closures.rs:124-138`):
`ByValue` captures get the (CoW-resolved, `Ptr(T)` → `T`) value type;
`ByMutRef` captures get a `MutPtr(T)` field so the closure can write
through to the outer variable.

### The creation site is an ordinary consuming position

A closure environment owns its by-value captures exactly as a user struct
owns its fields, so the creation-site `StructInit` runs the *same* three
passes `lower_struct_init` runs, in the same order (`closures.rs:338-380`):

1. **`ensure_owned_at_boundary`** materializes borrows — CoW `Ptr(T)`
   aliases, ref-state locals, untracked resources. It is skipped for
   `ByMutRef` captures, whose field *is* a `MutPtr`: the pointer operand is
   exactly what must be stored, and cloning it would deep-copy the pointee
   and store the address of a temporary.
2. **`clone_multi_use_resource_args`** applies the other half of the
   consuming-position table: clone a source that is still live past the
   capture; leave one that is dead alone. It reads the capture's occurrence
   span through a synthesized `Expr::Identifier` argument, and it is what
   makes a bare borrow param clone regardless of liveness — the caller keeps
   ownership, so moving out of it would hand the caller's own buffer across
   the boundary.
3. **`move_zero_consumed_args`** performs the transfer, *after* the init has
   read its operands: every resource source the environment now owns is
   `move_zero_and_mark`ed.

That third pass is why the transfer is sound rather than merely quiet.
`move_zero_and_mark` **keeps** the source's drop entry and zeroes the slot,
so `drop_elab` statically deletes the drop on the paths where the move
happened and keeps it on the paths where it did not. Deleting the entry
outright is not CFG-aware — every path loses the drop, which is how a
transfer manufactures the return-borrow double-free class.

Routing through the shared sequence, rather than reimplementing the table
here, is [layering rule 3](24-layering-discipline.md) at a boundary where
two implementations had drifted: one that clones borrows, and one that
clones borrows *and* live owned sources. A capture is not a special
consuming position — it is the fifth one.

### `is_closure_env` is identity, not an ownership claim

The lifted struct's `TypeMetadata` sets `is_closure_env: true`
(`closures.rs:181`) beside `closure_call_fn` and `closure_captures`. The
flag answers "is this type a closure environment?" and nothing else.

The env's `drop_strategy` and `copy_semantics` are **computed from its
fields** by `TypeRegistry::compute_drop_strategy_for_struct`
(`closures.rs:172`) — the same helper every other aggregate registration
uses. An env holding a droppable capture is `(Recursive, Resource)`, so
`__Closure_N__drop` is synthesized and the field is freed exactly once, by
the environment that owns it. Asserting `(None, Trivial)` instead would be
an environment carrying a value while declaring it carries nothing: no drop
is synthesized, and the capture is either leaked or freed by whoever else
still aliases the buffer. The Tier 1c coherence validator sees closure envs
like any other struct and flags that mismatch.

One narrow carve-out remains, in the consume-site validator's `StructInit`
arm (`validate.rs:2596-2613`): a capture whose type
`lacks_materialization_path` (`types.rs:790`). That predicate is derived,
never listed — `needs_drop && !is_resource_type && !is_refcount_clone_type`
— and it selects the single-owner-by-design handles, `Callable[T]` above
all, which lowers to `GirType::FnPtr` and has neither a deep clone nor an
incref. Every materializing pass already skips those operands because there
is nothing they know how to copy, so ownership at such a capture genuinely
is undecided. It is carved out rather than flagged because none of the three
answers can be given: an implicit clone or an implicit move breaches the
ratified carve-out, and a rejection has no fix-it a user can write until
per-variable capture lists exist. Flagging it would turn working programs
into build failures with no way out.

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
The lifted struct param ABI is registered with the env passed `ByPtr`.

**Closure identity is typed metadata, and it is minted once.** The env
struct's `TypeMetadata` carries `closure_call_fn` — the name of its lifted
call body — and `closure_captures`, the `ByValue` captures with their
struct field indices (only `ByValue` captures are spawnable across thread
boundaries; `ByMutRef` pointers can't cross). Both are written at the one
mint, beside `is_closure_env`, and read back through
`TypeRegistry::closure_call_fn(type_id)` / `closure_captures(type_id)`.
Call dispatch and the spawn-wrapper path both ask the TYPE; neither
reconstructs `{name}__call` from a spelling, and there is no parallel
`struct_name → info` map to fall out of step (layering rule 3).

The same fact travels two more hops as typed fields, because the layers
below cannot reach the GIR registry: `StructDef::closure_call_fn` carries
it to the backends, and `Function::takes_env` — set by the very expression
that pushes `__env` as parameter 0 — answers the *other* question, "is
this function a closure's call body?", for the LIR call lowering, the
dead-code root set and BIR's signature snapshots.

Neither question may be answered from a name, and the reason is that the
namespace is shared. A user method mangles to `{Type}__{method}`, so
`equip Runner: int call(self, …)` is `Runner__call` — indistinguishable
from a closure call body to any substring test for `__call`. A callee
mistaken for a closure body has its closure arguments left unpacked, and
the environment it then reads is the wrong shape: a capture-less closure
faults on a one-byte object, while a closure whose first capture is itself
a `Callable` finds a valid code pointer where an integer belongs and
returns a plausible wrong number with a clean exit code. The typed
carriers exist so that question has one answer and no spelling can supply
a second.

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
