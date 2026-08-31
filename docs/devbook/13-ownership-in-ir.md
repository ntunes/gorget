# 13. Ownership in the IR

Chapter 12 covered the mechanics of GIR lowering — how the AST becomes
basic blocks of `Instruction`s. This chapter covers the *ownership*
information those instructions carry: how every read of a resource-typed
value is tagged `Copy` / `Move` / `Clone` / `Borrow`, how each local's
ownership state and borrow provenance are recorded directly on the GIR
`Local`, and the decision tree that picks clone-vs-move-vs-borrow at
every consuming position. It lives in `src/ir/instructions.rs` (the typed
read-mode and ownership vocabulary), `src/ir/mod.rs` (the per-local state:
`LocalOwnership`, `BorrowOrigin`, `Local`), `src/ir/lowering/` (the
emission sites that *write* the modes), and `src/ir/validate.rs` (the
fatal validator that *checks* them).

> *Verified against the source as of the commit this chapter was written
> against (the figures below — variant names, file:line, the seven-branch
> `lower_var_decl_assign_mode`, the validator's fatal gates — are
> re-derived from current source). Re-derive line numbers if they've
> drifted.*

This is the IR-side counterpart to the copy-on-write design (Chapter 11):
CoW decides *when* a borrowed value must become owned; this chapter is
about the typed metadata that records the answer and the validator that
enforces it. The user-facing contract — what the compiler promises at
`push`/`put`/`set`/`insert`/`send` and at bare-identifier assignment — is
in `CLAUDE.md` "Ownership at Consuming Positions"; the full CoW spec is
[Chapter 11](11-copy-on-write.md).

## The problem this metadata solves

Every Gorget value is either **trivial** (freely bit-copyable — `int`,
`bool`, `float`, simple structs with no resource fields) or a **resource**
(owns heap data — `Vector`, `Dict`, `Set`, `String`, user structs with
resource fields, closures, `Box`, `Task`, …). For a resource, a shallow
`memcpy` produces two slots pointing at the same heap buffer; both get
dropped at scope exit; double-free. So the GIR must record, at *every*
read of a resource, whether that read is a bitwise copy, an ownership
transfer, a deep clone, or a non-owning borrow — and a downstream pass
must reject any read that would shallow-copy a resource.

The historical failure mode was that this decision was
spread across a dozen parallel name-based lookup tables and half a dozen
sidecar `HashMap`s keyed by `LocalId`, which drifted relative to each
other — "every use-after-move that escaped the borrow checker in the last
six months is a case where one sidecar said one thing and another said
something else" (§6.1). The fix was two consolidations, **Phase A** (the
*type* axis — one `TypeMetadata` per type) and **Phase D** (the *local*
axis — one `LocalOwnership` field per `Local`), and then **Phase C** (a
fatal validator made possible by them). Phases C and D have shipped (D6
partial); Phase A is partial. This chapter documents the shipped state.

## The unified read-mode vocabulary (`ReadMode`)

There is one canonical four-case enum for "how is this place read?", at
`src/ir/instructions.rs:91-108`:

```rust
pub enum ReadMode {
    Copy,    // bitwise copy — source type MUST be trivial
    Move,    // consume the source — must own AND be at last use
    Clone,   // deep clone via the type's clone fn (Phase A metadata)
    Borrow,  // destination is a reference / view — source stays live
}
```

This replaced four previously-parallel per-instruction mode enums
(`AssignMode`, `FieldLoadMode`, `IndexLoad.borrow: bool`, `ArgOwnership`),
each carrying the same four-option choice in slightly different
vocabulary and validated by slightly different code — exactly the drift
risk Phase D §6.5 calls out. Existing instructions keep their shape;
typed views alias into the one enum so the validator and the readers
share one rule:

- `pub type AssignMode = ReadMode;` (`instructions.rs:117`) — a type
  alias, not a wrapper struct, so every existing
  `AssignMode::{Copy,Move,Clone,Borrow}` emission site folds onto the
  unified vocabulary for free.
- `IndexLoad.read: ReadMode` (`instructions.rs:158-171`) — how an element
  flows out of a collection: `Borrow` for a zero-copy view (string
  for-loop iteration, and — since Fix C — recursive-struct / enum
  for-loop elements bound as `Ptr` aliases; see Chapter 11's "For-loop
  elements: borrow the element, don't clone it"), `Clone` (the default)
  everywhere else. `Copy` and `Move` are reserved here — the LIR currently
  only routes Borrow vs Clone for collection reads. The LIR honoured the
  `Borrow` mode for *strings only* until Fix C — a read-mode invariant
  erosion at the layer boundary, the worked example below in Chapter 24.
- `EnumFieldLoadMode { Move, Borrow }` (`instructions.rs:133-137`) is the
  one mode enum that stayed separate, because it carries a narrower
  semantics — see [Move-vs-borrow on enum payload extraction](#move-vs-borrow-on-enum-payload-extraction)
  below.

The mode is set at emission. The builder constructors make the default
explicit: `FunctionBuilder::index_load` emits `read: ReadMode::Clone`,
`index_load_borrow` emits `read: ReadMode::Borrow`
(`src/ir/builder.rs:248-257`).

### What each mode means at the read site

| Mode | Validator rule | Runtime effect |
|------|----------------|----------------|
| `Copy` | source type MUST be trivial | bitwise copy; both slots valid |
| `Move` | source must own AND be at last use | transfer ownership; source becomes logically dead, zeroed by a later `MoveZero` only if drop-tracking would re-drop it |
| `Clone` | source's type must expose a clone fn | deep copy; both slots independently owned |
| `Borrow` | unique-vs-shared borrow rules | destination is a `Ptr(T)`/view; source stays the owner |

The `Move` mode is paired with the `MoveZero { place }` instruction
(`instructions.rs:306-308`): `Move` records the *transfer*, `MoveZero`
records the *source-zero*. The zero is a backend optimization for drop
correctness — emitted only when drop-tracking would otherwise re-drop the
moved value, elided when liveness proves the source is unobservable. It is
not part of the move semantics (see `CLAUDE.md` "Ownership at Consuming
Positions").

## Per-local ownership state (`LocalOwnership`)

The local-axis consolidation (Phase D) puts ownership on the GIR `Local`
itself rather than in sidecar maps. The `Local` struct
(`src/ir/mod.rs:663-689`) carries:

```rust
pub struct Local {
    pub type_id: TypeId,
    pub name_hint: Option<String>,
    pub ownership: LocalOwnership,   // the rich ownership state
    pub slot_kind: SlotKind,         // LIR slot layout/access
    pub is_owning_param: bool,       // `!`-sigil resource params
}
```

`LocalOwnership` (`src/ir/mod.rs:449-497`) is broader than the four
variants the design doc proposed; the current set is:

- `Untracked` (`#[default]`) — no ownership decision recorded yet. This is
  the load-bearing addition over the doc's design: it preserves the legacy
  "absent from the `FxHashMap`" semantics so readers like `is_owned_local`
  return `true` *only* when a setter explicitly wrote an owning state. Without
  it, retiring the old `local_ownership` map would have flipped every
  not-yet-decided local to `Owned` and silently registered spurious drops
  (`mod.rs:441-458`).
- `Owned` — owns its data, registered for scope-exit drop. Heap data may be
  shared (e.g. via Move from a non-fresh source).
- `FreshOwned` — owns *and* the allocation is provably fresh (no other local
  shares the buffer). Strictly stronger than `Owned`; set when a runtime
  callee returns a freshly-allocated `GorgetString` (`gorget_str_cat`,
  `gorget_string_format`, …). Used by the return-clone elision and the
  self-referential-reassign guard, both sound only when aliasing is excluded
  (`mod.rs:464-471`).
- `Borrowed { origin: BorrowOrigin, mutability: Mutability }` — does not
  drop; carries provenance (which root it borrows into) and shared-vs-unique.
- `View { source: BorrowOrigin }` — a runtime view (cap=0 string slice
  today); drop is a no-op until materialized; source mutation triggers the
  materialize.
- `SharedHeap { source: LocalId }` — the `String b = a` value-aliasing
  shape: the local IS its own 32-byte owned slot at runtime but its heap
  buffer is shared with `source`. Flushes to the same slot kind as `Owned`
  so ABI routing keeps the value layout, while still participating in
  source-mutation invalidation and return-path clone queries. This is the
  sole source of truth for `String b = a`; the legacy `string_borrow_sources`
  sidecar was retired in its favour (`mod.rs:481-491`).
- `MaybeOwned` — started borrowed, may have been materialized on some paths;
  conditional drop guard. Kept until Phase C makes it unreachable
  (`mod.rs:492-496`).

The three predicate methods on the enum encode the shipped contract:
`is_ref()` returns true for everything that's a Ptr at runtime —
`Borrowed`, `View`, `MaybeOwned` — and false for `Untracked`, `Owned`,
`FreshOwned`, `SharedHeap` (the last because it *is* owned at runtime, only
its heap data is shared); `is_owned()` is true for `Owned`/`FreshOwned`/
`SharedHeap`; `is_fresh()` only for `FreshOwned`
(`src/ir/mod.rs:499-533`). The pure-borrow distinction — a borrow that can
never be materialized and so must never be dropped — needs the local's own
id to detect self-rooted `Param(self)`/`Alias(self)` placeholders, hence
the `is_pure_borrow_for(self_id)` variant (`mod.rs:560-574`).

Because ownership lives on `Local`, it persists into the LIR: a single
typed field carries through the GIR→LIR boundary instead of being
recomputed per monomorphization. `SavedScope` captures it as a typed
`Vec<LocalOwnership>` for branch save/restore; the old `local_ownership:
FxHashMap` is gone (every setter writes `builder.locals[idx].ownership`
directly).

## Borrow provenance (`BorrowOrigin`)

When a local is `Borrowed` or `View`, the question downstream passes ask
is *"what does it borrow into, so I know which mutation invalidates it?"*.
That is `BorrowOrigin` (`src/ir/mod.rs:582-625`):

```rust
pub enum BorrowOrigin {
    Param(LocalId),               // param of the enclosing fn
    CollectionElement(LocalId),   // element of this collection local
    Field { base: LocalId, field: u32 },  // field of this struct local
    Alias(LocalId),               // transitive alias — resolve to root
    RuntimeView(LocalId),         // s.trim(), s[1..3] — borrows source's buffer
    FieldPath(String),            // multi-layer dotted path e.g. "self.data"
    CowBorrowPending,             // set_cow_borrow without a known source yet
    TupleElement { tuple: LocalId, index: u32 },  // for return-path MoveZero
}
```

The design rationale is worth
restating: `BorrowOrigin` is deliberately *not* a `Place` (LocalId +
projection path). A `Place` says where the borrow *points*; `BorrowOrigin`
says which mutation *triggers materialization* — a coarser concept. An
element borrowed via `outer.get(i).get(j)` is invalidated when `outer` is
mutated *or* when the inner vector is; collapsing that to a single Place
loses the structure. Each variant *is* the materialization trigger.
`FieldPath(String)` exists because a single `LocalId` can't represent a
multi-layer chain (`self.data.items`); `CowBorrowPending` is a placeholder
for deferred source resolution that should not survive once eager
source-propagation lands.

This is "the actual invention" the doc claims (§6.5): Rust puts
provenance in lifetime parameters in the *type*; Gorget chose to keep
lifetimes out of the user-visible language, which forces the compiler to
track provenance *somewhere*, and the only honest place is on the local.
`BorrowOrigin` is what buys "no lifetime annotations" without giving up
the safety guarantees.

### CoW materialization is one typed match

`cow_before_mutation` (`src/ir/lowering/context.rs:2658-2746`) is the
single entry point for CoW severance — called before any in-place mutation
(reassignment, mutating method, `&`-arg, `!`-arg transfer). It is now a
typed walk over `Local.ownership` with no hashmap walks or name-based
fallbacks. A leading bare-param clone-to-owned step (Phase 1c) precedes
six numbered cases that map one-to-one onto the
`BorrowOrigin`/`LocalOwnership` shapes: (1) `local` is an `Alias(s)` of
another local (clone the source in); (2) `local` is the source *of*
aliases (clone into each); (3) collection-element refs into a mutated
collection; (4) live `View`s of a mutated string; (5) `SharedHeap`
value-aliases (tag-invalidate only, heap already deep-cloned); and (6)
named field-borrows into a reassigned struct. The alias-source read, for
example, is a direct match:

```rust
match &builder.locals[idx].ownership {
    LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(s), .. }
        if *s != local => Some(*s),
    _ => None,
}
```

(`context.rs:2679-2682` — the `*s != local` guard rejects the
`set_ref` self-loop placeholder.)

### Slot kind — the LIR-facing view

`Local.slot_kind: SlotKind` (`src/ir/mod.rs:640-659`) is the
narrower LIR-facing projection of ownership: `Value` (slot holds the value
directly), `OwnedPtr` (slot holds a pointer the local owns — `borrow_mut`,
`Option[Ref[T]]::unwrap`), `BorrowedPtr` (slot holds a non-owning view
pointer — every borrow setter). `lower_place_addr` and the downstream LIR
readers route on `SlotKind`, not on the rich enum directly — this is the
D6-partial state: ownership flows GIR→LIR via `slot_kind`, but a typed
`Slot.origin: Option<BorrowOrigin>` for borrow-aware codegen has not yet
landed.

### `is_owning_param` — distinguishing `!` from `&`

Both `&` and `!` resource parameters share the same ownership shape
(`Borrowed { Param(self), Unique }`) and the same `BorrowedPtr` slot kind
for read-site routing — they're both MutPtrs for ABI uniformity. The one
typed bit that distinguishes them is `Local.is_owning_param`
(`mod.rs:677-688`), set only for `!`-sigil params by `set_owning_param`
(`context.rs:2243-2248`). A `!` param owns its pointee and must drop it at
function exit unless ownership is transferred onward; `lower_drop` reads
the flag to emit the deref-aware drop sequence instead of the
`is_pure_borrow_for` Nop. This is a textbook instance of the "no name
matching" rule: the distinction is a typed bit at the writer, not a shape
heuristic at the reader.

## The clone-vs-move-vs-borrow decision tree

There are two distinct decision sites, because the two questions are
independent: *consume vs borrow* is a
position-class question; *clone vs move* (within a consume) is a
liveness/ownership question.

### At a variable declaration: `lower_var_decl_assign_mode`

`String b = a` is a consume position; the question is whether to
value-alias, clone, or move. `lower_var_decl_assign_mode`
(`src/ir/lowering/stmts/mod.rs:1081-1273`) is the seven-branch tree that
answers it. It is the cleaned-up descendant of the "smoking gun" ~100-line
predicate-soup the design doc cited;
every branch now reads typed predicates (`source_live`, the source's
typed `LocalOwnership`) — the `is_named_local` proxy has been fully retired
from this function:

- **A** — Owned + live `GorgetString`, same type → the value-aliasing
  `String b = a` shape: `Borrow` mode + `set_shared_heap`, unregistering
  the source's drop (`mod.rs:1153-1160`).
- **B** — Owned + live source, non-resource type with a cross-type clone fn
  (e.g. `Str`→`GorgetString`) → emit clone, `Move` (`mod.rs:1170-1179`).
- **C** — live resource source, CoW-safe → create a `Ptr` alias instead of
  cloning: retype the local to `Ptr(T)`, `set_ref`, `emit_borrow`,
  `cow_register_alias`, drop-unregister (`mod.rs:1190-1209`). Source need
  *not* be `Owned` — transitive alias chains propagate here.
- **D** — live resource source, CoW-unsafe and *not* `Borrowed` → clone,
  `Move`. The `!Borrowed` bail routes `Borrowed` transitive aliases to
  E/F/G with correct behaviour instead of a clone-failure→safety-net-Move
  chain (`mod.rs:1224-1232`).
- **E** — `View` source, same-type `GorgetString` → clone-to-owned, `Move`
  (`mod.rs:1235-1241`).
- **F** — dead source + droppable type → `Move`. Two typed clauses cover
  unnamed temps / named-at-last-use, and Option/Result wrapper targets
  (`mod.rs:1256-1262`).
- **G** (safety net) — still `Copy` but target is a resource → `Move`
  (`mod.rs:1268-1270`).

The comments on branches A/B/C/D record *why* each typed predicate
replaced its proxy, including the fixtures that regressed when a naive
substitution was tried (e.g. naive `is_named_local` removal in branch C
SIGSEGVed 50+ fixtures because unnamed temps die at end-of-statement,
leaving a dangling Ptr alias — the `source_live` predicate excludes them).
That history is exactly the "don't redesign around a gap; verify the
premise" discipline in `CLAUDE.md`.

### At a call argument: callee-driven dispatch

The key asymmetry: **bare function
call arguments are NOT a materialization point.** A bare `f(x)` propagates
the caller's `Ptr` alias to the callee at zero cost — no clone, no move.
Clones happen only at the ownership boundaries where the destination must
own (assignment, mutating method, struct/enum/tuple init, collection put,
return, move transfer, field store, closure capture) — and "boundary" is
structural, not a fixed numbered list: it is wherever the two materialization
helpers (`ensure_owned_at_boundary` / `ensure_owned_at_consuming_arg`) are
invoked (see Chapter 11's "enforced boundary set" callout — any doc quoting a
closed count is stale). And the dispatch is **callee-driven**: the per-arg mode is
chosen by looking up the *callee's* parameter declaration, not the
caller's syntactic sigil. The caller's `&`/`!`/bare sigil is an opt-in
annotation; sigil parity is not required.

`lower_call_arg` (`src/ir/lowering/exprs/calls.rs:17-…`) implements this.
It looks up the callee's `ParamABI` (`fn_param_abis`, the single source of
truth) and only falls back to type-based derivation for extern/runtime fns
(`calls.rs:26-49`). For a bare arg whose callee passes by pointer, it
emits a const `Ptr` via `emit_borrow` by default, or a `MutPtr` via
`emit_borrow_mut` when the callee's param ownership is `Move` — and
crucially it does **not** route bare call args through
`ensure_owned_at_consuming_arg` (`calls.rs:187-256`). For an explicit
`^name` where `name` is a `!`-sigil resource param (`is_owning_param`), it
severs CoW aliases, forwards the pointer, and schedules a post-call
`MoveZero` on the param slot (`calls.rs:90-108`).

The consuming-position helper, `ensure_owned_at_consuming_arg`
(`src/ir/lowering/context.rs:1927-2007`), is what the *actual*
materialization points call (collection `push`/`put`/`set`, index-assign
sugar). Its rule is the canonical decision tree:

1. `Ptr(T)` borrow → clone through the pointer, always
   (`context.rs:1942-1955`).
2. by-value resource, expression temp (non-identifier) → last-use by
   construction, **no clone** (caller `MoveZero`s after the call)
   (`context.rs:1992-1995`).
3. by-value resource, named identifier → clone iff it's a borrow
   (not drop-registered / bare param / ref-local / cow-borrow) **or** not
   the last use; otherwise move (`context.rs:1965-1991`).

The companion `ensure_owned_at_boundary` (`context.rs:1756-1908`) handles
non-arg boundaries (returns, var bindings, struct-field init) and adds
case 0 for resource-typed module globals — a `String DT_LOCAL = "literal"`
global must be cloned through `GlobalRefPtr` at an ownership boundary or
the consumer's scope-exit drop frees the global's buffer and the next read
double-frees it.

### Method receivers

For a method call, the receiver is dispatched on the method's `self`
declaration (param index 0). `has_consuming_self` reads
`fn_param_ownerships[name][0]` and is true iff `self` is declared `^self`
(`Ownership::Move`) (`src/ir/lowering/exprs/methods.rs:1757-1761`); a `^self`
method gets a post-call `MoveZero` on the receiver, a `&self`/bare-`self`
method leaves the receiver live. This closed the "hardcoded `OpMove`
without callee-signature consultation" class.

### Liveness

The clone-vs-move decision needs "is this the last use of the source?".
That comes from a backward AST liveness pass,
`compute_function_liveness` (`src/ir/lowering/liveness.rs:42`),
producing `last_use_spans: FxHashMap<usize, String>` — each last-use span
start mapped to the *name* it is the last use of, because the query is
"is this use of `name` its last use", which a position-only set cannot
answer. It is read by `is_last_use_at` (`context.rs:1389`, conservative —
returns false when liveness data is absent) and the higher-level
`source_live_past` (`context.rs:1420`). The pass counts *every* identifier
read as a use regardless of mode (borrow or consume). That conservatism is
*semantically correct*, not a limitation: the borrow checker (Pass 5,
Chapter 10) forbids borrow-after-move at compile time, so a consume at
position P that has any later read — borrow or consume — is correctly not
a last use.

#### Loops: the back-edge is not visible to a single backward walk

A backward walk over straight-line code sees every future read before it
reaches the use it is judging. A **loop** breaks that: the body's *first*
statement is followed, at runtime, by the body's *last* statement on the
next iteration, and a backward AST walk reaches the first statement with
nothing after it. So a name the body reads on every iteration looks dead
on the iteration being judged, and the consuming position moves it — once
per iteration, out of one slot.

Every loop-shaped form therefore takes **two passes** through one helper,
`walk_loop_two_pass`. Pass 1 walks the body to collect the set that is
live *at the back edge* and throws its last-use decisions away, because
those decisions are the ones that cannot see the back edge. Pass 2 walks
the body again against that loop-propagated set and records the decisions.
Pass 2 **unions** its result into the enclosing live set rather than
overwriting it: a body can run zero times, or a `break` can skip past a
kill, so a kill inside the body must not delete a name that is live before
the loop.

The header splits by *execution frequency*, not by syntax. Anything
re-executed per iteration — a `while` condition, a `for`'s pattern
re-binding, a comprehension's `if` filter — is walked inside both passes,
after the body, since a backward walk visits it after what it guards.
Anything evaluated once before the loop starts — the `for` iterable, the
comprehension iterable, a `meta for`'s range — is walked by the caller
after the two passes, against the post-loop live set.

**A comprehension is a loop.** `[e for x in xs]` executes `e` once per
element exactly as `for x in xs: acc.push(e)` does, and its accumulate is a
consuming position exactly like `push`, so it takes the same two passes —
as do `while`, `for`, `loop`, and the compile-time `meta for` / `meta
while`, whose bodies are real code that liveness sees before meta
expansion runs. Seven arms, one helper: a per-arm copy of this dance is
how the comprehension arms drifted away from their statement siblings in
the first place, so the count is held by
`liveness_loop_back_edge_single_source` (`tests/lints.rs`). A count of
call sites alone cannot see the regression that matters — a *new* arm
hand-rolling the dance beside the helper rather than calling it — so the
guard also counts the dance's own ingredients, ending at the one no
spelling can shed: two passes over the live set need two clones of it,
so a second dance is visible in the `live.clone()` count however it is
written.

## The Phase C validator — shallow copy of a resource is fatal

Phase D made Phase C tractable: with an authoritative per-local ownership
field and one shared `ReadMode`, the validator is one rule applied at
every read site instead of six per-instruction paths. `validate_read`
(`src/ir/validate.rs:1367-1378`) is that rule:

```rust
fn validate_read(site: ReadSite<'_>, registry: &TypeRegistry) -> Option<ResourceMoveWarning> {
    if !registry.is_resource_type(site.source_ty) { return None; }
    match site.mode {
        ReadMode::Borrow | ReadMode::Move | ReadMode::Clone => None,
        ReadMode::Copy => Some(ResourceMoveWarning { /* ShallowCopyOf… */ }),
    }
}
```

A `Copy`-mode read of a resource type is the one rejected case. The walker
`for_each_read_site` (`validate.rs:1419-1560`) extracts the conceptual read
site from four instruction shapes (`FieldLoad`, `IndexLoad`,
`EnumFieldLoad`, `Call`/`CallExtern` args) and feeds the one rule;
`warning_kind_for` (`validate.rs:1383-1410`) fans out the per-class
diagnostic label. The `Assign { mode: Copy }` class does *not* flow
through this walker — it is handled by a separate extractor,
`assign_read_site` (`validate.rs:1119`), whose own auto-deref skip logic
doesn't fit `for_each_read_site`'s instruction-discriminant match (see the
doc comment at `validate.rs:1412-1418`, which still names the long-renamed
`check_resource_moves` — the live standalone fn is `validate_resource_moves`
at `validate.rs:995`, and the bucket-population path uses the inline
`assign_read_site` extractor).

These run during GIR lowering and are **fatal**:
`validate_resource_sites_all` collects five class buckets (`assign`,
`call_args`, `index_reads`, `enum_reads`, `field_reads`) — the latter four
populated by the `for_each_read_site` walk, the `assign` bucket by the
inline `assign_read_site` extractor (`validate.rs:1076-1084`) — and each non-empty bucket
`panic!`s the compile with a class-specific `[resource-…]` message
(`src/ir/lowering/mod.rs:1628-1670`). A separate consume-site validator,
`validate_consume_sites` (`validate.rs:2400-…`), enforces the consuming
positions; it reads the typed `Module::consume_externs` registry (plus the
runtime collection mutators) to recognise consuming runtime calls
(`validate.rs:2974-2994`) rather than name-matching. The net guarantee:
**the IR cannot express a shallow alias of an owned resource** — which is
exactly what the resource-move validators exist to guarantee.

## Move-vs-borrow on enum payload extraction

`EnumFieldLoad` is the one read that kept its own two-case mode,
`EnumFieldLoadMode { Move, Borrow }` (`instructions.rs:133-137,
278-284`), because it carries a narrower semantics tied to a specific bug
class ("Snag #34"). When `emit_pattern_bindings` extracts a resource
payload that the binding takes ownership of, it uses `Move` — which zeros
the source's payload field at LIR, preventing shallow-copy double-free.
When `lower_pattern_condition` inspects a *nested* constructor's tag and
payload to evaluate a match condition *without* destroying the scrutinee,
it uses `Borrow` — skipping the source-zero, so the subsequent
`emit_pattern_bindings` re-reads the same un-zeroed source. Without the
split, the condition test's destructive read zeros the payload and the
binding sees zeros — the surface symptom was "`Dict[String, NonCopyEnum]`
silently drops mutations." The mode is the typed metadata that lets the
condition test and the binding share a scrutinee safely.

## Reference instructions: `LoadRef` / `StoreRef`

`LoadRef { dst, src }` and `StoreRef { dst, value }`
(`instructions.rs:176-186`) are the explicit Ptr-dereference instructions:
`LoadRef` loads the `T` value by dereferencing a `Ptr(T)`-typed local;
`StoreRef` writes a value through a `Ptr(T)`. The two have diverged in how
far they've landed:

- **`LoadRef` is emitted.** The GIR lowering layer produces it at eight
  sites (`exprs/methods.rs:87,484,532,1451`; `exprs/mod.rs:1592,1944`;
  `exprs/calls.rs:577`; `exprs/operators.rs:23`, all via
  `FunctionBuilder::load_ref`), replacing the implicit auto-deref for
  unique-borrow (`&`/`!`) parameters that the older lowering threaded
  through ad-hoc sidecars.
- **`StoreRef` is defined and LIR-handled but not yet emitted by
  lowering.** It exists in the instruction set (`instructions.rs:183`) and
  the LIR backend lowers it (`src/lir/lower/insts.rs:1531`), but the GIR
  lowering layer has *no* `store_ref` call sites. Write-back through a
  pointer in mutable captures still uses the older mechanism: an `Assign`
  whose place carries a `Projection::Deref` (e.g.
  `src/ir/lowering/stmts/mod.rs:827,1740,1902,2902,3027`;
  `exprs/mod.rs:148,457,2034`). So `StoreRef` has *not* replaced the
  Deref-projection write-back — that path is still live.

(The folded `ownership-ir.md` lists both as "future"; that is stale for
`LoadRef` — it is an emitted instruction in the live IR — and partly so
for `StoreRef`, which is defined and LIR-handled though not yet emitted.)

## In the self-host

The self-host lowerer (`tests/fixtures/self_host_lowerer/`) mirrors this
ownership machinery closely — it is one of the more faithful Rust-parity
ports in the self-host. The GIR types in `gir.gg` are direct analogues of
the Rust enums, with comments citing the Rust file:line they mirror:

- `enum LocalOwnership` (`gir.gg:150-155`): `LoOwned`, `LoBorrowed`,
  `LoView`, `LoParam`, `LoMaybeOwned` — a five-variant analogue of the Rust
  enum (note: the self-host does not yet carry the Rust `Untracked` /
  `FreshOwned` / `SharedHeap` refinements).
- `enum Operand` (`gir.gg:51-69`) carries the four read modes as operand
  variants: `OpCopy`, `OpMove`, `OpClone`, `OpBorrow` — the analogue of
  Rust's `ReadMode`.
- `enum BorrowOrigin` (`gir.gg:212-221`): `BoNone`, `BoParam`,
  `BoCollectionElement`, `BoField`, `BoRuntimeView`, `BoAlias`,
  `BoFieldPath`, `BoTupleElement`, `BoCowBorrowPending` — the analogue of
  Rust's `BorrowOrigin`, with the same provenance variants. The payloads on
  the four original variants are present but several remain UNUSED at the
  current commit (callers pass sentinel `-1`); the writer sites that fill in
  real source ids are still being wired (`gir.gg:204-211`). This is a TODO
  item, not chapter material — it is the self-host's tail of Phase D6.
- `struct GirLocal` (`gir.gg:223-227`) carries `ownership` and
  `borrow_origin` as typed fields, the analogue of putting state on `Local`.

The decision tree is in `lower.gg`. `enum ConsumeKind` (`gir.gg:167-184`)
is a typed materialization-point taxonomy: four consume positions
(`CkAssign`, `CkReturn`, `CkCallArgOwning`, `CkFieldWrite`) and four borrow
positions (`CkCallArgBorrow`, `CkBinOpArg`, `CkFormatArg`, `CkMatchPtr`).
`op_consume` (`lower.gg:1389-…`) is the operand-mode dispatcher: at a
non-consume kind it returns `OpBorrow` unconditionally; at a consume kind
it dispatches on the source's `LocalOwnership` — a `Ptr(T)`-at-consume
source routes through `decide_ptr_consume`, a non-resource source is
`OpCopy`, and for a by-value resource source the match
(`lower.gg:1465-1471`) returns **`OpClone` for `LoBorrowed` *and*
`LoView`** (a borrow at an owning destination must materialise an
independent copy — returning `OpBorrow` there would shallow-alias the
borrow's buffer and double-free, the bug the leading comment at
`lower.gg:1450-1464` guards against) and `OpMove` in the `else`
(`LoOwned`/`LoParam`/`LoMaybeOwned`).
`classify_call_arg(&gmod, fname, arg_idx)`
(`lower.gg:1357-1365`) is the callee-driven classifier — it returns
`CkCallArgOwning` iff the callee's `arg_idx`-th param is `!T` (looked up in
`GirModule.fn_move_params: Dict[String, Vector[bool]]`), else
`CkCallArgBorrow`, the exact analogue of Rust's `lower_call_arg` switching
on `param.ownership`. Method receivers dispatch through a distinct site
(`lower.gg:4874-4885`), not through `op_consume`'s general by-value match:
a `LoBorrowed` receiver passes `OpBorrow(recv)` directly (4880) — moving
or cloning a borrowed receiver that names a real element in place would be
wrong — while every other receiver ownership goes through `op_consume` on
`classify_call_arg(&gmod, full_name, 0)`, so `^self` → `CkCallArgOwning` →
`OpMove` and `&self`/bare `self` → `CkCallArgBorrow` → `OpBorrow`. The
receiver-dispatch fix — replacing a hardcoded `OpMove` with
callee-signature consultation — has shipped.

One divergence to note in the self-host's LIR layer (`lower_operand`,
`lir_lower.gg:2403-2533`): `OpCopy`, `OpMove`, and `OpBorrow` all lower to
a plain `ISlotLoad` (`lir_lower.gg:2439-2452,2522-2532`), but **`OpClone`
already emits a real runtime clone call** — `resource_clone_fn` picks the
matching `gorget_*_clone` / `T__clone` symbol and the handler emits an
`ICallExtern` against it when the source slot holds (value-typed slot,
via `ISlotAddr`) or points to (Ptr-to-known-struct slot, via the loaded
pointer) a resource (`lir_lower.gg:2453-2521`). So the "Phase 2c emission
flip" for `OpClone` has partly landed — clone-on-borrow and Ptr-to-resource
sources materialise end-to-end; raw `LT_PTR` slots with no pointee-struct
signal still fall through to a plain load, and `OpMove`'s move-zero
codegen is the remaining deferred piece. The in-tree comment at `lower.gg:1338-1342`
still claims all four modes produce no clone codegen; that comment is
itself stale.

**Parity is a procedure, not a number.** The `lowerer_comparison` test is
diagnostic-always-pass — a green run asserts nothing about parity. To read
the current fn-shape match, run:

```
cargo test --test integration lowerer_comparison -- --nocapture
```

and read the printed matched-count. The relevant front-end comparison
tests (`resolver_comparison`, `parser_comparison`, `type_comparison`,
`lowerer_comparison`) all follow the same diagnostic-only pattern; never
quote a fixed parity figure from memory — re-derive it from the
`--nocapture` output.

## See also

- Chapter 11 — Copy-on-write & view provenance (the *when*; this chapter
  is the *what's recorded*).
- Chapter 10 — the safety checker (Pass 5), which forbids borrow-after-move
  at compile time and is *why* the liveness conservatism here is correct.
- Chapter 12 — GIR & lowering generally.
- `CLAUDE.md` "Ownership at Consuming Positions" — the user-facing compiler
  contract this metadata implements.
- [Chapter 11](11-copy-on-write.md) (materialization points) — the
  materialization-point spec (note any closed "seven" enumeration is stale;
  see Chapter 11's "enforced boundary set" callout — boundaries are structural,
  driven by the two materialization helpers, not a fixed count).
- The former `unified-resource-model.md` deep-dive — §3 (Phase A, type axis),
  §5 (Phase C, validator), §6 (Phase D, local axis) — has been folded into
  this chapter (and chapters 15/18/25).
- The canonical clone-vs-move-vs-borrow decision tree and the sigil asymmetry
  (folded here from the former `clone-emission-at-calls.md`) — see the
  decision-tree section above.
