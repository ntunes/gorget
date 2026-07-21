# Unified Resource Model and LIR Correctness Roadmap

> **Status:** Proposed (2026-04-28). Revised 2026-05-01 to add Phase D (the IR-side counterpart to Phase A), §9 (sequencing alongside self-host + contract evolution discipline), and to merge in the LIR correctness roadmap — folding overlapping items into Phases A/C/D and adding Tier E for residual LIR-only hygiene work. Revised same day to **defer Phase B entirely** to a documented fallback (see §4) — the cost-benefit reads net-negative once Phase C is on the road. This document supersedes `lir-correctness-roadmap.md`.
> **Authors:** opus-4.7 session.
> **Builds on:** `ownership-ir.md`, `copy-on-write.md`, `safety-checker.md`, `layering-discipline.md`.
> **Supersedes once landed:** the parallel name-based lookup tables enumerated in §3.1; the 2026-04-12 "cap at field index 1" layout decision (replaced by "cap at field index 0" — see §4); the seven sidecar maps in `LoweringContext` enumerated in §6.1.

This document proposes a three-phase architectural change to make a recurring class of bugs — double-frees, use-after-free, and use-after-move on resource-typed values — *structurally impossible* rather than chased one fixture at a time. It also folds in the residual LIR-correctness work (Tier E) that doesn't touch resources directly but is part of the same "make the IR correct by construction" agenda.

The active plan is:

- **Phase A — Unified resource metadata.** Replace the parallel name-based lookup tables (`clone_fn_for_ptr`, `infer_drop_strategy`, `elem_drop_fn_for_*`, `needs_drop`, `is_resource_type`, `collection_runtime_type`, `c_sizeof_with_structs`, …) with a single `ResourceMetadata` struct attached to every resource type at registration time. Includes the typed runtime function table (`RuntimeFn` enum + signature data — `RuntimeFn` already shipped, the declaration table extends it). All consumers read from one accessor. Adding a new resource type or runtime fn touches one declaration site. *Type-axis consolidation.*
- **Phase D — Local-state consolidation and borrow provenance.** The IR-side mirror of Phase A. Replace the seven parallel sidecar maps in `LoweringContext` with a single typed `LocalOwnership` field on every `Local`, including a first-class `BorrowOrigin` that persists from GIR through LIR. Subsumes the LIR roadmap's per-value origin metadata refactor — same pattern at the LIR layer. *Local-axis consolidation.* Without this, Phase C is intractable.
- **Phase C — Strict move/clone validation.** Make every read of a resource-typed value an explicit `Move` / `Clone` / `Borrow`. Reject any IR that produces a shallow alias of an owned resource. Plugs into the LIR's per-pass validator framework (Tier E §8.3) — Phase C is one validator entry; the framework hosts it and other invariant checks.
- **Tier E — LIR-side correctness and shape work.** Independent items from the LIR audit: drop-flag dataflow init, critical-edge splitting + post-SSA invariants, validator-runs-after-every-pass, optimizer fixpoint, typed `LirType::FuncRef`. Independent of A/D/C; runs in parallel.

**Deferred fallback (not on the active road):**

- **Phase B — Universal view/owner discrimination at runtime.** Originally proposed as a runtime safety net while Phase C was being built. Deferred 2026-05-01 because the permanent ABI cost (`Box[T]` 8→16 B, `GorgetClosure` 16→24 B, `Task[T]` 16→24 B) is decisively net-negative once Phase C is on the road. Documented in §4 as a complete design so a future contributor doesn't have to re-derive it if Phase C ever stalls and the runtime backstop is genuinely needed.

The phases compose: A and D are refactors that unblock C (A on the type axis, D on the local axis); C is the compile-time guarantee that the bug class can't recur. Tier E proceeds independently throughout.

> **Headline status (2026-05-12):**
> - **Phase D — SHIPPED.** D1–D5 + D4.5 all in. `local_ownership: FxHashMap` retired (every setter writes directly to `Local.ownership`; `SavedScope` captures typed `Vec<LocalOwnership>`). `ReadMode` unification shipped (D5); `AssignMode = ReadMode` type alias. D6 partial (BorrowOrigin flows GIR → LIR via `slot_kind` but no typed `Slot.origin` yet). Residual: Tier 3b proxy-read ratchet (BUDGET=77, cosmetic).
> - **Phase C — SHIPPED.** `validate_resource_moves` + the read-site quartet + Tier 2a `validate_consume_sites` (with typed `consume_externs` registry promoted 2026-05-12) all fatal. Shallow copy of a resource is a compile-time error.
> - **Phase A — PARTIAL.** `TypeMetadata` carries the field-set Phase A specified; Tier 1c locks coherence-at-construction. The big unshipped piece is §3.6's `RUNTIME_DECLS` / `resources.toml` build-tooling pipeline (~3-4 weeks).
> - **Tier E — ongoing in parallel.** Validator framework, drop-flag hygiene, SSA invariants all shipped piecemeal.

**Already shipped** (subsets of Phase A, delivered by the LIR audit): `Inst::CallRuntime` + `RuntimeFn` enum (LIR A1/A2 — typed runtime call boundary), `Inst::CollectionCtor` (LIR A3 — typed collection ctor with `ElemMeta` replacing `original_name` parsing in three downstream passes). What's left of Phase A is the GIR-side type-metadata consolidation (substantially done — see Tier 1c) and the runtime declaration table extension.

---

## 1. The recurring failure pattern

Pull the SECURITY-tagged TODOs from the last six months together:

| Date | Bug | Root |
|---|---|---|
| 2026-04-28 | `Vector[Callable]` SEGV (attack_82) | `infer_collection_element_type` Vector branch missing Callable; LIR `Named("Callable__…")` mapped to `Ptr` instead of `Struct`; CallExtern path didn't wrap closures into GorgetClosure. |
| 2026-04-28 | `Vector[Callable]` env leak | `elem_drop_fn_for_*` (two parallel tables) didn't have Callable arms. |
| 2026-04-28 | `Ref[Callable].clone()` shallow copy | `clone_fn_for_ptr` didn't recognise Callable. |
| 2026-04-28 | httpserver_middleware double-free | `wrap_single_closure_arg` didn't deep-clone pre-packed Callable sources. |
| 2026-04-28 | httpserver_before SEGV | `clone_fn_for_collection_element` (LIR IndexLoad) didn't have Callable arms. |
| 2026-04-28 | `Dict[K, Callable]` double-free | `Dict.get()` returns by-value; intermediate Callable shares env with slot; both drop. |
| 2026-04-28 | sec_85 dict-struct-key hang | Hash-fn callback dispatch trips ASan stack-redzone. |
| 2026-04-28 | `__gorget_drop_fn` UBSan trip | Function-pointer types don't round-trip through `void(*)(void*)`. Partial fix; user `T__drop` still latent. |
| 2026-04-27 | `Box[T]` doesn't drop | Drop wrapper isn't registered with auto-drop machinery. |
| 2026-04-24 | match-expression drops Some-arm | Result-slot handling forgets the Some-arm value. |
| 2026-04-23 | `Option[Shared[T]]` drop pointer-type warning | Variant payload typed `void*` loses inner type. |
| 2026-04-23 | Mutex double-lock deadlock | Borrow checker doesn't track live `Guard`. |
| 2026-04-22 | `Dict[Point, String]` ASan hang | Key-eq callback ABI mismatch. |
| 2026-04-22 | Vector grow pointer invalidation | Borrow across `.push()` not detected. |

Patterns (each row above falls into one or more):

1. **Lookup-table drift.** A new resource type or new pattern is added; one of the ten parallel tables is forgotten; the bug surfaces when an unusual fixture exercises the missed path. Caught only because httpserver/Dict/match-expression happens to use it.
2. **Two parts of the pipeline disagree about a value's size, ABI, or ownership.** elem_size says 8, struct says 16, codegen does memcpy with the wrong number → SEGV.
3. **Shallow copy across an ABI boundary.** Both ends think they own the value, both drop it, double-free / UAF.

The first pattern is a *refactoring* problem. The second is a *single-source-of-truth* problem. The third is a *type-system* problem. Each phase below targets one.

---

## 2. What's already there

We aren't starting from scratch. The codebase has partial implementations of all three ideas:

**Toward Phase A (TypeDef metadata):**
- `TypeMetadata` already has `drop_strategy`, `copy_semantics`, `clone_fn`, `collection_kind`. Used in `needs_drop`, `is_resource_type`, and a few other places.
- Problem: every consumer that reads it *also* has a name-based fallback path "for types without TypeDefs", and those fallbacks have drifted. `infer_drop_strategy` and `clone_fn_for_ptr` each have their own list of `if name.starts_with("Vector__") …`. They mostly agree, but every new resource has to be added to each independently.

**Toward Phase B (view/owner):**
- `String` (`GorgetString`) already does it: `cap == 0` ⇒ view, `gorget_string_free` no-ops on views. Source: `src/backend/c/c_runtime.rs`.
- `GorgetArray` partially does it: views exist (cap=0 + len>0), but `gorget_array_free` doesn't no-op on them — it always frees `data`. View-arrays survive only because `data` is always NULL on a view.
- No other resource has any view discipline. `GorgetClosure` doesn't. `GorgetMap` doesn't. User structs don't.

**Toward Phase C (linear/affine typing):**
- `OwnershipState` enum on locals (`Owned`, `Borrowed`, `Ref`, `Param`).
- `AssignMode` enum on `Assign` instructions (`Copy`, `Move`, `Clone`, `Borrow`).
- `MoveZero` instruction signals ownership transfer.
- The borrow checker (Pass-5a) catches dangling references at function boundaries.
- DropAccountant tracks scope-exit drops.
- *Gap:* The IR allows shallow-copy `Assign { mode: Copy }` of resource-typed locals where it should require `Move` or `Clone`. `Param`-bound locals can be assigned to other locals via shallow copy. Field projections of resource structs return values without an explicit `Borrow` mode in many places. The validator doesn't reject any of these.

So the bones of all three phases exist — the work is consolidation (A), generalisation (B), and tightening (C).

---

## 3. Phase A — Unified resource metadata

### 3.1 The parallel lookup tables today

For just *one* resource type (Callable), these are the lookup paths I touched in the deep-clone landing:

| File | Function | What it answers |
|---|---|---|
| `src/ir/types.rs` | `is_resource_type` | "Should the borrow checker treat this as Move-only?" |
| `src/ir/types.rs` | `is_collection_type_name` | "Is this a Vector/Dict/Set?" |
| `src/ir/types.rs` | `needs_drop` | "Should `register_local` register this for scope-exit drop?" |
| `src/ir/types.rs` | `needs_param_drop` | "Should params of this type get registered too?" |
| `src/ir/lowering/context.rs` | `clone_fn_for_ptr` | "What runtime fn deep-clones this from a Ptr?" |
| `src/lir/lower/drops.rs` | `infer_drop_strategy` | "What runtime fn drops this (LIR fallback)?" |
| `src/lir/lower/drops.rs` | `lower_drop` (FnPtr arm) | "What strategy applies to bare FnPtr?" |
| `src/lir/lower/types.rs` | `c_sizeof_with_structs` | "What's the byte size of this in the C ABI?" |
| `src/lir/lower/types.rs` | `elem_drop_fn_for_type` | "What drops a slot-element of this type?" |
| `src/lir/lower/types.rs` | `elem_clone_fn_for_type` | "What deep-clones a slot-element of this type?" |
| `src/lir/lower/calls.rs` | `clone_fn_for_collection_element` | "What clone-on-read fn for IndexLoad?" |
| `src/lir/lower/mod.rs` | `collection_runtime_type` | "What runtime struct does this Named type alias to?" |
| `src/lir/lower/mod.rs` | `map_gir_type_with_structs` | "What LirType does this lower to?" |
| `src/backend/c_lir/helpers.rs` | `elem_drop_fn_for_c_type` | "Same as above but at C-codegen time." |
| `src/backend/c_lir/helpers.rs` | `elem_clone_fn_for_c_type` | "Same." |
| `src/backend/c_lir/helpers.rs` | `elem_materialize_fn_for_c_type` | "What materialises a view to owned (CoW)?" |

Sixteen lookup sites. Each has a slightly different `if name.starts_with(…)` chain. **Adding a resource means touching all sixteen.** Forgetting one is the mechanical source of half the bugs above.

> **Already partly retired (LIR audit, shipped).** `Inst::CollectionCtor` (the typed collection-constructor instruction) replaced `original_name` string-parsing in three of these passes — `wire_collection_bridges`, `find_hashable_key_types`, `infer_collection_elem_fns` — with structured `ElemMeta::{Primitive, Resource, UserStruct, UserEnum}` reads. Same pattern as Phase A's `ResourceMetadata`, applied at the LIR layer. What's left is the GIR-side and C-emit-side consolidation: roughly 12-13 sites still name-match.

### 3.2 The proposed schema

One struct, attached to every resource type at registration:

```rust
pub struct ResourceMetadata {
    /// Stable name used for runtime fn naming (e.g. "GorgetString", "GorgetArray").
    pub runtime_name: &'static str,

    /// Byte size in the C ABI. Authoritative — c_sizeof_with_structs reads this.
    pub size: u32,

    /// Alignment.
    pub align: u32,

    /// LirType the GIR Named lowers to. `Struct(_)` for aggregates,
    /// `Ptr` for opaque handles. Authoritative — map_gir_type reads this.
    pub lir_type: LirTypeRef,

    /// Drop function (single-arg, `void(*)(void*)`). Always present for
    /// resources; trivial types use `None`. Authoritative for needs_drop,
    /// infer_drop_strategy, lower_drop, and the elem_drop_fn_for_* tables.
    pub drop_fn: &'static str,

    /// Deep-clone-from-pointer function (`T(*)(const T*)`). Authoritative
    /// for clone_fn_for_ptr and clone_fn_for_collection_element.
    pub clone_fn: &'static str,

    /// In-place clone (`void(*)(void*)`) for collection elem_clone slots.
    /// Defaults to a generated wrapper around clone_fn.
    pub clone_inplace_fn: &'static str,

    /// CoW materialise function (`void(*)(void*)`) — view → owned in place.
    /// `None` if the type has no view/owner distinction (opaque handles).
    pub materialize_fn: Option<&'static str>,

    /// True if the type participates in the universal view-discriminator
    /// scheme (§4): first 8 bytes == 0 ⇒ view, otherwise owner. Almost
    /// every Gorget resource sets this. False for opaque handles whose
    /// layout we don't control (FFI-shaped types like `int64_t`-sized
    /// runtime handles).
    pub has_view_header: bool,

    /// On `.get(i)` from a collection of this type, do we return
    /// `Option[Ref[T]]` (borrow) or `Option[T]` (by value)?
    /// Authoritative — Vector, Dict, Set should all return Borrow.
    pub on_get: GetReturnConvention,

    /// Marker for the borrow-checker / drop accountant.
    pub copy_semantics: CopySemantics,

    /// Marker for collection methods (push/set/put/etc.) — what ABI tag
    /// the runtime fn expects at the element parameter position.
    pub elem_abi: AbiKind,
}

pub enum GetReturnConvention {
    /// `coll.get(i)` returns `Option[Ref[T]]`. Caller .clone()s for ownership.
    /// (Vector today; Dict/Set should join after Phase A.)
    Borrow,

    /// `coll.get(i)` returns `Option[T]` (by value). Trivial types only —
    /// resource types in this mode are unsound (see Dict[K, Callable] bug).
    /// Phase A migration step: flip every resource collection to Borrow.
    Value,
}
```

The earlier draft of this doc had a `ViewScheme` enum with three variants
(`AlwaysOwned`, `SentinelField`, `LeadingHeader`) to accommodate per-type
discriminator placement. After the §4 convergence — every resource starts
with the discriminator at offset 0 — `ViewScheme` collapses to a single
boolean (`has_view_header`). One rule everywhere: **first 8 bytes == 0
⇒ view, otherwise owner**.

### 3.3 The accessor

A single function — every consumer goes through it:

```rust
impl TypeRegistry {
    pub fn resource_metadata(&self, type_id: TypeId) -> Option<&ResourceMetadata> {
        self.get(type_id)
            .and_then(|gir_ty| self.resolve_resource_metadata(gir_ty))
    }
}
```

The 16 lookup sites become one-liners that read the relevant field:

```rust
// before:
if name.starts_with("Vector__") || name.starts_with("Deque__") || name == "GorgetArray" {
    Some("gorget_array_free".into())
} else if … // 8 more arms, slightly different rules in each table
```

```rust
// after:
ctx.type_registry.resource_metadata(tid).map(|m| m.drop_fn)
```

### 3.4 Registration

Each resource type registers its metadata at one canonical site. Built-in types (String, Array, Map, Set, Closure, Box, Task, …) register in a single table at `TypeRegistry::register_builtin_resources`. User-defined resource types (struct with custom drop, enum with resource payload variants) register at TypeDef-creation time.

Adding a new resource = appending one entry to the builtin table. The compiler error if a `ResourceMetadata`-required field is missing prevents drift.

### 3.5 Migration plan for Phase A

Stage A1: Define `ResourceMetadata` struct. No code changes elsewhere.

Stage A2: Populate the table for the existing built-in resources (String, Array, Map, Set, Closure, Box, Task, opaque handles). Each registration is a 10-line struct literal.

Stage A3: Replace one lookup site at a time with `resource_metadata(tid).map(|m| m.X)`. Run the full test suite after each. Order: easiest first (`c_sizeof_with_structs`, then the `elem_*_fn_for_*` pairs), hardest last (`is_resource_type`, `needs_drop`).

Stage A4: Delete the name-based fallback paths in each consumer once it reads exclusively from `ResourceMetadata`.

Stage A5: Add a `cargo test --lib` check that every TypeId returned as a resource by `is_resource_type` has matching metadata. Lock the invariant.

Estimated effort: 2 weeks. Risk: low — purely a refactor with the existing test suite as the safety net. Each stage is incremental and revertable.

### 3.6 Companion: the runtime declaration table

Phase A consolidates metadata for resource *types*. The same single-source-of-truth pattern applies to runtime *functions* — and the LIR audit's "RuntimeFn enum + signature table" (B1 in the old roadmap) is the natural extension.

`RuntimeFn` already exists (LIR A2, shipped) — an enum with ~80 variants and `c_name()` / `from_c_name()` / `signature()` accessors. What's left is consolidating the *full signature data* (param types, return types, side-effect markers) into one declarative table:

```rust
pub static RUNTIME_DECLS: &[RuntimeDecl] = &[
    RuntimeDecl {
        callee: RuntimeFn::ArrayNew,
        c_name: "gorget_array_new",
        params: &[CRuntimeType::Size],
        ret: CRuntimeType::GorgetArray,
        side_effects: SideEffects::Allocates,
    },
    /* ~80 entries */
];
```

The Rust struct `RuntimeDecl` shown above is the *deserialization target*, not the canonical source. The canonical source is `resources.toml` (§9.2), which carries both `[resource.X]` and `[runtime_fn.X]` sections; `build.rs` emits the const Rust data, the C runtime header, and the self-host Gorget form from the same TOML.

From the resulting `RUNTIME_DECLS` const, the C backend emits `extern` declarations, the LLVM backend emits `declare`s, the (future) WASM backend emits `(import …)` statements. The hand-written C runtime header is auto-generated from the same source, so the runtime and the frontend can never disagree on a signature.

Crucially, `RuntimeDecl.params` and `RuntimeDecl.ret` reference *resource types by their `ResourceMetadata` entry* — so any future layout change (e.g., a Phase B revival, §4) propagates automatically to runtime signatures. One source of truth covers both axes (types and functions).

Estimated effort: 3-4 weeks (the bulk of "what's left of Phase A"). Cost concentrates on the TOML schema design, the `build.rs` emitters, and the migration of the existing hand-written `runtime_extern_sig` function in `src/lir/lower/calls.rs`.

---

## 4. Phase B — Universal view/owner discrimination *(deferred fallback)*

> **Status: documented, not scheduled for implementation.** Phase B was originally proposed as a runtime safety net while Phase C (compile-time validation) was being built. The cost-benefit reads net-negative once Phase C is on the road: permanent ABI growth (`Box[T]` 8→16 B, `GorgetClosure` 16→24 B, `Task[T]` 16→24 B) for transitional protection that becomes redundant in steady state. Reviewed 2026-05-01: **deferred entirely**, not piecewise. The active plan bets on Phase C; this section preserves the design and rationale so a future contributor doesn't have to re-derive it if Phase C ever stalls and the runtime backstop is genuinely needed.

### 4.1 The original case

The bug class Phase B uniquely catches: shallow-copy double-free at runtime, regardless of whether the IR tracked ownership correctly. `String` already has this property (`gorget_string_free` checks `cap == 0` and no-ops on views) and the bug never bites for strings. Generalising the same pattern to every resource type would make shallow-copy double-free physically impossible at runtime — the runtime equivalent of Phase C's compile-time guarantee.

Concrete bugs it would close:
- `Dict[K, Callable]` double-free (view bit on `.get().unwrap()` result; slot stays owner).
- `routes.put(key, h)` double-free (slot owner, post-put `h` drop is no-op).
- `Vector[Callable]` env leak from intermediate locals.

Roughly half the SECURITY-tagged TODOs in §1's table fall into this class. **Phase D + Phase C close them at compile time** (the local-state consolidation makes the source-of-shallow-aliases impossible to express in the IR; the validator rejects what's left). Phase B is the runtime backstop if compile-time fails.

### 4.2 The design (if revived)

**One rule:** every resource type's first 8 bytes are the discriminator. `0` ⇒ view, non-zero ⇒ owner.

```c
static inline bool __gorget_is_view(const void* p) {
    return *(const uint64_t*)p == 0;
}
```

Layouts:

| Type | Today | If revived | Δ |
|---|---|---|---|
| `GorgetString` | `{data, cap, len, alloc}` 32 B (cap at offset 8) | `{cap, data, len, alloc}` 32 B (cap at offset 0) | reorder, same size |
| `GorgetArray` | `{data, cap, len, …}` 64 B | `{cap, data, len, …}` 64 B | reorder |
| `GorgetMap` / `GorgetSet` | cap at field index 1 | cap at field index 0 | reorder |
| `GorgetClosure` | `{fn_ptr, env}` 16 B | `{flags, fn_ptr, env}` 24 B | **+8 B (50 %)** |
| `Box[T]` | `T*` 8 B | `{flags, T*}` 16 B | **+8 B (100 %)** |
| `Task[T]` | `{task_ptr, drop_fn}` 16 B | `{flags, task_ptr, drop_fn}` 24 B | **+8 B (50 %)** |
| Opaque handles (Socket, Mutex, …) | bare `int64_t`-sized | unchanged | 0 |

Drop functions become uniform: every `*_free` checks `__gorget_is_view(p)` first; the body is otherwise type-specific (allocator, elem-drop callbacks, etc.). Post-drop, the function zeros the first 8 bytes so subsequent drops are idempotent no-ops.

### 4.3 Why deferred

- **Permanent ABI growth, transitional protection.** Box doubles, Closure +50 %, Task +50 %. Once shipped, can't be undone without another ABI break. Phase C makes the protection redundant in steady state — we'd be paying the ABI cost forever for a safety net that catches nothing once C is sound.
- **Forced lockstep window with self-host.** The cap-reorder + the prepend together require both compilers to agree byte-for-byte. The only synchronised cutover in an otherwise-parallel plan; eliminating it removes ~1 week of freeze.
- **Cosmetic uniformity, not real consolidation.** `__gorget_is_view(p)` is a 1-line save in front of still-type-specific drop bodies. Every type's drop function still has to know its own allocator, elem-drop callbacks, etc. The "uniform check" is real but small.
- **Identity signal.** Gorget's pitch is "Rust-grade safety without runtime tax." Pre-allocating 8 bytes per `Box[T]` because we don't trust the compile-time discipline is admitting we don't. Either Phase C is the destination or it isn't.
- **No piecewise rollout.** The cap-reorder alone (without the prepend) is cosmetic — today's drop functions check `cap` at offset 8 and work fine; renumbering to offset 0 only buys a uniform `__gorget_is_view` helper, which doesn't justify the lockstep cost on its own. Doing the reorder *as preparation* for the prepend is paying for the prepend's setup without committing to the prepend; if revival never happens the work is wasted. Either revive the whole design (reorder + prepend together) or leave both alone.

### 4.4 What would trigger revival

Concrete signals, not "just in case":
- Phase C migration stalls for >3 months without convergence on the hard cases.
- Production users hit shallow-alias bugs that the IR validator demonstrably missed.
- A specific bug class proves cheaper to backstop at runtime than to fix in the validator.

If revived: ~2 weeks of focused work, one synchronised cutover with self-host. The design above is the implementation plan — no further design needed.

---

## 5. Phase C — Strict move/clone validation

### 5.1 The compile-time guarantee

Phase C makes the IR refuse to *produce* a shallow alias of an owned
resource in the first place. The semantic spec is the CoW contract
from [`copy-on-write.md` §Phase 3](copy-on-write.md) — that document
is authoritative. CoW's default is borrow at zero cost; Phase C
enforces that only valid moves and necessary clones replace borrow
when an ownership boundary requires it.

The contract in one sentence: at every read of a resource-typed
value, a move is valid IFF the source **owns its data AND is dead at
that read**. Otherwise, it must be a clone or a borrow.

The IR encodes three valid outcomes:

- `Move` — source becomes logically dead, destination owns. Only
  when the source both owns and is dead after this read. The IR
  instruction is `MoveZero`; the backend zeros the source slot only
  when drop-tracking would otherwise re-drop the value and elides
  the zero when liveness proves it unobservable.
- `Clone` — explicit deep-copy via the type's `clone_fn`. Source
  stays live (or, equivalently, the source was never owning to begin
  with).
- `Borrow` — destination has type `Ref[T]` / `Ptr<T>` and never gets
  a drop registered. Source stays the owner.

Any other read of a resource value is a compile error.

**Two consequences worth pinning, because the IR violates them today:**

- Bare resource-type parameters are borrows (see
  `copy-on-write.md` §"Function parameters"). They don't own data the
  function body can give away. Reads of a param at a consuming
  position must produce `Clone` (or `Borrow` if the destination is
  itself a Ptr) — never `Copy`, never `Move`.
- Collection-read aliases (`vec.get(0).unwrap()`) and view-returning
  method results (`s.trim()`, `s[1..3]`) are also borrows. Same rule.

There is no "Param-bound locals" special case to carve out. One rule
applies everywhere; Phase D's `LocalOwnership` lets the validator
ask "is this a borrow?" with one typed match.

### 5.2 What's already done

- `OwnershipState` enum on locals (`Owned`, `Borrowed`, `Ref`, `Param`).
- `AssignMode` enum on `Assign` (`Copy`, `Move`, `Clone`, `Borrow`).
- `MoveZero` instruction.
- DropAccountant tracks per-scope drop schedule.
- Borrow checker (Pass-5a) catches dangling references.
- Auto-clone-at-boundary heuristic (`auto_clone_if_ptr`) — inserts clones at "ownership boundaries" (function call args of resource type, etc.).

### 5.3 What's missing

- `Assign { mode: Copy }` of a resource-typed local is silently allowed. Today's bug: `Callable h = handler` shallow-copies the GorgetClosure struct. Both have the same env. Both drop. Double-free.
- Field projections of resource-typed fields aren't required to produce `Borrow`-mode bindings.
- `Param`-bound locals are read with `Copy` mode in many places.
- Function returns of resource types use shallow copy on the way back to the caller.
- No validation pass that walks the IR and rejects these patterns.

### 5.4 The proposed validation pass

Run after IR lowering, before LIR conversion. Fail-fast: any violation is a compile error.

```rust
fn validate_resource_moves(func: &Function, registry: &TypeRegistry) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    for bb in &func.blocks {
        for inst in &bb.instructions {
            match inst {
                Instruction::Assign { dst, value, mode } => {
                    let dst_ty = func.locals[dst.local.0 as usize].type_id;
                    if !registry.is_resource_type(dst_ty) { continue; }

                    match (value, mode) {
                        (Operand::Copy(_), AssignMode::Copy) => {
                            errors.push(ValidationError::ShallowCopyOfResource {
                                local: dst.local, mode: *mode,
                            });
                        }
                        // Move / Clone / Borrow are sound.
                        _ => {}
                    }
                }
                // Same for IndexLoad, EnumExtract, FieldLoad, Call args, …
                _ => {}
            }
        }
    }
    errors
}
```

The IR-lowering passes that today emit `AssignMode::Copy` for resource sources need to be migrated to emit one of `Move | Clone | Borrow`. Most are already doing the right thing for the common cases (last-use becomes Move, multi-use becomes Clone) — the gaps are around Param sources, field projections, and intermediate temps from method-call chains.

### 5.5 The cost

Every read of a resource value must resolve to one of `Move`,
`Clone`, or `Borrow`. In CoW terms: assignments and reads default to
borrow (zero-cost); clones fire at ownership boundaries (returns,
struct fields, collection puts, function-call args where the
collection-must-own contract requires it); moves fire only when the
source both owns and is dead.

Concrete examples and the validator's decision:

- `T x = y` where `y` is a Param of resource type → `x` becomes a
  borrow alias of `y`. Mode: `Borrow`. No clone, no move. (Clone
  fires later if `x` crosses an ownership boundary.) Today many
  lowering sites emit `AssignMode::Copy` here — that's the bug
  Phase C catches.
- `T x = struct.field` where field is resource → `x` is a borrow of
  the field. Mode: `Borrow`.
- `return y` where `y` is a resource local that owns its data →
  `Move` (return is always last-use). If `y` is a borrow,
  `Clone` (return is an ownership boundary).
- `coll.push(y)` where `y` is a resource local → `Move` if `y` owns
  AND is at last use; `Clone` otherwise — including when `y` is a
  bare param or any other borrow shape.

Most of these the compiler decides automatically from liveness +
ownership state (Phase D's `LocalOwnership`). Some require the user
to be explicit: `!y` for move when liveness can't prove last-use,
`y.clone()` to defeat borrow propagation when the user wants an
independent owned copy. The README endorses this — "Borrows and
moves are marked at call sites" — so it's not a language change,
just enforcement.

### 5.6 Migration plan for Phase C

Stage C1: Implement `validate_resource_moves` as a *warning* pass (not yet fail). Run it across all fixtures, collect every violation, sort by frequency.

Stage C2: Fix the highest-frequency violation patterns by upgrading the IR-lowering passes that emit the offending `Assign { mode: Copy }`. Rerun the warning pass; the count should drop.

Stage C3: Once the warning count is below ~10, audit the remaining cases. Either fix or document why they're sound (likely none — every shallow copy of a resource is a latent bug).

Stage C4: Promote `validate_resource_moves` from warning to compile error. Lock the invariant.

Estimated effort: ~1 week with Phase D in place (collapses to a typed-match validator over `LocalOwnership`). The original 3-week estimate assumed Phase D wasn't done first. Risk: low-medium — the upstream lowering changes already happened in Phase D.

### 5.7 Why Phase C suffices on its own

With Phase B deferred, Phase C is the sole guarantee against shallow
aliasing of resources. Under CoW, the IR shouldn't emit shallow
copies at all — every read defaults to borrow, every ownership
boundary materializes a clone. Phase C catches the sites where
today's lowering *does* emit shallow copies despite the CoW intent:
`AssignMode::Copy` on resource sources, field projections that don't
propagate borrow, return paths that bit-copy, intermediate temps in
method-call chains. Each is a CoW violation that produces a
double-free or aliased-mutable-state defect at runtime; the
validator rejects them at compile time.

Phase B's deferred design (§4) would have caught the double-free
half at runtime via a no-op-on-view drop, and would not have caught
aliased-mutable-state at all — both copies in that scenario are
physically valid; the bug is semantic. Phase C addresses both
classes at compile time, which is why it suffices on its own.

### 5.8 Hosting in the validator framework

`validate_resource_moves` doesn't need its own pass infrastructure — Tier E's per-pass validator framework (§8.3) hosts it. Phase C's contribution is one entry in the validator registry; the framework provides the "run after every pass" plumbing. Phase C lands as: define the rule, register it, done.

---

## 6. Phase D — Local-state consolidation and borrow provenance

> **Status:** Added 2026-05-01 after a `lower_var_decl` walkthrough. Phase A consolidates the *type* axis; Phase D is the missing consolidation on the *local* axis. Without it, Phase C's validator either re-derives state from instruction sequences (slow, fragile) or queries half a dozen sidecar maps that today disagree at the seams.

### 6.1 The fragmentation on the IR side

Phase A targets the type axis: 16 parallel name-based lookup tables collapsed into one `ResourceMetadata` accessor. The mirror image — *per-local* ownership state during lowering — is just as fragmented. To answer "what is `_42`?" today the lowering context queries:

| Source | Question it answers |
|---|---|
| `Local.ownership: OwnershipState` (3 variants on the post-lowering `Local`) | Owned / Ref / MaybeBorrowed |
| `LoweringContext.local_ownership: FxHashMap<LocalId, LocalOwnershipState>` (7 variants) | Owned / Alias / CollectionRef / BareParam / Ref / CowBorrow / ViewOf |
| `func_state.string_borrow_sources: FxHashSet<LocalId>` | "Has this string been borrowed-from?" |
| `func_state.cow_alias_sources` / `cow_ptr_params` | CoW alias bookkeeping |
| `func_state.move_override_params` | "Is this generic param being moved?" |
| `func_state.mut_capture_locals` | "Was this local declared `&` or `!`?" |
| `func_state.tuple_element_locals` | "What element locals back this tuple temp?" |
| `func_state.field_load_origins` | "Which struct field did this temp come from?" |
| `func_state.fresh_strings` | "Is this string a fresh allocation, safe to skip clone?" |
| `drops.is_registered` / `is_moved` | "Will scope-exit drop this? Is it dead?" |

The decision tree in `lower_var_decl` (`src/ir/lowering/stmts/mod.rs:521–620`) is the smoking gun: ~100 lines query a dozen of these predicates in a specific order to choose one of four `AssignMode` values. That's the same shape as Phase A's "16 lookup sites for one type" pattern, just on the local axis.

The downstream collapse to `OwnershipState` (3 variants on `Local`) loses information. Once lowering finishes, no pass can ask *"did this Ptr borrow from collection X or struct field Y?"* — that distinction lived on `LocalOwnershipState` and was thrown away by the time the LIR runs.

This violates the project rule "no name matching, no parallel lists that have to stay in sync" (CLAUDE.md): the sidecars *are* parallel lists, and they have drifted before — every "use-after-move that escaped the borrow checker" bug in the last six months is a case where one sidecar said one thing and the other said another.

### 6.2 Single typed `LocalOwnership` field

Replace the parallel sidecars with a single typed field on `Local`:

```rust
pub struct Local {
    pub type_id: TypeId,
    pub name_hint: Option<String>,
    pub ownership: LocalOwnership,
}

pub enum LocalOwnership {
    /// Owns its data. Registered for drop at scope exit.
    Owned,
    /// Borrowed — does NOT drop. Carries provenance (§6.3).
    Borrowed { origin: BorrowOrigin, mutability: Mutability },
    /// Runtime view (Phase B): cap=0 sentinel, source-zero discriminator.
    /// Drop is a no-op, source mutation triggers materialisation.
    View { source: BorrowOrigin },
    /// Started borrowed, may have been materialised on some paths.
    /// Conditional drop guard via `__gorget_is_view`. Today's
    /// `MaybeBorrowed` state — kept until Phase C makes it unreachable.
    MaybeOwned,
}

pub enum Mutability { Shared, Unique }
```

This collapses the existing 7-variant `LocalOwnershipState`, the 3-variant `OwnershipState`, **and** the six sidecar maps listed above into one field per local. Equivalent encodings:

| Today | After |
|---|---|
| `local_ownership[l] = BareParam` + `ownership = Ref` | `Borrowed { origin: Param(p), mutability: Shared }` |
| `local_ownership[l] = CollectionRef { collection }` | `Borrowed { origin: CollectionElement(c), mutability: Shared }` |
| `local_ownership[l] = ViewOf { source }` (cap=0 byte-slice via `slice/trim/...`) | `View { source: Local(s) }` |
| `string_borrow_sources.insert(source)` (Branch A: `String b = a` 32-byte shallow copy) | `View { source: Local(s) }` *with a value-aliasing variant* — see retirement note |
| `mut_capture_locals.contains(l)` (param `&` or `!`) | `Borrowed { origin: Param(p), mutability: Unique }` |
| `cow_ptr_params[l] = source` | absorbed into `Borrowed { origin, … }` |

**`string_borrow_sources` retirement gating (probed 2026-05-04, deferred).** The doc treats `ViewOf` and `string_borrow_sources` as siblings unifying to one `View { source }` variant, but today's `LocalOwnershipState::ViewOf` and the sidecar model **structurally different invariants** at the post-lowering layer: ViewOf flushes to `OwnershipState::MaybeBorrowed`, which the LIR backend's `lower_place_addr` reads as Ptr ABI (`SlotLoad → void*`). That's correct for cap=0 byte-slice views (the Str's `.data` does point into another buffer, the local IS a 32-byte struct whose `.data` field aliases another buffer's bytes). It is **wrong** for Branch A's value-aliasing case, where the LHS local holds a full 32-byte shallow copy of the source's `{data, cap, len, alloc}` and IS the new owner — not a pointer into anything. Tagging Branch A's LHS as ViewOf surfaces as a C codegen type mismatch ("incompatible types when assigning to type 'void *' from type 'Str'") at the drop site. Both invariants answer the same question on the read side ("if I move/return X, is its heap data shared with another live local?"), but the LIR-side encoding diverges. Retirement requires either (a) `flush_ownership_to_locals` distinguishing cap=0-view ViewOf from value-aliasing ViewOf and flushing only the former to MaybeBorrowed; or (b) a separate `LocalOwnershipState::SharedHeap { other }` variant that flushes to Owned but participates in `views_of_source` queries. Sidecar kept until §6.6's Stage D5 lands the typed unification on `Local.ownership` — at that point `View { source }` is a single shape and the Branch A / cap=0-view distinction lives on the **flush rule**, not the IR encoding.

### 6.3 First-class `BorrowOrigin`

Today `Instruction::Borrow { dst, place }` carries the source `place` *at emission time*, but the information evaporates downstream. A pass that wants to ask "what does `_42` point into?" reads `local_ownership[_42]`, walks `Alias { source }` chains, and matches on enum variants. CoW materialisation (`cow_before_mutation`) re-derives this on every mutation.

Promote it to a typed field inside `LocalOwnership::Borrowed`:

```rust
pub enum BorrowOrigin {
    /// Param N of the enclosing function. Const if Shared, mutable if Unique.
    Param(LocalId),
    /// Element borrowed from a collection. Mutation of the collection
    /// triggers materialisation (today's `LocalOwnershipState::CollectionRef`).
    CollectionElement(LocalId),
    /// Field of a struct local. Mutation of the struct (or assignment
    /// to the field) triggers materialisation.
    Field { base: LocalId, field: u32 },
    /// Alias of another local — propagate origin transitively to root.
    Alias(LocalId),
    /// Fresh runtime view (e.g., `s.trim()`, `s[1..3]`) borrowing
    /// from `source`'s buffer. Today's `LocalOwnershipState::ViewOf`.
    RuntimeView(LocalId),
}
```

`cow_before_mutation` collapses to one typed match on `local.ownership` — no hashmap walks, no name-based fallbacks. The borrow checker (Pass-5a) and the validator (Phase C) both read the same field.

Crucially, this **persists through the LIR**: today `Slot { ty, name }` doesn't carry ownership; future LIR slots get an `origin: Option<BorrowOrigin>` so backends can emit safer code. (The C backend's deref-vs-clone decision becomes a typed match instead of the current name heuristics.)

### 6.4 Uniform read-mode discipline

Phase C as drafted (§5) handles `Assign { mode: Copy }` of resource types. There are *six* other reads in the IR, each with its own mode encoding:

| Instruction | Mode encoding today |
|---|---|
| `Assign` | `AssignMode { Copy, Move, Clone, Borrow }` |
| `FieldLoad` | `FieldLoadMode { Copy, MoveZeroSource }` |
| `IndexLoad` | `borrow: bool` |
| `LoadRef` | implicit (always reads through Ptr) |
| `Call` per-arg | `ArgOwnership { Copy, Move, Borrow }` |
| `Operand::Copy(Place)` (anywhere) | implicit copy |

Replace with **one shared `ReadMode`** that every read-of-a-place carries:

```rust
pub enum ReadMode {
    /// Trivial bitwise read. Validator: source type MUST be `Trivial`.
    Copy,
    /// Move ownership — validator: source MUST be Owned and last-use.
    Move,
    /// Deep clone — validator: source must have a clone fn (Phase A metadata).
    Clone,
    /// Borrow — destination becomes Borrowed { origin derived from source }.
    /// Validator: respects unique-vs-shared borrow rules.
    Borrow(Mutability),
}
```

Existing instructions keep their shape — `AssignMode`, `FieldLoadMode`, `IndexLoad.borrow`, `ArgOwnership` all become typed views of this one enum. `LoadRef`/`StoreRef` become explicit `Borrow`-mode reads.

The Phase C validator (§5.4) becomes one rule applied uniformly:

```rust
fn validate_read(local: &Local, mode: ReadMode, registry: &TypeRegistry) -> Result<()> {
    match (registry.copy_semantics(local.type_id), mode) {
        (CopySemantics::Trivial,    ReadMode::Copy)        => Ok(()),
        (CopySemantics::Resource,   ReadMode::Copy)        => Err(ShallowCopyOfResource),
        (CopySemantics::Resource,   ReadMode::Move)        => check_last_use(local),
        (CopySemantics::Resource,   ReadMode::Clone)       => check_clone_fn_exists(local),
        (CopySemantics::Resource,   ReadMode::Borrow(mu))  => check_borrow_rules(local, mu),
        (CopySemantics::RefCounted, _)                     => Ok(()), // Shared[T]
        // ...
    }
}
```

One rule, applied at every read site, replacing six per-instruction validation paths.

### 6.5 Rationale — why this shape, not others

**Why a typed field on `Local` instead of a parallel hashmap?** Three reasons.

1. *Locality of reasoning.* When a pass wants "what is `_42`?", it reads `func.locals[42]` — type, name, ownership all in one place. Today the answer is split across the IR (`type_id`), the lowering context (`local_ownership` + six sidecars), and the drop accountant (`drops`). The bug pattern in this codebase is reliably one of those going stale relative to the others.
2. *Persistence through pipeline stages.* `LocalOwnershipState` lives only in `LoweringContext`; it's gone by the time LIR runs. Putting it on `Local` carries it to LIR and beyond — backends, validators, future borrow-checker passes all read the same source of truth.
3. *Symmetry with Phase A.* Phase A puts type metadata on `TypeDef`. This puts local metadata on `Local`. Same rule on both axes: declarative state at the source, typed accessors everywhere else. The CLAUDE.md "no name matching" prohibition applies symmetrically — sidecar maps keyed by `LocalId` are the local-axis equivalent of the name-based runtime-symbol lookup tables Phase A is killing.

**Why `BorrowOrigin` as an enum, not a `Place` (LocalId + projection path)?** A `Place`-based representation is the obvious alternative. Rejected because it conflates *where the borrow points* (a Place) with *which mutations trigger materialisation* (a coarser concept — the whole collection, the whole struct). A `Vector[Vector[int]]` element borrowed via `outer.get(i).get(j)` should be invalidated when `outer` is mutated *or* when the inner vector is, but treating that as one Place loses the structure. The enum makes the materialisation predicate explicit; each variant *is* the trigger.

**Why one `ReadMode` instead of keeping the four instruction-specific enums?** Each existing enum carries the *same* four-option choice in slightly different vocabulary, validated by slightly different code. Sharing the type means sharing the validator — and ensures the rules can't drift between AssignMode-Copy and FieldLoadMode-Copy. We've already had that drift: `AssignMode::Copy` of a resource is rejected by convention (today, by the lowering's politeness, not by validation); `FieldLoadMode::Copy` of a resource field still happens because field-projection lowering doesn't go through the AssignMode path. Same rule, two implementations, two opportunities to drift.

**Why not just keep the lowering-context map and accept the cost?** The lowering context is rebuilt per function. The information it carries is recomputed every monomorphisation, every generic instantiation. Persisting onto `Local` removes that recomputation. More importantly: the mental model improvement isn't free if you have to look at two places to answer "what is `_42`?" — keeping the sidecar means keeping the cognitive cost.

**Why now, not later?** Phase C's validator pass needs an authoritative source of "is this local owned, borrowed, or a view, and from where?" Without Phase D, that pass either re-derives the answer (slow, error-prone) or pulls from `local_ownership` (incomplete after lowering finishes, and tangled with the six sidecars). Landing Phase D first is what makes Phase C's validator small enough to actually write — without it, Phase C is a 3-week IR-tour, with it, Phase C is a 200-line walker over a typed field.

**Why is this Gorget-shaped, not Rust-shaped?** Rust solves the same problem with lifetime parameters: provenance is in the type. Gorget deliberately chose to keep lifetimes out of the user-visible language. That decision *requires* the compiler to track provenance somewhere — and the only honest place is on the local. CoW provenance via `BorrowOrigin` is the IR mechanism that buys "no lifetime annotations" without giving up the safety guarantees. It's the actual invention.

### 6.6 Migration plan for Phase D

Stage D1: Define `LocalOwnership` and `BorrowOrigin` enums. Add the field to `Local` alongside the existing `ownership: OwnershipState` (don't remove yet — allow both during transition).

Stage D2: At every site that today writes to `local_ownership`, also write the corresponding `LocalOwnership` variant onto the local. Both stay in sync; consumers can pick which to read.

Stage D3: Migrate consumers one at a time. Easiest first (`is_owned_local`, `is_bare_param`, `is_cow_borrow`) — each becomes a typed match on `local.ownership`. Hardest last: `cow_before_mutation` and the `lower_var_decl` decision tree.

Stage D4: Delete `local_ownership: FxHashMap`, the six sidecar maps (`string_borrow_sources`, `cow_alias_sources`, `cow_ptr_params`, `move_override_params`, `mut_capture_locals`, `tuple_element_locals`), and the old `OwnershipState` enum.

> **Status (2026-05-12):** D1-D5 + D4.5 shipped. D6 partial.
> - `OwnershipState` enum: deleted.
> - 7-variant `LocalOwnershipState` enum: deleted.
> - `cow_alias_sources`, `cow_ptr_params`, `string_borrow_sources` (writer side), `view_returning_temps`, `cow_collection_refs`, `cow_alias_targets`: retired in favor of `LocalOwnership::Borrowed` / `View` variants.
> - `move_override_params`: retired from `HashSet<String>` to `FxHashSet<LocalId>` (typed key).
> - **D4.5 — `local_ownership: FxHashMap` retired (shipped, date unrecorded).** Every setter now writes directly to `builder.locals[idx].ownership`; `SavedScope` captures `pre_save_ownership: Vec<LocalOwnership>` as a typed array. `flush_ownership_to_locals` survives as a vestige — its body only derives `slot_kind` per (type, ownership) now; the "flush" semantics is gone. The doc-comment at `src/ir/lowering/context.rs:2280` documents the retirement.

Stage D5: Introduce `ReadMode` as the shared enum. Migrate `AssignMode`, `FieldLoadMode`, `IndexLoad.borrow`, `ArgOwnership` to be typed views of it. Update the validator (§5.4) to use the unified `validate_read()` rule.

> **Status (2026-05-12): D5 shipped.** `pub enum ReadMode { Copy, Move, Clone, Borrow }` lives at `src/ir/instructions.rs:92`; `AssignMode = ReadMode` (type alias, not a wrapper struct) so all existing emission and consumer sites unify on one vocabulary. `IndexLoad.read: ReadMode` joined; `EnumFieldLoadMode` migrated (Snag #34 follow-on). `validate_read()` at `src/ir/validate.rs` consumes the unified enum.

Stage D6: Persist `LocalOwnership` through GIR → LIR (`Slot.origin: Option<BorrowOrigin>`). This unblocks future borrow-aware codegen optimisations.

> **Status (2026-05-12): D6 partial.** `Local.ownership: LocalOwnership` flows through to LIR via `flush_ownership_to_locals` (which now derives `Slot.slot_kind` per the §6.8 Stage 3 rule). But the LIR side reads ownership indirectly via `slot_kind` (`Value` / `OwnedPtr` / `BorrowedPtr`); it does not yet expose a typed `Slot.origin: Option<BorrowOrigin>` field for downstream borrow-aware codegen. That last step is the future enhancement gating cross-pass borrow optimisations.

> **⭐ DESIGN DECISION for D6's final hop (owner, 2026-06-06): build the slot-provenance state as a SINGLE unified enum, NOT the two-field `slot_kind` + `Slot.origin` split.** When D6 actually lands the LIR-side borrow provenance (in either compiler — and per the "complete Rust if it helps reference-grade" directive, possibly both), use:
> ```
> enum SlotProvenance:
>     Value                   # slot holds the value directly
>     Owned                   # slot holds a pointer the local owns (borrow_mut, Option[Ref]::unwrap)
>     Borrowed(BorrowOrigin)  # non-owning view pointer; the origin rides in the payload
> ```
> The layout/access decision the 6 current `slot_kind == BorrowedPtr` consumers do (`src/lir/lower/{drops.rs:524,operands.rs:22,insts.rs:815/967/1291/1495}`) becomes a match on the **variant**; the borrow provenance rides in the `Borrowed` payload. **One source of truth instead of two** — no risk of `slot_kind`/`origin` disagreeing, one accessor. This is an "exceed Rust" call (the current `slot_kind`+`Slot.origin` split is an artifact of `slot_kind` shipping first while `Slot.origin` was deferred). ⚠ Build this layer at all only once a REAL consumer exists (a cross-pass borrow-aware optimisation); until then `Slot.origin`/`SlotProvenance` is unused infrastructure (the deferred-and-unused status above still holds). The minimal nearer-term cleanup remains: port the SHIPPED `slot_kind` projection (D4.5) to retire shape-test reconstruction — but if/when the provenance layer is built on top, prefer the unified `SlotProvenance` shape. See also the self-host foundational-subsystem plan (`self-host-resource-model.md`, Phase 4) and the session memory `design-unified-slotprovenance`.

> **⭐ DESIGN DECISION — return-view lazy materialization = the real D6 consumer (owner, 2026-07-21). STATUS: ruled, NOT IMPLEMENTED.** Today's compilers still materialise at the return boundary (`ensure_owned_at_boundary` → `ReturnFromBorrow`). This section is the reclaim *design*, not shipped behaviour — keep user-facing docs honest about that split (`language-design.md` §3.6). The dominant self-compile clone cost is `ReturnFromBorrow` + `VarDeclFromBorrow` (~71% of attributed top-level clone events; DEEP-1 / #13 — e.g. `Parser.peek(): return self.tokens.get(i).unwrap()`). Reclaiming it means extending lazy CoW **across the function-return boundary**: a function that returns a projection of its receiver/param carries typed **view-return provenance** on its signature (the `returns_view` axis — today builtin-only in `src/ir/lowering/builtins.rs`, lifted to user functions), the caller propagates that `BorrowOrigin`, and the view materialises **lazily**, only where a conflicting mutation of the source is *statically* reachable while the view is live. **This is exactly the "cross-pass borrow-aware optimisation" the `SlotProvenance`/D6 gate was waiting for** — the real consumer that justifies building the layer. Two rulings pin the fork:
> 1. **Static provenance, NOT a runtime refcount.** A Swift-style refcounted-buffer CoW (mutation checks "am I shared?" and copies-on-write) is **rejected** — it taxes every mutation with a shared-check and pins the old buffer in memory, breaking the zero-runtime-overhead / hand-optimal-cost pillar (`language-design.md`). Provenance is tracked at compile time on the local (`BorrowOrigin` — "the actual invention" of §6.3: it buys "no user-visible lifetimes" *without* a runtime tax).
> 2. **Materialise-when-unsure, NEVER reject.** Consistent with the killed reject-gate (2026-07-02): where the static escape/effect analysis cannot *prove* the view stays valid, it materialises (today's clone), never rejects the program. **Sound by construction** — conservatism can only add clones, never a UAF (no runtime backstop, so the analysis must be clone-biased).
> **Consequences / honest scope.** Yield is bounded by the analysis's reach: short-lived, provably-dead-before-mutation views reclaim (the hot "peek → check kind → advance" case); views held *across* a source mutation, escaped into a long-lived field, or crossing an opaque boundary the effect analysis can't follow stay clones. It needs (a) typed view-return provenance on **user** signatures and (b) an **inter-procedural effect analysis** ("does a callee mutate the source while a returned view is live?"). Sequencing: land D6 / `SlotProvenance` → view-return provenance on signatures → the effect/escape analysis → measured reclaim. **The reclaim MUST be end-to-end-measured on *leaf* clone volume before committing** — the attributed count is ~3.5% of leaf clones, so the real yield is unproven (the tree's "scout estimates must be end-to-end-verified" rule applies). Split for risk: the `VarDeclFromBorrow` sub-slice where the *self-host clones but Rust already borrow-aliases* the identical `x = coll.get(i)` (the ~1.93× SH-excess) is a low-risk SH-catches-up-to-Rust fix that needs **none** of this new machinery — grab it first. The cross-return-boundary view is the highest-UAF-risk CoW increment (the intra-procedural materialize already needed 5 UAF folds; ASan is blind to view-into-element UAFs — stdout fixtures are the primary net).

**Residual: Tier 3b proxy-read ratchet.** Phase D3's migration retired the sidecar maps but left ~77 callsites still going through proxy fns (`is_named_local`, `is_owned_local`, `drops.is_registered`, `drops.is_moved`). All of these are typed-field reads under the hood — the proxies just hide the field accessor. Tracked by `tests/lints.rs::no_growth_in_phase_d_proxy_reads` (BUDGET=77, one-way ratchet). Mechanical migration; no soundness impact. See `structural-guards.md` Tier 3b for the ratchet's design.

Estimated effort (2026-05-12, historical): Phase D as a whole was ~2 weeks. D6's final hop and the Tier 3b ratchet burn-down are the remaining incremental items.

### 6.7 What Phase D enables

- **Phase C's validator becomes ~50 lines** instead of a per-instruction tour.
- **The `lower_var_decl` decision tree** (`stmts/mod.rs:521–620`) collapses to a few typed matches on `source.ownership`. The 12-predicate query becomes one read.
- **CoW materialisation (`cow_before_mutation`) becomes a typed match** on `BorrowOrigin`. New origins (e.g., `Field`, `RuntimeView`) get materialisation rules added by extending the enum, not by adding a sidecar.
- **Self-host parity is easier.** A single typed local-state struct is simpler to mirror than the seven sidecars it replaces. (Self-host's typechecker drift, currently 845/861, has the same root cause as the sidecar drift here: scattered state of record.)
- **Future borrow-checker enhancements** (cross-block, cross-function, alias-aware) read one canonical source instead of reconstructing state from instruction sequences.
- **The IR validator already exists** (`src/ir/validate.rs` — 1200+ lines, with `UseAfterMove` detection across blocks) — Phase D plugs into it; no new pass infrastructure needed.

This is what makes "Rust-grade memory safety, no lifetime annotations" a property of the IR, not a property of an exhausting decision tree spread across a dozen files.

### 6.8 Companion: LIR-side per-value provenance

The LIR has the same fragmentation today, one layer down. Both backends rebuild parallel arrays — `str_lit_vals`, `null_vals`, `cstr_vals`, `func_addr_targets`, `spawn_source_fn`, `ptr_pointee` — to recover origin information about each `ValueId`. ~37 emit-decision sites per backend.

**Phase D extends naturally to the LIR.** The same `BorrowOrigin` vocabulary (Param / CollectionElement / Field / Alias / RuntimeView) plus a few LIR-specific origins (StrLit, NullPtr, FuncAddr, CstrLit) becomes a typed `Provenance` field on `LirFunction` indexed by `ValueId`. Either as one shared array on the function (replacing the five backend bitmaps) or encoded into the instruction variants that produce values with those origins (`Inst::StrLit`, `Inst::NullPtr`, `Inst::FuncAddr` already exist — making them the *only* way to produce values with those origins is the cleanest shape).

This is the LIR roadmap's "A4 — origin metadata as per-value tags" item. It's not a separate refactor; it's Phase D's continuation across the GIR/LIR boundary, using the same enum vocabulary. Co-design the two together.

WASM-specific: linear-memory loads need precise width (`i32.load8_u` vs `i32.load`). Per-value type info MUST be authoritative — falling back to "infer from context" doesn't work in WASM. Phase D's LIR side is what makes a WASM backend tractable.

Estimated effort: 2 commits, folded into Phase D's overall window. Subsumes A4 entirely.

---

## 7. The composition

| Bug class | Phase A | Phase C | Phase D | Phase B *(deferred)* |
|---|---|---|---|---|
| Forgotten lookup table for new resource type | **Fixed** | — | — | — |
| Two parts of pipeline disagree on size/ABI | **Fixed** | — | — | — |
| Shallow copy → both drop, double-free | — | **Fixed (compile error)** | Enables Phase C | (Would fix at runtime; not needed if C lands) |
| Shallow copy → aliased mutable state | — | **Fixed** | Enables Phase C | — |
| Use-after-free from outliving source | — | **Fixed (borrow checker rejects)** | Enables Phase C | — |
| Type-erased function-pointer ABI mismatch | — | Partial (separate UBSan-shim issue) | — | — |
| Sidecar maps drift (`local_ownership` vs `string_borrow_sources` vs `cow_ptr_params`) | — | — | **Fixed** | — |
| Provenance lost between GIR and LIR | — | — | **Fixed** | — |
| `lower_var_decl` 12-predicate decision tree | — | — | **Fixed (collapses to typed match)** | — |

Phase A unblocks D and C — both need authoritative metadata. Phase D is the IR-side counterpart to A, and is what makes Phase C tractable. Phase C is the principled fix for the entire shallow-alias bug class. Phase B (deferred, §4) would be the runtime backstop *if* Phase C ever stalls — not part of the active plan.

Planned landing order was **A → D → C** with D before C mandatory (Phase C without D is a 3-week IR-tour; with D it's a ~1-week typed-match walker). Actual landing (2026-05-12): A's type-metadata side substantially shipped (locked by Tier 1c coherence-at-construction validator); D shipped (D1-D5 + D4.5 in, D6 partial); C shipped (Phase C validators + Tier 2a `validate_consume_sites` all fatal). The remaining open piece is **A's §3.6 `RUNTIME_DECLS` / `resources.toml` build-tooling pipeline** — orthogonal to D/C, doesn't block any other phase. Tier E (§8) proceeded in parallel throughout. §9 describes how this order interacted with self-host work.

---

## 8. Tier E — LIR-side correctness and shape work

LIR-layer items from the audit that don't touch resources directly: drop-flag hygiene, SSA invariants, optimizer convergence, typed function references, validator framework. Independent of Phases A/D/C — runs in parallel throughout. Listed here so the unified roadmap is complete; sequenced with the resource phases in §9.

### 8.1 Drop-flag init from dataflow

Today's drop-flag instrumentation (commit `d28b8f86`) seeds `bb0 = false` and instruments `SlotStore` to set the flag at first store. Conservative but correct. The dataflow pass already computes per-block init states; the flag's initial value at each block could be seeded from that state directly — no blanket false, no reliance on `SlotStore` to "fix" the flag at first use. Catches function-param slots and other unconditionally-init cases without waiting for the explicit param-`SlotStore`.

Estimate: 1 commit + extended drop test fixtures.

**GIR drop-emission contract — defensive-by-default (Snag #30, 2026-05-10).** The GIR drop accountant in `src/ir/lowering/drops.rs` emits `DropIfAlive` unconditionally for every resource-typed scope-exit drop. The LIR `drop_elab` pass statically elides the runtime drop-flag check when slot init is provably unconditional, so the always-conditional shape is free at runtime. Snag #30's bug class — `DropEntry::maybe_moved` producing a false negative across nested matches with early-return paths — motivated this contract: the per-arm `maybe_moved` flag in the GIR-level drop accountant is NOT a sound CFG-aware analysis. A local marked moved in one match's Some arm could appear as not-moved at a later match's None-arm `emit_early_exit_drops` callsite (because `lower_match_expr` doesn't `snapshot_moved`/`restore_moved`/`union_moved` between arms the way `lower_match_stmt` does). The always-`DropIfAlive` contract sidesteps the whole class. The `maybe_moved` sidecar persists for future invariant audits but is no longer load-bearing for soundness — the LIR pass is the source of truth for whether the runtime check fires. See `docs/internals/structural-guards.md` §"Drop-emission contract: defensive-by-default" for the full design + commit pointer.

### 8.2 Critical-edge splitting + post-SSA invariant validation

SSA construction (`src/lir/ssa.rs`) uses a simplified Braun et al. algorithm that assumes no critical edges. There's no validator that asserts:
- The CFG is reducible (no irreducible loops).
- Every value use is dominated by its definition.
- Critical edges (block with multiple successors → block with multiple predecessors) don't exist.

WASM has structured control flow only. If the LIR produces an irreducible CFG, the WASM backend can't emit it without a relooper pass — which is its own correctness hazard. Long-term-correct: critical edges split at LIR construction time (or by a dedicated pre-SSA pass). Post-SSA validator asserts reducibility, dominance, and edge-set well-formedness. The dominance check exists in debug builds (`ssa.rs:32-36`) but isn't called from `validate_module`.

Estimate: 1 commit — split + validator extension.

### 8.3 Validator runs after every pass

Today `validate_module` runs once before SSA. After every pass — optimizer, BIR lowering, drop elaboration — invariants can drift silently. Long-term-correct: the test harness invokes `validate_module` after each pass in debug builds. Cheap, catches every shape regression. **Phase C's `validate_resource_moves` plugs into this framework** (§5.8) — same registry, same per-pass invocation.

Estimate: 1 commit.

### 8.4 Optimizer fixpoint

`optimize_function` runs three iterations and stops, regardless of whether it would have converged in four. Replace with snapshot-equality fixpoint check (already used in SSA, drop elab). Trivial.

Estimate: 1 commit.

### 8.5 Cross-block constant propagation, GVN, LICM (deferred)

The LIR optimizer is intra-block CSE + intra-block constant folding today. Cross-block passes — constant propagation, global value numbering, loop-invariant code motion — are textbook but lower priority while LLVM's `clang -O2` does most of this on the C backend's output, and LLVM's own optimizer does it on the LLVM backend's output. The LIR optimizer matters most for WASM (where downstream optimization is weaker). **Defer until WASM ships.**

### 8.6 Typed function references (`LirType::FuncRef`)

Today `Inst::FuncAddr { dst, func: FuncId }` produces a `Ptr` value. That works for C (any function pointer is `void(*)`) and LLVM (any function pointer is `ptr`). It does NOT work for WASM, which uses table indices, not pointers — and it conflates "raw function pointer" with "boxed closure" semantically.

Long-term shape:

```rust
// New LirType variant.
LirType::FuncRef
// Inst::FuncAddr's dst becomes LirType::FuncRef instead of Ptr.
Inst::CallByRef { dst, fref: ValueId, args }  // call-via-table-or-pointer
```

Backends:
- C/LLVM: lower `FuncRef` to `void*` / `ptr`, `CallByRef` to indirect call.
- WASM (future): lower `FuncRef` to a table index, `CallByRef` to `call_indirect`.

Originally framed as a co-design with Phase B's closure layout (because both edited `GorgetClosure`). With Phase B deferred, FuncRef stands alone — pure typed-IR refactor, no ABI implications, can land independently whenever convenient.

Estimate: 3-5 days. Independent.

### 8.7 Already shipped

For completeness: `Inst::CallRuntime` + `RuntimeFn` enum (LIR A1/A2) and `Inst::CollectionCtor` (LIR A3) shipped from the audit and are subsets of Phase A's typed-metadata story (see §3.1, §3.6). No further work needed on those.

---

## 9. Sequencing and contract discipline

A multi-phase plan is only useful if the phases can land without blocking unrelated work, and if the contracts each phase exposes don't drift once implementation begins. Self-host (`tests/fixtures/self_host_lowerer/`) is the most relevant other-track — it mirrors the Rust implementation and is currently at 845/861 typechecker drift. This section describes how to sequence the active phases (A → D → C) alongside self-host work, and the discipline that keeps shared contracts (metadata schemas, IR shapes) stable while migrations are in flight.

### 9.1 Per-phase contract surfaces

Each phase has a *contract surface* — the typed metadata, schema files, or layout decisions that downstream consumers (in Rust and self-host alike) read. Pinning these before implementation begins is what prevents divergence.

| Phase | Contract surface | Crosses Rust ↔ self-host? |
|---|---|---|
| A | `resources.toml` (canonical) + the `ResourceMetadata` / `RuntimeDecl` struct definitions (§3.2, §3.6) it deserializes into. Generated artifacts for Rust, self-host, and the C runtime header. | **Yes** — both compilers regenerate from the same TOML. |
| D | `LocalOwnership` enum (§6.2), `BorrowOrigin` enum (§6.3), `ReadMode` enum (§6.4). | No — internal IR shape per compiler. |
| C | The `validate_read()` rule (§5.4 / §6.4). Internal pass. | No — consumers don't see it. |
| B *(deferred)* | Field offsets for every resource type (§4.2). Full runtime ABI. | **Yes** — would force a lockstep window if revived. |

Only Phase A touches a contract that bridges Rust and self-host in the active plan. Phases C and D are internal to whichever compiler implements them — self-host adopts them at its own pace. Phase B's contract is documented for completeness but not on the road; revival would re-introduce the only forced sync window.

### 9.2 The single canonical metadata source

The metadata lives in one place and every consumer reads a *generated* artifact derived from it. The canonical source is a neutral data file (TOML); both the Rust compiler and self-host are generated consumers, on equal footing.

```
resources.toml  (canonical, hand-edited, version-stamped)
    │
    └── build.rs ────┬──→ src/ir/gen/resources.rs       (const Rust data, included via include!)
                     ├──→ lib/std/gen/resources.gg      (const Gorget data, for self-host)
                     └──→ src/backend/c/gen/resources.h (extern decls + struct layouts)
```

```toml
# resources.toml — single source of truth. Hand-edited.

schema_version = 1

[resource.GorgetString]
size              = 32
align             = 8
drop_fn           = "gorget_string_free"
clone_fn          = "gorget_string_clone"
has_view_header   = true
copy_semantics    = "Resource"
on_get            = "Borrow"
elem_abi          = "ByValue"

[resource.GorgetArray]
size              = 64
…

[runtime_fn.gorget_array_new]   # extends Phase A's RUNTIME_DECLS
params            = ["Size"]
ret               = "GorgetArray"
side_effects      = "Allocates"
…
```

Cost: ~1 week on top of Phase A's 2-week estimate (build script + the schema design + ~10-12 resource entries + ~80 runtime-fn entries). Pays back across future resource additions (one row, all consumers regenerate), self-host parity (zero hand-mirrored data), and any future layout change including a hypothetical Phase B revival (one TOML edit reaches every consumer).

**Why TOML, not const Rust data.** An earlier draft of this section proposed `pub static RESOURCES: &[ResourceMetadata] = &[…]` with the C header and self-host form generated *from the Rust source*. That privileges Rust unnecessarily — if any consumer requires generation (and self-host does), the build-script cost is already paid; generating *every* consumer from a neutral source is symmetric and avoids the implicit hierarchy. TOML is the right neutral source: both Rust (`toml` + `serde`) and self-host (small Gorget parser, or just emit Gorget literals) can read it trivially. The `ResourceMetadata` struct definition still lives in Rust; the TOML just populates it via `serde`.

This satisfies Rule 3 of the layering discipline (`layering-discipline.md`) — one source of truth, applied at the cross-language axis. No consumer is canonical; the TOML is. Drift between Rust and self-host metadata becomes physically impossible: both are regenerated from the same file, version-checked at load time.

**Versioning.** The TOML's `schema_version` field is embedded into every generated artifact. Consumers fail to load a version they don't recognize — the mechanical safety net for §9.4's freeze discipline.

### 9.3 Sequencing alongside self-host

| Stage | Duration | Self-host track | Rust track | Notes |
|---|---|---|---|---|
| 1 | ~3 weeks | continues unblocked | Phase A + declarative-source tooling | Self-host PRs that touch the lookup-table sites are deferred until 1.5 |
| 1.5 | ~2 days | adopts the generated `lib/std/gen/resources.gg` | (idle on this track) | Small follow-up PR; mechanical |
| 2 | ~2 weeks | continues unblocked | Phase D | Internal IR; self-host adopts later as a separate task |
| 3 | ~1 week | continues unblocked | Phase C (validator over Phase D's typed state) | No self-host impact |

Total elapsed: ~6 weeks, fully parallel with self-host. **No forced sync window** — Phase B (which would have provided the only lockstep stage) is deferred. Self-host's typechecker work, fixture additions, and bug fixes proceed without coordination throughout.

Phase D shrinks Phase C from the original 3-week estimate to ~1 week (§6.7) — the validator collapses to a single rule once `LocalOwnership` is the source of truth. That's where the time savings come from.

If Phase B is ever revived (§4.4), it adds a forced ~2-week sync window with self-host. Counted as deferred, not active.

### 9.4 Contract evolution discipline

Contracts will need to be revised. The first consumer migration in Phase A will shake out fields the schema didn't anticipate; the first `cow_before_mutation` rewrite in Phase D will reveal `BorrowOrigin` variants that weren't in the initial enum. **Treat this as expected, and discipline the revision process so it doesn't cause divergence.**

Four rules:

1. **Spike before freeze.** Before declaring a contract "ready for migration," implement *one* consumer migration end-to-end as a throwaway spike. The spike's job is to find the schema gaps — fields that turn out to be needed, enum variants that turn out to be missing, layout decisions that turn out to be unsound. Update the contract based on what the spike revealed, then freeze. A 3-day spike routinely saves a week of "we found another field we need" rework.

2. **Freeze before broad migration.** Once a contract is frozen, no edits to its surface while migrations are in flight. If migrations are running on multiple tracks (Rust + self-host, or multiple consumer migrations in Rust at once), an unannounced contract change desynchronises them — each migration was written against a different version of the schema. Edits to a frozen contract require recalling the in-flight migrations first.

3. **Recall on drift.** If a real issue surfaces that requires a contract change — a field is wrong-shaped, a variant is missing, a layout decision was unsound — *stop* in-flight migrations, update the contract, then resume. Do not try to migrate "around" a known-broken contract; the divergence cost compounds. Recalling is cheap (each migration is bounded scope); divergence is expensive (every migration needs reconciliation).

4. **Versioned schema as runtime backstop.** The canonical `resources.toml` carries a `schema_version` field. Every generated artifact (Rust const data, `resources.gg`, C runtime header) embeds it; consumers refuse to load a version they don't recognize. When the schema changes, the version bumps and every consumer either upgrades together or fails loudly with a build error. This is the mechanical safety net for rule 2 — even if the freeze discipline slips, the version mismatch surfaces as a build failure rather than as silent divergence.

The same shape applies to internal-only contracts (Phase D's enums): version them at the type-definition level (a `#[allow(...)]`-style marker that bumps when the enum changes; consumers that haven't been updated trip a compile error). Mechanical safety net beats discipline alone.

### 9.5 Why these rules and not others

**Why spike-first?** Contracts that look complete on paper routinely have gaps that only surface in implementation. Phase A's schema almost certainly omits something that the first real migration will reveal — better to find it via a focused 3-day spike than via a 2-week migration that's halfway done before the gap is noticed. The spike is throwaway by design: its output is *information about the contract*, not production code.

**Why freeze-then-implement?** Contract drift mid-flight is the project's most expensive failure mode. A contract edited after migrations are running means every running migration is a candidate for rework — and the more migrations are running, the more rework piles up. Freezing flips the cost: a contract change costs *one* recall + restart, not N partial-redos. The freeze isn't bureaucracy; it's how you keep the cost of revision linear.

**Why recall on drift, not "fix forward"?** Trying to amend mid-migration ("I'll just adjust the spec note for everyone in flight") fails under pressure: in-flight work has already absorbed the old contract's shape into local decisions. The cleanest reset is recall + restart against the new contract. Painful, but bounded — and the spike rule (rule 1) is what keeps recall events rare.

**Why versioned schema?** Discipline rules ("don't edit the frozen contract") fail under pressure. The version field is the mechanical safety net: a schema edit that didn't bump the version is caught at load time; a schema edit that did bump the version forces every consumer to acknowledge the upgrade. Same shape as Phase B's `__gorget_is_view` runtime check — discipline is the design, runtime check is the defence in depth.

**Why does this apply to self-host specifically?** Self-host's existing 845/861 drift is the cautionary tale. It didn't drift all at once; it drifted across many small unsynchronised changes against a moving target. The contract discipline above is what stops that pattern from repeating in the new shared metadata source — and from compounding through Phases B/D/C as well.

### 9.6 Where the discipline lives

- **The contract sources** themselves (`resources.toml` for `ResourceMetadata` + `RuntimeDecl`, the `LocalOwnership` / `BorrowOrigin` enum definitions in `src/ir/`, the Phase B layout decisions) — versioned, frozen, edited only between phases.
- **Phase landing checklists** in `TODO.md` — each phase has a "Spike done? ✓ / Schema frozen? ✓ / Migrations green on both tracks? ✓" gate.
- **`AGENTS.md` cross-reference** — when adding a new resource type, builtin, or runtime fn, the rule "edit `resources.toml` first, regenerate, all consumers see it" gets cited; same shape as the existing "no name matching" cite.

---

## 10. Risks and trade-offs

### 10.1 ABI breaks

- Phase A: none — purely a refactor (the TOML and generated artifacts are tooling, not ABI).
- Phase D: none — internal IR shape only. `Local` grows by one enum field (~16 bytes); `Slot` grows by one `Option<BorrowOrigin>` (~16 bytes). No runtime ABI impact.
- Phase C: none — the IR pass is internal.
- Phase B *(deferred)*: would grow `Box[T]`, `GorgetClosure`, `Task[T]` by 8 bytes each (uniform `flags` header at offset 0). The principal reason it's deferred — see §4.3.

**Net effect of the active plan: zero runtime ABI changes.** That's the whole point of deferring B.

### 10.2 Performance

- Phase A: zero cost — just better organised code.
- Phase D: net **win** at compile time. Eliminates the 12-predicate query in `lower_var_decl` and the alias-chain walks in `cow_before_mutation`. Adds ~16 bytes per local in the IR (memory, not runtime). Net runtime: zero.
- Phase C: depends on how many shallow copies become Clones vs Moves. Liveness analysis already in place; should be a wash or net improvement (fewer clones because Move is preferred at last use).
- Phase B *(if revived)*: one bitwise check per drop (negligible) and ~+8 B per `Box[T]` / `GorgetClosure` / `Task[T]` instance (memory cost, not runtime). The memory cost is the principal reason for deferral.

### 10.3 User-facing language changes

- Phase A: none.
- Phase C: stricter compile-time checking. Users may see new errors on previously-accepted code that was silently wrong. The errors are diagnosable (point at the shallow-copy site, suggest `!` or `.clone()`). README already promises this style of safety.
- Phase D: none. Better diagnostics indirectly — error messages can name the borrow origin ("borrowed from `outer` at line 42, invalidated by mutation at line 47") because the origin is now structurally available.
- Phase B *(if revived)*: none — runtime detail.

### 10.4 Test surface

Each phase has a clear validation point:
- A: existing test suite (~2000 tests) must stay green at every stage.
- C: each warning fixed in C1 → C2 prevents regression by adding the case to a `validate_resource_moves` test.
- D: stage D2's "both write the new field and the old map" gives a free cross-check — assert at end of lowering that the typed field and the legacy sidecars agree. Failure means a write-site was missed; fix before promoting.

The validation pass in C is itself a test mechanism — once it's a hard error, every CI run verifies the invariant.

### 10.5 Migration cost

- A: ~3 weeks (2 weeks of consolidation + 1 week for the TOML build-script tooling). Refactor with strong tests as safety net. Mostly mechanical.
- D: 2 weeks. IR refactor, sidecar deletion is the dangerous part. Strong existing test coverage (~2000 tests) is the safety net.
- C: ~1 week (down from 3 because D supplies the typed state the validator reads). Risk: low — most of the upstream lowering changes already happened in D.
- B *(deferred)*: 2 weeks if ever revived, with a forced lockstep window with self-host. Not on the active road.

Total active work: ~6 weeks for A → D → C, fully parallel with self-host. Compare against the cost of *not* doing this: roughly two SECURITY-tagged TODOs per session, each ~1-2 hours to investigate and fix, plus the risk that some go unnoticed in user code. Pays back within ~3 months at current bug-discovery rate.

---

## 11. Open questions

1. ~~**Should Phase B's view bit be uniformly at offset 0?**~~ **Resolved 2026-05-01: moot — Phase B is deferred entirely (§4).** The active plan bets on Phase C as the principled fix. If Phase B is ever revived, §4.2 records the converged design (offset 0, reorder + prepend together) so the question doesn't have to be re-litigated.

2. **How does Phase C handle generic `T: Resource` parameters?** When the body calls `T.clone()` it should resolve via the unified metadata (Phase A). When it does `move_only T x = y`, the validation pass needs to know T is Resource even before monomorphisation. Probably need a `Resource` trait bound that's checked at the generic-fn level.

3. **`Shared[T]` and `Weak[T]` interaction.** These deliberately allow shared ownership via refcounting. Phase C must not reject `Shared[T]` shallow copies — they're sound because the runtime refcounts. The metadata's `copy_semantics` should distinguish `Resource` (move-only) from `RefCounted` (shallow-copy ok, refcount the source).

4. **Self-host implications.** The self-host lowerer in `tests/fixtures/self_host_lowerer/` mirrors the Rust implementation. Phase A's metadata needs a self-host equivalent. Resolved (§9.2): canonical source is `resources.toml`; both Rust and self-host get generated artifacts via `build.rs`. Neither language is privileged; drift is mechanically prevented by the `schema_version` check.

5. **External / FFI types.** `extern "C"` types wrapped via `extern fn` declarations don't have full Gorget TypeDefs. Phase A needs a story for "minimum metadata required to declare an external resource type" — probably just `drop_fn` and `clone_fn`, with `view: AlwaysOwned` as a safe default.

6. **Performance regression test.** Add a microbenchmark suite (fib, primes, JSON parse, regex match, …) that runs before and after each phase. Prevents Phase C from accidentally introducing a 10× slowdown via over-aggressive cloning.

7. **Phase D `BorrowOrigin::Field` granularity.** When a struct field is borrowed and another field of the same struct is mutated, should the borrow be invalidated? Conservative answer (today's behaviour): yes, treat any struct mutation as invalidating all field-borrows. Optimal answer: per-field tracking, only invalidate on same-field mutation. Tractable in the IR but adds complexity to `cow_before_field_mutation`. Defer until a fixture demonstrates the cost.

---

## 12. What this doesn't fix

This is a memory-safety architecture proposal. It does not address:

- **Concurrency safety beyond `shared`.** Cross-thread aliasing of resources requires a separate `Send`/`Sync` analogue. Out of scope.
- **Iterator invalidation across `.push()`.** Borrow-vs-mutation conflicts at the same scope; tracked separately by the borrow checker enhancements.
- **Match-expression result-slot bugs.** IR-lowering correctness, not ownership.
- **The flaky `vector_task_get` shared-int race.** Atomic-counter / synchronisation issue.
- **Self-host typechecker drift** (845/861 today). Separate compilation-correctness work.

These continue to be addressed individually.

---

## 13. Summary

We kept finding double-free / UAF / shallow-alias bugs because the architecture had them baked in. **Three** structural changes close the recurring class; the active path was **A → D → C** with Tier E running throughout, plus Phase B documented as a deferred runtime fallback.

**Status (2026-05-12):**

- **Phase D — SHIPPED.** D1-D5 + D4.5 all in. `LocalOwnership` with first-class `BorrowOrigin` is the active typed store on every `Local`. Six historical sidecar maps retired; `local_ownership: FxHashMap` retired (every setter writes directly to `Local.ownership`, `SavedScope` captures typed). `ReadMode` unification shipped (D5); `AssignMode` is a type alias of it. D6 partial — `BorrowOrigin` flows GIR → LIR via `slot_kind` but no typed `Slot.origin` field for downstream borrow-aware optimisations yet. Residual: Tier 3b proxy-read ratchet (BUDGET=77 cosmetic callsites) — see `structural-guards.md` Tier 3b.

- **Phase C — SHIPPED.** `validate_resource_moves` + the read-site quartet (`_field_reads`, `_index_reads`, `_enum_reads`, `_call_args`) plus Tier 2a `validate_consume_sites` (with the typed `consume_externs` registry promoted 2026-05-12) all fatal. Shallow copy of a resource is a compile-time error.

- **Phase A — PARTIAL.** Type-axis metadata consolidation has substantially shipped (`TypeMetadata` carries the field-set Phase A specified — `drop_strategy`, `copy_semantics`, `collection_kind`, `enum_kind`, `c_runtime_alias`, `clone_fn`, etc.). Tier 1c's `validate_type_metadata_coherence` locks coherence-at-construction. The big unshipped piece is **§3.6's `RUNTIME_DECLS` runtime-function table + `resources.toml` build-tooling pipeline** — the single declarative source that generates Rust const + C runtime header + self-host Gorget form. ~3-4 weeks; not yet started.

- **Tier E — ongoing.** Drop-flag hygiene shipped (Snag #30 always-`DropIfAlive` contract). Validator framework shipped (`assert_module_valid` with VALIDATORS registry, per-pass invariants under debug + `GG_VALIDATE_PASSES`). Critical-edge splitting + post-SSA invariants in place. Typed `LirType::FuncRef` partly shipped. Remaining items are incremental — none gate other phases.

- **Phase B — STILL DEFERRED.** §4 design preserved; revival triggers documented. Not on the road.

**The two genuinely unshipped pieces today:**

1. **Phase A's `RUNTIME_DECLS` / `resources.toml`** (~3-4 weeks). The largest open structural project. Generates Rust const data + C runtime header + self-host form from one TOML at `build.rs` time. Closes the "frontend disagrees with runtime" bug class structurally and unblocks parallel backends (LLVM, WASM) with zero drift risk.

2. **Phase C self-host moves-class burn-down (~89k violations).** Long-running; the validator suite for self-host's lowerer is shipped and ratchets in place. Burn-down compounds: every cluster closed improves both the language definition and the self-host's role as the elegance-showcase. Multi-week sustained work.

**What shipped:** the bug class IS dead at the Rust compiler layer. CoW-with-typed-provenance — Phase D's `BorrowOrigin` — is the actual Gorget invention. The structural-guards framework (`docs/internals/structural-guards.md`) is the enforcement mechanism; URM Phases A/D defined the typed metadata, structural-guards Tiers 1-3 validate it at every build.

What's left turns "Rust-grade memory safety, no lifetime annotations" from "true at the Rust compiler" to "true at every layer of the toolchain, mechanically enforced from a single source of truth."
