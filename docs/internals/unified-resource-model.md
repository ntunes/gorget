# Unified Resource Model — Design

> **Status:** Proposed (2026-04-28). Revised same day to converge `cap` with the universal header (§4).
> **Authors:** opus-4.7 session.
> **Builds on:** `ownership-ir.md`, `copy-on-write.md`, `safety-checker.md`.
> **Supersedes once landed:** the parallel name-based lookup tables enumerated in §3.1; the 2026-04-12 "cap at field index 1" layout decision (replaced by "cap at field index 0" — see §4).

This document proposes a three-phase architectural change to make a recurring class of bugs — double-frees, use-after-free, and use-after-move on resource-typed values — *structurally impossible* rather than chased one fixture at a time.

The changes are:

- **Phase A — Unified resource metadata.** Replace the current ~10 parallel name-based lookup tables (`clone_fn_for_ptr`, `infer_drop_strategy`, `elem_drop_fn_for_*`, `needs_drop`, `is_resource_type`, `collection_runtime_type`, `c_sizeof_with_structs`, …) with a single `ResourceMetadata` struct attached to every resource type at registration time. All consumers read from one accessor. Adding a new resource type touches one declaration site instead of ten.
- **Phase B — Universal view/owner discrimination.** Generalise the CoW pattern that `String` already uses (`cap == 0` ⇒ view, the drop fn is a no-op) to every resource type, with one rule: **first 8 bytes == 0 ⇒ view**. Collections move existing `cap` to offset 0; non-collections (Box, Closure, Task) prepend a uniform `flags` header. Shallow copies become safe at runtime: only the owner ever frees, every other holder is a view that no-ops on drop. One `__gorget_is_view(void*)` function works on any resource pointer with no metadata lookup. Defends in depth against bugs that escape Phase C.
- **Phase C — Strict move/clone validation.** Make every read of a resource-typed value either `MoveZero` (source dies), an explicit `Clone` (independent deep copy), or a `Borrow` (typed `Ref[T]`/`MutPtr<T>` that has its own no-drop discipline). Reject any IR that produces a shallow alias of an owned resource. This makes the Phase B safety net redundant in steady state, but keeps it as defence in depth during migration.

The phases compose: Phase A is a refactor that unblocks the others; Phase B is a runtime safety net that ships fast and catches bugs while Phase C is being built; Phase C is the compile-time guarantee that the bug class can't recur.

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

---

## 4. Phase B — Universal view/owner discrimination

### 4.1 Why this is a runtime safety net, not just a refactor

Phase A makes every consumer agree about what a type is. It doesn't change the runtime behaviour where two values share a heap allocation. The Dict[K, Callable] bug landed two days ago because:

- `Dict.get(key).unwrap()` returned a Callable VALUE (not a Ref).
- The IR registered the unwrap result for drop (since Callable is now `needs_drop`).
- The Dict slot also registers drop via `val_drop`.
- Both pointed at the same heap env. Both dropped. Double-free.

String has the same shape but doesn't bite because `gorget_string_free` checks `cap == 0` and no-ops on views. Whichever copy still has `cap > 0` becomes the owner; everyone else is a view that no-ops on drop.

**Generalising this to every resource type makes shallow-copy double-free physically impossible**, regardless of whether the IR tracks ownership correctly.

### 4.2 One rule for every resource

The earlier draft of this section had three view-discriminator schemes (per-type sentinel fields, leading-header word, pointer-bit tagging) chosen for ABI preservation. That preservation isn't worth the maintenance debt — bit-stealing on `env` size prefixes and pointer alignment bits is exactly the kind of clever-now-cursed-later trick this whole document is supposed to eliminate.

The right design: **every resource type's first 8 bytes are the discriminator. `0` ⇒ view, anything non-zero ⇒ owner. One rule, one runtime check.**

```c
static inline bool __gorget_is_view(const void* p) {
    return *(const uint64_t*)p == 0;
}
```

For collections this is *already true* if we move the existing `cap` field from offset 8 to offset 0 — `cap == 0` already coincides with "view" (a borrowed buffer with no allocated capacity of its own). The 2026-04-12 layout decision (cap at offset 8 / field index 1) was the project's first attempt at a uniform discriminator location; this is the second attempt that picks the *useful* location instead of the merely consistent one.

For non-collections (`GorgetClosure`, `Box[T]`, `Task[T]`), we prepend an 8-byte `flags` header. Bit 0 = `OWNED`. Other bits reserved for future use (refcounted, frozen, thread-shared, etc. — all the things we'll inevitably want to add later). The "extra" 8 bytes is honest cost for a uniform safe scheme; trying to avoid it via bit-stealing is the trap.

### 4.3 Layouts after convergence

| Type | Today | After Phase B | Δ |
|---|---|---|---|
| `GorgetString` | `{data, cap, len, alloc}` 32 B (cap at offset 8) | `{cap, data, len, alloc}` 32 B (cap at offset 0) | reorder only, same size |
| `GorgetArray` | `{data, cap, len, elem_size, alloc, elem_drop, elem_clone, elem_materialize}` 64 B | `{cap, data, len, elem_size, alloc, elem_drop, elem_clone, elem_materialize}` 64 B | reorder only |
| `GorgetMap` / `GorgetSet` | cap at field index 1 | cap at field index 0, all other fields shift left | reorder only |
| `GorgetClosure` | `{fn_ptr, env}` 16 B | `{flags, fn_ptr, env}` 24 B | +8 B (50%) |
| `Box[T]` | `T*` 8 B | `{flags, T*}` 16 B | +8 B (100%) |
| `Task[T]` | `{task_ptr, drop_fn}` 16 B | `{flags, task_ptr, drop_fn}` 24 B | +8 B (50%) |
| Opaque handles (Socket, Mutex, …) | bare `int64_t`-sized | unchanged | 0 — these set `has_view_header: false` in the metadata and stay FFI-shaped |

The cost concentrates on small/hot types. For collections it's free (just a field-index renumber). For Box, Closure, Task it's an honest +8 bytes — paid for runtime safety, debuggability, and the elimination of bit-stealing tricks.

Cache impact of moving `cap` to offset 0 is essentially zero: `cap` and `data` always live in the same 64-byte cache line, so the access order doesn't change cache misses. If anything it helps — the discriminator-first layout lets the CPU's branch predictor speculate the view-vs-owner check before the data load completes.

### 4.4 Drop functions become uniform

```c
// Before:
static inline void gorget_string_free(void* p) {
    GorgetString* s = (GorgetString*)p;
    if (s->cap == 0) { *s = (Str){0}; return; }
    s->alloc->dealloc(s->alloc->ctx, s->data, s->cap);
    *s = (Str){0};
}

// After (uniform check, type-specific free):
static inline void gorget_string_free(void* p) {
    if (__gorget_is_view(p)) { return; }   // first 8 bytes == 0 ⇒ view
    GorgetString* s = (GorgetString*)p;
    s->alloc->dealloc(s->alloc->ctx, s->data, s->cap);
    *(uint64_t*)p = 0;                      // mark as view post-drop (idempotent)
}

static inline void gorget_closure_free(void* p) {
    if (__gorget_is_view(p)) { return; }
    GorgetClosure* c = (GorgetClosure*)p;
    if (c->env) free((char*)c->env - sizeof(size_t));  // env still uses size prefix
    *(uint64_t*)p = 0;
}

static inline void gorget_box_free(void* p) {
    if (__gorget_is_view(p)) { return; }
    GorgetBox* b = (GorgetBox*)p;
    free(b->data);
    *(uint64_t*)p = 0;
}
```

Same shape every time. The post-drop `*(uint64_t*)p = 0` makes free idempotent — repeated drops of the same slot are no-ops. That's the same property `gorget_string_free` already gets accidentally from setting `*s = (Str){0}`; we make it uniform and explicit.

### 4.5 When does the view bit get set?

Any runtime path that produces a shallow copy of an owned resource sets the view bit on either the source or the destination. The two specific cases:

1. **`Dict.get(key).unwrap()` for `Dict[K, Callable]`** (today's known double-free). The unwrap result reads the slot's GorgetClosure value. The IR emits a `mark_view` on the unwrap result; the Dict slot stays owner. View-result drop is no-op; slot drop frees as normal.

2. **Field projections of a resource-typed struct field.** Reading `s.field` where field is resource produces a view of the field's data. The struct stays the owner.

Once Phase C lands, both of these become impossible to *write* in the IR — the only way to read a resource value is `Move`, `Clone`, or `Borrow`. The view-bit machinery becomes the runtime safety net under Phase C, only relevant if Phase C has bugs.

### 4.6 What this saves

Every "shallow copy and both drop" bug class. Concretely, after Phase B:
- `Dict[K, Callable]` double-free → impossible (view bit on unwrap result).
- `routes.put(key, h)` double-free → impossible (slot stays owner; h's post-put drop is no-op).
- `Vector[Callable]` env leak from intermediate locals → impossible.
- The `__gorget_drop_fn` UBSan trip stays (separate type-system issue).
- Dropped match-arm value stays (separate IR-lowering issue).

Phase B closes ~half the SECURITY-tagged TODOs and prevents future ones in that class.

### 4.7 Migration plan for Phase B

Stage B1: Reorder collection structs to put `cap` at offset 0. Update positional struct initializers in `c_runtime.rs` (~20-30 sites — most use designated initializers and are robust). Renumber LIR `FieldPtr` indices: `cap` was index 1, becomes 0; `data` was 0, becomes 1; rest of fields renumber accordingly. Audit hardcoded field-offset assumptions in self-host (project memory documents these — must move in lockstep).

Stage B2: Add `__gorget_is_view(void*)` runtime helper. Update every `*_free` runtime function (string, array, map, set) to check it first. Add idempotent post-drop zeroing.

Stage B3: Prepend `uint64_t flags` to `GorgetClosure`, `Box[T]`, `Task[T]`. Update the matching `*_free` and `*_clone` functions. Wire the LIR sites that allocate / pack these to set `flags = GORGET_OWNED`. Update every site that assumes `sizeof(GorgetClosure) == 16` (probably ~10-20 sites; mechanical).

Stage B4: For user-defined Resource structs, the auto-generated `T__drop` prepends a `__gorget_is_view` check. The struct itself either gets a generated `flags: u64` field at offset 0 (mandated by the resource registration), or — if the user already declared a leading 8-byte field that's never zero in practice — reuse that offset.

Stage B5: Audit all custom drop functions for runtime types to confirm they all check the view marker first. Add a compile-time assertion that every `ResourceMetadata.drop_fn` starts with the view check (sanity-checked via a runtime preamble that wraps each registered drop_fn).

Stage B6: Wire the IR sites that produce shallow copies of resources (Dict.get unwrap, field projection of resource fields) to emit a `mark_view` LIR instruction on the result.

Estimated effort: 2 weeks (revised from 1.5 — the field-reorder ripples are real, especially for self-host parity). Risk: medium. Each resource type can be migrated independently, but the field-reorder needs the whole runtime + self-host moved in one synchronised change.

---

## 5. Phase C — Strict move/clone validation

### 5.1 The compile-time guarantee

Phase B catches double-frees at runtime via a no-op-on-view check. Phase C makes the IR refuse to *produce* a shallow alias of an owned resource in the first place.

Rule: every read of a resource-typed value (a Place dereference, an Operand::Copy, a Field projection) emits one of:

- `MoveZero`: source becomes invalid, destination owns. Only when the source is dead after this read (last use, or `!source` annotation).
- `Clone`: explicit deep-copy via the type's `clone_fn`. Source stays live.
- `Borrow`: destination has type `Ref[T]`/`MutPtr<T>` and never gets a drop registered. Source stays the owner.

Any other read of a resource value is a compile error (or at minimum a hard validation panic in debug, a `Drop::Unknown` strategy in release).

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

Every shallow alias of a resource value must become an explicit Move, Clone, or Borrow. This means:
- `T x = y` where `y` is a Param of resource type and is used after this point → must Clone. Today silent shallow copy + double-free.
- `T x = struct.field` where field is resource → must Borrow (returns `Ref[T]`) or Clone.
- `return y` where `y` is a resource local → must Move.
- `f(y)` where the param expects `T` (resource) and `y` is used after the call → must Clone.

Most of these the compiler can decide automatically based on the existing liveness analysis. Some require the user to be explicit (`!y` for move, `y.clone()` for clone). The README explicitly endorses this — "Borrows and moves are marked at call sites" — so it's not a language change, just enforcement.

### 5.6 Migration plan for Phase C

Stage C1: Implement `validate_resource_moves` as a *warning* pass (not yet fail). Run it across all fixtures, collect every violation, sort by frequency.

Stage C2: Fix the highest-frequency violation patterns by upgrading the IR-lowering passes that emit the offending `Assign { mode: Copy }`. Rerun the warning pass; the count should drop.

Stage C3: Once the warning count is below ~10, audit the remaining cases. Either fix or document why they're sound (likely none — every shallow copy of a resource is a latent bug).

Stage C4: Promote `validate_resource_moves` from warning to compile error. Lock the invariant.

Stage C5: Optional — once Phase C is proven sound across the integration suite, deprecate Phase B's runtime view-checks. They're now a defence-in-depth net for bugs Phase C can't catch.

Estimated effort: 3 weeks. Risk: medium-high. The validation pass is small; the upstream lowering changes are widespread. Worth doing because it's the only change that *prevents the bug class entirely* rather than catching it after the fact.

### 5.7 What Phase C catches that Phase B doesn't

Phase B turns shallow-copy double-free into a no-op on the view side. The free still happens once (on the owner). But there's a *correctness* issue Phase B can't fix:

- Caller has a Callable with env = {captured_state: 5}.
- Caller passes Callable to callee by shallow copy. View bit set on caller's copy.
- Callee mutates env via a captured state mutation (FnMut-equivalent).
- Callee returns. Callee's view-bit copy goes out of scope, no-op drop.
- Caller now sees env = {captured_state: <mutated>}.

This is an aliased mutable state bug — the caller's "I have my own closure" expectation is violated. Phase B doesn't catch it because both copies are physically valid; the bug is semantic.

Phase C catches it by requiring the caller's `f(handler)` either Move (caller doesn't keep handler) or Clone (callee gets independent copy). Aliased mutable state becomes impossible.

This is why Phase C is the actual safety guarantee, not just defence in depth.

---

## 6. The composition

| Bug class | Phase A | Phase B | Phase C |
|---|---|---|---|
| Forgotten lookup table for new resource type | **Fixed** | — | — |
| Two parts of pipeline disagree on size/ABI | **Fixed** | — | — |
| Shallow copy → both drop, double-free | — | **Fixed (runtime no-op)** | **Fixed (compile error)** |
| Shallow copy → aliased mutable state | — | — | **Fixed** |
| Use-after-free from outliving source | — | Partial (drop is no-op so use sees stale data) | **Fixed (borrow checker rejects)** |
| Type-erased function-pointer ABI mismatch | — | — | Partial (separate UBSan-shim issue) |

Phase A unblocks B and C — both need authoritative metadata. Phase B is faster to land and gives immediate runtime safety. Phase C is the real fix and supersedes B over time.

Recommended landing order: A → B → C. Each phase builds on the previous and is independently shippable.

---

## 7. Risks and trade-offs

### 7.1 ABI breaks

- Phase A: none — purely a refactor.
- Phase B grows `Box[T]`, `GorgetClosure`, `Task[T]` by 8 bytes each (uniform `flags` header at offset 0). Real ABI break — every site that assumes `sizeof(GorgetClosure) == 16` needs to update. Bounded though: ~10-20 sites in the runtime + LIR codegen, all mechanical. The earlier-draft pointer-tagging alternative was rejected as too clever (see §4.2).
- Phase C: none — the IR pass is internal.

### 7.2 Performance

- Phase A: zero cost — just better organised code.
- Phase B: one bitwise check per drop. Negligible. View-vs-owner tracking also avoids unnecessary deep clones in some hot paths.
- Phase C: depends on how many shallow copies become Clones vs Moves. Liveness analysis already in place; should be a wash or net improvement (fewer clones because Move is preferred at last use).

### 7.3 User-facing language changes

- Phase A: none.
- Phase B: none — runtime detail.
- Phase C: stricter compile-time checking. Users may see new errors on previously-accepted code that was silently wrong. The errors are diagnosable (point at the shallow-copy site, suggest `!` or `.clone()`). README already promises this style of safety.

### 7.4 Test surface

Each phase has a clear validation point:
- A: existing test suite (~2000 tests) must stay green at every stage.
- B: each migrated resource gets a focused fixture exercising its view-vs-owner discipline.
- C: each warning fixed in C1 → C2 prevents regression by adding the case to a `validate_resource_moves` test.

The validation pass in C is itself a test mechanism — once it's a hard error, every CI run verifies the invariant.

### 7.5 Migration cost

- A: 2 weeks. Refactor with strong tests as safety net. Mostly mechanical.
- B: 1.5 weeks. Runtime + LIR. Requires careful audit of each resource's drop function.
- C: 3 weeks. IR-lowering changes spread across many sites. Highest risk of "fixing one violation creates another".

Total: ~7 weeks of focused work (revised from 6.5 — Phase B's field-reorder ripples into the self-host lowerer's hardcoded offsets, see project memory's 2026-04-12 layout note). Compare against the cost of *not* doing this: roughly two SECURITY-tagged TODOs per session, each ~1-2 hours to investigate and fix, plus the risk that some go unnoticed in user code. Pays back within ~3 months at current bug-discovery rate.

---

## 8. Open questions

1. ~~**Should Phase B's view bit be uniformly at offset 0?**~~ **Resolved 2026-04-28: yes.** The full first 8 bytes of every resource are the discriminator (`0` ⇒ view, non-zero ⇒ owner). Collections move existing `cap` to offset 0; non-collections prepend a uniform `flags` header. See §4.2-4.3 for the converged design. The cost (Box/Closure/Task gain 8 bytes each) is paid for: a single `__gorget_is_view` function works on any resource pointer, no metadata-dispatch needed in the hot drop path, no bit-stealing on `env` size prefixes or pointer alignment bits.

2. **How does Phase C handle generic `T: Resource` parameters?** When the body calls `T.clone()` it should resolve via the unified metadata (Phase A). When it does `move_only T x = y`, the validation pass needs to know T is Resource even before monomorphisation. Probably need a `Resource` trait bound that's checked at the generic-fn level.

3. **`Shared[T]` and `Weak[T]` interaction.** These deliberately allow shared ownership via refcounting. Phase C must not reject `Shared[T]` shallow copies — they're sound because the runtime refcounts. The metadata's `copy_semantics` should distinguish `Resource` (move-only) from `RefCounted` (shallow-copy ok, refcount the source).

4. **Self-host implications.** The self-host lowerer in `tests/fixtures/self_host_lowerer/` mirrors the Rust implementation. Phase A's metadata table needs a self-host equivalent. Probably means generating the table from a single source-of-truth file (TOML or JSON) that both Rust and self-host read. Adds tooling cost but eliminates drift.

5. **External / FFI types.** `extern "C"` types wrapped via `extern fn` declarations don't have full Gorget TypeDefs. Phase A needs a story for "minimum metadata required to declare an external resource type" — probably just `drop_fn` and `clone_fn`, with `view: AlwaysOwned` as a safe default.

6. **Performance regression test.** Add a microbenchmark suite (fib, primes, JSON parse, regex match, …) that runs before and after each phase. Prevents Phase C from accidentally introducing a 10× slowdown via over-aggressive cloning.

---

## 9. What this doesn't fix

This is a memory-safety architecture proposal. It does not address:

- **Concurrency safety beyond `shared`.** Cross-thread aliasing of resources requires a separate `Send`/`Sync` analogue. Out of scope.
- **Iterator invalidation across `.push()`.** Borrow-vs-mutation conflicts at the same scope; tracked separately by the borrow checker enhancements.
- **Match-expression result-slot bugs.** IR-lowering correctness, not ownership.
- **The flaky `vector_task_get` shared-int race.** Atomic-counter / synchronisation issue.
- **Self-host typechecker drift** (845/861 today). Separate compilation-correctness work.

These continue to be addressed individually.

---

## 10. Summary

We keep finding double-free / UAF / shallow-alias bugs because the architecture has them baked in. Three structural changes — A (consolidate metadata), B (universal view/owner discrimination), C (strict move/clone validation) — close the recurring class.

Recommended path: A first (foundation), B second (runtime safety net), C third (compile-time guarantee that supersedes B).

Total cost: ~7 weeks. Returns: roughly halves the SECURITY-tagged TODO discovery rate going forward; makes the README's "Rust-grade memory safety" promise mechanical instead of aspirational.

Land Phase A and we're already winning. Land Phase C and the bug class is dead.
