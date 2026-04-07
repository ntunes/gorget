# Ownership Semantics in the GIR

How the Gorget Intermediate Representation encodes ownership, borrowing, and resource lifecycle.

## Overview

Every value in Gorget is either **trivial** (can be freely copied — int, bool, float, simple structs) or a **resource** (owns heap data — Vector, Dict, Set, String). The GIR encodes ownership decisions directly in instructions so downstream passes (LIR, C backend) can act without re-deriving from context.

## Assignment Modes (`AssignMode`)

Every `Assign` instruction carries a mode:

```rust
pub enum AssignMode {
    Copy,    // Bitwise copy (trivial types)
    Move,    // Transfer ownership — source zeroed after copy
    Clone,   // Deep clone — both source and dest are independent
    Borrow,  // Ptr stays as Ptr (String views, borrowed refs)
}
```

| Scenario | Mode | What happens |
|----------|------|-------------|
| `int x = y` | Copy | Bitwise copy, both valid (trivial types only) |
| `Vector[int] v = make_vec()` | Move | Temp→variable, temp zeroed |
| `Vector[int] b = a.clone()` | Clone | Deep clone via Cloneable trait, a and b independent |
| `Vector[int] c = !a` | Move | Transfer ownership, a consumed |
| `auto d = a` | Borrow | Ptr reference, borrows from a |
| `String s = owned_string.str()` | Borrow | Ptr preserved, source unregistered from drop |

**Key rule:** `Copy` mode is ONLY for trivial types (int, bool, float, simple structs without resource fields). Resource types MUST use Move, Clone, or Borrow. No implicit shallow copies.

**Decision tree** (at emission time):
1. Source is named variable with `.clone()` call → **Clone** (via Cloneable trait)
2. Source is drop-registered temp → **Move** (transfer ownership)
3. Source is unregistered droppable temp → **Move**
4. Otherwise → **Copy**

## Field Load Modes (`FieldLoadMode`)

Every `FieldLoad` instruction carries a mode:

```rust
pub enum FieldLoadMode {
    Copy,           // Trivial field — bitwise copy
    MoveZeroSource, // Resource field — zero source after extraction
}
```

Used in tuple and enum destructuring:
```gorget
auto a, b = get_pair()   # Each field extracted with appropriate mode
match result:
    case Ok(value):       # value extracted with MoveZeroSource if resource type
```

**When MoveZeroSource fires**: The source struct's field is zeroed after the value is copied out. This prevents double-free when both the extracted value and the source struct are dropped.

## Call Argument Ownership (`ArgOwnership`)

Every `Call` instruction carries per-argument ownership:

```rust
pub enum ArgOwnership {
    Borrow, // Caller retains — callee gets pointer
    Move,   // Caller relinquishes — source zeroed after call
    Copy,   // Trivial — no tracking
}
```

Empty `arg_owners` vector means all Borrow (backward compatible).

## Reference Instructions (`LoadRef` / `StoreRef`)

Explicit Ptr dereference, replacing implicit auto-deref patterns:

```rust
LoadRef  { dst, src }    // Deref Ptr(T) → T value
StoreRef { dst, value }  // Write through Ptr(T)
```

These replace the `mut_capture_locals` auto-deref pattern where `&` and `!` parameters were implicitly dereferenced on every identifier access.

## Tracking Sets in the Lowering Context

| Set | Purpose | Will be replaced by |
|-----|---------|-------------------|
| `named_locals` | Distinguish vars from temps for clone decisions | AssignMode (partially) |
| `local_ownership` | Unified `LocalOwnershipState` enum (Owned/Alias/CollectionRef/BareParam/Ref) | LoadRef/StoreRef (future) |
| `mut_capture_locals` | `&`/`!` params with auto-deref + write-through | LoadRef/StoreRef (future) |
| `field_load_origins` | Track source field for post-assign zeroing | FieldLoadMode (partially) |
| `drops` | Track moved/registered locals for scope-exit drops | AssignMode::Move (partially) |

## Struct Field Ownership

Structs **own** their resource-type fields. Field **loads** return non-owning references.

| Field type | Stored as (TypeDef) | Loaded as (FieldLoad result) |
|-----------|--------------------|-----------------------------|
| `String` | `GorgetString` (owned) | `Ptr(GorgetString)` (reference) |
| `Vector[T]` | `Vector__T` (owned) | `Ptr(Vector__T)` (reference) |
| `Dict[K,V]` | `Dict__K__V` (owned) | `Ptr(Dict__K__V)` (reference) |
| `int`, `bool` | Trivial | Trivial (copy) |

**Principle**: ownership is a **type-level** concern (what the struct holds). Borrowing is an **operation-level** concern (what a field read returns). This mirrors Rust: a `String` field in a struct is owned, but `&self.name` borrows it as `&str`.

**Why not Str views in the TypeDef?** If struct fields stored `Str` (non-owning), the struct wouldn't own its string data. The strings would point to external GorgetString temps, requiring the caller to keep those alive — leading to the `should_unregister_string_args` leak heuristic. With owned fields, the struct is self-contained and recursive drop frees everything.

**Uniform Ptr(T) for all resource fields.** All resource-type fields — strings, collections, user structs — return `Ptr(T)` on load. This unified approach means CoW materialization works identically for all field types. The old model returned `Str` (non-owning view) for string fields, but this created a special case that complicated CoW logic.

Auto-clone fires when a `Ptr(T)` reference crosses an ownership boundary (CoW materialization):
```gorget
String x = obj.name.clone()   # Ptr→GorgetString explicit clone
Vector[int] v = obj.items.clone()  # Ptr→Vector explicit clone
auto y = obj.name              # Stays as Ptr reference (zero cost)
auto w = obj.items             # Stays as Ptr reference (zero cost)
```

## CoW Materialization Points

When a borrowed value (`Ptr(T)`) must become owned, the compiler materializes it — cloning the data so the new owner has an independent copy. There are SEVEN materialization points:

| Point | Trigger | GIR Mechanism |
|-------|---------|---------------|
| 1. Assignment | `x = expr` where x is a CoW alias | `cow_before_mutation` in assign handler |
| 2. Mutating method | `x.push(val)` where x is a CoW alias | `cow_before_mutation` before method dispatch |
| 3. Struct/enum init | `Foo(x)` where x is borrowed | `emit_enum_init_owned` + LIR FieldLoad clone |
| 4. Collection put | `v.push(x)` where x is borrowed | `clone_multi_use_resource_args` |
| 5. Return | `return x` where x is borrowed | `lower_return` Ptr→T auto-clone |
| 6. Move transfer | `consume(!x)` where x is borrowed | Ownership::Move Ptr→clone |
| 7. Field store | `self.f = x` where x is borrowed | `lower_field_assign` Ptr→`clone_fn_for_ptr` |

**`cow_before_mutation()`** is the single entry point for CoW severance at points 1 and 2. It checks the `local_ownership` map for the local's state (`BareParam`, `Alias`, or collection refs via derived scan), and if a CoW relationship exists, emits a clone of the source data, then updates the ownership state to `Owned`. All mutation sites (assignment, mutating method calls) route through this gate.

## Call Result Drop Registration

Function and method call results are automatically registered for drop via `call_tracked()` / `call_extern_tracked()`. These use the narrow `needs_drop_for_temp()` check:

| Type | Registered? | Reason |
|------|------------|--------|
| Collections (Vector, Dict, Set) | Yes | Name-based detection |
| GorgetString | Yes | `DropStrategy::Trivial` |
| User structs with Custom drop | Yes | `DropStrategy::Custom` |
| User structs with Recursive drop | No | Move-zero on consumed temps can conflict with shallow copies |
| Primitives, Ptr, Str | No | Non-owning or trivial |

The VarDecl path detects registered temps via `is_registered()` → `AssignMode::Move` → `mark_moved()`, preventing double-free at scope exit.

## LIR SlotStore Move Flag

`Inst::SlotStore { slot, value, is_move }` carries a move flag from GIR `AssignMode::Move`. The C backend uses this to choose between:

- `is_move: true` → `memcpy` (transfer ownership, source will be zeroed by MoveZero)
- `is_move: false` → `gorget_string_clone` (copy; views stay as views, owned strings get independent copy)

This eliminates unnecessary `clone + free` round-trips for string temporaries.

## Design Decisions

### Deep clone on variable-to-variable assignment

`Vector[int] b = a` deep-clones. This ensures `a` and `b` are independent — modifying one doesn't affect the other. Without this, `memcpy` creates a shallow copy sharing heap data, causing double-free on scope exit.

**Exception**: Temps from function calls use Move (zero-cost ownership transfer) instead of Clone. The `named_locals` set distinguishes the two cases.

### Borrowed resource params stay as Ptr

Bare-borrow resource params (`Vector[int] v` without `&` or `!`) are `Ptr(T)` throughout the callee body. They are NOT auto-dereferenced to values. This prevents all shallow-copy bugs — the callee never creates a value copy from a borrowed param.

Methods and field access resolve through `pointee_type()`. For-loops explicitly deref Ptr iterables into a read-only view (not drop-registered).

### Auto-clone on type context

When a `Ptr(T)` value (from IndexLoad or borrowed param) is assigned to an explicitly-typed `T` variable, the compiler auto-clones. `auto` gives the fast path (reference); explicit type gives an owned copy.

### GorgetString excluded from auto-clone

GorgetStrings have their own provenance-based ownership system (view vs owned). Auto-cloning GorgetStrings would break the `.str()` method chain pattern where a String view borrows from the backing GorgetString.

### Collection elem_drop

`GorgetArray` and `GorgetMap` have function pointer fields (`elem_drop`, `val_drop`) set at construction. These are called in all element-removing operations:

- `gorget_array_set` / `gorget_map_put` — drop old element before overwriting
- `gorget_array_clear` — drop all elements before zeroing length
- `gorget_array_remove` — drop removed element before memmove
- `gorget_array_free` — drop all elements before freeing the buffer

## Key Files

| File | Role |
|------|------|
| `src/ir/instructions.rs` | AssignMode, FieldLoadMode, ArgOwnership, LoadRef/StoreRef definitions |
| `src/ir/builder.rs` | `assign_mode()`, `field_load_mode()`, `load_ref()`, `store_ref()` |
| `src/ir/lowering/stmts/mod.rs` | VarDecl mode decision tree |
| `src/ir/lowering/stmts/assigns.rs` | Reassignment mode decision tree |
| `src/ir/lowering/context.rs` | Tracking sets, `clone_fn_for_ptr()`, `is_named_local()`, `call_tracked()` |
| `src/ir/lowering/drops.rs` | Drop elaboration, `is_moved()`, `is_registered()` |
| `src/ir/lowering/exprs/mod.rs` | Field load Ptr/Str conversion, `register_owned_string_for_drop()` |
| `src/ir/lowering/types.rs` | Struct field TypeDef registration (GorgetString ownership) |
| `src/ir/types.rs` | `needs_drop()`, `needs_drop_for_temp()`, `is_collection_type()` |
| `src/lir/mod.rs` | `Inst::SlotStore { is_move }` flag |
| `src/backend/c_lir/mod.rs` | SlotStore clone vs memcpy, block param type inference |
| `src/lir/lower.rs` | LIR interpretation of AssignMode, FieldLoad, LoadRef/StoreRef |
| `src/backend/c/c_runtime.rs` | `elem_drop`/`val_drop` on GorgetArray/GorgetMap |
