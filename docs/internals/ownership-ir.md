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
| `int x = y` | Copy | Bitwise copy, both valid |
| `Vector[int] v = make_vec()` | Move | Temp→variable, temp zeroed |
| `Vector[int] b = a` | Clone | Deep clone, a and b independent |
| `String s = owned_string.str()` | Borrow | Ptr preserved, source unregistered from drop |

**Decision tree** (at emission time):
1. Source is GorgetString, dest is String (view) → **Borrow** (unregister source)
2. Source is named variable with clone function → **Clone** (emit clone call, then Move the result)
3. Source is drop-registered temp → **Move** (transfer ownership)
4. Source is GorgetString temp, dest is GorgetString → **Move**
5. Otherwise → **Copy**

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
| `ref_locals` | Ptr-typed locals that skip auto-deref | LoadRef/StoreRef (future) |
| `mut_capture_locals` | `&`/`!` params with auto-deref + write-through | LoadRef/StoreRef (future) |
| `field_load_origins` | Track source field for post-assign zeroing | FieldLoadMode (partially) |
| `drops` | Track moved/registered locals for scope-exit drops | AssignMode::Move (partially) |

## Struct Field Ownership

Structs **own** their resource-type fields. Field **loads** return non-owning references.

| Field type | Stored as (TypeDef) | Loaded as (FieldLoad result) |
|-----------|--------------------|-----------------------------|
| `String` | `GorgetString` (owned) | `Str` (non-owning view) |
| `Vector[T]` | `Vector__T` (owned) | `Ptr(Vector__T)` (reference) |
| `Dict[K,V]` | `Dict__K__V` (owned) | `Ptr(Dict__K__V)` (reference) |
| `int`, `bool` | Trivial | Trivial (copy) |

**Principle**: ownership is a **type-level** concern (what the struct holds). Borrowing is an **operation-level** concern (what a field read returns). This mirrors Rust: a `String` field in a struct is owned, but `&self.name` borrows it as `&str`.

**Why not Str views in the TypeDef?** If struct fields stored `Str` (non-owning), the struct wouldn't own its string data. The strings would point to external GorgetString temps, requiring the caller to keep those alive — leading to the `should_unregister_string_args` leak heuristic. With owned fields, the struct is self-contained and recursive drop frees everything.

**Why not Ptr(GorgetString) for string fields?** Unlike collections, strings have a natural non-owning representation (`Str`) that all consumers already handle (operators, methods, print, format). `Ptr(GorgetString)` would require every string consumer to unwrap the pointer.

Auto-clone fires when a loaded reference/view is assigned to an explicitly-typed owned variable:
```gorget
String x = obj.name        # Str→GorgetString auto-clone (gorget_string_clone)
Vector[int] v = obj.items  # Ptr→Vector auto-clone (gorget_array_clone)
auto y = obj.name           # Stays as Str view (zero cost)
auto w = obj.items          # Stays as Ptr reference (zero cost)
```

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
- `is_move: false` → `gorget_string_clone` (independent copy, source stays alive)

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

### Collection elem_drop on overwrite

`GorgetArray` and `GorgetMap` have function pointer fields (`elem_drop`, `val_drop`) set at construction. `gorget_array_set` calls `elem_drop` on the old element before overwriting; `gorget_map_put` calls `val_drop` when a key already exists. This prevents resource leaks from element overwrite.

Collection destruction (`gorget_array_free`) does NOT use `elem_drop` — element drops on destruction are handled by the LIR's `elem_drop_recipes` mechanism to avoid double-drops.

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
