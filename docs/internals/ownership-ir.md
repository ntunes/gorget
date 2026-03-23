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
    Borrow,  // Ptr stays as Ptr (str views, borrowed refs)
}
```

| Scenario | Mode | What happens |
|----------|------|-------------|
| `int x = y` | Copy | Bitwise copy, both valid |
| `Vector[int] v = make_vec()` | Move | Temp→variable, temp zeroed |
| `Vector[int] b = a` | Clone | Deep clone, a and b independent |
| `str s = owned_string.str()` | Borrow | Ptr preserved, source unregistered from drop |

**Decision tree** (at emission time):
1. Source is GorgetString, dest is str → **Borrow** (unregister source)
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

GorgetStrings have their own provenance-based ownership system (str vs String). Auto-cloning GorgetStrings would break the `.str()` method chain pattern where a str view borrows from the backing GorgetString.

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
| `src/ir/lowering/context.rs` | Tracking sets, `clone_fn_for_ptr()`, `is_named_local()` |
| `src/ir/lowering/drops.rs` | Drop elaboration, `is_moved()`, `is_registered()` |
| `src/ir/types.rs` | `needs_drop()`, `is_collection_type()`, `is_resource_type()` |
| `src/lir/lower.rs` | LIR interpretation of AssignMode, FieldLoad, LoadRef/StoreRef |
| `src/backend/c/c_runtime.rs` | `elem_drop`/`val_drop` on GorgetArray/GorgetMap |
