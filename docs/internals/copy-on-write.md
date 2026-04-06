# Copy-on-Write Ownership Model

> **Status:** All phases implemented (1a–1g, 2a–2d).
> **Date:** 2026-04-05 (closure Ptr capture + for-loop iterator safety)
> **Supersedes:** Implicit clone warnings, LIR per-element drop recipes.

## Core Principle

**Everything is a reference until mutation.** All assignments, function parameters, and collection reads produce pointers to the source data. The first mutation on either side of an alias triggers a clone, giving the mutator its own independent copy. If no mutation ever occurs, no clone ever happens.

## The Universal Rule

When a value is aliased (assigned, passed, or read from a collection), the compiler tracks the alias relationship. Three outcomes are possible:

1. **Neither side mutated:** No clone. The alias is just a pointer. Zero cost.
2. **The alias is mutated:** Clone the source data into the alias, sever the relationship, then mutate the clone. The source is unchanged.
3. **The source is mutated:** Clone the data into the alias first (so it preserves the value it saw), sever the relationship, then mutate the source.

## How It Applies

### Variable assignment

```gorget
String a = "hello"
String b = a              # b is a pointer to a's data
print(b)                  # read — zero cost
b = b.upper()             # mutation on b → clone a's data into b, then mutate
                          # a is still "hello", b is "HELLO"
```

### Function parameters

```gorget
void process(String name):     # name is a pointer to caller's value
    print(name)                # read — zero cost
    name = name.upper()        # mutation → clone, then mutate locally
                               # caller's value unchanged

String s = "hello"
process(s)                     # passes pointer. No copy.
print(s)                       # still "hello"
```

### Collection reads

Collection reads (`.get()`, `v[i]`) return a mutable borrow (`&T`) into the
collection's storage. Both `auto` and typed bindings produce borrows — there is
no implicit clone:

```gorget
auto entry = v.get(i).unwrap() # &Entry — mutable borrow into v's storage
Entry entry = v.get(i).unwrap() # &Entry — also a borrow, NOT a clone
print(entry.name)              # read through borrow — zero cost
entry.name = "new"             # mutation in place through & — modifies v[i] directly
```

Borrows propagate through field access and destructuring:

```gorget
auto ev = events.get(i).unwrap()  # &GameEvent
match ev:
    case .ItemPickup(cat, name, pos):  # name is &String (borrow propagates)
        hud.pickup_text = name          # ERROR: cannot store borrow in owned field
        hud.pickup_text = name.clone()  # ✓ explicit clone for ownership
```

To get an owned copy, use `.clone()`:

```gorget
Entry owned = v.get(i).unwrap().clone()  # deep clone — caller owns the copy
```

### Collection mutation while borrow exists

```gorget
auto entry = v.get(i).unwrap() # &Entry — borrow into v's storage
v.push(something)              # ERROR: cannot mutate v while entry borrows from it
```

The borrow checker rejects this — `v.push()` may reallocate the buffer,
invalidating the borrow.

### No difference between `auto` and explicit type

```gorget
auto x = v.get(i).unwrap()    # &T — borrow
String x = v.get(i).unwrap()  # &T — also a borrow (no auto-clone)
String x = v.get(i).unwrap().clone()  # T — owned copy (explicit)
```

## Ownership Transfer (push, put, struct constructors)

When a value is stored into a collection or struct field, the collection/struct must **own** the data exclusively. The compiler automatically determines the cheapest correct strategy:

1. **Auto-move (zero-cost):** If the source variable is dead after the store (not used again in the current scope), the compiler moves it — zeroing the source. No clone needed.
2. **Auto-clone:** If the source variable is used after the store, the compiler deep-clones it so both the source and the destination have independent copies.
3. **Explicit `!` (override):** Forces a move regardless of analysis. The source is dead after — using it is a compile error. Use this when the compiler's liveness analysis is too conservative.

```gorget
Vector[Item] items = Vector[Item]()
Item a = Item("first")
items.push(a)                  # auto-move: a is dead after → zero-cost transfer
                               # (a is zeroed, items owns the data)

Item b = Item("second")
items.push(b)                  # auto-clone: b is used on next line → clone
print(b.name)                  # OK: b has its own independent copy

items.push(!b)                 # explicit move: zero-cost, b is dead after
```

This applies to: `.push()`, `.put()`, `.set()`, struct field initialization, and enum variant construction.

## Move (`!`) as Optimization

`!` explicitly transfers ownership, avoiding the clone. It is a performance hint, not a correctness requirement. The program is correct with or without `!`.

```gorget
v.push(a)                      # auto-move if dead, auto-clone if alive
v.push(!a)                     # always moves (a is dead after)
```

## Consuming Self (`!self`) for Equip Methods

Methods that return a new value constructed from self's fields can use
`!self` to consume the receiver, eliminating clone overhead:

```gorget
equip Config:
    Config with_width(!self, int w):
        return Config(w, self.height, self.title, self.fullscreen)
        #                             ^^^^^^^^^^
        #                             moved out (zero-cost), not cloned
```

With `!self`, resource-type field loads from self use `MoveZeroSource` —
the field value is moved out and the source field is zeroed. The struct's
drop function handles cleanup of any unconsumed fields (e.g., when
`with_title` replaces `self.title` with a new value, the old title stays
in the zeroed struct and is freed by the drop).

At the call site, temps auto-move (zero-cost), and named vars require
explicit `!` or are rejected by the borrow checker:

```gorget
# Temps: auto-move (zero-cost, invisible)
Config c = default_config().with_width(1920).with_title("game")

# Named vars: consumed, use-after-move is a compile error
Config c = Config(800, 600, "game", false)
Config c2 = c.with_width(1920)   # c is consumed
print(c.title)                    # ERROR: use of moved value `c`
```

## Mutable Borrow (`&`) Unchanged

`&` still means "the callee can mutate the caller's value directly." This bypasses CoW — the callee operates on the original data. The borrow checker ensures no aliases exist during the mutable borrow.

```gorget
void append(Vector[int] &v):
    v.push(42)                 # mutates caller's vector directly

Vector[int] nums = Vector[int]()
append(&nums)                  # nums is modified
```

## Static Analysis

The compiler determines at compile time where clones are needed. For each alias, it checks whether either side is mutated in the current scope. If yes, it inserts a clone before the first mutation. If the compiler cannot prove statically (value escapes to a closure, returned from function, stored in a collection), it conservatively clones at the point where it loses visibility.

No reference counting. No runtime checks. The compiler makes all decisions at compile time using existing infrastructure: provenance tracking, borrow origins, mutation analysis.

## Future Optimizations

1. **Smarter escape analysis:** Reduce conservative clones by proving more cases statically.
2. **Field-granularity CoW:** When only one field of a struct is mutated, clone only that field's container, not the entire struct.

## What This Replaces

- Implicit clone warnings (no longer needed — clones are always correct and intentional)
- Shallow copy bugs (eliminated — all copies are either pointers or deep clones)
- LIR per-element drop recipes (eliminated — collections with CoW are self-cleaning)
- The `should_unregister_string_args` leak heuristic (eliminated — struct fields are self-contained)
- String field `Str` views (replaced by `Ptr(GorgetString)` — uniform with collection fields)

## What Stays the Same

- `!` for explicit move (optimization)
- `&` for mutable borrow (shared mutation)
- Borrow checker enforcement (no `&` while aliases exist)
- Drop at scope exit (owned values are dropped, pointers are not)
- User-defined `Drop` trait (called before field drops)

## Self-Cleaning Collections

Once CoW eliminates shallow copies, collections become self-cleaning:

- `gorget_array_free` calls `elem_drop` per element before freeing the buffer.
- `gorget_map_free` calls `key_drop` per key and `val_drop` per value before freeing.
- No more LIR per-element drop tags or recipe system.
- Enum/struct drop functions just call `gorget_array_free`/`gorget_map_free` — the collection handles its own element cleanup.

This is safe because CoW guarantees every collection element is solely owned by that collection. No other variable holds a shallow reference to the element's inner data.

## CoW Materialization Points

When a borrowed value (Ptr) crosses an ownership boundary, the compiler
materializes it — clones the borrowed data so the new owner has an
independent copy. These are the SEVEN points where materialization occurs:

| Point | Trigger | Status |
|-------|---------|--------|
| 1. Assignment | `x = expr` where x is borrowed | Done (assign handler) |
| 2. Mutating method | `x.push(val)` where x is borrowed | Done (cow_before_mutation) |
| 3. Struct/enum init | `Foo(x)` where x is borrowed | Done (emit_enum_init_owned + LIR FieldLoad clone) |
| 4. Collection put | `v.push(x)` where x is borrowed | Done (clone_multi_use_resource_args) |
| 5. Return | `return x` where x is borrowed | Done (lower_return Ptr→T auto-clone) |
| 6. Move transfer | `consume(!x)` where x is borrowed | Done (Ownership::Move Ptr→clone) |
| 7. Field store | `self.field = x` where x is borrowed | Done (lower_field_assign Ptr→clone) |

Point 7 covers field assignments where the RHS is a `Ptr`-typed local
(a borrowed parameter or reference). Without cloning, the field and the
caller's original share the same heap allocation — when the caller drops
its copy, the field becomes a dangling pointer. `lower_field_assign` in
`assigns.rs` detects `Ptr`-typed sources via `pointee_type()` and emits
`clone_fn_for_ptr()` (e.g. `gorget_string_clone_to_owned` for strings,
`gorget_array_clone` for vectors) to produce an independently-owned copy.

All 7 points are implemented. `ensure_owned_string` has been deleted —
its role is now handled by the generic `is_non_owned_string` check in
the resource clone paths.

## Additional Safety Checks

### Closure Ptr capture

Closures capturing CoW alias variables (`Ptr(T)`) clone through the Ptr at capture
time, producing an independent owned `T` in the closure struct. Without this, the
raw Ptr copy would become stale if the source is mutated after capture.

```gorget
Vector[int] nums = Vector[int]()
nums.push(1)
nums.push(2)
auto snap = nums              # snap is Ptr alias of nums
auto f = (): print(snap.len())  # closure captures snap → clones
nums.push(3)                  # mutates nums — snap's clone unaffected
f()                           # prints 2, not 3
```

### For-loop iterator invalidation

The borrow checker rejects mutations on a collection during iteration,
regardless of element type:

```gorget
for item in items:
    items.push(new_item)    # ERROR: cannot mutate collection during iteration
```

Tracked via `for_loop_iterables` in the safety checker. The restriction applies
to all mutating methods (push, pop, remove, clear, etc.).

### Set deep-clone

`gorget_set_clone` deep-clones resource-type keys (strings). Previously it did
a shallow `memcpy`, causing double-free on `Set[String]` drop.

## Implementation Phases

| Phase | What | Status |
|-------|------|--------|
| 1a | Static mutation analysis pass — detect which locals are mutated | Done |
| 1b | Pointer semantics for variable assignment — aliased values stay as pointers | Done |
| 1c | Pointer semantics for function params — bare params clone on mutation | Done |
| 1d | Pointer semantics for collection reads — IndexLoad returns pointer, clone on mutation | Done (CollectionRef variant in local_ownership) |
| 1e | `!` optional for push/put/set | Done |
| 1f | Liveness analysis — full-function reverse walk for last-use detection | Done |
| 1g | VarDecl borrow propagation — typed bindings keep Ptr, no auto-clone | Done (34 tests need materialization points 3-6) |
| 2a | Unified `Type__drop` — one drop function per type, eliminate inline field walks | Done (emit_type_drop_fns) |
| 2b | Self-cleaning collections — elem_drop/val_drop/key_drop | Done |
| 2c | CoW materialization points 3-6 | Done |
| 2d | Delete `ensure_owned_string` — replaced by CoW materialization | Done |
