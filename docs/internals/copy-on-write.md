# Copy-on-Write Ownership Model

> **Status:** Phases 1a–1e, 2b implemented. Phase 1f in progress.
> **Date:** 2026-03-27
> **Supersedes:** Implicit clone warnings, `directive explicit-clone`, LIR per-element drop recipes.

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

```gorget
auto entry = v[i]              # pointer into v's storage
print(entry.name)              # read — zero cost
entry.name = "new"             # mutation on entry → clone v[i] into entry, mutate
```

```gorget
auto entry = cache.get("key")  # pointer into cache's storage
print(entry.len())             # read — zero cost, no clone
```

### Collection mutation while alias exists

```gorget
auto entry = v[i]              # pointer into v's storage
v.push(something)              # mutation on v → clone entry out first
                               # (v's buffer may relocate, entry would dangle)
```

### No difference between `auto` and explicit type

```gorget
auto x = v[i]                  # pointer — same behavior
String x = v[i]                # pointer — same behavior
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
3. **Runtime refcount fallback:** For cases where static analysis is too conservative, add optional refcount on collection/string headers to defer clones until they're truly needed. This is an optimization, not a correctness requirement.

## What This Replaces

- Implicit clone warnings (no longer needed — clones are always correct and intentional)
- The `directive explicit-clone` model (superseded — clones happen automatically at the right time)
- Shallow copy bugs (eliminated — all copies are either pointers or deep clones)
- LIR per-element drop recipes (eliminated — collections with CoW are self-cleaning)
- The distinction between "view" and "owned" string types (a String is always CoW)

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

## Implementation Phases

| Phase | What | Status |
|-------|------|--------|
| 1a | Static mutation analysis pass — detect which locals are mutated | Done |
| 1b | Pointer semantics for variable assignment — aliased values stay as pointers | Done |
| 1c | Pointer semantics for function params — bare params clone on mutation | Done |
| 1d | Pointer semantics for collection reads — IndexLoad returns pointer, clone on mutation | Done (cow_collection_refs wired up) |
| 1e | `!` optional for push/put/set | Done |
| 1f | Auto-move-when-dead at push/constructor — move if source dead, clone if alive | In progress |
| 2a | Unified `Type__drop` — one drop function per type, eliminate inline field walks | Steps 1-4 done, Step 5 blocked on 1f |
| 2b | Self-cleaning collections — elem_drop/val_drop/key_drop | Done |
