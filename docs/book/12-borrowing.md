# Borrowing and References

Ownership prevents aliasing bugs, but constantly moving values would be impractical.
Borrowing lets you use a value without taking ownership — like lending a book rather
than giving it away.

---

## The Borrowing Rules

At any given point, for a given value, you may have either:

- **Any number of immutable borrows**, OR
- **Exactly one mutable borrow**

Never both. This is checked at compile time.

---

## Immutable Borrows

The default: pass a value to a function without giving it up. For resource
types, this borrow is automatic — the compiler passes a pointer, not a copy.
No data is duplicated. This is zero-cost and prevents aliasing bugs.

```gorget
void print_message(Message msg):    # immutable borrow (default)
    print(msg.text)

void main():
    Message msg = Message("Alice", "hello")
    print_message(msg)    # borrowed — msg still valid
    print_message(msg)    # can borrow again
```

Multiple immutable borrows are fine — reading doesn't conflict:

```gorget
void compare(Message a, Message b):
    if a.text == b.text:
        print("same")

compare(msg, msg)    # two immutable borrows of msg — allowed
```

---

## Mutable Borrows

To modify a value through a borrow, use `&`:

```gorget
void set_priority(Message &msg, int priority):
    msg.priority = priority

void main():
    Message msg = Message("Alice", "hello", 1)
    set_priority(&msg, 5)
    print(f"{msg.priority}")    # 5
```

The `&` appears in both the parameter declaration and the call site. This makes
mutation visible at every call site — no hidden side effects.

### Method Syntax

Methods use `&self` for mutable access:

```gorget
equip Message:
    void set_priority(&self, int p):
        self.priority = p

msg.set_priority(5)    # &self is implicit at call site
```

---

## Borrow Conflicts

The compiler prevents aliasing that could cause data races or corruption:

```gorget
# These are compile errors:
f(&x, &x)         # two mutable borrows of same value
f(x, &x)          # immutable + mutable borrow of same value
f(&x, !x)         # mutable borrow + move of same value
```

The rule is simple: you can read from many places or write from one place, but
never both at the same time.

---

## Auto-Borrowing

At method call sites, Gorget automatically borrows as needed:

```gorget
struct Counter:
    int value

equip Counter:
    void increment(&self):
        self.value += 1
    int get(self):
        return self.value

Counter c = Counter(0)
c.increment()          # auto-borrows mutably for &self
print(f"{c.get()}")     # auto-borrows immutably for self
```

You don't write `(&c).increment()` — the compiler inserts the borrow.

---

## Lifetimes

A borrow must not outlive the value it borrows from. Gorget never asks
you for lifetime annotations; the compiler tracks origins internally.

### Automatic Inference

```gorget
String get_greeting():
    return "hello"         # static / literal — always valid

String identity(String s):
    return s               # today: move or clone at the return boundary

String forward(String s):
    String local = s       # in-body alias (CoW borrow)
    return local           # today: move/clone into the caller's owned result
```

These all work without annotations. What the compiler does **today**:
- `"hello"` is a static literal — always valid
- `identity` / `forward` give the caller an **owned** `String` (move if
  the source is dead, clone if it is still live) — not a user-visible
  borrow of the parameter

**Not yet implemented:** keeping a compiler-internal **view** across a
function return when static provenance proves the result is a short-lived
projection of a live parameter/receiver (**return-view lazy
materialization**). That path is design-ruled (still zero annotations in
source) but not shipped — see `docs/language-design.md` §3.6.

---

## Structs Own Their Fields

A struct field is an ownership boundary: a struct owns its resource-type
fields and frees them when it is dropped. Storing a value into a field
therefore can't leave a borrow behind that outlives the struct — the
compiler materializes (clones) the borrow at the field-store boundary,
or you write `!`/`.clone()` to make the transfer explicit:

```gorget
struct Holder:
    String name

Holder make_holder(String s):
    return Holder(s)       # field-store boundary — `s` is cloned into the field

void main():
    Holder h = make_holder("hello")
    print(h.name)          # valid — h owns its own copy of the string
```

Because the field is owned, `Holder` is independent of the argument
passed to `make_holder`: the struct can outlive that source, and
mutating the source later doesn't disturb the field. This is the same
rule as everywhere else — borrow by default, clone only at an ownership
boundary (here, the struct field).

---

## Collection Element Borrowing

Reading an element from a collection returns a **read-only borrow** — a
zero-cost reference into the collection's storage:

```gorget
Vector[Player] players = get_players()
auto p = players.get(0).unwrap()  # p borrows player 0 — no copy
print(p.name)                     # read through the borrow — zero cost
```

Like every borrow, `p` is read-only: writing through it (`p.score += 10`)
copies-on-write into a private `p` and leaves `players` untouched — see
[Copy-on-Write](11-ownership.md#mutating-a-borrow-gives-you-a-private-copy).
To change the element *in the collection*, ask for write access — mutate
the place directly on a collection you own, or iterate mutably:

```gorget
players[0].score += 10       # direct place mutation — players[0] changes
for p in &players:           # mutable iteration — write-through
    p.score += 10
```

The read-only borrow propagates through field access and destructuring:

```gorget
auto ev = events.get(i).unwrap()   # &GameEvent
match ev:
    case .ItemPickup(cat, name, pos):
        # name is &String — borrow propagates from ev
        hud.pickup_text = name.clone()  # .clone() for ownership
```

To get an owned copy, use `.clone()`:

```gorget
Player owned = players.get(0).unwrap().clone()  # deep clone
```

### MutationWhileBorrowed

You cannot mutate a collection while a borrow into it exists:

```gorget
auto entry = v.get(0).unwrap()  # borrows from v
v.push(42)                       # ERROR: cannot mutate v while entry borrows it
```

The push might reallocate the buffer, invalidating the borrow. The compiler catches
this at compile time.

### For-Loop Iteration

A for-loop creates an implicit read-only borrow of the collection:

```gorget
for item in items:
    print(item)
    items.push(new_item)   # ERROR: cannot mutate during iteration
```

This applies to all mutating methods — push, pop, remove, clear, etc.

---

## Branch Merging

When control flow branches, the compiler is conservative:

```gorget
String s = "hello"
if condition:
    consume(!s)
else:
    pass
# s treated as potentially moved — using it is an error
```

If *any* branch moves or mutably borrows a variable, the compiler assumes the
worst case after the branch. This is safe but occasionally requires restructuring
code.

---

## Summary

| Concept | Syntax | Meaning |
|---------|--------|---------|
| Immutable borrow | `f(x)` | Read access, original stays valid |
| Mutable borrow | `f(&x)` | Write access, exclusive |
| Mutable parameter | `void f(Type &x)` | Declares mutable borrow |
| Auto-borrowing | `x.method()` | Compiler inserts borrow for `self`/`&self` |
| Borrow rule | — | Many readers OR one writer, never both |
