# Ownership and Move Semantics

Every value in Gorget has exactly one owner. When the owner goes out of scope, the
value is dropped. This single rule eliminates use-after-free, double-free, and
dangling pointer bugs — at compile time, not runtime.

---

## The Ownership Rules

1. Every value has exactly **one owner**.
2. When the owner goes out of scope, the value is **dropped** (freed).
3. Ownership can be **transferred** (moved) using `!`.
4. After a move, the source variable is **invalid** — using it is a compile error.

---

## Copy vs Resource Types

Not every type follows move semantics. Small, simple types are **copied** implicitly:

**Copy types** (always cheap to duplicate):
- All integers: `int`, `int8`, `int16`, `int32`, `uint`, `uint8`, etc.
- All floats: `float`, `float32`
- `bool`
- Tuples of copy types

**Resource types** (own heap data, follow copy-on-write — see below):
- `String`
- All structs and enums with resource fields
- Collections: `Vector`, `Dict`, `Set`, `HashMap`, `HashSet`, `Deque`

For resource types, bare-identifier assignment creates a **borrow** (a
pointer to the same heap data) — no data is copied. The variables share
storage until one of them mutates, at which point the compiler clones
behind the scenes so each variable owns an independent copy.

```gorget
# Copy types — freely duplicated
int a = 42
int b = a          # a is still valid
print(f"{a} {b}")   # 42 42

# Resource types — bare-assign borrows (zero cost)
String s1 = "hello"
String s2 = s1     # s2 borrows from s1
print(s1)          # "hello"  — both names still valid
print(s2)          # "hello"
s2 = s2 + "!"      # mutation triggers clone — s1 unaffected
print(s1)          # "hello"
print(s2)          # "hello!"

# `!` is the explicit move opt-in
String s3 = "world"
String s4 = !s3    # explicit move — s3 now invalid
# print(s3)        # COMPILE ERROR: use after move
```

### When `!` is still required

Some resource types still require the explicit `!` operator (or
`.clone()`) to transfer ownership. This is **one principled rule plus a
few by-design members**.

The rule — **drop-purity**: a type with a custom `Drop` anywhere in its
transitive field graph is *drop-tainted*. Copying it implicitly would run
its custom `drop` code twice, so a bare `R b = a` is rejected:

```gorget
struct R:
    int id

equip R with Drop:
    void drop(!self):
        print(f"drop {self.id}")

R a = R(1)
R b = a            # COMPILE ERROR: would run R's drop twice
R c = !a           # OK — move (a is now invalid)
R d = b.clone()    # OK — an independent, separately-dropped copy
```

Taint is transitive: a `Vector[R]`, a tuple `(R, int)`, an `Option[R]`,
or a struct with an `R` field are all drop-tainted too. A **field or
index place** of a tainted type (`hh.r`, `v[0]`) must use `.clone()` —
`!hh.r` would be a partial move.

The by-design single-owner members follow the same `!`/`.clone()` rule:

- `Box[T]`, `Task`, `TaskGroup`, `Guard` — single-owner heap
  allocations whose value-semantics ARE move-semantics
- `Callable[...]` and closure values — closures hold captured-env
  references that aren't safe to alias
- `Owned[T]` — when you've explicitly asked the type system to track
  ownership transfers

For all of these, `Box[int] b = a` is a compile error; write
`Box[int] b = !a` (or `.clone()` for an independent copy). A fresh
temporary (`R b = R(1)`, `Box[int] b = make()`) is not a live place — it
moves without an operator and is never rejected. The refcounted/handle
types (`Shared[T]`, `Weak[T]`, `Mutex[T]`, `Channel[T]`) are the
sanctioned multi-owner escape hatch and are not drop-tainted by their
payload.

---

## Moving Values

The `!` operator transfers ownership:

```gorget
struct Message:
    String sender
    String text

Message msg = Message("Alice", "hello")
Message copy = !msg    # msg is moved to copy
# msg is now invalid
```

### Move in Function Calls

Functions declare how they receive values:

```gorget
void read(Message msg):         # borrows (default)
    print(msg.text)

void consume(Message !msg):     # takes ownership
    archive(msg)
```

At the call site:

```gorget
Message msg = Message("Alice", "hello")
read(msg)          # borrow — msg still valid
consume(!msg)      # move — msg now invalid
```

### Reviving a Moved Variable

Reassigning a moved variable makes it valid again:

```gorget
Message msg = Message("Alice", "hello")
consume(!msg)                            # msg is invalid
msg = Message("Bob", "reply")           # msg is valid again
read(msg)
```

---

## Scope and Drop

Values are automatically dropped when their owner goes out of scope:

```gorget
void process():
    String s = "hello"
    # use s...
    # s is dropped here when process() returns
```

Block scopes work too:

```gorget
void main():
    if true:
        String temp = "temporary"
        print(temp)
    # temp is dropped here — leaving the if block

    print("after if")
```

### Custom Drop

Implement the `Drop` trait for cleanup logic:

```gorget
struct Resource:
    String name

equip Resource with Drop:
    void drop(!self):
        print(f"dropping {self.name}")

void main():
    Resource r = Resource("alpha")
    print("using resource")
# Output:
# using resource
# dropping alpha
```

Drop runs automatically when the value goes out of scope. The `!self` parameter
means `drop` consumes the value.

### Drop Order

Multiple values in the same scope are dropped in **reverse declaration order**:

```gorget
void main():
    Resource a = Resource("first")
    Resource b = Resource("second")
# Output:
# dropping second
# dropping first
```

---

## The `with` Statement

For scoped resource management, `with` guarantees cleanup:

```gorget
with File.open("data.txt") as f:
    String content = f.read_all().unwrap()
    print(content)
# f.drop() called here, even if an error occurred
```

Multiple resources:

```gorget
with File.open("input.txt") as reader, File.create("output.txt") as writer:
    String data = reader.read_all().unwrap()
    writer.write(data)
# both closed here
```

The `with` statement is syntactic sugar for scoped ownership — the resource is
dropped when the block exits, regardless of how it exits.

---

## Move Restrictions

The compiler prevents dangerous patterns:

### No Move in Loops

```gorget
String s = "hello"
for i in 0..3:
    consume(!s)    # COMPILE ERROR: move in loop body
```

The first iteration would move `s`, leaving iterations 2 and 3 with an invalid
variable.

### No Double Move

```gorget
String s = "hello"
consume(!s)
consume(!s)        # COMPILE ERROR: use after move
```

### Conservative Branch Merging

```gorget
String s = "hello"
if condition:
    consume(!s)    # moved in one branch
else:
    pass
# s is treated as moved here (conservative)
print(s)           # COMPILE ERROR
```

If any branch moves a variable, the compiler treats it as moved after the branch.

---

## Copy-on-Write

For resource types (`String`, `Vector`, `Dict`, user structs/enums with
resource fields, etc.), Gorget uses copy-on-write semantics across all
the consume positions you'd reach for in everyday code: bare-identifier
assignment, function parameters, match scrutinees, closure captures,
and collection reads. Each of those produces a **borrow** (a pointer
to the original data) — no clone happens. The compiler inserts a clone
only at the first mutation through one of the aliases, giving the
mutator its own independent copy.

```gorget
Vector[int] a = [1, 2, 3]
Vector[int] b = a          # b borrows from a — zero cost
print(b.len())             # read through borrow — zero cost
b.push(4)                  # mutation → compiler clones for b
                           # a is still [1, 2, 3], b is [1, 2, 3, 4]
```

The same applies to user structs and enums:

```gorget
struct Spanned:
    String text
    int start

Spanned a = Spanned("hello", 0)
Spanned b = a              # b borrows from a — zero cost
print(b.text)              # "hello" — read through borrow
a.text = "world"           # mutation → clone for a, b keeps "hello"
print(a.text)              # "world"
print(b.text)              # "hello"
```

### `auto` vs Explicit Type

`auto` and an explicit type behave the same for bare-assign — both
produce a borrow that CoW-severs on mutation:

```gorget
auto x = obj.name              # x borrows from obj.name
String y = obj.name            # y also borrows from obj.name
```

The difference is in inference: `auto` lets the compiler pick the
type, an explicit type is a check that the RHS matches.

To get an independent owned copy up front (before any mutation), use
`.clone()`:

```gorget
String x = obj.name.clone()    # x is an independent owned copy
```

### Ownership boundaries: one rule everywhere

A few positions need to **own** their value rather than borrow it: a
collection put (`push`, `put`, `insert`), a struct or enum field at
construction (`S(name)`, `Some(name)`), a value `return`ed from a
function, and a closure capture. At every one of these the rule is the
same:

- If the source is **still used afterward** (live), the compiler
  **clones** it — the boundary gets its own independent copy and your
  original stays valid.
- If the source is at its **last use** (dead), the compiler **moves**
  it — zero cost, no clone.

```gorget
String name = greeting()
Holder h = Holder(name)    # name is read below → cloned into the field
print(name)                # still valid

Option[String] tag = Some(make_tag())   # the temp is dead → moved, no clone
```

There is no special case: a `push`, a `Some(...)`, a struct literal,
and a `return` all behave identically. You never write `.clone()` to
make a constructor work — borrow-by-default and clone-when-needed is
one mental model across the whole language.

### When does Gorget copy?

Almost never — and never speculatively. You write plain value
semantics; binding a value, reading it, and passing it to functions
don't copy anything. The compiler inserts a copy at exactly one
moment: when something you still hold would otherwise be changed
underneath you. And if that moment never arrives while the program
runs, no copy ever happens:

```gorget
Vector[String] names = ["ann", "bob"]
String first = names.get(0).unwrap()   # no copy — first refers to the element

if should_log:
    names.push("carol")   # the only moment first could be disturbed —
                          # right here, first quietly gets its own copy

print(first)              # "ann" either way
```

If `should_log` is false on a given run, this code performs zero
copies — not at the bind, not at the print. If it's true, exactly one
copy happens, at the `push`, and `first` still prints what you read
into it. The meaning of the program is plain value semantics either
way; the copies are just as few as if you had placed them by hand.

`.clone()` exists for one purpose only: to *force* an independent copy
up front, before any mutation. You never need it for correctness —
only when you explicitly want to pay for the copy now.

The compiler handles all of this automatically. You don't need to think
about when clones happen — the rule is simple: borrows are free,
ownership costs a clone, and a clone happens only at the moment it's
actually needed.

### Mutating a borrow gives you a private copy

A borrow is *read-only*. The moment you write through one, the compiler
hands you your own copy and the write lands there — the value you
borrowed from never changes. This holds for every borrow: a bare local,
a bare function parameter, a bare alias, a `for` loop variable.

```gorget
void relabel(Vector[Point] pts):    # bare param — a read-only borrow
    pts[0].label = "start"           # write → pts materializes a copy here
    print(pts[0].label)              # "start" — this function sees its copy

void main():
    Vector[Point] world = make_world()
    relabel(world)
    print(world[0].label)            # unchanged — the caller's data is intact
```

`relabel` cannot corrupt its caller: the write copies-on-write into a
private `pts`, and `world` is untouched. (Rust would *reject* that write;
Gorget copies instead — it is more tolerant.) Two consequences are worth
internalizing:

**1. The copy rebinds locally.** After the mutation, later reads *inside
the same function* see the copy (`"start"`), while the caller keeps the
original. The name now refers to the private copy.

**2. Each alias copies independently.** Two aliases of the same value
stay independent — mutating one never disturbs the other:

```gorget
Vector[int] a = source
Vector[int] b = source     # both borrow source
a.push(99)                 # a materializes ITS own copy
print(b.len())             # unchanged — b still borrows the original
```

That per-alias, at-the-write copy is what makes it copy-on-*write* rather
than "mutate in place."

### When you *want* the caller to see the change: `&`

Use a mutable borrow (`&`) when the whole point is to modify the
original. `&` propagates write access outward to the real owner, so the
change is visible to the caller:

```gorget
void relabel(Vector[Point] &pts):   # mutable borrow — write-through
    pts[0].label = "start"

void main():
    Vector[Point] world = make_world()
    relabel(&world)                  # & at the call site, too
    print(world[0].label)            # "start" — the caller sees it
```

**A method's `self` is just the first parameter, and the same split
applies.** Plain `self` is a read-only borrow: a write through it
materializes a private copy and the caller's object is untouched — perfect
for a read-only method, or one that scratches a private copy. When a method
is *meant* to change the receiver, declare it `&self`:

```gorget
equip Counter:
    void bump(self):        # plain self — read-only borrow
        self.n = self.n + 1  # writes a private copy; the caller's Counter is unchanged
    void bump_through(&self):   # &self — write-through
        self.n = self.n + 1  # the caller sees the increment
```

Because a write to plain `self` that the method never reads back is almost
always a mistake (you meant to change the caller), the compiler warns:
*"this writes to a private copy that is never read — the caller's value is
unchanged; did you mean `&self`?"*. The fix is usually a one-character edit:
`self` → `&self`.

The same split shows up in `for` loops. `for x in coll` borrows each
element read-only, so mutating `x` copies-on-write and the collection is
left intact; `for x in &coll` asks for write access, so the mutation
lands back in the collection (see the
[language reference](../language-reference.md), §6.11):

```gorget
for p in points:        # read-only — mutating p copies, points unchanged
    p.label = "x"

for p in &points:       # write-through — points IS modified
    p.label = "x"
```

---

## Summary

| Concept | Syntax | Meaning |
|---------|--------|---------|
| Copy type | `int b = a` | Implicit copy, both valid |
| Resource bare-assign | `String b = a` | Borrow, CoW-severs on mutation |
| Explicit move | `Type b = !a` | Transfer ownership; `a` invalid after |
| Explicit clone | `Type b = a.clone()` | Independent owned copy |
| Move parameter | `void f(Type !name)` | Function takes ownership |
| Box / closure assign | `Box[int] b = !a` | Still requires explicit `!` |
| Use after move | — | Compile error |
| Drop | `equip T with Drop: void drop(!self)` | Cleanup on scope exit |
| `with` statement | `with expr as name:` | Scoped resource management |
| Reassign after move | `x = new_value` | Revives the variable |
