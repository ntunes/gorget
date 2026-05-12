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

A few resource types still require the explicit `!` operator to
transfer ownership:

- `Box[T]`, `Task`, `TaskGroup`, `Guard` — single-owner heap
  allocations whose value-semantics ARE move-semantics
- `Callable[...]` and closure values — closures hold captured-env
  references that aren't safe to alias
- `Owned[T]` — when you've explicitly asked the type system to track
  ownership transfers

For these, `Box[int] b = a` is a compile error; write `Box[int] b = !a`
(or `.clone()` for an independent copy).

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

The compiler handles all of this automatically. You don't need to think
about when clones happen — the rule is simple: borrows are free,
ownership costs a clone, and clones only happen when they're actually
needed.

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
