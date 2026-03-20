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

## Copy vs Move Types

Not every type follows move semantics. Small, simple types are **copied** implicitly:

**Copy types** (no `!` needed):
- All integers: `int`, `int8`, `int16`, `int32`, `uint`, `uint8`, etc.
- All floats: `float`, `float32`
- `bool`, `char`
- Tuples of copy types

**Move types** (require `!` to transfer):
- `String`
- All structs and enums
- Collections: `Vector`, `Dict`, `Set`

```gorget
# Copy types — freely duplicated
int a = 42
int b = a          # a is still valid
print(f"{a} {b}")   # 42 42

# Move types — ownership transfers
String s1 = "hello"
String s2 = !s1    # s1 is now invalid
print(f"{s2}")      # hello
# print(f"{s1}")    # COMPILE ERROR: use after move
```

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

The keyword form `move` is equivalent:

```gorget
Message copy = move msg
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

## Summary

| Concept | Syntax | Meaning |
|---------|--------|---------|
| Copy type | `int b = a` | Implicit copy, both valid |
| Move | `!expr` or `move expr` | Transfer ownership |
| Move parameter | `void f(Type !name)` | Function takes ownership |
| Use after move | — | Compile error |
| Drop | `equip T with Drop: void drop(!self)` | Cleanup on scope exit |
| `with` statement | `with expr as name:` | Scoped resource management |
| Reassign after move | `x = new_value` | Revives the variable |
