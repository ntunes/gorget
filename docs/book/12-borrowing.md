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

The default: pass a value to a function without giving it up.

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
void set_priority(&Message msg, int priority):
    msg.priority = priority

void main():
    Message msg = Message("Alice", "hello", 1)
    set_priority(&msg, 5)
    print(f"{msg.priority}")    # 5
```

The `&` appears in both the parameter declaration and the call site. This makes
mutation visible at every call site — no hidden side effects.

The keyword form `mutable` is equivalent:

```gorget
void set_priority(mutable Message msg, int priority):
    msg.priority = priority

set_priority(mutable msg, 5)
```

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

A borrow must not outlive the value it borrows from. Gorget infers lifetimes
automatically in most cases.

### Automatic Inference

The compiler tracks where borrows come from and ensures they're valid:

```gorget
str get_greeting():
    return "hello"         # string literal — always valid

str identity(str s):
    return s               # returns input — lifetime follows parameter

str forward(str s):
    str local = s          # alias — same lifetime
    return local
```

These all work without annotations. The compiler sees that:
- `"hello"` is a static literal — always valid
- `identity` returns its parameter — the result lives as long as the input
- `forward` assigns to a local then returns — same lifetime chain

### The `live` Annotation

When the compiler can't infer lifetimes (trait methods, FFI, ambiguous cases),
annotate explicitly:

```gorget
str first_live(live str a, str b):
    return a
```

The `live` annotation tells the compiler that the return value borrows from `a`.
Without it, with two `str` parameters and no body to analyze (e.g., in a trait
declaration), the compiler wouldn't know which parameter the result depends on.

### Named Lifetime Groups

For complex cases with multiple borrow sources:

```gorget
str pick(live(x) str a, live(y) str b) where x outlives y:
    return a
```

Named groups (`x`, `y`) let you express relationships between lifetimes. The
`where` clause declares that `x` outlives `y`.

---

## Structs with Borrowed Fields

Structs can hold borrowed values:

```gorget
struct View:
    str name

View make_view(str s):
    return View(s)         # View borrows from s

void main():
    View v = make_view("hello")
    print(v.name)          # valid — "hello" is static
```

The compiler tracks that `View.name` borrows from the argument passed to
`make_view`. The `View` cannot outlive that source.

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
| Mutable borrow | `f(&x)` or `f(mutable x)` | Write access, exclusive |
| Mutable parameter | `void f(&Type x)` | Declares mutable borrow |
| Auto-borrowing | `x.method()` | Compiler inserts borrow for `self`/`&self` |
| Lifetime annotation | `live str s` | Explicit borrow tracking |
| Named lifetimes | `live(name) str s` | Grouped lifetime relationships |
| Borrow rule | — | Many readers OR one writer, never both |
