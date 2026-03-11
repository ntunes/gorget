# Smart Pointers

Sometimes ownership rules are too restrictive. You need shared ownership, heap
allocation, or interior mutability. Smart pointers provide these capabilities
while preserving safety.

---

## Box — Heap Allocation

`Box[T]` puts a value on the heap with single-owner semantics:

```gorget
from std.collections import Box

Box[int] b = Box(42)
print(f"{b.get()}")       # 42
b.set(100)
print(f"{b.get()}")       # 100
```

Use `Box` when you need:
- Heap allocation for large values
- Trait objects (polymorphic dispatch)
- Recursive data structures (a struct can't contain itself, but can contain a `Box` of itself)

### Dereferencing

```gorget
Box[int] b = Box(42)
int val = *b             # dereference
print(f"{val}")           # 42
```

### Boxing Closures

`Box` can hold closures for dynamic dispatch:

```gorget
Box[Callable[int(int)]] f = Box.new((n): n * 2)
print(f"{f(5)}")          # 10

int factor = 3
Box[Callable[int(int)]] g = Box.new((n): n * factor)
print(f"{g(7)}")          # 21
```

---

## Shared — Reference-Counted Ownership

`Shared[T]` allows multiple owners of the same value. The value is dropped when
the last owner goes away. Always thread-safe (atomic reference counting):

```gorget
Shared[int] s = Shared[int](42)
print(f"{s.get()}")       # 42
```

### Shared Bindings

The `shared` keyword creates shared variables for concurrent use:

```gorget
async void main():
    shared int x = 99
    Task[void] t = spawn print_value(x)
    t.await()
    print(f"{x}")
```

The compiler uses control-flow analysis to determine the right synchronization
strategy.

### Explicit Synchronization

Override with annotations when needed:

```gorget
shared(rwlock) Dict[str, str] cache = Dict[str, str]()
shared(atomic) int counter = 0
```

---

## Weak — Non-Owning References

`Weak[T]` holds a non-owning reference to a `Shared[T]` value. It doesn't prevent
the value from being dropped:

```gorget
Shared[int] s = Shared[int](42)
Weak[int] w = s.downgrade()

Option[Shared[int]] maybe = w.upgrade()
match maybe:
    case Some(upgraded):
        print(f"{upgraded.get()}")    # 42
    case None:
        print("value was dropped")
```

Use `Weak` to break reference cycles (e.g., parent-child relationships where both
sides hold references).

---

## Cell and RefCell — Interior Mutability

### Cell[T]

`Cell[T]` provides interior mutability for Copy types without runtime overhead:

```gorget
Cell[int] c = Cell[int](0)
c.set(42)
print(f"{c.get()}")    # 42
```

Only works with Copy types (integers, floats, bools).

### RefCell[T]

`RefCell[T]` provides interior mutability for any type with runtime borrow checking:

```gorget
RefCell[String] r = RefCell[String]("hello")
# Runtime borrow checks instead of compile-time
```

Panics at runtime if you violate the borrowing rules (e.g., two mutable borrows).
Use sparingly — compile-time checking is always preferable.

---

## Mutex and RwLock — Concurrent Access

### Mutex[T]

`Mutex[T]` wraps data with a lock for exclusive access:

```gorget
Mutex[int] counter = Mutex[int](0)

# In concurrent code:
# lock() returns the value; unlocks when scope exits
```

Only one thread can hold the lock at a time. Others block until it's released.

### RwLock[T]

`RwLock[T]` allows multiple readers or one writer:

```gorget
RwLock[Dict[str, int]] cache = RwLock[Dict[str, int]](Dict[str, int]())

# Multiple readers can access simultaneously
# Writers get exclusive access
```

Use `RwLock` when reads are much more frequent than writes.

---

## When to Use What

| Type | Use When |
|------|----------|
| `Box[T]` | Single owner, heap allocation, recursive types |
| `Shared[T]` | Multiple owners of the same data |
| `Weak[T]` | Breaking reference cycles |
| `Cell[T]` | Interior mutability for Copy types |
| `RefCell[T]` | Interior mutability for any type (runtime checks) |
| `Mutex[T]` | Shared mutable access across threads |
| `RwLock[T]` | Many readers, few writers across threads |

**Default to regular ownership.** Smart pointers are escape hatches — use them
when ownership rules don't fit your problem, not as the default approach.

---

## Summary

| Pointer | Ownership | Thread-Safe | Overhead |
|---------|-----------|-------------|----------|
| `Box[T]` | Single | N/A | Heap allocation |
| `Shared[T]` | Shared (ref-counted) | Yes (atomic) | Ref count + allocation |
| `Weak[T]` | Non-owning | Yes | Ref count check on upgrade |
| `Cell[T]` | Interior mutability | No | None (Copy types only) |
| `RefCell[T]` | Interior mutability | No | Runtime borrow tracking |
| `Mutex[T]` | Shared + mutable | Yes | Lock overhead |
| `RwLock[T]` | Shared + mutable | Yes | Lock overhead |
