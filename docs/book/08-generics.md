# Generics

Sometimes you write a function and realize the logic doesn't depend on the type at
all. A "swap two values" function works the same way for integers, strings, and
structs. Generics let you write that function once and use it for any type.

---

## Generic Functions

A generic function declares type parameters in square brackets after the name:

```gorget
T identity[T](T x) = x
```

`T` is a *type parameter* — a placeholder that the compiler fills in at each call
site. When you call `identity[int](42)`, the compiler generates a version specialized
for `int`. When you call `identity[str]("hello")`, it generates another for `str`.

Type parameters can appear in parameter types, return types, and local variables:

```gorget
T first[T](T a, T b):
    T result = a
    return result
```

Multiple type parameters work too:

```gorget
A pick_left[A, B](A left, B right):
    return left
```

### Calling Generic Functions

Specify the type arguments explicitly:

```gorget
void main():
    int a = identity[int](42)
    float b = identity[float](3.14)
    str c = identity[str]("hello")
    int d = first[int](10, 20)
    print(f"{a}")    # 42
    print(f"{b}")    # 3.14
    print(f"{c}")    # hello
    print(f"{d}")    # 10
```

---

## Generic Structs

Structs can have type parameters too:

```gorget
struct Container[T]:
    T value

struct Pair[A, B]:
    A first
    B second
```

When constructing a generic struct, you must provide the type arguments — the compiler
doesn't infer them at construction sites:

```gorget
void main():
    Container[int] c = Container[int](42)
    Container[str] s = Container[str]("hello")
    Pair[int, str] p = Pair[int, str](10, "world")
    print(f"{p.first}")     # 10
    print(f"{p.second}")    # world
```

### Methods on Generic Structs

Attach methods with `equip`, using the same type parameters:

```gorget
equip Container[T]:
    T get(self):
        return self.value

    Container[T] wrap(self):
        return Container[T](self.value)

equip Pair[A, B]:
    A get_first(self):
        return self.first
    B get_second(self):
        return self.second
```

Each instantiation gets its own specialized methods:

```gorget
void main():
    Container[int] c = Container[int](42)
    print(f"{c.get()}")              # 42

    Container[int] c2 = c.wrap()
    print(f"{c2.get()}")             # 42

    Pair[int, str] p = Pair[int, str](10, "world")
    print(f"{p.get_first()}")        # 10
    print(f"{p.get_second()}")       # world
```

### Nesting Generics

Generic types compose freely:

```gorget
Container[Container[int]] nested = Container[Container[int]](Container[int](99))
Container[int] inner = nested.get()
print(f"{inner.get()}")    # 99
```

This extends to standard library types:

```gorget
Vector[Vector[int]] grid = Vector[Vector[int]]()
Dict[str, Vector[int]] groups = Dict[str, Vector[int]]()
Option[Vector[int]] maybe_items = Some(items)
```

---

## Generic Enums

Enums can be generic too. In fact, two of the most important types in Gorget are
generic enums:

```gorget
enum Option[T]:
    Some(T)
    None

enum Result[T, E]:
    Ok(T)
    Error(E)
```

These are built-in — you don't need to define them. Their variants (`Some`, `None`,
`Ok`, `Error`) are available bare, without qualification:

```gorget
Option[int] x = Some(42)
Option[str] name = None()
Result[int, str] ok = Ok(100)
Result[int, str] err = Error("failed")
```

User-defined generic enums follow the same pattern:

```gorget
enum Either[A, B]:
    Left(A)
    Right(B)

Either[int, str] val = Either.Left(42)
```

Note: user-defined enum variants require qualified access (`Either.Left`), while the
built-in `Option` and `Result` variants stay bare.

---

## Trait Bounds

An unconstrained type parameter accepts any type. Often that's too broad — you need
the type to support certain operations. Trait bounds constrain what types are
acceptable.

### Inline Bounds

Place the trait name before the type parameter:

```gorget
trait Printable:
    str show(self)

T echo[Printable T](T x):
    return x
```

Only types that implement `Printable` can be passed to `echo`. The compiler checks
this at every call site:

```gorget
struct Num:
    int val

equip Num with Printable:
    str show(self):
        return "num"

void main():
    Num n = Num(42)
    Num m = echo[Num](n)
    print(f"{m.show()}")    # num
```

### Multiple Bounds

Combine bounds with `&`:

```gorget
void log_and_compare[Displayable & Comparable T](T a, T b):
    print(f"{a}")
    if a > b:
        print("a is larger")
```

The type must implement *all* listed traits.

### Bounded Generic Structs

Trait bounds work on struct type parameters too. The bound is enforced wherever the
struct is instantiated:

```gorget
struct SortedPair[Comparable T]:
    T lo
    T hi
```

---

## Implementing Traits for Generic Types

You can equip a generic struct with a trait:

```gorget
trait Describable:
    int code(self)

struct Container[T]:
    T value

equip Container[T] with Describable:
    int code(self):
        return 42
```

Each monomorphized instantiation gets its own implementation:

```gorget
void main():
    Container[int] c1 = Container[int](100)
    Container[str] c2 = Container[str]("hello")
    print(f"{c1.code()}")    # 42
    print(f"{c2.code()}")    # 42
```

Default methods work with generic types:

```gorget
trait Labeled:
    int label(self)
    int full_label(self):
        return self.label() * 10

struct Wrapper[T]:
    T inner

equip Wrapper[T] with Labeled:
    int label(self):
        return 7

void main():
    Wrapper[int] w = Wrapper[int](10)
    print(f"{w.label()}")        # 7
    print(f"{w.full_label()}")   # 70
```

---

## Monomorphization

Gorget compiles generics by *monomorphization*: each unique combination of type
arguments produces a specialized copy of the code. When you write:

```gorget
T identity[T](T x) = x

void main():
    identity[int](42)
    identity[str]("hello")
    identity[float](3.14)
```

The compiler generates three separate functions — one for `int`, one for `str`, one
for `float`. At runtime, there's no indirection and no type erasure. A generic
function is exactly as fast as a hand-written specialized version.

The trade-off: many distinct instantiations increase binary size. In practice this
is rarely a problem, and the performance guarantee is worth it.

---

## Summary

| Concept | Syntax | Example |
|---------|--------|---------|
| Generic function | `T f[T](T x)` | `T identity[T](T x) = x` |
| Generic struct | `struct S[T]: T field` | `struct Container[T]: T value` |
| Generic enum | `enum E[T]: Variant(T)` | `enum Option[T]: Some(T), None` |
| Explicit type args | `f[Type](args)` | `identity[int](42)` |
| Struct construction | `S[Type](args)` | `Container[int](42)` |
| Trait bound | `[Trait T]` | `T echo[Printable T](T x)` |
| Multiple bounds | `[A & B T]` | `[Displayable & Cloneable T]` |
| Generic equip | `equip S[T] with Trait:` | Methods for any instantiation |
| Monomorphization | Automatic | Zero-cost generics at runtime |
