# Traits

A struct holds data. A trait defines behavior — a contract that types can satisfy.
If a struct has the right methods, it can be used anywhere the trait is expected.
Traits are how Gorget achieves polymorphism without inheritance.

---

## Defining a Trait

A trait is a named set of method signatures:

```gorget
trait Shape:
    float area(self)
    String describe(self)
```

Any type that implements `Shape` must provide both methods. The `self` parameter
means the method is called on an instance: `my_shape.area()`.

Traits can require multiple methods, or just one:

```gorget
trait Printable:
    String show(self)
```

---

## Equipping Types with Traits

The `equip` keyword attaches trait implementations to a type:

```gorget
struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return 3.14159 * self.radius * self.radius
    String describe(self):
        return f"circle with radius {self.radius}"
```

Now `Circle` satisfies the `Shape` contract. You can call `area()` and `describe()`
on any `Circle` instance.

You must implement every method the trait requires. Missing one is a compile error:

```gorget
equip Circle with Shape:
    float area(self):
        return 3.14159 * self.radius * self.radius
    # ERROR: missing required method `describe`
```

### Inherent Methods

You can also attach methods to a type without a trait — these are *inherent methods*:

```gorget
equip Circle:
    float diameter(self):
        return self.radius * 2.0
```

Inherent methods are always available on the type. They don't need a trait to exist.

---

## Default Methods

A trait can provide a default implementation for some methods. Types that implement
the trait can override the default or inherit it:

```gorget
trait Greeter:
    String name(self)
    String greeting(self):
        return "hello"

struct English:
    String person

struct French:
    String person

equip English with Greeter:
    String name(self):
        return self.person
    # greeting() inherited — returns "hello"

equip French with Greeter:
    String name(self):
        return self.person
    String greeting(self):
        return "bonjour"    # override the default
```

```gorget
void main():
    English e = English("Alice")
    French f = French("Bob")
    print(f"{e.greeting()} {e.name()}")   # hello Alice
    print(f"{f.greeting()} {f.name()}")   # bonjour Bob
```

Default methods can call other methods in the same trait:

```gorget
trait Summary:
    String title(self)
    String body(self)
    String full(self):
        return f"{self.title()}: {self.body()}"
```

Any type implementing `Summary` gets `full()` for free — it just needs `title()` and
`body()`.

---

## Trait Inheritance

A trait can extend another trait. Implementing the child requires implementing the
parent too:

```gorget
trait Named:
    String name(self)

trait Greeter extends Named:
    String greet(self)
```

To equip a type with `Greeter`, you must provide both `name()` and `greet()`:

```gorget
struct Person:
    String first_name

equip Person with Greeter:
    String name(self):
        return self.first_name
    String greet(self):
        return f"hi, I'm {self.name()}"
```

Default methods from parent traits are inherited through the chain:

```gorget
trait Base:
    int value(self)
    int doubled(self):
        return self.value() * 2

trait Extended extends Base:
    int extra(self)

struct Thing:
    int x

equip Thing with Extended:
    int value(self):
        return self.x
    int extra(self):
        return self.x + 100
    # doubled() inherited from Base — returns self.value() * 2
```

---

## Trait Bounds

When you write a generic function, you can require that the type parameter satisfies
a trait:

```gorget
trait Printable:
    String show(self)

T echo[Printable T](T x):
    return x
```

The bound `Printable T` means: "T can be any type, as long as it implements
`Printable`." Calling `echo` with a type that doesn't implement `Printable` is a
compile error.

Multiple bounds use `&`:

```gorget
void log_and_copy[Displayable & Cloneable T](T item):
    print(f"{item}")
    T backup = item.clone()
```

Trait bounds are covered in more detail in the next chapter on generics.

---

## Trait Delegation with `via`

When a struct wraps another type, you can delegate trait implementations to an inner
field:

```gorget
trait Showable:
    String show(self)

struct Inner:
    String label

equip Inner with Showable:
    String show(self):
        return self.label

struct Outer:
    Inner inner

equip Outer with Showable via inner
```

No colon, no block, no `pass` — just the declaration. `Outer.show()` automatically
forwards to `self.inner.show()`. The inner field's type must implement the target
trait.

You can also provide a block to override specific methods while delegating the rest:

```gorget
equip Outer with Showable via inner:
    String show(self):
        return f"Outer: {self.inner.show()}"
```

---

## Built-in Traits

Gorget provides a set of built-in traits that hook into language features. You don't
need to define these — they exist in the prelude.

### Displayable

Enables f-string interpolation and `print()`:

```gorget
struct Point:
    float x
    float y

equip Point with Displayable:
    String display(self):
        return f"({self.x}, {self.y})"

void main():
    Point p = Point(3.0, 4.0)
    print(f"{p}")    # (3.0, 4.0)
```

### Equatable

Enables `==` and `!=`:

```gorget
equip Point with Equatable:
    bool eq(self, Point other):
        return self.x == other.x and self.y == other.y

void main():
    Point a = Point(1.0, 2.0)
    Point b = Point(1.0, 2.0)
    if a == b:
        print("equal")    # equal
```

### Comparable

Enables `<`, `>`, `<=`, `>=` by returning -1, 0, or 1:

```gorget
equip Point with Comparable:
    int compare(self, Point other):
        float mag_self = self.x * self.x + self.y * self.y
        float mag_other = other.x * other.x + other.y * other.y
        if mag_self < mag_other:
            return -1
        elif mag_self > mag_other:
            return 1
        return 0
```

### Cloneable

Enables deep copying:

```gorget
equip Point with Cloneable:
    Point clone(self):
        return Point(self.x, self.y)

Point copy = p.clone()
```

### Hashable

Enables use as `Dict` keys and `Set` elements. State-based:
implementations forward each field into the caller's `FxHasher`
(imported from `std.hash`) instead of producing a standalone int.

```gorget
from std.hash import FxHasher

equip Point with Hashable:
    void hash(self, FxHasher &h):
        self.x.hash(&h)
        self.y.hash(&h)
```

For a one-shot int digest (e.g. in tests), use `hash_of[T](v)`:

```gorget
from std.hash import hash_of

int h = hash_of[Point](p)
```

### Drop

Auto-cleanup when a value goes out of scope or a `with` block ends. The `!` in
`^self` means the method consumes ownership:

```gorget
struct Connection:
    int fd

equip Connection with Drop:
    void drop(!self):
        close_fd(self.fd)

# Automatic cleanup:
with Connection(open_fd("db")) as conn:
    conn.query("SELECT 1")
# conn.drop() called here
```

### Iterator and Iterable

Enable `for` loop iteration:

```gorget
struct Counter:
    int current
    int max

equip Counter with Iterator[int]:
    Option[int] next(&self):
        if self.current < self.max:
            int val = self.current
            self.current += 1
            return Some(val)
        return None()

void main():
    for i in Counter(0, 5):
        print(f"{i}")    # 0 1 2 3 4
```

`Iterator[T]` requires `next(&self)` returning `Option[T]`. `Iterable[T]` requires
`iter(&self)` returning an `Iterator[T]` — collections implement `Iterable`, iterators
implement `Iterator`.

### Parseable

Fallible string parsing:

```gorget
Option[int] n = int.parse("42")       # Some(42)
Option[int] bad = int.parse("hello")   # None
int port = int.parse(port_str).unwrap_or(8080)
```

Built-in for all numeric primitives.

### Default

Provides a zero/default value:

```gorget
@derive(Default)
struct Config:
    int width
    int height
    bool enabled
    String name

Config c = Config.default()    # Config(0, 0, false, "")
```

### Full Built-in Trait Table

| Trait | Required Method | Enables |
|-------|----------------|---------|
| `Displayable` | `String display(self)` | f-string interpolation, `print()` |
| `Equatable` | `bool eq(self, Self other)` | `==` and `!=` |
| `Comparable` | `int compare(self, Self other)` | `<`, `>`, `<=`, `>=` |
| `Hashable` | `void hash(self, FxHasher &h)` | `Dict` keys, `Set` elements |
| `Cloneable` | `Self clone(self)` | Deep copying |
| `Drop` | `void drop(!self)` | Auto-cleanup on scope exit |
| `Iterator[T]` | `Option[T] next(&self)` | `for` loop iteration |
| `Iterable[T]` | `Iterator[T] iter(&self)` | `for` loop on collections |
| `Default` | `Self default()` (static) | Zero/default construction |
| `Parseable` | `Option[Self] parse(String)` (static) | String parsing |
| `Measurable` | `int len(self)` | `len(x)` free function |
| `From[T]` | `Self from(T)` (static) | Infallible conversion |
| `TryFrom[T]` | `Result[Self, String] try_from(T)` (static) | Fallible conversion |
| `Add[Out]` .. `Neg[Out]` | Various | Operator overloading |
| `Index[K, V]` | `V get(self, K key)` | `a[k]` read access |
| `IndexMut[K, V]` | `void set(&self, K key, V value)` | `a[k] = v` write access |

---

## Deriving Traits

Writing `eq` for a struct with five fields is tedious when the answer is "compare all
of them." The `@derive` attribute generates implementations automatically:

```gorget
@derive(Equatable, Displayable, Cloneable, Hashable)
struct Point:
    float x
    float y
```

This generates `eq`, `display`, `clone`, and `hash` by operating on all fields.
Works on enums too:

```gorget
@derive(Equatable, Displayable, Cloneable)
enum Color:
    Red
    Green
    Blue(int)

void main():
    Color r = .Red
    Color r2 = .Red
    if r == r2:
        print("colors equal")    # colors equal
```

**Derivable traits:** `Equatable`, `Displayable`, `Debuggable`, `Cloneable`, `Hashable`, `Default`,
`Serializable`, `Deserializable`. Single-field structs can also derive `From` and
`TryFrom`.

For `Default`, the derived implementation zero-initializes all fields (0 for numbers,
`""` for strings, `false` for bools). If you need custom defaults, implement it
manually:

```gorget
struct Nested:
    Point origin
    int count

equip Nested with Default:
    Nested default():
        return Nested(Point.default(), 0)
```

---

## Summary

| Concept | Syntax | Purpose |
|---------|--------|---------|
| Define a trait | `trait Name: methods` | Declare a behavior contract |
| Implement a trait | `equip Type with Trait: methods` | Satisfy the contract |
| Inherent methods | `equip Type: methods` | Add methods without a trait |
| Default methods | Method body in trait definition | Provide fallback implementation |
| Trait inheritance | `trait Child extends Parent:` | Require parent's methods too |
| Trait bounds | `[Trait T]` or `[A & B T]` | Constrain generic type parameters |
| Delegation | `equip Type with Trait via field` | Forward to inner field |
| Derive | `@derive(Trait1, Trait2)` | Auto-generate implementations |
