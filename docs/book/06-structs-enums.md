# Structs and Enums

Structs group data into named fields. Enums define a type that can be one of several
variants. Together they model almost any data shape.

---

## Structs

### Definition

```gorget
struct Point:
    float x
    float y

struct Person:
    String name
    int age
    bool active
```

### Construction

Structs are constructed positionally:

```gorget
Point p = Point(1.0, 2.0)
Person alice = Person("Alice", 30, true)
```

### Field Access

```gorget
float x = p.x
String name = alice.name
print(f"{alice.name} is {alice.age}")
```

---

## Methods

Methods are attached to types using `equip`:

```gorget
equip Point:
    float magnitude(self):
        return (self.x * self.x + self.y * self.y) as float

    Point translate(self, float dx, float dy):
        return Point(self.x + dx, self.y + dy)
```

`self` refers to the instance the method is called on:

```gorget
Point p = Point(3.0, 4.0)
print(f"{p.magnitude()}")           # 25.0
Point p2 = p.translate(1.0, 1.0)
```

### Mutable Methods

Methods that modify the instance take `&self`:

```gorget
struct Counter:
    int value

equip Counter:
    void increment(&self):
        self.value += 1

    int get(self):
        return self.value

void main():
    Counter c = Counter(0)
    c.increment()
    c.increment()
    print(f"{c.get()}")    # 2
```

### Static Methods

Methods without `self` are static — called on the type, not an instance:

```gorget
equip Point:
    static Point origin():
        return Point(0.0, 0.0)

Point o = Point.origin()
```

---

## Enums

### Basic Enums

```gorget
enum Direction:
    North
    South
    East
    West
```

### Variant Construction

User-defined enum variants require qualified access. Parentheses are optional for
nullary (no-payload) variants:

```gorget
Direction d = Direction.North     # no parens needed for nullary
Direction e = Direction.North()   # parens also accepted
```

### Dot-Shorthand

When the type is known from context, use `.Variant`:

```gorget
Direction d = .North          # type known from declaration
d = .South                    # type known from variable
```

Dot-shorthand works in declarations, assignments, returns, function arguments,
and match patterns. Parentheses are required only for variants that carry data.

### Enums with Payloads

Variants can carry data:

```gorget
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Triangle(float, float, float)

Shape s = Shape.Circle(5.0)
Shape r = .Rectangle(3.0, 4.0)
```

### Pattern Matching on Enums

```gorget
match s:
    case Circle(radius):
        print(f"circle with radius {radius}")
    case Rectangle(w, h):
        print(f"rectangle {w} x {h}")
    case Triangle(a, b, c):
        print(f"triangle with sides {a}, {b}, {c}")
```

### Bare Variants

The built-in `Option` and `Result` variants are available without qualification:

```gorget
Option[int] x = Some(42)       # not Option.Some(42)
Option[int] y = None()
Result[int, String] ok = Ok(100)
Result[int, String] err = Error("fail")
```

This is a prelude privilege — user-defined enums use qualified access or
dot-shorthand.

### Glob Imports

To bring all variants of a user-defined enum into bare scope:

```gorget
from gg.log import LogLevel.*

LogLevel level = Info()         # bare access via glob import
```

---

## Newtypes

A newtype wraps an existing type to create a distinct type — the compiler treats
them as different, preventing accidental mixing:

```gorget
newtype UserId(int)
newtype Meters(float)
newtype Milliseconds(int)
```

### Construction and Access

```gorget
UserId id = UserId(42)
Meters dist = Meters(3.14)

print(f"{id.0}")       # 42 — access inner value via .0
print(f"{dist.0}")     # 3.14
```

The inner value is accessed with `.0`. This is intentionally explicit — if you're
reaching through the newtype, you should know you're doing it.

### Why Newtypes

Without newtypes, two `int` parameters can be swapped by accident:

```gorget
# Dangerous — easy to swap user_id and account_id
void transfer(int user_id, int account_id, int amount):
    ...
```

With newtypes, the compiler catches the mistake:

```gorget
newtype UserId(int)
newtype AccountId(int)

void transfer(UserId user, AccountId account, int amount):
    ...

transfer(AccountId(1), UserId(2), 100)   # COMPILE ERROR: type mismatch
```

---

## Combining Structs and Enums

Structs and enums compose freely:

```gorget
struct Address:
    String street
    String city

enum ContactMethod:
    Email(String)
    Phone(String)
    Mail(Address)

struct Person:
    String name
    ContactMethod preferred_contact

Person p = Person("Alice", .Email("alice@example.com"))

match p.preferred_contact:
    case Email(addr):
        print(f"email: {addr}")
    case Phone(num):
        print(f"call: {num}")
    case Mail(address):
        print(f"mail to {address.city}")
```

---

## Summary

| Concept | Syntax | Example |
|---------|--------|---------|
| Struct | `struct Name: fields` | `struct Point: float x, float y` |
| Construction | `Type(args)` | `Point(1.0, 2.0)` |
| Inherent method | `equip Type: methods` | `equip Point: float mag(self)` |
| Mutable method | `&self` parameter | `void inc(&self): self.val += 1` |
| Static method | No `self` | `static Point origin(): ...` |
| Enum | `enum Name: variants` | `enum Dir: North, South` |
| Qualified access | `Type.Variant()` | `Direction.North()` |
| Dot-shorthand | `.Variant()` | `.North()` (when type is known) |
| Payload variant | `Variant(types)` | `Circle(float)` |
| Bare variants | `Some`, `None`, `Ok`, `Error` | Built-in only |
| Newtype | `newtype Name(type)` | `newtype UserId(int)` |
