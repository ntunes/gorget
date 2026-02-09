# Gorget Language Design - Brainstorming Document

## Vision

**Gorget** = Rust's memory safety + Python's indentation + C/Java's type declarations

A systems-capable language that reads like pseudocode but compiles to safe, efficient machine code. The goal: make ownership and borrowing *feel natural* rather than fighting the programmer.

---

## 1. Core Principles

1. **Safe by default** - no null, no data races, no use-after-free
2. **Readable first** - code should look clean; minimize sigils and noise
3. **Explicit types at boundaries** - function signatures are fully typed; locals can be inferred
4. **Mutable by default, const opt-in** - variables are mutable; use `const` for immutability
5. **Zero-cost abstractions** - traits, generics, and closures compile away
6. **No garbage collector** - ownership + borrowing, like Rust

---

## 2. Basic Syntax

### 2.1 Variables

```gorget
int x = 5              # mutable by default (C/Java-like)
const int y = 10       # immutable (const, like C/C++/JS)
auto name = "gorget"    # type inferred (mutable)
const auto pi = 3.14   # type inferred (immutable)
```

No semicolons. Newline terminates statements. No curly braces - indentation defines blocks.

### 2.2 Primitive Types

```
int8   int16   int32   int64    (signed; `int` aliases int64)
uint8  uint16  uint32  uint64   (unsigned; `uint` aliases uint64)
float32  float64                 (`float` aliases float64)
bool
char                             (Unicode scalar value, 4 bytes)
```

### 2.3 Functions

C/Java-style: return type before name, typed parameters.

```gorget
int add(int a, int b):
    return a + b

void greet(str name):
    print("Hello, {name}")

# Expression body shorthand for simple functions
int double(int x) = x * 2
```

`void` means no return value. `str` is an immutable string slice (like Rust's `&str`). `String` is an owned, heap-allocated string.

### 2.4 Entry Point

```gorget
void main():
    print("Hello, World!")
```

### 2.5 Logical Operators (Python-style)

```gorget
if not ready:           # NOT (frees ! for move operator)
if a and b:             # AND
if a or b:              # OR
```

---

## 3. Ownership & Borrowing

The heart of the language. Three modes of passing data, with visual "loudness" matching danger level.

### 3.1 The Three Modes

| Mode | Declaration | Call Site | Meaning |
|------|------------|-----------|---------|
| Immutable borrow | `String s` | `f(name)` | Read-only access, caller keeps ownership |
| Mutable borrow | `String &s` | `f(&name)` | Read+write access, caller keeps ownership |
| Move (ownership) | `String !s` | `f(!name)` | Full ownership transfer, caller loses access |

```gorget
# Immutable borrow (default - no symbol, safest)
void print_len(String s):
    print(s.len())

# Mutable borrow (& between type and name)
void push_exclaim(String &s):
    s.push('!')

# Ownership transfer (! between type and name)
void consume(String !s):
    self.data = s              # can store permanently
    # s is freed when no longer needed
```

At call sites, symbols mirror declarations:
```gorget
String name = "hello"
print_len(name)          # immutable borrow - name still valid
push_exclaim(&name)      # mutable borrow - name still valid (now "hello!")
consume(!name)           # moved - name is GONE after this
# print(name)            # COMPILE ERROR: name was moved
```

### 3.2 Ownership (Move Semantics)

For non-Copy types (String, Vector, etc.), assignment with `!` moves:
```gorget
String s1 = "hello"
String s2 = !s1          # explicit move, s1 is invalid
# print(s1)              # COMPILE ERROR
```

Primitive types (int, float, bool, char) are Copy - always copied automatically:
```gorget
int a = 5
int b = a                # just copies, both valid (no ! needed)
```

### 3.3 The Borrow Rules (same as Rust)

At any given time, for a given piece of data, you can have **either**:
- Any number of immutable borrows (`String s`), OR
- Exactly one mutable borrow (`String &s`)

Never both simultaneously. Enforced at compile time. This prevents data races and aliasing bugs.

### 3.4 Lifetimes

**The compiler infers lifetimes from function bodies automatically.** Most programmers never write lifetime annotations.

When explicit annotation is needed (e.g., trait methods without bodies), use the `live` keyword on parameters whose data the return value depends on:

```gorget
# Compiler infers: return borrows from both x and y (no annotation needed)
str longer(str x, str y):
    if x.len() > y.len():
        return x
    return y

# Explicit: when compiler can't infer (e.g., trait methods)
trait Container:
    str get(live Container self, int index)
    # live tells compiler: return value depends on self's data

# Combining live with & (mutable borrow whose data lives in the return)
str process(live String &data):
    data.sort()
    return data.first()
```

**Lifetime design tiers:**
1. **Auto-inference** (99% of code) - compiler analyzes function body, no annotations needed
2. **`live` keyword** (~1% of code) - mark parameters whose data the return value depends on
3. **Explicit lifetime variables** (almost never) - `[life a]` for truly exotic cases:

```gorget
# Tier 3: Explicit lifetime variables (rare — e.g., self-referential structures)
struct Parser[life a]:
    str[a] input            # input has lifetime 'a'
    int position

    str[a] remaining(self):
        return self.input[self.position..]
```

---

## 4. Type System

### 4.1 Structs

```gorget
struct Point:
    float x
    float y

struct Person:
    String name
    int age

# Usage
Person alice = Person("Alice", 30)    # mutable by default
alice.age = 31                        # OK
const Person bob = Person("Bob", 25)  # immutable
# bob.age = 26                        # COMPILE ERROR
```

### 4.2 Enums (Algebraic Data Types)

```gorget
enum Color:
    Red
    Green
    Blue
    Custom(uint8, uint8, uint8)

enum Option[T]:
    Some(T)
    None

enum Result[T, E]:
    Ok(T)
    Error(E)
```

### 4.2.1 Option Sugar

`Option[T]` is Gorget's null replacement. Rich sugar makes it ergonomic:

```gorget
Option[String] name = Some("Alice")
Option[int] age = None

# Pattern matching with 'is'
if name is Some(n):
    print("Name: {n}")

# Optional chaining (?.)
# Returns None if any step is None, otherwise the final value
auto len = user?.name?.len()          # Option[int]
auto city = user?.address?.city       # Option[String]

# Nil coalescing (??)
# Unwraps the Option, or uses the default if None
String display = user?.name ?? "anonymous"
int count = map.get(key) ?? 0

# Combining ?. and ??
String city = user?.address?.city ?? "unknown"

# ? for early return (function must return Option[T])
Option[String] get_user_email(int id):
    User user = find_user(id)?            # returns None if None
    Address addr = user.address?          # returns None if None
    return Some(addr.email)

# Methods on Option
auto upper = name.map(it.to_upper())              # Option[String]
auto parsed = input.and_then((s): s.parse[int]()) # Option[int]
String n = name.unwrap_or("default")               # String (with fallback)
String n = name.unwrap()                            # String (panics if None!)
```

### 4.3 Generics

Square brackets `[]` for type parameters. `where...is` for trait bounds (readable English).

```gorget
# [] declares type variables, where constrains them
T max[T](T a, T b) where T is Comparable:
    if a > b: a else: b

# Unconstrained generic (no where clause)
T identity[T](T x):
    return x

# Type parameters in types
Vector[int] numbers = Vector.new()
HashMap[String, int] users = HashMap.new()
Vector[Option[int]] nested = Vector.new()

struct Pair[A, B]:
    A first
    B second
```

**No ambiguity with indexing**: `Vector[int]` = generic (Vector is a type), `arr[0]` = indexing (arr is a variable). Compiler knows which names are types.

**Monomorphization**: Each concrete type gets its own compiled version (zero-cost, like Rust/C++).

### 4.4 Traits (Interfaces)

```gorget
trait Displayable:
    String to_string(self)

trait Comparable:
    int compare(self, Self other)

# Trait with default implementation
trait Greetable:
    String name(self)

    String greeting(self):
        return "Hello, {self.name()}!"

# Trait inheritance with extends
trait Animal extends Displayable:
    str name(self)
    str sound(self)

# Associated types
trait Iterable:
    type Item
    Iterator[Self.Item] iter(self)

trait Iterator[T]:
    Option[T] next(&self)
```

### 4.4.1 Trait Naming Conventions

Gorget doesn't enforce trait naming rules, but following consistent conventions makes code read naturally with `is` (trait bounds) and `equip...with` (implementations).

| Category | Suffix | Examples | Reads with `is` / `equip...with` |
|----------|--------|----------|-----------------------------------|
| Capabilities | `-able` / `-ible` | `Hashable`, `Equatable`, `Displayable`, `Serializable`, `Cloneable` | `is Hashable` / `equip Point with Hashable` |
| Behaviors / roles | `-er` / `-or` | `Iterator`, `Handler`, `Formatter`, `Greeter` | `is Iterator` / `equip Vec with Iterator` |
| Operators / conversions | bare verb/noun | `Add`, `Sub`, `From`, `Into`, `Index`, `Copy`, `Default` | `is Add` / `equip Point with Add` |
| Domain abstractions | bare noun | `Shape`, `Animal`, `Collection` | `is Shape` / `equip Circle with Shape` |

**Guidelines:**

- Prefer `-able`/`-ible` suffixes for traits that describe **what a type can do** (capabilities). These read most naturally with `is`: "T is Displayable."
- Prefer `-er`/`-or` suffixes for traits that describe **what a type acts as** (roles/behaviors). These work well with both keywords: "T is Iterator" / "equip Vec with Iterator."
- Operator traits (`Add`, `Sub`, etc.) and conversion traits (`From`, `Into`) keep short, bare names — they're used so frequently that brevity wins.
- Domain-specific traits (`Shape`, `Animal`) use whatever noun is natural — don't force `-able` onto everything.
- When in doubt, ask: does the trait describe a **capability** (-able) or an **identity** (noun/-er)? Pick accordingly.

**Anti-patterns:**

```gorget
# Avoid
trait Sequence:           # ambiguous — is this a type or a trait?
    ...
# Prefer
trait Iterable:           # clearly a capability
    ...

# But domain nouns are fine when they ARE the concept
trait Shape:              # "Shape" is the right name — don't call it "Shapeable"
    float area(self)
```

### 4.5 Self parameter modes

```gorget
equip Point:
    float distance(self, Point other):        # immutable borrow (default)
        ...
    void translate(&self, float dx, float dy): # mutable borrow
        self.x += dx
        self.y += dy
    String into_string(!self):                 # consuming (takes ownership)
        return "({self.x}, {self.y})"
    static Point origin():                     # no self (static)
        return Point(0.0, 0.0)
```

Method calls **auto-borrow** (no &/! at call sites for methods).

### 4.6 Equipping Traits

```gorget
equip Point with Displayable:
    String to_string(self):
        return "({self.x}, {self.y})"

# Generic implementation with where...is
equip Vector[T] with Displayable where T is Displayable:
    String to_string(self):
        auto parts = [item.to_string() for item in self]
        return "[{parts.join(", ")}]"

# Blanket implementation
equip[T] T with Printable where T is Displayable:
    void print(self):
        println(self.to_string())
```

### 4.7 Trait Bounds with `where...is`

```gorget
# Single bound
void print_all[T](Vector[T] items) where T is Displayable:
    for item in items:
        print(item.to_string())

# Multiple bounds
void process[T](T item) where T is Displayable + Cloneable + Comparable:
    ...

# Multiple type variables
void complex[T, U](T a, U b) where T is Displayable + Cloneable, U is Into[T]:
    ...
```

### 4.8 Dynamic Dispatch

When the concrete type isn't known at compile time, use `dynamic Trait` for vtable-based dispatch:

```gorget
# As a parameter — accepts any type implementing Shape
void draw(dynamic Shape shape):
    shape.render()

# Stored in collections — must be boxed (known size on heap)
Vector[Box[dynamic Shape]] shapes = Vector.new()
shapes.push(Box.new(Circle(5.0)))
shapes.push(Box.new(Rectangle(3.0, 4.0)))

for shape in shapes:
    shape.render()

# As a return type — caller doesn't know the concrete type
Box[dynamic Shape] make_shape(str kind) throws ValueError:
    match kind:
        case "circle": Box.new(Circle(1.0))
        case "rect": Box.new(Rectangle(1.0, 1.0))
        else: throw ValueError("unknown shape: {kind}")
```

**When to use `dynamic` vs generics:**
- **Generics** (`[T] where T is Shape`): zero-cost, monomorphized at compile time. Use when types are known statically.
- **`dynamic`**: small runtime cost (vtable indirection). Use when types vary at runtime (heterogeneous collections, plugin systems, returned from factories).

### 4.9 Const Generics

```gorget
struct FixedArray[T, const int N]:
    T[N] data
```

### 4.10 V2 Feature: Structural Bounds (`has`)

*Deferred to V2* - would allow ad-hoc polymorphism without defining a trait:
```gorget
# V2: int get_length[T](T item) where T has .length: int:
```

---

## 5. Control Flow

### 5.1 Conditionals

```gorget
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")
```

### 5.2 Pattern Matching (match/case)

Exhaustive - compiler error if cases aren't covered. Uses `else` as catch-all (not `case _`).

```gorget
match color:
    case Red:
        print("red")
    case Custom(r, g, b):
        print("rgb({r}, {g}, {b})")
    else:
        print("other")
```

**Guards:**
```gorget
match value:
    case x if x > 100:
        print("large")
    case 0:
        print("zero")
    else:
        print("something else")
```

**Or-patterns:**
```gorget
match status_code:
    case 200 | 201 | 204:
        print("success")
    case 400 | 422:
        print("client error")
    else:
        print("other")
```

**Nested destructuring:**
```gorget
match response:
    case Ok(User(name, age)) if age >= 18:
        print("Adult: {name}")
    case Ok(User(name, _)):
        print("Minor: {name}")
    case Error(e):
        print("Error: {e}")
```

### 5.3 Match as Expression

Single-expression arms use `:` with the value on the same line. Multi-line arms use `:` followed by an indented block:

```gorget
String label = match color:
    case Red: "red"
    case Green: "green"
    case Custom(r, g, b): "rgb({r}, {g}, {b})"
    else: "other"
```

### 5.4 The `is` Keyword (Pattern Matching in Conditions)

Instead of Rust's `if let`, reads like English:

```gorget
if result is Ok(value):
    use(value)
elif result is Error(e):
    handle(e)

if color is Red:
    print("it's red!")

if result is not Error(_):
    print("not an error")

while iter.next() is Some(item):
    process(item)
```

### 5.5 If as Expression

```gorget
int abs_val = if x >= 0: x else: -x

String msg = if user.is_admin():
    "Welcome, admin"
else:
    "Welcome, user"
```

### 5.6 Loops

```gorget
# For loop (iterating - immutable borrow by default)
for item in collection:
    process(item)                # collection still valid

# Mutable borrow (modify items in-place)
for item in &collection:
    item.transform()

# Consuming (takes ownership of each item)
for item in !collection:
    store(!item)

# Range
for i in 0..10:                  # 0 through 9
for i in 0..=10:                 # 0 through 10 (inclusive)

# While loop
while condition:
    do_something()

# Loop (infinite, break to exit)
loop:
    if done():
        break
```

### 5.7 for/else and while/else (Python-style)

```gorget
for item in collection:
    if item.matches():
        break
else:
    print("no match found")     # runs if loop completes without break
```

### 5.8 Loop as Expression

```gorget
int result = loop:
    if compute() is Some(v):
        break v                  # break with a value
```

### 5.9 Comprehensions (Python-style)

```gorget
Vector[int] squares = [x * x for x in 0..10]
Vector[int] evens = [x for x in 0..100 if x % 2 == 0]
HashMap[String, int] lengths = {s: s.len() for s in words}
HashSet[int] unique = {x * x for x in 1..=10}
```

---

## 6. Error Handling

### 6.1 The `throws` + `try` Model

Functions that can fail use `throws`. Errors auto-propagate without `?`:

```gorget
# Clean: no ?, no Result wrapping
Data process(str path) throws AppError:
    String content = read_file(path)          # auto-propagates if error
    Config config = parse_config(content)     # auto-propagates if error
    return transform(config)

# To handle an error locally, use try:
Data safe_process(str path) throws AppError:
    auto result = try read_file(path)       # captures Result instead of propagating
    match result:
        case Ok(content): return parse(content)
        case Error(e):
            log("Fallback: {e}")
            return default_data()

# To explicitly raise an error, use throw:
Record parse_line(str line) throws ParseError:
    if line.is_empty():
        throw ParseError("empty line")      # raises error, exits function
    return parse(line)

# Non-throwing functions must try:
void main():
    match try process("data.txt"):
        case Ok(data): print(data)
        case Error(e): print("Error: {e}")
```

**Keywords summary:**
- `throws` — annotates a function that can fail (on the signature)
- `throw` — explicitly raises an error (inside a `throws` function)
- `try` — captures the result as `Result[T, E]` instead of auto-propagating

Under the hood, `throws` desugars to `Result`. Both styles available:
- **throws style**: clean, auto-propagation (most code)
- **Result style**: when you need to manipulate errors as data (map, and_then)

### 6.2 Custom Error Types

```gorget
enum AppError:
    Io(IoError)
    Parse(ParseError)
    NotFound(String)

equip AppError with Displayable:
    String to_string(self):
        match self:
            case Io(e): "IO error: {e}"
            case Parse(e): "Parse error: {e}"
            case NotFound(path): "Not found: {path}"

equip AppError with From[IoError]:
    AppError from(IoError !e):
        return AppError.Io(!e)
```

### 6.3 Error Backtraces

Three layers, from cheap to detailed:

| Layer | Available | Cost | Info |
|-------|-----------|------|------|
| Source location | Always | ~zero | File + line where error was created |
| Propagation trace | Debug builds | Moderate | Full chain of throws propagation |
| `.context()` | Always | String alloc | Human-readable context messages |
| `GORGET_BACKTRACE=1` | On demand | Heavy | Full native stack trace |

```gorget
# Adding context:
String content = try read_file(path)
    .context("loading config from {path}")

# Accessing trace:
match try process("data.txt"):
    case Error(e):
        print(e)           # error message
        print(e.trace())   # propagation trace (debug builds)
        print(e.source())  # file:line (always)
```

### 6.4 Panic (Unrecoverable)

```gorget
void critical_section():
    if not valid():
        panic("invariant violated")
```

---

## 7. Closures & Lambdas

### 7.1 Syntax: `(params):` + Implicit `it`

Closures use parenthesized parameters with a colon, mirroring function definitions. For single-parameter closures, the implicit `it` keyword (Kotlin-inspired) eliminates boilerplate.

```gorget
# Implicit 'it' for single-parameter closures
auto doubled = numbers.map(it * 2)
auto names = users.filter(it.age >= 18).map(it.name)
auto lengths = words.map(it.len())

# Explicit single parameter
auto doubled = numbers.map((x): x * 2)

# Multiple parameters
auto sum = pairs.map((a, b): a + b)
auto zipped = list1.zip(list2).map((x, y): x + y)

# Type-annotated parameters
auto parsed = strings.map((String s): s.parse[int]())

# Multi-line closure (indented block)
auto process = (int x):
    int result = x * 2
    result += 1
    result                          # last expression is return value

# Multi-line with implicit 'it'
auto transformed = items.map():
    auto temp = it.transform()
    temp.validate()
    temp

# No-parameter closure
auto greet = (): "hello!"
auto lazy_init = ():
    auto config = load_config()
    config.validate()
    config
```

### 7.2 Function Types (C-Style)

Function types mirror declaration syntax — return type followed by parameter types in parentheses, with no name:

```gorget
# Function declaration:  int add(int a, int b)
# Function type:         int(int, int)  — same shape, no names

# Function type as variable
int(int, int) adder = add
void(String) callback = print
String(int) formatter = (n): "Value: {n}"

# As parameter types
void apply(Vector[int] data, int(int) transform):
    for item in &data:
        item = transform(item)

# As return type (higher-order functions)
int(int) make_multiplier(int factor):
    return !(x): x * factor

# In structs
struct Button:
    String label
    void(Event) on_click

# Generic function types
Vector[int(int)] transforms = [
    (x): x * 2,
    (x): x + 1,
    (x): x * x,
]

# No params, no return
void() do_nothing = (): pass
```

### 7.3 Capture Semantics

The compiler infers the minimal capture mode automatically:
- Immutable capture for reads
- Mutable capture for mutations
- Move capture for ownership transfer

Use `!` before the parameter list to force-move ALL captures:

```gorget
# Default: auto-infer (immutable borrow captures)
String name = "Alice"
auto greet = (): print("Hello {name}")
print(name)     # OK — name was only borrowed

# Move ALL captures with !()
auto handle = thread.spawn(!():
    print("Hello from thread: {name}")
)
# name is moved into the closure, invalid here

# Returning closures (must own captures to outlive the function)
int(int) make_adder(int n):
    return !(x): x + n     # n moved into closure

# Multi-line closure with ! (moves all captures)
auto processor = !(data):
    auto result = data.transform()
    result.validate()
    result
```

### 7.4 V2 Feature: Per-Variable Capture Control

*Deferred to V2.* Will allow fine-grained capture modes per variable using `(captures)(params)` syntax:

```gorget
# V2: Per-variable capture — captures first, params second
# (!name, count)(x): x + count + name.len()
# !name = move, count = borrow, x = parameter
```

### 7.5 `it` Rules

- `it` is only valid inside closures with exactly one parameter
- If `it` appears, no explicit parameter list is needed
- `it` is always an immutable borrow (use explicit params for `&` or `!`)
- Nested closures: `it` refers to the innermost closure's parameter

```gorget
# 'it' is the single parameter
auto result = numbers.filter(it > 0).map(it * 2)

# When you need mutable access, use explicit params
auto modified = items.map((Item &item): item.transform())

# Nested: each 'it' refers to its own closure
auto nested = matrix.map(it.map(it * 2))
# outer 'it' = each row, inner 'it' = each element
```

---

## 8. Modules & Visibility

### 8.1 Module System

File = module (like Rust). Directory with `mod.gg` = package.

```
project/
  src/
    main.gg
    math/
      mod.gg              # package root — controls what's exported
      geometry.gg
      algebra.gg
    utils.gg
```

### 8.2 Package Root (`mod.gg`)

A `mod.gg` file defines the public API of a directory-based package. It explicitly re-exports items from its child modules:

```gorget
# math/mod.gg — the public API of the math package
from math.geometry import Point, Circle       # re-export selectively
from math.algebra import Matrix               # re-export selectively
# internal.gg items are NOT re-exported — they stay private
```

Consumers then import from the package:
```gorget
from math import Point, Circle, Matrix        # clean, flat imports
```

### 8.3 Imports

```gorget
import std.io                                 # import entire module
import std.collections.HashMap                # import specific type
from std.fmt import Displayable, format       # from...import
from math.geometry import Point, Circle       # project-local module
import std.sync.{Arc, Mutex, RwLock}          # multiple items with {}
```

### 8.4 Visibility

Two levels: `public` (visible everywhere) and private (default, visible only within the module).

```gorget
public struct Point:
    public float x                # public field
    public float y
    float internal_id             # private field (default)

public int add(int a, int b):    # public function
    return a + b

int helper(int x):               # private (default, no keyword)
    return x + 1
```

Items in a package are visible to sibling modules within the same package, but not outside it unless re-exported through `mod.gg`.

---

## 9. Memory & Smart Pointers

### 9.1 Allocation

```gorget
# Stack allocation (default — fastest, no heap overhead)
Point p = Point(1.0, 2.0)

# Heap allocation (Box — single owner, fixed size on stack)
Box[Point] heap_point = Box.new(Point(1.0, 2.0))
```

### 9.2 Shared Ownership

```gorget
# Reference-counted (single-threaded)
Rc[String] shared = Rc.new("shared data")
Rc[String] clone = shared.clone()     # increments ref count

# Weak reference (doesn't prevent deallocation)
Weak[String] weak = shared.downgrade()
if weak.upgrade() is Some(strong):
    print(strong)                      # still alive

# Atomic reference-counted (thread-safe)
Arc[Mutex[int]] counter = Arc.new(Mutex.new(0))
```

### 9.3 Interior Mutability

When you need to mutate data behind an immutable reference:

```gorget
# Cell — for Copy types (single-threaded)
Cell[int] counter = Cell.new(0)
counter.set(counter.get() + 1)

# RefCell — for non-Copy types (single-threaded, runtime borrow checking)
RefCell[Vector[int]] data = RefCell.new(Vector.new())
data.borrow_mut().push(42)
print(data.borrow().len())            # 1

# Mutex — for thread-safe interior mutability
Mutex[Vector[int]] shared_data = Mutex.new(Vector.new())
auto guard = shared_data.lock().unwrap()
guard.push(42)
```

### 9.4 Deref Coercion

Smart pointers automatically dereference to their inner type:
```gorget
Box[String] boxed = Box.new(String.from("hello"))
print(boxed.len())       # Box[String] auto-derefs to String, calls String.len()
```

---

## 10. Concurrency

```gorget
import std.thread
import std.sync.{Arc, Mutex}

void main():
    Arc[Mutex[int]] counter = Arc.new(Mutex.new(0))
    Vector[JoinHandle[void]] handles = Vector.new()

    for _ in 0..10:
        auto c = counter.clone()
        auto handle = thread.spawn(!():
            auto guard = c.lock().unwrap()
            *guard += 1
        )
        handles.push(handle)

    for h in handles:
        h.join().unwrap()

    print("Count: {*counter.lock().unwrap()}")
```

### Async/Await

Python-style `await` as a prefix keyword (not postfix like Rust). Composes naturally with `throws`.

```gorget
# async + throws compose naturally
async String fetch(str url) throws HttpError:
    Response resp = await http.get(url)        # await is a prefix keyword
    return await resp.text()

# async without throws (infallible async)
async int compute_slowly():
    await sleep(Duration.seconds(1))
    return 42

# Calling async functions
async void main():
    String data = await fetch("https://example.com")
    print(data)

# Concurrent execution
async void fetch_all():
    # Launch multiple tasks concurrently
    auto task1 = spawn fetch("https://api.example.com/a")
    auto task2 = spawn fetch("https://api.example.com/b")

    # Await results
    String a = await task1
    String b = await task2
    print("{a}, {b}")

# async closures
auto fetcher = async (str url):
    return await http.get(url)

# async with error handling (throws + try)
async void resilient_fetch(str url):
    match try await fetch(url):
        case Ok(data): print(data)
        case Error(e): print("Failed: {e}")
```

---

## 11. Unsafe Code

Gorget is safe by default. The `unsafe` keyword opts into operations the compiler can't verify:

### 11.1 Unsafe Blocks

```gorget
unsafe:
    int &ptr = raw_pointer as int&
    *ptr = 42
```

### 11.2 What Requires `unsafe`

- **Raw pointer operations**: dereferencing, arithmetic, casting
- **FFI calls**: calling external C functions
- **Mutating static variables**: global mutable state (thread safety risk)
- **Implementing unsafe traits**: e.g., `Sendable`, `Syncable` for manual implementations

### 11.3 Raw Pointers

```gorget
# Raw pointer types
RawPtr[int] ptr = ...           # mutable raw pointer
ConstRawPtr[int] cptr = ...     # immutable raw pointer

unsafe:
    int value = *ptr             # dereference
    ptr = ptr.offset(1)          # pointer arithmetic
```

### 11.4 FFI (Foreign Function Interface)

```gorget
# Declare external C functions
extern "C":
    int printf(str format, ...)
    RawPtr[void] malloc(uint size)
    void free(RawPtr[void] ptr)

# Calling C functions
void main():
    unsafe:
        printf("Hello from C! %d\n", 42)

# Wrapping unsafe in a safe API
int abs_value(int x):
    unsafe:
        return c_abs(x)
```

### 11.5 Unsafe Functions

```gorget
# Entire function is unsafe — caller must use unsafe block
unsafe void dangerous_operation(RawPtr[int] ptr):
    *ptr = 0
```

---

## 12. String Interpolation

Built-in, no prefix needed (any type implementing Displayable auto-formats):

```gorget
String name = "world"
int count = 42
print("Hello, {name}! Count is {count}")
print("Math: {2 + 2}")                     # expressions in braces
print("Escaped brace: {{literal}}")         # double-brace to escape
```

---

## 13. Comments & Documentation

```gorget
# Single-line comment

#/ Documentation comment for the item below.
#/ Supports **markdown** formatting.
#/
#/ ## Examples
#/ ```
#/ int result = add(2, 3)
#/ assert(result == 5)
#/ ```
public int add(int a, int b):
    return a + b
```

---

## 14. Complete Example Program

```gorget
#/ A simple linked list implementation demonstrating
#/ ownership, generics, traits, and pattern matching.

from std.fmt import Displayable

public enum List[T]:
    Cons(T, Box[List[T]])
    Nil

equip[T] List[T] where T is Displayable:
    #/ Creates an empty list.
    public static List[T] new():
        return List.Nil

    #/ Prepends a value to the front of the list.
    public List[T] prepend(self, T value):
        return List.Cons(value, Box.new(self))

    #/ Returns the length of the list.
    public int len(self):
        match self:
            case Cons(_, tail): 1 + tail.len()
            case Nil: 0

equip[T] List[T] with Displayable where T is Displayable:
    String to_string(self):
        match self:
            case Cons(head, tail):
                match *tail:
                    case Nil: return "{head}"
                    case _: return "{head} -> {tail.to_string()}"
            case Nil:
                return "[]"

void main():
    auto list = List[int].new()
    list = list.prepend(3)
    list = list.prepend(2)
    list = list.prepend(1)

    print("List: {list.to_string()}")       # "1 -> 2 -> 3"
    print("Length: {list.len()}")            # 3
```

---

## 15. Design Decisions (Resolved)

| # | Question | Decision |
|---|----------|----------|
| 1 | **Lifetime syntax** | `live` keyword for explicit annotation; compiler auto-infers from function bodies (99% of code) |
| 2 | **Implicit borrow at call sites?** | Yes — bare type = immutable borrow, no annotation needed |
| 3 | **Expression-oriented blocks?** | Both — `return` for explicit early returns, last expression as implicit return value |
| 4 | **Inheritance?** | None — composition via traits only |
| 5 | **Macro system?** | None for V1; add hygienic macros in V2+ |
| 6 | **File extension** | `.gg` |
| 7 | **Indentation** | 4 spaces (enforced by `gg fmt`) |
| 8 | **Compilation target** | LLVM (Cranelift for debug builds in future) |
| 9 | **Package manager name** | `forge` |
| 10 | **Option handling** | `Option[T]` with rich sugar: `is` pattern matching, `?.` optional chaining, `??` nil coalescing, `.unwrap()`, `.unwrap_or()`, `?` early return |
| 11 | **Tuple syntax** | `(int, str)` — concise, universal |
| 12 | **Array syntax** | C-style: `int[5]` fixed array, `Vector[int]` growable, `int[]` slice |
| 13 | **Operator overloading** | Via traits (like Rust) |
| 14 | **Type aliases** | `type Name = String` |
| 15 | **Mutability** | Mutable by default, `const` for immutable. No `mut` keyword. |
| 16 | **Associated type access** | `Self.Item` (uppercase Self, dot access) |
| 17 | **Expression arms** | `:` for both single-line and block arms (no `=>`). Disambiguated by same-line vs newline+indent. |

---

## 16. How Gorget Compares

| Feature | Gorget | Rust | Python | C/Java |
|---------|-------|------|--------|--------|
| Memory safety | Ownership | Ownership | GC | Manual/GC |
| Block syntax | Indentation | `{}` | Indentation | `{}` |
| Type position | Before name | After name | Optional/after | Before name |
| Semicolons | No | Yes | No | Yes |
| Null | `Option[T]` | `Option<T>` | `None` | `null` |
| Borrowing | bare / `&` / `!` | `&` / `&mut` / move | N/A | Implicit |
| Generics | `[T]` | `<T>` | `[T]` | `<T>` |
| Mutability | Mutable default + `const` | `let` default + `mut` | Default mutable | `final`/`const` |
| Error handling | `throws` + `try` | `Result` + `?` | Exceptions | Exceptions |
| Closures | `(params):` + `it` | `\|params\|` | `lambda` | `->` (Java) |
| Inheritance | Traits only | Traits only | Classes | Classes |

---

## 17. Destructuring & Advanced Pattern Matching

*Expands on the basics introduced in section 5 (Control Flow) with additional patterns.*

### 17.1 Variable Destructuring

```gorget
# Tuple destructuring
auto (x, y) = get_coordinates()
(int, String) pair = (42, "hello")
auto (id, name) = pair

# Struct destructuring
Point(px, py) = some_point
auto Person(name, age) = get_person()

# Partial destructuring (ignore fields with ..)
auto Person(name, ..) = get_person()

# Nested destructuring
auto (Point(x1, y1), Point(x2, y2)) = get_line_segment()
```

### 17.2 Pattern Matching with Guards

```gorget
match value:
    case x if x > 100:
        print("large")
    case x if x > 0:
        print("positive")
    case 0:
        print("zero")
    case x:
        print("negative: {x}")
```

### 17.3 The `is` Keyword - Pattern Matching in Conditions

Instead of Rust's `if let`, Gorget uses the more Pythonic `is`:

```gorget
# Single pattern
if result is Ok(value):
    use(value)
elif result is Error(e):
    handle(e)

# With guard
if result is Ok(value) and value > 0:
    process(value)

# In while loops
while iter.next() is Some(item):
    process(item)

# Negation
if result is not Ok(_):
    panic("expected success")
```

**Why `is` instead of `if let`**: Python developers already think of `is` as a check. Gorget repurposes it for structural pattern matching. It reads naturally: "if result *is* an Ok containing a value."

### 17.4 Nested Match

```gorget
match (command, arg):
    case ("get", key):
        return store.get(key)
    case ("set", _):
        print("set requires two args")
    case ("delete", key):
        store.delete(key)
    case (cmd, _):
        print("unknown command: {cmd}")
```

### 17.5 Or-patterns

```gorget
match status_code:
    case 200 | 201 | 204:
        print("success")
    case 400 | 422:
        print("client error")
    case 500 | 502 | 503:
        print("server error")
    case code:
        print("unexpected: {code}")
```

---

## 18. Comprehensions (Python's Killer Feature, Adopted)

*Expands on section 5.9 with ownership semantics and additional collection types.*

### 18.1 List Comprehensions

```gorget
Vector[int] squares = [x * x for x in 0..10]
Vector[int] evens = [x for x in 0..100 if x % 2 == 0]
Vector[String] names = [p.name for p in people if p.age >= 18]
```

### 18.2 Dict Comprehensions

```gorget
HashMap[String, int] lengths = {s: s.len() for s in words}
HashMap[int, Vector[String]] grouped = {k: v for k, v in groups.entries()}
```

### 18.3 Set Comprehensions

```gorget
HashSet[int] unique_squares = {x * x for x in range}
```

### 18.4 Ownership in Comprehensions

Comprehensions produce **owned** collections. The iterator yields owned or cloned values:

```gorget
# Default: immutable borrow (people still valid after)
Vector[str] names = [p.name for p in people]

# Consuming: takes ownership of each person (people is gone after)
Vector[String] names = [!p.name for p in !people]

# Clone to get owned copies while keeping the original
Vector[String] names = [p.name.clone() for p in people]
```

This is where Rust's ownership model intersects Python's ergonomics. The comprehension syntax stays clean, but the programmer must be aware of moves vs. borrows.

---

## 19. Named Arguments & Default Parameters

```gorget
void create_user(String name, int age, bool admin = false, String role = "user"):
    ...

# Positional call
create_user("Alice", 30)

# Named arguments (any order after positional)
create_user("Bob", 25, admin = true)
create_user("Charlie", 28, role = "editor", admin = false)

# All named
create_user(name = "Dave", age = 35, admin = true)
```

**Rule**: Once you use a named argument, all subsequent arguments must also be named. This prevents ambiguity.

---

## 20. Attributes & Derive

Python-style `@` decorator syntax for compiler attributes:

```gorget
@derive(Debuggable, Cloneable, Equatable)
struct Point:
    float x
    float y

@derive(Debuggable, Serialize, Deserialize)
enum Message:
    Text(String)
    Image(Vector[uint8])
    Ping

@test
void test_addition():
    assert_eq(add(2, 3), 5)

@test
@should_panic("division by zero")
void test_div_zero():
    divide(1, 0)

@inline
int fast_add(int a, int b):
    return a + b

@cfg(target_os = "linux")
void linux_only():
    ...

@deprecated("use new_api() instead")
void old_api():
    ...
```

---

## 21. Associated Types & Const Generics

### 21.1 Associated Types

```gorget
# Iterable: equip your type with this to make it work with for-loops
trait Iterable:
    type Item
    Iterator[Self.Item] iter(self)

# Iterator: the stateful cursor that walks through elements
trait Iterator[T]:
    Option[T] next(&self)

    # Default methods
    Vector[T] collect(&self):
        Vector[T] result = Vector.new()
        while self.next() is Some(item):
            result.push(item)
        return result

# Making a custom type iterable
equip Counter with Iterable:
    type Item = int
    Iterator[int] iter(self):
        return CounterIterator(self.start, self.max)

struct CounterIterator:
    int current
    int max

equip CounterIterator with Iterator[int]:
    Option[int] next(&self):
        if self.current < self.max:
            self.current += 1
            return Some(self.current)
        return None
```

### 21.2 Const Generics

```gorget
struct FixedArray[T, const int N]:
    T[N] data

equip[T, const int N] FixedArray[T, N]:
    static FixedArray[T, N] zeroed() where T is Default:
        return FixedArray([T.default(); N])

    T get(self, int index):
        assert(index < N, "index out of bounds")
        return self.data[index]

# Usage
FixedArray[float, 3] vec3 = FixedArray([1.0, 2.0, 3.0])
FixedArray[int, 256] buffer = FixedArray[int, 256].zeroed()
```

---

## 22. Operator Overloading via Traits

```gorget
# Standard library defines:
trait Add[Rhs = self]:
    type Output
    Self.Output add(self, Rhs rhs)

trait Sub[Rhs = self]:
    type Output
    Self.Output sub(self, Rhs rhs)

trait Mul[Rhs = self]:
    type Output
    Self.Output mul(self, Rhs rhs)

trait Index[Idx]:
    type Output
    Self.Output index(self, Idx idx)

# User equips:
equip Point with Add:
    type Output = Point
    Point add(self, Point rhs):
        return Point(self.x + rhs.x, self.y + rhs.y)

equip Matrix with Index[int]:
    type Output = Vector[float]
    Vector[float] index(self, int row):
        return self.data[row]

# Now operators work:
Point c = a + b              # calls a.add(b)
Vector[float] row = matrix[0]   # calls matrix.index(0)
```

---

## 23. String Types in Depth

```gorget
# str  - immutable string slice (borrowed, like Rust's &str)
# String - owned, heap-allocated, growable (like Rust's String)

str literal = "hello"                # string literal, type is str
String owned = String.from("hello")  # explicit owned string

# Automatic coercion: String -> str (deref coercion)
void takes_str(str s):
    print(s)

String owned = String.from("world")
takes_str(owned)           # String auto-coerces to str

# Raw strings (no escape processing)
str regex = r"^\d+\.\d+$"
str path = r"C:\Users\name\docs"

# Multi-line strings
str query = """
    SELECT *
    FROM users
    WHERE active = true
"""

# Byte strings
[uint8] bytes = b"hello"

# Character literals
char letter = 'A'
char emoji = '\u{1F40D}'     # snake emoji

# String interpolation (built-in, all strings)
String greeting = "Hello, {name}! You are {age} years old."
String math = "2 + 2 = {2 + 2}"
String formatted = "Pi is approximately {pi:.4}"     # format specifiers
String padded = "Value: {x:>10}"                     # right-align, width 10
```

---

## 24. Arrays, Slices, and Vectors

C-style array syntax: `Type[Size]` for fixed arrays, `Vector[T]` for growable.

```gorget
# Fixed-size array (C-style: type[size])
int[5] arr = [1, 2, 3, 4, 5]

# Type inferred
auto arr = [1, 2, 3, 4, 5]           # inferred as int[5]

# Slice: a borrowed view into contiguous memory
int[] slice = arr[1..4]       # [2, 3, 4]

# Vector: owned, heap-allocated, growable
Vector[int] vec = Vector.new()
vec.push(1)
vec.push(2)

# Vector from literal
Vector[int] nums = [1, 2, 3, 4, 5]   # literal syntax, type annotation clarifies

# Slicing operations
int[] first_three = arr[..3]
int[] last_two = arr[3..]
int[] middle = arr[1..4]

# Array methods
int length = arr.len()
bool has_3 = arr.contains(3)
```

**Disambiguation**: `int[5]` = fixed array type (type position), `arr[5]` = indexing (value position). Compiler knows which is which from context, same as generics (`Vector[int]` vs `vec[0]`).

---

## 25. The `with` Statement (Scoped Resource Management)

Python-style `with` for explicit resource scoping:

```gorget
# In a throws function, errors auto-propagate:
void read_data(str path) throws IoError:
    with File.open(path) as file:
        String content = file.read_all()
        print(content)
    # file is closed here (Drop called)

    with mutex.lock() as guard:
        *guard += 1
    # lock released here

    # Multiple resources
    with File.open("in.txt") as input, File.create("out.txt") as output:
        String data = input.read_all()
        output.write(data)
```

This is syntactic sugar - it just creates a scope. Ownership + Drop handles the cleanup. But it makes the intent **explicit** and is familiar to Python developers.

---

## 26. Method Chaining & Fluent APIs

```gorget
# Leading-dot continuation for multi-line chains
auto result = items
    .iter()
    .filter((x): x.is_valid())
    .map((x): x.transform())
    .collect[Vector[Item]]()

# Builder pattern
auto config = ConfigBuilder.new()
    .host("localhost")
    .port(8080)
    .max_connections(100)
    .build()
```

**Indentation rule**: A leading `.` on a new line is a continuation of the previous expression, not a new statement. This is unambiguous with indentation-based parsing.

---

## 27. Expression Blocks with `do`

For when you need a block that evaluates to a value:

```gorget
int result = do:
    int a = compute_a()
    int b = compute_b()
    a + b                   # last expression is the value

# Useful in complex initializations
Config config = do:
    auto builder = ConfigBuilder.new()
    if env == "production":
        builder.set_strict(true)
    builder.build()
```

`do:` introduces an expression block. The last expression is the block's value. This is explicit - no ambiguity about when a block is an expression vs. a statement.

---

## 28. Compile-Time Evaluation

```gorget
const int MAX_SIZE = 1024
const float PI = 3.14159265358979
const float TAU = 2.0 * PI

# Const functions (evaluated at compile time when called with const args)
const int factorial(int n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

const int FACT_10 = factorial(10)      # computed at compile time

# Static variables (one instance, global lifetime)
static int COUNTER = 0                 # mutable static; requires unsafe to mutate (thread safety)
static Regex EMAIL_RE = Regex.compile(r"^[\w.]+@[\w.]+$")
```

---

## 29. Testing (Built-in, First-Class)

```gorget
# Unit tests live alongside the code (like Rust)
@test
void test_add():
    assert_eq(add(2, 3), 5)
    assert_ne(add(2, 3), 6)
    assert(add(0, 0) == 0)

@test
@should_panic("division by zero")
void test_div_zero():
    divide(1, 0)

# Test with setup
@test
void test_user_creation():
    auto db = TestDb.new()
    auto user = db.create_user("Alice", 30)
    assert_eq(user.name, "Alice")
    assert(user.id > 0)

# Documentation tests (code in doc comments is compiled & run)
#/ Returns the larger of two values.
#/
#/ ```
#/ assert_eq(max(3, 5), 5)
#/ assert_eq(max(10, 2), 10)
#/ ```
T max[T](T a, T b) where T is Comparable:
    if a > b: a else: b

# Benchmarks
@bench
void bench_sort(Bencher &b):
    b.iter(():
        auto data = random_vec(10000)
        data.sort()
    )
```

```bash
# Run tests
gg test
forge test

# Run specific test
gg test test_add

# Run benchmarks
gg bench
```

---

## 30. Tricky Indentation Cases (Parser Design)

### 30.1 Long Function Signatures

```gorget
# Continuation with hanging indent
Vector[ProcessedItem] process_all[T](Vector[T] items, Config config,
        Logger &logger) throws ProcessError
        where T is Processable:
    for item in items:
        ...
```

**Rule**: Arguments inside `()` can span multiple lines freely (implicit continuation, like Python).

### 30.2 Long Conditions

```gorget
if (user.is_authenticated()
        and user.has_permission("admin")
        and not user.is_banned()):
    grant_access()
```

### 30.3 Nested Closures

```gorget
auto processor = (Vector[int] data):
    auto transform = (int x):
        return x * 2 + 1
    return data.iter().map(transform).collect()
```

### 30.4 Multiline Collection Literals

```gorget
Vector[Point] points = [
    Point(0.0, 0.0),
    Point(1.0, 0.0),
    Point(1.0, 1.0),
    Point(0.0, 1.0),
]                        # trailing comma allowed
```

### 30.5 Where Clauses

```gorget
void complex_fn[T, U, V](T a, U b, V c)
        where T is Displayable + Cloneable,
              U is Into[T] + Debuggable,
              V is Iterator[Item = T]:
    ...
```

---

## 31. Type Aliases & Newtype Pattern

```gorget
# Simple alias
type Callback = int(int, int)
type StringMap[V] = HashMap[String, V]
type IoResult[T] = Result[T, IoError]

# Newtype (distinct type wrapping another, zero-cost)
newtype Meters(float)
newtype Seconds(float)
newtype UserId(int)

# Newtypes prevent accidental mixing
Meters distance = Meters(100.0)
Seconds time = Seconds(9.58)
# float speed = distance + time    # COMPILE ERROR: can't add Meters + Seconds

# Implement conversions explicitly
equip Meters:
    float value(self):
        return self.0             # .0 accesses the first (only) field by position

    Kilometers to_km(self):
        return Kilometers(self.0 / 1000.0)
```

Tuple fields are accessed by numeric index: `.0`, `.1`, `.2`, etc. This applies to both tuples and tuple-style newtypes.

---

## 32. Ranges as First-Class Types

```gorget
Range[int] r1 = 0..10          # exclusive: 0,1,2,...,9
RangeInclusive[int] r2 = 0..=10  # inclusive: 0,1,2,...,10
RangeFrom[int] r3 = 5..        # unbounded end
RangeTo[int] r4 = ..10         # unbounded start

# Used everywhere
for i in 0..n:
    ...
auto slice = array[2..5]
bool in_range = value in 1..=100
```

---

## 33. Conditional Compilation & Platform Abstractions

```gorget
@cfg(target_os = "linux")
void platform_init():
    # Linux-specific setup

@cfg(target_os = "windows")
void platform_init():
    # Windows-specific setup

@cfg(debug)
void debug_log(str msg):
    print("[DEBUG] {msg}")

@cfg(not(debug))
void debug_log(str msg):
    pass    # no-op in release
```

---

## 34. Build System (`forge`) in Detail

### forge.toml (Best of Cargo + npm + pyproject.toml)
```toml
[package]
name = "my_project"
version = "0.1.0"
authors = ["Nuno Antunes <nuno@example.com>"]
edition = "2026"
license = "MIT"
gorget = ">=1.0"              # minimum compiler version

# Custom tasks (inspired by npm scripts)
[scripts]
dev = "forge run --watch"
deploy = "forge build --release && ./deploy.sh"
lint = "forge check && forge fmt --check"

# Dependencies with explicit semver prefixes
[dependencies]
http = "^1.2"                # ^1.2 = >=1.2.0, <2.0.0
json = "~0.5"                # ~0.5 = >=0.5.0, <0.6.0
logger = "=2.0.1"            # exact version
crypto = { version = "^2.0", features = ["aes", "rsa"] }
local-lib = { path = "../my-lib" }
git-dep = { git = "https://github.com/user/repo", tag = "v1.0" }

[dev-dependencies]
mock = "^0.3"

# Compile-time feature flags
[features]
default = ["json"]
full = ["json", "crypto", "http"]

# Build profiles
[profiles.dev]
opt-level = 0
debug = true

[profiles.release]
opt-level = 3
lto = true
strip = true

# Monorepo support
[workspace]
members = ["core", "cli", "web"]

# Tool configuration in one file (from pyproject.toml)
[tool.fmt]
indent = 4
line-length = 100

[tool.lint]
deny = ["unused-variables", "dead-code"]
warn = ["missing-docs"]
```

### Project Layout
```
my_project/
  forge.toml               # manifest
  forge.lock               # lockfile (auto-generated, committed to git)
  src/
    main.gg                # binary entry point
    lib.gg                 # library root
    utils.gg
    models/
      mod.gg               # package root (public API)
      user.gg
      post.gg
  tests/
    test_models.gg
  benches/
    bench_sort.gg
  examples/
    hello.gg
```

### CLI Commands
```bash
forge new my_project       # create project from template
forge build                # compile
forge run                  # compile and run
forge run dev              # run a custom script
forge test                 # run tests
forge bench                # run benchmarks
forge check                # type-check only (fast)
forge fmt                  # format code
forge lint                 # run linter
forge doc                  # generate documentation
forge push                 # publish to foundry registry
forge add http             # add dependency to forge.toml
forge update               # update lockfile
forge audit                # scan for vulnerabilities
```

---

## 35. Error Messages Philosophy

Errors should be **helpful, specific, and suggest fixes**. Like Rust but even friendlier:

```
error[E0382]: use of moved value `name`
 --> src/main.gg:5:12
  |
3 |     String name = "hello"
  |            ---- `name` has type `String` (non-Copy)
4 |     String other = !name
  |                    ----- value moved here
5 |     print(name)
  |           ^^^^ value used here after move
  |
help: consider cloning the value if you need both variables
  |
4 |     String other = name.clone()
  |                        ++++++++

error[E0502]: cannot borrow `list` as mutable because it is also borrowed as immutable
 --> src/main.gg:8:5
  |
6 |     int first = list[0]
  |                 ------- immutable borrow occurs here
7 |     ...
8 |     list.push(42)
  |     ^^^^^^^^^^^^ mutable borrow occurs here
9 |     print(first)
  |           ----- immutable borrow later used here
  |
help: consider using the value before mutating the collection
```

---

## 36. More Complete Examples

### 36.1 HTTP Server

```gorget
import std.net.{TcpListener, TcpStream}
import http.{Request, Response, Router}

Response handle_index(Request req) throws HttpError:
    return Response.ok("Welcome to Gorget!")

Response handle_user(Request req, int id) throws HttpError:
    auto user = db.find_user(id)
    if user is Some(u):
        return Response.json(u)
    return Response.not_found("User not found")

void main():
    auto router = Router.new()
    router.get("/", handle_index)
    router.get("/users/{id}", handle_user)

    auto listener = TcpListener.bind("0.0.0.0:8080").unwrap()
    print("Server running on :8080")

    for stream in listener.incoming():
        if stream is Ok(s):
            thread.spawn(!():
                router.handle(s)
            )
```

### 36.2 Generic Binary Tree with Ownership

```gorget
public enum Tree[T]:
    Node(T, Box[Tree[T]], Box[Tree[T]])
    Leaf

equip[T] Tree[T] where T is Comparable + Displayable:
    public static Tree[T] new():
        return Tree.Leaf

    public Tree[T] insert(self, T value):
        match self:
            case Leaf:
                return Tree.Node(
                    value,
                    Box.new(Tree.Leaf),
                    Box.new(Tree.Leaf),
                )
            case Node(v, left, right):
                if value < v:
                    return Tree.Node(v, Box.new(left.insert(value)), right)
                elif value > v:
                    return Tree.Node(v, left, Box.new(right.insert(value)))
                else:
                    return Tree.Node(v, left, right)  # duplicate, no-op

    public void in_order(self):
        match self:
            case Node(v, left, right):
                left.in_order()
                print("{v} ")
                right.in_order()
            case Leaf:
                pass

    public bool contains(self, T target):
        match self:
            case Node(v, left, right):
                if target == v:
                    return true
                elif target < v:
                    return left.contains(target)
                else:
                    return right.contains(target)
            case Leaf:
                return false

void main():
    auto tree = Tree[int].new()
    for val in [5, 3, 7, 1, 4, 6, 8]:
        tree = tree.insert(val)

    tree.in_order()         # prints: 1 2 3 4 5 6 7 8
    print(tree.contains(4)) # true
    print(tree.contains(9)) # false
```

### 36.3 File Processor with Error Handling

```gorget
import std.fs
import std.path.Path
from std.io import BufReader, BufRead

enum ProcessError:
    Io(IoError)
    Parse(String)
    InvalidFormat(String)

equip ProcessError with From[IoError]:
    ProcessError from(IoError e):
        return ProcessError.Io(e)

@derive(Debuggable)
struct Record:
    String name
    int value

Record parse_line(str line) throws ProcessError:
    auto parts = line.split(',').collect[Vector[str]]()
    if parts.len() != 2:
        throw ProcessError.InvalidFormat("expected 2 fields, got {parts.len()}")
    String name = parts[0].trim().to_string()
    int value = try parts[1].trim().parse[int]()
        .map_err((e): ProcessError.Parse("invalid number: {e}"))
    return Record(name, value)

Vector[Record] process_file(Path path) throws ProcessError:
    auto file = fs.File.open(path)           # auto-propagates IoError → ProcessError
    auto reader = BufReader.new(file)
    Vector[Record] records = Vector.new()

    for line_result in reader.lines():
        String line = line_result             # auto-propagates
        if line.starts_with('#') or line.is_empty():
            continue
        auto record = parse_line(line)        # auto-propagates
        records.push(record)

    return records

void main():
    match try process_file(Path.new("data.csv")):
        case Ok(records):
            print("Processed {records.len()} records")
            int total = records.iter().map(it.value).sum()
            print("Total value: {total}")
        case Error(e):
            print("Error: {e}")
```

### 36.4 Trait Objects and Dynamic Dispatch

```gorget
trait Animal:
    str name(self)
    str speak(self)

    String describe(self):
        return "{self.name()} says {self.speak()}"

struct Dog:
    String name

struct Cat:
    String name
    bool indoor

equip Dog with Animal:
    str name(self):
        return self.name

    str speak(self):
        return "woof!"

equip Cat with Animal:
    str name(self):
        return self.name

    str speak(self):
        if self.indoor:
            return "mew"
        return "MEOW!"

void introduce_all(Vector[Box[dynamic Animal]] animals):
    for animal in animals:
        print(animal.describe())

void main():
    Vector[Box[dynamic Animal]] zoo = Vector.new()
    zoo.push(Box.new(Dog(String.from("Rex"))))
    zoo.push(Box.new(Cat(String.from("Whiskers"), true)))
    zoo.push(Box.new(Dog(String.from("Buddy"))))
    zoo.push(Box.new(Cat(String.from("Shadow"), false)))

    introduce_all(zoo)
    # Rex says woof!
    # Whiskers says mew
    # Buddy says woof!
    # Shadow says MEOW!
```

---

## 37. Formal Grammar Sketch (EBNF)

A simplified grammar showing the core structure:

```ebnf
program        = { top_level_item } ;
top_level_item = function_def | struct_def | enum_def | trait_def
               | equip_block | import_stmt | type_alias | newtype_def ;

(* Indentation produces INDENT/DEDENT tokens in the lexer, like Python *)
block          = COLON NEWLINE INDENT { statement } DEDENT ;

(* Functions *)
function_def   = { attribute } [ "public" ] [ "async" ] [ "const" ] [ "static" ]
                 return_type IDENT [ generic_params ] "(" [ param_list ] ")"
                 [ "throws" type ] [ where_clause ] ( block | "=" expr NEWLINE ) ;
return_type    = type | "void" ;
param_list     = param { "," param } ;
param          = type [ "&" | "!" ] IDENT [ "=" expr ] ;

(* Structs *)
struct_def     = { attribute } [ "public" ] "struct" IDENT [ generic_params ]
                 COLON NEWLINE INDENT { field_def } DEDENT ;
field_def      = [ "public" ] type IDENT NEWLINE ;

(* Enums *)
enum_def       = { attribute } [ "public" ] "enum" IDENT [ generic_params ]
                 COLON NEWLINE INDENT { variant } DEDENT ;
variant        = IDENT [ "(" type_list ")" ] NEWLINE ;

(* Traits *)
trait_def      = { attribute } [ "public" ] "trait" IDENT [ generic_params ]
                 [ "extends" trait_bound_list ]
                 COLON NEWLINE INDENT { trait_item } DEDENT ;
trait_item     = function_def | "type" IDENT [ COLON trait_bound_list ] NEWLINE ;

(* Equip blocks *)
equip_block    = "equip" [ generic_params ] type [ "with" type ]
                 COLON NEWLINE INDENT { function_def } DEDENT ;

(* Types *)
type           = primitive_type | IDENT [ generic_args ]
               | type "[" expr "]"           (* fixed array: int[5] *)
               | type "[" "]"               (* slice: int[] *)
               | "(" type_list ")"           (* tuple *)
               | "dynamic" IDENT
               | type "(" [ type_list ] ")"  (* function type: int(int, int) *) ;
generic_params = "[" generic_param { "," generic_param } "]" ;
generic_param  = IDENT [ COLON trait_bound_list ]
               | "life" IDENT
               | "const" type IDENT ;
generic_args   = "[" type { "," type } "]" ;
where_clause   = "where" where_bound { "," where_bound } ;
where_bound    = IDENT "is" trait_bound_list
               | IDENT "has" field_bound { "," field_bound } ;
trait_bound_list = trait_bound { "+" trait_bound } ;
trait_bound    = IDENT [ "[" assoc_type_binding { "," assoc_type_binding } "]" ] ;
assoc_type_binding = IDENT "=" type ;
field_bound    = "." IDENT ":" type ;

(* Expressions *)
expr           = assignment | binary_expr | unary_expr | call_expr
               | field_access | index_expr | match_expr | if_expr
               | do_expr | closure | literal | IDENT | "(" expr ")" ;
match_expr     = "match" expr block_with_cases ;
if_expr        = "if" expr block [ "elif" expr block ] [ "else" block ] ;
do_expr        = "do" block ;
closure        = [ "!" ] "(" [ param_list ] ")" ":" ( expr | block )
               | expr_using_it ;
func_type      = type "(" [ type_list ] ")" ;

(* Statements *)
statement      = var_decl | expr_stmt | return_stmt | throw_stmt
               | for_stmt | while_stmt | loop_stmt | if_stmt
               | match_stmt | with_stmt | break_stmt | continue_stmt ;
throw_stmt     = "throw" expr NEWLINE ;
return_stmt    = "return" [ expr ] NEWLINE ;
break_stmt     = "break" [ expr ] NEWLINE ;
continue_stmt  = "continue" NEWLINE ;
var_decl       = [ "const" ] ( type | "auto" ) IDENT "=" expr NEWLINE ;
for_stmt       = "for" IDENT "in" [ "&" | "!" ] expr block [ "else" block ] ;
while_stmt     = "while" expr block [ "else" block ] ;
loop_stmt      = "loop" block ;
with_stmt      = "with" with_binding { "," with_binding } block ;
with_binding   = expr "as" IDENT ;

(* Top-level declarations *)
import_stmt    = ( "import" dotted_name [ ".{" IDENT { "," IDENT } "}" ] NEWLINE )
               | ( "from" dotted_name "import" IDENT { "," IDENT } NEWLINE ) ;
type_alias     = "type" IDENT [ generic_params ] "=" type NEWLINE ;
newtype_def    = "newtype" IDENT "(" type ")" NEWLINE ;
attribute      = "@" IDENT [ "(" attr_args ")" ] NEWLINE ;
```

---

## 38. Potential Pitfalls & Mitigations

| Pitfall | Mitigation |
|---------|------------|
| Indentation + ownership = complex error messages | Invest heavily in error message quality |
| Bare/`&`/`!` syntax unfamiliar | Progressive: bare (90%) → `&` (9%) → `!` (1%) |
| Lifetime syntax is unfamiliar | Elide 95% of cases; make explicit syntax readable |
| Python devs expect GC | Clear docs: "this is not Python, it's Python-shaped Rust" |
| C/Java devs expect null | Option[T] with good sugar (`is`, `?`, `unwrap_or`) |
| Generics `[]` conflicts with indexing | Disambiguated by context: `arr[0]` vs `Vector[int]` (type position vs value position) |
| `auto` hides types, reduces readability | Enforce types at function boundaries; `auto` only for locals |

---

## 39. What Makes Gorget Worth Building?

1. **The "Readable Rust" gap is real**: Many developers want Rust's safety but find the syntax hostile. Gorget's indentation + bare/`&`/`!` borrowing + C-style types could genuinely lower the barrier.

2. **Python developers are the largest audience**: If you can give them memory safety without a GC while keeping the visual style they love, that's a huge unlock.

3. **C/Java type declarations are universal**: `int x = 5` is the most widely-understood variable declaration in programming. Rust's `let x: i32 = 5` is alien by comparison.

4. **Zero-cost abstractions without the ceremony**: Rust's `impl<T: Display + Clone> Foo<T> for Bar<'a, T> where T: Send + Sync` becomes `equip[T] Bar[T] with Foo[T] where T is Displayable + Cloneable + Sendable + Syncable:` — significantly less visual noise.

---

## 40. Standard Library (Batteries Included)

### 40.1 Philosophy

Gorget ships with a rich standard library — everything you need for common tasks without external dependencies. Like Python and Go, not like Rust.

### 40.2 Module Map

```
std/
├── core/               # Auto-imported, always available
│   ├── types           # Primitives, Option, Result, String, str, bool, tuples
│   ├── traits          # Displayable, Cloneable, Comparable, Equatable, Hashable, etc.
│   ├── ops             # Add, Sub, Mul, Div, Mod, Index (operator traits)
│   └── mem             # Box, Rc, Arc, size_of, drop
│
├── collections/        # Data structures
│   ├── Vector, HashMap, HashSet
│   ├── BTreeMap, BTreeSet      # Sorted collections
│   ├── LinkedList, VecDeque    # Specialized
│   └── BinaryHeap             # Priority queue
│
├── io/                 # Input/output
│   ├── Read, Write     # Core traits
│   ├── BufReader, BufWriter
│   ├── stdin, stdout, stderr
│   └── Cursor          # In-memory I/O
│
├── fs/                 # Filesystem
│   ├── File, read, write, create_dir, remove
│   ├── metadata, permissions
│   ├── walk_dir        # Recursive directory traversal
│   └── temp            # Temporary files/dirs
│
├── path/               # Path manipulation
│   ├── Path, PathBuf
│   └── join, parent, extension, stem
│
├── net/                # Networking
│   ├── TcpListener, TcpStream, UdpSocket
│   ├── IpAddr, SocketAddr
│   └── dns             # DNS resolution
│
├── http/               # HTTP (batteries included!)
│   ├── Client          # HTTP client (GET, POST, etc.)
│   ├── Request, Response
│   ├── Server          # Simple HTTP server
│   ├── Router          # URL routing
│   └── StatusCode, Headers
│
├── json/               # JSON (batteries included!)
│   ├── parse, stringify
│   ├── Value           # Dynamic JSON value
│   ├── Serializable    # Trait: to JSON
│   └── Deserializable  # Trait: from JSON
│
├── sync/               # Synchronization
│   ├── Mutex, RwLock
│   ├── Arc, Channel (mpsc)
│   ├── Barrier, Condvar
│   └── Once            # One-time initialization
│
├── thread/             # Threading
│   ├── spawn, sleep, JoinHandle
│   ├── ThreadLocal
│   └── pool            # Thread pool
│
├── async/              # Async runtime (built-in, Go-style)
│   ├── spawn           # Spawn async task
│   ├── sleep           # Async sleep
│   ├── select          # Wait on multiple futures
│   ├── Channel         # Async channel
│   ├── Mutex           # Async mutex
│   └── timeout         # Timeout wrapper
│
├── time/               # Time and duration
│   ├── Instant, Duration, SystemTime
│   ├── format, parse   # Date/time formatting
│   └── Timer           # Periodic timer
│
├── math/               # Mathematics
│   ├── constants (PI, E, TAU)
│   ├── min, max, abs, clamp
│   ├── sqrt, pow, log, exp
│   ├── floor, ceil, round
│   └── sin, cos, tan, atan2
│
├── regex/              # Regular expressions
│   ├── Regex, Match
│   ├── compile, is_match, find, find_all
│   └── replace, split
│
├── fmt/                # Formatting
│   ├── Displayable, Debuggable
│   ├── format          # String formatting
│   └── Formatter       # Custom format control
│
├── convert/            # Type conversions
│   ├── From, Into, TryFrom, TryInto
│   └── parse           # String parsing
│
├── iter/               # Iterator utilities
│   ├── Iterable, Iterator
│   ├── range, repeat, once, empty
│   ├── chain, zip, enumerate
│   └── map, filter, fold, collect (methods on Iterator)
│
├── hash/               # Hashing
│   ├── Hashable, Hasher
│   └── DefaultHasher
│
├── env/                # Environment
│   ├── args, var, vars
│   ├── current_dir, home_dir
│   └── temp_dir
│
├── process/            # Process management
│   ├── Command, Output # Run external commands
│   ├── exit, abort
│   └── id              # Current process ID
│
├── os/                 # OS-specific APIs
│   ├── signals
│   └── platform info
│
├── log/                # Logging (batteries included!)
│   ├── debug, info, warn, error
│   ├── Logger          # Configurable logger
│   └── Level           # Log levels
│
├── crypto/             # Basic cryptography
│   ├── hash            # SHA-256, SHA-512, etc.
│   ├── hmac
│   └── random          # Cryptographic random
│
├── random/             # Random number generation
│   ├── random, random_range
│   ├── Rng             # Random generator trait
│   └── shuffle, choose
│
├── encoding/           # Data encoding
│   ├── base64
│   ├── hex
│   └── utf8
│
└── testing/            # Test framework
    ├── assert, assert_eq, assert_ne
    ├── Bencher
    └── mock             # Basic mocking utilities
```

### 40.3 Core Traits (auto-imported, always available)

```gorget
# These are always available without import:
trait Displayable       # .to_string() — human-readable representation
trait Debuggable        # .debug() — development/debug representation
trait Cloneable         # .clone() — deep copy
trait Comparable        # .compare() — total ordering (enables <, >, <=, >=)
trait Equatable         # .equals() — equality (enables ==, !=)
trait Hashable          # .hash() — hash value for HashMap/HashSet
trait Default           # .default() — default value
trait Copy              # Marker: bitwise copyable (primitives)
trait Iterable          # .iter() — produce an Iterator
trait Serializable      # .to_json(), .to_bytes() — serialization
trait Deserializable    # .from_json(), .from_bytes() — deserialization
```

### 40.4 Async Runtime (Built-in)

The async runtime is part of the language — no external dependency needed.

```gorget
import std.async

# Spawning tasks
async void main():
    auto task1 = async.spawn(fetch("https://api.example.com/a"))
    auto task2 = async.spawn(fetch("https://api.example.com/b"))

    String a = await task1
    String b = await task2

# Select (wait for first completion)
async void race():
    match async.select(fetch(url1), fetch(url2), async.sleep(Duration.seconds(5))):
        case (0, result): print("First URL won: {result}")
        case (1, result): print("Second URL won: {result}")
        case (2, _): print("Timeout!")

# Async channels
async void producer_consumer():
    auto (sender, receiver) = async.Channel[int].new()

    async.spawn(!():
        for i in 0..10:
            sender.send(i)
    )

    while await receiver.recv() is Some(value):
        print("Got: {value}")

# Timeouts
async void with_timeout():
    match async.timeout(Duration.seconds(5), fetch(url)):
        case Ok(data): print(data)
        case Error(_): print("Timed out!")
```

### 40.5 HTTP (Built-in)

```gorget
import std.http

# HTTP client — simple
async void fetch_example() throws HttpError:
    # Simple GET
    String body = await http.get("https://api.example.com/data").text()

    # POST with JSON
    auto resp = await http.post("https://api.example.com/users")
        .json(User("Alice", 30))
        .send()

    User user = resp.json[User]()

# HTTP server — simple
void main():
    auto server = http.Server.new()

    server.get("/", (Request req):
        Response.ok("Hello, Gorget!")
    )

    server.get("/users/{id}", (Request req):
        int id = req.param("id").parse[int]() ?? 0
        auto user = db.find(id)
        if user is Some(u):
            return Response.json(u)
        Response.not_found("User not found")
    )

    server.listen("0.0.0.0:8080")
```

### 40.6 JSON (Built-in)

```gorget
import std.json

# Serialize — any type with @derive(Serializable)
@derive(Serializable, Deserializable)
struct User:
    String name
    int age

String json_str = json.stringify(User("Alice", 30))
# {"name": "Alice", "age": 30}

# Deserialize
User user = json.parse[User](json_str)

# Dynamic JSON
json.Value data = json.parse(raw_string)
auto name = data["name"].as_string() ?? "unknown"
auto items = data["items"].as_array()
```

---

## 41. Implementation Roadmap

1. **Phase 1 - Specification** (this document, then formalize)
   - Formal grammar (EBNF), type system rules, borrow checker rules
   - Write a language reference document

2. **Phase 2 - Lexer/Parser** (Rust implementation)
   - Indentation-aware lexer (emits INDENT/DEDENT tokens)
   - Recursive descent or PEG parser -> AST
   - Libraries: `logos` for lexing, hand-written parser (more control for indentation)

3. **Phase 3 - Semantic Analysis**
   - Name resolution, type checking, type inference
   - Trait resolution, generic monomorphization
   - Borrow checker (MIR-based, like Rust's)

4. **Phase 4 - Code Generation**
   - HIR -> MIR -> LLVM IR
   - Optimization passes
   - Target: native binaries via LLVM

5. **Phase 5 - Standard Library** (batteries included)
   - Core types, traits, collections, iterators
   - I/O, filesystem, networking, HTTP, JSON
   - Built-in async runtime, threading, sync primitives
   - Regex, crypto, logging, encoding, random

6. **Phase 6 - Tooling**
   - `forge` package manager
   - `gg fmt` formatter
   - LSP server for editor support
   - `gg doc` documentation generator

7. **Phase 7 - Ecosystem**
   - Package registry (foundry?)
   - Community, tutorials, books
