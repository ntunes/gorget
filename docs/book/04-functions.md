# Functions

Functions are the basic unit of code organization. Gorget functions declare their
return type first, take typed parameters, and can be as short as a single expression.

---

## Defining Functions

A function has a return type, a name, parameters, and a body:

```gorget
int add(int a, int b):
    return a + b

float average(float x, float y):
    float sum = x + y
    return sum / 2.0
```

Functions that don't return a value use `void`:

```gorget
void greet(String name):
    print(f"Hello, {name}!")
```

### Every Path Must Return

A function with a non-`void` return type must return on **every** path
through its body — the compiler rejects a function that could reach the
end of its body without a `return`:

```gorget
int sign(int x):
    if x > 0:
        return 1      # error: what if x <= 0? control falls off the end
```

Add an `else` (or a trailing `return`) so every path produces a value.
Paths that `throw`, call a diverging function like `panic`, or enter a
loop that never exits normally also count as returning — they never reach
the end of the body.

### Expression-Body Shorthand

When a function is a single expression, skip the block:

```gorget
int double(int x): x * 2
bool is_positive(int n): n > 0
float square(float x): x * x
```

This expression-body form is equivalent to `return expr` in a block. Use it when the
function fits on one line.

---

## Parameters

Parameters are typed with the type-first style:

```gorget
String format_greeting(String name, int age):
    return f"Hello {name}, age {age}"
```

### Ownership Modes

Parameters control how values are passed. This is covered fully in the
ownership chapter, but here's the quick version:

```gorget
void read(Message msg):         # immutable borrow (default)
    print(msg.subject)

void modify(Message &msg):      # mutable borrow
    msg.priority = 5

void consume(Message !msg):     # move — caller gives up ownership
    archive(msg)
```

At the call site:

```gorget
read(msg)           # borrow
modify(&msg)        # mutable borrow
consume(!msg)       # move — msg is invalid after this
```

Copy types (integers, floats, bools) don't need `!` — they're implicitly
copied. Resource types (String, Vector, Dict, user structs with resource
fields) are **never** copied by value — the bare parameter mode automatically
creates an immutable borrow. This is zero-cost (a pointer) and prevents
aliasing bugs. To take ownership, use `!`.

---

## Multiple Return Values

Functions can return tuples:

```gorget
String, int parse(String line):
    return "key", 42

void main():
    auto key, value = parse("key:42")
    print(f"{key}: {value}")
```

The bare form (no parentheses) is preferred for both declaration and unpacking.

---

## Recursion

Functions can call themselves:

```gorget
int factorial(int n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

void main():
    print(f"{factorial(10)}")    # 3628800
```

---

## Closures

A closure is an anonymous function that captures variables from its surrounding
scope:

```gorget
int x = 10
auto add_x = (int y): x + y
print(f"{add_x(5)}")     # 15
print(f"{add_x(20)}")    # 30
```

The syntax is `(parameters): body`. Parameter types can be inferred:

```gorget
auto add = (a, b): a + b
print(f"{add(3, 4)}")    # 7
```

### Multi-Line Closures

Indent the body for multiple statements:

```gorget
auto process = (int x):
    int result = x * 2
    result += 1
    result
```

The last expression is the return value (no `return` keyword needed).

### Capturing Variables

Closures capture by **immutable borrow** by default — the same rule as a bare
parameter. A closure that only reads a captured variable does not own it, and
the original is untouched.

To write *through* to the outer variable, the capture is marked with `&`, the
same sigil you would use at a call:

```gorget
int count = 0
auto increment = (&count)():
    count += 1
increment()
increment()
increment()
print(count)    # 3
```

A `&`-capture is exclusive while the closure is alive: the closure holds the
only writable path to `count`, so you cannot read `count` from outside until
the closure is done with it.

> Two things above are specification rather than today's compiler. The
> capture-list syntax is not implemented — the mode is currently inferred from
> what the closure body does, and a `&` written *inside* the body has no effect.
> And a bare capture currently behaves as a snapshot taken when the closure is
> created, rather than as a borrow.

### Move Closures

To force a closure to take ownership of captured variables, prefix with `!`:

```gorget
auto handler = !(msg):
    print(f"got: {msg}")
```

### No-Argument Closures

```gorget
auto greet = (): print("hello")
greet()
```

---

## Function Types

Functions are values. You can store them in variables and pass them as arguments:

```gorget
int(int, int) operation = (a, b): a + b
print(f"{operation(3, 4)}")    # 7

operation = (a, b): a * b
print(f"{operation(3, 4)}")    # 12
```

The type syntax is `ReturnType(ParamTypes)`:

```gorget
int(int) transformer            # takes int, returns int
void() callback                 # takes nothing, returns nothing
bool(String, String) comparator  # takes two strings, returns bool
```

### Passing Functions as Arguments

```gorget
int apply(int(int) f, int x):
    return f(x)

void main():
    int result = apply((n): n * 2, 5)
    print(f"{result}")    # 10
```

---

## Summary

| Concept | Syntax | Example |
|---------|--------|---------|
| Function | `RetType name(params): body` | `int add(int a, int b): return a + b` |
| Expression body | `RetType name(params): expr` | `int double(int x): x * 2` |
| Void function | `void name(params): body` | `void greet(String s): print(s)` |
| Multiple returns | `T1, T2 name(params):` | `String, int parse(String s):` |
| Closure | `(params): expr` | `(x): x * 2` |
| Move closure | `!(params): expr` | `!(x): process(x)` |
| Function type | `RetType(ParamTypes)` | `int(int, int)` |
