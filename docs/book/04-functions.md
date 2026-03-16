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
void greet(str name):
    print(f"Hello, {name}!")
```

### Expression-Body Shorthand

When a function is a single expression, skip the block:

```gorget
int double(int x): x * 2
bool is_positive(int n): n > 0
float square(float x): x * x
```

The `= expr` form is equivalent to `return expr` in a block. Use it when the
function fits on one line.

---

## Parameters

Parameters are typed with the type-first style:

```gorget
str format_greeting(str name, int age):
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

Copy types (integers, floats, bools, chars) don't need `!` — they're implicitly
copied.

---

## Multiple Return Values

Functions can return tuples:

```gorget
str, int parse(str line):
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

Closures capture by reference by default. They can read and mutate captured
variables:

```gorget
int count = 0
auto increment = ():
    count += 1
increment()
increment()
increment()
print(f"{count}")    # 3
```

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
bool(str, str) comparator       # takes two strings, returns bool
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
| Void function | `void name(params): body` | `void greet(str s): print(s)` |
| Multiple returns | `T1, T2 name(params):` | `str, int parse(str s):` |
| Closure | `(params): expr` | `(x): x * 2` |
| Move closure | `!(params): expr` | `!(x): process(x)` |
| Function type | `RetType(ParamTypes)` | `int(int, int)` |
