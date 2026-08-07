# Variables, Types, and Operators

Gorget is statically typed: every variable has a type known at compile time. But
you don't always have to write it — `auto` lets the compiler figure it out when the
type is obvious.

---

## Variable Declarations

Variables are declared with the type first, then the name:

```gorget
int x = 5
float pi = 3.14159
bool active = true
String greeting = "hello"
String letter = 'A'
```

This is the **type-first** style: `int x`, not `x: int`. It reads like natural
language — "an integer called x."

### Type Inference with `auto`

When the type is obvious from the right-hand side, use `auto`:

```gorget
auto x = 42           # inferred as int
auto name = "hello"   # inferred as String
auto pi = 3.14        # inferred as float
auto flag = true      # inferred as bool
```

`auto` is not dynamic typing — the compiler infers and locks the type at
compile time. You can't reassign `x` to a string later.

### Constants

Use `const` for values that never change:

```gorget
const int MAX_SIZE = 1024
const auto LIMIT = 500
```

Constants are compile-time fixed. Reassigning a `const` is a compile error.

---

## Primitive Types

### Numbers

| Type | Size | Range |
|------|------|-------|
| `int` | 64-bit signed | Default for integer literals |
| `int8`, `int16`, `int32` | 8/16/32-bit signed | Smaller integers |
| `uint`, `uint8`, `uint16`, `uint32` | Unsigned variants | Non-negative integers |
| `float` | 64-bit (double) | Default for float literals |
| `float32` | 32-bit (single) | Lower precision |

Integer literals are `int` by default. Float literals (anything with a `.`) are
`float` by default.

```gorget
int big = 9223372036854775807    # int max
uint8 byte_val = 255
float64 precise = 3.141592653589793
```

### Booleans

```gorget
bool yes = true
bool no = false
```

### Strings

Gorget has a single string type: **`String`**. Both single-quoted (`'A'`) and
double-quoted (`"hello"`) literals produce `String` values.

```gorget
String greeting = "hello"
String letter = 'A'
String name = "world"
String combined = greeting + " " + name   # concatenation creates new String
```

Behind the scenes, `String` is a 32-byte value type. String literals, `slice()`,
`trim()`, and `char_at()` are **zero-allocation views** — they point into
existing data without copying. Concatenation, f-strings, and methods like
`to_upper()` produce **owned** copies. The compiler auto-materializes views
when you mutate the source (copy-on-write). You don't need to think about
this — just use `String` everywhere.

### String Interpolation

Prefix a string with `f` to enable interpolation with `{}`:

```gorget
String name = "Alice"
int age = 30
print(f"Name: {name}, Age: {age}")
```

To print literal braces in an f-string, double them: `f"{{escaped}}"` prints `{escaped}`. Plain strings without the `f` prefix treat `{` and `}` as literal characters.

### Void

`void` is the unit type — it carries no information. Functions that don't return
a value use `void`:

```gorget
void greet(String name):
    print(f"Hello, {name}!")
```

---

## Type Casting

The `as` keyword converts between compatible types:

```gorget
float f = 42 as float       # int to float
int n = 3.14 as int         # float to int (truncates toward zero)
uint8 b = 255 as uint8      # narrowing cast
```

Casts between numeric types are always allowed. The compiler won't silently
narrow or widen — you must be explicit.

---

## Operators

### Arithmetic

```gorget
x + y          # addition
x - y          # subtraction
x * y          # multiplication
x / y          # division (integer division if both are int)
x % y          # remainder (sign follows dividend)
x.mod(y)       # Euclidean modulo (sign follows divisor)
-x             # negation
```

The distinction between `%` and `.mod()`: `-7 % 3` is `-1` (remainder), while
`(-7).mod(3)` is `2` (Euclidean modulo). Use `.mod()` when you want the result
to always match the divisor's sign.

### Comparison

```gorget
x == y         # equal
x != y         # not equal
x < y          # less than
x > y          # greater than
x <= y         # less than or equal
x >= y         # greater than or equal
```

### Logical

```gorget
a and b        # logical AND (short-circuiting)
a or b         # logical OR (short-circuiting)
not a          # logical NOT
```

Gorget uses words, not symbols: `and`/`or`/`not` instead of `&&`/`||`/`!`. This
avoids conflicts with `&` (borrow) and `!` (move).

### Bitwise

```gorget
x & y          # bitwise AND
x | y          # bitwise OR
x ^ y          # bitwise XOR
~x             # bitwise NOT
x << n         # left shift
x >> n         # right shift
```

### Compound Assignment

```gorget
x += 1         # x = x + 1
x -= 1         # x = x - 1
x *= 2         # x = x * 2
x /= 2         # etc.
x %= 5
x &= 0xFF
x |= 0x01
x <<= 2
x >>= 3
```

### Wrapping Arithmetic

Integer overflow always panics (catches bugs). When you intentionally want
wrapping behavior, use the per-operator wrapping forms:

```gorget
x +% y         # wrapping add
x -% y         # wrapping subtract
x *% y         # wrapping multiply
```

Wrapping is per-expression by design — there is no whole-file or whole-build
mode that changes what plain `+`/`-`/`*` do.

### Exponentiation with `**`

`x ** y` raises `x` to the power `y`:

```gorget
int r  = 2 ** 10          # 1024
float f = 2.0 ** 0.5      # 1.414... (square root)
int a  = 2
a **= 10                  # compound assign — a = 1024
```

`**` is right-associative (`2 ** 3 ** 2` is `2 ** (3 ** 2)` = 512, matching Fortran/Python/JS/Ruby), binds tighter than unary `-`, and does not switch types (`int ** int → int`, `float ** float → float`; mixed operands are rejected). An integer overflow OR negative exponent traps uncatchably — write `**` explicitly with matching types.

Unparenthesized `-x ** 2` is rejected as ambiguous (does the `-` apply to `x` or to `x ** 2`?) — write `-(x ** 2)` or `(-x) ** 2`.

> **`^` is bitwise XOR, not power.** `2 ^ 10` = 8 (XOR), not 1024. The compiler emits a fix-it warning on the narrow shape `{2 | 10} ^ N` where `N` looks like an exponent.

### Fallible Arithmetic

Between the panic-on-overflow default (`+`) and the wrapping form (`+%`),
Gorget offers a third discipline: the **fallible** arithmetic operators
`+!`, `-!`, `*!`, `/!`, `%!`, `<<!`, `>>!`. They surface the failure into
the ordinary `throws` / `Result` channel instead of trapping.

```gorget
int add_or_throw(int a, int b):
    return a +! b               # auto-infers `throws ArithError`

void main():
    int r = add_or_throw(2, 3)! catch (e): -1
    print(r)                     # 5

    # Capture form — the Result value is the destination.
    Result[int, ArithError] r2 = 9223372036854775807 +! 1
    match r2:
        case Ok(v):  print(v)
        case Error(e): print(-1)  # Overflow branch: prints -1
```

`ArithError` is a prelude enum with two variants:

```gorget
enum ArithError:
    Overflow       # add/sub/mul, shift range, signed INT_MIN/-1 div
    DivByZero      # / and % with rhs == 0
```

**Auto-inference.** A function body that syntactically contains any
fallible-arith operator auto-infers `throws ArithError` on its signature —
silently, without a diagnostic. Users see the throws in callers via the
ordinary `!` propagate and `catch` machinery, so nothing else in the
function needs to change. An explicit `throws E` declaration wins over
auto-inference (the user's contract is preserved). `main()` is not auto-
inferred, since `main` can only throw `int` — every `+!` in `main` must
be captured or catch-handled at the use site.

Fallible arithmetic is INTEGER-ONLY in v1 (a float operand rejects at
check with `E_FallibleArithmeticOnNonInt`) and compound forms (`+!=`,
`-!=`, …) are excluded. See the language reference §7.5 and §10.9 for
the full specification.

---

## Summary

| Concept | Syntax | Example |
|---------|--------|---------|
| Type-first declaration | `Type name = expr` | `int x = 5` |
| Type inference | `auto name = expr` | `auto x = 42` |
| Constant | `const Type name = expr` | `const int MAX = 100` |
| Type cast | `expr as Type` | `42 as float` |
| Arithmetic | `+`, `-`, `*`, `/`, `%`, `.mod()` | `x + y`, `x.mod(3)` |
| Wrapping arithmetic | `+%`, `-%`, `*%` | `a +% b` (never traps) |
| Fallible arithmetic | `+!`, `-!`, `*!`, `/!`, `%!`, `<<!`, `>>!` | `a +! b` → `Result[T, ArithError]` |
| Comparison | `==`, `!=`, `<`, `>`, `<=`, `>=` | `x == y` |
| Logical | `and`, `or`, `not` | `a and b` |
| Bitwise | `&`, `\|`, `^`, `~`, `<<`, `>>` | `x & 0xFF` |
| String interpolation | `f"{expr}"` | `f"x is {x}"` |
