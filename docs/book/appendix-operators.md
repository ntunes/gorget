# Appendix A — Operator Precedence

Operators listed from highest precedence (binds tightest) to lowest.

## Precedence Table

| Precedence | Operators | Category | Associativity |
|:----------:|-----------|----------|:-------------:|
| 20 | `.` `?.` `[]` `()` `.0` `.1` ... | Postfix / access | Left |
| 20 | `-` `not` `~` `*` | Unary prefix | Right |
| 20 | `!` | Move | Right |
| 20 | `&` | Mutable borrow | Right |
| 20 | `spawn` `spawn blocking` | Task creation | Right |
| 18 | `as` | Type cast | Left |
| 17 | `*` `/` `%` `*%` | Multiplicative | Left |
| 16 | `+` `-` `+%` `-%` | Additive | Left |
| 15 | `<<` `>>` | Shift | Left |
| 14 | `..` `..=` | Range | Non-assoc |
| 13 | `&` (bitwise) | Bitwise AND | Left |
| 12 | `^` | Bitwise XOR | Left |
| 11 | `\|` | Bitwise OR | Left |
| 10 | `in` | Membership | Left |
| 9 | `<` `>` `<=` `>=` | Relational | Left |
| 8 | `==` `!=` | Equality | Left |
| 7 | `is` `is not` | Type test | Left |
| 6 | `and` | Logical AND | Left |
| 5 | `or` | Logical OR | Left |
| 4 | `??` | Default | Right |
| 3 | `rethrow` `catch` | Error transform / recovery | Right |

---

## Arithmetic Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `+` | Addition | `3 + 4` |
| `-` | Subtraction | `10 - 3` |
| `*` | Multiplication | `5 * 6` |
| `/` | Division | `10 / 3` |
| `%` | Remainder | `10 % 3` |
| `.mod(n)` | Euclidean modulo (method) | `(-7).mod(3)` |
| `-` (unary) | Negation | `-x` |

### Wrapping Arithmetic

| Operator | Operation | Example |
|----------|-----------|---------|
| `+%` | Wrapping add | `255_u8 +% 1` |
| `-%` | Wrapping subtract | `0_u8 -% 1` |
| `*%` | Wrapping multiply | `200_u8 *% 2` |

Wrapping operators never panic on overflow. They wrap around using modular arithmetic.
Plain `+`/`-`/`*` always check overflow (trap uncatchably on fault); the `+%`/`-%`/`*%` operators are the only way to opt into wrapping, per-expression, and the `+!`/`-!`/`*!`/`/!`/`%!`/`<<!`/`>>!` operators are the way to capture the failure as `Result[T, ArithError]` instead.

---

## Comparison Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `==` | Equal | `x == 5` |
| `!=` | Not equal | `x != 0` |
| `<` | Less than | `a < b` |
| `>` | Greater than | `a > b` |
| `<=` | Less or equal | `a <= b` |
| `>=` | Greater or equal | `a >= b` |

---

## Logical Operators

| Operator | Operation | Short-circuits |
|----------|-----------|:--------------:|
| `and` | Logical AND | Yes |
| `or` | Logical OR | Yes |
| `not` | Logical NOT | N/A |

---

## Bitwise Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `&` | Bitwise AND | `flags & MASK` |
| `\|` | Bitwise OR | `flags \| FLAG` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT | `~mask` |
| `<<` | Left shift | `1 << n` |
| `>>` | Right shift | `x >> 4` |

---

## Access and Call Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `.` | Field access / method call | `point.x`, `v.len()` |
| `?.` | Optional chaining | `user?.name` |
| `[]` | Index access | `items[0]` |
| `()` | Function call | `add(1, 2)` |
| `.0`, `.1` | Tuple element access | `pair.0` |

---

## Special Operators

| Operator | Operation | Example |
|----------|-----------|---------|
| `??` | Default (lazy) | `opt ?? fallback()` |
| `..` | Exclusive range | `0..10` |
| `..=` | Inclusive range | `0..=9` |
| `as` | Type cast | `x as float` |
| `is` | Type test | `x is int` |
| `is not` | Negative type test | `x is not None` |
| `in` | Membership test | `x in collection` |
| `^` | Move operator | `take(^value)` |
| `&` | Mutable borrow | `modify(&value)` |
| `*` | Dereference | `*ptr` (unsafe) |
| `rethrow` | Error transform | `risky() rethrow (e): wrap(e)` |
| `catch` | Error recovery | `risky() catch (e): fallback` |

---

## Assignment Operators

Assignment is a statement, not an expression. Compound forms:

| Operator | Equivalent |
|----------|------------|
| `+=` | `x = x + rhs` |
| `-=` | `x = x - rhs` |
| `*=` | `x = x * rhs` |
| `/=` | `x = x / rhs` |
| `%=` | `x = x % rhs` |
| `&=` | `x = x & rhs` |
| `\|=` | `x = x \| rhs` |
| `^=` | `x = x ^ rhs` |
| `<<=` | `x = x << rhs` |
| `>>=` | `x = x >> rhs` |
