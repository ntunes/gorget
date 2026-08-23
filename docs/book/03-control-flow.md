# Control Flow

Gorget uses indentation-based blocks — a colon starts the block, indentation
defines it. No braces, no `end` keywords.

---

## If / Elif / Else

```gorget
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")
```

`elif` chains as many branches as needed. `else` is optional.

### If as an Expression

`if` can return a value:

```gorget
int abs_val = if x >= 0: x else: -x
String label = if count == 1: "item" else: "items"
```

---

## For Loops

### Range Iteration

```gorget
for i in 0..5:           # [0, 1, 2, 3, 4] — exclusive end
    print(f"{i}")

for i in 0..=5:          # [0, 1, 2, 3, 4, 5] — inclusive end
    print(f"{i}")
```

### Collection Iteration

```gorget
auto items = [10, 20, 30]
for x in items:
    print(f"{x}")
```

### Tuple Unpacking

```gorget
auto pairs = [(1, "one"), (2, "two"), (3, "three")]
for num, name in pairs:
    print(f"{num}: {name}")
```

### For-Else

The `else` block runs if the loop completes without `break`:

```gorget
for item in items:
    if item == target:
        print("found!")
        break
else:
    print("not found")
```

This replaces the common "found" flag pattern. If `break` fires, `else` is skipped.

---

## While Loops

```gorget
int count = 0
while count < 5:
    print(f"{count}")
    count += 1
```

### While-Else

Same as for-else — the `else` runs on normal completion (no `break`):

```gorget
int tries = 0
while tries < 3:
    if try_connect():
        break
    tries += 1
else:
    print("all attempts failed")
```

---

## Infinite Loops

```gorget
loop:
    String input = readline()
    if input == "quit":
        break
    process(input)
```

`loop` runs forever until `break`. Clearer than `while true`.

---

## Break and Continue

`break` exits the loop. `continue` skips to the next iteration:

```gorget
for i in 0..10:
    if i % 2 == 0:
        continue           # skip even numbers
    if i > 7:
        break              # stop at 7
    print(f"{i}")           # prints 1, 3, 5, 7
```

---

## Match / Case

Pattern matching is Gorget's most powerful control flow construct. It replaces
`switch` statements with something far more capable.

### Basic Matching

```gorget
match color:
    case "red":
        print("stop")
    case "green":
        print("go")
    case "yellow":
        print("caution")
    else:
        print("unknown")
```

### Matching Enum Variants

```gorget
enum Direction:
    North
    South
    East
    West

match direction:
    case North:
        print("going north")
    case South:
        print("going south")
    case East:
        print("going east")
    case West:
        print("going west")
```

### Exhaustiveness

A `match` on an enum must account for **every** variant. The compiler knows the
full set, so leaving one out is an error, not a silent fall-through:

```gorget
enum Direction:
    North
    South
    East
    West

match direction:          # error: non-exhaustive match: missing variants: West
    case North:
        print("going north")
    case South:
        print("going south")
    case East:
        print("going east")
```

The error names the variants you forgot, in declaration order. Add them, or add
an `else` — the catch-all that covers everything remaining:

```gorget
match direction:
    case North:
        print("going north")
    else:
        print("some other way")
```

A bare name works as a catch-all too, and binds the matched value:

```gorget
match direction:
    case North:
        print("going north")
    case rest:
        print("some other direction")
```

Two things do *not* count toward coverage.

A **guarded** arm doesn't, because a guard can be false — the arm might not run
even when the pattern fits:

```gorget
match direction:          # error: non-exhaustive match: missing variants: West
    case North:
        print("going north")
    case South:
        print("going south")
    case East:
        print("going east")
    case West if windy:   # a guard is a maybe, so this covers nothing
        print("going west")
```

And **non-enum** scrutinees aren't checked at all. An `int` or a `String` has no
closed set of values for the compiler to compare against, so a `match` on one is
accepted with whatever arms you give it. If you want a default, write it:

```gorget
match count:
    case 0:
        print("none")
    case 1:
        print("one")
    else:
        print("many")
```

This is why enums are worth reaching for. When the set of possibilities lives in
the type, adding a variant turns every place that handles it into a compile
error you have to answer — the compiler hands you the list of sites to update
instead of leaving you to find them at runtime.

### Destructuring Payloads

Enum variants with data can be destructured:

```gorget
Option[int] result = Some(42)
match result:
    case Some(value):
        print(f"got {value}")
    case None:
        print("nothing")
```

```gorget
Result[int, String] r = Ok(100)
match r:
    case Ok(v):
        print(f"success: {v}")
    case Error(msg):
        print(f"failed: {msg}")
```

### Guards

Add conditions to cases with `if`:

```gorget
match value:
    case x if x > 100:
        print("large")
    case x if x > 0:
        print("positive")
    case 0:
        print("zero")
    else:
        print("negative")
```

### Match as an Expression

```gorget
String label = match color:
    case "red": "danger"
    case "green": "safe"
    else: "neutral"
```

### The `is` Keyword

For quick pattern checks without a full `match`:

```gorget
if result is Some(value):
    print(f"got {value}")

if response is Error(msg):
    print(f"failed: {msg}")

if option is None:
    print("absent")
```

---

## Pass

A no-op placeholder for empty blocks:

```gorget
if condition:
    pass       # TODO: implement later

void stub():
    pass
```

---

## Summary

| Construct | Syntax | Notes |
|-----------|--------|-------|
| If/elif/else | `if cond: ... elif: ... else: ...` | Also works as expression |
| For loop | `for x in collection:` | Ranges: `0..5`, `0..=5` |
| For-else | `for x in items: ... else: ...` | Else runs if no `break` |
| While | `while cond:` | While-else also available |
| Infinite loop | `loop:` | Exit with `break` |
| Break / Continue | `break`, `continue` | Standard loop control |
| Match | `match expr: case pattern: ...` | Destructuring, guards, expressions |
| Exhaustiveness | every enum variant, or an `else` | Guards don't count; non-enums aren't checked |
| Is | `if x is Pattern:` | Quick pattern check |
| Pass | `pass` | No-op placeholder |
