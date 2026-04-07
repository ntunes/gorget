# `meta` — Compile-Time Evaluation Design

## Overview

Gorget adopts a restricted compile-time evaluation system under the `meta` keyword. Inspired
by Zig's `comptime` but tailored to Gorget's philosophy: one keyword, minimal syntax,
complements existing generics rather than replacing them.

**Principle:** One keyword (`meta`) that modifies existing constructs. No new control flow,
no new sigils. Types are first-class values in meta contexts.

## Keyword: `meta`

Short (4 chars), language-agnostic, carries the right connotation — "about the program itself."
Reads naturally as an adjective: "meta integer," "meta assertion," "meta if," "meta type."

Leaves room to grow into reflection, type introspection, and code generation — all naturally
"meta" concepts — without being narrowly tied to "compile time" as a concept.

## Syntax

### Compile-time constants

```
meta int MAX_CONNECTIONS = 1024
meta int BUFFER_SIZE = MAX_CONNECTIONS * 64
meta str VERSION = "2.1.0"
meta float PI = 3.14159265358979
```

The right-hand side must be evaluable at compile time: literals, arithmetic/string/boolean ops
on other `meta` values, or calls to compile-time-evaluable functions.

### Compile-time type aliases and computation

Types are first-class values in `meta` contexts. They can be named, computed, and chosen
conditionally — but they remain compile-time-only. You cannot store a type in a runtime
variable or branch on a type at runtime.

**Type aliases:**

```
meta type IntVec = Vector[int]
meta type Coordinate = Pair[float, float]
meta type Callback = int(str, int)

IntVec items = IntVec()
Coordinate pos = Coordinate(1.0, 2.0)
```

**Conditional types:**

```
meta type Map = Dict if feature("ordered") else HashMap

Map[str, int] counts = Map[str, int]()
# Compiles to Dict[str, int] or HashMap[str, int] depending on build flags
```

**Type configuration:**

```
meta type Precision = float if feature("fast") else double
meta type Index = int32 if MAX_ENTITIES <= 2147483647 else int64

Precision compute(Precision a, Precision b):
    return a * b + a

Index lookup(str key):
    ...
```

**Computed types via meta functions:**

```
meta type sized_int(int bits):
    if bits <= 8:
        return int8
    elif bits <= 16:
        return int16
    elif bits <= 32:
        return int32
    else:
        return int64

meta type Word = sized_int(arch_word_bits())
Word value = 42

meta type IdType = sized_int(ID_BITS)
IdType user_id = 0
```

Meta functions that return types use `meta type` as their declaration prefix. The body is
regular Gorget control flow — `if/elif/else`, `match`, etc. — but operates on compile-time
values and returns a type.

### Compile-time assertions

```
meta assert BUFFER_SIZE <= 1048576, "buffer too large"
meta assert MAX_CONNECTIONS > 0
meta assert TABLE_SIZE == 1024, "expected power of two"
```

Checked during compilation. Failure is a compile error with the provided message.

### Conditional compilation

```
meta if platform() == "macos":
    import cocoa
    str DEFAULT_PATH = "/Library/Frameworks"
elif platform() == "linux":
    import x11
    str DEFAULT_PATH = "/usr/lib"
else:
    meta assert false, "unsupported platform"
```

The `meta` prefix on the first `if` makes the entire `if/elif/else` chain compile-time.
Only the taken branch is emitted to C. Dead branches are pruned entirely — they don't need
to type-check or resolve imports.

### Feature flags

```
# Passed via: gg build --feature debug --feature metrics
meta if feature("debug"):
    import debug_tools

    run():
        debug_tools.init()
        main()
elif feature("metrics"):
    import metrics

    run():
        metrics.start()
        main()
        metrics.report()
```

### Compile-time function evaluation

Functions are NOT marked `meta`. Any pure function (no I/O, no allocation, no side effects)
can be called from a `meta` context. The compiler evaluates it and substitutes the result.

```
int next_pow2(int n):
    int result = 1
    while result < n:
        result = result * 2
    return result

meta int TABLE_SIZE = next_pow2(MAX_CONNECTIONS)    # evaluated at compile time
int dynamic_size = next_pow2(user_input)             # evaluated at runtime — same function
```

If a function called in `meta` context contains impure operations (I/O, heap allocation,
syscalls), the compiler emits an error at the call site:

```
str read_version():
    return File.read("VERSION")   # I/O — impure

meta str V = read_version()  # ERROR: cannot evaluate at compile time: read_version()
                              #        contains I/O operation at line N
```

### What `meta` can modify

| Form | Meaning |
|------|---------|
| `meta <type> <name> = <expr>` | Compile-time constant value |
| `meta type <name> = <type-expr>` | Compile-time type alias / conditional type |
| `meta type <name>(<params>): ...` | Meta function returning a type |
| `meta assert <expr>, <msg>` | Compile-time assertion |
| `meta if <expr>: ... elif ... else ...` | Conditional compilation |

Five parser productions. One keyword.

## Types as meta values — design rationale

### Why NOT remove square brackets

It's tempting to unify type arguments and value arguments into one set of parens:

```
# Hypothetical — types in parens alongside values
Vector(int) items = Vector(int)()      # two sets of parens — confusing
Pair(int, str, 10, "hello")            # where do types end and values begin?
max(int, 3, 5)                         # is int a type or a variable?
```

This creates ambiguity. Parens already mean "constructor/function arguments." Square brackets
serve a real purpose: they visually separate type arguments from value arguments. Every language
that keeps this distinction (Scala `[]`, Kotlin/C++/TypeScript `<>`, Gorget `[]`) does so
because it's more readable, not less.

**Decision: keep `[]` for type application.** The `meta type` system complements generics —
it doesn't replace them.

### What meta types actually provide

The power isn't in the application syntax — it's in the ability to **name, compute, and choose
between types** at compile time:

- **Type aliases** — name complex types for readability
- **Conditional types** — swap implementations based on build flags or platform
- **Computed types** — derive types from compile-time values (e.g., `sized_int(32)`)
- **Type variables** — configure a module's types from one place

### Diamond inference — reducing redundancy

The remaining verbosity in generic code is the repetition between type annotation and
constructor:

```
Vector[int] items = Vector[int]()           # Vector[int] written TWICE
Dict[str, int] counts = Dict[str, int]()    # Dict[str, int] written TWICE
```

Two existing mechanisms already solve this:

**`auto` inference (RHS → LHS):**
```
auto items = Vector[int]()
auto counts = Dict[str, int]()
auto p = Pair[int, str](10, "hello")
```

**Diamond inference (LHS → RHS) — proposed:**
```
Vector[int] items = Vector()           # compiler infers [int] from declared type
Dict[str, int] counts = Dict()         # compiler infers [str, int]
Pair[int, str] p = Pair(10, "hello")   # compiler infers [int, str]
```

Combined with meta type aliases, declarations become concise without any syntax changes:

```
meta type Vec = Vector[int]
meta type Map = Dict if feature("ordered") else HashMap

Vec items = Vec()                          # clean — no redundancy
Map[str, int] counts = Map()               # diamond infers [str, int]
auto p = Pair[int, str](10, "hello")       # auto infers the whole type
```

### What meta types do NOT provide

- **Types are NOT runtime values** — you cannot store a type in a variable at runtime, pass
  it to a runtime function, or branch on it at runtime. Purely compile-time.
- **No higher-kinded types** — `meta type Container = Vector` (without type args) is deferred.
  It's powerful but opens a can of worms around kind-checking.
- **Does not replace generics** — `struct Pair[A, B]` still works the same way. Meta types
  are about naming/computing concrete types, not defining parameterized ones.

## Built-in meta functions

These are compiler-provided functions that are always available in `meta` contexts. They use
regular function call syntax — no special sigils.

| Function | Return type | Description |
|----------|-------------|-------------|
| `platform()` | `str` | Target OS: `"macos"`, `"linux"`, `"windows"` |
| `arch()` | `str` | Target architecture: `"x86_64"`, `"aarch64"` |
| `arch_word_bits()` | `int` | Word size in bits: `32` or `64` |
| `feature(str)` | `bool` | Build-time feature flag (`gg build --feature X`) |
| `debug()` | `bool` | Shorthand for `feature("debug")` |
| `sizeof(Type)` | `int` | Size of a type in bytes |
| `alignof(Type)` | `int` | Alignment of a type in bytes |
| `typename(Type)` | `str` | String name of a type: `"int"`, `"Vector[int]"` |

These can also be called at runtime where it makes sense (`sizeof`, `alignof`), but
`platform()`, `arch()`, `feature()`, and `debug()` are only meaningful at compile time.

## What's NOT in the design (deferred)

- **No `meta:` blocks** — avoids scoping ambiguity (do declarations leak out?)
- **No `meta` on function declarations** — functions are implicitly meta-capable if pure
- **No compile-time reflection** (`fields(T)`, `methods(T)`) — deferred to future design
- **No `@` sigil for builtins** — regular function syntax
- **No compile-time code generation** — deferred to future design
- **No higher-kinded types** — `meta type Container = Vector` deferred

## Interaction with allocator design

`meta` and explicit allocators complement each other:

```
meta int POOL_SIZE = next_pow2(MAX_ENTITIES * 128)
Arena pool = Arena(POOL_SIZE)

meta if feature("track_allocs"):
    TrackingAllocator tracked = TrackingAllocator(pool)
    with tracked:
        run_game()
else:
    with pool:
        run_game()
```

Compile-time computation of allocator sizes and conditional allocation tracking.

## Compiler implementation

### Pipeline

```
.gg source → lexer → parser → semantic analysis → META EVALUATION → C codegen → cc → binary
                                                       ↑
                                             (new pass: evaluate meta exprs,
                                              resolve meta types, prune
                                              meta-if branches, replace
                                              meta vars with literal values)
```

The C output never sees `meta`. A `meta int X = 42` becomes `#define X 42` or
`static const int X = 42`. A `meta type Vec = Vector[int]` becomes nothing — all uses of
`Vec` are replaced with `Vector[int]` before codegen. A `meta if` becomes just the taken
branch.

### Parser changes

One new keyword (`meta`) recognized by the lexer. Five new statement forms in the parser:

1. `meta <type-first-decl>` → `MetaConst` AST node
2. `meta type <name> = <type-expr>` → `MetaType` AST node
3. `meta type <name>(<params>): <block>` → `MetaTypeFunc` AST node
4. `meta assert <expr> [, <string>]` → `MetaAssert` AST node
5. `meta if <expr>: <block> [elif <expr>: <block>]* [else: <block>]` → `MetaIf` AST node

### Meta evaluation pass

A new compiler pass between semantic analysis and codegen:

1. **Evaluate `meta` constants** in dependency order. Build a symbol table of meta values.
2. **Resolve `meta type`** aliases and conditional types. Build a type symbol table.
3. **Evaluate `meta type` functions** when called. Interpret the body, return the resulting type.
4. **Replace all `meta type` references** in the AST with their resolved concrete types.
5. **Evaluate `meta assert`** conditions. Emit compile error on failure.
6. **Evaluate `meta if`** conditions. Replace `MetaIf` nodes with the taken branch's contents.
   Discard dead branches entirely.
7. **For function calls in meta context:** interpret the function body with the meta symbol
   table. Only pure operations allowed (arithmetic, string ops, comparisons, control flow).
   Reject I/O, allocation, extern calls.

### Compile-time evaluable operations

The meta evaluator supports a subset of Gorget:

- Integer arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Boolean: `and`, `or`, `not`
- String concatenation: `+`
- String methods: `.len()`, `.strip()`, `.upper()`, `.lower()`
- Control flow: `if/elif/else`, `while`, `for` (over meta-known ranges)
- Local variables within meta functions
- Calls to other pure functions
- Type expressions (in `meta type` contexts): concrete types, parameterized types, conditionals

NOT supported in meta context:
- Heap allocation (`Vector`, `String` construction, `Dict`, etc.)
- I/O (`print`, `File`, network, etc.)
- Extern function calls
- Mutable global state

## Implementation phases

| Phase | What | Effort |
|-------|------|--------|
| **M0** | Lexer: add `meta` keyword. Parser: `MetaConst`, `MetaAssert`, `MetaIf`, `MetaType`, `MetaTypeFunc` AST nodes. | Small |
| **M1** | Meta evaluation pass: evaluate constants (literals + arithmetic only). Replace in AST. Emit `#define` in codegen. | Medium |
| **M2** | Meta type aliases: resolve `meta type X = ConcreteType`, substitute in AST before codegen. | Medium |
| **M3** | Meta assertions: evaluate boolean exprs, emit compile errors. | Small |
| **M4** | Meta if: evaluate conditions, prune dead branches before codegen. | Medium |
| **M5** | Conditional meta types: `meta type X = A if cond else B`. Evaluate condition, resolve type. | Small | ✅ **DONE** |
| **M6** | Meta type functions: `meta type foo(params): ...`. Interpret body, return type. | Large | ✅ **DONE** |
| **M7** | Meta function evaluation: interpret pure function bodies at compile time for value computation. | Large |
| **M8** | Built-in meta functions: `platform()`, `arch()`, `feature()`, `debug()`, `sizeof()`, `alignof()`, `typename()`. Wire `--feature` flag to CLI. | Medium |
| **M9** | Diamond inference: infer RHS type args from LHS declared type (independent of meta, but synergistic). | Medium |

M0–M4 deliver the most practical value (constants, type aliases, assertions, conditional
compilation). M5–M6 add computed types. M7 is the big lift (embedded interpreter). M8
provides built-in introspection. M9 reduces generic verbosity orthogonally.
