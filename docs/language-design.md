# Gorget Language Design

## Vision

**Gorget** = Rust's memory safety + Python's indentation + C/Java's type declarations

A systems-capable language that reads like pseudocode but compiles to safe, efficient machine code. The goal: make ownership and borrowing *feel natural* rather than fighting the programmer.

---

## Design Direction

Long-term objectives grouped by pillar. These targets and anti-targets guide every design decision — from syntax choices to runtime semantics. Each row captures both what Gorget aims for and what it deliberately avoids.

### Memory Safety

| Design Targets | Explicitly Avoid |
|---|---|
| Ownership enforced at compile time — no runtime GC pauses | Garbage collector (incompatible with real-time and bare-metal targets) |
| Move semantics with an explicit operator (`!`) — transfers visible at call sites | Hidden heap allocation with no way to control or redirect it |
| Borrow checking without lifetime annotations in API signatures | Rust-style lifetime annotations leaking into public APIs |
| Scope-guarded references with generation tokens as a safety fallback | Null pointers as a default value for any type |
| Scoped allocator control — `with Arena() as pool:` redirects all allocations in a block; `alloc=` on constructors for one-shot control; copy-on-write aliasing with lazy materialization (the copy happens at the mutation that demands it, never speculatively) + last-use move analysis eliminates unnecessary copies | Use-after-free and double-free reachable from safe code |
| Stale-condition warnings when shared data crosses suspension points | Silent memory corruption from undefined behavior |
| Distinct newtype / semantic types (UserId ≠ int at compile time) | C-style pointer arithmetic accessible without explicit opt-in |

### Performance

| Design Targets | Explicitly Avoid |
|---|---|
| Safety is the constraint, speed is the objective function — every optimization must be a semantic REFINEMENT, with eager-copy correctness as the always-available fallback | Buying performance by weakening an invariant — no "fast mode" that reintroduces memory unsafety or UB |
| Value semantics at hand-optimal cost — CoW + liveness + lazy materialization make the compiler place the minimal clone set, as if the user had written every copy by hand | Making the user pay for speed with annotations, visible lifetimes, or unsafe escape hatches on the default path |
| Runtime safety backstops (bounds checks, cap-driven frees) removed only by per-site compiler PROOF of unreachability | Globally disabling safety checks as an optimization flag |
| Performance claims measured as EXECUTED behavior (operation counts, peak RSS, wall-clock) on real programs | Static estimates or proxy counters standing in for measured results |
| Every aliasing/copy-elision optimization ships WITH its executable guard and behavioral oracle (dual-compiler output diffing, fixture batteries, enumeration lints) | Optimizations whose safety argument lives only in prose or in someone's memory |
| Zero-cost where provable, cheap where not, never unsafe | Undefined behavior as a performance technique |

### Type System

| Design Targets | Explicitly Avoid |
|---|---|
| Static nominal typing — no accidental structural matches | Type erasure (Java generics — runtime ClassCastException from compile-time types) |
| Full generics with monomorphization — zero-cost at runtime | Implicit numeric coercions (C's int promotions cause subtle bugs) |
| Trait-based composition (`equip...with`) over inheritance hierarchies | Implicit interface satisfaction — equipping should be deliberate |
| Sum types (enums with payloads) + exhaustive pattern matching | Structural typing for everything — leads to accidental duck-type matches |
| Local type inference — annotate at boundaries, infer inside | Orphan rule violations — trait coherence must be enforced |
| Distinct / newtype types to prevent semantic misuse at compile time | `typeof null === 'object'`-style lies — the type system must be honest |
| Integer overflow as an explicit compile-time choice, never silent UB | Dynamic typing as the default path for any core operation |

### Error Handling

| Design Targets | Explicitly Avoid |
|---|---|
| Errors as typed values — `Result[T, E]` and `Option[T]` as first-class builtins | Exceptions and stack unwinding — hidden control flow from callers |
| Exhaustive pattern matching on error variants — unhandled cases are compile errors | Silent failure (returning false/null/0 with no type-level signal) |
| Auto-propagation + `rethrow` escalation — happy path is straight-line code; add context only where needed | Exception hierarchies (Java checked/unchecked split created more ceremony than safety) |
| `on error` cleanup — run resource release only on the error path | Panic as default for recoverable errors (missing map key should never crash) |
| Clear semantic split: panics for invariant violations, errors for expected failures | Swallowing errors with `_` or equivalent without explicit acknowledgment |
| Assertion-first design — asserts active by default, strippable for release | Global error state (errno-style) — non-reentrant and invisible to the type system |
| Error set inference — compiler tracks possible errors without manual listing | Stringly-typed errors (`throw "something went wrong"`) |

### Concurrency

| Design Targets | Explicitly Avoid |
|---|---|
| async/await with stackless coroutine codegen — no hidden stack frames | GIL / Global Interpreter Lock — must be genuinely parallel |
| M:N work-stealing executor as the default scheduler | One OS thread per task — does not scale to 10,000 concurrent connections |
| Typed channels for message passing (CSP-style) | Implicit sharing — any field accessible from any thread without annotation |
| Shared data primitives (`Mutex[T]`, `RwLock[T]`) where the lock wraps the data | Goroutine/task leaks — every spawned task must have a clear parent scope |
| Structured concurrency — tasks live within scopes, cancelled on exit | Callback hell / nested promise chains — async/await exists to flatten these |
| Compiler-enforced token semantics at spawn boundaries | `async` as a viral modifier that infects the entire call chain |
| Stale-condition warnings for shared data across await points | Data races reachable from safe code |
| Pluggable scheduler backends (pool, thread, inline, single) | Lock-free primitives exposed without safety guarantees |

### Syntax & Expressiveness

| Design Targets | Explicitly Avoid |
|---|---|
| Python-level readability — hello world in 2 lines, no boilerplate ceremony | C-style preprocessor macros — text substitution with no type awareness |
| f-string interpolation — opt-in, type-safe, calls `.to_string()` | Significant whitespace as the *sole* block mechanism (the meta system provides structured alternatives for codegen) |
| Exhaustive pattern matching with destructuring and guards | Semicolons as mandatory noise — infer or make optional |
| Comptime / meta system — compile-time computation that replaces macros and codegen scripts | Operator overloading without convention — `<<` meaning stream insertion (C++) |
| Pipe operator (`\|>`) — left-to-right composition, no inside-out nesting | Implicit falsy coercions (`0 == false`, `"" == false`) |
| `for/else` and `while/else` — loop-completion semantics without extra flags | Verbosity for its own sake — names earn their length |
| Curated trait-exposed dual spellings — `len(x)`/`x.len()` via `Measurable`, `map`/`filter` as free fn and method. NOT general UFCS: a universal free-fn↔method equivalence would let any `&`-taking free function mutate through method syntax with no `&` at the call site (violating the mutation-acknowledgment rule, §4.5's one sanctioned exception) and would add a second spelling for every call (see "Multiple incompatible ways" opposite) — abandoned as a target 2026-07-06 (decision ledger D16) | Magic / spooky action at a distance — behavior visible at the call site or nowhere |
| | Multiple incompatible ways to do the same common task |

### Tooling

| Design Targets | Explicitly Avoid |
|---|---|
| One official formatter (`gg fmt`) — one style, no debates, enforced by the toolchain | Multiple competing build systems (the C++ CMake/Bazel/Meson civil war) |
| One build system (`gg build`) — programmatic, in the language itself, not a DSL | External dependencies for formatting — if it's not in the toolchain, it won't be used |
| One package manager (`gg add`) — lock files, semver, offline cache, reproducible builds | Slow compilation — compile speed is a UX feature, not an implementation detail |
| LSP server — go-to-definition, rename, inline errors, auto-complete | Dependency on C headers for C interop — import a `.h` and get typed bindings |
| `gg run` — compile + run in one step, feels like a scripting language | Breaking ecosystem changes post-1.0 — plan the stability story before you have users |
| `gg test` — built-in test runner, no framework choice needed | Tooling that only works on one OS — cross-platform from day one |
| `gg doc` — doc comments generate browsable HTML, always in sync | IDE lock-in — standard tooling should work with any editor via LSP |
| `gg check` — semantic analysis only, fast feedback loop for editors | Cryptic build output — errors should point to source, not generated C |
| Excellent, actionable error messages (Rust quality — explain, locate, suggest) | Silent linting failures — warnings should be loud or not exist |

### C Interop & FFI

| Design Targets | Explicitly Avoid |
|---|---|
| C as the compilation target — portable, zero new backend dependency | JVM / managed runtime requirement — kills the bare-metal story |
| C ABI compatibility — generated symbols follow C calling conventions | Hidden runtime dependencies — hello world should not link unused runtime sections |
| Explicit `unsafe` blocks — auditable regions where safety guarantees are suspended | Name mangling without an escape hatch — FFI must produce predictable C names |
| | Incompatible calling conventions without explicit annotation |
| WASM target — browser and edge compute deployment path | Requiring manual translation layers to call existing C libraries |
| Embedded / no-stdlib mode — bare runtime, no OS primitives required | Platform-specific ABI surprises — stdcall/cdecl confusion |

### Standard Library

| Design Targets | Explicitly Avoid |
|---|---|
| Collections (generic Vector, Dict, Set, Heap) with full ownership semantics | 30 competing ways to do the same thing — one canonical form per common task |
| String handling with explicit byte/codepoint distinction (no silent encoding bugs) | Inconsistent naming conventions — stdlib API should follow one rule |
| HTTP client + server in stdlib — not an external package | Requiring a package manager for basics (JSON, HTTP, math) |
| JSON, CSV, XML out of the box | Mutable global state in stdlib functions (errno-style non-reentrancy) |
| SQLite with zero external dependencies | Stdlib functions that silently return wrong results on edge cases |
| DateTime with explicit timezone handling | Deprecated functions that cannot be removed due to backwards-compat promises |
| Cryptographic primitives (hash, random) — not a third-party concern | Leaking C implementation details through the stdlib API surface |
| Six composable allocators in stdlib (Arena, Pool, TLSF, Tracking, FixedBuffer, Fallback) with `with`/`alloc=` integration and compile-time escape analysis | Stdlib that only works with the default allocator |

### Learnability & Ergonomics

| Design Targets | Explicitly Avoid |
|---|---|
| Python-familiar entry point — target audience can be productive on day one | 90-page spec required to write hello world |
| One obvious way to do common things (Zen of Python principle) | Footguns on the default path — the easy way and the safe way must be the same way |
| Progressive disclosure — simple code is simple, low-level control is opt-in | Magic / implicit behavior not visible at the call site |
| No undefined behavior in safe code — every operation has defined semantics | Version fragmentation at 1.0 — editions, compat shims, mixed patterns |
| Fast feedback loop — `gg run` should feel instant | Slow first-compile experience that discourages exploration |
| Consistent naming conventions enforced by the formatter | Multiple valid styles for the same construct causing endless style debates |
| Compiler errors that explain the problem, show the cause, suggest the fix | Generic error messages that point to the wrong file or line |
| Self-hosting as a dogfood signal — the language should handle compiler-scale programs | Relying on a different language to bootstrap the compiler indefinitely |

---

## 1. Core Principles

1. **Safe by default** - no null, no data races, no use-after-free
2. **Readable first** - code should look clean; minimize sigils and noise
3. **Explicit types at boundaries** - function signatures are fully typed; locals can be inferred
4. **Mutable by default, const opt-in** - local variables are mutable; use `const` for immutability. Function arguments are the opposite: an immutable (read-only) borrow by default, `&` for **write-through** to the caller. A bare argument never mutates the caller's data — a write through it *copies-on-write* into a private copy (materializes) rather than failing; `&` is how a change reaches the caller. See §3.2.
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

#### Variable Initialization

Every variable declaration must include an explicit initializer. The form is always `Type name = value` (or `auto name = value`):

```gorget
int count = 0
float ratio = 1.0
bool ready = false
String name = ""
```

There is no uninitialized-variable form: a bare `int x` (no `= …`) is a compile error, not an implicitly zero-valued variable. This removes the class of bugs from uninitialized reads *and* the subtler class where a value is syntactically fine but semantically unset — a `0` or `""` that was never meant to be a real value. If zero or empty is the correct starting value, you write it. Consistent with the language's safety-by-default stance, the safe choice is the only choice: there is no flag to opt out.

(Struct fields are declared without initializers and supplied at construction — a separate construct from a local variable declaration.)

### 2.2 Primitive Types

```
int8   int16   int32   int64    (signed; `int` aliases int64)
uint8  uint16  uint32  uint64   (unsigned; `uint` aliases uint64; `byte` aliases uint8)
float32  float64                 (`float` aliases float64)
bool
```

#### Integer Overflow

Integer arithmetic **panics on overflow** in both debug and release builds by default. This catches bugs that silently corrupt data in C/Go.

```gorget
int8 x = 127
x += 1                    # panic: integer overflow (int8)
```

For intentional wrapping, use the per-operator wrapping operators:

```gorget
int8 x = 127
int8 z = x +% 1                # z == -128 (wrapping add)
int8 w = x *% 2                # wrapping multiply
```

Wrapping operators: `+%`, `-%`, `*%`. These mirror Zig's approach. No wrapping division (division by zero panics separately).

Plain `+`, `-`, `*` always check overflow; the per-operator `+%`/`-%`/`*%` forms are the only way to opt into wrapping. There is no global "wrap the whole build" mode — wrapping is per-expression, by design.

The overflow panic is also **locally recoverable**. Wrapping an arithmetic expression in `(...) catch Fault.Overflow:` branches an overflowing op to a fallback instead of panicking:

```gorget
int big = 9223372036854775807
int r = (big * 2) catch Fault.Overflow: -1     # r == -1 (overflow caught)
int ok = (3 * 4) catch Fault.Overflow: -1      # ok == 12 (no fault)
```

Recovery is **local and lexical** — the catch covers only the faultable ops emitted directly into the wrapped expression, and an uncaught overflow still panics exactly as above. See §6 for the full `Fault` model.

### 2.3 Functions

C/Java-style: return type before name, typed parameters.

```gorget
int add(int a, int b):
    return a + b

void greet(String name):
    print(f"Hello, {name}")

# Expression body shorthand for simple functions
int double(int x): x * 2
```

`void` means no return value. `String` is Gorget's unified string type — a 32-byte value with copy-on-write semantics. Literals and slicing operations are zero-allocation views; the compiler auto-materializes when the source is mutated.

#### Multiple Return Values

Functions can return tuples, which are destructured at the call site:

```gorget
# Return a tuple
(int, int) divmod(int a, int b):
    return (a / b, a % b)

# Destructuring assignment at call site
auto (quotient, remainder) = divmod(17, 5)

# With explicit types
(int, int) (q, r) = divmod(17, 5)

# Ignore values with _
auto (_, remainder) = divmod(17, 5)

# Works with any tuple size
(String, int, bool) parse_header(String line):
    return (name, value, is_required)

auto (name, value, _) = parse_header("Content-Type: text/html")
```

This is Go-style multiple returns with Python-style unpacking. Under the hood, it's just tuples + destructuring — no special multi-return mechanism needed.

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

**The one rule underneath all three — full lazy copy-on-write.** A
resource value is a cheap **borrow** until something tries to **modify** it. At that moment the
compiler asks whether the write can reach a real owner through an
unbroken chain of `&` (mutable) access. If it can, the write lands on the
owner (**write-through**). The instant that chain instead hits an
immutable binding — a bare local, a bare parameter, a bare alias, a
`for x in coll` element — the value **materializes** (copies-on-write)
*there* and the write lands on the private copy, leaving everything
upstream untouched. The copy is **fully lazy** — deferred to the mutation that
demands it, never speculative; a mutation that never runs never allocates
(§9.6). So bare (the default) is *read-only-and-copy-on-write*,
`&` is *write-through*, `!` is *move*. This is deliberately **more tolerant
than Rust**, which rejects a mutation through an immutable borrow; Gorget
copies instead. Everything below — parameters (§3.2), assignments
(§3.3–3.4), fields (§3.4), collection elements, loops — is this single
rule applied at each position.

**The cost side — when to reach for `&`.** Because a bare mutation
*copies*, mutating through a bare binding can pay a clone (the
copy-on-write), whereas an `&` mutation writes **in place** with no copy.
So `&` does two jobs at once: it makes the change **reach the caller**,
and it **avoids the clone**. Reach for it on the values you actually mean
to modify — especially large collections on hot paths; leave a parameter
bare when you only read it, or when a cheap private-copy mutation is
genuinely what you want. In short: **bare = safe default, copies if you
write; `&` = write-through, no copy.**

### 3.2 Type Categories: Resource vs Trivial

Every type in Gorget falls into one of two categories based on whether it owns a resource:

| Category | Meaning | Examples | Parameter passing |
|----------|---------|----------|-------------------|
| **Trivial** | Pure data — can be freely copied | `int`, `float`, `bool`, `String` (view provenance), `Point { float x, float y }` | By value (copy) |
| **Resource** | Owns a heap allocation, file handle, or lock — cannot be implicitly copied | `String` (owned provenance), `Vector[T]`, `Dict[K,V]`, `Guard[T]` | By pointer (const by default) |

The distinction is about **what the type owns**, not how large it is. A `Point` with two floats is Trivial because it's just data. A `Vector[int]` is Resource because it owns a heap-allocated buffer that must be freed exactly once.

**Resource types and parameter passing:**

| Declaration | Pointer kind | A mutation through the param… | Callee drops? |
|-------------|-------------|-------------------------------|---------------|
| `Vector[int] v` | `const T*` (read-only) | materializes a private copy (copy-on-write); the caller is untouched | No |
| `Vector[int] &v` | `T*` (mutable) | writes through to the caller's value | No |
| `Vector[int] !v` | `T*` (mutable) | writes through; the callee owns and drops it | Yes |

A bare Resource param is a **read-only borrow** of the caller's data: the callee reads it freely but cannot change what the caller sees through it — the incoming pointer is `const`. A mutation *attempt* is **not** a compile error, though: it **materializes** a private copy (copy-on-write) and the write lands on that copy, leaving the caller untouched. (This is where Gorget is **more tolerant than Rust** — Rust rejects a mutation through an immutable borrow outright; Gorget copies instead.) So `void sneaky(Vector[Res] v): v[0].name = "x"` compiles and runs — it just mutates `sneaky`'s own private copy, never the caller's vector. The `&` sigil is the opt-in for **write-through** (a change made through an `&` param reaches the caller's value), and `!` transfers ownership. **Resource types are never copied by value (memcpy) at the boundary** — the only copy that ever happens is the copy-on-write clone at a mutation. A freshly owned resource value comes from construction, `.clone()`, `!move`, or that implicit copy-on-write clone.

**Storing borrowed parameters:**

Storing a borrowed parameter (bare or `&`) into a structure that escapes the callee's frame **clones it at the boundary** — the stored value is an independent copy, never a shallow alias of the caller's heap allocation. When the source is dead at that point (or you write `!`), the value is **moved** instead of cloned:

```gorget
struct Wrapper:
    Vector[int] data

# OK: a live borrowed parameter is CLONED into the field at the boundary
Wrapper clone_in(Vector[int] v):
    return Wrapper(v)        # v is still live → cloned into Wrapper.data

# OK: move transfers ownership with no clone (v is dead afterwards)
Wrapper move_in(Vector[int] !v):
    return Wrapper(!v)

# OK: read-only access is fine
int first(Vector[int] v):
    return v.get(0).unwrap()
```

Either way, resource-type values still end up with exactly one owner — the clone gives the new owner its own copy, the move hands over the original. Write `!` when you want to force the transfer (no clone) and let the original go dead.

**Trivial types** pass by value regardless of sigils (except `&` on primitives, which creates a mutable pointer for out-params).

**Collection element access returns references:**

Indexing a collection (`v[i]`, `dict[key]`) returns a reference to the element in-place, not a copy. This is the same mechanism as borrowed parameters — the value stays as a pointer internally, and methods/field access resolve through it at zero cost:

```gorget
Vector[Vector[int]] matrix = [[1, 2], [3, 4]]

# auto → reference (Ptr), zero cost — no clone
auto row = matrix[0]
print(row.len())         # reads through pointer

# Method call directly on index — no intermediate copy
print(matrix[1].len())

# for-loop over referenced element — iterates through pointer
for x in matrix[0]:
    print(x)
```

**Bare-assign borrows — `auto` and explicit type are identical:**

Reading an indexed element (`matrix[0]`) returns a borrow (`Ptr(T)`). Binding it with a bare assignment borrows whether you write `auto` or an explicit type — the type annotation is only a check on the RHS, not a clone signal. The borrow CoW-severs on the first mutation; for an independent owned copy up front, call `.clone()`. A clone is inserted only at a genuine ownership boundary (return, struct/enum field init, collection store, closure capture, a move of a borrow):

```gorget
# auto → borrow, zero cost
auto row_ref = matrix[0]

# Explicit type → also a borrow (NOT an auto-clone); CoW-severs on mutation
Vector[int] row = matrix[0]
row.push(5)               # mutation severs the alias — row is cloned here
print(matrix[0].len())    # original unchanged — 2, not 3

# .clone() for an independent owned copy up front
Vector[int] row_copy = matrix[0].clone()

# Move (!) of a reference → auto-clone for ownership transfer
consume(!row_ref)

# Return from function with T return type → auto-clone (ownership boundary)
Vector[int] get_first(Vector[Vector[int]] m):
    return m[0]            # auto-clones for owned return
```

Both `auto` and an explicit type give the fast path (borrow); `.clone()` gives an owned copy. The `!` sigil gives a zero-cost move. Clones happen only at a mutation through an alias or at an ownership boundary — never silently on a typed read.

### 3.3 Ownership (Copy-on-Write)

For Resource types (String, Vector, etc.), bare-identifier assignment **borrows** by
default — it creates a second alias (a Ptr) to the same data at zero cost. The aliases
share storage until one of them mutates, at which point the compiler clones so each
side owns an independent copy. This is copy-on-write, and it is fully lazy: the copy
is placed at the mutation itself, so a mutating path that never runs never pays for
one. The source stays valid:
```gorget
String s1 = "hello"
String s2 = s1           # borrow — both names valid, no allocation
print(s1)                # "hello"
s2 = s2 + "!"            # mutation severs the alias — s1 is unaffected
print(s1)                # "hello"
print(s2)                # "hello!"
```

The `!` operator is the explicit move opt-in. It transfers ownership and invalidates
the source:
```gorget
String s3 = "world"
String s4 = !s3          # explicit move, s3 is invalid
# print(s3)              # COMPILE ERROR: use after move
```

A few single-owner-by-design types still **require** `!` (or `.clone()`) on bare-assign,
because aliasing them is unsafe: `Box[T]`, `Task`, `TaskGroup`, `Guard`, `Owned[T]`,
`Callable[...]` and closure values. For these, `Box[int] b = a` is a compile error
(`E_MoveWithoutOperator`) — write `Box[int] b = !a`.

Trivial types (int, float, bool) are always copied automatically:
```gorget
int a = 5
int b = a                # just copies, both valid (no ! needed)
```

### 3.4 Assignment Semantics

For resource types, there are no implicit deep copies. Bare-identifier assignment
**borrows** (copy-on-write); `!` **moves**; `.clone()` is an explicit deep copy:

| Assignment | Meaning | Cost |
|-----------|---------|------|
| `Vector[int] b = a` | Borrow — b aliases a (Ptr), clones on first mutation | Zero cost |
| `auto b = a` | Borrow — identical to the explicit-type form above | Zero cost |
| `Vector[int] b = !a` | Move — b takes ownership, a consumed | Zero cost |
| `Vector[int] b = a.clone()` | Clone — b is an independent deep copy up front | Heap allocation |
| `Vector[int] b = f()` | Move from temp — b owns the result | Zero cost |
| `a = &b` | Mutable reference — a aliases b | Zero cost |

```gorget
Vector[int] a = [1, 2, 3]

Vector[int] b = a          # borrow — b aliases a, zero cost
print(b.len())             # 3 — read through the alias
b.push(4)                  # mutation severs the alias: b is cloned
print(a.len())             # 3 (unchanged)
print(b.len())             # 4

Vector[int] c = a.clone()  # explicit clone — independent up front
Vector[int] d = !a         # move — a is consumed, d owns the data
# print(a.len())           # COMPILE ERROR: a was moved

Vector[int] e = make_vec() # move from temp — zero cost
```

Trivial types (int, float, bool, simple structs) are copied by value — no heap allocation, no `.clone()` needed.

**Key rule:** Resource types are never *deep-copied* implicitly. Bare-assign aliases for free and the compiler inserts a clone only at the first mutation through an alias; `!` moves, `.clone()` copies up front. Every heap allocation is still visible — it happens at a mutation, a `.clone()`, or an ownership boundary (collection put, struct/enum field init, return), never silently on read.

**`auto` vs explicit type:** for bare-assign they behave identically — both borrow with copy-on-write. The only difference is inference: `auto` lets the compiler pick the type; an explicit type is a check that the RHS matches. For ephemeral sources (function-call results), the bound value is already owned-by-construction, so the bind is a move, not a borrow.

### 3.4.1 The `Cloneable` Trait

Types that support deep copying implement the `Cloneable` trait:

```gorget
trait Cloneable:
    Self clone(self)
```

Use `@derive(Cloneable)` to auto-generate field-by-field clone for structs:

```gorget
@derive(Cloneable)
struct Player:
    String name
    Vector[int] scores

Player p1 = Player("Alice", [90, 85, 95])
Player p2 = p1.clone()    # deep copy — p2.name and p2.scores are independent
```

Built-in collection types (`Vector`, `Dict`, `Set`, `String`) implement `Cloneable` natively. User structs with resource fields must derive or implement it explicitly. Trivial types don't need `Cloneable` — they're `Copy` by default.

#### Struct Field Access

Reading a struct field **borrows** — it does not copy or move:

```gorget
struct Player:
    String name
    Vector[int] scores

Player p = Player("Alice", [90, 85, 95])

# Field reads return non-owning references (zero cost)
print(p.name)              # Str view into p's owned string
print(p.scores.len())      # reads through Ptr to p's vector
```

Structs **own** their fields. When a struct is dropped, all its resource-type fields are freed. But reading a field produces a lightweight reference — not a deep copy:

| Field type | Read returns | Cost | Ownership |
|-----------|-------------|------|-----------|
| `String` | `String` (view) | Zero — copies 32-byte header | Struct still owns the data |
| `Vector[T]` | `Ptr(Vector[T])` | Zero — pointer to field | Struct still owns the data |
| `int`, `bool` | Value copy | Zero — trivial | Independent copy |

The struct retains ownership. The view/reference borrows from the struct and is valid as long as the struct is alive.

**Bare-assign borrows on read** — same rule as collection element access (and as §3.4): both `auto` and an explicit type produce a borrow that CoW-severs on mutation. The explicit type is only a check on the RHS, not a clone signal. For an independent owned copy up front, call `.clone()`:

```gorget
auto fast = p.name         # String view (zero cost, borrows from p)
String view = p.name       # ALSO a borrow — CoW-severs on mutation, not an auto-clone

auto ref = p.scores        # Ptr reference (zero cost)
Vector[int] row = p.scores # ALSO a borrow — CoW-severs on mutation

String owned = p.name.clone()      # owned copy up front (explicit)
Vector[int] copy = p.scores.clone() # owned copy up front (explicit)
```

Both `auto` and an explicit type give the fast path (borrow). The clone is deferred to a later mutation through the alias, or inserted at an ownership boundary; an explicit `.clone()` makes it eager. The compiler never inserts a clone silently on a typed read.

### 3.5 The Borrow Rules (same as Rust)

At any given time, for a given piece of data, you can have **either**:
- Any number of immutable borrows (`String s`), OR
- Exactly one mutable borrow (`String &s`)

Never both simultaneously. Enforced at compile time. This prevents data races and aliasing bugs.

### 3.6 Borrow Origin Tracking

**Gorget requires zero lifetime annotations.** The compiler's borrow checker internally tracks the *origin* of every borrowed value — which parameter, local, or field a reference derives from — to catch use-after-move and dangling-return errors at compile time. This analysis is fully automatic; programmers never annotate lifetimes.

This is possible because Gorget's ownership model draws a hard line at function boundaries: **resource types (`String`, `Vector[T]`, structs with resource fields, etc.) always transfer ownership when returned or stored.** There is no user-visible borrowed-view type that can escape a function. Borrowed parameters (bare and `&`) are only valid within the callee's frame, and a function's return value is always an independent owned value. Within a function body, bare-identifier assignment of resource-typed locals (`Spanned b = a`) follows copy-on-write — see §3.4 — but the cross-function-boundary contract is unchanged. This structural guarantee eliminates the class of bugs that Rust's lifetime annotations exist to prevent.

```gorget
# The compiler tracks that x and y are borrowed parameters.
# Returning one is safe because it transfers ownership (Move).
String longer(String x, String y):
    if x.len() > y.len():
        return x
    return y

# Trait methods need no annotation either — the compiler knows
# the return value is a freshly constructed or moved value.
trait Container:
    String get(Container self, int index)

# Mutable borrow — the compiler tracks that data is mutated
# and that first() returns a value derived from data.
String process(String &data):
    data.sort()
    return data.first()
```

**What the compiler checks automatically:**
- **Use-after-move** — accessing a variable after it has been moved (`!`) is a compile error
- **Dangling returns** — returning a reference to a local variable is rejected
- **Borrow/move conflicts** — using a borrowed reference after the source has been moved
- **Transitive tracking** — when the return calls another function, the compiler uses the callee's already-computed origin metadata
- **Local aliases** — assignments from parameters to locals are traced through

---

### 3.7 Comparison with Rust Lifetimes

Gorget provides the same memory safety guarantees as Rust but requires **zero lifetime annotations**. This section is aimed at Rust-experienced users.

| Aspect | Rust | Gorget |
|--------|------|--------|
| Lifetime annotations | Required on signatures (`'a`, `'b`) | None — fully inferred |
| Inference source | Signature-only elision rules | Ownership model + body analysis |
| Borrowed return values | Allowed — annotated with `'a` | Not applicable — Move types transfer ownership on return |
| Use-after-move | Checked | Checked |
| Dangling references | Prevented by lifetime bounds | Prevented structurally — borrows cannot escape their scope |
| User-facing syntax | `'a`, `'b`, `'static`, `where 'a: 'b` | No lifetime syntax — the compiler handles everything internally |

**Why no annotations are needed:** Rust needs lifetime annotations because it allows functions to return borrowed references — the caller must know *which* input the return value borrows from. Gorget sidesteps this entirely: Move types always transfer ownership when returned. There is no user-visible borrowed-view type that can escape a function boundary. The compiler's internal origin tracking catches safety violations without exposing any of this machinery to the programmer.

```gorget
# Gorget: no annotation needed — return transfers ownership
String longer(String x, String y):
    if x.len() > y.len():
        return x
    return y
```

```rust
// Rust: must annotate — the caller needs to know the return borrows from both inputs
fn longer<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

**Trade-off:** Gorget's annotation-free approach means there is no explicit lifetime contract in the signature. Rust's annotations serve as documentation and a stability guarantee — changing which input a return borrows from requires a signature change. In Gorget, the ownership model makes this moot: returned values are always owned, so there is no borrowing relationship to document.

---

### 3.8 Purity Inference

The compiler automatically infers whether a function has side effects — with **zero annotations**. This is computed during borrow checking (Pass 5) alongside lifetime and ownership analysis, since the same whole-body walk that tracks borrows can also observe reads, writes, and calls.

#### Purity Levels

Every function is assigned one of four purity levels, from purest to most effectful:

| Level | Meaning | Example |
|-------|---------|---------|
| **Pure** | Reads only its arguments, calls only pure functions, no globals or IO | `int double(int x): x * 2` |
| **ReadOnly** | May read global variables but never mutates them, no IO | `int get_threshold(): return GLOBAL_MAX` |
| **MutatesArgs** | May mutate `&` or `!` parameters but no globals or IO | `void push(Vector[int] &v, int x): v.push(x)` |
| **HasSideEffects** | Anything else: IO, global mutation, shared variable access, extern calls | `void greet(String name): print(f"hello {name}")` |

Purity levels form a lattice ordered by increasing impurity. When a function calls another, its purity is the *join* (least-pure) of its local purity and all callee purities. This means purity propagates conservatively through the call graph.

#### How Inference Works

Purity inference runs in two phases:

1. **Local analysis** — walk each function's body and observe:
   - Does it read a global? → at least `ReadOnly`
   - Does it write a global? → `HasSideEffects`
   - Does it mutate a `&` parameter? → at least `MutatesArgs`
   - Does it call an extern or unknown function? → `HasSideEffects`
   - Does it access a `shared` variable? → `HasSideEffects`

2. **Call-graph propagation** — fixed-point iteration over the call graph:
   - For each function, join its local purity with the purity of every callee
   - Repeat until no purity levels change (converges because the lattice is finite and join is monotone)

**Conservative defaults:** Unknown callees (closures, trait objects, extern functions) are assumed `HasSideEffects`. This is always safe — purity is informational, never a constraint that blocks compilation.

#### Why Purity Matters

Purity inference enables several compiler optimizations and safety guarantees:

- **Compile-time evaluation** — pure functions can safely run during `meta` evaluation
- **Parallel safety** — pure functions on disjoint data can run in parallel without synchronization
- **Shared variable optimization** — pure function calls inside `with shared_var:` blocks don't need to release the synchronization token (no yield point needed)
- **Memoization** — the compiler can cache pure function results when inputs repeat
- **API stability** — purity changes are semantically breaking changes; the compiler can warn when a function's purity level regresses between versions

#### Design Philosophy

No other systems language infers function purity at compile time without annotations. Haskell tracks effects but requires the programmer to use monads. Rust has no purity tracking. D has `pure` as a keyword. Gorget's approach is fully automatic — write normal code and the compiler tells you what's pure.

The key insight is that borrow checking already walks every expression in every function body. Adding purity observation to this existing walk is almost free — it's a few extra flags per function, not a new pass.

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

**Variant namespacing:** User-defined enum variants are namespaced under their type and require qualified access: `Color.Red()`, `Color.Custom(255, 128, 0)`. This eliminates name collisions when two enums define variants with the same name (e.g., `Color.Red` and `Status.Red`) and aligns with Gorget's "explicit by default" philosophy.

Built-in `Option` and `Result` variants (`Ok`, `Error`, `Some`, `None`) are part of the prelude and always available bare — they are foundational types used pervasively.

```gorget
Color c = Color.Red()              # user enum — qualified required
Option[int] x = Some(42)           # prelude — bare OK
Result[int, String] r = Ok(42)     # prelude — bare OK

match c:
    case Color.Red():
        print("red")
    case Color.Custom(r, g, b):
        print(f"{r},{g},{b}")
```

**Dot-shorthand:** When the expected type is unambiguous from context (variable declaration, assignment, return, or function parameter), `.Variant()` desugars to `EnumType.Variant()`. This is Swift-style type inference for enum construction and matching:

```gorget
Color c = .Red()           # → Color.Red()
c = .Blue(42)              # → Color.Blue(42)
return .Green()            # → Color.Green(), return type known
match c:
    case .Red():           # → Color.Red, scrutinee type is Color
    case .Blue(n):         # → Color.Blue, scrutinee type is Color
```

Dot-shorthand eliminates the redundancy of repeating the type name when the compiler can infer it, while retaining the explicit-type discipline of Phase 1 qualified access. Both forms are always valid; dot-shorthand is purely syntactic sugar.

**Glob import:** When working extensively with one enum, use `EnumName.*` to bring all its variants into bare scope:

```gorget
from xtd.log import LogLevel.*

LogLevel lvl = Info()    # bare — from glob import
match lvl:
    case Info():  print("info")
    case Err():   print("err")
```

### 4.2.1 Option Sugar

`Option[T]` is Gorget's null replacement. Rich sugar makes it ergonomic:

```gorget
Option[String] name = Some("Alice")
Option[int] age = None

# Pattern matching with 'is'
if name is Some(n):
    print(f"Name: {n}")

# Optional chaining (?.)
# Returns None if any step is None, otherwise the final value
auto len = user?.name?.len()          # Option[int]
auto city = user?.address?.city       # Option[String]

# Default operator (??)
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

Square brackets `[]` for type parameters. Trait bounds are written inline before the parameter name, using `&` to combine multiple bounds.

```gorget
# Inline trait bound — Comparable comes before the param name T
T max[Comparable T](T a, T b):
    if a > b: a else: b

# Unconstrained generic
T identity[T](T x):
    return x

# Type parameters in types
Vector[int] numbers = Vector[int]()
HashMap[String, int] users = HashMap[String, int]()
Vector[Option[int]] nested = Vector[Option[int]]()

struct Pair[A, B]:
    A first
    B second
```

**No ambiguity with indexing**: `Vector[int]` = generic (Vector is a type), `arr[0]` = indexing (arr is a variable). Compiler knows which names are types.

**Monomorphization**: Each concrete type gets its own compiled version (zero-cost, like Rust/C++).

### 4.4 Traits (Interfaces)

```gorget
trait Displayable:
    String display(self)

trait Comparable:
    int compare(self, Self other)

# Trait with default implementation
trait Greetable:
    String name(self)

    String greeting(self):
        return f"Hello, {self.name()}!"

# Trait inheritance with extends
trait Animal extends Displayable:
    String name(self)
    String sound(self)

# Iteration traits (generic parameter pattern)
trait Iterable[T]:
    Iterator[T] iter(&self)

trait Iterator[T]:
    Option[T] next(&self)
```

### 4.4.1 Trait Naming Conventions

Gorget doesn't enforce trait naming rules, but following consistent conventions makes code read naturally with `is` (trait bounds) and `equip...with` (implementations).

| Category | Suffix | Examples | Reads with `is` / `equip...with` |
|----------|--------|----------|-----------------------------------|
| Capabilities | `-able` / `-ible` | `Hashable`, `Equatable`, `Displayable`, `Debuggable`, `Serializable`, `Cloneable`, `Iterable` | `is Hashable` / `equip Point with Hashable` |
| Behaviors / roles | `-er` / `-or` | `Iterator`, `Writer`, `Reader`, `Hasher`, `Handler`, `Formatter` | `is Writer` / `equip File with Writer` |
| Operators / conversions | bare verb/noun | `Add`, `Sub`, `From`, `Into`, `Index`, `Copy`, `Default`, `Error` | `is Add` / `equip Point with Add` |
| Domain abstractions | bare noun | `Shape`, `Animal`, `Collection` | `is Shape` / `equip Circle with Shape` |

**The I/O pair — byte-shaped roles.** `Writer` and `Reader` are canonical
`-er` role traits: a type *is a* Writer / Reader — it's not an incidental
capability, it's the type's job. The method set is minimal (one
`write` / `read` + derived helpers) so any byte-producing
source (File, String builder, Socket, TlsSocket) plugs in with one
equip block.

**The hashing split — role + capability.** `Hashable` (capability —
"can be hashed") takes a `Hasher` (role — "accumulates hash state")
and forwards field bytes into it. The split is deliberate: hash
algorithms vary (FxHash, SipHash, …) and belong behind a role trait;
what each type contributes to that state is a fixed capability.

**Debuggable vs Displayable.** Both -able, but distinct categories:
`Displayable.display(self)` is user-facing (`{v}` in f-strings, Error
messages), `Debuggable.debug(self)` is developer-facing (`{v:?}` in
f-strings, panic traces). `@derive(Debuggable)` auto-generates
field-by-field output; `Displayable` is typically hand-written.

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
    String into_string(!self):                 # move (takes ownership)
        return f"({self.x}, {self.y})"
    static Point origin():                     # no self (static)
        return Point(0.0, 0.0)
```

Method receivers are **auto-borrowed**: the compiler automatically takes a reference to the receiver based on the method's `self` declaration — immutable borrow for `self`, mutable borrow for `&self`, move for `!self`. No `&` or `!` annotation is needed at the method call site. This is the one exception to Gorget's rule that `&` must appear at the call site to acknowledge mutation — method signatures are part of the API contract, and requiring explicit borrows on every method call would make chaining and fluent APIs impractical (`(&(&items).filter(f)).map(g)`).

### 4.6 Equipping Traits

```gorget
equip Point with Displayable:
    String display(self):
        return f"({self.x}, {self.y})"

# Generic implementation — bound on the equip's T
equip[Displayable T] Vector[T] with Displayable:
    String display(self):
        auto parts = [item.display() for item in self]
        return f"[{parts.join(", ")}]"

# Blanket implementation
equip[Displayable T] T with Printable:
    void print(self):
        println(self.display())
```

### 4.7 Inline Trait Bounds

Trait bounds are written inline in the generic parameter list, before the parameter name. This follows Gorget's type-first convention: the constraint comes first, then the name it constrains.

```gorget
# Single bound — Displayable before T
void print_all[Displayable T](Vector[T] items):
    for item in items:
        print(item.display())

# Multiple bounds on one param — use & to combine
void process[Displayable & Cloneable & Comparable T](T item):
    ...

# Multiple type variables with different bounds
void complex[Displayable & Cloneable T, Into[T] U](T a, U b):
    ...
```

The `where` keyword is reserved for future use (e.g., complex trait bounds). Currently, all constraints are expressed inline via trait bounds on generic parameters.

### 4.8 Method Dispatch

Gorget uses a simple dispatch model — the programmer never chooses between static and dynamic dispatch. The compiler does the right thing automatically:

- **Known concrete type → direct dispatch.** When the compiler can see the concrete type at compile time, it calls the method directly. This enables inlining and is zero-cost.
- **Unknown concrete type → dynamic dispatch.** When the concrete type isn't known (heterogeneous collections, trait-typed parameters), the compiler uses vtable-based dispatch automatically.

```gorget
# Direct dispatch — compiler knows c is a Circle
Circle c = Circle(5.0)
c.render()                # calls Circle_render() directly

# Dynamic dispatch — collection holds mixed types
Vector[Box[Shape]] shapes = Vector[Box[Shape]]()
shapes.push(Box.new(Circle(5.0)))
shapes.push(Box.new(Rectangle(3.0, 4.0)))

for shape in shapes:
    shape.render()        # vtable dispatch — could be Circle or Rectangle

# Return type is a trait — caller doesn't know the concrete type
Box[Shape] make_shape(String kind) throws ValueError:
    match kind:
        case "circle": Box.new(Circle(1.0))
        case "rect": Box.new(Rectangle(1.0, 1.0))
        else: throw ValueError(f"unknown shape: {kind}")
```

**Design rationale:** No `dyn`/`dynamic` keyword. The programmer focuses on *what* they want (a Shape), not *how* it's dispatched. The compiler has enough information to decide. Generics are still monomorphized — `Vector[int]` and `Vector[String]` generate separate specialized code. This combines the simplicity of Go's interfaces with the performance of Rust's monomorphized generics.

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
        print(f"rgb({r}, {g}, {b})")
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
        print(f"Adult: {name}")
    case Ok(User(name, _)):
        print(f"Minor: {name}")
    case Error(e):
        print(f"Error: {e}")
```

### 5.3 Match as Expression

Single-expression arms use `:` with the value on the same line. Multi-line arms use `:` followed by an indented block:

```gorget
String label = match color:
    case Red: "red"
    case Green: "green"
    case Custom(r, g, b): f"rgb({r}, {g}, {b})"
    else: "other"
```

### 5.4 The `in` Keyword (Containment Checks)

`in` works as a boolean operator outside of `for` loops, testing whether a value exists in a collection:

```gorget
# Works on Vector, Set, Dict (checks keys), Array, String, Range
if "admin" in roles:
    grant_access()

if user_id not in banned_ids:
    allow_login()

# String containment (substring search)
if "error" in log_line:
    alert()

# Range containment
if port in 1024..65535:
    print("valid port")

# Dict checks keys (like Python)
if "name" in config:
    print(config["name"])

# Use in match guards and while conditions
Result[int, String] parsed = parse(input)
match parsed:
    case Ok(n) if n in 1..=100: print("valid")
    else: print("out of range")
```

Any type implementing the `Contains` trait supports `in`:

```gorget
trait Contains[T]:
    bool contains(self, T value)
```

The compiler desugars `x in collection` to `collection.contains(x)` and `x not in collection` to `not collection.contains(x)`.

### 5.5 The `is` Keyword (Pattern Matching in Conditions)

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

### 5.6 If as Expression

```gorget
int abs_val = if x >= 0: x else: -x

String msg = if user.is_admin():
    "Welcome, admin"
else:
    "Welcome, user"
```

### 5.7 Loops

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

### 5.8 for/else and while/else (Python-style)

```gorget
for item in collection:
    if item.matches():
        break
else:
    print("no match found")     # runs if loop completes without break
```

### 5.9 Loop as Expression

```gorget
int result = loop:
    if compute() is Some(v):
        break v                  # break with a value
```

### 5.10 Comprehensions (Python-style)

```gorget
Vector[int] squares = [x * x for x in 0..10]
Vector[int] evens = [x for x in 0..100 if x % 2 == 0]
HashMap[String, int] lengths = {s: s.len() for s in words}
HashSet[int] unique = {x * x for x in 1..=10}
```

---

## 6. Error Handling

### 6.1 The `throws` Model

Functions that can fail use `throws`. Errors auto-propagate without `?`:

```gorget
# Clean: no ?, no Result wrapping
Data process(String path) throws AppError:
    String content = read_file(path)          # auto-propagates if error
    Config config = parse_config(content)     # auto-propagates if error
    return transform(config)

# To handle an error locally, use type-directed Result capture:
Data safe_process(String path) throws AppError:
    Result[String, AppError] result = read_file(path)   # Result type suppresses auto-propagation
    match result:
        case Ok(content): return parse(content)
        case Error(e):
            log(f"Fallback: {e}")
            return default_data()

# To explicitly raise an error, use throw:
Record parse_line(String line) throws ParseError:
    if line.is_empty():
        throw ParseError("empty line")      # raises error, exits function
    return parse(line)

# Non-throwing functions capture via Result type:
void main():
    Result[Data, AppError] result = process("data.txt")
    match result:
        case Ok(data): print(data)
        case Error(e): print(f"Error: {e}")
```

**Keywords summary:**
- `throws` — annotates a function that can fail (on the signature)
- `throw` — explicitly raises an error (inside a `throws` function)
- **Type-directed capture** — declaring the destination as `Result[T, E]` suppresses auto-propagation

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
    String display(self):
        match self:
            case Io(e): f"IO error: {e}"
            case Parse(e): f"Parse error: {e}"
            case NotFound(path): f"Not found: {path}"

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
Result[String, IOError] content = read_file(path)
    .context(f"loading config from {path}")

# Accessing trace:
Result[Data, AppError] result = process("data.txt")
match result:
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

#### Panic vs Result: When to Use Which

**Panic** = programmer errors, internal invariant violations, and resource exhaustion:
- Index out of bounds, integer overflow, division by zero
- Unwrap on `None` / `Error`
- Assertion failures
- Send/recv on closed channel
- OOM (constructors, arena overflow, any allocation failure)
- Unreachable code paths

**Result** = environmental failures (the external world didn't cooperate):
- File I/O (open, read, write — file might not exist, permissions, disk full)
- Network (connect, send, recv — host unreachable, timeout)
- Parsing external data (JSON, TOML, regex compile, `String.to_int`)
- Process spawn (command not found, permission denied)
- TLS/crypto (handshake failure, invalid certificate)

**Rule of thumb:** Can the caller prevent this failure by writing correct code? Yes → panic. No → Result.

The rule still holds: a fault (overflow, bounds, div0) is a programmer error, so it **panics by default** — continuing with corrupted state is worse than stopping. What the rule adds, rather than reverses, is **opt-in local recovery**: a faulting op can be caught *lexically*, at the operation site, instead of aborting the process.

#### Recoverable Faults

A small, closed set of panics — **`Fault.Overflow`** (integer overflow), **`Fault.DivByZero`** (division or remainder by zero), and **`Fault.Bounds`** (an out-of-bounds index into an array-backed collection) — form the **fault** kind. They panic by default, but an expression that can fault may opt into a local fallback with `catch`:

```gorget
# Pattern form: catch one named variant, yield a fallback.
int r = (big * 2) catch Fault.Overflow: -1

# Binding form: bind the constructed Fault value and match on it.
int d = (10 / z) catch f: match f:
    case Fault.Overflow(): 111
    case Fault.DivByZero(): 222
```

This recovery is **strictly local and lexical**:

- It covers only the faultable ops emitted directly into the wrapped expression's own basic blocks. A fault raised *inside a function the expression calls* is not caught — it still panics.
- An uncaught fault still panics with a diagnostic and exits, exactly as before. `catch` adds a recovery path; it does not change the default.
- Faults stay **out of function signatures** — a plain `int sum(...)` does not become a `Result`-returning function just because it does arithmetic. Faults are not contract errors; they are not part of any function's `throws` type or the API surface.

The variants are spelled **qualified** (`Fault.Overflow`, not bare `Overflow`); a wrong qualifier (`Bogus.Overflow`) is a compile-time error. `INT_MIN / -1` and `INT_MIN % -1` are overflows (`Fault.Overflow`), not div-by-zero. This fault `catch Fault.X:` is distinct from the `Result`/`throws` `catch (e):` form (§6.1's model), which recovers a *contract* error from a throwing call.

### 6.5 Assert (Always-On)

`assert` checks a condition and panics with a message if it fails. Unlike C/Java, **assertions are always enabled** — they are never stripped in release builds.

```gorget
void process(Vector[int] data):
    assert data.len() > 0, "data must not be empty"
    assert is_sorted(data)                             # message is optional

    int result = compute(data)
    assert result >= 0                                 # post-condition
```

Rationale: assertions that only run in debug builds create a false sense of safety. If a condition is worth checking, it's worth checking in production. The performance cost of an `assert` is the cost of evaluating its condition — if that's too expensive, use a debug-only check explicitly:

```gorget
@[debug_only]
void expensive_invariant_check(Tree t):
    assert t.is_balanced()
    assert t.size() == t.count_nodes()
```

The `@[debug_only]` attribute strips the entire function in release builds, making the opt-out explicit rather than implicit.

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
String(int) formatter = (n): f"Value: {n}"

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

Closures support three user-facing capture modes:

- **Immutable borrow** (default) — the variable is read but not mutated inside the closure; the outer binding stays valid.
- **Mutable borrow** — the compiler detects that the closure mutates the variable and captures a pointer to the outer slot automatically.
- **Move** — the closure takes ownership of the captured value. Use `!` before the parameter list to force ALL captures into move mode.

The compiler infers immutable-borrow vs. mutable-borrow automatically from the closure body. Move capture is never inferred — it must be requested explicitly with `!`. Internally, immutable-borrow and move captures are both stored by value in the closure struct (the difference is whether the outer binding survives); mutable-borrow captures store a pointer to the outer variable.

Use `!` before the parameter list to force-move ALL captures:

```gorget
# Default: auto-infer (immutable borrow captures)
String name = "Alice"
auto greet = (): print(f"Hello {name}")
print(name)     # OK — name was only borrowed

# Move ALL captures with !()
auto handle = thread.spawn(!():
    print(f"Hello from thread: {name}")
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

### 7.5 Untyped Parameters & Contextual Inference

Closure parameters can omit type annotations when the compiler can infer them from the calling context. This is the recommended middle ground between the fully implicit `it` (single-param only) and fully typed parameters:

```gorget
# Fully typed — always works, most verbose
auto sum = v.fold(0, (int acc, int x): acc + x)

# Untyped — inferred from fold's signature: T(T, T) where T = int
auto sum = v.fold(0, (acc, x): acc + x)

# Implicit it — only works for single-param closures
auto doubled = v.map(it * 2)
```

The three tiers give authors a smooth verbosity dial:

| Need | Syntax | When to use |
|---|---|---|
| Trivial single-param body | `it` | `.map(it * 2)`, `.filter(it > 0)` |
| Multi-param or named clarity | `(params): body` | `.fold(0, (acc, x): acc + x)` |
| Explicit types needed | `(typed params): body` | Ambiguous contexts, documentation |

**Design rationale.** Positional placeholders like `$1`, `$2` (Swift-style) were considered and rejected. They add a new sigil, introduce arity ambiguity (the compiler must scan the body to determine parameter count), and lose readability for multi-param closures where names like `acc` and `x` carry semantic meaning. The untyped-but-named style `(acc, x):` is nearly as concise while remaining self-documenting.

**Inference strategy.** When a closure is passed to a function or method whose parameter type is known (e.g., `fold` expects `T(T, T)`), the compiler unifies each untyped closure parameter with the corresponding type from the expected signature. This is the same mechanism used for `it` inference, generalized to N parameters.

### 7.6 `it` Rules

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
import std.sync.{Mutex, RwLock}               # multiple items with {}
from xtd.log import LogLevel, Logger           # import type only (qualified variants)
from xtd.log import LogLevel.*                 # glob: import type + all variants bare
from xtd.log import LogLevel.*, Logger         # glob + other names in same statement
```

**Glob import** (`EnumName.*`) brings a type's enum variants into bare scope, useful when code makes heavy use of one enum. Without the glob, variants require qualified access (`LogLevel.Info()`). With the glob, bare names work (`Info()`). The type itself is always in scope regardless.

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

#### Allocation Philosophy

Gorget is a Python-like language: string concatenation, f-strings, `upper()`, `replace()`, and collection construction all allocate without ceremony. This is a deliberate trade-off — ergonomic code on the default path, explicit control when you need it.

**What allocates implicitly:** `+` on strings, f-strings, `upper()`, `replace()`, `split()`, `Vector()`, `Dict()`, format conversions. **What doesn't allocate:** string literals, `slice()`, `trim()`, `strip()`, `char_at()`, `removeprefix()`, `removesuffix()`, and `String t = s` when `s` is a view — these are zero-cost views via copy-on-write. The compiler auto-materializes views when the source is mutated.

**How to take control:** All allocations go through `__gorget_current_alloc`, a thread-local allocator pointer. Two mechanisms redirect it:

```gorget
from std.alloc import Arena

# Scoped allocator — ALL allocations in this block use the arena,
# including string operations, f-strings, collection resizes, etc.
with Arena(4096) as pool:
    Vector[int] v = Vector[int]()
    String s = name.upper()        # arena-allocated
    String msg = f"hello {name}"   # arena-allocated
    # pool.bytes_used() shows total arena consumption

# One-shot allocator — only this constructor uses the arena
Vector[int] v = Vector[int](alloc=pool)
```

The compiler enforces safety: escape analysis prevents arena-scoped data from outliving its allocator (compile-time error, not a runtime crash).

Six composable allocators are in `std.alloc`: Arena (bump), PoolAllocator (fixed-size blocks), TlsfAllocator (general-purpose O(1)), TrackingAllocator (instrumentation wrapper), FixedBufferAllocator (stack bump), and FallbackAllocator (primary + secondary combinator). See language reference §15.3.

### 9.2 Shared Ownership

```gorget
# Reference-counted shared ownership (thread-safe, atomic refcount)
Shared[String] shared = Shared[String]("shared data")
Shared[String] clone = shared.clone()  # increments ref count

# Weak reference (doesn't prevent deallocation)
Weak[String] weak = shared.downgrade()
if weak.upgrade() is Some(strong):
    print(strong)                      # still alive

# Shared mutable state across threads — wrap the data in a Mutex
Shared[Mutex[int]] counter = Shared[Mutex[int]](Mutex[int](0))
```

`Shared[T]` is the single shared-ownership primitive — it is always atomic, so it
works single- or multi-threaded (one type, no single-threaded/atomic split).

### 9.3 Interior Mutability

When you need to mutate data behind an immutable reference, wrap it in a lock:

```gorget
# Mutex — for thread-safe interior mutability
Mutex[Vector[int]] shared_data = Mutex[Vector[int]](Vector[int]())
auto guard = shared_data.lock().unwrap()
guard.push(42)
```

### 9.4 Deref Coercion

Smart pointers automatically dereference to their inner type:
```gorget
Box[String] boxed = Box.new(String("hello"))
print(boxed.len())       # Box[String] auto-derefs to String, calls String.len()
```

---

## 10. Concurrency

The concurrency model includes async/await, spawn, channels, `Shared[T]`, `Mutex[T]`, `RwLock[T]`, `select`, and structured concurrency with multiple scheduler backends (`pool`, `thread`, `inline`, `single`).

```gorget
from std.sync import Mutex

async void bump(Shared[Mutex[int]] !counter):
    auto guard = counter.lock().unwrap()
    *guard += 1

async void main():
    Shared[Mutex[int]] counter = Shared[Mutex[int]](Mutex[int](0))
    Vector[Task[void]] handles = Vector[Task[void]]()

    for _ in 0..10:
        Task[void] handle = spawn bump(!counter.clone())
        handles.push(handle)

    for h in handles:
        h.await()

    print(f"Count: {*counter.lock().unwrap()}")
```

### Async/Await

Postfix `.await()` as a method-call suffix (similar to Rust's `.await` but with parentheses for visual consistency with method calls). The postfix style keeps data flow left-to-right and chains naturally with method calls and `throws`.

```gorget
# async + throws compose naturally
async String fetch(String url) throws HttpError:
    Response resp = http.get(url).await()      # .await() is postfix
    return resp.text().await()

# async without throws (infallible async)
async int compute_slowly():
    sleep(Duration.seconds(1)).await()
    return 42

# Calling async functions
async void main():
    String data = fetch("https://example.com").await()
    print(data)

# Concurrent execution
async void fetch_all():
    # Launch multiple tasks concurrently
    auto task1 = spawn fetch("https://api.example.com/a")
    auto task2 = spawn fetch("https://api.example.com/b")

    # Await results
    String a = task1.await()
    String b = task2.await()
    print(f"{a}, {b}")

# async closures
auto fetcher = async (String url):
    return http.get(url).await()

# async with error handling (throws + Result capture)
async void resilient_fetch(String url):
    Result[String, IOError] result = fetch(url).await()
    match result:
        case Ok(data): print(data)
        case Error(e): print(f"Failed: {e}")
```

### Thread Safety: Sendable & Syncable

Gorget uses two marker traits to enforce thread safety at compile time, similar to Rust's `Send`/`Sync`:

- **`Sendable`** — a type can be moved to another thread. Most types are Sendable. Types that aren't: raw pointers, thread-local handles, non-threadsafe FFI wrappers.
- **`Syncable`** — a type can be shared (by reference) across threads. A type is Syncable if `&T` is safe to access from multiple threads concurrently.

```gorget
# thread.spawn requires the closure and its captures to be Sendable
void main():
    auto data = Vector[int].from([1, 2, 3])
    thread.spawn(!():                # data is moved (!), Vector is Sendable — OK
        print(f"{data.len()}")
    )

# Shared[T] is Sendable + Syncable when T is Syncable
# Mutex[T] makes any T Syncable (by synchronizing access)
Shared[Mutex[int]] counter = Shared[Mutex[int]](Mutex[int](0))    # Sendable + Syncable
```

**Auto-derivation**: The compiler automatically derives `Sendable` and `Syncable` for types whose fields are all `Sendable`/`Syncable`. Most user-defined structs and enums are automatically thread-safe.

```gorget
struct Point:                      # auto-Sendable, auto-Syncable (all fields are int)
    int x
    int y

struct UnsafeHandle:
    RawPtr[void] ptr               # RawPtr is NOT Sendable — Point is not auto-Sendable
```

To manually implement these traits for types the compiler can't verify (e.g., FFI wrappers with internal synchronization), use `unsafe equip`:

```gorget
unsafe equip MyFfiHandle with Sendable    # "I guarantee this is safe to send"
unsafe equip MyFfiHandle with Syncable    # "I guarantee this is safe to share"
```

The `thread.spawn` function has an implicit `[Sendable F]` bound, so the compiler rejects any attempt to send non-Sendable types across threads — no data races from accidental sharing.

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
    int printf(String format, ...)
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

F-strings (prefix `f`) support interpolation — any type implementing Displayable auto-formats:

```gorget
String name = "world"
int count = 42
print(f"Hello, {name}! Count is {count}")
print(f"Math: {2 + 2}")                     # expressions in braces
print(f"Escaped brace: {{literal}}")         # double-brace to escape

# Format specifiers
print(f"{255:x}")         # "ff"     — hex
print(f"{3.14159:.2f}")   # "3.14"   — 2 decimal places
print(f"{42:08d}")        # "00000042" — zero-padded
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

equip[Displayable T] List[T]:
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

equip[Displayable T] List[T] with Displayable:
    String display(self):
        match self:
            case Cons(head, tail):
                match *tail:
                    case Nil: return f"{head}"
                    case _: return f"{head} -> {tail.display()}"
            case Nil:
                return "[]"

void main():
    auto list = List[int].new()
    list = list.prepend(3)
    list = list.prepend(2)
    list = list.prepend(1)

    print(f"List: {list.display()}")         # "1 -> 2 -> 3"
    print(f"Length: {list.len()}")            # 3
```

---

## 15. Design Decisions (Resolved)

| # | Question | Decision |
|---|----------|----------|
| 1 | **Lifetime annotations** | None required — the compiler's borrow checker infers all origin tracking internally. Move types transfer ownership on return, eliminating the need for user-facing lifetime syntax. |
| 2 | **Implicit borrow at call sites?** | Yes — bare type = immutable borrow, no annotation needed |
| 3 | **Expression-oriented blocks?** | Both — `return` for explicit early returns, last expression as implicit return value |
| 4 | **Inheritance?** | None — composition via traits only |
| 5 | **Macro system?** | None for V1; add hygienic macros in V2+ |
| 6 | **File extension** | `.gg` |
| 7 | **Indentation** | 4 spaces (enforced by `gg fmt`) |
| 8 | **Compilation target** | C (via SSA-based LIR → C transpilation, then a system C compiler); an LLVM backend exists behind `--backend=llvm` |
| 9 | **Package management** | Built into `gg` CLI (`gg new`, `gg add --git/--path`, `gg remove`) |
| 10 | **Option handling** | `Option[T]` with rich sugar: `is` pattern matching, `?.` optional chaining, `??` default operator, `.unwrap()`, `.unwrap_or()`, `?` early return |
| 11 | **Tuple syntax** | `(int, String)` — concise, universal |
| 12 | **Array syntax** | C-style: `int[5]` fixed array, `Vector[int]` growable; `int[]` slices are `String`-only today (see §24) |
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
| Lifetimes | Fully inferred (no annotations) | Signature-only + `'a` | N/A | N/A |
| Generics | `[T]` | `<T>` | `[T]` | `<T>` |
| Mutability | Mutable default + `const` | `let` default + `mut` | Default mutable | `final`/`const` |
| Error handling | `throws` + Result capture | `Result` + `?` | Exceptions | Exceptions |
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
        print(f"negative: {x}")
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
        print(f"unknown command: {cmd}")
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
        print(f"unexpected: {code}")
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
Vector[String] names = [p.name for p in people]

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
@derive(Cloneable, Equatable, Hashable)
struct Point:
    float x
    float y

@derive(Serializable, Deserializable)
enum Message:
    Text(String)
    Image(Vector[uint8])
    Ping

test "addition":
    assert add(2, 3) == 5

@should_panic("division by zero")
test "division by zero":
    divide(1, 0)

@inline
int fast_add(int a, int b):
    return a + b

# *Not yet implemented*
# @cfg(target_os = "linux")
# void linux_only():
#     ...

# *Not yet implemented*
# @deprecated("use new_api() instead")
# void old_api():
#     ...
```

---

## 21. Associated Types & Const Generics

### 21.1 Associated Types

*Not yet implemented.* Associated types are parsed but not validated or resolved in semantic analysis. Use generic parameters instead:

```gorget
# Iterable uses generic parameter (not associated type)
trait Iterable[T]:
    Iterator[T] iter(&self)

# Iterator: the stateful cursor that walks through elements
trait Iterator[T]:
    Option[T] next(&self)

# Making a custom type iterable
equip Counter with Iterable[int]:
    Iterator[int] iter(&self):
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

equip[Default T, const int N] FixedArray[T, N]:
    static FixedArray[T, N] zeroed():
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
# Standard library defines (generic parameter pattern):
trait Add[Out]:
    Out add(self, Self rhs)

trait Sub[Out]:
    Out sub(self, Self rhs)

trait Mul[Out]:
    Out mul(self, Self rhs)

trait Index[K, V]:
    V get(self, K key)

# User equips:
equip Point with Add[Point]:
    Point add(self, Point rhs):
        return Point(self.x + rhs.x, self.y + rhs.y)

equip Matrix with Index[int, Vector[float]]:
    Vector[float] get(self, int row):
        return self.data[row]

# Now operators work:
Point c = a + b              # calls a.add(b)
Vector[float] row = matrix[0]   # calls matrix.get(0)
```

---

## 23. String Types in Depth

Gorget has a single `String` type — a 32-byte struct `{ data, cap, len, alloc }`. The `cap` field distinguishes **views** (`cap == 0` — zero allocation, backed by a pointer into existing data like `.rodata` or another string's buffer) from **owned** strings (`cap > 0` — heap-allocated, growable). Programmers write `String` everywhere; the compiler infers which operations produce views and which produce owned copies.

This is similar to how Swift's `String` unifies owned and borrowed representations behind a single type. Gorget uses compile-time `ViewOf(source)` provenance tracking to auto-materialize views when the source is mutated — **full lazy copy-on-write**: the copy is deferred to the mutation itself, and a mutation that never executes never allocates. This is implemented as the default in both compilers (the self-host lowerer realizes the provenance design directly; the Rust lowerer reaches the same observable behavior through read-site materialize hooks) — see the compiler internals book, [`devbook/11` §"Full lazy materialization"](devbook/11-copy-on-write.md#full-lazy-materialization-37--the-lazy-cow-default).

**Provenance inference rules:**
- String literals (`"hello"`) are views into static data — zero allocation.
- Concatenation, formatting, and mutation produce owned strings.
- Passing a string to a function that only reads it keeps the view provenance.
- The compiler inserts ownership promotion (copy to heap) only when needed — e.g., storing into a collection or returning from a function where the source would go out of scope.

**Struct field ownership:** String fields in structs are **owned** (heap-allocated). The struct is responsible for freeing them. Reading a string field returns a **view** (no allocation, borrows from the struct). A bare-assign to a `String` variable also borrows — whether `auto` or explicitly typed — and CoW-severs on the first mutation; use `.clone()` for an independent owned copy up front:

```gorget
struct Config:
    String name

Config c = Config("myapp")
print(c.name)                 # view — zero cost, borrows from c
String view = c.name          # ALSO a borrow — CoW-severs on mutation, not an auto-clone
String owned = c.name.clone() # independent owned copy (explicit)
# When c goes out of scope, c.name is freed automatically
```

```gorget
# All of these are type String
String literal = "hello"               # view into static data — no allocation
String owned = "hello" + " world"      # owned — concatenation allocates

# No coercion needed — it's the same type
void greet(String s):
    print(s)

greet("hi")                            # view — no allocation
greet("hello" + " world")             # owned — compiler promotes automatically

# Raw strings (no escape processing)
String regex = r"^\d+\.\d+$"
String path = r"C:\Users\name\docs"

# Multi-line strings
String query = """
    SELECT *
    FROM users
    WHERE active = true
"""

# Byte strings — *Removed; use `Vector[uint8]` instead*
# [uint8] bytes = b"hello"

# Single-quoted strings — just String values (no separate char type)
String letter = 'A'
String emoji = '\u{1F40D}'     # snake emoji

# String interpolation (f-strings only)
String greeting = f"Hello, {name}! You are {age} years old."
String math = f"2 + 2 = {2 + 2}"
String formatted = f"Pi is approximately {pi:.4f}"      # format specifiers
String hex = f"Color: #{r:02x}{g:02x}{b:02x}"          # zero-padded hex

# String repetition (Python-style * operator)
String line = "-" * 40                   # "----------------------------------------"
String indent = "  " * depth             # repeat by variable
String border = "=-" * 20               # "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
```

String repetition uses `*` with a string on the left and an integer on the right (like Python's `"abc" * 3`). This is implemented via the `Mul[int]` trait on `String`, so it composes naturally with operator overloading.

---

## 24. Arrays, Slices, and Vectors

C-style array syntax: `Type[Size]` for fixed arrays, `Vector[T]` for growable.

```gorget
# Fixed-size array (C-style: type[size])
int[5] arr = [1, 2, 3, 4, 5]

# Type inferred
auto arr = [1, 2, 3, 4, 5]           # Vector[int] — dynamic, supports push/pop/etc.

# Vector: owned, heap-allocated, growable
Vector[int] vec = Vector[int]()
vec.push(1)
vec.push(2)

# Vector from literal
Vector[int] nums = [1, 2, 3, 4, 5]   # literal syntax, type annotation clarifies

# Array methods
int length = arr.len()
bool has_3 = arr.contains(3)
```

> **Slices are `String`-only today.** The general `T[]` slice type — a borrowed
> view into a contiguous array/vector — is parsed and type-checked but **not
> lowered**: it has no runtime representation. The only runtime view is the
> `cap == 0` `String` view (§23), which works precisely because owned and view
> share one 32-byte type. So `String` slicing (`s[1..4]`, `byte_slice`) is fully
> supported; a non-`String` `int[]` works only as a function-local binding and
> miscompiles if it escapes (e.g. returned). For an array/vector sub-range use
> `Vector[T]` and `Vector.slice(start, end)`, which returns an **independent
> copy** (deep-cloning resource-type elements), not a borrowed view.

**Disambiguation**: `int[5]` = fixed array type (type position), `arr[5]` = indexing (value position). Compiler knows which is which from context, same as generics (`Vector[int]` vs `vec[0]`).

#### Bounds Checking

All indexing operations on arrays, slices, and vectors are **bounds-checked at runtime**. Out-of-bounds access panics with a clear error message rather than causing undefined behavior:

```gorget
int[3] arr = [10, 20, 30]
int x = arr[5]                   # panic: index 5 out of bounds (length 3)

Vector[int] vec = [1, 2, 3]
vec.set(10, 42)                  # panic: index 10 out of bounds (length 3)
```

This is a **language guarantee**, not an implementation detail. Gorget will never silently read or write out-of-bounds memory. The compiler may elide bounds checks when it can statically prove the index is in range (e.g., iterating with `for i in 0..arr.len()`), but the semantics are always as-if checked.

For performance-critical inner loops where bounds are known safe, `unsafe` indexing is available:

```gorget
unsafe:
    int x = arr.get_unchecked(i)     # no bounds check — UB if out of range
```

#### Stdlib Design: Option Over Panic

The standard library follows a strict principle: **methods that can fail return `Option[T]` (or `Result[T, E]`) instead of panicking**. A lookup that might not find anything is not an error — it's a normal outcome that the type system should represent.

```gorget
Vector[int] v = [10, 20, 30]
Option[int] x = v.get(5)            # None (not a panic)
int y = v.get(1).unwrap()           # 20

Dict[String, int] m = {"a": 1}
Option[int] val = m.get("z")        # None (not a panic)
int fallback = m.get("z") ?? 0      # 0 via default operator
```

The guideline: if a caller can reasonably trigger the failure case through normal use (missing key, out-of-bounds index, empty collection), the method must return `Option[T]`. Panics are reserved for logic errors — violations of documented preconditions that indicate a bug in the calling code (e.g., `unwrap()` on `None`, indexing with `[]` out of bounds).

Convenience wrappers that provide inline fallbacks (`get_or`, `get_or_put`, `unwrap_or`) return `T` directly — the caller has already decided what to do on absence.

#### Index Access Ownership

Subscript read (`v[i]`) borrows the element in place — it is not moved out of the collection and not eagerly copied. Whether that borrow is read-only or write-through depends on the root (the one rule from §3): **direct place mutation** on a collection you own or hold via `&` writes through in place (`matrix[0].push(4)`, `v[i] = x`); a subscript **bound to a local** (`Vector[int] row = matrix[0]`) is a read-only borrow like any other, and mutating that local **materializes** a private copy (copy-on-write), leaving the collection untouched (§3.2). Here Gorget is **more tolerant than Rust's `Index`/`IndexMut`** — a write through the bound borrow copies rather than being rejected.

What happens next depends on how the borrow is used:

| Usage | Semantics | Example |
|-------|-----------|---------|
| Read through the reference | Zero-cost borrow | `print(v[0])` |
| Bare-assign to a variable (`auto` or typed) | Zero-cost borrow — CoW-severs on mutation | `Vector[int] row = matrix[0]` |
| `.clone()` the read | Owned copy up front | `Vector[int] row = matrix[0].clone()` |
| Call a mutating method | Mutate in place | `matrix[0].push(4)` |
| Pass to a function taking `&` | Pass the borrow | `process(&matrix[0])` |

**Explicit clone required:** Resource types are never implicitly copied. When you need an independent owned copy, call `.clone()` explicitly. This makes every heap allocation visible in the source code:

```gorget
Vector[int] row = matrix[0].clone()   # explicit clone — allocation visible
auto row_ref = matrix[0]              # borrow — zero cost reference
Vector[int] taken = matrix.remove(0).unwrap() # remove + unwrap — take ownership, no clone
```

**Consuming element access:** To move an element out of a collection (transferring ownership), use a consuming method instead of subscript:

```gorget
Vector[int] row = matrix.remove(0).unwrap()   # removes and returns — no clone
Option[int] last = v.pop()           # removes last — no clone
```

**Subscript write** (`v[i] = val`) drops the existing element and moves `!val` into the slot. `v.insert(i, !val)` shifts elements right and moves `!val` in.

**Design rationale:** This follows Rust's principle that indexing borrows, explicit methods consume. A bare-assign from a subscript read borrows whether you write `auto` or an explicit type — the clone is deferred to a mutation through the alias (copy-on-write) or to an ownership boundary, and an explicit `.clone()` makes it eager. This keeps reads zero-cost by default while keeping every heap allocation visible at the mutation or `.clone()` site, consistent with Gorget's "Python-like surface, Rust-like safety" philosophy.

---

## 25. The `with` Statement (Scoped Resource Management)

Python-style `with` for explicit resource scoping:

```gorget
# In a throws function, errors auto-propagate:
void read_data(String path) throws IoError:
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

All compile-time constructs use the `meta` keyword. Meta declarations are evaluated before type-checking, substituted into the AST, and then removed — nothing reaches the runtime binary.

```gorget
# Constants
meta int   MAX_SIZE = 1024
meta float PI       = 3.14159265358979
meta float TAU      = 2.0 * PI

# Assertion (compile error if false)
meta assert MAX_SIZE > 0, "MAX_SIZE must be positive"

# Type alias (plain and conditional)
meta type Index    = int32
meta type FastMap  = Dict if feature("ordered") else HashMap

# Type function (multi-branch type selection)
meta type sized_int(int bits):
    if bits <= 8:   return int8
    elif bits <= 16: return int16
    elif bits <= 32: return int32
    else:           return int64

meta type Word = sized_int(arch_word_bits())

# Conditional compilation — losing branch is never type-checked
meta if platform() == "linux":
    from std.net import LinuxSocket as Socket
else:
    from std.net import MacSocket as Socket

# Ordinary functions called at compile time (no annotation needed)
int factorial(int n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

meta int FACT_10 = factorial(10)   # computed at compile time
```

For full syntax and semantics — including evaluation order, built-in meta functions (`platform()`, `arch()`, `sizeof()`, `feature()`, etc.), compile-time function evaluation rules, and limits — see **Section 19** of the language reference.

Note: `static` is a separate concept (§5.9 of the language reference) — a global variable with program lifetime, not a compile-time constant.

---

## 29. Testing (Built-in, First-Class)

Gorget's test framework is built into the language and compiler. The design philosophy:

- **Zero new concepts where existing features suffice.** Parameterized tests, fixtures, and contracts are all expressed through features that already exist (`meta for`, `with` + `Drop`, `assert`).
- **Always-on assertions.** `assert` is never stripped in release builds. If a condition is worth checking, it's worth checking in production. Debug-only assertions create a false sense of safety.
- **Smart `assert` over assertion libraries.** A single `assert` with compile-time expression decomposition replaces `assert_eq`, `assert_ne`, `assert_contains`, `assert_gt`, and every other assertion variant. The compiler shows both values on failure automatically.

### 29.1 Test Blocks

```gorget
test "addition":
    assert add(2, 3) == 5
    assert add(2, 3) != 6
    assert add(0, 0) == 0

@should_panic("division by zero")
test "division by zero":
    divide(1, 0)
```

Tests coexist with `main()` in the same file. `gg test` compiles the test runner; `gg build` ignores test blocks entirely.

### 29.2 Smart Assert (Expression Introspection)

A plain `assert` with a comparison automatically captures and displays both sides on failure:

```gorget
test "string operations":
    String result = greet("world")
    assert result == "hello world"
```

On failure:
```
  test: string operations ... FAIL (0ms)
    assertion failed: left == right
      left:  "hello, world"
      right: "hello world"
```

This works for all types that implement `Formatter` — primitives, strings, enums, structs. No `assert_eq` needed. The compiler rewrites `assert a == b` at the IR level to capture sub-expression values before comparing. This is the pytest/Swift Testing approach, implemented at compile time via Gorget's existing infrastructure.

For non-comparison assertions, the condition expression is included in the failure message:

```gorget
assert items.len() > 0, "data must not be empty"   # custom message
assert is_sorted(data)                               # shows "assertion failed"
```

### 29.3 Postconditions (`assert return`)

Functions can assert properties of their return value. `assert return` checks the condition at every `return` site:

```gorget
int binary_search(List[int] data, int target):
    assert data.len() > 0             # precondition — checked on entry
    assert is_sorted(data)            # precondition — checked on entry
    assert return >= -1               # postcondition — checked at every return
    assert return < data.len()        # postcondition — checked at every return

    # ... body ...
    return idx
```

- Preconditions: regular `assert` statements before any non-assert code.
- Postconditions: `assert return <expr>` — the compiler inserts the check before each `return` statement, with `return` bound to the return value.
- Both are always-on `assert` — they respect `@[debug_only]` and `directive strip-asserts` for hot paths.
- `assert return` is invalid in `void` functions (compile error).
- For tuple returns, `return` is the whole tuple: `assert return.0 <= return.1`.

No new keywords. `assert` and `return` are both existing keywords.

### 29.4 Scoped Resources (`with` Clause)

Tests use the standard `with` statement to bind resources that are automatically cleaned up via `Drop`:

```gorget
test "reads file":
    with File.open("data.txt") as f:
        auto content = f.read_all().unwrap()
        assert content == "expected"
        # f.drop() called automatically on both success and failure
```

For reusable setup, use factory functions:

```gorget
Database make_test_db():
    Database d = Database.open(":memory:")
    d.migrate()
    return d

test "insert":
    with make_test_db() as db:
        db.exec("INSERT INTO t VALUES (1)")
        assert db.count("t") == 1
```

### 29.5 Parameterized Tests via `meta for`

Instead of dedicated parameterized test syntax, use `meta for` to generate test blocks at compile time:

```gorget
meta for name, a, b, expected in [
    ["positives", 1, 2, 3],
    ["zeros", 0, 0, 0],
    ["negatives", -1, 1, 0],
]:
    test "addition - {name}":
        assert a + b == expected
```

This expands to three independent `test` blocks at compile time. Each has its own name, its own pass/fail, its own timing. No special runner infrastructure.

Single-parameter variant:

```gorget
meta for n in [1, 2, 3, 4, 5]:
    test "square of {n}":
        assert n * n > 0
```

Composes with resource bindings:

```gorget
meta for query, expected in [["SELECT 1", 1], ["SELECT 2", 2]]:
    test "query {query}":
        with make_test_db() as db:
            assert db.query_int(query) == expected
```

### 29.6 Suite Setup and Teardown

```gorget
suite setup:
    print("before all tests")

suite teardown:
    print("after all tests")
```

One of each per file. Panics in setup/teardown are fatal.

### 29.7 Tags, Filtering, and Skip

```gorget
@tag("smoke")
test "quick check":
    assert true

@tag("slow")
test "long computation":
    assert true

@skip("not implemented yet")
test "future feature":
    assert false
```

```bash
gg test file.gg --tag smoke            # only tagged tests
gg test file.gg --exclude-tag slow     # skip tagged tests
gg test file.gg --filter "fibonacci"   # name substring
```

### 29.8 Benchmarks

```gorget
bench "vector sort":
    Vector[int] data = random_vec(10000)
    data.sort()

bench "string concat":
    String s = "hello" + " world"
```

```bash
gg test --bench file.gg                # run benchmarks
gg test --bench --filter "sort"        # filter by name
```

Benchmark execution:
1. **Warmup** — 3 iterations to stabilize caches and JIT
2. **Auto-calibrate** — start at 100 iterations, double until total time >= 1 second
3. **Report** — iterations, average time per iteration (auto-scaled: ns, us, ms, s)

Output:
```
Running 2 benchmarks...

  bench: vector sort ... 1200 iters, 832.50 us/iter
  bench: string concat ... 64000000 iters, 19 ns/iter

2 benchmarks complete.
```

Benchmarks use the same body syntax as tests. Suite setup/teardown runs before/after the bench suite. The `--filter` flag works for benchmarks too.

### 29.9 How Familiar Patterns Map to Gorget

Developers coming from other test frameworks will look for features that Gorget handles differently. This table shows how:

| Other frameworks | Gorget equivalent |
|---|---|
| **`assert_eq(a, b)` / `assertEqual`** (pytest, JUnit, Rust) | `assert a == b` — smart assert shows both values on failure. One `assert` replaces all assertion variants. |
| **`@pytest.mark.parametrize` / `test.each` / `@ParameterizedTest`** (pytest, Jest, JUnit) | `meta for` generating `test` blocks. Each row becomes an independent test at compile time. Full control over test names. |
| **Fixtures with DI** (pytest `@pytest.fixture`, yield-based setup/teardown) | `with Expr as name` inside test body + `Drop` trait for per-test resources. Factory functions for reuse. `suite setup`/`suite teardown` for shared state. |
| **`beforeEach` / `afterEach` / `setUp` / `tearDown`** (Jest, JUnit, unittest) | `with` block inside test body handles setup+teardown in one place. `Drop` guarantees cleanup. No split setup/teardown pairs. |
| **`require` / `ensure` / contracts** (D, Eiffel) | `assert` for preconditions (at function top). `assert return` for postconditions. No new keywords — same `assert` used everywhere. |
| **`#[should_panic]`** (Rust) | `@should_panic` / `@should_panic("message substring")`. |
| **`@pytest.mark.skip` / `@Disabled` / `@ignore`** (pytest, JUnit, Rust) | `@skip("reason")`. |
| **Snapshot testing** (`toMatchSnapshot`, `insta`) | `snapshot "name" expr` inside test blocks. `gg test --snapshot save "v1"` to capture, `--snapshot diff "v1" "v2"` to compare. Stored in `.gorget/snapshots/`. |
| **Property-based testing** (Hypothesis, QuickCheck, proptest) | Not yet implemented. Planned as stdlib module with generator combinators. |
| **Doctests** (Rust, Elixir, Python) | Not yet implemented. Planned: code examples in `#/` doc comments compiled and run as tests. |
| **Mocking / stubbing** (Mockito, jest.mock, unittest.mock) | Use trait-based dependency injection + simple test implementations. No mocking framework — design for testability instead. |
| **`describe` / `it` nesting** (Jest, Mocha, RSpec) | Flat `test` blocks. Use naming conventions for grouping: `test "parser - handles empty input"`. Tags for categorization. |
| **Test classes / `TestCase` inheritance** (JUnit 4, unittest) | Not needed. Tests are top-level `test` blocks. No classes, no inheritance, no boilerplate. |
| **`conftest.py` / shared fixture files** (pytest) | Import factory functions from a shared module. Explicit imports, no magic discovery. |
| **Keyword-driven tests** (Robot Framework) | Write Gorget functions as high-level "keywords." The language itself is the keyword library — no separate DSL layer. |

```bash
gg test                            # run all tests
gg test --filter "parse*"          # name filter
gg test --tag slow                 # tag inclusion
gg test --exclude-tag network      # tag exclusion
gg test --bench                    # run benchmarks
gg test --timeout 30s              # global timeout (5s, 500ms, 5000)
gg test --parallel 4               # N worker processes
gg test --failed-only              # re-run only failed tests
gg test --failed-first             # run failed tests first
gg test --format junit-xml         # CI output (planned)
gg test --snapshot save "v1"       # save snapshot version
gg test --snapshot diff "v1" "v2"  # diff two versions (exit 0=same, 1=different)
gg test --snapshot list            # list saved versions
gg test --snapshot show "v1"       # print snapshot contents
gg test --snapshot delete "v1"     # remove a saved version
```

---

## 30. Tricky Indentation Cases (Parser Design)

### 30.1 Long Function Signatures

```gorget
# Continuation with hanging indent
Vector[ProcessedItem] process_all[Processable T](Vector[T] items, Config config,
        Logger &logger) throws ProcessError:
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

### 30.5 Multiline Generic Params with Bounds

```gorget
void complex_fn[Displayable & Cloneable T,
                Into[T] & Debuggable U,
                Iterator[Item = T] V](T a, U b, V c):
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

*Not yet implemented.* The `@cfg` attribute is planned but not yet available. Use `std.os.platform()` for runtime platform checks.

```gorget
@cfg(target_os = "linux")
void platform_init():
    # Linux-specific setup

@cfg(target_os = "windows")
void platform_init():
    # Windows-specific setup

@cfg(debug)
void debug_log(String msg):
    print(f"[DEBUG] {msg}")

@cfg(not(debug))
void debug_log(String msg):
    pass    # no-op in release
```

---

## 34. Build System & Package Management (`gg`) in Detail

### gorget.toml
```toml
[package]
name = "my_project"
version = "0.1.0"

[dependencies]
local-lib = { path = "../my-lib" }
git-dep = { git = "https://github.com/user/repo", tag = "v1.0" }
git-branch = { git = "https://github.com/user/repo", branch = "main" }
```

Dependencies are sourced from a local path or a Git URL; a `gorget.lock` lockfile pins exact commits and is auto-generated. The fetched sources are cached in `~/.gorget/cache/` for offline reproducibility.

### Project Layout
```
my_project/
  gorget.toml               # manifest
  gorget.lock              # lockfile (auto-generated, committed to git)
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
gg new my_project                                    # create project from template
gg build                                             # compile
gg run                                               # compile and run
gg test                                              # run tests
gg check                                             # type-check only (fast)
gg fmt                                               # format code
gg add mylib --git https://github.com/user/repo      # add a git dependency
gg add mylib --path ../mylib                         # add a local path dependency
gg remove mylib                                      # remove a dependency
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

equip[Comparable & Displayable T] Tree[T]:
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
                print(f"{v} ")
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

Record parse_line(String line) throws ProcessError:
    auto parts = line.split(',').collect[Vector[String]]()
    if parts.len() != 2:
        throw ProcessError.InvalidFormat(f"expected 2 fields, got {parts.len()}")
    String name = parts[0].trim().to_string()
    Result[int, String] parsed = parts[1].trim().parse[int]()
    int value = parsed.map_err((e): ProcessError.Parse(f"invalid number: {e}")).unwrap()
    return Record(name, value)

Vector[Record] process_file(Path path) throws ProcessError:
    auto file = fs.File.open(path)           # auto-propagates IoError → ProcessError
    auto reader = BufReader.new(file)
    Vector[Record] records = Vector[Record]()

    for line_result in reader.lines():
        String line = line_result             # auto-propagates
        if line.starts_with('#') or line.is_empty():
            continue
        auto record = parse_line(line)        # auto-propagates
        records.push(record)

    return records

void main():
    Result[Vector[Record], ProcessError] result = process_file(Path.new("data.csv"))
    match result:
        case Ok(records):
            print(f"Processed {records.len()} records")
            int total = records.iter().map(it.value).sum()
            print(f"Total value: {total}")
        case Error(e):
            print(f"Error: {e}")
```

### 36.4 Trait Objects and Dynamic Dispatch

```gorget
trait Animal:
    String name(self)
    String speak(self)

    String describe(self):
        return f"{self.name()} says {self.speak()}"

struct Dog:
    String name

struct Cat:
    String name
    bool indoor

equip Dog with Animal:
    String name(self):
        return self.name

    String speak(self):
        return "woof!"

equip Cat with Animal:
    String name(self):
        return self.name

    String speak(self):
        if self.indoor:
            return "mew"
        return "MEOW!"

void introduce_all(Vector[Box[Animal]] animals):
    for animal in animals:
        print(animal.describe())

void main():
    Vector[Box[Animal]] zoo = Vector[Box[Animal]]()
    zoo.push(Box.new(Dog(String("Rex"))))
    zoo.push(Box.new(Cat(String("Whiskers"), true)))
    zoo.push(Box.new(Dog(String("Buddy"))))
    zoo.push(Box.new(Cat(String("Shadow"), false)))

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
                 [ "throws" type ] ( block | ":" expr NEWLINE | "=" STRING_LITERAL NEWLINE ) ;
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
               | type "(" [ type_list ] ")"  (* function type: int(int, int) *) ;
generic_params   = "[" generic_param { "," generic_param } "]" ;
generic_param    = [ trait_bound_list " " ] IDENT
                 | "const" type IDENT ;
generic_args     = "[" type { "," type } "]" ;
trait_bound_list = trait_bound { "&" trait_bound } ;
trait_bound      = IDENT [ "[" assoc_type_binding { "," assoc_type_binding } "]" ] ;
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
| Lifetime concerns | Fully inferred — no user-facing lifetime syntax |
| Python devs expect GC | Clear docs: "this is not Python, it's Python-shaped Rust" |
| C/Java devs expect null | Option[T] with good sugar (`is`, `?`, `unwrap_or`) |
| Generics `[]` conflicts with indexing | Disambiguated by context: `arr[0]` vs `Vector[int]` (type position vs value position) |
| `auto` hides types, reduces readability | Enforce types at function boundaries; `auto` only for locals |

---

## 39. What Makes Gorget Worth Building?

1. **The "Readable Rust" gap is real**: Many developers want Rust's safety but find the syntax hostile. Gorget's indentation + bare/`&`/`!` borrowing + C-style types could genuinely lower the barrier.

2. **Python developers are the largest audience**: If you can give them memory safety without a GC while keeping the visual style they love, that's a huge unlock.

3. **C/Java type declarations are universal**: `int x = 5` is the most widely-understood variable declaration in programming. Rust's `let x: i32 = 5` is alien by comparison.

4. **Zero-cost abstractions without the ceremony**: Rust's `impl<T: Display + Clone + Send + Sync> Foo<T> for Bar<T>` becomes `equip[Displayable & Cloneable & Sendable & Syncable T] Bar[T] with Foo[T]:` — significantly less visual noise.

---

## 40. Standard Library (Batteries Included)

### 40.1 Philosophy

Gorget ships with a rich standard library — everything you need for common tasks without external dependencies. Like Python and Go, not like Rust.

### 40.2 Module Map

```
std/                     # Core standard library — libc only, always lightweight
├── collections          # Vector, Dict, HashMap, Set, HashSet, Box, Shared, Weak
│                         #   (BTreeMap, LinkedList, VecDeque — *Not yet implemented*)
├── heap                 # Heap — binary min-heap / priority queue
├── iter                 # Lazy Iterator[T] state machines + adapters (take/map/filter/…)
├── fs                   # read_file, write_file, file_exists, mkdir, readdir, …
├── path                 # path_join, path_basename, path_extension, path_parent, …
├── os                   # getenv, getcwd, args, platform, exit, mem_live
├── process              # exec, exec_output, process_spawn
├── signal               # signal_trap, signal_check, SIGINT / SIGTERM / …
├── io                   # readline, input, stdout/stderr/stdin; Writer / Reader / IoError
├── term                 # red/green/bold/…, is_tty, strip_ansi
├── conv                 # int_to_str, parse_int, parse_float, ord, chr; ParseError
├── fmt                  # pad_left, center, join, str_truncate, repeat
├── bytes                # bytes_from_str, bytes_to_hex, base64_encode/decode, endian helpers
├── encoding             # url_encode/decode, html_escape, latin1, utf8 helpers
├── math                 # sqrt, pow, sin, cos, abs, min, max, floor, ceil; PI/E/TAU
├── random               # rand, seed, rand_range
├── time                 # time, time_ms, sleep_ms, format_time
├── datetime             # DateTime (now/utc_now, add_*, diff_*, format, weekday)
├── thread               # thread_spawn, current_thread_id
├── sync                 # AtomicInt/Bool, Barrier, WaitGroup, Semaphore, RWLock, CondVar, OnceFlag
├── channel              # Channel[T] (MPSC): send / recv / recv_timeout / close
├── async                # sleep (cooperative); spawn / await are language keywords (see §10)
├── net.socket           # TCP: socket_connect, server_socket_bind, nb_* async variants
├── net.tls              # TLS: tls_connect, tls_server_bind (OpenSSL)
├── net.udp              # UDP: udp_bind, sendto / recvfrom, multicast
├── hash                 # Hashable, Hasher, FxHasher, hash_of[T]
└── alloc                # Arena, PoolAllocator, TlsfAllocator, TrackingAllocator,
                          #   FixedBufferAllocator, FallbackAllocator

xtd/                     # Extended "batteries included" library
├── http / httpserver         # HTTP/1.1 client (TLS) + server (routing, middleware)
├── json / jsonpath           # JSON parse/stringify + JSONPath queries
├── csv / yaml / toml / xml    # Data formats
├── regex                     # PCRE2-backed regular expressions
├── db / sqlite / influx      # Database traits + SQLite (embedded) + InfluxDB
├── crypto / compress         # SHA/HMAC/AES/Ed25519/X25519/HKDF; zlib/deflate/crc32
├── ssh / p2p                 # SSH2 client; Ed25519/X25519 peer-to-peer
├── tensor / dataframe / math3d   # N-d tensors, DataFrames, Vec/Mat 3D math
├── gfx / sdl / gl / metal / gpu   # 2D wrapper, SDL2, OpenGL, Metal, adaptive GPU
├── image / audio             # stb_image; SDL2_mixer audio
├── ecs                       # Entity component system
└── cli / log / uuid          # Arg parsing, logging, UUID
```

Core traits (`Displayable`, `Cloneable`, `Comparable`, `Equatable`, `Hashable`,
`Default`, `Iterable`, `From`/`TryFrom`, operator traits) and `Option`/`Result`/`Box`
are part of the prelude — always available without an import. Testing (`test` / `bench`
blocks, smart `assert`) is built into the language and compiler (see §29), not a
stdlib module.

### 40.3 Core Traits (auto-imported, always available)

```gorget
# These are always available without import:
trait Displayable       # .display() — human-readable representation
trait Cloneable         # .clone() — deep copy
trait Comparable        # .compare() — total ordering (enables <, >, <=, >=)
trait Equatable         # .eq() — equality (enables ==, !=)
trait Hashable          # .hash(&h) — state-based hashing into a FxHasher (std.hash)
trait Hasher            # write_int / write_bytes / write_string / finish — hash state accumulator
trait Default           # .default() — default value (static method)
trait Iterable          # .iter() — produce an Iterator
trait From[T]           # .from(T) — infallible type conversion (static method)
trait TryFrom[T]        # .try_from(T) — fallible type conversion (static method)
trait Parseable          # .parse(String) — fallible string parsing (static method)
trait Serializable      # .serialize(ser) — serialization (derivable via @derive, import xtd.json)
trait Deserializable    # .deserialize(de) — deserialization (derivable via @derive, import xtd.json)
trait Debuggable        # .debug() — developer/debug representation (derivable via @derive)
trait Sendable          # marker: safe to send across threads (auto-derived)
trait Syncable          # marker: safe to share across threads (auto-derived)
```

"Copy" is a type *category* (the Trivial types of §3.2: int, float, bool, and structs
of them), not a user-facing trait — there is no `Copy` trait to equip. The compiler
classifies a type as Copy automatically based on whether it owns a resource.

### 40.4 Async Runtime (Built-in)

`spawn` and `await` are language keywords, not library calls (see §10). `spawn` returns
a `Task[T]`; `.await()` suspends until it completes. `select:` multiplexes over channels.
Schedulers (`pool`, `thread`, `inline`, `single`) are chosen via `directive scheduler=…`
or `--scheduler`.

```gorget
# Spawning tasks — spawn returns a Task[T], does not block
async void main():
    Task[String] task1 = spawn fetch("https://api.example.com/a")
    Task[String] task2 = spawn fetch("https://api.example.com/b")

    String a = task1.await()
    String b = task2.await()

# Select (respond to whichever channel is ready first)
async void multiplex(Channel[int] urgent, Channel[String] messages):
    loop:
        select:
            case int code = urgent.recv():
                print(f"urgent: {code}")
            case String msg = messages.recv():
                print(msg)

# Channels (std.channel)
async void producer_consumer():
    Channel[int] ch = Channel[int](8)
    spawn produce(ch)               # produces 0..10 then closes

    while ch.recv() is Some(value):
        print(f"Got: {value}")
```

### 40.5 HTTP (Built-in)

```gorget
import xtd.http

# HTTP client — simple
async void fetch_example() throws HttpError:
    # Simple GET
    String body = http.get("https://api.example.com/data").await().text()

    # POST with JSON
    auto resp = http.post("https://api.example.com/users")
        .json(User("Alice", 30))
        .send().await()

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
import xtd.json

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
   - AST -> GIR (monomorphization, drop insertion, closures) -> SSA-based LIR -> BIR (backend IR, expands canonical ops) -> C or LLVM IR
   - C backend (`c-lir`): sole default production backend; LLVM IR backend available via `--backend=llvm`
   - Target: native binaries via a system C compiler or LLVM (optimization handled by the backend)

5. **Phase 5 - Standard Library** (batteries included)
   - Core types, traits, collections, iterators
   - I/O, filesystem, networking, HTTP, JSON
   - Built-in async runtime, threading, sync primitives
   - Regex, crypto, logging, encoding, random

6. **Phase 6 - Tooling**
   - `gg` package management subcommands
   - `gg fmt` formatter
   - LSP server for editor support
   - `gg doc` documentation generator

7. **Phase 7 - Ecosystem**
   - Package registry (foundry?)
   - Community, tutorials, books

### Post-V1 Ideas

- **`--watch` mode** — `gg run --watch` and `gg test --watch` for automatic recompile-and-rerun on file changes. Node/Deno/Bun all ship this now. Essential for a language targeting Python developers who are used to fast iteration loops.
