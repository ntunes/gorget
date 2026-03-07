# Gorget Design Direction

Long term objectives, grouped by pillar. Independent of Gorget's current implementation state.

---

## 🧠 Memory Safety

| Design Targets | Explicitly Avoid |
|---|---|
| Ownership enforced at compile time — no runtime GC pauses | Garbage collector (incompatible with real-time and bare-metal targets) |
| Move semantics with an explicit operator (`!`) — transfers visible at call sites | Implicit heap allocation hidden from the programmer |
| Borrow checking without lifetime annotations in API signatures | Rust-style lifetime annotations leaking into public APIs |
| Scope-guarded references with generation tokens as a safety fallback | Null pointers as a default value for any type |
| Explicit allocator interface — every allocation visible at the call site | Use-after-free and double-free reachable from safe code |
| Stale-condition warnings when shared data crosses suspension points | Silent memory corruption from undefined behavior |
| Distinct newtype / semantic types (UserId ≠ int at compile time) | C-style pointer arithmetic accessible without explicit opt-in |

---

## 🧩 Type System

| Design Targets | Explicitly Avoid |
|---|---|
| Static nominal typing — no accidental structural matches | Type erasure (Java generics — runtime ClassCastException from compile-time types) |
| Full generics with monomorphization — zero-cost at runtime | Implicit numeric coercions (C's int promotions cause subtle bugs) |
| Trait-based composition (`equip...with`) over inheritance hierarchies | Implicit interface satisfaction — equipping should be deliberate |
| Sum types (enums with payloads) + exhaustive pattern matching | Structural typing for everything — leads to accidental duck-type matches |
| Local type inference — annotate at boundaries, infer inside | Orphan rule violations — trait coherence must be enforced |
| Distinct / newtype types to prevent semantic misuse at compile time | `typeof null === 'object'`-style lies — the type system must be honest |
| Integer overflow as an explicit compile-time choice, never silent UB | Dynamic typing as the default path for any core operation |

---

## 🎯 Error Handling

| Design Targets | Explicitly Avoid |
|---|---|
| Errors as typed values — `Result[T, E]` and `Option[T]` as first-class builtins | Exceptions and stack unwinding — hidden control flow from callers |
| Exhaustive pattern matching on error variants — unhandled cases are compile errors | Silent failure (returning false/null/0 with no type-level signal) |
| Error chaining via `with` blocks — no nested match ladders | Exception hierarchies (Java checked/unchecked split created more ceremony than safety) |
| `errdefer`-style cleanup — run resource release only on the error path | Panic as default for recoverable errors (missing map key should never crash) |
| Clear semantic split: panics for invariant violations, errors for expected failures | Swallowing errors with `_` or equivalent without explicit acknowledgment |
| Assertion-first design — asserts active by default, strippable for release | Global error state (errno-style) — non-reentrant and invisible to the type system |
| Error set inference — compiler tracks possible errors without manual listing | Stringly-typed errors (`throw "something went wrong"`) |

---

## ⚡ Concurrency

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

---

## ✍️ Syntax & Expressiveness

| Design Targets | Explicitly Avoid |
|---|---|
| Python-level readability — hello world in 2 lines, no boilerplate ceremony | C-style preprocessor macros — text substitution with no type awareness |
| f-string interpolation — opt-in, type-safe, calls `.to_string()` | Significant whitespace as the only block delimiter (hinders codegen and macros) |
| Exhaustive pattern matching with destructuring and guards | Semicolons as mandatory noise — infer or make optional |
| Comptime / meta system — compile-time computation that replaces macros and codegen scripts | Operator overloading without convention — `<<` meaning stream insertion (C++) |
| Pipe operator (`|>`) — left-to-right composition, no inside-out nesting | Implicit falsy coercions (`0 == false`, `"" == false`) |
| `for/else` and `while/else` — loop-completion semantics without extra flags | Verbosity for its own sake — names earn their length |
| Uniform Function Call Syntax — `arr.filter(f)` and `filter(arr, f)` are identical | Magic / spooky action at a distance — behavior visible at the call site or nowhere |
| | Multiple incompatible ways to do the same common task |

---

## 🔧 Tooling

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

---

## 🔗 C Interop & FFI

| Design Targets | Explicitly Avoid |
|---|---|
| C as the compilation target — portable, zero new backend dependency | JVM / managed runtime requirement — kills the bare-metal story |
| C ABI compatibility — generated symbols follow C calling conventions | Hidden runtime dependencies — hello world must not silently link a 150kB runtime |
| Explicit `unsafe` blocks — auditable regions where safety guarantees are suspended | Name mangling without an escape hatch — FFI must produce predictable C names |
| | Incompatible calling conventions without explicit annotation |
| WASM target — browser and edge compute deployment path | Requiring manual translation layers to call existing C libraries |
| Embedded / no-stdlib mode — bare runtime, no OS primitives required | Platform-specific ABI surprises — stdcall/cdecl confusion |

---

## 📚 Standard Library

| Design Targets | Explicitly Avoid |
|---|---|
| Collections (generic Vector, Dict, Set, Heap) with full ownership semantics | 30 competing ways to do the same thing — one canonical form per common task |
| String handling with explicit byte/codepoint distinction (no silent encoding bugs) | Inconsistent naming conventions — stdlib API should follow one rule |
| HTTP client + server in stdlib — not an external package | Requiring a package manager for basics (JSON, HTTP, math) |
| JSON, CSV, XML out of the box | Mutable global state in stdlib functions (errno-style non-reentrancy) |
| SQLite with zero external dependencies | Stdlib functions that silently return wrong results on edge cases |
| DateTime with explicit timezone handling | Deprecated functions that cannot be removed due to backwards-compat promises |
| Cryptographic primitives (hash, random) — not a third-party concern | Leaking C implementation details through the stdlib API surface |
| Arena / explicit allocator in stdlib — not just malloc-under-the-hood | Stdlib that only works with the default allocator |

---

## 🌱 Learnability & Ergonomics

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
