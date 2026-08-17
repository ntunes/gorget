# The Gorget Book

A guide to the Gorget programming language for working programmers.

This book assumes you know how to program — you've written functions, used loops, and
debugged a segfault or two. It does not assume you know Rust, Python, or any particular
language. When Gorget borrows an idea from another language, it says so, but you don't
need prior exposure to follow along.

---

## Table of Contents

### Part I — Foundations

1. **[Getting Started](01-getting-started.md)**
   Installing Gorget, your first program, the `gg` CLI, building and running.

2. **[Variables, Types, and Operators](02-types.md)**
   Primitive types, type-first declarations, `auto` inference, arithmetic, comparison,
   logical operators, type casting.

3. **[Control Flow](03-control-flow.md)**
   `if`/`elif`/`else`, `for` loops and ranges, `while`, `loop`, `break`/`continue`,
   `match`/`case` with pattern matching, `pass`.

4. **[Functions](04-functions.md)**
   Defining functions, return types, parameters, expression-body shorthand,
   multiple return values, closures.

5. **[Strings and Collections](05-collections.md)**
   The `String` type (unified, provenance-inferred), f-string interpolation,
   `Vector`, `Dict`, `HashMap`, `Set`, `HashSet`, arrays, tuples, slices,
   comprehensions, higher-order methods (`map`, `filter`, `fold`, `reduce`,
   `any`, `all`, `sort`), built-in functions (`enumerate`, `zip`, `type`). Integer
   sequences use `..` / `..=` (see Chapter 3).

6. **[Structs and Enums](06-structs-enums.md)**
   Defining structs, field access, methods via `equip`, enums with payloads,
   qualified variant access, dot-shorthand.

### Part II — The Type System

7. **[Traits](07-traits.md)**
   Defining traits, equipping types with traits, default methods, trait inheritance,
   trait delegation (`via`), built-in traits, `@derive`.

8. **[Generics](08-generics.md)**
   Type parameters, trait bounds, monomorphization, generic structs, generic
   functions, generic enums, implementing traits for generic types.

9. **[Option and Result](09-option-result.md)**
   Null safety with `Option[T]`, error values with `Result[T, E]`, combinators
   (`map`, `and_then`, `filter`, `flatten`), optional chaining (`?.`), default
   operator (`??`).

10. **[Error Handling](10-errors.md)**
    The `throws` model, auto-propagation, `rethrow`, `on error` cleanup,
    Result capture, `throws int` on main, error types, panics vs. errors.

### Part III — Ownership

11. **[Ownership and Move Semantics](11-ownership.md)**
    Every value has one owner, move vs. copy, the `!` operator,
    scope and drop, `with` for resource management.

12. **[Borrowing and References](12-borrowing.md)**
    Immutable borrows, mutable borrows (`&`), the borrow checker rules,
    auto-borrowing at call sites, and lifetime inference.

### Part IV — Concurrency

13. **[Tasks and Async/Await](13-async.md)**
    Async functions, `spawn`, `Task[T]`, awaiting results, suspension points,
    what can cross an `await`, schedulers.

14. **[Concurrency](14-concurrency.md)**
    Channels, shared variables, `with` blocks, coordination patterns,
    what the compiler catches.

### Part V — Advanced Topics

15. **[Modules and Imports](15-modules.md)**
    The module system, `import` and `from`/`import`, visibility (`public`),
    file-based modules, the standard library.

16. **[Smart Pointers](16-smart-pointers.md)**
    `Box[T]`, `Shared[T]`, `Weak[T]`, `Mutex[T]`, `RwLock[T]`,
    when to use each.

17. **[Meta Programming](17-meta.md)**
    Compile-time constants, `meta if`, `meta for`, type predicates,
    struct/enum reflection, meta type functions, feature flags.

18. **[Testing](18-testing.md)**
    `test` blocks, assertions, `@should_panic`, `@tag`, suite setup/teardown,
    `with` resources, test filtering, HTML reports.

19. **[The Standard Library](19-stdlib.md)**
    Core `std.*` modules: file system, path ops, OS interface, process management,
    signals, I/O, terminal colors, type conversions, string formatting, bytes,
    encoding, math, time/datetime, collections, concurrency (threads, channels,
    sync primitives), networking (TCP/TLS/UDP), memory allocators.

20. **[The Extended Library](20-xtd.md)**
    Batteries-included `xtd.*` modules: HTTP client and server, JSON/YAML/TOML/XML/CSV,
    JSONPath queries, regex, databases (SQLite, InfluxDB), crypto, compression,
    SSH, P2P, tensors, dataframes, 3D math, graphics (SDL2/OpenGL/Metal/GPU),
    image processing, audio, ECS, CLI parsing, logging, UUID.

21. **[Interop and Unsafe](21-interop.md)**
    `extern` blocks, calling C from Gorget, `unsafe` blocks, the C backend,
    compilation model.

22. **[Build Targets](22-targets.md)**
    The `--target` flag, native vs freestanding, architecture selection,
    UEFI bare-metal applications, the `gg.fb` framebuffer API.

### Appendices

- **[A — Operator Precedence](appendix-operators.md)**
- **[B — Built-in Traits Reference](appendix-traits.md)**
- **[C — CLI Reference](appendix-cli.md)**
- **[D — Directives](appendix-directives.md)**
- **[E — Formatting and Code Style](appendix-formatting.md)**

---

*This book is a work in progress. Chapters are added as the language evolves.*
