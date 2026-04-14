# Gorget

Rust-grade memory safety. Python-grade readability. No GC, no lifetime annotations, no ceremony.

Gorget is a statically typed, compiled language that uses ownership and borrowing to manage memory at compile time — but without the annotation tax. Programs compile to native binaries through C. Moves and borrows are marked at call sites (`!` and `&`), so ownership transfers are visible where they happen, not buried in type signatures.

```gorget
struct Message:
    String sender
    String subject
    int priority

void preview(Message msg):           # immutable borrow — caller keeps ownership
    print("[Preview] {msg.subject}")

void send(Message !msg):             # move — ownership transfers, msg is consumed
    print("[Sent] {msg.subject}")

void main():
    Message msg = Message("Alice", "Meeting tomorrow", 1)
    preview(msg)      # borrow — msg still alive
    send(!msg)        # move — msg is now dead
    # preview(msg)    # compile error: use of moved value `msg`
```

```
$ gg run mail.gg
[Preview] Meeting tomorrow
[Sent] Meeting tomorrow
```

> Gorget is in active development. The language is expressive enough to self-host its own lexer, parser, and type checker (comparison tests pass at 100% against the Rust originals), but it hasn't seen production use yet. Expect breaking changes before 1.0.

## Why Gorget

|  | Gorget | Rust | Go | Python |
|--|--------|------|----|--------|
| Memory safety | Compile-time ownership | Compile-time ownership | GC | GC |
| Lifetime annotations | None | Required for complex borrows | N/A | N/A |
| Error model | Auto-propagating `throws` | `Result` + `?` at every call | `if err != nil` | Exceptions |
| Null safety | `Option[T]` — no nulls | `Option<T>` — no nulls | Nil pointers | `None` everywhere |
| Concurrency | Compiler-checked `shared` | `Send`/`Sync` traits | Goroutines + channels | GIL |
| Compile target | C → native binary | LLVM → native binary | Go toolchain | Interpreted |
| Syntax | Indentation-based | Braces | Braces | Indentation-based |
| Generics | Monomorphized | Monomorphized | Type-erased | Dynamic |

We think safe languages shouldn't have to be verbose. Here's what that means in practice:

- **Ownership without lifetime annotations** — the borrow checker works without lifetime parameters in function signatures. Borrows and moves are marked at call sites (`&` for mutable borrow, `!` for move), so ownership transfers are visible where they happen.
- **Error handling without noise** — functions declare `throws` and errors propagate automatically. No `?` at every call site, no `try` blocks wrapping your logic, no `if err != nil`. When you do need control, `catch`, `rethrow`, and `on error` give you exactly the level of handling you want.
- **Compiler-checked concurrency** — `shared int count = 0` gives you thread-safe mutable state. The compiler selects the right synchronization primitive (atomic, mutex, or rwlock), prevents deadlocks by enforcing consistent lock ordering, and warns about stale reads, check-then-act races, and lost updates — all at compile time.
- **Compile-time meta system** — `meta` blocks run at compile time with full type introspection: iterate fields, expand variants, conditionally compile. No macros, no codegen scripts — just the language itself.
- **Automatic purity inference** — every function is classified as Pure, ReadOnly, MutatesArgs, or HasSideEffects through call-graph analysis. No annotations needed.
- **Contracts** — `assert return` lets you write postconditions directly in function bodies. Active by default, strippable for release.
- **Rich static analysis** — warns on unchecked `.unwrap()`, variables that could be `const`, unnecessarily mutable borrows, unused imports, unreachable code, and suggests corrections for typos.
- **Batteries included** — HTTP, JSON, CSV, XML, YAML, TOML, SQLite, regex, crypto, and more ship in the standard library.
- **One toolchain** — `gg build`, `gg test`, `gg fmt`, `gg sim`, `gg add`. No external build system, no formatter choice, no test framework decision.

## A Taste of the Language

### Functions and type inference

```gorget
int add(int a, int b):
    return a + b

# Expression-body shorthand
int double(int x) = x * 2

# Type inference for locals
auto result = add(10, double(5))
```

### Error handling

Errors propagate automatically — you only write code where you want control:

```gorget
# Errors propagate up through throwing functions automatically
Config load_config(String path) throws String:
    String content = read_file(path)  # throws? we throw too — automatic
    Config cfg = parse(content)       # same here
    return cfg

# catch — recover with a fallback
void main():
    int port = parse_port(input) catch: 8080

# rethrow — transform errors with context
Config load(String path) throws AppError:
    String content = read_file(path) rethrow (String e): AppError.Io("reading {path}: {e}")
    return parse(content) rethrow (String e): AppError.Parse(e)

# on error — cleanup that only runs on failure (like Zig's errdefer)
void process(String path) throws String:
    File f = File.open(path)
    on error f.close()
    String data = f.read_all()        # if this throws, f.close() runs
    transform(data)

# raw — drop to manual Result handling when you need full control
void main():
    match raw load_config("app.conf"):
        case Ok(cfg):
            serve(cfg)
        case Error(e):
            print("failed: {e}")
```

### Pattern matching

```gorget
enum Shape:
    Circle(float radius)
    Rect(float w, float h)

float area(Shape s):
    match s:
        case Circle(r):
            return 3.14159 * r * r
        case Rect(w, h):
            return w * h
```

### Shared variables

Thread-safe mutable state with automatic synchronization — the compiler selects the primitive, prevents deadlocks, and warns about races:

```gorget
async void main():
    shared int count = 0

    Task[void] t1 = spawn increment(&count, 1000)
    Task[void] t2 = spawn increment(&count, 1000)
    t1.await()
    t2.await()

    with count:
        print(count)    # 2000 — lock held, value is fresh

async void increment(int &count, int n):
    for i in 0..n:
        count += 1      # auto-locked, incremented, auto-unlocked
```

### Compile-time meta

```gorget
# Derive common traits — no boilerplate
@derive(Equatable, Displayable, Cloneable)
struct Point:
    float x
    float y

# Iterate fields at compile time
void describe_fields[T]():
    meta for fname, ftype in fields(T):
        print(f"{fname}:{ftype}")

# Conditional compilation over types
void count_numeric_fields[T]():
    auto count = 0
    meta for fname, ftype in fields(T):
        meta if ftype is numeric:
            count += 1
    print(count)
```

### Contracts

```gorget
int clamp(int value, int lo, int hi):
    assert lo <= hi                    # precondition
    assert return >= lo                # postcondition
    assert return <= hi                # postcondition

    if value < lo: return lo
    if value > hi: return hi
    return value
```

### Traits

```gorget
trait Printable:
    String describe(self)

struct Circle:
    int radius

equip Circle with Printable:
    String describe(self):
        return "circle with radius {self.radius}"
```

### Closures and higher-order functions

```gorget
from std.collections import Vector

void main():
    Vector[int] nums = [1, 2, 3, 4, 5]
    Vector[int] evens = nums.filter((int x): x % 2 == 0)
    int total = nums.fold(0, (int acc, int x): acc + x)
```

### Async and channels

```gorget
from std.sync import Channel

async void producer(Channel[int] ch, int count):
    for i in 0..count:
        ch.send(i)
    ch.close()

async int consumer(Channel[int] ch, int count):
    int total = 0
    for i in 0..count:
        total = total + ch.recv()
    return total

async void main():
    Channel[int] ch = Channel[int](2)
    Task[void] prod = spawn producer(ch, 10)
    Task[int] cons = spawn consumer(ch, 10)
    prod.await()
    int result = cons.await()
    print(result)   # 45
```

## Install

```bash
curl -fsSL https://raw.githubusercontent.com/ntunes/gorget/main/install.sh | sh
```

Or install a specific version:

```bash
curl -fsSL https://raw.githubusercontent.com/ntunes/gorget/main/install.sh | VERSION=v0.1.0 sh
```

Binaries are available for macOS (ARM64, x86_64) and Linux (x86_64, ARM64).

### Build from source

```bash
cargo build --release
```

## Quick Start

```bash
cat > hello.gg << 'EOF'
void main():
    auto name = "Gorget"
    print("Hello, {name}!")
EOF

gg run hello.gg
```

## CLI

| Command | Description |
|---------|-------------|
| `gg <file>` | Compile and run (same as `gg run`) |
| `gg run <file>` | Compile and run |
| `gg build <file>` | Compile to native binary |
| `gg test <file>` | Run tests |
| `gg check <file>` | Semantic analysis only (fast feedback for editors) |
| `gg fmt <file>` | Format source code |
| `gg sim <file>` | Interpret / simulate (runtime checking, no compile) |
| `gg lex <file>` | Tokenize and print tokens |
| `gg parse <file>` | Parse and print AST |
| `gg report <trace>` | Generate HTML report from trace file |
| `gg` | Interactive REPL |

### Package commands

| Command | Description |
|---------|-------------|
| `gg init` | Initialize a new project in the current directory |
| `gg new <name>` | Create a new project directory |
| `gg add <pkg>` | Add a dependency |
| `gg remove <pkg>` | Remove a dependency |

### Build flags

| Flag | Description |
|------|-------------|
| `--strip-asserts` | Remove all `assert` statements |
| `--no-strip-asserts` | Keep asserts even if source has `directive strip-asserts` |
| `--overflow=wrap` | Enable wrapping arithmetic (no overflow panic) |
| `--overflow=checked` | Force checked arithmetic even if source says `wrap` |
| `--hot-reload` | Enable hot code reload (builds host + guest shared library) |
| `--shared [-o F]` | Build as shared library (.dylib/.so) |
| `--sanitize` | Enable AddressSanitizer + UBSan |
| `--emit-gir` | Dump GIR (intermediate representation) to stdout |
| `--emit-lir` | Dump LIR (low-level SSA IR) to stdout |

### Test flags

| Flag | Description |
|------|-------------|
| `--trace` | Enable execution tracing |
| `--no-trace` | Disable tracing even if source has `directive trace` |
| `--report html` | Generate HTML report (implies `--trace`) |
| `--filter <substr>` | Only run tests whose name contains `<substr>` |
| `--tag <name>` | Only run tests matching this tag (repeatable) |
| `--exclude-tag <name>` | Skip tests with this tag (repeatable) |
| `--timeout <ms>` | Per-test timeout in milliseconds |
| `--parallel <n>` | Run tests across `n` parallel workers |

## Testing

```bash
# Run all tests in a file
gg test my_tests.gg

# Filter and tag
gg test my_tests.gg --filter "fibonacci"
gg test my_tests.gg --tag "slow" --exclude-tag "integration"

# Generate HTML report with execution traces
gg test my_tests.gg --report html
```

Test files use `test` blocks with `assert` for contracts, `@should_panic` for expected failures, `suite setup:` / `suite teardown:` for lifecycle hooks, and `with` bindings for scoped resource management.

## Development

```bash
cargo build                                        # build the compiler
cargo test --lib                                   # ~970 unit tests
cargo test --test integration -- --test-threads=4  # ~960 integration tests
```

Integration tests live in `tests/fixtures/*.gg` — each is a self-contained program with deterministic stdout.

## Architecture

```
.gg source → Lexer → Parser → Semantic Analysis → IR Lowering → C Backend → cc → Binary
```

| Stage | Directory | Description |
|-------|-----------|-------------|
| Lexer | `src/lexer/` | Logos-based tokenizer with indentation tracking |
| Parser | `src/parser/` | Recursive descent parser producing AST |
| Semantic analysis | `src/semantic/` | Name resolution, type checking, traits, borrow checking |
| IR lowering | `src/ir/` | Monomorphization, drop insertion, closure conversion |
| C backend | `src/backend/c/` | GIR to C code generation |
| LIR backend | `src/backend/c_lir/` | SSA-based backend (next-gen, in A/B testing) |
| Formatter | `src/formatter/` | Source formatter (`gg fmt`) |
| Simulator | `src/sim/` | Interpreter with runtime safety checks (`gg sim`) |

## Documentation

- [The Gorget Book](docs/book/README.md) — learn the language from scratch (assumes programming experience, not Gorget experience)
- [Language Reference](docs/language-reference.md) — full syntax and semantics specification
- [Language Design](docs/language-design.md) — design philosophy, safety features, and rationale

## License

MIT
