# Gorget

A compiled language where safe code looks clean and clean code is safe.

Gorget compiles to native binaries through C. It enforces ownership and borrowing at compile time — no garbage collector, no runtime cost — while keeping the syntax minimal enough that programs read like pseudocode.

```
void main():
    print("Hello, World!")
```

```
$ gg run hello.gg
Hello, World!
```

> Gorget is in active development. Expect breaking changes before 1.0.

## Why Gorget

Most safe languages pay for safety with syntax. Gorget doesn't.

- **Ownership without annotations** — the borrow checker works without lifetime parameters in function signatures. Borrows and moves are visible at call sites (`&` for mutable borrow, `!` for move), not buried in type signatures.
- **Compile-time meta system** — no macros, no codegen scripts. `meta` blocks run at compile time with full access to type introspection: field iteration, variant expansion, conditional compilation. Derive traits, generate match arms, embed files — all in the language itself.
- **Batteries included** — HTTP, JSON, CSV, XML, YAML, TOML, SQLite, regex, crypto, and more ship in the standard library. One import, no package hunting for basics.
- **One toolchain** — `gg build`, `gg test`, `gg fmt`, `gg sim`, `gg add`. No external build system, no formatter choice, no test framework decision.

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

### Error handling

```gorget
from gg.http import get, HttpResponse

void main():
    Result[HttpResponse, str] result = get("https://httpbin.org/get")
    match result:
        case Ok(resp):
            print("Status: {resp.status_code}")
        case Error(e):
            print("Error: {e}")
```

### Ownership and borrowing

```gorget
struct Message:
    str sender
    str subject
    int priority

void preview(Message msg):           # borrow — read-only, caller keeps ownership
    print("[Preview] {msg.subject}")

equip Message:
    void set_priority(&self, int p): # mutable borrow — modify, caller keeps ownership
        self.priority = p

void send(Message !msg):             # move — ownership transfers to callee
    print("[Sent] {msg.subject}")

void main():
    Message msg = Message("Alice", "Meeting tomorrow", 1)
    preview(msg)         # borrow — msg still alive
    msg.set_priority(5)  # mutable borrow — modify in place
    send(!msg)           # move — msg is now dead
    # preview(msg)       # compile error: use of moved value `msg`
```

### Traits

```gorget
trait Printable:
    str describe(self)

struct Circle:
    int radius

equip Circle with Printable:
    str describe(self):
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
cargo test --lib                                   # ~913 unit tests
cargo test --test integration -- --test-threads=4  # ~709 integration tests
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
