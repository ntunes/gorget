# Gorget

Gorget is a statically typed, Python-like language with Rust-inspired ownership and borrow checking.

> **Note:** Gorget is highly experimental. Expect breaking changes, missing features, and the possibility of a complete redesign before version 1.0.0.

```
void main():
    print("Hello, World!")
```

```
$ gg run hello.gg
Hello, World!
```

## Features

- **Indentation-based syntax** with static typing and type inference
- **Compiles to C** via transpilation, then to native code via `cc`
- **Semantic analysis** including name resolution, type checking, trait registry, and borrow checking
- **Generics** with monomorphization
- **Traits** with dynamic dispatch via vtables
- **Pattern matching** with guards, destructuring, and or-patterns
- **For/else and while/else** loops (else runs when loop completes without `break`)
- **Closures**, comprehensions, error handling, and more

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

### Write a program

```bash
cat > hello.gg << 'EOF'
void main():
    auto name = "Gorget"
    print("Hello, {name}!")
EOF
```

### Compile and run

```bash
# Two-step: compile then run
gg build hello.gg
./hello

# Or one-step:
gg run hello.gg
```

## Language Tour

### Functions

```
int add(int a, int b):
    return a + b

# Expression-body shorthand
int double(int x) = x * 2

int factorial(int n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### Variables and Type Inference

```
int x = 10
const int y = 20
auto name = "gorget"     # inferred as str
auto pi = 3.14           # inferred as float
auto flag = true          # inferred as bool
```

### Control Flow

```
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")

for i in 0..10:
    print("{i}")

# For/else: else runs only if loop completes without break
for i in 0..100:
    if i == 42:
        break
else:
    print("not found")
```

### Structs and Enums

```
struct Point:
    int x
    int y

enum Color:
    Red
    Green
    Blue

Color c = Red()
match c:
    case Red:
        print("red")
    case Green:
        print("green")
    case Blue:
        print("blue")
```

### Generics

```
struct Pair[A, B]:
    A first
    B second

Pair[int, int] p = Pair[int, int](10, 20)
```

### Traits

```
trait Shape:
    int area(self)

struct Circle:
    int radius

equip Circle with Shape:
    int area(self):
        return 3 * self.radius * self.radius
```

### Closures

```
int x = 10
int(int) add_x = (int y): x + y
```

### Ownership & Borrowing

Gorget uses Rust-style ownership to prevent use-after-free and double-free bugs at compile time, with zero runtime cost.

```
# Move: transfers ownership — caller can't use msg after this
void send(Message !msg):
    print("sent")

# Borrow: read-only access — caller keeps ownership
void preview(Message msg):
    print("peek")

# Mutable borrow via method — can modify, caller keeps ownership
equip Message:
    void set_priority(&self, int p):
        self.priority = p
```

```
Message msg = Message("Alice", "Hello", 1)
preview(msg)        # borrow — msg still alive
msg.set_priority(5) # mutable borrow — modify in place
send(!msg)          # move — msg is now dead
# preview(msg)      # compile error: use of moved value `msg`
```

Copy types (int, float, bool) pass freely without `!`. See [`examples/ownership.gg`](examples/ownership.gg) for a full walkthrough.

## CLI Commands

| Command | Description |
|---------|-------------|
| `gg lex <file>` | Tokenize and print tokens |
| `gg parse <file>` | Parse and print AST |
| `gg check <file>` | Run semantic analysis |
| `gg build <file>` | Compile to native binary |
| `gg run <file>` | Build and run in one step |

## Testing

```bash
cargo test --lib                                    # 249 unit tests
cargo test --test integration -- --test-threads=1   # 102 integration tests
```

Integration tests live in `tests/fixtures/*.gg` — each is a self-contained program with deterministic stdout.

## Documentation

- [Language Reference](docs/language-reference.md) — full syntax and semantics specification
- [Language Design](docs/language-design.md) — design philosophy, safety features, and rationale

## Architecture

```
.gg source → Lexer → Parser → Semantic Analysis → C Codegen → cc → Binary
```

| Stage | Directory | Description |
|-------|-----------|-------------|
| Lexer | `src/lexer/` | Logos-based tokenizer with indentation tracking |
| Parser | `src/parser/` | Recursive descent parser producing AST |
| Semantic | `src/semantic/` | Name resolution, type checking, traits, borrow checking |
| Codegen | `src/codegen/` | C code generation (GCC extensions) |

## License

MIT
