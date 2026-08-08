# Getting Started

Gorget is a statically typed language with Python-level readability and Rust-level safety.
This chapter gets you from zero to running code.

---

## Installing Gorget

**From pre-built binaries** (macOS and Linux, x86_64 and ARM64):

```bash
curl -fsSL https://raw.githubusercontent.com/ntunes/gorget/main/install.sh | sh
```

**From source** (requires Rust toolchain):

```bash
git clone https://github.com/ntunes/gorget.git
cd gorget
cargo build --release
# Binary is at target/release/gg
```

After installation, verify:

```bash
gg --version
```

---

## Your First Program

Create a file called `hello.gg`:

```gorget
void main():
    print("Hello, World!")
```

That's the whole program. No imports, no boilerplate, no semicolons. The `main`
function is the entry point. `print` is a built-in. Blocks are defined by
indentation.

---

## Building and Running

**Build and run in one step:**

```bash
gg run hello.gg
```

Output:

```
Hello, World!
```

**Build only** (produces a native binary):

```bash
gg build hello.gg
./hello
```

The compiler pipeline: source `.gg` file is lexed, parsed, type-checked, lowered
to an intermediate representation, transpiled to C, and compiled to a native binary.
This all happens behind `gg build`.

---

## The `gg` CLI

The `gg` command is the single tool for everything: building, running, testing,
formatting, and checking code.

| Command | What it does |
|---------|-------------|
| `gg run file.gg` | Compile and execute |
| `gg build file.gg` | Compile to native binary |
| `gg check file.gg` | Type-check without compiling (fast feedback) |
| `gg test file.gg` | Build and run tests |
| `gg fmt file.gg` | Format source code |
| `gg fmt -i file.gg` | Format in place |
| `gg lex file.gg` | Show lexer tokens (debugging) |
| `gg parse file.gg` | Show parsed AST (debugging) |

The most common workflow: edit, `gg run`, repeat. For larger projects: edit,
`gg check` (instant feedback), then `gg build` when ready.

---

## A Slightly Bigger Example

```gorget
int factorial(int n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

void main():
    int result = factorial(10)
    print(f"10! = {result}")
```

```
10! = 3628800
```

Key things to notice:

- **Type-first declarations**: `int n`, not `n: int`. The type comes before the name.
- **Indentation-based blocks**: No braces. A colon starts a block, indentation
  defines it.
- **String interpolation**: `f"{result}"` embeds the variable directly in the string. The `f` prefix is required.
- **No semicolons**: Lines end naturally.

---

## Project Structure

For a single file, just write `file.gg` and run it. For larger projects:

```bash
gg new myproject
```

This creates a `manifest.toml` manifest. Add dependencies with:

```bash
gg add somelib --git https://github.com/user/somelib.git
```

Dependencies are locked in `gorget.lock` for reproducible builds.

---

## What's Next

The next chapter covers Gorget's type system: variables, primitive types, operators,
and how type-first declarations work.
