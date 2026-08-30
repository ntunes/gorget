# Interop and the C Backend

Sometimes you need to call a C library. Gorget does that through `extern` blocks —
declarations that name a foreign symbol and the Gorget types it is marshalled
through. There is no escape hatch to go with them: a foreign function is type
checked, ownership tracked, and borrow checked like any other, so calling C does
not switch off the rest of the language.

---

## Extern Blocks

Declare foreign functions with `extern`:

```gorget
extern "C":
    int abs(int x)
    int atoi(cstr s)
```

The string `"C"` specifies the ABI, and it selects how arguments are marshalled:
`String` parameters are passed as `const char*`, and the `cstr` type is only
meaningful inside an `extern "C"` block.

### Binding to C Symbols

Map a Gorget function name to a C symbol:

```gorget
extern int llabs_wrapper(int x) = "llabs"

void main():
    int a = llabs_wrapper(-42)
    print(f"{a}")    # 42
```

The `= "llabs"` tells the compiler to link against the C function `llabs` but
expose it in Gorget as `llabs_wrapper`.

### Calling C Libraries

```gorget
extern "C":
    int open(cstr path, int flags)
    int close(int fd)
```

Extern functions are automatically available after declaration. The linker
resolves them against system libraries.

Declarations are top-level: an `extern` block sits alongside your functions, not
inside one. Wrapping a foreign call in an ordinary Gorget function is the usual
way to give it a typed, idiomatic signature:

```gorget
extern "C":
    int abs(int x)

# Callers use the wrapper and never touch the foreign declaration
int distance_from_origin(int x):
    return abs(x)

void main():
    print(f"{distance_from_origin(-42)}")
```

---

## The C Backend

Gorget compiles by transpiling to C, then invoking a C compiler:

```
source.gg → lexer → parser → type checker → IR → C code → cc → binary
```

The generated C code is a single `.c` file containing all functions, types, and
runtime support. You don't normally see it, but it's there:

```bash
gg build app.gg           # produces: app (binary) and app.c (intermediate)
```

### Implications

- **Portability**: Runs anywhere a C compiler runs
- **Interop**: C libraries link directly — no FFI bridge
- **Debugging**: You can inspect the generated C for low-level issues
- **Performance**: The C compiler (GCC/Clang) handles optimization

### Compiler Flags

Pass flags through to the C compiler or control the pipeline:

```bash
gg build app.gg --sanitize          # AddressSanitizer + UBSan
gg build app.gg --emit-c-lir        # dump generated C to stdout
gg build app.gg --shared            # build as shared library
gg build app.gg --hot-reload        # build for hot code reloading
```

`--sanitize` instruments the whole program here, because on this backend your
code *is* C. That is not true of `--backend=llvm`, where only the runtime is
instrumented and user-code faults go unreported — see
[Appendix: CLI](appendix-cli.md#gg-build-filegg). The C backend is the lane to
reach for when you are chasing a memory bug.

---

## Hot Reload

For development workflows, Gorget supports hot code reloading:

```gorget
directive hot-reload
```

```bash
gg build app.gg --hot-reload
```

This builds a host binary and a guest dynamic library. The guest can be rebuilt
and reloaded at runtime without restarting the host — useful for game development,
UI iteration, and live-coding workflows.

---

## Summary

| Feature | Syntax | Purpose |
|---------|--------|---------|
| Extern block | `extern "C": declarations` | Declare foreign functions |
| Symbol binding | `extern T f(args) = "symbol"` | Map to C symbol name |
| C backend | `gg build` pipeline | Transpile to C, compile to native |
| Sanitizers | `--sanitize` | Runtime bug detection (fully on the C backend; leak- and runtime-side only on `--backend=llvm`) |
| Hot reload | `directive hot-reload` | Reload code at runtime |
