# Interop and Unsafe

Sometimes you need to call C libraries, use raw pointers, or bypass the compiler's
safety checks. Gorget supports this through `extern` blocks and `unsafe` — clearly
marked escape hatches that don't compromise safety in the rest of your code.

---

## Extern Blocks

Declare foreign functions with `extern`:

```gorget
extern "C":
    int abs(int x)
    void free(RawPtr[void] ptr)
```

The string `"C"` specifies the ABI. It's optional — C is the default:

```gorget
extern:
    int abs(int x)
```

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
extern:
    int open(String path, int flags)
    int read(int fd, RawPtr[void] buf, int count)
    int close(int fd)
```

Extern functions are automatically available after declaration. The linker
resolves them against system libraries.

---

## Unsafe Blocks

The `unsafe` keyword opts into operations the compiler can't verify:

```gorget
unsafe:
    int value = *raw_ptr          # raw pointer dereference
    c_function()                  # FFI call
    GLOBAL_COUNTER += 1           # mutable static access
```

### What Unsafe Allows

- **Raw pointer dereferencing**: `*ptr` where `ptr` is a `RawPtr[T]`
- **FFI calls**: Calling extern functions
- **Mutable static access**: Modifying global state

### What Unsafe Does NOT Disable

Unsafe doesn't turn off the type system, the borrow checker, or ownership rules.
It only unlocks the specific operations listed above. An `unsafe` block with normal
Gorget code is still fully checked:

```gorget
unsafe:
    int x = 5         # still type-checked
    String s = "hi"   # still ownership-tracked
    *raw_ptr           # THIS is the unsafe part
```

### Minimizing Unsafe

Wrap unsafe operations in safe abstractions:

```gorget
int safe_abs(int x):
    extern int abs(int x) = "abs"
    unsafe:
        return abs(x)

# All callers use the safe wrapper — no unsafe needed
void main():
    print(f"{safe_abs(-42)}")
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

## Raw Pointers

`RawPtr[T]` is the unsafe pointer type:

```gorget
unsafe:
    RawPtr[int] ptr = ...
    int value = *ptr              # dereference
```

Raw pointers bypass all safety checks. They exist for FFI and low-level memory
manipulation. In normal Gorget code, you never need them.

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
| Unsafe block | `unsafe: operations` | Bypass compiler safety checks |
| Raw pointer | `RawPtr[T]` | Unmanaged pointer (FFI) |
| C backend | `gg build` pipeline | Transpile to C, compile to native |
| Sanitizers | `--sanitize` | Runtime bug detection (fully on the C backend; leak- and runtime-side only on `--backend=llvm`) |
| Hot reload | `directive hot-reload` | Reload code at runtime |
