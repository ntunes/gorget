# Proposal: Extern Module ABI Declarations

**Author:** nantunes + claude
**Date:** 2026-04-01
**Status:** Draft

## Problem

Gorget's compiler does not model the ABI boundary between Gorget code and external (runtime/FFI) functions. This causes three concrete problems:

### 1. The compiler guesses how to marshal function arguments

When Gorget code calls an external function with a `String` argument, the compiler must decide how to pass it across the boundary. The external function might expect:

- A **null-terminated byte pointer** (`const char*` in C, `i8*` in LLVM, `i32` offset in WASM) — used by regex, SDL, file I/O, most C libraries
- A **raw byte pointer + length** — used by binary protocols, socket writes
- The **full Gorget string struct** by value — used by Gorget-aware runtime functions like sqlite wrappers
- An **opaque handle** passed through unchanged — used by collection methods, closure dispatch

Today the compiler makes this decision through hand-maintained whitelists in the C backend:

- `takes_cstr_for_str_param()` — 40+ hardcoded function names that take `const char*`
- `needs_null_terminated_cstr()` — 30+ hardcoded function names that need `\0` termination
- `runtime_arg_by_ptr()` — functions that take struct args by pointer
- `runtime_extern_sig()` — 100+ canonical function signatures

These whitelists are:

- **Incomplete** — new runtime functions require manual additions (gorget-arena's SDL/GL functions were missing)
- **C-specific** — an LLVM or WASM backend would need its own parallel lists
- **Fragile** — a typo in a function name silently produces wrong code
- **Scattered** — the ABI knowledge lives in Rust codegen code, not in the module declarations

### 2. CoW string unification exposed the debt

Before string unification, `String` values were passed as 32-byte structs. A `void*` pointer to a Str struct happened to have its `.data` field at offset 0, so passing `Str*` as `const char*` accidentally worked in many cases — the C compiler read the first 8 bytes (the data pointer) as the char pointer.

After CoW, strings are 8-byte pointers to Str structs. Passing this pointer as `const char*` gives the C function the address of the struct, not the address of the character data. Every runtime function that takes `const char*` broke unless explicitly listed in the whitelist.

### 3. No path to alternative backends

An LLVM backend needs typed function declarations:

```llvm
declare void @gorget_regex_find(%Regex*, i8*, i64)  ; i8* = const char*
declare void @gorget_sqlite_open(%Str)               ; %Str = struct by value
```

The backend must emit different IR depending on whether a `String` parameter should become `i8*` or `%Str`. Today, this information exists only in C backend whitelists. A WASM backend would face the same problem with different representations.

The ABI boundary knowledge must live in a backend-agnostic layer that all backends can read.

## Design

### Core idea

Extern functions are declared in **module interface files** (`.ggi`) that specify both the Gorget-level types and the ABI-level marshalling for each parameter. The compiler reads these declarations and generates the correct boundary code for any backend.

### Extern module declaration syntax

```gorget
extern module regex:
    version 1.0.0
    link "pcre2"
    platform "macos", "linux", "windows"

    type Regex = OpaquePtr
    type Match = OpaquePtr

    Regex compile(cstr pattern, cstr flags)
    void free(Regex rx)
    Match find(Regex rx, cstr subject, int offset)
    bool is_match(Regex rx, cstr subject)
    Vector[Match] find_all(Regex rx, cstr subject)
    String replace(Regex rx, cstr subject, cstr replacement)
    String escape(cstr text)
    cstr last_error()
```

```gorget
extern module sdl:
    version 2.0.0
    link "SDL2"
    platform "macos", "linux", "windows"

    type Window = OpaquePtr
    type Renderer = OpaquePtr
    type Texture = OpaquePtr

    Window create_window(cstr title, int x, int y, int w, int h, uint32 flags)
    Renderer create_renderer(Window win, int index, uint32 flags)
    void destroy_window(Window win)
    void set_window_title(Window win, cstr title)
    cstr last_error()
```

```gorget
extern module sqlite:
    version 3.0.0
    link "sqlite3"

    type Database = OpaquePtr
    type Statement = OpaquePtr

    # These wrappers accept full Gorget strings (they handle cstr
    # conversion internally)
    Database open(String path)
    Statement prepare(Database db, String sql)
    void bind_str(Statement stmt, int idx, String val)
    void close(Database db)
```

### ABI type vocabulary

The extern type system is deliberately small. These types describe how values cross the ABI boundary, not their Gorget-level semantics:

| Extern type | Meaning | C | LLVM | WASM |
|---|---|---|---|---|
| `cstr` | Null-terminated byte pointer. Compiler extracts `.data` from String, ensures `\0` | `const char*` | `i8*` | `i32` (memory offset) |
| `byteptr` | Raw byte pointer without null guarantee. Compiler extracts `.data` | `const char*` | `i8*` | `i32` (memory offset) |
| `String` | Full Gorget string struct by value. Callee understands the layout | `Str` (32-byte struct) | `%Str` | Struct in linear memory |
| `int`, `float`, `bool` | Scalars by value | `int64_t`, `double`, `bool` | `i64`, `double`, `i1` | `i64`, `f64`, `i32` |
| `uint8`..`uint64`, `int8`..`int64` | Fixed-width scalars | Corresponding C types | Corresponding LLVM types | `i32`/`i64` |
| `ptr[T]` | Typed pointer to T | `T*` | `%T*` | `i32` |
| `OpaquePtr` | Untyped handle, pass-through | `void*` | `i8*` | `i32` |
| Named opaque (`Regex`, `Window`) | Declared opaque types, same as OpaquePtr | `int64_t` / `void*` | `i64` / `i8*` | `i32` / `i64` |

### Module metadata

Each extern module carries metadata beyond function signatures:

```gorget
extern module gl:
    version 2.1.0              # ABI version (major.minor.patch)
    link "GL", "GLU"           # libraries to link
    platform "macos", "linux"  # supported platforms
    capabilities THREAD_SAFE   # static capability flags

    # ...functions...
```

- **`version`** — semantic version of the extern interface. Major bumps indicate breaking changes. The compiler can check compatibility.
- **`link`** — libraries to pass to the linker. Replaces hardcoded linker flag detection in the backend.
- **`platform`** — target platforms. The compiler skips modules that don't match the target. Replaces `meta if platform() == "macos"` guards in library code.
- **`capabilities`** — static flags the compiler or runtime can query. Examples: `THREAD_SAFE`, `ASYNC_SAFE`, `NO_ALLOC`.

### Two-tier interface

Users never see extern types. The extern module is wrapped by a Gorget library that provides ergonomic types:

```
┌─────────────────────────────────────────────────────────┐
│  User code                                              │
│  Result[Regex, String] rx = regex_compile(r"\d+")       │
│  Gorget types: String, Result, Regex                    │
└──────────────────────┬──────────────────────────────────┘
                       │ calls
┌──────────────────────▼──────────────────────────────────┐
│  Library wrapper (lib/xtd/regex.gg)                     │
│  Result[Regex, String] regex_compile(String pattern):   │
│      Regex rx = _ffi.compile(pattern, "")               │
│      if rx.is_null():                                   │
│          return Error(_ffi.last_error())                 │
│      return Ok(rx)                                      │
│  Gorget types in, Gorget types out                      │
└──────────────────────┬──────────────────────────────────┘
                       │ calls (auto-marshalled)
┌──────────────────────▼──────────────────────────────────┐
│  Extern module (lib/xtd/regex.ggi)                      │
│  extern module regex:                                   │
│      Regex compile(cstr pattern, cstr flags)             │
│  ABI types: cstr, OpaquePtr                             │
└──────────────────────┬──────────────────────────────────┘
                       │ backend-specific codegen
              ┌────────┼────────┬────────┐
              ▼        ▼        ▼        ▼
           C backend  LLVM    WASM    Metal
```

The `.ggi` file is the ABI contract. The `.gg` wrapper provides the Gorget-idiomatic API. Users import from the wrapper, never from the `.ggi` directly.

### Marshalling rules

When a Gorget value crosses the extern boundary, the compiler applies a marshalling rule based on the extern type declaration:

| Gorget value | Extern type | Compiler action |
|---|---|---|
| `String` | `cstr` | Deref CoW pointer → extract `.data` → ensure null-terminated |
| `String` | `byteptr` | Deref CoW pointer → extract `.data` |
| `String` | `String` | Deref CoW pointer → load full struct |
| `Vector[T]`, `Dict[K,V]` | `ptr[T]` | Pass collection's internal pointer |
| Any value | `OpaquePtr` | Pass as-is (integer/pointer) |
| `int`, `float`, `bool` | `int`, `float`, `bool` | Pass directly |

These rules are backend-agnostic. Each backend implements them in its own IR:

- **C backend**: `((Str*)ptr)->data`, `*(Str*)ptr`, `(void*)ptr`
- **LLVM backend**: `getelementptr` + `load`, `load %Str`, `bitcast`
- **WASM backend**: `i32.load` from linear memory offset

### Compiler pipeline integration

```
                 Parse .ggi          IR lowering          Backend
                 ─────────           ──────────           ───────
.ggi file  →  ExternModule {      ExternCallInst {     emit_extern_call():
               name, version,       fn: "compile",       read param.abi_kind
               fns: [{              args: [pat, ""],     match CStr:
                 name: "compile",   abi: [CStr, CStr]     emit .data extraction
                 params: [                               match String:
                   (CStr, "pat"),                           emit struct load
                   (CStr, "flags")                       match OpaquePtr:
                 ],                                        emit pass-through
                 ret: OpaquePtr
               }]
             }
```

The `AbiKind` enum flows from `.ggi` parsing through the IR to codegen:

```
enum AbiKind {
    CStr,          // null-terminated byte pointer
    BytePtr,       // raw byte pointer
    GorgetString,  // full Gorget string struct
    Scalar,        // int, float, bool — pass by value
    TypedPtr(T),   // typed pointer to T
    OpaquePtr,     // untyped handle
}
```

Each backend implements `fn emit_marshal(value: Value, from: GorgetType, to: AbiKind) -> BackendValue` — one method that replaces all the whitelists.

## Migration path

### Phase 1: Internal ABI tags (non-breaking)

Add `AbiKind` to the IR's extern function representation. Default all existing externs to `AbiKind::Auto` (current behavior — whitelists). No syntax changes, no regressions.

**Effort:** Small. Add enum, thread it through IR, backends ignore it when `Auto`.

### Phase 2: Annotate runtime modules

Convert the synthetic module generators (`gen_regex_module`, `gen_sdl_module`, etc.) to emit `AbiKind` tags derived from the existing whitelists. The whitelists become the source for generating tags instead of being queried at codegen time.

**Effort:** Mechanical. One-to-one mapping from whitelist entries to ABI tags.

### Phase 3: `.ggi` parser

Add parser support for `extern module` blocks in `.ggi` files. Move runtime module declarations from Rust codegen into `.ggi` files. The synthetic module generators are deleted.

**Effort:** Medium. Parser extension + file loading. The module declarations already exist conceptually — they just move from Rust to Gorget.

### Phase 4: Delete whitelists

With all extern functions carrying ABI tags, `takes_cstr_for_str_param()`, `needs_null_terminated_cstr()`, `runtime_arg_by_ptr()`, and `runtime_extern_sig()` are deleted. The C backend reads ABI tags directly.

**Effort:** Small. Delete code, replace call sites with tag checks.

### Phase 5: User-facing extern syntax

Expose `extern module` blocks to user code for third-party FFI. Users can declare extern functions with ABI types and call them from Gorget.

**Effort:** Documentation + testing. The infrastructure exists from phases 1-4.

## What this enables

- **LLVM backend**: reads the same ABI tags, emits typed LLVM IR declarations. No new whitelists needed.
- **WASM backend**: reads ABI tags, emits WASM import declarations with correct types.
- **User FFI**: users can call C libraries without compiler changes.
- **Gorget-arena and future projects**: new runtime functions (SDL, GL, Metal) work immediately — declare them in `.ggi`, done. No compiler patches.
- **Runtime as a library**: the runtime's interface is specified in `.ggi` files, not scattered across Rust codegen. It can be versioned, documented, and tested as a standalone contract.

## What this does NOT change

- Gorget-to-Gorget calls are unaffected. CoW, ownership, drop semantics — all internal.
- The C runtime source files (`c_runtime.rs`, `gorget_sqlite.c`, etc.) stay as-is. Only the declarations change.
- The `.gg` library wrappers that users import stay as-is. They wrap the extern calls with Gorget types.

## Open questions

1. **Should `cstr` guarantee null termination, or should there be separate `cstr` (guaranteed) and `byteptr` (not guaranteed)?** Current proposal has both. Could merge into one with an annotation.

2. **How should return types be marshalled?** Functions returning `cstr` need wrapping into Gorget `String` (via `gorget_str_from_cstr`). Functions returning `OpaquePtr` need no wrapping. This is the inverse problem.

3. **Should `.ggi` files support conditional declarations?** For example, Metal functions only on macOS. Current proposal uses module-level `platform` metadata. Per-function conditions may be needed.

4. **Should capability flags be standardized or user-defined?** FFmpeg uses both (public `AV_CODEC_CAP_*` and private `FF_CODEC_CAP_*`). Gorget could start with user-defined strings and standardize later.

5. **Interaction with `directive explicit-clone`**: With CoW, implicit clones at the extern boundary are cheap (just a `.data` extraction, no allocation). The `explicit-clone` directive may not make sense for extern calls. Should extern marshalling always be implicit?
