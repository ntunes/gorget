# Extern Functions and FFI

**Status:** Implemented (core ABI pipeline, module migration, inline extern syntax)
**Updated:** 2026-04-03

## Overview

Gorget code calls external functions (C runtime, OS libraries, GPU APIs) through
`extern` declarations in `.gg` files. The compiler marshals values across the ABI
boundary based on the language tag (`"C"` or `"Gorget"`) and explicit FFI types.

User code never sees FFI types. Library APIs expose `String`, `Vector`, `Result` —
the extern boundary is an internal implementation detail.

## Syntax

### Extern blocks

Group multiple extern declarations under a shared ABI:

```gorget
extern "C":
    extern String read_file(cstr path) = "gorget_read_file"
    extern void write_file(cstr path, cstr content) = "gorget_write_file"
    extern bool file_exists(cstr path) = "gorget_file_exists"
```

### Inline extern declarations

Single extern function with ABI tag:

```gorget
extern "C" int exec(String cmd) = "gorget_exec"
```

Works in equip blocks too:

```gorget
equip Socket:
    extern "C" blocking int write_str(String s) = "gorget_socket_write_str"
    extern blocking Vector[uint8] read(int n) = "gorget_socket_read"
```

### Gorget ABI (default)

An extern without a language tag uses the Gorget ABI — the C function accepts
Gorget types directly (`Str` struct, `GorgetArray*`, etc.):

```gorget
extern String pattern_str() = "gorget_regex_pattern_str"
```

This is equivalent to `extern "Gorget"`.

## Language tags

| Tag | String params | String returns | Use when |
|-----|--------------|----------------|----------|
| `"C"` | `String` → `const char*` via `gorget_str_to_cstr` | `cstr` → owned `const char*` wrapped by `gorget_string_adopt` | Calling C library functions |
| `"Gorget"` | `String` → `Str` struct by value | `String` → `Str` struct by value | Gorget-aware runtime wrappers |
| *(none)* | Same as `"Gorget"` | Same as `"Gorget"` | Default for all `extern = "symbol"` |

## The `cstr` type

`cstr` is a contextual identifier — only valid inside `extern "C"` contexts (block
or inline). It represents a null-terminated `const char*` at the C boundary.

**As a parameter type:** the compiler extracts the string data and ensures null
termination via `gorget_str_to_cstr()`.

**As a return type:** the compiler wraps the returned `const char*` into an owned
Gorget `String` via `gorget_string_adopt()` (for heap-allocated returns) or
`gorget_str_from_cstr()` (for extern "C" returns that may be static).

Outside `extern "C"` contexts, `cstr` is a regular identifier — the parser rejects
it as a type with a clear error message.

```gorget
# Valid — inside extern "C": block
extern "C":
    extern cstr getenv(cstr name) = "gorget_getenv"

# Valid — inline extern "C"
extern "C" bool file_exists(String path) = "gorget_file_exists"

# Error — cstr outside extern "C"
void foo(cstr s):    # parser error: cstr only valid in extern "C"
    pass
```

### String vs cstr in extern "C" blocks

Inside `extern "C":` blocks, `String` params are auto-marshalled to `const char*`
(same as `cstr`). Using `cstr` explicitly is allowed but optional — it documents
the C-side type for readability. The two are equivalent:

```gorget
extern "C":
    extern void write_file(cstr path, cstr content) = "gorget_write_file"
    extern void write_file(String path, String content) = "gorget_write_file"
    # Both produce identical code — String is auto-marshalled to const char* in "C" blocks
```

For return types, `cstr` and `String` are different: `cstr` signals the C function
returns `const char*` (needs wrapping), while `String` signals it returns `Str`
by value (no wrapping needed).

## Ownership of extern results

All extern function results are **owned by definition**. A C function cannot return
a view into Gorget-managed memory. The compiler tracks this via `extern_body_fns`
and marks results as owned in the GIR, preventing redundant clones.

**Future:** The `borrowed` qualifier will handle C functions that return non-owned
pointers (e.g., `SDL_GetError()`'s internal buffer). The compiler will auto-clone
at the boundary:

```gorget
extern "C":
    extern borrowed String sdl_get_error() = "gorget_sdl_get_error"
    # Returns a view into SDL's internal buffer — compiler copies to owned String
```

Until `borrowed` is implemented, C wrappers that return non-owned pointers copy
internally (via `gorget_str_from_cstr`), so Gorget always gets owned data.

## Other qualifiers

### `blocking`

Marks extern functions that may block the current thread. The shared_async
transform releases and reacquires shared variable locks around blocking calls:

```gorget
extern "C":
    extern blocking String read_file(cstr path) = "gorget_read_file"
    extern blocking Result[Socket, String] socket_connect(cstr host, int port) = "gorget_socket_connect"
```

### `async`

Marks extern functions that suspend the current coroutine. The return type is
automatically wrapped in `Future[T]`:

```gorget
extern "C":
    extern async void sleep(int seconds) = "gorget_reactor_sleep_seconds"
```

## Compiler pipeline

```
  .gg source              IR lowering            LIR lowering           C backend
  ──────────              ───────────            ────────────           ─────────

  extern "C":             fn_extern_abi_kinds:   LirExtern {            resolve_param_abi():
    extern String         { "read_file":           name: "gorget_...",    check LirExtern.param_abis
      read_file             [CStr] }               param_abis: [CStr],   → emit gorget_str_to_cstr()
      (cstr path)                                  return_abi: Auto }
      = "gorget_..."      fn_return_abis:
                          { "getenv": CStr }     cstr_vals bitmap:      SlotStore Ptr→Str:
  extern "C" blocking                             tracks const char*     → gorget_string_adopt()
    int write_str         extern_body_fns:         return values          or gorget_str_from_cstr()
      (String s)          { "write_str", ... }
      = "gorget_..."
                          yield_point_fns:
  extern cstr             { "read_file",
    getenv(cstr name)       "write_str", ... }
    = "gorget_getenv"
```

### Key data structures

| Structure | Location | Purpose |
|-----------|----------|---------|
| `fn_extern_abi_kinds` | IR Module | Per-function param ABI vectors (`[Auto, CStr, CStr]`) |
| `fn_return_abis` | IR Module | Per-function return ABI kind (CStr for `const char*` returns) |
| `extern_body_fns` | LoweringContext | Functions with `FunctionBody::Extern` — results are always owned |
| `yield_point_fns` | IR Module | Blocking/async functions — yield points for shared var locks |
| `extern_bindings` | LoweringContext | Gorget name → C symbol mapping |
| `LirExtern.param_abis` | LIR Module | Param ABI vector, consumed by C backend's `resolve_param_abi` |
| `cstr_vals` | C backend | Bitmap of values from `const char*`-returning functions |

### ABI derivation sources (priority order)

1. **Explicit `cstr` type** in `extern "C"` declaration → `AbiKind::CStr`
2. **Block ABI string** — `extern "C":` auto-derives CStr for `String` params
3. **Inline ABI tag** — `extern "C"` on individual function, same effect
4. **Fallback whitelist** — `takes_cstr_for_str_param()` for Declaration-body
   methods with no `.gg` declaration (~15 remaining entries)

## Remaining whitelists

These whitelists in `src/backend/c_lir/mod.rs` are fallbacks for functions not yet
declared in `.gg` files. Each can be eliminated by adding explicit `.gg` declarations:

| Whitelist | Entries | Covers |
|-----------|---------|--------|
| `takes_cstr_for_str_param` | ~15 | Declaration-body regex methods, internal runtime |
| `runtime_arg_by_ptr` | ~15 | Functions taking struct args by pointer |
| `is_cstr_returning_fn` | ~20 | Cast-path functions (int→str, format) at LIR level |
| `last_error_fn` | ~9 | Thread-local error check functions |

## File layout

All built-in modules are `.gg` files under `lib/`:

```
lib/
  std/
    fs.gg          # extern "C": block for file I/O
    path.gg        # extern "C": block for path operations
    os.gg          # extern "C": block for getenv, platform, etc.
    conv.gg        # extern "C": block + Gorget Result wrappers
    io.gg          # extern "C": block for input/readline
    socket.gg      # extern "C": block + equip extern methods
    tls.gg         # extern "C": block + equip extern methods
    udp.gg         # extern "C": block + equip extern "C" methods
    process.gg     # extern "C": block + equip extern methods
    collections.gg # opaque structs (Vector, Dict, Box, File)
    channel.gg     # generic Channel[T] with Declaration equip
    thread.gg      # generic Thread[T] + extern current_thread_id
    sync.gg        # AtomicInt, Barrier, RWLock[T], etc.
    ...
  xtd/
    regex.gg       # extern "C": block + equip extern/extern "C" methods
    crypto.gg      # extern "C": block + Gorget Result wrappers
    sdl.gg         # extern "C": block for SDL functions
    gl.gg          # extern "C": block for OpenGL functions
    metal.gg       # extern "C": block for Metal functions
    ...
```

The C runtime implementations live in `src/backend/c/c_runtime.rs`. The `.gg` files
declare the interface; the C file provides the implementation.
