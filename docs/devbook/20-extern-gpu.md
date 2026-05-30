# Chapter 20 — Extern, interop & GPU backends

This chapter covers how Gorget code reaches out of the language: the `extern`
declaration syntax (parsed in `src/parser/`), the ABI-marshalling metadata that
crosses every IR layer (`src/ir/abi.rs`, derived in `src/ir/lowering/mod.rs`),
and the GPU/graphics runtime architecture — Metal, OpenGL, SDL — which is
*built on* the extern machinery: a `.gg` interface file declares opaque-handle
extern functions and a C-runtime string (`src/backend/c/c_runtime.rs`) provides
the implementations, conditionally included by the C backend
(`src/backend/c_lir/emit_types.rs`) and conditionally linked by the driver
(`src/main.rs`).

The user never sees ABI types. A library API exposes `String`, `Vector[uint8]`,
`Result` — the extern boundary is an implementation detail of the standard
library and the `lib/` modules. The job of this subsystem is to turn a
`String` argument into the `const char*` the C function wants, wrap a returned
`const char*` back into an owned `String`, and pass pointer-sized GPU handles
through untouched.

This chapter folds the former `extern-modules.md` deep-dive. That doc described the
`borrowed` return qualifier as a *future* feature; it has since shipped — see
[The `borrowed` qualifier](#the-borrowed-qualifier). The GPU half had no
internals doc and is written here from the runtime source.

## Extern declaration syntax

An `extern` declaration binds a Gorget function signature to a C symbol. The
parser recognises two shapes, both handled by the same `FunctionDef`
construction path.

### Inline extern

A single `extern` function, optionally with an ABI tag, written wherever a
function definition is legal (top level, or inside an `equip` block):

```gorget
extern "C" int exec(String cmd) = "gorget_exec"
extern String pattern_str() = "gorget_regex_pattern_str"   # bare = Gorget ABI
```

The `extern` keyword is consumed at the head of `parse_function_def`
(`src/parser/mod.rs:1626`). An optional string literal immediately after it
becomes the ABI tag (`extern_abi`, `src/parser/mod.rs:1627-1639`). The body is
not a block — `finish_function_def` requires `= "c_symbol_name"` and stores it
as `FunctionBody::Extern(String)` (`src/parser/mod.rs:1743-1751`). The string is
the runtime symbol; this is the *one* place a name is allowed to be the
contract (the C-emit boundary, per the layering-discipline exception).

### Extern block

`parse_extern_block` (`src/parser/mod.rs:1451`) groups declarations under a
shared ABI tag, so the tag is written once:

```gorget
extern "C":
    extern String read_file(cstr path) = "gorget_read_file"
    extern bool file_exists(cstr path) = "gorget_file_exists"
```

The block reads its optional ABI string (`src/parser/mod.rs:1455-1463`), opens
an indented block, and parses each member through the same
`parse_function_def` used for free functions (`src/parser/mod.rs:1477`). The
result is an `Item::ExternBlock(ExternBlock)` (`src/parser/ast.rs:1155`), holding
the `abi: Option<Spanned<String>>` and the `Vec<Spanned<FunctionDef>>`. The
top-level dispatcher at `src/parser/mod.rs:571-579` disambiguates `extern "C":`
(block — string literal then block-start) from `extern "C" int foo()` (inline —
string literal then a type).

### The `in_extern_c` parser flag

Two type spellings — `cstr` and `T*` — are *contextual*: legal only inside an
`extern "C"` context. The parser tracks this with one `bool` field,
`in_extern_c` (`src/parser/mod.rs:46-48`). It is set on entry to an `extern
"C":` block (`src/parser/mod.rs:1467-1469`) or an inline `extern "C"`
declaration (`src/parser/mod.rs:1642-1643`), and *restored* (not blindly
cleared) on exit (`mod.rs:1482`, `mod.rs:1707`), so nesting and the
non-`"C"` cases compose correctly.

- **`cstr`** is parsed in `parse_type` (`src/parser/types.rs:90-98`): when
  `in_extern_c` is set it becomes `Type::Primitive(PrimitiveType::CStr)`;
  otherwise the parser raises a targeted error ("`cstr` is only valid inside
  `extern "C"` declarations"). Outside the context, `cstr` is just an ordinary
  identifier — no keyword reservation, no breakage for user code that names a
  variable `cstr`.
- **`T*`** is parsed in `parse_type_postfix` (`src/parser/types.rs:152-165`):
  inside `extern "C"` a trailing `*` wraps the base type as `Type::Pointer`,
  meaning "pass as `const T*` in C — take the address of the struct value".
  Outside the context the `*` is deliberately *not* consumed, so it stays
  available to the expression parser as multiplication.

### Qualifiers

After the (optional) ABI tag, `parse_function_def` consumes a run of qualifier
keywords into `FunctionQualifiers` (`src/parser/mod.rs:1648-1664`,
struct at `src/parser/ast.rs:146`). The ones that matter at the extern
boundary:

- **`blocking`** (`is_blocking`) — the call may block the OS thread. Recorded
  in `yield_point_fns` during lowering (`src/ir/lowering/mod.rs:806-810`) so the
  shared-async transform releases and reacquires shared-variable locks around
  the call.
- **`async`** (`is_async`) — the call suspends the current coroutine; also a
  yield point (`mod.rs:806`).
- **`noreturn`** (`is_noreturn`) — the call never returns (`exit`, `abort`).
  Recorded in `noreturn_fns` (`src/ir/lowering/mod.rs:818-823`) so the IR
  terminates the basic block with `unreachable` afterwards — without this a
  divergent `exit(2)` in a match arm would fall through to a bogus arm-value
  assignment.

## ABI marshalling: `AbiKind`

How a value crosses the extern boundary is described by `AbiKind`
(`src/ir/abi.rs:13`), a `#[derive(Copy)]` enum that is deliberately
*backend-agnostic*: the C backend, a future LLVM backend, and a WASM backend
all read the same tags and emit their own marshalling. This is the
layering-discipline "typed metadata, not name-matched" rule applied to the FFI
boundary — the decision lives on a typed field, not in a name whitelist (the
old `is_cstr_returning_fn` / `takes_cstr_for_str_param` whitelists have been
deleted).

The variants, with their meaning per the doc comments at `src/ir/abi.rs`:

| Variant | Marshalling | C lowering |
|---|---|---|
| `Auto` | migration default — backend uses current behaviour | (whitelist/structural) |
| `CStr` | extract `.data`, ensure NUL-termination | `const char*` via `gorget_str_to_cstr` |
| `BytePtr` | extract `.data`, no NUL guarantee | `const char*` (binary/length-prefixed APIs) |
| `GorgetString` | deref CoW pointer, load full 32-byte `Str` | `Str` struct by value |
| `Scalar` | by value, no transformation | int/float/bool |
| `Ptr` | pass directly as a pointer | collection self-by-ptr, element args, opaque handles |
| `Opaque` | pass as-is | Regex/Window/Database handles |
| `ByValue` | deref if pointer, else pass through | aggregate by value (array/set union args) |
| `VoidElem` | `void*` to element data | collection element params |

### Where the `AbiKind` vector is derived

The per-function ABI is computed during the lowering prescan and stored on the
IR module as `fn_extern_abi_kinds: FxHashMap<String, Vec<AbiKind>>`
(`src/ir/mod.rs:267-269`) and a per-function return kind `fn_return_abis`
(`src/ir/mod.rs:274-276`). There are two derivation sites — one for inline
externs, one for extern blocks — that apply the *same* rules:

**Inline extern** (`src/ir/lowering/mod.rs:705-751`). The block ABI tag selects
the "string ABI": `Some("C") => CStr`, everything else (including bare
`extern`) `=> GorgetString` (`mod.rs:720-723`). Note that bare `extern` defaults
to **Gorget**, *not* Auto — the comment at `mod.rs:712-719` explains why: a
bare String param registered as `Auto` is indistinguishable from "pass a
pointer" to the LLVM x86_64 backend, which then omits the `byval(...)` attr and
the C side reads the `Str` struct from the wrong place (aarch64 happens to be
ABI-compatible with bare `ptr`, hiding the bug there). Per-param: explicit
`cstr` → `CStr`; a string type → the block's string ABI; a non-string resource
type → `Ptr`; everything else → `Auto` (`mod.rs:724-735`). The vector is stored
only if some entry is non-`Auto`, and keyed under *both* the Gorget name and
the resolved C symbol (`mod.rs:736-739`).

**Extern block** (`src/ir/lowering/mod.rs:774-873`). Same string-ABI selection
(`mod.rs:776-779`), same per-param rules with the addition of an explicit
`Type::Pointer(_)` (`T*`) → `Ptr` arm (`mod.rs:836-849`). Both sites also derive
the *return* ABI from an explicit `cstr` return type → `CStr`
(`mod.rs:741-744`, `mod.rs:858-863`).

### `cstr` vs `String` in `extern "C"` blocks

Inside `extern "C"`, a `String` *param* is auto-marshalled to `const char*`
exactly like `cstr` — they produce identical code (the `is_string_type(tid)` arm
maps to the `CStr` string-ABI). Writing `cstr` is optional documentation of the
C-side type. For *returns* they differ: `cstr` signals the C function returns
`const char*` (needs wrapping into an owned `String`), while `String` signals it
returns the `Str` struct by value (no wrapping). The `PrimitiveType::CStr`
return type itself maps to the owned string type in
`src/ir/lowering/types.rs:535`.

## Ownership of extern results

By definition, an extern function result is **owned**: a C function cannot
return a borrowing view into Gorget-managed memory. That rationale is recorded
on `extern_body_fns` (declared `src/ir/lowering/context.rs:345`, doc comment at
`context.rs:343-344`), which the lowering prescan fills with every
`FunctionBody::Extern` name (`src/ir/lowering/mod.rs:707`, `mod.rs:828`,
`mod.rs:909`, `mod.rs:1018`). **Note: the set is currently write-only** — it has
no read site anywhere in `src/` (a `grep 'extern_body_fns\b'` returns only the
inserts and the declaration). It records the intent but suppresses no clone.

The behaviour that actually keeps extern results owned is the generic
call-lowering path: `ctx.call_tracked` (`src/ir/lowering/exprs/calls.rs:1386`)
registers the result as an owned local, so no redundant clone is inserted. The
one place a clone *is* forced is the `borrowed`-return branch immediately after
it (`calls.rs:1401`), below.

### The `borrowed` qualifier

The internals doc described `borrowed` as future work; **it is implemented.**
Some C functions return a pointer into a buffer they own and may mutate or free
later — `SDL_GetError()`'s internal buffer, `strerror`, errno-style accessors.
Treating that as owned would either double-free or read freed memory. The
`borrowed` qualifier marks such a return so the compiler clones at the boundary.

Parsing: `borrowed` is recognised in `parse_function_def` *only* when
`is_extern` is set (`src/parser/mod.rs:1666-1683`). It is read as a plain
`Token::Identifier` whose text is `"borrowed"` — exactly like `cstr`, it is
contextual and reserves no keyword. The result is the `returns_borrowed: bool`
field on `FunctionDef` (`src/parser/ast.rs:138-142`).

```gorget
extern "C":
    extern borrowed String sdl_get_error() = "gorget_sdl_get_error"
```

Lowering records the flag in `fn_returns_borrowed: FxHashSet<String>`
(`src/ir/mod.rs:285-292`), keyed under both the Gorget name and the C symbol
(`src/ir/lowering/mod.rs:747-750` for inline, `mod.rs:867-872` for blocks).

The consumer is wired at the call site in
`src/ir/lowering/exprs/calls.rs:1401-1422`. When `fn_returns_borrowed` contains
the resolved call name, the lowerer materialises an independent owned copy:
unregister the borrowed alias from drop tracking (its buffer belongs to the
FFI), emit an `ImplicitCloneReason::BorrowedExternReturn` warning
(`src/ir/mod.rs:208`, `mod.rs:224`), clone via the type's owned-clone routine,
then register and `set_owned` the clone. This mirrors the by-value-resource
branch of `ensure_owned_at_boundary`. The caller's slot now survives any
subsequent FFI state mutation that invalidates the original buffer.

## The compiler pipeline for an extern call

```
.gg source            IR lowering (prescan)        LIR / C backend
──────────            ─────────────────────        ───────────────
extern "C":           fn_extern_abi_kinds:         LirExtern { param_abis }
  extern String         { read_file: [CStr] }      → emit gorget_str_to_cstr()
    read_file         fn_return_abis:                for each CStr param
    (cstr path)         { getenv: CStr }
    = "gorget_..."    fn_returns_borrowed:         CStr return / cstr_val:
extern borrowed         { sdl_get_error, ... }     → gorget_string_adopt()
  String                extern_body_fns:             or gorget_str_from_cstr()
  sdl_get_error()       { read_file, ... }
                      yield_point_fns / noreturn_fns
```

Key metadata, all on the IR module or `LoweringContext`:

| Structure | Location | Purpose |
|---|---|---|
| `fn_extern_abi_kinds` | `src/ir/mod.rs:269` | per-fn param `AbiKind` vector |
| `fn_return_abis` | `src/ir/mod.rs:276` | per-fn return `AbiKind` |
| `fn_returns_borrowed` | `src/ir/mod.rs:292` | functions whose result is cloned at the boundary |
| `extern_body_fns` | `LoweringContext` (`context.rs:345`) | `FunctionBody::Extern` fns — records "results always owned" rationale; write-only (no read site) |
| `extern_bindings` | `LoweringContext` | Gorget name → C symbol |
| `yield_point_fns` | IR module (`src/ir/mod.rs:273`) + `LoweringContext` (`context.rs:311`) | blocking/async fns (yield points) |
| `noreturn_fns` | `LoweringContext` only (`context.rs:316`) | noreturn fns |

The remaining structural whitelist is `legacy_self_by_ptr`
(`src/backend/c_lir/helpers.rs:751`), the self-by-ptr fallback for *unmapped* GIR
runtime names — those that bypass `RuntimeFn::resolve_lir_sig` and so carry no
`arg_abis`. It matches a handful of explicit `gorget_str_*` names plus the
`gorget_array_`/`map_`/`set_`/`heap_`/`bytes_` prefixes (excluding `_new`) and
returns `Ptr` for arg 0. It is consulted only when an explicit ABI tag is
absent, by `resolve_param_abi` (`helpers.rs:1548-1564`): explicit tag wins,
then this structural fallback, then `Auto`. The function's own doc comment marks
it LEGACY/DEPRECATED — a safety net for names that don't yet route through the
tagged `arg_abis` path.

## The C runtime: marshalling helpers

The C-side helpers that the `AbiKind`s lower to live in
`src/backend/c/c_runtime.rs`:

- `gorget_str_to_cstr(Str)` (`c_runtime.rs:3097`) — produces a NUL-terminated
  `const char*` for a `CStr` param.
- `gorget_string_adopt(char*)` (`c_runtime.rs:1520`) — takes ownership of a
  heap `const char*` return.
- `gorget_str_from_cstr(const char*)` (`c_runtime.rs:1901`) — copies a possibly
  static/non-owned `const char*` into an owned `Str`.
- `gorget_str_own_region(const char*, size_t)` (`c_runtime.rs:2138`) — copies a
  bounded region into an owned `Str`; used where the C source buffer is
  autoreleased (e.g. Metal's `device.name` in `gorget_metal_device_name`,
  `c_runtime.rs:12738-12744`).

## GPU & graphics: opaque-handle FFI built on extern

> Note: the MEMORY index still calls these "synthetic modules in
> `gen_metal_module()` / `gen_gl_module()`". **That is stale.** The Metal/GL/SDL
> interfaces are now ordinary embedded `.gg` files
> (`lib/xtd/{metal,gl,gpu,sdl}.gg`), pulled in via `builtin_module_source` in
> `src/stdlib.rs:127-130`; `generate_builtin_module` now always returns `None`
> (`src/stdlib.rs:55-57`). There is no synthetic-module generator anymore.

The graphics backends are *not* a separate compiler backend. They are pure
applications of the extern machinery: a `.gg` interface declares constants,
opaque structs, and `extern "C"` functions; a C-runtime string in
`c_runtime.rs` implements them; the C backend conditionally includes that string
and the driver conditionally links the platform libraries.

### Opaque handles as `int`

Every GPU object — Metal device, command queue, texture; GL buffer, texture —
crosses the FFI as a pointer-sized `int` (`int64_t` in C). The `.gg`
interface declares this directly:

```gorget
# lib/xtd/metal.gg:266-269
extern "C":
    extern int metal_create_device() = "gorget_metal_create_device"
    extern String metal_device_name(int device) = "gorget_metal_device_name"
```

On the C side every wrapper casts to/from the int handle
(`c_runtime.rs:12730`):

```c
static int64_t gorget_metal_create_device(void) {
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        return (int64_t)(intptr_t)device;
    }
}
```

This is the cleanest possible interop: there is no Gorget-side struct mirroring
the Metal class layout, no ABI fragility, no header dependency leaking into the
type system. The handle is an opaque token; the runtime owns the object graph.
Bulk data crosses as `const GorgetArray*` (e.g.
`gorget_metal_create_buffer_with_data`, `c_runtime.rs:12823`); strings cross via
the standard `String`/`cstr` marshalling. The platform-neutral `xtd.gpu` layer
(`lib/xtd/gpu.gg`) wraps these int handles in named structs (`GpuDevice`,
`GpuBuffer`, `GpuTexture` — each just an `int handle` plus metadata) and selects
Metal vs GL at compile time via `meta platform()`, so the unused backend's code
is never emitted.

### Memory management of handles

Metal objects are reference-counted Objective-C objects, but ARC is *disabled*
(`-fno-objc-arc`, `src/main.rs:1014`) precisely because handles are laundered
through `int64_t` where ARC cannot see them. The convention is documented at
`c_runtime.rs:12696-12700`: `new*` methods return +1 (caller owns), non-`new`
methods are retained before returning, and `gorget_metal_release(int64_t)`
(`c_runtime.rs:13407`) does `[(id)handle release]`. Every wrapper body is
bracketed by `@autoreleasepool`.

### Conditional runtime inclusion

The C backend only emits a runtime block if the program references its symbols.
The selector is `has(&|n| n.starts_with(...))` in
`src/backend/c_lir/emit_types.rs`:

- **SDL** — `sdl_` / `gorget_sdl_` prefix (`emit_types.rs:2220-2232`); also
  emits `#define GORGET_USE_SDL_IMAGE` / `_TTF` when the corresponding
  functions are present, which the driver reads back to choose link flags.
- **OpenGL** — `gorget_gl_` prefix → `GL_RUNTIME` (`emit_types.rs:2240-2242`).
- **Metal** — `gorget_metal_` *or* `gorget_sdl_metal_` prefix → `METAL_RUNTIME`
  (`emit_types.rs:2273-2276`).

The runtime strings carry their own platform preamble. `METAL_RUNTIME`
(`c_runtime.rs:12702`) is wrapped in `#ifdef __APPLE__` and `#import`s
`<Metal/Metal.h>` + `<QuartzCore/CAMetalLayer.h>`. `GL_RUNTIME`
(`c_runtime.rs:11058`) includes `<OpenGL/gl.h>` on Apple and `<GL/gl.h>`
elsewhere, and `#define`s the GL 3.x core entry points to their `APPLE`/`ARB`
extension spellings (and stubs the few unavailable on macOS legacy GL) so the
same `gorget_gl_*` body compiles on both.

### Conditional compile & link flags

The driver (`src/main.rs`) detects GPU usage from the concatenated source — note
this is a *source-text contains* check, not symbol-based:
`let needs_metal = concat_source.contains("xtd.metal")` (`src/main.rs:921`).
Two consequences follow:

1. **Objective-C compilation.** Metal wrappers are Objective-C, so on macOS the
   driver inserts `-x objective-c` *before* the source file, plus
   `-fno-objc-arc` and several `-Wno-*` suppressions
   (`src/main.rs:1011-1019`). The flag ordering matters — it must precede the
   `.c` file to retroactively retag it.
2. **Framework / library linking.** `add_metal_flags` links
   `-framework Metal -framework QuartzCore -framework Foundation` on macOS and
   is a no-op elsewhere (`src/main.rs:260-271`); `add_gl_flags` links
   `-framework OpenGL` on macOS, `-lGL` elsewhere (`src/main.rs:213-222`).
   Because Metal is layered on SDL's `SDL_Metal_CreateView`, the SDL link flags
   are forced on when `needs_metal` is set (`src/main.rs:1039`).

The same conditional-link pattern is used for the other native dependencies
(`add_audio_flags` / SDL2_mixer, `add_compress_flags` / zlib,
`add_tls_flags` / OpenSSL) — they share the `add_*_flags(cmd, needs_x)` shape
and prefer `pkg-config` where available.

### GPU tier status

The level of API coverage (Metal Tier 1 done; Tiers 2/3 partial; GL Phases 1-6
done) is *roadmap*, not architecture — see `TODO.md` and the MEMORY index
("GPU/Metal Architecture", "GL Architecture") for the live list. The
architecture above (opaque int handles, conditional inclusion, conditional
link) is stable regardless of how much of each API is bound.

## In the self-host

n/a. The self-host frontend (`tests/fixtures/self_host_*`) covers the lexer,
parser, resolver, type checker, and GIR/LIR lowering — it does **not** cover the
C backend, the C runtime, or the GPU runtimes. The extern *declaration syntax*
(`extern "C":` blocks, `cstr`, `T*`, `borrowed`) is parsed by the self-host
parser to the extent the comparison fixtures exercise it, but the ABI derivation
(`fn_extern_abi_kinds`), the `gorget_str_to_cstr`/`adopt` lowering, and all of
the Metal/GL/SDL machinery live in the Rust backend with no self-host
counterpart. There is no self-host parity figure to report for this chapter.
