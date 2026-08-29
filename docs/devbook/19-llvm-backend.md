# 19. The LLVM backend

The LLVM backend translates an optimized LIR (BIR) module into LLVM IR
textual format (`.ll`), then drives `llc` + a C compiler to produce a native
binary. It lives entirely in `src/backend/llvm/mod.rs` (one ~6.7k-line file)
plus the build-pipeline glue in `src/main.rs` (`compile_llvm_pipeline`,
`src/main.rs:1088`). It is a second backend selected with `--backend=llvm`
(or `GG_BACKEND=llvm` in the test harness) and held at parity with the C
backend (`src/backend/c_lir/`) against the full integration fixture set.

> The C/LIR backend is described elsewhere as the *sole production backend*
> (`CLAUDE.md`, `docs/language-design.md`); this LLVM backend exists behind
> `--backend=llvm` and is tested at C-parity. The "sole production backend"
> wording in those docs predates this chapter's framing — a doc-vs-doc
> wording inconsistency worth aligning, not a contradiction in behavior.

This chapter describes the implementation: the near-1:1 LIR→LLVM mapping, how
the C runtime is reused unchanged (compiled separately and linked as a `.o`),
where the two backends share ABI logic, and the handful of places the LLVM
emitter must do work the C compiler otherwise did for free (signedness,
struct padding, sret, overflow intrinsics).

> The old planning doc (the former `llvm-backend-plan.md`, now folded into this
> chapter) was framed in the future tense ("Phase 1 … Phase 3 … will add closures,
> enums, …"). That work has **shipped** — the backend is at C-parity. Treat that
> doc's status claims as historical; the live facts below are re-derived from
> current source.

## Where it plugs in

The backend is the `impl super::Backend for LlvmBackend` at
`src/backend/llvm/mod.rs:24` (the struct `LlvmBackend` is at `mod.rs:11`; the
`Backend` trait itself is defined at `src/backend/mod.rs:377`). Like the C
backend it consumes a `BirModule` — a LIR module that has passed
`bir::lower_lir_to_bir` so no canonical-level high-level ops remain — and the
entry point just unwraps to the underlying `LirModule`:

```rust
fn generate(&self, module: &crate::bir::BirModule) -> super::CodegenOutput {
    super::CodegenOutput { code: generate_llvm_ir(module.as_lir()), extension: "ll" }
}
```
(`src/backend/llvm/mod.rs:29`)

Backend selection is a string read off the CLI: `--backend=llvm` vs the default
`"c-lir"` (`src/main.rs:2497`). `main` dispatches on it
(`src/main.rs:730`); anything that isn't `"llvm"` falls through to
`CLirBackend`. One transparent fallback: **hot-reload** programs are forced
back to the C backend even under `--backend=llvm`, because the host/guest split
(`generate_hot_reload_split`) bracket-matches C `int main(` text and has no IR
equivalent (`src/main.rs:719-729`).

`generate_llvm_ir` (`src/backend/llvm/mod.rs:799`) is the top-level emitter. It
writes, in order: the target datalayout/triple (host-arch-gated cfg blocks,
`src/backend/llvm/mod.rs:808-817`), struct type definitions, interned string
globals, module globals, extern `declare`s, intrinsic `declare`s, then every
function body. LLVM accepts out-of-order references, so no forward declarations
are needed (`src/backend/llvm/mod.rs:877`).

## Why LIR maps almost 1:1

LIR is already SSA with typed values, block parameters (the phi equivalent),
explicit memory ops, and structured control flow, so most instructions are a
direct transliteration. The interesting design point is what LIR has *already
done* by the time the backend runs — monomorphization, drop/clone insertion,
closure lowering, BIR expansion of canonical ops — so the emitter is a dumb
printer for the common case and only carries ABI/representation knowledge.

The transliterations, with current emit sites:

| LIR instruction / terminator | LLVM IR emitted | site |
|---|---|---|
| `Add/Sub/Mul/Div/Rem` | `add`/`sub`/`mul`/`sdiv`\|`udiv`/`srem`\|`urem` | `mod.rs:3182` (div) |
| `Shr` | `ashr` (signed) / `lshr` (unsigned) | `mod.rs:3264` |
| `Cmp` | `icmp slt`\|`ult` … (signedness-selected) | `mod.rs:3283` |
| `Load` / `Store` | `load`/`store` | — |
| `FieldPtr` | `getelementptr` | `mod.rs:3545` (GEP at 3555/3598) |
| `ElemPtr` | `getelementptr` | `mod.rs:3647` (GEP at 3651) |
| `Memcpy` / `Memset` | `call @memcpy`/`@memset` | `mod.rs:3653` |
| `Call` / `CallExtern` / `CallClosure` | `call` | `mod.rs:3662` |
| `Branch` | `br i1` | `emit_term`, `mod.rs:6609` |
| `Switch` | `switch` | `emit_term` |
| `Ret` / `RetVoid` | `ret <T>` / `ret void` | `emit_term` |
| block params `(ValueId, LirType)` | `phi` nodes | `mod.rs:2741` |

Values are SSA already, so a LIR `ValueId v` becomes the LLVM virtual register
`%vN`; function parameters are `%pN` (`src/backend/llvm/mod.rs:2144`).

### Aggregates flow as pointers

The one global representation decision: **aggregates (structs, the runtime
resource types) are represented as pointers throughout the emitted IR**, not as
first-class `{…}` aggregate values. `emit_function` rewrites the shared
`value_types` so any `Struct(sid)` value becomes `PtrTo(sid)`
(`src/backend/llvm/mod.rs:2175-2180`), and additionally overrides the result
type of `Call`/`CallClosure` whose target returns an aggregate, and of
Option/Result combinators and the `*__parse` inline handlers, all of which
materialize their result into an `alloca` and yield the pointer
(`src/backend/llvm/mod.rs:2185-2243`). Consequently block-parameter phis for
aggregate values are emitted as `phi ptr`, not `phi <struct>`
(`src/backend/llvm/mod.rs:2680`).

## Type mapping

`llvm_type` handles the scalar cases — `I8/U8→i8`, … `I64/U64→i64`,
`F32→float`, `F64→double`, `Bool→i1`, all pointer kinds and `FuncRef→ptr`
(`src/backend/llvm/mod.rs:42`). Note `i8` etc. carry **no signedness** — that is
on the LIR `ty`, and the backend re-derives it (next section). `Struct` and
`Void` are routed through `llvm_type_full`, which maps a struct id to its named
LLVM type `%Name` (or bare `ptr` for `Box__` opaque boxes) and resource kinds to
`%GorgetString`/`%GorgetArray`/`%GorgetMap`/`%GorgetSet`/`%GorgetClosure` (or
`ptr` for `RefCounted`) (`src/backend/llvm/mod.rs:64`). `llvm_arg_type` is the
same except `Void` becomes `ptr` (closure env), since `void` is illegal as an
argument type (`src/backend/llvm/mod.rs:93`).

### Signedness is re-derived from the LIR type

LIR carries signedness in the *type* (`I32` vs `U32`) but not in the
instruction; LLVM puts it in the *opcode*. `is_signed` (`src/backend/llvm/mod.rs:416`)
inspects the instruction's `ty` to pick `sdiv`/`udiv`, `srem`/`urem`,
`ashr`/`lshr`, `icmp slt`/`ult`, and `sext`/`zext` for widening casts. The
width-coercion sequence (`trunc`/`sext`/`zext`) used when an operand's actual
width doesn't match the target is centralized in `emit_int_coerce`
(`src/backend/llvm/mod.rs:442`) and reused at the call-argument site
(`mod.rs:3688`), the spill path (`mod.rs:5896`), and `emit_term`'s return
coercion (`mod.rs:6669`). The overflow check is *not* one of these callers —
`emit_overflow_check` (`mod.rs:6473`) carries its own inline `adjust_operand`
closure (`mod.rs:6506-6522`).

## Struct layout

`emit_struct_types` (`src/backend/llvm/mod.rs:943`) emits a named LLVM type per
LIR `StructDef`. Because the binary links against the **C-compiled runtime**,
every layout must be C-ABI-identical or pointers walk off the ends of allocas.
The cases:

- **Enums** (`is_union_layout`): `{ i32 tag, i32 pad, [payload x i8] }`, the
  payload sized by `enum_payload_size` (`src/backend/llvm/mod.rs:960-968`).
- **Opaque/empty runtime structs**: driven by the *shared* tables
  `opaque_runtime_layout` / `opaque_runtime_size` in `src/lir/lower/types.rs`
  so all backends agree; pointer-sized handles become `{ ptr }`, `Task__*` is
  `{ ptr, ptr }`, trait boxes are the 16-byte `{ ptr, ptr }` TraitObj (gated on
  the typed `is_trait_box` flag, **not** a name match)
  (`src/backend/llvm/mod.rs:969-1008`).
- **Regular structs**: fields are emitted **with explicit inter-field padding**
  computed from `c_alignof_lir_type`, plus trailing padding up to
  `computed_c_size`, because LLVM's natural struct alignment can disagree with
  C's when an aggregate field has lower apparent alignment than its C alignment
  (`src/backend/llvm/mod.rs:1009-1052`). VTable structs (`*_VTable`) force all
  fields to bare `ptr` regardless of the LIR closure typing, matching the C
  backend (`src/backend/llvm/mod.rs:1019`).
- A special-case override emits `%File`/`%GorgetFile` as `{ ptr, i64 }` (the
  gorget-visible 8-byte `File` is a cover for the 16-byte runtime struct;
  without the override an `alloca %File` is 8 bytes short and the runtime store
  of `out->owned` SIGSEGVs) (`src/backend/llvm/mod.rs:956`).

## String literals

`Inst::StrLit` becomes an interned rodata global plus a stack-built
`GorgetString`. All literals are collected up front by `StrGlobals`
(`src/backend/llvm/mod.rs:902`) and emitted as
`@.str.N = private unnamed_addr constant [len x i8] c"…\00"`
(`src/backend/llvm/mod.rs:934`, in `emit_string_globals` at `mod.rs:930`). At
the use site the emitter `alloca`s a
`%GorgetString` (`src/backend/llvm/mod.rs:2430`) and stores the four fields via
GEP: data `= @.str.N`, **cap = 0** (the view/non-owning discriminator — see the
uniform view-discriminator layout, cap at field index 1 / offset +8), len, and
alloc = null (`src/backend/llvm/mod.rs:3063-3084`). Module-level string globals
that lower to a `gorget_str_from_literal` view init are emitted as cap=0 rodata
static initializers with no runtime constructor call
(`src/backend/llvm/mod.rs:848-862`, `1160`).

Format strings for `Printf`/`Fprintf` are interned the same way; `nth_printf_spec`
(`src/backend/llvm/mod.rs:1746`) lets the emitter inspect the Nth conversion spec
so it can coerce a vararg to match (the C backend's `%lld`→`%f` fix), and any
format strings rewritten during emission are appended as "late-added" globals
after the function bodies (`src/backend/llvm/mod.rs:884-895`).

## Calling convention: sret and byval

Returning aggregates uses the **sret** convention with a hidden first pointer
parameter, but only for *large* aggregates — `needs_sret` defers to the shared
`is_small_aggregate` (`src/lir/lower/types.rs`) so small structs (≤16 bytes on
aarch64) are returned in registers, exactly as the GIR→LIR pass and C backend
expect (`src/backend/llvm/mod.rs:15-22, 2155`). An sret function is emitted as
`define void @f(ptr sret(%T) %sret.out, …)` (`src/backend/llvm/mod.rs:2158-2165`);
`main` is exempt and always `i32 @main(i32 %argc, ptr %argv)`
(the `is_main` branch at `src/backend/llvm/mod.rs:2156-2157`).

Large by-value aggregate *arguments* need a `byval(...) align 8` attribute on
x86_64 SysV but **not** on aarch64 AAPCS64 (where the bare `ptr` already matches
the implicit-pointer rule). This is a single helper, `large_agg_byval_attr`,
applied identically at the call site and the extern `declare` so they stay in
sync; `GG_LLVM_FORCE_X86_64_ABI=1` forces the x86_64 shape for cross-target IR
inspection on an aarch64 box (`src/backend/llvm/mod.rs:100-119`). Spawn-wrapper
functions, which cross the C↔LLVM boundary, additionally force their large
aggregate params to `ptr` (`src/backend/llvm/mod.rs:2143-2149`); the
"is this a spawn wrapper" predicate lives in `lir::queries::is_spawn_wrapper`
so the naming patterns are tracked in one place, not name-matched here.

## Entry-block alloca hoisting

**Invariant: every frontend-emitted `alloca` must live in the function *entry*
block.** LLVM only reclaims an `alloca`'s stack slot at function return, and it
never reclaims one that sits *outside* the entry block across loop iterations —
each iteration through a body-block `alloca` allocates a fresh slot that leaks
for the whole call. A per-instruction temp `alloca` emitted inside a loop body
therefore turns an N-iteration loop into N stack frames' worth of slots: a
backward-dataflow fixpoint over a large module (the self-host driver compiling
its own ~660K-line source) drove one such site to *hundreds of MB* of stack in a
single frame and SIGSEGV'd — surfacing as a bogus-looking crash in an unrelated
leaf (`gorget_map_get`'s prologue store first touching the guard page). The C
backend is immune because its temps are function-scope C locals reused each
iteration; this is an LLVM-only placement bug, not a hash/ABI/offset bug.

So `emit_function` streams the body into a separate buffer, extracts every
single-line static `alloca` *definition* (`%x = alloca <ty>`) out of it, and
re-emits those defs in the entry block (before the `br` to the first body
block); the followers that *use* the pointer (`store`/`memcpy`/`select`/calls)
stay in place, and an entry-block alloca dominates all of them, so every hoist
is SSA-valid. A genuinely runtime-sized `alloca <ty>, i64 %reg` must NOT be
hoisted past the register that computes its size; the extractor skips that shape.
The matching invariant is enforced as a structural guard
([Chapter 25](25-structural-guards.md)): after extraction, zero `alloca` lines
may remain in the body buffer, so the next emit arm that introduces a body
alloca is caught by an assertion rather than by the next SIGSEGV.

## Overflow, bounds, and trap intrinsics

`Overflow::Trap` on `Add`/`Sub`/`Mul` lowers to LLVM's checked-arithmetic
intrinsics. `emit_overflow_check` (`src/backend/llvm/mod.rs:6473`) emits
`call { iN, i1 } @llvm.{s|u}{add|sub|mul}.with.overflow.iN`, extracts value and
flag, and branches to a trap block that prints `file:line:col: integer overflow`
to stderr and traps with `call void @exit(i32 1)` + `unreachable`
(`src/backend/llvm/mod.rs:6539-6540`) — byte-matching the C backend's panic
message. The panic location is resolved per call site (`resolve_panic_loc`,
`src/backend/llvm/mod.rs:503`) and its message interned lazily, hence the
late-added-globals pass.

`emit_intrinsic_declarations` (`src/backend/llvm/mod.rs:1916`) unconditionally
declares `@llvm.trap`, `@llvm.memcpy.p0.p0.i64`, and `@llvm.memset.p0.i64`
(`src/backend/llvm/mod.rs:1967-1969`), then scans the module to declare only
the `with.overflow` triples (`mod.rs:1972-1976`) and the
`@llvm.fptosi.sat`/`fptoui.sat` saturating float→int intrinsics
(`mod.rs:1979-1985`) actually used — the latter give Rust `as`-style saturation
(NaN→0, out-of-range→clamp) instead of the UB-poison a raw `fptosi` yields.
Note `@llvm.trap` is declared but never referenced: the actual trap path is the
`@exit(i32 1)` + `unreachable` pair above, not an `@llvm.trap` call.

## The runtime: compiled separately, linked as a `.o`

The decisive parity lever is that **the LLVM backend does not reimplement the
runtime** — it reuses the same C runtime (`src/backend/c/c_runtime.rs`) the C
backend embeds, compiling it to a separate object and linking. Every
`CallExtern` in the IR references a runtime function by name; the backend emits
matching `declare`s for them (`emit_extern_declarations`,
`src/backend/llvm/mod.rs:1208`).

`compile_llvm_pipeline` (`src/main.rs:1088`) is the build driver:

1. **Compose the runtime C source.** Concatenate the runtime modules the
   program needs — always the string/array/map/set/error/io/math/etc. core, and
   conditionally the heavy modules (async, sync, thread, alloc, net, crypto,
   sqlite, trace) gated on `concat_source.contains("std.async")` and on extern
   names present in the module (`src/main.rs:1107-1236`). This mirrors the C
   backend's `emit_runtime_modules`, just composed manually because the LLVM
   path doesn't go through it.
2. **Append the C "wrapper glue."** `generate_llvm_wrappers`
   (`src/backend/c_lir/mod.rs:390`) — really `generate_c_inner_impl(module,
   false, true)` — emits *everything the C backend would except the user
   function bodies*: monomorphized drop/clone functions, Option/Result
   combinators, channel/shared/mutex/spawn-await helpers, adapter functions,
   globals, and the test-runner `main`. These are C functions that call both
   runtime functions and the *user* functions (which live in the LLVM `.o`, and
   resolve at link time) (`src/main.rs:1231-1236`). This is why the two backends
   stay at parity for free: the hard monomorphized glue is generated once, in C,
   and shared.
3. **De-staticify** the runtime so its `static inline` functions are link-visible
   (`static inline `→``, `static `→``), carefully preserving `_Thread_local` /
   `__thread` statics (`src/main.rs:1241-1247`). SQLite is appended *after* this
   transform because its amalgamation relies on file-local linkage
   (`src/main.rs:1259-1275`).
4. **Compile runtime C → `.o`** with `cc -c -O2 -std=c11
   -Werror=implicit-function-declaration` (+ `-pthread` off macOS, and the
   sanitizer flags under `--sanitize` — see below) (`src/main.rs:1601-1633`).
   Deliberately no blanket `-w`: silencing everything here hides
   implicit-declaration bugs, so that one class is a hard error and the rest of
   the warning flood stays off.
5. **Compile the `.ll` → `.o`** with `llc -filetype=obj -O0
   -relocation-model=pic` (`src/main.rs:1306-1313`).
6. **Link** the two objects with `cc -o exe user.o runtime.o -lm` (+ `-pthread`,
   `-fwrapv` if overflow-wrap, crypto/TLS flags conditionally)
   (`src/main.rs:1333-1357`).

Intermediate files are per-fixture-named (`__gorget_runtime_{stem}.c/.o`,
`__gorget_user_{stem}.o`) so parallel `gg build` invocations don't clobber each
other's runtime mid-compile (`src/main.rs:1100-1105`), and cleaned up unless
`GORGET_KEEP_RUNTIME=1` (`src/main.rs:1362-1368`).

### `--sanitize` on this backend, and what it does not cover

The split-compilation shape above is what makes the runtime reuse cheap, and it
is also what bounds sanitizer coverage on this lane. `--sanitize` adds
`-fsanitize=address,undefined -fno-omit-frame-pointer -g` — via the single
`add_sanitize_flags` helper the C backend also uses — to **two** of the steps:
the runtime `cc -c` (step 4) and the final link (step 6). It cannot be added to
step 5, because step 5 is `llc`, and `llc` consumes finished IR: ASan's
instrumentation is an IR-level pass that has already not run by the time `llc`
sees the module. Emitting instrumented IR would mean the backend attaching the
`sanitize_address` attribute itself and the resulting IR surviving the
instrumentation passes — neither of which it does today.

The consequence is a real and stated asymmetry with the C backend, where user
code *is* C and `cc` instruments all of it:

| defect class | C backend | LLVM backend |
|---|---|---|
| memory leak | caught | **caught** — LeakSanitizer intercepts the allocator, so instrumentation is irrelevant |
| UAF / double-free / overflow faulting **inside the runtime** | caught | **caught** — the runtime `.o` is instrumented |
| UAF / overflow / stack error faulting **in generated user code** | caught | **not caught** |

Two consequences worth internalising. First, a program that is clean under
`--sanitize --backend=llvm` is *not* thereby known clean: the C backend is the
lane the safety gates are meant to run on. Second — and this is the trap —
a **leak-based** check cannot detect that this coverage was lost, because leak
detection survives with zero instrumentation. Distinguishing "linked the ASan
runtime" from "linked it *and* instrumented something" needs a probe that looks
for `__asan_report_*` references in the artifact, which only instrumented code
emits. `sanitizer_gate_is_real_on_both_backends` (`tests/security.rs`) is that
probe, and it exists because the flag was silently dropped on this lane for an
unknown period, making every LLVM sanitizer result free.

### LLVM version compatibility

The IR uses opaque pointers (bare `ptr`). LLVM ≥15 makes that the default;
LLVM 14 needs `-opaque-pointers`; LLVM 22 removed the flag. `compile_llvm_pipeline`
probes `llc --version` once via `llc_needs_opaque_pointers_flag`
(`src/main.rs:1066`) and adds the flag only when the major version is `< 15`,
falling open (no flag) if the version can't be parsed
(`src/main.rs:1314-1321`). `LLC` and `CC` are overridable via env.

Runtime-initialized globals (`LirGlobalInit::Extern { name, args }` —
`src/lir/mod.rs:1603`; the older `RuntimeCall(String)` variant was replaced by
this typed shape) are dispatched off the `Extern` variant
(`src/backend/llvm/mod.rs:2380`) into `emit_global_runtime_init`
(`src/backend/llvm/mod.rs:1772`), which builds the ctor sequence (alloca + sret
call + memcpy into the global slot for sret constructors,
`src/backend/llvm/mod.rs:1818-1820`).

## Shared code with the C backend

The two backends agree on ABI by sharing logic rather than duplicating it. Beyond
the runtime `.o` and the C wrapper glue (above), the LLVM backend imports from
the C backend / shared LIR layer:

- `is_small_aggregate`, `opaque_runtime_layout`, `opaque_runtime_size`,
  `c_alignof_lir_type` from `src/lir/lower/types.rs` — the register-vs-memory
  threshold and opaque-struct layouts (`src/backend/llvm/mod.rs:19-22, 972-979, 1030`).
- `c_func_name` keyword-escaping: the local `C_RESERVED` list **must** match the
  C backend's, because the linked runtime references the mangled names
  (`src/backend/llvm/mod.rs:140-160`).
- `is_str_literal_view_init` and the HOF/combinator name parsers
  (`parse_vector_hof`, `parse_option_result_combinator`, …) — these *are*
  name-pattern parsers, but they recognize the same monomorphized-symbol forms the
  C backend's glue emits, so the two ends of the link stay consistent.

The discipline here is **read the canonical table; never re-derive a value the LIR
layer already stored.** Both backends share the *table*, not a re-computation.
The size, pointee-type, and ABI-classification facts the backend needs are
written once into canonical fields (`computed_c_size`, `func.pointee_types`) and
read identically by `c_lir` and `llvm`. When the LLVM backend instead reconstructs
one of these locally — a move-out null-zero size from a fragile `FieldPtr` scan,
or a cover-struct size from a field-sum — it diverges from C and miscompiles
(double-free, or a wrong `sret` classification → SIGSEGV). Both bugs are the same
layering smell, and the worked examples are in
[Chapter 24 — "A backend re-deriving a canonical value"](24-layering-discipline.md#a-backend-re-deriving-a-canonical-value-rule-3-at-the-backend-boundary).
This is exactly why a regression on one backend but not the other (next section)
almost always means a change touched a backend-specific *re-derivation* rather
than the shared table.

Note: the LLVM backend carries a dead-code `infer_inst_type`
(`#[allow(dead_code)]` at `src/backend/llvm/mod.rs:534`, fn at `535`) — it has
zero callers and makes no emit decisions. The live value-type recovery for
emit holes (when the shared `value_types` is `None` for a slot) is the
*homonymous* function in the C backend, `infer_inst_type`
(`src/backend/c_lir/helpers.rs:1695`), invoked at `src/backend/c_lir/mod.rs:1176`.

## Parity status and verification

The backend is at C-parity over the integration suite, per the project's build
instructions (`GG_BACKEND=llvm` "at C-parity"). The harness wiring: `gg_backend()`
reads `GG_BACKEND` (`tests/integration.rs:56`), and `gg_command` appends
`--backend=llvm` to every `build`/`test`/`run` invocation
(`tests/integration.rs:94-103`) so the *same* fixture list runs under either
backend without a forked test list. A small number of tests short-circuit under
LLVM via `skip_under_llvm()` (`tests/integration.rs:63`) for documented
LLVM-specific gaps (concurrency races / optimizer quirks); each call site states
why.

To run the LLVM sweep (use `--test-threads=1` for full runs — the parallel
runner hits cargo rebuild races; single-test runs at the default thread count
are fine):

```bash
GG_BACKEND=llvm cargo test --test integration --release -- --test-threads=1 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

A regression on one backend but not the other almost always means a change
touched a backend-specific path rather than shared LIR — the shared-code
discipline above is precisely what keeps that surface small.

## In the self-host

The Gorget self-host (`tests/fixtures/self_host_*`) goes well past the
frontend: in addition to the lexer/parser/resolver/type-checker/lowerer (see the
`*_comparison` tests), `self_host_lowerer/` reimplements the GIR→LIR lowering
(`lir.gg`, `lir_lower.gg`), SSA construction (`lir_ssa.gg`), drop elaboration
(`drop_elab.gg`), and a full LIR→**C** backend (`lir_codegen.gg`, ~5.3k lines,
spelling runtime symbols like `__gorget_box_alloc_<inner>` directly — see
`emit_box_allocators_from_lir` at `lir_codegen.gg:738`). That C path is exercised
by `c_emit_comparison` and `self_host_bootstrap_fixed_point`.

What the self-host does **not** reimplement is *this* chapter's subject — the
LLVM backend. There is no `llvm_codegen.gg`; the self-host's only code generator
emits C. So while the self-host covers the LIR→native pipeline in general, it has
no analogue of the LLVM emitter, and there is no self-host parity to report
specifically for the `.ll`-emission path described in this chapter.
