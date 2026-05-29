# 17 — The C backend

The C backend is the default production backend: it walks an `LirModule` and
emits a single `.c` translation unit (the Gorget C runtime, prepended,
plus generated struct definitions, wrappers, and function bodies). It lives
in `src/backend/c_lir/` and implements the `Backend` trait
(`src/backend/c_lir/mod.rs:3035`). (An LLVM backend ships as a second backend
behind `--backend=llvm`, held at C-parity against the full integration fixture
set — see [Chapter 19](19-llvm-backend.md).) The module file's own one-line summary is
the design thesis: *"Thin 1:1 translation from LIR to C code. No semantic
decisions — all type coercions, drop calls, vtable dispatch, etc. are already
explicit in LIR instructions."* (`src/backend/c_lir/mod.rs:1-5`).

This chapter describes how that 1:1 translation works, where the famous
remappings (`map_monomorphized_to_runtime`, self-by-ptr, cstr handling)
*actually live* — which is often **not** in this directory — and what the
"dumb backend" principle buys you.

A note on the pipeline shape: by the time codegen runs, LIR has already been
lowered through a **BIR** pass (`src/bir/`) that expands the *canonical* LIR
ops — `SizeOf`, `EnumInit`, `EnumCheck`, `EnumExtract`, `StructInit`,
`CowClone`, `TraitCall`, `HofExpand`, `AddressOf`, `BoxAlloc` (ten in all) —
into primitive instructions. The C backend never sees those — its `emit_inst`
treats every one of them as `unreachable!("canonical LIR op survived BIR
lowering")` (`mod.rs:2044-2055`). That is the most important thing to
internalise before reading the rest of this chapter: not just collection
higher-order methods, CoW clones, and trait dispatch, but also enum/struct
construction and `SizeOf` are lowered to primitives before codegen — they are
*not* the C backend's problem.

The files:

| File | Lines (approx) | What |
|------|------|------|
| `mod.rs` | ~3200 | module assembly, `emit_function`, `emit_inst`, terminators, `Backend` impl |
| `emit_types.rs` | ~2930 | struct/enum layout, runtime-module selection, `emit_abi_arg`, LIR helpers |
| `emit_call_extern.rs` | ~910 | the `Inst::CallExtern` arm (the largest single match arm) |
| `helpers.rs` | ~1990 | name/type helpers, `resolve_param_abi`, C-string escaping, reserved-word munging |
| `emit_hof.rs` | ~240 | inline **Option/Result** combinator expansion (not collection HOFs) |
| `emit_printf.rs` | ~240 | `printf` argument decomposition (Str → `%.*s`, etc.) |

(Re-derive line counts with `wc -l src/backend/c_lir/*.rs`; the table is a
snapshot.)

## The "dumb backend" principle

The central architectural claim is that **all ownership, drop, dispatch, and
coercion decisions are made upstream (in GIR→LIR lowering, the LIR passes, and
the BIR expansion), and the backend only spells them out in C.** This is the
layering-discipline "resolve once, write through" rule applied to the codegen
boundary (`CLAUDE.md` → Layering discipline, rule 4).

Concrete consequences you can see in the source:

- **Drops are explicit LIR instructions.** The backend does not decide when to
  free anything. `Inst::Load` deliberately does a *shallow* deref and never
  clones, because "the GIR drop elaborator already determines ownership: if the
  loaded value needs freeing, it emits a Drop/MoveZero"
  (`src/backend/c_lir/mod.rs:2346-2358`). The comment there names the principle:
  *"This keeps the backend 'dumb' — it reads declared facts, never re-infers."*

- **Canonical ops are gone before codegen.** The ten BIR-lowered ops
  (`SizeOf`, `EnumInit`, `EnumCheck`, `EnumExtract`, `StructInit`, `CowClone`,
  `TraitCall`, `HofExpand`, `AddressOf`, `BoxAlloc`) are `unreachable!` in
  `emit_inst` (`mod.rs:2044-2055`). If one survives, the BIR validator should
  have rejected it; the backend asserting rather than attempting a fallback is
  the layering rule made enforceable.

- **Value types are a written-through sidecar, not re-inferred.** The
  per-value type tables (`val_types`, `ptr_pointee`) are *seeded* from the
  LIR-canonical `func.value_types` / `func.pointee_types`, which are populated
  by `compute_module_value_types` / `compute_module_pointee_types` in
  `src/lir/types.rs`. The backend layers only narrow fixups on top, and only
  where the shared pass returned `None`
  (`src/backend/c_lir/mod.rs:1121-1158`).

- **ABI marshalling is a typed tag, not a name heuristic.** Each
  `Inst::CallExtern` carries `arg_abis: &[AbiKind]` resolved upstream by
  `RuntimeFn::resolve_lir_sig`; the backend reads the tag and emits the
  corresponding C (`src/backend/c_lir/emit_call_extern.rs:704-714`). The
  legacy name-based fallbacks are explicitly marked DEPRECATED safety nets
  (see [Self-by-ptr and the ABI tags](#self-by-ptr-and-the-abi-tags)).

The litmus test from `CLAUDE.md`: if the backend reconstructs a semantic fact
from a name or shape, the boundary upstream was drawn wrong. The few places
the backend *does* match on a runtime-symbol name are the sanctioned exception
— at the C-emit boundary the symbol name *is* the contract with the runtime
(`CLAUDE.md` → "No name matching"). Even there the spelling is meant to be
driven by typed metadata, not routing decisions.

## Module assembly: `generate_c`

`generate_c(module)` is the whole-module entry point
(`src/backend/c_lir/mod.rs:377`). It delegates to
`generate_c_inner_impl(module, include_runtime, wrappers_only)`
(`mod.rs:403`), which is shared with two siblings:

- `generate_c_inner(module, include_runtime)` — standalone C (used by tests
  and `--emit-c-lir`).
- `generate_llvm_wrappers(module)` — emits everything *except* user function
  bodies, to be appended to the runtime `.c` and linked against the
  LLVM-generated object (`mod.rs:390`). This is why struct layout, typedefs,
  monomorphized wrappers, globals, and the test-runner `main` all live in the
  C backend even when LLVM is selected: those are shared glue.

Emission order in `generate_c_inner_impl` (`mod.rs:403` onward):

1. **Runtime modules** (if `include_runtime`) via `emit_runtime_modules`
   (`emit_types.rs:1791`) — see [The runtime is a menu](#the-runtime-is-a-menu).
2. **Struct forward declarations**, skipping runtime-provided structs,
   monomorphized wrappers, and `c_runtime_alias`-tagged types (`mod.rs:425-467`).
   `Task__*` and `Box__*` get early forward typedefs because they appear in
   field types before their real typedef (`mod.rs:468-493`).
3. **Struct definitions, topologically sorted** by Kahn's algorithm so an inline
   `Struct(j)` field is defined before its container — otherwise C complains
   about incomplete types (`mod.rs:496-547`). Cycles (recursive types broken by
   `Box`/`Ptr`) fall back to original order (`mod.rs:533-541`).
4. **Monomorphized wrapper typedefs + inline wrappers** (`mod.rs:654-657`).
5. **Function forward declarations**, then bodies.
6. **Globals** and the **test/bench runner main**.

### Struct names and the runtime-alias remap

`build_struct_names` (`mod.rs:255`) maps each `StructId` to its C type name:

- Runtime structs and named LIR structs keep their real names (the
  `RUNTIME_STRUCTS` and `LIR_NAMED_STRUCTS` lists, `mod.rs:156-171`).
- A small fixed remap renames a handful of LIR names to runtime names —
  `lir_to_runtime_name` (`mod.rs:210`), e.g. `GorgetString` → `Str`,
  `File` → `GorgetFile`, `Socket` → `GorgetSocket`. This is one of the
  sanctioned name-spellings at the runtime boundary, not a routing decision.
- Everything else (user structs) becomes `__lir_s{id}` or `__gg_{Name}`.

### Enum layout

Enums are lowered to a tag + union. `is_union_layout` structs emit
`<tag_ty> <tag>;` followed by a `union { … } data;` whose members are grouped
by variant-name prefix (`mod.rs:555-595`). Small enums (`Option`, `Result`)
keep a flat layout for C-backend compatibility; large enums use the union form
(see `MEMORY.md` → LIR Backend). Whether a struct is `Result`/`Option` is read
from the typed `enum_kind` field — e.g. the "throws-`int`-`main`" override
checks `s.enum_kind == EnumKind::Result` rather than matching the type name
(`mod.rs:1050-1052`).

## The runtime is a menu

`emit_runtime_modules` (`emit_types.rs:1791`) does **not** paste a fixed
runtime blob. It scans every call name in the module — externs, function names,
`CallExtern` names inside bodies, and global initialiser externs
(`emit_types.rs:1794-1813`) — and conditionally `push_str`s only the runtime
sub-modules actually referenced. The runtime source itself is a set of
`pub const &str` blobs in `src/backend/c/c_runtime.rs` (e.g.
`RUNTIME_PREAMBLE`, `RUNTIME_STRING`, `RUNTIME_STRING_EXTENDED`,
`RUNTIME_ARENA_ALLOC`), with large third-party pieces embedded via
`include_str!` (`c_runtime.rs:14896` for stb_image, `14898` for the SQLite
amalgamation).

A `has(&pred)` closure tests whether any call name matches a predicate
(`emit_types.rs:1814`); e.g. the extended-string module is only emitted when a
`gorget_str_to_upper`/`_contains`/… symbol appears (`emit_types.rs:1853-1873`).
Freestanding targets short-circuit to a minimal `#include "runtime.c"` and skip
all hosted modules (`emit_types.rs:1819-1824`).

This is name-matching, and it is one of the few legitimate uses: it is a
*coarse linker-style "is this family used?"* gate over the runtime source, not
a semantic decision about what a call *means*. It can over-include (a false
positive just compiles dead runtime code) but must not under-include.

## Emitting a function

`emit_function` (`mod.rs:1046`) emits one LIR function:

- **Signature.** `main` is special-cased to `int main(int argc, char** argv)`
  with a `gorget_init_args(argc, argv)` prologue (`mod.rs:1056-1058`). A `main`
  that returns `Result` is rewritten to return `int` (the throws-main shape,
  `mod.rs:1050-1053`). Other functions emit `<ret> <name>(params)`; a `Void`
  param (closure env) becomes `void*` because bare `void` is illegal as a
  non-sole C parameter (`mod.rs:1072-1073`); `const_params` add a leading
  `const` to pointer params (`mod.rs:1074-1076`).

- **Value declarations.** SSA values are flat C locals named `__v{id}`. The
  emitter computes `max_val` across all block params and instruction
  destinations (`mod.rs:1104-1119`) and declares each `__v{id}` at its inferred
  type (seeded from `func.value_types`, `mod.rs:1146-1158`). Slots (stack
  locals) are `__s{id}`; block params get temporaries `__bp{vid}`.

- **Blocks → labels + goto.** Each LIR basic block becomes a C label
  `__bb{id}:` (`mod.rs:1834`). Terminators emit plain `goto` (`emit_term`,
  `mod.rs:2934`; the match arms span ~`2937-3015`): `Term::Jump` →
  `goto __bb{target}`, `Term::Branch` → `if (cond) goto … else goto …`,
  `Term::Switch` → a C `switch` with `goto` cases, `Term::Unreachable` →
  `__builtin_unreachable()`.

- **Block params are the phi mechanism.** SSA block params are realised with a
  parallel-copy idiom: a `Jump`/`Branch`/`Switch` with arguments stores each
  arg into the target's param temporary `__bp{vid}` *before* the `goto`
  (`emit_jump_args`, `mod.rs:3018-3027`), and at block entry each param is
  copied out of its temporary into the value local: `__v{vid} = __bp{vid};`
  (`mod.rs:1846-1849`). The temporary breaks the swap/cycle problem a naive
  direct assignment would have; the backend does **not** do SSA repair, phi
  insertion, or scope tracking — the LIR is already in block-param SSA form and
  this is a mechanical lowering.

- **Per-instruction panic locations.** `emit_inst` takes a resolved
  `(file, line, col)` triple for inline panic messages, but resolving it is
  gated by `inst_needs_loc` (`mod.rs:132-152`) so only instructions that can
  trap (overflowing arithmetic, div/rem, shifts, bounds/div checks, and —
  conservatively — every `CallExtern`) pay for the span lookup. Eager
  per-instruction resolution was a measured codegen regression
  (`mod.rs:1851-1854`, and the docstring on `resolve_panic_loc`,
  `mod.rs:97-118`).

## Instruction emission: `emit_inst`

`emit_inst` (`mod.rs:1909`) is a big `match` over `Inst`. A few load-bearing
arms:

- **`SlotStore`** (`mod.rs:1922`) handles the string/cstr coercion cases — see
  [cstr handling](#cstr-handling). It reads the slot's declared type and the
  value's *origin* (string literal? cstr? null?) and picks a direct assign, a
  `gorget_str_from_cstr` / `gorget_string_adopt`, a `memset`-to-zero (the `None`
  case), or a `memcpy` (move vs CoW copy) accordingly.

- **`Load`** (`mod.rs:2346`) is the shallow-deref described under
  [the dumb-backend principle](#the-dumb-backend-principle). When `ty` is
  `Void` it falls back to the `ptr_pointee` table rather than emitting illegal
  `*(void*)`.

- **`CallExtern`** delegates to `emit_call_extern` (`mod.rs:2585`, body in
  `emit_call_extern.rs`).

- **Canonical ops** (`SizeOf`, `EnumInit`, `EnumCheck`, `EnumExtract`,
  `StructInit`, `CowClone`, `TraitCall`, `HofExpand`, `AddressOf`, `BoxAlloc`)
  are `unreachable!` (`mod.rs:2044-2055`) — they were expanded by the BIR pass
  and never reach codegen.

### Value origins, not bitmaps

Per-value facts (is this a string literal? a NULL pointer? a const-char\*
return? a function address?) are read through the `EmitContext` typed accessors
(`mod.rs:39-95`), which dispatch on `func.value_origins[v]: Option<ValueOrigin>`.
The docstring (`mod.rs:21-24`) is explicit that these replaced the older
parallel per-value bitmaps — one typed sidecar, read through one accessor, per
the layering rules. For example `is_cstr_extern` (`mod.rs:69`) distinguishes an
extern-"C" cstr return (use the safe `gorget_str_from_cstr`) from a runtime-fn
cstr return (adopt the heap allocation, no copy).

## `map_monomorphized_to_runtime` lives in the LIR lowerer, not the backend

This is the single most important layering fact about the backend, and it
contradicts a common (and stale) assumption that the remapping is a C-backend
concern. `map_monomorphized_to_runtime` and its table-aware variants are
defined in **`src/lir/lower/calls.rs:270`** (and `:216`, `:241`), part of the
GIR→LIR lowering — **not** in `src/backend/c_lir/`. By the time the backend
sees an `Inst::CallExtern`, the callee name has *already* been rewritten from
the monomorphized GIR name (`Vector__GorgetString__push`) to the runtime symbol
(`gorget_array_push`).

How it works (`calls.rs:270-420+`):

- The "which family is this?" decision reads the typed `method_prefix` field
  from `compiler/data/resources.gg`'s resource table
  (`crate::ir::resources::table().lookup(name)`, `calls.rs:280-281`) — *not* a
  `name.starts_with("Vector__")` test. This is the layering rule-2 fix called
  out in the source comment (`calls.rs:270-279`).
- Within a family, the method name selects the runtime symbol:
  `push` → `gorget_array_push`, `get` → `gorget_array_safe_get` (non-panicking),
  `pop` → `gorget_array_safe_pop`, `remove` → `gorget_array_remove_opt`
  (`calls.rs:330-335`); for `Dict`, `set` → `gorget_map_put`, `has` →
  `gorget_map_contains`, `Dict.new()` → `gorget_dict_new` (ordered)
  (`calls.rs:342-358`); for `String`, `GorgetString__substring` →
  `gorget_str_slice`, `trim_left` → `gorget_str_lstrip`, etc.
  (`calls.rs:392-402`).
- **Higher-order methods return `None`** (`calls.rs:296-304`) so the
  monomorphized GIR name is *kept* — `filter`, `map`, `flat_map`, `fold`,
  `reduce`, `any`, `all`, `each`, `find`, `count`, and friends are *not*
  runtime functions. But that kept name does **not** reach the C backend as a
  `Vector__T__map` `CallExtern`: later in GIR→LIR lowering these calls are
  rewritten into `Inst::HofExpand` instructions
  (`src/lir/lower/insts.rs:2571`, `:2739`, `:3206`), which the **BIR** pass
  then expands into primitive loops (`src/bir/lower.rs:302`) — *before* codegen
  runs. The C backend never generates collection-HOF loops; it asserts
  `unreachable!` if a `HofExpand` survives (`mod.rs:2044-2055`), and
  `emit_call_extern.rs:432-438` confirms "Vector/Dict/Set higher-order methods
  no longer dispatch here". (`emit_hof.rs` is unrelated — it inlines only
  **Option/Result** combinators; see `emit_hof.rs:1-13`.)
- The element-typed sort variants (`gorget_array_sort_int` / `_float` / `_str`
  / `_generic`) are selected here so qsort gets the right comparator
  (`calls.rs:318-328`); the typed overload
  `map_monomorphized_to_runtime_with_operand_types` derives the suffix from the
  receiver's `LirType::Resource { params }` instead of stripping the name
  prefix, when the operand is available as typed `Resource` (`calls.rs:241-268`).

`fix_printf_format` (the `%lld`→`%f`/`%.*s` rewrite) also moved out of the
backend into the lowerer (`calls.rs:22`); `helpers.rs:1623-1625` documents the
move. The backend's `emit_printf.rs` only does the *structural* decomposition
of a Str argument into a `(len, data)` pair, which can't be decided at lowering
time for polymorphic `void*` returns (`emit_call_extern.rs:773-784`).

## Self-by-ptr and the ABI tags

Collection runtime functions take `self` (and element args) by pointer. The
backend decides each argument's marshalling from the typed `AbiKind`
(`src/ir/abi.rs:13-50`), in priority order at the call site
(`emit_call_extern.rs:703-714`):

1. **Instruction-level tag** — `arg_abis[i]`, resolved upstream by
   `RuntimeFn::resolve_lir_sig`. If non-`Auto`, `emit_abi_arg` honours it.
2. **Extern-declaration tag** — `resolve_param_abi(ext_decl, name, i)`
   (`helpers.rs:1548`) reads `ext.param_abis[i]` (from `extern "C"` blocks /
   `T*` syntax).
3. **Legacy whitelist fallback** — only for unmapped GIR names that bypass
   `resolve_lir_sig`. `legacy_self_by_ptr` (`helpers.rs:751`) hardcodes a few
   `gorget_str_*` names plus a structural "`gorget_array_`/`map_`/`set_`/`heap_`/
   `bytes_` and not `_new`" rule, and forces arg 0 to `AbiKind::Ptr`
   (`helpers.rs:1560-1564`). Both this and `collection_void_param_indices`
   (`helpers.rs:681`) are marked **DEPRECATED** safety nets — *"All CallExtern
   instructions now carry `arg_abis`"* (`helpers.rs:678-680`, `748-750`).

`emit_abi_arg` (`emit_types.rs:706`) is the actual C-spelling switch:

- `AbiKind::Ptr` — struct arg → take address; pointer arg → pass through; a
  scalar reaching `Ptr` trips a `debug_assert!` because the tag would be wrong
  (`emit_types.rs:777-784`). This is the self-by-ptr path: a `Vector` receiver
  arrives as a struct/pointer and is passed as a `void*` to `gorget_array_*`.
- `AbiKind::ByValue` — aggregate by value; if the arg is a pointer it falls
  back (returns `false`) so the deref cascade can use the extern's param type
  (`emit_types.rs:767-776`).
- `AbiKind::VoidElem` — element/key/value pointers: `&(Type){val}` for concrete
  scalar values (typed compound literal), `&val` for Str-literal/struct values,
  pass-through for pointers (`emit_types.rs:796-825`; the enum variant doc is
  `abi.rs:49`).

The `debug_assert!`s in `emit_abi_arg` are the backend's way of *failing loud
when upstream lied* — a scalar tagged `Ptr` means the ABI was mis-resolved one
layer up, and the fix belongs there, not in a backend special-case.

## cstr handling

Gorget's `String` is a 32-byte `Str` struct (`{data, cap, len, alloc}`,
view-discriminator layout, see `MEMORY.md` → LIR Backend). C functions that
want a `const char*` need the `data` pointer, possibly null-terminated. The
backend handles this on two sides:

- **Passing a Gorget string to a C-string param** — `AbiKind::CStr`
  (`emit_types.rs:716-735`): a Str struct/literal → `(const char*)val.data`; a
  pointer-to-Str → `(val ? gorget_str_to_cstr(*(Str*)val) : NULL)` (which
  guarantees null-termination). `AbiKind::BytePtr` is the no-null-termination
  variant (`emit_types.rs:736-750`). Functions like the integer/float parsers,
  which need a genuinely null-terminated buffer, are handled at bespoke emit
  sites (`emit_call_extern.rs:351-366`): they call `gorget_try_parse_int` /
  `gorget_try_parse_float`, converting the argument with the `coerce_arg_to_cstr`
  helper (which yields a null-terminated cstr) before the call.

- **Receiving a C-string return into a Gorget string slot** — the `SlotStore`
  arm (`mod.rs:1947-1963`). If the value's origin is a cstr and the slot is a
  `Str`/`GorgetString`:
  - extern-"C" return → `gorget_str_from_cstr(v)` (may be static or heap, so
    copy safely) (`mod.rs:1949-1950`);
  - runtime-fn return → `gorget_string_adopt((char*)v)` (heap-allocated, adopt
    ownership, no leak, no copy) (`mod.rs:1952-1953`).

  The extern-vs-runtime distinction is the typed `is_cstr_extern` origin
  (`mod.rs:69`), not a name list.

String *literals* assigned to a `Str` slot are a direct struct assign — the
literal is a static `.rodata` view (cap=0), zero-alloc (`mod.rs:1944-1946`; the
global-init counterpart is `is_str_literal_view_init`, `helpers.rs:1635`).

## Folded internals docs

Two older internals memos fold into this chapter. Both describe work that has
since shipped; the *evergreen* lessons are lifted below in past tense, with
status re-derived from current source rather than copied from the (stale)
doc text.

- **`docs/internals/codegen-gap-spike.md`** ("`equip` on primitive receivers").
  The gap was that every equip-lowering site filtered on `Type::Named`, so
  `equip String:` was silently dropped and the call fell through to
  `map_monomorphized_to_runtime`, which rewrote `GorgetString__user_method` to a
  nonexistent `gorget_str_user_method`. The fix landed; the *evergreen* lesson
  for this chapter is that `map_monomorphized_to_runtime`'s `GorgetString__*`
  rule is aggressive — it would remap a *user* equip method — and the only thing
  that saves it is the registered-function check during LIR lowering, which skips
  the remap when the function exists in `func_index` (`src/lir/lower/insts.rs`
  guards every dispatch on a `self.func_index.get(func)` lookup, e.g.
  `insts.rs:438`, `:593`, `:1775`). The takeaway: the remap is a *fallback for
  unmapped names*, and any path that bypasses the func-index check would
  reintroduce the gap.

- **`docs/internals/tier1c-cluster1-burn-down.md`** (making `Option`/`Result`
  full Resource types by removing the `monomorphize_enum` carve-out at
  `src/ir/lowering/generics/mod.rs`). This **shipped** (commit `c779d976`,
  2026-05-11; `DONE.md` "Tier 1c COMPLETE"). The carve-out is gone:
  `generics/mod.rs:2455` now computes the drop strategy
  *unconditionally* — `registry.compute_drop_strategy_for_enum(&variants)`
  for every enum including `Option`/`Result` — so they are full Resource types
  rather than special-cased Copy values. (The internals doc still reads
  "Status: Active"; it is stale and should be marked closed.) The
  backend-visible lesson is the *cross-type adapter destination-size bug*: when
  an `Option`/`Result` combinator changes a payload type (`map`/`map_err`
  turning `Result[int, String]` into `Result[int, int]`), the destination local
  must take the *new* result type, or the backend emits a
  `memcpy(dst, src, sizeof(OLD_type))` that overreads. The fix is at the
  VarDecl-from-adapter-result site in GIR lowering:
  `src/ir/lowering/exprs/methods.rs:2867-2924`
  computes the cross-type result type (building the new `Result`/`Option` def
  via `make_result_type_def` / `ensure_option_type_registered`) and allocates
  the result local with it (`src/ir/lowering/exprs/methods.rs:2924`) — a
  textbook "fix at the write
  site" per the layering debugging heuristic. The backend faithfully emits
  whatever size token the LIR hands it.

## In the self-host

Self-host C emission is the **biggest parity gap** of any self-host stage. The
self-host lowerer/C-emitter live in `tests/fixtures/self_host_lowerer/`
(`lower.gg`, `lir_lower.gg`, etc.), and the comparison harness is
`c_emit_comparison` (`tests/integration.rs:13549`).

That test is **diagnostic-always-pass**: it counts user-function definitions in
the emitted C (`user_fn_count`, looking for `) {`-terminated lines after the
"Function Definitions" marker, `tests/integration.rs:13565-13593`), compares the
Rust `gg`'s count against the self-host driver's count per fixture, and prints a
summary — there is **no `assert!` in the test body**. A green `cargo test` says
*nothing* about parity; only the printed match-rate does, and "matched" means
*fn-count parity*, not byte-identical C, so true byte-parity is lower still.

To read current parity, run the test with `--nocapture` and read the printed
"Match rate" / crash / mismatch lines (`tests/integration.rs:13661-13704`):

```bash
cargo test --test integration c_emit_comparison -- --nocapture
```

As of the last recorded reading (`MEMORY.md`, 2026-05-29) this stood at roughly
64% fn-count-matched with a handful of self-host crashes — by far the widest gap
of the self-host stages (lexer/parser/resolver/typechecker all sit well above
95%). **Re-run the command before quoting any number; the figure in `MEMORY.md`
is dated the moment it was written.** The recurring crash signature has been a
String-returning builtin missing from the self-host's return-type list →
result mis-typed as `I64_TYPE` → empty `(Str){0}` substituted at a string
boundary → downstream corruption (`MEMORY.md` → NORTH STAR / blocker chain); a
new such crash should first be suspected in the self-host's
`builtin_call_return_type` / `infer_method_return_type`, mirroring the Rust
`map_monomorphized_to_runtime` family routing.

Note also that closing the C-emit gap is the explicit north-star item: parity
means the self-host compiles all fixtures the *same way* Rust `gg` does, which
for this stage means matching emitted C, not merely producing *some* linkable C
(that weaker property is what `self_host_bootstrap`
(`tests/integration.rs:13724`) and `self_host_bootstrap_fixed_point`
(`tests/integration.rs:13897`) lock in).
