# Fidelity Design v1 — Self-host c_emit CLASS-1: vtable-global emission + DCE seeding

**Author:** compiler architect agent
**Date:** 2026-05-29
**Scope:** Close the dominant remaining c_emit parity gap ("CLASS 1") — the
self-host drops ~39 trait-impl method bodies for `json_parse.gg`
(Rust = 67 user fns, self-host = 28). Root cause is **upstream**: the
self-host emits NO vtable globals, so its C-emit DCE
(`compute_reachable_fns`) has nothing to follow and prunes the entire
trait-impl island. Rust keeps the island live because it emits a
struct-of-FuncAddr `_VTable` global for **every** non-generic trait impl
unconditionally, and its DCE seeds reachability roots from those globals'
FuncAddr fields.

This is a **DESIGN, not code.** Every load-bearing claim is cited to
`file:line` against current source. A downstream reviewer should verify
each.

---

## 0. Ground truth (empirically re-confirmed this session)

Built `gg` at this worktree and emitted Rust C for json_parse:

```
./target/debug/gg build --emit-c-lir tests/fixtures/json_parse.gg
```

Confirmed facts:

- **Rust user_fn_count = 67** (counted by the harness metric: lines after
  `// ── Function Definitions ──` starting at col 0 ending `) {`).
- **Rust emits exactly 7 vtable globals**, `__lir_g0..g6`
  (`/tmp/json_rust.c:3908-3914`):
  - `g0 = __gg_Error_VTable {(void*)&Error_for_IoError__source}`
  - `g1 = __gg_Writer_VTable {(void*)&Writer_for_GorgetString__write, …__flush}`
  - `g2 = __gg_Writer_VTable {(void*)&Writer_for_File__write, …__flush}`
  - `g3 = __gg_Reader_VTable {(void*)&Reader_for_File__read}`
  - `g4 = __gg_Error_VTable {(void*)&Error_for_ParseError__source}`
  - `g5 = __gg_Deserializer_VTable {18 fields}`
  - `g6 = __gg_Serializer_VTable {12 fields}`
- Each vtable global's C type is a `__gg_<Trait>_VTable` **typedef'd struct**
  whose fields are precise fn-ptr types (e.g.
  `__gg_Result__int64_t__IoError (*write)(const void*);`), but each global
  **initializer field is a plain `(void*)&<mangled_impl_fn>` cast**
  (`/tmp/json_rust.c:3908`). The fixture never instantiates a
  JsonSerializer — Rust emits the global **unconditionally** for the impl.
- Globals with FuncAddrs are emitted **after** the function forward
  declarations (`/tmp/json_rust.c`: g0 at line 3908, forward-decls precede;
  Function Definitions marker at line 4250). This matches Rust's
  deferred-global mechanism in `src/backend/c_lir/mod.rs:758-867`.

The **+39** is the union: Serializer(12) + Deserializer(18) + Writer-for-
File/String(2+2) + Reader-for-File(1) + Error.source×2(2) + the
`Json__keys`/`errno_to_io_error` tail reached transitively. These trait
impls live in `lib/std/io.gg`, shared across ~150 fixtures.

The harness metric (`tests/integration.rs:13565-13593`, `user_fn_count`)
counts only function-body opening lines after `// ── Function
Definitions ──`. **The vtable globals/typedefs themselves do not count**
— what counts is the +39 method **bodies** becoming reachable through the
DCE gate at `lir_codegen.gg:5246`. This means the `_VTable` struct
typedef's field spellings (precise fn-ptr vs `void*`) are **irrelevant to
the metric**; only the bodies' reachability matters. (Byte-parity of the
typedef block is a separate, later concern — see §7 Risk (e).)

---

## 1. Approach decision: (a) typed metadata, end-to-end — MANDATED, and why (b) is rejected

### Approach (a) — ADOPTED

Model the vtable as a struct-of-FuncAddr global threaded through
GIR Global → LIR Global → C emit, exactly as Rust does:
`src/ir/lowering/traits.rs:985 emit_vtable_globals` builds
`Global { init: GlobalInit::Struct { fields: [GlobalInit::FnRef(name), …] } }`
(`src/ir/mod.rs:721-741`), which lowers to
`LirGlobalInit::Struct { struct_id, fields: [LirGlobalInit::FuncAddr(fid), …] }`
(`src/lir/mod.rs:1587-1607`, `src/lir/lower/types.rs:580-594`), and the C
backend emits `__gg_<Trait>_VTable __lir_gN = {(void*)&fn0, …};`
(`src/backend/c_lir/emit_types.rs:656-672`). DCE seeds roots from these
globals' FuncAddr fields (`src/lir/optimize.rs:268-275`,
`collect_global_func_refs:364-374`) and follows `Inst::FuncAddr`
transitively (`collect_inst_func_refs:358`).

### Approach (b) — REJECTED: "emit vtable as raw C text in codegen + register fids as DCE roots by name-scanning `*_for_*__*`"

Rejected on two independent grounds:

1. **Violates CLAUDE.md layering discipline rule 2 ("Typed metadata, not
   name-matched") and the "No name matching" sub-rule.** Approach (b)
   would make a *semantic* reachability decision — "this fn is a live
   vtable slot" — by substring-matching `name.contains("_for_") &&
   name.contains("__")` on the function-symbol string. That is precisely
   the forbidden pattern: a downstream pass reconstructing a fact (vtable
   membership) from an identifier shape. Per the layering doc's litmus:
   "if a downstream pass reconstructs information from names … the boundary
   upstream was drawn wrong. The fix is always upstream — add the field,
   write it at the source, read it at the consumer." The missing fact
   (which fns are vtable slots) belongs as a typed FuncAddr field on a
   typed Global, set once at the lowering source.

   It is also a *false* root set: `*_for_*__*` matches the inherited-
   override methods that lower.gg emits with `Type__method` naming
   (`lower.gg:9716-9720`) and any user fn that happens to contain
   `_for_`, while *missing* the transitive `Json__keys`/`errno_to_io_error`
   tail that is reached through ordinary call edges *from* the seeded
   methods — exactly the tail the typed approach gets for free via the
   existing `ICallExtern`-by-name transitive edge
   (`lir_codegen.gg:1071-1101`).

2. **Does not reproduce Rust's actual globals, so c_emit byte-parity stays
   diverged.** The North Star is c_emit parity with Rust. Rust's emitted C
   *contains* the 7 `__lir_gN` vtable globals and their `__gg_<Trait>_VTable`
   typedefs (`/tmp/json_rust.c:3484-3645, 3908-3914`). If the self-host
   only seeds DCE roots without emitting matching globals, the two C files
   structurally differ forever (7 globals + 6 typedefs missing). The
   `user_fn_count` metric would reach 67, but the deeper byte-parity goal
   (and `bootstrap_fixed_point`'s self-reproduction, which compiles the
   self-host *with itself*) would not converge on the global section.
   Approach (a) emits the globals as Rust does, so both the metric and the
   structural shape align.

**Cite in the implementing PR:** `docs/devbook/24-layering-discipline.md`
rules 1-2 + the "No name matching" section.

---

## 2. The gap, precisely (writer-site trace per CLAUDE.md debugging heuristic)

Tracing the missing reachability edge to its WRITE site, layer by layer:

- **C-emit DCE read site** (`lir_codegen.gg:918 compute_reachable_fns`,
  transitive walk `:1028-1103`): the `match` at `:1052-1061` has arms for
  `ICall`, `ICallExtern`, `IClosurePack` — **no `IFuncAddr` arm**, and the
  root-seeding loop (`:944-1026`) **never scans `m.globals`**. But this is
  not the disease — there is nothing to follow.

- **LIR model** (`lir.gg:285 LirGlobal`): models only a SCALAR init —
  `init_kind` ∈ {`GINIT_ZEROED=0`, `GINIT_FUNC_ADDR=1`,
  `GINIT_RUNTIME_CALL=2`} (`lir.gg:281-291`), with `init_func_id` (scalar
  FuncAddr) and `init_expr` (runtime-call C expr). **No struct-of-FuncAddr
  variant.** (The C-emit at `lir_codegen.gg:833-835` already has a
  `GINIT_FUNC_ADDR` arm but emits `= {0}` placeholder — dead scaffold,
  never exercised.)

- **GIR model** (`gir.gg:279 GirModule`): has NO `globals` field and NO
  `Global`/`GlobalInit` type at all. The only module-level global-ish
  state is `statics: Dict[String, GirStaticInfo]` (`gir.gg:326`,
  `GirStaticInfo:275` = `{init_expr, type_id}`), which models *runtime-
  call-init* statics (`stdout`/`stderr`/`stdin` from io.gg) — not vtables.

- **Lowering source** (`lower.gg:9686-9748`, the `did_split` block for
  explicit non-derive trait equips, and `lower_equip_block:7573`): lowers
  the trait-impl method **bodies** into `gmod.functions` with mangled names
  `Trait_for_Type__method` (`lower.gg:7589`), but **never emits a vtable
  global** referencing them. This is the true write-site of the gap.

**Verdict:** the bug is at the lowering write-site (no vtable global
emitted), cascading down. The "complex" C-emit fix (DCE arms) is small and
correct *once the globals exist* — but it is downstream of the real
omission. Fix order is bottom-up: model → emit → C-render → DCE.

---

## 3. Mechanism — full spec, grounded in current source

### 3.1 GIR Global model extension (`gir.gg`)

Add a typed Global model mirroring Rust's `ir::Global` /
`ir::GlobalInit` (`src/ir/mod.rs:713-741`). Keep it **minimal** — only the
variants needed for vtables (Rust's `Bytes`/`Extern` GIR variants are not
needed here because the self-host already routes runtime-call statics
through the separate `statics` channel).

```
# gir.gg — new, appended near GirStaticInfo (~:273)

# Global initializer kind tags. ADD-AT-END discipline (MEMORY: inserting
# mid-enum shifts ordinals → regressions). These are int consts, not an
# enum, matching the LIR GINIT_* style already in lir.gg:281.
const int GGINIT_ZEROED    = 0
const int GGINIT_FN_REF    = 1   # scalar &fn (by name)
const int GGINIT_STRUCT    = 2   # struct-of-inits (vtable)

# One field of a struct global init. Recursive via kind tag (Rust's
# GlobalInit::Struct { fields: Vec<(String, GlobalInit)> }). For vtables
# every field is GGINIT_FN_REF, so a flat representation suffices: a
# struct field carries a kind + the fn name (for FN_REF). Nested structs
# are not needed for vtables; if ever required, add a GGINIT_STRUCT-typed
# child vector — defer until a real case appears (YAGNI; Rust supports
# nesting but vtables never nest).
struct GirGlobalField:
    int kind          # GGINIT_* (only GGINIT_FN_REF used for vtables)
    String fn_name    # for GGINIT_FN_REF: the mangled impl fn name

struct GirGlobal:
    String name           # e.g. "Serializer_for_JsonSerializer_vtable"
    int type_id           # GIR type id of the _VTable struct (GtNamed)
    int init_kind         # GGINIT_STRUCT for vtables
    Vector[GirGlobalField] fields   # struct-init fields (ordered slots)
    bool is_const
```

Add the storage field to `GirModule` (`gir.gg:279`), **appended at the
end** of the struct field list (after `option_ref_payload:363`) to avoid
shifting any positional constructor call sites:

```
    Vector[GirGlobal] globals
```

> **CRITICAL ordinal/constructor note (Risk §7b):** `GirModule` is
> constructed positionally somewhere. Adding a field at the END is the
> safe choice, but **every `GirModule(...)` constructor call must gain the
> new trailing arg**. Grep `GirModule(` across `self_host_lowerer/` +
> `self_host_typechecker/` before landing; the lowerer's `gir.gg` is its
> own copy (NOT symlinked — see §4), but `GirModule` may also be
> constructed in symlinked files. If `GirModule` is only ever built via a
> helper/`new`-style fn, update that one site. Verify with
> `grep -rn 'GirModule(' tests/fixtures/self_host_*`.

Add accessor + builder helpers near `static_put` (`gir.gg:376`):

```
int gir_mod_add_global(GirModule &gmod, GirGlobal g):
    int id = gmod.globals.len()
    gmod.globals.push(g)
    return id
```

### 3.2 LIR Global model extension (`lir.gg:285`)

Add a struct-of-FuncAddr init variant, mirroring
`LirGlobalInit::Struct { struct_id, fields: Vec<LirGlobalInit> }`
(`src/lir/mod.rs:1594-1598`). The self-host encodes init-kind as the
`GINIT_*` int tag, so add **one new tag at the end** and the field storage:

```
# lir.gg:281 — ADD AT END (do NOT renumber existing 0/1/2)
const int GINIT_STRUCT = 3        # struct-of-FuncAddr (vtable)

# lir.gg:266 — a struct global init field. For vtables: a FuncAddr by fid.
struct LirGlobalField:
    int kind          # reuse GINIT_FUNC_ADDR for fn-ptr fields
    int func_id       # for GINIT_FUNC_ADDR
    # (room to grow: nested GINIT_STRUCT not needed for vtables)

# lir.gg:285 — extend LirGlobal
struct LirGlobal:
    String name
    int ty                      # LirType (the _VTable struct: LT_STRUCT_BASE + sid)
    int init_kind               # GINIT_* constant (now incl. GINIT_STRUCT)
    int init_func_id            # for GINIT_FUNC_ADDR
    String init_expr            # for GINIT_RUNTIME_CALL
    bool is_const
    Vector[LirGlobalField] struct_fields   # for GINIT_STRUCT  (NEW, at end)
```

> Adding a trailing field to `LirGlobal` means every `LirGlobal(...)`
> constructor must gain the trailing arg. There are 2 today:
> `lir_lower.gg:3609` (runtime-call statics) and any in `lir.gg` defaults.
> Grep `LirGlobal(` across `self_host_lowerer/`. The runtime-call site
> passes an empty `[]` for `struct_fields`.

### 3.3 The vtable-emission pass (`lower.gg`)

**Where hooked:** Inside the existing `did_split` block,
`lower.gg:9694-9746` — the *only* place that already (a) knows the equip
is a non-derive, non-generic, explicit trait impl whose trait is in
`trait_defs`, and (b) has `tdef` (the `TraitDef` with the full ordered
method list) and `target_name`/`eq_target_sp` in hand. This is the exact
analog of Rust's `emit_vtable_globals` gate
(`src/ir/lowering/traits.rs:990-1009`):
`if trait_name.is_empty() || equip.generic_params.is_some() { continue }`
+ `trait_info.get(&trait_name)` lookup.

**Self-host gate (mirror, using existing predicates):**

- `eqblk.trait_name` is `Some(tname)` (`lower.gg:9691`)
- `not is_builtin_trait(tname)` (`lower.gg:9693`, `is_builtin_trait:7556`)
  — excludes derive traits (Serializable/Hashable/Equatable/…). Matches
  Rust treating built-in traits via the separate
  `register_unregistered_trait_equip_sigs` path (`traits.rs:1047`), which
  does NOT emit vtable globals.
- `trait_defs.contains(tname)` (`lower.gg:9694`) — the trait is a real
  `Item::Trait` in this module (analog of `trait_info.get(&trait_name)`).
- non-generic equip: the surrounding `if not is_generic_equip:`
  (`lower.gg:9686`) already gates this — exactly Rust's
  `equip.generic_params.is_some()` skip.

**Slot ordering — HOW to obtain the trait's full ordered method slots:**
Read `tdef.methods` (`ast.gg:201`, `Vector[FunctionDef]`, in trait
declaration order). Filter to **instance methods only** (first param is
`self`) — mirror Rust's `register_trait_types` skip of static methods
(`traits.rs:88-94`: `has_self` check). For each slot, build the mangled
impl fn name `mangle_trait_name(tname) + "_for_" + target_name + "__" +
slot.name` — the **same** mangling `lower_equip_block` uses for the bodies
(`lower.gg:7588-7589`), guaranteeing the FnRef names resolve to real
emitted fns.

> **Subtlety (matches Rust exactly):** Rust's `emit_vtable_globals` builds
> one FnRef per **trait slot** (`traits.rs:1015-1028`), naming
> `{trait}_for_{type}__{slot.name}` *regardless of whether this impl
> overrides the slot or inherits a default*. Default methods are lowered
> under the `Trait_for_Type__` name too (`lower.gg:9727-9734`), and
> inherited-override methods under `Type__` naming (`:9716`). So a slot
> whose impl is a default still resolves correctly to the
> `Trait_for_Type__slot` default-method body the self-host emits at
> `lower.gg:9731-9734`. **Verify** (Risk §7d): for every trait slot, the
> name `Trait_for_Type__slot` must exist in `gmod.functions` after the
> `did_split` block runs. The default-emit loop at `:9727` emits exactly
> those; the `own_methods` lowering at `:9713-9715` emits the overridden
> ones with the same prefix. Inherited *parent-trait* defaults are emitted
> under `Type__` (`:9741`) — but those are slots of the PARENT trait, which
> gets its OWN vtable global from its OWN equip block, so they are not
> slots of `tdef` here. No mismatch.

**Emission (build a `GirGlobal`):**

```
# inside the `if not is_derive_sp and trait_defs.contains(tname):` block,
# AFTER the method bodies are lowered (after lower.gg:9746), guarded so
# it runs once per equip:
String vt_struct_name = mangle_trait_name(tname) + "_VTable"   # "Serializer_VTable"
int vt_tid = lookup_or_register_named(&gmod, vt_struct_name)   # gir.gg:437 pattern
Vector[GirGlobalField] vt_fields = []
for tslot in tdef.methods:
    # instance-only filter (first param self) — mirror traits.rs:88-94
    if method_has_self(tslot):
        String impl_fn = mangle_trait_name(tname) + "_for_" + eq_target_sp + "__" + tslot.name
        vt_fields.push(GirGlobalField(GGINIT_FN_REF, impl_fn))
String vt_global_name = mangle_trait_name(tname) + "_for_" + eq_target_sp + "_vtable"
gir_mod_add_global(&gmod, GirGlobal(vt_global_name, vt_tid, GGINIT_STRUCT, vt_fields, false))
```

**The `_VTable` struct TypeDef itself** must be registered so
`map_gir_type` resolves `vt_tid` to a real LIR struct (else the global's
C type is `void*` and the typedef is absent). Mirror Rust's
`register_trait_types` VTable struct creation
(`traits.rs:178-199`): when first processing each non-derive trait
(iterate `trait_defs` once, or lazily on first impl), register a
`GirTypeInfo`/type-def for `<Trait>_VTable` whose fields are one fn-ptr
per instance slot. **For the `user_fn_count` metric the field types don't
matter**, so model each field as `GtPtr(UNIT)` (→ `void*` in C) for v1
simplicity; this gives a valid C struct `struct __gg_<Trait>_VTable {
void* slot0; … };`. (Byte-parity of the precise fn-ptr field spellings is
deferred — Risk §7e.) The struct must enter `m.structs` via the normal
`lower_type_defs` path (`lir_lower.gg:681`) so it gets a `sid` and
`c_type_name(LT_STRUCT_BASE+sid)` → `__gg_<Trait>_VTable`
(`lir_codegen.gg:94-98`).

> **Where to register the `_VTable` type-defs:** add a small pre-pass over
> `trait_defs` near the start of the equip-lowering region (after
> `trait_defs` is built at `lower.gg:9526-9531`), registering a `_VTable`
> `GirTypeInfo` for each non-derive non-generic trait. This mirrors Rust's
> `register_trait_types` running as a pre-scan before equip lowering
> (`traits.rs:63`, called early in `mod.rs`). Registering for a trait that
> has no impl in this module is harmless (the global is what's gated on the
> impl; an unused typedef is dropped by struct DCE / `should_skip_struct`
> at `lir_codegen.gg:455` if unreferenced — verify it's referenced by the
> global's type so it's kept).

### 3.4 GIR → LIR translation (`lir_lower.gg`)

In `lower_gir_to_lir` (`lir_lower.gg:3589-3632`), after the runtime-call
statics loop (`:3605-3610`), add a loop over `gmod.globals` that translates
each `GirGlobal` to a `LirGlobal`. Build the `func_index`
(name→fid, already built at `:3612-3618`) **before** this loop so FnRef
names resolve to fids (analog of Rust's `lower_global_init` resolving
`GlobalInit::FnRef(name)` via `func_index.get(name)`,
`src/lir/lower/types.rs:573-579`):

```
# after func_index is built (move the existing :3612-3618 block up, or
# add a second pass). For each GirGlobal:
for gg_global in gmod.globals:
    int g_ty = map_gir_type(gg_global.type_id, &gmod, &sr)   # → LT_STRUCT_BASE+sid
    Vector[LirGlobalField] lf = []
    for f in gg_global.fields:
        if f.kind == GGINIT_FN_REF:
            int fid = -1
            if func_index.contains(f.fn_name):
                fid = func_index.get(f.fn_name).unwrap()
            lf.push(LirGlobalField(GINIT_FUNC_ADDR, fid))
    lir_mod_add_global(&m, LirGlobal(gg_global.name, g_ty, GINIT_STRUCT, -1, "", gg_global.is_const, lf))
```

> If a FnRef name has no fid (e.g. a default that wasn't emitted), Rust
> falls back to `LirGlobalInit::Zeroed` (`types.rs:577`). Self-host analog:
> push a `GINIT_ZEROED` field (kind=GINIT_ZEROED, func_id=-1) so the C
> emit produces `NULL` for that slot — never a dangling reference. This
> matches Rust and keeps the global well-formed. (Should not happen given
> §3.3's default-emit guarantee, but defensive parity with Rust.)

### 3.5 C emit (`lir_codegen.gg`)

Two edits, mirroring Rust's deferred-global split
(`src/backend/c_lir/mod.rs:747-867`):

**(i) `emit_globals` (`lir_codegen.gg:818`):** add a `GINIT_STRUCT` arm.
But struct-of-FuncAddr globals reference functions, so they must be
**deferred** past the forward declarations (Rust does this:
`mod.rs:758-867` splits plain globals first, FuncAddr-containing globals
after forward-decls). Refactor `emit_globals` into two helpers:

```
# emit_globals: plain (non-FuncAddr) globals only — keep the existing
# GINIT_ZEROED / GINIT_RUNTIME_CALL arms; SKIP GINIT_STRUCT and any
# GINIT_FUNC_ADDR (defer them). Mirrors mod.rs:759-768 (has_func_addrs skip).

# NEW emit_deferred_globals(m, sn): emit GINIT_STRUCT (+ scalar
# GINIT_FUNC_ADDR if ever used) globals with full initializers. Mirrors
# mod.rs:857-864.
String emit_deferred_globals(LirModule &m, Vector[String] &sn):
    String out = ""
    int i = 0
    while i < m.globals.len():
        LirGlobal g = m.globals.get(i).unwrap()
        if g.init_kind == GINIT_STRUCT:
            String kw = ""
            if g.is_const:
                kw = "const "
            String ty = c_type_name(g.ty, &sn)   # → __gg_<Trait>_VTable
            out = out + kw + ty + " __lir_g" + int_to_str(i) + " = {"
            int fi = 0
            while fi < g.struct_fields.len():
                if fi > 0:
                    out = out + ", "
                LirGlobalField fld = g.struct_fields.get(fi).unwrap()
                if fld.kind == GINIT_FUNC_ADDR and fld.func_id >= 0:
                    # DIRECT (void*)&fn — NOT the __adapt_ closure wrapper
                    # that IFuncAddr *instructions* use (lir_codegen.gg:2857-2860).
                    # Rust emits (void*)&fname (emit_types.rs:656-658).
                    String fname = c_func_name(m.functions.get(fld.func_id).unwrap().name)
                    out = out + "(void*)&" + fname
                else:
                    out = out + "NULL"
                fi += 1
            out = out + "}; // " + g.name + "\n"
        i += 1
    return out
```

**Call-site reorder (`lir_codegen.gg:5199, 5207`):** keep
`emit_globals` at `:5199` (plain globals, before forward decls), and add
`out = out + emit_deferred_globals(&m, &sn)` **after**
`emit_func_forward_decls` (`:5207`) and after the hashable-key bridges
(`:5213`), i.e. right before adapter functions (`:5216`). This guarantees
the `(void*)&fn` casts reference already-forward-declared functions.

**(ii) Critical: the vtable global field uses `(void*)&fn` DIRECTLY, not
the `__adapt_` wrapper.** The self-host `IFuncAddr` *instruction* renders
through a closure-adapter protocol (`lir_codegen.gg:2857-2860`:
`(void*)__adapt_<fn>`). The vtable global must NOT use that — Rust emits
the bare `(void*)&Trait_for_Type__method` (`emit_types.rs:656-658`). The
spec above does this correctly (it does not route through
`collect_func_addr_targets`/`emit_adapter_functions`). The vtable FuncAddr
fields therefore do **not** add adapter functions and do not perturb the
adapter path.

### 3.6 The two DCE edges (now load-bearing because globals exist)

**Edge (i) — seed DCE roots from globals' FuncAddr fields.** In
`compute_reachable_fns` (`lir_codegen.gg:918`), in the root-seeding region
(after the drop-table roots, ~`:1026`, before the transitive closure
`:1028`), add a scan of `m.globals` (analog of
`src/lir/optimize.rs:268-275` + `collect_global_func_refs:364-374`):

```
    # Vtable-global roots: a struct-of-FuncAddr global keeps its slot
    # functions live even when no IFuncAddr/ICall references them (Rust:
    # find_live_functions optimize.rs:268-275). Mirror Rust unconditionally
    # keeping every non-generic trait impl's methods reachable.
    int gvi = 0
    while gvi < m.globals.len():
        LirGlobal gv = m.globals.get(gvi).unwrap()
        if gv.init_kind == GINIT_STRUCT:
            int gfi = 0
            while gfi < gv.struct_fields.len():
                LirGlobalField gf = gv.struct_fields.get(gfi).unwrap()
                if gf.kind == GINIT_FUNC_ADDR and gf.func_id >= 0 and gf.func_id < n:
                    if not reachable.get(gf.func_id).unwrap():
                        reachable.set(gf.func_id, true)
                        worklist.push(gf.func_id)
                gfi += 1
        gvi += 1
```

**Edge (ii) — transitive `IFuncAddr` arm.** In the transitive-closure
`match` (`lir_codegen.gg:1052-1061`), add an arm mirroring the existing
`IClosurePack` arm (analog of `collect_inst_func_refs:358`,
`Inst::FuncAddr { func } => cb(*func)`):

```
                    case IFuncAddr(_, fa_fid):
                        target_fid = fa_fid
```

(`target_fid` is then pushed by the existing `:1062-1065` block.)

**The transitive tail (`Json__keys`, `errno_to_io_error`) carries
automatically.** Once the Serializer/Deserializer/Writer/Reader method
bodies are seeded live by edge (i), the existing `ICallExtern`-by-name
transitive edge (`lir_codegen.gg:1071-1101`, with its `_for_` trait-
dispatch suffix matching) walks the calls *inside* those bodies — e.g.
`Deserializer_for_JsonDeserializer__keys` calls `Json__keys`, and the io
methods call `errno_to_io_error`. No extra edge needed; verify by counting
67 after the change (§5).

### 3.7 `eliminate_dead_globals` carve-out (`lir_lower.gg:3650`)

The self-host's `eliminate_dead_globals` (`lir_lower.gg:3650-3677`) prunes
globals not referenced by any `IGlobalAddr` instruction. Vtable globals are
referenced by **no** `IGlobalAddr` (the fixture never instantiates the
type), so this pass would delete them — exactly the case Rust's
`eliminate_dead_globals` guards against by keeping any global whose init
contains FuncAddrs (`src/lir/optimize.rs:436-443`,
`global_has_func_addrs`). Add the same carve-out: in Pass 1
(`lir_lower.gg:3653-3662`), mark any `GINIT_STRUCT` global (or any global
with a `GINIT_FUNC_ADDR` field) as referenced:

```
    # Keep vtable globals (FuncAddr-bearing) — referenced indirectly via
    # (void*) casts in the C global, not via IGlobalAddr. Mirror Rust
    # optimize.rs:436-443 global_has_func_addrs.
    int kg = 0
    while kg < m.globals.len():
        if m.globals.get(kg).unwrap().init_kind == GINIT_STRUCT:
            referenced.put(kg, true)
        kg += 1
```

The stale comment at `lir_lower.gg:3647-3649` ("Self-host only emits
GINIT_RUNTIME_CALL globals — no FUNC_ADDR vtable globals … so the rule is
simple: referenced ↔ kept") **must be updated** — it is now false (a
false-historical-record smell per CLAUDE.md "Self-host as the elegance
showcase"). Rewrite it to describe the FuncAddr carve-out.

---

## 4. Files to change, per directory

`self_host_lowerer/` is the **sole** c_emit driver. Symlink map (verified
`ls -l tests/fixtures/`):

- `lower.gg`, `gir.gg`, `lir.gg`, `lir_codegen.gg`, `lir_lower.gg`,
  `driver.gg`, `format_gir.gg`, `format_lir.gg` — **own files of
  self_host_lowerer** (NOT symlinks).
- `traits.gg`, `derive.gg`, `ast.gg`, `parser.gg`, `resolve.gg`,
  `typecheck.gg`, etc. — **symlinks to `self_host_typechecker/`**.

**This change touches ONLY the lowerer's own files** — no symlinked file
is modified, so `self_host_typechecker/` and `self_host_check/` are
unaffected and their comparison tests (`type_comparison`, `check`,
`resolver_comparison`, `parser_comparison`) cannot regress from this work.

Files to edit (all in `tests/fixtures/self_host_lowerer/`):

1. **`gir.gg`** — add `GGINIT_*` consts, `GirGlobalField`, `GirGlobal`
   structs; add `globals: Vector[GirGlobal]` field to `GirModule` (AT END,
   §3.1); add `gir_mod_add_global` helper; register `_VTable` type-defs
   (or do that in lower.gg). Update every `GirModule(...)` constructor.
2. **`lower.gg`** — `_VTable` type-def pre-pass (§3.3); vtable-global
   emission in the `did_split` block (`:9694-9746`); a `method_has_self`
   helper if not present.
3. **`lir.gg`** — `GINIT_STRUCT` const; `LirGlobalField` struct;
   `struct_fields` field on `LirGlobal` (AT END, §3.2); update
   `LirGlobal(...)` constructors.
4. **`lir_lower.gg`** — GIR→LIR global translation loop (§3.4);
   `eliminate_dead_globals` FuncAddr carve-out + comment fix (§3.7);
   update the `LirGlobal(...)` site at `:3609`.
5. **`lir_codegen.gg`** — `emit_globals` skip-deferred + new
   `emit_deferred_globals` (§3.5); call-site reorder at `:5199/5207`;
   `IFuncAddr` transitive arm + global-FuncAddr root seeding in
   `compute_reachable_fns` (§3.6); import `GINIT_STRUCT` (`:20`).

No `format_gir.gg` / `format_lir.gg` change is strictly required for the
c_emit metric, BUT see Risk §7b — if those printers exhaustively `match`
on global init kinds, the new variant needs an arm or the self-host won't
compile itself (bootstrap). **Grep both for global-printing.**

---

## 5. Validation plan

**Primary metric:** `c_emit_comparison`'s `user_fn_count`
(`tests/integration.rs:13565-13593`) for `json_parse.gg`: self-host
28 → must reach **67** (= Rust). Measure exactly as the harness does:

```
# Rust reference (already captured): 67
./target/debug/gg build --emit-c-lir tests/fixtures/json_parse.gg \
  | awk '/Function Definitions/{f=1;next} f && /^[A-Za-z_].*\) \{$/{n++} END{print n}'

# Self-host after change — build the driver, then:
<driver> tests/fixtures/json_parse.gg lib --lir-c \
  | awk '/Function Definitions/{f=1;next} f && /^[A-Za-z_].*\) \{$/{n++} END{print n}'
```

Also confirm the 7 `__lir_g0..6` vtable globals + 6 `__gg_*_VTable`
typedefs appear in the self-host C and the C **compiles** (the harness
runs `gg build`; a malformed global would link-fail). Spot-check that the
self-host's `c_emit` for a fixture that *does* instantiate the trait
(e.g. any io.gg-heavy fixture) still matches.

**Test gates (run by the orchestrator, not sub-agents):**

1. `c_emit_comparison` (the printed matched-count is the real signal — the
   test always passes; read `--nocapture`). Target: json_parse moves from
   the mismatch list to matched; aggregate matched count rises by the
   number of fixtures sharing this exact gap (~62-150 per MEMORY).
2. **`self_host_bootstrap_fixed_point` MUST stay GREEN** — see Risk §7b.
   This is the highest-risk gate: changing the self-host's own IR model
   means the self-host must compile the new model *with itself* to a fixed
   point (stage-2 == stage-3 == stage-4 byte-identical). Run with
   `GG_STAGE1_TIMEOUT_SECS=900` (DEBUG self-host builds are slow on loaded
   boxes; the MEMORY/CLAUDE note bumps to 600-900).
3. `lowerer_comparison` — no regression in matched count (this is GIR
   fn-shape; the new globals don't change GIR functions, only add a
   `globals` vector, so it should be untouched — confirm).
4. `cargo test --lib` — green (no Rust-side change at all in this work,
   so this is a sanity gate that nothing in the worktree broke).
5. Targeted: `cargo test --test integration c_emit_comparison`,
   `... self_host_bootstrap_fixed_point`, `... lowerer_comparison`.

Pipe through `tee /tmp/cemit-$RANDOM.log` per CLAUDE.md.

---

## 6. Phasing — discrete, independently-validatable commits

Bottom-up (model → emit → C-render → DCE), each bisectable. Phases 1-3
add latent capability with **no behavioral change** (no global is emitted
until Phase 4 flips it on); Phase 4 lights up emission; Phase 5 makes the
bodies survive DCE. Crucial: **Phase 4 emits globals but Phase 5's DCE
edges are what raise the count** — so the user_fn_count jump happens at
Phase 5. Keep Phase 4+5 close (or squash) so no intermediate commit emits
globals whose slot fns are still DCE'd (harmless — dead globals reference
live-or-zeroed fids — but confusing).

- **Phase 1 — GIR + LIR model extension (`gir.gg`, `lir.gg`).** Add
  `GGINIT_*`/`GINIT_STRUCT` consts, `GirGlobalField`/`GirGlobal`/
  `LirGlobalField`, the `globals` field on `GirModule`, `struct_fields` on
  `LirGlobal`; update all constructors; add printer arms if needed.
  **Gate:** `cargo test --test integration self_host_bootstrap_fixed_point`
  stays GREEN (model added, nothing emitted → output byte-identical) +
  `lowerer_comparison` unchanged. This phase alone proves the bootstrap
  survives the model change — the single biggest risk, isolated first.

- **Phase 2 — GIR→LIR translation + DCE carve-out (`lir_lower.gg`).** Add
  the `gmod.globals` → `m.globals` loop and the `eliminate_dead_globals`
  FuncAddr carve-out + comment fix. **Gate:** bootstrap + lowerer
  unchanged (still no globals produced, loop is a no-op over empty vector).

- **Phase 3 — C emit (`lir_codegen.gg`).** Add `emit_deferred_globals`,
  the `emit_globals` skip, the call-site reorder. **Gate:** bootstrap +
  c_emit_comparison unchanged (empty globals → no deferred output).

- **Phase 4 — vtable-global EMISSION (`lower.gg`, `gir.gg` type-defs).**
  `_VTable` type-def pre-pass + the `did_split` global emission. **Gate:**
  self-host json_parse C now *contains* `__lir_g0..6` + `__gg_*_VTable`
  typedefs and **compiles**; user_fn_count may still be 28 (bodies not yet
  DCE-survived) — that's expected until Phase 5. Confirm globals present
  via `grep _VTable`.

- **Phase 5 — DCE seeding edges (`lir_codegen.gg`).** Global-FuncAddr root
  seeding + `IFuncAddr` transitive arm in `compute_reachable_fns`.
  **Gate (the payoff):** json_parse user_fn_count 28 → 67;
  `c_emit_comparison` matched count rises by the shared-fixture cohort;
  bootstrap GREEN; lowerer/check/type/parser/resolver comparisons
  unchanged; `cargo test --lib` green.

A regression at any gate localizes to that phase's file set.

---

## 7. Risks / landmines (called out explicitly)

**(a) "+39 keeps arguably-dead methods (like Rust)" — IS this desired
parity?** YES. The North Star is c_emit *parity with Rust* (MEMORY:
"compiling ALL fixtures the same way Rust does"). Rust emits a vtable
global for **every** non-generic trait impl unconditionally
(`emit_vtable_globals` has no use-site gate —
`traits.rs:990-1037`), keeping those methods live even when the fixture
never instantiates the type (json_parse never builds a JsonSerializer, yet
Rust emits g6 + all 12 Serializer bodies). Matching that is **correct
parity, not a bug.** A future whole-program DCE that prunes truly-unused
impls would have to land in BOTH compilers together to preserve parity;
out of scope here.

**(b) Bootstrap sensitivity — the self-host must compile its OWN new IR
model. HIGHEST RISK.** Two sub-hazards:
  - *Ordinal shifts.* MEMORY warns: inserting a variant mid-enum shifts
    ordinals → regressions; ADD AT END. The design adds `GGINIT_STRUCT=2`/
    `GINIT_STRUCT=3` and new struct fields strictly at the end (§3.1, §3.2).
    But `GirModule`/`LirGlobal`/`GirGlobal` are constructed *positionally*
    in Gorget — adding a trailing field breaks every existing
    `GirModule(...)` / `LirGlobal(...)` call unless updated. **Grep and
    update all constructor sites** (`grep -rn 'GirModule(\|LirGlobal('
    tests/fixtures/self_host_lowerer/`). Phase 1's gate (bootstrap green
    with model-only change) is designed to catch any missed site or
    printer-match gap immediately.
  - *Exhaustive `match` on init-kind.* VERIFIED: `format_lir.gg:389-400
    format_lir_global` matches `g.init_kind` with an `else` arm
    (`:399-400`), so adding `GINIT_STRUCT` does **not** break compilation —
    it falls through to the bare `result`. For `--emit-lir` fidelity, add a
    `GINIT_STRUCT` arm (cosmetic, NOT bootstrap-blocking). No global-printer
    found in `format_gir.gg` (GIR globals are new). Still grep any other
    LIR consumer that matches init-kind without an `else`. The bootstrap
    compiles the self-host *with the self-host*, so any genuinely
    non-exhaustive match would surface as a stage-1 compile failure.

  - *Constructor sites — VERIFIED exactly one each.* `GirModule(...)` is a
    single positional 23-arg call at `lower.gg:9185`; `LirGlobal(...)` a
    single 6-arg call at `lir_lower.gg:3609`. Adding a trailing field to
    each struct requires updating only that one call site (plus the struct
    def). `GirGlobal`/`GirGlobalField`/`LirGlobalField` are new, so no
    existing call sites. This tightly bounds the ordinal/constructor risk.

**(c) C struct-typedef / forward-decl ordering.** The `__gg_<Trait>_VTable`
struct must be defined (typedef) before the global that uses it, and the
global's `(void*)&fn` casts must follow the function forward-declarations.
The design: (1) `_VTable` structs go through the normal `lower_type_defs`
struct path and are emitted in `emit_structs` (`:446`) with topo-sort, so
the typedef precedes the global section; (2) the global itself is
**deferred** to after `emit_func_forward_decls` (§3.5), mirroring Rust's
`mod.rs:857-864`. Landmine: if a `_VTable` struct is unreferenced by any
`IGlobalAddr` but *is* referenced by the global's `ty`, ensure
`should_skip_struct` (`lir_codegen.gg:455`) does not drop it — the global's
type field keeps it live; verify the topo-sort includes it.

**(d) Does the self-host trait metadata carry the full ordered slot list?**
YES — `TraitDef.methods` (`ast.gg:198-202`) is `Vector[FunctionDef]` in
declaration order, available as `trait_defs.get(tname)` in `lower_module`
(`lower.gg:9526-9531, 9696`). This is the same source Rust reads
(`trait_def.items` → `register_trait_types:86`). The one nuance (§3.3
subtlety): a slot whose impl is a **default** method must still resolve to
a `Trait_for_Type__slot` body — the default-emit loop at `lower.gg:9727-
9734` produces exactly those names, so the FnRef resolves. **Verify** by
checking that for json_parse every slot in g0..g6 has a matching emitted
fn (the Rust output confirms the names; the self-host uses identical
mangling at `:7588-7589`). If a default body is somehow not emitted, the
§3.4 fallback pushes a `NULL`/zeroed field — well-formed, parity-safe, and
the body simply stays DCE'd (count < 67, caught at the Phase 5 gate).

**(e) `_VTable` struct field-type byte-parity (deferred, not blocking).**
Rust spells precise fn-ptr field types in the typedef
(`__gg_Result__int64_t__IoError (*write)(const void*)`,
`/tmp/json_rust.c:3608-3645`). The v1 design uses `void*` fields for
simplicity. This does **not** affect `user_fn_count` (the metric ignores
the typedef block), so json_parse reaches 67 either way. But it leaves a
residual c_emit byte-divergence in the typedef section. **Log to TODO.md**
as a follow-up (precise fn-ptr field spellings for full byte-parity), per
CLAUDE.md "Don't redesign around compiler gaps" — the gap is recorded, not
hidden, and the metric-level parity is achieved now.

---

## 8. Top-3 risks (summary)

1. **Bootstrap regression from the IR-model change** (§7b) — mitigated by
   ADD-AT-END discipline, a full constructor-site grep, printer-arm audit,
   and Phase 1's model-only bootstrap gate that isolates this risk first.
2. **C ordering** — vtable struct typedef must precede the global, and the
   `(void*)&fn` global must follow forward-decls (§7c) — mitigated by the
   deferred-global emission mirroring Rust `mod.rs:857-864`.
3. **Default-method slot resolution** (§7d) — a slot's FnRef must resolve
   to an emitted `Trait_for_Type__slot` body; mitigated by the existing
   default-emit loop (`lower.gg:9727`) + a defensive zeroed-field fallback.

---

## 9. Review-pass-1 FOLDS (these SUPERSEDE the cited spots above)

Fresh-review pass 1 SIGNED OFF (empirically reproduced the diagnosis: Rust=67/7-vtables,
self-host=28/0-vtables, and confirmed all 7 vtable-target fns ARE present in the
self-host LIR — pruned only at C-emit DCE). Four non-blocking corrections to fold,
none change the architecture or any phase gate:

**Fold 1 — §3.3 `_VTable` struct registration (supersedes the "GtPtr(UNIT)" framing).**
Registering the `_VTable` struct so the global's `type_id` resolves AND it lands in
`m.structs` requires BOTH: (a) a `type_table` GtNamed entry via
`lookup_or_register_named(&gmod, vt_struct_name)`, AND (b) a `GirTypeInfo` in
`gmod.type_infos` (so `lower_type_defs` Pass-1b at `lir_lower.gg:835-848` fills its
fields in Pass 2). `GirFieldInfo.type_name` is a **String** (`gir.gg:248-250`), and
`resolve_field_lir_type` (`lir_lower.gg:648-678`) falls back to `LT_PTR` for any
unrecognized type_name — so model each vtable slot field with an OPAQUE type_name
string (→ `void*` in C), NOT a `GtPtr(UNIT)`. (Field types are irrelevant to
`user_fn_count`; byte-parity of the precise fn-ptr spellings stays deferred per §7e.)

**Fold 2 — mirror Rust's method-less-trait skip.** Rust skips `_VTable` registration
for traits with zero instance methods (`if methods.is_empty() { continue }`,
`traits.rs:174`). Add the same guard: only register a `_VTable` typedef + emit a vtable
global when `tdef.methods` has ≥1 instance method (first param `self`). Avoids spurious
`struct __gg_X_VTable { char __pad; }` typedefs (valid C — emit_structs pads 0-field
structs — but a needless byte-divergence from Rust).

**Fold 3 — §3.4 func_index placement (drop the "move up" hedge).** `func_index` is
ALREADY built at `lir_lower.gg:3612-3618`, after the statics loop and before the
function-lowering loop (`:3628`). Just place the new `gmod.globals → m.globals`
translation loop AFTER `:3618` — no reordering, no second pass needed.

**Fold 4 — `method_has_self` helper is NOT present as a named fn.** Extract a small
helper (or inline) the existing `has_self` pattern at `lower.gg:7614-7619` (first param
name == "self"), mirroring Rust `traits.rs:89-91`, for the instance-only slot filter.

**Stale-figure note for the execution PR:** the MEMORY "7 self-host c_emit crashes"
figure is STALE — current baseline is `c_emit_comparison` 717/1028 (69.7%), 310
mismatched, **0 self-host crashes**. Re-confirm counts from `--nocapture` when quoting.

**Risk §7c clarification (pass-1):** `should_skip_struct` (`lir_codegen.gg:272-291`)
only skips runtime/aliased/imported structs, never user structs by reference count —
so the `_VTable` typedef survives regardless of `IGlobalAddr` references. The §7c
"verify it's referenced" hedge is unnecessary (harmlessly so).

---

## 10. Review-pass-2 NIT folds (pass 2 SIGNED OFF; these are doc-honesty/checklist polish)

**Fold 5 — STRIKE the stale §3.3 "GtPtr(UNIT)" field framing.** §3.3 (lines ~340-351)
says to model each `_VTable` field as `GtPtr(UNIT)`. That is NON-IMPLEMENTABLE: self-host
struct fields flow through `GirTypeInfo.fields` → `GirFieldInfo.type_name` (a **String**,
`gir.gg:248-250`) → `resolve_field_lir_type` — there is NO `GtPtr`-on-a-struct-field path.
Fold 1 (§9) is the ONLY correct mechanism: register a `GirTypeInfo` whose field
`type_name`s are opaque strings (→ `void*` via the `LT_PTR` fallback). When executing,
IGNORE §3.3's GtPtr wording; follow Fold 1.

**Fold 6 — opaque field `type_name` must be NON-resource, NON-keyword.** The opaque
`type_name` string (Fold 1) must NOT be `"void"` (maps to `LT_VOID`, not `LT_PTR`) and
must NOT collide with a registered resource type name (else `populate_drop_metadata`,
`lir_lower.gg:3454`, which iterates ALL `type_infos`, could emit a spurious slot drop).
`drop_fn_for_type` returns `""` for arbitrary opaque names (`lir_lower.gg:3451`), so any
opaque non-resource, non-keyword string (e.g. a `__vtslot`-style placeholder) is safe.

**Fold 7 — Fold 4 prose correction.** The instance-method ("has self") filter matches on
the first param's TYPE being `TSelf()` (the pattern at `lower.gg:7614-7619`), NOT on
`param_name == "self"`. Extract/inline THAT pattern (mirrors Rust `traits.rs:88-93`).

**Fold 8 — imports checklist.** §4's file list must also IMPORT the new `LirGlobalField`
type into `lir_lower.gg` (its import block ~`:15-16`) and `lir_codegen.gg` (~`:10`),
alongside `GINIT_STRUCT`. (The new `GirGlobal`/`GirGlobalField` likewise imported where
consumed.)

Pass 2 verdict: SIGN OFF — folds 1-4 verified correct against source; constructor sites,
did_split hook, IFuncAddr pattern, deferred-global indexing, eliminate_dead_globals
ordering, format_lir `else`-arm all re-confirmed. Architecture + phase gates unchanged.

---

## 11. Review-pass-3 NIT (pass 3 SIGNED OFF clean — this is non-blocking, for the executor)

**NIT (dormant, log as a one-line TODO, NOT a design change):** §3.3's gate reuses the
existing `did_split` predicate `not is_builtin_trait(tname) and trait_defs.contains(tname)`.
Rust's `emit_vtable_globals` gate is purely `trait_info.get(tname).is_some()` (any
non-generic local `Item::Trait` with ≥1 instance method) — it has NO `is_builtin_trait`
name-exclusion. So the prose claim that the gate mirrors Rust "exactly" is slightly
overstated. BUT an exhaustive search found NO live divergence: no locally-declared
`trait <BuiltinName>` is non-generically equipped with instance methods (Iterator/Iterable
are only generically equipped; Serializable/Hashable-as-local-trait are never equipped;
`equip Key with Hashable` uses the BUILT-IN Hashable, excluded by `trait_defs.contains` in
BOTH compilers). The divergence is DORMANT, and the `is_builtin_trait` exclusion is
actually internally REQUIRED for the self-host (a builtin-trait equip emits bodies under
`Type__method` naming, so a `Trait_for_Type__slot` FnRef wouldn't resolve). Does not
affect json_parse reaching 67, blocks no phase. Soften the "exactly" wording + TODO it.

