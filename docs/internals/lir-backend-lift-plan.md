# LIR → BIR Backend-Lift Plan — "Dumb Backend" Endgame

**Status:** Design, 2026-04-17. Not yet implemented.
**Context:** After several rounds of backend lifts, both C and LLVM backends still
contain thousands of lines of semantic decisions (HOF inlining, enum construction,
sentinel wrapping). This document identifies exactly which LIR primitives are
missing, what lift passes would close the gap, and introduces a new **BIR**
(Backend IR) stage between LIR and the backends to enforce the "dumb backend"
contract at the type level.

## TL;DR

The new pipeline is:

```
.gg → AST → GIR → LIR → BIR → machine code
```

- **LIR** stays as the mid-level SSA IR but gains new canonical high-level ops
  (`HofExpand`, `EnumInit`, `TraitCall`, `StructInit`, `SizeOf`, …).
- **BIR** is a newtype wrapper over `LirModule` that guarantees those high-level
  ops have been expanded — backends take `&BirModule` and see only primitives.
- A single lowering pass `lower_lir_to_bir` expands the canonical ops into block
  sequences that use existing primitives. A validator asserts no high-level ops
  remain.

This removes ~2,500 lines of duplicated logic from the two backends and makes
adding new backends (WASM, LLVM-JIT, freestanding) a fraction of today's effort.

## Current Gap Audit

Measuring "how dumb the backend is" by counting name-based dispatch sites
(`name == "X"`, `name.starts_with("Y")`, `name.contains("Z")`):

| Backend file | Lines | Name checks |
|---|---:|---:|
| `src/backend/llvm/mod.rs` | 6,451 | **228** |
| `src/backend/c_lir/emit_call_extern.rs` | 1,726 | **84** |
| `src/backend/c_lir/mod.rs` | 2,545 | **62** |

A truly dumb backend has **zero** name-based dispatch — it only looks at
instruction opcodes and declared LIR types.

The single largest chunk of smart-backend code is **HOF inlining** —
per-backend generation of filter/map/fold/reduce/each loops:

| Backend | Vector HOFs | Dict/Set HOFs | Total |
|---|---:|---:|---:|
| LLVM | 860 | 563 | 1,423 |
| C    | ~450 | ~240 | ~690 |
| **Duplicated logic** | | | **~2,100 lines** |

## Research Base — How Mature IRs Handle This

Studied SSA IRs, focusing on their closure/loop/enum handling:

- **Swift SIL (OSSA)** — Progressive lowering through "passes". Canonical SIL
  has high-level ops (`apply`, `init_enum_data_addr`, `init_existential_addr`,
  `class_method`, `destructure_struct`). Lowered SIL (after SILGen cleanup) has
  only memory primitives. Each pass lowers ops to simpler ones. *Key takeaway:*
  you don't need one-level IR — you need an IR with *named* stages and passes.

- **Cranelift CLIF** — Explicit calling convention carried on every call site
  (`call_indirect` takes a signature id). No aggregate SSA values — all structs
  go through memory (we match this). *Key takeaway:* call-site signatures are
  explicit data, not inferred from name.

- **MLIR Dialects** — `scf.for`/`scf.while` for loops; `memref` for sized memory;
  `func.call_indirect` with signature. Progressive dialect conversion. *Key
  takeaway:* high-level loop constructs are legitimate IR — they lower to
  block-args + branches during a dedicated pass.

- **QBE** — 4 types, minimalist. Calls carry explicit ABI annotations
  (`call :foo(w %a, :s %b, ...)`). No string-matching. *Key takeaway:* if
  info is needed at emit time, make it an instruction operand.

- **Roc MonoIR** — Explicit refcount operations (`Inc`, `Dec`, `DecRef`).
  Enum construction as dedicated ops. *Key takeaway:* ownership/refcount
  operations belong in the IR, not the backend.

- **Rustc (HIR → MIR → LLVM IR)** — Distinct datatypes per stage, each lowering
  is a type transform. Type system enforces "you can't pass HIR to codegen."
  *Key takeaway:* datatype separation catches "forgot to lower" bugs at
  compile time.

## The Three Gaps

### Gap 1 — LIR Lowering Still Defers to Backend

Several patterns reach the backend as a single string-named CallExtern
when they should already be expanded. `src/lir/lower/lifts.rs` shows the
correct pattern (nullable-void→Option, last_error→Result) — same approach
needs to extend.

**Under-expanded patterns:**

| Pattern | Example | Current | Desired |
|---|---|---|---|
| HOFs | `v.filter(p)` | CallExtern `Vector__int_t__filter` | explicit loop blocks + CallClosure |
| Collection ctors | `Vector[int]()` | CallExtern `Vector__int64_t__new` | CallExtern `gorget_array_new` with SizeOf |
| Newtype ctor | `Counter(5)` | CallExtern `Counter` | StructInit inst |
| Enum ctor | `Ok(42)` | scattered alloca+FieldPtr+Store | EnumInit inst |
| Trait method | `obj.describe()` | CallExtern `Describer_TraitObj__describe` | TraitCall inst |

### Gap 2 — LirExtern Metadata Is Under-Used

`LirExtern` already carries `return_type`, `param_abis`, `return_abi`,
`is_variadic`. But backends re-infer from function name instead of trusting
the declared types. Examples:

- **sret ABI decision** (LLVM): 100+ lines of `name.contains("gorget_str_slice")`
  etc. to decide whether return is by-sret. Should just check
  `needs_sret(extern.return_type, structs)`.

- **Return-type-by-name** (LLVM around line 1330): infers `returns_array` /
  `returns_string` / `returns_map` from name matching. `LirExtern.return_type`
  has the answer.

- **CStr coercion** (both backends): checks arg name patterns. `param_abis[i]`
  has the authoritative answer, already plumbed (per previous AbiKind migration).

### Gap 3 — A Handful of LIR Instructions Are Missing

To eliminate the remaining backend smartness, we add these new canonical ops to
`Inst`. Each is expanded away by `lower_lir_to_bir` before backend emission.

#### 3a. `SizeOf { dst, type_id }`

Today's state: `IConst { value: 8 }` with sizeof resolved at GIR→LIR lowering
by consulting `opaque_runtime_size` and `c_sizeof_lir_type`. Scattered.

New state: first-class `SizeOf` instruction that the BIR lowering resolves
through the shared `opaque_runtime_size` table. Unifies collection
constructors and any sizeof-dependent codegen.

#### 3b. `StructInit { dst, struct_id, fields: Vec<(field_idx, ValueId)> }`

Today: alloca + N×(FieldPtr + Store) per struct literal. Both backends open-code
the expansion.

New: one canonical instruction. Expanded to the same alloca+stores during
`lower_lir_to_bir`. Callers don't care how.

#### 3c. `EnumInit { dst, struct_id, variant_tag, variant_idx, payload: Option<ValueId> }`

Today: alloca + store tag (4 bytes) + FieldPtr to payload + memcpy/store
payload. Every `Ok(x)`, `Some(v)`, `Error(msg)` open-codes this.

New: one canonical instruction. The Option/Result wrapping patterns (which
account for ~400 lines in `emit_sentinel_scalar_option_wrap`,
`emit_nullable_ptr_option_wrap`, etc.) reduce to one `EnumInit` after checking
the condition.

#### 3d. `EnumCheck { dst, value, variant_tag }` + `EnumExtract { dst, value, variant_idx, payload_field, ty }`

Today: FieldPtr to tag, Load i32, Cmp, Branch. Then FieldPtr to payload offset,
Load ty.

New: two canonical instructions. Pattern matching lowers to EnumCheck + branches
+ EnumExtract. BIR lowering expands to the current primitive sequence.

#### 3e. `NamedFieldPtr { dst, base, struct_name, field_name }`

Today: `FieldPtr` uses field **index**. For opaque runtime structs (declared
as `struct X: pass` in Gorget, with no LIR fields), there's no way to access
fields at all — backends hardcode `getelementptr i8, ptr %x, i64 16`.

New: symbolic access resolved through a shared offset table (extension of
`opaque_runtime_size`). Usage: `NamedFieldPtr { base: str_val, struct_name: "Str",
field_name: "data" }` resolves to the `.data` field offset in `GorgetString`.

Eliminates ~50 hardcoded offsets across both backends (Str's data/len/cap/alloc,
Match's start/end/text, GorgetArray's data/len/cap, etc.).

#### 3f. `CallClosure.param_kinds`

Today: `CallClosure { dst, kind, closure, args, ret_ty, arg_abis }`. But
`arg_abis` is not always populated — and the LLVM backend still has to
heuristically decide "is this arg passed by value or by reference?" (the
SlotAddr-based heuristic I added in commit `3a858bcb`).

New: require `param_kinds: Vec<ClosureArgKind>` on every CallClosure, where:
- `ByValue` — load the struct, pass by value (closure takes small aggregates
  by value)
- `ByRef` — pass pointer unchanged (`&Counter`-style args)
- `Move` — pass pointer, source is consumed (zeroing elsewhere)

GIR→LIR lowering sets this from the closure's declared signature. No more
heuristics.

#### 3g. `HofExpand { collection, op, element_ty, closure, result_slot }`

The big one. A single canonical op expressing "run `op` over the collection
using `closure`." Expansion lives in `lower_lir_to_bir` and generates
block-args + branches + CallClosure — the same pattern both backends currently
duplicate.

After expansion, BIR sees only primitives. **Before** expansion, LIR-level
optimizations can reason about HofExpand as a unit (e.g., fuse adjacent
`HofExpand(filter)` + `HofExpand(map)` into one pass).

### Tradeoff Table — Should HOFs Be in the LIR?

| Approach | Pros | Cons |
|---|---|---|
| Expand at GIR→LIR (QBE-style) | LIR stays minimal | 2,000 lines of lowering code up front |
| `HofExpand` + BIR lowering pass (SIL-style) | LIR has one high-level op that explains intent, fusable | LIR isn't strictly low-level |
| Keep backend-inline (current) | No new code | Can't share logic, no optimization visibility |

**Decision: SIL-style with a BIR lowering pass.** Fusion optimizations become
natural at the HofExpand level. Reading LIR remains intuitive — a HOF call is
one instruction, not 50 lines of branch/phi/closure-dispatch.

## The Pipeline

```
GIR (high-level, ownership-aware, trait-resolved, monomorphized)
  │
  ▼ lir::lower (unchanged path + emits new canonical ops)
LIR (SSA + memory + can contain HofExpand/EnumInit/TraitCall/StructInit/SizeOf/…)
  │
  ▼ optimization passes (can fuse/simplify at LIR level)
  │
  ▼ bir::lower::lower_lir_to_bir  (expands high-level ops into primitives)
  ▼ bir::validate::assert_primitives_only  (enforces invariant)
BIR (pure primitives — same Inst type, but validator guarantees no high-level ops)
  │
  ▼ backend::llvm or backend::c_lir (pure 1:1 translator — zero name-based dispatch)
machine code
```

The LIR and BIR **use the same underlying types** (`LirModule`, `LirFunction`,
`Inst`, `Term`). BIR is a newtype wrapper that guarantees a lowering pass has
run and the validator passed.

## Concrete Design

### File layout

```
src/ir/       → GIR (exists)
src/lir/      → LIR (exists; `Inst` gains new canonical variants)
src/bir/      → NEW
    mod.rs          — BirModule newtype wrapper, from_lir() entry point
    lower.rs        — lower_lir_to_bir (expansion pass)
    validate.rs     — assert_primitives_only (validator)
src/backend/llvm/   → takes &BirModule (not &LirModule)
src/backend/c_lir/  → takes &BirModule
```

### Type definitions

```rust
// src/lir/mod.rs — existing `Inst` enum, augmented
pub enum Inst {
    // existing primitives — no change
    SlotStore { .. }, SlotLoad { .. }, Add { .. }, Load { .. },
    Store { .. }, Call { .. }, CallExtern { .. }, Branch { .. },
    Jump { .. }, /* ... etc. */

    // NEW canonical ops — expanded away by bir::lower, rejected by BIR validator
    HofExpand { coll: ValueId, op: HofOp, element_ty: LirType,
                closure: ValueId, closure_sig: ClosureSig,
                result_slot: Option<SlotId> },
    StructInit { dst: ValueId, struct_id: StructId,
                 fields: Vec<(u32, ValueId)> },
    EnumInit { dst: ValueId, struct_id: StructId, variant_tag: u32,
               variant_idx: u32, payload: Option<ValueId> },
    EnumCheck { dst: ValueId, value: ValueId, variant_tag: u32 },
    EnumExtract { dst: ValueId, value: ValueId, variant_idx: u32,
                  payload_field: u32, ty: LirType },
    NamedFieldPtr { dst: ValueId, base: ValueId,
                    struct_name: String, field_name: String },
    SizeOf { dst: ValueId, type_id: TypeKey },
    TraitCall { dst: Option<ValueId>, object: ValueId,
                trait_name: String, method: String,
                args: Vec<ValueId>, arg_abis: Vec<AbiKind> },
    CowClone { dst: ValueId, src: ValueId, ty: LirType },
}

// src/bir/mod.rs — new
/// BIR is a LIR module whose canonical ops have been expanded to primitives.
/// Newtype seals the invariant: backends can only receive this type.
pub struct BirModule(LirModule);

impl BirModule {
    /// Lowers a LIR module to BIR by expanding canonical ops and validating.
    pub fn from_lir(m: LirModule) -> Result<Self, BirError> {
        let lowered = crate::bir::lower::lower_lir_to_bir(m)?;
        crate::bir::validate::assert_primitives_only(&lowered)?;
        Ok(BirModule(lowered))
    }

    /// Read-only access to the underlying LirModule. Backends use this
    /// internally for their 1:1 translation.
    pub fn as_lir(&self) -> &LirModule { &self.0 }
}

// Backends take &BirModule — the type system guarantees they never
// receive an unlowered LirModule.
pub fn generate_llvm_ir(m: &BirModule) -> String { ... }
pub fn generate_c_ir(m: &BirModule) -> String { ... }
```

### The validator

`src/bir/validate.rs` is the single source of truth for "what's in BIR":

```rust
pub fn assert_primitives_only(m: &LirModule) -> Result<(), BirError> {
    for func in &m.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                match inst {
                    // Primitives — allowed in BIR
                    Inst::SlotStore { .. } | Inst::SlotLoad { .. } |
                    Inst::Add { .. } | Inst::Load { .. } |
                    Inst::Store { .. } | Inst::Call { .. } |
                    Inst::CallExtern { .. } | Inst::Branch { .. } |
                    Inst::Jump { .. } | /* … all existing primitives … */ => {}

                    // Canonical ops — must have been lowered
                    Inst::HofExpand { .. } |
                    Inst::EnumInit { .. } | Inst::EnumCheck { .. } |
                    Inst::EnumExtract { .. } | Inst::StructInit { .. } |
                    Inst::NamedFieldPtr { .. } | Inst::SizeOf { .. } |
                    Inst::TraitCall { .. } | Inst::CowClone { .. } => {
                        return Err(BirError::UnloweredCanonicalOp {
                            fn_name: func.name.clone(),
                            op: inst.opcode_name(),
                        });
                    }
                }
            }
        }
    }
    Ok(())
}
```

Adding a new canonical op requires exactly one match-arm change here. Adding
a new primitive requires no change (primitives are the "everything else"
default).

### Where optimization passes run

| Pass | Runs on | Why |
|---|---|---|
| SSA construction | LIR (early) | Primitives only — canonical ops don't affect SSA shape |
| HOF fusion | LIR | Needs to see `HofExpand` to fuse adjacent HOFs |
| Enum simplification | LIR | Needs `EnumInit`/`EnumCheck` visibility |
| SizeOf folding | LIR | Resolves to IConst when sizes are known |
| DCE / copy-prop / inlining | LIR or BIR | Either works; run on LIR for optimization visibility |
| mem2reg / scalar promotion | BIR | Primitives only, cleaner analysis |

The rule: **if a pass benefits from seeing canonical ops, run it on LIR; otherwise run it on BIR.**

## Concrete Lift List (by ROI)

### Priority 0 — Infrastructure setup (prerequisite)

Create `src/bir/` with the newtype wrapper, an initially trivial
`lower_lir_to_bir` (just unwraps), and a validator with today's empty
canonical-op set. Switch both backends to take `&BirModule`. **Zero behavior
change but the type boundary is enforced.**

### Priority 1 — Low effort, high backend simplification

1. **Trust `LirExtern.return_type` for sret** (~100 lines removed from LLVM).
   The `name.contains("gorget_str_slice")` chains disappear — backends use the
   declared type.

2. **Trust `param_abis` for arg coercion** (~60 lines each backend).
   The `is_cstr_returning_fn` / `needs_null_terminated_cstr` patterns become
   reads of `param_abis[i]`.

3. **Add `SizeOf` canonical op** — shared `opaque_runtime_size` lookup
   centralized to BIR expansion.

### Priority 2 — Medium effort, eliminates big sections

4. **Add `EnumInit` / `EnumCheck` / `EnumExtract`** — eliminates ~400 lines
   of sentinel/nullable/last-error wrapping in both backends. The existing
   `emit_sentinel_scalar_option_wrap` helpers move to BIR expansion.

5. **Add `StructInit`** — eliminates newtype constructor special-casing
   (~50 lines each backend).

6. **Add `NamedFieldPtr`** — eliminates hardcoded `i64 8` / `i64 16` offsets
   for opaque runtime struct fields (~50 sites across backends).

7. **Require `param_kinds` on `CallClosure`** — replaces the SlotAddr
   heuristic with declared metadata. Correct closure dispatch for all cases.

### Priority 3 — Large effort, largest cleanup

8. **`HofExpand` + BIR lowering pass** — ~2,100 lines of duplicated HOF
   inlining become one shared expansion pass + a `HofExpand` instruction.
   Also unlocks **fusion optimizations** (adjacent HofExpand ops can merge —
   same-pass filter+map, etc.).

### Priority 4 — Smaller but valuable

9. **`TraitCall`** (~80 lines removed from LLVM, fixes `print_trait_object`-style
   struct-by-value method cases).

10. **`CowClone { src, ty }`** — explicit CoW materialization instead of the
    C backend's inline injection in SlotStore. Both backends translate to the
    same `gorget_string_copy_cow` call.

### Priority 5 — ABI coercion + heap alloc, added during agent work

These two came from the self-host bootstrap agent's feedback: issues the
backends could not fix at their layer because the LIR produced ambiguous
shapes at GIR→LIR time. Both fit the plan's "make the implicit explicit"
pattern cleanly.

11. **`AddressOf { dst, value, ty }`** — take the address of an SSA value.
    Replaces the scattered "manually spill to temp slot and SlotAddr" pattern
    at call sites where an extern's `AbiKind::Ptr` param needs an address but
    the source operand is an SSA register (scalar) rather than already
    slot-backed. BIR expansion:

    ```
    if source already slot-backed:
        SlotAddr  dst = &source_slot
    else (scalar SSA value):
        s_tmp = add_slot(ty)
        SlotStore s_tmp = value
        SlotAddr  dst = &s_tmp
    ```

    Kills the self-host `runtime_arg_needs_addr` name-lookup table —
    GIR→LIR emits `AddressOf` whenever the declared `AbiKind` requires it,
    no name matching needed at the consumer.

12. **`BoxAlloc { dst, inner_ty, value }`** — allocate a `Box[T]` on the heap
    with an initial value. BIR expansion:

    ```
    %sz = SizeOf(inner_ty)
    %p  = CallExtern "__gorget_alloc"(sz)     ; returns void*
    Store / Memcpy *p = value                  ; scalar or aggregate per inner_ty
    dst = %p
    ```

    Kills the backend's known-T / unknown-T fork for `Box(x)` calls — the
    inner type is explicit on the instruction, one expansion covers every
    case. Small win per commit, clean alignment with SizeOf + canonical
    enum/struct init.

    *Not recommended: a more general `HeapAlloc` + separate init.* BoxAlloc
    keeps allocation + initialization atomic, matches the Gorget surface
    semantics of `Box(value)`, and leaves room for future write-combining /
    alignment-hint optimizations at the op level.

**Deferred intentionally:** `Deref` as a separate op. `Load { dst, ptr, ty }`
is already the canonical dereference — adding `Deref` would only rename the
primitive without unlocking any expansion or optimization.

## Fundamental Architectural Issue (Addressed)

The LIR design doc says:

> *"Explicit everything. If the C backend currently generates code for it,
> LIR must have an instruction for it."*

Today the LIR is **faithful to this principle for explicit operations** (load,
store, branch). But it **abandons the principle for implicit operations** —
CallExtern with a specific name carries *semantic meaning* that the backend
decodes.

`CallExtern { name: "Vector__int64_t__filter" }` is not an explicit op — it's
a *symbolic reference* to a pattern the backend must unfold. That's not
"explicit everything." That's "explicit for scalars, implicit for
collections/closures/sentinels."

**Fix:** add canonical ops to LIR for each such pattern (`HofExpand`,
`EnumInit`, `TraitCall`, etc.), expand them to primitives in
`lower_lir_to_bir`, and enforce at the BIR boundary that nothing symbolic
leaks to the backend. That's the contract the design doc promised.

## Answers to "Best Compiler Ever" Question

The ingredients that separate world-class compilers from merely good ones:

1. **Progressive lowering with type-enforced stage boundaries** — Rustc
   (HIR → MIR → LLVM IR), GHC (Core → STG → Cmm). Each stage is a distinct
   type; moving between stages is a type transform; backends can only receive
   the final type. *Adopted:* GIR → LIR → BIR, with `BirModule` as a sealed
   newtype.

2. **Explicit ABI at call sites** — Cranelift and QBE carry signatures on
   `call_indirect`. *Adopted:* `param_kinds` on `CallClosure`, richer
   `AbiKind` already threads through CallExtern.

3. **High-level ops that lower** — SIL's `apply`/`init_enum_data_addr`,
   MLIR's `scf.for`. Let optimizations work at the highest level they can.
   *Adopted:* `HofExpand`, `EnumInit`, `TraitCall`.

4. **No name-based dispatch in backends** — QBE, Cranelift. Names are just
   labels — meaning is in the instruction kind and operand types. *Adopted:*
   BIR's validator rejects symbolic CallExtern patterns that should've
   been expanded.

5. **Shared metadata tables** — LLVM's intrinsic table, SPIR-V's op table —
   one place defines each operation's semantics. *Extended:* `opaque_runtime_size`
   grows into `opaque_runtime_layout` with fields and offsets consumed by
   `SizeOf` and `NamedFieldPtr` expansion.

6. **Ownership in IR (optional)** — Swift OSSA, Roc MonoIR. Gorget handles
   this at GIR level; LIR stays ownership-unaware per original design doc.

## Not Recommended

- **Full MLIR-style dialects.** Gorget is a single language; the dialect
  infrastructure is overengineering (same conclusion as original LIR design doc).

- **Sea of Nodes.** V8 abandoned it. Harder to debug, slower to compile.

- **Stack-based bytecode.** We want to target LLVM/native, not a VM.

- **Same-datatype phases with runtime validators (SIL-style).** Works for
  well-funded teams with extensive invariant checking infrastructure. For
  Gorget's solo/AI-assisted maintenance model, type-level errors are
  dramatically cheaper than runtime validator errors.

## Naming Rationale — Why "BIR"?

Considered several candidates:

| Name | Meaning | Rejected because |
|---|---|---|
| **BIR** | Backend IR | ← chosen |
| CIR | Canonical IR or Core IR | Overloaded with GHC's Core and could be confused with canonical LIR phase |
| EIR | Emit IR | Phonetically awkward, less memorable |
| PIR | Primitive IR | Parrot already owns this name |
| MIR | Mid IR | Rustc owns this |
| FIR | Flat/Final IR | "Flat" vague; "Final" ambiguous |
| TIR | Target IR | Zig already uses |

**BIR wins on:** clarity (*this is what backends eat*), pronounceability
(`beer` or `bee-eye-arr`), no collision with established SSA-IR names in the
compilers ecosystem, and describes *role* not content — if we later add a
stage below BIR (machine-specific IR), BIR's meaning doesn't shift.

## Migration Strategy

Each step is a single commit, each removes code, each demonstrates payoff:

1. **Step 0** — Create `src/bir/` scaffolding. `BirModule(LirModule)` newtype,
   trivial `lower_lir_to_bir`, empty-allowlist validator. Both backends switch
   to `&BirModule`. Zero behavior change.

2. **Step 1** — Trust `LirExtern.return_type` for sret. Delete `name.contains`
   chains in LLVM extern-declaration emission.

3. **Step 2** — Trust `param_abis` for coercion in both backends.

4. **Step 3** — Add `SizeOf` variant + BIR expansion. Remove ad-hoc sizeof
   resolution at emit time.

5. **Step 4** — Add `EnumInit`/`EnumCheck`/`EnumExtract` + BIR expansion.
   Move `emit_sentinel_*`/`emit_nullable_*` helpers from `src/lir/lower/lifts.rs`
   to produce these ops directly.

6. **Step 5** — Add `StructInit`, `NamedFieldPtr`, `CowClone`.

7. **Step 6** — Add `CallClosure.param_kinds`, remove SlotAddr heuristic.

8. **Step 7** — Add `TraitCall` + BIR expansion. Fixes trait method ABI issues.

9. **Step 8** — The big one. `HofExpand` variant + BIR expansion pass
   generating loop blocks + CallClosure. Remove HOF inlining from both
   backends.

   **Migration status:**
   - Vector: `each` ✓ (pathfinder). `map`, `filter`, `flat_map`, `fold`,
     `reduce`, `any`, `all`, `find`, `find_index`, `count`, `sorted`,
     `sort_by`, `sorted_by`, `sort_by_key`, `sorted_by_key`, `windows`,
     `chunks`, `unique` — still inline in backends.
   - Dict: all variants still inline in backends.
   - Set: all variants still inline in backends.

10. **Step 9** — Add `AddressOf { value, ty }` + BIR expansion. GIR→LIR
    emits this whenever an `AbiKind::Ptr` extern param is fed an SSA-value
    operand. Eliminates the backend's "value already in a slot vs. needs
    spilling" fork plus the self-host `runtime_arg_needs_addr` table.

11. **Step 10** — Add `BoxAlloc { inner_ty, value }` + BIR expansion.
    GIR→LIR emits this for every `Box(x)` construction. Eliminates the
    backend's known-T vs. unknown-T fork on Box construction.

After Step 10, the backends are ~60% smaller and truly dumb. No name-based
dispatch, no inline loop generation, no sentinel-wrapping logic, no runtime
ABI name lookup, no box-T inference. Each backend is a pure 1:1 translator
from BIR to target syntax.

## Expected Outcomes

- **Backends shrink dramatically.** LLVM backend from ~6,500 lines to
  ~3,000. C backend from ~5,000 to ~2,500.
- **Adding a WASM backend becomes feasible** (~1,500 lines instead of today's
  ~6,500 estimate).
- **Self-host LIR backend** (currently ~6,200 lines, 656/924 fixtures) catches
  up faster because it has less semantics to reimplement — it only needs to
  understand BIR.
- **Freestanding/UEFI builds** get simpler: no runtime-specific smartness in
  the backend to work around.
- **HOF fusion** optimization becomes trivially implementable as an LIR pass
  (pre-BIR).
- **Type safety** at the layer boundary catches "forgot to lower" bugs at
  compile time rather than runtime.

The "best compiler ever" outcome isn't from one heroic feature — it's from
**consistent application of "explicit everything, backends are dumb"** until
no semantic decisions remain at the emit layer. `BirModule` is how we enforce
that consistency.
