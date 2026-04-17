# LIR Backend-Lift Plan — "Dumb Backend" Endgame

**Status:** Design analysis, 2026-04-17. Not yet implemented.
**Context:** After several rounds of backend lifts, both C and LLVM backends still
contain thousands of lines of semantic decisions (HOF inlining, enum construction,
sentinel wrapping). This document identifies exactly which LIR primitives are
missing and what lift passes would close the gap.

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
  only memory primitives. Each pass lowers ops to simpler ones. **Key takeaway:**
  you don't need one-level IR — you need an IR with *named* stages and passes.

- **Cranelift CLIF** — Explicit calling convention carried on every call site
  (`call_indirect` takes a signature id). No aggregate SSA values — all structs
  go through memory (we match this). **Key takeaway:** call-site signatures are
  explicit data, not inferred from name.

- **MLIR Dialects** — `scf.for`/`scf.while` for loops; `memref` for sized memory;
  `func.call_indirect` with signature. Progressive dialect conversion. **Key
  takeaway:** high-level loop constructs are legitimate IR — they lower to
  block-args + branches during a dedicated pass.

- **QBE** — 4 types, minimalist. Calls carry explicit ABI annotations
  (`call :foo(w %a, :s %b, ...)`). No string-matching. **Key takeaway:** if
  info is needed at emit time, make it an instruction operand.

- **Roc MonoIR** — Explicit refcount operations (`Inc`, `Dec`, `DecRef`).
  Enum construction as dedicated ops. **Key takeaway:** ownership/refcount
  operations belong in the IR, not the backend.

## The Three Gaps

### Gap 1 — GIR→LIR Lowering Still Defers to Backend

Several patterns reach the backend as a single string-named CallExtern
when they should already be expanded. The `src/lir/lower/lifts.rs` module
shows the correct pattern (nullable-void→Option, last_error→Result) — same
approach needs to extend.

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

To eliminate the remaining backend smartness, we need these primitives:

#### 3a. `SizeOf { dst, type_id }`

Today's state: `IConst { value: 8 }` with sizeof resolved at GIR→LIR lowering
by consulting `opaque_runtime_size` and `c_sizeof_lir_type`. Backends sometimes
re-resolve.

New state: first-class `SizeOf` instruction that backends resolve through the
**shared** `opaque_runtime_size` table. Unifies collection constructors and any
sizeof-dependent codegen.

#### 3b. `StructInit { dst, struct_id, fields: Vec<(field_idx, ValueId)> }`

Today: alloca + N×(FieldPtr + Store) per struct literal. Both backends open-code
the expansion.

New: one instruction. C backend emits a compound literal; LLVM emits
`insertvalue` chains or alloca + stores (its choice). Callers don't care.

#### 3c. `EnumInit { dst, struct_id, variant_tag, variant_idx, payload: Option<ValueId> }`

Today: alloca + store tag (4 bytes) + FieldPtr to payload + memcpy/store
payload. Every `Ok(x)`, `Some(v)`, `Error(msg)` open-codes this.

New: one instruction. The Option/Result wrapping patterns (which account for
~400 lines in `emit_sentinel_scalar_option_wrap`, `emit_nullable_ptr_option_wrap`,
etc.) reduce to one `EnumInit` after checking the condition.

#### 3d. `EnumCheck { dst, value, variant_tag }` + `EnumExtract { dst, value, variant_idx, payload_field, ty }`

Today: FieldPtr to tag, Load i32, Cmp, Branch. Then FieldPtr to payload offset,
Load ty.

New: two instructions. Pattern matching lowers to EnumCheck + branches +
EnumExtract. Backends know how their ABI lays out the tag+payload.

#### 3e. `NamedFieldPtr { dst, base, struct_name, field_name }`

Today: `FieldPtr` uses field **index**. For opaque runtime structs (declared
as `struct X: pass` in Gorget, with no LIR fields), there's no way to access
fields at all — backends hardcode `getelementptr i8, ptr %x, i64 16`.

New: symbolic access resolved through a shared offset table (mirror of
`opaque_runtime_size`). Usage: `NamedFieldPtr { base: str_val, struct_name: "Str",
field_name: "data" }` resolves to the `.data` field offset in `GorgetString`.

Eliminates ~50 hardcoded offsets across both backends (Str's data/len/cap/alloc,
Match's start/end/text, GorgetArray's data/len/cap, etc.).

#### 3f. `CallClosure.param_kinds`

Today: `CallClosure { dst, kind, closure, args, ret_ty, arg_abis }`. But
`arg_abis` is not always populated — and the LLVM backend still has to heuristically
decide "is this arg passed by value or by reference?" (I added a SlotAddr-based
heuristic in commit `3a858bcb`).

New: require `param_kinds: Vec<ClosureArgKind>` on every CallClosure, where:
- `ByValue` — load the struct, pass by value (used for closures that take
  small aggregates by value)
- `ByRef` — pass pointer unchanged (used for `&Counter`-style args)
- `Move` — pass pointer, source is consumed (zeroing happens elsewhere)

Lowering sets this from the closure's declared signature. No more heuristics.

#### 3g. `HofExpand { collection, op, element_ty, closure, result_slot }` — **OPTIONAL**

The cleanest way to lift HOF inlining is to have GIR→LIR emit explicit loops.
But that's ~2,000 lines of lowering code per HOF (filter/map/fold/reduce/each/any/all/find/flat_map).

Alternative: a high-level `HofExpand` instruction + a dedicated lowering pass
run between GIR→LIR and backend emission. The pass expands `HofExpand` into
block-args + branches + CallClosure. Both backends then see only primitives.

**This is the Swift SIL pattern** — high-level ops in canonical form, lowered
via dedicated passes before backend entry.

### Tradeoff Table — Should HOFs Be in the LIR?

| Approach | Pros | Cons |
|---|---|---|
| Expand at GIR→LIR (QBE-style) | LIR stays minimal | 2,000 lines of lowering code |
| `HofExpand` + lowering pass (SIL-style) | LIR has one high-level op that explains intent | LIR no longer strictly low-level |
| Keep backend-inline (current) | No new code | Can't share logic, no optimization visibility |

**Recommendation: SIL-style with a lowering pass.** Reason: optimizations like
fusion (`v.filter(p).map(f)` → single pass) become natural at the HofExpand
level. Also makes it obvious what the program is doing when you read the LIR.

## Proposed Pipeline

```
GIR (high-level, ownership-aware)
  │
  ▼ lower_to_canonical_lir
Canonical LIR (has: HofExpand, EnumInit, TraitCall, etc.)
  │
  ▼ lower_canonical_to_core
Core LIR (block-args + branches + memory only)
  │
  ▼ (optimization passes, same as today)
  ▼
Backend (pure 1:1 translator — zero name-based dispatch)
```

The Canonical LIR and Core LIR use the **same types** (LirModule, LirFunction, Inst,
Term) — the distinction is which instruction *variants* appear. A validator can
check "this function is in Core LIR" by asserting no HofExpand, EnumInit, etc.

Matches Swift SIL's approach (canonical SIL → lowered SIL — same datatypes,
progressive lowering).

## Concrete Lift List (by ROI)

### Priority 1 — Low effort, high backend simplification

1. **Trust `LirExtern.return_type` for sret** (~100 lines removed from LLVM).
   The `name.contains("gorget_str_slice")` chains disappear — backends use the
   declared type.

2. **Trust `param_abis` for arg coercion** (~60 lines each backend).
   The `is_cstr_returning_fn` / `needs_null_terminated_cstr` patterns become
   reads of `param_abis[i]`.

3. **Add `SizeOf` instruction** (~40 lines of shared `opaque_runtime_size`
   uses become one lookup).

### Priority 2 — Medium effort, eliminates big sections

4. **Add `EnumInit` / `EnumCheck` / `EnumExtract`** — eliminates ~400 lines of
   sentinel/nullable/last-error wrapping in both backends (those already-written
   `emit_sentinel_scalar_option_wrap` etc. helpers directly become these instructions
   emitted at their call sites).

5. **Add `StructInit`** — eliminates newtype constructor special-casing
   (~50 lines each backend).

6. **Add `NamedFieldPtr`** — eliminates hardcoded `i64 8` / `i64 16` offsets
   for opaque runtime struct fields (~50 sites across backends).

7. **Require `param_kinds` on `CallClosure`** — replaces my SlotAddr heuristic
   with declared metadata. Unblocks correct closure dispatch for all cases.

### Priority 3 — Large effort, largest cleanup

8. **`HofExpand` + lowering pass** — ~2,100 lines of duplicated HOF inlining
   become one lowering pass + a `HofExpand` instruction variant. This is the
   big one. Also unlocks **fusion optimizations** (adjacent HofExpand ops can
   merge — same-pass filter+map, etc.).

### Priority 4 — Smaller but valuable

9. **`TraitCall`** (~80 lines removed from LLVM, fixes `print_trait_object`-style
   struct-by-value method cases).

10. **`CowClone { src, ty }`** — explicit CoW materialization instead of the
    C backend's inline injection in SlotStore. Both backends translate to the
    same `gorget_string_copy_cow` call.

## Fundamental Architectural Issue

**Yes, there is one.** The LIR's design doc says:

> *"Explicit everything. If the C backend currently generates code for it,
> LIR must have an instruction for it."*

The LIR is **faithful to this principle for explicit operations** (load, store,
branch). But it **abandons the principle for implicit operations** — CallExtern
with a specific name carries *semantic meaning* that the backend decodes.

Concretely: `CallExtern { name: "Vector__int64_t__filter" }` is not an explicit
operation — it's a *symbolic reference* to a pattern the backend must unfold.
That's not "explicit everything." That's "explicit for scalars, implicit for
collections/closures/sentinels."

The fix: treat each such pattern as a distinct LIR instruction during lowering.
Backends only see unfolded primitives. That's the contract the design doc
promised but hasn't fully delivered.

## Answers to "Best Compiler Ever" Question

The ingredients that separate world-class compilers from merely good ones:

1. **Progressive lowering** — Multiple named IR stages with validators.
   Swift, Rustc (MIR + LIR), GHC, V8. *Add to Gorget:* Canonical LIR + Core LIR
   distinction with a validator asserting stage invariants.

2. **Explicit ABI at call sites** — Cranelift and QBE carry signatures on
   `call_indirect`. *Add to Gorget:* `param_kinds` on CallClosure, richer
   AbiKind on CallExtern.

3. **High-level ops that lower** — SIL's `apply`/`init_enum_data_addr`,
   MLIR's `scf.for`. Let optimizations work at the highest level they can.
   *Add to Gorget:* `HofExpand`, `EnumInit`, `TraitCall`.

4. **No name-based dispatch** — QBE, Cranelift. Names are just labels — meaning
   is in the instruction kind and operand types. *Fix in Gorget:* eliminate all
   `name == "X"` / `name.starts_with` via Priority 1-3 lifts above.

5. **Shared metadata tables** — LLVM's intrinsic table, SPIR-V's op table —
   one place defines each operation's semantics. *Extend in Gorget:*
   `opaque_runtime_size` → `opaque_runtime_layout` with fields and offsets.

6. **Ownership in IR (optional)** — Swift OSSA, Roc MonoIR. Gorget already
   handles this at GIR level. Keep it there; LIR stays ownership-unaware per
   design doc.

## Not Recommended

- **Full MLIR-style dialects.** Gorget is a single language; the dialect
  infrastructure is overengineering (same conclusion as original LIR design doc).

- **Sea of Nodes.** V8 abandoned it. Harder to debug, slower to compile.

- **Stack-based bytecode.** We want to target LLVM/native, not a VM.

## Summary Recommendation

Implement Priorities 1-3 in order. After that, the backends will be
~60% smaller and truly dumb. The HofExpand lift (Priority 3, #8) is the
biggest single win — 2,000 lines of duplicated HOF code become one shared
lowering pass.

After this work:
- Adding a WASM backend becomes feasible (~1,500 lines vs today's 6,500).
- Adding an LLVM-JIT backend becomes feasible (same).
- Freestanding/UEFI builds get simpler (no runtime-specific smartness in
  the backend to work around).
- Self-host LIR backend (currently ~6,200 lines, 656 fixtures) catches up
  faster because it has less semantics to reimplement.

The "best compiler ever" outcome isn't from one heroic feature — it's from
**consistent application of "explicit everything, backends are dumb"** until
no semantic decisions remain at the emit layer.
