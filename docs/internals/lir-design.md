# LIR Design — Low-Level Intermediate Representation

## Motivation

The current compilation pipeline is:

```
.gg source → Lexer → Parser → Semantic → GIR lowering → C backend → binary
```

GIR (Gorget IR) is a high-level, pre-SSA IR that carries type information and ownership semantics. The C backend then performs **31 categories of implicit semantic work** on top of GIR — drop glue emission, vtable construction, closure dispatch, type coercions, collection method inlining, iterator protocol, printf formatting, and more. This work lives entirely in the 10,000-line C backend and is invisible to the optimizer.

This coupling creates two problems:

1. **Optimization ceiling.** Dead function elimination, copy propagation, and inlining all fail because the optimizer can't see what the backend will reference. Every optimization pass requires reverse-engineering C backend assumptions.

2. **Backend lock-in.** Adding LLVM or WASM backends means reimplementing all 31 categories of implicit work. Each new backend is a full compiler, not a thin translation layer.

LIR solves both by making all implicit operations explicit *before* backend emission.

## Research Base

This design is informed by study of six production SSA IRs:

- **Cranelift CLIF** — Block parameters (not phi nodes), no aggregate types, FunctionBuilder with def_var/use_var for SSA construction. Validates block-parameter approach.
- **Swift SIL (OSSA)** — Ownership annotations on SSA values, loadable vs address-only type split, `destructure_struct` for consuming aggregates. Proves SSA + ownership is possible but adds significant complexity.
- **Rust MIR** — Non-SSA with mutable places. Chosen because borrow checking runs on MIR. Gorget's borrow checking runs on the AST, so this constraint doesn't apply.
- **QBE** — Accepts non-SSA input and constructs SSA automatically. 4 base types, no pointer type. ~15k lines, targets "70% of LLVM's performance in 10% of the code."
- **Go SSA** — Memory as a first-class SSA value. Decompose pass breaks aggregates into scalars. Architecture-specific lowering via rewrite rules.
- **Zig** — 4 IR layers (ZIR→AIR→MIR→machine code). Untyped pre-specialization IR cached per-file. Backend-specific MIR dialects.

Key takeaways:
- **Sea of Nodes rejected** — V8 abandoned it for CFG after years of investment. Too complex, slower compile times, harder to debug.
- **MLIR rejected** — dialect infrastructure is overengineered for a single-language compiler.
- **Ownership in LIR unnecessary** — Gorget's borrow checker runs on the AST. By LIR time, all ownership decisions are made and drops are inserted as regular calls. (Swift needs OSSA because its borrow analysis runs on SIL.)

## New Pipeline

```
.gg → Lexer → Parser → Semantic → GIR → LIR → Backend (C / LLVM / WASM)
```

- **GIR** stays as-is: high-level, ownership-aware, good for monomorphization, drop insertion, closure lifting, trait dispatch. No changes needed.
- **LIR** is a new SSA-form IR where every operation the backend performs is an explicit instruction. Backends become thin 1:1 translators.
- **GIR → LIR lowering** is a new pass that absorbs the 10,000 lines of implicit C backend logic into structured IR.

## Design Principles

1. **SSA with block parameters** (Cranelift-style). Block parameters are simpler to construct and transform than phi nodes, and map naturally to both C (via parallel-move lowering) and LLVM IR (via phi insertion). Validated by Cranelift's production use in Wasmtime.

2. **Non-SSA input with mechanical SSA construction.** GIR→LIR lowering emits place-based code (store/load to named slots). A separate pass promotes slots to SSA values and inserts block parameters. This is the QBE/LLVM mem2reg pattern — dramatically simpler than constructing SSA during lowering.

3. **Address-only aggregates.** Scalars (int, float, bool, pointers) are SSA values. Structs, enums, and arrays live in memory (stack slots), accessed via typed field projections. This matches GIR's current model, maps naturally to C, and works well with LLVM (alloca+mem2reg) and WASM (linear memory). SSA aggregates (SIL-style) can be added later as a targeted optimization for small structs.

4. **No ownership tracking.** LIR is ownership-unaware. Drop calls are regular function calls. No `@owned`/`@guaranteed` annotations. Borrow checking is done, drops are placed — LIR just executes them.

5. **Explicit everything.** If the C backend currently generates code for it, LIR must have an instruction for it. No implicit drops, no implicit coercions, no name-convention dispatch.

6. **Backend-agnostic.** LIR knows about memory layout but not about C syntax, LLVM intrinsics, or WASM opcodes. Backends translate LIR 1:1 without semantic decisions.

## Type System

LIR types are concrete machine representations. Following QBE's philosophy of minimalism, but with enough structure for sizeof/layout computation.

```rust
/// Concrete machine type — no generics, no ownership qualifiers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LirType {
    // Scalars (SSA values)
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool,                           // i8 with 0/1 semantics (may remove per Cranelift's lesson)
    Ptr,                            // opaque pointer (like LLVM's ptr)

    // Aggregates (address-only — live in stack slots, not SSA values)
    Struct(StructId),

    // Special
    Void,
}
```

Note: no separate `FnPtr` type — function pointers are just `Ptr`. No separate `RawPtr` vs typed pointer — all pointers are opaque `Ptr` (like LLVM's opaque pointer transition). The type of the pointed-to data is carried by load/store instructions, not the pointer itself.

### Struct Definitions

```rust
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, LirType)>,
    // Size and alignment computed lazily or by the backend
}
```

Structs cover all aggregate types:
- Gorget structs → LIR Struct with named fields
- Gorget enums → LIR Struct with `tag: I32` + union-like variant fields
- Gorget tuples → LIR Struct with positional fields `_0`, `_1`, ...
- `Str` → Struct `{ data: Ptr, len: I64 }`
- `GorgetString` → Struct `{ data: Ptr, len: I64, cap: I64 }`
- `GorgetArray` → Struct `{ data: Ptr, len: I64, cap: I64, elem_size: I64 }`
- Closures → Struct `{ fn_ptr: Ptr, env: Ptr }`
- Trait objects → Struct `{ data: Ptr, vtable: Ptr }`
- Tasks → Struct `{ task_ptr: Ptr, drop_fn: Ptr }`

### GIR → LIR Type Mapping

| GIR / Gorget Type | LIR Type |
|---|---|
| `int` / `int64` | `I64` |
| `float` / `float64` | `F64` |
| `bool` | `Bool` |
| `str` | `Struct(Str)` |
| `String` | `Struct(GorgetString)` |
| `cstr` | `Ptr` |
| `Vector[T]` | `Struct(GorgetArray)` |
| `Dict[K,V]` | `Struct(GorgetDict_K_V)` |
| `Option[T]` | `Struct(Option_T)` |
| `Result[T,E]` | `Struct(Result_T_E)` |
| `Box[T]` | `Ptr` |
| `Box[Trait]` | `Struct(TraitObj)` |
| `Shared[T]` | `Ptr` |
| `Callable[R(P...)]` | `Struct(Closure)` |
| User struct | `Struct(S)` — fields mapped recursively |
| User enum | `Struct(E)` — `{ I32, union fields }` |

## Function Representation

```rust
pub struct LirFunction {
    pub name: String,
    pub params: Vec<LirType>,       // parameter types (slots _1.._N)
    pub return_type: LirType,
    pub slots: Vec<Slot>,            // named memory slots (pre-SSA "locals")
    pub blocks: Vec<Block>,
}

/// A named memory slot — the pre-SSA representation of a local variable.
/// SSA construction promotes scalar slots to SSA values + block parameters.
/// Aggregate slots remain as stack allocations.
pub struct Slot {
    pub ty: LirType,
    pub name: Option<String>,        // debug name hint
}

/// A basic block with optional parameters (added by SSA construction).
pub struct Block {
    pub id: BlockId,
    pub params: Vec<(ValueId, LirType)>,
    pub insts: Vec<Inst>,
    pub terminator: Term,
}
```

### Pre-SSA vs Post-SSA

The same data structures serve both phases:

**Pre-SSA (output of GIR→LIR lowering):**
- All values go through slots: `SlotStore { slot, value }` / `SlotLoad { dst, slot }`
- Block params are empty (no merge-point resolution yet)
- Scalar slots may be stored multiple times (not single-assignment)

**Post-SSA (output of SSA construction pass):**
- Scalar slots promoted to SSA values — `SlotStore`/`SlotLoad` replaced with direct value references
- Block params populated at merge points
- Aggregate slots remain as stack allocations with `SlotAddr`/`Load`/`Store`

## Instruction Set

```rust
pub type ValueId = u32;
pub type SlotId = u32;
pub type BlockId = u32;
pub type StructId = u32;
pub type FuncId = u32;
pub type GlobalId = u32;

pub enum Term {
    Ret(ValueId),
    RetVoid,
    Jump(BlockId, Vec<ValueId>),
    Branch {
        cond: ValueId,
        then_block: BlockId, then_args: Vec<ValueId>,
        else_block: BlockId, else_args: Vec<ValueId>,
    },
    Switch {
        value: ValueId,
        cases: Vec<(i64, BlockId, Vec<ValueId>)>,
        default: BlockId, default_args: Vec<ValueId>,
    },
    Unreachable,
}

pub enum Inst {
    // ── Slot Access (pre-SSA, lowered by SSA construction) ────
    SlotStore  { slot: SlotId, value: ValueId },
    SlotLoad   { dst: ValueId, slot: SlotId, ty: LirType },
    SlotAddr   { dst: ValueId, slot: SlotId },           // address of slot (for aggregates)

    // ── Constants ─────────────────────────────────────────────
    IConst     { dst: ValueId, ty: LirType, value: i64 },
    FConst     { dst: ValueId, ty: LirType, value: f64 },
    BoolConst  { dst: ValueId, value: bool },
    NullPtr    { dst: ValueId },
    FuncAddr   { dst: ValueId, func: FuncId },
    GlobalAddr { dst: ValueId, global: GlobalId },
    StrLit     { dst: ValueId, data: Vec<u8> },          // static string → Str struct

    // ── Arithmetic ────────────────────────────────────────────
    Add        { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Sub        { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Mul        { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Div        { dst: ValueId, lhs: ValueId, rhs: ValueId },
    Rem        { dst: ValueId, lhs: ValueId, rhs: ValueId },
    Mod        { dst: ValueId, lhs: ValueId, rhs: ValueId },  // Python semantics
    Neg        { dst: ValueId, operand: ValueId },

    // ── Bitwise ───────────────────────────────────────────────
    BitAnd     { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitOr      { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitXor     { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitNot     { dst: ValueId, operand: ValueId },
    Shl        { dst: ValueId, lhs: ValueId, rhs: ValueId },
    Shr        { dst: ValueId, lhs: ValueId, rhs: ValueId },

    // ── Comparison & Logic ────────────────────────────────────
    Cmp        { dst: ValueId, op: CmpOp, lhs: ValueId, rhs: ValueId },
    Not        { dst: ValueId, operand: ValueId },

    // ── Type Conversions (ALL coercions are explicit) ─────────
    IntCast    { dst: ValueId, value: ValueId, to: LirType },  // int widening/narrowing
    FloatCast  { dst: ValueId, value: ValueId, to: LirType },  // float precision change
    IntToFloat { dst: ValueId, value: ValueId, to: LirType },
    FloatToInt { dst: ValueId, value: ValueId, to: LirType },
    PtrCast    { dst: ValueId, value: ValueId },               // pointer reinterpret
    Bitcast    { dst: ValueId, value: ValueId, to: LirType },  // same-size reinterpret

    // ── Memory ────────────────────────────────────────────────
    Load       { dst: ValueId, ptr: ValueId, ty: LirType },
    Store      { ptr: ValueId, value: ValueId },
    FieldPtr   { dst: ValueId, base: ValueId, struct_id: StructId, field: u32 },
    ElemPtr    { dst: ValueId, base: ValueId, index: ValueId, elem_size: u32 },
    Memset     { ptr: ValueId, byte: ValueId, size: ValueId },
    Memcpy     { dst_ptr: ValueId, src_ptr: ValueId, size: ValueId },

    // ── Calls ─────────────────────────────────────────────────
    Call       { dst: Option<ValueId>, func: FuncId, args: Vec<ValueId> },
    CallExtern { dst: Option<ValueId>, name: String, args: Vec<ValueId> },
    CallPtr    { dst: Option<ValueId>, callee: ValueId, args: Vec<ValueId> },

    // ── Runtime Checks ────────────────────────────────────────
    BoundsCheck { index: ValueId, len: ValueId },              // trap if index >= len
    DivCheck    { divisor: ValueId },                          // trap if divisor == 0
    Trap        { msg: String },                               // unconditional abort

    // ── Printf (pragmatic high-level instruction) ─────────────
    // Str args pre-expanded to (len, data) pairs during lowering.
    // Backend lowers to platform-appropriate printf/fprintf.
    Printf     { fmt: String, args: Vec<ValueId> },
    Fprintf    { fd: ValueId, fmt: String, args: Vec<ValueId> },

    // ── Nop (source mapping placeholder) ──────────────────────
    Nop,
}

pub enum Overflow {
    Trap,   // default: abort on overflow
    Wrap,   // wrapping arithmetic (+%, -%, *%)
}

pub enum CmpOp {
    Eq, Ne, Lt, Le, Gt, Ge,
}
```

## Module Structure

```rust
pub struct LirModule {
    pub structs: Vec<StructDef>,
    pub globals: Vec<LirGlobal>,
    pub functions: Vec<LirFunction>,
    pub externs: Vec<LirExtern>,
    pub source_filename: Option<String>,
}

pub struct LirGlobal {
    pub name: String,
    pub ty: LirType,
    pub init: LirGlobalInit,
    pub is_const: bool,
}

pub enum LirGlobalInit {
    Zeroed,
    Bytes(Vec<u8>),
    FuncAddr(FuncId),
    Struct { struct_id: StructId, fields: Vec<LirGlobalInit> },
}

pub struct LirExtern {
    pub name: String,
    pub params: Vec<LirType>,
    pub return_type: LirType,
}
```

## SSA Construction Pass

The SSA construction pass runs after GIR→LIR lowering. It uses the standard algorithm (Braun et al. 2013, "Simple and Efficient Construction of SSA Form"):

1. **Identify promotable slots.** A slot is promotable if it has scalar type and is never addressed (no `SlotAddr` pointing to it). Aggregate slots and addressed slots stay as stack allocations.

2. **For each promotable slot**, walk blocks in dominator-tree order:
   - `SlotStore { slot, value }` → record `value` as the current definition of `slot`
   - `SlotLoad { dst, slot }` → replace with the reaching definition of `slot`
   - At merge points (blocks with multiple predecessors), insert block parameters

3. **Remove promoted `SlotStore`/`SlotLoad` instructions.** The slot itself becomes dead.

4. **Update terminators.** Jumps/branches to blocks with parameters must provide matching arguments.

This is the same algorithm as LLVM's mem2reg and QBE's SSA fixup.

## What Moves from C Backend to GIR→LIR Lowering

Each category of implicit C backend work becomes explicit LIR instructions:

### 1. Drop Glue → Regular Call/Loop Sequences

GIR `Drop { place }` is resolved during lowering using the type registry. The lowering emits explicit calls to drop functions, element-iteration loops, and field-recursive drops as regular LIR instructions (Call, Branch, Load, Store).

### 2. Vtable Dispatch → FieldPtr + Load + CallPtr

```
// Trait method call on Box[Trait]:
%vtable_ptr = field_ptr %trait_obj, TraitObj, 1    // .vtable
%vtable     = load %vtable_ptr, Ptr
%method_ptr = field_ptr %vtable, VTable, 0         // .area slot
%fn         = load %method_ptr, Ptr
%data       = field_ptr %trait_obj, TraitObj, 0    // .data
%result     = call_ptr %fn, [%data]
```

### 3. Closure Dispatch → Load + CallPtr

```
// Indirect closure call:
%fn_ptr = load (field_ptr %closure, Closure, 0), Ptr
%env    = load (field_ptr %closure, Closure, 1), Ptr
%result = call_ptr %fn_ptr, [%env, %arg1, %arg2]
```

### 4. Type Coercions → Explicit FieldPtr + Load/Store

```
// GorgetString → Str (currently implicit in C backend):
%data = load (field_ptr %gs, GorgetString, 0), Ptr
%len  = load (field_ptr %gs, GorgetString, 1), I64
store (field_ptr %str_slot, Str, 0), %data
store (field_ptr %str_slot, Str, 1), %len
```

### 5. Collection Methods → Call to Runtime Functions

All collection method dispatch resolved during lowering. The result is plain `Call` or `CallExtern` instructions to runtime functions (`gorget_array_push`, `gorget_map_get`, etc.).

### 6. Named-Function Adapters → Regular LIR Functions

`__adapt_*` wrappers emitted as regular LIR functions during lowering, with `FuncAddr` instruction to reference the original function.

### 7. Spawn Wrappers → Regular LIR Functions

`__spawn_run_*` and `__spawn_drop_*` emitted as regular LIR functions with explicit `Call` to the spawned function.

### 8. Printf Formatting → Pre-expanded Args

Str arguments expanded to `(len, data)` pairs during lowering:
```
%data    = load (field_ptr %str, Str, 0), Ptr
%len     = load (field_ptr %str, Str, 1), I64
%len_i32 = int_cast %len, I32
printf "%.*s", [%len_i32, %data]
```

### 9. Test Harness → Regular LIR main() Function

Test runner main() with setjmp/timing/cleanup emitted as regular LIR code.

## Backend Responsibilities (Post-LIR)

Each backend is a thin translator with no semantic decisions:

### C Backend (~2000 lines, down from ~10,000)

- `LirType` → C type string
- `Inst` → C statement (1:1)
- `Block` → labeled block with gotos
- Block parameters → parallel move variables + goto
- `StructDef` → C struct/union/typedef
- `Slot` → C local variable declaration
- `#include` for runtime library

### LLVM Backend (~1500 lines)

- `LirType` → LLVM type (scalars direct, Ptr → `ptr`)
- `Inst` → LLVM instruction (1:1)
- `Slot` → `alloca` (LLVM's mem2reg promotes further)
- Block parameters → phi nodes (mechanical transformation)
- Debug info from source spans

### WASM Backend (~1500 lines)

- `LirType` → WASM value types
- Restructure CFG into structured control flow (Relooper/Stackifier)
- `Slot` → WASM locals or linear memory offsets
- `Inst` → WASM instructions

## Optimization Passes on LIR

All optimizations move to post-SSA LIR:

| Pass | Works on GIR? | Works on LIR? | Notes |
|---|---|---|---|
| Constant propagation | Yes (keep as pre-pass) | Yes — full SSA | |
| Constant folding | Yes (keep) | Yes | |
| Dead code elimination | Yes (keep) | Yes — trivial (unused ValueId) | |
| Dead function elimination | **Broken** | **Works** — all refs explicit | The main motivator |
| Copy propagation (scalar) | **Broken** | **Works** — coercions explicit | Second motivator |
| Common subexpression elim. | Yes (limited) | Yes — hash-consing on values | |
| Function inlining | No | **New** — straightforward on SSA | |
| Strength reduction | Yes (keep) | Yes | |
| Loop-invariant code motion | No | **New** — dominator tree + loops | |
| Tail call optimization | No | **New** — detect in terminators | |
| Escape analysis | No | **New** — stack-allocate non-escaping | |

## Implementation Plan

### Phase 1: Data Structures + Skeleton

- `src/lir/mod.rs` — `LirModule`, `LirFunction`, `Block`, `Inst`, `Term`, `LirType`, `StructDef`
- `src/lir/types.rs` — type IDs, struct registry, type mapping helpers
- `src/lir/display.rs` — human-readable LIR dump (`gg build --dump-lir`)
- `src/lir/validate.rs` — invariant checking (post-SSA: every use dominated by def, block params match jump args)
- No behavioral changes — just data structures and printers.

### Phase 2: GIR → LIR Lowering (Incremental)

Emits **pre-SSA** LIR (slot-based, not SSA). Each sub-phase adds one category:

1. **Scalars + arithmetic + control flow** — constants, binops, cmp, branch, return
2. **Function calls** — Call, CallExtern
3. **Structs + field access** — SlotAddr, FieldPtr, Load/Store
4. **Enums + match** — tag field, Switch
5. **Type conversions** — all explicit Cast/Coercion instructions
6. **Drop elaboration** — resolve GIR `Drop` into call sequences
7. **Closures** — closure struct, CallPtr, adapter emission
8. **Vtables + trait objects** — vtable globals, dispatch via CallPtr
9. **Collections** — constructor/method lowering to runtime calls
10. **Concurrency** — spawn wrappers, coroutine state machines, task types
11. **Test harness** — test runner main()

### Phase 2.5: SSA Construction

- Implement Braun et al. algorithm for slot promotion
- Validate with existing test suite
- Add `--dump-lir` flag showing both pre-SSA and post-SSA forms

### Phase 3: C Backend on LIR

- New `src/backend/c_lir/` — thin C emitter (~2000 lines)
- Wire `gg build --backend=c-lir` for A/B testing
- Run integration tests against both backends
- Once at parity, replace old C backend

### Phase 4: LIR Optimizations

- Dead function elimination (trivial — all refs explicit)
- Copy propagation (trivial — coercions explicit)
- Function inlining
- Constant propagation on SSA (more powerful than GIR version)

### Phase 5: LLVM Backend

- `src/backend/llvm/` using `inkwell` crate
- `gg build --backend=llvm`
- Inherits all LIR optimizations

### Phase 6: WASM Backend

- `src/backend/wasm/` with Relooper for structured control flow
- `gg build --backend=wasm`

## What Stays in GIR

GIR is NOT replaced. It stays as the high-level IR for:

- Monomorphization (generic instantiation)
- Ownership/borrow checking validation
- Drop insertion (deciding WHERE drops go — LIR decides HOW)
- Closure lifting (deciding WHAT to capture)
- Trait method resolution (deciding WHICH function — LIR handles dispatch mechanics)
- Quick pre-optimization (constant folding, dead block/local elimination)

**The split: GIR decides semantics, LIR decides mechanics.**

## Open Questions

1. **Bool type: keep or remove?** Cranelift removed booleans in 2022 (just use i8 with 0/1) to eliminate representation ambiguity. Worth considering for simplicity. Current plan: keep `Bool` initially, evaluate during implementation.

2. **Printf as instruction vs Call to runtime.** Printf is kept as a high-level instruction for ergonomics and because format-string expansion varies by backend. Alternative: lower to explicit write() calls during GIR→LIR lowering. Current plan: keep Printf, let backends lower it.

3. **Coroutine state machines: GIR→LIR or LIR→LIR?** Current plan: GIR→LIR lowering emits the state machine as regular blocks. Alternative: emit normal function bodies and transform to state machines as a LIR→LIR pass. The latter allows optimizing before state splitting.

4. **String literal deduplication.** Leave to backend initially (C compiler / LLVM handle this).

5. **Debug info / source spans.** Separate span map indexed by instruction position, matching GIR's `inst_spans` approach. Not needed for Phase 1.
