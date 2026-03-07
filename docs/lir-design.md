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

## New Pipeline

```
.gg → Lexer → Parser → Semantic → GIR → LIR → Backend (C / LLVM / WASM)
```

- **GIR** stays as-is: high-level, ownership-aware, good for monomorphization, drop insertion, closure lifting, trait dispatch. No changes needed.
- **LIR** is a new SSA-form IR where every operation the backend performs is an explicit instruction. Backends become thin 1:1 translators.
- **GIR → LIR lowering** is a new pass that absorbs the 10,000 lines of implicit C backend logic into structured IR.

## Design Principles

1. **SSA with block parameters** (Cranelift-style, not phi nodes). Block parameters are simpler to construct and transform than phi nodes, and map naturally to both C (via parallel-move lowering) and LLVM IR (via phi insertion).

2. **Explicit everything.** If the C backend currently generates code for it, LIR must have an instruction for it. No implicit drops, no implicit coercions, no name-convention dispatch.

3. **Typed instructions.** Every value has an LIR type. Types are concrete (no generics — monomorphization happens in GIR). Types include pointer representations, struct layouts, and function signatures.

4. **Backend-agnostic.** LIR knows about memory layout but not about C syntax, LLVM intrinsics, or WASM opcodes. Backends translate LIR 1:1 without semantic decisions.

5. **Optimizable.** Standard SSA optimizations (constant propagation, dead code elimination, copy propagation, function inlining, common subexpression elimination) work directly on LIR with no special cases.

## Type System

LIR types are concrete machine representations, not Gorget semantic types.

```rust
enum LirType {
    // Scalars
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool,
    Ptr(Box<LirType>),         // typed pointer
    RawPtr,                     // void*
    FnPtr(FnSig),              // typed function pointer

    // Aggregates (all fields have concrete LirType)
    Struct(StructId),           // reference to StructDef
    Array(Box<LirType>, usize), // fixed-size array [T; N]

    // Special
    Void,                       // for void-returning functions
}

struct StructDef {
    name: String,
    fields: Vec<(String, LirType)>,
    size: usize,
    align: usize,
}

struct FnSig {
    params: Vec<LirType>,
    ret: LirType,
}
```

### GIR → LIR Type Mapping

| GIR / Gorget Type | LIR Type |
|---|---|
| `int` / `int64` | `I64` |
| `float` / `float64` | `F64` |
| `bool` | `Bool` |
| `str` | `Struct(Str)` — `{ Ptr(U8), I64 }` (data, len) |
| `String` | `Struct(GorgetString)` — `{ Ptr(U8), I64, I64 }` (data, len, cap) |
| `cstr` | `Ptr(U8)` |
| `Vector[T]` | `Struct(GorgetArray)` — `{ RawPtr, I64, I64, I64 }` (data, len, cap, elem_size) |
| `Dict[K,V]` | `Struct(GorgetDict_K_V)` |
| `Option[T]` | `Struct(Option_T)` — `{ I32, Union(T) }` |
| `Result[T,E]` | `Struct(Result_T_E)` — `{ I32, Union(T,E) }` |
| `Box[T]` | `Ptr(T)` |
| `Box[Trait]` | `Struct(TraitObj)` — `{ RawPtr, Ptr(VTable) }` |
| `Shared[T]` | `Ptr(SharedInner_T)` |
| `Callable[R(P...)]` | `Struct(Closure)` — `{ FnPtr, RawPtr }` (fn_ptr, env) |
| User struct `S` | `Struct(S)` — fields mapped recursively |
| User enum `E` | `Struct(E)` — `{ I32, Union(...variants...) }` |
| `Task[T]` | `Struct(Task_T)` — `{ RawPtr, FnPtr }` (task_ptr, drop_fn) |

## Instruction Set

### Values and Control Flow

```rust
/// SSA value — produced by exactly one instruction, used zero or more times.
/// In block parameters and instructions, values are referenced by ValueId.
struct ValueId(u32);

/// A basic block with parameters (replaces phi nodes).
struct Block {
    id: BlockId,
    params: Vec<(ValueId, LirType)>,   // block parameters (SSA "phis")
    insts: Vec<Inst>,
    terminator: Term,
}

enum Term {
    Ret(ValueId),
    RetVoid,
    Jump { target: BlockId, args: Vec<ValueId> },
    Branch { cond: ValueId, then_: BlockId, then_args: Vec<ValueId>,
             else_: BlockId, else_args: Vec<ValueId> },
    Switch { value: ValueId, cases: Vec<(i64, BlockId, Vec<ValueId>)>,
             default: BlockId, default_args: Vec<ValueId> },
    Unreachable,
}
```

### Instructions

Each instruction produces zero or one `ValueId`. All side-effecting operations are explicit.

```rust
enum Inst {
    // ── Constants ──────────────────────────────────────────────
    IConst   { dst: ValueId, ty: LirType, value: i64 },
    FConst   { dst: ValueId, ty: LirType, value: f64 },
    BoolConst{ dst: ValueId, value: bool },
    StrLit   { dst: ValueId, data: Vec<u8> },  // emits static const + Str{} init
    NullPtr  { dst: ValueId, ty: LirType },
    FuncAddr { dst: ValueId, func: FuncId },    // address of a function
    GlobalAddr { dst: ValueId, global: GlobalId },

    // ── Arithmetic ────────────────────────────────────────────
    // All arithmetic is explicit about overflow behavior.
    Add      { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Sub      { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Mul      { dst: ValueId, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Div      { dst: ValueId, lhs: ValueId, rhs: ValueId },  // traps on zero
    Rem      { dst: ValueId, lhs: ValueId, rhs: ValueId },  // C remainder
    Mod      { dst: ValueId, lhs: ValueId, rhs: ValueId },  // Python modulo
    Neg      { dst: ValueId, operand: ValueId },
    // Bitwise
    BitAnd   { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitOr    { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitXor   { dst: ValueId, lhs: ValueId, rhs: ValueId },
    BitNot   { dst: ValueId, operand: ValueId },
    Shl      { dst: ValueId, lhs: ValueId, rhs: ValueId },
    Shr      { dst: ValueId, lhs: ValueId, rhs: ValueId },
    // Comparison
    Cmp      { dst: ValueId, op: CmpOp, lhs: ValueId, rhs: ValueId },
    // Logical
    Not      { dst: ValueId, operand: ValueId },

    // ── Type Conversions ──────────────────────────────────────
    // ALL coercions are explicit instructions.
    IntCast  { dst: ValueId, value: ValueId, to: LirType },     // int widening/narrowing
    FloatCast{ dst: ValueId, value: ValueId, to: LirType },     // float widening/narrowing
    IntToFloat { dst: ValueId, value: ValueId, to: LirType },
    FloatToInt { dst: ValueId, value: ValueId, to: LirType },
    PtrCast  { dst: ValueId, value: ValueId, to: LirType },     // pointer reinterpret
    Bitcast  { dst: ValueId, value: ValueId, to: LirType },     // same-size reinterpret

    // ── String coercions (currently implicit in C backend) ────
    // GorgetString → Str: extract data+len into Str struct
    StringToStr { dst: ValueId, value: ValueId },
    // Str → const char*: extract .data field (NOTE: not null-terminated!)
    StrToPtr { dst: ValueId, value: ValueId },
    // const char* → Str: wrap with strlen
    PtrToStr { dst: ValueId, value: ValueId },
    // Str → GorgetString: heap-copy
    StrToString { dst: ValueId, value: ValueId },

    // ── Memory ────────────────────────────────────────────────
    StackAlloc { dst: ValueId, ty: LirType },                   // alloca
    HeapAlloc  { dst: ValueId, ty: LirType, allocator: ValueId },
    HeapAllocArray { dst: ValueId, ty: LirType, count: ValueId, allocator: ValueId },
    Free     { ptr: ValueId, allocator: ValueId },
    Load     { dst: ValueId, ptr: ValueId, ty: LirType },       // *ptr
    Store    { ptr: ValueId, value: ValueId },                   // *ptr = value
    Memset   { ptr: ValueId, value: ValueId, size: ValueId },   // memset (for move zeroing)
    Memcpy   { dst: ValueId, src: ValueId, size: ValueId },     // memcpy (for deep copy)

    // ── Aggregate Access ──────────────────────────────────────
    // All struct/enum field access goes through explicit GEP + Load/Store.
    GetFieldPtr { dst: ValueId, base: ValueId, struct_id: StructId, field: u32 },
    GetElementPtr { dst: ValueId, base: ValueId, index: ValueId, elem_ty: LirType },
    ExtractValue { dst: ValueId, aggregate: ValueId, field: u32 },  // SSA aggregate extract
    InsertValue  { dst: ValueId, aggregate: ValueId, field: u32, value: ValueId },

    // ── Aggregate Construction ────────────────────────────────
    StructLit { dst: ValueId, struct_id: StructId, fields: Vec<ValueId> },
    // Enum variant: sets tag + variant data
    EnumLit   { dst: ValueId, struct_id: StructId, tag: i32,
                variant_field: u32, fields: Vec<ValueId> },

    // ── Calls ─────────────────────────────────────────────────
    // Direct call to a known function.
    Call     { dst: Option<ValueId>, func: FuncId, args: Vec<ValueId> },
    // Indirect call through a function pointer (closures, vtable dispatch).
    CallPtr  { dst: Option<ValueId>, callee: ValueId, sig: FnSig, args: Vec<ValueId> },

    // ── Drop / Cleanup ────────────────────────────────────────
    // Explicit drop call — the GIR→LIR lowering resolves the drop strategy
    // and emits the appropriate sequence (call custom drop, iterate fields,
    // loop over collection elements, etc.) as regular instructions.
    // There is NO implicit drop instruction in LIR.
    //
    // Example: dropping a Vector[String] becomes:
    //   loop over elements { call gorget_string_free(elem) }
    //   call gorget_array_free(vec)
    //
    // This is emitted as regular Call + branch instructions during lowering.

    // ── Overflow Trap ─────────────────────────────────────────
    Trap     { msg: String },  // abort with message (overflow, bounds, etc.)

    // ── Checked Operations ────────────────────────────────────
    // Bounds check: traps if index >= len.
    BoundsCheck { index: ValueId, len: ValueId },
    // Division-by-zero check: traps if divisor == 0.
    DivCheck { divisor: ValueId },

    // ── Printf (kept as a high-level instruction for ergonomics) ──
    // Backend lowers to appropriate printf/fprintf with format expansion.
    // All Str args are pre-expanded to (len, data) pairs during lowering.
    Printf   { fmt: String, args: Vec<ValueId> },
    Fprintf  { fd: ValueId, fmt: String, args: Vec<ValueId> },

    // ── Inline Assembly / Backend Escape Hatch ────────────────
    InlineAsm { template: String, inputs: Vec<ValueId>, outputs: Vec<ValueId> },
}

enum Overflow {
    Trap,   // default: abort on overflow
    Wrap,   // wrapping arithmetic (+%, -%, *%)
    // Future: Saturate, Clamp
}
```

## What Moves from C Backend to GIR→LIR Lowering

Each category of implicit C backend work becomes explicit LIR instructions during the lowering pass:

### 1. Drop Glue → Regular Call/Loop Instructions

GIR `Drop { place }` is resolved during lowering using the type registry:

```
// GIR:  Drop { place: _5 }  (where _5: Vector[String])
// LIR:
    %len = load %vec_ptr.len
    %i = block_param(0)           // loop counter
    jump loop_body(%i)
loop_body(%i):
    %done = cmp ge %i, %len
    branch %done, loop_exit(), loop_iter()
loop_iter:
    %elem_ptr = get_element_ptr %vec_data, %i, Str
    %elem = load %elem_ptr
    call @gorget_string_free(%elem)
    %next = add %i, 1
    jump loop_body(%next)
loop_exit:
    call @gorget_array_free(%vec_ptr)
```

No more `emit_drop_code` / `emit_field_drops` / `lookup_drop_strategy` in the backend.

### 2. Vtable Dispatch → CallPtr through Explicit GEP

```
// GIR:  Call { func: "Shape_for_Circle__area", args: [_3] }
//   (emitted via vtable lookup in C backend)
// LIR:
    %vtable_ptr = get_field_ptr %trait_obj, TraitObj, 1   // .vtable
    %vtable = load %vtable_ptr
    %method_ptr = get_field_ptr %vtable, Shape_VTable, 0  // .area slot
    %fn = load %method_ptr
    %data = get_field_ptr %trait_obj, TraitObj, 0          // .data
    %result = call_ptr %fn, [%data]
```

### 3. Closure Dispatch → CallPtr with Env Extraction

```
// GIR:  CallIndirect { callee: _2, args: [_3] }
// LIR:
    %fn_ptr = extract_value %closure, 0     // .fn_ptr
    %env = extract_value %closure, 1        // .env
    %result = call_ptr %fn_ptr, [%env, %arg]
```

### 4. Type Coercions → Explicit Conversion Instructions

```
// GIR:  Assign { dst: _5, value: Copy(_3) }
//   (where _3: GorgetString, _5: Str — implicit coercion in C backend)
// LIR:
    %data = get_field_ptr %gs, GorgetString, 0  // .data
    %len = get_field_ptr %gs, GorgetString, 1   // .len
    %str = struct_lit Str, [%data, %len]
```

### 5. Collection Methods → Inlined Call Sequences

```
// GIR:  Call { func: "vec_push", args: [&_1, _2] }
//   (C backend inlines bounds check + realloc + memcpy)
// LIR:
    call @gorget_array_push(%vec_ptr, %elem_ptr, %elem_size)
```

Or for methods that the C backend currently inlines (pop, sort, etc.):

```
// vec.pop() → LIR:
    %len = load %vec.len_ptr
    %new_len = sub %len, 1
    store %vec.len_ptr, %new_len
    %elem_ptr = get_element_ptr %vec.data, %new_len, T
    %elem = load %elem_ptr
```

### 6. Named-Function Adapters → FuncAddr + StructLit

```
// GIR:  Assign { dst: _5, value: Constant(FuncRef("add")) }
//   (C backend generates __adapt_add wrapper)
// LIR:
    %adapter = func_addr @__adapt_add      // adapter function emitted during lowering
    %null_env = null_ptr RawPtr
    %closure = struct_lit Closure, [%adapter, %null_env]
```

The adapter function itself is emitted as a regular LIR function during lowering.

### 7. Printf Formatting → Pre-expanded Args

```
// GIR:  CallExtern { func: "printf", args: [fmt, _3] }
//   (C backend expands Str args to (int)len, data)
// LIR:
    %data = extract_value %str, 0
    %len = extract_value %str, 1
    %len_i32 = int_cast %len, I32
    printf "%.*s", [%len_i32, %data]
```

### 8. Iterator Protocol → Explicit Loop CFG

```
// GIR:  (for-loop desugaring is partially in GIR, partially in C backend)
// LIR:  fully explicit loop with call to __next, tag check, branch
    %iter = call @NumberRange__iter(%range_ptr)
loop:
    %opt = call @NumberRangeIter__next(%iter_ptr)
    %tag = extract_value %opt, 0
    %done = cmp eq %tag, 1   // None tag
    branch %done, exit(), body()
body:
    %val = extract_value %opt, 1
    // ... loop body ...
    jump loop()
exit:
    // ... post-loop ...
```

### 9. Spawn Wrappers → Regular LIR Functions

`__spawn_run_*` and `__spawn_drop_*` become regular LIR functions emitted during lowering, with explicit `Call` to the spawned function. No more implicit generation in the backend.

### 10. Test Harness → Regular LIR main() Function

The test runner `main()` with setjmp/longjmp, timing, cleanup registration — all emitted as regular LIR instructions during lowering.

## Function Representation

```rust
struct LirFunction {
    id: FuncId,
    name: String,
    sig: FnSig,
    blocks: Vec<Block>,
    // No locals array — SSA values are the "locals"
}

struct LirModule {
    structs: Vec<StructDef>,
    globals: Vec<LirGlobal>,
    functions: Vec<LirFunction>,
    externs: Vec<ExternDecl>,
    // Runtime strings, vtable data, etc. — all as globals
}

struct LirGlobal {
    id: GlobalId,
    name: String,
    ty: LirType,
    init: GlobalInit,    // Zeroed, ConstStruct, ConstArray, FuncAddr, etc.
    is_const: bool,
}
```

## Optimization Passes on LIR

All optimization passes move to LIR. Current GIR passes can remain as a quick pre-pass, but the heavy lifting happens on SSA:

| Pass | GIR (current) | LIR (new) |
|---|---|---|
| Constant propagation | Yes (keep) | Yes — full SSA, much more powerful |
| Constant folding | Yes (keep) | Yes |
| Dead code elimination | Yes (keep) | Yes — trivial on SSA (unused ValueId) |
| Dead function elimination | Broken (implicit refs) | **Works** — all refs explicit |
| Copy propagation | Broken (type coercion) | **Works** — coercions are explicit Cast instructions |
| Common subexpression elim. | Yes (limited) | Yes — hash-consing on SSA values |
| Function inlining | No | **New** — straightforward on SSA |
| Strength reduction | Yes (keep) | Yes |
| Loop-invariant code motion | No | **New** — dominator tree + loop detection |
| Tail call optimization | No | **New** — detect tail calls in terminators |
| Escape analysis | No | **New** — stack-allocate non-escaping heap objects |

## Backend Responsibilities (Post-LIR)

Each backend becomes a simple translator:

### C Backend (~2000 lines, down from ~10,000)

- Map LirType → C type string
- Map Inst → C statement (1:1)
- Map Block → labeled block with gotos
- Map block parameters → parallel move + goto
- Emit struct/union/typedef declarations from StructDef
- Emit `#include` for runtime library
- No semantic decisions, no type inspection, no name-convention dispatch

### LLVM Backend (~1500 lines, new)

- Map LirType → LLVM type
- Map Inst → LLVM instruction (1:1 — LIR is designed to map closely)
- Block parameters → phi nodes (mechanical transformation)
- Use alloca+mem2reg for register allocation
- Emit debug info from source spans

### WASM Backend (~1500 lines, new)

- Map LirType → WASM value types
- Restructure CFG into structured control flow (Relooper/Stackifier algorithm)
- Map Inst → WASM instructions
- Linear memory layout for structs

## Implementation Plan

### Phase 1: LIR Data Structures + Skeleton

- Define `src/lir/mod.rs` with `LirModule`, `LirFunction`, `Block`, `Inst`, `Term`, `LirType`
- Define `src/lir/types.rs` for `StructDef`, `FnSig`, type mapping
- Define `src/lir/pretty.rs` for human-readable LIR dump (`gg build --dump-lir`)
- Define `src/lir/validate.rs` for SSA invariant checking (every use dominated by def, block params match jump args)
- No behavioral changes — just data structures and printers.

### Phase 2: GIR → LIR Lowering (Core)

Start with the simplest subset and grow incrementally. Each sub-phase adds one category:

1. **Scalars + arithmetic + control flow** — constants, binops, cmp, branch, return. Enough for `hello_world.gg`.
2. **Structs + field access** — StructLit, GetFieldPtr, Load/Store. Enough for struct tests.
3. **Function calls** — Call, CallExtern. Enough for stdlib function calls.
4. **Enums + match** — EnumLit, TagOf, Switch. Enough for Option/Result.
5. **Type conversions** — all Cast/Coercion instructions. Str/String/cstr.
6. **Drop elaboration** — resolve GIR `Drop` into LIR call sequences. The big one.
7. **Closures** — closure struct construction, CallPtr, adapter emission.
8. **Vtables + trait objects** — vtable globals, trait dispatch via CallPtr.
9. **Collections** — collection constructor/method lowering.
10. **Concurrency** — spawn wrappers, coroutine state machines, task types.
11. **Test harness** — test runner main() generation.

### Phase 3: C Backend on LIR

- New `src/backend/c_lir/` — thin C emitter from LIR (target: ~2000 lines)
- Wire `gg build --backend=c-lir` flag for gradual rollout
- Run integration tests against both backends, fix mismatches
- Once at parity, replace old C backend

### Phase 4: LIR Optimizations

- Move existing GIR passes to LIR (they become simpler on SSA)
- Enable dead function elimination (now trivial)
- Enable copy propagation (now trivial)
- Add function inlining

### Phase 5: LLVM Backend

- `src/backend/llvm/` using `inkwell` crate (safe LLVM bindings)
- Wire `gg build --backend=llvm`
- Inherit all LIR optimizations for free

### Phase 6: WASM Backend

- `src/backend/wasm/` with Relooper for structured control flow
- Wire `gg build --backend=wasm`

## Risk Assessment

| Risk | Mitigation |
|---|---|
| GIR→LIR lowering is as complex as the C backend | It IS the C backend logic, restructured. Same complexity, better architecture. The win is write-once vs write-per-backend. |
| Performance regression during transition | Old C backend stays until LIR C backend reaches parity. `--backend=c-lir` flag allows A/B testing. |
| SSA construction complexity | Block parameters (Cranelift-style) are simpler than phi nodes. GIR is already in block form — SSA construction is mostly inserting block params at control-flow merge points. |
| Printf/Fprintf are high-level in a low-level IR | Pragmatic choice. The alternative (lowering to individual write() calls) is worse for readability and debugging. Backends can lower Printf however they want. |
| Scope creep | Strict phase discipline. Each phase has a clear deliverable and can be tested independently. |

## What Stays in GIR

GIR is NOT replaced — it stays as the high-level IR for:

- Monomorphization (generic instantiation)
- Ownership/borrow checking validation
- Drop insertion (deciding WHERE drops go — LIR decides HOW)
- Closure lifting (deciding WHAT to capture)
- Trait method resolution (deciding WHICH function — LIR handles dispatch mechanics)
- Quick pre-optimization (constant folding, dead block/local elimination — cheap wins before LIR lowering)

The split: **GIR decides semantics, LIR decides mechanics.**

## Open Questions

1. **Coroutine state machines in LIR or in GIR→LIR lowering?** Currently the C backend does coroutine transformation (poll function generation, state splitting). This could live in GIR→LIR lowering (emit the state machine as regular LIR blocks) or as a dedicated LIR→LIR transform. The former is simpler; the latter allows optimizing the state machine.

2. **How much inlining of runtime calls?** Some collection methods (vec.pop, vec.len) are trivially inlineable. Should LIR inline them during lowering, or should the runtime remain opaque and let the C compiler / LLVM optimize? Recommendation: keep them as `Call` to runtime functions initially; add selective inlining in Phase 4 if profiling shows benefit.

3. **Debug info representation.** LIR needs source location tracking for error messages and debugger support. Options: (a) attach spans to every instruction (verbose), (b) attach spans to blocks only (coarse), (c) separate span map indexed by ValueId (flexible). Recommendation: (c), matching GIR's `inst_spans` approach.

4. **String literal deduplication.** Multiple `StrLit` with the same content should share storage. Handle during LIR construction (intern into a string table) or leave to the backend (C compiler / LLVM do this anyway)? Recommendation: leave to backend initially.
