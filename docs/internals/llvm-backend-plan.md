# LLVM Backend Plan

## Context

Gorget compiles `.gg` source through: lexer -> parser -> semantic analysis -> GIR lowering -> GIR optimization -> LIR lowering -> SSA construction -> LIR optimization -> backend -> binary.

The sole production backend (`CLirBackend`) generates C code from LIR. This plan adds an LLVM backend that generates LLVM IR textual format (`.ll`), enabling direct compilation via `llc`/`clang` without the C intermediary.

## Why LIR Maps Cleanly to LLVM IR

LIR is already SSA with block parameters (phi-equivalent), typed values, explicit memory operations, and structured control flow. The mapping is nearly 1:1:

| LIR | LLVM IR |
|-----|---------|
| `Inst::Add { dst, ty, lhs, rhs }` | `%dst = add i64 %lhs, %rhs` |
| `Inst::FieldPtr { dst, base, struct_id, field }` | `%dst = getelementptr %S, ptr %base, i32 0, i32 field` |
| `Inst::Load { dst, ptr, ty }` | `%dst = load i64, ptr %ptr` |
| `Inst::Store { ptr, value }` | `store i64 %value, ptr %ptr` |
| `Inst::Call { dst, func, args }` | `%dst = call i64 @func(args)` |
| `Inst::CallExtern { name, args }` | `%dst = call i64 @name(args)` |
| `Term::Branch { cond, then, else }` | `br i1 %cond, label %then, label %else` |
| `Term::Jump(target, args)` | `br label %target` (+ phi nodes) |
| `Term::Switch { value, cases, default }` | `switch i64 %value, label %default [cases]` |
| `Term::Ret(val)` / `Term::RetVoid` | `ret i64 %val` / `ret void` |
| Block params `(ValueId, LirType)` | `phi` nodes |
| `Inst::IConst { value }` | literal constant inline |
| `Inst::StrLit { value }` | global constant string + struct init |
| `Inst::Memcpy/Memset` | `@llvm.memcpy.p0.p0.i64` / `@llvm.memset.p0.i64` |

## LIR Type System -> LLVM Types

| LirType | LLVM Type | Notes |
|---------|-----------|-------|
| I8/U8 | i8 | Signedness tracked by LirType, not LLVM type |
| I16/U16 | i16 | |
| I32/U32 | i32 | |
| I64/U64 | i64 | |
| F32 | float | |
| F64 | double | |
| Bool | i1 | |
| Ptr / PtrTo(_) | ptr | LLVM opaque pointers (since LLVM 15) |
| Struct(id) | %struct.Name | Named struct type |
| Void | void | |

### Signedness Handling

LIR carries signedness in types (I32 vs U32) but NOT in instructions. The LLVM backend must emit signed vs unsigned variants:

| LIR Instruction | Signed (I*) | Unsigned (U*) |
|----------------|-------------|---------------|
| Div | sdiv | udiv |
| Rem | srem | urem |
| Shr | ashr | lshr |
| Cmp Lt/Le/Gt/Ge | icmp slt/sle/sgt/sge | icmp ult/ule/ugt/uge |
| IntCast (widen) | sext | zext |
| IntToFloat | sitofp | uitofp |
| FloatToInt | fptosi | fptoui |

Rule: inspect the `ty` field on each instruction. If it's `I*` -> signed op, `U*` -> unsigned op.

## Struct Layout

LIR `StructDef` has fields `Vec<(String, LirType)>` and `is_enum: bool`. The `computed_c_size: Option<usize>` cache holds the C-ABI-compatible size.

**Regular structs:** Sequential field layout with natural alignment (each field aligned to min(sizeof, 8)).

**Enum structs** (`is_enum == true`): Tag (I32) + union of variant payloads. Field 0 is always "tag", fields 1+ grouped by variant name prefix. LLVM representation: `{ i32, [max_variant_bytes x i8] }`.

**Builtin runtime structs** (pre-registered with known sizes):
- GorgetString (Str): `{ ptr, i64, i64, ptr }` = 32 bytes
- GorgetArray: 56 bytes (7 fields, 3 runtime-internal not in LIR)
- GorgetMap/GorgetSet: 128 bytes (16 fields, 3 runtime-internal)
- GorgetClosure: `{ ptr, ptr }` = 16 bytes
- TaskHandle: `{ ptr, ptr }` = 16 bytes

For LLVM, emit struct types matching C ABI layout. Use `getelementptr` with field indices for FieldPtr.

## C Runtime Linking Strategy

The C runtime is ~13,756 lines embedded in `c_runtime.rs` as string constants. It defines ~1,085 functions (709 static inline, 377 static non-inline).

### Approach: Compile runtime separately, link with LLVM output

1. **At build time**, write the concatenated runtime C source to a temporary file
2. **Compile with clang** to a `.o` object file: `clang -c -O2 -std=c11 gorget_runtime.c -o gorget_runtime.o`
3. **Generate LLVM IR** for user code -> `.ll` file
4. **Compile LLVM IR** with `llc` or `clang`: `clang -c output.ll -o output.o`
5. **Link**: `clang output.o gorget_runtime.o -lm -lpthread -o binary`

The inline functions will be available at link time (LTO) or can be converted to regular functions with a `-DGORGET_NO_INLINE` compile flag.

### Runtime function declarations in LLVM IR

Every `CallExtern` in LIR references a runtime function by name. The LLVM backend must emit `declare` statements for each:

```llvm
declare i64 @gorget_array_len(ptr)
declare void @gorget_array_push(ptr, ptr)
declare ptr @gorget_string_new(ptr)
; ... etc
```

These declarations are generated from `LirModule.externs`.

## Handling C-Backend-Specific LIR Features

### InlineC (13 remaining)
- 5 in for_loops.rs (dict/set key/value extraction)
- 8 in stmts/mod.rs (snapshot writes, assert formatting)

**LLVM strategy:** These are lowered to `Inst::InlineC` in LIR. The LLVM backend should:
- Skip InlineC instructions (they're C-only)
- The 5 key/value extractions already have equivalent `gorget_map_iter_key/value` runtime functions available
- The 8 snapshot/assert calls can be lowered to regular CallExtern at GIR level (separate prep task)

### Printf/Fprintf
Currently high-level instructions in LIR. **LLVM strategy:** Lower to `call @printf(ptr %fmt, ...)` with the same format string. The C backend's format-string fixing logic (replacing `%lld` with `%f` for floats) should move to a shared utility or be applied during LIR lowering.

### RuntimeCall globals
`LirGlobalInit::RuntimeCall(String)` holds a C expression for runtime-initialized globals. **LLVM strategy:** Emit a module constructor function (`@llvm.global_ctors`) that calls the runtime initialization functions and assigns globals.

### StrLit (String Literals)
`Inst::StrLit { dst, value }` materializes a string literal as a Str struct. **LLVM strategy:**
```llvm
@.str.0 = private unnamed_addr constant [6 x i8] c"hello\00"
; then in function body:
%str = insertvalue {ptr, i64, i64, ptr} undef, ptr @.str.0, 0
%str1 = insertvalue {ptr, i64, i64, ptr} %str, i64 5, 1
%str2 = insertvalue {ptr, i64, i64, ptr} %str1, i64 0, 2  ; cap=0 (view)
%str3 = insertvalue {ptr, i64, i64, ptr} %str2, ptr null, 3  ; alloc=null
```

### Overflow Checking
`Overflow::Trap` on Add/Sub/Mul. **LLVM strategy:** Use LLVM overflow intrinsics:
```llvm
%result = call {i64, i1} @llvm.sadd.with.overflow.i64(i64 %a, i64 %b)
%val = extractvalue {i64, i1} %result, 0
%overflow = extractvalue {i64, i1} %result, 1
br i1 %overflow, label %trap, label %continue
```

### BoundsCheck / DivCheck / Trap
Emit conditional branches to trap blocks. Use `@llvm.trap()` intrinsic or `call @abort()`.

## Implementation Phases

### Phase 1: Core Emission (~1,000 LOC)
**Goal:** Compile simple programs (arithmetic, control flow, function calls) to working binaries.

Create `src/backend/llvm/mod.rs`:
- `LlvmBackend` implementing `Backend` trait
- Type mapping: `LirType` -> LLVM IR type string
- Struct type definitions
- Function signatures and declarations
- Instruction emission for: IConst, FConst, BoolConst, NullPtr, Add, Sub, Mul, Div, Rem, Neg, BitAnd/Or/Xor/Not, Shl, Shr, Cmp, Not, IntCast, FloatCast, IntToFloat, FloatToInt, PtrCast, Bitcast, Load, Store, FieldPtr, ElemPtr, Memset, Memcpy, Call, CallExtern, CallPtr, BoundsCheck, DivCheck, Trap, Nop
- Terminator emission: Ret, RetVoid, Jump, Branch, Switch, Unreachable
- Block parameter -> phi node translation
- Slot -> alloca for non-promoted aggregates
- Extern function declarations from `LirModule.externs`
- Global variable emission (Zeroed, Bytes, FuncAddr, Struct inits)

### Phase 2: Runtime Integration (~500 LOC)
**Goal:** String operations, collections, print/println work.

- StrLit emission (global constant + struct aggregate)
- Printf/Fprintf lowering to libc printf call
- RuntimeCall globals via `@llvm.global_ctors`
- Runtime .o compilation and linking pipeline in main.rs
- String literal null-termination handling
- ABI marshalling for extern calls (CStr, BytePtr, GorgetString params)

### Phase 3: Full Feature Parity (~1,500 LOC)
**Goal:** All 890 integration tests pass on LLVM backend.

- Closure support (GorgetClosure struct, CallPtr emission)
- Enum/union layout emission
- Drop/clone function generation (recursive struct drops, enum drops)
- Test runner main() generation
- Spawn/thread wrapper generation
- Monomorphized type wrapper functions (Channel, Shared, Mutex, etc.)
- Higher-order collection helpers (map, filter, fold inline functions)
- Option/Result combinator expansion
- Hot-reload support (optional, defer if complex)

### Phase 4: Optimization & Polish
- LLVM optimization flags (-O0, -O1, -O2, -O3) pass-through
- Debug info emission (DWARF line numbers from LIR span data)
- LTO support (link-time optimization with runtime)
- Performance benchmarking vs C backend
- WASM target exploration (--target=wasm32-unknown-unknown)

## Key Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `src/backend/llvm/mod.rs` | CREATE | LLVM IR emitter, LlvmBackend struct |
| `src/backend/llvm/types.rs` | CREATE | LirType -> LLVM type mapping, struct layout |
| `src/backend/llvm/runtime.rs` | CREATE | Runtime .o compilation, extern declarations |
| `src/backend/mod.rs` | MODIFY | Register LlvmBackend, add --backend=llvm flag |
| `src/main.rs` | MODIFY | LLVM compilation pipeline (llc/clang invocation) |
| `src/lir/mod.rs` | MAYBE | Add field offset metadata to StructDef if needed |

## Shared Code Between C and LLVM Backends

~850 lines from the C backend can be extracted into `src/backend/shared.rs`:
- Type inference for polymorphic externs (`infer_inst_type`)
- Name parsing helpers (`parse_*_method`, `parse_option_result_combinator`)
- Struct traversal for drop/clone analysis
- Runtime function signature lookup

## Verification Strategy

1. **Phase 1:** Compile `tests/fixtures/alloc_keyword.gg` (minimal) through LLVM -> verify output matches C backend
2. **Phase 2:** Compile string-heavy fixtures (hello_world, string_ops) -> verify correct output
3. **Phase 3:** Run full `cargo test --test integration` with `--backend=llvm` flag
4. **Ongoing:** Compare LLVM binary output with C binary output for each fixture

## Prerequisites (Prep Tasks)

Before starting LLVM backend implementation:

1. **Move Printf format-fixing logic** from C backend to shared utility (used by both backends)
2. **Add `--backend` CLI flag** to main.rs (default "c-lir", accept "llvm")
3. **Ensure `llc` and `clang` are available** in the build environment
4. **Consider**: Convert 5 remaining for_loops InlineC to CallExtern using `gorget_map_iter_key/value` (requires fixing C backend MutPtr deref for output params — see TODO)
