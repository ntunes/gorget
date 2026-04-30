# LIR Correctness Roadmap

> Status: design draft (2026-04-30). Captures the architectural work
> identified by the LIR/BIR/backend audit, sequenced for long-term
> correctness with WASM as a near-term backend target.

## Framing

The LIR layer has been steadily lifting semantic decisions out of
backends ("LIR carries more, backends are dumber"). Today's work landed
the typed-IDs cluster (`SetCollectionBridge.key_struct: StructId`,
`TraitCall.{trait_obj_struct, method_idx}`) and several Tier-1
correctness fixes. What remains is bigger: instruction-shape changes,
runtime-API typing, and SSA invariants.

**Decision lens:** long-term correctness is the priority. Calendar cost
is a secondary consideration. Where two designs differ, prefer the one
that produces a more strongly-typed LIR — even if it means more refactor
work — over a sidecar/hint approach that defers the typing question.

**WASM as a forcing function:** several of these items go from
"nice-to-have" to "required" once WASM is on the menu, because WASM:

- Imports must be declared with typed signatures (forces a typed
  runtime-function representation).
- No opaque struct types in MVP (forces typed metadata for collection
  ctors so the backend knows byte layouts).
- Linear memory model (forces precise per-value type tracking — every
  load needs a width).
- Structured control flow only (forces reducibility / critical-edge
  splitting at LIR level).
- Function references via table indices (forces an abstract
  function-reference type, not raw pointers).

The items below are tagged **[wasm-forced]** when WASM elevates them
from refactor to requirement.

---

## Tier A — Get the LIR shape right

These touch instruction definitions; everything downstream gets easier.

### A1. Split `CallExtern` → `CallRuntime` + `CallExtern` **[wasm-forced]**

```rust
Inst::CallRuntime { dst, callee: RuntimeFn, args, arg_abis }
Inst::CallExtern  { dst, name: String, args, arg_abis }  // user externs only
```

Today, `CallExtern` does two unrelated jobs: invoke a finite, known set
of runtime functions (`gorget_dict_new`, `gorget_string_format`, etc.)
AND invoke user-declared C externs (`SDL_CreateWindow`,
`crypto_rsa_verify`, etc.). The dispatch boundary string-compares the
name to figure out which job is which. That's the source of every
`name.starts_with("gorget_dict_new")` site in the optimizer and
backends.

Once split:
- The validator type-checks `CallRuntime` arg counts/types against the
  `RuntimeFn`'s known signature.
- The optimizer can const-fold runtime calls (e.g., `RuntimeFn::ArrayLen`
  on a known-size array).
- Backends pattern-match the enum, no string compares.
- The WASM backend scans `CallRuntime` insts to determine which imports
  to declare — an O(N) pass instead of regex over names.

**Depends on:** A2 (the `RuntimeFn` enum itself).
**Unlocks:** A3 (cleaner CollectionCtor design — runtime ctors become a
specific `CallRuntime` shape), B1 (runtime declaration table can derive
the import set from `RuntimeFn` variants).

**Estimate:** 1 keystone commit + per-site migration. ~80 sites.

### A2. `RuntimeFn` enum + signature table **[wasm-forced]**

```rust
// src/lir/runtime.rs
pub enum RuntimeFn {
    ArrayNew, ArrayWithCapacity, ArrayPush, ArrayGet, ArrayClone, ArrayFree,
    DictNew, DictNewStr, DictPut, DictGet, DictRemove, DictFree,
    StringFromLiteral, StringFormat, StringClone, StringFree,
    /* ~80 variants */
}

impl RuntimeFn {
    pub const fn c_name(self) -> &'static str { /* match */ }
    pub fn from_c_name(s: &str) -> Option<Self> { /* match */ }
    pub const fn signature(self) -> &'static RuntimeSig { /* table */ }
}
```

Single source of truth for the runtime API at the IR level. Bidirectional
between the enum and the C symbol name. Const signature lookup means
the validator and optimizer get type info without string parsing.

WASM-specific: `signature()` produces the WASM import signature
(`(param i32 i32) (result i32)` etc.) directly.

**Estimate:** ~80 enum variants, mechanical. One commit.

### A3. New `Inst::CollectionCtor` variant **[wasm-forced]**

Replaces the `CallExtern` + `original_name` parsing pattern entirely:

```rust
Inst::CollectionCtor {
    dst: ValueId,
    kind: CollectionCtorKind,  // Vector | Dict | Set | HashMap | HashSet
    elem: ElemMeta,            // for Vector / Set
    key: Option<ElemMeta>,     // for Dict / HashMap
    val: Option<ElemMeta>,     // for Dict / HashMap
    capacity: Option<ValueId>, // present iff with_capacity
    str_keyed: bool,           // String-key fast path (gorget_*_new_str)
}

enum ElemMeta {
    Primitive(LirType),       // int64_t, double, bool, etc.
    Resource(ResourceKind),   // GorgetString, GorgetArray, GorgetMap, GorgetClosure
    UserStruct(StructId),
    UserEnum(StructId),
}
```

Today three passes string-parse `original_name` to recover this same
information: `wire_collection_bridges`, `find_hashable_key_types`,
`infer_collection_elem_fns`. After A3 they read structured fields. The
audit's #4 (drop `original_name` entirely) becomes a one-line deletion.

WASM-specific: `ElemMeta` lets the WASM backend compute the linear-memory
allocation size at lowering time without needing a runtime symbol-name
parser.

**Depends on:** A1 + A2 (CollectionCtor lowers to `CallRuntime` in BIR).
**Estimate:** 2 commits — variant + lowering, then consumer migration.

### A4. Origin metadata as per-value tags **[wasm-forced]**

Today both backends rebuild parallel arrays:
`str_lit_vals`, `null_vals`, `cstr_vals`, `func_addr_targets`,
`spawn_source_fn`, `ptr_pointee` — each backend's own logic, ~37
emit-decision sites total per backend.

Long-term-correct: attach origin info to the value at definition time,
not as a parallel bitmap. Either:
- Per-value flags on `LirFunction` (one shared array, replaces the
  five bitmaps), OR
- Encoded in instruction variants (`Inst::StrLit`, `Inst::NullPtr`,
  `Inst::FuncAddr` already exist — make them the *only* way to produce
  values with those origins).

WASM-specific: linear-memory loads need precise width (`i32.load8_u` vs
`i32.load`). Per-value type info MUST be authoritative — falling back
to "infer from context" doesn't work in WASM.

**Depends on:** Mostly independent of A1–A3, but easier after them.
**Estimate:** 2 commits — lift to LIR, then retire backend-side bitmaps.

### A5. Function references as a typed concept **[wasm-forced]**

Today `Inst::FuncAddr { dst, func: FuncId }` produces a `Ptr` value.
That works for C (any function pointer is `void(*)`) and LLVM (any
function pointer is `ptr`). It does NOT work for WASM, which uses
table indices, not pointers.

Long-term shape:

```rust
Inst::FuncAddr { dst, func: FuncId }      // dst: LirType::FuncRef
Inst::CallByRef { dst, fref: ValueId, args }  // call-via-table-or-pointer
```

Add `LirType::FuncRef`. Backends:
- C/LLVM: lower `FuncRef` to `void*` / `ptr`.
- WASM: lower `FuncRef` to a table index, `CallByRef` to `call_indirect`.

This is also the right place to stop conflating "raw function pointer"
with "boxed closure" — `GorgetClosure { fn_ptr: FuncRef, env: Ptr }`
becomes properly typed.

**Depends on:** None (independent).
**Estimate:** 1 commit — new LirType + FuncAddr return-type fix +
backend lowering.

---

## Tier B — Runtime as a typed boundary

### B1. Runtime declaration table **[wasm-forced]**

Today the C runtime is hand-written C in a giant string in
`src/backend/c/c_runtime.rs`. Function signatures are duplicated between:
- The C source (in c_runtime.rs)
- `runtime_extern_sig()` in `src/lir/lower/calls.rs` (Rust-side)

A change to a runtime function's signature requires updating both, with
no compile-time check. A mismatch is a silent miscompile.

Long-term-correct shape:

```rust
// src/lir/runtime/decls.rs — single source of truth
pub static RUNTIME_DECLS: &[RuntimeDecl] = &[
    RuntimeDecl {
        callee: RuntimeFn::ArrayNew,
        c_name: "gorget_array_new",
        params: &[CRuntimeType::Size],
        ret: CRuntimeType::GorgetArray,
        side_effects: SideEffects::Allocates,
    },
    /* ~80 entries */
];
```

From this table:
- `RuntimeFn::signature()` is `RUNTIME_DECLS[self as usize].sig`.
- The C backend emits `extern` declarations (or includes the runtime
  header that matches).
- The LLVM backend emits `declare`s.
- The WASM backend emits `(import "env" "name" (func ...))`.
- The C runtime header itself is auto-generated from the table, so the
  hand-written C and the Rust table can never diverge.

This is a multi-week refactor but it ends the entire class of
"runtime-vs-frontend signature mismatch" bugs.

**Depends on:** A2 (the enum is the table key).
**Unlocks:** WASM backend's import declaration, automated cross-language
signature checking.
**Estimate:** Multi-week. Probably 3-5 commits across runtime header
generation, table population, and consumer migration.

### B2. Eliminate the pre-mapping fallback path

Today the C backend has `emit_collection_constructor` (helpers.rs:794)
which fires on `CallExtern { name: "Dict__K__V" }` literal-name calls.
That path is a fallback for cases where the LIR didn't post-map the
ctor name to `gorget_dict_new`. It coexists with the post-mapping path,
making it a "in case we missed something" safety net.

Long-term-correct: ALL collection ctors have ONE canonical form,
`Inst::CollectionCtor` (post A3). The fallback path deletes.

**Depends on:** A3.
**Estimate:** 1 commit — delete `emit_collection_constructor` and
verify no LIR producer emits the pre-mapping form.

---

## Tier C — Drop & SSA correctness

### C1. Drop-flag init from dataflow

Today's fix (`d28b8f86`) seeds bb0 = false and instruments SlotStore.
That's correct but conservative. The dataflow already computes per-block
init states; the flag's initial value at each block could be seeded from
that state directly.

Long-term-correct: drop-flag instrumentation reads the dataflow's
out-state and sets the flag to match — no blanket false, no need to rely
on SlotStore to "fix" the flag at first use. Catches function-param
slots and other unconditionally-init cases without waiting for the
explicit param-SlotStore.

**Depends on:** Mostly independent.
**Estimate:** 1 commit + extended drop test fixtures.

### C2. Critical-edge splitting + post-SSA invariant validation **[wasm-forced]**

Today SSA construction (`src/lir/ssa.rs`) uses a simplified Braun et al.
algorithm that assumes no critical edges. There's no validator that
asserts:
- The CFG is reducible (no irreducible loops).
- Every value use is dominated by its definition.
- Critical edges (block with multiple successors → block with multiple
  predecessors) don't exist.

WASM has structured control flow only. If the LIR produces an
irreducible CFG, the WASM backend can't emit it without a relooper
pass — which is its own correctness hazard.

Long-term-correct: critical edges split at LIR construction time
(or by a dedicated pass before SSA). Post-SSA validator asserts
reducibility, dominance, and edge-set well-formedness. The dominance
check exists in debug builds (`ssa.rs:32-36`) but isn't called from
`validate_module`.

**Depends on:** None.
**Estimate:** 1 commit — split + validator extension.

### C3. Validator runs after every pass

Today `validate_module` runs once before SSA. After every pass —
optimizer, BIR lowering, drop elaboration — invariants can drift
silently. Long-term-correct: the test harness invokes `validate_module`
after each pass in debug builds. Cheap, catches every shape regression.

**Depends on:** None (incremental).
**Estimate:** 1 commit.

---

## Tier D — Optimizer becomes useful

### D1. Fixpoint instead of MAX_ITERS=3

`optimize_function` runs three iterations and stops, regardless of
whether it would have converged in four. Replace with snapshot-equality
fixpoint check. Already used elsewhere (SSA, drop elab).

**Estimate:** Trivial. One commit.

### D2. Cross-block constant propagation, GVN, LICM

Today the optimizer is intra-block CSE + intra-block constant folding.
A constant defined in `bb0` and used in `bb1` won't be propagated.
These are textbook passes; payoff scales with how much code we generate
that LLVM later folds anyway.

Lower priority: LLVM's `clang -O2` does most of this on the C backend's
output, and LLVM's own optimizer does it on the LLVM backend's output.
The LIR optimizer matters most for the WASM backend (where downstream
optimization is weaker).

**Depends on:** None.
**Estimate:** Multi-week per pass; deferrable until WASM ships.

---

## Sequencing

Dependency graph (→ means "must complete before"):

```
A2 (RuntimeFn enum) ─┬─→ A1 (CallRuntime split) ──┬─→ A3 (CollectionCtor)
                     │                            │
                     └─→ B1 (Runtime decl table)  └─→ B2 (kill pre-mapping fallback)

A4 (origin metadata)   — independent
A5 (FuncRef typing)    — independent
C1 (drop-flag from dataflow) — independent
C2 (SSA invariants)    — independent
C3 (validate per pass) — depends on validator extensions in A1/A3
D1 (fixpoint)          — trivial
```

**Recommended order if starting fresh:**

1. **A2 + A1** (RuntimeFn + CallRuntime split). Keystone. Foundation
   for everything else.
2. **A3** (CollectionCtor). Retires `original_name` entirely. Closes
   the audit's #4.
3. **B1** (Runtime declaration table). Becomes mandatory if A2's
   single-source-of-truth claim is to hold across the C/LLVM/WASM
   triple.
4. **A4** (origin metadata). WASM needs this; backends get simpler.
5. **A5** (FuncRef typing). Required before WASM backend starts.
6. **C2** (SSA invariants). WASM-required validation.
7. **C1** + **C3** (drop-flag refinement, validator-per-pass). Polish.
8. **D1** (fixpoint). Quick win whenever.

WASM backend implementation comes after step 6 at the earliest.

## Estimates summary

| Item | Type | Cost |
|------|------|------|
| A1 + A2 | Refactor | 1-2 weeks (combined) |
| A3 | Refactor | 1 week |
| A4 | Refactor | 1 week |
| A5 | New abstraction | 3-5 days |
| B1 | Major | 3-4 weeks |
| B2 | Cleanup | 1 day |
| C1 | Polish | 2-3 days |
| C2 | Validation | 3-5 days |
| C3 | Validation | 1-2 days |
| D1 | Trivial | 1 hour |
| D2 | Multi-major | Deferred |

Total before WASM-readiness: ~10 weeks of focused work, parallelizable
along independent lines. Significant, but each item delivers value
independently — the codebase gets more correct with each merge, not
only at the end.

## What this roadmap is NOT

- **A commitment.** It's the design view from one audit + WASM lens.
  Reality will surface new findings.
- **A blocker for shipping.** Today's compiler is correct enough to ship
  programs; this roadmap is about making it correct *by construction*
  instead of *by testing*.
- **A complete picture of WASM work.** The WASM backend itself —
  linear-memory layout, table management, growable memory, JS interop —
  is a separate plan. This roadmap only covers the LIR-side
  prerequisites.
