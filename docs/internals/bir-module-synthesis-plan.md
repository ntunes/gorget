# BIR Module-Level Synthesis (Option C) — Plan

**Status:** Design, 2026-04-20. Not yet implemented.
**Context:** Extension of `docs/internals/lir-backend-lift-plan.md` Step 8.
Replaces the `sort_by` / `sorted_by` / `sort_by_key` / `sorted_by_key`
TLS-trampoline at `src/backend/c_lir/emit_types.rs:180-260` with a
module-level BIR synthesis pass that emits one `__gg_synth_*` function
per (op, element-type, closure-sig) tuple.

## TL;DR

`lower_lir_to_bir` grows a second responsibility: besides expanding
canonical ops in-place inside a function, it can **emit new function
definitions** into the module tail and rewrite matching `HofExpand`
call sites into ordinary `Call` instructions. The synthesized body is
primitive LIR (branches, `ElemPtr`, `Load`/`Store`, `CallClosure`, `Cmp`)
— no TLS, no libc `qsort`, no per-backend helper.

The sort family is the proof-of-concept. After this step, backends
contain **zero** sort-specific codegen.

## Scope

**In scope for the PoC migration:**
- `HofOp::SortBy` (in-place sort)
- `HofOp::SortedBy` (clone + sort)
- `HofOp::SortByKey` (in-place sort by key extractor)
- `HofOp::SortedByKey` (clone + sort by key extractor)

**Out of scope (separate work):**
- Self-host LIR backend (`tests/fixtures/self_host_*/lir_codegen.gg`)
  — its backend doesn't go through BIR today; catches up in a later step.
- Quicksort. First cut uses in-place insertion sort (simple, stable,
  no recursion needed). Upgrading to a non-recursive heapsort or
  introsort is a follow-up.
- Other synthesis candidates (e.g. future `Windows`/`Chunks`
  migrations). The infrastructure is reusable but no new op migrates
  in this plan.

## Answers to the Ten Architectural Questions

### 1. Function-synthesis API

**Decision:** by-value in, by-value out. `fn lower_lir_to_bir(module: LirModule) -> Result<LirModule, BirError>` keeps its signature. Inside,
the pass owns a `SynthPool` that appends new `LirFunction`s to the
module before returning it.

```rust
// src/bir/synth.rs (new)
pub(super) struct SynthPool<'m> {
    structs: &'m [StructDef],
    /// (key → FuncId) — once assigned, reused for every call site with
    /// the same key.
    cache: FxHashMap<SynthKey, FuncId>,
    /// New functions to append; live here until the caller splices them
    /// back into `module.functions` at the end of the pass.
    new_fns: Vec<LirFunction>,
}
```

Rejected alternatives:
- `&mut LirModule` signature — breaks the "module in, validated module
  out" contract and complicates call-site rewriting because functions
  and instructions would be mutated concurrently.
- Side-channel `Vec<LirFunction>` returned separately — introduces a
  second stage where the caller must remember to splice. The pass
  already owns the module; one place less to forget.

### 2. De-duplication

**Decision:** key by `(HofOp, mangled(element_ty), closure_call_fn_name)`.

```rust
#[derive(Hash, Eq, PartialEq)]
struct SynthKey {
    op: HofOp,          // SortBy | SortedBy | SortByKey | SortedByKey
    element_ty: String, // mangled via display::mangle_lir_type
    closure_call: String, // __Closure_7__call or a FuncRef target name
}
```

Why these three:
- `HofOp` — different expansions per op (clone-first vs in-place, key
  vs direct compare).
- `element_ty` — `ElemPtr` size and `Load` type differ per T.
- `closure_call` — the `CallClosure` target in the body must match the
  caller's closure dispatch. Two call sites with different closure types
  get different bodies because `param_tys` / `ret_ty` feed ABI
  decisions.

Rejected:
- Key by closure **struct type** alone — same struct, different
  element types still need distinct bodies.
- Key by mangled full site (caller fn + site) — no dedup, blows up
  module size at 20 call sites.

### 3. Naming & namespace

**Decision:** prefix `__gg_synth_`. Full shape:

```
__gg_synth_<op>_<elem_mangle>_<closure_mangle>
```

Examples:
- `__gg_synth_sort_by_i64___Closure_7__call`
- `__gg_synth_sorted_by_Str___Closure_12__call`
- `__gg_synth_sort_by_key_Struct_s14___Closure_3__call`

Reserved: monomorphization produces names starting with user-defined
identifiers or `Vector__` / `Dict__` / `__Closure_` / `__gorget_` /
runtime-stub names. `__gg_synth_` collides with none of them. The
synthesis pass asserts no module function already starts with the
prefix at entry (defensive).

### 4. Optimization ordering

**Current pipeline** (from `src/main.rs:540-565`):

```
LIR lower → SSA → optimize → compute_value_types → BIR → backend
```

**Post-Option-C pipeline:**

```
LIR lower → SSA → optimize → compute_value_types → BIR synthesis → compute_value_types (delta) → backend
```

**Synthesis-pass position:** *after* optimization. Reasons:
1. Synthesized bodies are hand-emitted in already-good shape — no
   dead code, no redundant copies. Skipping optimize on them costs
   nothing.
2. Running optimize *before* synthesis means user code has been DCE'd
   and copy-propped; BIR sees the canonical post-opt `HofExpand`s and
   generates one synth fn per surviving call site, not one per dead
   call site.
3. `compute_module_value_types` needs re-running **only for the new
   synthesized functions** — user functions' `value_types` is already
   populated. The synthesis pass calls `compute_function_value_types`
   (new helper, extracted from the module-level pass) on each emitted
   function at append time.

Rejected: re-running the full optimizer post-BIR. Correctness isn't
affected, but it's ~2× compile time for zero payoff — synthesized
bodies are deterministic and already minimal.

### 5. `ClosureCallSig` visibility inside synthesis

**Decision:** the synthesis pass reconstructs per-closure signatures
from `LirModule.functions` at pass start. No new module-level state.

At BIR time, every `__Closure_N__call` and every user function that
can serve as a `FuncRef` target already exists in `module.functions`
with its declared `params` and `return_type`. The synthesis pass
walks them once and builds:

```rust
FxHashMap<String /*fn name*/, (Vec<LirType>, LirType) /*(params, ret)*/>
```

Semantically identical to `LoweringContext.closure_call_sigs` but
derived from LIR, not plumbed through. The two sources must agree;
they already do because GIR→LIR lowering just copies the declared
types into `LirFunction.params` / `return_type`.

Rejected: threading `closure_call_sigs` from `LoweringContext` onto
`LirModule`. Adds a new field with a one-pass lifetime. Re-deriving
is O(n) and keeps the data next to its source of truth.

### 6. Monomorphization correctness

`HofExpand.element_ty` is already a **concrete** `LirType` when the
op is emitted. Proof chain:

- `try_emit_vector_each_hof` (src/lir/lower/insts.rs:2485) reads
  `elem_c_name` from the monomorphized GIR name
  (`Vector__int64_t__sort_by` → `"int64_t"`).
- `component_to_lir_type` maps `"int64_t"` → `LirType::I64`.
- Generic placeholders never reach this code path: monomorphization
  runs at GIR stage, and only after all placeholders are substituted
  does GIR→LIR run. A HofExpand with a generic `element_ty` is not
  reachable.

Synthesis therefore sees concrete `LirType` and can unconditionally
compute `elem_size` via `c_sizeof_lir_type`. No fallbacks, no
conditional generic handling.

### 7. Drop elaboration

Drop elaboration runs during GIR→LIR (`src/lir/drop_elab.rs`). The
synthesized function is created *after* that pass, so it does not
participate in drop elaboration directly. This is fine because:

- The synthesized function has two parameters: `arr: Ptr` (or
  `arr: Ptr, cl: Ptr` for cmp variants) and a closure `Ptr`. Both
  are borrows; the function does not own them.
- The in-place sort variants do no allocation, no owned moves
  through scopes — only `Load`/`Store` into element slots the caller
  already owns.
- The clone-then-sort variants emit one `CallExtern { name:
  "gorget_array_clone" }` that returns the cloned `GorgetArray` as the
  function's return value. Ownership transfer happens at the boundary
  (return from synth fn → caller slot), exactly as existing
  collection-returning runtime calls already work — and they go
  through `compute_module_value_types` + the existing aggregate-return
  ABI.
- The closure is invoked via `Inst::CallClosure`, which is a primitive
  op. No drop side-effects at call boundaries.

Conclusion: no drop-elaboration re-run needed. The synthesis pass
produces functions whose drop shape is structurally identical to the
existing BIR scaffolds (`emit_hof_loop_scaffold` & friends), which
already skip drop-elab.

### 8. Self-host scope

The self-host LIR backend in `tests/fixtures/self_host_*/lir_codegen.gg`
currently does **not** route through BIR. The self-host-bootstrap work
at `project_selfhost_bootstrap_plan.md` is on a separate track and will
align with BIR module synthesis in its own commit sequence.

For this plan: **self-host is out of scope.** After Option C lands, the
self-host comparison tests may show additional mismatches on sort_by
fixtures; acceptable — they will be addressed by the self-host track,
not here.

### 9. BIR validator & synthesized functions

`assert_primitives_only` (src/bir/validate.rs:36) walks **every**
function in `module.functions`. Synthesized functions live in the same
vector, so they're covered automatically. No allow-list change needed.

Synthesized bodies contain only validator-approved primitives:
`IConst`, `FieldPtr`, `ElemPtr`, `Load`, `Store`, `Add`, `Sub`, `Cmp`,
`CallClosure`, `CallExtern`, `Jump`, `Branch`, `Return`. None of the
canonical high-level ops appear. If a future synth ever emits a
canonical op (e.g. `SizeOf` for a variable-length element), BIR's own
expansion handles it naturally — synthesis runs before the validator
walk, not after.

### 10. Atomic vs. incremental rollout

**Decision:** incremental across 5 commits. The rollout mirrors the
parent plan's discipline: each commit removes code and demonstrates
payoff.

Rejected: one atomic commit. Too large to review, and a single
regression on any one variant blocks the entire cleanup.

Rejected: feature flag / dual code paths. The flag would live for one
commit before being deleted; carrying it isn't worth the review
surface.

## Worked Example

### Input

```gg
Vector[int] v = [3, 1, 4, 1, 5, 9, 2, 6]
v.sort_by(|int a, int b| a - b)
```

The closure mangles to `__Closure_7`; `__Closure_7__call(ptr env, int64_t a, int64_t b) -> int64_t`.

### LIR today — what `try_emit_vector_each_hof` produces

```text
; in user function fn.main
bb3:
    ; ... v setup ...
    v6 = ...                                              ; &GorgetArray
    v8 = SlotAddr s2                                      ; closure __Closure_7 (captures empty)
    ; wrap_closure_call_args: wrap bare __Closure_7 in a GorgetClosure
    v9 = /* pack of fn_ptr + env into GorgetClosure slot */
    HofExpand {
        coll:             v6,
        hof_op:           SortBy,
        element_ty:       I64,
        value_ty:         None,
        closure:          v9,
        closure_kind:     EscapedClosure,
        closure_ret_ty:   I64,
        closure_arg_abis: [Scalar, Scalar],
        dst:              None,
        init:             None,
    }
    Jump bb4
```

(None of this exists today for sort_by — it currently flows through
`CallExtern` to the TLS trampoline. Step 2 of the migration wires
`try_emit_vector_each_hof` to emit the above.)

### LIR after synthesis — rewritten call site

```text
bb3:
    v6 = ...
    v8 = SlotAddr s2
    v9 = /* pack */
    Call { func: fn.__gg_synth_sort_by_i64___Closure_7__call,
           args: [v6, v9], dst: None }
    Jump bb4
```

### Synthesized function body (insertion sort)

Nine blocks. Full LIR:

```text
fn.__gg_synth_sort_by_i64___Closure_7__call(arr: Ptr, cl: Ptr) -> Void:

bb0 (entry):
    c1      = IConst I64 1
    Jump bb1(c1)

bb1 (outer_check) params: (i: I64):
    lenp    = FieldPtr arr, GorgetArray, 2
    len     = Load lenp, I64
    cond    = Cmp Lt, i, len
    Branch cond, bb2, bb8

bb2 (outer_body):
    datap   = FieldPtr arr, GorgetArray, 0
    data    = Load datap, Ptr
    tmpp    = ElemPtr data, i, 8
    tmp     = Load tmpp, I64
    c1b     = IConst I64 1
    j_init  = Sub I64, i, c1b, Wrap
    Jump bb3(j_init)

bb3 (inner_check) params: (j: I64):
    zero    = IConst I64 0
    ge      = Cmp Ge, j, zero
    Branch ge, bb4, bb7(j)

bb4 (inner_compare):
    datap2  = FieldPtr arr, GorgetArray, 0
    data2   = Load datap2, Ptr
    jp      = ElemPtr data2, j, 8
    jval    = Load jp, I64
    cmp_r   = CallClosure cl, [jval, tmp],
                          kind=EscapedClosure,
                          arg_abis=[Scalar, Scalar],
                          ret_ty=I64
    zero2   = IConst I64 0
    gt      = Cmp Gt, cmp_r, zero2
    Branch gt, bb5, bb7(j)

bb5 (shift):
    c1c     = IConst I64 1
    jplus1  = Add I64, j, c1c, Wrap
    datap3  = FieldPtr arr, GorgetArray, 0
    data3   = Load datap3, Ptr
    dst     = ElemPtr data3, jplus1, 8
    Store dst, jval
    new_j   = Sub I64, j, c1c, Wrap
    Jump bb3(new_j)

bb7 (inner_done) params: (jf: I64):
    c1d     = IConst I64 1
    jfplus1 = Add I64, jf, c1d, Wrap
    datap4  = FieldPtr arr, GorgetArray, 0
    data4   = Load datap4, Ptr
    dst2    = ElemPtr data4, jfplus1, 8
    Store dst2, tmp
    next_i  = Add I64, i, c1d, Wrap
    Jump bb1(next_i)

bb8 (done):
    Return
```

Primitives only. Validator passes.

### Variant shapes

- **SortedBy** — same as SortBy, but `bb0` starts with
  `ret = CallExtern "gorget_array_clone"(arr)` returning a
  `GorgetArray`; the sort loop operates on a pointer to `ret`, and
  `bb8` returns `ret`. Fn signature becomes `(arr: Ptr) -> Struct(GorgetArray)`.
  (Note the synthesized fn **takes** a pointer but **returns** by value,
  exactly like existing `gorget_array_clone`.)
- **SortByKey** — `bb4` calls `cl(arr[j])` and `cl(tmp)` to extract
  keys, then compares the two keys via the same `Cmp Gt` using a
  type-appropriate compare (Str goes through `gorget_str_cmp` as
  `CallExtern`; integers and floats use `Cmp`). The key type comes from
  the closure's return type (`closure_ret_ty` on `HofExpand`). Key is
  cached per outer-loop iteration, not re-extracted per inner-loop
  compare, so `tmp_key` sits in a block param alongside `tmp`.
- **SortedByKey** — clone-then-sort-by-key combo.

## Migration Sequence

Each step is one commit. Integration target: 1013/1013 after every
commit.

### Commit 1 — Infrastructure (zero behavior change)

New file: `src/bir/synth.rs`.

- `SynthPool` struct with `cache`, `new_fns`, `structs` refs.
- `SynthKey` enum.
- `SynthPool::get_or_emit_sort_by(&mut self, element_ty, closure_call_name, closure_sig) -> FuncId`
  — dispatched off `HofOp`; returns cached FuncId if seen, else builds
  the body (currently only the SortBy shape is implemented; SortedBy/
  SortByKey/SortedByKey panic with `todo!`).
- `SynthPool::finish(self) -> Vec<LirFunction>` — returns appended fns.
- `lower_lir_to_bir` constructs a `SynthPool`, passes it through
  `expand_func`, and appends `pool.finish()` to `module.functions` at
  the end. Re-runs `compute_function_value_types` on each appended fn.
- Assert on entry: `assert!(module.functions.iter().all(|f| !f.name.starts_with("__gg_synth_")))`.

`expand_func` still handles `HofOp::SortBy` via the existing
fall-through (keep-as-is) because no call site emits it yet.

**Payoff:** module-level synthesis framework exists. Backends untouched. Integration still 1013/1013.

**LOC:** +~300 in `src/bir/synth.rs`, +~40 in `src/bir/lower.rs`, +0 backends.

### Commit 2 — SortBy end-to-end

- `try_emit_vector_each_hof` grows `"sort_by" => (HofOp::SortBy, …)`.
- `expand_func`'s HofExpand match-arm routes SortBy to
  `pool.get_or_emit_sort_by(...)`, then replaces the HofExpand with
  `Inst::Call { func: fid, args: [coll, closure], dst: None }`.
- `SynthPool::emit_sort_by_body` produces the insertion-sort LIR shown
  above.
- Backend: `HIGHER_ORDER_METHODS` loses `"sort_by"`; `emit_vector_helper`'s `sort_by` arm deleted.

**Payoff:** `sort_by` is TLS-free. `emit_vector_helper` shrinks.

**LOC:** +~120 (synth body), -~30 (TLS arm), +~10 (emit hookup).

### Commit 3 — SortedBy

- Add `"sorted_by" => (HofOp::SortedBy, …)` to the emit switch.
- `SynthPool::emit_sorted_by_body` — clone the source array first,
  sort the clone in place, return the clone.
- `"sorted_by"` arm deleted from `emit_vector_helper`.

**Payoff:** the second half of `sort_by` / `sorted_by` pair is
TLS-free.

**LOC:** +~50 (reuses the SortBy body via an `in_place: bool` flag on
the emitter), -~30 (TLS arm).

### Commit 4 — SortByKey + SortedByKey

- Emit switch learns both names.
- `SynthPool::emit_sort_by_key_body` — extracts keys via one
  `CallClosure` per compare (cached at outer-loop boundary), dispatches
  on key-type to pick `Cmp` vs `CallExtern "gorget_str_cmp"`.
  `sorted_by_key` layers the clone-first shape on top.
- Both `sort_by_key` / `sorted_by_key` arms deleted from
  `emit_vector_helper`.

**Payoff:** all four sort variants are TLS-free.

**LOC:** +~80 (key extraction + key compare switch), -~60 (both TLS
arms and the key-compare switch that exists today for the TLS
trampoline).

### Commit 5 — Dead-code sweep

- `HIGHER_ORDER_METHODS` const deleted (now empty).
- `parse_vector_higher_order` deleted.
- `CollHelper::Vector` variant deleted.
- `emit_vector_helper` deleted.
- The `CollHelper` enum loses its `Vector` variant; the dispatch in
  `emit_higher_order_collection_helpers` falls back to Dict/Set only.
  When both of those are also empty (they already are — see Step 8
  migration status in the parent plan), the whole function becomes
  deletable in a follow-up.
- Update the Vector row of the Migration-status table in
  `docs/internals/lir-backend-lift-plan.md` (delete "Vector still in
  backends" entry).

**Payoff:** backend contains zero sort-family code.

**LOC:** -~200 (the whole sort_by family inline-C generator).

## Acceptance Criteria

Option C is done when **all** of the following hold:

- Integration tests pass at 1013/1013 (or higher — baseline is the
  number on trunk at merge time).
- `src/backend/c_lir/emit_types.rs` contains **no** references to
  `qsort`, `sort_by`, `sorted_by`, `sort_by_key`, `sorted_by_key`,
  `__tls_`, or `__cmp_`.
- `rg -n 'sort_by' src/backend/` returns **zero** matches outside
  comments.
- `src/backend/llvm/mod.rs` sort-family arms deleted.
- `docs/internals/lir-backend-lift-plan.md` Step 8 migration table:
  "Vector still in backends (qsort TLS trampoline)" row is gone.
- `__gg_synth_` prefix appears in at least one integration-test
  fixture's generated C (spot-check), confirming the synth path is
  exercised in CI.
- BIR validator still catches stray canonical ops — `cargo test --lib
  bir::` passes, including a new test that feeds a module with a
  `HofExpand { SortBy }` through `from_lir` and asserts the rewritten
  module contains a `Call` to a `__gg_synth_*` function and no
  `HofExpand`.

## Open Questions

- **Insertion vs. heapsort.** Insertion sort is O(n²). For the large
  array fixtures (a few hundred elements), that's fine. If a
  benchmark regresses noticeably, upgrade to iterative heapsort —
  still primitive LIR, no recursion, stable memory behavior. Flagged
  for follow-up, not a blocker.
- **Stable sort contract.** Today's qsort is unstable; insertion sort
  is stable. Existing fixtures don't test stability, so the change is
  invisible. If a user relies on the previous unstability (unlikely),
  they'll be pleasantly surprised. No plan change.
- **Recursive synth functions.** If a synthesized function ever wants
  to call itself (e.g. a recursive quicksort body), the synthesis
  pass needs to insert the self-`FuncId` into the fresh body before
  `pool.new_fns` is committed. Insertion sort avoids this. If the
  heapsort upgrade also avoids recursion (it can), we never need it.
  If a future synth does need self-recursion, the infrastructure
  extends by reserving the `FuncId` *before* emitting the body.
- **SortedBy return ABI.** The synthesized fn returns `Struct(GorgetArray)` by value. Both C and LLVM backends already handle
  aggregate-return via the existing `needs_sret` machinery for
  `gorget_array_clone` — the synth fn should be covered by the same
  mechanism. If it isn't (e.g. LLVM backend decides sret based on
  function-name patterns rather than return type), the Step 1 of the
  parent plan's "trust `LirExtern.return_type` for sret" pre-requisite
  must land first. Verify at Commit 3.
- **Integration with Step 9 (`AddressOf`).** Today the synth body
  `Load`s element values directly. If an element type is an aggregate
  (struct) and the closure takes it by pointer, the body needs a
  pointer into the array, not a loaded value. The existing
  `emit_hof_loop_scaffold` handles this via `pass_by_ptr`; the synth
  emitter adopts the same pattern. Concrete form: the `elem_arg` the
  body passes to `CallClosure` is `ElemPtr` directly if
  `closure_arg_abis[0] == Ptr`, else a `Load`. Already decided — no
  new `AddressOf` call needed because `ElemPtr` is already a pointer.
- **Hidden dependency on `HofExpand.closure_kind`.** The synth body
  uses `ClosureDispatchKind::EscapedClosure` in its `CallClosure` —
  same as the emitter passed. `CallableParam` shaped closures
  (void*[2]) don't reach this path because
  `try_emit_vector_each_hof` rejects them. If `CallableParam`
  support is added later, synth needs a parallel body. Deferred.

## Files Touched (summary)

| File | Change | Commit |
|---|---|---|
| `src/bir/synth.rs` | new | 1 |
| `src/bir/lower.rs` | append pool, route SortBy | 1–4 |
| `src/lir/lower/insts.rs` | emit `HofOp::Sort*` from `try_emit_vector_each_hof` | 2–4 |
| `src/backend/c_lir/emit_types.rs` | delete TLS arms, `HIGHER_ORDER_METHODS`, `parse_vector_higher_order`, `emit_vector_helper` | 2, 3, 4, 5 |
| `src/backend/llvm/mod.rs` | delete sort-family LLVM inliner arms | 2, 3, 4, 5 |
| `docs/internals/lir-backend-lift-plan.md` | update Step 8 migration status table | 5 |

Expected net LOC delta: roughly `+500` synth (well-documented,
structured, primitive-LIR heavy) minus `~600` backend TLS + LLVM
inliner + dispatch glue. Net `-100` LOC with a **much** cleaner
boundary — one code path per op instead of two per backend.
