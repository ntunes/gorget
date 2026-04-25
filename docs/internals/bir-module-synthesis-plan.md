# BIR Module-Level Synthesis (Option C) — Plan

**Status:** Landed (2026-04-24). Design written 2026-04-20 (revised same day
after alignment with `docs/internals/stdlib-design.md`).

Commits 1–3 are in: `SynthPool` infrastructure, SortBy / SortedBy end-to-end,
and the SortByKey / SortedByKey pair. Commit 4 (dead-code sweep of
`HIGHER_ORDER_METHODS` / `parse_vector_higher_order` / `emit_vector_helper` and
the Dict/Set inline-HOF fallback blocks that went unreachable once HofExpand
covered them) completed in the 2026-04-24 housekeeping pass.

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
— no TLS, no libc `qsort`, no per-backend helper. Sort uses **iterative
bottom-up mergesort** for stable O(n log n) worst-case performance.

The sort family is the proof-of-concept. After this step, backends
contain **zero** sort-specific codegen.

## Relationship to the Iterator Protocol

This plan and `docs/internals/stdlib-design.md` (gorget-3's track) are
complementary, not overlapping. The division of labor:

| Path | Handles | Mechanism |
|---|---|---|
| **Iterator protocol** (stdlib-design §4) | chained lazy HOFs: `v.iter().filter(p).map(f).fold(z, g)` | Concrete state-machine state structs (`Map[Filter[VectorIter[T], P], F]`) monomorphized per call site. LLVM inlines `next()` through the chain into one loop. Zero intermediate allocations. |
| **BIR `HofExpand` synthesis** (this plan) | eager single-op calls: `v.sort_by(cmp)`, `v.each(f)`, `v.map(f)` as a convenience wrapper | Compiler synthesizes one primitive-LIR function per `(op, element_ty, closure_sig)`; rewrites call sites to `Call`. |

**Cross-HOF fusion is the Iterator protocol's job, not HofExpand's.**
Once stdlib-design Phase 2c lands, the eager Vector/Dict/Set wrappers
(`v.filter(p)`, `v.map(f)`, …) become thin shells over
`iter().method().collect()`, and their `HofExpand` variants become
unreachable. Deleting them is a future cleanup driven by gorget-3's
timeline, not by this plan.

What stays on the BIR-synthesis path long-term:

- **Eager in-place mutations** that don't fit the pull-based `next()`
  protocol: `sort_by`, `sort_by_key`, `reverse`, `retain`, `fill`,
  `swap`.
- **Eager cloning variants** of those: `sorted_by`, `sorted_by_key`,
  `reversed`.
- **Terminal side-effect ops with no equivalent Iterator shape:**
  `each` for the rare case where `for x in v:` isn't wanted.

Sort is the canonical representative — stateful, in-place, algorithm
choice belongs in one place, doesn't compose into an Iterator chain.

## Scope

**In scope for the PoC migration:**
- `HofOp::SortBy` (in-place sort)
- `HofOp::SortedBy` (clone + sort)
- `HofOp::SortByKey` (in-place sort by key extractor)
- `HofOp::SortedByKey` (clone + sort by key extractor)

**Out of scope (separate work):**
- Self-host LIR backend (`tests/fixtures/self_host_*/lir_codegen.gg`)
  — its backend doesn't go through BIR today; catches up in a later step.
- Other synthesis candidates (e.g. future `Windows`/`Chunks`
  migrations). The infrastructure is reusable but no new op migrates
  in this plan.
- Iterator-protocol work (gorget-3's stdlib-design.md Phase 2).

## Sort Algorithm Choice

**Iterative bottom-up mergesort.** Stable, O(n log n) worst case, no
recursion, one aux buffer allocation per call.

Rejected alternatives:

| Algorithm | Verdict |
|---|---|
| Insertion sort | O(n²) — unacceptable at scale. |
| Heapsort | O(n log n) and in-place, but unstable — breaks Rust/Python/Java convention for `sort_by`. Reserved for a future `sort_unstable_by` op if anyone needs zero-alloc sort. |
| Recursive quicksort / introsort | Recursion requires either a synthesized-fn self-call (adds emit-order complexity) or an explicit stack. Not worth it for the first cut. |

Mergesort's aux buffer is allocated via `gorget_array_new_like(src)`
at the top of the synth body and freed at exit. The O(n) extra memory
per call is dwarfed by the O(n log n) compares; `sorted_by` already
allocates a full clone, so for that variant the aux buffer is free.

## The Opaque-Closure Invariant

Synthesized bodies dispatch closures exclusively via `Inst::CallClosure`
with the `EscapedClosure` kind. They never inspect the closure env, never
read `__Closure_N` struct fields, never depend on closure layout. This is
an invariant, not a current-state observation:

> **Rule:** Synthesized function bodies treat the closure parameter as
> an opaque `Ptr` and communicate with it only through `CallClosure`.
>
> **Why:** De-duplication keys by closure `__call` function name alone
> (see Question 2 below). Two captures of the same `__Closure_N` type
> with different env values share one synth fn and pass different
> closure pointers at runtime. This is only correct if the synth body
> never depends on env layout.
>
> **Enforcement:** unit test in `src/bir/synth.rs` walks every emitted
> synth body and asserts no `FieldPtr { base: closure_param, .. }`
> appears. Debug-assert in the emitter.

## Answers to the Ten Architectural Questions

### 1. Function-synthesis API

**Decision:** by-value in, by-value out. `fn lower_lir_to_bir(module: LirModule) -> Result<LirModule, BirError>` keeps its signature. Inside,
the pass owns a `SynthPool` that appends new `LirFunction`s to the
module before returning it.

```rust
// src/bir/synth.rs (new)
pub(super) struct SynthPool<'m> {
    structs: &'m [StructDef],
    /// Indexed lookup of existing module functions — needed to resolve
    /// closure signatures (see Question 5). Built once at pass entry.
    fn_sigs: FxHashMap<String, (Vec<LirType>, LirType)>,
    /// (key → FuncId) — once assigned, reused for every call site with
    /// the same key.
    cache: FxHashMap<SynthKey, FuncId>,
    /// New functions to append; live here until the caller splices them
    /// back into `module.functions` at the end of the pass.
    new_fns: Vec<LirFunction>,
}
```

Rejected alternatives: `&mut LirModule` breaks the "module in, validated
module out" contract; side-channel `Vec<LirFunction>` forces the caller
to remember a splice step.

### 2. De-duplication

**Decision:** key by `(HofOp, mangled(element_ty), closure_call_fn_name)`.

```rust
#[derive(Hash, Eq, PartialEq)]
struct SynthKey {
    op: HofOp,          // SortBy | SortByKey (Sorted* variants share impls — see below)
    element_ty: String, // mangled via display::mangle_lir_type
    closure_call: String, // __Closure_7__call or a FuncRef target name
}
```

Why these three:
- `HofOp` — different expansions per op (direct compare vs. key extract).
- `element_ty` — `ElemPtr` size and `Load` type differ per T.
- `closure_call` — feeds ABI decisions inside the body (param ABIs,
  return type).

The opaque-closure invariant is what makes this key safe: two call
sites with the same closure type but different captures pass distinct
closure pointers at runtime, and the body only dispatches through those
pointers.

**Shared impl between pairs:** `sort_by` and `sorted_by` share one
synthesized function (`__gg_synth_sort_impl_*`); the `sorted_by` call
site inlines `clone → Call sort_impl → return clone`. Same for the key
variants. Result: 2 synth fns per `(elem_ty, closure)` pair, not 4.

### 3. Naming & namespace

**Decision:** prefix `__gg_synth_`. Full shape:

```
__gg_synth_<op>_<elem_mangle>_<closure_mangle>
```

Examples:
- `__gg_synth_sort_by_i64___Closure_7__call`
- `__gg_synth_sort_by_key_Str___Closure_12__call`

Reserved: monomorphization produces names starting with user-defined
identifiers, `Vector__`, `Dict__`, `__Closure_`, `__gorget_`, or
runtime-stub names. `__gg_synth_` collides with none. The synthesis
pass asserts no module function already starts with the prefix at
entry (defensive).

### 4. Optimization ordering

**Decision:** optimize runs **after** BIR synthesis, not before.

**Revised pipeline:**

```
LIR lower → SSA → compute_value_types → BIR synthesis → optimize → backend
```

Reasons:

1. Synthesized mergesort bodies benefit materially from LLVM-level
   optimization passes (mem2reg, copy-prop, constant folding) once
   they're primitive LIR.
2. DCE after synthesis drops orphan synth fns (no caller after
   upstream DCE).
3. Inliner can inline trivial closures (`|a,b| a-b`) into the synth
   body, collapsing `CallClosure` to a direct compare.
4. **No loss on the fusion front**, because fusion is the Iterator
   protocol's job and happens at a completely different layer
   (monomorphization + LLVM inlining of iterator state machines).

The rule we're setting: *HOF-shape-aware passes run pre-synthesis on
`HofExpand`. Primitive-LIR passes run post-synthesis on everything.*
No HOF-shape-aware passes are planned today; the slot exists if we
ever need one.

`compute_module_value_types` runs pre-synthesis on user functions and
again, incrementally, on synth-emitted functions at the end of the
synthesis pass.

### 5. `ClosureCallSig` visibility inside synthesis

**Decision:** the synthesis pass builds a `FxHashMap<String, (Vec<LirType>, LirType)>` of function signatures from
`LirModule.functions` at pass entry, via a helper:

```rust
fn closure_call_sig_of(module: &LirModule, name: &str)
    -> (Vec<LirType>, LirType)
{
    let f = module.functions.iter().find(|f| f.name == name)
        .expect("synth pass found HofExpand with unknown closure call");
    let skip = if name.starts_with("__Closure_") && name.ends_with("__call") {
        1  // hide the env param
    } else {
        0
    };
    (
        f.params.iter().skip(skip).cloned().collect(),
        f.return_type.clone(),
    )
}
```

Semantically identical to `LoweringContext.closure_call_sigs`, derived
from LIR at BIR time rather than plumbed through. One source of truth:
the actual `LirFunction`.

### 6. Monomorphization correctness

`HofExpand.element_ty` is a **concrete** `LirType` when the op is
emitted. `try_emit_vector_each_hof` (src/lir/lower/insts.rs:2485) reads
`elem_c_name` from the monomorphized GIR name
(`Vector__int64_t__sort_by` → `"int64_t"`), and
`component_to_lir_type` maps that to `LirType::I64`. Generic
placeholders are substituted at GIR stage before GIR→LIR runs; a
HofExpand with a generic `element_ty` is not reachable.

Synthesis therefore sees concrete `LirType` and unconditionally
computes `elem_size` via `c_sizeof_lir_type`. No fallbacks.

### 7. Drop elaboration

Drop elaboration runs during GIR→LIR (`src/lir/drop_elab.rs`). The
synthesized function is created *after* that pass; it doesn't
participate. This is fine because:

- Synth params are borrows: `arr: Ptr`, `closure: Ptr`. No ownership
  transfer into the fn.
- Mergesort uses `gorget_array_new_like` + `gorget_array_free` for the
  aux buffer — both are primitive `CallExtern` calls inside the synth
  body, explicitly paired.
- `sorted_by` allocates one clone via `gorget_array_clone`, returns it
  by value. The return ABI matches existing collection-returning
  runtime calls (handled by the normal aggregate-return machinery).
- Closure dispatch via `CallClosure` is a primitive — no implicit drop
  side-effects at the call boundary.

No drop-elab re-run needed. The synth body's drop shape is
structurally simpler than the existing `emit_hof_loop_scaffold`
bodies, which already skip drop-elab.

### 8. Self-host scope

The self-host LIR backend in `tests/fixtures/self_host_*/lir_codegen.gg`
currently does **not** route through BIR. Alignment is tracked by
`project_selfhost_bootstrap_plan.md` on a separate track. For this
plan: **self-host is out of scope.** After Option C lands, self-host
comparison tests may show additional mismatches on sort_by fixtures
— acceptable, handled in that track.

### 9. BIR validator & synthesized functions

`assert_primitives_only` (src/bir/validate.rs:36) walks **every**
function in `module.functions`. Synthesized functions are covered
automatically. No allow-list change needed.

Synth bodies contain only validator-approved primitives: `IConst`,
`FieldPtr`, `ElemPtr`, `Load`, `Store`, `Add`, `Sub`, `Cmp`,
`CallClosure`, `CallExtern`, `Jump`, `Branch`, `Return`. None of the
canonical high-level ops appear.

### 10. Atomic vs. incremental rollout

**Decision:** 4 commits. Each removes code. Each demonstrates payoff.

Commits 2 and 3 merge (sort_by + sorted_by share one impl); commits 4
and 5 merge (sort_by_key + sorted_by_key share one impl); what was
formerly Commit 5 (dead-code sweep) becomes the final commit. Rejected:
atomic — too large; feature flag — carries for one commit before
deletion.

## Worked Example

### Input

```gg
Vector[int] v = [3, 1, 4, 1, 5, 9, 2, 6]
v.sort_by(|int a, int b| a - b)
```

The closure mangles to `__Closure_7`; `__Closure_7__call(ptr env, int64_t a, int64_t b) -> int64_t`.

### LIR today — what `try_emit_vector_each_hof` would produce

Commit 2 of the migration teaches the emitter to produce:

```text
; in user function fn.main
bb3:
    ; ... v setup ...
    v6 = ...                                              ; &GorgetArray
    v8 = SlotAddr s2                                      ; closure __Closure_7
    v9 = /* pack fn_ptr + env into GorgetClosure slot */
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

### LIR after synthesis — rewritten call site

```text
bb3:
    v6 = ...
    v8 = SlotAddr s2
    v9 = /* pack */
    Call { func: fn.__gg_synth_sort_impl_i64___Closure_7__call,
           args: [v6, v9], dst: None }
    Jump bb4
```

### Synthesized function body (iterative bottom-up mergesort)

```text
fn.__gg_synth_sort_impl_i64___Closure_7__call(arr: Ptr, cl: Ptr) -> Void:

bb0 (entry):
    ; Allocate aux buffer of same shape as arr.
    aux    = CallExtern "gorget_array_new_like"(arr)     ; ret: Struct(GorgetArray)
    aux_p  = AddressOf aux                               ; Ptr for passing
    ; Reserve aux.len = arr.len (grow capacity once, up front).
    lenp   = FieldPtr arr, GorgetArray, 2
    n      = Load lenp, I64
    CallExtern "gorget_array_reserve"(aux_p, n)
    ; Outer loop: width doubles each pass — 1, 2, 4, …
    one    = IConst I64 1
    Jump bb1(one)

bb1 (outer_check) params: (width: I64):
    lt     = Cmp Lt, width, n
    Branch lt, bb2, bb10        ; if width >= n, we're done

bb2 (pass_setup):
    zero   = IConst I64 0
    Jump bb3(zero)

bb3 (pass_check) params: (left: I64):
    lt2    = Cmp Lt, left, n
    Branch lt2, bb4, bb9        ; finished this pass → double width

bb4 (merge_one_run):
    ; mid  = min(left + width, n); right = min(left + 2*width, n)
    mid_raw = Add I64, left, width, Wrap
    mid_lt  = Cmp Lt, mid_raw, n
    mid     = Select mid_lt, mid_raw, n            ; (*)
    two     = IConst I64 2
    ww      = Mul I64, width, two, Wrap
    right_raw = Add I64, left, ww, Wrap
    right_lt  = Cmp Lt, right_raw, n
    right     = Select right_lt, right_raw, n      ; (*)
    ; Merge arr[left..mid] + arr[mid..right] into aux[left..right]
    ; via inner three-way loop.
    Jump bb5(left, mid, left)

bb5 (merge_loop) params: (i: I64, j: I64, k: I64):
    ; while i < mid && j < right: copy smaller of arr[i], arr[j] to aux[k++]
    i_lt_mid = Cmp Lt, i, mid
    Branch i_lt_mid, bb5a, bb5c(i, j, k)   ; i exhausted → go drain right side

bb5a (check_j):
    j_lt_r = Cmp Lt, j, right
    Branch j_lt_r, bb5b, bb5c(i, j, k)     ; j exhausted → go drain left side

bb5b (compare):
    datap  = FieldPtr arr, GorgetArray, 0
    data   = Load datap, Ptr
    ip     = ElemPtr data, i, 8
    iv     = Load ip, I64
    jp     = ElemPtr data, j, 8
    jv     = Load jp, I64
    cmpr   = CallClosure cl, [iv, jv],
                         kind=EscapedClosure,
                         arg_abis=[Scalar, Scalar],
                         ret_ty=I64
    zero2  = IConst I64 0
    le     = Cmp Le, cmpr, zero2                     ; stability: ≤ takes left
    ; aux[k] = le ? iv : jv;  advance i or j accordingly
    auxdp  = FieldPtr aux_p, GorgetArray, 0
    auxd   = Load auxdp, Ptr
    kp     = ElemPtr auxd, k, 8
    pick   = Select le, iv, jv
    Store kp, pick
    ni     = Add I64, i, one, Wrap
    nj     = Add I64, j, one, Wrap
    new_i  = Select le, ni, i
    new_j  = Select le, j,  nj
    new_k  = Add I64, k, one, Wrap
    Jump bb5(new_i, new_j, new_k)

bb5c (drain) params: (di: I64, dj: I64, dk: I64):
    ; Copy leftovers from whichever side remains.
    ; Implemented as two short loops bb6 (drain-left), bb7 (drain-right).
    Jump bb6(di, dj, dk)

bb6 (drain_left) params: (i: I64, j: I64, k: I64):
    ; while i < mid: aux[k++] = arr[i++]
    ...                                              ; shape mirrors bb5b scalar copy
    Jump bb7(…)

bb7 (drain_right) params: (i: I64, j: I64, k: I64):
    ; while j < right: aux[k++] = arr[j++]
    ...
    Jump bb8

bb8 (run_done):
    ; Copy aux[left..right] back to arr[left..right] via memcpy.
    span     = Sub I64, right, left, Wrap
    byte_sz  = Mul I64, span, 8, Wrap                ; 8 = sizeof(I64)
    dst_base = ElemPtr data, left, 8
    src_base = ElemPtr auxd, left, 8
    Memcpy dst_base, src_base, byte_sz
    ; advance left by 2*width (the run we just merged)
    new_left = Add I64, left, ww, Wrap
    Jump bb3(new_left)

bb9 (next_width):
    new_w  = Mul I64, width, two, Wrap
    Jump bb1(new_w)

bb10 (done):
    CallExtern "gorget_array_free"(aux_p)
    Return
```

(*) `Select` is primitive LIR for `cond ? a : b`. If LIR doesn't have a
Select inst today, the emitter expands it to a Branch + block-param
pattern. Either way, no new canonical op needed.

Approximate block count: 11. Approximate instruction count: ~90.
Validator sees only primitives. The aux buffer's
`gorget_array_new_like` + `gorget_array_free` handle drop lifetime
explicitly — no drop-elab involvement.

### Variant shapes

- **SortedBy** — no separate synth fn. The `sorted_by` call site emits
  inline: `ret = gorget_array_clone(arr); Call(__gg_synth_sort_impl_*, &ret, closure); ret`.
- **SortByKey** — separate synth fn `__gg_synth_sort_impl_key_<T>_<closure>`.
  In `bb5b`, the compare step first calls `cl(iv)` and `cl(jv)` to
  extract keys (type `K` = `closure_ret_ty` on the HofExpand), then
  compares the two keys. The key comparison is primitive `Cmp` for
  numeric types and a `CallExtern "gorget_str_cmp"` for `Str`. Key
  caching within a single merge-step is a micro-optimization deferred
  to a follow-up.
- **SortedByKey** — clone inline at call site, then `Call` the key
  variant's impl.

## Migration Sequence

Four commits. Each is a single commit. Integration target: 1013/1013
after every commit.

### Commit 1 — Infrastructure (zero behavior change)

Files: `src/bir/synth.rs` (new), `src/bir/lower.rs`, `src/main.rs`,
`src/ir/lowering/mod.rs`.

- New file `src/bir/synth.rs` with `SynthPool`, `SynthKey`, empty
  `get_or_emit(...) -> FuncId` API (panics on any op — only used
  once Commit 2 wires a caller).
- `lower_lir_to_bir` constructs a `SynthPool`, passes it through
  `expand_func`, splices `pool.finish()` into `module.functions` at
  the end. Re-runs `compute_function_value_types` on each appended fn.
- Pipeline reorder: **move `lir::optimize::optimize_module` to run
  after `BirModule::from_lir`** at all three sites in `src/main.rs`
  and one site in `src/ir/lowering/mod.rs`. Requires exposing an
  `optimize_module(&mut LirModule)` shape that can be called on the
  unwrapped BIR (`bir.as_lir_mut()`). Re-run value-types after the
  optimize pass.
- Defensive: assert no module fn already starts with `__gg_synth_`.

No `HofExpand` routes through the pool yet; validator still sees
unmigrated `SortBy` etc. as they exist today (unreachable; the sort
path still flows through CallExtern + TLS trampoline).

**Payoff:** synthesis framework + correct optimizer placement.

**LOC:** +~250 `synth.rs`, +~40 `bir/lower.rs`, +~30 `main.rs` / `lowering`.

### Commit 2 — SortBy + SortedBy end-to-end

- `try_emit_vector_each_hof` grows `"sort_by" => (HofOp::SortBy, …)`
  and `"sorted_by" => (HofOp::SortedBy, …)`.
- `SynthPool::emit_sort_impl_body` — the mergesort body shown above.
- `expand_func`'s HofExpand arm routes:
  - `SortBy` → replace with `Call(sort_impl, [coll, closure])`.
  - `SortedBy` → replace with inline `ret = clone(coll); Call(sort_impl, [&ret, closure]); ret`.
- Backend: `HIGHER_ORDER_METHODS` loses `"sort_by"` / `"sorted_by"`;
  those arms deleted from `emit_vector_helper`.

**Payoff:** `sort_by` / `sorted_by` are TLS-free.

**LOC:** +~200 (synth body), -~60 (TLS arms), +~20 (emit hookup).

### Commit 3 — SortByKey + SortedByKey

- Emit switch learns both names.
- `SynthPool::emit_sort_impl_key_body` — mergesort body where compare
  extracts keys first. Key comparison dispatches on `closure_ret_ty`
  (numeric → `Cmp`, `Str` → `gorget_str_cmp`, fallback → `memcmp`
  via `CallExtern`).
- `sorted_by_key` inlines clone + call, like `sorted_by`.
- Both `sort_by_key` / `sorted_by_key` arms deleted from
  `emit_vector_helper`.

**Payoff:** all four sort variants are TLS-free.

**LOC:** +~120 (key-variant synth), -~60 (both TLS arms and the
per-key-type compare switch that exists in the TLS path).

### Commit 4 — Dead-code sweep

- `HIGHER_ORDER_METHODS` const deleted (now empty).
- `parse_vector_higher_order` deleted.
- `CollHelper::Vector` variant deleted.
- `emit_vector_helper` deleted.
- LLVM backend: corresponding sort-family arms deleted.
- Update the Vector row in the Step-8 migration-status table in
  `docs/internals/lir-backend-lift-plan.md`.

**Payoff:** backend contains zero sort-family code.

**LOC:** -~250 (backend TLS + LLVM inliner + dispatch glue).

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
  fixture's generated C (spot-check).
- `cargo test --lib bir::` passes, including a new test that feeds
  a module with a `HofExpand { SortBy }` through `from_lir` and
  asserts (a) the rewritten module contains a `Call` to a
  `__gg_synth_*` function, (b) no `HofExpand` remains, and (c) the
  synth body contains no `FieldPtr` with the closure-param as base
  (opaque-closure invariant).
- `lir::optimize::optimize_module` runs **after**
  `BirModule::from_lir` in every compile path.
- Sort behaviour: **stable** on all fixtures that exercise sort
  stability. Existing fixtures don't test this; add one.

## Open Questions

- **Key caching in `sort_by_key`.** The current synth plan re-extracts
  keys for every compare inside a merge step. Real-world use often
  has expensive key fns; caching keys into an `aux_keys: Vector[K]`
  parallel to `aux: Vector[T]` would halve closure calls. Deferred
  to a follow-up; the mergesort shape accommodates it cleanly.
- **Heapsort variant as `sort_unstable_by`.** If a user wants
  zero-alloc sort and doesn't care about stability, heapsort is the
  natural partner. Separate HofOp, separate synth fn. Not in scope
  for Option C but mentioned so the architecture accommodates it.
- **SSA validity on synth bodies.** Synth emits SSA-form directly
  (block params for loop-carried state, explicit `alloc_value`, no
  cross-block `SlotStore`/`SlotLoad` for values). Debug-mode: run
  `src/lir/validate.rs` on each emitted synth body as a cheap
  backstop. Not required for correctness — required for catching
  emitter bugs during development.
- **SortedBy return ABI.** The synth-path `sorted_by` inlines its
  clone at the call site rather than delegating to a synth fn with
  an aggregate return, sidestepping the by-value-vs-sret question
  entirely. (If a future variant does need aggregate return, the
  parent plan's Step 1 "trust `LirExtern.return_type` for sret" is
  a prerequisite.)
- **Aux buffer on the hot path.** Mergesort allocates one aux
  `GorgetArray` per `sort_by` call. For sorting a 10-element array
  this is a measurable overhead. Acceptable for the first cut;
  microbenchmark if it matters, and if it does, thread a
  pass-local scratch pool through the synth fn's signature. No
  architectural change required.

## Relationship Summary

- **What this plan owns:** eager, non-chainable ops that fit the
  `HofExpand` shape. Sort is the representative.
- **What gorget-3 owns** (stdlib-design.md): the Iterator protocol,
  lazy chained HOFs, fusion-via-monomorphization.
- **What happens when the two meet:** the eager Vector/Dict/Set
  wrappers that currently emit `HofExpand` for `filter`/`map`/`fold`
  become thin shells over `iter().method().collect()` in Phase 2c.
  Their `HofExpand` variants become unreachable; a follow-up commit
  deletes the corresponding BIR scaffolds. That cleanup belongs to
  the gorget-3 Phase-2c timeline, not this plan.

## Files Touched (summary)

| File | Change | Commit |
|---|---|---|
| `src/bir/synth.rs` | new — SynthPool, emit_sort_impl_body, emit_sort_impl_key_body | 1, 2, 3 |
| `src/bir/lower.rs` | route SortBy/SortedBy/SortByKey/SortedByKey through SynthPool | 1, 2, 3 |
| `src/lir/lower/insts.rs` | emit `HofOp::Sort*` from `try_emit_vector_each_hof` | 2, 3 |
| `src/backend/c_lir/emit_types.rs` | delete TLS arms, `HIGHER_ORDER_METHODS`, `parse_vector_higher_order`, `emit_vector_helper` | 2, 3, 4 |
| `src/backend/llvm/mod.rs` | delete sort-family LLVM inliner arms | 2, 3, 4 |
| `src/main.rs`, `src/ir/lowering/mod.rs` | move `optimize_module` to post-BIR | 1 |
| `docs/internals/lir-backend-lift-plan.md` | update Step 8 migration-status table | 4 |

Expected net LOC delta: roughly `+600` synth (mergesort body, key
variant, structured, well-commented) minus `~400` backend TLS + LLVM
inliner + dispatch glue. Net `+200` LOC with a **much** cleaner
boundary: one sort implementation, one place, one algorithm. The
eventual deletion of the eager Vector-wrapper `HofExpand` variants
after gorget-3 Phase 2c makes this a net LOC win across both plans.
