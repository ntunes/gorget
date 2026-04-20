# Handover — Option C (BIR Module-Level Synthesis) Plan

> **Status:** Scope memo, 2026-04-20. Plan not yet written.
>
> This is a handover for the **next session** to write a design doc for
> Option C. Not a plan itself — a scope memo with the questions the
> plan must answer and the context pointers to start from.

## What Option C is

BIR becomes a **module-level** rewrite pass (not just per-function) that
can synthesize new function definitions. The first use case is the
Vector `sort_by` / `sorted_by` / `sort_by_key` / `sorted_by_key`
family — currently the last remaining HOF inliner in the C backend
(`src/backend/c_lir/emit_types.rs:180-260`, the TLS trampoline).

Under Option C, BIR lowering emits a synthesized `__gg_sort_impl_<…>`
function composed of primitive LIR (containing a quicksort loop that
uses `CallClosure` natively for comparisons) and rewrites each
`sort_by` call site into a `Call` to that function. Backends see only
primitive LIR — no per-call-site adapter, no TLS, no qsort wrapper.

## Why this instead of Option A

Option A (tiny per-call-site adapter + runtime quicksort) would remove
the TLS hack and ship in one commit. The user chose Option C because
it sets up better for:

- **WASM backend** — backends see only primitive blocks, no runtime
  trampoline dependency.
- **Freestanding / UEFI** — no libc qsort dependency.
- **"Backends are dumb" endgame** — matches the plan doc's TL;DR that
  `&BirModule` is the type-safe boundary after which no semantic
  decisions remain at the emit layer.

The scope expansion is real: BIR changes from a function-level pass to
a module-level pass that can emit new function definitions.

## Context pointers (read these first)

- **`docs/internals/lir-backend-lift-plan.md`** — the Steps 0-10 plan.
  Read the TL;DR, the "Three Gaps" section, and Step 8's **Migration
  status** block (≈ line 583). Step 8's HOF migration is what created
  the `HofExpand` / `expand_*` precedent Option C extends.

- **`src/bir/lower.rs`** — today's `lower_lir_to_bir` is a per-function
  rewrite driven by `expand_func`. Option C changes this signature.
  The module-level scaffolds (`emit_hof_loop_scaffold`,
  `emit_dict_hof_loop_scaffold`, `emit_set_hof_loop_scaffold`) are the
  pattern the synthesized sort function will follow internally, but
  they'll be emitted into a fresh function body instead of splicing
  into an existing block.

- **`src/backend/c_lir/emit_types.rs:180-260`** — current sort_by TLS
  trampoline. ~80 lines of per-type inline C with thread-local save/
  restore. This is what Option C replaces. Use it as the
  specification for what the synthesized function must do.

- **`src/lir/mod.rs`** — `HofOp` enum, `Inst::HofExpand`, the existing
  canonical ops. Option C likely adds `HofOp::SortBy`, `SortedBy`,
  `SortByKey`, `SortedByKey` variants that the synthesis consumes.

- **`src/lir/lower/insts.rs::try_emit_vector_each_hof`** — the emit-
  time intercept pattern for Vector HOFs. SortBy would plug in here
  with `is_sort_by=true` and emit a `HofExpand` for BIR to consume.

- **`LoweringContext::closure_call_sigs`** — snapshot of closure
  signatures built once per module, keyed by `__Closure_N__call` and
  (for FuncRef) the target function name. Synthesis will need this
  to generate the correct `CallClosure` arg ABIs inside the
  synthesized function.

- **`src/lir/optimize.rs`** — the LIR optimization pipeline. Ordering
  question: run optimize before or after BIR synthesis? (Probably
  after — so DCE can drop unused synthesized helpers.)

- **`tests/fixtures/self_host_*/lir_codegen.gg`** — the self-host
  LIR backend. Currently doesn't go through BIR. Plan should state
  whether the self-host is expected to implement Option C or is
  deferred until a later step.

## Key architectural questions the plan must answer

### 1. Function synthesis API

Does `lower_lir_to_bir` grow:

- **(a)** A module-level `LirModule` return that splices synthesized
  functions into `module.functions`, or
- **(b)** A separate `synthesized: Vec<LirFunction>` return the caller
  merges, or
- **(c)** An in-place `&mut LirModule` signature that appends
  directly?

The existing signature is `fn lower_lir_to_bir(module: LirModule) -> Result<LirModule, BirError>`. Option (c) via `&mut` breaks that;
option (a) keeps by-value-in, by-value-out. (a) is probably right.

### 2. De-duplication

If `v.sort_by(|a,b| …)` appears at 20 call sites with the same closure
type, how many `__gg_sort_impl_*` functions?

- Key = closure `__call` function name + element LIR type.
- Same call shape → same synthesized function.
- Need a `SynthCache: FxHashMap<SynthKey, FuncId>` in the pass.

### 3. Naming & namespace

Synthesized function names live in the same flat namespace as user
functions. Reserved prefix (e.g. `__gg_synth_`) that monomorphization
is guaranteed never to produce. The plan doc must specify the
prefix and reserve it.

### 4. Optimization ordering

Today's pipeline: `GIR → LIR → optimize → BIR expansion → backend`.

After Option C: synthesized functions need optimization too (DCE, etc).
Probably: `GIR → LIR → BIR synthesis → LIR optimize → backend`. The
plan must pick one and justify.

### 5. ClosureCallSig visibility inside synthesized functions

The synthesized `__gg_sort_impl_ClosureN` needs to emit `CallClosure`
against `ClosureN`. It has access to `ClosureCallSig` via the module,
but the synthesis pass must plumb that through. The current
`LoweringContext.closure_call_sigs` isn't visible to `lower_lir_to_bir`
— plan must specify how it flows.

### 6. Monomorphization interaction

Monomorphization runs at GIR. BIR synthesis runs later. Are there
cases where synthesis would produce a function that monomorphization
*should have* produced differently? (E.g., synthesized function
captures an element type that's a generic placeholder rather than a
concrete type.) Plan must demonstrate synthesis always sees concrete
types.

### 7. Drop elaboration

Drop elaboration runs during GIR → LIR lowering (computes ownership /
move semantics per function). The synthesized function didn't exist
at GIR time, so its drops must be derived from the body it contains.
Since the body is hand-assembled from primitive LIR (loads + stores +
CallClosure + Cmp), the drop shape should be analogous to existing
scaffolds (`emit_hof_loop_scaffold`). Plan must confirm no drop-elab
pass re-runs would be needed.

### 8. Self-host

The self-host LIR backend doesn't have BIR today. Option C adds BIR
module-level synthesis that self-host will eventually need. Plan must
scope: "self-host out of scope for this step; catches up separately."

### 9. Type-safety at the BIR boundary

Plan doc's current TL;DR says "backends take `&BirModule` and see only
primitives." Synthesized functions are composed of primitive LIR, so
this invariant still holds — but plan must say so explicitly and the
validator (`src/bir/validate.rs`) must cover synthesized functions
with the same allow-list.

### 10. Incremental rollout

Does Option C ship atomically (synthesize sort_by family, delete TLS
trampoline, all in one commit), or incrementally with a feature flag
(synthesis emits the new function; legacy TLS path kept as fallback;
cutover in a follow-up)?

The existing plan's "each step is a single commit, each removes code,
each demonstrates payoff" philosophy suggests atomic. But Option C is
bigger than a single Step 8 sub-commit. Plan must decide.

## What NOT to do in the plan

- Don't spec a custom quicksort implementation inside the runtime C —
  that's Option A's approach. The synthesized function should contain
  the sort loop in LIR, not call a runtime quicksort.
- Don't propose changing the closure ABI globally. Option C uses the
  existing `CallClosure` protocol from inside synthesized code; no
  ABI change.
- Don't plan the self-host LIR backend implementation — scope it as
  deferred.
- Don't write a plan that's also an implementation. Architecture
  first. Implementation comes in a follow-up session after the plan
  is reviewed.

## What I already shipped (Step 8 finale)

Four commits landed on this branch before this handover:

- `fcd71d48` — Set union/intersection/difference/symmetric_difference
  via runtime stubs (type-independent, one stub per op covers every T;
  `gorget_set_new_like` mirrors src's hash/eq/drop/clone config).
- `81fababe` — Set.filter via HofExpand + `gorget_set_new_like`.
- `75af2dde` — Dict.filter via HofExpand + `gorget_map_new_like`.
- `b5653d3e` — Dict.get_or / Dict.get_or_put intercepted at LIR emit
  time (block-split, no per-type C helper, no new HofOp variant).

After these: `emit_dict_helper` and `emit_set_helper` are TODO stubs;
the `emit_vector_helper` only contains the sort_by family TLS
trampoline that Option C removes.

Integration: 1013/1013 at handover time.
