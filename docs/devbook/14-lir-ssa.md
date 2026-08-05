# 14 — LIR & SSA

LIR (Low-Level Intermediate Representation) is the SSA-form IR that sits between
GIR (the high-level, ownership-aware IR) and the backends. Everything the old
GIR→C backend used to do implicitly — drop glue, vtable dispatch, closure
dispatch, coercions, collection-method inlining, printf formatting — is made
*explicit* as LIR instructions before a backend ever runs, so backends are thin
1:1 translators and the optimizer can see every reference. LIR lives in
`src/lir/`: the data structures and `LirType`/`Inst`/`Term` enums in
`src/lir/mod.rs`, SSA construction in `src/lir/ssa.rs`, critical-edge splitting
in `src/lir/split_edges.rs`, invariant checking in `src/lir/validate.rs`, the
optimizer in `src/lir/optimize.rs`, and GIR→LIR lowering under `src/lir/lower/`.
The backend boundary (the `Backend` trait) is in `src/backend/mod.rs`.

This chapter covers the IR's shape, SSA construction, the critical-edge /
dominance validators, the typed `LirType::FuncRef`, and the backend boundary.
GIR→LIR lowering itself (how each Gorget construct becomes LIR) is a separate
concern; this chapter describes the *target* of that lowering and the passes
that run on it. The design rationale and the survey of production SSA IRs
that informed it (formerly in `lir-design.md`) are folded into this chapter.

## The split: GIR decides semantics, LIR decides mechanics

GIR is *not* replaced by LIR. GIR stays as the high-level IR for
monomorphization, borrow checking, drop *insertion* (deciding **where** drops
go), closure lifting (deciding **what** to capture), and trait resolution
(deciding **which** function). LIR decides the **how**: a GIR `Drop` becomes a
concrete call/loop sequence, a trait call becomes vtable-load + indirect-call,
a coercion becomes explicit field loads/stores.

Two consequences fall out of this split, both load-bearing:

1. **LIR is ownership-unaware.** By the time LIR exists, borrow checking is
   done and drops are placed. There are no `@owned`/`@guaranteed` annotations
   à la Swift SIL — Gorget's borrow checker runs on the AST, not on LIR, so LIR
   has no reason to carry ownership qualifiers. (`SlotStore.is_move` is the lone
   dataflow hint, and it is consumed by drop elaboration, not by a borrow
   analysis — `src/lir/mod.rs:552`.)
2. **Explicit everything.** If a backend emits code for it, LIR has an
   instruction for it. No implicit drops, no implicit coercions, no
   name-convention dispatch. This is what unblocks dead-function elimination
   and copy propagation, which were structurally impossible against the old C
   backend.

## Types

`LirType` (`src/lir/mod.rs:87`) is a concrete machine type: no generics, no
ownership qualifiers. The variants split into **scalars** (live in SSA values /
registers) and **aggregates** (address-only — live in stack slots):

- Scalars: `I8`/`I16`/`I32`/`I64`, `U8`..`U64`, `F32`/`F64`, `Bool`, and three
  pointer-shaped types — `Ptr` (opaque, like LLVM's `ptr`), `PtrTo(StructId)`
  (a pointer known to address a specific struct), and `FuncRef` (a typed
  function reference, §FuncRef below).
- Aggregates: `Struct(StructId)`, and the aggregate-shaped `Resource` kinds.
- `Void`.

`LirType::Resource { kind, params }` (`src/lir/mod.rs:150`) carries the
element/key/value parameters of a built-in resource type as **typed metadata**,
so consumers never re-parse a mangled C symbol to recover them (CLAUDE.md
layering rule 3 — one source of truth per axis). The `ResourceKind`
(`src/lir/mod.rs:470`) enumerates `GorgetString`, `GorgetArray`, `GorgetMap`,
`GorgetSet`, `GorgetClosure`, and `RefCounted` (the catch-all for pointer-shaped
RC handles: `Box`/`Shared`/`Weak`/`Channel`/`Mutex`/`RWLock`/`Guard`). The
arity per kind is fixed and validated — see `expected_resource_arity`
(`src/lir/mod.rs:180`): array/set/RefCounted take 1 param, map takes 2,
string/closure take 0.

Crucially, a `Resource` is **scalar iff `RefCounted`** — RC handles are 8-byte
pointers, while `GorgetArray`/`Map`/`Set`/`String`/`Closure` are aggregate
structs that live in slots. This is encoded once in `LirType::is_scalar`
(`src/lir/mod.rs:163`) and `is_aggregate` (`src/lir/mod.rs:171`); SSA promotion
keys off `is_scalar`, so a `RefCounted` slot promotes like any pointer while an
array slot stays in memory.

All pointer-shaped types (`Ptr`, `PtrTo`, `FuncRef`, `RefCounted`) lower to a
single 8-byte register at the C/LLVM ABI — `is_ptr` (`src/lir/mod.rs:231`)
groups them — but the IR keeps them distinct so passes can make decisions
without inspecting names.

Aggregates are address-only by deliberate design (the Cranelift/QBE model):
scalars are SSA values, structs/enums/arrays live in stack slots accessed via
typed `FieldPtr`/`ElemPtr` + `Load`/`Store`. SSA-aggregate optimizations
(SIL-style small-struct values) are explicitly left as a future targeted
optimization.

## The instruction set

`Inst` (`src/lir/mod.rs:547`) is one big enum; each instruction produces at most
one value (`dst`). `Inst::dst()` (`src/lir/mod.rs:1029`) is the canonical
"does this define a value" accessor. The families:

- **Slot access (pre-SSA)** — `SlotStore`/`SlotLoad`/`SlotAddr`
  (`src/lir/mod.rs:552`). These are the place-based code GIR→LIR lowering emits;
  SSA construction eliminates the promotable ones (§SSA below).
- **Constants** — `IConst`, `FConst`, `BoolConst`, `NullPtr`, `StrLit`,
  `FuncAddr`, `NamedFuncAddr`, `GlobalAddr`, `ParamRef`, plus the canonical-op
  `SizeOf`.
- **Arithmetic / bitwise / comparison** — `Add`..`Neg` (each carries an
  `Overflow` tag, `src/lir/mod.rs:286`, defaulting to `Trap`), `BitAnd`..`Shr`,
  `Cmp` (with a `CmpOp`, `src/lir/mod.rs:296`), `Not`.
- **Conversions (all coercions explicit)** — `IntCast`, `FloatCast`,
  `IntToFloat`, `FloatToInt`, `PtrCast`, `Bitcast`.
- **Memory** — `Load`, `Store`, `FieldPtr`, `ElemPtr`, `Memset`, `Memcpy`.
- **Calls** — `Call` (direct to a `FuncId`), `CallExtern` (**user-declared
  externs only**), `CallRuntime` (typed dispatch to a known runtime function via
  the `RuntimeFn` enum, `src/lir/runtime.rs:218`), `CallPtr` (indirect through a
  pointer), `CallByRef` (indirect through a `FuncRef`), `CallClosure` (through a
  closure's fn_ptr+env).
- **Runtime checks** — `BoundsCheck`, `DivCheck`, `Trap`.
- **Pragmatic high-level ops** — `Printf`/`Fprintf` (kept as instructions
  because format-string expansion varies by backend), `InlineC` (a
  backend-specific escape hatch), `Nop`.
- **Closures / drop guards / ownership** — `ClosurePack`, `DropGuardOpen`/
  `DropGuardClose` (conditional drop blocks, gated by a `DropGuardKind`,
  `src/lir/mod.rs:495`), `MoveSlot` (a pure dataflow annotation for drop
  elaboration).

### Canonical ops and the BIR boundary

A subset of `Inst` are **canonical ops**: structured, high-level instructions
that a *later* expansion pass lowers into the primitive subset that backends
consume. The expansion pass is `bir::lower_lir_to_bir`, and its output is a
`BirModule` newtype wrapping `LirModule` (`src/bir/mod.rs:72`). The canonical
ops include `SizeOf`, `EnumInit`/`EnumCheck`/`EnumExtract`, `StructInit`,
`CowClone`, `TraitCall`, `HofExpand`, `AddressOf`, `BoxAlloc`, and
`CollectionCtor` (`src/lir/mod.rs:572`–`966`). Each has a reject arm in the BIR
validator (`src/bir/validate.rs:54`–`108`) and an expansion arm in
`bir::lower_lir_to_bir` (`src/bir/lower.rs`). Each variant's doc comment names
the BIR expansion it stands for. Most are emitted today — `TraitCall`
(`src/lir/lower/insts.rs:2367`) and `HofExpand` (`src/lir/lower/insts.rs:2571`,
`:2739`, `:3206`) are both produced by GIR→LIR lowering and expanded by
`lower_lir_to_bir`. `CowClone` is the one canonical op that has its validator
reject arm and BIR expansion arm but no producer in lowering yet (grep for
`Inst::CowClone {` under `src/lir/lower/` returns nothing). Re-derive emission
status from the lowering code, not from this list.

Note that `SetCollectionBridge` (`src/lir/mod.rs:1005`) is **not** a canonical
op despite carrying a structured doc comment: it has no expansion arm in
`bir::lower_lir_to_bir` and no reject arm in the BIR validator (it falls through
the `_ => Ok(())` primitive catch-all, `src/bir/validate.rs:111`). It is a
backend-consumed primitive that survives unchanged into BIR — both backends
compile it as two field stores, and the C backend uses the carried `key_struct`
to pick the right `__gorget_ktable_*` helper.

The BIR boundary is enforced by the type system: the `Backend` trait consumes a
`BirModule`, and the only way to build one is `BirModule::from_lir`
(`src/bir/mod.rs:77`), which runs the expansion and a validation check. That
turns "forgot to lower a canonical op" from a silent miscompile into a
compile-time error — a downstream backend literally cannot be handed a module
that still contains an unexpanded canonical op (`src/backend/mod.rs:367`).

### Per-value provenance, not name matching

The C backend used to reconstruct facts about each `ValueId` (is it a string
literal? a NULL? a cstr from an extern? a function address?) by maintaining five
parallel bitmaps. That is exactly the name-matching / sidecar-map anti-pattern
CLAUDE.md's layering discipline forbids. LIR replaces it with one typed field:
`LirFunction.value_origins: Vec<Option<ValueOrigin>>` (`src/lir/mod.rs:1392`),
indexed by `ValueId.0`, populated once by `compute_module_value_origins` and
read at emit-decision sites via a typed match. `ValueOrigin`
(`src/lir/mod.rs:520`) tags a value as `StrLit`, `NullPtr`, `CStr{from_extern}`,
`FuncAddr(FuncId)`, or `SpawnSource(String)`. The companion `pointee_types`
table (`src/lir/mod.rs:1383`) carries per-pointer-value pointee types the same
way. This is the LIR side of Phase D6 of the unified resource model
(the former `unified-resource-model.md` §6.8, now folded into chapters 13–14):
the GIR-side `BorrowOrigin` provenance has a deliberate LIR-side counterpart. Note that a *typed
`Slot.origin` field* (per-slot, as opposed to per-value `value_origins`) is NOT
yet present — `Slot` (`src/lir/mod.rs:1334`) carries only `ty` and `name`; that
last step remains future work (it gates cross-pass borrow-aware codegen).

## Functions, blocks, slots, terminators

`LirModule` holds `structs`, `globals`, `functions`, `externs`
(`src/lir/mod.rs:1702`). A `LirFunction` (`src/lir/mod.rs:1343`) holds `params`,
`return_type`, `slots`, `blocks`, an internal `next_value` counter, plus the
typed sidecar tables (`value_types`, `pointee_types`, `value_origins`) computed
after SSA + optimization. Allocate identities with `next_value`
(`src/lir/mod.rs:1416`), `add_slot` (`src/lir/mod.rs:1440`), `add_block`
(`src/lir/mod.rs:1447`).

A `Slot` (`src/lir/mod.rs:1334`) is the pre-SSA representation of a local
variable: just a type and a debug-name hint. SSA construction promotes scalar
slots to values; aggregate slots stay as stack allocations.

A `Block` (`src/lir/mod.rs:1276`) is `{ id, params, insts, terminator }` plus a
parallel `span_map` (one `Option<Span>` per instruction; invariant
`span_map.len() == insts.len()`) and a `terminator_span`. **Block parameters,
not phi nodes** — this is the Cranelift model: `params: Vec<(ValueId, LirType)>`
is empty pre-SSA and populated at merge points by SSA construction. Because the
`span_map` must stay in lockstep with `insts`, mutate through `push_inst`,
`push_synthetic`, `insert_inst` (`src/lir/mod.rs:1301`–`1325`) rather than
poking `block.insts` directly.

A `Term` (`src/lir/mod.rs:1207`) is the block terminator: `Ret(v)`, `RetVoid`,
`Jump(target, args)`, `Branch{cond, then/else block+args}`, `Switch{value,
cases, default}`, or `Unreachable`. Every edge carries its own argument vector
(the values passed to the target's block parameters). `Term::successors`
(`src/lir/mod.rs:1258`) and `Term::uses` (`src/lir/mod.rs:1235`) are the
traversal accessors the CFG passes use.

## SSA construction

GIR→LIR lowering emits **non-SSA** place-based code: scalars go through
`SlotStore`/`SlotLoad`, and a scalar slot may be stored many times. A separate
pass — `construct_ssa` (`src/lir/ssa.rs:19`) — promotes scalar slots to SSA
values and inserts block parameters. This is the QBE/LLVM-mem2reg pattern:
constructing SSA *after* lowering is dramatically simpler than constructing it
during lowering. The algorithm is a simplified Braun et al. 2013 ("Simple and
Efficient Construction of SSA Form").

The pass runs per-function:

1. **Find promotable slots.** `find_promotable_slots` (`src/lir/ssa.rs:43`): a
   slot is promotable iff it is scalar (`LirType::is_scalar`) and no `SlotAddr`
   instruction references it. An addressed slot's identity escapes, so it must
   stay in memory.
2. **Walk blocks in reverse postorder.** `compute_rpo` (`src/lir/ssa.rs:67`)
   produces an RPO so dominators are processed before the blocks they dominate.
   This matters concretely: critical-edge splitting can append high-numbered
   blocks that dominate low-numbered GIR blocks, and RPO order is what keeps
   `read_variable` finding definitions from already-processed predecessors
   (`src/lir/ssa.rs:138`).
3. **Per block (`process_block`, `src/lir/ssa.rs:156`):** a `SlotStore` to a
   promotable slot records the stored value as that slot's current definition
   and is dropped; a `SlotLoad` is replaced by the reaching definition
   (`read_variable`, `src/lir/ssa.rs:224`), and the old `dst → reaching`
   mapping is recorded in `value_subst`.
4. **Reaching definitions.** `read_variable` resolves a slot's value at a block:
   a local def short-circuits; a single predecessor recurses; **multiple
   predecessors** force a block parameter via `add_block_param`
   (`src/lir/ssa.rs:270`). A predecessorless entry block with no def gets a
   typed zero/null constant inserted at the block head (`src/lir/ssa.rs:233`).
5. **Patch terminators.** `patch_terminators` (`src/lir/ssa.rs:318`) iterates to
   a fixpoint resolving reaching defs (creating cascading phis where
   predecessors disagree — `resolve_reaching_def`, `src/lir/ssa.rs:392`), then
   appends the right arguments to each predecessor's terminator edge in
   block-param order via `add_args_to_terminator` (`src/lir/ssa.rs:441`).
6. **Apply substitutions.** `remove_promoted_instructions`
   (`src/lir/ssa.rs:292`) rewrites every remaining use of an eliminated
   `SlotLoad.dst` to its reaching value via `substitute_inst_values` /
   `substitute_term_values` (`src/lir/ssa.rs:480`, `:628`), after flattening any
   transitive substitution chains with `resolve_value` (`src/lir/ssa.rs:212`).

After construction, `construct_ssa` runs `validate_ssa_dominance` in debug
builds and asserts it is empty (`src/lir/ssa.rs:30`–`37`).

## Critical-edge splitting

Braun-et-al SSA construction assumes there are **no critical edges** — an edge
from a block with >1 successor to a block with >1 predecessor. Without
splitting, you cannot always place a per-edge action (a phi argument, an
edge-local copy) at a unique program point, because the source is shared across
siblings and the target across cousins. `split_critical_edges`
(`src/lir/split_edges.rs:29`) is a **pre-SSA** pass that inserts a fresh empty
block with a single unconditional `Jump` on every critical edge and repoints the
source terminator at it. The post-conditions: for every edge `s → t`,
`succ(s).len() == 1 || pred(t).len() == 1`; the edge count is preserved (each
split removes one edge and adds one); and no values/slots/phis are introduced —
the pass touches only the CFG skeleton (`src/lir/split_edges.rs:16`–`21`). It is
idempotent. The module-wide driver is `split_critical_edges_module`
(`src/lir/split_edges.rs:78`).

This also pays off for a future WASM backend: WASM has structured control flow
only, and the relooper/stackifier needs critical-edge-free, reducible CFGs.

## Validation: dominance, reducibility, no critical edges

`validate_module` (`src/lir/validate.rs:39`) runs the structural checks
(block-param/arg counts, in-range block ids, no duplicate value defs, valid
terminator targets) and then, for every function, the three CFG/SSA invariants:
`check_no_critical_edges` (`src/lir/validate.rs:440`), `check_reducible_cfg`
(`src/lir/validate.rs:485`), and `validate_ssa_dominance`
(`src/lir/validate.rs:605`) — all wired in at `src/lir/validate.rs:50`–`52`.

> `validate_ssa_dominance` *is* wired into `validate_module` (`src/lir/validate.rs:52`),
> alongside the critical-edge and reducibility checks on lines 50–51 — it is not a
> debug-only helper.

Both `check_reducible_cfg` and `validate_ssa_dominance` compute dominators with
the Cooper-Harvey-Kennedy iterative algorithm over RPO numbering (the two share
the same shape so they stay consistent — `src/lir/validate.rs:491`). Reducibility
is checked by a colored DFS: an edge `u → v` to a gray (on-stack) node is a
back-edge, and the CFG is reducible iff every back-edge target dominates its
source (`src/lir/validate.rs:554`–`600`). Dominance validation flags any value
use not dominated by its definition.

### Validator framework: runs after every pass

`validate_module` is not the only validator. There is a registry of per-pass
validators — `VALIDATORS` (`src/lir/validate.rs:98`) — including
`validate_box_inner_type`, `validate_drop_completeness`, `validate_drop_fn_presence`,
`validate_resource_arity` (the `expected_resource_arity` arity invariant), and
their consistency inverses. `assert_module_valid(module, after)`
(`src/lir/validate.rs:112`) runs the whole registry; in debug builds it runs
unconditionally, and in release builds it is a no-op unless `GG_VALIDATE_PASSES`
is set. The production pipeline calls it after *every* LIR pass — see the
`assert_module_valid(..., "lir-lowering")`, `"ssa-construction"`, `"optimize"`,
`"wire-collection-bridges"`, `"promote-runtime-calls"`, `"compute-types"`
sequence in `src/main.rs` (e.g. `src/main.rs:594`–`619`) and mirrored in the
test harness (`src/lir/integration.rs:31`–`55`). Each new shape invariant plugs
in by appending one `fn(&LirModule) -> Vec<LirError>` to the registry.

## The pass pipeline

The end-to-end LIR pipeline (the `gg build` path, `src/main.rs`, and the same
sequence in `src/lir/integration.rs:11`–`68`):

1. GIR → LIR: `lower::lower_module` (`src/lir/lower/mod.rs`).
2. `split_critical_edges_module` — pre-SSA, so Braun-et-al SSA sees no critical
   edges (`src/main.rs:594`).
3. `construct_ssa` per function (`src/main.rs:597`).
4. `optimize::optimize_module` (`src/main.rs:601`).
5. `wire_collection_bridges` then `promote_runtime_calls` (the
   `CallExtern → CallRuntime` promotion, `src/lir/runtime.rs:675`).
6. `compute_module_pointee_types` → `compute_module_value_types` →
   `compute_module_value_origins` (order matters: pointee types first so value
   types can fall back through them, `src/main.rs:617`).
7. `BirModule::from_lir` for the C-emit path, which runs the canonical-op
   expansion and re-runs split/optimize/compute-types on the lowered module
   (`src/main.rs:658`–`668`).

### Optimizer

`optimize_module` (`src/lir/optimize.rs:47`) runs per-function passes — DCE,
constant folding, intra-block CSE, copy propagation, dead-function elimination,
drop elaboration, etc. — to a **fixpoint**: `optimize_function`
(`src/lir/optimize.rs:104`) loops until an iteration makes no change, with a
hard cap of `MAX_ITERS = 32` as a safety net (`src/lir/optimize.rs:110`–`131`).

> Re-derive the loop's shape from `optimize.rs`. Older descriptions had it run a
> fixed three iterations and stop; that is not what the code does.

Cross-block passes — global constant propagation, GVN, LICM — are explicitly
**deferred**: the LIR optimizer is intra-block
today, on the reasoning that `clang -O2` (C backend) and LLVM's own optimizer
(LLVM backend) do most cross-block work downstream. Cross-block LIR optimization
matters most for WASM, where downstream optimization is weaker, so it is deferred
until WASM ships. This is a roadmap item, not a chapter fact.

## `LirType::FuncRef` — typed function references

`Inst::FuncAddr` / `Inst::NamedFuncAddr` produce a value of type
`LirType::FuncRef` (`src/lir/mod.rs:115`; the type is assigned at
`src/lir/types.rs:252`), and `Inst::CallByRef { fref, ... }`
(`src/lir/mod.rs:889`) consumes one. A `FuncRef` is pointer-shaped at the
C/LLVM ABI — it lowers to `void*` / LLVM `ptr` and `CallByRef` is treated
exactly like `CallPtr` — but it is **semantically distinct** from `Ptr`: it
references *code*, not *data*, and never aliases data. Two reasons the distinct
type exists:

- A future WASM backend can lower `FuncRef` to a **table index** and `CallByRef`
  to `call_indirect`, rather than an opaque indirect-pointer call — WASM has no
  raw function pointers.
- Passes can distinguish "raw function ref" from "boxed closure" without
  inspecting names — `is_funcref` (`src/lir/mod.rs:204`) is the typed predicate,
  not a name heuristic.

The §8.6 plan ("Inst::FuncAddr's dst becomes FuncRef instead of Ptr") has
**shipped** — `infer_inst_type` returns `FuncRef` for both `FuncAddr` and
`NamedFuncAddr` (`src/lir/types.rs:251`–`252`). `validate_module` checks that
`CallByRef.fref` references a *defined* SSA value (`check_call_by_ref`,
`src/lir/validate.rs:223`–`228`); it does **not** yet type-check that the value
is a `LirType::FuncRef`, despite the `CallByRef` doc comment claiming
"`validate_module` bound-checks this" (`src/lir/mod.rs:884`–`885`). The
definedness check exists; the FuncRef-type assertion is doc-ahead-of-code.
Re-derive the actual status from source.

## The backend boundary

The `Backend` trait (`src/backend/mod.rs:377`) is intentionally tiny:
`name()`, `generate(&BirModule) -> CodegenOutput`, an optional `features()`
(`BackendFeatures` — debug info / hot reload / per-function emit,
`src/backend/mod.rs:358`), and an optional `emit_function` for the
per-function debug path. A backend is a thin translator with no semantic
decisions: `LirType` → a type string/value type, `Inst` → one target statement,
`Block` → a labeled block, block parameters → parallel-move variables (C) or phi
nodes (LLVM). The production backend is `src/backend/c_lir/`; an LLVM backend
ships behind `GG_BACKEND=llvm` (`--backend=llvm`). Both consume the same LIR/BIR
and should be at parity — a regression on one but not the other points at a
backend-specific path rather than shared LIR.

Note the trait takes a `BirModule`, not a raw `LirModule` — the canonical-op
expansion is a precondition baked into the type, per the BIR boundary above
(`src/backend/mod.rs:367`–`382`).

## In the self-host

The self-host compiler reimplements the LIR layer in Gorget under
`tests/fixtures/self_host_lowerer/`: `lir_lower.gg` (GIR→LIR lowering, ~3,700
lines), `lir_ssa.gg` (SSA construction, ~600 lines), and `lir_codegen.gg`
(emission, ~5,300 lines). These are independent Gorget ports of the Rust passes
above.

`lir_ssa.gg` mirrors `src/lir/ssa.rs` closely: `find_promotable_slots`
(`lir_ssa.gg:21`) walks scalar slots and removes any with an `ISlotAddr`;
`compute_predecessors` (`lir_ssa.gg:52`) and `compute_rpo` (`lir_ssa.gg:84`)
build the CFG order; `process_block`, `read_variable`, `add_block_param`,
`patch_terminators`, and `construct_ssa` carry the same names and shapes as the
Rust originals. A notable divergence forced by Gorget's surface: where the Rust
`SsaBuilder` keeps `current_def`/`block_params`/`incomplete_phis`/`value_subst`
as struct fields, the Gorget port threads them as explicit `&`-borrowed `Dict`
arguments through every function (e.g. `read_variable`'s signature at
`lir_ssa.gg:215`), because it has no equivalent builder-struct-with-mutable-self
idiom in this code path. The `compute_rpo` comment (`lir_ssa.gg:73`–`83`)
documents a real bug the port hit: processing blocks out of RPO order makes the
single-pred recursion in `read_variable` zero-init instead of finding the store,
silently corrupting destructure-then-branch match shapes — the same RPO
requirement the Rust pass relies on.

**Parity.** The self-host LIR lowering is exercised by the `lowerer_comparison`
test, which compares the **GIR function count** the Gorget driver emits against
the Rust `gg build --emit-gir` count, per fixture (`tests/integration.rs:13390`).
These `*_comparison` tests are diagnostic-always-pass — a green `cargo test`
asserts nothing about parity; only the printed matched-count does. To read the
current score:

```bash
cargo test --test integration lowerer_comparison -- --nocapture
```

and read the printed matched / mismatched / crashed counts. (As of the last
recorded handover the lowerer was fn-count-matched on the vast majority of
fixtures; backend C-emission parity, measured by `c_emit_comparison`, was the
larger remaining gap — but **re-run the comparison rather than trusting any
quoted figure**, including this one.) There is no separate self-host coverage of
the *backend* `Backend`-trait boundary as a unit — `lir_codegen.gg` emits C
directly; the comparison net is the GIR/LIR-shape match plus the
`bootstrap_fixed_point` self-reproduction test, not a typed backend-trait
contract.
