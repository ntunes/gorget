# Chapter 15 — Drop elaboration & optimization

Drop elaboration is the LIR pass that turns conservatively-emitted, always-conditional resource drops into the cheapest correct runtime form: it *deletes* a drop the dataflow proves dead, *strips the guard* off a drop the dataflow proves live, and falls back to a stack-local `bool` drop flag only when initialization is genuinely path-dependent. It runs as the tail of the LIR optimizer. The pass lives in `src/lir/drop_elab.rs`; it is scheduled (after the main fixpoint optimizer and a validation pass) by `optimize_module` in `src/lir/optimize.rs:46`. This chapter also covers the surrounding LIR optimizer fixpoint loop, since the two ship together and the doc that motivated this chapter (the former `unified-resource-model.md` §8.4, now folded here) made a now-false claim about it.

## Why the pass exists

The conditional guard is materialized in two layers. The GIR drop accountant (`src/ir/lowering/drops.rs`) emits every resource-typed scope-exit drop as an **unconditional `DropIfAlive`** — it never decides statically whether the slot still owns a value, because the GIR-level `maybe_moved` analysis is *not* a sound CFG-aware move analysis (see `structural-guards.md` and `unified-resource-model.md` §8.1). The GIR→LIR lowering then turns each `DropIfAlive` into the actual guard shape: `Instruction::DropIfAlive` lowers via `lower_drop(place, bb, /*conditional=*/true)` (`src/lir/lower/insts.rs:1426`), and with `conditional == true` that materializes a `DropGuardOpen { kind: NonZero { size } }` … `DropGuardClose` pair wrapping the `T__drop` call — emitted at `src/lir/lower/drops.rs:142` and several sibling sites, always with `DropGuardKind::NonZero`. This is "defensive by default": GIR always emits the runtime check and lets a real dataflow pass downstream remove it.

The runtime check is not free. The C backend lowers `NonZero { size }` to an inline `memcmp` against a zeroed stack buffer (`src/backend/c_lir/mod.rs:2883`):

```c
{ char __dia_z[SIZE] = {0}; if (memcmp(ADDR, __dia_z, SIZE) != 0) { /* drop body */ } }
```

Drop elaboration replaces that `memcmp` with compile-time knowledge wherever a forward dataflow can supply it, and with a cheap `bool` load (`DropGuardKind::Bool`, lowered to `if (flag)` at `mod.rs:2880`) where it cannot.

### Every exit is an ordinary exit — the by-value error channel

The scope-exit drops this pass optimizes are the *whole* drop story: Gorget has no unwind substrate, so there is no second, unwind-time drop path for the elaborator to reason about. Errors propagate by value, not by unwinding. A `throws E` function lowers to a `Result[T, E]`-valued return; a `throw` builds `Error(val)` and returns it by value, and every call site that receives a `Result` from a callee auto-propagates by re-wrapping the error in the current frame's `Result` and returning by value again (`maybe_auto_propagate` → `emit_result_auto_propagate`, `src/ir/lowering/exprs/`), so an error threads up N frames as N ordinary returns. At each of those early returns — and at every `return`/`break`/`continue` — the GIR drop accountant runs `emit_early_exit_drops` (`src/ir/lowering/drops.rs`), emitting exactly the `DropIfAlive` markers a fall-off-the-end scope exit would; drop-correctness across a propagating error is therefore just ordinary CFG drop insertion, and this pass elaborates those guards identically. Nothing in either backend carries an unwinder — there are no `landingpad`/`invoke`/`personality`/`resume` instructions in the LLVM backend, and an uncaught panic is a plain `exit(1)` (`gorget_panic_at`, `panic_normal.c`). The `gorget_throw`/`GORGET_TRY` `setjmp`/`longjmp` pair in `runtime_error.c` is vestigial — emitted only as the fallback arm for a `throw` in a non-`throws` (ill-typed) context — and never sits on the drop path this chapter governs.

## The initialization lattice

The analysis is a standard forward init/uninit dataflow. The per-slot lattice has three reachable states (`src/lir/drop_elab.rs:27`):

- `Initialized` — the slot holds a live resource on **every** predecessor path.
- `Uninitialized` — the slot was zeroed/moved on every predecessor path.
- `MaybeInitialized` — live on some paths, dead on others; a runtime guard is still required.

The meet (join over predecessors) is: equal states are idempotent, and `Initialized` meet `Uninitialized` = `MaybeInitialized`. `MaybeInitialized` is the top of the reachable lattice (absorbing).

### Packed 2-bit `SlotStates`

The dataflow state is *not* a `Vec<InitState>` or a `HashMap`. It is a packed bit-vector: 2 bits per slot, 32 slots per `u64` word, indexed by `SlotId.0` (`src/lir/drop_elab.rs:80`). The 2-bit encoding (`InitState::bits`, `drop_elab.rs:45`) is deliberately chosen so that **the lattice meet is exactly word-wise bitwise OR** (`SlotStates::meet`, `drop_elab.rs:137`):

| State | Bits |
|---|---|
| Top / no-info | `0b00` |
| `Initialized` | `0b01` |
| `Uninitialized` | `0b10` |
| `MaybeInitialized` | `0b11` |

`0b01 | 0b10 = 0b11` is exactly `meet(Init, Uninit) = Maybe`; `0b11 | x = 0b11` makes Maybe absorbing; `0b00 | x = x` makes Top the OR-identity. The decode (`InitState::from_bits`, `drop_elab.rs:53`) reads both `0b11` (Maybe) and `0b00` (no-info) as `MaybeInitialized`, so a slot the analysis never reached defaults to the safe "guard required" state — matching the prior dense-`Vec` `.get(...).unwrap_or(MaybeInitialized)` shape.

The motivation is memory, not cleverness for its own sake. The hot self-host lowerer function has ~2300 slots; the pass clones the state per worklist pop, per successor edge, and per block in `elaborate_block`, so a 1-byte-per-slot `Vec` made each clone a ~2.3 KB memcpy and each meet a ~2.3 KB allocation. Packing to 2 bits/slot shrinks every clone/alloc ~16× and turns the per-slot three-way match into one bitwise-OR processing 32 slots per instruction (`drop_elab.rs:65-79`). This is the project's "performance work measures MEMORY" rule in action — the packing was a memory fix, not a speed micro-optimization.

Out-of-capacity reads (`SlotStates::get`, `drop_elab.rs:108`) and out-of-capacity writes (`SlotStates::set`, `drop_elab.rs:122`) are both bounds-checked: reads beyond the sized words return `MaybeInitialized`, writes beyond capacity silently drop. `clear_tail` (`drop_elab.rs:152`) zeroes the unused 2-bit fields in the final word so two `SlotStates` compare equal iff their in-range contents match — no spurious tail-bit differences breaking the fixpoint equality check.

## The forward dataflow

`forward_dataflow` (`src/lir/drop_elab.rs:210`) is a worklist-based forward analysis producing one entry (`in`) state per block.

### Entry seeding

By GIR convention slot 0 is the reserved return-value local and slots `1..=N` are the function parameters. The bb0 entry state seeds **param slots `Initialized`** (the caller wrote the value) and **everything else `Uninitialized`** (`drop_elab.rs:221-228`). This precise seed is load-bearing for the drop-flag phase: with params seeded `Initialized`, the bb0 flag initialization can emit `flag := true` for owning params directly, without relying on the explicit param-`SlotStore` to "fix up" a blanket-false flag at first use.

Non-entry blocks start at lattice **Top** (`None`, `drop_elab.rs:237`), not `all_init`. Meeting Top with a predecessor's out-state adopts that out-state directly. The comment at `drop_elab.rs:233` records why this matters: seeding non-entry blocks `all_init` would collapse a sole `Uninitialized` predecessor into `MaybeInitialized` (because `meet(Init, Uninit) = Maybe`), over-flagging definitely-dead slots.

### Transfer function

The per-instruction transfer (`apply_inst_effect`, `drop_elab.rs:303`) is small:

- `SlotStore` / `ClosurePack` writing a slot → `Initialized`.
- `MoveSlot { slot }` → `Uninitialized`. This is a zero-cost annotation (no runtime effect) emitted upstream purely as the dataflow signal for ownership transfer.
- `Memset { ptr }` whose `ptr` traces back to a slot → `Uninitialized`. Only *projected* `MoveZero` (field-level moves) still emits `Memset`; whole-slot moves use `MoveSlot`.

The `ptr → slot` mapping comes from `build_val_to_slot` (`drop_elab.rs:179`), which records every `ValueId` produced by a `SlotAddr` instruction. This map is built once per function and stays valid across all phases, because elaboration only removes instructions *inside* guard ranges (and dead Memsets) — never the `SlotAddr`s that precede them (`drop_elab.rs:796`).

The worklist (`drop_elab.rs:243`) starts with only bb0; successors are queued as their in-states acquire information. Unreachable blocks keep an empty (default) in-state, which is safe — every read defaults to `MaybeInitialized`, so nothing gets eliminated for an unanalyzed block.

## Guard elaboration (Phase 1)

`elaborate_block` (`src/lir/drop_elab.rs:379`) walks each block with its seeded entry state, updating the running state per instruction, and at each `DropGuardOpen { kind: NonZero }` it resolves the guarded slot (via `val_to_slot`) and branches on the slot's current state (`drop_elab.rs:409`):

- **`Uninitialized`** → delete the **entire** guard sequence: open + drop body + matching close (`drop_elab.rs:413`). The drop is provably dead. The slot is recorded in `deleted_slots` so its companion `Memset` can be removed in Phase 2.
- **`Initialized`** → delete only the open and close (`drop_elab.rs:423`), keeping the inner drop calls. The drop is unconditionally live; the `memcmp` guard is pure overhead.
- **`MaybeInitialized`** → record the slot in `maybe_init_slots` for Phase 3's bool-flag treatment. Leave the instructions in place for now.

### Matching opens to closes

`find_matching_close` (`drop_elab.rs:344`) does depth-tracked matching of nested guard pairs within a block. Crucially, it **panics** on an orphan open rather than skipping. The rationale (documented at `drop_elab.rs:336`): a missing close means an earlier pass corrupted the LIR; silently passing through would either leave a runtime guard the elaborator promised to remove (a leak) or leave a stale `DropGuardClose` that paints a later open's drop as this one's (a silent miscompile). The pass forces the responsible upstream transform to surface. The contract: open/close pairs are emitted matched by `lower/drops.rs`, and any pass that reorders/deletes/splits guards must preserve nesting and keep pairs within a block.

The sweep at the end of `elaborate_block` (`drop_elab.rs:450`) uses lockstep `Vec::retain` on `block.insts` and the parallel `block.span_map` to drop deleted indices in place without allocating fresh vectors — and reseeds `span_map` to parallel-empty if it was out of sync.

## Companion Memset removal (Phase 2)

`remove_companion_memsets` (`src/lir/drop_elab.rs:476`) removes `Memset`-to-zero instructions whose target slot is in `deleted_slots`. Once the guard *and* the drop call for a slot were both deleted (the `Uninitialized` case), the zeroing memset that fed the now-gone `memcmp` is dead — nothing will ever read those bytes. Same in-place lockstep `retain`-with-span_map shape as Phase 1.

## Bool drop flags (Phase 3)

`insert_drop_flags` (`src/lir/drop_elab.rs:538`) handles the `MaybeInitialized` slots. For each such slot it allocates a fresh `Bool` flag slot named `_df_<n>` (`drop_elab.rs:554`), processed in sorted slot-id order for deterministic output. Then four steps:

1. **Allocate** one bool flag slot per maybe-init slot.
2. **Seed** the flag at bb0 entry from the dataflow's bb0 in-state (`drop_elab.rs:589`): `Initialized` → `flag := true` (a param or other unconditionally-live slot), otherwise `flag := false` (a local not yet stored, or the return-value slot). The inits are prepended to bb0, with the `span_map` prepend mirrored so the parallel-array invariant holds. This dataflow-derived seed is the §8.1 improvement: it makes the param case correct *by construction* rather than relying on the bb0 param-`SlotStore` to flip a blanket-false flag.
3. **Instrument** flag transitions across all blocks (`drop_elab.rs:635`): after each `SlotStore` to a flagged user slot → `flag := true` (covers re-initialization across loop iterations, e.g. the `drop_loop_reinit.gg` move-then-restore pattern); after each `MoveSlot` / `Memset` emptying a flagged slot → `flag := false`. The pass skips stores to the flag slots themselves — flag slots are values of `slot_to_flag`, never keys, so the `SlotStore` arm naturally ignores its own writes (`drop_elab.rs:652`).
4. **Replace** each remaining `NonZero` guard open on a flagged slot with a `SlotLoad` of the flag followed by `DropGuardOpen { kind: Bool }` (`drop_elab.rs:707`). Closes are matched to their opens by separate `flag_depth` / `passthrough_depth` counters so a replaced open's close becomes a flag-close and an untraceable open's close passes through unchanged.

A guard whose slot can't be traced via `val_to_slot` is left fully untouched (`drop_elab.rs:728`) — the pass only rewrites guards it can prove correspond to a known maybe-init slot.

## MoveSlot sweep (Phase 4) and follow-up DCE

After phases 1–3 consume them as dataflow signals, the `MoveSlot` annotations have no further purpose and are swept from every block (`elaborate_drops`, `src/lir/drop_elab.rs:835`). The orchestration in `elaborate_drops` (`drop_elab.rs:792`) runs all four phases per function and returns `ElabStats` (`drop_elab.rs:768`: `guards_eliminated`, `memsets_removed`, `flags_inserted`, `move_slots_removed`).

Back in `optimize_module`, if elaboration changed anything, a follow-up `eliminate_dead_code` pass runs over every function (`src/lir/optimize.rs:88`) to clean orphaned `SlotAddr` / `IConst` values left behind by deleted guards and memsets. Both before and after elaboration, `assert_module_valid` runs (`optimize.rs:73`, `optimize.rs:83`) so any shape regression is attributed to the right pass (Tier E §8.3's "validator after every pass").

## Sync-type release-on-drop and the `clone_fn`-gate

Everything above is the LIR-level *guard* machinery: it decides whether a
`DropIfAlive` runs, not whether one is emitted in the first place. For the
single-owner concurrency handles — `Mutex[T]`, `RWLock[T]` — the harder
question is *who* drops, and the answer is encoded one layer up, at GIR
type registration. This is the Core invariant #3 "register ownership at
the value's birth" rule applied to opaque resource handles, and it leans
on one keystone predicate; getting it wrong is a use-after-free, not a
leak.

### The owner frees; borrows do not

A `Mutex` / `RWLock` value is a `Copy`-semantics (`CopySemantics::Trivial`)
pointer to a heap-allocated lock. It is *single-owner*: the handle is
freed exactly once, by a unique `gorget_mutex_free` / `gorget_rwlock_free`,
with **no retain/release refcount**. So the producing local — the one
minted by `Mutex[int](v)` — must drop on scope exit, but every *borrow*
of it (a `Mutex &m` parameter, a match scrutinee, a collection read) must
**not**, or the caller's still-live handle is freed out from under it.

The owner-drop is registered by giving these handles a per-monomorph
`DropStrategy::Trivial("{mangled}__drop")`. The whitelist that does this
is the `base` match in `map_ast_type_mut` (`src/ir/lowering/types.rs:317-325`):
`Guard`, `Shared`, `Weak`, `Channel`, `ReadGuard`, `WriteGuard`, `Mutex`,
and `RWLock` all map to `{mangled}__drop`. The `__drop` wrapper itself is
emitted by the C backend (`src/backend/c_lir/helpers.rs`): for Mutex it is
`{type_name}__drop(...) { gorget_mutex_free(*self); }` (`helpers.rs:290`),
for RWLock `gorget_rwlock_free(*self)` (`helpers.rs:300`) — the runtime
unique-free entry points are `gorget_mutex_free` (`src/backend/c/runtime/mutex_runtime.c:37`)
and `gorget_rwlock_free` (`src/backend/c/runtime/sync_runtime.c:131`).

`RWLock` reaches registration by a second path: it is declared as a
`struct RWLock[T]` template (`lib/std/sync.gg`), so its ctor hits the
template-driven `monomorphize_struct` *before* `map_ast_type_mut` can stamp
the wrapper. The arm at `src/ir/lowering/generics/mod.rs:2392-2412` mints
the same `Trivial("{mangled}__drop")` there; without it RWLock fell into
the generic `compute_drop_strategy_for_struct` over its empty monomorph
fields → `DropStrategy::None`, and the handle leaked (Core #8 Inc-B). Mutex
has no template, so `map_ast_type_mut` alone covers it. The
`ensure_{mutex,rwlock}_type_def` helpers (`src/ir/lowering/exprs/type_reg.rs:170,193`)
register the same shape for the eager-registration paths.

### The keystone: `needs_param_drop`'s `clone_fn.is_some()` gate

Borrow-params are excluded from drop by a single predicate,
`TypeRegistry::needs_param_drop` (`src/ir/types.rs:531`). It returns true
**only** when all three hold (`types.rs:535-537`):

```rust
type_def.metadata.copy_semantics == CopySemantics::Trivial
    && type_def.metadata.drop_strategy != DropStrategy::None
    && type_def.metadata.clone_fn.is_some()
```

The first two clauses are satisfied by every sync handle (they are
`Trivial` and carry a `{mangled}__drop`). The third — `clone_fn.is_some()`
— is the discriminator. Single-owner handles keep **`clone_fn = None`**:
the `clone_fn` write in `map_ast_type_mut` (`src/ir/lowering/types.rs:347-353`)
sets the per-mono `{mangled}__clone` only for the per-mono `__clone`
family (`Shared`, `Weak`, `Channel` plus the Move-semantics guards
`Guard`, `ReadGuard`, `WriteGuard` — which `needs_param_drop` excludes
via its `Trivial` clause anyway) and
leaves Mutex/RWLock at the protocol default of `None`; the
`ensure_{mutex,rwlock}_type_def` docstrings (`type_reg.rs:166-169,189-192`)
and the `monomorphize_struct` RWLock arm (`generics/mod.rs:2403-2405`)
record the same.

`needs_param_drop` gates `register_param` (`src/ir/lowering/drops.rs:179-180`),
which is what registers a Copy-semantics *parameter* for a scope-exit
drop. With Mutex/RWLock at `clone_fn = None`, the gate returns false and
their borrow-params are never registered — only the producing local drops.

**Why the gate exists (the trap).** The naive fix is "the param has a
`__drop`, so register it for drop." That is strictly *worse* than the
leak it replaces: a `Mutex &m` param holds the same pointer the caller
still owns, so dropping it calls `gorget_mutex_free` on a live handle —
a heap **use-after-free**, and a double-free once the caller's own owner-drop
fires. The `clone_fn.is_some()` clause is exactly the test for "this
handle has a retain, so a param drop is a balanced release, not a free."

### Refcounted carriers, by contrast, *do* drop their params

`Channel`, `Shared`, `Weak` are also `Trivial` with a `{mangled}__drop`,
but they carry a real `clone_fn = Some("{mangled}__clone")` — a runtime
**retain**, balanced by a **release** in `__drop`. Their `__drop`
wrappers call `gorget_channel_release` / `gorget_shared_drop` /
`gorget_weak_drop` (`src/backend/c_lir/helpers.rs:241,250,279`), and
`{mangled}__clone` calls the matching retain (e.g. `gorget_channel_retain`,
`helpers.rs:240`). For these, `needs_param_drop` *does* return true: a
param is a held reference whose teardown must decrement the refcount, so
registering it for drop is correct (it balances the retain, never frees a
live handle). The `!`-move call path also consults `needs_param_drop` to
zero the caller slot on `!x` so a moved refcount isn't released twice
(`src/ir/lowering/exprs/calls.rs:371`). This is the only aspect of the
refcounted-carrier / spawn machinery that belongs to the drop model; the
rest of the async path is documented elsewhere. (For how `clone_fn` is
*consumed* on the clone side — `resource_clone_fn` picking the matching
`gorget_*_clone` / `{T}__clone` symbol at an `OpClone` materialization —
see Chapter 13's LIR-lowering note, `13-ownership-in-ir.md:542-556`.)

| Handle family | `copy_semantics` | `drop_strategy` | `clone_fn` | `needs_param_drop` | param drop is… |
|---|---|---|---|---|---|
| `Mutex`, `RWLock` (single-owner) | `Trivial` | `Trivial("{m}__drop")` → `gorget_*_free` | `None` | **false** | excluded — owner alone frees |
| `Channel`, `Shared`, `Weak` (refcounted) | `Trivial` | `Trivial("{m}__drop")` → release (`gorget_channel_release` / `gorget_shared_drop` / `gorget_weak_drop`) | `Some("{m}__clone")` | **true** | a balanced refcount release |

### The self-host mirror

So both compilers agree, the self-host lowerer classifies the same way.
`build_resource_metadata` (`tests/fixtures/self_host_lowerer/lir_lower.gg`)
tags `Mutex__`/`RWLock__` as **`CsResource`** (single-owner) at lines
360-384 and `Channel__`/`Shared__`/`Weak__` as **`CsRefCounted`** at lines 385-400,
routing each handle's drop fn by a typed `method_prefix` (`Some("gorget_mutex")`,
`Some("gorget_rwlock")`) read via `opaque_ptr_method_prefix`
(`lir_lower.gg:509`) — never a name-substring test (`is_refcounted_carrier`,
`lir_lower.gg:193`, reads the `CsRefCounted` metadata, matching Core #2 /
layering rule 2).

The owner-drop registration mirrors the GIR side. The ctor mints the
owning local as a `GtPtr(Handle)` slot, which makes the shared
`register_local_for_drop` (`tests/fixtures/self_host_lowerer/lower_drops.gg:235`)
a silent no-op — `is_droppable_type` rejects every `GtPtr` (a pointer is
normally non-owning). So a dedicated `register_owning_opaque_local`
(`lower_drops.gg:340`) handles these: it derefs the `GtPtr(inner)` wrapper,
gates on the local being `LoOwned` with a runtime-resource pointee (a
typed discriminator, not a name), and resolves the drop fn via the
`gorget_mutex` / `gorget_rwlock` method-prefix arm of `drop_fn_for_type`.

### R2 coupling: forcing both guard typedefs for `shared(rwlock)`

One RWLock-specific wrinkle worth a note. A `shared(rwlock)` facade lowers
`read`/`write` through 16-byte by-value guards (`gorget_read_guard_t` /
`gorget_write_guard_t`, returned via sret). If only a bare `GirType::Named`
were registered for `ReadGuard__T` / `WriteGuard__T`, the name never
reaches `module.structs`, the C backend emits no `typedef gorget_read_guard_t
ReadGuard__T;`, the slot falls back to `void*` (8 bytes), and the 16-byte
runtime write stack-buffer-overflows — silent UB until the RWLock owner-drop
perturbs the stack (Core #8 Inc-B). The `SharedStrategy::ArcRwLock` arm
(`src/ir/lowering/stmts/mod.rs:1524`) therefore force-emits *both* guard
TypeDefs via `ensure_rwlock_guard_type_def` (`src/ir/lowering/exprs/type_reg.rs:212`,
called at `stmts/mod.rs:1554-1557`) whenever the facade is used, regardless
of whether user code names a guard local. The typedef *body* is still
driven by the typed resources table, not a name match — registration only
ensures the name reaches `module.structs`.

## The optimizer fixpoint loop

> **Correction to the internals doc.** `unified-resource-model.md` §8.4 (line 761) claims the optimizer "runs three iterations and stops, regardless of whether it would have converged in four." **This is stale.** The current code runs a snapshot/change-counter fixpoint with a generous safety cap, exactly the convergence behavior §8.4 proposed as future work.

`optimize_function` (`src/lir/optimize.rs:104`) iterates the intra-block passes — `fold_constants`, `simplify_algebraic`, CSE, `fold_constant_branches`, `eliminate_dead_blocks`, `merge_linear_blocks`, `eliminate_dead_code`, `propagate_copies` — to a fixpoint. The loop bound is `const MAX_ITERS: usize = 32` (`optimize.rs:110`), a safety cap; the *primary* termination signal is the sum of every pass's "changes made" counter:

```rust
let progress = folded + algebraic + cse + branches + dead_blocks + merged + dead_insts + copies;
if progress == 0 {
    break; // fixpoint — no pass made any change this iteration
}
```
(`src/lir/optimize.rs:129-132`)

The comment at `optimize.rs:105` records the history: the bound was bumped from a tight `3` (which "silently stopped before convergence on some functions") to `32`. The change-counter approach is used rather than a `(blocks.len(), inst_count)` snapshot because passes like constant folding, algebraic simplification, and copy propagation rewrite *values* without adding or removing instructions — they'd be invisible to a structural snapshot, yet they unblock downstream DCE/CSE on the next iteration (`optimize.rs:99`).

Module-level passes (`eliminate_dead_functions`, `eliminate_dead_globals`) run once before the per-function loop (`optimize.rs:49-53`); they are whole-program reachability walks, not part of the per-function fixpoint.

## In the self-host

The self-host implements the same pass in `tests/fixtures/self_host_lowerer/drop_elab.gg`, a direct port of the Rust reference with the same four-phase structure: `forward_dataflow` → `elaborate_block` → `remove_companion_memsets` → `insert_drop_flags` → `sweep_move_slots`, driven by `elaborate_drops(LirModule &m)` (`drop_elab.gg:673`).

Notable divergences from Rust `gg`:

- **No packed bit-vector.** The self-host uses a dense `Vector[int]` of state codes (`IS_INITIALIZED=0`, `IS_UNINITIALIZED=1`, `IS_MAYBE_INITIALIZED=2`, `drop_elab.gg:48`) with an explicit `init_meet` (`drop_elab.gg:54`) rather than the bitwise-OR-is-meet trick. The lattice semantics are identical; the encoding is the readable one.
- **Recoverable `find_matching_close`.** Where Rust panics on an orphan open, the self-host returns `-1` and the caller skips that guard (`drop_elab.gg:279`, `drop_elab.gg:347`) — chosen during bring-up so an upstream pass bug doesn't abort the whole build.
- **`DropGuardKind` as an int.** Self-host encodes the kind as an integer where `kind == 0` is `Bool` and `kind > 0` is `NonZero` (the byte size lives elsewhere); see the `if k > 0` test at `drop_elab.gg:336` and the `IDropGuardOpen(0, v_flag)` Bool-replacement at `drop_elab.gg:579`.
- **A cheap pre-gate.** `function_needs_drop_elab` (`drop_elab.gg:649`) scans for any `IDropGuardOpen` / `IMoveSlot` and skips the whole pass for functions with none — avoiding the `n_blocks × n_slots` allocation across ~600 functions in `driver.gg`. The docstring (`drop_elab.gg:640`) records that without it stage1 ballooned to multi-GB RSS. The forward-dataflow loop also deliberately *borrows* the block into `compute_transfer` rather than value-binding it (`drop_elab.gg:226`), because a value-bind deep-clones the entire `insts` vector on every worklist re-visit — the same CoW-contract memory discipline as the Rust packed-state fix, surfaced one layer up.

To re-confirm current self-host parity for the lowerer, run the diagnostic comparison test and read its printed matched-count (the `*_comparison` tests are diagnostic-always-pass — a green run asserts nothing about parity):

```
cargo test --test integration lowerer_comparison -- --nocapture
```

The test driver dir is `tests/fixtures/self_host_lowerer/` (`tests/integration.rs:13390`); its `drop_elab.gg` is rebuilt by that test.
