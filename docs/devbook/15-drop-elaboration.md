# Chapter 15 — Drop elaboration & optimization

Drop elaboration is the LIR pass that turns conservatively-emitted, always-conditional resource drops into the cheapest correct runtime form: it *deletes* a drop the dataflow proves dead, *strips the guard* off a drop the dataflow proves live, and falls back to a stack-local `bool` drop flag only when initialization is genuinely path-dependent. It runs as the tail of the LIR optimizer. The pass lives in `src/lir/drop_elab.rs`; it is scheduled (after the main fixpoint optimizer and a validation pass) by `optimize_module` in `src/lir/optimize.rs:46`. This chapter also covers the surrounding LIR optimizer fixpoint loop, since the two ship together and the doc that motivated this chapter (the former `unified-resource-model.md` §8.4, now folded here) made a now-false claim about it.

## Why the pass exists

The conditional guard is materialized in two layers. The GIR drop accountant (`src/ir/lowering/drops.rs`) emits every resource-typed scope-exit drop as an **unconditional `DropIfAlive`** — it never decides statically whether the slot still owns a value, because the GIR-level `maybe_moved` analysis is *not* a sound CFG-aware move analysis (see `structural-guards.md` and `unified-resource-model.md` §8.1). The GIR→LIR lowering then turns each `DropIfAlive` into the actual guard shape: `Instruction::DropIfAlive` lowers via `lower_drop(place, bb, /*conditional=*/true)` (`src/lir/lower/insts.rs:1426`), and with `conditional == true` that materializes a `DropGuardOpen { kind: NonZero { size } }` … `DropGuardClose` pair wrapping the `T__drop` call — emitted at `src/lir/lower/drops.rs:142` and several sibling sites, always with `DropGuardKind::NonZero`. This is "defensive by default": GIR always emits the runtime check and lets a real dataflow pass downstream remove it.

The runtime check is not free. The C backend lowers `NonZero { size }` to an inline `memcmp` against a zeroed stack buffer (`src/backend/c_lir/mod.rs:2883`):

```c
{ char __dia_z[SIZE] = {0}; if (memcmp(ADDR, __dia_z, SIZE) != 0) { /* drop body */ } }
```

Drop elaboration replaces that `memcmp` with compile-time knowledge wherever a forward dataflow can supply it, and with a cheap `bool` load (`DropGuardKind::Bool`, lowered to `if (flag)` at `mod.rs:2880`) where it cannot.

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
