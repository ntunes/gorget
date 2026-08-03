# Clone Emission at Call Sites

Companion to [`copy-on-write.md`](copy-on-write.md) + [`ownership-ir.md`](ownership-ir.md). Distilled from two design scouts (2026-05-21) that audited Rust's implementation against the canonical CoW model and produced the empirical decision tree.

## TL;DR

- **Bare function call arguments propagate Ptr aliases at zero cost** — no clone, no move. They are NOT a CoW materialization point.
- **Clones happen only at the seven materialization points** ([`copy-on-write.md`](copy-on-write.md) Phase 3): assignment, mutating method, struct/enum init, collection put, return, move transfer, field store.
- **Sigils are asymmetric**: the callee's parameter declaration (`T` / `&T` / `!T`) names what it will do with the value; the caller's syntactic sigil (bare / `&` / `!`) is an opt-in annotation. They don't need to match.
- **The dispatch is callee-driven**: at every operand-read site, the operand mode is chosen by looking up the callee's parameter declaration, not by inspecting the caller's local liveness alone.
- **Borrow-after-move is forbidden by the borrow checker** at compile time. This means treating later borrow reads as live-range-extending (for move-eligibility) is correct — Rust does the same. Self-host's `liveness_operand_uses` conservatism is semantically right.

## The decision tree (canonical)

At an operand read of a resource-typed local in self-host's `op_consume`:

```text
operand_read(local_id, ConsumeKind):
  if ConsumeKind is NOT a consume kind:                       # CkCallArgBorrow, CkBinOpArg, CkFormatArg, CkMatchPtr, …
    return OpBorrow                                            # Ptr alias, zero cost

  # consume kind (CkAssign, CkReturn, CkFieldWrite, CkCallArgOwning):
  match source.ownership:
    LoBorrowed | LoView | LoParam (when callee bare T):
      return OpClone                                           # source doesn't own data
    LoOwned (or LoMaybeOwned):
      if is_last_use_at(local_id, position):
        return OpMove                                          # source dies; drop_elab elides guard
      else:
        return OpClone                                         # source still live; receiver needs own copy
```

Two axes are independent:
- **Consume vs borrow** — the position-class question, answered by `ConsumeKind`.
- **Const vs mut borrow** — handled below at operand-mode dispatch (`OpCopy` const, `OpBorrow` mut), driven by callee parameter declaration. Not in `ConsumeKind`.

## The seven materialization points

From `copy-on-write.md` Phase 3, the only positions where a borrowed value must become owned:

1. **Assignment**: `x = expr` where x is borrowed
2. **Mutating method**: `x.push(val)` where x is borrowed
3. **Struct/enum init**: `Foo(x)` where x is borrowed
4. **Collection put**: `v.push(x)` where x is borrowed
5. **Return**: `return x` where x is borrowed
6. **Move transfer**: `consume(!x)` where x is borrowed
7. **Field store**: `self.field = x` where x is borrowed

**Regular function call arguments are NOT in this list.** A bare `f(x)` propagates the Ptr alias to the callee at zero cost. This is the asymmetry that distinguishes consume-position args from borrow-position args.

## Sigil asymmetry — the rule

| Caller writes | Callee declares | Behavior |
|---|---|---|
| bare `f(x)` | bare `T` | borrow propagates; no clone, no move |
| bare `f(x)` | `&T` | mutable borrow propagates; no clone, no move |
| bare `f(x)` | `!T` (consume) | callee takes ownership; safety checker enforces caller doesn't use `x` after the call |
| `f(&x)` | bare `T` or `&T` | mutable borrow propagates |
| `f(&x)` | `!T` | legal; mutable borrow + consume (over-restrictive caller annotation, safe) |
| `f(!x)` | bare `T` or `&T` | over-eager move on caller side (safe; source becomes dead even if callee only borrows) |
| `f(!x)` | `!T` | move transfer; canonical consume pattern |

The compiler does NOT require sigil parity. The callee's declaration drives the per-arg semantics; the caller's sigil is an optimization annotation.

## Rust implementation (file:line citations)

The decision tree above is what Rust implements. Key dispatch sites:

- `src/ir/lowering/exprs/calls.rs:17-251` — `lower_call_arg`: regular call-arg lowering. For bare args, emits `emit_borrow` (const Ptr) or `emit_borrow_mut` (mut Ptr) based on the callee parameter's ownership declaration via `ctx.fn_param_ownerships`. **Does NOT route through `ensure_owned_at_consuming_arg`** for bare call args.
- `src/ir/lowering/exprs/calls.rs:~581` — `Box()` constructor: routes through `ensure_owned_at_consuming_arg` (consuming position).
- `src/ir/lowering/exprs/methods.rs:~116` — `Box.new()`: same as Box().
- `src/ir/lowering/exprs/methods.rs:~1757-1761` — method-receiver dispatch: reads `ctx.fn_param_ownerships[name][0]` (the `self` declaration). If `Ownership::Move`, post-call code emits `MoveZero` for the receiver local. Otherwise the receiver stays live; no post-call zero.
- `src/ir/lowering/exprs/methods.rs:~1887` — `push/put/set/add/extend` mutating methods: route through `ensure_owned_at_consuming_arg` for value args (consuming positions).
- `src/ir/lowering/stmts/assigns.rs:~636/~822/~839` — assignments to owned destinations: same.
- `src/ir/lowering/context.rs:~1014` — `is_last_use_at(name, span)`: AST-level liveness query.
- `src/ir/lowering/context.rs:~1033` — `ensure_owned_at_boundary`: skips non-consume positions.
- `src/ir/lowering/context.rs:~1411-1993` — `ensure_owned_at_consuming_arg`: the clone-vs-move decision at consuming positions.
- `src/ir/lowering/liveness.rs` — AST liveness pass. Counts every identifier read as a "use" regardless of mode (borrow, consume, etc.). This is correct because **the borrow checker forbids move-after-borrow at compile time** (`src/semantic/borrow_check.rs`); the lowering layer can assume the pattern is impossible.
- `src/ir/instructions.rs:92-107` — `ReadMode` enum: Copy (const borrow), Borrow (mut borrow), Move (transfer), Clone (deep copy).
- `src/lir/drop_elab.rs` — LIR forward dataflow: proves slot init-state, elides scope-exit drop guards after MoveSlot. Makes moves "free" — the source slot retains bytes (no zero) but drop is suppressed.

## Self-host implementation map (post-CkCallArg refinement 2026-05-21)

The relevant self-host code:

- `tests/fixtures/self_host_lowerer/gir.gg` — `ConsumeKind` enum (post-refinement: 8 variants):
  - **Consume**: `CkAssign`, `CkReturn`, `CkCallArgOwning`, `CkFieldWrite`
  - **Borrow**: `CkCallArgBorrow`, `CkBinOpArg`, `CkFormatArg`, `CkMatchPtr`
- `tests/fixtures/self_host_lowerer/gir.gg` — `OperandMode` enum: `OpMove`, `OpClone`, `OpCopy`, `OpBorrow`
- `tests/fixtures/self_host_lowerer/gir.gg` — `LocalOwnership` enum: `LoOwned`, `LoBorrowed`, `LoView`, `LoParam`, `LoMaybeOwned`
- `tests/fixtures/self_host_lowerer/gir.gg` — `GirModule.fn_move_params: Dict[String, Vector[bool]]` — per-callee per-parameter "is this `!T`" flag, populated at function-registration time.
- `tests/fixtures/self_host_lowerer/lower.gg::op_consume` — operand-mode dispatcher; takes `ConsumeKind`.
- `tests/fixtures/self_host_lowerer/lower.gg::classify_call_arg(&gmod, fn_name, arg_idx)` — the canonical lookup helper; returns `CkCallArgOwning` if `fn_move_params[fn_name][arg_idx]` is true, else `CkCallArgBorrow`.
- `tests/fixtures/self_host_lowerer/lower.gg::compute_liveness` — backward CFG dataflow pass (Phase 1); produces `last_use_of_op[(b, i, op_idx)]` decisions.
- `tests/fixtures/self_host_lowerer/lower.gg::wire_liveness_into_modes` — post-pass (Path A Phase 2a) that consumes `last_use_of_op` and rewrites operand modes.
- `tests/fixtures/self_host_lowerer/drop_elab.gg` — LIR drop elaboration pass (Phase 2c COMMIT 1, shipped 2026-05-21). Forward dataflow on slot init-state.

## Known Path A bugs and their fixes

The CkCallArg refinement (commit `41c74285`, 2026-05-21) split `CkCallArg` into `CkCallArgBorrow` / `CkCallArgOwning` via callee-signature lookup. That closed one class of coarse classification — but two more were identified in the same audit:

### Hardcoded `OpMove(LoOwned source)` without callee-signature consultation

Three sites in `lower.gg` (as of 2026-05-21) hardcode `OpMove` for LoOwned sources, ignoring the callee's parameter declaration. They mirror the CkCallArg bug at non-arg positions:

| Site | File:line | What it dispatches | Fix |
|---|---|---|---|
| Method-call receiver | `lower.gg:3674-3678` | Receiver of `x.method(...)` | Look up method's `self` param (idx 0) in `fn_move_params` |
| Field-access receiver | `lower.gg:~3855` | Base of `x.field` (lowers to field-getter call) | Look up field-getter's param 0 in `fn_move_params` |
| User-fn bare-param else branch | `lower.gg:~4282` | Non-borrow branch of `f(arg)` lowering | Use `classify_call_arg` |

All three accept the same `classify_call_arg`-style lookup. Receiver dispatch fix is in progress as of 2026-05-21.

### Liveness conservatism: NOT a bug

`lower.gg::liveness_operand_uses` (~lines 963-978) treats OpBorrow reads as live-range-extending. This is **semantically correct** — matches Rust's `liveness.rs`. Do NOT refine.

Reasoning: Rust's borrow checker forbids move-after-borrow at compile time, but it does NOT eliminate the liveness-extension question. A consume at position P that has any later use (borrow or consume) is correctly NOT a last-use. Treating borrow reads as live-range-extending is therefore the right default. Self-host's behavior matches Rust's.

## Empirical baseline

`/tmp/stage1_probe.c` (Rust gg compiling `tests/fixtures/self_host_lowerer/driver.gg`, 550K lines):
- **338 direct `gorget_*_clone(` calls** — almost entirely inside `<Type>__clone` body emissions (structural clones for recursive struct/enum field cloning)
- **1607 `<Type>__clone(` calls** — user-type clones at the structural layer
- **Zero clones at regular bare function-call arguments** — confirmed via sampled site analysis

Self-host operational OpClone count timeline (GIR layer, on the same input):
- Pre-Path A (the historical, since-retired `tighten_owned_operand_modes` baseline): 0 (OpClone was no-op ISlotLoad placeholder)
- Path A first attempt (naive emission flip): ~51,000 predicted (DISPROVEN by /tmp/stage1_probe.c sampling — Rust emits 26× fewer)
- Path A Phase 2a + 2b (label correctness only, no emission flip): 19,590 GIR-level OpClone (no runtime effect)
- After CkCallArg refinement (commit `41c74285`): 15,330 GIR-level OpClone (-22%)
- After 3-site receiver-dispatch fix (in progress): conservative estimate ≤8,000
- After Phase 2c COMMIT 2 emission flip (deferred until residual is sub-1K): runtime clone calls become real
- Phase 2c COMMITs 3+4 ship together: drop_elab elides redundant drop guards; DropElaborator emission fires
- Target end-state: ~338 stage-1.c runtime clones (Rust-parity)

## Materialization-point taxonomy: docs vs implementation

The docs list seven points. The Rust implementation is more granular (per-site routing) but produces the same behavior at every required materialization point. The presentational difference doesn't matter for correctness — both arrive at the same emissions.

Self-host's `ConsumeKind` enum is a typed materialization-point taxonomy at the GIR layer. Each variant maps to one or more of the seven points:

| ConsumeKind variant | Maps to point(s) |
|---|---|
| `CkAssign` | 1 (assignment), 2 (mutating method) |
| `CkReturn` | 5 (return) |
| `CkCallArgOwning` | 4 (collection put when callee declares `!T`), 6 (move transfer) |
| `CkFieldWrite` | 3 (struct/enum init via field store), 7 (field store) |
| `CkCallArgBorrow` | NOT a materialization point — Ptr alias propagation |
| `CkBinOpArg`, `CkFormatArg`, `CkMatchPtr` | NOT materialization points — all borrow |

## Reference scout reports

The findings here distill two design scouts in agent task `task-20` and `task-22` (2026-05-21):
- **CoW-design + Rust-emission scout** (the conformance audit): verified Rust matches the docs; built the canonical decision tree; sampled 10+ stage1.c clone sites.
- **Path A semantics scout** (the borrow-after-move + receiver dispatch audit): verified borrow-after-move is forbidden by the borrow checker; identified the 3 hardcoded `OpMove(LoOwned)` sites; recommended skipping liveness refinement (Q4) as not needed.

The scout raw outputs are session-ephemeral. Future contributors should refer to this doc rather than re-derive.

## See also

- [`copy-on-write.md`](copy-on-write.md) — full CoW spec; the seven materialization points (Phase 3)
- [`ownership-ir.md`](ownership-ir.md) — `LocalOwnership` enum + ownership-state propagation
- [`safety-checker.md`](safety-checker.md) — safety-pass enforcement of borrow/move rules
- [`layering-discipline.md`](layering-discipline.md) — the layering rules cited throughout
- [`CLAUDE.md`](../../CLAUDE.md) "Ownership at Consuming Positions" — the user-facing compiler contract
