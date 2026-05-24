# Consumer Audit (Phase A.4 checklist)

**Status:** IN PROGRESS — started 2026-05-24. Aggregates three parallel static audits
(self-host LIR layer, self-host `lower.gg`, Rust reference impl) plus parent
pre-synthesis findings from the canonical drop/CoW docs.

This file is the Phase A.4 output. Phase A.2's dynamic-probe results go in
`docs/plans/cascade_probe_results.md` (separate file).

---

## Parent pre-synthesis findings (from doc reading, pre-agent-merge)

### PF-01 — **Phase D.3 contradicts the defensive-by-default drop contract (HIGH)**

**Source:** `docs/internals/structural-guards.md:112-120` ("Drop-emission contract:
defensive-by-default", Snag #30, 2026-05-10) + `docs/internals/self-host-resource-model.md`
§6.3.

The plan's **Phase D.3** says: walk the drop queue, *check each entry's `maybe_moved`
flag, and only splice `GIDropIfAlive` if not moved.* That gates emission on `maybe_moved`.

The established contract says the **opposite**:

> The GIR drop accountant (`src/ir/lowering/drops.rs`) emits `DropIfAlive`
> **unconditionally** for every resource-typed scope-exit drop, regardless of the
> local's `maybe_moved` flag. The LIR `drop_elab` pass then statically elides the
> runtime drop-flag check when slot init is provably unconditional... The
> `DropEntry::maybe_moved` sidecar is preserved... but it's **no longer load-bearing
> for soundness**.

**Why the contract exists (Snag #30 repro):** `maybe_moved` tracking across nested
matches with early-return paths produces *false negatives* — a local marked moved in
one match's Some arm appears not-moved at a later match's None-arm
`emit_early_exit_drops` callsite → unconditional `Drop` → double-free of the heap
aliased between the move-zero'd source and the move'd dst. `lower_match_expr` doesn't
`snapshot/restore/union_moved` between arms the way `lower_match_stmt` does.

**Consequence for the plan:**
- Phase D.3's `maybe_moved` gate is both **redundant** (`GIDropIfAlive` already carries
  a runtime aliveness check) and **dangerous** (reintroduces the Snag #30 double-free).
- Phase D becomes **simpler**: emit `GIDropIfAlive` unconditionally for every
  resource-typed local at scope exit (early-exit skips only the explicitly-excluded
  local + non-resource types). No `maybe_moved` consultation. No "snapshot scope state
  / reconstruct after pop" gymnastics — the plan's D.3 "Gotcha" evaporates.
- This is the CLAUDE.md "fix complexity = signal of wrong layer" heuristic firing:
  the queue+flush+maybe_moved-reconstruct complexity was a tell. The right layer for
  elision is LIR `drop_elab`, not GIR emission.

**Open dependency to confirm:**
1. Self-host **must have** a working `drop_elab` elision pass for the unconditional
   contract to be free at runtime. `drop_elab.gg` exists (Phase 2c COMMIT 1, shipped
   2026-05-21 per clone-emission-at-calls.md). NEEDS: confirm self-host `GIDropIfAlive`
   actually carries a runtime drop-flag check that `drop_elab` can elide (vs. an
   unconditional drop with a misleading name). → flagged to lower.gg audit agent.
2. Confirm current Rust `drops.rs` still emits unconditionally (vs. the plan's
   maybe_moved framing). → flagged to Rust reference audit agent.

**Still-needed: queue+flush remains.** The plan's reasoning for queue+flush (drops are
recorded inline during `lower_stmt`, before `wire_liveness_into_modes` runs, so
ordering/placement needs deferral) is independent of the maybe_moved question. We still
queue the drop sites and flush them; we just don't *gate* the flush on `maybe_moved`.

**CONFIRMED (2026-05-24):** `lower.gg:767-779` is the authoritative docstring and states
the unconditional contract explicitly: "Rust's DropElaborator (`drops.rs:506,542`) also
emits DropIfAlive unconditionally post-Snag #30; we follow the same defensive shape. The
downstream LIR `drop_elab` pass statically elides the memcmp when slot init is provably
unconditional." Note `lower.gg:169` (the stub-description comment, "gated on not
maybe_moved") **contradicts its own line-767 docstring** — the plan's D.3 inherited the
wrong one. PF-01 dependency #1 RESOLVED yes: self-host `GIDropIfAlive` lowers (via
`lir_lower.gg:2989-3015`) to an `IDropGuardOpen(byte_size+1)/IDropGuardClose` pair that
C-codegen materialises as `if (memcmp(&slot, zeros, size) != 0) <drop>`
(`lir_codegen.gg:3592-3598`, `__gorget_drop_if_alive_open__SIZE`/`_close`). That memcmp
IS the elidable runtime check.

### PF-04 — The real blocker is the MoveZero→IMoveSlot→memset chain, not the drop gate (HIGH)

**Source:** `lower.gg:800-834` (docstring dated 2026-05-20, **now partially stale**).

`emit_scope_drops` emission is currently a deliberate **no-op** (`lower.gg:838-843`), and
the docstring gives the two reasons it was disabled:

1. `lir_lower.gg`'s OpMove handler does NOT emit `IMoveSlot` at the load site → moved-out
   slots retain the moved value's bytes → the scope-exit `IDropGuardOpen` memcmp sees
   non-zero bytes and runs the drop, double-freeing data the receiver/return-place also
   owns (glibc double-free).
2. Even if (1) were fixed, `lir_codegen.gg` emits `IMoveSlot` as a `/* move_slot */;`
   no-op (~line 3120), AND "the self-host doesn't have `drop_elab`."

**STALENESS FLAG:** reason (2)'s "self-host doesn't have drop_elab" is **outdated** —
`drop_elab.gg` shipped 2026-05-21 (Phase 2c COMMIT 1), one day *after* this docstring.
So the current true state is *between* the docstring (no drop_elab) and the plan (assumes
drop_elab works).

**PF-04 RESOLUTION (review agent, 2026-05-24):** `drop_elab.gg` is a **full working 710-line
port** of Rust's `drop_elab.rs`, wired into the pipeline at `driver.gg:79`. It maps
`IMoveSlot(slot) → IS_UNINITIALIZED` (`drop_elab.gg:104-106`); Phase 1 deletes guard+drop for
Uninitialized slots; Phase 2 strips guards for Initialized; **Phase 3 `insert_drop_flags`
(`drop_elab.gg:444+`) replaces MaybeInit memcmp guards with a stack bool drop-flag,
instrumented on every `MoveSlot`/`SlotStore`/`Memset`**. So a moved-out slot is detected
**statically via the IMoveSlot annotation**, NOT via runtime bytes. **Conclusion: the
codegen `memset` (a-4) has no consumer if drop_elab runs correctly — it is likely dead work,
not a fix.** Pre-emptively emitting it would (i) be redundant and (ii) mask a drop_elab
dataflow gap. The stale comments at `lir_lower.gg:3164` and `lower.gg:811-818` are FALSE
today. **Revised action:** verify drop_elab's IMoveSlot dataflow reaches the
return-MoveZero'd slots during the probe; add the codegen memset ONLY if a residual
Initialized-but-actually-moved misclassification is observed.

**Implication:** the chain that must work end-to-end is:
`lower_return`/consume-site emits **GIMoveZero(local)** (Phase E) → `lir_lower` lowers it
to **IMoveSlot** → `lir_codegen` emits a **real memset** (or `drop_elab` proves the slot
dead and elides the exit memcmp). All three links must be live. The plan's E.1 covers
"which local gets GIMoveZero'd"; the consumer audit (A) must cover the IMoveSlot lowering
+ codegen links. If any link is a no-op, drops double-free. This is the mechanism behind
all 9 E.1 failures, stated precisely.

### PF-02 — E.1 MoveZero is the slot-death mechanism, not drop suppression

Consistent with PF-01: E.1's `MoveZero` on the returned local doesn't *suppress* a drop
emission — it makes the moved-from slot observably **dead** so the unconditionally-emitted
`GIDropIfAlive` skips it at runtime (and `drop_elab` can prove the skip statically). The
architecture is: `MoveZero` (slot dead) + unconditional `GIDropIfAlive` (runtime check) +
`drop_elab` (static elision) = correct, no double-free, no leak. The seven `lower_return`
concerns in Phase E are about *which* local gets MoveZero'd and how the return value is
wrapped/cloned — not about deciding whether to emit the exit drop.

### PF-03 — Canonical references for the synthesis

- Seven materialization points: `copy-on-write.md` Phase 3 / `clone-emission-at-calls.md`.
  Bare call args are NOT a materialization point (Ptr alias, zero cost). Clones happen at:
  assignment, mutating method, struct/enum init, collection put, return, move transfer,
  field store.
- Self-host operand dispatch: `lower.gg::op_consume(ConsumeKind)`; ConsumeKind has 8
  variants (4 consume: CkAssign/CkReturn/CkCallArgOwning/CkFieldWrite; 4 borrow:
  CkCallArgBorrow/CkBinOpArg/CkFormatArg/CkMatchPtr). OperandMode: OpMove/OpClone/OpCopy/OpBorrow.
- A borrow operand must lower to an **address** (ISlotAddr), not a loaded value (ISlotLoad).
  Seed bug: self-host OpBorrow emits ISlotLoad at lir_lower.gg:2482.

---

### PF-05 — **The queue+flush infrastructure may be UNNECESSARY (HIGH — revisits a "DECIDED" plan choice)**

**Source:** PF-01 + agent-2 (lower.gg) report, which surfaced the contradictory docstring
at `lower.gg:82-94`.

The plan's Phase D states: *"DECIDED: queue+flush (forced by lower.gg:82-88 docstring —
`maybe_moved` is universally false at inline-emit time)."* That decision rests entirely on
needing to **defer reading `maybe_moved`** until after `wire_liveness_into_modes` runs.

But PF-01 establishes we **must not gate on `maybe_moved` at all** (unconditional contract).
Remove the `maybe_moved` dependency and the *sole stated reason* for queue+flush evaporates.
GIDropIfAlive can be emitted **directly/inline at scope-pop time** — which is exactly the
shape `pop_drop_scope` (`lower.gg:851`) already supports ("emit its drops at the current
block IF the terminator is still GTNone").

Corroborating evidence the queue is the wrong path: agent 2 reports `flush_drop_queue`'s
own docstring (`lower.gg:1092-1095`) flags that **scopes are already popped by flush time**,
so reading `maybe_moved` via a live scope index is broken and would need pre-flattening into
`DropEmission`. That's the plan's D.3 "Gotcha" — and it's a self-inflicted wound from the
queue design. Drop the queue, drop the gotcha.

**Hypothesis (HIGH confidence, pending Rust confirmation):** Phase D collapses from
"implement queue recorders + flush + snapshot reconstruction" to "emit `GIDropIfAlive`
directly in `emit_scope_drops` / `emit_drops_for_early_exit`." The `DropEmission` struct +
`drop_queue` + `flush_drop_queue` hooks become dead code to retire (Phase F.2).

**MUST CONFIRM:** does Rust `drops.rs` emit drops **inline at scope exit** (direct), or defer
via a worklist? `lower.gg:774` cites "Rust's DropElaborator (drops.rs:506,542) emits
DropIfAlive unconditionally" — if that emission is inline at scope-exit, direct emission is
confirmed correct. → flagged to Rust reference audit agent (agent 3, still running).

---

## Agent 2 findings — self-host `lower.gg`

### lower_return (Phase E target) — `case SReturn` at lower.gg:5291-5305

Current shape is the bare minimum: `lower_expr → GIAssign(0, op_consume(val, CkReturn)) →
emit_drops_for_early_exit (NO-OP) → set_terminator(GTReturn(op_consume(0, CkReturn)))`.
All 7 Phase-E concerns grep-verified absent by name (`owning_param_returned`,
`clone_resource_global_ref`, `is_explicit_result_variant`, `maybe_auto_propagate`,
`enum_init`/`GIEnumInit`, `current_throws_result_type` → 0 hits).

| # | Concern | Rust ref (stmts/mod.rs) | Self-host status |
|---|---------|-------------------------|------------------|
| 1 | owning_param_returned MoveZero | 1536-1547 detect, 1946-1948 zero | **ABSENT** |
| 2 | clone_resource_global_ref | 1552 | **ABSENT** |
| 3 | is_explicit_result_variant | 1518-1522, 1561-1568 | **ABSENT** (return *type* widened to `Result__T__E` at lower.gg:6501-6508, but value path unaware) |
| 4 | maybe_auto_propagate | 1572-1576 | **ABSENT** |
| 5 | returned_local + MoveZero (headline) | 1579-1584, 1812-1889 | **ABSENT at SReturn**; only emergent via `wire_liveness_into_modes` (lower.gg:2199,2216) when op_consume→OpMove survives last-use demotion — indirect, not deterministic return-place exclusion |
| 6 | Ok-wrap for non-explicit return | 1654-1677 | **ABSENT** — possible live bug OR moot (see gap #1 below) |
| 7 | Ptr(T)→T auto-clone | 1716-1754 | **PARTIAL** — resource-clone half covered generically by `op_consume`→`decide_ptr_consume` (lower.gg:1227,1378-1396); non-resource deref-load + `set_ref` Ptr-propagation fallback ABSENT |

**Bottom line:** 4/7 fully absent, 1 emergent-elsewhere, 1 partial. Headline (5) + throws
machinery (3,4,6) = bulk of the work.

### Drop stubs + infra
- `emit_scope_drops` (838-843): no-op (`int _len = entries.len()`).
- `emit_drops_for_early_exit` (1051-1071): walks scopes, honors `exclude_local`, inner action no-op.
- `flush_drop_queue` (1096-1105): no-op; called post-`wire_liveness_into_modes` at 6630 (fns), 6861 (closures).
- `DropEmission` (121-126): `{local_id, target_block, insert_at_inst_idx, dse_scope_idx, entry_idx}`.
- `DropEntry` (57-60): `{local_id, type_id, maybe_moved}`; `maybe_moved` set ONLY by `mark_local_moved` (948-961).
- `wire_liveness_into_modes` (2175-2220): runs in lower_function@6621 (closures@6856) AFTER inline lowering + `pop_drop_scope`, BEFORE `flush_drop_queue`. For every operand finalized OpMove, emits trailing `GIMoveZero(lid)` (2199,2216) + `mark_local_moved` (2283).

### match_scrutinee_ptr (5829-5836)
Uses `CkAssign` (a consume kind) → forces OpMove/OpClone on resource scrutinees. But it
only takes the *address* of `tmp` for `__field_read_*`/`__tag_read` plumbing (which use
`CkMatchPtr` = pure borrow). **Fix: `CkAssign` → `CkMatchPtr`** (is_consume_kind=false →
unconditional OpBorrow). Confirms plan seed C-02.

### is_droppable_type (901-927)
EXCLUDES user resource types (returns true ONLY for `GtNamed` with
`resource_meta_for(name) = Some(_)`, i.e. runtime types). Checks `resource_meta_for` only,
NOT `gmod.resource_types` — **contradicting its own docstring@783**. Phase C.2 flips this.

### Other notable op_consume sites
- **E10 (smell): operator-overload sites** (3761,3775,3788,3829,3853) hardcode `OpMove(lhs)`
  regardless of liveness, leaning on wire-pass demotion. "Default to Move, fix downstream"
  shape the layering doc warns about. Review for double-use.
- E6 (array push, 4687): element `OpMove(el)` not MoveZero-guarded at GIR.

### Phase F.2 retire targets (workarounds, all band-aids for missing drop+move machinery)
- **F1: `add_local_inheriting`** (406-434) + **F2: `inherit_borrow_from`** (472-509) +
  **F3: 5 call sites** (4265,4548,4763,5858,6037) — reader-side borrow-tag propagation that
  exists because the writer side (drop emission) can't distinguish owner from alias. The
  CLAUDE.md "fix complexity = wrong layer" symptom; cured by real drop+move at LIR.
- **F4** (5191-5217): deliberate leak-over-double-free in SVarDecl BorrowAlias.
- **F5** (8088-8123, "Fix C 2026-05-22"): transitive `is_transitively_resource` fixpoint —
  likely a *genuine* correctness fix (forward-ref resource detection), keep unless replaced
  by typed upstream metadata.
- **F6** (8177-8189, "Fix D 2026-05-22"): ctor registration in `fn_move_params` with
  per-field resource flags — arguably *correct* layering (field-init is a consume position).

### Agent-2 gaps (need dynamic probe / cross-file confirmation)
1. **Concern #6 (Ok-wrap): live bug or moot?** Return type widened to `Result__T__E` but
   value not wrapped. Either fixtures always write explicit `Ok(...)`, or bare `return x` in
   throws fns mis-compiles. Needs test/grep evidence.
2. E10 `OpMove(lhs)` operator sites — actual runtime double-use depends on wire-pass demotion reliability.
3. Cross-file: agent 2's §C cites `lir_lower.gg:3009` + `lir_codegen.gg:3120` (the OpMove/IMoveSlot
   no-op blockers) as lower.gg's *claims* — to be confirmed by agent 1.

---

## Agent 1 findings — self-host LIR layer (lir_lower.gg + lir_codegen.gg)

**Seed reframing — L1 is a red herring.** `lir_lower.gg:2482` (OpBorrow→ISlotLoad) confirmed
at the exact line, BUT `lower_operand` is the bare-read *fallback*; every real
address-needing consumer (GIAssign@2550, call-arg ptr arms@2754/3017) re-discriminates
OpBorrow and emits `ISlotAddr` itself. So L1 never reaches an address-needing position → not
a cascade source. The plan's headline seed was misdiagnosed.

**The real double-free bugs (cluster a):**
- **L2** (`lir_lower.gg:3146-3158`, GIDrop): takes `ISlotAddr(slot)` → `T__drop(&slot)`
  unconditionally. When the slot is Ptr-typed (A.2/B.1 retyped field-read & borrow-source
  locals to *hold a pointer*), `&slot` is a pointer-to-pointer; `T__drop` reads the 8-byte
  pointer as the struct head → double-frees the pointee the real owner also frees. **This is
  the field-read-aliasing double-free.**
- **L3** (`lir_lower.gg:3169-3186`, GIDropIfAlive): same `&slot` bug, wrapped in the
  `IDropGuardOpen(size+1)/IDropGuardClose` memcmp gate — and the gate is *defeated* by C2.
- **C2** (`lir_codegen.gg:3132-3133`, IMoveSlot): emits `/* move_slot */;` — a **no-op**.
  Moved-out slots keep their bytes → the L3 memcmp gate sees non-zero → drops data the
  receiver/return-place already owns → double-free. **The central E.1 blocker.**
- **B1** (`lir_codegen.gg:3528,3538`, `__field_read_` cast): `((c_struct*)base_val)->field`.
  Seed confirmed; *mitigated* (`needs_ptr_arg` routes base through ptr arms so it IS a
  pointer). The cast is safe; the hazard is upstream — the field-read instruction carries no
  `returns_view`/`is_borrow` typed flag, so downstream drop-registration can't tell the
  `dst` is a borrow alias vs an owner. **Layering rule-2 hole = the root of L2.**

**The leak bugs (cluster b — independent, kills the OOM):**
- **D1** (`lir_lower.gg:2839-2844`, Vector/Deque/Channel/Shared ctor): emits bare
  `gorget_array_new(elem_size)`, no elem_drop. `Vector[T]` of user resources never drops
  elements on overwrite/free → the ~13 GB `elaborate_drops` OOM on driver.gg.
- **D2** (`lir_lower.gg:2800-2824`, Dict/Map ctor): bare `gorget_map_new`, no `val_drop`.
- **D3** (`lir_lower.gg:2825-2834`, Set ctor): bare `gorget_set_new`.
- **D4** (`lir_codegen.gg:1246-1248`, `emit_dict_ctor_wiring`): **the one elem-drop path that
  exists** — wires `key_drop`/`key_clone` fn-pointers for user-struct Dict/Set *keys*. This
  is the correct post-construction-store pattern (matches Rust §3); it just needs
  generalizing to Vector elems (D1) and Dict values (D2).

**Drop-body generation (mostly fine):**
- **E1** (`lir_lower.gg:3439,3475`, `__imported_type__` skip): **CORRECT** — skips imported
  types whose drop/clone the Rust preamble provides as `static inline` (re-emitting
  double-defines). The "0 user-type drops" is a *wiring/firing* problem (D-class), NOT a
  generation problem. **Do NOT remove this skip (Phase C.1 is misframed).** Generator works
  (Token__clone confirmed emitted).
- **F1** (`lir_codegen.gg:4517` vs `4609/4754`): struct drops take `cname*`, enum/unified take
  `void*` — ABI inconsistency, both pointers so works, minor cleanup.
- **F2** (`lir_codegen.gg:4609-4652`, enum drops): Box-in-payload variant emits `free(access)`
  without recursively dropping the boxed T's interior resources → leak. Mirror Rust Snag #13
  `box_inner_type` typed field.

## Agent 3 findings — Rust reference spec (the port blueprints)

**lower_return** = `src/ir/lowering/stmts/mod.rs:1508-1959` (NOT ...1750 — plan's range
drifted). Order of the 7 concerns + exact lines: see the agent-2 table above for the
self-host status; Rust line cites: (1) detect 1536-1547 / act 1942-1948; (2) call 1552 /
body 34-79; (3) 1518-1522 + 1561-1568; (4) 1572-1576; (5) detect 1578-1584 / zero
1604-1608,1674-1676,1812-1829,1851-1889 (+ tuple-element-sources walk); (6) 1612-1677; (7)
1716-1754. **Adjacent concerns the port must NOT miss:** String-return clone (1685-1715),
`try_lift_option_ref` (1755-1793), string-view materialize in returned collections
(1908-1937), `move_override_params` (1896-1907), early-exit drops (1952).

**Owning-param drop nuance:** `drop_place_for` emits `Place {local, projections:[Deref]}` for
owning-param entries so `*v` goes through addr-load and bypasses the empty-projection
`is_pure_borrow` Nop short-circuit.

**Drop scope mgmt** = `src/ir/lowering/drops.rs:1-562`. `entries: Vec<DropEntry>` per scope.
`pop_scope`→`emit_scope_drops_ordered` (borrow-aware Kahn topo so borrowers drop before
sources); `pop_scope_no_emit` (when early-exit already emitted); `emit_early_exit_drops`
(walk innermost→target, `exclude` the returned local). **Emission is unconditional
`drop_if_alive` with `maybe_moved`/`owning_param` discarded (504-505,540-541).**

**Allocator dispatch** = `src/lir/lower/insts.rs`. ONE allocator; post-construction fn-ptr
stores at fixed byte offsets (Array elem_drop=40/clone=48/materialize=56; Map
val_drop=104/val_clone=112/key_drop=120/key_clone=128/val_materialize=136; Set
key_drop=120/key_clone=128). Drop fn from `elem_drop_fn_for_type`
(`src/lir/lower/types.rs:103-128`) reading `metadata.drop_strategy`.

**Operand lowering** = `src/lir/lower/operands.rs:7-95`. The ISlotAddr-vs-ISlotLoad rule
(operands.rs:30-34): `SlotKind::BorrowedPtr` or `PtrTo(GorgetString)` w/o leading Deref →
`SlotLoad{ty:Ptr}` (read pointer directly); else `SlotAddr{slot}`. **Only Move is special at
LIR** (`SlotStore{is_move}`); Clone is materialized at GIR (explicit clone_fn call); Borrow
is just `lower_place_addr`.

**Match scrutinee** = `patterns.rs:174-423`. Rust **borrows** (Copy non-resource / Move
owned-last-use / **Borrow otherwise**), never clones. Snag #41 direct-source path for
consuming arms on non-discriminator aggregates: alias source local directly, zero payload
field in-place. Confirms the `match_scrutinee_ptr` fix direction.

### Plan-level discrepancies Rust agent flagged (CRITICAL)
1. **Phase C.3 framing wrong** — Rust uses one allocator + post-construction fn-ptr stores,
   not a `gorget_array_new_drop` symbol. Picking `_new_drop` by name also violates
   "No name matching". → reframe as "generalize the D4 store pattern."
2. **`flush_drop_queue` doesn't exist in Rust** — confirms PF-05. Map self-host's
   emit_scope_drops/emit_drops_for_early_exit/flush_drop_queue onto Rust's
   pop_scope/pop_scope_no_emit/emit_early_exit_drops; retire the queue.
3. **`maybe_moved` does NOT gate Drop-vs-DropIfAlive** — confirms PF-01. "If the self-host
   branches on maybe_moved to choose, that's a latent double-free source — Rust deliberately
   moved away from it."
4. **OpBorrow/OpClone/OpMove are NOT three symmetric LIR ops** — only Move special at LIR;
   Clone at GIR; Borrow via addr-lowering. Self-host shouldn't invent three parallel LIR ops.

---

## A.4 checklist (merged)

| ID | Site (file:line) | Current | Expected (Rust) | Fix | Cluster | Static-found? |
|----|------------------|---------|-----------------|-----|---------|----------------|
| **a-1** | lower.gg is_droppable_type 901-927 + emit_scope_drops registration | borrows/Ptr locals would register for drop | Rust `register_local` skips `!needs_drop`; borrows never registered | gate registration on owner-ness (LoOwned), exclude LoBorrowed/LoView/Ptr | a (double-free) | ✓ |
| **a-2 (L2)** | lir_lower.gg:3146-3158 GIDrop | `T__drop(&slot)` unconditional | branch on slot kind | if Ptr-kind slot → `ISlotLoad` the pointer, pass directly; else `&slot`. Real fix upstream (a-1) | a | ✓ |
| **a-3 (L3)** | lir_lower.gg:3169-3186 GIDropIfAlive | same `&slot` bug + memcmp gate | unconditional drop_if_alive, slot-kind aware | same slot-kind branch as a-2 | a | ✓ |
| **a-4 (C2)** | lir_codegen.gg:3132 IMoveSlot | `/* move_slot */;` no-op | **likely correct as-is** — `drop_elab.gg` consumes the IMoveSlot *annotation* (PF-04 resolution) | **VERIFY drop_elab reaches return-MoveZero slots; add memset ONLY if a residual Initialized-misclassification shows in the probe.** Do NOT pre-emptively memset. | a | ✓ |
| **a-5 (B1)** | lir_codegen.gg:3528 + field-read inst | no `returns_view`/`is_borrow` typed flag | typed borrow flag on field-read | add typed flag → drop-registration reads it (kills a-1 root) | a | ✓ |
| **a-6 (E.1)** | lower.gg SReturn 5291-5305 | bare; 4/7 concerns absent | port all 7 concerns + 5 adjacent | full lower_return port (Phase E) | a | ✓ |
| **a-7** | lower.gg match_scrutinee_ptr 5829 | `CkAssign` (consume) | borrow | `CkAssign`→`CkMatchPtr` | a | ✓ |
| **b-1 (D1)** | lir_lower.gg:2839 Vector ctor | bare `gorget_array_new` | one allocator + post-ctor elem_drop store @off40 | generalize D4 pattern to Vector, drive from drop_strategy metadata | b (leak/OOM) | ✓ |
| **b-2 (D2)** | lir_lower.gg:2800 Dict ctor | bare `gorget_map_new` | + val_drop store @off104 | symmetric to D4 key_drop | b | ✓ |
| **b-3 (D3)** | lir_lower.gg:2825 Set ctor | bare `gorget_set_new` | + elem coverage | extend key_drop path | b | ✓ |
| **c-1** | lir_lower.gg:3439 `__imported_type__` skip | correct | correct | **NO CHANGE** (plan C.1 misframed); keep skip | — | ✓ |
| **c-2** | lower.gg is_droppable_type user types | excludes user types | include once drops fire & don't double-free | flip to include user resource types — **couples with cluster a** (needs a-1..a-5 first) | a-dependent | ✓ |
| **min-1 (F1)** | lir_codegen.gg:4517/4609 | struct `cname*` vs enum `void*` | unify | unify on `void*`+cast | minor | ✓ |
| **min-2 (F2)** | lir_codegen.gg:4609 enum Box payload | `free(access)` no recurse | recursive box-drop | box_inner_type typed field (Snag #13 shape) | b (leak) | ✓ |

**Static-found = ✓ for ALL rows.** The static audit + Rust cross-ref was conclusive (see A.6).

## A.5 dependency DAG + batches

```
Cluster b (LEAK/OOM) — INDEPENDENT, ship FIRST:
  b-1, b-2, b-3, min-2  ── (generalize D4 fn-ptr-store pattern; drive from drop_strategy)
       │  kills the 13 GB driver.gg OOM; resolves R-04 + the A.2 probe-stall fear
       ▼
Cluster a (DOUBLE-FREE) — must ship ATOMICALLY (this is why E.1 failed 9×):
  a-5 (typed borrow flag) ──┐
  a-1 (registration gate) ──┼──► a-2,a-3 (slot-kind drop) ──┐
  a-4 (IMoveSlot memset) ───┘                                ├──► c-2 (include user types)
  a-7 (match scrutinee borrow) ──────────────────────────────┤
  a-6 (lower_return 7 concerns = Phase E) ───────────────────┘
       │  closes the return-corruption SIGSEGV
       ▼
  Validate: self_host_bootstrap + fixed_point
Minor (independent): min-1 (ABI cleanup)
```

## A.6 decision gate — VERDICT: **do NOT trigger rewrite; proceed incrementally**

- Distinct cascade sites: **~13**, under the >15 threshold.
- Sites requiring *restructuring* (not patching): **0–1**. Cluster (a) is *coupling* (ship
  together), not *IR-shape redesign*. a-5 adds a typed flag (additive). a-6 is an additive
  port of 7 known concerns with exact Rust line cites. None redesign the IR.
- The static audit was **unusually conclusive** because the failure mode is now understood
  *mechanistically*: not a mystery cascade, but "cluster (a) shipped half at a time." The
  plan's central fear (audit incomplete → E.1 surprise) is addressed by the coupling
  diagnosis. **The A.2 dynamic probe shifts from a discovery tool to a validation tool.**

## Strategic reordering recommendation (deviates from plan order)

> **⚠️ EMPIRICALLY CORRECTED 2026-05-24 — the "(b) kills the OOM" premise was FALSE.**
> See "Phase B empirical validation" below. Cluster (b) emits correctly + compiles clean +
> stage-0 stays at 1 GB, but stage-1 (self-host-compiled, with (b)'s wiring) STILL OOMs at
> 14.4 GB on driver.gg. The OOM is driven by missing drop **emission** (cluster a:
> scope-exit + drop-on-overwrite are no-ops → NOTHING is freed), not missing element-drop
> *depth* (cluster b). `elem_drop` is inert until (a) emits the `free` calls that invoke it.
> The reorder below is superseded by the corrected ordering in the empirical section.

The plan ordered A→B→C→D→E with the OOM as a constant background threat. The audit
*initially* suggested a cleaner path (ship (b) first to kill the OOM) — but the empirical
probe disproved it. Retained for the audit trail:

1. ~~**Ship cluster (b) first** — it kills the 13 GB OOM~~ **FALSE (measured).** (b) is
   correct, low-risk groundwork (a prerequisite for (a)'s frees to be *deep*), but it does
   NOT reduce the OOM on its own. R-04 + the probe-stall risk are NOT resolved by (b).
2. **Cluster (a) is THE fix for BOTH** the return-corruption SIGSEGV AND the OOM — once
   scope-exit drops fire, stage-1 frees its per-function allocations (the 1 GB→14 GB delta).
3. Drop the queue+flush design (PF-05); emit drops directly + unconditionally (PF-01).
4. Reframe Phase C.3 (b-1/b-2/b-3) as "generalize D4," not "add gorget_array_new_drop." ✅ done.
5. Drop Phase C.1 (the `__imported_type__` skip removal) — it's correct as-is (c-1). ✅

## Phase B empirical validation (2026-05-24) — cluster (b) implemented + measured

**Implemented** b-1 (Vector elem) + b-2 (Dict value) in `lir_codegen.gg`:
`lc_coll_val_lookup`, `lc_collection_drop_fn`, `lc_collection_clone_fn`, `is_runtime_array_ctor`,
`emit_collection_elem_wiring` (called from the `ICallExtern` arm). Drop+clone wired as a pair
(never drop-without-clone → no clone-path double-free).

**Measurements (driver.gg):**
| Binary | Compiled by | Scope drops | Peak RSS | Result |
|--------|-------------|-------------|----------|--------|
| stage-0 | Rust gg (full drops) | yes | 1.0 GB | ✅ 580,555 lines |
| stage-1 | self-host (+ (b) wiring) | **no-op** | **14.4 GB** | ❌ OOM-killed @ 1262 lines |

stage-0 emits **41 `.elem_drop`** + **7 `.val_drop`** wirings into stage1.c; cc clean.

**Conclusion:** the 1 GB→14 GB gap = everything stage-1 fails to free at scope exit (cluster a
emission is disabled). (b) is **necessary** (so (a)'s frees are deep — e.g. freeing a
`Vector[Vector[int]]` also frees the inner arrays) but **not sufficient** — it cannot reduce
memory until (a) emits the frees. **The OOM and the SIGSEGV are the SAME root cause: missing
drop emission (cluster a).** They are not separable the way the reorder assumed.

**Revised path:** keep (b) as committed correctness groundwork (validated: emits, compiles,
lowerer_comparison 1/1, no stage-0 regression). The OOM + SIGSEGV are closed together by
cluster (a). The A.2 staged probe (parser→loader→driver) is still needed — driver.gg will OOM
under stage-1 until (a) lands, so cheap cascades must be drained on smaller inputs first.

(b) committed: `d2efd716`. Docs/plan v3: `89f71963`.

### Drop-emission gap, quantified — Rust driver.c vs self-host stage1.c (2026-05-24)

Both are C for the SAME program (driver.gg): `driver.c` = Rust gg (reference, correct drops);
`stage1.c` = stage-0 self-host emission. Diffing drop-call patterns gives the exact gap:

| pattern | Rust driver.c | self stage1.c |
|---|---|---|
| `__drop(` (user-type drops) | 2339 | **0** |
| `gorget_string_free(` | 5276 | **0** |
| `gorget_array_free(` | 543 | **0** |
| `gorget_map_free(` | 100 | **0** |
| `gorget_set_free(` / `gorget_closure_free(` | 1 / 1 | **0** / **0** |
| `.elem_drop =` (fn-ptr wiring) | 8* | 41 |
| `.val_drop =` | 3* | 7 |

\* Rust wires elem/val-drop via byte-offset stores (`*(...)((char*)&slot+40)=...`), not named
fields — so the `.elem_drop =` text count understates Rust; both wire them. Style difference,
not a gap.

**Verdict:** the self-host emits **~8,260 → 0** drop/free calls. The ENTIRE 14 GB leak is the
total absence of scope-exit drop emission. (b)'s 41 elem_drop fn-pointers are inert — there
are 0 `gorget_array_free` calls to invoke them.

**VALIDATION HARNESS for cluster (a):** re-emit stage1.c after each (a) sub-step and re-run
this grep-diff; watch the self-host drop counts climb to Rust parity (2339 `__drop`, 5276
string_free, 543 array_free, 100 map_free). Convergence = leak closed. This is the
gradient "definition of done" — far stronger than RSS or the drop-blind lowerer_comparison.
Reproduce:
```bash
RUST=tests/fixtures/self_host_lowerer/driver.c          # Rust gg compiling driver.gg
SELF=/tmp/stage1.c                                       # stage-0 self-host emitting driver.gg
for p in '__drop(' 'gorget_string_free(' 'gorget_array_free(' 'gorget_map_free('; do
  printf '%-22s rust=%s self=%s\n' "$p" "$(grep -cF "$p" $RUST)" "$(grep -cF "$p" $SELF)"; done
```

### Cluster (a) SNAG #1 — un-disabling emit_scope_drops hangs stage-0 (2026-05-24)

First cluster-(a) probe: changed `emit_scope_drops` + `emit_drops_for_early_exit`
(`lower.gg`) from no-op to unconditional LIFO `GIDropIfAlive(entry.local_id)` emission (the
Phase D edit). Result: **stage-0 (Rust-compiled self-host) emitting driver.gg TIMED OUT at
300s, producing only 758 lines** (was 580K lines, ~1 GB, completing fast before the change).
Drop counts still 0 (it hung in the GIR/LIR phase before emitting drop-bearing C).

This is a pathological slowdown/hang, NOT a runtime double-free (we never ran the binary).
Emitting `GIDropIfAlive` per scope-entry triggers it. Suspects (to investigate with the new
`--emit-gir`/`--emit-lir` dump flags once they land):
- Drop-type-name resolution at `lir_lower.gg:2490` (GIDropIfAlive → resolve type name) —
  possibly O(n²) or recursive over the now-multiplied drop instruction count.
- O(n²) instruction append into blocks (cf. the `sb_push` O(n²) lesson) once drop count
  explodes the per-block instruction list.
- A feedback loop where emitting into the current block during `lower_stmt` re-triggers
  scope processing.

Experiment reverted (`git checkout lower.gg`) — not shippable as-is. This is the first thing
cluster (a)'s real work must solve. The `--emit-gir`/`--emit-lir` flags (being added by a
background agent) are the right instrument: dump GIR/LIR for a SMALL input (parser.gg or a
tiny fixture) with emission un-disabled and see whether the drop instruction count explodes
or a specific pass stalls.

---

## Cluster (a) progress log (2026-05-24, continued — supersedes SNAG #1 framing above)

### SNAG #1 RE-DIAGNOSED: perf, NOT a hang (it was a stdout-buffering artifact)

The "stuck at 758 lines" was **block-buffered stdout** flushing 758 lines before the
timeout kill — the process was progressing the whole time. With `stdbuf -oL`, `--emit-gir`
on driver.gg **completes** (214,043 lines). Full `--emit-c` with drops completes in ~500-560s
(slow — an O(n²) to chase, suspects: liveness / drop-type resolution at `lir_lower.gg:2490`
/ block-instruction append — but it FINISHES, within the bootstrap's 600s build deadline).
So snag #1 is a perf regression, not non-termination. Far more tractable.

### Drop machinery WORKS — harness climbing off zero

With `emit_scope_drops` + `emit_drops_for_early_exit` un-disabled (unconditional LIFO
`GIDropIfAlive`, no maybe_moved gate):

| pattern | Rust | self (no-drops → un-disabled) |
|---|---|---|
| `gorget_string_free(` | 5342 | 0 → **1420** |
| `gorget_array_free(` | 544 | 0 → **454** |
| `gorget_map_free(` | 102 | 0 → **76** |
| `__drop(` (user types) | 2355 | 0 → 0 (gated by C.1, see below) |

Tiny program (`main` with one String local) emits exactly-correct GIR: `drop_if_alive _2`
+ `move_zero _1`. The GIDropIfAlive → lir_lower → C path produces real drop calls.

### Phase C.1 was WRONGLY CANCELLED — the __imported_type__ skip over-skips user types

The audit (agent 1) judged the `__imported_type__` skip "correct, keep it." **Empirically
false for whole-program compilation.** In the bootstrap, driver.gg imports every module, so
`loader.gg:705,717` tags EVERY user enum/struct `__imported_type__`. `populate_drop_metadata`
(`lir_lower.gg:3439,3475`) then skips all of them → `recursive_drop_structs` empty → **0
`__drop` definitions** (Rust emits 212). The skip's rationale ("Rust preamble provides them
via static inline") is true only for RUNTIME types (GorgetString → gorget_string_free); the
bootstrap preamble is `rust_c[..first "typedef struct __gg_"]`, which contains NO user types,
so user-type `<Type>__drop` is NOT preamble-provided and MUST be emitted. Both Rust gg and
the self-host are whole-program — there is no separate-compilation linkage for the skip to
defer to.

**Fix (C.1 revived):** removed both skip sites in `populate_drop_metadata`. Result:
`__drop` defs 0 → **267** (clone_inplace 0 → 89), **0 redefinition errors** (the `fn_exists()`
guard in emit_struct_drops/emit_type_drop_fns handles genuine double-define — the proper
mechanism, vs. a blanket skip). C.1 skip-removal is double-define-safe. ✅

### C.1 removal UNMASKS the predicted C-01/C-02 cascade (as the audit foresaw)

Once user enums are recognized as resource types (now in `recursive_drop_enums`),
`op_consume` on a match scrutinee flips from borrow → clone. cc then fails with **2780×
"cannot convert to a pointer type"**: e.g. `parser___token_tag` emits
`__v1 = Token__clone(__v0); __v5 = ((__gg_Token *)(__v1))->tag;` — the clone returns a VALUE,
and the tag-read casts it to a pointer. This is exactly audit rows **C-01** (slot-kind /
tag-read assumes pointer base) + **C-02** (match scrutinee forced clone). Confirms the
audit's core thesis: **C.1/c-2 (user types droppable) and a-7/a-2/a-3 (borrow scrutinee +
slot-kind drop) are coupled — they must ship together.** This is why all 9 prior E.1 attempts
failed: each shipped part of the cluster.

### a-7 applied (validating)

`match_scrutinee_ptr` (`lower.gg`): `CkAssign` → `CkMatchPtr` (borrow, not consume) — a match
borrows its scrutinee; the tag/field reads take its address. Should eliminate the
clone-then-tag-read cascade. Validation (rebuild + emit + cc, ~560s) in flight.

### Current uncommitted WIP (atomic cluster — does NOT compile yet, do not commit until green)
- `lower.gg`: emit_scope_drops + emit_drops_for_early_exit un-disabled; match_scrutinee_ptr a-7.
- `lir_lower.gg`: `__imported_type__` skip removed (C.1).
Remaining in the atomic cluster after a-7: a-1 (registration gate: don't register borrows),
a-5 (typed borrow flag — root of a-1/B1), a-2/a-3 (slot-kind-aware GIDrop/GIDropIfAlive),
a-6 (lower_return 7 concerns), then runtime-validate via the staged probe + the perf fix.

### a-7 VALIDATED; a-1 null; a-6 keystone applied (2026-05-24, cont.)

- **a-7 (match scrutinee CkAssign→CkMatchPtr): SUCCESS.** Pointer-cast cascade 2780 → 0,
  cc exit 0. stage-1 C compiles. Drop CALLS now fire: `__drop` 0→338, `string_free` 0→1549,
  `array_free` 0→632, `map_free` 0→116. **OOM is GONE: stage-1 peak RSS 1 MB (was 14.4 GB).**
  But stage-1 RUN double-frees at startup (exit 134, "free(): double free in tcache 2").
- **a-1 (skip LoBorrowed/LoView in register_local_for_drop): NULL RESULT.** Counts identical
  (632/116/338) — the over-dropped locals are NOT tagged LoBorrowed/LoView (ownership tagging
  is incomplete at registration), so the gate skipped nothing. a-1 is correct-in-principle
  (kept) but not the active cause. Real over-drop root is deeper (untagged aliases) — revisit
  with a-5 (typed borrow flag) if needed. Params already correctly gated (p.ownership==2 only).
- **a-6 concern #5 (returned-local MoveZero): applied.** The startup double-free signature
  (1 MB, 0 lines) = return-path corruption: `lower_return` moved the returned local into _0
  but excluded only `Some(0)` (the return SLOT, never registered) — the returned LOCAL was
  dropped at exit AND freed by the caller. Fix: capture the `OpMove(src)` from
  `op_consume(val, CkReturn)`, emit `GIMoveZero(src)`, and exclude `src` (not 0) from
  emit_drops_for_early_exit. Validating.

**Cluster (a) status:** OOM closed (14.4 GB→1 MB). Cascade closed (a-7). User-type drops
generate + fire (C.1 + 338 calls). Remaining: close the double-free (a-6 keystone in flight;
possibly a-6 concerns #1-4/#6/#7 + the untagged-alias over-drop a-5), then perf (~560s emit).

### DOUBLE-FREE PINPOINTED via gdb backtrace (2026-05-24) — move-into-construction

a-6 #5 did NOT clear the double-free (counts unchanged). gdb backtrace of the SIGABRT:
```
free → __gorget_global_dealloc_fn → gorget_string_free → Expr__drop
     → SpannedExpr__drop → Stmt__drop → gorget_array_free → Parser__parse_if_stmt
     → parse_statement → parse_block → parse_function_def → ... → main
```
**Diagnosis:** in `parse_if_stmt`, a `Vector[Stmt]` (the if-body) is moved INTO an `SIf` AST
node (enum construction) which the caller owns — but the local `Vector` is ALSO dropped at
scope exit (gorget_array_free → Stmt__drop → … → gorget_string_free on an already-freed
String). The move-into-construction does NOT move-zero the source local. a-6 #5 only covers a
*bare returned local* (`OpMove(src)` at SReturn); an enum/struct CONSTRUCTION that consumes a
resource arg (`SIf(cond, body, …)`) is a different consume site.

**Next step (precise):** ensure construction/field-init consume sites move-zero their moved
resource args. Two hypotheses to check:
1. The construction arg is lowered `OpBorrow` (field aliases `body`) instead of `OpMove` —
   then `body` and the node both free → fix is classify the ctor arg as owning
   (CkCallArgOwning/CkFieldWrite → OpMove), cf. audit F6 (`fn_move_params` ctor registration).
2. The arg IS `OpMove` but no paired `GIMoveZero` is emitted — `wire_liveness_into_modes`
   (2199/2216) emits GIMoveZero for OpMove operands; check whether it covers construction
   args (GIEnumInit/GIStructInit operands) or only call/assign operands.

The general invariant: EVERY `OpMove` of a resource local (return, construction, field-init,
collection-put, call-arg-owning) must pair with `GIMoveZero(src)` so the scope-exit
`drop_if_alive` is elided (drop_elab marks the slot UNINITIALIZED). This is the core of the
move-semantics work — the remainder of cluster (a) after the OOM was closed.

### Session end-state (uncommitted WIP — compiles, runs-then-double-frees, NOT shippable)
- `lower.gg`: emit_scope_drops + emit_drops_for_early_exit un-disabled (Phase D); a-7
  (match_scrutinee CkAssign→CkMatchPtr); a-1 (register_local_for_drop LoBorrowed/LoView gate —
  inert but correct); a-6 #5 (SReturn OpMove move-zero + exclude).
- `lir_lower.gg`: C.1 (__imported_type__ skip removed in populate_drop_metadata).
WINS LOCKED: OOM 14.4 GB → 1 MB; cascade 2780 → 0 (cc clean); user-type drops 0 → 267 defs /
338 calls. REMAINING: move-zero-at-construction (above) to clear the double-free, then perf
(~510s emit), then runtime-validate (bootstrap), then the rest of a-6's 7 concerns.
