# Keystone ③ — Typed LocalOwnership-aware drop/ownership model: research foundations

> Durable reference for the ③ chain (oracle spec + self-host current-state map +
> planned approach). The executor brief and reviewers cite this. Approach for
> ③(b) is **pending the prototype agent's RUN-verified diff** — do not treat the
> "planned approach" as final until the prototype confirms +4 / fixed_point GREEN.

Tip: `fc0a4176`. Runtime parity confirmed **393/929 = 42.3%** (re-measured this session).

## The one architectural gap (handover framing)
The self-host lacks a typed ownership/category subsystem, so it reconstructs
ownership/drop info from mangled NAMES / type-erased special-cases / not at all.
Build the typed subsystem ONCE, parity as a side effect, sequenced ③→②→①.
③ is the keystone: the drop/ownership model. It has three INTERLOCKING parts
(fixing one in isolation double-frees another):

- **(a) drop-on-overwrite** — `SAssign` overwrites without dropping the prior value.
- **(b) borrow-vs-own for destructured match payloads** — the +4 proven win; this
  is where the fixed_point double-free lives.
- **(c) user-`Drop`→auto-field-drop** sequence.

## Oracle spec (Rust `src/`, RUN-verified by the oracle-map scout)

### (b) Match-payload extraction — `EnumFieldLoadMode { Move, Borrow }`
`src/ir/instructions.rs:120-137` documents the split.
- **Binding** (`emit_pattern_bindings`, `src/ir/lowering/stmts/patterns.rs:458/465/483`)
  uses `enum_field_load_move` → at LIR (`src/lir/lower/insts.rs:1362-1389`) it
  **zeros the scrutinee's payload field** (`Store NullPtr` through `FieldPtr` →
  memset) for resource payloads. The scrutinee's whole-drop then sees cap==0 /
  ptr==NULL → **no-op** on that field. The bound local is owned & drop-tracked.
- **Condition test** (`lower_pattern_condition`, `patterns.rs:448`) uses
  `enum_field_load_borrow` → no zero; the bound is a **field borrow**
  (`ctx.set_field_borrow(builder, dst, scrut_local, i)`, `patterns.rs:911`),
  NOT drop-tracked; the scrutinee's whole-drop frees it once.
- **Scrutinee drop registration** (`stage_match_scrutinee`, `patterns.rs:174-322`):
  unnamed-temp or named-at-last-use source → transfer the drop to the scrutinee
  staging local (`drops.register_local(scrut_local, ...)`); named source NOT at
  last-use → drop stays on source, scrut is a shallow alias, extracted fields
  become views.
- **Borrow-bind + later consume** composes safely (`ensure_owned_at_boundary`,
  `src/ir/lowering/context.rs:1752-1904`): a Ptr(T) borrow consumed at a
  boundary clones the pointee (`clone_fn_for_ptr`), the clone is drop-tracked &
  owned, the original alias stays untracked → scrutinee whole-drop frees once,
  clone freed once. No leak, no double-free.

### (a) Drop-on-overwrite (reassignment)
`src/ir/lowering/stmts/assigns.rs:196-230`: **drop the old value AFTER computing
the RHS, BEFORE assigning**. For enums with resource payloads but
`DropStrategy::None`, emit an explicit `{Name}__drop` via a `borrow_mut` +
`call_void`; otherwise `drop` / `drop_if_alive` (the latter iff
`drops.is_moved(local_id)`). Sequencing makes `s = s.trim()` safe (RHS computed
on the old value, materialized, then old dropped, then assigned).

### (c) User `Drop` → auto field drop
`src/lir/lower/drops.rs:307-346` (`DropStrategy::Custom`): `DropGuardOpen` →
call user drop fn (or the unified `__gorget_dtor_Type`) → `lower_field_drops`
(per-field drops, AFTER the user fn, `drops.rs:555-660`) → `DropGuardClose`.

## Self-host current-state map (`tests/fixtures/self_host_lowerer/`)

### Drop-scope machinery (`lower.gg`)
- `push_drop_scope`/`pop_drop_scope`/`emit_scope_drops` (`:1059-1123`): at scope
  exit, emit `GIDropIfAlive(local_id)` per registered entry, LIFO.
- LIR (`lir_lower.gg:~2989`) wraps each in `IDropGuardOpen(byte_size+1)/Close`
  (memcmp gate); `drop_elab.gg` statically elides when init is unconditional.
- `register_local_for_drop` (`:1191`): no-op for non-droppable, and (the "a-1"
  fix) for `LoBorrowed`/`LoView` locals — only OWNERS get dropped.
- `is_droppable_type` (`:1148`): true for runtime resource types
  (`resource_meta_for`) and user `resource_types` structs/enums. **Returns FALSE
  for mono'd `Result__X__E`/`Option__X` locals** (they live only in
  `optionlike_resource_types`, which is field-position-only AND drives the
  by-value ABI — must NOT add locals to it). ← the foundation gap.

### Match-payload binding (`lower.gg`)
- `lower_ctor_pattern` (`:8242`), PBinding arm (`:8345-8389`):
  `scrut_borrow = scrutinee_nonowning(...)` (`:8370`); calls
  `emit_payload_read_mode(..., scrut_borrow)`. For an OWNED scrutinee
  `scrut_borrow=false` → `borrow_only=false`.
- `emit_payload_read_mode` (`:8046`): `borrow_only && is_deep_clone_fn(clone_fn)`
  elides the clone (returns the `LoBorrowed` alias) — but ONLY for deep-clone
  payloads; **String/Closure are excluded** (`:8104-8112`) and always clone.
  So an owned scrutinee with a String/struct payload → **deep-clones** an
  independent owner that is **never registered for drop → leak**; and the
  scrutinee itself isn't whole-dropped (is_droppable_type false) → its payload
  also leaks. ⇒ `leaked=true`.

### Drop-on-overwrite (`lower.gg`)
- `SAssign` EIdentifier (`:7258-7298`): emits `GIAssign(lid, op_consume(val))`
  (or `GIDerefStore` for `&`-params) with **NO prior-value drop**. ← ③(a) gap.

## Planned approach (③b — PENDING prototype confirmation)
Foundation (patch `/tmp/drop_optionlike_local_scout.patch` hunks 1+2): register
the owned Option/Result scrutinee for whole-drop via a STRUCTURAL decompose of
`is_droppable_type` (NOT touching `optionlike_resource_types`) + a `lir_lower`
`type_drop_fns` fallback. Alone = +0 (the clone still leaks).

Fix (③b): bind the owned-scrutinee match payload as a **Borrow** (LoBorrowed Ptr
alias into the scrutinee's payload field) — extend the borrow path to cover
String too when the scrutinee is owned-and-whole-dropped — and do NOT register
the payload bind for drop. The whole-scrutinee drop frees the payload once.
Mirrors the oracle's `EnumFieldLoadMode::Borrow` + CoW-on-consume. Chosen over
the oracle's Move+zero because the self-host's `OpMove` source-field-zero codegen
is the deferred piece (devbook ch.13). Target: `leak_match_struct`,
`leak_match_resource`, `leak_result_struct`, `leak_result_collections` flip to
`leaked=false` (+4 → 397), `bootstrap_fixed_point` GREEN.

⚠ Why NOT clone+drop (Prototype B): reaches +4 but double-frees fixed_point
(the self-host's conditional-drop never zeros the slot / resets the drop-flag
after `GIDropIfAlive`, so a per-iteration block-scoped clone+drop whose arm fires
on only SOME loop iterations leaves a stale drop-flag → re-fire). Borrow sidesteps
it. (Carry this finding into ③a/③c.)

## ⚠ BLOCKING follow-up found by review pass 2 (drop-axis vs move-axis desync)
The candidate (`320b3595`) generalized `is_droppable_type` (drop axis) but NOT
`op_consume` (move axis). For a LOCAL-only `Result__X__E`/`Option__X` with a
droppable payload (NOT in `optionlike_resource_types` → `is_resource_type_name`
FALSE), `op_consume` returns **OpCopy** at consume positions (`lower.gg:1522`),
while the local is now whole-drop-registered. So `return r` / `Result x = r` /
`v.push(r)` byte-copy the value out AND whole-drop the source → **use-after-free /
double-free** (a memory-safe leak converted to a UAF). The corpus + fixed_point
don't exercise this shape (latent), so the 1187/0 gate AND pass 1 missed it.
**Fix (move-axis follows drop-axis, oracle-faithful):** at `op_consume`'s
`if not is_resource_type_name(tname): return OpCopy` branch, when
`is_droppable_type(tid)` is true, branch on ownership instead — LoBorrowed/LoView
→ OpClone, else → OpMove — WITHOUT touching `is_resource_type_name` (ABI safe).
Blast radius is exactly the local-only Option/Result-with-resource-payload set.
⚠ The move-out EXCLUSION (moved source not re-dropped) is confirmed for SReturn
(`excl` + `GIMoveZero`, `:7236-7245`) but UNVERIFIED for var-decl/push/field-write
— RUN-verify (the repros under ASan) and add the minimal exclusion wiring if absent.

## Gates (every ③ chain)
Force-rebuild driver (`rm tests/fixtures/self_host_lowerer/driver{,.c}`) →
`self_host_runtime_diff` (parity, --release) → `bootstrap_fixed_point` (the
double-free canary, the load-bearing safety net) → `lowerer_comparison` (960) +
`c_emit_comparison` (891) → parent runs full `cargo test --test integration`.

## NOT ③ (logged, separate): match guard/arm-selection class
`match_advanced`/`match_enum_guard`/`match_guard_complex`/`match_nested_enum`/
`match_tuple_destructure`/`match_wildcard_arm` are WRONG-OUTPUT on arm SELECTION
(guards/ordering), not drop. Do not conflate with ③.
