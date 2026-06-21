# Brief — Self-host fault-catch Increment 2 (Bounds + div-split + plain-op INT_MIN trap)

**Track:** follow-up to self-host fault-catch Inc1 (`2a36bc2f`). Mirrors Rust Inc2
(`a447c726`/`4020e01e`). **Scope = pure LOWERING:** (A) `Fault.Bounds` catch, (C) the
`INT_MIN/-1`→`Fault.Overflow` div-split + partial-catch panics, (E) the plain-op `INT_MIN/-1`
trap (a Core-#8 cross-compiler defect the self-host shares). **Scout:** `a9c93ef7` (14 premises
verified, first-blockers measured by RUNNING).

**⚠ Inc1 already over-built the front-end — DO NOT touch it:** `infer.gg:926-928` already
accepts `Fault.Bounds` + rejects bad qualifiers; the `Fault` enum already has `Bounds` (tag 2,
`lower.gg:2273` + `resolve.gg:178/203`); `lower_nullary_variant_ident("Bounds")` already
constructs `Fault.Bounds()` (`lower_types.gg:1097`). So NO resolve/typecheck/registration work —
if you find yourself editing `infer.gg`/`resolve.gg`, stop.

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1`; must include
`2a36bc2f`). `git add` ONLY the self-host `.gg` files you change + `tests/fixtures/runtime_snapshots/*.out`
(+ `tests/lints.rs` if you add a ratchet). Self-host's OWN source stays panic-default (no
`catch Fault.Bounds` anywhere) → `bootstrap_fixed_point` must stay GREEN.

---

## Measured first-blockers (RUN-verified by the scout)
- **`fault_catch_bounds.gg`** (+ `_negidx`): emit-C + cc OK, runtime PANIC — the EIndex read still
  uses the panicking `gorget_array_get`; never routed to a bounds handler. Oracle `-1\n20\n7`.
- **`fault_catch_bounds_drop.gg` / `_resource_mut.gg`**: CC-FAIL —
  `incompatible type for argument 1 of 'gorget_string_clone_to_owned'`: the fault-catch
  DOUBLE-clones a `Vector[String]` element (`__v12 = clone(__v11)` correct, then
  `__v15 = clone(__v12)` where `__v12` is already an owned `Str` by-value). **Core-#8 class.**
- **`fault_catch_intmin_div.gg`**: WRONG-OUTPUT — `IFaultCheck` for Div tests only `rhs==0`, so
  `INT_MIN/-1` is treated as no-fault → plain `IDiv` lacking the INT_MIN trap → silent wrapped UB.
  Self-host `-9223372036854775808\n…`; oracle `1\n11\n22\n33`.

---

## SUB-INCREMENT 1 — Bounds (LARGE, do FIRST — changes the FaultScope shape)

1. **`lower.gg`:** add `Option[int] fault_bounds_handler` to `LowerCtx` (after `fault_divzero_handler`,
   ~:392). **Sibling-drift: add a trailing `None` to ALL positional `LowerCtx(...)` constructors**
   — the scout counts 5 (`lower_closures.gg:87,384,1689`; `lower.gg:1988,3741`). **`grep -n "LowerCtx("`
   first and update every site** (a miss = a build break or a wrong-field-offset miscompile). NOTE
   (review pass-1): there is NO self-host LowerCtx arm-count ratchet (`fault_op_lowering_arms_count`
   guards the RUST `FaultOp` enum, not this) — the exhaustive grep is the ONLY guard; the
   `lower_expr.gg:~4447` `LowerCtx(` hit is a COMMENT, not a constructor (exclude it).
2. **`lower_match.gg` `lower_fault_catch_expr` (~:940-1027):** the Bounds arm is currently an explicit
   stub (`:957` "An unknown / Inc2 variant (e.g. Bounds): catch nothing here"). Add a `catch_bounds`
   bool (pattern `FcpVariant(_, "Bounds")` sets it; binding-form sets all three); create
   `bounds_entry = new_block()` when set; save/restore `ctx.fault_bounds_handler` around ONLY the
   inner subtree (mirror the ovf/dz save/restore at ~:976-992); fill `bounds_entry` by
   `lower_nullary_variant_ident(ctx, gmod, "Bounds")` + bind + lower the handler (mirror ~:1006-1024).
3. **`lower_expr.gg` EIndex arm (~:2262-2353):** when the getter is `gorget_array_get` AND the
   collection is array-backed (gate on `CkVector`/`CkDeque` ONLY — dict/set keep panicking) AND
   `ctx.fault_bounds_handler` is `Some(h)`: emit `gorget_array_safe_get` (signed int64, NULL on OOB —
   `src/backend/c/runtime/runtime_array.c:41`, already used for `.get()` `lir_lower.gg:1697`) into a raw ptr →
   `GICmp(flag, CMP_EQ, raw_ptr, null)` → `GTBranch(flag, h, cont_bb)` → in `cont_bb` materialize the
   element. **SHARE the post-raw_ptr element-materialization (the existing dst 3-way type split) with
   the non-faultable path — do NOT duplicate it.** ⚠ **Case-(c) — SCOPE OUT struct elements (review pass-2 + pass-3):** the SCALAR/String deref handles
   `safe_get` (`returns_ptr_to_element`, `lir_lower.gg:2459` lists it), but the AGGREGATE by-value-struct
   copy-out (`eindex_raw_getter`, `lir_lower.gg:~3906`) is keyed on `gorget_array_get`/`gorget_map_get`
   and DELIBERATELY EXCLUDES `safe_get` → a faultable bounds read of a `Vector[Struct]` (dst case (c))
   would fall to a plain `ISlotStore` of a `void*` into a struct slot = miscompile.
   **DEFAULT: gate the faultable `safe_get` route to the EIndex dst-split cases (a) scalar
   (`eix_elem_tid < PRIM_COUNT`) AND (b) resource (`is_resource_type_name` → `GtPtr LoBorrowed`, which
   covers String AND resource-structs/optionlike — NOT just String); EXCLUDE only case (c) the plain
   non-resource struct (`eix_elem_tid >= 0`, `lower_expr.gg:~2343`).** For a case-(c) struct element,
   fall through to the normal (panicking) `gorget_array_get` path — i.e. the
   bounds-catch simply doesn't catch for struct vectors (it panics on OOB, an interim gap, NOT a wrong
   value). File a sharp TODO + add a `Vector[Struct]` bounds-catch fixture as a HELD-OUT guard (wired
   with the correct expected output, NOT snapshotted) so the gap is visible — "don't redesign around
   gaps." **⛔ Do NOT take the naive "add `safe_get` to `eindex_raw_getter`" route (pass-3 hazard): that
   set is deliberately keyed to NOT catch `.get()`/`.unwrap()`, whose Option-struct dst ALSO arrives as
   `safe_get` — adding it would make `Vector[Struct].get()` (Option-struct dst) hit the aggregate
   copy-out and emit `*(Option*)(void*)` over a void* pointing at the bare element = a NEW silent
   miscompile of `.get()` on struct vectors.** Closing case (c) properly later needs a distinguishing
   typed marker for the faultable raw `safe_get` (not a bare func_name add) + a `Vector[Struct].get()`
   regression fixture — out of Inc2 scope.
4. **⭐ CORE-#8 (the riskiest part — gate with ≥3 fresh brief-review passes + ASan both backends):**
   the faultable resource-element read must produce a correctly-OWNED element EXACTLY ONCE and must
   NOT double-clone at the catch boundary, and the OOB-path NULL dst must NEVER reach a clone/push.
   The CC-FAIL shows the `result_local`/`op_consume(CkAssign())` re-clones an already-owned
   `GtPtr(GorgetString)`. Investigate: the `result_local` for the faultable-clone case likely must be
   typed as the OWNED element (not `GtPtr`), mirroring Rust's `ReadMode::Clone` + the `is_faultable_clone`
   typed flag from Rust Inc2. A plain `Vector[String]` index-read+push works in the self-host (scout
   verified `alice\neve`), so this is fault-catch-context-specific. **If this resource path proves
   intractable within scope, land Bounds for the NON-resource case (`fault_catch_bounds`/`_negidx`) +
   all of Sub-increment 2, and DEFER `_drop`/`_resource_mut` with a sharp TODO + those two fixtures
   held out (NOT redesigned) — do not dodge the bug by reshaping the fixtures.**
5. Fixtures flipped → snapshot `tests/fixtures/runtime_snapshots/*.out`: `fault_catch_bounds`,
   `_negidx` (always); `_drop`, `_resource_mut` (if step 4 lands).

## SUB-INCREMENT 2 — Div-split + plain-op INT_MIN trap (MEDIUM, HIGH-scrutiny)

1. **`gir.gg`:** add `const int OP_DIV_OVERFLOW = <next free, scout says 14>` — the INT_MIN/-1
   fault-check op (mirror Rust `FaultOp::DivOverflow`).
2. **`lower_expr.gg` EBinaryOp Div/Rem gate (~:888-905):** emit a check+branch ONLY for the CAUGHT
   categories; the uncaught category needs NO panic block (review pass-2 — the originally-briefed LIR
   `ITrap` block was WRONG-LAYER: the gate emits GIR, and there is no GIR `ITrap`/Trap, no GIR→LIR ITrap
   path, and no `GTUnreachable` terminator). Design:
   - If `fault_overflow_handler` is `Some(ho)`: `GIFaultCheck(flag_ovf, OP_DIV_OVERFLOW, lhs, rhs)` →
     `GTBranch(flag_ovf, ho, next_bb)`.
   - If `fault_divzero_handler` is `Some(hz)`: `GIFaultCheck(flag_dz, OP_DIV/OP_REM, lhs, rhs)` →
     `GTBranch(flag_dz, hz, next_bb)`.
   - Then the bare `IDiv`/`IRem` in the final cont block. **The uncaught category falls through to this
     bare op, whose step-4 guards panic with the correct message** — `INT_MIN/-1` → the new (E) `"integer
     overflow"` guard; `rhs==0` → IDiv's existing div0 guard (and IRem's new one). The two fault
     conditions are mutually exclusive (`INT_MIN/-1` has `rhs=-1≠0`), so order doesn't matter. This gives
     the exact Rust partial-catch semantics WITHOUT any panic block: `(INT_MIN/-1) catch Fault.DivByZero:`
     → overflow uncaught → falls through → (E) guard panics "integer overflow" (`fault_intmin_partial`);
     `(10/0) catch Fault.Overflow:` → divzero uncaught → falls through → div0 guard panics "division by
     zero" (`fault_intmin_partial_divzero`); binding-form `catch f` sets both → both branch.
   - **ENTRY-GATE RESTRUCTURE (review pass-1):** the current gate sources `fault_handler` from
     `fault_divzero_handler` ALONE and enters only when `fault_handler >= 0`. Replace that with the
     per-handler check above (emit each branch independently keyed on its OWN `Some`). When BOTH are
     `None`, emit no FaultCheck/branch at all — just the bare (E)-guarded `IDiv`/`IRem`.
3. **`lir_codegen.gg` `IFaultCheck` C-emit (~:3624-3642):** add an `OP_DIV_OVERFLOW` arm — signed →
   `dst = ((ct)lhs == INT*_MIN && (ct)rhs == -1)` (per-width MIN); unsigned → `dst = 0`. NOTE (review
   pass-3): there is NO existing `INT_MIN` helper (`c_type_name`/`:83` only yields the C type name) —
   add a small inline `ty`→`"INT64_MIN"`/`"INT32_MIN"`/`"INT16_MIN"`/`"INT8_MIN"` mapping, restricted
   to SIGNED widths (`LT_I8..LT_I64`); unsigned types never overflow on div (→ `dst = 0`). The step-4
   plain-op guards use the same per-width-MIN mapping.
4. **Plain-op INT_MIN trap (the self-host (E) analog — REQUIRED so partial-catch + plain ops match
   Rust):** `lir_codegen.gg` `IDiv` (~:3607-3611) and `IRem` (~:3613-3617): add the unconditional
   `INT_MIN/-1` guard (`if ((ct)l==INT*_MIN && (ct)r==-1){ fprintf(stderr,"gorget: integer overflow\n");
   exit(1);}`); `IRem` ALSO needs the missing div0 guard. RUN-verified the self-host currently silently
   miscompiles plain `INT_MIN/-1` (Core-#8 cross-compiler defect). HOT PATH — the full runtime sweep is
   the regression gate.
5. Fixture flipped → snapshot: `fault_catch_intmin_div`. Per the add-exercising-fixtures rule, add a
   plain-op `mod`/`div`-style INT_MIN fixture for (E) if the held-out set doesn't already exercise the
   plain path (verify what `fault_catch_intmin_div` covers first).

---

## Gate battery
- Rebuild the self-host driver; `self_host_runtime` lock-in (+ up to 5 new snapshots) — 0 regressed.
- `GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` — GREEN (self-host source untouched).
- `GG_RUNTIME_DIFF=1 … self_host_runtime_diff --nocapture` — confirm the 5 fixtures flip + no regressions; report `PARITY =`.
- `lowerer_comparison`/`c_emit_comparison`/`parser_comparison`/`resolver_comparison`/`type_comparison` — no regression (MATCHED COUNTS).
- **ASan on `fault_catch_bounds_drop`/`_resource_mut` (both the C path AND a manual `-fsanitize=address` run)** — the resource-element drop is the leak/UAF risk; stdout is leak-blind so ASan is load-bearing.
- `cargo test --lib`. Parent runs the full both-backend sweep.

## Riskiest part
Sub-increment 1 step 4 (the Bounds resource-element clone/drop boundary). The Rust Inc2 was a
UAF/double-free chain (its output-review caught a masked NULL-deref); the self-host's symptom is a
compile-time double-clone, same class. Heavy review + ASan; graceful-degrade to non-resource Bounds +
Div-split if intractable (per step 4), never reshape the fixtures.
