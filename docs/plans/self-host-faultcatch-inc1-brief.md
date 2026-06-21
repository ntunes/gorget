# Brief — Self-host fault-catch fast-follow, Increment 1 (Overflow + DivByZero)

**Track:** error-model Phase-1 follow-up (e), §11.8. Port the LANDED Rust local fault-catch
into the SELF-HOST compiler so it can COMPILE `(expr) catch Fault.X: …` programs the way Rust
gg does. **Scope = self-host Increment 1: arithmetic faults only (`Fault.Overflow` +
`Fault.DivByZero`).** Self-host Increment 2 (`Fault.Bounds`, the `INT_MIN/-1`→Overflow
div-split, partial-catch panic blocks) is a SEPARATE follow-up after this lands. `equip Error`
/ `dyn Error` is Phase 2 — NOT this track.

**Scout:** `a317c64d` (all 6 premises verified against source + measured end-to-end). Rust
reference: Inc1 `8ab75635` + Inc2 `a447c726`; `docs/plans/error-model.md` §11 / §11.8.

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1` first).
`git add` ONLY the self-host `.gg` files you change + `tests/lints.rs` if you add the ratchet.
Do NOT touch Rust `src/`, docs, or any non-self-host fixture. The self-host's OWN source must
stay panic-default — you are ADDING the ability to *compile* fault-catch, not making the
self-host *use* it.

---

## Hard invariant (the whole track's safety rests on this)
Every new lowering/typecheck/parse arm fires ONLY on the NEW `EFaultCatch` AST node. NO
self-host source file contains a fault-catch expression, so `self_host_bootstrap_fixed_point`
(stage-2==3==4 byte-identical) and the frozen `runtime_snapshots` stay untouched. The existing
contract-`catch` path (`ECatch`, `catch (e):`) MUST remain byte-identical — the scout verified
`catch_basic.gg` self-host output == Rust oracle today; it must still hold after. Keep the two
forms on DISTINCT paths end-to-end (parse, AST node, typecheck, lower).
**Reservation 4ii (review pass-1):** the existing contract `ECatch` currently has NO dedicated
`infer.gg` arm — it falls through to `else: return NO_TYPE` (`infer.gg:914`), i.e. contract-catch
type inference is a no-op today. The new `EFaultCatch` arm must be ADDITIVE and must NOT perturb
that — do NOT "helpfully" add an `ECatch` arm alongside it, or you risk moving the byte-identical
contract canary.

## First-blocker (measured — this is what you're fixing)
The self-host parser consumes `(big*2) catch Fault.Overflow: -1` as the CONTRACT-catch (Result)
form → emits a plain trapping multiply (never re-pointed) + treats the int result as a Result
(reads `->t_tag`, branches `t_Ok_0`/`t_Error_0`, spells the tag-read `(int64*)`) → emitted C
fails to compile: `'int64' undeclared`. Category `CcFailed`. The whole fault-catch shape is
absent (parse + typecheck + lower all miss).

## Dir-duplication map (VERIFIED by md5 — apply each change to the right set)
Source-of-truth dir = `self_host_typechecker/`.
- **`ast.gg` + `parser.gg`:** THREE INDEPENDENT divergent copies — `self_host_typechecker/`,
  `self_host_parser/`, `self_host_resolver/` (each hand-edited, distinct md5). Apply the AST
  node + parser production to ALL THREE, faithful to each copy's structure. `self_host_check/`
  + `self_host_lowerer/` symlink → typechecker (auto-follow).
- **`infer.gg`:** `self_host_typechecker/` only (check/lowerer symlink; parser/resolver have none).
- **lowerer files** (`lir.gg`, `lir_lower.gg`, `lir_codegen.gg`, `lower_match.gg`,
  `lower_expr.gg`, `lower_types.gg`, context): `self_host_lowerer/` only.

---

## Port plan — 7 must-have steps (mirror the named Rust symbols)

Stage like Rust §11.7: **shared LIR shape → C emit → AST/parser/typecheck → fixtures.** One C
backend, so no LLVM leg.

1. **Register the `Fault` enum** (mirror Rust `resolve.rs:152-166` + the builtin-enum
   injection). Register variants `Overflow`, `DivByZero`, `Bounds` (full set, matching Rust's
   enum — even though Inc1 only LOWERS Overflow/DivByZero catching; Bounds-catch lowering is
   Inc2). **Registration SHAPE (review pass-2 clarification):** `Fault.Overflow` is QUALIFIED,
   unlike the prelude-BARE `Some`/`Ok`/`None`/`Error`. So the `resolve.gg:162-168`
   Result/Ok/Error lines are only the precedent that *a builtin enum is registered at all* —
   they use bare `scopes.define`, which is the WRONG shape for a qualified enum. Derive the
   actual registration shape from the USER-enum path (`resolve.gg:186-194`, `alloc_def` +
   parent-aware `DkVariant`), which is how qualified-variant enums register. Plus the lowerer's
   builtin-enum-template injection so `Fault.Overflow()` constructs (`lir_lower.gg:~1177` /
   `lower_types.gg:~423` region — the Option/Result template site). NO `equip Error`.
2. **New AST node** (mirror `Expr::FaultCatch` + `FaultCatchPattern`): add
   `EFaultCatch(Box[SpannedExpr] inner, FaultCatchPattern pattern, Box[SpannedExpr] handler)`
   and a `FaultCatchPattern` enum = `Variant(qualifier, variant)` | `Binding(name)`. Add to ALL
   THREE `ast.gg` copies. Do NOT overload the existing `ECatch` (it's Result-welded:
   `ECatch(Box[SpannedExpr], String, Box[SpannedExpr])`, `ast.gg:92`).
3. **Parser production** (mirror Rust `expr.rs:1073-1135`): in the `catch` handler of ALL THREE
   `parser.gg` copies (today `parser.gg:~1753-1757` hard-expects `catch ( name ) :`), after
   consuming `catch` branch on the next token: `(` → existing contract path (UNTOUCHED);
   `Ident . Ident` → `FaultCatchPattern::Variant`; bare `Ident :` → `FaultCatchPattern::Binding`.
4. **Typecheck path** (mirror `typecheck.rs:3072` + `check_fault_variant`): add an `EFaultCatch`
   arm to `infer_expr_type` (`self_host_typechecker/infer.gg`, before the `else: return NO_TYPE`
   at ~:914) — infer inner + handler types, unify as the result type. For `Variant`, reject a
   qualifier ≠ `"Fault"` (the Inc-2 (D) check) and an unknown `Fault` variant. For the
   binding-form `match f`, apply the panic-default-over-closed-`Fault` exhaustiveness rule
   (every OTHER enum stays strictly exhaustive — gate this on the `Fault` enum only).
5. **NEW shared flag-output checked-op-with-handler-branch LIR shape** (the single largest
   item; mirror `Inst::FaultCheck`/`FaultOp` + GIR `FaultableBinOp` + `FaultScope`). The
   self-host's `IAdd/ISub/IMul` carry only `overflow ∈ {OVF_TRAP, OVF_WRAP}` (`lir.gg:125-127`)
   — there is NO flag-output checked-op primitive; ADD one (the analog of Rust's `Inst::FaultCheck`,
   the genuinely new piece). Add a `FaultScope`-equivalent to the self-host `LowerCtx`
   (`overflow_handler`/`divzero_handler` optional block-ids).
   **⚠ SCOPE MECHANISM (review pass-1, Reservation 1 — LOAD-BEARING correction):** the
   fault-scope is a **per-function field, fresh/none at function entry**, pushed (save +
   restore) around ONLY the wrapped inner subtree of the fault-catch. A faultable op consults
   the *active* scope at its OWN lowering site (mirror Rust `fault_handler_for`,
   `operators.rs:332`). **Do NOT clear the scope at Call/CallExtern boundaries** — that was
   WRONG and would mis-lower the legitimate case `(f() + g()) catch Fault.Overflow:` (and
   `fault_catch_compound.gg`'s `big * 2 + 100 / 5`): after lowering the inner call operands the
   scope would be None, so the following caught `+`/`*` — which IS directly in the wrapped
   expression's own block and MUST be caught — would silently fall to panic-default and the
   fixture would NOT flip. The callee-not-caught (LEXICAL) guarantee comes for FREE because each
   function is lowered by its own `lower_function` pass (`lower_closures.gg:46`) with fresh
   per-function ctx, so a callee body never sees the caller's scope. At the faultable op under
   an active scope: emit op→flag, then `GTBranch{flag → handler_entry, else → cont}`. The GIR
   terminators are `GTBranch`/`GTJump` at **`gir.gg:173-174`** (NOT `lir.gg:225-226`, which are
   the LIR `TJump`/`TBranch`); `new_block` is available. C emit alongside
   `lir_codegen.gg:3574-3598`.
   **⚠ Do NOT be misled by the Rust DOC COMMENT at `context.rs:281-282`** — it is STALE and
   still says the scope is "CLEARED at any Call/CallExtern boundary." The IMPLEMENTATION does
   not do that (the only `fault_scope` writes are the per-function `FunctionState::default()`
   and the take/restore around the inner subtree in `lower_fault_catch_expr`,
   `exprs/mod.rs:3562-3575`). Mirror the implementation, not the comment.
6. **`lower_fault_catch`** cloned from `lower_catch_expr` (`lower_match.gg:877` — the verified
   CFG template: `ok_bb`/`err_bb`/`merge_bb` via `new_block`, `set_terminator(GTBranch)`,
   `GTJump(merge)`; mirror Rust `lower_fault_catch_expr` `exprs/mod.rs:3499`). Create
   handler-entry + merge blocks; push the fault-scope for the inner subtree; on the handler
   path **materialize the `Fault.Overflow()`/`Fault.DivByZero()` value via `IEnumInit`**
   (binding form binds it; pattern form discards) and run the handler. Wire a new `EFaultCatch`
   arm into the `lower_expr.gg:~3188` dispatch, NEXT TO the `ECatch` arm.
   - **Drop-correctness (subtlest property):** the handler branch MUST live in the GIR/LIR CFG
     so the self-host drop passes run over it — NEVER a C-emit goto. A
     `(struct.method() * k) catch …` temporary must be dropped correctly on BOTH the fault and
     no-fault paths. This is exactly why we clone the `lower_catch_expr` CFG template, not hand-emit.
   - **Materialization note (Reservation 4i, Inc2-forward):** Rust's `lower_fault_catch_expr`
     calls `ensure_owned_at_boundary` on the no-fault inner value (`exprs/mod.rs:3586`) to
     reconcile borrow-vs-owned across the two branches. This is DEAD WEIGHT for Inc1 (pure-int
     arithmetic results), so omit it — but file an Inc2 TODO: the resource-index `Fault.Bounds`
     path WILL need the boundary-materialize. Do not rediscover it as a bug.
7. **PREFER scope-gating over a new fault-op token (review pass-1, Reservation 2 — corrected):**
   faultable arithmetic uses the SAME operator spellings (`"+"`/`"*"`/`"/"`); the fault-ness is a
   property of the ACTIVE SCOPE at lowering time, NOT a new operator token. So gate fault routing
   on the active scope at the EXISTING binop lowering site (mirror Rust `fault_handler_for`,
   `operators.rs:332`) — do NOT manufacture a new pseudo-op (`OP_*_FAULT`) routed through
   `map_binop`. With the scope-gate, `map_binop` is never on the fault path. ONLY IF a new op
   constant is nonetheless introduced must you wire it explicitly in `map_binop`
   (`lower_types.gg:2433-2434`, the `else: diag_bug → OP_ADD` footgun) and add the `tests/lints.rs`
   arm-count ratchet — but the scope-gate is the preferred design (no new token).

## Fixtures that must flip CcFailed → MATCH (Inc1)
`fault_catch_overflow.gg`, `fault_catch_div0.gg`, `fault_catch_binding.gg`,
`fault_catch_compound.gg` (the ARITHMETIC fault-catch fixtures). The `Fault.Bounds` fixtures
(`fault_catch_bounds*.gg`) and `fault_catch_intmin_div.gg` stay CcFailed until self-host Inc2 —
that is EXPECTED, not a regression; do NOT redesign them.

## Gate battery (the port is unverifiable without all of these)
- `self_host_runtime_diff` (`GG_RUNTIME_DIFF=1`): the 4 arithmetic fault-catch fixtures move
  CcFailed → MATCH (the honest parity delta). Re-measure, don't estimate.
- `self_host_runtime` (lock-in snapshots): stays green.
- `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`): re-converges — the driver
  self-emits its OWN new parser/lowerer arms, so this is the load-bearing validation that the
  port is self-consistent. **Double-frees here on a wrong drop model — the canary for step 6.**
- `parser_comparison` / `resolver_comparison` / `type_comparison` / `c_emit_comparison`: no
  regression (the 3 divergent parser/ast copies must each stay structurally faithful — re-read
  the MATCHED COUNTS, these are diagnostic-always-pass).
- `cargo test --lib`, full `cargo test --test integration` (parent drives the full sweep).
- ASan on the 4 fault-catch fixtures (the handler-path temporary drop is the leak/UAF risk).

## Riskiest parts (brief the executor to slow down here)
1. **Step 5 (new flag-output checked-op LIR + scope-threading)** — the genuinely new machinery.
   Keep the existing trapping `IAdd`/`IDiv` paths byte-identical OUTSIDE an active fault-scope.
2. **Drop-correctness on the handler path (step 6)** — model in the CFG, never a goto;
   `bootstrap_fixed_point` + ASan are the canaries.
3. **3-way parser/AST copy drift** — apply steps 2-3 to each of the three divergent copies
   faithfully, or the `*_comparison` diagnostics rot.

## Open item to flag (do NOT silently work around)
The scout did NOT verify whether the self-host has the same plain-op `INT_MIN/-1` cross-backend
gap that Rust Inc2 (E) fixed. That is a self-host Inc2 concern; note it in TODO, do not address
here.
