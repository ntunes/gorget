# Error-model Inc-2.1c scout — DivByZero + Bounds cross-frame fault tags

**Status:** DESIGN/FEASIBILITY SCOUT + END-TO-END PROTOTYPE-MEASURED, 2026-06-25, on
`gorget-1` tip `e02627ae` (worktree). Supersedes the one-line TODO/DONE claim
"2.1c — DivByZero + Bounds tags … same slot-write, mostly fixtures." That claim is
**WRONG for DivByZero in one respect and WRONG for Bounds entirely.** This scout
PROTOTYPED DivByZero end-to-end (C + LLVM, compiles + runs + caught + panics-by-default),
found a **category-confusion correctness bug** the naive "same slot-write" approach hits,
and scoped Bounds as a distinct Phase-1.5 mechanism (a `FaultableIndexLoad`-in-the-callee
shape, NOT the same slot-write). Throwaway prototype — REVERTED, not integrated.

Grounded in `error-model.md` §11 (Phase-1 fault set + the `INT_MIN/-1` div-split + Bounds
via `gorget_array_safe_get`+NULL-branch), `error-model-inc21-scout.md` (the 2.1 mechanism
+ cite map), the DONE.md Inc-2.1a (`a1076edc`) + Inc-2.1b (`022342e6`) entries, and
`docs/devbook/24-layering-discipline.md` (typed discriminant, no magic literal).

---

## 0. VERDICT

- **DivByZero — Phase 1 (do it NOW).** Same hidden-slot ABI as Overflow, but it is **NOT
  just a different discriminant on the same branch** — the callee needs a SECOND
  fault-return block (writing the DivByZero tag), the participation analysis must detect
  DivByZero catches, and the call-site gate must select the matching handler.
  **PROTOTYPED + MEASURED green** (C + LLVM): deep div0 caught → `999`, uncaught deep div0
  panics `division by zero` exit 1, both backends at parity. ~40 lines of mechanical
  GIR-lowering change (NO backend-emit change — the slot-write is shared GIR; both backends
  already thread `fault_slot_param_count`).
- **⚠ A category-confusion BUG the naive approach introduces** (the prototype caught it):
  the GIR `FaultableCall` carries a SINGLE `fault_handler` + a single `slot != 0` branch.
  When the catch is the **binding form** (`catch f: match f`) OR a single-category catch
  over a callee that can produce a DIFFERENT category, the `!= 0` branch routes to the
  WRONG handler entry → constructs the WRONG `Fault` variant. **Measured:** binding-form
  deep div0 printed `100` (Overflow arm) instead of `200` (DivByZero arm). The principled
  fix is a **caller-side tag-dispatch** (read the slot tag VALUE, route to the matching
  category entry). This is the load-bearing design item for 2.1c — bigger than the
  per-category-tag-write, and it MUST land with DivByZero or 2.1c ships a silent
  miscompile (a reference-grade gate failure: "both backends agree on `100`" is the WRONG
  answer).
- **Bounds — Phase 1.5 (defer; distinct mechanism).** NOT the same slot-write. A deep
  bounds fault needs the callee's index read to lower to a `FaultableIndexLoad`
  (`gorget_array_safe_get`+NULL-branch) routing to a bounds-RETURN block that writes the
  Bounds tag — which means: (a) the participation analysis must detect **index reads**
  (`v[i]`), not just arith ops; (b) `setup_fault_return_scope` must wire a `bounds_handler`
  to a bounds-return block; (c) the index-read result-type subtlety (resource elements
  yield `Ptr(T)` borrows — `lower_fault_catch_expr` already handles this for the LOCAL case
  via `ensure_owned_at_boundary`, but the cross-frame callee returns a sentinel, so the
  result-type unification differs). **Measured:** deep bounds panics today AND under the
  DivByZero prototype (`index out of bounds`, uncaught). Phase-1.5 because it is a second
  distinct shape, not a tag tweak.

**Recommendation: scope 2.1c = DivByZero (incl. the tag-dispatch) now; Bounds = 2.1d
(Phase 1.5).** Reframe the TODO/DONE "same slot-write, mostly fixtures" line — it under-counts
both.

---

## 0.6 PASS-1 BRIEF-REVIEW FOLD — v2 corrections (AUTHORITATIVE; supersede §3/§4 where noted)

Pass-1 review (fresh, 2026-06-25) verified premises/tag-values/both-backends/drop-gate/Bounds-deferral CORRECT, and confirmed the pass-arm `..` sites (incl. `shared_async.rs:623`) are SAFE. Four folds the executor MUST apply:

- **FOLD-1 (BLOCKING — the prototype `.or()` masked it): broaden the slot-allocation gate.** `src/ir/lowering/exprs/calls.rs:1427-1428` keys slot allocation on `fault_overflow_handler.is_some()`. Under proper Option A (separate `overflow_handler`/`divzero_handler`, NO `.or()`), a `catch Fault.DivByZero:`-only scope has `overflow_handler = None` → the gate would pass `Constant::Null` → the callee panics inline instead of propagating → `fault_deep_catch_divzero.gg` FAILS. Broaden the predicate to allocate `&slot` when EITHER caught category is `Some` (`overflow_handler.is_some() || divzero_handler.is_some()`); pass the surviving per-category handlers (None for an uncaught category) into the new `FaultableCall` fields.
- **FOLD-2 (cite precision): `optimize.rs` has THREE `fault_handler`-binding `FaultableCall` arms, not two** — `:1884` (the `resolved[]` remap), `:2042` (the `remap[]` block-renumber), `:2087` (`successors()`). §6's cite-map conflated `:2042`/`:2087`. A stale handler block-id at `:2042` is a silent miscompile. Update all three; each has a `FaultableBinOp { overflow_handler, divzero_handler, .. }` sibling 1-2 lines ABOVE it (`:1877`/`:2035`/`:2076`) as the exact per-category template to mirror.
- **FOLD-3 (lint target): do NOT extend `FAULT_OP_VARIANTS` (`tests/lints.rs:2634`)** — it counts `FaultOp` LIR enum variants (Add/Sub/Mul/Div/Rem/DivOverflow), and 2.1c adds NO new `FaultOp` (DivByZero routes through Div/Rem). Add a NEW SIBLING lint pinning the `FaultableCall` handler-category count (so 2.1d's Bounds is forced through). Also update the exact-string lint match at `tests/lints.rs:2709` for the renamed fields.
- **FOLD-4 (decision): Option A is the firm choice** (not "A unless churn is heavy"). Churn is light + mechanical: every `FaultableCall` `fault_handler` site has an adjacent `FaultableBinOp` per-category sibling as a proven template (printer, GIR→LIR `insts.rs:125`, the 3 optimize.rs arms). Option B (synthesized dispatch block at the call site) is a devbook/24-rule-2 regression — rejected.

## 0.7 PASS-2 BRIEF-REVIEW FOLD — v3 corrections (AUTHORITATIVE; pin the §3 tag-switch shape)

Pass-2 (fresh) re-derived the plan + confirmed FOLD-1..4 correct and the `..`-elision audit exhaustive. Three more folds pin the §3 tag-switch implementation:

- **FOLD-5 (the LIR reader's tag source — NO magic literal):** the tag-switch lives at the GIR→LIR split (`src/lir/lower/insts.rs:686`), which runs in the LIR lowerer and does NOT have `LoweringContext::resolve_variant_tag` (`context.rs:3490`). The LIR lowerer's equivalent is `self.resolve_variant_ordinal("Fault", <variant>)` (`src/lir/lower/operands.rs:567`, used at `insts.rs:1264`). Compute `OVERFLOW_TAG = resolve_variant_ordinal("Fault","Overflow") + 1`, `DIVZERO_TAG = …"DivByZero" + 1` — matching the callee writer's `resolve_variant_tag(...).map(|o|o+1)`. Do NOT hardcode `tag == 1`/`2`.
- **FOLD-6 (uncaught-category re-panic — the ONE implementable shape):** the split has NO access to `FaultScope` (the `div_overflow_panic`/`div_zero_panic` blocks live there), so it CANNOT "materialize a panic block" itself — DROP that reading from §3. Mirror the LOCAL `FaultableBinOp` precedent (`insts.rs:160-169`, comment `:162` "Both handlers are always `Some` — user entry or panic block"): the GATE (`calls.rs`) resolves each handler as `scope.overflow_handler.unwrap_or(scope.div_overflow_panic)` and `scope.divzero_handler.unwrap_or(scope.div_zero_panic)`, so `FaultableCall` carries ALWAYS-SOME per-category handlers. The split just routes by tag (==0 continue; ==OVERFLOW_TAG → overflow_handler; ==DIVZERO_TAG → divzero_handler) — no scope access at the reader. An uncaught-by-this-scope category routes to its panic block automatically (a DivByZero-only catch over an also-overflowing callee panics on overflow — correct).
- **FOLD-7 (FOLD-1 precision — the SECOND gate):** FOLD-1 named the slot-alloc gate `calls.rs:1427`; the FaultableCall-vs-plain-Call decision + handler thread is a SECOND site, `calls.rs:1445-1462` (`if let (Some(handler), Some(slot_place)) = …` → `builder.fault_call`/`fault_call_void`). Under Option A this emits a `FaultableCall` whenever `slot_place.is_some()` and threads BOTH resolved (always-Some) handlers; the builder ctors (`builder.rs:391/410`) change signature (single caller `:1453/1456`) so it's compile-forced — update BOTH `:1427` (alloc) and `:1445` (emit/thread).

(Non-blocking cite-drift the executor will see correctly: §6 `successors :2044`→ arm `:2087` [FOLD-2 authoritative]; §2.1 `pattern_catches_overflow :89/:116`→`:86/:113`; fill-call sites `:1034/:1127/:1142`.)

## 1. PREMISE VERIFICATION (every cite re-verified this session)

### 1.1 The Overflow slot-write site (the template to extend) — CONFIRMED

There is exactly ONE slot-write, in the **GIR lowering** (NOT the C/LLVM backend — both
backends inherit it from shared GIR/LIR). It lives in `fill_fault_return_block`
(`src/ir/lowering/functions.rs:117-170`):

```rust
// functions.rs:124-129 — the discriminant comes from the TYPED Fault enum registry
let overflow_tag = ctx
    .resolve_variant_tag("Fault", "Overflow")       // → 0; +1 = tag 1 (0 = "no fault")
    .map(|ord| (ord + 1) as i32)
    .unwrap_or(1);
// functions.rs:145-149 — the slot-write (CATCHING-caller arm, after a NULL-check branch)
builder.switch_to(write_bb);
builder.store_ref(Place::local(slot_local), Operand::Constant(Constant::I32(overflow_tag)));
```

The discriminant registry: `Fault` enum variants defined ONCE at
`src/ir/lowering/generics/substitute.rs:332/336/340` = `Overflow`(0)/`DivByZero`(1)/`Bounds`(2)
(semantic twin `src/semantic/resolve.rs:178`). Tags (ord+1): **Overflow=1, DivByZero=2,
Bounds=3** — matches the 2.1 scout's D2 table. `resolve_variant_tag`
(`context.rs:3490`) is the typed accessor — NO magic literal (devbook/24 rule 2). ✅

The fault-return block is created in `setup_fault_return_scope`
(`functions.rs:62-102`); it sets `overflow_handler: Some(fault_return_bb)` but
**`divzero_handler: None` and `bounds_handler: None`** (`functions.rs:94-97`). It is
filled at three sites in `lower_function` (block-body `:1031`, expr-body-tail `:1124`,
expr-body-terminated `:1149`) + flagged `participates_in_fault` at `:1167`.

**Backend (no change needed for the tag):** the slot-write is GIR `StoreRef`, lowered to
LIR `Inst::Store`, emitted by BOTH backends already (the C/LLVM `Inst::Call` emit + the
trailing-`i32*` param are 2.1a/2.1b; the deliverable's prototype confirms NO backend-emit
touch). The C signature emit reads `fault_slot_param_count` at
`src/backend/c_lir/mod.rs:930`; LLVM at `src/backend/llvm/mod.rs:2218`.
`LirFunction.fault_slot_param_count` def `src/lir/mod.rs:1459`.

**Equip methods do NOT participate** — only `lower_function` (standalone) has the
fault-slot plumbing; `lower_equip_method` has none, consistent with the participation
analysis collecting only bare `Expr::Identifier` direct calls. So NO sibling fill-sites in
methods (method-call propagation is 2.3b). ✅

### 1.2 DivByZero check site — NOT identical to Overflow (the TODO claim refuted)

The div/rem check + the `INT_MIN/-1` split live in `fault_handler_for`
(`src/ir/lowering/exprs/operators.rs:332-364`) — the GIR routing, NOT a backend if-trap:

```rust
// operators.rs:357-360 — Div/Rem route BOTH categories; rhs==0 → divzero, TYPE_MIN/-1 → overflow
BinOp::Div | BinOp::Rem => Some((
    Some(scope.overflow_handler.unwrap_or(scope.div_overflow_panic)),
    Some(scope.divzero_handler.unwrap_or(scope.div_zero_panic)),
)),
```

In a PARTICIPATING callee, `setup_fault_return_scope` left `divzero_handler: None`
→ a deep div0 routes to `scope.div_zero_panic` → **panics inline, never propagates.**
**MEASURED (baseline):** `int q(int a, int b): a / b` called as `q(10,0) catch
Fault.DivByZero: 999` printed the panic `division by zero` (NOT `999`).

So DivByZero is "the same slot-write MECHANISM" but needs:
1. a **second fault-return block** (`divzero_return_bb`) writing tag 2;
2. `setup_fault_return_scope` → `divzero_handler: Some(divzero_return_bb)`;
3. the **participation analysis** to recognize a `catch Fault.DivByZero:` scope
   (`pattern_catches_overflow` → must become `pattern_catches_arith`);
4. the **call-site gate** to pick the divzero handler when that's what the scope catches.

The C backend div0 trap (`c_lir/mod.rs:2476` per the 2.1 scout) and LLVM (`llvm/mod.rs`) are
the panic-by-default path for the UNCAUGHT case — unchanged; they only fire on the
NULL-slot arm of the callee's fault-return block (the `panic_bb` in
`fill_fault_return_block`). ✅

### 1.3 Bounds — a DIFFERENT mechanism (safe-get + NULL-branch), confirmed Phase-1.5

The LOCAL bounds machinery is fully present (the parallel read confirmed, file:line):
- GIR `Instruction::FaultableIndexLoad { dst, base, index, read, fault_handler }`
  (`src/ir/instructions.rs:250-256`).
- The gate `bounds_handler_for` (`src/ir/lowering/exprs/methods.rs:3476-3478`) — emits a
  `FaultableIndexLoad` ONLY when `fault_scope.bounds_handler` is Some AND the base is an
  array (`methods.rs:3412-3436`, typed `collection_kind` gate, not a name check).
- GIR→LIR split (`src/lir/lower/insts.rs:1193-1243`): calls `gorget_array_safe_get`
  (returns the element ptr or NULL on OOB — signed index, negatives are OOB), tests NULL,
  branches BEFORE deref.
- Runtime `gorget_array_safe_get` (`src/backend/c/runtime/runtime_array.c:41-44`) — the
  non-panicking variant; vs panicking `gorget_array_get` (`:31-37`).
- Uncaught bounds panic: C `Inst::BoundsCheck` (`c_lir/mod.rs:3116`, "index out of bounds")
  + LLVM (`llvm/mod.rs:6604`, "index out of bounds: index %lld, len %lld").

**Why Bounds is a SEPARATE shape, not "the same slot-write":** in a participating callee,
`setup_fault_return_scope` sets `bounds_handler: None`, so the callee's `v[i]` lowers to a
plain `Inst::BoundsCheck` PANIC — there is no fault-return block, no slot-write, and the
participation analysis (which scans `is_faultable_arith`, NOT index reads) never even marks
the callee. To make a deep bounds fault propagate, 2.1d must:
1. extend the participation analysis to detect **uncaught index reads** + a `catch
   Fault.Bounds:` scope (a NEW detector category — arith vs index-read);
2. wire `setup_fault_return_scope` → `bounds_handler: Some(bounds_return_bb)` so the
   callee's `v[i]` becomes a `FaultableIndexLoad` routing to a bounds-return block;
3. the bounds-return block writes tag 3 (`Bounds`) + NULL-check-panics by default — the
   SAME `fill_fault_return_block` shape, BUT the panic message is "index out of bounds";
4. handle the index-read **result-type** subtlety: a resource-element read yields `Ptr(T)`;
   `lower_fault_catch_expr` resolves this for the LOCAL case via `ensure_owned_at_boundary`
   (`exprs/mod.rs:3639`), but the cross-frame callee returns the sentinel `_0` and the
   CALLER reads the call result — so the result-type unification for a `Vector[String]`
   element differs from the arith (int) case. Needs its own drop-correctness fixture.

**MEASURED:** under the DivByZero prototype, deep bounds STILL panics (`getx(xs, 99) catch
Fault.Bounds: -1` → `index out of bounds`, NOT `-1`). Confirmed distinct + not covered. ✅

### 1.4 Fixtures — CONFIRMED the gating

Existing `tests/fixtures/fault_*`: all the LOCAL fault-catch fixtures
(`fault_catch_overflow/div0/binding/compound/drop/bounds/bounds_negidx/bounds_drop/
bounds_resource_mut/bounds_struct/intmin_div/intmin_partial/intmin_partial_divzero`) PASS
today as plain `run_gg`/`run_gg_panics` (integration.rs:5778-5913) — they are NOT
CcFailed/ignored at the Rust-gg level. The `fault_catch_bounds*`/`fault_catch_intmin_div`
"stay CcFailed until then" note in TODO refers to the **self-host** `GG_RUNTIME_DIFF`
diagnostic (the self-host compiler can't lower them yet) — a SEPARATE track (TODO §
"self-host fault-catch Inc2"), explicitly NOT in this scope.

The CROSS-FRAME fixtures are only `fault_deep_catch*` (Overflow) +
`fault_deep_fnvalue_panic` (the 2.1b adapter regression guard, now un-gated under LLVM).
**There is NO `fault_deep_catch_divzero` or `fault_deep_catch_bounds` fixture yet** — 2.1c
ADDS them. Nothing is `#[ignore]`'d pending 2.1c.

The self-host `Fault.Bounds` Inc2 item (TODO § error-model, item (1)) is the self-host
compiler's OWN local fault-catch lowering — orthogonal, separate scout→brief, NOT scoped
here. (Noted per the mission; not touched.)

---

## 2. THE PROTOTYPE (REVERTED — measured, not integrated)

Throwaway edits in 3 files, built clean (`cargo build` 0 errors, `cargo test --lib`
1084/0), MEASURED, then `git checkout --` reverted. The deliverable is the durable artifact.

### 2.1 Prototype diff (DivByZero — the Phase-1 part)

**(a) `src/ir/lowering/fault_participation.rs`** — generalize the catch-detection from
Overflow-only to arithmetic (Overflow OR DivByZero):

```rust
// rename pattern_catches_overflow → pattern_catches_arith
fn pattern_catches_arith(pattern: &FaultCatchPattern) -> bool {
    match pattern {
        FaultCatchPattern::Variant { variant, .. } =>
            matches!(variant.node.as_str(), "Overflow" | "DivByZero"),
        FaultCatchPattern::Binding(_) => true,
    }
}
// (3 call sites updated: :89, :116 in the two visitors)
```
(`is_faultable_arith` already includes Div/Rem, so condition (a) is satisfied unchanged.)

**(b) `src/ir/lowering/functions.rs:62-170`** — two fault-return blocks instead of one:

```rust
fn setup_fault_return_scope(...) -> (BlockId, BlockId) {   // was -> BlockId
    let overflow_return_bb = builder.new_block();
    let divzero_return_bb  = builder.new_block();
    // ... per-category panic blocks unchanged (now dead for a participating callee) ...
    ctx.func_state.fault_scope = Some(FaultScope {
        overflow_handler: Some(overflow_return_bb),
        divzero_handler:  Some(divzero_return_bb),   // was None
        bounds_handler:   None,                      // still None — Bounds is 2.1d
        div_overflow_panic, div_zero_panic,
    });
    (overflow_return_bb, divzero_return_bb)
}

// fill_fault_return_block gains `variant: &str, panic_msg: &str` params; the tag is
//   ctx.resolve_variant_tag("Fault", variant).map(|o|(o+1) as i32)  (typed, no magic)
// and the NULL-slot panic uses panic_msg.

// the 3 fill-call sites (:1031, :1124, :1149) each call it TWICE:
fill_fault_return_block(ctx, b, overflow_bb, slot, is_void, "Overflow",  "integer overflow");
fill_fault_return_block(ctx, b, divzero_bb,  slot, is_void, "DivByZero", "division by zero");
// (the `let fault_return_bb = ...map(...)` binding becomes `fault_return_bbs` : Option<(BlockId,BlockId)>;
//  `func.participates_in_fault = fault_return_bbs.is_some();`)
```

**(c) `src/ir/lowering/exprs/calls.rs:1369-1373`** — the gate routes to whichever
arithmetic category the scope catches:

```rust
let fault_overflow_handler = if callee_participates_in_fault {
    ctx.func_state.fault_scope.and_then(|s| s.overflow_handler.or(s.divzero_handler))
} else { None };
```
**⚠ This `.or()` is the PROTOTYPE shortcut that EXPOSES the category-confusion bug (§2.3)
— NOT the shippable form. The shippable form is the tag-dispatch (§3).**

### 2.2 MEASURED — DivByZero WORKS (C + LLVM)

| Case | Command | Measured stdout | Exit | Verdict |
|---|---|---|---|---|
| Deep div0 caught (C) | `q(10,0) catch Fault.DivByZero: 999` | `999` | 0 | ✅ caught |
| Deep div0 caught (LLVM) | same, `--backend=llvm` | `999` | 0 | ✅ parity |
| Deep div0 UNCAUGHT (C) | `print(q(10,0))` | `division by zero` (panic, at callee line) | 1 | ✅ panic-by-default |
| Deep Overflow regression (C) | `fault_deep_catch.gg` | `-1` | 0 | ✅ no regression |
| All LOCAL fault fixtures | `fault_catch_*` (10 fixtures) | unchanged | — | ✅ no regression |
| `cargo test --lib` | — | 1084 passed; 0 failed | — | ✅ |

### 2.3 ⚠ MEASURED — the category-confusion BUG (binding form / mixed callee)

```gorget
int q(int a, int b): a / b
void main():
    int r = q(10, 0) catch f: match f:
        case Fault.Overflow():  100
        case Fault.DivByZero(): 200
    print(f"{r}")
```
**Measured: `100` (the Overflow arm). CORRECT = `200` (DivByZero arm).** A silent
miscompile.

**Root cause (devbook/24 — the read site reconstructs a fact the write site dropped):** the
GIR `FaultableCall` carries a SINGLE `fault_handler` and the GIR→LIR split
(`src/lir/lower/insts.rs:686-717`) does a single `slot != 0` branch to it. The binding
form's `lower_fault_catch_expr` (`exprs/mod.rs:3582-3584`) creates per-category entry
blocks (`overflow_entry`/`divzero_entry`), but the gate passes only ONE
(`overflow_handler`, since the binding sets BOTH and `.or()` picks overflow). The callee
correctly writes tag 2 (DivByZero), but the caller's `!= 0` branch lands on
`overflow_entry`, which constructs `Fault.Overflow()` — the tag value is THROWN AWAY.

The LOCAL binding form is correct (`fault_catch_binding.gg` → `111`/`222`) because the
local `FaultableBinOp` routes div0 DIRECTLY to `divzero_entry` (no slot, no single
handler). The bug is specific to the cross-frame single-handler `FaultableCall`.

**This MUST be fixed in 2.1c, not deferred** — shipping DivByZero with `.or()` makes the
binding form (and any multi-category catch over a div-capable callee) a silent miscompile.
Per Core invariant #8, "both backends agree on `100`" is the WRONG answer, not a pass.

---

## 3. THE PRINCIPLED FIX — caller-side tag-dispatch (the load-bearing 2.1c design item)

The `FaultableCall`'s single `!= 0` branch cannot distinguish categories. The fix is to
make the caller **read the slot tag VALUE and dispatch to the matching category entry** —
mirroring how the LOCAL `FaultableBinOp` already routes per-category. Two implementation
shapes (executor + reviews pick; recommend A):

### Option A (recommended) — per-category handlers on `FaultableCall` + tag-switch split

Extend the GIR variant to carry the SAME per-category handler shape as `FaultScope`:
```rust
// src/ir/instructions.rs:365 — replace `fault_handler: BlockId` with:
FaultableCall {
    dst, func, args, fault_slot,
    overflow_handler: Option<BlockId>,
    divzero_handler:  Option<BlockId>,
    // (bounds_handler: Option<BlockId>  — added in 2.1d)
},
```
The gate (`calls.rs:1369`) reads all of `s.overflow_handler`/`s.divzero_handler` and
threads them (Some only for caught categories). The GIR→LIR split
(`src/lir/lower/insts.rs:686`) becomes, after the `Inst::Call`:
```text
  tag = load slot
  if tag == 0: goto continuation        # no fault
  if tag == OVERFLOW_TAG(1): goto overflow_handler   # if Some
  if tag == DIVZERO_TAG(2):  goto divzero_handler    # if Some
  # an UNCAUGHT category for THIS scope (handler None): panic-by-default
  #   — but the callee already panicked on the NULL-slot arm IF this caller
  #     didn't pass &slot; here the caller DID pass &slot (it catches SOMETHING),
  #     so a category it doesn't catch must re-panic. Materialize a panic block
  #     ("integer overflow"/"division by zero") keyed by the tag (the
  #     `div_overflow_panic`/`div_zero_panic` blocks already exist on the scope).
```
**Subtlety (must spec):** when a caller catches ONLY DivByZero but the callee can ALSO
overflow, the callee passed `&slot` and writes tag 1 (Overflow) — the caller must
PANIC (not silently fall through). Route an un-caught tag to the scope's
`div_overflow_panic`/`div_zero_panic` block (those already exist, `FaultScope` fields). The
tag-switch makes this correct + uniform across both backends (it's all shared LIR
`Inst::Cmp` + `Term::Branch`; neither backend special-cased).

The tags are read from the discriminant registry (`resolve_variant_tag`) at the GIR build —
single source of truth, no magic literal. Extend the arm-count lint
(`tests/lints.rs:2634`, `FAULT_OP_VARIANTS`) or add a sibling lint pinning the
`FaultableCall` handler-category count so the next category (Bounds) is forced through.

**⚠ Option-A sibling site the executor MUST update:** `tests/lints.rs:2702/2709` greps for
the EXACT strings `Instruction::FaultableCall {` and `Instruction::FaultableCall {
fault_handler, .. }` — changing the variant's fields breaks these lint match-strings, so
update them in the SAME change (and re-key the second to the new per-category field names).

### Option B — keep single `fault_handler`, point it at a synthesized dispatch block

The gate emits a small dispatch block (reads the slot tag, switches to the per-category
entries) and passes ITS id as the single `fault_handler`. Less invasive to the GIR variant
+ all §1.4 pass arms, but it puts the tag-dispatch construction at the call site
(`calls.rs`) rather than as typed metadata on the instruction — weaker on devbook/24 rule 2
(the dispatch logic is reconstructed at lowering rather than carried as typed handler
fields). **Recommend A** unless the §1.4 pass-arm churn (the `FaultableCall` arm in
successors/thread_jumps/liveness/validate/sim/printer — all must enumerate the new handler
fields) proves heavier than expected; A keeps the routing as typed metadata read once.

**Either way the tag-dispatch is REQUIRED for correctness, and is the bulk of 2.1c** —
the per-category tag-write (§2.1b) is the easy half; the dispatch is the half the "mostly
fixtures" claim missed.

---

## 4. EXECUTOR BRIEF OUTLINE

**Scope: 2.1c = DivByZero deep propagation (incl. the tag-dispatch). Bounds → 2.1d
(Phase 1.5), filed separately.** Sub-slices:

1. **2.1c-i — per-category callee tag-write** (`functions.rs`): `setup_fault_return_scope`
   → two return blocks; `fill_fault_return_block` parameterized by `(variant, panic_msg)`,
   tag from `resolve_variant_tag` (typed); `divzero_handler: Some(...)`. Update the 3
   fill-sites (block-body / expr-tail / expr-terminated) + the `participates_in_fault`
   flag. (PROTOTYPED §2.1b — lift directly.)
2. **2.1c-ii — participation analysis** (`fault_participation.rs`):
   `pattern_catches_overflow` → `pattern_catches_arith` ({Overflow, DivByZero}); update the
   module doc-comment (it says "Overflow only"). (PROTOTYPED §2.1a.)
3. **2.1c-iii — the tag-dispatch** (Option A): extend GIR `FaultableCall` to per-category
   handlers; thread ALL §1.4 pass arms (the 2.1 scout's enumerated list — successors
   `optimize.rs`, thread_jumps, liveness, tag_ownership, validate, sim, printer, builder
   ctors — each must handle the new fields, NOT just `fault_handler`); rewrite the GIR→LIR
   split (`insts.rs:686`) as the tag-switch with the uncaught-category re-panic; update the
   gate (`calls.rs:1369`) to thread both handlers; extend/add the arm-count lint. **This is
   the bulk — the executor must NOT treat it as a fixture round.**
4. **2.1c-iv — fixtures + lock-in:**
   - `fault_deep_catch_divzero.gg` → `q(10,0) catch Fault.DivByZero: 999` → stdout `999`.
   - `fault_deep_catch_divzero_binding.gg` → the binding-form `match f` → stdout `200`
     (the §2.3 bug REGRESSION guard — this is the fixture that catches Option-A
     correctness).
   - `fault_deep_uncaught_divzero_panic.gg` → no catch → panics `division by zero`, exit 1
     (`run_gg_panics`).
   - `fault_deep_catch_divzero_drop.gg` → a `Drop`-typed local live across the div0 in the
     callee → ASan/UBSan-clean, dropped once (the Q9 drop-gate, cross-frame div0 variant).
   - Snapshot the C stdout into `tests/fixtures/runtime_snapshots/`; each runs under
     `GG_BACKEND=llvm` too (parity).
   - A mixed-callee fixture: a callee with BOTH `a*b` and `a/b` caught by `catch
     Fault.DivByZero:` only → the overflow must PANIC, the div0 must be caught (the §3
     uncaught-category-re-panic guard).

**Self-host impact: NONE** (the self-host's own source keeps panic-on-overflow/div0; new
deep-fault fixtures register as not-yet-at-parity in the diagnostic `self_host_runtime_diff`
— honest, never excluding a self-host failure). `bootstrap_fixed_point` + frozen
`runtime_snapshots` untouched (new GIR fields default 0/false).

### Discriminant registry entries needed
**NONE new** — `Fault.DivByZero` (ord 1 → tag 2) already exists
(`generics/substitute.rs:336`, `resolve.rs:178`). 2.1c only READS it via
`resolve_variant_tag` at the new write/dispatch sites.

### Gate battery (parent runs the full sweep)
- `cargo build` (executor self-gates) + `cargo test --lib` (≥1084/0).
- `cargo test --test integration fault_` BOTH backends (C + `GG_BACKEND=llvm`) — the full
  ~29-fixture fault prefix + the new deep-divzero fixtures.
- `self_host_bootstrap_fixed_point` GREEN (zero self-host impact, but PROVE it).
- ASan/UBSan-clean on the drop-gate fixtures.
- Full `cargo test --test integration -- --test-threads=4` (C) + a `GG_BACKEND=llvm`
  fault-prefix sweep — **parent's job, not the executor's.**
- Extend the arm-count lint (`tests/lints.rs`) so the Bounds category (2.1d) is forced
  through the dispatch.

---

## 5. REFERENCE-GRADE / BOTH-BACKENDS CONCERNS (Core invariant #8)

- **The tag-dispatch is the reference-grade linchpin.** Without it, 2.1c ships a SILENT
  MISCOMPILE for the binding form (measured `100` for a div0). "Both backends agree on
  `100`" is NOT a pass — it is the exact phrasing the gate must trip. The output-review's
  acceptance bar is *the right `Fault` variant*, not "C == LLVM."
- **Panic-by-default must match across C/LLVM.** Measured: uncaught deep div0 panics
  `division by zero` (C) — the LLVM path inherits the same GIR `panic_bb` (NULL-slot arm),
  so parity holds; the executor must MEASURE the LLVM uncaught case (the prototype measured
  only the caught LLVM case + the uncaught C case; close that gap).
- **The uncaught-CATEGORY re-panic (§3 subtlety) must be uniform.** A caller catching only
  DivByZero, over a callee that overflows, must panic "integer overflow" on BOTH backends —
  driven from the shared `div_overflow_panic`/`div_zero_panic` GIR blocks, never a
  backend-emit conditional.
- **Bounds (2.1d) carries its own reference-grade gate** — the resource-element result-type
  unification + the negative-index (`gorget_array_safe_get` treats `index<0` as OOB) catch
  semantics must match the LOCAL `fault_catch_bounds_negidx.gg` behavior across frames.

---

## 6. CITE MAP (re-verified 2026-06-25, tip `e02627ae`)

| Structure | file:line |
|---|---|
| Overflow slot-write (the template) | `src/ir/lowering/functions.rs:145-149` |
| `overflow_tag` typed lookup | `functions.rs:124-129` (`resolve_variant_tag` `context.rs:3490`) |
| `setup_fault_return_scope` (one block, divzero/bounds None) | `functions.rs:62-102` (`:94-97`) |
| `fill_fault_return_block` | `functions.rs:117-170` |
| 3 fill-sites + participates flag | `functions.rs:1031 / 1124 / 1149 / 1167` |
| call-site gate (reads only `overflow_handler`) | `src/ir/lowering/exprs/calls.rs:1369-1373` |
| `Fault` enum variants (Overflow/DivByZero/Bounds = 0/1/2) | `src/ir/lowering/generics/substitute.rs:332/336/340` (sem twin `resolve.rs:178`) |
| `fault_handler_for` (div0/TYPE_MIN split) | `src/ir/lowering/exprs/operators.rs:332-364` |
| `lower_fault_catch_expr` (per-category entries — already generalized) | `src/ir/lowering/exprs/mod.rs:3552-3722` (`:3566` category split) |
| GIR `FaultableCall` (single `fault_handler`) | `src/ir/instructions.rs:365-379` |
| `FaultableCall` GIR→LIR split (single `!= 0` branch) | `src/lir/lower/insts.rs:686-717` |
| GIR `FaultableIndexLoad` (Bounds shape) | `src/ir/instructions.rs:250-256` |
| `bounds_handler_for` gate | `src/ir/lowering/exprs/methods.rs:3412-3436 / 3476-3478` |
| `FaultableIndexLoad` GIR→LIR (safe-get+NULL-branch) | `src/lir/lower/insts.rs:1193-1243` |
| `gorget_array_safe_get` / `gorget_array_get` | `src/backend/c/runtime/runtime_array.c:41-44 / 31-37` |
| C bounds panic / LLVM bounds panic | `src/backend/c_lir/mod.rs:3116` / `src/backend/llvm/mod.rs:6604` |
| C / LLVM fault-slot signature emit (`fault_slot_param_count`) | `c_lir/mod.rs:930` / `llvm/mod.rs:2218` (`LirFunction` field `src/lir/mod.rs:1459`) |
| arm-count lint (`FAULT_OP_VARIANTS=6` + FaultableCall presence) | `tests/lints.rs:2634 / 2693-2703` |
| §1.4 CFG/sim pass arms to thread (per 2.1 scout) | successors `optimize.rs:2044`, thread_jumps `:1865`, liveness `:403`, validate `:380/792`, printer `:517`, sim, builder ctors |

---

## 7. DOCS THE DESIGN RESTS ON

`error-model.md` §11.1 (fault membership: Overflow+DivByZero inline, Bounds via
`gorget_array_safe_get`+NULL-branch, OOM→Phase 2), §11.2 (branch-before-store CFG,
handler-bb constructs the `Fault` variant — the tag-dispatch realizes this for the
cross-frame case), §11.7 (shared-LIR-first sequencing); `error-model-inc21-scout.md` §2 D2
(3-variant tag space, slot discriminants), §6 (typed-discriminant + sibling-arm
discipline); DONE.md Inc-2.1a `a1076edc` + Inc-2.1b `022342e6`;
`docs/devbook/24-layering-discipline.md` (the §2.3 bug IS a rule-1/rule-2 violation — the
read site reconstructs the category the write site dropped; the fix is typed handler fields
at the source).
