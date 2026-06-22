# Error-model Inc-2.1 scout — concrete-Fault CROSS-FRAME propagation

**Status:** DESIGN/FEASIBILITY SCOUT, 2026-06-22, on `gorget-1` tip `ded34460`. Supersedes the
line-number table in `error-model-phase2-inc1-scout.md` (its `file:line` had drifted; regenerated
below). Resolves the 3 confirm-in-brief sub-decisions (D2/D3/D4) and **prototypes the load-bearing
hidden-slot ABI shape end-to-end in C (compiles + runs + ASan/UBSan-clean)**. Throwaway prototype —
report only, NOT integrated to gorget-1. Grounded in `error-model.md` §9.1, `error-model-phase2-design.md`
§4/§5, `error-model-phase2-A-vs-B.md` (owner-accepted A+hybrid).

**Scope (owner-confirmed):** single-call-deep concrete-`Fault` propagation via a new GIR `FaultableCall`
+ a hidden `MutPtr<tag>` out-param. **NO unwind.** New LIR shape in BOTH backends (C + LLVM). DEFER
`Fault equip Error` / the unified `Error` surface to 2.1b.

---

## 0. VERDICT — implementable on the current structure, NO architectural blocker

The design is **implementable as specified**, on the current GIR/LIR/backend structure, with **no
architectural blocker found**. Every mechanical piece has a shipped sibling to clone:
- The **GIR→LIR call-with-slot-check-and-branch** shape is a near-verbatim clone of
  `FaultableIndexLoad`'s lowering (`src/lir/lower/insts.rs:1151-1201`): emit the call, test the slot,
  `Term::Branch { fault → handler, ok → continuation }`, BRANCH BEFORE reading the result.
- The **hidden out-param ABI** reuses the `&`-param → `MutPtr` lowering wholesale
  (`compute_param_abi`/`resolve_param_type`, `context.rs:1680-1697`; `register_mut_ptr_type`,
  `context.rs:1667`; `ParamABI::ByMutPtr`).
- The **callee fault-return arm** is `lower_throw`'s shape (`stmts/mod.rs:2407-2438`) with the `Fault`
  tag + hidden slot replacing `Error(val)`/`LocalId(0)`.
- The **call-site gate** is the mirror of `fault_handler_for` (`operators.rs:332`), inserted at the one
  central user-call emit (`calls.rs:1403-1407`).
- The **handler-entry** reuses `lower_fault_catch_expr`'s existing per-category entry blocks
  (`exprs/mod.rs:3543-3713`) verbatim — they already exist for the local case.

**The prototype confirms the riskiest claim (D3 + Q9):** the hidden-slot C shape compiles
`-fsanitize=address,undefined`-clean, returns `-1` for the overflow demonstrator, AND the callee's
droppable local is freed exactly once on the fault path (`live_resources=0`). See §5.

**One drift worth flagging beyond line numbers:** the inc1 scout cited the runtime-diff test at
`tests/integration.rs:17401`; the actual `self_host_runtime_diff` machinery is at `:17121`/`:17262`.
No semantic change — re-cite for any brief.

---

## 1. Regenerated cite map (every line re-verified this session; DRIFT flagged)

### 1.1 Phase-1 fault machinery (the substrate Inc-2.1 extends)

| Structure | inc1-scout cite | ACTUAL (this session) | Drift |
|---|---|---|---|
| `Instruction::FaultableBinOp` | `instructions.rs:222` | `:222` | exact |
| `Instruction::FaultableIndexLoad` | `:250` | `:250` | exact |
| `Instruction::Call` / `CallIndirect` | `:332`/`:339` | `:332`/`:339` | exact |
| `Inst::FaultCheck` (LIR) | `lir/mod.rs:857` | `:857` | exact |
| `enum FaultOp` (6 variants) | `lir/mod.rs:308` | `:308` | exact |
| `Inst::Call` (LIR) def | — | `lir/mod.rs:901` | new cite |
| GIR→LIR fault split | `lir/lower/insts.rs:125-202` | `:125-201` (`FaultableBinOp`); `FaultableIndexLoad` split `:1151-1201` | **±, FaultableIndexLoad cite NEW** |
| C `FaultCheck` emit | `c_lir/mod.rs:2570-2597` | `:2570` | exact |
| LLVM `FaultCheck` emit | `llvm/mod.rs:3627-3678` | `:3627` | exact |
| `FaultScope` struct | `context.rs:286-316` / `:295` | `:286` (field) / `:295` (struct) | exact |
| FaultScope clear-at-call comment | `context.rs:281` | `:281` | exact |
| `lower_fault_catch_expr` | `exprs/mod.rs:3543-3713` | `:3543` | exact |
| `fault_handler_for` gate | `operators.rs:332` | `:332` | exact |
| `Fault` enum `builtin_fault_enum` | `generics/substitute.rs:323` | `:323` (variants `:332/336/340`) | exact |
| `Fault` semantic reg | `resolve.rs:175` | `:175` | exact |
| Arm-count lint | `lints.rs:2462` (`FAULT_OP_VARIANTS=6`) | `:2462`/`:2464` | exact |

### 1.2 The by-value contract template Inc-2.1 CLONES

| Structure | inc1-scout cite | ACTUAL | Drift |
|---|---|---|---|
| `current_throws_result_type` set | `functions.rs:762` | `:762` (also method `:1116`) | exact |
| throws → Result return synth | `mod.rs:629-643` | `synthesize_throws_result_type` @ `mod.rs:634`, `:1000` | semantics OK |
| `lower_throw` (cleanups+drops+ret) | `stmts/mod.rs:2407` | `fn lower_throw` @ `:2407`; cleanups `:2436`, drops `:2437`, ret `:2438` | exact |
| setjmp fallback (non-throws arm) | `stmts/mod.rs:2439-2442` | `:2441` (`call_extern("gorget_throw", …)`) | exact |
| auto-prop hook fire | `exprs/mod.rs:78-88` | `:78` (suppress consume) | exact |
| `emit_result_auto_propagate` | `exprs/mod.rs:2922` | `:2922` | exact |
| `should_auto_propagate` | — | `exprs/mod.rs:3197` | new cite |
| `maybe_auto_propagate` | — | `exprs/mod.rs:3219` | new cite |
| `lower_catch_expr` | `exprs/mod.rs:3385` | `:3385` | exact |

### 1.3 The ABI plumbing Inc-2.1 REUSES (D3)

| Structure | ACTUAL | Note |
|---|---|---|
| `register_mut_ptr_type` | `context.rs:1667` | allocates `GirType::MutPtr(pointee)` |
| `resolve_param_type` (`&` → MutPtr) | `context.rs:1680` | `MutableBorrow → register_mut_ptr_type` |
| `compute_param_abi` (`&` → ByMutPtr) | `context.rs:1691` | `MutableBorrow → ParamABI::ByMutPtr` |
| sret / aggregate-return ABI | `lir/lower/types.rs:392-447` | the alternative tagged-return form |

### 1.4 The CFG/sim passes a new `FaultableCall { fault_handler }` MUST thread

These are the **same arm-sites** the Inc1/Inc2 `FaultableBinOp`/`FaultableIndexLoad` arms touch — fully
enumerable, no hidden sites:
- **`successors()` `optimize.rs:2044-2073`** — the per-instruction handler-as-successor enumeration
  (`FaultableBinOp` → push `overflow_handler`/`divzero_handler`; `FaultableIndexLoad` → push
  `fault_handler`). **`FaultableCall` adds one arm here** (push `fault_handler.0`). *Without this the
  handler block is dead-code-eliminated — the single most important non-obvious site.*
- **`thread_jumps` block-id remap `optimize.rs:1865-1875`** — remaps `overflow_handler`/`divzero_handler`/
  `fault_handler` after block renumbering. Add a `FaultableCall` arm.
- **`liveness.rs`** — def at `:299`/`:307`; uses at `:379`/`:395`. Add `FaultableCall` to the
  def-list (its `dst`) + use-list (`args`).
- **`tag_ownership.rs:245`** — `FaultableIndexLoad` ownership tagging; `Call` tagging at `:142`. A
  `FaultableCall` tags its `dst` like a `Call`.
- **`validate.rs`** — def/use enumeration at `:301`/`:326`/`:702`/`:710`/`:759`/`:775`/`:1471`/`:2958`/
  `:2966`. Add `FaultableCall` arms (or alias to `Call`'s arms where the handler isn't read).
- **`sim/dispatch.rs`** — `FaultableBinOp`/`FaultableIndexLoad` at `:63/67/879/984`; `Call` at
  `:83/1152`. A `FaultableCall` simulates as a `Call` + a slot-check branch.
- **`optimize.rs` DCE/replace** — `:457`/`:542`/`:555`/`:1676`/`:1714`/`:1730` (def/use scan).
- **`printer.rs:504`** (`Call` printing) — add a `FaultableCall` arm for IR dumps.
- **GIR builder** (`builder.rs:366-378`, `call`/`call_void`) — add a `fault_call` constructor.

### 1.5 The ONE call-site emit point the gate inserts at

`lower_call`'s central user-function emit: **`calls.rs:1403-1407`** —
```rust
let result = if ret_type == UNIT_TYPE {
    builder.call_void(&call_name, lowered_args);          // ← gate here
    Operand::Constant(Constant::Unit)
} else {
    let dst = ctx.call_tracked(builder, &call_name, lowered_args, ret_type);  // ← and here
```
This is the SOLE place a plain user-function `Instruction::Call` is emitted for the
`Expr::Call(Identifier)` path. The secondary callable/method-dispatch emit is `calls.rs:1556-1577`
(`FnPtr`/method) — that is the **indirect/method seam, OUT of 2.1** (lands at 2.3/2.3b).

---

## 2. The 3 confirm-in-brief sub-decisions — RESOLVED

### D2 — the Fault tag's variants → CONFIRM `{Overflow, DivByZero, Bounds}` (3 variants)

**Evidence:** `builtin_fault_enum()` (`substitute.rs:323`) defines exactly 3 Unit variants —
`Overflow` (`:332`), `DivByZero` (`:336`), `Bounds` (`:340`). `lower_fault_catch_expr` already maps
its catch patterns onto exactly these three (`exprs/mod.rs:3557-3566`). `FaultOp` (`lir/mod.rs:308`)
has 6 *op* variants (Add/Sub/Mul/Div/Rem/DivOverflow) but they collapse to the same 3 *fault
categories* — the arm-count lint pins `FAULT_OP_VARIANTS=6` (`lints.rs:2464`).

**Recommendation: CONFIRM the 3-variant tag space for 2.1.** The slot is `i32`: `0 = no fault`, `1 =
Overflow`, `2 = DivByZero`, `3 = Bounds` — the discriminants of the closed `Fault` enum.
- `Overflow`/`DivByZero` are the arithmetic demonstrator (`FaultableBinOp` today).
- `Bounds` threads identically (it's already a `FaultableIndexLoad`; a deep bounds fault is the same
  slot-write).
- **OutOfMemory stays OUT** (still scattered `exit(1)` in allocators with no slot path — `error-model.md`
  §11.1 keeps it Phase-2-later). Adding it would force allocator rework, out of 2.1's mechanical scope.

The slot tag MUST be driven from the `Fault` enum's discriminant registry (typed, not a magic literal)
so D2's "3 variants" is single-source-of-truth — see the layering note in §6.

### D3 — the hidden-slot ABI → CONFIRM a hidden trailing `MutPtr<i32-tag>` out-param

**The mechanism (PROTOTYPED end-to-end in C, §5):**
1. **Slot allocation — CALLER frame.** The catching caller allocates an `i32` slot, zero-initialized
   (`= FAULT_NONE = 0`). In GIR: `builder.add_local(I32_TYPE, …)` + an initializing `Store 0` before
   the call. (Zero-init is the "no fault" invariant — the callee only ever WRITES the slot on a fault,
   never clears it, so the caller's zero is the happy-path value.)
2. **Threading into the call.** The participating callee's lowered signature gets a **synthesized
   trailing param** `MutPtr<i32>` appended AFTER the user params. The call passes `&slot` as the last
   arg. This reuses `register_mut_ptr_type` (`context.rs:1667`) + `ParamABI::ByMutPtr`
   (`compute_param_abi`, `:1691`) — the EXACT lowering an `&out` param already gets. No new ABI kind.
3. **Callee writes the slot.** The callee's faultable-op lowering gets a third disposition (today: local
   handler, or panic). The new "participating callee, no local handler" arm: `Store FAULT_<tag>` into
   `*__fault`, run `emit_early_exit_drops` (the `lower_throw` shape, `stmts/mod.rs:2437`), return a
   sentinel `0`. The slot is the side-channel; the return register is unused on the fault path.
4. **Caller checks AFTER the call, branches BEFORE reading the result.** The `FaultableCall` GIR→LIR
   split (clone of `FaultableIndexLoad`, `insts.rs:1182-1196`): after the `Inst::Call`, emit
   `flag = (slot != 0)` then `Term::Branch { flag → handler, !flag → continuation }`. The continuation
   reads the call's result; the handler-entry block is `lower_fault_catch_expr`'s existing per-category
   entry. **Branch-before-read** = the call's sentinel result is never consumed on the fault path → no
   corrupted value (§3.1 stays MOOT, same as Phase 1).
5. **Propagation up.** N-frames is 2.2: a *middle* frame that is itself participating-but-not-catching
   threads its OWN slot down AND, on a callee fault, writes its OWN caller's slot + early-exits. 2.1 is
   the single hop (catch is the caller's direct parent).

**LIR/backend structures that change (D3 work breakdown, both backends):**
- **GIR:** add `Instruction::FaultableCall { dst: Option<ValueId>, func, args, fault_slot: LocalId,
  fault_handler: BlockId }` next to `Call` (`instructions.rs:332`) — a SEPARATE variant keeps all 50+
  `Call` sites untouched (same rationale `instructions.rs:215` gives for `FaultableBinOp`). Plus the
  signature extension: a `bool participates_in_fault` flag on the function decl + the synthesized
  trailing `MutPtr` param.
- **GIR→LIR (`src/lir/lower/insts.rs`):** new arm lowering `FaultableCall` → `Inst::Call` + slot-load +
  `Inst::Cmp{Ne, slot, 0}` + `Term::Branch`. ~30 lines, modeled on `:1182-1196`.
- **C backend (`c_lir/mod.rs`):** the `Inst::Call` emit at `:2924` is UNCHANGED — the slot is just
  another pointer arg + the branch comes from the shared LIR. Only the signature-emit (append the
  `int32_t*` param) + the caller-frame slot decl + zero-init need touching. **Lightest-touch backend.**
- **LLVM backend (`llvm/mod.rs`):** `Inst::Call` emit at `:4151`; signature build appends an `i32*`
  param; the branch is shared LIR. Structurally closer (already branch-shaped).

**Recommendation: hidden trailing `MutPtr<i32-tag>` out-param.** It reuses the `&`-out-param lowering
wholesale (the prototype proves the C is trivial + clean), is the simplest to thread through both
backends, and adds zero new `ParamABI` kind. The tagged-return-register alternative
(`lir/lower/types.rs:392-447`) avoids one stack slot but complicates the return ABI in both backends
(the §5 cost gate's "branchless csinc" ideal favors a register, but that's a 2.x optimization — the
out-param is the correct *first* shape; correctness before the csinc micro-opt). **Owner picks; the
out-param is the recommended default and everything downstream is mechanical either way.**

### D4 — FaultCatch accepts a Call inner → CONFIRMED, no new front-end

**Evidence:** `Expr::FaultCatch { expr: Box<Spanned<Expr>>, pattern, handler }` (`ast.rs:600-604`) — the
inner `expr` is an **arbitrary expression**, so `faulty(BIG, BIG) catch Fault.Overflow: -1` already
parses (`expr.rs:1119-1128`), resolves (`resolve.rs:1853`), and typechecks (`typecheck.rs:3072-3102`)
TODAY. Phase-1 added the whole `FaultCatch` AST/grammar/typecheck distinct from contract `catch`
(DONE `8ab75635`). The visitor/rewrite/meta/safety passes all already recurse into the inner expr
(`visitor.rs:272`, `rewrite.rs:444/800`, `meta.rs:2436`, `safety/check_expr.rs:1047`).

**What 2.1 adds is ONLY the call-site LOWERING gate, NOT grammar/typecheck.** Today
`lower_fault_catch_expr` (`exprs/mod.rs:3543`) pushes the `FaultScope` (`:3607`) and lowers the inner
(`:3617`). When inner is `a*b`, `lower_binary_op` consults the scope (`fault_handler_for`,
`operators.rs:332`) → emits `FaultableBinOp` routing to the handler entries. When inner is `faulty(…)`,
the call lowering (`calls.rs:1403`) does NOT consult the scope → emits a plain `Call` (panic-deep). **The
ONE 2.1 change to the front-end:** make the call emit consult `fault_scope` + the callee's
`participates_in_fault` flag, and when both hold, emit `FaultableCall` routing to the SAME handler
entries. The handler-entry blocks, the merge, the binding-form `Fault` materialization
(`exprs/mod.rs:3692`) are all REUSED unchanged — a local `a*b` and a deep `faulty(a,b)` in the same
`catch` converge on one handler set and one merge.

**Recommendation: CONFIRMED — no new grammar/typecheck. One call-site lowering gate.** (The composition
is *additive over Phase-1 local catch* exactly as the design says — one handler set, one `catch`.)

---

## 3. The minimal 2.1 slice + fixture sketch

**The smallest faultable-op-propagating-across-one-frame case** (from `phase2-design.md` §4):
```gorget
int faulty(int a, int b): return a * b              # may overflow; NO local catch
void main():
    int r = faulty(BIG, BIG) catch Fault.Overflow: -1
    print(f"{r}")                                   # expects -1, NOT a panic
```
Deterministic stdout `-1` on x86_64 CI (`BIG` chosen so `BIG*BIG` overflows i64, e.g. `2^62`).

**Fixture set (`tests/fixtures/` + `runtime_snapshots/`):**
1. `fault_deep_catch.gg` — the demonstrator above → stdout `-1`.
2. `fault_deep_catch_divzero.gg` — `int q(int a, int b): return a / b` caught one frame up → confirms
   the DivByZero tag threads (D2 second variant).
3. `fault_deep_catch_drop.gg` — the **Q9 drop-gate**: `faulty`'s frame holds a live `Drop`-typed local
   (a `Vector[int]` or a resource struct), caught one frame up. **Run under ASan/UBSan; assert no leak
   + no double-free.** (The prototype §5 proves this shape is clean.)
4. `fault_deep_uncaught_panic.gg` — a deep fault with NO catch in the caller → still `exit(1)` (a
   `should_panic`/non-zero-exit negative fixture; preserves panic-by-default).
5. Both-backend lock-in: each runs under `GG_BACKEND=llvm` too; snapshot the C stdout into
   `runtime_snapshots/`.

**Self-host impact: NONE (no regression).** The self-host's own source keeps panic-on-overflow (it
won't use deep fault-catch), so `bootstrap_fixed_point` + the frozen `runtime_snapshots` are untouched.
New deep-fault fixtures the self-host can't compile yet register as not-yet-at-parity in the diagnostic
`self_host_runtime_diff` (env-gated `GG_RUNTIME_DIFF=1`, `tests/integration.rs:17121`/`:17262`) — honest,
never inflating parity (it never *excludes* a self-host failure).

---

## 4. Prototype OR feasibility assessment — BOTH (focused prototype + honest sub-slicing)

**I prototyped the load-bearing piece (the hidden-slot ABI shape, D3 + Q9) end-to-end in C, by hand —
NOT the full compiler-pass implementation.** Rationale: the full Rust implementation (new GIR variant +
signature extension + participating-callee analysis + 9 CFG/sim pass arms + both backend emits + the
front-end gate) is a **multi-pass funded round** — comparable to a Phase-1 increment, too large to land
*cleanly* in a scout window without leaving a half-wired tree. But the single highest-RISK claim — "a
hidden `MutPtr` slot can carry a fault across a frame, drop-correctly, without unwind, in the C the
backend would emit" — is exactly what a hand-written C prototype DECIDES. It passed (§5). Everything
else is mechanical cloning of shipped siblings (§0).

**This is the honest split:** the *mechanism* is de-risked (prototype-proven); the *plumbing* is bounded
(enumerated in §1.4) but voluminous. So: design-confirmed + mechanism-prototyped; the executor implements
the plumbing.

### Incremental sub-slices for the executor brief

- **2.1a — GIR `FaultableCall` + GIR→LIR split + C backend + the front-end gate + participating-callee
  flag, single hop only.** The bulk. Ships fixtures 1, 3, 4 on C. *This is where the real work is.*
  - 2.1a-i: add the GIR variant + builder ctor + thread all §1.4 pass arms (no behavior yet — a
    `FaultableCall` with a no-op handler lowers identically to `Call`; gate it behind the front-end not
    yet emitting it). Land green.
  - 2.1a-ii: the signature extension (synthesized `MutPtr` trailing param + `participates_in_fault`
    flag) + the callee fault-return arm.
  - 2.1a-iii: the call-site gate (`calls.rs:1403`) + the GIR→LIR slot-check-branch + C emit. Fixtures.
- **2.1b — LLVM backend at parity.** Append the `i32*` param in the LLVM signature build; the branch is
  shared LIR. `GG_BACKEND=llvm` sweep.
- **2.1c — the DivByZero + Bounds tags** (fixture 2 + a deep bounds fixture). Mostly fixtures once the
  Overflow path works — the slot-write is the same.

**DEFER past 2.1 (unchanged from the design's sequencing):** 2.2 N-frames transitive threading → 2.3
generics (Q13 Seam B) → 2.3b indirect/method calls (`calls.rs:1556-1577`; slot enters the callable
TYPE — the named weak point of "hidden ≠ signature change") → 2.4 Task/TaskGroup boundary (the only
runtime-touching one) → 2.5 `meta`/const-eval. **2.1b (`Fault equip Error` / unified `Error` surface) is
GREENFIELD and independent** — defer per owner scope; 2.1's catch binds a concrete `Fault` (the existing
binding form), no `Error` trait needed.

---

## 5. The prototype (committed; DO NOT INTEGRATE)

Two hand-written C programs modeling the exact shape the Inc-2.1 C backend must emit. Both compile
`-O0 -Wall -fsanitize=address,undefined` and run clean.

**`/tmp/inc21_proto/proto.c` — the minimal demonstrator.** `faulty(a,b,&slot)` with a hidden trailing
`int32_t *__fault`; on `__builtin_mul_overflow` it sets `*__fault = FAULT_OVERFLOW` and returns a
sentinel; `main` allocates a zero-init slot, calls, tests `__fault_slot != 0` BEFORE reading the result,
branches to the `catch Fault.Overflow: -1` handler. **Output: `-1`. Exit 0. ASan/UBSan clean.**

**`/tmp/inc21_proto/proto_drop.c` — the Q9 drop-gate.** The callee `faulty` frame holds a live
heap-owned `Res` local; on the fault path it runs `res_drop(&local)` (the `emit_early_exit_drops` shape)
BEFORE writing the slot and returning; the fault is caught one frame up in `caught()`. **Output: `-1`
then `live_resources=0`. Exit 0. ASan/UBSan clean** — the droppable local is freed exactly once on the
fault path, no leak, no double-free. This is the load-bearing correctness proof for the "fault past a
frame still runs drops, without unwind" claim.

Both files are under `/tmp/inc21_proto/` (throwaway scratch, not in the worktree); the design doc is the
durable artifact. No `.gg` fixture or Rust code was changed — the prototype is C-only by intent (it
proves the *backend output shape*, which is the riskiest part; the compiler plumbing that *generates*
this shape is the executor's mechanical work).

---

## 6. Layering / no-name-matching notes for the brief

- **The slot tag is the `Fault` enum's discriminant — read it from the typed registry, NOT a magic
  literal.** D2's "3 variants" must be single-source-of-truth: the `0/1/2/3` the slot carries are the
  `Fault` enum discriminants (`substitute.rs:323`), so the gate writes them via the enum's discriminant
  lookup, and the arm-count lint (`lints.rs:2462`) extends to pin the slot-tag arm count. No
  `if name == "Overflow"` substring routing.
- **`participates_in_fault` is a TYPED flag on the function decl, set at the source (the lowering pass
  that detects an uncaught faultable op in the body), read via an accessor at the call-site gate.** NOT
  a name-prefix or a re-derivation at the call site. 2.1 is conservative — any fn whose body contains a
  reachable uncaught faultable op is a candidate; 2.2/3b refine reach. This is the "write the flag at the
  source, read it downstream" discipline (`docs/devbook/24`).
- **Sibling-site discipline:** the `FaultableCall` arm must be added to ALL §1.4 pass sites in ONE
  change (successors / thread_jumps / liveness / tag_ownership / validate / sim / optimize-DCE /
  printer), and the `successors()` arm (`optimize.rs:2062`) is the one a forgotten arm silently breaks
  (handler DCE'd → fault recovery vanishes). Extend the arm-count lint to force the next sibling through.

---

## 7. Cite map (docs the design rests on)

`error-model.md` §9.1 (owner A+hybrid), §6 (by-value contract leg), §3/§3.1 (ubiquity + partial-state),
Q14 (out-of-band structured enum + dynamic match), §11.1 (`Fault` membership / OOM-deferred), §11.2
(Phase-1 `FaultScope`/CFG template), §11.7 (sequencing); `error-model-phase2-design.md` §1 (linchpin),
§3a (hidden slot), §4 (Inc-2.1 + demonstrator), §5 (lean-slot BINDING instruction + indirect-call seam,
Q9 re-entrancy=abort); `error-model-phase2-A-vs-B.md` (A+hybrid verdict); `error-model-phase2-inc1-scout.md`
(prior scout — line numbers superseded by §1 here); `docs/devbook/24-layering-discipline.md` (the typed-flag
+ sibling-arm discipline §6 rests on).
