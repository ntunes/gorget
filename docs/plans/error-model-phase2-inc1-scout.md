# Error-model Phase 2 — Increment 2.1 scout (first implementable increment)

**Status:** READ-ONLY DESIGN SCOUT, 2026-06-22. Reconciles `error-model.md` §9.1 +
`error-model-phase2-design.md` §4 + `error-model-phase2-A-vs-B.md` (owner-accepted
A+hybrid) against CURRENT source. NOT a brief — surfaces the first increment + the
open owner-decisions that gate it. Every cited `file:line` below was re-verified this
session (the design docs' line numbers have DRIFTED — see the verification table in §6;
the *semantics* hold, the line numbers must be regenerated for any brief).

---

## 0. What is already SHIPPED (the baseline Inc-2.1 builds on)

Phase 1 (LOCAL catch) is fully landed in BOTH backends AND the self-host (DONE.md
`8ab75635` Inc1, `a447c726` Inc2, `f5287ff5`/`bf0dd44b` self-host). Concretely present:

- **GIR faultable ops** — `Instruction::FaultableBinOp { dst, op, type_id, lhs, rhs,
  overflow_handler: Option<BlockId>, divzero_handler: Option<BlockId> }`
  (`src/ir/instructions.rs:222`) and `FaultableIndexLoad { …, fault_handler: BlockId }`
  (`:250`). Each carries an embedded **handler `BlockId` in the SAME function**.
- **GIR→LIR split** — lowering splits the block: emit `Inst::FaultCheck { dst, op:
  FaultOp, ty, lhs, rhs }` (a FLAG, no trap) + `Term::Branch { flag → handler, !flag →
  continuation }`, then compute `dst = lhs op rhs` (WRAP) in the continuation
  (`src/lir/lower/insts.rs:125-202`; `Inst::FaultCheck` at `src/lir/mod.rs:857`; `FaultOp`
  6 variants at `:308`).
- **Backend emit** — flag-compute is `__builtin_*_overflow`/`rhs==0`/`TYPE_MIN&&-1`
  (C, `src/backend/c_lir/mod.rs:2570-2597`) and `@llvm.*.with.overflow`/`icmp`
  (`src/backend/llvm/mod.rs:3627-3678`); the branch comes from the shared LIR.
- **`FaultScope`** on `func_state` (`src/ir/lowering/context.rs:286-316`):
  `overflow_handler`/`divzero_handler`/`bounds_handler: Option<BlockId>` +
  `div_overflow_panic`/`div_zero_panic: BlockId`. Pushed/popped scoped at
  `exprs/mod.rs:3606-3619`; read at `operators.rs:337` / `methods.rs:3477`.
- **`lower_fault_catch_expr`** (`src/ir/lowering/exprs/mod.rs:3543-3713`), cloned from
  `lower_catch_expr` (`:3385`): builds per-category handler-entry blocks + a merge
  block; the handler-entry **materializes the `Fault` variant** via
  `emit_enum_init_owned(builder, "Fault", variant, …)` (`:3689`) for the binding form.
- **`Fault` enum** — closed, 3 Unit variants `Overflow`/`DivByZero`/`Bounds`
  (`builtin_fault_enum()`, `src/ir/lowering/generics/substitute.rs:323-347`); semantic
  registration qualified-only (`src/semantic/resolve.rs:175-184`); eager IR
  registration (`mod.rs:237-242`).
- **Arm-count lint** — `fault_op_lowering_arms_count` (`tests/lints.rs:2462`,
  `FAULT_OP_VARIANTS = 6`).

**The Phase-1 lexical-reach guarantee, exactly as it works:** a callee's faults are
lowered in a SEPARATE function pass whose `func_state` (hence `fault_scope`) **starts
fresh** (`operators.rs:304-309`). So a callee never sees the caller's scope — that IS
the "cleared at call boundary" the `context.rs:281` doc comment describes (the
mechanism is per-function fresh state, not an explicit clear; the self-host achieves
the same via a fresh per-function `LowerCtx`, DONE.md `2a36bc2f`). **This per-function
seam is precisely what Inc-2.1 reaches across.**

**The by-value contract channel Inc-2.1 REUSES** (linchpin, all re-verified):
- `throws E` → `Result[T,E]` value-union return (`functions.rs:762`,
  `synthesize_throws_result_type` via `mod.rs:629-643`).
- `throw` in a `throws` fn builds `Error(val)`, runs `emit_on_error_cleanups` +
  `emit_early_exit_drops` + `builder.ret` (`stmts/mod.rs:2413-2438`); the setjmp
  fallback (`gorget_throw`) is only the non-`throws` ill-typed arm (`:2439-2442`).
- Each receiving call site auto-propagates: `maybe_auto_propagate` (hook fired at
  `exprs/mod.rs:78-88`) → `emit_result_auto_propagate` (`:2922`), Error path re-wraps in
  the current frame's `Result` and returns by value (`:3057`) after the same cleanups +
  drops (`:3080-3082`). Fires only when the enclosing fn can propagate
  (`should_auto_propagate`, `:3197-3211`).
- **So drop-correct deep by-value propagation ALREADY ships.** Inc-2.1 clones this
  template with a hidden `Fault` slot instead of the visible `Result`.

**Pre-impl cost gate** is SATISFIED (DONE/docs `b7ef79d5`: ~0.016 ns/frame happy-path,
the ideal slot lowers to a branchless `csinc`). The BINDING instruction it adds: lower
the slot as a **hidden out-param / tagged-return register, NOT the fat `Result`-union
struct-return** `throws` uses today (the current `Result` lowering is ~9× the floor —
redundant memcpys).

---

## 1. The first implementable increment — Inc-2.1

**Inc-2.1: single-call-deep fault catch via a hidden by-value fault-slot, additive
over Phase-1 local catch.** A fault raised in a DIRECTLY-called function flows to the
caller's lexical `catch` via a compiler-synthesized side-channel — no unwind, no
fault on any user signature.

### 1.1 The LIR/ABI shape (hidden fault-slot)

Three mechanical pieces, each modeled on a shipped sibling:

**(a) Callee side — a function that contains uncaught faultable ops gets a hidden
fault-return slot.** Today a faultable op inside a fault scope branches to a same-frame
handler; OUTSIDE a scope it panics. Inc-2.1 adds a third disposition for the
*participating-callee* case: instead of panicking, set the hidden slot to the `Fault`
discriminant and do an `emit_early_exit_drops` return (the existing drop machinery).
Concretely:
- Extend the function's lowered signature with a synthesized **out-param** (a
  `MutPtr<Fault-tag>` appended after the user params — the codebase already lowers `&`
  params as `MutPtr` out-params, `context.rs:1679`, and has sret/out-param ABI
  plumbing, `src/lir/lower/types.rs:392-447`), OR a **tagged-return register** widening
  the return ABI. The §5 instruction mandates the lean register/out-param form, NOT a
  `Result`-union struct.
- The callee's faultable-op lowering gets a "no local handler but I am a
  participating callee" arm: write the tag into the out-param slot, run
  `emit_early_exit_drops`, return a sentinel value. This is `lower_throw`'s shape
  (`stmts/mod.rs:2413-2438`) with the `Fault` tag + hidden slot replacing
  `Error(val)`/`LocalId(0)`.

**(b) Call-site side — a CALL to a participating callee, when lexically inside a fault
scope, checks the slot and branches to the handler.** The clean parallel to the
shipped `FaultableBinOp`/`FaultableIndexLoad` (both carry `fault_handler: BlockId`) is
a new **`Instruction::FaultableCall { dst, func, args, fault_handler: BlockId }`**
(or extend `Call` — but a SEPARATE variant keeps all 50+ existing `Call` sites
untouched, exactly the rationale `instructions.rs:219` gives for `FaultableBinOp`). Its
GIR→LIR lowering mirrors `FaultableIndexLoad`'s NULL-branch shape: emit the call,
**test the hidden slot**, `Term::Branch { fault → handler, ok → continuation }`. The
handler-entry block is the SAME `lower_fault_catch_expr` machinery — it materializes the
`Fault` variant from the slot's tag and runs user recovery. The `(Call dst, args)`
slot at `instructions.rs:332` is the clean insertion point; `CallIndirect` (`:339`,
"reserved, not emitted") is the future indirect seam (2.3b), out of 2.1.

**(c) Composition with Phase-1 local catch — additive, ONE handler set.** The caller's
`FaultScope` already holds the handler `BlockId`s. Inc-2.1 makes a `FaultableCall`
inside a scope route to the *same* `overflow_handler`/`divzero_handler`/`bounds_handler`
blocks that a local `FaultableBinOp` routes to. The scope-gating at the call site is the
mirror of `fault_handler_for` (`operators.rs:332`): `fault_scope.is_some()` AND the
callee is a participating-faulting fn → emit `FaultableCall`; else plain `Call`
(panic-by-default deep, unchanged). A local `a*b` and a deep `faulty(a,b)` in the same
`catch` converge on one handler block and one merge — the user writes one `catch`.

### 1.2 Drop interaction (the "fault past a frame still runs drops, without unwind")

This is the part the by-value design makes *free*, and the §5 mandate must not break:
- The faulting **callee** runs its own `emit_early_exit_drops` at the slot-set return
  — every droppable local in the callee's scope is cleaned up by **ordinary CFG drop
  insertion** at a statement boundary (the `lower_throw` shape, verified
  `stmts/mod.rs:2437`). No unwind, no cleanup-stack.
- The **caller's** handler-entry block lives in the GIR/LIR CFG (the
  `lower_fault_catch_expr` template), so drop-insertion/elaboration run over it and any
  live owned temporary on the caller side (e.g. the call's own argument temporaries) is
  dropped exactly once on the fault path — identical to how `fault_catch_drop.gg`
  already proves it for the local case.
- **Branch-before-commit holds across the call:** the callee's faulting op branched
  BEFORE its store (Phase-1 §11.2), and the caller checks the slot BEFORE reading the
  call's (sentinel) result — so no corrupted value materializes and no user `drop()`
  reads partial state (§3.1 is MOOT, same as Phase 1).
- **Re-entrancy (fault inside a `drop`) — DECIDED abort** (`phase2-design.md` §Q9):
  not a 2.1 blocker (2.1's drop-gate fixture uses a non-resource callee frame; add the
  resource-frame + fault-in-drop fixtures when those land in 2.2).

### 1.3 Minimal end-to-end slice + backend sequencing (§11.7)

Per the §11.7 "shared LIR shape FIRST → ONE backend → typecheck → second backend →
fixtures" rule:
1. **Shared LIR + GIR**: add `FaultableCall` (GIR) + its `FaultCheck`-on-slot GIR→LIR
   split; add the hidden out-param/tagged-return ABI on participating callees; thread
   `fault_handler` through all CFG passes (`successors`/`thread_jumps`/
   `eliminate_dead_blocks`/`tag_ownership`/`liveness`/`validate`/`sim` — the same
   sites the Inc1/Inc2 arms touched, see `tag_ownership.rs:245`, `liveness.rs:299`).
2. **ONE backend first (C)** — emit the slot write (callee) + the slot test + branch
   (caller). C is the flat-emit backend; get it green before LLVM.
3. **Front-end**: extend `lower_fault_catch_expr` to emit `FaultableCall` for a
   participating-callee call in scope (the call-site gate); mark a function
   "participating" when its body contains an uncaught faultable op reachable from a
   fault-scope call site (2.1: conservative — any fn with a faultable op is a
   candidate; 2.2/3b refine reach via 3b reachability-scoping).
4. **Second backend (LLVM)** at parity (the LLVM emitter is structurally closer —
   already branch-shaped).
5. **Fixtures** (below).

**The minimal demonstrator fixture** (from `phase2-design.md` §4, this is the slice):
```gorget
int faulty(int a, int b): return a * b          # may overflow; NO local catch
void main():
    int r = faulty(BIG, BIG) catch Fault.Overflow: -1
    print(f"{r}")                                # expects -1, NOT a panic
```
Plus the Q9 drop-gate: `faulty`'s frame holds a live `Drop`-typed local, caught one
frame up, **ASan/UBSan clean**. Plus a `fault_panic_default`-style negative:
uncaught-deep still panics `exit(1)`. Plus both-backend + `runtime_snapshots/` lock-in.

### 1.4 What Inc-2.1 does NOT touch (sequenced after)
2.2 N-frames-deep (transitive threading) → 2.3 generics/trait reach (Q13 Seam B,
conservative "may-fault") → **2.3b indirect calls** (`CallIndirect`/closure/`Callable[T]`
— the slot enters the callable TYPE; the weakest point of "hidden ≠ signature change",
named in §5) → 2.4 Task/TaskGroup boundary (the literal "server keeps serving"; the
ONLY runtime-touching sub-item — a fault field on the task struct + surfaced in `join`;
`task_group_runtime.c` has NO such field today, verified) → 2.5 `meta`/const-eval.

---

## 2. OPEN OWNER-DECISIONS (recommendations + blocking-vs-deferrable)

### BLOCKING for Inc-2.1

**D1 — `Fault equip Error` is NOT YET implemented; is it in scope for 2.1?** (BLOCKING
if the hybrid's unified `catch (e): match e` boundary is wanted at 2.1; DEFERRABLE if
2.1 ships only the concrete-`Fault` catch first.)
- *Finding:* Phase-1 §11.1 item 2 said `Fault equip Error`; the scout found **no
  `Error`/`Displayable`/`Debuggable` impl on `Fault`** in current source. The hybrid
  (the one ergonomic win the owner accepted from Option B — a unified `dyn Error`
  boundary handler) is GREENFIELD. It needs the 3 synthesized methods (`display`,
  `debug`, `source`) + built-in-`equip` injection (TODO line 58 flags it "blocked on
  built-in-`equip` injection").
- *Recommendation:* **DEFER the unified `dyn Error` surface past 2.1.** 2.1's catch
  binds a concrete `Fault` (exactly like Phase-1's binding form). Ship `Fault equip
  Error` as its own increment (2.1b) once 2.1's propagation works — the propagation
  mechanism is independent of the typing surface. Make the call: is the deep-catch
  demonstrator allowed to use `catch Fault.Overflow:` / `catch f: match f` (concrete)
  at 2.1, deferring `catch (e): match e` (dyn Error)?

**D2 — `Fault` membership for the deep case.** (BLOCKING — defines the slot's tag
space.)
- *Finding:* `Fault` today = `{Overflow, DivByZero, Bounds}` (no OutOfMemory). Deep
  propagation must encode WHICH fault crossed the slot.
- *Recommendation:* 2.1 deep-propagates the existing 3 (Overflow/DivByZero are the
  demonstrator; Bounds is already a `FaultableIndexLoad` so it threads identically).
  **OutOfMemory stays out** (still scattered `exit(1)` in allocators, no slot path) —
  keep it Phase-2-later as §11.1 says. Confirm the 3-variant slot tag.

**D3 — "participating callee" determination + the hidden-slot ABI choice.** (BLOCKING
— it is the §5 binding instruction made concrete.)
- *Finding:* the cost gate mandates a lean out-param/tagged-return, NOT a `Result`
  struct. The codebase has BOTH idioms: `&`-param → `MutPtr` out-param
  (`context.rs:1679`) and sret/register-return ABI (`lir/lower/types.rs`).
- *Recommendation:* a **hidden trailing `MutPtr<i32-tag>` out-param** (simplest,
  reuses the `&`-out-param lowering wholesale; the slot is "0 = no fault, else the
  `Fault` discriminant"). Tagged-return-register is the alternative if the owner wants
  zero extra param. Owner picks the ABI form; everything downstream is mechanical.

**D4 — `catch`-by-`Fault` on a deep CALL: grammar/typecheck.** (BLOCKING — but likely
already covered.)
- *Finding:* Phase-1 already added `Expr::FaultCatch` AST/grammar/typecheck DISTINCT
  from contract `catch` (DONE `8ab75635`). A deep call `faulty(…) catch Fault.X` is the
  SAME surface syntax — the only change is the call-site LOWERING gate, not new
  grammar.
- *Recommendation:* confirm the existing `FaultCatch` AST accepts a `Call` inner (it
  should — the inner is an arbitrary expr); no new front-end. DEFERRABLE-leaning-not.

### DEFERRABLE past Inc-2.1 (gate later increments, not 2.1)

**D5 — the fast knob (debug-checked / release-wrap).** §6/§9 Q2. **RESOLVED already**
(DONE `fb2e5037`: the global wrap mode was retired; plain `+`/`-`/`*` ALWAYS check,
`+%`/`-%`/`*%` wrap and never fault). *No open decision* — the type never lies, deep
faults are always deliverable. Note it as closed.

**D6 — `meta`/const-eval overflow three-way split.** §9 Q11 (`meta.rs:1278`). *Finding
(re-verified):* `eval_binary_op` still `wrapping_add/sub/mul`s silently
(`src/semantic/meta.rs:1278-1280`); Div/Rem-by-zero already error. So today: compile-time
WRAPS, runtime FAULTS. A fault can't "recover at a boundary" at compile time (no
boundary). *Recommendation:* cleanest answer = `meta` arithmetic overflow becomes a
**compile error** (not wrap, not catch) — const-eval has determinate inputs, a wrap is a
silent miscompile of a constant. A `catch` in `meta` context = compile error. **DEFER
to 2.5** (Q11 is the §4 2.5 sub-item); 2.1 doesn't touch `meta`. Owner decides the
three-way; flag it's a real divergence that needs a decision before 2.5.

**D7 — `Never` spelling.** §9 Q4. **RESOLVED** (owner 2026-06-21): `Never` is the
bottom type (`types.rs:68`), NOT renamed to `Fault`; they're opposite kinds. *No open
decision* for the fault model. Note closed.

**D8 — Result reconciliation.** §9 Q6 / A-vs-B §6. *Finding:* under A+hybrid the
contract `Result[T,E]` and the out-of-band `Fault` stay cleanly separate; the boundary
`catch (e)` over `dyn Error` (the hybrid) is the only place they meet, via `Fault equip
Error` — NOT a `Fault | UserError` union. *Recommendation:* keep separate; the only
reconciliation work is D1 (`Fault equip Error`). DEFERRABLE (rides D1).

**D9 — full fault set / OOM.** §9 Q7. Tied to D2; OOM deferred (allocator rework).
DEFERRABLE.

**D10 — `on error` on a fault propagation.** §9 Q12 / phase2-design §5. *Recommendation
(design's):* `on error` (which already runs `emit_on_error_cleanups` on the by-value
error path, `stmts/mod.rs:2436`) ALSO runs on a fault propagation — free on by-value,
consistent. DEFERRABLE to when `on error` + deep-fault interact (not 2.1's
non-`on-error` demonstrator); state it in the spec.

**D11 — `main`/top-level boundary.** §9 Q16. *RESOLVED by by-value* (phase2-design
§Q16): `main` is just the outermost frame; a by-value fault reaching it is caught like
any boundary, so "recoverable overflow in a plain CLI" is reachable (default uncaught =
still abort). DEFERRABLE — confirmed unblocked, no decision needed for 2.1's `main`-level
catch.

---

## 3. Self-host parity story

**Rust-first regresses NO self-host gate** (§11.8, re-confirmed):
- The self-host's OWN source keeps panic-on-overflow (it won't use deep fault-catch),
  so `bootstrap_fixed_point` and the frozen `runtime_snapshots` are untouched. New
  deep-fault fixtures the self-host can't compile yet register as not-yet-at-parity in
  the diagnostic `self_host_runtime_diff` (env-gated `GG_RUNTIME_DIFF=1`,
  `tests/integration.rs:17401`) — honest, not a regression.
- **The Phase-1 substrate the self-host fast-follow needs is ALREADY present** in
  `tests/fixtures/self_host_lowerer/` (the `EFaultCatch` AST + `FaultScope` +
  `GIFaultCheck`/`IFaultCheck` + `lower_fault_catch_expr`, DONE `2a36bc2f`/`bf0dd44b`/
  `f5287ff5`). The self-host already has the by-value `throws`/`Result` early-exit
  machinery (it self-compiles the Rust-mirrored auto-prop).

**Self-host fast-follow for Inc-2.1** (its own scout→brief→reviews, AFTER Rust 2.1):
mirror the SAME `FaultableCall` GIR/LIR shape + the hidden-slot ABI into the self-host
lowerer; clone its `lower_fault_catch_expr` call-site gate. The self-host's `FaultScope`
is per-function-fresh (DONE `2a36bc2f`) — the same seam Inc-2.1 crosses, so the change
is structurally identical. Guard the `map_binop` unknown-op→`OP_ADD` footgun
(`lower_types.gg:2434`) if any new op token is introduced (it isn't for 2.1 — the call
gate reuses the existing call lowering).

**Pre-impl measurement on the self-host self-compile is SATISFIED** (DONE `b7ef79d5`,
~0.016 ns/frame). The `--clones=stats` + `scripts/self_host_mem_baseline.sh` harness is
the re-measure path if the slot threading is ever suspected of bloat.

---

## 4. Effort estimate + Phase-2 increment count

**Phase 2 splits into ~5–6 increments** (phase2-design §4 sequencing):
- **2.1** single-call-deep (this scout) — **the bulk of the new machinery** (the
  `FaultableCall` GIR/LIR shape + hidden-slot ABI in BOTH backends + the call-site
  gate). Comparable in size to Phase-1 Inc1 (a new faultable LIR shape in both
  backends), MINUS the front-end (grammar/typecheck reused) PLUS the ABI extension.
  **Medium-large, single funded chain** (scout→brief→≥3 reviews→executor→output-review,
  one backend then the other).
- **2.1b** `Fault equip Error` + unified `dyn Error` boundary (the hybrid; blocked on
  built-in-`equip` injection) — **small-medium**, independent of propagation.
- **2.2** N-frames transitive threading — **small** (the slot threads recursively once
  2.1's single-hop works; mostly fixtures + the conservative reach).
- **2.3 / 2.3b** generics (Q13 Seam B) + indirect-calls (slot in callable type) —
  **medium each**, the genuinely harder seams (signature-visible for indirect).
- **2.4** Task/TaskGroup boundary — **medium**, the only runtime-touching one (fault
  field on the task struct + `join`).
- **2.5** `meta`/const-eval — **small**, mostly a decision (D6) + a negative fixture.

Inc-2.1 alone is a multi-pass funded round; the whole of Phase 2 is several rounds. The
**unwind substrate is NOT built** (the kill-risk avoided) — that's what keeps each
increment bounded.

---

## 5. Cite map (doc sections + code the design rests on)

**Docs:** `error-model.md` §9.1 PHASE 2 (owner A+hybrid), §6 (by-value contract leg),
§3/§3.1 (ubiquity + partial-state), Q14 (out-of-band structured enum + dynamic match),
§11.2 (the Phase-1 `FaultScope`/CFG template 2.1 extends), §11.7 (sequencing);
`error-model-phase2-design.md` §1 (linchpin), §3a (hidden slot), §4 (Inc-2.1 + the
demonstrator), §5 (the lean-slot BINDING instruction + the indirect-call seam);
`error-model-phase2-A-vs-B.md` (verdict + the hybrid); `cast-via-construction.md` §7.4
(Seam B, the generics seam inherited at 2.3).

**Code (re-verified this session — line numbers DRIFTED from the docs; regenerate for
any brief):**

| Premise | Doc-cited | ACTUAL (this session) | Status |
|---|---|---|---|
| `current_throws_result_type` set | `functions.rs:715` | `:762` | semantics OK, line drifted |
| throws → Result return synth | `mod.rs:626-645` | `:629-643` | VERIFIED |
| `throw` early-exit (cleanups+drops+ret) | `stmts/mod.rs:2373-2398` | `:2413-2438` (fn `lower_throw` @2407) | semantics OK, drifted |
| setjmp fallback (non-throws arm) | `stmts/mod.rs:2400-2402` | `:2439-2442` | VERIFIED |
| auto-prop hook fire | `exprs/mod.rs:87` | hook @ `:78-88` | VERIFIED |
| `emit_result_auto_propagate` | `exprs/mod.rs:2922` | `:2922` | VERIFIED (exact) |
| Error-path by-value ret + drops | `exprs/mod.rs:3055-3057` | `:3080-3082` | semantics OK, drifted |
| `lower_catch_expr` | `exprs/mod.rs:3341` | `:3385` | semantics OK, drifted |
| `suppress_auto_prop` | `exprs/mod.rs:3375` | `:3422` | VERIFIED |
| `FaultScope` decl | `context.rs:286-315` | `:286-316` | VERIFIED |
| FaultScope clear-at-call comment | `context.rs:281` | `:281` ("CLEARED at Call/CallExtern") | VERIFIED (mechanism = per-fn fresh `func_state`, `operators.rs:304-309`) |

**Phase-1 fault machinery (new this scout):** `FaultableBinOp`
`src/ir/instructions.rs:222`; `FaultableIndexLoad` `:250`; `Call`/`CallIndirect`
`:332`/`:339`; `Inst::FaultCheck` `src/lir/mod.rs:857`; `FaultOp` `:308`; GIR→LIR split
`src/lir/lower/insts.rs:125-202`; C emit `src/backend/c_lir/mod.rs:2570-2597`; LLVM emit
`src/backend/llvm/mod.rs:3627-3678`; `lower_fault_catch_expr`
`src/ir/lowering/exprs/mod.rs:3543-3713`; `fault_handler_for` gate
`src/ir/lowering/exprs/operators.rs:332`; `Fault` enum `builtin_fault_enum`
`src/ir/lowering/generics/substitute.rs:323`; semantic reg `src/semantic/resolve.rs:175`;
lint `tests/lints.rs:2462`; `&`-out-param ABI `context.rs:1679`; sret/out-param ABI
`src/lir/lower/types.rs:392-447`; `meta` wrap `src/semantic/meta.rs:1278-1280`;
`gorget_panic`=exit(1) `panic_normal.c`; task struct (no fault field)
`task_group_runtime.c:6`; runtime-diff test `tests/integration.rs:17401`.

**Two findings that need a brief/owner note (not in the docs):**
1. **`Fault equip Error` is NOT implemented** in current Rust gg (the hybrid's
   unified-`dyn Error` surface is greenfield, blocked on built-in-`equip` injection) —
   D1.
2. The design docs' `file:line` numbers have all DRIFTED ~40-60 lines; a brief built on
   the doc citations verbatim would mis-point. Regenerate from the table above.
