# Error-model Phase 2 (deep / boundary fault catch) — design

**Status:** DESIGN, scout-produced (`afa58d78`, 2026-06-22). **Design review COMPLETE — 2 clean
sequential fresh passes:** pass 1 (`a8bbd2b9`) SIGN OFF (linchpin cites VERIFIED against source) + 4
minors folded; pass 2 (`a990fe31`) SIGN OFF (fold clean, additions sound, cites re-confirmed, no
contradictions). TL;DR hedge applied.
**OWNER DECISION 2026-06-22: reframe NOTED, implementation HELD for a later round.** The owner wants a
fresh A-vs-B comparison before adopting, and leans toward a *simpler* model than this doc's hidden-slot:
**Option B = use `throws` for Faults too, uniformly** (faults ride the visible `throws`/`Result` channel
like contract errors; ~zero new machinery; cost = `throws` ubiquity + the §3 tension + merging the
fault/contract distinction). This doc is **Option A** (hidden out-of-band by-value slot; faults off
signatures). **VERDICT (comparison `a0ea33be` → `error-model-phase2-A-vs-B.md`): Option A, with a hybrid
that delivers B's one ergonomic win (a unified `Error` boundary handler via `Fault equip Error`).**
⚠ Prior-art CORRECTION: Swift TRAPS on overflow (uncatchable) — a precedent FOR A, NOT B; the real
B-camp is Java/C#/Python/Ruby (unchecked exceptions). **NOTHING is adopted; the linchpin (deep by-value
is possible + drop-correct) holds for BOTH options.** **✅ OWNER-ACCEPTED 2026-06-22: Option A + the
hybrid** (folded into `error-model.md` §9.1 PHASE 2). Increment 2.1 (single-call-deep by-value catch) is
now briefable; **pre-impl gate ✅ SATISFIED 2026-06-22** (measured ~0.016 ns/frame happy-path, scout
`ace83307`; unwinding's edge marginal, A confirmed) — with the BINDING impl instruction: lower the slot
as a hidden **out-param / tagged-return**, NOT a `Result`-union struct-return (§5).
**Owner picked A+hybrid (2026-06-22)** — Increment 2.1 (§4) is now scoutable/briefable; the §9.1
"deep ⇒ unwind" framing is superseded (folded into `error-model.md`).

> ⚠ This design **reframes a load-bearing assumption** in `error-model.md` §9.1 — that deep/boundary
> fault catch requires a greenfield unwind substrate. The reframe rests on the claim that Gorget's
> *contract*-error path is ALREADY a deep, drop-correct, by-value channel. **Every `file:line` below
> is a load-bearing premise that a fresh reviewer MUST verify against current source before this is
> acted on.** If the linchpin claim (§1) is wrong, the recommendation collapses.

## TL;DR (the de-risking find)

The load-bearing fork — **true stack unwinding vs by-value typed-error return** — resolves decisively
for **by-value**, and the substrate for it **already exists and ships**. Gorget's `throws`/`Result`
path is already a Swift/Zig-style deep, cross-frame, drop-correct by-value error channel. True
unwinding (setjmp/longjmp or LLVM landing pads) is **greenfield, fragmented, and unnecessary** for the
owner's actual goal ("server keeps serving"). Phase-2 fault catch = the existing by-value propagation
machinery, gated so faults stay out of public signatures (fully for direct/monomorphized calls;
indirect/generic call sites reflect the slot in the callable/bound type — §5) via a hidden out-of-band
return slot — which sidesteps the multi-month unwind substrate entirely (and with it B2/Q9/Q15/Q16/§3.1).

## 1. The load-bearing fork, and the premises to verify

**Claim (LINCHPIN — all cites VERIFIED against current source, review pass 1 `a8bbd2b9`):** Gorget's
contract-error model is already by-value, deep, and drop-correct:
- `throws E` lowers to a `Result[T,E]` **value-union return** — `src/ir/lowering/functions.rs:715`
  (`current_throws_result_type` + the equip-method path `:911-934`) AND the plain-function twin
  `src/ir/lowering/mod.rs:626-645` (where most `throws` functions get their `Result` return type).
- A `throw` in a `throws` fn builds `Error(val)` and **returns it by value**, running
  `emit_on_error_cleanups` then `emit_early_exit_drops` first — `src/ir/lowering/stmts/mod.rs:2373-2398`
  (the `throws` conditional opens at `:2373`; cleanups `:2396`, drops `:2397`, `builder.ret` `:2398`)
  — so every droppable local in scope is cleaned up at the early-exit point by **ordinary CFG drop
  insertion**, no unwind. **CRUX, confirmed exactly as claimed.**
- Each call site receiving a `Result` from a callee auto-propagates via the centralized producer-side
  hook `maybe_auto_propagate` (`src/ir/lowering/exprs/mod.rs:87`) → `emit_result_auto_propagate`
  (`:2922`), which on the Error path re-wraps in the *current* frame's `Result` and **returns by value**
  (`:3057`) after running `emit_on_error_cleanups` (`:3055`) + `emit_early_exit_drops` (`:3056`). So an
  error **threads up N frames by value**, each frame running its own drops correctly.
- `catch (e):` (`lower_catch_expr`, `src/ir/lowering/exprs/mod.rs:3341`) catches that by-value error at
  any boundary up the chain.

**Claim (the setjmp substrate is vestigial, not production) — VERIFIED:** `gorget_throw`/`GORGET_TRY`
(`src/backend/c/.../runtime_error.c:3-28`) is emitted ONLY as the fallback arm for a `throw` in a
*non*-`throws` (ill-typed) context — `stmts/mod.rs:2400-2402`. The real path is by-value. The test-mode
cleanup-stack + longjmp (`panic_test.c:2-41`) is driven by the **test harness**: `__gorget_cleanup_push`
is gated `if func.is_test_fn` (`src/backend/c_lir/mod.rs:1853` calling `helpers.rs:1938`), NOT general
drop insertion.

**Claim (no zero-cost EH anywhere):** no `landingpad`/`invoke`/`personality`/`resume` in
`src/backend/llvm/` (the single "invoke" hit is a comment, `llvm/mod.rs:4269`). Production panic is
`exit(1)` (`panic_normal.c:3-9`). Schedulers don't catch panic; `join` has no error/fault field
(`task_group_runtime.c`) — greenfield for task-boundary recovery.

**Claim (Phase-1 fault-catch is fully local):** `FaultScope` in `func_state` routes faultable ops to
handler blocks in the *same* function (`src/ir/lowering/context.rs:286-315`); a fault never crosses a
call today.

### Why this settles the fork (if the premises hold)

| Axis | True unwind | **By-value typed return (RECOMMENDED)** |
|---|---|---|
| Drop correctness across N frames | build cleanup-on-unwind from scratch; collides with ownership invariants (Q9) | **already solved** — `emit_early_exit_drops` at each by-value early return |
| LLVM backend | greenfield landing pads / `invoke` / personality; done twice (backends-at-parity) | **zero new EH** — a `Result`-shaped return + branch both backends already emit |
| FFI (Q15) | longjmp over a C frame skips C cleanup → UB | **non-issue** — a by-value return never jumps over a foreign frame |
| `main`/boundary (Q16) | needs a top-level setjmp install | `main` is just the last frame; the value reaches it like any other |
| partial-state (§3.1) | Drop runs *during* unwind seeing partial state (the Rust `panic=unwind` hazard) | faulting op **branches before the store** (Phase-1 §11.2); drops run at a clean stmt boundary |
| hot-path cost | cheap (no threading) — unwind's only real advantage | a fault-carrying slot threaded through opted-in callers — **the one genuine cost (§3)** |

The fork is not "which is feasible" — both are. It is "which matches Gorget's existing architecture,"
and by-value wins overwhelmingly. True unwinding builds a *parallel* error-propagation mechanism next
to the one Gorget already has, solely to keep faults off the hot-path signature — obtainable more
cheaply (§3).

**Prior art:** Swift (typed `throws`, no unwinding — the decisive precedent, scales to servers); Zig
`!T` error unions (by-value, `try` auto-prop; overflow/bounds are panics, not union members);
Rust `panic=unwind` (the cautionary Drop-during-unwind hazard the by-value path avoids; `panic=abort`
exists because shops don't want it); Midori/Erlang (support the fault≠contract split but recover by
*abandoning the unit of work* — modeled by a by-value "fail this request, discard outputs" handler).

## 2. Open questions — decisions

- **B2 (unwind mechanism) — DISSOLVED.** By §1 there is no unwind mechanism to choose. Does not block
  the first increment (removed from scope). If the owner mandates true unwind for hot-path reasons, B2
  becomes the multi-month kill-risk (§5).
- **Q9 (drop across propagation) — already solved; blocks nothing.** Each frame early-returning a fault
  value runs its in-scope drops via existing CFG insertion. The widened §3.1 concern (a user
  `drop(!self)` reading partial state) does not arise: the fault return is at a statement boundary after
  the faulting op branched *before* its store. **Gate fixture:** a fault propagating up 3 frames, each
  with a live `Drop`-typed local, ASan/UBSan clean.
  - **Re-entrancy policy (fault-inside-a-destructor) — DECIDED: abort.** A faultable op (e.g. `a*b`
    overflow) inside a user `drop(!self)` body that itself runs *during* `emit_early_exit_drops` of an
    in-flight fault propagation, when that drop is lexically within a deep-catch scope, is a
    fault-during-fault-cleanup re-entrancy. Policy: **a fault raised inside a `drop` aborts** (matches
    Rust's abort-on-double-panic). Not a 2.1 blocker (2.1's fixtures use non-resource frames); state it
    in the spec + add a fixture when resource-bearing deep frames land.
- **Q15 (FFI) — non-issue; blocks nothing.** A by-value return never unwinds over a foreign frame. A
  fault *inside* an extern still aborts (unchanged, correct). Document: faults do not cross `extern` by
  propagation.
- **Q16 (`main`/top-level) — resolved; ships in the first increment.** `main` is the outermost frame;
  a by-value fault reaching it is caught like any boundary. Default: uncaught fault still aborts
  (panic-by-default preserved); explicitly caught at any function-level boundary recovers. This makes
  "recoverable overflow" reachable in a plain CLI (Q16 had flagged it otherwise unreachable).
- **§3.1 (partial-state) — covered by Q9; blocks nothing.** The boundary discards the unit; no code
  past the catch reads the faulted unit's outputs.

## 3. The real cost — §3 ubiquity, and how by-value handles it

The only genuine tension: if a fault rides the value channel, does `Overflow` end up in ~every
signature? Three sub-options, increasing cost:

- **(3a) Hidden out-of-band return slot (RECOMMENDED first).** Thread the fault through a
  compiler-synthesized side-channel return (hidden out-param / tagged-return ABI extension), NOT part
  of the user-visible signature and NOT in the `Result` type. A function that might fault *and is within
  a deep-catch scope* gets the slot; callers on the path to a catch check-and-propagate; a plain
  `int sum(...)` stays `int` at the source level. This realizes Q14's decision (B) "out-of-signature"
  **by-value instead of by-unwind**; the fault value is the closed `Fault` enum (Q14 A); the boundary
  does a dynamic match (Q14 C).
- **(3b) Monomorphization-scoped threading.** Only thread the slot through functions statically
  reachable between a faultable op and an active deep-catch boundary. Bounds ubiquity to participating
  code. Defer — (3a) can thread unconditionally for correctness first, optimize reach later.
- **(3c) True out-of-band unwind.** The doc's original assumption. Revisit only if (3a)/(3b)'s measured
  hot-path cost is unacceptable. Do not start here.

The §3 impossibility argument ("recoverable-default + informative-row + universal: pick two") is about
the typed **contract** row and remains intact — faults stay out of the contract row / public signature.
(3a) puts them in a *hidden* channel, so the contract row stays sparse. (The doc's Q14 already decided
"out-of-band, structured enum, dynamic match"; this design's contribution: out-of-band can be by-value.)

## 4. Recommended FIRST Phase-2 increment

**Increment 2.1: single-call-deep fault catch via by-value hidden-slot threading, additive over
Phase-1 local catch.**

1. **Extend `FaultScope` reach across ONE call boundary.** Today a faultable op consults
   `func_state.fault_scope` (`context.rs:286-315`) for a handler in the *same* function. The increment:
   a function containing faultable ops emits a **hidden fault-return slot**; a *call* to such a function
   lexically inside a fault-catch checks the slot and branches to the existing handler-entry blocks
   instead of panicking.
2. **Reuse the by-value return + drop machinery.** The callee, on a locally-uncaught fault, sets the
   hidden slot and does an `emit_early_exit_drops` return (clone the `throw` path
   `stmts/mod.rs:2380-2398`, with the `Fault` enum + hidden slot instead of `Result`/`LocalId(0)`).
3. **Handler-bb materializes the `Fault`** exactly as Phase-1 does.

**Fixture** (+ `runtime_snapshots/`):
```gorget
int faulty(int a, int b): return a * b        # may overflow, no local catch
int main():
    int r = faulty(BIG, BIG) catch Fault.Overflow: -1
    print(r)                                   # expects -1, NOT a panic
```
Plus the Q9 drop-correctness gate: `faulty`'s frame holds a live `Drop`-typed local, caught one frame
up, ASan/UBSan clean.

**Sequencing:** 2.1 single-call-deep → 2.2 N-frames-deep (transitive threading) → 2.3 generics/trait
reach (Q13 Seam B; conservative "may-fault") → **2.3b indirect calls** (fn-pointer/closure/`Callable[T]`
— the slot reflected in the callable type; see §5) → 2.4 Task/TaskGroup boundary (the literal
"server keeps serving"; **the only sub-item touching the runtime** — add a fault field to the task
struct + surface it in `join`; still by-value, no scheduler setjmp) → 2.5 `meta`/const-eval (Q11; keep
Phase-1 answer). **Self-host fast-follows each** (Phase-1 substrate already present in
`tests/fixtures/self_host_lowerer/`).

## 5. Risks & kill-conditions

- **Kill-risk (avoided): the unwind substrate.** If true stack-unwinding is mandated (zero-hot-path-cost
  guarantee, or catching faults raised *inside* externs), Phase 2 becomes multi-month: setjmp/longjmp +
  cleanup-on-unwind drop elaboration (C backend), landing pads / `invoke` / personality from scratch
  (LLVM — zero EH today), per-task setjmp in all four schedulers, FFI catch frames (Q15 UB).
  **Strong recommendation: do not build it unless a measured hot-path regression forces it.**
- **Real cost on the recommended path: hot-path threading (§3) — ✅ MEASURED 2026-06-22 (scout
  `ace83307`), gate SATISFIED.** A's well-lowered propagation cost is **~zero on the happy path**:
  ~**0.016 ns/frame** (~3% on a 600M-call call-SATURATED microbenchmark, unmeasurable on real
  workloads) — the ideal hidden-slot lowers to a **branchless `csinc`** the CPU absorbs. Unwinding's
  happy-path edge over A is therefore **marginal-to-unmeasurable**, while its price is enormous
  (greenfield landing pads in BOTH backends + drop-during-unwind hazard + FFI UB) → **A confirmed; do
  NOT build unwinding.** (3b) reachability-scoping makes A's cost **zero** on non-participating frames
  (self-host: ~916 fns, only ~10 `throws`; deep-catch is a boundary feature → tiny spine).
  **⚠ ONE BINDING IMPL INSTRUCTION the measurement adds:** lower the hidden slot as a **hidden out-param
  / tagged-return register** — **NOT** the fat by-value `Result`-union struct-return that `throws` uses
  today. The current `Result` lowering is ~9× the floor (63ms ideal → 86ms clean 32-byte struct-return
  → 765ms actual; the 8× gap above clean struct-return = **redundant memcpys** in the current lowering,
  visible in the emitted C). Q9 drop-correctness across propagation is PROVEN today on the existing
  by-value path (scout built the fixture, ASan/UBSan clean) — inherited, not new work.
- **Seam B (Q13) generics** — a type-param method's fault-channel is unknown pre-mono; conservative
  "may-fault" threading is correct-but-overbroad. Defer bound-spelling to 2.3; don't gate 2.1.
- **Indirect-call seam (fn-pointer / closure / `Callable[T]`) — the weakest point of the
  "hidden slot ≠ signature change" claim; NAME it, don't leave it silent.** For a DIRECT call the
  hidden fault-return slot is invisible at the source signature (2.1 relies on this). For an INDIRECT
  call through a `Callable[int(int,int)]` holding a faulting function, the call site can't see the
  callee's fault-ness, so the slot must become part of the **function-pointer TYPE** — i.e. a de-facto
  signature change for indirect calls (analogous to Seam B). 2.1 (direct calls only) dodges it; it is a
  deferred seam, sequenced as **2.3b** alongside the generics seam. The honest framing: "hidden by-value
  slot" is fully transparent only for direct calls; indirect/generic call sites need the slot reflected
  in the callable/bound type. This does NOT sink the design (direct + monomorphized calls are the bulk
  and 2.1/2.2 cover them) but it bounds the "no signature change anywhere" claim.
- **`on error` (Q12)** already runs on the by-value error path (`emit_on_error_cleanups`,
  `stmts/mod.rs:2396`). Recommend it also runs on a fault propagation (consistency; free on by-value).

## 6. Doc grounding

Rests on `error-model.md` **§9.1** (phasing; refines its "out-of-band deep ⇒ unwinding" leg into
"out-of-band can be **by-value**"), **§6** (the by-value contract leg already present), **Q14**
(out-of-band structured `Fault` enum + dynamic match — adopted, realized by-value), **§3/§3.1**
(ubiquity + partial-state — satisfied by hidden-slot + branch-before-store), **§11.2** (the Phase-1
`FaultScope`/CFG template 2.1 extends); `cast-via-construction.md` **§7.4 Seam B** (generics seam,
inherited); `language-design.md` **§6.4** (panic-by-default preserved); `book/10-errors.md`.
