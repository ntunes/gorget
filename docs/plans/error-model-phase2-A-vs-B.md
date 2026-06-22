# Error-model Phase 2 — fault propagation: Option A vs Option B (comparison verdict)

**Status:** comparison produced by a fresh analyst (`a0ea33be`, 2026-06-22) at the owner's request
("review both options and come back with which one is better"). **VERDICT: Option A, with a hybrid that
delivers Option B's one real ergonomic win.** **✅ OWNER-ACCEPTED 2026-06-22: Option A + hybrid** —
folded into `error-model.md` §9.1 PHASE 2 (the "deep ⇒ unwind" framing is superseded). Increment 2.1 now
briefable; pre-impl gate = measure the hidden-slot hot-path cost on the self-host self-compile.

## The two options (both by-value — the unwind substrate was already killed)
- **Option A** — hidden out-of-band by-value slot (`error-model-phase2-design.md`): faults ride a
  compiler-synthesized slot, NOT in the user signature / `Result` type. Faults stay OFF signatures.
- **Option B** (the owner's lean) — uniform `throws` for faults: faults ride the visible `throws`/
  `Result` channel like contract errors. ~zero new machinery; cost = `throws` ubiquity + §3 tension +
  merging the fault/contract distinction.

Both reuse the SAME shipped drop-correct early-exit machinery (`emit_early_exit_drops`); the ONLY delta
is whether the fault is written into the visible `Result` (B) or a hidden slot (A). So this is not
"cheap B vs expensive A" — it's "two by-value designs differing in signature visibility."

## VERDICT: Option A. Reject B. (Offer B's ergonomics via the hybrid below.)

### 1. B does NOT dissolve §3 — it forfeits the no-annotation leg and pays via `throws`-spine annotation
§3: you can't have all of {recoverable-default overflow, informative contract row, universal-without-
annotation}. A keeps all three (faults off the contract row). B gives up "recoverable-default" for the
DEEP case (recovery requires annotating the chain) AND re-enters faults onto the API surface via
`throws Fault`. **Transitivity multiplier (verified):** auto-propagation fires only if the enclosing fn
can itself propagate (`src/ir/lowering/exprs/mod.rs:46`, `:3152-3170`), and the compiler FORCES handling
a `throws` call (`book/10-errors.md:99-100`). So deep recovery under B forces `throws Fault` onto EVERY
frame from the catch boundary down to the faulting op — the Java-checked-exceptions failure mode that
`error-model.md` §5 calls "the canonical failure of this design."

### 2. Panic-by-default: both safe; B has a "same op, two behaviors" footgun
Both preserve abort-on-uncaught (no silent wrong value). But under B, bare `a*b` early-returns in a
`throws` fn and panics in a non-`throws` fn — signature-dependent control flow with no syntactic marker
(action-at-a-distance). Under A the determinant is LEXICAL (in/out of a `catch` scope, as Phase-1
already is) — more local, more teachable.

### 3. Self-host ubiquity — MEASURED (the §3 fear meets reality)
- ~30% of `self_host_lowerer` functions contain integer arithmetic (318/1054).
- Only ~5% of all 1285 fixtures use any `throws` (122 signatures total); `self_host_lowerer` has ~3 real
  `throws` sigs after stripping the `throws_type` AST field.
- The "server keeps serving" path is non-`throws` today: `httpserver_basic.gg:7`
  `HttpServerResponse handle(HttpRequest req):`.
Panic-by-default does NOT reduce B's ubiquity to near-zero: handlers aren't leaves, so `throws Fault`
floods the protected call spine (handler→logic→helpers), transitively + forced. **A = zero signature
changes; the self-host's own source stays byte-identical (bootstrap untouched).**

### 4. Swift CORRECTION (load-bearing — overturns a prior-art claim)
The premise "Swift is the mainstream language that did B" is **WRONG**. Swift integer overflow **traps**
(uncatchable `EXC_BAD_INSTRUCTION`); recovery is `&+`/`&-`/`&*` wrapping operators +
`addingReportingOverflow` (by-value tuple), NOT `try/catch`. **Swift is a precedent FOR A** (faults
separate, uncatchable-by-default), with Zig/Rust/Midori. The actual B-camp is Java/C#/Python/Ruby
(unchecked exceptions) — the cautionary family §3/§8 warn against (a contract `catch` silently swallows
a genuine bug). No first-tier safe language puts faults on the recoverable channel.

### 5. What B loses
The bug-vs-contract distinction at the catch site (`catch Fault.X` vs `catch (e)` become one construct
over `dyn Error` → a contract catch can silently swallow a bug); faults off the API/compat surface (§4);
the Midori bug≠expected classification. All deliberately built by the doc + already taught in
`book/10-errors.md:615-629`.

### 6. Result reconciliation
B forces either a `Fault | UserError` union (the §3 closed-enum sprawl) or a `dyn Error` dynamic match
(which negates B's own exhaustiveness claim — and is the same dynamic match A's Q14(C) already uses). A
keeps them cleanly separate.

## Tradeoff table
| Axis | A (hidden slot) | B (uniform throws) |
|---|---|---|
| §3 compliance | ✅ all 3 legs | ❌ forfeits no-annotation; faults on API surface |
| Panic-default clarity | ✅ lexical determinant | ⚠️ signature-dependent control flow on `a*b` |
| Self-host ubiquity (measured) | ✅ zero sig changes | ❌ floods the protected call spine |
| Implementation cost | ⚠️ slot synth/threading + 2.3b seam (reuses B's drop machinery) | ✅ least compiler code |
| Uniformity / teachability | ✅ extends shipped fault/contract split | ⚠️ surface-uniform but reverses the book's split |
| What's lost | — | ❌ bug≠contract, off-API-surface, swallow-safety |
| Result reconciliation | ✅ separate | ❌ union sprawl OR exhaustiveness-negating dynamic match |
| Indirect-call seam | present (slot in callable type) | present (`throws Fault` in callable type) — **wash** |
| Prior art | Swift/Zig/Rust/Midori | Java/C#/Python/Ruby (the warned-against family) |

## The hybrid to offer the owner (honors the uniformity goal)
Keep **A's propagation mechanism** (faults off signatures) AND deliver B's one genuine win — a unified
boundary handler `catch (e): match e` catching BOTH a propagated contract error AND an out-of-band
fault — by leaning on the already-planned **`Fault equip Error`** (Phase-1 §11.1 item 2). The owner gets
"one `catch` at the boundary handles everything that went wrong in this request" WITHOUT `throws Fault`
on any signature and without re-muddying the book. = A-as-propagation + B's-surface-at-the-boundary-only.

## Open question genuinely for the owner / pre-2.1 gate
A's hidden-slot **hot-path threading cost is UNMEASURED** (`error-model-phase2-design.md` §5 flags it:
measure `--clones=stats` + RSS on the self-host self-compile). Plausibly tiny (3b reachability-scoped),
but it's the one place A's cost is real and unquantified — MEASURE before Increment 2.1 ships. B has no
equivalent (it reuses the existing `Result` return). A severe measured overhead is the only fact that
could reopen this.

**Files this rests on:** `error-model.md` §3/§4/§5/§8/§9.1/Q14/§11; `error-model-phase2-design.md`
§1/§3a/§4/§5; `cast-via-construction.md` §7.3/§7.4; `language-design.md:189-218`/`:1300-1341`;
`book/10-errors.md:615-629`/`:99-100`; `src/ir/lowering/exprs/mod.rs:46`/`:3152-3170`;
`httpserver_basic.gg:7`; `self_host_lowerer/*.gg` (ubiquity measurements — re-verifiable).
