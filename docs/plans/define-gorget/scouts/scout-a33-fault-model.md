# A33 + FAULT-MODEL DESIGN SCOUT — FULL REPORT (v2, Q4 extension folded in)
> Scout run 2026-07-11, read-only, per `docs/plans/define-gorget/a33-fault-model-scout-mandate.md`
> (v2, incl. the mid-scout Q4 sigil-economy extension, orchestrator message + `a5e1be2e`).
> Worktree: `/workspace/gorget/.claude/worktrees/agent-ac7a3eb8fcbe7808e` (gorget-1 branch).
> All citations verified against CURRENT source this session.

---

## 0. Docs absorbed (grounding)

- `docs/plans/define-gorget/decisions.md` — D11 (:337-347), D18 (:368-373), D23 (:285-298),
  A31 (:161-167), A32 (:168-173), A33 + rider + 2026-07-11 owner update (:174-204),
  2026-07-07 dogfood LOG entry (:307-318).
- `docs/language-reference.md` §7.1 precedence (:1489-1513), §7.5 wrapping ops (:1552),
  §10.5 catch (:2466-2515), §10.9 Faults (:2570-2589), §3.1 keywords (:153 `move`).
- `spec/prose/trap-codes.md` (whole; 8 classes, catchable subset = Overflow|DivByZero|Bounds).
- `docs/plans/error-model.md` §9 (:325-450), §9.1 (:452-536), §10 (:538-551).
- `docs/language-design.md` §2.2 Integer Overflow (:189-218), §4.2.1 Option sugar (:778-814), §6 (:1229-1393).
- `docs/plans/error-model-phase2-design.md` (224 lines) + `error-model-phase2-A-vs-B.md` (97).

Key doc-vs-implementation discovery (feeds Q2 and the docs write-through of any ruling):

**⚠ language-reference §10.5:2515 and language-design §6.4:1362-1366 state fault recovery is
"local and lexical … a fault raised inside a function the expression calls is not caught."
This is STALE. Phase-2 Increment 2.1 (single-call-DEEP catch) is fully implemented in BOTH
compilers and locked by 15 `fault_deep_*` fixtures** (e.g. `fault_deep_catch.gg` expects `-1`,
`tests/integration.rs:6721`): a participating callee gets a synthesized trailing `MutPtr<i32>`
fault-slot param, writes a category tag, the caller tag-dispatches to handlers, uncaught
categories RE-PANIC (`fault_deep_bounds_swallow_guard.gg`), fn-value/HOF indirection passes
NULL (panics; 2.3b deferred; `fault_deep_fnvalue_panic.gg`). Whatever D25 decides, the docs
and the implementation currently disagree — a ruling fixes one of them.

---

## 1. CENSUS (Q2.1 + Q2.2) — COMPLETE, not sampled

### 1.1 `catch Fault` / fault-catch live-use census

| Corpus | Files w/ fault-catch | Occurrences | Classification |
|---|---|---|---|
| `tests/fixtures/` | **31** (33 `fault_*` files total) | 49+ | **31/31 test-of-the-feature-itself** (0 organic) |
| self-host (all `self_host_*`, symlinks deduped) | 12 real files, 17 hits | 17 | **17/17 COMMENTS** — implementation docs of the feature inside the self-host compiler; **0 uses** |
| gorget-arena (`/workspace/gorget-1/target/gorget-arena/src`, 67 .gg files) | 0 | 0 | — |
| gorget-js (`/workspace/gorget/.worktrees/gorget-js/src`, 15 .gg files) | 0 | 0 | 59 `catch` uses, all parenthesized contract-error catches |
| `spectests/` | 0 | 0 | ggdef models no catch — definition already matches removal |

**Headline: ZERO organic uses of fault-catch exist anywhere in the language's entire corpus.**

Fixture breakdown (all under `tests/fixtures/`):
- **Lexical (Phase 1), 15+1 files**: fault_catch_{overflow,div0,binding,compound,
  contract_unchanged,drop,intmin_div,bounds,bounds_drop,bounds_negidx,bounds_resource_mut,
  bounds_struct}, fault_intmin_partial{,_divzero}, fault_catch_bad_qualifier (negative).
- **Deep (Phase 2.1), 15 files**: fault_deep_catch{,_bounds,_bounds_binding,_bounds_drop,
  _bounds_resource,_divzero,_divzero_binding,_divzero_drop,_drop}, fault_deep_bounds_swallow_guard,
  fault_deep_fnvalue_panic, fault_deep_mixed_{bounds,divzero}_only,
  fault_deep_uncaught_{bounds_,divzero_,}panic.
- **Panic-by-default, 2 files** (fault_panic_default, fault_bounds_panic_default) — no catch; survive removal.
- Plus `tests/integration.rs:6664-7000` (~340 lines of test fns).

The self-host's own comment confirms the emptiness at the source: `self_host_lowerer/gir.gg:932`
— "Empty for the entire existing suite + self-host source (no arith/bounds `catch Fault.X`
over a user CALL)".

### 1.2 gorget-js pre-check census (the finding-(b) sites)

Full sweep of `/workspace/gorget/.worktrees/gorget-js/src` (~13.6k lines):

| Site | Shape | Class |
|---|---|---|
| `lexer.gg:97,100,106,111,121` (5) | cursor EOF guards (`at_eof`, `peek_byte()→-1`, `peek_byte_at()→-1`, `advance()` no-op at end) | **(a)** |
| `parser.gg:51,1321,1325,1332` (4) | token-stream cursor guards | **(a)** |
| `main.gg:46` (1) | argv bounds guard | **(a)** |
| scanning loops (`while i < len`) in parser/env/abstract_ops/main | bounded iteration, not dodges | n/a |
| `env.gg:665-727` | `objects.get(id).unwrap()` — safe API + assert-on-invariant | model-consistent |
| `parser.gg:133-141` | float literal overflow → +Infinity (IEEE data handling) | not integer-fault |

**Totals: (a) = 10, (b) = 0, (c) = 0.** No integer-fault dodges at all (gorget-js interprets
JS float semantics — no trapping integer arithmetic on user data). **Zero (c)-class evidence.**

Honest re-read of finding (b): on CURRENT source, the "pre-check" sites are EOF-cursor guards
a lexer/parser would write in any language, naturally expressible with `.get()` → Option.
The conditions guarded are expected DATA (EOF) — exactly the channel/Option's job, not bug
containment. Reported loudly per mandate: this weakens the pro-catch evidence rather than
strengthening it.

---

## 2. MACHINERY INVENTORY (Q2.3) — what removal retires

Measured by reading; "mention-lines" = lines matching `[Ff]ault` excluding `default` (a floor);
physical scope estimated from blocks actually read.

### 2.1 Rust gg (~645 mention-lines across 43 files; est. **~1,100–1,400 physical lines**)

| Component | Where | Size |
|---|---|---|
| Participation pre-pass ((a) uncaught-faultable-op ∩ (b) called-inside-catching-scope, single-hop, free-fns only) | `src/ir/lowering/fault_participation.rs` | **237 lines, whole file** |
| Fault-return scope + fill + trailing `__fault: MutPtr<i32>` slot-param synthesis + adapter flag | `src/ir/lowering/functions.rs:53-124, :140-196, :872-885, :943-958, :989-999, :1083-1085, :1178-1180, :1222-1224` | ~205 |
| Lexical catch lowering (`lower_fault_catch_expr`: handler blocks, per-category re-panic blocks, FaultScope push/pop, owned-at-boundary materialize) | `src/ir/lowering/exprs/mod.rs:3739-3921` + dispatch `:1578-1580` | ~198 |
| `FaultScope` + `fault_scope`/`fault_slot_param`/`participating_fault_fns` + accessor | `src/ir/lowering/context.rs:293-345, :518-525, :633-634` | ~60 |
| 3 dedicated GIR variants: `FaultableBinOp`, `FaultableIndexLoad`, `FaultableCall` | `src/ir/instructions.rs:212-258, :345-400` | ~105 |
| Call-site gate (slot alloc, trailing arg, tag dispatch) | `src/ir/lowering/exprs/calls.rs` | ~60-80 |
| GIR passes: optimizer `successors()` arms (silent-DCE linchpin `optimize.rs:2062`), validate, liveness, tag_ownership, printer, substitute, shared_async | 7 files | ~110 |
| LIR: `Inst::FaultCheck` + `FaultOp` + block-splitting faultable lowering | `src/lir/mod.rs`, `src/lir/lower/insts.rs:111-250`, ssa/split_edges/optimize/display/validate | ~180 |
| Backends: `FaultCheck` emit + NULL-slot adapter (`fault_slot_param_count`) | `src/backend/llvm/mod.rs:2403-2496, :3960-4010`; `src/backend/c_lir/mod.rs` | ~120 |
| Parser: `EFaultCatch` + `FaultCatchPattern`, 3-way catch disambiguation | `src/parser/ast.rs`, `expr.rs`, visitor, formatter | ~40 |
| Semantic: fault-catch typing + `Fault`-qualifier check, resolve, safety | `typecheck.rs`, `resolve.rs` | ~60 |

Removal collapses `catch` back to ONE production (parenthesized contract binding) in both parsers.

### 2.2 Self-host (~440 mention-lines, unique real files; est. **~700–900 physical lines**)

| Component | Where | Mention-lines |
|---|---|---|
| Lexical catch lowering `lower_fault_catch_expr` | `self_host_lowerer/lower_match.gg:1024-…` | 43 |
| Repanic blocks + Bounds safe-get route + FaultableCall dispatch (`emit_fault_repanic_block` :7415; invoked :7383/:7392/:7401) | `self_host_lowerer/lower_expr.gg` | 82 |
| Deep-participation pre-pass | `self_host_lowerer/lower_generics.gg` | 74 |
| `fill_fault_return_block` twin (:47-104) + slot param in `lower_function` | `self_host_lowerer/lower_closures.gg` | 48 |
| FaultScope fields (:384-389) + module pre-pass (:3530-3535) | `self_host_lowerer/lower.gg` | 34 |
| `participating_fault_fns` typed field + accessor (:930-935) | `self_host_lowerer/gir.gg` | 29 |
| LIR + C emit twins | `lir_codegen.gg` 19, `lir_lower.gg` 13, `lir.gg` 11 | 43 |
| AST/parser/typecheck/infer/resolve/format arms (per-stage copies) | `self_host_{parser,resolver,typechecker}/…` | ~45 |

### 2.3 ggdef + spec artifacts

`spec/ggdef/src/eval.rs` models NO catch; `TrapKind::is_catchable` (:111-112) is a pure
registry accessor (consumers: §10.9 prose + T2a parity lint). Removal = delete the accessor +
the `Catchable?` column of `spec/prose/trap-codes.md`. **The executable definition is already
complete under removal — zero ggdef work.** Under KEEP, ggdef must GAIN fault-scope eval
machinery. `src/trap.rs`: catchability column only.

### 2.4 Test/fixture surface

31 fixtures + ~340 lines `tests/integration.rs` + parity-floor bookkeeping. The 2
panic-default fixtures and the 8 `trap_*`/spectests survive untouched.

### 2.5 The PENDING-WORK kill list (machinery still GROWING — filed follow-ups removal cancels)

1. **Inc-C method/equip participation** (TODO.md:100) — fault-catch does NOT work over method
   calls today; needs slot-threading at a second hand-synced param-build site + new gate + fixtures.
2. `lower_equip_block` param-loop dedup (TODO.md:101) — the ABI-drift smell the slot aggravated.
3. 2.2 transitive participation (deep catch is single-hop only).
4. 2.3b indirect/fn-value fault propagation (NULL slot → panics today).
5. Both-compiler uncaught-fault RE-PANIC normalization (TODO.md:1057-1063) — 3 Rust live-site
   categories + self-host twins still emit `gorget_panic` + exit 1, not `trap[T_X]` + 101.
6. Dead-fault-catch lint (TODO.md:695) — needed precisely because lexical coverage is invisible.
7. `Fault equip Error` unified surface + OOM class (TODO.md:182).
8. Self-host deep-parity residuals (TODO.md:182-183).

### 2.6 The A32 retrofit-pain confirmation

The fault-slot closure adapters cited in the A32 queue entry were a memory-safety incident: a
participating fn as a first-class value has a hidden 3rd param not in its 2-arg callable type;
forwarding a phantom slot wrote a fault tag through a wild pointer (SIGSEGV/ASan) until the
NULL-slot adapter fix (`functions.rs:1222`, `llvm/mod.rs:2487-2496`; gorget-js progress.md:80
records upstream fix `a4ed04ee`). The A32 effect-retrofit cost, paid in miniature.

---

## 3. Q1 — THE SUPERVISED-BOUNDARY HOOK (D24 candidate)

### 3.1 The conversion point's shape

**Recommendation: Task join is the ONLY v1 hook.** No lexical `supervise:` block — that is
`catch_unwind` by another name (same-task partial state → D1-refinement break; drops across
frames → D11 collision; the Rust UnwindSafe cautionary tale; WHY-NOT ledger #4). The task is
the natural whole-unit: defined start, defined result point (join), scheduler-known liveness
(Task drop = join, reference:1395-1405), enumerable resources for teardown.

In-repo working precedent: **`gg test` mode's boundary** (`src/backend/c/runtime/panic_test.c`,
222 lines): `_Thread_local` registered-cleanup stack (:2-18), trap → copy detail OUT of the
dying frames before teardown (:37-44, the R-B hardening), run cleanups from the unit's mark,
`longjmp` to the runner (:52-54, :69-81), report, continue next unit. One test = one
supervised unit of work — the exact shape the Task boundary generalizes.

Spec pin (scheduling semantics stay phase-3): a supervised task converts an in-task trap into
a fault VALUE surfaced at join; the join is a normal `throws`-typed API — so `catch (e):`,
auto-propagation, and D23 totality apply with ZERO new control-flow surface. Whether
supervision is spawn-site (`spawn supervised f()`) or join-site (`t.try_await()` /
`t.await_supervised()`) is syntax for the ruling. Default (unsupervised) stays: a trapped
task's trap propagates at join/drop-join — panic-by-default is permanent.

### 3.2 The fault value's shape

A prelude struct (e.g. `TaskFault`):
- `TrapCode code` — closed prelude enum mirroring the D11 registry 1:1, pinned by the same
  parity-lint mechanism as `src/trap.rs` ↔ ggdef. The code is the ONLY normative surface.
- `String detail` + `String location` — impl-defined; spec states these observe the
  implementation, not the language (the D1 allocator-introspection precedent, decisions.md:62-64).
- **All 8 trap classes convert at the boundary, not just the catchable 3.** The boundary is
  bug CONTAINMENT (Erlang supervisors restart on any crash; catch_unwind catches all panics;
  Go recover recovers any panic); a boundary that lets `T_AssertFailed` through kills the
  server it exists to keep alive. No tension with the lexical catchability principle — that
  governs in-band recovery; the boundary is out-of-band whole-unit discard. (Under D25-remove
  the catchable-subset concept disappears and the question dissolves: registry-uniform.)
- Conversion contract: `T join() throws TaskFault`. D6: `E` fixed by the API — nothing
  inferred. A31: TaskFault is an ordinary error type, composes into future sets. A33 rider:
  satisfied by construction — this IS the explicit conversion point.
- OOM: outside v1 (D11) but the boundary is the reserved surface where per-task allocation
  failure eventually converts.

### 3.3 The permanence set

1. Faults panic by default; observation is always opt-in.
2. Faults never in signatures — `join() throws TaskFault` is ordinary throws; the fault became
   a value AT the point.
3. A33 rider: explicit conversion points only.
4. **Whole-unit discard**: no scope-exit drops inside the trapped task (D11 normative), no
   continuation into partial state, the task's result is REPLACED by the fault value; resource
   reclamation is the boundary's job (per-task heap/arena teardown or a panic_test.c-style
   registered-cleanup stack — phase-3 implementation freedom; spec pins only "reclaimed
   without running user Drop on the trap path"). Flag for phase-3: Shared[T]-lock state held
   by a trapped task (the Rust poisoning question) — v1 spec says only "a supervised trap must
   not deadlock the joiner".
5. A trapped SUPERVISED task dropped without an observing join re-panics at drop-join
   (a fault is never silently swallowed — the swallow-guard rule at the new boundary).

### 3.4 Precedent survey

- **Erlang/OTP** — north star: process = unit of failure; crash → structured value at the
  supervisor only; all crash classes contained; restart-not-recover.
- **Rust catch_unwind + UnwindSafe** — the cautionary tale: in-process catch forced an
  auditing trait nobody understands, mutex poisoning, panic=abort semantic forks, FFI-unwind
  UB. Gorget's boundary is a value conversion at a scheduler-known point, not an unwind.
- **Go recover** — can technically recover anywhere but converged by convention on
  goroutine/request boundaries (net/http per-request recovery); Gorget makes the convention structural.
- **Pony** — no unwinding at all; actor isolation contains faults; made division TOTAL
  (`x/0 == 0`) to avoid a fault channel — the defined-wrong trade Gorget rejects.
- **Swift** — traps uncatchable, full stop; server-side practice isolates at PROCESS level.
  The removal hypothesis IS the Swift model plus the structured Task boundary Swift lacks.

---

## 4. Q2 — SHOULD LEXICAL FAULT-CATCH SURVIVE v1? (D25 candidate)

### 4.1 The evidence

1. **Census: zero organic uses** (§1.1) across all four corpora + spectests.
2. **Pre-check census: zero (c)-sites** (§1.2) — the program that motivated catchable faults
   needed EOF-cursor guards, class (a).
3. **Machinery: ~2,000+ physical lines across both compilers** (§2.1-2.2) + 8 pending
   follow-up tracks (§2.5) + one memory-safety incident already paid (§2.6) — for a feature
   with zero organic uses that doesn't even work over method calls.
4. **Docs already diverge from the implementation** (§0): "local and lexical" is false.
5. **ggdef models no catch** — removal completes the definition for free; keep obligates new
   definitional machinery.
6. **Replacement completeness (Q2.4):**

| Catchable class | Value-level twin | Status |
|---|---|---|
| Bounds (read `v[i]`, incl. negative idx) | `v.get(i) → Option[T]` + `??`/`is Some` | **EXISTS** (reference:3267; `gorget_array_safe_get`, runtime_array.c:56-59, handles negatives) |
| Overflow (`+ - *`; unary-neg via `0 -! x`) | `+! -! *!` (D26) | lands with-or-before removal (D13 pattern) |
| DivByZero + `INT_MIN / -1` | `/! %!` (D26) | with-or-before |
| Shift out-of-range (normalizes to T_Overflow) | `<<! >>!` (D26, lean-include) | with-or-before |

   The deep-catch comparison: today's deep catch is SINGLE-HOP and free-fn-only; the throws
   channel `+!` rides propagates any depth, through methods/closures/HOFs — the replacement is
   strictly MORE powerful than the machinery it retires.
7. **Taxonomy coherence** (2026-07-11): the lexical catch is the one place a fault becomes a
   value without a signature-visible, explicitly-fallible form.

### 4.2 Recommendation

**REMOVE lexical + shipped single-hop-deep fault-catch from v1 (the Swift model), GATED on
D26 landing with-or-before (D13 pattern).** Faults become uniformly uncatchable; the Q1
boundary is the only fault→value conversion; delete the `Fault` prelude enum, the
catchable-subset concept, and the trap-codes catchability column.

- Fixtures: ~10 migrate to `+!`/`.get()` forms as D26 positives (§5.6), 2-3 become negative
  fixtures asserting `catch Fault.X:` is rejected with a fix-it naming the replacement, rest delete.
- Docs write-through: §10.5 fault paragraph deleted, §10.9 rewritten, language-design
  §2.2:210-218 + §6.4 "Recoverable Faults" replaced by the fallible family + boundary story,
  book ch.10 sweep — this write-through also fixes the already-stale "local and lexical" text.
- Order: fixtures/tests flip → lowering (both compilers) → parser/AST → docs; bootstrap-gated.

### 4.3 Fallback if kept (Q2.5)

Write the catchability principle into trap-codes.md: **implicit machine checks (Overflow,
DivByZero, Bounds) catchable; explicit programmer assertions (unwrap, assert, panic) never**
— a machine check is the compiler's test on data; an assertion is the programmer's own
declared invariant; catching your own invariant is self-contradiction. Then fund the 8-track
queue (§2.5), starting with Inc-C — the free-fn-only hole is arbitrary and user-visible.

### 4.4 Evidence that would change it

A genuine (c)-class site (none found in ~115k corpus lines); an owner domain where
task-wrapping is unacceptable AND per-op forms too fine; D26 rejection (Overflow/DivByZero
would lose their replacement — D25-remove is conditional on D26-adopt).

---

## 5. Q3 — FALLIBLE-OPERATOR FAMILY, re-glyphed `+!` per Q4 (D26 candidate)

### 5.1 The symmetry to pin

| form | semantics | on failure | type story |
|---|---|---|---|
| `a + b` | checked assertion | trap `T_Overflow` (a bug) | `int`, no channel |
| `a +% b` | wrapping | never fails (defined) | `int` |
| `a +! b` | fallible | **throws** prelude arith error (data) | `int` in EVERY position (D23); auto-propagates; catchable with existing `catch (e):`; capturable as `Result[int, E]` |

(Family: `+! -! *! /! %!`; lean-include `<<! >>!` for class completeness. `+?` is the
REJECTED-for-collision alternative — see §6.4: it would break the in-language `?` = Optional
convention that `?.`/`??` already establish.)

### 5.2 Error type

One closed, payload-free prelude enum (name at ruling):

```gorget
enum ArithError:      # equips Error like IoError/ParseError
    Overflow          # +! -! *! overflow; INT_MIN /! -1; INT_MIN %! -1; <<!/>>! range
    DivByZero         # /! %! zero divisor
```

- Payload-free: operands are in scope at the catch site; a payload drags allocation into the
  hot path and would tempt conformance to compare impl text. Location rides the normal thrown-
  error trace machinery (language-design §6.3).
- Mapping mirrors the trap registry exactly (INT_MIN/-1 → Overflow, not DivByZero — same rule
  as `fault_catch_intmin_div.gg`; shift-range → Overflow per the 2026-07-10 ruling).
- **D6: no interaction** — `E` is fixed by the operator. **A31:** ArithError is an ordinary
  member of any future inferred set. **A33 rider:** `+!` is an "explicitly fallible API" —
  the rider's third category; strictly, an overflowing `a +! b` never WAS a fault — the
  operator is a different operation returning through the channel; the trap registry is untouched.
- Typecheck: integer operands only (floats never overflow-trap → rejected with fix-it).

### 5.3 Implementation shape (why this is cheap)

`a +! b` types and lowers exactly as a call to an intrinsic `int __checked_add(int a, int b)
throws ArithError`: checked op → on-failure construct `Error(ArithError.X)` into the EXISTING
throws/Result channel (auto-prop hook `exprs/mod.rs:87/:2922`, `emit_early_exit_drops` — all
long-shipped, drop-correct, both compilers). No new propagation machinery, no new control-flow
IR: the three Faultable GIR variants are NOT needed for it. Lexer tokens + binop-table rows +
one typecheck arm + one lowering arm per compiler.

### 5.4 Compound assignment (`+!=` …) — recommend EXCLUDE from v1

`x +!= y` is a THROWING statement: fine in throws fns, unusable in non-throws fns (postfix
catch binds to the RHS expression, not the operator's own failure) — an operator whose
compound form works in half the contexts is worse than its absence (`+%=` is not a precedent;
it never fails). Visual hazard seals it: `a +!= b` is one keystroke from `a != b`. Pure
widening later. Flag to owner as a deliberate asymmetry with the `%`-family.

### 5.5 Precedent survey (the owner's "how does Swift do it?")

- **Swift**: default `+` traps uncatchably; `&+ &- &*` wrap (Gorget's `+%`); checked =
  `addingReportingOverflow()` → tuples, universally considered clunky; no checked operator
  ever landed in Swift Evolution.
- **Rust**: `checked_*` → Option, `overflowing_*`, `wrapping_*`, `saturating_*`,
  `Wrapping<T>`/`Saturating<T>`; checked-operator RFCs floated, never accepted;
  `a.checked_add(b).ok_or(E)??` is the acknowledged wart.
- **Zig**: `+%` wrap, `+|` saturate, checked = `std.math.add(T, a, b)` → error union with
  `try` — closest semantic prior art, but as a FUNCTION.
- **⚠ Pony — CORRECTION to the mandate's premise: Pony ships fallible arithmetic OPERATORS**,
  spelled `+?` `-?` `*?` `/?` `%?` ("partial arithmetic": raise Pony's `error` on
  overflow/div-zero, compose with `try`; Pony's default `+` wraps and `x/0 == 0`). So the
  operator-family idea has field history. Pony's error is UNTYPED and payload-free with no
  propagation typing; Gorget's typed prelude error in a statically-totalized channel (D23)
  with auto-propagation remains novel. Note Pony chose the `?` glyph — consistent with its
  own `?`-for-partiality convention; Gorget's convention (Q4) assigns partiality-with-error
  to `!` and Option-ness to `?`, so `+!` is the convention-consistent Gorget spelling.
- Conclusion: adopt the operator family; one prior occupant de-risks the concept; the glyph
  choice is convention-local.

### 5.6 Migration preview (5 spot-samples of the 31 fixtures)

1. `fault_catch_overflow.gg`: `int r = (big * 2) catch Fault.Overflow: -1`
   → `int r = big *! 2 catch (_): -1`
2. `fault_catch_div0.gg`: `int r = (10 / z) catch Fault.DivByZero: 999`
   → `int r = 10 /! z catch (_): 999`
3. `fault_catch_binding.gg` (which-fault dispatch):
   `int d = (10 / z) catch f: match f:` / `case Fault.Overflow(): 111` / `case Fault.DivByZero(): 222`
   → `int d = 10 /! z catch (e): match e:` / `case ArithError.Overflow(): 111` / `case ArithError.DivByZero(): 222`
4. `fault_catch_bounds.gg`: `int r = (xs[10]) catch Fault.Bounds: -1`
   → `int r = xs.get(10) ?? -1`   *(shorter, already idiomatic, and `?`-for-Option per Q4)*
5. `fault_deep_catch.gg` (deep — where the new form is strictly stronger):
   `int faulty(int a, int b): a * b` + `faulty(big, big) catch Fault.Overflow: -1`
   → `int faulty(int a, int b) throws ArithError: a *! b` + `faulty(big, big) catch (e): -1`
   — failure now rides the ONE channel: signature-visible (D23 diagnostic contract),
   any-depth, works through methods/HOFs (which fault-catch never did).

Compound coverage (`fault_catch_compound.gg`) becomes per-op: `(big *! 2 + 100 /! 5) catch (_): -7`
— slightly more verbose than blanket lexical scope, and BETTER: which ops are checked is
visible (the filed dead-fault-catch lint becomes unnecessary).

Fixture cost: ~10 migrate 1:1 (D26 positives), 2-3 become negatives, drop/CoW-coherence
guards map onto the throws channel's long-locked drop tests, rest delete.

### 5.7 Saturating family

Reserve the NAME informally ("a saturating family is held"), spec nothing. Under Q4's swap the
Zig glyph `+|` remains available and unconflicted. Zero corpus demand today; pure widening.

### 5.8 D18 / const-eval interaction — coherent, no new rule

`a +! b` does not FAULT — it throws into the channel. Mirror-runtime: const `1 +! 0` folds to
1; const `INT_MAX +! 1` evaluates to the error value; an unhandled error value in a const
initializer is rejected by D23 TOTALITY (no throws-destination in const context →
E_UnhandledThrows) — same outcome as const `INT_MAX + 1` (compile error), different, correct
diagnostic. `const int x = INT_MAX +! 1 catch (_): 0` → 0 at compile time iff const-eval
supports catch-expressions. **Flag for the ruling:** catch-in-const v1 support (mirror-runtime
says eventually yes; rejecting v1 with a clear diagnostic is a safe narrowing).

### 5.9 What would change D26

Owner glyph taste (Q4 decides `+!` vs `+?` vs third glyph); waiting for A31 inferred sets is
unnecessary (the named enum is forward-compatible as a set member).

---

## 6. Q4 — SIGIL ECONOMY: `!` = errors, `?` = optionals, move rehomed (D27 candidate)

### 6.1 Census: the `!`-move surface (occurrences by site class)

Method: grep `![a-zA-Z_]` over concatenated corpus (excludes `!=`); classes by context regex;
counts are upper bounds (a few hits sit in comments/strings). Verified orchestrator
pre-findings: `not` is the negation keyword (reference:1511 row 16); prefix `!` is exclusively
move (`Token::Bang`; **7** `Token::Bang` match sites in `src/parser/` — types.rs:27,204,
expr.rs:569,2055, mod.rs:240,1872, stmt.rs:680 — vs the pre-finding's "10"; plus formatter/
lexer/diagnostic sites outside the parser).

| Corpus | lines | total `!x` | call-site `f(!x)` | `(!self` | param `T !n` | assign `= !x` | return `!x` | density /kloc |
|---|---|---|---|---|---|---|---|---|
| fixtures (1,515 files; 133 w/ any move) | 51,518 | 337 | 149 | 42 | 79 | 33 | 1 | 6.5 |
| self-host (real files) | 85,199 | 365 | 229 | 6 | 45 | 13 | 1 | 4.3 |
| gorget-js | 13,622 | 36 | 2 | 0 | 0 | 12 | 0 | 2.6 |
| gorget-arena | 15,931 | 132 | 95 | 0 | 5 | 1 | 0 | 8.3 |
| **TOTAL** | ~166k | **≈870** | 475 | 48 | 129 | 59 | 2 | ~5.2 |

(Unclassified remainder ≈ comments/strings/f-string text and rarer shapes.)

- **D7 capture lists: ZERO corpus uses** (`!():` = 0 hits anywhere; the D7 per-variable
  capture syntax is ratified but unimplemented) → the capture-position re-spelling is a PURE
  SPEC RIDER, zero code migration.
- **Bare postfix `?`: fully dormant in BOTH compilers** — Rust: `Token::Question` appears in
  ONE closure-disambiguation lookahead (`expr.rs:1468`), no production; self-host:
  `TkQuestion` appears only in a token-kind mapping table (`self_host_typechecker/parser.gg:226`),
  no production. Zero code uses (all grep hits are inside string literals — URLs, prompts).
  The postfix-`?` Option early-return in language-design.md:803-807 is design prose only.
- **`??` / `?.` counts**: `??` = 84 occurrences (28 fixtures / 40 self-host / 16 gorget-js /
  0 arena), in 19 fixture-tree files (pre-finding confirmed). `?.` = 2 (self_host_lexer
  implementation/format code only). The `?` = Optional convention is real and carried by `??`.
- **Huffman argument: HOLDS.** Moves run ~2.6–8.3/kloc and are boundary-mechanical
  (ownership transfer at consuming positions); the error channel is the language's headline
  (gorget-js: "every test routes through `throws RuntimeException`", progress.md:80; the
  self-host is throws-dense). The rare op can afford a keyword; the common concept deserves
  the glyph family. Honest caveat: the FALLIBLE OPERATORS' own future frequency is unknown
  (possibly lower than moves in numeric-light code); the stronger form of the argument is
  GLYPH BUDGET, not raw counts — `!` freed from move lets errors own a whole future sugar
  family (`+!`, and any later error-position forms), while `?` stays coherently Optional.

### 6.2 Move-rehoming candidates at all four positions

**(a) `move` keyword — RECOMMENDED. Decisive finding: `move` is ALREADY a reserved keyword
and ALREADY a move-closure prefix.** `Keyword::Move` (`src/lexer/token.rs:323/:442/:532`);
reference keyword table :153; reference:483 "Move closure (`!` or `move` prefix)". Better
still, the parser TODAY parses `move (params): body` and carries an explicit rejection arm
for everything else: `src/parser/expr.rs:586-595` — "use `!` for move expressions (e.g.
`!x`). The `move` keyword is only valid for closures" — i.e. rehoming flips an existing
diagnostic into the real parse. Zero new-keyword reservation cost, zero identifier collision.

| Position | Today | Under `move` | Reading |
|---|---|---|---|
| call | `f(!x)` | `f(move x)` | Rust-identical prior; excellent |
| assign | `String t = !s` | `String t = move s` | Rust/C++ prior; excellent |
| param | `void consume(Message !msg)` | `void consume(Message move msg)` | the weak point: keyword between type and name; asymmetric with `&msg` — but reads as English and param-moves are only ~129 sites corpus-wide |
| capture (D7) | `(!name, &total)(x):` / `!():` | `(move name, &total)(x):` / `move ():` | `move (…):` ALREADY PARSES today; the D7 rider is nearly free (spec-only re-spelling; zero corpus uses) |
| Drop self | `void drop(!self)` | `void drop(move self)` | 48 sites corpus-wide |

**(b) `take` keyword — REJECT.** `take` is a live METHOD across the corpus: the Iterator
adapter `.take(n)` (43 fixture + 88 self-host + 4 gorget-js `\btake\b` hits incl. user fns
`void take(...)`). Making it a keyword breaks method-position uses or forces
keyword-as-method-name carve-outs. No prior over `move` compensates.

**(c) `^` sigil — VIABLE but weak.** Prefix `^` is currently free (no unary `^`; infix xor
prec 9 + `^=` only), and param `Message ^msg` mirrors `&msg` nicely. Costs: `^` must leave the
"never-a-sigil" closure-disambiguation list (`expr.rs:1459-1460`) and joins the dual-role
ambiguity set the parser maintains for `- * & !` (`expr.rs:1449-1455` comment); priors are
foreign-or-wrong (Obj-C blocks, Go xor, Pascal deref); LLM/human zero-shot reading of
`f(^x)` is a guess where `f(move x)` is not. Choose only if the owner wants sigil symmetry
with `&` more than readability.

Diagnostics/docs surface for any rehoming (grep-able, mechanical): `E_MoveWithoutOperator`
message ("write `!source` or `source.clone()`"), the expr.rs:593 hint text, CLAUDE.md sigil
quick-ref (two ✓/✗ lines), README, reference §5.1/§7.14/§7.6 call_arg grammar/row-16
precedence/§9 ownership, language-design §3.1/§3.4, book ownership chapters, formatter
emit code in BOTH compilers, self-host lexer/parser (it compiles Gorget), gg fmt.

### 6.3 `+!` lexing under the rehomed grammar — CLEAN

Once move is rehomed and negation stays `not`, prefix `!` vanishes; `!` survives only inside
`!=`. Add `#[token("+!")]` etc. (Logos maximal munch; self-host lexer's hand-ordered
`lex_emit` mirrors — the `+%`/`+%=` precedent at token.rs:55-66 / self-host lexer.gg:862/:938).
- `a != b` unaffected (distinct first char). `a +! b`, `a+!b` → PlusBang. `a *! -b` → StarBang, unary minus.
- Compound `+!=` excluded (§5.4) → `a +!= b` lexes `+!` `=` → loud parse error, never silent.
- **Legacy-migration hazard measured EMPTY: zero `+ !x` / `+!x` shapes in all four corpora**
  (a move expression as a bare arithmetic operand doesn't occur), so re-lexing risk during
  migration is nil; and post-rehoming, any stray `!x` is itself a parse error with a
  migration fix-it ("prefix `!` is no longer the move sigil; write `move x`").
- During any transition window, keeping `Token::Bang` lexed but parse-rejected with that
  fix-it gives mechanical, loud migration.

### 6.4 Prior-collision analysis (feeds the LLM-correctness KPI)

- **Zig-primed readers/LLMs**: `!` = error-union (`fn f() !T`) — read `a +! b` as "add that
  can error" — CORRECT.
- **Swift/TS/Kotlin-primed**: postfix `!`/`!!` = force/non-null-assert — may read `+!` as
  "add and trap on failure" (the OPPOSITE). Verified consequence chain: a user who writes
  `a +! b` wanting trap semantics (i) in a non-throws fn with no handler hits D23's
  `E_UnhandledThrows` COMPILE error ("this call throws ArithError; declare throws or handle
  it" — reference:2418, the D23 diagnostic contract) — loud, immediate; (ii) in a throws fn,
  the signature must name ArithError (visible) and the failure propagates as an error rather
  than trapping — still a failure, never silent wrong data; (iii) a user who actually wants
  trap semantics writes plain `+`, which already traps — the default IS the Swift reading.
  Conclusion verified: no silent-behavior surprise is reachable from the misreading.
- **`+?` (the rejected glyph)**: Pony-exact prior, but in-language it would put `?` on a
  construct that neither produces nor consumes an Option — breaking the `?.`/`??`-established
  convention exactly as the owner argued. Documented as rejected-for-collision.
- The optional-family future widening (one paragraph, not specced here): with `?` reserved
  for Option-ness, `v[i]?` → `Option[T]` indexing (sugar for `v.get(i)`) chaining with
  existing `??`/`?.` is the natural later addition; it composes with D26 without overlap
  (`!` forms throw; `?` forms return Option).

### 6.5 D27 (sigil economy) — option-question draft

**Options:**
- **(A) FULL SWAP — recommended.** `!` = errors (the `+!` family now; future error-position
  sugar later), `?` = optionals (existing `??`/`?.`; `v[i]?` as future widening), move →
  `move` keyword at all four positions; `!()` move-all sugar replaced by the already-parsing
  `move (…):`; D7 capture lists re-spelled `(move name, &total)` (pure spec rider, zero code).
  Evidence: `move` already reserved + already a closure prefix + an existing diagnostic arm
  that literally points the other way (§6.2a); moves rare (~5/kloc) and mechanical (Huffman);
  Zig prior aligns; D23 guards the misread (§6.4); `+ !` adjacency hazard measured zero (§6.3).
- **(B) `!` stays move; fallible family takes a third glyph** (not `+?` — collision). The
  leftover glyph space is poor (`+~`? `+|` is reserved for saturating; `+#` comment-adjacent)
  and errors — the headline feature — never get a coherent glyph identity. Weakest option.
- **(C) Status quo + `+?`.** Breaks the `?` = Option convention the shipped surface already
  carries; two failure monads share one glyph. Rejected-for-collision (owner's own argument).

**Migration cost (from the census, all mechanical/grep-able):** ≈870 corpus occurrences
(fixtures 337 over 133 files · self-host 365 · gorget-js 36 · arena 132) in four sed-able
classes; both compilers' lexer/parser/formatter + `E_MoveWithoutOperator`/`expr.rs:593`
diagnostics; docs sweep (CLAUDE.md quick-ref, README, reference, design, book). `gg fmt` is
the natural auto-migration vehicle (parse old, emit new); bootstrap-gated (self-host source
itself carries 365 sites). Estimate: one medium mechanical track + a transition-window lexer
fix-it. **Would change it:** owner keeps `!x` terseness at call sites as a value judgment
(the census says the cost is ~475 call sites reading `f(move x)` instead of `f(!x)`); a
future unary-negation reclaim of `!` (owner has shown no interest; `not` is established).

---

## 7. WHY-NOT LEDGER → appendix draft "Why not dynamic exceptions"

Rejected alternatives with killing reasons:

1. **Dynamic/deep fault catch (unwinding or slot-propagated), unbounded depth** — breaks D1
   refinement (mid-stack handlers observe partial states that distinguish lazy from eager);
   collides with D11 no-drops-on-trap (either drops run on unwind — greenfield machinery ×4
   implementations — or handlers see leaked/inconsistent frames); violates the A33 rider's
   spirit (faults become values implicitly, at any distance); measured cost: the SINGLE-HOP
   restricted version already cost ~2,000 lines + a wild-write memory-safety incident + 8
   pending tracks (§2).
2. **Faults in the contract channel (`throws Fault` everywhere)** — the §3 ubiquity
   impossibility; measured self-host signature flood (error-model.md:527-528, Option B
   rejected 2026-06-22); Swift traps rather than throws — the counter-precedent.
3. **Checked METHODS (`checked_add`)** — owner-rejected on ergonomics; Swift/Rust field
   experience concurs (tuple/Option ceremony, unused in application code).
4. **Lexical `supervise { }` block / catch_unwind** — Rust's cautionary tale (UnwindSafe,
   poisoning, panic=abort forks); same-task partial state = deep catch with a nicer name;
   the Task boundary already exists as the coherent unit.
5. **Wrapper types (`Wrapping<T>`)** — type-granularity for a per-OP decision; Gorget chose
   per-operator with `+%` ("no global mode changes `+`", reference:1552).
6. **Scoped modes (C# `checked {}`)** — action-at-a-distance; semantics change invisibly
   with context.
7. **Total-by-definition arithmetic (Pony `x/0 == 0`)** — silently-wrong results in totality's
   name; contradicts language-design:191's founding sentence.
8. **Fault → Result auto-inference at boundaries** — forbidden verbatim by the A33 rider
   (decisions.md:299-305).

Draft spec prose:

> **Why not dynamic exceptions.** Gorget deliberately has no dynamic exception mechanism — no
> construct that transfers control from a fault site to a handler an unbounded number of
> frames away. Languages with pervasive unwinding (Java, C#, Python) make every call a
> potential exit and every scope a potential witness of half-completed work; the cost is not
> the unwinder but the *program states it manufactures*: every function must be correct not
> only for its return paths but for every interior point a foreign handler can observe.
> Gorget's semantics rest on three commitments unwinding would break: value semantics whose
> lazy copy-on-write refinement must be unobservable (D1); deterministic drops that never run
> on the trap path (D11); and one typed error channel in which failure is data in the
> signature, never control flow around it (D23). A fault — an overflow, a division by zero,
> an out-of-bounds index — is a *bug the machine caught*, not a value the program produced.
> Bugs are not handled; they are contained: the faulting unit of work (a task) is discarded
> whole, and the fact of its failure becomes an ordinary value — a `TaskFault` carrying the
> trap code — at exactly one place, the supervised join, in the same typed channel as every
> other error. Where failure is *expected data* — arithmetic on untrusted inputs, an index
> that may miss — Gorget gives it a spelling that says so in the expression: `a +! b` throws;
> `v.get(i)` returns an `Option`. The result is a language in which every failure is either a
> value you can see in a type, or a termination you can only contain — never a control
> transfer you have to fear.

---

## 8. THE FOUR OPTION-QUESTIONS (candidate rulings)

### D24 — supervised-boundary hook (Q1)
**Recommend:** Task join = the ONLY v1 conversion point (no supervise block); fault value =
prelude struct with closed `TrapCode` (ALL 8 classes; code = the only normative surface;
detail/location impl-observed); conversion = plain `throws`-typed join (syntax at ruling);
permanence set: panic-by-default, faults-out-of-signatures, explicit conversion only,
whole-unit discard with no drops on the trap path, unobserved supervised trap re-panics at
drop-join. Scheduling stays phase-3. **Would change it:** phase-3 scheduler without per-task
resource enumeration; a real sub-task containment need (census: none).

### D25 — lexical fault-catch disposition (Q2)
**Recommend: REMOVE (Swift model), gated on D26 with-or-before (D13 pattern).** Census: 0
organic uses anywhere; 0 (c)-class pre-check sites; ~2,000+ both-compiler lines + 8 pending
tracks retired; ggdef already complete under removal; every catchable class has an
equal-or-better value-level twin; the "local and lexical" doc promise is already false.
**Fallback if kept:** catchability principle into trap-codes.md (machine checks catchable /
assertions never) + fund the 8-track queue starting with Inc-C. **Would change it:** a
genuine (c)-site; owner domain where task-wrapping is unacceptable AND per-op forms too fine;
D26 rejection.

### D26 — fallible-operator family (Q3, re-glyphed per Q4)
**Recommend: ADOPT `+! -! *! /! %!`** (+ lean-include `<<! >>!`), throwing payload-free
prelude `enum ArithError: Overflow / DivByZero` into the ONE channel; D23-total; precedence =
base ops; integer-only; compound `+!=` EXCLUDED v1; `+|`-saturating name-reserved; const
story falls out of D18+D23 (flag: catch-in-const timing). Lexing verified trivial in both
lexers; zero `+ !` adjacency hazard measured. `+?` documented as rejected-for-collision with
the `?` = Option convention; Pony precedent (`+?` partial arithmetic) validates the operator
concept, Gorget's typed+total version stays novel. **Would change it:** owner glyph taste
(D27 outcome).

### D27 — sigil economy (Q4)
**Recommend: FULL SWAP** — `!` = errors, `?` = optionals, move → `move` keyword at all four
positions. Decisive: `move` is ALREADY a reserved keyword AND an already-parsing move-closure
prefix with a diagnostic arm that today points users to `!x` (`expr.rs:586-595`) — rehoming
flips one arm; `take` rejected (live `.take(n)` method, 135+ hits); `^` viable-but-foreign.
Moves are rare (~870 sites, ~5/kloc, four mechanical classes) — Huffman says the rare op can
afford the keyword; D7 capture rider is spec-only (zero corpus uses of capture lists);
`move (…):` move-all already parses. Migration = one medium mechanical track via `gg fmt` +
transition fix-it, bootstrap-gated. **Would change it:** owner terseness preference at the
~475 call sites (`f(move x)` vs `f(!x)`).

---

## 9. Corrections this scout makes to prior premises (Core #5 discipline)

1. Pre-census "39 fixture files" → **31 fixture files actually USE fault-catch** (the 39
   included 12 self-host files whose 17 hits are all comments; 3 binding-form-only fixtures
   were missed by the `catch Fault` grep; `fault_catch_bad_qualifier.gg` uses `catch Bogus.Overflow`).
2. "0 gorget-js files" — confirmed exactly right.
3. Mandate Q3.3's "no language ships fallible arithmetic operators" → **corrected: Pony ships
   `+? -? *? /? %?` partial arithmetic** (untyped error; Gorget's typed/total version stays novel).
4. §10.5 "local and lexical" is stale spec prose — single-hop deep catch is implemented and
   fixture-locked in both compilers.
5. The 2026-07-07 finding-(b) framing overstates: current gorget-js "pre-checks" are (a)-class
   EOF-cursor guards, not fault-recovery workarounds.
6. Q4 pre-finding "10 parser sites for Token::Bang" → measured **7** in `src/parser/`
   (plus formatter/lexer/diagnostic sites elsewhere).
7. Q4 pre-finding "`??` in 19 fixture files" — confirmed (19 files incl. self-host-tree
   implementation files; 84 occurrences corpus-wide).
