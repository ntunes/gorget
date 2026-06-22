# RFC: The Gorget Error Model — one typed error channel, two kinds of error

> **Status: DRAFT / exploratory (owner brainstorm 2026-06-20).** This is a
> LANGUAGE-DIRECTION doc, far bigger than any parity fix. It captures a design
> exploration; nothing here is approved or implemented. It needs the full
> scout → brief → ≥3 fresh reviews cycle before any code moves. Companion to
> [`cast-via-construction.md`](cast-via-construction.md) — conversion-overflow and
> arithmetic-overflow are the two worked examples that fall out of this model.
>
> **Scout pass 1 complete (2026-06-20):** premises P1–P8 verified against current
> source (overflow DOES panic today; `throws` is sugar for `Result`; the
> `From`-widening machinery is real; precedents all accurate). Two findings folded
> below: **§0.5** — this RFC *reverses a documented, rule-backed safety decision*
> (`language-design.md:1312`), the #1 blocking item; and **§6 reality-check** —
> the fault-unwind leg is greenfield (production panic = `exit(1)`), not reuse.

## 0. The question that spawned this

The cast RFC decided **conversion overflow throws** (`byte(x)` is recoverable).
That raised: *should ARITHMETIC overflow (`a + b`) also be recoverable rather than
hard-panic — and if so, how, without taxing every function?* Owner's deeper probe:
*would it be that bad to give EVERY function a typed error channel (like every
function having `stdout` + `stderr`, both typed) — most throwing `Never`, but
auto-propagating?* This doc answers both, and the answer is one model.

## 0.5 ⚠ THIS REVERSES A DOCUMENTED, RULE-BACKED DECISION (scout-verified 2026-06-20)

The single most important caveat — the doc must not bury it. The existing language
design **deliberately and explicitly** classifies overflow, bounds, div0,
unwrap-on-`None`, `assert`, and OOM as **panics**, by a *stated rule*:

> *"Can the caller prevent this failure by writing correct code? **Yes → panic.
> No → Result.**"* — `docs/language-design.md:1312` (§6 Panic vs Result — heading
> `:1295`, panic list `:1297-1303`; verified review pass 1)

By that rule overflow is unambiguously a panic (the caller *can* prevent it: wider
type, `checked_*`, `+%`). `language-design.md:191` frames overflow-panics as a
safety stance ("catches bugs that silently corrupt data in C/Go");
`book/10-errors.md:6-14` teaches users that overflow/bounds/unwrap "**panic
immediately, because continuing with corrupted state is worse than stopping**" —
which is *exactly* the "recover-and-keep-serving" this RFC proposes for faults.

So this RFC does not merely *add* a fault kind — **it overturns a deliberate,
documented design decision** (overflow was once switchable build-wide; that
global mode has since been retired so plain `+`/`-`/`*` always check).
That reversal must be argued on its merits, and the docs
(`language-design.md` §2.2 + §6, `book/10-errors.md`) rewritten as part of the
work. It cannot be slipped in as "just a classification." This was the scout's #1
reservation.

**✅ OWNER-DECIDED 2026-06-21: the reversal is ACCEPTED.** The intent ("early
gorget was very strict on overflow; the user should be able to recover") is
confirmed: `language-design.md` §2.2 + §6 (the `:1312` "caller can prevent → panic"
rule) and `book/10-errors.md:6-14` are to be **rewritten** to the new model
(overflow/bounds/div0 become recoverable faults; the Panic-vs-Result rule is
restated). This is no longer an open blocker — it is committed scope. The doc
rewrite is part of the implementation work, not a precondition to be argued.

**Softened by the phased model (§9.1, 2026-06-21):** the change is **ADDITIVE, not a
full reversal.** Overflow/bounds/div0 **stay panic-by-default** — the documented
"continuing with corrupted state is worse than stopping" rationale still holds for
the *uncaught/default* case. We only ADD **opt-in recovery** (local in Phase 1, deep
in Phase 2). So the doc edit is "overflow panics by default AND is catchable," which
is far less disruptive than overturning the rule outright.

## 1. Vocabulary (read first — there are TWO axes, don't conflate them)

**Axis A — Channels (a function's output paths). Exactly two:**
- the **value channel** (the success result), and
- the **error channel** (the failure result), **typed**, defaulting to `Never`
  (a function that cannot fail has error type `Never` and the channel is elided).

This is the function *shape*. Every function has both. It is exactly Zig's `!T`
("error union") and Koka's effect row. A non-failing function is just the dull
case where the error channel is `Never`. There is **one** error channel.

**Axis B — Kinds of error (a classification of the TYPES flowing in the error
channel). Two kinds:**
- **Contract errors** — failures that are part of the function's *contract*
  (parse, I/O, a narrowing conversion of external data). The caller is *expected*
  to deal with them. Curated, usually a small set.
- **Faults** — bugs and resource-exhaustion (integer overflow, out-of-bounds,
  divide-by-zero, OOM). Not part of any contract; they mean "something is wrong,"
  and the realistic recovery is coarse (fail this request/task, keep serving).

**"Two channels" ≠ "two kinds."** Channels = value vs error (the shape). Kinds =
contract vs fault (a property of the error *type*). The error channel carries
*both kinds*; the kind drives the *obligations*, not the channel count.

## 2. The model in one paragraph

⚠ **SCOPE (2026-06-21): this §2 describes the BROAD/eventual model.** The FUNDED
near-term — **Phase 1 (§11)** — is NARROWER and does **NOT** implement the universal
inferred error channel below: it leaves today's explicit `throws E`/`Result`
*contract* model UNCHANGED and adds only the `Fault` enum + local catch. **No
function becomes `throws Never` in Phase 1; no signature gains an error channel.**
The universal-inferred-channel + boundary-annotation (§5) is an un-phased, larger
aspiration, not on the Phase-1/2 path. See **§11.0**.

Every function has a value channel and one **inferred, typed** error channel,
`Never` by default, auto-propagated to callers (no explicit `?`/rethrow in the
common path), with the inferred error type **annotated at public/module
boundaries** (inferred internally, declared at the API surface). Error types are
classified **contract** or **fault**. Contract errors impose handling and sit on
the API/compat surface; faults auto-propagate to a coarse boundary, are catchable
there (precisely, by type), default to **abort** if uncaught, and are **excluded
from the API/compat surface**. That single rule set gives recoverable, typed
overflow *without* the universal-throws pathologies.

## 3. Why faults must be a distinct KIND (the impossibility argument)

The owner wants three things simultaneously:

1. **default `a + b` overflow is recoverable**,
2. the **typed error channel stays informative**,
3. the model is **universal + typed**.

**You cannot have all three.** "Default `+` recoverable" puts `Overflow` in the
inferred error type of ~**every function that does arithmetic** — ~99% of them. An
error type present on 99% of functions **partitions nothing**: "can throw
Overflow" then carries as much information as "uses memory." The information
content of a distinction is *how often the two sides differ*; overflow-in-row vs
not is ~99/1. **Typing does not rescue this** — a ubiquitous effect is an
uninformative one whether typed or not.

The escapes, and what each costs:
- make default `+` *wrap/panic*, only `a.checked_add(b)` enters the row → row stays
  informative, **but you lose "default overflow recoverable"** (goal #1).
- put overflow in the contract row by default → **row stops being informative**
  (goal #2), public APIs gain `Overflow` transitively (compat breakage), and
  high-level error types become sprawling `Overflow | Bounds | DivZero | Parse | …`
  unions you can't catch meaningfully.

**The `fault` kind is the only thing that satisfies all three:** a fault is
*recoverable* (goal #1) and *typed* (goal #3), but **not part of the contract** —
so it does not impose handling and is off the compat surface, leaving the
informative contract row sparse (goal #2). Recoverable-but-not-contract is exactly
what "fault kind" means.

**One alternative the argument must NOT define out (scout):** "faults stay
*untyped panics*, recovered only at a coarse process/task boundary" — the
Erlang/Midori model §8 cites approvingly — *also* satisfies "recoverable overflow"
without a typed channel at all, and is arguably **simpler** (no second "kind," no
`catch`-by-type, no classification metadata, and it's closer to what the runtime
can almost do — test-mode already longjmps panics). The impossibility argument is
sound *given the premise* "faults are typed and live in the error channel"; it does
not refute untyped-panic-with-supervision. This RFC *prefers* typed-and-catchable-
by-type (so you can `catch Overflow` distinctly from `catch Bounds`), but it must
**argue that preference on its merits, not by omission.** Open: justify typed
faults over untyped-panic-with-supervisor recovery (now §9 Q14).

### 3.1 Is recovering from a fault even SAFE? (the corrupted-state objection — review pass 1)

The docs' objection is explicit (`book/10-errors.md:13-14`): *"continuing with
corrupted state is worse than stopping."* The overflowing op already produced an
undefined value — is it safe to recover and keep going? **The answer is YES, but
ONLY because recovery is at a COARSE boundary — and that must be LOCKED IN, not
left open.** When a fault is caught at a task/request/supervisor boundary, the
*entire unit of work* (the request, the task) is **failed and unwound**, and **no
application logic past the boundary observes its outputs** (the unit's results are
discarded). That is the Erlang/Midori "abandon the unit of work" answer (§8), and
it is safe in a way that "`catch Overflow` *inline* and keep using the result" is
**not**. So the safety property hangs on **boundary-only catch**: §9 Q1's
"catchable *anywhere*?" answered "anywhere" **resurrects the exact footgun the docs
warn against** (recover mid-computation, keep using the corrupted value). **Design
invariant (for Phase-2 DEEP catch): a fault from a CALLED FUNCTION is catchable only
at a declared coarse boundary that discards-and-unwinds the unit of work.**

⚠ **Carve-out — superseded for Phase 1 by §9.1/§11 (final review 2026-06-21):** the
"never inline" rule above targets the SPECIFIC footgun of *catching inline AND then
keeping the corrupted result*. **Phase-1 *local* catch is NOT that footgun and IS
safe:** the checked op **branches before the store** (§11.2), so the corrupted value
is never observed — the handler computes a fresh fallback. So Phase-1 inline/local
catch is *permitted and safe*; the boundary-only constraint applies only to the
**Phase-2 deep (cross-call)** case, where a fault propagated up from a callee must
discard the whole unit of work.

⚠ **Precision (review pass 2): "discarded" is NOT "never observed."** Gorget has a
user-definable `Drop` (`language-reference.md:2841`: `drop(!self): close_fd(self.fd)`
— the destructor reads `self.fd`) that **runs on the failure path**
(`language-reference.md:4743` — `drop()` "is called on both the success and failure
paths"). So the unwind ITSELF invokes user destructors that read fields, and a
multi-field mutation that faulted partway leaves invariant-linked state inconsistent
for a `drop(!self)` to read. The honest safety claim is therefore: **(i)** the
overflowing scalar is **never committed** to observable user state (the helper
traps before returning, `runtime_checked_arith.c:8`; the inline emitter writes an
SSA temp then `exit(1)`s before any `IStore`, `c_lir/mod.rs:2438` — equivalent
under today's abort, and the value-commit question is what Q9 must settle for the
proposed longjmp); **(ii)** **no application code past the
boundary** reads the unit's corrupted outputs; **(iii)** the unwind is
**memory-safe** — no leak/double-free — *if* Q9 is solved. It is **not** "no
destructor observes inconsistent state." This is exactly the exposure Rust's
`panic=unwind` carries (Drop runs during unwind, can see partial state; mitigated by
exception-safety discipline / `Mutex` poisoning) — a known, bounded class, not a
novel hole. **Q9 is widened to cover value-observation by user `drop()` across the
unwind, not just leak/double-free.**

## 4. The two kinds, side by side

| | **Contract error** | **Fault** |
|---|---|---|
| Examples | parse, I/O, narrowing conversion (`byte(x)`) | overflow, bounds, div0, OOM |
| In the typed error channel? | yes | yes — flagged `fault` |
| Handling | **mandatory** (handle, or it appears in your boundary-declared error type) | **optional** — auto-propagate to a boundary |
| On the API / compat surface? | **yes** (curated, small) | **no** (excluded → never a silent breaking change) |
| Default if uncaught | compile error (must be handled/declared) | **abort** (with a diagnostic) |
| Catch site | local | task / request / supervisor boundary, **catch by type** (`catch Overflow`) |
| Likely codegen | Zig-`!T`-style value-union return | unwind / abort path (see §6) |

> **Phase-1 clarification (shipped, §11):** the table describes the BROAD/eventual model.
> What ships in Phase 1 narrows the fault column to a closed `Fault` enum (Overflow,
> DivByZero, Bounds) that is **out of every function's signature** (a plain `int sum(...)`
> stays `int`) and is recovered **locally and lexically** via `(expr) catch Fault.X` at the
> faulting op — NOT (yet) at a coarse boundary. The "Catch site: boundary" row is the
> Phase-2 deep/boundary catch and is unchanged. Faults still panic by default if uncaught.

## 5. Inference + boundary annotation (the make-or-break detail)

"Both channels typed, like `throws` today" has a fork the owner must see:

- **Declared on every function** = **Java checked exceptions** — `throws X`
  boilerplate up every call chain, the canonical *failure* of this design. **No.**
- **Inferred** (Zig `!T`, Koka) — the compiler computes the error type from the
  body; you don't write it; `Never` where empty. **This is the only viable form.**

Inference's one real tax: a public function's error type is determined by its body
*transitively* — add a fallible call three layers down and the API's contract error
type silently changes. **Fix (ML signatures / Koka / disciplined Zig): infer
internally, REQUIRE annotation at public/module boundaries.** So "most functions
error `Never` implicitly" holds for **private** functions; **public** ones declare
their contract error type — which is exactly where you *want* it pinned. (Faults,
being off the compat surface, are *not* part of the boundary annotation — that is
what stops them from poisoning every public signature.)

## 6. Mechanics & the "fast" leg

The two kinds have opposite access profiles, so they should lower differently:

- **Contract errors** — frequent-enough-to-handle, locally caught → a **value-union
  return** (Zig `!T`): explicit, cheap, threaded through the value channel. Catching
  is a branch you wrote on purpose. (In Gorget today this leg ALREADY exists — it is
  the existing `Result`-tagged-enum return, not a new shape.)
- **Faults** — rare to actually fire, recovered only at coarse boundaries → an
  **unwind/abort path**. The common (no-fault) path does **not** thread a fault
  value through every call, so it stays branch-free of error-union plumbing; the
  unwind only runs when a fault actually fires.

**⚠ Reality check (scout-verified 2026-06-20): the fault-unwind leg is GREENFIELD,
not reuse.** The *contract* leg already matches the runtime — `throws` lowers to a
`Result`-value return + the `Result→T` auto-prop hook
(`src/ir/lowering/exprs/mod.rs:44-62`) + early `Error(val)` return
(`stmts/mod.rs:2380`). But production **panic = `exit(1)`**
(`src/backend/c/runtime/panic_normal.c:3-9`); overflow/bounds/div0 all hard-abort
(`src/lir/lower/calls.rs:82` `Overflow::Trap`; `runtime_array.c:31`; `c_lir/mod.rs:2476`). A
setjmp/longjmp substrate exists (`runtime_error.c`: `__gorget_jmp_stack`,
`GORGET_TRY`) but is **gated to test/`throws` mode and wired to neither panics nor
`Task`/`TaskGroup`** (no scheduler has a panic-catch path — verified across
`scheduler_{inline,pool,thread,single}_runtime.c`; a task panic reaches
`gorget_panic`→`exit(1)`, `panic_normal.c:6`). So delivering "recover a fault at a boundary" requires NEW
infrastructure: (a) make `gorget_panic` longjmp (not `exit(1)`) for fault-typed
panics; (b) install per-task setjmp frames in the inline *and* pool schedulers;
(c) run drop/cleanup unwinding across the longjmp (§9 Q9); (d) thread the fault
value out of `join`. Plausibly the single largest implementation item in the RFC —
§9 Q10 tracks it; do not let the one-liner above stand in for it.

**Caveat (honest):** the unwind lowering makes *propagation* cheap, but the
overflow *check* itself is still per-op and still inhibits auto-vectorization. So
the "fast" knob is orthogonal and must be decided separately: likely follow Rust —
**overflow-checks on in debug, defined wrapping in release** by default, with an
opt-in to checked-in-release. ⚠ This creates a type/runtime tension to resolve: if
the type says "can fault with Overflow" but release-mode wraps silently, the type
promises a fault the runtime won't deliver. Options: (a) faults are a *latent*
capability not a runtime guarantee (the type means "may fault if checks are on");
(b) checked-always for the types/scopes that opt in. **Open — decide in the scout.**

## 7. How it composes with the cast RFC (the payoff)

This model gives a *principled* answer to "why does `byte(x)` throw but `a + b`
doesn't, by default?" — the asymmetry is **kind**, not arbitrary:

- `byte(x)` validates (often external) data against a **contract** → **contract
  error**, typed, mandatory-handle. ✅ exactly the cast RFC's decision.
- `a + b` overflow is a **fault** (a bug or a wrong width) → **fault**, recoverable
  at a boundary, off the API surface, default-abort.

Both are *recoverable* and *typed*; they differ only in **kind**, and the kind is
read off the *intent* (contract validation vs computational fault), not bolted on
per-site. The owner's goal — "the user should be able to recover from overflow, not
just crash" — is met (catch the `Overflow` fault at the request boundary) without
making `throws` universal-and-meaningless.

## 8. Precedents (this is well-trodden; we're choosing, not inventing)

- **Zig** — `!T` with **inferred error sets** + `try` auto-propagation + `error{}`
  (= `Never`) is *almost exactly* the proposed shape. Decisive: Zig **excludes
  overflow/bounds from the error union** — they are panics (safety-checked), `+%`
  wraps, `@addWithOverflow` reports. The language closest to this model keeps faults
  out of the typed contract channel **on purpose**.
- **Koka** — inferred effect rows (`a -> e b`), `total` = empty, automatic
  propagation, handlers discharge. The academic backbone for "inferred, typed,
  auto-propagating."
- **Swift 6** — just added **typed throws** (`throws(MyError)`) and its own guidance
  is to use it **sparingly**, not universally. Same instinct: typed error channels
  are good, universal-typed is not the default.
- **Midori** (Joe Duffy, "The Error Model") — the canonical modern writeup: split
  **recoverable errors** (typed/checked, contract) from **bugs/abandonment**
  (fail-fast, isolated/recovered at process granularity). Explicitly argued AGAINST
  turning bugs into recoverable typed contract errors. = our contract/fault split.
- **Erlang/BEAM** — "let it crash" + supervisors: ambient failure, recovery at a
  *coarse* boundary. The gold standard for "a fault in one request doesn't kill the
  server" = our fault-caught-at-boundary.
- **Java checked exceptions** — the cautionary tale for *declared*-universal throws
  (boilerplate; everyone escapes to unchecked). Motivates **inference** (§5).
- **Rust** — overflow-checks debug-only, defined wrapping in release; `checked_*` /
  `wrapping_*` / `saturating_*` for explicit intent. Motivates the §6 "fast" knob.

**⚠ Honest tension (review pass 1):** the two strongest precedents here — **Zig and
Midori — have UNTYPED faults** (Zig's overflow is a panic, not an `error{}` member;
Midori's bugs are untyped abandonment). They robustly support *"faults are not
contract errors"* (the split, which this RFC has) but they lean *away* from *"faults
are typed and catchable-by-type"* (which this RFC prefers, §9 Q14). Do not cite them
as if they back typed faults — they are evidence for the untyped-panic alternative
the RFC must argue against.

## 9. Open questions (for the scout/brief, before any implementation)

1. **Fault catch syntax & scope — TWO regimes (review pass 1 + phasing §9.1, §3.1).**
   ⚠ **Phase 1 = inline/LOCAL catch (safe, permitted):** `(a*b) catch Overflow: …`
   recovers a fault from the wrapped expression's own ops — safe because the checked op
   **branches before the store** (§11.2/§3.1 carve-out), so the corrupted value is never
   observed. **Phase 2 = DEEP catch** of a fault propagated from a CALLED FUNCTION —
   that one is catchable ONLY at a declared coarse boundary that discards-and-unwinds the
   unit of work (a deep fault caught inline mid-computation, keeping the corrupted value,
   IS the footgun, `book/10-errors.md:13-14`). So "boundary-only" is a **Phase-2 deep**
   invariant, NOT a blanket one. Still open: the *spelling* and how it relates to the
   existing postfix `catch` (which is for *contract* errors).
2. **The "fast" tension** (§6) — debug-checked/release-wrapping vs checked-always;
   the type-vs-runtime-promise reconciliation. This is now RESOLVED in favor of
   checked-always: plain `+`/`-`/`*` always check (the global wrap mode was
   retired), so `catch Overflow` is always meaningful and the type never lies.
   Explicit per-op wrapping (`+%`/`-%`/`*%`) never faults and never enters a
   fault contract.
3. **How `fault` is declared** on an error type — a marker on the enum/type decl
   (typed metadata, never name-matching), so the classification is read via an
   accessor, not a name list.
4. **`Never` spelling** and how "this function is total" is expressed/checked
   (a `total` qualifier that demands an empty contract row?). ⚠ **`Never` is NOT
   renamed to `Fault` (owner Q 2026-06-21):** `Never` is the **bottom/uninhabited
   type** (the type of `return`/`throw`/diverging exprs, `src/semantic/types.rs:68`) —
   "no value, ever." `Fault` is a **closed enum of inhabited variants** (Overflow,
   Bounds, …) — "one of these specific faults." Opposite kinds of thing; in the
   channel model `Never` is the EMPTY error set (= *cannot* fail), the OPPOSITE of
   "can throw a fault." Keep them distinct.
5. **Boundary-annotation rules** — which definitions must declare their contract
   error type (all public? all crate-public?); how inference flows across modules.
6. **Interaction with `Result[T,E]`** — is the contract error channel *the same
   thing* as today's `Result`/`throws`, or a generalization? (Almost certainly:
   today's explicit `throws E` becomes the *declared* form of the inferred contract
   channel; `Result[T,E]` stays the reified value.) Reconcile, don't duplicate.
7. **Which runtime conditions are faults vs contract — AND reconcile the existing
   rule.** ⚠ The docs ALREADY answer this, the *opposite* way: `language-design.md:1312`
   ("caller can prevent → panic") + the §6 panic list (`:1297-1303`) classify overflow/bounds/div0/
   unwrap/assert/OOM as PANIC. So this is not "enumerate the set" — it's "we are
   *reversing* a documented, rule-backed decision; justify it and rewrite
   `language-design.md` §2.2+§6 and `book/10-errors.md`." See **§0.5** (the #1 item).
8. **Migration / blast radius — the GATING item, not the last bullet.** Touches the
   whole language. The self-host is **95 arithmetic-dense `.gg` files** +
   `bootstrap_fixed_point`, which must re-converge through any fault-lowering — the
   load-bearing validation and the top risk. The new error-channel/inference pass
   lands in BOTH Rust gg and the self-host's own typechecker. Staged plan + guards
   (`tests/lints.rs`) + the self-host parity story come BEFORE any code.
9. **Drop/CoW correctness across a fault unwind (scout; WIDENED review pass 2).**
   Gorget's ownership model (drop insertion, `MoveZero`, `on error` cleanup, CoW)
   assumes normal return or `exit(1)`. A longjmp-based fault unwind that *continues
   execution* must run drops correctly for every live owned value across the unwound
   frames; the cleanup-stack is test-only today (`panic_test.c`). **Beyond
   memory-correctness (no leak/double-free), this must ALSO cover value-observation:**
   user `Drop` runs on the failure path (`language-reference.md:4743`), so a
   `drop(!self)` can read invariant-linked state left inconsistent by the
   partially-completed faulting unit (§3.1). The design must say what a destructor may
   assume across a fault unwind (Rust's answer: exception-safety + poisoning). Collides
   with the CLAUDE.md ownership invariants — needs a design before any code.
10. **The fault-unwind infrastructure cost (scout).** See §6 reality-check:
    production panic is `exit(1)`; fault-recovery needs new per-task setjmp/longjmp +
    drop-unwind in BOTH schedulers + fault-value threading out of `join`. Likely the
    largest single implementation item — size it explicitly, don't hand-wave it.
11. **Compile-time / `meta` / const-eval overflow (review pass 1 — a real divergence).**
    Const-eval arithmetic currently **wraps silently** (`src/semantic/meta.rs:1278-1280`,
    `wrapping_add`/`sub`/`mul`). If runtime overflow becomes a recoverable fault, the
    RFC creates a THREE-way split (compile-time wraps / debug-runtime faults /
    release-runtime wraps per §6) with no story — and a fault cannot "recover at a
    boundary" at compile time (there is no boundary). Decide: does `meta` overflow
    become a compile error, wrap, or stay as-is?
12. **`rethrow` / `on error` under the inferred channel (review pass 1).** Both are
    compile-errors today *unless the fn is declared `throws`* (`language-reference.md:2439,2518`).
    With the contract channel INFERRED (not declared), that rule must be redefined in
    terms of the inferred channel. And: do `on error` cleanup blocks (errdefer-shaped)
    run on a **fault** unwind, or only a **contract**-error unwind? Collides with Q9
    (drop/CoW correctness) — answer them together.
13. **`Never`-default inference across generics/trait methods (review pass 1).** A
    generic fn calling a type-parameter's method whose fault/contract set is unknown
    pre-monomorphization has an unknown inferred channel → forces an effect-carrying
    BOUND. This is the *same* "Seam B" the cast RFC hit (`cast-via-construction.md` §7.4),
    inherited for the WHOLE error channel. Spec the bound.
14. **Fault representation & catch model — RESOLVED 2026-06-21 (the "typed vs
    untyped" framing was a CONFLATION).** "Typed/untyped" was overloading three
    distinct axes: **(A)** value representation — structured enum vs `String`;
    **(B)** whether the fault rides the function's *static type* (the in-signature
    `Result` row); **(C)** whether the boundary catch is *statically exhaustive* vs a
    *dynamic match*. The reviewers' "untyped panic" meant **B = no** (not in the
    signature), NOT **A = String**. **Owner decision:**
    - **(A) Structured `Fault` enum, NEVER `String`** — `Fault.Overflow`,
      `Fault.Bounds`, `Fault.DivByZero`, `Fault.OutOfMemory`, … a closed enum of all
      non-panicking faults the user matches on. (This is what "typed" meant.)
    - **(B) OUT-OF-BAND — the `Fault` does NOT enter any function's `Result`/static
      type.** Signatures stay clean; faults ride the separate ambient fault channel
      (the unwind path, §6), not `Result`. This is what avoids the §3 ubiquity
      pollution (a fault in `Result` would put `Overflow` on ~every signature + on
      the API surface — the thing §3/§4 rule out).
    - **(C) DYNAMIC match at the boundary**, not static per-boundary exhaustiveness:
      `catch fault: match fault: case Fault.Overflow: …`. The Rust `catch_unwind` /
      Go `recover` / Erlang model. No fault-set inference up the call graph (that
      would re-introduce a milder ubiquity problem for a thin payoff — a catch-all
      "fail the unit of work" is the normal handler anyway).

    **Retire "untyped" as a misnomer:** BOTH error paths are typed (both carry
    structured types); they differ only in *where* the type lives — `Result[T,E]` is
    **in the signature** (contract), the `Fault` enum is **ambient/out-of-signature**
    (faults). ⚠ **Follow-up — NOT happening (owner 2026-06-21):** an earlier note here
    proposed reframing §1/§4's "ONE error channel, two kinds" into "two channels." The
    owner KEEPS the "one error channel, two kinds" framing; the reframe is dropped. The
    out-of-signature distinction above is captured by the §4 Phase-1 clarification, not a
    §1/§4 restructure.
15. **FFI / `extern`-boundary fault unwind (review pass 2).** A longjmp-based fault
    unwind (§6) that jumps over a foreign C frame skips C-side cleanup and is UB on
    many ABIs. The doc has zero treatment of a fault crossing an `extern` boundary.
    For a longjmp design this is load-bearing: either faults cannot unwind across FFI
    (they abort at the boundary) or the boundary installs a catch. Spec it.
16. **`main` / single-threaded top-level fault boundary (review pass 2).** §4 says
    uncaught faults "abort"; §3.1 says a Phase-2 DEEP fault is catchable only at a coarse boundary.
    But is `main` (or a single-threaded CLI with no Task/request) itself such a
    boundary? If NOT, the most basic program shape has no boundary → every fault
    aborts → "recoverable overflow," the owner's GOAL, is unreachable without a
    Task/server. Define the default top-level boundary. (The existing `main() throws int`
    → exit-code path, `language-reference.md:2480`, is *contract*-channel, orthogonal.)

## 9.1 UNDER EXPLORATION (owner 2026-06-21): unified `Error` typing + the typing/propagation split

The owner asked whether a **single synchronous channel** could carry both contract
errors and faults, with **all errors extending a base `Error` that includes the
faults**, **uncaught faults panicking (as today)**, so a match is **always
exhaustive**. Grounding (verified): Gorget already has an **`Error` trait**
(`language-reference.md:2766`, the Rust `std::error::Error` model — `IoError`/
`ParseError` implement it), trait `extends` for **supertraits only**, **NO class
subtyping** (`language-design.md:1926/1958`), and **NO open enums** (enums are
closed; matches are enforced-exhaustive, `typecheck.rs:3664`). So "extend a base
`Error`" = **implement the `Error` trait** (composition), not inherit a base enum
with variants.

**The key realization: TYPING and PROPAGATION are orthogonal, and the proposal only
settles TYPING.**
- **TYPING (adopt — cheap, idiomatic):** make the closed `Fault` enum `equip Error`,
  like the stdlib errors. Then one boundary `catch e` (`e: the `Error` trait`) handles BOTH a
  contract error and a fault, uniformly. This REFINES Q14's "separate `Fault` type"
  into "`Fault` is an `Error` impl." "Uncaught faults panic" keeps a plain `int
  sum(...)` non-`Result` (no ubiquity), overflow panic-by-default (today's safety +
  fast path), recovery opt-in — keep this. "Always exhaustive" = the closed `Fault`
  enum is exhaustively matchable; the contract side is open (trait); unhandled faults
  **fall through to panic** (exhaustiveness-via-implicit-panic-default — a small
  `non_exhaustive`-style match rule). One literal enum holding faults + user variants
  with static exhaustiveness would need **open enums** (a new feature, a lean toward
  Java's `Throwable`).
- **PROPAGATION (the real cost — UNCHANGED by the typing):** how a fault from a DEEP
  call reaches that `catch`. Trilemma — pick two of {deep-catch, no-ubiquity,
  no-unwind}:
  - **local-only catch** (`(a*b) catch Overflow: …` at the op) → truly in-band, no
    ubiquity, no unwind — but cannot catch a fault several frames down.
  - **in-band deep** (fault rides `Result` up) → every arithmetic fn becomes `Result`
    = ubiquity (the §3 problem).
  - **out-of-band deep** (fault unwinds to the handler) → unwinding (the §6 greenfield
    infra) — what Java/C#/Python actually do under their unified `try/catch` surface.

  So "single synchronous channel" stays synchronous only for **local** faults; the
  **server-keeps-serving** use case (catch an overflow deep in a handler) forces
  unwinding regardless of how errors are typed. Java's unified `try/catch` IS
  stack-unwinding underneath.

**✅ RESOLVED 2026-06-21 (owner): BOTH, PHASED** — ship local-only first, add deep
later as a separate funded effort.

**PHASE 1 — local catch (cheap; NO unwinding, NO ubiquity).**
- `Fault` enum `equip Error` (unified typing — one `catch` shape for faults +
  contract errors).
- Faults **panic by default** (today's behavior, unchanged — fast path preserved,
  signatures stay non-`Result`).
- **Local catch only:** `int r = (a * b) catch Overflow: saturate()` lowers the
  wrapped arithmetic to a **checked op that branches to the handler** — the trap
  branches BEFORE the store, so no corrupted value ever materializes. In-band,
  synchronous, no unwind.
- Exhaustiveness via **implicit-panic-default** (a `non_exhaustive`-style rule: a
  fault match may omit variants → they panic).
- ⭐ **Phase 1 sidesteps EVERY unwind-dependent blocker:** B2 (greenfield unwind),
  Q9 (drop-across-unwind), Q15 (FFI unwind), Q16 (`main` boundary), and the §3.1
  partial-state/Drop-observation concern are **all Phase 2** — none apply when the
  catch is lexical and the checked op branches before committing. Phase 1's new
  machinery: the `Error`-impl on `Fault`; **a NEW LIR checked-op-with-handler-branch
  shape in BOTH backends** (the single largest item — there is no existing trap to
  re-point; see §11.2); and the panic-default match rule.

**PHASE 2 — deep / boundary catch (separate funded effort).**
- Catch a fault from a deep call at a task/request boundary (server-keeps-serving).
- **⭐ RESOLVED 2026-06-22 (OWNER-DECIDED: Option A + hybrid — supersedes "needs unwinding").**
  A scout (verified against source) + a fresh A-vs-B comparison + 3 review passes established that
  Phase 2 does **NOT** need the §6 greenfield unwind infra: Gorget ALREADY ships a deep, drop-correct
  **BY-VALUE** error channel (the `throws`/`Result` path running `emit_early_exit_drops` at each
  early-exit frame, `src/ir/lowering/stmts/mod.rs:2373-2398` + the auto-prop hook
  `exprs/mod.rs:87`/`:2922`). Phase-2 fault propagation REUSES it via a **hidden out-of-band return
  slot** (faults stay OFF signatures — the §3 ubiquity guarantee holds — NOT stack unwinding), so
  **Q9/Q15/Q16/B2 dissolve**. The **hybrid**: a unified `catch (e): match e` boundary handler over
  `Error` (catching BOTH contract errors AND faults) via `Fault equip Error`, with NO `throws Fault`
  on any signature — this delivers the owner's uniformity goal without re-importing the `throws`-spine
  ubiquity. (Option B = uniform `throws` for faults was REJECTED: measured self-host ubiquity floods the
  protected call spine; Swift — the assumed precedent — actually TRAPS on overflow, a precedent FOR A.)
  Design: `error-model-phase2-design.md`; A-vs-B rationale: `error-model-phase2-A-vs-B.md`.
  **Open pre-impl gate:** MEASURE the hidden-slot hot-path threading cost on the self-host self-compile
  before shipping Increment 2.1.
- Strictly **additive** over Phase 1 — same `Fault`/`Error` typing, same `catch` surface; only the
  PROPAGATION reach extends from lexical to deep (BY-VALUE, not unwind). First increment = single-call-
  deep (2.1, `error-model-phase2-design.md` §4).

(The §1/§4 "one channel / two kinds" framing is KEPT — the once-anticipated "two channels" reframe is not happening, owner 2026-06-21. Phase 1 added a §4 Phase-1 clarification only.)

## 10. Bottom line

The owner's "every function has two typed channels" is **real and good** — it's the
Zig/Koka shape, and as the **value + (inferred, typed) error** channel it unifies
throwing and non-throwing functions into one shape (`Never` = the dull case). The
one thing that does *not* work, even fully typed, is putting **default arithmetic
overflow into the contract error type** — that's an impossibility (recoverable-
default + informative-row + universal: pick two), and Zig, the language with this
exact channel, agrees by excluding faults from it. The resolution keeps the owner's
uniformity **and** recoverable overflow: **one typed error channel, two kinds of
error — contract (impose/curate, on the API surface) and fault (auto-propagate,
recover-at-boundary, off the API surface).** Conversion-overflow is a contract
error (cast RFC, decided); arithmetic-overflow is a fault (recoverable at a
boundary). Same model, both worked examples.

## 11. PHASE 1 SPEC (the review target — owner decisions of 2026-06-21)

Phase 1 is the small, shippable, **unwind-free** increment. This section is the
consolidated spec the **Phase-1 review** evaluates; Phase 2 (deep/boundary catch)
is OUT of scope and gets its own review cycle after Phase 1 lands. Rationale &
decisions live in §0.5 (additive reversal), §3 (impossibility), Q14 (fault
representation), §9.1 (phasing). **(The `(expr) catch Overflow: …` spelling in the
examples below is ILLUSTRATIVE — the exact fault-catch syntax is open, §11.5.)**

### 11.0 Scope boundary — what Phase 1 does NOT touch (owner Qs 2026-06-21)
- **Phase 1 does NOT implement the universal inferred error channel** of §2/§5/§10.
  Today's *contract* model — explicit `throws E` = sugar for `Result[T,E]`
  (`book/09-option-result.md:328`), declared, propagated via `?`/`rethrow`/`on error`
  — is **UNCHANGED**. **No non-throwing function implicitly becomes `throws Never`;
  no signature gains an error channel.** That universal/inferred-channel direction is
  a separate, much larger effort (its own future phase), explicitly off the Phase-1/2
  path.
- **Faults are out-of-SIGNATURE (not in any function's type) — and in Phase 1 they are
  handled by LOCAL in-band control flow, NOT unwinding.** A plain `int sum(...)` stays
  `int`; overflow panics by default; a *local* catch (`(a*b) catch Overflow: …`) turns
  the faulting op's overflow into a **branch to a handler block in the SAME function**
  (an inline "if-overflow-goto-handler", §11.2) — the fault never crosses a call, so
  nothing needs a signature and nothing unwinds. ⚠ Distinguish the two axes: faults are
  out-of-*signature* in BOTH phases; out-of-*band* (cross-call propagation via
  unwinding) is **Phase 2 only**. Phase-1 local catch covers only the faultable ops in
  the wrapped expression's OWN basic blocks (not ops inside a function it CALLS — those
  panic → Phase 2). This is the "no ubiquity" guarantee (§3, §9.1).
- **`Never` is untouched and unrelated to `Fault`** (the bottom type vs the fault
  enum — §9 Q4).

### 11.1 Scope — what Phase 1 delivers
1. A **closed `Fault` enum**, scoped (review pass 1) to faults that are
   **inline-checkable at the operation site** — so the §11.2 branch-to-handler works
   without unwinding. The §11.2 mechanism is NOT uniform across faults:
   - ✅ **`Fault.Overflow`, `Fault.DivByZero`** — checked INLINE today
     (`c_lir/mod.rs:2438` overflow, `:2476` div0). The DATA path is solid; ⚠ but the
     check is NOT a re-pointable LIR branch (it's a C/LLVM-emit-time `if(...)exit(1)`) —
     Phase 1 needs the new LIR checked-op-with-handler-branch shape, §11.2.
   - ⚠ **`Fault.Bounds`** — checked INSIDE the runtime fn `gorget_array_get`
     (`runtime_array.c:31`), reached via `CallExtern` (deep). BUT a non-panicking
     **`gorget_array_safe_get` already exists** (`runtime_array.c:39`); a catch-scoped
     index can lower to it + an **inline NULL-check branch** to the handler — still
     unwind-free. **Brief decides:** include in Phase 1 via the safe-variant rewrite,
     or defer to a Phase 1.5. (Note: `gorget_array_safe_get` takes a SIGNED index and
     treats `index<0` as OOB, vs `gorget_array_get`'s `size_t` — so a negative index
     becomes a catchable `Bounds` inside a catch; intended, but document the
     in-catch/out-of-catch difference.)
   - ❌ **`Fault.OutOfMemory`** — scattered `exit(1)` deep in allocators
     (`runtime_string_extended.c:348` et al.), no inline check, no safe variant →
     **Phase 2** (needs unwind or an allocator rework). Do NOT promise it in Phase 1.

   So the mechanism cleanly covers Overflow+DivByZero now, Bounds via a known
   safe-variant swap, OOM deferred. (`UnwrapNone`/`Assert` candidates = Q7, brief.)
2. **`Fault equip Error`** — `Fault` implements the EXISTING `Error` trait
   (`language-reference.md:2766`), so faults compose with contract errors under one
   supertype. ⚠ (the **unified `Error` surface** — matching a fault AND a contract
   error in ONE handler — is the **Phase-2 boundary goal**; Phase-1 *local* catch binds
   a concrete `Fault` value, see item 5.) ⚠ **(review pass 1)** `Error` **extends `Displayable &
   Debuggable`** (`language-reference.md:2766`; `Displayable` `:2768`), so the compiler-internal `Fault`
   must synthesize **THREE** methods, not one: `String display(self)`,
   `String debug(self)` (the supertraits) and `Option[String] source(&self)` (`Error`).
   `@derive(Debuggable)` + a hand-written `display` covers it. No new base type, no
   subtyping, no open enums.
3. **Panic-by-default, unchanged.** Outside a fault `catch`, overflow/bounds/div0
   panic exactly as today (`exit(1)`). A plain `int sum(...)` stays `int` — NO
   signature change, NO `Result` ubiquity, the fast path is untouched.
4. **LOCAL (lexical) catch.** `int r = (a * b) catch Overflow: saturate()` recovers a
   fault from the faultable ops **syntactically within** the wrapped expression. It
   does NOT catch faults that occur inside FUNCTIONS CALLED by the expression — those
   are deep → panic → Phase 2.
5. **Exhaustiveness via implicit-panic-default — over the closed `Fault` enum (review
   pass 2).** The Phase-1 local-catch scrutinee binds a **concrete `Fault` value** (a
   closed enum), NOT an `Error` trait object — so the panic-default rule is coherent:
   a fault match handles the variants it names; unnamed `Fault` variants fall through to
   **panic** (a `non_exhaustive`-style rule keyed on the `Fault` enum at
   `typecheck.rs:3640-3666`, leaving every OTHER enum strictly exhaustive). The unified
   `Error` surface (item 2) is Phase 2 — Phase-1's `Fault equip Error` only makes
   faults composable later; the Phase-1 catch itself matches the closed `Fault`.

### 11.2 Lowering — no unwinding
- ⭐ **The central Phase-1 implementation item (review pass 2): a NEW LIR
  "checked-faultable-op-with-handler-branch" shape, in BOTH backends — NOT a re-point
  of existing machinery.** Verified: every faultable check today TERMINATES THE PROCESS
  inline at C/LLVM emit time (`if(__builtin_add_overflow(...))exit(1)`,
  `c_lir/mod.rs:2438`; LLVM `emit_overflow_check`, `llvm/mod.rs:3324`; bounds `:3063`;
  div0 `:2476`), synthesized AFTER drop-insertion/elaboration run. **There is no
  LIR-level branch to re-point.** So Phase 1 must ADD a checked faultable op whose
  overflow/null outcome is a real `Inst::Branch` to the handler bb (e.g. an
  `Overflow::Branch(bb)` variant; the Bounds safe-get NULL-branch), emitted from GIR/LIR
  CFG — implemented in the **C AND LLVM emitters** (backends-at-parity). This is the
  single largest Phase-1 item, bigger than the `Fault equip Error` impl and the
  panic-default rule combined; the brief must size it as such, not as "branch the flag's
  trap elsewhere." (Emit-level asymmetry, final review: the **LLVM** emitter ALREADY
  produces branch-structured output — intrinsic→`%flag`→`br`→trap/ok-block, value
  pre-committed in an SSA temp — so its side is structurally closer; the **C** emitter
  is a flat `if(...)exit(1)`. Both nonetheless derive the branch from the SHARED LIR
  shape per the CFG rule above, so neither is special-cased.)
- Because that branch exists at **LIR level BEFORE the drop passes run**, the existing
  drop-insertion (`drops.rs`)/elaboration (`drop_elab.rs`) clean up live owned
  temporaries on the handler path (`(bigStruct.compute() * k) catch …`) — the template
  is `lower_catch_expr`'s `err_bb`/`merge_bb` (`exprs/mod.rs:3338-3485`). Model it as
  CFG, NEVER a C-emit `goto` on the inline trap, or temporaries leak. This is what keeps
  Phase 1 drop-correct without unwinding.
- The check branches BEFORE the store → no corrupted value materializes (verified: the
  wrapped result lives only in a dead SSA temp the handler never reads; **for Bounds the
  inline `if(p==NULL)` branch MUST precede any deref of `p`**). Pure local control flow —
  NO setjmp/longjmp, NO unwinding, NO drop-across-unwind.
- **Handler-bb entry constructs the `Fault` value (review pass 3).** For the binding
  form (`catch f: match f`), the handler block must, at entry, **materialize the
  corresponding `Fault` variant** — `Fault.Overflow()` / `Fault.DivByZero()` /
  `Fault.Bounds()`, the discriminant encoding WHICH op faulted — and bind it to `f`.
  (The pattern form `catch Overflow:` needs no constructed value.) Each faulting op's
  branch targets a handler-entry that knows its own variant. Mechanically simple but
  load-bearing — spec it so it isn't discovered mid-implementation.
- ✅ **Overflow-mode interaction — MOOT (the global wrap mode was retired).** There is
  no longer a build-wide "wrap" mode that could defeat a local `catch`: plain `+`/`-`/`*`
  always check (`calls.rs` emits `Overflow::Trap` unconditionally). A `catch`-scoped op
  lowers through `FaultableBinOp`, which force-checks structurally regardless — no
  per-expr "force-checked" plumbing is needed. The only way to opt into wrapping is the
  per-operator `+%`/`-%`/`*%` forms, which never fault and are never wrapped in a fault
  contract.
- ✅ **A fault-catch cannot swallow a contract error (review pass 2):** the auto-prop
  hook fires only on `Call`/`MethodCall` operands returning `Result` (`exprs/mod.rs:42-70`);
  a bare `a*b` is neither, so it never fires inside a fault-catch. The existing
  `suppress_auto_prop` machinery that contract `catch` already uses (`exprs/mod.rs:3375`,
  `typecheck.rs:3039`) is the reusable template — the fault-catch lowers over the raw
  inner the same way.

### 11.3 Explicitly OUT of Phase 1 (→ Phase 2)
Deep/boundary catch (a fault from a called function); the §6 greenfield unwind infra
(B2/Q10); drop/CoW-across-unwind (Q9); FFI-boundary unwind (Q15); the `main`/task
top-level boundary (Q16); the §3.1 partial-state/`Drop`-observation argument (MOOT in
Phase 1 — the checked op branches before any partial mutation commits). None of these
are touched by Phase 1.

### 11.4 Doc + framing changes that land WITH Phase 1
Phase 1 updates ALL the documentation surfaces it touches — enumerate the full list
in the brief; the known set (owner Q 2026-06-21 — "does the plan update all docs?"):
- **`docs/language-design.md` §2.2 (overflow) + §6 (Panic-vs-Result rule `:1312`):**
  ADD "overflow/bounds/div0 panic by default AND are locally catchable" (the additive
  change, §0.5); restate the rule to cover opt-in recovery. Add `Numeric`/`Fault` to
  the trait/type registry if applicable.
- **`docs/book/10-errors.md`:** a user-facing section on faults + local catch (the
  `(a*b) catch Overflow: …` idiom) alongside the existing contract-error chapter;
  reconcile the "continuing with corrupted state is worse than stopping" passage
  (`:13-14`) with the now-recoverable-locally story.
- **`docs/language-reference.md`:** the new fault-catch **grammar** (catching a fault
  off a non-throwing expr — distinct from the `Result` `catch`, §11.5); a **`Fault`
  enum** reference + its variants; note `Fault` implements `Error` (`:2766`).
- **`error-model.md` itself:** the once-anticipated §1/§4 "two channels" reframe is
  NOT happening (owner 2026-06-21) — the "one error channel, two kinds" framing is KEPT.
  Phase 1 adds only a §4 Phase-1 clarification (faults are out-of-signature, recovered
  locally via `catch Fault.X`); §1 and the §4 table are unchanged.
- **Examples** across book/reference that assume overflow is always fatal.

### 11.5 Phase-1 open questions (resolve in the brief)
- **Exact `Fault` membership** (Q7, the Phase-1 subset).
- **Lexical reach, precisely (review pass 1)** — define as **"faultable ops emitted
  DIRECTLY into the wrapped expression's own basic blocks, not through any
  `Call`/`CallExtern`."** Clean for operator trees (`a*b + c/d`); the sharp edge is an
  inline closure — `(xs.map((int x): x*2)) catch Overflow` — where `x*2` is lexically
  visible but invoked via the `.map` CALL, so it is **deep (Phase 2), NOT caught**.
  Defensible (call-boundary = the Phase-2 line) but a teachability footgun — the brief
  must adopt the basic-block definition and document it crisply, not leave it as prose.
- **`catch`-by-`Fault` syntax — a NEW form, not a typecheck relaxation (review pass 2).**
  Today's `catch` is structurally welded to `Result[T,E]`: the parser hard-expects
  `catch (name):` (`expr.rs:1072`), the AST `Catch` node carries only `error_binding:
  Spanned<String>` (`ast.rs:585`), and typecheck extracts `err_ty` only when the inner is a 2-arg
  `Result` (`typecheck.rs:3047`). A fault-catch off a bare `a*b` (neither `throws` nor
  `Result`) needs a **new AST node/variant + grammar + a DISTINCT typecheck path**, kept
  separate so the existing contract-error `catch (name):` path is UNTOUCHED. `(expr)
  catch Overflow: …` vs `(expr) catch f: match f`.
- **`catch` precedence on a bare faultable expr (review pass 2).** `Catch` is an infix op
  (`expr.rs:771`, `InfixBP`). Decide the binding of `a * b catch Overflow: …` (does
  `catch` wrap the whole `a*b` or just `b`?) and whether an un-parenthesized faultable
  expr is even accepted — it DETERMINES which ops are "in the wrapped expression's own
  basic blocks" (the lexical-reach definition above). The examples always parenthesize,
  sidestepping it; the brief must settle it.
- **`meta`/const-eval** — `meta` arithmetic wraps silently today (`meta.rs:1278-1280`).
  Cleanest Phase-1 answer: a `catch` in a `meta` context is a **no-op / compile error**
  (const-eval has no runtime fault to catch); decide + state it, don't leave it silent.
- **Self-host parity** — the lowering change must keep `self_host_*` /
  `bootstrap_fixed_point` green; verify no fixture relies on the un-catchable panic
  shape. (The global wrap mode was retired, so there is only the default checked build to
  exercise — `FaultableBinOp` force-checks structurally, no per-expr override path.)

### 11.6 New fixtures to LOCK IN Phase-1 behavior (owner Q 2026-06-21 — required, not optional)
Per CLAUDE.md (executable guards > prose; negative fixtures; the gate battery), Phase 1
ships with NEW fixtures, all deterministic-stdout:
- **Positive (recovery works):** `(a*b) catch Overflow: fallback` yields the fallback;
  div0 catch; Bounds catch (if Phase 1 includes it); a `catch f: match f` binding reads
  the right `Fault.Overflow()`/`Fault.DivByZero()` variant; nested/compound expr
  (`a*b + c/d`) catches the right op.
- **Panic-default preserved:** an UNCAUGHT overflow/div0/bounds still panics (`exit(1)`)
  exactly as today — a fixture asserting the un-catchable shape is unchanged outside a
  catch.
- **Always-checked:** plain `+`/`-`/`*` always check (there is no global wrap mode), so an
  op inside `catch Overflow` reliably fires the handler. Only `+%`/`-%`/`*%` wrap, and they
  never fault.
- **Negative / exhaustiveness:** an unhandled `Fault` variant falls through to panic
  (not a compile error — the `non_exhaustive`-style rule); fault-`catch` on a
  non-throwing expr typechecks; contract-error `catch` is UNCHANGED (a regression guard
  that the new form didn't perturb the `Result` path).
- **Lock-in nets:** runtime-snapshot fixtures (`tests/fixtures/runtime_snapshots/`) for
  the above; `self_host_runtime`/`bootstrap_fixed_point` stay green; both backends
  (default + `GG_BACKEND=llvm`) at parity. Consider a `tests/lints.rs` ratchet for the
  new LIR fault-op arm-count.

### 11.7 Sequencing for the both-backends implementation (final review 2026-06-21)
- **The new LIR checked-op-with-handler-branch shape must land FIRST** (in the shared
  LIR + at least one emitter) — the AST/grammar/typecheck work produces no runnable
  output until a backend can emit the branch. Stage: **shared LIR shape → one backend →
  AST/grammar/typecheck → second backend → fixtures.**
- **No override needed — plain ops are already always-checked.** With the global wrap mode
  retired, `lower_binop` emits `Overflow::Trap` for plain `+`/`-`/`*` unconditionally, so a
  `catch`-scoped op is checked by construction (via `FaultableBinOp`); there is no
  module-global to defeat and no per-expr "force-checked" signal to thread.
- Both backends are symmetric at the LIR level (neither has a re-pointable branch today);
  drive the branch from the shared LIR per §11.2 so neither emitter is special-cased.

### 11.8 SELF-HOST parity — scout-verified 2026-06-21 (Rust-first, self-host fast-follow)
The self-host (`tests/fixtures/self_host_*/`) is in good shape; **the key prerequisite
is already satisfied** and **Rust-gg Phase-1 alone regresses NO self-host gate.**
- **✅ Already present (no prerequisite work):** the self-host **already emits CHECKED
  arithmetic** by default — `a+b` → `__builtin_add_overflow(...) exit(1)`
  (`lir_codegen.gg:3577`), with a per-instruction `overflow` field (`lir.gg:125-127`),
  div0 always checked (`:3598`). Plus the `lower_catch_expr` err_bb/merge_bb CFG template
  (`lower_match.gg:877`), CFG branches (`TBranch`/`TJump`, goto-label emit), enum-variant
  construction (`Fault.Overflow()`), `equip with Error`, the `Displayable`/`Debuggable`
  supertraits + `@derive(Debuggable)`, and the `+%` wrap operator.
- **✅ Rust-first is safe (Q3):** the self-host's OWN source keeps panic-on-overflow (it
  won't use fault-catch), so `bootstrap_fixed_point` and the frozen `runtime_snapshots`
  are untouched; new fault-catch fixtures the self-host can't compile yet register as
  not-yet-at-parity in the diagnostic `self_host_runtime_diff` — honest, not a regression.
- **Self-host work, when it FAST-FOLLOWS (not blockers):** the SAME new
  checked-op-with-handler-branch LIR shape (unsolved in both; mirror Rust's design once
  proven); a new fault-catch AST/grammar/typecheck path (its `ECatch` is `Result`-welded
  like Rust's, and `infer.gg` has NO `ECatch` handler — the typecheck path is greenfield);
  a `lower_fault_catch` cloned from `lower_catch_expr` (branch from the checked-op, not a
  Result tag) + handler-bb `Fault` materialization; and a per-expr force-checked signal
  (different shape — no global wrap mode here, it's operator-driven `+`/`+%`).
- ⚠ **Footgun to guard:** self-host `map_binop` silently defaults unknown operators to
  `OP_ADD` with only a `diag_bug` warning (`lower_types.gg:2434`) — wire any new
  fault-catch operator token explicitly + add a fixture/lint when the self-host side lands.
