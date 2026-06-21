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
documented, implemented-with-a-flag (`--overflow=wrap/checked`, `main.rs:2456`)
design decision.** That reversal must be argued on its merits, and the docs
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
invariant: faults are catchable ONLY at a declared coarse boundary that
discards-and-unwinds the unit of work — never inline.**

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

1. **Fault catch syntax & scope — LEANS boundary-only (review pass 1, §3.1).**
   Faults are catchable ONLY at a declared coarse boundary that discards-and-unwinds
   the unit of work — NOT inline/anywhere (anywhere-catch resurrects the corrupted-
   state footgun, `book/10-errors.md:13-14`). The boundary-only constraint is now a
   design INVARIANT, not an open toss-up. Still open: the *spelling* (`catch Overflow:`
   at a boundary block) and how it relates to the existing postfix `catch` (which is
   for *contract* errors).
2. **The "fast" tension** (§6) — debug-checked/release-wrapping vs checked-always;
   the type-vs-runtime-promise reconciliation. **LOAD-BEARING, not a minor knob:**
   it determines whether `catch Overflow` is even *meaningful* — if release wraps
   (`--overflow=wrap`) but the type says "may fault Overflow," the type lies.
3. **How `fault` is declared** on an error type — a marker on the enum/type decl
   (typed metadata, never name-matching), so the classification is read via an
   accessor, not a name list.
4. **`Never` spelling** and how "this function is total" is expressed/checked
   (a `total` qualifier that demands an empty contract row?).
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
    (faults). ⚠ **Follow-up (structural):** §1/§4's "ONE error channel, two kinds"
    framing should be revisited — mechanically these are **two channels** (the
    in-signature `Result` contract channel + the ambient structured-`Fault` channel),
    per §6's value-union-vs-unwind split. That reframe is a doc edit that should get
    its own confirming review (don't silently overhaul the pass-3-signed-off framing).
15. **FFI / `extern`-boundary fault unwind (review pass 2).** A longjmp-based fault
    unwind (§6) that jumps over a foreign C frame skips C-side cleanup and is UB on
    many ABIs. The doc has zero treatment of a fault crossing an `extern` boundary.
    For a longjmp design this is load-bearing: either faults cannot unwind across FFI
    (they abort at the boundary) or the boundary installs a catch. Spec it.
16. **`main` / single-threaded top-level fault boundary (review pass 2).** §4 says
    uncaught faults "abort"; §3.1 says faults are catchable ONLY at a coarse boundary.
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
  like the stdlib errors. Then one boundary `catch e` (`e: dyn Error`) handles BOTH a
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
  catch is lexical and the checked op branches before committing. Phase 1's only new
  machinery: the `Error`-impl on `Fault`; the local-catch lowering (the SAME checked
  op the `--overflow=checked` flag already emits, just branching to a handler instead
  of `exit(1)`); and the panic-default match rule.

**PHASE 2 — deep / boundary catch (separate funded effort; needs unwinding).**
- Catch a fault from a deep call at a task/request boundary (server-keeps-serving).
- Buys the full §6 greenfield unwind infra (Q10) + inherits Q9/Q15/Q16 + the §3.1
  boundary-discards-the-unit safety argument + the boundary-only invariant.
- Strictly **additive** over Phase 1 — same `Fault`/`Error` typing, same `catch`
  surface; only the PROPAGATION reach extends from lexical to deep.

(The §1/§4 "one channel / two kinds" reframe from Q14's follow-up lands with Phase 1.)

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
representation), §9.1 (phasing).

### 11.1 Scope — what Phase 1 delivers
1. A **closed `Fault` enum** — the runtime faults that become catchable:
   `Fault.Overflow`, `Fault.DivByZero`, `Fault.Bounds`, `Fault.OutOfMemory`
   (initial set; exact membership = Q7, settled in the brief — `UnwrapNone`/`Assert`
   are candidates).
2. **`Fault equip Error`** — `Fault` implements the EXISTING `Error` trait
   (`language-reference.md:2766`), so ONE `catch`/match surface handles faults AND
   contract errors uniformly. No new base type, no subtyping, no open enums.
3. **Panic-by-default, unchanged.** Outside a fault `catch`, overflow/bounds/div0
   panic exactly as today (`exit(1)`). A plain `int sum(...)` stays `int` — NO
   signature change, NO `Result` ubiquity, the fast path is untouched.
4. **LOCAL (lexical) catch.** `int r = (a * b) catch Overflow: saturate()` recovers a
   fault from the faultable ops **syntactically within** the wrapped expression. It
   does NOT catch faults that occur inside FUNCTIONS CALLED by the expression — those
   are deep → panic → Phase 2.
5. **Exhaustiveness via implicit-panic-default.** A fault match handles the variants
   it names; unnamed `Fault` variants fall through to **panic** (a
   `non_exhaustive`-style rule for fault-typed matches — you are NOT forced to
   enumerate every fault).

### 11.2 Lowering — no unwinding
- A `catch`-wrapped expression compiles its faultable ops as **checked ops that
  branch to the handler** — the SAME `__builtin_*_overflow` / checked path the
  `--overflow=checked` flag already emits (`src/backend/c_lir/mod.rs:2438`,
  `runtime_checked_arith.c`), except the overflow branch jumps to the handler block
  instead of `gorget_panic`/`exit(1)`. The trap branches BEFORE the store → no
  corrupted value materializes. Pure local control flow — NO setjmp/longjmp, NO
  unwinding, NO drop-across-unwind.
- ⚠ **Overflow-mode interaction (load-bearing):** the global `--overflow=wrap`
  (release-fast) flag must NOT defeat a local `catch`. A `catch`-scoped expression is
  compiled **checked regardless of the global mode** — globally you may run wrap/fast,
  but the ops inside a `catch Overflow` are locally checked. This is the Phase-1
  answer to the §6/Q2 "fast knob": per-expression checked override, not a global
  commitment. The brief verifies the codegen can scope checked-ness per expression.

### 11.3 Explicitly OUT of Phase 1 (→ Phase 2)
Deep/boundary catch (a fault from a called function); the §6 greenfield unwind infra
(B2/Q10); drop/CoW-across-unwind (Q9); FFI-boundary unwind (Q15); the `main`/task
top-level boundary (Q16); the §3.1 partial-state/`Drop`-observation argument (MOOT in
Phase 1 — the checked op branches before any partial mutation commits). None of these
are touched by Phase 1.

### 11.4 Doc + framing changes that land WITH Phase 1
- **§1/§4 reframe:** "one error channel, two kinds" → **two channels** (in-signature
  `Result` contract channel + the `Fault` channel), per Q14's follow-up.
- **`language-design.md` §2.2/§6 + `book/10-errors.md`:** ADD "overflow panics by
  default AND is locally catchable" (the additive change, §0.5); restate the
  Panic-vs-Result rule to cover opt-in recovery.

### 11.5 Phase-1 open questions (resolve in the brief)
- **Exact `Fault` membership** (Q7, the Phase-1 subset).
- **Lexical reach, precisely** — does a `catch` cover ops inside an inline block
  expression or an inline closure body within the wrapped expr, or strictly the
  top-level operator tree? A closure passed to `.map(…)` is invoked via a CALL — its
  body faults are deep (Phase 2). Define the boundary unambiguously.
- **`catch`-by-`Fault` syntax** — reuse the existing postfix `catch`
  (`book/10-errors.md:169`): `(expr) catch Overflow: …` vs `(expr) catch f: match f`.
- **`meta`/const-eval** — Phase-1 local catch at compile time: N/A, or does a `catch`
  in a `meta` context force checked const-eval? (Today `meta` wraps, `meta.rs:1278-1280`.)
- **Self-host parity** — the lowering change must keep `self_host_*` /
  `bootstrap_fixed_point` green; verify no fixture relies on the un-catchable panic
  shape.
