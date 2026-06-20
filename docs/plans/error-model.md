# RFC: The Gorget Error Model — one typed error channel, two kinds of error

> **Status: DRAFT / exploratory (owner brainstorm 2026-06-20).** This is a
> LANGUAGE-DIRECTION doc, far bigger than any parity fix. It captures a design
> exploration; nothing here is approved or implemented. It needs the full
> scout → brief → ≥3 fresh reviews cycle before any code moves. Companion to
> [`cast-via-construction.md`](cast-via-construction.md) — conversion-overflow and
> arithmetic-overflow are the two worked examples that fall out of this model.

## 0. The question that spawned this

The cast RFC decided **conversion overflow throws** (`byte(x)` is recoverable).
That raised: *should ARITHMETIC overflow (`a + b`) also be recoverable rather than
hard-panic — and if so, how, without taxing every function?* Owner's deeper probe:
*would it be that bad to give EVERY function a typed error channel (like every
function having `stdout` + `stderr`, both typed) — most throwing `Never`, but
auto-propagating?* This doc answers both, and the answer is one model.

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
  is a branch you wrote on purpose.
- **Faults** — rare to actually fire, recovered only at coarse boundaries → an
  **unwind/abort path**. The common (no-fault) path does **not** thread a fault
  value through every call, so it stays branch-free of error-union plumbing; the
  unwind only runs when a fault actually fires.

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

## 9. Open questions (for the scout/brief, before any implementation)

1. **Fault catch syntax & scope.** Catchable *anywhere*, or only at declared
   task/request/`supervisor` boundaries? What's the spelling (`catch Overflow:` at a
   boundary block)? How does it relate to Gorget's existing postfix `catch`?
2. **The "fast" tension** (§6) — debug-checked/release-wrapping vs checked-always;
   the type-vs-runtime-promise reconciliation.
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
7. **Which runtime conditions are faults vs contract** — overflow/bounds/div0/OOM =
   fault is clear; what about `None.unwrap()`, explicit `assert`, allocation failure
   in a no-abort context? Enumerate the full set.
8. **Migration / blast radius** — this touches the whole language. Staged plan,
   guards (`tests/lints.rs`), and a parity story for the self-host (which is
   wall-to-wall arithmetic) before committing.

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
