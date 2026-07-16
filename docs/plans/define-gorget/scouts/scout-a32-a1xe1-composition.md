# A32 pre-impl design question — the A1×E1 composition (unannotated function types: latent or infallible?)

**Status:** OPEN design question, filed 2026-07-16 at the review of commit `e44b6120`. Needs an
owner pin before the A32 implementation brief is written. Nothing here re-litigates a ratified
fork; the question lives in the *seam between* two ratified pins.

**Purpose of this document:** a self-contained statement of the problem + a recommended
resolution ("Option 2"), written to be handed to a fresh reviewer with no session context.
Reviewer: please verify the load-bearing claims against the cited files, stress-test the
recommendation (§6 lists the corners we most want poked), and return either SIGN OFF or
specific cited reservations — do not rubber-stamp, do not invent reservations.

---

## 1. Context for a fresh reviewer (60 seconds)

Gorget is a statically typed, Python-like systems language (type-first declarations:
`int x = 5`; functions `int add(int a, int b):`; function types spelled return-type-first:
`int(int, int)`). The error model is being finalized through a decisions ledger:
`docs/plans/define-gorget/decisions.md` (normative LOG at the bottom). The pins relevant here,
all owner-ratified 2026-07-16:

- **D23 (throws totality):** a `throws E` call is typed as its success type `T` in every
  position; the `Result` wrapping is unobservable.
- **D29 (visible error propagation):** every *fallible call* — a callee declared `throws E`
  OR with a declared `Result[T,E]` return — carries a mandatory postfix `!` at the call site:
  `f()!`. This is a uniform *fallible-use marker* (Swift-style always-mark): handled calls
  carry it too (`f()! catch (e): …`, `Result[T,E] r = f()!`). Bare fallible calls are always
  illegal. `throws E` remains the explicit contract spelling on declarations. Signatures never
  take `! E`.
- **A31 (reserved):** bare `!` on a signature (`int f()!:`) is grammar-locked to mean
  "inferred error set" — parses today, teaching-rejected until A31 inference is implemented.
- **A32 (HOF effect polymorphism, basic design ratified as forks A1–G1 — evidence:
  `scouts/scout-a32-hof.md`):** a higher-order function is fallible iff a function argument
  it *invokes* is fallible. Key forks for this document:
  - **A1 — inferred rethrows:** HOF source declarations stay success-typed (no `throws`, no
    `rethrows` keyword, no effect parameters); the HOF call's effect is *computed* from the
    callback at each call site.
  - **B1 — same-E:** multiple fallible callbacks must share one error type `E` in v1.
  - **C1 (A32's Fork C) — throws callables:** closures may be `(params) throws E: body`;
    function types carry an effect slot (today they don't:
    `ResolvedType::Function { params, param_ownerships, return_type }`,
    `src/semantic/types.rs:32-37` — no throws field; this slot is an A32-impl prerequisite).
  - **E1 — no silent coerce:** a throwing/fallible callable does **not** silently coerce to
    an infallible function type.
- **Doctrine:** sigils mark *flow* at use-sites; keywords state *contracts* at declarations.
  No permanent `try_map`/`try_filter` stdlib duals.

The compiler monomorphizes generics. The current substrate has no HOF effect polymorphism at
all: closures are forced non-throwing frames (Snag #44, `src/semantic/typecheck.rs:3348+`),
and builtin HOF typing reads only the callback's success type.

## 2. The problem — A1 and E1 assign opposite meanings to the same spelling

Both pins are individually right. But they both speak about the same syntactic object — an
unannotated function type like `int(String)` — and, read literally together, they deadlock.

### Example 1 — the case A32 exists for (must be ACCEPTED)

```gorget
# Stdlib HOF, per A1 — success-typed signature, no effect anywhere:
Vector[U] map[T, U](self, U(T) f):
    ...

int parse_one(String s) throws ParseError:
    ...

Vector[int] ys = xs.map(parse_one)!    # A32's flagship: map rethrows ParseError
```

At the type level, `parse_one` (effect `throws ParseError`) is passed into a parameter whose
declared type is `int(String)` — no effect. That is, verbatim, "a fallible callable coercing
to an infallible function type." **E1 read literally rejects this call.** If E1 wins here,
A32's inference never fires on any program.

### Example 2 — the case E1 exists for (must be REJECTED)

```gorget
Vector[int(String)] handlers = []
handlers.push(parse_one)     # effect laundering: a later handlers[0](s)
                             # invocation has no visible error channel
```

If Example 1 is fixed by "unannotated function types accept fallible callables everywhere,"
this must be accepted too — and a `throws` function now sits in an infallible-typed slot,
every later indirect call having lost the error channel (and its mandatory D29 `!`). That is
exactly the hole E1 was ratified to close.

So neither blanket reading works: "E1 always wins" kills A32; "A1 always wins" kills E1.

### Example 3 — the middle, where the real rule gets decided

```gorget
struct Pipeline:
    Vector[int(String)] stages

void add_stage(Pipeline &self, int(String) f):
    self.stages.push(f)        # STORES f  → E1 territory
    int probe = f("test")      # INVOKES f → A1 territory

pipeline.add_stage(parse_one)  # accept or reject? if reject — at which line, whose file?
```

The same parameter is both invoked (A1 wants its effect to flow out) and stored (E1 forbids a
fallible value landing in an infallible slot). And the ratified rule ("a HOF is fallible iff
it invokes a fallible fn-arg") is not limited to generics:

```gorget
int apply_twice(int(int) f, int x):    # not generic at all
    return f(f(x))
```

If `int(int)` in param position accepts fallible callbacks here too, then an unannotated
function type at a parameter *never* means "infallible," and E1 has no force at call
boundaries at all — only at storage boundaries. Maybe that's the right design; but today it
is an unstated inference from two pins, not a pin.

There is one adjacent facet any resolution must also answer: **what does the HOF body look
like under D29's always-mark?** Inside `map`, the invocation `f(elem)` *may* fail (the effect
is latent), so presumably it must be written `f(elem)!` — even for instantiations where the
callback is infallible. (Swift faces the same with `try` inside `rethrows` bodies.) A rule
for marks on latent-effect invocations must exist or the stdlib HOFs cannot be written.

## 3. Option 1 — positional rule (the first sketch; workable, not recommended)

> An unannotated function-type **parameter** is effect-generic (latent effect variable,
> resolved per call per A1). Every **other** position (local binding, struct field, collection
> element, return type) is concrete-infallible per E1. Moving an effect-generic param into a
> concrete position forces resolution: the latent effect must be infallible, or the
> destination type must spell the effect.

This makes all three examples come out right (Example 3 rejects at the `push` line). Its
weakness: **the same type spelling means different things depending on position** — an
invisible special case of the C++ "array decays to pointer in params" species. It reads fine
in a ledger and costs a teaching paragraph forever; the polymorphism of `map` is invisible in
`map`'s signature (an API-stability wrinkle too: a HOF that stops invoking its callback
silently changes effect for all callers); and the implementation needs positional defaulting
logic in the type resolver.

## 4. Option 2 — explicit latent marker on the callable type: `U(T)!` (RECOMMENDED)

Reuse the already-ratified spelling. A31 grammar-locked bare `!` after a signature's `)` to
mean *inferred error set*. Extend the same meaning to function **types**:

> **Rule:** bare postfix `!` on a function type (`U(T)!`) = "callable with a latent/inferred
> error set" (which may resolve to empty). Absent `!` = concretely infallible. **E1 is then
> uniform everywhere, with no positional carve-out:** a fallible callable never silently
> coerces to an effect-less function type — anywhere. A32's rethrow inference fires only
> through `!`-marked callable params.

```gorget
Vector[U] map[T, U](self, U(T)! f):     # `!` — f may carry an error set; map rethrows it
    Vector[U] out = []
    for x in self:
        out.push(f(x)!)                 # D29 always-mark applies mechanically: f is
    return out                          # latent-fallible as far as this body knows

void add_stage(Pipeline &self, int(String) f):   # no `!` — concretely infallible
    self.stages.push(f)                          # fine: f provably cannot throw
    int probe = f("test")                        # no mark needed

pipeline.add_stage(parse_one)   # REJECTED at the CALL boundary (E1, uniform):
                                # "parse_one throws ParseError but `f` is infallible
                                #  int(String) — mark the param `int(String)!` to accept
                                #  fallible callbacks, or pass an infallible function"
xs.map(parse_one)!              # accepted; this instantiation of map throws ParseError
xs.map((int x): x + 1)          # accepted; latent effect resolves EMPTY → no call-site mark
```

Example 3 stops being ambiguous: it is whatever the author wrote. With `int(String)! f`, the
`push` into `Vector[int(String)]` rejects (latent → concrete needs resolution — store it in a
`Vector[Callable[int(String)!]]`-shaped slot or prove it infallible); with `int(String) f`,
the call `add_stage(parse_one)` rejects at the caller. Both rejections land at the honest
boundary with a teachable message.

### Why this beats Option 1

1. **One uniform E1.** "A fallible callable never silently coerces to an effect-less type,
   anywhere" — one sentence, no positional footnote, book-grade.
2. **Uniform glyph algebra.** Post-wave, `!` always sits immediately after a call shape:
   `f()!` (call site, D29) · `int f()!:` (signature, A31 reservation) · `U(T)!` (callable
   type, this pin). One visual rule — *paren-close, then bang: the error channel is here* —
   in all three positions.
3. **Contract honesty.** The HOF's effect polymorphism is visible in its signature. This
   fixes A1's API-stability wrinkle (silent effect change when a body stops invoking the
   callback) and answers D29's founding critique ("no way to tell if calling this can fail")
   at the signature level, not just the call level.
4. **The body-mark question answers itself.** Inside the HOF, an `!`-typed callable is
   fallible-as-far-as-the-body-knows, so D29's always-mark applies with zero new rules:
   `f(elem)!`. For infallible instantiations the mark is vacuous-but-true (the latent set is
   empty), matching D23 totality: mono can compile it as a plain call.
5. **Simpler implementation.** The effect slot on `ResolvedType::Function` is set exactly
   where written; no positional defaulting pass; the E1 check is one uniform coercion rule;
   B1's same-E join reads the latent variables off the marked params.

### Costs and caveats (state them honestly)

- **One glyph per HOF callback param.** Stdlib gains roughly a dozen `!`s (map / filter /
  fold / each / sort-by / iterator adapters). Users writing HOFs must know the marker; the
  E1 diagnostic must carry the fix-it ("mark the param `int(String)!` to accept fallible
  callbacks").
- **Parse corner in bare param position.** The D29 packet measured `int(int)! name` as
  ambiguous while prefix-`!` is still the move sigil (pre-D27). Until D27 lands (`^` takes
  over move), the marker may need the bracketed form in param position:
  `Callable[int(int)!] f`. A32 impl is sequenced after D29 and near the D27 wave, so the
  corner likely dissolves before it binds; if not, bracket-only in param position is an
  acceptable v1 spelling (it is already the packet's recommended callable form).
- **It bends A1's "no new surface" by one glyph.** A1 said: no `rethrows` keyword, no effect
  parameters, effect computed per call. Option 2 keeps all of that (the `!` introduces no
  named effect variable and no keyword) but does put a marker in the signature, so it needs a
  one-line owner amendment to A1/E1 — an amendment, not a re-litigation.
- **Doctrine tension to adjudicate:** "sigils = flow at use-sites; keywords = contracts at
  declarations" — this is a sigil in a declaration. Counterpoint: D29 already grammar-locked
  bare `!:` on signatures as A31's contract spelling ("error set inferred"), so `U(T)!` is
  arguably the same contract spelling in its third position, not a new doctrine exception.
  The reviewer should weigh whether this reads as extension or violation.

### Interaction checklist (how Option 2 composes with the other ratified forks)

- **B1 (same-E):** two `!`-marked params (e.g. a fold with two callbacks) join their latent
  sets; v1 requires them equal, else type error. Unchanged.
- **D1 (collections rethrow / Result combinators data-plane):** unchanged; `Result.map`'s
  callback param simply stays unmarked (`U(T)`) in v1, or marked with the same-E constraint
  per the ratified D1 text.
- **D29 one-mark-for-both-kinds:** the latent set covers both fallibility kinds (throws
  callee or declared-`Result` return) exactly as D29 defines a fallible call.
- **F1 (traits/equip):** the marker appears in trait method signatures the same way; default
  method bodies check under the latent effect.
- **E1's own examples:** storing a fallible callable requires the destination to spell the
  effect (`Callable[int(String) throws ParseError]`-shaped, per Fork C's function-types-carry-
  effect) — the "no silent coerce" behavior users were promised, now with a visible opt-in.

## 5. Other options considered (and why not)

- **Option 3 — per-instantiation template checking (no signature rule at all):** monomorphize
  with the actual callback's effect and re-check the HOF body; storage misuse fails *inside
  the callee body*. Maximally permissive, zero surface — but errors point into someone else's
  function body, and a HOF's contract becomes unknowable from its signature (the exact
  "downstream reconstructs what upstream should have declared" shape the project's layering
  discipline exists to kill). Rejected on principle.
- **Option 4 — registry-only rethrows (builtin HOFs get a typed `rethrows` flag; user params
  stay infallible):** tiny to implement, but user HOFs then force `try_apply`-style duals —
  the "no permanent try_map" doctrine violated one level out — and it keys a semantic
  capability off registry membership (Fork C3's rejected cousin). Acceptable only as an
  explicit staging increment *inside* the real design.
- **Option 5 — flip the default (every function type latent everywhere):** storage becomes
  unrestricted and every indirect call is marked. E1 evaporates, the mark loses signal (if
  everything is marked, nothing is), and latent sets raise type-identity questions. Rejected.
- **Wrap-coerce at the boundary** (convert the error to a fault): this is Fork E's rejected
  option E2. Dead.

## 6. What we ask the reviewer

1. Verify the load-bearing claims: the A1/E1/C1 fork texts and the D29/A31 pins in
   `docs/plans/define-gorget/decisions.md` (LOG, 2026-07-16 entries) and
   `scouts/scout-a32-hof.md`; the substrate facts (`src/semantic/types.rs:32-37` — no effect
   slot; `src/semantic/typecheck.rs:3348+` — Snag #44 non-throwing closure frames).
2. Is the deadlock real as stated, or is there a reading of A1+E1 that dissolves it without a
   new pin?
3. Stress-test Option 2 against corners we may have missed, especially: nested function types
   (`Vector[int(int)!]` elements — is invoking an element a marked call? [we believe: yes,
   and that is the point]); returning an `!`-typed callable from a function; effect-generic
   params captured by closures; partial application patterns; `gg fmt` migration (can the
   marker be inserted mechanically for stdlib rows?); diagnostics quality at both rejection
   boundaries; whether latent-set EMPTY resolution ever needs surface syntax.
4. Compare honestly against Option 1 (positional). If you conclude Option 1 (or another
   option) is stronger, say so with reasons — the recommendation is not the assignment.
5. Return SIGN OFF or specific reservations with `file:line`/section citations.
