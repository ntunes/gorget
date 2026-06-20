# RFC — Conversion via construction (retiring `as`, `From`/`Into`/`TryFrom`)

> **Status:** DRAFT / design discussion (owner-initiated 2026-06-20). Not yet
> approved; no implementation. Supersedes the ad-hoc `as` operator and the
> `From`/`Into`/`TryFrom` trait trio for the conversion use case.
>
> **Core decision (v2):** fallibility is the **`throws` effect**, not a return
> type, and it is decided by **types**, not values — a widening constructor is
> simply not `throws`; a narrowing one is. No argument-dependent "throw elision."
> Enforcement is throw-site (a declared throw you must handle), not compile-site.

## 1. The problem

Gorget converts values two incompatible ways:

- **Numerics** use `as` — `int x = 3.7 as int`. It is **silently lossy**
  (`300 as byte` → `44` with no signal), routes through no trait, and is the
  one conversion path that ignores the language's "no silent footguns" ethos.
  (`as` is also a known regret in Rust, the language it was borrowed from.)
- **Everything else** uses constructors (`Meters(3.0)`, `Celsius(...)`) plus the
  `From[T]` / `Into[T]` / `TryFrom[T]` traits.

So numerics are the *only* types that don't convert the way every other value is
built, and the trait trio duplicates what constructors already express. The
language already rejects cross-type `as` (`String as int` → compile error). This
RFC finishes that thought: **conversion is construction**, uniformly.

## 2. The principle

> **Converting a value to type `T` is constructing a `T` from it.**

Gorget already builds every value as `T(args)` and already has the three pieces
this needs — `T(...)` constructors, **named arguments** (`Vector[int](alloc=pool)`),
and compile-time evaluation (`meta`) — plus the **`throws`** machinery
(`throws`/`throw`/`rethrow`/`on error`/`catch` + `From`-widening) that makes the
fallible case ergonomic. So "the Swift model" costs almost no new surface; it
mostly *deletes* `as` and the trait trio.

## 3. The design

### 3.1 The constructor is the conversion

```gorget
# widening — always lossless, plain constructor
float f = float(42)              # 42.0
long  l = long(small_int)

# float → int — truncate toward zero (the defining meaning of float→int)
int n = int(3.7)                 # 3
```

### 3.2 Fallibility is `throws`, not a return type  *(the heart of this RFC)*

A conversion constructor **returns `Self`** and may **`throw CastError`**. Two
things make that ergonomic instead of viral:

1. **Failability is TYPE-level — there is no argument-dependent "throw
   elision."** A conversion whose source type *fits* in the target (widening:
   `float(int)`, `long(byte)`, `i64(i32)`) is simply **not `throws`** — an
   ordinary non-throwing function picked by type-directed resolution, callable
   anywhere with zero ceremony. `i64(i32)` doesn't throw for the same reason
   `2 + 2` doesn't. A conversion that *can* exceed the target (narrowing:
   `byte(i64)`, `int(float)`) **is `throws`** — always. Compile-time-constant
   narrowings (`byte(200)`) are checked by ordinary `meta` const-evaluation
   (fits → ok; `byte(300)` → compile error). The only residual — "this *runtime*
   value fits but its type is wider" (`byte(x % 256)`) — uses a total flavor
   (§3.3), **not** a value-flow analysis. (We deliberately do NOT introduce
   call-site-argument-dependent effect; whether a cast throws is decided by its
   types, statically.)
2. **When it genuinely can fail** (narrowing a runtime value, parsing,
   NaN/∞→int), it throws. In a `throws` function it **propagates** — widening its
   `CastError` into the function's error type *via the target error's
   constructor* (§4). In a non-`throws` function the compiler **requires you to
   handle it** (`catch`) or pick a total flavor (§3.3) — Gorget's existing
   throws-handling rule, unchanged.

This is why "always return `Result`" would be **too much ceremony** (it taxes the
infallible majority — struct ctors, widenings — with a `Result` they must unwrap
even though it's never `Error`), while `throws` costs *nothing* there. It also
**dissolves the "failable-constructor mechanism" question**: the return type is
*always* `Self`, so there is no `Self`-vs-`Result` overload to invent.

You recover a value explicitly only when you want to handle failure locally:

```gorget
byte b = byte(x)                          # throws fn: propagates; non-throws fn: must catch (or use a flavor)
Result[byte, CastError] r = catch byte(x) # explicit handling → typed Result
Option[byte]            o = (catch byte(x)).ok()   # Swift's Optional, on demand
```

So you get **`throws` propagation, `Result`, and `Option` all from one
mechanism** — and the earlier "Result vs Option" question resolves as: the
*channel* is `throws CastError`; `catch` yields `Result[T, CastError]` (typed,
not stringly); `.ok()` yields `Option[T]`.

### 3.3 Named-argument flavors are the **total, non-throwing** opt-outs

Swift's labeled initializers (`UInt8(clamping:)`, `Int8(truncatingIfNeeded:)`)
map onto Gorget's named args — and they're the way to say "don't throw; give me
a defined value instead." They never fail:

```gorget
byte b = byte(clamping   = 300)   # 255   — saturate
byte b = byte(truncating = 300)   # 44    — wrap / bit-truncate
int  n = int(rounding    = 3.7)   # 4     — round-to-nearest (bare int(f) truncates)
byte b = byte(bits = signed_x)    # reinterpret bit-pattern — explicit, never implicit
```

| form | int → narrower int | float → int | widening / struct |
|---|---|---|---|
| `T(x)` | narrowing ctor: returns `T`, **throws** Overflow (constants const-checked) | returns `int`, truncates fraction, **throws** on NaN/∞/out-of-range | widening ctor: not `throws` |
| `T(clamping = x)` | saturate (total) | saturate (total) | — |
| `T(truncating = x)` | wrap (total) | — | — |
| `T(rounding = x)` | — | round (total) | — |
| `catch T(x)` | `Result[T, CastError]` | `Result[int, CastError]` | (valid; never `Error`) |

### 3.4 The error type — typed, not stringly

```gorget
enum CastError:
    Overflow            # value too large for the target
    Underflow           # value too small (e.g. negative into unsigned)
    NotRepresentable    # NaN / ±∞ → an integer type
    Parse(ParseError)   # String → T: position + reason
```

(Upgrade over today's `TryFrom -> Result[Self, String]`.)

### 3.5 `int(float)` is settled by the same rule

`int(3.7) → 3` truncates the fraction — that is what float→int *means*, not a
footgun, so no ceremony. The genuinely undefined cases throw:

```gorget
int n = int(3.7)        # 3        (truncate toward zero)
int n = int(0.0/0.0)    # throws CastError(NotRepresentable)   (NaN)
int n = int(1.0/0.0)    # throws CastError(Overflow)           (+∞)
int n = int(1e30)       # throws CastError(Overflow)           (doesn't fit int64)
```

## 4. The payoff — one mechanism, three trait deletions

If conversion *is* (possibly-throwing) construction:

- `From[T]`   → a constructor `Self(T)`.
- `TryFrom[T]` → a **throwing** constructor `Self(T) throws CastError` (`catch` for a `Result`).
- **`Into[T]` → deleted.** You write `int(x)`, never `x.into()`.
- **Auto-propagation error-widening** — Gorget already converts a thrown error to
  a `throws` function's declared error type via `From[T]` (no `?`; auto-propagated:
  `String s = read_file(p)` widens `IoError`→`AppError` through
  `equip AppError with From[IoError]`). Now it looks up the target's **constructor**
  `AppError(IoError)` instead of the `From[IoError]` impl.

```gorget
# user newtype — already a constructor; nothing special:
Celsius c = Celsius(fahrenheit = f)              # was: equip Celsius with From[Fahrenheit]
Percentage p = Percentage(120)                    # was: TryFrom → Result; now: throws CastError(Overflow)

# error widening at a boundary — the target error's constructor:
equip AppError:
    AppError(IoError e): ...                       # was: equip AppError with From[IoError]

int parse_port(String s) throws PortError:
    int raw = int(s)            # int(String) throws CastError(Parse) → widens to PortError via the ctor above
    byte hi = byte(raw >> 8)    # throws CastError(Overflow) if out of range → propagates
    return raw
```

So builtin numerics, newtype conversion, and error widening all go through
**constructors**; `as`, `Into`, and `TryFrom` retire into them.

## 5. Open questions

**Resolved this round:**

- **Generic bounds → caller converts.** A function that needs an `int` takes
  `int`; the caller writes `f(int(x))`. The old `Into[int]` bound only existed to
  *hide* a conversion behind a call boundary — the opposite of this RFC's goal.
  (A `Numeric` trait stays for genuinely number-polymorphic code like
  `T sum[T: Numeric](Vector[T])` — that's parametric polymorphism, orthogonal to
  conversion.)
- **Failable-constructor mechanism → dissolved** by §3.2 (always `Self` + `throws`).
- **`int(float)` edges → §3.5** (truncate fraction; throw on NaN/∞/out-of-range).
- **`as` removed entirely (owner-decided 2026-06-20).** `T(x)` is the *sole*
  conversion spelling; the `as` operator is gone — no second way to convert, no
  lossless-`as` carve-out. Every existing `as` becomes a constructor call.
- **Enforcement → throw-site (owner-decided 2026-06-20, Knob 1a).** A narrowing
  `T(x)` on a non-provable value compiles and *throws* `CastError`; the compiler
  enforces that the throw is handled (propagate in a `throws` fn, `catch`, or use
  a total flavor) — it is NOT a compile error. The Zig compile-site alternative
  was considered and declined.
- **No throw-elision (owner-decided 2026-06-20, Knob 2).** Failability is
  type-level (§3.2): widening ctors are not `throws`, narrowing ctors are — no
  argument-dependent analysis to make a narrowing call "not throw." Constants are
  `meta`-const-evaluated; the "runtime value fits but its type is wider" case
  uses a total flavor. So "elision precision" is moot — there is no elision to
  tune. (This deliberately avoids introducing call-argument-dependent effect.)
- **No conversion marker (owner-decided 2026-06-20).** A 1-arg constructor
  `Self(T)` IS the conversion from `T` — structural, no annotation. This is
  CONTINUITY with the existing mechanism, not a new risk: today a thrown error
  auto-widens to a `throws` fn's declared error type via an *explicit* `From[T]`
  impl (`equip AppError with From[IoError]` / `@derive(From)`); the constructor
  `AppError(IoError)` simply *replaces* that `From` impl, and auto-propagation
  looks it up structurally. You still opt in by *writing the constructor*. The
  C++ implicit-converter footgun doesn't apply — auto-conversion fires only for
  ERROR types in `throws` contexts, so a `Meters(float)` ctor never "accidentally"
  converts (it would require `float` to be thrown as an error). `@derive(From)`
  (newtype-only today) → the newtype's auto-generated 1-arg constructor.

**All design questions are now resolved.** The remaining work is implementation
(§6), not design.

## 6. Migration

A **both-compilers, language-surface** change (Rust gg + self-host + spec + book
+ negative fixtures), staged:

1. **Spec first** (this doc → `docs/language-design.md` + `docs/book` once
   settled). The design knobs are now resolved; the remaining spec work is the
   per-type-pair widening/narrowing table + the `CastError` enum + the
   constant-narrowing compile-check rule.
2. **Numeric constructors (widening = non-`throws`, narrowing = `throws`) +
   flavors + `CastError`** in both compilers, with constant-narrowing checked at
   compile time (`meta`). No new value-flow analysis.
3. **Remove `as` entirely.** Widening `as` → the non-throwing constructor;
   narrowing/`float→int` `as` → the throwing constructor (`float as int` →
   `int(f)`, which throws on the un-representable edges). Per CLAUDE.md core
   invariant #8, the outcome is that a lossy conversion can no longer happen
   *silently* — it's handled (throw), a flavor, or a compile-checked constant —
   in **both** compilers, with negative fixtures.
4. **Fold `From`/`TryFrom` into constructors; delete `Into`;** rewire the
   `throws` auto-propagation error-widening to look up the target's constructor
   (instead of its `From[T]` impl); `@derive(From)` → the newtype's generated ctor.

### Interaction with the in-flight `as` fix (parity track #1)

A parity track currently makes the **self-host's `as` actually convert** (it's a
no-op stub emitting garbage today). Two coherent paths:

- **Interim (A):** land that fix so self-host `as` matches Rust gg's truncating
  `as` (+3 parity, removes garbage), and treat this RFC as the replacement. Risk:
  briefly cements the silent-lossy behavior we plan to retire.
- **Direct (B):** skip the truncate fix; start §6.2/§6.3 — numeric constructors
  (widening = non-`throws`, narrowing = `throws` + `CastError`) and remove `as` —
  in both compilers. Bigger, but it's the reference-grade target and avoids
  shipping-then-reverting. (Note: the cast-EMISSION machinery the interim fix
  builds — GIR `GICast` → the existing LIR int/float cast ops — is exactly what
  the `int(x)`/`byte(x)` constructors lower through, so it is reusable either way;
  only the `as`-syntax hookup is interim.)

Owner's call. (The const-read parity track is independent and proceeds
regardless.)

## 7. Doc-consistency review (2026-06-20)

Three read-only reviewers checked this RFC against `language-design.md`,
`docs/book/`, and `docs/devbook/` + the actual `src/` internals
(`a88a07ca` / `ab22c7ab` / `a02477f7`). Verdict: **directionally sound — the
philosophy fits and the backbone exists — but it is a LARGER change than §6
implied, three premises were inaccurate, and there are 3 genuine design FORKS
for the owner (below).**

### 7.1 Confirmed sound (no change)
- **Philosophy fit is excellent.** "No silent narrowing / explicit conversion"
  is already the docs' voice (`language-design.md:43/48/401/471`,
  `book/02-types.md:132`, `book/09-option-result.md:47-87`). Removing silent-lossy
  `as` *strengthens* a value the docs already preach.
- **The type-level-failability BACKBONE already exists, layering-clean.**
  `is_safe_integer_widening` (`src/semantic/typecheck.rs:159-181`) decides
  widening-vs-narrowing from the `(src,tgt)` PrimitiveType pair as typed metadata
  (not name-matching) — this is exactly what should drive throws-ness. Use it.
- **The interim EAs (#1) machinery is genuinely reusable.** `int(x)`/`byte(x)`
  constructors lower through the same LIR cast ops (`IntCast`/`FloatCast`/
  `FloatToInt`/`IntToFloat`, `src/lir/lower/insts.rs:213-407`) that `Expr::As`/
  `Instruction::Cast` and the existing `int(x)` builtin both emit. Only the
  `as`-syntax hookup is interim; ideally unify `int(x)` onto `Instruction::Cast`
  (retiring the name-matched `emit_name=="int"` LIR special-case, `insts.rs:3896`).
- **Named args + flavor names are clear.** `clamping`/`truncating`/`rounding`/
  `bits` clash with no keyword and don't trip "once named, rest named". `bits`
  is even a live param name today.
- **`@derive(From)` → ctor is a small refactor** — `src/semantic/derive.rs:674`
  already generates `T from(U v): return T(v)`; the body IS the ctor call.

### 7.2 Corrections to APPLY (not forks — clear fixes for when this is implemented)
- **Scope the `as` removal to the CAST `as_expr` only.** `as` is a reserved
  KEYWORD with live non-cast uses that MUST stay: `with X as Y` (the `with`-form
  disambiguator, `language-reference.md:1354`), `import X as Y` (`:841`), and the
  unsafe pointer/Ref reinterpret `raw_pointer as int&` (`language-design.md:1779`).
  "Remove `as` entirely" read literally breaks all three. Decide the unsafe-ptr
  cast separately (keep raw-cast / `bits=` reinterpret / leave in `unsafe`).
- **Drop `catch byte(x)` → use TYPED-DESTINATION capture.** Gorget's `catch` is a
  POSTFIX recovery operator yielding the success type (`book/10-errors.md:169`),
  NOT a prefix → `Result`. The existing idiom for "capture as Result" is a typed
  destination: `Result[byte, CastError] r = byte(x)` (`book/10-errors.md:105`).
  Use it; `.ok()` (Result→Option) doesn't exist today — specify it or rely on the
  typed dest + match.
- **`Into` deletion is a NO-OP; `Numeric` "stays" = no work.** `Into` is not
  implemented (not in the trait registry, zero `.into()` call-sites) — only the
  `language-design.md` text (`:848/874/944`) mentions it; fix those. `Numeric`
  ALREADY exists (`src/semantic/traits.rs:413/816`) — it's in the book but NOT
  language-design's registry §40.3, so ADD it there (don't "keep" it).
- **Preserve `!` (move) on conversion ctors.** Today's `AppError from(IoError !e)`
  MOVES; the RFC's bare `AppError(IoError e)` would CLONE under CoW. Write `!e`.
- **Reconcile `Parseable`** — the built-in, NEVER-panics `String→numeric`
  (`language-reference.md:2935`, `Option`-returning) is the documented String path
  alongside `int.parse`/`parse_int`. `int(String) throws CastError(Parse)` makes a
  THIRD spelling; fold/relate them (the RFC's "one mechanism" goal demands it).
- **The error-widening rewire is TWO layers, not one** — typecheck keys on the
  `From` trait def-id (`lookup_from_conversion`, `typecheck.rs:4493`) AND lowering
  resolves by name-matching the `_for_<E>__from` symbol suffix (`exprs/mod.rs:3092`,
  a layering-rule smell). BOTH move to constructor resolution. (Confirms the
  From-based auto-widening IS real in the compiler; the book just never teaches it
  — add a book section.)
- **The const-narrowing compile-check (`byte(300)`→error) is NEW logic**, not free
  `meta` — no existing const-evaluator rejects an out-of-range narrowing literal.
  Frame it as an extension.
- **Numeric `T(x)` is REJECTED today for most types.** `typecheck.rs:1391-1422`
  emits `UnloweredBuiltinCall` for `byte`/`int8..64`/`uint*`/`float32/64`/`str` —
  only `int`/`float`/`bool` lower (and only to I64/F64/Bool, no narrowing emit).
  §6.2 is a from-scratch fallible-conversion subsystem, not a tweak.

### 7.3 OPEN FORKS — owner decisions before implementing
1. **Overflow on narrowing: panic vs throw vs (current) saturate?** THREE behaviors
   are in tension: today the cast OPS **saturate** (Rust-`as`: NaN→0, clamp;
   `c_lir/mod.rs:2631`), the docs say integer overflow **panics**
   (`language-design.md:191/1298`, rule "caller could prevent → panic", `:1312`),
   and this RFC says **throw** `CastError`. Pick one, and reconcile with the
   arithmetic-overflow rule (why would `byte(x)` throw while `a + b` panics?).
2. **The constructor mechanism for throwing/overloaded conversions.** It does NOT
   exist: `T(args)` is a body-less `Expr::StructLiteral` (`semantic/mod.rs:272`)
   with no return slot, no effect channel, field-count-only checks; `throws` lives
   only on `FunctionDef`; there is NO constructor overloading by arg type. So
   "ctor returns `Self` and throws `CastError`" + "widening ctor non-throws,
   narrowing throws" needs one of: (a) constructors gain a real function form with
   an effect slot + overload resolution (big new feature); (b) numeric conversions
   are real `throws` *functions* merely SPELLED `T(x)` (typecheck/resolution
   change, not StructLiteral); or (c) `T(x)` stays sugar over a conversion TRAIT
   that survives under the hood (reuses `From`/`is_safe_integer_widening`; least
   new machinery, but doesn't fully "delete the trait"). This is the central call.
3. **`str(x)`: re-open, or keep rejected?** "`T(x)` is the sole conversion
   spelling" reverses the standing OWNER DECISION 2026-06-10 that `str(x)` is
   rejected (f-strings / `.display()` are THE String conversion, one-obvious-way).
   Keep that carve-out, or reverse it?

**Bottom line:** the design is worth doing and the backbone (O1) + interim
machinery are real, but §6 under-scopes it (from-scratch fallible-conversion
subsystem + a new constructor-effect/overload facility), and forks 1–3 need owner
calls before a scout/brief. Until then this stays a DRAFT.
