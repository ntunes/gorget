# RFC — Conversion via construction (retiring `as`, `From`/`Into`/`TryFrom`)

> **Status:** DRAFT / design discussion (owner-initiated 2026-06-20). Not yet
> approved; no implementation. Supersedes the ad-hoc `as` operator and the
> `From`/`Into`/`TryFrom` trait trio for the conversion use case.
>
> **Core decision (v2):** fallibility is the **`throws` effect**, not a return
> type, and it is decided by **types**, not values — a widening constructor is
> simply not `throws`; a narrowing one is. No argument-dependent "throw elision."
> Enforcement is throw-site (a declared throw you must handle), not compile-site.
>
> **⚠ §7 IS THE AUTHORITATIVE RESOLUTION LAYER** (the doc-consistency review +
> the owner's final decisions) and **SUPERSEDES §3–§6 where they differ** — notably:
> overflow → THROW (off the strict-panic stance); `T(x)` is the SURFACE over an
> INTERNAL conversion-dispatch (the `From` registry survives under the hood; the
> surface trait names vanish); **flavors are NAMED-ARGUMENT constructors**
> (`byte(clamping = x)`, `int(rounding = f)` — §3.3; an earlier source-method detour
> was REVERSED 2026-06-22, see the correction in §7.3 fork 2); recover a
> Result via the **typed destination** (`Result[T,CastError] r = byte(x)`), NOT
> `catch byte(x)`; value→String is **`String(T)`** (= `Displayable.display()`),
> `str(x)` stays rejected. §3–§4 below are the original v2 sketch; read §7 for the
> final shape. A consolidating rewrite happens when the impl track is funded.

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

# float → int — the rounding mode is a REQUIRED named argument (no bare int(f);
# a silent fractional-loss default is exactly the footgun we ban)
int n = int(truncating = 3.7)    # 3   (toward zero)
int n = int(rounding   = 3.7)    # 4
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
int  n = int(rounding    = 3.7)   # 4     — round-to-nearest (bare int(f) is REJECTED — a mode is required)
byte b = byte(bits = signed_x)    # reinterpret bit-pattern — explicit, never implicit
```

| form | int → narrower int | float → int | widening / struct |
|---|---|---|---|
| `T(x)` | narrowing ctor: returns `T`, **throws** Overflow (constants const-checked) | **rejected** — a rounding mode is required (rows below) | widening ctor: not `throws` |
| `T(clamping = x)` | saturate (total) | — | — |
| `T(truncating = x)` | wrap (total) | toward zero¹ (total fraction; **throws** range) | — |
| `T(rounding = x)` | — | round (total fraction; **throws** range) | — |
| `T(flooring = x)` · `T(ceiling = x)` | — | floor · ceil (total fraction; **throws** range) | — |

¹ `truncating` means "discard what doesn't fit" in both columns — high bits (int
narrowing → wrap) vs the fraction (float→int → toward zero); the meaning is
selected by the source type. Whether to keep the shared label or split them is a
naming detail to settle in the scout. Range loss (NaN/∞/out-of-`int64`) throws
uniformly; a saturating range policy for float→int is a possible later addition.
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

### 3.5 `float → int` requires an explicit rounding mode

A float→int conversion is lossy in *two* independent ways — the fraction
(`3.7`→`3`? `4`?) and the range (NaN/∞/`1e30` don't fit). The fraction has no safe
default (truncating silently is the banned footgun; throwing on every non-integer
float is miserable), so the **rounding mode is a required named argument** — bare
`int(f)` is a typecheck error. The range edges always throw:

```gorget
int n = int(truncating = 3.7)   # 3    (toward zero)
int n = int(rounding   = 3.7)   # 4
int n = int(flooring   = -2.5)  # -3
int n = int(ceiling    = -2.5)  # -2
int n = int(3.7)                # TYPECHECK ERROR: pick a mode (truncating/rounding/flooring/ceiling)
int n = int(rounding = 0.0/0.0) # throws CastError(NotRepresentable)   (NaN)
int n = int(rounding = 1.0/0.0) # throws CastError(Overflow)           (+∞)
int n = int(rounding = 1e30)    # throws CastError(Overflow)           (doesn't fit int64)
```

(The mode label resolves the fraction — total; the range edges throw, uniform with
integer narrowing. A *saturating* range policy for float→int is a possible later
addition, not part of this design.)

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
- **`int(float)` → §3.5** (rounding mode is a REQUIRED named arg — no bare `int(f)`; range edges throw).
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
   narrowing `as` → the throwing constructor; `float as int` → a mode-labeled
   constructor (`int(truncating = f)` / `int(rounding = f)` / …, which throws on
   the un-representable range edges). Per CLAUDE.md core
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

### 7.3 The 3 forks — RESOLVED (owner-decided 2026-06-20)
1. **Overflow → THROW (recoverable), not panic, not saturate.** A narrowing
   `T(x)` overflow throws `CastError(Overflow)`; the user can recover. This moves
   Gorget OFF the strict overflow-panics stance (`language-design.md:191/1298`).
   ⚠ **OPEN SUB-DECISION (scope) → now has its own doc:
   [`error-model.md`](error-model.md).** Should *arithmetic* overflow (`a + b`)
   also be recoverable? **Resolution under the error model: YES, but as a FAULT,
   not a contract error.** `byte(x)` is a **contract error** (validating external
   data → typed, mandatory-handle, on the API surface); `a + b` overflow is a
   **fault** (a bug/wrong-width → typed, auto-propagate, recover at a task/request
   boundary, OFF the API surface, default-abort). Both recoverable, both typed;
   they differ by KIND, not by an arbitrary panic-vs-throw split. This dissolves
   "why does `byte(x)` throw while `a+b` panics?" — see `error-model.md` §3 (the
   impossibility argument: recoverable-default-overflow + informative-row +
   universal-typed = pick two) and §7. Recorded for the cast: **conversion
   overflow throws (contract error).**
2. **Constructor mechanism → option (c): `T(x)` is the SURFACE; the conversion
   dispatch survives UNDER THE HOOD** (the `From` registry + `is_safe_integer_widening`).
   A 1-arg ctor `Self(T)` auto-registers the internal "from T" (no user-written
   trait — `From`/`Into`/`TryFrom` vanish from the *surface*). **Flavors are
   NAMED-ARGUMENT constructors** (§3.3): `byte(clamping = x)`, `byte(truncating = x)`,
   `int(rounding = f)` — the type is the callee, the mode is a static argument
   LABEL selecting a distinct (total) overload, exactly Swift's `UInt8(clamping:)`.

   ⚠ **CORRECTION 2026-06-22 (owner).** An earlier pass here moved the flavors to
   SOURCE METHODS with a type argument (`x.clamped(byte)` / `x.truncated(byte)` /
   `x.rounded(int)`) to "avoid argument-dependent throws-ness." That was a category
   error: a runtime `mode` PARAMETER (`byte(x, mode = runtimeVar)`) genuinely makes
   throws-ness undecidable and stays banned — but a compile-time argument LABEL
   (`clamping =`) is not a value; it selects a statically-resolved overload whose
   throws-ness is known at compile time. The method form is also the one that put a
   TYPE in value-argument position (`x.truncated(int)`), which the language doesn't
   otherwise allow. **Resolution: named-arg construction (§3.3) stands; `T(x)` is the
   single constructor, the labeled forms are its total siblings. The "§3.3 must be
   revised → source methods" instruction is WITHDRAWN.**

   Builtin numeric conversions are compiler-native (`int(<any numeric>)`,
   throws-ness from the typed widening table) — **no user overloading**; only USER
   types convertible from multiple sources use the (internal) multi-impl dispatch
   that `From[T]` already provides.
3. **`str(x)` stays rejected; the value→String conversion is `String(T)`.** `str`
   was never a type; `String` is. `String(x)` is the type's constructor doing the
   conversion — the constructor SPELLING of `Displayable.display()` (infallible →
   non-`throws`), uniform with `int(x)`. `f"{x}"` / `x.display()` / `String(x)` are
   ONE mechanism (`Displayable`), three syntaxes — so "one obvious way" holds;
   `String(x)` is the canonical conversion spelling, f-strings the interpolation
   ergonomic. (The 2026-06-10 `str(x)` rejection is preserved.)

**Bottom line:** the design is worth doing and the backbone (O1) + interim
machinery are real, but §6 under-scopes it (from-scratch fallible-conversion
subsystem + a new constructor-effect/overload facility), and forks 1–3 need owner
calls before a scout/brief. Until then this stays a DRAFT.

### 7.4 KNOWN SEAMS — honest caveats to carry into implementation
Two places where "just write `T(x)`, the type rule reports failability" does NOT
hold cleanly. Neither is fatal; both are "document + specify the edge," not
"the design collapses." Record them so they aren't discovered mid-build.

**Seam A — float→int has no clean "exact" default.** `int(3.7)` is lossy in a
DIFFERENT way than integer narrowing: it's not range overflow, it's fractional
loss. If `int(aFloat)` "throws when not representable," then *almost every* float
throws (3.7, 0.1, …) — miserable, and it pushes every float→int through a
handler. If it truncates silently, we've reintroduced the silent-lossy default we
banned. **Resolution (2026-06-22):** float→int has **no bare `int(f)` form** — the
user MUST pick the rounding mode as a REQUIRED named argument on the constructor:
`int(truncating = f)` (toward zero) / `int(rounding = f)` / `int(flooring = f)` /
`int(ceiling = f)`. Bare `int(f)` is a TYPECHECK ERROR with a fix-it listing the
modes. The label resolves the fraction (total); range edges (NaN/∞/out-of-`int64`)
throw, uniform with integer narrowing. (Swift's `Int(3.7)` truncates-by-default —
we deliberately diverge: no silent lossy default, ever.) So the spelling stays
**construction throughout** — the type is always the callee, the mode is a static
label; nothing lands in type-as-value-argument position. The "one rule" is:
*integer→integer* narrows-throws/widens-total; *float→integer* requires a mode
label (throws on range); *→float* widens-total. Still typed, still no name-match.

**Seam B — generics RELOCATE the From/TryFrom distinction, they don't delete it.**
In monomorphized code `T(x)`'s effect is statically known (the `(src,tgt)` pair is
concrete → `is_safe_integer_widening` decides). But to TYPECHECK *generic* code
over a numeric/convertible `T`, the checker must know whether `T(x)` throws BEFORE
monomorphization — which forces a **bound** that carries the effect:
`[T: WidensFrom[U]]` (the conversion is total) vs `[T: ConvertsFrom[U]]` (it may
throw). That is the `From`/`TryFrom` distinction surviving *in bound form*. So the
RFC's "delete the two traits" is true **at the call site** (the common case — a
real win) but NOT end-to-end: the two-way distinction reappears the moment you
write generic conversion code, just relocated from `impl`-surface to bound-surface.
Rust/Swift have the same seam (you pick `From` vs `TryFrom` at the bound); the
difference is they're honest about it up front and C hides it until generics.
**Resolution:** keep ONE surface trait name for the bound — `From[T]` — and let
the bound additionally constrain the effect (`[T: From[U]]` allows throwing
conversion in the body; a `total`-qualified bound or a separate `WidensFrom`
demands the non-throwing one). Spec the exact bound spelling during the scout; do
not pretend the distinction is gone.

## 8. CROSS-LANGUAGE SURVEY & THE SYNTHESIS (why From stays, Into/Try* dissolve)

The owner's instinct — "keep `From[T]`, but `Into[T]` and the `Try*` forms feel
like too much" — is **correct, and there's a precise reason.** A 9-language survey:

| Lang | Spelling | Default narrowing | Recover | Lossy/total | Extensible? |
|------|----------|-------------------|---------|-------------|-------------|
| **Python** | `int(x)` ctor | **raises** ValueError | try/except | (bignum: no overflow) | dunders `__int__`/`__index__` |
| **Go** | `T(x)` syntax | **silent truncate** ⚠ | — | silent | no (write a func) |
| **Rust** | `as` / `.into()` / `try_from()?` | `as`=silent ⚠ / TryFrom=`Result` | `?`/match | `as` silent / methods | `From`/`TryFrom` traits |
| **Swift** | `Int8(x)` ctor | **traps** | `Int8(exactly:)`→`?` | labeled inits | failable/throwing inits |
| **C#** | `(T)x` / op | `checked`=throws, `unchecked`=trunc | `TryParse(out)` ⚠ | `implicit`/`explicit` op | conversion operators |
| **Kotlin** | `x.toByte()` method | **silent truncate** ⚠ | `.toIntOrNull()` | silent | extension fns |
| **Scala** | `x.toInt` method | silent | `.toIntOption` | `implicit def` ⚠(gated) | implicits (cautionary) |
| **Ada** | `Integer(X)` ctor | **raises** Constraint_Error | exception handler | range-checked | type conversions |
| **Haskell** | `fromIntegral` | **silent wrap** ⚠ | — | one footgun fn | `Integral` class |

**Four lessons fall straight out:**

1. **Conversion-as-construction is the MAINSTREAM-GOOD model, not exotic.** Python,
   Go, Swift, C#, Ada all spell conversion as `T(x)`. Gorget is squarely in proven
   company. **Ada (1983) is the deep pedigree:** `Integer(X)` raising
   `Constraint_Error` on out-of-range is *exactly* "conversion = construction +
   throws on overflow," shipping for 40 years. Throws-on-narrow is not a gamble.

2. **The silent-lossy DEFAULT is the universal footgun** — Go `T(x)`, Rust `as`,
   Kotlin `.toByte()`, Haskell `fromIntegral`, Go's `string(int)`. Every language
   that made the *default* conversion silently lossy bolted on a warning afterward
   (go vet, clippy, Scala gating implicits). **C's "no silent lossy default; lossy
   is a NAMED method" is the distilled lesson-learned, backed by 5 languages' scar
   tissue.** This is C's strongest *safety* claim.

3. **The safety distinction (lossless/lossy ≡ widening/narrowing) keeps getting
   encoded — the only question is WHERE.** C#: `implicit`/`explicit` keyword on the
   operator (author-declared). Python: `__index__` (lossless) vs `__int__`
   (coercing) — separate protocols. Swift: failable `init?` vs plain. **Gorget's
   gorgeous move: for builtins, DERIVE it from the numeric lattice
   (`is_safe_integer_widening`); for user types, the author's existing `throws`
   annotation on the constructor IS the implicit/explicit distinction.** `MyType(Foo)`
   = lossless/implicit (C#); `MyType(Foo) throws` = lossy/explicit (C#). **No new
   keyword** — `throws` already carries it. That is the single most elegant point in
   the whole design.

4. **Implicit/automatic coercion (`Into` auto-bounds, Scala implicits, C# implicit
   ops) is contentious-to-harmful.** Scala 3 GATED implicit conversions behind a
   language import after years of pain. This is direct ammunition for dropping
   `Into[T]` — its only real value is ergonomic auto-coercion at call sites, which
   is precisely the implicit magic that bites.

### 8.1 So: From stays, Into/TryFrom/TryInto/`as` DISSOLVE — and exactly why
The decisive realization: **`TryFrom` exists ONLY because Rust's `From` is
hardcoded-infallible** (`fn from(T) -> Self`, no failure channel). Gorget's
constructors can be `throws`. So the moment a constructor can throw, the
fallible/infallible split stops needing two traits:

| Rust | What it really is | Gorget |
|------|-------------------|--------|
| `From[T]` (infallible) | "build Self from T" | **non-throws ctor `Self(T)`** ✅ keep the capability/name |
| `TryFrom[T]` (fallible) | "From, but Rust's From can't fail" | **`throws` ctor `Self(T)`** — the `throws` IS the "Try" |
| `Into[T]` | `From` read backwards, for bounds | **deleted** (use `From` in the bound) |
| `TryInto[T]` | `TryFrom` read backwards | **deleted** |
| `as` (silent lossy) | unchecked truncation | **deleted** (lossy = named-arg ctor `T(truncating = x)` / `T(clamping = x)`) |

**Five conversion concepts → ONE constructor form + the `throws` effect we already
have.** `From[T]` survives only as the *name of the capability* a 1-arg
constructor grants (and the bound-position name, per Seam B); `Into`/`Try*`/`as`
genuinely evaporate. The owner's "Into and Try* are too much" is exactly right —
**Into is From-backwards (redundant), and Try* is a workaround for a limitation
Gorget doesn't have.** Crediting them less is the correct judgment, not a
knowledge gap.

### 8.2 The "fast" leg (the owner asked for fast too)
Conversion-as-construction here is **as fast as Rust, with Python's ergonomics** —
a combination you rarely get:
- The widening/narrowing decision is **static** (compile-time, from the type
  lattice) → widening = a sign/zero-extend (free); narrowing-throws = ONE
  compare + a predictable branch to the throw path; lossy methods = a truncate
  (free). No dynamic dispatch.
- The From-dispatch for user types is **monomorphized** (resolved at compile time,
  no vtable) — same cost model as Rust's `TryFrom`.
- Contrast Python's `int(x)`: same lovely spelling, but DYNAMIC (type lookup +
  `__int__` dispatch at runtime). C gets Python's surface with Rust's codegen.

**So: gorgeous (one spelling), ergonomic (construction, the model millions already
know from Python/Go/Swift), fast (static + monomorphized, zero-overhead widening),
safe (no silent lossy default — the 5-language lesson — + recoverable narrowing
via `throws`, the Ada/Python pedigree). Yes — we are moving in that direction;
this section is the evidence that the destination is right.**
