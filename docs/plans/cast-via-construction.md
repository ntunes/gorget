# RFC — Conversion via construction (retiring `as`, `From`/`Into`/`TryFrom`)

> **Status:** DRAFT / design discussion (owner-initiated 2026-06-20). Not yet
> approved; no implementation. Supersedes the ad-hoc `as` operator and the
> `From`/`Into`/`TryFrom` trait trio for the conversion use case.
>
> **Core decision (v2):** fallibility is the **`throws` effect**, not a return
> type. A conversion constructor returns `Self` and *throws* `CastError`; the
> compiler **elides the throw** when it can prove the conversion can't fail.

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

1. **The compiler elides the throw when it can prove the conversion can't fail.**
   Widening (`float(int)`, `long(int)`), and narrowings the value is
   `meta`/literal/range-provably within — these are infallible, return the value
   directly, and are callable from a non-`throws` function with zero ceremony.
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
| `T(x)` | returns `T`, **throws** Overflow (elided if provably fits) | returns `int`, truncates fraction, **throws** on NaN/∞/out-of-range | infallible, no `throws` |
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
- The `?` / `throws` **error-widening** calls the target error type's constructor
  (`AppError(io_err)`) instead of `From::from`.

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

**Still open:**

1. **Throw-site vs compile-site enforcement (the one real knob).** This RFC
   defaults to "throws, handle it (or use a flavor)," with elision keeping the
   provably-safe casts ceremony-free. The alternative is Zig-style: make a
   non-provable narrowing a **compile error** that forces you to pick
   `clamping`/`truncating`/`catch` *syntactically*. Trade-off: `throws` is more
   ergonomic and uniform; compile-forcing is louder and non-viral. Default:
   throws+elision. Revisit if the viral-`throws` property bites in practice.
2. **Elision precision.** How far does "provably can't fail" reach — literals and
   `meta` constants for sure, but also value-range analysis (`x % 256` → fits a
   `byte`)? Spec needs to pin the guaranteed-elided set so the ergonomics are
   predictable.
3. **Marking a user conversion.** A user type defines `Self(T)` or
   `Self(T) throws`. Do we need a marker so a 1-arg constructor is recognized as
   *the* conversion from `T` (for `?`-widening discovery / tooling), or is "any
   1-arg constructor whose param is `T`" enough? Lean: the latter.
4. **Retire `as` entirely?** Recommendation: **yes** — `int(x)` is clearer and
   uniform; keeping `as` re-introduces the inconsistency this RFC removes.

## 6. Migration

A **both-compilers, language-surface** change (Rust gg + self-host + spec + book
+ negative fixtures), staged:

1. **Spec first** (this doc → `docs/language-design.md` + `docs/book` once
   settled). Ratify §5.1 (resolved) and the §5.2 elision-precision spec — they
   gate the ergonomics.
2. **Numeric constructors + flavors + `CastError` + throw-elision** in both
   compilers (the elision analysis is the substantive new compiler piece).
3. **Deprecate then remove `as`.** Lossy `as` → its throwing constructor
   (`float as int` → `int(f)`, which throws on the un-representable edges and is
   elided when provably finite/in-range); lossless `as` → the infallible
   constructor. Per CLAUDE.md core invariant #8, the outcome is that a lossy
   conversion can no longer happen *silently* — it's either elided-safe,
   handled, or a flavor — in **both** compilers, with negative fixtures.
4. **Fold `From`/`TryFrom` into constructors; delete `Into`;** rewire `?`/`throws`
   error-widening to the target constructor.

### Interaction with the in-flight `as` fix (parity track #1)

A parity track currently makes the **self-host's `as` actually convert** (it's a
no-op stub emitting garbage today). Two coherent paths:

- **Interim (A):** land that fix so self-host `as` matches Rust gg's truncating
  `as` (+3 parity, removes garbage), and treat this RFC as the replacement. Risk:
  briefly cements the silent-lossy behavior we plan to retire.
- **Direct (B):** skip the truncate fix; start §6.2/§6.3 — numeric constructors
  with throw-elision + reject lossy `as` — in both compilers. Bigger, but it's
  the reference-grade target and avoids shipping-then-reverting.

Owner's call. (The const-read parity track is independent and proceeds
regardless.)
