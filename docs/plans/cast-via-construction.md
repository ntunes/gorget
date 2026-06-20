# RFC — Conversion via construction (retiring `as`, `From`/`Into`/`TryFrom`)

> **Status:** DRAFT / design discussion (owner-initiated 2026-06-20). Not yet
> approved; no implementation. Supersedes the ad-hoc `as` operator and the
> `From`/`Into`/`TryFrom` trait trio for the conversion use case.

## 1. The problem

Gorget converts values two incompatible ways:

- **Numerics** use `as` — `int x = 3.7 as int`. It is **silently lossy**
  (`300 as byte` → `44` with no signal), routes through no trait, and is the
  one conversion path that ignores the language's "no silent footguns" ethos.
  (`as` is also a known regret in Rust, the language it was borrowed from.)
- **Everything else** uses constructors (`Meters(3.0)`, `Celsius(...)`) plus the
  `From[T]` / `Into[T]` / `TryFrom[T]` traits.

So numerics are the *only* types that don't convert the way every other value is
built, and the trait trio (`From`/`Into`/`TryFrom`) is a parallel mechanism that
duplicates what constructors already express. The owner is "not convinced" by
`TryFrom`/`Into` — and rightly: they are ceremony that a constructor subsumes.

The language already rejects cross-type `as` (`String as int` is a compile
error → "use `String.to_int()`"). This RFC finishes that thought: **conversion
is construction**, uniformly, for builtins and user types alike.

## 2. The principle

> **Converting a value to type `T` is constructing a `T` from it.**

Gorget already builds every value as `T(args)` and already has the three pieces
this needs — `T(...)` constructors, **named arguments** (`Vector[int](alloc=pool)`),
and compile-time evaluation (`meta`). So "the Swift model" costs almost no new
surface; it mostly *deletes* `as` and the trait trio.

## 3. The design

### 3.1 The constructor is the conversion

```gorget
# widening — always lossless, plain constructor
float f = float(42)              # 42.0
long  l = long(small_int)

# float → int — one well-defined default (truncate toward zero), like Swift Int(3.7)
int n = int(3.7)                 # 3
```

### 3.2 Flavors are named arguments (Swift's labels = Gorget's named args)

Swift's `UInt8(clamping: x)` / `Int8(truncatingIfNeeded: x)` map one-to-one onto
Gorget's named arguments. The **label names the semantics** (also Zig's virtue:
you cannot lose data without saying so).

```gorget
byte b = byte(clamping   = 300)        # 255   — saturate
byte b = byte(truncating = 300)        # 44    — wrap / bit-truncate
int  n = int(rounding    = 3.7)        # 4     — round-to-nearest (default int(f) truncates)

# fallible form (see §3.4):
Result[byte, CastError] r = byte(checked = 300)   # Error(Overflow)
```

| Swift | Gorget |
|---|---|
| `Int8(x)` (traps) | `byte(x)` — compile error unless provably fits (§3.3) |
| `Int8(exactly: x) -> Int8?` | `byte(checked = x) -> Result[byte, CastError]` (§3.4) |
| `Int8(clamping: x)` | `byte(clamping = x)` |
| `Int8(truncatingIfNeeded: x)` | `byte(truncating = x)` |
| `Int8(bitPattern: x)` | `byte(bits = x)` — explicit reinterpret, never implicit |

### 3.3 The default (`byte(x)`): Swift ergonomics, **Zig safety**

Swift's bare `UInt8(x)` **traps at runtime** on overflow. Gorget leans on
*compile-time* errors (escape analysis, ownership, `meta assert`), so:

> The bare `byte(x)` is a **compile error** when the compiler cannot prove `x`
> fits, directing the programmer to `clamping=` / `truncating=` / `checked=`.

- **Widening** (`float(int)`, `long(int)`) — always allowed, lossless.
- **`int(float)`** — truncates toward zero (one obvious semantics); the NaN/±∞/
  out-of-range edge requires `checked=` (or saturates — open question §5.3).
- **Narrowing** (`byte(int)`, `i32(i64)`) — allowed bare only when the value is
  `meta`/range-provably in range; otherwise pick a flavor. No silent loss, **no
  surprise runtime trap** — strictly better than Swift here.

### 3.4 The fallible form returns `Result[Self, CastError]` (not `Option`)

The owner's question. **Result**, because:

1. **It composes with the error model.** Gorget propagates via `throws` +
   `From`-widening; a `Result[Self, CastError]` (or a throwing constructor, §3.5)
   flows through that and widens into the caller's error type *via the same
   constructor-based conversion this RFC adopts*. An `Option` can't propagate —
   you'd `.ok_or(...)` at every call site.
2. **It's a strict superset.** `r.ok()` recovers Swift's lightweight `Option`
   whenever you don't care *why*. The reverse forces you to invent an error.
3. **Continuity.** `TryFrom[T]` already returns `Result[Self, String]`; we keep
   Result and only upgrade the error to a **typed `CastError`**, not stringly:

```gorget
enum CastError:
    Overflow         # value too large for the target
    Underflow        # value too small (e.g. negative into unsigned)
    Inexact          # float → int lost a fraction (only under strict modes)
    Parse(ParseError)  # String → T: position + reason
```

(Swift's Optional is the right call when failure is *truly* info-free; `.ok()`
gives exactly that on demand, so Result dominates.)

### 3.5 Throwing constructors are the ergonomic surface

Because Gorget has `throws`, the failable conversion is most ergonomic as a
**throwing constructor** — explicit `Result` is the escape hatch:

```gorget
int parse_port(String s) throws PortError:
    int raw = int(s)          # int(String): throws CastError(Parse) -> widens to PortError via From/ctor
    byte hi = byte(checked = raw >> 8) ?     # propagate the cast error upward
    return raw

# explicit, no throws context:
match byte(checked = n):
    case Ok(b):    use(b)
    case Error(e): log(e)        # typed CastError, not a string
```

## 4. The payoff — one mechanism, three trait deletions

If conversion *is* construction:

- `From[T]`  → a constructor `Self(T)`.
- `TryFrom[T]` → a constructor returning `Result[Self, CastError]` (`checked=` / throwing).
- **`Into[T]` → deleted.** You write `int(x)`, never `x.into()`. (The mirror trait was the part that grated.)
- The `?` / `throws` **error-widening** calls the target error type's constructor
  (`AppError(io_err)`) instead of `From::from`.

So builtin numerics, newtype conversion (`Meters(3.0)`), and error widening all
go through **constructors** — `as`, `Into`, and `TryFrom` retire into it.

```gorget
# user newtype — already a constructor today; nothing special:
Celsius c = Celsius(fahrenheit = f)             # was: equip Celsius with From[Fahrenheit]
Result[Percentage, CastError] p = Percentage(checked = 120)   # was: TryFrom

# error widening at a boundary — the target error's constructor:
equip AppError:
    AppError(IoError e): ...        # was: equip AppError with From[IoError]
```

## 5. Open questions (the real design work)

1. **Generic bounds — the one place a capability is still needed.**
   `fn f[T](x: T)` that accepts "anything convertible to int" has no `Into[int]`
   bound anymore. Options: (a) a `Convert[Target]` *capability used only for
   bounds* (surface stays `int(x)`); (b) no implicit generic conversion — the
   caller converts at the call site. **Decide deliberately.** This is the only
   thing `Into` genuinely bought.
2. **The failable-constructor mechanism.** `byte(checked = x) -> Result[...]` has a
   different return type than `byte(x) -> byte`. Swift uses a *failable
   initializer* (`init?`). Gorget needs: allow a constructor overload to return
   `Result[Self, E]` (most faithful), or keep `checked=`/parsing as a
   distinguished form the compiler knows. Lean: failable constructor.
3. **`int(float)` edges:** NaN / ±∞ / out-of-`int64`-range. Truncate-and-pray
   (Swift-ish), saturate, or force `int(checked = f)`? Recommend: bare `int(f)`
   defined only for finite in-range; else `checked=`.
4. **User-defined conversions & coherence.** A user type defines `Self(T)` /
   `Self(checked = T)` constructors; do we need a marker so a 1-arg constructor
   is recognized as "the conversion from T" (for generic bounds / `?`-widening)?
5. **Does `as` survive at all?** Recommendation: **retire it.** `int(x)` is
   clearer and uniform; keeping `as` as a second spelling re-introduces the
   inconsistency this RFC removes.

## 6. Migration

This is a **both-compilers, language-surface** change (Rust gg + self-host +
spec + book + negative fixtures), so it is staged, not a single PR:

1. **Spec first** (this doc → `docs/language-design.md` + `docs/book` once
   settled). Resolve §5.1 (generic bounds) and §5.2 (failable constructor) — they
   gate everything.
2. **Numeric constructors + flavors** in both compilers; `meta`/range analysis
   for the provably-fits check.
3. **Deprecate then remove `as`** (lossy `as` → compile error pointing to a
   flavor; lossless `as` → the constructor). Per CLAUDE.md core invariant #8,
   the lossy-`as` rejection IS the reference-grade outcome — a typecheck error +
   negative fixtures in BOTH compilers.
4. **Fold `From`/`TryFrom` into constructors; delete `Into`;** rewire `?`/`throws`
   error-widening to the target constructor.

### Interaction with the in-flight `as` fix (parity track #1)

A parity track currently makes the **self-host's `as` actually convert** (it is a
no-op stub today, emitting garbage). Two coherent paths:

- **Interim (A):** land that fix so self-host `as` matches Rust gg's truncating
  `as` (a +3 parity win + removes garbage), and treat this RFC as the eventual
  replacement. Risk: it briefly cements the silent-lossy behavior we plan to
  reject.
- **Direct (B):** skip the truncate fix; implement lossless `as` only + **reject
  lossy `as`** (typecheck error + negative fixtures) as the first step of §6.3,
  in both compilers. Bigger, but it is the reference-grade target and avoids
  shipping-then-reverting.

Owner's call. (The const-read parity track is independent of this and proceeds
regardless.)
