# Cast-via-construction — First-Increment Implementation Scout (read-only)

> **Status:** read-only design scout (2026-06-22). Grounds the owner-approved
> RFC (`cast-via-construction.md` §7/§8) against CURRENT source. NOT an
> implementation; NOT approved. Feeds the brief → ≥3 reviews → launch cycle.
> All `file:line` re-verified against the worktree this session.

## 0. Re-verification of the RFC's load-bearing premises (some line numbers drifted)

| RFC claim | RFC cite | Verified location | Verdict |
|---|---|---|---|
| `is_safe_integer_widening` decides widen-vs-narrow from the typed `(src,tgt)` pair | `typecheck.rs:159-181` | `typecheck.rs:162-181` | ✅ EXACT |
| `Expr::As` lowers to `Instruction::Cast` | (GICast machinery) | `exprs/mod.rs:446-480` (`builder.cast`) | ✅ |
| LIR cast ops `IntCast`/`FloatCast`/`IntToFloat`/`FloatToInt` | `insts.rs:213-407` | defs `lir/mod.rs:873-879`; emit `insts.rs:466-498` | ✅ (the 213-407 range is the broader Cast arm 306-500) |
| Name-matched `emit_name=="int"` LIR special-case | `insts.rs:3896` | **STALE** — real site is `insts.rs:4059-4144` (Tier 3c) | ⚠ cite drift |
| Only `int`/`float`/`bool` lower (to I64/F64/Bool); `byte`/`int8..64`/`uint*`/`float32/64`/`str` rejected | `typecheck.rs:1391-1422` | `typecheck.rs:1402-1421` (`UnloweredBuiltinCall`) + `insts.rs:4072` match has only `"float"/"int"/"bool"` arms | ✅ EXACT |
| `lookup_from_conversion` | `typecheck.rs:4493` | `typecheck.rs:4574-4589` | ⚠ drift, present |
| Lowering name-matches `_for_<E>__from` suffix | `exprs/mod.rs:3092` | `exprs/mod.rs:3120` (`format!("_for_{caller_name}__from")`, filter `fn_sigs` keys) | ✅ |
| `@derive(From)` generates `T from(U v): return T(v)` | `derive.rs:674` | `derive.rs:674-682` | ✅ EXACT |
| `Into` not registered; `From`/`TryFrom` are; `Numeric` real | `traits.rs:413/816` | `From` `traits.rs:762`; `TryFrom` `:773`; `Numeric` `:818`; **no `Into`/`TryInto` entry** | ✅ EXACT |

**Two NEW findings the RFC did not flag (decisive for slicing):**

1. **`FloatToInt` is SATURATING TODAY, in BOTH backends, with full range
   detection already written.** C: `c_lir/mod.rs:2669-2698` — a per-target
   ternary (NaN→0, `≥upper`→TYPE_MAX, `<lower`→TYPE_MIN) with exact bounds for
   I8/I16/I32/I64/U8/U16/U32/U64. LLVM: `llvm/mod.rs:3850-3867` —
   `@llvm.fptosi.sat`/`fptoui.sat`. **The overflow DETECTION the RFC's
   narrowing-throws needs already exists at these sites; today it saturates
   where the RFC wants it to THROW.** The change is "branch to throw" instead of
   "select clamped value," not greenfield range logic.

2. **Integer narrowing (`IntCast`) has NO range check at all** — C emits a raw
   `(t)(val)` truncation (silent wrap). So `byte(i64)` silently truncates today.
   There is **no const-narrowing compile check anywhere** (`errors.rs:830`'s
   out-of-range message is for `enum_from_ordinal`/meta only, not cast). RFC §7.2
   "const-narrowing is NEW logic" → confirmed; it's from scratch.

## 1. The first implementable increment (recommended slice)

### Slice 1 — **`int(float)` retires to the rounding-mode source methods (Seam A), + the bare-`int(f)` typecheck error.**

**Why this is the cleanest first slice (not numeric narrowing, not `as` removal):**

- It is **self-contained and independently shippable**: it touches exactly ONE
  conversion pair (float→int), needs NO new `CastError` enum, NO `throws`-ctor
  facility, NO From-registry rewire, and NO error-model dependency.
- It **deletes a live silent-lossy default** (`int(3.7)` silently truncates today
  via `FloatToInt`) — the RFC's headline safety win — and replaces it with the
  Seam-A resolution (`x.truncated(int)` / `.rounded(int)` / `.floored(int)` /
  `.ceiled(int)`; bare `int(f)` = typecheck error with a fix-it). This is the one
  sub-decision the RFC already fully specified (§7.4 Seam A) so there is no open
  design question blocking it.
- It **exercises the whole pipeline thinly** (typecheck reject + method dispatch +
  LIR cast-op reuse) — a vertical slice that de-risks the larger narrowing work,
  while being valuable on its own.
- The source methods lower through the **existing** `FloatToInt`/IntCast ops with
  the right rounding pre-step (truncate = current `FloatToInt`; round/floor/ceil =
  a `gorget_f64_round`/`floor`/`ceil` extern then `FloatToInt`). No new backend op.

**Passes touched (both backends):**

| Layer | File:line | Change |
|---|---|---|
| Typecheck | `typecheck.rs:1402-1421` | Add `int`/`float64`/… float→int bare-call to the rejected set WHEN the sole arg is float-typed; emit a NEW teaching error with a fix-it listing `.truncated/.rounded/.floored/.ceiled`. (Today `int` is NOT in the rejected set — it lowers.) |
| Method registry | `traits.rs` / builtin-method table | Register `truncated(T)`/`rounded(T)`/`floored(T)`/`ceiled(T)` as builtin methods on `float`/`float32/64`, returning the named integer type. |
| GIR lowering | new arm near `exprs/mod.rs` method-call lowering | Route the 4 methods to a `Cast` (truncated) or round-extern-then-`Cast`. |
| LIR | reuse `insts.rs:466-498` `FloatToInt` | No new op; round/floor/ceil add a `gorget_f64_*` extern before the cast. |
| C runtime | `backend/c/` | `gorget_f64_round/floor/ceil` if not present (likely present — `round()`/`floor()`/`ceil()` from libm). |
| Spec | `language-design.md`, `book/02-types.md:124-128` | Replace `int n = 3.14 as int` with the method spelling; document the bare-`int(f)` rejection. |
| Fixtures | `tests/fixtures/` | NEW negative fixture: bare `int(aFloat)` → typecheck error. NEW positive: `f.truncated(int)`/`.rounded(int)` round-trip with deterministic stdout. |

**Migration touched by Slice 1:** every `<float> as int` / `int(<float>)` in the
corpus + self-host (the dominant `as int` = 35 corpus / 28 self-host sites — see
§5; not all are float-sourced, only the float ones migrate here).

### Why NOT lead with the alternatives

- **Numeric integer-narrowing-throws (`byte(i64)`)** is the RFC's core but it is
  the BIG slice: it needs the `CastError` enum, a **throwing-constructor /
  constructor-effect facility** (today no ctor carries `throws` for builtins),
  the const-narrowing compile check (greenfield), AND the saturate→throw flip in
  both backends. Land it as Slice 2 once Slice 1 has proven the method+typecheck
  plumbing and the `CastError` enum is in.
- **`as`-removal** must come AFTER the constructor forms exist (you can't retire
  `x as int` until `int(x)`/`x.truncated(int)` cover every case) — it's a
  late-stage slice (Slice 4), and it must be scoped to the cast `as_expr` ONLY
  (RFC §7.2: `with X as Y`, `import X as Y` survive — 54 such sites in corpus).

## 2. Full staging sequence (each increment independently shippable + reviewable)

> Spec-delta lands WITH each slice, not up front (the RFC §6.1 "spec first" is
> satisfied incrementally; the per-type widening/narrowing table is small enough
> to write as Slice 2 lands).

- **S0 — `CastError` enum + `Displayable`-spelling `String(T)` (prep, no behavior flip).**
  Add the `enum CastError { Overflow, Underflow, NotRepresentable, Parse(ParseError) }`
  (RFC §3.4) to the prelude. Add `String(T)` as the constructor spelling of
  `Displayable.display()` (RFC §7.3) — pure additive, `f"{x}"`/`.display()` already
  exist. Independently shippable; unblocks S1/S2's error channel.
- **S1 — float→int = source methods + bare-`int(f)` reject (the recommended first slice, §1).**
  No throws yet (truncated/floored/ceiled/rounded are total).
- **S2 — integer narrowing throws `CastError(Overflow)` + widening total + const-narrow compile check.**
  The core. `byte(i64)` becomes a throwing builtin ctor; `is_safe_integer_widening`
  (`typecheck.rs:162`) drives throws-ness; saturate→throw flip in both backends
  (`c_lir/mod.rs:2692` ternary, `insts.rs` IntCast); `meta` const-narrow rejects
  `byte(300)`. Lower `byte/int8..64/uint*/float32/64` (today `UnloweredBuiltinCall`).
- **S3 — total flavor methods for integer narrowing (`x.clamped(byte)`/`.truncated(byte)`).**
  RFC §7.3 fork 2: flavors are SOURCE METHODS, not named-arg ctors. Total, non-throwing.
- **S4 — retire `as` (cast `as_expr` only).** Widening `as` → total ctor; narrowing/
  float→int `as` → throwing ctor / method. Negative fixtures that `<lossy> as <T>` no
  longer silently truncates. Keep `with…as`/`import…as` (54 corpus + 2 import sites).
- **S5 — fold `From`/`TryFrom` into constructors; delete `Into` (no-op); rewire
  error-widening.** `@derive(From)` → the newtype's generated ctor (`derive.rs:674`,
  add `!value` move per §7.2); error-widening typecheck (`typecheck.rs:4663`
  `from_conversions`) + lowering name-match (`exprs/mod.rs:3120` `_for_<E>__from`)
  re-point to constructor resolution; `TryFrom` derive (`derive.rs:684`) → throwing ctor.
- **S6 — generics bound-form (Seam B).** `[T: From[U]]` bound carries the
  conversion-may-throw effect; a `total`/`WidensFrom` bound demands non-throwing.
  Spec the exact spelling here (open — §3 below).

Each of S1–S6 is reviewable in isolation; S0 is prep that S1/S2 depend on.

## 3. Open owner-decisions (recommendations, do NOT decide)

1. **`String(T)` vs the existing `Parseable`/`int.parse` (§7.2 reconcile).**
   `int(String) throws CastError(Parse)` would be a THIRD String→numeric spelling
   beside `Parseable.parse` (`traits.rs:794`, Option-returning) and `parse_int`.
   *Recommendation:* keep `String→numeric` parsing as `Parseable` (Option-channel,
   never-throws) and make `int(String)` an ALIAS that throws `CastError(Parse)` on
   the None case — one mechanism, two ergonomics — OR defer `int(String)` out of the
   cast track entirely (it's parsing, not numeric narrowing). Lean: **defer**; the
   cast track is numeric-pair conversion. String parsing is its own RFC.

2. **`@derive(From)` move-vs-clone (§7.2).** The generated ctor body is bare
   `return T(value)` (`derive.rs:674-681`) → CLONE under CoW. RFC §7.2 says write
   `!value`. *Recommendation:* make the generated body `return T(!value)` as part of
   S5 — but confirm the newtype field-init move is sound for resource fields (it is,
   per the CoW consuming-position rule). Low risk; flagged because it changes
   generated-code semantics.

3. **Seam-B bound spelling (`[T: From[U]]` vs `[T: WidensFrom[U]]` + a `total`
   qualifier).** Two-way distinction (may-throw vs total) must reappear at the bound
   (RFC §7.4). *Recommendation:* ONE surface name `From[U]` (may-throw in body) + a
   `total`-qualified form for the non-throwing demand, rather than a second trait
   name — fewer names, matches §7.4's lean. But this is a genuine design fork; punt
   to the S6 brief.

4. **Does `bool(x)` stay?** It currently lowers (`insts.rs:4131`, truthiness cast).
   The RFC numeric story is int/float; `bool(x)` truthiness is arguably a different
   concept (not a widen/narrow). *Recommendation:* leave `bool(x)` as-is (out of the
   cast-redesign scope) unless the owner wants `bool(int)` reframed.

5. **`char` as a cast target.** Corpus has `as char` (2 sites). `char` is not in the
   PrimitiveType narrowing lattice. *Recommendation:* the owner should say whether
   `char(int)`/`int(char)` are in-scope for S2 or handled as a codepoint conversion
   (today `int(String)`→`gorget_str_ord`). Flag, don't decide.

## 4. Interaction with the error-model (the key decoupling answer)

**The first cast slice (S1), and even the core narrowing slice (S2), can land
INDEPENDENTLY of the error-model phases.** Evidence:

- The cast's narrowing-overflow is a **CONTRACT error**, not a fault
  (`error-model.md:281-282`: "`byte(x)` validates … against a contract → contract
  error, typed, mandatory-handle. ✅ exactly the cast RFC's decision"). Faults
  (arithmetic `a+b` overflow, bounds, div0) are the SEPARATE kind that the
  error-model Phase 1/2 builds.
- `throws E` is **already** sugar for `Result[T,E]`, declared and propagated via
  the existing `?`/`rethrow`/`on error` machinery (`error-model.md:559-560`,
  `book/09-option-result.md:328`) — UNCHANGED by error-model Phase 1
  (`error-model.md:558-564`). So `CastError` flows through the *existing* throws
  pipeline (the same `from_conversions`/auto-prop machinery at `typecheck.rs:4663`
  / `exprs/mod.rs:3120`). No new fault-unwind leg is needed.
- The error-model's greenfield work (the fault enum + local catch + Phase-2
  unwinding, `error-model.md:565-576`) is for `a+b`/bounds/div0 — **orthogonal to
  cast**. `error-model.md:288-290` confirms the cast decision is met by the contract
  channel, not the fault channel.

**Conclusion: cast does NOT need error-model Phase 2 first.** S0–S5 use only the
existing `throws`=`Result` contract machinery. The ONLY soft coupling: both RFCs
add a typed error enum to the prelude, and both should agree `CastError(Overflow)`
(contract) is distinct from a `Fault.Overflow` (arithmetic) — a naming-coordination
note, not a build dependency. Recommend the cast track proceed and simply name its
enum `CastError` (already specified, §3.4) to avoid collision.

## 5. Migration scope (blast radius — counts regenerated this session)

Commands run this session (grep over the worktree):

- **`as`-cast, corpus (`tests/fixtures/*.gg`), primitive/numeric targets:** **77**
  sites (the real migration set). Breakdown of the common ones:
  `as int` 35 · `as float` 22 · `as uint8` 11 · `as bool` 3 · `as String` 3 ·
  `as char` 2 · `as int8` 1. (The earlier "877 / 784-other" figure was inflated by
  prose and `with…as` false-positives; the clean primitive-target count is 77.)
- **`as`-cast, self-host (`self_host_*`), numeric targets:** **31** sites,
  `as int` dominant (~28). (The "594" raw figure caught comment prose:
  "as Rust", "as Phase", "as EAs", "as Type" — not casts.)
- **ptr/ref reinterpret `as X&`:** **0** in corpus — the unsafe-ptr-cast carve-out
  (RFC §7.2) has no corpus sites to migrate (still must keep the keyword path).
- **`From` impls / derives, corpus:** **6 fixture files** with `with From[` /
  `@derive(From)` / `with TryFrom[`. Representative: `from_trait.gg`,
  `from_trait_multi.gg`, `try_from_trait.gg`, `snag11_from_mediated_propagation.gg`,
  `derive_from_multi_field_error.gg`.
- **`From`/`TryFrom` impls, self-host:** **5** sites (the showcase — migrate to ctor
  form in S5; `derive.gg` is the generator).
- **`Into[` / `.into()` / `TryInto[` / `.try_into()`:** **0** corpus sites — RFC §7.2
  "Into deletion is a no-op" CONFIRMED (not in `traits.rs` registry either).
  `TryFrom[`: 4 sites (minimal).
- **Non-cast `as` that MUST survive (`with…as` / `import…as`):** **54** corpus sites
  — the keyword cannot be deleted; S4 scopes removal to `as_expr` only.

**Retirement blast radius tally:** cast-`as` corpus ≈ **77**, cast-`as` self-host ≈
**31**, `From` impl sites ≈ **6 corpus + 5 self-host = 11**, `Into`/`TryInto` = **0**,
`TryFrom` = **4**. The dominant, mechanical migration is `as int`/`as float`
(float→int handled by S1's methods; int→float widening by S2's total ctor).

## 6. Citations

RFC: `docs/plans/cast-via-construction.md` §3.2/§3.3 (throws model + flavors),
§3.4 (CastError), §4 (trait deletions), §6 (staging + interim-`as` interaction),
§7.1 (backbone), §7.2 (corrections — `as`-scope, typed-dest, Into-no-op,
`!`-move, Parseable, two-layer rewire, const-narrow NEW, `T(x)` rejected today),
§7.3 (forks — overflow→throw, flavors=methods, `String(T)`), §7.4 (Seam A float→int
methods, Seam B generics bound-form), §8.1 (From-stays/Into-Try-dissolve).

Error-model: `docs/plans/error-model.md` §7 (contract vs fault for cast, :276-290),
§11.0 (Phase-1 scope boundary, throws=Result unchanged, :557-564).

Code: `typecheck.rs:162-181` (widening lattice), `:1402-1421` (builtin-cast
reject), `:2901-2937` (`Expr::As` typecheck), `:4574-4589` (`lookup_from_conversion`),
`:4663` (`from_conversions` record); `exprs/mod.rs:446-480` (`Expr::As`→Cast),
`:3120` (`_for_<E>__from` name-match); `insts.rs:466-498` (numeric-cast op select),
`:4059-4144` (Tier-3c builtin cast-call — the real `int(x)` site, NOT 3896);
`lir/mod.rs:873-879` (cast op defs); `traits.rs:762/773/818` (From/TryFrom/Numeric;
no Into); `derive.rs:674-682` (`@derive(From)` gen), `:684-692` (TryFrom gen);
`c_lir/mod.rs:2669-2698` (FloatToInt SATURATE — the detection to flip to throw);
`llvm/mod.rs:3850-3867` (LLVM saturate); `book/02-types.md:124-128` (`as` docs).
