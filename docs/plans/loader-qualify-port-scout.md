# Scout: loader-qualify port (collision-excluding variant map) — measured +0, parity-neutral

**Status:** PROTOTYPE complete, MEASURED end-to-end. Throwaway — DO NOT INTEGRATE.
**Bottom line — DO NOT LAND:** The collision-excluding map is the right reference-grade *design*
(mirrors Rust `build_variant_map_from_all`) and is **parity-neutral (+0, zero fixture flips across
1066)** — but as prototyped it **REGRESSES the bootstrap** (`self_host_bootstrap_fixed_point` FAILS):
the self-host mis-compiles the prototype's `Dict[String,int]`-keys-iteration code shape, so
stage-1.bin builds an empty variant index and can't compile itself. A single-dict-sentinel
reformulation (proven self-host-safe, §7) dodges that, but the yield stays **+0**. The two fixtures
the brief targeted (`string_enum_variants`, `string_error_handling`) are **NOT variant-collision
bugs** — they are **String-index-read-on-a-by-pointer-param** bugs, unrelated to variant resolution.
The genuine residual collision gap (`qual2`) is unblocked by the map but then re-blocked by a
**second independent gap** (forward-referenced callee param types unavailable at the caller's
arg-lowering site). Net: this is a hygiene/layering change at best, not a parity play; the real
yield in this area is the String-index bug (own scout).

---

## 1. Reproduce-before — the brief's premises RE-VERIFIED

### Premise A (brief): `string_enum_variants` is "a user `enum Message: Empty()` cross-enum collision"
**REFUTED.** Reproduced WRONG-OUTPUT today (`tokens: 16` vs oracle `tokens: 7`), but the root cause
is NOT a variant collision:
- The fixture uses **only qualified forms** (`Token.Punct(ch)`, `Message.Empty()`); the variant
  names (Word/Number/Punct/Whitespace/Text/Command/Empty) **do not collide across `Token`/`Message`**,
  and no lib enum in scope reuses them. Emitted C has exactly 5 enum-shaped structs
  (Token, Message, Option__Token, Option__Message, +1); no third enum to collide with.
- The actual bug: `String ch = input[i]` where `input` is a **String parameter passed by pointer**
  lowers the index-read to a **literal `0`** instead of `gorget_str_index`/`char_at`. Minimal repro
  (`min6.gg`):
  ```gorget
  void scan(String input):
      int i = 0
      while i < input.len():
          String ch = input[i]   # → emitted C: __v9 = (int64_t)0LL;  (should be gorget_str_index)
          print(f"ch={ch}")
          i = i + 1
  void main(): scan("ab")
  ```
  Oracle prints `ch=a / ch=b`; self-host prints `ch=0 / ch=0`. In the full fixture this cascades:
  `ch` is empty → `ch.is_alpha()` becomes `gorget_uint8_is_alpha(0)`, `ch == " "` fails, and every
  char falls into `Token.Punct((Str){0})` → `tokens: 16` Puncts with empty payloads. The
  `.Punct_0 = (Str){0}` "lost argument" symptom is downstream of the lost `ch`, not a ctor bug.

### Premise B (brief): `string_error_handling` is a collision case (CC-FAIL→CRASH)
**REFUTED (same family).** Reproduced CRASH: `panic: string byte index out of bounds: index 0,
byte length 0`. Same `String[i]` / byte-index family. Not a variant collision.

### Premise C (brief): "the interim does NOT fix the qualified `EnumName.Variant()` form"
**REFUTED — the qualified form already works.** `qual3.gg` (`describe(RxNode.Empty())`) →
**MATCH** today. The qualified key path in `lir_codegen.gg` (interim) resolves it correctly.

### Premise D (the GENUINE gap — confirmed): bare collision as a forward-referenced call arg
`qual2.gg` — a bare `Empty()` whose enclosing expected-type isn't anchored at the call site:
```gorget
from std.conv import parse_int      # transitively imports ParseError.Empty (+ IoError.Empty)
enum RxNode: Empty; Lit(String)
void main(): print(describe(Empty()))   # bare Empty() as a direct call arg
String describe(RxNode node): match node: case RxNode.Empty(): return "empty"; ...
```
- **Self-host today:** CC-FAIL `incompatible types '__gg_RxNode' from '__gg_ParseError'` — the
  first-write-wins `enum_variant_parent` map routes bare `Empty` → `ParseError` (first import wins),
  minting `(__gg_ParseError){.tag=0}` into a `__gg_RxNode` slot.
- **Rust gg:** MATCH (`empty`). Rust resolves it via BOTH halves: the loader collision-excludes
  `Empty` (owned by >1 non-generic enum), THEN typecheck `decl_type_hint` (`typecheck.rs:479/1087/1346`)
  anchors the bare ctor from the callee's param type.
- This is the only fixture-class the loader-qualify port is even *about*. There is no such fixture
  in the corpus today (hence +0); `qual2` is a constructed repro.

---

## 2. The right-layer decision (grounded in Rust + devbook + language-reference)

**Decision: build the collision-excluding map at the GIR-build site `lower.gg:2542`** (where
`enum_variant_parent_idx` is built today), NOT in `loader.gg`.

Rationale:
- **Rust** builds it in the loader (`build_variant_map_from_all`, `src/loader.rs:907-940`) because in
  Rust the loader is the "resolve bare→parent once" point, feeding typecheck `decl_type_hint`.
- **In the self-host**, `loader.gg` only *merges* imported enums into `m.items`/`type_infos`
  (`:833-843`); the variant→parent index (`enum_variant_parent_idx`) is **built at the GIR-build**
  (`lower.gg:2542`, consumed by the `enum_variant_parent()` accessor in `lower_types.gg:772`). That
  build site IS the self-host's equivalent of Rust's `build_variant_map_from_all` — the single
  "resolve once" point for bare→parent.
- **devbook 24 Rule 4 (resolve once, write through)** and **Rule 3 (one source of truth per axis)**:
  the bare→parent resolution must happen once and be written through the typed index. First-write-wins
  is a *name-collision arbiter* — the exact rule-2 violation the doc names ("resolving abstractions by
  identifier-string lookup, ignoring typed context"). Collision-exclusion removes the arbiter: the
  index holds only unambiguous facts; ambiguous ones defer to typed context (expected-type / qualified).
- **language-reference.md:683-692**: "User-defined enum variants are accessed via qualified syntax
  `EnumName.Variant(args)`. … Variants are namespaced under their enum type to prevent name collisions."
  The bare `return Empty()` in `lib/xtd/regex.gg` is loader-rewritten shorthand; the spec-compliant
  resolution is exactly Rust's: ambiguous bare names are NOT resolvable by the bare map.

The LIR-level `enum_variant_parent` (`lir_lower.gg:4669`, last-write-wins bare + additive qualified
keys from the interim) is a SEPARATE map at a SEPARATE layer (C-emit) and is left UNCHANGED — it is
a last-mile convenience for already-disambiguated names, exactly as the interim's comment states.

---

## 3. The four design questions, resolved

1. **Where does the map build belong?** → `lower.gg:2542` (the GIR-build), the self-host's
   "resolve once" equivalent of Rust's loader. (Above.)

2. **Bare-ambiguous variant with NO expected type (`qual2`)?** → With the map, `enum_variant_parent
   ("Empty")` returns `""` (excluded). The expected-type write-through (`lower_expr.gg:4923`) then
   fires *iff* `ctx.expected_type` is the parent enum. At a typed binding / return (`RxNode n =
   Empty()`, `qual1`) it works. At a **call arg** (`describe(Empty())`, `qual2`) it requires the
   callee's param type — which is unavailable for a **forward-referenced** callee (see §5). So the
   answer is: it becomes a *correct resolution* ONLY when the expected type reaches the site; for the
   forward-reference case it remains CC-FAIL (NOT a clean resolution error — the self-host has no
   bare-variant-ambiguity diagnostic pass; that is the residual). Rust avoids this because its
   typecheck `decl_type_hint` reads the callee signature order-independently.

3. **Retire or complement the interim?** → **COMPLEMENT, retire nothing.** Rust uses BOTH the loader
   collision-exclusion AND typecheck `decl_type_hint` — they are the two halves. The self-host's
   interim (expected-type write-through in `lower_expr.gg` + additive qualified keys in
   `lir_lower.gg` + qualified-key preference in `lir_codegen.gg`) IS the self-host's `decl_type_hint`
   analogue. The collision-excluding map is the missing *other* half. **No part of the interim
   becomes dead:** the regex cluster still relies on the expected-type write-through (bare `return
   Empty()` in `lib/xtd/regex.gg` resolves via the return-type context — collision-exclusion only
   *drops* the bare entry, it never *resolves* anything). Verified: regex cluster stays MATCH with the
   map applied.

4. **Does the qualified `EnumName.Variant()` form resolve?** → **Already YES** (`qual3` MATCH today,
   independent of the map). The interim's qualified-key path in `lir_codegen.gg` handles it.

---

## 4. The port (prototype) — what landed in the throwaway

Single file: `tests/fixtures/self_host_lowerer/lower.gg` (~37 lines). Mirrors Rust two-pass:
- **Pass 1 (in the existing IEnum loop):** record a sighting per variant name —
  `evp_first_seen: variant→first enum`, `evp_sightings: variant→distinct-enum count`. **Exclude
  generic enum TEMPLATES** (`edef.type_params.len() == 0`, mirror `loader.rs:914`) and **PRELUDE
  variants** (`Ok`/`Error`/`Some`/`None`, mirror `loader.rs:920`). Count only DISTINCT enums.
- **Pass 2 (after the loop):** populate `enum_variant_parent_idx` from only the unambiguous
  (`sightings == 1`) entries (mirror `loader.rs:930-940`).

Typed throughout (reads `edef.type_params` / `var.name` off the typed AST decl; the prelude set is
the same closed literal set the rest of the lowerer uses). No name-matching, no sidecar map — it
replaces first-write-wins in-place at the one source of truth.

A count-based sightings proxy (vs a true `Set[enum]`) is sufficient: we only need `==1` vs `>1`, and
the DISTINCT-enum guard (`first_seen != edef.name`) prevents a same-enum double-count.

---

## 5. The SECOND gap (the real blocker for `qual2`) — forward-reference param types

Even WITH the map, `qual2` still CC-FAILs. Root-caused by instrumentation:
- At `describe(Empty())`, the arg-lowering loop (`lower_expr.gg:4670-4693`) sets
  `ctx.expected_type = callee_param_types[ai]` — but `get_fn_param_types("describe")` returns **`[]`**
  because `describe` is defined AFTER `main` and `gmod.functions` is populated **incrementally in
  source order** (bodies lower top-to-bottom; a forward-referenced callee's params aren't there yet).
- With `expected_type = -1`, the expected-type write-through (`:4923`) can't fire → bare `Empty`
  stays bare → first-write-wins LIR map mints `ParseError`.
- **Proof:** move `describe` ABOVE `main` (`qual2b.gg`) → `describe_params=[RxNode]`,
  `Empty` gets `expected_type=RxNode`, the qualify fires, and **qual2b MATCHES**.

Note: `fn_sigs` (RETURN types) IS pre-populated for all functions before bodies lower
(`lower.gg:2359`), so return-type lookup is order-independent. **PARAM types are not** — there is no
`gmod`-level param-type pre-pass. The real reference-grade fix is to add one (parallel to `fn_sigs`),
so `get_fn_param_types` becomes order-independent. That is a separate, larger change and is the true
unblock for the `qual2` class.

---

## 6. MEASURED parity delta — +0, perfectly neutral, ZERO regressions

Command: `GG_RUNTIME_DIFF=1 cargo test --test integration --release self_host_runtime_diff --
--nocapture` (full 1066-fixture corpus, live `gg run` oracle), run for BOTH baseline (`git checkout`
of `lower.gg`) and prototype, same session.

| Category     | Baseline (`5cab6d51`) | Prototype | Δ |
|--------------|-----------------------|-----------|---|
| MATCH        | 727                   | 727       | 0 |
| WRONG-OUTPUT | 98                    | 98        | 0 |
| CC-FAIL      | 212                   | 212       | 0 |
| CRASH        | 29                    | 29        | 0 |
| **PARITY**   | **727/1066 = 68.2%**  | **68.2%** | **0** |

**Per-fixture category diffs (baseline vs prototype) are ALL EMPTY** — `diff b_wrong.txt
p_wrong.txt`, `diff b_ccfail.txt p_ccfail.txt`, `diff b_crash.txt p_crash.txt` each produced no
output. Not a single fixture flipped in either direction. The map is exactly parity-neutral.

Spot-checks (prototype driver, emit→cc→run→diff vs `gg run`):
- regex cluster (the interim's +3) — `regex_basic` / `regex_corpus` / `regex_extended`: **MATCH**
  (no regression).
- `qual3` (qualified form): **MATCH**. `qual2b` (collision, callee-before-caller): **MATCH**
  (the map + expected-type write-through resolve it once params are available).
- `qual2` (collision, callee-after-caller): **CC-FAIL** (blocked by the §5 forward-reference gap).
- `string_enum_variants`: **WRONG** (unchanged — String-index bug, §1).

---

## 7. No-regression evidence + the BOOTSTRAP BOUNDARY (the prototype as-written REGRESSES)

- **Full runtime_diff:** 727/1066 both runs, all category diffs empty. Prelude resolution intact.
- **`cargo test --lib`:** 1084 passed / 0 failed (unaffected — the change is in `tests/fixtures/`,
  not `src/`).
- **`self_host_bootstrap_fixed_point`: ❌ FAILED (200s) — this is the BOUNDARY.** stage-2 cc error:
  `gorget_array_push(__v353, __v110)` where `__v110` is a VALUE `(__gg_Item){.tag=14, .IMetaIf=…}`,
  not a pointer; and a `[bug] EIdentifier: … unknown identifier 'IDropGuardClose'` placeholder in
  stage-2.c. stage-1.c cc's clean; **stage-1.bin (the prototype's self-compiled binary) mis-compiles
  stage-2** → not a fixed point.

### Root cause of the regression — a PRE-EXISTING self-host compiler gap my code-shape trips

`IDropGuardClose` (on `Instruction`, lir.gg:214) is a SINGLE-sighting variant — the collision map
should KEEP it. It was dropped not by the loader-qualify *logic* but because **the self-host
mis-compiles the prototype's second-pass code shape**: a `Dict[String,int].get(k).unwrap()` used in a
condition inside a `for k in otherDict.keys()` loop. Minimal repro (`dk3.gg`):
```gorget
Dict[String, String] first = {}; Dict[String, int] counts = {}
first.put("a","X"); counts.put("a",1); first.put("b","Y"); counts.put("b",2)
Dict[String, String] out = {}
for k in first.keys():
    if counts.get(k).unwrap() == 1:        # ← self-host drops this branch/put
        out.put(k, first.get(k).unwrap())
print("out size: " + int_to_str(out.len()))   # oracle: 1   self-host: 0
```
Oracle `out size: 1`; self-host `out size: 0`. So stage-1.bin builds an EMPTY/under-populated
`enum_variant_parent_idx` → every bare variant ctor in the self-host's own source becomes "unknown
identifier" / mis-typed → bootstrap breaks. **This is a self-host bug independent of loader-qualify**
(parallel-Dict cross-lookup inside a keys-iteration), surfaced by the prototype's two-dict shape.
→ File as a TODO: self-host mis-lowers `Dict[K,V2].get(k).unwrap()` in a condition inside
`for k in Dict[K,V1].keys()`.

### The port IS salvageable with a self-host-safe shape (still +0 parity)

A SINGLE-dict sentinel formulation dodges the gap entirely (no parallel int-dict, no post-loop
keys-iteration): keep one `Dict[String,String]`; on the first sighting `put(v, enum)`; on a sighting
of a DIFFERENT enum, `put(v, "")` (ambiguous sentinel); have `enum_variant_parent` treat `""` as
"not found". Repro (`dk4.gg`) compiles+runs **MATCH** under the self-host (`a=EX`, `b=[]`). This is
the shape a real port should use. **It does not change the +0 yield** — it only makes the port
bootstrap-safe.

---

## 8. Recommendation

**Do NOT land the loader-qualify port.** Three independent reasons:
1. **+0 parity** (measured, full corpus, zero fixture flips).
2. **As prototyped it BREAKS the bootstrap** (fixed_point FAILS — §7). A self-host-safe single-dict
   reformulation exists, but see (1): it would land a hygiene change that costs a non-trivial
   re-prototype + re-gate for zero parity.
3. The genuine collision case it targets (`qual2`) needs a SECOND fix anyway (the §5 forward-reference
   param-type pre-pass), and there is no corpus fixture exercising it.

**The actual parity yield in this area lives elsewhere — re-file the TODO:**
- **String-index-on-by-pointer-param** (`min6`: `String ch = input[i]` lowers to literal `0`) is the
  root cause of BOTH `string_enum_variants` (WRONG) and `string_error_handling` (CRASH), and likely a
  broader class. **This is the higher-value target — own scout.** TODO item-4a/4b/4c's diagnosis
  ("cross-enum collision" / "loader-qualify port likely subsumes 4a") is **WRONG** and must be
  re-filed: 4a/4b are String-index bugs; 4c (loader-qualify) is +0 and bootstrap-risky.
- **Self-host Dict gap (new, file it):** `Dict[K,V2].get(k).unwrap()` in a condition inside
  `for k in Dict[K,V1].keys()` is mis-lowered (the put/branch is dropped). Minimal repro `dk3.gg`
  (§7). Independent of loader-qualify; worth its own fixture + fix.

**If the orchestrator still wants the layering cleanup for its own sake** (replacing first-write-wins
with the reference-grade collision-excluding map; devbook-24 rule-2/3/4 compliance): it is achievable
and parity-neutral, but MUST use the single-dict-sentinel shape (§7) to survive the bootstrap, and
MUST be framed as hygiene, not parity. The interim is COMPLEMENTED, not retired (§3).
