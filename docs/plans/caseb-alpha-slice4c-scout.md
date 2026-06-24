# Scout — Case-B Track-α SLICE 4c: `SMetaForMatch` arm-BODY substitution (callee-rename + type-arg subst)

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-24, worktree off
`gorget-1` tip `bee137d9` (the LANDED 4a+4b meta engine). Every number below was regenerated
THIS session by building the prototype self-host lowerer driver, running it over the full
**1294-fixture** corpus (`GG_RUNTIME_DIFF=1 … self_host_runtime_diff`), and cross-checking every
flipped fixture against the Rust `gg run` oracle whole-stdout. The prototype is committed as
throwaway `3bf97f2b` (`PROTOTYPE(scout): … DO NOT INTEGRATE`) — it must NOT be integrated as-is
(no independent-copy ripple / snapshot / lint; see §7).

This is the LAST α residual-clearer for the **meta-var NAME** residual: it clears the
match-position `vname` (the dominant ×280 of the β scout's 22-residual) by making the `meta for`
inside a `match` substitute `vname` into the RETURN-position ctor call AND the bare-id return
value AND the `T` type-arg — not just the pattern head.

Grounded in `docs/language-reference.md §19.26` (`meta for` Inside Match Arms — the authoritative
THREE substitution positions: pattern, callee identifier, type argument), `§19.20`/`§19` builtin
`variant_payloads(T) → [name, inner_type]` pairs, Rust `src/semantic/meta.rs`
(`substitute_match_arm:2035` → `substitute_pattern:2006` + `substitute_expr:2291` +
`substitute_stmt:2138`; the `Expr::Call` callee-rename special-case `:2303-2307`; the bare-id
`meta_value_to_expr` Str→StringLiteral `:2466`; `substitute_type:1867`; `meta_str_to_type:2046`;
`expand_match_meta_for:2073` with its `type_env` built from string meta values `:2105-2113`), and
the slice-4 scout (`docs/plans/caseb-alpha-slice4-scout.md §6 sub-slice 4c`) + slice-4b scout §5/§6
(which confirmed `vname` in `meta_variant_payloads` is the `SMetaForMatch` match-arm loop var,
expanded by `expand_meta_for_arms` in `meta.gg`, a SEPARATE construct from the 4a/4b lowerer engine).

---

## 0. Headline

| metric | command | baseline (`bee137d9`) | THIS scout's prototype (`3bf97f2b`) |
|---|---|---|---|
| runtime parity | `self_host_runtime_diff` | **750/1069 = 70.2%** | **754/1069 = 70.5% (+4)** |
| fixtures flipped to MATCH | (whole-corpus non-match-set diff) | — | **`meta_variant_payloads` + `dataframe_basic` + `dataframe_tier2_basic` + `dataframe_transform` (4)** |
| regressions | (whole-corpus non-match-set diff) | — | **0** |
| `bootstrap_fixed_point` | `self_host_bootstrap_fixed_point` | GREEN | **GREEN (re-converged, 318s)** |
| `type_comparison` | `type_comparison --nocapture` | 1180 exact + 42 superset = 1222, 72 mismatched, 0 crashed | **1180+42=1222 / 72 / 0 — BYTE-IDENTICAL** |
| `c_emit_comparison` | `c_emit_comparison --nocapture` | (4b) ~1045 matched | **1045 matched / 138 mismatched / 0 self-host crashes** |
| `resolver_comparison` | `resolver_comparison --nocapture` | independent dir — untouched | **1275 matched / 19 mismatched / 0 crashed (unaffected)** |
| `parser_comparison` | `parser_comparison` | independent dir — untouched | **ok (unaffected)** |
| `self_host_runtime` lock-in net | `self_host_runtime` | GREEN (passing set 723) | **GREEN (passing set 723)** |

**Bottom line:** 4c is REAL, CLEAN, and over-delivers. `meta_variant_payloads` goes from CC-FAIL
to byte-exact MATCH, AND **three `dataframe_*` fixtures flip as a BONUS** (they transitively use the
`xtd.dataframe` library's `meta for vname, T in variant_payloads(…)` column-dispatch, the exact
construct 4c fixes). All 4 flips are byte-exact oracle MATCHes, **0 regressions** anywhere, the
bootstrap re-converges (the driver self-compiles its OWN new `subst_arm_*`/`subst_arm_type` to a
byte-identical fixed point), and every structural diagnostic is neutral or unaffected. **+4 measured.**
This clears the match-position `vname` residual — the META-NAME half of the α→β unblock.

---

## 1. The reproduced gap end-to-end (measured this session, on the 4a+4b tip)

Target: `tests/fixtures/meta_variant_payloads.gg` — `match s: meta for vname, T in
variant_payloads(Shape): case vname(w): return vname(rewrap[T](w))` (and a sibling `dispatch_name`
with `case vname(w): return vname`).

Rust oracle: `Circle/Square/Tag/Circle/Square/Tag/done`.

Baseline self-host (`F lib --emit-c` → `cc -O0` → run): **CC-FAIL**.

The gap is BIGGER than the slice-4 scout's §6-4c framing ("the pattern head gets `vname`
substituted but not the return-position ctor call"). In fact, the baseline `substitute_arm`
(`meta.gg:1221`) substitutes ONLY the pattern head (via `substitute_pattern_head:1209`) — it leaves
`template.body` **completely untouched**. So THREE body positions survive unsubstituted (matching
the three the language-reference §19.26 enumerates):

1. **Bare `return vname`** (`dispatch_name`) → emitted C carries `/* [bug] EIdentifier: unknown
   identifier 'vname' — returning OpConstI64(0) placeholder (WRONG) */`; each arm does `__s0 =
   (Str){0}; return __s0` → returns EMPTY strings. **WRONG output** (no CC-FAIL).
2. **Ctor callee `return vname(rewrap[T](w))`** (`rewrap_shape`) → `__v14 = vname(__v12);` →
   `error: incompatible types when assigning to type '__gg_Shape' from type 'int'` (the unknown
   `vname` callee defaults to `int`). **CC-FAIL** (`mvp.c:1848`).
3. **Type-arg `rewrap[T]`** → emitted as `rewrap__T(...)` (the literal type-param `T`, a broken
   mono). Surfaces only once #2 is fixed (see §3 sub-gap).

WHY (write-site trace): the WRITE site is `substitute_arm` (`meta.gg:1221`), called from
`expand_meta_for_arms` (`meta.gg:1248`/`:1269`). It builds the concrete arm as
`MatchArm(new_pat, template.guard, template.body)` — `template.body` passed through verbatim. The
loop-var values (`vname`→variant name, `T`→payload type) are available at the call site (`v.name`
and the variant's single field type) but never threaded into the body. Mirrors NOTHING of Rust's
`substitute_match_arm` (`meta.rs:2036-2038`), which substitutes pattern + guard + **body**.

---

## 2. The reference-grade fix shape, traced (file:line)

Port Rust `substitute_match_arm`'s body substitution into the self-host `substitute_arm`. The
self-host already has TWO complete recursive substituters in `meta.gg` to model on:
`subst_expr`/`subst_stmts` (the `[PROTOTYPE]` meta-const substituter, `:459`/`:508`) and
`rename_aliases_expr`/`rename_aliases_stmts`/`rename_aliases_type` (the import-alias renamer,
`:698`/`:776`/`:628`). The fix is a NEW dedicated body-substituter that combines the two behaviors
Rust splits across `substitute_expr` + `substitute_type`:

**The load-bearing distinction (Rust `meta.rs:2303-2307` vs `:2466`):** `vname` substitutes
DIFFERENTLY by position —
- as an **ECall callee** (`vname(…)`) → rename to an **IDENTIFIER** (`Circle(…)`), NOT a string
  literal (else `"Circle"(…)` — a type error). [Rust `meta.rs:2303-2307`]
- as a **bare identifier** (`return vname`) → a **STRING literal** (`return "Circle"`), since
  `vname` is `MetaValue::Str`. [Rust `meta.rs:2466` `meta_value_to_expr(Str)`]

**The `T` type-arg distinction:** the type loop var renames in every TYPE position (`ECall` `targs`,
`SVarDecl` type, `EAs` target, nested arrays/slices/tuples/fn-types). [Rust `substitute_type:1867`,
`type_env` from `meta_str_to_type:2046`/`:2105-2113`].

The prototype's helpers (all NEW, in `meta.gg`, the shared/symlinked dir):

| helper | `meta.gg` line (proto) | role | Rust mirror |
|---|---|---|---|
| `meta_name_to_spanned_type(name, sp)` | `:1245` | Gorget type-name string → `SpannedType`; **TPrimitive for primitives, TNamed else** | `meta_str_to_type:2046` |
| `subst_arm_type(st, tmap)` | `:1263` | recursive TYPE-position substituter (`T`→payload type) | `substitute_type:1867` |
| `subst_arm_expr(se, vn, vval, tmap)` | `:1301` | EXPRESSION substituter with the callee-vs-bare-id `vname` split + `targs` type-subst | `substitute_expr:2291` (+ `:2303` callee, `:2466` bare-id) |
| `subst_arm_stmts(stmts, vn, vval, tmap)` | `:1396` | STATEMENT substituter (SReturn/SExpr/SVarDecl/SAssign/SIf/SWhile/SFor/SMatch) | `substitute_stmt:2138` |
| `extract_payload_inner_type(v)` | `:1453` | the variant's single-field inner type-arg name + `Str`/`GorgetString`→`String` norm | `variant_payloads` inner extraction `meta.rs:3078-3115` |
| `substitute_arm(template, var, name, tparam_var, tparam_val)` | `:1481` | now substitutes pattern head **AND body** | `substitute_match_arm:2035` |

Call-site change (`expand_meta_for_arms:1512`): pass `vars.get(1)` (the optional `T` loop var) and
`extract_payload_inner_type(v)` to `substitute_arm`, so `T`→`float`/`int`/`String` per-variant.

### The non-obvious sub-gap the prototype HAD to solve (mangle divergence)

The naive port (substitute `T` → `TNamed("float", [])` via the existing `rename_aliases_type`)
COMPILED but **link-failed**: `undefined reference to rewrap__float / rewrap__int`. Root cause
(traced via a temporary debug eprint in the emission loop `lower.gg:4268`):

- **Discovery** (`discover_generic_calls_expr`, `lower_generics.gg:378-387`) mangles the `targs` via
  `compute_mangled_name` → `spanned_type_to_c_name(TNamed("float",[]))` → `prim_to_c_name("float")`
  = **`double`** → registers `rewrap__double` / `rewrap__int64_t`.
- **Call site** (`lower_call`, `lower_expr.gg:384-385`) mangles via `type_to_c_name(TNamed("float",[]))`
  → `mangle_type_name("float",…)` = **`float`** (Gorget name kept) → emits `rewrap__float`.

`spanned_type_to_c_name` (discovery) and `type_to_c_name` (call site) DISAGREE on
`TNamed("float",[])` — a pre-existing inconsistency that never surfaces in normal code (the parser
produces `TPrimitive("float")` for a resolved primitive, and BOTH route `TPrimitive` through
`prim_to_c_name`). The fix is to make the substituted type a **`TPrimitive`** for primitive payloads
(exactly Rust's `meta_str_to_type` → `Type::Primitive(Float)`), so discovery and call-site agree
(both → `double`). That is what `meta_name_to_spanned_type` does. After this, all three monos
mangle to `rewrap__double`/`rewrap__int`/`rewrap__GorgetString` consistently and link cleanly.

This is the layering-discipline win: the bug was a missing TYPED distinction (primitive vs named) at
the substitution WRITE site, exposed because the substituted node carried the wrong shape. NO
name-matching; the primitive set comes from the `prim_name_to_type` oracle (mirrored in
`meta_name_to_spanned_type`).

---

## 3. Prototype + MEASURED end-to-end (regenerated this session)

`d_proto F lib --emit-c --runtime-dir=src/backend/c/runtime` → `cc -O0` → run, diffed against
`gg run F` (Rust oracle):

| fixture | baseline | prototype (4c) |
|---|---|---|
| `meta_variant_payloads` | CC-FAIL (`vname(…)` ctor → `int`→`__gg_Shape`) | **byte-exact MATCH** ✅ |
| `dataframe_basic` | CC-FAIL | **byte-exact MATCH** ✅ |
| `dataframe_tier2_basic` | CC-FAIL | **byte-exact MATCH** ✅ |
| `dataframe_transform` | CC-FAIL | **byte-exact MATCH** ✅ |

The `dataframe_*` flips are NOT accidental: those fixtures import `xtd.dataframe`, whose column
machinery uses `meta for vname, T in variant_payloads(Column)` for typed column dispatch (the exact
spec example, language-reference §19.26). They CC-FAILed on the same un-substituted-arm-body gap and
flip for the same reference-grade reason. Each verified byte-exact vs the oracle this session.

**Verified flip/regression set** (strict whole-corpus non-match-set diff, base `750` vs proto `754`):
exactly **FOUR fixtures left the non-match set** (`meta_variant_payloads`, `dataframe_basic`,
`dataframe_tier2_basic`, `dataframe_transform`), and **ZERO entered it** (`comm -13` empty).

**Other gates (all regenerated this session):**
- `bootstrap_fixed_point` **GREEN** (318s, re-converged). The driver's own source uses
  `meta`-machinery primitives, so the new `subst_arm_*`/`subst_arm_type` are self-compiled to a
  byte-identical stage-2==3==4 fixed point — the load-bearing internal-consistency check.
- `type_comparison` **1180+42=1222 / 72 / 0 — byte-identical** to baseline.
- `c_emit_comparison` **1045 matched / 138 mismatched / 0 self-host crashes**.
- `resolver_comparison` **1275 / 19 / 0** (independent dir, unaffected); `parser_comparison` **ok**
  (independent dir, unaffected) — `self_host_parser`/`self_host_resolver` have NO `meta.gg` and do
  not use the arm-expansion, so they are structurally isolated.
- `self_host_runtime` lock-in net **GREEN** (passing set 723, 0 broken snapshots).

---

## 4. Reference-grade gate (Core #8)

All 4 flipped fixtures produce **CORRECT** (byte-exact Rust-oracle) output, verified twice this
session — not merely non-failing, not allow-listed, not snapshot-pinned-wrong.

- `vname` is **genuinely substituted/evaluated**: the ctor callee becomes the real `Circle`/`Square`/
  `Tag` identifier (a real enum-variant constructor), the bare-id becomes the real variant-name
  string, and `T` becomes the real payload type (`float`/`int`/`String`), monomorphizing
  `rewrap[T]` into the correct `rewrap__double`/`rewrap__int`/`rewrap__GorgetString`. This matches
  the language-reference §19.26 expansion EXACTLY (all three substitution positions).
- No KNOWN DEFECT is shipped. The fix REJECTS nothing and MASKS nothing: the still-WRONG meta
  fixtures (`meta_reflection`/`meta_implements`/`meta_type_is`/`meta_numeric_meta`/`meta_while`/
  `meta_platform_guard`/`meta_delayed_match`/`field_access`) are UNTOUCHED by 4c — they fail on
  SEPARATE missing meta-PREDICATE builtins (`implements`/`type_is`/`field_value`/reflection
  predicates), NOT on the arm-body substitution. Verified this session: NONE of them leaks
  `vname`/`fname`/`ftype`/`idx` as an undefined identifier (leak-count = 0 for each), so 4c neither
  regresses nor masks them. They are later, distinct slices (4a/4b scout §6: predicate families).

**Verdict: SHIP.** Reference-grade, +4 measured, parity-neutral on every structural diagnostic,
bootstrap-green.

---

## 5. Residual-cleared honesty — does this complete the meta-NAME residual (the β-flip prereq)?

**YES for the meta-var NAME residual; NO it does not reduce the β residual to ONLY snag51.** Be
precise about what 4c clears vs what the brief over-claimed:

The β scout (`caseb-track-beta-scout.md §2/§3`) splits the residual into a **meta-var NAME** class
(`vname`×280, `fname`, `ftype`, `idx`) and FOUR non-meta (a)-classes. The α slices clear the
meta-NAME class:
- **4a** cleared `fname`/`ftype`/`vname`(statement-body loop var).
- **4b** cleared `idx`.
- **4c (this slice)** clears **`vname` in the `SMetaForMatch` (match-arm) shape — the ×280 dominant
  hit.** With 4c landed, the match-arm `vname` is genuinely substituted (callee→ident, bare→string)
  before resolve/lower ever sees it, so it can no longer leak as an undefined name. **The meta-var
  NAME residual is COMPLETE** (verified: no remaining meta fixture leaks `vname`/`fname`/`ftype`/`idx`).

**BUT** — the brief's claim "after 4c the ONLY remaining β-flip residual is snag51's `s`/`n`" is
NOT accurate per the β scout's measured §3 table. After the meta cluster clears, the β scout lists
**THREE** small non-meta (a)-classes still blocking the clean β flip (`caseb-track-beta-scout.md
§3`):
1. **`ecs_*` `Entity` typealias-transitive** (3 fixtures) — a type alias in a transitively-imported
   module used bare in the entry fixture.
2. **`sqlite_basic` parser `blocking`/`noreturn` inline-extern** (1 fixture) — the inline-extern
   parse arm skips `borrowed` but not `blocking`/`noreturn`.
3. **`snag51_closure_block_tail_value` `s`/`n` pattern-binding** (1 fixture) — the A_closure track
   (lexer `Box`-de-keyword + the closure-call ABI mirage).

So: **4c COMPLETES the meta-NAME residual (the α half of the α/β ordering constraint — α binds the
names so β won't false-reject them), but it does NOT leave snag51 as the sole residual.** The β flip
still needs the three small (a)-class fixes above (none of which is meta, none touches the meta
engine or `meta.gg`). Correct the handover wording to: *"4c completes the meta-var NAME residual;
the remaining β-flip prereqs are the three non-meta (a)-classes — Entity-typealias, parser
blocking/noreturn, and A_closure/snag51."*

---

## 6. Decompose? — NO. 4c is a single coherent increment.

The three substitution positions (pattern already done; callee-rename; bare-id-string; type-arg)
are ENTANGLED at one write site (`substitute_arm`'s body walk): you cannot land the callee-rename
without the body walk, the type-arg needs the same walk, and the mangle-consistency sub-gap (§2) is
forced the moment the type-arg substitutes. The prototype proves all 4 fixtures flip with the single
coherent change. **Ship 4c as one increment.** (One could split "bare-id-string only" — it would
flip nothing alone, since `meta_variant_payloads` needs the ctor+type-arg to compile — so splitting
buys nothing.)

---

## 7. Zones, disjointness, and what productionizing 4c still needs

**Slice-4c zone (the ONLY file touched):**
- `self_host_typechecker/meta.gg` — `substitute_arm`/`expand_meta_for_arms` + the 5 new helpers
  (`meta_name_to_spanned_type`, `subst_arm_type`, `subst_arm_expr`, `subst_arm_stmts`,
  `extract_payload_inner_type`). Shared/symlinked into `self_host_lowerer` and `self_host_check`
  (`md5sum`-confirmed: `self_host_{lowerer,check}/meta.gg` are symlinks → `self_host_typechecker/
  meta.gg`; `self_host_parser`/`self_host_resolver` have NO `meta.gg`). Added imports: `TArray,
  TSlice, TTuple, TFunction` (the constructor forms the type-substituter builds).

**Disjointness (confirmed — your zone is `meta.gg`'s expand, fully isolated):**
- **vs the landed 4a/4b engine** (`lower_generics.gg` `evaluate_delayed_meta_stmts` + the int/string
  MetaValue env): 4c touches NEITHER — the `SMetaForMatch` match-arm expansion is a pre-resolve AST
  rewrite in `meta.gg` (`expand_meta_for_match` at `driver.gg:382`), a DIFFERENT construct + pass
  from the per-monomorphization statement-body `SMetaFor` engine. **Disjoint.**
- **vs Track β** (`resolve.gg` EIdentifier-miss arm + import allow-set + `loader.gg`
  `imported_bare_names`): 4c touches NONE of them. **Disjoint.**
- **vs A_closure** (`caseb-aclosure-scout.md`: lexer `Box`/`Rc`/… de-keyword + Box-lowering +
  closure-call ABI): 4c touches neither lexer nor closures. **Disjoint.**

**What this prototype does NOT yet do (REQUIRED before integration):**
1. **`EStructLiteral` generic-arg type-subst is string-keyed (`subst_arm_expr` EStructLiteral arm):**
   it pushes the bare Gorget name `tmap[g]` (e.g. `float`) into the struct-literal `gargs` strings.
   For a primitive payload used as a struct-literal generic arg, this could re-introduce the
   `float`/`double` mangle divergence (§2) the same way the ECall-`targs` path did. The target
   corpus has no such shape (no flip depends on it), so the prototype is correct as measured — but
   production should route struct-literal generic args through the same `meta_name_to_spanned_type`
   C-name oracle (or confirm `EStructLiteral` gargs are mangled identically to ECall targs). FLAG
   for the executor.
2. **Lock-in snapshots.** Add `tests/fixtures/runtime_snapshots/{meta_variant_payloads,dataframe_basic,
   dataframe_tier2_basic,dataframe_transform}.out` so `self_host_runtime` pins the 4 flips (all
   stable byte-exact MATCH — verified self-host + oracle this session).
3. **Arm-body-subst arm-count lint** (optional, per CLAUDE.md "fix the class"): `substitute_arm`'s
   pattern-head subst and the new body subst now both run; a lint (e.g. assert the
   `subst_arm_expr` ECall-callee special-case stays present, or that `substitute_arm` walks the body)
   guards the next sibling (e.g. when `meta for` gains a guard-expr or a multi-arm template).
4. **Independent-copy ripple: NONE needed.** Unlike 4a/4b (which rippled an AST arity),
   4c adds no AST variant and changes no AST arity — it only adds functions + a per-call `tval`. The
   independent `self_host_parser`/`self_host_resolver` dirs have no `meta.gg` and are unaffected. So
   the integration tail is just the snapshots + lint, not an AST-shape propagation.

---

## 8. Reproduce

```bash
git merge --ff-only gorget-1            # tip bee137d9 (LANDED 4a+4b)
cargo build                             # ./target/debug/gg

# baseline (revert meta.gg to HEAD~1, or check out bee137d9)
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_runtime_diff -- --nocapture     # → 750/1069
cargo test --test integration --release type_comparison -- --nocapture   # → 1180+42=1222 / 72 / 0

# prototype (commit 3bf97f2b)
GG_BUILD_TIMEOUT_SECS=600 ./target/debug/gg build \
    tests/fixtures/self_host_lowerer/driver.gg -o /tmp/d_proto
for F in meta_variant_payloads dataframe_basic dataframe_tier2_basic dataframe_transform; do
  /tmp/d_proto tests/fixtures/$F.gg lib --emit-c --runtime-dir=src/backend/c/runtime > /tmp/$F.c \
    && cc -O0 -w -o /tmp/$F /tmp/$F.c -lm -lpthread
  diff <(/tmp/$F) <(./target/debug/gg run tests/fixtures/$F.gg) && echo "$F BYTE-EXACT"
done
GG_RUNTIME_DIFF=1 … self_host_runtime_diff               # → 754/1069 (+4)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_bootstrap_fixed_point                      # GREEN (318s)
cargo test --test integration --release type_comparison    -- --nocapture  # → 1222/72/0 (neutral)
cargo test --test integration --release c_emit_comparison  -- --nocapture  # → 1045 matched / 0 sh-crash
cargo test --test integration --release resolver_comparison -- --nocapture # → 1275/19/0 (unaffected)
cargo test --test integration --release self_host_runtime  -- --nocapture  # → passing set 723 (GREEN)
```

## 9. Docs the design rests on
- `docs/language-reference.md §19.26` (`meta for` Inside Match Arms — the **authoritative THREE
  substitution positions**: pattern, callee identifier, type argument; the example
  `vname(col_slice_inner[T](c,…))` is the exact target shape), `§19.20`/`§19` (`variant_payloads(T)`
  `[name, inner_type]` pairs + the `Str`/`GorgetString`→`String` normalization).
- Rust `src/semantic/meta.rs`: `substitute_match_arm:2035` (the reference — pattern + guard +
  **body**), `substitute_pattern:2006`, `substitute_expr:2291` with the `Expr::Call` callee-rename
  special-case `:2303-2307` and the bare-id `meta_value_to_expr` Str→StringLiteral `:2466`,
  `substitute_stmt:2138`, `substitute_type:1867`, `meta_str_to_type:2046` (TPrimitive-vs-TNamed →
  the mangle-consistency fix), `expand_match_meta_for:2073` + its `type_env`-from-string-meta
  `:2105-2113`, `variant_payloads` inner-arg extraction `:3078-3115`.
- `docs/plans/caseb-alpha-slice4-scout.md §6 sub-slice 4c` (the decomposition that defined this
  slice; this scout CORRECTS its "pattern head only" framing — the baseline substitutes nothing in
  the body) + §5 (the residual-names framing).
- `docs/plans/caseb-alpha-slice4b-scout.md §5/§6` (confirmed `vname` here is the `SMetaForMatch`
  loop var, a separate construct from the 4a/4b engine, expanded by `expand_meta_for_arms`).
- `docs/plans/caseb-track-beta-scout.md §2/§3` (the 22-residual `vname`×280 + the α/β ordering
  constraint — α binds the names so β can flip; the THREE non-meta (a)-classes still blocking the β
  flip AFTER the meta cluster clears, which CORRECTS the brief's "only snag51 remains" claim).
- `docs/plans/caseb-aclosure-scout.md` (snag51 = the A_closure track: lexer `Box`-de-keyword + the
  closure-call ABI mirage — one of the three remaining β-flip prereqs, NOT cleared by 4c).
- CLAUDE.md — Core-#8 reference-grade gate (all 4 fixtures CORRECT, not just non-failing),
  "Don't redesign around compiler gaps" (the names are genuinely SUBSTITUTED, not allow-listed),
  "Typed metadata, never name-matching" (the TPrimitive-vs-TNamed distinction is a TYPED node shape
  from the `prim_name_to_type` oracle, NOT a name-prefix heuristic — and it is the load-bearing fix
  for the discovery/call-site mangle agreement), "Layering discipline — fix complexity is a signal"
  (the link-fail was a missing typed distinction at the substitution WRITE site, fixed there),
  "Re-verify a premise … MEASURE end-to-end" (every number above regenerated this session — the +4 /
  0-regress / bootstrap-green / 1222-72-0 / 1045-matched / 1275-19-0 / passing-723 figures).
```
