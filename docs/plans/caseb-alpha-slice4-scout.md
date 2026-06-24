# Scout — Case-B Track-α SLICE #4: the `fields()`/`variant_names()` reflection engine

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-23, worktree off
`gorget-1` tip `d3981a00` (the meta #3 delayed-meta engine, `12e08c45`). All numbers below
were regenerated THIS session by building the prototype self-host lowerer driver, running it
over the full **1294-fixture** corpus (`GG_RUNTIME_DIFF=1 … self_host_runtime_diff`), and
cross-checking every meta fixture against the Rust `gg run` oracle whole-stdout. The prototype
is committed as a throwaway `PROTOTYPE(scout): … DO NOT INTEGRATE` commit — it must NOT be
integrated as-is (it carries no lint/snapshot/independent-copy ripple; see §7).

Grounded in `docs/language-reference.md` (meta `for`/`fields()`/`variant_names()` semantics),
Rust `src/semantic/meta.rs:2990-3063` (`fields`/`variant_names`/`variant_count` reflection),
`src/semantic/meta.rs:3300-3481` (`evaluate_delayed_meta_block`'s `Stmt::MetaFor` List arm),
and the self-host meta #3 engine (`lower_generics.gg` `evaluate_delayed_meta_stmts`) it extends.

---

## 0. Headline

| metric | command | baseline (`d3981a00`) | THIS scout's prototype |
|---|---|---|---|
| runtime parity | `self_host_runtime_diff` | **747/1069 = 69.9%** | **748/1069 = 70.0% (+1)** |
| fixtures flipped to MATCH | (diff of non-match sets) | — | **`meta_fields` (1)** |
| regressions | (diff of non-match sets) | — | **0** |
| `bootstrap_fixed_point` | `self_host_bootstrap_fixed_point` | GREEN | **GREEN (re-converged, 330s)** |
| `type_comparison` | `type_comparison --nocapture` | 1180 exact + 42 superset = 1222, 72 mismatched | **1222/72 — BYTE-IDENTICAL** |
| `resolver`/`parser`/`lexer` comparison | (independent dirs — untouched) | n/a | **unaffected, 0 crashes** |

**Bottom line:** the reflection-engine extension is REAL and CLEAN — `meta_fields.gg` goes from
WRONG-OUTPUT (`0/0/done`, the whole `meta for … in fields(T)` body silently dropped) to a
byte-exact MATCH of the Rust oracle, and the change is parity-neutral on every structural
diagnostic + the bootstrap. This is the FIRST shippable sub-slice (4a: `fields()` struct-field
unroll + `variant_names()` variant unroll + the `ftype is numeric` meta-if predicate).

The full slice #4 DECOMPOSES into 3 sub-slices (§6). Sub-slice **4a (this prototype) is the
first shippable increment, +1 measured.** Sub-slices 4b (`meta const` + `enum_ordinal`/
`variant_count` + int-range) and 4c (`SMetaForMatch` body ctor-subst) are larger and gated on
in-body `meta const` parsing (a SEPARATE parser gap, §3) — they are NOT in this prototype.

---

## 1. The failing fixtures + reproduced divergence (measured this session)

`F lib --emit-c --runtime-dir=<abs>` → `cc -O0` → run, diffed against `gg run F` (Rust oracle).

| fixture | construct | Rust oracle | self-host BASELINE | prototype (4a) |
|---|---|---|---|---|
| **`meta_fields`** | `meta for fname, ftype in fields(T):` + nested `meta if ftype is numeric:` | `x:float`/`y:float`/`name:String`/`health:int`/`alive:bool`/`2`/`1`/`done` | `0`/`0`/`done` (loop body dropped) | **MATCH (byte-exact)** ✅ |
| `meta_enum_ordinal` | `meta for vname in variant_names(T):` + `meta const idx = enum_ordinal(T, vname)`; `meta for i in 0..variant_count(T):` + `meta const vname = enum_from_ordinal(T, i)` | `North=0…West=3`/`Red=0…Blue=2`/`Red`/`Green`/`Blue`/`done` | `done` only (both loops dropped) | `North=0`/`East=0`/…/`Blue=0`/`done` (variant NAMES now unroll; `idx`=0 because `meta const` is dropped at parse; `print_names` int-range still inert) — **WRONG, but no regression** |
| `meta_variant_payloads` | `match s: meta for vname, T in variant_payloads(Shape): case vname(w): return vname(rewrap[T](w))` | `Circle`/`Square`/`Tag`×2/`done` | CC-FAIL (`vname(…)` survives as unknown ident → `int`→`__gg_Shape` C type error) | **unchanged CC-FAIL** (separate `SMetaForMatch` construct, §6 sub-slice 4c) |
| `meta_reflection` | `field_count`/`has_field`/`field_names`/`field_type`/`variant_count` meta-ifs + `meta for … in field_names(T)` | many lines | WRONG (`true/true/done`) | WRONG (now prints the `variant_names()` unroll lines too) — **no regression** (was WRONG, still WRONG, CLOSER) |
| `meta_delayed_for` | int-range `meta for i in 0..3:` + nested `meta if i == N` | `first/second/third`×2/`int/float` | WRONG (`int/float/unknown`) | unchanged WRONG (int-range loop not handled, §6) |
| `field_access` | `field_value`/`field_set`/`make_variant` reflection call-builtins | runtime values | CC-FAIL (`undefined reference to field_value`) | unchanged CC-FAIL (needs the runtime builtins — separate gap) |

**Verified flip/regression set** (diff of base-vs-proto non-match fixture sets over the whole
corpus): exactly **one fixture left the non-match set (`meta_fields`)**, and **zero entered it**.

---

## 2. The WRITE-site gap, traced across ALL layers (file:line)

The meta-for-over-reflection is lossy at the PARSER — the same class as the meta #3 `SMetaIf`
and the `IMetaIf` #1 lessons (the parser discards the construct's data, so downstream can't
materialize it).

| layer | file:line (baseline) | gap |
|---|---|---|
| **AST** | `self_host_typechecker/ast.gg:121` | `SMetaFor(Vector[Stmt])` carries **only the body** — the loop-var names AND the iterable (`fields(T)`) are absent. (Contrast `SMetaForMatch(Vector[String], SpannedExpr, Vector[MatchArm])` at `:133`, which ALREADY carries vars+range — the statement-body sibling was never upgraded.) |
| **Parser** | `self_host_typechecker/parser.gg:3026-3030` | `meta for …: body` calls `skip_meta_header()` (`:1154`), which consumes EVERYTHING between `meta for` and the `:` — vars and iterable both. Builds `SMetaFor(body)`. (The capture code it SHOULD use already exists 230 lines down at `:3260-3274`, in the in-match `SMetaForMatch` arm.) |
| **Meta engine** | `lower_generics.gg:705 evaluate_delayed_meta_stmts` | NO `SMetaFor` arm → falls through `else: out.push(s)` → the meta-for passes through unmodified. |
| **Lower** | `lower_stmt.gg:794 case SMetaFor(_): pass` | the residual `SMetaFor` lowers to a no-op → the whole loop body vanishes. |
| **(secondary) in-body `meta const`** | `self_host_typechecker/parser.gg:3074-3076` | `meta const idx = …` INSIDE a function body falls into `else: skip_meta_rest(); return SMeta()` — **completely discarded**. There is NO `SMetaConst` statement variant in the self-host AST. This is why `meta_enum_ordinal`'s `idx` stays 0 (§6 sub-slice 4b). |

The data the engine needs is already present at lowering time:
- `gmod.type_infos: Dict[String, GirTypeInfo]` (struct fields: `GirFieldInfo{name, type_name}`),
  populated at `lower.gg:2563` with SOURCE-level names mangled via `spanned_type_to_name`
  (so `Point.x`→`"double"`; reverse-mangle to `"float"` needed, §3).
- `gmod.enum_registry: Dict[String, Vector[String]]` (enum variant names), populated at
  `lower.gg:2586`.
- Both are populated BEFORE `lower_generic_function` runs (`lower.gg:4266`), so the per-mono
  engine can read them. `tn_env` (param→concrete-typename, `build_meta_typename_env`,
  `lower_generics.gg:589`) maps `T`→`Point`.

---

## 3. The reference-grade fix shape (prototyped, sub-slice 4a)

Mirrors Rust `evaluate_delayed_meta_block`'s `Stmt::MetaFor` List-expression arm
(`meta.rs:3434-3468`): evaluate the iterable to a list, bind the loop-var(s) per item,
substitute into a clone of the body, recurse (for nested `meta if`), splice.

1. **AST** (`ast.gg:121`): `SMetaFor(Vector[Stmt])` → `SMetaFor(Vector[String], SpannedExpr, Vector[Stmt])`
   (loop-vars + iterable + body). Mirrors meta #3's `SMetaIf`-carries-condition.
2. **Parser** (`parser.gg:3026`): capture `v1[, v2]* in <iterable>:` (reuse the existing
   `parse_meta_for_var_name` helper, same shape as the in-match arm). Arity ripple at the
   in-match `meta if` wrapper (`:3325`) + the `SMetaFor(_)` wildcard sites (loader/lower/
   lower_stmt) → `SMetaFor(_, _, _)`, and the case-binding sites (resolve×2, typecheck, format).
3. **Engine** (`lower_generics.gg`): new `SMetaFor` arm in `evaluate_delayed_meta_stmts` (now
   threading `&gmod`):
   - `eval_meta_fields(iter, tn_env, gmod) → [(name, demangled_type)]` reading `gmod.type_infos`.
   - `eval_meta_variant_names(iter, tn_env, gmod) → [name]` reading `gmod.enum_registry`.
   - `demangle_field_type_name` reverses `prim_to_c_name` (`double`→`float`, `GorgetString`→
     `String`, `int64_t`→`int`, …) so `fields()` reports SOURCE-level names, matching Rust's
     `Str`/`GorgetString`→`String` normalization (`meta.rs:3005-3008`). This is the de-mangle
     boundary (the type_info `type_name` is the C contract; the reverse map is the one place it
     is spelled back to the user surface — analogous to the C-emit symbol boundary).
   - `subst_mf_expr`/`subst_mf_stmts`: bind the loop var → `EStringLiteral` value, walking
     f-string parts (so `f"{fname}:{ftype}"` prints) + the ECall-callee identifier RENAME (so
     `vname(w)`→`Circle(w)`, mirroring Rust `substitute_expr`'s `Expr::Call` special-case) +
     the `EIs` LHS (so `ftype is numeric` post-subst is `"int" is numeric`).
   - `meta_type_category_is` + an `EIs` arm in `eval_delayed_meta_cond` for `ftype is numeric`
     (and `integer`/`float`/`string`/`bool`).
4. **Resolver** (`resolve.gg:563,974`): bind the loop vars in the body scope
   (`scopes.define(mfv, DkVariable(), …)`) — makes `fname`/`ftype`/`vname` genuinely DEFINED
   (Case-B residual, §5), not allow-listed.

**Self-host CoW lesson learned during prototyping (NOT a compiler gap):** a `subst` function
that returns the borrowed match-binding `se` unchanged (passthrough) and stores it into an
OWNED reconstructed parent node (`SMetaIf(subst(cond), …)`) creates a dangling Box → segfault
on the next deref. The fix is the correct CoW shape: `subst_mf_expr` must ALWAYS return an
OWNED expr (rebuild every arm, never `return se` for a node whose result is stored owned).
Confirmed: owned rebuild = no crash + correct output. This is the self-host's
CoW-default-borrow model working as designed, not a bug.

### MEASURED end-to-end yield (regenerated this session)
- runtime parity **747 → 748 (+1)**, `meta_fields` WRONG→MATCH, **0 regressions**
  (whole-corpus non-match-set diff: 1-in / 0-out).
- `bootstrap_fixed_point` **GREEN** (re-converged — the driver self-compiles its OWN AST/parser/
  engine changes to a byte-identical stage-2==3==4 fixed point; this is the load-bearing
  internal-consistency check, since the driver's own source uses these primitives).
- `type_comparison` **1222/72 — byte-identical to baseline** (isolated re-run from the rebuilt
  driver). `resolver`/`parser`/`lexer` comparison use independent dirs (not touched), 0 crashes.

---

## 4. Reference-grade gate (Core #8)

`meta_fields` produces CORRECT output (byte-exact Rust-oracle MATCH), not merely non-failing.
The change makes the meta names genuinely DEFINED + the loop genuinely UNROLLED (the
reference-grade fix), NOT allow-listed or suppressed — satisfying "Don't redesign around
compiler gaps." No KNOWN DEFECT is shipped: the fixtures still WRONG (`meta_enum_ordinal`,
`meta_reflection`, `meta_delayed_for`, `meta_variant_payloads`) are UNTOUCHED by 4a and are
filed as the §6 follow-on sub-slices — none regressed, none is masked.

---

## 5. Which of the 22 Case-B residual names this clears

The β scout's 22-residual is dominated by `vname` (×280), `fname`, `ftype`, `idx`.

- **`fname`, `ftype`** (`meta_fields`, `dataframe_*` derive family): the `resolve.gg` SMetaFor
  loop-var binding now `scopes.define`s them in the body scope → genuinely DEFINED. **CLEARED**
  (reference-grade — bound, not allow-listed).
- **`vname`** when it is a STATEMENT-body `meta for vname in variant_names(T):` loop var
  (`meta_enum_ordinal`): now bound by the same resolve.gg arm → **CLEARED** for that shape.
- **`vname` in `meta_variant_payloads`** is a `SMetaForMatch` (match-arm) loop var, a SEPARATE
  construct (`expand_meta_for_arms`, `meta.gg:1248`) — **NOT cleared by 4a** (sub-slice 4c).
- **`idx`** comes from `meta const idx = enum_ordinal(T, vname)`, a `meta const` STATEMENT that
  the parser DISCARDS as bare `SMeta()` (§2). My change does NOT bind `idx` — **NOT cleared**
  (sub-slice 4b needs in-body `meta const` parsing).

**Caveat (re-verified against the β scout §1b):** the self-host's `EStringLiteral` resolve arm
is a bare `pass` (`resolve.gg`), so it does NOT walk f-string interp exprs — meaning today
these names leak ONLY at the LOWERING-stage EIdentifier-miss, not the resolve diagnostic. So
"CLEARED" here means: **when the Track-β undefined-name flip lands, the SMetaFor loop-var
binding makes `fname`/`ftype`/`vname`(stmt) genuinely DEFINED so the flip will not
false-reject them** — exactly the reference-grade unblock the β scout's escalation #2 demands
(α binds the names so β can flip). It is the right half of the α/β ordering constraint.

---

## 6. Decompose? — YES. Sub-slice roadmap + the first shippable increment

Slice #4 is too large for one increment (the owner's step-6 hypothesis is confirmed). It splits
into 3 independent sub-slices, gated by DIFFERENT parser/AST gaps:

- **Sub-slice 4a — `fields()` + `variant_names()` statement-body unroll (THIS prototype).**
  Yield **+1** (`meta_fields`). Clears `fname`/`ftype`/`vname`(stmt) in the resolver. Needs only
  the `SMetaFor` AST carry + parser capture + the engine unroll + `ftype is numeric`. **SHIP
  THIS FIRST.** Zones: `ast.gg`/`parser.gg`/`resolve.gg`/`typecheck.gg`/`format.gg` (shared,
  symlinked into lowerer+check) + `lower_generics.gg`/`loader.gg`/`lower.gg`/`lower_stmt.gg`
  (lowerer real files).

- **Sub-slice 4b — in-body `meta const` + `enum_ordinal`/`enum_from_ordinal`/`variant_count` +
  int-range `meta for i in 0..N`.** Needed for `meta_enum_ordinal` (the `idx` and `print_names`
  halves). Requires a NEW `SMetaConst(String, Type, SpannedExpr)` AST stmt + parser support
  (`parser.gg:3074` currently discards `meta const` in a function body) + a small int-MetaValue
  env in the engine (the unroll already supports the `0..N` shape via the existing
  `eval_delayed_meta_range`; wire it to the SMetaFor arm). Estimated +1 (`meta_enum_ordinal`);
  also unblocks `idx` in the 22-residual. MEDIUM.

- **Sub-slice 4c — `SMetaForMatch` body completeness (return-position ctor-call subst).** Needed
  for `meta_variant_payloads`. The `expand_meta_for_arms` (`meta.gg:1248`) substitutes `vname`
  into the pattern HEAD but not the return-position `vname(rewrap[T](w))` ctor call. Port Rust
  `substitute_expr`'s `Expr::Call` callee-rename into `substitute_arm`. Estimated +1; clears
  `vname` in the SMetaForMatch shape. Disjoint from 4a/4b (touches `meta.gg`'s expand, not the
  lowerer engine).

A FOURTH, larger track (`meta_reflection`: `field_count`/`has_field`/`field_names`/`field_type`
meta-if predicates + `meta for … in field_names(T)`) is downstream of 4a/4b's infrastructure.

**Recommendation: ship sub-slice 4a (this prototype, productionized per §7) as the keystone
α-increment — measured +1, parity-neutral, bootstrap-green, and it clears the
`fname`/`ftype`/`vname`(stmt) residual names, which is the load-bearing α→β unblock.**

---

## 7. Zones, disjointness, and what productionizing 4a still needs

**Slice-4a zone (shared, symlinked into self_host_{lowerer,check}):**
- `self_host_typechecker/ast.gg:121` (the `SMetaFor` arity)
- `self_host_typechecker/parser.gg:3026,3325` (capture + in-match arity)
- `self_host_typechecker/resolve.gg:563,974` (loop-var binding) — **EXPRESSION-resolution +
  stmt-resolution arms; DISJOINT from Track-β's EIdentifier-miss arm (β scout §5).** The two
  tracks share only `resolve.gg` and edit non-overlapping line ranges → clean merge.
- `self_host_typechecker/typecheck.gg:1264`, `format.gg:376` (arity)

**Slice-4a zone (lowerer real files):**
- `lower_generics.gg` (the engine + helpers — the bulk of the diff)
- `loader.gg:427,766,1026`, `lower.gg:593`, `lower_stmt.gg:794` (arity ripples)

**Disjointness vs Track β:** β owns `resolve.gg`'s EIdentifier-miss arm + the import-allow-set +
`scope.gg` variant-query + `loader.gg`'s `imported_bare_names` carrier. 4a owns the
SMetaFor stmt/expr-resolution arms + the meta engine. The only shared file is `resolve.gg`
(disjoint regions, per β scout §5) and `loader.gg` (β: `load_imports` signature; 4a: the
`SMetaFor(_, _, _)` arity in `block_mentions_iter`/`has_real_body` — different functions). Clean.

**What this prototype does NOT yet do (REQUIRED before integration):**
1. **Independent-copy ripple.** `self_host_parser/ast.gg:116`+`parser.gg:2745`+`format.gg:480`
   and `self_host_resolver/ast.gg:121`+`parser.gg:2724`+`resolve.gg:611,1042`+`format.gg:480`
   carry their OWN `SMetaFor(Vector[Stmt])`. The prototype did NOT touch them (the runtime path
   doesn't use them), so `parser_comparison`/`resolver_comparison` are UNAFFECTED — but
   production MUST ripple the arity there too (else those drivers won't build the upgraded AST),
   OR keep them at the old arity by NOT sharing the AST (they are independent copies, so they
   can stay 1-arity if their parser keeps `skip_meta_header`). Decision for the executor: ripple
   all copies for consistency, or leave the independent copies on the old shape (parity-neutral
   either way, since they're only exercised by their own comparison tests). The prototype proves
   the LOWERER+TYPECHECKER path; the independent copies are the integration tail.
2. **Lock-in snapshot.** Add `tests/fixtures/runtime_snapshots/meta_fields.out` so
   `self_host_runtime` pins the flip (it's a stable MATCH — verified self-host twice + oracle).
3. **`SMetaFor(_, _, _)` arm-count lint** (optional, per CLAUDE.md "fix the class"): the
   in-match `meta if`→`SMetaFor` hack (`parser.gg:3325`) and the statement-body arm now BOTH
   build the 3-arity node; a lint guards the next sibling.

---

## 8. Reproduce

```bash
# baseline
git stash push -m TMP <the 9 files>      # revert prototype
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_runtime_diff -- --nocapture     # → 747/1069
cargo test --test integration --release type_comparison -- --nocapture   # → 1222/72
git stash pop                            # restore prototype

# prototype
GG_BUILD_TIMEOUT_SECS=600 ./target/debug/gg build \
    tests/fixtures/self_host_lowerer/driver.gg -o /tmp/d
/tmp/d tests/fixtures/meta_fields.gg lib --emit-c \
    --runtime-dir=src/backend/c/runtime > /tmp/m.c && cc -O0 -w -o /tmp/m /tmp/m.c -lm -lpthread && /tmp/m
./target/debug/gg run tests/fixtures/meta_fields.gg     # oracle — byte-identical
GG_RUNTIME_DIFF=1 … self_host_runtime_diff               # → 748/1069 (+1, meta_fields)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_bootstrap_fixed_point                      # GREEN
cargo test --test integration --release type_comparison -- --nocapture   # → 1222/72 (neutral)
```

## 9. Docs the design rests on
- `docs/language-reference.md` — meta `for`/`fields()`/`variant_names()` semantics.
- `src/semantic/meta.rs:2990-3063` (`fields`/`variant_names`/`variant_count` reflection, the
  `Str`/`GorgetString`→`String` normalization), `:3300-3481`
  (`evaluate_delayed_meta_block`'s `Stmt::MetaFor` List arm — the reference unroll),
  `:2299-2315` (`substitute_expr`'s `Expr::Call` callee-rename).
- `docs/plans/meta-for-binding-scout.md` — the REFUTED naive "bind SMetaFor loop vars yields 0"
  premise (it does, BECAUSE the f-string sink); this scout's engine UNROLLS (the real fix),
  which is why the yield is +1 not 0.
- `docs/plans/caseb-track-beta-scout.md` §5 (the α/β disjointness + the ordering constraint:
  α binds the meta names so β can flip without false-rejecting), §3 (the 22-residual).
- `docs/plans/ill-typed-case-b-scout.md` §Increment-3 (the meta construct list).
- CLAUDE.md — Core-#8 (reference-grade gate: `meta_fields` is CORRECT, not just non-failing),
  "Don't redesign around compiler gaps" (the names are BOUND/UNROLLED, not allow-listed),
  "Typed metadata, never name-matching" (the iterable+vars are TYPED AST fields written once at
  the parser), "Re-verify a premise … MEASURE end-to-end" (the +1 / 0-regress / bootstrap-green
  numbers were all regenerated this session).
