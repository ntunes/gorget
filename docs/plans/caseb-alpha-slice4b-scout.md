# Scout — Case-B Track-α SLICE 4b: in-body `meta const` + `enum_ordinal`/`enum_from_ordinal`/`variant_count` + integer-range `meta for i in 0..N`

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-24, worktree off
`gorget-1` tip `1ee85d46` (the slice-4a reflection engine, `d3981a00`). Every number below
was regenerated THIS session by building the prototype self-host lowerer driver, running it
over the full **1069-fixture** parity corpus (`GG_RUNTIME_DIFF=1 … self_host_runtime_diff`),
and cross-checking both target fixtures against the Rust `gg run` oracle whole-stdout. The
prototype is committed as throwaway `2010d5fc` (`PROTOTYPE(scout): … DO NOT INTEGRATE`) — it
must NOT be integrated as-is (no independent-copy ripple / snapshot / lint; see §7).

This RIDES ON the just-landed 4a engine (the per-monomorphization `SMetaFor` unroll in
`lower_generics.gg` `evaluate_delayed_meta_stmts` reading `gmod.type_infos`/`gmod.enum_registry`)
and is the next Track-α increment: it clears the residual meta-var name **`idx`** and completes
the **int-range** half (the `i`/`vname` int-range shape).

Grounded in `docs/language-reference.md` §19.10/§19.7 (delayed `meta for`/`meta const` semantics
— "evaluated at monomorphization time when the type parameters are concrete", `meta const`/range
are compile-time expressions), Rust `src/semantic/meta.rs` (the reference delayed-meta engine:
`evaluate_delayed_meta_block` `:3300`, the `Stmt::MetaConst` bind `:3320`, the integer-range
`Stmt::MetaFor` arm `:3403`, `eval_delayed_meta_range` `:3590`, the `variant_count`/`enum_ordinal`/
`enum_from_ordinal` builtins `:3045`/`:3129`/`:3157`, `substitute_expr`'s `meta_value_to_expr`
Int-vs-Str literal materialization `:2466-2468`), and the slice-4a scout
(`docs/plans/caseb-alpha-slice4-scout.md` §6 sub-slice 4b — the decomposition that defined this slice).

---

## 0. Headline

| metric | command | baseline (`1ee85d46`) | THIS scout's prototype (`2010d5fc`) |
|---|---|---|---|
| runtime parity | `self_host_runtime_diff` | **748/1069 = 70.0%** | **750/1069 = 70.2% (+2)** |
| fixtures flipped to MATCH | (whole-corpus non-match-set diff) | — | **`meta_delayed_for` + `meta_enum_ordinal` (2)** |
| regressions | (whole-corpus non-match-set diff) | — | **0** |
| `bootstrap_fixed_point` | `self_host_bootstrap_fixed_point` | GREEN | **GREEN (re-converged, 326s)** |
| `type_comparison` | `type_comparison --nocapture` | 1180 exact + 42 superset = 1222, 72 mismatched, 0 crashed | **1180+42=1222 / 72 / 0 — BYTE-IDENTICAL** |
| `c_emit_comparison` | `c_emit_comparison --nocapture` | (4a) ~1034 matched, 0 self-host crashes | **1034 matched, 149 mismatched, 0 self-host crashes** |
| `resolver_comparison` | `resolver_comparison --nocapture` | independent dir — untouched | **1275 matched / 19 mismatched / 0 crashed (unaffected)** |
| `parser_comparison` | `parser_comparison` | independent dir — untouched | **ok (unaffected)** |
| `self_host_runtime` lock-in net | `self_host_runtime` | GREEN | **GREEN** |

**Bottom line:** 4b is REAL and CLEAN. Both `meta_delayed_for` and `meta_enum_ordinal` go from
WRONG-OUTPUT to **byte-exact MATCH** of the Rust oracle, with **0 regressions** anywhere, the
bootstrap re-converges (the driver self-compiles its OWN new `SMetaConst` AST + the int-MetaValue
engine to a byte-identical fixed point), and every structural diagnostic is neutral or unaffected.
**+2 measured.** This clears the `idx` residual name (genuinely BOUND in the resolver, not
allow-listed) and lands the int-range `meta for i in 0..N` shape.

---

## 1. The failing fixtures + reproduced divergence (measured this session)

`d_proto F lib --emit-c --runtime-dir=src/backend/c/runtime` → `cc -O0` → run, diffed against
`gg run F` (Rust oracle).

| fixture | construct | Rust oracle | self-host BASELINE | prototype (4b) |
|---|---|---|---|---|
| **`meta_enum_ordinal`** | `meta for vname in variant_names(T):` + `meta const idx = enum_ordinal(T, vname)`; AND `meta for i in 0..variant_count(T):` + `meta const vname = enum_from_ordinal(T, i)` | `North=0…West=3`/`Red=0…Blue=2`/`Red`/`Green`/`Blue`/`done` | `North=0`/`East=0`/…/`Blue=0`/`done` — variant NAMES unroll (4a) but **`idx`=0** (`meta const` discarded at parse) AND **`print_names` int-range inert** (the `Red/Green/Blue` lines missing) | **byte-exact MATCH** ✅ |
| **`meta_delayed_for`** | int-range `meta for i in 0..3:` + nested `meta if i == 0 / elif i == 1 / else` (the int loop-var flows into the meta-if predicate) | `first/second/third`×2 + `int/float/unknown` | `int/float/unknown` only — the **whole int-range loop dropped** (the 6 `first/second/third` lines missing) | **byte-exact MATCH** ✅ |

**WHY baseline was wrong (root cause, traced this session):**
- `parser.gg` (RE-GREPPED this session) discards in-body `meta const` at the `else` arm of the
  in-body meta-statement dispatch — **line 3074** (`else: self.skip_meta_rest(); return SMeta()`).
  There is **NO `SMetaConst` AST variant** (confirmed: `ast.gg` had only `SMeta`/`SMetaFor`/`SMetaIf`/
  `SMetaForMatch`). So `idx` never binds → its f-string slot prints `0`.
- The 4a `SMetaFor` engine arm handled only the LIST iterables (`fields(T)` 2-var, `variant_names(T)`
  1-var); the **integer-range** `0..N` shape fell through to `if not handled: pass` → the whole loop
  body vanished. So `meta_delayed_for`'s `0..3` loop and `meta_enum_ordinal`'s `0..variant_count(T)`
  loop were both inert.

**Verified flip/regression set** (strict fixture-name diff of base-vs-proto non-match sets over the
whole 1069-fixture corpus): exactly **TWO fixtures left the non-match set
(`meta_delayed_for`, `meta_enum_ordinal`)**, and **ZERO entered it**.

---

## 2. The WRITE-site gap, traced across ALL layers (file:line)

Same class as 4a (`SMetaIf`/`SMetaFor`): the parser discards the construct's data, and the engine
lacks the int-valued env to evaluate it. Two write-site gaps (parser discard + engine string-only env).

| layer | file:line (baseline) | gap |
|---|---|---|
| **AST** | `self_host_typechecker/ast.gg` (after `SMetaIf` `:132`) | no `SMetaConst` variant — in-body `meta const` had nowhere to land. |
| **Parser** | `self_host_typechecker/parser.gg:3074` | in-body `meta const <name> = <expr>` falls into `else: skip_meta_rest(); return SMeta()` — name AND value both discarded. (Contrast the ITEM-level `IMetaConst(Type, name, val)` parse at `:1319`, and the Rust in-body `parse_meta_const_stmt` `stmt.rs:999` which is `meta const <name> = <expr>` — **no explicit type**, value-inferred.) |
| **Engine — int env** | `lower_generics.gg` `evaluate_delayed_meta_stmts:1083`, `eval_delayed_meta_cond:661` | threaded only `tn_env: Dict[String, String]` — NO int/string MetaValue env, so `meta const idx`/the int loop-var `i` had nowhere to bind, and `meta if i == 0` could not see `i`. |
| **Engine — int-range arm** | `lower_generics.gg` `SMetaFor` arm `if not handled: pass` (`:1208` baseline) | no integer-range case — `0..N` left inert. (No `eval_delayed_meta_range` analogue existed; the iterable was only ever evaluated as `fields()`/`variant_names()`.) |
| **Engine — enum int builtins** | `lower_generics.gg` `eval_meta_builtin_int:632` | only `sizeof`/`bitwidth` — no `variant_count(T)`/`enum_ordinal(T, "V")`; no `enum_from_ordinal(T, n)` string evaluator. |
| **Subst** | `lower_generics.gg` `subst_mf_expr:808` | the loop-var subst ALWAYS produced `EStringLiteral` — an int value (`idx`/`i`) would have rendered `f"{idx}"` as `"0"` (string) and broken `meta if i == 0` (int). |

The data the engine needs was already present at lowering time (4a established this):
`gmod.enum_registry: Dict[String, Vector[String]]` (variant names → positions), and `tn_env`
maps `T`→`Color`. The int-MetaValue env is the only NEW state.

---

## 3. The reference-grade fix shape (prototyped, sub-slice 4b)

Mirrors Rust `evaluate_delayed_meta_block` (`meta.rs:3300`): for `MetaConst`, evaluate the value,
bind into `local_env`, substitute the remaining stmts, remove; for integer-range `MetaFor`, evaluate
the bounds via `eval_delayed_meta_range`, bind the loop var per `val in start..upper` into a child
env, substitute, recurse; and the leaf identifier substitution uses `meta_value_to_expr` to pick
`IntLiteral` vs `StringLiteral` (`meta.rs:2466`).

The prototype's cross-layer shape (all in `self_host_typechecker/{ast,parser,resolve,format}.gg` —
shared/symlinked into the lowerer driver — plus `self_host_lowerer/{lower_generics,lower_stmt,lower}.gg`):

1. **AST** (`ast.gg`, after `SMetaIf`): `SMetaConst(String, SpannedExpr)` = name + value.
   **NOTE — the slice-4 scout §6 proposed `SMetaConst(String, Type, SpannedExpr)`; that is WRONG.**
   Rust's in-body `meta const` has NO explicit type (`stmt.rs:999` parses `const <name> = <expr>`,
   type inferred from the value). Use the 2-field shape.
2. **Parser** (`parser.gg:3074`): add `elif self.check_kw(KW_CONST):` before the discard `else` —
   `advance` past `const`, `token_ident` the name, `expect_tok(TOK_EQ)`, `parse_expr` the value,
   `skip_newlines`, return `SMetaConst(name, value)`. `KwConst`/`KW_CONST=72` already exists
   (`lexer.gg:155`, `parser.gg:150`).
3. **Engine — int MetaValue env** (`lower_generics.gg`): thread `int_env: Dict[String, int]`
   alongside `tn_env` through `evaluate_delayed_meta_stmts`, `eval_delayed_meta_cond`, and (new)
   `eval_meta_int_v2`/`eval_meta_str_v2`. New helpers:
   - `eval_meta_int_v2(se, tn_env, int_env, gmod, ok)` — int literal, bound int name (`int_env`),
     `sizeof`/`bitwidth(T)`, `variant_count(T)` (= `enum_registry.get(T).len()`),
     `enum_ordinal(T, "V")` (= position of `"V"` in the registry). Mirrors `meta.rs:3045`/`:3129`.
   - `eval_meta_str_v2(se, tn_env, int_env, gmod, ok)` — string literal, `enum_from_ordinal(T, n)`
     (= `enum_registry.get(T)[n]`). Mirrors `meta.rs:3157`.
   - `resolve_meta_tparam(arg, tn_env)` — the shared `T`→concrete-name router.
4. **Engine — `SMetaConst` arm** in `evaluate_delayed_meta_stmts`: evaluate the (already loop-var-
   substituted) value, prefer int → `cv_iv`/`int_env`, else string → `cv_sv`; DROP the stmt. The
   block-scan accumulates `cv_sv`/`cv_iv` and substitutes them into each SUBSEQUENT statement at the
   top of the loop (the self-host's left-to-right analogue of Rust's "substitute remaining stmts").
5. **Engine — integer-range `SMetaFor` arm**: when `mf_iter` is `ERange(start, end, inclusive)` with
   one loop var, eval the bounds via `eval_meta_int_v2` (so `0..variant_count(T)` works), then for
   `rv in start..upper` bind `rv` into a child `int_env` COPY + an int subst-map so `i` renders as an
   `EIntLiteral` and any nested `meta const`/`meta if i == N` sees it, then recurse.
6. **Subst — int vs string** (`subst_mf_expr`/`subst_mf_stmts`): add `iv: Dict[String, int]`. In the
   `EIdentifier` arm, check `iv` first → `EIntLiteral` (so `f"{idx}"`→`0`, not `"0"`), then `sv` →
   `EStringLiteral` (the 4a path). Add a `SMetaConst` arm to `subst_mf_stmts` so the const's value
   expr gets the outer loop-var substituted (`enum_ordinal(T, vname)`→`enum_ordinal(T, "North")`) —
   **this was the one bug found during prototyping** (without it, `idx` stayed 0 because the meta-
   const value was never substituted; mirrors Rust's `substitute_stmt` recursing into `MetaConst`).
7. **Resolver** (`resolve.gg`, `resolve_stmt` SMeta arm region): add `SMetaConst(mc_name, mc_val):
   scopes.define(mc_name, DkVariable(), …)` — binds `idx` genuinely (Track-α residual, §5), NOT
   allow-listed. (The 2nd resolve copy `:1001` has an `else: pass` catch-all — no arm needed.)
8. **Arity ripple** (exhaustive-match sites): `format.gg` (`SMetaConst → "meta"`), `lower_stmt.gg`
   (`SMetaConst(_,_): pass`, the residual no-op for non-generic contexts that bypass the engine),
   `lower.gg` `stmt_kind` (`SMetaConst → "SMetaConst"`). `typecheck.gg`'s stmt match has `else: pass`
   — covered. `discover_generic_calls_stmt` has `else` — covered.

### MEASURED end-to-end yield (regenerated this session)
- runtime parity **748 → 750 (+2)**, both `meta_delayed_for` and `meta_enum_ordinal` WRONG→MATCH,
  **0 regressions** (strict whole-corpus non-match-set diff: 2-out / 0-in).
- `bootstrap_fixed_point` **GREEN** (re-converged, 326s — the driver self-compiles its own new
  AST/parser/engine to a byte-identical stage-2==3==4 fixed point; the load-bearing internal-
  consistency check).
- `type_comparison` **1180+42=1222 / 72 / 0 — byte-identical** to the 4a baseline.
- `c_emit_comparison` **1034 matched / 149 mismatched / 0 self-host crashes**.
- `resolver`/`parser` comparison — independent dirs, **untouched** (`self_host_parser`/`self_host_resolver`
  have their OWN `ast.gg`/`parser.gg` at the old shape; `git status` confirms 0 edits there): 1275/19/0
  and `ok`, both unaffected — the identical pattern to 4a's independent-copy isolation.

---

## 4. Reference-grade gate (Core #8)

Both flipped fixtures produce **CORRECT** output (byte-exact Rust-oracle MATCH verified twice this
session), not merely non-failing. The values are **genuinely evaluated**, not snapshot-pinned or
allow-listed:
- `meta_enum_ordinal` prints `North=0/East=1/South=2/West=3/Red=0/Green=1/Blue=2/Red/Green/Blue/done`
  — the ordinals are computed by `enum_ordinal` against `gmod.enum_registry` (a real position lookup),
  and the int-range `0..variant_count(T)` is a real registry-length bound. `idx` is genuinely BOUND
  in the resolver (`scopes.define`), not suppressed.
- `meta_delayed_for` prints `first/second/third`×2 + `int/float/unknown` — the int loop-var `i`
  flows into `meta if i == 0/1` as a real int comparison.

No KNOWN DEFECT is shipped. The fixtures still WRONG (`meta_reflection`, `meta_variant_payloads`,
`meta_implements`, etc.) are UNTOUCHED by 4b and remain filed as later slices (§6); none regressed,
none is masked. No allow-list, no snapshot-pinned wrong result, no compiler-gap dodge — the names are
BOUND and the loops genuinely UNROLLED. Satisfies "Don't redesign around compiler gaps" and Core-#8.

---

## 5. Which of the 22 Case-B residual names this clears

The β scout's 22-residual (`caseb-track-beta-scout.md` §2/§5) is dominated by `vname`×280, then
`fname`, `ftype`, `idx`. 4a cleared `fname`/`ftype`/`vname`(stmt-body loop var). **4b clears `idx`:**

- **`idx`** — comes from `meta const idx = enum_ordinal(T, vname)`. 4b adds the `SMetaConst` resolve
  arm (`scopes.define(mc_name, …)`), so `idx` is genuinely DEFINED in the body scope → the Track-β
  undefined-name flip will not false-reject it. **CLEARED (reference-grade — bound, not allow-listed).**
- **The int-range loop var (`i` in `meta for i in 0..N`)** — bound by the 4a `SMetaFor` resolve arm
  (it already `scopes.define`s the loop var; 4b extends the ENGINE to actually unroll the int-range,
  but the resolve-side binding was already present). So the int-range `vname`/`i` resolve cleanly.

**Still NOT cleared (→ 4c / later):** `vname` in `meta_variant_payloads` is a `SMetaForMatch`
(match-arm) loop var — a SEPARATE construct (`expand_meta_for_arms`, `meta.gg`), untouched by 4b
(§6 sub-slice 4c). The bulk `vname`×280 hits are mostly the `SMetaForMatch` family + `field_access.gg`'s
f-string (a resolve-EStringLiteral-walk gap, β scout §1b).

**Caveat (same as 4a):** the self-host's `EStringLiteral` resolve arm doesn't walk f-string interp
exprs, so these names leak at the LOWERING-stage EIdentifier-miss, not the resolve diagnostic.
"CLEARED" means: when the Track-β undefined-name flip lands, the `SMetaConst` binding makes `idx`
genuinely DEFINED so the flip won't false-reject it — the right half of the α/β ordering constraint
(α binds the names so β can flip). This is exactly the unblock the β scout §5 demands.

---

## 6. Decompose? — NO further split needed for 4b; the targets are exhausted by it

4b as scoped (in-body `meta const` + the 3 enum builtins + int-range) is a SINGLE coherent
increment — the three pieces are entangled at one read site (the int-MetaValue env): `meta const idx`
needs the int env to bind, `enum_ordinal`/`variant_count` need it to produce ints, and the int-range
loop needs it for the loop var. You cannot land one without the env, and once the env exists all three
fall out together. The prototype proves both target fixtures flip with the single change. **Ship 4b as
one increment.** (One could split "int-range alone" — it would flip `meta_delayed_for` for +1 — but
`meta_enum_ordinal` needs all three, and the env is shared, so splitting buys nothing.)

The REMAINING meta cluster (unchanged by 4b, for later slices):
- **Sub-slice 4c — `SMetaForMatch` body completeness** (`meta_variant_payloads`): the return-position
  `vname(rewrap[T](w))` ctor-call subst in `expand_meta_for_arms` (`meta.gg`). Disjoint from 4b
  (touches `meta.gg`'s expand, not the lowerer engine). Clears `vname` in the `SMetaForMatch` shape.
- **`meta_reflection`** (`field_count`/`has_field`/`field_names`/`field_type` meta-if predicates +
  `meta for … in field_names(T)`): downstream of 4a/4b infra; a separate predicate-builtin slice.
- **`meta_implements`/`meta_type_is`/`meta_numeric_meta`/`meta_platform_guard`/`meta_while`** — each a
  distinct meta predicate/builtin family (still WRONG in both baseline and proto, untouched).

---

## 7. Zones, disjointness, and what productionizing 4b still needs

**Slice-4b zone (shared, symlinked into self_host_{lowerer,check}):**
- `self_host_typechecker/ast.gg` (the `SMetaConst` variant)
- `self_host_typechecker/parser.gg:3074` (the `meta const` capture)
- `self_host_typechecker/resolve.gg` (the `SMetaConst` arm in `resolve_stmt` — the **Track-α
  statement-resolution zone**, β scout §5 `:584-605`; DISJOINT from Track-β's EIdentifier-miss expr
  arm at `:631`)
- `self_host_typechecker/format.gg` (the `SMetaConst` arity)

**Slice-4b zone (lowerer real files):**
- `lower_generics.gg` (the int-env engine + `eval_meta_int_v2`/`eval_meta_str_v2`/`resolve_meta_tparam`
  + the `SMetaConst` + int-range arms + the int-vs-string subst — the bulk of the diff)
- `lower_stmt.gg` (the `SMetaConst(_,_): pass` residual no-op)
- `lower.gg` `stmt_kind` (the `SMetaConst` arity)

**Disjointness vs Track β:** β owns `resolve.gg`'s EIdentifier-miss arm (`:631`) + import allow-set +
`scope.gg` variant-query + `loader.gg`'s `imported_bare_names`. 4b owns the `SMetaConst`
stmt-resolution arm (`resolve_stmt`, ~`:560`) + the meta engine. The only shared FILE is `resolve.gg`,
and the edits are in disjoint functions / non-overlapping line ranges (β scout §5 confirms the meta-stmt
arms `:584`/`:1001` are >40 lines from the expr-miss `:631`). **Clean merge.**

**Disjointness vs A_closure** (`caseb-aclosure-scout.md`): A_closure owns lexer keywords + Box-lowering.
4b touches neither. **Disjoint.**

**What this prototype does NOT yet do (REQUIRED before integration):**
1. **Independent-copy ripple.** `self_host_parser/{ast,parser,format}.gg` and
   `self_host_resolver/{ast,parser,resolve,format}.gg` carry their OWN AST without `SMetaConst`. The
   prototype did NOT touch them (the runtime path doesn't use them) → `parser_comparison`/
   `resolver_comparison` UNAFFECTED. Same decision as 4a: production can ripple all copies for
   consistency, OR leave the independent copies on the old shape (parity-neutral either way, since
   they keep `skip_meta_header`/the `SMeta()` discard and are exercised only by their own comparison
   tests). The prototype proves the LOWERER+TYPECHECKER path; the independent copies are the
   integration tail.
2. **Lock-in snapshots.** Add `tests/fixtures/runtime_snapshots/meta_delayed_for.out` AND
   `meta_enum_ordinal.out` so `self_host_runtime` pins the flips (both are stable MATCH — verified
   self-host + oracle byte-exact this session). (`meta_fields.out` from 4a is present.)
3. **`SMetaConst` arm-count / int-vs-string-subst lint** (optional, per CLAUDE.md "fix the class"):
   the `subst_mf_*` int/string split and the engine arms now both build/read the int env; a lint
   guards the next sibling builtin (e.g. `enum_size`, `field_offset`) from silently skipping the env.

---

## 8. Reproduce

```bash
git merge --ff-only gorget-1            # tip 1ee85d46
cargo build                             # ./target/debug/gg

# baseline (revert the 7 prototype files, or check out 1ee85d46)
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_runtime_diff -- --nocapture     # → 748/1069
cargo test --test integration --release type_comparison -- --nocapture   # → 1180+42=1222 / 72 / 0

# prototype (commit 2010d5fc)
GG_BUILD_TIMEOUT_SECS=600 ./target/debug/gg build \
    tests/fixtures/self_host_lowerer/driver.gg -o /tmp/d_proto
for F in meta_enum_ordinal meta_delayed_for; do
  /tmp/d_proto tests/fixtures/$F.gg lib --emit-c --runtime-dir=src/backend/c/runtime \
      > /tmp/$F.c && cc -O0 -w -o /tmp/$F /tmp/$F.c -lm -lpthread
  diff <(/tmp/$F) <(./target/debug/gg run tests/fixtures/$F.gg) && echo "$F BYTE-EXACT"
done
GG_RUNTIME_DIFF=1 … self_host_runtime_diff               # → 750/1069 (+2)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_bootstrap_fixed_point                      # GREEN (326s)
cargo test --test integration --release type_comparison  -- --nocapture   # → 1222/72/0 (neutral)
cargo test --test integration --release c_emit_comparison -- --nocapture  # → 1034 matched / 0 sh-crash
cargo test --test integration --release resolver_comparison -- --nocapture # → 1275/19/0 (unaffected)
```

## 9. Docs the design rests on
- `docs/language-reference.md` §19.10 (delayed `meta if`/`meta for` — monomorphization-time eval),
  §19.7 (`meta const`/range are compile-time expressions, same rules as module-level meta),
  §18.10 (`meta for` parameterized tests — the int-range/list-iterable surface), §19.9 (`meta log`
  accepts meta const names / `typename(T)` / `sizeof(T)` — the meta-expr grammar).
- Rust `src/semantic/meta.rs`: `evaluate_delayed_meta_block` `:3300` (the reference engine),
  `Stmt::MetaConst` bind+substitute+remove `:3320`, integer-range `Stmt::MetaFor` arm `:3403`,
  `eval_delayed_meta_range` `:3590`, `variant_count` `:3045` / `enum_ordinal` `:3129` /
  `enum_from_ordinal` `:3157`, `substitute_expr`'s `meta_value_to_expr` Int-vs-Str literal
  materialization `:2466`. Self-host in-body `meta const` parse reference: `src/parser/stmt.rs:999`
  (`meta const <name> = <expr>`, NO explicit type — corrects the slice-4 scout's `SMetaConst(String,
  Type, SpannedExpr)` proposal to the 2-field shape).
- `docs/plans/caseb-alpha-slice4-scout.md` §6 (the sub-slice 4b decomposition + 4a's measured base),
  §3 (the 4a engine the int env extends), §5 (the residual-names framing).
- `docs/plans/caseb-track-beta-scout.md` §2/§5 (the 22-residual `idx`/`vname` breakdown + the α/β
  ordering constraint + the resolve.gg file-zone disjointness this honors).
- CLAUDE.md — Core-#8 reference-grade gate (both fixtures CORRECT, not just non-failing),
  "Don't redesign around compiler gaps" (`idx` BOUND/loops UNROLLED, not allow-listed),
  "Typed metadata, never name-matching" (the const name+value are TYPED AST fields written once at
  the parser; the int-MetaValue env is typed state, not a string-shape heuristic),
  "Re-verify a premise … MEASURE end-to-end" (every number above regenerated this session — the +2 /
  0-regress / bootstrap-green / 1222-72-0 / 1034-matched figures).
```
