# Scout — self-host meta-for-binding AST fix (Case-B Inc-3 prerequisite)

**Status:** READ-ONLY scout, prototyped + MEASURED + REVERTED (2026-06-22, worktree off `gorget-1`
tip, fast-forwarded clean). All prototype edits reverted; `git status` clean.

**Bottom line, up front:** the brief's premise is **REFUTED by measurement.** Fixing the `SMetaFor`
AST to carry loop-var names + binding them in the resolver:
- does **NOT** flip ANY meta fixture to MATCH (byte-identical output before/after — measured), and
- does **NOT** remove ANY name from the undefined-name leak set (identical leak set with/without binding).

The names that actually miscompile / would-false-reject (`BONUS`, `MSG`, `Map`, `vname`, `idx`) come
from **four entirely different constructs**, none of which is a statement-level `SMetaFor` loop var.
The `SMetaFor`-loop-var binding fixes a non-problem. See §3 for the measured data.

---

## 1. Root cause (file:line) — where loop-var names are dropped, AND the wider class

Grounded in `docs/devbook/07-name-resolution.md` §"Meta statements", `src/semantic/resolve.rs:1398-1404`
(MetaFor), `:1380-1395` (MetaIf), `src/semantic/meta.rs:860-880` (`flatten_meta_ifs`), and the Case-B
scout §Increment-3.

### 1a. The `SMetaFor` loop-var drop is at the PARSER (as the brief guessed) — but it doesn't matter

- AST: `SMetaFor(Vector[Stmt])` — body only (`self_host_typechecker/ast.gg:121`, symlinked into
  `self_host_lowerer`/`self_host_check`; independent copies in `self_host_parser`/`self_host_resolver`).
- Parser: `skip_meta_header()` (`parser.gg:1154-1158`) consumes EVERYTHING between `meta for` and the
  `:` — the loop-var names (`fname, ftype`) AND the iterable (`fields(T)`) — then
  `parser.gg:2891-2895` builds `SMetaFor(body)`. **The names never reach the AST.**
- Resolver: `resolve.gg:927-930` (resolve-bodies) + `:550-553` (collect) push a block scope and walk
  the body with the loop vars UNBOUND.

So yes, the fix would start at the parser (like the A.3 `is_extern_stub` story). **But binding them
changes nothing** because:

### 1b. WHY binding the SMetaFor loop vars is inert — the suppression already covers them

The self-host `EStringLiteral(_, _)` resolve arm is a bare **`pass`** (`resolve.gg:591-592`) — unlike
Rust (`resolve.rs:1460-1476`), the self-host does **not walk f-string interpolation exprs at all.** Every
meta-for loop var in the fixtures is referenced **only inside an f-string** (`print(f"{fname}:{ftype}")`,
`print(f"{vname}={idx}")`) or inside a **skipped meta-if header** (`meta if ftype is numeric:` →
`skip_meta_header` discards the condition). Neither path reaches the `EIdentifier` diagnostic site
(`resolve.gg:596-600`). Measured: `meta_fields` leaks ZERO undefined names with OR without binding.

### 1c. The REAL gaps that cause the meta miscompiles (all independent of SMetaFor binding)

| Name | Construct | Where it's dropped | Rust reference |
|---|---|---|---|
| `BONUS`, `MSG` | top-level `meta if C: meta int BONUS = 100` | **`IMetaIf` is a NULLARY AST variant** (`ast.gg:162`); parser pushes bare `IMetaIf()` (`parser.gg:4203`) then `skip_meta_rest()` (`:4288-4291`) — the whole body is thrown away. No flatten phase exists. | `Item::MetaIf(meta_if)` carries the branch bodies; `flatten_meta_ifs` (`meta.rs:860-880`) splices the winning branch's items (incl. `meta int BONUS`) into the module. |
| `Map` | `meta type Map = Dict if ORDERED else HashMap` used as a **value ctor** `Map[String,int]()` | meta_aliases resolves `Map`→`Dict` for TYPES (`meta.gg:639-650`), but the alias isn't threaded to the **expression/identifier** path nor `lower_index_assign` (`[lower_fail] unsupported base type Map__…`). | type-alias resolution applies to ctor-call exprs too. |
| `vname` | `match s: meta for vname, T in variant_payloads(Shape): case vname(w): return vname(rewrap[T](w))` | `SMetaForMatch` expansion (`meta.gg:762-790` `substitute_arm`) substitutes `vname` into the **pattern head** but NOT into the **return-position constructor call** in the body → `vname(…)` survives as an unknown ident → `OpConstI64(0)` + C type error. | full template substitution covers body uses. |
| `idx`, `vname` | `meta for vname in variant_names(T): meta const idx = enum_ordinal(T, vname)` | `meta const` STATEMENTS inside a meta-for body aren't registered/evaluated; the whole meta-for-over-`variant_names` is a no-op (`SMetaFor` lowers to `pass`, `lower_stmt.gg:794-797`). There is NO `fields()`/`variant_names()` unroll anywhere in the self-host lowerer. | meta-for is unrolled at mono; `meta const` evaluated per-iteration. |

The `lower.gg` EIdentifier-miss emits `OpConstI64(0)` + a `[bug] unknown identifier` C comment — these
are **LOWERING-stage** failures, orthogonal to the resolver undefined-name diagnostic (Inc-2). The
self-host `check` (resolve+typecheck) prints `ok` (exit 0) for ALL six meta fixtures today.

---

## 2. The fix design (the brief's design — and why it's the wrong fix)

The brief's proposed change (for completeness; **do not ship it alone — measured zero yield**):

- **AST:** `SMetaFor(Vector[Stmt])` → `SMetaFor(Vector[String], Vector[Stmt])`. Touches `ast.gg` in
  3 real copies (`self_host_parser`, `self_host_resolver`, `self_host_typechecker`; the latter is
  symlinked into `self_host_lowerer` + `self_host_check`).
- **Ctor/pattern sites (per the symlink map):** 1 AST def + 2 parser ctor sites + 2 resolve patterns
  + 1 typecheck pattern + 1 format pattern + 1 lower_generics pattern + 1 lower_stmt pattern +
  1 lower.gg debug-name pattern + 3 loader patterns = ~13 sites in the typechecker/lowerer path, ×
  the parser+resolver independent copies for their comparison tests. (I prototyped all of them; the
  driver built clean.)
- **Parser:** reuse the existing `parse_meta_for_var_name()` helper (already used by `SMetaForMatch`
  at `parser.gg:3119-3127`) to capture `v1[, v2…]` before `skip_meta_header()` eats the `in range`.
- **Resolver:** `for v in mf_vars: scopes.define(v, DkVariable(), Span(0,0))` in the body scope.

**This is correct AST hygiene but solves nothing measurable.** It is NOT the prerequisite the owner
believes it is.

The fix that WOULD move the meta fixtures (each its own increment, much larger than "bind loop vars"):
1. **`IMetaIf` body + `flatten_meta_ifs`** (parser AST change `IMetaIf` nullary→carries cond+branches,
   + a meta-expansion flatten phase mirroring `meta.rs:860`) → fixes `BONUS`/`MSG`. This is the single
   highest-yield meta gap and is wholly separate from `SMetaFor`.
2. **`meta type` alias on the value/ctor + index-assign path** → fixes `Map`.
3. **`SMetaForMatch` body substitution completeness** (substitute `vname`/`T` into return-position
   ctor calls, not just pattern heads) → fixes `meta_variant_payloads`.
4. **Real `meta for … in fields(T)/variant_names(T)` unroll + `meta const` per-iteration eval** → fixes
   `meta_fields`, `meta_enum_ordinal`. (No unroll exists today; `SMetaFor` lowers to `pass`.)

---

## 3. The MEASURED yield (per-fixture before→after, prototype applied then reverted)

Self-host lowerer driver, `gg run <fixture> --lib-dir=lib`, whole-stdout diff vs `gg run` (Rust oracle).

| Fixture | Rust oracle | Self-host BASELINE | Self-host WITH binding prototype | Δ |
|---|---|---|---|---|
| `meta_basic` | `1024/512/1.0/true/70/99/100` | `…99/0` + `[bug] unknown ident 'BONUS'` | **identical** | **none** |
| `meta_builtins` | `64/true/false/false/feature disabled` | `64/true/false/false/0` + `[bug] 'MSG'` | **identical** | **none** |
| `meta_conditional_types` | `1` | `0` + `[lower_fail] …Map__…` | **identical** | **none** |
| `meta_fields` | `x:float/y:float/name:String/health:int/alive:bool/2/1/done` | `0/0/done` | **identical** | **none** |
| `meta_variant_payloads` | `Circle/Square/Tag/Circle/Square/Tag/done` | C compile error + `[bug] 'vname'` | **identical** | **none** |
| `meta_enum_ordinal` | `North=0…West=3/Red=0…Blue=2/Red/Green/Blue/done` | `done` only | **identical** | **none** |

**Undefined-name leak set (SCOUT print at the `EIdentifier`-miss, simulating the Inc-2 diagnostic),
WITH vs WITHOUT the binding — IDENTICAL:**

```
meta_basic:              BONUS
meta_builtins:           MSG
meta_conditional_types:  Map
meta_fields:             (none)
meta_variant_payloads:   vname (×6)
meta_enum_ordinal:       idx, vname
```

The binding moved nothing. `meta_fields` (the canonical `fields(T)` case the brief named) already
leaks zero names because its loop vars live only in f-strings.

---

## 4. Does this FULLY unblock Inc-2's meta concern? — NO

Inc-2's concern is the undefined-name diagnostic false-rejecting meta names. The `SMetaFor`
loop-var binding does **not** remove a single meta name from the leak set. The 5 leaking names
(`BONUS`/`MSG`/`Map`/`vname`/`idx`) would STILL false-reject after the binding lands. To stop them
false-rejecting you need EITHER:
- **(ii) the real per-construct fixes** in §2 (meta-if flatten, meta-type-on-ctor, SMetaForMatch body
  subst, meta-for unroll + meta-const eval) — large, 4+ increments, each independently the
  reference-grade fix; OR
- **(i) the Rust-style suppression** the Case-B scout already proposed (Inc-3 option i): suppress the
  diagnostic inside meta-for/meta-if/meta-block bodies. This is reference-grade (Rust suppresses the
  same class via the f-string sink) and is the ONLY thing that actually unblocks Inc-2 cheaply.

**Critical correction for the owner:** the choice was framed as "fix the meta-for binding (option ii)
vs suppress (option i)." But **"fix the meta-for binding" is NOT option ii** — it's a third thing that
does neither (no fixture flips, no name un-leaks). The real option ii is the §2 cluster (much bigger
than "bind loop vars"). The genuine decision is: **suppression (option i) now**, vs **block Case B on
the §2 meta-expansion cluster.**

---

## 5. Honest scope/effort + split

- The brief's "meta-for-binding fix" (AST carry loop vars + resolver bind): ~13 edit sites across the
  symlink map + parser/resolver independent copies, ~1 small increment to write — but **yield = 0**.
  Do not ship it as a Case-B prerequisite; it's inert. (If desired purely as AST hygiene / future
  groundwork, it's harmless but must not be sold as unblocking anything.)
- The actual meta miscompiles are **4 independent, larger increments** (§2), each touching the parser
  AST + `meta.gg` expansion + (for the unroll) the lowerer. None is a "bind the names" one-liner.
- For Inc-2 specifically, the **suppression stopgap (Inc-3 option i)** is the right unblock: small,
  reference-grade, does not bury the gap (files the §2 cluster), zero regression. The §2 fixes are
  separate parity work that can land independently of Case B.

**Recommendation:** Do NOT spend the increment on `SMetaFor` loop-var binding. Either take the
suppression stopgap for Case-B Inc-3, or (if the owner wants the meta fixtures to MATCH) open the §2
cluster as its own multi-increment parity track — starting with **`IMetaIf` flatten** (highest yield:
2 fixtures, `BONUS`+`MSG`, and the most architecturally clean — mirrors `meta.rs:860`).

---

## 6. Docs the analysis rests on

- `docs/devbook/07-name-resolution.md` §"Meta statements" (skip conditions/ranges/scrutinees; meta vars
  materialize at mono).
- `src/semantic/resolve.rs:1398-1404` (MetaFor — Rust ALSO doesn't bind loop vars; f-string sink covers
  them), `:1380-1395` (MetaIf), `:1460-1476` (f-string interp sink suppression), `:1478-1512`
  (Identifier-miss exclusion triple).
- `src/semantic/meta.rs:860-880` (`flatten_meta_ifs` — the `IMetaIf` body splice the self-host lacks).
- `docs/plans/ill-typed-case-b-scout.md` §Increment-3 (the construct list + the suppression option i).
- CLAUDE.md "Don't redesign around compiler gaps" (the §2 fixes make names genuinely defined, not
  allow-listed); "Re-verify a premise … MEASURE end-to-end" (this scout's whole point — the premise
  was source-plausible but measured zero).
