# Scout report — Tuple/pattern-destructured SVarDecl DefId slice (MoveInLoop FP)

**Verdict: FP CONFIRMED by measurement. Filed fix is SOUND but requires an
SVarDecl AST-arity change (a `Vector[int]` field), not just "a name_span".
Prototyped end-to-end: FP gone, true positives preserved, 15/15 liveness
fixtures identical, 18/18 driver tests pass, c_emit unchanged (untriggered,
+0 parity). Prototype patch: `/tmp/recover_tupledefid_proto.patch` (257 lines,
8 files).**

## Verified premises (file:line, base = HEAD f42eea96)

| # | Premise | Evidence | Status |
|---|---------|----------|--------|
| 1 | Parser tuple SVarDecl sets `name_span=-1` | `self_host_typechecker/parser.gg:3589,3591-3603` (LPAREN tuple) + `:3609-3611` (bare-tuple `auto a,b=`) both force `name_span=-1`; only the single whole-binding gets `name_tok.lex_start` (:3607) | ✅ |
| 2 | Tuple is munged into ONE string name `"(a, b)"`, ONE SVarDecl (no per-element node) | `parser.gg` tuple branch builds `name="(" + ... + ")"`; `ast.gg:150 SVarDecl(SpannedType,String,SpannedExpr,int,int,int)` — 6 fields, no pattern/vector | ✅ |
| 3 | resolve splits the string + defines each element but records NO resolution_map entry for tuple elements | `resolve.gg:449-464` (resolve_stmt) + `:925-937` (resolve_stmt_expr): the `name.starts_with("(")` branch loops `scopes.define(part,...)` but only records `resolution_map[name_span]` (which is −1 for tuples, gated out) | ✅ |
| 4 | liveness SVarDecl reads ONE `name_span` → seeds ONE loop-local; tuple → `decl_def=-1` → NO seed | `typecheck.gg:1395-1410` (base): `if name_span>=0 and resolution_map.contains(name_span)` then seed; tuple's −1 skips it | ✅ |
| 5 | Base self-host FP-rejects; production accepts | MEASURED (below) | ✅ |
| 6 | Symlink topology | `self_host_typechecker/{ast,parser,resolve,typecheck,meta,format}.gg` = REAL canonical; `self_host_check` + `self_host_lowerer` SYMLINK all six (+ own real `loader.gg`, lowerer + `lower*.gg`); `self_host_parser`/`self_host_resolver` = INDEPENDENT copies | ✅ |
| 7 | parser/resolver copies are ALREADY divergent | `self_host_parser/parser.gg:3156` + `self_host_resolver/parser.gg:3139` still `SVarDecl(...,-1)` unconditionally — never got 567f053e's single-binding name_span change (separate compilation units, don't run liveness) | ✅ |
| 8 | Under-arity pattern match leniency is LIVE | `case V(a,b)` on a 3-field variant compiles+runs (measured); same family as DONE.md 2026-07-14 / TODO ~250 | ✅ |
| 9 | UNTRIGGERED by any fixture/self-host source | Only 8 fixtures use tuple destructure; only `bare_tuples.gg`+`test_tuples.gg` combine loop+tuple, and NEITHER has a tuple-SVarDecl move-in-loop (top-level destructures + read-only Copy-int for-patterns). c_emit byte-identical base vs patched | ✅ |

## Measured before/after (self_host_lowerer driver, `<fixture> lib --lir-c`)

| Probe | Shape | Production gg | BASE self-host | PATCHED self-host |
|-------|-------|---------------|----------------|-------------------|
| `repro_loop` | `for…: auto (a,b)=gp(); sink(!a); sink(!b)` | accept | **REJECT (E_MoveInLoop ×2: a,b)** | **accept** ✅ |
| `p_wild` | `for…: auto (a,_)=gp(); sink(!a)` | accept | REJECT (FP) | accept ✅ |
| `p_bare` | `for…: auto a,b=gp(); sink(!a);sink(!b)` | accept | REJECT (FP) | accept ✅ |
| `p_ab_distinct` | move a AND b in loop | accept | REJECT (FP) | accept ✅ |
| `probe_noloop` | destructure + move OUTSIDE loop | accept | accept | accept ✅ |
| `probe_truepos` | outer single var moved in loop | REJECT (E_MoveInLoop) | REJECT | **REJECT (only `outer`)** ✅ TP preserved |
| `p_mixed` | tuple-elem move (legal) + outer move (illegal), same loop | REJECT `outer` | — | **REJECT only `outer`** ✅ |
| `p_dm` | tuple elem `a` double-moved (no loop) | REJECT (E_DoubleMove) | — | **REJECT "double move"** ✅ elem DefId keys moved-state |
| `p_forpat` | `for (a,b) in pairs: sink(!a)` | REJECT (E_MoveInLoop) | — | **REJECT** ✅ (sibling path already matches production — for-var is a borrow, correctly non-movable; NOT part of this fix) |

**Regression gates (patched driver):**
- 15/15 existing liveness fixtures behave identically (5 in-tree rejects + 6 accepts; 4 rejects live in spectests).
- `cargo test self_host_driver_*`: **18 passed / 0 failed** (incl. `self_host_driver_accepts_liveness` + `_rejects_liveness`), 152s.
- `c_emit_comparison`: **1248/1359 (91.8%)** — byte-identical to base (fix is monotonic: only removes FP-rejections, never adds rejects, never alters emitted-C *content*; the 1 self-host crash `stress_shared_comprehensive.gg` T_UnwrapNone is pre-existing, unrelated).

## Prototype design (what the patch does)

Adds a 7th field `Vector[int]` to `SVarDecl` = per-element decl spans (EMPTY for
single/select/shared bindings, so the single-binding path stays **byte-identical**
— 567f053e + all liveness fixtures untouched). Parser captures each tuple element's
`ptok.lex_start`; resolve records `resolution_map[elem_spans[i]] = def_i` (aligned
1:1 with the split parts, skipping `_`); liveness SVarDecl loops `elem_spans`, reads
each DefId, and seeds it loop-local + reinits it — mirroring the single-binding path.
This completes DEEPDEF "DefId identity for all binding shapes" (layering rules 2/3/4).

Files touched (8): `ast.gg` (arity), `parser.gg` (capture + 4 ctor sites),
`resolve.gg` (2 tuple branches), `typecheck.gg` (liveness seed), `meta.gg` (3
reconstructs), `lower.gg`/`lower_generics.gg` (reconstructs), `lower_types.gg`
(synthetic ctor). The prototype leans on the under-arity leniency for the ~31
remaining pure-match wildcard sites (they compile unchanged).

## Corrections to the TODO entry

1. **It is NOT "just emit per-element name_spans" — it needs an AST arity change.**
   The tuple is a munged *string* `"(a, b)"` in a 6-field SVarDecl; there is no
   per-element node to hang a scalar `name_span` on, and one `int` can't key N
   bindings. The fix adds a `Vector[int]` field (a sidecar map would violate the
   owner's retire-sidecars directive; name-mangling spans into the string would
   violate layering rule 2). This is inherent, not avoidable.
2. **"pattern SVarDecl" is a slight misnomer** — SVarDecl carries a string name,
   not a `Pattern`; the elements are re-derived by `name.split(", ")`.
3. **Blast is contained to the typechecker-canonical + check-loader + lowerer
   files** (NOT the parser/resolver independent copies — they're separate units,
   don't run liveness, and are already divergent). ~10 construction sites (MUST)
   + ~31 pure-match sites (SHOULD — see below).
4. **Genuinely +0 parity** — untriggered confirmed by corpus grep + identical
   c_emit. Value is completing the DEEPDEF invariant, per owner reference-grade /
   leak-freedom-even-at-+0 philosophy.

## Recommended executor plan

1. `ast.gg`: `SVarDecl(..., Vector[int])` (canonical only; check/lowerer follow the symlink).
2. `parser.gg`: capture `elem_spans` in the LPAREN-tuple + bare-tuple branches; pass
   `Vector[int]()` at the select-recv, shared, and closure-param-desugar ctor sites.
3. `resolve.gg`: both tuple branches — index-loop `parts`, `resolution_map.put(elem_spans[i], pdef)`
   for non-`_` parts (keep the `.unwrap_or(-1)` idiom — do NOT use `is Some` on the
   mutating `scopes.define` return; that's the live codegen bug 567f053e worked around).
4. `typecheck.gg` SVarDecl liveness: `for es in elem_spans:` → read DefId → `live_reinit` + seed loop-local.
5. Reconstruct sites (`meta.gg` ×3, `lower.gg`, `lower_generics.gg`): match+pass the 7th field (preserves spans through substitution). `lower_types.gg`: `Vector[int]()`.
6. **CLEAN (do not depend on the under-arity leniency bug):** update all ~31
   remaining `case SVarDecl(...)` wildcard sites in the symlink-family units to add
   the 7th field (`_`). Trivial/mechanical. If skipped, the fix silently depends on
   a filed leniency bug and breaks when that's enforced.
7. Land WITH a regression fixture: `tests/fixtures/liveness/tuple_loop_local_move_accept.gg`
   (= `repro_loop`) wired into `self_host_driver_accepts_liveness`; optionally a
   `p_mixed` reject twin into `self_host_driver_rejects_liveness`.

**Gates:** `cargo build` + `cargo test --lib`; `self_host_driver_accepts_liveness`
+ `_rejects_liveness`; `c_emit_comparison` (expect unchanged 1248); **PARENT:**
`self_host_bootstrap_fixed_point` + full integration (both backends).

## New pre-existing issues found en route (report, not fix)
- **None new.** The under-arity pattern-match leniency (premise 8) is already filed
  (DONE.md 2026-07-14 + TODO ~250); I confirmed it's still LIVE and that any
  arity-change landing interacts with it. The `resolve.gg` tuple `def_id`
  overwrite-in-loop is harmless (its only consumer is the `sk>0` shared-var gate,
  never a tuple).
