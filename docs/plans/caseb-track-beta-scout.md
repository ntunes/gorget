# Scout — Case-B "Track β": the allow-set + undefined-name machinery (re-measured + prototyped)

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-23, worktree off `gorget-1`
tip `02e35d81`. Re-verifies + re-scopes `docs/plans/ill-typed-case-b-scout.md` (the 2026-06-22 plan,
off `7da95d0e`). **All numbers below were regenerated THIS session** by building a SCOUT-instrumented
self-host lowerer driver, running it over the full **1294-fixture** corpus, cross-checking every hit
against the Rust `gg check` oracle (`</dev/null` load-bearing), THEN prototyping the machinery+flip and
re-measuring the residual. The prototype is committed as a throwaway `PROTOTYPE(scout): … DO NOT
INTEGRATE` commit; it must NOT be integrated.

Grounded in `docs/devbook/07-name-resolution.md` (the intended `is_builtin`/`__return__`/
`is_known_variant_name` exclusion triple, the non-generic-variant `alloc_def` rule, the meta-suppression
rule) and the Rust reference `src/semantic/resolve.rs`.

---

## 0. Headline

| metric | plan (2026-06-22) | THIS scout (2026-06-23) |
|---|---|---|
| corpus | 1290 | **1294** |
| naive-flip fires (≥1 undefined) | 234 | **232** |
| → genuine WIN (Rust rejects undefined) | 1 | **1** (`undefined_name_error.gg`) |
| → Rust-also-errors, divergent message | 3 | **3** (`import_collides_with_user_def`, `user_def_collides_with_import`, `variable_no_initializer_errors`) |
| → **TRUE SPURIOUS (Rust clean-accepts)** | 230 | **228** |
| **spurious AFTER Track β machinery + flip** | — (not measured) | **27 fixtures / 11 names** (measured) |
| **spurious after β + the 3 small new (a)-fixes** | — | **22 fixtures (= pure Track-α/meta residue)** |

**Bottom line:** the Track β machinery (B1 variant-query + B1 intrinsic carve-out + B2 import allow-set)
takes the spurious set from **228 → 27 fixtures**, and is **parity-NEUTRAL** (bootstrap fixed-point green,
`type_comparison`/`resolver_comparison` byte-identical to baseline). The 27 residual fixtures need
either Track α (meta-var binding — 22 of them) or three SMALL new (a)-class fixes (5 of them, see §3).
Track β alone CANNOT reach 0; that was always true and the measurement now pins exactly what's left.

---

## 1. What's LANDED since the 2026-06-22 plan (do NOT re-scope)

Verified against current source:

- **A.2 (`panic` builtin)** — LANDED. `is_builtin` (`self_host_typechecker/resolve.gg:62`) has
  `if name == "panic": return true`. `panic_builtin.gg` no longer fires.
- **A.4 (aliased imports `from X import Y as Z`)** — LANDED. `ImportStmt` now carries
  `aliases: Vector[String]` (`ast.gg:252-261`); `collect_import` registers the alias spelling
  (`resolve.gg:281-293`); the loader/driver rename path is in place. `import_alias.gg` (`mcos`/`msin`)
  no longer fires.
- **A.5 (`field_value`/`field_set`/`make_variant` ECall carve-out)** — PARTIALLY LANDED. The ECall
  special-case IS present (`resolve.gg:631-672` — the prototype shifted these lines; baseline ~620-651):
  it resolves only the genuine value args and skips the field-name/type-name arg. **BUT it does NOT
  fully clear `field_access.gg`:** that fixture ALSO uses the meta-loop var `fname` in an f-string
  (`f"{fname}={v}"`, `field_access.gg:18`), which the resolver walks and cannot bind. So `field_access.gg`
  is now a **Track α (meta-var)** residue, NOT an A.5 gap. (The A.5 ECall carve-out is correct and done.)

Remaining (a)-class gaps from the plan: **A.1 (`borrowed` inline-extern parser skip) is LANDED**
(`parser.gg:4088` skips `borrowed` in the inline arm). **A.3 (loader drops non-String externs) is the
machinery this scout prototypes** (the `imported_bare_names` carrier). The plan's "math intrinsics" and
"bytes externs" are subsumed by the A.3 carrier (verified below).

---

## 2. The FRESH measured spurious set, classified (228 fixtures, 22489 (fixture,name) hits)

Measured on the **build path** = `self_host_lowerer/driver.gg` (the import-MANGLING `load_imports`
loader — where the defect is). Instrument: `resolve.gg` EIdentifier-miss `pass` → `print("SCOUT_UNDEF "
+ name, file=stderr)`. Sweep `driver check <f> --lib-dir=lib` over all 1294 fixtures; cross-check each
firing fixture with `gg check <f> </dev/null`.

Per-(fixture,name) hit class (cross-referenced against the per-fixture `call_redirects` keyset, dumped
via a `SCOUT_REDIRECT` print at the driver check arm):

| class | distinct names | (fixture,name) hits | fixtures | machinery that handles it |
|---|---|---|---|---|
| **B2 import-redirect** (name in that fixture's `call_redirects`) | 311 | **18051** | 168 | B2 import allow-set |
| **B1 bare variant / type-ctor** (`Null`,`IntCol`,`Str`,`FloatCol`,`StrCol`,`Red`,…) | 64 | **2740** | 79 | B1 `is_known_variant_name` |
| **B1 `__`-intrinsic** (`__metaop__*`,`__dict_iter_*`,`__bytes_*`,`__set_*`,`__return__`,`__dt_decompose`) | 24 | **1244** | 192 | B1 intrinsic carve-out |
| **ALPHA meta-var** (`vname`×280,`fname`,`ftype`,`idx`) | 4 | 286 | 21 | **Track α** (NOT β) |
| **A3 math intrinsic** (`sqrt`,`floor`,`sin`,`ceil`,`cos`,`tan`,`acos`,`abs`,`min`) | 9 | 160 | 28 | A.3 carrier (NOT in call_redirects) |
| **A pattern-binding gap** (`s`,`n` in `snag51_closure_block_tail_value.gg`) | 2 | 4 | 1 | NEW (a)-class (closure-block match-arm) |
| **A3 extern non-String** (`_gorget_sqlite_open/step/exec_simple`) | 3 | 4 | 1 | NEW (a)-class — parser `blocking`/`noreturn` inline-extern |
| **TOTAL** | 417 | **22489** | 228 | |

### Key structural finding (sharper than the plan)
The plan framed the 6216 "in call_redirects" hits as the only B2 class. The re-measurement shows
**18051** redirect-covered hits — the dominant class is **transitively-merged LIBRARY bodies calling
bare import names**. `closure_float_ret.gg` imports `xtd.dataframe`, which transitively pulls `std.io`,
`std.bytes`, `xtd.p2p`, etc.; the resolver walks THOSE merged bodies, which call bare `p2p_make_u32` /
`write_all` / `file_open` / `_errno_to_io_error`. The loader registered those under their MANGLED name
(`xtd_p2p__p2p_make_u32`, via `m.items.push(IFunction(renamed))` at `loader.gg:780-788`), so the bare
call-site lookup misses. This is exactly what the B2 allow-set (keyed on bare import names) fixes.

### The A.3 hole is confirmed and precise
Non-String externs (`bytes_concat→Vector[uint8]`, the `blocking` sqlite stubs) and bodyless math
fwd-decls (`sqrt`,`floor`,… in `lib/std/math.gg:19-37`) are NOT in `call_redirects` (verified:
the `A3 math intrinsic` / `A3 extern non-String` rows above are the NOT-in-redirect names). The loader's
String-only carve-out (`loader.gg:818-831`, `stub_is_string_ret`) is why. **The B2 carrier therefore
MUST be `imported_bare_names` (record EVERY imported fn bare name), NOT `call_redirects.keys()`** — the
plan's A.3 conclusion is correct.

---

## 3. The remaining gaps after Track β (the 27-fixture residual, MEASURED)

Built the prototype machinery + flip (FLIP_UNDEF print, so it doesn't change exit codes), re-ran the
sweep over the 228 spurious fixtures:

| residual class | hits | fixtures | owner |
|---|---|---|---|
| **Track α meta-var** (`vname`,`fname`,`ftype`,`idx`,`Map`) | 287 | 22 | **Track α (meta cluster)** |
| **A_typealias_transitive** (`Entity`) | 11 | 3 (`ecs_*`) | NEW small (a)-class |
| **A.1b parser `blocking`/`noreturn` inline-extern** (`_gorget_sqlite_*`) | 4 | 1 (`sqlite_basic`) | NEW small (a)-class |
| **A_closure_pattern_binding** (`s`,`n`) | 4 | 1 (`snag51_…`) | NEW small (a)-class |

**22 of the 27 residual fixtures are PURE Track-α (meta)** — they clear when meta-for binds its loop vars
(or the diagnostic is suppressed inside meta bodies). The other **5** need three small new (a)-class fixes,
none of which is Track β's allow-set and none touches Track β's resolve.gg region:

1. **A.1b — `blocking`/`noreturn` on a top-level INLINE extern (parser).** `extern blocking int
   _gorget_sqlite_open(...) = "sym"` mis-parses: the inline-extern arm (`parser.gg:4085-4123`) skips
   `borrowed` (`:4088`) but NOT `blocking`/`noreturn` — the block arm (`:4139-4148`) and the equip-method
   arm (`:3925-3931`) skip all three; the inline arm was missed. **Fix:** add the `blocking`/`noreturn`
   ident-skips at `:4088` alongside `borrowed`. Sibling of A.1 (CLAUDE.md "fix the class not the
   instance" — this is the un-fixed sibling site). Closes `sqlite_basic.gg`. Parser zone, not resolve.gg.
2. **A_typealias_transitive — `type Entity = SlotKey` (loader/resolver).** A type alias defined in a
   transitively-imported module (`lib/xtd/ecs.gg:13`) used as an identifier in the entry fixture
   (`Entity e1 = pool.create()`, `ecs_advanced.gg:5`) isn't registered when only sibling names
   (`EntityPool`,`SparseSet`) are explicitly imported. Closes `ecs_{advanced,basics,query2}.gg`.
3. **A_closure_pattern_binding — `s`/`n` (resolver).** Match-arm pattern bindings (`case Box.A(s):`)
   used in the arm body f-string, inside a `match`-as-tail-value in a closure block, aren't bound by the
   resolver (`snag51_closure_block_tail_value.gg:85-89`). Rust accepts cleanly. Small resolver scope gap.

These 5 are filed as Increment-1-style (a)-class fixes; they are NOT blockers for the Track β machinery
landing parity-neutral, only for the FINAL flip to reach 0-spurious-on-clean-accepts.

---

## 4. The prototyped machinery (file:line, against current source)

The prototype is the minimal faithful shape for MEASUREMENT. Production should swap the FLIP `print` for
the real `DkUndefinedName` push and replace the variant-query with name-indexed variants (reference-grade
option (i)) if the O(definitions) scan proves hot (it did not regress the bootstrap; it's a per-miss scan
and misses are rare on clean programs).

**A.3 carrier (`loader.gg`):**
- `load_imports` signature gains `Dict[String, bool] &imported_bare_names` (`loader.gg:573`; 3 driver
  call sites updated). Records EVERY imported bare fn name unconditionally at the IFunction arm top
  (`:714`, `imported_bare_names.put(fdef.name.clone(), true)`) and at the IExternBlock arm (`:1106`).
  Superset of `call_redirects` bare keys, plus the dropped math/non-String externs.

**B1 variant query + intrinsic carve-out:**
- `scope.gg` (symlinked across `self_host_{typechecker,lowerer,check}`): `def_kind_is_variant`
  (`:101`) + `is_known_variant_name(ScopeTable, String)` (`:114`) — scans `definitions` for a
  `DkVariant` (alloc_def'd variants have no name-table entry, so a plain lookup misses them).
  *Reference-grade alt:* name-index variants in `alloc_def` (`scope.gg:231`) + scan `name_index`, mirroring
  Rust's `is_known_variant_name`.
- `resolve.gg`: `is_compiler_intrinsic_name(String)` (`:111`) — centralized `__`-carve-out for the KNOWN
  synthetic set (`__return__`, `__metaop__*`, `__dict_iter_*`, `__set_*`, `__bytes_*`, `__dt_decompose`).
  CLAUDE.md "no name matching" is satisfied: these are compiler-internal, never user identifiers, and the
  predicate is centralized + commented.

**B2 threading:**
- `resolve.gg`: `ResolveContext` gains `Dict[String, bool] import_allow` (`:54`); ctor updated (`:107`).
  `resolve_module` (`:1094`) delegates to a new `resolve_module_with_imports(…, import_allow)` (`:1098`)
  which assigns `ctx.import_allow` between `collect_top_level` and `resolve_bodies`. The 3 comparison
  drivers (typechecker/check/resolver) keep calling the 3-arg `resolve_module` → empty allow-set →
  parity-neutral.
- `driver.gg`: `from resolve import resolve_module_with_imports` (`:5`); 3 sites build a local
  `Dict[String, bool] imported_bare_names = {}` (`:366,638,772`) and pass it both to `load_imports`
  and to `resolve_module_with_imports` (`:387,650,784`).

**The flip site:** `resolve.gg:631-647` EIdentifier-miss arm. Production: replace the FLIP `print`
(`else: print("FLIP_UNDEF "…)`) with
`ctx.diagnostics.push(Diagnostic.error(expr.span, DkUndefinedName(), "undefined name `" + name + "`"))`.
`DkUndefinedName` exists (`diagnostic.gg:43`, rendered `:99`) — add it to the `from diagnostic import`
line. **The 4th copy** `self_host_resolver/resolve.gg` has TWO miss sites (`:668`, `:878`) and needs the
SAME machinery+push (it is NOT symlinked).

### Parity-neutrality (MEASURED with the prototype)
- `self_host_bootstrap_fixed_point`: **GREEN** (393s, `GG_BUILD_TIMEOUT_SECS=600`).
- `type_comparison`: 1182 exact + 42 superset = 1224, 70 mismatched, **0 crashed** — BYTE-IDENTICAL to
  baseline (stash-compared this session).
- `resolver_comparison`: 1280 matched, 14 mismatched, **0 crashed** — BYTE-IDENTICAL to baseline.
- The genuine WIN still fires (`undefined_name_error.gg` → `FLIP_UNDEF nonexistent`). Two of the three
  negatives (`import_collides_with_user_def`, `user_def_collides_with_import`) NO LONGER appear in the
  undefined-name set after the machinery (the variant/import allow-set suppresses them; Rust catches them
  as "duplicate definition") — so escalation #1 SHRINKS to just `variable_no_initializer_errors.gg`.

---

## 5. File-zone disjointness vs Track α (meta cluster)

**Track β zone (THIS track):**
- `loader.gg` — `load_imports` signature + `imported_bare_names.put` at `:573,714,1106`.
- `scope.gg` (symlinked) — `def_kind_is_variant` / `is_known_variant_name` at `:101,114` (+ optionally
  `alloc_def` `:231` for the reference-grade variant name-index).
- `resolve.gg` (symlinked) — `ResolveContext` (`:34-54`), ctor (`:107`), `is_builtin` (`:62`),
  `is_compiler_intrinsic_name` (`:111`), the **EIdentifier-miss arm** (`:631-647`),
  `resolve_module`/`resolve_module_with_imports` (`:1094-1102`). Plus the 4th copy
  `self_host_resolver/resolve.gg` miss sites `:668,878`.
- `driver.gg` — import line `:5`, 3 call-site pairs `:366/387,638/650,772/784`.
- `parser.gg` — `:4088` (the A.1b sibling fix), if bundled.

**Track α zone (meta cluster — DO NOT TOUCH):**
- `resolve.gg` — the **SMetaFor/SMetaForMatch/SMetaIf/SMeta arms** at `:584-605` (in `resolve_stmt`) and
  a second copy at `:1001-1015`. These are STATEMENT-resolution arms; Track β only touches
  EXPRESSION-resolution (EIdentifier) + the context struct + module entry. **Different functions,
  non-overlapping line ranges — NO collision.** Track α also owns `meta.gg` + `ast.gg`.

The only shared FILE is `resolve.gg`, and the two tracks edit disjoint regions (expr-miss `:631` vs
meta-stmt `:584`/`:1001`). A merge is clean. The diagnostic-PUSH at `:631` and the meta-suppression at
`:584` are the two halves and are >40 lines apart.

---

## 6. Is the machinery one track or splittable?

**Recommend ONE coordinated Track β.** B1+B2 are entangled at a single read site (the EIdentifier-miss
`elif` chain): you cannot flip the `pass` without ALL of {builtin, intrinsic, import-allow, variant}
guards in place, or the flip regresses immediately. The A.3 carrier (loader) and B1 variant-query (scope)
are prerequisites read by that one arm. Landing them piecemeal leaves the flip un-landable and the
machinery un-exercised (the allow-set has no observable effect until the flip). So: **land carrier +
variant-query + intrinsic + threading + flip as ONE parity-affecting change**, gated together.

The 3 small new (a)-class fixes (A.1b parser, type-alias-transitive, closure-pattern-binding) ARE
splittable and independent — each can land separately before the flip (they shrink the spurious set so
the flip's new-rejection gate is cleaner). A.1b in particular is a 2-line parser fix and a free win.

---

## 7. Escalations (owner call per Core-#8 — do NOT decide)

1. **`variable_no_initializer_errors.gg` — divergent-message negative.** Rust rejects with "variable
   declaration requires an initializer"; the self-host flip would reject with "undefined name". Both
   REJECT (directionally correct); the message differs. Any integration test pinned to the stderr
   substring must account for this. (The other two negatives, `*_collides_with_*`, dropped out of the
   undefined-name set entirely once the machinery landed — they're caught by duplicate-definition.)
   **Tradeoff:** accept the divergent-message rejection now vs hold for the better message.

2. **Meta fixtures that may MISCOMPILE** (`meta_fields.gg`, `meta_variant_payloads.gg`,
   `meta_enum_ordinal.gg`). These are in the 22-fixture Track-α residue. If any currently miscompiles
   AND would be false-rejected by the flip on its meta-loop var, suppressing (Track α option i) leaves
   the miscompile and rejecting turns a silent miscompile into a false rejection of a Rust-valid program.
   **The reference-grade answer is the meta-for-binding fix (Track α), not an allow-list.** This is Track
   α's escalation, surfaced here so the orchestrator sequences α before/with the β flip. **Track β must
   NOT flip the `pass` to a hard rejection until Track α has bound (or reference-grade-suppressed) the
   meta names — else the 22 dataframe/meta fixtures regress.** This is the load-bearing inter-track
   ordering constraint.

3. **The 3 small new (a)-class fixes** (A.1b, type-alias-transitive, closure-pattern-binding) are real
   parity bugs the flip would expose. They should land (cheap) rather than be suppressed. Not a hard
   escalation — just the Increment-1 tail. Filing them as TODO items is the orchestrator's call.

---

## 8. Reproduce

```bash
# instrument resolve.gg EIdentifier-miss `pass` -> print("SCOUT_UNDEF "+name, file=stderr)
GG_BUILD_TIMEOUT_SECS=600 ./target/debug/gg build tests/fixtures/self_host_lowerer/driver.gg -o /tmp/bin
for f in tests/fixtures/*.gg; do /tmp/bin check "$f" --lib-dir=lib 2>&1 | grep SCOUT_UNDEF; done   # the 232
for f in <firing>; do ./target/debug/gg check "$f" </dev/null; done                                # the oracle (228 clean / 1 win / 3 neg)
# machinery+flip prototype (this commit): rebuild, re-sweep -> 27 residual fixtures / 11 names
cargo test --test integration type_comparison resolver_comparison -- --nocapture   # parity-neutral
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point   # GREEN
```

## 9. Docs the design rests on
- `docs/devbook/07-name-resolution.md` — the exclusion triple, non-generic-variant `alloc_def`, meta
  suppression, the 4-copy `resolve.gg` layout.
- `docs/language-design.md` §2.1 — undefined name = hard compile error.
- `src/semantic/resolve.rs` — the Identifier-miss reference, the `field_value`/`field_set`/`make_variant`
  ECall carve-out, `is_builtin`, `is_known_variant_name`.
- CLAUDE.md — Core-#8 (reference-grade gate), "fix the class not the instance" (A.1b is the un-fixed
  `borrowed` sibling), "no name matching" (centralized intrinsic predicate), "Don't redesign around
  compiler gaps" (the meta residue is FIXED by Track α, not allow-listed).
