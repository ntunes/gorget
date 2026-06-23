# Scout — Case-B (a)-class resolver/loader registration fixes: PROTOTYPE result

**Status:** THROWAWAY PROTOTYPE, fresh measurement on `gorget-1` tip `b88fe0d3`
(2026-06-23). Prototypes A.2 + A.4 + A.5 of `docs/plans/ill-typed-case-b-scout.md`
§2 "Increment 1"; A.3 ASSESSED (not built). Numbers regenerated this session.
DO NOT INTEGRATE — this branch is a measurement vehicle.

Grounded in `docs/devbook/07-name-resolution.md` (§"Expression walk and identifier
resolution" — the `is_builtin`/`__return__`/`is_known_variant_name` exclusion triple;
§"Import fixups" — aliased-import `rebind_alias`; §"In the self-host" — the 4-copy
`resolve.gg` layout) and the Rust references `src/semantic/resolve.rs:2108-2114`
(`is_builtin`), `:1545-1574` (`field_value`/`field_set`/`make_variant` ECall
carve-out), `:224-242` (aliased-import rebind).

---

## 0. Headline

Two axes were measured END-TO-END (compile + cc + run + whole-stdout diff for
AXIS-1; SCOUT-instrumented full-corpus resolve for AXIS-2):

| Fix | AXIS-1 (runtime parity flips) | AXIS-2 (spurious (fixture,name) hits removed) | Cost |
|---|---|---|---|
| **A.2 `panic` builtin** | **+0** (lowerer also needs the `panic`→`gorget_panic` call-lowering port) | `panic_builtin panic` (1 pair, 3 hits) | ~1 line |
| **A.5 reflection carve-out** | **+0** (lowerer also needs the `field_value`→`.field` meta-rewrite port) | `field_access field_value`+`field_set` (2 pairs, 10 hits) **+ 5 bonus `fname` hits** | ~25-line port |
| **A.4 aliased imports** | **+0** (lowerer `map_runtime_name` needs the alias→original rewrite) | `import_alias msin`+`mcos`+`abs` (3 pairs, 3 hits) | AST field + parser + resolver (medium) |
| **A.3 imported non-String externs** | (not built) | dominates the spurious set (`bytes_concat`=965, `bytes_slice`=845 …) | the B2 carrier; pair with Increment 2 |

**The (a)-class registration fixes are +0 on runtime parity — they are Case-B
PREREQS, not standalone parity wins.** Each fixture's actual runtime miscompile
lives in a SEPARATE lowerer/meta-rewrite pass the self-host hasn't ported. This
is exactly the "scout parity estimates MUST be end-to-end-verified" lesson — the
brief's "several are standalone wins" premise did NOT hold for ANY of the three
on the runtime axis. They ARE real Case-B prereqs (they make Rust-valid names
DEFINED so the eventual undefined-name diagnostic can't false-positive), which is
their stated FIRST-ordered-stage purpose.

**No-regression: CLEAN.** `bootstrap_fixed_point` GREEN; `cargo test --lib` 1084/0;
all five `*_comparison` byte-identical to the HEAD baseline (measured both ways
this session).

---

## 1. A.2 — `panic` missing from `is_builtin`

**Re-verified premise.** `is_builtin` (`self_host_typechecker/resolve.gg:57-98`,
symlinked into lowerer/check) lists `print/format/len/type` + numeric/string ctors
but NOT `panic`. Rust `resolve.rs:2109` includes `panic`. Confirmed: baseline
SCOUT fires `SCOUT_UNDEF panic` x3 on `panic_builtin.gg`.

**Audit of the rest of Rust's `is_builtin` list (`resolve.rs:2109-2113`):** the only
self-host gap is `panic`. (Self-host additionally has `char`/`String`, which Rust
lacks, and lacks `byte`, which Rust has — but `byte` never appears as a bare value
identifier so it's immaterial. `assert`/`embed_file` are NOT in Rust's `is_builtin`
— they're statement keywords, not call-builtins — so no port needed.)

**Fix (~1 line):** add `if name == "panic": return true` to `is_builtin`.

**AXIS-1 (runtime): +0.** Both baseline and fixed `panic_builtin.gg` CC-FAIL
identically: the emitted C is `__v6 = panic(__v5)` — `panic` lowered as an ordinary
value-returning call (undefined reference + `conversion to non-scalar type`). Rust
lowers `panic(msg)` via `ir/lowering/exprs/calls.rs:525` → `call_void("gorget_panic",
…)` + unreachable + `noreturn_fns` registration. **The self-host LOWERER has no
`panic` special-case** (grep: zero hits for `"panic"`/`gorget_panic` in
`lower.gg`/`lir_lower.gg`). So A.2 (resolver) alone does NOT flip the fixture — to
reach byte-exact MATCH the self-host needs the lowerer `panic` call-lowering port
too (a separate, larger Increment-1 item).

**AXIS-2 (spurious): removes 1 pair / 3 hits** (`panic_builtin panic`). `panic_builtin`
goes from 3 spurious hits → 0 (the only fully-cleared fixture). Oracle `gg check
panic_builtin.gg` = `OK: no semantic errors` (Rust-clean-accept → was a TRUE
spurious), so the resolver fix is correct.

---

## 2. A.5 — `field_value` / `field_set` / `make_variant` reflection carve-out

**Re-verified premise.** The self-host ECall arm (`resolve.gg:607-610`) had no
carve-out (grep confirmed zero hits for `field_value`/`field_set`/`make_variant` in
`resolve.gg`). Rust special-cases all three in `resolve.rs:1545-1574`: resolve only
the genuine VALUE args, skip the callee + the field-name/type-name arg (a meta-loop
var or string literal). Confirmed: baseline SCOUT fires `field_value` x7, `field_set`
x3 on `field_access.gg`.

**Fix (~25-line port).** Peek `(*callee).expr` for `EIdentifier(cname)` and:
`field_value(obj, fname)` → resolve only arg0; `field_set(obj, fname, value)` →
resolve arg0 + arg2 (skip arg1); `make_variant(T, "V")` → resolve nothing. Falls
through to the normal callee+args walk otherwise. (Note: self-host `ECall` args are
`Vector[SpannedExpr]` directly — no Rust `Arg.value` indirection — so `args.get(0)
.unwrap()` is the object expr.) This is the one name-match the resolver legitimately
needs at the ECall site (centralized known-compiler-callee carve-out, per CLAUDE.md
"No name matching" exception).

**AXIS-1 (runtime): +0.** Both baseline and fixed `field_access.gg` CC-FAIL
identically with `undefined reference to 'field_value'`. The resolver carve-out only
suppresses the spurious diagnostic; the actual rewrite `field_value(val,"x")` →
`val.x` lives in Rust's `semantic/rewrite.rs:810-840` + `meta.rs:2507` — **the
self-host has NO such rewrite pass**, so `field_value` stays a literal call →
undefined reference. A.5 (resolver) is a pure Case-B prereq.

**AXIS-2 (spurious): removes 2 pairs / 10 hits + 5 BONUS hits.** `field_value`(7) +
`field_set`(3) cleared. BONUS: `field_access fname` drops 8→3 hits — because the
meta-loop var `fname` appeared as `field_value(val, fname)`'s arg1, and A.5 now skips
resolving it. The 3 surviving `fname` hits are in OTHER contexts (`f"{fname}={v}"`
f-strings), which are class-(c) meta-gap, left for Increment 3. So A.5 also shrinks
the Increment-3 burden. Oracle `gg check field_access.gg` = clean (warnings only) →
TRUE spurious, fix correct.

---

## 3. A.4 — aliased imports `from X import Y as Z`

**Re-verified premise + root cause.** The self-host `ImportStmt`
(`ast.gg:252-254`) models `module_path` + `names` with **NO alias field**. The
parser's `parse_import_item` (`parser.gg:3974-3990`) pushes each name then loops on
`match_tok(TOK_COMMA)` — it **never consumes `as <alias>`**. On `from std.math
import sin as msin, cos as mcos, abs`, after pushing `sin` the next token is `as`
(not `,`), the loop exits, and `as msin, cos as mcos, abs` is left UNCONSUMED → the
parse corrupts and `msin`/`mcos`/`abs` all go unregistered. Confirmed: baseline
SCOUT fires `msin`, `mcos`, AND `abs` (the bare import after the alias is collateral
damage). Rust handles aliases via `rebind_alias` + a Pass-1.5 AST rewrite
(`resolve.rs:224-242`, `mod.rs:244-246`).

**Fix (medium — AST field + parser + resolver, 3 sites).**
1. `ast.gg`: add `Vector[String] aliases` to `ImportStmt` (parallel to `names`,
   `""` = bare). Ripples to the 5 `ImportStmt(...)` ctor sites — but only the
   typechecker-family parser (1 real file, symlinked ×3) needs it for the lowerer/
   check path; the independent `self_host_parser`/`self_host_resolver` copies were
   left untouched and stay internally consistent.
2. `parser.gg`: new `parse_import_alias()` helper consumes optional `as <ident>`;
   call it after each name. Store original in `names`, alias in `aliases`.
3. `resolve.gg` `collect_import`: register each name under its LOCAL spelling (alias
   when present, else bare) so identifier lookups find `msin`.

**AXIS-1 (runtime): +0.** Both baseline and fixed `import_alias.gg` CC-FAIL
identically with `undefined reference to 'msin'`. The resolver now REGISTERS `msin`
(no spurious diagnostic), but the LOWERER's `map_runtime_name` (`lir_lower.gg:1304`)
maps the call name `sin`→`gorget_sin` / `abs`→`gorget_abs` by literal name —
`msin` maps to itself → undefined reference. To flip the fixture the self-host needs
the **alias→original AST rewrite** (Rust's Pass-1.5 `import_aliases`) so the call
site spells `sin` before `map_runtime_name` runs. (Note `abs` ALSO doesn't run at
baseline because the corrupted parse dropped it; with A.4 the parse is clean so `abs`
→`gorget_abs` would work — but `import_alias.gg` still CC-FAILs on `msin`/`mcos`, so
the whole fixture stays red.) A.4 (registration half) is a pure Case-B prereq; the
runtime flip needs the rewrite pass too.

**True cost of A.4 (separated, per the brief's request).** The registration half
(prereq) is the AST field + parser + `collect_import` change prototyped here =
tractable, bootstrap-safe (proven below). The runtime-parity half needs: (a) carry
`alias→original` through the loader/lowerer, and (b) an AST rewrite (or a
`call_redirects.put(alias, real_symbol)`) so `map_runtime_name` sees the original
name. That second half is the larger piece and is the §4-escalation candidate the
brief flagged (a 1-fixture runtime hole vs an AST-rewrite pass).

**AXIS-2 (spurious): removes 3 pairs / 3 hits** (`msin`, `mcos`, `abs`). NOTE
`import_alias` still has 2 surviving spurious hits — `_math_infinity`/`_math_nan`,
which are A.3-class non-String math externs the loader drops (NOT addressed by A.4).
Oracle `gg check import_alias.gg` = `OK` → TRUE spurious, fix correct.

**Multi-copy discipline note.** `format.gg`'s `format_import` (`format.gg:623-630`)
still renders only `imp.names`, dropping the alias on round-trip. The comparisons
passed byte-identical anyway (the formatter's import-line output isn't a load-bearing
RES/type line in any comparison), but the INTEGRATION version should also render
`as <alias>` for fidelity. Filed as a follow-up below.

---

## 4. A.3 — ASSESSMENT (imported non-String externs/intrinsics dropped by the loader)

**Current shape (re-verified).** The lowerer `load_imports`
(`self_host_lowerer/loader.gg:573-1185`, the real file is
`self_host_typechecker/loader.gg`) registers an imported free-fn extern stub into
scope ONLY when its return type is `String` (the carve-out at `loader.gg:818-828`,
`stub_is_string_ret`). Non-String imported externs — `bytes_concat`→`Vector[uint8]`,
the 12 `_gorget_sqlite_*`, the math fwd-decls `sqrt`/`floor`/`sin`/`cos`/… and
`_math_nan`/`_math_infinity` — take the `else` branch and are **silently dropped**:
they land in NEITHER `call_redirects` NOR scope.

**What landed since the 2026-06-22 plan (re-checked).**
- `851052fa` (Inc-1a / A.1, `borrowed` extern skip) — LANDED.
- `05daf35b` (Inc-1b / A.3, "typed is_extern_stub — emit dropped non-String inline
  externs") — LANDED, but it fixed the **MAIN-MODULE inline extern**
  (`extern int f(...) = "sym"` in the ENTRY file: preserve the `= "sym"` redirect).
  It did NOT touch the IMPORTED-extern String-only carve-out (818-828 unchanged).
- `239083f2` (entry-module extern return types in `fn_sigs`) — LANDED, also
  entry-module-scoped.
- `imported_bare_names` (the B2 carrier) — does **NOT exist anywhere yet** (grep
  confirms 0 hits across the self-host tree).

**So A.3-as-the-B2-carrier is STILL OPEN.** The imported-extern registration hole
remains for non-String returns.

**Magnitude (the load-bearing finding).** The A.3-class names DOMINATE the spurious
set, by far: `bytes_concat`=965 hits, `bytes_slice`=845, `bytes_str_checked`=149,
`__bytes_to_str_raw_io`=149, … `sqrt`=41, `floor`=40, `sin`=11, `cos`=8, the
`_gorget_sqlite_*` family, `_math_nan`/`_math_infinity`. These account for the large
majority of the 22349 total post-fix SCOUT hits.

**Recommendation.** A.3 is NOT needed for the three (a)-class parity-prereq wins
above (A.2/A.4/A.5 stand alone as prereqs), but it IS the carrier B2 must read in
Increment 2 — keying the allow-set on `call_redirects.keys()` alone would miss
`bytes_concat`/sqlite/math entirely. **Build A.3 together with B2** (A.3 builds the
`imported_bare_names` set, B2 threads it into `resolve_module` and reads it at the
identifier-miss site). Do NOT build A.3 standalone now — it has no AXIS-1 yield by
itself and its only consumer is the not-yet-built B2.

---

## 5. No-regression evidence (all on the FINAL fixed code, this session)

- **`self_host_bootstrap_fixed_point` GREEN** (`GG_BUILD_TIMEOUT_SECS=600`,
  release, `--test-threads=1`): `test result: ok. 1 passed`. The bootstrap compiles
  the driver's OWN source — which now contains the new `as`-parsing, the 3-field
  `ImportStmt`, the `field_value` carve-out, and the `panic` builtin — and
  re-converges to a byte-identical fixed point. This is the load-bearing gate
  (resolve.gg/ast.gg/parser.gg are all in the bootstrap's compiled set).
- **`cargo test --lib`**: 1084 passed, 0 failed.
- **Comparison parity — byte-identical to the HEAD `b88fe0d3` baseline** (measured
  both with-fix and at-HEAD this session):
  | test | with-fix | HEAD baseline |
  |---|---|---|
  | resolver_comparison | 1276/1291 matched, 15 mism, 0 crash | (untouched driver `self_host_resolver`) |
  | type_comparison | exact 1182, superset 42, mism 67, crash 0 | exact 1182, superset 42, mism 67, crash 0 |
  | check_comparison | exact 1162, superset 61, mism 68, crash 0 | exact 1162, superset 61, mism 68, crash 0 |
  | lowerer_comparison | matched 1067, mism 114, crash 5 | matched 1067, mism 114, crash 5 |
  | c_emit_comparison | matched 1031, mism 149, self-host crash 0 | matched 1031, mism 149, self-host crash 0 |
  (The 5 lowerer "crashes" are pre-existing negative fixtures — break/continue-
  outside-loop etc. — where the self-host correctly exits nonzero; none of mine.)
- **My 3 target fixtures resolve+check cleanly** with the fixed driver:
  `panic_builtin: ok`, `import_alias: ok`, `field_access: ok`.

**Measurement commands (re-runnable):**
- Build driver: `GG_BUILD_TIMEOUT_SECS=600 gg build
  tests/fixtures/self_host_lowerer/driver.gg -o /tmp/sh_driver`.
- AXIS-1: `/tmp/sh_driver F lib --emit-c --runtime-dir=$PWD/src/backend/c/runtime
  > F.c; cc -O0 -w -o F F.c -lm -lpthread; ./F </dev/null` vs `gg run F`.
- AXIS-2: temporarily replace `resolve.gg`'s identifier-miss `pass` with
  `print("SCOUT_UNDEF " + name, file=stderr)`, build, then for every
  `tests/fixtures/*.gg` run `/tmp/sh_driver check <f> --lib-dir=lib </dev/null`
  and collect `SCOUT_UNDEF` per fixture. Diff baseline-scout vs fixed-scout.

---

## 6. Copies touched

- `tests/fixtures/self_host_typechecker/resolve.gg` (A.2 + A.5 + A.4 register) —
  the REAL file, symlinked into `self_host_lowerer/` + `self_host_check/`.
- `tests/fixtures/self_host_typechecker/parser.gg` (A.4 parse) — symlinked ×3.
- `tests/fixtures/self_host_typechecker/ast.gg` (A.4 `ImportStmt.aliases`) —
  symlinked ×3.
- NOT touched: `self_host_resolver/` (independent 4th copy) and `self_host_parser/`
  (independent ast/parser). They keep the 2-field `ImportStmt` + old parser and stay
  internally consistent; their comparisons (resolver/parser) are unaffected.

For the INTEGRATION version, the same A.2/A.5/A.4 changes must also be applied to
`self_host_resolver/{resolve.gg,parser.gg,ast.gg}` to claim the `resolver_comparison`
parity win on `import_alias` (currently a 54-vs-59-line mismatch there, pre-existing).

---

## 7. Bottom line for the orchestrator

1. **A.2/A.4/A.5 are correct Case-B prereqs, +0 on runtime parity.** Land them in
   Increment 1 to make the Rust-valid names DEFINED (so the future undefined-name
   diagnostic can't false-positive), NOT as standalone parity wins. The brief's
   "several are standalone wins" did not survive end-to-end measurement — each
   fixture's runtime miscompile lives in a separate lowerer/meta-rewrite the
   self-host hasn't ported (`panic`→`gorget_panic` lowering; `field_value`→`.field`
   meta-rewrite; alias→original call-name rewrite). Those three lowerer ports are
   the real standalone-parity follow-ups, each its own item.
2. **A.5 has a bonus:** it also clears 5 class-(c) `fname` meta-var hits (the
   reflection-call arg1), shrinking the Increment-3 surface.
3. **A.4's runtime half (alias→original rewrite) is the bigger piece** — file it
   separately (§4 escalation: 1-fixture runtime hole vs an AST-rewrite pass).
4. **A.3 stays open and DOMINATES the spurious set** (`bytes_concat`=965, …). It is
   the B2 carrier; build it WITH B2 in Increment 2, not standalone (no AXIS-1 yield
   alone; only consumer is the not-yet-built B2). Inc-1b (`05daf35b`) fixed only the
   ENTRY-module inline extern, not the imported-extern hole.
5. **Follow-up for the integration version:** apply the same fixes to the
   `self_host_resolver` copy (resolver_comparison `import_alias` win) and render
   `as <alias>` in `format.gg`'s `format_import` for round-trip fidelity.
