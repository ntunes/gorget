# Scout — ill-typed Case B (reject undefined identifiers): the reference-grade increment plan

**Status:** READ-ONLY scout, fresh measurement (2026-06-22, worktree off `gorget-1` tip `7da95d0e`).
Supersedes the dated figures in `ill-typed-undefined-name-brief.md` (which cited 238/233 from
`a3b28b2c`/`a60bfa12`). All numbers below were **regenerated this session** by building a SCOUT-instrumented
self-host and running it over the full 1290-fixture corpus + cross-checking each hit against Rust `gg check`.
The prototype edits were reverted; the tree is clean.

The deliverable is grounded in `docs/devbook/07-name-resolution.md` (the INTENDED resolution semantics —
"binds value/type names to DefIds and **reports undefined names**", the `is_builtin`/`__return__`/
`is_known_variant_name` exclusion triple at the Identifier-miss site, the non-generic-variant `alloc_def`
rule, the f-string/meta suppression rule) and the Rust reference `src/semantic/resolve.rs:1478-1512`.

---

## 0. How this was measured (so the executor can re-run it)

The defect lives in the **build path** = `tests/fixtures/self_host_lowerer/driver.gg` (the real
`gg-selfhost` with `build`/`check`/`run` subcommands + the import-MANGLING `load_imports` loader). The
`check_comparison` harness uses a DIFFERENT driver (`self_host_check/driver.gg`) whose `load_all`
loader MERGES imported items into scope under their real bare names (`self_host_check/loader.gg:490`,
`entry.items.push(imp_item)`) — so its spurious set is far smaller. **Measure on the lowerer driver; it
is where the defect is and where the fix must hold.**

Prototype recipe (reproduce):
1. Replace `resolve.gg:597-598` `elif not is_builtin(name): pass` with
   `elif not is_builtin(name): print("SCOUT_UNDEF " + name, file=stderr)`.
   (`resolve.gg` is symlinked into `self_host_lowerer/` + `self_host_check/`; the 4th copy is
   `self_host_resolver/resolve.gg:656`.)
2. To classify imports, also dump `call_redirects.keys()` to stderr (`SCOUT_REDIRECT <key>`) right
   before `resolve_module(...)` in `self_host_lowerer/driver.gg`'s check arm (line ~764).
3. `gg build self_host_lowerer/driver.gg -o /tmp/bin`; for every `tests/fixtures/*.gg` run
   `/tmp/bin check <f> --lib-dir=lib`, collect `SCOUT_UNDEF`/`SCOUT_REDIRECT`.
4. Cross-check each hit fixture with `./target/debug/gg check <f> </dev/null` (Rust oracle).
   **`</dev/null` is load-bearing** — without it `gg check` blocks on stdin and the scan stalls (a
   1-hour false-stall earlier this session).

---

## 1. THE FRESH MEASURED SPURIOUS SET (lowerer / build path)

Corpus: **1290** fixtures.

| | fixtures | meaning |
|---|---|---|
| naive fix fires (≥1 undefined-name) | **234** | every fixture that emits a diagnostic |
| → Rust REJECTS as undefined-name | **1** | `undefined_name_error.gg` — the diagnostic SHOULD fire (a WIN) |
| → Rust ALSO errors (negative test, diff message) | **3** | `import_collides_with_user_def.gg`, `user_def_collides_with_import.gg`, `variable_no_initializer_errors.gg` — directionally-OK but error TEXT differs |
| → **Rust accepts CLEANLY (TRUE SPURIOUS)** | **230** | the actual regression set |

So the naive fix is a **230-fixture corpus regression** (plus 3 borderline negatives, plus 1 correct
new rejection). Matches the brief's ~233 order of magnitude; the precise truly-spurious count is **230**.

### Per-(fixture,name) hit classification (the 233 Rust-accept-ish fixtures, 7062 total hits)

| class | (fixture,name) hits | distinct names | how the fix must handle it |
|---|---|---|---|
| **(b) import-mangled — in `call_redirects`** | 6216 | most | thread the import allow-set into the resolver |
| **NOT in `call_redirects`** | 846 | 125 | further split below |

The 125 NOT-in-redirect distinct names split:

| sub-class | distinct | representative names | root |
|---|---|---|---|
| (b) synthetic `__` intrinsic | 13 | `__return__`, `__metaop__{add,sub,mul,div,eq,ne,lt,le,gt,ge}`, `__dict_iter_*`, `__set_*`, `__bytes_*`, `__dt_decompose` | intrinsic carve-out |
| (b) bare enum-variant / type ctor | 66 | `Str Bool Int Float Arr Null Obj StrCol IntCol Red Blue Err Warn Info Debug DateTime Empty Star Repeat …` | variant allow-set |
| (a) imported extern free-fns NOT in `call_redirects` | 14 | `_gorget_sqlite_*` (12, `lib/xtd/sqlite.gg`), `_llabs_borrowed`, `_test_borrowed_empty` | loader / parser gaps |
| (a) math intrinsics (bodyless fwd-decl) | 9 | `abs sqrt floor ceil sin cos tan acos min` (`lib/std/math.gg:19-37`) | intrinsic builtins missing |
| (a) bytes externs (extern=symbol, non-String ret) | 9 | `bytes_concat bytes_slice bytes_from_hex bytes_from_str bytes_read_u16_be bytes_read_u32_be bytes_write_u16_be bytes_write_u32_be random_bytes` (`lib/std/bytes.gg:35-36 …`) | loader gap (see §2.A.3) |
| (a) aliased imports | 2 | `mcos msin` (`from std.math import cos as mcos, sin as msin`, `import_alias.gg`) | alias-not-registered |
| (b)/builtin `field_value`/`field_set` call-builtins | 2 | `field_value field_set` (`field_access.gg`) | port Rust's ECall special-case |
| (a) builtin gap | 1 | `panic` (`panic_builtin.gg`) | add to `is_builtin` |
| (c) meta-introduced bindings | ~6 | `BONUS MSG Map vname fname ftype idx` | NOT allow-listed — fix the meta gap |
| (d) negative-test locals (Rust ALSO rejects) | ~5 | `f i s n flag` (`variable_no_initializer_errors.gg`) | rejection is CORRECT, message differs |

---

## 2. THE ORDERED REFERENCE-GRADE INCREMENT PLAN

The "0 spurious" gate is UNSATISFIABLE with B1+B2 alone (review `a60bfa12` was right). The track is
**multi-increment**: fix the real (a)-class registration gaps FIRST so the legitimate names become
DEFINED and the diagnostic can't false-positive on them; then add the (b) allow-set; then the (c) meta
gap; then flip the `pass`.

### Increment 1 — (a)-class resolver/loader registration fixes (real parity bugs; fix BEFORE the diagnostic)

These are Rust-VALID programs the self-host fails to register. Each is its own small fix. Fixing them
also REMOVES them from the spurious set (the name becomes defined).

**A.1 — `borrowed` qualifier in extern blocks (parser).** ROOT-CAUSED this session.
`parse_extern_block` (`self_host_lowerer/parser.gg:3926`) skips the inner `extern` keyword (`:3965-3966`)
and then `blocking`/`noreturn`/`async` (`:3970-3975`) before `parse_function_def`, but does NOT skip
`borrowed`. `extern borrowed int _llabs_borrowed(int x) = "llabs"` therefore mis-parses and drops the fn.
`borrowed` is a bare IDENT in the self-host lexer (not a keyword), like `blocking`/`noreturn`. **Fix:** add a
`borrowed`-ident skip alongside the `blocking`/`noreturn` skips at `:3970-3973` (and the inline-extern arm
at `:3933-3934` if it can carry `borrowed`). Verified: removing `borrowed` from the fixture → resolves
clean. Closes `extern_borrowed.gg` + `borrowed_extern_string.gg` (both reproduce on BOTH drivers, so it's
a true parser gap, not import-mangling).

**A.2 — `panic` missing from `is_builtin` (resolver).** `is_builtin` (`resolve.gg:57-98`) lists
`print/format/len/type` + the numeric/string type ctors but NOT `panic`. Rust's `is_builtin`
(`resolve.rs:2108`) includes it. **Fix:** add `if name == "panic": return true`. Closes `panic_builtin.gg`.
(Also audit Rust's `is_builtin` list for other gaps — reflection `field_value`/`field_set` are handled in
A.5, but check `assert`, `embed_file`, etc. against `resolve.rs:2108`.)

**A.3 — imported extern/intrinsic free-fns the lowerer loader DROPS.** This is the subtle one. The
lowerer `load_imports` (`self_host_lowerer/loader.gg:573-1128`) only registers a bare name into
`call_redirects` when the imported fn `has_real_body` (`:775-780`) or is an equip-method extern stub
(`:894-912`). A free-function extern stub (`extern Vector[uint8] bytes_concat(...) = "gorget_bytes_concat"`,
`lib/std/bytes.gg:35`) takes the `else` branch (`:784-826`) which pushes an empty-body copy under the
source name **ONLY if the return type is `String`** (`:813-826`). Non-String externs (`bytes_concat` →
`Vector[uint8]`, the 12 `_gorget_sqlite_*` → mixed, the 9 math fns `abs`/`sqrt`/… which are bodyless
intrinsic fwd-decls in `lib/std/math.gg` with NO `= "symbol"` at all) are **silently dropped — they land in
NEITHER `call_redirects` NOR scope NOR `imported_fns` under the bare name.** Rust resolves them because it
merges the extern decl into scope under its real name.
**This is why B2 cannot key the allow-set on `call_redirects` keys alone** — it would miss `bytes_concat`,
the sqlite externs, and the math intrinsics (verified: `bytes_concat` never appears as a `SCOUT_REDIRECT`).
**Fix (the clean one):** have `load_imports` record EVERY imported top-level fn/extern BARE name (real-bodied,
extern-stub, AND bodyless-intrinsic fwd-decl) into a dedicated `Dict[String,bool] imported_bare_names`
(or reuse `imported_fns` but keyed by BARE name), regardless of return type. That set is the B2 carrier
AND fixes the latent "dropped non-String extern" registration hole. Closes the 14 extern + 9 math + 9 bytes
distinct-name hits across `sqlite_basic.gg`, the `http*`/`httpserver*` family, `dataframe_*`, `closure_float_ret.gg`, etc.
NOTE: this overlaps with B2 — do A.3 and B2 together (A.3 builds the carrier B2 reads).

**A.4 — aliased imports `from X import Y as Z` (parser/loader).** `mcos`/`msin` from
`from std.math import cos as mcos, sin as msin` (`import_alias.gg`) are unregistered. The self-host
`ImportStmt` (`ast.gg:239-241`) models only `module_path` + `names: Vector[String]` — **no `as`-alias
field.** Confirm whether the parser stores the alias (`msin`) or the original (`sin`) in `names`, then
whether `load_imports`/`collect_import` (`resolve.gg:263-282`) registers it. Rust handles this via
`rebind_alias` + an AST rewrite (`resolve.rs:204-213`, devbook §"Aliased imports"). **Fix:** model the alias
(extend `ImportStmt` with an alias, OR register the alias name pointing at the real symbol). Closes `import_alias.gg`.

**A.5 — `field_value` / `field_set` reflection call-builtins (resolver).** Rust special-cases these in the
`Expr::Call` arm (`resolve.rs:1545-1561`): resolve only the object arg, skip the field-name arg (a
meta-loop var / string literal). The self-host ECall arm (`resolve.gg:607-610`) has NO such special-case
(grep confirms zero hits for `field_value`/`field_set`/`make_variant` in `resolve.gg`). **Fix:** port the
3-callee special-case (`field_value`/`field_set`/`make_variant`) into the self-host `ECall` arm. Closes
`field_access.gg`. (This is class-(b)-shaped — a known-callee carve-out — but lives at the ECall site, not
the Identifier site, so it must land before the diagnostic.)

### Increment 2 — the (b) allow-set + the diagnostic push

After Increment 1, the only legitimate-but-unresolvable names left are: **bare enum variants**, the
**`__`-intrinsic set**, and the **imported bare names** (the A.3 carrier). Build the guard, THEN flip `pass`.

**B1 — known-variant + intrinsic exclusions (mirror Rust's triple).**
- **Variant names:** the self-host has NO `is_known_variant_name`. Non-generic variants are allocated via
  `alloc_def` (`scope.gg:209-213`) which — unlike `define` (`scope.gg:160-177`) — does **NOT** push to
  `name_index`, so a `name_index` scan MISSES `Red`/`Str`/`IntCol`. Two options (pick one):
  (i) **extend `alloc_def` to name-index variants** like Rust does, then add an `is_known_variant_name`
  that scans the index for a `DkVariant`; or (ii) add a query that scans `definitions` for a `DkVariant`
  with the given name. (i) is the reference-grade match (Rust's `is_known_variant_name` consults
  `name_index`, devbook §"name index"). Covers the 66 Capitalized variant/type-ctor names.
- **`__return__` + `__`-intrinsics:** add `name == "__return__"` (Rust `resolve.rs:1499`) plus the
  enumerated `__`-synthetic set (`__metaop__*`, `__dict_iter_*`, `__set_*`, `__bytes_*`, `__dt_decompose`).
  Per CLAUDE.md "no name matching", prefer a typed/centralized predicate; a `__`-prefix carve-out for the
  KNOWN compiler-synthetic names is acceptable IF centralized + commented (these are compiler-internal, not
  user identifiers). Covers the 13 `__` names.

**B2 — thread the imported-bare-name set into the resolver.** `resolve_module(module, &scopes, &types)`
(`resolve.gg:1020`) does not receive the import set. The carrier should be the **A.3
`imported_bare_names`** set (NOT `call_redirects.keys()` — that misses the dropped non-String externs).
Hang it on `ResolveContext` (which already carries `diagnostics` and is threaded to `resolve_expr` —
`resolve.gg:34-53`, `577`) or pass it as a param. The three lowerer-driver `resolve_module` call sites
(`self_host_lowerer/driver.gg:386,643,764`) pass it; the `self_host_typechecker/driver.gg:81` and
`self_host_check/driver.gg:73` sites (which run WITHOUT `load_imports`) pass an empty set (default).
**R3 fold:** confirm `type_comparison`/`resolver_comparison`/`check_comparison` stdout stays byte-stable
after the signature change (these drain DIAG to stderr; stdout is type-only — `resolve.gg:39-44`).

**Diagnostic push site:** `resolve.gg:597-598`, replacing `pass` with
`ctx.diagnostics.push(Diagnostic.error(expr.span, DkUndefinedName(), "undefined name \`" + name + "\`"))`.
`DkUndefinedName` already EXISTS (`diagnostic.gg:43`, rendered at `:99`); add it to the
`from diagnostic import …` line (`resolve.gg:18` currently imports only `Diagnostic`). The push pattern is
identical to `typecheck.gg:1206` etc. No state snapshot needed (a `Vector.push`, no TypeTable mutation).
**Fix all 4 copies:** the symlinked `resolve.gg` covers `self_host_{typechecker,lowerer,check}`; the
non-symlinked `self_host_resolver/resolve.gg:656` needs the same edit.

### Increment 3 — (c) meta-introduced bindings (do NOT allow-list — fix the gap)

`BONUS` (`meta int BONUS = 100`, `meta_basic.gg:11`), `MSG` (`meta String MSG`, `meta_builtins.gg`),
`Map` (`meta type Map = Dict if ORDERED else HashMap`, `meta_conditional_types.gg:2`), and the meta-loop
vars `vname`/`fname`/`ftype`/`idx` (`meta for fname, ftype in fields(T):`, `meta_fields.gg:10`,
`meta_enum_ordinal.gg`, `meta_variant_payloads.gg`, the `dataframe_*` derive family).

**Root cause:** resolution runs after `expand_meta_types`/`expand_meta_for_match` but the meta-FOR
construct is lossy in the self-host AST: `SMetaFor(body)` (`ast.gg`, handled at `resolve.gg:927-930`)
stores ONLY the body — it DISCARDS the loop-variable names and the iterable. So `resolve_block_expr(body)`
walks `print(f"{fname}:{ftype}")` with `fname`/`ftype` UNBOUND. Likewise meta-introduced `meta int BONUS`
inside a `meta`/`meta if` block isn't registered, so a later `print(BONUS)` misses. Rust handles meta by
skipping the meta condition/range/scrutinee and resolving the body, with meta-loop vars materializing at
mono (devbook §"Meta statements", `resolve.rs:1253-1308`).

**This is the (c) entanglement: allow-listing `vname`/`BONUS`/etc. would MASK the meta gap (forbidden by
"Don't redesign around compiler gaps").** The reference-grade fix is to make meta-for BIND its loop
variables during resolution (requires the `SMetaFor` AST node to carry the loop-var names — an AST change)
and meta-blocks to register their meta-introduced decls. Until that lands, **the undefined-name diagnostic
MUST NOT fire on meta-introduced names.** Two acceptable interim shapes (executor + owner pick):
- (i) Suppress diagnostics inside meta-for/meta-block bodies (mirror Rust's f-string sink suppression at
  `EStringLiteral`, `resolve.rs:1471-1475`) — a SUPPRESSION, not an allow-list, and it's reference-grade
  (Rust suppresses the same class). This is the cleaner stopgap and does NOT bury the gap (the gap is
  "meta vars aren't bound"; suppression just doesn't ERROR on them, same as Rust).
- (ii) Land the real meta-for-binding fix first (its own increment).

`meta_basic` compiles correctly today (`gg run` → `1024/512/1.0/true/70`); the naive fix would REGRESS it.
`meta_fields`/`meta_variant_payloads`/`meta_enum_ordinal` reportedly miscompile — verify each with a
self-host-binary-vs-Rust whole-stdout diff before deciding; a miscompiling meta fixture is an ESCALATION
(see §4), not a silent allow-list.

---

## 3. THE SATISFIABLE GATE (replaces the brief's unsatisfiable "0 spurious")

> **The diagnostic fires ONLY on genuinely-undefined names. Every fixture the self-host currently
> compiles CORRECTLY still passes (no new rejection). Every NEW rejection is a genuinely-undefined name
> (Rust also rejects it, or it is a meta/registration gap that has been FIXED, not allow-listed).**

How to MEASURE it (the executor runs this before/after):
1. **Spurious-regression gate (load-bearing).** Build the post-fix `self_host_lowerer` driver. For every
   `tests/fixtures/*.gg`: run `driver check <f> --lib-dir=lib` (exit + stderr) AND `gg check <f> </dev/null`
   (Rust oracle). Assert: **no fixture that Rust accepts CLEANLY (`OK: no semantic errors`) is rejected by
   the self-host.** Target count: the truly-spurious set goes **230 → 0**. (Use the §0 recipe minus the
   SCOUT prints; the real diagnostic now drives the exit code.)
2. **New-rejection correctness gate.** For every fixture the self-host NOW rejects, assert Rust ALSO errors
   on it (any nonzero / non-`OK`). Today that set is exactly `{undefined_name_error.gg}` (+ the 3 negatives
   `import_collides_with_user_def`, `user_def_collides_with_import`, `variable_no_initializer_errors`, where
   Rust errors too). The new rejection set must be a SUBSET of "Rust also errors".
3. **Targeted regression net** (model on the `#[ignore]`'d `self_host_check_rejects_illtyped`,
   `integration.rs:16691`): add `tests/fixtures/<undefined>.gg` (or reuse `undefined_name_error.gg`) and an
   integration test `self_host_rejects_undefined_name` that runs the self-host `check` subcommand (DIAG →
   stderr, `exit(1)` on `has_errors` — the `self_host_lowerer/driver.gg:767-769` path) and asserts
   `!status.success()` + stderr contains `undefined name`. VERIFY it FAILS on current code (exit 0, prints
   `ok`) and PASSES after. Do NOT use `check_comparison`'s driver path — it drains DIAG silently and only
   diffs TYPE lines (`integration.rs:14425-14438`), which is exactly why this defect hid.
4. **`self_host_bootstrap_fixed_point` GREEN** (`GG_BUILD_TIMEOUT_SECS=600`): the resolver self-resolves the
   driver's own source — the new diagnostic path must not fire on it and must re-converge.
5. **No comparison regression:** `resolver_comparison`/`type_comparison`/`check_comparison`/
   `lowerer_comparison`/`c_emit_comparison` (structurally neutral for well-typed programs).

---

## 4. ESCALATION LIST (owner call per Core-#8 — do NOT decide these)

Each below is a fixture where the only options are "ship a rejection on an already-broken fixture" vs
"leave the hole." List the tradeoff; do not resolve.

1. **Negative fixtures where Rust errors with a DIFFERENT message** (`variable_no_initializer_errors.gg`,
   `import_collides_with_user_def.gg`, `user_def_collides_with_import.gg`). Rust rejects these (e.g.
   "variable declaration requires an initializer"); the self-host would reject with "undefined name". Both
   REJECT (directionally correct), but the message differs and any integration test pinned to the self-host
   stderr substring must be updated. `variable_no_initializer_errors.gg`'s own header comment already flags
   the "undefined name" message as a logged papercut. **Tradeoff:** accept the divergent-message rejection
   now (correct direction, both reject) vs hold for the better message. Likely ACCEPT, but it's an
   error-text divergence to flag.

2. **Meta fixtures that already MISCOMPILE** (`meta_fields.gg`, `meta_variant_payloads.gg`,
   `meta_enum_ordinal.gg` per the prior review — RE-VERIFY each with a self-host-binary-vs-Rust whole-stdout
   diff this session before acting). If a meta fixture both (a) currently miscompiles AND (b) would be
   FALSE-REJECTED by an undefined-name diagnostic on its meta-loop var, then suppressing the diagnostic
   (Increment 3 option i) leaves the miscompile; rejecting it turns a silent miscompile into a false
   rejection of a Rust-VALID program. **The reference-grade answer is to fix the meta-for-binding gap so
   the name IS defined (Increment 3 option ii) — but that's a larger AST change.** Owner decides: ship the
   suppression stopgap (no regression, gap stays filed) vs block Case B on the meta-for-binding fix.

3. **`mcos`/`msin` aliased imports** if A.4 turns out to need an `ImportStmt` AST change that ripples into
   the parser/loader/lowerer (alias model). If the alias fix is non-trivial, the owner may prefer to file
   it and SUPPRESS (not allow-list) those two names interim. Tradeoff: a 1-fixture hole vs an AST change.

---

## 5. Docs the design rests on

- `docs/devbook/07-name-resolution.md` — §"Expression walk and identifier resolution" (the
  `is_builtin`/`__return__`/`is_known_variant_name` exclusion triple, `resolve.rs:1478-1512`), §"Pass 1 …
  Enum" (non-generic variants `alloc_def`'d into `name_index` but not scope), §"name index", §"Import
  fixups" (aliased-import `rebind_alias`), §"Meta statements … skip conditions/ranges/scrutinees", §"In the
  self-host" (the 4-copy `resolve.gg` layout + `RES`-line correctness).
- `docs/language-design.md` §2.1 — undefined name = hard compile error; no uninitialized-variable form.
- `src/semantic/resolve.rs:1478-1512` (Rust reference at the Identifier-miss site), `:1545-1561`
  (`field_value`/`field_set`/`make_variant` ECall special-case), `:2108` (`is_builtin`).
- CLAUDE.md — "Don't redesign around compiler gaps" (no allow-listing the meta miscompiles), Core-#8 (the
  reference-grade gate: rejection must be CORRECT, not just matching the reference), "No name matching"
  (typed variant/intrinsic predicates over substring tests), "Layering discipline" (fix the import-drop at
  the loader WRITE site, A.3).

---

## 6. One-line bottom line for the orchestrator

The naive fix is a **230-fixture corpus regression**, not shippable. The path is: **Increment 1**
(5 real (a)-class registration fixes: extern-`borrowed` parser skip, `panic` builtin, the
loader-drops-non-String-extern hole = the A.3 import-bare-name carrier, aliased imports,
`field_value`/`field_set` ECall special-case) → **Increment 2** (variant + `__`-intrinsic exclusions +
thread the A.3 import set + push `DkUndefinedName` + fix all 4 `resolve.gg` copies) → **Increment 3**
(meta suppression or the meta-for-binding fix — owner call). Gate on "no Rust-clean-accept fixture is
rejected (230→0)" + "every new rejection is one Rust also rejects" + a DIAG-surfacing
`self_host_rejects_undefined_name` test + `bootstrap_fixed_point` green. The meta miscompiles and the
divergent-message negatives are the §4 escalations.
