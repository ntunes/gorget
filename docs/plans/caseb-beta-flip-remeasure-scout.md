# Scout — Case-B β-flip RE-MEASURE on the current tip + flip-readiness verdict

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-24, worktree off `gorget-1`
tip `85217064` (Track-α COMPLETE; Inc-1c `2fa251ff` landed A.1b + A_typealias; α slices 4a/4b/4c
landed). Re-measures `docs/plans/caseb-track-beta-scout.md` §3 (which was stale: it measured **27
residual** off tip `02e35d81`, BEFORE Track-α and Inc-1c landed). **Every number below was
regenerated THIS session** by re-applying the β machinery on the current tip, instrumenting the
EIdentifier-miss site, sweeping the full **1294**-fixture corpus, and cross-checking every firing
against the Rust `gg check </dev/null` oracle. The prototype is committed as a throwaway
`PROTOTYPE(scout): … DO NOT INTEGRATE` (`45d6cae0`); it must NOT be integrated.

Grounded in `docs/devbook/07-name-resolution.md` (the `Expr::Identifier`-miss exclusion triple —
`is_builtin` / synthetic `__return__` / `is_known_variant_name`; the `name_index` for O(K) variant
lookup; the `field_value`/`field_set`/`make_variant` ECall carve-out), `docs/language-design.md`
(§2.1: undefined name = hard compile error; line 176: no-initializer decl = compile error), and the
Rust reference `src/semantic/resolve.rs:1485-1509` (the Identifier-miss `UndefinedName` site) +
`src/semantic/scope.rs:442` (`is_known_variant_name`).

---

## 0. HEADLINE — the spurious residual collapsed from 27 → **1**

| metric | β scout (stale, tip `02e35d81`) | **THIS scout (tip `85217064`, regenerated 2026-06-24)** |
|---|---|---|
| corpus | 1294 | **1294** |
| fixtures firing FLIP_UNDEF after the machinery | 27 | **3** |
| → genuine WIN (Rust rejects undefined name) | 1 | **1** (`undefined_name_error` → `nonexistent`) |
| → Rust-also-rejects, divergent message | 1 (`variable_no_initializer_errors`) | **1** (`variable_no_initializer_errors`) |
| → **TRUE SPURIOUS (Rust CLEAN-ACCEPTS)** | 25 (22 meta + Entity×3) | **1** (`snag51_closure_block_tail_value` → `s`/`n`) |

**Bottom line:** Track-α (4a/4b/4c) cleared the entire **22-fixture meta-var residual** (`vname`/
`fname`/`ftype`/`idx`), and Inc-1c (`2fa251ff`) cleared the **Entity (ecs×3)** and **sqlite_basic**
classes. **The β scout's §3 stale claim that A.1b/A_typealias still BLOCK is REFUTED — they
DON'T anymore** (re-measured: those fixtures fire ZERO FLIP_UNDEF, §3 below). The ONLY remaining
spurious-on-Rust-clean-accept is **`snag51_closure_block_tail_value`**, blocked by the
**A_closure** `Box`-keyword fossil (`docs/plans/caseb-aclosure-scout.md`).

**Flip-readiness verdict: case (b) — needs A_closure first, then the flip is ready.** The flip
itself is parity-NEUTRAL (proven byte-identical to baseline over the full corpus) and does the
reference-grade-correct thing on the genuine win + the no-init negative. The single blocker is the
known A_closure bug, which is NOT a freebie (it unmasks a latent `Box__T._0`-vs-`void*` lowering bug
that regresses `bootstrap_fixed_point`; the aclosure scout's "land both together" finding stands).

---

## 1. What Inc-1c + Track-α cleared (RE-MEASURED — refutes the β scout's stale §3)

The β scout's §3 listed the 27-residual as: 22 Track-α meta + Entity(ecs×3) + `_gorget_sqlite_*`
(sqlite_basic) + `s`/`n` (snag51). **Re-sweeping the SAME fixtures on the current tip, with the
machinery re-applied** (instrumented driver `/tmp/sh_beta_*`):

```
$ for f in ecs_advanced ecs_basics ecs_query2 sqlite_basic field_access \
           meta_fields meta_variant_payloads meta_enum_ordinal; do
    $SH check tests/fixtures/$f.gg --lib-dir=lib 2>&1 | grep -c '^FLIP_UNDEF '
  done
ecs_advanced: 0    ecs_basics: 0    ecs_query2: 0    sqlite_basic: 0
field_access: 0    meta_fields: 0   meta_variant_payloads: 0   meta_enum_ordinal: 0
```

**All ZERO.** Explicitly confirmed cleared:
- **`Entity` (ecs_advanced / ecs_basics / ecs_query2)** — cleared by **Inc-1c A_typealias**: the
  `ITypeAlias` import-merge arm at `loader.gg:1095-1097` (lowerer copy) now merges
  `type Entity = SlotKey` from `lib/xtd/ecs.gg:13` so `collect_top_level`'s `ITypeAlias` arm
  registers it. (The β scout's §3 row "A_typealias_transitive — Entity — 3 ecs" is now empty.)
- **`_gorget_sqlite_*` (sqlite_basic)** — cleared by **Inc-1c A.1b** (`parser.gg:4131-4145`,
  typechecker copy, symlinked into check+lowerer: the inline-extern arm now skips
  `blocking`/`noreturn`/`borrowed` so `extern blocking int _gorget_sqlite_open(...) = "sym"` parses)
  **+ the B2 import-allow machinery** (the non-`blocking` `_gorget_sqlite_*` + transitive `std.io`
  bare names are in `imported_bare_names`).
- **`vname`/`fname`/`ftype`/`idx` (field_access, meta_*)** — cleared by **Track-α (4a/4b/4c)**: meta
  vars are now substituted into the meta-for / meta-for-match bodies (commits `47ce52aa` /
  `c4b8b680` / `23634aab`), so the resolver no longer walks an unbound meta-loop variable.

This is the "re-verify a premise" lesson in action: the β scout's residual table was a faithful
snapshot of tip `02e35d81`, but 3 rounds landed on top of it. The residual is now 24 fixtures
SMALLER.

---

## 2. The RE-MEASURED spurious set, classified (3 fixtures total)

Instrument: `resolve.gg` EIdentifier-miss `else:` → `print("FLIP_UNDEF " + name, file=stderr)`
(machinery re-applied per §4; driver force-rebuilt `gg build self_host_lowerer/driver.gg`).
Sweep: `$SH check <f> --lib-dir=lib` over all 1294 fixtures; cross-check each firing with
`./target/release/gg check <f> </dev/null` (Rust is the oracle, `</dev/null` load-bearing).

```
$ for f in tests/fixtures/*.gg; do
    $SH check "$f" --lib-dir=lib 2>&1 | grep '^FLIP_UNDEF ' | awk '{print $2}' | sort -u
  done   # → exactly 3 fixtures fire
```

| fixture | undefined names | self-host FLIP hits | **Rust `gg check`** | class |
|---|---|---|---|---|
| `undefined_name_error` | `nonexistent` | 1 | **REJECTS** (`error: undefined name 'nonexistent'`) | **genuine WIN** |
| `variable_no_initializer_errors` | `f`,`flag`,`i`,`s` | 5 | **REJECTS** (`variable declaration requires an initializer`) | Rust-also-rejects, divergent message |
| `snag51_closure_block_tail_value` | `n`,`s` | 4 | **CLEAN-ACCEPT** (`OK: no semantic errors`) | **TRUE SPURIOUS** (A_closure) |

Belt-and-suspenders: the 5 fixtures whose instrumented driver exits non-zero in check mode
(`break_outside_loop_error`, `continue_outside_loop_error`, `main_throws_non_int_error`,
`throw_in_non_throwing_error`, `typecheck_control_flow_diagnostics`) each emit **0** FLIP_UNDEF —
they reject on TYPECHECK control-flow diagnostics (which run AFTER resolve), not on undefined names,
so none masks a spurious. No fixture crashes the driver before reaching resolve.

### 2.1 `undefined_name_error` — the genuine WIN (reference-grade, Core #8 ✓)
```
$ ./target/release/gg check tests/fixtures/undefined_name_error.gg </dev/null
error: undefined name `nonexistent`
```
The flip rejects exactly what Rust rejects, with the same message. The Rust integration test
`undefined_name_error` (`tests/integration.rs:22690`, pins `"undefined name"`) tests RUST and is
unaffected. The flip does the RIGHT thing.

### 2.2 `variable_no_initializer_errors` — Rust-also-rejects, message gap (NOT a clean-accept)
The fixture is a NEGATIVE test (`int i` with no `= …` SHOULD be rejected;
`docs/language-design.md:176`). **Both compilers REJECT** — directionally correct. The divergence
is the MESSAGE: Rust's PARSER rejects the no-`=` shape at the decl site with
`"variable declaration requires an initializer"` (`tests/integration.rs:6325-6335` pins exactly
that), whereas the self-host parser still lets `int i` fall through to expression parsing, so the
flip rejects it as `"undefined name"` (the OLD, misleading message — the fixture's own header
comment, lines 4-7, documents this as a logged papercut). Per Core #8 this is a real but SMALLER
parity gap (the self-host is missing Rust's parser-level no-init rejection); the reference-grade
fix is to PORT Rust's parser no-init diagnostic to the self-host parser. It is NOT a spurious-
on-clean-accept and does NOT regress runtime parity (the fixture is a negative test, never a
self-host MATCH). See §5 escalation #1.

### 2.3 `snag51_closure_block_tail_value` — the sole TRUE SPURIOUS (A_closure)
```
$ ./target/release/gg check tests/fixtures/snag51_closure_block_tail_value.gg </dev/null
OK: no semantic errors                       # Rust CLEAN-ACCEPTS
```
The fixture declares `enum Box: A(String) B(int)` (`:71-73`) and pattern-matches
`case Box.A(s):` / `case Box.B(n):` (`:163-167`). The self-host lexer STILL tokenizes `Box` as the
keyword `KwBox` (`self_host_typechecker/lexer.gg:255-256`, symlinked into lowerer); Rust treats it
as an identifier (`src/lexer/token.rs:317`). So `Box.A` doesn't lex as `TOK_IDENT`, the qualified-
name pattern path (`parse_pattern_atom`, the TOK_IDENT-gated qualified-constructor arm,
`parser.gg:1572-1679`) is gated out, the pattern collapses to `PWildcard`, and the
bindings `s`/`n` are LOST in the PARSER — the resolver then legitimately sees `s`/`n` as undefined.
**This is upstream of the resolver; the flip cannot allow-list around it.** The fix is A_closure
(de-keyword `Box`-family). This is the ONLY fixture the flip would false-reject. snag51 is NOT a
committed runtime snapshot (`tests/fixtures/runtime_snapshots/` has no snag51 entry), so the flip
rejecting it does not regress the lock-in net — but it IS a Rust-clean-accept, so per the
satisfiable gate (§6) the flip must NOT land until A_closure clears it.

---

## 3. Parity-neutrality of the machinery — PROVEN byte-identical (regenerated this session)

The machinery (B1 variant-query + B1 intrinsic carve-out + B2 import-allow) is **provably parity-
neutral** for the comparison drivers, by direct corpus diff (stronger than the diagnostic-always-
pass comparison-count delta):

**Method:** built the BASELINE typechecker driver from tip `85217064`'s `resolve.gg`/`scope.gg`
(`/tmp/sh_typecheck_baseline_*`) and the PROTOTYPE typechecker driver
(`/tmp/sh_typecheck_proto_*`), ran BOTH over all 1294 fixtures with the exact comparison invocation
(`driver <fixture>`), diffed stdout per-fixture:

```
STDOUT-differing fixtures (baseline vs FLIP-print prototype) = 0   (all 1294)
STDOUT-differing fixtures (baseline vs REAL-PUSH flip)       = 0   (all 1294)
```

**Zero** stdout differences in BOTH the FLIP-print and the real-`DkUndefinedName`-push versions.
Why: (a) the comparison drivers call the 3-arg `resolve_module`, which delegates to
`resolve_module_with_imports` with an **EMPTY** `import_allow` — so the new guard arms only fire on
a genuine miss, where the baseline already did `pass`; the new arms don't write `resolution_map`, so
rendered types are unchanged. (b) The real `DkUndefinedName` push lands in `ctx.diagnostics`, which
the comparison driver **drains to stderr** (`resolve.gg:39-43`), keeping stdout byte-stable. The
added diagnostic surfaces only on stderr (`DIAG error undefined-name … undefined name 's'`,
confirmed on the 3 firing fixtures) and in the BUILD/CHECK path (the lowerer driver) — exactly where
the rejection belongs.

Diagnostic comparison counts (regenerated this session; these are diagnostic-always-pass, so the
byte-diff above is the load-bearing proof):
```
$ cargo test --test integration --release type_comparison -- --nocapture --test-threads=1
Fixtures compared: 1294, exact: 1180, superset: 42, total: 1222, mismatched: 72, crashed: 0
$ cargo test --test integration --release resolver_comparison -- --nocapture --test-threads=1
Fixtures compared: 1294, matched: 1275, mismatched: 19, crashed: 0
```
The `type_comparison` 72-mismatched is the CURRENT BASELINE (Track-α shifted it from the β scout's
stale 70 off `02e35d81`); the byte-diff proves the machinery added ZERO of those. `resolver_
comparison` uses the INDEPENDENT `self_host_resolver/resolve.gg` (which has NONE of the machinery),
so it is trivially baseline.

**`self_host_bootstrap_fixed_point`: GREEN** — `test result: ok. 1 passed; 0 failed; finished in
278.63s` (run this session, `GG_BUILD_TIMEOUT_SECS=600`). The machinery's
loader.gg/driver.gg/resolve.gg/scope.gg changes are self-compiled by the driver each stage; the
empty-allow path + the FLIP-print are exercised but change no resolution result, so the bootstrap
re-converged stage-2==stage-3==stage-4 byte-identical.

---

## 4. The re-applied machinery (file:line, against current source `85217064`)

Re-applied the `29838afc` shape MINUS the `parser.gg` A.1b part (which LANDED in Inc-1c
`2fa251ff`). Touches 4 files (resolve.gg + scope.gg symlinked across typechecker/check/lowerer;
loader.gg + driver.gg are lowerer-only):

**A.3 carrier (`self_host_lowerer/loader.gg`):**
- `load_imports` signature gains `Dict[String, bool] &imported_bare_names` (`:573`; 3 driver call
  sites updated). Records EVERY imported bare fn name at the `IFunction` arm (`:714`,
  `imported_bare_names.put(fdef.name.clone(), true)`) and the `IExternBlock` arm (`:1122`).
  Superset of `call_redirects` bare keys + the dropped math/non-String externs.

**B1 variant query + intrinsic carve-out:**
- `scope.gg` (symlinked): `def_kind_is_variant` + `is_known_variant_name(ScopeTable, String)`
  (after `:99`) — linear scan of `definitions` for a `DkVariant` (alloc_def'd variants have no
  name-table entry). **⚠ Production note:** the reference-grade version should name-index variants
  in `alloc_def` and consult `name_index` (O(K)), per `docs/devbook/07-name-resolution.md:84-92`
  + Rust `scope.rs:442` — this linear scan is the measurement-equivalent (ii); the aclosure scout
  measured it as non-regressing because misses are rare on clean programs, but the production land
  should do (i).
- `resolve.gg`: `is_compiler_intrinsic_name(String)` (after `:105`) — centralized `__`-carve-out
  for the KNOWN synthetic set (`__return__`, `__metaop__*`, `__dict_iter_*`, `__set_*`,
  `__bytes_*`, `__dt_decompose`). CLAUDE.md "no name matching" is satisfied: these are compiler-
  internal, never user identifiers; the predicate is centralized + commented.

**B2 threading (`resolve.gg`):**
- `ResolveContext` gains `Dict[String, bool] import_allow` (`:58`); the single constructor call in
  `collect_top_level` updated (`:131`). `resolve_module` (`:1119`) delegates to a new
  `resolve_module_with_imports(…, import_allow)` which assigns `ctx.import_allow`. The 3 comparison
  drivers keep calling 3-arg `resolve_module` → empty allow-set → parity-neutral.
- `driver.gg`: `from resolve import … resolve_module_with_imports` (`:5`); the 3 build/check/main
  sites build a local `Dict[String, bool] imported_bare_names = {}` and pass it to both
  `load_imports` and `resolve_module_with_imports`.

**The flip site:** `resolve.gg:645-658`, the EIdentifier-miss arm. Measurement uses the FLIP-print
(`print("FLIP_UNDEF " + name, file=stderr)`); the PRODUCTION flip replaces it with
`ctx.diagnostics.push(Diagnostic.error(expr.span, DkUndefinedName(), "undefined name `" + name +
"`"))` and adds `DkUndefinedName` to the `from diagnostic import` line (`:18` — `DkUndefinedName`
exists, `diagnostic.gg:43`, rendered `:99`; already imported in `infer.gg:24`). Mirrors Rust
`resolve.rs:1498-1509`. **Both the print AND the real push were built + verified this session**
(real-push: `undefined_name_error` REJECTS with `undefined name 'nonexistent'`; `hello` /
`closure_float_ret` clean-accept; the 3 firing fixtures emit the `DIAG error undefined-name` on
stderr).

**⚠ The 4th copy** `self_host_resolver/resolve.gg` is INDEPENDENT (not symlinked) and was NOT
touched by this scout (it has its own EIdentifier-miss sites). The PRODUCTION land must apply the
SAME machinery+push to it (or accept that `resolver_comparison` does not exercise the flip — which
is fine for parity, but the 4th copy should be kept in sync per CLAUDE.md "fix the class, not the
instance" / the 4-copy resolve.gg layout in `docs/devbook/07`).

---

## 5. Flip-readiness verdict

**Case (b): do A_closure first, then the flip is ready.** The residual is now a SINGLE
spurious-on-Rust-clean-accept (`snag51`), and it is gated entirely by the **A_closure** `Box`-keyword
fossil (`docs/plans/caseb-aclosure-scout.md`). Specifically:

1. The flip is parity-NEUTRAL (§3, byte-identical over the full corpus) and reference-grade-correct
   on the genuine win (`undefined_name_error`) and directionally-correct on the no-init negative
   (`variable_no_initializer_errors`). It is internally READY.
2. The ONLY thing standing between the current state and a satisfiable-gate-clean flip is `snag51`.
   A_closure clears it: de-keywording `Box` makes `Box.A` lex as `TOK_IDENT`, the qualified-pattern
   path fires, and `s`/`n` become genuinely BOUND (the aclosure scout PROVED this end-to-end via the
   `Box`→`Shape` bisection + the prototype emit showing `->A_0`/`->B_0` extraction).
3. **A_closure is NOT a freebie.** Per `docs/plans/caseb-aclosure-scout.md` §4, de-keywording `Box`
   alone REGRESSES `bootstrap_fixed_point` (it unmasks a latent `Box__T._0`-vs-`void*` lowering
   bug at the self-host AST's `Box[SpannedType]`/`Box[SpannedExpr]` payload-construction sites). The
   executor must land **de-keyword + the Box-lowering fix together**. The aclosure scout's "land
   both together" finding STANDS and is the load-bearing prerequisite.

**Sequencing:** A_closure (de-keyword + Box-lowering fix) → then the β flip. They are file-disjoint
(A_closure: `lexer.gg`/`parser.gg` keyword arms; β flip: `resolve.gg` miss-arm + `scope.gg`
variant-query + `loader.gg` carrier + `driver.gg` threading), so the β machinery can even land FIRST
as parity-neutral scaffolding (it has no observable effect until the flip), with the flip's `pass`→
`push` change held until A_closure lands. But the cleanest order is A_closure first (it removes the
last spurious), then the machinery+flip as ONE parity-affecting change.

**Could the flip land WITHOUT A_closure?** Only by leaving `snag51` as a known spurious-reject of a
Rust-valid program — which the satisfiable gate (§6) FORBIDS, and which Core #8 says is not
shippable. There is no allow-list dodge: the binding is lost in the PARSER, upstream of the
resolver, so the resolver's `s`/`n`-undefined is genuine given the broken parse. Do NOT special-case
`s`/`n` or suppress the rejection — fix the parse (A_closure).

---

## 6. The satisfiable gate design

**Test name:** `self_host_rejects_undefined_name` (DIAG-surfacing, build-breaking).

**Shape:** a fixture with a genuinely-undefined name → the self-host resolver MUST REJECT it,
matching Rust. The existing `undefined_name_error.gg` is the ready-made fixture (Rust pins
`"undefined name"` at `tests/integration.rs:22690`). The new test asserts the self-host BUILD/CHECK
path rejects it too (the lowerer/check driver, where the `DkUndefinedName` push surfaces — NOT the
type_comparison driver, which drains to stderr). Mirror the `check_gg_fails` shape against the
self-host driver, pinning the substring `undefined name`.

**Gate conditions (all THREE, regenerated each run):**
1. **0 spurious-on-Rust-clean-accepts** — sweep the corpus through the flipped self-host check
   driver; for every fixture the flip rejects, `gg check <f> </dev/null` (Rust) must ALSO reject.
   Today: `snag51` is the single violator → A_closure is the gate's prerequisite. After A_closure:
   expect 0 violators.
2. **Every new rejection is one Rust also rejects** — the genuine win (`undefined_name_error`,
   Rust rejects) PASSES; `variable_no_initializer_errors` PASSES the direction check (Rust also
   rejects) — the message-substring divergence is logged (escalation #1), not a gate failure, since
   the gate keys on REJECT/ACCEPT direction, not message text.
3. **`self_host_bootstrap_fixed_point` GREEN** — the driver self-compiles its own flipped resolver
   and must still re-converge stage-2==stage-3==stage-4 byte-identical.

**Burn-down framing (CLAUDE.md "executable guard"):** the spurious-count check can ship env-gated
(`GG_BETA_FLIP=1`) as a diagnostic first, burned to 0 (A_closure), then made fatal — converting the
recurring "undefined-name false-reject" class into a ratchet.

---

## 7. Reference-grade gate (Core invariant #8)

- **The genuine win is reference-grade:** the flip rejects `undefined_name_error` exactly as Rust
  does (same message). ✓
- **The no-init negative is directionally reference-grade** (both REJECT;
  `docs/language-design.md:176` mandates it) but the self-host MESSAGE is worse than Rust's. This is
  a ≥1-bug finding (the self-host parser lacks Rust's no-init decl-site rejection), filed as
  escalation #1 — NOT a "benign both-reject" pass. The reference-grade resolution is to port Rust's
  parser-level no-init diagnostic; until then the flip's direction is correct and the message gap is
  the fixture's already-logged papercut.
- **The sole spurious (`snag51`) is a self-host bug, not a Rust-reference defect:** Rust correctly
  accepts it (the program IS valid). The flip false-rejecting it would be the self-host being
  WRONG — so the gate correctly blocks the flip until A_closure makes the self-host accept it too.
  No known defect ships.

---

## 8. Disjointness

- **β flip zone:** `resolve.gg` EIdentifier-miss arm (`:645-658`) + `is_compiler_intrinsic_name`
  (after `:105`) + `ResolveContext.import_allow` (`:58`) + `resolve_module_with_imports` (`:1119`);
  `scope.gg` `is_known_variant_name` (after `:99`); `loader.gg` `imported_bare_names` carrier
  (`:573,714,1122`); `driver.gg` 3 call-site pairs. Plus the 4th copy
  `self_host_resolver/resolve.gg` for production.
- **Track-α (COMPLETE):** the meta arms `SMetaFor`/`SMetaForMatch`/`SMetaIf` in `resolve_stmt`
  (`:584-617`) and the second copy (`:1015-1035`) — STATEMENT resolution, disjoint from the
  EXPRESSION-miss flip site (`:645`). Different functions, >25 lines apart. Track-α is landed, so no
  pending collision.
- **A_closure zone:** `lexer.gg:255-270` (KwBox-family keyword arms) + `parser.gg` keyword_tag/type/
  expr special-cases + the latent Box-lowering fix in `lir_lower.gg`. File-disjoint from the β flip
  zone (no shared file). They can land in either order; A_closure-first is cleanest.

A merge is clean. The flip + the machinery are one coordinated change (B1+B2 are entangled at the
single EIdentifier-miss read site — you cannot flip the `pass` without ALL of {builtin, intrinsic,
import-allow, variant} guards in place).

---

## 9. Reproduce (every number regenerated this session)

```bash
git merge --ff-only gorget-1   # tip 85217064
cargo build --release

# re-apply the machinery (29838afc shape MINUS parser.gg A.1b, landed in 2fa251ff) — see commit 45d6cae0
# instrument: resolve.gg EIdentifier-miss else: -> print("FLIP_UNDEF " + name, file=stderr)
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg -o /tmp/sh_beta

# the 3 firing fixtures + the oracle
for f in tests/fixtures/*.gg; do /tmp/sh_beta check "$f" --lib-dir=lib 2>&1 | grep '^FLIP_UNDEF ' | sed "s|^|$(basename $f) |"; done | awk '{print $1}' | sort -u
#   -> snag51_closure_block_tail_value, undefined_name_error, variable_no_initializer_errors
for f in snag51_closure_block_tail_value undefined_name_error variable_no_initializer_errors; do
  ./target/release/gg check tests/fixtures/$f.gg </dev/null 2>&1 | grep -iE 'OK|undefined|initializer'; done
#   -> snag51: OK (clean-accept = SPURIOUS); undefined_name_error: rejects (WIN); var_no_init: rejects (negative)

# confirm Inc-1c + Track-α cleared their classes (all 0)
for f in ecs_advanced ecs_basics ecs_query2 sqlite_basic field_access meta_fields; do
  echo -n "$f: "; /tmp/sh_beta check tests/fixtures/$f.gg --lib-dir=lib 2>&1 | grep -c '^FLIP_UNDEF '; done

# parity-neutrality: byte-diff baseline vs prototype typechecker driver over the corpus -> 0
#   (build baseline from 85217064 resolve.gg/scope.gg, build prototype, diff `driver <fixture>` stdout)
cargo test --test integration --release type_comparison resolver_comparison -- --nocapture --test-threads=1
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
```

## 10. Docs the design rests on
- `docs/devbook/07-name-resolution.md:280-319` — the `Expr::Identifier`-miss exclusion triple
  (`is_builtin` / `__return__` / `is_known_variant_name`), the `field_value`/`field_set`/
  `make_variant` carve-out, the f-string error-sink; `:82-92` — the `name_index` O(K) variant
  lookup (the reference-grade production shape for `is_known_variant_name`); the 4-copy resolve.gg
  layout.
- `docs/language-design.md:176` — no-initializer decl is a compile error (the
  `variable_no_initializer_errors` invariant); §2.1 — undefined name = hard compile error.
- `src/semantic/resolve.rs:1485-1509` — the Identifier-miss `UndefinedName` reference (the exact
  exclusion shape the flip mirrors); `src/semantic/scope.rs:442` — `is_known_variant_name`.
- `src/lexer/token.rs:317` — `Box`/`Rc`/… are IDENTIFIERS in Rust (the A_closure reference).
- `docs/plans/caseb-aclosure-scout.md` — the A_closure de-keyword + latent Box-lowering bug (the
  sole flip prerequisite); `docs/plans/caseb-inc1c-scout.md` — what A.1b + A_typealias cleared;
  `docs/plans/caseb-track-beta-scout.md` — the machinery this re-applies (stale §3 residual).
- CLAUDE.md — Core-#8 (reference-grade gate: the no-init message gap is a finding, not a benign
  pass; the snag51 spurious is a self-host bug to FIX, not allow-list), "no name matching"
  (centralized intrinsic predicate), "fix the class, not the instance" (the 4th resolve.gg copy +
  the name-index variant production shape), "re-verify a premise" (the β scout's §3 was stale —
  re-measured here).
