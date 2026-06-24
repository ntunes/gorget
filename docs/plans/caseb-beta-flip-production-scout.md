# Scout — Case-B β-flip PRODUCTIONIZE + fully MEASURE (the TERMINAL Case-B deliverable)

**Status:** READ-ONLY + PRODUCTION-PROTOTYPING scout, fresh measurement 2026-06-24, worktree off
`gorget-1` tip `0e036317` (A_closure `19d1529a` LANDED — the SOLE β-flip prereq cleared; all
Inc-1c + Track-α 4a/4b/4c prereqs present). Productionizes `docs/plans/caseb-beta-flip-remeasure-scout.md`
(which measured the residual 27→1 off `85217064`; A_closure has since cleared that last one). **Every
number below was regenerated THIS session** by re-applying the machinery WITH THE REAL
`DkUndefinedName` PUSH (not a FLIP_UNDEF print), building all driver copies, sweeping the full
**1294**-fixture corpus, cross-checking every firing against the Rust `gg check </dev/null` oracle,
and proving parity-neutrality by BYTE-DIFFING the type+check drivers vs baseline over the whole corpus.
The prototype is committed as a throwaway `PROTOTYPE(scout): … DO NOT INTEGRATE` (`62b41d2a`); it must
NOT be integrated.

Grounded in `docs/devbook/07-name-resolution.md:85-92` (the `name_index: FxHashMap<String, Vec<DefId>>`
O(K) variant lookup), `:148` (non-generic variants in `name_index` but NOT the value namespace),
`:285-289` (the `Identifier`-miss `UndefinedName` site + `is_known_variant_name` exclusion), `:298-309`
(the `field_value`/`field_set`/`make_variant` ECall carve-out + the 4-copy resolve.gg layout);
`docs/language-design.md:176` (no-initializer decl = compile error) + §2.1 (undefined name = hard
compile error); and the Rust reference `src/semantic/resolve.rs:1485-1509` (the Identifier-miss
`UndefinedName` site) + `src/semantic/scope.rs:193-210` (`alloc_def` inserts into `name_index`) + `:442`
(`is_known_variant_name`).

---

## 0. HEADLINE — spurious = 0, bootstrap GREEN with the REAL flip, fully parity-neutral

| metric | re-measure scout (`85217064`) | **THIS scout (`0e036317`, regenerated 2026-06-24)** |
|---|---|---|
| corpus | 1294 | **1294** |
| fixtures the REAL flip rejects (undefined name) | 3 (FLIP_UNDEF print) | **2** (real `DkUndefinedName` push) |
| → genuine WIN (Rust rejects undefined name) | 1 | **1** (`undefined_name_error` → `nonexistent`) |
| → Rust-also-rejects, divergent message | 1 | **1** (`variable_no_initializer_errors`) |
| → **TRUE SPURIOUS (Rust CLEAN-ACCEPTS)** | 1 (`snag51`, A_closure-blocked) | **0** (A_closure cleared `snag51`) |
| `self_host_bootstrap_fixed_point` with REAL flip | (proven with FLIP-print) | **GREEN, 295.60s** (real push) |
| parity (`self_host_runtime_diff` MATCH) | 754/1069 (FLIP-print) | **754/1069 = 70.5%** (real push) |
| type/check driver stdout vs baseline (all 1294) | 0 diff | **0 diff** (byte-identical, both drivers) |

**Bottom line:** the β flip is READY. With A_closure landed, the spurious-on-Rust-clean-accept residual
is **0**. The REAL `DkUndefinedName` push (not a print) is **bootstrap-GREEN** (the load-bearing
question — answered below), is **byte-identically parity-neutral** for the comparison/build drivers
(proven by full-corpus stdout diff, the strongest form), and does the reference-grade-correct thing on
the genuine win. The satisfiable-gate test is added + PROVES-IT-BITES. The variant query is name-indexed
(O(K), per devbook), and the 4th `self_host_resolver` copy is kept in sync (measurement-neutral). The
ONLY owner-call is the divergent MESSAGE on the `variable_no_initializer_errors` NEGATIVE (both reject;
escalation #1) — NOT a flip blocker.

**Decompose verdict:** land the machinery + flip as **ONE coordinated parity-affecting change** (B1+B2
are entangled at the single EIdentifier-miss read site). The diag test + the 4th-copy sync ride along.
See §8.

---

## 1. The REAL flip re-applied on the current tip (BUILDS — all driver copies)

Re-applied the `caseb-beta-flip-remeasure-scout.md` §4 machinery on `0e036317`, with the FLIP-print
replaced by the **production `DkUndefinedName` push**, the variant query **name-indexed** (not a linear
scan), and the **4th `self_host_resolver` copy** brought into variant-query sync. The prototype builds
the lowerer + type + check + resolver drivers clean (each force-built this session; the lowerer driver
at `/tmp/sh_final_*`, type at `/tmp/td_proto_*`, check at `/tmp/cd_proto_*`).

**The flip site** (`self_host_typechecker/resolve.gg:650-667`, symlinked into lowerer+check):
```gorget
        case EIdentifier(name):
            if scopes.lookup(name) is Some(def_id):
                ctx.resolution_map.put(expr.span.start, def_id)
            elif is_builtin(name):
                pass
            elif is_compiler_intrinsic_name(name):
                pass
            elif ctx.import_allow.contains(name):
                pass
            elif is_known_variant_name(scopes, name):
                pass
            else:
                # Genuinely-undefined name — reject it (mirrors Rust gg
                # resolve.rs:1485-1509). Drains to stderr via the comparison
                # driver; halts the build/check path via has_errors(ctx.diagnostics).
                ctx.diagnostics.push(Diagnostic.error(expr.span, DkUndefinedName(), "undefined name `" + name + "`"))
```
`DkUndefinedName` is imported (`resolve.gg:18`, `from diagnostic import Diagnostic, DkUndefinedName`;
exists `diagnostic.gg:43`, rendered `:99`). The guard order matches Rust's exclusion triple
(builtin / synthetic-intrinsic / known-variant) plus the B2 import-allow set.

---

## 2. RE-MEASURED spurious residual on the current tip = **0** (regenerated this session)

**Method.** Force-built the prototype lowerer driver with the REAL push:
```
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg -o $SH
```
Swept `check` mode over all 1294 fixtures, counted fixtures emitting a `DkUndefinedName` (the real push
prints `error: undefined name '…'` and `exit(1)`s), cross-checked each against the Rust oracle:
```
for f in tests/fixtures/*.gg; do
  hits=$($SH check "$f" --lib-dir=lib 2>&1 | grep -c 'undefined name')
  [ "$hits" -gt 0 ] && echo "$(basename $f .gg) $hits"
done
#   -> undefined_name_error 1
#      variable_no_initializer_errors 5         (EXACTLY 2 fixtures fire)
for f in undefined_name_error variable_no_initializer_errors snag51_closure_block_tail_value; do
  ./target/release/gg check tests/fixtures/$f.gg </dev/null >/dev/null 2>&1; echo "$f rust-exit=$?"; done
#   -> undefined_name_error rust-exit=1 ; variable_no_initializer_errors rust-exit=1 ; snag51 rust-exit=0
```

| fixture | self-host flip | **Rust `gg check`** | class |
|---|---|---|---|
| `undefined_name_error` | REJECTS (`undefined name 'nonexistent'`) | **REJECTS** (same message) | **genuine WIN** |
| `variable_no_initializer_errors` | REJECTS (`undefined name 'i'`/`f`/`flag`…) | **REJECTS** (`variable declaration requires an initializer`) | Rust-also-rejects, divergent message |
| `snag51_closure_block_tail_value` | **clean-accept (`ok`)** | **CLEAN-ACCEPT** (`ok`) | A_closure cleared it ⇒ **no longer spurious** |

**`snag51` now clean-accepts on BOTH** — A_closure de-keyworded `Box`, so `case Box.A(s):` binds `s`/`n`
in the parser and the resolver no longer sees them undefined (confirmed: `$SH check snag51… ` → `ok`).
The TRUE SPURIOUS residual is **0**. The only two firing fixtures are Rust-rejected negatives.

The re-measure scout's escalations #2 (meta fixtures) and the Entity/sqlite classes are all **0** now
(Track-α + Inc-1c cleared them before A_closure; re-confirmed: `field_access`, `meta_fields`,
`ecs_*`, `sqlite_basic` each emit 0 FLIP hits in the prior scout and 0 undefined-name in this one).

---

## 3. ⛔ THE LOAD-BEARING BOOTSTRAP QUESTION — answered: YES the diag runs, and the flip stays GREEN

**Does the self-host driver RUN the resolver's undefined-name DIAGNOSTIC during self-compile?**
**YES.** The lowerer driver's build AND check paths gate on `has_errors(ctx.diagnostics)` and `exit(1)`:
- `self_host_lowerer/driver.gg:417-419` (compile_main / build path) — `if has_errors(ctx.diagnostics): report_diagnostics(...); exit(1)` BEFORE `lower_module`.
- `:656-657` (run_build_mode) and `:790-792` (run_check_mode) — same gate.

So the REAL `DkUndefinedName` push is NOT bootstrap-inert: if the self-host's OWN source resolved any
name the machinery did not cover, the self-compile would HALT and the bootstrap would break. The
comment at `driver.gg:414-416` even says the gate assumed "ZERO diagnostics on its own source" — but
that was written for the TYPECHECK pass; the flip makes the RESOLVER a new diagnostic source.

**PROOF it stays GREEN with the REAL flip (not a print):**
```
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
#   test self_host_bootstrap_fixed_point ... ok      (finished in 295.60s)
```
The driver self-compiles its own flipped resolver (which now pushes `DkUndefinedName` + halts on
errors) and STILL re-converges stage-2==stage-3==stage-4 byte-identical. **Why it holds:** the B2
import-allow set (`imported_bare_names`) covers every transitively-merged library bare call in the
self-host's own source. Spot-checked directly (all 0 undefined-name hits):
```
$SH check tests/fixtures/self_host_lowerer/driver.gg --lib-dir=lib   -> ok
$SH check tests/fixtures/self_host_typechecker/{resolve,typecheck}.gg --lib-dir=lib  -> 0 hits
$SH check tests/fixtures/self_host_lowerer/{lower,loader}.gg --lib-dir=lib            -> 0 hits
```
The bootstrap-green is the load-bearing validation: the self-host's own source compiles cleanly through
the flipped resolver. **The flip is bootstrap-SAFE.**

---

## 4. The satisfiable-gate test — ADDED, RUNS, ASSERTS, PROVES-IT-BITES

**Test:** `self_host_rejects_undefined_name` (`tests/integration.rs:17776`, `#[serial(self_host_lowerer_driver)]`,
default-running / build-breaking). Builds the self-host lowerer driver (cached, shared with the bootstrap
+ comparison tests) and runs it in `check` mode against `undefined_name_error.gg`:
```rust
let output = run_with_timeout(
    Command::new(&driver_exe).arg("check").arg(&fixture).arg(format!("--lib-dir={}", lib_dir.display())),
    "undefined_name_error.gg");
assert!(!output.status.success(), "…must REJECT…the flip likely regressed to `pass`…");
assert!(String::from_utf8_lossy(&output.stderr).contains("undefined name"), "…");
```
**Runs + passes (this session):**
```
cargo test --test integration --release self_host_rejects_undefined_name -- --nocapture
#   test self_host_rejects_undefined_name ... ok
```
**PROVES-IT-BITES (this session):** reverted the flip's `else:` push back to `pass`, rebuilt, re-ran →
```
thread 'self_host_rejects_undefined_name' panicked at tests/integration.rs:17804:
… must REJECT undefined_name_error.gg …, but it succeeded. The flip likely regressed to `pass` …
test result: FAILED. 0 passed; 1 failed;
```
The test is a genuine ratchet: it FAILS the instant the flip regresses to silent-accept. (I restored
the flip after the bite check.) This is the diag-surfacing gate the re-measure scout §6 designed —
keyed on the build/CHECK path where the push surfaces, NOT the type_comparison driver which drains to
stderr. The companion satisfiable guarantee (NO Rust-clean-accept is false-rejected) is the §2 full-corpus
measurement (0 spurious), not a per-fixture assert.

---

## 5. Productionization (per the re-measure scout's notes)

### 5.1 `is_known_variant_name` is NAME-INDEXED (O(K)), not a linear scan

The prototype uses a `variant_name_index: Dict[String, Vector[int]]` on `ScopeTable`
(`self_host_typechecker/scope.gg:83`), populated in `alloc_def` (`:252` — every `DkVariant` records its
def_id), consulted by `is_known_variant_name` (`:131` — O(K) lookup over the (typically 1–5) def_ids for
that name). This mirrors Rust gg (`scope.rs:195` `alloc_def` inserts into `name_index`; `:442`
`is_known_variant_name` consults it) per `docs/devbook/07:85-92`.

**⚠ Self-host vs Rust `name_index` mismatch — the reason for a SEPARATE index.** In Rust,
`name_index: HashMap<String, Vec<DefId>>` maps name → **def_ids**, and `alloc_def` pushes the def_id
there. In the self-host, the existing `name_index: Dict[String, Vector[int]]` maps name → **parallel
name-table indices** (`name_keys`/`name_def_ids`), and `alloc_def` does NOT touch it (variants are not
in the name table). Reusing `name_index` would conflate def_ids with name-table indices. So the
production-grade self-host form is a DEDICATED `variant_name_index` (name → def_ids) — NOT a linear scan
(the prototype's earlier (ii) form), NOT a reuse of `name_index`. This is the reference-grade shape.

### 5.2 The 4th `self_host_resolver/resolve.gg` copy — synced (variant query only)

The 4th copy is the standalone DIAGNOSTIC resolver used by `resolver_comparison`. **It does NOT call
`load_imports`** (its driver resolves a single file in isolation — `self_host_resolver/driver.gg:23`
`resolve_module(m, …)`, no import merge), so the B2 import-allow machinery is **inapplicable** there.
**It ALREADY pushes `DkUndefinedName`** at its EIdentifier-miss arm (`resolve.gg:683` baseline — it is
NOT a `pass`), and already matches Rust at 1276/1294. The only β-machinery that applies is the B1
variant query, which the prototype adds for sync (`resolve.gg:684`; `scope.gg:67/104/203` — its own
`variant_name_index` since its `scope.gg` is a simpler shape with no `name_index`/`children`).
**MEASURED neutral:**
```
cargo test --test integration --release resolver_comparison -- --nocapture --test-threads=1
#   Fixtures compared: 1294, matched: 1276, mismatched: 18, crashed: 0     (= baseline 1276, 0 crashes)
```
So the 4th-copy variant query changes nothing measurable but keeps the class in sync (CLAUDE.md "fix the
class, not the instance"). **Production note:** the 4th copy's flip is ALREADY done (it always pushed);
only the variant carve-out + index are the new sync. If the orchestrator prefers minimal blast radius,
the 4th-copy variant query is optional (measurement-neutral) — but applying it is reference-grade.

### 5.3 The `self_host_check` driver — inherits the flip, empty allow-set, byte-neutral

`self_host_check/driver.gg` (used by `check_comparison`) shares the symlinked `resolve.gg`/`scope.gg`
(so it inherits the flip + variant query) but calls the 3-arg `resolve_module` (empty allow-set) and a
DIFFERENT loader (`load_all`, not `load_imports`). Its `loader.gg` is a separate real file — it does
NOT need the `imported_bare_names` carrier (the check driver prints `format_types` and drains diagnostics
to stderr; the flip is stdout-neutral there). **PROVEN byte-neutral over the full corpus** (§6.2). The
production land does NOT need to touch `self_host_check/loader.gg`.

---

## 6. ALL gates (regenerated this session — quote the command, not a remembered number)

### 6.1 Parity — NEUTRAL (754/1069 = 70.5%, = A_closure baseline)
```
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=60 \
  cargo test --test integration --release self_host_runtime_diff -- --nocapture --test-threads=1
#   MATCH 754  WRONG-OUTPUT 90  CC-FAIL 194  CRASH 31  RUST-CRASH 0
#   PARITY = 754/1069 = 70.5%
```
Identical to baseline: the `DkUndefinedName` drains to stderr; the only fixtures the flip rejects are the
2 Rust-rejected negatives, which were never in the MATCH set (they don't produce a runnable binary under
Rust either). The flip is parity-neutral by construction.

### 6.2 type/check drivers — BYTE-IDENTICAL to baseline over ALL 1294 fixtures (the strongest proof)
Built BASELINE type+check drivers (from HEAD `resolve.gg`/`scope.gg`) and PROTOTYPE drivers, diffed
per-fixture stdout over the whole corpus:
```
# baseline TD = /tmp/td_base_* ; prototype TD = /tmp/td_proto_*
# baseline CD = /tmp/cd_base_* ; prototype CD = /tmp/cd_proto_*
for f in tests/fixtures/*.gg; do [ "$($TDB $f 2>/dev/null)" != "$($TD $f 2>/dev/null)" ] && echo $f; done | wc -l
#   STDOUT-differing fixtures (TYPE driver, baseline vs prototype)  = 0
for f in tests/fixtures/*.gg; do [ "$($CDB $f 2>/dev/null)" != "$($CD $f 2>/dev/null)" ] && echo $f; done | wc -l
#   STDOUT-differing fixtures (CHECK driver, baseline vs prototype) = 0
```
**ZERO diffs in BOTH** — the machinery + real flip add NOTHING to the comparison drivers' stdout.

### 6.3 Comparison counts (diagnostic-always-pass; the §6.2 byte-diff is the load-bearing proof)
```
cargo test --test integration --release type_comparison      -- --nocapture --test-threads=1
#   Fixtures compared: 1294, exact: 1180, superset: 42, total: 1222, mismatched: 72, crashed: 0
cargo test --test integration --release check_comparison     -- --nocapture --test-threads=1
#   Fixtures compared: 1294, exact: 1160, superset: 61, total: 1221, mismatched: 73, crashed: 0
cargo test --test integration --release resolver_comparison  -- --nocapture --test-threads=1
#   Fixtures compared: 1294, matched: 1276, mismatched: 18, crashed: 0
# (batched run, --test-threads=4): parser 1271 / 23 / 0 ; lexer 0 mismatch / 0 crash ; c_emit 1045 matched / 0 self-host crash
```
**⚠ The `type_comparison`/`check_comparison` mismatch count is FLAKY by ±1** (`serialize_collections.gg`
flips between MATCH and mismatch on the synthetic temp `__t14`/`__t49` numbering vs Rust — a PRE-EXISTING
nondeterminism, NOT my machinery). A batched `--test-threads=4` run showed 73 type-mismatched; the clean
single-threaded re-run showed 72 (= baseline); the per-fixture byte-diff (§6.2) confirms baseline and
prototype type-driver output are IDENTICAL for `serialize_collections` (diff exit 0). So the load-bearing
number is the **0-diff** in §6.2, not the flaky comparison count. `type` is parity-neutral.

### 6.4 `self_host_runtime` lock-in + the new diag test
```
cargo test --test integration --release self_host_runtime -- --nocapture --test-threads=1
#   passing set : 727   regressed : 0   (= A_closure baseline 727/0; includes self_host_full_program ok)
cargo test --test integration --release self_host_rejects_undefined_name -- --nocapture
#   test self_host_rejects_undefined_name ... ok
```

### 6.5 Bootstrap (the load-bearing real-flip proof, §3)
```
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
#   test self_host_bootstrap_fixed_point ... ok   (finished in 295.60s)
```

---

## 7. Reference-grade gate (Core invariant #8)

- **The genuine WIN is reference-grade.** `undefined_name_error` → the self-host now REJECTS exactly
  what Rust rejects, with the SAME message (`undefined name 'nonexistent'`). This is the whole point of
  Case-B: the self-host correctly rejects an ill-formed program Rust also rejects. ✓ No known defect
  ships in this direction.
- **No Rust-clean-accept program is false-rejected.** The full-corpus sweep (§2) found EXACTLY 2 firing
  fixtures, BOTH Rust-rejected negatives. The single prior spurious (`snag51`) is cleared by A_closure
  and now clean-accepts on BOTH. The satisfiable gate (§4) is satisfiable: 0 spurious. ✓
- **`variable_no_initializer_errors` — directionally-correct, divergent MESSAGE (owner-call escalation,
  NOT a flip blocker).** Both compilers REJECT (`docs/language-design.md:176`: bare `int x` is a compile
  error). The divergence is the MESSAGE: Rust's PARSER rejects the no-`=` shape with `"variable
  declaration requires an initializer"` (`tests/integration.rs:6325` pins it); the self-host parser lets
  `int i` fall through to expression parsing, so the flip rejects it as `"undefined name 'i'"` (the
  OLD, less-helpful message — the fixture's own header comment documents this as a logged papercut). Per
  Core #8 this is a real-but-SMALLER parity gap (the self-host parser lacks Rust's parser-level no-init
  rejection). The reference-grade fix is to PORT Rust's parser no-init diagnostic to the self-host
  parser. It is NOT a spurious-on-clean-accept and does NOT regress runtime parity (negative test, never
  a MATCH). **Flag for the owner; do NOT hold the flip for it** — the flip's direction is correct.

---

## 8. Decompose-or-not + complete site/symlink list

**ONE coordinated parity-affecting change.** B1 (variant-query + intrinsic carve-out) + B2 (import
allow-set) are entangled at the single EIdentifier-miss read site — you cannot flip the `pass` without
ALL of {builtin, intrinsic, import-allow, variant} guards in place, or the flip regresses immediately.
The A.3 carrier (loader) and B1 variant-index (scope) are prerequisites the one arm reads; landing them
piecemeal leaves the flip un-landable and the allow-set un-exercised (it has no observable effect until
the flip). So land carrier + variant-index + intrinsic + threading + flip together, gated as one. The
diag test + the 4th-copy variant sync ride along (the 4th-copy sync is measurement-optional but
reference-grade; §5.2).

**Complete site-list (file:line, against the prototype `62b41d2a`):**

*Main machinery — `resolve.gg` + `scope.gg` are SYMLINKED `self_host_typechecker → lowerer, check`*
(edit the `self_host_typechecker` REAL files; lowerer+check inherit):
- `self_host_typechecker/resolve.gg`: import `is_known_variant_name` (`:16`) + `DkUndefinedName` (`:18`);
  `ResolveContext.import_allow` (`:60`); ctor (`:136`); `is_compiler_intrinsic_name` (`:116`);
  **the EIdentifier-miss flip** (`:650-667`); `resolve_module` (`:1128`, delegates) +
  `resolve_module_with_imports` (`:1135`).
- `self_host_typechecker/scope.gg`: `variant_name_index` field (`:83`); `new_scope_table` ctor (`:111`);
  `def_kind_is_variant` (`:107`) + `is_known_variant_name` (`:131`); `alloc_def` index-populate (`:252`).

*Carrier + threading — `self_host_lowerer` only (the build/check path):*
- `self_host_lowerer/loader.gg`: `load_imports` signature `+Dict[String,bool] &imported_bare_names`
  (`:573`); `imported_bare_names.put` at the `IFunction` arm (`:718`) + the `IExternBlock` arm (`:1129`).
- `self_host_lowerer/driver.gg`: import `resolve_module_with_imports` (`:5`); 3 call-site pairs —
  `imported_bare_names = {}` + `load_imports(…, &imported_bare_names)` + `resolve_module_with_imports`
  at `:366/387` (compile_main), `:638/650` (run_build_mode), `:772/784` (run_check_mode).

*4th copy — `self_host_resolver` (INDEPENDENT real files; variant-query sync only, measurement-neutral):*
- `self_host_resolver/scope.gg`: `Dict` import (`:4`); `variant_name_index` (`:67`); ctor (added arg);
  `def_kind_is_variant` (`:91`) + `is_known_variant_name` (`:104`); `alloc_def` (`:203`).
- `self_host_resolver/resolve.gg`: import `is_known_variant_name` (`:16`); EIdentifier-miss variant
  carve-out (`:684`). (It ALREADY pushes `DkUndefinedName` — no flip change needed there.)

*The satisfiable gate:*
- `tests/integration.rs`: `self_host_rejects_undefined_name` (`:17776`, `#[serial(self_host_lowerer_driver)]`).

*The genuine-win fixture (already present):* `tests/fixtures/undefined_name_error.gg` (Rust pins
`"undefined name"` at `tests/integration.rs:22691`).

**Disjointness:** the only shared file with Track-α (COMPLETE) is `resolve.gg`, and the flip touches the
EXPRESSION-miss arm (`:650`) while Track-α's meta arms are STATEMENT-resolution (`SMetaFor`/etc., a
different function, >25 lines away). A_closure is file-disjoint (`lexer.gg`/`parser.gg` keyword arms +
`lower.gg` Box-skip) and already landed. No pending collision.

---

## 9. Reproduce (every number regenerated this session)
```bash
git merge --ff-only gorget-1   # tip 0e036317 (A_closure landed)
cargo build --release

# re-apply the machinery + REAL push (this scout's commit 62b41d2a) — see §8 site-list
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg -o $SH

# §2 spurious residual = 0 (only the 2 Rust-rejected negatives fire)
for f in tests/fixtures/*.gg; do h=$($SH check "$f" --lib-dir=lib 2>&1 | grep -c 'undefined name'); [ "$h" -gt 0 ] && echo "$(basename $f .gg) $h"; done
for f in undefined_name_error variable_no_initializer_errors snag51_closure_block_tail_value; do ./target/release/gg check tests/fixtures/$f.gg </dev/null >/dev/null 2>&1; echo "$f rust=$?"; done

# §3 bootstrap GREEN with the real flip (the load-bearing proof)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture

# §6.1 parity NEUTRAL 754/1069
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=60 cargo test --test integration --release self_host_runtime_diff -- --nocapture --test-threads=1

# §6.2 byte-identical type+check drivers vs baseline (build baseline from HEAD resolve.gg/scope.gg, diff stdout over corpus -> 0/0)
# §6.3 comparison counts ; §6.4 lock-in 727/0 + diag test ok ; §4 prove-it-bites (revert flip -> test FAILS)
cargo test --test integration --release self_host_runtime self_host_rejects_undefined_name -- --nocapture --test-threads=1
```

## 10. Docs the design rests on
- `docs/devbook/07-name-resolution.md:85-92` (`name_index` O(K) variant lookup), `:148` (non-generic
  variants in `name_index` but not the value namespace), `:285-309` (the `Identifier`-miss
  `UndefinedName` site + `is_known_variant_name` + the `field_value`/`field_set`/`make_variant` carve-out
  + the 4-copy resolve.gg layout).
- `docs/language-design.md:176` (no-initializer decl = compile error, the
  `variable_no_initializer_errors` invariant) + §2.1 (undefined name = hard compile error).
- `src/semantic/resolve.rs:1485-1509` (the Identifier-miss `UndefinedName` reference — the exact
  exclusion shape the flip mirrors); `src/semantic/scope.rs:193-210` (`alloc_def` inserts into
  `name_index` — the Rust reference for the variant index) + `:442` (`is_known_variant_name`).
- `docs/plans/caseb-beta-flip-remeasure-scout.md` (the residual 27→1 + the machinery shape this
  productionizes); `docs/plans/caseb-track-beta-scout.md` (the original allow-set machinery);
  `docs/plans/caseb-aclosure-scout.md` (A_closure, the cleared blocker).
- CLAUDE.md — Core-#8 (the genuine win is reference-grade; the no-init message gap is a FINDING, not a
  benign pass; 0 spurious ships no known defect), "no name matching" (centralized `is_compiler_intrinsic_name`
  predicate; the variant query is a TYPED def-kind index, not a substring test), "fix the class, not the
  instance" (the 4th resolve.gg copy + the name-indexed variant production shape), "re-verify a premise"
  (the re-measure scout's residual was re-measured here on the post-A_closure tip).
