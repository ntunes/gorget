# Executor Brief — fix the imported-module semantic-check bypass (the meta-bug + its ~7 victims)

**Status:** DRAFT — under ≥3 fresh-review discipline before launch. Fallout recon-backed (root cause +
all bug sites measured). **Risk:** MEDIUM-HIGH (touches Rust checker + stdlib + multiple self-host dirs;
breaks the self-host suite until the cleanup lands — must be ONE coherent chain).

## 0. Worktree discipline
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch
`/workspace/gorget-1`; no `cd` there; no `/workspace/gorget-1/...` paths. `git add <specific files>` only —
never `-a`/`.`/`commit -a`. Commit in your worktree (several commits OK — suggest: one per bug cluster, then
the truncate removal last). FORCE-REBUILD the self-host driver before comparison/bootstrap runs
(`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`). Run `cargo build`
+ `cargo test --lib` + the targeted gates below. ⚠ Because this touches the self-host comparison/bootstrap,
you WILL need to run those (they're the gate) — use `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=120`. Do
NOT run the entire `cargo test --test integration` (parent's final job); run the SPECIFIC comparison +
bootstrap tests named in §4.

## 1. The bug + the fix (root-caused by the fallout recon)
Rust gg checks imported-module bodies but **throws the errors away**: `check_items_recursive_tc`
(`src/semantic/typecheck.rs:6127`) descends into `Item::Module` and DOES call `check_function` on imported
bodies, but the `Item::Module` branch at **`typecheck.rs:6143-6151`** snapshots `errors.len()` then
`errors.truncate(error_count)` — discarding every error from the imported module, re-appending only
`hard_errors` (concrete call-arg mismatches, populated solely at `:1434-1448`). So `NonExhaustiveMatch` +
ordinary body type errors are checked-then-discarded. The language spec REQUIRES exhaustive match
(`docs/language-design.md:44,1003`), so this is a bug. `check_match_exhaustiveness` (`:3248`, incl. the
`meta for` deferral at `:3260`) is correct and already runs — recon confirmed ZERO false positives.

**THE FIX = remove the truncate** at `typecheck.rs:6143-6151` (stop discarding imported-module errors; keep
the recursion). Recon verified: ~6-line deletion, no new state threading, both regression tests then pass
un-ignored. ⚠ **But the deletion surfaces ~7 real latent bugs that currently break `gg build`/`gg check` of
the self-host + stdlib-importing fixtures — so FIX THOSE FIRST, then remove the truncate LAST.**

## 2. Fix the ~7 real bugs FIRST (recon-measured; all genuine latent defects — fix, don't dodge)

### Rust stdlib (3 sites)
1. **`lib/xtd/httpserver.gg:584` & `:608`** — these are LOCAL-VARIABLE annotations (NOT return-type decls):
   `url_decode`/`form_decode` are IMPORTED from `std.encoding` (declared `Result[String, ParseError]` at
   `std/encoding.gg:69,104`), but the local result is annotated `Result[String, String]` at `:584`/`:608` —
   a mismatch. Every OTHER call site in httpserver.gg already annotates `ParseError` (`:217,220,386,389,580,581,
   604,605`); only these 2 are wrong. Fix → change the local annotation to **`Result[String, ParseError]`**.
   Surfaces in **24** importer fixtures (`grep -rln 'xtd.httpserver' tests/fixtures/*.gg` = 24; a 25th
   "httpserver" hit in `cow_borrow_outlives_push.gg:17` is a comment-only mention, not an import).
2. **`lib/xtd/yaml.gg:1269` & `:1276`** — `yaml_p.pos` references a NONEXISTENT field; the correct field is
   **`yaml_pos`** (silent miscompile today). Fix the field name (grep the struct def to confirm).
3. **`lib/xtd/jsonpath.gg:248` & `:252`** — `segments.push(.ArrayLen)` / `.Wildcard` dot-shorthand enum ctor
   not inferred through the `push()` arg (the expected-type isn't threaded into the `push` argument).
   ⚠ **DECISION (per "Don't redesign around compiler gaps"): PREFER fixing the inference gap.** The error is
   `expected enum type context for dot-shorthand`; `DotShorthand` resolution (typecheck.rs:2685-2719) depends
   entirely on `decl_type_hint` being set. For builtin collection methods (`push`/`put`/`insert`/`set`) the
   elem type is NOT a sig param — `push` routes through `builtin_method_type` (**`src/semantic/typecheck.rs:4810`**,
   NOT the lowering file), and its args are inferred at `:1905-1907` with NO hint and BEFORE `builtin_method_type`
   runs. **The bounded fix: extract the receiver's elem type (`type_args.first()`, `:4804`) and set
   `decl_type_hint` before that arg-inference loop.** (`push(Segment.ArrayLen())` qualified already works via
   `:1741-1762`, so it's purely the shorthand-hint gap.) Note: this same fix also clears `lib/xtd/query.gg`'s
   shorthand sites (see §3). If too large for this chain, FALLBACK: qualify the 2 sites
   (`.ArrayLen`→`Segment.ArrayLen()`) WITH a cited TODO. State which you chose.

### Self-host (fix in ALL affected driver-dir copies — recon enumerated them; self-host convention = fix every copy)
4. **`parser.gg` `snap_name_tok.span`** → `SpannedToken` has NO `span` field (fields: `lex_token`/`lex_start`/
   `lex_end`); the correct field is **`.lex_start`** (the check/lowerer/typechecker copies already use it —
   these are 2 DRIFTED copies). Sites: `self_host_parser/parser.gg:2521`, `self_host_resolver/parser.gg:2496`
   (grep `snap_name_tok.span` across all `tests/fixtures/self_host_*/parser.gg` to catch every drifted copy).
5. **`parser.gg` two `match X.expr: case EMethodCall` single-arm fall-through matches** (in all 5 parser
   copies) — a single `case` + a trailing `return`, NO `else` → genuinely non-exhaustive (the entry-file
   checker rejects this idiom). Fix: add an explicit **`else: pass`** (or `case _: pass`) to make the
   no-op-on-other-variants intent spec-compliant. Grep the 5 `self_host_*/parser.gg` copies.
6. **`resolve.gg` `resolve_stmt` `match stmt:`** missing statement arms — **4 PATHS, 2 REAL FILES (symlinked;
   brief-review pass-1+2).** `self_host_{check,lowerer}/resolve.gg` are SYMLINKS to
   `self_host_typechecker/resolve.gg` (one real file, md5 `8a3198d5`, `resolve_stmt` match at `:396`) — it
   misses `SMeta`+`SAssertReturn`+`SSnapshot`. The distinct real file `self_host_resolver/resolve.gg` (md5
   `113aeab8`, match at `:443`) misses ONLY `SMeta` (its `SAssertReturn:590`/`SSnapshot:614` arms already
   exist). ⚠ **(pass-2 — BLOCKING) `else: pass` is WRONG for `SAssertReturn`/`SSnapshot`** — they carry
   expressions the canonical resolver MUST resolve (Rust `src/semantic/resolve.rs:1235-1244`; the resolver
   copy already does `resolve_expr(condition)` at `:590` / `resolve_expr(value)` at `:614`). An `else: pass`
   would DODGE resolution (drop RES entries → regress `type_comparison`/`check_comparison`/`c_emit_comparison`
   on `assert_return_*`/`snapshot_basic`) AND mask the exhaustiveness error via the `has_else` short-circuit —
   a §6 violation. **Add REAL arms** to the typechecker file (`:396`): `case SAssertReturn(condition, _):
   resolve_expr(condition, &scopes, &ctx)` + `case SSnapshot(_, _, value): resolve_expr(value, &scopes, &ctx)`
   + `case SMeta(): pass` (SMeta is a genuine no-op — ast.gg:107, no payload). For the resolver file (`:443`),
   add ONLY `case SMeta(): pass`. (Editing `typechecker/resolve.gg` fixes its 3 symlinked paths.) The SECOND
   function `resolve_stmt_expr` (`:767`) already has a trailing `else:` — leave it. Confirm each `resolve_stmt`
   is exhaustive after your edit, and that the `assert_return_*`/`snapshot_basic` comparison counts do NOT
   regress (proof the real arms resolve correctly).
7. **`self_host_lowerer/format_gir.gg:167`** missing **`GIFieldLoad`** arm, **`:196`** missing **`GTNone`**
   arm (lowerer only). ⚠ NOTE the `&`-param fix just added a `GIDerefStore` arm here — your `GIFieldLoad`
   arm is separate. Add a render arm for each (mirror the sibling arms). (This is the exact `format_gir`
   case the `&`-param brief-review pass-4 cited as the proof of the bypass.)

⚠ **Self-host multi-copy rule:** `parser.gg`/`resolve.gg`/`ast.gg` exist as INDEPENDENT copies across
`self_host_{lexer,parser,resolver,typechecker,lowerer,check}/` (some symlinked — `md5sum` to tell). A fix to
a shared primitive must land in EVERY copy that has the defect, or that dir's `gg check`/comparison stays
red. Grep each defect across all 6 dirs; fix every real occurrence.

## 3. THEN remove the truncate + un-ignore the tests (LAST)
- Delete the `errors.truncate(...)` + `hard_errors` re-append at `typecheck.rs:6143-6151` (keep the
  recursion). ⚠ (brief-review pass-1) BOTH the `error_count` (`:6143`) AND `hard_count` (`:6144`) snapshots
  become dead — remove them too. The clean reduction is the **9-line block → the single line
  `check_items_recursive_tc(checker, inner);`** (review-verified it builds).
- ⚠ **(brief-review pass-1 — not a blocker, log it) the recon MISSED 3 same-class latent stdlib bugs:**
  `lib/xtd/query.gg` (same dot-shorthand class + `Option[Json]` mismatches — the jsonpath inference fix above
  ALSO clears query's shorthand), `lib/xtd/ssh.gg` (~17 Result-unwrap mismatches), `lib/xtd/gpu.gg`
  (duplicate-def + GLContext mismatches). **NONE are imported by any fixture** (grep-verified), so they do NOT
  gate the comparison/bootstrap suite and are OUT of scope for this chain — but they're real. LOG them to
  TODO (per §6) so they're not lost; do NOT expand this chain to fix them.
- Remove `#[ignore]` from `imported_nonexhaustive_match_should_error` + `imported_body_type_error_should_error`
  (`tests/integration.rs` ~`:1895`). They should now PASS (the `#[ignore]` comments' root-cause description
  was already corrected to cite the truncate).

## 4. Gates (ALL must hold — this chain is "green" only when the whole self-host suite re-greens)
1. `cargo build` clean; `cargo test --lib` green (recon: stays 1065/0 — confirm).
2. The 2 un-ignored tests PASS: `imported_nonexhaustive_match_should_error`, `imported_body_type_error_should_error`.
3. **`gg check` is now CLEAN on ALL 6 self-host drivers** — `cargo run -- check tests/fixtures/self_host_{lowerer,parser,resolver,typechecker,check,lexer}/driver.gg` → no semantic errors each. (This is the burn-down's success signal. `self_host_lexer` has none of the 7 defects but check it for completeness.)
4. **The self-host comparison + bootstrap suite re-greens** (force-rebuild the driver first). ⚠ (brief-review pass-1) the `*_comparison` tests are **DIAGNOSTIC-ALWAYS-PASS (no `assert!`)** — "green" is MEANINGLESS; you MUST read the printed matched-counts via `--nocapture` for ALL of them: `c_emit_comparison` (expect ≥**850**, may RISE as the 24 httpserver fixtures + others now type-check — record the number), `lowerer_comparison`, `resolver_comparison`, `parser_comparison`, `type_comparison`, `check_comparison` — each at-or-above its baseline (none regressed). Only `self_host_bootstrap` + `self_host_bootstrap_fixed_point` actually ASSERT (real red/green) — both must be GREEN. ⚠ A regressed count or a red bootstrap means a self-host defect you missed — fix it, don't exclude it.
5. The 24 httpserver + 2 yaml + the jsonpath-importing fixtures build + run correctly (the stdlib fixes).
   NOTE jsonpath is reached via `query_basic.gg` (imports `xtd.jsonpath`), not a `jsonpath_*` fixture.

## 5. Report back
The diff per bug cluster + commit hashes; the jsonpath decision (inference fix vs qualify+TODO); the
before/after `c_emit` count; confirmation `gg check` is clean on all self-host drivers; the full comparison +
bootstrap gate results; the 2 newly-active tests passing. Flag any NEW latent bug the truncate-removal
surfaced that you had to fix (likely — report each).

## 6. Don't-dodge rules
- Every error the truncate-removal surfaces is a REAL bug (recon found zero false positives except the
  jsonpath inference gap). FIX each at its source; do NOT re-add a suppression, do NOT `#[ignore]` a
  comparison, do NOT exclude a fixture to make the suite green. If you find a NEW false-positive class the
  recon missed (e.g. a generic-T match the exhaustiveness checker wrongly flags), STOP and report it — that's
  a checker bug to fix, not a reason to keep the truncate.
- The self-host fixes must read like idiomatic Gorget (the showcase rule) — an `else: pass` for a genuine
  no-op is fine; a missing arm that SHOULD handle a case must handle it, not be `else`'d away.
