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
1. **`lib/xtd/httpserver.gg:584` & `:608`** — `url_decode`/`form_decode` declare return `Result[String, String]`
   but should be **`Result[String, ParseError]`** (the actual error type they construct/return). Surfaces in
   24 fixtures. Fix the return-type annotation to match the real error type (read the fn bodies to confirm
   `ParseError` is what's thrown).
2. **`lib/xtd/yaml.gg:1269` & `:1276`** — `yaml_p.pos` references a NONEXISTENT field; the correct field is
   **`yaml_pos`** (silent miscompile today). Fix the field name (grep the struct def to confirm).
3. **`lib/xtd/jsonpath.gg:248` & `:252`** — `segments.push(.ArrayLen)` / `.Wildcard` dot-shorthand enum ctor
   not inferred through the `push()` arg (the expected-type isn't threaded into the `push` argument).
   ⚠ **DECISION (per "Don't redesign around compiler gaps"): PREFER fixing the inference gap** (thread the
   collection's elem type as the expected-type for `push`/`put`/`insert`/`set` args so dot-shorthand resolves
   — find where call-arg expected-type is set, likely `src/ir/lowering/exprs/calls.rs` or the typecheck
   call-arg path; `push(Segment.ArrayLen())` qualified already works, so it's purely the shorthand inference).
   If the inference fix is too large for this chain, FALLBACK: qualify the 2 sites
   (`.ArrayLen`→`Segment.ArrayLen()`) WITH a cited TODO for the inference gap. State which you chose.

### Self-host (fix in ALL affected driver-dir copies — recon enumerated them; self-host convention = fix every copy)
4. **`parser.gg` `snap_name_tok.span`** → `SpannedToken` has NO `span` field (fields: `lex_token`/`lex_start`/
   `lex_end`); the correct field is **`.lex_start`** (the check/lowerer/typechecker copies already use it —
   these are 2 DRIFTED copies). Sites: `self_host_parser/parser.gg:2521`, `self_host_resolver/parser.gg:2496`
   (grep `snap_name_tok.span` across all `tests/fixtures/self_host_*/parser.gg` to catch every drifted copy).
5. **`parser.gg` two `match X.expr: case EMethodCall` single-arm fall-through matches** (in all 5 parser
   copies) — a single `case` + a trailing `return`, NO `else` → genuinely non-exhaustive (the entry-file
   checker rejects this idiom). Fix: add an explicit **`else: pass`** (or `case _: pass`) to make the
   no-op-on-other-variants intent spec-compliant. Grep the 5 `self_host_*/parser.gg` copies.
6. **`resolve.gg:396` & `:443` `match stmt:`** missing `SMeta`/`SAssertReturn`/`SSnapshot` arms (2 copies —
   `self_host_resolver/resolve.gg` + wherever the other lives; grep). Add the missing arms (or an `else: pass`
   if those statements are genuinely no-ops in resolve — match what the entry-checker-clean copies do).
7. **`self_host_lowerer/format_gir.gg:167`** missing **`GIFieldLoad`** arm, **`:196`** missing **`GTNone`**
   arm (lowerer only). ⚠ NOTE the `&`-param fix just added a `GIDerefStore` arm here — your `GIFieldLoad`
   arm is separate. Add a render arm for each (mirror the sibling arms). (This is the exact `format_gir`
   case the `&`-param brief-review pass-4 cited as the proof of the bypass.)

⚠ **Self-host multi-copy rule:** `parser.gg`/`resolve.gg`/`ast.gg` exist as INDEPENDENT copies across
`self_host_{lexer,parser,resolver,typechecker,lowerer,check}/` (some symlinked — `md5sum` to tell). A fix to
a shared primitive must land in EVERY copy that has the defect, or that dir's `gg check`/comparison stays
red. Grep each defect across all 6 dirs; fix every real occurrence.

## 3. THEN remove the truncate + un-ignore the tests (LAST)
- Delete/neuter the `errors.truncate(error_count)` + the `hard_errors` re-append at `typecheck.rs:6143-6151`
  (keep the recursion). Read the surrounding code to remove it cleanly (the `error_count` snapshot may also
  become dead — remove it too).
- Remove `#[ignore]` from `imported_nonexhaustive_match_should_error` + `imported_body_type_error_should_error`
  (`tests/integration.rs` ~`:1895`). They should now PASS (the `#[ignore]` comments' root-cause description
  was already corrected to cite the truncate).

## 4. Gates (ALL must hold — this chain is "green" only when the whole self-host suite re-greens)
1. `cargo build` clean; `cargo test --lib` green (recon: stays 1065/0 — confirm).
2. The 2 un-ignored tests PASS: `imported_nonexhaustive_match_should_error`, `imported_body_type_error_should_error`.
3. **`gg check` is now CLEAN on the self-host drivers** — `cargo run -- check tests/fixtures/self_host_lowerer/driver.gg` (and the parser/resolver/typechecker/check dirs) → no semantic errors. (This is the burn-down's success signal.)
4. **The self-host comparison + bootstrap suite re-greens** (force-rebuild the driver first): `c_emit_comparison` (expect ≥**850**, may RISE as the 24 httpserver fixtures + others now type-check — record the number), `lowerer_comparison`, `resolver_comparison`, `parser_comparison`, `type_comparison`, `check_comparison`, `self_host_bootstrap`, `self_host_bootstrap_fixed_point` — ALL must be GREEN/at-or-above baseline. ⚠ A RED here means a self-host defect you missed — fix it, don't exclude it.
5. The 24 httpserver + 2 yaml + jsonpath fixtures build + run correctly (the stdlib fixes).

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
