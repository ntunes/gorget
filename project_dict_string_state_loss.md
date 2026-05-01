# Dict[String, _] state-loss investigation

> **Goal:** root-cause and fix the runtime / codegen bug behind TODO.md's
> "Self-host Dict[String, _] state-loss" item, then revert one of the six
> parallel-Vector workarounds as proof.

## Root cause (Session 1, 2026-05-01)

**The `Dict[String, _]` runtime is fine.** The bug isn't a runtime issue.
The TODO's hypothesis was incorrect.

**The actual bug** is span-collision across imported modules in the self-host
loader. Both `cli_basic.gg` and `encoding_basic.gg` failures are manifestations
of the same root cause.

### How it presents

Symptom from `tests/fixtures/encoding_basic.gg`: with the `# URL decode error`
comment present, `case Error(enc_msg):` fails to type the binding `enc_msg`.
Removing or shortening the comment makes the binding work. One byte flips it.

### How it actually fails

Tracing identifier resolution for `enc_r2` in `match enc_r2:`:

```
==WITH comment==
DBG resolve EIdent name='enc_r2' span_start=698 -> def_id=171
DBG resolve EIdent name='enc_ch' span_start=698 -> def_id=196   # <-- collision

==WITHOUT comment==
DBG resolve EIdent name='enc_r2' span_start=676 -> def_id=171
# (no enc_ch use lands at 676, no collision)
```

`enc_r2` is in `tests/fixtures/encoding_basic.gg` (entry file).
`enc_ch` is in `lib/std/encoding.gg` (imported file).

Both files have a use of their respective identifier at byte offset 698 in
their own source. The resolver writes both:

```
ctx.resolution_map.put(698, 171)   // enc_r2
ctx.resolution_map.put(698, 196)   // enc_ch — last write wins
```

Later, the typechecker's `infer_expr_type` for `enc_r2` reads
`resolution_map.get(698) = 196` (def for `enc_ch`, declared in another scope).
That def's `type_id` happens to be `12` (some primitive — `byte`), not the
expected `38` (`Result[String, ParseError]`). So `match enc_r2:` proceeds
with scrutinee_type=12; the `case Error(...)` arm hits the
`RTGeneric(Result, ...)` branch, doesn't find Result, falls through, and
`enc_msg` never gets typed.

### Why Rust doesn't have this bug

Rust's `loader::ModuleLoader` tracks `next_offset: usize` and parses each
non-entry module via `Parser::new_with_offset(source, base_offset)`. Each
module's spans are shifted by `base_offset` so the merged module has
globally-unique span values. The self-host loader (`self_host_check/loader.gg`,
`self_host_lowerer/loader.gg`) doesn't do this — every module is parsed at
offset 0, which guarantees collisions in any large multi-file project.

### Why this looked like a Dict[String, _] issue

It isn't. `resolution_map` is `Dict[int, int]` — integer-keyed, not
string-keyed. The TODO line 11 hypothesis ("variant_field_types ... due to
hash collision sensitivity to string-array layout") was wrong. The
comment-length sensitivity comes from byte offsets shifting when the source
length changes, not from string-hash perturbation.

The earlier sessions papered over this with parallel-Vector workarounds for
several `Dict[String, _]` sites in `self_host_lowerer/loader.gg`. Those
workarounds incidentally helped because they bypass any Dict at all — but
they don't address the underlying span-collision problem, just narrow the
window where it can fire.

## Fix plan

Mirror Rust: add `base_offset: int` to the self-host lexer and parser, and
have the loader assign cumulative offsets to each module's parser. Apply to
both `self_host_check/` and `self_host_lowerer/`.

Steps:

1. **Lexer**: every emitted `Span(start, end)` shifts to
   `Span(start + base_offset, end + base_offset)`.
   - `self.lex_pos` stays a 0-based source index for byte access.
   - When emitting a token, compute span as
     `Span(start + base_offset, end + base_offset)`.
2. **Parser**: passes `base_offset` to lexer at construction. All span
   arithmetic in the parser uses already-shifted span values from tokens.
3. **Loader** (`self_host_check/loader.gg` and `self_host_lowerer/loader.gg`):
   - Add `next_offset: int` to the loader state, init to 0.
   - Entry module: parse at offset 0; advance `next_offset = source.len() + 1`.
   - Subsequent modules: parse at `next_offset`, advance.
4. **Span-aware identifier-only**: the change is purely additive — span
   values get bigger but their semantics are identical.

Validation:
1. Unit test: parse two synthetic modules, confirm spans don't overlap.
2. `check_comparison`: `cli_basic.gg` and `encoding_basic.gg` should now
   pass.
3. Revert one of the six Vector-pair workarounds in
   `self_host_lowerer/loader.gg` (e.g. the `loaded` workaround) and confirm
   stage-1 still terminates. If it does, that's proof the underlying Dict
   bug was a phantom.

## Logbook

### Session 1 — 2026-05-01 — span-collision identified, fixed, partial DoD met

**Confirmed bug reproduces:** `cli_basic.gg` (missing `"r1" = Result[bool,
String]`) and `encoding_basic.gg` (missing `"enc_msg" = ParseError`) fail
`check_comparison`. Comment-toggle test confirmed: removing
`# URL decode error` from `encoding_basic.gg` makes `enc_msg` typed again.

**Diagnostic prints in self-host:** added `print()` calls at
`resolve_variant_field_types`, `SMatch`, `EIdentifier` infer, `lookup`,
`find_name_entry`, `EIdentifier` resolve. Found that `find_name_entry`
returns the right index, `lookup` returns the right def_id, but
`resolution_map` later returns a different def_id for the same
`expr.span.start`. Cross-file span collision.

**Root cause located:** `expr.span.start` is a file-local byte offset. With
the comment, `enc_r2`'s use lands at the same offset as `enc_ch`'s use in
`std/encoding.gg`. Resolution map last-write-wins.

**Rust pipeline confirmed correct:** `Parser::new_with_offset(source,
base_offset)` shifts spans by per-module base_offset, computed by
`ModuleLoader.next_offset`. Self-host loader does no offsetting.

**Fix implemented:**
- `tests/fixtures/self_host_typechecker/lexer.gg` — added `lex_base_offset`
  field to Lexer, modified `lex_emit` to add it to every emitted span,
  added `lex_tokenize_with_offset(source, offset)` public API.
- `tests/fixtures/self_host_typechecker/parser.gg` — added
  `parse_source_with_offset(source, offset)` that calls the lexer's
  offset variant.
- `tests/fixtures/self_host_check/loader.gg` — `load_all` takes
  `entry_source_len`, tracks `next_offset = entry_source_len + 1` for
  imports.
- `tests/fixtures/self_host_check/driver.gg` — passes
  `source.byte_len()` to `load_all`.
- `tests/fixtures/self_host_lowerer/loader.gg` — same pattern for
  `load_imports`.
- `tests/fixtures/self_host_lowerer/driver.gg` — same pattern.

**Validation results:**
- `check_comparison`: 902 exact → 904 exact (+2 fixtures: cli_basic and
  encoding_basic), 0 regressions, 13 → 11 mismatches.
- `parser_comparison`: 990 → 998 matched (+8).
- `resolver_comparison`: 572 → 575 matched (+3).
- `type_comparison`: 949 exact → 959 exact (+10).
- `self_host_bootstrap` and `self_host_bootstrap_fixed_point`: still pass.
- `lowerer_comparison`: 704/1005 matched (was lower previously).
- All comparisons report 0 crashes.

**DoD partial:** `cli_basic.gg` and `encoding_basic.gg` now pass
`check_comparison`. ✓

**DoD NOT met:** revert one Vector workaround. Attempted to revert
`Dict[String, bool] loaded` in `self_host_lowerer/loader.gg`. Bootstrap
TIMED OUT (300s deadline blown). So a real `Dict[String, _]` bug DOES
exist beyond the span-collision issue. Reverted the revert; bootstrap
restored.

**Distinct bug confirmed:** there are TWO independent bugs that were
both attributed to "Dict[String, _] state-loss":

1. **Span collision (FIXED in this session)** — manifested as
   comment-length-dependent identifier mis-resolution. Affected
   cli_basic.gg, encoding_basic.gg in `check_comparison`. Root cause:
   `Dict[int, int] resolution_map` keyed on file-local byte offsets that
   collide across imported modules.

2. **Real Dict[String, bool] state-loss (UNFIXED)** — manifests in
   `self_host_lowerer/loader.gg`'s `loaded` Dict, where
   `loaded.contains(module_path)` returns `false` after a successful
   `loaded.put(module_path, true)`. Causes runaway re-loading
   (`std.collections` loaded 286× per the historical TODO note).
   Minimal reproductions in this session DID NOT reproduce (5 distinct
   modules, then 25 with 10× repeat, then struct-field-keyed put — all
   work correctly at small scale). Likely requires the specific call
   pattern + scale of the actual lowerer driver to surface.

### Session 2 — TBD — Dict[String, bool] state-loss

The remaining bug. Plan:

1. Re-revert the `loaded` Dict in `self_host_lowerer/loader.gg`.
2. Add `print()` calls inside the `if not loaded.contains(...): loaded.put(...)` branch logging the key, current size, and a put-counter.
3. Build stage-0 driver, run on a tiny test fixture (not driver.gg)
   that imports a few modules. Look for cases where the same key gets
   `put` more than once.
4. If reproducible at small scale: extract the minimal failing pattern
   into a focused Rust-compiled fixture; that's the C-level repro.
5. If only reproducible at full driver.gg scale: instrument the runtime
   `gorget_map_put` / `gorget_map_get` (`__gorget_str_key_hash`) with
   a `GG_TRACE_DICT=1` env-gated logger that prints every key's hash,
   bucket index, found state. Diff between the working (Vector) and
   broken (Dict) runs to find the divergence.
6. Root-cause and fix at the appropriate layer. Likely candidates:
   - `__gorget_str_key_hash` reads beyond `len`? — checked; it doesn't.
   - `gorget_map_grow` mis-rehashes some key shape? — read the code,
     looks correct.
   - String key materialize fails for some specific pattern (cap=0
     vs cap>0 boundary)?
   - GorgetMap's `hash_fn` field gets clobbered by an unrelated write
     elsewhere (e.g. struct-field write that overshoots)?

## Files

- `tests/fixtures/self_host_check/lexer.gg` — needs `base_offset` field +
  span emission shift.
- `tests/fixtures/self_host_check/parser.gg` — pass `base_offset` to lexer.
- `tests/fixtures/self_host_check/loader.gg` — track `next_offset`, pass
  to each parsed module.
- `tests/fixtures/self_host_check/ast.gg` — Span struct unchanged.
- Mirror in `tests/fixtures/self_host_lowerer/{lexer,parser,loader}.gg`.
- (`self_host_typechecker/` doesn't have a loader — single-module test
  driver. Not affected by cross-file collision but worth fixing
  consistently if I touch lexer.)

## Reverted-workaround target (proof-of-fix)

`tests/fixtures/self_host_lowerer/loader.gg:82`
`Dict[String, String] call_redirects = {}` — easiest to revert and verify.
Documented in DONE.md commit `93f20493...a7b03b1c`: workaround #1 was
`loaded: Dict[String, bool] → Vector[String] + loaded_contains`.

## What this DOESN'T fix

- Stage-1 hang at mi=64/73 in mono-gen — likely a separate issue.
- Other items in the 13-mismatch check_comparison list (closure inference,
  generic param rendering, etc.) — different root causes.
- The actual `Dict[String, _]` runtime if there really were a runtime bug.
  Based on the runtime audit + minimal repros all working, there is NO
  runtime bug. The "state-loss" narrative was incorrect attribution.
