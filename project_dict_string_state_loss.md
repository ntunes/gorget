# Dict[String, _] state-loss investigation

> **STATUS: CLOSED 2026-05-01 (Session 2)** — DoD met.
>
> **Definition of done (from user):**
> - ✅ Bug fixed (codegen, not C runtime as TODO hypothesized).
> - ✅ One of six lowerer Vector workarounds reverted (`loaded` Dict).
> - ✅ `cli_basic.gg` and `encoding_basic.gg` pass `check_comparison`.
>
> **Two distinct bugs both fixed:**
> 1. Span collision in `resolution_map` — fixed by `parse_source_with_offset`
>    plumbing (commit `ed348009`).
> 2. `__gorget_map_new_sized_` codegen ignored key type — fixed by routing
>    K=Str/GorgetString to `gorget_map_new_str` (commit `6e76b13c`).
>
> Five other Vector workarounds are likely redundant now; deferred to
> follow-up.

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

### Session 2 — 2026-05-01 — Dict[String, bool] state-loss closed

**Plan:** re-revert `loaded` Dict in `self_host_lowerer/loader.gg`, add
debug prints, run on small input to see if put-then-contains lies.

**Diagnostic prints in lowerer's loader.gg** showed: with the Dict revert
AND span-collision fix in place, the driver runs `driver.gg` to completion
on its own — 27 puts, 167 contains=true (correct), no runaway re-loading.
So the `loaded.contains` worked correctly when the prints were on.

**But removing the prints and running the bootstrap test** (`stage-1 → stage-2.c`):
stage-1 timed out at 300s. The driver hung when run on driver.gg without
prints.

That's strange — adding prints "fixed" the bug? That hinted at print I/O
flushing affecting timing... but also at something more fundamental being
broken when the driver emits its OUTPUT C.

**Inspected stage-1's emitted C** (`stage1.c`) and found the smoking gun:
- 34 calls to `gorget_map_new(sizeof(Str), sizeof(...))` ← BROKEN
- 0 calls to `gorget_map_new_str(sizeof(...))` ← MISSING

The Rust-emitted C (stage-0 driver) had 42 `gorget_map_new_str` calls.
But the self-host's emitted C had ZERO.

**Root cause located:** `lir_codegen.gg:3515` `__gorget_map_new_sized_`
branch ALWAYS expanded to `gorget_map_new(sizeof(K), sizeof(V))` without
checking K type. Empty-literal `Dict[String, _] x = {}` lowered through
this magic-name path, bypassing the explicit `Dict__GorgetString__V__new`
routing in `map_monomorphized_to_runtime` (which DID check K and route
to `gorget_dict_new_str`).

Without `_str`, `hash_fn = NULL`, runtime falls back to byte-FNV on the
32-byte `Str` struct (data ptr + cap + len + alloc). Different `String`
instances of "std.collections" had different `data` pointers, hashed to
different buckets, contains=false after put.

**Fix:** in the `__gorget_map_new_sized_` codegen branch, check resolved
K c-type name; if `Str` or `GorgetString`, emit `gorget_map_new_str(sizeof(V))`.
Single change in `lir_codegen.gg` (12 lines added).

**Validation after fix:**
- stage1.c: 0 raw `gorget_map_new(sizeof(Str), ...)` (was 34), 35
  `gorget_map_new_str(sizeof(...))` calls.
- `self_host_bootstrap` AND `self_host_bootstrap_fixed_point`: BOTH PASS.
- Reverted `Vector[String] loaded` to `Dict[String, bool] loaded` in
  `self_host_lowerer/loader.gg` as proof-of-fix; removed dead
  `loaded_contains` helper. Stage-1 still works.
- `check_comparison`: 904 exact, 11 mismatches, 0 crashes (unchanged).
- Full integration sweep: **1052/1052 PASS, 0 failures**.

**Stage-1 mono-gen hang at mi=64/73 was the same bug.** With the codegen
emitting raw `gorget_map_new` (no hash), the various Dict[String, _]
sites in lower.gg / gir.gg / lir_lower.gg also failed to dedup, causing
infinite-progress loops. The fix at the codegen layer cascades to all
of them.

**Remaining workarounds (now likely redundant):** five other
parallel-Vector workarounds in self_host_lowerer (LowerCtx.named_locals,
GirModule.const_decls / none_decls / enum_names, seen_instances,
fn_templates) faced the same bug. Reverting them is a follow-up commit
for code cleanliness; the stage-1 binary now passes bootstrap with the
codegen fix even with these workarounds in place.

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
