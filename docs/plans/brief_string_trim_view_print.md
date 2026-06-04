# Brief — String trim/strip + Str-view printing — +7, RUN-VERIFIED (TWO roots, both required)

A RUN-verify scout built + ran + measured this end-to-end: **385 → 392 (+7)**, ZERO regressions
(corpus-wide non-match-set diff confirmed). TWO coordinated roots — **fixing only Root 1 flips NOTHING**
because Root 2 masks it. The 7 flips: `string_strip`, `string_stdlib`, `test_str_predicates`,
`string_methods3`, `str_byte_slice`, `string_chained_methods`, `string_immutability`.

## Root 1 — no-arg strip dispatch (`lir_codegen.gg:~4044`)
The self-host lowers `.strip()`/`.lstrip()`/`.rstrip()` (NO-ARG forms) to `gorget_str_strip`/`_lstrip`/
`_rstrip` and pads the missing chars-set arg with an empty `((Str){0})`. `gorget_cp_in_str` over a 0-len set
is always false → strips NOTHING. **Fix:** rewrite the NO-ARG form to the whitespace variant —
`gorget_str_trim` / `gorget_str_lstrip_ws` / `gorget_str_rstrip_ws` — mirroring Rust
`src/backend/c_lir/emit_call_extern.rs:157-163`. Place the rewrite BEFORE `name` is read for the emitted
symbol. (The 1-arg forms `.strip(chars)` keep routing to the existing `gorget_str_strip` etc.)

## Root 2 — Str-VIEW printing over-reads (`lower.gg:4649/4656/6609/6611` + `lir_codegen.gg:4037`)
print/f-string format a `GorgetString` arg with `%s` + `.data` (NUL-terminated). But trim/strip/slice/
substring/removesuffix return non-NUL-terminated **cap=0 VIEWS** into the backing buffer, so `%s` over-reads
(`"hello"` prints as `"hello  "` — reads past the view into the untrimmed tail). **Fix:** emit `%.*s` +
`(int)len, data` for a Str arg (mirror Rust), instead of `%s` + `.data`.
⚠⚠ **THE POSITION-0 CARVE-OUT (SIGSEGV without it — the scout hit this):** the format-string argument
itself (arg index 0 of the printf-family emit) is in some emits a `gs_ty`/Str value and must NOT get the
`(int)len, data` decomposition — it's the format string, passed as `(const char*)data`. **Gate the `%.*s`
+ `(len,data)` decomposition on `ai != 0`** (only the value args, never the format-string slot).

## ⚠ SENSITIVITY (LOAD-BEARING — Root 2 touches the whole corpus)
Root 2 changes the emit for EVERY `print(str)` / f-string-string-interpolation in the corpus — the same
sensitive area as MEMORY's `7c43abcf` borrowed-String +42 win. A wrong `%.*s` decomposition (or a missed
carve-out) regresses MANY fixtures or SIGSEGVs. The scout measured ZERO corpus-wide regressions, but the
executor MUST re-confirm via the FULL lock-in net (`self_host_runtime` stays green, snapshot set grows by
EXACTLY the 7 flips, NO existing print/f-string snapshot changes) — not just the runtime_diff count.

## Reviewers verify (load-bearing)
1. **Root 1 scoped:** only the NO-ARG `.strip()`/`.lstrip()`/`.rstrip()` rewrite to the `_ws` variants;
   the 1-arg `.strip(chars)` forms are UNTOUCHED. Confirm the rewrite is placed before the symbol name is
   consumed and mirrors Rust `emit_call_extern.rs:157-163`.
2. **Root 2 carve-out:** the `%.*s` + `(int)len, data` decomposition fires for Str VALUE args (`ai != 0`)
   and NEVER for the position-0 format string (which stays `(const char*)data`). Confirm both: (a) a Str
   value arg now prints via `%.*s` (so a view prints its `len` bytes, not over-reading), and (b) the format
   string slot is untouched (no SIGSEGV). Confirm this matches Rust's printf emit.
3. **Both roots required:** Root 1 alone flips nothing (Root 2's over-read masks the now-correct trim);
   confirm the brief ships BOTH edits.
4. **No corpus-wide regression:** the change is in the hot print/f-string path. Reason about why a Str
   VALUE that is OWNED (NUL-terminated) still prints correctly under `%.*s` (it does — `len` is correct for
   both owned and view), and confirm non-Str args are unaffected.

## Gates (executor; force-rebuild driver; baseline 385)
- `self_host_runtime` lock-in **392/0** (7 new snapshots; NO existing snapshot changes — ESPECIALLY no
  existing print/f-string fixture regressed); `runtime_diff` 385→**392** (exactly the 7 flip, ZERO
  regressions).
- `lowerer_comparison` / `c_emit_comparison` — report (the strip-rename + printf-arg change may shift c_emit
  fn-count? unlikely — it's emit-shape, not new fns; CONFIRM 960/891 or explain).
- `bootstrap_fixed_point` GREEN (the driver prints extensively via this path → strong neutrality signal);
  `cargo test --lib` 1072/0.
- Stage ONLY `tests/fixtures/self_host_lowerer/lower.gg` + `tests/fixtures/self_host_lowerer/lir_codegen.gg`
  + the 7 new `runtime_snapshots/*.out`.

## Out of scope (log to TODO)
Residual cluster non-flips are SEPARATE roots: `string_methods2` (baseline CRASH — substring/char-subscript
view path), `test_string_methods` L38 (char/byte-at returns `0` instead of the char). NOT this fix.
