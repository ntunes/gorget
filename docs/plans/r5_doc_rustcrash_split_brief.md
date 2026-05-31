# Executor Brief — R5 DOC: split the `c_emit_comparison` "Rust crashes" mislabel

**Status:** DRAFT — under fresh-review discipline before launch. Scout-verified.
**Risk:** LOW (a diagnostic-label fix in ONE test; counts unchanged). **Files:** `tests/integration.rs` ONLY.
**Branch from the post-imported-check base** (integration.rs was touched by the imported-check un-ignore at
~`:1907`; this DOC edit is at ~`:13615` — line-disjoint, but same file).

## 0. Worktree discipline
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch `/workspace/gorget-1`.
`git add tests/integration.rs` only. `cargo build` + `cargo test --test integration c_emit_comparison
--nocapture` (the only affected test). `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=120`.

## 1. The mislabel (scout-ground-truthed)
`c_emit_comparison` (`tests/integration.rs`) buckets ANY non-zero Rust-gg exit as `Outcome::RustCrash` and
prints "Rust crashes: 92". But those 92 are clean REJECTIONS of error-test fixtures (Rust gg correctly exits
non-zero WITH a diagnostic) — NOT crashes. Ground-truth: of the fixtures, **92 are clean rejections (exit
1-127), 0 are true crashes (signal-terminated)**. The "Rust crashes: 92" label is alarming and wrong.
⚠ This is `c_emit_comparison`-ONLY (verified: `RustCrash` appears nowhere else in integration.rs; the other
comparison tests run Rust IN-PROCESS or already bucket rust=0 errors as `ErrorOnly`). ⚠ NOTE: `run_with_timeout`
PANICS on a real timeout, so a timeout crashes the whole test, never reaching this bucket — the bucketing
condition is purely: clean non-zero exit (`status.code().is_some()`) = rejection; signal-terminated
(`status.code().is_none()` on Unix → SIGSEGV etc.) = true crash.

## 2. The fix (find the CURRENT lines — Chain 2 + imported-check shifted integration.rs; grep `RustCrash`)
The scout's cites (pre-shift): enum variant `~:13615`, return `~:13629`, count `~:13670`, print `~:13681`,
denominator `~:13686`. RE-FIND them by `grep -n 'RustCrash\|rust_crashes\|Rust crashes' tests/integration.rs`.
1. Add an `Outcome::RustRejected` variant alongside `RustCrash` (the enum).
2. At the return site (currently `if !rust_out.status.success() { return Outcome::RustCrash; }`), split:
   `if !rust_out.status.success() { return if rust_out.status.code().is_some() { Outcome::RustRejected }
   else { Outcome::RustCrash }; }`.
3. Add a `rust_rejected` counter beside `rust_crashes`; bucket `Outcome::RustRejected => rust_rejected += 1`.
4. Update the summary print to show BOTH separately, e.g.
   `"Rust rejected (error fixtures): {rust_rejected}, Rust crashes: {rust_crashes}"`.
5. ⚠ Keep the match-rate DENOMINATOR correct: wherever it currently subtracts `rust_crashes` from the total
   (the "excl. Rust crashes" rate), subtract BOTH `rust_rejected + rust_crashes` so the processable count is
   unchanged (the 92 just move buckets — the match-rate must NOT shift).

## 3. Gates
- `cargo build` clean.
- `cargo test --test integration c_emit_comparison --nocapture`: the **matched count is UNCHANGED (850)**,
  the **mismatched count UNCHANGED**, the match-rate UNCHANGED — only the label changes to
  "Rust rejected (error fixtures): 92, Rust crashes: 0". (The 92 move from the crash bucket to the rejected
  bucket; nothing else moves.)
- Sanity: `cargo test --lib` still green (you only touched integration.rs, so this is unaffected — skip if
  you prefer).

## 4. Report back
The diff + commit; the before/after summary-print lines (proving "Rust crashes: 92" → "Rust rejected: 92,
Rust crashes: 0"); confirmation the matched/mismatched/match-rate numbers are UNCHANGED. Confirm you touched
only `integration.rs` (only the `c_emit_comparison` test region) and never `/workspace/gorget-1`.

## 5. Don't-dodge
If ANY fixture buckets as a TRUE crash (`status.code().is_none()`), that's a real Rust gg crash — do NOT
hide it; report it (the scout measured 0, but verify). The split must be honest: rejection = clean non-zero
exit; crash = signal/no-exit-code.
