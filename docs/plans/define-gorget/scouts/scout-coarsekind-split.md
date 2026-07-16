# Scout Report — split the self-host coarse diagnostic kinds `DkTypeMismatch` + `DkControlFlow` 1:1 with the registry

Scout on branch `gorget-1`. Prototype `/tmp/recover_coarsekind_proto.patch` (3 files, +89/-31, applies CLEAN).

## VERDICT: the SPLIT is FEASIBLE, sound, and PROVEN end-to-end (all 12 codes) — but the TODO's headline PREMISE is WRONG

The self-host now emits `error[E_<code>]:` for **all 12** former-coarse reject kinds (measured on the real
committed reject fixtures, not source-read). **BUT** the TODO's stated payoff — "then those reject families
migrate to **four-lane** conformance spectests too" — is **NOT achievable**: ggdef's `reject_code` axis is
**may-move-ONLY**, so ggdef never renders a `reject: E_X` verdict for any type/control-flow error. The split is
still worth landing (it aligns the self-host with production + lets the self-host DRIVER reject tests assert the
CODE, the strongest signal ggdef's surface allows), but it is **floor-neutral**, adds **no** `spectests/run/`
fixtures, and the "four-lane migration" must be dropped from scope and re-filed as a ggdef-elaborate extension.

---

## 1. Verified emit-site table (site → production code → proposed DiagKind) — all against CURRENT source

**Single physical source of truth:** `tests/fixtures/self_host_typechecker/{typecheck.gg,diagnostic.gg}`.
`diagnostic.gg` is symlinked into `self_host_lowerer/`; `typecheck.gg` is symlinked into `self_host_check/`
(and `self_host_check` has NO `diagnostic.gg` — its symlinked `typecheck.gg` resolves the `diagnostic` import
against the real typechecker dir). The `self_host_parser/`, `self_host_resolver/`, `self_host_lexer/` copies are
**independent, minimal enums** that DO carry `DkTypeMismatch`/`DkControlFlow` but never EMIT them and have no
`diag_kind_code`/`render_diagnostic` — **they need no change** (verified: full build + all reject/accept/comparison
tests green without touching them).

All 13 emit sites live in `self_host_typechecker/typecheck.gg`. Message text matches the production `Display`
impl **exactly** (confirmed against `src/semantic/errors.rs`).

### DkTypeMismatch — 5 emit sites (NOT 6; see correction ①)
| typecheck.gg site | production code (`errors.rs`) | proposed DiagKind |
|---|---|---|
| `:452` `??` non-Option/Result LHS | `E_DefaultOpNonOptional` (`errors.rs:714`) | `DkDefaultOpNonOptional` |
| `:479` `*` on non-`Box[T]` | `E_DerefNonBox` (`:713`) | `DkDerefNonBox` |
| `:1843` `main()` throws non-int | `E_MainThrowsNonInt` (`:732`) | `DkMainThrowsNonInt` |
| `:2686` value out of range | `E_ValueOutOfRange` (`:775`) | `DkValueOutOfRange` |
| `:2714` string index-assign | `E_StringIndexAssign` (`:757`) | `DkStringIndexAssign` |

### DkControlFlow — 8 emit sites → 7 codes (PositionalAfterNamed twice)
| typecheck.gg site | production code | proposed DiagKind |
|---|---|---|
| `:385` required param after defaulted | `E_RequiredAfterDefault` (`:792`) | `DkRequiredAfterDefault` |
| `:2184` positional after named (free-fn) | `E_PositionalAfterNamed` (`:762`) | `DkPositionalAfterNamed` |
| `:2205` positional after named (method) | `E_PositionalAfterNamed` (`:762`) | `DkPositionalAfterNamed` |
| `:2288` awaited twice | `E_DoubleAwait` (`:786`) | `DkDoubleAwait` |
| `:2892` return outside function | `E_ReturnOutsideFunction` (`:728`) | `DkReturnOutsideFunction` |
| `:2902` break outside loop | `E_BreakOutsideLoop` (`:726`) | `DkBreakOutsideLoop` |
| `:2911` continue outside loop | `E_ContinueOutsideLoop` (`:727`) | `DkContinueOutsideLoop` |
| `:2918` throw in non-throwing fn | `E_ThrowInNonThrowingFunction` (`:729`) | `DkThrowInNonThrowingFunction` |

## 2. Corrections to the filed TODO entry (do NOT trust the filed lists)

① **DkTypeMismatch has only 5 self-host emit sites, not 6.** The TODO lists `E_TypeMismatch` among its codes,
   but the self-host **never emits a plain type-mismatch** (it is a partial typechecker — no expected-vs-actual
   comparison reject exists). Design consequence: **keep `DkTypeMismatch` and map it 1:1 to `E_TypeMismatch`**
   (a real registry code; the enum's own comment "expected vs actual type disagreement" = its namesake), a
   reserved-but-now-CODED slot ready for a future site — mirrors the existing reserved `DkUnreachable`. The 5
   real sites split off into the precise kinds above.

② **Three of the TODO's DkControlFlow code names are wrong** (shorthand that doesn't match the registry):
   - `E_Break` → **`E_BreakOutsideLoop`**
   - `E_Continue` → **`E_ContinueOutsideLoop`**
   - `E_ReturnOutsideLoop` → **`E_ReturnOutsideFunction`** (wrong on the noun too — it is *Function*, not *Loop*).

③ **`DkControlFlow` has NO residual registry code** (there is no `E_ControlFlow`), so after the split it is
   fully dead → **remove it** (unlike `DkTypeMismatch`, which stays). Prototype removes it cleanly.

④ **`~13 emit sites` is exact: 13** (5 + 8).

## 3. Proven prototype result (MEASURED — built the self-host driver, ran the real reject fixtures)

Prototype = the full split: +12 DiagKinds, `DkControlFlow` removed, `DkTypeMismatch`→`E_TypeMismatch`, all 13
sites repointed, `diag_kind_str` + `diag_kind_code` extended, `typecheck.gg`/`infer.gg` imports fixed. The
self-host `self_host_lowerer/driver` **built clean** (`GG_BUILD_TIMEOUT_SECS=600`, only pre-existing warnings).

**BEFORE (baseline):** these kinds returned `""` from `diag_kind_code` → bare `error:` headline.
**AFTER (driver run on the 14 committed reject fixtures, ANSI-stripped):** every one now prints the correct
contiguous `error[E_<code>]` (the `extract_reject_code` `find("error[")` reads it — raw-byte-verified), exit 1,
box rule present, stdout empty:

```
deref_non_box_rejected               error[E_DerefNonBox]           break_outside_loop_error    error[E_BreakOutsideLoop]
default_op_non_optional_rejected     error[E_DefaultOpNonOptional]  continue_outside_loop_error error[E_ContinueOutsideLoop]
required_after_default_error         error[E_RequiredAfterDefault]  throw_in_non_throwing_error error[E_ThrowInNonThrowingFunction]
value_out_of_range_error             error[E_ValueOutOfRange]       positional_after_named(+method) error[E_PositionalAfterNamed]
string_index_assign(+compound)       error[E_StringIndexAssign]     main_throws_non_int_error   error[E_MainThrowsNonInt]
```

## 4. Blast radius — MEASURED ZERO

- **No test asserts on `diag_kind_str` values** (`"type-mismatch"`/`"control-flow"`): grep of `tests/` finds
  only Rust fn-names and PRODUCTION `check_gg_fails` message substrings — nothing reads the self-host DIAG kind
  string. The `format_diagnostic` "DIAG" line is stderr-routed; comparison reads stdout.
- **All existing driver reject tests assert `stderr.contains("error")` + message + box rule** — `error[E_X]:`
  still contains `"error"` and the unchanged message, so they PASS untouched.
- **Authoritative test runs (release, fresh driver rebuild):**
  - `self_host_driver_rejects_*` → **14 passed / 0** (incl. all coarse-kind rejects).
  - `self_host_driver_accepts_*` → **3 passed / 0** (no over-rejection).
  - `type_comparison` → **ok** (the `diag_kind_str` change does not perturb the comparison harness).
- **Conformance floors are UNCHANGED** (C/LLVM/SELFHOST/MIN_FIXTURES = **202**, `GGDEF_MATCH_FLOOR` = **202**):
  the split adds NO `spectests/run/` fixture. (Regenerating not needed — floor-neutral by construction.)
- **Bootstrap:** the self-host source (typecheck.gg/diagnostic.gg) contains no coarse-kind errors, so it
  self-compiles clean; the driver built + ran, proving the new enum/match compiles. The byte-identical
  `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) is the **executor/parent** gate (bootstrap-gated).

## 5. THE BLOCKING FINDING — four-lane migration is IMPOSSIBLE; ggdef reject-codes are may-move-ONLY

The reject-diagnostic landing scoped ggdef-elaborate to the **may-move axis only** (`spec/ggdef/src/lib.rs:148`
sets `reject_code` = `MoveErrorKind::code()` iff `Outcome::IllFormed`; ggdef's frontend is lexer+parser+AST +
its own may-move `elaborate`, NOT production semantic analysis). I ran **`ggdef run` on every real coarse-kind
reject fixture** — none yields a `reject: E_X` verdict; they fall into three buckets:

| ggdef behavior | codes | four-lane? | three-lane (C/LLVM/self-host via GGDEF-SKIP)? |
|---|---|---|---|
| **SKIP** — `FrontendError` (parse/elab "outside phase-0 subset") | E_DerefNonBox, E_DefaultOpNonOptional, E_PositionalAfterNamed, E_ThrowInNonThrowingFunction, (trait) E_RequiredAfterDefault | ❌ (no ggdef code) | possible, but `ggdef gen` can't fill the `reject:` code → must hand-author expect (violates "expectations flow FROM the definition") |
| **eval-IllFormed, codeless** — `Ok(Run{IllFormed})`, `reject_code=None`, exit 1 | E_StringIndexAssign, E_BreakOutsideLoop, E_ContinueOutsideLoop | ❌ | committing `reject: E_X` → **fatal ggdef MISMATCH** (got None); committing codeless → no code axis compared (defeats purpose) |
| **ACCEPT** — exit 0, evals to a Value | E_ValueOutOfRange, E_RequiredAfterDefault, E_MainThrowsNonInt, (likely E_ReturnOutsideFunction) | ❌ | ❌ — ggdef **under-rejects** (see NEW BUG below); any lane MISMATCHes |

`ggdef gen` on a `break` program produced `exit: 1 / stdout: "" ` with **NO `reject:` line** — direct proof the
code cannot be recorded. **Conclusion: the "migrate to four-lane conformance spectests" clause of the TODO is
not implementable without first extending ggdef-elaborate to model these static-rejection axes with reject
codes** — a much larger, separate track (the ggdef verdict landing deliberately shipped only the may-move axis).

## 6. NEW pre-existing bugs found en route (report, don't fix)

**(A) ggdef definition-integrity gap — the arbiter UNDER-REJECTS on non-move static axes.** `ggdef run` returns
**exit 0** (accepts + evaluates) for programs production + self-host correctly REJECT: `value_out_of_range_error.gg`,
`required_after_default_error.gg`, `main_throws_non_int_error.gg` (and `return`-outside-function). This is the
SAME over/under-rejecting-arbiter class the ggdef verdict-triple landing (`67ce92f8`) fixed for the may-move
axis — it silently corrupts any downstream track that trusts ggdef's acceptance oracle on these axes. Worth a
HIGH definition-integrity follow-up (extend ggdef-elaborate to reject integer-range / param-signature-order /
main-throws-non-int, mirroring how it now rejects may-move).

**(B) `infer.gg` dead imports.** `tests/fixtures/self_host_typechecker/infer.gg:24` imported `DkControlFlow` and
`DkTypeMismatch` but emits only `DkUndefinedName` — both were unused. Pre-existing dead-import smell; the
prototype cleans it (import list now just `Diagnostic, DkUndefinedName`).

## 7. Recommended executor plan (with gates)

**This is a SPLIT-ONLY track, floor-neutral. Drop the "four-lane migration" from scope.**

1. **Apply the split** (`/tmp/recover_coarsekind_proto.patch`, 3 files): `diagnostic.gg` (enum: +12 kinds,
   −`DkControlFlow`, `DkTypeMismatch`→`E_TypeMismatch`; `diag_kind_str` + `diag_kind_code` arms; header/comment
   rewrites), `typecheck.gg` (import + 13 repointed sites), `infer.gg` (dead-import cleanup). Design fork for
   brief review: **keep `DkTypeMismatch`→`E_TypeMismatch`** (reserved-but-coded, prototyped) vs remove it — I
   recommend KEEP (registry-faithful, mirrors `DkUnreachable`).
2. **Lock the codes at the driver level** — upgrade the **7** existing coarse-kind self-host driver reject tests
   (`self_host_driver_rejects_{default_op_non_optional,default_op_non_optional_nested,positional_after_named,
   positional_after_named_method,required_after_default,trait_required_after_default,value_out_of_range,
   string_index_assign,string_index_compound_assign}` + `rejects_invalid_program` = throw) to also assert
   `stderr.contains("error[E_<code>]")`. This is the strongest conformance signal ggdef's surface permits.
   Optionally ADD 4 new driver reject tests for the codes that today have only production `check_gg_fails`
   (`deref_non_box_rejected`, `main_throws_non_int_error`, `break_outside_loop_error`, `continue_outside_loop_error`
   — fixtures already exist) to lock all 12.
3. **Correct the record:** move the TODO entry to `DONE.md` as a SPLIT (not a four-lane migration); rewrite the
   `diag_kind_code` doc-comment (prototype already does). File follow-ups: (A) the ggdef under-rejection gap; and
   the ggdef-elaborate-extension prerequisite for any genuine four-lane migration of these kinds.
4. **Executor FOREGROUND gates:** `cargo test --test integration --release self_host_driver_rejects` +
   `self_host_driver_accepts` + `type_comparison` (all green) · `cargo test --lib`. **Parent/integrate gate:**
   full C+LLVM sweep + `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — bootstrap-gated
   (self-host source); serialize with any other self-host-source track.

## Artifacts
- Prototype diff: `/tmp/recover_coarsekind_proto.patch` (3 files, +89/-31; the full 12-code split, PROVEN).
- Baselines: `/tmp/baseline_{diagnostic,typecheck,infer}.gg`.
