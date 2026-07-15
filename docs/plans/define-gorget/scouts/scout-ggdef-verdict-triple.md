# Scout Report — ggdef verdict-triple rendering + E_-code conformance + exit-code scheme (4-lane)

Agent `ac76efc55a37bf9e7`, 2026-07-15 (two rounds: ggdef half, then the production-lane extension).
Combined prototype: `scouts/patches/ggdef-vt-4lane-proto.patch` (== `/tmp/ggvt_combined.patch`).
**Applies CLEAN to main HEAD `b180b9d2`; 11 files, +1183/−69.** Supersedes the engine-only
`ggdef-elaborate-move-proto.patch` (this patch INCLUDES it as its baseline).

## VERDICT: FEASIBLE, MEASURED, 3-of-4 lanes fully green — with ONE tracked self-host gap (split off HIGH)
The ggdef verdict-triple (exit + stderr render + E_-code conformance) and the production C/LLVM reject
adjudication are prototype-and-measured green end-to-end. The self-host lane rejects CORRECTLY but does not
render the `error[E_<code>]` marker → held at floor 195, surfaced (not papered over), fixed by a separate
committed HIGH track (bootstrap-gated). This report grounds the executor brief `wave-ggdef-elab-eval-brief.md`.

## PIECE A — exit-code scheme (Option A, ratified `decisions.md`): 0/1/2/101/103
ggdef reconcile MEASURED via `ggdef run <f>; echo $?`:

| input | exit | note |
|---|---|---|
| use-after-move reject | **1** | was 102 |
| double-move reject | **1** | |
| move-in-loop reject | **1** | |
| parse-error input | **1** | was 2 (`FrontendError::Parse`) |
| elaborate-error input | **1** | was 2 (`FrontendError::Elaborate`) |
| genuine usage (no file / unreadable path) | **2** | kept `EXIT_USAGE` |
| trap (`1/0`) | **101** | |
| Value (`print(42)`) | **0** | |

`EXIT_ILLFORMED 102→1` (`eval.rs`), `FrontendError::{Parse,Elaborate} 2→1` (`main.rs`), `EXIT_FUEL=103`
unchanged + re-doc'd ggdef-only, header taxonomy rewritten. `102` retired (nothing emits it; the one
frontmatter test referencing 102 repointed to 103).

## PIECE B — stderr rendering (pin 1): `error[E_Code]: <msg> at file:line:col`, empty stdout
Each measured with `stdout_bytes=0` (the program never ran):
```
error[E_UseAfterMove]: use of moved value `x` at /tmp/ggvt_fx/uam.gg:6:5
error[E_DoubleMove]: `x` moved more than once (double move) at /tmp/ggvt_fx/dm.gg:6:5
error[E_MoveInLoop]: cannot move `x` out of an enclosing scope inside a loop at /tmp/ggvt_fx/mil.gg:7:9
```
Statement-granular locations, correct (e.g. `dm.gg:6:5` is the SECOND `sink(!x)`). Rendered via the SAME
`gorget::span::offset_to_location` machinery as the existing `Outcome::Trap` arm (`main.rs`); a missing span
renders without the ` at …` suffix (matches the trap arm), never a bogus location. Old span-less
`ggdef: ill-formed: {m}` render RETIRED.

## PIECE C — E_-code conformance comparison (pin 3): a WRONG code FAILS conformance
- `ggdef -- gen` records the **CODE** (`#   reject: E_UseAfterMove`), not the message — no `gen` change needed.
- Correct code → ggdef conformance MATCH: `total=196 · MATCH=196 · MISMATCH=0`.
- Corrupt the expected code (`E_UseAfterMove → E_DoubleMove`) → conformance **FAILS**: `total=196 · MATCH=195 ·
  MISMATCH=1`, the mismatch precisely on the reject axis:
  `reject Some("E_UseAfterMove") vs expect Some("E_DoubleMove")` — exit/trap/stdout all still match, the E_
  code is the SOLE distinguishing axis. Restored → green. **This is the whole point: the guard is executable.**

## Structured E_-code design (resolve-once typed metadata, NO prose re-parsing — layering rule 2/4)
- `liveness.rs`: new `enum MoveErrorKind { UseAfterMove, DoubleMove, MoveInLoop }` with `.code() -> &'static
  str` (mirrors `TrapKind::code` — exhaustive match = registry ratchet). `struct LivenessError { kind, message,
  span }`; `check_liveness -> Result<(), LivenessError>`. The **code is produced at the violation site**
  (`set_err(kind, msg)` in `check_use`/`check_move`); the **span is the current statement span** (`cur_span`
  stamped at `check_stmt` entry — the device eval uses for trap provenance).
- `eval.rs`: `Run` gains `reject_code: Option<&'static str>` + `illformed_span: Option<Span>` — carried on
  `Run`, **NOT `Outcome`** (mirrors `trap_span`), so conformance IDENTITY excludes span. `Outcome::IllFormed(
  String)` UNCHANGED (message only) → zero ripple to the many eval-internal IllFormed sites/tests.
- Threading: gate → `Run.reject_code` → (a) `main.rs` render `error[{code}]: {msg}{loc}`, (b) `lib.rs
  render_expect_block` `#   reject:` line, (c) `frontmatter.rs` parse → `Expect.reject`, (d)
  `spec_conformance_ggdef.rs` compares `got_reject == expect.reject`, keyed on `Outcome::IllFormed` kind + the
  typed code. NEVER re-parsed from the message.

## PRODUCTION-lane reject adjudication (`tests/spec_conformance.rs`) — C/LLVM green, self-host tracked-gap
The production harness already `use ggdef::{parse_frontmatter, Expect}`, so `Expect.reject` parses for free.
- `extract_reject_code(stderr) -> Option<String>`: first `error[` marker → read to `]`, guard `E_` prefix.
  CODE compared only (pin 3; robust to ANSI colour — the `error[E_UseAfterMove]` token is contiguous, colour
  outside the brackets, verified on real bytes). The exact analogue of the `trap[…]` arm ignoring ` at loc`.
- `adjudicate` reject arm: `expect.reject.is_some()` → a build-stage failure whose stderr carries the matching
  code = **Match**; wrong/absent code = **Mismatch** (code axis); built-and-ran = **Mismatch** (should have
  been rejected at check). A NON-reject fixture keeps the prior contract byte-for-byte (plain build failure
  still = BuildFail).
- **I verified independently:** production `gg check` AND `gg build` on a use-after-move program emit
  `error[E_UseAfterMove]: use of moved value \`x\`` at exit 1 — the reject is at the semantic/check stage
  (backend-independent), so C and LLVM reject identically.

**Per-lane reject evidence (via the actual harness), floors C=196 / LLVM=196 / self-host=195:**
```
spec_conformance_c        MATCH    reject_use_after_move.gg   total=196 · MATCH=196 · MISMATCH=0
spec_conformance_llvm     MATCH    reject_use_after_move.gg   total=196 · MATCH=196 · MISMATCH=0
spec_conformance_selfhost MISMATCH reject_use_after_move.gg   total=196 · MATCH=195 · MISMATCH=1
```
Wrong code (→ `E_DoubleMove`): C and LLVM each `MISMATCH (want error[E_DoubleMove] · got error[E_UseAfterMove])`
→ MATCH 195 < floor 196 → FAILED (the code axis is live on the production lanes too). Restored → green.

## ⚠ CRITICAL FINDING → split off HIGH (owner-confirmed 2026-07-15): self-host reject-diagnostic-rendering gap
The self-host driver **rejects correctly** (exit 1, empty stdout, right semantic verdict) but its headline is a
bare `error:`, not `error[E_<code>]:`. Exact self-host stderr (ESC-stripped):
```
error: use of `x` after it was moved
   ┌─ …/reject_use_after_move.gg:25:11
```
- **Root cause = rendering-only (typed metadata IS present).** `diagnostic.gg:293` builds the headline as
  `severity_str(sev) + ": " + message` (no `[<code>]` segment); `diag_kind_str` (`diagnostic.gg:123`) maps
  `DkUseAfterMove → "use-after-move"` (hyphenated display), not the registry `E_UseAfterMove`. The self-host
  already carries the structured `DiagKind` (`DkUseAfterMove`/`DkMoveInLoop`/`DkMoveWithoutOperator`/…) — pure
  render-alignment gap, NOT a missing analysis, NOT a safety/semantic defect (the language rejects on all four
  lanes).
- **Fix (own HIGH track, bootstrap-gated — the self-host is compiled by the bootstrap):** add a `DiagKind →
  E_<code>` map + change the `diagnostic.gg:293` headline to `severity_str[code]: message` for error-severity
  diagnostics carrying a registry code (design decision: codeless kinds still render bare `error:`). Broad
  blast radius (reformats every coded self-host error diagnostic). Then raise `SELFHOST_MATCH_FLOOR` 195→196 =
  four-lane green. This is the ONE thing between three-lane affirmation and four-lane-green.
- **Held honestly this landing:** `SELFHOST_MATCH_FLOOR` = 195 (one below `MIN_FIXTURES=196`), surfaced as a
  MISMATCH in the table + documented at the source; the suite is green (195 ≥ 195). NOT a silent residual.

## Production floors — regenerated to OBSERVED (in-patch)
`C_MATCH_FLOOR 195→196` · `LLVM_MATCH_FLOOR 195→196` · `SELFHOST_MATCH_FLOOR 195` (held, the gap) ·
`MIN_FIXTURES 195→196` · ggdef `GGDEF_MATCH_FLOOR 195→196`. Stale module-doc "all three production floors
equal" corrected. The ggdef floor and the production floors move independently.

## Gates (foreground, measured)
- `cargo test -p ggdef`: **130 lib** (127 baseline + 3 reject-biconditional tests) + conformance 196/196 + gen
  idempotent + frontmatter seeds + coverage_histogram — green, no warnings.
- `cargo test --test spec_conformance`: **3 passed** at the new floors (C 196/196, LLVM 196/196, self-host
  195/196 w/ the documented reject MISMATCH). No warnings.

## Design forks / follow-ups for the brief
1. **Self-host reject-diagnostic-rendering track — HIGH, split off (owner-confirmed).** See CRITICAL FINDING.
2. Reject fixture lives in `spectests/run/` (`mode: run`), NOT a `static-error/` sub-lane — rationale: `verdict
   = elaborate ∘ eval` is ONE `run_source` call, so an `IllFormed` reject is a run *outcome*; `gen` + the
   existing lane compare it with no new sub-lane. Decide the eventual home for the FULL migration.
3. `reject: ⟺ exit==1` biconditional (symmetric with `trap ⟺ 101`); sound because `Outcome::Value` is always
   exit 0. The Value-exit-1-vs-reject-exit-1 concern is handled by keying comparison on outcome KIND +
   reject_code, not exit alone.
4. Codeless-IllFormed edge: `gen` on an internal-malformed program (no `main`) emits exit 1 + empty stdout +
   NO `reject:` line → `parse_frontmatter` rejects that block (`RejectExitWithoutCode`); only affects programs
   that are never valid conformance fixtures (migrate's `NonzeroExit` tripwire also blocks them). Render falls
   back to `error: no \`main\` function` (no bracket). If EVERY IllFormed must carry a code, the eval-internal
   cases (no-main / `Halt::IllFormed` / propagate) need codes too — out of scope here (fileable LOW).
5. Frontend (parse/elaborate) errors now exit 1 but KEEP the `ggdef: <msg>` render (they have no E_ codes in
   ggdef yet). Upgrading them to the `error[E_code]:` family is a larger separate task — fileable if pin-1's
   "best-rendered" bar is later read to demand it.
6. Field named `reject:` (frontmatter) → renders `error[E_x]:` (stderr); asymmetry with `trap:`→`trap[T_x]:`
   chosen for unambiguity (static rejection vs runtime trap). Executor may prefer `error:` for symmetry — trivial.
