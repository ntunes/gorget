# T1 — Trap-normalization DEFINITION slice (executor brief)

> **Wave position:** first slice of the trap-normalization track (enforcement wave step 1,
> `HANDOVER.md`). T1 defines the trap FORMAT; T2a/T2b make production emit it; T3 (D23) is
> parallel. **T1 lands first** because expectations flow FROM ggdef (the definition), so the
> `T_` codes + the `trap:` conformance contract + the ggdef oracle must exist before any
> production side can be diffed against them.
>
> **Scope discipline:** this slice touches ONLY the definition zone —
> `spec/ggdef/*`, `spec/prose/*`, `spectests/*`, `tests/spec_conformance.rs`,
> `spec/ggdef/tests/spec_conformance_ggdef.rs`, and `docs/language-reference.md §10.9`.
> It does **NOT** touch `src/` (production emit = T2a/T2b) and does **NOT** touch the
> throws diagnostic / smith / reference §10.1 (D23 = T3). If you find yourself editing
> `src/backend/*`, `src/ir/*`, `src/semantic/*`, or reference §10.1 — STOP, that's another
> slice.

## The ratified target (D11 — do NOT relitigate)

Source: `docs/plans/define-gorget/decisions.md` (LOG 2026-07-06 "D11 RATIFIED IN FULL" +
the exit-101 entry) and `decision-batch-4-proposal.md` (the "D11 … TRAP NORMALIZATION"
section). A closed `TrapKind` registry names every trap class; `T_` codes DERIVE from
variant identity (the `E_` convention); uncaught trap → stderr `trap[T_X]: detail at
file:line:col` + exit **101**; §10.9 `Fault` is re-founded as the **catchable subset**;
no-drops-on-uncaught-trap is normative. Conformance compares the **T_ code + exit only**,
never the human detail, with the trailing ` at file:line:col` normalized out.

**The 8 ratified variants** (initial registry): `Overflow, DivByZero, Bounds, UnwrapNone,
UnwrapError, UnwrapErrorOnOk, AssertFailed, Panic`. **Catchable subset** (§10.9 Fault):
exactly `Overflow, DivByZero, Bounds`.

**Code naming is mechanical `T_<VariantName>`** — mirroring `E_<VariantName>`
(`spec/prose/diagnostic-codes.md` "Naming scheme"; `SemanticErrorKind::code()`,
`src/semantic/errors.rs:641`). So: `T_Overflow`, `T_DivByZero`, `T_Bounds`, `T_UnwrapNone`,
`T_UnwrapError`, `T_UnwrapErrorOnOk`, `T_AssertFailed`, `T_Panic`. The proposal's illustrative
`T_IndexOutOfBounds` is superseded — the ratified variant is `Bounds`, so the code is `T_Bounds`.

## Owner design rulings that constrain this slice (2026-07-10)

- **Q1 — conformance compares T_ code + exit 101 ONLY, never the human detail.** ggdef says
  "arithmetic overflow", production says "integer overflow" — they provably diverge and are
  allowed to. The language contract is `{T_ code, exit 101}`; the human detail is impl-defined.
- **Q3 — ggdef DUPLICATES the registry** (it will re-derive `TrapKind` in `spec/ggdef/`), pinned
  later by a parity lint on the T2a side. Do NOT try to import a production registry — the import
  ratchet forbids ggdef importing `src/ir`/`src/semantic` anyway (`tests/lints.rs`, standing-rule 5).
  In T1 you define ggdef's `TrapKind`; T2a defines `src/trap.rs` + the parity lint.
- **Q4 — assert keeps its detail + a real span** (single emit site). In ggdef, `assert(false)`
  → `Trap(AssertFailed)`.
- Bounds real-locations (owner: "thread locations now") is a **T2b** concern (production emit).
  T1 does not touch production emit; ggdef already Traps on bounds.

## Work items (each cites current source; verify before editing)

### W1 — ggdef `Fault` → `TrapKind` (the 8-variant closed registry)
`spec/ggdef/src/eval.rs:54` currently: `enum Fault { Overflow, DivByZero, Bounds, Panic(String) }`.
- Introduce the closed `TrapKind` (the definition's own copy) with the 8 variants above.
  Keep a detail payload where a variant needs one for the RENDERED line (e.g. `Panic(String)`
  for a user message; unwrap variants may carry the type/variant word) — **detail is for
  rendering only; it is never compared** (Q1), and `code()` derives from variant identity alone.
- Add `TrapKind::code(&self) -> &'static str` = an **exhaustive, catch-all-free** `match`
  returning `"T_<Variant>"` (mirror `SemanticErrorKind::code()`, `src/semantic/errors.rs:641`;
  no `_` arm, so rustc exhaustiveness is the ratchet). Keep `message()`/detail for the human line.
- Add `TrapKind::is_catchable(&self) -> bool` = `true` for `Overflow | DivByZero | Bounds` only.
  This is the §10.9 Fault subset. **It is a PURE registry accessor** — its only T1 consumers are
  the §10.9 prose subset (W5) and, later, the T2a parity lint. **ggdef models NO `catch`** (fault
  or contract — it's outside the phase-0 subset; a grep of `spec/ggdef/src/` finds zero
  `catch`/`Catch` handling). So do NOT go hunting a fault-catch path to thread this through — there
  isn't one. Just add the accessor.
- Thread the rename through: `Outcome::Trap(TrapKind)` (eval.rs:76), `exit_code()` (eval.rs:83,
  Trap → `EXIT_TRAP`=101 unchanged), `Halt::Trap(TrapKind)` (eval.rs:97) and its conversion
  (eval.rs:251), and every `Err(Halt::Trap(Fault::…))` construction site — the COMPLETE set (verified
  by grep) is eval.rs:648, 727, 736, 770, 1207, 1314, 1364, 1559, 1562, 1565, 1569, 1571, 1576,
  1578, 1609. Overflow/DivByZero/Bounds sites map 1:1.
- **SPLIT the unwrap arm by receiver (required — do not just rename it `Panic`).** The current
  `M::Unwrap` arm (eval.rs:1206-1208) has a SINGLE branch `variant == "None" || variant == "Error"`
  → `Fault::Panic(...)`. A blind `Fault::Panic`→`TrapKind::Panic` rename would give BOTH unwrap
  traps the code `T_Panic` and W6's `T_UnwrapNone`/`T_UnwrapError` fixtures would be
  un-generatable. Split it: receiver `None` → `TrapKind::UnwrapNone`, receiver `Error` →
  `TrapKind::UnwrapError` (keep the human detail string on each for rendering; the code derives from
  the variant). The `.unwrap_error()` method (W2) is the third unwrap trap → `TrapKind::UnwrapErrorOnOk`.
- **Drop the PROVISIONAL banner** (eval.rs:40) once the codes are normative; replace with a
  one-line pointer to `spec/prose/trap-codes.md`. **Also fix the adjacent `EXIT_TRAP` doc comment
  (eval.rs:44, "A catchable fault escaped uncaught")** — after re-founding, most trap variants
  (unwrap/assert/panic) are UNcatchable, so reword to "an uncaught trap (§10.9 catchable subset OR
  an uncatchable panic/unwrap/assert)". **Also update the parallel provisional note in
  `spec/ggdef/src/main.rs:11`** ("Exit codes (provisional until … lands in B)") so the two don't
  contradict. **And align the ggdef CLI's own trap print** (`spec/ggdef/src/main.rs:116`,
  currently `eprintln!("ggdef: trap: {}", f.message())`) to render the normative
  `trap[{code}]: {detail}` shape W5 defines — the definition's own tool should model the format it
  normativizes. (This line is a human diagnostic, NOT conformance-compared per Q1, so it's a
  consistency nicety, not a contract — but do it.)
- **Pin the catchable subset with a unit test** (`spec/ggdef/src/tests.rs`): assert
  `is_catchable()` is `true` for EXACTLY `{Overflow, DivByZero, Bounds}` and `false` for the other
  five — nothing else in T1 exercises the accessor, and this locks the §10.9 subset (W5) against a
  future variant being silently mis-classified.

### W2 — ggdef: implement the 3 missing trap classes (assert, panic, unwrap_error)
The scout MEASURED these as exit-2 (`EXIT_USAGE`, a *frontend* parse/elaborate error —
`spec/ggdef/src/main.rs:13,96`), i.e. they don't elaborate today. Implement each as a real
elaborated construct that evaluates to a Trap:
- **`.unwrap_error()`** — add `BuiltinMethod::UnwrapError` to `spec/ggdef/src/ggc.rs` (near
  `Unwrap`, ~line 202); recognize it in the elaborate builtin table
  (`spec/ggdef/src/elaborate/mod.rs:1610+`, alongside `"unwrap" => (BuiltinMethod::Unwrap, Some(0))`);
  eval arm modeled on `M::Unwrap` (`eval.rs:1200`): on an `Ok` receiver →
  `Trap(TrapKind::UnwrapErrorOnOk)`; on an `Error` receiver → extract the payload. (Mirror the
  reference semantics: `unwrap_error()` is the dual of `unwrap()`.)
- **`panic(msg)`** — recognize as a call in elaborate (near `print`, `elaborate/mod.rs:1221`);
  eval → `Trap(TrapKind::Panic(msg))`. Confirm it is `noreturn` in ggdef's flow (no value produced).
- **`assert cond` / `assert cond, msg`** — this is a STATEMENT, not a call: `ast::Stmt::Assert
  { condition, message }` (`src/parser/ast.rs:1053-1057`, the shared AST ggdef elaborates from).
  Recognize it in the elaborate STATEMENT path (not the call/builtin table); eval: when `cond` is
  false → `Trap(TrapKind::AssertFailed)` (detail = the message if present, else "assertion failed").
  When true → continue.
- Add a targeted unit test in `spec/ggdef/src/tests.rs` for each new trap class (evaluate a tiny
  program, assert `Outcome::Trap(TrapKind::X)` and exit 101).

### W3 — spectests frontmatter: the `trap:` field (reader + writer)
- **Reader:** `spec/ggdef/src/frontmatter.rs` — add `trap: Option<String>` to `Expect`
  (struct ~line 44). Parse a nested `#   trap: T_X` line inside the `expect:` block. **The
  invariant to enforce is `trap: present ⟺ exit == 101`** (a trap is exactly the exit-101
  outcome). Concretely, reject with new specific `FrontmatterError` variant(s): (a) a `trap:`
  code present with `exit != 101` (including `exit: 0`), and (b) `exit == 101` WITHOUT a `trap:`
  code. **Do NOT reject other nonzero exits without a trap code** — ggdef has two nonzero
  NON-trap outcomes, `IllFormed → 102` and `FuelExhausted → 103` (`eval.rs:47-49, 87-88`), which
  legitimately carry no trap code; the earlier "every nonzero exit is a trap" framing was wrong and
  would both reject valid 102/103 fixtures and contradict the writer (`render_expect_block_from`
  emits `exit_code()` for any outcome). Keying the rule on `== 101` keeps the `gen`→parse
  round-trip consistent. Update the frontmatter unit tests
  (frontmatter.rs:395+) to cover a `trap:`-bearing fixture round-trip AND both rejection shapes.
- **Writer (`gen`):** `render_expect_block_from` (`spec/ggdef/src/lib.rs:146`) currently takes
  `(exit, stdout)`. Extend it to also emit `#   trap: {code}` when the outcome is a Trap. The
  cleanest shape: pass the `TrapKind::code()` (or `Option<&str>`) through from
  `render_expect_block` (lib.rs:138, which holds `run.outcome`). Preserve the byte-exact
  round-trip (`json_escape` ⇄ `parse_json_string`; the `splice_expect` LAST-key convention).
  Order the block `exit:` (index 1) then `stdout:` (index 2) then `trap:` (index 3, only when the
  outcome is a Trap) — keep `exit` at index 1 and `stdout` at index 2 so the existing no-trap
  assertions stay valid.
- **Fix the test the signature change breaks:** `render_expect_block_from_round_trips_json_escape`
  (`spec/ggdef/src/tests.rs:1404-1423`) calls `render_expect_block_from(0, s)` / `(101, "")` and
  asserts `block.len() == 3` and `block[1] == "#   exit: …"`. Preserve those for the no-trap case
  (a 3-line block; the 4th `trap:` line appears only on a Trap outcome). Update or extend the test
  as needed. Also thread `Fault`→`TrapKind` through the `lib.rs:26` re-export and the existing
  `Fault::*` asserts in `tests.rs` (~:6, 234, 245, 255, 386, 807) — rustc will guide these, but
  they ARE part of W1's rename surface, not eval.rs-only. **⚠ `tests.rs:386` needs a SEMANTIC
  retarget, not a blind rename:** it asserts `Outcome::Trap(Fault::Panic(_))` for an unwrap-**None**
  program; after the W1 split, unwrap-None → `TrapKind::UnwrapNone` (a bare `Fault::Panic`→
  `TrapKind::Panic` rename would still COMPILE but fail at runtime under `cargo test -p ggdef`).
  Re-target it to `TrapKind::UnwrapNone`. **Adding `trap: Option<String>` to
  `Expect` also breaks three `Expect { … }` literals** — `frontmatter.rs:207` (parser), `:446`,
  `:472` (unit tests); update all three.
- **External consumer (ROOT crate — no `-p ggdef` gate compiles it):** `tests/smith/main.rs:557`
  is the ONLY out-of-crate consumer — `ggdef::Outcome::Trap(f) => …Trap { fault: f.message()… }`.
  It stays green PROVIDED you keep `Outcome::Trap` single-payload (it is: `Trap(TrapKind)`) AND
  preserve `TrapKind::message()` (the human-detail accessor) — both mandated by W1. Do NOT rename
  `message()`. The gate battery adds a compile-only smith check so this can't break invisibly.
- **N2 name-collision guard — do NOT touch the production `Fault` LANGUAGE enum.** There is a
  SECOND, unrelated `Fault` — the user-facing prelude enum `Fault.Overflow/DivByZero/Bounds` used
  across `tests/fixtures/*.gg`, `tests/lints.rs`, `tests/integration.rs`, and the self-host
  frontends. You are renaming ONLY ggdef's internal Rust enum in `spec/ggdef/src/eval.rs`. A
  global/blind `Fault`→`TrapKind` rename would be catastrophic. Stay inside `spec/ggdef/`.

### W4 — `adjudicate` tightening (BOTH lanes)
- **Harness lane:** `tests/spec_conformance.rs:229` `adjudicate`. The `expect.exit == 0`
  branch is UNCHANGED. The `expect.exit != 0` branch (currently "nonzero exit + stdout prefix,
  code NOT compared", the dormant defensive branch ~242-253) TIGHTENS to require:
  `out.status.code() == Some(101)` AND the fixture's `expect.trap` code appears in the observed
  stderr as `trap[<code>]` (strip/ignore the trailing ` at file:line:col`; **never compare the
  detail**), AND the pre-trap `expect.stdout` is a prefix of observed stdout. Update the
  module-doc comment (spec_conformance.rs:213-228) which currently says production "always exits
  1 … codes can never be equal" — that becomes the post-T2 contract; note T1 defines it and
  production meets it in T2.
- **ggdef lane:** `spec/ggdef/tests/spec_conformance_ggdef.rs` — the ggdef lane is HARD (every
  run fixture must MATCH ggdef). Ensure it compares the `trap:` code against ggdef's
  `Outcome::Trap(kind).code()` (the expectation is GENERATED from ggdef, so this matches by
  construction — but the comparison must actually check the code, not just exit).
- **CRITICAL green-story invariant:** after this tightening, a new trap fixture MATCHES on the
  ggdef lane (expectation generated from ggdef) and MISMATCHES on C/LLVM/self-host (they still
  exit 1, no `trap[` line). Because the existing 187 fixtures still MATCH, each production lane's
  `matched` count stays 187 and `matched >= FLOOR(187)` HOLDS (spec_conformance.rs:67-69,
  floor logic :495-508). **Do NOT bump the three MATCH floors** (C/LLVM/self-host stay 187 —
  production doesn't match yet; T2 bumps them). **DO** update `MIN_FIXTURES` (spec_conformance.rs:75,
  the glob-emptiness/count guard) to the new total fixture count so a silently-dropped fixture is
  caught. Verify the existing 187 stay green (the exit-0 branch is untouched).
- **DO bump `GGDEF_MATCH_FLOOR`** (`spec/ggdef/tests/spec_conformance_ggdef.rs:43`, which is BOTH
  the ggdef match floor AND its glob-count guard, `:63-64`) to the new total 187+N — on the ggdef
  lane the new fixtures MATCH by construction, so `matched` rises to 187+N and the lane's own rule
  (:40-42) + CLAUDE.md floor discipline require locking the gain in the same commit. (Unlike the
  three PRODUCTION floors, which stay 187.)
- **Fix the now-falsified doc comments** in `tests/spec_conformance.rs`: after the `MIN_FIXTURES`
  bump it no longer equals the three production floors, so ":46-47" ("All three lanes floor at
  MIN_FIXTURES") and ":72-75" ("all three lane MATCH floors currently equal it") become false —
  update both to describe the new reality (MIN_FIXTURES = corpus count; the three production floors
  stay at 187 until T2 lands the trap emit).

### W5 — prose: `spec/prose/trap-codes.md` + reference §10.9 re-founding
- **New `spec/prose/trap-codes.md`** mirroring `spec/prose/diagnostic-codes.md` exactly in
  structure: header (phase-1 deliverable, RFC pointer); "Source of truth & the ratchet"
  (`TrapKind::code()` in ggdef — and note T2a adds the production `src/trap.rs` copy + parity
  lint; exhaustive no-catch-all match = the ratchet); "Naming scheme" (`T_<VariantName>`,
  mechanical variant identity); the code→class→catchable table for the 8 variants; "Rendering"
  (`trap[T_X]: detail at file:line:col` + exit 101). Add a pointer to it from
  `spec/prose/README.md`.
- **Reference `docs/language-reference.md §10.9`** (`:2568-2585`): add the sentence re-founding
  `Fault` as the catchable SUBSET of the trap registry — the three variants
  (`Overflow`/`DivByZero`/`Bounds`) are exactly the catchable ones; the uncatchable classes
  (unwrap/assert/panic) are traps that panic uncatchably. Cross-reference `trap-codes.md`. Do NOT
  edit §10.1 (that's T3/D23). Keep the existing §10.9 variant descriptions.

### W6 — trap fixtures (ggdef-generated expectations)
Add run-tier fixtures under `spectests/run/` — **one per trap class you can generate** — with
frontmatter `expect: { exit: 101, trap: T_X, stdout: "<pre-trap output>" }`. Generate the
expectation via `cargo run -p ggdef -- gen <fixture>` (NEVER hand-write the expectation; NEVER
copy a backend's output — invariant #8 + the flow-from-definition rule). At minimum:
`T_Overflow`, `T_DivByZero`, `T_Bounds` (ggdef already supports these), plus `T_AssertFailed`,
`T_Panic`, `T_UnwrapNone`, `T_UnwrapError`, `T_UnwrapErrorOnOk` (enabled by W2). Each fixture:
minimal, deterministic, prints a line or two BEFORE the trap (so the stdout-prefix check has
something to bite). Use the existing `spectests/run/*.gg` frontmatter format as the template
(`#!spectest … #!end`, `mode: run`, `adjudicator: ggdef`, `since:`, `features:`). Human-review
the generated `trap:` code + exit against your intent.

## What stays green / the landing story (state this in your report)
- Existing 187 fixtures: UNCHANGED (exit-0 branch untouched) → all 4 lanes still MATCH them.
- New trap fixtures: ggdef lane MATCHES (generated-from-ggdef); C/LLVM/self-host lanes MISMATCH
  (exit 1, no `trap[` line) — EXPECTED, floors hold at 187, T2 flips them.
- ggdef crate tests (`cargo test -p ggdef`) green including your new W2 unit tests.

## NON-goals (explicitly out of scope — file nothing, just don't do it)
- Any `src/` change (production emit format, exit code, `src/trap.rs`, the runtime `.c` fold) — **T2a/T2b**.
- The D23 `E_UnhandledThrows` diagnostic, the no-`Result[` lint, smith throws tier, reference §10.1 — **T3**.
- Bounds real source locations in production — **T2b** (owner ratified "thread locations now" there).
- The runtime-library census fold, `abort()`→trap reroute — **T2b**.
- **The "no-drops-on-uncaught-trap is normative v1" text.** ggdef ALREADY models no drops on an
  uncaught trap (a `Halt::Trap` unwinds without running scope-exit drops), so there is nothing to
  implement here; the NORMATIVE prose statement of it belongs with the reference/T2 runtime-behavior
  write-through, not T1. Do not add it as a T1 work item.

## Gate battery (run FOREGROUND, generous timeouts; report actual output)
```
cargo build -p ggdef                                   # ggdef compiles
cargo test -p ggdef 2>&1 | tee /tmp/t1_ggdef_$$.log    # ggdef unit + frontmatter + your W2 tests
GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1 --nocapture 2>&1 | tee /tmp/t1_conf_$$.log
cargo test --test lints 2>&1 | tee /tmp/t1_lints_$$.log # import ratchet must stay green (no new src imports in ggdef)
cargo test --no-run --test smith 2>&1 | tee /tmp/t1_smith_$$.log # COMPILE-ONLY: catches the tests/smith/main.rs:557 ggdef::Outcome::Trap consumer (root crate, not covered by -p ggdef)
```
Acceptance: ggdef builds + all ggdef tests green; `spec_conformance` shows the 187 baseline
still MATCH on every lane, the new trap fixtures MATCH on ggdef and MISMATCH (not error) on the
production lanes, and NO floor regression; lints green (the ggdef import ratchet at
`tests/lints.rs` — budget 0 — must not trip; you add no `src/ir`/`src/semantic` import).

## Worktree & agent discipline (non-negotiable — CLAUDE.md "Multi-agent orchestration")
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm BOTH point inside your worktree
(under `/workspace/gorget/.claude/worktrees/`). NEVER touch `/workspace/gorget` (main) or
`/workspace/gorget-1` directly; do NOT `cd` into either; do NOT use absolute paths starting
`/workspace/gorget/...` (your worktree nests UNDER main — an absolute path there writes into
MAIN). All file ops RELATIVE to your worktree. At entry run `git merge --ff-only gorget-1
2>/dev/null || true`. **Checkpoint progress to `/tmp/t1_progress_$$.md` after each work item**
(you are killable). Stage ONLY the exact files you changed by name (`git add spec/... spectests/...
docs/...`) — NEVER `git add -a`/`git add .`/`git commit -a`. NEVER `git stash` (save to /tmp
instead). After any non-Edit-tool write, run `git -C /workspace/gorget status` and STOP if it
shows changes (you leaked into main). Commit on your worktree branch with a clear message; do
NOT run the full `cargo test --test integration` (that's the parent's job).

## Deliverable
A report (also checkpointed to `/tmp/t1_report_$$.md`): the diff summary per work item with
file:line, the exact gate-battery command output (paste the MATCH/MISMATCH/floor lines from
`spec_conformance`), the list of new fixtures + their ggdef-generated `trap:` codes, and any
premise that turned out different from this brief (correct it, don't paper over it).
