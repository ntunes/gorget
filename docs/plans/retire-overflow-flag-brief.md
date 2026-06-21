# Implementation Brief — Retire the `--overflow` flag

> **Status: DRAFT for review (2026-06-21).** Scout-verified change-set (every site traced
> end-to-end). Owner decisions baked in: (1) retire BEFORE Increment 2; (2) `directive
> overflow=wrap` becomes a HARD ERROR (`UnknownDirective`), not a silent no-op; (3) the
> lexical `wrapping{}` scope is DEFERRED (filed as a TODO; `+%` is the sole escape).

## 0. The goal
Delete the `--overflow=wrap|checked` global flag + the `directive overflow=wrap` + the
`overflow_wrap` plumbing. After this: plain `+`/`-`/`*` ALWAYS check overflow (panic, or
`catch Fault.Overflow` recovers); `+%`/`-%`/`*%` wrap (per-op, explicit, UNCHANGED). One
global mode gone. This is a *deletion* — it removes complexity. The `+%` path is fully
separate (hardcoded `Overflow::Wrap`, never reads the global) and SURVIVES.

## 1. The change-set (9 source files) — `default_overflow` → always `Overflow::Trap`
Delete the flag plumbing top to bottom (all `file:line` scout-verified against current src):
1. **CLI parse (3 sites):** `src/main.rs:2456-2457` (build), `2606-2607` (run), `3484-3485`
   (test) — remove the `--overflow=` parse. Remove the threading into `LoweringOptions`
   (`main.rs:2478, 2936-2937, 2966-2967, 3004-3005, 3033-3034, 3492-3493`).
2. **`LoweringOptions` fields:** `src/ir/lowering/mod.rs:46-49` (`overflow_wrap`,
   `overflow_checked`) — delete both.
3. **Directive parse + CLI-override merge:** `src/ir/lowering/mod.rs:515` (the `"overflow"`
   arm) + `:530-531` — delete.
4. **`LoweringContext` field:** `src/ir/lowering/context.rs:336` + init `:511` — delete.
5. **Runtime-config write + field:** `src/ir/lowering/mod.rs:2048` + `src/ir/mod.rs:118-119`
   (`RuntimeConfig.overflow_wrap`) — delete.
6. **THE DECISION SITE:** `src/lir/lower/calls.rs:81-86` — `lower_binop(... overflow_wrap: bool)`:
   **drop the param**; `default_overflow` becomes the literal `Overflow::Trap`. The
   `AddWrap`/`SubWrap`/`MulWrap` arms (`calls.rs:100-102`) already hardcode `Overflow::Wrap`
   and never read the param — **leave them; the `+%` path is confirmed separate.**
7. **`lower_binop` threading:** `src/lir/lower/mod.rs:96` (field), `:290` (read from
   `gir.runtime.overflow_wrap`), `:1484, 1542` (ctor) — delete. Callsites
   `src/lir/lower/insts.rs:105, 133, 165` — drop the `self.overflow_wrap` arg. (`:165` is the
   fault-catch commit path; it passes `commit_op = *Wrap` explicitly, so NO behavior change.)
8. **`-fwrapv` cc/link args (4 sites):** `src/main.rs:870, 976, 1121-1123, 1444-1446` —
   delete (belt-and-suspenders only; always-Trap uses `__builtin_*_overflow`, so `-fwrapv`
   was never load-bearing). ⚠ verify no fixture relied on `-fwrapv` masking a *different* UB.
9. **Interpreter/sim (`gg sim`):** `src/sim/dispatch.rs:227` (field), `:287` (init), and the
   `if self.overflow_wrap { wrapping_* } else { checked_* }` for plain Add/Sub/Mul
   (`:5338/5343/5348/5387/5392/5397`) — collapse each to the CHECKED branch. The
   `BinOp::*Wrap` arms stay (operator-driven).
10. **Directive now ERRORS (owner decision #2):** `src/semantic/mod.rs:143-155` — **delete the
    `"overflow" =>` arm** so `directive overflow=wrap` falls to the `_ =>` arm (`:172-179`) and
    becomes an `UnknownDirective` error. Reference-grade: reject the removed knob.

**Backends: ZERO changes** — C-LIR (`c_lir/mod.rs:2439-2459`) and LLVM (`llvm/mod.rs:2111-2113,
3335-3355`) already gate `__builtin_*_overflow`/intrinsics purely on `overflow == Trap`; no
global. Retirement only changes which `Overflow` variant `lower_binop` emits for plain ops.

## 2. Out of scope (filed, not built)
- **Lexical `wrapping { … }` scope (owner decision #3 — DEFERRED).** `+%` covers every
  legitimate use; the self-host runs fully-checked and isn't overflow-bound (proof always-checked
  is acceptable). FILE a TODO: "add a `wrapping{}` lexical scope IF measurement ever demands it —
  it lowers to the same `*Wrap` GIR ops, additive, no backend work; self-host compile is the
  benchmark." Do NOT build it here.

## 3. Staging (build GREEN at each step)
1. **Core:** drop the `lower_binop` param → `default_overflow = Overflow::Trap` literal; delete
   the `overflow_wrap` field threading (`lir/lower/{mod,insts}.rs`, `ir/mod.rs`,
   `ir/lowering/{mod,context}.rs`); collapse the sim path. Build + `cargo test --lib`.
2. **CLI + directive:** remove the `--overflow` parse + `LoweringOptions` fields + `-fwrapv`;
   make `directive overflow=wrap` an `UnknownDirective` (`semantic/mod.rs`). Build.
3. **Tests:** migrate/delete/edit per §4. Build + the overflow-family integration tests.
4. **Docs:** rewrite per §5.

## 4. Test plan (executor runs targeted; parent runs the full sweep)
- **Migrate (the ONE output-dependent fixture):** `tests/fixtures/numeric_overflow_wrap.gg` →
  use `+%` (`int wrapped = max +% 1`), drop the directive; **assert the EXACT same output**
  (`9223372036854775807` / `-9223372036854775808` / `true`). Repoint `lir_ab.rs:815`
  (`lir_ab_numeric_overflow_wrap`) to it (real cross-backend wrap coverage).
- **Delete:** integration tests `overflow_wrap` (`integration.rs:5638-5641`),
  `directive_overflow_wrap` (`:5813-5817`), `directive_cli_override_overflow_checked`
  (`:5826-5831`); `lir_ab` tests `lir_ab.rs:257` + `:791` (⚠ RISK #1 — these build
  `use_overflow_wrap.gg`/`overflow_wrap.gg` with NO flag, so post-retirement the directive
  ERRORS → `run_gir` returns `None` → the test PANICS; delete them in THIS change); fixtures
  `tests/fixtures/{overflow_wrap,use_overflow_wrap}.gg`.
  - **`src/parser/tests.rs:1050-1060` (pass 1 — OPTIONAL cleanup-delete):** this only checks the
    directive *parses* (the parser is unchanged), so it would still PASS post-change — delete it as
    cleanup if you like, but it is NOT a break; don't mis-attribute a failure to it.
- **Edit:** semantic unit `valid_directives_no_error` (`src/semantic/typecheck.rs:7501-7511`) —
  remove `directive overflow=wrap` from the input string (would now be `UnknownDirective`).
  (Pass 1: this is the single most-likely SILENT test break — it actively asserts NO
  `UnknownDirective` for that directive; must be edited.)
- **Add (negative fixture, reference-grade):** `directive_overflow_removed.gg` —
  `directive overflow=wrap` → `UnknownDirective` typecheck error (use `check_gg_fails`).
- **`+%` positive coverage ALREADY EXISTS (pass 1) — do NOT add a new fixture:**
  `tests/fixtures/wrapping_ops.gg` (`integration.rs:5740`, `run_gg`, NO flag) already exercises
  `+%`/`-%`/`*%`/`+%=` at the overflow boundary (`INT_MAX +% 1 → INT_MIN`, etc.). It is the
  load-bearing flag-free proof that `+%` survives the global's removal — just VERIFY it stays green.

## 5. Docs to rewrite (scout-located)
- `docs/language-design.md` §2.2 line **213** — delete the `--overflow=wrap` sentence; keep
  panic-by-default + the `+%`/`-%`/`*%` escape.
- `docs/language-reference.md` line **1538** (the `directive overflow=wrap` mode sentence; keep
  "`+%` wraps" but drop "regardless of mode"); §16.3 directive row **4585**, example **4569**,
  merge-table **4613-4615**; §17 CLI flag rows **4652-4653**.
- `docs/book/`: `appendix-directives.md` (**39-56, 131, 146**), `appendix-cli.md` (**41-42, 204**),
  `appendix-operators.md` (**54** — "always wrap, regardless of build"), `02-types.md` (**216**),
  `10-errors.md` (**537**; keep **540** `+%`).
- `docs/plans/error-model.md`: STRIKE the now-vacuous override discussion — §11.2 note
  **638-650** (the "global `--overflow=wrap` must not defeat a local catch" force-checked-override),
  §11.4 doc-obligation, §11.5 test bullet **727** ("checked even under `--overflow=wrap`"), §11.7
  **743-745**, plus the stragglers at `error-model.md:44` and `:332`. (Increment 1's `FaultableBinOp`
  already force-checks structurally regardless of any flag, `insts.rs:117-170`, so this whole worry
  is moot post-retirement.)
- **`docs/devbook/` (pass 1 — MISSED in the first draft):** `01-pipeline-and-driver.md:182` (lists
  `--overflow=wrap|checked` as a build-shaping flag) and `21-simulator.md:210` ("`--overflow=wrap`
  build option threaded through lowering") — both must be removed/updated, or they violate the §7
  acceptance ("no surviving `--overflow=wrap` reference").

## 6. Constraints (NON-NEGOTIABLE)
- **Worktree:** `pwd` + `git rev-parse --show-toplevel` FIRST; INSIDE your worktree, NEVER
  `/workspace/gorget-1`. First action `git merge --ff-only gorget-1`. No `cd` to the main tree.
- **Stage by filename:** `git add <files>` only — NEVER `git add -a`/`.`/`commit -a`.
- **Do NOT touch:** the `+%`/`-%`/`*%` per-op wrap path (it must keep wrapping); the self-host
  (`tests/fixtures/self_host_*/` — it never used the flag, verified); Div/Rem `INT_MIN/-1`
  handling (that's Increment 2's (E), separate). Build a `wrapping{}` scope (deferred).
- **Both backends:** the overflow-family integration tests pass on default AND `GG_BACKEND=llvm`
  (backends unchanged, but verify).
- Report: per-stage diff, which tests deleted/migrated/edited, `cargo test --lib` + the
  overflow-family integration results on both backends, any deviation + why. Do NOT run the full
  sweep (parent's job). If the design doesn't hold against source, STOP and report.

## 7. Acceptance criteria (parent verifies)
- `cargo build` + `cargo test --lib` green.
- Plain `int x = INT_MAX + 1` PANICS (no longer wrappable via a flag); `INT_MAX +% 1` WRAPS.
- `directive overflow=wrap` → `UnknownDirective` typecheck error (`directive_overflow_removed.gg`).
- `numeric_overflow_wrap.gg` (now `+%`) produces the SAME output on both backends.
- All deleted tests/fixtures removed; the `lir_ab` panics (risk #1) gone; semantic unit edited.
- `self_host_bootstrap_fixed_point` + the full both-backend sweep green (self-host guaranteed
  unaffected).
- Docs rewritten (no surviving `--overflow=wrap` / `directive overflow=wrap` reference, except the
  negative fixture + the deferred `wrapping{}` TODO).
- The `overflow_wrap` symbol is GONE from `src/` (grep returns only the LIR `OVF_WRAP`/`*Wrap`
  operator constants).
