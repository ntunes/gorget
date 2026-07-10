# R-C brief — LLVM `T_UnwrapErrorOnOk` combinator guard (+ happy-path repair)

> **Round:** review-residuals (xhigh review of `f42eea96..7aad1844`, TODO High entry
> "D11/D23-wave RESIDUALS" item (c)). **Zone:** `src/backend/llvm/` + 2 new fixtures
> + their harness wiring in `tests/integration.rs` (⚠ R-D also edits integration.rs
> — DIFFERENT region: R-D touches the `check_gg_fails_no_desugar` harness ~:7208 and
> its d23 call sites ~:4736-4807; R-C wires 2 fixtures ~:6459-6482. Textual
> adjacency only; parent resolves at integration).
> **Scout:** report `/tmp/scout_rr_c_report.md`, prototype `/tmp/scout_rr_c_prototype.patch`
> (198 lines), measured end-to-end at `cab529cd`.
> **Status:** v1 — awaiting sequential fresh-review passes.

## Verified premises (scout, empirical — with a significant sharpening)

1. **CONFIRMED: no tag guard in the LLVM combinator arm** (pre-fix
   `src/backend/llvm/mod.rs:5568-5624`) while both C twins trap
   (`emit_hof.rs:154-163` span-carrying `gorget_trap_at`;
   `emit_types.rs:232-254` `gorget_trap`).
2. **The two-path split, precisely:** `.unwrap_error()` on a Copy/Move PLACE with
   an inferable Result type takes the GIR fast path
   (`src/ir/lowering/exprs/methods.rs:1014-1069`) → `__result_unwrap_error` →
   LIR Tier-2a guard (`src/lir/lower/insts.rs:4067-4244`, emitter `:3603-3651`)
   → traps on BOTH backends. Any other receiver goes through the generic mangle
   (`methods.rs:1780`) → monomorphized `Result__T__E__unwrap_error`, which
   Tier-2a does NOT match (`insts.rs:4071-4075` matches `*__unwrap`, not
   `*__unwrap_error`) → survives raw to the backends.
3. **The reaching shape: a module-level static receiver** (`Result[int,int] R =
   Ok(7)`; `R.unwrap_error()`) — a `Constant::GlobalRef`, not a place
   (`methods.rs:1033`). ⚠ **Record correction:** the D11 T2b commit (`f6fddc0b`)
   claimed the C-side fold was "defensive (all shapes intercepted by Tier-2a)" —
   WRONG; the C fold was load-bearing. The LLVM twin was left broken.
4. **The LLVM arm had THREE defects, not one:** (i) no tag guard; (ii) on
   untyped receivers (a global's address is bare `Ptr` in `val_types`) the
   silent `(offset 8, "i64")` fallback reads the **Ok slot** as the "error";
   (iii) dst mis-typed as `PtrTo(Result-struct)` by the val_types combinator
   override (`_` arm, pre-fix `mod.rs:2705-2740`) though unwrap_error returns
   the PAYLOAD → llc type errors at consumers — **the happy path was broken
   too** (`'%v1' defined with type 'i64' but expected 'ptr'`).

## Measured divergence (pre-fix, the normative violation)

`Result[int,int] R = Ok(7)` + discarded `R.unwrap_error()` + `print("after")`:
- **C**: `trap[T_UnwrapErrorOnOk]: unwrap_error on Ok at …:4:5`, exit **101**
- **LLVM**: prints `after`, exit **0** — silent keep-running.

Consumed form (`int x = R.unwrap_error()`): llc failure on BOTH Ok and Error
receivers. Six conventional receiver shapes (local/field/generic/closure/element/
chain) all take Tier-2a and trap identically — probed, no other reaching shape.

## Design (prototyped): narrow guard + mirrored pre-pass arm; fix (b) stays a separate track

- **Emit-side guard** using the established LLVM trap idiom: `tag != 1` → intern
  code/detail/file + `call void @gorget_trap_at` + `unreachable`; detail text and
  span BYTE-MATCH the C twin (conformance-invisible, but reference-grade).
- **Struct-layout resolution** for untyped receivers falls back to the name-prefix
  registry — the SAME mechanism the C twin uses (parity over purity here; the
  typed-metadata root fix is the filed LIR-extern follow-up, not this track).
- **val_types fix:** dst = field-2 payload type (not `PtrTo(Result-struct)`) —
  repairs the happy path.
- **Pre-pass mirror arm placed FIRST in the CallExtern chain** (before
  `is_opt_combinator`, which also matches these names via RES_COMB — pointer
  comments on BOTH sides): bump + `unwraperr.{bid}.{uid}.ok` label iff
  `dst.is_some()`; dst-None = no bump (this also removes a latent pre-existing
  dst-None desync). Gating is exactly the emit's: name-match && !args.is_empty().
- **Why NOT fix (b) here (measured):** the `block_exit_labels` twin is 227 lines
  with a single consumer (`mod.rs:3297`); (b)'s mechanics are ~25 lines. BUT it
  is not drop-in: correctness inverts to "every label-creating site must update
  `current_label`", and at least one site already violates that —
  `gorget_str_clear` creates `scl.done` labels and returns without updating
  (`mod.rs:5356-5361`; the pre-pass covers it at `:3238`). ~44 label-writing
  sites need the audit; the full LLVM sweep is the only honest gate. Filed as its
  own track (TODO structural-guard entry updated with these measurements).

## After-fix (scout, byte-identical on both backends)

Trap cases exit 101 with identical trap lines (discarded/consumed/printed, int
and String payloads); happy paths print `42`/`boom` exit 0 (were llc-fails);
the acid test — `unwrap_error` in a `while` loop BEFORE two overflow-checked
adds — prints `15` on both, and the .ll confirms the guard consumed uid 0,
shifted the add labels to `ov.2.1`/`ov.2.2`, and the loop phis reference
`%ov.2.2.ok`: pre-pass and emit agree under exactly the drift hazard.

New fixtures: `tests/fixtures/unwrap_error_on_ok_combinator_traps.gg` (trap
route, `run_gg_panics_with_stdout`) + `unwrap_error_combinator_static.gg` (happy
path, `42\nboom\n`), wired at `tests/integration.rs:6459-6482`.

## Scout gates (all green this session)

lib 1105/0 · unwrap C 15/0 / LLVM 15/0 (incl. the 2 new tests) · trap C 11/0 /
LLVM 11/0 · fault C 54/0 / LLVM 54/0. Logs `/tmp/scout_rr_c_*.log`.

## Twin-agreement pins (the executor verifies each, they are the track's risk)

1. Pre-pass arm ordered BEFORE `is_opt_combinator` on both sides, with pointer
   comments referencing each other.
2. Bump iff `dst.is_some()` — exactly mirrors the emit.
3. Bare `__result_unwrap_error` reaching the backend is dst-None-only (Tier-2a
   takes dst-Some) — both sides no-op consistently.
4. The removed `PtrTo(enum)` dst typing had NO green dependents (all reaching
   shapes were llc-broken) — re-verify with the full unwrap/fault suites.
5. Self-host lane untouched (its combinator path is C-emit; no .gg edits).

## Executor protocol (multi-agent rules in full)

Worktree-isolated; worktree-relative paths only; no `git stash`; checkpoint diff
to /tmp after each work item; stage by explicit file name; final gates FOREGROUND
with generous timeouts. Base: apply `/tmp/scout_rr_c_prototype.patch`, re-derive
judgment hunk by hunk (you own it — especially the pre-pass mirror arm and the
val_types dst change).

## Gate list (executor, foreground, tee'd)

1. `cargo build`
2. `cargo test --lib` — 1105/0
3. `cargo test --test integration unwrap -- --test-threads=4` — 15/0, then same
   under `GG_BACKEND=llvm --release` — 15/0
4. `cargo test --test integration trap -- --test-threads=4` (C) and
   `GG_BACKEND=llvm --release` — 11/0 each (skip self_host under LLVM if timeouts
   bite; parent covers)
5. `cargo test --test integration fault -- --test-threads=4` C AND LLVM — 54/0
   each (the phi/label hazard lives here)
6. The acid test transcript (loop + overflow-checked ops after the guard) on
   both backends + a diff of the two backends' stderr for the trap fixtures
   (byte-identical expected)
7. `cargo test --test lints` — no deltas expected.

Parent (NOT executor): full both-backend sweep + bootstrap + spec_conformance at
integration.
