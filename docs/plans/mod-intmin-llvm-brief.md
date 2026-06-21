# Brief — `.mod()` (`Inst::Mod`) LLVM `INT_MIN/-1` + div0 guards

**Track:** sibling of the error-model Inc-2 (E) plain-op `INT_MIN/-1` fix. Rust-backend only
(NOT self-host, NOT docs). Files: `src/backend/llvm/mod.rs`, `src/sim/dispatch.rs`, +2 new
fixtures. Disjoint from the concurrent docs/self-host/parity tracks.

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1` first).
`git add` ONLY the changed source files + the 2 new fixtures. Do not touch `error-model.md`
or any self-host fixture.

---

## The bug (scout `abf308cd`, verified against source + by running + cross-arch disasm)

`.mod()` is Euclidean modulo, lowered to `Inst::Mod` — DISTINCT from `%` (`Inst::Rem`).
Path: `x.mod(d)` → `methods.rs:502-507` `BinOp::Mod` → `calls.rs:92` `GirBinOp::Mod => Inst::Mod`.

**Two defects in the LLVM `Inst::Mod` arm (`src/backend/llvm/mod.rs:3457-3464`, the non-float
signed branch — re-locate, line drifts):** it emits a BARE `srem → add → srem` Euclidean
chain with **(1) no `INT_MIN/-1` guard** and **(2) no div0 guard at all.**

- **(1) `INT_MIN.mod(-1)`** — Euclidean result is genuinely `0` for all x (range `[0,|-1|)={0}`).
  Bare `srem INT_MIN,-1` is LLVM-IR UB. **⚠ CORE-#8 / host-arch trap:** on aarch64 (this
  sandbox) it lowers to `sdiv` and accidentally returns `0`, so the fixture "passes" here and
  C+LLVM appear to agree — but on **x86-64** the same IR lowers to `idivq`, and
  `idiv INT_MIN,-1` raises CPU divide-overflow → **SIGFPE, program crash.** The scout confirmed
  this with `llc -mtriple=x86_64-unknown-linux-gnu` showing `idivq`. *"Both backends agree on
  0 on aarch64" is NOT a pass — it is the exact necessary-not-sufficient trap.*
- **(2) `x.mod(0)`** — host-INDEPENDENT: LLVM-Mod returns the dividend (printed `5` on aarch64)
  instead of panicking. C-Mod panics with `division by zero`.

**C-Mod is already correct** (`src/backend/c_lir/mod.rs:2535-2553`): div0 trap + per-type
`if (l==TYPE_MIN && r==-1) d=0; else <Euclidean>`. The fix brings LLVM to this parity.
**const-fold is safe at BOTH layers** — the LIR fold (`src/lir/optimize.rs:764`, `checked_rem`
→ `None`) AND the GIR fold (`src/ir/transforms/optimize.rs:662-668` i64 / `:691-695` i32, also
`checked_rem`-guarded) both decline `INT_MIN/-1` and div0. No change to either const-fold layer.

---

## The fix

### 1. `src/backend/llvm/mod.rs` — `Inst::Mod` arm (the one site)
Mirror C-Mod's structure with LLVM basic-block/branch plumbing. Reuse the file's existing
div0-trap idiom — it is emitted INLINE in the `Inst::Div` arm (`:3384-3401`) and `Inst::Rem`
arm (`:3419-3436`), not at the brief's earlier (wrong) `:3377` cite. The per-type `TYPE_MIN`
decimal `-{1u128 << (bits-1)}` is computed by `emit_div_overflow_trap` (`:6905`) and also at
`FaultCheck` (`:3511`) — reuse that EXPRESSION. **⚠ Do NOT call `emit_div_overflow_trap`
verbatim — it TRAPS; Mod needs `INT_MIN/-1 → 0`, not a trap.** Mirror the Div/Rem arms'
`current_label`/`trap_counter` threading exactly (the new branches introduce basic blocks; set
`*current_label = ok_label` after, never emit naked labels — the FaultCheck block-exit-label
pre-pass at `:3466-3473` is the relevant pattern), all in scope inside `emit_inst` (`:3077-3090`):
- **Signed path:** (a) `rhs == 0` → emit the same div0 panic+trap C-Mod emits
  (`"{file}:{line}:{col}: division by zero"`, exit 1); (b) `lhs == TYPE_MIN && rhs == -1` →
  `dst = 0` (a select or branch — NOT a trap; Mod's result is genuinely 0, unlike Div/Rem
  which panic); (c) else the existing `srem/add/srem` Euclidean chain.
- **Unsigned (`urem`) path:** add the `rhs == 0` div0 trap too (`urem x,0` is UB; C-Mod's
  unsigned arm guards it). The `INT_MIN/-1` special-case is signed-only.

### 2. `src/sim/dispatch.rs:5390-5394` — companion (interpreter / `meta` eval)
The signed `BinOp::Mod` arm guards `ri == 0` but then does bare `li % ri`, which **panics in
Rust debug on `INT_MIN % -1`.** Add the `ri == -1 => 0` special-case before the `li % ri`
(mirror the backends). The unsigned sim arm (`:5353`) is fine. Re-locate the lines; verify the
guard is genuinely the signed Mod arm before editing.

### 3. Fixtures
- **`tests/fixtures/mod_intmin.gg`** — model on `div_intmin_plain.gg`/`rem_intmin_plain.gg`
  but expected output **differs: prints `0`, does NOT panic.** Use NON-CONSTANT operands (locals)
  for ROBUSTNESS and to mirror the established `div_intmin_plain.gg`/`rem_intmin_plain.gg`
  fixtures. (The reason the literal-only `mod_rem.gg` never caught this is NOT that
  `INT_MIN/-1` constants fold away — they don't: `checked_rem` returns `None` on `INT_MIN/-1`
  AND div0, so the const-fold layers decline both cases. `mod_rem.gg` simply uses
  NON-overflowing constants like `7.mod(3)`, which fold to a value and so never exercise the
  backend arm at all, AND it has no `INT_MIN/-1` case. Locals guarantee the value reaches the
  backend arm regardless.):
  ```gorget
  void main():
      int imin = -9223372036854775807 - 1
      int neg1 = -1
      int r = imin.mod(neg1)
      print(f"{r}")
  ```
  Expected stdout BOTH backends: `0`.
- **`tests/fixtures/mod_zero.gg`** — negative fixture: `x.mod(0)` panics with `division by
  zero` on BOTH backends (locks in defect (2)). Use a non-constant 0 divisor so it isn't
  folded. Wire it the way the other panic/negative fixtures are wired in `tests/integration.rs`
  (find how `div_intmin_plain`/a div0 panic fixture asserts a panic+nonzero exit, and mirror).

---

## Verification (LOAD-BEARING — the bug is invisible on aarch64)

The output-review and the executor's own check MUST NOT conclude "no bug / fixed" from running
the fixture on aarch64 alone (it prints `0` even unfixed). Required proof the fix actually works:
1. **Inspect the emitted LLVM IR** for `mod_intmin.gg`. There is NO `--emit-llvm` flag — the
   `.ll` is a build artifact left next to the fixture at `<stem>.ll` by `compile_llvm_pipeline`
   (`src/main.rs:1189`); just build with the LLVM backend and read `mod_intmin.ll` (cf. the
   existing `div_intmin_plain.ll`, which shows what a guarded path looks like). Confirm the IR
   now contains explicit `rhs==-1 → 0` and `rhs==0 → trap` branches around the `srem`, so the
   `INT_MIN/-1` path no longer reaches a bare `srem`/`idivq`.
2. **Cross-arch check:** `llc -mtriple=x86_64-unknown-linux-gnu` on that IR — confirm the
   `INT_MIN/-1` path is guarded (does not reach an unguarded `idivq`). The scout did exactly
   this to prove the bug; do it to prove the fix.
3. Run `mod_intmin.gg` on aarch64 C + LLVM → both print `0`. Run `mod_zero.gg` → both panic.

## Environment caveat to verify (do NOT preemptively act)
The scout hand-compiled C with `-std=gnu11` and noted C-Mod emits GNU `typeof(...)`, which a
strict `-std=c11` gcc rejects. **BUT** the existing Inc-2 div/rem fixtures build fine on the C
backend here, and gcc's default dialect (gnu*) supports `typeof`. So FIRST run the ACTUAL
`gg build` (default C backend) on `mod_intmin.gg` and see whether it builds. Only if it
genuinely fails to build on C is there anything to do — and that would be a SEPARATE pre-existing
`typeof`-vs-`__typeof__` fix (file a TODO), NOT part of this brief. Do not change C-Mod's
`typeof` unless the fixture's C build actually breaks.

## Acceptance bar (Core-#8)
Both backends produce identical, CORRECT behavior: `INT_MIN.mod(-1) == 0` (verified via IR +
x86-64 cross-check, not just aarch64 run), `x.mod(0)` panics. The sim no longer panics on
`INT_MIN % -1`. No reliance on host-arch UB remains. Gate: `cargo test --lib`,
`cargo test --test integration` C backend, `GG_BACKEND=llvm` integration, `bootstrap_fixed_point`.
