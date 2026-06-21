# Implementation Brief — Error Model Phase 1, Increment 2 (Bounds + Div-split + qualifier; wrap-override already done)

> **Status: DRAFT brief for review (2026-06-21).** Spec: `docs/plans/error-model.md` §11.
> Builds on Increment 1 (LANDED `8ab75635`, DONE.md) — read its record + the
> Increment-1 brief `docs/plans/error-model-phase1-brief.md` for the machinery this
> extends (`Inst::FaultCheck`, `Instruction::FaultableBinOp`, `FaultScope`,
> `lower_fault_catch_expr`, the per-category handler-entries). Scout-verified design;
> every premise below was traced end-to-end (and (B) compiled+run).

## 0. The goal
Extend local fault-catch with: **(A)** `Fault.Bounds` (catch an out-of-bounds index),
**(C)** split `INT_MIN/-1` out of `DivByZero` into `Fault.Overflow`, **(D)** reject a
wrong enum qualifier (`Bogus.Overflow`), and **(B)** lock in the
already-working `--overflow=wrap` behaviour. All four are in the same fault-catch
subsystem; stage them in the §3 order.

## 1. Scope — Increment 2 (IN), by sub-task

### (A) `Fault.Bounds` — catch out-of-bounds indexing  [LARGE / HIGH-scrutiny]
An index `xs[i]` is NOT a binop — it lowers via a dedicated GIR `Instruction::IndexLoad`
(`src/ir/instructions.rs:158`, built at `src/ir/lowering/exprs/methods.rs:3412`), then
GIR→LIR materializes `CallExtern("gorget_array_get", …)` (`src/lir/lower/insts.rs:1062`)
whose **bounds check is RUNTIME-SIDE** (`src/backend/c/runtime/runtime_array.c:31`,
`exit(1)` on OOB). (Note: LLVM has no own index lowering — it calls the same C symbol;
the runtime is at `src/backend/c/runtime/runtime_array.c`, not the brief-stale
`src/backend/c/runtime_array.c`.) So this is the OPPOSITE of overflow/div0 (which are
inline). Mirror `FaultableBinOp`, reuse the already-wired safe path:
1. **New GIR `Instruction::FaultableIndexLoad { dst, base, index, read, fault_handler: BlockId }`**
   — a separate variant paralleling `FaultableBinOp` (NOT a field on `IndexLoad`; keeps
   the ~existing optimizer/sim/liveness/SSA `IndexLoad` sites untouched). Update every
   exhaustive GIR site (use/def, printer, validate, optimize incl. the CFG passes
   `successors`/`thread_jumps`/`eliminate_dead_blocks` that must remap the embedded
   `fault_handler` BlockId, sim) — the compiler's exhaustive matches force this.
2. **`FaultScope` gains `bounds_handler: Option<BlockId>`** (`src/ir/lowering/context.rs:294-300`),
   set in `lower_fault_catch_expr` (`src/ir/lowering/exprs/mod.rs:3527-3536`) alongside a
   new `bounds_entry` block.
3. **GIR build decision at `methods.rs:3412`** (mirror `operators.rs:310`): a typed helper
   `bounds_handler_for(ctx)` reads `ctx.func_state.fault_scope?.bounds_handler` (typed,
   NEVER name-matched); if `Some`, emit `FaultableIndexLoad`. **Gate to array/Vector
   element-READ only** (the only path with a runtime bounds check); dict-get / string-index
   / range-slice are OUT (different runtime fns; some have no `safe_*` variant) — the
   non-array `_ =>` arm stays the non-faulting `IndexLoad`.
4. **GIR→LIR new arm in `insts.rs`** (mirror the `FaultableBinOp` arm `:117-170`): emit
   `CallExtern("gorget_array_safe_get", [base, idx]) → raw_ptr` (the non-panicking variant,
   `runtime_array.c:41`, signed `int64_t`, returns NULL on OOB), then `Inst::Cmp Eq raw_ptr,
   null → flag`, then `Term::Branch { flag → handler (block_map remap), else → cont_bb }`;
   in `cont_bb` deref `raw_ptr` for the element (NULL is NEVER deref'd before the branch —
   branch-before-deref, unwind-free). This is the `.get()` shape (`methods.rs:2510-2521`)
   with the null-branch pointed at the handler instead of building `None`. ⚠ PRESERVE the
   borrowed-element metadata the normal arm sets: str-ptr marking (`str_ptr_values.insert`,
   `insts.rs:1080`), the Ptr-return-vs-deref split, and resource/FieldPath provenance
   (`methods.rs:3413-3421`) — do not drop them.
5. **Handler-entry**: add a `bounds_entry` arm in `lower_fault_catch_expr`'s `lower_entry`
   (`exprs/mod.rs:3585-3596`) materializing `Fault.Bounds()` via `emit_enum_init_owned(…"Bounds"…)`.
   Binding-form `catch f:` installs all handlers (overflow/divzero/bounds); pattern-form
   `catch Fault.Bounds:` installs only `bounds_handler`.
6. **Register `Bounds`** in BOTH layers: `builtin_fault_enum()` (`src/ir/lowering/generics/substitute.rs:330-339`)
   + `src/semantic/resolve.rs:178` (`&["Overflow","DivByZero","Bounds"]`).
7. **Arm-count lint**: extend `fault_op_lowering_arms_count` (`tests/lints.rs:2355`) to also
   count the `FaultableIndexLoad` lowering arm + its handler-entry.

### (C) Split `INT_MIN/-1` → `Fault.Overflow`  [MEDIUM / HIGH-scrutiny]
A single signed Div op has TWO fault conditions; today both collapse into one flag →
`divzero_handler` → `Fault.DivByZero()` (wrong for `INT_MIN/-1`, which is an overflow).
`FaultScope` ALREADY has both `overflow_handler` AND `divzero_handler` (`context.rs:294-300`).
1. **Split `FaultOp::Div`** (`src/lir/mod.rs:308`): add `FaultOp::DivOverflow` (tests
   `lhs==MIN && rhs==-1` only); narrow `Div`/`Rem` to test `rhs==0` only. The C
   (`c_lir/mod.rs:2580`) + LLVM (`llvm/mod.rs:3475-3489`) `FaultCheck` emits already compute
   both sub-predicates as an `||` — DECOMPOSE that `||` into the two kinds (symmetric in both
   backends — verify the LLVM `and`/`or` decomposition matches C).
2. **GIR→LIR Div faultable arm emits TWO sequential checks/branches**: `flag_ovf =
   FaultCheck(DivOverflow)` → `Branch{flag_ovf → overflow_handler, else → next}`; in `next`:
   `flag_dz = FaultCheck(Div/Rem div0)` → `Branch{flag_dz → divzero_handler, else → cont}`;
   in `cont`: the committed `lhs/rhs` (both faults statically excluded).
3. **`fault_handler_for`** (`operators.rs:342-347`): Div/Rem returns a faultable form if
   EITHER `overflow_handler` OR `divzero_handler` is set (so `catch Fault.Overflow:` on
   `INT_MIN/-1` catches).
4. ⚠ **PARTIAL-CATCH (the sharp edge):** when a Div is in a scope catching only ONE of its
   two faults, the OTHER condition must still PANIC-by-default. The uncaught condition's
   branch targets the panic path (re-emit its trapping check / `commit_op` `insts.rs:163`),
   not a handler. So `catch Fault.DivByZero:` on `INT_MIN/-1` still panics, and `catch
   Fault.Overflow:` on `10/0` still panics. VERIFY the uncaught condition is checked exactly
   once (not double-checked, not silently wrapped).
5. **Rem too**: `INT_MIN % -1` is the same overflow (the emits include the MIN/-1 term for Rem).
6. **Lint**: update `fault_op_lowering_arms_count` (`tests/lints.rs:2351-2355`) for the new
   `FaultOp` variant.

### (D) Validate the fault-catch enum qualifier  [TINY / LOW]
`(big*2) catch Bogus.Overflow:` is silently accepted as `Fault.Overflow` — the parser
(`src/parser/expr.rs:1116-1118`) discards the qualifier prefix, keeping only the variant in
`FaultCatchPattern::Variant(variant)` (`src/parser/ast.rs:756`); `check_fault_variant`
(`src/semantic/typecheck.rs:3114`) validates only the variant name.
- Change `FaultCatchPattern::Variant` to carry the qualifier: `Variant { qualifier:
  Spanned<String>, variant: Spanned<String> }`. Store `enum_or_name` as the qualifier at
  parse (`expr.rs:1116`). In typecheck (before `check_fault_variant`, `typecheck.rs:3083-3086`):
  if `qualifier.node != "Fault"`, emit a clear diagnostic at the qualifier span.
- Update the mechanical readers for the new shape: `resolve.rs:1853-1861` and the GIR-lowering
  match on `FaultCatchPattern::Variant` (`exprs/mod.rs:3514`).

### (B) `--overflow=wrap` — ALREADY WORKS; lock-in fixture only  [TINY / NEAR-ZERO]
⚠ **The spec §11.2/§11.7 + TODO premise that this is a gap is REFUTED (scout, by running):**
`(big*2) catch Fault.Overflow: -1` already yields `-1` under `--overflow=wrap` (handler fires)
because `FaultCheck` codegen is unconditionally `__builtin_*_overflow` and `fault_handler_for`
never reads `overflow_wrap`. **Do NOT add override plumbing.** Add ONE lock-in fixture built
under `--overflow=wrap`. If `run_gg` can't pass build flags, add a `run_gg_with_flags` helper
(`tests/integration.rs:~5649-5684`). Correct the spec §11.2/§11.7 + the error-model TODO entry
to "already correct (orthogonal to `overflow_wrap`); documented, not implemented."

## 2. Out of scope (deferred, own briefs)
- `Fault equip Error` / `dyn Error` unified surface — Phase 2.
- OOM — Phase 2. Deep/boundary catch + unwinding — Phase 2.
- The doc rewrite (`language-design.md`/`book`/`reference`) + the §1/§4 two-channels reframe —
  own brief + confirming review. (This brief only corrects the §11.2/§11.7 (B) premise.)
- Self-host fast-follow (§11.8).
- dict-get / string-index / range-slice Bounds — future (no shared `safe_*` path).

## 3. Staging WITHIN the worktree (build GREEN at each step)
1. **(A) Bounds** FIRST — it changes `FaultScope`'s shape (adds `bounds_handler`); the new GIR
   variant + both backends + handler-entry + register `Bounds`. Build + `cargo test --lib`.
2. **(C) Div-split** — the `FaultOp` split + two-branch lowering + partial-catch + both backends
   + lint. Build + `cargo test --lib` + `cargo test --test lints`.
3. **(D) Qualifier** — AST/parse/typecheck. Build + `cargo test --lib`.
4. **(B) wrap-fixture** + the spec/TODO premise correction.
5. **Fixtures** (§4) — all of them, on BOTH backends.

## 4. Test plan (executor runs; parent runs the full sweep)
New fixtures (deterministic stdout), each on default AND `GG_BACKEND=llvm`:
- `fault_catch_bounds.gg` — `(xs[10]) catch Fault.Bounds: -1` on a len-3 Vector → `-1`; an
  in-bounds read → the element; binding `catch f: match f` reads `Fault.Bounds()`.
- `fault_catch_bounds_negidx.gg` — `(xs[-1]) catch Fault.Bounds: -1` → caught (documents the
  in-catch signed-index behaviour; a negative index is a catchable `Bounds` INSIDE a catch and
  a panic OUTSIDE — the §11.6 caveat).
- `fault_bounds_panic_default.gg` — an UNCAUGHT `xs[10]` still panics `index out of bounds`
  `exit(1)` (regression guard, unchanged).
- `fault_catch_intmin_div.gg` — `(INT_MIN/-1) catch f: match f` reads `Fault.Overflow()`;
  `(INT_MIN/-1) catch Fault.Overflow:` caught; `(10/0) catch Fault.DivByZero:` still caught;
  `(INT_MIN % -1) catch Fault.Overflow:` caught.
- `fault_intmin_partial.gg` — `(INT_MIN/-1) catch Fault.DivByZero:` does NOT catch → panics;
  `(10/0) catch Fault.Overflow:` does NOT catch → panics (the partial-catch guard).
- `fault_catch_overflow_wrap.gg` — built under `--overflow=wrap`: handler fires where the same
  op wraps outside the catch.
- `fault_catch_bad_qualifier.gg` — `(big*2) catch Bogus.Overflow:` → typecheck error (negative
  fixture).
Executor runs: `cargo build`, `cargo test --lib`, `cargo test --test lints`, and these fixtures
on BOTH backends. NOT the full integration sweep (parent's job).

## 5. Constraints (NON-NEGOTIABLE)
- **Worktree:** `pwd` + `git rev-parse --show-toplevel` FIRST; confirm INSIDE your worktree,
  NEVER `/workspace/gorget-1`. First action `git merge --ff-only gorget-1` (gets Increment 1).
  Do NOT `cd` to `/workspace/gorget-1` or use its absolute paths.
- **Stage by filename:** `git add <specific files>` ONLY — NEVER `git add -a`/`.`/`commit -a`.
  Small commits per stage are fine.
- **Both backends at parity** — every new fixture passes on default AND `GG_BACKEND=llvm`.
- **Do NOT touch:** self-host (`tests/fixtures/self_host_*/`); the `Result`-`catch` path; the
  existing `FaultableBinOp`/Overflow+DivByZero Increment-1 behaviour except the (C) split; the
  non-array index paths; `docs/` EXCEPT the §11.2/§11.7 (B)-premise correction in
  `docs/plans/error-model.md` and the error-model TODO entry.
- **No name-matching for semantics** — fault routing is typed metadata on the GIR/LIR inst /
  the `FaultScope` fields, never a string check.
- **Drop-correctness:** the Bounds handler branch lives in GIR/LIR CFG so drop passes run over
  it; a `fault_catch_bounds` with a Drop-bearing element/temporary should be ASan-clean.
- **Both new faultable shapes (`FaultableIndexLoad`, the split `FaultOp`) go through the
  shared path + the arm-count lint** — fix the class, not the instance.
- Report back: per-stage diff summary, which fixtures pass on which backend, `cargo test --lib`
  + `--test lints` results, and any deviation + why. Do NOT wait for the full sweep. If the
  design doesn't hold against source, STOP and report rather than improvising out-of-scope.

## 6. Acceptance criteria (parent verifies at output-review)
- `cargo build` + `cargo test --lib` + `cargo test --test lints` green.
- All §4 fixtures pass on BOTH backends — incl. the partial-catch guard and the negative
  qualifier fixture.
- `Fault.Bounds` works on arrays (array-only; dict/string/range untouched); negative-index
  in-catch documented; borrowed-element/str-ptr/resource provenance preserved (no regression
  to existing index reads).
- `INT_MIN/-1` → `Fault.Overflow`; `div0` → `Fault.DivByZero`; partial-catch panics correctly;
  Rem handled.
- `Bogus.Overflow` rejected at typecheck; `Fault.Overflow`/`Fault.DivByZero`/`Fault.Bounds`
  accepted.
- (B): the wrap fixture passes; the spec §11.2/§11.7 + TODO premise corrected to "already works".
- Shared LIR/GIR shapes (one representation, both emitters derive); arm-count lint extended.
- Panic-by-default unchanged for uncaught faults; `bootstrap_fixed_point` + `self_host_*`
  untouched; Result-`catch` unperturbed.
- Diff is the scoped slice only — no equip Error / OOM / deep-catch / doc-rewrite / self-host creep.
