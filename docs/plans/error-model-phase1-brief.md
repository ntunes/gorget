# Implementation Brief — Error Model Phase 1, Increment 1 (fault-catch core, both backends)

> **Status: DRAFT brief for review (2026-06-21).** Spec: `docs/plans/error-model.md`
> §11 (READ IT FIRST — every mechanism + `file:line` anchor is there; this brief
> scopes + sequences, it does not restate). This brief covers **Increment 1** only;
> deferred items have their own briefs (§ "Out of scope" below).

## 0. The one-line goal
Make `int r = (a * b) catch Fault.Overflow: fallback` and `… catch Fault.DivByZero: …` WORK on
**both** the C and LLVM backends: a faultable op that overflows/divides-by-zero
branches to a local handler block (same function, no unwinding) instead of
`exit(1)`; uncaught faults still panic exactly as today.

## 1. Scope — Increment 1 (IN)
1. **`Fault` enum** (compiler-internal, no generics, variants `Overflow`,
   `DivByZero` ONLY this increment). Register it alongside `Option`/`Result` via the
   built-in-enum injection — but register at **BOTH layers (pass 2)**: **(1) semantic**
   `src/semantic/resolve.rs:152-166` (where `Option`/`Result` are defined as
   `DefKind::Enum` with variants populated into `ctx.enum_variants`) — REQUIRED so
   name-resolution + typecheck + the panic-default rule resolve `Fault.Overflow`
   BEFORE IR lowering; **(2) IR-lowering** `src/ir/lowering/generics/substitute.rs:317`
   (`inject_builtin_enums`, the corrected path — feeds `enum_templates`). Direct
   precedent at both; `Fault` is a free name (no collision). Omitting (1) → `Fault.Overflow`
   unresolved in typecheck. **NO `equip Error` this
   increment** (deferred → §2, review pass 1): Increment 1 binds a *concrete* `Fault`
   and matches on it; nothing calls `.display()`/`.debug()`/`.source()`, so the `Error`
   impl is pure Phase-2 (`dyn Error`) surface AND sits on an unsolved built-in-`equip`
   registration problem (`Error` lives in `lib/std/io.gg:223`, not the prelude; there is
   no machinery to inject an `equip` block for a built-in type). Keep it out.
2. **The NEW shared LIR "checked-op-with-handler-branch" shape** — the §11.2 central
   item. A checked faultable op (`IAdd`/`ISub`/`IMul` overflow; `IDiv`/`IRem` zero)
   whose fault outcome is a real **`Term::Branch`** (a block terminator — `lir/mod.rs:1218`,
   NOT an `Inst`; the brief earlier mis-named it `Inst::Branch`) to a handler basic block,
   built in GIR/LIR CFG (NOT a C-emit goto), so drop-insertion/elaboration see it. The
   `lower_catch_expr` `ok_bb`/`err_bb`/`merge_bb` shape (`src/ir/lowering/exprs/mod.rs:3338`)
   is the CFG **template** — ⚠ but NOT a clean reuse (pass 2): `lower_catch_expr` branches
   on a *materialized Result tag* (`tag_of`→`cmp`→`branch`); the fault op must instead
   **output an overflow/zero FLAG and SPLIT the block at the op** (an `Inst` is mid-block,
   a `Term` ends it — they can't be one node). **No existing LIR op outputs such a flag** —
   this is the genuinely new piece (spec §11.2's "single largest item"); don't let the
   template framing imply it's reuse.
   - ⚠ **GIR→LIR handoff (pass 2 — name it):** the catch-scope ("which ops are the
     left-operand") is known at GIR/IR-lowering, but the flag+branch is emitted at
     LIR-lowering (`lower_binop`, `src/lir/lower/calls.rs:81`, which today takes only a
     single `bool overflow_wrap`). Thread the "this op faults to handler-bb N" as NEW
     TYPED METADATA on the GIR op (layering-discipline), pushed for the WHOLE left-operand
     subtree and **CLEARED at any `Call`/`CallExtern` boundary** (so callee faults stay
     deep). The `suppress_auto_prop` one-shot (`context.rs:260`) is a precedent for
     context-threading but is consumed at `lower_expr` entry — the fault-scope needs a
     scoped push/pop that survives the subtree, not a one-shot. Specify this mechanism.
   ⚠ **(pass 1) `IDiv`/`IRem` have NO `overflow` field and are checked UNCONDITIONALLY**
   (`c_lir/mod.rs:2467`, `llvm/mod.rs:3365`) — do NOT gate `DivByZero` on `Overflow::Trap`;
   it is well-defined in BOTH checked and wrap builds. Only `Overflow` (Add/Sub/Mul)
   keys off the `Trap`/`Wrap` field.
3. **BOTH backends emit it** — C (`src/backend/c_lir/`) AND LLVM
   (`src/backend/llvm/`). Required together: a fixture must pass on the default run
   AND `GG_BACKEND=llvm`, or the LLVM sweep regresses. (LLVM emit is already
   branch-shaped per §11.2 note; C is flat today — both derive the branch from the
   shared LIR.)
4. **Fault-catch AST + grammar + typecheck — a NEW form**, distinct from the
   `Result` `catch` (which is welded to `Result[T,E]`: parser `expr.rs:1072`, AST
   `ast.rs:585` `error_binding: Spanned<String>`, typecheck `typecheck.rs:3047`).
   Do NOT perturb the existing contract-`catch` path. Support BOTH spellings:
   - pattern form `(expr) catch Fault.Overflow: fallback` (no value bound);
   - binding form `(expr) catch f: match f` where `f` binds a concrete `Fault`.
5. **Handler-bb constructs the `Fault` value** for the binding form — materialize
   `Fault.Overflow()`/`Fault.DivByZero()` at the handler entry via `EnumInit`
   (`exprs/mod.rs:1907`) and bind it. Pattern form needs no constructed value. (§11.2.)
6. **Exhaustiveness via implicit-panic-default** over the closed `Fault` enum: a
   fault match may omit variants → they panic; keyed on the `Fault` enum only at
   `typecheck.rs:3640-3666`, leaving every other enum strictly exhaustive. (§11.1.5.)
7. **Lexical reach** = faultable ops emitted DIRECTLY into the wrapped expression's
   own basic blocks, NOT through any `Call`/`CallExtern`. A fault inside a called
   function (incl. an inline closure invoked via a call) is deep → still panics.
   (§11.5 — adopt the basic-block definition.)
8. **Panic-by-default preserved** — outside a fault `catch`, overflow/div0 panic via
   `exit(1)` exactly as today; plain `int sum(...)` stays `int`, no signature change.
9. **Fixtures** (§11.6, the Increment-1 subset) — see §4 below.

## 1.5 Decisions this brief SETTLES (the §11.5 questions the spec delegated — pass 1)
- **`equip Error` → DEFERRED to Phase 2.** (§1 item 1.) Not needed for any Increment-1
  fixture; no built-in-`equip` precedent; `Error` not in the prelude. Phase 2 (the
  `dyn Error` unified surface) owns it.
- **Catch grammar / parens → NO parens required; reach = the left-operand expression.**
  `catch` is the lowest-BP infix (`expr.rs:771`), so `a*b catch Fault.Overflow: …` already
  binds as `(a*b) catch …` — consistent with the existing `expr catch (e):` (no parens
  required there either). Lexical reach (§11.5) = the faultable ops emitted directly
  into the **left-operand expression's own basic blocks**, not through any
  `Call`/`CallExtern`. Parens are allowed for grouping but not required.
- **Variant spelling → QUALIFIED `Fault.Overflow` (pass 2), not bare `Overflow`.**
  Per the language's qualified-variant rule (`Color.Red()`, not `Red()`; only the
  prelude variants `Ok`/`Some`/`None`/`Error` are bare). Do NOT make `Fault`'s variants
  prelude-bare. So: `(a*b) catch Fault.Overflow: handler`.
- **Parser disambiguation (pass 2) → ONE new production, three cases by what follows
  `catch`:** `(` → the EXISTING `Result` `catch (name):` (untouched); a qualified path
  `Fault.Overflow` → fault PATTERN (catch that variant); a bare ident → fault BINDING
  (`catch f: <body matching f>`). The existing arm hard-expects `(` (`expr.rs:1072`), so
  the leading token disambiguates cleanly (LL(1)). The pattern/binding distinction is
  **semantic**, not two parser forms. **REQUIRED fixture: a parse guard proving
  `catch (e):` (Result) and `catch Fault.Overflow:` (fault) coexist unambiguously.**
  (Syntax is provisional pending the doc/§1-§4 reframe phase; the executor confirms it
  parses without conflict.)
- **`meta` / const-eval → COMPILE ERROR this increment.** `meta` arithmetic wraps
  silently (`meta.rs:1278-1280`), so there is no runtime fault to catch; a fault-`catch`
  in a `meta`/const-eval context is rejected with a clear diagnostic (do NOT silently
  accept). Revisit if `meta` ever gains checked arithmetic.

## 2. Out of scope — Increment 1 (deferred, own briefs)
- **`Fault equip Error`** (the three `Error` methods + the `dyn Error` surface) —
  Phase 2 (where matching faults + contract errors in ONE handler actually needs it).
- **`Fault.Bounds`** (via `gorget_array_safe_get` + inline NULL-branch) — Increment 2.
- **`Fault.OutOfMemory`** — Phase 2 (deep).
- **The per-expr `--overflow=wrap` force-checked override** (§11.2/§11.7). Increment 1
  works in the DEFAULT (`--overflow=checked`/Trap) build, where ops are already
  checked. Under `--overflow=wrap` a `catch Overflow` is a no-op for now — acceptable;
  the override is Increment 2. **Do NOT** add the "checked-under-wrap" fixture yet.
- **Doc rewrite** (`language-design.md` §2.2/§6, `book/10-errors.md`,
  `language-reference.md` grammar/`Fault` ref) + the **§1/§4 reframe** — own brief,
  own confirming review (§11.4).
- **Self-host fast-follow** (§11.8) — separate parity-chain item; the self-host's own
  source must NOT change this increment (keeps `bootstrap_fixed_point` green).
- **Phase 2** (deep/boundary catch, unwinding) entirely.

## 3. Staging WITHIN the executor's worktree (build green at each step)
Per §11.7 — produce a compiling tree at each step; do not leave it broken between:
1. **Shared LIR shape** — add the checked-op-with-handler-branch representation
   (e.g. an `Overflow::Branch(bb)`-style outcome / a fault-op variant carrying a
   handler-bb target). Build.
2. **C backend** emits it (re-point the trap to the handler bb; result committed only
   on the OK path). Build.
3. **AST + grammar + typecheck** for the new fault-catch form + the `Fault` enum +
   the panic-default match rule (NO `equip Error` — deferred, §2). Build + `cargo test --lib`.
4. **LLVM backend** emits the same shared shape. Build (+ a quick `GG_BACKEND=llvm`
   smoke on one fixture).
5. **Fixtures** (§4). Run targeted integration on BOTH backends.

## 4. Test plan (the executor runs these; parent runs the full sweep)
New fixtures under `tests/fixtures/` (deterministic stdout), each passing on the
default AND `GG_BACKEND=llvm` runs:
- `fault_catch_overflow.gg` — `(a*b) catch Fault.Overflow: <fallback>` yields the fallback;
  prints a deterministic line.
- `fault_catch_div0.gg` — `(a/b) catch Fault.DivByZero: <fallback>`.
- `fault_catch_binding.gg` — `… catch f: match f: case Fault.Overflow: … else: …`
  reads the right variant.
- `fault_catch_compound.gg` — `(a*b + c/d) catch Fault.Overflow: …` catches the right op.
- `fault_panic_default.gg` — an UNCAUGHT overflow still panics `exit(1)` (assert via
  the harness's crash/exit path, mirroring the existing overflow-panic fixtures).
- `fault_catch_contract_unchanged.gg` — a regression guard: an existing `Result`
  `catch (e):` still behaves identically (the new form didn't perturb it).
- `fault_catch_drop.gg` — **REQUIRED, not optional (pass 1):** a faultable op whose
  operand involves a `Drop`-bearing temporary (e.g. `(makeDroppable().n * k) catch
  Overflow: …`), asserting via output (and ideally an ASan run) that the live owned
  temporary is dropped exactly once on the handler path — the single subtlest
  correctness property; do NOT substitute a manual-only check.
- **`tests/lints.rs` arm-count ratchet — REQUIRED (pass 1, sibling-site drift):** a
  lint that forces any new faultable LIR op through the shared checked-op-with-branch
  path (template: `container_literal_arms_count`, `tests/lints.rs:707`), so the next
  fault op can't silently skip it. (Runtime-snapshot lock-in, §11.6 — deferred to the
  follow-up increment; state it, don't add now.)
Executor runs: `cargo build`, `cargo test --lib`, `cargo test --test lints`, and
`cargo test --test integration -- <these fixtures>` on BOTH backends. Executor does
NOT run the full integration sweep (that's the parent's job).

## 5. Constraints (NON-NEGOTIABLE)
- **Worktree:** run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm INSIDE your
  worktree, NEVER `/workspace/gorget-1`. Do NOT `cd` there or use its absolute paths.
  First action: `git merge --ff-only gorget-1` (gets the §11 spec + this brief).
- **Staging:** `git add <specific files>` ONLY — NEVER `git add -a`/`.`/`commit -a`.
- **Both backends at parity** — every new fixture passes on default AND `GG_BACKEND=llvm`.
- **Do NOT touch:** the self-host fixtures (`tests/fixtures/self_host_*/`); the
  existing `Result`-`catch` parser/AST/typecheck/lower path (add a DISTINCT form);
  `main`/the overflow-mode global default; `docs/` (doc rewrite is a separate brief).
- **No name-matching for semantics** (CLAUDE.md): the fault-op routing is typed
  metadata on the LIR inst, not a string check.
- **Drop-correctness:** the handler branch lives in GIR/LIR CFG so the drop passes
  run over it; verify a `(struct_with_drop.method() * k) catch Fault.Overflow: …` shape
  doesn't leak (a fixture or a manual ASan check).
- Report back: the diff summary, which fixtures pass on which backend, `cargo test
  --lib` result, and anything you had to deviate from this brief + why. Do NOT wait
  for the full integration sweep.

## 6. Acceptance criteria (parent verifies at output-review)
- `cargo build` + `cargo test --lib` + `cargo test --test lints` green.
- All §4 fixtures pass on BOTH backends — including `fault_catch_drop.gg`
  (handler-path drop-correctness) and the new arm-count lint.
- No regression in the existing `Result`-`catch` fixtures.
- The new LIR shape is shared (one representation, both emitters derive from it) — not
  a C-emit goto, not duplicated per-backend logic.
- Panic-by-default unchanged (uncaught fault → `exit(1)`).
- `bootstrap_fixed_point` + `self_host_*` untouched (self-host source unchanged).
- Diff is the scoped slice only — **no `equip Error`**, no Bounds/OOM/override/doc/
  self-host creep. (`equip Error` is explicitly Phase 2, §2.)
- The two §1.5 rulings honored: no-parens grammar with left-operand reach; fault-`catch`
  in `meta` is a compile error.
