# Error-model Inc-2.1b scout — LLVM fault-slot adapter parity with C

**Status:** DESIGN/FEASIBILITY SCOUT, 2026-06-24, on `gorget-1` tip `57b44418` (verified `git rev-parse HEAD`).
All four premises CONFIRMED against current source. The minimal fix was PROTOTYPED in this worktree,
built, and RUN end-to-end under `GG_BACKEND=llvm` — it flips `fault_deep_fnvalue_panic` from
*incidentally-passing UB* to *deterministic-correct*. Throwaway prototype (NOT committed/pushed; only
this doc is committed). Grounds in `docs/plans/error-model-inc21-scout.md`, `docs/plans/error-model.md`
§11, `docs/devbook/24-layering-discipline.md` (typed-metadata rule), and the landed C fix `a4ed04ee`.

---

## 0. VERDICT — 2.1b is "adapter + un-gate + verify", NOT a signature-append

**Re-scoped (the scout `error-model-inc21-scout.md` §4 guessed "append the `i32*` param in the LLVM
signature build"; that guess is WRONG for the landed shape).** The direct-call fault path ALREADY works
under LLVM — the call-site slot/branch is shared LIR, emitted identically for both backends. The ONLY
LLVM gap is the **first-class/closure adapter** (`__adapt_*`), which forwards the synthesized trailing
fault-slot as a phantom param exactly as the C adapter did before `a4ed04ee`.

**The fix is a single ~14-line edit in one function** (`src/backend/llvm/mod.rs`, the `__adapt_*` emit),
mirroring the C adapter-gen, gated on the already-existing typed `LirFunction.fault_slot_param_count`.
No new field, no signature plumbing, no GIR/LIR change. Measured: build green, `cargo test --lib` green
(debug), all 29 fault integration tests green under LLVM with the fix + the un-gate.

---

## 1. Premises — ALL CONFIRMED (file:line, this session, tip `57b44418`)

### P1 — the C adapter fix (the template to mirror) ✅

`src/backend/c_lir/mod.rs:916-952` (the `for fid_raw in &adapter_fids` loop). It:
- computes `let user_param_count = target.params.len().saturating_sub(target.fault_slot_param_count);`
  (`:930`),
- declares the adapter over USER params only: `target.params.iter().take(user_param_count)` (`:933`),
- forwards `__p{i}` for `i < user_param_count`, else `NULL` for the trailing slot(s) (`:942-949`).

Emits `T __adapt_fn(void* __env, i64 __p0, i64 __p1) { return faulty(__p0, __p1, NULL); }` for a
participating fn. CONFIRMED it declares only user params and passes `NULL` for the trailing slot.

### P2 — the typed gate exists and is readable from the LLVM path ✅

- `LirFunction.fault_slot_param_count: usize` — `src/lir/mod.rs:1459` (field), `:1479` (default 0 in
  `LirFunction::new`).
- Set at GIR→LIR: `src/lir/lower/mod.rs:1501` —
  `lir_func.fault_slot_param_count = if gir_func.participates_in_fault { 1 } else { 0 };`
- Sourced from GIR `Function.participates_in_fault` — `src/ir/mod.rs:409` (field), set at the source in
  `lower_function` (`src/ir/lowering/functions.rs:1145`: `func.participates_in_fault =
  fault_return_bb.is_some();`).
- The LLVM adapter loop already binds `let target = &module.functions[*fid_raw as usize];`
  (`src/backend/llvm/mod.rs:2196`), where `target: &LirFunction` — so `target.fault_slot_param_count`
  is directly readable, EXACTLY as the C loop reads it. No plumbing needed.

This is the correct devbook/24-rule-2 shape: a typed flag set at the source, propagated as a typed
field, read via the field — no name/shape-matching.

### P3 — the LLVM adapter site DOES forward the phantom slot (the bug) ✅ — exact cite

`src/backend/llvm/mod.rs:2195-2238` (the `for fid_raw in &adapter_fids` loop). The buggy line is `:2209`:

```rust
for (i, p) in target.params.iter().enumerate() {     // ← ALL params, incl. the trailing slot
    let ty = llvm_arg_type(p, snames);
    param_decls.push(format!("{ty} %a.p{i}"));        // declares %a.p2 (the slot)
    param_names.push(format!("{ty} %a.p{i}"));        // forwards %a.p2 to @faulty
}
```

It iterates ALL `target.params` (never reads `fault_slot_param_count`), so for a participating
3-arg `faulty(i64, i64, ptr)` it declares the adapter as `(ptr env, i64, i64, ptr %a.p2)` and forwards
`%a.p2`. PROVEN in the generated IR (un-fixed build, `/tmp/inc21b_scout/fdfp_llvm.ll:78-80`):

```llvm
define i64 @__adapt_faulty(ptr %a.env, i64 %a.p0, i64 %a.p1, ptr %a.p2) {
  %a.r = call i64 @faulty(i64 %a.p0, i64 %a.p1, ptr %a.p2)   ; ← %a.p2 forwarded
  ret i64 %a.r
}
```

The adapter is stored as a callable (`store ptr @__adapt_faulty`, `:214/:242/:261`) and INVOKED through
the 2-arg callable ABI — `call i64 %cc.X.X.fnp(ptr env, i64, i64)` (`:152` and `:225`), supplying only
env + 2 user args. So `%a.p2` is **whatever junk is in the 3rd integer-arg register** at the indirect
call site (a UB call through an incompatible signature). On overflow, `@faulty`'s fault arm
(`fdfp_llvm.ll:bb1`/`bb2`, `:105-115`) checks `%p2 != NULL` and, if junk is non-NULL, **stores `i32 1`
through it** (`:114`: `store i32 %v13, ptr %v15`) — the wild write. It panics today ONLY because the
register happens to be 0 on this schedule (CONFIRMED by run — see §3).

### P4 — the fixture gate ✅

`tests/integration.rs:5852` `fn fault_deep_fnvalue_panic()` contains `if skip_under_llvm() { return; }`
at `:5858`. `skip_under_llvm()` is `tests/integration.rs:63` (`matches!(gg_backend().as_deref(),
Some("llvm"))`). It is the ONLY fault fixture gated C-only — confirmed via
`grep -n "fault_" tests/integration.rs` cross-referenced with `skip_under_llvm` (the other 28 fault
tests run unconditionally on both backends).

---

## 2. The minimal fix (PROTOTYPED — exact diff)

Mirror the C adapter-gen in the LLVM `__adapt_*` loop, gated on `target.fault_slot_param_count`.
Declare only user params; append `null` to the FORWARD list for each trailing slot. (`llvm_arg_type`
maps `MutPtr<i32>`/`Void` → `ptr` at `src/backend/llvm/mod.rs:93-98`, so `ptr null` is well-typed.)

```diff
--- a/src/backend/llvm/mod.rs
+++ b/src/backend/llvm/mod.rs
@@ around :2208
         param_decls.push("ptr %a.env".to_string()); // env (ignored)
-        for (i, p) in target.params.iter().enumerate() {
+        // Cross-frame fault (Inc-2.1b): a PARTICIPATING fn has synthesized trailing
+        // `MutPtr<i32>` fault-slot param(s) that are NOT part of its callable type.
+        // The adapter is invoked through the 2-arg callable ABI, so it must declare
+        // ONLY the user params and pass `null` for the trailing slot(s) — forwarding
+        // a phantom slot arg writes a fault tag through a wild pointer (memory
+        // corruption / UB call-signature mismatch). `null` makes the callee's fault
+        // arm panic inline = panic-by-default for an indirectly-invoked fault
+        // (indirect propagation is deferred to 2.3b). Typed count off the LIR
+        // function, never name/shape-matched (devbook/24 rule 2). Mirrors the C
+        // adapter (src/backend/c_lir/mod.rs).
+        let user_param_count = target.params.len().saturating_sub(target.fault_slot_param_count);
+        for (i, p) in target.params.iter().take(user_param_count).enumerate() {
             let ty = llvm_arg_type(p, snames);
             param_decls.push(format!("{ty} %a.p{i}"));
             param_names.push(format!("{ty} %a.p{i}"));
         }
+        // Append `null` for each synthesized trailing fault-slot param (panic-by-default).
+        for p in target.params.iter().skip(user_param_count) {
+            let ty = llvm_arg_type(p, snames);
+            param_names.push(format!("{ty} null"));
+        }
```

This leaves `param_decls` (the adapter's OWN signature) over the user params + env, and pushes
`ptr null` into `param_names` (the forward-arg list) for each trailing slot. `target_uses_sret` is
orthogonal — the sret pointer is prepended to BOTH `param_decls` and `fwd_args` separately (`:2205-2222`),
and a participating fn returns a scalar (the fault is the side-channel), so the sret path is unaffected.

**Post-fix IR** (`/tmp/inc21b_scout/fdfp_llvm_fixed.ll:78-81`):

```llvm
define i64 @__adapt_faulty(ptr %a.env, i64 %a.p0, i64 %a.p1) {
  %a.r = call i64 @faulty(i64 %a.p0, i64 %a.p1, ptr null)
  ret i64 %a.r
}
```

The adapter now declares the 2-arg callable ABI exactly and forwards `ptr null` for the slot — byte-for-
byte the C shape. `@faulty`'s fault arm now sees `%p2 == NULL` and takes the panic path.

**Sibling-site check (devbook/24 rule 4 — fix the class, not the instance):** the `__adapt_*` emit loop
collects `adapter_fids` from BOTH `Inst::FuncAddr` (`:2186`) AND `Inst::ClosurePack { needs_adapter:
true }` (`:2189`). Both wrap a fn through the SAME adapter generated by this one loop. `ClosurePack`
lowering (`:6722`) only stores `@__adapt_*` as the fn-ptr; `CallClosure` (`:6741`) invokes via the 2-arg
ABI. So **the single fix at the adapter-gen loop covers every indirect-invocation path** — there is no
second forwarding site to patch. (The C side is the same: one `__adapt_*` loop.)

---

## 3. MEASURED end-to-end (build + run + diff)

All under `GG_BACKEND=llvm`, release, in this worktree. Logs in `/tmp/llvm-fault-*.log`,
IR + binaries in `/tmp/inc21b_scout/`.

**Fault fixtures + gates** (`grep "fault_" tests/integration.rs`): 28 fault tests run on BOTH backends;
`fault_deep_fnvalue_panic` is the SOLE `skip_under_llvm()`-gated one (`:5858`). The direct-call deep
fixtures (`fault_deep_catch`, `fault_deep_catch_drop`, `fault_deep_uncaught_panic`) are NOT gated.

**Direct-call fault fixtures ALREADY pass under LLVM (TODO claim CONFIRMED).** Baseline run (un-fixed
LLVM, fixture still gated): all 29 fault tests pass; `fault_deep_catch`, `fault_deep_catch_drop`,
`fault_deep_uncaught_panic` genuinely pass (shared-LIR slot/branch). `fault_deep_fnvalue_panic` "passes"
only because the gate early-returns. → **2.1b is the adapter + un-gate, NOT a signature-append.**

**The bug manifests (incidental-pass CONFIRMED).** Standalone un-fixed LLVM binary
(`/tmp/inc21b_scout/fdfp_llvm`):

```
fault_deep_fnvalue_panic.gg:12:31: integer overflow
-1
42
72
(exit 1)
```

It panics — but ONLY because the 3rd int-arg register holds 0 (NULL) on this schedule. The IR (P3)
proves a different schedule (or optimizer pass / register pressure) leaves junk there → wild `store i32 1`.
This is exactly the latent UB the C-only gate was protecting against. (No clang in this sandbox to ASan-
confirm the wild write directly, but the IR + the C-side `a4ed04ee` ASan global-buffer-overflow report
are conclusive; `llc` IS present and the LLVM backend builds/runs.)

**The prototype flips it to deterministic-correct.** Fixed LLVM binary
(`/tmp/inc21b_scout/fdfp_llvm_fixed`):

```
fault_deep_fnvalue_panic.gg:12:31: integer overflow
-1
42
72
(exit 1)
```

Same observable output, but now BY CONSTRUCTION (the adapter passes `ptr null` → callee's panic arm
fires deterministically), not by register luck. The happy-path indirect calls (`fp(6,7)→42`,
`apply(faulty,8,9)→72`) and the direct catch (`-1`) all return correctly — the NULL slot is benign on the
no-fault path. Matches the fixture's `run_gg_panics_with_stdout("...", "-1\n42\n72", "integer overflow")`.

**Through the harness (fix + un-gate applied).** `GG_BACKEND=llvm ... cargo test fault_deep`:
`fault_deep_fnvalue_panic`, `fault_deep_uncaught_panic`, `fault_deep_catch`, `fault_deep_catch_drop` →
**4 passed; 0 failed**. Full LLVM fault suite (`fault_`): **29 passed; 0 failed**. C backend (default)
fault suite: **29 passed; 0 failed** (zero regression — non-participating fns have
`fault_slot_param_count == 0`, so `user_param_count == params.len()`, forwarding ALL params unchanged,
no `null` appended).

**Gates.** `cargo build --release` green. `cargo test --lib` green in DEBUG (the C-side commit
`a4ed04ee` reported "1084/0" in debug). ⚠ `cargo test --lib --release` reports 2 failures —
`lir::validate::tests::assert_module_valid_includes_validator_error` and
`...assert_module_valid_panics_with_pass_name` — both `should_panic` tests gated on `debug_assert!`
(compiled out in release). **CONFIRMED PRE-EXISTING** (reproduced after `git stash` on clean
`57b44418`; pass in debug). NOT caused by this change; the executor can ignore them but should run
`cargo test --lib` in debug, not release.

---

## 4. Re-scoped effort estimate

**2.1b = "adapter fix + un-gate + verify."** Concretely:
1. The ~14-line `src/backend/llvm/mod.rs` edit in §2 (one function, the `__adapt_*` loop). Done +
   prototyped.
2. Un-gate `fault_deep_fnvalue_panic` (remove `if skip_under_llvm() { return; }`, `:5858`, and update
   the now-stale "C-ONLY (2.1a)" comment `:5853-5857` to "BOTH backends (2.1b)").
3. Verify: `GG_BACKEND=llvm` fault suite + a full LLVM integration sweep (orchestrator's job) for
   regression. `cargo build` + `cargo test --lib` (debug) + the C fault suite.

**NOT a signature-append** (the scout `error-model-inc21-scout.md` §4 / §1.5's "append the i32* param in
the LLVM signature build" is moot — the participating callee's signature ALREADY carries the slot via
the shared synthesized GIR param, and the direct call site ALREADY passes it; only the adapter wrapper
forwarded it wrong). Estimate: **half-day** including the full-sweep validation, dominated by the LLVM
integration sweep wall-time, not the code.

---

## 5. `skip_under_llvm()` gates to lift

Exactly ONE: `fault_deep_fnvalue_panic` (`tests/integration.rs:5858`). No other fault fixture is gated.
(Verified by grep; the other `skip_under_llvm()` call sites in the file are unrelated — concurrency/
optimizer-quirk gates documented at their own sites, NOT fault-related, and must stay.)

---

## 6. Blast-radius / concerns for the executor brief

- **Zero regression for ordinary fn-values.** A non-participating fn has `fault_slot_param_count == 0`
  → `user_param_count == params.len()` → the `take(...)` keeps all params and the `skip(...)` loop is
  empty → byte-identical to today's emission. (Confirmed: C-side `a4ed04ee` verified `add` as fn-ptr →
  7, via HOF → 30; the LLVM fix has the same algebraic identity.) The full LLVM fault suite + C suite
  green in this scout corroborate.
- **sret orthogonal.** The sret pointer is handled on its own branch (`:2205-2222`); a participating fn
  returns a scalar (fault is the side-channel), so the sret path is untouched. Still, the executor should
  keep an eye that no participating fn is also large-aggregate-returning (none today; if one ever is, the
  `null`-append still appends to `param_names` AFTER the user params, before nothing — correct).
- **One source of truth.** The fix reads `fault_slot_param_count` — the SAME field the C adapter reads.
  Do NOT introduce a second derivation (e.g. re-checking `participates_in_fault` or name-matching) in the
  LLVM path; that would split the source of truth (devbook/24 rule 3).
- **Single fix site.** Both `FuncAddr` and `ClosurePack(needs_adapter)` feed the one adapter-gen loop
  (§2 sibling check) — there is no second LLVM forwarding site. The executor should NOT need an
  arm-count lint here (it's one loop, not an enumerated set), but if a future indirect-call path emits
  its own forwarding it must read the same field.
- **Self-host: untouched.** Per the C-side commit, the self-host's own source keeps panic-on-overflow;
  `bootstrap_fixed_point` and `runtime_snapshots` are unaffected (new field defaults 0). The LLVM fix
  changes only emitted IR for participating fns taken as values — the self-host has none.
- **No `cargo test --lib --release` confusion.** Brief the executor to gate on `cargo test --lib` in
  debug (the 2 `should_panic`/`debug_assert` validate tests fail in release on the clean base — a
  pre-existing artifact, NOT introduced by this change).

---

## 7. Cite map

C template: `src/backend/c_lir/mod.rs:916-952` (fix `a4ed04ee`). LLVM bug site:
`src/backend/llvm/mod.rs:2195-2238` (the `__adapt_*` loop; buggy `:2209`). Typed field:
`src/lir/mod.rs:1459` / set `src/lir/lower/mod.rs:1501` / source `src/ir/mod.rs:409` +
`src/ir/lowering/functions.rs:1145`. `llvm_arg_type`: `src/backend/llvm/mod.rs:93-98`. Fixture +
gate: `tests/fixtures/fault_deep_fnvalue_panic.gg`, `tests/integration.rs:5852` / `:5858` / helper
`:63`. Adapter-fid collection: `src/backend/llvm/mod.rs:2186` (FuncAddr) / `:2189` (ClosurePack).
Docs: `docs/plans/error-model-inc21-scout.md` (prior scout — §4 sub-slicing superseded for 2.1b shape),
`docs/plans/error-model.md` §11, `docs/devbook/24-layering-discipline.md`.
