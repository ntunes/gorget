# SCOUT REPORT — T2b (bounds trap normalization, the LAST slice of D11)

Worktree `agent-a1c7ad91f58a0f564`. Read-only + end-to-end MEASURED. Two throwaway prototypes REVERTED; worktree + main clean. Status: COMPLETE.

## HEADLINE (read first)
1. **All THREE lanes emit the bounds trap through the SAME shared runtime helper `gorget_array_get` (runtime_array.c:31-37)** — Rust C, Rust LLVM, AND self-host all link `src/backend/c/runtime`. Baseline (MEASURED, all three): `gorget: panic: index out of bounds: index 5, length 3` + **exit 1**. This is structurally UNLIKE T2a (whose overflow was emitted INLINE per-backend, forcing a separate self-host reroute). **T2b's self-host lane flips for FREE from the runtime change — NO self-host `.gg` edit and NO driver rebuild needed** (MEASURED).
2. **The owner's real-location ruling is ACHIEVABLE and PROVEN end-to-end.** The span IS reachable at the bounds emit site (`block.span_map` already resolves `loc` for every `CallExtern`). Prototyped `gorget_array_get_at(arr, idx, code, file, line, col)` + a C-emit-boundary rewrite → `trap[T_Bounds]: index out of bounds: index 5, length 3 at .../trap_bounds.gg:18:13` + **exit 101**, and `spec_conformance_c` went **194→195 (MATCH=195 MISMATCH=0, zero regressions)**. No per-call-site ABI fan-out required.
3. **T2b is bigger than "flip the fixture" — there is a SECOND bounds emit path and a T2a REMNANT.** The Fault re-panic blocks (`exprs/mod.rs:3800`, `functions.rs:102`) emit `gorget_panic("index out of bounds")` → OLD `file:L:C: index out of bounds` + exit 1 (MEASURED: `fault_deep_bounds_swallow_guard` → `...:12:13: index out of bounds` exit 1). Their **overflow/divzero siblings survived T2a unchanged** (MEASURED: `fault_deep_mixed_divzero_only` → `...:20:13: integer overflow` exit 1; `fault_deep_uncaught_divzero_panic` → `...:6:26: division by zero` exit 1). Fix-the-class ⇒ T2b must normalize all three re-panic emits (closing a T2a sibling-gap).
4. **Fault-catch is untouched.** The catchable path uses `gorget_array_safe_get` (returns NULL, insts.rs:1249) — a separate helper. The real-location change to the panicking `gorget_array_get` path does NOT interact with `Fault.Bounds`.
5. **The abort()/134 sites are DEAD.** Rust `Inst::BoundsCheck/DivCheck/Trap` and self-host `IBoundsCheck/IDivCheck/ITrap` are NEVER constructed anywhere in lowering (grep-verified). `emit_hof.rs:156`/`emit_types.rs:246` (unwrap_err-on-Ok combinator) abort()s are latent-reachable (not in corpus) and are UnwrapErrorOnOk, not bounds.

---

## SECTION 1 — PREMISE VERIFICATION (claim → verdict + file:line)

| # | Premise | Verdict | Evidence |
|---|---|---|---|
| 1a | Rust C bounds = runtime helper `gorget_array_get` (no span at helper) | **CONFIRMED** | `runtime_array.c:31-37` `if(index>=arr->len){fprintf("gorget: panic: index out of bounds: index %zu, length %zu"); exit(1);}`. Emitted via `gorget_array_get(__v20,__v21)` (LIR `insts.rs:1170-1178`). MEASURED: `gorget: panic: index out of bounds: index 5, length 3` exit 1. |
| 1b | Rust LLVM bounds = helper or inline? | **CONFIRMED helper** (same runtime) | LLVM has NO inline bounds check for `v[i]`; it emits the same `gorget_array_get` CallExtern. MEASURED identical stderr + exit 1. |
| 1c | self-host bounds = inline abort() exit 134 (per prior scout) | **CORRECTED** | Self-host `v[i]` lowers to `gorget_array_get` (`lir_lower.gg:2618/2898/4512`), emits `__v20 = gorget_array_get(__v18,__v17)` — the SHARED runtime helper. MEASURED via driver+cc: `gorget: panic: index out of bounds...` + **exit 1** (NOT 134). The exit-134 `IBoundsCheck` abort (`lir_codegen.gg:4857`) is DEAD (never constructed) — the prior scout mis-identified the reachable path. |
| 2 | Span available at the `v[i]` emit site? | **CONFIRMED reachable** | `block.span_map.get(idx)` → `resolve_panic_loc` runs for EVERY `CallExtern` (`inst_needs_loc` includes `CallExtern`, `c_lir/mod.rs:150`; LLVM per-inst `mod.rs:3315-3317`). The `loc` tuple `(file,line,col)` is already in scope at both emit boundaries. Inline-at-emit-site is CLEANEST — no helper-ABI fan-out. |
| 3 | Full bounds class > the fixture | **CONFIRMED** | Fixture `trap_bounds.gg` = `a[5]` Vector READ → only `gorget_array_get`. Class (~30 sites): array set/remove/insert/swap/swap_remove/slice (`runtime_array.c:81/94/156/173/380/445`), str index/slice/byte (`runtime_string.c:701 gorget_panic, :723/:761/:778`), shared (`shared_runtime.c:99/107`), bytes (`bytes_runtime.c` 8 + `bytes_f32_runtime.c` 8). NON-bounds (do NOT map to T_Bounds): capacity-overflow/alloc-failed (`runtime_array.c:298/302/314/318/331/335`), pop-from-empty (`:144`). |
| 4 | abort()/134 sites reachable or latent | **DEAD (Inst variants) / latent (combinator)** | `Inst::BoundsCheck/DivCheck/Trap` (`c_lir/mod.rs:3120/3129/3137`, `llvm/mod.rs:6959/6980/6999`) NEVER constructed (grep: zero `push_inst`/build sites). Self-host `IBoundsCheck/IDivCheck/ITrap` (`lir_codegen.gg`) same. `emit_hof.rs:156`+`emit_types.rs:246` = UnwrapErrorOnOk combinator abort(), latent (corpus hits the `gorget_panic` guard instead). |
| 5 | ggdef models bounds; floors all 194 | **CONFIRMED** | ggdef `eval.rs:70 Bounds`, `:93 "T_Bounds"`, `:107 is_catchable`, `:116 detail "index out of bounds"`, `eval_index :715 Err(Halt::Trap(Bounds))`. Prod registry `src/trap.rs:42/64 Bounds→"T_Bounds"`, catchable `:77`. Floors `spec_conformance.rs:77-79 = 194,194,194`; `MIN_FIXTURES:88 = 195`. |
| — | NEW: Fault re-panic is a 2nd bounds emit path (T2a remnant) | **CONFIRMED (MEASURED)** | `exprs/mod.rs:3800` + `functions.rs:102` `gorget_panic("index out of bounds")`; siblings `:3786/:3793` + `:88/:95` overflow/divzero. All emit OLD `file:L:C: <detail>` + exit 1 (not `trap[T_X]`+101). |

---

## SECTION 2 — END-TO-END MEASUREMENT (two prototypes, both reverted)

### 2a. Baseline (all three lanes) — `a[5]` on a len-3 Vector
| Lane | stderr | exit | path |
|---|---|---|---|
| Rust C | `gorget: panic: index out of bounds: index 5, length 3` | 1 | `gorget_array_get` (runtime_array.c:31) |
| Rust LLVM | `gorget: panic: index out of bounds: index 5, length 3` | 1 | same helper |
| self-host | `gorget: panic: index out of bounds: index 5, length 3` | 1 | emits `gorget_array_get` → same helper |

### 2b. Prototype #1 — C backend, REAL span (proves owner ruling)
Throwaway edits: (i) `runtime_array.c` add `gorget_array_get_at(arr, index, code, file, line, col)` → on OOB `snprintf` the detail + `gorget_trap_at(code, detail, file, line, col)`; else return elem ptr. (ii) `emit_call_extern.rs` early-return branch `name=="gorget_array_get" && args.len()==2 && dst.is_some()` → `dst = gorget_array_get_at(arr, idx, "T_Bounds", "file", ln, cl);`, code from `crate::trap::TrapKind::Bounds.code()`, loc from the resolved `loc` tuple.
```
BEFORE:  gorget: panic: index out of bounds: index 5, length 3            exit 1
AFTER:   trap[T_Bounds]: index out of bounds: index 5, length 3 at .../trap_bounds.gg:18:13   exit 101
```
`cargo test --release --test spec_conformance spec_conformance_c` → **total=195 · MATCH=195 · MISMATCH=0 · BUILD-FAIL=0** (was 194/1). `trap_bounds.gg` MATCH; zero regressions across all 195 fixtures. **Real location threaded; span reachable.**

### 2c. Prototype #2 — runtime-only base normalization flips self-host + LLVM for FREE
Changed ONLY `gorget_array_get`'s OOB body → `gorget_trap("T_Bounds", detail)` (span-less). Ran the self-host lane (driver emit → cc → run; NO driver rebuild) AND, after a `cargo build`, the LLVM lane:
```
self-host:  trap[T_Bounds]: index out of bounds: index 5, length 3 at <unknown>:0:0   exit 101   → MATCH
Rust C:     trap[T_Bounds]: ... at <unknown>:0:0   exit 101   → MATCH
Rust LLVM:  trap[T_Bounds]: ... at <unknown>:0:0   exit 101   → MATCH
```
**Decisive:** the shared runtime helper is the single choke point — normalizing it flips ALL THREE lanes to MATCH span-less. Real location is a Rust-C+LLVM add-on (`_at`), NOT needed to flip the floor. Self-host has no spans anyway (consistent with its `<unknown>:0:0` overflow/divzero).

### 2d. Fault re-panic path (MEASURED, current clean build)
```
fault_bounds_panic_default (uncaught plain xs[10]):   gorget: panic: index out of bounds: index 10, length 3   exit 1   (rides gorget_array_get → T2b fixes)
fault_deep_bounds_swallow_guard (fault-scope bounds):  .../fault_deep_bounds_swallow_guard.gg:12:13: index out of bounds   exit 1   (bounds_panic block — NOT normalized)
fault_deep_mixed_divzero_only (fault-scope overflow):  .../:20:13: integer overflow   exit 1   (div_overflow_panic — T2a remnant)
fault_deep_uncaught_divzero_panic (fault-scope div0):  .../:6:26: division by zero    exit 1   (div_zero_panic — T2a remnant)
```
Prototype diff saved `/tmp/scout_t2b_prototype.patch`; both prototypes reverted; `git status` clean (worktree + main).

---

## SECTION 3 — DESIGN PROPOSAL

### 3.1 Two-layer bounds reroute (the shared-helper choke point makes this clean)
**Layer A — runtime helper normalization (span-less, flips all 3 lanes + internal callers).**
Change every bounds/index-family OOB in the runtime from `fprintf("gorget: panic: …out of bounds…"); exit(1)` (and `gorget_panic("str index out of bounds")`) → `gorget_trap("T_Bounds", detail)`. This is the mechanical class fix (CLAUDE #4). The self-host and Rust internal callers ride it automatically (`<unknown>:0:0` + exit 101).
- `runtime_array.c`: `gorget_array_get :31` (fixture), plus `gorget_array_set :79`, `remove :92`, `swap_remove :154`, `swap :171`, `insert :380`, `slice :445`.
- `runtime_string.c`: `gorget_str_index :701` (`gorget_panic`→`gorget_trap`), `str_slice :723`, `byte_slice :761`, `byte index :778`.
- `shared_runtime.c :99/:107`; `bytes_runtime.c` (8) + `bytes_f32_runtime.c` (8) offset checks.
- LEAVE as-is (NOT T_Bounds): capacity-overflow / alloc-failed (`:298…:335`), pop-from-empty (`:144`) — resource/empty, not index bounds (owner Q-B).

**Layer B — real-location for the flagship (Rust C + LLVM only).**
Add `gorget_array_get_at(arr, index, code, file, line, col)` to `runtime_array.c` (formats detail, calls `gorget_trap_at`). Reroute the compiler-emitted `v[i]` READ → `gorget_array_get_at` at BOTH Rust emit boundaries, threading the resolved `loc` + `TrapKind::Bounds.code()`:
- C: `emit_call_extern.rs` — early-return branch (PROVEN in §2b).
- LLVM: `llvm/mod.rs` CallExtern rewrite (symmetric to the existing `gorget_trap` branch at `:4597`: intern "T_Bounds"+file, i32 line/col, preserve `dst`).
- `gorget_array_get` (Layer A) stays as the span-less fallback for internal runtime callers + self-host.
**Layering note:** the `_at` path threads the `T_Bounds` code as DATA from `TrapKind::code()` (invariant honored). `gorget_array_get`'s own span-less normalization hardcodes `"T_Bounds"` once — defensible (it's the bounds helper, not a routing table), but flag to reviewer (owner Q-A).

### 3.2 Fault re-panic blocks (fix-the-class — MUST, and closes a T2a gap)
Reroute the 6 `gorget_panic(Str)` re-panic emits to the existing 2-arg `gorget_trap(Str(code), Str(detail))` (the backend rewrite already exists, `emit_call_extern.rs:74`, `llvm/mod.rs:4597`; span already threaded → real location for free):
- `exprs/mod.rs:3800` + `functions.rs:102` → `gorget_trap("T_Bounds", "index out of bounds")` (the 2nd bounds path).
- **Siblings (T2a remnant — same edit):** `exprs/mod.rs:3786/3793` + `functions.rs:88/95` → `gorget_trap("T_Overflow"/"T_DivByZero", …)`. Fixing bounds without these = fixing the instance, not the class (CLAUDE #4 / sibling-drift doc).

### 3.3 abort()/134 fold (hygiene — kills the 4th exit format)
- Dead `Inst::BoundsCheck/DivCheck/Trap` (`c_lir/mod.rs:3120/3129/3137`, `llvm/mod.rs:6959/6980/6999`) + self-host `IBoundsCheck/IDivCheck/ITrap` (`lir_codegen.gg`): fold `abort()` → `gorget_trap`/`gorget_trap_at` (Bounds→T_Bounds, DivCheck→T_DivByZero, Trap→T_Panic). Dead today, so purely defensive; keeps no `exit 134` path latent.
- `emit_hof.rs:156` + `emit_types.rs:246` unwrap_err-on-Ok `abort()` → `gorget_trap("T_UnwrapErrorOnOk", …)` (NOT bounds; latent-reachable via fused `.map().unwrap_err()`). Owner Q-C: fold in T2b or defer.

### 3.4 Floors
`spec_conformance.rs:77-79` all `194 → 195 (= MIN_FIXTURES)` in the T2b commit, AFTER the reroute lands. Never bump before the reroute.

### 3.5 Regression-net / blast radius (verified robust)
- `run_gg_panics` / `run_gg_panics_with_stdout` / `security_traps` assert **nonzero exit + substring** (not exit==1, not exact stderr). Since T2b KEEPS the detail substrings ("index out of bounds", "str index out of bounds", "byte_slice out of bounds", "integer overflow", "division by zero"), these all PASS at exit 101. (Affected: `security.rs:778/783/1119`, `integration.rs:6392/6672/6701/6730/6756/6820`.)
- **Stale doc comments to update** (not assertions): `fault_bounds_panic_default.gg` ("exit(1)"), the `fault_deep_*` comments ("exit 1"), `functions.rs:79-83`. These now mean `trap[T_X]` + exit 101.
- `c_emit_comparison` compares fn COUNT (blind to trap text); `self_host_runtime_diff` skips non-zero-exit fixtures; `bootstrap_fixed_point` unaffected (emitted C for `gorget_array_get` call is unchanged; runtime is read via `--runtime-dir` at cc time). Executor should still run `self_host_bootstrap_fixed_point` + ASan (CLAUDE #7).

---

## SECTION 4 — RECOMMENDED SLICING + SIZE/RISK

**T2b is ONE agent** (the shared-helper choke point makes the self-host FREE — no split like T2a). Size **M**, risk **Low-Med**.

| Part | Scope | Files | Notes |
|---|---|---|---|
| MUST-1 fixture+floor | `gorget_array_get`→normalize + `gorget_array_get_at` + Rust C+LLVM reroute + floors 194→195 | `runtime_array.c`, `emit_call_extern.rs`, `llvm/mod.rs`, `spec_conformance.rs` | PROVEN end-to-end (§2b). Flips all 3 lanes. |
| MUST-2 fault re-panic | bounds_panic + overflow/divzero siblings → `gorget_trap` | `exprs/mod.rs:3786-3805`, `functions.rs:87-107` | Closes T2a sibling-gap; 6 mechanical line-changes; real span free. |
| SHOULD full class | set/remove/insert/swap/slice/str/shared/bytes → span-less `gorget_trap("T_Bounds",…)` | `runtime_array.c`, `runtime_string.c`, `shared_runtime.c`, `bytes*_runtime.c` | ~25 mechanical edits; conformance-invisible but reference-grade. |
| MAY hygiene | abort()/134 fold (dead Inst + self-host I-variants + emit_hof/emit_types) | `c_lir/mod.rs`, `llvm/mod.rs`, `lir_codegen.gg`, `emit_hof.rs`, `emit_types.rs` | Dead/latent; kills the 4th exit format. Deferrable if agent gets large. |

**Self-host `.gg`: ZERO required** for the corpus (rides the runtime helper — MEASURED). The only self-host touch is the OPTIONAL abort()-fold hygiene.
**Sub-slice question:** span-threading is NOT a separate slice — it's the same `_at` helper + emit-boundary rewrite as the format-flip (they land together; the format-flip without the span would be span-less, which the owner ruling forbids for the flagship). Real-location and format-flip are ONE change for the READ path.
**Sequencing:** T2b edits `c_lir/mod.rs`/`llvm/mod.rs` regions DISJOINT from T2a's; run after T2a-rust+T2a-selfhost (all LANDED). Fine to run solo now.

---

## SECTION 5 — OWNER DESIGN QUESTIONS (with recommendations)

**Q-A — the one hardcoded `"T_Bounds"` in `gorget_array_get` (span-less fallback).** The `_at` path threads the code as data (invariant-pure); the span-less `gorget_array_get` normalization spells `"T_Bounds"` once in C for internal callers (no compiler emit site to thread it from). **Recommend: accept it** — a bounds-specific helper unconditionally raising its one code is not a routing table (the thing the "no C-side table" rule forbids). Alternative (thread it everywhere) buys nothing for callers that have no span.

**Q-B — does the real location apply to the WHOLE index class or just the flagship `v[i]` READ?** Each additional real-location site (`v[i]=x` write, `.remove/.insert/.slice`, `str[i]`, `shared[i]`, bytes offset) needs its OWN `_at` variant + emit-boundary rewrite (wide). **Recommend: real location for the `v[i]` READ path in T2b (flagship = the fixture); span-less `trap[T_Bounds]` for the rest of the class in T2b; file "thread real spans into the remaining bounds sites" as a follow-up.** Flips the fixture, fixes the whole class's format+exit, gives the flagship its real location, defers the wide per-site span plumbing.

**Q-C — is the T2a fault re-panic remnant in T2b or a separate T2a-followup?** The overflow/divzero re-panic blocks emit old format + exit 1 today (MEASURED) — a pre-existing T2a gap, but literally the adjacent lines to the bounds_panic T2b must touch. **Recommend: fix all three in T2b** (one fix, all siblings). Splitting them out re-opens the same file for a 6-line change.

**Q-D — abort()/134 fold in T2b or deferred?** The `Inst::*Check/Trap` variants are DEAD; `emit_hof`/`emit_types` unwrap-on-Ok are latent-reachable (a known defect: `.map().unwrap_err()` on Ok → exit 134 + non-normalized format). **Recommend: fold the emit_hof/emit_types ones in T2b** (they're a real latent defect — CLAUDE #8), and either fold or delete the dead `Inst` variants (dead code; folding is cheap, deleting is cleaner but wider).

**Q-E — pop-from-empty / capacity-overflow / alloc-failed codes.** Not index-bounds. **Recommend: leave off T_Bounds** — capacity/alloc are resource/OOM (a separate D17-ish category, out of the D11 closed registry); pop-from-empty could be `T_Bounds` or its own thing but is not modeled by ggdef and not in the corpus — leave span-less-panic for now, file for a future registry decision.

**Q-F — Fault.Bounds catch interaction.** NONE. The catchable path is `gorget_array_safe_get` (returns NULL, branch-before-deref, `insts.rs:1249`); the real-location change only touches the panicking `gorget_array_get`/`_at`. A CAUGHT `Fault.Bounds` never reaches the trap; only the uncaught re-panic does (now normalized via §3.2). Negative index rides the same `gorget_array_get` (size_t cast → OOB → T_Bounds), so it gets the real location for free.

## Appendix — commands (all in-worktree, this session)
- Baseline: `gg build spectests/run/trap_bounds.gg` (+ `--backend=llvm`) → both `gorget: panic: index out of bounds: index 5, length 3` exit 1.
- Self-host baseline: `driver trap_bounds.gg lib --emit-c --runtime-dir=$PWD/src/backend/c/runtime` → cc → run → same, exit 1.
- Prototype #1 (real span, C): edit `runtime_array.c`+`emit_call_extern.rs`; `cargo build`; run → `trap[T_Bounds]: … at …/trap_bounds.gg:18:13` exit 101; `cargo test --release --test spec_conformance spec_conformance_c` → total=195 MATCH=195 MISMATCH=0.
- Prototype #2 (runtime-only): edit `gorget_array_get`→`gorget_trap`; self-host + C + LLVM all → `trap[T_Bounds]: … at <unknown>:0:0` exit 101.
- Fault paths: `gg build tests/fixtures/fault_deep_{bounds_swallow_guard,mixed_divzero_only,uncaught_divzero_panic}.gg` → old `file:L:C: <detail>` exit 1 (unnormalized).
- Both prototypes reverted (`git checkout --`); `git status` clean (worktree + main).
