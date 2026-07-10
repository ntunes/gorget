# T2a-selfhost — mirror the trap-emit reroute in the SELF-HOST `.gg` lowering (executor brief)

> **Wave position:** the self-host half of D11 production emit. T2a-rust (the Rust gg C+LLVM
> backends) LANDED — the C runtime now has `gorget_trap`/`gorget_trap_at` (exit 101). This slice
> makes the SELF-HOST compiler emit the SAME runtime calls with the SAME `T_` codes, so the 7
> non-bounds trap fixtures flip to MATCH on the self-host lane (`SELFHOST_MATCH_FLOOR` 187→194;
> `trap_bounds` stays MISMATCH = T2b). Disjoint from T3b (`tests/smith`). **The scout prototyped
> ALL 7 classes end-to-end and MEASURED the flip (`spec_conformance_selfhost` → 194 MATCH / 1
> MISMATCH); this design is proven, not theoretical.**
>
> **Grounded in:** `docs/plans/define-gorget/scouts/scout-t2a-selfhost-emit.md` (the measured scout
> with the exact site map + prototype). The self-host emits C ONLY (no LLVM twin), has NO span on
> the inline path (span-less `gorget_trap` suffices), and does NOT use the 4-arg
> `gorget_assert_fail_values` (its asserts are unified through `gorget_panic` with a static message).

## The measured site map (which site each fixture hits — from the scout's trace)
| Fixture | Code | Site | Mechanism |
|---|---|---|---|
| `trap_overflow` | `T_Overflow` | `lir_codegen.gg:4412` (IAdd; siblings 4419 ISub, 4426 IMul, 4440 IDiv INT_MIN, 4454 IRem INT_MIN) | inline C-string (A) |
| `trap_divbyzero` | `T_DivByZero` | `lir_codegen.gg:4441` (IDiv dz; siblings 4455 IRem, 4470 IMod) | inline C-string (A) |
| `trap_unwrap_none` | `T_UnwrapNone` | `lower_expr.gg:3525` (uw_word="None") | GICallExtern (B) |
| `trap_unwrap_error` | `T_UnwrapError` | `lower_expr.gg:3525` (uw_word="Error") | GICallExtern (B) |
| `trap_unwrap_error_on_ok` | `T_UnwrapErrorOnOk` | `lir_codegen.gg:5146` (ue_guard; siblings 5140 ou→UnwrapNone, 5177 re→UnwrapError) | inline C-string (A) |
| `trap_assert` | `T_AssertFailed` | `lower_stmt.gg:1441` (static msg) | GICallExtern (B) |
| `trap_panic` | `T_Panic` | `lower_expr.gg:7714` | GICallExtern (B) |

Re-verify each site:line before editing (the self-host files drift; re-grep the surrounding function).

## Work items

### W1 — Mechanism A: inline C-string reroute (`lir_codegen.gg`)
Replace the inline abort-emitting C-text these sites emit — **fprintf/exit for the ARITH sites**
(`{ fprintf(stderr, "gorget: <msg>\n"); exit(1); }`), **inline `gorget_panic("...")` for the UNWRAP
GUARDS** (5140/5146/5177 emit e.g. `{ gorget_panic("called \`unwrap()\` on a \`None\` value"); }`) —
with `{ gorget_trap("T_<Code>", "<detail>"); }` (span-less — the inline path has no `loc`; conformance
ignores location per Q1). Sites + codes:
- Arith overflow → `T_Overflow`: `lir_codegen.gg:4412` (IAdd), 4419 (ISub), 4426 (IMul), 4440 (IDiv
  INT_MIN), 4454 (IRem INT_MIN). (The scout used `replace_all` on the `"integer overflow"` guard text
  — 4 occurrences after IAdd; verify the count.)
- Arith divzero → `T_DivByZero`: 4441 (IDiv), 4455 (IRem), 4470 (IMod).
- Inline unwrap guards: 5140 ou_guard → `T_UnwrapNone`; 5146 ue_guard → `T_UnwrapErrorOnOk`; 5177
  re_guard → `T_UnwrapError`.

### W2 — Mechanism B: GICallExtern reroute (`lower_*.gg`)
Change `GICallExtern(-1, "gorget_panic", [msg])` → `GICallExtern(-1, "gorget_trap", [OpConstStr(code), msg])`:
- `lower_expr.gg:3525` unwrap → code from `uw_word` (`"None"`→`T_UnwrapNone`, `"Error"`→`T_UnwrapError`).
- `lower_stmt.gg:1441` assert → `T_AssertFailed`.
- `lower_expr.gg:7714` user panic → `T_Panic`.
- **Do NOT reroute the fault-scope cross-frame repanic siblings** (`lower_closures.gg:93`,
  `lower_expr.gg:7412`) — leave them emitting `gorget_panic` (revised from an earlier Q-D ruling that
  was WRONG on two counts): (1) T2a-rust did NOT reroute the Rust TWIN of this path
  (`src/ir/lowering/functions.rs` `fill_fault_return_block` still emits `gorget_panic` at ~:88/96/103/189-191),
  so rerouting only the self-host side would DIVERGE the self-host from Rust production on an
  un-gated cross-frame-fault path (Core #7/#8) — the opposite of the goal; (2) these sites are
  PER-CATEGORY fault repanics (Overflow/DivByZero/Bounds), so `T_Panic` would emit the
  self-contradicting `trap[T_Panic]: integer overflow`. Leaving them as `gorget_panic` matches current
  Rust production AND still flips all 7 corpus fixtures (they hit the DIRECT sites above, never this
  cross-frame repanic — scout-measured). **FILE a TODO (Medium):** reroute the cross-frame fault
  repanic in BOTH compilers together (`functions.rs` + the two self-host helpers), category-correct
  (variant → `T_Overflow`/`T_DivByZero`/`T_Bounds`, NEVER `T_Panic`) — that is the honest "fix the
  class," since the class spans Rust + self-host.
- **The ONE table edit that makes Mechanism B marshal correctly:** `runtime_arg_is_cstr`
  (`lir_codegen.gg:2724`) — add `gorget_trap` returning true for `arg_idx == 0` AND `arg_idx == 1`
  (both the code literal and the message are C-strings). The generic arg loop (`lir_codegen.gg:6167-6210`)
  then marshals both via `gorget_str_to_cstr` (the proven `gorget_panic` path). No noreturn
  registration needed (the generated C calls `gorget_trap` then the existing dead `GTJump` block;
  `gorget_trap` `exit(101)`s — the scout proved all 7 compile + run).

### W3 — the `T_` code parity lint (rule-2 mitigation, Q-B)
The self-host hand-spells `"T_Overflow"` etc. as string literals (it can't import Rust's `TrapKind`).
Add a Rust lint to `tests/lints.rs` (mirror the existing self-host arm-count lints that read `.gg`
files): scan `lir_codegen.gg`/`lower_expr.gg`/`lower_stmt.gg`/`lower_closures.gg`, extract every
`T_<Ident>` literal in a trap-emit context, and assert (a) each ∈ `gorget::trap::TrapKind::code()`
set, and (b) all 7 non-Bounds codes appear (no site silently dropped). This pins self-host ↔
Rust/ggdef codes WITHOUT a hand-synced Rust-side list.

### W4 — floor bump (`tests/spec_conformance.rs`)
`SELFHOST_MATCH_FLOOR` 187 → **194** (`:79`), same commit. `trap_bounds` stays MISMATCH (T2b). Verify
exactly the 7 flip on the self-host lane, no baseline regression.

## Decided (scout Q-rulings) — do NOT re-open
- **Q-A: accept span-less self-host traps** (`<unknown>:0:0` — matches the self-host's pre-existing
  no-span arith; conformance ignores location; threading spans into self-host codegen is a separate
  larger change).
- **Q-E: leave the message-less comparison assert as-is** (self-host emits a static `"assertion
  failed: left == right"` via the unified assert path → `trap[T_AssertFailed]` + exit 101 = MATCH;
  only the impl-defined detail differs from Rust's runtime-value dump — conformance-invisible; no
  message-less-cmp-assert trap fixture exists).
- **Q-F: leave the latent `abort()` sites for T2b** (`lir_codegen.gg:4851` bounds, `4854` divzero-helper,
  `4857` generic — the uncatchable-abort paths; T2b flips `trap_bounds` on all lanes).

## OUT OF SCOPE — FILE, do NOT do here
- **Q-C (self-host shift-out-of-range gap — a Core-#8 finding to FILE):** the self-host does NOT guard
  out-of-range shift AT ALL (`lir_codegen.gg:4534/4537` emit raw `<<`/`>>`), where BOTH Rust backends
  now trap → `T_Overflow` (T2a-rust + owner ruling). So the self-host is now the lone backend with
  silent UB on `x << 64`. This is PRE-EXISTING (the self-host never guarded shift) and NOT in the
  conformance corpus (ggdef doesn't model shift), so it does NOT affect the 7-fixture flip and is NOT
  a "reroute an existing guard" (there's no guard to reroute — it needs a NEW guard). FILE it as a
  TODO (Medium, self-host-shift-parity): add the self-host shift-range guard emitting
  `gorget_trap("T_Overflow", "shift out of range")` + a self-host-lane shift test, so all three
  backends agree. Do NOT add it in this brief.

## Gate battery (run FOREGROUND, generous timeouts — the self-host driver rebuild is ~2.5-3 min; PASTE output)
```
cargo build
GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance spec_conformance_selfhost -- --nocapture 2>&1 | tee /tmp/t2ash_conf_$$.log
cargo test --test lints 2>&1 | tee /tmp/t2ash_lints_$$.log                                   # the new T_ parity lint green
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point -- --nocapture 2>&1 | tee /tmp/t2ash_boot_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration c_emit_comparison -- --nocapture 2>&1 | tee /tmp/t2ash_cemit_$$.log
```
Acceptance (all verified SAFE by the scout): builds; `spec_conformance_selfhost` shows
**total=195 · MATCH=194 · MISMATCH=1** (only `trap_bounds`) · BUILD-FAIL=0, floor 194 holds; the new
`T_`-parity lint green (all 7 codes present + ∈ TrapKind); `bootstrap_fixed_point` green (`gorget_trap`
is in the runtime preamble; stage1/stage2 both from the modified source → identical → fixed point
holds — the load-bearing canary); `c_emit_comparison` green (compares user-fn COUNTS, not C text, so
trap-string changes are invisible — SAFE). Do NOT run the full `cargo test --test integration`
(parent's job).

## Scope fences
- EDIT ONLY these three `.gg` files: `tests/fixtures/self_host_lowerer/{lir_codegen,lower_expr,lower_stmt}.gg`
  (the direct trap sites), plus `tests/lints.rs` (the new parity lint), `tests/spec_conformance.rs`
  (the floor bump), `TODO.md` (BOTH the Q-C self-host-shift follow-up AND the cross-frame-repanic
  follow-up).
- **`lower_closures.gg` is READ by the W3 parity lint ONLY — do NOT EDIT it.** Its trap site (`:93`)
  deliberately STAYS `gorget_panic` per W2 (rerouting it to `gorget_trap("T_Panic", …)` would
  reintroduce the self-host/Rust divergence pass 1 removed — and NO gate catches it: the parity lint
  accepts `T_Panic`, bootstrap stays green, no corpus fixture exercises the cross-frame path).
- Do NOT touch `src/` (T2a-rust landed; the runtime `gorget_trap`/`gorget_trap_at` already exist),
  `spec/ggdef/*`, `spectests/*`, the reference, or the latent `abort()` sites (T2b).

## Worktree & agent discipline (NON-NEGOTIABLE)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree; STOP if either is
`/workspace/gorget` or `/workspace/gorget-1`. Paths RELATIVE to your worktree; on Edit desync re-Read +
retry the Edit tool (NEVER a heredoc with an absolute path); after any non-Edit write `git -C
/workspace/gorget status` and STOP if it shows changes. Entry: `git merge --ff-only gorget-1
2>/dev/null || true`. **Checkpoint to `/tmp/t2ash_report_$$.md` after each work item.** Run the FINAL
gates (esp. the ~3-min bootstrap) FOREGROUND with `GG_BUILD_TIMEOUT_SECS=600` — do NOT background a
long run then end (rule 9). Stage ONLY exact files by name; NEVER `git add -a`/`.`/`commit -a`; NEVER
`git stash`. Commit on your worktree branch, message ending:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
Per work item: files+file:line and one-line what. PASTED gate output — the `spec_conformance_selfhost`
194/1 summary, the parity-lint result, bootstrap green. The TWO TODOs you filed (the Q-C
self-host-shift-parity follow-up AND the both-compiler cross-frame-repanic follow-up). Any
site whose line/shape differed from the scout's map (corrected). Branch + commit hash.
