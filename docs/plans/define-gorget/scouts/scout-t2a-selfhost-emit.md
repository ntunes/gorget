# SCOUT REPORT: T2a-selfhost — mirror trap-emit reroute in self-host lowering

PID=11382 / worktree agent-a29ef6dfd4289427d
Status: IN PROGRESS

## PREMISE VERIFICATION (checkpoint)

Fixture → self-host emit site → mechanism:
| Fixture | Code | Self-host site | Mechanism |
|---|---|---|---|
| trap_overflow | T_Overflow | lir_codegen.gg:4412 (IAdd) | inline C `fprintf;exit(1)` |
| trap_divbyzero | T_DivByZero | lir_codegen.gg:4441 (IDiv dz) | inline C `fprintf;exit(1)` |
| trap_unwrap_none | T_UnwrapNone | lir_codegen.gg:5140 (ou_guard) OR lower_expr.gg:3525 | inline gorget_panic / GICallExtern |
| trap_unwrap_error | T_UnwrapError | lower_expr.gg:3525 (Result.unwrap) | GICallExtern gorget_panic |
| trap_unwrap_error_on_ok | T_UnwrapErrorOnOk | lir_codegen.gg:5146 (ue_guard) | inline gorget_panic |
| trap_assert | T_AssertFailed | lower_stmt.gg:1441 | GICallExtern gorget_panic (static msg) |
| trap_panic | T_Panic | lower_expr.gg:7714 | GICallExtern gorget_panic |

KEY FINDINGS:
- Self-host has NO gorget_panic_at / gorget_trap_at anywhere -> no span threading. Inline arith emits bare "gorget: integer overflow" (no file:line:col). So self-host reroute can use SPAN-LESS gorget_trap(code, detail) -> routes to <unknown>:0:0 + exit 101. Conformance ignores location. SIMPLER than Rust.
- Self-host assert path is UNIFIED: ALL asserts (msg, msgless-cmp, generic) -> gorget_panic(static_msg). Does NOT emit gorget_assert_fail_values. So NO 4-arg update needed for self-host. (assert_fail_values only appears in pred_tostr classification at lir_codegen.gg:8960, not an emit site.)
- Runtime gorget_trap/gorget_trap_at ARE present in src/backend/c/runtime (landed by T2a-rust): panic_normal.c:17/23, runtime_string.c:463 fwd-decl, panic_test.c:50. Self-host generated C links against SAME runtime -> gorget_trap available.
- adjudicate (spec_conformance.rs:256-266): trap MATCH iff exit==101 AND stderr contains "trap[T_X]" AND stdout prefix.
- selfhost_step (spec_conformance.rs:358): driver <fixture> <lib_dir> --emit-c --runtime-dir=ABS > x.c ; cc -O0 -w x.c -lm -lpthread ; run.
- SELFHOST_MATCH_FLOOR = 187 (spec_conformance.rs:79). Target 194.

TWO reroute mechanisms in self-host:
1. Inline C-string emit (lir_codegen.gg): arith 4412-4472, unwrap guards 5140/5146/5177 -> emit gorget_trap(code,detail) raw C.
2. GICallExtern gorget_panic (lower_*.gg): assert 1441, Result.unwrap 3525, user panic 7714, emit_fault_repanic_block 7412, closure 93 -> GICallExtern gorget_trap [OpConstStr(code), msg]; needs runtime_arg_is_cstr("gorget_trap", 0/1)=true.

## MEASUREMENT (in progress)
- Baseline trap_overflow self-host: `gorget: integer overflow` + exit 1 = MISMATCH. C guard line: `if (__builtin_add_overflow(...)) { fprintf(stderr, "gorget: integer overflow\n"); exit(1); }`
- Prototype edits (throwaway):
  1. lir_codegen.gg:4412 IAdd overflow -> `gorget_trap("T_Overflow","integer overflow")`
  2. lir_codegen.gg ue_guard -> `gorget_trap("T_UnwrapErrorOnOk", ...)`
  3. lower_expr.gg:7714 user panic GICallExtern -> `gorget_trap` [OpConstStr("T_Panic"), msg]
  4. runtime_arg_is_cstr: gorget_trap arg 0 & 1 = cstr
- Rebuilding driver...

## Generic CallExtern marshaling (verified)
lir_codegen.gg:6167-6210 generic arg loop: per-arg `cstr_pos = runtime_arg_is_cstr(name, ai)`. cstr_pos && is_str -> `gorget_str_to_cstr(v)`; cstr_pos && is_ptr -> `gorget_str_to_cstr(*(Str*)v)`. So gorget_trap [OpConstStr(code), msg] with both args cstr-marked marshals correctly. Proven pattern (gorget_panic already uses it for its single msg arg).

## MEASUREMENT RESULT (3 mechanisms PROVEN)
Prototype driver rebuilt (~3 min). Self-host lane per fixture:
- trap_overflow  -> `trap[T_Overflow]: integer overflow at <unknown>:0:0` exit 101 = MATCH  [inline arith]
- trap_unwrap_error_on_ok -> `trap[T_UnwrapErrorOnOk]: ... at <unknown>:0:0` exit 101 = MATCH  [inline ue_guard]
- trap_panic -> `trap[T_Panic]: explicit panic at <unknown>:0:0` exit 101 = MATCH  [GICallExtern]
Non-prototyped still MISMATCH (each needs own reroute):
- trap_divbyzero: `gorget: division by zero` exit 1  [inline IDiv 4441]
- trap_unwrap_none: `<unknown>:0:0: called unwrap on None` exit 1  [GICallExtern lower_expr.gg:3525, uw_word=None]
- trap_unwrap_error: `<unknown>:0:0: called unwrap on Error` exit 1  [GICallExtern lower_expr.gg:3525, uw_word=Error]
- trap_assert: `<unknown>:0:0: one is not greater than two` exit 1  [GICallExtern lower_stmt.gg:1441]

MEASURED site map (which site each fixture ACTUALLY hits):
- overflow -> lir_codegen.gg:4412 IAdd (inline)
- divbyzero -> lir_codegen.gg:4441 IDiv (inline)
- unwrap_none / unwrap_error -> lower_expr.gg:3525 (GICallExtern, uw_word None/Error)
- unwrap_error_on_ok -> lir_codegen.gg:5146 ue_guard (inline)
- assert -> lower_stmt.gg:1441 (GICallExtern, static msg)
- panic -> lower_expr.gg:7714 (GICallExtern)
Inline ou_guard(5140)/re_guard(5177) NOT hit by 7 fixtures but same class -> reroute anyway.

CONFIRMED: span-less gorget_trap(code,detail) -> <unknown>:0:0 + exit 101 -> MATCH (location not compared).

## FULL PROTOTYPE RESULT (all 7 sites) — DEFINITIVE
Complete prototype (all inline arith + inline guards + GICallExtern unwrap/assert/panic) rebuilt driver, then ran spec_conformance_selfhost harness:
  total=195 · MATCH=194 · MISMATCH=1 · BUILD-FAIL=0
Only MISMATCH = trap_bounds (T2b). PROVES self-host floor 187 -> 194.
Manual per-fixture (replicating adjudicate): all 7 non-bounds = MATCH (trap[T_X]+exit101), bounds MISMATCH.
Prototype REVERTED; worktree clean; main untouched.

## REGRESSION-NET SAFETY (verified)
- c_emit_comparison (integration.rs:16621): compares user_fn_count (function-def COUNT), NOT C text. Trap-emit string changes are invisible. SAFE.
- self_host_runtime_diff (integration.rs:20350): opt-in (GG_RUNTIME_DIFF=1), EXCLUDES non-zero-exit fixtures (trapping fixtures skipped), compares STDOUT only. SAFE.
- bootstrap_fixed_point (integration.rs:17031): stage1==stage2 C, both from same modified source -> identical -> fixed point holds. gorget_trap defined in runtime preamble. SAFE (executor should still run it as the net).

## RISK / DIVERGENCE FINDINGS
- Self-host inline arith has NO loc/span -> emits span-less gorget_trap -> <unknown>:0:0 (Rust has real span). Conformance ignores location -> MATCH. Divergence conformance-invisible.
- Self-host does NOT guard shift-out-of-range AT ALL (lir_codegen.gg:4534/4537 raw <<,>>). Rust traps it -> T_Overflow. Pre-existing self-host gap (silent UB). Not in corpus (ggdef no shift). Separate parity item.
- Self-host msgless-cmp-assert uses STATIC msg via gorget_panic (Rust uses runtime values via gorget_assert_fail_values). Self-host never emits gorget_assert_fail_values -> NO 4-arg update needed. Divergence conformance-invisible (detail not compared).
- Fault-scope siblings (lower_closures.gg:93 closure-panic; lower_expr.gg:7412 fault-repanic via 7376/7385) are gorget_panic emits NOT in corpus. Reroute for class-completeness.
- Latent abort()/134 sites (lir_codegen.gg:4851 bounds, 4854 divzero-helper, 4857 generic) = T2b.
