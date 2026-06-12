# BRIEF — `thread_spawn(fn, stack_size)` consolidation (1) + sized compiler stack (A) + parse-time expr-depth limit (B)

Status: v3 (orchestrator, 2026-06-12; brief-review #1 SIGN-OFF-with-4-reservations folded: P1-keying scoped to one-size-per-fn, GirModule ctor site + corrected `.put` sibling set added, decomposition shared-file map fixed [E0+E1b share lower_expr.gg; E0+B-sh share the 3 parser.gg], B covers all 3 parser copies, line drifts corrected). Owner chose **(1)+(A)+(B)**: consolidate the
sized-stack primitive INTO `thread_spawn` (not a separate `run_with_stack`), which
makes the self-host default-arg fix a prerequisite. Supersedes v1's `run_with_stack`
design. Two scouts + an orchestrator re-measurement informed this; the P0/P1 design
scout (`afe384c7cdc69ae6f`) verified every site below against current source and
built+ran the bug repro. **NEEDS ≥3 fresh brief-reviews before launch; executor
launch is OWNER-GATED.**

## Mission
Make the gg compiler degrade gracefully on a pathologically deep expression instead
of SIGSEGV-ing, via:
- **(B)** a PARSE-TIME expression-nesting limit (`MAX_EXPR_DEPTH = 128`) → clean
  teaching error (à la clang `-fbracket-depth` / rustc `recursion_limit`).
- **(A)** the gg compiler runs its own compilation on a 512MB stack so it can lower
  everything up to that 128 limit without crashing (true-8MB crash is ~88).
- **(1)** (A)'s sized stack is delivered by extending the existing `thread_spawn`
  stdlib primitive with an optional `int stack_size = 0`, NOT a new function — which
  requires **P0: closing the self-host default-argument gap** (a real latent
  miscompile, fixed here as a prerequisite).

## Settled measurements (REGENERATED 2026-06-12; re-run before re-quoting — see [[feedback-measure-true-stack-not-container-default]])
Method: `( ulimit -S -s 8192; <self-host driver> <chain.gg abs> <lib abs> --lir-c )` (LOWER soft limit before exec; raising is a no-op + the container default ~24MB ≠ 8MB).
- **Self-host driver, TRUE 8MB: crash ≈ 88–90** (`var` and `lit` shapes identical; ~66KB/frame). Real-code max expr depth = **52** (`self_host_typechecker/derive.gg`).
- **512MB pthread: crash > 2400** (>4800 at 1GB before OOM). Sized stack widens the wall >25×.
- **Limit 128**: 2.46× over real-52; ~18× under the (A)-widened crash; below 88 ONLY with (A) → **(A) is required for a 128 limit** (a (B)-alone limit would have to be ~64).

---

# P0 — self-host default-argument support (PREREQUISITE; also fixes a parity bug)
**Bootstrap-safe (scout-verified):** the compiled self-host source + `lib/std/*` it imports use ZERO default-arg params (the only defaults are in `lib/xtd/http*`, never compiled by the bootstrap) → P0's lowering change is byte-neutral on `fixed_point`; it only affects NEW default-arg code.

**The bug (end-to-end verified):** `int f(int a, int b = 10): return a+b` + `print(f(5))` → Rust gg prints `15` (synthesizes `__v1 = 10`); the self-host emits `f(__v0)` — a 1-arg call to a 2-param fn, default silently dropped → garbage 2nd param. Grammar `docs/language-reference.md:586` spec's defaults, so this is a spec violation. The self-host has **no `WrongArgCount` check** (only a type-var-inference equality gate at `self_host_check/typecheck.gg:888`), so P0 is **fill-only — no "un-reject" work**.

**Rust reference model (the spec to mirror):** parser captures `Param.default: Option<Spanned<Expr>>` (`src/parser/mod.rs:1856-1860`, AST `src/parser/ast.rs:174`); the actual call-site FILL is in IR lowering `resolve_call_args` (`src/ir/lowering/exprs/calls.rs:386-445`, reading `ctx.fn_defaults` populated `mod.rs:680-688`) — it places positional/named args into a param-ordered slot array, then fills empty trailing slots with `default_expr.clone()`. (`rewrite.rs:522` only name-resolves *inside* the default expr — NOT the fill.)

**Distinct copies (md5-verified) — edit each:**
- `ast.gg`: 3 distinct — `self_host_parser`, `self_host_resolver`, `self_host_typechecker` (check + lowerer SYMLINK → typechecker).
- `parser.gg`: 3 distinct — `self_host_parser` (`53065d7f`), `self_host_resolver` (`37c6a2ec`), `self_host_typechecker` (`2059081b`) (check + lowerer SYMLINK → typechecker).

**Edit sites (scout-verified per copy):**
1. **AST `Param`** — add `Option[SpannedExpr] default` (idiomatic; precedent `Option[SpannedType] throws_type` on FnDef): `self_host_parser/ast.gg:159-162`, `self_host_resolver/ast.gg:164-167`, `self_host_typechecker/ast.gg:168-171`.
2. **`Param(...)` constructors — 22 arity-3 sites, add 4th arg `None()`** except the parse-capture site: `self_host_parser/parser.gg` {1990,1995,3150,3157,3164,**3193**,3218}; `self_host_resolver/parser.gg` {1943,1948,3125,3132,3139,**3168**,3193}; `self_host_typechecker/parser.gg` {2016,2021,2062,3359,3366,3373,**3420**,3448}. (Add an arm-count lint in `tests/lints.rs` so a new `Param(` site is forced to supply the field.)
3. **Parser — capture instead of discard** at the **bolded** sites: e.g. `self_host_parser/parser.gg:3189-3193` `if check_tok(TOK_EQ): advance(); self.parse_expr()` (discarded) → `Option[SpannedExpr] dflt = None(); if check_tok(TOK_EQ): advance(); dflt = Some(self.parse_expr())` then `Param(ty,pname,own,dflt)`. (resolver `:3164-3168`, typechecker `:3416-3420` — same shape.)
4. **Lowering registration** — add `Dict[String, Vector[Option[SpannedExpr]]] fn_defaults` field to `struct GirModule` (`self_host_lowerer/gir.gg:411-432`, beside `fn_borrow_params`/`fn_move_params`) AND the positional `GirModule(...)` constructor call (`self_host_lowerer/lower.gg:~2308`, ~31 positional args — self-catching arity error, but list it; review #1 caught the omission); populate in the pre-scan `for p in fdef.params` loop (`lower.gg:1997-2009`, add `defaults.push(p.default)`). ⚠ **Fix the CLASS:** register `fn_defaults` at EVERY `fn_borrow_params.put`/`fn_move_params.put` sibling — VERIFIED set this session (regenerate by grep, don't trust the list): `lower.gg` {`2005-2006` top-level, `2049-2050` equip, `2233` struct-ctor, `2252-2253` enum-variant-ctor, `2277`/`2279`/`2281`/`2297` prelude Some/Ok/Error/Box, `2719` mono-ctor, `2888-2889` equip-short-key} (the v2 `:2766` was a drift; the Rust analog has the same second mono'd site at `mod.rs:771`). Add a `tests/lints.rs` arm-count so a NEW `.put` site is forced to register defaults.
5. **Call-site FILL** — `self_host_lowerer/lower_expr.gg` `lower_call`, after `callee_param_types` resolves (`~:2832`) and AFTER the `for (ai, arg) in args.enumerate()` arg-lowering loop (`~:2873`): for `idx in args.len()..callee_param_types.len()`, look up `fn_defaults[call_name][idx]`; if `Some(expr)`, lower it with `expected_type = callee_param_types[idx]` and append the operand to `gir_args`. ⚠ **APPEND after the loop (don't mutate `args`); route the default exprs through the SAME expected-type + `op_consume` path the loop uses** (factor it) — else a sibling-drift ownership hole.
6. **(Optional, DEFER to follow-up TODO)** — `validate_default_param_ordering` (mirror `resolve.rs:316`) in `self_host_typechecker/resolve.gg:360-373` emitting `RequiredAfterDefault`. NOT load-bearing for parity (diagnostic only; no corpus fixture needs the rejection).

**P0 gates:** `self_host_bootstrap_fixed_point` GREEN + BYTE-IDENTICAL (verified-zero-defaults → must not move); the `default_params_basic`/`default_params_complex`/`named_args` fixtures now MATCH Rust under the self-host (`c_emit_comparison` + runtime parity should IMPROVE — confirm which were mismatched); a new self-host-runs-correctly fixture for `f(5)→15`; lib/lints.

---

# P1 — extend `thread_spawn` with `int stack_size = 0`
**Stdlib decl** (`lib/std/thread.gg:10`): `Thread[T] thread_spawn(T() fn)` → `Thread[T] thread_spawn(T() fn, int stack_size = 0)` (0 = OS default). Parses under Rust today; under the self-host only AFTER P0. ⚠ **One decl, shared zone** — have ONE executor own this line.

**⚠ Critical ordering (Rust):** the `thread_spawn` intrinsic check (`calls.rs:914-936`) runs on the RAW `args` BEFORE `resolve_call_args` (the default-fill). So the fill does NOT reach the intrinsic: `thread_spawn(fn)` arrives as **1 raw arg**, `thread_spawn(fn, N)` as **2**. The intrinsic must handle BOTH arities explicitly; the `= 0` default is cosmetic for the intrinsic path (the intrinsic supplies the 0-behavior).

**Rust intrinsic extension** (`src/ir/lowering/exprs/calls.rs:914-936` + emit `src/backend/c_lir/emit_types.rs:562-587`):
- KEEP the `args.len() == 1` branch **byte-identical** (emits today's `__gorget_thread_spawn_{fn}(void)` plain `pthread_create`) → the 5 existing 1-arg thread fixtures (`thread_basic`/`thread_atomic`/`thread_mutex`/`thread_barrier`/`sync_condvar`) don't shift on the Rust side.
- ADD an `args.len() == 2` branch: read `args[1]` as stack_size. **Const `0` → route to the SAME plain wrapper (byte-identical).** Non-zero → a stack-sized wrapper variant that does `pthread_attr_init` + `pthread_attr_setstacksize(&a, N)` + `pthread_create(&t,&a,...)` + `pthread_attr_destroy(&a)`.
- **Typed plumbing (layering §3 — one source of truth, NOT name-encoded):** add a `stack_size` field to `ThreadSpawnedFn` (`src/lir/mod.rs:1698-1703`), populated from `ctx.spawn.thread_fns` (`context.rs:118`) and threaded through the dedup build (`mod.rs:2300-2306`) into the wrapper emit. ⚠ **Keying — SCOPED to one stack size per spawned fn (the realistic case; review #1 found the fn-name keying spans 3 sites):** the emitted C symbol STAYS `__gorget_thread_spawn_{fn_name}` (`calls.rs:932`/`emit_types.rs:582`); the size rides as the typed `stack_size` field on that fn's entry (0 → today's plain wrapper, byte-identical; non-zero → +`setstacksize`). In the corpus each fn is spawned exactly once — `compile_main`@512MB is the ONLY sized spawn, the 5 thread fixtures are all plain — so NO collision. Spawning the SAME fn at two DIFFERENT non-zero sizes is **UNSUPPORTED in V1** (the fn-name dedup keeps one entry; encoding the size into the symbol name to support it is a follow-up TODO if ever needed). Do NOT add a wrapper parameter (would change the 1-arg signature and break byte-identity).

**Self-host intrinsic — BUILD FRESH (currently MISSING; this is also a parity win):** today the self-host lowers `thread_spawn(compute)` as a GENERIC user call → emits closure adapters + a call to an UNDEFINED `thread_spawn` C symbol → all 5 thread fixtures CC-FAIL/mismatch under the self-host. Add the intrinsic in `self_host_lowerer/lower_expr.gg` `lower_call` (beside the existing `fname ==` intrinsic dispatch where print/panic/len live): recognize `fname == "thread_spawn"`, extract the bare-fn-ref arg + optional stack_size, register into `thread_spawned_fns` (LIR plumbing EXISTS: `LirThreadSpawnedFn` `lir.gg:419,544,560`; codegen `lir_codegen.gg:6990`; `Thread__` join `lir_lower.gg:368`), emit `__gorget_thread_spawn_{fn}()`. ⚠ **The emitted wrapper text MUST be byte-identical to Rust's `emit_types.rs:572-585`** (gated by `c_emit_comparison` + `fixed_point`) — mirror the `(fn_name, stack_size)` keying exactly.

**P1 gates:** the 5 thread fixtures CC-FAIL/mismatch → MATCH on the self-host (parity win); Rust-emitted C for the 1-arg fixtures BYTE-IDENTICAL to today (no regression); `c_emit_comparison`/`fixed_point` agree on the wrapper.

---

# A — the compiler opts into the sized stack
- **Self-host driver** (`tests/fixtures/self_host_lowerer/driver.gg`): rename the `void main():` body to a top-level `void compile_main():`, and:
  ```gorget
  void main():
      thread_spawn(compile_main, stack_size = 512 * 1024 * 1024).join()
  ```
  `Thread[T].join()` exists (`lib/std/thread.gg`, returns `T`). The driver reads argv via the global `args()` (`driver.gg:28`), NOT main params, so the relocated no-arg `compile_main` works (`gorget_init_args` runs in the wrapper `main` first). `compile_main` must be a bare zero-arg fn ref (intrinsic constraint). 512MB is mmap'd/lazy (cheap); env-overridable later.
- **Rust gg driver** (`src/main.rs` compile path): wrap the compile entry in a sized `std::thread::Builder::new().stack_size(N).spawn(...).join()` (rustc pattern; env `GG_MIN_STACK`, default 512MB) — the Rust gg has the same cliff (~420 at 8MB), so this is real, not just symmetry.
- **⚠ Land-together + LLVM:** the driver opt-in CALL + BOTH `thread_spawn` emitters must land together (the driver self-emits its own `thread_spawn(compile_main,…)` → the self-host emitter must handle it or stage-1 breaks). Under `GG_BACKEND=llvm`, `thread_spawn`/the sized wrapper is a no-op direct-call (LLVM `@main` is plain, no thread_spawn handling, `fixed_point` `skip_under_llvm()` `tests/integration.rs:~14375`); a deep-expr SIGSEGV simply remains under LLVM — document, don't force.

**A gates:** `self_host_bootstrap_fixed_point` GREEN (driver self-emits `thread_spawn(compile_main,512MB)` + re-converges byte-identically — THE proof); `c_emit_comparison` lockstep; a deep-chain fixture (>88, was crashing) now COMPILES through the driver (proves the widened stack).

---

# B — parse-time depth guard (limit 128) — file-disjoint, PARALLEL
- **`const MAX_EXPR_DEPTH = 128`**. Track **AST-TREE depth, NOT call-stack** (the flat `a+a+…` chain is parsed ITERATIVELY in the Pratt loop, parser stack stays ~2 deep): a depth field incremented on `parse_prefix` entry/exit (parens/unary) PLUS a left-spine-count check inside the `parse_expr_bp_with_lhs` precedence loop (flat chains). The overflow is in *lowering*, but a parse-time reject fires before it (verified: `gg parse` depth-500 → rc=0).
- **Rust**: `src/parser/expr.rs` — add `expr_depth: usize` to `Parser` (precedent `call_arg_depth` `src/parser/mod.rs:43`, init `:99`); inc/check in `parse_prefix` (`:381`) + the `parse_expr_bp_with_lhs` loop (`:353`). New `ParseErrorKind::ExpressionTooDeep { depth, limit }` (`src/errors.rs`; mirror the `src/semantic/meta.rs:1478` "recursion limit exceeded" phrasing) + Display.
- **Self-host**: ALL THREE distinct `parser.gg` copies (class-consistency — E0 already touches all 3; review #1 flagged the 3rd) — ⚠ **per-copy lines DIFFER, grep each:** `self_host_parser/parser.gg` `parse_prefix` **:2018**, loop **:1595**; `self_host_typechecker/parser.gg` `parse_prefix` **:2102**, loop **:1571** (covers check+lowerer via symlink); `self_host_resolver/parser.gg` (the 3rd, driven by `resolver_comparison`) — grep its `parse_prefix`/`parse_expr_bp_with_lhs` sites. Self-host already has `call_arg_depth`/`speculation_depth` int counters on the parser (`:988`/`:994`) → `expr_depth` is an idiomatic addition. Error idiom (VERIFIED): `self.diagnostics.push(Diagnostic.error(self.current_span(), DkParseError(), "expression nesting too deep (max 128); split into intermediate variables"))` (cf. `parser.gg:1082/1086`) — the parser uses the Diagnostic mechanism (an earlier review's `self.errors.push` claim was spurious — no such field).
- **B fixtures**: over-limit (150-term chain) → clean error via `check_gg_fails` (`tests/integration.rs:~5592`); just-under-limit (127-term `1+1+…+1` → stdout `127`) compiles+runs.
- **B gates**: `parser_comparison` count-neutral on normal code; the 2 fixtures; lib/lints.

---

# Executor decomposition + dependency order
```
E0  (P0 self-host default args)  — ONE executor (E0a AST+parser + E0b lowering fill tightly coupled: E0b needs E0a's field)
      zone: self_host_{parser,resolver,typechecker}/{ast,parser}.gg + self_host_lowerer/{gir,lower,lower_expr}.gg
      RECOMMEND E0 ALSO absorbs the self-host depth-guard edits (B-sh below) — it already owns all 3 parser.gg copies.
      SERIAL PREREQ for E1/A.
E1a (P1 Rust thread_spawn ext)   — parallel after E0 (disjoint: src/ only); OWNS the one-line lib/std/thread.gg decl.
      zone: src/ir/lowering/exprs/calls.rs, src/backend/c_lir/emit_types.rs, src/lir/mod.rs (+ src/lir/lower)
E1b (P1 self-host thread_spawn)  — after E0; ⚠ REBASES on E0 (shares self_host_lowerer/lower_expr.gg: E0's
      default-fill at the arg-loop tail vs E1b's thread_spawn intrinsic near the `fname ==` dispatch — region-disjoint,
      SAME FILE). Leaves lib/std/thread.gg to E1a. Must emit the byte-identical wrapper (fn-keyed symbol + stack_size field).
      zone: self_host_lowerer/{lower_expr,lir,lir_codegen,lir_lower}.gg
A   (driver opt-in + Rust sized thread)  — after E1a+E1b
      zone: tests/fixtures/self_host_lowerer/driver.gg, src/main.rs
B-rust (Rust depth limit)  — FULLY PARALLEL, disjoint (src/ only)
      zone: src/parser/expr.rs, src/errors.rs
B-sh   (self-host depth limit)  — ⚠ NOT file-disjoint from E0: edits ALL THREE parser.gg copies
      (self_host_{parser,resolver,typechecker}) that E0 also edits — region-disjoint (B at parse_prefix/precedence-loop,
      E0 at Param-capture), but RECOMMEND FOLDING INTO E0 (one owner per parser.gg) rather than a separate executor.
```
⚠ **Shared-file map (review #1 corrected the "all file-disjoint" claim):** E0+E1b share `lower_expr.gg` (E1b rebases on E0); E0+B-sh share the 3 `parser.gg` copies (fold B-sh into E0). The only TRULY parallel-from-scratch executors are **E1a** and **B-rust** (both src/-only). Parent drives the integration sweep (`fixed_point` + full integration + `c_emit`/`parser`/`resolver` comparisons). Executors commit after build + lib + lints + their targeted fixtures.

## Process
≥3 fresh sequential brief-reviews of THIS brief → present + **OWNER-GATED launch** → fresh output-reviews → integrate. Each commit cites this brief + Co-Authored-By. Worktree-isolation + `git merge --ff-only gorget-1` preamble + explicit-file staging per "Multi-agent orchestration".

## Open decisions (resolved here; reviewers verify)
- Wrapper keying = `(fn_name, stack_size)` (byte-identical 0-path + no size collision). NOT a wrapper parameter (would break the 1-arg signature byte-identity).
- `validate_default_param_ordering` in the self-host = DEFERRED to a follow-up TODO (diagnostic-only, no corpus need).
- The self-host "silently mis-lowers a too-few-args call with no default" (no WrongArgCount) is pre-existing + orthogonal → separate TODO, out of scope.
