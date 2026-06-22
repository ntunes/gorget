# Brief — LLVM backend: hoist per-instruction temp allocas to the entry block (fix the driver SIGSEGV)

**Track:** the "separate LLVM bug" — the self-host driver SIGSEGVs under `--backend=llvm` on its own
source in `coal_compute_live_blocks`/`gorget_map_get` (the reason `self_host_bootstrap_fixed_point` is
`skip_under_llvm()`-gated). Scout: `a09ff841` (2026-06-22, ROOT-CAUSED + fix PROTOTYPED to byte-identical
bootstrap output + 362 LLVM tests green).

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1` FIRST; verify pwd is
your worktree, NOT /workspace/gorget-1). `git add` ONLY the files you change. NEVER `git stash` (shared
stack — use `cp`). This is a BACKEND (LLVM) change — gate on a full LLVM integration sweep + the LLVM
bootstrap.

## Root cause (RUN-confirmed — NOT a map hash/ABI bug)
The crash is a **stack overflow**, not a bad map pointer. The LLVM backend emits per-instruction
temporary `alloca`s **inside loop-body basic blocks** (not the function entry block). Per LLVM
semantics, allocas outside the entry block are **never reclaimed across loop iterations** — they
accumulate for the whole call. `coal_compute_live_blocks` (`lir_codegen.gg:5390/5402`, a backward
dataflow fixpoint with nested `while` loops) runs millions of iterations when the driver compiles its
own ~660K-line module → a single frame consumed **~536MB** (measured by frame-pointer delta), exhausting
the 512MB compile thread; `gorget_map_get`'s prologue store is merely the leaf that first touches the
guard page. The C backend is immune (its temps are function-scope C locals reused each iteration).
DISTINCT from the union-payload offset bug (`392089a3`): that was an enum-field-offset name-match; this
is an alloca-lifetime/placement bug, no name-matching, no offsets.

**Why driver-source-only:** input-size-dependent. Corpus fixtures' loops run too few iterations to
exhaust the stack; only the ~660K-line self-compile drives the fixpoint to millions of iterations.

## The fix (reference-grade, ENUMERATION-FREE, body-buffered — revised per review pass 1)
Hoist EVERY per-instruction temp `alloca` **definition** in the function body to the function **entry
block** — LLVM's documented requirement for frontend-emitted allocas. **Do NOT enumerate sites** (review
pass 1 proved the hand-list missed ~30, incl. the load-bearing general-sret `mod.rs:6391` that
`coal_compute_live_blocks` actually hits, and `emit_branch_arg_casts:7049` which isn't even reachable via
`emit_inst`). And **do NOT** "flush into the entry prelude after :2639" — `emit_function` (`mod.rs:2321`)
*streams* the prelude directly into `out` (`:2576` `entry.prelude:`, slot/StrLit allocas `:2596-2639`,
`br label %bb0` `:2686`) BEFORE it loops over blocks (`emit_inst` at `:3002`), so there is no buffer to
flush into. Use the **body-buffering + line-extraction** shape instead:

1. **Emit the function body into a SEPARATE `String body_buf`,** not directly into `out`. In
   `emit_function`, the per-block loop (the `emit_inst` calls at `~:3002` AND the `emit_branch_arg_casts`
   call at `~:3022`) writes into `body_buf`. (The body-emitting HOF helper closures take an
   `out: &mut String` param — they'll write into `body_buf` because that's what gets passed down; no
   per-site threading needed.)
2. **Extract every alloca DEFINITION from `body_buf` by line** into a `hoisted: Vec<String>`: a body
   alloca def is a single line matching `^\s*%\S+ = alloca ` (all body allocas are single-line +
   statically sized + unique-named — review pass 1 + M1 confirmed NO runtime-count `alloca <ty>, i64 %reg`
   form exists anywhere). Move the matched line to `hoisted`; leave all FOLLOWER lines (the
   `memset`/`store`/`select`/`memcpy`/call that USE the pointer) in `body_buf`. An entry-block alloca
   dominates all uses, so every hoist is SSA-valid.
   - ⚠ **SAFETY:** if a line ever matches `= alloca <ty>, i64 %<reg>` (a RUNTIME-sized alloca — operand
     register, not a constant), do NOT move it (hoisting before its size operand is computed would be
     wrong). Review pass 1 confirmed none exist today; assert/guard so a future one isn't silently
     mis-hoisted.
3. **Assemble `out`** = (prelude header + the existing entry slot/StrLit allocas `:2596-2639`, emitted as
   today) + `hoisted` (the extracted body allocas, placed in the entry block AFTER the slot allocas,
   before `br label %bb0`) + `body_buf`. This leaves the entry-prelude allocas (`:2596-2639`), the
   run-once `main`-prologue allocas in `emit_global_runtime_init`/`emit_global_init_arg_llvm`
   (`mod.rs:1908/1967/2027`), and the straight-line `__clone` wrapper alloca (`:2305`) UNTOUCHED — they
   are not in `body_buf`.

This is NOT the prototype's fragile whole-`out` string post-pass — it is a structured extraction from a
*dedicated body buffer*, so it cannot disturb the prelude or globals, and it is **enumeration-free** (the
pattern catches 6391/7049/5436-5509/6295 and any future site automatically). The site list below is
EVIDENCE of the bug's breadth, NOT a checklist to patch.

**STRUCTURAL GUARD (required — the anti-regression ratchet):** after extraction, assert `body_buf`
contains **zero** `= alloca ` lines (every body alloca was hoisted). Wire this as a debug assertion in
`emit_function` AND, if practical, a `tests/lints.rs`-style check — so the next emit arm that introduces
a body alloca is caught structurally, not by the next SIGSEGV. (This is the "convert a recurring bug
class into an executable guard" invariant — the hand-list is exactly what it replaces.)

**Evidence of breadth (NOT a checklist — the line-extraction handles all):** sret dests
`mod.rs:4110/4120/4190/4256/4707` + the **general CallExtern sret `:6391`** (the one
`coal_compute_live_blocks` hits) + the general scalar-spill `:6295`; map-op spills `:4898`; Option temps
`:4913/4964/4966/5193/5349/5365`; HOF/combinator temps `:4423/4465/4516/4747/4759/4808` + the
helper-closure temps `:5436/5451/5472/5487/5504/5509`; fat-ptr `[2 x ptr]` `:3281`; format/spill temps
`:5864/5934/6018/6037/6176/6189/6331/6341/6429/6440/6475/6485/6811/6817`; and the branch-arg-cast spill
in `emit_branch_arg_casts` `:7049`. `emit_inst` spans `~:3089-6886`; everything alloca'd in that range +
`emit_branch_arg_casts` is body-scope.

## Guard (REQUIRED — must bite on x86_64, where CI runs)
The scout's repro: a ~25-line fixture with a `while i < 3_000_000:` loop that each iteration calls an
sret-returning helper + a `Dict.put`/`Dict.contains`, printing a final count. This overflows the **main
thread's default ~8MB stack** (no 512MB compile-thread needed) → it SIGSEGVs identically on x86_64 (the
bug is architecture-independent — alloca lifetime, not endianness/offset). Add it as
`tests/fixtures/llvm_alloca_loop.gg` (or similar) with deterministic stdout. Under the C backend it
passes; under `GG_BACKEND=llvm` it goes SIGSEGV→correct-output WITH the fix. This is a far cheaper guard
than the heavy `bootstrap_fixed_point`. (Author the fixture's expected output to what the program SHOULD
print — do not reshape to dodge.)

## The reference-grade close — LIFT the bootstrap LLVM skip
This crash is THE reason `self_host_bootstrap_fixed_point` is `skip_under_llvm()`-gated. The scout
verified the fix makes the driver compile its own source under LLVM to **byte-identical** output vs the
C backend (1,031,106 lines, zero diff) → bootstrap would pass. So: after the fix, **run
`GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` and confirm GREEN, then
REMOVE the `skip_under_llvm()` gate** (`tests/integration.rs:~14952` — re-grep; the bootstrap test's
skip). If for any reason bootstrap does NOT go green under LLVM (e.g. a deeper second bug surfaces),
KEEP the skip, file the residual, and still land the alloca fix + the cheap guard (the fix is correct
regardless). Do NOT lift the skip on an un-green bootstrap.

## Gates (your worktree; parent runs the full both-backend sweep)
- The new `llvm_alloca_loop` guard: SIGSEGV→correct under `GG_BACKEND=llvm`, passes under C.
- **`GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release -- --test-threads=4`** — full LLVM sweep GREEN (this is the load-bearing gate; the prototype showed 362-test samples green — run the whole thing).
- `GG_BACKEND=llvm … self_host_bootstrap_fixed_point` GREEN → lift the skip.
- `cargo test --lib`; the DEFAULT (C-backend) full integration GREEN (the fix must be C-backend-neutral — it only changes where LLVM alloca defs are emitted; the C backend path is untouched, but confirm).
- No `c_emit`/parity regression (backend-only change; C unaffected).

## Riskiest part
Getting the body-buffering refactor right (emit blocks into `body_buf`, not `out`) and the
line-extraction precise: move ONLY single-line static `alloca` defs, leave followers in place, and place
hoisted allocas in the entry block before `br label %bb0`. The structural guard (zero `= alloca` lines
remain in `body_buf`) is what makes "enumeration-free" safe — make sure it actually fires. Confirm NO
runtime-sized `alloca <ty>, i64 %reg` form is moved (none exist today; the guard must skip one if it ever
appears). Verify the LLVM bootstrap byte-identicality (the scout's strongest evidence) reproduces, and
that the C-backend output is unchanged (byte-identical — the change is LLVM-only).

## Also file (side findings — NOT this brief; add to TODO.md / fix on contact)
1. **`sizeof_struct_by_name` returns 160 for GorgetMap, canonical is 152** (`mod.rs:2088`; the `:2073`
   comment already flags the over-size). Harmless (over-alloc never under-reads) but a latent
   re-derivation smell — a typed size source would kill it.
2. **SEPARATE LLVM type bug:** `acc += someVector.get(0).unwrap()` (Option-payload `int` add) emits
   `llvm.sadd.with.overflow.i64(i64, ptr)` → `llc` type error (the payload arrives as `ptr`, not
   unwrapped to `i64`). Distinct from this crash; needs its own fixture + scout. Does NOT block bootstrap
   (the driver's own source doesn't hit it — bootstrap is byte-identical with just the alloca fix).
