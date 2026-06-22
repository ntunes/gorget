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

## The fix (reference-grade, producer-centralized, layering-correct)
Hoist every per-instruction temp `alloca` **definition** to the function **entry block**
(`entry.prelude`) — LLVM's documented requirement for frontend-emitted allocas. **Do NOT** use the
prototype's string post-pass; use the production shape:

- Thread a `pending_entry_allocas: &mut Vec<String>` (or the self-host/Rust-appropriate buffer type)
  through `emit_inst` (`src/backend/llvm/mod.rs:3089`) and its helpers. When an arm currently writes a
  `%v{d} = alloca {ty}` / `%spill.N = alloca {ty}` **definition** line into `out`, instead PUSH that
  line onto `pending_entry_allocas`. The **follower** instructions that USE the alloca'd pointer
  (`memset`/`store`/`select`/`memcpy`/the call itself) STAY at the current call site — only the `alloca`
  *definition* moves.
- Flush `pending_entry_allocas` into `entry.prelude` in `emit_function`, AFTER the existing slot/StrLit
  entry allocas (`src/backend/llvm/mod.rs:2596-2639`). An alloca dominates all its uses once in the
  entry block, so every hoist is SSA-valid (the temps have statically-known sizes + unique value-id
  names, no value operands).

**The alloca sites to hoist** (all statically-sized → all hoistable; scout-enumerated — re-grep, line
numbers drift):
- sret-call destinations: `mod.rs:4110, 4120, 4190, 4256, 4707` (`%v{d} = alloca {ret_ty}`).
- scalar-key spills for map ops: `mod.rs:4898` (`%spill.N = alloca …` → `gorget_map_put`/`_get`).
- Option-construction temps: `mod.rs:4913, 4964, 4966, 5193, 5349, 5365`.
- HOF/combinator `%v{d} = alloca %GorgetString/%GorgetArray`: `mod.rs:4423, 4465, 4516, 4747, 4759, 4808`.
- fat-pointer `[2 x ptr]` allocas: `mod.rs:3281` (the prototype's `// skip array-of-ptr … conservative?`
  note was thinking-aloud — these ARE const-sized; **hoist them too**).
- `grep -n "= alloca" src/backend/llvm/mod.rs` and confirm EVERY non-entry-block alloca with a static
  size + no value operand is routed through the buffer. (Entry-block slot/StrLit allocas at `:2596-2639`
  are ALREADY correct — leave them.)

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
Threading the buffer through ALL emit paths without missing a site (a missed non-entry alloca = the bug
persists for that op) AND without hoisting a NON-static alloca (there should be none — confirm every
hoisted site is statically-sized; a runtime-sized alloca hoisted to entry would be wrong, but the scout
found none). Keep followers at the call site. Verify the LLVM bootstrap byte-identicality (the scout's
strongest evidence) reproduces.

## Also file (side findings — NOT this brief; add to TODO.md / fix on contact)
1. **`sizeof_struct_by_name` returns 160 for GorgetMap, canonical is 152** (`mod.rs:2088`; the `:2073`
   comment already flags the over-size). Harmless (over-alloc never under-reads) but a latent
   re-derivation smell — a typed size source would kill it.
2. **SEPARATE LLVM type bug:** `acc += someVector.get(0).unwrap()` (Option-payload `int` add) emits
   `llvm.sadd.with.overflow.i64(i64, ptr)` → `llc` type error (the payload arrives as `ptr`, not
   unwrapped to `i64`). Distinct from this crash; needs its own fixture + scout. Does NOT block bootstrap
   (the driver's own source doesn't hit it — bootstrap is byte-identical with just the alloca fix).
