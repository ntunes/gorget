# Endgame Plan — `GG_IMPL` build-time compiler selector

**Status:** roadmap / endgame deliverable (owner-chosen 2026-06-19). IN PROGRESS — sub-reqs 1 (cc/link, `d9059114`) + 5 (orchestrator `scripts/gg_impl.sh`) landed; sub-req 2 (CLI surface) PARTIAL. Still gated on self-host runtime parity (north-star) reaching ~100% on fixtures + the self-host gaining a complete standalone-compiler surface (below). This is the deliverable the parity north-star unlocks: a self-hosted Gorget compiler you can build, install, and use on arbitrary programs.

## Goal

A **build-time** selector that produces a **single** installed `gg` binary that IS one implementation:

```
GG_IMPL=rust      <build>  →  installs the Rust gg          (cargo/rustc artifact)
GG_IMPL=selfhost  <build>  →  installs the self-host gg     (Gorget driver.gg compiled to a binary)
```

No runtime `--frontend` flag, no bundling-both, no exec-delegation. The chosen binary IS the compiler. Owner preference (2026-06-19): this is cleaner and more principled than a runtime dispatcher — a genuine self-hosted compiler should STAND ALONE, not be a delegate the Rust `gg` shells out to.

## The bootstrap reality (keep this straight)

`GG_IMPL` selects the **installed** binary, NOT whether Rust touches the **build**. The build always needs a **seed** to compile `driver.gg`:

```
cargo build ─────────────► Rust gg   (seed; ALWAYS built first)
   │
   ├─ GG_IMPL=rust:     install the Rust gg as `gg`.  [stage stops here]
   └─ GG_IMPL=selfhost: Rust gg compiles driver.gg → gg-selfhost; install THAT as `gg`.
```

So `GG_IMPL=selfhost` is a two-stage build (seed → self-host); `GG_IMPL=rust` is one stage. The seed is build-only in the selfhost case. (Later distribution option: replace the Rust-gg seed with a checked-in stage0 self-host binary blob — the GHC/rustc model — to drop the Rust toolchain from the build entirely. Not required for `GG_IMPL`; decide separately.)

This is the standard compiler bootstrap (rustc's first compiler was written in OCaml; once it self-compiled, the seed's job was done). The `self_host_bootstrap_fixed_point` test already proves the self-host reproduces itself byte-for-byte — i.e. stage0→stage1→stage2 convergence is in place.

## Where the selector lives

A **build/install orchestrator** — a `Makefile` / `build.sh` / a `gg bootstrap` subcommand — reads `GG_IMPL` and runs the right stages. NOT a rustc `cfg`: "build the self-host" is a two-stage process cargo can't express on its own.

## Current state (grounded, 2026-06-22)

- `tests/fixtures/self_host_lowerer/driver.gg` is a STANDALONE CLI: `compile_main()` dispatches `build` / `run` / `check` subcommands (plus the unchanged legacy `<path> <lib_dir> [--emit-*]` test-harness path).
  - **Sub-req 1 (cc/link) — LANDED (`d9059114`).** `run_build_mode` owns the full source→C→cc→link pipeline (shells `cc` via `std.process.exec`), produces a real binary; `run` execs it and propagates the exit code.
  - **Sub-req 2 (full CLI surface) — PARTIAL (this increment).** Added: `check` (parse+resolve+typecheck, `has_errors`→exit code, no codegen — interim-permissive pending the filed typechecker diagnostic gap, see below); `--help`; `--backend=<b>` (accept `c`, reject `llvm` cleanly — the self-host emits C only); and `--emit-c`/`--lir-c`/`--emit-gir`/`--emit-lir` passthrough (dump-and-exit, mirroring the legacy path + Rust gg's `--emit-*`). Guarded by the `self_host_cli_pipeline` integration test (build/run/check/--help end-to-end), the first non-manual guard for the pipeline. Remaining sub-req-2 polish: richer flag coverage, surfaced diagnostics once the typechecker stops being permissive.
- The test harness can still drive the legacy `--emit-c | cc | run` flow (`build_gg_dir("self_host_lowerer","driver.gg")`, `tests/integration.rs`); the new subcommand path is additive.
- ⚠ **`check` is interim-permissive:** the self-host typechecker accepts many ill-typed programs (most of Rust's `self.error(...)` check sites are unmigrated), so `check` exits 0 on programs Rust rejects. The CLI plumbing is correct (surfaces whatever `has_errors` reports); the gap is the typechecker's, filed in TODO.md ("gg check PERMISSIVENESS"; "explicit-VarDecl path skips initializer inference"; "42 of 47 Rust self.error sites unmigrated") and NOT fixed by this CLI work. The `#[ignore]`'d `self_host_check_rejects_illtyped` test is the live breadcrumb that flips green when the typechecker is fixed.
- The Rust `gg` (`src/main.rs`) has `--backend=c|llvm` but **no** frontend/impl switch; commands `lex/parse/check/build/run`. It embeds the C runtime + `lib/std` sources via the resource schema (`include_str!`).
- Self-host source still lives under `tests/fixtures/self_host_lowerer/` (a TEST fixture), not a shipped location (sub-req 4).

## Sub-requirements for `GG_IMPL=selfhost` to be USABLE

The build-time/single-binary model removes the Rust-gg crutch, so the self-host binary must own the WHOLE pipeline. Required pieces (each its own scout→brief→review→execute when picked up):

1. **cc/link ownership.** ✅ LANDED (`d9059114`). `driver.gg`'s `run_build_mode` shells out to `cc` (via `std.process.exec`, `$CC`-aware) and produces the actual binary; `run` execs it and propagates the exit code.
2. **Full CLI surface.** 🟡 PARTIAL. `gg build` / `run` / `check` (+ `-o`, `--backend`, `--emit-*`, `--runtime-dir`/`--lib-dir`, `--help`, exit codes) are in the self-host driver. `check` is interim-permissive pending the filed typechecker diagnostic gap. Remaining: surfaced diagnostics, broader flag/error coverage.
3. **Runtime + stdlib bundling.** ❌ NOT done. The self-host binary must carry the C runtime lib + the embedded `lib/std` `.gg` sources (the Rust `gg` does this via the resource schema). Today they're located via `--runtime-dir`/`--lib-dir` (or `$GG_RUNTIME_DIR`/`$GG_LIB_DIR`, set by the orchestrator wrapper). Decide: embed (like Rust) vs ship-alongside. The relocatable-install in (5) is blocked on this.
4. **Relocate the self-host source** ❌ NOT done. Move out of `tests/fixtures/` to a shipped location (or embed), so the distribution can build it.
5. **The `GG_IMPL` orchestrator** (Makefile/build.sh/`gg bootstrap`) wiring stages 1–2 + install. 🟢 MOSTLY done — `scripts/gg_impl.sh` builds the seed, compiles `driver.gg` → `gg-selfhost`, and installs a thin wrapper that pre-sets `GG_RUNTIME_DIR`/`GG_LIB_DIR` to the repo paths. The RELOCATABLE install (a binary usable away from the repo tree) is blocked on (3) — the wrapper currently points the runtime/lib dirs back at the repo.

## Gating

- **Parity → ~100% on fixtures (necessary, not quite sufficient).** Fixtures are a proxy; arbitrary programs will hit language-surface combinations the corpus doesn't cover. Trusting `GG_IMPL=selfhost` on *any* program is a stronger bar than the fixture number — plan for a real-program soak (see accelerator below).
- **Self-host CLI completeness** (sub-requirements 1–4) — independent of the parity number; can proceed in parallel.

## Near-term accelerator (does NOT require the full endgame)

A standalone self-host that can compile arbitrary programs is itself the best parity ACCELERATOR: today we find gaps via the curated fixture corpus; a usable `gg-selfhost` lets us throw REAL programs (the compiler's own non-driver code, `examples/`, scratch) at it and surface gaps the corpus never will. We can dogfood NOW by building `gg-selfhost` and running it directly (the harness already does exactly this on every parity measurement) — no need to wait for the full `GG_IMPL` packaging. Sub-requirement 1 (cc/link ownership) is the smallest enabling step and pays for itself in faster gap-finding.

## Sequencing (suggested)

1. cc/link ownership in the self-host driver (smallest, unblocks dogfooding). 
2. Dogfood real programs through `gg-selfhost` → feed the gaps into the parity backlog (accelerator).
3. Full CLI + runtime/stdlib bundling + source relocation (in parallel with parity work).
4. The `GG_IMPL` orchestrator + install, once parity + CLI completeness justify shipping `GG_IMPL=selfhost`.
