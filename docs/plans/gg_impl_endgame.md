# Endgame Plan — `GG_IMPL` build-time compiler selector

**Status:** roadmap / endgame deliverable (owner-chosen 2026-06-19). NOT started. Gated on self-host runtime parity (north-star) reaching ~100% on fixtures + the self-host gaining a complete standalone-compiler surface (below). This is the deliverable the parity north-star unlocks: a self-hosted Gorget compiler you can build, install, and use on arbitrary programs.

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

## Current state (grounded, 2026-06-19)

- `tests/fixtures/self_host_lowerer/driver.gg` is ALREADY a CLI: `compile_main()` reads `args().get(1)` (input path) + `get(2)` (optional lib dir). But it only does **source → C** (emits C; does not link).
- The test harness does the rest: `Command::new(driver_exe).arg(fixture)` → captures the emitted C → `cc`s it → runs (`build_gg_dir("self_host_lowerer","driver.gg")`, `tests/integration.rs`).
- The Rust `gg` (`src/main.rs`) has `--backend=c|llvm` but **no** frontend/impl switch; commands `lex/parse/check/build/run`. It embeds the C runtime + `lib/std` sources via the resource schema (`include_str!`).
- Self-host source lives under `tests/fixtures/self_host_lowerer/` (a TEST fixture), not a shipped location.

## Sub-requirements for `GG_IMPL=selfhost` to be USABLE

The build-time/single-binary model removes the Rust-gg crutch, so the self-host binary must own the WHOLE pipeline. Required pieces (each its own scout→brief→review→execute when picked up):

1. **cc/link ownership.** `driver.gg` (or a thin self-host wrapper) must shell out to `cc` and produce the actual binary — today the harness does this. Mirror how the Rust `gg build` invokes the C compiler + links the runtime.
2. **Full CLI surface.** `gg build` / `run` / `check` (+ `-o`, flags, exit codes) in the self-host driver — today it's just "path → C on stdout".
3. **Runtime + stdlib bundling.** The self-host binary must carry the C runtime lib + the embedded `lib/std` `.gg` sources (the Rust `gg` does this via the resource schema). Decide: embed (like Rust) vs ship-alongside.
4. **Relocate the self-host source** out of `tests/fixtures/` to a shipped location (or embed), so the distribution can build it.
5. **The `GG_IMPL` orchestrator** (Makefile/build.sh/`gg bootstrap`) wiring stages 1–2 + install.

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
