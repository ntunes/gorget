# Brief — GG_IMPL bundling Inc-2: embed the runtime `.c` files → relocatable `gg-selfhost`

**Track:** GG_IMPL endgame sub-req 3 (runtime + `lib/std` bundling). **Inc-2** = embed the 62 core
`src/backend/c/runtime/*.c` files into the self-host driver via `embed_file` so a built `gg-selfhost`
carries its runtime and can `build hello.gg -o hello && ./hello` from ANY cwd with NO env vars. Scout
`a86b4318` (2026-06-22) PROTOTYPED + MEASURED the mechanism end-to-end on 4 files — relocatable build
WORKS, bootstrap survives, cost negligible. Inc-2 is the mechanical extension to all 62.

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1`; tip `96ff9ddf`+ —
INCLUDES embed_file Inc-1). `git add` ONLY the files you change. NEVER `git stash` (shared stack — `cp`).
This is a self-host FRONTEND change touching the self-compiled `driver.gg` → `bootstrap_fixed_point` is
the load-bearing gate.

## Current mechanism (verified)
- **`read_runtime(rdir, name)`** — `tests/fixtures/self_host_lowerer/lir_codegen.gg:6988` — does
  `read_file(rdir + "/" + name)`, exits(1) on empty. Called **78×** (feature-gated) by
  **`emit_runtime_preamble(LirModule &m, String runtime_dir)`** (`lir_codegen.gg:7180`).
- `runtime_dir` resolved in `driver.gg:309-312`: `getenv("GG_RUNTIME_DIR")` → fallback
  `"src/backend/c/runtime"` (repo-relative); `--runtime-dir=` overrides (`:345`).
- **62 `.c` files, 609 KB** in `src/backend/c/runtime/`. SQLite (8.8 MB), SDL, stb_image are vendored
  OUTSIDE `runtime/` (`../sqlite3/`, `../stb_image.h`, `lir_codegen.gg:7459/7485`) — **NOT embedded
  (stay disk-only / conditional, see below).**
- Rust reference: hand-listed `include_str!` constants in `src/backend/c/c_runtime.rs` +
  `GORGET_RESOURCES_PATH` disk-override (`src/resources.rs:42-53`).

## ⚠ The load-bearing constraint (scout-discovered — do NOT design around it wrong)
The meta-const pass evaluates ONLY top-level `module.items` (`src/semantic/meta.rs:476-478`);
`loader::merge_modules` nests imported modules in `Item::Module` wrappers (`loader.rs:1395`) that Phase-1
does NOT descend into. **So `meta String X = embed_file(...)` consts ONLY work in the ENTRY module.**
→ The `embed_file` runtime table + its builder MUST live in `driver.gg` (the entry), NOT a separate
module. (Confirmed by repro: a `meta String` embed const in an imported module → `undefined name`; the
same in the entry module works.)

## What to implement (mirror the scout's prototype, extended to 62)
1. **In `driver.gg`:** a `meta String RT_<name> = embed_file("<entry-relative path>/<name>.c")` const for
   EACH of the 62 `runtime/*.c` files (paths relative to the `driver.gg` entry dir — the scout used
   `"../../../src/backend/c/runtime/X.c"`; VERIFY the correct relative depth from the driver's location)
   + a `build_embedded_runtime() -> Dict[String, String]` mapping basename → contents. Hand-listed is
   acceptable (Rust's is too). The 62 names are the full `ls src/backend/c/runtime/*.c` set.
2. **`read_runtime` gains a `Dict[String,String] &embedded` param** (`lir_codegen.gg:6988`): consult the
   embedded dict FIRST, with the **disk-override escape hatch** preserved — `if getenv("GG_RUNTIME_DIR")
   == "": <use embedded>` else disk-read (mirrors Rust's `GORGET_RESOURCES_PATH`; so a dev can still
   point at on-disk runtime). A `--runtime-dir=` flag must also still force the disk path.
3. **Thread the dict** from the 3 driver callers (build/run/the `:305`-ish paths) through
   `emit_runtime_preamble` to all 78 `read_runtime` call sites.
4. **SQLite/SDL/GL/metal stay DISK-ONLY** (do NOT embed the 8.8 MB sqlite3.c into `driver.gg` — it would
   balloon the self-compiled driver source + the bootstrap). These remain on the disk/conditional path;
   Inc-4 handles SQLite separately (feature-gated).
5. **Add a `tests/lints.rs` arm-count ratchet** pairing the `embed_file` table against
   `ls src/backend/c/runtime/*.c | wc -l` so a NEW runtime file is forced into the table (else it'd
   silently fall back to disk and break relocatability). This is the "convert a recurring bug class into
   an executable guard" invariant.

## Guard (REQUIRED — relocatability is the whole point)
Add an integration test that builds a `gg-selfhost`, then from a DIFFERENT cwd (a tmpdir) with
`GG_RUNTIME_DIR`/`GG_LIB_DIR` UNSET runs `gg-selfhost build <import-free hello>.gg -o hello && ./hello`
and asserts it exits 0 + prints. Model on how `scripts/gg_impl.sh` + the existing `self_host_cli_pipeline`
test invoke the driver, but with the runtime dirs UNSET (the scout proved the baseline FAILS this,
patched PASSES). Also test the escape hatch: `GG_RUNTIME_DIR=<real>` still works. (Import-free `hello`
only — programs with `from std…` need Inc-3's `lib/std` embedding; note that in the test.)

## Gates (your worktree; parent runs the full both-backend sweep)
- The new relocatable-build test passes (foreign cwd, no env); the escape hatch works.
- `GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` GREEN — **the load-bearing gate**: the
  driver self-compiles with 62 large string literals now in its source; the self-host meta pass must
  evaluate all 62 `embed_file`s + re-converge. (Scout confirmed 4 works via stage-2 self-compile; 62 is
  more literal-heavy — watch compile time / RSS, bump the timeout if needed.)
- `cargo test --lib`; `self_host_runtime` 0 regressed; `lowerer_comparison`/`c_emit_comparison` no
  regression (the change is in the runtime-read path + driver, structurally neutral for emitted output
  of well-formed programs — the embedded bytes are identical to the disk bytes).

## Riskiest part
The `bootstrap_fixed_point` under 62 embedded literals (compile-time/RSS of the self-compiled driver) and
getting the entry-relative `embed_file` paths right for all 62. Keep SQLite OFF the embed path. If the
bootstrap compile time balloons unacceptably, that's a real finding — report it (a generated-manifest or
a seed-side `Item::Module` meta-recursion fix would be the heavier alternative).

## Downstream (NOT this brief)
- **Inc-3:** embed `lib/std/*.gg` via the SAME `embed_file` mechanism (same entry-module constraint),
  consulted in `load_imports` (`loader.gg:675`) — makes programs WITH `from std…` imports relocatable.
- **Inc-4:** SQLite (8.8 MB) — conditional/feature-gated, likely stays disk-only or a separate blob.
- After Inc-2+3: drop the `GG_RUNTIME_DIR`/`GG_LIB_DIR` exports from `scripts/gg_impl.sh` for a truly
  relocatable install.
- **Filed separately (TODO):** `run` mode execs the output by bare name (`driver.gg:448`
  `exec(output_path)`) → `gg-selfhost run hello.gg` from a foreign cwd can't find `hello` (needs `./`
  prefix / absolute path). `build` + manual `./hello` is unaffected. Small fix, not this brief.
