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
2. **`read_runtime` gains `&embedded` + an `explicit` signal** (`lir_codegen.gg:6988`), with a **3-STATE
   precedence (review pass-1 B1 — the naive `getenv("GG_RUNTIME_DIR")==""` check is WRONG).** By the time
   `read_runtime` runs, `runtime_dir` has ALREADY absorbed flag/env/default (`driver.gg:309-312` env,
   `:345` flag), so it cannot tell a `--runtime-dir=` flag from the default — and **9 `--runtime-dir=`
   test sites** (`tests/integration.rs:16162/16194/16212/16454/16793/17461/25003/…`) pass the flag WITHOUT
   setting `GG_RUNTIME_DIR`, so the naive check would silently feed them embedded bytes (a shipped no-op-
   flag defect). Required precedence: **explicit `--runtime-dir=` flag (disk) > `GG_RUNTIME_DIR` env
   (disk) > embedded (default).** Compute `bool runtime_dir_explicit = (--runtime-dir given) OR
   (getenv("GG_RUNTIME_DIR") != "")` IN THE DRIVER and pass it down. Then `read_runtime`:
   `if explicit: read_file(rdir+"/"+name)` (disk, exactly as today) `else:` look up
   `embedded[basename(name)]` → **on HIT use it; on MISS fall through to disk** (review pass-1 B2 —
   REQUIRED: `read_runtime` is ALSO called with `../stb_image.h`, `../sqlite3/sqlite3.c`,
   `../sqlite3/gorget_sqlite.c` at `lir_codegen.gg:7459/7485/7487`, NOT in the 62-set; a dict-ONLY read
   returns empty → `exit(1)` → breaks every SDL/SQLite/image fixture. Miss-fallback-to-disk keeps them
   working; an import-free `hello.gg` never triggers those feature-gated reads, so it stays relocatable).
   Mirrors Rust's `GORGET_RESOURCES_PATH`.
3. **Build the dict ONCE in `emit_runtime_preamble` — NO caller threading (review pass-1 M3).** All 78
   `read_runtime` calls live inside the SINGLE function `emit_runtime_preamble`
   (`lir_codegen.gg:7180-7501`), so build `build_embedded_runtime() -> Dict[String,String]` at its top
   and pass `&embedded` to each call — 78 mechanical 1-arg edits in ONE function, zero cross-function
   plumbing. ONLY the `explicit` bool (item 2) needs to reach `emit_runtime_preamble` from the 3 driver
   callers (a single `bool` param, not a dict).
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
The 3-state precedence (B1) + the dict-miss-fallback (B2) — get those wrong and you ship a no-op `--runtime-dir`
flag or break SDL/SQLite. And `bootstrap_fixed_point` under 62 embeds: **risk framing (review pass-1 M4) —
`driver.gg`'s SOURCE only gains 62 tiny `embed_file("path")` call expressions; the 609 KB materializes
IN MEMORY at meta-eval (`read_file`, `meta.gg:614`) — exactly as `read_runtime`'s disk reads do today.**
The real cost is transient meta-pass RSS + the post-subst AST holding ~609 KB of inlined string literals,
NOT source-file bloat — so the bootstrap risk is real but LOWER than "62 huge literals in the source"
implies. **Measure INCREMENTALLY (4 → 20 → 62)** — if `subst_stmts` String-clones or Dict growth balloon
RSS non-linearly, catching it at 20 is far cheaper than at 62. Keep `GG_BUILD_TIMEOUT_SECS=600`. Get the
entry-relative `embed_file` paths right for all 62. Keep SQLite OFF the embed path.
**Seed-fix alternative (review pass-1 M5) — correctly deferred, and it's NOT just "low-risk recursion":**
`embed_file` resolves relative to a single per-compilation `ctx.source_dir` (`meta.rs:1095`), not
per-module, so making imported-module embeds work without a per-module source_dir is a path-resolution
footgun that must land in BOTH compilers without bootstrap divergence. Driver.gg-as-home is genuinely the
lower-risk choice.

## Downstream (NOT this brief)
- **Inc-3:** embed `lib/std/*.gg` via the SAME `embed_file` mechanism (same entry-module constraint),
  consulted in `load_imports` (`loader.gg:675`) — makes programs WITH `from std…` imports relocatable.
- **Inc-4:** SQLite (8.8 MB) — conditional/feature-gated, likely stays disk-only or a separate blob.
- After Inc-2+3: drop the `GG_RUNTIME_DIR`/`GG_LIB_DIR` exports from `scripts/gg_impl.sh` for a truly
  relocatable install.
- **Filed separately (TODO):** `run` mode execs the output by bare name (`driver.gg:448`
  `exec(output_path)`) → `gg-selfhost run hello.gg` from a foreign cwd can't find `hello` (needs `./`
  prefix / absolute path). `build` + manual `./hello` is unaffected. Small fix, not this brief.
