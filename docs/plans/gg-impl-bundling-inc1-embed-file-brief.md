# Brief — GG_IMPL bundling Inc 1: teach the self-host `embed_file` (the endgame prerequisite)

**Track:** GG_IMPL endgame sub-req 3 (runtime + `lib/std` bundling → relocatable single binary).
This is **Increment 1**, the gating prerequisite: the self-host CANNOT compile a driver that calls
`embed_file` today, so nothing downstream (embedding runtime `.c` / `lib` `.gg`) is possible until
this lands. Scout: `a509198489` (design verified by RUNNING; `embed_file` measured).

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1`). `git add` ONLY
the files you change. NEVER `git stash` (shared stack — use `cp`). This is a self-host FRONTEND
change → `bootstrap_fixed_point` is the load-bearing gate (the driver self-compiles its own meta code).

## What `embed_file` is (the reference)
`embed_file(path)` is an EXISTING Rust-gg `meta` builtin (Gorget's `include_str!`): at compile time
it reads a file path relative to the source dir and returns a `String` constant. Rust impl:
`src/semantic/meta.rs:1086-1106` (reads relative to `ctx.source_dir`, returns `MetaValue::Str`).
Documented: `docs/language-reference.md:5280`, `docs/book/17-meta.md:265`. Tested:
`tests/fixtures/embed_file.gg` + `tests/integration.rs:17630`. **Measured:** embedding 609KB compiles
in 0.16s in Rust gg, no blowup.

## The gap (RUN-confirmed)
The self-host `meta.gg` (`tests/fixtures/self_host_lowerer/meta.gg`, ~:593-605 — the `ECall` arm
under `type_name == "str"|"String"`) only evaluates `meta String X = <EStringLiteral>` or
`typename(...)`. Feeding it `meta String P = embed_file("…")` → `[bug] EIdentifier: unknown
identifier 'P' — returning OpConstI64(0)` → the program outputs `0` instead of the byte count. So the
self-host meta evaluator must learn `embed_file`.

## What to implement
1. **Add an `embed_file` case to the self-host meta `ECall` arm** (`meta.gg` ~:597-605, mirror the
   adjacent `typename`/`str` handling + Rust `meta.rs:1086-1106`): when `fn_name == "embed_file"`,
   extract the string-literal arg, `read_file(source_dir + "/" + path)`, and store the contents into
   the `str_consts[mc_name]` table at the FILL site (`meta.gg:~596`, right next to the existing
   `meta String X = "lit"` → `str_consts[mc_name] = s` fill) — so the existing `subst_expr` machinery
   (the SUBST site, `meta.gg:~461-462`, which inlines `str_consts` entries) picks it up as an
   `EStringLiteral` unchanged. Match Rust's behavior: path is relative to the SOURCE dir; a missing
   file is a clean compile error (not a silent 0).
2. **Thread the ENTRY SOURCE DIR into the self-host meta pass.** Resolution is confirmed (review
   pass-1, by RUNNING Rust) = the ENTRY source file's directory (`src/main.rs:488/1500/...`
   `source_dir = Path::new(filename).parent()`); since NO `embed_file` calls live in imported lib
   files, per-file-vs-entry-dir never diverges — use the entry source dir. `expand_meta_types(Module
   &m, TypeTable &types)` (`meta.gg:~575`) carries NO source dir → add a `source_dir` param.
   **Compute it at the DRIVER, not the loader (R2):** the loader's `base_dir` (`loader.gg:585`) is
   LOCAL to `load_imports` and NOT in scope at the `expand_meta_types` call. At each driver call site
   compute the source dir via `std.path` (NOT the loader's `parent_dir` — see the per-driver imports
   below) and **bind it to a `String` VAR-DECL first**, then pass that local:
   ```
   String entry_source_dir = path_parent(path_absolute(<entry_var>))
   expand_meta_types(&m, &types, entry_source_dir)
   ```
   ⚠ The VarDecl binding is REQUIRED (pass-4): `path_parent`/`path_absolute` return `cstr`, and the
   self-host's cstr→String coercion (`gorget_str_from_cstr`) fires ONLY at an `SVarDecl` with a
   `String` declared type + a direct cstr-returning `ECall` init (`lower_stmt.gg:178`,
   `is_cstr_returning_call` `lower_types.gg:2262`) — NOT in argument position. Inlining
   `expand_meta_types(…, path_parent(path_absolute(path)))` would pass a raw `cstr` to the
   `String source_dir` param = a self-host miscompile. (The `meta.gg` arm does `source_dir + "/" + path`,
   a String concat, so `source_dir` MUST be a real `String`.)
   - **⚠ SIBLING-SITES / BUILD-BREAKER (R1, CORRECTED pass-2 — there are FOUR sites across THREE
     drivers, not three across two):** `meta.gg` is a SINGLE physical file (md5 `444d9a43`) SYMLINKED
     into THREE self-host dirs (`self_host_typechecker/` = the real file; `self_host_lowerer/` +
     `self_host_check/` = symlinks); each `driver.gg` is a REAL separate file. The `expand_meta_types`
     signature change MUST be propagated to ALL FOUR call sites across THREE real driver files:
     **`self_host_check/driver.gg:66`**, `self_host_typechecker/driver.gg:74`,
     `self_host_lowerer/driver.gg:124`, AND `:305`.
     **`grep -rn "expand_meta_types(" tests/fixtures/self_host_*`** to be exhaustive — a missed site
     breaks bootstrap + `type_comparison`/`lowerer_comparison`/**`check_comparison`** (add
     `check_comparison` to the gate list).
   - **Per-driver IMPORTS + the source-dir computation (R2/R3, CORRECTED pass-3 — use `std.path`
     UNIFORMLY; `parent_dir` is loader-specific and NOT reachable from the typechecker driver):**
     - `read_file` (for the `meta.gg` arm): `meta.gg` imports no fs today → add `from std.fs import read_file`.
     - **Compute the entry source dir via `std.path` in ALL THREE drivers** (it's reachable everywhere;
       avoids `parent_dir`, which the typechecker driver — having NO loader in its module graph — cannot
       import): `from std.path import path_parent, path_absolute` (VERIFY the exact exported names +
       signatures in `lib/std/path.gg` — `path_absolute` returns `cstr`), then
       `path_parent(path_absolute(<entry_var>))`. NONE of the 3 drivers import `std.path` today → add the
       import to each. (Do NOT use the loader's `parent_dir`/`base_dir` — unreachable from the typechecker.)
     - **Use the entry-path variable IN SCOPE at each site:** lowerer `:124` has `path`; lowerer `:305`
       has `input_path`; read the check `:66` + typechecker `:74` sites for their entry-var names — a
       literal `…(path)` won't compile at `:305`.
     - **Resolution-fidelity note:** Rust uses `Path::new(filename).parent()` (relative, no
       `path_absolute`); `path_parent(path_absolute(entry))` yields an ABSOLUTE dir but reads the SAME
       file after `join(rel_path)` — functionally equivalent (proven: `embed_file.gg` is in the
       `type_comparison`/`check_comparison` corpus + embeds real sibling files, so all 3 drivers need a
       REAL source dir, not a sentinel). Do NOT "fix" it.
3. Keep it MINIMAL and faithful — this increment ONLY adds `embed_file` evaluation; it does NOT yet
   convert any runtime/lib read site (that's Inc 2/3).

## Guard (REQUIRED — every fix lands with an exercising fixture)
Add a self-host test exercising `embed_file`: a `.gg` fixture that `embed_file`s a small known file
and prints its byte length (or content length), compiled by the SELF-HOST driver, asserting the
output is the correct count (NOT `0`). Model on the existing Rust `embed_file.gg` fixture +
`tests/integration.rs:17630`. Prove-it-bites: on the un-patched self-host it prints `0` / `[bug]`;
with the fix it prints the right count. (You may reuse/point at an existing small fixture file to
embed, to avoid a fragile path.)

## Gates (your worktree; parent runs the full both-backend sweep)
- The new `embed_file` self-host test passes (self-host compiles + runs it correctly).
- `GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` — **GREEN** (the driver self-compiles
  its own meta code; the new arm must lower identically and re-converge). This is the load-bearing gate.
- `cargo test --lib`; `self_host_runtime` 0 regressed; `lowerer_comparison`/`c_emit_comparison`/
  `type_comparison`/**`check_comparison`** no regression (the meta-pass change is frontend + touches
  ALL THREE symlinked drivers; confirm it's structurally neutral for everything that doesn't use
  `embed_file`).
- `embed_file_comparison`-style check if one exists, else confirm Rust's `embed_file.gg` still passes.

## Riskiest part
The `source_dir` threading + matching Rust's path-resolution semantics exactly, AND keeping
`bootstrap_fixed_point` green (the meta pass is on the self-compilation path). Gate hard on
`fixed_point`. If the path-resolution semantics are ambiguous, RUN the Rust `embed_file.gg` to see
exactly what dir it resolves against and match it.

## Downstream (NOT this increment — for context)
Inc 2: convert `read_runtime` (`lir_codegen.gg:~6904`) to embedded `meta String` constants (62
runtime `.c` files) → relocatable `gg-selfhost build hello.gg` from a foreign cwd with no env vars.
Inc 3: convert `load_imports`' `read_file` (`loader.gg:~675`) to an embedded module table (59 `lib`
`.gg`). Inc 4: SQLite (8.8MB, conditional). Keep a disk-override escape hatch (env / `--runtime-dir`
/ `--lib-dir`) throughout, mirroring Rust's `GORGET_RESOURCES_PATH`. NOT a SCHEMA_VERSION concern
(raw file contents, not the typed `resources.gg` table).
