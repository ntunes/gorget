# Brief — GG_IMPL bundling Inc-3: embed `lib/std/*.gg` → relocatable for programs WITH imports

**Track:** GG_IMPL endgame sub-req 3 (final piece). **Inc-3** embeds the 28 `lib/std/*.gg` modules into
the self-host driver via `embed_file` (the SAME proven mechanism as Inc-2 `e0abc60c`), so a program with
`from std.collections import Vector` builds + runs relocatably from any cwd with no env. Scout
`a9872f97` (2026-06-22) PROTOTYPED the FULL 28-file embed end-to-end — `bootstrap_fixed_point` GREEN
(289s), byte-identity holds, all guards green. This is a measured-green tight prototype on a proven
pattern → **collapse: re-apply + finalize guards → output-review → integrate** (skip the 3-pass
brief-review the novel Inc-2 needed; the precedence/fallback/entry-module pattern is already vetted).

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1`). `git add` ONLY the
files you change. NEVER `git stash`. Bootstrap-sensitive (driver.gg is self-compiled) → `fixed_point` is
the load-bearing gate.

## The proven design (mirror Inc-2; deltas spelled below — re-grep all line numbers)
**Read the LANDED Inc-2 brief `docs/plans/gg-impl-bundling-inc2-runtime-brief.md` as the template** (the
3-state precedence, dict-miss-fallback, the entry-module constraint, the lint ratchet, the byte-identity
verify all carry over). Inc-3 deltas:

1. **The read site is `load_imports`** (`tests/fixtures/self_host_lowerer/loader.gg:573`); the disk read
   is `read_file(file_path)` at **`loader.gg:675`** inside the worklist `while pi < import_paths.len()`
   loop (`:654`). ⚠ On a not-found import the loop SILENTLY SKIPS (no error at `:674`) → a relocatable
   build SILENTLY MISCOMPILES (`from std.math import PI` → `unknown identifier 'PI'` → `OpConstI64(0)` →
   prints `0`). The embed fixes this for the 28 std modules; the silent-skip-on-true-miss stays (matches
   today's behavior for a genuinely-absent module — do NOT change it to a hard error in this increment).
2. **File set: 28 `lib/std/*.gg`, ~129 KB** (`ls lib/std/*.gg`). NOT `lib/xtd` (31 files/876 KB — most
   pull SQLite/SDL/GL externs = a SEPARATE later increment; keep Inc-3 to `lib/std` only).
3. **`meta String LIB_<name> = embed_file("../../../lib/std/<name>.gg")` table + `build_embedded_lib()
   -> Dict[String,String]` in `driver.gg`** (the ENTRY module — entry-module constraint identical to
   Inc-2; verified it works there). **⚠ KEY DELTA: the dict is keyed on the NORMALIZED MODULE PATH**
   (`"std.collections"`, `"std.socket"`), mirroring `resolve_module_path`'s `std.net.X`→`std.X` rewrite
   (`loader.gg:17`) — NOT a bare filename. Match the exact key the lookup will use.
4. **`load_imports` gains `Dict[String,String] &lib_embedded` + `bool lib_explicit`**, threaded from the
   3 callers: `compile_main` (`driver.gg:294`, lib-dir FLAG-ONLY positional, no env), `run_build_mode`
   (`:554`, `GG_LIB_DIR` env→`--lib-dir=`→default), `run_check_mode` (`:672`, env→flag→default). Compute
   `lib_explicit` PER-SITE (flag-only for compile_main; flag-or-env for the other two — same pattern as
   Inc-2's `runtime_dir_explicit`).
5. **Precedence (verify carefully — slightly richer than Inc-2 because of local-module shadowing):** the
   existing disk resolution order is local `base_dir` → `lib_dir` → project-root (`resolve_module_path`).
   The embedded dict must be consulted such that: a **LOCAL user module shadows an embedded `std` module**
   (local-disk wins), an explicit `--lib-dir=`/`GG_LIB_DIR` forces the DISK path (not embedded), and only
   when neither a local nor an explicit-lib-dir file resolves do we use the embedded `std` copy, with a
   **disk-miss-fallback** for anything not in the 28-set. RUN the scout's checks: `--lib-dir=<bogus>`
   must force disk (NOT a silent no-op); `GG_LIB_DIR=<real>` reads disk; default reads embedded.
6. **Transitive imports need NO special handling** — `load_imports`'s recursive worklist
   (`loader.gg:686-691`) re-pushes each loaded module's nested imports, which re-enter the embedded
   consultation automatically (scout verified `encoding`→`collections`+`conv` resolved from embed, output
   matched Rust gg). Do NOT add a transitive-closure helper.
7. **`self_host_check/` has an INDEPENDENT `loader.gg` copy** (different md5) whose driver does NOT carry
   the embed table — OUT OF SCOPE for Inc-3. Note it, do NOT touch it.

## Guards (REQUIRED — finalize what the scout sketched)
- **Integration test** (mirror Inc-2's `self_host_relocatable_embedded_runtime`): build a `gg-selfhost`,
  then from a tmpdir with `GG_LIB_DIR`+`GG_RUNTIME_DIR` UNSET, `gg-selfhost build <std-importing>.gg -o x
  && ./x` → correct output (e.g. a `from std.math import PI; print(PI)` program → `3.14159…`). Prove-it-
  bites: the baseline SILENTLY prints `0`. Test the escape hatch (`GG_LIB_DIR=<real>` works;
  `--lib-dir=<bogus>` forces disk + fails). (Inc-2 already embeds the runtime `.c`, so a std-importing
  program is now FULLY relocatable — confirm end-to-end.)
- **`tests/lints.rs` arm-count ratchet:** the `LIB_` table count == `ls lib/std/*.gg | wc -l` (28), so a
  new `lib/std` module is forced into the table (mirror the Inc-2 `self_host_embedded_runtime_table_count`
  ratchet).
- **BYTE-IDENTITY verify:** an embedded-path build's loaded module bytes == a `--lib-dir=<disk>` build's
  (the embedded `.gg` bytes == disk bytes → emitted output unchanged for on-disk builds).

## Gates (your worktree; parent runs the full both-backend sweep)
- The new relocatable-import test passes (foreign cwd, no env); escape hatch + flag-forces-disk work.
- **`GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` GREEN** (scout saw 289s — the driver
  self-compiles with 28 `embed_file` consts + re-converges). Load-bearing.
- `cargo test --lib`; `cargo test --test lints`; `self_host_runtime` 0 regressed; `c_emit_comparison`/
  `lowerer_comparison` no regression; the Inc-2 `self_host_relocatable_embedded_runtime` test still passes.

## Commit (YOUR worktree branch only)
`git add` ONLY: `tests/fixtures/self_host_lowerer/driver.gg`, `tests/fixtures/self_host_lowerer/loader.gg`,
`tests/integration.rs`, `tests/lints.rs` (+ any new fixture). Commit message:
```
feat(gg-impl): embed lib/std/*.gg -> relocatable gg-selfhost with imports (Inc-3)

Embed the 28 lib/std/*.gg modules into driver.gg via embed_file; load_imports
consults the embedded dict (keyed on normalized module path) with the Inc-2
3-state precedence (--lib-dir flag > GG_LIB_DIR env > embedded) + local-module
shadowing + disk-miss-fallback. A std-importing program now builds+runs from
any cwd with no env (was a SILENT miscompile -> 0). Transitive imports resolve
via the existing worklist. lib/xtd + SQLite stay disk-only (later increment).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpfZ7JHtxtsgbL3AwzoAc6
```
REPORT BACK: worktree path + branch + commit hash; `git show --stat`; the `load_imports` precedence hunk
+ the `build_embedded_lib` shape; prove-it-bites (baseline silently 0 / patched correct); the
local-shadowing + escape-hatch results; byte-identity; bootstrap timing; all gate results. If a gate
fails or the precedence has an edge case (esp. local-module shadowing), STOP and report.

## Downstream (NOT this brief)
After Inc-3, `scripts/gg_impl.sh` can drop the `GG_LIB_DIR` (+ `GG_RUNTIME_DIR`) exports for a truly
relocatable install. Inc-3b = `lib/xtd` (876 KB; most pull SQLite/SDL/GL → coordinate with Inc-4 SQLite).
