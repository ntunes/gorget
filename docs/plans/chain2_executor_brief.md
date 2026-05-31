# Chain 2 Executor Brief — Self-host emits a FULL PROGRAM

**Status:** DRAFT — under ≥3 fresh-review discipline before launch.
**Source of truth:** `docs/plans/self_host_runtime_parity_spec.md` (CHAIN 2 section) + the recon below.
**North star:** runtime parity. Chain 2 makes the self-host emit a complete, compilable `.c`
(runtime preamble + body), unlocking Chain 3 (the splice-free harness).

---

## 0. Worktree discipline (NON-NEGOTIABLE)

Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree.
NEVER touch `/workspace/gorget-1` directly — every file operation, `cargo`, and `git` command runs in
your worktree path. Do NOT `cd` into `/workspace/gorget-1`. Do NOT use absolute paths starting with
`/workspace/gorget-1/...`. If your `pwd` reports `/workspace/gorget-1`, STOP and report it back.

Stage explicitly by file name with `git add <specific files>` — NEVER `git add -a`/`git add .`/`git commit -a`.
The files you will touch (and ONLY these):
- `tests/fixtures/self_host_lowerer/lir_codegen.gg` (add `emit_runtime_preamble` + `emit_lir_helpers` port)
- `tests/fixtures/self_host_lowerer/driver.gg` (split `--emit-c` from `--lir-c`; add runtime-dir handling)
- `tests/integration.rs` (add ONE new gate test `self_host_full_program`)
- (optionally) a curated fixture list — but prefer reusing existing `tests/fixtures/*.gg`

Do NOT run the full `cargo test --test integration` sweep (that's the parent's job). Run: `cargo build`,
`cargo test --lib`, your new gate test, and the two count gates (`c_emit_comparison`, `lowerer_comparison`)
+ `self_host_bootstrap_fixed_point`. ⚠ FORCE-REBUILD THE DRIVER before any comparison/bootstrap run:
`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c` then let the test
rebuild — the OnceLock-cached driver can be stale and segfault, falsely reporting 0-matched/all-crash.

---

## 1. What Chain 2 is (and is NOT)

**Goal:** the self-host driver, invoked `driver F lib --emit-c`, emits a COMPLETE `.c` = runtime preamble
(the conditionally-selected runtime `.c` files + the inline LIR helpers) followed by the existing body, so
`driver F lib --emit-c | cc - && ./a.out` runs with NO external preamble splice.

**You are porting Rust's `emit_runtime_modules` (`src/backend/c_lir/emit_types.rs:1791-2303`) AND
`emit_lir_helpers` (`emit_types.rs:2308-2401`) into the self-host.** READ THOSE TWO FUNCTIONS AS THE
AUTHORITATIVE SOURCE — the manifest in §4 is your checklist/map, not a substitute for reading the source.

**NOT in scope:** do NOT modify `generate_c` (it stays byte-identical = body-only). Do NOT touch Rust
(`src/...`). Do NOT modify the `self_host_bootstrap` / `fixed_point` / `c_emit_comparison` /
`lowerer_comparison` / `self_host_e2e` tests — they keep using `--lir-c` (body-only) and MUST stay
byte-identical/green. Do NOT extract `emit_lir_helpers` into a shared file (that would touch Rust). Do NOT
"stop splicing" in the bootstrap — that is a deferred follow-up, NOT Chain 2.

---

## 2. The design (locked — see recon §C and the driver audit)

`--emit-c` and `--lir-c` are currently ALIASES in `driver.gg` (both set `emit_c=true`, both emit body-only
via `generate_c(&lir)` at `driver.gg:156`). **Split them:**

- **`--lir-c` → body-only, UNCHANGED.** All 9 existing test invocations use `--lir-c`; they MUST stay
  byte-identical. Keep the existing `String _c = generate_c(&lir); print(_c)` path for `--lir-c`.
- **`--emit-c` → NEW full-program mode.** `String _c = emit_runtime_preamble(&lir, runtime_dir) + generate_c(&lir)`.
  `--emit-c` is used by ZERO existing tests (verified: `grep -c '"--emit-c"' tests/integration.rs == 0`), so
  repurposing it is free.

So in `driver.gg`, replace the single `bool emit_c` with two booleans, e.g. `bool emit_c_full` (set by
`--emit-c`) and `bool emit_c_body` (set by `--lir-c`). ⚠ **(brief-review pass-2 R2) Update TWO places:**
(1) the LIR-pipeline gate currently spelled `if emit_c or emit_lir:` (`driver.gg:116`) — which references
the now-removed `emit_c` — must become `if emit_c_full or emit_c_body or emit_lir:` (else `--emit-c`/`--lir-c`
silently fall through to the default GIR-text `else` at `:155`); and (2) the final-emit branch
(`driver.gg:153-161`, the `if emit_lir: ... else: generate_c`) must split full-vs-body. The pipeline
(lower→ssa→drop_elab) is identical for both; only the final emit differs: full =
`emit_runtime_preamble(&lir, rdir) + generate_c(&lir)`, body = `generate_c(&lir)`.
`--emit-lir`/`--emit-gir` stay as-is.

**Implementation lives in `lir_codegen.gg`:** add a top-level `String emit_runtime_preamble(LirModule &lir,
String runtime_dir)` and a `String emit_lir_helpers(LirModule &lir)` (called at the END of
`emit_runtime_preamble`, mirroring Rust where `emit_runtime_modules` ends by calling `emit_lir_helpers`,
`emit_types.rs:2302`). Export both from the module; `driver.gg` already does
`from lir_codegen import generate_c` — add `emit_runtime_preamble`.

⚠ **BUILD THE STRING WITH `StrBuf`/`sb_push`, NOT `out = out + X` (brief-review pass-2 R1 — this is the
file's hard-won OOM lesson).** `lir_codegen.gg:28-30,5379-5382` document that `out = out + emit_X(...)` in a
loop is O(N²) and OOM-kills stage-1 (RSS 103MB→10GB). `generate_c` uses `StrBuf` everywhere (132 `sb_push`
sites). The API (lir_codegen.gg:40-55): `struct StrBuf` with a `.s: String` field; `StrBuf sb_new()` (empty);
`StrBuf(String)` ctor; `void sb_push(StrBuf &buf, String rhs)` appends in-place (O(1) amortized via the
`gorget_string_append_buf` extern). **`sb_push` is ALSO immune to B1** — it mutates a `StrBuf &buf` STRUCT
through a pointer (struct `&`-mutation works), NOT a `&String` whole-value reassignment (which miscompiles).
So `emit_runtime_preamble` is: `StrBuf buf = sb_new()` → `sb_push(&buf, read_runtime(rdir, "x.c"))` per
family → `sb_push(&buf, emit_lir_helpers(&lir))` at the end → `return buf.s`. (The emit-once flags
`emitted_array`/`emitted_map` are plain function-local `bool`s — read/written in-function, never through a
`&`-param — so they work fine.)

**Order is preamble THEN body** — Rust's splice boundary `\ntypedef struct __gg_` (integration.rs:13790)
proves the runtime preamble (incl. `emit_lir_helpers`) precedes the body's user typedefs. So
`emit_runtime_preamble(...) + generate_c(...)` is the correct, source-faithful order. Keep `generate_c`'s
leading `#if GORGET_PRELUDE_FALLBACKS` block at the top of the BODY (it sits after the preamble, exactly as
in the spliced version today).

---

## 3. The `runtime_dir` mechanism (recon §D)

The self-host reads files via `read_file` (`lib/std/fs.gg:6`: `extern blocking String read_file(cstr path)
= "gorget_read_file"`) — returns a **bare String**, and on open-failure returns **"" silently** (NOT a
panic). So a wrong path yields a silently-incomplete preamble. Therefore:

- Add `from std.fs import read_file` to `lir_codegen.gg` (confirm it isn't already imported).
- Resolve `runtime_dir`: the driver runs with cwd = repo root (recon §D: the bootstrap tests invoke the
  driver with no `.current_dir()`, absolute driver/lib paths, so cwd = `CARGO_MANIFEST_DIR`). So
  **default `runtime_dir = "src/backend/c/runtime"`** (repo-relative) works. ALSO accept an explicit override
  so the gate test can pass an absolute path and not depend on cwd: parse a `--runtime-dir=<path>` flag (or a
  4th positional) in `driver.gg`, defaulting to `"src/backend/c/runtime"`. Pass it into
  `emit_runtime_preamble`.
- Add a GUARD: a helper `String read_runtime(String rdir, String name)` that does
  `String s = read_file(rdir + "/" + name)` and, if `s.len() == 0`, **`print(..., file=stderr)` then
  `exit(1)`** with a clear message (`runtime file <name> empty or missing at <rdir>`). ⚠ Use `exit(1)`, NOT
  `panic` (brief-review N1: `panic` is never actually invoked anywhere in the self-host; the established
  fail-loud idiom is `print(..., file=stderr); exit(1)` — see `validate.gg:25,293`). Import `exit` from
  `std.os` and `stderr` from `std.io` into `lir_codegen.gg`. Every runtime `.c` is non-empty, so an empty
  read ALWAYS means a bad path — fail loud, don't emit a broken preamble. (`EXECUTOR_RUNTIME` is the ONE
  empty const — it has NO file; do not read a file for it, emit nothing, see §4.)

---

## 4. The family manifest — your checklist for porting `emit_runtime_modules`

Port the selection logic from `emit_types.rs:1791-2300` IN THE SAME TOP-TO-BOTTOM ORDER. Each Rust
`out.push_str(crate::backend::c::c_runtime::CONST)` becomes
`sb_push(&buf, read_runtime(rdir, "<file>.c"))` (NOT `out = out + ...` — see §2/§4c). The const→file map
(all 62 verified):

```
RUNTIME_PREAMBLE→runtime_preamble.c            RUNTIME_ARENA_ALLOC→runtime_arena_alloc.c
RUNTIME_TRACKING_ALLOC→runtime_tracking_alloc.c RUNTIME_POOL_ALLOC→runtime_pool_alloc.c
RUNTIME_TLSF_ALLOC→runtime_tlsf_alloc.c        RUNTIME_FIXEDBUF_ALLOC→runtime_fixedbuf_alloc.c
RUNTIME_FALLBACK_ALLOC→runtime_fallback_alloc.c RUNTIME_STRING→runtime_string.c
RUNTIME_STRING_EXTENDED→runtime_string_extended.c RUNTIME_STRING_BASE_OPS→runtime_string_base_ops.c
RUNTIME_ALLOC_REPORT→runtime_alloc_report.c    RUNTIME_CLONE_STATS→runtime_clone_stats.c
PANIC_NORMAL→panic_normal.c                    PANIC_TEST→panic_test.c
RUNTIME_CHECKED_ARITH→runtime_checked_arith.c  RUNTIME_ARRAY→runtime_array.c
RUNTIME_STRING_ARRAY→runtime_string_array.c    RUNTIME_MAP→runtime_map.c
RUNTIME_SET→runtime_set.c                      RUNTIME_ERROR→runtime_error.c
RUNTIME_FILE→runtime_file.c                    RUNTIME_PATH→runtime_path.c
RUNTIME_ARGS→runtime_args.c                    RUNTIME_PARSE→runtime_parse.c
RUNTIME_TOSTR→runtime_tostr.c                  RUNTIME_ENV→runtime_env.c
RUNTIME_IO→runtime_io.c                        RUNTIME_MATH→runtime_math.c
RUNTIME_SORT→runtime_sort.c                    SYNC_RUNTIME→sync_runtime.c
ASYNC_RUNTIME→async_runtime.c                  TASK_COMMON→task_common.c
SCHEDULER_POOL_RUNTIME→scheduler_pool_runtime.c SCHEDULER_THREAD_RUNTIME→scheduler_thread_runtime.c
SCHEDULER_INLINE_RUNTIME→scheduler_inline_runtime.c SCHEDULER_SINGLE_RUNTIME→scheduler_single_runtime.c
MAIN_WAKER_RUNTIME→main_waker_runtime.c        CHANNEL_RUNTIME→channel_runtime.c
SHARED_RUNTIME→shared_runtime.c                MUTEX_RUNTIME→mutex_runtime.c
REACTOR_RUNTIME→reactor_runtime.c              BLOCKING_POOL_RUNTIME→blocking_pool_runtime.c
TASK_GROUP_RUNTIME→task_group_runtime.c        BYTES_RUNTIME→bytes_runtime.c
CRYPTO_RUNTIME→crypto_runtime.c                SOCKET_RUNTIME→socket_runtime.c
SERVER_SOCKET_RUNTIME→server_socket_runtime.c  UDP_SOCKET_RUNTIME→udp_socket_runtime.c
TLS_SOCKET_RUNTIME→tls_socket_runtime.c        TLS_SERVER_RUNTIME→tls_server_runtime.c
PROCESS_RUNTIME→process_runtime.c              PROCESS_SPAWN_RUNTIME→process_spawn_runtime.c
THREAD_RUNTIME→thread_runtime.c                TRACE_RUNTIME→trace_runtime.c
SDL_RUNTIME→sdl_runtime.c                      BYTES_F32_RUNTIME→bytes_f32_runtime.c
GL_RUNTIME→gl_runtime.c                        IMAGE_RUNTIME→image_runtime.c
AUDIO_RUNTIME→audio_runtime.c                  COMPRESS_RUNTIME→compress_runtime.c
METAL_RUNTIME→metal_runtime.c                  HOT_RELOAD_RUNTIME→hot_reload_runtime.c
EXECUTOR_RUNTIME→(EMPTY const, NO file — emit nothing)
```
The 3 vendored consts that are NOT in `runtime/`: `STB_IMAGE_SOURCE`→`src/backend/c/stb_image.h`,
`SQLITE_AMALGAMATION`→`src/backend/c/sqlite3/sqlite3.c`, `SQLITE_GORGET_WRAPPERS`→`src/backend/c/sqlite3/gorget_sqlite.c`.
For these, read with the parent path (e.g. `read_file(rdir + "/../stb_image.h")` — confirm the relative
path resolves; or special-case the `src/backend/c` base). **NOTE: the SDL/GL/Metal/SQLite/audio/image
families need external libs the CI box may lack — emit their selection logic faithfully, but the §6 gate
WILL NOT test those fixtures (they're platform/lib/non-deterministic and excluded). Port the logic; don't
worry about cc-ing them here.**

### 4a. The `all_call_names` scan (recon §B; CORRECTED per brief-review B2)
Build `Vector[String] all_call_names` = extern names (`lir.externs[].name`, `LirExtern.name` at lir.gg:306)
+ function names (`lir.functions[].name`, lir.gg:246) + EVERY `ICallExtern` name inside
`lir.functions[].blocks[].insts[]`. The inst variant is
`ICallExtern(int dst, String name, Vector[int] args, String original_name, Vector[int] arg_abis)`
(lir.gg:175) — match `case ICallExtern(_, name, _, _, _):` and collect `name`. The idiom already exists at
`lir_codegen.gg:738-763` (`emit_box_allocators_from_lir`) and `:1137` — copy that walk.

⚠ **YOU MUST ALSO scan globals' `init_expr` (brief-review B2 — this is a REAL cc-fail otherwise).** The
self-host has NO `LirGlobalInit::Extern` variant (so the spec can't be mirrored verbatim), BUT its globals
carry `String init_expr` (lir.gg:299) holding a full C call expression like `"gorget_math_infinity()"` for
`GINIT_RUNTIME_CALL` globals. A fixture whose ONLY trigger for a runtime family is a static global —
**verified: `static_init_imported.gg` uses only `INFINITY`/`NAN` → lowers to
`global f64 @INFINITY = extern gorget_math_infinity()`, emitted from the global's `init_expr`, NOT as any
body `ICallExtern`** — would miss `RUNTIME_MATH` and fail to link. So **append every global's `init_expr`
string into `all_call_names`** (they're full call exprs, so the `cn_has_prefix` matcher — e.g. prefix
`"gorget_math_"` matches `"gorget_math_infinity()"` — works directly). ~3 lines; do not omit. (The earlier
"only stdout/stderr/stdin globals" rationale was FALSE.)

### 4b. The `has(...)` predicate
Rust uses `has(&|n| n.starts_with("x") || ...)`. Implement two helpers (do NOT depend on closures):
- `bool cn_has_prefix(Vector[String] names, String prefix)` — any name `.starts_with(prefix)`.
- `bool cn_has_eq(Vector[String] names, String exact)` — any name `== exact`.
Then each Rust `has(&|n| n.starts_with("a") || n.starts_with("b"))` becomes
`cn_has_prefix(names, "a") or cn_has_prefix(names, "b")`. Transcribe EVERY prefix/eq literal from the Rust
source exactly (the string-extended block at `emit_types.rs:1853-1871` alone has ~40 prefixes — copy them
all). Build `all_call_names` ONCE before the conditionals.

### 4c. The emit-once dependency macros (recon §E; CORRECTED per brief-review B1)
`ensure_array!`/`ensure_map!` are emit-once. ⚠ **DO NOT use `void f(String &out, bool &emitted)`
mutable-ref helpers — the self-host SILENTLY MISCOMPILES whole-value reassignment of a `&String`/`&bool`
param (brief-review B1 PROVED it: `void append_x(String &out, bool &flag): out = out + "X"; flag = true`
emits the write-back under Rust gg but the self-host body NEVER stores through the pointer — the caller's
value is unchanged).** This is a self-host codegen gap (logged to TODO); for Chain 2, sidestep it entirely.

**MANDATORY pattern: `sb_push` into the function's `StrBuf`, with plain-`bool` emit-once flags.**
`emit_runtime_preamble` declares `StrBuf buf = sb_new()`, `bool emitted_array = false`,
`bool emitted_map = false` and `return buf.s` at the end (see §2 — this is `generate_c`'s actual idiom and is
B1-immune). At each array-needing site, open-code:
```
if not emitted_array:
    sb_push(&buf, read_runtime(rdir, "runtime_array.c"))
    emitted_array = true
```
MAP (depends ARRAY) — open-code the array check FIRST, then the map check:
```
# ensure array, then map:
if not emitted_array:
    sb_push(&buf, read_runtime(rdir, "runtime_array.c"))
    emitted_array = true
if not emitted_map:
    sb_push(&buf, read_runtime(rdir, "runtime_map.c"))
    emitted_map = true
```
SET depends MAP: the two blocks above, then `sb_push(&buf, read_runtime(rdir, "runtime_set.c"))`. Many
families need ARRAY first (string_array, file, path, args, io, sort, socket, server_socket, process_spawn) —
open-code the array block at each. ⚠ **FORBIDDEN: a `void f(String &out, bool &emitted)` helper (miscompiles,
B1) AND `out = out + X` in any loop (OOM, R1).** `sb_push(&buf, ...)` is both safe and the file's idiom.

### 4d. The derived booleans + ordering (recon §E — preserve EXACTLY)
- `is_test_or_bench = lir.test_fns.len() > 0 or lir.bench_fns.len() > 0 or lir.is_test_module`
  → gates RUNTIME_ALLOC_REPORT, PANIC_NORMAL-vs-PANIC_TEST (test→PANIC_TEST else PANIC_NORMAL), RUNTIME_ERROR.
- `needs_sync` (emit_types.rs:2081), `needs_async` (2094), `needs_spawn` (2193) — transcribe the exact
  prefix sets. Channel (2116) and Mutex (2130) re-append ASYNC_RUNTIME **only if not needs_async** → compute
  `needs_async` FIRST. BLOCKING_POOL also fires on `lir.spawned_fns.len() > 0`. THREAD also on
  `lir.thread_spawned_fns.len() > 0`. PROCESS_SPAWN gated on `needs_spawn`; PROCESS gated on
  `needs_spawn or has(gorget_process_/gorget_exec_/==getenv/==setenv)`.
- The async block ORDER (2102-2113): ASYNC_RUNTIME → TASK_COMMON → (scheduler-mode switch) → MAIN_WAKER →
  EXECUTOR(empty). The self-host `scheduler_mode` is an **int** with `SCHED_POOL=0/THREAD=1/INLINE=2/SINGLE=3`
  (lir.gg:88-91) — `if lir.scheduler_mode == SCHED_POOL: ...scheduler_pool_runtime.c` etc. Ordinals align
  with Rust's enum.
- `trace_filename` is a **String** (lir.gg:384), NOT Option — gate TRACE on `lir.trace_filename != ""`
  `or cn_has_prefix(names, "gorget_trace_")` (Rust emit_types.rs:2216 uses `.is_some()`).

### 4e. The 3 absent-field CORRECTIONS (recon §A/§F)
- **`clone_stats` ABSENT** → default false → SKIP the `RUNTIME_CLONE_STATS` block entirely.
- **`target`/freestanding ABSENT** → default hosted → SKIP the freestanding early-return (emit_types.rs:1820);
  always emit the full hosted preamble.
- **`elem_drop_fn` ABSENT from `LirStructDef`** (lir.gg:270-276, recon §A) → the three collection triggers
  (array/map/set, emit_types.rs:1949/1967/1978) lose their `s.elem_drop_fn.as_deref()==Some(...)` disjunct.
  Keep the OTHER two disjuncts: `recursive_drop_fn_used("gorget_array_free")` (scan
  `lir.recursive_drop_structs` values' `drop_fn_name` + `lir.recursive_drop_enums` values' `drop_fn_name`,
  fields at lir.gg:353/360) OR `cn_has_prefix(names, "gorget_array_")`/`cn_has_prefix(names, "Vector__")`.
  Mirror Rust's `recursive_drop_fn_used` closure (emit_types.rs:1942-1948) as a helper
  `bool recursive_drop_fn_used(LirModule &lir, String drop_fn)`.

### 4f. The special `#define`/`#pragma` blocks (recon §E — transcribe the literal lines)
SDL (emit_types.rs:2221-2232), stb_image (2244-2261), SQLite (2278-2292) emit literal `#define`/`#pragma`
lines around the runtime text. Transcribe those literal strings EXACTLY from the Rust source via
`sb_push(&buf, "\n#define STB_IMAGE_IMPLEMENTATION\n")` etc. (one `sb_push` per literal, NOT `out = out +`).
These are gate-excluded (need libs) but port them faithfully for completeness.

---

## 5. Porting `emit_lir_helpers` (emit_types.rs:2308-2401) — THE under-scoped piece (recon §F)

⚠ **This is the top correctness risk, NOT the dedup.** The ~90 lines of `emit_lir_helpers` are
inline-GENERATED C text (`gorget_str_ord`, `*__default`, `*__one`, `__gorget_hash_int`, `gorget_str_hash`,
`gorget_generic_compare`/`gorget_int_compare`/`gorget_float_compare`/`gorget_str_compare`, `gorget_char_chr`,
`gorget_str_codepoint_at`, `gorget_utf8_codepoint_len_at`, `<signal.h>`, the `gorget_task_group_submit`
macro, `gorget_file_create`, `__gorget_file_open_r`, etc.). VERIFIED: these are in NEITHER any self-host
`.gg` source NOR any of the 62 runtime `.c` files (the self-host body CALLS them, e.g. `int64_t__default()`).
Today they come from the SPLICED Rust preamble; once the self-host emits its own preamble, it MUST emit
these or the standalone `.c` fails to link.

**Port:** add `String emit_lir_helpers(LirModule &lir)` that reproduces `emit_types.rs:2308-2401` as C text,
built with `StrBuf`/`sb_push` (NOT `out = out + X`) and `return buf.s` — same as §2/§4c.
- READ the Rust function. Most of it is UNCONDITIONAL `static inline` defs — emit them as string literals.
- A few are `has(...)`-gated (e.g. `gorget_char_chr`, `gorget_utf8_codepoint_len_at`,
  `gorget_str_codepoint_at`, the `gorget_task_group_submit` macro, `gorget_file_create`/`gorget_file_open`).
  Use the same `cn_has_*` helpers; transcribe the exact gate predicates from the Rust source.
- **Byte-faithfulness bar:** the new gate (§6) compares RUNTIME OUTPUT (stdout), not C text, and the
  fixed_point test does NOT use `--emit-c` (it splices), so this port need only be FUNCTIONALLY correct
  (compiles + correct behavior). But reproducing the Rust C text verbatim is the safest way to get there —
  copy it exactly, including helper bodies and the `signal.h` include.
- **Do NOT double-port `emit_runtime_helpers`** (emit_types.rs:2405) — its self-host analog
  `emit_box_allocators_from_lir` (lir_codegen.gg:738) is ALREADY emitted by `generate_c`. Only
  `emit_lir_helpers` is missing. (Confirm `generate_c` does not already emit any of the §5 helpers before
  adding them — grep the body emitters; recon says it does not, but verify to avoid a double-definition.)

---

## 6. The gate test (the deliverable that proves Chain 2)

Add ONE new test `fn self_host_full_program()` in `tests/integration.rs`, near `self_host_e2e`
(`integration.rs:14398`). MODEL IT on `self_host_e2e` / `self_host_bootstrap` for: the cached driver build
(reuse the existing `build_gg_dir_cached` / driver-build helper), the cc invocation (reuse the EXACT cc
flags + libs those tests use — `-lm -lpthread` etc.), and run+capture-stdout.

⚠ **Gating decision (brief-review N2): the new test MUST RUN BY DEFAULT on plain `cargo test --test
integration` — do NOT copy `self_host_e2e`'s `skip_unless_full()` / `GG_FULL=1` gate.** This test is THE
deliverable that proves Chain 2 and the regression net; a `GG_FULL`-only gate wouldn't catch regressions on
ordinary runs. To keep default-run cost bounded, the asserted set is SMALL (~6-10 fixtures spanning ≥6
runtime families) and reuses the already-cached driver build (one build, shared). If even that proves too
slow for default CI, the fallback is to keep it default-run but trim to ~6 fixtures — NOT to `GG_FULL`-gate
it. State which you chose in the report.

**Mechanism per fixture F:** `driver F <abs lib> --emit-c --runtime-dir=<abs src/backend/c/runtime>` →
complete `.c` → write to temp → `cc` → run → capture stdout. Oracle = `gg run F` stdout (or the fixture's
committed `.expected` if one exists and is verified). Assert equal. Difference from `self_host_e2e`: NO
preamble splice — the `.c` is self-complete.

**Fixture list — curated, deterministic, no platform libs, families the self-host now emits.** ~6-10.
Suggested (VERIFY each exists in `tests/fixtures/` and that `gg run` gives deterministic output — adjust
freely): a basic arithmetic/control-flow fixture (e.g. `hello`), a `Vector`/array fixture, a `Dict`/map
fixture (e.g. `dict_literal`), a `Set` fixture, a string-methods fixture (e.g. `string_methods`), a math
fixture (e.g. `math_constants`), a struct/enum fixture, a closures fixture, and the `static_init_imported`
fixture (it specifically exercises the §4a global-`init_expr` scan — a good regression anchor for B2).
⚠ **(brief-review N3) `catch_basic.gg` does NOT exercise `RUNTIME_ERROR`** — the self-host lowers throw/catch
to plain `Result` returns (its body has 0 `gorget_catch`/`gorget_throw`/`gorget_cleanup_` symbols). It's
fine to include (it'll MATCH if lowering is correct) but it does NOT validate the error family. To cover
`RUNTIME_ERROR`, include a TEST-MODE fixture (where `is_test_or_bench` triggers `RUNTIME_ERROR` +
`PANIC_TEST` + `RUNTIME_ALLOC_REPORT`) — but note test-mode fixtures emit the alloc-report atexit line, so
the oracle must be `gg run`/`gg test` of the SAME fixture (apples-to-apples), not a hand-written expected.
If covering test-mode is awkward, accept `RUNTIME_ERROR` as gate-untested and log it. EXCLUDE:
error/`*_error.gg` (Rust rejects), random/time/network/thread/async-sleep (non-deterministic),
SDL/GL/Metal/SQLite/audio/image (platform libs), stress/bench.

**Make it diagnostic-friendly:** print MATCH/WRONG-OUTPUT/CC-FAIL per fixture + a summary line, like the
`*_comparison` tests. The ASSERT should cover the curated passing set (a regression fails the build). If a
fixture in the list doesn't pass on first run, INVESTIGATE (it's a real self-host gap or a port bug) — if
it's a genuine pre-existing self-host miscompile unrelated to the preamble, move it OUT of the asserted set
with a `// TODO(chain3): <fixture> WRONG-OUTPUT — <reason>` and log it to the report; do NOT reshape the port
to hide it (CLAUDE.md "don't redesign around compiler gaps"). The asserted set must be non-empty and span
≥6 distinct runtime families.

---

## 7. Validation gates (ALL must hold before you report done)

1. `cargo build` clean.
2. `cargo test --lib` green (you touched no Rust lib code, so this is a sanity check).
3. **NEW** `self_host_full_program` passes (≥6 families, non-empty asserted set).
4. **Existing byte-identity:** ⚠ (brief-review pass-2 R3) do NOT hard-code counts — they drift and the
   `*_comparison` tests are diagnostic-always-pass. Instead: BEFORE your change, force-rebuild the driver and
   record the printed `c_emit_comparison` / `lowerer_comparison` matched-counts from `--nocapture` (expected
   ~849 / ~951 at the time of writing, but READ the actual baseline). AFTER your change, force-rebuild and
   confirm BOTH are UNCHANGED vs your recorded baseline (the preamble is emitted only under `--emit-c`, which
   these tests don't use; `generate_c` is untouched, so they MUST be identical).
5. `self_host_bootstrap_fixed_point` GREEN (it uses `--lir-c` body-only + splice — untouched by you; confirm
   it still reconverges).
6. Report: the per-fixture MATCH/WRONG-OUTPUT/CC-FAIL table, the asserted set, any fixtures you had to
   exclude + why, and confirmation that `--lir-c` output is byte-identical to before (diff one fixture's
   `--lir-c` output pre/post your change to prove `generate_c` is untouched).

## 8. Known gotchas
- `read_file` returns "" silently on missing path → the §3 `read_runtime` guard MUST fail loud.
- The 3 vendored files (stb/sqlite) live OUTSIDE `runtime/` — path them relative to `src/backend/c`.
- Preserve the EXACT top-to-bottom emission order of `emit_runtime_modules` — a misordered dependency
  (e.g. MUTEX before ASYNC when `!needs_async`) produces a `.c` that won't compile.
- The `writeln!(out).unwrap()` at emit_types.rs:2078 emits a blank line between the core sections and the
  sync/async sections — reproduce it (`sb_push(&buf, "\n")`).
- Build `all_call_names` ONCE; the predicate helpers scan it each call — for ~50 families × N names this is
  fine (it's a one-shot emit), do not micro-optimize.
