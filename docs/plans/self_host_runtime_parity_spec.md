# SPEC — Shared runtime + self-host full-program emission + runtime-parity harness

**Status:** spec v4 (reviews #1+#2 folded + owner architecture decisions 2026-05-31). For ≥3 fresh reviews →
build as 3 staged chains.
**Owner decisions (2026-05-31):**
- Runtime parity (does the self-host binary produce Rust's output?) is the PRIMARY north-star; fn-count
  `c_emit_comparison` is demoted to a SECONDARY structural diagnostic (it overstates correctness — a sample
  found ~10-25% runtime-correct vs 83% fn-count "match").
- **Make the self-host emit a FULL PROGRAM** (runtime preamble + body), not a body-only `--lir-c`.
- **The runtime is ONE source of truth, extracted to shared `.c`/`.h` files**, consumed by all backends + the
  self-host.
- **Sequence: full-program first, then the harness** (the harness becomes trivial once the self-host emits a
  complete program; skip the interim splice harness).

## 0. Why (the blind spot + the architecture)

Every existing self-host validation measures STRUCTURE not BEHAVIOR: `c_emit_comparison` (849/1029 "matched")
compares emitted-C FUNCTION COUNTS — a body-level miscompile (right count, wrong code) is invisible. The
`~`-operator gap (`lower.gg` EUnaryOp silently dropped `~`, fixed `1289a7d7`) sat in this blind spot until a
2nd-order drop_elab double-free surfaced it. A runtime diagnostic (compile the self-host's emitted C + run,
diff vs Rust) over ~116 fixtures found only ~10-25% produce CORRECT output, ~30% RUN-BUT-WRONG. The `~` bug
was REPRESENTATIVE of a large invisible class. **fn-counting can't see a wrong body; runtime parity can.**

The clean way to validate runtime parity needs the self-host to emit a COMPLETE program — which it cannot
today: `driver.gg:156-161` only `print(generate_c(...))`, and `generate_c`/`lir_codegen.gg:286` emit no
runtime ("skip if the runtime preamble already defines it"). So this spec ALSO makes the self-host a complete
compiler.

### The runtime today (where it lives, who consumes it)
- The runtime C is ~65 C-text constants (across ~40 families) in `src/backend/c/c_runtime.rs` (617 KB):
  `RUNTIME_PREAMBLE`, `RUNTIME_STRING`, `RUNTIME_ARRAY`, `RUNTIME_ARENA_ALLOC`, plus the `*_RUNTIME`-suffixed
  ones (`ASYNC_RUNTIME`, `CHANNEL_RUNTIME`, …) + `PANIC_*`/`TASK_COMMON`.
- Rust's `emit_runtime_modules` (`src/backend/c_lir/emit_types.rs:1791`) `push_str`s each CONDITIONALLY:
  `if has(|n| n.starts_with("gorget_<family>_")) { out.push_str(RUNTIME_<FAMILY>) }` — a program carries only
  the families it calls.
- **FOUR existing Rust consumers of the SAME runtime constants** (the key architecture fact; consumer set
  CORRECTED per review #1 — grep `c_runtime::RUNTIME` to confirm exactly these):
  - **C backend — INLINES it** conditionally: `emit_runtime_modules` (`emit_types.rs:1791`) `push_str`s each
    `RUNTIME_*` if `all_call_names` has a `gorget_<family>_` match. This is the path the self-host mirrors.
  - **LLVM backend — LINKS it**: `main.rs:compile_llvm_pipeline` (`~:1088`) writes the selected `RUNTIME_*` to
    a REAL on-disk `.c` (`__gorget_runtime_<stem>.c`, `~:1108`) and `cc`s it to `.o`. ⚠ Its selection logic
    DIVERGES from the C backend's — it keys off `concat_source.contains("std.async")` + `lir_module.externs`
    scans (`main.rs:~1142-1226`), NOT the call-name family prefixes. So there is NO single shared "selection"
    today.
  - **Hot-reload host/guest split** — `backend/mod.rs:generate_hot_reload_split` (`~:478-498`) `push_str`s an
    UNCONDITIONAL hardcoded ~20-constant list. A real 4th consumer (review #1 caught the spec mislabeling it).
  - **Future WASM backend** — aspirational (`llvm/mod.rs:6047`, `c_lir/mod.rs:2674` comments). NOT a current
    consumer; must never become a load-bearing Chain-1 constraint.
  - **Self-host** becomes the 5th consumer (Chain 2) — INLINES like the C backend, mirroring
    `emit_runtime_modules` specifically.
- **KEY CONSEQUENCE (review #1):** because the four existing consumers each hardcode their OWN selection, Chain
  1 must NOT try to unify/share selection — it is a PURE TEXT MOVE (const → `include_str!`), leaving every
  selection site byte-for-byte unchanged. Output-neutrality then holds BY CONSTRUCTION. All selection work
  (incl. the family→trigger manifest) belongs to Chain 2.

## CHAIN 1 — Extract the runtime to shared `.c`/`.h` files (PURE TEXT MOVE — selection UNTOUCHED)

**Goal:** move the **~65 runtime C-text constant BODIES** out of `c_runtime.rs` into standalone files
(`src/backend/c/runtime/<family>.c|.h`), each constant becoming `pub const X: &str =
include_str!("runtime/<family>.c")`. ⚠ **The constants use TWO naming conventions — extract BOTH** (review #2):
only **27** are `RUNTIME_*`-PREFIXED; ~38 more use the `*_RUNTIME` SUFFIX (`ASYNC_RUNTIME`, `CHANNEL_RUNTIME`,
`THREAD_RUNTIME`, `MUTEX_RUNTIME`, `SCHEDULER_*_RUNTIME`, `METAL_RUNTIME`, …) plus `PANIC_NORMAL`/`PANIC_TEST`,
`TASK_COMMON`. ALL ~65 are consumed by `emit_runtime_modules`/`main.rs` and are equally in scope — the
`*_RUNTIME`-suffixed ones are the concurrency/scheduler/trace families Chain 2 most needs, so DON'T extract
only the 27 prefixed. ONE on-disk source of truth, consumable BOTH ways (inline: `push_str` of the same `&str`;
compile-and-link: the LLVM/WASM path `cc`s the real `.c` on disk). `include_str!` is the proven idiom here
(`c_runtime.rs:14896/14898/14903` already do it for `stb_image.h`/`sqlite3.c`).

**⚠ DO NOT TOUCH ANY SELECTION LOGIC IN CHAIN 1.** The four consumers each hardcode their OWN selection (C:
`emit_runtime_modules`' `if has(...)` chain; LLVM: `main.rs`' `source.contains("std.async")`; hot-reload:
`backend/mod.rs`' fixed list). Chain 1 changes ONLY the constant DEFINITIONS (string-literal → `include_str!`),
NOT a single `push_str` call or selection condition. Then every consumer keeps emitting byte-identical output
BY CONSTRUCTION — output-neutrality is automatic. (The family→trigger-prefix MANIFEST the self-host needs is a
CHAIN 2 artifact; do NOT build it here.)

**Approach (mechanical):**
1. For each runtime C-text `pub const X: &str = r#"..."#` (both `RUNTIME_*` and `*_RUNTIME`/`PANIC_*`/
   `TASK_COMMON`), write the body verbatim to `runtime/<family>.c` (or `.h`) and replace the const RHS with
   `include_str!("runtime/<family>.c")`. Review #2 confirmed: all are single-hash `r#"..."#` raw literals (no
   `r##` multi-hash escaping trap), 3 are already `include_str!`, 1 is the trivial empty `EXECUTOR_RUNTIME = ""`
   — there are ZERO `concat!`/`format!`-constructed runtime constants, so every one is 1:1 extractable (the
   pure-text-move premise holds). Still: inventory the full list first + handle the empty/already-`include_str!`
   ones as no-ops.
2. VERIFY byte-exactness BEFORE touching emission: a unit test asserts each `RUNTIME_<FAMILY>` value is
   byte-identical pre/post extraction (the `include_str!` content == the old literal). Zero diff required.

**⚠ OUTPUT-NEUTRAL GATE — must cover ALL FOUR consumers:**
- C inline: `gg build --emit-c-lir` over a broad fixture set → byte-identical to pre-extraction.
- C runtime behavior: full `cargo test --test integration` sweep stays 1172/0.
- LLVM link: `GG_BACKEND=llvm` build+run a representative set → identical behavior (the `__gorget_runtime_*.c`
  the link path writes must be byte-identical — `main.rs:~1108`).
- Hot-reload: exercise `generate_hot_reload_split` (`backend/mod.rs:~478`) if any test covers it; else assert
  its emitted text is unchanged.
- `cargo test --lib` green.

**Risk:** byte-exactness (whitespace/escaping/concatenated constants). Mitigated by the per-constant
byte-identity unit test (step 2) — land that FIRST, then the gate is mechanical.

## CHAIN 2 — Self-host emits a FULL PROGRAM (the parity win + the harness enabler)

**Goal:** the self-host driver emits a complete, compilable `.c` (runtime preamble + body), so
`driver F lib --emit-c | cc - && ./a.out` works with no external preamble.

**Approach — a FAITHFUL PORT of `emit_runtime_modules` (NOT a table lookup; review #1):**
- The self-host port must reproduce the C backend's `emit_runtime_modules` (`emit_types.rs:1791`, ~500 lines)
  — this is the real work of Chain 2. It is NOT a flat family→prefix manifest: it has dependency chains
  (`ensure_array!`/`ensure_map!`, MAP-depends-on-ARRAY, SET-depends-on-MAP), `module.*`-flag triggers
  (`test_fns`/`bench_fns`/`is_test_module`, `clone_stats`, `scheduler_mode`, `hot_reload`, `spawned_fns`/
  `thread_spawned_fns`, `trace_filename`, `target`), recursive-drop-table scans
  (`recursive_drop_structs`/`enums`), `elem_drop_fn` struct scans, strict ORDERING constraints (e.g. scheduler
  before task-group), and interleaved `#define`/`#pragma` emission (SQLite/STB/SDL, `emit_types.rs:2221-2292`).
  Build the family→trigger manifest HERE by reading that function; the self-host scans its LIR module's call
  names (its analog of `all_call_names`) + module flags, selects, reads the shared Chain-1 `.c`/`.h` files,
  and emits their text before the body.
- **⚠ Chain-2 sub-task (review #2): two enumerated triggers are ABSENT from the self-host `LirModule`** — it
  has `test_fns`/`bench_fns`/`is_test_module`/`scheduler_mode`/`trace_filename`/`hot_reload`/`spawned_fns`/
  `thread_spawned_fns`/`recursive_drop_structs`/`enums`/`drop_collision_types`/`type_drop_fns`
  (`self_host_lowerer/lir.gg:371-436`) but NO `clone_stats` and NO `target`/`freestanding` field. Both are
  safely DEFAULTABLE for the self-host's purposes: `clone_stats=false` (skip `RUNTIME_CLONE_STATS`), hosted
  `target` (skip the freestanding early-return). So the port can default them rather than add fields — but
  name this explicitly; do NOT assume all of `emit_runtime_modules`' flags exist on the self-host module.
- **MUST reuse the Chain-1 on-disk files** (NOT a vendored copy of the runtime text — that re-introduces the
  drift the owner rejected). The self-host already reads files (`driver.gg:20 from std.fs import read_file`,
  `lib/std/fs.gg:6`); pass the runtime dir like the existing `lib_dir` arg (`driver.gg:32`), or a known repo-
  relative path. Document the choice.
- `generate_c` (`lir_codegen.gg:5287`) gains a runtime-preamble prologue; `lir_codegen.gg:286`'s "skip if the
  runtime preamble already defines it" (`is_runtime_defined_named`) assumption now holds because the self-host
  EMITS that preamble. ⚠ **Dedup interaction (review #1):** the body's type-emission already skips types the
  preamble defines via `is_runtime_defined_named`; confirm the NEW self-emitted preamble + that dedup don't
  produce a DUPLICATE typedef (a type the preamble defines that the body re-emits, or vice-versa). This is the
  concrete correctness risk of Chain 2.
- **The correctness bar = the C BACKEND's preamble** (review #1 R3 — Rust has TWO preambles: the C-backend
  `emit_runtime_modules` and the LLVM `compile_llvm_pipeline`, which DIFFER). The self-host inlines like the C
  backend, so the gate is: **self-host preamble for program P == `emit_runtime_modules` output for P** (NOT the
  LLVM one). Diff against the C backend specifically.

**⚠ KEY RISK — how does the self-host get the shared runtime files?** The self-host runs as a compiled binary
in `tests/fixtures/self_host_lowerer/`. To read the shared `runtime/*.c`, it needs a path. Options the builder
must choose + document: (a) read from `src/backend/c/runtime/` via a relative/known path (couples the
self-host to the repo layout — acceptable for the in-repo self-host); (b) symlink the runtime dir under the
self-host fixture dir (like the other symlinked self-host sources); (c) pass the runtime dir as a driver arg
the test supplies. Whatever is chosen, it must be the SAME files Chain 1 created (no copy/drift).

**Gates:**
- `driver F lib --emit-c` for representative fixtures → a complete `.c` that `cc`s + runs == `gg run F`
  (FIRST for fixtures whose runtime families the self-host now emits — collections/string/etc., THEN the
  previously-splice-blocked families like concurrency).
- The emitted preamble for a program == Rust's emitted preamble for the same program (the selection matches).
  Diff them.
- `self_host_bootstrap` + `fixed_point` still GREEN (the self-host's OWN compilation now emits a preamble too;
  confirm the bootstrap still reproduces — it may need updating since it currently SPLICES Rust's preamble; if
  the self-host emits its own, the bootstrap simplifies — update it).
- `c_emit_comparison` / `lowerer_comparison` unchanged at the BODY level (the new preamble is additive; the
  comparison counts user fns in the body — confirm the preamble doesn't perturb the count).

## CHAIN 3 — The runtime-parity validation harness (now splice-free)

**Goal:** the test the owner asked for — run every eligible fixture through the self-host, confirm runtime
output == Rust gg.

**Mechanism (trivial once Chain 2 lands):** for fixture F: `driver F lib --emit-c` → complete `.c` → `cc` →
binary → run → stdout. Oracle = Rust gg's output (`gg run F`). Compare. NO preamble splice, NO splice risk.

**Two mechanisms (per owner steer):**
- **Diagnostic** (`self_host_runtime_diff`, env-gated/on-demand): report MATCH / WRONG-OUTPUT / CC-FAIL /
  EXCLUDED counts + per-fixture lists. Diagnostic-always-pass. The honest runtime-parity number.
- **Lock-in net** (`self_host_runtime`, build-breaking): assert the PASSING SET still matches; a regression
  FAILS the build. The set only grows.

**Categorization:**
- MATCH — self-host binary stdout == Rust. (parity ✓)
- WRONG-OUTPUT — runs, stdout != Rust → REAL silent miscompile (the `~` class; highest-value gap).
- CC-FAIL — self-host's complete `.c` won't compile → REAL gap (now unambiguous: the self-host emitted the
  whole program incl. its own preamble, so a cc-fail is genuinely the self-host's output, not a harness
  splice artifact). [This is WHY Chain 2 comes first — it makes CC-FAIL trustworthy.]
- EMIT-FAIL / TIMEOUT — self-host crashes emitting / hangs → real gap.
- EXCLUDED — Rust gg rejects (error/`*_error.gg` fixtures, 79 of them); non-deterministic
  (random/time/network: httpserver, p2p, random, async-sleep, socket); stress/bench; platform (metal/gl). The
  diagnostic auto-excludes via RUST-FAIL + a documented exclusion list. NOTE (review #1): the concurrency
  family (channel/shared/mutex/thread/task-group/async) is largely DETERMINISTIC and has `.expected` files —
  INCLUDE it (Chain 2 makes it emittable); only exclude the genuinely non-deterministic ones.

**Oracle & snapshot (cost):** Running `gg run` per fixture every build is expensive. The lock-in net asserts
against a committed SNAPSHOT of expected outputs (Rust-gg-generated, `GG_REGEN_RUNTIME_SNAPSHOT=1` to
regenerate). NOTE (review #1): **80 `*.expected` files already exist (35 of them `*.gg.expected`) in `tests/fixtures/` but
are UNWIRED** (0 refs in `integration.rs`) — reuse them to SEED the snapshot, but do NOT assume they're
validated by the current suite; regenerate/verify against `gg run`.

**Compare OUTPUT, never C text** (review #1): the self-host and Rust emit non-identical C for the same program
(e.g. self-host emits bare `sqrt`/`pow`, Rust emits `gorget_sqrt` wrappers — both link via `-lm`, both
correct). Parity is RUNTIME OUTPUT equality, not C-text equality. Never let this be "optimized" into a C diff.

**Cost:** parallelize via `parallel_map_fixtures` (`integration.rs:211`); gate the full diagnostic behind
`GG_RUNTIME_DIFF=1`; the lock-in net runs only the passing set + a generous timeout (`GG_*_TIMEOUT_SECS`).

## Acceptance (per chain)
- **Chain 1:** all backends output-neutral — C `--emit-c-lir` byte-identical, LLVM build+run identical, full
  integration sweep 1172/0, lib green. The reassembled-from-files preamble == the original `RUNTIME_*`
  concatenation (byte-diff = 0).
- **Chain 2:** `driver F lib --emit-c` → standalone `.c` compiles+runs == `gg run F` for a broad set incl. the
  previously-splice-blocked families; self-host preamble == Rust preamble per program; `fixed_point` green.
- **Chain 3:** trustworthy runtime-parity number; lock-in net green on the passing set + PROVABLY fails on a
  regression (revert the `~` fix — the `case "~":` in `lower.gg:4326`, landed in `1289a7d7` — `bitwise_ops`
  must flip to WRONG-OUTPUT — the `~` fix CODE is the `case "~":` at `lower.gg:4326`, which landed bundled in
  the drop_elab commit `1289a7d7` and is recorded in DONE.md under `326b124d`); the WRONG-OUTPUT/CC-FAIL
  backlog logged to TODO by feature family.

## Open questions for the reviewers
- Chain 1: `include_str!` (self-contained binary) vs on-disk files (the LLVM `.o` path needs real files
  anyway) — which, and does the LLVM/WASM link path force on-disk files (then Rust inline = `include_str!` of
  the same on-disk file)?
- Chain 2: the self-host's access to the shared runtime files (path/symlink/arg) — least-coupled choice?
- Chain 2: does emitting its own preamble simplify or complicate `self_host_bootstrap` (which currently
  splices Rust's preamble)? Update it accordingly.
- Is the conditional-selection manifest (family → trigger-prefixes) faithfully extractable from
  `emit_runtime_modules`' current `if has(...)` chain, and complete (all ~40 modules)?
- Exclusion list completeness (non-deterministic fixtures that would flake).
