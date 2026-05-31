# SPEC — Shared runtime + self-host full-program emission + runtime-parity harness

**Status:** spec v2 (review #1 of v1 folded + owner architecture decisions 2026-05-31). For ≥3 fresh reviews →
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
- The runtime C is ~40 string constants in `src/backend/c/c_runtime.rs` (617 KB): `RUNTIME_PREAMBLE`,
  `RUNTIME_STRING`, `RUNTIME_ARRAY`, `RUNTIME_ARENA_ALLOC`, … (one per family).
- Rust's `emit_runtime_modules` (`src/backend/c_lir/emit_types.rs:1791`) `push_str`s each CONDITIONALLY:
  `if has(|n| n.starts_with("gorget_<family>_")) { out.push_str(RUNTIME_<FAMILY>) }` — a program carries only
  the families it calls.
- **Four consumers of the SAME runtime** (this is the key architecture fact, found 2026-05-31):
  - **C backend** — INLINES it (`push_str` into the `.c`). (`emit_types.rs`)
  - **LLVM backend** — LINKS it: emits LLVM IR for user code, declares runtime fns `extern`
    (`llvm/mod.rs:1387`), bodies "live in the linked C runtime" (`:1675`, `:153`). Needs the runtime as a
    compiled `.o`.
  - **Future WASM backend** — planned (`llvm/mod.rs:6047`, `c_lir/mod.rs:2674`); will compile the runtime too.
  - **Self-host** (after this spec) — INLINES it, like the C backend.
- Other Rust consumers of `c_runtime::RUNTIME_*`: `src/backend/mod.rs`, `src/main.rs` (build orchestration —
  these handle the LLVM compile-and-link path; the extraction must keep them working).

## CHAIN 1 — Extract the runtime to shared `.c`/`.h` files (the foundation)

**Goal:** move the runtime out of `c_runtime.rs` Rust string constants into standalone `.c`/`.h` files (e.g.
`src/backend/c/runtime/<family>.c` + a manifest mapping family → file + the `gorget_<family>_` trigger
prefixes). ONE source of truth, consumable BOTH ways:
- **Inline** (C backend, self-host): read the file's text + concatenate (today's `push_str` becomes
  `push_str(read_file(...))` or `include_str!`).
- **Compile-and-link** (LLVM, future WASM): compile the file(s) to `.o`, link.

**Approach:**
- For each `RUNTIME_<FAMILY>` constant, move its body to `runtime/<family>.c` (or `.h` for decls). Keep the
  exact text (byte-identical) so emitted C is unchanged. A small manifest (the family → file + trigger-prefix
  list) preserves `emit_runtime_modules`' conditional logic — the SAME selection, just sourced from files.
- `include_str!` is the lightest in-Rust mechanism (compile-time embed, no runtime FS dependency, keeps the
  binary self-contained). PREFER it for the Rust side unless the LLVM/WASM link path needs real files on disk
  (it likely does — a `.o` is compiled from a real `.c`; so the files must exist on disk anyway, and Rust can
  `include_str!` them for the inline path while the link path `cc`s them). Decide + document.
- The self-host (Chain 2) reads the SAME files at emit time (from a known path, or a vendored-but-symlinked
  copy under the self-host fixture dir — see Chain 2's "how does the self-host get the files" risk).

**⚠ OUTPUT-NEUTRAL GATE (across ALL backends — the make-or-break for Chain 1):** the extraction must change
NO emitted output:
- C backend: `gg build --emit-c-lir` for a broad fixture set → byte-identical to pre-extraction.
- C backend runtime: a built binary for representative fixtures (collections/string/arena/concurrency/etc.)
  → identical behavior; the full `cargo test --test integration` sweep stays 1172/0.
- **LLVM backend:** `GG_BACKEND=llvm` build + run for a representative set → identical behavior (the linked
  runtime `.o` must be byte-identical). This is the easy-to-forget gate — the LLVM link path consumes the
  same constants (`backend/mod.rs`/`main.rs`); confirm the extracted files compile to the same `.o`.
- `cargo test --lib` green.

**Risk:** the byte-exactness of the extraction (whitespace/ordering). Mitigate by extracting mechanically
(script the constant→file split) + diffing the reassembled preamble against the original `RUNTIME_*`
concatenation before changing any emission code.

## CHAIN 2 — Self-host emits a FULL PROGRAM (the parity win + the harness enabler)

**Goal:** the self-host driver emits a complete, compilable `.c` (runtime preamble + body), so
`driver F lib --emit-c | cc - && ./a.out` works with no external preamble.

**Approach (mirror Rust's `emit_runtime_modules`):**
- Add a self-host port of `emit_runtime_modules`: scan the LIR module's call names, select the runtime
  families used (the SAME `gorget_<family>_` trigger prefixes as the manifest from Chain 1), read those
  shared `.c`/`.h` files, and emit their text BEFORE the body. This must reuse the Chain-1 files (NOT a
  vendored copy of the runtime text — that would re-introduce drift, the exact thing the owner rejected).
- `generate_c` (`lir_codegen.gg:5287`) gains a runtime-preamble prologue; `lir_codegen.gg:286`'s "skip if the
  runtime preamble already defines it" assumption now holds because the self-host EMITS that preamble.
- **The conditional-selection logic must MATCH Rust's exactly** (same trigger prefixes, same order, same
  modules) so the self-host's emitted preamble == Rust's for the same program — otherwise the runtimes drift.
  The manifest from Chain 1 is the shared selection spec both implement.

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
regenerate). NOTE (review #1): **115 `*.expected`/`*.gg.expected` files already exist in `tests/fixtures/` but
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
  must flip to WRONG-OUTPUT); the WRONG-OUTPUT/CC-FAIL backlog logged to TODO by feature family.

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
