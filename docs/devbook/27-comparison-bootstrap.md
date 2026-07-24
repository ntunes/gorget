# 27 — Comparison, bootstrap & report generation

This chapter covers the test machinery that measures the self-host frontend against the Rust `gg` reference (the `*_comparison` tests), the self-recompilation loop that proves the self-host is a usable compiler (`self_host_bootstrap` and `self_host_bootstrap_fixed_point`), and the HTML report generator (`src/report.rs`). It also documents the auxiliary harnesses that share this file family: the backend A/B equivalence guard (`tests/lir_ab.rs`), the layering-discipline ratchets (`tests/lints.rs`), the adversarial memory-safety suite (`tests/security.rs`), and the standalone runtime test (`tests/str_runtime.rs`). Everything except `report.rs` lives in `tests/`; the comparison and bootstrap tests are all in `tests/integration.rs`.

The single most important fact in this chapter: **the `*_comparison` tests are diagnostic-always-pass.** A green `cargo test` says *nothing* about self-host parity. Only the matched-counts printed to stderr carry the signal, and you only see them with `--nocapture`. This is by design and is explained below.

## The parity north-star

Gorget's self-host frontend (`tests/fixtures/self_host_{lexer,parser,resolver,typechecker,check,lowerer}/`) is a Gorget reimplementation of the compiler's own passes. The finish line is not "the test suite is green" — it is **feature parity with the Rust `gg`**: the self-host compiling every fixture the same way the Rust reference does. Parity is measured by the comparison tests' matched-counts climbing toward 100%, not by any pass/fail gate. A green suite (including a green `self_host_bootstrap_fixed_point`) is a milestone, not the destination — see `MEMORY.md`'s "NORTH STAR" section and `CLAUDE.md` "Self-host as the elegance showcase."

This distinction drives the entire design of the comparison harness: it must run over the whole fixture corpus, report how close the two implementations are, and **never block CI on the gap** — because the gap is expected to be non-zero for a long time and is the thing being actively closed.

## Why the comparison tests are diagnostic-only

Each comparison test ends the same way:

```
// Diagnostic test — always passes. Mismatches guide development.
eprintln!("\n================================\n");
```

Every comparison test (and `self_host_e2e`) ends with a diagnostic-only comment and **no `assert!` on the matched count**. The exact wording above is the canonical form, used verbatim by `parser_comparison` (`tests/integration.rs:12569`), `resolver_comparison` (`:12866`), `type_comparison` (`:13174`), and `lowerer_comparison` (`:13532`); `lexer_comparison` (`:9526`) and `c_emit_comparison` (`:13545`) use equivalent multi-line / shorter docstring variants. The only `assert!`s in any of these bodies are setup invariants ("fixtures dir is non-empty", "driver built successfully") — never "N fixtures matched."

The reasons:

1. **The gap is the work, not a regression.** If a fixture the self-host doesn't yet handle correctly turned the suite red, every routine `cargo test` would fail, and the suite would be useless as a green/red signal for *everything else*. The parity gap is tracked as a *number that should go up*, not a boolean that must stay true.
2. **The signal is the printed count, not the exit code.** To read parity you run the test with `--nocapture` and read the `Fixtures compared / matched / mismatched / crashed` line on stderr. Without `--nocapture`, cargo swallows the `eprintln!` and you learn nothing.
3. **Re-confirm before quoting.** Because the test always passes, a dated parity figure in a doc or commit message is unverifiable from the test's exit status. The only authoritative source is a fresh run. `MEMORY.md` flags repeatedly that these numbers drift; treat any transcribed figure as stale.

The procedure to read current parity for any pass:

```bash
cargo test --test integration <name>_comparison -- --nocapture 2>&1 | tee /tmp/cmp-$RANDOM.log
# then read the "=== <Name> Comparison Results ===" block on stderr
```

where `<name>` is one of `lexer`, `parser`, `resolver`, `type`, `check`, `lowerer`, `c_emit`.

## How a comparison test works

Every comparison test follows one shape, built on two shared helpers:

- `build_gg_dir(dir, main)` (`tests/integration.rs:9143`) compiles a self-host driver directory (e.g. `self_host_parser/driver.gg`) with the Rust `gg`, returning `(exe_path, c_path)`. `build_gg_dir_cached` (`:9191`) wraps it in a `OnceLock` so the several tests that share `self_host_lowerer/driver.gg` pay the ~57 s build once per test process rather than once each. As of writing, eight test fns call `build_gg_dir_cached("self_host_lowerer", "driver.gg")` — `lowerer_comparison`, `c_emit_comparison`, the two bootstrap tests, `self_host_e2e`, and three self-host guard tests (`self_host_snag5_synth_name_no_type_misroute`, `phase_c_closed_classes_remain_at_zero_self_host`, `validate_passes_passes_self_host`); grep for the call to get the live count rather than trusting the docstring's "e.g." list.
- `parallel_map_fixtures(fixtures, f)` (`:211`) fans the per-fixture comparison across worker threads (`available_parallelism / 2`, clamped to `[2, 8]`) and preserves input order. The work is embarrassingly parallel — one subprocess per fixture, dominated by fork+exec — and the halved worker count leaves headroom under cargo's own `--test-threads`.

The per-fixture closure does the same thing in every test:

1. **Rust side**: run the relevant Rust pass *in-process* (the test binary links `gorget` as a library) and format its output canonically. E.g. `resolver_comparison` calls `resolve::collect_top_level` + `resolve::resolve_bodies` then `format_resolution_canonical` (`:12739`–`:12751`); `type_comparison` calls `gorget::semantic::analyze` then `format_types_canonical` (`:13060`–`:13061`).
2. **Self-host side**: run the compiled driver binary as a subprocess with the fixture path as `argv[1]`, capturing stdout (e.g. `:12462`).
3. **Compare** the two canonical strings and classify the fixture as `Matched`, `Mismatched`, or `Crashed` (driver exited non-zero).

The two sides only agree if both implementations emit **the same canonical textual form**. The Rust-side canonical formatter is the contract. The big ones:

- **Lexer**: `describe_token_canonical_rust` (`:9248`) renders each token to a one-line string (`kw:...`, `ident:...`, `int:...`, `str:...`/`fstr:`/`rstr:`/`mstr:`/`bstr:`/`cstr:` via `describe_string_canonical_rust` at `:9224`). The diff is token-by-token with float tolerance (`canonical_token_eq`, `:9319`, relative epsilon `1e-6` because C's `%g` rounds at 6 sig-figs) and comment/doc tokens filtered out on both sides (`is_comment_token`, `:9341`).
- **Parser**: `format_module_canonical` produces a line-per-node AST dump; the diff finds the first divergent line (`:12480`).
- **Resolver**: `format_resolution_canonical` (`:12608`) emits `DEF`, `SCOPE`, and `RES` lines. `normalize_resolver_output` (`:12659`) then strips the `start:end` span from `DEF` lines (the Gorget AST doesn't store name spans) and **drops `SCOPE` lines entirely** (Rust's `Expr::Block` creates extra scopes the Gorget AST lacks). `RES` lines — the actual name→DefId resolutions — are compared verbatim; they are the core correctness check.
- **Type / check**: `describe_type_canonical` (`:12874`) renders a `ResolvedType` to its surface spelling, with two deliberate normalizations to absorb representation differences: `str`→`String` (`:12885`) and `Future[T]`→`T` (`:12891`, async wrapping isn't tracked self-host-side). `format_types_canonical` (`:12966`) emits one `TYPE i "name" = <type>` line per typed def.

### Match, superset, mismatch

`type_comparison` and `check_comparison` use a **three-way** outcome instead of binary match/mismatch (`:13041`). They extract the set of `"name" = type` pairs from each side (`extract_pairs`, `:13081`) and classify:

- `Matched` — the sets are equal.
- `SupersetMatched` — the Rust set is a *subset* of the self-host set (`rust_set.is_subset(&gorget_set)`, `:13091`). The self-host produced *more* type info than Rust, which is acceptable: the self-host philosophy is "both checkers should produce MORE correct info, never suppress output to match gaps" (`MEMORY.md`). The reported "adjusted" parity is `exact + superset`.
- `Mismatched` — there is a pair only in Rust (a real gap). The report shows the first Rust-only and first Gorget-only pair.

`check_comparison` (`:13193`) is the sister of `type_comparison`: where `type_comparison` runs `analyze()` on a single parsed module, `check_comparison` runs the full `gg check` path on the Rust side — `ModuleLoader::load_all` → `merge_modules` → `analyze` (`rust_check_output`, `:13229`) — against `self_host_check/driver.gg`, which runs its own `loader.gg` + typecheck pipeline. It wraps the Rust pipeline in `catch_unwind` and classifies a loader failure as `RustSkipped` (`:13266`) so a Rust-side loader limitation doesn't get charged against the self-host.

#### Current mismatch baseline (regenerate — do not trust the number)

Per the always-pass discipline above, the durable artifact is the **command**, not a transcribed count. Regenerate the type/check baseline with:

```bash
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    -- type_comparison check_comparison --nocapture 2>&1 | tee /tmp/cmp-$RANDOM.log
# read the "=== Type/Check Comparison Results ===" blocks: total / exact / mismatched / crashed
```

At the 2026-07-17 D29 round close this printed **`type_comparison` 1585 total / 1517 exact / 68 mismatched / 0 crashed** and **`check_comparison` 1585 / 1516 / 69 / 0** — the current floor. Those mismatches are the known self-host diagnostic-line-count parity gap plus a handful of typed-inference deltas, not regressions. A round that *adds* fixtures moves the baseline, so re-measure and re-record it at every round close — the same "round closes that add fixtures MUST regen the number" lesson the runtime-parity metric learned. (The "85/86 mismatched" figure quoted in older, since-retired briefs is stale: it predated the CoW-1A remediation and D29 fixtures.)

### Lowerer and c_emit: count-based, not text-based

The two backend-facing comparisons can't diff canonical text — the emitted GIR/C is enormous and legitimately differs in formatting — so they compare **function counts** as a coarse parity proxy:

- `lowerer_comparison` (`:13390`) runs Rust `gg build --emit-gir` and the self-host driver, then counts lines starting with `"fn "` on each side (`:13438`, `:13456`). A fixture is `Matched` if the counts agree, `ErrorOnly` if Rust emitted 0 functions (Rust rejected it at semantic analysis — not the self-host's fault, `:13463`), else a `RealMismatch`. The reported "Adjusted" figure is `matched + error-only` over processable fixtures.
- `c_emit_comparison` (`:13549`) runs Rust `gg build --emit-c-lir` and the self-host driver's `--lir-c` mode, then counts user function-body openings (`user_fn_count`, `:13565`: lines after the `Function Definitions` section marker that start with an identifier and end with `) {`). Outcomes: `Matched`, `RustOnly` (self-host emitted nothing), `Mismatched`, `SelfHostCrash`, `RustCrash`.

Because these count *functions*, a "matched" verdict means equal `user_fn_count`, **not** byte-identical output — true byte-parity is strictly lower. `MEMORY.md` notes `c_emit` is the biggest remaining parity gap and the highest-leverage target.

## In the self-host

This chapter *is* the self-host test machinery — it's how the self-host frontend is measured, not a Rust pass with a self-host port. The drivers under test live in `tests/fixtures/self_host_*/`:

- Each comparison test has its own driver directory. The parser/lexer/AST source files are **independent copies** across most directories, *except* `self_host_lowerer`, whose `parser.gg` and `ast.gg` are symlinks into `self_host_typechecker` (`ls -la tests/fixtures/self_host_lowerer/` shows `parser.gg -> ../self_host_typechecker/parser.gg`, `ast.gg -> ../self_host_typechecker/ast.gg`). When you change a parser/AST primitive you must update every relevant copy — the symlinks cover the lowerer↔typechecker pair for free, the others don't.
- The Rust-side canonical formatters in `tests/integration.rs` are the **shared contract** both implementations target. If you change a self-host pass's output format you must update the corresponding `*_canonical_rust` / `format_*_canonical` function (or the comparison will report spurious mismatches), and vice versa.

There is no comparison coverage for the backends below LIR emission, package loader internals, or codegen byte-output (only the coarse `c_emit` count). The `parallel_map_fixtures` docstring (`:202`) lists "fmt" among the comparison tests, but **no `fmt_comparison` test exists** in the current tree — that's a stale aspiration in the comment, not a live test.

## The bootstrap fixed point

`self_host_bootstrap` (`tests/integration.rs:13724`) and `self_host_bootstrap_fixed_point` (`:13897`) prove the stronger property that the self-host is a *usable* compiler: it can compile itself, and the recompilation converges.

### Stage construction

Both tests share the same stage-0 → stage-1 construction (`self_host_bootstrap` is the simpler of the two; the fixed-point test repeats and iterates it):

1. **Stage 0** = the self-host driver compiled by the Rust `gg` (`build_gg_dir_cached("self_host_lowerer", "driver.gg")`). `build_gg_dir` leaves `driver.c` next to the binary; the test reuses it.
2. **Stage-1 body C**: run the stage-0 driver on *its own source* (`driver.gg`) with `--lir-c`. This emits the self-host's C translation of itself, with no runtime preamble (`:13739`). The run gets a 600 s deadline because the driver needs ~30 s–1 min on its 4K-line source and far more under parallel load (`:13754`).
3. **Runtime preamble extraction**: read the Rust-compiled `driver.c` and cut everything before the first user-type typedef — `rust_c.find("\ntypedef struct __gg_")` (`:13775`). The stage-1 body re-emits the user types, so the preamble cut at that boundary avoids duplicate struct definitions. The preamble is the *runtime library* portion (`gorget_*` functions, GorgetArray/GorgetMap/etc.); the self-host doesn't emit the runtime, only the user code.
4. **Stage-1 source** = `preamble + body`, compiled with `cc -O0 -w -lm -lpthread` into the stage-1 binary (`:13788`). `-w` suppresses warnings so only hard errors (type mismatches, undefined refs) fail the test.

`self_host_bootstrap` then asserts the stage-1 binary *runs* on `driver.gg` and emits a stage-2 body at least half the size of stage-0's (`:13859`) — a guard against silent mid-emission OOM-kills (the docstring records the `sb_push` O(N²) regression that ballooned stage-1 to 5+GB RSS at `:13822`). It deliberately does **not** check byte-equality — that's the fixed-point test's job.

### Convergence

`self_host_bootstrap_fixed_point` iterates stage-N → stage-(N+1) (compile stage-N's C, run it on `driver.gg` to produce stage-(N+1)'s C body) up to `MAX_GEN = 5` (`:13991`). It declares success when `stage(K).c == stage(K+1).c` byte-for-byte for some K (`:14046`). The semantics: each pass of the self-host pipeline is a fixed point at convergence — "whatever the self-host understands about Gorget matches what it emits" (`:13880`).

Why the comparison starts at stage-2, not stage-1: stage-1 is built by the *Rust* `gg` (different internal lowering), so stage-1's emission can legitimately differ from later stages. `stages[0]` (stage-0's output, the stage-1 body) is allowed to differ; convergence is checked from stage-2 onward (`:13994`). On failure, every stage is persisted to `temp_dir` with pairwise `diff` hints in the panic message (`:14085`).

Why `MAX_GEN = 5` and not the stricter `N = 2`: self-host ownership-cascade changes take up to ~4 generations to quiesce, because each ownership-tag flip in `lower.gg`'s internals adds one extra `void*` slot per stage until the in-source algorithm and the in-binary lowering agree (`:13884`). Production bootstraps (Rust, OCaml, GHC) routinely allow N=4–5; the strict N=2 invariant is to be restored once the ownership cascade stabilises — this is a roadmap note, not a current guarantee.

Both bootstrap tests early-return under the LLVM backend (`skip_under_llvm()`, `:13725` / `:13898`) — they assert C-emission specifics. They are `#[serial(self_host_lowerer_driver)]` so they don't race each other on the shared cached driver.

### self_host_e2e — the whole-corpus version

`self_host_e2e` (`:14383`) is the heavyweight diagnostic: it builds stage-1 from the kitchen-sink preamble fixture (`_self_host_e2e_preamble.gg`, which imports every std module so the union of runtime templates is present, `:14395`), then for **every** fixture compiles the self-host's C, links it against that preamble, runs it, and diffs stdout against the Rust binary's. It classifies each fixture (`Match` / `OutputMismatch` / `CcFailed` / `RuntimeCrashed` / `DriverCrashed` / `RustNotBuildable` / `RustRuntimeFailed`, `:14580`). Like the comparison tests it **always passes** ("Diagnostic test — always passes"); the summary is the signal. It is gated behind `GG_FULL=1` (`skip_unless_full()`, `:14385`) because it costs ~2.5 min solo. The preamble-stitching is delicate — `strip_user_struct_defs` (`:14443`) removes the preamble's `struct __gg_X { ... };` bodies and dead `__lir_g<N>` globals so stage-1's re-emitted definitions don't collide.

### Runtime-parity harness — `self_host_runtime` / `self_host_runtime_diff`

Every test above this point measures **structure, not behavior**. `c_emit_comparison` compares emitted-C *function counts*, so a body-level miscompile — right count, wrong code — is invisible to it. The canonical instance: `lower.gg`'s `EUnaryOp` silently dropped the `~` operator (right function count, wrong body) and sat undetected in that blind spot until a second-order drop-elaboration double-free surfaced it. A runtime diagnostic over the corpus later confirmed `~` was *representative* of a large invisible class — only a fraction of fixtures the self-host "matched" by fn-count actually produced correct output. **Function-counting cannot see a wrong body; runtime output can.** That is why a behavior-level harness exists alongside the count-based comparisons.

`self_host_runtime_diff` (`tests/integration.rs:15961`) and `self_host_runtime` (`:16140`) compile each fixture *through the self-host* (`driver F lib --emit-c` → `cc` → run) and diff its **stdout** against the Rust `gg run` oracle. They compare OUTPUT, never C text: the two implementations legitimately emit non-identical C for the same program (e.g. self-host emits bare `sqrt`/`pow`, Rust emits `gorget_sqrt` wrappers — both link via `-lm`, both correct). Parity is runtime-output equality; do not let this be "optimized" into a C diff.

**Splice-free — and why that matters.** This harness's predecessor is `self_host_e2e` (above), which stitches a kitchen-sink Rust-emitted runtime preamble onto the self-host's body-only emission (`strip_user_struct_defs` removes colliding definitions). That splice means a compile failure is ambiguous — the harness's stitching or the self-host's output? The newer harness requires the self-host to emit a **complete program** (runtime preamble + body) on its own, so a CC-FAIL is unambiguously the self-host's bug, not a harness artifact. The self-host inlines its runtime like the C backend's `emit_runtime_modules` (`src/backend/c_lir/emit_types.rs`), reading the same on-disk shared runtime files (`src/backend/c/runtime/*.c`, `include_str!`-ed by `c_runtime.rs`) that the Rust backends consume — one source of truth, no vendored copy to drift.

**Two entry points, two roles:**

- `self_host_runtime_diff` — DIAGNOSTIC, env-gated (`GG_RUNTIME_DIFF=1`), always-pass. Full corpus, live `gg run` oracle. Prints the honest parity number and the per-family backlog. Like the `*_comparison` tests, a green `cargo test` says nothing — the printed count is the only signal.
- `self_host_runtime` — LOCK-IN NET, default-running, build-breaking. Oracle = committed snapshots in `tests/fixtures/runtime_snapshots/<stem>.out`. For each snapshotted fixture it re-emits via the self-host and asserts the run output still matches; a regression fails the build. The passing set only grows. Regenerate with `GG_REGEN_RUNTIME_SNAPSHOT=1` (`regenerate_runtime_snapshots`, `:16267`), which snapshots only fixtures that are a STABLE match (self-host twice + oracle twice, all identical) — the double-pass run-twice gate keeps non-deterministic fixtures out of the locked set.

**Categorization** (the diagnostic's per-fixture verdict): MATCH (stdout == Rust); WRONG-OUTPUT (runs, stdout ≠ Rust → a real silent miscompile, the `~` class, highest-value gap); CC-FAIL (the complete `.c` won't compile → a real self-host gap, trustworthy *because* there is no splice); EMIT-FAIL / TIMEOUT (self-host crashes or hangs); EXCLUDED.

**Exclusion discipline.** `runtime_parity_excluded` (`:15714`) holds ONLY fixtures whose *Rust* output is itself non-deterministic or host-specific (time/date, randomness, network/sockets, sleep/timing, stress/bench, GPU/windowing). Per CLAUDE.md "don't redesign around compiler gaps", a fixture the self-host MISCOMPILES is **never** excluded — it surfaces as WRONG-OUTPUT / CC-FAIL and goes to the TODO backlog. Inflating parity by excluding self-host failures is the forbidden anti-pattern. The predicates use precise shapes (prefix / exact-name / explicit `contains`), never a loose substring on a short token, and the run-twice stability filter is the final arbiter for the lock-in net. The concurrency family (channel/mutex/thread/task-group/async) is largely deterministic and is INCLUDED — only the genuinely non-deterministic members are excluded.

This is the operational form of the **runtime-parity north-star** ([Chapter 26](26-self-host-frontend.md)): the fn-count comparisons are a secondary structural diagnostic that overstates correctness; runtime-output parity is the primary metric.

## Report generation — `src/report.rs`

`src/report.rs` is unrelated to parity; it turns an execution **trace** into a standalone HTML report. The sole public entry point is `generate_html_report(trace_path, output_path)` (`src/report.rs:576`), invoked from two CLI paths in `main.rs`:

- `gg report <file>.trace.jsonl [--output <path>]` (`src/main.rs:2383`, generator call at `:2417`).
- `gg test --report html` / `gg run ... --report=html`, which implies `--trace` unless `--no-trace` is given (`src/main.rs:3122`) and generates `<stem>.report.html` from `<stem>.trace.jsonl` after the run (`:3262`–`:3269`).

### Trace format and pipeline

The input is a JSONL trace file: one JSON event per line, parsed by `parse_trace_file` → `parse_trace_line` into the `TraceEvent` enum (`src/report.rs:8`). The event kinds are `TestStart`/`TestEnd`, `Call`/`Return`, `Loop`, `StmtStart`/`StmtEnd`, and `Branch` — each carrying a `depth` (and vars/src as relevant). The trace itself is emitted by the compiled program when built with `--trace` (the C backend instruments calls/statements/branches); `report.rs` only consumes the file.

The pipeline is: parse → `build_tree` (`:240`) → `build_report` (`:364`) → HTML.

`build_tree` reconstructs nesting from the flat event stream using a frame stack keyed on `depth`. `Call`/`StmtStart` push `Normal` frames closed explicitly by their matching `Return`/`StmtEnd`; `Loop` and `Branch` push auto-closing frames that pop when the next sibling of the same kind arrives at the same depth or any event arrives at a lower depth (`FrameKind` enum at `:223`; close logic at `:254`). Unmatched closers and depth jumps emit `[trace-tree] warning:` to stderr rather than aborting.

`build_report` (`:364`) groups the tree into per-test `TestResult`s. The crucial branch is **trace-mode detection**: `is_trace_mode = tests.is_empty() && !orphan_events.is_empty()` (`:420`) — i.e. a raw program trace with no `TestStart`/`TestEnd` markers renders as a single "TRACE" entry, whereas a test trace renders pass/fail rows. `total_passed`/`total_failed` count `TestResult`s by status, excluding `trace` (`:431`–`:432`).

`generate_html_report` emits a self-contained HTML document (inline `REPORT_CSS`, light/dark toggle, collapsible call-tree) with per-test PASS/FAIL/CRASHED/TRACE badges (`:635`) and a pass-rate bar (test mode only, `:618`). Helper functions render values: `format_json_value` (`:31`, `null`→`"void"`), `substitute_vars` (`:54`, word-boundary substitution of variable values into source text, including `{var}` interpolation collapse), and `substitute_result` (`:124`).

## Auxiliary test harnesses

These share the `tests/` directory family and are worth knowing, though they sit outside the parity story.

### tests/lir_ab.rs — backend A/B (now effectively A/A)

`lir_ab.rs` was written to diff stdout between the GIR C backend and the LIR→C backend over a curated fixture list (704 `#[test]` entries; `ab_test` opens at `:181` and asserts the two stdouts equal at `:191`). It builds one side with the default flags (`run_gir`, `:14`) and the other with `--backend=lir` (`run_lir` at `:73`, the `--backend=lir` arg at `:80`). **But the GIR backend was retired** (`MEMORY.md` "LIR is sole backend"): in `main.rs` the backend selector defaults to `c-lir` and only `"llvm"` diverges — every other value, including `lir` and the absent flag, falls into the `_ =>` arm that yields `CLirBackend` (`src/main.rs:2502`, `:730`–`:732`). So both sides now run through the *same* C-LIR backend, making this an A/A equivalence smoke test (it does still catch nondeterminism and gross build breakage). The test file's "GIR" naming is historical.

### tests/lints.rs — layering-discipline ratchets

`lints.rs` scans the *source tree* (`src/**/*.rs` and `tests/fixtures/self_host_*`) for layering-discipline anti-patterns and asserts a count never grows past a budget. Five ratchets: `no_growth_in_name_prefix_routing` (`:166`, counts `starts_with("X__")` dispatch on mangled-type prefixes against `BUDGET`), `no_typed_metadata_sidecars` (`:321`, `BUDGET = 0` — no parallel `HashMap<key, watched-metadata>`), `no_growth_in_phase_d_proxy_reads` (`:434`, Phase D ownership proxy reads), `container_literal_arms_count` (`:600`), and `no_growth_in_self_host_name_prefix_routing` (`:637`) — the last one is the self-host counterpart, scanning `tests/fixtures/self_host_*` for the same mangled-prefix dispatch anti-pattern against its own budget. Each budget carries a commit-cited changelog in its doc comment; the rule is "lower the budget when you migrate, raise it only with a cited justification." These are the enforcement arm of [Chapter 24](24-layering-discipline.md) / [Chapter 25](25-structural-guards.md).

### tests/security.rs — adversarial memory-safety suite

`security.rs` builds each fixture under `tests/fixtures/security/` with the compiler's own `--sanitize` flag (`:156`), which the compiler expands to `-fsanitize=address,undefined` (described in the module comment, `:27`), and classifies it via four helpers (`:202`–`:293`): `security_safe` (must build and run exit-0 with expected stdout), `security_traps` (must panic with a Gorget-level message, not raw C UB), `security_rejected` (must be rejected at `gg check` with a stderr pattern), and `security_known_unsafe` (asserts a *known* bug is **still present** — when the bug is fixed the test fails, forcing a reclassification). Sanitize builds are slow, so the default build timeout is 180 s (vs the integration suite's 120) and auto-scales by load (`:39`–`:58`).

### tests/str_runtime.rs — standalone runtime test

`str_runtime.rs` compiles a hand-written C program that exercises the `Str` fat-pointer runtime functions directly, **bypassing the Gorget language pipeline entirely**: it concatenates `RUNTIME_PREAMBLE` + `PANIC_NORMAL` (both imported from `gorget::backend::c::c_runtime`, `:4`) + an inline `TEST_MAIN` (a local `const TEST_MAIN: &str` defined in the test file, `:89`), compiles with `cc -std=c11 -Wall -Wextra -Wno-unused-function -Werror -lm` (`:31`), runs it, and diffs against `EXPECTED_OUTPUT`. Unlike the comparison tests this one **does assert** — it's a unit test of the runtime C, not a parity diagnostic. See [Chapter 18](18-runtime-abi.md) for the runtime ABI it pins.
