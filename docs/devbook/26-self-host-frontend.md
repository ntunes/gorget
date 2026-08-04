# 26 — The self-host frontend

*Verified against commit `ffd58b65` (branch `gorget-2`).*

This chapter describes the **self-hosted Gorget compiler**: a near-complete
reimplementation of the `gg` frontend (lexer → parser → resolver → type checker
→ GIR lowering → LIR lowering → SSA → C codegen) written *in Gorget*, living
under `tests/fixtures/self_host_*/`. It is not a separate product — it is a set
of `.gg` programs that the Rust `gg` compiles and runs as integration fixtures.
Its outputs are compared, stage by stage, against the Rust frontend's outputs by
the `*_comparison` tests in `tests/integration.rs`, and its ability to recompile
itself to a byte-identical fixed point is verified by `self_host_bootstrap` /
`self_host_bootstrap_fixed_point`. This chapter is the overview for the whole
self-host subsystem; per-phase chapters (3, 4, 7, 9, 12, 14, …) each carry an
"In the self-host" section that drills into how *that* phase is mirrored.

## Why the self-host exists — three simultaneous roles

The self-host is deliberately load-bearing in three ways at once. All three are
non-negotiable; the third is what distinguishes this from a throwaway bootstrap
(the design narrative, formerly `self-host-resource-model.md` §0, is folded into this chapter):

1. **Stress test.** Compiling ~30k lines of `.gg` across the self-host
   directories exercises corners of the language no hand-written fixture
   reaches. When self-host crashes or miscompiles, the Rust compiler has a bug.
2. **Regression net.** The `*_comparison` tests plus
   `self_host_bootstrap_fixed_point` lock the implementation against silent
   drift — any change to the Rust frontend that the self-host doesn't track
   shows up as a comparison-count regression.
3. **Reference-grade idiomatic Gorget.** The self-host is the canonical answer
   to *"how should this kind of code be written?"* This is the rule that forbids
   defensive workarounds (see "The no-defensive-workarounds rule" below).

## What's reimplemented, and where it lives

Each `self_host_*` directory is a self-contained multi-file Gorget program with
its own `driver.gg` entry point. The directories are *cumulative*: each one adds
the next pipeline stage on top of the same upstream sources.

| Directory | Adds | Driver pipeline |
|-----------|------|-----------------|
| `self_host_lexer/` | tokenizer | source → tokens → canonical token lines (`driver.gg:21-31`) |
| `self_host_parser/` | recursive-descent parser | + parse → canonical AST lines |
| `self_host_resolver/` | name resolution | + resolve → `DEF`/`RES`/`SCOPE` lines |
| `self_host_typechecker/` | type inference + traits + derive + meta | + typecheck → `TYPE` lines (no loader) |
| `self_host_check/` | the loader-aware `check` path | full parse → load imports → derive → meta → resolve → typecheck |
| `self_host_lowerer/` | GIR lowering, LIR lowering, SSA, drop elaboration, C codegen | the full frontend; can emit GIR / LIR / C (`driver.gg:75-106`) |

The lowerer's `driver.gg` (`tests/fixtures/self_host_lowerer/driver.gg:24-106`)
is the most complete: it runs `parse_source → load_imports → expand_derives →
expand_meta_* → resolve_module → type_check_module → lower_module → run_validators
→ lower_gir_to_lir → eliminate_dead_globals → construct_ssa → elaborate_drops →
generate_c`, gated by `--emit-gir` / `--emit-lir` / `--emit-c` (alias `--lir-c`)
flags that mirror the Rust `gg`'s `--emit-gir` / `--emit-lir` / `--emit-c-lir`.

### Source files per directory

The phase sources have stable names across directories: `lexer.gg`, `parser.gg`,
`ast.gg`, `resolve.gg`, `scope.gg`, `types.gg`, `typecheck.gg`, `infer.gg`,
`traits.gg`, `derive.gg`, `meta.gg`, `format*.gg`, `diagnostic.gg`; the lowerer
adds `gir.gg`, `lower.gg`, `lir.gg`, `lir_lower.gg`, `lir_ssa.gg`,
`lir_codegen.gg`, `drop_elab.gg`, `validate.gg`, `loader.gg`, and the
`format_gir.gg` / `format_lir.gg` IR dumpers. The Rust-side equivalents are the
chapters cross-referenced in the table above.

## Symlinked vs independent copies

A subtle but critical fact: the phase sources are **not** a single shared copy.
Some directories symlink to a canonical source; others hold *independent copies*
that have drifted. This matters because a fix to a parser primitive must be
applied in **every** directory that has its own copy of `parser.gg`.

- **`self_host_lowerer/` and `self_host_check/` symlink** their upstream phase
  files into `self_host_typechecker/`. `ls -la tests/fixtures/self_host_lowerer/`
  shows `ast.gg → ../self_host_typechecker/ast.gg`,
  `parser.gg → ../self_host_typechecker/parser.gg`, plus `lexer.gg`, `resolve.gg`,
  `scope.gg`, `types.gg`, `typecheck.gg`, `infer.gg`, `traits.gg`, `derive.gg`,
  `meta.gg`, `format*.gg`, `diagnostic.gg`, `ids.gg`. Editing typechecker's copy
  changes the lowerer and check builds automatically.
- **`self_host_lexer/`, `self_host_parser/`, `self_host_resolver/`,
  `self_host_typechecker/` hold independent copies.** `md5sum` confirms the three
  independent `parser.gg` copies (parser/resolver/typechecker) all have distinct
  hashes, while `self_host_lowerer/parser.gg` and `self_host_check/parser.gg` are
  byte-identical to `self_host_typechecker/parser.gg` (the symlinks).
  `self_host_lexer/` has no `parser.gg`. Likewise the four `lexer.gg` copies
  (lexer/parser/resolver/typechecker) and three `ast.gg` copies
  (parser/resolver/typechecker) all have distinct hashes.

The practical rule (also recorded in project memory): when you change a parser /
lexer / AST primitive, fix it in **all** relevant directories — the symlinked
ones inherit automatically, the independent copies do not. A divergence between
copies is *expected* (a directory only needs the phases up to its stage to be
current), and is exactly why each comparison test rebuilds its own driver.

## The comparison tests — diagnostic, always-pass

*(Chapter 27 covers the test machinery, output normalization, and report
generation in depth; this section is the overview.)*

Each `*_comparison` test builds the relevant self-host driver, runs it over
every `tests/fixtures/*.gg` fixture in parallel, and compares its canonical
output against the Rust frontend's output for the same input.

> **These tests are DIAGNOSTIC and ALWAYS PASS.** They contain no assertion on
> the match count. A green `cargo test` says *nothing* about parity. The only
> parity signal is the printed count line — read it with `--nocapture`. (See e.g.
> `lexer_comparison`'s closing comment, `tests/integration.rs:9526`: *"The test
> passes even with mismatches — this is a diagnostic/tracking test."*)

The driver/comparison entry points:

| Test (`tests/integration.rs`) | Driver | Compares |
|-------------------------------|--------|----------|
| `lexer_comparison` (`:9346`) | `self_host_lexer/driver.gg` | canonical token lines, find-first-divergence |
| `parser_comparison` (`:12406`) | `self_host_parser/driver.gg` | canonical AST lines |
| `resolver_comparison` (`:12683`) | `self_host_resolver/driver.gg` | `DEF` + `RES` lines (`SCOPE` skipped) |
| `type_comparison` (`:12997`) | `self_host_typechecker/driver.gg` | `TYPE` lines (set-equality + superset) |
| `check_comparison` (`:13193`) | `self_host_check/driver.gg` | `TYPE` lines via the full loader path |
| `lowerer_comparison` (`:13390`) | `self_host_lowerer/driver.gg` | GIR `fn` count |
| `c_emit_comparison` (`:13549`) | `self_host_lowerer/driver.gg` | C user-fn count from `--emit-c-lir` vs `--lir-c` |

How "match" is defined differs by stage and is worth understanding:

- **Lexer / parser** compare *line sequences* and report the first divergence
  with surrounding context (`tests/integration.rs:9417-9465`). A driver crash is
  a distinct outcome from a mismatch.
- **Resolver** normalizes first (`normalize_resolver_output`,
  `tests/integration.rs:12659`): `DEF` lines have their trailing `start:end`
  span stripped (Gorget's AST doesn't store name spans), `SCOPE` lines are
  dropped entirely (Rust's `Expr::Block` makes extra scopes), and `RES` lines —
  the core correctness check — are compared verbatim.
- **Type checker / check** use *set* comparison with a **superset** allowance
  (`tests/integration.rs:13089-13093`): if the self-host's `TYPE` set exactly
  equals Rust's it's `Matched`; if Rust's set is a strict *subset* of the
  self-host's it's `SupersetMatched` and still counts toward parity. This encodes
  the project philosophy that the self-host should produce *more* correct
  information, never suppress output to match a gap. The report prints exact and
  superset counts separately (`tests/integration.rs:13145`).
- **Lowerer / c_emit** are deliberately coarse: they compare the *count* of
  emitted functions, not byte-identical IR/C. `user_fn_count`
  (`tests/integration.rs:13565`) counts `) {`-terminated function-body openings
  after the `Function Definitions` section marker. A self-host that emits zero
  functions where Rust emits some is `RustOnly` (the self-host couldn't process
  it at all), distinct from a count `Mismatched`. "Matched" here means *fn-count
  parity*, so true byte-level C parity is strictly lower than the reported rate.

### Reading current parity (procedure, not a number)

Any parity figure in this book would be stale the moment it's written. To get
the live number for a stage, run its comparison test with `--nocapture` and read
the printed line — for example:

```bash
cargo test --test integration resolver_comparison -- --nocapture --test-threads=4 \
  2>&1 | tee /tmp/rescmp-$RANDOM.log
# => "Fixtures compared: 1120, matched: 1103, mismatched: 17, crashed: 0"
```

The report shape differs by stage, so read whichever line that stage prints:

- The **lexer** prints three separate lines `Fixtures compared / Crashes /
  Mismatches` — there is no `matched` field (`tests/integration.rs:9496-9498`).
- The **parser** and **resolver** print the combined `Fixtures compared, matched,
  mismatched, crashed` line (`tests/integration.rs:12541`, `:12838`).
- The **type / check** tests print `exact / superset / total / mismatched /
  crashed` (`:13145`, `:13348`).
- The **lowerer** prints `Matched / Error-only / Real mismatches / Crashes` plus
  an `Adjusted` rate (`:13497-13507`).
- **`c_emit`** prints `Matched / Rust-only / Mismatched / Self-host crashes /
  Rust crashes` plus a match rate (`:13664-13676`).

The number of fixtures the suite compares grows as new `.gg` fixtures land, so
always read `Fixtures compared`, never assume a denominator.

> **Do not trust a quoted figure** — including any in project memory. As of this
> commit a fresh `lexer_comparison` reports hundreds of mismatches (e.g.
> `_self_host_e2e_preamble.gg` lexes `Mutex` as `kw:Mutex` self-host vs
> `ident:Mutex` Rust — a keyword-set drift between the independent `lexer.gg`
> copies), despite memory recording the lexer as "green". The largest remaining
> parity gap is `c_emit` (true C output), not the resolver or parser. Re-derive
> before quoting.

## The bootstrap fixed-point

*(Chapter 27 treats the bootstrap loop and its generation cap in depth; this
section is the overview.)*

The comparison tests check *agreement with Rust* per stage; the bootstrap tests
check the self-host's *internal consistency* — that the compiler, recompiled by
itself, converges to a byte-identical fixed point. The pipeline is a fixed point
*at convergence*: whatever the self-host understands about Gorget matches what it
emits (`tests/integration.rs:13876-13882`).

`self_host_bootstrap_fixed_point` (`tests/integration.rs:13897`) works by:

1. Stage 0: Rust `gg` builds `self_host_lowerer/driver.gg` into a native binary.
2. Stage 0 → stage 1: that binary emits C for `driver.gg` itself (`--lir-c`),
   which is compiled (with the Rust-emitted runtime preamble spliced in) into a
   stage-1 binary.
3. Iterate: each stage emits the next stage's C, up to `MAX_GEN` generations.
   The test passes as soon as `stage(N).c == stage(N+1).c`.

`MAX_GEN` was raised from 2 to 5 because ownership-cascade changes in the
self-host (e.g. `LoBorrowed` propagation through `.unwrap()` of borrowed Options)
take ~4 generations to quiesce: stage-1 is built by *Rust* lowering, stage-2+ by
the *self-host* lowering, and each ownership-tag flip costs one extra void* slot
per stage until the in-source algorithm and the in-binary lowering agree
(`tests/integration.rs:13884-13894`). The expensive lowerer driver build is
shared across `lowerer_comparison`, `c_emit_comparison`, `self_host_bootstrap`,
and `self_host_bootstrap_fixed_point` via `build_gg_dir_cached`
(`tests/integration.rs:9191`), and those tests are `#[serial(self_host_lowerer_driver)]`.

A green `bootstrap_fixed_point` is a **milestone, not the finish line**: it
proves a closed self-reproduction loop, not feature parity with Rust. The finish
line is the comparison counts climbing toward 100%.

## Shared canonical schema (`compiler/data/schema.gg`)

The self-host and the Rust compiler now share a single source-of-truth for the
resource + runtime-function model, at `compiler/data/schema.gg`. This file holds
**pure data definitions only** — enums and structs describing the static shape of
a resource entry or a runtime-function entry, no expression-level helpers
(`compiler/data/schema.gg:9-13`). Key types:

- `enum CopySemantics` / `CollectionKind` / `BoxKind` / `LirType` — the
  categorical axes of a resource (`schema.gg:33-56`).
- `struct ResourceMetadata` — `runtime_name`, `size_bytes`, `lir_type`,
  `drop_fn` / `clone_fn` / `materialize_fn` (each `Option[String]`),
  `copy_semantics`, `collection_kind`, `box_kind`, `opaque_handle`,
  `method_prefix`, `c_typedef_name`, `is_typed_constructor`
  (`schema.gg:58-87`).
- `struct RuntimeFn` + `CRuntimeType` / `AbiKind` / `SideEffects` — the typed
  runtime-symbol signature table (`schema.gg:105-162`).

The self-host imports these directly (`gir.gg:8-11`:
`from compiler.data.schema import ResourceMetadata, CopySemantics, …`). The Rust
compiler consumes a **hand-written mirror** at `src/resource_schema.rs`,
whose header states *"the Gorget side is the source of truth — this Rust file
exists so that the loader at `src/resources.rs` can produce typed values for
Rust consumers"* and that *"any field change here MUST bump `SCHEMA_VERSION`"*
(`src/resource_schema.rs:1-10`). `SCHEMA_VERSION` lives in
`compiler/data/resources.gg:36`. The duplication retires when the self-host
replaces Rust as the canonical compiler. This is the layering discipline
(chapter 24) applied across the language boundary: one source of truth per axis,
typed not name-matched.

## Typed metadata in the self-host IR (resource model)

The former `self-host-resource-model.md` deep-dive (folded into this chapter)
was the roadmap for porting the
Rust unified-resource-model (chapter 13) into the self-host's own GIR. Re-derived
against current source, the status is well ahead of that doc's present/future
tense:

- **Phase A (typed resource metadata) is shipped.** `GirModule` carries
  `Dict[String, ResourceMetadata] resource_metadata`
  (`tests/fixtures/self_host_lowerer/gir.gg:330`), read through the lazy-populating
  accessor `resource_meta_for(&gmod, name)` (called throughout `lir_lower.gg`,
  e.g. `:142`, `:161`, `:172`), with the prefix cascade centralized in
  `build_resource_metadata` as the single source of truth. This replaces the old
  scattered `name.starts_with("Vector__")` dispatch.
- **Phase D (local ownership state) — ownership-label computation is shipped,
  and clone codegen has now shipped too.** `GirLocal` carries `LocalOwnership
  ownership` and `BorrowOrigin borrow_origin` (`gir.gg:223-227`); the `Operand`
  enum has `OpClone(int)` and `OpBorrow(int)` alongside `OpCopy` / `OpMove`
  (`gir.gg:52-61`), and the `ConsumeKind` enum classifies the read position
  (`gir.gg:167-179`). The `op_consume` dispatcher (`lower.gg:1389`, doc comment
  `lower.gg:1305-1342`) writes the `OpClone` / `OpBorrow` / `OpMove` label from
  the source local's `ConsumeKind` and ownership tag — and for `LoBorrowed` /
  `LoView` sources at a genuine consume position it now returns **`OpClone`**, not
  `OpBorrow` (`lower.gg:1466-1471`, comment `lower.gg:1451-1452` *"a-5
  (clone-on-borrow) — WIRED"*). The labels now drive real codegen: the
  operand-emission site is `lower_operand` (`lir_lower.gg:2403-2533`), where the
  `OpClone` arm (`lir_lower.gg:2453-2521`) lowers to a **real runtime clone call**
  — `resource_clone_fn` picks the matching `T__clone` / `gorget_*_clone` symbol
  and the operand is passed by pointer (via `ISlotAddr` for a value-typed slot, or
  the loaded pointer for a `Ptr`-to-struct slot) to an `ICallExtern`. This is
  "Phase 2c COMMIT 2 + Phase 2.3", shipped. What is still pending is `OpMove`
  move-zero codegen: `OpMove` (and `OpBorrow`) still lower to a plain `ISlotLoad`
  (`lir_lower.gg:2445-2451` / `:2522-2532`), with the `SlotAddr` conversion for a
  by-pointer move done downstream in the `GICallExtern` handler. (The in-source
  doc comment at `lower.gg:1338-1342` still says "all four operand modes lower to
  the same `ISlotLoad`" — that line predates the `OpClone` wiring and is stale; do
  not read it as the current status.)
- **Phase C (strict move/clone validation) is shipped and fatal.** All three
  validators in `validate.gg` — `validate_resource_field_reads`,
  `validate_resource_call_args`, `validate_resource_moves` — are CLOSED
  (zero violations) and promoted to **unconditional fatal**: `run_validators`
  runs them before the GIR/LIR print and calls `exit(1)` on any violation
  (`tests/fixtures/self_host_lowerer/validate.gg:288-301`). The build halts on
  the first violation in any program. The `GG_VALIDATE_RESOURCE_MOVES` env gate
  survives only as a diagnostic log sink for sweep tooling (`validate.gg:302-309`).
  The validators mirror the Rust `src/ir/validate.rs` checks and run inside the
  lowerer's `driver.gg` pipeline (`driver.gg:72`). *(The internals doc still
  tags Phase C "IN PROGRESS 2026-05-10"; that status is stale — re-derive from
  `validate.gg` as above.)*

The folded roadmap doc's §6.4 `pending_phis` workaround in `lir_ssa.gg` has also
been **retired**: SSA now inserts phi params in place via chained borrow rather
than deferring them to a post-pass reconstruction
(`tests/fixtures/self_host_lowerer/lir_ssa.gg:165-172`).

## The no-defensive-workarounds rule

This is the rule that makes the self-host's third role (idiomatic showcase) real
rather than aspirational. It is the operational form of
`CLAUDE.md` *"Self-host as the elegance showcase"* + *"Don't redesign around
compiler gaps"*, formalized in the former `self-host-resource-model.md` §0
(folded into this chapter):

> When the stress-test role surfaces a gap — a pattern that *should* compile
> cleanly but doesn't, or compiles to wrong code — the response is always: **fix
> the gap in Gorget first, then write the self-host code the right way.** Never:
> file the gap and ship the workaround indefinitely.

The consequences for a contributor reading or editing self-host code:

- **No defensive code without a live, cited bug.** Workaround comments
  ("parallel because…", "wrapper to avoid…", "rebuild instead of mutate…") are
  technical debt with a stale justification. If the bug they cite is fixed,
  delete the workaround and use the idiomatic shape. The fixed-point and
  comparison tests catch regressions.
- **A compiler fix is incomplete until the dodge it enabled is gone.** Search
  every self-host directory for the workaround pattern before declaring a fix
  shipped — the symlink set propagates, the independent copies do not.
- **The wired-in expected output is the load-bearing artifact, not the comment.**
  A commented redesign still buries the bug, because the canonical-looking shape
  is what new contributors copy.

Concrete fossils this rule has already retired (cited so they're not re-grown):
the `StructRegistry` parallel `Vector[String]` + `Vector[int]` → `Dict`
(commit `8d944ddc`), the `type_info_keys_safe` Dict-keys wrapper, the
`sr_lookup` wrapper (retired; only a comment at `lower.gg:4670` still references
it), the flat-`pending_phis` SSA dodge, and the get-then-unwrap typed-accessor
wrapper in `traits.gg`. That last one is instructive: its comment cited *"a
codegen bug where inline `module.items.get(i).unwrap()` zero-inits the local
instead of loading"* — but the bug was fixed long ago, the inline
`.get(i).unwrap()` shape is used directly at ~10 live sites (e.g. `meta.gg`,
`loader.gg`, `driver.gg`) that compile and bootstrap fine, and the wrapper had
**zero callers** by the time it was deleted: a dead fossil whose stale comment
was a false historical record. The running audit log of gaps surfaced → fixed →
re-implemented is maintained inline in this chapter; an entry only counts as
fully retired once the idiomatic re-implementation has replaced the workaround
everywhere (not merely "the bug is fixed").

## Pointers

- Per-phase self-host detail: chapters 3 (lexer), 4 (parser/AST), 5 (formatter),
  6 (meta/derive), 7 (resolution), 9 (type checking), 12 (GIR lowering),
  14 (LIR/SSA), 15 (drop elaboration) each have an "In the self-host" section.
- The deep treatment of the test machinery this chapter overviews — the
  `*_comparison` tests, the `self_host_bootstrap` / `self_host_bootstrap_fixed_point`
  loop, and the HTML report generator — is chapter 27 (Comparison, bootstrap &
  report generation).
- Roadmap (not status — that's re-derived above): the former
  `self-host-resource-model.md` deep-dive, folded into this chapter.
- The discipline this chapter's last section enforces: chapter 24 (layering),
  chapter 25 (structural guards), and the `CLAUDE.md` sections
  *"Self-host as the elegance showcase"* and *"Don't redesign around compiler
  gaps"*.
