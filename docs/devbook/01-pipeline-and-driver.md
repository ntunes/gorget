# 1 — The pipeline & the `gg` driver

*Verified against commit `ffd58b65` (branch `gorget-2`).*

This chapter is the map. It describes what happens between a `.gg` file and a
native binary, who runs each phase, and what every `gg` subcommand actually
does. The driver lives entirely in `src/main.rs` (a binary crate that links the
`gorget` library crate); the phases it orchestrates live in `src/lexer/`,
`src/parser/`, `src/semantic/`, `src/ir/`, `src/lir/`, `src/bir/`, and
`src/backend/`. Subsequent chapters drill into each box; this one keeps you
oriented and names the exact entry points so you can follow control flow.

## The pipeline, end to end

A normal `gg build`/`gg run` walks this chain. Every box names the *function*
the driver calls and the *module* it lives in — those are the load-bearing
seams.

```
.gg source
  │
  ▼
Lexer          src/lexer/            Logos tokenizer + indentation tracking
  │                                  Lexer::new(src) → Iterator<Spanned<Token>>   (lexer/mod.rs:70, :888)
  ▼
Parser         src/parser/          Recursive descent + Pratt expressions
  │                                  Parser::new(src).parse_module() → ast::Module (parser/mod.rs:60, :464)
  ▼
Loader         src/loader.rs        Resolve `import`s, merge into one Module
  │                                  load_imports() in main.rs:33 → ModuleLoader::load_all
  ▼
Semantic       src/semantic/        analyze_with_source_dir() (semantic/mod.rs:96)
  │                                  Passes 0 … 5 (see below); produces AnalysisResult
  ▼
GIR lowering   src/ir/lowering/     lower_module(&module, &result, &opts)        (ir/lowering/mod.rs:77)
  │                                  monomorphization, drop insertion, closures
  ▼
GIR optimize   src/ir/transforms/   optimize::optimize_module(&mut gir)          (main.rs:556)
  ▼
LIR lowering   src/lir/lower/mod.rs lower::lower_module(&gir) → LirModule        (main.rs:681)
  │                                  split_critical_edges → construct_ssa (per fn)
  │                                  wire_collection_bridges, promote_runtime_calls,
  │                                  compute_module_{pointee,value}_types, value_origins
  ▼
BIR            src/bir/             BirModule::from_lir(lir)                      (bir/mod.rs:77)
  │                                  newtype over LirModule; expands canonical ops,
  │                                  validates "primitives only" → backends can't get raw LIR
  │                                  then optimize + recompute types post-BIR
  ▼
Backend        src/backend/         Backend::generate(&bir) → CodegenOutput      (backend/mod.rs:382)
  │                                  c_lir::CLirBackend (default) or llvm::LlvmBackend
  ▼
cc / clang+llc → binary             try_build_ir() drives the C compiler (main.rs:914+)
```

The whole sequence for a build is `try_build_ir` (`src/main.rs:404`). It is one
long function: parse, report parse errors, `load_imports`, `analyze`, report
semantic errors/warnings, GIR-lower, GIR-optimize, then the LIR→BIR→backend
run, then invoke the system C compiler (or the LLVM `llc`+`clang` pipeline).
The `--emit-gir` / `--emit-lir` / `--emit-c-lir` flags short-circuit it at the
matching stage and dump text instead of producing a binary (`src/main.rs:560`,
`:581`, `:637`).

### Why BIR exists between LIR and the backend

BIR (`src/bir/mod.rs`) is a *newtype* wrapper around `LirModule`, not a separate
IR data structure. Its job is a type-system guarantee: a backend takes
`&BirModule`, and `BirModule::from_lir` is the only constructor, so a backend can
never be handed un-expanded canonical ops. `from_lir`
(`src/bir/mod.rs:77`) runs `lower_lir_to_bir` (`src/bir/lower.rs:41`), which
expands canonical high-level LIR ops (the full set is the canonical-op arms in
`check_inst`, `src/bir/validate.rs:54-108`: `SizeOf`, `EnumInit`, `EnumCheck`,
`EnumExtract`, `StructInit`, `CowClone`, `TraitCall`, `HofExpand`, `AddressOf`,
`BoxAlloc`, `CollectionCtor`) into primitives, then `assert_primitives_only`
(`src/bir/validate.rs:36`) validates that none survive. The module's
value/pointee types are recomputed after BIR synthesis (`src/main.rs:714-716`),
and the LIR optimizer runs post-BIR (`src/main.rs:712`) so DCE/fold/CSE and
drop elaboration see the expanded primitives rather than the opaque high-level
shapes. (The type-computation triple
`compute_module_{pointee,value}_types`/`compute_module_value_origins` is the
one stage that runs twice — once pre-BIR at `src/main.rs:699-701` and again
post-BIR at `:714-716`.) See Chapter 16 for the full design.

### A subtlety: `--backend=llvm` composes its own runtime

The C backend pulls the runtime in through `emit_runtime_modules`; the LLVM
path has to assemble the runtime C by hand and compile it to a `.o`
(`compile_llvm_pipeline`, `src/main.rs:1089`). That manual assembly is a long
chain of `concat_source.contains("std.async")`-style feature probes
(`src/main.rs:1142`+) — it mirrors the C backend's conditional module
inclusion. If you add a runtime module, both inclusion paths must learn about
it. Hot-reload is C-only and silently falls back to the C backend even under
`--backend=llvm` (`src/main.rs:728`).

## The semantic passes — real numbers

The internals `README.md` pipeline diagram is **stale**: it lists a
"Pass 4.5 `provenance.rs` — String provenance inference." **There is no
provenance pass and no `provenance.rs` in `src/semantic/`.** String
provenance/view-vs-owned is decided in the copy-on-write machinery during GIR
lowering and the safety checker, not as a numbered semantic pass (see Chapters
10 and 11). The authoritative pass order is the body of
`analyze_with_source_dir` in `src/semantic/mod.rs:96`. The fractional pass
numbers below are the comments *in that function*, in execution order:

| Pass | Name (timing key)            | Module                               | What it does                                                                 | Source |
|------|------------------------------|--------------------------------------|-----------------------------------------------------------------------------|--------|
| 0    | `meta_consts`                | `semantic/meta.rs`                   | Evaluate & substitute `meta` constants (`meta if`/`meta for`/`meta const`)   | `mod.rs:108` |
| 0.5  | `expand_derives`             | `semantic/derive.rs`                 | Expand `@derive(...)` attributes into `equip` blocks                          | `mod.rs:113` |
| —    | `validate_directives`        | inline + `validate_attributes`       | Validate `directive`s and item attributes; validate `suite setup/teardown`   | `mod.rs:118`, `:202` |
| 1    | `collect_top_level`          | `semantic/resolve.rs`                | Collect top-level definitions into scopes/types                              | `mod.rs:235` |
| 1.5  | `rewrite_import_aliases`     | `semantic/rewrite.rs`                | Physically rename `from X import Y as Z` → `Y` in the AST                     | `mod.rs:244` |
| 2    | `resolve_bodies`             | `semantic/resolve.rs`                | Resolve names in all bodies; build the `ResolutionMap`                        | `mod.rs:249` |
| 2.5  | `rewrite_struct_calls`       | `semantic/rewrite.rs`                | `Call{callee:Ident("Foo")}` → `StructLiteral` once structs are known         | `mod.rs:258` |
| 2.6  | `apply_collect_target_rewrites` | `semantic/typecheck.rs`           | LHS-type-driven `.collect()` target selection                                | `mod.rs:266` |
| 3    | `build_trait_registry`       | `semantic/traits.rs`                 | Build the trait/impl registry; validate impls                                | `mod.rs:271` |
| 3.5  | `validate_derive_field_traits` | `semantic/derive.rs`               | Check `@derive`d field types satisfy the trait's requirements                 | `mod.rs:276` |
| —    | `populate_def_field_types`   | `populate_def_field_types` (mod.rs)  | Fill `field_types`/`variant_field_types` on `DefInfo` before typecheck        | `mod.rs:281`, defn `:397` |
| 3.6  | `cycle_check`                | `semantic/cycle_check.rs`            | Detect unbounded recursive types before typecheck                            | `mod.rs:286` |
| 4    | `typecheck_module`           | `semantic/typecheck.rs`              | Type inference, exhaustiveness, method resolution; infers method/call targs  | `mod.rs:291` |
| 4.5  | `apply_inferred_targs`       | `semantic/typecheck.rs`              | Write typecheck-inferred method/call generic args back into the AST          | `mod.rs:308` |
| 4.6  | `lint_suggest_throws`        | `semantic/lint_suggest_throws.rs`    | Lint: flag `Result`-returning fns with manual match-rethrow shapes           | `mod.rs:324` |
| 5    | `safety_check_module`        | `semantic/safety/`                   | Ownership, lifetimes, concurrency (two sub-passes 5a/5b); CFA for `shared`   | `mod.rs:337` |

The product is `AnalysisResult` (`src/semantic/mod.rs:43`) — scopes, types, the
trait registry, the resolution map, struct/enum field info, function info,
per-expression types, method resolutions, `shared`-binding strategies, inferred
purity, borrow dependencies, and (if enabled) `pass_times`. Each pass is wrapped
in `time_pass` (`src/semantic/mod.rs:76`) so `gg profile` can surface the
dominant sub-pass.

Two ordering facts worth internalizing because they bite people:
`lint_suggest_throws` (4.6) runs after typecheck because it consumes
`expr_types`, and its warnings are appended to the safety pass's warnings so
they flow through one reporting path (`src/semantic/mod.rs:359`).
`populate_def_field_types` runs between 3.5 and 3.6 specifically so `FieldAccess`
typecheck returns the real field type rather than `error_id` — the comment at
`src/semantic/mod.rs:381` records the self-host bug that motivated it.

## The `gg` subcommands

Dispatch is a hand-rolled `match` on `args[1]` in `main()` (`src/main.rs:2267`).
There is no clap/argparse layer — flags are scanned out of `args` with helpers
like `parse_features`, `parse_scheduler`, `parse_clone_modes`
(`src/main.rs:332`, `:313`, `:367`). Positional filename detection skips
flag-with-value pairs (`src/main.rs:2536`).

Three early shorthands run before the `match`:
- **No args** → interactive TUI (`run_tui`, `src/main.rs:2271`).
- **`gg --version` / `-V`, `gg --help` / `-h`** (`src/main.rs:2277`, `:2283`).
- **`gg script.gg`** (an arg ending in `.gg`) → treated as `gg run script.gg`:
  build to a tempdir, exec, propagate the exit code (`src/main.rs:2326`).

Package and report commands are handled before the main `match` as well: `report`
(`src/main.rs:2383`), `init` (`:2429`), `new` (`:2435`), `add` (`:2446`),
`remove` (`:2457`). The remaining commands go
through the `match command.as_str()` at `src/main.rs:2711`.

### `lex`
Run the lexer and print each `Spanned<Token>` as `[start..end] Debug`
(`src/main.rs:2712`). No parsing, no errors beyond what the lexer emits inline.

### `parse`
Parse to an `ast::Module` and pretty-print it (`{module:#?}`). Parse errors are
reported and exit non-zero; parse *warnings* (e.g. deprecated syntax) are printed
but non-fatal (`src/main.rs:2721`).

### `check`
The frontend without codegen: parse → `load_imports` → `analyze_with_source_dir`,
then report errors/warnings (`src/main.rs:2743`). Prints `OK: no semantic errors`
on success, exits non-zero on error. This is the command that runs the full
Pass 0–5 chain but stops before GIR. `--show-borrows` adds a borrow-inference
summary (`print_borrow_summary`, `src/main.rs:274`); `--warn-const` enables the
const-warning safety sub-check (threaded into `analyze` as `warn_const`).

### `build`
Compile to a native artifact via `try_build_ir` (`src/main.rs:2791`). Two
sub-modes: `--shared` builds a `.dylib`/`.so` (`src/main.rs:2793`), the normal
path produces an executable (and handles hot-reload's two-phase host+guest split
internally, `src/main.rs:810`). On success prints `Built: <path>` unless an
`--emit-*` flag short-circuited to a text dump. Build-shaping flags read here
include `--strip-asserts`/`--no-strip-asserts`,
`--trace`/`--no-trace`, `--hot-reload`, `--sanitize`, `--backend=<c-lir|llvm>`,
`--target=<native|freestanding[-arch]>`, `--feature`, `--scheduler=`, and the
`--clones[=…]` diagnostics. The `freestanding` target cross-compiles a UEFI PE
application with clang+lld and stages an ESP directory (`src/main.rs:914`).

Not every build-shaping flag is meaningful on every backend, and the driver
resolves that by **rejecting the combination, never by quietly building
something else**. `--shared`, `--target=freestanding…` and `--clones=stats` are
C-backend-only and error out under `--backend=llvm`, each naming both halves of
the combination; `--sanitize` instead threads through to the LLVM pipeline,
because doing so was cheap. Implementation cost is the whole discriminator —
the principle is uniform, and it is the same lower-or-reject rule the
`--backend=<unknown>` check applies one level up. `add_sanitize_flags` is the
single place the sanitizer flag set is spelled, so a newly-added compile or
link command either routes through it or is the next silent hole.

### `run`
Build to a tempdir, exec the binary, propagate its exit code (`src/main.rs:2897`).
Positional args after the filename are forwarded to the program
(`src/main.rs:2926`); `gg`-level flags and flag-value pairs are filtered out
first. The tempdir (and thus the generated `.c`/binary) is cleaned up on drop.

### `test`
The richest command (`src/main.rs:2956`). It builds with `test_mode: true` in
`LoweringOptions`, then runs the compiled test binary, which contains a runtime
test runner. Supports `--tag`/`--exclude-tag`/`--filter` selection,
`--bench`, `--timeout`, `--parallel N` (spawns N worker processes with
`GORGET_PARALLEL_{ID,TOTAL}` env vars and merges their result files,
`src/main.rs:3183`), `--failed-only`/`--failed-first` (persisted in
`.gorget/<stem>.test-results.json`, `src/main.rs:3139`), `--nocapture`,
`--report html` (implies `--trace`, then renders an HTML report from the trace),
and a `--snapshot <save|diff|list|show|delete>` family (`src/main.rs:3038`).
Pointing `gg test` at a **directory** triggers discovery mode: recursively find
`.gg` files containing `test "`/`bench "` blocks, run each in a child `gg test`,
and aggregate the pass/fail/skip summary (`src/main.rs:2567`, discovery at
`:3700`).

### `fmt`
Format source via `gorget::formatter::format_source` (`src/main.rs:3279`).
Default prints to stdout; `--in-place`/`-i` rewrites the file; `--check`/`-c`
exits non-zero if the file isn't already formatted (no write).

### `profile`
`gg profile <file>` runs the full pipeline with per-phase `Instant` timers and
emits structured JSON to stdout: per-phase `duration_ms`, semantic/GIR-lower
sub-pass breakdowns, totals (`frontend_ms`/`backend_ms`/`total_ms`), peak RSS
(Linux `/proc/self/status` `VmHWM`, `src/main.rs:1584`), and opt-stats counters
(`try_profile`, `src/main.rs:1378`). `gg profile --compare <base.json>
<cur.json>` diffs two profile files with delta/factor columns and flags
regressions >1.1× / >1.5× (`compare_profiles`, `src/main.rs:1641`). Note `profile`
re-implements the pipeline rather than calling `try_build_ir`, so if you change
the build pipeline, keep `try_profile` in sync.

### `report`
`gg report <file.trace.jsonl> [--output <path>]` renders a trace file (produced
by a `--trace`/`directive trace` build) into an HTML report via
`gorget::report::generate_html_report` (`src/main.rs:2383`).

### Package commands: `init` / `new` / `add` / `remove`
Thin wrappers over `src/manifest.rs` / `src/resolver` / `src/lockfile.rs`:
- `init` writes `gorget.toml`, a starter `main.gg`, and a `.gitignore` in the
  cwd (`cmd_init`, `src/main.rs:2089`).
- `new <name>` creates the directory, `cd`s in, and runs `init`
  (`cmd_new`, `src/main.rs:2136`).
- `add <name> --git <url> [--tag|--branch] | --path <dir>` edits the manifest and
  re-resolves the lockfile (`cmd_add`, `src/main.rs:2160`).
- `remove <name>` drops a dependency and re-resolves (`cmd_remove`,
  `src/main.rs:2235`).

Dependency resolution for any build is bootstrapped by `resolve_deps_for_file`
(`src/main.rs:96`): walk up for `gorget.toml`, resolve, and build the
`name → path` map that the loader consults. Covered in Chapter 22.

## The REPL / TUI

`gg` with no arguments starts an interactive TUI (`run_tui`, `src/main.rs:1789`).
It is a *whole-program accumulator*, not a true expression REPL: it keeps two
buffers — top-level `definitions` and `main()`-body `statements` — and
classifies each entry with `is_definition_line` (`src/main.rs:1708`). On `/run`
it synthesizes a complete `.gg` source via `generate_tui_source`
(`src/main.rs:1755`) — wrapping loose statements in a generated `void main():`
unless the user defined their own `main` — writes it to a tempfile, compiles it
with `try_build_ir`, and execs the result, streaming stdout/stderr back. Slash
commands: `/run`, `/check`, `/show`, `/reset`, `/help`, `/quit`|`/exit`
(`src/main.rs:1810`+). Multi-line blocks are read by detecting a trailing `:`
and consuming indented continuation lines until a blank or dedented line.

The line editor itself is `src/tui.rs` — a small raw-mode reader built on
`crossterm`. `read_line(prompt, show_menu)` (`src/tui.rs:310`) drives a
`LineEditor` with cursor movement, a slash-command completion menu rendered in a
box (`COMMANDS`, `src/tui.rs:21`; `menu_lines`, `:105`), and `Ctrl-D`/`Ctrl-C`
handling. Raw mode is restored via an RAII `RawModeGuard` (`src/tui.rs:46`), and
if raw mode can't be enabled it falls back to a cooked-mode `stdin().read_line`
(`src/tui.rs:314`).

## Where to go next

- The frontend boxes: [Chapter 3 (lexer)](03-lexer.md),
  [Chapter 4 (parser/AST)](04-parser-ast.md),
  [Chapter 5 (formatter)](05-formatter.md).
- The semantic passes in order:
  [6 (meta/derive)](06-meta-derive.md),
  [7 (resolution)](07-name-resolution.md),
  [8 (traits)](08-traits.md),
  [9 (typecheck)](09-type-checking.md),
  [10 (safety)](10-ownership-safety.md), with
  [11 (copy-on-write)](11-copy-on-write.md) covering the view/owned decision the
  stale README mislabeled "Pass 4.5 provenance."
- The lowering chain: [12 (GIR)](12-gir-lowering.md),
  [14 (LIR/SSA)](14-lir-ssa.md), [16 (BIR)](16-bir.md),
  [17 (C backend)](17-c-backend.md), [19 (LLVM backend)](19-llvm-backend.md).
- The non-codegen consumers: [22 (modules & packages)](22-modules-packages.md).
