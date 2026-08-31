# Appendix A — Subsystem to file map

This appendix is a navigational index for the `gg` compiler crate. It maps every
top-level directory and load-bearing file under `src/` to the subsystem it
implements, with an approximate line count (`wc -l`, Rust only). It exists so
that a contributor who reads a stack trace, a `// see <subsystem>` comment, or a
chapter cross-reference can jump to the right file without grepping the whole
tree. The pipeline is `.gg` source → lexer → parser → semantic analysis → GIR
lowering → LIR → BIR → backend (C or LLVM), and the directory layout follows it
almost stage-for-stage.

All LOC figures below are re-derived from the current working tree. The Rust
sources total ~169k lines (`find src -name '*.rs' | xargs wc -l`); the bundled C
runtime/vendored amalgamations are far larger and counted separately in the
[Backend](#inside-srcbackend) section. Numbers are rounded and **will drift** —
treat them as relative weights, not contracts, and re-run `wc -l` if you need an
exact figure.

## How to read this table

LOC is the sum of `.rs` files in the directory (recursively) unless a single
file is called out. "Pipeline stage" follows the order above. Files marked
*entry point* are where a stage's public API lives (the `mod.rs` or the
`check_module` / `lower_module`-style top-level function).

## Top-level directories

| Path | LOC | Pipeline stage | Responsibility |
|------|-----|----------------|----------------|
| `src/lexer/` | ~2,400 | Lex | Logos-based tokenizer with indentation tracking (header at `src/lexer/mod.rs:9`). |
| `src/parser/` | ~8,900 | Parse | Recursive-descent parser producing the AST defined in `src/parser/ast.rs`. Entry: `src/parser/mod.rs:1`. |
| `src/semantic/` | ~33,900 | Semantic analysis | Name resolution, type checking, trait registry, derive expansion, meta evaluation, borrow/safety checking. |
| `src/ir/` | ~54,300 | GIR + lowering | The GIR (mid-level IR; "G" for Gorget) plus AST→GIR lowering (monomorphization, drop insertion, closures) and GIR transforms. Largest subsystem. |
| `src/lir/` | ~22,000 | LIR | SSA-form low-level IR and the GIR→LIR lowering pass. Sole production lowering target. |
| `src/bir/` | ~6,300 | BIR | Backend IR — a typed newtype over `LirModule` guaranteeing canonical high-level ops are expanded to primitives before a backend sees them. |
| `src/backend/` | ~17,200 (Rust) | Codegen | C and LLVM backends. The C runtime is no longer embedded as a Rust string — it lives in ~62 external `.c` files under `c/runtime/` (~14,670 LOC, pulled in via `include_str!`) and is counted separately, as are the vendored C amalgamations (~282k lines; see below). |
| `src/formatter/` | ~9,300 | Tooling | Source formatter (`gg fmt`), Wadler-style pretty-printer. |

## Top-level files

| File | LOC | Responsibility |
|------|-----|----------------|
| `src/main.rs` | ~3,750 | CLI entry point. Dispatches subcommands `lex` / `parse` / `check` / `build` / `run` / `fmt` and the package commands `init` / `new` / `add` / `remove` (dispatch table around `src/main.rs:2712`; `cmd_*` fns from `src/main.rs:2089`). Drives the whole pipeline. |
| `src/stdlib.rs` | ~1,450 | Built-in module system covering `std.*` (core) and `xtd.*` (batteries). Resolves the file-based `.gg` stdlib modules under `lib/std/` and `lib/xtd/` (`src/stdlib.rs:1`). |
| `src/loader.rs` | ~1,450 | Import resolution and AST merging — reads source, parses, recursively resolves imports, merges into the main module (`src/loader.rs:1`). |
| `src/report.rs` | ~1,350 | Test/trace report generation (`TraceEvent` model at `src/report.rs:8`). |
| `src/resolver.rs` | ~420 | **Package dependency resolver.** Resolves manifest dependencies, fetches git deps, detects cycles/version conflicts, produces the lockfile (`ResolveError` at `src/resolver.rs:15`). See disambiguation below — this is **not** name resolution. |
| `src/errors.rs` | ~390 | Lex/parse error types and `codespan_reporting`-based diagnostic rendering (`src/errors.rs:1`). |
| `src/tui.rs` | ~375 | Terminal UI helpers (crossterm-based) for interactive output (`src/tui.rs:1`). |
| `src/span.rs` | ~220 | `Span` (byte-offset range) and `Spanned<T>` — the source-location primitives threaded through every layer (`src/span.rs:1`). |
| `src/lockfile.rs` | ~200 | Lockfile parse/serialize for the package manager. |
| `src/manifest.rs` | ~190 | Package manifest (`gorget.toml`-style) parsing and `DepSpec` model. |
| `src/intern.rs` | ~130 | Global string interner; `Symbol` (u32 handle) for fast identifier compare/hash (`src/intern.rs:1`). |
| `src/compiler_data.rs` | ~90 | Embeds the canonical resource/runtime-function table source (`compiler/data/{schema,resources}.gg`) into the binary via `include_str!` (`src/compiler_data.rs:1`). |
| `src/lib.rs` | ~20 | Crate root — module declarations only. |

## The two "resolvers" (disambiguation)

The codebase has two files whose names both say "resolve," and they are
**unrelated subsystems**:

- **`src/resolver.rs` (~420 LOC) — the package dependency resolver.** It consumes
  `manifest.rs` + `lockfile.rs`, fetches git dependencies, detects dependency
  cycles and version conflicts, and produces a resolved dependency graph. Its
  error type is `ResolveError` (`src/resolver.rs:15`) with variants like
  `CycleDetected` and `VersionConflict`. It runs *before* any source is compiled.
- **`src/semantic/resolve.rs` (~2,375 LOC) — name resolution.** It is part of the
  semantic-analysis stage: it walks the AST, builds scopes, binds identifiers to
  `DefId`s, and populates side tables like `StructFieldInfo` and
  `EnumVariantInfo` (`src/semantic/resolve.rs:1`). It runs *per-module* after
  parsing and feeds the type checker.

If a comment or chapter says "the resolver," check the context: dependency
graph → `src/resolver.rs`; identifier binding → `src/semantic/resolve.rs`.

## Inside `src/semantic/`

Name resolution, type checking, traits, and the borrow/safety pass. Entry point
is `src/semantic/mod.rs` (~455 LOC).

| File | LOC | Responsibility |
|------|-----|----------------|
| `typecheck.rs` | ~6,900 | Type inference and checking — the largest semantic file. |
| `meta.rs` | ~4,900 | Compile-time (`meta`) evaluation and meta-type expansion. |
| `safety/` | ~10,800 (dir) | Borrow/ownership/concurrency checking (the former monolithic `borrow.rs`, now 9 submodules; see below). |
| `traits.rs` | ~2,480 | Trait registry, impl resolution, vtable shape. |
| `resolve.rs` | ~2,375 | **Name resolution** (see disambiguation above). |
| `derive.rs` | ~1,790 | `@derive` attribute expansion into equip blocks. |
| `rewrite.rs` | ~1,180 | AST rewrites (e.g. `field_value` / `field_set` lowering of meta field access). |
| `scope.rs` | ~855 | Scope table, `DefKind`, `ScopeKind`. |
| `errors.rs` | ~815 | `SemanticError` / `SemanticErrorKind`. |
| `lint_suggest_throws.rs` | ~510 | Lint pass suggesting `throws` annotations. |
| `types.rs` | ~545 | `TypeTable` and the semantic-level `TypeId`. |
| `cycle_check.rs` | ~275 | Type/definition cycle detection. |
| `purity.rs`, `ids.rs` | ~110 total | Purity inference; `DefId`/`TypeId` newtypes. |

### `src/semantic/safety/` submodules

| File | LOC | Responsibility |
|------|-----|----------------|
| `tests.rs` | ~3,880 | Unit tests for the safety pass. |
| `check_stmt.rs` | ~1,580 | Statement/block/function walker, pattern handling. |
| `helpers.rs` | ~1,100 | Concurrency checks, call ownership/aliasing, spawn safety. |
| `check_expr.rs` | ~1,040 | Expression walker. |
| `return_borrows.rs` | ~750 | Pass 5a return-borrow analysis, closure visitors, alias map. |
| `validation.rs` | ~730 | Private-in-public, unused imports, purity inference. |
| `origins.rs` | ~730 | `BorrowOrigin` tracking, branch state save/restore. |
| `mod.rs` | ~670 | Core types, `BorrowChecker` struct, `check_module` entry point. |
| `type_utils.rs` | ~250 | `is_copy_type`, ref-type detection. |

## Inside `src/ir/` (GIR)

The GIR is the mid-level, post-monomorphization IR. `src/ir/lowering/` (~39,400
LOC) does AST→GIR; `src/ir/transforms/` (~4,830 LOC) runs GIR-level passes;
the remaining files define the IR itself. Module list at `src/ir/mod.rs:1`.

| File | LOC | Responsibility |
|------|-----|----------------|
| `lowering/exprs/mod.rs` | ~4,330 | Expression lowering core. |
| `lowering/mod.rs` | ~4,170 | Lowering entry point and module driver. |
| `transforms/optimize.rs` | ~3,820 | GIR optimization passes. |
| `validate.rs` | ~3,690 | GIR validator (invariant checks). |
| `lowering/stmts/mod.rs` | ~3,600 | Statement lowering core. |
| `lowering/exprs/methods.rs` | ~3,490 | Method-call lowering / dispatch. |
| `lowering/context.rs` | ~3,440 | Lowering context (locals, scopes, drop tracking). |
| `lowering/generics/mod.rs` | ~2,880 | Monomorphization / generic instantiation. |
| `lowering/exprs/calls.rs` | ~2,140 | Call lowering. |
| `lowering/functions.rs` | ~2,080 | Function-def lowering. |
| `lowering/traits.rs` | ~1,750 | Trait-method / vtable lowering. |
| `lowering/stmts/assigns.rs` | ~1,430 | Assignment + var-decl assign-mode lowering. |
| `lowering/types.rs` | ~1,380 | AST type → GIR type lowering. |
| `lowering/closures.rs` | ~1,260 | Closure capture / env lowering. |
| `lowering/builtins.rs` | ~1,140 | Builtin-call lowering and return-type tables. |
| `lowering/stmts/patterns.rs` | ~1,110 | Match / destructuring lowering. |
| `types.rs` | ~1,104 | GIR type table and GIR `TypeId` (distinct from semantic `TypeId`; `src/ir/types.rs:4`). |
| `lowering/stmts/for_loops.rs` | ~1,060 | `for` lowering. |
| `transforms/shared_async.rs` | ~1,010 | Shared/async (spawn, channels) transform. |
| `lowering/exprs/collections.rs` | ~925 | Collection-literal / index lowering. |
| `printer.rs` | ~880 | GIR pretty-printer. |
| `mod.rs` | ~855 | GIR module structures. |
| `lowering/liveness.rs` | ~790 | Liveness analysis (move/last-use), including the loop back-edge two-pass shared by all seven loop-shaped arms. |
| `lowering/drops.rs` | ~755 | Drop insertion. |
| `builder.rs` | ~735 | GIR builder helpers. |
| `lowering/exprs/spawn.rs` | ~730 | Spawn / task lowering. |
| `tag_ownership.rs` | ~650 | Ownership-tagging pass on GIR locals. |
| `resources.rs` | ~533 | Resource-table consumer. |
| `instructions.rs` | ~504 | GIR instruction set. |
| smaller files | — | `lowering/{exprs/operators,generics/substitute,exprs/type_reg,exprs/shared}.rs`, `resource_schema.rs` (~160), `abi.rs` (~50). |

## Inside `src/lir/` (LIR)

SSA-form low-level IR; all implicit operations (drop glue, vtable dispatch,
closures, coercions) are explicit (`src/lir/mod.rs:1`). `src/lir/lower/` does
GIR→LIR.

| File | LOC | Responsibility |
|------|-----|----------------|
| `lower/insts.rs` | ~4,360 | Per-instruction GIR→LIR lowering. |
| `optimize.rs` | ~2,245 | LIR optimization passes. |
| `mod.rs` | ~2,194 | LIR module/types, instruction set (`Inst`, `Term`, `LirType`). |
| `validate.rs` | ~2,026 | LIR validator. |
| `lower/mod.rs` | ~2,007 | LIR lowering driver / `FuncLowering`. |
| `lower/operands.rs` | ~1,588 | Operand/value lowering. |
| `types.rs` | ~1,094 | LIR type definitions. |
| `ssa.rs` | ~908 | SSA construction (block params = phi). |
| `drop_elab.rs` | ~873 | Drop elaboration (packed 2-bit slot-state dataflow). |
| `runtime.rs` | ~829 | **Typed runtime function table** — `RuntimeFn` enum, the single source of truth for runtime API at the IR level (`src/lir/runtime.rs:1`). |
| `lower/lifts.rs` | ~802 | Canonical-op lifting helpers. |
| `lower/types.rs` | ~752 | Type lowering incl. `c_sizeof_lir_type`. |
| `lower/drops.rs` | ~697 | Drop-recipe lowering. |
| `display.rs` | ~595 | LIR text dump. |
| `lower/calls.rs` | ~474 | Call lowering. |
| `split_edges.rs` | ~305 | Critical-edge splitting. |
| `integration.rs`, `queries.rs` | ~285 total | LIR pipeline integration *tests* (a `#[cfg(test)]` module, not glue code); LIR queries. |

## Inside `src/bir/` (BIR)

Backend IR. A newtype over `LirModule` that guarantees canonical high-level ops
have been expanded to primitives, so a backend can never receive an unlowered
module (`src/bir/mod.rs:1`).

| File | LOC | Responsibility |
|------|-----|----------------|
| `lower.rs` | ~4,058 | `lower_lir_to_bir` (`src/bir/lower.rs:41`) — expands canonical ops (`SizeOf`, `EnumInit`/`EnumCheck`/`EnumExtract`, `HofExpand` map/filter/fold/…) into primitive instruction sequences. *Note: the "Step 0 trivial passthrough" claim in the `mod.rs` header comment (`src/bir/mod.rs:20-23`, echoed at `src/bir/validate.rs:23`) is stale — the expansions are live (see the "Current canonical ops expanded" list at `src/bir/lower.rs:8`).* |
| `synth.rs` | ~1,981 | Module-level synthesis of specialized `LirFunction`s (e.g. `sort_by` family) so backends don't reinvent them per-type (`src/bir/synth.rs:1`). |
| `validate.rs`, `mod.rs` | ~215 total | `assert_primitives_only` validator; `BirModule` newtype + `BirError`. |

## Inside `src/backend/`

Codegen. The GIR→C backend was retired; all compilation goes through LIR
(`src/backend/c/mod.rs:1`). `src/backend/mod.rs:11` holds the shared
`map_stdlib_name` Gorget→C runtime-symbol table.

| File | LOC | Responsibility |
|------|-----|----------------|
| `c/c_runtime.rs` | ~243 | `include_str!` manifest that assembles the C runtime from the `.c` files in `c/runtime/` (`RUNTIME_PREAMBLE` at `src/backend/c/c_runtime.rs:2`). The runtime itself — strings, collections (`GorgetArray`/`GorgetMap`), allocator, async/channels, GL/Metal/SDL — is ~14,670 LOC across ~62 `.c` files in `c/runtime/`. |
| `llvm/mod.rs` | ~6,735 | LIR → LLVM IR (textual `.ll`) backend; near-1:1 since LIR is already SSA (`src/backend/llvm/mod.rs:1`). |
| `c_lir/mod.rs` | ~3,199 | LIR → C backend — thin 1:1 translation, no semantic decisions (`src/backend/c_lir/mod.rs:1`). |
| `c_lir/emit_types.rs` | ~2,931 | C type emission (struct layout, topo sort, union enums). |
| `c_lir/helpers.rs` | ~1,990 | Backend helpers (runtime-symbol mapping, cstr handling, printf fixups). |
| `c_lir/emit_call_extern.rs` | ~908 | Extern/runtime call emission. |
| `mod.rs` | ~559 | `Backend` trait + `map_stdlib_name`. |
| `c_lir/emit_printf.rs`, `c_lir/emit_hof.rs` | ~480 total | printf-format fixups; HOF emission. |
| `c/mod.rs` | ~8 | Module shim re-exporting `c_runtime` (GIR C backend removed); also hosts the runtime-extract test. |

**Vendored / non-Rust C** lives under `src/backend/c/` and is not part of the
Rust LOC: `sqlite3/sqlite3.c` (~260k), `sqlite3/sqlite3.h` (~13.6k),
`stb_image.h` (~8k), `sqlite3/gorget_sqlite.c` (~150). These ship inside the
binary alongside the generated runtime.

## Inside `src/lexer/`, `src/parser/`, `src/formatter/`

| File | LOC | Responsibility |
|------|-----|----------------|
| `lexer/mod.rs` | ~1,640 | Logos tokenizer + indentation/INDENT-DEDENT synthesis (`src/lexer/mod.rs:1`). |
| `lexer/token.rs` | ~760 | Token enum + keyword tables. |
| `parser/mod.rs` | ~2,120 | Parser driver / entry (`src/parser/mod.rs:1`). |
| `parser/expr.rs` | ~1,994 | Expression parsing. |
| `parser/ast.rs` | ~1,234 | The AST node definitions (consumed across the whole front end). |
| `parser/stmt.rs` | ~1,018 | Statement parsing. |
| `parser/tests.rs` | ~1,564 | Parser unit tests. |
| `parser/{visitor,pattern,types}.rs` | ~940 total | AST visitor; pattern parsing; type parsing. |
| `formatter/mod.rs` | ~8,200 | `gg fmt` formatter logic. |
| `formatter/doc.rs` | ~1,100 | Wadler-style document combinators. |

## Where to start reading

- **A new pipeline stage walk:** `src/main.rs` build path (`src/main.rs:2791`) →
  it threads source through lexer → parser → `semantic/mod.rs` → `ir/lowering/mod.rs`
  → `lir/lower/mod.rs` → `bir/lower.rs` → `backend/c_lir/mod.rs` or `backend/llvm/mod.rs`.
- **A diagnostic / span question:** `src/span.rs` then `src/errors.rs` /
  `src/semantic/errors.rs`.
- **"Which file owns this runtime symbol":** the typed table in
  `src/lir/runtime.rs` and the name map in `src/backend/mod.rs:11`; the actual C
  body in `src/backend/c/c_runtime.rs`.
