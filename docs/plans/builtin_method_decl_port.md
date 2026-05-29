# BuiltinMethodDecl Port — retire the 3 self-host name-match lists

> **Status (2026-05-29):** SCOPED plan, pending fresh review-before-execution. Produced by a Plan agent; verified against gorget-1 tip `aaaaec57`. Execution is a future round (~300 LOC, 4-5 gated commits). This is the "separate BuiltinMethodDecl-port effort" the live `self_host_namematch_retirement.md` plan recommended deferring to.

## Goal
Retire the three parallel name-match lists in `tests/fixtures/self_host_lowerer/lower.gg` (the only dir carrying them) by introducing a typed per-method declaration table, mirroring Rust's `BuiltinMethodDecl` (`src/ir/lowering/builtins.rs:59-86`). Closes the CLAUDE.md "No name matching" SHIP-GATE item.

The three consumers (current lines at `aaaaec57`):
1. `is_string_view_method` — def `lower.gg:475-486`, call `:4980` → `LoView` result-tag.
2. `is_owning_mutator_arg` — def `lower.gg:526-553`, call `:4907` → `CkCallArgOwning` (OpMove/OpClone).
3. `infer_method_return_type` String-returning list — `:3452` (fn spans `:3379-3540`), call `:4939` → builtin method return typing (was the NEXT BLOCKER #5/#6 fault surface).

## ✅ DESIGN DECISION (user, 2026-05-29): OPTION (A) — shared schema home, via the SHIPPED unified-resource-model pipeline.
The table lives in `compiler/data/schema.gg` + a static table in `compiler/data/resources.gg`. **This is NOT greenfield — it extends the already-shipped unified-resource-model infrastructure** (authoritative doc: `docs/internals/unified-resource-model.md` §3.6/§9.2/§13; landed 2026-05-19/20, TODO items 3-8 + 7c). Follow that pipeline EXACTLY:

- **`resources.toml` is DEAD** — the original TOML/build.rs plan was rejected 2026-05-19 (TODO:938). Canonical source is idiomatic Gorget `.gg`, baked into Rust via `include_str!` (`src/compiler_data.rs`), parsed at compiler-runtime by the compiler's OWN parser. Do NOT reintroduce TOML/codegen.
- **Existing pattern (mirror it):** `RESOURCES` + `RUNTIME_FNS` are two static tables in `resources.gg` parsed by the AST-walker in **`src/ir/resources.rs`** (`OnceLock`-cached `table()` accessor; walks `Item::StaticDecl`). The new `BUILTIN_METHODS` is a THIRD such table → extend `resources.rs` to walk it, mirror the new types in **`src/ir/resource_schema.rs`**, and add the self-host import/consumer (the self-host already does `from compiler.data.schema import ...` + reads via a `lookup_*`/`build_*` accessor like `build_resource_metadata` in `lir_lower.gg`).
- **Additive-change precedent = item 7c (2026-05-20, `c_typedef_name`):** purely-additive field, all existing rows leave it default/None, schema + Rust mirror + self-host constructor calls updated together, SCHEMA_VERSION bumped. The BuiltinMethodDecl table follows this exact playbook.
- **SCHEMA_VERSION:** bump 1→2 in `resources.gg` AND `SCHEMA_VERSION_EXPECTED` in `resources.rs` (the loader panics on mismatch) — schema + mirror + walker + bump in ONE atomic commit.

Reference-grade single-source-of-truth. Option (B) self-host-only is NOT taken. **The plan-review + execution MUST read `docs/internals/unified-resource-model.md` + study `src/ir/resources.rs` + the `c_typedef_name` precedent commits before writing code** — this is a well-trodden path, not a new design. (Original fork rationale below retained for context.)

## ⚠ The original design fork (RESOLVED → A above; retained for context)
**Where the table lives:**
- **(A) Shared home** — new `BuiltinMethodDecl`/`BuiltinRetKind`/`BuiltinMethodEntry` types in `compiler/data/schema.gg` + static `BUILTIN_METHODS` table in `compiler/data/resources.gg`, mirroring the existing `ResourceMetadata`/`RESOURCES` pattern. **Requires SCHEMA_VERSION bump (1→2) + an atomic Rust mirror update in `src/ir/resource_schema.rs`.** Reference-grade (single source of truth, Rust-mirrored), but a cross-language coordinate-land with bootstrap-fixed-point risk.
- **(B) Self-host-only module** — a new `tests/fixtures/self_host_lowerer/builtin_methods.gg` (or appended to `gir.gg`). Sidesteps the SCHEMA_VERSION bump entirely. The live plan's litmus weakness for self-host-only tables ("single-consumer = rename") does NOT apply here: THREE consumers justify the typed home.
- **Plan-agent recommendation:** prefer (A) for parity with Rust's single-source philosophy; fall back to (B) if the coordinate-land proves too heavy. **Surface this to the user/reviewer before executing** — it's a SCHEMA_VERSION + Rust-mirror commitment.

## Typed shape (minimal)
```
enum BuiltinRetKind: BrkVoid/BrkInt/BrkBool/BrkF64/BrkU8/BrkString/BrkArray/BrkSelf/BrkElem/BrkOptionElem/BrkOptionRefElem/BrkInfer
struct BuiltinMethodDecl: String name; bool returns_view; bool is_owning_mutator; BuiltinRetKind ret_kind
struct BuiltinMethodEntry: Vector[MatchKind] match_on; CollectionKind collection_kind; Vector[BuiltinMethodDecl] methods
```
Self-host can't port Rust's `fn(&Args,&Ctx)->TypeId` closures → use a `BuiltinRetKind` enum tag + small dispatcher instead. Accessor: `Option[BuiltinMethodDecl] builtin_method_decl(String type_name, String mname)` near `resource_meta_for`.

## Consumer rewrites (scope-bounded — do NOT over-port)
- **#1 view tag (`:4980`):** read `d.returns_view`; delete `is_string_view_method`. The `recv_is_string` gate is subsumed (non-String recv won't match the String entry).
- **#2 owning-mutator (`:4907`):** read `d.is_owning_mutator` for the method-name SET, but **KEEP the `CollectionKind` index `match`** (set=idx-1 on Vector, idx-0/1 on Dict — the self-host's strict-improvement-over-Rust typed disambiguation). Only the residual name-set retires.
- **#3 return-type (`:4939`):** source ONLY the String-returning rows (`:3452-3471`) + obvious typed rows (void/int/bool/u8) from `ret_kind`. **Leave receiver-aware (`remove` Vector-vs-Dict) + aggregate (`sum`/`avg`→I64) rows as `BrkInfer` fall-through.** Success grep: `grep -n '"slice"\|"byte_slice"\|"substring"\|"char_at"' lower.gg` → zero. Do NOT make `infer_method_return_type` fully table-driven (drifts output on fuzzy rows).

## Step plan (ONE chain, 4-5 sequential commits, output-neutral per step)
1. **Land table + accessor, no consumers** (additive data → must be a no-op). Gate: bootstrap + fixed_point + lowerer_comparison green.
2. **Migrate #1** (lowest risk, single bool). Gate: lowerer_comparison byte-identical GIR + fixed_point green.
3. **Migrate #2** (keep kind-match; watch `extend`/`push_back`/`send` rows). Gate: same.
4. **Migrate String rows of #3.** Gate: same + the success grep.
5. **Sweep:** `grep is_string_view_method\|is_owning_mutator_arg lower.gg` → zero; update TODO + namematch plan; close the CLAUDE.md item.

## Top risks
1. **Output drift / fixed-point break** — the decl table must be a faithful TRANSCRIPTION of the current 3 lists, verified row-by-row AND cross-checked against Rust's `GORGET_STRING_VIEW` (`builtins.rs:692-716`) since self-host and Rust must agree (the #5 fault surface). Migrate one consumer per commit for clean bisection.
2. **SCHEMA_VERSION coordinate-land** (option A only) — schema + Rust mirror + bump must be ONE atomic commit, or take option B.
3. **Over-porting `infer_method_return_type`** — port only String/typed rows; fuzzy rows stay `BrkInfer`.

## Validation harness
Per step: `lowerer_comparison` (output-neutrality, byte-identical GIR), `self_host_bootstrap` + `self_host_bootstrap_fixed_point` (self-compilation), `cargo test --lib --release` floor.

## Critical files
- `tests/fixtures/self_host_lowerer/lower.gg` (sites `:4980`/`:4907`/`:4939`, helpers `:475`/`:526`, fn `:3379-3540`)
- `compiler/data/schema.gg` (new types; SCHEMA_VERSION contract `:7`) + `compiler/data/resources.gg` (new table; `SCHEMA_VERSION` `:36`) — option A
- `src/ir/lowering/builtins.rs` (Rust reference to transcribe; String view rows `:692-716`)
- `src/ir/resource_schema.rs` (mandatory Rust mirror — option A only)
