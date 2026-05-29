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

## Typed shape (minimal) — REVISED per plan-review reservation 1
```
enum BuiltinRetKind: BrkVoid/BrkInt/BrkBool/BrkF64/BrkU8/BrkString/BrkArray/BrkSelf/BrkElem/BrkOptionElem/BrkOptionRefElem/BrkInfer
struct BuiltinMethodDecl:
    String name
    bool returns_view
    Vector[int] owning_arg_positions   # NOT a scalar bool — see below
    BuiltinRetKind ret_kind
struct BuiltinMethodEntry: Vector[MatchKind] match_on; CollectionKind collection_kind; Vector[BuiltinMethodDecl] methods
```
**Reservation-1 fix (CORRECTNESS):** `is_owning_mutator_arg(kind, mname, margs_idx)` (`lower.gg:526-553`) returns a per-`(kind, method, INDEX)` truth — Dict `put`/`set` own BOTH idx 0+1; Vector `set`/`insert` own ONLY idx 1 (idx 0 is the index arg); Vector `push`/Set `add` own idx 0. A scalar `bool is_owning_mutator` CANNOT encode this → site #2 would retire nothing. Therefore the decl carries **`Vector[int] owning_arg_positions`** (the owning argument indices for that method). Since the table is keyed per-family (`BuiltinMethodEntry.collection_kind`), the positions are naturally per-`(kind, method)`. Site #2's consumer becomes: `builtin_method_decl(recv_type_name, mname)` → check if `margs_idx in d.owning_arg_positions`. This RETIRES the name-set AND the per-kind index `match` (the index truth is now data, not code) — a fuller retirement than the original "keep the kind-match" framing. Cross-check the positions row-by-row against the current `is_owning_mutator_arg` body AND Rust's `consuming_positions_by_name` (`methods.rs:1851-1861`) + its `ParamABI::ByPtr` filter (`:1877-1888`).

Self-host can't port Rust's `fn(&Args,&Ctx)->TypeId` closures → use a `BuiltinRetKind` enum tag + small dispatcher instead. Accessor: `Option[BuiltinMethodDecl] builtin_method_decl(String type_name, String mname)` near `resource_meta_for`.

## ⚠ Pre-execution re-pin (plan-review reservation 2)
This plan was scoped at tip `aaaaec57`; HEAD is now `b80f9e2f` and NEXT BLOCKER #5 (`6f74dd3e`) already added `byte_slice`/`substring`/`char_at` to BOTH the site-#1 list (`~lower.gg:476`) AND the site-#3 list (`~:3452`). **Before transcribing, the execution agent MUST re-pin to current HEAD and re-verify the actual row sets** at `lower.gg:475-486` (is_string_view_method), `:526-553` (is_owning_mutator_arg), `:3452-3471` (infer_method_return_type String rows) — the "faithful row-by-row transcription" is the #1 risk and its source moved.

## ⚠ Loader test (plan-review note)
`resources_load_clean` in `src/ir/resources.rs:~434` asserts EXACT table counts (currently RESOURCES=31, RUNTIME_FNS=299). Adding `BUILTIN_METHODS` → the walker counts a new table → update this test's assertions in the step-1 (schema+table land) commit.

## ⚠ Self-host-resource-model constraints (`docs/internals/self-host-resource-model.md` — read it too)
The self-host side is the PRIMARY long-term consumer; that doc adds binding constraints:
- **§3.2 — real Gorget enums, NEVER int-coded.** `BuiltinRetKind` as an enum is correct; do NOT int-code any categorical axis (semantic-state-in-a-primitive-pun = Rule 2 violation). `Vector[int] owning_arg_positions` is fine — those are genuine integer indices, not encoded semantics.
- **§3.4.1 (closed 2026-05-10) — site #3 has LOAD-BEARING collection-getter Option rows. HARD CONSTRAINT.** `infer_method_return_type`'s `get/pop/unwrap/first/last/remove` arms were deliberately made to return `Option__V` (elem-typed) to feed the Tier-1 lift's `slot.enum_kind == EK_OPTION` discriminator (`lir_lower.gg` `emit_void_ptr_option_wrap`). **The BuiltinMethodDecl port must leave those collection-getter Option-return rows EXACTLY ALONE — only the String-VIEW return rows (`slice`/`substring`/`trim`/… → GorgetString) retire to `ret_kind`.** Touching the getter-Option rows re-breaks the lift. This is the sharp version of "don't over-port #3": getter-Option rows are not "fuzzy heuristics to leave as BrkInfer" — they are correct, load-bearing, and out of scope entirely.
- **§3.4 — fix Gorget, not work around.** If the table emit (struct-with-many-enum-fields read in hot lowering paths; `Option[...]` field reads) trips a self-host codegen bug, file it + fix `src/` — do NOT ship a `tests/fixtures/` workaround (per the override, a Rust-correctness bug takes precedence anyway).
- **§3.3 item 4 — lint ratchet.** `tests/lints.rs` has a `MANGLED_PREFIXES` budget scanning self-host name-prefix dispatches (type-name `X__` prefixes). The 3 retired lists are method-NAME matches (may or may not be in that budget) — check whether retiring them lowers the ratchet count + update the budget if so.

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
