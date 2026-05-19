# TODO

## Profile snapshot 2026-05-17

Fresh `gg profile` snapshot taken on `gorget-1` HEAD (e052606e) after the post-2026-05-17 batch (validate_resource walks collapsed `4b529742`, semantic self-type index `e4e1f6a3`, GIR Liveness bitset `79499789`, propagate_copies FxHashMap `469d7942`, loader rayon `107ab8dc`, TypeTable primitive_ids `615a3d0b`, LIR cse/find_live_functions/eliminate_dead_globals FxHashMap `5c38ee14`, stack-traces v1 regression closed `01f91844`, LIR codegen perf rewrite `962ae144`). Profile output medianed over 5 runs per workload. Goal: identify the **next** dominant phase outside the known/handled zones (drop_elab, semantic name-index, safety borrow-state-reset, and the just-shipped batch).

**Workloads profiled** (release build, median-of-5):

| Workload                                            | total_ms | LIR insts | C lines |
|-----------------------------------------------------|---------:|----------:|--------:|
| `self_host_typechecker/driver.gg` (~12k aggregate)  |   152.0  |    75 262 | 193 988 |
| `self_host_lowerer/driver.gg` (~30k aggregate)      |   550.8  |   194 828 | 536 943 |
| `self_host_resolver/driver.gg` (subst. for arena)   |    75.3  |    37 241 |  99 212 |
| `self_host_parser/driver.gg` (subst. for gorget-js) |    68.9  |    36 857 |  94 987 |

**Note on substitutions:** `target/gorget-arena/src/main.gg` and `target/gorget-js/src/main.gg` were both located but failed `gg check` at the time this profile was taken. **Arena fixed 2026-05-17** (see DONE) — root cause was a compiler bug in `register_function_signature` where an equip method `bool file_exists(self, String)` clobbered the same-named `std.fs::file_exists(cstr)` extern's `type_id` because the name-based lookup found the extern at the root scope when the equip block wasn't on the scope stack. The fix swaps to span-based lookup; arena's `gg check` is now clean. **JS still fails** with 2 `expected String, found RuntimeException` errors in `eval.gg:4266/5185` — these are real bugs in gorget-js code (snag #2 commit `547a9abd` already noted them as "Real bugs that the truncate had been hiding — gorget-js will fix them"). Filed as a separate "gorget-js: fix eval.gg host_error msg type mismatch (snag #2 surface)" follow-up below. Substituted with `self_host_resolver/driver.gg` and `self_host_parser/driver.gg` per the user's fall-back instruction. Re-run the profile against `target/gorget-arena/src/main.gg` once a fresh snapshot is desired — it should now serve as a useful third profile target alongside the lowerer and typechecker.

**Lowerer dropped 798ms → 550.8ms (−31%) since 2026-05-16.** Per-phase deltas vs the prior snapshot (median):

| Phase             | 2026-05-16 | 2026-05-17 | Δ      | Wins explaining the drop |
|-------------------|-----------:|-----------:|-------:|---|
| `lir_optimize`    |     226.3  |     177.2  | −21.7% | `propagate_copies` 18.6→10.5, `cse` 5.0→2.7, `eliminate_dead_globals` and tail wins (`469d7942`, `5c38ee14`) |
| `gir_lower`       |     155.2  |     108.7  | −30.0% | now has per-pass timing (`3dfc9916`); no specific writer-side win, likely cache/codegen-of-rustc + smaller-input drift |
| `codegen` (c_lir) |     136.3  |      84.5  | −38.0% | LIR codegen perf rewrite (`962ae144`) |
| `semantic`        |      86.5  |      48.1  | −44.4% | `validate_resource_*` collapse (`4b529742`), self-type index (`e4e1f6a3`), primitive_ids (`615a3d0b`) |
| `load_imports`    |      67.1  |      24.8  | −63.0% | rayon parallelization (`107ab8dc`) |
| `lir_ssa`         |      44.2  |      37.0  | −16.3% | secondary effect of FxHashMap swaps; structural cost unchanged |
| `lir_lower`       |      30.9  |      27.3  | −11.7% | minor |
| `gir_optimize`    |      27.0  |      22.6  | −16.3% | GIR Liveness bitset (`79499789`) |

**Zero regressions.** Every phase moved down. The biggest absolute drop is in `codegen` (51.8ms, the `962ae144` win) and `semantic` (38.4ms, the batch).

**Phase breakdown on `self_host_lowerer/driver.gg` (550.8ms, median-of-5)** — sorted by absolute time, with KNOWN/HANDLED rows annotated:

| Phase                              | ms     | % total | Notes |
|------------------------------------|-------:|--------:|---|
| `lir_optimize` (total)             | 177.2  | 32.2%   | of which `drop_elaboration` 117.1 (KNOWN), `eliminate_dead_code` 11.2, `propagate_copies` 10.5, `post_elab_dce` 4.7, `fold_constants` 3.6, `eliminate_dead_functions` 3.0, `cse` 2.7, `merge_linear_blocks` 2.5, `simplify_algebraic` 2.3, `eliminate_dead_blocks` 1.3, `fold_constant_branches` 1.2, `eliminate_dead_globals` 1.1, rest <0.3ms |
| `gir_lower`                        | 108.7  | 19.7%   | of which `lower_functions` 61.0, `validate_consume_sites` 15.2, `lower_equip_methods` 9.0, `monomorphize` 4.9, `validate_module` 3.8, `flatten_and_manglings` 3.4, `tag_ownership_infer_fresh_owned` 1.5, `validate_resource_sites_all` 1.5 (post-collapse, KNOWN), `auto_register_externs` 1.4, `validate_drop_pre_rebind_and_null_to_opt` 1.2, rest <1ms |
| `codegen` (c_lir)                  |  84.5  | 15.3%   | C string assembly + serialization. Down 38% from prior snapshot. Still scales with C line count (537k lines). |
| `semantic` (total)                 |  48.1  |  8.7%   | of which `safety_check_module` 14.8 (KNOWN), `typecheck_module` 14.2, `meta_consts` 13.8, `safety::check_items_recursive` 13.0 (KNOWN), `resolve_bodies` 3.1, `collect_top_level` 0.8, `safety::infer_purity` 0.8, `safety::unused_imports` 0.7, rest <0.7ms |
| `lir_ssa`                          |  37.0  |  6.7%   | Critical-edge split + SSA construction (dominators, phi insertion, renaming) |
| `lir_lower`                        |  27.3  |  5.0%   | GIR → LIR translation |
| `load_imports`                     |  24.8  |  4.5%   | rayon-parallel file I/O + parse (`107ab8dc` already shipped) |
| `gir_optimize`                     |  22.6  |  4.1%   | dead_drop / nop_elim / dead_block / dead_store passes |
| `parse`                            |   0.6  |  0.1%   | entry-file lex+parse only (imports counted in `load_imports`) |

**Cross-workload phase distribution** (% of total per workload, helps spot per-workload vs. universal costs):

| Phase            | typechecker | lowerer | resolver | parser |
|------------------|------------:|--------:|---------:|-------:|
| `lir_optimize`   |       19.4% |   32.2% |    13.9% |  15.2% |
| `gir_lower`      |       22.1% |   19.7% |    25.0% |  23.4% |
| `codegen`        |       14.9% |   15.3% |    13.3% |  14.4% |
| `semantic`       |       11.7% |    8.7% |    14.1% |  13.2% |
| `lir_lower`      |        7.7% |    5.0% |     7.6% |   7.5% |
| `lir_ssa`        |        7.4% |    6.7% |     6.6% |   6.5% |
| `gir_optimize`   |        5.2% |    4.1% |     5.2% |   5.2% |
| `load_imports`   |        7.4% |    4.5% |    10.4% |  10.6% |

**Cross-workload observations:**

1. **`gir_lower` is universal-heavy** (~20-25% on every workload) — the most consistently large phase across all four. **This makes it the highest-leverage optimization target by hit rate.**
2. **`lir_optimize` scales worse-than-linearly with code size** — 32% on lowerer vs 14-19% on the others. Drop_elab dominates at the high end; the rest of `lir_optimize` is closer to flat.
3. **`load_imports` matters more on small targets** — 10% on resolver/parser, 4% on lowerer (rayon's already extracted the parallel slack on the big one). Future wins here are tail.
4. **`semantic` is consistent ~10-14%** across all workloads except lowerer (where it's diluted to 8.7%). Hot sub-passes are the same everywhere: typecheck/meta_consts/safety.
5. **`codegen` is ~15% everywhere** — flat % across sizes, confirms it scales linearly with output. The remaining 84ms on lowerer is the next-largest pure-IO phase; 56× win in `962ae144` was real but there's still tail.

**Top-5 cheap-win candidates (excluding drop_elab, semantic name-index, safety borrow-state, and the items shipped in the 2026-05-17 batch):**

1. **`gir_lower::lower_functions` — INSTRUMENTED + PRESCAN FIX SHIPPED 2026-05-18** (~65ms on lowerer baseline → ~55ms after; instrumentation pattern from `3dfc9916` extended one layer deeper). The non-generic per-AST-function lowering loop in `src/ir/lowering/mod.rs:1242-1257` now reports a breakdown across `lower_function::{setup, prescan, body, finalize}` plus body sub-passes `body::{meta_expand, lower_block}` and prescan sub-passes `prescan::{cow_unsafe, cow_after, name_use_counts, liveness}` in `gg profile` JSON. **Post-instrument breakdown on lowerer (median-of-5)**:

   | Sub-pass                                  | Before (ms) | After (ms) | Δ |
   |-------------------------------------------|------------:|-----------:|---:|
   | `lower_functions` (parent total)          |        65.0 |       55.0 | −15% |
   | `lower_function::body`                    |          ~50 |       45.8 | tail |
   | &nbsp;&nbsp;`body::lower_block`           |          ~46 |       41.1 | (the bulk) |
   | &nbsp;&nbsp;`body::meta_expand`           |          ~4 |        3.4 | (block.clone+meta walk) |
   | `lower_function::prescan`                 |        18.0 |        7.8 | **−57%** |
   | &nbsp;&nbsp;`prescan::liveness`           |         9.0 |        3.0 | **−66%** |
   | &nbsp;&nbsp;`prescan::cow_after`          |         7.3 |        3.0 | **−59%** |
   | &nbsp;&nbsp;`prescan::name_use_counts`    |         1.2 |        1.2 | flat |
   | &nbsp;&nbsp;`prescan::cow_unsafe`         |         0.5 |        0.5 | flat |
   | `lower_function::setup`                   |         2.3 |        1.1 | (coalesced) |
   | `lower_function::finalize`                |         0.2 |        0.2 | flat |
   | `gir_lower` (parent phase)                |       116.0 |      104.5 | −10% |
   | `total`                                   |       581.0 |      570.0 | −2% |

   **Fix**: `src/ir/lowering/liveness.rs` `FxHashSet<String>` → `FxHashSet<&'a str>` borrowed from AST identifier nodes (cheap clone, no per-entry allocation at branch save/restore points). `src/ir/lowering/functions.rs::compute_cow_reassigned_after` and its helpers `FxHashSet<String>` → `FxHashSet<Rc<str>>` with a per-function `String→Rc<str>` interner (the analysis result owns the map across function boundaries so a `&str`-slice approach would require lifetime plumbing through `FunctionState`; `Rc<str>` is the localized equivalent — clones are refcount bumps). `ctx.func_state.cow_reassigned_after` type updated to match; `is_source_mut_unsafe_at` queries use `set.contains(s as &str)` via `Rc<str>: Borrow<str>`. Typechecker `lower_functions` 12.3 → 11.3ms (−1ms / −8%); parser tiny. **The fix illustrates CLAUDE.md "Debugging heuristic — fix complexity as a signal of wrong layer" obliquely** — the writer site (`future.clone()` at every statement boundary plus union sites in if/match) was paying full per-String alloc cost; the cheap fix is changing the cell type to make clones cheap. The 9ms body-side speedup (from ~50ms → 45.8ms) is a downstream effect: body lowering reads `cow_reassigned_after` via `is_source_mut_unsafe_at` (called at every CoW-borrow site), and those calls used to allocate `format!("@mut:{}", path)` `String`s as map keys — they still do today (look at `is_source_mut_unsafe_at` for the lingering `format!` calls; switching to a reused-buffer or stack-buf `&str` lookup would shave the remaining 1-2ms).

   **Remaining cheap-win at this site (deferred)**:
   - ~~(i) drop the residual `format!("@mut:{}", path)` allocations in `is_source_mut_unsafe_at`~~ **SHIPPED 2026-05-18** (perf bundle, commit `5579bd66`; physically on disk via `982c853e` co-commit): single reused `String` buffer + `truncate(PREFIX.len())` + incremental `push_str` rebuild eliminates 1 alloc for the direct marker + N-1 allocs for the ancestor-prefix walk.
   - ~~(ii) `body::meta_expand` at 3.4ms is `block.clone()` + tree walk. The clone could be elided for blocks that have no `meta for` / `meta if` to expand — quick scan check before the clone. Expected: 2-3ms shave.~~ **SHIPPED 2026-05-18** (perf bundle, commit `5579bd66`): new `meta::block_has_delayed_meta(&Block)` recursive scanner gates the three `evaluate_delayed_meta_block` call sites. **Measured: meta_expand 3.4ms → 0.1ms across all 4 workloads (−97%).**

   [updated: 2026-05-18 — Step 1 + Step 2 + perf bundle (`5579bd66`: items (i) + (ii) + lower_if instrumentation + var_decl get_type_def coalesce) shipped]

   **Post-instrument-take-2 breakdown — `body::lower_block` per-statement-kind (2026-05-18 post-perf-bundle, median-of-5)**:

   | Sub-sub-pass (stmt kind)                  | Self time (ms) | % of body::lower_block |
   |-------------------------------------------|---------------:|-----------------------:|
   | `body::lower_block::stmt::var_decl`       |          12.2  | 28% |
   | `body::lower_block::stmt::if`             |          11.0  | 25% |
   | `body::lower_block::stmt::expr`           |          ~6.0  | 14% |
   | `body::lower_block::stmt::match`          |          ~4.0  |  9% |
   | `body::lower_block::stmt::return`         |          ~3.3  |  8% |
   | `body::lower_block::stmt::assign`         |          ~2.3  |  5% |
   | `body::lower_block::stmt::while`          |          ~1.4  |  3% |
   | `body::lower_block::stmt::for`            |          <0.4  | <1% |
   | rest (loop/break/continue/throw/assert/…) |          <0.3  | <1% |
   | **SUM (matches `body::lower_block` 43.3)**|        **~43** | **100%** |

   **`stmt::if` sub-sub-buckets (2026-05-18 perf bundle, median-of-5)** — the bundle's lower_if instrumentation drilled one layer deeper:

   | `if::<sub>`         | Self time (ms) | % of stmt::if |
   |---------------------|---------------:|--------------:|
   | `if::cond_eval`     |           4.06 | 37% |
   | `if::then_branch`   |           5.19 | 47% |
   | `if::elif_branches` |           1.47 | 13% |
   | `if::else_branch`   |           0.46 |  4% |
   | `if::phi_merge`     |           0.07 |  1% |
   | **SUM (matches `stmt::if` ~11.0)** |   **~11.25** | **100%** |

   **Findings on the `if` breakdown**: dominant sub-pass is `then_branch` (5.2ms / 47%) — save_locals + push_scope + emit_is_bindings + lower_block + pop_scope/restore + snapshot/restore, executed once per Stmt::If. `cond_eval` (4.1ms / 37%) is the next-largest — pure `lower_expr` cost on conditions (typically `x is Pattern(...)`, `x == y`, `len(coll) > 0`-style). No one-line cheap win surfaces from this drilldown: the work is structurally what an `if`-lowering must do per occurrence. The `lower_expr` cost feeding `cond_eval` is the same `exprs/mod.rs` (4331 LOC) target that backs `stmt::expr` — a tour-grade refactor, not a one-line fix.

   These are **EXCLUSIVE (self) times** — `lower_stmt` subtracts nested `lower_stmt` calls (`Stmt::If` → recursive `lower_block` → `lower_stmt`) via a `ctx.stmt_nested_dur` running total. So the buckets sum to `body::lower_block` total, not double-counting recursion. Reported via `gg profile` JSON as `lower_function::body::lower_block::stmt::<kind>` entries.

   **Why `var_decl` dominates and why it's structural (no cheap-win surface found)**:
   - 1500+ var_decls in the lowerer driver (`grep -c "^\s*case \|^\s*if \|^\s*for \|^\s*while \|^\s*match " self_host_lowerer/lower.gg → 1603`).
   - Each `lower_var_decl` (stmts/mod.rs:308-948, ~640 LOC) does N typed registry checks: `is_box`, `type_name_for_id`, `get_type_def(c_runtime_alias)`, `is_resource_type`, `is_collection_type_name`, `needs_drop`, plus 2-3 `infer_operand_type_with_builder` calls, plus 2-3 drop registration calls.
   - I audited the path: every individual operation is a single FxHashMap-on-u32-or-String lookup. There's no O(N²) pattern, no per-call allocation, no name-prefix dispatch in this function. The 12ms is genuinely 1500 × (~8μs of typed bookkeeping). A code-quality cleanup landed alongside the instrumentation: `infer_operand_type_with_builder` / `infer_operand_type_full` / `infer_type_name_from_operand_full` all linear-scanned `ctx.locals_iter()` BEFORE the O(1) `builder.locals[idx]` fallback — swapped the order (in-range index first, ctx scan only for closure-param sentinel `LocalId(u32::MAX - i)` IDs). Measured: no perf delta on lowerer (the ctx map is small enough that the scan was already fast), but it's the right shape per CLAUDE.md "fix complexity as a signal of wrong layer".

   **Real cheap-win opportunities in `body::lower_block` (deferred — not exhausted, just not on the obvious O(N) surface)**:
   - ~~(A) **`var_decl` typed-flag check coalescing**~~ **SHIPPED 2026-05-18 (perf bundle `5579bd66`)**: coalesced the two `get_type_def(&tn)` lookups in the opt/result enum-droppability branch. Added `TypeRegistry::is_closure_runtime_type` to fold three independent `c_runtime_alias == Some("GorgetClosure")` chains into a typed call. Code quality + Layering rule 2 win; no isolated perf delta (sub-ms regime).
   - ~~(B) **`if`-statement at 9ms — `lower_if`**~~ **INSTRUMENTED 2026-05-18 (perf bundle `5579bd66`)**: see `if::*` sub-bucket table above. Dominant is `then_branch` (5.2ms / 47%); `cond_eval` 4.1ms (37%); no one-line cheap win surfaces from the drilldown.
   - (C) **`expr` at 6ms — pure `lower_expr` on top-level statement expressions** (calls, method invocations, `assert(...)`-equivalents). The 6ms is the expression-lowering machinery itself. `src/ir/lowering/exprs/mod.rs` (4331 LOC) is the structural target — won't yield to FxHashMap swaps; will need a tour.
   - (D) **`match` at 4ms — `lower_match_stmt`** in `patterns.rs:326`. Pattern dispatch, scrutinee type analysis, arm-body lowering (subtracted as nested). Likely structural too.

   Verdict for this round: **instrumentation-only commit, no measurable cheap-win in `body::lower_block` itself.** The per-kind breakdown is the deliverable; the structural work is real per-kind cost that requires deeper refactor at the writer site (e.g., flattening the var_decl typed-flag chain) and is the next round's medium-effort target.

2. **`gir_lower::validate_consume_sites` (15.2ms on lowerer, 4.6ms on typechecker) — `src/ir/lowering/` (location TBD).** Second-largest gir_lower sub-pass after `lower_functions`. Likely walks every GIR function looking for ownership consume sites and checking owner/move invariants. Cheap-win surface: if it walks insts and queries a name-keyed map for each, a one-pass merge with `validate_resource_sites_all` (1.5ms, post-`4b529742` collapse) or `validate_drop_pre_rebind_and_null_to_opt` (1.2ms) could shave 4-5ms via shared walks. FxHashMap swap if it uses stdlib HashMaps internally. [priority: medium, ~half day]

3. **`codegen` tail (84.5ms on lowerer, post-`962ae144`) — `src/backend/c_lir/`. STILL THE #3 PHASE BY ABSOLUTE TIME.** Output buffer is pre-sized to 256KB but actual output is ~5MB on lowerer (`src/backend/c_lir/mod.rs:408`); bumping `with_capacity(if include_runtime { 8 << 20 } else { 64 << 10 })` saves a dozen reallocations. Several `std::collections::HashMap` instances in the per-function emit path (`mod.rs:258`, `779`, `1567`, `1664`, `1780`, `helpers.rs` throughout) — swap to FxHashMap, all keys are integer-newtypes or short strings. Also `helpers.rs:131-178` has per-emit `name.starts_with("Dict__")`/`starts_with("HashMap__")` dispatching that's a Layering-discipline-rule-violation symptom; a typed `CollectionKind` on the type registry would eliminate string-prefix dispatch and the associated allocations. **Note: `src/backend/c_lir/` was the zone of the `962ae144` rewrite — coordinate with whoever's hot there next so we don't step on a redesign in flight.** [priority: medium, ~1 day for capacity+FxHashMap; the string-prefix dispatch cleanup is a layering-discipline fix worth its own commit]

4. **`semantic::meta_consts` (13.8ms on lowerer, 5.3ms on typechecker, 3.1ms on resolver, 2.6ms on parser) — `src/semantic/meta.rs:440 evaluate_meta_consts`. STILL UN-INVESTIGATED.** Carryover from the 2026-05-16 honorable mention. Universal (every workload hits it), grows linearly with module size. Same hypothesis as last time: re-evaluation of identical `meta` constants without memoization. The fact that resolver and parser hit ~3ms each suggests the cost is in per-module meta-constant walks, not specific to lowerer-scale code. A simple `FxHashMap<MetaConstId, Value>` memo across the module would likely halve this. [priority: medium, ~half day, well-scoped]

5. **`gir_lower::lower_equip_methods` (9.0ms on lowerer, 9.0ms on typechecker, 8.2ms on resolver, 7.2ms on parser) — `src/ir/lowering/`. NEAR-CONSTANT ACROSS WORKLOADS — SMELLS LIKE FIXED-COST WORK PER METHOD-WITH-EQUIP.** Notable: this phase barely scales with input size. Suggests either (a) a big chunk of work that's per-equip-block independent of the function bodies (signature registration?), or (b) a hot-path inefficiency that's masked by the equip count being similar across these self-host fixtures. Worth profiling for an O(equips × something) loop. If (a), check whether per-equip work duplicates per-method scaffolding that could hoist out. [priority: medium-low, investigation-first, ~half day to identify the cost source]

**Honorable mentions (slow, but structural — no cheap win expected):**

- **`drop_elaboration` 117.1ms on lowerer (KNOWN/HANDLED zone).** Still the single largest sub-phase by 4×. Any future big win on lowerer compile time has to attack this. `src/lir/drop_elab.rs` uses stdlib HashMap/HashSet heavily (`val_to_slot`, `deleted_slots`, `maybe_init_slots` — all keyed by u32 newtypes), but the work itself is intrinsically per-instruction per-fixpoint-iteration, so FxHashMap alone would only buy a few ms. The real lever is fewer fixpoint iterations or smarter init-state tracking. Out-of-scope for cheap-wins; treat as the next round's *structural* work.
- **`lir_ssa` 37.0ms on lowerer (carryover from 2026-05-16, still structural).** Standard Cytron-style construction. The 2026-05-16 audit covered this — re-validate that ssa.rs internal scratch maps use FxHashMap/Bitset/IndexVec, but don't expect more than 3-5ms.
- **`lir_lower` 27.3ms on lowerer.** Quiet middle of the pack, no specific hotspot visible. GIR→LIR translation; intrinsic per-instruction work.

**Cross-workload comparison (universal vs per-workload costs):**

| Cost                                | Universal? | Notes |
|-------------------------------------|------------|---|
| `gir_lower::lower_functions`        | YES        | top sub-pass on all 4 workloads (lowerer especially, but proportional everywhere) |
| `drop_elaboration`                  | YES        | KNOWN — biggest absolute, every workload |
| `gir_lower::lower_equip_methods`    | YES, flat  | unusually constant across sizes — suggests fixed per-equip cost |
| `semantic::meta_consts`             | YES        | every workload, linear-ish |
| `lir_optimize` non-drop tail        | YES        | proportional across workloads |
| `load_imports`                      | partial    | already-parallelized, dominates small targets only |
| `gir_lower::validate_consume_sites` | scales     | bigger on lowerer (15ms) than typechecker (5ms) |
| `codegen`                           | YES        | flat % of total (~15%), scales linearly with C lines |

**Where I'd point the next optimization agent:** **`gir_lower::lower_functions`** is the new #1. It's the largest single sub-phase outside the KNOWN/HANDLED zone (61ms on lowerer, 12ms on typechecker), it's universal (every workload), and it's currently profile-blind below the phase level — so step 1 is **instrument it further** (mirror the `gir_lower` instrumentation pattern from `3dfc9916`: add sub-pass timing for "exprs / stmts / setup / drops" within `lower_function`). Once the dominant sub-sub-pass is named, the cheap-win lever follows — most likely an FxHashMap swap in `src/ir/lowering/context.rs` (3330 LOC of per-function scratch state) or an O(N²) span/scope walk in `exprs/mod.rs`. Realistic expected win: 15-25ms on lowerer (~5% off total compile), proportional on every workload because this scales with function count. The follow-on (instrumented sub-pass identified, FxHashMap swap or merge of validation walks) is then a second 1-day session.

The smaller-but-mechanical alternative is **`semantic::meta_consts` memoization** — 13.8ms on lowerer, ~50% likely halvable with a `FxHashMap<MetaConstId, Value>` memo. Well-scoped, half a day, lower risk, every-workload win. Good "second agent" task or warmup.

**Out of scope for this round:** `drop_elaboration` (KNOWN, structural), the `gorget-arena`/`gorget-js` brokenness (not a compiler perf issue), `lir_ssa` (structural — already audited 2026-05-16). [filed 2026-05-17]

## High

- **Branch-merging-expression helper (Cluster A from the post-Snag-#39 architectural audit).** Defer. Three sites now use the discipline ("allocate result_local + multiple branches Move-assign into it + `ctx.set_owned(result_local)` after merge"): `assign_match_arm_to_result` (Snag #31), `lower_catch_expr` (Snag #38), and as of Snag #51 `build_if_chain_expr` — the third was upgraded to size from `expected_type` (refine from first non-divergent branch otherwise) AND route each branch value through `assign_match_arm_to_result` for clone discipline + Move-mode + `set_owned`. The branch-prologue (the size+refine rule) is inlined at each site, mirroring `lower_match_stmt_as_expr`. With three sites the parallel-implementation cost is real (any future "size+refine" rule has to land in three places) but the structural shape varies enough — match arms iterate, if/elif/else has named slots, catch has the err-binding prelude — that a unifying helper would carry awkward parameters. Reassess when a fourth instance appears or when one of the three drifts and re-introduces a bug. [updated: 2026-05-16, was: I64-by-luck note no longer applies — build_if_chain_expr now sizes properly]

- **Safety-pass branch-divergence audit (Cluster C from the post-Snag-#39 architectural audit).** Defer. The Snag #39 bug 2 fix added `save_branch_state` / `restore_branch_state` around `Expr::Catch` recovery and `Expr::Rethrow` transform — divergence in those sub-expressions no longer leaks past the branch boundary. Audit found no other actively-leaking sites: `DefaultOp` (`x ?? exit(1)`), `BinaryOp::And/Or` (`a or exit(1)`), `OptionalChain` (`obj?.field`) all checked clean because `self.diverged` is only set by `Stmt::Return/Throw/Break/Continue` — not by noreturn-call expressions like `exit()`. If safety-pass divergence tracking is ever extended to noreturn-call expressions in the future (likely accompanies the `panic`/`exit` typed-Never work), audit these expression forms for save/restore around their conditional branches. Also worth considering: upgrade Catch/Rethrow from save+restore to save+merge (using `merge_branch_states`) — preserves recovery-side var moves in the Error path rather than discarding them. Currently a Snag #39 limitation, hasn't bitten anyone. [added: 2026-05-12, from architectural audit]

- **Self-host: extend `tc_types.expr_types` write-through to more Expr arms; consider span-keyed-sidecar replacement.** With Gap #2 Phase 3 shipped (see DONE 2026-05-16) and Stage 9 scoped (DONE 2026-05-16: 3 of Rust's 10 sites have direct self-host analogues — EAwait, ESpawn, parse — now written; the other 7 either route through ECall in self-host (Rust 1619 qualified enum ctor), have no self-host analogue (Rust 1762 closure-method, Rust 4502 default/one), are folded into self-host's consolidated registry path (Rust 1756/1778 covered by 1712 at infer.gg:358), or are inlined into a hardcoded chain that would require refactoring to 1:1 mirror (Rust 1770 builtin_method_type)). Rust gg writes to its `expr_types: FxHashMap<Span, TypeId>` at 10 sites (`src/semantic/typecheck.rs` lines 1619, 1712, 1756, 1762, 1770, 1778, 2026, 2049, 4481, 4502 — actual line numbers post-2026-04 edits). Self-host now covers the architectural-analogue subset; the remaining 41+ infer_expr_type-family conversion (Stage 9 deferred) and Rust 1770's inlined-builtin-chain mirror remain to extend the write-through map. Two follow-ons:

  1. **Extend write-through coverage.** As the lowerer grows new query sites, add the matching write-through in the typechecker (mirroring Rust's coverage). Examples Rust has that self-host doesn't 1:1 yet: builtin-method chain (Rust 1770 → many self-host returns; would benefit lowerer queries for `len`/`get`/`keys`/etc.), closure-return inference, EAs cast, EBlock body-ret type. Add per query site, never speculatively — speculative full-coverage cost ~100s on self-host's own source (the wrapper-based variant I prototyped first). [updated: 2026-05-16, Stage 9 scoped (3 sites shipped) — remaining 41+ deferred]

  2. **Span-keyed sidecar smell.** Both Rust gg and self-host use a `Dict[Span, TypeId]` sidecar map. The pattern has known fragility: spans are byte offsets, and we've been bitten by span-collision bugs before (`resolution_map` in the resolver). Today's protection is per-module offset shifting in `loader.gg`. The architecturally cleaner shape would be a typed field on `SpannedExpr` itself (`int resolved_type`), keeping data with its owner per Layering rule 2. Pioneering this in self-host would be a divergence from Rust — only worth doing if (a) Rust agrees to follow, or (b) a span-collision bug surfaces that the sidecar can't safely fix. For now keep the sidecar; pair-port to Rust if/when we redesign. [filed 2026-05-16 — sidecar is a faithful Rust mirror, but worth flagging for a future "redo it right" pass]

- **Rust frontend: unify the 7 `lower_for_*` functions into a single scaffold + per-type element extractor.** Architectural cleanup opportunity surfaced by the self-host's `case SFor` work (2026-05-14). `src/ir/lowering/stmts/for_loops.rs` has 1159 lines covering 7 collection-specific lowering functions, each duplicating ~80% scaffold. Self-host's reference-grade `lower_for` (in `tests/fixtures/self_host_lowerer/lower.gg`) demonstrates the cleaner shape: ONE scaffold parameterised at the element-extract step (`lower_for_vector` fast path + `lower_for_iterator` protocol fallback). Rust frontend should mirror this.

  **Inventory** (`src/ir/lowering/stmts/for_loops.rs`):

  | Function           | Lines | Element shape                          | Notes |
  |--------------------|-------|----------------------------------------|---|
  | `lower_for_range`  |    75 | `loop_var + 1`                         | Inclusive vs exclusive cmp op |
  | `lower_for_string` |   160 | `gorget_str_codepoint_at` + `byte_pos += cplen` | Source-aware ptr setup (3 shapes: owning-named, ptr-typed, plain). UTF-8 codepoint-aware. |
  | `lower_for_array`  |   105 | `index_load_borrow` + `idx += 1`       | Uniform layout (Field(2) len). Pattern destructure via `emit_pattern_bindings`. |
  | `lower_for_enumerate` | 140 | Same as array + index binding          | Auto-deref Ptr-typed iterables (Snag 2026-05-13). Strips `.enumerate()` and binds tuple parts. |
  | `lower_for_dict`   |   145 | `gorget_map_iter_state` filter + `gorget_map_iter_key` + `_value` via output params | `oi` outer index + `idx = oi`. Filter via `state_ok` branch (`elem_bb` vs `incr_bb`). |
  | `lower_for_set`    |   155 | Ordered (`order_len` + `order[i]`) vs unordered (`cap` + state filter) | Two sub-shapes by `collection_kind == OrderedSet`. |
  | `lower_for_iterable` | 180 | `Type__iter()` → `Iter__next()` → `Option[T]` | self-as-iterator detect; tag check; payload extract. Universal protocol. |

  **Shared scaffold across all 7** (the duplication):
  ```rust
  let header_bb = builder.new_block();
  let body_bb   = builder.new_block();
  let incr_bb   = builder.new_block();    // skipped for protocol/iterable (next() advances)
  let exit_bb   = builder.new_block();
  let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
      (builder.new_block(), builder.new_block())
  } else {
      (exit_bb, exit_bb)
  };
  builder.jump(header_bb);
  // ... per-kind logic ...
  // tail:
  if let Some(else_body) = else_arm {
      builder.switch_to(else_exit_bb);
      lower_block_scoped(ctx, builder, else_body);
      builder.jump(exit_bb);
      builder.switch_to(break_exit_bb);
      builder.jump(exit_bb);
  }
  builder.switch_to(exit_bb);
  ```
  Plus the Borrow-mode iter_local init (5 of 7 functions — array/enumerate/dict/set/iterable):
  ```rust
  let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
  let iter_local = builder.add_local(iter_type, None);
  let iter_assign_mode = if ctx.type_registry.is_resource_type(iter_type) {
      AssignMode::Borrow
  } else {
      AssignMode::Copy
  };
  builder.assign_mode(iter_assign_mode, Place::local(iter_local), iter_op);
  ```
  Plus the body scope discipline (push_loop, drops.push_scope, lower_block, drops.pop_scope, pop_loop, save_locals/restore_locals).

  **Per-kind variability** (what doesn't unify cleanly):
  - **`lower_for_string`**: source-aware ptr setup is genuinely string-specific (Stage 5 §6.8, three iter_op shapes). Not a target for unification.
  - **`lower_for_range`**: doesn't use iter_op at all; just start/end values. `save_locals` wraps the WHOLE function (not body-scoped). Outlier.
  - **`lower_for_dict` / `lower_for_set`**: state-check filter (`state_ok` branch) introduces an extra block (`elem_bb`) between body_bb and the actual body lowering. Pattern destructure uses output-parameter calls (different from array's `index_load_borrow`).
  - **`lower_for_iterable`**: no `incr_bb` (next() advances internally); uses `header_bb` for continue. Different control flow.

  **Recommended approach — Three layers**:

  1. **Helper extraction** (low-risk, ~150 lines retired, no semantic change):
     - `alloc_for_blocks(builder, has_else, has_incr) -> ForBlocks` — block allocation
     - `emit_else_arm_tail(ctx, builder, else_arm, &blocks)` — else-handling tail
     - `init_borrow_iter_local(ctx, builder, iter_op) -> (LocalId, TypeId)` — Borrow-mode init for the 5 collection types that share it
     Apply to all 7 functions (range and string skip the `init_borrow_iter_local` helper). One PR. Tests stay green.

  2. **Filter abstraction** for dict/set (medium-risk, ~30 lines retired):
     - `with_state_filter(ctx, builder, state_check_op, incr_bb, |ctx, builder| { /* element extract + bind */ })` — emits the `state_ok → elem_bb else incr_bb` branch + the elem_bb body
     Mostly tightens dict/set/ordered-set/unordered-set's parallel paths.

  3. **Trait-based unification** (higher-risk, ~300-400 lines retired):
     - `trait CollectionLowerer { type State; fn init(...) -> Self::State; fn cond(...) -> Operand; fn extract_and_bind(...); fn advance(...); }`
     - One `lower_for_scaffold<L: CollectionLowerer>(ctx, builder, lowerer: L, ...)` shared scaffold
     - 7 backends: `ArrayLowerer`, `StringLowerer`, `DictLowerer`, `SetLowerer`, `IterableLowerer`, `EnumerateLowerer`, `RangeLowerer`
     - Each backend captures its specific state (idx/len, dict_ptr/oi/limit, byte_pos/cplen, etc.)
     The string source-aware ptr setup stays in `StringLowerer::init`; range's no-iter-op stays in `RangeLowerer::init`. Filter behaviour can live in `extract_and_bind` returning an optional skip signal, OR in a separate `should_skip` hook.

  **Verification strategy**: incremental — each of the three layers ships independently with full integration sweep (`cargo test --test integration --release --test-threads=4`). 1122/1122 must remain (the documented parallel-run flakes `vector_task_get` and `hot_reload_basic_lir` are pre-existing and pass in isolation).

  **Reference implementation**: `tests/fixtures/self_host_lowerer/lower.gg::lower_for` (and its two sub-paths `lower_for_vector` + `lower_for_iterator`). The self-host shape is ~120 lines covering Vector/Deque + iterator-protocol; the Rust frontend should compress to roughly 7× that for the 7 backends, with the scaffold shared.

  **Estimate**: Layer 1 alone is half a day. Layer 1+2 is a day. Layer 1+2+3 is 2-3 days. Layer 3 is the biggest win architecturally and the highest risk — recommend doing layers in order, with green tests gating each step.

  **Tied-off TODOs after this lands**: the "Self-host: register trait-equipped methods in `fn_sigs`" entry above and the iter-chain fixture mismatches in `lowerer_comparison` both unblock once Rust + self-host have matching architectures. [added: 2026-05-14, scoped 2026-05-14 with full design sketch for off-loading to a dedicated agent]

- **Runtime-side panic locations** (~50+ sites in `src/backend/c/c_runtime.rs`): explicit follow-on from stack-traces Phase 3. Compiler-side emit now produces `file:line:col: <msg>` for inline traps (Add/Sub/Mul/Div/Mod/Rem/Shl/Shr/BoundsCheck/DivCheck/Trap + unwrap_err combinator). But the runtime helpers — `gorget_array_index_oob`, `gorget_str_*_oob`, channel-closed sends, alloc failures, `gorget_panic("integer overflow")` for runtime `__builtin_*_overflow` paths at c_runtime.rs:5042/5047/5052, and ~45 more — are called *from inside other runtime functions* (e.g., `gorget_array_index_oob` is called from `gorget_array_get`) with no caller span available. They currently fall back to `<unknown>:0:0:` via the `gorget_panic` wrapper. Proper fix needs per-runtime-function span plumbing: every runtime entry point that can panic grows `(const char* file, int line, int col)` first three params, and every compiler emit site that calls it passes the LIR instruction's span. Surface is much larger than Phase 3 (every `gorget_array_get`, `gorget_str_at`, `gorget_chan_send`, etc. call site in c_lir's emit). Estimate: 5-8 days; touches every runtime panic helper signature + every compiler emit site that calls them. [added: 2026-05-17, from stack-traces v1 completion]

- **`caller_location()` builtin + multi-frame stack walking** [LOW PRIORITY]. Two future-direction items explicitly deferred from stack-traces v1. (a) **`caller_location()` builtin**: today gorget-js's internal helpers (`member_lookup(&realm, base, "length", 0, 0)`) take `line, col` params that callers thread through. A `#[track_caller]`-like attribute or implicit `caller_location()` builtin would let helpers inherit the actual call site without manual plumbing. Needs frame-walking discipline at GIR call lowering: the call site's span becomes the *callee's* `caller_location` at the topmost user frame. Plumbing: typed sidecar on `CallExtern`/`Call` carrying caller span, read by the builtin at the helper's body. (b) **Multi-frame stack walking**: today panics print one frame (the throw site). A full backtrace needs DWARF / libunwind plumbing in the C backend, or LLVM's native stackmap. Async / spawned-task panic locations also need the spawning call's span carried through spawn context construction. Both are multi-week investments; defer until v1 is battle-tested and the demand is concrete. [added: 2026-05-17, deferred from stack-traces v1]

- **Typed `Diagnostic` struct in the self-host — sweep remaining stages (parser / typechecker / lowerer / lexer).** POC shipped for the resolver 2026-05-17 (see DONE.md): new `tests/fixtures/self_host_resolver/diagnostic.gg` module defines `Diagnostic { Span span; Severity sev; DiagKind kind; String message }` + `Severity { Error, Warning, Suggestion }` + `DiagKind { UndefinedName, DuplicateDefinition, NotAType, TypeMismatch, ParseError, Unreachable }` + constructor helpers (`Diagnostic.error(...)` / `.warn(...)` / `.suggest(...)`). `ResolveContext` carries a `Vector[Diagnostic] diagnostics` field; 2 silent-error sites (`EIdentifier` / `EStructLiteral` undefined-name lookups) now push real diagnostics; driver renders them as `DIAG <severity> <kind> <span> <message>` lines after RES output. `resolver_comparison` integration test still passes (normalizer only checks DEF + RES lines, so DIAG lines slot in cleanly). Dead `errors.gg` stub retired.

  Remaining stages still on the old free-form `Vector[String] errors` accumulator pattern, with approximate push-site counts (from `grep -c "errors.push" tests/fixtures/self_host_<stage>/*.gg`):
    - ~~**parser** in `self_host_parser/`~~ — SHIPPED 2026-05-17 (see DONE.md). 8 sites migrated; driver drains diagnostics to stderr so the line-by-line stdout comparison stays stable. The other dirs' `parser.gg` copies (independent per `md5sum`) remain on the old shape — migrate when their stages migrate.
    - ~~**typechecker**~~ — SHIPPED 2026-05-18 (see DONE.md). Genuinely green-field migration: the stage had zero error sites pre-migration — type mismatches silently propagated `types.error_id`. Added `tests/fixtures/self_host_typechecker/diagnostic.gg`, plumbed `Vector[Diagnostic] diagnostics` onto `ResolveContext`, flipped all 19 `ResolveContext ctx` params to `ResolveContext &ctx` (113 caller sites updated to `&ctx`), wired the driver to drain to stderr, and seeded one exemplar push site (`MainThrowsNonInt` — `main` can only throw `int`) mirroring src/semantic/typecheck.rs:5197. Because of the typecheck+infer symlink, the lowerer dir auto-inherits the plumbing; only the lowerer driver was touched (pass `&ctx`) and a `diagnostic.gg` symlink added. **First port batch 2026-05-18**: 4 control-flow check sites ported (BreakOutsideLoop / ContinueOutsideLoop / ThrowInNonThrowingFunction / DoubleAwait) plus a fifth ReturnOutsideFunction. Added `DkControlFlow` variant to all 4 per-dir `diagnostic.gg` copies (parity), added `loop_depth` / `current_function_throws` / `in_function_body` fields to `ResolveContext` with save/restore at function and closure entry, wired `walk_expr_closures_inner`'s new EAwait case to fire DoubleAwait at return-position too (VarDecl already drives it via infer.gg). Fixture: `tests/fixtures/typecheck_control_flow_diagnostics.gg`. **42 of 47 Rust-side `self.error(...)` sites in src/semantic/typecheck.rs remain unmigrated** — TypeMismatch / WrongArgCount / NoFieldFound / RethrowInNonThrowingFunction / OnErrorInNonThrowingFunction / SelectOutsideAsync / AwaitOutsideAsync / SpawnNonFuture / AwaitNonFuture / NonExhaustiveMatch / PositionalAfterNamed / etc. Each is a separate follow-up requiring the corresponding check logic ported, not just the diagnostic call. Dead `errors.gg` stub retired (both dirs).
    - **lowerer**: `lower.gg`'s 1 silent error site (a fixme comment, not an active push). Diagnostic infrastructure already inherited via the typechecker migration (lowerer's `resolve.gg` / `typecheck.gg` are symlinks, and a `diagnostic.gg` symlink lives in the lowerer dir now). When lower.gg starts emitting validation diagnostics (recursive-drop / unreachable-after-throw / etc.), they can `ctx.diagnostics.push(Diagnostic.error(span, DkUnreachable(), msg))` directly. Estimate: low; mostly bookkeeping when the first lowerer-side check lands.
    - ~~**lexer**~~ — SHIPPED 2026-05-18 (see DONE.md). 5 sites migrated (4 in `lex_scan_string` + `lex_scan_char_lit`, 1 in `lex_scan_single_char`); dropped the write-only `String lex_error` field; added `LexResult { Vector[SpannedToken] tokens; Vector[Diagnostic] diagnostics }` + `lex_with_diagnostics(source)` entry point; `lex_tokenize` kept as a tokens-only convenience wrapper for callers (main.gg, parser's `parse_source`) that don't need diagnostics; driver drains to stderr. New `DkLexError` variant added to all three per-dir `diagnostic.gg` copies for parity.

  Both follow-ups surfaced by the parser POC closed 2026-05-17: spurious "expected type" diagnostics gated on `speculation_depth` (commit `8103afeb`); C-backend drop-fn forward-decl gap fixed at `emit_recursive_struct_drops` / `emit_recursive_struct_clones` (commit `5168d24e`), and the parser's `parse_source_into(&out_diagnostics)` out-parameter workaround retired in favor of a `ParseResult { Module module; Vector[Diagnostic] diagnostics }` struct (folded into commit `8103afeb`). See DONE.md.

  Status: parser / lexer / resolver / typechecker (infrastructure + 1 exemplar push + first 5-site port batch) all shipped. Lowerer infrastructure auto-inherited via the symlink chain. **42 of 47 Rust-side typecheck error sites remain** — each needs the corresponding check logic ported, not just the diagnostic call. The Diagnostic module should be promoted to a shared location once cross-directory imports become reasonable — depends on the loader's cross-directory import rules; today four per-directory copies (resolver/parser/lexer/typechecker, the last also serving lowerer via symlink) kept in sync by hand. All four copies have identical 8-variant DiagKind enums (UndefinedName / DuplicateDefinition / ParseError / LexError / NotAType / TypeMismatch / ControlFlow / Unreachable). Estimate per Rust→self-host typecheck site port: ~10-30 minutes mechanical when the check needs no new ctx state; ~30-60 minutes when it does (like the control-flow batch which added 3 ctx fields). [revised: 2026-05-18, post first port batch]

- **Result→T auto-propagation — retire the residual consumer-side `maybe_auto_propagate` safety nets.** The producer-side centralization shipped 2026-05-15 (see DONE.md) — `lower_expr` now applies `maybe_auto_propagate` automatically when the expression is a `Call` / `MethodCall`, plus the matching typechecker auto-prop gates for if/while/elif/index. Snag #49 holdouts (for-iter, if-cond, while-cond, index) closed; new consumer sites that lower a Call to a Result-returning fn auto-prop without any explicit hook. The seven consumer-side `maybe_auto_propagate` calls left in tree (`stmts/assigns.rs:112`, `stmts/mod.rs:129/317/1422`, `calls.rs:151/1030/1065/1206`, `methods.rs:224`, `exprs/mod.rs:1788/1823/1854`, the match-scrutinee fallbacks) all exist for the *Identifier-of-Result* case: `Result[T,E] r = ...; T x = r;` (RHS is an `Identifier`, not a producer). The producer-side hook can't fire on identifiers without breaking `body_r.unwrap()`-style code (the receiver MUST stay `Result` for `.unwrap()` to dispatch correctly). The TODO's principled fix — plumb the typechecker's `expr_types` map (the post-auto-prop semantic type per span) through monomorphization into IR-lowering, then at `lower_expr` exit check `op_type` vs `expr_types[span]` and auto-prop only when they disagree in the throws-sugar direction — would let us retire all seven safety nets. Estimate: 1-2 days, mostly the monomorphization plumbing (substitute_expr_types-shaped pass for spans), then one-line decision at the hook. The current state is the producer-side ~80%-covered fix the TODO calls out; the residual 20% (Identifier-of-Result destinations) remains belt-and-suspenders until the principled fix ships. [demoted: 2026-05-15, post Snag #49 closure]

- **Deferred String materialization — Site #4 (borrow-checker decidability)** [LOW PRIORITY] (filed 2026-05-04, sites #1 + #3 closed 2026-05-05/06, site #2 retired as theoretical 2026-05-11). The lifetime question — "can we statically prove `x` doesn't outlive `source`'s last possible mutation?" — needs a separate design pass. Today's heuristic (`is_cow_unsafe_at(name, span)` for reassignment-on-forward-path) catches the common case but isn't lifetime-aware. Defer to a dedicated session.




- **Residual: `Option[Box[T]]` / `Result[Box[T]]` field drops not emitted on enum variants and struct fields.** The Box-field-drop wrapper `Box__T__drop` and its wiring at struct/enum-variant scope-exit (cases a + b + c of the prior Box[T] item) were closed 2026-05-01 — see DONE. The Option/Result ENUM-VARIANT skip at `populate_recursive_drop_enums` (mod.rs:471-481) and STRUCT-FIELD skip at `populate_recursive_drop_structs` (mod.rs:412-422) was kept intentional: enabling the drop crashes the self-host `resolve_stmt` path because `stmts.get(i).unwrap()` (resolve.gg, post-inline 2026-05-10) returns `Stmt` by value — a shallow copy that aliases the vector's interior box/string pointers; both copy and source drop, and dropping the `Option[SpannedExpr]` field inside `Stmt` double-frees the SpannedExpr's Expr/string that the standalone SpannedExpr drop already freed. The proper fix is at the COMPILER level: make `Vector[T].get(i)` for resource T auto-clone (deep) or return `Ref[T]`-only (forcing the caller to .clone() at the boundary). Once that lands, the Option/Result drop skip can be removed and `option_box_enum.gg`'s 3 leaked Some(Box(...)) blocks will free correctly. Today: leak (3 blocks for option_box_enum), not unsoundness. [added: 2026-05-01, refreshed citation 2026-05-10]

- **Self-host silent-fallback audit — IN PROGRESS**. Diagnostic wiring shipped (commit af0cb513): three sites now emit `/* [bug] ... */` comments in the generated output instead of silently returning sentinels: (a) `map_binop` unknown operator, (b) `EIdentifier` unknown name, (c) `EFieldAccess` unknown field. Also added `map_compound_binop` for `+=` / `-=` / ... spellings (commit 299ffb0c — was the root cause of `last_us -= 1` → `last_us += 1`). Still to audit / tighten: `infer_method_return_type` I64 fallback for unknown methods, `collection_element_type` "" fallback for unknown prefixes, `type_id_to_name` "int64_t" fallback for non-GtNamed tids. Attempted a cap on `[bug]` emission count but stage-1's lowerer doesn't reliably propagate mutable global state so the counter stayed at 0 — reverted to loud-by-default; callers can filter via `| grep -v '\[bug\]'` and dedupe via `sort -u | uniq -c`. Proper env-var toggle (`GORGET_QUIET_FALLBACKS=1`) deferred until the env-var reader lands in the self-host. [revised: 2026-04-24]

- **Stdlib narrow waist — Phase 2c residual items**: (a) (2) impl-override sig substitution **SHIPPED 2026-04-29** (see DONE.md). (b) **Builtin Vector HOF expansions cleanup** — both void-return entries have now been retired in favour of their user-space wrappers in `lib/std/iter.gg`: `Vector.each` (2026-04-21 commit 1b0e7022) and `Vector.for_each` (2026-05-16, see DONE.md). The remaining typed-return Vector HOFs (filter / map / fold / reduce / any / all / find / find_index / count / enumerate / flat_map / zip) stay as BuiltinMethodDecl entries — IR-lowering reads their declared return types via `resolve_builtin_method_return_type` when the user-space wrapper's sig hasn't been registered yet (e.g. during early generic mono). Full retirement blocks on a separate signature source for IR-lowering when BuiltinMethodDecl is absent; that's a bigger task. Dict.each / Set.each BuiltinMethodDecls also stay (no user-space wrapper migration yet). LIR `HofOp` variants stay live regardless — they serve Dict / Set too. See design doc §10; `lib/std/iter.gg` is the authoritative source. [revised: 2026-05-16]

- **Self-host check_comparison residual gaps — 8 mismatches** [revised: 2026-05-10; current score 1013/1021 = 99.2%]:
  - **(a) Type-variable preservation** (~5 fixtures: `coroutine_collections`, `generic_pair_swap`, `httpserver_middleware`, `httpserver_router_extended`, `test_vector_bool`, `test_vector_edge_cases`). Rust keeps numbered inference vars (`?5`, `?0`, …) at unresolved closure-param call sites; self-host concretises or emits `<error>`. Architectural difference in how the two infer.
  - **(b) Function-type parser String-aliasing** (1 fixture: `generic_callable.gg`'s `Callable[<error>(int)]`). Same lineage as the long-running self-host parser bug where `int(int) f` (function type as param/return) corrupts the outer return-type's primitive name as bytes from the following identifier.
  - **(c) Misc one-offs** (closure_tuple_destructure, sigil_type_args, copy_struct_closure_capture). Each is a single-fixture quirk. For `sigil_type_args`: the trait-body assoc-type-alias parse error (`type Limit: Comparable = T` → `expected type`) was closed 2026-05-17 (see DONE) — residual mismatch is now the `int !` / `String &` iterator-intent sigil rendering in parameter types, plus the missing `?;` per assoc-type in the canonical trait formatter output. Sigil rendering is the load-bearing one; the `?;` gap only shows if the comparator reaches the trait line, which it doesn't while line 0 mismatches on `int !`.
  Fix path: most gains here are architecturally deep (type-var numbering preservation, parser String-aliasing). The score is stable; further closure may be net-negative ROI vs the layering-discipline migrations in `docs/internals/self-host-resource-model.md`. [revised: 2026-05-10]

- **Cloneable trait for generic bounds**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Runtime counters shipped via `--clone-stats` — atexit line emits `[clone-stats] array_clone=... map_clone=... set_clone=... string_cow=... string_cat=... box_alloc=... ... peak_rss_kb=...`. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`) — ships alongside the next round of ownership work. [updated: 2026-04-21]

- **C backend: retire local val_types/ptr_pointee fixup phases (follow-on after the 2026-05-15 seed migration)**: The C backend now seeds `val_types` and `ptr_pointee` from `func.value_types` and `func.pointee_types` (see DONE 2026-05-15), and pointee_types is computed BEFORE value_types so the shared `infer_inst_type` can fall back through pointee for `Inst::Load { ty: Void }`. The CallExtern→SlotStore slot-type override has also moved upstream (see DONE 2026-05-16). Remaining work is to push the rest of the C-backend-local augmentations upstream so the local pass disappears entirely:
  - Guard accessor inference from consumers (`mod.rs:1381-1456`) — `gorget_guard_get` / `gorget_shared_get` return void* but the inner type can be inferred from the next 10 instructions (arithmetic op, IntCast, printf %f, etc.).
  - Cross-type Option/Result map combinator override (`mod.rs:1465-1486`) — reads `LirExtern.combinator_result_struct_id` to pick the correct result struct.
  - Consumer-driven Add/Cmp peer-type back-propagation (`mod.rs:1490-1570`) — when one operand has type info and the other doesn't, propagate.
  - InlineC→SlotStore type inference (`mod.rs:1306-1322`) — InlineC dst values get the type of the slot they get stored to.
  - Ret-from-function backfill — Ret(value) implies function return type when value is untyped.
  Each of these is *cross-instruction* reasoning. To push them upstream, the shared `compute_module_value_types` would need a fixed-point pass or a multi-phase walk that today only the local pass has. Scope: medium — half-day for any one of them, plus tests. [updated: 2026-05-15, after partial seed migration shipped]

- **Decompose emit_call_extern.rs (~908 lines)**: Tier 1-3 lifts done; HOF cluster lifted 2026-05-16 (Option/Result combinator inlining → `emit_hof.rs`, -200 lines); printf rewriting lifted 2026-05-18 (`%lld → %f / %.*s` format-string fixup + single-arg fast path + per-arg Str decomposition → `emit_printf.rs`, -106 lines). The original "Vector HOF inline handlers" mentioned in the pre-2026-04-15 entry had already been migrated to LIR's `HofExpand` op (commits `79ab2cc2` and friends). Remaining clusters in `emit_call_extern.rs`: out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-05-18]

- **Phase A's `resources.toml` build-tooling pipeline — the cross-language single source of truth.** The big unshipped piece of Phase A per `docs/internals/unified-resource-model.md` §3.6 (the `RUNTIME_DECLS` table + Companion Source design), §9.2 (the canonical-source diagram + TOML rationale), and §13 (Summary — explicitly named as one of "the two genuinely unshipped pieces today"). The self-host side is the consumer: `docs/internals/self-host-resource-model.md` §3 (Phase A — self-host's `GirResourceMetadata` schema) is what reads the generated `lib/std/gen/resources.gg`; §3.4 anticipates the Gorget gaps a self-host emitter will surface. Cite both docs in PRs that touch the pipeline. Closes a latent bug class (frontend/runtime/self-host signature drift) and unblocks every future backend (LLVM is in TODO, WASM is the long-term plan) by eliminating hand-mirrored extern signature tables. The ONLY phase whose contract surface crosses Rust ↔ self-host (per unified §9.1 table) — every other phase is internal to whichever compiler implements it. Status: zero progress; verified by codebase grep (no `resources.toml`, no `build.rs`, no `RUNTIME_DECLS`/`RuntimeDecl` symbols, no `src/ir/gen/` / `lib/std/gen/` / `src/backend/c/gen/` artifact dirs, `runtime_extern_sig` still hand-written at `src/lir/lower/calls.rs:136`).

  **What this replaces.** Three hand-maintained mirrors of every runtime function's signature: (1) Rust-side `runtime_extern_sig` (`src/lir/lower/calls.rs:136`); (2) C runtime declarations in `src/backend/c/c_runtime.rs`; (3) self-host's mirror in `tests/fixtures/self_host_lowerer/lir_codegen.gg`. Each new runtime fn today is ~4-5 coordinated edits; drift is silent (linker error at best, UB at worst). We've shipped forensic fixes for individual drifts before — TOML closes the class structurally.

  **The pipeline as designed.**
  ```
  resources.toml (canonical, hand-edited, schema_version-stamped)
      │
      └── build.rs ──┬──→ src/ir/gen/resources.rs       (const Rust data via serde)
                     ├──→ lib/std/gen/resources.gg      (const Gorget data for self-host)
                     └──→ src/backend/c/gen/resources.h (extern decls + struct layouts)
  ```
  Schema: `[resource.X]` (~10-12 entries: GorgetString, GorgetArray, GorgetMap, GorgetSet, GorgetDeque, GorgetHeap, Mutex, Channel, Shared, Box, …) carrying `size`, `align`, `drop_fn`, `clone_fn`, `materialize_fn`, `copy_semantics`, `collection_kind`, `box_kind`, `opaque_handle`, etc. + `[runtime_fn.Y]` (~80 entries: gorget_array_new, gorget_str_to_cstr, …) carrying `params`, `ret`, `side_effects`. Top-level `schema_version = 1` field embedded in every generated artifact; load-time mismatch is a hard build error.

  **Why TOML, not YAML / const Rust / Gorget DSL.** Data is flat (two levels), schema is small, Rust toolchain (`toml` + `serde`) is rock-solid. YAML's Norway problem / 1.1-vs-1.2 drift / implicit-type-coercion footguns buy nothing for a flat schema. Const Rust would privilege Rust unnecessarily — if any consumer requires generation (self-host does), the build-script cost is paid; generating ALL consumers from a neutral source is symmetric. Gorget DSL creates a chicken-and-egg (build.rs would need the compiler). Only `build.rs` parses TOML; runtime consumers all read generated artifacts.

  **Concrete work items** (sequenced):
  1. **Schema design + spike** (~3 days). Design `[resource.X]` and `[runtime_fn.Y]` schemas; populate ~5 representative entries. Per §9.4 rule 1 ("Spike before freeze"): migrate ONE consumer end-to-end as a throwaway before declaring the schema frozen. Routinely saves a week of "we found another field we need" rework.
  2. **`build.rs` emitters** (~3 days). Three artifacts: Rust const data (via serde), self-host Gorget literals, C runtime header.
  3. **Migrate `runtime_extern_sig`** (~3 days). Replace the hand-written table at `src/lir/lower/calls.rs:136` with reads from generated const data. Mechanical once the schema is frozen.
  4. **Self-host adoption** (~3 days). Wire `lib/std/gen/resources.gg` into self-host's lowerer; retire the hand-mirror in `lir_codegen.gg`. The `*_comparison` tests are the regression net.
  5. **Version-stamp + recall buffer** (~2 days). schema_version checks at every consumer; verify mismatch produces a hard build error.

  **Estimate**: ~2-3 weeks of focused work (the doc says 3-4; the design-heavy phase is shorter than that headline suggests once you separate spike+emitters from migration). Mostly mechanical after the schema freezes.

  **Discipline (§9.4)**: spike-before-freeze, freeze-before-broad-migration, recall-on-drift, version-stamp-as-backstop. Pin the schema only after one end-to-end consumer migration validates it. While migrations are in flight, no edits to the contract surface. Cite the doc in PRs that touch the schema.

  **What this does NOT accomplish.** No language feature gain. No perf improvement (generated const data is roughly what `runtime_extern_sig` does today, just declaratively). No user-visible bug fix — the drift class is latent, not actively bleeding. The win is structural insurance: do once, benefit forever. Every future resource type, runtime fn, or backend addition is one row instead of three coordinated edits.

  **Sequencing note.** Not urgent — competes with the truncate removal, LSP, parity work, etc. for the same weeks. But high-leverage when finally done. Don't queue an agent on this — 3-4 weeks of design-heavy schema work needs collaborative review, not background autonomy. [added: 2026-05-19, filed from `docs/internals/unified-resource-model.md` §3.6 + §9.2 + §13]

## Medium

- **Drop the imported-module typecheck-error truncate (writer-site fixes).** `check_items_recursive_tc` in `src/semantic/typecheck.rs` truncates ALL errors from `Item::Module` (imported modules) to silence false positives from a "foreign scope context". Snag #2 (2026-05-17) added a `hard_errors` sidecar so concrete-vs-concrete call-arg mismatches survive — but the blanket truncate still hides everything else.

  **STATUS 2026-05-19 — fix (a) shipped; truncate still in place.** The byte-literal class (a) is closed at the writer site: BinaryOp now threads `decl_type_hint` from a fully-concrete integer-primitive LHS into a bare integer-literal RHS, so `byte c == b'X'` types as `uint8 == uint8` without losing the LHS context. Call-arg / VarDecl positions already worked via existing `decl_type_hint` plumbing; the BinaryOp arm was the gap. Same writer-site idiom that Unary already uses for `-IntLiteral`. Fixtures: `tests/fixtures/self_host_lexer/lexer.gg`'s 130+ byte-literal sites now type-check clean.

  **What's left blocking the truncate removal.** Diagnostic run with truncate temporarily disabled (and my (a) fix in place) surfaced these residual errors across the six self-host drivers — none of them match the original (b)/(c) classes described in the brief:
  - **Stale self-host code missing newer enum variants** — `EFString`, `ETry`, `EMove`, `EMutableBorrow`, `EAs`, `EAwait`, `ESpawn`, `ESpawnBlocking`, `ERethrow`, `ECatch` (Expr variants); `SMeta`, `SAssertReturn`, `SSnapshot`, `SMetaForMatch`, `SWith`, `SAssert`, `SNamedScope`, `SSelect`, `SOnError`, `SItem` (Stmt variants); `KwCatch`, `KwOn`, `KwSnapshot` (keyword variants); `GTNone` (GirType variants) — at non-exhaustive `match` sites in `self_host_{parser,resolver,typechecker,lowerer,check}/{parser,resolve,format_gir,lower}.gg`. These are real errors; self-host hasn't kept up with newer AST variants. Fixing requires updating all five driver directories to add catch-alls or proper handling.
  - **Real bug**: `no field 'span' found on type 'SpannedToken'` in `self_host_{parser,resolver}/parser.gg:2512`/`2485` (the `snap_name_tok.span` access).
  - **(b) and (c) NOT OBSERVED** — the self-host parser.gg has no `throws` functions, so the auto-prop unbound-Var-Ok class claimed in the brief doesn't manifest here. `eval.gg:1528`-style match-arm result mismatches are in gorget-js code (separate repo), not in our tree. Either the brief was based on a different snapshot, or those classes need to be re-surveyed against current code before fixing.

  **Next step (deferred):** retire the stale self-host code (add the missing match arms / catch-alls) across all five driver directories, fix the SpannedToken.span access, THEN try removing the truncate. Estimate: ~1-2 days for the self-host modernization sweep, plus a focused retry on (b)/(c) once we have a concrete fixture surfacing them.

  **Why not bull through and remove the truncate now**: the surfacing errors are real bugs (stale code), not the "foreign-scope false positives" the truncate's docstring claims. Removing the truncate would make `gg check` fail on the self-host drivers — but the fix isn't a compiler change, it's a self-host code update. That's a separate body of work from the writer-site brief. Per CLAUDE.md "Don't redesign around compiler gaps" — these aren't compiler gaps, so the rule cuts the other way: fix the self-host code, don't leave the truncate as a perma-mask.

  Once everything else is clean, the entire truncate (`errors.truncate(error_count)` block at typecheck.rs ~6071) plus the `hard_errors` field + `hard_type_mismatch` call sites at ~1364-1394 can be deleted; the signal becomes redundant when concrete-vs-concrete arg mismatches are normal `errors` again. [revised: 2026-05-19, fix (a) shipped via BinaryOp decl_type_hint thread]

- **`--clones=stats` per-CloneId runtime breakdown.** Today `--clones=stats` ships exactly the historical `--clone-stats` aggregate line (`array_clone=N map_clone=N …`). The compile-time half landed with `CloneId` as the join-key axis: every `ImplicitCloneWarning` carries an `id: CloneId`, and `--clones=verbose` renders `(id, span, type, reason, size_bytes, runtime_fn)` per site. The remaining work is the runtime side: thread `CloneId` from `ImplicitCloneWarning` through LIR lowering down to each runtime clone-call emission site (`gorget_array_clone`, `gorget_map_clone`, `<UserStruct>__clone`, …), allocate a per-id `_Atomic size_t` counter array sized at module finalization (`N = next_clone_id`), and bump `__gorget_clone_count[id]` at each call. The atexit handler then emits a per-id breakdown after the aggregate line when `--clones=verbose,stats` (or `all`). Design sketch: (a) record `clone_id_at_call: FxHashMap<InstId, CloneId>` on `LirModule` populated by the clone-emit sites in `ensure_owned_at_boundary` / `clone_fn_for_ptr` callers; (b) c_lir's `emit_extern_call` consults the map and pre-pends `__gorget_clone_count[<id>]++;` to the call expression; (c) c_runtime emits `static _Atomic size_t __gorget_clone_count[N] = {0};` from `LirModule.next_clone_id`. Blocker today: each warning is emitted at `warn_implicit_clone` time but the corresponding runtime call is materialized later (LIR lowering); the link between (warning id) and (LIR instruction) is not currently captured. The cleanest fix is to make `warn_implicit_clone` return the new `CloneId`, then thread it through `ensure_owned_at_boundary` to the LIR Inst::CallExtern as a typed sidecar — substantial plumbing but mechanical. Estimate: 1-2 days. [added: 2026-05-17, follow-up to --clones unification]

- **LSP server.** `language-design.md` lists LSP as a design target but nothing ships today. The gorget-js agent flagged this as the single biggest developer-experience gap: estimated 50% of their loops were "what's that field again?" / "how is this method spelled?" — for a language with rich types, no autocomplete/hover/go-to-def is a heavy accessibility loss. Biggest payoff, biggest investment among the gorget-js critique items — don't start until the smaller papercuts (lint:suggest_throws, import aliasing, parser fixes, stack traces, --clones=verbose) are addressed; those are days each, LSP is multi-week. The semantic analyzer already builds a complete typed scope tree (`ScopeTable` + `function_info` + `TypeRegistry`) — the foundation is there; the work is the LSP-protocol layer, file-watching for incremental updates, and the inevitable "what does Gorget's hover-on-`x.method` look like in practice" design pass. [added: 2026-05-12, from gorget-js critique]

- **`panic` as builtin — option (a) follow-on: retire the hardcoded `gorget_panic` lowering at `assert`.** Option (b) shipped 2026-05-13: `panic(msg)` is callable from user code, typechecks as Never (compatible with any expected type), and registers `gorget_panic` in `noreturn_fns` for indirect call paths. The hardcoded `call_extern("gorget_panic", …)` at `src/ir/lowering/stmts/mod.rs:2132` for `assert` lowering remains. Option (a) (layering-discipline-correct answer) would: declare `panic` in a stdlib module as `extern noreturn void panic(String msg)`, route the `assert` failure path through a normal `panic(msg)` call, retire the name-match. Defer until the prelude / auto-import machinery is fit for purpose (today only enum variants prelude-import — `panic` needs to be globally available without `from std.X import panic`). Also audit `lib/freestanding/runtime.c` and `c_runtime.rs` for any other `_Noreturn` C functions exposed to Gorget (likely none today). [revised 2026-05-13 — option (b) shipped; option (a) deferred behind prelude work]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

## Low

- **`gorget_string_copy_cow` 3-byte leak from f-string interpolating a String read through nested `Vector[Vector[String]]`.** Surfaced 2026-05-17 by the gorget_array_pop investigation (commit `4b3392a8`). Shape: `Vector[Vector[String]] outer = ...; print(f"{outer.pop().unwrap().get(0).unwrap()}")` leaks 3 bytes per evaluation. NOT a pop bug — the same leak reproduces without pop, e.g. `print(f"{outer.get(0).unwrap().get(0).unwrap()}")`. Localised to the f-string-interpolation lowering site: the implicit `gorget_string_copy_cow` (or similar) for the `{}` slot's String argument keeps the copy live past the f-string assembly, but the assembly path doesn't drop the temporary. Likely fix: trace `lower_fstring_interpolation` (or the AST→GIR rewrite for `EFString`) — confirm every interpolated expression's temporary is paired with a drop after the final concat. The 3-byte leak suggests one of the intermediate String COW headers (24-byte header + 3 bytes of "abc" payload that survives) — verify with LSan and grep COW emit sites for the missing drop. Estimate: half-day. [added: 2026-05-17, found while investigating the parser-claimed gorget_array_pop bug that didn't reproduce]

- **Clone reduction — 3 deferrable sites (audited 2026-05-16, kept)**: (1) `ensure_owned_at_boundary` struct-field init clone of Ptr(resource) (`context.rs:~1631`) → would need scope-escape check on the struct's lifetime, (2) Ptr-binding auto-clone at `lower_var_decl` (`stmts/mod.rs:~675`) → could defer to first mutation but needs mutation tracking across the no-clone span, (3) string field extraction in Constructor pattern (`stmts/patterns.rs:~937`) → needs per-arm escape analysis tracking returns / struct stores / captures. Each escape check is >30 lines of new logic at the consume site. Per CLAUDE.md ("fix complexity as signal of wrong layer"), the right fix is upstream — add typed escape metadata to the AST/GIR — that's a far bigger plumbing change than the marginal gain justifies. Audit of all 952 fixtures still showing max 5 implicit clones per fixture, all at necessary ownership boundaries. Audited and kept; re-evaluate if a future escape-analysis pass adds the typed metadata for free. [audited 2026-05-16; demoted from High 2026-04-09]

- **Self-host LIR backend**: ~6,200 lines across 4 files. 687/936 fixtures compile (was 462 baseline; net +225 over two sessions). 0 crashes. Key fixes across sessions: (1) SlotStore type-mismatch coercion — scalar→aggregate and aggregate→aggregate both emit `{0}` zero-init; (2) runtime fn return types — gorget_args/env_vars/cwd/str_to_upper/lower/char_at/byte_slice/int_to_str/float_to_str/bool_to_str all correctly typed; (3) runtime_arg_is_str table coerces pointers/scalars at Str parameter positions (str_cat/eq/cmp etc.); (4) ICmp narrowed to GorgetString plus memcmp fallback for struct==struct; (5) generic placeholder + enum variant filtering in type_defs; (6) bare opaque/prelude type constructors (TaskGroup, AtomicInt, Box, Shared, …); (7) is_type_constructor excludes primitive coercions; (8) post-gmod fn_sigs pass covers functions + equip methods; (9) extern time/time_ms/format_time/parse_time mappings; (10) Option/Result combinator takes address of aggregate src; (11) drop/clone forward declarations prevent static-after-implicit conflicts; (12) enum_variant_parent routes bare variant constructors to parent enum type; (13) Str/String/GorgetString identity coercion (Str("x") → x); (14) imported IEnum merged with __imported_type__ marker (skips drop/clone regen); (15) TFunction param ABI is Ptr(FnPtr) instead of unit — closure params now get pointer passing; (16) static method calls on type identifiers (Point.default(), int.parse(s)); (17) operator overload (+/-/*/div/rem/neg/==/!=/<=/>=) dispatches to TypeName__method for user structs, including monomorphized instances; (18) gorget_str_strip arity padding. Remaining ~249 failures: Str-as-int casts in JSON/XML/TOML parsers (b64_char_value), imported-struct field access (needs IStruct loader merge without drop conflicts — tried, regresses), DataFrame col_slice with Column placeholder types, Vector[T](alloc=…) keyword args, throws/Result auto-wrapping, SSA phi gaps (unassigned block params). [updated: 2026-04-17]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]


- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]

- **`Weak[T]` cycle-breaking — missing primitives (canonical fixtures landed, all `#[ignore]`d).** Filed four ignored fixtures: `tests/fixtures/weak_cycle_shape_{a,b}_{positive,negative}.gg` documenting the canonical interior-mut and parent/child topologies, registered as `#[ignore]` in `tests/integration.rs` with expected stdout that reflects what the language *should* produce. The cycle-breaking claim in `docs/book/16-smart-pointers.md:109-127` and `docs/language-reference.md:529-531` is currently **untestable end-to-end** because every path to forming a real Shared cycle is blocked: **(1) `Cell[T]` is documented in the book as the interior-mutability primitive for Copy types, but it does not exist in the resolver** — `Cell[int] c = Cell[int](42)` fails with "undefined name `Cell`". Same for `RefCell[T]`. They are listed in the language reference (§4.5 Smart Pointer Types) but never reached the runtime. **(2) `Shared[T]` exposes no mutability path for arbitrary struct fields**. The only mutation through `Shared` that works today is `gorget_shared_array_set` (`Shared[Vector[T]].set_at`) — there is no `Shared.mutate(closure)`, no `Shared[Mutex[T]].lock()` returning a `Guard[T]` whose field-writes propagate (compiles, but `Guard__Struct field_assign` codegens broken C: `error: incompatible types when assigning to type 'Guard__T' from type 'int32_t'`), and `Guard[Vector[T]]` lacks auto-delegated `push`/`len` (undefined references at link time). **(3) `p.children.push(c)` for `p: Shared[Parent]` silently no-ops**. Compiles, runs, but the push targets a discarded copy of the inner struct; `p.get().children.len()` reads 0 after a sequence of pushes. No warning, no error, just data loss — this is the worst of the three because it gives the false impression that mutation through `Shared` works. **What needs to land**: any ONE of {`Cell[T]` runtime impl, `RefCell[T]` runtime impl, `Shared[Mutex[T]].lock()` codegen fix for `Guard[Struct]` field-assign, `Shared.mutate(closure)` API} unblocks at least one fixture shape. Until then, the book's cycle-breaking claim remains aspirational. The four ignored tests assert the canonical expected behaviour (`leaked=true` for the all-Shared control, `leaked=false` for the Weak-broken positive case) — when a primitive lands, removing the `#[ignore]` is the contract that the cycle-breaker actually breaks cycles. [updated: 2026-05-18, was added: 2026-05-17]
