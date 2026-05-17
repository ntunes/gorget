# TODO

## Profile snapshot 2026-05-16

**Status after 2026-05-17 cleanup batch**: 4 of 5 top-candidates shipped — codegen perf win (`962ae144`, Agent 1, ~56× LW codegen), `gir_lower` instrumentation (`3dfc9916`, Agent A), `propagate_copies` FxHashMap (`469d7942`, Agent 5, ~20%), `load_imports` rayon parallelization (`107ab8dc`, Agent 4, ~31%). Plus the unrelated parallel wins: `validate_resource_*` walks collapsed into one (`4b529742`, Agent 3, ~75% on that family) and LLVM backend stack-traces parity (`f1d54193`, Agent 2). Remaining actionable from this snapshot: **`lir_ssa`** (#5, structural, deferred) and **`meta_consts`** (honorable mention, no investigation yet). A fresh snapshot post-batch would be a better baseline than reusing this one for the next round.

Fresh `gg profile` snapshot taken on `gorget-1` HEAD (a35ea90d) after the recent LW 3818ms→600ms wins. Goal: identify the **next** dominant phase **outside** the known/handled zones (drop_elab, semantic name-index, safety borrow-state-reset).

**Workloads profiled** (release build):

| Fixture                                             | total_ms | LIR insts | C lines |
|-----------------------------------------------------|---------:|----------:|--------:|
| `hello.gg` (baseline, 2 LOC)                        |    24.0  |         4 |   2 610 |
| `generic_nested_collections.gg` (15 LOC)            |    21.7  |       112 |   2 846 |
| `yaml_parse.gg` (453 LOC, single file)              |    38.8  |    18 417 |  44 923 |
| `httpserver_router_extended.gg` (75 LOC + imports)  |    66.7  |    20 258 |  27 153 |
| `self_host_typechecker/driver.gg` (~12k aggregate)  |   254.1  |    76 784 | 196 154 |
| `self_host_lowerer/driver.gg` (~30k aggregate)      |   798.0  |   197 119 | 540 202 |

The self-host lowerer dominates and is the right magnifying glass for the next round.

**Phase breakdown on `self_host_lowerer/driver.gg` (798ms)** — sorted by absolute time, with KNOWN/HANDLED rows annotated:

| Phase                              | ms     | % total | Notes |
|------------------------------------|-------:|--------:|---|
| `lir_optimize` (total)             | 226.3  | 28.4%   | of which `drop_elaboration` 141.4 (KNOWN), `propagate_copies` 18.6, `eliminate_dead_code` 13.3, `post_elab_dce` 5.9, `fold_constants` 5.0, `cse` 5.0, `eliminate_dead_functions` 5.0, `merge_linear_blocks` 3.0, `simplify_algebraic` 2.7, rest <2ms |
| `gir_lower`                        | 155.2  | 19.4%   | Monolithic — **no sub-pass timing today** |
| `codegen` (c_lir)                  | 136.3  | 17.1%   | C string assembly + serialization |
| `semantic` (total)                 |  86.5  | 10.8%   | of which `meta_consts` 33.4, `typecheck_module` 23.1, `safety_check_module` 19.5 (KNOWN — borrow-state-reset), `safety::check_items_recursive` 16.4 (KNOWN), `resolve_bodies` 5.5, `collect_top_level` 3.0, rest <2ms |
| `load_imports`                     |  67.1  |  8.4%   | File I/O + parse for each imported module |
| `lir_ssa`                          |  44.2  |  5.5%   | Critical-edge split + SSA construction (dominators, phi insertion, renaming) |
| `lir_lower`                        |  30.9  |  3.9%   | GIR → LIR translation |
| `gir_optimize`                     |  27.0  |  3.4%   | dead_drop / nop_elim / dead_block / dead_store passes |

**Top-5 cheap-win candidates (excluding drop_elab, semantic name-index, safety borrow-state):**

1. **`codegen` (136ms on lowerer, 37ms on typechecker, 7.5ms on http_router) — `src/backend/c_lir/`. CHEAP-WIN likely.** C emission scales with C line count (540k lines on lowerer). Almost certainly dominated by `String::push_str`, `format!`, and small-buffer growth. Cheap wins: pre-sized output buffer (peak is ~5MB of C — one `with_capacity(8 << 20)` saves dozens of reallocations), `write!` to a `Vec<u8>` instead of intermediate `format!()` returns, hoist any per-instruction `HashMap` lookups out of the hot path. Caveat: `src/backend/` is in the other-agents zone — flagged for that agent's next pass, not for direct work here. [priority: high, ~1-3 day investigation]

2. **`gir_lower` (155ms, no breakdown) — `src/ir/lowering/`. MEDIUM, profile-blind today.** This is the second-largest phase but ships as a single number. First action is **instrument the phase** — split into monomorphization, drop-insertion, closure synthesis, type lowering. Without sub-pass timing the cheap-win surface is invisible. `src/ir/lowering/` is in the other-agents zone — coordinate the instrumentation patch. [priority: high — unblocks targeted optimization, ~half day to add timings]

3. **`lir_optimize` non-drop-elab tail (~85ms) — `src/lir/optimize.rs`. CHEAP-WIN.** Confirmed: `propagate_copies` (18.6ms) uses default `std::collections::HashMap` (lines 1306/1343/1344/1396/1502/1529 grep-confirmed SipHash). A drop-in `rustc_hash::FxHashMap` swap is the canonical 2-5× hashing win and should shave several ms on lowerer. Similar audit needed for `cse` (uses `HashMap<CseKey, ValueId>` at line 994 — already a candidate). Each of the 11 fixpoint sub-passes also walks every block + every inst; a one-pass merge of `eliminate_dead_code` + `post_elab_dce` + `propagate_copies` (they all touch the same data) is worth scoping. `src/lir/` is in the other-agents zone. [priority: medium, FxHashMap swap is literally a `use` change + type aliases, ~1 hour]

4. **`load_imports` (67ms on lowerer, 39ms on typechecker, 19ms on http_router) — `src/loader.rs` (NOT in any forbidden zone). CHEAP-WIN.** Scales linearly with import count; serial today. Two independent wins: (a) `rayon::par_iter` the per-import file-read + parse — embarrassingly parallel and parses are CPU-bound at ~1-5ms each, ~3-4× speedup on multi-file drivers; (b) inspect for redundant re-reads of the same path (transitive imports). On self_host_lowerer (~30 imports) this could drop 67ms → ~20ms. **This is the only top-5 candidate in a non-forbidden zone — best fit for the next direct-edit agent.** [priority: medium-high, ~half day]

5. **`lir_ssa` (44ms on lowerer, 13ms on typechecker) — `src/lir/ssa.rs`. MEDIUM, likely structural.** SSA construction is dominator-tree + phi-insertion + renaming. Standard Cytron-style implementations are O(n α(n)); the cost is mostly intrinsic to the work. Cheap-win surface: check whether `Bitset` / `IndexVec` are used for dominator frontiers vs `HashSet<BlockId>`, and whether the renaming walk allocates per-block scratch maps. If using stdlib HashMaps anywhere, FxHashMap swap. Otherwise structural. `src/lir/` is in the other-agents zone. [priority: medium]

**Honorable mention — `meta_consts` (33.4ms in semantic, lowerer):** `src/semantic/meta.rs:440 evaluate_meta_consts`. This is the single largest semantic sub-pass outside the known-handled set. Hypothesis: re-evaluates the same `meta` constants each module load (no memoization). `src/semantic/` is in the other-agents zone, but worth flagging — a 30%+ reduction here would clip ~10ms off any non-trivial build.

**Where I'd point the next optimization agent:** `load_imports` parallelization is the standout — it's a 67ms phase on the worst-case workload, it's in a non-forbidden zone (`src/loader.rs`), the win is mechanical (wrap the existing per-import loop in `rayon::par_iter`), there are no layering-discipline traps, and the speedup is bounded only by the host's core count and the slowest single-file parse. Estimated 40-50ms saved on `self_host_lowerer/driver.gg` alone (~6% off total compile), with proportional wins on every multi-file fixture. The FxHashMap swap in `lir/optimize.rs::propagate_copies` is the smallest-touch alternative (1-hour patch, a few ms shaved) and pairs well as a warmup. [filed 2026-05-16]

## High

- **LIR optimizer `eliminate_dead_code` per-iteration allocation churn — replace `Vec::with_capacity` / `mem::take` shuffle with in-place retain.** Discovered during the 2026-05-17 profile sweep that produced the GIR `Liveness` bitset win. `src/lir/optimize.rs:483 eliminate_dead_code` is now the **2nd-largest LIR opt sub-pass** on the self-host lowerer (26.5 ms, up from 13.3 ms at the 2026-05-16 snapshot — almost certainly because more passes now make progress and re-trigger this one on the next fixpoint iteration). Per call it: (a) builds a `use_count: Vec<u32>` over all values (linear scan + extension count), (b) builds a `keep: Vec<bool>`, (c) `mem::take`s `block.insts` and `block.span_map` into temporaries, (d) allocates two NEW vecs with capacity, (e) re-pushes the kept entries. With 695 functions × ~10 fixpoint iterations × ~20 blocks, that's tens of thousands of redundant vec allocations per build. The principled fix is an in-place `Vec::retain` (or `retain_mut`) that walks both `insts` and `span_map` in lockstep — Rust's stdlib provides this directly and never reallocates. Caveat: `span_map` and `insts` are parallel arrays of (potentially) the same length, and `Vec::retain` doesn't natively zip two vecs; the cleanest shape is to either (i) precompute a `keep[]` mask and then call `retain` on each with an index closure, or (ii) interleave the data into a single `Vec<(Inst, Option<Span>)>` upstream and let `retain` do the work for free. Estimate: ~50 LOC, ~10ms saved on LW driver, proportional saving everywhere. Cited in 2026-05-17 profile commit. [added: 2026-05-17]

- **LIR optimizer `cse` HashMap → FxHashMap swap.** `src/lir/optimize.rs:1002 eliminate_common_subexpressions` allocates a fresh `HashMap<CseKey, ValueId>` per block per fixpoint iteration, using stdlib SipHash. Same mechanical swap as the 2026-05-16 `propagate_copies` win. The per-pass cost is small (4.5ms on LW), so this is a sub-millisecond win — but it's the only stdlib HashMap left on the LIR optimize hot path and the swap is literally a `use rustc_hash::FxHashMap` + type change. Pair-task: same swap inside `fold_constant_branches` if it uses any HashMaps (haven't grep-confirmed). Estimate: 5 minutes. [added: 2026-05-17]

- **Gorget-arena snag #1 — `String s = expr as String` followed by `s = s + ...` panics Tier 2a (AssignIntoOwnedSlot, untracked source).** Discovered while bringing `target/gorget-arena` up to date (2026-05-16). Surface: `gg check` passes, but `gg build` / `gg profile` panic with `Tier 2a consume-site violation: ... AssignIntoOwnedSlot(dst: GorgetString) — untracked source consumed (ownership not decided)` at the binary-`+` assignment site. Workaround in arena: drop the redundant `as String` cast (`char_at(i) as String` → `char_at(i)`) — `String__char_at` already returns String so the cast is a no-op at the language level, but the lowering of `as Type` to a self-same-type loses the source's ownership tag and the next mutating consume of the variable trips the Tier 2a validator. Minimal repro:
  ```
  void main():
      String s = "x" as String
      s = s + "y"
      print(s)
  ```
  Likely owner: `src/lir/lower/insts.rs` ~line 257 (`is_str_source` branch that handles `T as T` no-op for GorgetString). The no-op `as T` cast path should propagate the source local's `Owned/Borrowed/...` ownership through to the destination instead of emitting an untracked produce. Pure same-type cast → identity assign is the simplest fix; broader fix is to make the as-cast lowering propagate ownership on the "source is already T" path. [added: 2026-05-16, found in arena's `client/ui/console.gg::split_tokens`]

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

- **Lint: suggest `throws E` when `match Result: case Ok(x): x; case Error(e): return Error(e)` appears.** Surfaced by the gorget-js agent's critique (2026-05-12): a capable agent built a 4500-line interpreter and only retroactively realised that `throws E` + auto-propagation would eliminate ~40% of its match-Result boilerplate. The docs cover this, but the path of least resistance for someone coming from Rust/Go is `match Result`, and the compiler has every signal to flag it. Detection shape: in the typecheck pass, walk every `Stmt::VarDecl` / `Stmt::Assign` whose RHS is an `Expr::Match` over a Result-typed scrutinee, with exactly two arms `case Ok(x): x` and `case Error(e): return Error(e)` (or a single-target rethrow shape). Emit a `Suggestion::ThrowsRefactor` diagnostic pointing at the enclosing function with the message "this match-unwrap-or-rethrow pattern can be replaced by declaring `<enclosing fn> throws <E>` and writing `T x = <expr>` directly (auto-propagation)". Only fire when the enclosing function returns `Result[T, E]` for some `T` (otherwise the throws conversion would be a non-local refactor). Highest-ROI item from the critique — closes the discoverability gap that fooled an LLM with docs access AND feeds directly into the eval.gg cleanup pass the user is doing. Estimate: 2-3 days incl. the diagnostic infrastructure for opt-out via `# allow: suggest_throws` or `--no-lint=suggest_throws`. [added: 2026-05-12, from gorget-js critique]


- **Runtime-side panic locations** (~50+ sites in `src/backend/c/c_runtime.rs`): explicit follow-on from stack-traces Phase 3. Compiler-side emit now produces `file:line:col: <msg>` for inline traps (Add/Sub/Mul/Div/Mod/Rem/Shl/Shr/BoundsCheck/DivCheck/Trap + unwrap_err combinator). But the runtime helpers — `gorget_array_index_oob`, `gorget_str_*_oob`, channel-closed sends, alloc failures, `gorget_panic("integer overflow")` for runtime `__builtin_*_overflow` paths at c_runtime.rs:5042/5047/5052, and ~45 more — are called *from inside other runtime functions* (e.g., `gorget_array_index_oob` is called from `gorget_array_get`) with no caller span available. They currently fall back to `<unknown>:0:0:` via the `gorget_panic` wrapper. Proper fix needs per-runtime-function span plumbing: every runtime entry point that can panic grows `(const char* file, int line, int col)` first three params, and every compiler emit site that calls it passes the LIR instruction's span. Surface is much larger than Phase 3 (every `gorget_array_get`, `gorget_str_at`, `gorget_chan_send`, etc. call site in c_lir's emit). Estimate: 5-8 days; touches every runtime panic helper signature + every compiler emit site that calls them. [added: 2026-05-17, from stack-traces v1 completion]

- **`caller_location()` builtin + multi-frame stack walking** [LOW PRIORITY]. Two future-direction items explicitly deferred from stack-traces v1. (a) **`caller_location()` builtin**: today gorget-js's internal helpers (`member_lookup(&realm, base, "length", 0, 0)`) take `line, col` params that callers thread through. A `#[track_caller]`-like attribute or implicit `caller_location()` builtin would let helpers inherit the actual call site without manual plumbing. Needs frame-walking discipline at GIR call lowering: the call site's span becomes the *callee's* `caller_location` at the topmost user frame. Plumbing: typed sidecar on `CallExtern`/`Call` carrying caller span, read by the builtin at the helper's body. (b) **Multi-frame stack walking**: today panics print one frame (the throw site). A full backtrace needs DWARF / libunwind plumbing in the C backend, or LLVM's native stackmap. Async / spawned-task panic locations also need the spawning call's span carried through spawn context construction. Both are multi-week investments; defer until v1 is battle-tested and the demand is concrete. [added: 2026-05-17, deferred from stack-traces v1]

- **`--trace-cow` flag: dump the CoW analyzer's clone-insertion decisions.** Diagnostic gap. "CoW: r mutated while o holds an element — clone is inserted automatically" appears at dozens of sites; the agent had no way to inspect which clone, of what type, at what cost. The IR-lowering's clone-insertion decisions are concentrated in a handful of sites (`ensure_owned_at_boundary`, `clone_fn_for_ptr`, `coerce_null_to_option_none`, etc.); instrumenting them to emit `(span, reason, type, size_bytes, runtime_fn)` tuples is straightforward, then `--trace-cow=cow.log` dumps the table. Closes the "trust the analyzer" adoption-confidence gap. Estimate: medium, ~3-5 days. [added: 2026-05-12, from gorget-js critique]

- **Typed `Diagnostic` struct in the self-host — replace `Vector[String] errors` accumulator.** Today every resolver / typechecker / lower / validate stage of the self-host pushes free-form strings into a `&errors Vector[String]` out-param. Span / severity / category / source-snippet are baked into the message text (when present at all), so callers can't filter, sort, group, or render them differently. A typed `struct Diagnostic { Span span; String message; Severity sev; DiagKind kind; }` would (a) keep the accumulator pattern intact (no `throws`-style refactor needed; pairs cleanly with [[feedback-selfhost-snag-numbering]]'s "consistent over idiomatic" rule), (b) let CLI consumers add `--format=json` / `--filter=warn` flags trivially, (c) be the natural carrier for the lint-suggestion text in the in-tree `lint:suggest_throws` TODO and the per-site span+reason tuples for `--trace-cow`. Estimate: multi-day; touches `Vector[String] errors` at every push site (~80 sites across self_host_typechecker / self_host_resolver / self_host_lowerer) and every drain/render site on the driver side. Order of operations: introduce `Diagnostic` + `Severity` + `DiagKind` in a new self-host module first, migrate one stage (resolver is smallest) end-to-end as proof-of-concept, then sweep the others. Pairs with the sentinel-elimination project below — both are structural correctness wins, neither is cosmetic. [added: 2026-05-14, from idiomatic-restructure landscape audit]

- **Result→T auto-propagation — retire the residual consumer-side `maybe_auto_propagate` safety nets.** The producer-side centralization shipped 2026-05-15 (see DONE.md) — `lower_expr` now applies `maybe_auto_propagate` automatically when the expression is a `Call` / `MethodCall`, plus the matching typechecker auto-prop gates for if/while/elif/index. Snag #49 holdouts (for-iter, if-cond, while-cond, index) closed; new consumer sites that lower a Call to a Result-returning fn auto-prop without any explicit hook. The seven consumer-side `maybe_auto_propagate` calls left in tree (`stmts/assigns.rs:112`, `stmts/mod.rs:129/317/1422`, `calls.rs:151/1030/1065/1206`, `methods.rs:224`, `exprs/mod.rs:1788/1823/1854`, the match-scrutinee fallbacks) all exist for the *Identifier-of-Result* case: `Result[T,E] r = ...; T x = r;` (RHS is an `Identifier`, not a producer). The producer-side hook can't fire on identifiers without breaking `body_r.unwrap()`-style code (the receiver MUST stay `Result` for `.unwrap()` to dispatch correctly). The TODO's principled fix — plumb the typechecker's `expr_types` map (the post-auto-prop semantic type per span) through monomorphization into IR-lowering, then at `lower_expr` exit check `op_type` vs `expr_types[span]` and auto-prop only when they disagree in the throws-sugar direction — would let us retire all seven safety nets. Estimate: 1-2 days, mostly the monomorphization plumbing (substitute_expr_types-shaped pass for spans), then one-line decision at the hook. The current state is the producer-side ~80%-covered fix the TODO calls out; the residual 20% (Identifier-of-Result destinations) remains belt-and-suspenders until the principled fix ships. [demoted: 2026-05-15, post Snag #49 closure]

- **Deferred String materialization — Site #4 (borrow-checker decidability)** [LOW PRIORITY] (filed 2026-05-04, sites #1 + #3 closed 2026-05-05/06, site #2 retired as theoretical 2026-05-11). The lifetime question — "can we statically prove `x` doesn't outlive `source`'s last possible mutation?" — needs a separate design pass. Today's heuristic (`is_cow_unsafe_at(name, span)` for reassignment-on-forward-path) catches the common case but isn't lifetime-aware. Defer to a dedicated session.




- **Residual: `Option[Box[T]]` / `Result[Box[T]]` field drops not emitted on enum variants and struct fields.** The Box-field-drop wrapper `Box__T__drop` and its wiring at struct/enum-variant scope-exit (cases a + b + c of the prior Box[T] item) were closed 2026-05-01 — see DONE. The Option/Result ENUM-VARIANT skip at `populate_recursive_drop_enums` (mod.rs:471-481) and STRUCT-FIELD skip at `populate_recursive_drop_structs` (mod.rs:412-422) was kept intentional: enabling the drop crashes the self-host `resolve_stmt` path because `stmts.get(i).unwrap()` (resolve.gg, post-inline 2026-05-10) returns `Stmt` by value — a shallow copy that aliases the vector's interior box/string pointers; both copy and source drop, and dropping the `Option[SpannedExpr]` field inside `Stmt` double-frees the SpannedExpr's Expr/string that the standalone SpannedExpr drop already freed. The proper fix is at the COMPILER level: make `Vector[T].get(i)` for resource T auto-clone (deep) or return `Ref[T]`-only (forcing the caller to .clone() at the boundary). Once that lands, the Option/Result drop skip can be removed and `option_box_enum.gg`'s 3 leaked Some(Box(...)) blocks will free correctly. Today: leak (3 blocks for option_box_enum), not unsoundness. [added: 2026-05-01, refreshed citation 2026-05-10]

- **Self-host silent-fallback audit — IN PROGRESS**. Diagnostic wiring shipped (commit af0cb513): three sites now emit `/* [bug] ... */` comments in the generated output instead of silently returning sentinels: (a) `map_binop` unknown operator, (b) `EIdentifier` unknown name, (c) `EFieldAccess` unknown field. Also added `map_compound_binop` for `+=` / `-=` / ... spellings (commit 299ffb0c — was the root cause of `last_us -= 1` → `last_us += 1`). Still to audit / tighten: `infer_method_return_type` I64 fallback for unknown methods, `collection_element_type` "" fallback for unknown prefixes, `type_id_to_name` "int64_t" fallback for non-GtNamed tids. Attempted a cap on `[bug]` emission count but stage-1's lowerer doesn't reliably propagate mutable global state so the counter stayed at 0 — reverted to loud-by-default; callers can filter via `| grep -v '\[bug\]'` and dedupe via `sort -u | uniq -c`. Proper env-var toggle (`GORGET_QUIET_FALLBACKS=1`) deferred until the env-var reader lands in the self-host. [revised: 2026-04-24]

- **Self-host stage-1 hot-path performance**: `self_host_bootstrap_fixed_point` now passes (~200s for the full stage-0 → stage-1 → stage-2 chain as of 2026-05-10), so the previously-cited stage-1-on-driver.gg SIGSEGV class is closed. The cited Rust-side hotspot — `ScopeTable::lookup_within_function`'s O(N_defs) linear scan over the module-wide definition vector — was closed 2026-05-16 via an incremental name → DefIds index in `src/semantic/scope.rs`, yielding 2× speedup on the LW semantic phase (375→190 ms, total compile time -22%). Self-host's mirror of this lookup, if it has one, may need the same treatment when stage-1 timings become a concern again. [revised: 2026-05-16]

- **Stdlib narrow waist — Phase 2c residual items**: (a) (2) impl-override sig substitution **SHIPPED 2026-04-29** (see DONE.md). (b) **Builtin Vector HOF expansions cleanup** — both void-return entries have now been retired in favour of their user-space wrappers in `lib/std/iter.gg`: `Vector.each` (2026-04-21 commit 1b0e7022) and `Vector.for_each` (2026-05-16, see DONE.md). The remaining typed-return Vector HOFs (filter / map / fold / reduce / any / all / find / find_index / count / enumerate / flat_map / zip) stay as BuiltinMethodDecl entries — IR-lowering reads their declared return types via `resolve_builtin_method_return_type` when the user-space wrapper's sig hasn't been registered yet (e.g. during early generic mono). Full retirement blocks on a separate signature source for IR-lowering when BuiltinMethodDecl is absent; that's a bigger task. Dict.each / Set.each BuiltinMethodDecls also stay (no user-space wrapper migration yet). LIR `HofOp` variants stay live regardless — they serve Dict / Set too. See design doc §10; `lib/std/iter.gg` is the authoritative source. [revised: 2026-05-16]

- **Self-host check_comparison residual gaps — 8 mismatches** [revised: 2026-05-10; current score 1013/1021 = 99.2%]:
  - **(a) Type-variable preservation** (~5 fixtures: `coroutine_collections`, `generic_pair_swap`, `httpserver_middleware`, `httpserver_router_extended`, `test_vector_bool`, `test_vector_edge_cases`). Rust keeps numbered inference vars (`?5`, `?0`, …) at unresolved closure-param call sites; self-host concretises or emits `<error>`. Architectural difference in how the two infer.
  - **(b) Function-type parser String-aliasing** (1 fixture: `generic_callable.gg`'s `Callable[<error>(int)]`). Same lineage as the long-running self-host parser bug where `int(int) f` (function type as param/return) corrupts the outer return-type's primitive name as bytes from the following identifier.
  - **(c) Misc one-offs** (closure_tuple_destructure, sigil_type_args, copy_struct_closure_capture). Each is a single-fixture quirk.
  Fix path: most gains here are architecturally deep (type-var numbering preservation, parser String-aliasing). The score is stable; further closure may be net-negative ROI vs the layering-discipline migrations in `docs/internals/self-host-resource-model.md`. [revised: 2026-05-10]

- **Cloneable trait for generic bounds**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Runtime counters shipped via `--clone-stats` — atexit line emits `[clone-stats] array_clone=... map_clone=... set_clone=... string_cow=... string_cat=... box_alloc=... ... peak_rss_kb=...`. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`) — ships alongside the next round of ownership work. [updated: 2026-04-21]

- **C backend: retire local val_types/ptr_pointee fixup phases (follow-on after the 2026-05-15 seed migration)**: The C backend now seeds `val_types` and `ptr_pointee` from `func.value_types` and `func.pointee_types` (see DONE 2026-05-15), and pointee_types is computed BEFORE value_types so the shared `infer_inst_type` can fall back through pointee for `Inst::Load { ty: Void }`. The CallExtern→SlotStore slot-type override has also moved upstream (see DONE 2026-05-16). Remaining work is to push the rest of the C-backend-local augmentations upstream so the local pass disappears entirely:
  - Guard accessor inference from consumers (`mod.rs:1381-1456`) — `gorget_guard_get` / `gorget_shared_get` return void* but the inner type can be inferred from the next 10 instructions (arithmetic op, IntCast, printf %f, etc.).
  - Cross-type Option/Result map combinator override (`mod.rs:1465-1486`) — reads `LirExtern.combinator_result_struct_id` to pick the correct result struct.
  - Consumer-driven Add/Cmp peer-type back-propagation (`mod.rs:1490-1570`) — when one operand has type info and the other doesn't, propagate.
  - InlineC→SlotStore type inference (`mod.rs:1306-1322`) — InlineC dst values get the type of the slot they get stored to.
  - Ret-from-function backfill — Ret(value) implies function return type when value is untyped.
  Each of these is *cross-instruction* reasoning. To push them upstream, the shared `compute_module_value_types` would need a fixed-point pass or a multi-phase walk that today only the local pass has. Scope: medium — half-day for any one of them, plus tests. [updated: 2026-05-15, after partial seed migration shipped]

- **Decompose emit_call_extern.rs (~988 lines)**: Tier 1-3 lifts done; HOF cluster lifted 2026-05-16 (Option/Result combinator inlining → `emit_hof.rs`, -200 lines). The original "Vector HOF inline handlers" mentioned in the pre-2026-04-15 entry had already been migrated to LIR's `HofExpand` op (commits `79ab2cc2` and friends). Remaining clusters in `emit_call_extern.rs`: printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-05-16]

## Medium

- **LSP server.** `language-design.md` lists LSP as a design target but nothing ships today. The gorget-js agent flagged this as the single biggest developer-experience gap: estimated 50% of their loops were "what's that field again?" / "how is this method spelled?" — for a language with rich types, no autocomplete/hover/go-to-def is a heavy accessibility loss. Biggest payoff, biggest investment among the gorget-js critique items — don't start until the smaller papercuts (lint:suggest_throws, import aliasing, parser fixes, stack traces, --trace-cow) are addressed; those are days each, LSP is multi-week. The semantic analyzer already builds a complete typed scope tree (`ScopeTable` + `function_info` + `TypeRegistry`) — the foundation is there; the work is the LSP-protocol layer, file-watching for incremental updates, and the inevitable "what does Gorget's hover-on-`x.method` look like in practice" design pass. [added: 2026-05-12, from gorget-js critique]

- **`panic` as builtin — option (a) follow-on: retire the hardcoded `gorget_panic` lowering at `assert`.** Option (b) shipped 2026-05-13: `panic(msg)` is callable from user code, typechecks as Never (compatible with any expected type), and registers `gorget_panic` in `noreturn_fns` for indirect call paths. The hardcoded `call_extern("gorget_panic", …)` at `src/ir/lowering/stmts/mod.rs:2132` for `assert` lowering remains. Option (a) (layering-discipline-correct answer) would: declare `panic` in a stdlib module as `extern noreturn void panic(String msg)`, route the `assert` failure path through a normal `panic(msg)` call, retire the name-match. Defer until the prelude / auto-import machinery is fit for purpose (today only enum variants prelude-import — `panic` needs to be globally available without `from std.X import panic`). Also audit `lib/freestanding/runtime.c` and `c_runtime.rs` for any other `_Noreturn` C functions exposed to Gorget (likely none today). [revised 2026-05-13 — option (b) shipped; option (a) deferred behind prelude work]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

## Low

- **`TypeTable::primitive_id` slow-path is O(N_types)** (`src/semantic/types.rs:222`). The hot fast-path returns cached IDs for Bool/Int/Float/CStr/StringType/Void in O(1), but for Int8/16/32/64, Uint8/16/32/64, Float32/64 it falls through to a linear scan over `self.types` (which grows monotonically with the module: hits a few thousand entries at self_host scale). Callers include `ast_type_to_resolved` for every typed annotation plus `builtin_method_type` for `byte_at`/`to_upper`/`to_lower` (every call site). Fix: pre-allocate IDs for all PrimitiveType variants in `TypeTable::new`, store in a small array indexed by `PrimitiveType as u8` (or expand the field list). Cheap, ~10 LOC, removes a real O(N) loop. Measured impact unclear on self_host_lowerer/driver.gg (1-3% noise), but asymptotic improvement is real and the fix is layering-clean (typed lookup, no name matching). [added: 2026-05-16, found during semantic perf hunt]

- **`TraitRegistry::resolve_method` could use `(TypeId, &str) -> idx` index instead of two-pass scan** (`src/semantic/traits.rs:155`). After 2026-05-16's `trait_impls_by_type` index shipped, `resolve_method` still does TWO passes over the same impl-indices vector (overrides then default fallback) — each impl visited twice when the method isn't found on either path. Could combine into one pass that records "saw matching impl with default trait" and returns it on second-miss. Minor: typical bucket size is small (1-5 entries). [added: 2026-05-16]

- **`TraitRegistry` linear scans by `self_type_name`** (`src/semantic/traits.rs:285,293,305,330,364`). Five methods (`has_any_impl_by_name`, `has_inherent_only_impls`, `has_trait_impl_by_name`, `trait_generic_args_by_name`, `has_method_for_type`) walk every impl filtering by `self_type_name: String`. Used from typecheck for method resolution fallback when `TypeId` doesn't match (cross-module equip blocks) and from meta for `meta if implements(T, Trait)`. Asymptotic win is real but smaller than `trait_impls_by_type` since these paths are colder. Fix: add `FxHashMap<String, Vec<usize>>` parallel index. [added: 2026-05-16]

- **Clone reduction — 3 deferrable sites (audited 2026-05-16, kept)**: (1) `ensure_owned_at_boundary` struct-field init clone of Ptr(resource) (`context.rs:~1631`) → would need scope-escape check on the struct's lifetime, (2) Ptr-binding auto-clone at `lower_var_decl` (`stmts/mod.rs:~675`) → could defer to first mutation but needs mutation tracking across the no-clone span, (3) string field extraction in Constructor pattern (`stmts/patterns.rs:~937`) → needs per-arm escape analysis tracking returns / struct stores / captures. Each escape check is >30 lines of new logic at the consume site. Per CLAUDE.md ("fix complexity as signal of wrong layer"), the right fix is upstream — add typed escape metadata to the AST/GIR — that's a far bigger plumbing change than the marginal gain justifies. Audit of all 952 fixtures still showing max 5 implicit clones per fixture, all at necessary ownership boundaries. Audited and kept; re-evaluate if a future escape-analysis pass adds the typed metadata for free. [audited 2026-05-16; demoted from High 2026-04-09]

- **Self-host LIR backend**: ~6,200 lines across 4 files. 687/936 fixtures compile (was 462 baseline; net +225 over two sessions). 0 crashes. Key fixes across sessions: (1) SlotStore type-mismatch coercion — scalar→aggregate and aggregate→aggregate both emit `{0}` zero-init; (2) runtime fn return types — gorget_args/env_vars/cwd/str_to_upper/lower/char_at/byte_slice/int_to_str/float_to_str/bool_to_str all correctly typed; (3) runtime_arg_is_str table coerces pointers/scalars at Str parameter positions (str_cat/eq/cmp etc.); (4) ICmp narrowed to GorgetString plus memcmp fallback for struct==struct; (5) generic placeholder + enum variant filtering in type_defs; (6) bare opaque/prelude type constructors (TaskGroup, AtomicInt, Box, Shared, …); (7) is_type_constructor excludes primitive coercions; (8) post-gmod fn_sigs pass covers functions + equip methods; (9) extern time/time_ms/format_time/parse_time mappings; (10) Option/Result combinator takes address of aggregate src; (11) drop/clone forward declarations prevent static-after-implicit conflicts; (12) enum_variant_parent routes bare variant constructors to parent enum type; (13) Str/String/GorgetString identity coercion (Str("x") → x); (14) imported IEnum merged with __imported_type__ marker (skips drop/clone regen); (15) TFunction param ABI is Ptr(FnPtr) instead of unit — closure params now get pointer passing; (16) static method calls on type identifiers (Point.default(), int.parse(s)); (17) operator overload (+/-/*/div/rem/neg/==/!=/<=/>=) dispatches to TypeName__method for user structs, including monomorphized instances; (18) gorget_str_strip arity padding. Remaining ~249 failures: Str-as-int casts in JSON/XML/TOML parsers (b64_char_value), imported-struct field access (needs IStruct loader merge without drop conflicts — tried, regresses), DataFrame col_slice with Column placeholder types, Vector[T](alloc=…) keyword args, throws/Result auto-wrapping, SSA phi gaps (unassigned block params). [updated: 2026-04-17]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]


- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]


