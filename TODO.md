# TODO

## High

- **Branch-merging-expression helper (Cluster A from the post-Snag-#39 architectural audit).** Defer. Two existing sites use the discipline ("allocate result_local + multiple branches Move-assign into it + `ctx.set_owned(result_local)` after merge"): `assign_match_arm_to_result` (Snag #31) and `lower_catch_expr` (Snag #38). A third candidate — `build_if_chain_expr` — has different enough structure (plain `assign`, not `assign_mode(Move)`; no per-arm `set_owned`) that a unifying helper is premature. The "set_owned after merge" step is one line at each existing site, documented in place; abstract this when a fourth instance appears, or when `build_if_chain_expr` is upgraded to Move-mode for non-Copy results (currently I64-typed `result_id` survives by luck — runtime-cond if-expr returning non-Copy enum DOES work today, but only because the assign path falls into a compensating route the helper hasn't characterised). [added: 2026-05-12, from architectural audit]

- **Safety-pass branch-divergence audit (Cluster C from the post-Snag-#39 architectural audit).** Defer. The Snag #39 bug 2 fix added `save_branch_state` / `restore_branch_state` around `Expr::Catch` recovery and `Expr::Rethrow` transform — divergence in those sub-expressions no longer leaks past the branch boundary. Audit found no other actively-leaking sites: `DefaultOp` (`x ?? exit(1)`), `BinaryOp::And/Or` (`a or exit(1)`), `OptionalChain` (`obj?.field`) all checked clean because `self.diverged` is only set by `Stmt::Return/Throw/Break/Continue` — not by noreturn-call expressions like `exit()`. If safety-pass divergence tracking is ever extended to noreturn-call expressions in the future (likely accompanies the `panic`/`exit` typed-Never work), audit these expression forms for save/restore around their conditional branches. Also worth considering: upgrade Catch/Rethrow from save+restore to save+merge (using `merge_branch_states`) — preserves recovery-side var moves in the Error path rather than discarding them. Currently a Snag #39 limitation, hasn't bitten anyone. [added: 2026-05-12, from architectural audit]

- **Cross-module global initialiser does NOT execute for stdlib-imported `static`/`public` declarations.** `src/ir/lowering/mod.rs:1196` skips StaticDecls with zero-length spans (`decl.span.start == decl.span.end`) — the dummy-span shape produced when stdlib statics enter via the import path. As a result, `lib/std/math.gg`'s `public float INFINITY = _math_infinity()` never runs its initialiser; the underlying global is left as `__lir_g0 = {0}`. Today this is masked by the IR-lowering hardcoding of `INFINITY`/`NAN` in `module_constants` (kept as a holdout when the `PI`/`E`/`TAU` removal shipped). Fix: stdlib import-side statics should produce real (non-dummy) spans OR the skip predicate should not gate on span shape. Once the global-init bug is fixed, the `INFINITY`/`NAN` hardcoding in `src/ir/lowering/mod.rs` (the residual auto-injection) drops out — `from std.math import INFINITY, NAN` becomes the single source of truth, completing the Layering rule 3 cleanup. [added: 2026-05-10, surfaced when removing the larger PI/E/TAU hardcoding for Snag #29 follow-up #1]

### Self-host showcase blockers

These are Gorget bugs that surface as workarounds in self-host code. Per `docs/internals/self-host-resource-model.md` §0, every workaround in self-host *that exists because Gorget can't express the elegant shape today* is a goal regression: self-host is the demonstration of idiomatic Gorget, so a gap that forces it to be ugly is a blocker, not deferable. Each entry below names the workaround currently in tree, the Gorget-side fix needed, and the self-host re-implementation that follows. Closes when all three have shipped.

### Other High-priority items

- **Self-host: method-level generics on equip methods need call-site type-inference + fn_sigs registration.** Sole remaining iter-chain gap after this session's batch (see DONE 2026-05-15 for the closed gaps: fn_sigs core + transitive discovery + auto-import + generic struct templates). The Option B pre-pass intentionally skips methods with `type_params.len() > 0` (e.g. `void each[F](&self, F f):` inside `equip [T] Vector[T]:`, or `TakeWhileIter[Iter, T] take_while[P](self, P p):` inside `equip [T] VectorIter[T]:`). Bigger than initially scoped — the issue isn't just fn_sigs registration:

  1. **Call-site type inference** — Surface code writes `v.iter().take_while[bool(int)](less_than_3)` or `v.each(callback)`. To monomorphize, self-host needs to combine the receiver's equip-level subs (Vector → Vector[int] → VectorIter[int]) with the method-level `[P]` / `[F]` targs to produce the full mono name like `TakeWhileIter__VectorIter__int64_t__int64_t__bool_int64_t_fn`. Self-host's lowerer has no type inference today.

  2. **Discovery doesn't reach EMethodCall.targs** — `discover_generic_calls_expr` extracts targs from ECall but not EMethodCall (line ~4998). Even if (1) is solved, the mono'd methods need to appear in `generic_instances` for the existing emission paths to fire.

  3. **Return-type chaining** — `take_while` returns `TakeWhileIter[Iter, T]`. The mono'd return type must propagate into the next link of the chain (the next `.iter()` / `.next()` lookup). Today the chain stops at the first call.

  **Symptom of the missing fix**: stage-1 output for `tests/fixtures/iter_lazy_adapters.gg` emits `lower_fail SFor: no iter()/next() chain on type int64_t` (the I64 fallback when EMethodCall return type can't be resolved). Stage-1 output for `tests/fixtures/stdlib_iter_dict.gg` emits `[bug] EFieldAccess` on `TakeIter__DictIter__…__Tuple__…` — the composite mono'd struct's field metadata is missing because the GenericInstance was never discovered. Both fixtures still pass via Rust gg's lowerer; only self-host's stage-1 emit is diagnostic-incomplete. Bootstrap + integration suite remain green.

  **Defer** until self-host gets even partial type inference. Reasonable scope: a small per-method-call resolver that knows the receiver's type from `local_type_name(recv, ctx, gmod)` and combines that with the method's explicit `[targs]` (if present) or with inferred targs from the argument list. The latter is the bigger lift; the former might unblock `iter_lazy_adapters`-shaped fixtures specifically. [updated: 2026-05-15 — gap #1 (generic struct templates) shipped this session; gap #2 deepened scope]

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

- **Import aliasing: `from X import Y as Z`.** Today there's no way to disambiguate same-named imports across modules — the gorget-js agent had to globally rename a variable `exp` because `std.math.exp` would have collided. The Snag #29 follow-up #2 work (commit `9ea5313c`) already errors symmetrically on import-vs-user-def collisions; an `as` clause is the obvious release valve. Parser: add `Y as Z` to the `From` import name list. Resolver: register `Z` as `DefKind::Import` in the current scope while threading `Y` as the source-module name for lookup. Half-day. [added: 2026-05-12, from gorget-js critique; combines with the Snag #29 follow-up #2 resolver work]

- **Stack traces on runtime panics + `caller_location` for proto-aware helpers.** Two related needs that share a fix shape:
  - **Stack traces:** today `gorget: integer overflow` (and similar runtime panics) report no source location. The IR carries spans through the GIR→LIR→C pipeline; the C backend can emit `#line` directives at each LIR-block boundary OR embed a per-panic-site `__FILE__:__LINE__` string that the runtime prints alongside the panic message.
  - **`caller_location` (gorget-js critique 2026-05-13, new item #2):** internal helpers (e.g. gorget-js's `member_lookup(&realm, base, "length", 0, 0)`) take `line, col` params to forward to the panic site for any throwing operation they wrap. The 0, 0 literal passes when the source isn't handy are noisy and write-only. A `#[track_caller]`-like attribute (Rust analogue) or implicit `caller_location()` builtin would let the helper inherit the actual call site without threading the params through.
  Same plumbing solves both: GIR carries the call-site span on every CallExtern/Call; LIR forwards it as a typed sidecar; backend writes it as the C source location at panic-emitting sites. The `caller_location()` builtin reads from the same typed sidecar at the topmost user frame.
  Felt-value enormous — the agent flagged stack traces as a real debugging blocker. Estimate: a week, mostly plumbing the existing span data through the backend's panic-site emission. [added: 2026-05-12, from gorget-js critique; expanded 2026-05-13 with caller_location use case]

- **`--trace-cow` flag: dump the CoW analyzer's clone-insertion decisions.** Diagnostic gap. "CoW: r mutated while o holds an element — clone is inserted automatically" appears at dozens of sites; the agent had no way to inspect which clone, of what type, at what cost. The IR-lowering's clone-insertion decisions are concentrated in a handful of sites (`ensure_owned_at_boundary`, `clone_fn_for_ptr`, `coerce_null_to_option_none`, etc.); instrumenting them to emit `(span, reason, type, size_bytes, runtime_fn)` tuples is straightforward, then `--trace-cow=cow.log` dumps the table. Closes the "trust the analyzer" adoption-confidence gap. Estimate: medium, ~3-5 days. [added: 2026-05-12, from gorget-js critique]

- **Typed `Diagnostic` struct in the self-host — replace `Vector[String] errors` accumulator.** Today every resolver / typechecker / lower / validate stage of the self-host pushes free-form strings into a `&errors Vector[String]` out-param. Span / severity / category / source-snippet are baked into the message text (when present at all), so callers can't filter, sort, group, or render them differently. A typed `struct Diagnostic { Span span; String message; Severity sev; DiagKind kind; }` would (a) keep the accumulator pattern intact (no `throws`-style refactor needed; pairs cleanly with [[feedback-selfhost-snag-numbering]]'s "consistent over idiomatic" rule), (b) let CLI consumers add `--format=json` / `--filter=warn` flags trivially, (c) be the natural carrier for the lint-suggestion text in the in-tree `lint:suggest_throws` TODO and the per-site span+reason tuples for `--trace-cow`. Estimate: multi-day; touches `Vector[String] errors` at every push site (~80 sites across self_host_typechecker / self_host_resolver / self_host_lowerer) and every drain/render site on the driver side. Order of operations: introduce `Diagnostic` + `Severity` + `DiagKind` in a new self-host module first, migrate one stage (resolver is smallest) end-to-end as proof-of-concept, then sweep the others. Pairs with the sentinel-elimination project below — both are structural correctness wins, neither is cosmetic. [added: 2026-05-14, from idiomatic-restructure landscape audit]

- **Result→T auto-propagation — retire the residual consumer-side `maybe_auto_propagate` safety nets.** The producer-side centralization shipped 2026-05-15 (see DONE.md) — `lower_expr` now applies `maybe_auto_propagate` automatically when the expression is a `Call` / `MethodCall`, plus the matching typechecker auto-prop gates for if/while/elif/index. Snag #49 holdouts (for-iter, if-cond, while-cond, index) closed; new consumer sites that lower a Call to a Result-returning fn auto-prop without any explicit hook. The seven consumer-side `maybe_auto_propagate` calls left in tree (`stmts/assigns.rs:112`, `stmts/mod.rs:129/317/1422`, `calls.rs:151/1030/1065/1206`, `methods.rs:224`, `exprs/mod.rs:1788/1823/1854`, the match-scrutinee fallbacks) all exist for the *Identifier-of-Result* case: `Result[T,E] r = ...; T x = r;` (RHS is an `Identifier`, not a producer). The producer-side hook can't fire on identifiers without breaking `body_r.unwrap()`-style code (the receiver MUST stay `Result` for `.unwrap()` to dispatch correctly). The TODO's principled fix — plumb the typechecker's `expr_types` map (the post-auto-prop semantic type per span) through monomorphization into IR-lowering, then at `lower_expr` exit check `op_type` vs `expr_types[span]` and auto-prop only when they disagree in the throws-sugar direction — would let us retire all seven safety nets. Estimate: 1-2 days, mostly the monomorphization plumbing (substitute_expr_types-shaped pass for spans), then one-line decision at the hook. The current state is the producer-side ~80%-covered fix the TODO calls out; the residual 20% (Identifier-of-Result destinations) remains belt-and-suspenders until the principled fix ships. [demoted: 2026-05-15, post Snag #49 closure]

- **Empty-literal `[]` contextual typing in collection value positions defaults to elem_size=8.** Surfaced 2026-05-14 building the Dict.get regression fixture. `Dict[String, Vector[String]] g = {}; g["k"] = []` — the `[]` is parsed as `Vector[<unknown>]` with elem_size=8 (Ptr-shape), independent of the declared `Vector[String]` value type. Subsequent `g.get("k").unwrap().push("Alice")` truncates the Str struct (32 bytes) to 8 bytes, storing only the data pointer; reads return empty Str. Workaround: use the explicit `Vector[String]()` constructor instead of `[]`. The fix is contextual-type propagation: when assigning an empty literal into a slot whose declared type is `Vector[T]`, inherit `T` from context and set elem_size accordingly. Also applies to nested cases like `Vector[Vector[T]] v = [[]]` and struct fields. Affects all resource element types whose size exceeds 8 bytes (Str/GorgetString/GorgetMap/GorgetSet/GorgetArray/user structs). Surfaces as silent data corruption — not a crash, so easy to miss. Estimate: medium, ~1-2 days in semantic/typecheck for the contextual-type inference + IR lowering verification. [added: 2026-05-14, from the Dict.get alignment regression-fixture work]

- **Deferred String materialization — Site #4 (borrow-checker decidability)** [LOW PRIORITY] (filed 2026-05-04, sites #1 + #3 closed 2026-05-05/06, site #2 retired as theoretical 2026-05-11). The lifetime question — "can we statically prove `x` doesn't outlive `source`'s last possible mutation?" — needs a separate design pass. Today's heuristic (`is_cow_unsafe_at(name, span)` for reassignment-on-forward-path) catches the common case but isn't lifetime-aware. Defer to a dedicated session.




- **Residual: `Option[Box[T]]` / `Result[Box[T]]` field drops not emitted on enum variants and struct fields.** The Box-field-drop wrapper `Box__T__drop` and its wiring at struct/enum-variant scope-exit (cases a + b + c of the prior Box[T] item) were closed 2026-05-01 — see DONE. The Option/Result ENUM-VARIANT skip at `populate_recursive_drop_enums` (mod.rs:471-481) and STRUCT-FIELD skip at `populate_recursive_drop_structs` (mod.rs:412-422) was kept intentional: enabling the drop crashes the self-host `resolve_stmt` path because `stmts.get(i).unwrap()` (resolve.gg, post-inline 2026-05-10) returns `Stmt` by value — a shallow copy that aliases the vector's interior box/string pointers; both copy and source drop, and dropping the `Option[SpannedExpr]` field inside `Stmt` double-frees the SpannedExpr's Expr/string that the standalone SpannedExpr drop already freed. The proper fix is at the COMPILER level: make `Vector[T].get(i)` for resource T auto-clone (deep) or return `Ref[T]`-only (forcing the caller to .clone() at the boundary). Once that lands, the Option/Result drop skip can be removed and `option_box_enum.gg`'s 3 leaked Some(Box(...)) blocks will free correctly. Today: leak (3 blocks for option_box_enum), not unsoundness. [added: 2026-05-01, refreshed citation 2026-05-10]

- **Drainable for `Set[T]` / `Dict[K, V]`** — sibling capability trait shipped on Vector 2026-04-27 (O(n) reverse + pop). Set/Dict drain not yet equipped — they'd need a runtime helper `gorget_map_drain_entry(map, idx, out_key, out_val)` that moves the K/V out of the bucket and tombstone-marks the slot so the source's drop doesn't double-free, OR the equivalent move-out-of-collection-slot machinery applied to GorgetMap's bucket array. Today `Set.drain()` / `Dict.drain()` doesn't exist; users wanting drain semantics call `.iter()` + `.clone_each()` (clones every element) or build their own drain iterator over the bucket array. Priority: low until a real consumer needs it. [added: 2026-04-27]

- **Lazy `Dict.keys()` / `.values()`** — residual after Dict/Set lazy iter shipped 2026-04-25. `Dict.iter()` / `Set.iter()` are now lazy bucket-walks via `Ref[Dict[K, V]]` / `Ref[Set[T]]` borrow fields (DictIter / SetIter in `lib/std/iter.gg`). The matching `.keys()` / `.values()` projections still allocate eager `Vector[K]` / `Vector[V]`. Two natural follow-ons now that the borrow-field plumbing exists: (1) `DictKeysIter[K, V]` / `DictValuesIter[K, V]` state-machine structs, same shape as DictIter, that yield only the K (or V) component; (2) thin user-space wrappers — `d.keys()` returns `DictKeysIter[K, V]`, `d.values()` returns `DictValuesIter[K, V]`. Today users get the same effect via `d.iter().map(((K, V) p): p.0)` (verbose). Low priority — eager `.keys()` / `.values()` are unchanged behaviour, the gap is just allocation efficiency, not correctness. [demoted: 2026-04-25]


- **Self-host silent-fallback audit — IN PROGRESS**. Diagnostic wiring shipped (commit af0cb513): three sites now emit `/* [bug] ... */` comments in the generated output instead of silently returning sentinels: (a) `map_binop` unknown operator, (b) `EIdentifier` unknown name, (c) `EFieldAccess` unknown field. Also added `map_compound_binop` for `+=` / `-=` / ... spellings (commit 299ffb0c — was the root cause of `last_us -= 1` → `last_us += 1`). Still to audit / tighten: `infer_method_return_type` I64 fallback for unknown methods, `collection_element_type` "" fallback for unknown prefixes, `type_id_to_name` "int64_t" fallback for non-GtNamed tids. Attempted a cap on `[bug]` emission count but stage-1's lowerer doesn't reliably propagate mutable global state so the counter stayed at 0 — reverted to loud-by-default; callers can filter via `| grep -v '\[bug\]'` and dedupe via `sort -u | uniq -c`. Proper env-var toggle (`GORGET_QUIET_FALLBACKS=1`) deferred until the env-var reader lands in the self-host. [revised: 2026-04-24]

- **Self-host stage-1 hot-path performance**: `self_host_bootstrap_fixed_point` now passes (~200s for the full stage-0 → stage-1 → stage-2 chain as of 2026-05-10), so the previously-cited stage-1-on-driver.gg SIGSEGV class is closed. Residual concern: the typechecker pass is still the dominant cost in the bootstrap timeline. Earlier instrumentation (2026-04-30) showed `typecheck___type_check_stmt` going from ~3min to 10+min when the type graph is more-completely typed (previously short-circuited on I64 fallbacks, which the Phase A typed maps have since closed). Profile and identify the quadratic-or-worse loop. Probably interacts with self-host's CoW emit on widely-shared `&types` borrow. [revised: 2026-05-10]

- **Stdlib narrow waist — Phase 2c residual items**: (a) (2) impl-override sig substitution **SHIPPED 2026-04-29** (see DONE.md). (b) **Builtin Vector HOF expansions cleanup** — `src/ir/lowering/builtins.rs:257-271` look like dead code for the wrapped methods, but only `each` (void return) is actually safe to delete. Typed-return entries provide signature info that IR-lowering reads to declare the function correctly; deletion blocks on a separate signature-source for IR-lowering when BuiltinMethodDecl is absent. LIR `HofOp` variants stay live regardless — they serve Dict / Set too. See design doc §10; `lib/std/iter.gg` is the authoritative source. [revised: 2026-04-29]

- **Self-host check_comparison residual gaps — 8 mismatches** [revised: 2026-05-10; current score 1013/1021 = 99.2%]:
  - **(a) Type-variable preservation** (~5 fixtures: `coroutine_collections`, `generic_pair_swap`, `httpserver_middleware`, `httpserver_router_extended`, `test_vector_bool`, `test_vector_edge_cases`). Rust keeps numbered inference vars (`?5`, `?0`, …) at unresolved closure-param call sites; self-host concretises or emits `<error>`. Architectural difference in how the two infer.
  - **(b) Function-type parser String-aliasing** (1 fixture: `generic_callable.gg`'s `Callable[<error>(int)]`). Same lineage as the long-running self-host parser bug where `int(int) f` (function type as param/return) corrupts the outer return-type's primitive name as bytes from the following identifier.
  - **(c) Misc one-offs** (closure_tuple_destructure, sigil_type_args, copy_struct_closure_capture). Each is a single-fixture quirk.
  Fix path: most gains here are architecturally deep (type-var numbering preservation, parser String-aliasing). The score is stable; further closure may be net-negative ROI vs the layering-discipline migrations in `docs/internals/self-host-resource-model.md`. [revised: 2026-05-10]

- **Cloneable trait for generic bounds**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Runtime counters shipped via `--clone-stats` — atexit line emits `[clone-stats] array_clone=... map_clone=... set_clone=... string_cow=... string_cat=... box_alloc=... ... peak_rss_kb=...`. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`) — ships alongside the next round of ownership work. [updated: 2026-04-21]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]


- **C backend: migrate val_types to func.value_types**: Shared `compute_module_value_types()` runs after LIR optimization and populates `func.value_types`. The LLVM backend already reads from it. The C backend still uses its own single-pass `infer_inst_type` because its multi-phase fixups (guard accessor inference, CallExtern→SlotStore mismatch, cross-type map combinator) depend on `ptr_pointee` context computed in the same pass. Next step: seed the C backend's val_types from `func.value_types` and reduce the fixup phases. [updated: 2026-04-14]

- **LIR value origin metadata — enable Store/SlotStore/Call lifts**: The C backend maintains 5 origin bitmaps (`str_lit_vals`, `null_vals`, `cstr_vals`, `ptr_pointee`, `func_addr_targets`) beyond type info. These track value provenance needed for ~37 emit-decision sites. The type metadata (`func.value_types`) is now shared; origin metadata remains backend-local. Fix: attach origin tags to LIR values (e.g. `StrLit` → string-literal flag, `NullPtr` → null flag, `FuncAddr` → FuncId). Unblocks lifting Store routing (~50 lines), SlotStore string/cstr coercion (~22 lines), and Call/CallPtr ABI coercion (~100 lines). [updated: 2026-04-14]

- **Decompose emit_call_extern.rs (~1,850 lines)**: Tier 1-3 lifts complete — ~490 lines of inline expansion removed. Remaining: HOF inlining (map/filter/each/fold ~590 lines), printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-04-15]

## Medium

- **LSP server.** `language-design.md` lists LSP as a design target but nothing ships today. The gorget-js agent flagged this as the single biggest developer-experience gap: estimated 50% of their loops were "what's that field again?" / "how is this method spelled?" — for a language with rich types, no autocomplete/hover/go-to-def is a heavy accessibility loss. Biggest payoff, biggest investment among the gorget-js critique items — don't start until the smaller papercuts (lint:suggest_throws, import aliasing, parser fixes, stack traces, --trace-cow) are addressed; those are days each, LSP is multi-week. The semantic analyzer already builds a complete typed scope tree (`ScopeTable` + `function_info` + `TypeRegistry`) — the foundation is there; the work is the LSP-protocol layer, file-watching for incremental updates, and the inevitable "what does Gorget's hover-on-`x.method` look like in practice" design pass. [added: 2026-05-12, from gorget-js critique]

- **`is_box` consumer migration deferred** — `TypeMetadata::is_box: bool` field shipped (set in `register_collection_alias` for Box base_name; accessor `TypeRegistry::is_box(type_id)` / `is_box_name(name)`). Probed migrating `name.starts_with("Box__")` consumer sites in `context.rs`, `exprs/mod.rs`, `methods.rs`, `stmts/patterns.rs` to read the typed flag (2026-05-10) — regressed 7 trait-box tests (box_heap segfault, serializable/deserializable build failure, etc.). Root cause: `register_collection_alias` is the Box registration site for AST `Type::Named { name: "Box", ... }`, but cross-module imports (`from std.box import Box`) and other paths register Box TypeDefs via different paths that don't set `is_box: true`. Need to also set the flag at the cross-module import / monomorphization registration sites. Same shape as the `elem_type_to_meta` TODO. Reverted; framework kept for future migration. [added: 2026-05-10]

- **`elem_type_to_meta` collection_kind migration deferred** — `lir/lower/insts.rs::elem_type_to_meta` (`:1941-1949`) routes `Vector__/Deque__` / `Dict__/HashMap__` / `Set__/HashSet__` element names to `ResourceKind::GorgetArray/Map/Set`. Probed migration to typed `gir_types.get_type_def(n).and_then(|td| td.metadata.collection_kind)` reads regressed `vector_task_get` (Got 2, expected 3) on 2026-05-10 — `register_collection_alias` doesn't always register the TypeDef before this path runs (cross-module, monomorph synthetics). The matching `is_monomorphized_wrapper_type` in `c_lir/mod.rs` and `box_inner_drop_fn` already use `struct_def_by_name` to handle this, but the GIR-side path here would need a similar registration-timing guarantee. Same shape as the `opaque_runtime_size` cleanup TODO. Reverted; left as the prefix-match fallback for now. [added: 2026-05-10]

- **`panic` as builtin — option (a) follow-on: retire the hardcoded `gorget_panic` lowering at `assert`.** Option (b) shipped 2026-05-13: `panic(msg)` is callable from user code, typechecks as Never (compatible with any expected type), and registers `gorget_panic` in `noreturn_fns` for indirect call paths. The hardcoded `call_extern("gorget_panic", …)` at `src/ir/lowering/stmts/mod.rs:2132` for `assert` lowering remains. Option (a) (layering-discipline-correct answer) would: declare `panic` in a stdlib module as `extern noreturn void panic(String msg)`, route the `assert` failure path through a normal `panic(msg)` call, retire the name-match. Defer until the prelude / auto-import machinery is fit for purpose (today only enum variants prelude-import — `panic` needs to be globally available without `from std.X import panic`). Also audit `lib/freestanding/runtime.c` and `c_runtime.rs` for any other `_Noreturn` C functions exposed to Gorget (likely none today). [revised 2026-05-13 — option (b) shipped; option (a) deferred behind prelude work]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]


- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

## Low


- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **Self-host LIR backend**: ~6,200 lines across 4 files. 687/936 fixtures compile (was 462 baseline; net +225 over two sessions). 0 crashes. Key fixes across sessions: (1) SlotStore type-mismatch coercion — scalar→aggregate and aggregate→aggregate both emit `{0}` zero-init; (2) runtime fn return types — gorget_args/env_vars/cwd/str_to_upper/lower/char_at/byte_slice/int_to_str/float_to_str/bool_to_str all correctly typed; (3) runtime_arg_is_str table coerces pointers/scalars at Str parameter positions (str_cat/eq/cmp etc.); (4) ICmp narrowed to GorgetString plus memcmp fallback for struct==struct; (5) generic placeholder + enum variant filtering in type_defs; (6) bare opaque/prelude type constructors (TaskGroup, AtomicInt, Box, Shared, …); (7) is_type_constructor excludes primitive coercions; (8) post-gmod fn_sigs pass covers functions + equip methods; (9) extern time/time_ms/format_time/parse_time mappings; (10) Option/Result combinator takes address of aggregate src; (11) drop/clone forward declarations prevent static-after-implicit conflicts; (12) enum_variant_parent routes bare variant constructors to parent enum type; (13) Str/String/GorgetString identity coercion (Str("x") → x); (14) imported IEnum merged with __imported_type__ marker (skips drop/clone regen); (15) TFunction param ABI is Ptr(FnPtr) instead of unit — closure params now get pointer passing; (16) static method calls on type identifiers (Point.default(), int.parse(s)); (17) operator overload (+/-/*/div/rem/neg/==/!=/<=/>=) dispatches to TypeName__method for user structs, including monomorphized instances; (18) gorget_str_strip arity padding. Remaining ~249 failures: Str-as-int casts in JSON/XML/TOML parsers (b64_char_value), imported-struct field access (needs IStruct loader merge without drop conflicts — tried, regresses), DataFrame col_slice with Column placeholder types, Vector[T](alloc=…) keyword args, throws/Result auto-wrapping, SSA phi gaps (unassigned block params). [updated: 2026-04-17]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]


- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]

- **Missing `from module import *` (module-level wildcard glob imports)**: Gorget supports named imports (`from module import X`) and enum-variant globs (`from module import EnumName.*`), but not module-level wildcard imports (`from module import *` — Rust's `use crate::ast::*`). This is the primary reason the five self-host programs use symlinks to share code rather than imports — without glob imports, every shared symbol must be explicitly named, making multi-file sharing impractical for large modules like `ast.gg` (50+ exported types). Implementation would touch: parser (wildcard import syntax), semantic resolver (bind all exported names from the module into current scope), and loader (ensure the module is fully resolved before the wildcard expansion). Prerequisite for self-host unification into a single program with `--stage` flag. [added: 2026-05-09]

