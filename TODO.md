# TODO

## High

- **Self-host typechecker doesn't model `Expr::Block` Never-tail rule + closure return inference** (added 2026-05-06 alongside snag #11 — match-as-expression with block arms).

  After the round-5 fix, the Rust typechecker treats `Expr::Block` whose last statement is divergent (return / throw / break / continue) as type `Never`, and the closure return-type inference falls back to `closure_ret_var` when `body_type == Never` (so the parser's destructure-desugar `Block { ..., Return(expr) }` doesn't mis-specialize tuple-destructured closures as `Closure[Never(...)]`). The self-host typechecker has no analogous rules — its parser stores match arm bodies as `Vector[Stmt]` directly (no `Expr::Block` wrapper to special-case), and walks them through normal statement-checking where `Stmt::Return` doesn't carry an upward "this block diverges" signal.

  No current fixture exercises this divergence — `parser_comparison`, `resolver_comparison`, `type_comparison`, `check_comparison`, `lowerer_comparison` all stay green at HEAD. The gap will surface when a fixture has a VarDecl assignment from a match-expression with a diverging-tail arm AND a typed binding whose type the self-host needs to compute (e.g. `Frontmatter fm = match parsed: case Ok(f): f case Error(msg): print(msg); return`).

  Two paths when it bites:

  1. **Mirror the Rust rule on the self-host side** — add a "diverging tail" check in self-host's match-arm typechecking (it walks `Vector[Stmt]`, so the rule is "if the arm's last stmt is SReturn/SThrow/SBreak/SContinue, the arm produces no value-type contribution; let the other arms determine the match's overall type"). Closure return inference would also need a parallel "skip Never body_type" branch. Maybe ~30 lines across `typecheck.gg` + `infer.gg`.

  2. **Restructure the self-host AST** — add an `EBlock(Vector[Stmt])` variant and route multi-line match arm bodies through it (matching the Rust shape). Bigger refactor; touches parser + AST + resolver + typecheck across all five driver dirs (parser, resolver, typechecker, check + lowerer via symlink). Right answer for self-host parity but multi-week.

  Path 1 is the "fix when it bites" answer. Path 2 is the "self-host should mirror Rust's structure" answer; defer until other AST-shape divergences justify the cost.

  Also: the self-host parser's `parse_match_expr` always wraps single-line arm bodies in a 1-element block (because it always calls `parse_block()`), so it over-introduces a block scope for single-line cases. Harmless today but technically over-scopes pattern bindings' shadowing relative to Rust. Same fix path as above (1 narrow / 2 structural). [added: 2026-05-06]

- **Phase C extension: 3 read-site classes still emit shallow copies; promote to fatal once each lowering migration lands** [added 2026-05-05 by Phase C read-site validator commit; Call/CallExtern args class promoted to fatal in same commit at zero violations]. The validator code is in place; running with `GG_VALIDATE_RESOURCE_READS=<log_path>` surfaces per-class counts to a file. Sweep-of-record (1056 fixtures, 2026-05-05):

  1. **`EnumFieldLoad` shallow copy of resource payload — 12,294 violations.** Today's GIR `EnumFieldLoad` only auto-zeros the source for GorgetString payloads (LIR `is_str_field` path); other resource-typed payloads (Vector, Dict, Set, Box, Callable, user structs with resource fields) silently shallow-copy. Top types: IoError (2944), Vector__int64_t (2810), TypedColumn__double (1360), TypedColumn__int64_t (1360), Vector__uint8_t (788), Box__SpannedExpr (499), HttpRequest (492). The Box-in-enum cluster is precisely the latent class the `option_box_enum.gg` leak workaround documents. Lowering migration: extend `EnumFieldLoad`'s LIR auto-zero to cover all resource payloads (not just `is_str_field`), or emit explicit `MoveZero` / clone at the GIR layer for resource extracts. Estimated 1-2 days; risk medium because it touches Option/Result match-arm payloads, the Phase D4 hot path.

  2. **`FieldLoad` shallow copy of resource field — 2,568 violations.** Top types: PeerId (628), UdpAddr (624), Token (598) — all p2p-related user structs containing resource state. GorgetString (214) and Expr (185) follow. Today GIR `FieldLoad` of a resource field structurally produces a value-copy at LIR; Phase C contract demands explicit Borrow-mode bind or Clone at the boundary. Lowering migration: detect resource-field reads at GIR construction time and either route through a `Borrow` instruction (when dst is `Ptr(T)`) or insert an explicit clone at the use site. Estimated 1-2 days; risk medium because field-load is on every method-call receiver path.

  3. **`IndexLoad` with `borrow=false` of resource element — 2,556 violations.** Top types: GorgetString (2552), Task__int64_t (4). The GorgetString cluster is mostly a GIR/LIR boundary mismatch: GIR emits `borrow=false` but LIR's `clone_fn_for_collection_element` clones via `gorget_string_clone_to_owned` regardless of `borrow`, so today's runtime is sound. Phase C wants the GIR contract to encode the borrow-vs-clone decision explicitly so the validator can prove safety without reading downstream LIR routing. Lowering migration: route the GIR-level `borrow` flag from CoW state — emit `borrow=true` whenever the element is a borrow alias (CoW default), `borrow=false` only for explicit `.clone()` or ownership-boundary contexts. Estimated 1-2 days; lowest risk because the LIR layer already does the right thing — this is contract-tightening at GIR.

  Promotion contract: each class lands a separate commit that (a) migrates the lowering site so the count drops to zero on the integration sweep, (b) flips the validator from `GG_VALIDATE_RESOURCE_READS`-gated warning to unconditional fatal panic. Order: pick lowest-violation class first (IndexLoad GorgetString cluster) for the shortest feedback loop. Each promotion is one fail-fast guarantee added to the Phase C contract.
- **Phase D4 — `lower_var_decl` decision tree refactor** (deferred 2026-05-01, plan refined 2026-05-04, branches A/B/C/E/F/G shipped, D blocked on architectural change as of 2026-05-06). 6/7 branches now read typed predicates; only D retains `is_named_local` as genuine gating.

  **Status update 2026-05-06:** the 7-branch chain was extracted into a dedicated helper `lower_var_decl_assign_mode` (commit b61ee152) shaped as a typed match on `(target_resource, source_live, source_own)` per `docs/internals/unified-resource-model.md` §6.7. Subsequent commits migrated:
  - **A** (`530f5a56`) — `is_named_local` → `source_live && source_own.is_owned()`. Probe regressed 10 fixtures previously; the typed predicate excludes unnamed call-result temps and routes them to F's Move path. Full sweep 1067/1067.
  - **B** (`2702753e`) — same substitution. Probe regressed 16 fixtures previously; typed predicate excludes Result/Option recursive-drop temps. Full sweep 1067/1067.
  - **C** (`1357f07e`) — `is_named_local` → `source_live` (NOT gated on Owned because transitive alias chains require Borrowed sources too — `cow_transitive_alias.gg` regression caught this during the probe). Full sweep 1067/1067.
  - **F** (`fe65b99b`) — legacy `drops.is_registered(source_place.local)` proxy retired as fully subsumed by the typed `(Owned + dead + needs_drop(target))` clause. Full sweep 1067/1067.
  - **D probe (`7d60ccaa`)** — substituting `source_live` regressed self-host bootstrap with "local _19 read after MoveZero in bb5" in `is_cstr_returning_call`. Root cause: when D's `clone_fn_for_ptr` lookup fails, the safety-net G emits Move on the SOURCE, zeroing a Borrowed transitive alias's heap data. The `is_named_local` guard captured "named-and-live whether Owned or Borrowed" that neither `source_live && Owned` nor `source_live` alone preserves through the clone-fn-not-found fallthrough.

  **Remaining work for D:** retiring requires either (a) widening `is_resource_type` to enum-with-resource-payload so clone_fn lookup is reliable (sibling TODO entry, 112-fixture regression on prior naive widening); or (b) D explicitly bails out on Borrowed sources rather than falling through to G's source-Move-zero. Both are architectural changes, not single-line edits.

  Function shape: `lower_var_decl` 593 → 430 lines (-163); new helper at 192 lines with explicit branch docs and probe history pointers. Helper retired: none (probe history says `is_named_local` is still used in 6+ other call sites). Sub-TODO filed per branch in the helper itself.

  **Refined plan (2026-05-04):**

  ```rust
  // New helper on LoweringContext:
  fn source_live_past(&self, local: LocalId, stmt_span: Span) -> bool {
      // For named locals: !is_last_use_at(name, stmt_span).
      // For temps: false (temps dead after their producer).
      // For non-place operands: false.
  }

  fn source_ownership(&self, op: &Operand) -> Option<LocalOwnership> {
      // Look up via builder.locals[p.local].ownership for Copy/Move.
      // None for constants.
  }

  // Replaces the current 7-branch chain (~200 lines) with:
  let target_resource = ctx.type_registry.is_resource_type(actual_var_type);
  let source_live = ctx.source_live_past(operand.local(), stmt_span);
  let source_own = ctx.source_ownership(&operand);

  let assign_mode = match (target_resource, source_live, source_own) {
      (false, _, _)                                       => Copy,
      (true, _, Some(LocalOwnership::View { .. }))        => emit_clone_to_owned(),
      (true, _, Some(LocalOwnership::Borrowed { .. }))    => Borrow,
      (true, true,  Some(LocalOwnership::Owned))          => emit_cow_alias_or_clone(),
      (true, false, Some(LocalOwnership::Owned))          => Move,
      _                                                   => /* safety net */ Copy,
  };
  ```

  **Map of current branches to target arms:**
  - **A (~line 542, GorgetString same-type + named) → arm 4 (Owned + live → CoW alias path). DOCUMENTED 2026-05-04** — `is_named_local` probe regressed 10 fixtures (`leak_*`, `stress_alloc_strings/closures`, `string_builder*`). Genuine gating (Outcome 2): unnamed function-call temps own GorgetString data; unregistering their drop and treating them as borrow sources leaks the heap allocation. Retiring requires teaching `mark_string_borrow_source` to consult typed ownership instead — only named-and-still-live locals are legitimate borrow sources.
  - **B (~line 572, named non-resource with clone_fn, e.g. Str→GorgetString) → emit Clone. DOCUMENTED 2026-05-04 (cd9357f8)** — genuine cross-type guard.
  - **C (~line 604, named resource + CoW-safe) → arm 4 (CoW alias). DOCUMENTED 2026-05-04** — `is_named_local` probe regressed 50+ fixtures across arena/async/borrow/box/bytes/closure/collection/cow/csv/dataframe/derive/dict/... families before sweep was halted at letter "d". Genuine gating (Outcome 2): unnamed temp source dies at end-of-stmt, leaving the CoW Ptr alias dangling — SIGSEGV. Retiring requires CoW alias creation to consume (move-from) the source temp, not borrow it.
  - **D (~line 650, named resource + CoW-unsafe) → arm 2 (Owned + live → Clone fallback). DOCUMENTED 2026-05-04** — guard probed jointly with C (both gate on the same `is_named_local`); 50+ failures dominated by C's CoW-alias path. D's independent contribution would be a redundant clone of an already-Move-eligible unnamed temp (leak rather than UAF). Retiring requires the same source-consume migration as C plus widening F to subsume unnamed-temp resource sources unconditionally.
  - **E (~line 661, view-returning result) → arm 5 (View → Clone). SHIPPED 2026-05-04** — typed `LocalOwnership::View` match, sidecar `view_returning_temps` retired (commit 9dc2cf4d). Required first fixing cow_materialize_view's shallow-copy bug (Move mode at the clone-to-owned assign) so View-tagging unnamed temps was safe.
  - **F (~line 691, drop-registered temp or droppable temp) → arm 3 (Owned + dead → Move). PARTIAL 2026-05-04** — extended with typed `(needs_drop_target, source_dead, source_owned) => Move` to catch Option/Result-wrapper cases the legacy predicate misses (commit 886b4d0c). Legacy predicate retained as a strict subset until further investigation determines whether named drop-registered locals at non-last-use can be safely dropped from F.
  - **G (safety net) → catch-all. SHIPPED 2026-05-04** — predicate switched from `is_resource_type(rhs_type)` (source-keyed) to `target_resource` (target-keyed). Correct axis: Move applies to destination, not source.

  **Branches A/C/D probe summary (2026-05-04):** All three guards are genuine gating (Outcome 2) — none are mechanically redundant. The structural commonality is sharp: every one of these branches assumes the source local *owns its data and lives past the var_decl statement*, and the `is_named_local` predicate is today's proxy for both. Removing it lets unnamed call-result temps (which own data but die at end-of-stmt) flow into branches that either (i) unregister their drop (A: leak), (ii) borrow into them (C: dangling Ptr), or (iii) clone them while leaving their drop registered (D: redundant alloc + double-free risk depending on later moves). The unified retirement is a single migration: replace `is_named_local && X` in branches A/C/D with the typed condition `source_own == Owned && source_live`, and route the unnamed-temp case to Branch F's Move path. Without that consume-source rewrite, removing any guard individually is unsound.

  **Probe findings (2026-05-04):**

  Different sidecar/guard patterns yield different elegance outcomes — same probe-then-diagnose discipline, three possible conclusions:

  1. **Real consumer bug hiding behind the guard** (the view_returning_temps case): probe surfaces a shallow-copy or aliasing bug in a downstream pass that misreads typed state. Fix the consumer, retire the sidecar. **Net win: -1 sidecar, +1 Phase C correctness improvement.**

  2. **Guard is structurally non-trivial** (the Branch B case, commit cd9357f8): probe regresses many fixtures across diverse paths. The guard is genuine gating, not a workaround. **Document the rationale so the next session doesn't redo the probe.** Retiring requires architectural changes (e.g., widen is_resource_type to include enum-with-resource-payload, split the cross-type axis into its own arm).

  3. **Guard is mechanically redundant** (the `mut_capture_locals` case, retired 2026-05-04): probe is fully green. Retire the sidecar with no further work. `mut_capture_locals: FxHashMap<LocalId, TypeId>` migrated to typed `is_param_borrow_unique` predicate + `pointee_type(builder.local_type(local))` for value-type lookup — 5 writers, 11 readers across 8 files; no consumer bug surfaced. See DONE entry. One less sidecar, one typed predicate where there was a name-keyed map.

  When applying the discipline to remaining branches A/C/D, expect outcome (1) or (2). Each probe = ~10 min wall-clock for the integration sweep, so batch probes where the diagnosis isn't expected to interact.

  **Parallel work on `lower_assign` (commits 7ce8e056, 404c8716, 2026-05-04):** the sister decision tree at `src/ir/lowering/stmts/assigns.rs:229-298` got the same signal-lift treatment plus two probes:
  - **Branch D retired (mechanically redundant — outcome 3).** The duplicated string-to-string Move predicate after `else if drops.is_registered(...)` was unreachable via `else if` chaining (Branch A's identical predicate matched first).
  - **Branch E asymmetric with VarDecl's branch G (genuine gating — outcome 2).** Mirroring `target_resource = is_resource_type(type_id)` regressed 7 fixtures (dataframe_*, self_host_bootstrap*) with double-frees. Unlike VarDecl, this site sees cross-type cases where rhs is non-resource but target is — Move'ing the source would zero a primitive local still alive elsewhere. The legacy RHS-keyed `is_resource_type(rhs_type)` is genuinely correct here. Documented in code; the lower_var_decl→lower_assign mirror does not apply uniformly.
  - **Branch C+F-extension is the same shape as E** — also regressed the same 7 fixtures. Not committed; same root cause as E (target-keyed reasoning over a tree that handles cross-type cases differently). Defer until either a deeper invariant is established or the cross-type cases get their own explicit arm.

- **`is_resource_type` widening to enum-with-resource-payload — coordinated migration required** [BLOCKED, surfaced 2026-05-04]. The narrow/wide axis gap (`is_resource_type(Option[String]) == false` despite `needs_drop(Option[String]) == true`) is structurally the right axis to widen for Phase C's validator scope and Branch F's typed-match arm — but a one-line widen of `is_resource_name` to also check enum variants regressed **112 fixtures** in `dataframe_*`, `coroutine_*`, `collection_types`, `catch_basic`, etc. (commit reverted same session). Many consumers — pattern lowering, collection-element clone routing, drop accountant, ABI choice — depend on the current narrow semantics where Option/Result wrappers are NOT resource at the wrapper level (only their payloads are). The migration to widen requires updating those consumers in parallel; doing it as a one-line schema change destabilizes the suite. Either: (a) audit each `is_resource_type` consumer first, classify into "should accept widening" vs "needs the narrow check", expose a separate accessor for the latter (e.g., `is_directly_resource_type` keeps the current narrow shape); (b) do the migration as a multi-week coordinated PR with consumer-by-consumer fixes; (c) accept the gap and use `needs_drop(target)` at sites that need the wider semantics (Branch F's current shape). The documentation in `is_resource_name` (commit cd9357f8 follow-on) preserves the finding so a future session doesn't re-run the probe. [added: 2026-05-04]

  **Estimated 1-2 days for the remaining 6 branches; honest scope is closer to a week given the elegance bar.** Risk: medium — touches CoW alias creation, mark_string_borrow_source, drops.unregister, register_local re-binding. Validation: full integration sweep + cow_materialization_points fixture must stay green at every step. Migrate one branch at a time by replacing it with the typed-match arm and verifying integration. **The branch-E migration showed the right pattern: when typed-state migration regresses, the regression is almost always a downstream consumer with a latent correctness bug that the sidecar was hiding — fix the consumer, then migrate. That's the elegance step, not a workaround.**

  **Discipline while deferred:** don't accumulate new branches in `lower_var_decl` whose predicates aren't expressible as liveness queries. Each new case must be reducible to `(target.is_resource, source_live, source.ownership())`; if a case requires reading a different axis, flag it here instead of adding the branch silently. [added: 2026-05-01, plan refined: 2026-05-04, branch E shipped: 2026-05-04]

- **Phase D4.5 — retire `func_state.local_ownership: FxHashMap` in favor of `Local.ownership` as the live store** [DONE 2026-05-06]. All five steps shipped (5a-5d):
  - **5a** Added `LocalOwnership::Untracked` as the new `#[default]`, preserving the legacy "absent from FxHashMap" semantic for predicates like `is_owned_local`/`is_ref_local`. The previous default was `Owned`, which would have flipped every untracked local under naive migration.
  - **5b** Migrated all readers (`is_owned_local`, `is_ref_local`, `is_bare_param`, `is_param_borrow_unique`, `is_cow_borrow`, `is_fresh_string`, `cow_is_alias`, `cow_resolve_root`, `collection_ref_source`, `has_string_borrowers`, `views_of_source`, `shared_heap_aliases_of_source`, `tuple_element_sources`, `cow_aliases_of`, `cow_collection_refs_for_id`, `cow_has_collection_refs`, `cow_has_aliases`, `field_borrows_of`, plus the case-1 alias-source check in `cow_before_mutation` and the SharedHeap match in `stmts/mod.rs`). All consult `builder.locals[i].ownership` directly. Threaded `&FunctionBuilder` through ~75 call sites across context.rs, exprs/, stmts/.
  - **5c** Replaced `SavedScope`'s `local_ownership: FxHashMap` + `local_types_at_save: FxHashMap` with dense `Vec`s indexed by LocalId. `save_locals` walks `builder.locals` once; `restore_locals` writes through `builder.locals[i].ownership` directly.
  - **5d** Deleted the `func_state.local_ownership: FxHashMap` field and the FxHashMap-write halves of every setter. `Local.ownership` is the sole live store.
  - **5e** `flush_ownership_to_locals` retained for slot_kind derivation (its real purpose); the FxHashMap-coherence debug assert was dropped (no FxHashMap to drift from).

  Self_host_bootstrap green throughout. Full integration sweep at end: 1066/1066 passed.

- **Deferred String materialization — Sites #2 and #4 remaining** [LOW PRIORITY] (filed 2026-05-04, sites #1 + #3 closed 2026-05-05/06). The auto-deref path at `stmts/mod.rs` (Site #1) now propagates `Borrowed { Field { base, field }, .. }` for typed bindings off struct field-loads (closed 2026-05-06; option (a) shipped via consumer-bug fix at `lower_struct_init` Move-sigil branch). The CoW severance walk for NAMED Field borrows (Site #3) shipped 2026-05-05. Sites #2 and #4 remain:

  - **Site 2 (`methods.rs:2197` view propagation through Option-wrapping).** **Theoretical, not currently triggered.** The 6 string methods that return views (`trim`, `substring`, `slice`, `strip`, `str`, `as_str` per `builtins.rs:634-639`) all return `String`-by-value, not `Option[String]`. The `s.find(...)` example in the original TODO returns `Option[int]`, not `Option[StrView]`. No currently-shipped or planned API hits this path. Not a real gap until a future view-of-Option API exists.

  - **Site 4 (borrow-checker decidability).** The lifetime question — "can we statically prove `x` doesn't outlive `source`'s last possible mutation?" — needs a separate design pass. Today's heuristic (`is_cow_unsafe_at(name, span)` for reassignment-on-forward-path) catches the common case but isn't lifetime-aware. Defer to a dedicated session.

  [added: 2026-05-04, sites #1 + #3 shipped: 2026-05-05]




- **Residual: `Option[Box[T]]` / `Result[Box[T]]` field drops not emitted on enum variants and struct fields.** The Box-field-drop wrapper `Box__T__drop` and its wiring at struct/enum-variant scope-exit (cases a + b + c of the prior Box[T] item) were closed 2026-05-01 — see DONE. The Option/Result ENUM-VARIANT skip at `populate_recursive_drop_enums` (mod.rs:471-481) and STRUCT-FIELD skip at `populate_recursive_drop_structs` (mod.rs:412-422) was kept intentional: enabling the drop crashes the self-host `resolve_stmt` path because `get_stmt_at(stmts, i)` (resolve.gg:24) returns `Stmt` by value via `v.get(i).unwrap()` — a shallow copy that aliases the vector's interior box/string pointers; both copy and source drop, and dropping the `Option[SpannedExpr]` field inside `Stmt` double-frees the SpannedExpr's Expr/string that the standalone SpannedExpr drop already freed. The proper fix is at the COMPILER level: make `Vector[T].get(i)` for resource T auto-clone (deep) or return `Ref[T]`-only (forcing the caller to .clone() at the boundary). Once that lands, the Option/Result drop skip can be removed and `option_box_enum.gg`'s 3 leaked Some(Box(...)) blocks will free correctly. Today: leak (3 blocks for option_box_enum), not unsoundness. [added: 2026-05-01]

- **Drainable for `Set[T]` / `Dict[K, V]`** — sibling capability trait shipped on Vector 2026-04-27 (O(n) reverse + pop). Set/Dict drain not yet equipped — they'd need a runtime helper `gorget_map_drain_entry(map, idx, out_key, out_val)` that moves the K/V out of the bucket and tombstone-marks the slot so the source's drop doesn't double-free, OR the equivalent move-out-of-collection-slot machinery applied to GorgetMap's bucket array. Today `Set.drain()` / `Dict.drain()` doesn't exist; users wanting drain semantics call `.iter()` + `.clone_each()` (clones every element) or build their own drain iterator over the bucket array. Priority: low until a real consumer needs it. [added: 2026-04-27]

- **Lazy `Dict.keys()` / `.values()`** — residual after Dict/Set lazy iter shipped 2026-04-25. `Dict.iter()` / `Set.iter()` are now lazy bucket-walks via `Ref[Dict[K, V]]` / `Ref[Set[T]]` borrow fields (DictIter / SetIter in `lib/std/iter.gg`). The matching `.keys()` / `.values()` projections still allocate eager `Vector[K]` / `Vector[V]`. Two natural follow-ons now that the borrow-field plumbing exists: (1) `DictKeysIter[K, V]` / `DictValuesIter[K, V]` state-machine structs, same shape as DictIter, that yield only the K (or V) component; (2) thin user-space wrappers — `d.keys()` returns `DictKeysIter[K, V]`, `d.values()` returns `DictValuesIter[K, V]`. Today users get the same effect via `d.iter().map(((K, V) p): p.0)` (verbose). Low priority — eager `.keys()` / `.values()` are unchanged behaviour, the gap is just allocation efficiency, not correctness. [demoted: 2026-04-25]


- **Self-host silent-fallback audit — IN PROGRESS**. Diagnostic wiring shipped (commit af0cb513): three sites now emit `/* [bug] ... */` comments in the generated output instead of silently returning sentinels: (a) `map_binop` unknown operator, (b) `EIdentifier` unknown name, (c) `EFieldAccess` unknown field. Also added `map_compound_binop` for `+=` / `-=` / ... spellings (commit 299ffb0c — was the root cause of `last_us -= 1` → `last_us += 1`). Still to audit / tighten: `infer_method_return_type` I64 fallback for unknown methods, `collection_element_type` "" fallback for unknown prefixes, `type_id_to_name` "int64_t" fallback for non-GtNamed tids. Attempted a cap on `[bug]` emission count but stage-1's lowerer doesn't reliably propagate mutable global state so the counter stayed at 0 — reverted to loud-by-default; callers can filter via `| grep -v '\[bug\]'` and dedupe via `sort -u | uniq -c`. Proper env-var toggle (`GORGET_QUIET_FALLBACKS=1`) deferred until the env-var reader lands in the self-host. [revised: 2026-04-24]

- **Self-host stage-1 — runs but stalls on stage-1-on-driver-gg**: Stage-1 runs end to end on `void main(): pass` and produces output closely matching stage-0. Earlier blockers closed: enum sizing, struct alignof, None coercion, Option/Result match dispatch, longest-prefix field_read split, File.close/flush routing, emit_field_store scalar/void vty, **Stmt-slot-as-int64 hang** (3564ba22), **Pass 4 struct inter-field padding** (11886a9e), **PLiteral pattern lowering** (a1a6e34c), and **POr pattern lowering** (fd874afa 2026-04-23 — `case A | B | C:` alt-patterns now lower to chained attempt blocks instead of falling through to unconditional match; 32 → 0 "unsupported pattern variant" diagnostics). REMAINING: (1) **EFieldAccess types missing user structs — widespread I64 fallback**: root cause confirmed via instrumentation — `gmod.type_infos.get(k).unwrap()` returns an I64-typed local because `collection_element_type` in lower.gg doesn't handle `Dict__K__V` / `HashMap__K__V` prefixes. This cascades: when `match X.expr:` sees scrutinee=int64_t, all PConstructor arms fail with "enum_name empty" → ~4969 such failures. Attempted fix (add Dict/HashMap extraction) triggers a separate typechecker slowdown — stage-1's `typecheck___type_check_stmt` goes from ~3min to 10+min when the broader type graph is correctly typed (previously short-circuited on I64 fallbacks). Both narrow (only simple primitive keys) and type_infos.contains() gated variants tried — all trip the slowdown. Fix blocked pending typechecker performance work OR a very-narrow site-specific fix. (2) **SIGSEGV root cause**: exit 139 from stage-1-on-driver.gg happens in `lir_lower___lower_type_defs` at a `gorget_map_get(NULL, k)` site — a direct consequence of the int64-typed binding from (1). Fixing (1) fixes the SIGSEGV but uncovers the typechecker slowdown. (3) stage-1 `main`'s return type is `int8_t` instead of `int64_t` — minor type-inference drift. [revised: 2026-04-30 — earlier item (3) box-drawing Unicode truncation was actually `fix_printf_format`'s byte-as-char cast, fixed in trunk]

- **Stdlib narrow waist — Phase 2c residual items**: (a) (2) impl-override sig substitution **SHIPPED 2026-04-29** (see DONE.md). (b) **Builtin Vector HOF expansions cleanup** — `src/ir/lowering/builtins.rs:257-271` look like dead code for the wrapped methods, but only `each` (void return) is actually safe to delete. Typed-return entries provide signature info that IR-lowering reads to declare the function correctly; deletion blocks on a separate signature-source for IR-lowering when BuiltinMethodDecl is absent. LIR `HofOp` variants stay live regardless — they serve Dict / Set too. See design doc §10; `lib/std/iter.gg` is the authoritative source. [revised: 2026-04-29]

- **Self-host check_comparison gaps — 63 mismatches + 25 crashes** [revised: 2026-04-29 after extern-equip parser fix landed +106 fixtures, 802 → 908/996, 91.2%]:
  - **(i) extern equip methods** **SHIPPED** (commit 7a2d4bae): self-host parser was skipping `extern int bytes_used() = "..."` inside equip blocks. Fixed.
  - **(ii) httpserver_*.gg crashes (25 fixtures)** — pinpointed to `Callable[Option[T](X)]` field type inside a struct. Minimal repro: `struct A: int x; struct B: Callable[Option[A](A)] field` segfaults; `Callable[Option[A](int)] field` also segfaults; `Callable[A(A)] field` works. Top-level static decl with same type works. The crash is during typecheck, not parse. Likely null-deref in the typechecker's RTCallableTrait resolution when the inner function-type's return is itself an RTGeneric (Option[T]) AND the field is inside a struct. Needs investigation.
  - **(iii) dataframe_* fixtures (~14 fixtures, ~5 lines each)** **MOSTLY-FIXED** (commit 1ce538bd): meta-for-in-match expansion shipped. Self type now resolves as Column for all equip methods. Dataframe fixtures still mismatch by ~11 surface lines because Gorget materialises one `c` def per variant per method while Rust's expansion appears to share / dedupe. Reduced from ~14-line gap to ~11-line gap; underlying typing correct.
  - **(iv) std.io stdin/stdout/stderr globals (~14 fixtures)** — Gorget output has `stdin = File`, `stdout = File`, `stderr = File`, math constants (NAN, PI, TAU, etc.) at top level; Rust's loader-driven analyze() doesn't surface these. Difference: Rust wraps non-entry modules in `Item::Module {path, items}`; self-host's loader appends items flat. Defs in nested `Item::Module` don't surface in Rust's def_count() iteration. Possible fix: mirror Rust's wrapping, but requires self-host resolver to handle `Item::Module` (currently flat-iteration).
  Fix path: tackle (iii) next — single root cause, +14 fixtures. Then (ii) — needs typechecker null-deref guard. Then (iv) — bigger refactor. [revised: 2026-04-29]

- **Cloneable trait for generic bounds**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Runtime counters shipped via `--clone-stats` — atexit line emits `[clone-stats] array_clone=... map_clone=... set_clone=... string_cow=... string_cat=... box_alloc=... ... peak_rss_kb=...`. Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`) — ships alongside the next round of ownership work. [updated: 2026-04-21]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

- **Drop `Inst::CallExtern.original_name: Option<String>`** — the audit's Tier-2 item #4. After today's typed-IDs cluster (`SetCollectionBridge.key_struct: StructId`, `TraitCall.{trait_obj_struct, method_idx}`), the field still has three consumers that string-parse it: (1) `wire_collection_bridges` in `lir/types.rs` recovers the user key type from `Dict__T__V__new` to insert `SetCollectionBridge`; (2) `find_hashable_key_types` in `lir/optimize.rs` recovers reachability roots; (3) `infer_collection_elem_fns` in `lir/lower/insts.rs` recovers element types for the elem_drop / val_drop / key_drop wiring. Eliminating the field requires a typed sidecar — e.g., `Inst::CollectionCtor { ctor_kind: CollectionCtorKind, key_struct: Option<StructId>, val_struct: Option<StructId>, ... }` that replaces the `CallExtern("gorget_dict_new")`-with-original-name pattern at LIR construction time. Then the three consumers read structured fields, the `original_name` field is unused everywhere, and we can delete it. Estimated 200-300 lines across the LIR and lowering. [added: 2026-04-30]

- **C backend: migrate val_types to func.value_types**: Shared `compute_module_value_types()` runs after LIR optimization and populates `func.value_types`. The LLVM backend already reads from it. The C backend still uses its own single-pass `infer_inst_type` because its multi-phase fixups (guard accessor inference, CallExtern→SlotStore mismatch, cross-type map combinator) depend on `ptr_pointee` context computed in the same pass. Next step: seed the C backend's val_types from `func.value_types` and reduce the fixup phases. [updated: 2026-04-14]

- **LIR value origin metadata — enable Store/SlotStore/Call lifts**: The C backend maintains 5 origin bitmaps (`str_lit_vals`, `null_vals`, `cstr_vals`, `ptr_pointee`, `func_addr_targets`) beyond type info. These track value provenance needed for ~37 emit-decision sites. The type metadata (`func.value_types`) is now shared; origin metadata remains backend-local. Fix: attach origin tags to LIR values (e.g. `StrLit` → string-literal flag, `NullPtr` → null flag, `FuncAddr` → FuncId). Unblocks lifting Store routing (~50 lines), SlotStore string/cstr coercion (~22 lines), and Call/CallPtr ABI coercion (~100 lines). [updated: 2026-04-14]

- **Decompose emit_call_extern.rs (~1,850 lines)**: Tier 1-3 lifts complete — ~490 lines of inline expansion removed. Remaining: HOF inlining (map/filter/each/fold ~590 lines), printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-04-15]

## Medium

- **Extend `noreturn` qualifier to builtins (`panic`) and other `_Noreturn` C functions.** Round 6 added `noreturn` to the parser/typecheck/IR pipeline and marked `std.os.exit` as `extern noreturn`. `panic` is a hardcoded compiler builtin (treated as void-returning at typecheck via `matches!(cname, "print" | "assert" | "panic")` in `typecheck.rs:1443`, lowered via a hardcoded `call_extern("gorget_panic", …)` in `stmts/mod.rs:1814`), so the noreturn pipeline doesn't reach it — calls to `panic()` in match-as-expression arms still hit the void-vs-T mismatch. Two options: (a) declare `panic` in `lib/std/io.gg` (or wherever) as `extern noreturn void panic(String msg)` and remove the hardcoded path; (b) extend the typecheck builtin special-case to return `never_id` for panic and add `"gorget_panic"` to `LoweringContext.noreturn_fns` at construction. Option (a) is the layering-discipline-correct answer — it removes a name-match in the compiler. Also audit `lib/freestanding/runtime.c` and `c_runtime.rs` for any other `_Noreturn` C functions exposed to Gorget (likely none today). [added: 2026-05-06 from JS-interpreter snag #12 round 6 follow-up]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]


- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

- **LLVM backend test results (2026-04-16, post-session)**:
  - **738 PASS / 814 (90.7%)**, 29 FAIL, 38 CRASH, 9 BUILD_FAIL (after elem_drop re-enable). Up from 710 PASS baseline — **+28 net PASS, -10 FAIL, +3 CRASH**.
  - Fixes: (1) Option/Result combinator inline handlers, (2) CStr null-termination, (3) **LIR elem_drop/elem_clone stores re-enabled** + LLVM SlotStore String CoW clone + NamedFuncAddr declaration generation.
  - **elem_drop root cause (resolved)**: LLVM's SlotStore did plain memcpy for all aggregate stores regardless of `is_move`. C backend emits `gorget_string_copy_cow` on non-move Ptr→String stores (src/backend/c_lir/mod.rs:1629). Fix: mirror that CoW clone in LLVM backend src/backend/llvm/mod.rs SlotStore handler + declare `T__clone`/`T__drop` for NamedFuncAddr user-type references.
  - **Remaining 4 dataframe_* CRASH**: Still double-free somewhere in xtd.dataframe with elem_drop active — deferred (likely nested Vector[Vector[Column]] or Union-typed payload issue).
  - Remaining BUILD_FAIL (9): 4x LLC forward-ref type mismatch, conv_stdlib, shared_iterator_invalidation, print_trait_object, string_enum_variants, sqlite

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **Self-host LIR backend**: ~6,200 lines across 4 files. 687/936 fixtures compile (was 462 baseline; net +225 over two sessions). 0 crashes. Key fixes across sessions: (1) SlotStore type-mismatch coercion — scalar→aggregate and aggregate→aggregate both emit `{0}` zero-init; (2) runtime fn return types — gorget_args/env_vars/cwd/str_to_upper/lower/char_at/byte_slice/int_to_str/float_to_str/bool_to_str all correctly typed; (3) runtime_arg_is_str table coerces pointers/scalars at Str parameter positions (str_cat/eq/cmp etc.); (4) ICmp narrowed to GorgetString plus memcmp fallback for struct==struct; (5) generic placeholder + enum variant filtering in type_defs; (6) bare opaque/prelude type constructors (TaskGroup, AtomicInt, Box, Shared, …); (7) is_type_constructor excludes primitive coercions; (8) post-gmod fn_sigs pass covers functions + equip methods; (9) extern time/time_ms/format_time/parse_time mappings; (10) Option/Result combinator takes address of aggregate src; (11) drop/clone forward declarations prevent static-after-implicit conflicts; (12) enum_variant_parent routes bare variant constructors to parent enum type; (13) Str/String/GorgetString identity coercion (Str("x") → x); (14) imported IEnum merged with __imported_type__ marker (skips drop/clone regen); (15) TFunction param ABI is Ptr(FnPtr) instead of unit — closure params now get pointer passing; (16) static method calls on type identifiers (Point.default(), int.parse(s)); (17) operator overload (+/-/*/div/rem/neg/==/!=/<=/>=) dispatches to TypeName__method for user structs, including monomorphized instances; (18) gorget_str_strip arity padding. Remaining ~249 failures: Str-as-int casts in JSON/XML/TOML parsers (b64_char_value), imported-struct field access (needs IStruct loader merge without drop conflicts — tried, regresses), DataFrame col_slice with Column placeholder types, Vector[T](alloc=…) keyword args, throws/Result auto-wrapping, SSA phi gaps (unassigned block params). [updated: 2026-04-17]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]


- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]

