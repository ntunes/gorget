# TODO

## High

- **Dict / HashMap literal as `Some(...)` arg leaks the inner buffer.** `Option[Dict[K, V]] x = Some({"a": 1, "b": 2})` allocates the dict's hash-table backing buffer (~788 bytes for 2 entries), then `clone_resource_args_for_init` sees the dict temp as `Untracked` and emits `Dict__K__V__clone` into the Option payload, orphaning the original buffer. Symmetric to Snag #25b (array literal, fixed 2026-05-09) but the dict fix is harder: applying the same `set_owned` + `register_local` pattern to `lower_dict_literal` causes a double-free for Vector/resource VALUES — `gorget_map_put` only sets `val_materialize` for `GorgetString` (not Vector), so the dict's slot directly aliases the temp buffer; with the temp drop-registered, both the temp and the dict's elem_drop free the same buffer. Confirmed locally with `Option[Dict[String, Vector[int]]] x = Some({"a": [1,2,3]})` → `free(): double free detected`. Two viable paths: (1) wire `val_materialize = gorget_array_clone_inplace` for resource-typed values in Dict/HashMap LIR codegen (`src/lir/lower/insts.rs` around line 2010), parallel to the existing String materialize wiring at offset 136; OR (2) emit move-zero-and-mark for resource-typed key/value operands at `lower_dict_literal`'s `put_fn` call site (per-arg, conditional on resource type + Move-eligibility), parallel to the elem-mode treatment in `lower_array_literal`. Approach (1) is more architecturally consistent (the symmetry with String materialize is right there in the runtime). Set literals share the array literal lowering path so they're not affected. Today: silent leak for Some({K: V}) where V is a primitive resource (Dict header buffer); double-free if you naively replicate the array fix on dict. [added: 2026-05-09]



- **Phase D4 — `lower_var_decl` decision tree refactor** (deferred 2026-05-01, plan refined 2026-05-04, branches A/B/C/E/F/G shipped, D blocked on architectural change as of 2026-05-06). 6/7 branches now read typed predicates; only D retains `is_named_local` as genuine gating.

  **Status update 2026-05-06:** the 7-branch chain was extracted into a dedicated helper `lower_var_decl_assign_mode` (commit b61ee152) shaped as a typed match on `(target_resource, source_live, source_own)` per `docs/internals/unified-resource-model.md` §6.7. Subsequent commits migrated:
  - **A** (`530f5a56`) — `is_named_local` → `source_live && source_own.is_owned()`.
  - **B** (`2702753e`) — same substitution.
  - **C** (`1357f07e`) — `is_named_local` → `source_live`.
  - **F** (`fe65b99b`) — legacy `drops.is_registered(source_place.local)` proxy retired.
  - **D probe (`7d60ccaa`)** — substituting `source_live` regressed self-host bootstrap. Filed.

  **Remaining work for D:** retiring requires either (a) widening `is_resource_type` to enum-with-resource-payload (sibling TODO, 112-fixture regression on naive widening); or (b) D explicitly bails out on Borrowed sources. Both are architectural changes.


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

- **`is_resource_type` widening to enum-with-resource-payload — coordinated migration required** [BLOCKED, surfaced 2026-05-04; **Phase 1 audit complete 2026-05-05**]. The narrow/wide axis gap (`is_resource_type(Option[String]) == false` despite `needs_drop(Option[String]) == true`) is structurally the right axis to widen for Phase C's validator scope and Branch F's typed-match arm — but a one-line widen of `is_resource_name` to also check enum variants regressed **112 fixtures** in `dataframe_*`, `coroutine_*`, `collection_types`, `catch_basic`, etc. (commit reverted same session). Many consumers — pattern lowering, collection-element clone routing, drop accountant, ABI choice — depend on the current narrow semantics where Option/Result wrappers are NOT resource at the wrapper level (only their payloads are). The migration to widen requires updating those consumers in parallel; doing it as a one-line schema change destabilizes the suite. Either: (a) audit each `is_resource_type` consumer first, classify into "should accept widening" vs "needs the narrow check", expose a separate accessor for the latter (e.g., `is_directly_resource_type` keeps the current narrow shape); (b) do the migration as a multi-week coordinated PR with consumer-by-consumer fixes; (c) accept the gap and use `needs_drop(target)` at sites that need the wider semantics (Branch F's current shape). The documentation in `is_resource_name` (commit cd9357f8 follow-on) preserves the finding so a future session doesn't re-run the probe. [added: 2026-05-04]

  **Phase 1 audit findings (2026-05-05).** Catalogued every `is_resource_type` callsite in `src/ir/`. **Total: ~134 callsites** = 120 direct `registry.is_resource_type(_)` invocations + 14 `is_resource_type_local(_)` thin-wrapper invocations (the underlying `is_resource_name` has 4 internal callsites within `types.rs` itself, none external).

  **Per-file distribution:**
  - `src/ir/lowering/exprs/mod.rs` — 21 (FieldLoad/IndexLoad Ptr-wrapping, struct-init field clone gating)
  - `src/ir/lowering/stmts/mod.rs` — 21 (lower_var_decl/lower_assign branch picker; mostly named "is rhs/target Move-only?")
  - `src/ir/lowering/exprs/methods.rs` — 15 (unwrap MoveZero, IndexLoad result type, collection-element handling)
  - `src/ir/lowering/context.rs` — 16 (CoW alias bookkeeping, ensure_owned_at_boundary, clone-fn lookup; mostly typed predicate scaffolding)
  - `src/ir/lowering/stmts/assigns.rs` — 12 (assign-mode picker; mirror of stmts/mod's lower_var_decl)
  - `src/ir/lowering/functions.rs` — 11 (return-value Move-override, parameter ownership inference)
  - `src/ir/lowering/exprs/calls.rs` — 9 (interp_temp_mode, fstring, !arg consumption)
  - `src/ir/lowering/traits.rs` — 7 (trait-method default impl ownership; same shape as functions.rs)
  - `src/ir/lowering/generics/mod.rs` — 6 (monomorphized fn signature ownership)
  - `src/ir/lowering/stmts/patterns.rs` — 5 (match-arm scrutinee borrow + variant field Ptr-wrap)
  - `src/ir/lowering/stmts/for_loops.rs` — 5 (iter Borrow vs Copy)
  - `src/ir/lowering/mod.rs` — 5 (return-value drop registration in spawn helpers)
  - `src/ir/lowering/exprs/collections.rs` — 4 (literal-element clone fanout)
  - `src/ir/lowering/closures.rs` — 3 (capture clone + LIR ABI)
  - `src/ir/validate.rs` — 3 (one is the Phase C `validate_read` gate at L1249; one a payload-mode picker at L1383; one in DropOnNonDroppable at L558)
  - `src/ir/lowering/exprs/type_reg.rs` — 2 (the `is_resource_type_local` definition + 1 internal use)
  - `src/ir/lowering/exprs/spawn.rs` — 1 (closure-arg field MoveZero)
  - `src/ir/lowering/exprs/shared.rs` — 1 (Shared refresh path)
  - `src/ir/types.rs` — 10 (definitions + internal recursion in `is_resource_name`)

  **Classification (with rationale):**

  - **Narrow (~125 sites).** Almost every callsite reads "is this a *direct* resource value with custom move/clone semantics?" and uses the answer to decide:
    - Whether to emit `Ptr(T)` wrapping (FieldLoad/IndexLoad/pattern-binding result type)
    - Whether to set assign mode to `Move`/`Borrow` vs `Copy`
    - Whether to emit `move_zero` after a consumption
    - Whether to register a typed local drop (only for *direct* resource — `Option[String]` is byte-copyable; its payload's drop is structural via the upgrade scan)
    - Whether to call `clone_fn_for_ptr` (which only resolves for direct-resource types — Option/Result clone goes through `Type__clone` synthesised by `lir/lower/drops.rs`)
    These are correct on the narrow semantic. Widening would regress: e.g. `let opt: Option[String] = some_call()` would suddenly try `Ptr(Option__String)` wrapping, which `clone_fn_for_ptr` can't satisfy → falls back to Copy, double-free.

  - **Wide (~3 sites identified, possibly more).**
    - `src/ir/lowering/stmts/mod.rs:875-878` — Branch F (`needs_drop(actual_var_type)`): **already migrated**, uses `needs_drop` and explicitly comments why widening matters here for Option/Result wrapper Move.
    - `src/ir/lowering/exprs/methods.rs:2617-2618` — closure-result drop registration: `if needs_drop(t) || is_resource_type(t)` — the `is_resource_type` half is **already redundant** (subsumed by `needs_drop`). Stylistic cleanup.
    - `src/ir/validate.rs:558` — DropOnNonDroppable validator's "Option/Result with droppable payload" exemption: uses `type_needs_drop || is_resource_type || (Named-name-match)`. The `||` chain is wide-by-disjunction; can simplify to single `needs_drop` post-migration.

  - **Unclear (~5 sites — leaning wide, need closer probe in Phase 2).**
    - `src/ir/lowering/exprs/spawn.rs:450` — closure-field MoveZero after field-load. If a closure captures `Option[String]`, the field-load is byte-copy (Option is non-resource at narrow), so no MoveZero is emitted — but the source enum's Some-payload heap String is now aliased between closure-field and call-arg. Phase 2 candidate to widen via `needs_drop`.
    - `src/ir/lowering/exprs/methods.rs:589, 604, 662, 1858, 1870, 1901` — unwrap-then-MoveZero gates. Today checks `is_resource_type(dst)` where dst is the inner type — narrow correct for `Option[String]→String` but gives a false negative for `Option[Option[String]]→Option[String]` (the inner Option still owns heap, but is byte-copyable as a wrapper). Probably narrow-correct because the upgrade scan + drop accounting handle it; verify in Phase 2.
    - `src/ir/lowering/exprs/calls.rs:1471, 1089, 1091` — interp_temp clone routing: `clone_fn_for_ptr(value_type)` is gated by `is_resource_type` — clone-fn lookup only succeeds for direct resources. Narrow-correct given the lookup table's shape, but if Phase A unifies clone routing this gate may need to widen.

  **Single most important finding: `needs_drop` IS already the wider predicate.** The doc comment at `src/ir/types.rs:366-371` and the explicit usage at `stmts/mod.rs:875-878` both confirm `needs_drop(type_id)` returns true iff the type's drop is non-trivial (transitively, via `upgrade_types_from_fields`'s `DropStrategy::Recursive` setting on enum/struct payloads). The "wider predicate" the migration plan called for is `needs_drop`. **No new predicate is functionally needed** — Phase 2 is "audit each narrow callsite, decide if it should be `needs_drop` instead". The new `is_resource_or_contains_resource` alias (commit 2) exists purely to communicate intent at the migration site (sites flipping to wide read better as `is_resource_or_contains_resource(t)` than `needs_drop(t)`, even though both are the same code path).

  **Caveat: late-registration race.** `ensure_option_type_registered` (`context.rs:2818`) does NOT run the upgrade scan, so an `Option[T]` registered during function lowering after the module-level `upgrade_types_from_fields` pass has empty metadata — `needs_drop` would return false for it. The Phase 2 migration must either (a) make `ensure_option_type_registered` upgrade on registration (1-line fix; aligned with its own doc comment which is currently aspirational), or (b) accept the race and stay on the existing narrow-with-explicit-tests pattern. Option (a) is the right answer; the doc comment claims to do it but doesn't.

  **Phase 2 migration plan (filed 2026-05-05).** The new wider-predicate alias `is_resource_or_contains_resource` lives at `src/ir/types.rs:469` (added in commit ff13b9b3). It's a thin alias over `needs_drop` with no callers — purpose is to make Phase 2 migration sites self-document the narrow→wide flip. The work units below are sized to land as independent commits; each starts with a probe (single callsite flip + full integration sweep) and only widens further if the probe is green. Per-cluster work units:

  **Phase 2 status update (2026-05-07).** Clusters 2/3/7 shipped; Clusters 4/6 closed as no-op (re-audit revealed they're already wide / already narrow-correct); Cluster 1 reverted (regressed 17 fixtures); Cluster 5 deferred to Tier 1b cleanup territory. See DONE entry "is_resource_type widening Phase 2" for full detail.

  **Cluster 1 — Late-registration upgrade (prerequisite). REVERTED 2026-05-07.** Commit `f072c27f` shipped the fix but commit `f4203406` reverted it: the upgrade surfaces a class of latent shallow-copy violations (Option__GorgetString, Option__PeerInfo, Option__ReliableStream at `match` scrutinee shapes) that Phase C's `validate_resource_moves` validator now flags as fatal. 17 fixtures regressed (`collection_types`, all `p2p_*`, `shared_stress*`, `vector_task_get`). Snag #24's "What I tried 2026-05-06" recorded this exact failure mode ("the violation count for FieldLoad is 2,568 per the Phase C TODO sweep"). Fix isn't safely independent: requires migrating FieldLoad/EnumFieldLoad shallow-copy lowering sites to emit Borrow rather than Copy when the source is a borrowed parent. Snag #24's option (b) (inline-drop scheme without metadata upgrade) remains the architecturally cleaner alternative for the leak-fix half.

  **Cluster 2 — Stylistic redundancy cleanup. SHIPPED 2026-05-07 (commit `221c671b`).**

  **Cluster 3 — Spawn closure-arg field MoveZero. SHIPPED 2026-05-07 (commit `49285ee3`).** No fixture exercises Option/Result-of-resource captures through spawn today; widening is correctness-driven for future code. With Cluster 1 reverted, the late-registered case stays leaky (the upgrade scan only fires module-level — late-registered Option types stay Resource-but-no-drop_strategy). Pre-registered Option types work correctly.

  **Cluster 4 — Phase C `validate_read` resource scope. NO-OP after re-audit (2026-05-07).** Re-audit of `validate.rs:1249` and `:1383` showed: after the module-level `upgrade_types_from_fields` pass runs, `is_resource_type(Option[Resource]) == true` (because `copy_semantics=Resource` is set on the upgraded Option/Result). The `is_resource_type` and `needs_drop` predicates differ only on FnPtr, which can't appear as enum payload. The validator scope is already wide for non-late-registered types. The "12,294 violations" historical mention referred to a now-closed Phase C migration (commit `9c23e7d0`).

  **Cluster 5 — `lower_var_decl` / `lower_assign` Branch F + ABI choice. ~3-5 days.** The big one: `src/ir/lowering/stmts/mod.rs:818,848,918`, `src/ir/lowering/stmts/assigns.rs:267,292,332,690,976,1110,1128`, `src/ir/lowering/exprs/calls.rs:34,668,1060,1089,1471,1427`, `src/ir/lowering/closures.rs:267,357,407`, `src/ir/lowering/functions.rs` (return-value Move-override + parameter ownership at 11 sites), `src/ir/lowering/traits.rs` (trait-method default impl mirror at 7 sites), `src/ir/lowering/generics/mod.rs` (monomorphization mirror at 6 sites). These are all variations on "is the rhs/source a resource we should Move?" / "is the target a resource we should Borrow?". Most should stay narrow (the regression is real for Ptr-wrapping); a subset (Branch F-style, where the answer drives drop accounting on Option/Result wrappers) should widen. Per-site decision via the methodology used in Phase D4 mat-of-five-cases probes (commits 7ce8e056, 404c8716): substitute, full sweep, decide. **Owner: 1 agent over 1-2 weeks; each site is a commit; rollback boundary preserved.**

  **Cluster 6 — Pattern lowering + IndexLoad/FieldLoad Ptr-wrap. NOT a migration target — re-verified 2026-05-07.** Spot-checked `patterns.rs:693,751`, `methods.rs:2724,2730`, `mod.rs:1772,1849,1883`, `for_loops.rs:322`. All want the narrow predicate: Ptr-wrapping decisions only fire on direct-resource fields/elements. Widening would emit `Ptr(Option[String])` for an Option-wrapped field, which `clone_fn_for_ptr` can't satisfy. **Stays narrow.**

  **Cluster 7 — `is_resource: &|tid|` callbacks in context.rs. SHIPPED 2026-05-07 (commit `ec31fc34`).** Removed dead `is_resource` callback field from `LookupCtx` and the two construction sites in `context.rs:485,603`. The field was declared in `builtins.rs:49` but never invoked by any LookupCtx consumer (verified via exhaustive grep).

  **Total Phase 2 effort estimate: 2-3 weeks for a single agent, with Cluster 4 (Phase C coordination) being the load-bearing one.** Clusters 1, 2, 3 are independent and can land first as warm-ups. Cluster 5 is the bulk; Cluster 6 is a documentation-only deliverable (annotate why these stay narrow so future agents don't probe them).

  **Estimated 1-2 days for the remaining 6 branches; honest scope is closer to a week given the elegance bar.** Risk: medium — touches CoW alias creation, mark_string_borrow_source, drops.unregister, register_local re-binding. Validation: full integration sweep + cow_materialization_points fixture must stay green at every step. Migrate one branch at a time by replacing it with the typed-match arm and verifying integration. **The branch-E migration showed the right pattern: when typed-state migration regresses, the regression is almost always a downstream consumer with a latent correctness bug that the sidecar was hiding — fix the consumer, then migrate. That's the elegance step, not a workaround.**

  **Discipline while deferred:** don't accumulate new branches in `lower_var_decl` whose predicates aren't expressible as liveness queries. Each new case must be reducible to `(target.is_resource, source_live, source.ownership())`; if a case requires reading a different axis, flag it here instead of adding the branch silently. [added: 2026-05-01, plan refined: 2026-05-04, branch E shipped: 2026-05-04]

  **Sub-TODO: `Option[Option[Resource]]` unwrap MoveZero may be insufficient (audit-level concern, no fixture exercises it).** [filed 2026-05-07] At `src/ir/lowering/exprs/methods.rs:589, 604, 662, 1858, 1870, 1901` — unwrap-then-MoveZero gates check `is_resource_type_local(dst)` where `dst` is the inner type after unwrap. For `Option[String] → String` (direct resource), the gate fires and source is zeroed. For `Option[Option[String]] → Option[String]` (transitively-resource wrapper), the gate misses: the inner `Option[String]` is byte-copyable at the GIR level (its drop is structural via the upgrade scan). After unwrap, source's `Some._0._0` (heap String) and dst's `_0._0` alias the same pointer → potential double-free at scope-exit. No currently-shipped fixture exercises nested-Option of resource (`tests/fixtures/test_option_all.gg:107` covers `Option[Option[int]]` only). The fix when it bites: widen the gate to `is_resource_or_contains_resource(dst_type)` — but only after Cluster 1 / FieldLoad shallow-copy migration lands, since the same regression class would surface here. Speculative; document the audit gap and defer.

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


- **C backend: migrate val_types to func.value_types**: Shared `compute_module_value_types()` runs after LIR optimization and populates `func.value_types`. The LLVM backend already reads from it. The C backend still uses its own single-pass `infer_inst_type` because its multi-phase fixups (guard accessor inference, CallExtern→SlotStore mismatch, cross-type map combinator) depend on `ptr_pointee` context computed in the same pass. Next step: seed the C backend's val_types from `func.value_types` and reduce the fixup phases. [updated: 2026-04-14]

- **LIR value origin metadata — enable Store/SlotStore/Call lifts**: The C backend maintains 5 origin bitmaps (`str_lit_vals`, `null_vals`, `cstr_vals`, `ptr_pointee`, `func_addr_targets`) beyond type info. These track value provenance needed for ~37 emit-decision sites. The type metadata (`func.value_types`) is now shared; origin metadata remains backend-local. Fix: attach origin tags to LIR values (e.g. `StrLit` → string-literal flag, `NullPtr` → null flag, `FuncAddr` → FuncId). Unblocks lifting Store routing (~50 lines), SlotStore string/cstr coercion (~22 lines), and Call/CallPtr ABI coercion (~100 lines). [updated: 2026-04-14]

- **Decompose emit_call_extern.rs (~1,850 lines)**: Tier 1-3 lifts complete — ~490 lines of inline expansion removed. Remaining: HOF inlining (map/filter/each/fold ~590 lines), printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-04-15]

## Medium

- **Self-host codegen: `module.items.get(i).unwrap()` inlined (vs. wrapper pass-by-value) silently breaks at one specific call site in typecheck.gg.** Stage-1 binary fails silently (no compile error, exit code != 0, no stderr). Bisect 2026-05-10: the breaker is `typecheck.gg:1404` (the second of two near-identical loops over `module.items`). The first loop (line 1390) inlines fine; only the second — which calls `type_check_item(item, ...)` after the get — regresses. Routing through the existing `get_item_at(module.items, i)` wrapper (which passes `Vector[Item]` by value) keeps it stable. Hypothesis: passing the Vector by value implicitly clones it before the loop, stabilising iteration; the inline form chains through the struct field directly, and something in `type_check_item`'s borrow pattern interacts badly with that. Workaround in place at typecheck.gg:39 — `get_item_at` is the only retained typed-accessor wrapper. The other 6 typed accessors and 2 len helpers were inlined cleanly. [added: 2026-05-10]

- **Self-host codegen: nested `Vector[Vector[T]].get(i).unwrap().push(x)` silently breaks downstream codegen.** Surface symptom: Rust gg compiles `preds.get(succ).unwrap().push(bi)` correctly, but the self-host's emission of the same source triggers stage-2 cc errors in *unrelated* downstream functions (e.g., `parser___token_float_val` returning `void*` instead of `double`). The workaround pattern in `tests/fixtures/self_host_lowerer/lir_ssa.gg:60-79` (extract inner Vector to a fresh local, push, set back) IS load-bearing. Confirmed 2026-05-09 by replacing the workaround with the chained form — bootstrap_fixed_point regressed at stage-2 cc with type-mismatch errors. Same shape applies to lir_ssa.gg:486-508 (`Dict[int, Vector[int]].get(...).unwrap().push()` case). Investigation needed: the chained form likely emits incorrect type info during the implicit get → mutate → put/set writeback that pollutes type inference for some other expression. [added: 2026-05-09]

- **Parser bug: `(method_call())` in boolean context parses as tuple start.** Reproduces with `if x and (d.contains("a")):` — parser emits "expected ',', found '.'" at the dot in `d.contains`. The `(...)` is read as a tuple-element list, hits the dot, fails. Workaround: drop the redundant parens (`if x and d.contains("a"):` parses fine). Affects ergonomics — perfectly natural-looking Gorget breaks. Likely in the recursive-descent expression parser's prefix handling for `(`. [added: 2026-05-09]

- **Self-host codegen bug: `Option[int].unwrap_or(default)` skips the None-check.** When source is `dict.get(K)` returning `Option[int]`, the self-host's emission is:
  ```c
  __v143 = gorget_map_get(dict, key);   // returns NULL on miss
  __v144 = *(int64_t *)(__v143);        // CRASH on NULL
  ```
  No NULL guard, no fallback to the `default` arg. Reproduces during `self_host_bootstrap_fixed_point` when `lir_lower.gg` calls `sr.get(name).unwrap_or(-1)` on a non-trivial Dict. The Rust gg compiler emits this correctly; the self-host's lowering of `Option.unwrap_or` is the gap. Workaround in place: `sr_lookup(&sr, name)` helper using `if sr.contains(name): return sr.get(name).unwrap()` (line 40-43 of `tests/fixtures/self_host_lowerer/lir_lower.gg`). Inline once fixed. Likely a missing `OptionUnwrapOr` lowering in `lower.gg`'s expression emitter, or LIR codegen not honoring the None-tag. [added: 2026-05-09]

- **`elem_type_to_meta` collection_kind migration deferred** — `lir/lower/insts.rs::elem_type_to_meta` (`:1941-1949`) routes `Vector__/Deque__` / `Dict__/HashMap__` / `Set__/HashSet__` element names to `ResourceKind::GorgetArray/Map/Set`. Probed migration to typed `gir_types.get_type_def(n).and_then(|td| td.metadata.collection_kind)` reads regressed `vector_task_get` (Got 2, expected 3) on 2026-05-10 — `register_collection_alias` doesn't always register the TypeDef before this path runs (cross-module, monomorph synthetics). The matching `is_monomorphized_wrapper_type` in `c_lir/mod.rs` and `box_inner_drop_fn` already use `struct_def_by_name` to handle this, but the GIR-side path here would need a similar registration-timing guarantee. Same shape as the `opaque_runtime_size` cleanup TODO. Reverted; left as the prefix-match fallback for now. [added: 2026-05-10]

- **Extend `noreturn` qualifier to builtins (`panic`) and other `_Noreturn` C functions.** Round 6 added `noreturn` to the parser/typecheck/IR pipeline and marked `std.os.exit` as `extern noreturn`. `panic` is a hardcoded compiler builtin (treated as void-returning at typecheck via `matches!(cname, "print" | "assert" | "panic")` in `typecheck.rs:1443`, lowered via a hardcoded `call_extern("gorget_panic", …)` in `stmts/mod.rs:1814`), so the noreturn pipeline doesn't reach it — calls to `panic()` in match-as-expression arms still hit the void-vs-T mismatch. Two options: (a) declare `panic` in `lib/std/io.gg` (or wherever) as `extern noreturn void panic(String msg)` and remove the hardcoded path; (b) extend the typecheck builtin special-case to return `never_id` for panic and add `"gorget_panic"` to `LoweringContext.noreturn_fns` at construction. Option (a) is the layering-discipline-correct answer — it removes a name-match in the compiler. Also audit `lib/freestanding/runtime.c` and `c_runtime.rs` for any other `_Noreturn` C functions exposed to Gorget (likely none today). [added: 2026-05-06 from JS-interpreter snag #12 round 6 follow-up]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]


- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — final consolidation step**: `c_sizeof_with_structs()`'s direct prefix matches have been retired (Vector/Dict/Set/Task/Box/Guard now route through `opaque_runtime_size`, which remains the canonical name → runtime-size table). The remaining work is to retire `opaque_runtime_size`'s `_ if name.starts_with("Vector__") => 64` etc. arms by reading `computed_c_size` from each monomorphized alias's resolved `StructDef`. Blocker: `c_sizeof_with_structs` takes `&[StructDef]` (no alias map), so resolving `Vector__int64_t` via `module.struct_aliases` requires plumbing the alias `HashMap<String, StructId>` through several call sites (`elem_size_from_monomorphized`, `concurrency_elem_size`, `dict_elem_sizes_from_monomorphized`, `c_sizeof_tuple_fields`). Once plumbed, the table arms collapse to typed reads; `Tuple__`/`Option__` recursive paths stay (they compute structurally). [updated: 2026-05-09]


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

- **Missing `from module import *` (module-level wildcard glob imports)**: Gorget supports named imports (`from module import X`) and enum-variant globs (`from module import EnumName.*`), but not module-level wildcard imports (`from module import *` — Rust's `use crate::ast::*`). This is the primary reason the five self-host programs use symlinks to share code rather than imports — without glob imports, every shared symbol must be explicitly named, making multi-file sharing impractical for large modules like `ast.gg` (50+ exported types). Implementation would touch: parser (wildcard import syntax), semantic resolver (bind all exported names from the module into current scope), and loader (ensure the module is fully resolved before the wildcard expansion). Prerequisite for self-host unification into a single program with `--stage` flag. [added: 2026-05-09]

