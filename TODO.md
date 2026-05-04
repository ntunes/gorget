# TODO

## High

- **Phase C2 — fix highest-frequency resource-move violations** (Stage C1 sweep landed 2026-05-03; full sweep across 1203 fixtures via `GG_VALIDATE_RESOURCE_MOVES=1` produced **11,447 `AssignMode::Copy`-of-resource warnings** before promotion. C2.1 landed same day, bringing count to **10,862** — `@ParseError__display` 412 → 0). The validator catches latent shallow-aliases of owned resources — bugs that don't trigger today only because the call patterns happen to dodge double-free.

  **Progress log (2026-05-03 — 31 commits, 11447 → 17, ~99.85% reduction):**
    - **C2.1** (`81c01959`): f-string string-deref `lower_interp_segment` emits `AssignMode::Clone` instead of default `Copy` when pointee is a string type. -585 violations. Layering correctness: GIR carries the typed mode, no longer relying on the C-backend "deep clone for Ptr→String loads" name-shape magic.
    - **C2.2** (`0986c140`): `lower_for`'s deref-of-Ptr step (`iter_local = *self_ptr`) emits `AssignMode::Borrow` for resource iterables. The local was non-owning by intent; the comment confirmed it. -2250 across the Iter derive cluster.
    - **C2.3** (`9d691ef2`): f-string interp temp `tmp = lower(expr)` picks Move/Clone/Copy by source shape. Closes `@ParseError__debug` + `@IoError__debug` (-733).
    - **C2.4** (`37f7001f`): validator skips projected destinations (`_x.field = ...` is FieldStore, not struct alias). -1081 false positives.
    - **C2.5** (`f06713bf`, part 1): validator skips bare-place auto-deref (`dst:T = copy src` where `src: Ptr<T>`). -492.
    - **C2.6** (`f06713bf`, part 2): `lower_for_iterable`'s collection_local store emits `Borrow` mode for resource iters. -2260.
    - **C2.7** (`41b3d168`): `Expr::Move` lowering's tmp emits `AssignMode::Move` for resources (the source is zeroed by the immediately-following `move_zero_and_mark`, so Move IS the typed contract). Closes `@JsonParser__fail` (-746).
    - **C2.8** (`e0523bf0`): validator extends auto-deref skip to explicit Deref-projection shape (`dst:T = copy src.*` where `src: Ptr<T>`). Closes `@Writer_for_File__write` cluster (-685 across 137 fixtures).
    - **C2.9** (`6755ef42`): `lower_match_stmt`'s scrutinee staging picks Move/Borrow/Copy by source shape. Hits `@DataFrame__col_*`, `@JsonParser__fail`, `@MultiGroupBy__agg` (-396).
    - **C2.10** (`0efb415b`): `lower_return`'s `use_move` predicate widened to "any bare-place needs_drop source" — the existing post-assign `move_zero` block runs anyway, so GIR mode catches up to runtime intent. Hits `@xtd__httpserver___parse_*` cluster (-427).
    - **C2.11** (`18570770`): validator skips type-mismatched bare-place assigns (`Vector = copy i64`) — those are generic-monomorphization bugs, not Phase-C-class shallow-alias bugs (-164).
    - **C2.12** (`e97af1cb`): expression-body return staging picks Move via new `assign_to_return_slot` helper (5 sites consolidated). Hits `@HttpServerResponse__{ok,html,json,…}` cluster (-279).
    - **C2.13** (`fc521d09`): use_move predicate ORs in `is_resource_type` for types where `needs_drop` is false but `is_resource_type` is true (trait-derived `VectorIter` etc.). Hits `@Vector__T__iter` (-494).
    - **C2.14** (`e3dffb41`): pattern-extract clone reassign emits Move mode (the cloned temp is fresh + dead at the single use). Hits `@WindowsIter / @ChunksIter` min/max (-197).
    - **C2.15** (`2ae29841`): `Expr::Is` boolean form scrutinee staging picks Move/Borrow/Copy by source. Hits `is_bindings.gg` (13 → 0) and all `if X is Variant(field):` patterns missed by C2.9. (-202)
    - **C2.16** (`1a415a31`): `cow_materialize_alias` and `cow_materialize_collection_ref` emit Move mode for the cloned-to-owned-local transfer. The cloned temp is fresh + dead. Hits `@JsonParser__fail` (31 → 0) and any path through cow_before_mutation on bare params. (-25)
    - **C2.17** (`d33b26f0`): `lower_catch_expr`'s 4 internal assigns (val staging / Ok-extract / Err-bind / recovery) pick Move mode for resources via new `mode_for` closure. Hits `error_conditional_throw.gg` (8 → 0) and any `expr catch (e):` with resource-typed Result. (-51)
    - **C2.18** (`d57664e4`, parallel agent): nested-match scrutinee staging — `lower_match_stmt_as_expr` and `lower_match_expr` both used default `Copy`. Factored C2.9 logic into `stage_match_scrutinee` helper applied at all three call sites. Hits `@DataFrame__col_{add,sub,mul}` (102 → 0). (-102)
    - **C2.18** (`a1e85e7e`, mine, distinct fix): Box deref `*box` lowering at `Expr::Deref` had two default-Copy assigns (shallow + clone-reassign). Now Borrow + Move. Hits `@eval` in option_box_enum (12 → 0). (-12)
    - **C2.19** (`93724694`): Ptr-CoW upgrade-on-mutate at `stmts/assigns.rs:86-101` — fresh clone written back via Copy now uses Move. Hits `@std__fmt___pad_*` (-6). (Worktree agent ac8a6b independently converged on same site.)
    - **C2.21** (`f9803e2a`): for-loop iter staging Borrow mode in `lower_for_array` and `lower_for_iterable` (C2.6 missed array path). (-132)
    - **C2.22** (`001b60b7`): for-loop iter staging Borrow in `lower_for_dict` + `lower_for_set` (C2.21 didn't catch them). (-15)
    - **C2.23** (`b9da47f7`): tuple destructure pattern picks Move/Copy by source for resource tuples. Hits @main Tuple cluster across http_urls. (-12)
    - **C2.24** (`89cdb7b5`): array literal element staging picks Move for owned/temp resources (nested vector literals). Hits `vector_of_vectors.gg` (-3) plus broader applicability.
    - **C2.25** (`bcb4837e`): `stage_match_scrutinee` defaults to Borrow for resource scrutinees (the match-doesn't-consume invariant is unconditional). Hits `@xtd__toml___stringify_section` (4 → 0) and tightens loop-reassigned named locals.
    - **C2.26** (`53dd0f86`): rethrow expression's 4 internal assigns pick Move via `mode_for` closure (mirrors C2.17 catch). Hits @validated_port, @transform, @process, @parse_positive (-13).
    - **C2.27 + C2.28** (`0def1ca0`): batch from 4 parallel diagnosis agents (Explore type, read-only, no integration tests). Sites:
       - `lower_shared_var_decl` (stmts/mod.rs): Move + `resource_assign_mode` helper for hidden_local / rwlock_local + tmp + facade_local.
       - `lower_assert` (stmts/mod.rs): Move for LHS/RHS resource materialization.
       - comprehension non-range iter (collections.rs): Borrow.
       - comprehension acc_local: set_owned + drop_register so var-decl picks Move.
       - `Expr::Deref` non-Box resource path (exprs/mod.rs): Borrow.
       - `lower_with` (stmts/mod.rs): Move.
       - `lower_compound_assign` string += and general op-result (assigns.rs): Move.
       - REVERTED: `lower_for_string` Move attempt — caused double-free in 2 fixtures (string_enum_variants, leak_known_patterns). Original Copy + drop_register kept; for-string flag stays as a known Phase C limitation.
       Combined delta: 46 → 19 (-27 violations).

    - **C2.29** (`5d307b59`): trait-impl FunctionBody::Expression at traits.rs:953,1472 mirrored `assign_to_return_slot` helper inline. Hits @FromRow_for_Row__from_row (1 → 0).
    - **C2.30** (`e7ab2232`): compound indexed assign idx_local + field-collection temp pick Borrow for resource sources. Hits compound_index_assign (1 → 0).
    - **For-string deferred** (`21c127a1`): tried option (b) — Borrow + no drop_register at lower_for_string:165 leaning on cap=0 sentinel. Regressed leak_known_patterns P5 (for-char loop leaked ~30B). Root cause: iter_op has THREE distinct shapes (literal cap=0 view, owned cap>0 clone from CoW, shared cap>0 borrow) that are bit-identical at runtime — neither cap=0 sentinel alone nor a uniform "is-view" runtime bit can distinguish them. The fix needs IR-level origin tracking (Phase D's BorrowOrigin queryable from this site) — option (c). Reverted with detailed in-place comment listing options (a)/(b)/(c) and the regression finding.

  **Remaining (17 violations — 99.85% reduction from baseline 11447):**
    - 16 GorgetString — entirely the for-string cluster (lower_for_string at for_loops.rs:165). Touches @tokenize, @is_balanced, @caesar_encrypt, @max_depth, @rate_password, @parse_positive, @test_string + @main across 8 fixtures.
    - 1 Vector__int64_t — shared_iterator_invalidation in-loop body residue, related to with-clause + spawn interaction.

  **The for-string cluster** is the gating C3 residue. Three iter_op shapes that can't be distinguished at runtime alone:
    Shape | cap | Owner | iter_local must
    ------|-----|-------|----------------
    1. literal view | 0 | static | drop no-ops anyway
    2. owned clone (CoW) | >0 | nobody else | DROP (P5 leaks otherwise)
    3. shared borrow | >0 | upstream | NOT DROP (would double-free)

  Today: Copy + drop_register works for shapes 1+2; shape 3 is latent but unhit (the CoW upstream always materializes a clone before for-string entry). Option (a) — always-clone — costs an allocation per loop entry but is safe. Option (c) — source-aware via Phase D `BorrowOrigin` — is the principled fix; ~1-2 days work. The Phase C validator will keep flagging until either (a) or (c) lands.

  **Phase B context:** the for-string finding is also evidence that Phase B (universal runtime view discriminator) wouldn't suffice on its own. Even with a per-resource-type "is-view" bit, the compiler still needs IR-level origin info to set the bit correctly for shape 2 vs shape 3. Phase D + Phase C precision is the right path; Phase B remains correctly deferred.

  **Validated externally (2026-05-03):** gorget-arena (51-file user project at `.worktrees/gorget-arena/src/main.gg`) hits **zero** Phase C violations after C2.25. Real-world signal that the major patterns are covered.

  **Audit lens (per user direction 2026-05-03):** every remaining shallow copy is either (a) a latent bug we fix in lowering, or (b) a missing IR / syntax / protocol primitive needed to express the necessity legally. There is no third "accept the violation" bucket — that would defeat the Phase C guarantee.

  **Phase C C4 blocker (discovered 2026-05-03 evening): the §6.8 LIR conflation.**

  The for-string cluster (~16 violations, lower_for_string at for_loops.rs:165) has three iter_op shapes (literal cap=0 view / owned cap>0 clone / shared cap>0 borrow) that the IR can't currently distinguish. Option (c) — query iter_op's source ownership state via Phase D's BorrowOrigin + dispatch Move/Borrow accordingly — is the principled fix. It needs `set_ref` to mean "no-drop borrow" without ALSO triggering LIR SlotLoad routing.

  But LIR's `is_ref()` predicate at 6 sites bundles two semantics:
  - **Slot kind** (Ptr-sized vs value-sized for layout) — should check TypeId.
  - **Drop discipline** (no-drop borrow vs owned) — should check ownership.

  Splitting `is_ref()` cleanly *without* touching the GIR doesn't work: the GIR has dozens of `set_ref(value_typed_local)` call sites that exploit the conflation to opt into LIR SlotLoad routing on value-typed locals. This works because of a **layout-coincidence dependency** — collection structs (GorgetString, GorgetArray) have the data pointer as their first field, so loading the first 8 bytes of the slot via SlotLoad happens to give the right pointer.

  **Concrete attempt and result (2026-05-03):**
  - Replaced LIR's 6 `is_ref()` calls with `is_ptr_typed_local` (TypeId-based).
  - Wide regression: ~50 fixtures failed across diverse patterns (dataframe, xml, yaml, closures, control flow). The `set_ref` callers were depending on the SlotLoad routing.
  - Reverted.

  **What full §6.8 needs (multi-day project):**
  1. Audit all `set_ref` callers in src/ir/lowering. Categorise each:
     - "Drop discipline only" — pure no-drop signal. Keep set_ref semantics.
     - "Slot routing" — opt into LIR SlotLoad. Replace with explicit Ptr-typed local OR new typed flag.
  2. Add a separate `slot_kind: PtrSlot | ValueSlot` axis on GIR Local (or LIR Slot per the original §6.8 vision).
  3. LIR's 6 sites read slot_kind for routing, ownership for drop.
  4. Validate via integration tests at each step.

  **Decision (2026-05-03):** Phase C C4 (promote validator from warning to error) is GATED on §6.8. The validator stays warning-only until §6.8 lands. The persistent flag on the for-string cluster (~16 violations) IS the durable signal that work remains. Option (a) — always-clone — was considered and rejected because it would silence the validator and bury the architectural debt under a comment that future readers would miss.

  **Approaches considered and rejected:**
  - (a) Always-clone for-string. Closes the cluster but silences the validator at the affected sites. Hides architectural debt. Rejected.
  - (b) Borrow + no drop_register. Regressed leak_known_patterns (shape 2 leaks). Mechanically wrong without per-shape dispatch.
  - Localized §6.8 subset (split is_ref alone). Wide regression because GIR set_ref callers exploit the conflation. Doesn't work in isolation.

  **Forward path:**
  - Document the layout-coincidence dependency at affected GIR call sites (search: `set_ref` near `add_local` of value-typed slots).
  - Land full §6.8 (the multi-day project above).
  - Then option (c) closes the for-string cluster.
  - Then C4 ships.

  **Stages remaining:**
    - C3 (audit): each remaining cluster needs per-site classification. Likely shapes:
      - Box-deref auto-materialization: `_x = copy box.*` in call-arg path (eval pattern). Either propagate Box-deref skip to validator OR emit Borrow mode at the lowering site.
      - Tuple / set / smaller residue.
    - C4 (promote validator from warning to error) — locked once C3 closes the residue.

  **Top violations by destination type** (sweep aggregation):
    2597 GorgetString
    1079 ReliableStream  (xtd.p2p — ~754 from p2p_*; one user-defined struct)
     635 VectorIter__int64_t + ~3000 other Iter__* types (auto-generated stdlib trait impls)
     497 Vector__int64_t
     493 Column           (xtd.dataframe)
     258 Vector__uint8_t
     211 Vector__GorgetString
     192 HttpServerResponse
     186 Dict__GorgetString__GorgetString

  **Top violation hotspots by function name:**
    675 @main (broad — many fixtures)
    412×2 @ParseError__display + __debug   ← derive-generated for stdlib ParseError
    137×2 @IoError__display + __debug      ← derive-generated for stdlib IoError
    754   @xtd__p2p___p2p_*                ← p2p library hand-written
    525   @WindowsIter__…__sum/min/max/...  ← derive-generated iter trait impls
    170   @xtd__dataframe___df_from_*

  **Pattern analysis.** The high-volume cases are NOT in the derive code generator — that emits correct Gorget source like `case ParseError.InvalidNumber(a0): return f"InvalidNumber({a0})"`. The bug is in pattern-extraction lowering when the binding feeds a value-consuming call (e.g. `gorget_string_format`). The current GIR shape is:

      _7 = enum_field_load _2, InvalidNumber, 0   ; *GorgetString  (correctly bound as Ptr)
      _8 = copy _7.*                              ; GorgetString  ← SHALLOW Copy mode here
      _9 = call_extern @gorget_string_format(..., copy _8)
      drop _8                                      ; frees data while _2 still holds the alias

  The dereference `_7.*` materializes for the call argument with `AssignMode::Copy`, producing a bit-identical alias. The fix is to emit `AssignMode::Clone` at this dereference materialization (equivalently: route through `auto_clone_if_ptr` instead of bypassing it). Today `auto_clone_if_ptr` returns the Ptr unchanged for string types (see `context.rs:1688` — "Cloning only at ownership boundaries"), but a value-consuming runtime call IS an ownership boundary; the bypass is the bug.

  **Estimated leverage:** fixing just this one materialization pattern likely closes 6000-8000 of the 11,447 violations (every `case Variant(a): use(a)` over a resource-typed field where `a` is consumed by a call). The remaining 3000-5000 split across:
    (a) Iter trait impl derives — same root cause via stdlib derive (Sum/Product/Min/Max/Join/Collect take their state by value)
    (b) Vector/Dict element pulls feeding consuming calls
    (c) Field projection of resource-typed struct fields without Borrow-mode binding
    (d) Function-return paths that bit-copy

  **Stage C2 plan** (single fix per commit, sweep-validated decreasing count):
    1. Pattern-extract dereference at call boundary → `Clone` mode (biggest single fix).
    2. Iter trait impl materializations.
    3. Field-load of resource-typed fields → bind as Ptr(T) borrow.
    4. Audit residue (Stage C3).
    5. Promote validator from warning to error (Stage C4).

  Sweep raw log preserved at `/tmp/c1-sweep.log` during the next session — regenerate via `GG_VALIDATE_RESOURCE_MOVES=1 ./target/release/gg build <fixture>` per fixture. [added: 2026-05-03]

- **Phase D4 deferred — reference-grade refactor of `lower_var_decl` decision tree** (deferred 2026-05-01 in favour of minimal-D4 → D6 → C sequencing). The current 5-branch chain in `src/ir/lowering/stmts/mod.rs` reads four sidecar-style predicates (`named_local`, `cow_unsafe_at`, `drops.is_registered`, `needs_drop`) that are mostly liveness proxies. Reference shape: promote `func_state.liveness` to first-class axis via `source_live_past(local, span)` query, collapse to ~6-arm typed match on `(target.is_resource, source_live, source.ownership())`. ~1-2 days when picked up. **Why deferred:** Phase C's validator (post-D6) will provide a stronger fixture regression net for the refactor — bugs surfaced after C lands are likely real CoW correctness issues, not refactor-induced regressions, which is a strictly better debugging position. **Discipline while deferred:** don't accumulate new branches in `lower_var_decl` whose predicates aren't expressible as liveness queries. Each new case must be reducible to `(target.is_resource, source_live, source.ownership())`; if a case requires reading a different axis, flag it here instead of adding the branch silently — the whole point of the refactor is to make that "what axis am I reading" question explicit. [added: 2026-05-01]

- **Phase A residuals — 3 follow-ups left after the 13-commit consolidation (2026-05-02).** The 9-site migration off name-prefix matching landed; three pieces remain, each with different cost/value:

  1. **Callable / MutCallable / ConsumeCallable / GorgetClosure TypeDef registration.** Closes 6 explicit `name.starts_with("Callable__")` arms across `clone_fn_for_ptr` (context.rs), `elem_drop_fn_for_type` + `elem_clone_fn_for_type` (lir/lower/types.rs), `clone_fn_for_collection_element` (calls.rs), `infer_drop_strategy` (drops.rs), and `needs_drop` (types.rs). Tricky because Callable lowers to `GirType::FnPtr` at locals (no Named) and only appears as `GirType::Named("Callable__…")` inside collections via `resolve_inner_type` fallback. Adding a TypeDef alongside without breaking the FnPtr-at-local path needs care — likely register the Named TypeDef but keep the FnPtr lowering path for direct local declarations. Drop_strategy = Trivial("gorget_closure_free"), clone_fn = "gorget_closure_clone_to_owned", clone_inplace_fn = "gorget_closure_clone_inplace", copy_semantics = Resource (or stays POD-shaped — see comment on needs_drop).

  2. **`collection_runtime_type` migration (lir/lower/mod.rs:1085).** 6 call sites all in mod.rs where `self.gir.type_registry` is reachable. Become `runtime_struct_for_collection_kind(metadata.collection_kind)` plus a Callable arm (resolved by item 1 above). Clean and contained.

  3. **`elem_drop_fn_for_c_type` (c_lir/helpers.rs:765).** c_lir backend doesn't have GIR access. Clean fix: cache `elem_drop_fn` / `elem_clone_fn` / `materialize_fn` on LIR `StructDef` at construction time, c_lir reads from there. Real architectural refactor — touches the Backend trait boundary. Tightly coupled with Phase D's "LIR-side per-value provenance" item (`unified-resource-model.md` §6.8); better to do after Phase D is in place so the metadata-on-LIR-struct shape is shared with `BorrowOrigin`-on-LIR-Slot. [added: 2026-05-02]

- **CoW materialization of String views through expression-temp chains — three of seven boundary points silently corrupt** [BLOCKED ON Phase C2/C3 — substrate from Phase D is in place; this is downstream sweep work] (surfaced 2026-05-01 via the JS-interpreter project's snag #5; reproducer in `tests/fixtures/cow_materialization_points.gg`). The chain shape `String x = vec.get(i).unwrap().trim()` (any view-returning string method on a non-fresh receiver) loses ViewOf provenance: the trim-result temp is owned by default in `call_tracked`, the temp's view nature is only re-declared by `set_view_of` for **named** result locals (`src/ir/lowering/exprs/methods.rs:2197-2214`'s `is_named_local` guard, kept since trim's result here is a temp), VarDecl propagates the spurious Owned tag onto x, and downstream materialization checks at `Some(x)` / `Holder(x)` / `String y = x` / `h.field = x` skip the required clone — payload becomes a dangling pointer once vec mutates. Boundary points 1 (reassignment), 3b (enum init), 3a sometimes, and 7 (field store) are affected. Points 4 (collection store) and 5 (return) are saved by the runtime `gorget_string_materialize_inplace` hook and the explicit return-side clone, respectively. **Same bug also reproduces with substring on a borrow-source** (`String src = vec.get(0).unwrap(); String view = src.substring(...); Some(view)` → mojibake when vec mutates), confirming the issue is general loss of transitive view provenance, not chain-specific.

  **Why now blocked on Phase C2/C3, not D (revised 2026-05-03):** Phase D delivered the substrate (`BorrowOrigin::RuntimeView` on `LocalOwnership`, D6 lifted onto `Local` directly — the codegen-ABI mismatch from the original entry is gone). But the actual closer is Phase C's validator-driven sweep. Running `GG_VALIDATE_RESOURCE_MOVES=1` against `cow_materialization_points.gg` surfaces **5 shallow-Copy-of-resource violations** in the *direct* (non-chain) sub-cases that happen to work at runtime — exactly the C2 pattern of "Copy where Move/Clone/Borrow is correct". The chain cases are the same shape with one extra dereference layer: when C2 sweeps the remaining 658 violations and C4 promotes the validator to error, these fall out for free. Validating against the fixture as part of the C3 audit will catch them. Patching `lower_var_decl` ahead of C3 also bumps into the deferred Phase D4 discipline rule (don't add branches not reducible to `(target.is_resource, source_live, source.ownership())`).

  **Acceptance test is already in place:** `tests/fixtures/cow_materialization_points.gg` exercises all 7 boundaries × 2 source shapes (direct borrow, chained view). The three currently-broken sub-cases (`p1_reassign_chain_trim`, `p3b_enum_chain_trim`, `p7_field_chain_trim`) are commented out in `main()` with TODO markers. Uncommenting them + updating `.expected` to print `hello` for each is the acceptance test for C3 completion (or for C4 promotion-to-error proving the latent bugs got fixed not just suppressed). [added: 2026-05-01, blocked-on revised 2026-05-03]

- **Field-store of fresh `Some(literal)` is a silent no-op** (surfaced 2026-05-01, JS-interpreter snag #4b). `fm.desc = Some("hello")` where `desc: Option[String]` compiles, runs, and silently fails to update the field — `fm.desc.is_some()` returns false. The C codegen (`emit_field_store_with_cleanup`) writes only the `Some_0` payload Str into the destination Option slot, leaving the discriminant tag at 0 (None). Repro: `struct FM: Option[String] desc / FM fm = FM(None) / fm.desc = Some("hello")`. Worst class of bug — wrong output, no diagnostic. Likely the lowering for `field = enum_constructor_call` projects through to the inner field instead of constructing a fresh enum value and storing the whole thing. Workaround in user code: build the struct via positional constructor at the end, never reassign Option-typed fields after construction. [added: 2026-05-01]

- **Drop fragile `is_fresh_allocating_extern` name list in `src/ir/lowering/context.rs:2482`** [BLOCKED ON Phase A — `docs/internals/unified-resource-model.md` §3.6 (RuntimeDecl table)]. Same shape as the no-name-matching rule's anti-pattern, just predates it. The list enumerates which runtime callees return fresh-allocated GorgetString (replace, upper, lower, repeat, pad, join, str_cat, format, int_to_str, etc.). Drives `fresh_string_locals` tracking which several CoW decisions read. Each new fresh-allocating runtime helper rots this list silently.

  Phase A's runtime declaration table (§3.6) is the right home: `RuntimeDecl` carries a typed signature plus per-fn flags (`returns_fresh`, `returns_view`) sourced from one TOML, with a `build.rs` emitter producing the typed registry. `call_tracked` then reads `RuntimeDecl::returns_fresh` instead of name-matching. The Phase A-Rust foundation (commits 085c97cf onward) put the type-axis half in place; the runtime-fn half is "what's left of Phase A" per §3.6 ("Estimated effort: 3-4 weeks ... bulk of what's left"). Don't touch this list before that lands — the right fix is to delete the function entirely once the flag is on `RuntimeDecl`, and a piecemeal rename now would conflict.

  Same refactor unblocks the right-shape fix for the materialization snag above — both depend on the call result carrying typed provenance metadata, not on `matches!(name, "...")`. [added: 2026-05-01, blocked-on tagged 2026-05-02]

- ~~Self-host typechecker: comment-length-dependent type binding loss~~ **CLOSED 2026-05-01** — root cause was NOT a Dict[String, _] hash-collision issue but cross-file span collision in `resolution_map: Dict[int, int]`. Fixed via per-module `parse_source_with_offset` plumbing in `self_host_typechecker/{lexer,parser}.gg` and `self_host_{check,lowerer}/loader.gg`. cli_basic.gg and encoding_basic.gg now pass check_comparison. See DONE.md entry. [closed: 2026-05-01]



- **SECURITY: `Dict[K, Callable].get().unwrap().clone()` double-frees the closure env** (surfaced 2026-04-28 right after landing the deep-clone fix). `Dict.get()` returns `Option[V]` (by value), unlike `Vector.get()` which returns `Option[Ref[V]]`. The unwrap result is therefore a Callable VALUE that shares its `env` pointer with the Dict slot. With the new `needs_drop` returning true for Callable types, the unwrap result gets a scope-exit drop registered → `gorget_closure_free` runs on the shared env → next free (the cloned local OR the Dict's val_drop on its own slot) hits a freed allocation. Reproducer: `Dict[String, Callable[int(int)]] d; d.put("k", (int x): x); Callable[int(int)] g = d.get("k").unwrap().clone(); print(g(1))` → `free(): double free detected in tcache 2`. Strings dodge the same shape via CoW (cap=0 views skip free in `gorget_string_free`); Callable has no view/owner distinction. Fix options: (a) make `Dict.get` return `Option[Ref[V]]` for resource value types — symmetric with Vector, but bigger surface change; (b) add a view flag to GorgetClosure so `gorget_closure_free` no-ops on shared envs — costs an ABI bit; (c) mark Callable values from non-cloning extern paths (`__option_unwrap` on `Option[Callable]`, etc.) as CowBorrow so the IR doesn't register a drop on them — narrowest surgical fix. (a) is the cleanest design. No fixture exposes this in the integration suite today (httpserver uses Dict[String, Callable] but only invokes through dispatch, never extracts via `.clone()`); the gap will bite the first user code that tries to extract a Callable from a Dict. Same architectural bug applies to `Set[Callable]`. [added: 2026-04-28]

- **Mutex double-lock deadlocks silently** (audit 2026-04-23). `Guard[int] g1 = m.lock(); Guard[int] g2 = m.lock();` compiles cleanly (`gg check` reports OK) and the runtime hangs on the second lock. Non-reentrant semantics are fine; the borrow checker should detect the live Guard and reject the second lock at compile time. Fixture `attack_56_mutex_double_lock.gg` (not wired in the harness — the deadlock-test would need a timeout variant of `security_known_unsafe`; deferred). Low severity: deadlock is visible, not silently wrong. [added: 2026-04-23]

- **Residual: `Option[Box[T]]` / `Result[Box[T]]` field drops not emitted on enum variants and struct fields.** The Box-field-drop wrapper `Box__T__drop` and its wiring at struct/enum-variant scope-exit (cases a + b + c of the prior Box[T] item) were closed 2026-05-01 — see DONE. The Option/Result ENUM-VARIANT skip at `populate_recursive_drop_enums` (mod.rs:471-481) and STRUCT-FIELD skip at `populate_recursive_drop_structs` (mod.rs:412-422) was kept intentional: enabling the drop crashes the self-host `resolve_stmt` path because `get_stmt_at(stmts, i)` (resolve.gg:24) returns `Stmt` by value via `v.get(i).unwrap()` — a shallow copy that aliases the vector's interior box/string pointers; both copy and source drop, and dropping the `Option[SpannedExpr]` field inside `Stmt` double-frees the SpannedExpr's Expr/string that the standalone SpannedExpr drop already freed. The proper fix is at the COMPILER level: make `Vector[T].get(i)` for resource T auto-clone (deep) or return `Ref[T]`-only (forcing the caller to .clone() at the boundary). Once that lands, the Option/Result drop skip can be removed and `option_box_enum.gg`'s 3 leaked Some(Box(...)) blocks will free correctly. Today: leak (3 blocks for option_box_enum), not unsoundness. [added: 2026-05-01]

- **Drainable for `Set[T]` / `Dict[K, V]`** — sibling capability trait shipped on Vector 2026-04-27 (O(n) reverse + pop). Set/Dict drain not yet equipped — they'd need a runtime helper `gorget_map_drain_entry(map, idx, out_key, out_val)` that moves the K/V out of the bucket and tombstone-marks the slot so the source's drop doesn't double-free, OR the equivalent move-out-of-collection-slot machinery applied to GorgetMap's bucket array. Today `Set.drain()` / `Dict.drain()` doesn't exist; users wanting drain semantics call `.iter()` + `.clone_each()` (clones every element) or build their own drain iterator over the bucket array. Priority: low until a real consumer needs it. [added: 2026-04-27]

- **Lazy `Dict.keys()` / `.values()`** — residual after Dict/Set lazy iter shipped 2026-04-25. `Dict.iter()` / `Set.iter()` are now lazy bucket-walks via `Ref[Dict[K, V]]` / `Ref[Set[T]]` borrow fields (DictIter / SetIter in `lib/std/iter.gg`). The matching `.keys()` / `.values()` projections still allocate eager `Vector[K]` / `Vector[V]`. Two natural follow-ons now that the borrow-field plumbing exists: (1) `DictKeysIter[K, V]` / `DictValuesIter[K, V]` state-machine structs, same shape as DictIter, that yield only the K (or V) component; (2) thin user-space wrappers — `d.keys()` returns `DictKeysIter[K, V]`, `d.values()` returns `DictValuesIter[K, V]`. Today users get the same effect via `d.iter().map(((K, V) p): p.0)` (verbose). Low priority — eager `.keys()` / `.values()` are unchanged behaviour, the gap is just allocation efficiency, not correctness. [demoted: 2026-04-25]


- **Self-host silent-fallback audit — IN PROGRESS**. Diagnostic wiring shipped (commit af0cb513): three sites now emit `/* [bug] ... */` comments in the generated output instead of silently returning sentinels: (a) `map_binop` unknown operator, (b) `EIdentifier` unknown name, (c) `EFieldAccess` unknown field. Also added `map_compound_binop` for `+=` / `-=` / ... spellings (commit 299ffb0c — was the root cause of `last_us -= 1` → `last_us += 1`). Still to audit / tighten: `infer_method_return_type` I64 fallback for unknown methods, `collection_element_type` "" fallback for unknown prefixes, `type_id_to_name` "int64_t" fallback for non-GtNamed tids. Attempted a cap on `[bug]` emission count but stage-1's lowerer doesn't reliably propagate mutable global state so the counter stayed at 0 — reverted to loud-by-default; callers can filter via `| grep -v '\[bug\]'` and dedupe via `sort -u | uniq -c`. Proper env-var toggle (`GORGET_QUIET_FALLBACKS=1`) deferred until the env-var reader lands in the self-host. [revised: 2026-04-24]

- ~~Self-host Dict[String, _] state-loss~~ **CLOSED 2026-05-01** — root cause was `lir_codegen.gg`'s `__gorget_map_new_sized_` magic-name expansion always emitting `gorget_map_new(sizeof(K), sizeof(V))` regardless of K — for K=Str this produced `hash_fn=NULL` maps that fell back to byte-FNV on the 32-byte Str struct. Fixed by routing K=Str/GorgetString to `gorget_map_new_str(sizeof(V))`. See DONE.md entry. Stage-1 mono-gen hang at mi=64/73 also resolved (same bug). Six Vector-pair workarounds (`loaded`, `LowerCtx.named_locals`, `GirModule.const_decls`/`none_decls`, `seen_instances`, `fn_templates`, `equip_generic_names`) are likely now redundant; one (`loaded`) reverted as proof-of-fix. **Follow-up (low priority):** revert the other five for code cleanliness. The historical analysis below is preserved for context. **Original symptom (now historical):** stage-1 hung at ~mi=64/73 in mono-gen emission (2026-04-24). The self-host-compiled `Dict[String, _]` silently loses `hash_fn` somewhere in the LowerCtx/GirModule lifecycle — `put` succeeds, immediate `contains` succeeds, but a later `contains` on the same key (via a different String instance with identical content) returns false. `gorget_dict_new_str` IS emitted at init, so the hash_fn is set correctly at that point; the loss happens downstream. Minimal reproducers all work. The six workarounds (93f20493 through a7b03b1c): (1) `load_imports.loaded: Dict[String, bool]` → `Vector[String]` + `loaded_contains`. Module load events went 722 → 24 (std.collections loaded 286× → 1×). (2) `LowerCtx.named_locals: Dict[String, int]` → `nl_keys: Vector[String]` + `nl_vals: Vector[int]` + `nl_contains/nl_get/nl_put` helpers. (3) `GirModule.const_decls: Dict[String, int]` + `GirModule.none_decls: Dict[String, bool]` → parallel Vectors + `const_contains/const_lookup/const_put/none_contains/none_mark`. (4) `GirModule.enum_names + enum_variants` registry (new) to work around `gmod.type_infos` Dict bug for match lowering. Paired with `lower_ctor_pattern` fallbacks — qualified `ParseError.Empty` split-on-dot, or bare `EBinaryOp` scan-all-enums. Mirrors Rust's `ctx.resolve_enum_variant`. `PConstructor on non-enum` failures: 1185 → 92 (−92%). (5) `seen_instances: Dict[String, bool]` → `Vector[String]` + `seen_has`. **The transitive-generic fixpoint was hanging forever** because `seen.contains` kept missing, so each round re-added 'already seen' instances and `transitive_changed` stayed true. (6) `fn_templates: Dict[String, FunctionDef]` + `equip_generic_names: Dict[String, bool]` → parallel Vectors + `keys_index_of`. Also landed: `GorgetString__debug` / `__concat` / `__eq` / `__format` runtime mapping in lir_lower + matching return types in lir_codegen (the self-host was emitting the non-existent `gorget_str_debug` once enum-fallback unblocked ParseError/IoError derive-Displayable match arms). **Final numbers on stage-1 lowering driver.gg:** `[bug]` diagnostics 58k → 3082 (−95%). `[lower_fail] PConstructor` 1185 → 92 (−92%). Pipeline reaches main lowering loop (571 functions lowered out of 783 items), then hangs in mono-gen emission at mi=64 of 73 (SIGKILL ~21s CPU with no OOM signal — suspect another Dict[String, _] field or a Vector.contains interaction inside `lower_generic_function` / `setup_type_subs`). **NEXT:** instrument `lower_generic_function` and friends to find the remaining hang point. See feedback_selfhost_workarounds.md #9. Non-comment lines emitted: still 0 (stage-1 exits before reaching `generate_c`). [revised: 2026-04-24]

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

- **`shared_callable.gg` fixture blocked on Phase A residual #1** (added 2026-05-04). Documentation paragraph added to `docs/book/16-smart-pointers.md` explaining `Shared[Callable[T]]` for shared mutable captures vs plain `Callable.clone()` for independent copies. Fixture deferred: writing `Shared[Callable[int()]] tick = Shared[Callable[int()]](body)` triggers a C compile error — the LIR emits `Shared__Callable__GorgetClosure__new(Callable__GorgetClosure val)` but `Callable__GorgetClosure` has no TypeDef registration (Phase A residual #1: "Callable / MutCallable / ConsumeCallable / GorgetClosure TypeDef registration"). Once that lands, the fixture can be added back to demonstrate the composition end-to-end via `gg run`.

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

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]

