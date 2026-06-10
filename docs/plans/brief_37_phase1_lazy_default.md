# BRIEF — #37 Phase 1: Full lazy CoW materialization (incl. loops) as the PRODUCTION DEFAULT in Rust `gg`

Status: v5 (pass-4 review folded 2026-06-10: ⚠ **W3c ADDED — the W3b choke point
is NOT singular** [p4-R1, BLOCKING]: string INDEX/SLICE syntax (`s[0..5]`, `s[0]`)
produces the same cap=0 view class via `lower_index_access` →
`gorget_str_slice`/`gorget_str_index` (both return `gorget_str_view_region` views,
`runtime_string.c:714/:739`), bypassing `builtin_returns_view` entirely — and the
NAMED index bind `String t = s[0..5]` is NOT Branch-E-protected (index results
carry no View tag). Three failing programs RUN-PROVEN, all ASan-silent; the v4
sibling-grep instruction ("grep returns_view consumers") was itself the trap —
v5 rewords it to enumerate view-PRODUCING runtime emitters. Pass 4 PROTOTYPED
BOTH W3b and W3c at located hook points and RUN-PROVED them (all probes flip to
eager-identical at exact clone parity; witnesses stay 0/1/0; in-loop returns_view
4v4 with the flag guard doing real once-only work; substring named bind 2v2
undisturbed; commit-ordering green-by-construction verified incl. the ungated W2
additive parts; Step-0 `--sanitize` feasibility confirmed `src/main.rs:2414/2566`
→ `:866-867`). Line-ref drift fixed [p4-R2]. v4 folded pass 3 (W3b re-promoted —
demotion RUN-REFUTED); v3 folded pass 2 (W3a PROTOTYPED + RUN-PROVEN, gate-ON
sweep 1193/9 all-D2); v2 folded pass 1; v1 = orchestrator draft from the scout;
scout worktree `agent-ae10a3b18c5a97df7`; numbers regenerated at tip `a044a10f`.)

## Mission

Promote the RUN-PROVEN lazy loop-carried CoW prototype
(`docs/plans/cow_loopcarried_prototype.diff`, env-gated `GG_COW_LAZY_LOOP=1`) to the
**default** lowering in Rust `gg`, fixing the defects the scout's default-on sweep
exposed (D1 derived-view/alias class, D2 borrow_view C-ABI, H2 FieldPath UAF →
excluded), landing the witnesses + canaries as integration tests, and removing the
`GG_CLONE_TRACE` instrumentation. Observable program behavior must be byte-identical;
only clone counts change (dead mutation path: 1→0; taken path: exactly 1).

This is a production-DEFAULT CoW behavior change. Doc grounding: the documented
intent IS lazy materialize-on-mutation (`docs/language-design.md:219/368/2236/2409`;
`docs/devbook/11-copy-on-write.md` — var_decl is deliberately not a clone site;
mutation-severs via `cow_before_mutation`; the cap==0 view discriminator + runtime
materialize hooks). This change moves the implementation toward the spec.

## Ground truth (scout-verified at `a044a10f`, pass-1-reviewer re-verified; line
numbers from the UNPATCHED tip — re-grep before editing)

- Prototype `git apply --check` clean for the diff portion (lines 1–303); the 3
  witness fixtures are appended after the `=== NEW FILE` marker at line 304 and must
  be created as files.
- Legacy bind gate `source_is_cow_borrow && safe_in_loop && !source_mut_unsafe` at
  `src/ir/lowering/stmts/mod.rs:777`; `source_mut_unsafe` computed `:766-776`; the
  eager clone branch `:819-829`. Mutation dispatch `cow_before_mutation`
  `src/ir/lowering/context.rs:2654`, Case 3 refs loop `:2722-2731`. Prescan writer
  `compute_cow_reassigned_after` `src/ir/lowering/functions.rs:234` (mutating-method
  name list `:260-265`; `&`-arg / implicit-mut-sig / `!`-move collection `:446-505`).
  `lower_var_decl_assign_mode` branches at `stmts/mod.rs:1203+` (Branch C alias
  `:1203-1222`, Branch E View-tag clone `:1246-1254`, Move-steal F/G after).
  View-tag single choke point `methods.rs:2645-2665`; `returns_view` typed field
  `builtins.rs:71` (14 String methods).
- Witness clone counts (per-call `GG_CLONE_TRACE`, the only trustworthy counter;
  reproduced independently by scout AND pass-1 reviewer): `witness_never` eager 1 →
  lazy **0**; `witness_taken` lazy **1**; `witness_cond_straightline` 1 → **0**.
  Stdout byte-identical in every case. LLVM backend spot-check (scout): same counts.
- Default-on full integration (scout): **1192 passed / 10 failed**, reducing to D1
  (1 test) + D2 (9 tests, all "Build failed for self_host_lowerer/driver.gg").
  `cargo test --lib` 1072/0. 15 fixtures change lowering (emitted-C
  `gorget_string_borrow_view` grep): 3 witnesses + `collection_types,
  cow_borrow_field_access, cow_borrow_outlives_push,
  cow_element_borrow_source_mutate_with, cow_materialization_points,
  cow_p3_cond_nested_mutate, cow_p3_match_nested_mutate, dataframe_csv,
  dataframe_ops, empty_literal_struct_field, yaml_multi, yaml_parse` + the
  self-host driver (the 2 D2 sites).
- **ASan found a silent UAF the green suite missed** (`empty_literal_struct_field`,
  FieldPath shape) → ASan over the affected set is a mandatory gate. **BUT ASan is
  BLIND to the D1 wrong-output class** (reviewer-proven: `d1_alias` and
  `d1_movesteal` print garbage under lazy yet are ASan-clean) **AND can be silent
  even on genuine view-into-element heap UAFs** (pass-3-proven: the W3b probes are
  real UAFs that an `__asan`-instrumented binary runs without any report — likely a
  pool-allocator free path) → stdout fixture assertions are the PRIMARY net for
  BOTH the wrong-output and the view-UAF classes; ASan is supplementary
  defense-in-depth, not the safety argument.
- Mechanical safety insight: the cap=0 view copies the element's 32-byte `Str`
  header — `data` points at the element's character buffer, not the array backing
  store; `push`/`insert`/`sort`/`reverse`/`swap` move headers and cannot invalidate
  it. Only element-destroying ops can (element overwrite/`set`, `remove`/`clear`/
  `pop` via `elem_drop`, collection drop/reassign/move) — and each routes through
  `cow_before_mutation`-family dispatch (reviewer re-derived + probed: reassign
  `assigns.rs:70-72`, `v[i]=x` `:744-751`, mutating methods via typed `needs_mut` /
  registry-driven `is_mutating` `methods.rs:1623-1652`, `&`-args incl.
  `&`-param-rooted collections, `!v` move; `!v` inside a loop is a semantic error).
  One sibling dispatch path lacks lazy routing — see W4 sever-audit.
- Multi-mutation-site is SOUND today: `restore_locals` (`context.rs:1455-1486`)
  reverts per-branch ownership so each branch-arm mutation site re-finds the tag and
  emits its own guard (probe: 2 guard callsites, first dynamically dead, exactly 1
  runtime clone, correct output). Same-straight-line later sites are covered by
  dominance. No fix needed; document in code comment.
- Escapes of a still-view `s` (return / push / capture / field-store) are covered:
  the local stays `Borrowed{CollectionElement}` so `ensure_owned_at_boundary`
  (`context.rs:1752+`) / `ensure_owned_at_consuming_arg` (`:1923+`) clone at the
  boundary; field-stores route through `clone_ptr_rhs_if_needed`
  (`assigns.rs:646`); runtime `*_materialize` hooks (`runtime_string.c:236`)
  upgrade any cap=0 view landing in a collection slot. Pass-2 probes under lazy:
  plain assign `x = s` (safe via the `assigns.rs:178-196` clone guard),
  push-escape, return-escape, `Some(s)` ctor, substring named bind — all correct.
- **Named binds of view-returning methods are ALREADY SAFE** (pass-2 finding):
  `String t = s.substring(0,3)` clones via Branch E's View-tag handling. **But the
  UNNAMED view-temp class is REAL, not theoretical (pass-3 RUN-REFUTED the
  demotion):** probe 1 `show(s.substring(0,5), &v)` with the callee doing
  `v.set(0,…)` then printing the param → eager `a = hello`, lazy garbage; probe 2
  `String t = s.substring(0,5) + poke(&v)` (callee mutates v) → lazy garbage.
  Emitted-C mechanism confirmed: `borrow_view` bind → `gorget_str_slice` on the
  view captures the raw element-buffer pointer → the `&v` dispatch materializes
  `s` itself (correct) but not the temp → `gorget_array_set` frees the old buffer
  → stale temp read. Both probes ASan-SILENT. Bounding probes PASS (direct
  `f"{s} {poke(&v)}"` — formatting copies bytes immediately; direct
  `show(s, &v)` — header-move insight): the failing class is exactly
  view-of-lazy-view temps AND named view binds with no View tag. **The class has
  TWO compiler emit routes (pass-4 finding):** (1) `returns_view` builtin methods
  (sole consumer dispatch `methods.rs:2645`); (2) string INDEX/SLICE syntax —
  `Expr::Index` → `lower_index_access` (`exprs/mod.rs:226` → `methods.rs:3170+`),
  LIR rewrite `Str[range]→gorget_str_slice` / `Str[int]→gorget_str_index`
  (`src/lir/lower/insts.rs:895-955`), both returning `gorget_str_view_region`
  views — which NEVER consults `returns_view`, and whose results carry NO View
  tag (so even the NAMED bind `String t = s[0..5]` fails: run-proven garbage,
  plus `show(s[0..5], &v)` and `show(s[0], &v)`). Fixed in scope by W3b + W3c.
  Note the false comment at `methods.rs:3291` ("returns a new Str value (not a
  borrow)" — it returns a view region); fix it as part of W3c.
- `gorget_string_copy_cow` is view-preserving (`runtime_string.c:214-219` — cap==0
  in → view out). `gorget_string_borrow_view` is currently NOT a registered runtime
  callee; the typed registry + `AbiKind::Ptr` + `Inst::AddressOf` (`lir/mod.rs:762`)
  path exists for W2.

## Scope decisions (decided; reviewers scrutinize, executor implements)

- **String-only in Phase 1, typed-registry-structured.** `gorget_array_free`
  (`runtime_array.c:247`) runs `elem_drop` whenever `data != NULL` regardless of cap
  — a cap=0 array view would double-drop every element; Dict/Set similar; user
  structs have no view discriminator. Generalizing needs view-aware runtime frees →
  Phase 1b+. Structure NOW for it: add `borrow_view_fn: Option<&'static str>` to
  `BuiltinTypeProtocol` (`src/ir/lowering/builtins.rs`), sibling of
  `clone_fn`/`drop_fn`, `Some("gorget_string_borrow_view")` for String only;
  eligibility reads the typed accessor. No name-matching anywhere
  (devbook/24 rules 2/3).
- **FieldPath sources EXCLUDED (closes H2).** Eligibility requires the collection
  source to be a plain local (`CollectionId::Local`-shaped, not FieldPath). Two
  reasons, both RUN-proven: `cow_before_field_mutation` (`context.rs:2788-2801`)
  lacks lazy routing (probe: correct output but 20 clones vs eager 1), and
  root-struct mutation via `lower_field_assign` (`assigns.rs:494-510`) does not walk
  descendant FieldPath refs (the `empty_literal_struct_field` UAF). FieldPath lazy =
  Phase 1b TODO; the descendant-walk severance gap gets its own TODO entry (latent
  pre-existing structure, only unsafe under lazy).
- **EIndex (`String s = v[i]`) stays eager** (it never sets the `cow_borrow_sources`
  sidecar, so `source_is_cow_borrow` is false → unchanged behavior; reviewer-probed
  eager-equivalent). Phase 1b TODO, along with unifying the typed
  `collection_ref_source` origin vs the sidecar map (dual-store smell, devbook/24
  rule 3).
- **`for s in v:` is already zero-clone** (`index_load_borrow`, never reaches
  `lower_var_decl`) — out of scope. `Dict.get(k).unwrap()` is in scope and
  RUN-proven (`dict_get_unwrap_push_chain`).

## The work

### W1 — Promote the prototype, default-on
Apply `docs/plans/cow_loopcarried_prototype.diff` (split out the 3 fixtures), then:
remove the `GG_COW_LAZY_LOOP` env-gate (the lazy arm becomes the default branch —
but see Constraints: the gate removal lands in the FINAL commit so intermediate
commits stay green); **add the `CollectionId::Local`-only eligibility check** per
the H2 scope decision (the prototype has NO such check —
`CollectionId::{Local,FieldPath}` at `stmts/mod.rs:769-771`; without this line the
default ships the `empty_literal_struct_field` UAF); delete the `GG_CLONE_TRACE`
fprintf block from `gorget_string_clone_to_owned` (`runtime_string.c`); keep
`gorget_string_borrow_view` but REWORD its comment and the
`emit_lazy_loopcarried_borrow`/`cow_materialize_view_lazy_in_place` doc comments
for production (drop "PROTOTYPE (cow_loopcarried scout)" and the stale "Gated on
GG_COW_LAZY_LOOP=1" lines).

### W2 — Typed registry + D2 ABI fix
`borrow_view_fn` field on `BuiltinTypeProtocol` as above. Register
`gorget_string_borrow_view` as a typed runtime callee (the same registry every other
runtime helper uses — `AbiKind::Ptr` arg + `Inst::AddressOf`-style operand shaping;
it is currently unregistered). Fix the deref-projection ABI hole: when the bind
operand carries a Deref projection (emitted as `*(Str*)ptr`, a value, where
`const Str*` is expected — the 2 self-host-driver sites
`lir_lower___map_runtime_name`, `lir_codegen___resolve_sizeof_c_type`), normalize
the operand (pass the pre-deref pointer / take a borrow temp) so the C types line
up. Pass 2 reproduced both sites exactly (`gorget_string_borrow_view(*(Str*)__vN)`
value-vs-`const Str*`; the pre-deref pointer is available) — implementable as
specified. `fixed_point` is the proof this is closed.

### W3 — D1: the lazy-source read class (the load-bearing new work)
Defect class: a read that captures a lazy-view local's VALUE or ADDRESS into another
binding loses provenance to the collection, so Case 3 cannot materialize the copy.
RUN-proven members (reviewer): plain alias `String x = s` lowers via **Branch C**
(`stmts/mod.rs:1203-1222`) — x becomes a Ptr alias capturing the address of the
PRE-materialize SSA slot version (the `cow_materialization_points` p1a failure);
move-steal variants via Branches F/G (`String x = s; s = "other"; v.set(0,…)`) —
both print garbage under lazy, both ASan-clean.

**W3a (the fix; defined by SOURCE, not by lowering branch — pass-2 PROTOTYPED and
RUN-PROVEN):** in `lower_var_decl`, when the bind's source operand resolves to a
projection-free local present in `cow_lazy_mat_flag`, emit the existing
flag-guarded `cow_materialize_view_lazy_in_place` on that source BEFORE
`lower_var_decl_assign_mode` runs (pass 2 placed it at the top of the
`if !lazy_handled` block — a single site upstream of Branches A-G and the trailing
assign path). After materialization the bind takes Branch A (live source,
shared-heap) or F (dead source, Move) — exactly the eager-world states; Branch C
is unreachable for owned+live same-type strings (`stmts/mod.rs:1166/:1203`).
**The `cow_lazy_mat_flag` entry MUST survive the materialize** — `restore_locals`
can resurrect the `Borrowed{CollectionElement}` tag after a branch/loop boundary,
and the persistent map entry + runtime flag keeps re-emitted guards correct
(pass-2's alias-bind-inside-loop probe passes BECAUSE of this); only W4's
write-clearing removes the entry. Clone counts: never worse than eager; the common
non-aliased case stays fully lazy. Proven results: d1_alias + d1_movesteal correct
at 1v1 eager clone-parity; witnesses 0/1/0; `cow_materialization_points`
byte-identical to eager; full gate-ON sweep 1193/9 (all 9 = D2).

**W3b (PROMOTED back in scope — the pass-2 demotion was RUN-REFUTED by pass 3's
probes; pass-4 PROTOTYPED + RUN-PROVED the fix at a located hook point):**
in `src/ir/lowering/exprs/methods.rs`, when a `returns_view` method's RECEIVER
resolves to a projection-free local present in `cow_lazy_mat_flag`, emit
`cow_materialize_view_lazy_in_place` on the receiver **BEFORE the call captures
the header**. The proven hook location (pass 4; executor follows it): immediately
after the third `needs_mut` CoW block (`cow_before_field_mutation`, unpatched
`methods.rs:1696`) and before the receiver-borrow construction (`:1698+`) —
upstream of the receiver `emit_borrow` and ALL call-emission arms. Condition:
`ctx.builtin_returns_view(&type_name, method_name)` && recv is a projection-free
`Operand::Copy/Move` local && present in `cow_lazy_mat_flag` → call the helper
with the local + its flag. (The POST-call View-tag site at `methods.rs:2645-2665`
is too late — the temp captures the raw buffer pointer at call time.) Proven:
probe 1 → `a = hello`, probe 2 → `t = hello!`, clone counts 1v1 eager; hook
reverted reproduces the garbage; in-loop `returns_view` call = 4v4 with the flag
guard firing once, not per iteration; Branch-E substring named bind 2v2
undisturbed.

**W3c (NEW — pass-4 BLOCKING finding, fix PROTOTYPED + RUN-PROVEN): the string
INDEX/SLICE syntax route.** `s[0..5]` / `s[0]` lower via `lower_index_access`
(NOT via `returns_view` dispatch) to `gorget_str_slice`/`gorget_str_index`, both
returning cap=0 views; index results carry NO View tag, so even named binds
break. Third call site for the SAME helper: top of the place-arm in
`lower_index_access` (unpatched `methods.rs:3229`), same condition
(projection-free base local in `cow_lazy_mat_flag` → materialize in place).
Proven: `show(s[0..5], &v)`, `show(s[0], &v)`, and named `String t = s[0..5]` +
mutate all flip to eager-identical output at exact eager clone parity (1v1,
`GG_CLONE_TRACE`); witnesses stay 0/1/0; default mode untouched. Also fix the
false comment at `methods.rs:3291` as part of this hook.

**Sibling-completeness rule (rewritten — the v4 instruction was itself the
trap):** do NOT verify by grepping `returns_view` CONSUMERS (that comes back
singular and misses W3c). Enumerate view-PRODUCING runtime emitters instead:
every caller of `gorget_str_view_region` in the runtime (`str_index`,
`str_slice`, `str_byte_slice`, `str_char_at`, + any others found) and every
compiler emit site that can produce one (the `returns_view` method dispatch, the
`lower_index_access` place-arm, the LIR rewrites at `lir/lower/insts.rs:895-955`)
— confirm each site is covered by one of the THREE hooks (W3a bind / W3b
receiver / W3c index base) or is provably reachable only with a non-lazy base.
Cite the full enumeration in the PR. ONE shared helper
(`materialize_lazy_source_if_needed`), three call sites.

Rejected alternative (record why in the commit/PR): propagating CollectionElement
provenance to the alias — preserves more laziness but multiplies loop-placement and
alias-chain cases; revisit as Phase 1b if profiles justify.

### W4 — Hygiene & sibling-path fixes
- `emit_lazy_loopcarried_borrow` double-registers `s` for drop (two
  `gorget_string_free` in the exit block — benign for String, still wrong): use the
  update-not-reregister API (`update_or_register_type` exists) so `s` is
  drop-registered exactly once.
- **Clear the tag AND the `cow_lazy_mat_flag` entry when the lazy local is
  written** (reviewer-proven: nothing clears it today — `lower_assign`'s
  clone-on-mutate block `assigns.rs:76-99` is Ptr-only and the lazy local is
  value-typed). The two REAL write sites (pass-2-corrected): `lower_assign`'s
  Identifier arm (`assigns.rs:28-394`) and `lower_compound_assign`
  (`assigns.rs:896+` — NOTE its string-concat fast path `:1021-1039` RETURNS
  EARLY, so clear at the top of the identifier branch, or in both that path and
  the generic tail `:1101-1120`). The `consume(!s)` move shape is UNREACHABLE for
  Phase-1 strings: `calls.rs:318-327` short-circuits named string locals with `!`
  to a const-Ptr borrow with no MoveZero, so `s` is unchanged and the tag stays
  accurate — document that in the code comment (the generic move path needs the
  clear only if that short-circuit is ever retired). A stale tag+flag otherwise
  emits a pointless guarded clone on a later `v` mutation (and can leak the old
  buffer via the Move-assign).
- **Audit `cow_sever_all_aliases_from`** (`context.rs:2822-2825`): it unsets
  CollectionElement refs WITHOUT materializing and runs BEFORE the `assigns.rs:70-72`
  dispatch when the reassigned collection also has Alias-aliases. The reviewer's
  probe came out correct, but the path has no lazy routing — add routing or prove
  unreachable for lazy-tagged refs, and keep the probe shape as a fixture
  (sibling-site rule).
- The lazy routing in Case 3 must coexist with the `is_ref_local` liveness check the
  legacy arm uses — decide order deliberately and comment it.

### W5 — Tests & fixtures
- Wire `witness_never`, `witness_taken`, `witness_cond_straightline` as integration
  tests via `run_gg(fixture, expected_stdout)` with EMBEDDED expected stdout in
  `tests/integration.rs` (the harness convention — NOT `.expected` files; the
  DONE/TODO instruction was off-convention). No name collisions exist.
- Add regression fixtures (same convention) for, at minimum:
  `d1_alias` (plain alias of lazy view + source mutation; mirrors
  `cow_materialization_points` p1a), `d1_movesteal` (alias + steal `s = "other"` +
  mutation; distinct lowering path F/G), `p_staletag` (reassign-the-lazy-local then
  mutate source; W4 clearing), `p_severorder` (alias + lazy ref + collection
  reassign; W4 sever-audit), `p_compound` (compound assign `s += "x"` then mutate
  source — the second W4 write site incl. its early-return fast path; also locks
  the leak-via-Move-assign claim), substring-of-lazy-view NAMED bind + source
  mutation (EXPECTED to pass even pre-W3 via Branch E — a regression net, not a
  repro; label it so), **W3b probe 1** (`show(s.substring(0,5), &v)`, callee
  `v.set(0,…)` then prints the param) and **W3b probe 2**
  (`String t = s.substring(0,5) + poke(&v)`, callee mutates v) — the PRIMARY
  validation for W3b, an ASan-blind class, **W3c index-temp-as-arg**
  (`show(s[0..5], &v)`) and **W3c named index bind** (`String t = s[0..5]` then
  mutate source) — the PRIMARY validation for W3c, same ASan-blind class,
  H2 shape (FieldPath source — asserts correct output under the EXCLUSION),
  multisite (two conditional mutation sites, first dynamically dead), escape
  (return of a still-view + caller destroys source), reassign-source (`v = w`),
  move (`consume(!v)` straight-line).
- **Clone-count lock-in** (the property stdout tests can't see): a test that builds
  `witness_never` and asserts on the EMITTED C (the build artifact `<out>.c`;
  `tests/integration.rs:250/:1716/:1775/:1881` already compute `c_path`):
  (a) `gorget_string_borrow_view(` present in `main`;
  (b) `gorget_string_clone_to_owned` callsite count in `main` == **1** (the
  flag-guarded materialize inside the loop — statically present, dynamically dead);
  (c) optionally that the borrow_view call textually precedes it. (The eager
  baseline had the clone in the bind block and no borrow_view.) Keep it narrow (one
  fixture) so it doesn't rot.
- The existing 21 a12333a0 lock-in fixtures + canaries must stay green
  (`cow_borrow_outlives_push`, `dict_get_unwrap_push_chain`, `cli_basic`,
  `index_ref_auto_clone`, ...).

### W6 — Docs & TODO
- Update `docs/devbook/11-copy-on-write.md`: the materialization-points section
  gains the lazy loop-carried mechanism (bind = cap0 view + pre-loop flag; mutation
  = flag-guarded in-place materialize; the THREE materialize-at-derived-view hooks
  W3a/W3b/W3c + the view-PRODUCER enumeration rule for future hook siblings; the
  multi-site dominance argument; the String-only `borrow_view_fn` registry axis;
  the ASan-blind-on-wrong-output-and-view-UAF caveat for future debuggers).
- TODO.md entries (executor adds, exact wording owner-reviewable): FieldPath lazy
  (1b) incl. `cow_before_field_mutation` lazy routing + `lower_field_assign`
  descendant-FieldPath severance walk (latent gap, file:line); EIndex inclusion +
  sidecar/typed-origin unification; Vector/Dict/Set generalization blocked on
  view-safe frees (`gorget_array_free` cap-blind `elem_drop`); self-host port
  (Phase 2, already in TODO #37 — update status); prescan typed-metadata cleanup —
  the `MUTATING_METHODS` name list AND the prescan blind-spot family
  (`cow_after_expr_moves` `functions.rs:446-505` does not walk `Expr::Spawn`/
  `SpawnBlocking`, closure bodies, f-string interpolation exprs — NOT a new lazy
  hole, prescan-miss → eager, but load-bearing under lazy and worth typed
  enumeration); pre-existing `copy_cow` leaks in
  `cow_borrow_outlives_push` + `dict_get_unwrap_push_chain` (byte-identical both
  modes, same family as TODO:726 — phrase the entry with the ASan COMMAND to
  regenerate the figures, not the dated byte counts, per the handover rule);
  DISCOVERED pre-existing gap (pass 2, orthogonal — both modes, no mutation
  needed): tuple-destructure of a collection-element borrow
  (`auto (a,b) = (s,"z")` after `String s = v.get(0).unwrap()`) fails GIR
  resource-move validation ("shallow copy of resource
  Tuple__GorgetString__GorgetString", `lowering/mod.rs:1636`).
- DONE.md entry on completion; remove the #37 Phase-1 portion from TODO.md.

## Gates (in order; executor runs 0-5, parent re-runs 5-8 on the integrated tree)

0. **Step 0 (BEFORE any code change, on the pristine worktree at tip):** build the
   eager-baseline `gg` and capture the ASan table over the full affected fixture
   set (the 15 affected + canaries + the to-be-added fixture SHAPES where they
   exist as scratch programs) to a saved log (`/tmp/asan-base-$RANDOM.log` AND a
   copy inside the worktree so it survives). This is the "no NEW findings"
   baseline gate 3 compares against — it cannot be produced after W1-W4 are
   applied.
1. `cargo build` + `cargo test --lib` (expect 1072+/0).
2. Targeted: the 15 affected fixtures + witnesses + new regression fixtures + the 21
   lock-in set, via single-test `cargo test --test integration <name>` runs.
   **The D1-class fixtures' stdout assertions are the PRIMARY correctness net for
   W3 (ASan is blind to that class).**
3. **ASan sweep** (`gg build --sanitize`, `ASAN_OPTIONS=detect_leaks=1`) over ALL 15
   affected fixtures + witnesses + new fixtures + canaries, lazy vs the Step-0
   eager baseline — assert **no NEW findings vs baseline** (the 2 pre-existing
   copy_cow leaks are known-exempt; produce the comparison table in the handoff).
   ⚠ ASan's role is supplementary defense-in-depth ONLY: it caught H2 but is
   proven blind to the D1 wrong-output class AND to the W3b view-UAF class —
   the stdout fixtures are the primary net for both.
4. `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`, ~8 min) —
   proves D2 closed.
5. LLVM spot-check: witnesses + D1/H2 fixtures under `GG_BACKEND=llvm` single-test
   runs.
6. (parent) FULL integration `--test-threads=4` → expect 1202+/0 (1192 + new tests).
7. (parent) `cargo test --test lints`.
8. (parent) Re-run fixed_point on the integrated tree.

## Constraints

- Executor runs in an isolated worktree; opens with `pwd` +
  `git rev-parse --show-toplevel` verification + `git merge --ff-only gorget-1`;
  never touches `/workspace/gorget-1` or `main`; stages by explicit file name only.
- No name-matching for semantic decisions (devbook/24). The
  `_inner == ctx.type_mapper.owned_string_type` typed comparison is acceptable but
  must be replaced by the `borrow_view_fn` accessor as the eligibility read.
- Commit style: small logical commits, ordered so EVERY intermediate commit is
  green: land the W2/W3/W4 mechanisms FIRST with the env-gate still in place
  (default unchanged → suite green at each step), then a FINAL commit that removes
  the gate + adds the fixtures/tests + W6 docs (the default flip and its proof
  land atomically). Messages cite this brief + the scout; end with the Claude
  Co-Authored-By trailer.
- The brief's line numbers are from `a044a10f` — re-grep before editing.
