# BRIEF — Chain C: Rust-side miscompile burn-down (6 fixes + 1 retirement)

Status: v3 (pass-2 review folded 2026-06-10: cast-name audit enumeration
corrected — `float32`/`float64` IN, `int`/`float`/`bool` OUT (have real
lowerings, run-verified) [p2-R1]; TODO hygiene completed — :728 moves to
DONE in commit 1, :732/:730 move in commits 5/6 [p2-R2]; the §7.9 amendment
is ADDITIVE (String-exception sentence), the lead sentence + write doctrine
stay generic [p2-R3]. Pass 2 RUN-PROVED the item-3 PRODUCER half (Branch-C
flag; emitted-C coherent — `emit_borrow` initializes the Ptr slot, the
garbage store gone; 365 targeted tests green incl. string/Branch-A 90;
self-host driver smoke-runs) — producer (a) ALONE fixes the class. v2 was
pass-1 folded 2026-06-10: item-1 fixtures MUST use the
`v.get(i).unwrap()` bind shape [R1]; item-6 ICE hardening cites BOTH sibling
fall-through loops [R2]; item-6 scope includes the §7.9 language-reference
amendment — the spec currently lists String among mutable-borrow subscripts,
contradicting the new error [R3]; per-commit `GG_BACKEND=llvm` fixture runs
added to gates [R4]; item-2 widening is CALLER-side in `lower_static_decl`
(the predicate is type-blind) with `synthesize_static_init_fn` reuse making
the DCE warning auto-satisfied [R5]; item-3 producer suppress must be
BRANCH-C-SPECIFIC, never keyed on AssignMode::Borrow generally — Branch A
also returns Borrow with a load-bearing trailing assign [R6]. Pass 1
RUN-PROVED items 1, 2, 3(b), 5 end-to-end, EMPIRICALLY confirmed the item-1
landing-order warning (prescan-arm-alone = UAF garbage), and confirmed the
cast-name class is real (`int8(3)`/`byte(65)`/`float32(2)` all
check-pass+link-fail). v1.1 locked the OWNER decision: str(x) = rejection
(b). v1 was the orchestrator draft from scout `agent-a0663aa9d5dc19f5e` at
tip `6894cb6a`.)

## Mission

One executor, six sequenced commits, all Rust `src/` + plain fixtures —
file-disjoint from Chain B (self-host lowerer) and Chain D (backend leak
emit paths; coordinate only if D's fix lands in `src/ir` drop-registration).
Fixes two memory-safety bugs (items 1, 3), one whole-class miscompile (2),
one validator-panic class (5), two accepted-but-meaningless-surface holes
(6, the `str()` gap), and retires one refuted TODO entry (4). Commit order:
**5 → 1 → 3 → 2 → 6+str()+TODO-hygiene → 7**.

## Item 5 (commit 1) — tuple-literal resource destructure panic
- VERIFIED class (broader than the TODO): ANY tuple-destructure of an INLINE
  tuple literal with a resource element panics GIR validation ("shallow copy
  of resource Tuple__…", panic now at `src/ir/lowering/mod.rs:1647`) — even
  pure literals `auto (a,b) = ("x","z")`. Function-return and named-tuple
  destructures are fine.
- ROOT CAUSE (missing ownership tag at the value's birth — CLAUDE.md rule 3;
  sibling evidence: `lower_struct_literal` tags `set_owned` at
  `exprs/mod.rs:2000`): the `Expr::TupleLiteral` arm (`exprs/mod.rs:469-557`)
  never tags its `tuple_init` dst → Untracked → destructure picks Copy →
  validator correctly flags the whole-tuple resource copy. The TODO:728
  "clone at the tuple ctor" proposal is WRONG-SITE (element clones already
  happen via `ensure_owned_at_boundary`); since commit 1 FIXES the bug, MOVE
  TODO:728 to DONE.md IN COMMIT 1 with the corrected root-cause note
  [p2-R2] — never leave a fixed bug "corrected in place" in TODO.
- FIX: one line — `ctx.set_owned(builder, dst);` after `builder.tuple_init`
  (`exprs/mod.rs:~556`). Destructure then takes Move + existing MoveZero.
- FIXTURE: `tuple_destructure_literal_strings.gg` (literal, named-local, and
  collection-element-borrow element variants).
- RISK: Owned-tagging changes mode selection for other tuple-literal
  consumers (call args, returns) — full suite mandatory; struct-literal
  precedent says intended.

## Item 1 (commit 2) — `!`-move of a collection with live element borrows:
## MEMORY-UNSAFE (upgraded from value-bug)
- VERIFIED, fresh: move-BIND `Vector[String] w = !v` AND move-REASSIGN
  `w = !v` then `w.set(0,…)` → read-through (`gamma` where eager semantics
  say `alpha`); `w.clear()` → empty-string read; `w.push(...)` ×64 (realloc)
  → **SIGSEGV exit 139**. Controls correct: `consume(!v)`, plain reassign,
  the Phase-1 loop shapes.
- ROOT CAUSE (two cooperating holes; fix the SECOND — the write site):
  (i) prescan `cow_after_expr_moves` (`functions.rs:446-505`) matches only
  Call/MethodCall/BinaryOp — a bare `Expr::Move` RHS records nothing, so the
  element bind stays a deferred CollectionRef; (ii) **the `Expr::Move`
  lowering (`exprs/mod.rs:231`) does NOT call `cow_before_mutation` for
  local sources** — only for bare params (`:244-245`) — and then
  `unset_ownership`s refs WITHOUT materializing (`:292-295`). The call-arg
  move sibling DOES dispatch (`calls.rs:336` "sever aliases first") —
  textbook sibling-site drift. devbook/11's claim that moves route through
  the cow_before_mutation family is currently FALSE for `Expr::Move`.
- FIX: in the `Expr::Move` identifier path, call
  `ctx.cow_before_mutation(builder, source_local, inner.span)` for ANY local
  source (subsumes the bare-param-only call), mirroring `calls.rs:336`.
  Case 3 + Cases 1/2/4/6 of `cow_before_mutation` already dispatch the lazy
  in-place materialize AND legacy ref/alias/view severs. The stale-refs
  `unset_ownership` loop stays (harmless post-materialize). The prescan
  `Expr::Move` arm is OPTIONAL hardening and MUST NOT land alone (a lazy
  view with no move-site dispatch dangles). Accepted over-clone:
  move-then-never-mutate (documented refinement: tag-transfer to the
  destination needs info the Expr::Move layer lacks → TODO).
- FIXTURES (each expecting `alpha`): `cow_move_bind_element_borrow.gg`,
  `cow_move_reassign_element_borrow.gg`, `cow_move_clear_element_borrow.gg`,
  `cow_move_realloc_element_borrow.gg` (the SIGSEGV witness).
  ⚠ [p1-R1] the element bind MUST be the `v.get(i).unwrap()` shape — `v[i]`
  binds take a different, ALREADY-SAFE path at tip (run-verified: all four
  shapes print `alpha` pre-fix with `v[i]`), so `v[i]` fixtures would be
  green-before-fix nets guarding nothing.
- This fix REMOVES the basis for Chain B's move-shape oracle exception —
  note in the handoff (Chain B's EMove fixtures can be snapshotted in a
  follow-up once both land; do NOT touch Chain B's zone).
- GATES: the full `cow_lazy_*`/`witness_*` stdout battery (PRIMARY net —
  ASan blind to these classes) + ASan sweep vs a pre-change baseline +
  clone-count watch (`--clones=stats` directionally + the emitted-C callsite
  check on one witness; never trust stats alone for a yield CLAIM).

## Item 3 (commit 3) — `Result/Option x = src` identifier bind: SIGSEGV
- VERIFIED: `Result[int,String] x = src` → gcc `assignment to 'void *' from
  'int64_t'`, runtime SIGSEGV; also `Option[String]`. Unaffected:
  `Option[int]` (non-resource), reassign, direct match.
- ROOT CAUSE: var-decl Branch C retypes `x` to `Ptr(enum)` + `emit_borrow`;
  the unconditional trailing `assign_mode` (`stmts/mod.rs:986`) adds a
  Bw-mode value-into-ptr-slot assign (legal GIR idiom; Vector path coerces
  it to a benign slot_addr re-store). `try_enum_payload_extract`
  (`src/lir/lower/operands.rs:652`, from `insts.rs:25`) intercepts FIRST and
  mis-classifies: its same-enum dst check matches only `GirType::Named`, a
  `Ptr(enum)` dst falls through → it emits the auto-unwrap payload
  extraction (FieldPtr(1)+load) into the pointer slot → deref of payload as
  pointer → SIGSEGV.
- FIX (both layers, producer primary per devbook/24): (a) suppress the
  redundant trailing assign when Branch C fired — ⚠ [p1-R6] the suppress
  must be BRANCH-C-SPECIFIC (a flag set in that arm, same mechanism as the
  `lazy_handled` skip at `stmts/mod.rs:944`), NEVER keyed on
  `AssignMode::Borrow` generally: Branch A (`stmts/mod.rs:1240-1247`,
  SharedHeap string aliasing) also returns Borrow mode and its trailing
  assign IS load-bearing; (b) consumer hardening: in
  `try_enum_payload_extract`, bail when `mode == AssignMode::Borrow` (the
  caller has `mode` in scope — typed metadata currently ignored) AND unwrap
  `Ptr(inner)` in the dst comparison. If nervous, (b) lands first; both
  together is the end-state.
- FIXTURE: `result_whole_bind_identifier.gg` (Result[int,String] +
  Option[String], expected `5` / `hello`). Unblocks the parked ③(b)
  regression fixture noted in TODO (cite it).

## Item 2 (commit 4) — enum-typed STATIC initializers silently zeroed
- VERIFIED class = ALL enum-typed statics: `Option[String] G = None` →
  `some:`; `Option[int] H = Some(5)` → `some:0`; `Color C = Color.Blue()` →
  `red`. Emitted C: `= {0}` with no init; Option's Some = tag 0.
- ROOT CAUSE: `eval_static_init` (`src/ir/lowering/mod.rs:2459`) has no arm
  for NoneLiteral/enum-variant ctors → `GlobalInit::Zeroed` silently.
- FIX [p1-R5 corrected]: the widening is CALLER-SIDE — the predicate
  `initializer_needs_synthetic_fn(expr)` (`mod.rs:2392`) is type-blind;
  enum-ness comes from `type_id` + registry in `lower_static_decl`
  (`mod.rs:2340`, both in scope; pass-1 prototyped this shape, RUN-proven
  `none`/`some:5`/`blue`). Reuse `synthesize_static_init_fn` — the
  `__gg_static_init_` prefix is ALREADY DCE-seeded (`optimize.rs:217`), so
  the Bug-B pruning warning is auto-satisfied on that path. Commit message
  notes the bugB §3 init-ordering caveat now also covers enum statics whose
  variant args read other statics. Optional later: compile-time tag-only
  GlobalInit for payload-less variants → TODO.
- FIXTURE: `static_enum_init.gg` (None / Some(5) / user-enum non-first
  variant / `public static`; expected `none`, `some:5`, `blue`).

## Item 6 + the `str()` gap + TODO hygiene (commit 5)
- VERIFIED: `s[0] += "x"` AND the sibling `s[0] = "x"` are silent no-ops
  (write-back set-candidate loop `assigns.rs:1443-1461` falls through with
  no else). Docs confirm rejection is right: `s[i]` is documented as a
  read-only codepoint view (`language-reference.md:3140-3143`); string
  mutation is rebuild-based; variable-width UTF-8 makes index-assign
  incoherent.
- FIX: check-time gate in `Stmt::Assign` (`src/semantic/typecheck.rs:3008`)
  + `Stmt::CompoundAssign` (`:3026`): target `Expr::Index` with
  String-typed object (incl. range index) → new SemanticErrorKind:
  "strings are not index-assignable: `s[i]` is a read-only codepoint view —
  build a new string instead (e.g. `s.replace(...)`, slicing +
  concatenation)". Must NOT fire for Vector/Dict/user types with setters.
  Defense-in-depth: make BOTH sibling set-candidate fall-throughs hard ICEs
  [p1-R2] — `lower_index_assign`'s loop (`assigns.rs:~892-897`, fn `:755`,
  the PLAIN-assign no-op site) AND `lower_compound_assign`'s
  (`:1443-1461`). ALSO in this commit [p1-R3]: amend
  `docs/language-reference.md` §7.9 (`:1631-1633`) — it currently lists
  String among resource types whose subscript "returns a mutable borrow",
  contradicting the new error. [p2-R3] The amendment is ADDITIVE, not just a
  deletion: §7.9's lead sentence (`:1626`) and the subscript-write doctrine
  (`:1649-1653`) stay generic, so add ONE String-exception sentence to §7.9
  pointing at the §Strings view table (`:3141-3144`, the correct story).
  Pass 2 grepped reference+book+design: no OTHER doc site teaches string
  index-assign.
- THE `str()` GAP (found while probing the REFUTED item 4): `String s =
  str(3)` passes `gg check` (`str` listed in `is_builtin`,
  `resolve.rs:1966`) but has NO lowering → I64-typed result → the
  `emit_types.rs:753` ICE or CC error. The language documents no free
  `str()` (conversions = `as` casts + std.conv). FIX — **OWNER-DECIDED
  2026-06-10, not up for re-litigation in reviews**: check-time REJECTION
  with a teaching error (option (b); implementing `str(x)` was considered
  and declined — it would be a third way beside f-strings/`.to_string()`,
  against the one-obvious-way design target). Suggested message shape:
  "no builtin `str(...)` call: use an f-string `f\"{x}\"`, `.to_string()`,
  or `std.conv`"; AUDIT the other
  `is_builtin` cast-names — `int8`…`uint64`, `float32`/`float64`, `byte`
  [p2-R1: `float32(2)` run-verified link-fails] — for the same
  accepted-but-unlowered hole; NOT `int`/`float`/`bool`, which HAVE real
  lowerings (run-verified working). Fix the CLASS (one gate, all unlowered
  names).
- ITEM 4 RETIREMENT: the TODO:232 String-ABI-ICE entry is REFUTED at tip
  (all 6 probed sibling shapes work; intervening auto-prop/aggregate work
  fixed it) — DELETE it citing this scout; the `str()` gap entry replaces
  it. TODO:728 is handled in commit 1 (moved to DONE — see item 5). ALSO
  [p2-R2]: MOVE TODO:732 (the `s[i] +=` entry, fixed by this commit) and
  TODO:730 (the comprehension entry, fixed by commit 6) to DONE.md in their
  respective commits — fixed bugs never stay in TODO.
- FIXTURES: `check_gg_fails` negatives for `s[i] =`, `s[i] +=`, `str(3)`
  (+ any cast-name siblings found unlowered).

## Item 7 (commit 6, LAST — interacts with the lazy enumeration)
- VERIFIED: `[c for c in s]` → CC error (`int64_t` from `Str` at
  `gorget_str_index`). Root cause: `infer_collection_element_type`
  (`methods.rs:3361`) knows only `Vector__`/`Dict__`/`Map__` name prefixes
  (rule-2 debt — note in TODO) → String base falls to I64. TWO more latent
  wrongs behind the type fix: byte-length loop bound vs codepoint-indexed
  `gorget_str_index` (OOB on multi-byte), and cap=0 view elements pushed at
  an ownership boundary.
- FIX (docs-grounded, reference-grade): route String-based comprehensions
  through the `lower_for_string` loop shape (single UTF-8 pass, matches
  `for ch in s:` semantics, language-reference `:3144`) with a
  clone-at-boundary push body. Reuses the synthetic-codepoint emission that
  hook W3d already dominates → lazy-source materialize comes free and NO new
  view-producer emit sites → **all three lints stay green at current
  budgets**. The index-walk fallback design is O(n²) + new hook plumbing —
  rejected.
- LINT-FAILURE PROTOCOL (the guard's first real exercise — follow it, don't
  fight it): if the implementation DOES add a view-callee mention in
  `src/lir` or a new producer, do exactly what the lint message says —
  verify/add the dominating GIR hook, add the `STR_VIEW_PRODUCERS` row
  (producer `sig(`, never `sig_fresh(`), add the devbook/11 enumeration row,
  THEN bump `BUDGET: usize = 41` (`tests/lints.rs:1078`) with a
  justification comment naming the covering hook. Never a blind bump.
- FIXTURES: `string_comprehension.gg` — ASCII + MULTI-BYTE (`"héllo"`) +
  filtered variant + a lazy-eligible-base variant (source mutated after).

## Gates
- Per-commit: `cargo build` + `cargo test --lib` + the commit's fixtures +
  `cargo test --test lints` (10 expected; item 7 especially) +
  **`GG_BACKEND=llvm` single-test runs of the commit's new fixtures**
  [p1-R4] — every fix lands in shared GIR/LIR consumed by both backends,
  and item 1 is memory-unsafety; single-test LLVM runs are cheap at default
  threads.
- Items 1/5/7 touch CoW/lazy machinery: the full `cow_lazy_*` + `witness_*`
  battery per commit touching them (stdout = primary net), ASan sweep with a
  pre-change baseline for item 1.
- Final on the executor tree: full integration suite (partitioned per the
  TODO note if CPU-thrash appears), `self_host_bootstrap_fixed_point` once
  (GG_BUILD_TIMEOUT_SECS=600), `self_host_runtime` snapshot net,
  `self_host_runtime_diff` parity re-measure (item 1 changes Rust-side clone
  behavior at moves; expect neutral-or-positive vs a Step-0 baseline you
  record BEFORE any edit).
- Parent re-runs the full battery on the integrated tree.

## Constraints
- Worktree preamble as standard (pwd check, ff-only to gorget-1, never touch
  main/`/workspace/gorget-1`); explicit-file `git add`; no pushes; STOP on
  contradicted premises with freshly-printed evidence.
- File zone: `src/ir/lowering/**`, `src/lir/lower/**`, `src/semantic/**`,
  `src/lir/optimize.rs` (DCE seed), new `tests/fixtures/*.gg`,
  `tests/integration.rs` (append), `tests/lints.rs` ONLY under the
  lint-failure protocol, TODO.md, DONE.md. Do NOT touch
  `tests/fixtures/self_host_lowerer/**` (Chain B) or the backend print/leak
  emit paths (Chain D); `docs/plans/brief_rust_oracle_static_index_read.md`
  is a ready brief touching `exprs/methods.rs` `lower_index_access` — no
  overlap, but note it.
- Commit messages cite this brief + the scout; Co-Authored-By trailer.
- Line numbers are scout-fresh at `6894cb6a` — re-grep before editing.
