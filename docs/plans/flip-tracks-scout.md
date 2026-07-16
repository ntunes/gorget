# FLIP SCOUT — self-host callable-init class (+4) and Copy-axis struct case (+1)

STATUS: COMPLETE — GO. All 5 fixtures flip; SELFHOST lane 214/214 MATCH (was 209); bootstrap fixed-point PASSED; no regressions.

## Mission recap
RV-F left FIVE by-design self-host conformance misses (SELFHOST floor held at 209,
five below the corpus 214). Verify premises, design self-host fixes, prototype
end-to-end, MEASURE the flips (expect SELFHOST 209 → 214).

## The five fixtures (from spec_conformance.rs floor comments + fixture docs)
1. `reject_callable_bind_bare.gg`     — bind `Callable g = f` bare → want E_MoveWithoutOperator. Self-host ACCEPTS (MISMATCH).
2. `reject_callable_ctor_bare.gg`     — struct ctor `Holder(f, 7)` bare Callable → want E_MoveWithoutOperator. Self-host ACCEPTS (MISMATCH).
3. `reject_callable_enum_variant_bare.gg` — `Some(f)` bare Callable → want E_MoveWithoutOperator. Self-host ACCEPTS (MISMATCH).
4. `reject_callable_for_var_bind.gg`  — `for f in fns: Callable g = f` bare (f is for-var, NOT param) → want E_MoveWithoutOperator. Self-host ACCEPTS (MISMATCH).
5. `copy_struct_field_borrow_ok.gg`   — all-int STRUCT field bare read under `&h` → want ACCEPT exit0 "4". Self-host REJECTS/BUILD-FAILs (Copy axis scalar-only).

Floors today: C=214, LLVM=214, SELFHOST=209, MIN_FIXTURES=214.
Target after flip: SELFHOST 209 → 214, comments' by-design list → zero.

## KEY INFRA FACT (verified)
The selfhost conformance lane runs the LOWERER driver (`tests/fixtures/self_host_lowerer/driver.gg`),
but `typecheck.gg`/`infer.gg`/`types.gg`/`scope.gg`/`resolve.gg` in the lowerer dir are
SYMLINKS to `self_host_typechecker/`. So editing `self_host_typechecker/**` (my zone)
DOES drive the conformance lane. The other agent's zone (lower_loops.gg/lower_expr.gg,
lower_types.gg) are lowerer-only real files — NO overlap.

## STEP LOG
- [x] Confirmed worktree, read floor comments, read 5 fixtures.
- [x] BASELINE VERIFIED (release gg + self-host driver built). Ran all 5 through the driver:
      - 4 callable fixtures: driver exit=0 ACCEPTED (should reject) → MISMATCH by design. CONFIRMED.
      - copy_struct_field_borrow_ok: driver exit=1 `error[E_BorrowConflict]` (should accept) → BUILD-FAIL. CONFIRMED.
      Premises 100% hold.

## PRODUCTION GROUNDING (cited)
- `is_copy_type` (src/semantic/safety/type_utils.rs:17-88): scalar prims / tuple-of-Copy / Ref /
  Channel/Shared/Weak/Mutex generic / NON-drop-tainted struct all-fields-Copy / enum all-payloads-Copy.
- `needs_explicit_move` (type_utils.rs:102-125): drop-tainted OR {Function, CallableTrait,
  MutCallableTrait, ConsumeCallableTrait, BoxedCallable, Owned} OR Generic{Box,Task,TaskGroup,Guard}.
- Bind/reassign single-owner reject: `check_value_needs_move` (check_stmt.rs:1434), PARAM-EXEMPT
  (`def.kind==Variable && !def.is_param`, :1464). Called from VarDecl (:31) + Assign (:425).
- Ctor/enum/struct-literal reject: `require_explicit_move_for_single_owner_init` (check_expr.rs:24),
  NO param exemption (`def.kind==Variable`, :45). Called at ctor call-arg (:274, `is_constructor` gate),
  enum-ctor call-arg (:693), struct-literal field (:1183). NOT at plain-call args / return / put.
- SingleOwner message (errors.rs:1045): "a single-owner type (no implicit copy)".
- ggdef `ty_is_copy` (spec/ggdef/src/elaborate/mod.rs:546): Prim / tuple / non-tainted struct
  all-fields-copy / enum all-payloads-copy.

## SELF-HOST GROUNDING (cited)
- Conformance lane runs LOWERER driver; typecheck.gg etc. symlinked from self_host_typechecker/ (my zone).
- Copy axis: `resolved_type_is_copy` (typecheck.gg:837) — scalar/tuple/RTRef/handle-generics; RTDefined
  falls to `else`→false (the scalar-only gap). Consumed by `arg_place_is_copy` (:860) →
  `check_call_aliasing` (:865) D10(b) place-overlap. TODO.md:888 filed this exact fix
  (DefInfo.is_copy flag + compute_is_copy pass mirroring compute_drop_taint).
- Single-owner axis: ABSENT. Drop-taint axis exists (`reject_tainted_place` :673, structural) but
  callables aren't drop-tainted. TODO.md:257 filed the fix (mirror require_explicit_move_for_single_owner_init).
- DefInfo (scope.gg:31): has is_drop_tainted, NO field-type storage → Copy needs the compute-pass.
- `compute_drop_taint` (typecheck.gg:550) called at driver entry :3290 before the item walk — the model.
- Init sites in the safety walk: SVarDecl :1479, SAssign :1493, ECall ctor arm :1713-1720
  (`call_is_ctor` gate), EStructLiteral :1843, EDotShorthand :1849.

## BLAST RADIUS (pre-implementation grep)
- Self-host source: NO bare single-owner identifier binds/assigns (only `Option[Box[..]]` + ctor-calls
  `Some(Box(..))` — non-identifier RHS, unaffected). Bootstrap-safe by construction (self-host already
  compiles under Rust gg's stricter check).
- ACCEPT fixtures all safe: closures/`!a`/`make()`/`.clone()`/`.unwrap()`/param-source RHS — none bare.
  `callable_param_rebind_ok.gg` (`Callable g = f`, f a PARAM) confirms the bind param-exemption is
  LOAD-BEARING (without it this green fixture regresses).

## DESIGN + IMPLEMENTATION (prototyped; patch at /tmp/flip_proto.patch)

### Fix A — Copy-axis struct extension (flips copy_struct_field_borrow_ok.gg, +1)
Mirrors TODO.md:888 (DefInfo.is_copy flag + compute_is_copy pass, dual of compute_drop_taint).
- scope.gg: DefInfo gains `bool is_copy` (trailing field) + `set_def_is_copy` accessor; 3 ctor calls
  get trailing `false`.
- typecheck.gg: `resolved_type_is_copy` RTDefined arm → `not dd.is_drop_tainted and dd.is_copy`
  (was `else→false`). New `compute_is_copy(module, &scopes, &types)` — seeds every non-tainted
  struct/enum optimistically Copy, then fixpoint-flips to non-Copy on any non-Copy field/payload
  (non-Copy is the absorbing direction). Called at driver entry right AFTER compute_drop_taint
  (reads is_drop_tainted). Fully typed-metadata (rule 2), no name-matching.

### Fix B — single-owner Callable INIT class (flips the 4 reject fixtures, +4)
Mirrors Rust needs_explicit_move (type_utils.rs:102) + require_explicit_move_for_single_owner_init
(check_expr.rs:24) + check_value_needs_move single-owner arm (check_stmt.rs:1485).
- New `is_single_owner_type` (callables RTFunction/RTCallableTrait/RTMutCallableTrait/
  RTConsumeCallableTrait/RTBoxedCallable + RTGeneric Box/Task/TaskGroup/Guard), EXCLUDING
  drop-tainted (that axis is reject_tainted_place's — disjoint, no double-diagnosis).
- New `single_owner_message` (mirrors errors.rs:1045 SingleOwner + Whole shape).
- New `reject_single_owner_init(e, param_exempt, ...)` — fires on a bare EIdentifier resolving to
  DkVariable of a single-owner type; `param_exempt` skips params (bind/reassign only).
- Wired at: SVarDecl bind (param_exempt=true), SAssign reassign (param_exempt=true), ECall ctor arm
  (param_exempt=false), EStructLiteral (false), EDotShorthand (false). Matches production's exact
  call-site set (NOT plain-call args / return / put — those borrow / CoW-clone).

### Floor edits the executor makes (spec_conformance.rs)
- `SELFHOST_MATCH_FLOOR: 209 → 214` (now == C/LLVM/MIN_FIXTURES).
- Rewrite the by-design comments (module doc lines ~49-67 + inline ~117-163): the FIVE-below-corpus
  staging is CLOSED; all three production lanes now reach the whole corpus. C/LLVM/MIN unchanged (214).

## MEASURED RESULTS (regenerated this session, release gg)
- Baseline driver: 4 callables ACCEPT (exit 0), copy_struct REJECTS E_BorrowConflict. (premise confirmed)
- Prototype driver, per-fixture:
  - reject_callable_bind_bare      → REJECT error[E_MoveWithoutOperator]  ✓ MATCH
  - reject_callable_ctor_bare      → REJECT error[E_MoveWithoutOperator]  ✓ MATCH
  - reject_callable_enum_variant_bare → REJECT error[E_MoveWithoutOperator]  ✓ MATCH
  - reject_callable_for_var_bind   → REJECT error[E_MoveWithoutOperator]  ✓ MATCH  (for-var typing works)
  - copy_struct_field_borrow_ok    → ACCEPT → cc → run → "4" exit 0        ✓ MATCH
- Blast-radius ACCEPT fixtures (all still accept + correct output): callable_move_bind_return_ok [6 6],
  callable_param_rebind_ok [20] (PARAM EXEMPTION load-bearing — confirmed), consume_callable_once,
  vector_callable_two_locals, dict_callable_get_no_clone/_clone, copy_field_borrow_ok [7],
  copy_struct_closure_capture/_in_loop/_match/_return. reject_borrow_conflict_noncopy_field STILL
  REJECTs E_BorrowConflict (Copy-axis fix did not over-broaden); reject_consume_callable_double
  STILL REJECTs E_DoubleMove.
- FULL self-host conformance lane: `cargo test --release --test spec_conformance spec_conformance_selfhost`
  → **total=214 · MATCH=214 · MISMATCH=0 · BUILD-FAIL=0** (was 209). Regen cmd in the report.
- C / LLVM / GGDEF lanes: UNCHANGED by construction — the patch touches ONLY self-host .gg fixtures
  (scope.gg + typecheck.gg); no src/ nor spec/ggdef/ edits. The Rust compiler + ggdef oracle are untouched.

## FLOOR EDITS (exact, for the executor — tests/spec_conformance.rs)
- `const SELFHOST_MATCH_FLOOR: usize = 209;` → `214`.
- Module-doc comment (~lines 49-67): delete the "self-host floor is FIVE below" staging paragraph;
  state all three production lanes reach the whole corpus (214).
- Inline floor comments (~lines 117-163): the RV-F "SEVEN MATCH / FIVE below" narrative → all TWELVE
  RV-F fixtures now MATCH all three lanes; drop the "FOUR callable + FIFTH copy_struct by-design"
  paragraphs (the gaps are CLOSED). Keep C/LLVM/MIN at 214.
- TODO.md: move the two filed obligations (line ~257 callable-init class, line ~888 Copy-axis twin)
  from TODO.md to DONE.md (they are now landed).

## KNOWN MINOR ASYMMETRY (non-blocking; document, do not silently ignore)
Production's struct-BRACE-literal single-owner check (check_expr.rs:1183) sits in the `!target_is_ref`
branch — it SKIPS ref-typed fields (a `Ref[Callable]`/`MutRef[Callable]` field init is a borrow, not a
copy). My EStructLiteral/EDotShorthand arms apply the reject to ALL args (no ref-field gate). So the
self-host would OVER-REJECT the exotic shape `S{cbref: f}` where `cbref` is a borrow-typed field and
`f` a bare single-owner local. UNREACHABLE today: (a) the self-host source has NO struct brace-literals
(grepped); (b) no spectest exercises it; (c) enum-variant payloads are never ref-typed; (d) the ECall-ctor
path is UNGATED in production too (only the brace-literal path differs). It is over-reject-only (never
UAF). Executor options: accept as latent + file a one-line note, OR gate the EStructLiteral arm on
per-field ref-flags (mirrors compute_struct_field_ref_flags — heavier). Recommend the note; the class is
otherwise faithfully mirrored.

## GATES (all regenerated this session, in-worktree)
- `spec_conformance_selfhost` (release): total=214 · MATCH=214 · MISMATCH=0 · BUILD-FAIL=0. PASS.
- `self_host_bootstrap_fixed_point` (release, 900s budget, foreground-waited): ok in 504.6s. PASS —
  the new rejection does not fire on any self-host source (bootstrap-safe, as predicted: the self-host
  already compiles under Rust gg's identical check).
- `cargo test --lib` (debug — the CLAUDE.md-documented profile): the 2 `lir::validate` should-panic
  tests PASS. (In `--release` those 2 fail — a PRE-EXISTING profile artifact: they rely on
  debug_assert panics compiled out in release. Zero relation to this diff, which touches no Rust
  files. Worth a LOW note: release-profile --lib is not clean on those 2.)
- Extra corpus sweep: grep of tests/fixtures + spectests for bare single-owner binds found ONLY
  (a) `dict_box_callable.gg` — `Callable dfn = double_it` where RHS is a FREE FUNCTION name →
  DkFunction, exempt via the DkVariable gate; verified: driver still ACCEPTS (exit 0);
  (b) `move_without_operator_error.gg` — `Box[int] b = a`, an expected-REJECT fixture; the self-host
  NOW rejects it error[E_MoveWithoutOperator] exactly like production (verified side-by-side) — a
  BONUS parity improvement, not a regression.

## GO / NO-GO: **GO**
The prototype is production-mirroring, typed-metadata-only, measured end-to-end, and every gate is
green. Executor brief recommendations:
1. Land the patch as-is (/tmp/flip_proto.patch, 293 lines: scope.gg +19, typecheck.gg +163).
2. Same commit: SELFHOST_MATCH_FLOOR 209→214 + the spec_conformance.rs comment rewrite +
   TODO.md:257 and TODO.md:888 obligations → DONE.md.
3. Include the brace-literal ref-field asymmetry note (above) in the commit message or a LOW TODO line.
4. Round-close obligations per MEMORY: parity regen is OWED after fixture-affecting rounds (the harness
   currently OOMs solo — coordinate with the parent).
5. Cross-lane check per Core invariant #9: this is a LANE-CATCHUP (self-host reaching already-ratified
   semantics pinned by existing four-lane fixtures) — no new fixture needed; the five existing spectests
   ARE the pins.
