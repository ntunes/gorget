# Wave A2-S brief — port D12 drop-purity enforcement to the SELF-HOST compiler

> **Track A2-S** (Batch A follow-on). Mirrors the LANDED Rust half **A2-R1**
> (`b72ef446`) + its message rider **A2-R2** (`b4b6124a`) into the self-host
> `.gg` compiler so the self-host lane REJECTS drop-tainted implicit copies at
> all six positions, exactly as Rust gg and ggdef do.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-a2-s.md` (read-only, prototype
> applied → measured → reverted; compiler + 9 MB self-host driver built this
> session). **Every load-bearing anchor below is scout-verified `file:line`.**
>
> **Status:** v1 — pass-1 (Opus, fresh) folded. Pass-1 CONFIRMED the plumbing
> premise, taint inputs, Rust-model anchors, verbatim message text, the ggdef
> 15-test enumeration, the DefInfo 3-ctor blast radius, and the "no
> `lvalue_value_type`" simplification — and raised **4 reservations, ALL FOLDED:**
> **(1) BLOCKING** — closure-tails normalize to `SExpr`, NOT `SReturn`
> (`parser.gg:2323/2585`), so the "one `SReturn` hook" claim was false and a
> pos-4 SReturn-only hook would ACCEPT `(R x): x` (a Core-#8 defect in M1); pos-4
> now has TWO hooks (§4.2, §5, §6.6c). **(2)** pos-2 under-specified/mis-rated —
> `W(a)` and `use(a)` both parse `ECall`; must gate on ctor-ness (`infer.gg:149-226`)
> and leave plain-call/method args as legal borrows (§6.6b, re-rated MEDIUM).
> **(3)** the gates were blind to an OVER-rejection (self-host has zero tainted
> types) — added a legal guard fixture (§7). **(4)** [SUPERSEDED by v2(1) —
> pass-1 wrongly called `spec_conformance_selfhost` a phantom; it is real, in the
> `spec_conformance` binary. See the v2 block + §8.6.]
> Minors folded: §3 stage-path caveat (driver.gg/lower_cow.gg are lowerer-stage,
> NOT symlink-shared), ±1-2 line drift note, `RTOwned`-absent accessor
> completeness.
>
> **v2 — pass-2 (Opus, fresh) folded.** Pass-2 independently RE-DERIVED and CONFIRMED
> pass-1's blocking closure-tail correction (both hooks correct + sufficient for
> inline AND block closure bodies) and the pos-2 ctor-gating, and raised **3
> reservations, ALL FOLDED — two of them corrections to pass-1's own folds:**
> **(1) MATERIAL** — `spec_conformance_selfhost` is NOT a phantom; it is a real
> `#[test]` in the **`spec_conformance` binary** (`tests/spec_conformance.rs:562`),
> the self-host ACCEPTANCE floor pass-1 wrongly told the executor to skip — §8.6
> now runs it via the correct binary + keeps it as the over-rejection gate.
> **(2) MATERIAL** — the self-host emits NO `error[E_…]` codes (headline is
> `error: <msg>`, `diagnostic.gg:281`); pass-1's E-code render mandate was wrong —
> §6.5 now asserts on `error` + message TEXT + box rule (the working
> `self_host_driver_rejects_*` pattern). **(3)** the capture-tail skip was
> mis-cited (`check_expr.rs:150-155` → the correct `:997-1006` / pos-5 `:964-996`).
> Minors: dead `build` fn dropped from the §7 guard fixture.
>
> **v3 — pass-3 (Opus, fresh, confirming) folded.** Pass-3 independently RE-DERIVED
> and CONFIRMED all v1+v2 folds (spec_conformance binary, no-E-codes, capture-tail
> citation, the closure-tail TWO-hook core, pos-2 ctor-gating, the taint infra
> faithfulness) and raised **1 MATERIAL reservation that survived both prior
> passes:** the 19-fixture reuse corpus is a STRICT SUBSET (13/15) — the two
> GENERIC-PAYLOAD shapes (`Option[R] b = a`, `Result[R,int]`/`Result[int,R]`) have
> NO `.gg` fixture, yet exercise the distinct `RTGeneric` args-recursion
> (`mod.rs:489-501`) that NO §8 gate covers → a mis-mirror ships a silent
> under-rejection (Core-#8 defect the DoD forbids). FOLDED: §7/DoD now MANDATE
> authoring the 2 generic-payload reject fixtures (M1). Everything else pass-3
> verified clean + executor-ready.
>
> **v3-FINAL — pass-4 (Opus, fresh, confirming): SIGN OFF.** Pass-4 independently
> re-derived the v3 fold (generic-payload fixtures mirror ggdef `tests.rs:1129/1144`
> exactly; setup `Some(R(1))` is a fresh temp → rejects on the BIND line, not
> setup; M1 placement + `RTGeneric` args-recursion coverage correct) and re-verified
> whole-brief coherence across all 4 versions — NO stale remnant, NO fresh defect,
> all 15 ggdef shapes covered. Three ZERO-execution-impact polish nits (illustrative
> `;`-separated snippets, omitted prelude, `void drop(!self)` vs `R &self`) FOLDED
> into §7. **The gauntlet is CLEAN — brief is EXECUTOR-READY.**

---

## 0. Orchestrator rulings (the scout's 4 owner-questions — decided, not open)

These are doctrine/showcase-determined; the orchestrator ruled them so the
executor has one target. They are NOT to be re-litigated as open questions — a
reviewer may challenge a ruling with a CITED defect, but absent that, they stand.

| # | question | RULING | basis |
|---|---|---|---|
| Q1 | message: M2 shape-aware text vs blunt baseline? | **Mirror M2's shape-aware text.** | Self-host is the elegance showcase (CLAUDE.md); its lane asserts diagnostic TEXT. Shape/position is known at the reject site — no saving from a blunter message. |
| Q2 | positions 5+6 now, or M1-first fast-follow? | **Full 6-position parity in ONE landing** (staged as two milestones). | A partial A2-S that ACCEPTS a ggdef-rejected shape on the self-host lane is a Core-#8 known-defect the orchestrator must not sign off. |
| Q3 | taint storage: `DefInfo` field vs Dict side-table? | **`DefInfo.is_drop_tainted` typed flag.** | Layering rule 2 (typed metadata on the decl) + rule 3 (no parallel sidecar map). Same call Rust made. |
| Q4 | Shared/Weak/Mutex/Channel carve-out: name-match vs typed marker? | **Port Rust's name-match verbatim + file the self-host into the SAME typed-marker follow-up** (TODO.md:243). | Identical taint sets across the two compilers is the bootstrap-critical property; diverging the carve-out risks the over-taint that breaks the bootstrap. |

**This is NOT a new feature nerf beyond the owner-approved D12 wave.** A2-R1 (the
Rust rejection) was already ratified and landed; A2-S is the self-host catching up
to the same already-approved behavior. No new category-A/B flag to the owner is
owed on the reduction axis — but the executor MUST still surface any *new*
compiler gap it discovers (per "Don't redesign around compiler gaps").

---

## 1. Objective + the gap being closed (Core #8: this is a live bug)

The self-host compiler today **silently miscompiles** a drop-tainted implicit copy.
Measured by the scout end-to-end (self-host driver built, run):

| shape | self-host today | Rust gg (post-A2-R1) |
|---|---|---|
| `R b = a` (drop-tainted R, `a` dead after) | **accepts**, emits 3219 B C, `drop 1` once | rejects `E_MoveWithoutOperator` |
| `R b = a; print(a, b)` (`a` LIVE after) | **accepts** → runs → **`drop 1` printed TWICE** (double-drop) | rejects |
| `R c = hh.r` (field place) | **accepts** → divergent drop count | rejects |

The middle row is an **observable double-drop (double-free class) the self-host
emits and runs today**. This is not a "benign, both-agree" structural gap — it is
a live memory-safety defect the self-host commits and Rust gg rejects. Per Core
invariant #8, A2-S must land: the self-host must REJECT exactly what Rust/ggdef
reject.

**Objective:** add drop-taint computation + a six-position `E_MoveWithoutOperator`
(self-host `DkMoveWithoutOperator`) rejection to the self-host typechecker,
mirroring the Rust A2-R1 model + A2-R2 message faithfully, so the self-host driver
rejects the entire `tests/fixtures/d12_drop_purity/*.gg` reject corpus (exit ≠ 0,
no C, diagnostic text) and accepts the legal corpus (exit 0, C emitted), while the
self-host compiling ITSELF stays green (bootstrap fixed-point).

**The premise the A2-S filing rested on is STALE — corrected by the scout.** The
filing (TODO.md:251) said the self-host has "NO safety pass" and needs a NEW
~250-400-line pass. A3/D10a (`414e652a`, landed *after* the filing) already added
the ownership-rejection surface: the per-function `check_carrier_ops` walker, the
`check_local_borrow_bind` reject template, and the diagnostic gate. **A2-S is now
"add a taint pass + hook six positions into the EXISTING walker," ~180-320 `.gg`
lines** (corrects the estimate *downward*).

---

## 2. Grounding (read FIRST — design toward the docs + the landed model)

The executor and every reviewer must ground the design in these, not just current
self-host code (a code-only design faithfully reproduces whatever fossil is there):

- **`decisions.md` D12** (D4-production enforcement, straight-to-error) + **D4**
  (the drop-purity rule: a type with a transitive custom `Drop` is single-owner;
  implicit copy is rejected at the six positions).
- **CLAUDE.md** — "Self-host as the elegance showcase" (self-host reads like the
  user manual — no defensive fossils), "No name matching" (the taint flag is TYPED
  metadata on the decl, NOT a name/prefix test), "Ownership at Consuming Positions"
  (the six positions), Core invariant #8 (reference-grade, not parity-with-wrong).
- **The LANDED Rust model = the spec to mirror:**
  - `git show b72ef446 --stat` + `docs/plans/define-gorget/wave-a2-drop-purity-brief.md`
    (A2-R1: the taint pass `compute_drop_taint` `src/semantic/mod.rs:522-569`;
    `is_drop_tainted_type` `mod.rs:486-514`; the six positions in
    `src/semantic/safety/{check_stmt,check_expr}.rs`; `lvalue_value_type`
    `helpers.rs:769-817` — **NOT needed here**, §4).
  - `git show b4b6124a` (A2-R2: the M2 shape-aware message — `MoveReason`
    {DropTaint, SingleOwner} × `MoveShape` {Whole, FieldIndex, Capture};
    `src/semantic/errors.rs` Display `:1022`; the capture-position variant
    suggests pass-as-arg / `Shared[T]`, NOT `!`).
- **ggdef's 15 D4 tests = the conformance model** (`git show
  b72ef446:spec/ggdef/src/tests.rs:984-1210`): base 9 + A2-R1's 6 parity
  extensions (enumerated in §7). The self-host must reject/accept exactly these
  shapes.
- **The scout report** `docs/plans/define-gorget/scouts/scout-a2-s.md` — the
  measured prototype + all self-host `file:line` hooks.

---

## 3. Where everything lives in the self-host (scout-verified anchors)

The self-host typechecker is symlinked into every stage (incl.
`self_host_lowerer/`). `typecheck.gg` / `diagnostic.gg` / `scope.gg` / `types.gg` /
`infer.gg` / `ast.gg` / `parser.gg` ARE symlink-shared across stages — a single-file
edit to any of these propagates to every stage (confirmed). **But `driver.gg` and
`lower_cow.gg` are NOT shared** — the `has_errors` diagnostic gate lives in
`self_host_lowerer/driver.gg` (`:438-440` build, `:678-680` check), and `lower_cow.gg`
is a `self_host_lowerer`-stage file, a DIFFERENT compilation stage from the typecheck
pass (so it is likely NOT importable from `typecheck.gg` — see §6.7). Paths below are
under `tests/fixtures/self_host_typechecker/` unless a stage is noted. **Anchor line
numbers may drift ±1-2 lines** (e.g. an `args` line is +1 from its `case`); verify at
the edit site, don't trust the number blindly.

**Enforcement plumbing ALREADY in place (from A3/D10a — do NOT rebuild):**
- **Reject template / model:** `check_local_borrow_bind` (`typecheck.gg:477-482`) —
  a syntactic-shape reject pushing `Diagnostic.error(...)`, wired at `SVarDecl`
  (`:492`) and `SAssign` (`:496`). **This is the template A2-S copies.**
- **The exhaustive per-function walker:** `check_carrier_ops_stmt` /
  `check_carrier_ops_expr` (`typecheck.gg:485-730`), driven ONCE per function body
  AFTER `type_check_stmts` (types assigned) at `typecheck.gg:781`. It already
  visits every position D12 needs (§5 table).
- **The diagnostic gate:** `driver.gg:426-440` (build) + `:677-680` (check) — after
  `type_check_module`, `if has_errors(ctx.diagnostics): report_diagnostics(...);
  exit(1)`. Any `Diagnostic.error` an A2-S check pushes → non-zero exit, rendered
  diagnostic, NO C emitted. **Prototype-confirmed.**

**Taint inputs available:**
- **Drop-equip seed:** `types.trait_registry.impls : Vector[EquipInfo]`
  (`types.gg:115`); each `EquipInfo.trait_name` (`types.gg:89`) == `"Drop"` for a
  Drop equip. Resolve `impl_info.self_type` → `RTDefined(d)`/`RTGeneric(d,_)` →
  def_id. Mirrors Rust `mod.rs:522-536`.
- **Field graph (for the transitive fixpoint):** iterate `module.items`
  `IStruct(sd).fields` / `IEnum(ed).variants`, resolve each field type via
  `ast_type_to_resolved` (`types.gg:317`). (Scout recommends AST iteration over the
  name-keyed `variant_field_types` dict — mirrors Rust `mod.rs:544-569`.)
- **Place typing — on-demand + STRUCTURAL:** `infer_expr_type` (`infer.gg:107`)
  resolves `EFieldAccess` via `variant_field_types` (`infer.gg:251-263`) and
  `EIndex` structurally, on demand. **No `lvalue_value_type` port needed** — the
  single riskiest Rust item (the sparse-`expr_types` reroute that caused a
  double-drop) does not exist here (§4). Prototype-proven on `R c = hh.r`.
- **Param ownership (position 6):** `Param.ownership : int` (`ast.gg:212`) lives on
  the Param AST node, NOT on `DefInfo` (`scope.gg:31-38` has only `is_param`,
  `type_id`). Position 6 needs a param-name → (ownership, type) lookup — the
  hardest port item.

---

## 4. Two simplifications vs the Rust port (scout-measured — do NOT reintroduce)

1. **No `lvalue_value_type` port.** The Rust port's single riskiest item (the
   sparse-`expr_types` place-shape hole → double-drop, `helpers.rs:769-817`) does
   NOT exist in the self-host. `infer_expr_type` (`infer.gg:107,251-263`) is a
   recursive on-demand oracle that types `hh.r` / `v[i]` structurally. Use it
   directly for `tainted_place_name` typing. **Do not build a bespoke lvalue
   walker** — that would be redesigning around a gap that isn't there.
2. **Position 4 = a `SReturn` hook PLUS a closure-tail hook** (pass-1 CORRECTION —
   the scout's "one hook" claim was WRONG). Expression-body *functions*
   (`R passthru(R x): x`) and `return` normalize to `SReturn(Some(expr))`
   (`parser.gg:2894` [return stmt] + `:3730/:3745` [expr-body fn]; `:2728` is an
   EDo synthetic return, not a closure). **But closure expression-tails
   (`(R x): x`) normalize to a trailing `SExpr(expr)`, NOT `SReturn`**
   (`parser.gg:2323` inline closure, `:2585` alt closure form). The walker's
   `EClosure` arm recurses the body as stmts (`typecheck.gg:654-655`), so a
   pos-4 `SReturn`-ONLY hook NEVER fires on a closure tail →
   `closure_tail_reject.gg` (`(R x): x`, a closure *param* place) would be
   ACCEPTED on the self-host lane while Rust rejects it (`D12_MOVE_CODE`,
   `integration.rs:5729`) and ggdef requires it (`d4_closure_tail_param_place_rejected`).
   **That is a Core-#8 defect living in M1 (pos-4), not M2.** So pos-4 needs TWO
   hooks: (a) `SReturn` opt_expr (covers return + expr-body-fn) and (b) the
   TRAILING stmt of an `EClosure` body — if it is `SExpr(place)` with a tainted
   place, reject. `x` in `(R x): x` is the closure PARAM (not a capture), so pos-5
   does not rescue it; the pos-4 closure-tail hook is what closes it, in M1.
   (Rust still needed three arms; the self-host needs two — still simpler, but NOT
   one.) The capture-rooted double-report caveat re-scopes accordingly: once
   pos-5 lands (M2), a `(): a` capture-*tail* would fire from BOTH the pos-4
   closure-tail hook AND the pos-5 capture check — skip the pos-4 closure-tail
   report when the tail place's root is a capture (Rust `check_expr.rs:997-1006`,
   the pos-4 closure-tail arm whose comment reads "PARAM-ROOTED tails only … avoids
   a double-report"; the pos-5 capture check it defers to is `:964-996`).

---

## 5. The six positions — self-host walker sites (scout-verified)

| # | position | self-host site(s) in `check_carrier_ops_*` | milestone | risk |
|---|---|---|---|---|
| 1 | bind / assign | `SVarDecl` init_expr (`typecheck.gg:491`), `SAssign` value (`:494`) | **M1** | LOW (prototyped ✅) |
| 2 | ctor / field-init | `EStructLiteral` args (`:669`), `EDotShorthand` args (`:672`, enum variant), ctor-`ECall` args (`:623`) — **gate on ctor-ness** (§6.6a) | **M1** | **MEDIUM** (only pos-1 was prototyped) |
| 4 | return + expr-body **+ closure-tail** | `SReturn` opt_expr (`:503`, return + expr-body-fn) **PLUS** the trailing `SExpr` of an `EClosure` body (`:654`, closure-tail) — **TWO hooks** (§4.2 correction) | **M1** | **MEDIUM** (closure-tail hook un-prototyped) |
| 3 | collection put | `EMethodCall` args (`:627`) — gate on a mutating-builtin + collection-receiver classifier | **M2** | MEDIUM |
| 5 | closure capture | `EClosure` (`:655`) — compute the closure's free tainted outer locals (typecheck-time free-var walk); reject each | **M2** | HARD |
| 6 | materialize-on-write | `&self`/field write through a bare-borrow param — needs a param-name → (ownership, type) lookup | **M2** | HARDEST |

---

## 6. Design (mirror the Rust A2-R1 model faithfully — bootstrap-safety depends on it)

**Milestone 1 (positions 1, 2, 4 + shared taint infra):**

1. **Typed taint flag (Q3):** add `bool is_drop_tainted` to `DefInfo`
   (`scope.gg:31-38`). Blast radius = **3 constructor sites** (`scope.gg:204, 243,
   259`). Read via one accessor (rule 2/3).
2. **Taint pass** `compute_drop_taint(module, &scopes, &types)` — new function
   called in `type_check_module` (`typecheck.gg:2064`) AFTER `build_trait_registry`
   (`:2073`) and BEFORE the item-walk loop (`:2091`). Seed from
   `trait_registry.impls` where `trait_name == "Drop"`; **fixpoint** over
   struct/enum field type-ids (iterate `module.items`, `ast_type_to_resolved` each
   field). Mirror `mod.rs:522-569`. **The fixpoint is REQUIRED** — the scout's
   no-fixpoint prototype ACCEPTED `struct W{R inner}; W w2 = w` (Rust rejects); the
   transitive taint is load-bearing.
3. **`is_drop_tainted_type(type_id, scopes, types) -> bool`** accessor — recurse
   through `RTDefined`(flag) / `RTGeneric`(flag + args, WITH the
   `Channel|Shared|Weak|Mutex` name-matched carve-out — Q4, inherited debt, filed)
   / `RTTuple` / `RTArray` / `RTSlice` / `RTRef`(**false** — borrows aren't copies).
   Mirror `mod.rs:486-514` **exactly** (identical taint set = bootstrap-safe). Note:
   the self-host `ResolvedType` has NO `RTOwned` variant, so Rust's `Owned(inner) =>
   recurse` arm has no analogue — this list is exhaustive for the self-host's
   taint-carrying variants (pass-1 confirmed).
4. **Place helpers:** `expr_is_place(Expr)` (identifier / self / field / non-range
   index chain — mirror `helpers.rs:71-83`) + `tainted_place_name(SpannedExpr,
   scopes, &types, &ctx) -> Option[String]` using `infer_expr_type` for the type
   (NOT a bespoke lvalue walker) + `place_root_name`. A `!x` (EMove) and `.clone()`
   (EMethodCall) are naturally NOT places → legal with no special-casing
   (prototype-confirmed).
5. **Diagnostic kind:** add `DkMoveWithoutOperator` to `DiagKind`
   (`diagnostic.gg:41-60`) + its machine-slug in `diag_kind_str` (`:99-120`).
   `diagnostic.gg` is symlink-shared across stages — one edit. **⚠ pass-2
   CORRECTION — the self-host has NO `error[E_…]` codes.** Its user-facing headline
   is `render_diagnostic` (`:281`) → `severity_str + ": " + d.message` (no code
   bracket); zero `error[E_` literals exist in the self-host source. So do NOT try
   to emit `error[E_MoveWithoutOperator]` — that would be a one-off change to the
   byte-stable headline (which reproduces `gg check` output). **The self-host
   reject lane matches on `error` + message TEXT + the box rule** (the `\u{250c}`
   codespan char), exactly like `self_host_driver_rejects_invalid_program`
   (`integration.rs:18423-18426`) and §7. The conformance KEY across compilers is
   still the E_ CODE conceptually, but the self-host DRIVER test asserts on the
   self-host's native rendered message (its renderer has no E-codes — a pre-existing
   property shared by every self-host diagnostic incl. D10a's `DkLocalBorrowBind`,
   NOT something A2-S introduces). Message must contain `cannot copy` + the place
   name so the assertion is specific.
6. **Hooks (M1):**
   - **(a) pos 1** — `SVarDecl` init_expr + `SAssign` value (prototyped ✅).
   - **(b) pos 2** — `EStructLiteral` / `EDotShorthand` (enum variant) args, and
     `ECall` args **ONLY when the callee resolves to a constructor**. ⚠ `W(a)`
     (ctor) and `use(a)` (plain call) BOTH parse as `ECall`; ctor-ness is decided
     by resolving the callee to a struct/enum name (`infer.gg:149-226`, "struct/enum
     name used as constructor"). **Per CLAUDE.md "Ownership at Consuming Positions,"
     a plain function/method-call arg is a BORROW (legal, NO operator) — pos-2 must
     gate on ctor-ness and MUST NOT reject a tainted place passed to a plain call or
     a method call.** A naive "reject at every `ECall` arg" over-rejects legal
     borrows (guarded by the §7 legal fixture).
   - **(c) pos 4** — TWO hooks (§4.2 correction): `SReturn` opt_expr (return +
     expr-body-fn) AND the trailing `SExpr` of an `EClosure` body (closure-tail).
     Both push the native `DkMoveWithoutOperator` diagnostic (asserted on message
     text, not an E-code — see item 5).

**Milestone 2 (positions 3, 5, 6):**

7. **Pos 3** `EMethodCall` args (`:627`) — gate on a mutating-builtin +
   collection-receiver classifier. **`lower_cow.gg` is a `self_host_lowerer`-stage
   file, NOT importable from `typecheck.gg` (§3) — so add a small TYPED classifier
   in the typecheck stage (rule 2 — no ad-hoc name-matching).** Like pos-2, a plain
   (non-mutating) method-call arg is a BORROW and must NOT be rejected (guarded by
   the §7 legal method-call fixture).
8. **Pos 5** `EClosure` — compute the closure's free tainted outer locals
   (typecheck-time free-var walk); reject each. **Apply the capture-rooted-tail
   skip** (Rust `check_expr.rs:997-1006`, deferring to the pos-5 capture check at
   `:964-996`): a `(): a` capture-tail would otherwise
   fire from BOTH the `SReturn`-in-closure check (pos 4) AND the capture check
   (pos 5) — skip the pos-4 report when the returned place's root is a capture.
9. **Pos 6** `&self`/field-write through a bare-borrow param — needs a param-name →
   (ownership, type) lookup. Add `int param_ownership` to `DefInfo` alongside the
   taint flag (same 3 constructors), or thread the Param list. Mirror Rust's
   position-6 materialize-on-write rejection.

**Message (Q1 — mirror A2-R2's M2 shape-aware text):** the self-host lane asserts
diagnostic TEXT, so match Rust's user-facing string. Whole-place:
`` cannot copy `a`: `a` is a resource (a type with a custom `Drop` is single-owner) — write `!a` to move or `a.clone()` to copy ``. Field/index sub-place: `.clone()`
only (a bare `!hh.r` is a partial move). Capture-position: suggest pass-as-arg /
`Shared[T]`, not `!`. Root-name-only rendering (`hh.clone()` not `hh.r.clone()` for
sub-places) is the SAME low-priority inaccuracy filed for Rust (TODO.md:292) —
accept it identically for parity, do NOT diverge.

---

## 7. Tests + conformance model

**ggdef's 15 D4 tests (`b72ef446:spec/ggdef/src/tests.rs:984-1210`) = the shapes
to match** — base 9: `d4_position_1_bind` · `d4_position_2_ctor_init` ·
`d4_position_3_collection_put` · `d4_position_4_return` · `d4_position_5_capture` ·
`d4_position_6_materialize_on_write` · `d4_allows_fresh_temp_move_and_explicit_move`
(LEGAL `R b = !a`) · `d4_position_6_user_amp_self_mutator_on_tainted_borrow_rejected`
· `d4_user_amp_self_mutator_on_owned_tainted_local_allowed` (LEGAL). A2-R1's 6:
`d4_position_1_bind_option_payload_tainted` · `d4_bind_result_payload_tainted_both_arms`
· `d4_closure_tail_param_place_rejected` · `d4_closure_tail_fresh_temp_allowed`
(LEGAL) · `d4_field_place_bind_rejected` (REJECT-only) · `d4_field_place_return_rejected`
(REJECT-only).

**Self-host test lane — reuse the A2-R1 corpus, and author the SHAPES IT DOESN'T
COVER.** Add `self_host_driver_rejects_d12_*` fns (model: the
`self_host_driver_rejects_*` pattern at `tests/integration.rs:18387+`) that run the
EXISTING `tests/fixtures/d12_drop_purity/*.gg` corpus (19 fixtures, from A2-R1)
through the self-host driver:
- **reject fixtures** → assert non-zero exit + empty stdout (no C) + expected
  diagnostic text (`error` + `cannot copy` + the box rule — §6.5).
- **legal fixtures** → assert exit 0 + C emitted.

**⚠ pass-3 REQUIRED — the 19-fixture corpus is a STRICT SUBSET of the 15 ggdef
shapes (covers 13/15).** The two GENERIC-PAYLOAD shapes have NO `.gg` fixture
(`grep -l 'Option\[\|Result\[' tests/fixtures/d12_drop_purity/*.gg` = zero):
`d4_position_1_bind_option_payload_tainted` (`Option[R] b = a`) and
`d4_bind_result_payload_tainted_both_arms` (`Result[R,int]` AND `Result[int,R]`).
These exercise a DISTINCT, historically-buggy branch — `is_drop_tainted_type`'s
`RTGeneric(def_id, args)` **args-recursion** (`mod.rs:489-501`: `args.iter().any(…)`,
guarded by the `Channel|Shared|Weak|Mutex` carve-out that could WRONGLY swallow a
tainted arg if mis-mirrored) — a different path from the `RTDefined => is_drop_tainted`
path that `pos1_bind_reject.gg` covers. **None of the §8 gates catch a broken/omitted
args-recursion** (the bootstrap proves only no-over-taint; `spec_conformance_selfhost`
is an acceptance floor; no reject fixture binds a generic-payload place). So the
executor MUST AUTHOR THREE reject fixtures into `tests/fixtures/d12_drop_purity/`
(each a full standalone program — `struct R` + `equip R with Drop` + `void main()`,
multi-line Gorget, NO `;` separators — mirroring the exact shapes in ggdef
`tests.rs:1129` / `:1144` and the corpus idiom, e.g. `pos1_bind_reject.gg`):
```gorget
# fixture A — Option payload (mirrors d4_position_1_bind_option_payload_tainted)
struct R: int id
equip R with Drop:
  void drop(!self): print("drop")
void main():
  Option[R] a = Some(R(1))      # setup: Some(R(1)) is a fresh temp → NOT rejected
  Option[R] b = a               # REJECT — bind of a live tainted-generic place
```
plus fixture B (`Result[R, int] a = Ok(R(1))` / `b = a`) and fixture C
(`Result[int, R] a = Error(R(1))` / `b = a`) — the two arms of
`d4_bind_result_payload_tainted_both_arms`. Wire all three into the
`self_host_driver_rejects_d12_*` lane (reject + no C + `cannot copy` text). They are
position-1 binds → **M1**, alongside the taint infra they gate. (The `equip` Drop
signature is `void drop(!self):` — bare consuming `self`, the corpus idiom — NOT
`void drop(R &self):`.) (The over-rejection guard fixture in §7 is likewise authored, not
reused — "reuse the corpus" was never "author nothing"; the standard is
every-ggdef-shape-covered, §10.)

The parity guarantee is that EVERY ggdef-rejected shape has a self-host reject
test — the shared 13-shape `.gg` corpus PLUS the 2 authored generic-payload
fixtures PLUS the over-rejection guard. If a shape ggdef rejects has no self-host
driver test, that is a Core-#8 gap.

**⚠ Over-rejection guard fixture (pass-1 REQUIRED — the gates have a blind spot).**
The bootstrap-cleanliness argument (§8) proves the self-host source has no
UNDER-rejection regression, but it is SILENT to an OVER-rejection: the self-host
source contains ZERO drop-tainted types, and no existing legal fixture passes a
LIVE tainted place to a plain function/method call (`legal_closure_fresh_temp.gg`
passes a fresh temp, not a live place — it would not fire regardless). So a
pos-2/pos-3 that wrongly rejects a BORROW (`use(a)` / `x.method(a)`) would ship
uncaught by every listed gate. Per CLAUDE.md "convert a recurring bug class into an
executable guard": add a LEGAL fixture to the corpus + wire it into the self-host
lane —
```gorget
struct R:
    int id
equip R with Drop:
    void drop(!self): pass          # corpus idiom — consuming !self, NOT R &self
void use(R x): print(x.id)          # borrow — must NOT reject
void main():
    R a = R(1)
    use(a)                          # plain call: LEGAL borrow, must accept
    print(a.id)                     # a still live after the borrow — valid
```
plus the method-call analogue (`x.some_method(a)` where `some_method` is a plain
method → borrow → must accept). Assert exit 0 + C emitted. (`use` is not a keyword —
it is a plain identifier, used as a fn name elsewhere in the tree.) This is the
executable guard that a pos-2/pos-3 over-rejection cannot pass.

---

## 8. Gates (bootstrap-gated — CHUNKED-FOREGROUND, never background-then-end)

Run in the worktree, in this order. **A2-S is bootstrap-gated: the self-host
compiles itself, so an over-broad taint set would reject the self-host's OWN source
→ bootstrap fails.** The scout measured migration surface ≈ 0 (Rust's identical
A2-R1 model already accepts the whole self-host source + lib; zero `equip … with
Drop` in self-host; `VectorDrain` never bound bare) — but the gate is the proof.

1. `cargo build` (the Rust harness is unchanged; this builds `gg`).
2. `cargo test --lib` (~1107; unchanged — sanity).
3. **Self-host driver build** (chunked-foreground, ~2.5 min):
   `GG_BUILD_TIMEOUT_SECS=600 gg build tests/fixtures/self_host_lowerer/driver.gg`.
4. **The new self-host D12 lane:** `cargo test --test integration
   self_host_driver_rejects_d12 -- --test-threads=4 2>&1 | tee
   /tmp/a2s-d12-$RANDOM.log`.
5. **`self_host_bootstrap_fixed_point`** (chunked-foreground, ~150-170 s/stage —
   THE bootstrap-cleanliness gate; if the self-host source has a tainted implicit
   copy the new rejection flags, this fails and names it):
   `cargo test --test integration self_host_bootstrap_fixed_point 2>&1 | tee
   /tmp/a2s-boot-$RANDOM.log`.
6. **The self-host reject/illtyped lanes + the acceptance floor** (pass-2
   RE-CORRECTION — pass-1 wrongly called `spec_conformance_selfhost` a phantom; it
   only checked the `integration` binary). `spec_conformance_selfhost` IS a real
   `#[test]` — but it lives in the **`spec_conformance` binary**
   (`tests/spec_conformance.rs:562`), NOT `integration`, which is why `cargo test
   --test integration spec_conformance_selfhost` matched zero. It is the self-host's
   ACCEPTANCE floor (drives the self-host driver `--emit-c`→`cc`→run over
   `spectests/run/*.gg`) — exactly the gate that catches an A2-S OVER-taint wrongly
   rejecting a legal spectests fixture. Run all three:
   - `cargo test --test spec_conformance spec_conformance_selfhost` (acceptance
     floor — MUST stay green; an over-rejection here fails it).
   - `cargo test --test integration self_host_driver_rejects -- --test-threads=4`
     (the new `self_host_driver_rejects_d12_*` reject tests + existing rejects).
   - `cargo test --test integration self_host_check_rejects`
     (`self_host_check_rejects_illtyped`, `integration.rs:19783`).
7. `cargo test --test integration -- --test-threads=4` (full sweep — the PARENT's
   job at integration, but the executor runs the targeted lanes above).
8. `tests/lints.rs` — if a new enumerated arm-set is added (e.g. the six-position
   hooks), add/extend an arm-count lint so the next position is forced through the
   shared path (sibling-site-drift discipline).

If the bootstrap breaks because the self-host source has a tainted implicit copy:
that is a REAL migration item — migrate it to `!`/`.clone()` (idiomatic, per
"Self-host as the elegance showcase"), do NOT special-case the taint to dodge it.
Scout says this surface ≈ 0; if it isn't, report the measured lines.

---

## 9. Worktree + playbook preamble (non-negotiable — CLAUDE.md "Multi-agent")

Open the executor prompt with:
> Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point
> inside your worktree. NEVER touch `/workspace/gorget` (main) or
> `/workspace/gorget-1` directly. Do NOT `cd` into either. Do NOT use absolute
> paths starting with `/workspace/gorget/...` or `/workspace/gorget-1/...` (your
> worktree nests UNDER `/workspace/gorget`, so an absolute path there writes into
> MAIN). If `pwd` reports `/workspace/gorget` or `/workspace/gorget-1`, STOP.

Plus: `isolation: "worktree"`, `model: "opus"`; `git merge --ff-only gorget-1` on
entry; stage EXPLICITLY by file name (never `git add -a`/`.`/`commit -a`); NEVER
`git stash` (save with `git diff > /tmp/<name>.patch` + `git apply`); **checkpoint
the durable prototype/patch to `docs/plans/define-gorget/scouts/patches/` EARLY and
after each milestone**; run FINAL gates FOREGROUND with generous timeouts (rule-9
stalls bit 5 agents this wave — a backgrounded final gate that ends the agent
before commit loses the work). On an Edit-tool desync, re-Read + retry the Edit —
never fall back to a shell heredoc with an absolute path; after any non-Edit write,
`git -C /workspace/gorget status` and STOP if it shows changes.

---

## 10. Definition of done (acceptance — Core #8 gate)

- [ ] `DefInfo.is_drop_tainted` typed flag + `compute_drop_taint` fixpoint +
      `is_drop_tainted_type` accessor (mirrors `mod.rs:486-569`), Shared/Weak/
      Mutex/Channel carve-out name-matched + filed into TODO:243.
- [ ] All SIX positions reject on the self-host driver; `!x` / `.clone()` legal.
      **Pos-4 has TWO hooks** — `SReturn` (return + expr-body-fn) AND the
      `EClosure`-body trailing `SExpr` (closure-tail): `(R x): x` MUST reject
      (`closure_tail_reject.gg` / `d4_closure_tail_param_place_rejected`). **Pos-2
      gates on ctor-ness** — `use(a)` (plain call) and `x.method(a)` (plain method)
      are BORROWS and MUST accept.
- [ ] **Over-rejection guard fixture** (§7) in the corpus + wired to the self-host
      lane: a live tainted place passed to a plain function/method call ACCEPTS
      (exit 0 + C). Without it, a pos-2/pos-3 over-rejection ships uncaught.
- [ ] `DkMoveWithoutOperator` diagnostic kind + render arm; message mirrors A2-R2's
      M2 shape-aware text (whole vs field/index vs capture).
- [ ] `self_host_driver_rejects_d12_*` tests: every ggdef-rejected shape rejects on
      the self-host lane; every legal shape accepts. The corpus = the 13 covered
      `d12_drop_purity/*.gg` fixtures **PLUS 2 AUTHORED generic-payload reject
      fixtures** (`Option[R] b = a`; `Result[R,int]` and `Result[int,R]` — the
      `RTGeneric` args-recursion, `mod.rs:489-501`, which no existing fixture
      covers) **PLUS the over-rejection guard** (§7). All 15 ggdef shapes covered.
- [ ] **`self_host_bootstrap_fixed_point` GREEN** (the self-host compiles itself
      under the new rejection — bootstrap-cleanliness proof).
- [ ] `spec_conformance_selfhost` green; full integration sweep green (parent).
- [ ] Arm-count lint extended for the six-position hook set.
- [ ] **No known-defect ships** (Core #8): if any ggdef-rejected shape is NOT
      rejected on the self-host lane, that shape gets an `#[ignore]`d driver test
      asserting the CORRECT (reject) behavior + a sharp TODO — never a silent gap.
      A milestone-partial landing (M1 only) is legitimate ONLY as an
      explicitly-labelled-incomplete state with those `#[ignore]`d tests; the
      default target is full M1+M2 in one landing.
- [ ] No self-host defensive fossil introduced (elegance-showcase rule); any
      self-host-source migration is idiomatic `!`/`.clone()`, not a taint dodge.

---

## 11. Non-goals + follow-ups to file (do NOT expand scope)

- **Typed builtin-marker for the Shared/Weak/Mutex/Channel carve-out** — port the
  name-match verbatim (Q4); file the self-host side into the SAME follow-up as Rust
  (TODO.md:243). Do not solve the typed marker here.
- **Root-name-only sub-place rendering** (`hh.clone()` vs `hh.r.clone()`) — the
  same low-priority inaccuracy as Rust (TODO.md:292); accept identically, do not
  fix here.
- **The it-lambda position** (`hs.map(it.r)`) — DESCOPED-and-filed on the Rust side
  (no it-typing infra at the safety pass); the self-host inherits the same
  descope. Do not attempt.
- Any NEW compiler gap the executor hits → fixture + sharp TODO citing it (per
  "Don't redesign around compiler gaps"), never a reshape to dodge it.
