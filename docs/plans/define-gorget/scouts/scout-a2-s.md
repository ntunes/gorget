# Scout report — A2-S: port D12 drop-purity enforcement to the SELF-HOST compiler

**Read-only scout.** Worktree `/workspace/gorget/.claude/worktrees/agent-a4e5111e8c13d681d`
(verified inside worktree; prototype applied, MEASURED, then reverted — `git status`
clean except this report). Compiler + self-host lowerer driver built THIS session
(`gg build tests/fixtures/self_host_lowerer/driver.gg` → 9 MB binary). All findings
below are freshly measured, not source-read.

---

## TL;DR — the headline (a STALE premise, corrected)

The A2-S filing (TODO.md:251, from the A2 scout 2026-07-11) says the self-host has
**"NO `E_MoveWithoutOperator` surface at all (no safety pass, no ownership diagnostics
— `diagnostic.gg:41-60`)"** and A2-S is a **NEW ~250-400-line pass**.

**That premise is now PARTLY STALE.** A3/D10a (`414e652a`, landed 2026-07-11 **15:30** —
*after* the A2 scout filed A2-S the same day) added exactly the ownership-rejection
surface the premise said was missing. A2-S is **no longer "build a new pass from
scratch"** — it is "add a drop-taint computation + hook the six positions into the
EXISTING per-function `check_carrier_ops` walker," modeled on D10a's
`check_local_borrow_bind`. This SHRINKS and DE-RISKS the port. Confirmed end-to-end by
a working bind-position prototype (below).

**And the gap A2-S closes is a REAL observable defect, not a cosmetic one.** Measured:
the self-host today SILENTLY MISCOMPILES `R b = a` (a live-source copy of a
drop-tainted `R`) into an **observable DOUBLE-DROP** (`drop 1` printed twice) that
Rust gg now rejects. This is a memory-safety defect (double-free class), not a "benign,
both-agree" structural gap. Core-invariant #8 applies: A2-S is closing a live bug.

---

## §1 — Premise verification (file:line, all CONFIRMED or CORRECTED)

### 1.1 Does the self-host enforce D12 today? — NO (measured). CONFIRMS the gap.

Built the self-host lowerer driver and ran it on a drop-tainted bind
(`struct R … equip R with Drop … / R a = R(1); R b = a`):

```
$ driver d12_reject.gg lib --lir-c
exit: 0   stdout(C) bytes: 3219      # ACCEPTED, emits C, no diagnostic
```

Identical 3219-byte C for `R b = a` and `R b = !a` — the self-host does not distinguish
the copy from the move. **Rust gg (post-A2-R1) REJECTS** the same program:
`error[E_MoveWithoutOperator]: cannot copy \`a\`: … single-owner …` (verified this
session). Gap confirmed.

**Runtime consequence — measured, NOT benign in the general case:**

| shape | self-host `driver run` | Rust gg | verdict |
|---|---|---|---|
| `R b = a` (a dead after) | `use 1` / `drop 1` — **1 drop** | rejects | benign (liveness moves at last-use) |
| `R b = a; print(a,b)` (a LIVE after) | `a=1 b=1` / **`drop 1` / `drop 1`** — DOUBLE-DROP | rejects (`E_MoveWithoutOperator`) | **live unsoundness** |
| `R c = hh.r` (field place) | `c=1 hh=1` / `drop 1` (drops once — wrong count either way) | rejects | divergent miscompile |

The live-source row is a genuine double-drop the self-host emits and runs today. So the
"Rust-first split creates no new OBSERVABLE divergence / benign structural gap" framing
in the A2-R brief (lines 22-29) is true only at the *differential* level (the
Rust-rejects/self-host-accepts divergence pre-existed); the self-host's *acceptance is
itself an observable defect*. A2-S is the fix, and per Core #8 it must land — not an
optional parity nicety.

### 1.2 Where does the D12 check GO? — the EXISTING `check_carrier_ops` walker.

A3/D10a added, in `self_host_typechecker/` (symlinked into every stage incl.
`self_host_lowerer/`):

- **The rejection surface / model:** `check_local_borrow_bind` (`typecheck.gg:477-482`)
  — a syntactic-shape reject that pushes `Diagnostic.error(...)`, wired at `SVarDecl`
  and `SAssign` (`typecheck.gg:492, 496`). This IS the template A2-S copies.
- **The exhaustive per-function walker:** `check_carrier_ops_stmt` /
  `check_carrier_ops_expr` (`typecheck.gg:485-730`), driven ONCE per function body
  AFTER `type_check_stmts` (so types are assigned) at `typecheck.gg:781`.
  It already visits EVERY position D12 needs (enumerated in §1.4).
- **The diagnostic gate:** `driver.gg:426-440` (build) and `:677-680` (check) — after
  `type_check_module`, `if has_errors(ctx.diagnostics): report_diagnostics(...); exit(1)`.
  Any `Diagnostic.error` an A2-S check pushes → non-zero exit, rendered diagnostic, NO C
  emitted. Confirmed by the prototype (§2).

So the enforcement *plumbing* (walker + gate + diagnostic rendering) is 100% in place.
A2-S adds the taint computation + the per-position `tainted_place_name` checks.

### 1.3 Is the taint info available? — YES, and place-typing is SIMPLER than Rust.

- **Drop-equip seed:** `types.trait_registry.impls : Vector[EquipInfo]`
  (`types.gg:115`), each `EquipInfo` has `String trait_name` (`types.gg:89`; `""` for
  inherent equips, the trait name for trait equips — `traits.gg:233-237, 293`). Seed =
  scan impls for `trait_name == "Drop"`, resolve `impl_info.self_type` →
  `RTDefined(d)`/`RTGeneric(d,_)` → def_id. Mirrors Rust's `compute_drop_taint` seed
  (`src/semantic/mod.rs:522-536`).
- **Field graph (for the transitive fixpoint):** available two ways —
  `types.variant_field_types : Dict[String,int]` keyed `"Struct.field"→type_id`
  (`types.gg:151`, populated `typecheck.gg:1918-1940`), OR (cleaner for enumeration)
  iterate `module.items` `IStruct(sd).fields` / `IEnum(ed).variants` and resolve each
  via `ast_type_to_resolved` (`types.gg:317`). Recommend AST iteration — it mirrors
  Rust's `field_types`/`variant_field_types` fixpoint (`mod.rs:544-569`) and avoids the
  name-keyed dict's per-struct enumeration problem.
- **⭐ Place typing is on-demand + STRUCTURAL in the self-host — no `lvalue_value_type`
  port needed.** Rust needed a bespoke `lvalue_value_type` walker (helpers.rs:769-817)
  because its `expr_types` span-table is SPARSE (field/index spans never recorded) — the
  pass-3 BLOCKING place-shape hole (double-drop). The self-host has NO such hole:
  `infer_expr_type` (`infer.gg:107`) is a RECURSIVE oracle that resolves `EFieldAccess`
  via `variant_field_types` (`infer.gg:251-263`) and `EIndex` structurally, on demand.
  Calling it on `hh.r` returns R's type directly. **The single riskiest item in the
  Rust port (the double-drop-causing sparse-table reroute) does not exist here.**
  Prototype-proven: the field-place shape `R c = hh.r` rejects (§2).

### 1.4 The six positions in the self-host walker — all sites already VISITED.

| # | position | self-host site(s) | effort |
|---|---|---|---|
| 1 | bind / assign | `SVarDecl` init_expr (`:491`), `SAssign` value (`:494`) | **EASY** (prototyped) |
| 2 | ctor / field-init | `EStructLiteral` args (`:669`), `EDotShorthand` args (`:672`, enum variant), `ECall` args (`:623`, ctor-call) | EASY |
| 3 | collection put | `EMethodCall` args (`:626`) — gate on a mutating-builtin + collection receiver classifier | MEDIUM |
| 4 | return **+ expr-body + closure-tail** | `SReturn` opt_expr (`:503`) — **all THREE spellings** (see below) | **EASY** |
| 5 | closure capture | `EClosure` (`:655`) — needs free-var/capture computation at typecheck time | HARD |
| 6 | materialize-on-write | a `&self`/field write through a bare-borrow param — needs param-ownership tracking | HARDEST |

**⭐ Position 4 collapses to a SINGLE `SReturn` hook.** The self-host parser normalizes
BOTH expression-body functions (`R passthru(R x): x`) AND closure expression-tails
(`(R x): x`) to `SReturn(Some(expr))` (`parser.gg:2728, 2894, 3730, 3745`). Rust needed
THREE separate arms (`Stmt::Return`, `FunctionBody::Expression` at check_stmt.rs:1747,
`Expr::Closure` tail at check_expr.rs:948) plus a capture-rooted-skip; the self-host
gets all three from one `SReturn` check. (The capture-rooted double-report caveat still
applies once position 5 lands — a `(): a` capture-tail would fire from both the
`SReturn`-in-closure check and the capture check; handle with the same root-is-capture
skip Rust uses, `check_expr.rs:150-155`.)

**Param ownership for position 6 IS tracked** — `Param.ownership : int` (`ast.gg:212`)
— but it lives on the Param AST node, not on `DefInfo` (which has only `is_param`,
`type_id`; `scope.gg:31-38`). Position 6 needs a param-name → (ownership,type) lookup,
which the self-host does not currently expose at the walker. This is why position 6 is
the hardest port item.

### 1.5 ggdef's normative suite = the conformance model. FIFTEEN `#[test]` fns.

Enumerated from `git show b72ef446:spec/ggdef/src/tests.rs:984-1210`. Base 9 + A2-R1's
6 parity extensions:

Base (9): `d4_position_1_bind` · `d4_position_2_ctor_init` · `d4_position_3_collection_put`
· `d4_position_4_return` · `d4_position_5_capture` · `d4_position_6_materialize_on_write`
· `d4_allows_fresh_temp_move_and_explicit_move` (LEGAL `R b = !a`) ·
`d4_position_6_user_amp_self_mutator_on_tainted_borrow_rejected` ·
`d4_user_amp_self_mutator_on_owned_tainted_local_allowed` (LEGAL).

A2-R1 extensions (6): `d4_position_1_bind_option_payload_tainted` (`Option[R]`) ·
`d4_bind_result_payload_tainted_both_arms` (`Result[R,int]` AND `Result[int,R]`) ·
`d4_closure_tail_param_place_rejected` · `d4_closure_tail_fresh_temp_allowed` (LEGAL) ·
`d4_field_place_bind_rejected` (REJECT-only, no `!hh.r` legal — partial move) ·
`d4_field_place_return_rejected` (REJECT-only).

The self-host must reject/accept exactly these shapes. Also 19 probe fixtures already
exist under `tests/fixtures/d12_drop_purity/*.gg` (from A2-R1) — A2-S should run those
SAME fixtures through the self-host driver lane (the `self_host_driver_rejects_*` test
pattern, `integration.rs:18387+`) rather than authoring new source, so the two compilers
are pinned to one corpus.

### 1.6 Bootstrap cleanliness — the self-host source is ALREADY D12-clean. ~ZERO migration.

- **Direct evidence (this session):** `gg build tests/fixtures/self_host_lowerer/driver.gg`
  SUCCEEDED. That runs Rust gg's FULL A2-R1 D12 check (all six positions + fixpoint +
  place-shape) over the ENTIRE self-host source + `lib/`. It did not reject anything.
  **So the self-host source contains no drop-tainted implicit copy Rust's model flags.**
  Since A2-S mirrors that exact model, the self-host compiling ITSELF will be clean too.
- **Corroborating:** the self-host source defines **zero** `equip … with Drop`
  (grep across all `self_host_*/*.gg` = 0). The only custom Drop in reach is
  `VectorDrain[T]` (`lib/std/iter.gg:369`), and the self-host never binds one bare —
  the sole `.drain(` hit is a COMMENT (`self_host_check/loader.gg:140`).
- **Prototype corroboration:** the prototype driver's `check` on a real self-host module
  (`scope.gg`) exited 0 — no spurious rejection.

**Conclusion: migration surface ≈ 0 lines** (contrast A2-R1's 3 fixture-source
migrations, which were in the test corpus, not self-host source). The residual risk is
ONLY if A2-S's taint set is BROADER than Rust's (e.g. mishandling the
Shared/Weak/Mutex/Channel carve-out or VectorDrain's generic-equip taint) — mitigated by
mirroring `is_drop_tainted_type` (`mod.rs:486-514`) exactly and gating on the bootstrap.

---

## §2 — End-to-end measurement (the prototype)

Applied a MINIMAL bind-position prototype to `self_host_typechecker/typecheck.gg`
(≈70 lines: `def_has_drop_equip_proto` + `type_is_drop_tainted_proto` [direct + generic
args, NO struct fixpoint] + `expr_is_place_proto` + `place_root_name_proto` + a
7-line hook in the `SVarDecl` arm pushing a `Diagnostic.error`), rebuilt the driver
(~2.5 min), MEASURED, then `git checkout --` reverted it (worktree clean).

| test | program | result | expected |
|---|---|---|---|
| bind reject | `R b = a` | exit 1, 0 C bytes, `cannot copy \`a\`…` | ✅ reject |
| move accept | `R b = !a` | exit 0, 3219 C bytes, 0 err | ✅ accept |
| live-source double-drop | `R b = a; print(a,b)` | exit 1, 0 C | ✅ reject (closes the double-drop) |
| **field-place** | `R c = hh.r` | exit 1, 0 C | ✅ reject (**structural place-typing via `infer_expr_type` — no `lvalue_value_type`**) |
| transitive (no fixpoint) | `struct W{R inner}; W w2 = w` | exit 0, 4695 C | ⚠ ACCEPTED — confirms the **fixpoint is REQUIRED** in real A2-S (Rust rejects) |
| self-host source | `driver check scope.gg lib` | exit 0 | ✅ no spurious rejection |

**What the prototype PROVES:** (a) the Drop-equip seed from `trait_registry.impls`
works; (b) `infer_expr_type` delivers structural field/index place typing — the Rust
port's single riskiest item is a non-issue here; (c) the diagnostic gate rejects
correctly (exit 1, no C, rendered message); (d) `!a` (EMove) and `.clone()`
(EMethodCall) are naturally legal because they are not places — no special-casing;
(e) Rust gg still compiles the prototyped self-host source (bootstrap-inert to the added
`.gg`). **What it deliberately OMITS** (and thus flags as required real work): the
struct-field transitive fixpoint, the Shared/Weak/Mutex/Channel carve-out, positions
2/3/5/6, and a proper `DkMoveWithoutOperator` diagnostic kind.

---

## §3 — Design proposal + migration surface

Mirror the Rust A2-R1 model faithfully (that is what keeps the bootstrap green and the
two compilers at parity). Concretely:

1. **Typed taint flag (layering rule 2):** add `bool is_drop_tainted` to `DefInfo`
   (`scope.gg:31-38`). Blast radius = **3 constructor sites** (`scope.gg:204, 243, 259`)
   — trivially small; the faithful "flag on the decl" choice. (Alternative: a
   `Dict[int,bool]` on `TypeTable`, but the DefInfo field mirrors Rust and reads via one
   accessor — rule 3.)
2. **Taint pass** `compute_drop_taint(module, &scopes, &types)` — a new function called
   in `type_check_module` (`typecheck.gg:2064`) AFTER `build_trait_registry` (`:2073`)
   and BEFORE the item-walk loop (`:2091`). Seed from `trait_registry.impls` where
   `trait_name == "Drop"`; fixpoint over struct/enum field type-ids gathered by iterating
   `module.items` (IStruct.fields / IEnum.variants → `ast_type_to_resolved`). Mirror
   `mod.rs:522-569`.
3. **`is_drop_tainted_type(type_id, scopes, types) -> bool`** accessor — recurse through
   `RTDefined`(flag) / `RTGeneric`(flag + args, with the `Channel|Shared|Weak|Mutex`
   name-matched carve-out — inherited debt, same as Rust `mod.rs:496`, filed) /
   `RTTuple` / `RTArray` / `RTSlice` / `RTRef`(false — borrows aren't copies). Mirror
   `mod.rs:486-514`.
4. **Place helpers:** `expr_is_place(Expr)` (identifier/self/field/non-range-index chain,
   mirror `helpers.rs:71-83`) + `tainted_place_name(SpannedExpr, scopes, &types, &ctx)
   -> Option[String]` using `infer_expr_type` for the type (NOT a bespoke lvalue walker)
   + `place_root_name`.
5. **Six-position hooks** into `check_carrier_ops_*`:
   - pos 1: `SVarDecl` init_expr + `SAssign` value (prototyped ✅)
   - pos 2: `EStructLiteral`/`EDotShorthand`/ctor-`ECall` args
   - pos 3: `EMethodCall` args, gated by a mutating-builtin + collection-receiver
     classifier (check whether `lower_cow.gg` already exposes a reusable predicate — if
     not, a small typed classifier, per rule 2 — no ad-hoc name-matching)
   - pos 4: `SReturn` opt_expr — **one hook covers return + expr-body + closure-tail**
   - pos 5: `EClosure` — compute the closure's free tainted outer locals (typecheck-time
     free-var walk) and reject each
   - pos 6: `&self`/field-write through a bare-borrow param — needs a param-name →
     ownership+type lookup (add `int param_ownership` to DefInfo alongside the taint flag,
     or thread the Param list)
6. **Diagnostic kind:** add `DkMoveWithoutOperator` to `DiagKind` (`diagnostic.gg:41-60`)
   + one render arm (`:101-120`). One edit (diagnostic.gg is symlink-shared across stages).
7. **Message:** RECOMMEND the M2 shape-aware text Rust now emits — measured this session:
   `cannot copy \`a\`: \`a\` is a resource (a type with a custom \`Drop\` is single-owner)
   — write \`!a\` to move or \`a.clone()\` to copy`; for field/index sub-places, `.clone()`
   only (a bare `!hh.r` is a partial move). The self-host lane asserts the message TEXT
   (it renders codespan diagnostics), so matching Rust's user-facing string is the
   reference-grade choice and costs nothing extra. (See §5 Q1.)
8. **Tests:** add `self_host_driver_rejects_d12_*` fns (model: `integration.rs:18387+`)
   that run the EXISTING `tests/fixtures/d12_drop_purity/*.gg` corpus through the
   self-host driver — reject fixtures assert non-zero + no C + diagnostic text; legal
   fixtures assert exit 0 + C emitted. Reusing the A2-R1 corpus pins both compilers to
   one set of programs.

**Migration surface: ~0 self-host-source lines** (§1.6 — Rust's identical model already
accepts the whole self-host source, proven by the successful build this session).

**Line estimate: ~180-320 .gg lines** — CORRECTS the TODO's ~250-400 DOWNWARD. The
walker, gate, diagnostic rendering, place-typing oracle, and D10a template already exist;
the new code is the taint pass (~50), the accessor + place helpers (~50), six hooks
(~60-120, dominated by positions 5+6), the DefInfo field + 3 ctors + diagnostic kind
(~20). Positions 1/2/4 alone (the bulk of the value, all prototype-adjacent) are ~120 lines.

---

## §4 — Recommended slicing + size/risk

**Risk: LOW-MEDIUM, bootstrap-gated.** The mechanism is prototype-proven; the two hard
items (positions 5 capture, 6 materialize-on-write) are localized and independently
testable. The bootstrap can only break if A2-S over-taints vs Rust — mitigated by
mirroring `is_drop_tainted_type` exactly + running `self_host_bootstrap_fixed_point`
(chunked-foreground, ~150-170s/stage) as a gate.

**RECOMMENDATION: ONE A2-S brief, structured as two milestones** (not two separate
briefs — the taint pass is shared, and splitting it doubles the scaffolding):

- **M1 (core — positions 1, 2, 4 + taint pass + fixpoint + DefInfo flag + diagnostic
  kind + the `d12_drop_purity` reject/legal corpus for these positions):** closes the
  observable double-drop (§1.1) and the majority of the ggdef suite. All
  prototype-adjacent, LOW risk.
- **M2 (positions 3, 5, 6 + their probes + the capture-rooted-tail skip):** the
  classifier-dependent (3) and the free-var/param-ownership items (5, 6). MEDIUM risk;
  each has a matching ggdef test to gate against.

Ship M1+M2 in one landing if the gauntlet stays green; the milestone split just lets the
executor stage and de-risk. Do NOT split into two *briefs* — the taint infrastructure is
common and a half-landed A2-S leaves some ggdef shapes un-rejected on the self-host lane
(a Core-#8 known-defect state).

---

## §5 — Owner design questions (with recommendations)

**Q1 — Message parity: mirror M2's shape-aware text, or ship a simpler baseline?**
Rust just landed the M2 shape-aware message (measured: `cannot copy \`a\`: … single-owner
— write \`!a\` to move or \`a.clone()\` to copy`; capture-position variant suggests
pass-as-arg / `Shared[T]`, not `!`). The self-host lane asserts diagnostic TEXT.
**RECOMMEND: mirror M2.** The self-host is the elegance showcase; a divergent, blunter
message there is a visible parity gap for ~no saving (the shape/position is already known
at the reject site). One caveat: the self-host's root-name-only rendering would show
`hh.clone()` not `hh.r.clone()` for sub-places — the SAME low-priority inaccuracy filed
for Rust (TODO.md:292); accept it identically for parity.

**Q2 — Positions 5 & 6 scope: full parity now, or M1 (1/2/4) first with 3/5/6 as a
fast-follow?** Positions 5 (capture free-vars) and 6 (materialize-on-write) are the only
non-trivial ports (no existing self-host free-var/param-ownership surface at typecheck).
**RECOMMEND: full parity in one A2-S landing** (per §4), because a partial A2-S that
accepts a ggdef-rejected shape on the self-host lane is a Core-#8 known-defect the
orchestrator must not sign off. If the owner wants to timebox, M1-only is a legitimate
*labelled-incomplete* landing IF the un-ported positions get `#[ignore]`d self-host
driver tests asserting the CORRECT (reject) behavior + a sharp TODO — never a silent gap.

**Q3 — Taint flag storage: `DefInfo.is_drop_tainted` (mirror Rust) vs a `Dict` side-table?**
**RECOMMEND: the DefInfo field** — layering rule 2 (typed metadata on the decl), 3
constructor edits, one accessor. The Dict side-table is a rule-3 "parallel sidecar map"
smell. (Same call Rust made.)

**Q4 — The Shared/Weak/Mutex/Channel carve-out is name-matched in Rust
(`mod.rs:496`, filed debt TODO.md:243). Port the name-match, or wait for the typed
builtin-marker?** **RECOMMEND: port the name-match** (extends existing inherited debt,
keeps the two compilers' taint sets identical — the bootstrap-critical property), and
add the self-host to the SAME filed typed-builtin-marker follow-up. Diverging the
carve-out between compilers would risk exactly the over-taint that breaks the bootstrap.

---

## §6 — Notes / smaller findings

- The 3 hardest Rust items (sparse-`expr_types` place reroute, three separate
  expr-body/closure-tail arms) are **non-issues in the self-host** (on-demand
  `infer_expr_type`; parser normalizes expr-bodies to `SReturn`). The self-host port is
  structurally SIMPLER than the Rust original in these respects.
- The A2 scout's cited premise anchor `diagnostic.gg:41-60` is now stale: line 57 there
  is `DkLocalBorrowBind` (D10a's ownership diagnostic) — the surface the premise said
  didn't exist.
- Build/gate commands for the executor: driver build =
  `GG_BUILD_TIMEOUT_SECS=600 gg build tests/fixtures/self_host_lowerer/driver.gg`
  (~2.5 min); self-host reject test = `driver <fixture> lib --lir-c` → assert exit≠0 +
  empty stdout + diagnostic on stderr; bootstrap gate =
  `self_host_bootstrap_fixed_point` (chunked-foreground, per-stage).
- Left in the worktree: this report only (`git status` clean otherwise; prototype
  reverted, driver build artifacts are gitignored / cleaned).
