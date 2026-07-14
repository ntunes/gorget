# Wave B1 brief — D10(b) place-overlap CHECK (Rust production + ggdef model + §9.4 docs)

> **Track B1** (second of Batch B's three slices: B0 hand-hoists ✅ LANDED `40772fd4` →
> **B1 the CHECK (this brief)** → B2 self-host mirror). B1 lands the D10(b) place-overlap
> rejection in the **Rust production compiler** and the **ggdef executable definition**,
> plus the per-arm fixtures and the §9.4 docs rewrite. **B1 does NOT touch the self-host
> `.gg`** — that is B2 (which gates on B1 as the reference). B1 gates on B0 (the in-repo
> hoists must already be landed, else the new check breaks the lib/self-host build + bootstrap).
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-batch-b.md` — §1b (the two gaps + matrix),
> §1c (the PROVEN prototype: projection-prefix keying, ~40 lines, t1/t3/t4/t5/t6 reject +
> t7 disjoint-sibling accept), §1d + §3.5 (ggdef + docs), §5-Q2 (Copy-read exemption).
> **Owner ruling:** `decisions.md` LOG **2026-07-12 "D10(b) ADDENDUM"** (the live-alias rule
> + Rider 1 movers + **Rider 2 = read the TYPED Copy axis + ggdef models the identical rule,
> gates on the FULL ggdef suite** + provenance-bit DEFERRED (B)).
>
> **Status:** v2 — **pass-1 + pass-2 (both Opus, fresh) folded.** Pass-1 CONFIRMED the hardest
> call (ggdef, claim 4 — Rider 2 verbatim requires it, ggdef has elaboration infra + a
> projected-D10(a) precedent `tests.rs:1614-1643`) + rated the reference-grade gate exemplary,
> and raised 6 reservations (R1 Copy-exemption `is_copy` needs `lvalue_value_type` fallback;
> R2 risk relabel; R3 `(Move,Move)` reframe; R4 self/Variable mechanism; R5 §3.5 anchor; R6
> index-collapse triage) — ALL FOLDED into v1. **Pass-2 re-derived everything, confirmed all 6
> folds correct+complete, AND found a HIGH defect both prior passes missed + 2 citation nits:**
> **(P2-R1, HIGH → OWNER-RULED)** the mover-Copy POSITIVE `f(!x, x.copy_field)` (owner Rider 1
> "legal") is UNACHIEVABLE in `check_call_aliasing` — it's rejected UPSTREAM by the move-tracker
> (`E_UseAfterMove`: `!x` marks `x` moved, then `x.copy_field` is use-of-moved) AND rejecting it
> is actually reference-grade (Rust rejects the identical program; a move consumes storage). I
> MEASURED all three cases on gg @ HEAD (`f(!s,s.n)`→E_UseAfterMove; `f(&s,s.n)`→accepted;
> `f(s.n,!s)`→accepted+runs) and took it to the **owner, who RULED (2026-07-14, firm): REJECT
> the mover-Copy case** — revise Rider 1's mover clause (the mover-Copy reject is LIVENESS
> `E_UseAfterMove`, NOT place-overlap; the "Copy reads are snapshots not aliases" principle
> stands uniformly). Folded: §2.1 no longer targets the mover-Copy positive; §4 makes
> `f(!x,x.copy_field)` a NEG **pinned to the `E_UseAfterMove` diagnostic** (catches move-tracker
> drift) + adds the order-twin `f(s.copy_field,!x)` POS (eval-order-sensitive); §3 requires ggdef
> to ALSO reject the mover-Copy case via its liveness rule or FILE the gap (Core #8 no-divergence);
> `decisions.md` Rider 1 REVISED with the measurements cited. **(P2-R2/R3, LOW)** citation fixes:
> the partial-move-widening quote is in the 2026-07-11 D10(a) ADDENDUM (cited by entry now); the
> call-arg move site is `check_expr.rs:234-243` (not the `:154-161` expression arm). Awaiting
> pass-3 (fresh, confirming this fold).
>
> **Risk profile:** LOW (proven projection/sigil core) · MEDIUM (writer-Copy exemption path +
> ggdef model, both new/unproven; the mover-Copy axis is now OUT of scope — liveness, not overlap).

---

## 0. What B1 is, and the rulings it rests on

**D10(b) (ratified):** within a single call, two arguments whose **places overlap** under
**conflicting sigils** are rejected. "Conflicting" = at least one arg is a **writer** (`&`)
or **mover** (`!`/`^`) and they are not **both bare readers**. The check ranges over **LIVE
ALIASES** (the 2026-07-12 ADDENDUM): `&`/`!`/`^` writers-movers **and non-Copy bare reads**;
a **Copy-typed bare read is a value snapshot** that participates in NO overlap (it is
evaluated into an independent value before the callee runs — no memory edge to diverge
against). This is the ALIAS-vs-VALUE cut, and it matches Rust's two-phase-borrow behavior
(`f(&mut s, s.int_field)` compiles; `f(&mut s, &s.vec)` rejected) — independent derivation,
same cut.

**Rider 1 (movers) — REVISED by the owner 2026-07-14 (see `decisions.md`):** the "Copy reads
are snapshots not aliases" principle applies uniformly (writers AND movers), so a Copy read is
NEVER a live alias. BUT the mover-Copy case `f(!x, x.copy_field)` is **REJECTED** — by a
DIFFERENT axis: LIVENESS (`E_UseAfterMove`), because `!x` CONSUMES the slot, so the later read
is dead-slot access (Rust rejects the identical program; MEASURED E_UseAfterMove on gg @ HEAD).
The place-overlap rule neither needs nor grants a mover Copy-exemption. A non-Copy bare read
overlapping a mover IS a place-overlap reject (a live alias into a moving place). So: writer +
Copy read = exempt (legal); mover + Copy read = liveness reject (E_UseAfterMove, NOT overlap);
read-BEFORE-move (`f(s.copy_field, !x)`) = legal (eval order).

**Rider 2 (implementation, LOAD-BEARING for B1):**
1. the check MUST read the **TYPED Copy axis** — `is_copy_type` / `expr_value_is_copy`
   (the A2-R1 Copy∧Drop machinery), **rule 2, NO shape/name heuristics**;
2. **ggdef models the IDENTICAL rule in the same track**, with fixtures pinning BOTH
   directions per position (legal Copy-read positive + non-Copy-read rejection negative);
   **the track gates on the FULL ggdef suite** (Batch A's lesson: a rule track flips/adds
   ggdef expectations).

**Provenance bit DEFERRED (B):** call args already carry typed `CallArg.ownership`
(`arg.node.ownership`) at the exact site the check consumes, and the place primitive
`find_root_def_id_with_path` already exists — so the check keys on **SYNTACTIC
root+projection-prefix**, no borrow-provenance bit, no shape-walk, no sibling-drift.

**Scope discipline:** B1 is Rust production + ggdef + docs + fixtures. NOT the self-host
`.gg` (B2). NOT the value-position no-op-`&` family (TODO — separate rider). NOT the
borrow-provenance bit (deferred). Do NOT re-litigate the rulings above.

---

## 1. Current state — the check and its two gaps (re-verify each anchor)

`check_call_aliasing` (`src/semantic/safety/helpers.rs:1124-1186`), called at
`check_expr.rs:206` (Call) and `:371` (MethodCall). Current behavior (scout §1a, 6 probes,
`gg check`/build+run at HEAD — re-verify still reproduces on the branch):

| Probe | Shape | Current | D10(b) wants |
|-------|-------|---------|--------------|
| t1 | `f(&n, &n)` | REJECT (E_BorrowConflict) | reject ✓ (already) |
| t2 | `f(&n, n)` | REJECT | reject ✓ (already) |
| **t3** | `f(n, !n)` | **ACCEPT (GAP 1)** | **REJECT** |
| t4 | `f(!n, !n)` | REJECT (via E_DoubleMove, a different path) | reject (keep as-is) |
| **t5** | `f(&n, &n.field)` | **ACCEPT (GAP 2) — write through `&n.field` SILENTLY LOST (prints 1 not 2)** | **REJECT** |
| **t6** | `f(n.data, &n)` | **ACCEPT (GAP 2)** | **REJECT** (non-Copy sub-place read overlapping a writer) |
| t7 | `f(&m.a, &m.b)` | ACCEPT | **ACCEPT (must stay)** — disjoint siblings |

t5 is the sharpest reference-grade defect (Core #8): compiles, silently drops a write.

**The two gaps in the code:**
- **GAP 1 — sigil-arm:** the match at `helpers.rs:1157-1173` has `(MutBorrow,MutBorrow)`,
  `(MutBorrow,Move)|(Move,MutBorrow)`, `(Borrow,MutBorrow)|(MutBorrow,Borrow)` — but is
  MISSING `(Borrow, Move)|(Move, Borrow)` → `f(x,!x)` accepted.
- **GAP 2 — place granularity:** the collector at `:1128-1143` only handles
  `Expr::Identifier` and keys on **DefId equality** (`:1151`). Field/tuple-field/index args
  are never collected, and even two identifier args are compared whole — so `f(&n,&n.field)`
  (root overlap, one is a sub-projection) is invisible.

---

## 2. The fix — Rust production check (scout §1c prototype is the reference)

Rewrite `check_call_aliasing`'s body to key on **(root DefId, projection path)** and apply
the live-alias + Copy-exemption rule. The place primitive **already exists**:
`find_root_def_id_with_path` (`helpers.rs:458-483`) returns `Option<(DefId, Vec<String>)>` —
field names outer-to-inner; **Index/OptionalChain segments collapse to the root** (an index
borrow is from the collection itself; conservative — any `x[i]` overlaps `x`). Reuse it; do
NOT write a second place-walker.

### 2.1 Collection (replace `:1128-1143`)
For each arg: unwrap the sigil (`arg.node.ownership` ∈ {Borrow, MutableBorrow, Move}) and run
`find_root_def_id_with_path(&arg.node.value)`.
- If it returns `Some((root, path))` **and** the root's `DefKind == Variable`, record it.
  **Mechanism (get this right — the filter's meaning is subtle):** function **params ARE
  `DefKind::Variable`** (`resolve.rs:1096-1100` sets `is_param=true`; there is NO
  `DefKind::Parameter`, `scope.rs:10-23`), so the current filter already CHECKS param roots —
  `f(&p, p)` where `p` is a param is rejected today, and B1 must PRESERVE that (do not
  accidentally exclude params). What the current collector skips is **`self`-rooted places**:
  `Expr::SelfExpr` is in the no-op arm of `resolve_expr` (`resolve.rs:1487-1488`) so it gets
  NO `resolution_map` entry → `find_root_def_id_with_path(SelfExpr)` returns `None` → skipped.
  **B1 scope decision:** keep the Variable-root scope (which includes params, excludes `self`)
  for B1 — do NOT try to wire `self`-rooted places now (it needs a `SelfExpr` resolution path
  that doesn't exist and risks double-diagnosing the existing self-mutation checks).
  **But give the gap teeth** ("don't redesign around compiler gaps" rule 2): add an
  `#[ignore]`d negative fixture (`f(&self.a, &self.a.b)` / `f(&self.field, &self)`) whose
  EXPECTED output is rejection, plus a sharp `TODO.md` entry citing it — not a vague
  "follow-up."
- Record `(root, path, ownership, is_copy, span)`. **`is_copy`** = compute the arg VALUE's
  Copy-ness via the TYPED axis (Rider 2, **NOT a name/shape test**):
  ```
  let is_copy = self.expr_value_is_copy(&arg.node.value, self.lvalue_value_type(&arg.node.value));
  ```
  **This exact form is LOAD-BEARING — pass `lvalue_value_type` as the fallback, do NOT call
  `expr_value_is_copy(expr, None)`.** Why: `expr_value_is_copy` (`helpers.rs:798-819`) reads
  `expr_types.get(&expr.span)` first, but **the typechecker does NOT record `expr_types` at
  field/tuple/index spans** (its `infer_expr` returns those types without inserting them —
  documented verbatim at `helpers.rs:821-834`; `FieldAccess` returns `tid` bare at
  `typecheck.rs:~2702`). The PRIMARY exemption input is a field-access read on the WRITER side
  (`s.copy_int` in `f(&s, s.copy_int)`), so with a `None` fallback `is_copy` comes back **false**
  → `f(&s, s.copy_int)` would be WRONGLY REJECTED, failing its POS fixture (§4) and contradicting
  the ADDENDUM's Copy-snapshot principle. (The mover-Copy `f(!x, x.copy_field)` is a separate
  liveness reject — see below — and is NOT a POS fixture.)
  `lvalue_value_type` (`helpers.rs:835`) is exactly the STRUCTURAL resolver built for this
  gap (identifier/self → recorded/decl type; `s.field` → struct `field_types[idx]`; `t.0` →
  tuple elem; `c[i]` → collection elem; unknown → `None` = conservative non-Copy).
  `expr_value_is_copy` then applies `is_copy_type(t) && !is_buffer_owning_type(t)` — the
  latter is the Channel/Shared opaque-pointer caveat (those are non-Copy pointers even though
  `is_copy_type` calls them Copy), which is the semantically-honest "does a bare read snapshot
  an independent value" test. **⚠ UNPROVEN PATH:** the scout's §1c prototype had NO Copy
  exemption (its rule was `overlap ∧ (a||b is &/!) ∧ ¬both-bare`; t8 was a proposed fixture,
  never run). So the Copy-exemption code is NEW — VERIFY it end-to-end against the WRITER-side
  POS fixture `f(&s, s.copy_int)` and the order-twin `f(s.copy_field, !s)` (both must BUILD+RUN,
  not reject — MEASURED accepted on gg @ HEAD) as part of the executor's own gates.
  **The Copy exemption applies ONLY to the WRITER case in `check_call_aliasing`'s scope.** The
  mover-Copy case `f(!x, x.copy_field)` is a SEPARATE axis: it is REJECTED — but by LIVENESS
  (`E_UseAfterMove`), one layer BEFORE place-overlap, because `!x` consumes the slot (measured:
  E_UseAfterMove on gg @ HEAD; Rust rejects the identical program). This is the owner's
  **Rider 1 REVISION 2026-07-14** (`decisions.md`, the "⚠ RIDER 1 REVISED" note): the
  place-overlap rule neither needs nor grants a mover exemption for Copy reads. So the
  exemption code above must NOT try to make `f(!x, x.copy_field)` accept — that would require a
  move-tracker + backend-snapshot change explicitly OUT of B1's scope (and rejected on the
  merits by the owner). See §4 for the fixtures that pin this two-axis layering.
- Non-places (fresh temps, literals, `x.clone()`, `x.get()`) return `None` → **skip** (they
  cannot alias).

### 2.2 Pairwise conflict (replace `:1145-1185`)
For each pair `(i<j)` with the SAME root:
- **overlap** = one projection path is a prefix of the other:
  `a.path.iter().zip(&b.path).all(|(x,y)| x==y)` (the shorter being a prefix of the longer;
  `zip` stops at the shorter — so equal-prefix ⇒ overlap; a divergence at any position ⇒
  disjoint siblings, e.g. `["a"]` vs `["b"]` ⇒ NOT overlap). **Empty path overlaps
  everything with the same root** (whole vs any sub).
- if not overlap → continue (disjoint siblings — t7 stays accepted).
- **Copy-read exemption:** if one arg is a **bare reader** (`Borrow`) **and its `is_copy`**,
  it does not participate — skip the pair on that arg's behalf (a Copy bare read is a
  snapshot). Concretely: a pair conflicts only if, after dropping Copy bare readers, the
  remaining pair still has ≥1 writer/mover and is not both-bare-readers.
- **conflict** = overlap ∧ (≥1 of the pair is `MutableBorrow` or `Move`) ∧ ¬(both `Borrow`)
  ∧ ¬(the sole hazard arg is an exempt Copy bare reader). Add the **`(Borrow, Move)`** arm
  (GAP 1). **Keep `(Move, Move)` ENTIRELY OUT of this check** — E_DoubleMove already covers
  it correctly INCLUDING the projection cases, because **a projection move root-marks**: a
  `!p.field` arg runs `find_root_def_id(inner)` → `check_move(root_def_id)` — the operative
  CALL-ARG site is `check_expr.rs:234-243` (the arg loop; the sibling `Expr::Move` expression
  arm at `:154-161` has the same logic), `check_move` sets Moved + emits DoubleMove
  (`origins.rs:479-505`), and `var_states` is whole-`DefId`-keyed (`mod.rs:277`). So
  `f(!node, !node.field)` is **already rejected** (both mark `node` moved → the second trips
  E_DoubleMove) — there is NO gap to close, and duplicating it here would double-diagnose.
  `f(!m.a, !m.b)` (disjoint sibling moves) is **already over-rejected** by the same
  root-marking — whether it SHOULD accept is an **undecided partial-move-widening policy**
  (`decisions.md`, the **2026-07-11 "D10(a) ADDENDUM"** entry: "Rust-style destructuring partial
  moves remain a possible future WIDENING, undecided"), so B1 does NOT touch it. **Deliverables for this interaction:**
  (a) a NEG fixture pinning `f(!node, !node.field)` REJECT (proves the interaction holds,
  guards a future regression); (b) an `#[ignore]`d fixture + a `TODO.md` note for the
  `f(!m.a, !m.b)` disjoint-sibling-move question (expected-output = whatever the owner
  eventually rules) — do NOT silently bless the current over-rejection as canonical.
- Emit `SemanticErrorKind::BorrowConflict { name, detail }` (reuse — extend the `detail`
  strings per new arm: bare+move, projection-overlap). Point the span at the SECOND
  (overlapping) arg where that reads better than the first; match the existing arms' choice.

### 2.3 Message quality
The `detail` strings must name the actual places (`n` vs `n.field`), not just the root — a
`f(&n, &n.field)` message that says "borrow n mutably more than once" is confusing. Include
the projection (e.g. "cannot pass `&n` and `&n.field` in the same call — the borrows
overlap"). Keep it in the existing `BorrowConflict` kind; do NOT invent a new error code for
B1 (B2/self-host reuses `DkBorrowConflict`).

### 2.4 What must NOT change
- t1/t2 stay rejected (regression), t7 (`f(&m.a,&m.b)`) stays ACCEPTED, and — critically —
  the **~30 self-host disjoint-sibling arg-pairs** the scout catalogued
  (`drop_fn_for_type(&gmod.resource_types, &m.type_runtime_map, …)` in `lir_lower.gg`; lib
  `ParseError.InvalidSyntax(p.pos, !p.err)`) must stay accepted. B0 did NOT touch these
  (they're disjoint, correctly accepted) — but the full sweep is the proof.
- Do NOT weaken the check to make a newly-flagged site pass. See §5 (triage rule).

---

## 3. ggdef executable model (owner Rider 2 — the scout under-scoped this)

The scout (§1d) said "ggdef is value-semantics → prose + fixtures only." That is correct for
ggdef's **runtime** (it cannot observe a lazy/eager CoW divergence). But D10(b) is a
**STATIC** rule, and ggdef **already models the sibling D10(a) rule as an ELABORATION
rejection** (`spec/ggdef/src/tests.rs:1603-1616` — "local `&`-binds are rejected in the
definition"). The owner's Rider 2 is explicit and achievable: **ggdef models D10(b) as an
elaboration-time place-overlap rejection**, mirroring the D10(a) model.

Deliver:
- The elaboration check in ggdef (`spec/ggdef/src/` — find where call-arg elaboration lives;
  the D10(a) rejection is the structural template). Same rule: root+projection overlap ∧
  conflicting sigils ∧ non-Copy — read ggdef's own Copy notion (it has value-semantics types;
  a scalar/Copy read snapshots). If ggdef's model genuinely cannot represent the
  projection/Copy distinction at elaboration (verify — it models ownership and places for
  D10(a)/D4), then and ONLY then fall back to prose + fixtures for the parts it can't express,
  and state precisely which parts and why in the brief-delta (do not silently downscope the
  owner ruling).
- ggdef **fixtures both directions per position**: the D10(b) place-overlap model pins the
  WRITER-Copy-read positive (`f(&s, s.copy_int)` legal) + the non-Copy-read rejection negatives
  (`f(&n,&n.f)`, `f(n.f,&n)`, `f(x,!x)`) + the disjoint-sibling positive (`f(&m.a,&m.b)`) — mirror
  the production fixtures §4. Wire into the ggdef test suite the way the D10(a) tests are wired.
  **⚠ The mover-Copy case `f(!x, x.copy_field)` is NOT a place-overlap fixture** (per the Rider 1
  REVISION it's a LIVENESS reject, not an overlap reject). But production REJECTS it (E_UseAfterMove),
  so **ggdef MUST reject it too or you ship a production-vs-ggdef divergence (Core #8).** VERIFY
  ggdef has a use-after-move / dead-slot notion (the D10(a)/D4 move modeling suggests it does —
  `reject_if_tainted_live_place`, the move-bind rejection); if ggdef genuinely CANNOT reject a
  read of a moved place, that is a GAP to FILE (with a `TODO` + a note that the mover-Copy
  ggdef fixture is deferred) — do NOT let ggdef silently ACCEPT a program production rejects.
- **Gate on the FULL ggdef suite** (`cargo test -p ggdef` or the project's ggdef harness) —
  the rule track flips/adds expectations; the full suite must be green.

If, after genuine investigation, the ggdef static model proves to be a materially larger
piece than the Rust check (its own risk surface), FLAG it in the brief-review as a candidate
**B1b split** — but the DEFAULT per the owner ruling is: ggdef modeled in this track.

---

## 4. Fixtures (production — per-arm negatives + positives)

Add alongside the existing `borrow_conflict_error.gg` / `double_mutable_borrow_error.gg`
(both `f(&v,&v)`, already rejected). New (expected outputs = what the language SHOULD do):
- **NEG** `f(x, !x)` — the (bare, move) sigil-arm gap (t3). Check-time reject.
- **NEG** `f(&node, &node.field)` — projection overlap, both writers (t5) — **plus a run-twin
  fixture that (in the FIXED world) would assert the correct value**; since the program is now
  rejected, the negative fixture IS the proof the silent-lost-write can't happen. (Do NOT wire
  a build+run fixture that expects the buggy value — that would lock in the defect, forbidden.)
- **NEG** `f(node.field, &node)` — non-Copy sub-place read overlapping a writer (t6).
- **POS** `f(&m.a, &m.b)` — disjoint siblings ACCEPT (t7) — the regression guard; build+run,
  assert the correct mutation of BOTH fields.
- **POS** `f(&s, s.copy_int)` — the WRITER-Copy-read exemption (t8) — build+run, assert correct
  (MEASURED accepted on gg @ HEAD).
- **NEG** `f(!x, x.copy_field)` — the mover-Copy case (owner **Rider 1 REVISION 2026-07-14**):
  REJECTED, and **the fixture MUST assert the diagnostic is `E_UseAfterMove`, NOT an aliasing
  error.** This pins the two-axis layering: the move-tracker (liveness) rejects it one layer
  before place-overlap. If a future refactor makes it fail with the overlap error instead,
  that means the move-tracker silently lost a case — this fixture catches that drift. (MEASURED:
  `E_UseAfterMove "use of moved value"` on gg @ HEAD.)
- **POS** `f(s.copy_field, !x)` — the **order twin** (read-BEFORE-move; left-to-right eval):
  build+run, assert correct. Pins the rule as evaluation-order-sensitive — NOT a blanket "no
  reads of `x` in a call that moves `x`." (MEASURED accepted + prints correctly on gg @ HEAD.)
- (optional / note the MECHANISM) `f(!x, x.non_copy_field)` also REJECTS — but via
  `E_UseAfterMove` (liveness), NOT via D10(b) place-overlap, for the same reason as the
  mover-Copy case. Since it tests the move-tracker not the new check, prefer OMITTING it (or
  wire it only with an explicit comment that it pins the liveness path, not place-overlap).

Fixtures follow the existing negative-fixture harness convention (check-error expectation).
The POS build+run fixtures must run on **both backends** (C default + LLVM) via the standard
harness. Name them `place_overlap_*` / extend the existing `borrow_conflict_*` family
consistently.

---

## 5. The over-rejection gate + triage rule (Core #7 — LOAD-BEARING)

The production check is a real semantic tightening. B0 hoisted the **scanned** in-repo sites,
but the scanner is not a proof — **the FULL C + LLVM integration sweep + the bootstrap
fixed-point are the over-rejection gate** (slice validation is NOT sufficient). If the full
sweep newly-REJECTS a site B0 didn't hoist, **TRIAGE — do not weaken the check**:
- **Legitimately field-disjoint-safe but coarsely flagged** (a `f(struct.sub, &struct)`
  shape the scanner missed) → hoist it per B0's pattern (drop the redundant param / restructure),
  add it to the B0 class, note it. It is behavior-preserving.
- **A real overlap** (the check correctly found a `f(x,!x)` / `f(&n,&n.f)` bug) → GOOD, that's
  the point; if it's in-repo production code, FIX the code (it was a latent bug) + note it.
- **Index-collapse over-rejection** (a NEW class B0 didn't scan for): because `x[i]` collapses
  to root `x` (`find_root_def_id_with_path` `:478-480`), `f(&v[i], &v[j])` and `f(&v[i], &v)`
  now REJECT — conservative and per-spec (D10's root+prefix keying, `decisions.md:626`), NOT a
  defect. B0's hoists were field-disjoint-focused, so any in-repo index-disjoint site is caught
  ONLY by the full C+LLVM sweep. If one appears: it's correctly-rejected-per-spec — restructure
  the call site (behavior-preserving) or, if genuinely two disjoint indices that must both pass,
  STOP and report (it may need an owner ruling on index precision) — do NOT weaken the keying.
- **Never** relax the rule, add a name-based carve-out, or reshape a fixture to dodge the
  check (forbidden — "don't redesign around compiler gaps"; here the "gap" is the check
  working). Any genuinely-uncertain case → STOP and report, do not guess.

---

## 6. Gates (run FOREGROUND, CHUNKED, GG_BUILD_TIMEOUT_SECS=600 — rule 9)
1. `cargo build`
2. `cargo test --lib` (the new check's unit-level behavior; ~1107 baseline).
3. **Targeted fixture flip:** `cargo test --test integration place_overlap borrow_conflict double_mutable -- --test-threads=4` (each new NEG rejects, each POS builds+runs+asserts). Confirm EXACTLY the intended fixtures change; zero collateral.
4. **`self_host_bootstrap_fixed_point`** (chunked-foreground; the Rust check now compiles the self-host `.gg` — B0's hoists must carry it through). THE proof B0+B1 compose.
5. **ggdef full suite** (Rider 2) — the project's ggdef harness, all green.
6. **PARENT drives** the FULL **C** + FULL **LLVM** integration sweeps (the over-rejection gate, §5). Not the agent's job — the agent runs 1–5 + reports; the orchestrator runs the full sweeps before integrating.
7. `cargo test --test integration self_host -- --test-threads=4` (self-host lanes unregressed — B1 doesn't touch `.gg`, but the Rust check compiles them).

---

## 7. Docs write-through (mandatory)

`docs/language-reference.md §9.4 Same-Call Aliasing` (`:2328`) — STALE (lists only
`f(&x,&x)` / `f(x,&x)` / `f(&x,!x)`). Rewrite:
- Add the **`f(x, !x)`** arm (bare + move).
- State the **projection-overlap rule** (same root, one path a prefix of the other) **with
  the disjoint-sibling carve-out** (`f(&m.a, &m.b)` is fine).
- State the **Copy-read exemption** precisely (a bare read of a Copy-typed place is a value
  snapshot — not a live alias — so `f(&s, s.copy_int)` is legal; the existing "`f(x,x)` allowed
  for Copy types" line generalizes to this). **Note the two-axis boundary for moves:**
  `f(s.copy_int, !s)` (read BEFORE move) is legal, but `f(!s, s.copy_int)` (move THEN read) is
  rejected — not by aliasing but by **use-after-move** (a move consumes the storage; the later
  read is of a dead slot; Rust rejects the same). Keep this distinction explicit so a reader
  doesn't infer a blanket mover-Copy exemption.
- Cite **D10** (`decisions.md` ruling). The exclusivity principle is stated in
  **`docs/language-design.md` §3.5 The Borrow Rules** (`:580` — NOTE: that's
  language-DESIGN.md, not language-reference.md, whose `:580` is EBNF) and, within
  language-reference.md itself, near the §9.2 exclusivity region (`~:1755-1758`) — cross-reference
  the in-reference statement, don't duplicate.

Keep the framing consistent with `docs/language-design.md`'s exclusivity rationale (the
license-for-lazy-CoW argument — D10 exists to close the lazy/eager divergence channel).

---

## 8. Worktree + playbook preamble (non-negotiable — CLAUDE.md "Multi-agent")

Open the executor prompt with the standard preamble (verify `pwd` + `git rev-parse
--show-toplevel` inside the worktree; NEVER touch `/workspace/gorget` or `/workspace/gorget-1`;
no `/workspace/gorget/...` absolute paths — the worktree nests UNDER main). Plus:
`isolation: "worktree"`, `model: "opus"`; `git merge --ff-only gorget-1` on entry (or align to
main — CONFIRM the integration branch: recent tracks landed on **main**, gorget-1 is stale);
stage EXPLICITLY by file name (never `git add -a`/`.`/`commit -a`); NEVER `git stash` (save
with `git diff > /tmp/b1_<name>.patch`); checkpoint a durable patch to
`docs/plans/define-gorget/scouts/patches/b1-<part>.patch` after each part (Rust check / ggdef /
fixtures / docs). Run FINAL gates FOREGROUND with generous timeouts. On an Edit-tool desync,
re-Read + retry the Edit tool — never a shell heredoc with an absolute path.

---

## 9. Definition of done

- [ ] `check_call_aliasing` extended: place-keyed (root+projection via
      `find_root_def_id_with_path`), the `(Borrow,Move)` arm added, Copy-read exemption via
      the TYPED axis (`expr_value_is_copy`, no heuristics), disjoint siblings accepted,
      `(Move,Move)` projection interaction with E_DoubleMove resolved (no gap, no double-report).
- [ ] Messages name the actual places (`n` vs `n.field`), reuse `BorrowConflict`.
- [ ] **ggdef models D10(b)** as an elaboration rejection (mirror D10(a) `tests.rs:1603`) +
      both-direction fixtures per position; **the FULL ggdef suite is green** (Rider 2). The
      mover-Copy case `f(!x,x.copy_field)` must ALSO reject in ggdef (via its liveness/move rule)
      or the gap is FILED — ggdef must not accept a program production rejects (Core #8). If any
      part genuinely can't be modeled, state precisely which + why (owner ruling not silently downscoped).
- [ ] Production fixtures: NEG `f(x,!x)`, NEG `f(&n,&n.f)`, NEG `f(n.f,&n)`, POS `f(&m.a,&m.b)`,
      POS `f(&s,s.copy_int)` (writer-Copy exemption), **NEG `f(!x,x.copy_field)` pinned to the
      `E_UseAfterMove` diagnostic** (mover-Copy = liveness reject, per Rider 1 REVISION), **POS
      `f(s.copy_field,!x)`** (order-twin, read-before-move). POS build+run on BOTH backends; no
      fixture wires a buggy expected value.
- [ ] `self_host_bootstrap_fixed_point` GREEN (B0+B1 compose); self-host lanes unregressed.
- [ ] **FULL C + FULL LLVM integration sweeps GREEN** (parent-run — the over-rejection gate).
      Any newly-rejected site TRIAGED per §5 (hoist-if-safe / fix-if-bug / never-weaken),
      documented; none dodged.
- [ ] §9.4 docs rewritten (new arm + projection rule + disjoint carve-out + Copy exemption,
      cite D10).
- [ ] B1 is a standalone landing on TOP of B0; **B2 (self-host mirror) gates on B1 as the
      reference** and is NOT in this brief.
- [ ] No name-matching / shape-heuristic introduced (Copy axis is typed; place keying is the
      existing primitive). No self-host defensive fossil. Reference-grade: a program that
      should be rejected IS rejected in BOTH production backends AND ggdef (Core #8).

---

## 10. Non-goals (do NOT expand scope)
- **No self-host `.gg` changes** (B2).
- **No borrow-provenance bit** (owner DEFERRED (B)).
- **No value-position no-op-`&` work** (separate rider).
- **No out-of-repo hoists** (gorget-js/arena/gglox/gconf — deferred coordination round).
- Any NEW compiler gap the check surfaces → triage per §5 (fixture + sharp TODO if deferred),
  never a reshape to dodge it.
