# Scout report — Wave A2-R2 (D12 riders: position/shape-aware message + compound-assign ICE)

**Scout:** read-only verification + end-to-end measurement. No production code committed.
**Base:** `1ccd181d` (A2-R1 landed at `b72ef446`; `git merge --ff-only gorget-1` = up to date).
**Status: COMPLETE.** ICE reproduced AND fix prototyped end-to-end (measured). Item-1 premises
all re-verified against CURRENT source. All anchors in the v0 stub CONFIRMED (with line drift noted).

---

## Section 1 — Item 1: position/shape-aware `E_MoveWithoutOperator` message

### 1.1 Variant shape (CONFIRMED, line drifted)

`MoveWithoutOperator` is at **`src/semantic/errors.rs:451`** (stub said `:438` — drifted +13 by A2-R1),
currently `MoveWithoutOperator { name: String }`. To render pin-4 it needs, in addition to `name`:

- a **reason** axis — drop-taint (custom `Drop` → single-owner-by-D4) vs the pre-existing
  single-owner-by-design carve-out (Box/Task/Owned/closure/Guard). Controls the "why" clause.
- a **remedy/shape** axis — Whole-identifier place vs Field/Index sub-place vs Closure-capture.
  This is the load-bearing axis for pin-4's GATE (which remedies are valid).

The shape info **IS available at every site**: the tainted-place sites all hold `e: &Spanned<Expr>`,
and `expr_is_place` (`helpers.rs:19`) already distinguishes the three shapes structurally
(`Identifier|SelfExpr` = Whole; `FieldAccess|TupleFieldAccess` = Field; `Index` = Index). The
capture site iterates `capture_set.captures` (always the Capture shape). The single-owner sites are
always Whole (identifier-local). So NO new dataflow is needed — the shape is a pure function of the
expr already in hand. `tainted_place_name` returns only `String` today (loses the shape); the fix is
either to return the shape alongside the name, or compute it at the call site from the expr.

### 1.2 Construction sites — ENUMERATED (10 production + 1 test), post-A2-R1

Grep `MoveWithoutOperator` across `src/semantic/`. The stub's "8-9" is **10 production construction
sites** (+ 1 test constructor in `errors.rs`). For each: position (D4/D12), shape, reason, remedy:

| # | Site | D4 position | Shape at site | Reason | Correct remedy (pin-4) |
|---|------|-------------|---------------|--------|------------------------|
| 1 | `check_stmt.rs:1444` | 1 bind | ANY (via `tainted_place_name`) | drop-taint | Whole→`!x`/`.clone()`; Field/Index→`.clone()` only |
| 2 | `check_stmt.rs:1483` | 1 bind | Whole (identifier-local) | single-owner | `!x` / `.clone()` |
| 3 | `check_expr.rs:32` | 2 ctor/field-init | ANY (via `tainted_place_name`) | drop-taint | shape-dependent |
| 4 | `check_expr.rs:43` | 2 ctor/field-init | Whole (identifier-local) | single-owner | `!x` / `.clone()` |
| 5 | `check_expr.rs:600` | 3 collection-put | ANY (via `tainted_place_name`) | drop-taint | shape-dependent (Whole→`!arg`/`.clone()`) |
| 6 | `check_stmt.rs:835` | 4 return | ANY (via `tainted_place_name`) | drop-taint | shape-dependent (`return !place`/`.clone()`) |
| 7 | `check_stmt.rs:1796` | 4 expr-body tail | ANY (via `tainted_place_name`) | drop-taint | shape-dependent |
| 8 | `check_expr.rs:995` | 4 closure expr-tail | ANY (via `tainted_place_name`), param-rooted | drop-taint | shape-dependent |
| 9 | `check_expr.rs:972` | 5 closure capture | Capture | drop-taint | **PASS-AS-ARG / `Shared[T]` ONLY — no `!`, no `.clone()`** |
| 10| `helpers.rs:919` | 6 materialize-on-write | Whole (bare borrow param) | drop-taint | `!x` / `.clone()` |
| T | `errors.rs:1241` (test) | — | — | — | test constructor — must update to new fields |

Sites that route through `tainted_place_name` (#1,3,5,6,7,8) can carry ANY shape (Whole OR
Field OR Index) — these are exactly the sites where the current generic message is WRONG for
field/index places (it would advertise `!` which is a partial move → `E_UseAfterMove`).

### 1.3 Display arm + dead `move` alternative (CONFIRMED)

Display arm at **`errors.rs:989`** (stub said `:965` — drifted). Text:
`"cannot copy `{name}`: non-Copy type requires `!` or `move` to transfer"`.
The `` `move` `` alternative IS present and IS dead syntax: `move` lexes as a keyword
(`lexer/token.rs:323,442,532`) but there is NO move-expression parser rule for it — the move
operator is `!` (`parser/expr.rs:568-578`, `Expr::Move`). The only parser reference to
`Keyword::Move` is `parser/stmt.rs:701`, where it is consumed-and-discarded in the local-`&`-bind
sigil check (it never produces a move). So `x = move y` does not parse as a move. **Remove `move`.**

`code()` arm at `errors.rs:713` (`=> "E_MoveWithoutOperator"`) is field-agnostic (`{ .. }`) — SURVIVES.

### 1.4 Matchers/assertions that pin the message (must update)

- `errors.rs:1241` — `MoveWithoutOperator { name: "x".into() }` (test constructor) → add new fields.
- `safety/tests.rs:80` — `{ .. }` → survives.
- `safety/tests.rs:1552` — `{ name } if name == "g"` → change to `{ name, .. }`.
- `safety/tests.rs:1573` — `{ .. }` → survives.
- **`tests/integration.rs:26517`** — `check_gg_fails("move_without_operator_error.gg", "non-Copy type requires `!` or `move`")`
  — a TEXT assertion. Removing `move` from the message **breaks this**; update the expected substring
  (e.g. to `"non-Copy type"` or the new whole-place wording). LOAD-BEARING.
- `tests/fixtures/cow_struct_bare_assign.gg:4` — a comment quoting the old text (not an assertion;
  optional prose refresh).

### 1.5 The normative model message (ggdef) — the text to mirror

ggdef already renders shape/position-aware messages (`spec/ggdef/src/elaborate/mod.rs`):
- general place (pos 1-4,6), `:589`: `"E_MoveWithoutOperator: implicit copy of a drop-tainted value
  at {position}; a type with a custom `Drop` is single-owner — write `!<src>` to move or
  `<src>.clone()` to copy"`
- capture (pos 5), `:1878`: `"...closure captures the drop-tainted local `{c}` by value; ... — capture
  `!{c}` to move or `{c}.clone()` to copy"`.  ⚠ **ggdef's capture message CONTAINS `!` and even
  `.clone()`** — pin-4 says production's capture message must contain NEITHER (capture-list syntax
  D5/D7 is unbuilt; `.clone()`-into-local is equally tainted → does not compile). This is a
  production-vs-ggdef message DIVERGENCE the brief must call out (ggdef's own message advertises
  `!{c}` capture syntax it does not implement — the A2-R1 brief already flags this as a ggdef-gap
  work item). Production should NOT copy ggdef's capture text verbatim.
- closure-tail (pos 4), `:1908`: `"...implicit copy of the drop-tainted place `{root}` at
  closure-tail; ... — write `!{root}` to move or `{root}.clone()` to copy"`.

ggdef tests assert on substrings (`contains("E_MoveWithoutOperator")`, `contains("materialize-on-write")`
— `tests.rs:1009,1101`), NOT full text, so ggdef parity is not text-locked; production probe fixtures
assert the E_ code, not text (per A2-R1). The GATE ("capture message has no `!`") is the one text
constraint.

### 1.6 Proposed message design

Add to the variant (one clean option — a single enum carrying position + shape + reason):

```rust
MoveWithoutOperator { name: String, ctx: MoveCtx }

enum MoveCtx {
    /// Whole-identifier / self / param place, drop-tainted. `!x` or `x.clone()`.
    DropWhole { position: MovePos },
    /// Field / index sub-place, drop-tainted. `.clone()` ONLY (`!obj.f` is a
    /// partial move → E_UseAfterMove).
    DropSubPlace { position: MovePos },
    /// Closure captures a drop-tainted local by value. Pass it as an argument
    /// or wrap in `Shared[T]`. NO `!`, NO `.clone()`.
    DropCapture,
    /// Single-owner-by-design type (Box/Task/Owned/closure/Guard). `!x` / `x.clone()`.
    SingleOwner,
}
enum MovePos { Bind, CtorFieldInit, CollectionPut, Return, ExprBodyTail, ClosureTail, MaterializeOnWrite }
```

Display renders (matching ggdef's cause phrasing, minus the capture divergence):
- `DropWhole{Return}` → `` cannot move `x` out of a `return` without an operator: `x` is a resource
  (custom `Drop`), so a bare return would be an implicit copy — write `return !x` to move or
  `x.clone()` to copy ``
- `DropSubPlace{Bind}` → `` ...`obj.f` is a resource sub-place — write `obj.f.clone()` to copy (a
  bare `!obj.f` is a partial move and is rejected) `` (NO `!`)
- `DropCapture` → `` closure captures the resource `hh` by value — pass it as an argument or wrap it
  in `Shared[T]` `` (NO `!`, NO `.clone()`)
- `SingleOwner` → `` `g` is a single-owner type (no implicit copy) — write `!g` to move or
  `g.clone()` to copy ``

**GATE (must be an executable test):** render `DropCapture` and assert the string contains no `'!'`.
Add to `errors.rs` unit tests: build each `MoveCtx` variant, assert `.to_string()` (a) contains the
right remedy tokens and (b) for `DropCapture`, `!"!".contains`. This converts pin-4's GATE into a guard.

**Display-name refinement (design question, see §4):** `tainted_place_name` returns the ROOT name
(`hh` for `hh.r`), so a Field remedy would render `hh.clone()` not `hh.r.clone()`. To render the full
sub-place text, thread the place span and slice the source (or have `lvalue_value_type`/a place-printer
return the full path). Not required by the GATE; a quality improvement.

---

## Section 2 — Item 2: the compound-assign ICE — VERIFIED, REPRODUCED, FIX PROTOTYPED (measured)

### 2.1 Anchor re-verification (all CONFIRMED on current tip)

| Stub anchor | Current source | Status |
|-------------|----------------|--------|
| `lower_compound_assign` = `stmts/assigns.rs:1148` | `src/ir/lowering/stmts/assigns.rs:1148` | ✅ exact |
| `index_load_borrow` = `builder.rs:258` | `src/ir/builder.rs:258` (emits `IndexLoad{read:Borrow}`) | ✅ exact |
| Live panic = `mod.rs:1763` ("shallow copy of resource") | `src/ir/lowering/mod.rs:1763` (`findings.assign` fatal gate) | ✅ exact |
| Op-overload "siblings" = `assigns.rs:1129` and `:1775` | Both exist | ⚠ see 2.5 — these are the **no-setter** hard-asserts, a DIFFERENT class |

The Index compound arm is `assigns.rs:1496`. The **actual ICE mechanism**: the vector/dict read at
`assigns.rs:1608` uses `builder.index_load` (`ReadMode::Clone`) → `cur_val` is an owned clone of the
element. In the operator-overload branch (`:1713`), `builder.assign(Place::local(cur_local), cur_val)`
(`:1716`) shallow-COPIES that owned resource clone into a fresh local to borrow it for `self` —
`builder.assign` defaults to `AssignMode::Copy` (`builder.rs:236`). A shallow Copy of a resource
value is what the resource-move validator (`validate_resource_sites_all` → `findings.assign`) rejects
fatally at `mod.rs:1763`.

### 2.2 The ICE REPRODUCED (exact command + panic)

`/tmp/a2r2_scout/ice_repro.gg` — `struct Acc{int total}` with a custom `Drop` AND an `Add[Acc]`
overload, held in `Vector[Acc]`, then `v[0] += Acc(5)`:

```
$ target/debug/gg check /tmp/a2r2_scout/ice_repro.gg
OK: no semantic errors                                   # gg check PASSES (A2-R1 does NOT mask it — correct)
$ target/debug/gg build /tmp/a2r2_scout/ice_repro.gg -o /tmp/a2r2_scout/ice_repro
[resource-moves] 1 violation(s):
  @main::bb0::i18 — shallow copy of resource _16 : Acc
thread 'gg-main' panicked at src/ir/lowering/mod.rs:1763:13:
GIR module failed resource-move validation (1 violation(s))
```

Matches the TODO entry verbatim (`mod.rs:1759` in the stale entry → `:1763` current; `_9 : Acc` → `_16 : Acc`, just local renumber). Confirms `gg check` passes / `gg build` panics / A2-R1 taint does NOT mask (ggdef accepts owned-local collection-element compound writes — verified: the compound line itself is accepted).

### 2.3 The FIX prototyped (durable: `scouts/patches/a2-r2-ice-fix-prototype.patch`, 41+/13-, one file)

**Design (cleaner than the TODO's "move-out/move-back" suggestion): read the resource element by
BORROW, not Clone.** `add`/`sub`/… take `self` by read-only borrow, so an owned clone is pure waste
AND is the source of the shallow-copy ICE. Concretely, in `assigns.rs`:
- Vector/dict read (`:1606`): for a resource (non-string) element, use `index_load_borrow` into a
  `Ptr(elem)` (aliasing the element in place) instead of `index_load` (Clone). Flag `cur_is_borrow`.
  Strings keep Clone (concat consumes+drops the owned old value); primitives keep Clone (Copy).
- Op-overload branch (`:1713`): when `cur_is_borrow`, pass `cur_val` (the in-place Ptr) straight as
  the `self` receiver — no `builder.assign` shallow copy. Non-resource elements keep the legacy
  borrow-of-copy path.
- Write-back `__set` (vector) / `__put` (dict) pre-drops the old element and stores the fresh
  `result` → **drop-once**. No aliasing hazard: `add` returns before `__set` mutably borrows.

Mirrors the existing for-loop precedent (`for_loops.rs:494-499`: `register_ptr_type` +
`index_load_borrow` for resource elements) and the validator's own note (`validate.rs:1263-1271`:
"CoW routes resource-typed elements through Ptr(T) zero-copy borrow … for-loop iteration emits
index_load_borrow for resource elements").

### 2.4 MEASURED results (after `cargo build`, clean)

**ICE gone + drop-once, BOTH backends** (`v[0] += Acc(5)`, C and LLVM identical):
```
drop Acc 1      # old element dropped by __set pre-drop
6               # v[0].total == 6
drop Acc 6      # new v[0] dropped at scope end
drop Acc 2      # v[1] dropped at scope end
```
**Dict `d["a"] += mk(5)`**: same — no ICE, drop-once, ASan-clean.

**ASan/LSan (heap-owning `Acc{int total; Vector[int] buf}`, call-shaped RHS `mk(5)`):**
- `v[0] += mk(5)` (`heap_idx.gg`): **ASan-CLEAN** — all four `Acc`s drop exactly once, no leak, no double-free.
- `d["a"] += mk(5)` (`dict_res.gg`): **ASan-CLEAN**, drop-once.

**Regression gates (all GREEN):**
- `cargo test --lib` → **1105 passed / 0 failed**.
- `cargo test --test integration compound` → **C 20/0 · LLVM 20/0** (all non-tainted compound fixtures byte-identical).
- `operator_overload` → C 1/0 · LLVM 1/0. `drop` → C 40/0. `index` → C 48/0.
- Non-resource elements (e.g. `Vector[Vec2]` in `operator_overload_compound.gg`) take the unchanged
  Clone path (`is_resource_type(Vec2)`==false) → byte-identical, confirmed by the 20/0.

### 2.5 HONEST CAVEATS — two things the executor MUST know

**(A) A SEPARATE, ORTHOGONAL, PRE-EXISTING leak — the operator-overload resource-ARGUMENT leak.**
When the RHS operand of an operator-overload is a heap-owning resource TEMP, its drop is missed:
- `Acc c = a + Acc(5, one(5))` — **plain binary `+`, no compound assign, does not touch my code** —
  **leaks 64 bytes** (LSan: the temp's `buf` allocated in `one()`, never freed).
- `a += mk(5)` (identifier compound) — **leaks 64 bytes** (pre-existing; the identifier arm at
  `:1332` is untouched by this fix).
- `v[0] += Acc(5, one(5))` (index compound, INLINE-ctor RHS) — leaks 64 bytes (same class: the RHS
  operand temp, not the element).
This is a GENERAL operator-overload argument drop-registration bug (the borrowed resource arg to
`add` is never drop-registered after the call), fully orthogonal to the compound-assign ICE and NOT
introduced by this fix (it reproduces on plain `a + b`). **FILE as a new HIGH TODO with a plain-`+`
ASan fixture.** It is why the ICE fixture below uses a call-shaped RHS (`mk(5)`), which is ASan-clean
on the index arm — that is NOT dodging the ICE (it fully exercises `v[i] += resource`), it avoids an
unrelated bug, per "don't redesign around compiler gaps" the unrelated bug is FILED not buried.
  - Micro-note: with a call-shaped RHS the index arm DROPS the temp correctly (`v[0]+=mk(5)` clean),
    but the identifier arm leaks it even then — so the identifier arm has an ADDITIONAL old-value +
    RHS-temp leak the index arm does not. All pre-existing; all belong to the filed follow-up.

**(B) The stub's "sibling panics `:1129`/`:1775` are a DIFFERENT class.** `assigns.rs:1129`
(plain index-assign) and `:1775` (compound index-assign) are `panic!("BUG: … found no setter …
typecheck accepted an index-assign the lowering cannot dispatch")` — defensive **no-setter**
hard-asserts for a custom-indexable type whose typecheck/lowering disagree. They are NOT the
shallow-copy ICE and this fix does not touch them; they are not known to be user-triggerable (would
need a custom `Index`-equipped type accepted by typecheck but missing `__set`). Do NOT claim this fix
closes them. The TODO entries this fix DOES close: **line 290 (HIGH, 🐛💥 `v[i]+=x`/`d[k]+=x`
resource-element ICE)** and **line 326 (LOW, "Op-overload compound-assign resource-move validator
panic", `assigns.rs:1665` old-numbering)** — these two are the SAME shallow-copy bug from the
validator-site and lowering-site angles. Do NOT close the D12 parent (line 271) — A2-S remains.

---

## Section 3 — Recommended slicing (one brief vs two) + size/risk

**Zones are genuinely DISJOINT:**
- Item 1 (message): `src/semantic/errors.rs` (variant + Display + code + test) · `src/semantic/safety/`
  (10 sites, tests) · `tests/integration.rs:26517` (text assertion) · optionally `spec/ggdef/…` message.
- Item 2 (ICE): `src/ir/lowering/stmts/assigns.rs` ONLY + 1 new fixture + TODO close (lines 290, 326).
No file overlap. (Both conceptually concern `E_MoveWithoutOperator`, but Item 2 emits no diagnostic —
it is pure lowering.)

**Sizes/risk:**
- **Item 2**: SMALL (~15 lowering lines — prototyped GREEN here, one file, +41/-13), LOW risk, closes a
  HIGH ICE. Essentially executor-ready: apply the durable patch, add the fixture, close TODO 290+326,
  file the orthogonal leak (caveat A), run gates.
- **Item 1**: MEDIUM surface but MECHANICAL — 1 variant + 1 enum, 10 construction sites, 1 Display
  arm, ~4 test matchers, 1 integration text assertion, + a GATE unit test. No new dataflow (shape is
  a pure function of the expr already in hand). LOW-MEDIUM risk.

**RECOMMENDATION: ONE A2-R2 brief, two milestones (M1 = ICE fix FIRST, M2 = message), one executor,
sequential.** Rationale: (1) two full gauntlets (2×[brief+≥3 reviews+executor+output-review]) cost
more orchestration than one covering two disjoint, moderate, low-risk milestones; (2) M1 is
prototyped-green and tiny — lead with it to close the HIGH ICE immediately; (3) M2 is mechanical.
**Split into two parallel briefs ONLY if** the orchestrator wants two agents running at once — the
disjoint zones make that safe (brief each on the other's zone per multi-agent rule 5). Either way the
gauntlet (scout✔ → brief → ≥3 fresh reviews → worktree executor → fresh output-review) still applies;
this scout supplies M1's premises + measured prototype and M2's full site enumeration.

---

## Section 4 — Owner design questions (with recommendations)

1. **ICE fix shape — borrow-in-place vs the TODO's "move-out/move-back".** TODO line 290 suggested
   moving the dead element out, `add`, move result back. The prototype instead BORROWS the element
   in place (read-only `self`) and lets `__set`/`__put` pre-drop the old value. Borrow is simpler
   (no move-out hole, no move-back), matches the for-loop precedent, and measured ASan-clean.
   **REC: adopt borrow-in-place.** (Ask only if the owner prefers the move-shape for a reason.)

2. **The orthogonal operator-overload resource-ARG leak (caveat A) — fix now or file?** It is a
   general `a + heap_temp` leak, not compound-specific, root-caused in overload-arg drop-registration
   (a different site than this fix). **REC: FILE as a new HIGH TODO with a plain-`+` ASan fixture;
   do NOT expand A2-R2's ICE milestone to chase it** (scope creep into a different subsystem). The
   ICE fixture uses a call-shaped RHS so its ASan gate is honest and green today. Flag to owner
   because Core-#8 says a known defect must be recorded, not buried — it will be, in TODO.

3. **Message wording per shape (Item 1) — exact text.** REC (grounded in ggdef `:589`/`:1878`/`:1908`
   + book/11 carve-out): Whole/SingleOwner → "`!x` to move or `x.clone()` to copy"; Field/Index →
   "`obj.f.clone()` to copy (a bare `!obj.f` is a partial move and is rejected)"; Capture → "pass it
   as an argument or wrap it in `Shared[T]`" (NO `!`, NO `.clone()`). **Do NOT copy ggdef's capture
   message verbatim — ggdef's advertises `!{c}`/`.clone()` which pin-4 forbids for production** (the
   A2-R1 brief already files ggdef's `!{c}` as a ggdef-gap). GATE = a unit test asserting the rendered
   capture message contains no `'!'`.

4. **Dead `move` alternative + D27's `^`.** The `move` keyword is parser-dead (only consumed-and-
   discarded at `stmt.rs:701`); remove it from the message now. D27 will re-glyph move `!`→`^` later;
   **REC: write the new message with `!` today (current syntax), leave a `# D27: !→^` breadcrumb, and
   let D27's track re-sigil ALL messages/fixtures at once** — do not pre-emptively use `^` (it does
   not parse yet). Update the load-bearing `tests/integration.rs:26517` text assertion in the same PR.

5. **Full sub-place text in the message.** `tainted_place_name` returns the ROOT (`hh` for `hh.r`),
   so a Field remedy renders `hh.clone()` not `hh.r.clone()`. **REC: acceptable for A2-R2 (the GATE
   doesn't require it); optionally thread the place span to slice the source for the exact sub-place
   text — file as a LOW polish follow-up if not done inline.**

---

## Appendix — repro/prototype artifacts (durable + /tmp)
- Prototype patch (durable): `docs/plans/define-gorget/scouts/patches/a2-r2-ice-fix-prototype.patch`
- Repros (/tmp/a2r2_scout/): `ice_repro.gg` (the ICE), `heap_idx.gg`/`dict_res.gg` (ASan-clean drop-once),
  `heap_idx_inline.gg`/`heap_id.gg`/`plain_binop.gg` (the orthogonal pre-existing operator-overload
  arg leak — the last one has NO compound assign).
- Prototype is UNCOMMITTED in this worktree (scout discipline); executor re-derives from the patch.
