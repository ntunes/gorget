# Wave A2-R2 brief — D12 riders: compound-assign ICE fix (M1) + position/shape-aware message (M2)

> **Wave position:** the two D12 riders split from A2-R1 (landed `b72ef446`). Zones are DISJOINT
> (M1 = `src/ir/lowering/stmts/assigns.rs`; M2 = `src/semantic/errors.rs` + `safety/`), so this is
> **ONE brief, TWO milestones, ONE executor sequential** — M1 first (prototyped GREEN, closes a HIGH
> ICE), then M2 (mechanical). Scout COMPLETE: `scouts/scout-a2-r2.md`; the M1 fix is prototyped +
> measured at `scouts/patches/a2-r2-ice-fix-prototype.patch`.
>
> **Grounded in:** `scouts/scout-a2-r2.md` (measured), `wave-a2-drop-purity-brief.md` pin-4 (the
> normative message spec), `decisions.md` D12/D4, `docs/book` ch.11 carve-out. All anchors re-verified
> on the current tip (line-drift from the stub corrected below).

## Owner-ruling / decided (from the scout's §4 — do NOT re-open)
- **M1 fix shape = BORROW-IN-PLACE** (not the TODO's move-out/move-back) — simpler, no move-out hole,
  matches the for-loop precedent (`for_loops.rs:494-499`), measured ASan-clean.
- **Message wording (M2)** per pin-4: Whole/SingleOwner → "`!x` to move or `x.clone()` to copy";
  Field/Index → "`obj.f.clone()` to copy (a bare `!obj.f` is a partial move and is rejected)";
  Capture → "pass it as an argument or wrap it in `Shared[T]`" (**NO `!`, NO `.clone()`**).
- **`move` written with `!` today** (current syntax) + a `# D27: !→^` breadcrumb — D27's C3 track
  re-sigils ALL messages/fixtures at once; do NOT pre-emptively use `^` (it does not parse yet).
- **Conformance compares the E_ CODE, not message text** (static-error tier, like traps) — so M2 is
  PRODUCTION-ONLY; ggdef's own `E_MoveWithoutOperator` message (which advertises `!{c}`/`.clone()` for
  captures) is a SEPARATE already-filed ggdef-gap — do NOT touch ggdef here.

## Milestone 1 — the compound-assign ICE fix (do FIRST; prototyped green)
**The bug:** `v[i] += x` on a drop-tainted (custom-Drop) element ICEs: `gg check` passes, `gg build`
PANICS at `src/ir/lowering/mod.rs:1763` ("shallow copy of resource _N : Acc") — the resource-move
validator rejects the shallow Copy. A2-R1's taint check does NOT mask it (correct — ggdef accepts
owned-local collection-element compound writes).

**Root cause (scout-traced):** the Index compound arm (`stmts/assigns.rs:1496`) reads the element at
`:1606` via `builder.index_load` (`ReadMode::Clone`) → an owned clone; the operator-overload branch
(`:1713`) then `builder.assign(Place::local(cur_local), cur_val)` (`:1716`) shallow-COPIES that owned
resource clone into a fresh local to borrow for `self` (`builder.assign` defaults to `AssignMode::Copy`,
`builder.rs:236`) — the resource shallow-copy the validator ICEs on.

**The fix (apply the durable prototype `scouts/patches/a2-r2-ice-fix-prototype.patch` as the starting
point, +41/−13, one file — re-derive/verify against the current tip, don't blind-apply):**
- Vector/dict read (`assigns.rs:~1606`): for a RESOURCE (non-string) element, use `index_load_borrow`
  (`builder.rs:258`, emits `IndexLoad{read:Borrow}`) into a `Ptr(elem)` aliasing the element in place,
  instead of `index_load` (Clone). Set a `cur_is_borrow` flag. **Strings keep Clone** (concat
  consumes+drops the owned old value); **primitives keep Clone** (Copy — no ICE).
- Op-overload branch (`assigns.rs:~1713`): when `cur_is_borrow`, pass `cur_val` (the in-place Ptr)
  straight as the `self` receiver — NO `builder.assign` shallow copy. Non-resource elements keep the
  legacy borrow-of-copy path.
- Write-back `__set` (vector) / `__put` (dict) pre-drops the old element then stores the fresh `result`
  → **drop-once**. Mirrors `validate.rs:1263-1271`'s own note (CoW routes resource elements through
  `Ptr(T)` zero-copy borrow) and the for-loop precedent.
- **⚠ R1 — the borrow-in-place `self`-Ptr aliasing window (MEASURE + resolve; do NOT ship untested).**
  The prototype takes the `index_load_borrow` of `v[idx]` at `assigns.rs:~1606-1608` and then lowers the
  RHS at `~:1650` and dereferences the Ptr inside `add` at `~:1713-1720`. **If the RHS mutates/reallocs
  the SAME collection** (`v[0] += grow(&v)` where `grow` pushes to `v`), the backing buffer moves and
  the `self` Ptr DANGLES → UAF — a hazard the old Clone path was immune to (it materialized an owned
  value before RHS lowering). This is the same class as for-loop iterator invalidation and is exactly
  the D10(b) same-call place-overlap that is FILED-but-unimplemented (TODO `:279`, Batch B). **The
  executor MUST:** (1) add a probe fixture `v[0] += <expr that reallocs v>` **AND its dict sibling
  `d[k] += <expr that reallocs d>`** (dict realloc-on-insert is the same hazard class) to the M1 gate
  set and MEASURE the disposition on both backends + ASan; (2) **resolve, in this order of preference:** (a) if
  the current borrow-checker/exclusivity ALREADY rejects it (post-A3), the hazard is unreachable →
  keep the fix, DOWNGRADE the claim to "rejected at check, no runtime window", cite the rejection; (b)
  else CLOSE the window by lowering the RHS into an owned temp BEFORE taking the `v[idx]` borrow
  (reorder — the borrow is then taken post-realloc), and re-confirm the ~20 compound fixtures stay
  byte-identical + drop-once holds; (c) ONLY if (a)/(b) are both infeasible, FILE it as a D10(b) sibling
  with the fixture (per "don't redesign around gaps") and NOTE the ICE→UAF trade explicitly — do NOT
  leave a silent UAF unacknowledged. Report which of (a)/(b)/(c) you took + the measured evidence.

**M1 gates (measured green in the prototype — re-confirm):** a NEW tainted-compound integration
fixture — **use a CALL-shaped RHS (`v[0] += mk(5)`), NOT `Acc(5)`**, to keep the ASan gate honest
(see the caveat below) — asserting drop-once output on BOTH backends + ASan-clean; ALL ~20 non-tainted
compound fixtures BYTE-IDENTICAL both backends (`ls tests/fixtures | grep compound`); `drop`/`index`/
`operator_overload` suites green; `cargo test --lib` green (the scout's M1-only prototype measured
1105/0 — expect the count to GROW once M2's per-shape + capture-no-`!` unit tests land, esp. if M1+M2
ship in one commit; assert green + the new tests present, NOT a fixed number).

**M1 scope/close-out:**
- Close TODO entries **line 290** (🐛💥 compound-assign resource-element ICE, HIGH) + **line 326** (LOW,
  same bug other framing). **Do NOT close the D12 parent** (A2-S remains).
- **Do NOT claim to fix the sibling panics `assigns.rs:1129`/`:1775`** — the scout verified those are a
  DIFFERENT class (no-setter defensive hard-asserts, "typecheck/lowering disagree"); this fix does not
  touch them. Leave their TODO as-is.
- **⚠ R2 — the CUSTOM-INDEXABLE resource-element ICE sibling (confirm-or-file).** The op-overload branch
  at `assigns.rs:~1713-1720` is shared by ALL routes. The custom-container `__get` path (`~:1611-1646`)
  sets `cur_is_borrow = false` and returns an OWNED value; so a custom `Index`-equipped type with a
  resource (custom-Drop) element AND an `add` overload → `custom[i] += x` still reaches
  `builder.assign(cur_local, cur_val)` → the SAME shallow-copy-of-resource ICE at `mod.rs:1763`. M1's
  borrow-in-place is scoped to vector/dict (matching the filed bug). The executor MUST state whether
  this custom-indexable combination is UNREACHABLE/typecheck-rejected (confirm with a probe), or FILE
  it as a Core-#4 sibling TODO — do NOT leave it undocumented.
- **FILE a NEW HIGH TODO (caveat A — do NOT chase it in M1):** a SEPARATE, pre-existing, orthogonal
  leak — the operator-overload resource-ARGUMENT temp leaks (~64 bytes); **plain `a + heap_temp` (no
  compound assign, untouched code) leaks too** (scout repro `plain_binop.gg`). Root = overload-arg
  drop-registration, a different subsystem. File with a plain-`+` ASan fixture. This is why the M1
  fixture uses a call-shaped RHS (exercises the ICE fully, ASan-clean, doesn't trip the unrelated bug).

## Milestone 2 — position/shape-aware `E_MoveWithoutOperator` message (mechanical)
**The variant** `MoveWithoutOperator { name: String }` at `src/semantic/errors.rs:451` (stub said :438
— drifted). Add a **reason** axis (drop-taint vs single-owner) + a **shape/remedy** axis
(`Whole` / `FieldIndex` / `Capture`), rendered under the SAME `E_MoveWithoutOperator` code. **The shape
is already available at every site** — `expr_is_place` (`src/semantic/safety/helpers.rs:19`)
distinguishes it structurally from the `Expr` in hand; NO new dataflow.

- **Update the 10 production construction sites** (scout-enumerated — re-grep `MoveWithoutOperator`
  across `src/semantic/` to confirm post-A2-R1): `check_stmt.rs:1444,1483,835,1796`;
  `check_expr.rs:32,43,600,972,995`; `helpers.rs:919`. For EACH, pass the correct reason + shape.
- **Display arm `errors.rs:989`** (stub said :965): render the per-shape message (wording above);
  **REMOVE the dead `` `move` `` alternative** (parser-dead — only consumed-and-discarded at
  `stmt.rs:701`). Plus the test constructor `MoveWithoutOperator { name: "x".into() }` at
  **`errors.rs:1241`** (the brief's earlier `:1217` was WRONG — that's the `WrongFieldCount` Display arm;
  the constructor drifted to `:1241`) + the `safety/tests.rs` matchers (`:80`/`:1552`/`:1573`).
- **⚠ Load-bearing gotcha the stub missed:** `tests/integration.rs:26517` asserts the exact text
  `"non-Copy type requires ! or move"` — removing `move` BREAKS it; update it in the same PR.
- **GATE (M2's reference-grade check):** a unit test asserting the rendered CAPTURE-position message
  contains no `'!'` character (pin-4 forbids `!`/`.clone()` for captures).
- Sub-place text: `tainted_place_name` returns the ROOT (`hh` for `hh.r`), so a Field remedy renders
  `hh.clone()` not `hh.r.clone()` — ACCEPTABLE for A2-R2 (the GATE doesn't require it); FILE a LOW
  polish follow-up to thread the place span for the exact sub-place text.
- **R5 — refresh the stale message-text comment** at `tests/fixtures/cow_struct_bare_assign.gg:3-4`
  (quotes the OLD "requires `!` or `move` to transfer") when `move` is removed, so it isn't a stale
  historical record (self-host-elegance rule). Add it to the M2 touch list.

## Gate battery (run FOREGROUND; CHUNKED-FOREGROUND any >600s gate by test name — do NOT background-then-end, rule 9)
```
cargo build
cargo test --lib 2>&1 | tee /tmp/a2r2_lib_$$.log                                    # green (count > 1105 — M2 ADDS the per-shape + capture-no-! GATE unit tests); assert green + the new tests present, NOT a fixed number
cargo test --test lints 2>&1 | tee /tmp/a2r2_lints_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration compound -- --nocapture 2>&1 | tee /tmp/a2r2_compound_$$.log   # ~20 byte-identical + the new tainted fixture, both backends
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration drop -- --nocapture 2>&1 | tee /tmp/a2r2_drop_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration operator_overload -- --nocapture
# M2 message: the safety tests + the updated integration.rs:26517 assertion + the d12/move_without_operator fixtures
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration move_without_operator -- --nocapture
# ASan on the new tainted-compound fixture (drop-once, no leak) — see scripts/ for the ASan build flag
```
Also run the LLVM lane on the compound + tainted fixtures (`GG_BACKEND=llvm`) to confirm byte-identical
C↔LLVM. Do NOT run the full `cargo test --test integration` (parent's job).
Acceptance: builds; `--lib` green incl. the capture-no-`!` GATE + the per-shape message tests; the new
tainted-compound fixture builds+runs drop-once on BOTH backends, ASan-clean; all ~20 non-tainted
compound fixtures byte-identical; `integration.rs:26517` updated + green; lints green; TODO 290+326
closed, the D12 parent + sibling-panic entries left, the orthogonal op-overload arg leak filed HIGH.

## Scope fences
- M1 touches ONLY `src/ir/lowering/stmts/assigns.rs` + a new `tests/fixtures/*.gg` + `TODO.md`.
- M2 touches ONLY `src/semantic/errors.rs` + `src/semantic/safety/*` + `tests/integration.rs:26517` +
  the message fixtures.
- Do NOT touch: the sibling-panic hard-asserts (`assigns.rs:1129/1775`), the D12 parent entry, ggdef's
  message (separate filed gap), D27's `^` (does not parse yet), A2-S's self-host zone.

## Worktree & agent discipline (NON-NEGOTIABLE — CLAUDE.md multi-agent + this wave's playbook)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree; STOP if either is
`/workspace/gorget` or `/workspace/gorget-1`. Paths RELATIVE to your worktree; on Edit desync re-Read +
retry the Edit tool (NEVER a heredoc with an absolute path); after any non-Edit write `git -C
/workspace/gorget status` and STOP if it shows changes. Entry: `git merge --ff-only gorget-1 2>/dev/null
|| true`. **Checkpoint to `docs/plans/define-gorget/scouts/patches/` (DURABLE, not /tmp-only) after each
milestone.** **Run FINAL gates FOREGROUND, CHUNKED by test name for anything >600s — do NOT background a
long run then end (rule 9 stalled 5 agents this wave; the SendMessage nudge-resume cures a stall).**
Stage ONLY exact files by name; NEVER `git add -a`/`.`/`commit -a`; NEVER `git stash`. Commit on your
worktree branch (M1 and M2 may be one commit or two), message ending:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
Per milestone: files+file:line + one-line what. PASTED gate output — the tainted-compound fixture
drop-once on BOTH backends + ASan-clean; the ~20 compound fixtures byte-identical; the capture-no-`!`
GATE result + the per-shape messages; `integration.rs:26517` updated. The NEW HIGH TODO you filed (the
orthogonal op-overload arg leak, plain-`+` ASan fixture) + the LOW sub-place polish follow-up. Which
TODO entries you closed (290, 326) and which you LEFT (D12 parent, sibling panics). Branch + commit hash.
