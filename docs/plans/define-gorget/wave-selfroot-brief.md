# Wave self-root brief — close the D10(b) production↔ggdef self-root divergence

> **Owner-prioritized 2026-07-14** as the next B-item (after B1 landed, before B2). Closes the
> last D10(b) divergence: production ACCEPTS `g(&self.a, &self.a.b)` (two writers into
> overlapping `self`-rooted places) while ggdef REJECTS it — a self-rooted twin of the
> `f(&n, &n.field)` exclusivity violation B1 fixed for identifier roots.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-selfroot.md` (measured: repro live, root cause
> a write-site omission, double-diagnose risk MEASURED not-real, prototype green). Prototype:
> `scouts/patches/selfroot-proto.patch`. **Ruling:** D10 (`decisions.md` ~`:654` root+projection
> keying) + the 2026-07-12 ADDENDUM.
>
> **Status:** v1 — **pass-1 (Opus, fresh) folded.** Pass-1 SIGNED OFF, independently
> re-verified all 7 load-bearing claims (root cause; `self` is a real DefId literally named
> "self" via `make_self_param`, so `scopes.lookup("self")` resolves and — since `self` is a
> keyword — cannot hit a wrong binding; layering-correctness; double-diagnose delegation;
> for-loop nuance pre-existing+consistent; full-sweep the right net), and CHASED DOWN the
> flagged big risk (item 4): all `find_root_def_id` callers are in the safety layer + every
> non-safety `resolution_map` consumer is guarded/inert → the change is structurally confined.
> **2 LOW non-blocking notes FOLDED:** (A) added that structural-confinement argument to §2;
> (B) noted the closure-nested-self case is a pre-existing under-check, not a fix failure.
> Awaiting pass-2 (fresh, confirming).

---

## 0. The gap + root cause (scout-verified, file:line)

`check_call_aliasing` (B1) keys places on `find_root_def_id_with_path` (`src/semantic/safety/helpers.rs:496`), which for `Expr::SelfExpr` reads `resolution_map.get(span.start)`. **But `self` never gets a `resolution_map` entry:** `resolve_expr`'s `Expr::SelfExpr` sits in the resolver's **no-op arm** (`src/semantic/resolve.rs:1487`). So `find_root_def_id[_with_path](SelfExpr)` returns `None`, and ALL self-rooted places are invisible not just to the place-overlap check but to the whole safety layer (move-tracking `helpers.rs:474`, iterator-invalidation, needless-mut, arena, MutRef-exclusivity).

**The key fact (scout-verified):** `self` **already has a real `DefId`** — it is bound as an ordinary parameter in `resolve_function` (`resolve.rs:1096-1112`: `define_with_mutability(name, DefKind::Variable, …, mutable=true)`, then `is_param = true`, `param_ownership = <self's sigil>`). The resolver simply never WRITES the usage-site `resolution_map` entry the safety layer already READS. This is a pure **write-site omission** (devbook/24: "fix at the write site, not the read site").

**Measured:** `gg check` ACCEPTS `place_overlap_self_root_error.gg`; `gg run` prints `3` — the aliased `y.push(9)` (`y = &self.a.b`, passed alongside `&self.a`) executes and LANDS, so two overlapping writers run simultaneously — the exclusivity guarantee production's lazy CoW is licensed by is silently broken. ggdef rejects it (a genuine production↔ggdef divergence, Core #8).

---

## 1. The fix — option (a), a ~13-line resolver write-site correction

Move `Expr::SelfExpr` OUT of the resolver no-op arm into a dedicated arm that wires each usage to the existing `self` param DefId (the scout's proven prototype, `scouts/patches/selfroot-proto.patch`):

```rust
// `self` is bound as an ordinary parameter (DefKind::Variable, is_param=true)
// in `resolve_function`. Wire each usage site to that DefId so the safety
// layer's place primitives (`find_root_def_id[_with_path]`) can root
// self-projected places (`self.a.b`) for aliasing / move / borrow checks —
// exactly as for an identifier-rooted place. `lookup` returns None outside a
// method (SelfExpr cannot appear there), leaving behavior unchanged.
Expr::SelfExpr => {
    if let Some(def_id) = scopes.lookup("self") {
        resolution_map.insert(expr.span.start, def_id);
    }
}
```

**Why (a) and NOT a sentinel/enum root (option b):** `self` already has a real DefId, so (a) is typed metadata reused at its natural source — it fixes the WHOLE class (place-overlap, move-tracking, iterator-invalidation) in one write-site edit. Option (b) (a reserved sentinel DefId, or changing `find_root_def_id_with_path`'s return type to `Root { Def(DefId), SelfRoot }`) would invent a SECOND representation for a root that already has one, fix only the place-overlap instance while leaving move-tracking blind, and a reserved-sentinel-DefId is the exact anti-pattern devbook/24 rule 2 forbids. **(a) is the layering-correct choice.**

**Layering-discipline citations:** devbook/24 "fix at the write site" (the read site already handles SelfExpr; the writer was lossy) + "typed metadata, not name-matched" (reuse the real `self` DefId, no sentinel). Design rationale: `docs/language-design.md` §3.5 (exclusivity — self-rooted places must obey the same borrow rules as any other place).

---

## 2. Blast radius — MEASURED (scout §double-diagnose)

The fix activates self-rooted places across the whole safety layer. The load-bearing risk was double-diagnosis; the scout MEASURED 8 probes — **no doubling anywhere**:
- `g(&self.a, &self.a.b)` → **1× E_BorrowConflict** ✓ (the target).
- `take2(!self.a, !self.a)` → **1× E_DoubleMove** (check_call_aliasing's `(Move,Move)→skip` at `helpers.rs:1249` delegates cleanly to the move-tracker — no double).
- `f(&self.a, self.a)` → **1× E_BorrowConflict** (non-Copy read overlapping a writer, per ADDENDUM).
- `&self` mutate + two-bare-reader cases correctly ACCEPT.
The other self-checks (`origins.rs`, `return_borrows.rs`) never route through `find_root_def_id`, so they cannot double up.

**Measured gates on the prototype (scout):** lib **1107/0** · `self_host_bootstrap_fixed_point` **GREEN** (the self-host recompiles itself — its own source has NO newly-rejected self-rooted pattern) · place_overlap 8/0 · cow_ 91/0 · equip 9/0 · borrow_conflict/mutex/rwlock/dict_alias 0-fail. In-repo `for … in self.` blast radius = **0** (all 3 occurrences iterate method-CALL results, which `find_root_def_id` returns None for).

**One nuance — NOT a regression (do NOT try to fix it here):** `for x in self.a: self.b.push(…)` (DISJOINT self fields) over-rejects — but this is a PRE-EXISTING root-granularity limitation of the FOR-LOOP check (`check_expr.rs:501` is root-only, unlike check_call_aliasing's projection-aware `paths_overlap`). The identifier-rooted analog `for x in t.a: t.b.push(…)` (a path this fix does NOT touch) over-rejects IDENTICALLY today. So the fix makes self CONSISTENT with existing (imperfect) local behavior — it neither introduces nor worsens the for-loop granularity issue. File it as a separate owner question (§4), do NOT fold it in.

**Why the change is CONFINED (pass-1 structural argument, stronger than "measured"):** the new `resolution_map` entry is keyed at the `self` token offset, but it is CONSUMED only by the safety place-primitives — **every** caller of `find_root_def_id[_with_path]` is in `src/semantic/safety/` (grep: check_expr.rs / helpers.rs / check_stmt.rs; ZERO callers in lowering / typecheck / lint / rewrite). Each NON-safety `resolution_map` consumer is node-type/name/kind-guarded and provably inert to a SelfExpr entry: `ir/lowering/mod.rs` `root_static` reads it only in the `Expr::Identifier` arm (a self-place peels to SelfExpr → `_ => None`); `typecheck.rs` `resolve_name` is name-guarded on an identifier/callee/receiver text (never `"self"`, a keyword); `ir/lowering/stmts/patterns.rs` is Const/Static-kind-guarded (self is Variable); `lint_suggest_throws` reads a Constructor path-segment; `rewrite.rs` resolves a callee identifier. Lowering a `self` reference goes through its OWN name-based arms (`functions.rs` `Some("self")`, `generics/mod.rs` `env.lookup("self")`), never the map. So the change is "global" only in that the map GAINS an entry; its EFFECT is structurally confined to the safety layer. (The full C+LLVM sweep is the corpus-wide backstop for this, not the primary argument.)

**Obs B (pre-existing under-check, NOT a fix failure):** `scopes.lookup("self")` may not resolve inside a CLOSURE nested in a method (if `lookup` is function-scope-bounded), leaving a `self.a` reference INSIDE a closure body unrooted. This is the STATUS QUO today (no entry either way) — an under-check (never over-rejection, never unsoundness), NOT a regression the fix introduces. The executor must NOT treat an uncaught closure-internal self-alias as a fix failure; if desired, add a `#[ignore]`d fixture + TODO for it, but it is out of scope.

---

## 3. Deliverables

1. **The resolver fix** (`src/semantic/resolve.rs`) — the dedicated `Expr::SelfExpr` arm above.
2. **Flip the quarantined fixture ACTIVE:** `tests/fixtures/place_overlap_self_root_error.gg` — remove the `#[ignore]` on `place_overlap_self_root_error` in `tests/integration.rs` (it now REJECTS with `E_BorrowConflict`); update the fixture's header comment from "IGNORED — pins a KNOWN gap" to the active-reject reality. Verify it emits **exactly one** E_BorrowConflict.
3. **Add a POSITIVE regression guard:** a self disjoint-sibling fixture `f(&self.a, &self.b)` (two writers into DISJOINT self fields) must ACCEPT + build+run correctly — the self analog of `place_overlap_disjoint_siblings.gg`. Confirms the fix doesn't over-reject disjoint self projections at the CALL site (distinct from the for-loop nuance).
4. **Optional:** a self-move fixture `take2(!self.a, !self.a)` → E_DoubleMove pin (documents the delegation).
5. **File the 2 owner questions in TODO.md** (§4) as pending Medium items.
6. **Un-flip the TODO/DONE bookkeeping is the PARENT's job** — the executor does NOT edit the HIGH self-root TODO entry (parent moves it to DONE on landing).

---

## 4. Owner questions to FILE (not fold — separate work)

1. **For-loop iterator-invalidation is root-granular for BOTH self and locals** (`check_expr.rs:501`): `for x in c.a: c.b.push(…)` over-rejects disjoint fields (unlike the projection-aware call-arg check). Should the for-loop check adopt field-path disjointness (reuse `find_root_def_id_with_path` + `paths_overlap`)? Pre-existing, affects self AND locals equally; separate scout.
2. **Moving a field out of a bare (borrowed) `self`** (`take(!self.items)`) stays accepted — a D12/partial-move question, separate.

---

## 5. Gates (the FULL sweep is the real net — the fix is small but its EFFECT is global)

Executor runs FOREGROUND, CHUNKED (rule 9, `GG_BUILD_TIMEOUT_SECS=600`):
1. `cargo build`; 2. `cargo test --lib` (~1107); 3. the flipped + new fixtures: `cargo test --test integration place_overlap borrow_conflict double_mutable -- --test-threads=4` (self_root now ACTIVE-rejects; disjoint-self-sibling POS builds+runs); 4. `self_host_bootstrap_fixed_point` (self-host recompiles — no new self-root rejection); 5. self-heavy slices: `cargo test --test integration self_host equip -- --test-threads=4` + any `mutex`/`rwlock`/`arena` fixtures (catch double-diagnose/regression the scout's probes didn't).
- **PARENT drives the FULL C + FULL LLVM sweeps** — this fix changes SelfExpr resolution GLOBALLY, so the whole fixture corpus is the over-rejection/double-diagnose gate (slice validation is NOT sufficient here — the scout ran targeted slices + bootstrap, not the full sweep). If any in-repo site newly-rejects, TRIAGE (real overlap → good; disjoint over-reject → the for-loop nuance, file don't-weaken) — NEVER weaken the resolution fix.

---

## 6. Worktree + playbook preamble (CLAUDE.md "Multi-agent")

Standard preamble (verify `pwd`/`git rev-parse --show-toplevel` inside the worktree; NEVER touch `/workspace/gorget` or `/workspace/gorget-1`; no `/workspace/gorget/...` absolute paths). `isolation: "worktree"`, `model: "opus"`; worktree branches from current main (has B1 + the scout). Stage EXPLICITLY by file name; NEVER `git add -a`/`commit -a`/`git stash` (save with `git diff > /tmp/selfroot_<name>.patch`); checkpoint a durable patch to `scouts/patches/selfroot-fix.patch`. Run FINAL gates FOREGROUND. On Edit-tool desync, re-Read + retry — never a heredoc with an absolute path.

---

## 7. Definition of done

- [ ] `Expr::SelfExpr` wired to the `self` param DefId in `resolve.rs` (the write-site fix); NO sentinel/enum-root hack.
- [ ] `place_overlap_self_root_error.gg` FLIPPED ACTIVE — `#[ignore]` removed, emits **exactly one** `E_BorrowConflict` (verified — not doubled, not an aliasing-vs-move confusion); header comment updated to active-reject.
- [ ] Disjoint-self-sibling POS fixture `f(&self.a, &self.b)` ACCEPTS + build+runs on BOTH backends.
- [ ] `self_host_bootstrap_fixed_point` GREEN (self-host's own self-code not newly-rejected).
- [ ] **FULL C + FULL LLVM sweeps GREEN** (parent — the global-resolution-change over-rejection/double-diagnose gate). Any newly-rejected site TRIAGED (real → keep; for-loop-granularity disjoint → filed, not weakened).
- [ ] The production↔ggdef divergence CLOSED — `g(&self.a, &self.a.b)` now rejects in BOTH (Core #8).
- [ ] The 2 owner questions FILED in TODO.md (Medium); no new fossil; the fix reads as idiomatic (a missing resolver arm, restored).
- [ ] No double-diagnose introduced anywhere (the load-bearing risk — the full sweep confirms the scout's 8-probe measurement holds corpus-wide).

---

## 8. Non-goals
- **No for-loop granularity fix** (owner Q1 — separate; the fix makes self match existing local behavior, imperfect-but-consistent).
- **No self-host `.gg` self-root check** (that's B2's mirror; this is production-only).
- **No partial-self-move rule** (owner Q2).
- **No sentinel/enum root** (option b, rejected on layering grounds).
- Any NEW gap the fix surfaces → triage + file, never a reshape to dodge.
