# Scout report — D10(b) self-root gap

**Date:** 2026-07-14 · **Branch base:** `fd4d50be` (B1 landed) · **Status:** design recommended, prototype MEASURED clean.

## TL;DR

The gap is a **one-line write-site omission** in the resolver, not a design problem.
`self` already has a real `DefId` (it is bound as an ordinary param); the resolver
just never wires `Expr::SelfExpr` usage sites to it. The recommended fix is **option (a)**
— give `SelfExpr` its `resolution_map` entry at the resolver — a 13-line diff.

**The load-bearing unknown (double-diagnose risk) MEASURED as NOT REAL.** Every newly
active self-check emits exactly ONE correct diagnostic. lib 1107/0, bootstrap fixed-point
OK, place_overlap 8/0, cow_ 91/0, equip 9/0 — all green.

---

## 1. Confirmed repro + scope

### The target case (verified this session)
`tests/fixtures/place_overlap_self_root_error.gg`:
- **Baseline `gg check`: ACCEPTS** (`exit 0`, "OK: no semantic errors"). Should REJECT.
- **Baseline `gg run`: prints `3`** — the aliased `y.push(9)` (where `y = self.a.b`,
  passed alongside `&self.a`) **executes and lands**. This is a **live soundness hole**,
  not a latent-hazard-only case: two writers into overlapping self-rooted places
  (`&self.a` + `&self.a.b`) run simultaneously, silently breaking the D10(b) exclusivity
  guarantee that production's lazy CoW is licensed by.
- **ggdef REJECTS it**: `self_host_check/typecheck.gg:643` — `place_root_name` returns
  `"self"` for `ESelfExpr` because it roots on identifier TEXT (no DefId map). Confirmed
  production↔ggdef divergence.

### Mechanism (file:line, verified)
- `self` **is** a real binding: `resolve.rs:1096-1112` defines EVERY param — including
  `"self"` — as `DefKind::Variable`, `is_param = true`, with `param_ownership`.
  Bare `self` = `Ownership::Borrow`, `&self` = `MutableBorrow`, `!self` = `Move`
  (`parser/mod.rs:1863/1870/1877`, `make_self_param`).
- **The hole:** `resolve_expr`'s `Expr::SelfExpr` sits in the **no-op arm**
  (`resolve.rs:1487`) → no `resolution_map` entry → `find_root_def_id(SelfExpr)` /
  `_with_path(SelfExpr)` (`helpers.rs:474,496`) return `None` → `check_call_aliasing`
  skips self-rooted places (`helpers.rs:1193`).

### Full scope of "self-rooted places are invisible to the safety layer"
Every safety site that roots a place via `find_root_def_id[_with_path]` currently
misses self. Enumerated (all in `src/semantic/safety/`):

| Site | file:line | Effect of the gap today |
|---|---|---|
| **check_call_aliasing** (the target) | `helpers.rs:1193` | `g(&self.a, &self.a.b)` accepted (bug) |
| check_move (`!self.f` field-move; `f(!self.f)`) | `check_expr.rs:156, 236` | partial self-moves invisible to move-tracking (incl. `take2(!self.a, !self.a)` double-move) |
| check_borrow_field_mutation (`f(&self.f)`) | `check_expr.rs:254` | borrow-field invalidation misses self |
| for-loop iterator-invalidation (`for x in self.v`) | `check_stmt.rs:983` → `check_expr.rs:501` | mutating `self.v` while iterating it accepted |
| mut-param-mutated / needless-mut (`&self` mutates) | `check_expr.rs:374`, `helpers.rs:940` | self never marked mutated |
| arena-escape (self target under `with`) | `check_stmt.rs:444, 671`, `check_expr.rs:629` | self-rooted arena target misses escape check |
| MutRef exclusivity (`S(&self.f)`) | `check_expr.rs:1186` | self-source MutRef misses exclusivity |

**Not affected** (independent SelfExpr handling, do NOT use `find_root_def_id`):
`origins.rs:257` (uses `current_param_def_ids.first()`), `return_borrows.rs:529,627`
(use `param_names.contains_key("self")`). Typecheck `SelfExpr` (`typecheck.rs:1329`)
uses `current_self_type`, independent of `resolution_map`.

---

## 2. The SelfExpr root mechanism + candidate fixes

**Why no entry today:** `SelfExpr` was simply grouped with literals in the resolver's
no-op arm. There was never a reason it *couldn't* resolve — the param `DefId` exists in
the active function scope at the point `resolve_expr` walks the body (`resolve_function`
pushes the scope at `:1060`, defines params at `:1096`, resolves the body at `:1114`).

### Candidate (a) — RECOMMENDED: resolve `SelfExpr` to the existing `self` param DefId
```rust
Expr::SelfExpr => {
    if let Some(def_id) = scopes.lookup("self") {
        resolution_map.insert(expr.span.start, def_id);
    }
}
```
- **Layering rationale (docs/devbook/24 rules 1-2, "fix at the write site"):** this is the
  **write-site** fix. The safety layer already *asks* for a typed root of `self`
  (`helpers.rs:474,496` read `resolution_map` for `SelfExpr`) — the resolver just never
  *wrote* it. `self` genuinely IS a binding with a real `DefId`; wiring it is **typed
  metadata, not a name/shape hack**. No sentinel, no enum, no change to any place-primitive
  caller. It mirrors the `Identifier` arm exactly (`resolve.rs:1508`).
- **Blast radius:** intentionally the WHOLE class — every table row above now roots self.
  Measured clean (§3-4).
- **Consistency with the D10 ruling** (decisions.md:654, "root + projection prefix"):
  self-rooted places now participate in the exact same place-overlap machinery as
  identifier-rooted ones — `check_call_aliasing` already does field-path disjointness
  (`paths_overlap`) and the Copy-read exemption (ADDENDUM, decisions.md:346), so self
  inherits the *precise* rule, not an approximation.

### Candidate (b) — REJECTED: sentinel/synthetic self-root confined to place primitives
Would mean `find_root_def_id` returns an enum `Root { Def(DefId), SelfRoot }` or a reserved
sentinel `DefId`, rippling to the ~15 callers (they compare/`.get_def()` the root). This is
**strictly worse**: (1) it invents a second representation for a root that already has a
real `DefId`, violating "one source of truth per axis"; (2) it *narrows* the fix to the
place primitives, leaving move-tracking / needless-mut / iterator-invalidation still blind
to self — i.e. it fixes the instance, not the class ("Sibling-site drift"); (3) a reserved
`DefId` is exactly the sentinel-value anti-pattern devbook/24 rule 2 forbids. There is no
upside — (a) is smaller AND more principled.

---

## 3. ⚠ Double-diagnose risk — ENUMERATED + MEASURED (the load-bearing unknown)

Every Rust-side error-producing check that newly fires on self, PROTOTYPED and measured
(all via `gg check`, ANSI-stripped):

| Probe | Program (inside `equip`) | Result under fix | Verdict |
|---|---|---|---|
| **target** | `g(&self.a, &self.a.b)` | **1× E_BorrowConflict** (`&self.a`/`&self.a.b`) | intended, single |
| p1 | `&self` method: `self.items.push(x)` | **accept** | no spurious needless-mut |
| p2 | `take(!self.items)` single partial move | accept (unchanged) | pre-existing gap, not regressed |
| p3 | `for x in self.items: self.items.push(x)` | **1× E_MutationWhileBorrowed** | correct NEW reject (parity w/ locals) |
| p4 | `sum2(self.a, self.a)` two bare non-Copy reads | accept | correct (no writer → no conflict) |
| p5 | `for x in self.a: self.b.push(x)` (disjoint) | 1× E_MutationWhileBorrowed | see note ↓ |
| p6 | `for x in t.a: t.b.push(x)` (LOCAL struct, disjoint) | 1× E_MutationWhileBorrowed | **baseline — my fix does NOT touch this path** |
| p7 | `take2(!self.a, !self.a)` double self-move | **1× E_DoubleMove** ("first move here") | correct NEW reject, no doubling |
| p8 | `f(&self.a, self.a)` writer + non-Copy read | **1× E_BorrowConflict** | correct NEW reject (ADDENDUM cut) |

**No double-diagnosis observed anywhere.** Each newly active check emits a single,
correct diagnostic. Why the risk the `#[ignore]` comment/TODO feared did NOT materialize:
- `check_call_aliasing` already has the `(Move,Move)→skip` guard (`helpers.rs:1249`,
  delegates to E_DoubleMove — see p7) and the Copy-reader exemption (`helpers.rs:1241`).
- The other self-mutation checks (`origins.rs`, `return_borrows.rs`) never routed through
  `find_root_def_id`, so they cannot double up.
- The one existing "reject &self mutator" check is `reject_amp_self_mutator` in **ggdef**
  (`typecheck.gg`) — a SELF-HOST check, **not Rust-side** (out of scope for this fix per
  the brief; flag for B2).

### The ONE nuance (p5/p6) — NOT a regression
`for x in self.a: self.b.push(x)` (iterate one field, mutate a **disjoint** sibling)
over-rejects. But **this is a pre-existing root-granularity limitation** of the for-loop
iterator-invalidation check (`check_expr.rs:501` uses `for_loop_iterables.contains(root)`,
root-only, NOT the `paths_overlap` field-path logic that `check_call_aliasing` uses).
p6 proves it: the **identifier-rooted local analog** `for x in t.a: t.b.push(x)` — a path
my fix does NOT change — rejects **identically**. My fix brings self to the same (imperfect
but consistent) behavior locals already have; it introduces no new false-positive *class*.
→ **Owner/TODO note:** the for-loop invalidation check should adopt field-path disjointness
like `check_call_aliasing`, but that is a **separate pre-existing issue affecting identifier
roots equally** — out of scope for the self-root fix. (In-repo blast radius of this = 0; see §4.)

---

## 4. Prototype end-to-end + MEASURED gates

Fix applied (13-line diff, `resolve.rs` only — `patches/selfroot-proto.patch`).

| Gate | Command | Result |
|---|---|---|
| self-root fixture | `gg check place_overlap_self_root_error.gg` | **REJECT, 1× E_BorrowConflict** (was: accept + run→`3`) |
| unit tests | `cargo test --lib` | **1107 passed, 0 failed** |
| **bootstrap** | `cargo test --test integration self_host_bootstrap_fixed_point` | **ok (1 passed)** — self-host recompiles itself, no new rejections |
| place_overlap | `cargo test --test integration place_overlap` | 8 passed, 0 failed (2 ignored) |
| borrow_conflict | `… borrow_conflict` | 1 passed, 0 failed |
| cow_ (alias runtime) | `… cow_` | 91 passed, 0 failed |
| equip | `… equip` | 9 passed, 0 failed |
| snag30 / box_deref_self / mutex_alias / rwlock_alias / dict_alias | resp. filters | all 1 passed, 0 failed |

**In-repo `for ... in self.` blast radius = 0:** only 3 occurrences exist
(`self_host_typechecker/scope.gg:473`, `types.gg:306`, `parser.gg:2552`) and **all three
iterate over method-call results** (`.enumerate()`, `.parse_call_args()`); `find_root_def_id`
returns `None` for `MethodCall` (`helpers.rs:475`), so none are tracked as self-iterables.
The bootstrap passing (compiles the entire self-host — the repo's most method/equip/self-heavy
program) confirms no self-rooted place in self-host source newly-rejects.

**Yield: measured end-to-end (compile AND run AND diff), not source-read.** The fixture goes
accept-and-miscompile → reject-with-one-error; no counter-regression in any gate.

---

## 5. Recommendation

**Ship option (a)** — the 13-line resolver write-site fix. It is minimal, principled (fixes
the class at the source, typed metadata, mirrors the Identifier arm), closes a live soundness
hole, and MEASURED clean across lib + bootstrap + the full aliasing/equip/cow slice with zero
double-diagnosis. The double-diagnose risk that gated this work is **empirically not real**.

**Brief the fix track to also:**
1. Un-`#[ignore]` `place_overlap_self_root_error.gg` in the harness (it now rejects correctly);
   consider adding p7 (double self-move) + p8 (writer+read self) as negative fixtures.
2. Note in TODO/decisions: the `#[ignore]` comment's "risks double-diagnosing" justification
   is now disproven — remove it when landing.

**Owner design questions surfaced (none blocking):**
- **Q1 (real, but pre-existing & out of scope):** the for-loop iterator-invalidation check is
  **root-granular** (rejects disjoint-sibling `for x in s.a: s.b.push(...)` for BOTH self and
  locals). Should it adopt `paths_overlap` field-path disjointness like `check_call_aliasing`?
  This is a standalone precision bug affecting identifier roots equally — file as its own item,
  do NOT fold into the self-root fix.
- **Q2 (confirm scope):** moving a field OUT of a bare (borrowed) `self` — `take(!self.items)`
  — is currently ACCEPTED (p2) and stays accepted (my fix makes move-tracking *see* it but it
  doesn't newly reject a single move). If the language should reject "move out of borrowed self"
  (a D12/partial-move question), that is separate work — flag for the D12 owner, not this fix.

## Artifacts
- Prototype patch: `/tmp/selfroot_proto.patch` AND `docs/plans/define-gorget/scouts/patches/selfroot-proto.patch`
- Probe programs: `/tmp/selfroot_probes/p1..p8.gg`
- Gate logs: `/tmp/selfroot_scout/{lib_test,bootstrap,integ_slice2}.log`
