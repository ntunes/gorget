# RV-C SCOUT — compound index-assign eval order + un-rejected aliasing

**Scout-only. No src/ changes. All numbers regenerated this session against HEAD
of worktree `agent-a1aa7869c2f2630ba` (built `target/debug/gg` + `target/debug/ggdef`).**

Regen commands (probes in `/tmp/rvc/`):
- Production per-shape: `gg run <f>` and `gg run --backend=llvm <f>` and `gg check <f>`
- ggdef: `ggdef run <f>`

---

## 1. CONFIRMED: the finding reproduces

`gg check` **ACCEPTS** `v[0] += mutate(&v)` for every element type (exit 0) — the
aliasing shape is un-rejected. And the evaluation order is **element-type-dependent**:
resource (custom-`Drop`, non-string) elements lower **RHS-FIRST**; int / String /
custom-`Index` elements lower **READ-FIRST**. Both backends agree (the divergence is
in shared GIR lowering, `assigns.rs:1621` `rhs_pre`, NOT backend-specific).

Root cause in code: the Index compound arm (`src/ir/lowering/stmts/assigns.rs`) sets
`borrow_in_place = is_resource && != String` (`:1604`). When true, it lowers the RHS
into an owned temp FIRST (`rhs_pre`, `:1621`) — the **R1 reorder** — then takes the
borrow-in-place element `Ptr`. When false, it reads `cur_val` first (`:1647`) then
lowers RHS (`:1703`). The reorder exists to close a real UAF (see §5) but leaks its
ordering as observable semantics.

---

## 2. MEASURED MATRIX

Probe convention: `v[0]` starts at 1 (or "a"); the RHS `mutate(&v)` sets `v[0] := 100`
(or "MUT") and RETURNS 10 (or "X"). READ-FIRST ⇒ `1 op 10`; RHS-FIRST ⇒ `100 op 10`.

### Production (C and LLVM identical on every row)

| Shape                              | element      | order       | C / LLVM out | `gg check` |
|------------------------------------|--------------|-------------|--------------|------------|
| `v[i] += mutate(&v)`               | int          | READ-FIRST  | `11`         | OK (accept)|
| `v[i] += mutate(&v)`               | String       | READ-FIRST  | `aX`         | OK         |
| `v[i] += mutate(&v)`               | resource+Add | **RHS-FIRST** | `110`      | OK         |
| `g[i] += mutate(&g)` custom `Index`| int (get)    | READ-FIRST  | `11`         | OK         |
| `h.v[i] += mutate(&h)` field path  | int          | READ-FIRST  | `11`         | OK         |
| `h.v[i] += mutate(&h)` field path  | resource+Add | **RHS-FIRST** | `110`      | OK         |
| `d[k] += mutate(&d)` dict          | resource+Add | **RHS-FIRST** | `110`      | OK         |

Divergence CONFIRMED: `11` (read-first) vs `110` (RHS-first) on the SAME program shape,
differing only by element type. All 7 rows `gg check` = OK ⇒ aliasing fully un-rejected.
The field-collection path `h.v[i]` behaves like the plain path (routes through the same
Index arm). Resource elements need `!av` at the ctor (D12) — expected, orthogonal.

### Side-effecting index `v[pick()] += 10` (does the index eval once?)

| Lane                          | `pick()` calls | note |
|-------------------------------|----------------|------|
| Production, plain `v[i]`      | **1** (once)   | idx saved to `idx_local` `:1569`, reused read+write |
| Production, field `v[i].f`    | **1** (once)   | FieldAccess arm resolves place once |
| ggdef                         | **2** (TWICE)  | NEW BUG — see §6 |
| self-host, plain `v[i]`       | 1 (once)       | `lower_index_compound_assign` reads idx once (`lower_stmt.gg:2011`) |
| self-host, field `v[i].f`     | **2** (TWICE)  | filed TODO ~865 (read `lower_expr(target)` `:1184` + write `emit_field_write_from_local(base…)` `:1191`) |

### ggdef (int-element subset)

| shape                     | verdict | order/out | note |
|---------------------------|---------|-----------|------|
| `v[0] += mutate(&v)`      | ACCEPT  | `11` read-first | same aliasing gap as production; CompoundAssign arm `elaborate/mod.rs:705` never calls `check_arg_place_overlap` |
| `v[pick()] += 10`         | ACCEPT  | pick ×2   | double-eval (§6) |

**ggdef order = read-first** (eval evaluates `value` before the target place,
`eval.rs:590-591`; `value = Binary(lhs=read x, rhs)` so the lhs read fires first).

### Self-host (read from `tests/fixtures/self_host_lowerer/lower_stmt.gg`)

- **Plain `v[i] += x`** (`lower_index_compound_assign`, `:1993`): UNIFORMLY READ-FIRST
  for every element type (`cur = getter(base,idx)` `:2054` BEFORE `rhs = lower_expr` `:2056`);
  idx resolved once. NO resource reorder ⇒ **diverges from production's resource path**.
  This is the filed WRONG-output `compound_index_resource_taint` (TODO ~246(i), a separate
  self-host resource drop bug — its RHS is a pure ctor so ORDER is not the cause; do not
  fold it into RV-C, but read-first-unifying production ALIGNS the order axis).
- **Field-on-index `v[i].f += x`**: base+idx evaluated TWICE (TODO ~865).

**Lane tally on eval order: 3 of 4 lanes are uniformly READ-FIRST** (production-nonresource,
self-host, ggdef). Only production's resource path is RHS-first. Read-first is also the
reference-grade order for a Python-like L-to-R language (`v[i] += e` = read place, then
evaluate `e`). Production's RHS-first is a UAF-driven wart, not a design choice.

---

## 3. DRAFT LEDGER RULING (ready for `decisions.md`)

> **RV-C / D10(b) EXTENSION — compound-assign LHS/RHS place-overlap + uniform read-first
> order (owner RATIFY?).** A compound index/field assignment `LHS op= RHS` is a
> **read-modify-WRITE of the place rooted at `LHS`** — the LHS is an implicit exclusive
> **writer** for the duration of the statement, exactly as an `&`-arg is a writer for the
> duration of a call. Therefore the D10(b) place-overlap rule (ratified 2026-07-12, live
> aliases not syntactic reads) **extends to the compound-assign RHS**: any **live alias**
> to the LHS's root place appearing in `RHS` — a `&`-borrow, a `!`-move, a mutating method
> receiver, or a **non-Copy bare read** of `root(LHS)` or an overlapping sub-place — is a
> writer/writer (resp. writer/mover, writer/live-reader) overlap and is **REJECTED at check
> time** (`E_BorrowConflict`, the D10(b) diagnostic family). A **Copy-typed bare read** of
> an overlapping sub-place (`v[0] += v[1]` for int) is a value **snapshot** and is
> **EXEMPT** — uniform with the ratified D10(b) Copy-read carve. So `v[0] += mutate(&v)`,
> `v[0] += grow(&v)`, `h.v[0] += grow(&h)`, `d["a"] += growd(&d)` are all **REJECTED**;
> `v[0] += v[1]` (int) and `v[0] += pure_expr` stay accepted.
>
> **Consequence — the evaluation order becomes unobservable for all accepted programs**
> (a RHS that neither reads nor mutates `root(LHS)` cannot distinguish read-first from
> RHS-first). We therefore **fix ONE uniform order: READ-FIRST** (read the current element,
> then evaluate the RHS) across all element types and all four lanes. This retires
> production's element-type-dependent split by removing the resource RHS-FIRST reorder
> (`assigns.rs:1621`), whose sole justification — the R1 realloc-UAF window — is now
> subsumed by the reject: with no live alias to `root(LHS)` legal in the RHS, the RHS can
> never realloc the collection while the borrow-in-place `Ptr` is held. The ICE-closing
> borrow-in-place read (A2-R2 M1) **STAYS**; only the RHS-first *reorder* is dropped.
> Read-first is also what self-host and ggdef already do and is the reference-grade
> Python-like L-to-R order.
>
> **Rejected alternatives.** (a) *Pin an order, keep accepting aliasing.* Read-first
> re-opens the resource UAF (the borrow-in-place `Ptr` dangles when the aliased RHS
> reallocs) unless resources stay RHS-first — i.e. the divergence survives; and RHS-first-
> uniform gives `v[0] += mutate(&v)` the genuinely confusing "read the element AFTER an
> aliased write to it" semantics that Gorget's exclusivity model exists to forbid.
> Principle loses, not just cost. (b) *Reject only the reallocating case.* Not statically
> decidable (cannot know if `f(&v)` reallocs); would need runtime guards. Rejected.
> (c) *Reject aliasing + keep the reorder as defense-in-depth + also flip primitives to
> RHS-first for uniformity.* Churns ~20 byte-identical C↔LLVM snapshot fixtures and makes
> production diverge from self-host/ggdef (both read-first) — strictly worse for
> conformance. Rejected in favor of read-first-unify.
>
> **Blast radius = ZERO.** A whole-repo census (2179 `.gg` files: `tests/`, `lib/`,
> `examples/`, self-host, arena fixtures) found NO accepted program whose compound-assign
> RHS aliases the LHS root. The only `v[i] op= f(&v)` sites in the tree are R1's own
> realloc UAF *counterfactual probes* (`docs/plans/define-gorget/scouts/patches/a2-r2-realloc-probes/`)
> — precisely the hazard this reject is meant to forbid. Every real compound-index-assign
> in the corpus (35 statements) has a constant / pure-ctor / disjoint RHS.

(Numbering note for the orchestrator: file as a D10(b) EXTENSION addendum, not a new
D-number — it is the same live-alias/CoW-divergence axis D10 polices, applied to the
implicit compound-assign writer.)

---

## 4. FIX-SHAPE SKETCH PER LANE (for the eventual B-successor executor)

**Evaluate the reject FIRST (owner mandate), then the order unification.**

### Production (`src/`) — zones disjoint from RV-A/RV-B
1. **Reject (semantic safety).** New helper `check_compound_assign_aliasing(target, value)`
   in `src/semantic/safety/helpers.rs`, mirroring `check_call_aliasing` (`:1179`):
   synthesize the LHS root place as an implicit `Ownership::MutableBorrow` writer
   (`find_root_def_id_with_path(target)`), collect every place-reference in `value` with
   its effective ownership (call-arg sigils, `!`-moves, **mutating method receivers**,
   non-Copy bare reads via `expr_value_is_copy`), reuse the pairwise overlap + Copy-read
   exemption + `paths_overlap`. Emit `SemanticErrorKind::BorrowConflict`. Call site:
   `src/semantic/safety/check_stmt.rs` `Stmt::CompoundAssign` arm (`:655`). NB the RHS walk
   must range over the FULL expression, not just top-level call args — the method-receiver
   case (`v[0] += v.grow_method()`) is a live alias the top-level-args-only check would miss.
2. **Unify order.** In `src/ir/lowering/stmts/assigns.rs`, drop the `rhs_pre` reorder
   (`:1621-1625`) — make the `borrow_in_place` path read-first like the others (take the
   borrow-in-place `Ptr` at `:1651`, then lower RHS at `:1703`). Keep borrow-in-place
   (ICE fix). Accepted-program output is unchanged (verified: the resource-taint fixture's
   RHS is a pure ctor ⇒ order-invariant).
3. **Add a `container_literal_arms`-style sibling lint** if a new compound-assign lowering
   arm is ever added, so the reorder-drop can't silently regrow (Core #4).

### Self-host (`tests/fixtures/self_host_typechecker/typecheck.gg`)
1. **Mirror the reject** in the self-host safety walk (the D10(b) call-arg mirror already
   exists — extend it to SCompoundAssign). Bootstrap-gated.
2. **Order: already read-first** — no lowering change. (The separate `compound_index_resource_taint`
   WRONG output [TODO ~246(i)] and the `v[i].f += x` double-eval [TODO ~865] ride their own
   tracks — NOT RV-C; flag both in the brief so the executor doesn't conflate them.)

### ggdef (`spec/ggdef/`)
1. **Mirror the reject** in `elaborate/mod.rs` `CompoundAssign` arm (`:705`): treat the
   elaborated LHS as a writer and run the `check_arg_place_overlap` logic (`:1088`) against
   the RHS's place-references before desugaring. Currently NO check runs there.
2. **Fix the double-eval** (§6) — resolve the place once. This is REQUIRED for the
   read-first-once semantics to hold in the definition (production evals once).

---

## 5. WHY THE R1 REORDER EXISTS (the UAF the reject subsumes)

`docs/plans/define-gorget/scouts/patches/a2-r2-realloc-probes/realloc_v.gg`:
`v[0] += grow(&v)` where `grow` pushes 200 elements (reallocs the vector) then returns.
The borrow-in-place element `Ptr` (taken for `add`'s read-only `self`) dangles across the
realloc → `heap-use-after-free` (ASan, counterfactual with reorder off; DONE.md 2026-07-12).
The reorder (RHS-first) sidesteps it by lowering the RHS before taking the `Ptr`. Under the
RV-C reject, `grow(&v)` is a `&v` writer overlapping the `v[0]` writer ⇒ **rejected at
check** ⇒ the program never lowers ⇒ the UAF window is closed by the reject, and the reorder
is redundant. **Keep the three realloc probes as-is** — after the reject lands they become
NEGATIVE conformance fixtures (should-be-rejected).

---

## 6. NEW BUG FOUND EN ROUTE (file-don't-fix)

**🐛 ggdef double-evaluates a side-effecting compound-assign index (production evals once).**
`ggdef run /tmp/rvc/gg_sidefx.gg` on `v[pick(&log)] += 10` runs `pick` TWICE (`log.len() == 2`);
production runs it ONCE. Root: `elaborate/mod.rs:705` desugars `x op= e → x = x op e` by
elaborating the target expr TWICE (`target_expr` for the write + `lhs` for the read); at eval
(`eval.rs:588-591`) the value (containing the `lhs` index) and the target place each evaluate
the index expression, so any side-effect in the index fires twice. This is a real
`elaborate ∘ eval` conformance divergence from production (and from the intended once-only
compound-assign place semantics — C/Python/Rust all evaluate the place once). Fix direction:
resolve the compound-assign target place ONCE and thread it to both the read and the write
(the natural place is the CompoundAssign elaboration — emit a single place binding, or an
eval-side compound-store op). **File as a ggdef HIGH; it pairs with RV-C's ggdef lane** but is
a distinct defect from the aliasing reject. Repro: `/tmp/rvc/gg_sidefx.gg`.

---

## 7. GATES FOR THE EVENTUAL EXECUTOR

- **Reject discriminates:** new NEG fixture `compound_index_alias_reject.gg`
  (`v[0] += mutate(&v)` → `E_BorrowConflict`) rejected in production AND self-host AND ggdef;
  the three `a2-r2-realloc-probes` become should-reject. POS control: `v[0] += v[1]` (int,
  Copy snapshot) + `v[0] += mk(5)` (pure ctor) stay accepted.
- **Order unified:** `gg run` == `gg run --backend=llvm` on every compound fixture; the
  resource-taint fixture output BYTE-UNCHANGED after dropping the reorder (accepted RHS is
  pure). Add a POS fixture pinning read-first: `v[0] += v[1]` where reading order is
  observable only via a Copy snapshot (result = old v[0] + old v[1]).
- **No ICE regression:** `compound_index_resource_taint.gg` still drop-once, ASan-clean,
  byte-identical C↔LLVM (the borrow-in-place read stays).
- **Cross-lane:** `spec_conformance_*` (ggdef/C/LLVM/self-host) all green at floor; ggdef
  double-eval fix pins `v[pick()] += x` = one call in all lanes.
- **Full battery:** `cargo test --lib`; full integration C + LLVM at `--test-threads=4`;
  `self_host_bootstrap_fixed_point`; `self_host_runtime` (mandatory for the self-host reject);
  parity regen (predicted neutral — no accepted program changes output).

---

## Probe inventory (`/tmp/rvc/`)
`int_mutates.gg` `str_mutates.gg` `res_mutates.gg` `getcustom.gg` `field_int.gg`
`field_res.gg` `dict_res.gg` `realloc_int.gg` `sidefx_index.gg` `sidefx_field.gg`
`gg_alias.gg` (ggdef) `gg_sidefx.gg` (ggdef).
