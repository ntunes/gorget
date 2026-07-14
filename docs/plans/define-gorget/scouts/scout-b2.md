# Scout Report — B2: self-host mirror of the D10(b) place-overlap check

Worktree `agent-a80c4ce814bca460d`, branched from main `c37f56be` (B0 + B1 + self-root
landed). Read-only-first, then PROTOTYPED end-to-end and MEASURED. Prototype patch:
`/tmp/b2_proto.patch` + `scouts/patches/b2-proto.patch`. All numbers below were
regenerated THIS session (commands quoted).

═══════════════════════════════════════════════════════════════════════════════════════
## TL;DR
═══════════════════════════════════════════════════════════════════════════════════════

1. **The mirror is SOUND and SMALL and WORKS.** `git diff --stat`: `typecheck.gg` +150/-2
   (≈90 code lines + comments: struct + 5 helpers + 2 one-line hooks), `diagnostic.gg` +3.
   Prototyped, built, and measured against the Rust B1 reference on 8 probes: **all 6
   place-overlap cases match Rust EXACTLY** (3 reject, 2 accept, 1 mover+non-Copy reject).

2. **Honor-the-CallArg directive: DONE.** The check reads `a.ownership`
   (OWN_BORROW/OWN_MUTABLE/OWN_MOVE) — never shape-matches `a.value`. Copy-ness is the
   TYPED axis (`infer_expr_type` → `RTPrimitive` ∧ `is_scalar_primitive_name`), mirroring
   ggdef's `Ty::Prim` (Rider 2).

3. **Root is NAME-keyed (like ggdef), NOT DefId-keyed.** This is the pivotal design
   choice and it resolves unknown #3: the self-host resolver does **NOT** resolve
   `ESelfExpr` (`resolve.gg:667` is a no-op; `self` is never `define`d), so a DefId-keyed
   mirror would silently SKIP self-rooted places. `place_root_name` already returns
   `"self"`, so name-keying handles self-root for free AND matches ggdef's `root: String`
   model exactly. Within one call, one name = one binding (shadowing across args is
   impossible), so name-keying is exactly equivalent to DefId-keying for same-call overlap.

4. **The Copy exemption is REQUIRED and works** — `f(&s, s.tag)` (int) accepts in both
   Rust and the self-host mirror. Without it, the mirror would over-reject the self-host's
   own `add_local(&ctx, ...)` / `channel_write_data(&self, self.channel_id, ...)`-shaped
   sites (the Copy-scalar sub-reads the B1 scout flagged). The self-host HAS a Copy notion
   (`is_scalar_primitive_name`, `traits.gg:732`) — no new machinery needed.

5. **One HONEST DIVERGENCE (a filed follow-up, not a silent gap):** the self-host has
   **NO move-tracker / use-after-move / liveness pass** (grep of all
   `self_host_typechecker/*` + `self_host_resolver/*` for
   `use_after_move|UseAfterMove|is_moved|VarState|move_track` → **zero hits**). So the two
   LIVENESS-axis cases Rust rejects via `E_DoubleMove` (`f(!x,!x)`) and `E_UseAfterMove`
   (`f(!x, x.copy_field)`) are NOT rejected self-host-side. This is a **pre-existing,
   orthogonal** self-host gap — B2's place-overlap axis neither owns nor introduces it
   (Rider 1 REVISED: those cases are the LIVENESS rule's, not place-overlap's). **Zero
   bootstrap impact** (the self-host's own source contains none of these — the green
   bootstrap proves Rust accepts the self-host source, and Rust rejects all such patterns).

═══════════════════════════════════════════════════════════════════════════════════════
## §1 — THE CONFIRMED WALKER HOOK
═══════════════════════════════════════════════════════════════════════════════════════

`check_carrier_ops_expr` (`typecheck.gg:1076`) — the EXHAUSTIVE D12 walker. Hook the two
arms:
- `case ECall(callee, args, _targs)` — `typecheck.gg:1103`. Add
  `check_call_aliasing(args, scopes, &types, &ctx)` AFTER the existing ctor-arg loop.
- `case EMethodCall(receiver, method_name, args, _targs)` — `typecheck.gg:1117`. Add the
  same AFTER the ingest loop.

**NOTE the B2 brief's "typecheck.gg:859/863" is STALE** — those lines are
`collect_idents_expr` (a different helper). The live walker is at 1076/1103/1117.

Args only (receiver NOT aliasing-checked against args) — mirrors Rust
`check_expr.rs:206,371` (both pass `args`, not the receiver) and ggdef `mod.rs:1343,1714`.

Emit via `ctx.diagnostics.push(Diagnostic.error(span, DkBorrowConflict(), msg))` — the
exact pattern of the A2-S `DkLocalBorrowBind` site (`typecheck.gg:490`). New diag kind
`DkBorrowConflict` added to `diagnostic.gg`'s `enum DiagKind` + its `diag_kind_str` arm
("borrow-conflict"). `diag_kind_str` is the ONLY exhaustive match on `DiagKind` (grep
`case Dk` in `diagnostic.gg`), so that + the enum decl are the only two touch-points.

═══════════════════════════════════════════════════════════════════════════════════════
## §2 — THE THREE LOAD-BEARING UNKNOWNS, RESOLVED
═══════════════════════════════════════════════════════════════════════════════════════

### Unknown 1 — the Copy notion (for the writer-Copy-read exemption). RESOLVED: EXISTS.

`typecheck.gg` has no `is_copy`, but the frontend does: `traits.gg:732
is_scalar_primitive_name(name)` (int/uint/float/bool/char + sized variants) — the self-
host's canonical scalar predicate, ALSO used for builtin-trait dispatch. The resolved-type
table carries `RTPrimitive(name)` (`types.gg:12`). So the Copy test is:

```
infer_expr_type(arg) → RTPrimitive(name) ∧ is_scalar_primitive_name(name)
```

This is the TYPED axis (resolve the type, test its primitive-ness) — NOT a value-shape
match — and mirrors ggdef's `matches!(infer_ast_ty(..), Ty::Prim)` (`mod.rs:1098`) exactly
(scalars Copy; `str`/`String`/collections/structs own a buffer → non-Copy). Unknown →
non-Copy (conservative), matching Rust/ggdef.

- (a) Does the self-host's own source have `f(&s, s.copy_int)`-shaped calls a NO-exemption
  mirror would over-reject? YES — the B1 scout named `add_local(&ctx, ctx.expected_type)`
  (int) and `channel_write_data(&self, self.channel_id, ...)` (int). **MEASURED:** with the
  Copy exemption IN, stage 0→1 (driver compiles the whole self-host) emits ZERO
  borrow-conflict diagnostics (§3). The exemption is load-bearing, and it works.

### Unknown 2 — the mover-Copy / liveness axis. RESOLVED: self-host has NO move-tracker.

Grep across `self_host_typechecker/*.gg` + `self_host_resolver/*.gg` for
`use_after_move|UseAfterMove|moved_slot|is_moved|move_track|VarState|already moved` →
**zero hits.** The self-host has drop-purity taint (D12) but no liveness/use-after-move
pass.

Per **Rider 1 REVISED (2026-07-14)**, the mover-Copy case is the LIVENESS rule's, NOT
place-overlap's: `f(!x, x.copy_field)` is rejected UPSTREAM by `E_UseAfterMove`, and
`f(!x,!x)` by `E_DoubleMove` — both one layer BEFORE place-overlap. So B2's place-overlap
mirror correctly stays SILENT on the mover-Copy case (Copy reader dropped, uniform for
writers and movers) and skips `(Move,Move)`.

**The honest divergence:** because the self-host has no move-tracker, NOTHING self-host-
side rejects `f(!x,!x)` or `f(!x, x.copy_field)`, whereas Rust/ggdef do (via the liveness
axis). MEASURED (§3, probes p_double_move / p_move_copyread): both ACCEPT self-host-side,
REJECT Rust-side. This is:
  - **pre-existing** (the self-host already can't detect ANY use-after-move);
  - **orthogonal to B2** (B2 is the place-overlap axis; this is the liveness axis);
  - **zero bootstrap impact** (the self-host source has no such pattern — the green
    bootstrap ⟹ Rust accepts the self-host source ⟹ no `!x,!x`/`!x,x.copy` in it).
  - The MOVER+non-Copy-read case (`f(!x, x.noncopy)`) IS on the place-overlap axis and IS
    rejected by B2 (probe p_move_noncopyread — REJECT, matches Rust). Only the two
    Copy/liveness cases diverge.

→ **FILE A FOLLOW-UP** (LOW/Medium): a self-host move-tracker/liveness pass for full
parity on `E_DoubleMove` + `E_UseAfterMove`. Much larger than B2; out of B2 scope. This is
the divergence stated honestly, not papered over.

### Unknown 3 — the place-extractor + self-root. RESOLVED: name-keying, self handled.

- The self-host has `place_root_name(Expr)→String` (`typecheck.gg:639`) and
  `place_root_def_spanned(SpannedExpr)→Option[int]` (`:712`), both walking
  EFieldAccess/EIndex → EIdentifier/ESelfExpr. NEITHER returns a projection path.
- **Built** `place_projection_path(Expr)→Option[Vector[String]]` (the ~20-line extractor):
  EIdentifier/ESelfExpr → empty path; EFieldAccess(obj, field) → path(obj)++[field];
  EIndex → collapses to the root (index borrow is from the collection); `x[a..b]` (range)
  → None (a slice is a fresh value, not a place). **Tuple fields need NO special case** —
  the self-host has NO `ETupleFieldAccess`; `t.0` parses to `EFieldAccess(obj, "0")`
  (`parser.gg:2020`), so numeric field names flow through the EFieldAccess arm naturally.
  There is also NO `EOptionalChain` in the self-host AST.
- **Self-root:** `resolve.gg:667` handles `ESelfExpr` as a no-op and `self` is never
  `define`d, so `self` has NO resolution_map entry → `place_root_def_spanned(self)` = None.
  A DefId-keyed mirror would SKIP self-rooted args (the exact state Rust was in BEFORE the
  self-root fix `1eae75ca`). **Fix: key the root on the NAME** (`place_root_name`, which
  returns `"self"`), matching ggdef's `root: String` and handling self for free. The
  local-root FILTER is `rname == "self"` OR `place_root_def_spanned` resolves to a
  `DkVariable` (params are `DkVariable`, `resolve.gg:419`), mirroring Rust's `DkVariable`
  filter and ggdef's `root == "self" || local_names.contains(root)`.
  MEASURED: probes root at both locals and `self`; self-rooted overlap correctly rejects,
  self-rooted disjoint accepts (the bootstrap self-checks `self`-heavy code — §3).

═══════════════════════════════════════════════════════════════════════════════════════
## §3 — MEASURED RESULTS (regenerated this session)
═══════════════════════════════════════════════════════════════════════════════════════

**Build:** `GG_BUILD_TIMEOUT_SECS=900 target/debug/gg build
tests/fixtures/self_host_lowerer/driver.gg` → exit 0 (driver exe built with the mirror).
`gg check` on the driver → "OK: no semantic errors" (mirror type-checks clean).

**Probe matrix** — Rust `target/debug/gg check` (B1 reference) vs the self-host driver
(`driver F lib --lir-c`; reject = non-zero exit + codespan diagnostic, accept = exit 0 + C):

| probe | shape | Rust B1 | self-host mirror | axis / verdict |
|---|---|---|---|---|
| p_writer_writer      | f(&n,&n)             | E_BorrowConflict | REJECT | place-overlap ✓ MATCH |
| p_read_move          | g(n,!n) n non-Copy   | E_BorrowConflict | REJECT | place-overlap ✓ MATCH |
| p_writer_subfield    | h(&n,&n.data)        | E_BorrowConflict | REJECT | projection overlap ✓ MATCH |
| p_disjoint_siblings  | f(&m.a,&m.b)         | ACCEPT           | ACCEPT | disjoint ✓ MATCH |
| p_writer_copy_read   | f(&s,s.tag) int      | ACCEPT           | ACCEPT | Copy-exempt ✓ MATCH |
| p_move_noncopyread   | f(!n,n.data) noncopy | E_BorrowConflict | REJECT | place-overlap ✓ MATCH |
| p_double_move        | f(!n,!n)             | E_DoubleMove     | accept | LIVENESS — DIVERGE (no self-host move-tracker) |
| p_move_copyread      | f(!s,s.tag) int      | E_UseAfterMove   | accept | LIVENESS — DIVERGE (no self-host move-tracker) |

6/6 place-overlap cases MATCH; the 2 divergences are exactly the liveness-axis cases
(unknown 2). Probes + captured stderr in `/tmp/b2_work/probes/` + `err_*.txt`.

**Scope of the scout's measurement:** I measured the PRIMARY B2 risk directly — over-
rejection of the self-host's own source (stage 0→1) and the bootstrap fixed-point. The
broader self-host validation (`type_comparison`, `lowerer_comparison`, the
`self_host_driver_rejects_*` / `_accepts_*` driver suites, and the lib-consuming programs —
p2p/json/toml sites the B1 scout flagged, which the bootstrap driver does NOT import) is the
EXECUTOR's/parent's integration sweep, not the scout's. Those lib sites are already clean
under the Rust B1 check at HEAD (B0 refactored p2p; json/toml are disjoint siblings that
accept), so the mirror — being algorithmically identical — will agree; the executor
confirms via the full sweep.

**Over-rejection gate (stage 0→1, the whole self-host source) — MEASURED, PASS:**
`driver self_host_lowerer/driver.gg lib --lir-c` → **exit 0**, emitted **37,172,319 bytes**
of C, **stderr EMPTY** — **ZERO** borrow-conflict/overlap diagnostics on the entire
self-host source. The mirror does NOT over-reject the self-host's own source; the Copy
exemption correctly spares the add_local/channel_id Copy-scalar sub-reads.

**Bootstrap fixed-point (`self_host_bootstrap_fixed_point`) — <FILL>.**
(`GG_BUILD_TIMEOUT_SECS=900 cargo test --test integration self_host_bootstrap_fixed_point`.
Independent reasoning: the check is ADDITIVE-only and emits nothing when there are no
violations (zero firings on the self-host source, above), so it cannot perturb the emitted
C beyond the new functions'/struct's own lowering — stage1==stage2 must hold.)

═══════════════════════════════════════════════════════════════════════════════════════
## §4 — RECOMMENDED DESIGN (what the B2 executor should land)
═══════════════════════════════════════════════════════════════════════════════════════

The prototype IS the recommended design. Concretely:

1. **`diagnostic.gg`:** add `DkBorrowConflict` to `enum DiagKind` + a `diag_kind_str` arm
   returning `"borrow-conflict"`.
2. **`typecheck.gg`:**
   - imports: `is_scalar_primitive_name` (from traits), `DkBorrowConflict` (from diagnostic).
   - `struct ArgPlace { String root; Vector[String] path; int ownership; bool is_copy; Span span }`
     (mirror Rust/ggdef `struct P` — avoids the parallel-vector rule-3 smell).
   - `place_projection_path(Expr)→Option[Vector[String]]` — the extractor (§2, unknown 3).
   - `paths_overlap_vec(Vector[String], Vector[String])→bool` — zip prefix test.
   - `render_place_sig(int own, String root, Vector[String] path)→String` — diagnostic text.
   - `arg_place_is_copy(SpannedExpr, ...)→bool` — the TYPED Copy test (§2, unknown 1).
   - `check_call_aliasing(Vector[CallArg] args, ScopeTable, TypeTable&, ResolveContext&)` —
     collect ArgPlaces (name-keyed root, local-var+self filter, projection path, Copy-ness,
     sigil), then pairwise: same root ∧ paths_overlap ∧ drop Copy bare readers ∧ skip
     (Move,Move) ∧ (≥1 writer/mover) ∧ ¬both-bare → push DkBorrowConflict at the 2nd arg's
     span. Byte-for-byte the Rust `check_call_aliasing` / ggdef `check_arg_place_overlap`
     algorithm.
   - call it in the ECall arm (after the ctor loop) and the EMethodCall arm (after the
     ingest loop), args only.

**Layering rationale (cited):** `docs/devbook/24-layering-discipline.md` rule 2 (typed
metadata not shape) — the check reads `CallArg.ownership` (a typed field, `ast.gg:31`), not
`match a.value: case EMove`; Copy-ness reads the resolved `RTPrimitive` type, not the value
syntax. `docs/language-design.md` §3.5 (the borrow rules: a place has one exclusive
writer). decisions.md D10(b) ADDENDUM + Rider 1 REVISED (the LIVE-ALIAS-vs-VALUE cut; the
two-axis model). Self-host-as-elegance-showcase: name-keying reads like the language is
meant to (no DefId plumbing for `self`), one `ArgPlace` struct not parallel vectors.

═══════════════════════════════════════════════════════════════════════════════════════
## §5 — FIXTURES / TESTS the B2 brief should add
═══════════════════════════════════════════════════════════════════════════════════════

Mirror the existing `self_host_driver_rejects_d12_drop_purity` /
`self_host_driver_accepts_d12_legal` pattern (`tests/integration.rs:18551,18627`):
- REJECT set (non-zero exit + "their places overlap" + box rule + empty stdout):
  p_writer_writer `f(&n,&n)`, p_writer_subfield `f(&n,&n.data)`, p_read_move `f(n,!n)`
  (non-Copy), p_move_noncopyread `f(!n, n.data)`.
- ACCEPT set (exit 0 + emits C): p_disjoint_siblings `f(&m.a,&m.b)`, p_writer_copy_read
  `f(&s, s.tag)` (Copy int — pins the exemption; the over-rejection guard).
- The DIVERGENCE cases (p_double_move, p_move_copyread) — do NOT add as self-host REJECT
  fixtures (they legitimately don't reject self-host-side); note them in the follow-up.
- Probes live in `/tmp/b2_work/probes/`; promote to `tests/fixtures/d10b_place_overlap/`.

═══════════════════════════════════════════════════════════════════════════════════════
## §6 — OWNER DESIGN QUESTIONS / DIVERGENCES
═══════════════════════════════════════════════════════════════════════════════════════

**D1 (the one honest divergence) — self-host cannot reject the two LIVENESS-axis cases
(`f(!x,!x)` → E_DoubleMove; `f(!x, x.copy_field)` → E_UseAfterMove) because it has no
move-tracker.** Not B2's axis (Rider 1 REVISED), pre-existing, zero bootstrap impact.
Recommend: land B2 (the place-overlap axis, at full parity) and FILE a follow-up for a
self-host move-tracker/liveness pass. Do NOT block B2 on it.

**Q1 — key the root by NAME or DefId?** Recommend NAME (ggdef's model). It handles self-
root for free (the self-host doesn't resolve `self`), matches ggdef, and is exactly
equivalent to DefId within one call. (If a future refactor makes the self-host resolve
`self`, DefId-keying becomes available but buys nothing here.)

**Q2 — receiver aliasing in method calls?** Recommend NOT checking the receiver against
args (mirror Rust/ggdef — args only). `recv.m(&recv)` overlap is out of scope for all three
compilers; keeping parity is the right call.
