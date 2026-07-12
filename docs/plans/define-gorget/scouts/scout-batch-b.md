# Scout Report — Batch B: D10(b) same-call PLACE-OVERLAP rejection

Read-only scout. No production code committed (prototype built, measured, REVERTED —
`git status` clean except this report). Date 2026-07-12. Worktree agent-a603620f8dcdea51b
(based on gorget-1, ff up to date). Scoped from CURRENT (pre-A2-S) state; A2-S lands first,
D10(b) integrates on top of the same self-host walker (see §3.2).

═══════════════════════════════════════════════════════════════════════════════════════
## TL;DR — the five headline findings
═══════════════════════════════════════════════════════════════════════════════════════

1. **The premise "D10(b) is UNIMPLEMENTED / both compilers ACCEPT `f(&x,&x)` / `f(&x,x)`"
   is WRONG.** `check_call_aliasing` (`src/semantic/safety/helpers.rs:1124-1186`) ALREADY
   rejects `f(&x,&x)`, `f(&x,x)`, `f(&x,!x)` (E_BorrowConflict), and `f(!x,!x)`
   (E_DoubleMove). **D10(b) is an EXTENSION of an existing partial check, NOT greenfield.**
   §9.4 of `docs/language-reference.md` already specs it (but incompletely — it too is
   missing the D10 arms). Measured — §1.

2. **The genuinely-accepted (miscompiled) gaps are exactly two shape-classes:**
   - **Sigil-arm gap:** `(Borrow, Move)` missing from the matrix → `f(x, !x)` ACCEPTED.
     **Zero in-repo occurrences** — fixture-only close.
   - **Place-shape gap:** the check collects only `Expr::Identifier` args (keyed on DefId),
     so field/index args are invisible → `f(&node, &node.field)`, `f(node.field, &node)`
     ACCEPTED. This is the p2p double-writer + the self-host tc_types/trait_registry family
     + (harmlessly) the Copy-scalar sub-reads. Measured — §1, §3.

3. **THE BORROW-PROVENANCE BIT (option A) IS NOT NEEDED FOR D10(b) — recommend (B).**
   The parser already lifts each call-arg sigil into typed metadata `CallArg.ownership`
   (`src/parser/expr.rs:1996,2032`); `arg.node.value` is the bare place. D10(b) reads
   the typed sigil + the EXISTING `find_root_def_id_with_path` place primitive
   (`helpers.rs:458`) — NO shape-walk, NO sibling-drift. The A3 sibling-drift fragility is
   specific to VALUE-position `&` (`Expr::MutableBorrow` in `auto r=&a`, `[&a]`, if/match
   tails) which has no CallArg sigil. Building the bit is a moderate-HIGH-risk type-inference
   change (§2) with ~0 payoff for D10(b). **Defer it as a separate optional cleanup for the
   value-position borrow-rule family (A3 walker + no-op-`&` / TODO:248).** — §2, §5-Q1.

4. **Projection-aware keying is LOAD-BEARING.** ~30 self-host arg-pairs pass DISJOINT
   `&`-fields of one struct (`drop_fn_for_type(&gmod.resource_types, &m.type_runtime_map,
   …)` `lir_lower.gg:1296` etc.) and lib json/toml/xml/yaml pass `X.pos` + `!X.err`
   (disjoint). Root-only keying would wrongly reject all of them. The check MUST compare
   projection PATHS (overlap = one path is a prefix of the other). Prototype confirmed both
   the rejects AND the disjoint-accepts — §1c, §3.

5. **The Copy-read exemption is the pivotal OWNER decision** (§5-Q2). Several overlaps read
   a COPY scalar sub-place under an overlapping `&whole` — `add_local(&ctx, ctx.expected_type)`
   (int), `channel_write_data(&self, self.channel_id, …)` (int). A Copy read snapshots an
   independent value at the call (no live alias, no CoW divergence). **Recommend EXEMPT.**
   If exempted, those sites need NO fix; if pure-syntactic, they need hoists.

**Corrected hand-hoist counts** (§3.3): the real non-Copy sites are the *tc_types /
trait_registry* families, NOT the "add_local family" the TODO cites (that one is
Copy-exempt). Under the recommended Copy-exempt design: **lib = 2** (both p2p
double-writers, a real restructure), **self-host = 8 call sites across 4 function
refactors** (resolved_to_gir_type ×1; resolve_method_full / resolve_method_for_generic_
receiver / substitute_shape_return_generic_receiver ×3), **fixtures = 2 existing negatives
extended**. Under pure-syntactic: lib = 3 (+ssh), self-host ≈ 12 (+add_local Copy sites).

═══════════════════════════════════════════════════════════════════════════════════════
## §1 — PREMISE VERIFICATION (measured, cited)
═══════════════════════════════════════════════════════════════════════════════════════

### 1a. Current behavior — 6 probe programs, `gg check`/`build`+run, gg @ HEAD debug

| Probe | Shape | `gg check` result | build+run |
|-------|-------|-------------------|-----------|
| t1 | `f(&n, &n)` | **REJECTED** E_BorrowConflict "borrow mutably more than once" | — |
| t2 | `f(&n, n)`  | **REJECTED** E_BorrowConflict "bare and mutable borrow" | — |
| t3 | `f(n, !n)`  | **ACCEPTED (GAP)** | builds, prints 5, exit 0 |
| t4 | `f(!n, !n)` | REJECTED — but via **E_DoubleMove** (move-tracker, not the aliasing check) | — |
| t5 | `f(&n, &n.field)` | **ACCEPTED (GAP)** | prints **1** — write through `&n.field` SILENTLY LOST (should be 2) |
| t6 | `f(n.data, &n)` | **ACCEPTED (GAP)** | prints 5, exit 0 |

Runtime consequence, measured under `--sanitize` (ASan+UBSan): NO crash/UAF on t3s (heap
Vector)/t5/t6. This CONFIRMS D10's own characterization ("not Rust-style lifetime safety;
ggdef cannot even observe them"): these are **value-semantics (lazy/eager CoW) divergences**,
not hard memory-unsafety. t5 is the sharpest reference-grade defect: `f(&n, &n.field)`
compiles and **silently drops** the write through `&n.field` (prints 1, not 2) — accepted-
but-should-reject, silently-wrong-value (Core invariant #8). Probes in scratchpad/t*.gg.

### 1b. The existing conflict matrix + its two gaps (`helpers.rs:1146-1185`)

`check_call_aliasing` collects only `Expr::Identifier` args, keys on DefId equality:

| pair | current | D10 wants | gap |
|------|---------|-----------|-----|
| (&,&) · (&,bare) · (&,!) | conflict | conflict | ok |
| (!,!) | not here (E_DoubleMove elsewhere) | conflict | cosmetic (message only) |
| **(bare,!) / (!,bare)** | **NOT conflict** | conflict | **GAP → `f(x,!x)`** |
| (bare,bare) | ok | ok (two readers) | ok |
| **field/index args** | **never collected** | keyed on root+projection | **GAP → `f(&x,&x.f)`** |

Called at Call (`check_expr.rs:206`) and MethodCall (`:371`). The `f(b.data,&b)` "miss" and
the `f(v,!v)` accept named in the D10 ruling (`decisions.md:518-530`) are exactly these two.

### 1c. Prototype (option B) — built, measured, reverted

Replaced `check_call_aliasing`'s body with projection-prefix keying (reuse
`find_root_def_id_with_path`; overlap = `path_a.iter().zip(path_b).all(==)`; conflict =
overlap ∧ (a or b is `&`/`!`) ∧ ¬both-bare). ~40 net lines, `cargo build` clean.

Probe results with prototype: t1 reject · t3 **now reject** · t4 reject · t5 **now reject**
· t6 **now reject** · **t7 `f(&m.a,&m.b)` (disjoint siblings) ACCEPT ✓**. The projection-
prefix key does the right thing on both sides. (Then reverted; tree clean.)

### 1d. ggdef cross-check

`grep` of `spec/ggdef/src/` finds NO same-call aliasing / place-overlap model (only a
D10(a) move-bind comment at `tests.rs:1611`). Confirms A29: ggdef is value-semantics and
cannot observe exclusivity violations → D10(b) is **static-only in the two production
compilers; ggdef gets prose + fixtures**, no executable-model change.

═══════════════════════════════════════════════════════════════════════════════════════
## §2 — BORROW-PROVENANCE BIT (option A) — PRIMARY DELIVERABLE, MEASURED
═══════════════════════════════════════════════════════════════════════════════════════

**Root-cause note being evaluated (TODO:247):** `Expr::MutableBorrow`/`Expr::Move` both
return `infer_expr(inner)` unchanged — `src/semantic/typecheck.rs:2924-2927`, comment
"ownership modifiers don't change the type". So a value-position `&` carries no typed
is-borrow signal, forcing syntactic shape-walks (A3's `expr_is_borrow_bind`).

### 2a. The bit's representation ALREADY EXISTS — this is not a new type feature

`ResolvedType::Ref(TypeId)` and `ResolvedType::Owned(TypeId)` already exist
(`src/semantic/types.rs:54,57`). `infer_expr` ALREADY produces `Ref` for borrow-view
results (collection reads `typecheck.rs:679`, coercion `:1039-1043`, …) and **38 code sites
already peel/handle `ResolvedType::Ref|Owned`** (deref, unification, describe). So "the bit"
is not greenfield — the type lattice already carries borrow-provenance. The ONLY gap is the
deliberate choice at `:2924-2927` to NOT wrap `&x`/`!x` in `Ref`/`Owned`.

### 2b. Real cost of flipping it (making `infer(&x) = Ref(inner)`)

Measured surface: **69 `Expr::MutableBorrow`/`Move` mentions across `src/`**; the change
would alter the type of EVERY `&x`/`!x` expression from `T` to `Ref[T]`/`Owned[T]`,
disturbing:
- **Type equality / coercion at call sites** — `f(&x)` to a bare-`T` param currently
  type-matches because `&x : T`; with `Ref[T]` every call-arg check, unification, and
  `param_ownerships` comparison must peel first (the comment at `:2926` exists precisely to
  avoid this).
- The **38 existing Ref/Owned arms** would need an audit to confirm none double-peels.
- A **self-host mirror** (`infer_expr_type` in `infer.gg` + every consumer) — the self-host
  type table has the same Ref/Owned notion; the change doubles.

Verdict: **moderate-to-HIGH risk, load-bearing (type inference of every borrow expr), and
it needs the twin self-host change.** This is a design-weight change on its own.

### 2c. Payoff for D10(b): ZERO. For the value-position family: a nice-to-have, not needed.

- **D10(b) does not touch value-position borrows.** Call-arg sigils are already typed:
  `parse_call_arg` (`expr.rs:1996`) runs `parse_ownership_modifier()` BEFORE `parse_expr`
  and stores it at `:2032`, so `&node.disc_socket` parses to `CallArg{ownership:
  MutableBorrow, value: <node.disc_socket>}` — sigil typed, value is the bare place. The
  204 `.ownership` readers already consume this. D10(b) needs (sigil, place); both are
  already typed data. **No bit required.**
- **The no-op-`&` value-position family (TODO:248)** and **A3's `expr_is_borrow_bind`
  walker** are where value-position `&` lives. The bit COULD collapse A3's If/Match/Do/Block
  walker into "is the RHS type a `Ref`?" — a genuine cleanup. But those families are equally
  well served by a *syntactic rejection* of `Expr::MutableBorrow` in value positions (reject,
  don't type), which is lower-risk than perturbing inference.

**Recommendation (the (A)/(B)/(C) fork):** **(B) for D10(b)** — extend `check_call_aliasing`
using the already-typed `CallArg.ownership` + `find_root_def_id_with_path`. Treat the
borrow-provenance bit as a **separate, optional, later cleanup track** owned by the
value-position borrow-rule family (A3 walker retirement + no-op-`&`), to be evaluated on
its own cost/benefit — NOT a D10(b) prerequisite, and possibly superseded by targeted
syntactic rejections. This is option (C) at the wave level: clean (B) now, bit deferred.

═══════════════════════════════════════════════════════════════════════════════════════
## §3 — DESIGN PROPOSAL
═══════════════════════════════════════════════════════════════════════════════════════

### 3.1 The place-overlap rule (both compilers)

For each call (`Call` and `MethodCall`), for each argument, compute:
- **sigil** ∈ {reader(bare), writer(`&`), mover(`!`)} — Rust: `arg.node.ownership`;
  self-host: match the arg expr `EMutableBorrow`→writer / `EMove`→mover / else reader.
- **place** = (root binding, projection path) of the (sigil-unwrapped) arg, ONLY when the
  arg is a place (identifier/field/tuple-field/index chain); non-places (fresh temps,
  literals, `x.clone()`, `x.get()`) have no place and are skipped (they cannot alias).

Two args **overlap** iff same root AND one projection path is a prefix of the other
(`zip(path_a,path_b).all(==)`). Disjoint sibling fields (`x.a` vs `x.b`) do NOT overlap.
Index/optional-chain segments collapse to the collection root (conservative: any `x[i]`
overlaps `x`) — this is exactly what `find_root_def_id_with_path` already does
(`helpers.rs:478-480`).

**Conflict** = overlap ∧ at least one arg is a writer/mover ∧ not-both-readers, **subject to
the Copy-read exemption (§5-Q2, recommended):** a bare reader of a **Copy-typed** place does
not conflict (it snapshots an independent value; no live alias). Emit `E_BorrowConflict`
(reuse — extend the `detail` strings for the new arms).

Rust hook: extend `check_call_aliasing` (prototyped, §1c). Self-host hook: §3.2.

### 3.2 Self-host integration (`tests/fixtures/self_host_typechecker/typecheck.gg`)

The walker `check_carrier_ops_expr` already visits `case ECall(callee, args, …)` and
`case EMethodCall(receiver, _m, args, …)` (`typecheck.gg:623-630`). Add the aliasing check
in those two arms. Needs a NEW ~15-line place-extractor (root+path over
`EFieldAccess`/`ETupleFieldAccess`/`EIndex` → `EIdentifier` root) — none exists on the
self-host side yet — plus the ~25-line pairwise check. Sigil is the arg's `EMutableBorrow`/
`EMove` wrapper (typed variant, `ast.gg:77-78`). Emit via `ctx.diagnostics.push(...)` with a
new `DkBorrowConflict()` diagnostic kind (mirror the A3 `DkLocalBorrowBind` pattern,
`typecheck.gg:480`). **Sequencing: A2-S lands its D12 drop-purity into this same walker
FIRST (per the orchestrator note); D10(b) adds a sibling check in the same two arms — no
conflict, but rebase onto A2-S.**

### 3.3 The in-repo hand-hoists (MUST land WITH or BEFORE the check — bootstrap/tests gate)

Measured by the prototype (each site confirmed to newly-error) + the same-root scanner.

**Lib — 2 genuine sites (both p2p double-writers, a REAL FIX not a hoist):**
`p2p_poll_socket(Node &node, UdpSocket &sock)` (`lib/xtd/p2p.gg:1776`) is called
`p2p_poll_socket(&node, &node.disc_socket)` (`:2057`) and `p2p_poll_socket(&node,
&node.socket)` (`:2067`). `sock` is ALWAYS a field of `node`, and the body both
`sock.recvfrom(…)`-mutates the socket AND `p2p_update_peer(&node,…)`/`p2p_send_raw(&node,…)`-
mutates node (which contains the socket) — two live mutable borrows of overlapping places.
You CANNOT hoist a `&`-borrow of a field into a local (D10(a) forbids `&`-binds). **Fix
(recommended): drop the `sock` param; pass a socket SELECTOR** (`bool use_disc` or a small
enum) and do `node.disc_socket.recvfrom(…)` / `node.socket.recvfrom(…)` inside — the
recvfrom then reads node.<socket> and the later `&node` mutations are field-disjoint
*within one function* (sequential, gg allows it). Alt: split into a call-site recvfrom +
`p2p_handle_packet(&node, pkt)`. Verify with `p2p_basic` (prototype: both sites errored).

**Self-host — 8 call sites, 4 function refactors (the tc_types / trait_registry families):**
Two occurrences of the SAME structural shape — *a function reads a struct's sub-table while
registering into other fields of the same struct*, spelled `f(struct.subtable, &struct)`:
- `resolved_to_gir_type(int rtid, TypeTable types, ScopeTable scopes, GirModule &gmod)`
  (`lower_types.gg:383`), called `(…, gmod.tc_types, gmod.tc_scopes, &gmod)` at
  `lower_types.gg:561,568,595` and `lower_generics.gg:105`. `&gmod` genuinely mutates gmod
  (`lookup_or_register_named(&gmod)`, `register_ptr`, `record_enum_category` …) but only
  gmod's *GIR* fields — never `tc_types`/`tc_scopes`. So the code is field-disjoint-SAFE;
  root+projection keying flags it because `&gmod` (whole) overlaps `gmod.tc_types` (sub).
- `resolve_method_full(TraitRegistry registry, …, TypeTable &types)` (`traits.gg:658`) +
  siblings `resolve_method_for_generic_receiver`, `substitute_shape_return_generic_receiver`,
  called `(types.trait_registry, …, &types)` at `infer.gg:276,303,382` and `typecheck.gg:1133`.
  Same shape: reads `types.trait_registry`, mutates other `types` fields via `&types`.

**Fix (recommended): change each signature to take ONLY `&gmod` / `&types` and read
`gmod.tc_types` / `types.trait_registry` INTERNALLY** (drop the redundant sub-table params —
they were the struct's own fields all along; this is also MORE idiomatic per "self-host as
elegance showcase"). The read then lives inside the function, where reading `gmod.tc_types`
while calling a `&gmod`-mutating helper on a disjoint field is permitted (sequential
statements). **RISK (medium): must confirm gg's within-function borrow checker accepts
reading `gmod.tc_types` in the same body that mutates `gmod` — likely yes (different
statements, field-disjoint), but verify on the branch; if it trips, fall back to threading
the sub-tables as separate top-level bindings destructured before the loop.** This is the
single biggest risk/effort in Batch B and the reason the self-host slice is real work, not a
mechanical hoist.

**Copy-exempt sites (NO fix under the recommended Copy exemption; hoist only if
pure-syntactic):** `add_local(&ctx, ctx.expected_type)` (`lower_expr.gg:5002`, int),
`add_local_inheriting(&ctx, ctx.locals.get(…).type_id, …)` (`lower_match.gg:743,1015,1212` —
also not a place: method chain → Copy int), `channel_write_data(&self, self.channel_id, …)`
(`ssh.gg:633`, int). The TODO's "self-host 8 = the add_local family" is a MIS-CITE: the
add_local sites are the Copy-exempt outliers; the real 8 are tc_types/trait_registry.

**Disjoint-sibling sites that must KEEP passing (regression guard, do NOT touch):**
`drop_fn_for_type(&gmod.resource_types, &m.type_runtime_map, &gmod.optionlike_resource_types,
&m.drop_collision_types, &m.type_method_prefix_map)` (`lir_lower.gg:1296,1352,1355,4639,
4751,4872,5040,5500,5563` — ~30 arg-pairs) and lib `ParseError.InvalidSyntax(p.pos, !p.err)`
(`json.gg:515`, `toml.gg:1522`, `xml.gg:358`, `yaml.gg:1269,1276`). Prototype confirmed
these pass (projection-prefix keying). Add a fixture pinning disjoint-sibling ACCEPT so a
future coarsening can't regress them.

### 3.4 Fixtures (per-arm negatives + positives)

Extend the 2 existing negatives (`borrow_conflict_error.gg`, `double_mutable_borrow_error.gg`
— both `f(&v,&v)`, already rejected) and ADD per D10-arm:
- NEG `f(x, !x)` (bare+move — the sigil-arm gap; t3).
- NEG `f(&node, &node.field)` (projection overlap, both writers; t5) + wire the run-twin
  asserting the CORRECT value (proves the silent-lost-write is fixed).
- NEG `f(node.field, &node)` (reader-of-subplace + writer-of-whole; t6).
- POS `f(&m.a, &m.b)` (disjoint siblings ACCEPT; t7) — the regression guard.
- POS (if Copy-exempt adopted) `f(&s, s.copy_int)` ACCEPT (t8) — pins the exemption.
Mirror the negatives into the self-host lane's expectations. ggdef: prose in the exclusivity
section + fixtures per A29 (no model change).

### 3.5 Docs write-through (mandatory per project decision)

`docs/language-reference.md §9.4 Same-Call Aliasing` (`:2328`) is the target — it is STALE:
its table lists only `f(&x,&x)` / `f(x,&x)` / `f(&x,!x)` and says "`f(x,x)` allowed for Copy
types". Add the `f(x,!x)` arm, the projection-overlap rule (with the disjoint-sibling
carve-out), and the Copy-read exemption note. `§3.5 The Borrow Rules` (`:580`) already states
the exclusivity principle. Cite D10.

═══════════════════════════════════════════════════════════════════════════════════════
## §4 — RECOMMENDED SLICING, SIZE/RISK, BOOTSTRAP-GATING
═══════════════════════════════════════════════════════════════════════════════════════

Batch B is **bootstrap-gated** (self-host hoists must land so the fixed-point stays green).
Recommend **THREE briefs**, sequenced; the borrow-provenance bit is NOT one of them.

- **B0 — the hand-hoists FIRST (own brief, lands before the checks).** p2p ×2 selector
  refactor + the 4 self-host function refactors (tc_types/trait_registry). This is the real
  work + the real risk (the within-function-borrow question, §3.3). Gate: `p2p_basic` +
  `self_host_bootstrap_fixed_point` GREEN on the *unchanged* checker (the hoists are pure
  refactors, must be behavior-preserving). Medium size, medium risk.
- **B1 — Rust place-overlap check + fixtures + §9.4 docs.** Extend `check_call_aliasing`
  (prototyped, ~40 lines) with the Copy exemption; add the per-arm fixtures + the
  disjoint-sibling POS guard; rewrite §9.4. Small-medium, LOW risk (prototype proven).
  Gates on B0 (else lib/self-host builds break). Zero-collateral check: exactly the intended
  fixtures flip.
- **B2 — self-host mirror in `check_carrier_ops` (ECall/EMethodCall arms).** The ~40-line
  walker addition + `DkBorrowConflict` + self-host lane expectations. **Rebases onto A2-S**
  (same walker). Gates on B1 (Rust reference) AND B0 (hoists). Bootstrap-gated: fixed-point
  must converge. Small-medium, low-medium risk.

Out-of-repo (gorget-js 24 sites, arena, gglox, gconf) stays in the **deferred coordination
round** (decisions.md ruling) — B1/B2 land in-repo only; those projects pin their gg version
until the coordination round.

The **no-op-`&` value-position family (TODO:248)** and the **borrow-provenance-bit
evaluation** are a SEPARATE later track (not Batch B) — see §5-Q1/Q3.

═══════════════════════════════════════════════════════════════════════════════════════
## §5 — OWNER DESIGN QUESTIONS (with recommendations)
═══════════════════════════════════════════════════════════════════════════════════════

**Q1 (THE FORK) — build the typed borrow-provenance bit before D10(b)?**
→ **Recommend NO (option B/C).** Measured: D10(b) uses already-typed `CallArg.ownership` +
the existing place primitive; the bit has zero D10(b) payoff and is a moderate-HIGH-risk
type-inference change requiring a self-host twin (§2). Do D10(b) via (B); file the bit as an
independent, optional cleanup for the value-position family (A3 walker + no-op-`&`), where it
competes against simpler syntactic-rejection alternatives.

**Q2 (PIVOTAL) — exempt Copy-typed sub-place reads under an overlapping `&`?**
E.g. `f(&ctx, ctx.expected_type)` (int), `channel_write_data(&self, self.channel_id, …)`.
→ **Recommend EXEMPT.** A Copy read snapshots an independent value at the call — no live
alias, no CoW divergence, nothing for exclusivity to protect. Exempting keeps the rule
semantically honest AND spares the add_local/channel_id hoists. (Pure-syntactic is simpler to
implement and matches the coarse "one writer per place" phrasing literally, but it forces
ugly hoists on provably-safe Copy-scalar reads and would make the self-host uglier — against
"elegance showcase".) §9.4 already gestures at Copy ("`f(x,x)` allowed for Copy types").

**Q3 — the no-op-`&` value-position family (`[&a]`, `{k:&a}`, `(&a,&b)`, `=&BASE`;
TODO:248).** D10(b)'s place rule is about CALL ARGS and does NOT reach these (they are
`Expr::MutableBorrow` nodes in container/tuple/default-value positions, no CallArg sigil).
→ **Recommend: a SEPARATE small rider (not Batch B) that syntactically REJECTS `&` in value
positions** (uniform with the call-site "bare vs `&`" error and with D10(a)'s bind
rejection) — the inconsistency (a value-position `&` silently binds a no-op copy while
`f(&a)` to a by-value param errors) is a real wart. This rider is the natural place to also
evaluate the borrow-provenance bit (Q1), since both concern value-position `&`.

**Q4 — the self-host `f(struct.subtable, &struct)` shape is field-disjoint-SAFE yet
D10(b) rejects it (whole-vs-sub coarseness).** The recommended fix (read the sub-table
inside the function) is idiomatic and removes redundant params, but it is a real refactor
with a within-function-borrow risk (§3.3). → No decision needed if the refactor lands
cleanly (B0's gate); flagging it so the owner knows the self-host slice is genuine work and
that a future *field-level* precision (knowing which fields a `&whole` mutates) could
retire the false-positive class — out of scope for Batch B, notable as a direction.

═══════════════════════════════════════════════════════════════════════════════════════
## Appendix — commands / artifacts
═══════════════════════════════════════════════════════════════════════════════════════
- Probes: `scratchpad/t1..t8*.gg` (+ `t3s.gg` heap UAF check). `--sanitize` runs clean.
- Same-root overlap scanner: `scratchpad/overlap_scan.py` (projection-aware; distinguishes
  disjoint siblings). Run over lib/ + self_host + fixtures.
- Prototype: `check_call_aliasing` replacement (saved diff not committed; original restored
  from `scratchpad/helpers.rs.orig`). `git status` clean except this report.
- Load-bearing citations: `helpers.rs:1124-1186` (existing check), `:458` (place primitive),
  `check_expr.rs:206,371` (call sites), `expr.rs:1996,2032` (parser sigil→CallArg.ownership),
  `types.rs:54,57` (Ref/Owned already exist), `typecheck.rs:2924-2927` (the bit gap),
  `typecheck.gg:623-630` (self-host walker hook), `ast.gg:77-78` (EMutableBorrow/EMove),
  `p2p.gg:1776,2057,2067`, `lower_types.gg:383,561,568,595`, `traits.gg:658`,
  `infer.gg:276,303,382`, `language-reference.md:2328` (§9.4), `decisions.md:518-530` (D10).
