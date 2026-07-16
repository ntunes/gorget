# EXECUTOR BRIEF — CoW-1A: for-loop `&`-iterable element write-through (BOTH-LANE) + gap A2

**Status:** DRAFT v3 — in the ≥3-fresh-pass review gauntlet. Pass 1 (3 reservations: the
enumerate twin gate — reproduced; the comprehension plan-mandate drop; gate gaps) FOLDED into
v2. Pass 2 (all premises re-verified + every gap reproduced on both backends; 3 reservations:
the self-host bare-enumerate cell unmeasured — cross-lane pin risk; the Rust comprehension fix
site unnamed — `collections.rs:~628` missing Ptr-deref; v2 pointed the executor at the WRONG
self-host comprehension site) FOLDED into this v3. Do not execute until the gauntlet records a
clean pass.
**Scout evidence (read FIRST):** `docs/plans/cow-1a-scout.md` (premise table, measured matrix,
prototype design, cross-lane analysis). Prototype patch: `/tmp/cow1a_proto.patch` (backup
`/tmp/recover_cow1a_proto.patch`; 182 lines, self-host only, verified end-to-end, clone-neutral).
**Campaign context:** `docs/plans/cow-writethrough-materialize-closed-set.md` (v3) — 1A is the
wave-1 closer; 1B/1C landed. Semantics authority: `docs/language-design.md` §3.1 +
`docs/devbook/11-copy-on-write.md` + CLAUDE.md "Ownership at Consuming Positions".

## Mission

Land the for-loop element-binding semantics REFERENCE-GRADE and UNIFORM on Rust gg (C+LLVM
shared lowering) AND the self-host, with a ggdef out-of-subset note (Core invariant #9):

- **Bare `for x in coll:`** — the element binding is IMMUTABLE (a materialize-capable borrow);
  a body write to `x`/`x.field` lands in a private materialized copy and does NOT reach the
  collection. Uniform over value AND resource elements.
- **`for x in &coll:`** — the element binding is a WRITE-THROUGH place (pointer into the
  element); body writes reach the collection's backing store. Uniform over value AND resource
  elements. If the collection root is a lazy borrow-alias (`b = a`), the FIRST write-through
  mutation must SEVER the alias (materialize `b`) so `a` is untouched.

This closes **gap A** (value-struct `&` write-through lost — BOTH compilers wrong today),
**gap A2** (Rust bare-for over a RESOURCE element wrongly writes through — self-host correct
today), **the enumerate twins of both** (review pass 1 REPRODUCED the A2-twin:
`for i, c in a.enumerate():` over a bare resource wrongly prints `101` on Rust —
`lower_for_enumerate` carries a twin of the same gate, `for_loops.rs:682-690` + its own `&`
auto-deref `:601-621`), and **the comprehension-over-`&` READ bug** (plan-mandated: TODO's
"`[x*2 for x in &a]` yields an EMPTY vector" HIGH — silent wrong output; review pass 2
REPRODUCED it on BOTH Rust backends, len 0 where §3.1 wants len 3). The measured sites
(pass-2-verified — do not mis-hunt):
- **Rust:** `src/ir/lowering/exprs/collections.rs` `lower_list_comprehension`, the non-range
  branch (~`:628-657`) lowers the `&a` MutableBorrow to a Ptr `iter_type` but — unlike
  `for_loops.rs:171-193` — NEVER auto-derefs it, so element-type inference fails and the loop
  runs 0 iterations. The fix mirrors the for-loop deref; reconcile with the campaign plan's
  "one shared iterable-mode helper, not two parallel fixes" mandate (the statement-for deref
  and the comprehension index-walk are currently separate code).
- **Self-host:** the `lower_expr.gg:~6949` list-comprehension site HAS the iterable EXPRESSION
  available — the strip helper DOES apply there; that is where the self-host READ bug lives (if
  it reproduces on that lane — measure). The comprehension paths that arrive as an
  already-resolved `recv` (the HOF intercepts) are the DEFERRED write facet, not this bug.
Scout + pass-2 measured the core matrix; your acceptance target is the "derived expected"
column extended by the fixtures table below.

**Explicitly DEFERRED (update the campaign plan + TODO to say so):** comprehension element
WRITE-THROUGH (`[… for x in &v]` mutating the element — the write facet of Gap I) and
enumerate-over-`&` write-through. Both get a one-line deferral note; neither may silently
change behavior in this landing.

**Semantics note (review-verified):** the bare-for-element silent-materialize IS the ratified
semantics (`docs/language-design.md` §3.1 ~:317-332 names the `for x in coll` element); the
`DeadBareParamWrite` lint is param-scoped and explicitly excludes for-iteration
(`check_stmt.rs:1736`) — so the bare fixtures compile CLEAN with output `1`; do not expect or
add a diagnostic.

## Hard constraints

1. **Self-host-only or Rust-only landings are NO-GO.** Baseline lanes AGREE (on the wrong
   answer) for gap A; fixing one lane alone diverges them → a parity REGRESSION. Both lanes land
   in THIS track, pinned by the cross-lane fixtures below.
2. **Fix the class, not the cells (Core #1/#4).** Rust: the element binding in
   `src/ir/lowering/stmts/for_loops.rs` (`lower_for` / `lower_for_array`) is today driven by
   value-vs-resource (`is_recursive_struct` gate, scout cites `:487-539`; review pass 1
   confirmed `:478-490`) and IGNORES the `&` mode (`:171-193` auto-derefs). The reference-grade
   fix RESTRUCTURES the element binding to be MODE-driven (bare → materialize semantics; `&` →
   write-through place), uniform over value/resource — **implemented as ONE shared
   element-binding helper used by BOTH `lower_for_array` AND `lower_for_enumerate`** (the
   enumerate path carries a twin gate at `:682-690` + its own deref `:601-621`; mode-driving
   only for_array ships an inconsistent end state). Do NOT bolt a `&`-special-case beside the
   existing gates. Latitude note: for the BARE value-struct path, the existing eager value-copy
   at bind (`:533`) already realizes correct materialize semantics — you may keep it; "uniform"
   constrains observable semantics, not the binding implementation per element class.
3. **Self-host: productionize the prototype, don't just apply it.** Extract ONE shared
   iterable-mode helper (`for_iterable_mode`-shaped: strip `EMutableBorrow`, return
   (inner, write_through)) feeding the stmt-for path AND the `lower_expr.gg:~6949`
   list-comprehension site (the iterable EXPRESSION is available there — that is where the
   self-host comprehension READ fix lands if the lane reproduces the bug; measure first). The
   already-resolved-`recv` comprehension paths (HOF intercepts) stay write_through=false — the
   deferred write facet per the Mission section. Extend the proto beyond the owned-local root:
   non-owned roots (`for c in &self.field`, `&v[i]`, statics) via the 1B ptr-or-borrow base
   idiom. Add a Deque probe. Typed gates only — no name-matching.
   **MEASURE the self-host bare-enumerate-over-resource cell** (`for i, c in v.enumerate():`
   mutating a resource element — expected `1`): the proto's else-branch suggests it already
   materializes, but it is UNMEASURED, and fixture #6 enters the parity corpus — a Rust-only
   enumerate fix with a self-host write-through there would DIVERGE the lanes (constraint 1).
   If the self-host writes through, apply the same materialize treatment.
   ESCAPE HATCH (BOTH lanes): if either comprehension READ fix turns out to be a multi-file
   re-architecture rather than a bounded deref/strip fix, STOP-AND-REPORT with the measured
   shape — do not silently descope and do not balloon.
4. **The alias-root is the hard Rust case — prove the sever.** Self-host currently passes it via
   a pre-existing eager copy (masking the sever path); Rust `b = a` is a 0-clone lazy alias, so
   an unsevered `&b` element write would mutate the SHARED buffer (`101`/`101` — a Core-#8
   write-to-both). The fixture pins `1`/`101`; verify by RUNNING, and inspect the emitted C for
   the sever/materialize call on first mutation.
5. **Before flipping A2 (and its enumerate twin), sweep for dependents:** grep the corpus +
   self-host sources for bare-for over a vector followed by element mutation — BOTH forms:
   `for <x> in <vec>:` AND `for <i>, <x> in <vec>.enumerate():`, each followed by `<x>.field =`
   / a mutating method on `<x>`. Any fixture whose WIRED expectation relies on the buggy Rust
   write-through is a REPORT (don't silently rewrite it — that's the redesign-around-gaps trap;
   list them with proposed dispositions).
6. **STOP-AND-REPORT** on any conflict between these mandates, any gate failure, any surprise
   (e.g. the sever requires new materialize machinery rather than routing through
   `cow_before_mutation` Case 2 / the existing collection-materialize path). Never silently
   weaken a fixture.

## Fixtures (cross-lane pins; expected outputs are §3.1-derived — the language's INTENDED
behavior, all RED on at least one lane today)

| Fixture | Shape | Expected stdout |
|---|---|---|
| `cow_for_amp_vector_field_writethrough.gg` | value struct, `&`, owned root | `101` then `102` |
| `cow_for_amp_vector_alias_root.gg` | `b = a; for c in &b:` mutate; print `a[0]` then `b[0]` | `1` then `101` |
| `cow_for_bare_vector_control.gg` | value struct, bare, mutate | `1` |
| `cow_for_bare_resource_elem_materialize.gg` | resource struct, bare, mutate | `1` (pins A2; RED on Rust today) |
| `cow_for_amp_resource_elem_writethrough.gg` | resource struct, `&`, owned root | `101` |
| `cow_for_enumerate_bare_resource_materialize.gg` | resource struct, bare `.enumerate()`, mutate | `1` (pins the A2 enumerate twin; RED on Rust today) |
| `cow_comprehension_amp_source.gg` | `[x * 2 for x in &a]` over a populated vector; print len + elems | the CORRECT doubled values (pins the yields-empty HIGH; derive the exact stdout from §3.1 — read path, no mutation) |
| `cow_for_amp_deque_field_writethrough.gg` | value struct, `&`, Deque root | `101` (the Deque probe constraint 3 mandates) |

Wire all of them as `run_gg` integration tests (both backends inherit via the harness). ggdef lane:
`for x in &coll` is OUT of the phase-0 subset (Increment B2) — add the EXCLUDE rows with the
documented out-of-subset reason (the `corpus_b1.rs` pattern), and note the subset gap in the
campaign plan's status header. The bare-mode fixtures (3, 4) use no `&` iterable — check whether
they fall IN subset; if yes they are ggdef-gated too (report what ggdef prints).

## Gates (executor scope — the parent runs the full sweeps + parity at integration)

FOREGROUND, explicit generous timeouts; self-host-touching commands get
`GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`; if a gate exceeds one 10-minute command,
split it by test name into sequential foreground chunks — never background a final gate.

1. `cargo build` + `cargo test --lib`.
2. ALL new fixtures (the table above): build+run on C AND LLVM (`--backend=llvm`), diff stdout
   against expected.
3. The scout's regression set byte-identical: `cow_direct_for_element_resource_struct`,
   `auto_struct_vector`, `cow_field_of_for_element_read`, `cow_loop_borrow_propagation`, plus
   the targeted `cow_*`/for-loop/comprehension integration filter.
4. ASan (`--sanitize`) on the new fixtures — the element-ptr no-drop path is the #1 ASan risk
   (mirror 1B's gate). The A2/enumerate-twin fixes change drop behavior for the bare resource
   path: ASan there too.
5. Clone-stats neutrality on the bare controls (`--clones=stats`): the bare path must not gain
   clones; report the `&`-path counts (write-through should REMOVE the per-element copy).
6. `self_host_bootstrap_fixed_point` (self-host lowerer touched; scout says the compiler source
   itself uses no `for x in &coll`, so expect INERT — verify).
6b. The self-host-owned cross-lane cells: run fixtures #3/#4/#6 (bare controls + the enumerate
   twin) AND the `&` cells through the SELF-HOST driver (emit-C → cc → run) and diff against
   expected — the parent's parity regen will compare these lanes; do not hand it a divergence.
7. `cargo test -p ggdef` — you edit `corpus_b1.rs` EXCLUDE rows and ADD fixtures whose names the
   corpus filters pick up (`cow_*` prefix!); the definition lane must be run, not assumed
   (the Batch-A lesson: expectation-affecting fixture adds gate on the FULL ggdef suite). Note
   the bare-mode fixtures may fall IN subset — report what ggdef prints for them.
8. Emitted-shape drift guard: grep `tests/` for anchored LIR/C snapshot landmarks touching
   for-loop element binding (the new ptr locals change emitted shape) BEFORE the gates —
   per the emitted-shape-change discipline; list any snapshot tests you had to regenerate.

## Commit discipline

Your worktree only. Stage EXPLICITLY by file name (`git add src/ir/lowering/stmts/for_loops.rs
src/ir/lowering/exprs/collections.rs tests/fixtures/cow_for_*.gg
tests/fixtures/cow_comprehension_amp_source.gg tests/integration.rs
tests/fixtures/self_host_lowerer/lower_loops.gg tests/fixtures/self_host_lowerer/lower_expr.gg
spec/ggdef/tests/corpus_b1.rs docs/plans/cow-writethrough-materialize-closed-set.md` — adjust
to what you actually touched;
NEVER `git add .`/`-a`/`commit -a`). One commit; message with the measured cell matrix; trailers:

    Co-Authored-By: Claude Opus <noreply@anthropic.com>
    Claude-Session: https://claude.ai/code/session_01TYkkHveF8WhhTVX4DjbCTN

Checkpoint progress to `/tmp/cow1a_exec_progress.md` after every gate. Final message: commit
hash + branch, the full fixtures×2-lane measured matrix, gate results, the A2/enumerate
dependent-sweep findings, the comprehension-lane measurements, and
any smells (the self-host `b = a` eager-clone pessimization is ALREADY filed — don't fix it here).

## Zones

Serialize on `lower_loops.gg`/`lower_expr.gg` (the R39-T1 self-host mirror entry also wants
`lower_expr.gg` — it is NOT running now; you have the zone). Other concurrent agents:
`tests/integration.rs` (harness landing — do NOT touch that file's `run_with_deadline` region;
your integration.rs edits are test-wiring additions only, expect a possible rebase),
Rust `src/lexer|parser|semantic` + `spec/ggdef/src` (D29 scout — read-only for you except
`spec/ggdef/tests/corpus_b1.rs` EXCLUDE rows).
