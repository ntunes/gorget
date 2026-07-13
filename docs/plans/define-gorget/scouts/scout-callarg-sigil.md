# Scout: call-arg-sigil preservation fix (self-host)

Status: COMPLETE. Read-only scout (prototype applied + MEASURED + reverted; tree
clean). HEAD `aab08f28`. Patches persisted under `patches/`.

## TL;DR — RECOMMEND SHAPE (a). Proven clean + proven to fix the bug.

- **Gap confirmed** (parser.gg:2047/2051/2069): `parse_call_args` calls
  `skip_ownership_markers()` before each arg, ADVANCING PAST + DISCARDING the
  `!`/`&`/`move`/`mutable` sigil → `f(!x)`/`coll.push(!x)` parse to bare `x`.
- **Shape (a) — wrap the arg — is the winner.** Minimal (delete the two skip
  calls; the EXISTING prefix parser already builds `EMove`/`EMutableBorrow` for
  `!`/`&`/`move`), mirrors expression-context parsing, and is Layering-rule-1
  correct (ownership no longer dropped at parse).
- **Shape (a) is PROVEN clean end-to-end** despite rewrapping 9059 `&ident` +
  171 `!ident` call args in the self-host source:
  - `lowerer_comparison`: baseline and prototype logs **byte-identical modulo
    timing** — Total 1524 / Matched 1222 / Error-only 150 / Real mismatches 127 /
    Crashes 25, and the full per-fixture mismatch+crash lists diff EMPTY (no
    fixture changed category). The self-host lowerer emits identical C. NO
    miscompile, NO lowering change.
  - `self_host_bootstrap_fixed_point`: **PASSES** (439.63s) — the self-host
    compiles ITSELF with every `&gmod`/`&self` arg wrapped and reaches the
    byte-identical fixed point.
  - `box_deref` self-host-driver ASan gates (the `&*box` reroute path): **6/6
    clean**.
- **Shape (a) PROVES it fixes the bug** (re-enabled pos-2/3 on the prototype):
  `W(!a)` ctor-move + `v.push(!b)` collection-move → self-host driver **exit 0,
  emits C (ACCEPTED)**; bare `W(a)`/`v.push(a)` → **exit 1, REJECTED** with
  `cannot copy 'a'... write '!a' to move`. The over-rejection hole is closed and
  the D12 check is restored to correct behavior.
- **Shape (b) (parallel ownership vector / AST field)** is the fallback: it
  requires an ECall/EMethodCall arity change touching **~150 destructuring +
  construction sites** across every self-host dir (ast.gg is symlinked). Higher
  churn, and NOT needed — shape (a) doesn't touch the lowerer's behavior at all.

## Section 1 — Gap confirmation (file:line + measured lowering behavior)

`tests/fixtures/self_host_typechecker/parser.gg` is the REAL file; **`self_host_check/parser.gg`
and `self_host_lowerer/parser.gg` are SYMLINKS to it** (lints.rs:3339 documents
this). `self_host_parser/parser.gg` + `self_host_resolver/parser.gg` are separate
real copies.

- `parse_call_args` (:2037): `skip_ownership_markers()` at :2047 (first arg) and
  :2051 (each `,`-separated arg).
- `skip_ownership_markers` (:2069): handles ALL FOUR spellings
  `&`/`!` (TOK) + `move`/`mutable` (KW) and `advance()`s past them — discarding.
- `parse_call_arg` (:2076) → `parse_expr` on the now-bare inner → `f(!x)` yields
  `EIdentifier("x")`.
- Expression-context prefix parser ALREADY builds the wrappers: `!x`/`move x`→
  `EMove` (:2493/:2503, :2506/:2515), `&x`→`EMutableBorrow` (:2544/:2547). There
  is NO `mutable`-as-prefix arm, but `mutable X` as a call-arg sigil is used
  **nowhere** in the tree (only in comments) — a completeness item, not a live path.
- Named-arg VALUES already preserve the sigil today (`f(k = !v)` routes through
  the `IDENT =` branch :2121 → `parse_expr("!v")` → `EMove`). Only POSITIONAL args
  were broken.
- D12 disabled positions live ONLY in `self_host_typechecker/typecheck.gg`
  (:1105 pos-2, :1118 pos-3, both `TEMPORARILY DISABLED (2026-07-12)`).

**Current lowering of `push(!x)` (sigil discarded) — MEASURED:** the self-host
lowerer decides borrow-vs-copy from the CALLEE's declared param types
(`borrow_flags[ai]`) + liveness, NOT the arg sigil. Evidence: lower_expr.gg:8626
"A `&t` call arg (parser strips the sigil; callee param is `&T` = borrow_flags[ai])";
:8590 "The parser strips the `&` sigil, so this reaches the arg loop as a bare
`EIdentifier`"; the general `EMove` arm (:5604) is transparent
(`return lower_expr(*inner_box)`). So `push(!x)` and `push(x)` lower IDENTICALLY at
the shape level — the sigil is redundant FOR THE LOWERER; it only matters for the
TYPECHECKER's D12 place-purity check. (The prototype's byte-identical
`lowerer_comparison` log CONFIRMS this: rewrapping every arg changed no emitted C.)

## Section 2 — the two shapes' blast radius (MEASURED)

### Symlink topology (decisive)
- REAL canonical frontend files in `self_host_typechecker/`; `self_host_check/`
  and `self_host_lowerer/` SYMLINK the whole frontend (ast, parser, typecheck,
  infer, resolve, format, meta, derive, traits, ...). `self_host_lowerer/` adds
  its OWN real lowering files (lower_*.gg, lir_*.gg, gir.gg, drop_elab.gg,
  reachability.gg, validate.gg). `self_host_parser/` + `self_host_resolver/` are
  independent real copies.
- Consequence: one edit to `self_host_typechecker/parser.gg` changes the
  TYPECHECKER **and the LOWERER** (and check). The lowerer/bootstrap DO see it —
  so the miscompile risk is real and was DIRECTLY measured (and found absent).

### Shape (a) — wrap the arg (remove the two skip calls; reuse the prefix parser)
Edit surface: `self_host_typechecker/parser.gg` (the two `skip_ownership_markers()`
deletions — symlink covers typechecker+check+lowerer) + add a `mutable`-prefix arm
near :2544; PLUS `self_host_parser/parser.gg` + `self_host_resolver/parser.gg` (2
independent copies) for consistency (see §5). NO ast.gg change, NO arity change.

Consumer blast radius — frontend consumers ALREADY handle the wrappers:
- `expr_is_place` (typecheck.gg:603) returns false for EMove/EMutableBorrow (`else`
  :622); its comment (:601) says "`!x` (EMove) ... are NOT places → naturally
  legal" — shape (a) is the DESIGNED path for the typechecker.
- infer.gg:863/:866, resolve.gg:756/:759, format.gg:275/:277 all have EMove/
  EMutableBorrow arms; spawn-arg peel already present at lower_expr.gg:1494-1498.

Lowerer arg-shape sites that key off the bare shape (the ONLY risk) — MEASURED to
be behavior-neutral because the wrapper arms recurse to the inner and borrow_flags
still drives the decision:
- lower_expr.gg:8570 `ad_param_by_ptr` match (`&*box` EDeref / `&static`
  EIdentifier) — after shape (a) the box-deref handling reroutes from :8570 to the
  :5606 EMutableBorrow arm (inner EDeref match :5617) — the `box_deref` ASan gates
  cover this and stayed clean.
- lower_expr.gg:8632 `cow_materialize_projected_root(arg)`; :8655
  `case EMutableBorrow(_) → is_borrow` (was dead for call args, now fires but is
  redundant with borrow_flags).
Exposure measured: **9059 `&ident` + 171 `!ident`** call-arg sites in the self-host
source — all rewrapped by the prototype, all lowered to identical C.

### Shape (b) — parallel ownership vector (AST field on ECall/EMethodCall)
ast.gg is SYMLINKED, so a new field on `ECall`/`EMethodCall` changes arity for
EVERY dir. MEASURED distinct sites (real files; symlinks not double-counted):
- destructuring `case ECall(`/`case EMethodCall(`: canonical typechecker 19+16=35;
  lowerer-OWN files ~72; self_host_parser 4; self_host_resolver 8 → **~119**.
- construction `ECall(...)`/`EMethodCall(...)`: ~35 more.
- → **~150 mechanical arity sites.** Low per-site risk (arity mismatch = loud
  compile error; precedent + guard: the `Param` 4th-field add,
  lints.rs:`self_host_param_ctor_site_count`), but a LARGE churn surface. The
  lowerer needs ZERO behavior change (keeps reading borrow_flags) — which is
  exactly what shape (a) ALSO delivers, at a fraction of the churn.

## Section 3 — RECOMMENDED shape: (a), with measured evidence

**Shape (a).** It is minimal, mirrors the language's own expression-context
parsing, satisfies Layering rule 1 (ownership becomes a preserved invariant), and
is PROVEN to change no lowering behavior:

| Gate | Baseline | Shape (a) prototype |
|------|----------|--------------------|
| `lowerer_comparison` (1524 fixtures) | Matched 1222 / MM 127 / Crash 25 | **identical** (full log diff empty modulo timing) |
| `self_host_bootstrap_fixed_point` | passes | **passes** (439.63s) |
| `box_deref_*` self-host ASan (6) | pass | **6/6 pass** |
| D12 re-enabled: `W(!a)`/`v.push(!b)` | (disabled) | **ACCEPT** (exit 0, emits C) |
| D12 re-enabled: bare `W(a)`/`v.push(a)` | (disabled) | **REJECT** (exit 1) |

Commands (all run this session, `--release`, worktree):
`cargo test --test integration --release lowerer_comparison -- --nocapture`;
`... self_host_bootstrap_fixed_point ...`; `... box_deref ...`;
`self_host_lowerer/driver <fixture> lib --lir-c` (direct accept/reject probes).

The prototype patch is `patches/callarg-sigil-shapeA.patch` (the two skip
deletions). NOTE: it relies on the existing prefix parser for `!`/`&`/`move`; the
full fix must ALSO add a `mutable`-prefix arm (near parser.gg:2544 → EMutableBorrow)
so all four spellings are covered — unused in-tree today but required for
completeness/robustness. The re-enable proof patch is
`patches/callarg-sigil-reenable-proof.patch`.

There is NO miscompile. The brief's central fear ("a subtle call-arg miscompile
that passes the bootstrap but corrupts output") was hunted directly via the
byte-identical count-diff (1524 fixtures) + the passing fixed-point + the ASan
`&*box` gates + direct accept/reject driver runs — all clean.

## Section 4 — A2-S pos-2/3 re-enable checklist + B2 unblock

The re-enable is the EXACT inverse of the disable commit `5ea1c92b` (4 hunks).
Both gating helpers ALREADY EXIST — no new logic:
- `is_ctor_callee(*callee, scopes, ctx)` (typecheck.gg:691)
- `is_collection_ingest_method(name)` (:752) + `is_collection_receiver(...)` (:766)

1. **typecheck.gg ECall arm (pos-2, ~:1103)** — restore:
   ```
   bool call_is_ctor = is_ctor_callee(*callee, scopes, ctx)
   for a in args:
       if call_is_ctor:
           reject_tainted_place(a, scopes, &types, &ctx)
       check_carrier_ops_expr(a, scope_id, &scopes, &types, &ctx)
   ```
2. **typecheck.gg EMethodCall arm (pos-3, ~:1113)** — restore:
   ```
   bool ingest = is_collection_ingest_method(method_name) and is_collection_receiver(*receiver, scopes, &types, &ctx)
   reject_amp_self_mutator(*receiver, method_name, scopes, &types, &ctx)
   for a in args:
       if ingest:
           reject_tainted_place(a, scopes, &types, &ctx)
       check_carrier_ops_expr(a, scope_id, &scopes, &types, &ctx)
   ```
3. **lints.rs `self_host_d12_reject_hook_count`**: EXPECTED **7 → 9** (:902).
4. **integration.rs `self_host_driver_rejects_d12_drop_purity`** (:18457): restore
   the 3 dropped fixtures — `pos2_ctor_init_reject`, `pos3_collection_put_reject`,
   `pos3_field_place_reject` (they still exist on disk; Rust gg already asserts
   them via `check_gg_fails` at integration.rs:5699/5704/5709).
5. **NEW over-rejection ACCEPT guard** (the hole that let the bug through): add a
   `W(!x)` ctor-move + `coll.push(!x)` collection-move fixture (my synthetic
   `accept_move_arg.gg` is a ready model) to `d12_drop_purity/` and wire it into
   `self_host_driver_accepts_d12_legal` (:18533) so a future sigil-drop regression
   is caught as an over-rejection. (Existing `legal_explicit_move` only covers a
   pos-1 BIND `!a`, not a ctor/collection MOVE arg — the exact gap. This is the
   missing guard.)

**Fix the parser FIRST** (shape a), THEN re-enable — the re-enable alone (without
the parser fix) re-opens the over-rejection.

### B2 unblock — CONFIRMED
The disable commit `5ea1c92b` states the parser fix is "the SHARED PREREQUISITE to
re-enable pos-2/3 AND land Batch B B2 (whose self-host place-overlap mirror needs
the same sigil)." Shape (a) lands the per-arg sigil in the AST (EMove/
EMutableBorrow), which is exactly what the B2 place-overlap mirror consumes → B2 is
unblocked by the same fix.

## Section 5 — slicing / size / risk / owner design question

**Slicing (single small PR):**
1. parser.gg: delete the two `skip_ownership_markers()` calls in `parse_call_args`
   + add a `mutable`-prefix arm → EMutableBorrow (all four spellings). (symlink
   covers typechecker/check/lowerer.)
2. `self_host_parser/parser.gg` + `self_host_resolver/parser.gg`: same skip
   removal in THEIR `parse_call_args` (independent copies; keeps the self-host
   family consistent and the parser/resolver round-trip correct). NOTE these two
   ALSO strip the sigil in FORMAT round-trip today (`f(!x)`→`f(x)`) — a latent
   round-trip fidelity bug the same fix closes; verify with `parser_comparison` /
   `resolver_comparison` count-diff (rebuild their drivers).
3. typecheck.gg: re-enable pos-2/3 (§4.1-2).
4. lints.rs 7→9 (§4.3); integration.rs restore 3 fixtures (§4.4) + new accept
   guard (§4.5).

**Size:** SMALL. Core is ~4 deleted lines + 1 small prefix arm + a pure revert of
a 4-hunk disable + one new fixture. Executor validation: `lowerer_comparison` +
`type_comparison` + `check_comparison` + `parser_comparison` + `resolver_comparison`
count-diff (expect unchanged), `self_host_bootstrap_fixed_point`, the D12
accept/reject lanes, the arm-count lint, and `box_deref` ASan.

**Risk:** LOW, and directly measured on the LOWERER (the only place it could bite).
The one thing NOT exercised by the prototype is `mutable`-as-call-sigil (unused
in-tree) — the added prefix arm covers it; add a tiny `f(mutable x)` parse/format
fixture to lock it.

**Residual to re-measure at execution time (per "regenerate every number"):** the
`type_comparison`/`check_comparison`/`parser_comparison`/`resolver_comparison`
count-diffs (this scout measured `lowerer_comparison` + bootstrap + ASan directly;
the other frontends are lower-risk but should be count-diffed to confirm unchanged).

**Owner design question:** Shape (a) makes the self-host AST carry `EMove`/
`EMutableBorrow` wrappers on call args, matching how expression-context `!x`/`&x`
already parse — but it does NOT (yet) mirror Rust gg's `CallArg.ownership` FIELD
(src/parser/expr.rs:1996/2032). For the self-host that's fine: the D12 check reads
place-ness via `expr_is_place` (which the wrapper naturally satisfies) and the
lowerer ignores the sigil (borrow_flags-driven). **Do you want the self-host to
stay wrapper-based (simplest, proven), or eventually converge on a typed
`CallArg.ownership` to structurally mirror Rust gg?** The wrapper approach is
recommended (zero arity churn, already the idiom for `!x`/`&x` elsewhere); the
`CallArg.ownership` field is the shape (b) direction and only worth it if a future
pass needs per-arg ownership WITHOUT re-deriving it from the wrapper.
