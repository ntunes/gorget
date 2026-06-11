# EXECUTOR BRIEF — Chain F: snag #11 (cross-error-type auto-propagation) → From-mediated, BOTH compilers symmetric

Status: v3 (pass-1 + pass-2 brief-reviews folded, 2026-06-11, on gorget-1 tip
`5d7be65a`). Pass-2 fold (fresh reviewer, all claims PASS, 4 minor scoping
refinements): reuse `FunctionInfo.throws_type_id` for caller-E (not a new
field); flag the self-host lowering span-plumbing symmetry (~13 sites); name
the self-host AST-Type→TypeId resolver `ast_type_to_resolved` (types.gg:278);
drop the literal parity baseline → capture it at session start. NEEDS ≥1 more
fresh brief-review (until a pass raises no reservations).
Provenance: scout `agent-a65eb1d6506b83c2f` (GO; end-to-end-proven the Rust
gate + both lowerings; corpus fallout = 0 → land-direct, no migration). Owner
decision 2026-06-11: **SYMMETRIC** — both compilers REJECT and CONVERT
identically in this change. Pass-1 fold (fresh reviewer, all 6 reservations,
none a wrong root cause): reframed "one decision"→"shared helper at TWO
chokepoints" (Route B's 12 guards already funnel through `is_auto_propagation_compatible`);
flagged the reject-must-emit-at-decision-point trap (false-return reroutes to a
misleading `unify` mismatch since OK-types match); flagged the span-plumbing
(choke point carries no span, `from_conversions` is span-keyed); made the
self-host From-lookup concrete + flagged its AST-arg-resolution difficulty;
relabeled the `:2188` Index site; dropped the `span` field from the error kind.
NEEDS ≥2 more fresh brief-reviews (until a pass raises no reservations).

## Mission
Close the cross-error-type auto-propagation miscompile (a real out-of-bounds
read / type confusion) as a CLASS, at both layers, in BOTH compilers, with
From-mediated semantics per `docs/language-design.md §36.3`. Land-direct: the
whole corpus (incl. all 6 self-host drivers + lib/std) already type-checks
clean under the new gate (scout-measured), so no migration is required.

## The bug + the TWO routes (re-derive line nums — they drift)
Repro `docs/plans/snag11_cross_error_propagation.gg`: `inner() throws String`
called in `outer() throws BigErr` (BigErr larger; no `From`, no `rethrow`)
type-checks clean and lowers to `memcpy(dst, src, sizeof(BigErr))` reading 64
bytes from a 32-byte `Str` → GCC `-Wstringop-overread`; ASan
stack-buffer-overflow READ of size 64; caught fields `a/b/c/d` are garbage.

There are **TWO propagation routes into the SAME lowering choke point** — the
executor MUST cover both (the scout proved Route B is a live, separate
instance; a lowering-only guard link-errors on it):
- **Route A (throws-fn call):** the centralized producer-peel,
  `src/semantic/typecheck.rs:1572-1629`. Gated to `func_info.throws_type_id`
  (:1572); peels `Result[T,E] → T` returning `return_type` and **discarding
  `err_ty`** (:1623). This is where the repro slips through.
- **Route B (explicit `Result[T,E]`-returning callee):** the ~12 consumer
  guards calling `is_auto_propagation_compatible` (`typecheck.rs:4248`, which
  destructures `(ok_type, _err_type)` at :4251 and discards `_err_type`).
  Call-site classification (re-grep current lines):

  | Site | Args | Role | Needs the E-check |
  |---|---|---|---|
  | ~1484 | (param_type, arg_type) | call-arg into Result-param (capture) | YES |
  | ~2188 | (expected, found) | Index-position auto-prop (`Result[K,E]`→index type) | audit (only when peeling a throws/Result value) |
  | ~3028 | (declared_type, value_type) | VarDecl | YES (proven leak route) |
  | ~3086 | (target_type, value_type) | assignment target | YES |
  | ~3137 | (ret_type, expr_type) | Return | YES |
  | ~3270/3297/3306 | (bool_id, cond_type) | if/while/match-guard cond peel of `Result[bool,E]` | YES (E still re-wrapped on err path) |
  | ~4095/4196 | (ftid, arg_ty) | enum-variant / struct-ctor field init | YES |
  | ~4953/4983 | (param_types[i], arg_type) | imported-fn call args | YES |

  The `(bool_id, cond_type)` sites are NOT "leave-alone bool coercion" — they
  peel a throwing/Result value and carry the same E-mismatch hazard.

- **Shared lowering choke point (BOTH routes):** `emit_result_auto_propagate`
  (`src/ir/lowering/exprs/mod.rs:2784`), err path ~`2848-2869`: loads `err_val`
  as the CALLEE's `err_field_type` (:2848-2853) then `enum_init`s the CALLER's
  `fn_res_type` Error with it (:2869) → the memcpy. `should_auto_propagate`
  (:2929) has no E check. Keys on `EnumCategory::Result`, not on throws — so
  one fix here serves both routes.
- **Self-host twin (lowering):** `maybe_auto_propagate`
  (`tests/fixtures/self_host_lowerer/lower_match.gg:1005-1067`), re-wrap at
  ~`:1058-1061` (`GICallExtern(err_dst, "Error", [err_val])` into the caller's
  `fn_result_type`). Identical hole.

## Design — centralize, write through typed metadata (devbook/24)
**The E-check belongs in a shared helper invoked at TWO chokepoints — NOT one
mythical single site, and NOT scattered across the guards.** Route B is ALREADY
one site: all 12 Route-B guards funnel through the single function
`is_auto_propagation_compatible` (`typecheck.rs:~4248`), so the E-check goes in
its body. Route A is the second site: the producer-peel (`typecheck.rs:~1572`).
Both invoke the SAME shared helper, which classifies a propagation position as
{not-a-propagation / same-type-OK / convert-via-From(DefId) / REJECT} so Route A
and Route B cannot diverge.

1. **Typed-metadata axis (Rust).** Add `from_conversions: FxHashMap<Span,
   DefId>` to the analysis output (`AnalysisResult` in `src/semantic/mod.rs` —
   re-verify the struct + the `check_module` return threading; mirror an
   existing side-table like `method_resolutions`/`expr_types`). Key = the
   propagation **call-site span**; value = the resolved `From::from` method
   `DefId`. **Thread the DefId, never a reconstructed `format!("From__…")`
   name** — the scout's name hack was feasibility-only and won't handle generic
   error types; the lowering resolves the DefId to its mono symbol the same way
   other resolved calls do.
2. **Typecheck gate (Rust), at the shared decision.** Resolve callee-E (the
   `Result`'s E — `err_ty` at the Route-A peel; the discarded `_err_type` for
   Route B) and caller-E. For the caller's error type, REUSE the existing
   `FunctionInfo.throws_type_id` (the resolved caller-E already read at the
   Route-A peel `typecheck.rs:1572`; fn-entry hook ~`:5688`) rather than
   re-resolving `func.throws` from AST — one source of truth. Only add a new
   `current_*` field if a `Result`-typed (non-`throws`) return needs deriving
   that `throws_type_id` doesn't already cover; prefer the existing field.
   Three cases at every propagation position:
   - **same type** → accept, no metadata (today's path — MUST stay
     byte-identical; the scout proved this).
   - **different + `From[calleeE]` equipped on callerE** → accept; resolve the
     `From::from` impl (`equip B with From[A]:` registers an `EquipInfo`,
     `traits.rs:89-113`; lookup via the trait/equip registry) and RECORD its
     `DefId` in `from_conversions` keyed by the call span.
   - **different + no `From`** → **reject** with a dedicated
     `SemanticErrorKind::UnconvertibleErrorPropagation { caller_err, callee_err }`
     (add the variant + its render; NO `span` FIELD — siblings like
     `TypeMismatch`/`WrongArgCount` carry none; the span is passed via
     `self.error(kind, span)`. Do NOT reuse `TypeMismatch`). Message: suggest
     `equip CallerE with From[CalleeE]:` or `rethrow`, cite §36.3. Model
     registration/rendering on an existing kind (e.g. `wrong_arg_count_error`,
     `src/semantic/errors.rs`).
     ⚠ **EMIT THE ERROR AT THE DECISION POINT — do NOT just return `false`.**
     For the cross-type-no-From case the OK-types MATCH (e.g. `bool` vs `bool`),
     so today `is_auto_propagation_compatible` returns `true` and the program
     slips through; if the new E-check merely returns `false`, the caller falls
     through to `unify(Result[bool,E], bool)` and emits a MISLEADING
     `expected bool, found Result` `TypeMismatch` instead of the teaching error.
     The shared helper must EMIT `UnconvertibleErrorPropagation` itself (it needs
     `&mut self` / the error sink) and signal the caller to STOP — not re-unify.
     (This means the helper is NOT a pure `&self` predicate like today's
     `is_auto_propagation_compatible`; plan the signature accordingly — e.g.
     return an enum {NotProp, Ok, Convert(DefId), Rejected} and let the caller
     skip the unify on `Rejected`.)
3. **Lowering conversion (Rust).** At `emit_result_auto_propagate` err path:
   read `from_conversions` for the call span; if present, emit a call to the
   recorded `From::from` (DefId → mono symbol) on `err_val`, then `enum_init`
   the caller Error with the CONVERTED value; else today's path. DCE seeds
   automatically off the emitted call reference (scout-confirmed).
   ⚠ **Span plumbing is real, unmentioned work:** `maybe_auto_propagate` /
   `emit_result_auto_propagate` carry NO span today (signature
   `(ctx, builder, operand)`), but `from_conversions` is span-KEYED. The
   propagation span IS available at the centralized `lower_expr` hook
   (`src/ir/lowering/exprs/mod.rs:~84`, `expr.span` ~:95) — add a span parameter
   and thread it through `maybe_auto_propagate` → `emit_result_auto_propagate`
   across its ~15 call sites (calls.rs, methods.rs, stmts/*, mod.rs). Required,
   not optional — it's the metadata key.
4. **From-only.** Auto-propagation uses infallible `From` only. A `TryFrom`
   (fallible) conversion is NOT auto-applied — the user must `rethrow`/`catch`.
   Do NOT synthesize a conversion without a `From` impl (owner directive;
   payload-carrying errors make Zig-style implicit coercion unsound).

## Self-host — SYMMETRIC (owner decision; the higher-risk, UN-PROTOTYPED part)
The scout proved the Rust gate + BOTH lowerings, but did NOT prototype the
self-host TYPECHECK gate. Implement it RUN-verified, incrementally:
   ⚠ **Self-host lowering span-plumbing is the SAME burden as Rust (step 3):**
   the self-host `maybe_auto_propagate(LowerCtx &ctx, int val, GirModule &gmod)`
   carries NO span either, at ~13 call sites (lower_loops/lower_expr/lower_stmt/
   lower_match) — thread a span through to read its `from_conversions` side-table,
   exactly as the Rust step 3. Scope it up front.
1. **Lowering conversion (self-host)** `lower_match.gg:1058` — mirror the Rust
   lowering: when the metadata says convert, emit the `From` conversion on
   `err_val` before the Error re-wrap; else today's path.
2. **Typecheck gate (self-host) — NET-NEW.** The self-host typechecker
   (`tests/fixtures/self_host_typechecker/typecheck.gg`) has NO
   producer-peel/auto-prop-compat logic today. ⚠ `typecheck.gg` is SYMLINKED
   into `self_host_check` and `self_host_lowerer` — `md5sum` the copies, edit
   the CANONICAL `self_host_typechecker` copy only. Add: at the self-host's
   call-typing for a throws/`Result`-returning callee in a propagating context,
   compute callee-E vs caller-E; same → peel as today; different + `From` on
   callerE → record (self-host metadata side-table, mirror the Rust axis);
   different + no `From` → emit the teaching error. **Self-host From-lookup —
   concrete + HARDER than Rust:** use `EquipInfo` (`types.gg:~76`:
   `trait_name`/`self_type`/`trait_generic_args`/`method_def_ids`) for the impl
   data, and mirror the existing span-keyed side-table `types.expr_types`
   (`infer.gg:~437`) for the self-host `from_conversions`. ⚠ The self-host
   `TraitRegistry.trait_impls` (`types.gg:~104`) is keyed on `self_type` ONLY
   (no resolved composite `(trait,type,args)` key like Rust's), so the From
   match must SCAN candidate impls for `self_type==callerE`, filter
   `trait_name=="From"`, and RESOLVE the AST `trait_generic_args[0]` Type to a
   TypeId via the EXISTING self-host resolver
   `ast_type_to_resolved(Type ast_ty, Span span, ScopeTable scopes, TypeTable &types) -> int`
   (`types.gg:~278`) to compare against calleeE — AST-type-resolution work the
   Rust direct lookup avoids (this is the highest-risk, un-prototyped part;
   `ast_type_to_resolved` is the tool that does it). Same emit-at-decision-point
   rule (don't silently fall through).
3. **RUN-verify the self-host gate:** build the self-host typechecker driver,
   run a cross-type repro through it, confirm it REJECTS; run a From-mediated
   positive through the self-host lowerer, confirm correct output. This part
   has no scout proof — treat the `fixed_point` + `*_comparison` gates as
   load-bearing (a self-host typecheck change shifts `type_comparison` /
   `check_comparison`; confirm no parity regression by reading the printed
   counts).

## Sibling-guard lint (CLAUDE.md rule 4)
Add an arm-count lint to `tests/lints.rs` over the `is_auto_propagation_compatible`
call sites (or the centralized peel decision) so the NEXT propagation position
is forced through the shared E-checked path. Count the sites this session; pin
the count; comment why.

## Fixtures (embedded stdout / expected-reject)
- **Negative (Route A):** the repro shape → `gg check` REJECTS with the
  teaching error. Wire into the negative-fixture harness (see how
  `*_error` fixtures register, e.g. `wrong_arg_count_error`,
  `type_mismatch_return_error`). Keep `docs/plans/snag11_cross_error_propagation.gg`
  as the canonical repro or move it into `tests/fixtures/`.
- **Negative (Route B):** an explicit `Result[T,E]`-returning callee variant
  (no `throws`) → also REJECTS (proves Route B is gated).
- **Positive (From-mediated, both routes):** `equip B with From[A]:` +
  `inner() throws A` in `outer() throws B` (and the Result-returning variant) →
  compiles, RUNS, prints the CONVERTED error (assert the `B.from` field, e.g.
  `code=42`); `-Wstringop-overread` = 0 / ASan-clean. Both backends (C + LLVM).
- **Same-type guard:** an existing `throws E` … `throws E` fixture still
  auto-props with byte-identical emitted C (the gate no-ops when E matches).
- **Self-host:** a test proving the self-host typecheck REJECTS the cross-type
  shape (driver-level or via the comparison harness).

## Gates (executor; parent re-runs the full battery on the integrated tree)
- `cargo build`; `cargo test --lib` (quote counts); `cargo test --test lints`
  incl. the new arm-count lint.
- Repro REJECTS; positive compiles+runs+correct (C and LLVM); over-read gone.
- Negative + positive integration fixtures wired and passing.
- `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`) — THE
  load-bearing canary for the self-host typecheck change.
- `type_comparison`, `check_comparison`, `lowerer_comparison`,
  `c_emit_comparison` — re-run and READ THE PRINTED COUNTS (diagnostic-
  always-pass); confirm the self-host typecheck change does not regress parity.
- Parity re-measure: `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test
  --test integration --release self_host_runtime_diff -- --nocapture` (read the
  `PARITY = …` line). CAPTURE the baseline by running this command at session
  start on the unmodified tree — do NOT trust a dated figure; then expect the
  post-fix number ≥ baseline (the fix can only add MATCHes by emitting correct
  conversions, never remove them).

## Constraints
- Standard worktree preamble: `pwd` + `git rev-parse --show-toplevel` inside
  your worktree FIRST; `git merge --ff-only gorget-1`; NEVER touch
  `/workspace/gorget-1` or use `/workspace/gorget-1/...` paths; do not `cd`
  there. Explicit-file `git add` only — never `git add -a`/`.`/`commit -a`. No
  pushes. STOP and report on any contradicted premise (esp. if the self-host
  typecheck gate breaks `fixed_point`, or the corpus turns out to have a
  cross-type-without-From site after all — the scout measured 0).
- File zone: `src/semantic/typecheck.rs`, `src/semantic/mod.rs`,
  `src/semantic/errors.rs` (or wherever `SemanticErrorKind` lives — re-verify),
  `src/ir/lowering/exprs/mod.rs`, `src/ir/lowering/traits.rs` (if the From
  symbol resolution needs it), `tests/fixtures/self_host_typechecker/typecheck.gg`,
  `tests/fixtures/self_host_lowerer/lower_match.gg`, new `tests/fixtures/*.gg`,
  `tests/integration.rs` (append), `tests/lints.rs`, `TODO.md`, `DONE.md`. Do
  NOT touch Chain B's self_host_lowerer parser/ast symlinked files beyond
  lower_match.gg / the typecheck gate.
- Two commits acceptable (Rust class fix; then self-host symmetric) — or one.
  Commit messages cite this brief + the scout; Co-Authored-By trailer.
- The scout's risk reviewers must re-derive from source: (a) the same-type
  byte-identity (the gate must be a true no-op when E matches); (b) the
  centralization actually covers BOTH routes (no propagation position left on
  the old un-checked path); (c) the self-host typecheck gate keeps the
  bootstrap green.
