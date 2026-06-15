# TASK #39 — self-host lazy `Iterator[T]` adapter support (reference-grade feature port)

## Goal (feature completeness, NOT a yield play)

Make the self-host compiler compile idiomatic lazy iterator code the way Rust gg
does — chained adapter expressions like
`v.iter().take(3).map(double).filter(is_even).collect()` and the bare-form
`Vector.map/.fold/.any` convenience wrappers (`lib/std/iter.gg:434`:
`Vector.map = self.iter().map(f).collect()`). This is a **reference-grade
language-feature gap** ("self-host as the elegance showcase"), pursued for
capability completeness — flip-count is a *validation signal*, not the target.

## Root cause (re-verified end-to-end by scout `a64e7fe0`)

The self-host's `expr_types` side-table is keyed on **`span.start` only**
(`self_host_typechecker/types.gg:169`, `Dict[int,int]`). The parser sets a whole
method chain's `span.start` from its receiver (`parser.gg:1803` — this MATCHES
Rust `expr.rs:1124`, so the parser is NOT the divergence). Result: every link in
`a.b().c()` collides onto one key → last-writer-wins → `.filter` on a `TakeIter`
reads the *output* (`FilterIter`) type, not the `TakeIter` receiver type → emits
`FilterIter__…__filter(...)` → `incompatible types … from type int` CC-FAIL.
Measured: single-link `v.iter().take(2)` MATCHes; the break starts at link #2.

**The cheap composite-(start,end) re-key is a MEASURED dead end** (scout
prototyped it: net **−4/+0** — it breaks the bare-form terminal-recording /
name-guard contract the start-only key is load-bearing for). Do NOT re-attempt
the re-key.

## The fix (architecturally correct — retires the start-only-key fossil)

Port Rust's recursive, **span-table-free** type oracle as the chain-link type
source, and re-derive the self-host's proto-discovery / terminal-recording /
name-guard machinery off it.

### Port these Rust functions into the SYMLINKED typecheck driver
(`self_host_typechecker/{infer.gg,traits.gg,types.gg}` — edit the
`self_host_typechecker` copy; `self_host_lowerer`'s parser/ast/typecheck are
symlinked to it):

1. **`infer_expr_ast_type`** (`src/ir/lowering/generics/mod.rs:1511-1607`, ~97 LOC,
   **recursive, NO span side-table**) — the chain-link type oracle. For a
   `MethodCall{receiver, method, targs}`, recursively infer the *receiver's*
   type, resolve the method on it, and return the method's return type with
   substitutions applied. This is what replaces the span-keyed `lookup_expr_gir_type`
   lookups for chain links.
2. Helpers it needs: **`try_register_default_return_type`**
   (`generics/mod.rs:~1073-1170`), **`find_default_trait_method`**,
   **`extract_base_and_args`**, **`build_equip_type_substitutions`**.

### Re-derive the existing machinery off the new oracle
- `lookup_expr_gir_type` / `bare_method_targ_rtids` (`lower_types.gg:482-507`):
  for chain-link method calls, source the receiver type from the recursive
  oracle instead of the `span.start` slot.
- The proto-discovery snapshot/restore + name-disambiguation
  (`typecheck.gg:958-1041` + `expr_method_targs_name`) is *built around* the
  start-only key — re-derive it coherently off the oracle. This is the bulk of
  the risk; do it as one coherent change, not a patch.

### (B) Transitive adapter-of-adapter discovery — SURGICAL EXEMPTION, not a fixpoint
Mirror Rust exactly: **do NOT scan trait-default bodies during transitive
discovery** (`generics/mod.rs:877-894`) and **demand-gate return-type
registration to non-method-generic call sites** (`:1073-1084`). This is what
avoids the driver HANG the TODO warned about (the hang came from a fixpoint/
worklist; Rust uses neither). Do NOT implement a fixpoint.

### (C) FREEBIE: `proto_walk_stmts` missing `SMatch` arm
`lower_generics.gg:153-181` falls to `else: pass` for `SMatch`. Add the arm.
(Measured 0-flip standalone — it's inert without A — but it's part of the
coherent change and bootstrap-safe.)

## Method (NON-NEGOTIABLE): flip-prove fixture-by-fixture, never estimate
The scout's "~11 fixtures" is unproven and several candidates have ORTHOGONAL
blockers the adapter fix will NOT clear (e.g. `coroutine_collections_advanced`→
`Vector__fold` undef, `stdlib_iter_set`→`__set_iter_order_len__int64_t` undef,
`coroutine_option/result_combinators`→memcpy/Result-return, `vector_*_hofs`→
`gorget_array_new` arg). For EACH lazy-iterator fixture: self-host emit C → cc →
run → diff vs `cargo run -- run <fixture>` (the Rust oracle). Report MATCH /
CC-FAIL / WRONG-OUTPUT with the actual cause. A fixture that doesn't flip
because of an orthogonal blocker is fine — name it, don't force it.

Start with the SIMPLEST chain (`v.iter().take(4).filter(is_even).count()` or
similar 2-link) to prove the oracle end-to-end, THEN widen.

## Zone + concurrency
- Zone: `self_host_typechecker/{infer.gg,traits.gg,types.gg,typecheck.gg}` +
  `self_host_lowerer/{lower_generics.gg,lower_types.gg}`.
- **File-disjoint from the concurrent Box track** (`lower_expr.gg`/`lir_lower.gg`)
  → the two run truly concurrently in separate worktrees.

## Gates (THE bootstrap is load-bearing — the driver itself uses iterator constructs)
- `cargo build`, `cargo test --lib`, `cargo test --test lints`
- `type_comparison` + `lowerer_comparison` (diagnostic — read the printed counts;
  must not regress)
- **`self_host_bootstrap_fixed_point`** (`GG_BUILD_TIMEOUT_SECS=600`) — MUST keep
  converging at EVERY step; the self-host source uses iterator constructs, so a
  broken oracle breaks the bootstrap. Gate incrementally, not just at the end.
- Full integration + `self_host_runtime` (add snapshots for the flipped fixtures).

## Honesty clause
If the architecture does not converge (the proto-discovery re-derivation fights
the oracle, or the bootstrap can't be kept green), STOP and report the wall —
do not ship a half-port that leaves the driver mis-typing chains. A partial,
bootstrap-green increment that flips even one chain cleanly is a valid landing;
a broad rewrite that regresses the bootstrap is not.

## Brief-review refinements (folded 2026-06-15, brief-review pass `aa5df8a2` CONDITIONAL SIGN-OFF)

1. **ALL line numbers in this brief are APPROXIMATE — they drifted ~50-100 lines
   vs current source.** Re-grep the cited FUNCTION NAMES, not the positions
   (e.g. the proto-discovery snapshot/restore machinery is at `typecheck.gg`
   ~`1009-1046`, not `958-1041`). The LOGIC is verified correct; only positions
   moved. Total port size measured at ~262 LOC core (oracle ~97 + helpers
   `try_register_default_return_type` ~99 / `find_default_trait_method` ~35 /
   `build_equip_type_substitutions` ~23 / `extract_base_and_args` ~8), +
   integration call-sites → ~300-350 with overhead.

2. **THE load-bearing step is re-deriving the proto-discovery / terminal-recording
   / name-guard machinery (`typecheck.gg` snapshot/restore + `expr_method_targs_name`
   `types.gg:181-189`) off the recursive oracle** — the brief-review flagged this
   as the least-verified, highest-risk part (no paper design eliminates it; only
   incremental building does). MANDATORY approach: (a) port `infer_expr_ast_type`
   + helpers FIRST and prove the oracle returns correct chain-link types on a
   2-link fixture; (b) THEN rewire the snapshot/restore so the contract is
   PRESERVED when oracle lookups replace `span.start`-table reads — verify
   line-by-line; (c) exhaustively test the name-guarding (`expr_method_targs_name`)
   on NESTED chains (`a.b().c().d()`) before widening; (d) **gate `self_host_bootstrap_fixed_point`
   after EACH of a/b/c**, not just at the end. If b can't be done without breaking
   the bootstrap, STOP and report (honesty clause).

3. **File-disjointness from the concurrent Box track — confirmed at the DEFINITION
   level, with one coupling to watch.** #39 EDITS `{infer.gg, traits.gg, types.gg,
   typecheck.gg, lower_generics.gg, lower_types.gg}`; Box EDITS `{lower_expr.gg,
   lir_lower.gg}` — disjoint edit sets → concurrent worktrees are safe. CAVEAT:
   `lower_generics.gg`/`lower_types.gg` IMPORT (read-only) functions from
   `lower_expr.gg`/`lir_lower.gg` (e.g. `resource_meta_for`). That's not a
   definition conflict, but if the Box track changes a SIGNATURE that #39 imports,
   the orchestrator must catch it at serial integration + re-gate. Integrate the
   two tracks SERIALLY (re-gate between), not in one merge.

4. **Oracle wire-in point (resolve in step 2a).** Pass-3 (`ab152a65`) verified
   that **EVERY caller of `lookup_expr_gir_type` is already a method-call context**
   (`lower_generics.gg` ~`108/126` receivers, `lower_expr.gg` method-return-type,
   `lower.gg` receiver-mutability) — there are NO non-method callers needing a
   "fallback". So make `lookup_expr_gir_type` route through the recursive oracle as
   the SOLE path for method-chain links; the `span.start`-table read is at most a
   theoretical-future fallback, not a live one. Still verify line-by-line that
   each existing single-link caller gets the SAME type it does today before
   widening. (Recursion terminates: `infer_expr_ast_type` base-cases on
   `Identifier`/`SelfExpr`.)

5. **THE #1 BOOTSTRAP RISK (pass-3 `ab152a65`): the oracle must return IDENTICAL
   types during the proto-discovery snapshot/restore as in the main flow.** The
   snapshot/restore (`typecheck.gg` ~`1041-1046`) saves/restores `expr_types` to
   isolate method-targ recording. If the oracle path makes `lookup_expr_gir_type`
   return a DIFFERENT type during snapshot vs main flow, proto-discovery silently
   records wrong method-targs → the lowerer mangles wrong symbols → a SUBTLE
   bootstrap regression (green build, wrong output). Mitigation is mandatory and
   already in step 2b ("verify line-by-line") — do NOT merge stages 2a→2b→2c; gate
   `self_host_bootstrap_fixed_point` after EACH of 2a, 2b, 2c (not only at the end).
