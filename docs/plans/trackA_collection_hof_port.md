# Track A — collection-HOF adapter inlining (eager Vector/Set/Dict `map`/`filter`/`fold`/…) — self-host parity

## Goal
Flip the dominant self-host parity cluster: ~30 fixtures CC-FAILing with
`incompatible types when assigning to type 'GorgetArray' … from type 'int'`
(and WRONG-OUTPUT siblings like `iter_collect_set` 4-vs-7). Baseline this round:
**PARITY = 565/1029 = 54.9%** (`/tmp/parity-baseline-15966.log`). This is the
round's primary parity track. Reference-grade: it mirrors Rust gg's canonical
path for DIRECT collection HOFs.

## REFUTED premise (do NOT repeat it)
This is **NOT** a return-type / `expr_link_types`-oracle fix (the prior brief
hypothesis, scout-REFUTED). `infer_method_return_type` already returns
`GorgetArray` for map/filter (`lower_types.gg:~1658-1662`); the destination temp
is correctly typed in the emitted C. The real defect: the self-host's method
dispatch (`lower_expr.gg:~1463`, `full_name = recv_type_name + "__" + mname`)
emits `GICallExtern(Vector__int64_t__filter, …)` to a symbol it **never defines
and never declares** → C implicit-declares it returning `int` → "from int".
Rust gg has no such symbol either: it **inlines** these adapters to `GorgetArray`
loops via `Inst::HofExpand` (`src/lir/lower/operands.rs:263-298`) → BIR loop
scaffold (`src/bir/lower.rs:948` `emit_hof_loop_scaffold`, Dict/Set variants
`:2491`/`:3233`). The self-host has **no BIR / no HofExpand** — the whole
adapter-expansion machinery is missing. This is a **feature port**, unrelated to
#39's oracle.

## The fix: inline expansion via the existing comprehension machinery
The self-host's cleanest port path (simpler than porting Rust's BIR scaffold) is
to reuse its list-comprehension lowering:
- `v.map(f)`   ≡ `[f(x) for x in v]`
- `v.filter(p)` ≡ `[x for x in v if p(x)]`
- `v.fold(init, f)` ≡ accumulate over a loop; `v.reduce(f)` ≡ fold without an init (first elem seeds)
- `v.collect()` on a VectorIter ≡ drain to array
Reuses `comp_make_acc` + `comp_synth_body` (`lower_expr.gg:~2855/2872`) +
`lower_for_vector` (`lower_loops.gg:~206`). The closure is **inlined via AST
substitution** (the `4158530b` `closure_extract` approach: the closure's param
name binds the loop var and its body expr is reused verbatim — NOT a `__callable_N`
indirect dispatch). `fold`'s accumulator type is threaded from the init expr's
`type_id`. A single `try_lower_collection_hof` + a 1-call hook in the EMethodCall path.

## START FROM THE PARKED T2 COMMIT `4158530b` (reviewed SIGN OFF, round 1)
It already implements this approach for Vector `map`/`filter`/`fold`/`reduce`/
`each`/`any`/`all` (+259 lines, `lower_expr.gg` only; flips `vector_higher_order`).
FIRST: `git cherry-pick 4158530b` (or `git show 4158530b -- …lower_expr.gg | git apply`)
in your worktree. ⚠ It and the landed Box commit (`719aae66`) both touch
`lower_expr.gg` — resolve any overlap (different regions: T2's HOF intercept vs
Box's `Box.new` mangling). RE-VERIFY it still flips `vector_higher_order`
end-to-end on the current tree + keeps `fixed_point` green. If it does NOT apply
cleanly / no longer flips, fall back to building the intercept fresh (the scout's
prototype proved map/filter via this same machinery, +3). Then EXTEND.

## Staged scope — flip-prove each stage end-to-end (compile+run+diff vs `gg run`), gate fixed_point per stage
1. **Foundation (from `4158530b`):** Vector map/filter/fold/reduce/any/all/each. Re-verify.
2. **Vector collect / flat_map** — the dominant SECOND blocker (most ~19 Vector fixtures link-fail on `fold`/`collect` after map/filter).
3. **Closure-param type hints** — untyped closures `(x): x` and struct-element closures hit a param ABI mismatch (`test_vector_bool` bool-element, `test_vector_of_structs`). Mirror Rust's `closure_param_type_hints` (`src/ir/lowering/exprs/methods.rs:~1904`): infer the closure param type from the receiver's element type.
4. **Vector sort/sorted/unique/enumerate/zip** — more loop variants (zip/windows/chunks lower priority).
5. **Set** (filter/fold/union/intersection/difference) + **Dict** (filter/fold) — separate loop scaffolds (Set binary ops are non-trivial; mirror Rust's `emit_{set,dict}_hof_loop_scaffold`).

## Reference-grade requirements
- Detect the HOF receiver via the TYPED `collection_kind` (`resource_meta_for`), NOT a name-match on `Vector__`/the receiver string. The method name is the op selector (acceptable — it's the operation, not a meaning-decision). Mirror Rust's `HofOp` set.
- Centralize at the one `try_lower_collection_hof` intercept (fix the class), not per-method patches scattered through dispatch.

## Honest yield + exclusions
~30 GENUINE cluster fixtures (NOT 50). EXCLUDE these ~7 orthogonal (they belong to OTHER tracks — do NOT count them, do NOT chase them here): `snag43_throws_call_inline_arg` (generic-param leak), `serialize_collections` (Box trait-object method), `stdlib_io_file_writer`/`stdlib_io_stdout_typed` (primitive `int64_t__bytes`), `test_result_chaining` (`Ok__map`), `test_option_chaining`/`test_option_all` (Option-combinators). Report the MEASURED flip count per stage; never estimate.

## Zone + disjointness
`lower_expr.gg` ONLY (helpers read-only from `lower_loops.gg`/`lower_types.gg`).
DISJOINT from #39 (`expr_link_types` — unrelated; that's return-type inference,
already correct), T4 (`src/semantic`), DOC, and the CLEANUP track. (Overlaps the
landed Box work in `lower_expr.gg` only at cherry-pick time — resolve once.)

## Gates
`cargo build`, `cargo test --lib`, `cargo test --test lints`, `self_host_runtime`
(+ new snapshots for flipped fixtures), **`self_host_bootstrap_fixed_point`**
(`GG_BUILD_TIMEOUT_SECS=600`) per stage — scout CONFIRMED safe (the self-host
driver uses lazy `.iter().map()`/`MapIter`, NOT direct `Vector[T].map()`, so the
intercept doesn't perturb self-compilation; re-confirm). The scout's prototype
had **ZERO new CC-FAIL / ZERO new WRONG-OUTPUT** — hold that bar.

## Honesty clause
Stage Vector-first. A partial landing (Vector map/filter/fold/reduce/collect)
that flips +N cleanly with fixed_point green is a VALID landing; do NOT force
Set/Dict if they balloon. If a stage regresses fixed_point or introduces a new
failure, STOP and report.

## Brief-review pass-1 folds (a6dfc6b3 — mostly verified; one reservation REFUTED)
1. **`4158530b` is NOT a "retired branch" (reviewer conflation, git-refuted).** The
   reviewer conflated it with `eb730d49`, which touches ONLY `src/backend/c_lir/emit_types.rs`
   (the RUST C-backend) and removed inline-C *helper-FUNCTION* generators in favor of
   HofExpand. `4158530b` is a SELF-HOST commit doing INLINE expansion via `comp_make_acc`
   + `lower_for_vector` (8 call sites, no helper-function generation) — i.e. the SAME inline
   approach as Rust's CURRENT HofExpand direction. It was parked by the round-1 T2 deferral,
   not retired. Reusing it is ALIGNED with Rust's current strategy. Proceed.
2. **Staging clarity for UNTYPED closures (valid).** Stage 1 (the `4158530b` foundation,
   typed closures) will NOT flip fixtures whose closures are untyped (`(x): x`) or over
   struct elements (`test_vector_bool`, `test_vector_of_structs`) — those need the Stage-3
   closure-param-type-hints. That is EXPECTED, not a regression: don't count those fixtures
   in Stage 1's flip-set, and don't treat their continued CC-FAIL as a Stage-1 failure. If a
   high-value Stage-1/2 fixture turns out to need param-hints, pull the hint plumbing forward.
3. Measure flip count PER STAGE (already mandated). The `/tmp/parity-baseline-15966.log`
   cite is a measurement artifact (not a repo file) — re-measure from `self_host_runtime_diff`.
4. Verify `__callable_N` indirect dispatch threads `fold`'s accumulator type correctly
   (`4158530b` implements fold — flip-prove it on the current tree before extending).
