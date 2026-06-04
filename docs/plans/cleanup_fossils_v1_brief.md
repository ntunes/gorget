# CLEANUP v1 — retire 3 RUN-proven self-host fossils (executor brief)

> 1:1:1:1 CLEANUP slot. From the 2026-06-04 fossil audit (RUN-proven). All three
> are instances of ONE dead bug class — "Vectors don't support in-place field
> mutation / Dict.set through a `&`-borrow" — fixed by the option_ref_borrow
> Phase-4 + Dict-key-hash work. Each fix is **output-neutral** (pure refactor to
> the idiomatic shape). Re-verify against CURRENT source + by RUNNING before editing.
> Deferred fossils #4/#5 are in TODO.md ("🧹 CLEANUP — self-host fossil audit").

## Doctrine
CLAUDE.md "Self-host as the elegance showcase": no defensive code without a LIVE cited bug; when a compiler gap is fixed, its self-host workaround is debt with a stale justification. Each fossil below cites a bug that is now DEAD (proven by RUN + by sibling copies that already use the idiomatic shape). Replace with the shape you'd recommend in `docs/book/`.

## Fossil #1 — resolver `defined_id` extract-modify-putback (DIVERGENT COPY)
`tests/fixtures/self_host_resolver/types.gg:88-91`:
```gorget
        # Extract-modify-putback: Dict.set through &self loses type
        Dict[int, int] cache = !self.defined_cache
        cache.put(def_id, tid)
        self.defined_cache = !cache
        return tid
```
**Dead bug + proof:** the `_typechecker` AND `_lowerer` copies of the SAME method (`types.gg:221`) already use the idiomatic one-liner `self.defined_cache.put(def_id, tid)`. The resolver copy simply never got updated — a divergent stale copy, proven by its own siblings.
**Replace** lines 89-91 (the comment + the 3 lines) with:
```gorget
        self.defined_cache.put(def_id, tid)
        return tid
```
(i.e. make `defined_id`'s tail byte-identical to the typechecker/lowerer copies.)
**Files:** `self_host_resolver/types.gg` only. **Guard:** `resolver_comparison` (DIAGNOSTIC-always-pass → re-check the MATCHED COUNT is unchanged, not just "green").

## Fossil #2 — `refine_local_type` rebuilds a `GirLocal`
`tests/fixtures/self_host_lowerer/lower.gg:4649-4651`:
```gorget
void refine_local_type(LowerCtx &ctx, int lid, int new_type):
    GirLocal old = ctx.locals.get(lid).unwrap()
    ctx.locals.set(lid, GirLocal(new_type, old.name_hint, old.ownership, old.borrow_origin))
```
The comment (`:4646-4648`) cites "Vectors don't support in-place field mutation through `.get()`" — DEAD. The Rust mirror is literally `builder.locals[result_id.0].type_id = ty` (in-place).
**Replace** the body (and update the stale comment) with the in-place mutation:
```gorget
void refine_local_type(LowerCtx &ctx, int lid, int new_type):
    ctx.locals.get(lid).unwrap().type_id = new_type
```
**Files:** `self_host_lowerer/lower.gg`. **Guard:** `self_host_bootstrap_fixed_point` + `self_host_runtime` + `lowerer_comparison`/`c_emit_comparison` (build-breaking; output-neutral, must re-converge + counts unchanged).

## Fossil #3 — `emit` rebuilds `block_insts` (THE HOTTEST PATH — RUN-VERIFY FIRST)
`tests/fixtures/self_host_lowerer/lower.gg:652-657`:
```gorget
void emit(LowerCtx &ctx, Instruction inst):
    # Workaround: can't push to a vector inside a vector via .get().
    # Reconstruct the block_insts with the new instruction appended.
    Vector[Instruction] old = ctx.block_insts.get(ctx.current_block).unwrap()
    old.push(inst)
    ctx.block_insts.set(ctx.current_block, old)
```
The cited bug ("can't push to a vector inside a vector via `.get()`") is DEAD. **Replace** with the in-place push:
```gorget
void emit(LowerCtx &ctx, Instruction inst):
    ctx.block_insts.get(ctx.current_block).unwrap().push(inst)
```
⚠ **This is the single hottest function in the lowerer — every emitted instruction goes through it.** The fix relies on `ctx.block_insts.get(idx).unwrap()` returning a MUTABLE borrow into the STORED vector, so `.push()` mutates in place (NOT a CoW copy that gets discarded). **MANDATORY RUN-VERIFY before trusting it:** if `.push()` mutated a copy, `emit` would silently DROP every instruction → catastrophic miscompile. Two strong signals:
1. Apply ONLY this fossil, rebuild the driver, and confirm `self_host_bootstrap_fixed_point` stays GREEN — a dropped/mis-emitted instruction makes the driver mis-compile itself, so fixed_point would FAIL to converge (or crash). This is the load-bearing guard.
2. Confirm `runtime_diff` parity is UNCHANGED (still 408/940) and `lowerer_comparison`/`c_emit_comparison` counts unchanged.
If ANY of these regress, REVERT fossil #3 (keep #1/#2) and report — the in-place-push idiom may not yet be fully supported for nested-Vector-through-struct-borrow in the hot path.
**Files:** `self_host_lowerer/lower.gg`. **Guard:** as #2.

## Sequence + gates
Apply all three, then gate (`GG_BUILD_TIMEOUT_SECS=600`):
1. `rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`
2. `cargo build` + `cargo test --lib` (1072/0).
3. `self_host_bootstrap_fixed_point` — MUST stay GREEN (the load-bearing guard, esp. for #3).
4. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` — parity MUST be UNCHANGED at **408/940** (output-neutral).
5. `self_host_runtime` — UNCHANGED (no snapshot should change; 0 regressed).
6. `lowerer_comparison` (971) + `c_emit_comparison` (902) — counts UNCHANGED.
7. `resolver_comparison` — re-read the MATCHED COUNT (diagnostic-always-pass); MUST be unchanged by #1.
   (If the driver build picks up #1 — the resolver dir isn't compiled by the lowerer driver, but `resolver_comparison` builds its own driver; run it.)
(PARENT runs the full `cargo test --test integration` at integration.)

This is OUTPUT-NEUTRAL: every gate must show NO change except the code being cleaner. Any parity/count delta = a fossil whose bug wasn't actually dead → revert that one + report.

## Worktree discipline (NON-NEGOTIABLE)
- `pwd` + `git rev-parse --show-toplevel` FIRST; inside YOUR worktree, NOT `/workspace/gorget-1`. `git merge --ff-only gorget-1` FIRST.
- Stage ONLY by name: `git add tests/fixtures/self_host_resolver/types.gg tests/fixtures/self_host_lowerer/lower.gg docs/plans/cleanup_fossils_v1_brief.md` — NEVER `git add -a`/`.`.
- Commit on your branch; do NOT merge to gorget-1.
- ⚠ FILE-DISJOINT from the in-flight ③(c) fidelity chain (which touches `lir_lower.gg` + `lir_codegen.gg`, NOT `lower.gg`/resolver). Stay out of `lir_lower.gg`/`lir_codegen.gg`.
