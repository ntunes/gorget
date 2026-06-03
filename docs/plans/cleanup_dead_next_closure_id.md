# CLEANUP brief — remove the dead `LowerCtx.next_closure_id` field (self-host)

## Goal
Delete the never-read `next_closure_id` field from `struct LowerCtx`. Pure
output-neutral cleanup (CLAUDE.md "self-host as the elegance showcase" — dead
fields are debt). NO parity/behavior change.

## Verified dead (gorget-1 tip `eb649a04`)
- `struct LowerCtx` (lower.gg:170) declares `int next_closure_id` (lower.gg:177),
  the **6th** positional field (after `named_locals`, before
  `loop_continue_stack`).
- It is **never read or written** as `.next_closure_id` anywhere — grep for
  `\.next_closure_id|next_closure_id =|next_closure_id:` returns ZERO hits.
- All closure-id allocation goes through the **separate** module-level function
  `gmod_next_closure_id(GirModule &gmod)` (lower.gg:3541), called at
  :5710/:5919/:10293 — NOT the field. (Step A made `lower_expr` +
  `gmod_next_closure_id` the sole id source; the ctx field was orphaned.)

## The fix (mechanical)
1. Delete the field line lower.gg:177 (`    int next_closure_id`).
2. `LowerCtx` is constructed **positionally** at exactly 4 sites — remove the 6th
   positional argument (the `0` that sits between the `named_locals` arg and the
   `loop_continue_stack` arg) at each:
   - lower.gg:8899 `LowerCtx([], !block_insts, !block_terms, 0, !_named_locals, 0, !_lc_stk, …)`
   - lower.gg:9093 `LowerCtx([], [], [], 0, !_named_locals2, 0, !_lc_stk2, …)`
   - lower.gg:10347 `LowerCtx([], !cblock_insts, !cblock_terms, 0, !_cnamed, 0, !_clc, …)`
   - lower.gg:11891 `LowerCtx([], [], [], 0, !_named_locals3, 0, !_lc_stk3, …)`
   ⚠ **There are TWO `0`s in a row context** (`…, 0, !_named_locals, 0, !_lc_stk`).
   The FIRST `0` is `current_block` (4th field) — KEEP it. The SECOND `0`
   (immediately after the `named_locals` arg, immediately before the
   `loop_*_stack` arg) is `next_closure_id` — REMOVE that one. Verify against the
   field order in the struct decl (lower.gg:170–178) for each site.
3. lower.gg:6212 is a COMMENT illustrating `LowerCtx([], ..., {},` — not a real
   ctor; leave it (or update if it lists the field, but it's illustrative).

## Scope discipline
- ONLY `tests/fixtures/self_host_lowerer/lower.gg`. Do NOT touch
  `gmod_next_closure_id` (the function — it's live and correct).
- Do NOT renumber/touch any other field.

## File zone
ONLY `tests/fixtures/self_host_lowerer/lower.gg`, hunks at line 177 + the 4 ctor
sites (8899/9093/10347/11891) — disjoint from the FIDELITY chain's EDo/EBlock
hunks (5845/6015).

## Gates (force-rebuild the driver first —
`rm tests/fixtures/self_host_lowerer/driver{,.c}`)
- Driver builds clean (positional ctor arity must match — a wrong field removed
  = a type error or a SILENT field-shift miscompile, so this is the real risk;
  the build + the byte-identical gates below catch it).
- `self_host_bootstrap_fixed_point` GREEN (byte-identical reconverge — the
  decisive neutrality gate; the driver IS lowered by itself so a field-shift
  would corrupt it).
- `lowerer_comparison` UNCHANGED (≥954), `c_emit_comparison` UNCHANGED (≥883).
- `self_host_runtime` 284/0 UNCHANGED, `runtime_diff` parity 284 UNCHANGED.
