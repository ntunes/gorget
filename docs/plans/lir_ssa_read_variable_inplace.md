# Brief: `read_variable` prepend-rebuild → in-place `insert(0, …)` (perf, self-host showcase)

**Track:** perf (Round-3 1:1:1). **File touched:** `tests/fixtures/self_host_lowerer/lir_ssa.gg` ONLY (single-copy — see §Validation step 3).
**Goal:** Remove the per-zero-init whole-block clone in SSA `read_variable` by mutating the live block's inst list in place, mirroring the proven-zero-clone `add_block_param` shape in the same file. **Output-neutral.**

## Root cause (CoW terms — verified against docs/language-design.md §3.2–3.4 + docs/book/11-ownership.md)

`read_variable`'s entry-block zero-init branch (`lir_ssa.gg:222–257`) builds a fresh `new_insts`
seeded with the const, then pushes every existing inst and writes the list back:

```gorget
LirBlock blk = f.blocks.get(bb).unwrap()
... new_insts = [IBoolConst(val, false)] ...
while ni < blk.insts.len():
    new_insts.push(blk.insts.get(ni).unwrap())   # ← push is a CONSUMING POSITION (collection must own).
    ni += 1                                       #   source is a Ptr read from a collection → CoW "Borrow → clone before call"
                                                  #   → deep-clones each LirInst (incl. its embedded Vector[int] arg-lists)
blk.insts = new_insts
f.blocks.set(bb, blk)                             # + a per-block set-back clone
```

Every existing inst is deep-cloned, and the whole branch can run once per undefined promotable
slot at the entry block (each call re-clones the *current* (growing) list). This is one of the
three per-instruction whole-list-rebuild sites the handover pinned as the 1.5B-array_clone /
~4GB-RSS source (TODO.md "🚨 (High, MEMORY BUG …)"). **Move-on-last-use is unavailable** here: the
source is borrowed from `blk.insts`, which must stay valid — you cannot move out of a borrowed
collection element. The only way to eliminate the clone is to stop rebuilding and mutate in place.

## The fix

Replace each of the four branches' rebuild-and-set with a single in-place prepend:

```gorget
elif bb_preds.len() == 0:        # (existing guard)
    int val = lir_fn_next_value(&f)
    LirSlot s = f.slots.get(slot).unwrap()
    if s.ty == LT_BOOL:
        f.blocks.get(bb).unwrap().insts.insert(0, IBoolConst(val, false))
    elif s.ty == LT_F32 or s.ty == LT_F64:
        f.blocks.get(bb).unwrap().insts.insert(0, IFConst(val, s.ty, 0))
    elif s.ty == LT_PTR:
        f.blocks.get(bb).unwrap().insts.insert(0, INullPtr(val))
    else:
        f.blocks.get(bb).unwrap().insts.insert(0, IIConst(val, s.ty, 0))
    current_def.put(key, val)
    return val
```

Removed vs current: the `LirBlock blk = f.blocks.get(bb).unwrap()` fetch (no longer needed — we
never read `blk.insts.len()`), the four `new_insts` rebuild loops, and the `f.blocks.set(bb, blk)`
set-back. Kept: `lir_fn_next_value(&f)`, `LirSlot s = f.slots.get(slot).unwrap()` (still read for
`s.ty`), `current_def.put(key, val)`, `return val`.

## Equivalence detail — the `process_block` write-back clobber (verified pass-1)

`read_variable`'s entry-block (0-pred) branch is reached from two call paths:

- **From `process_block` (`lir_ssa.gg:189`, during an `ISlotLoad`)** while `process_block(bb)` is mid-iteration
  of that SAME `bb`. `process_block` builds a parallel `new_insts` from the ORIGINAL `blk.insts` and at
  `:201–203` re-fetches the block and does `final_blk.insts = new_insts` — which **overwrites** whatever
  `read_variable` prepended (the const is not in `new_insts`). The `:201` re-fetch exists to preserve
  `add_block_param`'s `.params` push (a different field); `.insts` is replaced wholesale.
- **From `patch_terminators` (`lir_ssa.gg:481`)**, which runs AFTER all `process_block` calls — no concurrent
  `new_insts` rebuild, so the prepended const survives here.

**This change is therefore output-neutral, NOT a behavior fix.** At the moment `read_variable` returns,
the current `blk = get; blk.insts = [const]++existing; f.blocks.set(bb, blk)` form and the proposed
`f.blocks.get(bb).unwrap().insts.insert(0, const)` form leave `f.blocks[bb].insts` byte-identical
(const at index 0, existing after). The downstream clobber (`process_block` path) or survival
(`patch_terminators` path) then acts identically on both. Do NOT claim the insert is observable from the
`process_block(entry)` path — it is clobbered there in BOTH old and new forms, by design; only the
returned/memoized value-id (`current_def.put(key, val)`) flows downstream from the `process_block` path.
(Whether the const-inst being clobbered-but-its-value-id-still-referenced is a pre-existing latent issue
is OUT OF SCOPE — it is identical before and after this change; the bootstrap currently passes with it.)

## Why this is zero-clone and output-neutral (per the CoW contract)

- `f.blocks.get(bb).unwrap()` → Ptr to the **live** block (collection read returns a reference, §3.2; zero cost).
- `.insts` → field access through the ref → Ptr to the live insts vector (zero cost, resolves in place).
- `.insert(0, …)` → mutation through the mutable borrow (`f` is `LirFunction &f`); the element arg is a
  **freshly-constructed owned enum temp** → "move from temp — zero cost"; existing elements shift via a
  shallow memmove inside `gorget_array_insert` (NO per-element clone — `gorget_array_insert` memcpy's bytes,
  it does not call `elem_clone`).
- This is byte-identical in EFFECT to `[const] ++ existing`: `insert(0, const)` puts `const` at index 0 and
  shifts the rest right by one. Same resulting order the rebuild produced.
- It is the SAME shape as `add_block_param` at `lir_ssa.gg:289` (`f.blocks.get(bb).unwrap().params.push(...)`),
  whose docstring (`:269–277`) documents it as the proven zero-clone mutate-through-borrow form and which
  shipped as a committed conversion this session.

## Load-bearing facts (verified this session, cite on review)

1. Rust type-checker knows `Vector.insert`: `src/semantic/typecheck.rs:4838` (→ void) and
   `src/ir/lowering/builtins.rs:251` (`BuiltinMethodDecl{name:"insert", runtime_callee:"gorget_array_insert",
   self_conv:MutBorrow, is_mutating:true, returns_view:false, params:[I64_TYPE, elem], return:void}`).
   So the stage-0 (Rust) build of the self-host compiles `vec.insert(0, x)`.
2. Self-host handles `insert` end-to-end: runtime map `lir_lower.gg:1449–1450` (`case "insert": return "gorget_array_insert"`),
   storage-arg-by-ptr `lir_lower.gg:1942` (`gorget_array_insert and arg_idx == 2`), ownership/consume
   handling `lower.gg:531/537/3437/7824`. So the self-host (stage-1+) compiles it too.
3. Runtime `gorget_array_insert` exists: `src/backend/c/c_runtime.rs:5607–5625` — grows cap, memmove
   elements up, memcpy elem at index, len++. No `elem_clone` call.
4. ⚠ RISK: there is currently **no `.insert(` Vector call site anywhere in self-host or lib `.gg`**
   (grep clean). This conversion is the FIRST. "Wired + type-checks" ≠ "lowers correctly through the
   self-compile." Validation MUST confirm the bootstrap (self-host compiling itself) produces correct
   output, not just that stage-0 builds. The #5/#6 truncation class (a builtin mis-typed at a writer
   site → empty Str/`sizeof(` corruption) is the cautionary precedent for "wired but mis-lowered."

## Validation gates (parent drives the heavy ones)

1. `cargo build --release` clean.
2. `cargo test --lib --release` — expect 1060/1062 (2 pre-existing `lir::validate` release `should_panic`).
3. `lir_ssa.gg` is SINGLE-COPY: `find tests/fixtures -name lir_ssa.gg` returns exactly one regular file,
   `tests/fixtures/self_host_lowerer/lir_ssa.gg` (md5 `7fb5338c…`; no copy in typechecker/parser/resolver,
   not a symlink) — verified pass-1. Apply the change there only. (Re-confirm with `find` before editing in
   case the tree changed, but expect one copy.)
4. `self_host_bootstrap` GREEN (use `GG_STAGE1_TIMEOUT_SECS=900`).
5. `self_host_bootstrap_fixed_point` GREEN (the gate; hardcoded 600s/stage).
6. `lowerer_comparison` — count-identical to baseline (diagnostic-always-pass; read the printed counts).
7. **Measurement (the point of the round):** use the canonical harness `scripts/self_host_mem_baseline.sh`
   (builds `self_host_lowerer/driver.gg` with `--clone-stats`, runs it over its own source with `--lir-c`,
   captures peak RSS + `array_clone` + alloc counts to JSON). Workflow: `scripts/self_host_mem_baseline.sh --out
   /tmp/before.json` at the integration tip → apply change → delete the driver binary/C to force self-host
   rebuild → `scripts/self_host_mem_baseline.sh --compare /tmp/before.json`. Report the `array_clone` + peak-RSS
   delta to attribute read_variable's share of the 1.5B. (If small, that informs whether process_block/apply —
   the harder, new-primitive sites — are worth the next round.) `--clone-stats` is `gg build`'s real flag
   (`src/main.rs:374`); the line is `[clone-stats] array_clone=N` (`src/lir/mod.rs:1752`).

## Explicitly OUT of scope (deferred, with reasons)

- `process_block` filter-rebuild (`:175–203`): in-place removal is entangled with `read_variable`
  reentrancy mutating the same block (the `:199–203` re-fetch exists for that) → index-shift hazard.
  Needs a `retain`-style primitive or careful index management. Separate round.
- `apply_value_substitutions` rewrite (`:303–312`): clone is at `substitute_inst`'s by-value arg; a `.set`
  was tried and regressed (+161K, per TODO). Needs in-place enum-field rewrite through a mutable borrow.
  Separate round.

## Anti-goals / discipline

- Do NOT introduce a new runtime primitive for this site (not needed).
- Do NOT touch `src/` (Rust) — `insert` is already wired both sides.
- Do NOT reshape surrounding code to dodge any gap; if `.insert(0,…)` mis-lowers in the self-compile,
  that is a NEW compiler bug → file a fixture + sharp TODO, do not work around it.
