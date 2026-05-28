# Self-Host SSA Cleanup — Scoping (port `run_ssa` to `lower_gir_to_lir`?)

> **Bottom line (rev 1, 2026-05-28):** the brief's framing is wrong about the
> ground state — the self-host **already has** a faithful port of Rust's
> `construct_ssa` (`tests/fixtures/self_host_lowerer/lir_ssa.gg`, 639 lines),
> wired into the driver after `lower_gir_to_lir` and before `elaborate_drops` /
> `generate_c` (`tests/fixtures/self_host_lowerer/driver.gg:87` calls `run_ssa(&lir)`
> which loops calling `construct_ssa(&func)` per function at `:112`). The
> commit-message claim that the three recent crashes happened because
> "the self-host has no SSA cleanup" is **misleading**. The crashes happen at
> non-promotable slots — slots that Rust's `construct_ssa` *also wouldn't
> promote*. Both sides apply the same promotability filter
> (`src/lir/ssa.rs:43-63` ≡ `lir_ssa.gg:21-48`). The real difference is
> upstream: Rust's GIR lowering keeps slot types accurate via `expected_type`
> propagation, so the bad stores never get emitted in the first place; the
> self-host's lowering produces mistyped slots, hits codegen "give-up"
> branches (`lir_codegen.gg:2810 = NULL;`, `:2814 = 0;`), and the SSA pass —
> already running — has no way to fix it because the bad slots are
> non-promotable.
>
> **Recommendation: DEFER the brief-as-written. ALTERNATIVE = port the bits
> of Rust's `expected_type` propagation that the self-host is still missing
> (the actual class root).** Details in §7.

## Table of contents

1. Ground-state correction — self-host already has SSA
2. What Rust's `construct_ssa` actually does (file:line enumeration)
3. Per-crash verification of the "SSA-collapsible" claim
4. Survey of 2-3 nearby fixes (`4ce2be0f`, `2041d255`/`30f882ec`)
5. The minimal-port design *as the brief asked* (for reference)
6. Blast radius + regression gates (for the as-asked port)
7. What this scoping uncovered — the actual reference-grade fix is upstream
8. Recommendation + immediate next step
9. Effort/risk/hidden-complications table
10. What an SSA pass would NOT fix

## 1. Ground-state correction — self-host already has SSA

The brief opens with: *"The self-host pipeline has NO SSA-cleanup pass between
`lower_gir_to_lir` and `generate_c`, so each bad store survives."* This is
false. Confirmed by `git log --oneline -- tests/fixtures/self_host_lowerer/lir_ssa.gg`:

- `95e16dc9` (2026-04-13) — "feat: self-host SSA construction pass (Phase 3)"
  shipped the initial 488-line port. Commit body: *"faithful port of
  src/lir/ssa.rs (804 lines Rust)."*
- Subsequent maintenance: `7869f05a` RPO fix, `170da69b` phi persistence,
  `30a396ba` patch_terminators insertion order, `e08f1232` pending_phis retirement.

Current state, `tests/fixtures/self_host_lowerer/lir_ssa.gg` (639 lines):

| Rust `src/lir/ssa.rs` | Self-host `lir_ssa.gg` | Status |
|-----------------------|------------------------|--------|
| `construct_ssa` `:19`         | `construct_ssa` `:601`           | ✅ same entry point |
| `find_promotable_slots` `:43-63` | `find_promotable_slots` `:21-48` | ✅ same criterion (scalar + no SlotAddr) |
| `compute_predecessors` `:94-102` | `compute_predecessors` `:52-71`  | ✅ identical |
| `compute_rpo` `:67-91`        | `compute_rpo` `:84-132`          | ✅ same (iterative form) |
| `process_block` `:156-209`    | `process_block` `:173-203`       | ✅ same SlotStore-drop / SlotLoad-rewrite |
| `read_variable` `:224-267`    | `read_variable` `:215-266`       | ✅ same single/multi-pred / entry-zero logic |
| `add_block_param` `:270-288`  | `add_block_param` `:286-308`     | ✅ same |
| `apply_value_substitutions` (implicit `remove_promoted_instructions` `:292-314`) | `apply_value_substitutions` `:312-330` | ✅ same |
| `patch_terminators` `:318-388`| `patch_terminators` `:470-553`   | ✅ same fixed-point + insertion order |
| `substitute_inst_values` `:480-625` | `substitute_inst` `:345-446` | ✅ same per-variant substitution |
| `substitute_term_values` `:628-674` | `substitute_term` `:448-466`  | ✅ same |

It's wired:

```
tests/fixtures/self_host_lowerer/driver.gg
  16: from lir_ssa import construct_ssa
  79: LirModule lir = lower_gir_to_lir(&gir)
  86: eliminate_dead_globals(&lir)
  87: run_ssa(&lir)                      # ← actual call, every function
  88: validate_lir_after("ssa-construction", &lir)
  95: elaborate_drops(&lir)
 103: print(generate_c(&lir))
 108: void run_ssa(LirModule &m):
 112:     construct_ssa(&func)
```

And `self_host_bootstrap` / `self_host_bootstrap_fixed_point`
(`tests/integration.rs:13695`/`:13868`) exercise driver.gg end-to-end via
`--lir-c`, so every commit lands with SSA running.

**Implication:** the brief's "minimum-viable port" is **already shipped**.
The new question is: *why doesn't the existing pass collapse the three
crashes?* (§3 answers.)

## 2. What Rust's `construct_ssa` actually does

Read `src/lir/ssa.rs` end-to-end. It is **not** "copy-prop + DSE" as the brief
characterises — the file's own doc-comment is correct:

```
src/lir/ssa.rs:1-13:
//! SSA construction pass for LIR.
//! Promotes scalar slots to SSA values using a simplified version of
//! Braun et al. 2013 ("Simple and Efficient Construction of SSA Form").
//!
//! Algorithm:
//! 1. Identify promotable slots (scalar type, no SlotAddr instruction).
//! 2. Walk blocks in order, tracking the current definition of each slot.
//! 3. At slot stores, record the stored value as the new definition.
//! 4. At slot loads, replace with the reaching definition.
//! 5. At merge points (blocks with multiple predecessors), insert block
//!    parameters and patch predecessor terminators with arguments.
//! 6. Remove dead SlotStore/SlotLoad instructions.
```

It is a **Braun 2013 SSA construction pass**. The "copy-prop + DSE" *effects* —
chains of `__sN = __sM; __sM = const; ret __sN` collapsing into `ret const` —
are **emergent from slot promotion**: a promoted slot's stores never reach
codegen (skipped at `:172-178`), and its loads are replaced with the reaching
definition's SSA value (`:179-191`). After promotion, the C backend sees
`v1 = 42; ret v1` directly, with no `__sN`.

Transforms (`file:line`):

1. **Promotable-slot identification** `:43-63`
   - All `LirType::is_scalar()` slots are candidates `:46-51`
   - Any slot referenced by `Inst::SlotAddr` is removed `:53-60`
   → A slot's address being taken (return-by-pointer ABI, `&` operator,
     value/struct-into-pointer rebind) makes it non-promotable, period.

2. **Reverse-postorder block visit** `:67-91, :143`
   - DFS-based post-order, reversed → dominators visit before dominated blocks
   - Critical for `read_variable`'s single-pred recursion to find reaching defs

3. **`SlotStore`-as-definition** `:172-178`
   - `SlotStore { slot, value }` on a promoted slot: record `current_def[(bb,slot)] = resolve_value(value)`, **drop the instruction** (skipped from `new_insts`)
   - The `resolve_value` chase `:212-221` flattens substitution chains

4. **`SlotLoad`-rewrite** `:179-191`
   - `SlotLoad { dst, slot }` on a promoted slot: `reaching = read_variable(slot, bb)`, record `value_subst[dst] = reaching`, drop the instruction
   - All later uses of `dst` are substituted to `reaching`

5. **Phi insertion at merges** `:263-266, :270-288`
   - Multiple predecessors → `add_block_param(slot, bb)` synthesises a new block-param ValueId
   - Lazy/iterative — recorded in `incomplete_phis`, resolved in `patch_terminators` `:318-388`

6. **Value substitution sweep** `:292-314, :480-625, :628-674`
   - After all blocks processed, `substitute_inst_values` / `substitute_term_values` rewrite every value reference per the accumulated `value_subst`

7. **Terminator patching** `:318-388`
   - For each merge with phis, walk preds; reaching-def-resolve each pred (creating cascading phis as needed); append args to Jump/Branch/Switch terminators

**Notable mechanics that DON'T exist:**

- No standalone "copy-prop" pass over `__sN = __sM` chains. The collapse is a
  byproduct of step 4 substituting all `dst` uses with `reaching`.
- No standalone "DSE" pass over `__sN = const` followed by no reads. The
  collapse is a byproduct of step 3 dropping the SlotStore instruction itself.
- No type-driven slot rewriting. The pass does NOT change slot types or
  insert type-coercion. **If the slot type is wrong, SSA can't fix it.**
- No promotion of aggregate slots, and no promotion of address-taken slots.

The self-host `lir_ssa.gg` mirrors all seven transforms (table in §1).

## 3. Per-crash verification of the SSA-collapsible claim

**Crash A — `6e49ead3` parse_int `Error(Empty())` return SIGSEGV.**

Commit body says (`6e49ead3:20-22`):
> The Rust-gg-compiled driver tolerated the same mistyping (its run_ssa
> copy-prop collapses the dead Ptr-slot store); the self-host
> lower_gir_to_lir has no SSA cleanup, so s1bin emitted the literal NULL store.

This is **doubly wrong**. (i) The self-host DOES have SSA. (ii) The slot in
question is non-promotable, so neither side's SSA touches it.

The actual mechanism, traced from the commit body and `lir_codegen.gg:2810`:

1. Self-host `lower.gg` types the `Error(e)` result local as
   `GtNamed("Error") → LT_PTR` (the standalone variant name resolves to a
   scalar/pointer slot — 8 bytes).
2. `try_lower_prelude_variant` bails because the LIR slot isn't aggregate.
3. The constructed Result value (aggregate, e.g. 40 bytes) gets stored into
   the Ptr slot via `ISlotStore(slot, aggregate_value, false)`.
4. `lir_codegen.gg:2804-2810` hits *"Pointer slot (void* / Ptr) receiving an
   aggregate value — self-host lost track of slot type"* and emits
   `__sN = NULL;` as a give-up.
5. The next line is the return-slot copy: `memcpy(&__s0, __vM, ...)` where
   `__vM` is the just-NULLed Ptr value. memcpy-from-NULL → SIGSEGV.

Critical: is `__sN` (the result slot) promotable? **No.** The Ptr-into-Ptr
store at step 4 is downstream of code paths that take the slot's address
(at minimum, the `memcpy(&__s0, __vM, ...)` at step 5 implies `__s0` is
address-taken via `IFieldPtr`/`ISlotAddr`-equivalent). And inside the
prelude-variant emission, `IEnumInit` has a `target` operand which is itself
a slot address. So `__sN` and `__s0` both carry SlotAddr references → both
filtered out of the promotable set at `find_promotable_slots`
(`lir_ssa.gg:21-48` / `src/lir/ssa.rs:43-63`).

**Would Rust's `run_ssa` collapse this if it ran on the self-host's bad LIR?
No.** The slots are non-promotable in both compilers. Rust escapes the crash
because Rust's GIR lowering at `src/ir/lowering/stmts/mod.rs:1523-1526` sets
`func_state.expected_type = locals[0].type_id` before calling `lower_expr`,
which causes the variant constructor to be typed with the parent enum's full
monomorphised name (`Result__int64_t__PErr`), producing a correctly-aggregate-typed
slot. Step 3 never happens; step 4's give-up never triggers; no NULL store.

The fix (`6e49ead3`, `lower.gg` SReturn handler) ports exactly this Rust
mechanism — `expected_type = locals[0].type_id` at SReturn for prelude-variant
ctors. It's a **GIR-lowering fix, not an SSA fix.**

**Verdict A:** the SSA-collapse claim is **wrong** for crash A. The actual root
fix is upstream of SSA — `expected_type` propagation in GIR lowering.

---

**Crash B — `dfe64fb7` parse_dot_expr NULL `!`-move-arg SIGSEGV.**

Commit body: *"GIAssign emitted `__s3=NULL` (the aggregate-into-pointer
give-up) for `_3 = move _10`. Rust keeps values register-resident; SSA
collapses the dead store."*

The fix is `lir_lower.gg` `lower_instruction` GIAssign handler: extend the
Ptr-dst ← value-src rebind (which already existed for `OpBorrow`) to
`OpMove`/`OpCopy`. The rebind emits:

```
lir_push_inst(&f, bb, ISlotAddr(addr_val, src_slot))   ← (!) takes src_slot's address
lir_push_inst(&f, bb, ISlotStore(slot, addr_val, false))
```

Look at what this means for SSA: the fix **adds** an `ISlotAddr(src_slot)`.
Per `find_promotable_slots`, this **removes** `src_slot` from the promotable
set. The store into `slot` is also not eligible because `slot` is a
`!`-param-backed loop variable — its address is taken elsewhere too. So the
pre-fix store `ISlotStore(slot, value, false)` where `value` was the
value-loaded aggregate would have been to a non-promotable slot, and so was
the post-fix `ISlotStore(slot, addr_val, false)`. SSA touches neither
before nor after.

The Rust side does **exactly the same thing** in its lowering (the commit
calls out *"mirroring the driver's back-edge phi `__bp = &result`"*). Rust's
own `ir/lowering` emits ISlotAddr+ISlotStore for the rebind, then `run_ssa`
sees those instructions and skips them. The store sticks, but it stores a
*correct* address; codegen emits `lhs_ptr = &result;` and life is good.

The self-host bug pre-fix was: the OpMove/OpCopy branch did NOT emit the
ISlotAddr rebind. Instead it value-loaded the aggregate and stored the
8-byte head into the Ptr slot, which `lir_codegen.gg:2810` turned into
`= NULL;`. **Same codegen give-up as crash A.**

**Verdict B:** the SSA-collapse claim is **wrong** for crash B. The fix is at
the writer site (GIAssign handler in `lir_lower.gg`). Rust's `run_ssa` would
NOT have collapsed the buggy LIR; Rust just emits different LIR upstream.

---

**Crash C — `dfe64fb7` NULL-Box (`EFieldAccess(Box(lhs),…)`).**

Commit body: *"IBoxAlloc result into a scalar slot → ISlotStore's
'aggregate-into-scalar → 0' give-up → NULL box. Rust keeps the box value
live in a register and never round-trips it through a typed slot."*

The fix is `lir_lower.gg` `emit_box_alloc`: when `IBoxAlloc` produces a
`Box__<inner>`-typed (pointer-represented but LIR-classified aggregate) value
but the GIR-defaulted slot is scalar `i64`, **retype the slot** to its
proper `Box__<inner>` struct type *before* the `ISlotStore`. (Also adds a
companion `lir_type_is_box` guard in `lir_codegen.gg` to short-circuit the
"aggregate-into-Ptr → NULL" give-up for Box-typed values.)

Pre-fix the slot was scalar i64 → `lir_codegen.gg:2811-2814` hit *"Scalar slot
(I64/I32/etc.) receiving an aggregate value"* → emitted `__sN = 0;`. Same
class of codegen give-up as A/B.

For SSA: a scalar i64 slot, no SlotAddr → **promotable.** So pre-fix, the
SlotStore would actually be dropped by SSA. But the value `dst` of IBoxAlloc
gets substituted to the (correct, non-NULL) IBoxAlloc result everywhere…
*at the SSA layer*. The catch: `lir_codegen.gg`'s ISlotStore-give-up for
this slot fires BEFORE SSA's substitution sweep would have erased it? **No —
SSA runs at line 87 of driver.gg, BEFORE generate_c at line 103.**

So actually for crash C: SSA *would* drop the store; *but* the
`lir_codegen` give-up doesn't run on a non-existent SSA-removed instruction.
The remaining question is: does substituting `IBoxAlloc.dst` to its
SSA-promoted value at all use sites prevent the crash?

Reading the pre-fix path more carefully: post-SSA, the i64 slot is gone, and
all reads of "the box" become reads of `IBoxAlloc.dst` — a `Box__<inner>`-typed
value held as a register-style ValueId. Subsequent use sites (e.g. the
`EFieldAccess(Box(lhs), …)` variant ctor's box-arg read) would consume the
correct ValueId, not a NULLed slot. **So crash C IS the one case where SSA
arguably could have helped, IF the rest of the lowering generated
correctly-typed value references on top of the now-deleted slot.**

But there's a catch — the codegen give-up at `lir_codegen.gg:2811-2814` is an
**ISlotStore-time** give-up. Post-SSA, the ISlotStore is gone, so the give-up
doesn't fire. The IBoxAlloc result flows directly into downstream uses via
value substitution. Codegen emits `__vN = gorget_box_alloc(…);` and downstream
uses of `__vN` see the correct pointer.

So why does crash C still happen with self-host SSA running? Two possibilities:

(i) The IBoxAlloc result's `LirType` (`Box__<inner>`) is not propagated into the
   `value_subst` rewrite path — the substitution copies the ValueId but
   downstream consumers query the value's type from `val_types` (a per-function
   side table populated during emission). If the side table records the
   IBoxAlloc producer's type as `i64` (matching its destination slot's pre-fix
   type), then downstream use sites still treat the value as i64 and the
   "aggregate-into-Ptr → NULL" give-up fires at the next ISlotStore that
   consumes this value (e.g. when packing it into the parent
   `EFieldAccess.lhs` field). The same codegen give-up — just one layer
   downstream. **This is the more likely root.**

(ii) The slot in question has an ISlotAddr emitted elsewhere (some `&box.0`
   path or a temp address-take during the EFieldAccess lowering), making it
   non-promotable. Then SSA leaves the bad ISlotStore alone and the give-up
   fires immediately.

Either way, the **fix is upstream** — make the slot type correct so codegen
doesn't hit the give-up, regardless of whether SSA promotes the slot. That's
what `emit_box_alloc` does. Even if (i) is the actual mechanism (where SSA
*does* drop the immediate store but a downstream consumer hits the same
give-up), the fix at the producer is the right place.

**Verdict C:** the SSA-collapse claim is **partially defensible** — there IS
a plausible SSA-only path that would have collapsed the immediate store. But
the give-up class re-fires at the next downstream consumer because the
producer's typed signature is wrong. The producer-side fix (retype the slot)
is correct and necessary; an SSA-only fix would have been brittle.

---

**Three-crash summary:**

| Crash | SSA-collapsible claim | Actual root | What fixed it |
|-------|----------------------|-------------|---------------|
| A (parse_int Error)  | ❌ wrong — non-promotable slot, same on both sides | GIR-lowering: missing `expected_type` at SReturn | `lower.gg` SReturn retype (`6e49ead3`) |
| B (parse_dot_expr)   | ❌ wrong — non-promotable slot, same on both sides | LIR-lowering: GIAssign missing OpMove/OpCopy rebind | `lir_lower.gg` GIAssign handler (`dfe64fb7`) |
| C (NULL-Box)         | ⚠ partially — SSA could erase immediate store, but downstream give-up still fires | LIR-lowering: IBoxAlloc result slot type wrong | `lir_lower.gg` emit_box_alloc + `lir_codegen.gg` box guard (`dfe64fb7`) |

**Count of crashes a fresh SSA port would close: 0/3.** (The pass already
ships, and the bugs are all upstream of the slots SSA would touch.) The
brief's coverage claim is unsupported by source.

## 4. Survey of nearby fixes

**`2041d255` / `30f882ec` — bug #3b clone-OOM cluster.** Not SSA-related. The
self-host's get-mutate-set idiom (`LirBlock blk = f.blocks.get(bb).unwrap();
blk.insts.push(x); f.blocks.set(bb, blk)`) deep-cloned the entire block on
every instruction emission, O(n²) → ~13 GB. Fix: in-place
`lir_push_inst`/`lir_set_term` helpers that borrow the block instead of
cloning. Compiler-language-runtime bug (CoW contract violation), not SSA.

**`4ce2be0f` — view-returning String method results LoView (large-String
truncation).** Not SSA-related. `.slice()` returns a `cap==0` view into self's
buffer; the result-local ownership was tagged `LoOwned` instead of `LoView`,
so `op_consume` emitted `OpMove` (by-value move) instead of materialising the
view to an owned String. The moved view dangled. Fix: add
`is_string_view_method` (mirrors Rust's `returns_view: true` GorgetString-method
flag) and tag the local `LoView`. Ownership-tagging bug at GIR-lowering level,
not SSA.

**Pattern across all five fixes (A, B, C, bug-#3b, view-trunc):** root cause
is always at a **GIR-lowering writer site** where Rust gg has a typed-metadata
or expected-type-propagation mechanism that the self-host hasn't fully ported.
None of the five is in the class "Rust would have done copy-prop, self-host
doesn't have copy-prop." Four of five never reach SSA's purview; the fifth (C)
might reach it but the better fix is producer-side anyway.

## 5. The minimal-port design *as the brief asked*

For completeness — what would a "port the missing copy-prop + DSE" pass look
like if the existing SSA weren't there? It's documented because the brief
asked for it, but §3 says it wouldn't close the surveyed class.

**Pipeline hook (already present):** `driver.gg:87`
`run_ssa(&lir)` between `eliminate_dead_globals` and `elaborate_drops`.
The `run_ssa` wrapper at `driver.gg:108-114` iterates module functions.

**File layout (already present):** `tests/fixtures/self_host_lowerer/lir_ssa.gg`,
639 lines, mirrors `src/lir/ssa.rs` (~908 lines including tests). Imports
`LirFunction`, `LirBlock`, `LirInst`, `LirTerm`, `LirSlot`, `BlockParam`,
`SwitchCase`, `FieldInit` from `lir`; uses `lir_is_scalar` /
`lir_fn_next_value` helpers.

**Public API (already present):** `void construct_ssa(LirFunction &f)` —
in-place mutation. No return value, no side effects beyond mutating `f.blocks`
and `f.next_value`.

**Skeleton of what the brief implied as "minimum-viable" (this is pseudocode;
the actual self-host implementation is a strict port of Rust):**

```
void ssa_cleanup(LirFunction &f):
    # Phase 1: identify promotable slots
    promotable = scalar_slots_with_no_SlotAddr(f)

    # Phase 2: per block in RPO, walk insts
    rpo = compute_rpo(f)
    for bb in rpo:
        for inst in block(bb).insts:
            match inst:
                case SlotStore(slot, value) if slot in promotable:
                    record current_def[(bb, slot)] = resolve(value)
                    DROP this inst
                case SlotLoad(dst, slot) if slot in promotable:
                    reaching = read_variable(slot, bb)
                    record value_subst[dst] = reaching
                    DROP this inst
                else:
                    keep

    # Phase 3: rewrite all remaining inst/term value-refs via value_subst
    apply_substitutions(f, value_subst)

    # Phase 4: at multi-pred merges, insert block params (phis) +
    # patch predecessor terminators with args
    patch_phis(f)
```

This is exactly what `lir_ssa.gg` already does (compare with §2's enumeration).

## 6. Blast radius + regression gates (for the as-asked port)

Moot — the pass already ships. For reference, the regression net is:

- `self_host_bootstrap` (`tests/integration.rs:13695`) — stage-0 driver builds
  stage-1 .c via SSA + drop-elab + codegen, links, runs on driver.gg, produces
  ≥ half-size output. Catches any regression that breaks the LIR pipeline
  end-to-end. Currently green.
- `self_host_bootstrap_fixed_point` (`tests/integration.rs:13868`) — stage-N
  recompiles stage-N+1 until byte-identical. Currently failing on a
  different pre-existing blocker (NEXT BLOCKER #3 / empty-name ECall →
  NULL Dict key in `lower.gg::lower_call` — String-view/empty-name issue, NOT
  SSA-class; see `docs/plans/drop_emission.md`).
- `lowerer_comparison` — diffs GIR fn-counts vs Rust. **Note: this catches
  GIR-lowering regressions but NOT post-LIR pipeline regressions** (SSA pass
  runs AFTER GIR, only on the LIR module). The brief flagged this honestly.
  Currently green.

If we WERE porting from scratch today, the blast radius is small: only the
LIR `--lir-c` path runs the SSA pass; the default `--emit-gir` path skips it
entirely (driver.gg:75-77, 78). So a buggy SSA port can only break
`self_host_bootstrap[_fixed_point]`, not the existing parser/resolver/
typechecker comparison tests. As-shipped, the pass is mature enough that
post-april-2026 work has been able to lean on it.

## 7. The actual reference-grade fix — port the missing `expected_type` propagation

Per §3, all three surveyed crashes (and the parse_int fix that landed) trace
back to Rust gg's `func_state.expected_type` propagation, which threads the
caller's expected type into expression lowering so constructors, literals,
and aggregates get correctly typed without round-tripping through a
scalar slot.

Rust sites already in the codebase (`grep "expected_type"
src/ir/lowering/stmts/mod.rs src/ir/lowering/exprs/mod.rs`):

```
src/ir/lowering/exprs/mod.rs:198-200    EmptyArrayLiteral materialise via expected_type
src/ir/lowering/exprs/mod.rs:398-406    bare None → tagged Option via expected_type
src/ir/lowering/exprs/mod.rs:1403-1404  EIdentifier path tries expected_type first
src/ir/lowering/exprs/mod.rs:1461-1502  Ok / Error / Some variant ctors resolve from expected_type
src/ir/lowering/exprs/mod.rs:1531       call result expected_type-coerce
src/ir/lowering/exprs/mod.rs:1800-1887  struct literal: prev/restore expected_type per field
src/ir/lowering/stmts/mod.rs:1523-1526  SReturn sets expected_type = locals[0].type_id (this is what 6e49ead3 ported)
```

The self-host `lower.gg` has the SReturn one (after `6e49ead3`) and an ad-hoc
fix for Some-args inside lower_call. The other ~8 sites are not ported. Each
unported site is a potential next crash of the same shape.

This is what the `docs/plans/drop_emission.md` "RESIDUAL CLASS" block already
identifies:

> *The reference-grade fix is to port `expected_type` propagation to those
> positions; until then each may surface as its own stage-2 crash. Likely the
> proper home for this is the same pass that retypes at SReturn.*

**This — not "port SSA again" — is the architectural root fix.**

Effort estimate for porting `expected_type` (rough, by Rust-site complexity):

- Add a `func_state.expected_type: int` field to the self-host `LowerCtx`
  (mirror Rust's `Option<TypeId>` as `int` with -1 sentinel).
- 8-10 prev/restore call-sites in `lower.gg`'s `lower_call`, `lower_var_decl`,
  `lower_assign`, `lower_return` (already there), variant-ctor handler,
  collection-literal handler, struct-literal-per-field handler.
- Each site: read `prev = ctx.expected_type`, set new value, call `lower_expr`,
  restore.
- The variant-ctor consumer side (`try_lower_prelude_variant`,
  `enum_variant_parent`, the bare-`None`/`Ok`/`Error` paths) reads
  `ctx.expected_type` to pick the right parent enum's monomorphised name.

Rough ballpark: ~150-300 LOC change in `lower.gg`, plus a one-field add to
`LowerCtx`. Touches `tests/fixtures/self_host_lowerer/lower.gg` only (one
file). Risk: the GIR-comparison test (`lowerer_comparison`) will see actual
changes — every retyped slot is a typed-field change, which is exactly the
intended fix. The test's adjusted-pass criterion (per MEMORY.md
"GIR Lowerer: 814/913 fn-count match, adjusted 889/889 (100.0%)") may
fluctuate. Should be net-improvement (more matches to Rust), but expect
short-term noise during landing.

## 8. Recommendation

**DEFER the brief-as-written.** A second SSA port is not the right fix —
the existing one is already faithful, and the surveyed crashes are
*upstream* of what SSA can touch.

**ALTERNATIVE = port the missing `expected_type` propagation sites in
`lower.gg`.** This closes the actual class — including the latent crashes
that the `docs/plans/drop_emission.md` "RESIDUAL CLASS" block warns about
(bare `Ok`/`Error` as call-args / field inits / collection pushes /
indirect-ctor returns). Likely 2-3 agent cycles depending on how aggressively
the call-args / field-inits / collection-pushes are batched.

**Immediate next step (suggested agent brief topic):** *"port Rust gg's
`expected_type` propagation sites to self-host `lower.gg`, starting with
non-return consume positions of bare prelude-variant constructors."* That
brief should:

1. List the Rust call sites by `file:line` (see §7 inventory).
2. Identify which the self-host already has (currently: SReturn, partial
   Some-args in `lower_call ~5569-5579`, incidental var-decl monomorphised-
   collection recovery at `lower.gg:5632-5639`).
3. Spec the `LowerCtx.expected_type` field shape + prev/restore protocol.
4. Order: var-decl/assign first (broadest impact); then call-args;
   then field-inits; then collection pushes; then indirect-ctor returns.
5. Validation: stage-2 binary should clear progressively further into
   `lower_module` between successive sites.

**Out of scope:** an SSA pass touch — the existing one is correct. Anything
touching `lir_ssa.gg` to "close" these crashes would be re-implementing the
real fix in the wrong layer.

## 9. Effort/risk/hidden-complications table

| Option | LOC | Cycles | Risk | Closes |
|--------|-----|--------|------|--------|
| Port SSA again (as brief) | 0 (already there) | 0 | n/a | 0/3 crashes |
| Audit/improve existing SSA | ~50-150 if any bugs found | 1-2 | LOW (small surface) | Possibly C; A+B unaffected |
| `expected_type` port (§7) | ~150-300 | 2-3 | MEDIUM (touches hot-path GIR lowering; `lowerer_comparison` noise) | Eliminates the class A is in; latent siblings of B/C |
| Keep peeling per-crash | ~20-50 LOC per crash | 1 per crash, ongoing | LOW per peel, accumulates tech debt | One crash at a time |

**Hidden complications spotted in `src/lir/ssa.rs` (none affect the
recommendation, noted for completeness):**

- `:30-37` — debug-only `validate_ssa_dominance` post-pass. The self-host
  has its own `validate_lir_after("ssa-construction", &lir)` at
  driver.gg:88 doing similar work. Both can stay.
- `:251` — `LirType::Resource { .. }` zero-init uses `NullPtr` form. The
  self-host's `read_variable` `lir_ssa.gg:241-247` only handles `LT_PTR`
  (which collapses Ptr+Resource for self-host's purposes). Potential
  fidelity gap, but doesn't bite in practice because resource slots are
  rarely scalar-promotable.
- `:159-165` — `process_block` has an explicit `span_map` parallel-rebuild
  invariant that the self-host's `lir_ssa.gg:201-203` simplifies away.
  Self-host doesn't track per-inst spans in the same form; the
  simplification is intentional.

**Surprising finding in `src/lir/ssa.rs`:** the entire test suite at `:676-907`
runs `construct_ssa` standalone, asserting that promoted slots' SlotStore/SlotLoad
get removed. There's a test specifically asserting that the `branch.cond` post-SSA
uses the **original** const-true ValueId, not the eliminated SlotLoad's dst
(`:818`). This is the textbook copy-prop behaviour — but it's an *emergent*
property of slot promotion, not a separate pass. The lib tests directly
exercise this path; if `lir_ssa.gg` regressed the same path, the
`self_host_bootstrap` smoke test would catch it via stage-1 NULL stores.

## 10. What an SSA pass would NOT fix (and the existing one doesn't)

- **NEXT BLOCKER #3** — `lower_call`'s empty-name ECall → NULL Dict key in
  `__gorget_str_key_hash`. String-view / empty-name lowering gap, completely
  unrelated to SSA. (`docs/plans/drop_emission.md` top block.)
- **`__gg_R` / `__gg_W` convergence drift** — generic-struct + slot-numbering
  fixed-point-tail issue. Multi-generation drift, not addressable by per-fn
  Braun SSA. (`docs/plans/drop_emission.md`.)
- **Ownership tagging** (LoView vs LoOwned per `4ce2be0f`) — GIR-level
  metadata, not SSA-touchable.
- **CoW / clone-OOM cluster** (`2041d255`, `30f882ec`) — runtime/lowering
  behavior, not SSA.
- **Latent siblings of crashes A/B/C** at non-return positions (bare
  `Ok`/`Error` as call-args / field-inits / collection pushes,
  indirect-ctor returns) — these are GIR-lowering writer-site issues, same
  class as the three above. Each is a missing `expected_type` propagation;
  fixing them closes the class.

The summary is: **the SSA-collapsible class, at the layer SSA can see, is
empty.** All the brief's recurring NULL-store crashes are at non-promotable
slots; they look "SSA-collapsible" only if you don't check the promotability
filter, because both compilers' SSA passes apply the same scalar+no-SlotAddr
gate. The recurring class is at the GIR-lowering writer site, where Rust has
`expected_type` and the self-host doesn't (yet) have all of it.

---

### Citations recap

- `src/lir/ssa.rs:1-13, 19, 43-63, 67-91, 156-209, 224-267, 270-288, 292-314, 318-388, 480-625, 628-674, 676-907`
- `src/ir/lowering/stmts/mod.rs:1509-1577` (`lower_return` setting `expected_type`)
- `src/ir/lowering/exprs/mod.rs:198-200, 398-406, 1403-1502, 1531, 1800-1887` (other `expected_type` sites)
- `tests/fixtures/self_host_lowerer/driver.gg:16, 79, 86-103, 108-114`
- `tests/fixtures/self_host_lowerer/lir_ssa.gg:1-639` (existing SSA port)
- `tests/fixtures/self_host_lowerer/lir_codegen.gg:2802-2814` (the codegen give-ups that fired in crashes A/B/C)
- `tests/fixtures/self_host_lowerer/lir_lower.gg:2376-2401` (`emit_box_alloc` retype) and `:2613-2659` (Ptr-dst ← value-src rebind)
- Commits: `6e49ead3`, `dfe64fb7`, `4ce2be0f`, `2041d255`, `30f882ec`, `95e16dc9`, `7869f05a`, `170da69b`, `30a396ba`, `e08f1232`
- `docs/plans/drop_emission.md` top blocks (RESIDUAL CLASS, NEXT BLOCKER #3)
