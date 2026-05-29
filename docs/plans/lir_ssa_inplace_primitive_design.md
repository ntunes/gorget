# Design: zero-clone `process_block` + `apply_value_substitutions` (the 1.5B-array_clone bomb)

**Track:** perf (MEMORY). **Sites:** `tests/fixtures/self_host_lowerer/lir_ssa.gg` `process_block`
(filter-rebuild, ~:173–203) and `apply_value_substitutions` (rewrite-rebuild, ~:279–297).
**Goal:** make both sites zero-clone (or near-zero) by mutating the live block's inst list in
place instead of building a fresh `new_insts` and pushing every kept/rewritten inst into it.
**Status:** DESIGN ONLY — not implemented. To be reviewed (≥3 fresh passes) before execution.

This is the direct sequel to `docs/plans/lir_ssa_read_variable_inplace.md` (shipped, commit
`168e988e`), which converted `read_variable`'s entry-block zero-init prepend-rebuild to in-place
`insert(0, …)` and MEASURED that it removed only **−1.9M of ~1.5B array_clones (−0.1%)** — because
that branch is a minor path (fires only for entry-block undefined promotable slots). The handover
(TODO.md "🚨 (High, MEMORY BUG … ROOT NOW PINPOINTED 2026-05-29 pm)") attributes the remaining
~1.5B to these two sites, which rebuild on **every instruction of every block**.

---

## 0. Verified facts (cite on review)

All line numbers verified against the worktree at design time; re-confirm before editing.

- **`LirInst` is a resource-bearing enum** — `lir.gg:103–208`. Many variants embed `Vector[int]`
  (`ICall` `:174`, `ICallExtern` `:175`, `ICallPtr` `:176`, `ICallClosure` `:197`, `IPrintf` `:184`,
  `IFprintf` `:185`) or `Vector[FieldInit]` (`IStructInit` `:167`, `IEnumInit` `:171`) or `String`
  (`IStrLit` `:116`, `ICallExtern` `:175`, `IInlineC` `:188`, `ITrap` `:181`). So `LirInst`'s
  `elem_clone` / `elem_drop` / `elem_materialize` hooks are all **set** on a `Vector[LirInst]`.
- **The clone counter `array_clone` is bumped by `gorget_array_clone`** (`c_runtime.rs:5641–5642`),
  which is what an `LirInst` `elem_clone` ultimately calls for each embedded `Vector[int]` arg-list
  (the generated `LirInst__clone` deep-copies its inner vectors via `gorget_array_clone`). So
  "1.5B array_clone" = roughly "1.5B LirInst arg-list vectors deep-copied," dominated by these two
  per-instruction rebuild loops cloning every inst they pass through.
- **`gorget_array_push` does NOT itself deep-copy** (`c_runtime.rs:5244–5258`): it `memcpy`s the
  bytes and then runs `elem_materialize` (view→owned), not `elem_clone`. The deep clone is emitted
  by the **compiler before the push**, at the CoW *consuming position*: pushing a value read from a
  collection element binds a **borrow** (`docs/language-design.md:2373` — subscript/`.get()` returns
  a borrow, not a move-out), and you cannot move out of a borrowed collection element, so the
  compiler inserts `gorget_array_clone` to produce an owned copy before the push takes ownership.
  This is the CoW contract in CLAUDE.md ("Borrow, OR owned but live past this call → clone before
  call") and `docs/book/11-ownership.md`.
- **`gorget_array_set`** (`c_runtime.rs:5308–5319`): bounds-check, `elem_drop` the OLD element, then
  `memcpy` the new one. No `elem_clone` of the incoming element — but the *caller* still clones to
  produce the owned element it passes (same consuming-position reasoning), AND `set` adds an
  `elem_drop` of the displaced element. This is why the previously-tried `.set(ii, substitute_inst(...))`
  on `apply_value_substitutions` REGRESSED +161K (TODO.md): the by-value `substitute_inst` arg still
  cloned the borrowed source, and `set` piled `elem_drop` on top. **A naive `.set` does not help — it
  must be paired with a primitive that takes the new value BY POINTER and skips the clone (§2/§3).**
- **`gorget_array_insert`** (`c_runtime.rs:5607–5625`): memmove up + memcpy, no `elem_clone`.
- **`gorget_array_remove`** (`c_runtime.rs:5321–5335`): `elem_drop` then memmove-down, no clone.
- **No `retain` / in-place-filter / in-place-rewrite primitive exists today** (grep of
  `c_runtime.rs` for `retain`/`filter_inplace`/`rewrite` → none on arrays).
- **`process_block` ↔ `read_variable` reentrancy:** `process_block` (`:189`) calls `read_variable`
  during an `ISlotLoad`, and `read_variable` can mutate `f.blocks[bb]` for the SAME `bb` — both via
  `add_block_param`'s `.params.push` (`:273`) AND (since commit `168e988e`) via the entry-block
  zero-init `.insts.insert(0, …)` (`:233–239`). `process_block` iterates a value-snapshot `blk`
  (`:174`) and re-fetches `final_blk` at `:201` to reconcile the `.params` push. The `.insts` are
  replaced wholesale at `:202`, which is why the read_variable `insert(0,…)` is output-neutral
  (clobbered on the `process_block` path, per that plan's §"Equivalence detail").

---

## 1. Site A — `apply_value_substitutions` (the dominant site; do this FIRST)

### 1.1 Current code (`lir_ssa.gg:279–297`)

```gorget
void apply_value_substitutions(LirFunction &f, Dict[int, int] &value_subst):
    if value_subst.len() == 0:
        return
    int bi = 0
    while bi < f.blocks.len():
        LirBlock blk = f.blocks.get(bi).unwrap()       # value copy of the block? NO — see §1.2
        Vector[LirInst] new_insts = []
        int ii = 0
        while ii < blk.insts.len():
            LirInst inst = substitute_inst(blk.insts.get(ii).unwrap(), &value_subst)  # ← clone #1
            new_insts.push(inst)                        # ← consuming position (owned temp → move, cheap)
            ii += 1
        blk.insts = new_insts
        blk.term = substitute_term(blk.term, &value_subst)
        f.blocks.set(bi, blk)
        bi += 1
```

The dominant clone is **clone #1**: `substitute_inst` takes `LirInst inst` **by value**
(`:312`). The argument `blk.insts.get(ii).unwrap()` is a **borrow** of the live element; binding it
to a by-value parameter forces a clone-before-call (consuming position; the parameter slot must own
because `substitute_inst` may reconstruct and return it). Every inst of every block is cloned here.

`new_insts.push(inst)` itself is cheap: `inst` is the owned return value of `substitute_inst` (a
fresh owned temp at last use) → CoW moves it (zero-cost). So there is exactly ONE clone per inst,
at the `substitute_inst` call boundary. (The `blk` fetch at `:285` and `f.blocks.set(bi, blk)` at
`:296` are per-block, not per-inst — minor; addressed in §1.4.)

### 1.2 Why it dominates the 1.5B (estimate vs Site B)

`apply_value_substitutions` runs once per function and walks **every inst of every block** —
`O(total_insts)` clones per function, unconditionally (the `value_subst.len()==0` early-out at :280
skips functions with no promoted loads, but any function with a promoted-slot load has a non-empty
`value_subst`, which is essentially all of them). `process_block` ALSO walks every inst of every
block once. So at the per-inst-loop level the two sites touch a **similar instruction count**.

The tie-breaker — why Site A is expected to dominate — is the *clone shape per inst*:
- **Site A clones EVERY inst it visits** (the `substitute_inst` by-value arg fires for all insts,
  including the `else: return inst` fall-through arms — the value is already cloned at the call
  boundary before `substitute_inst`'s body even runs).
- **Site B (`process_block`) clones only the KEPT insts** (`new_insts.push(inst)` in the `else`
  arms / non-promotable `ISlotStore`/`ISlotLoad` arms). Promoted `ISlotStore`/`ISlotLoad` (the whole
  point of SSA construction — typically a large fraction of pre-SSA insts) are DROPPED, never pushed,
  never cloned.

So Site A clones a strict superset of Site B's per-inst set, on the same block walk, plus it runs
after Site B has already removed the slot insts (so Site A sees the post-promotion list — fewer
insts, but still clones all of them). **Net expectation: Site A is the larger contributor; both are
large.** This is a *reasoned* estimate, not measured per-site — the staged plan (§4) measures Site A
in isolation first, which both validates the ranking and de-risks the harder Site B.

### 1.3 The fix: in-place rewrite via a new primitive `gorget_array_set_noclone` (element-by-pointer set, NO clone, NO drop-of-self)

The core problem is that `substitute_inst` returns a *fresh, independently-owned* `LirInst` that
must REPLACE element `ii` in `blk.insts`. We want to write it back without (a) cloning it again and
(b) cloning the old element. `gorget_array_set` won't do — it `elem_drop`s the displaced old element
(correct in general, but here the old element is exactly what we're overwriting and we hold its only
reference) AND the existing `.set` call shape clones the incoming value at the by-value arg.

Two sub-problems, two parts:

**Part 1 — kill clone #1 (the `substitute_inst` by-value arg).** Change `substitute_inst` to take
its input **by mutable borrow and mutate in place**, returning void:

```gorget
# NEW shape — mutate the live inst through a borrow, no clone in, no clone out.
void substitute_inst_inplace(LirInst &inst, Dict[int, int] &subst):
    match inst:                                  # match on &inst binds field reads as borrows
        case ICall(dst, func, args):
            # rewrite args IN PLACE — see Part 1a
            ...
        case IAdd(dst, ty, lhs, rhs, ovf):
            inst = IAdd(dst, ty, sub_val(lhs, &subst), sub_val(rhs, &subst), ovf)  # scalar-only reconstruct
        ...
        else:
            return                               # no values to substitute — leave inst untouched
```

Caller becomes:

```gorget
int ii = 0
while ii < f.blocks.get(bi).unwrap().insts.len():
    substitute_inst_inplace(&f.blocks.get(bi).unwrap().insts.get(ii).unwrap(), &value_subst)
    ii += 1
```

`f.blocks.get(bi).unwrap().insts.get(ii).unwrap()` returns a **mutable borrow** of element `ii`
(subscript/`.get()` = mutable borrow, `docs/language-design.md:2373`). Passing `&…` to a
`LirInst &inst` param propagates the Ptr alias at zero cost (CoW bare-borrow). `substitute_inst_inplace`
mutates through it. **No clone in** (it's a borrow, not a by-value copy), **no clone out** (void
return, no `new_insts`, no `set`).

> **CRITICAL CAVEAT — the scalar-reconstruct arms (`IAdd`, `ICmp`, …) still assign `inst = IAdd(...)`.**
> For variants whose payload is ALL scalar (`int`/`bool`), reconstructing the variant and assigning
> it back through the `&inst` borrow is **zero array_clone** — there are no embedded vectors to deep-copy;
> the assignment is a flat memcpy of the enum's scalar payload over itself. These arms do NOT contribute
> to `array_clone`. So even the simplest possible version of Part 1 (keep the scalar arms as
> reconstruct-and-assign-through-borrow, only special-case the vector-bearing arms) already removes the
> clone-#1 deep-copy for **every** inst, because clone #1 was the *input* borrow→by-value copy of the
> whole inst (incl. its vectors), and that is now gone for all arms.

**Part 1a — the vector-bearing arms (`ICall`/`ICallExtern`/`ICallPtr`/`ICallClosure`/`IPrintf`/`IFprintf`/`IStructInit`/`IEnumInit`/`TSwitch`-in-term).**
For these, the naive `inst = ICall(dst, func, sub_vals(args, &subst))` still works and is **already
zero-array_clone in the common case**: `sub_vals` (`:304–310`) builds a *fresh* `Vector[int] result`
and pushes scalars into it — that fresh vector is an owned temp, moved into the new `ICall` variant
(zero-cost), and the new variant is assigned through the `&inst` borrow. The OLD variant's old
`args` vector is dropped (freed) when overwritten — a `free`, not a clone. **So Part 1a's
reconstruct-through-borrow is also zero-`array_clone`** — `sub_vals` allocates+frees but does not
`gorget_array_clone`. The 161K-regressing `.set` path cloned because it read the *whole inst* by
value first (clone #1, the deep `LirInst__clone` incl. vectors); routing through `&inst` eliminates
that whole-inst clone, and `sub_vals`'s fresh-vector construction was never an `array_clone` to begin
with.

**=> Part 1 alone (rewrite `substitute_inst` to mutate through `&inst`, keep every arm's body as a
reconstruct-and-assign-through-the-borrow) removes essentially all of Site A's `array_clone`s, with
NO new runtime primitive.** This is the move-on-last-use insight applied (§ goal item 4): the clone
was never fundamental — it was the by-value parameter binding forcing a defensive copy of a borrow.

**Do we still need a primitive?** Only if profiling shows the `sub_vals` fresh-vector
alloc/free churn (not `array_clone`, but `total_allocs`/`array_new`+`array_free`) is itself a peak-RSS
problem. If so, Part 2 (optional, deferred) adds an in-place `sub_vals_inplace(Vector[int] &v, subst)`
that rewrites each element of the EXISTING args vector through a borrow (no fresh vector, no
alloc/free):

```gorget
void sub_vals_inplace(Vector[int] &vals, Dict[int, int] &subst):
    int i = 0
    while i < vals.len():
        int nv = sub_val(vals.get(i).unwrap(), &subst)   # int read = copy (scalar), no clone
        vals.set(i, nv)                                  # scalar set: elem_drop is no-op for int, memcpy
        i += 1
```

This needs NO new C primitive either — `Vector[int].set` already exists and is zero-clone for scalar
elements (int `elem_drop` is a no-op; the incoming `int` is a scalar, no consuming-position clone).
The only blocker is matching on `&inst` to get a mutable borrow of the embedded `args` vector
(`case ICall(dst, func, args):` where `args` binds as `Vector[int] &args` under an `&inst` match) —
**verify the self-host match-on-borrow binds payload fields as borrows** (§1.5 risk). If it binds by
value, `sub_vals_inplace` can't reach the live vector and Part 1's reconstruct-through-borrow (which
is already zero-`array_clone`) stands as the final form.

### 1.4 Per-block residue (`:285` fetch + `:296` set-back)

With `new_insts` gone, also drop `LirBlock blk = f.blocks.get(bi).unwrap()` (:285) and
`f.blocks.set(bi, blk)` (:296). The block is mutated in place through `f.blocks.get(bi).unwrap()`.
The terminator rewrite stays in place too:

```gorget
LirTerm new_term = substitute_term(f.blocks.get(bi).unwrap().term, &value_subst)
f.blocks.get(bi).unwrap().term = new_term
```

`substitute_term` (`:415–433`) takes `LirTerm` by value → one clone per block (cheap: one term per
block, and `TSwitch`'s `Vector[SwitchCase]` is the only vector-bearing term). Optionally convert it
to `&term`-in-place by the same pattern, but per-block (not per-inst) so it is NOT a 1.5B contributor —
leave for a follow-up unless measurement says otherwise.

### 1.5 Risks specific to Site A

1. **Match-on-mutable-borrow payload binding.** Does the self-host (and Rust gg, since stage-0 must
   compile it) bind `case ICall(dst, func, args):` payload fields as **borrows** when scrutinee is
   `&inst`, allowing `args` mutation to write through to the live element? If match-on-borrow copies
   the payload out by value, Part 1a's `sub_vals_inplace` is impossible and only the
   reconstruct-through-borrow form (still zero-`array_clone`, §1.3 Part 1a) is viable — which is the
   recommended primary form anyway. **The reconstruct-through-borrow form does NOT require
   match-on-borrow to bind fields as mutable borrows** — it reads the scalar fields by value (fine —
   they're scalars) and `sub_vals` re-reads the args vector by value (the ONE place a borrow→copy
   happens, but `sub_vals` reads element-by-element as scalars, never clones the vector). Assigning the
   freshly-built variant back through `&inst` is the only mutation. **So Part 1 is robust to whatever
   match-on-borrow does;** only the optional Part 2 (`sub_vals_inplace`) depends on field-borrow binding.
2. **`substitute_inst` is single-copy?** `find tests/fixtures -name lir_ssa.gg` — per the
   read_variable plan it's single-copy (`self_host_lowerer/lir_ssa.gg` only, not symlinked, md5
   `7fb5338c…`). Re-confirm before editing.
3. **Output-neutrality.** The rewrite must produce byte-identical post-substitution insts. The
   reconstruct-through-borrow form builds the SAME variant the old `substitute_inst` returned, then
   stores it at the same index — identical result. Gate on `lowerer_comparison` count-parity +
   `bootstrap_fixed_point` byte-identity.

---

## 2. Site B — `process_block` (filter-rebuild; do this SECOND)

### 2.1 Current code (`lir_ssa.gg:173–203`)

```gorget
void process_block(int bb, LirFunction &f, ...):
    LirBlock blk = f.blocks.get(bb).unwrap()
    Vector[LirInst] new_insts = []
    int ii = 0
    while ii < blk.insts.len():
        LirInst inst = blk.insts.get(ii).unwrap()      # borrow read
        match inst:
            case ISlotStore(slot, value, _):
                if promotable.contains(slot): ... (DROP — promoted, recorded in current_def)
                else: new_insts.push(inst)             # ← clone (consuming position, borrowed source)
            case ISlotLoad(dst, slot, _):
                if promotable.contains(slot):
                    int reaching = read_variable(slot, bb, &f, ...)   # ← REENTRANT mutation of f.blocks[bb]
                    ... (DROP — promoted)
                else: new_insts.push(inst)             # ← clone
            else:
                new_insts.push(inst)                   # ← clone (the common path: all non-slot insts)
        ii += 1
    LirBlock final_blk = f.blocks.get(bb).unwrap()     # re-fetch to pick up read_variable's .params push
    final_blk.insts = new_insts
    f.blocks.set(bb, final_blk)
```

This is a **filter**: keep non-promotable insts, drop promoted `ISlotStore`/`ISlotLoad`. Each KEPT
inst is `new_insts.push(inst)` where `inst` is a **borrow** of the live element → clone-before-push.

### 2.2 The reentrancy hazard (the hard part)

`read_variable` (`:189`) can mutate `f.blocks[bb]` for the **same `bb`** mid-iteration:
- `add_block_param` (`:273`) `.params.push(...)` — appends to `.params`, NOT `.insts`. Harmless to
  an `.insts` in-place filter (different field).
- entry-block zero-init (`:233–239`) `.insts.insert(0, const)` — **inserts into `.insts` at index 0**,
  shifting every element right by one. This is the index-corruption hazard: an in-place filter walking
  `f.blocks[bb].insts` by index would see indices shift under it.

**BUT — when can the `:233–239` insert actually fire during `process_block(bb)`?** Only when
`read_variable(slot, bb, …)` hits the **0-pred (entry) branch for THIS bb** (`bb_preds.len()==0`).
`process_block` calls `read_variable(slot, bb, …)` with the SAME `bb` it is iterating (`:189`). So
the insert targets the very block being filtered **iff `bb` is the entry block (0 preds) AND the
slot is undefined at entry** (not yet in `current_def` for `(bb,slot)`). For a NON-entry `bb`,
`read_variable` recurses into *predecessors* (`:245`, `:250`) — the `insert(0,…)` then targets a
*predecessor* block (or an ancestor), NOT `bb`. So **the only block whose `.insts` can be mutated
mid-`process_block(bb)` is `bb` itself, and only when `bb` is the 0-pred entry block.**

Per the read_variable plan's §"Equivalence detail," even in that entry-block case the prepended
const is **clobbered** by `process_block`'s own `final_blk.insts = new_insts` at `:202` (the const
is not in `new_insts`) — only its value-id survives via `current_def`. So today the insert during
`process_block(entry)` is *already* a no-op on the final `.insts` (overwritten wholesale). This is
the key that makes a safe in-place scheme possible.

### 2.3 Safe in-place scheme — **two-phase mask-then-compact**, NOT mutate-during-iterate

Do NOT filter `f.blocks[bb].insts` in place *while* the `read_variable` loop runs (that's the
index-shift trap). Instead split into two passes over the live vector, with the reentrant
`read_variable` calls confined to PASS 1 (which only reads `.insts` by index and never structurally
mutates it):

```gorget
void process_block(int bb, LirFunction &f, ...):
    # PASS 1 — classify every inst, record promoted slot defs, gather the keep-mask.
    # Reads .insts[ii] by borrow; calls read_variable (which may .params.push and,
    # for the entry block, .insts.insert(0,…)). We must run PASS 1 BEFORE any
    # structural .insts mutation of bb, and snapshot the length up front.
    Vector[bool] keep = []
    int n = f.blocks.get(bb).unwrap().insts.len()
    int ii = 0
    while ii < n:
        # NB: if bb is the entry block, a read_variable insert(0,…) below shifts
        # indices. Guard: do PASS 1 against a STABLE view — see §2.4 for the
        # entry-block special-case (process entry insert AFTER pass 1, or detect+offset).
        LirInst inst = f.blocks.get(bb).unwrap().insts.get(ii).unwrap()   # borrow, NO push → NO clone
        match inst:
            case ISlotStore(slot, value, _):
                if promotable.contains(slot):
                    int resolved = resolve_value(value, &value_subst)
                    current_def.put(ssa_key(bb, slot), resolved)
                    keep.push(false)
                else: keep.push(true)
            case ISlotLoad(dst, slot, _):
                if promotable.contains(slot):
                    int reaching = read_variable(slot, bb, &f, ...)   # reentrant — see §2.4
                    current_def.put(ssa_key(bb, slot), reaching)
                    if reaching != dst: value_subst.put(dst, reaching)
                    keep.push(false)
                else: keep.push(true)
            else: keep.push(true)
        ii += 1
    # PASS 2 — in-place compaction: walk from the END, remove dropped insts.
    # Removing from the back keeps the indices of not-yet-visited (lower) insts stable.
    int j = n - 1
    while j >= 0:
        if not keep.get(j).unwrap():
            f.blocks.get(bb).unwrap().insts.remove_at(j)   # ← NEW primitive: in-place, NO clone, drops dropped elem
        j -= 1
```

**Why back-to-front compaction:** `remove_at(j)` memmoves elements `[j+1..len)` down by one; indices
`< j` are untouched. Walking `j` from `n-1` downto `0`, every index we still need to consult is
`≤ j`, so it never shifts. Zero clone of survivors (they only memmove). Each dropped inst is
`elem_drop`'d by `remove_at` (correct — promoted slot insts are dead).

**The survivors are NEVER cloned** — they are only memmoved (byte shuffle), exactly the CoW
"mutate-through-borrow" zero-clone shape. This is the whole win: the old code cloned every KEPT inst
(push into new_insts); the new code clones NONE (memmove only).

### 2.4 Resolving the entry-block reentrancy precisely

Two correct options; **Option (i) is recommended** (simplest, provably safe):

**Option (i) — keep the existing read_variable entry-insert, but make PASS 2 idempotent to it.**
The `insert(0, const)` during PASS 1 (entry block only) shifts indices by +1 for the rest of PASS 1.
This corrupts the `keep`-mask alignment (mask index `k` no longer maps to inst `k`). Fix: in PASS 1,
do NOT index `f.blocks[bb].insts` directly across a possible insert. Instead **snapshot the inst
list length and detect insert** by re-reading `.insts.len()` — if it grew, the entry const was
prepended at index 0; offset all subsequent reads by the growth. Cleaner: **process the entry-block
zero-init insert as a POST-step.** Since the const prepended by `read_variable` during
`process_block(entry)` is clobbered anyway (§2.2), we can:
  1. Pass `process_block` a flag/param suppressing the `.insts.insert` inside `read_variable` for the
     `bb==current` case (the value-id is still allocated + memoized in `current_def`; only the inst
     emission is deferred), THEN
  2. After PASS 2 compaction, prepend any entry-block consts that `read_variable` *would* have
     inserted, in one `insert(0,…)` per const (cheap — entry consts are few).

This is more plumbing than Option (ii). **Prefer Option (ii) below unless it proves infeasible.**

**Option (ii) — RECOMMENDED: snapshot the keep-decisions against a fixed length, compact a
fresh-length-aware index.** Observe that the entry-block insert only ever prepends at index 0 and
only for `bb==entry`. Capture `int base_len = f.blocks.get(bb).unwrap().insts.len()` ONCE at the top.
In PASS 1, read inst `ii` via `f.blocks.get(bb).unwrap().insts.get(ii + inserted_count).unwrap()`
where `inserted_count` is the number of front-inserts observed so far (recompute as
`current_len - base_len` at each step — any growth since base_len is front-inserted consts). The
`keep` mask is built for the `base_len` ORIGINAL insts only (the front-inserted consts are kept —
they're real zero-init defs). PASS 2 then compacts: front-inserted consts (indices
`[0, inserted_count)`) are always kept; original insts (now at `[inserted_count, current_len)`) are
kept per `keep[ii]`. Walk back-to-front over `current_len`, removing where the mapped mask says drop.

> **Honest assessment:** Option (ii)'s index arithmetic is the kind of "careful index management" the
> read_variable plan flagged as the reason this site was deferred. It is correct but fiddly. Given
> §2.2's finding that **the only mutating reentrancy into `bb`'s `.insts` is the entry-block 0-pred
> insert, and that insert is already clobbered/no-op on the final `.insts` today**, the
> **cleanest-of-all** option is:

**Option (iii) — BEST: hoist the entry-block zero-init OUT of the per-block reentrant path.**
The entry-block (0-pred) zero-init is a *function-global* concern (it seeds undefined promotable
slots at the function entry). Run a tiny pre-pass over the entry block BEFORE the main
`process_block` loop that allocates+memoizes (`current_def.put`) the zero-init value-ids AND inserts
their consts once, so that by the time `process_block(entry)` runs, `read_variable`'s 0-pred branch
always hits the `current_def.contains(key)` early-return (`:217`) and NEVER inserts. With the insert
hoisted out, **`read_variable` can no longer structurally mutate `bb`'s `.insts` during
`process_block(bb)` for ANY bb** (the only remaining reentrant write is `.params.push`, a different
field). Then PASS 1/PASS 2 (§2.3) need no index gymnastics at all — `keep[ii]` maps to inst `ii`
directly. This is the layering-correct fix: the zero-init seeding is a distinct phase, not a
side-effect smuggled through `read_variable`'s recursion. **Recommend Option (iii).** It also makes
the read_variable entry-insert (shipped in `168e988e`) cleaner — that insert moves to the pre-pass.

### 2.5 The new primitive: `Vector[T].remove_at(i)` → `gorget_array_remove` (already exists!)

PASS 2 needs an in-place remove-by-index that drops the removed element and memmoves the rest, with
**no clone and no Option-return** (we don't want the removed value). **`gorget_array_remove`
ALREADY EXISTS** (`c_runtime.rs:5321–5335`): bounds-check, `elem_drop`, memmove-down, `len--`. It is
NOT currently exposed as a self-host Vector method — the self-host maps `"remove"` →
`gorget_array_remove_opt` (the Option-returning variant, `lir_lower.gg:1448`). We need a SECOND
method name that maps to the void `gorget_array_remove`.

**=> The "new primitive" is mostly a NEW METHOD NAME on an EXISTING runtime fn, not new C.** Add a
Vector method `remove_at(int)` (or `delete(int)` / `remove_drop(int)`) → `gorget_array_remove`.
End-to-end threading in §3.

(If a name collision or semantic-clarity concern argues against `remove_at`, `gorget_array_remove`
can also be reached by an in-place `retain`-style filter — see §2.6 — but `remove_at` is the minimal
change and PASS 2 already has the mask, so a dedicated `retain` primitive is NOT needed.)

### 2.6 (Rejected alternative) a `retain(mask)` C primitive

A single-pass `gorget_array_retain(GorgetArray*, const bool* keep_mask, size_t n)` that compacts in
one memmove-coalescing sweep (drop-then-shift runs) would be O(n) with one pass and fewer memmoves
than n individual `remove_at` calls (which are O(n²) worst case — each remove memmoves the tail).
**This IS worth it if PASS 2's repeated `remove_at` shows up as a time regression** (the clone win is
the same either way; this is a memmove-count optimization). Defer to a follow-up: ship the
`remove_at` version first (correctness + clone win), measure, and only add `retain` if the O(n²)
memmove cost bites. Spec for the day it's needed is in §3.4.

### 2.7 Estimate for Site B

Removes the clone of **every kept inst** (the `new_insts.push(inst)` clones). Kept insts =
total − promoted-slot insts. In SSA-construction input, promoted `ISlotStore`/`ISlotLoad` are a
large share but the kept remainder (arithmetic, calls, loads/stores) is still
the bulk of real code. Expect Site B to remove a **large fraction** of the remaining ~1.5B, on the
same order as Site A. (Reasoned, not measured — §4 measures it in isolation after Site A.)

---

## 3. End-to-end threading for the new method name `remove_at` (Site B's primitive)

The runtime C already exists (`gorget_array_remove`). What's missing is exposing it as a distinct
Vector method so both compilers route to it. **Both compilers must agree** (self-host compiles
itself). Mirror the existing `insert`/`remove`/`swap` wiring exactly.

### 3.1 Rust type-checker / builtin decl — `src/ir/lowering/builtins.rs`

Add next to the existing `remove` entry (`:267`):

```rust
BuiltinMethodDecl {
    name: "remove_at",
    runtime_callee: Some("gorget_array_remove"),   // the VOID variant (not _opt)
    self_conv: SelfConvention::MutBorrow,
    is_mutating: true,
    returns_view: false,
    returns_fresh: false,
    params: int_param,                              // (int index)
    return_type: ret_void,                          // void — drops the removed elem
},
```

Also add to the type-checker's method table if it has a parallel list (the read_variable plan cites
`src/semantic/typecheck.rs:4838` for `insert`; check for a sibling `remove` entry there and mirror).

### 3.2 Rust codegen mapping — `src/lir/lower/calls.rs`

The `gorget_array` family match (`:298–335`) currently has `"remove" => gorget_array_remove_opt`
(:334). Add:

```rust
"remove_at" => return Some("gorget_array_remove".into()),
```

`gorget_array_remove` takes `(GorgetArray* arr, size_t index)` — both by value-ish (arr is the
receiver pointer, index is scalar). No element pointer arg, so no `needs_ptr_arg` entry needed for
the value (there is no value arg). Verify the receiver-by-pointer path (`collection_self_by_ptr`)
already covers it (it covers all `gorget_array_*`).

### 3.3 Self-host mirror — `tests/fixtures/self_host_lowerer/lir_lower.gg`

Add to `map_array_method` (next to `:1447–1448`):

```gorget
case "remove_at":
    return "gorget_array_remove"
```

`needs_ptr_arg` (`:1857–`): `gorget_array_remove` has only `(arr, index)`; index is scalar (not a
storage-pointer arg), so NO new `needs_ptr_arg` arm is required (unlike `_set@2`/`_insert@2` which
pass an element pointer). Confirm by checking that `gorget_array_remove_opt` (already mapped) has no
`needs_ptr_arg` entry for its index either (it shouldn't — index is scalar).

Ownership/consuming-arg handling in `lower.gg` (`is_owning_mutator_arg`, `:526–553`): `remove_at`
takes only an index (primitive), no owned element, so it is NOT an owning mutator — leave
`is_owning_mutator_arg` unchanged (it returns false for unlisted methods). Method-return-type
inference: `remove_at` → void; ensure the self-host's method-return-type table maps it to
void/UNIT (mirror how `clear`/`reverse` are handled — they're void mutators).

### 3.4 (Deferred) spec for `gorget_array_retain` if §2.6 proves needed

```c
// In-place filter: keep element i iff keep[i]. Drops dropped elements (elem_drop),
// compacts survivors via run-coalescing memmove. NO clone of survivors. O(n).
static inline void gorget_array_retain(GorgetArray* arr, const uint8_t* keep, size_t n) {
    if (n > arr->len) n = arr->len;        // defensive
    size_t w = 0;                          // write cursor
    size_t r = 0;                          // read cursor
    while (r < n) {
        if (keep[r]) {
            if (w != r)
                memmove((char*)arr->data + w*arr->elem_size,
                        (char*)arr->data + r*arr->elem_size, arr->elem_size);
            w++;
        } else if (arr->elem_drop) {
            arr->elem_drop((char*)arr->data + r*arr->elem_size);
        }
        r++;
    }
    // any insts beyond n (e.g. entry-block front-inserts) are kept as-is after w — but
    // with Option (iii) the entry insert is hoisted out, so n == arr->len and there is no tail.
    arr->len = w;
}
```
Threading: this takes a `bool*`/`uint8_t*` mask — NOT expressible as a plain Gorget Vector method
(no buffer-pointer ABI for the mask). It would need a bespoke lowering (pass the `Vector[bool] keep`'s
`.data` pointer). **This is why `remove_at` (a plain scalar-arg method) is the recommended first
cut — it needs zero new ABI.** Only pursue `retain` if back-to-front `remove_at` is measurably O(n²)-slow.

---

## 4. Staged implementation plan

**Stage 1 — Site A (`apply_value_substitutions`), no new primitive.** Rewrite `substitute_inst` →
`substitute_inst_inplace(LirInst &inst, …)` (reconstruct-and-assign-through-borrow form, §1.3 Part 1),
drop `new_insts`/per-block fetch+set (§1.4). Pure self-host `.gg` change in `lir_ssa.gg`. No `src/`,
no runtime C. This is the dominant site and the lowest-risk (no primitive, no reentrancy).
- Gate: `cargo build --release` clean; `cargo test --lib --release` (expect 1060/1062, the 2
  pre-existing release `should_panic`); `lir_ssa.gg` single-copy re-confirmed.
- Gate: `self_host_bootstrap_fixed_point` GREEN (byte-identical stages — output-neutrality proof) with
  `GG_STAGE1_TIMEOUT_SECS=900`.
- Gate: `lowerer_comparison` count-identical (diagnostic; read printed counts).
- **Measure:** `scripts/self_host_mem_baseline.sh --out /tmp/before-A.json` at tip → apply → delete
  driver binary/C → `--compare /tmp/before-A.json`. Report `array_clone` + peak-RSS delta. Expect a
  LARGE drop (the design's hypothesis is Site A dominates). If it's small, the per-site ranking
  assumption was wrong — re-attribute before Stage 2.

**Stage 2 — Site B (`process_block`), Option (iii) hoist + mask/back-compact + `remove_at` primitive.**
Three sub-steps, each independently gated:
- 2a. Thread the `remove_at` → `gorget_array_remove` method end-to-end (§3.1–3.3): Rust builtins +
  calls.rs, self-host `lir_lower.gg`. Add a throwaway fixture exercising `vec.remove_at(i)` to prove
  it lowers correctly in BOTH stage-0 and the self-compile (the read_variable plan's §"⚠ RISK" lesson:
  "wired + type-checks" ≠ "lowers correctly through the self-compile" — there is currently NO
  `remove_at` call site, so this is its first use). Gate: the fixture's stdout matches expected;
  `lowerer_comparison` count-stable.
- 2b. Hoist the entry-block zero-init out of `read_variable` into a pre-pass (Option (iii), §2.4) so
  `read_variable` no longer mutates `bb.insts` during `process_block(bb)`. Gate: `bootstrap_fixed_point`
  GREEN (output-neutral — the consts are emitted at the same place, just from a different caller).
- 2c. Convert `process_block` to PASS 1 (mask + reentrant read_variable) / PASS 2 (back-to-front
  `remove_at` compaction), §2.3. Gate: `bootstrap_fixed_point` GREEN; `lowerer_comparison` count-identical.
- **Measure:** baseline harness before/after Stage 2 → report Site B's `array_clone` + peak-RSS delta.

**Stage 3 (optional) — `retain` primitive (§2.6/§3.4) IF Stage 2c's `remove_at` loop is O(n²)-slow.**
Only if the timing (not clone) regresses. Spec ready in §3.4.

**Ordering rationale:** Site A first because it's (a) expected-dominant, (b) zero-new-primitive,
(c) zero-reentrancy — pure clone win with the least machinery, so it both delivers most of the prize
and de-risks the harder Site B by confirming the clone-attribution model on the easy site first.

---

## 5. CoW contract citations (for the review passes)

- **Borrowed source at consuming position → clone before call.** CLAUDE.md "Ownership at Consuming
  Positions" table ("Borrow, OR owned but live past this call → clone before call");
  `docs/internals/copy-on-write.md` Phase 3; `docs/book/11-ownership.md`.
- **Subscript/`.get()` returns a mutable borrow, not a move-out.** `docs/language-design.md:2373`.
- **Owned temp at last use → move (zero-cost).** CLAUDE.md "three move-eligible shapes" ("expression
  temp (last-use + owning by construction)"). This is why `sub_vals`'s fresh vector moves into the
  reconstructed variant for free, and why `read_variable`'s `insert(0, const)` (the const is a fresh
  owned temp) is zero-cost.
- **Mutate-through-borrow is the proven zero-clone shape.** `lir_ssa.gg:252–275` (`add_block_param`
  docstring) + the shipped `read_variable` `insert(0,…)` conversion (`168e988e`,
  `docs/plans/lir_ssa_read_variable_inplace.md`). Both Site A's reconstruct-through-`&inst` and Site
  B's `remove_at`-through-`f.blocks.get(bb).unwrap().insts` are the same shape.

## 6. Anti-goals / discipline

- Do NOT reshape `lir_ssa.gg`'s surrounding logic to dodge a gap. If `substitute_inst_inplace`'s
  match-on-`&inst` or `remove_at`'s lowering misbehaves in the self-compile, that is a NEW compiler
  bug → file a fixture + sharp TODO, do not work around it (CLAUDE.md "Don't redesign around compiler
  gaps").
- Do NOT add `gorget_array_retain` (new C) unless Stage 3's condition is met — `remove_at` reuses the
  existing `gorget_array_remove`, so Stages 1–2 add ZERO new C runtime code (only a new method-name
  route to an existing fn).
- Do NOT touch the other agents' zones: BuiltinMethodDecl/resources.gg/schema.gg (agent 1) — note
  §3.1 adds ONE `BuiltinMethodDecl` entry; coordinate or sequence after agent 1 lands to avoid a
  merge collision in `builtins.rs`. `lir_codegen.gg`/`lir_lower.gg` c_emit work (agent 2) — §3.3
  touches `lir_lower.gg`'s `map_array_method`; coordinate the edit window.
- Keep all changes output-neutral: every stage gates on `bootstrap_fixed_point` byte-identity +
  `lowerer_comparison` count-parity BEFORE the memory measurement is trusted.
```
