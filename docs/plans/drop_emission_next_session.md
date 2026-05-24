# Drop Emission — Next Session Execution Plan

**Status:** Ready to execute — **REVIEWED + REVISED 2026-05-24** (fresh-agent review, verdict:
ship-with-changes; STEP 1 diagnosis was inverted and has been rewritten, STEP 3/a-5 found to be
unwired dead code, push pitfall corrected — all folded in). Written after a session that closed
the OOM and opened the move-semantics core. Companion to `consumer_audit.md` (full empirical
log) and `drop_emission_completion.md` (v3.1 strategic plan; its Phase D/C.1/reorder were
empirically corrected — see its v3.1 banner).

**Guiding principle (from the user):** *Only owners free their resources. Zero EVERY move for
now — do NOT port Rust's selective-zeroing optimization yet. Make it correct first, optimize
later.* So the invariant to enforce everywhere: **every `OpMove` of a resource pairs with a
`GIMoveZero` of its source; consuming from a non-owner (borrow/view) CLONES.**

---

## 0. Start state (commit `1614ac2a`, branch `gorget-1`)

The WIP is committed (marked, NOT shippable — stage-1 double-frees at startup). `cargo build`
+ stage-1 cc are clean; the **stage-1 binary double-frees** running on driver.gg.

**Locked wins (measured this session):**
- OOM **14.4 GB → 1 MB** (the bootstrap-fragility root — closed).
- Pointer-cast cascade **2780 → 0** (a-7 match-scrutinee borrow).
- User-type drops **0 → 267 defs / 338 calls** (C.1: removed the over-broad `__imported_type__`
  skip in `populate_drop_metadata`).
- Double-free classes closed: **return-path** (a-6 #5), **enum-variant-ctor** (Fix-D extension).

**WIP edits already in `1614ac2a`** (re-derive from `git show 1614ac2a` if needed):
- `lower.gg`: Phase D un-disable (`emit_scope_drops` ~838, `emit_drops_for_early_exit` ~1051);
  a-7 (`match_scrutinee_ptr` ~5829 `CkAssign→CkMatchPtr`); a-1 (`register_local_for_drop` ~934
  skips `LoBorrowed`/`LoView` — INERT, see §4); a-6 #5 (`SReturn` ~5312 OpMove→GIMoveZero +
  exclude); enum-variant ctor `fn_move_params` registration (~8231 Fix-D `IEnum` case); a TODO
  comment at the method-mutator value-arg site (~4438).
- `lir_lower.gg`: C.1 (removed `__imported_type__` skip, `populate_drop_metadata` ~3439/3475).

---

## 1. The validation loop (use this every step — it is the definition of done)

The drop-count grep-diff + gdb backtrace replaced RSS-guessing this session and is far sharper.

**Build + emit + cc + run (one cycle ≈ 520s; the emit dominates):**
```bash
cd /workspace/gorget-1
./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg          # rebuild stage-0
OUT=/tmp/s1.c
timeout 600 stdbuf -oL ./tests/fixtures/self_host_lowerer/driver \
    tests/fixtures/self_host_lowerer/driver.gg lib --emit-c > "$OUT"           # MUST use stdbuf -oL (block-buffering looks like a hang otherwise)
python3 -c "r=open('tests/fixtures/self_host_lowerer/driver.c').read();i=r.find('\ntypedef struct __gg_');open('/tmp/s1full.c','w').write(r[:i]+'\n'+open('$OUT').read())"
cc -O0 -w -o /tmp/s1bin /tmp/s1full.c -lm -lpthread                            # double-define / ABI errors show here
```

**Drop-count harness (watch self climb to Rust parity; over-count = over-drop / under = leak):**
```bash
RUST=tests/fixtures/self_host_lowerer/driver.c
for p in '__drop(' 'gorget_string_free(' 'gorget_array_free(' 'gorget_map_free('; do
  printf '%-22s rust=%s self=%s\n' "$p" "$(grep -cF "$p" $RUST)" "$(grep -cF "$p" $OUT)"; done
# Rust targets (approx, drift slightly as self-host source grows):
#   __drop 2358 · string_free 5340 · array_free 544 · map_free 102
```

**gdb backtrace (THE tool — pinpoints each double-free to a C function chain):**
```bash
timeout 120 gdb -batch -ex run -ex "bt 25" --args /tmp/s1bin \
    tests/fixtures/self_host_lowerer/driver.gg lib --emit-c 2>&1 | grep -E '^#[0-9]+ '
```

**IR inspection (when a backtrace points at a function, dump its GIR/LIR to see operand modes):**
```bash
timeout 260 stdbuf -oL ./tests/fixtures/self_host_lowerer/driver <input>.gg lib --emit-gir > /tmp/gir.txt
awk '/fn @<MangledFnName>\(/{f=1} f{print} f&&/^}/{exit}' /tmp/gir.txt    # look for borrow/move/clone on the args
```
(`--emit-gir`/`--emit-lir`/`--emit-c` shipped this session; `--lir-c` is an alias for `--emit-c`.)

---

## 2. STEP 1 (immediate blocker): collection-mutator value arg is OpBorrow (operand-kind, NOT ABI)

> **REWRITTEN per fresh-agent review (2026-05-24) — the original STEP 1 diagnosis was INVERTED.**
> It claimed the pointer-ABI was wrong (value not address-taken because lir_lower maps the name
> too late). FALSE: lir_lower computes `effective_name = map_runtime_name(func_name)`
> (`lir_lower.gg:~2962`) BEFORE `needs_ptr_arg(effective_name, ai)` (~2964), and EVERY mutator
> value arg is already in `needs_ptr_arg` on the runtime name (`gorget_array_set`@2,
> `gorget_array_push`@1, `gorget_array_insert`@2, `gorget_map_put`@1&2, `gorget_set_add`@1,
> `gorget_channel_send`@1 — verified ~1893-1925). The method path ALREADY address-takes the
> value. **The bug is purely the OPERAND KIND in `lower.gg`, and the old Options (A)/(B) were
> a no-op / over-invasive respectively.**

**Symptom:** stage-1 double-frees in `meta_expand_for_match` → `gorget_array_set` → `Item__drop`.
**Root (confirmed via `--emit-gir` + the review):** `items.set(i, new)` lowers the VALUE arg as
`borrow` (GIR: `call_extern @Vector__Item__set(borrow _29, borrow _3, borrow _30)`) because
`classify_call_arg("Vector__Item__set", val_idx)` returns `CkCallArgBorrow` — the mutator method
isn't in `fn_move_params` (only user struct/enum ctors are registered). So `op_consume` yields
`OpBorrow`: (a) no `GIMoveZero` is paired with it, and (b) it never clones a borrowed source.
The runtime memcpy's the (pointer to the) value into storage AND drops the old element; the
source local is still live → both it (scope-exit) and `items[i]` own the same heap → double-free.

**Fix (Option C — the only correct one; `lower.gg` ~4438, currently a TODO):** classify the
value arg (and the key arg, for map/set) of a collection mutator as `CkCallArgOwning` instead of
letting `classify_call_arg` default it to borrow. `op_consume` then returns `OpMove` for an
owned/last-use source, and `wire_liveness_into_modes` (verified: rewrites `GICallExtern` args at
~2330, emits the paired `GIMoveZero`) handles the zero automatically. **NO `lir_lower` change** —
the pointer-ABI/address-taking is already wired. ~5 lines at one site.

**BUT — this is exactly what threw 146× "incompatible type" before, and WHY is the spike.** The
review's hypothesis (verify first): those errors are value args whose SOURCE SLOT is already a
pointer (`LT_PTR`, e.g. a `.get()`-derived element). `op_consume`→`OpMove` on an `LT_PTR` slot
hits lir_lower's `if slot_ty == LT_PTR: lower_operand` sub-branch, which passes the LOADED
pointer by value → mismatches the `void*`-element C sig. Those sources are BORROWS and must be
**cloned**, not moved — which couples STEP 1 to STEP 3 (a-5), because the clone-on-borrow path
is currently DEAD CODE (see §4). **Mandatory spike (this is the real 30-min task, NOT reading
lir_lower's ABI which is already correct):** re-apply the `CkCallArgOwning` value-arg change,
reproduce the 146×, and bucket the failing sites by value-slot kind — owned-struct-value
(OpMove is fine) vs `LT_PTR`/borrowed (needs clone via a-5). Fix owned-value first; the
borrowed-value bucket lands with a-5.

**Fold in (same one-line fix, same already-wired ptr-ABI):** Dict/Set `put`/`insert`/`add`
value+key args (`gorget_map_put`@1&2, `gorget_set_add`@1) route through the SAME ~4438 path with
the SAME OpBorrow defect. Do them in STEP 1, not as separate STEP 2 backtraces.

**Do NOT copy `lower_index_assign` verbatim (trap flagged by 2nd review).** The WIP TODO at
`lower.gg:~4445` and an old (now-struck) `consumer_audit.md` note point at `lower_index_assign`
(`v[i]=x`, ~5557-5604) as the model. It is the *less*-correct sibling: it emits the **mangled**
`coll_tn+"__set"` name (fine — maps before `needs_ptr_arg`) but **hardcodes `OpMove(value)`**
with NO borrowed-value clone (its own latent borrowed-value bug). Use `op_consume(ma_val,
CkCallArgOwning())` instead — it returns OpMove for owned, leaves OpBorrow for borrowed (→ a-5),
which is strictly more correct than index-assign's blind OpMove.

**Validate:** cc clean (no "incompatible type"), then gdb — the meta_expand double-free gone.
Harness: array_free toward 544.

**Pitfall (CORRECTED):** the METHOD `.push(resource)` is NOT separately handled — it flows
through the SAME generic ~4438 path as `.set()`, so STEP 1 must fix push/put/set/insert/add
together. The `lower.gg` ~4687 path is the `EArrayLiteral` element-push (`[a,b,c]` construction,
owned last-use temps → OpMove already safe) — genuinely separate, leave it untouched.

---

## 3. STEP 2: re-gdb loop until stage-1 runs to completion

After STEP 1 (+ a-5, which is coupled — see §4), re-run the gdb backtrace. Each remaining
double-free is another unclean move: **a resource moved to a new owner without zeroing the
source, or consumed from a borrow without cloning.** Remaining-site hypotheses (NOT
Dict/Set put/insert — those are folded into STEP 1):
- Struct field-assign (`self.field = x`) of a resource (CkFieldWrite) — verify it move-zeros.
- Closure captures of resources (the genuinely-unknown remainder).
- Any other consume site whose callee isn't in `fn_move_params`.
Fix each with the same invariant; re-validate. Stop when stage-1 emits a full stage-2 body
(≥ ~half of stage-0's ~580K lines) with exit 0.

---

## 4. STEP 3: a-5 — clone-on-borrow is DEAD CODE; wire it (COUPLED to STEP 1)

**Review correction — this is bigger than "verify propagation":** the plan assumed "consume
from a borrow → clone" is available machinery. **It is NOT wired.** `op_consume`'s `LoBorrowed`
arm returns `OpBorrow`, NOT `OpClone` (`lower.gg` ~1244-1250). The OpClone-on-borrow decision
lives only in `decide_operand_at_consuming_arg`, which `wire_one_operand` only reaches when
handed an `OpMove`/`OpClone` to *refine* — it never *promotes* an `OpBorrow`. So a value/field
source correctly tagged `LoBorrowed` still becomes `OpBorrow` → no clone → double-free.

**Why this couples to STEP 1:** the 146× "incompatible type" bucket (LT_PTR / `.get()`-derived
value sources) are borrows that must clone, not move. STEP 1's owned-value fix alone leaves them
broken; closing them needs the clone path wired HERE. Do a-5 alongside STEP 1's borrowed-value
bucket.

**a-5 work:** wire clone-on-LoBorrowed into the consume path — either add the OpClone-on-
LoBorrowed arm directly to `op_consume` (for consume kinds: CkAssign/CkReturn/CkFieldWrite/
CkCallArgOwning), or route consume positions through `decide_operand_at_consuming_arg` so its
LoBorrowed→OpClone verdict actually fires. THEN the upstream tagging matters: verify `LoBorrowed`
propagates through `collection.get(i) → .unwrap() → match-payload-binding → field-access`
(`inherit_borrow_from`/`add_local_inheriting`, `lower.gg` ~435/~499) so the clone fires at the
right sources. (The `meta_expand` case happened to work only because its FIELDS clone into the
new node, making it owned — not because clone-on-borrow is wired.)

**a-1 stays** (`register_local_for_drop` skips LoBorrowed/LoView) — correct, but inert until the
tagging above makes aliased locals actually carry `LoBorrowed`.

**RESOLVED (pre-flight check the 3rd review wanted): OpClone materializes end-to-end — a-5's
clones WILL fire.** The decision-layer docstring at `lower.gg:1262-1270` ("clone-emission
deferred to Phase 2c... dead code, no caller") is STALE. Materialization is live one layer
down: `lir_lower.gg:2399-2464` (`case OpClone(local_id)`) emits a real
`ICallExtern(dst, clone_fn, ...)` with `clone_fn = resource_clone_fn(...)` = `T__clone` /
`gorget_*_clone` ("Phase 2c COMMIT 2 + Phase 2.3: OpClone lowers to a real clone"). Empirically
confirmed: the `meta_expand` GIR's `call @FunctionDef(clone _21, clone _22, …)` field-clones did
NOT double-free → they produced independent copies. So once a-5 makes `op_consume` emit
`OpClone` for a LoBorrowed consume source, lir_lower turns it into a working clone call — no
deferred-emission blocker. (Retire the stale `lower.gg:1262-1272` docstring in STEP 5 cleanup.)

---

## 5. STEP 4: perf (the ~510s emit)

`--emit-c` with drops takes ~510-560s (was much faster pre-drops, but completes — within the
600s build deadline). Suspects (profile first, per the perf-hunt playbook): drop-type
resolution at `lir_lower.gg:2490` (GIDropIfAlive type-name lookup, possibly O(n²)), or O(n²)
block-instruction append as drop count grows. Only optimize once correctness (stage-1 runs) is
in. If `self_host_bootstrap`'s 600s build deadline is at risk, bump `GG_BUILD_TIMEOUT_SECS` as a
stopgap and file the perf fix.

---

## 6. STEP 5: validate + ship the cluster atomically

- `cargo test --test integration --release self_host_bootstrap -- --test-threads=1` PASSES
  (stage-1 runs on driver.gg, emits valid stage-2, links).
- `cargo test --test integration --release self_host_bootstrap_fixed_point -- --test-threads=1`
  PASSES (stage-1 ≡ stage-2 byte-equal — the real fixed point, now with REAL drops not labels).
- `cargo test --lib --release` (1059/1061 baseline) + `lowerer_comparison` 1/1.
- Drop-count harness at Rust parity (±, see §1).
- **Commit the whole cluster as one coherent change** (squash the WIP `1614ac2a` + the
  STEP 1-4 fixes), since it's the atomic cluster (a) per the plan — shipping partials crashes.
- Update `consumer_audit.md` + `drop_emission_completion.md` to "shipped"; retire the
  Phase F.2 workarounds the proper machinery obviates (add_local_inheriting band-aids, etc.).
- Retire stale code comments flagged by the 2nd review: the `decide_operand_at_consuming_arg`
  docstring at `lower.gg:~1262` ("dead code in this commit. No caller exists" — it IS called at
  ~2290 now) and the WIP TODO at `lower.gg:~4445` (points at lower_index_assign as the model;
  see the STEP 1 "do not copy" note). Also consider fixing index-assign's own hardcoded
  `OpMove(value)` (no borrowed-value clone) for symmetry once a-5 lands.

---

## Invariants & guardrails (do not violate)

1. **Only owners free.** The consume decision (Rust `ensure_owned_at_consuming_arg`; CLAUDE.md
   "Ownership at Consuming Positions"): **owned AND dead-at-this-call → MOVE** (+ zero source);
   **borrow/view, OR owned-but-live-past → CLONE**. NOT "owner always moves" — an owner used
   again later must clone. The self-host already does this for OWNED sources (`op_consume`
   LoOwned→OpMove; wire-pass demotes live-past→OpClone). a-5 only adds the missing borrow arm.
   Never pass a resource to a consuming position (return / construction / field-init /
   collection-put / `!arg`) as a plain borrow that leaves two live owners.
2. **Make it work first; zero every move (for now).** Do NOT port Rust's selective-zeroing
   optimization. Emit `GIMoveZero` for every `OpMove`; `drop_elab` elides the redundant runtime
   check. Correctness before perf — **but the deferred optimizations are recorded in §7 so they
   are not forgotten.**
3. **Runtime ABI is ALREADY pointer-correct** for mutator value args via the method path
   (`needs_ptr_arg` keyed on the mapped runtime name address-takes them). An `OpMove` operand on
   a struct-value slot IS address-taken cleanly. The ONLY ABI trap is a value source whose slot
   is already `LT_PTR` (`.get()`-derived) — that needs CLONE (a-5), not OpMove. Do NOT "fix"
   lir_lower's ABI; it's correct.
4. **Drop emission is unconditional** (PF-01): `GIDropIfAlive` for every droppable owned local
   at scope exit, never gated on `maybe_moved`. `drop_elab` (driver.gg:79) handles elision.
5. **No name-matching for routing** beyond the defined contract (the `push/put/set/insert/send`
   consuming-method set IS the CLAUDE.md contract — acceptable; prefer a typed flag long-term).
6. **stdbuf -oL** on every `--emit-*` run, or block-buffering masquerades as a hang.

---

## Reviewer's answers (RESOLVED — fresh-agent review 2026-05-24, verdict: ship-with-changes)

The review ground-truthed every code claim against WIP `1614ac2a` and rewrote STEP 1 (above).
Resolutions:

1. **Option A vs B → NEITHER. Use Option C** (operand-kind fix in `lower.gg`, §2). A is a no-op
   (the ABI is already correct) and would re-add mangled-name routing (a "No name matching"
   smell); B (route through `lower_index_assign`) is over-invasive (bypasses the method path's
   return-type + `need_writeback`/`need_chain_writeback` logic). C is ~5 lines at one site.
2. **`need_chain_writeback` interaction → contained.** That logic forces `OpBorrow` on the
   RECEIVER (to keep it alive for a subsequent `coll.set(idx,!recv)`); it's orthogonal to the
   VALUE arg. Confirm in the GIR dump that when the value arg IS the chain target, the explicit
   move + wire's GIMoveZero don't conflict (idempotent → benign, but check — cf. #4 below).
3. **Ship ATOMICALLY (squash).** consumer_audit.md proves it: all 9 prior E.1 attempts failed
   because each shipped PART of the coupled cluster. C.1 + a-7 + the move-zero/clone fixes are
   interdependent — any partial state double-frees or won't compile. One commit; bootstrap flips
   green only at the end. Do NOT land intermediate commits with the bootstrap gated off (a
   half-cluster on a shared branch is a landmine).

## Known smells / cleanups (from the review — low priority)

- **a-6 #5 redundant GIMoveZero:** `SReturn` explicitly emits `GIMoveZero(src)` AND
  `wire_liveness_into_modes` (2199) also emits one for the `GIAssign(0, ret_op)` OpMove operand
  → two zeros for one move. Idempotent/harmless, but consider dropping the explicit one once
  the wire-pass coverage is confirmed (keep the `exclude` arg either way).
- **Line refs drift:** several refs in earlier drafts were off (`lower_index_assign` GICallExtern
  is ~5706 not 5705; the method value-arg loop body is ~4437-4470; `takes_array_ptr_args` /
  `map_monomorphized_to_runtime` do NOT exist — the real fns are `needs_ptr_arg` @1810,
  `map_runtime_name` @1111, `map_array_method`/`map_dict_method`/`map_set_method`). Re-grep on
  entry; treat line numbers as approximate.

---

## 7. Deferred optimizations — DO NOT FORGET (make it work first, then do these)

We are deliberately choosing the correct-but-conservative shape now and deferring perf. These
are NOT bugs and NOT to be done during the correctness push — but they MUST NOT be lost. File
each into `TODO.md` (and reference here) once the cluster ships green, so they survive.

1. **MoveZero elision (the headline deferred opt).** We emit `GIMoveZero` for EVERY `OpMove`
   (guardrail #2). Rust's `drops.rs` skips the zero when drop-tracking proves the source is
   never re-observed (`drop_elab` slot-state = provably-Uninitialized). The self-host's
   `drop_elab.gg` already has the dataflow to do this static elision; we just don't lean on it
   to *omit* the GIMoveZero emission. **Later:** let `drop_elab` drive selective MoveZero
   elision (or skip emitting GIMoveZero where the slot is provably dead) → fewer instructions,
   less codegen, faster emit. Measure against the ~510s emit (STEP 4).

2. **Conservative clone where Rust would move (last-use precision).** Rust elides a clone to a
   MOVE whenever the owned source is provably dead at the call (better liveness). Where the
   self-host's `compute_liveness` can't prove last-use (or where a-5's borrowed-source clone is
   actually the last use of a value the caller no longer needs), we will CLONE conservatively.
   That's correct (a clone is always sound) but wasteful. **Later:** tighten last-use analysis
   so more consume sites move instead of clone → fewer `T__clone` calls, toward Rust's ~338
   stage-1 clone-call parity (vs. a conservative over-count). Track via the drop-count harness
   (clone-call count) once stage-1 runs.

3. **Clone emission layer: GIR (Rust) vs LIR (self-host).** Rust emits the clone INLINE at GIR
   (`ensure_owned_at_consuming_arg` → `builder.call(clone_fn)`); the self-host labels `OpClone`
   at GIR and materializes at LIR (`lir_lower.gg:2399-2464`). Functionally equivalent, but a
   layering divergence from Rust. **Later (self-host-as-showcase reconciliation, optional):**
   decide whether to keep the split (it may be defensible) or move materialization to GIR to
   mirror Rust. Note it can also cause GIR-shape differences vs Rust in any GIR-level diff.

4. **Perf: the ~510s emit (STEP 4).** Already a step, restated here so it's not lost: the
   O(n²) suspect in drop-type resolution / block-instruction append. Profile after correctness.

5. **`fixed_point` N back to N=2 + retire Phase F.2 workarounds** (completion.md F.1/F.2):
   `add_local_inheriting`/`inherit_borrow_from` band-aids should be auditable-and-retired once
   real move+clone+drop machinery lands; tighten `bootstrap_fixed_point` from N=5 to N=2.

**On entry next session:** these live here; on ship (STEP 5) copy them into `TODO.md` so they
outlive this plan doc.
