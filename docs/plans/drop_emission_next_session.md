# Drop Emission — Next Session Execution Plan

**Status:** Ready to execute. Written 2026-05-24 after a session that closed the OOM and
opened the move-semantics core. Companion to `consumer_audit.md` (full empirical log) and
`drop_emission_completion.md` (v3 strategic plan; note its Phase D/C.1 were corrected — see
consumer_audit.md).

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

## 2. STEP 1 (immediate blocker): set/push value consume-via-pointer ABI

**Symptom:** stage-1 double-frees in `meta_expand_for_match` → `gorget_array_set` → `Item__drop`.
**Root (confirmed via `--emit-gir`):** the `items.set(i, new)` VALUE arg is passed as `borrow`
(GIR: `call_extern @Vector__Item__set(borrow _29, borrow _3, borrow _30)`). The runtime
`gorget_array_set` memcpy's the value into storage (ownership transfer) AND drops the old
element; the source local `_30` is a borrow alias → both `_30` (scope-exit) and `items[i]`
own the same heap → double-free.

**Why the naive fix failed (already tried + reverted):** forcing `CkCallArgOwning` on the
mutator value arg in the method-call loop (`lower.gg` ~4438) makes `op_consume` return
`OpMove`, which passes the struct **by value** — but `gorget_array_set`/`push` take the value
**by pointer** → 146× "incompatible type for argument N of gorget_array_set/push".

**The correct fix — consume-via-pointer.** The value must be passed **by pointer** (runtime
memcpy's it) AND the source **move-zeroed post-call** (owned) / **cloned pre-call** (borrowed).
`lower_index_assign` (`lower.gg:5705`) already does this for `v[i]=x`:
```gorget
Vector[Operand] set_args = [base_op, op_consume(..., idx, CkCallArgBorrow()),
                            op_consume(..., val, CkCallArgOwning())]
emit(&ctx, GICallExtern(-1, setter, set_args))     # setter = "gorget_array_set" (RUNTIME name)
```
It works because it emits the **runtime name directly** (`GICallExtern("gorget_array_set",…)`),
so lir_lower's `needs_ptr_arg`/`takes_array_ptr_args` recognizes the value as pointer-ABI and
emits `SlotAddr`. The METHOD path emits the **mangled** name (`Vector__Item__set`), which
lir_lower maps to the runtime fn AFTER the ptr-arg decision → the value is never address-taken.

**Two fix options (pick after a 30-min spike reading lir_lower's call-ABI path):**
- **(A) Fix lir_lower (preferred — narrower, no lowering-shape change):** make `needs_ptr_arg`
  (and `takes_array_ptr_args`) consult the **mapped runtime name** for the value arg of
  collection mutators, so a method-call `Vector__T__set/push/put/insert` value arg is
  address-taken just like the direct `gorget_array_set` call. Locate `needs_ptr_arg`
  (`lir_lower.gg` ~1810-1839) + `map_monomorphized_to_runtime`; ensure the mapping is applied
  before the ptr-arg classification, OR add the mangled mutator names to the ptr-arg set.
  Then re-apply the `CkCallArgOwning` value-arg change in `lower.gg` (~4438, currently a TODO).
- **(B) Route method mutators through `lower_index_assign`'s mechanism:** in the method-call
  path, for `push/put/set/insert/add/send`, emit `GICallExtern(-1, "<runtime setter>", …)`
  with `CkCallArgOwning` value, instead of the generic mangled `GICall`. More invasive; risks
  diverging from the existing method-call return-type/writeback logic.

**Validate:** cc clean (no "incompatible type"), then gdb — the meta_expand double-free should
be gone. drop-count harness: array_free should move toward 544.

**Pitfall:** `push` has a separate dedicated path (`lower.gg` ~4687, `[OpMove(adst),OpMove(el)]`)
— check whether it's already correct (it cc'd fine before) and don't double-handle it. The
generic method path is the one that mishandles `.set()`.

---

## 3. STEP 2: re-gdb loop until stage-1 runs to completion

After STEP 1, re-run the gdb backtrace. Each remaining double-free is another unclean move at a
specific site; the pattern is always **a resource moved to a new owner without zeroing the
source, or consumed from a borrow without cloning.** Likely remaining sites (hypotheses):
- Other collection mutators on resource values not covered by STEP 1's method set.
- `Dict`/`Set` put/insert value+key (gorget_map_put) — same consume-via-pointer shape.
- Struct field-assign (`self.field = x`) of a resource (CkFieldWrite) — verify it move-zeros.
- Closure captures of resources.
Fix each with the same invariant; re-validate. Stop when stage-1 emits a full stage-2 body
(≥ ~half of stage-0's ~580K lines) with exit 0.

---

## 4. STEP 3: a-5 — verify LoBorrowed propagation (why a-1 was inert)

a-1 (`register_local_for_drop` skips `LoBorrowed`/`LoView`) changed **nothing** — the
over-dropped locals aren't tagged `LoBorrowed`. The GIR for `meta_expand_for_match` showed
field clones DO fire there (so the chain works at that site), but the inert a-1 means SOME
aliased locals are mis-tagged `LoOwned`. After STEP 1-2, re-check: if over-drops remain
(array_free/map_free self > Rust), trace which locals are dropped that shouldn't be, dump their
GIR ownership, and fix the propagation through `collection.get(i) → .unwrap() →
match-payload-binding → field-access` (`inherit_borrow_from`/`add_local_inheriting`, `lower.gg`
~435/~499). Keep a-1 (it's correct); it just needs correct upstream tagging to bite.

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

---

## Invariants & guardrails (do not violate)

1. **Only owners free.** Consuming from a borrow/view CLONES; consuming from an owner MOVES +
   zeroes the source. Never pass a resource to a consuming position (return / construction /
   field-init / collection-put / `!arg`) as a plain borrow that leaves two live owners.
2. **Zero every move (for now).** Do NOT port Rust's selective-zeroing optimization
   (`drops.rs` skips some MoveZeros). Emit `GIMoveZero` for every `OpMove`. `drop_elab` elides
   the redundant runtime check. Optimize later, only after correctness.
3. **Runtime ABI:** collection mutators (`gorget_array_set/push`, `gorget_map_put`) take the
   value BY POINTER. Consume = pointer-ABI for the call + zero/clone the source — NOT pass-by-
   value `OpMove`.
4. **Drop emission is unconditional** (PF-01): `GIDropIfAlive` for every droppable owned local
   at scope exit, never gated on `maybe_moved`. `drop_elab` (driver.gg:79) handles elision.
5. **No name-matching for routing** beyond the defined contract (the `push/put/set/insert/send`
   consuming-method set IS the CLAUDE.md contract — acceptable; prefer a typed flag long-term).
6. **stdbuf -oL** on every `--emit-*` run, or block-buffering masquerades as a hang.

---

## Open questions for the reviewer

- STEP 1: is option (A) lir_lower-`needs_ptr_arg`-on-mapped-name actually narrower/safer than
  (B) routing through `lower_index_assign`? Which has fewer side effects on existing
  method-call return-type/writeback logic?
- Is there a risk that STEP 1's owning value arg, once address-taken, interacts badly with the
  existing `need_chain_writeback` / `need_writeback` logic (the `coll.get(i).unwrap().push(x)`
  writeback path)?
- Should the cluster be shipped atomically (squashed) or as a reviewed sequence of commits with
  the bootstrap gated off until the final one?
