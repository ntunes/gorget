# SCOUT: Hang-census ROOT A — self-host lazy-iterator adapter receiver-clone

Status: IN PROGRESS. Checkpoint after every step.

## Step 1 — REPRODUCED at current HEAD (gorget-1 f42eea96)
- Built Rust `gg` (target/debug/gg) + self-host driver.
- `driver stdlib_iter_set.gg lib --emit-c` → cc -O0 → run (8s timeout):
  - direct `for x in s.iter():` → 10,20,30,40  CORRECT
  - `.take(2)` → 10,10  (count right, VALUE wrong)
  - `.skip(2)` → 10 forever  SPIN
- Rust oracle: 10,20,30,40,--,10,20,--,30,40  (correct).

## Step 1 — smoking gun still present (emitted C, TakeIter__SetIter__…__next):
```c
__v22 = (void*)&((__gg_TakeIter__SetIter__int64_t__int64_t *)(__v0))->inner; // field addr
__v23 = *(__gg_SetIter__int64_t *)(__v22);   // deref -> VALUE byte-copy
__s9  = __v23;                                // store to temp slot
__v24 = &__s9;                               // ptr to temp
__v25 = SetIter__int64_t__next(__v24);       // cursor advance lands in temp, discarded
```
VectorIter (resource inner) has NO deref+store: `__v22 = &..->inner; VectorIter__next(__v22)`.

## Step 2 — WRITE SITE localized
- `lower_expr` `case EFieldAccess` (lower_expr.gg:4695-4725): the GIFieldLoad dst is
  Ptr-typed ONLY when `is_resource_type_name(found_type_name)` (line 4718-4721); otherwise
  value-typed → LIR emits a deref+byte-copy (materialize).
- Receiver path: EMethodCall (lower_expr.gg:3289) calls `lower_place_base(recv)`;
  `lower_place_base` (lower_stmt.gg:1514) has NO EFieldAccess arm → falls through to
  `lower_expr(EFieldAccess)` → the value-copy above for non-resource fields.
- Struct diff: VectorIter{Vector[T] source} = resource → Ptr field load (borrow place).
  SetIter/DictKeysIter/DictValuesIter{Ref[..] source} = NOT resource → value copy.
- So the class boundary = a `&self` method RECEIVER that is a field place whose field TYPE
  is not resource-classified. CoW mandates borrow-the-place at a receiver (Core inv #1).

## Step 3 — PROTOTYPE LANDED + MEASURED (all 3 flip CRASH→MATCH)
Fix: new `lower_recv_place` in lower_expr.gg (before `lower_expr`), called at the method-call
receiver site (was `lower_place_base` at ~3289). For an `EFieldAccess` receiver whose base is a
bare local/`self` and whose field is a PLAIN (non-resource, non-Ref, non-scalar) struct, it
emits a Ptr-typed GIFieldLoad = borrow the field PLACE. Everything else delegates to
`lower_place_base` unchanged. Mirrors Rust methods.rs:2037-2064.

KEY GOTCHA: the self receiver is `ESelfExpr`, NOT `EIdentifier("self")` (lower.gg:1067) — first
attempt only matched EIdentifier and did nothing; adding ESelfExpr made it fire.

Emitted C now (TakeIter__SetIter…__next): `__v22 = &self->inner; SetIter__next(__v22)` — no
deref+store. Matches the VectorIter correct shape.

Measured (self-host binary vs Rust oracle):
- stdlib_iter_set  → 10,20,30,40,--,10,20,--,30,40   MATCH (was 10,10 + skip SPIN)
- dict_keys_lazy   → 60,3,2                            MATCH (was SPIN)
- dict_values_lazy → 100,2                             MATCH (was SPIN)

## Step 3/4 — CONTROL + ASan + BLAST RADIUS all CLEAN
- Iterator sweep (27 fixtures): all MATCH incl. Vector adapters (iter_lazy_adapters,
  iter_chain_*, stdlib_iter_adapters) — control preserved. Only pre-existing CC-FAIL:
  stdlib_iter_dict (DictIter.take() returns int — a chained-call return-type reg bug,
  NOT my path; confirmed CC-FAIL on the PRE-FIX driver too).
- ASan (stdlib_iter_set, dict_keys_lazy, dict_values_lazy, iter_lazy_adapters,
  stdlib_iter_map_filter): rc=0, no UAF / double-free / leak. Receiver-ptr binding is drop-safe.
- BLAST RADIUS: ran all 76 fixtures matching the field-access-receiver shape
  `(self|id).field.method(` through BOTH pre-fix and post-fix drivers; per-fixture status
  is BYTE-IDENTICAL (55 MATCH / 13 CC-FAIL / 0 WRONG / 0 CRASH / 0 HANG both). The 13
  CC-FAILs are all pre-existing "returns int placeholder" / arg-type / OpenSSL-link issues,
  unchanged. ZERO regressions, ZERO compensating flips.
- NET: +3 census fixtures CRASH→MATCH, 0 regressions across the at-risk set.

## Step 3/4 — REGRESSION FIXTURES + FULL GATES
- New fixtures (in proto.patch):
  - `set_filter_count.gg`  → `3`     (`s.iter().filter(k>=20).count()` — the spin shape)
  - `set_take_values.gg`   → `10\n20` (pins `.take(2)` VALUES, not `10,10`)
  Both MATCH on Rust-C + Rust-LLVM + self-host. Integration `run_gg` tests added.
- `cargo test --lib`: 1107 passed / 0 failed.
- Integration (9 iterator tests incl. the 2 new, Rust-C lane): all pass. LLVM (2 new): pass.
- `self_host_bootstrap_fixed_point` (release, 583s): PASS — stage-2==3==4 fixed point HOLDS
  with the lowerer change (Core inv #7 gate). No RSS blow-up (release build finished normally).
- EXPECTED_HANGS guard: NOT landed yet (census recommendation only) — nothing to shrink now.

================================================================================
## GO / NO-GO:  **GO**
================================================================================
Root cause (confirmed by RUNNING, not source-read): the self-host method-receiver lowering
value-copied a `&self` receiver that is a struct-FIELD place when the field type is not
resource-classified. SetIter/DictKeysIter/DictValuesIter hold a `Ref` (not a resource), so
`self.inner.next()` inside the lazy adapters (`Take`/`Skip`/`Filter`/… + terminals) advanced a
DISCARDED byte-copy of the inner cursor → infinite re-yield of element 0. VectorIter holds a
`Vector` (a resource → already Ptr-loaded) so its adapters were fine. The LIR `GIFieldLoad`
handler already alias-stores a Ptr-typed dst (and already carves out `Ref__`/opaque fields), so
the fix is purely to make the RECEIVER field-load Ptr-typed for a plain struct field.

FIX (write-site, Core inv #1; mirrors Rust methods.rs:2037-2064):
- New `lower_recv_place` (self_host_lowerer/lower_expr.gg, just before `lower_expr`), called at
  the EMethodCall receiver site (was `lower_place_base`, ~line 3289).
- For an `EFieldAccess` receiver on a BARE-LOCAL / `ESelfExpr` base whose field is a plain
  (non-scalar `>= PRIM_COUNT`, `not is_resource_type_name`, `not Ref__/MutRef__`) struct →
  emit a Ptr-typed `GIFieldLoad` = borrow the field PLACE. Everything else delegates to
  `lower_place_base` UNCHANGED (byte-identical emission; the bare-base restriction makes the
  fall-through re-resolve free — `nl_get` never emits).
- Typed-metadata-driven, no name-matching (`is_resource_type_name` + field-type-id gate;
  the one `Ref__`/`MutRef__` name-prefix test is the SAME accepted residual the LIR handler
  and `resolve_field_lir_type` already use).

VALIDATION (all end-to-end, compile+run+diff):
- 3 census fixtures CRASH/SPIN → MATCH.  2 new regression fixtures MATCH ×3 lanes.
- 76 field-access-receiver fixtures: per-fixture status BYTE-IDENTICAL pre vs post (0 flips).
- ASan clean.  --lib 1107/0.  bootstrap_fixed_point PASS.

EXECUTOR-BRIEF RECOMMENDATIONS:
1. Land the 4-file patch (`/tmp/roota_proto.patch`) as-is; applies cleanly (round-trip verified).
2. Run the FULL parity regen and RAISE the floor: `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600
   cargo test --test integration --release self_host_runtime_diff -- --nocapture`. Expect
   MATCH +3 (the 3 CRASH→MATCH), 0 regressions predicted. Bump `RUNTIME_DIFF_MATCH_FLOOR`
   (currently seeded 1161 from MATCH 1166) by +3 in the SAME commit, per the ratchet.
   ⚠ MEMORY.md: the parity harness OOMs solo (~15.9GB) — prune agent worktrees first / run
   when the box is quiet; if it OOMs, the +3 is still safe to assert from a targeted regen.
3. EXPECTED_HANGS no-new-hangs guard (census §RECOMMENDATIONS(i)): if landed here or later,
   it must list ONLY `async_select` — this track removes dict_keys_lazy / dict_values_lazy /
   stdlib_iter_set from the hang set. (Landing the guard in this commit = it starts at 1, not 4.)
4. Fresh output-review + the standard gauntlet; this is a SEMANTIC self-host lane fix, but it is
   self-host-lane-only (Rust C/LLVM already correct; ggdef out-of-subset — stdlib generic
   iterators are not in the ggdef core) → per Core inv #9 the cross-lane obligation is met by
   the 2 new fixtures being 3-lane MATCHes; note the ggdef exemption in the commit.

KNOWN LIMITATIONS / FOLLOW-UPS (report, do NOT expand scope here):
- The fix covers a SINGLE-level bare-base field receiver (`self.inner.next()` = the entire census
  population). A deeper chain (`self.a.b.next()` with `a` a non-resource struct) or a static/Box/
  Guard-based struct-field receiver still value-copies (falls through to `lower_place_base`). Not
  in the corpus. A fuller fix teaches `lower_place_base` itself to borrow field places recursively
  — bigger blast radius, MEASURE before doing. File as Low.
- SEPARATE pre-existing class, NOT this root: `stdlib_iter_dict` / `linked_list` / `tensor_extra`
  CC-FAIL with `DictIter.take()`/`MapIter` "returns int placeholder" (a chained-call adapter
  return-type registration bug on Dict/Map iterators). Confirmed CC-FAIL on the pre-fix driver
  too. Own track.
- Do NOT chase the pre-existing lazy-iterator leak (TODO ~207, BOTH compilers) — my ASan runs
  on the touched fixtures were clean regardless.

DELIVERABLES: this file + `/tmp/roota_proto.patch` (4 files, applies clean). Commit nothing.
No lingering processes (ps-verified). Driver artifact left in place (build_gg_dir_cached contract).
