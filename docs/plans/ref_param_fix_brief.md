# Executor Brief — fix the self-host `&`/`!`-param scalar read/write miscompile

**Status:** DRAFT — under ≥3 fresh-review discipline before launch.
**Root cause:** CONFIRMED + cross-checked against source (this brief's line cites are verified).
**Goal:** make the self-host compile `&`/`!`-param value read/write the way Rust gg does (deref-load on
read, store-through-pointer on write), retiring a real language-spec violation. The self-host is the
language's stress test — this is a snag to FIX, not work around.

---

## 0. Worktree discipline (NON-NEGOTIABLE)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm both point inside YOUR worktree. NEVER touch
`/workspace/gorget-1`. Do NOT `cd` there or use `/workspace/gorget-1/...` paths. If `pwd` is
`/workspace/gorget-1`, STOP and report.
Stage explicitly by name (`git add <files>`); NEVER `git add -a`/`.`/`commit -a`. Commit in your worktree so
the parent can cherry-pick. Run `cargo build` + `cargo test --lib` + the targeted gates below — NOT the full
integration sweep (parent's job). FORCE-REBUILD the driver before any comparison/bootstrap run:
`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`.

## 1. The bug (confirmed by side-by-side emitted-C diff vs Rust gg)
`&`/`!` params (ownership 1/2) are typed **`GtMutPtr(inner)`** (`lower.gg:7773-7774`). A scalar `int &x` slot
holds an `int64_t*`. Every site that should produce/consume the VALUE `*ptr` instead handles the POINTER
bits, because the deref/store-through-pointer step was dropped in the port from Rust:
- `int &x` READ → `add_overflow((int64_t)__v0, 10, …)` (arithmetic on the pointer; no deref).
- int/bool/String WRITE → writes a dead LOCAL slot, never `*(T*)ptr = v`.
- `String &s` READ derefs OK (resources are already pointer-valued) — but its WRITE is still dropped.
Rust gg (the spec; `language-reference.md:301,553` = `&` is read+write) derefs to read and stores through
the pointer to write. The self-host's OWN source uses ZERO scalar `&`-params and ZERO whole-value `&`-param
reassignment (all its `&`-params are aggregates mutated via `.field=`/`.push()`), so **this fix has ZERO
blast radius on `fixed_point`** — verified by the design recon.

## 2. Detection predicate (the discriminator — get this exactly right)
Match the local's GIR type. Fix ONLY `GtMutPtr(inner)`; distinguish from the working cases:
- `GtMutPtr(inner)`, `inner` NON-resource (scalar/value) → **broken read AND write** → fix both.
- `GtMutPtr(inner)`, `inner` a resource (String/collection/struct-with-resource) → read already works
  (pointer-valued slot, consumers deref); **write is also broken** → fix write only, leave read alone.
- `GtPtr(inner)` (bare-by-value resource param, `lower.gg:7770` via `register_ptr`) → **leave entirely
  alone** (already reads correctly).
⇒ READ fix gates on `GtMutPtr(non-resource inner)`; WRITE fix gates on `GtMutPtr(any inner)`.
Predicate: fetch the local's `type_id`, `gmod.type_table.get(tid)`, `match GtMutPtr(inner)`, then for the
read-gate `not is_resource_type_name(type_id_to_name(inner, &gmod), &gmod)` (`is_resource_type_name` at
`lower.gg:3844`). ⚠ Do NOT use `is_ptr_type` (`lir_lower.gg:2003`) — it returns true for BOTH `GtPtr` and
`GtMutPtr` and would misfire on resource bare params. (`LoParam`/`BoParam` origins exist but every param gets
`LoParam()/BoParam(-1)` at `lower.gg:7776` regardless of ownership, so they CAN'T discriminate `&`/`!` from
bare-resource — the GIR TYPE `GtMutPtr` is the only correct signal, and it's sufficient: only `&`/`!` params
produce `GtMutPtr`-typed named locals.)

## 3. The four edits + one new GIR op

### New op: `GIDerefStore(int ptr_local, Operand value, int inner_ty)` — store `value` through `*ptr_local`
Add to the `gir.gg` Instruction enum **at the TAIL — after `GIFieldLoad` (`gir.gg:128`), NOT mid-enum**
(MEMORY pitfall: inserting mid-enum shifts ordinals; self-host matches are by-name but follow the safe
end-append rule). It is the inverse of `GIDeref` (`gir.gg:117` = `GIDeref(dst, src_ptr, inner_ty)`; its
lowering at `lir_lower.gg:3247`). Thread it through:
- **`gir.gg`** — add the variant (carry the `inner_ty` too if the LIR store needs it: `GIDerefStore(int,
  Operand, int)` = ptr_local, value, inner_ty — match `GIDeref`'s shape).
- **`lir_lower.gg`** — new arm near `GIDeref` (`:3247`). ✅ **`IStore(ptr, value)` ALREADY EXISTS**
  (`lir.gg:150`) and its C codegen (`lir_codegen.gg:3138-3149`) already dispatches scalar
  (`*(T*)ptr = value`) / aggregate (`memcpy(ptr, &value, sizeof)`) / ptr — exactly a whole-value
  store-through-pointer for BOTH non-resource and resource inner. So the `GIDerefStore` arm is just:
  `ISlotLoad(ptr_val, ptr_slot, LT_PTR)` (load the pointer) → `lower_operand(value)` → `IStore(ptr_val,
  lowered_value)`. **DO NOT add a new LIR inst and DO NOT add a new `lir_codegen.gg` C-emit arm** (review-pass-1
  confirmed `IStore` covers it). The GIR-op-only path is the clean one.
- **`lir_codegen.gg`** — NO new arm needed (the existing `IStore` codegen at `:3138-3149` already emits
  `*(T*)ptr = value;` / `memcpy` per inner type).
- **`format_gir.gg`** — add a render arm for `GIDerefStore` (mirror the `GIDeref` arm at `format_gir.gg:188`)
  so GIR dumps don't break.
- ⚠ **`lower.gg` liveness arms — RUNTIME-CORRECTNESS-required, NOT exhaustiveness-enforced (review-pass-4
  — DO NOT SKIP, and DO NOT trust the compiler to catch a miss).** ⚠⚠ **CRITICAL (pass-4, empirically
  verified): Rust gg enforces `match` exhaustiveness ONLY on the ENTRY file (`driver.gg`). Imported module
  bodies (`lower.gg`/`lir_lower.gg`/`format_gir.gg`/`validate.gg`) are NOT exhaustiveness-checked — a missing
  `case` arm with no `else` SILENTLY FALLS THROUGH to the trailing default at RUNTIME (returns
  `-1`/empty/no-op), with NO build error and NO crash.** (Proof: `format_gir.gg:166` is ALREADY missing its
  `GIFieldLoad` arm and the driver builds + checks clean.) So a missed `GIDerefStore` liveness arm =
  a SILENT wrong-liveness MISCOMPILE of the self-host (the `ptr_local`+`value` reads go unrecorded → bad
  move/clone decisions — the exact bug class this chain fixes). **You CANNOT lean on the compiler; discovery
  MUST be exhaustive grep, verified by hand.**
  The COMPLETE set of no-`else` `Instruction` matchers needing a `GIDerefStore` arm (pass-4 grep-verified
  complete across the whole dir): `liveness_inst_def` (`lower.gg:1845`) → NO def (stores through a pointer,
  defines no value local; `return -1`, mirror `GIDrop`'s no-def arm); `liveness_inst_operand_uses`
  (`:1883`) + `liveness_inst_operand_local_ids` (`:2272`) → record BOTH reads: the `ptr_local` the
  `GIDeref` way (raw int local: `acc.set`/`out.push(ptr_local)`) AND the `value` the `GIAssign` way (via
  `liveness_operand_uses`/`liveness_operand_local_id` on the Operand). PLUS the already-listed
  `lir_lower.gg:2604` lowering arm and `format_gir.gg:166` render arm (both ALSO no-`else`). The `else`-having
  matchers absorb the variant silently — NO arm needed FOR CORRECTNESS: `rewrite_inst_modes`
  (`lower.gg:2566`, `else` `:2593` — but it optionally gets a `GIDerefStore` arm for clone→move PARITY, see
  Edit-2's note, distinct from this correctness floor), `update_last_pos_for_inst` (`:2626`, `else` `:2643`),
  and all 3 `validate.gg` `Instruction` matchers (`:105`/`:171`/`:237`, all with `else`).
**So the new op threads through 4 files** (`gir.gg` variant + `lir_lower.gg` lowering arm + `format_gir.gg`
render arm + `lower.gg`'s 3 liveness arms) — NOT 3. ⚠ Verify by `grep -rn "GIFieldLoad" tests/fixtures/
self_host_lowerer/` (a KNOWN partially-handled variant) to enumerate every `Instruction` matcher, then
hand-check each has (or you add) a `GIDerefStore` arm — the build will NOT tell you. (No new LIR inst / no
new `lir_codegen.gg` C-emit — codegen reuses `IStore`.)

### Edit 1 — READ (`lower.gg:4074-4075`)
Currently `case EIdentifier(name): if nl_contains(&ctx, name): return nl_get(&ctx, name)` returns the MutPtr
slot. After `int lid = nl_get(&ctx, name)`, fetch its type; if `GtMutPtr(inner)` with NON-resource `inner`,
emit a deref-load into a fresh value temp and return THAT:
```
int value_local = add_local(&ctx, inner, NO_NAME)
emit(&ctx, GIDeref(value_local, lid, inner))
return value_local
```
else return `lid` unchanged. **This is the EXACT pattern already shipped at `lower.gg:6599-6607`** (the
`Option[Ref[T]]` getter: resource inner → return dst; primitive inner → `GIDeref` into a value temp) — copy
it. Do NOT deref resource inner (that path works; double-deref would break it). Rust parity:
`src/ir/lowering/exprs/mod.rs:140-164` (Deref projection into a `pointee_type` temp). ⚠ NOTE the deliberate
divergence: Rust's read-deref gates on `pointee_type` existing (it derefs resources too, via Copy=clone),
but the self-host read-gate fires ONLY for NON-resource inner — because self-host resource slots are
pointer-valued by ABI and their consumers already deref. This matches the `lower.gg:6599` precedent; do NOT
"fix" it to match Rust's gate or you'll double-deref the working `&String` read path. (The WRITE fire on any
inner, by contrast — resource `&`-writes ARE currently broken.)

### Edit 2 — WRITE (`lower.gg:6099-6118`)
`case SAssign → EIdentifier`: currently `emit(&ctx, GIAssign(lid, op_consume(...)))`. If `lid` is
`GtMutPtr(inner)` (ANY inner — resource writes are also broken), keep the existing `expected_type`
propagation (`:6112-6116`), lower the RHS, then emit `GIDerefStore(lid, op_consume(&ctx, &gmod, val,
CkAssign()), inner)` instead of `GIAssign`. Else keep `GIAssign`. Rust parity:
`src/ir/lowering/stmts/assigns.rs:231-243` (`assign` into a `Place{local,[Deref]}`).
⚠ **(pass-4) clone-not-move for resource `&`-writes — wire `GIDerefStore` into `rewrite_inst_modes`.**
`GIDerefStore` carries an `Operand value`, but `rewrite_inst_modes` (the `OpClone`→`OpMove` last-use
promoter, `lower.gg:2566`) currently absorbs it via its `else` (no promotion). For SCALAR targets (`int &x`,
`bool &b` — the PRIMARY bug) `op_consume` returns `OpCopy` (never promoted) → zero impact. But for a RESOURCE
write (`String &s = s + "!"`) `op_consume` returns `OpClone`, which then stays an un-promoted CLONE where
Rust emits a MOVE → CORRECT output but an extra clone (a `c_emit` byte-parity miss vs Rust, NOT a crash; the
fixture still passes). Add a `GIDerefStore` arm to `rewrite_inst_modes` that promotes its `value` operand at
last-use (mirror the `GIAssign` arm's `wire_one_operand`, `lower.gg:2517`) so resource `&`-writes move like
Rust. If wiring it complicates the chain, you MAY defer — but then log a TODO (`GIDerefStore` resource
`&`-write over-clones by 1 vs Rust) and note it in the report.

### Edit 3 — COMPOUND-ASSIGN (`lower.gg:6199-6213`) — the recon's extra find, do NOT skip
`case SCompoundAssign → EIdentifier` (`x += 1` on a `&`-param) is broken on BOTH legs: it reads `lid`
directly via `op_consume(lid, …)` (`:6209`, bypassing Edit 1's read fix) AND writes via `GIAssign(lid,…)`
(`:6213`, the rebind bug). If `lid` is `GtMutPtr(inner)`: (a) replace the `:6209` read of `lid` with a
deref-load (`GIDeref(tmp, lid, inner)` then use `tmp` as the lhs value), and (b) replace the `:6213`
`GIAssign(lid, op_dst)` with `GIDerefStore(lid, op_dst, inner)`. Gate on `GtMutPtr`.

### Edit 4 — `&global` scalar-read sibling (`lir_lower.gg:2987-2998`) — SAME class, DISTINCT site
The `__global_ref__<name>` rewrite emits `IGlobalAddr(gr_dst, gid) → ISlotStore(dst_slot, gr_dst)` — it
stores the ADDRESS, missing the load (Rust does GlobalAddr→Load, `src/lir/lower/operands.rs:128-134`).
Symptom: scalar runtime-static reads (`INFINITY`/`NAN` from `lib/std/math.gg`) emit `&__lir_gN` → float
prints `0.0` / `void* > double` cc-fails (the Chain-2 `static_init_imported`/`math_constants`/`numeric_trait`
exclusions). Fix: when the dst slot is a VALUE type (NOT a pointer/resource — `stdout`/`stderr`/`stdin` are
resource statics where address-of IS wanted, leave those), insert an `ILoad(loaded, gr_dst, dst_slot_ty)`
between the `IGlobalAddr` and the store, storing `loaded`. ⚠ This is a DISTINCT code path (LIR, not the GIR
param path) — fold it into this chain but do NOT conflate the two; gate it on the dst-slot being a
non-pointer value type. (`const float PI/E/TAU` inline earlier via `lower_float_const_ident` and are
unaffected.)

## 4. New active fixture
Add `tests/fixtures/ref_param_reassign.gg` (NOT `#[ignore]`) + register it in `tests/integration.rs`
mirroring `static_ref_param` (`integration.rs:~16003` — grep the fn name, line cites drift). Assert exact runtime output. Use VALID Gorget
syntax (verify the bool-to-string idiom against the language — the harness builds via Rust gg, which is
correct):
```
from std.conv import int_to_str
struct S:
    int v
void bump_int(int &x): x = x + 10
void append_str(String &s): s = s + "!"
void flip(bool &b): b = true
void set_v(S &s, int n): s.v = n      # struct field-write — already works (regression guard)
void main():
    int i = 5
    bump_int(&i)
    print("int:" + int_to_str(i))      # int:15
    String t = "hi"
    append_str(&t)
    print("str:" + t)                  # str:hi!
    bool b = false
    flip(&b)
    if b: print("bool:true")
    else: print("bool:false")          # bool:true
    S s = S(0)
    set_v(&s, 99)
    print("struct:" + int_to_str(s.v)) # struct:99
```
Expected stdout: `int:15` / `str:hi!` / `bool:true` / `struct:99`. (Confirm `gg run` gives this — Rust gg is
correct; this fixture then doubles as a self-host parity datapoint via `c_emit_comparison`.)

## 5. Validation gates (ALL must hold)
1. `cargo build` clean; `cargo test --lib` green.
2. `ref_param_reassign` passes (Rust gg builds it correctly; the point is the SELF-HOST now matches — verify
   by building the fixture's `--emit-c` self-host output and running it: `int:15/str:hi!/bool:true/struct:99`).
3. Targeted: `cargo test --test integration static_ref_param math_constants numeric_trait numeric_trait_ops
   static_init_imported ref_param_reassign self_host_full_program` — all pass; the Chain-2 `TODO(chain3)`
   exclusions for the `&global` fixtures should now compile+run correctly (verify; if so, REMOVE them from
   the gate's exclusion list and assert them — that's the fix paying off).
4. **Output-neutrality where required:** force-rebuild the driver, then `c_emit_comparison` ≥ **849** (no
   regression; should rise as `&global`/`&`-param fixtures reach parity — record the new number) and
   `lowerer_comparison` = **951** (fn-count neutral — the new `GIDerefStore` op doesn't change fn shape).
   `self_host_bootstrap_fixed_point` GREEN (zero blast — re-confirm byte-reconvergence).
5. Report: the diff/commit; which broken shapes you fixed (read/write/compound/global); the
   before/after `c_emit` count; whether the 3 `&global` fixtures now pass (and were de-excluded); the
   `ref_param_reassign` runtime output; confirmation `fixed_point` is GREEN.

## 6. Hard rules
- Don't redesign around gaps. If a fix exposes a deeper gap, INVESTIGATE + log to TODO; don't reshape to
  hide it. The fix must make the self-host MATCH Rust, not paper over a difference.
- Mirror Rust exactly (the cited `src/ir/lowering/...` sites are the spec). When in doubt, diff the
  self-host's emitted C for `ref_param_reassign` against Rust gg's (`--emit-c-lir`) and converge.
- Stay in the file zone: `lower.gg` (the 3 semantic edits at 4075/6118/6209+6213 **+ 3 trivial
  exhaustiveness arms** for the new variant at the `:1844/:1882/:2270` liveness matchers — see §3),
  `gir.gg` (+`GIDerefStore` variant at the enum tail), `lir_lower.gg` (+the `GIDerefStore` arm reusing
  `IStore`, +the Edit-4 `ILoad`), `format_gir.gg` (+render arm) — all in `tests/fixtures/self_host_lowerer/`
  — plus the new fixture + its `integration.rs` registration. (NO `lir.gg` edit — `IStore` already exists;
  NO `lir_codegen.gg` edit — its `IStore` codegen already emits the store.) These are unique to the lowerer dir (only parser.gg/ast.gg
  are symlinked), so no cross-dir propagation needed. ⚠ Another chain (R5 PERF) edits `lower.gg:3093` and
  (R5 FIDELITY) edits `lower.gg` codegen region — your edits are at 4075/6118/6209 (disjoint line ranges);
  if those chains land first you may rebase, but you're branching from current `gorget-1` so it's moot.
