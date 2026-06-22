# `throws`-codegen scout — two pre-existing memory-unsafety / ill-typed-C defects

**Status:** READ-ONLY scout (no implementation, no commit). Both bugs **confirmed
to reproduce on the current `gorget-1` tip**. Each is a Core-#8 known defect
(must not ship). Root cause located + reference-grade fix designed for both.

Doc grounding: `docs/devbook/11-copy-on-write.md` ("Ownership at Consuming
Positions", `emit_enum_init_owned` at `context.rs:1306`, `MoveZero` §"`MoveZero`
and post-call ownership transfer"); `docs/devbook/24-layering-discipline.md`
(rule 3 "one source of truth per axis", rule 4 "resolve once, write through");
`CLAUDE.md` Core #1 (fix at the WRITE site), Core #3 (register ownership at the
value's birth), Core #4 (one fix, all siblings), and the debugging heuristic
("fix complexity is a signal of the wrong layer").

---

## BUG 1 — `throws` equip-METHOD call consumed by `catch` (or bare-`T`) emits ill-typed C

### (1) Confirmed-reproduces-on-current-tree

Fixture (`/tmp/throws_scout/bug1_method.gg`):

```gorget
struct Calc:
    int base

equip Calc:
    int add(self, int x) throws String:
        if x < 0:
            throw "negative"
        return self.base + x

void main():
    Calc c = Calc(10)
    int r = c.add(5) catch (e): -1
    print(r)
    int r2 = c.add(-1) catch (e): -99
    print(r2)
```

Command: `gg build /tmp/throws_scout/bug1_method.gg -o …`

Observed cc errors (exactly as filed in TODO):

```
error: incompatible types when assigning to type 'int64_t' from type '__gg_Result__int64_t__GorgetString'
  #define __v6 __coal1            // __v6 is int64_t
  __s6 = __v6;
error: cannot convert to a pointer type
  __v9 = (void*)&((Str *)(__v6))->data;
```

**Contrasts confirmed by RUNNING (not source-reading):**

| Shape | Result |
|-------|--------|
| free-fn `int r = add(10, 5) catch (e): -1` | **WORKS** → `15` / `-99` |
| method bound to explicit `Result[int,String] r = c.add(5)` + `match` | **WORKS** → `15` |
| method consumed by `catch` (`int r = c.add(5) catch (e): -1`) | **ill-typed C** |

So the method *body* is correct (it emits a `Result`-returning C function); the
divergence is at the CALL SITE.

### (2) Precise root-cause WRITE site

**`src/ir/lowering/mod.rs:1004`** — the non-generic equip-method `fn_sigs`
pre-scan registers the method's return type **WITHOUT the `throws` → `Result`
synthesis**:

```rust
// mod.rs:1000-1004
for method in &equip.items {
    let method_def = &method.node;
    let mangled = format!("{}__{}", type_name, method_def.name.node);
    let ret_type = ctx.type_mapper.map_ast_type_mut(&method_def.return_type.node, &mut ctx.type_registry);
    //              ^^^ ignores method_def.throws — registers bare `int`, not `Result[int, String]`
```

Compare the **free-function** pre-scan, which DOES synthesize it
(`mod.rs:629-654`, the `} else if func.throws.is_some() { … Result__{ok}__{err} … }`
branch), AND the method-body lowering `lower_equip_method`
(`functions.rs:968-993`), which ALSO synthesizes it correctly. The pre-scan is the
ONE copy of the throws→Result resolution that omits the `throws` axis.

Downstream read: at the call site, `lower_method_call` resolves the result type
from this stale entry — `fn_sig_ret = ctx.fn_sigs.get(&effective_name)`
(`exprs/methods.rs:2121`) → `ret_type` (`methods.rs:2204`) → the `dst` of
`ctx.call_tracked(builder, call_name, call_args, ret_type)` (`methods.rs:2600`).
So the call-result local is typed `int64_t` while the emitted `Calc__add` C
function returns `__gg_Result__int64_t__GorgetString`. `lower_catch_expr`
(`exprs/mod.rs:3424`) then `infer_operand_type_full`s the mis-typed local, reads
the (nonexistent) `Result` tag off it as `->data`, and emits the assignment
mismatch.

**This is NOT a missing `maybe_auto_propagate` step** (the TODO/brief's "likely
sibling to Snag #43" hypothesis). The catch machinery is fully type-driven and
correct — it works for free functions and for the explicit-`Result`+`match`
binding precisely because in those paths the operand's type IS `Result[T,E]`. The
only thing wrong is the *type recorded for the method-call result*, one layer up
at the `fn_sigs` write site. (Textbook "fix complexity is a signal of the wrong
layer": a read-site auto-prop patch would have been the wrong fix.)

### (3) Reference-grade fix design (Core #1 / devbook-24 rule 4)

Apply the **same** throws→Result synthesis at the equip-method pre-scan that the
free-function pre-scan and `lower_equip_method` already use — write the correct
type through at the source so the call-site read is faithful. In `mod.rs`, replace
the line-1004 `let ret_type = …map_ast_type_mut(return_type)…` with the
`method_def.throws.is_some()` branch that builds/looks-up
`Result__{ok_c}__{err_c}` via `make_result_type_def` (verbatim shape of
`functions.rs:968-993` / `mod.rs:631-650`).

**VERIFIED end-to-end this scout:** applied exactly this patch, rebuilt, and
`gg run /tmp/throws_scout/bug1_method.gg` → `15` / `-99` (correct). Reverted; tree
clean.

**One-fix-all-siblings note (Core #4 / devbook-24 rule 3).** The deeper smell is
that the throws→Result mapping is now spelled in **three** places
(`functions.rs:968` body, `mod.rs:631` free-fn pre-scan, `mod.rs:1004` method
pre-scan). The minimal, safe fix is to add the missing branch at `:1004`. The
reference-grade follow-up (file it; not required to close BUG 1) is to extract a
single `synthesize_throws_result_type(ctx, return_type, throws)` helper and call
it from all three sites, so a fourth throws-sig registration path cannot silently
drift again. The brief-review should weigh "minimal one-branch fix now" vs
"extract-the-helper now"; I lean **extract the helper** — it is the genuine
single-source-of-truth fix and the third drifted copy is exactly the failure
devbook-24 rule 3 warns about.

### Secondary finding (independent of BUG 1 — FILE SEPARATELY, do NOT fold in)

With the BUG-1 fix applied, the int-payload method+catch works. But a
**String-(resource-)payload throws fn consumed by `catch`** then trips a Tier-2a
consume-site validator panic:

```
Tier 2a consume-site violation: … fn @main … AssignIntoOwnedSlot(dst: GorgetString)
  — untracked source consumed (ownership not decided).
```

**This is PRE-EXISTING and orthogonal — it reproduces IDENTICALLY for a FREE
FUNCTION on the UN-patched tree** (`String tag(...) throws String: …` consumed by
`int t = tag(...) catch (e): "err"` panics at `mod.rs:2030` with the same
violation). So it is a `catch`-of-resource-Ok-payload ownership gap, not a
method-vs-free-fn divergence. BUG 1's fix correctly brings the method path to
*parity* with the free-fn path; it neither causes nor cures this resource-payload
catch gap. Recommend a separate TODO entry + fixture (free-fn AND method form).

---

## BUG 2 — expr-body `throws` fn with `T`=`Result`/`Option` having a RESOURCE inner payload DOUBLE-FREES

### (1) Confirmed-reproduces-on-current-tree

Fixture: `tests/fixtures/throws_t_result_resource_inner.gg` (the `#[ignore]`'d
`throws_t_result_resource_inner` test).

Command: `gg build --sanitize tests/fixtures/throws_t_result_resource_inner.gg -o … && ./…`

ASan output (heap String from `"val-"+"ok"`):

```
==…==ERROR: AddressSanitizer: attempting double-free …
  freed by:  gorget_string_free ← Result__GorgetString__GorgetString__drop ← wrap_result (bug2.c:3071)
  also freed: gorget_string_free ← main (bug2.c:3488)
  allocated:  gorget_str_cat ← mk_result   (the "val-"+"ok" concat)
```

i.e. the inner `Result[String,String]`'s heap String is freed **once inside
`wrap_result`** (the inner enum's `__drop` at scope exit) AND **once at the call
site in `main`** (the value the outer-`Ok`-wrap shallow-aliased).

**Scope narrowed by RUNNING each shape under `--sanitize`:**

| Shape | Result |
|-------|--------|
| **expr-body** `Result[String,String] wrap(int x) throws String: mk(x)` | **DOUBLE-FREE** |
| block-body `… throws String:` / `return mk(x)` | **CLEAN** → `val-ok` |
| block-body, `T=Option[String]`, `return mk_option(x)` | **CLEAN** → `opt-yes` |
| expr-body, `T=Result[int,String]` (NON-resource int inner) | **CLEAN** → `15` |

So BUG 2 is **specifically the expr-body tail path with a resource inner
payload.** The block-body forms are already correct, and the non-resource inner
is harmless (shallow memcpy of an int is fine, inner-drop is a no-op).

### (2) Precise root-cause WRITE site — the failed ownership transfer

**`src/ir/lowering/functions.rs:85`**, inside `wrap_expr_tail_in_ok`:

```rust
// functions.rs:69-87
fn wrap_expr_tail_in_ok(ctx, builder, operand, ret_type, throws) -> Operand {
    if !throws { return operand; }
    let op_ty = infer_operand_type_full(ctx, &operand, builder);
    if op_ty == ret_type { return operand; }
    let type_name = ctx.type_registry.type_name(ret_type).unwrap_or("Result");
    let ok_val = builder.enum_init(type_name, "Ok", ret_type, vec![operand]);  // <-- RAW enum_init
    FunctionBuilder::copy(ok_val)
}
```

`builder.enum_init` (`builder.rs:437`) emits a **raw `Instruction::EnumInit`** —
a shallow memcpy of the payload bytes into the variant slot, with **no clone, no
`MoveZero`, and no drop-unregister of the consumed source**. The inner Result
temp (`__s2` in the emitted C) is byte-copied into the outer `Ok_0` field, but its
scope-exit `DropEntry` stays live, so it is dropped at `wrap_result` exit while
the outer Result (returned, aliasing the same `data`) is dropped again at the call
site.

**The exact contrast in emitted C** — expr-body (broken) vs block-body (correct),
same inner-Result temp `__s2`:

```c
// EXPR-BODY (wrap_expr_tail_in_ok, bug2.c:3057-3073) — DOUBLE FREES
__s2 = __v2;                                  // inner Result (owns heap String)
memcpy(__v11 /*outer Ok_0*/, __v4 /*&__s2*/, sizeof(inner Result));  // shallow
memcpy(&__s0 /*ret slot*/, __v5 /*&__s3 outer*/, …);
Result__GorgetString__GorgetString__drop(__v7 /*&__s2*/);   // <-- FIRST free (no move-zero!)
return *…__s0;                                // returned value re-drops at call site → 2nd free

// BLOCK-BODY (lower_return Ok-wrap, bug2_block.c) — CLEAN
__s2 = __v2;
memcpy(__v9 /*outer Ok_0*/, __v4 /*&__s2*/, sizeof(inner Result));  // shallow
memcpy(&__s0, __v5, …);
return *…__s0;                                // NO drop of __s2 — it was move-zeroed
```

The block-body path is `lower_return` (`stmts/mod.rs:1765-1830`), whose Ok-wrap
branch ALSO uses raw `builder.enum_init` (`:1810`) but then **move-zeros the
consumed source** at `:1827-1829`:

```rust
if let Some(local) = returned_local {
    ctx.move_zero_and_mark(builder, local);   // suppresses the source's exit drop
}
```

`wrap_expr_tail_in_ok` is the **sibling site that forgot this step** (and also
forgot the resource-clone-vs-move decision that `lower_return:1769-1806` makes).
Classic sibling-site drift (Core #4 / devbook-24 §"sibling-site drift"). The
expr-body arm captures `returned_local` from the ALREADY-wrapped operand
(`functions.rs:922`, = the outer `ok_val`), so `emit_early_exit_drops`
(`functions.rs:929`) skips the outer wrapper but never learns the inner source
needs suppression.

### (3) Reference-grade fix design (Core #3 — register/transfer ownership at the value's birth)

**Preferred: route `wrap_expr_tail_in_ok`'s Ok-construction through the
CoW-aware enum-init helper, not raw `builder.enum_init`.**

`emit_enum_init_owned` (`context.rs:1306`) is the single chokepoint that already
does exactly the right thing for user-level `Ok(...)`/`Some(...)`
(`exprs/mod.rs:1487-1626`): it `clone_resource_args_for_init`s the payload
(clone-if-live, move-if-dead per the CoW table), then **`drops.unregister`s the
consumed source** (`context.rs:1346/1349`) so it is not re-dropped at scope exit.

Replace the `wrap_expr_tail_in_ok` body's `builder.enum_init(…)` with
`ctx.emit_enum_init_owned(builder, &type_name, "Ok", ret_type, vec![operand], Some(vec![span]))`.
The inner Result temp from `mk(x)` is an owned, last-use temp → the helper MOVES
it (no clone) and unregisters it from drops → the inner `__drop` at scope exit is
elided, leaving exactly one owner (the returned outer Result). No read-side
drop-suppress, no save/restore — the transfer is registered at the wrap site
(the value's consuming birth into the outer `Ok`), which is the Core-#3 location.

This is the architecturally-correct unification: **every** Ok/Some wrap — user
`Ok(...)`, the `lower_return` throws wrap, AND the expr-body tail wrap — should go
through `emit_enum_init_owned`, so the consume-position ownership rule has one
implementation. (`functions.rs:870` `throws main`'s implicit `Ok(unit)` uses raw
`enum_init` too, but `unit` is non-resource so it's safe; route it through the
helper anyway for uniformity, or leave it — reviewer's call.)

**Fallback (if the helper has a context constraint that makes it awkward from
`wrap_expr_tail_in_ok`):** mirror `lower_return:1769-1829` exactly — clone the
operand if it is a borrowed/live resource, build the Ok via `enum_init`, then
`move_zero_and_mark` the consumed source local BEFORE `returned_local` is
recomputed in the caller. This is strictly more code than the helper route and
re-implements the sibling logic — so it is the fallback, not the recommendation.

**Litmus (devbook-11):** if the fix sketch grows save/restore or per-shape rules,
it's the wrong layer — the right fix is one call swap (`enum_init` →
`emit_enum_init_owned`) at the producer.

### (4) Do the two bugs share a root?

**No — they are independent.**

- BUG 1: `mod.rs:1004` (the equip-method `fn_sigs` pre-scan omits throws→Result).
  A *type-resolution* gap. Fix: add the throws branch at the sig write site.
- BUG 2: `functions.rs:85` (`wrap_expr_tail_in_ok` uses raw `enum_init`, no
  ownership transfer). An *ownership-transfer* gap. Fix: route through
  `emit_enum_init_owned`.

Different files, different mechanisms, no shared code. BUG 2 *does* have an
internal sibling (the `lower_return` outer-Ok wrap, `stmts/mod.rs:1810`) that got
ownership transfer right — that's the within-BUG-2 "one fix, all siblings"
relationship, not a shared root with BUG 1. They CAN be fixed independently and in
parallel (disjoint write sites). Note both fixes touch the same broad subsystem
(`src/ir/lowering/`) so a single executor doing both sequentially is cleanest;
they do not conflict.

### (5) Fixtures to add / un-ignore

- **BUG 1:** add `tests/fixtures/throws_method_catch.gg` (or extend an existing
  throws fixture) exercising a `throws` equip method consumed by `catch` —
  success AND error paths — with `int` payload (the case the fix closes). Assert
  the two-line output (`15` / `-99` shape). The String-payload method+catch shape
  is the *secondary finding* (separate TODO); its fixture rides the resource-catch
  fix, not BUG 1.
- **BUG 2:** un-`#[ignore]` the existing `throws_t_result_resource_inner` test and
  flip `tests/fixtures/throws_t_result_resource_inner.gg` to active. Its committed
  expected output is the language-correct `val-ok\nopt-yes`. Gate the fix with
  `--sanitize` (ASan/LSan must be clean) — the double-free is the canary.
  Recommend also adding the block-body twin shapes to the same fixture as
  regression anchors (they already work; lock them so the fix stays uniform).

### Validation gates (per CLAUDE.md, for whoever implements)

`cargo test --lib`; the error-handling integration suite; `--sanitize` clean on
every `T=Result`/`T=Option` resource-inner shape (BUG 2); `c_emit_comparison` and
`self_host_runtime` 0-regressed; `self_host_bootstrap_fixed_point` GREEN (the
double-free canary). Re-run the existing `throws_expr_body_tail.gg` to confirm the
non-resource-inner cases stay green.
