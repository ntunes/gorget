# Scout: `throws` fn/method with a RESOURCE success payload consumed by `catch`

**Status:** Root-caused, fix PROTOTYPED + MEASURED (ASan-clean, both backends). Ready to brief an executor.
**Scouted on:** gorget-1 tip `e02627ae` (worktree fast-forwarded; `git merge --ff-only gorget-1` → "Already up to date").
**Mission:** the pre-existing Tier-2a codegen crash for `String f(int) throws String` / `String add(self, int) throws String` consumed by `catch`. Orthogonal to the already-fixed equip-method-vs-free-fn `fn_sigs` type-synthesis bug (`throws_method_catch.gg`).

---

## 1. Measured repro — BOTH forms panic IDENTICALLY

Free fn (`/tmp/throwscatch_fn.gg`):
```
String f(int x) throws String:
    if x < 0:
        throw "negative"
    return "value"            # heap String success payload
void main():
    String r = f(5) catch (e): e
    print(r)
```
Equip method (`/tmp/throwscatch_method.gg`): same body inside `equip Adder: String add(self, int x) throws String: …`, consumed by `a.add(5) catch (e): e`.

Both panic, byte-identical message and site:
```
thread 'gg-main' panicked at src/ir/lowering/mod.rs:2061:17:
Tier 2a consume-site violation: 1 violation(s) in module '<unknown>'.
First: fn @main bb2 i3 — AssignIntoOwnedSlot(dst: GorgetString) — untracked source consumed (ownership not decided).
```
Backtrace: `validate_consume_sites` → panic in `lower_module` (`src/ir/lowering/mod.rs:2061`). The validator class is `AssignIntoOwnedSlot`, violation `UntrackedSourceConsumed` (`src/ir/validate.rs:2799-2804`, `2326`/`2379`).

## 2. Root cause — the `catch (e):` error binding is born OWNED but left `Untracked`

Dumped GIR for `main` (free-fn form), error path `bb2`:
```
local _6 : GorgetString  ownership=Owned       name=None         # result_local (Snag #38 tags this)
local _8 : GorgetString  ownership=Owned       name=None         # err_val (enum_field_load_move → Owned)
local _9 : GorgetString  ownership=Untracked   name=Some("e")    # err_local  ← THE BUG
bb2:
  i0: EnumFieldLoad { dst:_8, base:_3, variant:"Error", field:0, mode:Move }
  i1: MoveZero { _3 }                                            # base payload moved out
  i2: Assign { Move, dst:_9, value: Copy(_8) }                   # err binding receives the moved-out payload
  i3: Assign { Move, dst:_6 (result_local, Owned), value: Copy(_9) }   # ← PANIC: source _9 is Untracked
```
Method form is the same shape (`_12` = `e`, `Untracked`, consumed at `bb2 i3` into `result_local`).

The recovery expression `catch (e): e` evaluates to the bare error binding `_9`. `_9` is created by
`builder.add_local(err_field_type, Some(&error_binding.node))` and receives a **Move-mode** assign from
the moved-out `Error` payload (`enum_field_load_move` + `MoveZero` of `val_local`) — so it genuinely OWNS
the heap String. But the lowering never tags it `Owned`; it stays `Untracked`. When the recovery returns
that bare binding into the Owned `result_local`, the validator (correctly) sees an untracked owned source.

This is exactly why `catch_into_noncopy_dest.gg` did NOT catch it: there the recovery is `V.B(msg)`
(a fresh `EnumInit` producing a `FreshOwned` temp), so the source into `result_local` was a fresh owned
value, never the bare error binding. `throws_method_catch.gg` dodges it with an `int` payload (Copy mode,
not drop-tracked). The resource-payload + bare-binding-recovery combination is the uncovered shape.

**Untracked-source site (the panic):** `src/ir/validate.rs:2799` (`validate_assign_consume`,
`Untracked` arm) — but that is the READ site; per layering discipline the fix is one layer up at the WRITE site.

**Ownership-register WRITE site (the fix):** `src/ir/lowering/exprs/mod.rs:3506-3510`, immediately after
the err-binding's Move-mode `assign_mode`. This mirrors the **Snag #38** `ctx.set_owned(builder, result_local)`
already at `:3536` and the **Snag #31** `assign_match_arm_to_result` tag — the same "the Move-mode assign IS
the ownership commitment; the typed tag must follow" pattern. Core invariant #3: register ownership at the
value's birth.

## 3. Recommended fix (PROTOTYPED + measured)

After the err-binding's `assign_mode`, tag it `Owned` when the assign was Move-mode (a resource payload).
Guarded on `Move` so a primitive (Copy-mode) error payload stays `Untracked`, which is correct — primitives
aren't drop-tracked, and the existing int-payload `throws_method_catch.gg` must stay green.

```diff
--- a/src/ir/lowering/exprs/mod.rs
+++ b/src/ir/lowering/exprs/mod.rs
@@ lower_catch_expr, error path
     let err_op = FunctionBuilder::copy(err_val);
     let err_mode = mode_for(ctx, builder, &err_op, err_field_type);
     builder.assign_mode(err_mode, Place::local(err_local), err_op);
+    // Tier 2a (Core invariant #3): the error binding is born OWNING the
+    // moved-out `Error` payload — `enum_field_load_move` + `MoveZero` of
+    // `val_local` transferred ownership of the heap data into `err_val`,
+    // which the Move-mode assign forwards into `err_local`. Tag the typed
+    // ownership at this writer site so a recovery expression that returns
+    // the bare binding (`… catch (e): e`) flows a tracked Owned source into
+    // `result_local`, instead of an Untracked one that trips Tier 2a's
+    // `AssignIntoOwnedSlot` validator. Mirrors the Snag #38 `set_owned` of
+    // `result_local` below and the Ok-payload move-out shape above. Guarded
+    // on Move mode: a primitive (Copy-mode) error payload is not
+    // drop-tracked, so leaving it Untracked is correct.
+    if matches!(err_mode, crate::ir::instructions::AssignMode::Move) {
+        ctx.set_owned(builder, err_local);
+    }
     ctx.register_local(&error_binding.node, err_local, err_field_type);
```

This is a **1-real-line** change (a guarded `ctx.set_owned`), at the write site, no read-side patch.

### Why it does not double-register or leak on the error path

- The error path always binds `e` and runs the handler; the err binding being `Owned` is correct — it
  owns the moved-out payload and the existing scope-exit drop accounting handles it (no double-drop:
  the base `val_local` was already `MoveZero`'d when the payload moved out, so only the binding/`result_local`
  chain holds the live copy). Verified ASan-clean below.
- When the recovery returns the bare binding (`catch (e): e`), the binding's value moves into `result_local`
  (Move-mode assign), so it isn't dropped twice. When the recovery is a fresh value (`catch (e): "x"`), the
  binding is dropped at error-path scope exit (now correctly, because it's tracked) — also ASan-clean.
- Success path is untouched (the Ok branch never creates the err binding).

### Measured results

- `cargo build` ✓ (1-line change).
- `cargo test --lib` → **1084 passed, 0 failed.**
- `cargo test --test integration -- --test-threads=4 catch` → **26 passed, 0 failed** (incl. `catch_into_noncopy_dest`, `throws_method_catch`, all `fault_catch_*`).
- `cargo test --test integration -- --test-threads=4 throws rethrow error snag4 main_throws` → **152 passed, 0 failed.**
- **ASan + UBSan** (`gg build --sanitize`), free-fn + equip-method, both success+error paths, with resource-error-binding recovery AND fresh-literal recovery: clean, correct output.
- **Backend parity:** identical output on the default C backend AND `--backend=llvm` (fix is in shared GIR lowering).

## 4. Fixture to add — `tests/fixtures/throws_catch_resource_payload.gg`

Covers BOTH free-fn AND equip-method forms, success + error paths, with resource recovery returning the
bare error binding AND a fresh literal:

```
# (header comment: the bug + the Core-#3 write-site fix; see this scout doc)
String f(int x) throws String:
    if x < 0:
        throw "f-negative"
    return "f-ok"

struct Label:
    String text

equip Label:
    String describe(self, int x) throws String:
        if x < 0:
            throw "m-negative"
        return self.text

void main():
    String a = f(5) catch (e): e            # free fn, success
    print(a)
    String b = f(-1) catch (e): e           # free fn, error -> bare resource binding recovery
    print(b)
    String c = f(-1) catch (e): "f-fallback" # free fn, error -> fresh literal recovery
    print(c)
    Label lbl = Label("m-ok")
    String d = lbl.describe(7) catch (e): e  # equip method, success
    print(d)
    String g = lbl.describe(-1) catch (e): e # equip method, error -> bare resource binding recovery
    print(g)
    String h = lbl.describe(-1) catch (e): "m-fallback" # equip method, error -> fresh literal recovery
    print(h)
```

**Expected stdout** (verified, ASan-clean, both backends):
```
f-ok
f-negative
f-fallback
m-ok
m-negative
m-fallback
```

Integration registration (mirrors `throws_method_catch`):
```rust
#[test]
fn throws_catch_resource_payload() {
    // …header explaining the err-binding-Owned write-site fix…
    run_gg("throws_catch_resource_payload.gg",
        "f-ok\nf-negative\nf-fallback\nm-ok\nm-negative\nm-fallback");
}
```
Also update the `NOTE` comments in `tests/fixtures/throws_method_catch.gg` and `tests/integration.rs:4121`
that say the resource-payload shape "trips a separate, pre-existing Tier-2a validator panic … filed in
TODO" — that gap is now FIXED by this fixture; reword to cite it.

## 5. Blast radius + gate battery for the executor

**Blast radius (small):** one guarded `ctx.set_owned` in `lower_catch_expr`. Only fires on the error
path, only for Move-mode (resource) error payloads. Cannot affect Copy-mode (int/primitive) error
bindings → `throws_method_catch.gg` and all fault-catch fixtures are unaffected (confirmed green). The
err binding *should* be Owned (it owns the moved-out payload), so this is strictly correcting a missing
typed tag, not changing semantics.

**Gate battery:**
1. `cargo build`
2. `cargo test --lib`
3. `cargo test --test integration -- --test-threads=4 catch throws rethrow error snag4 main_throws` (pipe through `tee /tmp/…-$RANDOM.log`)
4. New fixture `gg build --sanitize` run → ASan/UBSan clean, exact stdout above.
5. Backend parity: `GG_BACKEND=llvm cargo test --test integration --release throws_catch_resource_payload`.
6. Parent's full integration sweep on both backends + `self_host_bootstrap_fixed_point`.

## 6. Two PRE-EXISTING gaps discovered (do NOT fix here; file as follow-ups)

- **(A) Separate unrelated codegen crash — string concat `+` with `to_string()`.** While building the
  first repro (`return "value:" + x.to_string()`), `gg build` panics at
  `src/backend/c_lir/emit_types.rs:783` — `GorgetString ABI received non-Str, non-Ptr value '__v3'
  (type Some(I32))`. It reproduces with NO catch at all (`String g(int x): return "x:" + x.to_string()`),
  so it is wholly orthogonal to throws/catch — a string-concat/`to_string` ABI bug. **File in TODO.md.**
  (The proposed fixture deliberately avoids this idiom by using plain string literals / `self.field`.)

- **(B) Self-host parity (note-only, per mission).** The self-host `lower_catch_expr`
  (`tests/fixtures/self_host_lowerer/lower_match.gg:976`) routes the err binding through
  `add_local_inheriting(…, err_val)` + `op_consume(… CkAssign())`, which inherits ownership from the
  moved-out payload — so it may NOT have the gap. AND the self-host driver does not run a Tier-2a
  consume-site validator pass, so even if the tag were missing it would not panic there. **This must be
  confirmed by RUNNING (compile the self-host-emitted C and diff vs Rust on this shape), not source-read.**
  File a parallel TODO follow-up to verify self-host runtime parity on the resource-catch shape once the
  Rust fix lands.
