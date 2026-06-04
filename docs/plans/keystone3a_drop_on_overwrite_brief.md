# ③(a) drop-on-overwrite — executor brief (self-host `lower.gg` SAssign)

> Keystone-③ continuation. Oracle: `src/ir/lowering/stmts/assigns.rs:197-231`.
> Foundations: `docs/plans/keystone3_drop_model_foundations.md`.
> All premises below are RUN-verified end-to-end (compile self-host C + run + diff vs `gg run`) at gorget-1 tip `cd8f224d`. Re-verify against CURRENT source before editing.

## The gap (RUN-verified)
`SAssign` → `EIdentifier(name)` in the self-host lowerer (`tests/fixtures/self_host_lowerer/lower.gg`, the `nl_contains(&ctx, name)` arm, currently **lines 7430-7472**) emits `GIAssign(lid, op_consume(...))` (or `GIDerefStore` for `&`-params) with **NO drop of the prior value**. The overwritten owned value leaks (or, for the missing-drop class, the user-`Drop` side effect is skipped).

Proof (`drop_reassign.gg`): a `Tracked` struct (String field) with a user `Drop` that prints `drop {label}`.
```
Tracked t = Tracked("first")     # t owns A("first")
t = Tracked("second")            # A should drop here -> "drop first"; t now owns B("second")
String alive = t.label
print(f"alive: {alive}")         # scope exit drops B -> "drop second"
```
- Oracle: `drop first` / `alive: second` / `drop second`.
- Self-host (current): `alive: second` / `drop second`. **Missing `drop first`** — the prior-value drop at the reassign. (The scope-exit drop of the NEW value already works.)

In the emitted C, the reassign is `__v7 = __s5; __s3 = __v7;` (B copied into t's slot `__s3`) with **no `Tracked__drop(&__s3)` for the old A** between them. We must insert that drop.

## Oracle (Rust `assigns.rs:197-231`, the relevant core)
After computing the RHS operand and BEFORE assigning:
1. `needs_drop = type_registry.needs_drop(type_id)` — drop the old value only if droppable.
2. Self-referential-view guard (lines 178-196): if the RHS may be a VIEW into the old LHS (`s = s.trim()` / `s = s[a..b]`), `clone_to_owned` the RHS into a fresh owned local FIRST (so the subsequent drop doesn't invalidate it).
3. Drop the old value: `if drops.is_moved(local_id) { drop_if_alive } else { drop }`.
4. Then assign (Move/Copy).
5. The CoW alias machinery at the TOP of `lower_assign` (lines 60-99) handles the case where `lid` is a borrow/alias — those paths do NOT reach the owned-drop (a borrow doesn't own the old value). The self-host has no separate CoW-sever pass here, so we replicate the effect with an OWNERSHIP GATE (below).

## The fix (self-host) — precise spec

Edit ONLY the `EIdentifier(name)` → `nl_contains` arm, in the PLAIN-LOCAL case (`asn_inner_ty < 0`). Do NOT touch:
- the `&`/`!`-param deref-store case (`asn_inner_ty >= 0`) — Rust writes through the pointer and does NOT drop the old pointee (the caller owns it; `assigns.rs:232-244`),
- the `static_contains` module-static-write branch (statics live for the program; out of scope),
- the `EFieldAccess` / `EIndex` arms (field-write / index-assign have their own drop-on-overwrite story — log as follow-up, do not implement here).

Replace the current tail of the `asn_inner_ty < 0` plain-local path:
```gorget
        else:
            emit(&ctx, GIAssign(lid, op_consume(&ctx, &gmod, val, CkAssign())))
```
with logic that drops the prior value first. Concrete shape (adapt names to match surrounding style; `asn_lhs_full_ty` and `lid` are already in scope):

```gorget
        else:
            Operand rhs_op = op_consume(&ctx, &gmod, val, CkAssign())
            # ③(a) drop-on-overwrite (mirror Rust gg assigns.rs:197-231):
            # drop `lid`'s PRIOR value AFTER computing the RHS, BEFORE the
            # assign overwrites the slot. Gate on (1) the slot type being
            # droppable AND (2) `lid` currently OWNING its value. A
            # LoBorrowed/LoView alias does NOT own the old value — dropping
            # it would free another local's heap (protects
            # cow_param_alias_reassign: `alias = alias + " world"` where
            # `alias` is a CoW borrow of a param). Same ownership gate as
            # register_local_for_drop's a-1 rule.
            bool drop_old = false
            if is_droppable_type(asn_lhs_full_ty, &gmod):
                match ctx.locals.get(lid).unwrap().ownership:
                    case LoBorrowed():
                        pass
                    case LoView():
                        pass
                    else:
                        drop_old = true
            # Self-assign guard: `s = s` lowers to rhs source == lid;
            # dropping then re-moving the same slot is a UAF. Skip the drop
            # (degenerate no-op). Extract the rhs source local id.
            int rhs_src = -1
            match rhs_op:
                case OpMove(s0):
                    rhs_src = s0
                case OpClone(s0):
                    rhs_src = s0
                case OpCopy(s0):
                    rhs_src = s0
                case OpBorrow(s0):
                    rhs_src = s0
                else:
                    pass
            if rhs_src == lid:
                drop_old = false
            if drop_old:
                # Self-referential VIEW guard (mirror assigns.rs:178-196):
                # when rhs_op is an OpClone of a borrow/view, the source may
                # alias `lid`'s buffer (`s = s.trim()` / `s = s.slice(..)`).
                # Materialise the clone into a fresh OWNED temp FIRST, so the
                # clone reads the OLD buffer while it is still alive; only
                # THEN free the old value and Move the temp in. (OpMove/OpCopy
                # sources are independent of lid — concat/ctor results, owned
                # locals — and the self-assign guard above already excluded
                # rhs_src == lid, so they need no pre-materialisation.)
                match rhs_op:
                    case OpClone(_):
                        int mat = add_local(&ctx, asn_lhs_full_ty, NO_NAME)
                        emit(&ctx, GIAssign(mat, rhs_op))
                        rhs_op = OpMove(mat)
                    else:
                        pass
                # GIDropIfAlive (memcmp-gated), NOT GIDrop: a slot already
                # moved-out earlier in this fn (drop_reassign_after_move:
                # `tokens.push(current)` zeroes current's slot via the
                # post-pass GIMoveZero, then `current = ""`) is then a
                # runtime no-op. Mirrors Rust's is_moved -> drop_if_alive.
                emit(&ctx, GIDropIfAlive(lid))
            emit(&ctx, GIAssign(lid, rhs_op))
```

### Why this is correct (each gate RUN-traced)
- **drop_reassign** (`t = Tracked("second")`): `t` is LoOwned, `Tracked` droppable, rhs_op = OpMove(fresh ctor temp), rhs_src != lid → `GIDropIfAlive(t)` fires (slot live) → `Tracked__drop` → **`drop first`**. ✓ Target flips to MATCH.
- **string_reassign_loop** (`result = result + "\n" + next` in a while loop, then `return result`): `result` is bound from `lines.get(0).unwrap()` (an `Option[Ref]` getter → the local may stay **LoBorrowed**), so the ownership gate may SKIP the prior-value drop entirely — and even when it drops, the concat rhs is a fresh independent buffer. Either way: no double-free, the final `result` is RETURNED (SReturn move-zeroes + excludes it from scope drops). Currently MATCHes; **RUN-verified stays MATCH** under the fix. ✓ (The per-iteration mechanism is gate-skip, not drop — the conclusion holds regardless.)
- **cow_param_alias_reassign** (`alias = alias + " world"`, `alias` is LoBorrowed CoW of param `s`): ownership gate → LoBorrowed → `drop_old=false`. No drop emitted; behaviour unchanged. ✓ (Critical safety case.)
- **lifetime_reassign** (`s = "world"` after `s = "hello"`): `s` is LoOwned + droppable; rhs is a fresh string-literal temp. Drop old "hello" — but a string LITERAL is `cap==0` and `gorget_string_free` no-ops on `cap==0` (`if (s->cap == 0) { *s = (Str){0}; return; }`), so the drop is a safe no-op. Output `world` unchanged. ✓
- **drop_reassign_after_move** (`tokens.push(current); current = ""`): the push's post-pass `GIMoveZero(current)` zeroes the slot; the reassign's `GIDropIfAlive(current)` memcmp-gate sees zeros → no-op (no double-free). ✓ (NOTE: this fixture is ALSO blocked by a SEPARATE pre-existing gap — for-over-String-chars doesn't iterate, logged in TODO — so it will NOT flip to MATCH from ③(a) alone. Verify the reassign-after-move SEMANTICS with a `/tmp`-style explicit-statement repro, not this fixture.)
- **self-ref view** (`s = s.trim()`/`.slice()`/`.substring()` — string_algorithms, string_loops_complex, leak_reassign): rhs_op = OpClone(view) → materialise into `mat` BEFORE the drop → no UAF. These fixtures already CC-FAIL/WRONG for unrelated reasons (NOT in the 407), so they won't regress the parity number — but the materialise-first is MANDATORY because the self-host DRIVER itself uses string-view reassigns; a UAF there breaks `bootstrap_fixed_point` (the load-bearing canary).

### Interaction with the post-pass (verified)
`wire_liveness_into_modes` (`lower.gg:2587`) walks every inst, RE-DECIDES OpMove/OpClone operands by liveness+ownership, calls `mark_local_moved`, and inserts `GIMoveZero(src)` after each finalised OpMove — preserving instruction ORDER. So:
- the RHS move-source is auto-zeroed (no explicit GIMoveZero needed here — unlike SReturn, which needs it for its own early-exit-drop exclusion);
- `GIAssign(mat, OpClone(val))` stays OpClone (val is LoView/LoBorrowed) → real clone into `mat`;
- `GIAssign(lid, OpMove(mat))` stays OpMove (`mat` is LoOwned via `add_local`'s default + last-use) → `mat` zeroed after; harmless (fresh temp).
Do NOT add explicit GIMoveZero for the reassign source — that would double-zero.

## What NOT to do (carry the foundations lessons)
- Do NOT use a raw `GIDrop` (unconditional) — the moved-out slot would double-free. Use `GIDropIfAlive`.
- Do NOT register `mat` for drop — it is moved into `lid`, which keeps its own scope-exit drop.
- Do NOT change `lid`'s ownership tag after the reassign (a LoBorrowed alias that gets reassigned to an owned value: leaving it LoBorrowed means the new value leaks — UNCHANGED from today's behaviour, and OUT of ③(a) scope; the leak is invisible to the target fixtures). Adding ownership-promotion here risks a double-free and belongs to a later phase.
- Do NOT touch the `&`-param / static / field / index arms.

## Files
- EDIT: `tests/fixtures/self_host_lowerer/lower.gg` (the one SAssign EIdentifier plain-local arm only).
- The driver is force-rebuilt by the gate (`rm tests/fixtures/self_host_lowerer/driver{,.c}`). No other self-host dir is involved (the lowerer's parser/ast are symlinked to the typechecker; this change is in lower.gg only — confirm no other dir has an independent lower.gg copy that needs the same edit: `ls tests/fixtures/self_host_*/lower.gg`).

## Add a regression fixture + snapshot
- `drop_reassign` becomes the canonical ③(a) win. After the fix, RUN it and confirm `drop first`/`alive: second`/`drop second`. If it MATCHes, ADD its snapshot to the lock-in net: write `tests/fixtures/runtime_snapshots/drop_reassign.out` with the oracle's exact stdout — confirmed content (trailing newline, matching `lifetime_reassign.out`'s format):
  ```
  drop first
  alive: second
  drop second
  ```
  (so `self_host_runtime` keeps it green; the snapshots ARE the committed oracle stdout). Verify the EXACT bytes against `cargo run -q -- run tests/fixtures/drop_reassign.gg` before writing.

## Gates (in order; STOP and report on any red)
1. Force-rebuild driver: `rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`
2. `bootstrap_fixed_point` — THE load-bearing canary (double-frees on a wrong drop model). MUST stay GREEN:
   `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture 2>&1 | tee /tmp/fp-$RANDOM.log`
3. Parity (must be >= 407, target 408+):
   `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture 2>&1 | tee /tmp/rd-$RANDOM.log` — read the `PARITY = MATCH/...` line.
4. `self_host_runtime` (default-running lock-in net, includes the new snapshot):
   `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime -- --nocapture`
5. `lowerer_comparison` (>= 971) + `c_emit_comparison` (>= 902): `cargo test --test integration --release lowerer_comparison c_emit_comparison -- --nocapture` (read the matched counts).
6. `cargo build` + `cargo test --lib` (1072/0 debug).
7. ALSO RUN under ASan a self-referential-view repro (`s = s.trim()` in a loop) to confirm no UAF — the executor writes a small `/tmp` repro and compiles with `-fsanitize=address`.
(The PARENT runs the full `cargo test --test integration` at integration time — the executor runs the targeted gates above only.)

## Worktree discipline (NON-NEGOTIABLE)
- Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree, NOT `/workspace/gorget-1`.
- `git merge --ff-only gorget-1` FIRST to fast-forward to the current tip.
- Stage ONLY by name: `git add tests/fixtures/self_host_lowerer/lower.gg tests/fixtures/runtime_snapshots/drop_reassign.out docs/plans/keystone3a_drop_on_overwrite_brief.md` — NEVER `git add -a`/`.`/`-am`.
- Commit on your branch; do NOT merge to gorget-1 (the parent integrates after a fresh output review).
