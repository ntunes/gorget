# Executor Brief — Liveness FP fix: re-key the move-state by DefId

**Track:** Wave-Liveness FP fix (owner-ruled 2026-07-15: fix-forward + **re-key move-state by DefId now**).
**Base:** main (the liveness landing `2928d9cb` + folds `8165157c` are ON main — you fix forward on top).
**Deliverable:** eliminate the 23-fixture false-positive regression by re-keying the liveness move-state
from ROOT NAME (`String`) to **DefId (`int`)**, so two distinct same-named bindings no longer collide.
Preserve ALL correct detection (MoveInLoop, double-move, use-after-move, re-init, branch-merge, `!self`
/ConsumeCallable). Full-sweep-gated INCLUDING the corpus tests the original gates missed.

## 0. WORKTREE PREAMBLE (non-negotiable)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm both are inside your worktree. NEVER touch
`/workspace/gorget` (main) or `/workspace/gorget-1`. Do NOT `cd` into either. Worktree-RELATIVE paths only
(an absolute `/workspace/gorget/...` path writes into MAIN). On an Edit desync, re-Read + retry the Edit
tool — never a shell heredoc with an absolute path. After any non-Edit write, `git -C /workspace/gorget
status` and STOP if it shows changes. Stage by explicit file name (NEVER `git add -a`/`-A`/`commit -a`).
NEVER `git stash` — save state with `git diff > /tmp/fpfix_<name>.patch`. Checkpoint to /tmp EARLY.
**Run your FINAL gates FOREGROUND** with generous timeouts (a backgrounded final run stalls the handoff).
**CONTAMINATION:** build the self-host driver, then IMMEDIATELY `cp` it to a PRIVATE `/tmp/fpfix_$$/`
path and run every probe against THAT copy.

## 1. THE BUG (confirmed, root-caused — do NOT re-litigate, but DO verify step 2 before coding)
The self-host's new liveness pass over-rejects 23 legal iterator/stdlib fixtures with
`` `x` moved more than once (double move) `` — production **accepts** all of them (verified:
`gg check tests/fixtures/iterator.gg` → "OK: no semantic errors"). Minimal repro (durable copy
`docs/plans/define-gorget/scouts/patches/liveness-fp-repro-b1.gg`):
```
struct Counter: int start; int max
struct CounterIter: int current; int max
equip CounterIter with Iterator[int]:
    Option[int] next(&self):
        if self.current >= self.max: return None
        int val = self.current; self.current = self.current + 1; return Some(val)
equip Counter with Iterable[int]:
    CounterIter iter(&self): return CounterIter(self.start, self.max)
void main():
    CounterIter c2 = Counter(10, 13).iter()
    print(f"{c2.next().unwrap()}")      # <-- double-moves `x` (NO user `x` in source)
```
The `x` is a **synthetic binding** (trait-default / `unwrap`-desugar / iterator-protocol temp) — there is
no user `x`. The move-state `SafetyState.moved` is `Dict[String, int]` keyed by `place_root_name` (the
root NAME), so two distinct synthetic bindings both named `x` **collide** → false double-move. Production
keys by DefId (distinct bindings never collide). **Name-keying conflating same-named distinct bindings is
the class** — the owner's ruling is to fix the class by DefId-keying, not patch the one instance.
NOT the bug (verified — do NOT change): the for-loop-var direct move (`for x: sink(!x)` non-Copy) IS a
correct `MoveInLoop` in BOTH production and the self-host; leave that behavior intact.

## 2. VERIFY-FIRST (mandatory — de-risks the owner's DefId decision)
Before re-keying, INSTRUMENT to confirm DefId-keying will actually separate the colliding `x`:
- Add a temporary debug print in `live_mark_move` (typecheck.gg:1167) logging, on each move: the enclosing
  function, `place_root_name`, and `place_root_def_spanned` (the DefId). Rebuild the driver, run `b1.gg`.
- Confirm the TWO moves of `x` that trigger the double-move have **DISTINCT DefIds** (or one has a DefId
  and the other resolves to a different binding). If they do → DefId-keying separates them (proceed).
- **If the two "moves" share the SAME DefId** (a single binding genuinely moved twice), DefId-keying will
  NOT fix it — the double-move would be in the DESUGAR itself. STOP and REPORT this immediately with the
  instrumented evidence; do not force the re-key. (Also note WHAT the synthetic `x` is — trait-default
  method? unwrap desugar? — for the report.)
Remove the debug print before finalizing.

## 3. THE FIX — re-key ONLY the `moved` double-move set by DefId (mixed keying)
**SCOPE (tightened after brief review — read the WHY, it is load-bearing):** re-key ONLY `moved` by
DefId. **Keep `loop_locals` AND `rebind` NAME-keyed, unchanged.** The FP lives entirely in the `moved`
double-move set (`typecheck.gg:1181`); `loop_locals`/`rebind` only feed the MoveInLoop check, never the
double-move, so leaving them name-keyed cannot reintroduce the FP. Re-keying `loop_locals` by DefId is
NOT executable and would introduce a NEW bug: binding-INTRODUCTION sites are not DefId-resolvable at the
safety walk — a regular `SVarDecl` is emitted with `name_span = -1` (`parser.gg:3610`) so resolve stores
NO resolution_map entry for it (gated `name_span >= 0`, `resolve.gg:463`); pattern bindings use `Span(0,0)`
with no entry (`resolve.gg:1100-1101`); and `check_safety_stmts` threads the function `body_scope`
(`typecheck.gg:1825`) and never descends into child scopes, so `lookup_from_scope` walks UPWARD
(`scope.gg:311-318`) — it finds outer bindings (which must NOT be loop-local) and misses inner ones
(which MUST be seeded). "Leave untracked" is UNSAFE for `loop_locals`: untracked ⇒ not exempt ⇒ a legal
in-loop move (`loop_local_move_accept.gg`) becomes a MoveInLoop false positive. So do NOT touch that axis.

Concretely:
- `SafetyState.moved`: `Dict[String, int]` (name→span) → **`Dict[int, int]` (DefId→span)**.
- `SafetyState.loop_locals`: **stays `Dict[String, int]`, name-keyed, untouched.**
- `SafetyState.rebind`: **stays `String` (name; "" = none), untouched.**
- **`moved` keying sites — all reliably DefId-resolvable** (an identifier USE stores
  `resolution_map[expr.span.start]=def_id` at `resolve.gg:672-674`, read by `place_root_def_spanned`
  :712): `live_check_use` (called with `expr.span` from the `EIdentifier` arm :1548-1550),
  `live_mark_move`, `live_move_operand`, the ECall/EMethodCall arg loop, and the `SAssign`-ident re-init
  target (`target.expr` is an EIdentifier with a span). Compute the DefId via `place_root_def_spanned`;
  a place with no trackable DefId is untracked (as today — safe for `moved`, it only under-detects a
  double-move, never over-rejects).
- **`self` on `moved`:** the self-host does NOT resolve `self` to a DefId (name-keyed `"self"`:
  :1171/:1552). Use a reserved sentinel key **`SELF_KEY = -2`** on `moved` (CONFIRMED SAFE by review: real
  DefIds are monotonic `>= 0` (`scope.gg:215/254/265`); `NO_DEF/NO_SCOPE = -1`; so `-2` collides with no
  real DefId). Key all `self` uses / `!self`-consumes / receiver-moves on `moved` by `-2`. Do NOT wire
  self→DefId in the resolver (separate change).
- **`live_mark_move` uses MIXED keying** (it has BOTH the `root` name AND the DefId in hand): the
  double-move check is `moved.contains(def_id)`; the MoveInLoop check stays
  `state.loop_depth > 0 and not state.loop_locals.contains(root) and root != state.rebind` (name-keyed).
- **Re-init:** keep `live_reinit` on the `SAssign`-ident target keyed by the target's **DefId** (needed —
  `reinit_accept.gg` `x = fresh()` after `sink(!x)` reuses the SAME binding/DefId → must clear
  `moved[def_id]`). The `SVarDecl` re-init becomes a correct NO-OP under DefId keying (a fresh binding gets
  a fresh DefId never in `moved`, and a shadow's later uses resolve to the new DefId) — leave the call in
  (harmless) or drop it; either is fine, note which.
- Keep the move/double-move/UAM/branch-merge (`safety_branch`/`safety_commit`/`safety_loop_body` iterate
  `moved.keys()`, key-type-agnostic) logic IDENTICAL — only `moved`'s KEY TYPE changes.

**Do NOT re-key `loop_locals`, `rebind`, the `SVarDecl`/for-`_pat`/pattern SEED sites, or the for-loop
`_pat`** (the `SFor` arm intentionally ignores `_pat` — moving the loop var IS a correct MoveInLoop; there
is nothing to re-key there). **Place-overlap (B2)** / `ArgPlace.root: String` stays NAME-keyed (per-call,
same-scope args ⇒ no cross-scope collision). Add a one-line comment at `SafetyState` noting `moved` is
DefId-keyed while `loop_locals`/`rebind`/place-overlap stay name-keyed, and WHY.

## 4. VERIFY (your own gate — the corpus run the original gates missed)
Build the driver to a PRIVATE path, then FOREGROUND:
1. **The repro:** `driver b1.gg lib --lir-c` → exit 0, NO "double move" (blast contribution 0).
2. **All 23 regressed fixtures** compile clean via the driver (list: `closure_tuple_destructure,
   collect_target_positions, dict_drain_basic, iter_chain_past_one_step, iter_chain_terminal_count,
   iter_collect_dict, iter_collect_set, iter_enumerate_zip, iter_fold_inference, iter_lazy_adapters,
   iter_map_filter_method_sugar, iter_map_inference, iter_method_sugar, iter_predicate_inference,
   iter_terminal_method_sugar, iterator, set_drain_basic, stdlib_iter_adapters, stdlib_iter_collect,
   stdlib_iter_drain, stdlib_iter_map_filter, stdlib_iter_terminals, stdlib_vector_iter`) — run each
   `.gg` (in `tests/fixtures/`) through the driver; NONE may emit a double-move.
3. **Correct detection PRESERVED:** run the liveness reject fixtures (`tests/fixtures/liveness/*_reject.gg`)
   through the driver — they MUST still reject (UAM/DM/MoveInLoop). Run the accept fixtures — still accept.
   Run `for x in ["a","b"]: sink(!x)` (non-Copy) — MUST still `MoveInLoop`-reject (production parity).
4. **Whole-source blast = 0** (`driver self_host_lowerer/driver.gg lib --lir-c 2>err`; `wc -c err` == 0).
5. `cargo build --release` + `cargo test --lib` green.
6. **`self_host_bootstrap_fixed_point`** isolated, FOREGROUND: `GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point --
   --test-threads=1`.
7. **The two corpus tests that CAUGHT this (the whole point):** `GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=600 cargo test --test integration --release c_emit_comparison self_host_runtime
   -- --test-threads=1` — `self_host_runtime` must report **0 regressed**; `c_emit_comparison` self-host
   crashes back to baseline. (These are `#[serial]`, so `--test-threads=1` runs them cleanly.)
The PARENT drives the full C + full LLVM sweeps at `--release` (do not wait on those).

## 5. DO NOT
- Do NOT weaken real detection to make the FP go away (no blanket "skip synthetic nodes", no dropping the
  double-move check). The fix is the KEY TYPE, nothing else.
- Do NOT wire `self`→DefId in the resolver here (sentinel only).
- Do NOT touch production `src/` or `spec/ggdef/`.
- Do NOT reshape any fixture to dodge the FP — the fixtures are legal programs; the PASS is the fix.

## 6. FINAL REPORT
Commit hash + summary; the verify-first evidence (the two `x` moves' DefIds — distinct?); the repro +
23-fixture results (all clean); the preserved-detection results (reject fixtures still reject, MoveInLoop
still fires); blast=0; bootstrap + lib results; **`self_host_runtime` = 0 regressed + `c_emit_comparison`
baseline** (the load-bearing gate); what the synthetic `x` turned out to be; whether place-overlap stayed
name-keyed (+ why); confirm `git -C /workspace/gorget status` CLEAN. Do NOT declare done until your
foreground gates — especially `self_host_runtime` 0-regressed — are green.
