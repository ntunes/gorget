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

## 3. THE FIX — re-key the liveness move-state by DefId
Change the move-state axis from name-keyed to DefId-keyed (`SafetyState`):
- `moved`: `Dict[String, int]` (name→span) → `Dict[int, int]` (**DefId→span**).
- `loop_locals`: `Dict[String, int]` → `Dict[int, int]` (DefId set).
- `rebind`: `String` (name; "" = none) → `int` (**DefId**; use a sentinel like `-1` for none).
Update every keying site to compute the DefId instead of the name:
- **Place uses/moves** (`live_check_use`, `live_mark_move`, `live_move_operand`, the ECall/EMethodCall
  arg loop, EIdentifier/ESelfExpr): use `place_root_def_spanned` (typecheck.gg:712, already returns
  `Option[int]` DefId) for the key. A place with no trackable root DefId is simply untracked (as today).
- **Binding sites that seed re-init / loop-local** (`SVarDecl` name, `SAssign` ident target, for-loop
  `_pat`, match-arm patterns via `collect_pattern_names_into`): these currently work with NAMES; resolve
  each binding to its **DefId** in the current scope (`scopes` + `scope_id` — mirror how
  `place_root_def_spanned` / the resolver map yields a DefId for a bound name). Seed `loop_locals` /
  call `live_reinit` by DefId. (If a binding name can't be resolved to a DefId here, prefer leaving it
  untracked over guessing — untracked = no false positive.)
- **`self` sentinel:** the self-host does NOT resolve `self` to a DefId (it is name-keyed `"self"`:
  typecheck.gg:1171 `root == "self"`, :1552 `live_check_use("self", ...)`). Pick a **reserved sentinel key**
  (e.g. a named constant `SELF_KEY = -2`, distinct from the rebind `-1` none-sentinel and from any real
  DefId) and key all `self` uses/`!self`-consumes/receiver-moves by it. Document the sentinel. (Do NOT
  attempt to wire self→DefId in the self-host resolver in THIS fix — that's a separate change; the sentinel
  is the minimal, correct move here and keeps `self` distinguishable from every named binding.)
- Keep the move/double-move/UAM/re-init/branch-merge/loop logic IDENTICAL — only the KEY TYPE changes.

**Place-overlap (B2) axis:** `check_call_aliasing` / `ArgPlace.root: String` is a SEPARATE per-call axis
(args within ONE call are same-scope, so same-name ⇒ same binding — no cross-scope collision). Leave it
NAME-keyed UNLESS your verification finds a real collision there; if you leave it, add a one-line comment
noting the move-state is DefId-keyed while place-overlap stays name-keyed and WHY (within-call, no
cross-scope collision). Do not silently diverge.

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
