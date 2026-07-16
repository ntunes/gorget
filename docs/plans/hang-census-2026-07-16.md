# Hang/Spin Census — Scout Report (FINAL)

Worktree: /workspace/gorget/.claude/worktrees/agent-a71788ef5b97a6196
Harness under test: cherry-picked b2f0d797 -> a90e803c (capped-drain + process-group-kill).
Parity run (canonical, fresh driver, --release, default per-test timeout):
  MATCH 1156/1230 = 94.0%  (WRONG 11 · CC-FAIL 52 · CRASH 11 · DRIVER-FAIL 0 · RUST-CRASH 1)
Process-group fix VALIDATED IN ANGER: after the harness exited, ZERO orphaned fixture/gg
processes; tmp_root Drop-cleaned. stdlib_iter_set now honestly labeled "runaway output —
killed (>256MiB)" where the pre-fix battery-c log mislabeled it "timed out after 30s".

## THE CENSUS (every timeout/runaway row)
Self-host emitted-binary hangs (CRASH bucket, timeout/runaway label):
  1. stdlib_iter_set    SPIN (runaway, ~86% CPU observed)   — ROOT A (adapter inner-copy)
  2. dict_keys_lazy     SPIN (timeout, ~96% CPU observed)   — ROOT A
  3. dict_values_lazy   SPIN (timeout)                      — ROOT A
  4. async_select       BLOCK (timeout, 0% CPU, 11 threads futex_wait) — ROOT B (select dropped)
Oracle hang (RUST-CRASH bucket):
  5. deadwrite_ok_while_drain — ALREADY ROOT-CAUSED (bare-param CoW write discarded per
     while-iteration). Cited, not re-investigated.

NOT hangs (CRASH bucket, other labels — noted, out of census scope):
  - dataframe_filter_sort / dataframe_groupby / dataframe_tier2_joins
      exit=Some(101) trap[T_UnwrapNone]  (clean panic; self-host dataframe miscompile)
  - snag51_closure_block_tail_value / unwrap_error_combinator_phi_acid /
    unwrap_error_combinator_static / vector_capacity
      exit=None (no stderr) = SIGNAL crash (segfault-class), NOT a hang

Cross-checks: no timeout-class CC-FAIL (the one timeout-NAMED CC-FAIL, test_process_timeout,
is a genuine cc type error). battery-c-30726.log confirms the same 4 self-host rows.
No NEW hangs beyond the known starting population.

================================================================================
## ROOT A — SPIN class (rows 1,2,3): Set/Dict lazy-iterator ADAPTERS copy `inner`
================================================================================
LANE: self-host-emitted binary ONLY. Rust-C oracle correct; Rust-LLVM unaffected (the bug
is in the self-host lowerer's C emission, not shared GIR). ggdef: OUT of subset (stdlib
generic iterators / trait-default adapters are not in the ggdef core) — no ggdef lane row.

DIRECT EVIDENCE (compiled the harness-kept .c, ran line-buffered):
  stdlib_iter_set:
    `for x in s.iter():`          -> 10,20,30,40         CORRECT (direct SetIter loop fine)
    `s.iter().take(2)` (for-loop) -> 10,10               count right, VALUE wrong
    `s.iter().skip(2)` (for-loop) -> 10 forever (47M lines/141MB in 3s)  SPIN
  dict_keys_lazy:  first loop prints 60,3 CORRECT; then `.filter(..).count()` SPINS
  dict_values_lazy: first loop prints 100 CORRECT; then `.filter(..).count()` SPINS

MINIMAL PROBE (the class boundary):
  s.iter().filter((int k): k>=20).count()  [Set]    -> self-host SPIN
  v.iter().take(2).count()                 [Vector] -> self-host OK (2)
  TakeIter[VectorIter] named + for-loop    [Vector] -> self-host OK (10,20)
  => Set/Dict-iterator adapters SPIN; Vector-iterator adapters are FINE.

EMITTED-C SMOKING GUN — the `self.inner.next()` lowering diverges by inner type:
  VECTOR (correct):
    __v22 = &((TakeIter*)__v0)->inner;              // borrow the FIELD place
    __v24 = VectorIter__int64_t__next(__v22);
  SET (buggy):
    __v22 = &((TakeIter*)__v0)->inner;              // field address ...
    __v23 = *(__gg_SetIter__int64_t*)(__v22);       // ... but CLONE-MATERIALIZE it
    __s9  = __v23;
    __v24 = &__s9;                                  // borrow the throwaway TEMP
    __v25 = SetIter__int64_t__next(__v24);          // cursor advance written to temp, discarded
  Rust lane on the SAME Set case computes the copy too but then RE-DERIVES the field addr
  for the call (`__v26 = &self->inner; next(__v26)`) — so its cursor advance sticks.
  Struct diff driving it: VectorIter{ GorgetArray source; }(value/view) vs
  SetIter/DictKeysIter/DictValuesIter{ void* source; }(a Ref field).

MECHANISM: at a `&self` method-call RECEIVER that is a struct-field place, the self-host
lowerer clone-materializes the receiver into a temp and borrows the temp — for iterator
types whose field set makes them "needs-materialize" (the Ref-holding Set/Dict iterators),
NOT for the view-holding VectorIter. CoW mandates borrow-the-place at a receiver position
(zero-cost Ptr alias). The inner iterator's cursor advance is lost, so it re-yields element
0 forever and the adapter/terminal (take/skip/filter/count) spins. `remaining` (a DIRECT
field of the adapter) is written back correctly — only the FIELD-AS-RECEIVER borrow is wrong.

CLASS BOUNDARY: any `place.field.method()` where `method` takes `&self` and mutates through
it AND `field`'s type is treated as needs-materialize by the self-host receiver lowering.
Population today = Set/Dict lazy-iterator adapters (Take/Skip/Filter/Map) + terminals
(count/fold/…). Sibling to (but DISTINCT from) the older iterator-SOURCE-field crash class
(TODO ~line 206: ctor stored &stack_clone). That crash class is FIXED — the DIRECT loop now
works — so this SPIN is the next exposed layer.

RELATION TO EXISTING FILINGS: TODO ~206/207/223 describe the iterator source-field/drop
saga in an EARLIER CRASH state (stdlib_iter_set CRASHED then). Current tree: direct loop
correct, adapters spin — a NEW manifestation not precisely captured by those entries.

PROBES: /tmp/hangprobe_{set_filter_count,vec_take_forloop,adapter_copy}.gg
EMITTED C: /tmp/hang_c_captures/{stdlib_iter_set_diff.c, stdlib_iter_set_RUST.c,
           dict_keys_lazy_diff.c, dict_values_lazy_diff.c}

SUGGESTED OWNING TRACK: self-host CoW/receiver-borrow — make the `&self` receiver of a
method call borrow the field PLACE (not a materialized temp) for Ref-holding aggregate
types. Mirror the Rust lowering (compute-copy-if-needed but call on `&field`). Land with a
regression fixture (`set_filter_count` / restore stdlib_iter_set+dict_*_lazy to MATCH).

================================================================================
## ROOT B — BLOCK (row 4): self-host drops the `select:` statement body -> deadlock
================================================================================
LANE: self-host-emitted binary ONLY. Rust-C oracle runs it (else it'd be RUST-CRASH not
CRASH). ggdef: OUT of subset (async runtime not in core) — no ggdef lane row.

CLASSIFICATION: BLOCK. All 11 threads (async worker pool) state S, wchan futex_wait_queue,
0% CPU, zero output — a true deadlock, not a spin.

ISOLATION PROBES (both Rust-oracle-correct -> 3):
  spawn + channel send/recv/await, NO select  -> self-host OK (3)   [channels/spawn all fine]
  select over ONE channel                      -> self-host DEADLOCK
  => the `select:` statement is the sole culprit.

EMITTED-C EVIDENCE: the self-host `int main` emits NO gorget_channel_poll_recv, NO
__gorget_select_yield call, NO case-variable binding. The select's while-body block `__bb2`
is just `count = count + 1` with `total` untouched — the entire select statement lowered to
an empty body. Rust lane emits the poll loop (`__bb3`: select_yield -> poll_recv ->
ready?case:reloop). Runtime symbol counts: Rust poll_recv×2 / select_yield×2 vs self-host
×0 user calls.

CONSEQUENCE CHAIN (the deadlock): no receiver -> spawned producer fills the cap-1 channel
(send #0) then BLOCKS on send #1 forever -> main's count-loop finishes on garbage -> main
calls `await producer` -> producer never completes -> all threads park in futex_wait.

STRIP SITE: SSelect is parsed (parser.gg:3060), resolved, typechecked, and recursed by
every analysis pass, but has NO LIR-emission arm in the statement dispatcher
(lower_stmt.gg `lower_stmt` :47 — arms for every S-kind EXCEPT SSelect; else -> lower_fail).
Yet the driver exits 0 with no stderr and emits the empty body, so an earlier pass consumes
the select before LIR emission (exact strip-pass is a fix-owner detail; collect_rewrite_stmts
:2891 is NOT it — its else preserves unknowns). NOTE: the TODO's cited `lower.gg:669-670` is
STALE — that line is only the `stmt_kind` naming diagnostic; the real gap is a missing
`case SSelect` LIR-lowering arm.

RELATION TO EXISTING FILINGS: ALREADY FILED — TODO ~line 174 & ~132(c): "async_select ALSO
needs a full SSelect lowering arm (select: is unimplemented ... the whole block is silently
DROPPED — own large sub-track)." This census CONFIRMS it still reproduces and supplies the
emitted-C evidence + the exact deadlock mechanism + the stale-line-number correction.

PROBES: /tmp/hangprobe_{select_one,chan_norecv_select}.gg
EMITTED C: /tmp/hangprobe_select_one_{sh,RUST}.c ; /tmp/hang_c_captures/async_select_diff.c

SUGGESTED OWNING TRACK: implement `SSelect` LIR lowering in the self-host (mirror Rust's
poll-yield loop: for each recv/send arm poll the channel non-blocking; if none ready call
__gorget_select_yield and re-poll). Large sub-track. Land restoring async_select to MATCH.

================================================================================
## RECOMMENDATIONS
================================================================================
(i) NO-NEW-HANGS EXECUTABLE GUARD — fits the existing floor/ratchet idiom
   (RUNTIME_DIFF_MATCH_FLOOR, tests/integration.rs). In self_host_runtime_diff, after the
   CRASH backlog print, collect the set of fixtures whose Crashed.stderr_first contains
   "timed out" or "runaway output", and assert:
       const EXPECTED_HANGS: &[&str] =
           &["async_select","dict_keys_lazy","dict_values_lazy","stdlib_iter_set"];
     - a hang NOT in EXPECTED_HANGS => FAIL loudly ("NEW self-host hang: <fixture>") — the
       no-new-hangs floor.
     - a fixture in EXPECTED_HANGS that no longer hangs => FAIL asking to REMOVE it from the
       list in the same commit (ratchet-down, mirrors "raise the MATCH floor on improvement").
   This is a named allowlist that MUST SHRINK, never grow — the same shape as the MATCH
   floor and the tests/lints.rs name-prefix budget. (It rides the diagnostic test that
   already computes the data; the labels are now honest post-b2f0d797.)

(ii) SHARED ROOTS -> TWO TRACKS:
   - Rows 1,2,3 = ONE class, ONE root (Root A, adapter field-as-receiver copy) = ONE track;
     one fix + the three fixtures flip together (plus the set_filter_count regression fixture).
   - Row 4 (Root B, select lowering) = a SEPARATE, larger track.
   - Row 5 (deadwrite oracle) = already root-caused elsewhere (CoW bare-param-write-in-loop),
     its own track.

(iii) HARNESS BUCKETING SMELL (raise-anything-dumb): the CRASH bucket CONFLATES three very
   different failure modes — genuine HANGS (timeout/runaway), SIGNAL crashes (exit=None,
   segfault-class), and CLEAN PANICS (exit=Some(101) trap[...]). Debugging triage would be
   sharper if RuntimeParityOutcome::Crashed split into Hang{spin|block} / SignalCrash /
   PanicTrap (the label string already distinguishes them; it's just not bucketed). Minor,
   optional — the honest labels already make it greppable.
