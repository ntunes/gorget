#!/usr/bin/env bash
# Run the fixture corpus under ASan+UBSan and diff against the committed
# allowlists in tests/sanitize/.
#
# WHY THIS EXISTS. Measured 2026-08-18: of 2100 top-level fixtures, 13 carried a
# sanitize assertion. 99.4% ran an stdout compare and nothing else — and an
# stdout compare cannot observe a use-after-free. Every memory-safety defect
# found in R42 was found INCIDENTALLY by someone hand-building an ASan probe.
# This converts that into an enumeration (owner ratified 2026-08-18:
# "we should be running everything under a sanitizer").
#
# ---------------------------------------------------------------------------
# HOW IT ADJUDICATES. Four properties, each bought by a measured defect in the
# gate itself. A gate whose verdict is not trustworthy is worse than no gate:
# it manufactures phantom regressions and trains readers to re-run until green.
#
#  1. A DETERMINISTIC ROOT SET. LeakSanitizer walks live thread stacks
#     CONSERVATIVELY, so a stale pointer left in a worker thread's frame makes a
#     genuinely-leaked block read as "still reachable" and the leak is
#     SUPPRESSED. That was this gate's whole source of run-to-run flapping.
#     `use_stacks=0` removes that root. `use_globals` STAYS ON: the scheduler's
#     own global pool is legitimately live at exit and reporting it would add
#     ~78KB of noise in 7 allocations.
#     ⚠ The flag is only valid for a program that EXITED NORMALLY. A program
#     that traps mid-stack legitimately holds its only reference in a live
#     frame, so an abnormal exit is RE-RUN under the DEFAULT root set and its
#     leak verdict is taken from that run. Without that clause the flag
#     false-positives on every trapping fixture. `exitcode=0` is what makes the
#     program's OWN exit status readable (a sanitizer finding no longer
#     overwrites it) — and, as a bonus, it lets a leaking fixture's stdout
#     flush, which the old instrument swallowed.
#
#  2. REPEATED RUNS WITH A UNION VERDICT, plus a FLAKY census. The build is the
#     expensive half, so extra runs of the same binary are near-free. UNION is
#     the only polarity a shrink-only ratchet may have: "re-run until green"
#     must not work. Symmetrically the "no longer leaking" advisory requires
#     CLEAN on ALL reps. A row whose VERDICT SET is not unanimous across reps is
#     FLAKY and fails the gate — flakiness is computed over the whole verdict
#     string, so a fixture that reports ASan on one rep and a leak on the next
#     is caught (a bad-run COUNT would call that unanimous).
#
#  3. A CLASS-KEYED LEAK ALLOWLIST. A row used to be a bare fixture stem, so it
#     tolerated ANY leak in that fixture forever: a second, entirely different
#     leak mechanism appearing inside an already-allowlisted fixture was
#     invisible. Rows now carry the per-class leak-record COUNTS the fixture is
#     tolerated for (`<top-frame>*<records>`), and any class — or any extra
#     record of a tolerated class — fails the gate. Counts, not a set: a set
#     cannot see a SECOND leak of a class the fixture already exhibits.
#     Membership is unchanged by this schema; it only makes the rows honest.
#
#  4. EVERY VERDICT THE CLASSIFIER EMITS IS CONSUMED. It used to emit nine
#     labels and diff two. UBSan findings were computed and thrown away (in a
#     job named "ASan+UBSan"), a `timeout` kill read as CLEAN, and a fixture
#     silently leaving the population — BUILD_FAIL_*, NO_BINARY, SKIP_COPY —
#     was an undetected coverage loss. Each now has a ceiling or a floor, and
#     RUNNER_FAIL was added because a broken runner writes an EMPTY log, which
#     every content-based classifier reads as CLEAN: the whole corpus silently
#     green. Within a run the labels are a SET, not an if/elif chain, so an ASan
#     finding no longer suppresses the leak verdict underneath it.
#
# SELF-TEST. Before it reports anything the sweep runs four control fixtures in
# tests/fixtures/sanitize_selftest/ (a SUBDIRECTORY, so the corpus walk below
# cannot see them) THROUGH THE SAME xargs PIPELINE the corpus uses, and asserts
# its own detectors fire: the leak detector fires,
# the clean path stays quiet, the flake detector fires on an alternating row,
# and the class check fires on a SECOND leak record of an ALREADY-TOLERATED
# class inside an already-allowlisted fixture. A guard that has never been seen
# to fail is not evidence (docs/devbook/25-structural-guards.md:123), so this
# gate refuses to report a corpus verdict until it has watched itself fail.
# `scripts/sanitize_sweep.sh --selftest` runs just that, in seconds.
# ---------------------------------------------------------------------------
#
# COST. A single-run sweep is ~25 min at parallelism 8, DOMINATED BY ~2150
# BUILDS, which is why it is a separate CI job rather than part of the main
# integration run. An extra REPETITION only re-runs the already-built binaries:
# directly measured at 262 s (~4.4 min) for one full pass over the corpus at
# JOBS=8 on a 10-core box under load. So REPS=3 costs about 9 minutes more than
# a single-run sweep — it does not triple it. Regenerate before quoting; do not
# read a per-repetition cost off a whole-sweep wall clock.
#
# Exit 0 = no NEW corruption, no NEW leak (or leak class), no flaky row, no
# dropped verdict. Exit 1 = something regressed. Exit 2 = the INSTRUMENT is
# broken (no compiler, malformed allowlist, self-test failed) — that is not a
# verdict about the tree and must never be read as one.
# Fixtures that fix a listed defect show up as "no longer failing" — that is a
# PASS, and the message tells you to delete the row.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 2

JOBS="${JOBS:-8}"
REPS="${REPS:-3}"
GG="${GG:-target/debug/gg}"
OUT="${OUT:-/tmp/sanitize_sweep_$$}"
# `-` not `:-`: an explicitly EMPTY LSANOPT selects the default root set, which
# is how the paired instrument comparison is run.
LSANOPT="${LSANOPT-use_stacks=0}"
ASANOPT="${ASANOPT:-detect_leaks=1:exitcode=0}"
# Self-test knob: a file of .gg paths to sweep instead of the corpus. For
# demonstrations only — CI sweeps the corpus.
FIXLIST="${FIXLIST:-}"
RUN_SELFTEST="${RUN_SELFTEST:-1}"
SELFTEST_ONLY=0
[ "${1:-}" = "--selftest" ] && SELFTEST_ONLY=1

# Overridable so the gate can be pointed at a stub list for a demonstration or
# a baseline measurement. CI passes neither and gets the committed lists.
CORRUPT_LIST="${CORRUPT_LIST:-tests/sanitize/CORRUPTION_ALLOWLIST.txt}"
LEAK_LIST="${LEAK_LIST:-tests/sanitize/LEAK_ALLOWLIST.txt}"
SELFTEST_DIR=tests/fixtures/sanitize_selftest

# --- ceilings and floors -----------------------------------------------------
# Every one of these is a RATCHET: it may move toward zero (or, for the floor,
# upward) freely; moving it the other way is a deliberate, reviewed admission.
#
# FLAKY and CLASS_UNSTABLE open at 0 and are fatal from day one — strictly
# better than an env-gate to be burned down later, because under this instrument
# the corpus measures ZERO of each and there is nothing to burn down. (Under the
# DEFAULT root set the same corpus measured 2 flaky and 7 class-unstable, which
# is why the row-1 flag and the row-4a class column are one deliverable and not
# two: you cannot pin a key you have not measured stable.)
FLAKY_CEILING="${FLAKY_CEILING:-0}"
CLASS_UNSTABLE_CEILING="${CLASS_UNSTABLE_CEILING:-0}"
# COUNT drift is a weaker finding than CLASS drift: the SET of mechanisms is
# identical run to run, only HOW MANY records of one appear moves — a genuinely
# racy program (how many workers had leaked by exit). Such a row keeps its
# mechanisms pinned and marks the racy class `*N+` ("at least N, count not
# pinned"), because pinning an exact count on a racy row would make THIS gate
# flap, which is the defect it exists to remove.
#
# It is therefore a CENSUS, printed with its rows, and NOT a ceiling. A ratchet
# whose own measured value depends on REPS and on the scheduler is not a gate —
# it is a coin flip with a threshold, and this file is the wrong place to ship
# one. The rows it names are what earn a `*N+` marker in the allowlist.
# UBSan findings were computed and then DISCARDED by the old diff, in a CI job
# named "Sanitize sweep (ASan+UBSan)". Fatal at 0, and the corpus measures ZERO
# — so there is nothing to burn down and no reason this ceiling should ever
# move off 0. The one finding it was shipped red on, `test_vector_sort_methods`
# passing `qsort(NULL, 0, …)` on an empty collection, was fixed at R47 close:
# both emitters now build the call through one `qsort_guarded` producer, and
# `emitted_qsort_is_guarded` in tests/lints.rs is what stops the next arm
# spelling it directly. That defect was `t0780`; see DONE.md.
UBSAN_CEILING="${UBSAN_CEILING:-0}"
# A `timeout 60` kill used to read as CLEAN. AGENTS.md: every hang/spin/timeout
# gets root-caused into a census row, never merely killed.
TIMEOUT_CEILING="${TIMEOUT_CEILING:-0}"
# SKIP_COPY + NO_BINARY + RUNNER_FAIL: the sweep's own plumbing failing.
# Never legitimate, and RUNNER_FAIL in particular would otherwise read CLEAN.
INFRA_CEILING="${INFRA_CEILING:-0}"
# CRASH: the process died on a SIGNAL, or exited off the ratified toolchain
# taxonomy (docs/define-gorget/decisions.md:2074-2070 — a TOTAL enumeration, so
# its complement is exactly the fault domain). This label DID NOT EXIST here
# before: every such run fell into the CLEAN sink, which is how a deterministic
# SIGSEGV was published as a clean run. Fatal at 0 — a crash in this corpus is a
# soundness defect, not debt.
CRASH_CEILING="${CRASH_CEILING:-0}"
# UNKNOWN: the classifier could not positively determine what happened. Also new
# — the old body's answer to this case was CLEAN. An UNKNOWN is the instrument
# telling you it cannot see, which is strictly more useful than a green it has
# not earned.
#
# ⚠ IT IS A CENSUS, NOT A CEILING, AND THE DISTINCTION IS DELIBERATE. Every other
# gate in this file counts a DEFECT IN THE COMPILER; UNKNOWN counts a LIMITATION
# OF THE INSTRUMENT. Two reasons that must not become a ratchet at 0:
#   * the `run_rc1` ambiguity cell (todo/t0647: a program's own `exit(1)` is
#     indistinguishable from `gorget_panic_at`'s) means ANY fixture that
#     legitimately exits 1 lands here — and this corpus has no expectation column
#     to resolve it with, unlike the robustness map, which uses its hand-written
#     "loud failure" expectation for exactly that.
#   * this file's own rule, stated for FLAKY and CLASS_UNSTABLE above: *"you
#     cannot pin a key you have not measured stable."* The corpus-wide UNKNOWN
#     count has never been measured, so pinning it at 0 unmeasured would be the
#     coin-flip-with-a-threshold this file explicitly refuses to ship.
# MEASURED, so it is now a ratchet at the measured value (R47 close, three
# full-corpus sweeps agreeing): exactly ONE fixture, `test_timeout`, and it is
# precisely the cell the second bullet above predicts. It is a test-runner
# program whose `@timeout("100")` "slow test" is MEANT to fail, so the runner
# reports the failure and exits 1 — deterministically, verified by running the
# built binary directly — and `todo/t0647`'s ambiguity means the classifier
# cannot tell that apart from `gorget_panic_at`'s exit 1.
#
# ⚠ WHAT MOVING THIS COSTS, so the next person moves it deliberately. Every
# other ceiling here counts a compiler defect; this one counts fixtures the
# instrument cannot see. Pinning it at 1 means a NEW fixture that legitimately
# exits 1 reds this gate — which is the intended pressure (resolve the
# ambiguity, per t0647, or say why the fixture is exempt), not a bug in the
# ratchet. Raising it is an admission that the blind spot grew.
UNKNOWN_CEILING="${UNKNOWN_CEILING:-1}"
# COVERAGE FLOOR: fixtures that actually produced a RUN verdict. A fixture
# drifting CLEAN -> BUILD_FAIL_* is a silent coverage loss the old gate could
# not see; this is the ratchet that sees it. Raise it when the corpus grows.
COVERAGE_FLOOR="${COVERAGE_FLOOR:-1743}"
# Per-run wall-clock budget for one fixture. Overridable so the TIMEOUT verdict
# can be demonstrated without a 60-second fixture.
RUN_TIMEOUT="${RUN_TIMEOUT:-60}"

# PRE-FLIGHT. A test binary left alive by a DEAD run raises loadavg, and this
# sweep's own `timeout $RUN_TIMEOUT` budget is a wall-clock one -- a poisoned box
# turns real work into spurious TIMEOUTs. Dry run; the predicate is OWNERSHIP,
# never a name (see scripts/reap_orphans.py). GG_SKIP_PREFLIGHT=1 escapes it.
if [ -z "${GG_SKIP_PREFLIGHT:-}" ] && command -v python3 >/dev/null 2>&1; then
  python3 scripts/reap_orphans.py --preflight || {
    echo "refusing to measure a poisoned box (GG_SKIP_PREFLIGHT=1 overrides)"; exit 2; }
fi

[ -x "$GG" ] || { echo "no gg at $GG — run cargo build first"; exit 2; }
[ "$REPS" -ge 2 ] || { echo "REPS must be >= 2: a union verdict and a flakiness census need at least two runs"; exit 2; }
mkdir -p "$OUT/logs" "$OUT/tmp" "$OUT/w"

# --- classification ----------------------------------------------------------
# The label set for ONE run is decided by scripts/verdict.py, the ONE classifier
# every lane uses. It is SHELLED OUT rather than reimplemented here, on the same
# precedent tests/lints.rs already sets for `python3 scripts/todo_index.py`:
# three hand-maintained copies of a marker set and an exit-code table cannot
# catch their own divergence, and two of the three copies in this tree were
# measurably wrong.
#
# ⚠ WHAT THE OLD BODY GOT WRONG, and why this is not a refactor. It ended with
#   `[ -z "$_labels" ] && _labels=CLEAN`
# — a DEFAULT SINK. Any exit code outside {124,125,126,127} with a silent log
# read CLEAN, so a deterministic **rc 139 SIGSEGV published as a clean run**.
# That is R47 failure instance 1, and it lived in this file. verdict.py has no
# sink: CLEAN is a POSITIVE verdict (rc 0, no sanitizer finding, no timeout) and
# anything unclassifiable is UNKNOWN and loud.
#
# What is KEPT, because it was right: the multi-label SET (not an if/elif chain
# — a chain lets the first match shadow every later one, which is how UBSan
# stayed invisible inside all 316 leaking fixtures), the `ASAN_<kind>` spelling
# the allowlists and the ceiling greps key on, the rc triage for 124 and
# 125/126/127, and the CAPTURED sanitizer kind.
#
# The ASan exit-code convention is passed as an INPUT parsed from $ASANOPT, not
# assumed: this file uses `exitcode=0` and every other site uses 99, so a
# classifier that hardcodes either is wrong under the other.
ASAN_EXITCODE=$(printf '%s' "$ASANOPT" | sed -n 's/.*exitcode=\([0-9]*\).*/\1/p')
[ -z "$ASAN_EXITCODE" ] && ASAN_EXITCODE=1
classify_log() {
  _log="$1"; _rc="$2"
  # --subject program: this corpus runs COMPILED FIXTURES, not the compiler.
  # The ratified exit-code taxonomy binds the TOOLCHAIN; a fixture that exits 7
  # on purpose (tests/fixtures/gg_impl_exit7.gg does exactly that) is using the
  # USER's exit API, and grading it a crash red-ed this gate on the first full
  # corpus run.
  python3 scripts/verdict.py --phase run --rc "$_rc" --stderr "$_log" \
      --subject program --sanitizer-exitcode "$ASAN_EXITCODE" --format sweep
}

# The leak CLASS signature of one run: for each leak record, the first stack
# frame that is not an allocator interceptor or the runtime's global alloc
# shim — i.e. the allocation site, which is the "top frame" the allowlist
# header's own big-groups table is written in. Counted by RECORD (one distinct
# allocation stack), which is a structural property; byte and object counts
# move with loop trip counts and data sizes and are deliberately NOT pinned.
leak_classes() {
  awk '
    /^(Direct|Indirect) leak of/ { want=1; next }
    want && /^[ \t]*#[0-9]+ / {
      sym="UNSYMBOLIZED"
      if (match($0, / in [^ ]+/)) sym=substr($0, RSTART+4, RLENGTH-4)
      if (sym ~ /^__interceptor_/) next
      if (sym == "__gorget_global_alloc_fn" || sym == "__gorget_global_realloc_fn" || sym == "__gorget_global_calloc_fn") next
      if (sym == "malloc" || sym == "calloc" || sym == "realloc" || sym == "strdup" || sym == "operator") next
      print sym; want=0
    }
  ' "$1" 2>/dev/null | sort | uniq -c \
    | awk '{printf "%s%s*%s", (NR>1?",":""), $2, $1} END{if (NR==0) printf "-"; printf "\n"}'
}

run_one() {
  f="$1"; stem="$(basename "$f" .gg)"; d="$OUT/w/$stem"; mkdir -p "$d"
  cp "$f" "$d/" 2>/dev/null || { printf '%s\t%s\t%s\t%s\n' "$stem" SKIP_COPY - -; return; }
  if ! "$GG" build --sanitize "$d/$stem.gg" >"$OUT/logs/$stem.build" 2>&1; then
    # A fixture that also fails WITHOUT --sanitize is a pre-existing build issue,
    # not a sanitizer finding. Distinguish them; do not report the wrong thing.
    if "$GG" build "$d/$stem.gg" >/dev/null 2>&1; then
      printf '%s\t%s\t%s\t%s\n' "$stem" BUILD_FAIL_SANITIZE_ONLY - -
    else
      printf '%s\t%s\t%s\t%s\n' "$stem" BUILD_FAIL_BOTH - -
    fi
    return
  fi
  [ -x "$d/$stem" ] || { printf '%s\t%s\t%s\t%s\n' "$stem" NO_BINARY - -; return; }
  : > "$OUT/tmp/$stem.v"; : > "$OUT/tmp/$stem.c"
  i=1
  while [ "$i" -le "$REPS" ]; do
    log="$OUT/logs/$stem.run$i"
    # stderr to a FILE, never a pipe — a pipeline masks the signal and a crash
    # reads as a clean exit (this cost two wrong verdicts in this tree).
    if [ -n "$LSANOPT" ]; then
      ( cd "$d" && ASAN_OPTIONS="$ASANOPT" LSAN_OPTIONS="$LSANOPT" timeout "$RUN_TIMEOUT" "./$stem" >/dev/null 2>"$log" )
      rc=$?
      if [ "$rc" -ne 0 ] && [ "$rc" -ne 124 ]; then
        # ABNORMAL EXIT. A program that trapped mid-stack legitimately holds its
        # only reference in a live frame, so the conservative root set is the
        # right instrument for it. Re-run under the DEFAULT roots and take this
        # rep's verdict from there — coverage preserved exactly, never skipped.
        log="$OUT/logs/$stem.run$i.defaultroots"
        ( cd "$d" && ASAN_OPTIONS="$ASANOPT" timeout "$RUN_TIMEOUT" "./$stem" >/dev/null 2>"$log" )
        rc=$?
      fi
    else
      ( cd "$d" && ASAN_OPTIONS="$ASANOPT" timeout "$RUN_TIMEOUT" "./$stem" >/dev/null 2>"$log" )
      rc=$?
    fi
    classify_log "$log" "$rc" >> "$OUT/tmp/$stem.v"
    leak_classes "$log"       >> "$OUT/tmp/$stem.c"
    i=$((i+1))
  done
  union=$(tr ',' '\n' < "$OUT/tmp/$stem.v" | grep -v '^$' | sort -u | grep -v '^CLEAN$' | paste -sd, -)
  [ -z "$union" ] && union=CLEAN
  flags=""
  [ "$(sort -u "$OUT/tmp/$stem.v" | wc -l)" -gt 1 ] && flags=FLAKY
  # Both computed over the reps that actually LEAKED, and they are DIFFERENT
  # findings, which is why they are two columns — but only ONE of them gates:
  #   CLASS_UNSTABLE — the SET of mechanisms differs run to run. Such a row
  #                    cannot be pinned at all. FATAL, ceiling 0.
  #   COUNT_DRIFT    — same mechanisms, different number of records (a genuinely
  #                    racy program: how many workers leaked before exit). The
  #                    row stays pinnable by marking that class `*N+`, so this is
  #                    a CENSUS and gates nothing: its own measured value moves
  #                    with REPS and with the scheduler, and a ratchet on a
  #                    quantity like that is a coin flip with a threshold. See
  #                    the ceilings block near the top of this file.
  [ "$(grep -v '^-$' "$OUT/tmp/$stem.c" | sed 's/\*[0-9]*//g' | sort -u | wc -l)" -gt 1 ] \
    && flags="$flags${flags:+,}CLASS_UNSTABLE"
  [ "$(grep -v '^-$' "$OUT/tmp/$stem.c" | sort -u | wc -l)" -gt 1 ] \
    && flags="$flags${flags:+,}COUNT_DRIFT"
  [ -z "$flags" ] && flags=-
  classes=$(awk -F, '{for(i=1;i<=NF;i++){split($i,a,"*"); if(a[1]!="" && a[1]!="-" && (a[2]+0)>(m[a[1]]+0)) m[a[1]]=a[2]+0}} END{for(s in m) print s"*"m[s]}' "$OUT/tmp/$stem.c" \
    | sort | paste -sd, -)
  [ -z "$classes" ] && classes=-
  printf '%s\t%s\t%s\t%s\n' "$stem" "$union" "$flags" "$classes"
}
export -f run_one classify_log leak_classes
# ⚠ ONE list, used by the corpus path AND by the self-test's re-export, so the
# two cannot drift. Paired with `bash -uc` at both call sites: a name missing
# from here is now an unset-variable ABORT inside the worker rather than an
# empty string that quietly turns the corpus green.
SWEEP_WORKER_ENV="OUT GG REPS LSANOPT ASANOPT RUN_TIMEOUT ASAN_EXITCODE"
# shellcheck disable=SC2086
export $SWEEP_WORKER_ENV

# --- leak adjudication -------------------------------------------------------
# $1 allowlist  $2 verdicts.tsv  $3 destination dir.
# Writes new_leak, new_class, fixed_leak, shrunk_class (each possibly empty).
adjudicate_leaks() {
  _allow="$1"; _verd="$2"; _dst="$3"; mkdir -p "$_dst"
  : > "$_dst/new_leak"; : > "$_dst/new_class"; : > "$_dst/fixed_leak"; : > "$_dst/shrunk_class"
  awk -F'\t' -v dst="$_dst" '
    FNR==NR {
      if ($0 ~ /^[[:space:]]*#/ || NF == 0 || $1 == "") next
      allow[$1]=1; sig[$1]=$2
      next
    }
    $2 ~ /(^|,)LEAK(,|$)/ { seen[$1]=1; got[$1]=$4 }
    END {
      for (s in seen) {
        if (!(s in allow)) { print s > (dst "/new_leak"); continue }
        n=split(sig[s], A, ",")
        split("", allowed); split("", loose)
        for (i=1;i<=n;i++) {
          split(A[i], kv, "*")
          if (kv[1] == "" || kv[1] == "-") continue
          # A trailing `+` means "at least this many; the COUNT is not pinned".
          # It is for a fixture whose leak count is genuinely racy — how many
          # workers had leaked by exit. Pinning an exact count on such a row
          # would make this gate flap, which is the defect it exists to remove.
          # The MECHANISM is still pinned; only the multiplicity is not, and the
          # count-drift census below says which rows earned the marker.
          if (kv[2] ~ /\+$/) loose[kv[1]]=1
          allowed[kv[1]]=kv[2]+0
        }
        m=split(got[s], B, ",")
        viol=""; shrink=""
        for (i=1;i<=m;i++) {
          split(B[i], kv, "*")
          if (kv[1] == "" || kv[1] == "-") continue
          c=kv[1]; k=kv[2]+0
          if (!(c in allowed))     viol = viol (viol ? "; " : "") c " x" k " (class not tolerated)"
          else if (c in loose)     continue
          else if (k > allowed[c]) viol = viol (viol ? "; " : "") c " x" k " (row tolerates x" allowed[c] ")"
          else if (k < allowed[c]) shrink = shrink (shrink ? "; " : "") c " x" k " (row says x" allowed[c] ")"
        }
        for (c in allowed) {
          found=0
          for (i=1;i<=m;i++) { split(B[i], kv, "*"); if (kv[1] == c) found=1 }
          if (!found) shrink = shrink (shrink ? "; " : "") c " gone (row says x" allowed[c] ")"
        }
        if (viol   != "") print s "\t" viol   > (dst "/new_class")
        if (shrink != "") print s "\t" shrink > (dst "/shrunk_class")
      }
      for (s in allow) if (!(s in seen)) print s > (dst "/fixed_leak")
    }
  ' "$_allow" "$_verd"
  for _f in new_leak new_class fixed_leak shrunk_class; do
    sort -o "$_dst/$_f" "$_dst/$_f"
  done
}

# --- self-test ---------------------------------------------------------------
# Watch every detector fire, in this invocation, before trusting any verdict.
run_selftest() {
  _sout="$OUT/selftest"; mkdir -p "$_sout/logs" "$_sout/tmp" "$_sout/w"
  _saved_out="$OUT"; _saved_reps="$REPS"
  OUT="$_sout"; [ "$REPS" -lt 2 ] && REPS=2
  # shellcheck disable=SC2086  -- the SAME list the corpus path exports, never a
  # second hand-maintained copy: re-exporting only OUT and REPS here is what let
  # this self-test pass while the corpus path was silently missing a name.
  export $SWEEP_WORKER_ENV
  : > "$_sout/verdicts.tsv"
  # Drive the controls through the SAME `xargs bash -c` pipeline the corpus uses,
  # not by calling run_one in this shell. A control that takes a different path
  # from the thing it certifies cannot catch that path's failures — and this is
  # not hypothetical: an unexported RUN_TIMEOUT made `timeout` fail, every log
  # came back empty, and the whole corpus read CLEAN while an in-process
  # self-test passed. Q2, on the self-test itself.
  find "$SELFTEST_DIR" -maxdepth 1 -name '*.gg' | sort \
    | xargs -P 1 -I{} bash -uc 'run_one "$@"' _ {} > "$_sout/verdicts.tsv"
  OUT="$_saved_out"; REPS="$_saved_reps"
  # shellcheck disable=SC2086
  export $SWEEP_WORKER_ENV

  _fail=0
  _get() { awk -F'\t' -v s="$1" -v c="$2" '$1==s{print $c}' "$_sout/verdicts.tsv"; }
  _want() { # name column expected
    _got="$(_get "$1" "$2")"
    if [ "$_got" != "$3" ]; then
      echo "  SELF-TEST FAIL: $1 column $2 = '$_got', expected '$3'"; _fail=1
    fi
  }
  _want selftest_clean            2 CLEAN
  _want selftest_clean            3 -
  _want selftest_leak             2 LEAK
  _want selftest_leak             3 -
  _want selftest_leak_twice       2 LEAK
  _want selftest_leak_twice       3 -
  _want selftest_alternating_leak 2 LEAK
  _want selftest_alternating_leak 3 FLAKY

  # ⚠ EVERY LABEL, FIRED. The four fixtures above demonstrate CLEAN and LEAK —
  # two of the labels this gate can emit — and a guard that has never been seen
  # to fire is not evidence (docs/devbook/25). The rest cannot be produced by a
  # .gg fixture on demand (you cannot ask a fixture to be killed by SIGSEGV under
  # a sanitizer that catches SIGSEGV, or to make `timeout` itself fail), so they
  # are fired through the SAME `classify_log` the corpus calls, on a synthetic
  # log. Same code path, same shell-out, same one classifier.
  _cl() { # rc log-contents expected
    printf '%s' "$2" > "$_sout/probe.log"
    _cg="$(classify_log "$_sout/probe.log" "$1")"
    if [ "$_cg" != "$3" ]; then
      echo "  SELF-TEST FAIL: classify_log(rc=$1) = '$_cg', expected '$3'"; _fail=1
    fi
  }
  _cl 0   ''                                          CLEAN
  _cl 139 ''                                          CRASH:sig11
  _cl 134 ''                                          CRASH:sig6
  _cl 124 ''                                          TIMEOUT
  _cl 127 ''                                          RUNNER_FAIL
  _cl 1   ''                                          UNKNOWN
  _cl 0   'x.c:1: runtime error: signed overflow'     UBSAN
  _cl 0   '==1==ERROR: LeakSanitizer: detected memory leaks' LEAK
  _cl 0   '==1==ERROR: AddressSanitizer: heap-use-after-free' ASAN_heap-use-after-free
  _cl 0   '==1==ERROR: AddressSanitizer: stack-overflow'      ASAN_stack-overflow
  # THE INVERSE for CLEAN, which is the label that mattered: it must NOT fire on
  # a silent nonzero exit. That single row is R47 failure instance 1 — this
  # file's own `[ -z "$_labels" ] && _labels=CLEAN` published CLEAN over a
  # deterministic rc-139 SIGSEGV, and "make CLEAN fire" passed the whole time.
  for _rc in 139 134 7 1 124 127; do
    printf '' > "$_sout/probe.log"
    case "$(classify_log "$_sout/probe.log" "$_rc")" in
      *CLEAN*) echo "  SELF-TEST FAIL: rc=$_rc classified CLEAN — the sink is back"; _fail=1;;
    esac
  done

  # The class signatures must be the SAME mechanism at 1 record and 2 records —
  # that is what makes the next assertion a same-fixture, same-signature test
  # rather than a trivial cross-class one.
  _sig1="$(_get selftest_leak 4)"; _sig2="$(_get selftest_leak_twice 4)"
  _sym1="${_sig1%%\**}"; _sym2="${_sig2%%\**}"
  if [ "$_sig1" != "$_sym1*1" ] || [ "$_sig2" != "$_sym2*2" ] || [ "$_sym1" != "$_sym2" ]; then
    echo "  SELF-TEST FAIL: expected one class at x1 and the SAME class at x2, got '$_sig1' and '$_sig2'"
    _fail=1
  fi
  # The class key IS a symbol name, so the whole schema depends on a working
  # symbolizer. Without one every frame resolves to UNSYMBOLIZED and all 313
  # corpus rows red at once with no hint why — so fail HERE, on one line.
  if [ "$_sym1" = "UNSYMBOLIZED" ] || [ "$_sym2" = "UNSYMBOLIZED" ]; then
    echo "  SELF-TEST FAIL: leak frames are not being symbolized — install llvm-symbolizer"
    echo "                  (or set ASAN_SYMBOLIZER_PATH). The leak allowlist is keyed by"
    echo "                  allocation-site SYMBOL and cannot be checked without it."
    _fail=1
  fi

  # Now drive the real adjudicator. `selftest_leak_twice` is allowlisted for
  # ONE record of exactly the class it exhibits: a per-class COUNT check must
  # fire on the second record, a signature-SET check structurally cannot. This
  # is the guard catching its own class (Core #15e Q2).
  printf '%s\t%s\n' selftest_leak       "$_sig1" >  "$_sout/allow_probe"
  printf '%s\t%s\n' selftest_leak_twice "$_sig1" >> "$_sout/allow_probe"
  adjudicate_leaks "$_sout/allow_probe" "$_sout/verdicts.tsv" "$_sout/adj"
  grep -qx 'selftest_alternating_leak' "$_sout/adj/new_leak" \
    || { echo "  SELF-TEST FAIL: an UNLISTED leaking fixture was not reported as a NEW LEAK"; _fail=1; }
  grep -q  '^selftest_leak_twice	' "$_sout/adj/new_class" \
    || { echo "  SELF-TEST FAIL: a SECOND record of an ALREADY-TOLERATED class was not reported"; _fail=1; }
  grep -q  '^selftest_leak	' "$_sout/adj/new_class" \
    && { echo "  SELF-TEST FAIL: a fixture leaking exactly what its row tolerates was reported"; _fail=1; }

  if [ "$_fail" -ne 0 ]; then
    echo
    echo "❌ THE SANITIZE GATE'S OWN INSTRUMENT IS BROKEN. No corpus verdict is"
    echo "   trustworthy until the controls in $SELFTEST_DIR/ behave again."
    echo "   verdicts: $_sout/verdicts.tsv   logs: $_sout/logs/"
    return 1
  fi
  echo "self-test:   OK — leak detector fired, flake detector fired, class check fired on a"
  echo "             second record of an already-tolerated class, clean control stayed quiet"
  return 0
}

if [ "$RUN_SELFTEST" = "1" ] || [ "$SELFTEST_ONLY" = "1" ]; then
  run_selftest || exit 2
fi
[ "$SELFTEST_ONLY" = "1" ] && exit 0

# Allowlisted names (column 1, comments stripped).
awk '!/^#/ && NF {print $1}' "$CORRUPT_LIST" | sort -u > "$OUT/allow_corrupt"
awk '!/^#/ && NF {print $1}' "$LEAK_LIST"    | sort -u > "$OUT/allow_leak"

# Every LEAK row must carry its class column, or the class check silently
# degrades to the bare-stem behaviour it replaced (Core #14: an assertion with
# no enforcing guard is rot).
malformed=$(awk -F'\t' '!/^#/ && NF && (NF < 2 || $2 == "" || $2 == "-") {print $1}' "$LEAK_LIST")
if [ -n "$malformed" ]; then
  echo "MALFORMED $LEAK_LIST — these rows have no class column:"; echo "$malformed" | sed 's/^/    /'
  echo "Format: <fixture-stem> TAB <top-frame>*<records>[,<top-frame>*<records>...]"
  exit 2
fi

# --- the corpus --------------------------------------------------------------
if [ -n "$FIXLIST" ]; then cat "$FIXLIST"; else find tests/fixtures -maxdepth 1 -name '*.gg' | sort; fi \
  | xargs -P "$JOBS" -I{} bash -uc 'run_one "$@"' _ {} > "$OUT/verdicts.tsv"

awk -F'\t' '$2 ~ /ASAN_/             {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_corrupt"
awk -F'\t' '$2 ~ /(^|,)LEAK(,|$)/    {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_leak"
awk -F'\t' '$2 ~ /(^|,)UBSAN(,|$)/   {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_ubsan"
awk -F'\t' '$2 ~ /(^|,)TIMEOUT(,|$)/ {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_timeout"
awk -F'\t' '$2 == "SKIP_COPY" || $2 == "NO_BINARY" || $2 ~ /(^|,)RUNNER_FAIL(,|$)/ {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_infra"
# CRASH carries its signal or its off-taxonomy rc after a colon (CRASH:sig11),
# so the class match is on the PREFIX -- the kind is captured, never enumerated.
awk -F'\t' '$2 ~ /(^|,)CRASH(:|,|$)/   {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_crash"
awk -F'\t' '$2 ~ /(^|,)UNKNOWN(,|$)/   {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_unknown"
# EXIT:n — the fixture chose a nonzero exit code. Its own business; this sweep
# adjudicates MEMORY VALIDITY, and a deliberate exit is neither a leak nor a
# crash. Counted so it cannot read as CLEAN, gating nothing.
awk -F'\t' '$2 ~ /(^|,)EXIT:/          {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_exit"
awk -F'\t' '$3 ~ /FLAKY/             {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_flaky"
awk -F'\t' '$3 ~ /CLASS_UNSTABLE/    {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_class_unstable"
awk -F'\t' '$3 ~ /COUNT_DRIFT/       {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_count_drift"
# "covered" = produced a RUN verdict at all. Everything else left the gate's
# population without the gate noticing, which is the coverage hole this floor
# exists to catch.
awk -F'\t' '$2 !~ /^(SKIP_COPY|NO_BINARY|RUNNER_FAIL|BUILD_FAIL_SANITIZE_ONLY|BUILD_FAIL_BOTH)$/ {print $1}' \
  "$OUT/verdicts.tsv" | sort -u > "$OUT/covered"

new_corrupt=$(comm -23 "$OUT/got_corrupt" "$OUT/allow_corrupt")
fixed_corrupt=$(comm -13 "$OUT/got_corrupt" "$OUT/allow_corrupt")
adjudicate_leaks "$LEAK_LIST" "$OUT/verdicts.tsv" "$OUT"

n_covered=$(wc -l < "$OUT/covered")
n_flaky=$(wc -l < "$OUT/got_flaky")
n_class_unstable=$(wc -l < "$OUT/got_class_unstable")
n_count_drift=$(wc -l < "$OUT/got_count_drift")
n_ubsan=$(wc -l < "$OUT/got_ubsan")
n_timeout=$(wc -l < "$OUT/got_timeout")
n_infra=$(wc -l < "$OUT/got_infra")
n_crash=$(wc -l < "$OUT/got_crash")
n_unknown=$(wc -l < "$OUT/got_unknown")
n_exit=$(wc -l < "$OUT/got_exit")

echo "=== sanitize sweep ==="
echo "instrument:  REPS=$REPS  JOBS=$JOBS  timeout=${RUN_TIMEOUT}s  LSAN_OPTIONS='${LSANOPT:-<default roots>}'  ASAN_OPTIONS='$ASANOPT'"
# Enough provenance that a reader can tell WHICH compiler produced these
# numbers: a version string alone cannot distinguish two builds of the same
# release, and a sweep run against a binary that was rebuilt underneath it is
# how one measurement in this gate's history had to be thrown away.
echo "gg:          $GG ($("$GG" --version 2>/dev/null | head -1), $(stat -c '%s bytes, mtime %y' "$GG" 2>/dev/null | cut -c1-40))"
echo "scanned:     $(wc -l < "$OUT/verdicts.tsv")"
echo "covered:     $n_covered (floor $COVERAGE_FLOOR)"
echo "corruption:  $(wc -l < "$OUT/got_corrupt") (allowlisted $(wc -l < "$OUT/allow_corrupt"))"
echo "leaks:       $(wc -l < "$OUT/got_leak") (allowlisted $(wc -l < "$OUT/allow_leak"))"
echo "ubsan:       $n_ubsan (ceiling $UBSAN_CEILING)"
echo "timeout:     $n_timeout (ceiling $TIMEOUT_CEILING)"
echo "infra:       $n_infra (SKIP_COPY/NO_BINARY/RUNNER_FAIL; ceiling $INFRA_CEILING)"
echo "crash:       $n_crash (signal / off-taxonomy exit; ceiling $CRASH_CEILING)"
echo "unknown:     $n_unknown (classifier could not tell; ceiling '${UNKNOWN_CEILING:-<unset>}')"
echo "exit-nonzero:$n_exit (the fixture's OWN exit code; census, not a fault)"
echo "flaky:       $n_flaky (verdict not unanimous over $REPS runs; ceiling $FLAKY_CEILING)"
echo "class-drift: $n_class_unstable (leak MECHANISM SET not identical over $REPS runs; ceiling $CLASS_UNSTABLE_CEILING)"
echo "count-drift: $n_count_drift (same mechanisms, differing record COUNTS — census, not a gate;"
echo "             these rows earn a '*N+' marker in $LEAK_LIST)"
[ "$n_count_drift" -gt 0 ] && sed 's/^/                 /' "$OUT/got_count_drift"
echo "population census (every label the classifier can emit):"
awk -F'\t' '{print $2}' "$OUT/verdicts.tsv" | sort | uniq -c | sort -rn | sed 's/^/    /'
echo "raw verdicts: $OUT/verdicts.tsv   logs: $OUT/logs/"

rc=0
if [ -n "$new_corrupt" ]; then
  echo; echo "❌ NEW MEMORY CORRUPTION — this is a soundness regression, not debt:"
  echo "$new_corrupt" | sed 's/^/    /'
  echo "    Fix it. Adding a row to $CORRUPT_LIST ships a known memory-safety bug"
  echo "    and needs an owner decision plus a filed TODO entry."
  rc=1
fi
if [ -s "$OUT/new_leak" ]; then
  echo; echo "❌ NEW LEAK(S) — no row in $LEAK_LIST at all:"; sed 's/^/    /' "$OUT/new_leak"
  echo "    Fix it, or justify adding it to $LEAK_LIST."
  rc=1
fi
if [ -s "$OUT/new_class" ]; then
  echo; echo "❌ NEW LEAK CLASS inside an ALREADY-ALLOWLISTED fixture:"
  sed 's/^/    /' "$OUT/new_class"
  echo "    The row tolerates the leak this fixture HAD. This is a different one,"
  echo "    or one more of the same. Fix it, or adjudicate the row deliberately."
  rc=1
fi
if [ "$n_ubsan" -gt "$UBSAN_CEILING" ]; then
  echo; echo "❌ UNDEFINED BEHAVIOUR (UBSan) — ceiling $UBSAN_CEILING:"; sed 's/^/    /' "$OUT/got_ubsan"
  rc=1
fi
if [ "$n_timeout" -gt "$TIMEOUT_CEILING" ]; then
  echo; echo "❌ TIMED OUT (killed at ${RUN_TIMEOUT}s) — ceiling $TIMEOUT_CEILING:"; sed 's/^/    /' "$OUT/got_timeout"
  echo "    A hang is a defect, not a flake. Root-cause it into a census row."
  rc=1
fi
if [ "$n_crash" -gt "$CRASH_CEILING" ]; then
  echo; echo "❌ CRASHED (signal, or an exit code off the ratified taxonomy) — ceiling $CRASH_CEILING:"
  sed 's/^/    /' "$OUT/got_crash"
  echo "    These used to read CLEAN. A process that dies on a signal has not"
  echo "    run correctly, whatever its stdout said."
  rc=1
fi
if [ -n "$UNKNOWN_CEILING" ] && [ "$n_unknown" -gt "$UNKNOWN_CEILING" ]; then
  echo; echo "❌ UNCLASSIFIABLE OUTCOME(S) — ceiling $UNKNOWN_CEILING:"
  sed 's/^/    /' "$OUT/got_unknown"
  echo "    The classifier found no positive discriminator. Root-cause it into a"
  echo "    verdict; do NOT widen a rule until it swallows the case."
  rc=1
elif [ "$n_unknown" -gt 0 ]; then
  echo; echo "ℹ UNCLASSIFIABLE OUTCOME(S) — within the ceiling of ${UNKNOWN_CEILING:-<unset>}:"
  sed 's/^/    /' "$OUT/got_unknown"
  echo "    These used to read CLEAN. Each is the run-phase rc-1 ambiguity cell"
  echo "    (todo/t0647): a program's own exit(1) is indistinguishable from"
  echo "    gorget_panic_at's. Resolving t0647 retires them and lowers the ceiling."
fi
if [ "$n_infra" -gt "$INFRA_CEILING" ]; then
  echo; echo "❌ SWEEP PLUMBING FAILED (SKIP_COPY / NO_BINARY / RUNNER_FAIL) — ceiling $INFRA_CEILING:"
  sed 's/^/    /' "$OUT/got_infra"
  rc=1
fi
if [ "$n_flaky" -gt "$FLAKY_CEILING" ]; then
  echo; echo "❌ NONDETERMINISTIC ROW(S) — verdict not unanimous over $REPS runs:"
  sed 's/^/    /' "$OUT/got_flaky"
  echo "    A flaky row makes this gate unfalsifiable: it manufactures phantom"
  echo "    regressions and it trains readers to re-run until green. Root-cause it."
  rc=1
fi
if [ "$n_class_unstable" -gt "$CLASS_UNSTABLE_CEILING" ]; then
  echo; echo "❌ UNSTABLE LEAK MECHANISMS — the same fixture leaked through a different"
  echo "   SET of mechanisms on different runs, so its row cannot be pinned at all:"
  sed 's/^/    /' "$OUT/got_class_unstable"
  rc=1
fi
# COVERAGE_FLOOR=0 disables the check — that is what a FIXLIST demonstration
# over a handful of fixtures passes, since a corpus-scale floor is meaningless
# there. Any other value is checked, so the guard stays demonstrable.
if [ "$COVERAGE_FLOOR" -gt 0 ] && [ "$n_covered" -lt "$COVERAGE_FLOOR" ]; then
  echo; echo "❌ COVERAGE FELL: $n_covered fixtures ran under the sanitizer, floor is $COVERAGE_FLOOR."
  echo "    A fixture that stops BUILDING leaves this gate silently. Find it in the"
  echo "    census above (BUILD_FAIL_*), fix it, or lower the floor deliberately."
  rc=1
fi
[ -n "$fixed_corrupt" ] && { echo; echo "✅ no longer corrupting — DELETE these rows from $CORRUPT_LIST:"; echo "$fixed_corrupt" | sed 's/^/    /'; }
[ -s "$OUT/fixed_leak" ] && { echo; echo "✅ no longer leaking — DELETE these rows from $LEAK_LIST:"; sed 's/^/    /' "$OUT/fixed_leak"; }
[ -s "$OUT/shrunk_class" ] && { echo; echo "✅ leaking LESS than its row admits — TIGHTEN these rows in $LEAK_LIST:"; sed 's/^/    /' "$OUT/shrunk_class"; }
exit $rc
