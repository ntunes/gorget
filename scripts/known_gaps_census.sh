#!/usr/bin/env bash
# Regenerate the known_gaps GRADUATION CENSUS: run every `#[ignore]`d test that
# cites a `tests/fixtures/known_gaps/` fixture against the compiler at HEAD and
# report which ones now PASS.
#
# ── WHY THIS EXISTS ───────────────────────────────────────────────────────
# A `known_gaps/` fixture + an `#[ignore]`d test asserting the INTENDED
# behaviour is the tree's durable-repro contract (AGENTS.md "Task Continuity").
# The contract's second half — "graduate it the same round the bug is fixed" —
# was PROSE, and prose rots: nothing in the suite ever runs an ignored test, so
# a gap fixed as a side effect of some other round stays filed forever,
# inflating `scripts/convergence.sh`'s known_gaps count and leaving a TODO item
# describing a bug that no longer exists. This script is the executable half
# (devbook/25 "structural guards": prose rots, guards don't).
#
# ⚠ A PASS IS A FINDING, NOT A VERDICT. Three things make a test pass:
#   (a) GRADUATE      — the gap is genuinely fixed. Un-ignore, move the fixture
#                       out of `known_gaps/`, close the TODO item.
#   (b) BLIND ASSERT  — the test cannot observe the gap it is parked for (the
#                       reason names the SELF-HOST lane but the body calls the
#                       Rust-lane `run_gg`; the reason is a PERF pathology but
#                       the body asserts only stdout). It was GREEN ON ARRIVAL
#                       and pins nothing (Core #12). Fix the ASSERTION — do NOT
#                       un-ignore, that converts a live gap into a closed one on
#                       paper.
#   (c) PINS TODAY    — the test asserts CURRENT behaviour while the open item
#                       is a DESIGN question. Neither graduate nor rewire until
#                       the design is ratified; it is an owner ask.
# Telling (a) from (b) needs the MECHANISM, not the exit code: probe the axis
# the filing names (self-host emit/cc/run for an SH-lane row, `--clones=stats`
# for a perf row) and break-and-watch the cited fix site (Core #13) before
# calling anything graduated. Measured at R44: 12 of 98 passed and ALL TWELVE
# were (b) or (c) — zero graduations.
#
# Usage:
#   scripts/known_gaps_census.sh            # full census, TSV on stdout
#   scripts/known_gaps_census.sh --check    # CI GATE: exit 1 on any drift vs
#                                           # tests/gaps/PASSING_ALLOWLIST.txt
#   scripts/known_gaps_census.sh --list     # enumerate the roster, run nothing
#   scripts/known_gaps_census.sh --fast     # skip the self-host-driver rows
#   scripts/known_gaps_census.sh --isolate  # one subprocess per row (hang triage)
#   scripts/known_gaps_census.sh <name>…    # census only these tests
#
# COST — NO RUN TIMING OR ROSTER COUNT IN THIS BLOCK, DELIBERATELY: those two are
# the ones that rotted. Two figures elsewhere in this header deliberately STAY,
# because neither tracks the roster: the structural `~90 s` driver build (it
# explains WHY batching matters) and the DATED `Measured at R44: 12 of 98`
# above (history, supporting a qualitative claim, not a live count).
# Run it (Core #5):
#   time scripts/known_gaps_census.sh          # full
#   time scripts/known_gaps_census.sh --fast   # meant to skip the self-host rows
# Each run ends with its own `# roster N · PASS n · FAIL n · SKIPPED_SH n` line
# on stderr; that line IS the number, and it is always current.
#
# ⚠ THIS COMMENT USED TO CARRY "1 min 47 s (99 rows), --fast 14 s (87 rows)"
# one line under an instruction not to trust quoted figures — and it was the
# FIFTH copy of that rotted pair. The roster had grown past 99 long before
# anyone noticed, because a number can be transcribed and a command cannot.
# The four other copies (tests/lints.rs ×2, tests/gaps/PASSING_ALLOWLIST.txt,
# .github/workflows/ci.yml) now all point HERE and quote no figure either. If
# you are about to add a timing to any of them: don't — add the command.
#
# ⚠ AND THE `--fast` FIGURE WAS NEVER REACHABLE. `is_sh_row` (below) decides
# "does this row drive the self-host lowerer?" by GREPPING THE TEST BODY FOR
# THREE HELPER NAMES, so every row reaching the driver through any other helper
# is invisible to it and runs anyway — which means `--fast` still pays the
# ~90 s driver build and is nowhere near the quoted 14 s. Filed as `todo/t0826`
# with the reproduction and the reference-grade shape.
#
# ⚠ AND `--list | wc -l` IS OFF BY ONE: `--list` prints a `# ignored tests
# citing known_gaps: N` header before the rows. Read that header, or the
# `roster N` summary line, rather than counting lines.
# BATCHING every row for a test file into ONE invocation is what keeps the full
# run cheap: `build_gg_dir_cached`'s `OnceLock` builds `driver.gg` (~90 s) once
# and shares it across every test in that process — which is most of the full
# run's cost and nearly all of the gap between it and `--fast`. `--isolate`
# forfeits the sharing, re-paying that ~90 s PER self-host row, so use it only
# to localise a hang, never as the default.
set -uo pipefail
cd "$(dirname "$0")/.."

ALLOWLIST=tests/gaps/PASSING_ALLOWLIST.txt
LIST_ONLY=0; FAST=0; CHECK=0; ISOLATE=0
ONLY=()
for arg in "$@"; do
  case "$arg" in
    --list)    LIST_ONLY=1 ;;
    --fast)    FAST=1 ;;
    --check)   CHECK=1 ;;
    --isolate) ISOLATE=1 ;;
    -*) echo "unknown flag: $arg" >&2; exit 2 ;;
    *)  ONLY+=("$arg") ;;
  esac
done

# ── Enumerate: an `#[ignore]`d test fn that cites a known_gaps fixture ────
# ⚠ The ignore detector matches `#[ignore` ONLY in ATTRIBUTE POSITION (start of
# line, modulo indentation). A DOC COMMENT that merely MENTIONS `#[ignore]`
# must not mark the next test fn as ignored — that bug in the pre-R44
# `scripts/convergence.sh` classified four live tests as open gaps. Do not
# relax this to a bare substring match.
#
# Two reference spellings are recognised: `known_gaps/<name>.gg` (the common
# one) and `known_gaps/<dir>` as a bare directory argument to `run_gg_dir`.
enumerate() {
  for f in tests/*.rs; do
    awk -v F="$f" '
      /^[[:space:]]*#\[ignore/          { ign = 1; next }
      /^[[:space:]]*(pub )?fn [A-Za-z0-9_]+\(/ {
        if (pend_n > 0) { for (i = 0; i < pend_n; i++) print pend[i]; pend_n = 0 }
        cur_ign = ign; ign = 0
        match($0, /fn [A-Za-z0-9_]+/); cur_fn = substr($0, RSTART + 3, RLENGTH - 3)
        cur_line = NR; refs = ""
        next
      }
      {
        line = $0
        while (match(line, /known_gaps\/[A-Za-z0-9_]+(\/[A-Za-z0-9_.]+)?(\.gg)?/)) {
          r = substr(line, RSTART + 11, RLENGTH - 11)
          sub(/\.gg$/, "", r); sub(/\/.*$/, "", r)
          if (cur_ign && cur_fn != "" && index(refs, "|" r "|") == 0) {
            refs = refs "|" r "|"
            pend[pend_n++] = cur_fn "\t" F ":" cur_line "\t" r
          }
          line = substr(line, RSTART + RLENGTH)
        }
      }
      END { for (i = 0; i < pend_n; i++) print pend[i] }
    ' "$f"
  done | awk -F'\t' '{ k = $1 "\t" $2; fx[k] = (k in seen ? fx[k] "," $3 : $3); seen[k] = 1 }
                     END { for (k in fx) print k "\t" fx[k] }' | sort
}

ROSTER=$(mktemp); trap 'rm -f "$ROSTER" "$RESULTS" "$BATCH"' EXIT
RESULTS=$(mktemp); BATCH=$(mktemp)
enumerate > "$ROSTER" || exit 1
total=$(wc -l < "$ROSTER" | tr -d ' ')

if [ "$LIST_ONLY" -eq 1 ]; then
  echo "# ignored tests citing known_gaps: $total"
  cat "$ROSTER"
  exit 0
fi

# A row whose body drives the self-host lowerer. These are the expensive ones,
# and the reason batching matters.
# Reads tests/integration.rs ONLY. An SH-driver row in another tests/*.rs would
# be RUN by --fast rather than skipped -- slower, never wrong, and --fast is not
# the CI gate (the full run is). Widen the sed target if such a row appears.
is_sh_row() {
  sed -n "/^fn $1(/,/^}/p" tests/integration.rs 2>/dev/null |
    grep -q 'assert_self_host_stdout\|self_host_emit_cc_run\|build_gg_dir_cached'
}

cargo test --no-run --tests >/dev/null 2>&1 || {
  echo "known_gaps_census.sh: test binaries failed to build" >&2; exit 1; }
bin_for() {
  local stem=${1#tests/}; stem=${stem%.rs}
  ls -t target/debug/deps/"$stem"-* 2>/dev/null | grep -v '\.d$' | head -1
}

export GG_BUILD_TIMEOUT_SECS="${GG_BUILD_TIMEOUT_SECS:-600}"
export GG_TEST_TIMEOUT_SECS="${GG_TEST_TIMEOUT_SECS:-120}"

# Select the rows to run.
: > "$BATCH"
while IFS=$'\t' read -r name site fixtures; do
  if [ ${#ONLY[@]} -gt 0 ]; then
    keep=0; for w in "${ONLY[@]}"; do [ "$w" = "$name" ] && keep=1; done
    [ "$keep" -eq 1 ] || continue
  fi
  if [ "$FAST" -eq 1 ] && is_sh_row "$name"; then
    printf '%s\t%s\tSKIPPED_SH\t%s\n' "$name" "$site" "$fixtures" >> "$RESULTS"
    continue
  fi
  printf '%s\t%s\t%s\n' "$name" "$site" "$fixtures" >> "$BATCH"
done < "$ROSTER"

# ── Run. PROBE DISCIPLINE (Core #15d): every exit status is read off the BARE
# command, never off a pipeline (`cmd | tail` reports tail's status), and output
# is captured to a file rather than piped.
run_group() {  # $1 = test file, rest = names
  local file=$1; shift
  local bin; bin=$(bin_for "$file")
  if [ -z "$bin" ]; then
    for n in "$@"; do printf '%s\tNO_BINARY\n' "$n" >> "$RESULTS.v"; done
    return
  fi
  local log; log=$(mktemp)
  # An outer deadline: the harness bounds each fixture's build/run, but not a
  # hang inside libtest itself.
  # NOT `--nocapture`: with it libtest emits `test NAME ... ` and defers the
  # verdict to a later line, so every FAIL parsed as NOT_RUN (measured — the
  # first cut of this script reported 82 NOT_RUN and 0 FAIL). Captured mode puts
  # the verdict on one line; use `--isolate` when you want the panic text.
  timeout 3600 "$bin" --ignored --exact --test-threads=1 "$@" > "$log" 2>&1
  local rc=$?
  for n in "$@"; do
    if grep -qE "^test $n \.\.\. ok$" "$log"; then
      printf '%s\tPASS\n' "$n" >> "$RESULTS.v"
    elif grep -qE "^test $n \.\.\. (FAILED|ignored)$" "$log"; then
      printf '%s\tFAIL\n' "$n" >> "$RESULTS.v"
    elif [ $rc -eq 124 ]; then
      printf '%s\tTIMEOUT\n' "$n" >> "$RESULTS.v"
    else
      # No verdict line at all means the process died before reaching this row.
      printf '%s\tNOT_RUN\n' "$n" >> "$RESULTS.v"
    fi
  done
  rm -f "$log"
}

: > "$RESULTS.v"
if [ "$ISOLATE" -eq 1 ]; then
  while IFS=$'\t' read -r name site fixtures; do
    run_group "${site%%:*}" "$name"
  done < "$BATCH"
else
  for file in $(cut -f2 "$BATCH" | cut -d: -f1 | sort -u); do
    mapfile -t names < <(awk -F'\t' -v f="$file" '{ split($2, a, ":"); if (a[1] == f) print $1 }' "$BATCH")
    [ ${#names[@]} -gt 0 ] && run_group "$file" "${names[@]}"
  done
fi

# Join verdicts back onto the roster rows.
while IFS=$'\t' read -r name site fixtures; do
  v=$(awk -F'\t' -v n="$name" '$1 == n { print $2; exit }' "$RESULTS.v")
  printf '%s\t%s\t%s\t%s\n' "$name" "$site" "${v:-NOT_RUN}" "$fixtures" >> "$RESULTS"
done < "$BATCH"
rm -f "$RESULTS.v"

sort -o "$RESULTS" "$RESULTS"
printf 'test\tsite\tverdict\tfixtures\n'
cat "$RESULTS"

n_pass=$(awk -F'\t' '$3 == "PASS"' "$RESULTS" | wc -l | tr -d ' ')
n_fail=$(awk -F'\t' '$3 == "FAIL"' "$RESULTS" | wc -l | tr -d ' ')
n_skip=$(awk -F'\t' '$3 == "SKIPPED_SH"' "$RESULTS" | wc -l | tr -d ' ')
echo "# roster $total · PASS $n_pass · FAIL $n_fail · SKIPPED_SH $n_skip" >&2

[ "$CHECK" -eq 1 ] || exit 0

# ── --check: the GATE. The PASS set must equal the allowlist EXACTLY.
#
# Both directions are failures, and neither is cosmetic:
#   PASS but not allowlisted → an ignored test silently started passing.
#                              Adjudicate: graduate, or rewire the assertion.
#   allowlisted but no longer PASS → someone rewired the assertion (the good
#                              outcome) or graduated the test; the row is now
#                              stale and the ceiling must come down with it.
# Exact-set rather than a count, so a row cannot be swapped for another and
# stay green.
expected=$(mktemp); actual=$(mktemp)
grep -vE '^\s*#|^\s*$' "$ALLOWLIST" | awk '{print $1}' | sort -u > "$expected"
awk -F'\t' '$3 == "PASS" { print $1 }' "$RESULTS" | sort -u > "$actual"

if [ "$FAST" -eq 1 ]; then
  # Skipped rows cannot be adjudicated this run — drop them from BOTH sides so
  # --fast reports drift on what it actually measured, never a phantom.
  skipped=$(mktemp)
  awk -F'\t' '$3 == "SKIPPED_SH" { print $1 }' "$RESULTS" | sort -u > "$skipped"
  comm -23 "$expected" "$skipped" > "$expected.f" && mv "$expected.f" "$expected"
  rm -f "$skipped"
fi

new_pass=$(comm -13 "$expected" "$actual")
gone=$(comm -23 "$expected" "$actual")
rm -f "$expected" "$actual"

rc=0
if [ -n "$new_pass" ]; then
  rc=1
  echo "known_gaps_census: ✗ ignored test(s) now PASS and are NOT in $ALLOWLIST:" >&2
  echo "$new_pass" | sed 's/^/    /' >&2
  echo "  A PASS is a FINDING, not a graduation (see this script's header). Adjudicate" >&2
  echo "  each row by MECHANISM: probe the axis its filing names, and break the cited" >&2
  echo "  fix site to watch the test go red (Core #13). Then either GRADUATE it" >&2
  echo "  (un-ignore + move the fixture out of known_gaps/ + close the TODO item), or" >&2
  echo "  REWIRE its assertion onto the lane/axis the gap lives on, or — only if" >&2
  echo "  neither applies — add a row here with its REASON CODE and raise the ceiling" >&2
  echo "  in tests/lints.rs with a cited justification." >&2
fi
if [ -n "$gone" ]; then
  rc=1
  echo "known_gaps_census: ✗ allowlisted row(s) no longer PASS:" >&2
  echo "$gone" | sed 's/^/    /' >&2
  echo "  This is usually the WIN: the assertion was rewired onto the real lane, or the" >&2
  echo "  test graduated. Delete the row and LOWER the CEILING in tests/lints.rs in the" >&2
  echo "  same commit — that is the only way this backlog shrinks." >&2
fi
[ $rc -eq 0 ] && echo "known_gaps_census: ✓ PASS set matches $ALLOWLIST exactly" >&2
exit $rc
