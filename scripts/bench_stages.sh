#!/usr/bin/env bash
#
# scripts/bench_stages.sh — per-stage wall + peak-RSS + clone-count decomposition
# of the self-host bootstrap fixed-point chain (the `self_host_bootstrap_fixed_point`
# integration test's stage sequence, run standalone as a tracked meter).
# Answers "where does the bootstrap wall-clock go" — the per-stage direct meter
# behind the indirect 600s stage deadlines (which exist to catch clone-bombs
# and other de-optimizations; this makes the signal direct and attributable).
#
# ⚠ RUN SOLO ON A QUIET BOX. The wall numbers are the deliverable; parallel
#   cargo/test load contaminates them (the ~866s-vs-594s fixed_point drift
#   investigations died on load-contaminated numbers). Requires a current
#   `./target/release/gg` (build first: cargo build --release).
#
# Output: one TSV row per stage (tee'd to --out), columns:
#   stage        S0_build | S0->1 | cc_1 | S1->2 | cc_2 | S2->3
#   wall_s       wall-clock seconds for the stage
#   peak_rss_mb  peak resident set (MB). Sources, in order of exactness:
#                the driver's own [clone-stats] peak_rss_kb self-report
#                (exact, /proc/self/status VmHWM at exit — S0->1 only, the
#                stage built with --clones=stats), else a 50ms /proc VmHWM
#                poll of the direct child. "n/a (fork)" for the cc stages —
#                the cc wrapper execs cc1 in a fork the poll can't see.
#                (/usr/bin/time is absent in the dev container.)
#   array_clone  the stage's self-reported [clone-stats] array_clone count.
#                ALL compile stages report it: the aggregate counters and the
#                armed atexit report live in the runtime PREAMBLE, prepended
#                from the --clones=stats driver's emitted C — so even the
#                SH-lowered stage binaries self-report aggregates. (What
#                SH-emitted C still lacks is per-SITE attribution, the
#                __gorget_clone_site_hit emission — TODO.md's stage-1 clone
#                ceiling guard candidate is directly seedable from this
#                harness's S1->2 number.) "-" = no [clone-stats] line
#                (S0_build, the cc stages).
#   note         what the stage does
#
# Stages: S0_build = Rust gg (release) builds the stage-0 driver with
# --clones=stats; S0->1 = that driver self-compiles (driver.gg+lib --lir-c →
# stage1.c) — THE canonical clone workload, same as `self_host_clone_ceiling`;
# cc_N = cc -O0 of stageN.c; S1->2 / S2->3 = the O0-built stage binaries
# self-compile. The final line diffs stage2.c vs stage3.c (the fixed-point
# convergence check; expect IDENTICAL).
#
# Usage:
#   scripts/bench_stages.sh [--out /tmp/stages.tsv]
#
# Not a test (yet): re-runnable meter first; ratchet-ification (per-stage
# wall/RSS budgets) is a TODO.md follow-up once numbers stabilize.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

OUT="/tmp/stages_$$.tsv"
if [[ "${1:-}" == "--out" ]]; then OUT="$2"; shift 2; fi

GG="./target/release/gg"
DRIVER_GG="tests/fixtures/self_host_lowerer/driver.gg"
LIB="$REPO_ROOT/lib"
W="$(mktemp -d /tmp/bench_stages.XXXXXX)"
DRIVER_EXE="$W/driver_exe"

if [[ ! -x "$GG" ]]; then
    echo "ERROR: $GG not found — run 'cargo build --release' first." >&2
    exit 2
fi

# poll_rss <pid> — prints the max VmHWM (kB) observed on the direct child.
poll_rss() {
    local pid="$1" max=0 v
    while kill -0 "$pid" 2>/dev/null; do
        v=$(awk '/VmHWM:/{print $2}' "/proc/$pid/status" 2>/dev/null || echo 0)
        if [[ -n "$v" && "$v" -gt "$max" ]]; then max="$v"; fi
        sleep 0.05
    done
    echo "$max"
}

# run_stage <label> <poll|selfreport|none> <stdout-file> -- cmd...
#   → echoes "wall_s<TAB>rss_mb". Child stderr lands in $W/<label>.stderr.
# The stdout target is an EXPLICIT argument, never ambient env: a leaked
# env-prefix assignment persisting across calls made a later cc stage truncate
# an earlier stage's emitted .c in the prototype (assignments-only commands
# assign in the CURRENT shell, and the stale value redirected the next stage).
run_stage() {
    local label="$1" rssmode="$2" stdout_to="$3"; shift 3
    if [[ "${1:-}" == "--" ]]; then shift; fi
    local errlog="$W/$label.stderr" t0 t1 wall rss_kb=0
    t0=$(date +%s.%N)
    ( "$@" >"$stdout_to" 2>"$errlog" ) &
    local cpid=$!
    if [[ "$rssmode" == "poll" ]]; then rss_kb=$(poll_rss "$cpid"); fi
    wait "$cpid" || { echo "ERROR: stage $label failed; stderr tail:" >&2; tail -5 "$errlog" >&2; exit 1; }
    t1=$(date +%s.%N)
    wall=$(awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.1f", b - a}')
    if [[ "$rssmode" == "selfreport" ]]; then
        rss_kb=$(grep '^\[clone-stats\]' "$errlog" | tail -1 | \
                 awk '{for (i=1; i<=NF; i++) {split($i, k, "="); if (k[1]=="peak_rss_kb") print k[2]}}')
        rss_kb="${rss_kb:-0}"
    fi
    echo -e "${wall}\t$(awk -v k="$rss_kb" 'BEGIN{printf "%.0f", k / 1024}')"
}
# clones_of <label> — the stage's [clone-stats] array_clone, or "-" if absent.
clones_of() {
    local c
    c=$(grep '^\[clone-stats\]' "$W/$1.stderr" 2>/dev/null | tail -1 | \
        awk '{for (i=1; i<=NF; i++) {split($i, k, "="); if (k[1]=="array_clone") print k[2]}}')
    echo "${c:--}"
}

echo -e "stage\twall_s\tpeak_rss_mb\tarray_clone\tnote" | tee "$OUT"

r=$(run_stage S0_build poll /dev/null -- \
    "$GG" build --clones=stats "$DRIVER_GG" -o "$DRIVER_EXE")
echo -e "S0_build\t${r}\t-\tRust gg (release) builds driver.gg (spawns cc internally)" | tee -a "$OUT"

# Runtime preamble = everything before the first user typedef in the driver's
# emitted C (`gg build X -o E` writes the C next to the exe as E.c).
PRE="$W/preamble.c"
awk '/^typedef struct __gg_/{exit} {print}' "$DRIVER_EXE.c" > "$PRE"

r=$(run_stage S0to1 selfreport "$W/stage1.c" -- \
    "$DRIVER_EXE" "$DRIVER_GG" "$LIB" --lir-c)
echo -e "S0->1\t${r}\t$(clones_of S0to1)\tdriver self-compiles driver.gg+lib (clone workload)" | tee -a "$OUT"
cat "$PRE" "$W/stage1.c" > "$W/stage1_full.c"

r=$(run_stage cc_1 none /dev/null -- \
    cc -O0 -w -o "$W/stage1_bin" "$W/stage1_full.c" -lm -lpthread)
echo -e "cc_1\t${r%%$'\t'*}\tn/a (fork)\t-\tcc -O0 stage1.c" | tee -a "$OUT"

r=$(run_stage S1to2 poll "$W/stage2.c" -- \
    "$W/stage1_bin" "$DRIVER_GG" "$LIB" --lir-c)
echo -e "S1->2\t${r}\t$(clones_of S1to2)\tO0 stage1_bin self-compiles" | tee -a "$OUT"
cat "$PRE" "$W/stage2.c" > "$W/stage2_full.c"

r=$(run_stage cc_2 none /dev/null -- \
    cc -O0 -w -o "$W/stage2_bin" "$W/stage2_full.c" -lm -lpthread)
echo -e "cc_2\t${r%%$'\t'*}\tn/a (fork)\t-\tcc -O0 stage2.c" | tee -a "$OUT"

r=$(run_stage S2to3 poll "$W/stage3.c" -- \
    "$W/stage2_bin" "$DRIVER_GG" "$LIB" --lir-c)
echo -e "S2->3\t${r}\t$(clones_of S2to3)\tO0 stage2_bin self-compiles (convergence)" | tee -a "$OUT"

if diff -q "$W/stage2.c" "$W/stage3.c" >/dev/null 2>&1; then verdict=IDENTICAL; else verdict=DIFFER; fi
echo "convergence: stage2.c vs stage3.c → $verdict" | tee -a "$OUT"
echo "(work dir kept for inspection: $W   table: $OUT)" >&2
