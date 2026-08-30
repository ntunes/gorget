#!/usr/bin/env bash
#
# scripts/self_host_mem_baseline.sh — capture self-host memory & clone baseline.
#
# Workload: the Rust-compiled self-host driver (tests/fixtures/self_host_lowerer/driver.gg)
# running over its own source. The driver binary is re-used as input — this is the
# realistic self-host compilation step that exercises the clone-heavy paths in
# the Gorget runtime (GorgetArray/Map/Set clone, string CoW, box alloc).
#
# Output:
#   - JSON snapshot of: peak RSS (from GNU time), user/sys time, the [clone-stats] line,
#     and (optionally) `gg profile` per-phase timings.
#   - Printed summary to stderr.
#
# Usage:
#   scripts/self_host_mem_baseline.sh                          # capture baseline, write to /tmp/
#   scripts/self_host_mem_baseline.sh --out path/to/snap.json  # write to a specific path
#   scripts/self_host_mem_baseline.sh --compare path/to/base.json  # compare vs baseline
#
# Requires: /usr/bin/time (or gtime on macOS) for portable RSS capture.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"
# The workload is DECLARED, not spelled here: scripts/clone_meter.spec is the one
# definition the Rust gate reads too. This script used to roll its own argv, which
# is how the two instruments came to report different numbers for one meter.
# shellcheck source=scripts/clone_meter.sh
source "$REPO_ROOT/scripts/clone_meter.sh"

OUT="/tmp/self_host_mem_baseline.json"
COMPARE_AGAINST=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --out) OUT="$2"; shift 2 ;;
        --compare) COMPARE_AGAINST="$2"; shift 2 ;;
        *) echo "Unknown flag: $1" >&2; exit 2 ;;
    esac
done

# Pick /usr/bin/time (Linux) or gtime (macOS with coreutils). OPTIONAL — we fall
# back to the [clone-stats] line's peak_rss_kb (read from /proc/self/status inside
# the child) when no GNU time is available.
TIME_BIN=""
if [[ -x /usr/bin/time ]]; then
    if /usr/bin/time -v /bin/true >/dev/null 2>&1; then
        TIME_BIN=/usr/bin/time
    fi
fi
if [[ -z "$TIME_BIN" ]] && command -v gtime >/dev/null 2>&1; then
    TIME_BIN=gtime
fi

# Release build — we want realistic perf/alloc numbers.
echo "[1/4] Building gg release binary..." >&2
cargo build --release >&2

# Build the self-host driver (stage-0) with --clones=stats so its own emitted
# binary will also print the [clone-stats] line at exit. We're about to use
# that binary as the workload.
echo "[2/4] Building self-host driver with --clones=stats..." >&2
DRIVER_EXE="/tmp/self_host_driver_bench"
clone_meter_build ./target/release/gg "$DRIVER_EXE" /dev/stderr

# Run it on its own source — representative self-host workload.
echo "[3/4] Running self-host driver on its own source (captures RSS + clone-stats)..." >&2
TIME_LOG="/tmp/self_host_mem_time.log"
STDERR_LOG="/tmp/self_host_mem_stderr.log"

# `--lir-c` makes it emit C to stdout. We redirect stdout to /dev/null.
set +e
if [[ -n "$TIME_BIN" ]]; then
    # /usr/bin/time wraps the DECLARED invocation; it is not part of the meter.
    TIME_PREFIX=("$TIME_BIN" -v -o "$TIME_LOG")
else
    : > "$TIME_LOG"
    TIME_PREFIX=()
fi
clone_meter_run_timed() {
    local driver lib run_args
    driver=$(clone_meter_get driver); lib=$(clone_meter_get lib)
    run_args=$(clone_meter_get run_args)
    ( cd "$CLONE_METER_ROOT" && "${TIME_PREFIX[@]}" "$DRIVER_EXE" "$driver" "$lib" $run_args ) \
        >/dev/null 2>"$STDERR_LOG"
}
clone_meter_run_timed
EXIT=$?
set -e

if [[ $EXIT -ne 0 ]]; then
    echo "ERROR: self-host driver run failed (exit $EXIT)" >&2
    echo "stderr tail:" >&2
    tail -30 "$STDERR_LOG" >&2
    exit $EXIT
fi

# Extract metrics (from GNU time when available; zeros otherwise).
PEAK_RSS_KB=$(awk '/Maximum resident set size/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")
USER_TIME=$(awk '/User time \(seconds\)/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")
SYS_TIME=$(awk '/System time \(seconds\)/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")
WALL_TIME=$(awk -F': ' '/Elapsed \(wall clock\) time/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")
PAGE_FAULTS_MAJOR=$(awk '/Major \(requiring I\/O\) page faults/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")
PAGE_FAULTS_MINOR=$(awk '/Minor \(reclaiming a frame\) page faults/ {print $NF}' "$TIME_LOG" 2>/dev/null || echo "")

CLONE_STATS_LINE=$(grep '^\[clone-stats\]' "$STDERR_LOG" | tail -1 || true)

# Parse clone-stats fields (expected format:
#   [clone-stats] array_clone=N map_clone=N set_clone=N string_cow=N string_cat=N box_alloc=N ...)
parse_field() {
    local field="$1"
    echo "$CLONE_STATS_LINE" | awk -v f="$field" '{
        for (i=1; i<=NF; i++) {
            split($i, kv, "=")
            if (kv[1] == f) { print kv[2]; exit }
        }
    }'
}
ARRAY_CLONE=$(parse_field array_clone)
# ⚠ `string_clone` was MISSING from this snapshot until R47, while all four
# ceiling comments cited this script as the way to regenerate the STRING pin.
# The documented command could not produce the number it documented.
STRING_CLONE=$(parse_field string_clone)
MAP_CLONE=$(parse_field map_clone)
SET_CLONE=$(parse_field set_clone)
STRING_COW=$(parse_field string_cow)
STRING_CAT=$(parse_field string_cat)
BOX_ALLOC=$(parse_field box_alloc)
ARRAY_NEW=$(parse_field array_new)
STRING_NEW=$(parse_field string_new)
TOTAL_ALLOCS=$(parse_field total_allocs)
TOTAL_FREES=$(parse_field total_frees)
LIVE_BYTES=$(parse_field live_bytes)
# Prefer GNU time's peak RSS when available; fall back to /proc/self/status (Linux)
# captured inside the runtime at exit, embedded in the clone-stats line.
PEAK_RSS_STATS=$(parse_field peak_rss_kb)
if [[ -z "$PEAK_RSS_KB" && -n "$PEAK_RSS_STATS" ]]; then
    PEAK_RSS_KB=$PEAK_RSS_STATS
fi

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
GIT_REV=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
GIT_DIRTY=$(git diff --quiet HEAD 2>/dev/null && echo "false" || echo "true")

cat > "$OUT" <<EOF
{
  "timestamp": "$TIMESTAMP",
  "git_rev": "$GIT_REV",
  "git_dirty": $GIT_DIRTY,
  "workload": "self_host_lowerer/driver.gg (stage-0 on its own source, --lir-c)",
  "peak_rss_kb": ${PEAK_RSS_KB:-0},
  "user_time_s": ${USER_TIME:-0},
  "sys_time_s": ${SYS_TIME:-0},
  "wall_time": "${WALL_TIME:-"unknown"}",
  "page_faults_major": ${PAGE_FAULTS_MAJOR:-0},
  "page_faults_minor": ${PAGE_FAULTS_MINOR:-0},
  "clone_stats": {
    "array_clone": ${ARRAY_CLONE:-0},
    "string_clone": ${STRING_CLONE:-0},
    "map_clone":   ${MAP_CLONE:-0},
    "set_clone":   ${SET_CLONE:-0},
    "string_cow":  ${STRING_COW:-0},
    "string_cat":  ${STRING_CAT:-0},
    "box_alloc":   ${BOX_ALLOC:-0},
    "array_new":   ${ARRAY_NEW:-0},
    "string_new":  ${STRING_NEW:-0},
    "total_allocs": ${TOTAL_ALLOCS:-0},
    "total_frees":  ${TOTAL_FREES:-0},
    "live_bytes":   ${LIVE_BYTES:-0}
  }
}
EOF

echo "[4/4] Snapshot written to $OUT" >&2
echo >&2
cat "$OUT" >&2

# Optional: diff vs a baseline.
if [[ -n "$COMPARE_AGAINST" ]]; then
    if [[ ! -f "$COMPARE_AGAINST" ]]; then
        echo "ERROR: baseline $COMPARE_AGAINST not found" >&2
        exit 4
    fi
    echo >&2
    echo "=== Delta vs $COMPARE_AGAINST ===" >&2
    diff_field() {
        local key="$1"
        local path="$2"
        local base cur
        base=$(jq -r "$path" "$COMPARE_AGAINST")
        cur=$(jq -r "$path" "$OUT")
        if [[ -z "$base" || "$base" == "null" || -z "$cur" || "$cur" == "null" ]]; then
            return
        fi
        if [[ "$base" =~ ^[0-9]+$ && "$cur" =~ ^[0-9]+$ ]]; then
            local delta=$((cur - base))
            local pct=""
            if [[ "$base" -gt 0 ]]; then
                pct=$(awk -v d="$delta" -v b="$base" 'BEGIN{printf "%+.1f%%", (d/b)*100}')
            fi
            printf "  %-18s %12s → %-12s (%+d %s)\n" "$key" "$base" "$cur" "$delta" "$pct" >&2
        else
            printf "  %-18s %12s → %-12s\n" "$key" "$base" "$cur" >&2
        fi
    }
    diff_field "peak_rss_kb"  ".peak_rss_kb"
    diff_field "user_time_s"  ".user_time_s"
    diff_field "array_clone"  ".clone_stats.array_clone"
    diff_field "string_clone" ".clone_stats.string_clone"
    diff_field "map_clone"    ".clone_stats.map_clone"
    diff_field "set_clone"    ".clone_stats.set_clone"
    diff_field "string_cow"   ".clone_stats.string_cow"
    diff_field "string_cat"   ".clone_stats.string_cat"
    diff_field "box_alloc"    ".clone_stats.box_alloc"
    diff_field "array_new"    ".clone_stats.array_new"
    diff_field "total_allocs" ".clone_stats.total_allocs"
    diff_field "live_bytes"   ".clone_stats.live_bytes"
fi
