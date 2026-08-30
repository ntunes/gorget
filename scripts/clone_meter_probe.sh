#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════
# scripts/clone_meter_probe.sh — regenerate the two tables in
# `scripts/clone_meter.spec` that nobody should have to re-derive by argument.
# ═══════════════════════════════════════════════════════════════════════════
#
#   scripts/clone_meter_probe.sh --axes      # does any suspected input move
#                                            # the meter?  (~6 runs, ~12 min)
#   scripts/clone_meter_probe.sh --closure   # which files does the workload
#                                            # actually OPEN?  (~1 min, strace)
#
# WHY IT EXISTS. R47 shipped a ceiling pinned from one instrument while its own
# documented regeneration command used another, and the two disagreed by 294
# clones on one axis. "Close enough" is not an answer to that; a measured table
# of what does and does not move the meter is. Every row of the spec's
# "inputs that are not inputs" table comes from `--axes`; the closure block
# comes from `--closure`.
#
# Requires `./target/release/gg` (and, for the profile row, `./target/debug/gg`).

set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
# shellcheck source=scripts/clone_meter.sh
source "$ROOT/scripts/clone_meter.sh"

W=$(mktemp -d /tmp/clone_meter_probe.XXXXXX)
trap 'rm -rf "$W"' EXIT
DRIVER_REL=$(clone_meter_get driver)
DRIVER_ABS="$ROOT/$DRIVER_REL"
LIB_REL=$(clone_meter_get lib)
LIB_ABS="$ROOT/$LIB_REL"
RUN_ARGS=$(clone_meter_get run_args)

# report <label> <errlog> — the two pinned counters off a run's stderr.
report() {
    printf '%-34s array_clone=%-12s string_clone=%s\n' \
        "$1" "$(clone_meter_counter "$2" array_clone)" "$(clone_meter_counter "$2" string_clone)"
}

axes() {
    [[ -x ./target/release/gg ]] || { echo "need ./target/release/gg (cargo build --release)" >&2; exit 2; }

    echo "── the meter's PRODUCER: does the build-time argv spelling change the program? ──"
    clone_meter_build ./target/release/gg "$W/drvREL" "$W/b1.err" || { tail -5 "$W/b1.err"; exit 1; }
    # The one deliberately OFF-SPEC build: absolute driver path, to show it is
    # equivalent rather than to bless it.
    ./target/release/gg build --clones=stats "$DRIVER_ABS" -o "$W/drvABS" 2>"$W/b2.err" >/dev/null
    md5sum "$W/drvREL.c" "$W/drvABS.c"
    echo "   (identical md5 ⇒ build-time argv spelling is not an input)"

    if [[ -x ./target/debug/gg ]]; then
        clone_meter_build ./target/debug/gg "$W/drvDBG" "$W/b3.err"
        md5sum "$W/drvREL.c" "$W/drvDBG.c"
        echo "   (identical md5 ⇒ the gg BUILD PROFILE is not an input)"
    else
        echo "   (no ./target/debug/gg — profile row SKIPPED, not measured)"
    fi

    echo "── the meter's RUN: which run-time axis moves a counter? ──"
    clone_meter_run "$W/drvREL" /dev/null "$W/r_spec.err";  report "declared invocation" "$W/r_spec.err"
    ( cd "$ROOT" && "$W/drvREL" "$DRIVER_ABS" "$LIB_ABS" $RUN_ARGS ) >/dev/null 2>"$W/r_abs.err"
    report "driver+lib argv ABSOLUTE" "$W/r_abs.err"
    clone_meter_run "$W/drvREL" "$W/stage1.c" "$W/r_file.err"; report "stdout to a FILE" "$W/r_file.err"
    if [[ -x ./target/debug/gg && -x "$W/drvDBG" ]]; then
        clone_meter_run "$W/drvDBG" /dev/null "$W/r_dbg.err"; report "driver built by DEBUG gg" "$W/r_dbg.err"
    fi
    echo "Any row differing from the first is an UNDECLARED INPUT: declare it in"
    echo "scripts/clone_meter.spec or normalise it away. Do not average them."
}

closure() {
    command -v strace >/dev/null || { echo "closure probe needs strace" >&2; exit 2; }
    [[ -x "$W/drvREL" ]] || clone_meter_build ./target/release/gg "$W/drvREL" "$W/b1.err" \
        || { tail -5 "$W/b1.err"; exit 1; }
    # The loader opens every source it needs in the first seconds; 60s is far
    # past that and far short of the ~120s full run.
    timeout 60 strace -f -e trace=openat -o "$W/trace.txt" \
        "$W/drvREL" "$DRIVER_REL" "$LIB_REL" $RUN_ARGS >/dev/null 2>/dev/null
    awk '/openat/ && !/-1 ENOENT/ {match($0, /"[^"]*"/); if (RSTART) print substr($0, RSTART+1, RLENGTH-2)}' \
        "$W/trace.txt" | sort -u > "$W/opened.txt"
    # Canonicalise through realpath so a file reached via the symlink seam is
    # recorded where it really lives — that IS the declaration.
    while read -r p; do
        case "$p" in /*) rp=$(realpath -m "$p" 2>/dev/null) ;;
                      *)  rp=$(realpath -m "$ROOT/$p" 2>/dev/null) ;; esac
        [[ -f "$rp" ]] && echo "${rp#"$ROOT"/}"
    done < "$W/opened.txt" | sort -u > "$W/closure.txt"
    grep '\.gg$' "$W/closure.txt" > "$W/closure_gg.txt"
    echo "closure_all_files = $(wc -l < "$W/closure.txt")"
    echo "closure_gg_files  = $(wc -l < "$W/closure_gg.txt")"
    echo "closure_gg_lines  = $(xargs -a "$W/closure_gg.txt" cat | wc -l)"
    echo "── .gg by directory ──"
    sed 's|/[^/]*$||' "$W/closure_gg.txt" | sort | uniq -c
    echo "── non-.gg by directory ──"
    grep -v '\.gg$' "$W/closure.txt" | sed 's|/[^/]*$||' | sort | uniq -c
}

case "${1:---axes}" in
    --axes)    axes ;;
    --closure) closure ;;
    --all)     axes; closure ;;
    *) echo "usage: $0 [--axes|--closure|--all]" >&2; exit 2 ;;
esac
