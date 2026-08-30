#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════
# scripts/clone_meter.sh — the BASH accessor for `scripts/clone_meter.spec`.
# ═══════════════════════════════════════════════════════════════════════════
#
# Source this; do not run it. Every bash instrument that reads the clone meter
# (`self_host_mem_baseline.sh`, `bench_stages.sh`, `clone_meter_probe.sh`)
# builds its invocation through these functions rather than spelling the driver
# path, the flags or the argv order itself — so the meter has ONE definition
# (Layering rule 3). The Rust gate reads the same spec through
# `fn clone_meter_spec()` in `tests/integration.rs`.
#
# ⚠ `tests/lints.rs::clone_meter_instruments_read_the_declared_spec` FAILS if an
#   instrument spells the invocation itself again. That lint is the mechanism;
#   this comment is not.

# Repo root, independent of where the caller cd'd to.
CLONE_METER_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CLONE_METER_SPEC="$CLONE_METER_ROOT/scripts/clone_meter.spec"

# clone_meter_get <key> — the declared value of a single-valued spec key.
# Fails loud on a missing key: a silently-empty argv is how the drift started.
clone_meter_get() {
    local key="$1" val
    val=$(sed -n "s/^[[:space:]]*${key}[[:space:]]*=[[:space:]]*//p" "$CLONE_METER_SPEC" | head -1)
    if [[ -z "$val" ]]; then
        echo "clone_meter: no key '$key' in $CLONE_METER_SPEC" >&2
        return 1
    fi
    printf '%s' "$val"
}

# clone_meter_build <gg-binary> <out-exe> <stderr-log>
# Builds the instrumented self-host driver — the meter's producer. cwd is forced
# to the repo root and the driver path is spelled repo-relative, which is what
# makes the number independent of where the checkout lives.
clone_meter_build() {
    local gg="$1" out="$2" errlog="$3"
    local driver build_args
    driver=$(clone_meter_get driver)
    build_args=$(clone_meter_get build_args)
    ( cd "$CLONE_METER_ROOT" && "$gg" build $build_args "$driver" -o "$out" ) 2>"$errlog"
}

# clone_meter_run <driver-exe> <stdout-target> <stderr-log>
# Runs the canonical clone workload: the driver compiling its own source.
# <stdout-target> is a caller argument rather than a spec constant because the
# emitted C is the deliverable of some instruments (bench_stages.sh) and noise
# to others — and the spec records that this axis was MEASURED to move the
# counters by exactly zero.
clone_meter_run() {
    local exe="$1" stdout_to="$2" errlog="$3"
    local driver lib run_args
    driver=$(clone_meter_get driver)
    lib=$(clone_meter_get lib)
    run_args=$(clone_meter_get run_args)
    ( cd "$CLONE_METER_ROOT" && "$exe" "$driver" "$lib" $run_args ) >"$stdout_to" 2>"$errlog"
}

# clone_meter_counter <stderr-log> <counter-name> — a field of the last
# [clone-stats] line, or "-" when the line is absent (usually a build that
# forgot --clones=stats, or a stage C assembled without the instrumented
# preamble).
clone_meter_counter() {
    local errlog="$1" name="$2" v
    v=$(grep '^\[clone-stats\]' "$errlog" 2>/dev/null | tail -1 | tr ' ' '\n' |
        sed -n "s/^${name}=//p" | head -1)
    printf '%s' "${v:--}"
}
