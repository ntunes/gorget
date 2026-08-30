#!/usr/bin/env bash
# Load- and RAM-aware integration test runner.
#
# cargo test fixes its thread pool at launch, so this autoscales `--test-threads`
# ONCE at start from the current box state: N = min(free_cores, ram_budget, cap).
# A busy box (parallel agents, or heavy load) computes fewer free cores and so
# yields threads automatically — it never fights other work for the whole box.
#
# The binding constraint on this suite is RAM, not cores: each heavy self-host
# fixture peaks ~1.2-1.5 GB RSS, so we cap threads by available memory too, or an
# 8-thread run OOMs on a RAM-tight box even with cores idle.
#
# Usage:
#   scripts/run_integration.sh [cargo args...]          # e.g. --release, or a filter
#   GG_BACKEND=llvm scripts/run_integration.sh --release
#   GG_MAX_TEST_THREADS=6 scripts/run_integration.sh    # lower the cap by hand
#   GG_TEST_THREADS=1 scripts/run_integration.sh        # pin exactly (perf runs: pin
#                                                        # for reproducible wall-time)
#
# Env passthrough (GG_BACKEND, GG_RUNTIME_DIFF, GG_BUILD_TIMEOUT_SECS, …) is inherited.
set -euo pipefail

# PRE-FLIGHT. The autoscaling below reads /proc/loadavg, and so does every
# load-adjusted deadline inside the suite (tests/integration.rs's
# `env_or_load_adjusted_secs`). A test binary left spinning by a DEAD run
# therefore corrupts this run's numbers in BOTH directions at once: fewer
# threads AND longer deadlines, so a genuine hang can quietly pass. Measured,
# not theoretical -- one orphan burned a core for 40 hours here and skewed a
# whole round's measurements.
#
# The predicate is OWNERSHIP, never a name and never PPID: see
# scripts/reap_orphans.py's header for why both of the obvious ones are wrong
# (a name-matching pkill has already killed a live executor's run in this tree).
# It is a DRY RUN -- this never kills anything.
#
# REACH, stated rather than implied: this covers ONE of the round-close battery's
# entry points. The others -- the LLVM sweep (this same script with GG_BACKEND
# set, so covered), `cargo test --lib`, `--test lints`, `--test security`,
# `--test spec_conformance`, `-p ggdef`, and `python3 scripts/robustness_map.py`
# -- start on an unchecked box. `scripts/sanitize_sweep.sh` calls it too.
if [ -z "${GG_SKIP_PREFLIGHT:-}" ] && command -v python3 >/dev/null 2>&1; then
  if ! python3 "$(dirname "$0")/reap_orphans.py" --preflight; then
    echo "[run_integration] refusing to measure a poisoned box."
    exit 1
  fi
fi

NCPU=$(nproc)
CAP=${GG_MAX_TEST_THREADS:-8}

if [ -n "${GG_TEST_THREADS:-}" ]; then
  N=$GG_TEST_THREADS
  echo "[run_integration] pinned --test-threads=$N (GG_TEST_THREADS set)"
else
  LOAD=$(awk '{print $1}' /proc/loadavg)                          # 1-min load average
  FREE_CORES=$(awk -v n="$NCPU" -v l="$LOAD" 'BEGIN{f=n-l; if(f<1)f=1; printf "%d", f}')
  FREE_RAM_G=$(awk '/MemAvailable/{printf "%.1f", $2/1024/1024}' /proc/meminfo)
  RAM_CAP=$(awk -v r="$FREE_RAM_G" 'BEGIN{c=int(r/1.5); if(c<1)c=1; print c}')  # ~1.5G/thread
  N=$FREE_CORES
  [ "$RAM_CAP" -lt "$N" ] && N=$RAM_CAP
  [ "$CAP"     -lt "$N" ] && N=$CAP
  [ "$N" -lt 1 ] && N=1
  echo "[run_integration] nproc=$NCPU load=$LOAD free_cores≈$FREE_CORES free_ram=${FREE_RAM_G}G ram_cap=$RAM_CAP cap=$CAP → --test-threads=$N"
fi

exec cargo test --test integration "$@" -- --test-threads="$N"
