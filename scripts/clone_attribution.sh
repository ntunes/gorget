#!/usr/bin/env bash
#
# scripts/clone_attribution.sh — ranked per-CloneId RUNTIME clone attribution.
#
# Joins the two halves the compiler already emits (docs/devbook/11-copy-on-write.md
# § "Observability — --clones and per-site attribution"):
#
#   static  : `gg build --clones=sites-tsv=PATH` → one row per CloneId:
#             id, file, line, col, type, reason (the typed ImplicitCloneReason —
#             the planner's reason tag), size_bytes, runtime_fn
#   runtime : the same build with `--clones=stats` → the compiled binary prints
#             `[clone-site] #id=count` lines at exit (GG_CLONE_SITES_TOP=0 = all)
#
# joined on the CloneId column into a table ranked by dynamic count, plus a
# by-reason rollup. Pure offline composition — ZERO compiler change: the
# compiler emits typed halves, this tool joins them (devbook/24 rule 4:
# resolve once, write through — the reason is read off the typed site table,
# never re-derived).
#
# COVERAGE CAVEAT — read before quoting numbers:
#   The ranked counts are TOP-LEVEL implicit-clone EVENTS (every
#   `warn_implicit_clone` site, lint-paired with its runtime hit via
#   `clone_warn_hit_pairing`). They are NOT the leaf clone volume: on the
#   self-host self-compile the instrument attributes ~3.5% of the aggregate
#   array_clone+string_clone call count, because
#     (a) synthesized deep `Type__clone` fns cascade — one site hit expands
#         into N inner gorget_array_clone/gorget_string_clone leaf calls;
#     (b) explicit `.clone()` (ExplicitUserClone) tags the instruction but
#         mints no CloneId/hit (the "N implicit clones" report contract);
#     (c) LIR-layer clone emissions have no CloneId concept yet (the
#         documented un-attributed residual, devbook/11 § Observability).
#   The ranking is still the actionable lever: killing a top-level clone
#   removes its ENTIRE cascade. Extensions (tracked in TODO.md): v2 = mint
#   CloneIds for the LIR-layer emissions (+ optionally ExplicitUserClone
#   sites, kept OUT of the implicit-clone report); v3 = runtime-callsite
#   tagging on the clone runtime fns for full leaf-volume attribution.
#
# Overhead: `--clones=stats` instrumentation measured at ~1% wall on the
# self-compile workload (2026-07-19) — cheap enough to run routinely.
#
# Usage:
#   scripts/clone_attribution.sh <gg> <target.gg> [build-args...] -- [run-args...]
#
#   TOPN=N   widens/narrows the ranked table (default 20).
#
# Example — the canonical self-compile clone workload (same as the
# `self_host_clone_ceiling` test and `scripts/self_host_mem_baseline.sh`):
#   scripts/clone_attribution.sh ./target/release/gg \
#       tests/fixtures/self_host_lowerer/driver.gg -o /tmp/drv -- \
#       tests/fixtures/self_host_lowerer/driver.gg "$PWD/lib" --lir-c

set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "usage: $0 <gg> <target.gg> [build-args...] -- [run-args...]" >&2
    exit 2
fi
GG="$1"; TARGET="$2"; shift 2
BUILD_ARGS=(); RUN_ARGS=(); seen_sep=0
for a in "$@"; do
    if [[ "$a" == "--" ]]; then seen_sep=1; continue; fi
    if [[ $seen_sep -eq 0 ]]; then BUILD_ARGS+=("$a"); else RUN_ARGS+=("$a"); fi
done
TOPN="${TOPN:-20}"
WORK="$(mktemp -d /tmp/clone_attr.XXXXXX)"
SITES="$WORK/sites.tsv"; RUNLOG="$WORK/run.stderr"

# Find -o in the build args; synthesize an exe path when absent.
EXE=""
for ((i=0; i<${#BUILD_ARGS[@]}; i++)); do
    if [[ "${BUILD_ARGS[$i]}" == "-o" ]]; then EXE="${BUILD_ARGS[$((i+1))]}"; fi
done
if [[ -z "$EXE" ]]; then EXE="$WORK/exe"; BUILD_ARGS+=(-o "$EXE"); fi

echo "[1/3] build --clones=stats --clones=sites-tsv (static site table + runtime hits)" >&2
"$GG" build --clones=stats "--clones=sites-tsv=$SITES" "$TARGET" \
    ${BUILD_ARGS[@]+"${BUILD_ARGS[@]}"} >&2

echo "[2/3] run target under GG_CLONE_SITES_TOP=0 (all nonzero sites)" >&2
set +e
GG_CLONE_SITES_TOP=0 "$EXE" ${RUN_ARGS[@]+"${RUN_ARGS[@]}"} >/dev/null 2>"$RUNLOG"
set -e

echo "[3/3] join on CloneId → ranked table" >&2
grep '^\[clone-stats\]' "$RUNLOG" | tail -1 >&2 || true
awk -v topn="$TOPN" '
  # pass 1: sites.tsv  id \t file \t line \t col \t type \t reason \t size \t fn
  FNR==NR { split($0, f, "\t"); id = f[1];
            loc[id] = f[2] ":" f[3] ":" f[4]; typ[id] = f[5]; rsn[id] = f[6];
            sz[id] = f[7]; fn[id] = f[8]; next }
  # pass 2: [clone-site] #id=count
  /^\[clone-site\] #/ { s = $0; sub(/^\[clone-site\] #/, "", s); split(s, kv, "=");
                        cnt[kv[1]] = kv[2] + 0; ids[kv[1]] = 1 }
  END {
    n = 0; for (i in ids) { arr[n++] = i }
    # sort by count desc (insertion sort — N is the site count, small)
    for (a = 0; a < n; a++) for (b = a + 1; b < n; b++)
        if (cnt[arr[b]] + 0 > cnt[arr[a]] + 0) { t = arr[a]; arr[a] = arr[b]; arr[b] = t }
    printf "%-9s %-14s %-28s %-22s %-7s %s\n", "count", "reason", "runtime_fn", "type", "id", "location";
    tot = 0; for (a = 0; a < n; a++) tot += cnt[arr[a]]
    shown = 0;
    for (a = 0; a < n && shown < topn; a++) { id = arr[a];
      printf "%-9d %-14.14s %-28.28s %-22.22s %-7s %s\n",
             cnt[id], rsn[id], fn[id], typ[id], id, loc[id]; shown++ }
    printf "--- total_site_hits=%d across %d sites (top %d shown; TOPN=N widens) ---\n", tot, n, shown;
    # by-reason rollup — the planner-facing costing view
    for (a = 0; a < n; a++) { id = arr[a]; rc[rsn[id]] += cnt[id] }
    printf "\n== by reason ==\n";
    for (r in rc) printf "%-30s %d\n", r, rc[r] | "sort -k2 -rn"
  }
' "$SITES" "$RUNLOG"
echo "(work dir kept for inspection: $WORK)" >&2
