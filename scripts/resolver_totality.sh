#!/usr/bin/env bash
# scripts/resolver_totality.sh — resolver totality worklist (A + B).
#
# ⚠ WORKLIST GENERATOR, NEVER A CORRECTNESS GATE (Core #13).
# Some(wrong_root) counts as resolved. Instrument C (build-and-run) is the
# eventual soundness gate and is NOT invoked here.
#
# Outputs:
#   [resolver-census]     arm counts per resolver (from lint --nocapture)
#   [resolver-divergence] unexempted cell count (must be 0 / ≤ MAX)
#   [resolver-hist]       fall-through shapes (sample or aggregated sweep)
#   Resolver-totality: … / Resolver-totality-B: … dashboard lines
#
# Usage:
#   scripts/resolver_totality.sh              # A + B corpus + self-host (default)
#   scripts/resolver_totality.sh --sweep      # same as default
#   scripts/resolver_totality.sh --sample     # A + B one-fixture sample (fast)
#   scripts/resolver_totality.sh --with-hist  # alias for --sample (compat)
#   scripts/resolver_totality.sh --a-only     # A only (fast)
#   scripts/resolver_totality.sh --calibrate  # Gate 0 reference + sample re-run
#
# Env:
#   RESOLVER_HIST_FIXTURE   sample fixture (default cow_amp_projection_base_shapes.gg)
#   RESOLVER_SWEEP_GLOB     corpus glob (default tests/fixtures/*.gg — top-level only)
#   RESOLVER_SELFHOST_GG    self-host entry (default tests/fixtures/self_host_lowerer/driver.gg)
#   RESOLVER_SWEEP_OUT      where to write ranked TSV (default /tmp)
#   GG                      path to gg binary (default ./target/debug/gg)
#
# Non-compiling fixtures: counted as hist_missing (no lower → no B). Listed in
# $RESOLVER_SWEEP_OUT/resolver_totality_hist_missing.txt under --sweep.
#
# GATE 0 calibration (Round XVIII — blocking before ranking roots):
#   Procedure (mirrors A's three breaks; record before/after in the commit):
#     1. Baseline: gg build --emit-gir --resolvers=hist $RESOLVER_HIST_FIXTURE
#     2. Remove ONE None-exit resolver_miss hook that fires on that fixture
#        (try_resolve_place `_ =>` NoArm — src/ir/lowering/exprs/mod.rs)
#     3. Rebuild; total_misses must DROP and that shape must VANISH
#     4. Restore; baseline matches pre-break
#   Measured 2026-07-30 (cow_amp_projection_base_shapes.gg):
#     hook broken: try_place MissReason::NoArm fall-through (`_ =>` arm)
#     baseline total_misses=1  try_place NoArm Identifier
#     break    total_misses=0  shapes=0  (Identifier vanished)
#     restore  total_misses=1  try_place NoArm Identifier  (matches baseline)
#
# Numbers come ONLY from the lint tags and --resolvers=hist output — never a
# hand-grep of arm names in this script body.

set -euo pipefail
cd "$(dirname "$0")/.."

MODE="${1:-}"
A_ONLY=0
SWEEP=1
SAMPLE=0
CALIBRATE=0
case "$MODE" in
  --a-only) A_ONLY=1; SWEEP=0; SAMPLE=0 ;;
  --sample|--with-hist) A_ONLY=0; SWEEP=0; SAMPLE=1 ;;
  --sweep|"") A_ONLY=0; SWEEP=1; SAMPLE=0 ;;
  --calibrate) A_ONLY=0; SWEEP=0; SAMPLE=1; CALIBRATE=1 ;;
  -h|--help)
    sed -n '2,48p' "$0" | sed 's/^# \?//'
    exit 0
    ;;
  *)
    echo "usage: $0 [--a-only|--sample|--with-hist|--sweep|--calibrate]" >&2
    exit 2
    ;;
esac

echo "=== A: static census + divergence (cargo test --test lints) ==="
echo "⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."

A_LOG=$(mktemp /tmp/resolver_A_XXXXXX.log)
cargo test --test lints place_resolvers_arm_census_and_divergence -- --nocapture 2>&1 \
  | tee "$A_LOG"

# Family-2 SET-equality sibling (kept separate; do not replace).
cargo test --test lints field_and_tuple_place_resolvers_cover_the_same_object_forms -- --nocapture 2>&1 \
  | tee -a "$A_LOG"

CENSUS_LINE=$(grep -E '^\[resolver-census\]' "$A_LOG" | tail -1 || true)
DIV_LINE=$(grep -E '^\[resolver-divergence\]' "$A_LOG" | tail -1 || true)

if [[ -z "$CENSUS_LINE" || -z "$DIV_LINE" ]]; then
  echo "error: missing [resolver-census]/[resolver-divergence] tags from lint --nocapture" >&2
  exit 1
fi

echo "$CENSUS_LINE"
echo "$DIV_LINE"

ROOT=$(echo "$CENSUS_LINE" | sed -n 's/.*root=\([0-9]*\).*/\1/p')
PLACE=$(echo "$CENSUS_LINE" | sed -n 's/.*place=\([0-9]*\).*/\1/p')
FIELD=$(echo "$CENSUS_LINE" | sed -n 's/.*field=\([0-9]*\).*/\1/p')
TUPLE=$(echo "$CENSUS_LINE" | sed -n 's/.*tuple=\([0-9]*\).*/\1/p')
DIV=$(echo "$DIV_LINE" | sed -n 's/.*unexempted=\([0-9]*\).*/\1/p')

TOP_MISS="(skipped)"
TOTAL_MISSES="?"
SHAPES="?"
RESOLVERS_TOUCHED="?"

if [[ "$CALIBRATE" -eq 1 ]]; then
  echo ""
  echo "=== GATE 0: calibration reference (positive re-run only; break is manual) ==="
  echo "Procedure: remove one resolver_miss hook → total_misses DROP + shape VANISH → restore."
  echo "Recorded 2026-07-30: baseline 1 / break 0 / restore 1 on cow_amp_projection_base_shapes.gg"
  echo "  hook: try_place MissReason::NoArm  shape: Identifier"
fi

# Always return 0 so set -e callers keep going; hist may print even when
# emit-gir fails after lower. Never toggle set -e inside (shell-global).
run_one_hist() {
  local gg="$1" fixture="$2" out="$3"
  if "$gg" build --emit-gir --resolvers=hist "$fixture" >"$out" 2>&1; then
    return 0
  fi
  return 0
}

extract_hist_rows() {
  # stdin: gg --resolvers=hist log → stdout: count\tresolver\treason\tshape
  awk '
    /^root_local|^try_place|^field_place|^tuple_place|^ptr_field/ {
      res=$1; cnt=$2; reason=$3;
      shape=$4; for(i=5;i<=NF;i++) shape=shape" "$i
      print cnt "\t" res "\t" reason "\t" shape
    }
  '
}

if [[ "$A_ONLY" -eq 0 ]]; then
  cargo build -q
  GG="${GG:-./target/debug/gg}"

  if [[ "$SWEEP" -eq 1 ]]; then
    echo ""
    echo "=== B: SWEEP fall-through histogram (corpus + self-host) ==="
    echo "⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."
    echo "    (gg check never lowers — build path only.)"
    echo "    hist_missing = fixtures that never reached lower (typecheck/parse fail)."

    AGG=$(mktemp /tmp/resolver_B_agg_XXXXXX.tsv)
    : >"$AGG"
    FAIL_LOG=$(mktemp /tmp/resolver_B_fail_XXXXXX.log)
    : >"$FAIL_LOG"
    N_OK=0
    N_FAIL=0
    N_TOTAL=0

    # Corpus: tests/fixtures/*.gg only (top-level; not known_gaps/security subdirs
    # unless listed). Override with RESOLVER_SWEEP_GLOB.
    GLOB_PAT="${RESOLVER_SWEEP_GLOB:-tests/fixtures/*.gg}"
    # shellcheck disable=SC2086
    for f in $GLOB_PAT; do
      [[ -f "$f" ]] || continue
      N_TOTAL=$((N_TOTAL + 1))
      B_LOG=$(mktemp /tmp/resolver_B_one_XXXXXX.log)
      run_one_hist "$GG" "$f" "$B_LOG"
      if grep -qE '^\[resolver-hist\]' "$B_LOG"; then
        N_OK=$((N_OK + 1))
        extract_hist_rows <"$B_LOG" >>"$AGG"
      else
        N_FAIL=$((N_FAIL + 1))
        echo "$f" >>"$FAIL_LOG"
      fi
      rm -f "$B_LOG"
      if [[ $((N_TOTAL % 200)) -eq 0 ]]; then
        echo "  … $N_TOTAL fixtures (hist_ok=$N_OK hist_missing=$N_FAIL)"
      fi
    done

    echo "corpus: fixtures_seen=$N_TOTAL hist_ok=$N_OK hist_missing=$N_FAIL"

    # Self-host self-compile surface (not in top-level *.gg)
    SH_DRIVER="${RESOLVER_SELFHOST_GG:-tests/fixtures/self_host_lowerer/driver.gg}"
    SH_TOTAL_MISSES="?"
    if [[ -f "$SH_DRIVER" ]]; then
      echo ""
      echo "=== B: self-host driver ($SH_DRIVER) ==="
      SH_LOG=$(mktemp /tmp/resolver_B_sh_XXXXXX.log)
      run_one_hist "$GG" "$SH_DRIVER" "$SH_LOG"
      if grep -qE '^\[resolver-hist\]' "$SH_LOG"; then
        grep -E 'Resolver Fall-through|\[resolver-hist\]|^root_local|^try_place|^field_place|^tuple_place|^ptr_field' "$SH_LOG" \
          | head -40 || true
        extract_hist_rows <"$SH_LOG" >>"$AGG"
        SH_LINE=$(grep -E '^\[resolver-hist\]' "$SH_LOG" | tail -1 || true)
        echo "self-host $SH_LINE"
        SH_TOTAL_MISSES=$(echo "$SH_LINE" | sed -n 's/.*total_misses=\([0-9]*\).*/\1/p')
      else
        echo "warning: self-host hist missing (lower may have failed early)" >&2
        tail -5 "$SH_LOG" >&2 || true
      fi
      rm -f "$SH_LOG"
    else
      echo "warning: no self-host driver at $SH_DRIVER" >&2
    fi

    # Aggregate: sum counts by (resolver, reason, shape)
    echo ""
    echo "=== Aggregated ranked histogram (corpus + self-host) ==="
    echo "⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."
    RANKED=$(mktemp /tmp/resolver_B_rank_XXXXXX.txt)
    awk -F'\t' '
      NF>=4 {
        key=$2 "\t" $3 "\t" $4
        for(i=5;i<=NF;i++) key=key " " $i
        sum[key]+=$1
      }
      END {
        for (k in sum) print sum[k] "\t" k
      }
    ' "$AGG" | sort -t$'\t' -k1,1nr >"$RANKED"

    printf '%-14s %8s  %-14s  %s\n' "resolver" "count" "reason" "shape"
    head -40 "$RANKED" | while IFS=$'\t' read -r cnt res reason shape; do
      printf '%-14s %8s  %-14s  %s\n' "$res" "$cnt" "$reason" "$shape"
    done

    TOTAL_MISSES=$(awk -F'\t' '{s+=$1} END{print s+0}' "$RANKED")
    SHAPES=$(wc -l <"$RANKED" | tr -d ' ')
    RESOLVERS_TOUCHED=$(awk -F'\t' '{print $2}' "$RANKED" | sort -u | wc -l | tr -d ' ')
    TOP_MISS=$(head -1 "$RANKED" | awk -F'\t' '{print $4; for(i=5;i<=NF;i++) printf " %s",$i}')
    if [[ -z "$TOP_MISS" ]]; then TOP_MISS="(empty)"; fi

    echo "[resolver-hist] total_misses=${TOTAL_MISSES} resolvers_touched=${RESOLVERS_TOUCHED} shapes=${SHAPES} (aggregated)"
    echo "Resolver-totality-B: total_misses=${TOTAL_MISSES} shapes=${SHAPES} top=${TOP_MISS}"
    echo "  (corpus hist_ok=${N_OK}/${N_TOTAL} hist_missing=${N_FAIL} self_host_misses=${SH_TOTAL_MISSES})"

    # Persist for Step 2 partition (not committed)
    OUT_DIR="${RESOLVER_SWEEP_OUT:-/tmp}"
    {
      echo -e "count\tresolver\treason\tshape"
      cat "$RANKED"
    } >"${OUT_DIR}/resolver_totality_ranked.tsv"
    echo "wrote ${OUT_DIR}/resolver_totality_ranked.tsv"
    if [[ "$N_FAIL" -gt 0 ]]; then
      cp "$FAIL_LOG" "${OUT_DIR}/resolver_totality_hist_missing.txt"
      echo "hist_missing fixtures listed in ${OUT_DIR}/resolver_totality_hist_missing.txt ($N_FAIL)"
    fi
    rm -f "$AGG" "$RANKED" "$FAIL_LOG"
  else
    # Sample / calibrate: one fixture
    echo ""
    echo "=== B: fall-through histogram SAMPLE (gg build --emit-gir --resolvers=hist) ==="
    echo "⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."
    echo "    (gg check never lowers — build path only.)"
    echo "    (one-fixture sample; default/--sweep for corpus + self-host)"

    FIXTURE="${RESOLVER_HIST_FIXTURE:-tests/fixtures/cow_amp_projection_base_shapes.gg}"
    if [[ ! -f "$FIXTURE" ]]; then
      FIXTURE=$(ls tests/fixtures/cow_amp_*.gg 2>/dev/null | head -1 || true)
    fi
    if [[ -z "${FIXTURE:-}" || ! -f "$FIXTURE" ]]; then
      echo "warning: no sample fixture for B; skipping hist" >&2
      TOP_MISS="(no-fixture)"
    else
      B_LOG=$(mktemp /tmp/resolver_B_XXXXXX.log)
      run_one_hist "$GG" "$FIXTURE" "$B_LOG"
      grep -E 'Resolver Fall-through|\[resolver-hist\]|^root_local|^try_place|^field_place|^tuple_place|^ptr_field' "$B_LOG" \
        | head -50 || true
      HIST_LINE=$(grep -E '^\[resolver-hist\]' "$B_LOG" | tail -1 || true)
      if [[ -n "$HIST_LINE" ]]; then
        echo "$HIST_LINE"
        TOTAL_MISSES=$(echo "$HIST_LINE" | sed -n 's/.*total_misses=\([0-9]*\).*/\1/p')
        SHAPES=$(echo "$HIST_LINE" | sed -n 's/.*shapes=\([0-9]*\).*/\1/p')
        RESOLVERS_TOUCHED=$(echo "$HIST_LINE" | sed -n 's/.*resolvers_touched=\([0-9]*\).*/\1/p')
      fi
      TOP_MISS=$(awk '
        /^resolver[ ]+count/ { next }
        /^===/ { next }
        /^⚠/ { next }
        /^\[resolver-hist\]/ { next }
        NF>=4 && $1 ~ /^(root_local|try_place|field_place|tuple_place|ptr_field)$/ {
          shape=$4; for(i=5;i<=NF;i++) shape=shape" "$i
          print shape; exit
        }
      ' "$B_LOG" || true)
      if [[ -z "$TOP_MISS" ]]; then
        TOP_MISS="(empty-or-zero)"
      fi
      rm -f "$B_LOG"
    fi
  fi
fi

echo ""
echo "Resolver-totality: arms G1=${ROOT} G2=${PLACE} G3=${FIELD}/${TUPLE} · divergence=${DIV} · top_miss=${TOP_MISS}"
if [[ "$SWEEP" -eq 1 && "$TOTAL_MISSES" != "?" ]]; then
  echo "Resolver-totality-B: total_misses=${TOTAL_MISSES} shapes=${SHAPES} top=${TOP_MISS}"
fi
echo "(worklist only; not a correctness gate — Core #13)"
