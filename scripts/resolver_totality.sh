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
#   [resolver-hist]       top fall-through shapes over a fixture sample
#
# Usage:
#   scripts/resolver_totality.sh              # A + B sample (default)
#   scripts/resolver_totality.sh --a-only     # A only (fast)
#   scripts/resolver_totality.sh --with-hist  # A + B (same as default)
#
# Dashboard line (paste beside Convergence:):
#   Resolver-totality: arms G1=… G2=… G3=…/… · divergence=N · top_miss=…
#
# Numbers come ONLY from the lint tags and --resolvers=hist output — never a
# hand-grep of arm names in this script body.

set -euo pipefail
cd "$(dirname "$0")/.."

MODE="${1:-}"
A_ONLY=0
if [[ "$MODE" == "--a-only" ]]; then
  A_ONLY=1
fi

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

# Parse arm counts from census line:
# [resolver-census] root=6 place=4 field=6 tuple=6 ptr_expr=0 ptr_callers=2
ROOT=$(echo "$CENSUS_LINE" | sed -n 's/.*root=\([0-9]*\).*/\1/p')
PLACE=$(echo "$CENSUS_LINE" | sed -n 's/.*place=\([0-9]*\).*/\1/p')
FIELD=$(echo "$CENSUS_LINE" | sed -n 's/.*field=\([0-9]*\).*/\1/p')
TUPLE=$(echo "$CENSUS_LINE" | sed -n 's/.*tuple=\([0-9]*\).*/\1/p')
DIV=$(echo "$DIV_LINE" | sed -n 's/.*unexempted=\([0-9]*\).*/\1/p')

TOP_MISS="(skipped)"
if [[ "$A_ONLY" -eq 0 ]]; then
  echo ""
  echo "=== B: fall-through histogram (gg build --emit-gir --resolvers=hist) ==="
  echo "⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."
  echo "    (gg check never lowers — build path only.)"

  cargo build -q
  GG="${GG:-./target/debug/gg}"
  FIXTURE="${RESOLVER_HIST_FIXTURE:-tests/fixtures/cow_amp_projection_base_shapes.gg}"
  if [[ ! -f "$FIXTURE" ]]; then
    # Fallback sample if the preferred fixture is renamed.
    FIXTURE=$(ls tests/fixtures/cow_amp_*.gg 2>/dev/null | head -1 || true)
  fi
  if [[ -z "${FIXTURE:-}" || ! -f "$FIXTURE" ]]; then
    echo "warning: no sample fixture for B; skipping hist" >&2
    TOP_MISS="(no-fixture)"
  else
    B_LOG=$(mktemp /tmp/resolver_B_XXXXXX.log)
    # emit-gir avoids full cc when possible; still runs lower.
    set +e
    "$GG" build --emit-gir --resolvers=hist "$FIXTURE" >"$B_LOG" 2>&1
    B_EC=$?
    set -e
    # Build may fail on emit-only paths for some fixtures; hist still prints after lower.
    grep -E 'Resolver Fall-through|\[resolver-hist\]|^root_local|^try_place|^field_place|^tuple_place|^ptr_field' "$B_LOG" \
      | head -50 || true
    HIST_LINE=$(grep -E '^\[resolver-hist\]' "$B_LOG" | tail -1 || true)
    if [[ -n "$HIST_LINE" ]]; then
      echo "$HIST_LINE"
    fi
    # First data row after the header columns (skip banner).
    TOP_MISS=$(awk '
      /^resolver[ ]+count/ { next }
      /^===/ { next }
      /^⚠/ { next }
      /^\[resolver-hist\]/ { next }
      NF>=4 && $1 ~ /^(root_local|try_place|field_place|tuple_place|ptr_field)$/ {
        # shape is last field(s); reconstruct from $4..
        shape=$4; for(i=5;i<=NF;i++) shape=shape" "$i
        print shape; exit
      }
    ' "$B_LOG" || true)
    if [[ -z "$TOP_MISS" ]]; then
      TOP_MISS="(empty-or-zero)"
    fi
    # Non-zero exit from gg is informational for B (default never gates CI).
    if [[ "$B_EC" -ne 0 && "${STRICT_HIST:-0}" == "1" ]]; then
      echo "error: STRICT_HIST=1 and gg exited $B_EC" >&2
      exit "$B_EC"
    fi
  fi
fi

echo ""
echo "Resolver-totality: arms G1=${ROOT} G2=${PLACE} G3=${FIELD}/${TUPLE} · divergence=${DIV} · top_miss=${TOP_MISS}"
echo "(worklist only; not a correctness gate — Core #13)"
