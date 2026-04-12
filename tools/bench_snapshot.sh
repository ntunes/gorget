#!/bin/bash
# Usage: tools/bench_snapshot.sh [label]
# Runs all bench fixtures, saves results to benchmarks/<label>.txt
# Compare with: tools/bench_compare.py benchmarks/a.txt benchmarks/b.txt

set -e

LABEL="${1:-$(git rev-parse --short HEAD)}"
DIR="benchmarks"
mkdir -p "$DIR"
OUT="$DIR/$LABEL.txt"

echo "# Gorget benchmark snapshot: $LABEL" > "$OUT"
echo "# Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$OUT"
echo "# Commit: $(git rev-parse HEAD)" >> "$OUT"
echo "" >> "$OUT"

FIXTURES=(
    "tests/fixtures/bench_string.gg"
    "tests/fixtures/bench_string_methods.gg"
    "tests/fixtures/bench_collections.gg"
    "tests/fixtures/bench_compute.gg"
)

for fixture in "${FIXTURES[@]}"; do
    name=$(basename "$fixture" .gg)
    echo "Running $name..."
    cargo run --release --quiet -- test --bench "$fixture" 2>&1 >> "$OUT"
    echo "" >> "$OUT"
done

echo "Saved to $OUT"
echo ""
echo "To compare: python3 tools/bench_compare.py $OUT <other_snapshot>"
