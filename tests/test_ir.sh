#!/usr/bin/env bash
# Test all .gg fixtures through the --ir pipeline, compare output with non-IR pipeline.
# Usage: bash tests/test_ir.sh [filter]

set -o pipefail
PASS=0 MISMATCH=0 BUILD_FAIL=0 SKIP=0 TOTAL=0
FILTER="${1:-}"

for gg in tests/fixtures/*.gg; do
    stem=$(basename "$gg" .gg)
    [[ -n "$FILTER" && "$stem" != *"$FILTER"* ]] && continue
    TOTAL=$((TOTAL + 1))

    # Get expected output from non-IR build
    expected_file="tests/fixtures/${stem}.expected"
    if [[ -f "$expected_file" ]]; then
        expected=$(cat "$expected_file")
    else
        # Build with normal pipeline to get expected output
        # Normal build ignores -o, outputs to tests/fixtures/${stem}
        build_out=$(cargo run --quiet -- build "$gg" 2>&1)
        if [[ $? -ne 0 ]]; then
            SKIP=$((SKIP + 1))
            continue
        fi
        ref_bin="tests/fixtures/${stem}"
        if [[ ! -x "$ref_bin" ]]; then
            SKIP=$((SKIP + 1))
            continue
        fi
        expected=$("$ref_bin" 2>/dev/null || true)
    fi

    # Build with IR pipeline
    ir_build=$(cargo run --quiet -- build --ir "$gg" -o "/tmp/${stem}_ir" 2>&1)
    if [[ $? -ne 0 ]]; then
        BUILD_FAIL=$((BUILD_FAIL + 1))
        echo "BUILD_FAIL: $stem"
        continue
    fi

    # Run IR binary
    actual=$("/tmp/${stem}_ir" 2>/dev/null || true)

    if [[ "$actual" == "$expected" ]]; then
        PASS=$((PASS + 1))
    else
        MISMATCH=$((MISMATCH + 1))
        echo "MISMATCH: $stem"
    fi
done

echo ""
echo "=== Results ==="
echo "PASS: $PASS / $TOTAL"
echo "MISMATCH: $MISMATCH"
echo "BUILD_FAIL: $BUILD_FAIL"
echo "SKIP: $SKIP"
