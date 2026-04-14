#!/bin/bash
# Test all fixtures through --backend=llvm and categorize results
# Usage: bash tests/test_llvm.sh [fixture_name_filter]

GG="./target/release/gg"
FIXTURE_DIR="tests/fixtures"
FILTER="${1:-}"

PASS=0
FAIL=0
CRASH=0
BUILD_FAIL=0
SKIP=0
TOTAL=0

FAIL_LIST=""
CRASH_LIST=""
BUILD_FAIL_LIST=""

for gg_file in "$FIXTURE_DIR"/*.gg; do
    name=$(basename "$gg_file" .gg)

    # Apply filter if provided
    if [ -n "$FILTER" ] && [[ "$name" != *"$FILTER"* ]]; then
        continue
    fi

    stdout_file="$FIXTURE_DIR/$name.stdout"

    # Skip if no .stdout file (error tests, benchmarks, etc.)
    if [ ! -f "$stdout_file" ]; then
        SKIP=$((SKIP + 1))
        continue
    fi

    TOTAL=$((TOTAL + 1))

    # First check if it builds with C backend (skip if it doesn't)
    c_build_output=$($GG build "$gg_file" 2>&1)
    c_build_exit=$?
    if [ $c_build_exit -ne 0 ]; then
        SKIP=$((SKIP + 1))
        TOTAL=$((TOTAL - 1))
        continue
    fi

    # Build with LLVM backend
    build_output=$($GG build --backend=llvm "$gg_file" 2>&1)
    build_exit=$?

    if [ $build_exit -ne 0 ]; then
        BUILD_FAIL=$((BUILD_FAIL + 1))
        # Extract the key error
        error_line=$(echo "$build_output" | grep -E "error:|undefined|LLVM|llc" | head -1)
        BUILD_FAIL_LIST="$BUILD_FAIL_LIST\n  BUILD_FAIL: $name -- $error_line"
        continue
    fi

    # Run the binary
    binary="${gg_file%.gg}"
    if [ ! -f "$binary" ]; then
        # Try common output locations
        binary="./target/gorget/$name"
        if [ ! -f "$binary" ]; then
            BUILD_FAIL=$((BUILD_FAIL + 1))
            BUILD_FAIL_LIST="$BUILD_FAIL_LIST\n  BUILD_FAIL: $name -- binary not found"
            continue
        fi
    fi

    run_output=$(timeout 10 "$binary" 2>/dev/null)
    run_exit=$?

    if [ $run_exit -eq 124 ]; then
        # Timeout
        CRASH=$((CRASH + 1))
        CRASH_LIST="$CRASH_LIST\n  CRASH: $name (timeout)"
        continue
    fi
    if [ $run_exit -ge 128 ]; then
        # Signal (segfault=139, abort=134)
        sig=$((run_exit - 128))
        CRASH=$((CRASH + 1))
        CRASH_LIST="$CRASH_LIST\n  CRASH: $name (signal $sig)"
        continue
    fi

    # Compare stdout (trim whitespace like the integration test harness)
    actual_trimmed=$(echo "$run_output" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
    expected_trimmed=$(cat "$stdout_file" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
    if [ "$actual_trimmed" = "$expected_trimmed" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST\n  FAIL: $name"
    fi
done

echo "=== LLVM Backend Test Results ==="
echo "PASS: $PASS / $TOTAL"
echo "FAIL: $FAIL"
echo "CRASH: $CRASH"
echo "BUILD_FAIL: $BUILD_FAIL"
echo "SKIP: $SKIP"
echo ""

if [ -n "$BUILD_FAIL_LIST" ]; then
    echo "--- BUILD FAILURES ---"
    echo -e "$BUILD_FAIL_LIST"
    echo ""
fi

if [ -n "$CRASH_LIST" ]; then
    echo "--- CRASHES ---"
    echo -e "$CRASH_LIST"
    echo ""
fi

if [ -n "$FAIL_LIST" ]; then
    echo "--- OUTPUT MISMATCHES ---"
    echo -e "$FAIL_LIST"
    echo ""
fi
