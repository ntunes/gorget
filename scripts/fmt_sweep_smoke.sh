#!/usr/bin/env bash
#
# scripts/fmt_sweep_smoke.sh — Round XXXVI FMT-C manual sweep smoke.
#
# Runs `gg fmt --in-place` over the D27-target corpus (the 8 directories
# that R35's Wave-plan C3 aborted on), then runs the self-host bootstrap
# fixed-point test to confirm the corpus still bootstraps after the sweep.
#
# This is the missing gate that would have caught R35's fmt sweep BEFORE
# it broke bootstrap. Run MANUALLY before a D27 Round A retry (or any
# other sweep-of-formatter-changes round) — NOT part of the default CI
# battery, since it mutates the working tree in-place.
#
# Safety: the script COPIES each corpus into a scratch dir first, runs
# fmt there, and shows a diff summary. It NEVER writes to the real corpus
# unless you pass --apply (which is deliberately awkward).
#
# Usage:
#   scripts/fmt_sweep_smoke.sh                # dry-run: fmt into scratch, diff, keep scratch for inspection
#   scripts/fmt_sweep_smoke.sh --check        # dry-run + fail if any file would change
#   scripts/fmt_sweep_smoke.sh --apply        # write formatted output back to the real corpus (DESTRUCTIVE)
#   scripts/fmt_sweep_smoke.sh --skip-bootstrap  # skip the bootstrap fixed-point test (fast diff-only mode)
#
# Requires: cargo (for `cargo build --release` of the `gg` binary and
# `cargo test --release self_host_bootstrap_fixed_point`).

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

APPLY=0
CHECK=0
SKIP_BOOTSTRAP=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --apply) APPLY=1; shift ;;
        --check) CHECK=1; shift ;;
        --skip-bootstrap) SKIP_BOOTSTRAP=1; shift ;;
        -h|--help)
            head -n 22 "$0" | tail -n +2
            exit 0
            ;;
        *) echo "Unknown flag: $1" >&2; exit 2 ;;
    esac
done

# The D27 sweep target corpora (same as R35's Wave-plan C3 originally hit).
# Each entry is a directory relative to the repo root. Directories that
# don't exist in this checkout are skipped with a note (self-host layouts
# vary slightly across trees).
CORPORA=(
    "tests/fixtures"
    "lib"
    "spec"
    "examples"
    "demo"
    "spectests"
    "compiler/data"
)

echo "== 1/3 Building release gg =="
cargo build --release --bin gg

GG="$REPO_ROOT/target/release/gg"
[[ -x "$GG" ]] || { echo "gg binary not found: $GG" >&2; exit 1; }

SCRATCH="$(mktemp -d -t fmt_sweep.XXXXXX)"
trap 'echo "scratch kept at: $SCRATCH"' EXIT

echo ""
echo "== 2/3 Formatting corpus into $SCRATCH =="
total_files=0
changed_files=0
skip_corpora=0
for corpus in "${CORPORA[@]}"; do
    if [[ ! -d "$REPO_ROOT/$corpus" ]]; then
        echo "  skip: $corpus (missing)"
        skip_corpora=$((skip_corpora + 1))
        continue
    fi
    dest="$SCRATCH/$corpus"
    mkdir -p "$dest"
    # Copy corpus into scratch. Preserve subdirectory structure.
    (cd "$REPO_ROOT/$corpus" && find . -name "*.gg" -print) | while read -r f; do
        mkdir -p "$(dirname "$dest/$f")"
        cp "$REPO_ROOT/$corpus/$f" "$dest/$f"
    done
    # Run fmt on each .gg file in the copy.
    files_here=0
    changed_here=0
    while read -r f; do
        files_here=$((files_here + 1))
        if ! "$GG" fmt --in-place "$dest/$f" 2>/dev/null; then
            echo "  ⚠ fmt refused: $corpus/$f (unreadable / parse error)"
            continue
        fi
        if ! diff -q "$REPO_ROOT/$corpus/$f" "$dest/$f" >/dev/null 2>&1; then
            changed_here=$((changed_here + 1))
        fi
    done < <(cd "$dest" && find . -name "*.gg" -print)
    total_files=$((total_files + files_here))
    changed_files=$((changed_files + changed_here))
    printf "  %-30s files=%d changed=%d\n" "$corpus" "$files_here" "$changed_here"
done

echo ""
echo "  Corpora skipped:      $skip_corpora"
echo "  Total files touched:  $total_files"
echo "  Files changed by fmt: $changed_files"

if [[ "$CHECK" == "1" && "$changed_files" != "0" ]]; then
    echo "  --check: $changed_files file(s) would change; failing (as instructed)."
    exit 1
fi

echo ""
if [[ "$SKIP_BOOTSTRAP" == "1" ]]; then
    echo "== 3/3 Bootstrap fixed-point (skipped: --skip-bootstrap) =="
else
    echo "== 3/3 Bootstrap fixed-point (real corpus, or --apply first for a formatted-corpus check) =="
    if [[ "$APPLY" == "1" ]]; then
        echo "  --apply: copying formatted output back to real corpus…"
        for corpus in "${CORPORA[@]}"; do
            if [[ -d "$SCRATCH/$corpus" ]]; then
                # Copy the *.gg files back, preserving relative structure.
                (cd "$SCRATCH/$corpus" && find . -name "*.gg" -print) | while read -r f; do
                    cp "$SCRATCH/$corpus/$f" "$REPO_ROOT/$corpus/$f"
                done
            fi
        done
    fi
    GG_BUILD_TIMEOUT_SECS=900 GG_TEST_TIMEOUT_SECS=600 \
        cargo test --test integration --release self_host_bootstrap_fixed_point
    if [[ "$APPLY" == "1" ]]; then
        echo ""
        echo "  --apply was set; the real corpus is now REFORMATTED."
        echo "  Review with: git status / git diff and commit or restore."
    else
        echo ""
        echo "  Bootstrap fixed-point passed on the REAL corpus."
        echo "  The scratch (formatted) copy is at: $SCRATCH"
        echo "  Re-run with --apply to reformat the real corpus in-place."
    fi
fi
