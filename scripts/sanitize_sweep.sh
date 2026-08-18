#!/usr/bin/env bash
# Run the fixture corpus under ASan+UBSan and diff against the committed
# allowlists in tests/sanitize/.
#
# WHY THIS EXISTS. Measured 2026-08-18: of 2100 top-level fixtures, 13 carried a
# sanitize assertion. 99.4% ran an stdout compare and nothing else — and an
# stdout compare cannot observe a use-after-free. Every memory-safety defect
# found in R42 was found INCIDENTALLY by someone hand-building an ASan probe.
# This converts that into an enumeration (owner ratified 2026-08-18:
# "we should be running everything under a sanitizer").
#
# Whole sweep is ~25 min at parallelism 8, which is why it is a separate CI job
# rather than part of the main integration run.
#
# Exit 0 = no NEW corruption and no NEW leak. Exit 1 = something regressed.
# Fixtures that fix a listed defect show up as "no longer failing" — that is a
# PASS, and the message tells you to delete the row.
set -uo pipefail
cd "$(dirname "$0")/.."

JOBS="${JOBS:-8}"
GG="${GG:-target/debug/gg}"
OUT="${OUT:-/tmp/sanitize_sweep_$$}"
CORRUPT_LIST=tests/sanitize/CORRUPTION_ALLOWLIST.txt
LEAK_LIST=tests/sanitize/LEAK_ALLOWLIST.txt

[ -x "$GG" ] || { echo "no gg at $GG — run cargo build first"; exit 2; }
mkdir -p "$OUT/logs"

# Allowlisted names (column 1, comments stripped).
awk '!/^#/ && NF {print $1}' "$CORRUPT_LIST" | sort -u > "$OUT/allow_corrupt"
awk '!/^#/ && NF {print $1}' "$LEAK_LIST"    | sort -u > "$OUT/allow_leak"

run_one() {
  f="$1"; stem="$(basename "$f" .gg)"; d="$OUT/w/$stem"; mkdir -p "$d"
  cp "$f" "$d/" 2>/dev/null || { echo -e "$stem\tSKIP_COPY"; return; }
  if ! "$GG" build --sanitize "$d/$stem.gg" >"$OUT/logs/$stem.build" 2>&1; then
    # A fixture that also fails WITHOUT --sanitize is a pre-existing build issue,
    # not a sanitizer finding. Distinguish them; do not report the wrong thing.
    if "$GG" build "$d/$stem.gg" >/dev/null 2>&1; then
      echo -e "$stem\tBUILD_FAIL_SANITIZE_ONLY"
    else
      echo -e "$stem\tBUILD_FAIL_BOTH"
    fi
    return
  fi
  [ -x "$d/$stem" ] || { echo -e "$stem\tNO_BINARY"; return; }
  # stderr to a FILE, never a pipe — a pipeline masks the signal and a crash
  # reads as a clean exit (this cost two wrong verdicts in this tree).
  ( cd "$d" && ASAN_OPTIONS=detect_leaks=1 timeout 60 "./$stem" >/dev/null 2>"$OUT/logs/$stem.run" )
  log="$OUT/logs/$stem.run"
  if   grep -q 'ERROR: AddressSanitizer: stack-overflow' "$log" 2>/dev/null; then echo -e "$stem\tASAN_stack-overflow"
  elif grep -q 'ERROR: AddressSanitizer'  "$log" 2>/dev/null; then echo -e "$stem\tASAN_$(grep -o 'AddressSanitizer: [a-z-]*' "$log" | head -1 | cut -d' ' -f2)"
  elif grep -q 'ERROR: LeakSanitizer'     "$log" 2>/dev/null; then echo -e "$stem\tLEAK"
  elif grep -q 'runtime error:'           "$log" 2>/dev/null; then echo -e "$stem\tUBSAN"
  else echo -e "$stem\tCLEAN"; fi
}
export -f run_one; export OUT GG

find tests/fixtures -maxdepth 1 -name '*.gg' | sort \
  | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} > "$OUT/verdicts.tsv"

awk -F'\t' '$2 ~ /^ASAN_/  {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_corrupt"
awk -F'\t' '$2 == "LEAK"   {print $1}' "$OUT/verdicts.tsv" | sort -u > "$OUT/got_leak"

new_corrupt=$(comm -23 "$OUT/got_corrupt" "$OUT/allow_corrupt")
new_leak=$(comm -23 "$OUT/got_leak"    "$OUT/allow_leak")
fixed_corrupt=$(comm -13 "$OUT/got_corrupt" "$OUT/allow_corrupt")
fixed_leak=$(comm -13 "$OUT/got_leak"    "$OUT/allow_leak")

echo "=== sanitize sweep ==="
echo "scanned:     $(wc -l < "$OUT/verdicts.tsv")"
echo "corruption:  $(wc -l < "$OUT/got_corrupt") (allowlisted $(wc -l < "$OUT/allow_corrupt"))"
echo "leaks:       $(wc -l < "$OUT/got_leak") (allowlisted $(wc -l < "$OUT/allow_leak"))"
echo "raw verdicts: $OUT/verdicts.tsv   logs: $OUT/logs/"

rc=0
if [ -n "$new_corrupt" ]; then
  echo; echo "❌ NEW MEMORY CORRUPTION — this is a soundness regression, not debt:"
  echo "$new_corrupt" | sed 's/^/    /'
  echo "    Fix it. Adding a row to $CORRUPT_LIST ships a known memory-safety bug"
  echo "    and needs an owner decision plus a filed TODO entry."
  rc=1
fi
if [ -n "$new_leak" ]; then
  echo; echo "❌ NEW LEAK(S):"; echo "$new_leak" | sed 's/^/    /'
  echo "    Fix it, or justify adding it to $LEAK_LIST."
  rc=1
fi
[ -n "$fixed_corrupt" ] && { echo; echo "✅ no longer corrupting — DELETE these rows from $CORRUPT_LIST:"; echo "$fixed_corrupt" | sed 's/^/    /'; }
[ -n "$fixed_leak" ]    && { echo; echo "✅ no longer leaking — DELETE these rows from $LEAK_LIST:";      echo "$fixed_leak"    | sed 's/^/    /'; }
exit $rc
