#!/usr/bin/env bash
# Regenerate the round-close convergence metric (AGENTS.md "Round lifecycle" step 5).
#
# WHY THIS IS A SCRIPT AND NOT AN INLINE grep: the metric is a GATE, and a
# hand-run grep drifted twice. The naive `grep -cE '^- \*\*' TODO.md` counts the
# PROSE bullets above the topical sections — the handover's candidate-bundle
# list, the hot-list, the operating invariants. Every round close REWRITES the
# handover by mandate, so that count moves without a single work item opening or
# closing: Round XIII's rewrite inflated it by +7 while real items went DOWN by
# 1, turning a net −3 into a reported −5. Counting only filed work items makes
# the number reproducible.
#
# Usage:
#   scripts/convergence.sh                      # current counts
#   scripts/convergence.sh <prev_kg> <prev_todo> # full `Convergence:` line
#
# Convention: net = Δknown_gaps + Δtodo_items. NEGATIVE is convergent. This
# combined net is THE number the gate reads (AGENTS.md Round lifecycle step 5):
# a `known_gaps` graduation counts as a closure, and "TODO alone fell" is a
# different claim. Under the STRICT 2× RULE (owner 2026-08-02, binding from
# Round XXVIII) net >= 0 does not close, full stop — the old "name the reason in
# the DONE entry" exemption is RETIRED; add tracks until the net is negative.

set -euo pipefail
cd "$(dirname "$0")/.."

# Prose sections: `## ` headings that hold narrative, not filed work. Bullets
# here are commentary and queue pointers. `### UNOWNED, HIGH SEVERITY` nests
# under the handover but DOES hold real items, so it is re-admitted.
# Matched as substrings of the `## ` heading line (ASCII only — the real
# headings carry emoji and an en-dash).
readonly PROSE_SECTIONS=('CURRENT NEXT' 'NEXT 1' 'Operating invariants')
readonly PROSE_RE='(CURRENT NEXT|NEXT 1|Operating invariants)'

# Guard: if the prose headings are ever renamed the skip list silently stops
# matching and the count jumps. Fail loudly instead of reporting a wrong gate.
for section in "${PROSE_SECTIONS[@]}"; do
  if ! grep -qE "^## .*${section}" TODO.md; then
    echo "convergence.sh: prose section heading not found: '## …${section}…'" >&2
    echo "  TODO.md headings changed — update PROSE_SECTIONS/PROSE_RE before trusting this number." >&2
    exit 1
  fi
done

# Guard: filed work items must live in a CATEGORIZED section, never in a prose
# section. The count above deliberately skips prose, so an item filed there is
# INVISIBLE to the gate — it inflates neither the filed side nor the closed
# side. Round XXVIII filed 1 and closed 3 inside the handover block and the
# counter read 533→533 flat while the DONE entry claimed a strict decrease.
# `🆕` is the project's "filed this round" marker, so its presence in a prose
# section is exactly the defect. Fail loudly rather than report a wrong gate.
stray_filings=$(awk -v prose_re="$PROSE_RE" '
  /^## /  { in_prose = ($0 ~ prose_re); sub_admit = 0; next }
  /^### / { sub_admit = ($0 ~ /UNOWNED, HIGH SEVERITY/); next }
  (in_prose && !sub_admit) && /^- \*\*/ && /🆕/ { printf "  TODO.md:%d  %.90s\n", NR, $0 }
' TODO.md)
if [ -n "$stray_filings" ]; then
  echo "convergence.sh: filed work item(s) inside a PROSE section — invisible to this gate:" >&2
  echo "$stray_filings" >&2
  echo "  Move them into a categorized section (## CoW … / ## Semantics … / etc.)." >&2
  echo "  The handover block carries STATE and pointers, never filed work (AGENTS.md step 5)." >&2
  exit 1
fi

known_gaps=$(find tests/fixtures/known_gaps -name '*.gg' | wc -l | tr -d ' ')

todo_items=$(awk -v prose_re="$PROSE_RE" '
  /^## /  { in_prose = ($0 ~ prose_re); sub_admit = 0; next }
  /^### / { sub_admit = ($0 ~ /UNOWNED, HIGH SEVERITY/); next }
  (!in_prose || sub_admit) && /^- \*\*/ { items++ }
  END { print items + 0 }
' TODO.md)

if [ $# -eq 2 ]; then
  prev_kg=$1
  prev_todo=$2
  net=$(( (known_gaps - prev_kg) + (todo_items - prev_todo) ))
  # Match the ledger's typography: U+2212 for negatives, explicit + otherwise.
  if [ "$net" -lt 0 ]; then net_str="−${net#-}"; else net_str="+${net}"; fi
  printf 'Convergence: known_gaps %s→%s · TODO items %s→%s · net %s (regen: `scripts/convergence.sh %s %s`)\n' \
    "$prev_kg" "$known_gaps" "$prev_todo" "$todo_items" "$net_str" "$prev_kg" "$prev_todo"
  if [ "$net" -ge 0 ]; then
    echo "  ⚠ net >= 0 — the round does NOT close (STRICT 2× RULE, owner 2026-08-02). Add tracks / land more closures until the net is negative; the 'name the reason' exemption is RETIRED." >&2
  fi
else
  printf 'known_gaps=%s todo_items=%s\n' "$known_gaps" "$todo_items"
  echo "  (pass the previous round's two numbers to emit the DONE-entry line)"
fi
