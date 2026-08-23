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
#   scripts/convergence.sh                              # current counts
#   scripts/convergence.sh <prev_kg> <prev_todo>        # full `Convergence:` line
#   scripts/convergence.sh <prev_kg> <prev_todo> <filed> # + STRICT 2x VERDICT
#
# ── THIS SCRIPT IS THE ARBITER OF THE STRICT 2x RULE ──────────────────────
# Not prose in a DONE entry. A round that asserts compliance without quoting
# this script's verdict line has not demonstrated it (measured twice: Round
# XXVIII claimed "TODO strictly decreases" with TODO flat, and Round XXIX
# claimed "STRICT 2x satisfied" at a 1.7:1 ratio).
#
# The rule's clause (a) — "close >= 2x what you file" — is EXACTLY the
# inequality  net <= -filed.  Algebra: net = filed - closed, so
# closed >= 2*filed  <=>  filed - net >= 2*filed  <=>  net <= -filed.
# That matters because `filed` and `net` are both countable while `closed`
# invites hand-counting, which is how the ratio drifted.
#
# WHAT COUNTS (no other reading is available):
#   FILED   = a NEW TODO work item (a `- **` bullet in a categorized
#             section) or a NEW known_gaps/*.gg fixture that is an OPEN GAP
#             -- i.e. it has an #[ignore]d test asserting the intended
#             behaviour, or no wired test at all.
#   CLOSED  = a TODO work item REMOVED, or a known_gaps fixture GRADUATED
#             (its #[ignore] removed) / deleted.
#   NOT A GAP AT ALL = a known_gaps fixture referenced ONLY by LIVE tests.
#             That is the regression net of a bug already FIXED, parked in
#             that directory because `runtime_parity_corpus` never descends
#             subdirectories. It is not filed work and never was.
#   NEITHER = rewriting, narrowing, or re-scoping an existing entry;
#             splitting one fused bullet into several (a counting
#             correction, not new work); amending an entry in place;
#             anything in a prose section (see the stray-filing guard
#             below — filing there is banned precisely because it is
#             invisible here).
# There is NO size, effort, or "big-ticket" exemption to any clause, and
# none may be inferred. See AGENTS.md "Round lifecycle" step 5.
#
# ── PHASED WORK: ONE BULLET PER DECLARED PHASE ────────────────────────────
# A single bullet describing N phases makes a landed phase INVISIBLE here: it
# closes nothing, files nothing, reads `net +0`, and is indistinguishable from
# a round that did nothing — even if the phase was a thousand lines of
# measured, fixture-covered work. THAT, not size or difficulty, is the whole
# reason architecture rounds looked like they needed an exemption. They do not:
# encode each declared phase as its own bullet and every landing closes one
# (`net −1`) and passes clause (c) on its merits.
#
# The accounting is NEUTRAL over the item's life: +(N−1) once when it is filed
# as N bullets instead of 1, then −1 per phase landed = −1 total, identical to
# the single-bullet encoding. Splitting manufactures no credit; it only changes
# WHEN the credit lands, so intermediate progress stops reading as zero.
#
# ⚠ CORRECTING AN EXISTING FUSED ENTRY: do it BETWEEN rounds. The split is a
# counting correction (NEITHER, above), but the raw count still moves, so a
# round that splits an entry AND lands one of its phases nets +1 and fails.
# Done in the interstitial, the one-time +(N−1) lands in the NEXT round's
# BASELINE — which no round is claiming compliance against.
#
# Convention: net = Δknown_gaps + Δtodo_items. NEGATIVE is convergent. This
# combined net is THE number the gate reads (AGENTS.md Round lifecycle step 5):
# a `known_gaps` graduation counts as a closure, and "TODO alone fell" is a
# different claim. Under the STRICT 2× RULE (owner 2026-08-02, binding from
# Round XXVIII) net >= 0 does not close, full stop — the old "name the reason in
# the DONE entry" exemption is RETIRED; add tracks until the net is negative.

set -euo pipefail
cd "$(dirname "$0")/.."

# Scratch for the known_gaps classification below; removed on every exit path.
TMP_ALL=$(mktemp); TMP_STATUS=$(mktemp); TMP_IGN=$(mktemp)
TMP_LIVE=$(mktemp); TMP_NETS=$(mktemp)
trap 'rm -f "$TMP_ALL" "$TMP_STATUS" "$TMP_IGN" "$TMP_LIVE" "$TMP_NETS"' EXIT

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

# A committed repro in known_gaps/ is an OPEN GAP only if it still has an
# #[ignore]d test asserting the INTENDED behaviour, or no wired test at all.
# A fixture referenced ONLY by LIVE tests is the opposite of a gap: it is the
# regression net of a bug that has been FIXED, parked in that directory because
# `runtime_parity_corpus` never descends subdirectories. Counting those as gaps
# made the metric PUNISH the wide, axis-complete nets Core #11/#12 demand, and
# taxed the compliant placement -- measured at R43, where 12 of 22 new fixtures
# were live nets for that round's own fixes and scored as 12 new gaps.
#
# ⚠ ONLY-live is the load-bearing word. Three R43 fixtures carry an #[ignore]d
# test AND a live reference (membership in a control array such as
# SILENT_EXPECTED). Those are still OPEN -- the ignored test is the one making
# a claim about intended behaviour. "Any live reference ⇒ not a gap" scored
# 15/7 against the hand-checked truth of 12/10.
#
# Graduating a fixture (removing its #[ignore]) therefore counts as a CLOSURE
# here, which is exactly what the WHAT-COUNTS block above means by graduation.
# ⚠ TWO DEFECTS FIXED 2026-08-23, both found by the R44 Track-G census, both
# mine from the R43 rebuild. They made this script over-count OPEN GAPS by 7:
#
#  (1) NESTED FIXTURES WERE UNREFERENCEABLE. The inventory keyed on `basename`
#      while the reference regex forbade `/`, so a fixture at
#      `known_gaps/<dir>/repro.gg` could never match a citation and counted as
#      open forever -- and four different `repro.gg` files collapsed to ONE row
#      under `sort -u`. 15 nested files became 10 permanently-open phantoms.
#      Both sides now key on the path RELATIVE to known_gaps/, which is exactly
#      how tests/integration.rs already spells them.
#
#  (2) THE IGNORE DETECTOR MATCHED PROSE. `/#\[ignore/` fires anywhere on a
#      line, so any COMMENT mentioning `#[ignore]` marked the next `fn` as
#      ignored -- including integration.rs:7792, whose comment records that the
#      `#[ignore]` was REMOVED BECAUSE THE BUG WAS FIXED. 32 comment lines
#      qualify. The attribute is now anchored to the start of its own line.
#
# This is a COUNTING CORRECTION (NEITHER filed nor closed) and it moves the raw
# number with no work done, so it lands BETWEEN rounds and the -7 belongs to the
# NEXT baseline -- never to a round claiming compliance. Same rule the fused-entry
# note above states.
known_gaps=$(
  # A UNIT is one filed gap: a top-level fixture, or a whole repro DIRECTORY.
  # A multi-file repro (entry point + the modules it imports) is ONE gap, not N
  # -- six of its support files had no citation of their own and each counted as
  # a separate permanently-open row.
  { find tests/fixtures/known_gaps -maxdepth 1 -name '*.gg' | sed 's|.*/||; s|\.gg$||'
    find tests/fixtures/known_gaps -mindepth 1 -maxdepth 1 -type d | sed 's|.*/||'
  } | sort -u > "$TMP_ALL"
  # Scan EVERY test target that cites a repro, not just integration.rs: lints.rs
  # and security.rs carry citations too, and security.rs's ignored tests were a
  # population this metric could not see at all.
  awk '
    /^[[:space:]]*#\[ignore/          { ign = 1; next }
    /^[[:space:]]*fn [A-Za-z0-9_]+\(/ { cur_ign = ign; ign = 0 }
    # (3) COMMENTS ARE NOT REFERENCES -- the deflating sibling of defect (2), found by the
    # R44 Track-G census and reproduced by the parent: a fixture path mentioned in a COMMENT
    # inside a LIVE test marks that fixture live-referenced. For a fixture with no wired test
    # it flips "open gap" to "closed-bug net" and the count drops with NO WORK DONE (measured
    # 96 -> 95 from one comment line). Direction matters: defect (2) inflated, this one DELETES
    # a filed gap. Skip whole-line comments, and strip a trailing // when it is not inside a
    # string literal (even number of quotes before it).
    /^[[:space:]]*(\/\/|\*|\/\*)/ { next }
    {
      line = $0
      ci = index(line, "//")
      if (ci > 0) {
        before = substr(line, 1, ci - 1)
        q = gsub(/"/, "\"", before)
        if (q % 2 == 0) line = before
      }
      while (match(line, /known_gaps\/[A-Za-z0-9_\/]+\.gg/)) {
        ref = substr(line, RSTART + 11, RLENGTH - 11 - 3)
        sub(/\/.*$/, "", ref)          # a nested citation belongs to its DIRECTORY unit
        printf "%s %s\n", ref, (cur_ign ? "IGNORED" : "LIVE")
        line = substr(line, RSTART + RLENGTH)
      }
    }
  ' tests/integration.rs tests/lints.rs tests/security.rs | sort -u > "$TMP_STATUS"
  awk '$2=="IGNORED"{print $1}' "$TMP_STATUS" | sort -u > "$TMP_IGN"
  awk '$2=="LIVE"   {print $1}' "$TMP_STATUS" | sort -u > "$TMP_LIVE"
  comm -23 "$TMP_LIVE" "$TMP_IGN" > "$TMP_NETS"      # live-ONLY = closed-bug nets
  comm -23 "$TMP_ALL"  "$TMP_NETS" | wc -l | tr -d ' '
)

todo_items=$(awk -v prose_re="$PROSE_RE" '
  /^## /  { in_prose = ($0 ~ prose_re); sub_admit = 0; next }
  /^### / { sub_admit = ($0 ~ /UNOWNED, HIGH SEVERITY/); next }
  (!in_prose || sub_admit) && /^- \*\*/ { items++ }
  END { print items + 0 }
' TODO.md)

if [ $# -ge 2 ]; then
  prev_kg=$1
  prev_todo=$2
  filed=${3:-}
  net=$(( (known_gaps - prev_kg) + (todo_items - prev_todo) ))
  # Match the ledger's typography: U+2212 for negatives, explicit + otherwise.
  if [ "$net" -lt 0 ]; then net_str="−${net#-}"; else net_str="+${net}"; fi
  printf 'Convergence: known_gaps %s→%s · TODO items %s→%s · net %s (regen: `scripts/convergence.sh %s %s%s`)\n' \
    "$prev_kg" "$known_gaps" "$prev_todo" "$todo_items" "$net_str" "$prev_kg" "$prev_todo" \
    "${filed:+ $filed}"

  fail=0
  # Clause (c): the combined net strictly decreases.
  if [ "$net" -ge 0 ]; then
    echo "  ✗ CLAUSE (c) FAILS: net $net_str is not < 0. The round does not close. Add closures." >&2
    fail=1
  fi
  # Clause (a): closed >= 2x filed, i.e. net <= -filed. Needs the round's
  # declared filing count — the ONE number a human supplies. Everything else
  # here is measured, so a wrong declaration is the only way to fake this.
  if [ -n "$filed" ]; then
    closed=$(( filed - net ))
    need=$(( -filed ))
    if [ "$net" -le "$need" ]; then
      printf '  ✓ CLAUSE (a) PASSES: filed %s, closed %s (net %s ≤ −%s) — ratio ≥ 2:1.\n' \
        "$filed" "$closed" "$net_str" "$filed"
    else
      printf '  ✗ CLAUSE (a) FAILS: filed %s ⇒ net must be ≤ −%s, but net is %s (closed %s, ratio %s:%s < 2:1).\n' \
        "$filed" "$filed" "$net_str" "$closed" "$closed" "$filed" >&2
      echo "     Close $(( 2 * filed - closed )) more item(s), or file fewer. No size/effort exemption exists." >&2
      fail=1
    fi
  else
    echo "  ⚠ CLAUSE (a) NOT CHECKED — pass the round's filing count as a 3rd argument." >&2
    echo "     A round close that does not quote a clause-(a) verdict has not demonstrated it." >&2
    fail=1
  fi
  [ "$fail" -eq 0 ] || exit 1
else
  printf 'known_gaps=%s todo_items=%s\n' "$known_gaps" "$todo_items"
  echo "  (pass the previous round's two numbers + the round's filing count)"
fi
