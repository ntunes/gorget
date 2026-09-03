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
#   scripts/convergence.sh <prev_kg> <prev_todo> <filed> # + filed/closed report
#
# ── THIS SCRIPT MEASURES; IT NO LONGER GATES (owner 2026-08-23) ─────────────
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
# ── WHERE THE ITEMS LIVE (owner decision 2026-08-23, migrated between rounds) ─
# Work items are FILES: one `todo/<id>.md` per item, TOML front-matter above a
# `+++` fence and the item's prose verbatim below it. `TODO.md` keeps the
# handover block, the operating invariants, the heading skeleton and a
# GENERATED index of pointers (`scripts/todo_index.py`).
#
# So `todo_items` is now a FILE COUNT, not a regex over prose. That retires the
# whole class this script's header is a monument to: every drift it records
# (Round XIII's +7 from a handover rewrite, Round XXVIII's invisible in-prose
# filing, the three arbiter defects of R44) came from counting BULLETS IN A
# NARRATIVE. `ls todo | wc -l` cannot be fooled by a rewritten paragraph.
# ⚠ The migration is COUNTING-NEUTRAL by construction: it moved 674 bullets to
# 674 files with the prose byte-identical, so the R45 baseline `todo=674` means
# exactly what it meant before.
#
# WHAT COUNTS (no other reading is available):
#   FILED   = a NEW `todo/<id>.md` item file, or a NEW known_gaps/*.gg OPEN GAP
#             that NO item cites -- i.e. it has an #[ignore]d test asserting the
#             intended behaviour, or no wired test at all.
#
#             ⚠ OWNER RULING 2026-08-23: A MANDATED REPRO COUNTS WITH ITS
#             BULLET, NOT AS A SECOND FILING. The cardinal rule REQUIRES a
#             durable known_gaps repro for every filed reproducible bug, so
#             counting the repro separately charged +2 for one discovery --
#             and clause (a) then demanded FOUR closures for it, while filing
#             the SAME bug WITHOUT the mandated repro cost only two. The
#             metric was rewarding a cardinal-rule violation. Measured live:
#             one review-required filing turned a track's net -1 into +0.
#             A repro CITED by a TODO bullet is that bullet's evidence.
#             An UNCITED gap fixture still counts on its own, so a gap filed
#             as a fixture with no bullet stays visible to this gate.
#   CLOSED  = a `todo/<id>.md` file `git rm`'d, or a known_gaps fixture
#             GRADUATED (its #[ignore] removed) / deleted.
#             ⚠ CLOSURE IS REMOVAL, never `status = "closed"` in place: an
#             in-place status field grows the directory forever and puts this
#             arbiter back to interpreting field values, which is the class
#             that produced three defects in it in one round. `git log
#             --diff-filter=D -- todo/` preserves the item's whole life.
#   NOT A GAP AT ALL = a known_gaps fixture referenced ONLY by LIVE tests.
#             That is the regression net of a bug already FIXED, parked in
#             that directory because `runtime_parity_corpus` never descends
#             subdirectories. It is not filed work and never was.
#   NEITHER = rewriting, narrowing, or re-scoping an existing item;
#             splitting one fused item into several (a counting
#             correction, not new work); amending an item in place;
#             anything written into TODO.md's prose instead of a
#             `todo/` file (see the stray-filing guard below — filing
#             there is banned precisely because it is invisible here).
# There is NO size, effort, or "big-ticket" exemption to any clause, and
# none may be inferred. See AGENTS.md "Round lifecycle" step 5.
#
# ── PHASED WORK: ONE ITEM FILE PER DECLARED PHASE ─────────────────────────
# A single item describing N phases makes a landed phase INVISIBLE here: it
# closes nothing, files nothing, reads `net +0`, and is indistinguishable from
# a round that did nothing — even if the phase was a thousand lines of
# measured, fixture-covered work. THAT, not size or difficulty, is the whole
# reason architecture rounds looked like they needed an exemption. They do not:
# encode each declared phase as its own item file and every landing closes one
# (`net −1`) and passes clause (c) on its merits.
#
# The accounting is NEUTRAL over the item's life: +(N−1) once when it is filed
# as N files instead of 1, then −1 per phase landed = −1 total, identical to
# the single-file encoding. Splitting manufactures no credit; it only changes
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
TMP_CITED=$(mktemp); TMP_EXEMPT=$(mktemp)
trap 'rm -f "$TMP_ALL" "$TMP_STATUS" "$TMP_IGN" "$TMP_LIVE" "$TMP_NETS" "$TMP_CITED" "$TMP_EXEMPT"' EXIT

# Guard: the item directory must exist. Without it `find … | wc -l` reports a
# serene 0 and every round looks like it closed everything — the loudest way
# this metric could ever be wrong.
if [ ! -d todo ]; then
  echo "convergence.sh: todo/ is missing — that is where work items live" >&2
  echo "  (owner decision 2026-08-23: one todo/<id>.md per item; TODO.md keeps the handover)." >&2
  exit 1
fi

# Guard: a work item must be a `todo/<id>.md` FILE, never a bullet written into
# TODO.md. TODO.md is counted by nothing, so an item filed there is INVISIBLE
# to this gate — it inflates neither the filed side nor the closed side. Round
# XXVIII filed 1 and closed 3 inside the handover block and the counter read
# 533→533 flat while the DONE entry claimed a strict decrease. `🆕` is the
# project's "filed this round" marker, so a `- **`-bullet carrying it inside
# TODO.md is exactly the defect. Fail loudly rather than report a wrong gate.
# (The generated index lines start `- [` and never trip this.)
stray_filings=$(awk '/^- \*\*/ && /🆕/ { printf "  TODO.md:%d  %.90s\n", NR, $0 }' TODO.md)
if [ -n "$stray_filings" ]; then
  echo "convergence.sh: filed work item(s) written into TODO.md — invisible to this gate:" >&2
  echo "$stray_filings" >&2
  echo "  Move each into its own todo/<id>.md file and regenerate the index" >&2
  echo "  (scripts/todo_index.py). TODO.md carries STATE and pointers, never filed work." >&2
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
# -- ONE DEFINITION OF WHAT A `known_gaps` CITATION LOOKS LIKE --------------
# Fixed 2026-09-03 (R49 Track H). This script recognised ONLY the
# `known_gaps/<name>.gg` spelling, while the INVENTORY above (and
# `scripts/known_gaps_census.sh`'s enumerator, deliberately) treats a whole
# repro DIRECTORY as a unit and tests spell those as a BARE `known_gaps/<dir>`.
# So those units could never match a citation and counted as open forever, and
# `snag52b` was doubly wrong -- it is LIVE-wired through
# `run_gg_dir("known_gaps/snag52b", ..)`.
#
# WARNING: THE FIX HAD TO LAND AT BOTH SITES, 17 LINES APART (Core #4): the
# LIVE/IGNORED scan over `tests/*.rs` and the CITED scan over the record used
# the same blind regex, and fixing only one would have left the two halves of
# the exemption running on different rules.
#
# WARNING: TERMINATING BOUNDARY, load-bearing once the `.gg` is optional. The
# record legitimately writes GLOBS -- `known_gaps/rust_gg_bug_*`,
# `known_gaps/cow_scope_bare_param_{`. Without a boundary those read as
# citations of units called `rust_gg_bug_` and `cow_scope_bare_param_`, which
# is harmless only until a glob's PREFIX happens to be a real unit name, at
# which point a mention in prose silently exempts a genuinely open gap. A `.`
# followed by an alphanumeric is rejected for the same reason: it is a file
# EXTENSION (`known_gaps/foo.rs`), not a unit. A sentence-ending `.` is fine --
# no known_gaps name contains `.` or `-`, so nothing else needs a rule.
KG_EMIT='
function kg_emit(line, tag,   m, nxt, nxt2, ref) {
  while (match(line, /known_gaps\/[A-Za-z0-9_\/]+(\.gg)?/)) {
    m    = substr(line, RSTART, RLENGTH)
    nxt  = substr(line, RSTART + RLENGTH, 1)
    nxt2 = substr(line, RSTART + RLENGTH + 1, 1)
    line = substr(line, RSTART + RLENGTH)
    if (nxt == "*" || nxt == "{") continue
    if (nxt == "." && nxt2 ~ /[A-Za-z0-9]/) continue
    ref = substr(m, 12)
    sub(/\.gg$/, "", ref)
    sub(/\/.*$/, "", ref)          # a nested citation belongs to its DIRECTORY unit
    if (tag == "") print ref; else printf "%s %s\n", ref, tag
  }
}
'

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
  awk "$KG_EMIT"'
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
      kg_emit(line, (cur_ign ? "IGNORED" : "LIVE"))
    }
  ' tests/integration.rs tests/lints.rs tests/security.rs | sort -u > "$TMP_STATUS"
  awk '$2=="IGNORED"{print $1}' "$TMP_STATUS" | sort -u > "$TMP_IGN"
  awk '$2=="LIVE"   {print $1}' "$TMP_STATUS" | sort -u > "$TMP_LIVE"
  comm -23 "$TMP_LIVE" "$TMP_IGN" > "$TMP_NETS"      # live-ONLY = closed-bug nets
  # OWNER RULING 2026-08-23 (see WHAT COUNTS above): a repro CITED by an item is
  # that item's evidence, not a second filing. Subtract those too; an UNCITED
  # gap fixture still counts on its own.
  # Both TODO.md and todo/ are scanned: the item bodies moved into todo/, but a
  # `#### ` group heading and the handover still legitimately name a repro, and
  # a citation is a citation wherever the record makes it.
  # `-L` and a `cat` pipe rather than `find | xargs`: `todo` may legitimately be
  # a SYMLINK (it is in a sandboxed re-measurement), and an unfollowed symlink
  # makes this scan silently empty -- which reads as "nothing is cited" and
  # INFLATES the count with no work done. Measured while verifying this very fix.
  { cat TODO.md; find -L todo -type f -exec cat {} + ; } 2>/dev/null \
    | awk "$KG_EMIT"'{ kg_emit($0, "") }' \
    | sort -u > "$TMP_CITED"
  cat "$TMP_NETS" "$TMP_CITED" | sort -u > "$TMP_EXEMPT"
  comm -23 "$TMP_ALL"  "$TMP_EXEMPT" | wc -l | tr -d ' '
)

# One item = one file. No regex over prose, no section bookkeeping, nothing a
# handover rewrite can move.
todo_items=$(find todo -maxdepth 1 -name '*.md' -type f | wc -l | tr -d ' ')

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

  # ── MEASUREMENT, NOT A GATE (owner 2026-08-23) ───────────────────────────
  # The STRICT 2x RULE was REMOVED. It was failed repeatedly, and the rounds
  # that failed it were the ones doing the most valuable work: a round that
  # finds nine real defects -- three of them memory-safety -- is a GOOD round
  # that the ratio scored as failing. Measuring inflow is useful; GATING on it
  # selected against discovery.
  #
  # This block now REPORTS and always exits 0. Do not re-add a threshold here.
  # What survives is an owner ruling, not arithmetic: FIX INLINE unless the
  # defect is REALLY DISJOINT (discriminator: does the scope creep).
  if [ -n "$filed" ]; then
    closed=$(( filed - net ))
    if [ "$closed" -ge 0 ]; then
      printf '  declared filed %s · implied closed %s · measured net %s\n' "$filed" "$closed" "$net_str"
    else
      # closed cannot be negative: the declaration undercounts what actually landed.
      printf '  declared filed %s · measured net %s ⇒ %s MORE item(s) landed than were declared filed.\n' \
        "$filed" "$net_str" "$(( -closed ))"
      printf '     Re-count the round filings, or accept the declaration as a lower bound.\n'
    fi
  else
    printf '  measured net %s (pass the round filing count as a 3rd arg to also report closed)\n' "$net_str"
  fi
else
  printf 'known_gaps=%s todo_items=%s\n' "$known_gaps" "$todo_items"
  echo "  (pass the previous round's two numbers + the round's filing count)"
fi
