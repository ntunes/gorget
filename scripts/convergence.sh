#!/usr/bin/env bash
# Regenerate the round-close convergence metric (AGENTS.md "Round lifecycle" step 5).
#
# WHY THIS IS A SCRIPT AND NOT AN INLINE grep: the metric is QUOTED IN THE
# RECORD, and a hand-run grep drifted twice. The naive `grep -cE '^- \*\*' TODO.md` counts the
# PROSE bullets above the topical sections — the handover's candidate-bundle
# list, the hot-list, the operating invariants. Every round close REWRITES the
# handover by mandate, so that count moves without a single work item opening or
# closing: Round XIII's rewrite inflated it by +7 while real items went DOWN by
# 1, turning a net −3 into a reported −5. Counting only filed work items makes
# the number reproducible.
#
# Usage:
#   scripts/convergence.sh                              # current counts + debt ledger
#   scripts/convergence.sh <prev_kg> <prev_todo>        # full `Convergence:` line
#   scripts/convergence.sh <prev_kg> <prev_todo> <filed> # + filed/closed report
#   scripts/convergence.sh --ledger                     # the DEBT LEDGER alone
#   scripts/convergence.sh --ledger-diff [<axis>]       # NAME what moved on an axis
#   scripts/convergence.sh --bless                      # rewrite the ledger baseline
#
# ⚠ THE FIRST NUMBER IS CALLED `uncited_gaps`, AND IT USED TO BE CALLED
# `known_gaps`. It counts gap FIXTURES that no `todo/` item cites — NOT known
# failures. Under the old label it printed 17 while the tree carried 236 known
# `known_gaps` FAILURES, and the owner was misled by exactly that. The real
# per-axis failure counts are the DEBT LEDGER at the bottom of this file.
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
# Convention: net = Δuncited_gaps + Δtodo_items. NEGATIVE is convergent: an
# uncited-gap graduation counts as a closure, and "TODO alone fell" is a
# different claim.
#
# ⛔ THE STRICT 2× RULE IS RETIRED (owner 2026-08-23) AND NOTHING HERE GATES.
# This header used to end "net >= 0 does not close, full stop"; that sentence
# outlived its rule by rounds, which is exactly the decay the file below warns
# about. `net >= 0` is a REPORT. Same for the debt ledger at the bottom.

set -euo pipefail
cd "$(dirname "$0")/.."

# ── FLAGS, parsed out before the positional <prev_kg> <prev_todo> [filed] ────
LEDGER_ONLY=0; BLESS=0; DIFF_MODE=0
POS=()
for arg in "$@"; do
  case "$arg" in
    --ledger) LEDGER_ONLY=1 ;;
    --bless)  BLESS=1 ;;
    --ledger-diff) LEDGER_ONLY=1; DIFF_MODE=1 ;;
    -*) echo "convergence.sh: unknown flag: $arg" >&2; exit 2 ;;
    *)  POS+=("$arg") ;;
  esac
done
set -- ${POS+"${POS[@]}"}

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

if [ "$LEDGER_ONLY" = 1 ]; then
  :
elif [ $# -ge 2 ]; then
  prev_kg=$1
  prev_todo=$2
  filed=${3:-}
  net=$(( (known_gaps - prev_kg) + (todo_items - prev_todo) ))
  # Match the ledger's typography: U+2212 for negatives, explicit + otherwise.
  if [ "$net" -lt 0 ]; then net_str="−${net#-}"; else net_str="+${net}"; fi
  printf 'Convergence: uncited_gaps %s→%s · TODO items %s→%s · net %s (regen: `scripts/convergence.sh %s %s%s`)\n' \
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
  printf 'uncited_gaps=%s todo_items=%s\n' "$known_gaps" "$todo_items"
  echo "  (pass the previous round's two numbers + the round's filing count)"
fi

# ═══════════════════════════════════════════════════════════════════════════
#  THE DEBT LEDGER  (AGENTS.md Round lifecycle step 5)
# ═══════════════════════════════════════════════════════════════════════════
#
# WHY IT EXISTS. The metric above answers ONE question — "did the filed-work
# stock move?" — and its `uncited_gaps` row was, until this section landed,
# LABELLED `known_gaps` while counting something else entirely: gap FIXTURES
# that no `todo/` item cites. It read 17 while the actual number of known
# `known_gaps` FAILURES was 236, and the owner was misled by it. A label that
# names a bigger thing than it counts is worse than no number.
#
# So every axis now shows its OWN ACTUAL COUNT, and the three movements a debt
# row can make: DISCOVERED (+found), FIXED (−fixed), CARRIED (neither). Both the
# RATES and the STOCK, because a round can burn ten rows while ten more appear
# and a net of zero hides all twenty.
#
# ⛔ IT MEASURES AND REPORTS. IT DOES NOT GATE A ROUND CLOSE. The owner retired
# the strict 2× convergence rule in 2026-08 precisely because gating on inflow
# penalised discovery; the same reasoning binds here, one axis wider. A row
# moving the wrong way is NAMED in the round entry, never a blocker. There is
# deliberately NO threshold, NO failing assertion on any total, and this script
# still exits 0 on every path that reaches here.
#
# ⭐ DISCOVERED ≠ INTRODUCED, and the ledger cannot tell them apart — only the
# round can. A failure this round EXPOSED (better instrument, honest probe)
# becomes KNOWN: fix it this round, splitting the track if that is what it
# takes, else file it. A failure this round INTRODUCED is fixed BEFORE close.
# That is Core #9's "raising RUNTIME_DIFF_NONMATCH_CEILING for your own inflow
# is forbidden", generalised from one axis to all of them.
#
# ── EVERY NUMBER IS DERIVED, NONE IS PINNED ────────────────────────────────
# Layering rule 3 (one source of truth per axis) applied to the ledger: a
# hard-coded count here would rebuild exactly the defect above. Each axis names
# the command that regenerates it, and the enumerator emits MEMBER KEYS, not a
# total — the total is `wc -l` of the keys, and the movements are set
# differences. A scalar baseline could only ever report a NET.
#
#   known_gaps FAIL   `scripts/known_gaps_census.sh --list` roster MINUS the
#                     rows in tests/gaps/PASSING_ALLOWLIST.txt. Whenever
#                     `--check` is green (a round-close gate) it asserts the
#                     measured PASS set EQUALS the allowlist, so roster −
#                     allowlist is the NON-PASSING set. ⚠ NOT EXACTLY THE FAIL
#                     SET: the census also reports TIMEOUT / NOT_RUN /
#                     NO_BINARY, which are neither PASS nor FAIL and land on
#                     this side of the subtraction. It errs toward COUNTING
#                     MORE DEBT, which is the safe direction for a debt row;
#                     the census's own `roster N · PASS n · FAIL n` summary
#                     line is the exact split when you need it. `--list`
#                     runs nothing and costs ~0.05 s, which is why the ledger
#                     can be the FIRST thing a round close runs (owner
#                     2026-08-06) instead of a two-minute census.
#   ignored tests     `#[ignore]` in ATTRIBUTE POSITION (start of line, modulo
#                     indent) in tests/*.rs, attributed to the `fn` that
#                     follows. ⚠ NOT the bare `grep -c '#\[ignore' `, which
#                     reads 338 on tests/integration.rs alone because it counts
#                     DOC-COMMENT MENTIONS of the attribute — the identical
#                     defect this script's own header records fixing in 2026-08
#                     (defect (2): "the ignore detector matched prose"), and
#                     `scripts/known_gaps_census.sh` warns about in its
#                     enumerator. Anchored, every hit is followed by a `fn`, so
#                     the enumeration is total, not a sample.
#   sanitize leaks    non-comment rows of tests/sanitize/LEAK_ALLOWLIST.txt —
#   sanitize corrupt.  and of CORRUPTION_ALLOWLIST.txt. Same rows
#                     `sanitize_allowlists_shrink_only` pins in tests/lints.rs,
#                     read from the allowlist rather than from its constant, so
#                     the ledger cannot inherit a stale pin.
#   open CRITICALs    todo/ items whose front matter says severity = "CRITICAL".
#   guard constants   `const NAME: <int> = <literal>;` in tests/lints.rs. THE
#                     ONE AXIS THAT IS NOT DISCOVERY-SENSITIVE: nobody
#                     "discovers" a guard constant, it is purely our own
#                     artifact, so this row's honest target is 0 and a rise is
#                     never excusable as discovery. Keys are scoped by the
#                     enclosing `fn` because the bare names repeat (`EXPECTED`
#                     many times over); the scope is for UNIQUENESS, not
#                     semantics.
#   todo/ items       CONTEXT, EXPLICITLY NOT A DEBT AXIS. A backlog is not a
#                     failure count; trending it to zero would discourage
#                     filing, and the cardinal rule REQUIRES filing. It is here
#                     because the round entry quotes it, and for no other reason.
#
# ── THE BASELINE IS AUTO-WRITTEN, NEVER HAND-EDITED ────────────────────────
# `scripts/convergence.sh --bless` rewrites scripts/baselines/debt_ledger.tsv
# from the measured sets. Same write-back shape Core #6 ratified for pins: the
# number descends without a human typing it, so it can never be quietly
# "corrected" to whatever makes a round look good.
#
# ⚠ ONE DELIBERATE DIFFERENCE FROM A PIN'S `--bless`, AND IT IS NOT AN
# OVERSIGHT: a PIN's write-back is LOWER-ONLY, because a pin is a CEILING and
# accumulated slack is what rots. A BASELINE is not a ceiling — it is last
# round's MEASUREMENT — and a refusal to record a rise would make the NEXT
# round's delta a lie: discovery would show as +N forever, and the round that
# actually burned five of those rows would read +5 instead of −5. Recording an
# increase costs nothing here precisely because nothing gates on it. What
# replaces lower-only is the NAMING obligation above, which is the owner's own
# ruling on which direction a wrong-way row travels.

LEDGER_BASELINE=scripts/baselines/debt_ledger.tsv

# Axis table: id, label, kind (debt|context). Order is the print order.
LEDGER_AXES=(
  'known_gaps_fail|known_gaps FAIL|debt'
  'ignored_tests|#[ignore]d tests|debt'
  'leak_allowlist|sanitize leaks allowlisted|debt'
  'corruption_allowlist|sanitize corruption allowlisted|debt'
  'open_criticals|open CRITICALs|debt'
  'lints_guard_constants|tests/lints.rs guard constants|debt'
  'todo_items|todo/ items|context'
)

# Emit the MEMBER KEYS of one axis, one per line, unsorted.
ledger_members() {
  case "$1" in
    known_gaps_fail)
      local roster pass
      roster=$(mktemp); pass=$(mktemp)
      scripts/known_gaps_census.sh --list 2>/dev/null \
        | awk -F'\t' '!/^#/ && NF { print $1 }' | sort -u > "$roster"
      awk '!/^#/ && NF { print $1 }' tests/gaps/PASSING_ALLOWLIST.txt | sort -u > "$pass"
      comm -23 "$roster" "$pass"
      rm -f "$roster" "$pass"
      ;;
    ignored_tests)
      # `#[ignore` only in ATTRIBUTE POSITION, attributed to the NEXT `fn`.
      for f in tests/*.rs; do
        awk -v F="$(basename "$f" .rs)" '
          /^[[:space:]]*#\[ignore/ { ign = 1; next }
          /^[[:space:]]*fn [A-Za-z0-9_]+/ {
            if (ign) {
              n = $0; sub(/^[[:space:]]*fn /, "", n); sub(/[(<].*$/, "", n)
              printf "%s::%s\n", F, n
            }
            ign = 0
          }
        ' "$f"
      done
      ;;
    leak_allowlist)
      awk '!/^#/ && NF { print $1 }' tests/sanitize/LEAK_ALLOWLIST.txt ;;
    corruption_allowlist)
      awk '!/^#/ && NF { print $1 }' tests/sanitize/CORRUPTION_ALLOWLIST.txt ;;
    open_criticals)
      grep -l '^severity = "CRITICAL"' todo/*.md 2>/dev/null | sed 's|.*/||; s|\.md$||' || true ;;
    lints_guard_constants)
      # Scoped by the enclosing `fn` because the bare const names repeat; a
      # third occurrence of the same key gets a `#N` suffix. Keys need only be
      # STABLE and UNIQUE — they are diffed, not interpreted.
      awk '
        /^[[:space:]]*fn [A-Za-z0-9_]+/ {
          s = $0; sub(/^[[:space:]]*fn /, "", s); sub(/[(<].*$/, "", s); scope = s
        }
        /^[[:space:]]*(pub )?const [A-Za-z_0-9]+[[:space:]]*:[^=]*=[[:space:]]*[0-9_]+[[:space:]]*;/ {
          c = $0; sub(/^[[:space:]]*(pub )?const /, "", c); sub(/[[:space:]]*:.*$/, "", c)
          k = (scope == "" ? "<module>" : scope) "::" c
          seen[k]++
          if (seen[k] > 1) k = k "#" seen[k]
          print k
        }
      ' tests/lints.rs ;;
    todo_items)
      find todo -maxdepth 1 -name '*.md' -type f | sed 's|.*/||; s|\.md$||' ;;
    *) echo "convergence.sh: unknown ledger axis: $1" >&2; exit 2 ;;
  esac
}

# ═══════════════════════════════════════════════════════════════════════════
#  THE SECOND WITNESS — because a count from ONE extraction has no control
# ═══════════════════════════════════════════════════════════════════════════
#
# The INSTRUMENT SUSPECT warning below catches an enumerator that goes SILENT
# (N → 0). It does NOT catch one that is merely MAIMED, and that is the more
# likely failure by far: a pattern drifts, it does not stop matching. Measured
# on this very script — narrowing the `ignored_tests` pattern to a bare
# `#[ignore]` drops the 288 `#[ignore = "reason"]` forms and reports
# `295 → 7, −288` at rc 0, with no warning and 97% "progress".
#
# ⛔ NOT FIXED WITH A THRESHOLD. "A drop of more than X% is suspect" needs a
# constant, and Core #6 plus this whole file is about REMOVING constants, not
# adding one to the newest gate. A threshold is also wrong in both directions:
# it green-lights a small real break and reds a large real burn-down.
#
# ⭐ THE FIX IS THE REPO'S OWN DERIVE PATTERN: give each axis a SECOND,
# INDEPENDENT extraction of the same fact and compare. Two extractions that
# disagree name a broken instrument immediately, at ANY breakage size, with
# ZERO constants. Disagreement is a HARD FAILURE (rc 3) — if two derivations of
# one number disagree, one of them is broken and no round should quote either.
# It runs BEFORE `--bless`, so a broken enumerator can never be written into the
# baseline.
#
# ⛔ THIS IS A GATE ON INSTRUMENT AGREEMENT, NOT ON ANY DEBT TOTAL. Nothing here
# asserts a count is low, falling, or bounded; discovery is not penalised.
#
# THE WITNESSES:
#   ignored_tests         CONTAINMENT against `known_gaps_census.sh --list`.
#                         Every roster row is BY DEFINITION an `#[ignore]`d
#                         test, enumerated by a different script written by a
#                         different author, so the roster must be a SUBSET of
#                         this axis. 242 of the 295 rows are covered, which is
#                         what makes the measured 295→7 break fire.
#   known_gaps_fail       THREE partial witnesses, none of them complete:
#                         (i) CONTAINMENT — every FAIL row must appear as an
#                         `#[ignore]`d test in the independent enumerator above;
#                         (ii) SENTINEL — every PASSING_ALLOWLIST.txt row must
#                         still be IN the census roster (an allowlisted row IS a
#                         roster row), which catches a narrowed census
#                         enumerator; (iii) COUNT on the SUBTRAHEND — allowlist
#                         rows against `tests/lints.rs`'s pin, which
#                         `known_gaps_passing_allowlist_shrink_only` holds equal.
#   leak_allowlist        COUNT against `tests/lints.rs::LEAK_CEILING`.
#   corruption_allowlist  COUNT against `tests/lints.rs::CORRUPTION_CEILING`.
#                         Both pins are hand-maintained and held equal to the
#                         file by `sanitize_allowlists_shrink_only`, so they can
#                         never legitimately diverge from a correct row count.
#   open_criticals        COUNT against `TODO.md`'s GENERATED index, produced by
#   todo_items            `scripts/todo_index.py` from the same front matter by
#                         a different tool in a different language, and held
#                         current by the `todo_index_is_current` lint.
#
# ⚠ WHAT IS *NOT* WITNESSED, NAMED RATHER THAN PAPERED OVER. A named omission is
# durable content; leaving it in a commit message would lose it.
# SINGLE-WITNESS: lints_guard_constants — a second regex over the same file is
#   not an independent extraction, it is the same reading twice. There is no
#   other source for "how many pinned guard values does tests/lints.rs hold".
#   `todo/t1618` records the related under-count (an INLINE guard value has no
#   declaration and is invisible to this axis at all).
# SINGLE-WITNESS: known_gaps_fail — PARTIALLY witnessed only. Its membership and
#   its six allowlisted SENTINELS are checked; its MAGNITUDE is not. A narrowing
#   of the census's own enumerator that happens to SPARE all six sentinels
#   shrinks the roster undetected, because the survivors are still all ignored
#   tests and containment stays green. Six of 242 rows is a net, not a proof.
#   The exact second reading is `known_gaps_census.sh --check`'s own
#   `roster N · PASS n · FAIL n` line — which pays the ~90 s `driver.gg` build
#   plus the row runs (`time scripts/known_gaps_census.sh` regenerates the cost;
#   this comment quotes no figure deliberately). That is too much for a script
#   whose whole point is to run FIRST, in a second, before ~30 min of sweeps
#   (owner 2026-08-06). `--check` is a round-close battery step in its own
#   right, so the exact number IS read every close — just not here.

# Read a numeric pin out of ONE guard body in tests/lints.rs. Scoped to the fn
# because `CEILING` is a local name reused across guards; an unscoped grep would
# read a different guard's number and call it agreement.
lints_pin() {
  awk -v fnsig="fn $1(" -v cn="$2" '
    index($0, fnsig) == 1 { inb = 1 }
    inb && $0 ~ ("^[[:space:]]*const " cn "[[:space:]]*:[[:space:]]*usize[[:space:]]*=") {
      sub(/^.*=[[:space:]]*/, ""); sub(/[^0-9].*$/, ""); print; exit
    }
  ' tests/lints.rs
}

# The census roster as `<test file stem>::<fn>` keys — the INDEPENDENT
# enumeration this file's `ignored_tests` arm is checked against.
census_roster_keys() {
  scripts/known_gaps_census.sh --list 2>/dev/null \
    | awk -F'\t' '!/^#/ && NF {
        f = $2; sub(/:.*$/, "", f); sub(/^.*\//, "", f); sub(/\.rs$/, "", f)
        printf "%s::%s\n", f, $1
      }' | sort -u
}

# $1 axis · $2 measured count · $3 file of measured member keys.
# Prints ONE line per disagreement; silence means the two readings agree.
# ALWAYS returns 0 — the caller decides what a disagreement costs.
ledger_witness() {
  axis=$1; wnow=$2; wmembers=$3
  case "$axis" in
    ignored_tests)
      wtmp=$(mktemp)
      census_roster_keys > "$wtmp"
      wmiss=$(comm -23 "$wtmp" "$wmembers" | wc -l | tr -d ' ')
      wros=$(wc -l < "$wtmp" | tr -d ' ')
      rm -f "$wtmp"
      if [ "$wmiss" -ne 0 ]; then
        printf '  ignored_tests: %s of the census roster'"'"'s %s ignored tests are ABSENT from this axis'"'"'s %s member(s).\n' \
          "$wmiss" "$wros" "$wnow"
        printf '     Second reading: `scripts/known_gaps_census.sh --list` (an independent enumerator).\n'
        printf '     Every roster row IS an `#[ignore]`d test, so this axis cannot be missing one.\n'
      fi
      ;;
    known_gaps_fail)
      wtmp=$(mktemp); wign=$(mktemp)
      census_roster_keys | sed 's|^[^:]*::||' | sort -u > "$wtmp"
      ledger_members ignored_tests | sed 's|^[^:]*::||' | sort -u > "$wign"
      wmiss=$(comm -23 "$wmembers" "$wign" | wc -l | tr -d ' ')
      rm -f "$wtmp" "$wign"
      if [ "$wmiss" -ne 0 ]; then
        printf '  known_gaps_fail: %s of %s FAIL row(s) are not `#[ignore]`d tests in the independent enumeration.\n' \
          "$wmiss" "$wnow"
      fi
      # SENTINEL witness on the ROSTER itself: every allowlisted row is by
      # definition a roster row (it is a roster row that PASSes), and the
      # allowlist is maintained in a different file by a different hand. If the
      # census's OWN enumerator narrows, these six drop out of the roster and
      # this fires. Measured: breaking the census's ignore detector collapses
      # the roster to 1 row and all 6 sentinels go missing.
      wtmp=$(mktemp); wal=$(mktemp)
      scripts/known_gaps_census.sh --list 2>/dev/null \
        | awk -F'\t' '!/^#/ && NF { print $1 }' | sort -u > "$wtmp"
      awk '!/^#/ && NF { print $1 }' tests/gaps/PASSING_ALLOWLIST.txt | sort -u > "$wal"
      wgone=$(comm -23 "$wal" "$wtmp" | wc -l | tr -d ' ')
      wallow=$(wc -l < "$wal" | tr -d ' ')
      rm -f "$wtmp" "$wal"
      if [ "$wgone" -ne 0 ]; then
        printf '  known_gaps_fail: %s of the %s PASSING_ALLOWLIST.txt row(s) are ABSENT from the census roster.\n' \
          "$wgone" "$wallow"
        printf '     An allowlisted row IS a roster row — the census enumerator has narrowed.\n'
      fi
      wpin=$(lints_pin known_gaps_passing_allowlist_shrink_only CEILING)
      if [ -n "$wpin" ] && [ "$wallow" != "$wpin" ]; then
        printf '  known_gaps_fail: the SUBTRAHEND disagrees — PASSING_ALLOWLIST.txt has %s row(s), tests/lints.rs pins %s.\n' \
          "$wallow" "$wpin"
      elif [ -z "$wpin" ]; then
        printf '  known_gaps_fail: could not read the allowlist pin out of tests/lints.rs — the witness itself is broken.\n'
      fi
      ;;
    leak_allowlist)
      wpin=$(lints_pin sanitize_allowlists_shrink_only LEAK_CEILING)
      if [ -z "$wpin" ]; then
        printf '  leak_allowlist: could not read LEAK_CEILING out of tests/lints.rs — the witness itself is broken.\n'
      elif [ "$wnow" != "$wpin" ]; then
        printf '  leak_allowlist: %s row(s) measured, tests/lints.rs::LEAK_CEILING pins %s.\n' "$wnow" "$wpin"
      fi
      ;;
    corruption_allowlist)
      wpin=$(lints_pin sanitize_allowlists_shrink_only CORRUPTION_CEILING)
      if [ -z "$wpin" ]; then
        printf '  corruption_allowlist: could not read CORRUPTION_CEILING out of tests/lints.rs — the witness itself is broken.\n'
      elif [ "$wnow" != "$wpin" ]; then
        printf '  corruption_allowlist: %s row(s) measured, tests/lints.rs::CORRUPTION_CEILING pins %s.\n' "$wnow" "$wpin"
      fi
      ;;
    open_criticals)
      wix=$(grep -cE '^- \[`t[0-9]+`\]\(todo/t[0-9]+\.md\) \*\*CRITICAL\*\*' TODO.md || true)
      if [ "$wnow" != "$wix" ]; then
        printf '  open_criticals: %s item(s) measured from todo/ front matter, TODO.md'"'"'s generated index lists %s.\n' \
          "$wnow" "$wix"
      fi
      ;;
    todo_items)
      wix=$(grep -cE '^- \[`t[0-9]+`\]\(todo/t[0-9]+\.md\)' TODO.md || true)
      if [ "$wnow" != "$wix" ]; then
        printf '  todo_items: %s file(s) measured, TODO.md'"'"'s generated index lists %s pointer(s).\n' "$wnow" "$wix"
      fi
      ;;
    lints_guard_constants) : ;;   # SINGLE-WITNESS by design — see the header.
    *) printf '  %s: no witness arm and no SINGLE-WITNESS declaration.\n' "$axis" ;;
  esac
  return 0
}

LEDGER_NOW=$(mktemp); LEDGER_PREV=$(mktemp)
LEDGER_A=$(mktemp); LEDGER_B=$(mktemp)
trap 'rm -f "$TMP_ALL" "$TMP_STATUS" "$TMP_IGN" "$TMP_LIVE" "$TMP_NETS" "$TMP_CITED" "$TMP_EXEMPT" "$LEDGER_NOW" "$LEDGER_PREV" "$LEDGER_A" "$LEDGER_B"' EXIT

: > "$LEDGER_NOW"
for row in "${LEDGER_AXES[@]}"; do
  axis=${row%%|*}
  ledger_members "$axis" | sort -u | sed "s|^|$axis\t|" >> "$LEDGER_NOW"
done

# ── Run the witnesses BEFORE anything reads or writes the ledger ────────────
witness_fail=""
for row in "${LEDGER_AXES[@]}"; do
  waxis=${row%%|*}
  awk -F'\t' -v a="$waxis" '$1 == a { print $2 }' "$LEDGER_NOW" | sort -u > "$LEDGER_A"
  wcount=$(wc -l < "$LEDGER_A" | tr -d ' ')
  wmsg=$(ledger_witness "$waxis" "$wcount" "$LEDGER_A")
  if [ -n "$wmsg" ]; then witness_fail="$witness_fail$wmsg"$'\n'; fi
done
if [ -n "$witness_fail" ]; then
  {
    echo "convergence.sh: ⛔ THE DEBT LEDGER'S INSTRUMENTS DISAGREE — nothing below is quotable."
    printf '%s' "$witness_fail"
    echo "  Two independent readings of one number disagree, so ONE OF THEM IS BROKEN."
    echo "  This is a gate on INSTRUMENT AGREEMENT, never on a debt total: no count here is"
    echo "  asserted low, falling or bounded. Fix the enumerator (or the pin it is read"
    echo "  against) and re-run; do NOT \`--bless\` until this is silent."
  } >&2
  exit 3
fi

if [ "$BLESS" = 1 ]; then
  mkdir -p "$(dirname "$LEDGER_BASELINE")"
  {
    echo "# DEBT-LEDGER BASELINE — AUTO-GENERATED by \`scripts/convergence.sh --bless\`."
    echo "# ⛔ NEVER HAND-EDIT. It is last close's MEASUREMENT, not a target: editing a row"
    echo "#    here does not fix a failure, it only makes the next round's delta a lie."
    echo "#    Regenerate at round close, after the records commit, with:"
    echo "#      scripts/convergence.sh --bless"
    echo "# blessed $(date -u +%Y-%m-%dT%H:%M:%SZ) at $(git rev-parse --short HEAD 2>/dev/null || echo '?')"
    echo "# One <axis>\tmember key per line; the count is \`wc -l\`, the movements are set diffs."
    cat "$LEDGER_NOW"
  } > "$LEDGER_BASELINE"
  echo "convergence.sh: blessed $LEDGER_BASELINE ($(grep -vc '^#' "$LEDGER_BASELINE") member key(s))"
fi

if [ -f "$LEDGER_BASELINE" ]; then
  grep -v '^#' "$LEDGER_BASELINE" > "$LEDGER_PREV" || true
  blessed_at=$(sed -n 's/^# blessed //p' "$LEDGER_BASELINE" | head -1)
  have_prev=1
else
  : > "$LEDGER_PREV"
  blessed_at=""
  have_prev=0
fi

if [ "$DIFF_MODE" = 1 ]; then
  # Name what moved, so a round entry can quote members instead of a delta.
  want=${1:-}
  # An unknown axis name must NOT print an empty report at rc 0 — that reads as
  # "nothing moved" and is the same succeed-with-empty failure the INSTRUMENT
  # SUSPECT block below exists for. `ledger_members` already exits 2 on an
  # unknown axis; this path has to agree with it.
  if [ -n "$want" ] && ! printf '%s\n' "${LEDGER_AXES[@]}" | cut -d'|' -f1 | tr -d "'" | grep -qx "$want"; then
    echo "convergence.sh: unknown ledger axis: $want" >&2
    printf '%s\n' "${LEDGER_AXES[@]}" | cut -d'|' -f1 | tr -d "'" | sed 's/^/  /' >&2
    exit 2
  fi
  for row in "${LEDGER_AXES[@]}"; do
    axis=${row%%|*}; rest=${row#*|}; label=${rest%%|*}
    if [ -n "$want" ] && [ "$want" != "$axis" ]; then continue; fi
    awk -F'\t' -v a="$axis" '$1 == a { print $2 }' "$LEDGER_NOW"  | sort -u > "$LEDGER_A"
    awk -F'\t' -v a="$axis" '$1 == a { print $2 }' "$LEDGER_PREV" | sort -u > "$LEDGER_B"
    printf '%s (%s)\n' "$label" "$axis"
    comm -23 "$LEDGER_A" "$LEDGER_B" | sed 's/^/  +found  /'
    comm -13 "$LEDGER_A" "$LEDGER_B" | sed 's/^/  −fixed  /'
  done
  exit 0
fi

echo
if [ "$have_prev" = 1 ]; then
  printf 'Debt ledger (baseline %s, blessed %s)\n' "$LEDGER_BASELINE" "$blessed_at"
else
  printf 'Debt ledger (NO BASELINE YET — run `scripts/convergence.sh --bless`; movements unknown)\n'
fi
printf '  %-34s %6s %8s %10s\n' 'axis' 'now' '+found' '−fixed'

wrong_way=""
suspect=""
for row in "${LEDGER_AXES[@]}"; do
  axis=${row%%|*}; rest=${row#*|}; label=${rest%%|*}; kind=${rest##*|}
  awk -F'\t' -v a="$axis" '$1 == a { print $2 }' "$LEDGER_NOW"  | sort -u > "$LEDGER_A"
  awk -F'\t' -v a="$axis" '$1 == a { print $2 }' "$LEDGER_PREV" | sort -u > "$LEDGER_B"
  now=$(wc -l < "$LEDGER_A" | tr -d ' ')
  was=$(wc -l < "$LEDGER_B" | tr -d ' ')
  if [ "$have_prev" = 1 ]; then
    found=$(comm -23 "$LEDGER_A" "$LEDGER_B" | wc -l | tr -d ' ')
    fixed=$(comm -13 "$LEDGER_A" "$LEDGER_B" | wc -l | tr -d ' ')
    found_str="+$found"; fixed_str="−$fixed"
  else
    found_str="+?"; fixed_str="−?"; found=0; fixed=0
  fi
  if [ "$kind" = context ]; then
    printf '  ── context, NOT a debt axis ──────────────────────────────────────\n'
  fi
  printf '  %-34s %6s %8s %10s\n' "$label" "$now" "$found_str" "$fixed_str"
  if [ "$kind" = debt ] && [ "$found" -gt "$fixed" ]; then
    wrong_way="$wrong_way $axis"
  fi
  # ── A BROKEN ENUMERATOR REPORTS THE MAXIMUM POSSIBLE PROGRESS ────────────
  # An enumerator that stops MATCHING succeeds with EMPTY output, and every
  # remaining row then reads as fixed: measured, changing `^severity = ` to
  # `^sev = ` reported `open CRITICALs 0 +0 −10` at rc 0, and mangling the
  # `#[ignore` pattern reported `−295`. Both are the ledger's own subject —
  # a number that names a bigger thing than it counts — occurring INSIDE the
  # instrument, in the direction that flatters the round. Whole-axis collapse
  # is the shape that failure always takes, and a real burn-down to zero is
  # rare enough (and proud enough) to be worth saying out loud too.
  # ⚠ ASYMMETRY FIXED HERE: a wrong-way row already got seven lines while an
  # implausible right-way collapse got nothing.
  # ⛔ STILL A MEASUREMENT: no threshold, no constant, no gate — it prints and
  # the round entry adjudicates. A hard-FAILING enumerator aborts on its own
  # (`set -e`); this covers only succeed-with-empty, which cannot.
  if [ "$have_prev" = 1 ] && [ "$was" -gt 0 ] && [ "$now" -eq 0 ]; then
    suspect="$suspect $axis:$was"
  fi
done

if [ "$have_prev" = 1 ] && [ -n "$wrong_way" ]; then
  echo
  echo "  ⚠ ROW(S) MOVING THE WRONG WAY:$wrong_way"
  echo "     NAME each one in the round entry, with its verdict — this is a report, not a blocker."
  echo "     DISCOVERED (exposed by better instrumentation or honest work) ⇒ fix it this round,"
  echo "     splitting the track if that is what it takes; otherwise file it for a future round."
  echo "     INTRODUCED by this round's own changes ⇒ FIX IT BEFORE CLOSE. Introducing known"
  echo "     issues is not allowed (Core #9's own-inflow clause, generalised to every axis)."
  echo "     Names of what moved:  scripts/convergence.sh --ledger-diff <axis>"
fi

if [ -n "$suspect" ]; then
  echo
  for s in $suspect; do
    printf '  ⚠ INSTRUMENT SUSPECT — axis %s went from %s to 0.\n' "${s%%:*}" "${s##*:}"
  done
  echo "     A TERMINUS or a BROKEN ENUMERATOR — say WHICH in the round entry."
  echo "     An enumerator that stops matching succeeds with empty output, and every row"
  echo "     then reads as fixed; \`--bless\` would bake that into the baseline permanently."
  echo "     Check the axis's own source before quoting the row:  scripts/convergence.sh --ledger-diff <axis>"
fi
echo "  (regen: \`scripts/convergence.sh --ledger\` · rebless at close: \`scripts/convergence.sh --bless\`)"
