#!/usr/bin/env bash
# Run the `StagingMoveIntoOwnedSlot` consume-site class under its promoter and
# reconcile the outcome against `tests/gaps/STAGING_MOVE_BURNDOWN.txt`.
#
# ── WHY THIS EXISTS ───────────────────────────────────────────────────────
# `StagingMoveIntoOwnedSlot` catches a staging site that moves an owned source
# which is still LIVE (`[Mv] _dst = copy _src; move_zero _src` with a real later
# read). A new consume-site class is FATAL the instant it lands — the validator
# has no env-gate runway — so the class enters through the non-fatal
# `assign_warnings` list and `GG_STAGING_MOVE_GUARD=fatal` promotes it, exactly
# the ladder AGENTS.md Core #6 prescribes (env-gate → burn down → fatal).
#
# The measured hazard with that shape is that NOTHING EVER SETS THE PROMOTER.
# Its precedent, `GG_RETURN_SLOT_GUARD`, has three occurrences in the whole tree
# and all three are in `src/` — no test, script or CI job has ever exercised
# that runway. A promoter nothing sets is not a guard, it is theatre. This
# script is the thing that sets it, and CI runs it (`.github/workflows/ci.yml`,
# job `test`).
#
# ⚠ The ENV VAR, not class membership, separates the burned-down rows from the
# target defects: every violation of the staging shape carries the same class.
# So with the var unset the guard is silent on the very defects it was built
# for — which is why the wiring is part of the fix and not a follow-up.
#
# ── WHAT IT ASSERTS ───────────────────────────────────────────────────────
# The ledger has two row kinds and the gate is two-sided by construction
# (Core #13 — a gate that has never been seen to fail is not evidence, and one
# that fires on everything is not either):
#   TRIP  <fixture> <n>   must STILL violate, with exactly <n> violations.
#                         If it stops violating the write site got fixed:
#                         delete the row and lower `CEILING` in
#                         `tests/lints.rs::staging_move_burndown_shrink_only`.
#   CLEAN <fixture>       must NOT violate. These are the four staging sites the
#                         class was found through, all fixed — they keep the
#                         guard honest about its own siblings.
#
# Usage:
#   scripts/staging_move_burndown.sh --check   # CI GATE: exit 1 on any drift
#   scripts/staging_move_burndown.sh --sweep   # re-derive the TRIP set over the
#                                              # WHOLE top-level fixture corpus
#   scripts/staging_move_burndown.sh           # same as --check
#
# COST — regenerate it, do not trust a quoted figure (Core #5):
#   time scripts/staging_move_burndown.sh --check
#   time scripts/staging_move_burndown.sh --sweep
# `--check` builds only the ledger's rows; `--sweep` builds all of
# `tests/fixtures/*.gg`. Both use `--emit-gir`, which runs lowering and the
# validator and stops before the C backend and the linker.
set -uo pipefail
cd "$(dirname "$0")/.."

LEDGER=tests/gaps/STAGING_MOVE_BURNDOWN.txt
GG=${GG_BIN:-target/debug/gg}
# The promoter. CI supplies it through the step's `env:` so the wiring is
# visible in the workflow; the default keeps a local run honest.
PROMOTE=${GG_STAGING_MOVE_GUARD:-fatal}
# Per-fixture deadline. `--emit-gir` stops before the C backend and the linker,
# so 120 s is generous; the knob exists because a loaded multi-agent box is the
# normal case here.
DEADLINE=${GG_BUILD_TIMEOUT_SECS:-120}
SCRATCH=$(mktemp -d)
trap 'rm -rf "$SCRATCH"' EXIT

MODE=check
for arg in "$@"; do
  case "$arg" in
    --check) MODE=check ;;
    --sweep) MODE=sweep ;;
    *) echo "unknown flag: $arg" >&2; exit 2 ;;
  esac
done

if [ ! -x "$GG" ]; then
  echo "staging_move_burndown: no compiler at $GG — run \`cargo build\` first" >&2
  exit 2
fi

# Build one fixture under the promoter. Echoes "<rc> <n_violations>"; rc is read
# off the bare process, never off a pipeline.
probe() {
  local f="$1"
  local err="$SCRATCH/err.txt"
  # Per-fixture deadline, house knob. Without it one hanging fixture hangs the
  # whole CI job instead of failing it — and a hang would surface here as a
  # missing TRIP row, i.e. as the WRONG diagnosis.
  GG_STAGING_MOVE_GUARD="$PROMOTE" timeout "$DEADLINE" "$GG" build "$f" --emit-gir \
    -o "$SCRATCH/out.bin" >/dev/null 2>"$err"
  local rc=$?
  if [ "$rc" -eq 124 ]; then
    echo "TIMEOUT after ${DEADLINE}s: $f" >&2
  fi
  local n=0
  if grep -q 'StagingMoveIntoOwnedSlot' "$err"; then
    # Capture the count with a single anchored substitution. A two-stage
    # `grep -o … | grep -o '[0-9]*'` reads the `2` out of "Tier 2a" instead —
    # measured, and it made the gate red on a correct ledger.
    n=$(sed -n 's/.*consume-site violation: \([0-9][0-9]*\) violation.*/\1/p' "$err" | head -1)
    n=${n:-0}
  fi
  echo "$rc $n"
}

if [ "$MODE" = sweep ]; then
  sampled=0; trips=0
  for f in tests/fixtures/*.gg; do
    sampled=$((sampled + 1))
    read -r rc n <<<"$(probe "$f")"
    if [ "$rc" -eq 101 ] && [ "$n" -gt 0 ]; then
      trips=$((trips + 1))
      printf 'TRIP  %-55s %s\n' "$f" "$n"
    fi
  done
  echo "SAMPLED=$sampled TRIPS=$trips"
  exit 0
fi

fail=0

# ── (1) The TRIP set, reconciled by SET EQUALITY over the WHOLE corpus ──
# Walking only the ledger's own rows would be a selection: it could never see a
# NEW violation somewhere else in the corpus, which is precisely the regression
# this gate exists to catch (Core #15e Q3).
want_trips="$SCRATCH/want.txt"
have_trips="$SCRATCH/have.txt"
: > "$want_trips"
: > "$have_trips"
cleans=0
while read -r kind path want || [ -n "$kind" ]; do
  case "$kind" in ''|'#'*) continue ;; esac
  if [ ! -f "$path" ]; then
    echo "DRIFT: ledger row '$kind $path' — fixture does not exist" >&2
    fail=1
    continue
  fi
  case "$kind" in
    TRIP)  printf '%s\t%s\n' "$path" "$want" >> "$want_trips" ;;
    CLEAN) cleans=$((cleans + 1)) ;;
    *)     echo "DRIFT: unknown ledger row kind '$kind' (expected TRIP or CLEAN)" >&2
           fail=1 ;;
  esac
done < "$LEDGER"

sampled=0
for f in tests/fixtures/*.gg; do
  sampled=$((sampled + 1))
  read -r rc n <<<"$(probe "$f")"
  if [ "$rc" -eq 124 ]; then
    # A hung fixture yields NO trip row, so silence here would read as
    # "clean" — the wrong diagnosis. Fail loudly instead.
    echo "DRIFT: $f timed out under the promoter; the sweep cannot adjudicate it." >&2
    fail=1
  elif [ "$rc" -eq 101 ] && [ "$n" -gt 0 ]; then
    printf '%s\t%s\n' "$f" "$n" >> "$have_trips"
  fi
done
sort -o "$want_trips" "$want_trips"
sort -o "$have_trips" "$have_trips"

gone=$(comm -23 "$want_trips" "$have_trips")
new=$(comm -13 "$want_trips" "$have_trips")
if [ -n "$gone" ]; then
  echo "DRIFT — ledger rows that no longer trip (or whose count moved):" >&2
  printf '%s\n' "$gone" >&2
  echo "  A row that stopped tripping is the WIN: the write site was fixed." >&2
  echo "  Delete it from $LEDGER and lower CEILING in" >&2
  echo "  tests/lints.rs::staging_move_burndown_shrink_only in the SAME commit," >&2
  echo "  so the notch is recorded and cannot be spent again." >&2
  fail=1
fi
if [ -n "$new" ]; then
  echo "DRIFT — violations not in the ledger:" >&2
  printf '%s\n' "$new" >&2
  echo "  A staging site moved an owned-but-live source — a live aliasing" >&2
  echo "  hazard (double-free, or use-after-free if the survivor reallocs)." >&2
  echo "  Fix the WRITE SITE: route it through" >&2
  echo "  LoweringContext::assign_with_move_follow_through, which asks" >&2
  echo "  \`owns AND dead\`. ⛔ This ledger is SHRINK-ONLY — do not park it here." >&2
  fail=1
fi

# ── (2) The CLEAN rows, probed individually ──
# These carry the gate's other side, and they also reach fixtures the top-level
# sweep never sees (`tests/fixtures/security/…` lives in a subdirectory).
while read -r kind path _want || [ -n "$kind" ]; do
  [ "$kind" = CLEAN ] || continue
  [ -f "$path" ] || continue
  read -r rc n <<<"$(probe "$path")"
  if [ "$rc" -eq 124 ]; then
    echo "DRIFT: CLEAN row $path timed out; the gate cannot adjudicate it." >&2
    fail=1
  elif [ "$rc" -eq 101 ] || [ "$n" -gt 0 ]; then
    echo "DRIFT: CLEAN row $path NOW VIOLATES (rc=$rc, violations=$n)." >&2
    fail=1
  fi
done < "$LEDGER"

if [ ! -s "$want_trips" ] && [ "$cleans" -eq 0 ]; then
  echo "staging_move_burndown: ledger has no rows — the gate would pass vacuously" >&2
  exit 1
fi

if [ "$fail" -ne 0 ]; then
  echo "staging_move_burndown --check: FAILED (SAMPLED=$sampled)" >&2
  exit 1
fi
echo "staging_move_burndown --check: OK (SAMPLED=$sampled TRIPS=$(wc -l < "$have_trips" | tr -d ' ') CLEAN=$cleans promoter=$PROMOTE)"
