#!/usr/bin/env bash
# Run the two BOX-RECEIVER guards under their promoter and reconcile the outcome
# against `tests/gaps/BOX_RECEIVER_BURNDOWN.txt`.
#
# ── WHY THIS EXISTS ───────────────────────────────────────────────────────
# Two guards live in `src/lir/validate.rs`, behind `GG_BOX_RECEIVER_GUARD`:
#
#   validate_box_wrapper_abi              the DECLARATION. `emit_box_wrapper`
#                                         DEFINES `Box__T__get`/`__set`/
#                                         `__get_ptr` taking the handle BY
#                                         VALUE; `ensure_extern` synthesizes
#                                         the DECLARATION from the CALLER's
#                                         argument types. When they disagree
#                                         the callee reads the borrow's target
#                                         address as the handle.
#   validate_box_get_ptr_result_consumed  the VALUE. The D36 `get_ptr` receiver
#                                         projection's result must be READ.
#                                         `todo/t1513`: the struct-field branch
#                                         re-derives the receiver from the AST
#                                         and the projection goes dead.
#
# NEITHER IS A REFINEMENT OF THE OTHER, and this script exists because the
# alternative was two `eprintln`s behind an env var nothing sets. The precedent
# is `scripts/staging_move_burndown.sh`: a promoter nobody drives is theatre.
#
# ⛔ AND THE DECLARATION GUARD IS NOT PROMOTED TO `fatal`. Under the shipping
# fix its residual is 0 on every constructed cell — and `fatal` over a
# 0-residual guard would stand GREEN over `todo/t1513`'s program, which prints
# garbage at rc 0. Worse, its count is a function of PROGRAM SHAPE, not of
# defect count: it fires once PER SYMBOL (a file with five broken receiver
# places reports 1), and putting a correct owned-local read in front of a broken
# one takes a genuinely-broken program to 0. A promotion needs a CORPUS
# argument, not the closure of one item.
#
# ── WHAT IT ASSERTS ───────────────────────────────────────────────────────
# Three row kinds. Every one is an OUTCOME of RUNNING the compiler:
#
#   TRIP  <fixture> <decl_n> <val_n>   must STILL violate, with exactly those
#                                      counts. If a count drops the write site
#                                      was fixed: delete the row and lower the
#                                      ceiling in `tests/lints.rs::
#                                      box_receiver_burndown_shrink_only`.
#   CLEAN <fixture>                    must NOT violate — AND must still
#                                      EXERCISE the mechanism.
#
# ⭐⭐ `CLEAN` IS RECONCILED AGAINST THE `subject=` CENSUS, NOT AGAINST ZERO,
# and that is the whole reason this gate is not the vacuous one its own class
# already shipped twice. Both prototype guards were reported as "0 fires across
# the corpus" when the truth was that 96% of those programs never emit the
# instruction the guard inspects. `run_box_receiver_guards` therefore always
# prints `[box-receiver] subject=N decl=D val=V`, and a `CLEAN` row with
# `subject=0` is DRIFT, not a pass: it means the fixture stopped exercising the
# route it was committed to hold.
#
# Usage:
#   scripts/box_receiver_burndown.sh --check   # CI GATE: exit 1 on any drift
#   scripts/box_receiver_burndown.sh --sweep   # re-derive the TRIP set over the
#                                              # WHOLE top-level fixture corpus
#   scripts/box_receiver_burndown.sh           # same as --check
#
# COST — regenerate it, do not trust a quoted figure (Core #5):
#   time scripts/box_receiver_burndown.sh --check
# `--emit-lir` runs lowering, SSA construction and `promote_runtime_calls`, then
# stops before the C backend and the linker. That is the cheapest mode that
# reaches the guards' checkpoint — and it has to be post-SSA, because pre-SSA
# the projection's result is still consumed by the slot store that follows it,
# so the VALUE guard reads every cell as live.
set -uo pipefail
cd "$(dirname "$0")/.."

LEDGER=tests/gaps/BOX_RECEIVER_BURNDOWN.txt
GG=${GG_BIN:-target/debug/gg}
# The promoter. CI supplies it through the step's `env:` so the wiring is
# visible in the workflow; the default keeps a local run honest. `count`, NOT
# `fatal` — see the block above.
PROMOTE=${GG_BOX_RECEIVER_GUARD:-count}
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
  echo "box_receiver_burndown: no compiler at $GG — run \`cargo build\` first" >&2
  exit 2
fi

# Probe one fixture. Echoes "<rc> <subject> <decl> <val>"; rc is read off the
# BARE process, never off a pipeline (a `| tail` reports the pipe's status,
# which has greened a red gate three times in one session).
probe() {
  local f="$1"
  local err="$SCRATCH/err.txt"
  GG_BOX_RECEIVER_GUARD="$PROMOTE" timeout "$DEADLINE" "$GG" build "$f" --emit-lir \
    >/dev/null 2>"$err"
  local rc=$?
  if [ "$rc" -eq 124 ]; then
    echo "TIMEOUT after ${DEADLINE}s: $f" >&2
  fi
  # One census line per compilation; take the LAST in case a driver ever runs
  # the pipeline more than once.
  local line
  line=$(grep '^\[box-receiver\] ' "$err" | tail -1)
  local subject=0 decl=0 val=0
  if [ -n "$line" ]; then
    subject=$(printf '%s' "$line" | sed -n 's/.*subject=\([0-9][0-9]*\).*/\1/p')
    decl=$(printf '%s' "$line" | sed -n 's/.*decl=\([0-9][0-9]*\).*/\1/p')
    val=$(printf '%s' "$line" | sed -n 's/.*val=\([0-9][0-9]*\).*/\1/p')
  fi
  echo "$rc ${subject:-0} ${decl:-0} ${val:-0}"
}

if [ "$MODE" = sweep ]; then
  sampled=0; trips=0; subjects=0
  for f in tests/fixtures/*.gg tests/fixtures/self_host_gaps/*.gg \
           tests/fixtures/known_gaps/*.gg; do
    [ -f "$f" ] || continue
    sampled=$((sampled + 1))
    read -r rc subject decl val <<<"$(probe "$f")"
    [ "$subject" -gt 0 ] && subjects=$((subjects + 1))
    if [ "$decl" -gt 0 ] || [ "$val" -gt 0 ]; then
      trips=$((trips + 1))
      printf 'TRIP  %-58s %s %s\n' "$f" "$decl" "$val"
    fi
  done
  echo "SAMPLED=$sampled SUBJECT_BEARING=$subjects TRIPS=$trips"
  exit 0
fi

fail=0

# ── (1) The TRIP set, reconciled by SET EQUALITY over the WHOLE corpus ──
# Walking only the ledger's own rows would be a selection: it could never see a
# NEW violation somewhere else, which is precisely the regression this gate
# exists to catch (SIX-Q #3).
want_trips="$SCRATCH/want.txt"
have_trips="$SCRATCH/have.txt"
: > "$want_trips"
: > "$have_trips"
cleans=0
while read -r kind path a b || [ -n "$kind" ]; do
  case "$kind" in ''|'#'*) continue ;; esac
  if [ ! -f "$path" ]; then
    echo "DRIFT: ledger row '$kind $path' — fixture does not exist" >&2
    fail=1
    continue
  fi
  case "$kind" in
    TRIP)  printf '%s\t%s\t%s\n' "$path" "$a" "$b" >> "$want_trips" ;;
    CLEAN) cleans=$((cleans + 1)) ;;
    *)     echo "DRIFT: unknown ledger row kind '$kind' (expected TRIP or CLEAN)" >&2
           fail=1 ;;
  esac
done < "$LEDGER"

sampled=0
for f in tests/fixtures/*.gg tests/fixtures/self_host_gaps/*.gg \
         tests/fixtures/known_gaps/*.gg; do
  [ -f "$f" ] || continue
  sampled=$((sampled + 1))
  read -r rc subject decl val <<<"$(probe "$f")"
  if [ "$rc" -eq 124 ]; then
    # A hung fixture yields NO trip row, so silence here would read as "clean"
    # — the wrong diagnosis. Fail loudly instead.
    echo "DRIFT: $f timed out under the promoter; the sweep cannot adjudicate it." >&2
    fail=1
  elif [ "$decl" -gt 0 ] || [ "$val" -gt 0 ]; then
    printf '%s\t%s\t%s\n' "$f" "$decl" "$val" >> "$have_trips"
  fi
done
sort -o "$want_trips" "$want_trips"
sort -o "$have_trips" "$have_trips"

gone=$(comm -23 "$want_trips" "$have_trips")
new=$(comm -13 "$want_trips" "$have_trips")
if [ -n "$gone" ]; then
  echo "DRIFT — ledger rows that no longer trip (or whose counts moved):" >&2
  printf '%s\n' "$gone" >&2
  echo "  A row that stopped tripping is the WIN: the write site was fixed." >&2
  echo "  Delete it from $LEDGER and lower the ceiling in" >&2
  echo "  tests/lints.rs::box_receiver_burndown_shrink_only in the SAME commit," >&2
  echo "  so the notch is recorded and cannot be spent again." >&2
  fail=1
fi
if [ -n "$new" ]; then
  echo "DRIFT — violations not in the ledger:" >&2
  printf '%s\n' "$new" >&2
  echo "  A Box wrapper is declared with a POINTER receiver where the emitter" >&2
  echo "  defines it BY VALUE (silent wrong output at rc 0), or a D36 receiver" >&2
  echo "  projection went dead. Fix the WRITE SITE:" >&2
  echo "  \`deref_by_value_handle_receiver\` in src/ir/lowering/exprs/methods.rs" >&2
  echo "  is the by-value receiver chokepoint. ⛔ SHRINK-ONLY — do not park it here." >&2
  fail=1
fi

# ── (2) The CLEAN rows, probed individually ──
# Their job is to keep the guards honest about their own siblings, and the
# `subject=` census is what stops them being vacuous.
while read -r kind path _a _b || [ -n "$kind" ]; do
  [ "$kind" = CLEAN ] || continue
  [ -f "$path" ] || continue
  read -r rc subject decl val <<<"$(probe "$path")"
  if [ "$rc" -eq 124 ]; then
    echo "DRIFT: CLEAN row $path timed out; the gate cannot adjudicate it." >&2
    fail=1
  elif [ "$decl" -gt 0 ] || [ "$val" -gt 0 ]; then
    echo "DRIFT: CLEAN row $path NOW VIOLATES (decl=$decl, val=$val)." >&2
    fail=1
  elif [ "$subject" -eq 0 ]; then
    echo "DRIFT: CLEAN row $path no longer EXERCISES the mechanism (subject=0)." >&2
    echo "  Its zero is vacuous: the program emits no Box wrapper extern at all," >&2
    echo "  so the guards cannot go red on it however broken the compiler is." >&2
    echo "  Restore the Box read, or delete the row and lower CLEAN_FLOOR." >&2
    fail=1
  fi
done < "$LEDGER"

if [ ! -s "$want_trips" ] && [ "$cleans" -eq 0 ]; then
  echo "box_receiver_burndown: ledger has no rows — the gate would pass vacuously" >&2
  exit 1
fi

if [ "$fail" -ne 0 ]; then
  echo "box_receiver_burndown --check: FAILED (SAMPLED=$sampled)" >&2
  exit 1
fi
echo "box_receiver_burndown --check: OK (SAMPLED=$sampled TRIPS=$(wc -l < "$have_trips" | tr -d ' ') CLEAN=$cleans promoter=$PROMOTE)"
