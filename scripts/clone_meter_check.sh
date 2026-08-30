#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════
# scripts/clone_meter_check.sh — the ZERO-BUILD half of the clone ratchet.
# ═══════════════════════════════════════════════════════════════════════════
#
#   scripts/clone_meter_check.sh --track --base <sha> [--tip <sha>] --report <file>
#       The TRACK GATE, and the check the output-review runs. Answers: did this
#       track's diff touch the meter's declared closure, and if so does its
#       report carry the required attribution section?
#
#   scripts/clone_meter_check.sh --pin-staleness
#       Is each pinned constant still the last measurement? A pin whose
#       PINNED-BY commit predates a closure change is STALE — the number in the
#       tree is no longer what the meter would print.
#
# ⚠ WHY THIS IS NOT A `tests/lints.rs` LINT. A lint sees a TREE; both questions
# above are about a DIFF, and the failure class is an ABSENCE — nobody measured.
# A provenance line can prove WHO wrote a pin; it can never prove that anyone
# measured. The only tree-adjacent signal that can is history against the
# declared closure, which is what this script computes. Its RED case is
# demonstrated in `tests/lints.rs::clone_meter_check_refuses_an_unattributed_track`.
#
# It builds NOTHING: git + grep only, so an output-review that is barred from
# building can still run it.

set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
# shellcheck source=scripts/clone_meter.sh
source "$ROOT/scripts/clone_meter.sh"

MODE=""
BASE=""
TIP=""
REPORT=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --track)         MODE=track; shift ;;
        --pin-staleness) MODE=staleness; shift ;;
        --base)          BASE="$2"; shift 2 ;;
        # --tip defaults to HEAD; naming it lets a reviewer check a branch tip
        # without checking it out, and lets the lint below pin an exact range.
        --tip)           TIP="$2"; shift 2 ;;
        --report)        REPORT="$2"; shift 2 ;;
        *) echo "usage: $0 --track --base <sha> [--tip <sha>] --report <file> | $0 --pin-staleness" >&2; exit 2 ;;
    esac
done

CLOSURE_ROOTS=$(clone_meter_get closure_roots)
STAGE1_ROOTS=$(clone_meter_get stage1_closure_roots)
GATE_FILE=tests/integration.rs

# touched_closure <base> <tip> — the closure paths the range changed, one per line.
touched_closure() {
    # shellcheck disable=SC2086
    git diff --name-only "$1" "$2" -- $CLOSURE_ROOTS
}
touched_stage1() {
    # shellcheck disable=SC2086
    git diff --name-only "$1" "$2" -- $STAGE1_ROOTS
}

fail() { echo "❌ $*" >&2; FAILED=1; }
FAILED=0

track_mode() {
    [[ -n "$BASE" ]] || { echo "--track needs --base <sha>" >&2; exit 2; }
    local tip; tip=$(git rev-parse "${TIP:-HEAD}")
    local touched s1
    touched=$(touched_closure "$BASE" "$tip")
    s1=$(touched_stage1 "$BASE" "$tip")

    echo "clone-meter track check"
    echo "  base   : $BASE"
    echo "  tip    : $tip"
    echo "  closure: $(echo "$touched" | grep -c '[^[:space:]]') file(s) changed"

    # (1) Tracks do not re-pin. The pin has exactly ONE writer, the integrating
    #     parent; a track editing a constant is a red flag, not a fix.
    # ⚠ `| grep -q` would be WRONG here, and was: under `set -o pipefail` grep
    #   exits on its first match, git diff takes SIGPIPE, and the pipeline
    #   reports 141 — so the branch never fired on a diff that DID move a pin.
    #   `grep -c` consumes all of its input. (Found by running this check
    #   against the very commit that introduced the two-number model.)
    local pin_moves
    pin_moves=$(git diff "$BASE" "$tip" -- "$GATE_FILE" |
        grep -cE '^[+-] *(const [A-Z0-9_]*_CLONE_(PIN|ROUND_OPEN):|// (PINNED-BY|ROUND-OPENED-BY):)')
    if [[ "${pin_moves:-0}" -gt 0 ]]; then
        fail "this diff moves a clone PIN or ROUND-OPEN constant ($pin_moves changed line(s)).
   Tracks REPORT deltas; only the integrating parent writes a pin, and the round-open anchor
   moves once, at round open. The one exception is a track that RE-DEFINES the meter — then the
   pin is re-seeded and the citation comment must say so."
    fi

    if [[ -z "$touched" ]]; then
        echo "  ⇒ NO MEASUREMENT REQUIRED — the diff touches nothing in the declared closure."
        [[ $FAILED -eq 0 ]] && echo "✅ clone-meter track check PASSED"
        return $FAILED
    fi

    echo "$touched" | sed 's/^/      /'
    echo "  ⇒ MEASUREMENT REQUIRED (stage-0$( [[ -n "$s1" ]] && echo " AND stage-1" ))"

    if [[ -z "$REPORT" || ! -f "$REPORT" ]]; then
        fail "no executor report at '${REPORT:-<unset>}' — the attribution is carried by a REQUIRED
   section of the report, so its ABSENCE is exactly what this gate refuses."
        return 1
    fi

    # (2) The required section. Its three parts are the three things a reader
    #     needs to know the number is real: WHAT ran, WHEN (which commit), and
    #     the verbatim lines the gate printed.
    grep -q '^CLONE-METER-CMD:' "$REPORT" || fail "report has no 'CLONE-METER-CMD:' line (the exact command run)"
    local at
    at=$(awk '/^CLONE-METER-AT:/ && !seen { sub(/^CLONE-METER-AT:[[:space:]]*/, ""); print; seen = 1 }' "$REPORT")
    if [[ -z "$at" ]]; then
        fail "report has no 'CLONE-METER-AT: <sha>' line (the commit the numbers were taken at)"
    elif [[ "$tip" != "$at"* && "$at" != "$tip"* ]]; then
        fail "report measured at '$at', which is not the tip under review ($tip). A number taken at
   another commit attributes another commit's clones."
    fi
    grep -q '^\[clone-ceiling\] array_clone=' "$REPORT" || fail "report has no verbatim '[clone-ceiling] array_clone=' line"
    grep -q '^\[clone-ceiling\] string_clone=' "$REPORT" || fail "report has no verbatim '[clone-ceiling] string_clone=' line"
    if [[ -n "$s1" ]]; then
        grep -q '^\[stage1-clone-ceiling\] array_clone=' "$REPORT" ||
            fail "the diff changes what the SELF-HOST emits, so stage-1 is required, but the report has
   no '[stage1-clone-ceiling] array_clone=' line. Stage-1 is the only meter that sees a
   self-host-lowering-only clone bomb — the stage-0 ceiling rides straight over it."
        grep -q '^\[stage1-clone-ceiling\] string_clone=' "$REPORT" ||
            fail "report has no verbatim '[stage1-clone-ceiling] string_clone=' line"
    fi

    if [[ $FAILED -eq 0 ]]; then echo "✅ clone-meter track check PASSED"; fi
    return $FAILED
}

staleness_mode() {
    local tip; tip=$(git rev-parse HEAD)
    echo "clone-meter pin staleness (tip $tip)"
    local any=0
    # `// PINNED-BY: <sha> VALUE: <n>` sits directly above each constant.
    while read -r sha; do
        [[ -n "$sha" ]] || continue
        if ! git cat-file -e "$sha^{commit}" 2>/dev/null; then
            fail "PINNED-BY sha $sha does not resolve in this repo"
            continue
        fi
        local moved
        moved=$(touched_closure "$sha" "$tip" | wc -l)
        if [[ "$moved" -gt 0 ]]; then
            echo "  ⚠ STALE: pin from $sha — $moved closure file(s) changed since."
            any=1
        else
            echo "  ✅ fresh: pin from $sha — no closure change since."
        fi
    done < <(sed -n 's|^[[:space:]]*// PINNED-BY:[[:space:]]*\([0-9a-f]\{7,40\}\).*|\1|p' "$GATE_FILE" | sort -u)
    if [[ $any -eq 1 ]]; then
        echo
        echo "A STALE pin is not a failure — mid-round it is the normal state. It means the constant is"
        echo "no longer the last measurement, so the ATTRIBUTION baseline has drifted. The integrating"
        echo "parent re-pins from the number the track already reported (which costs no new build)."
    fi
    return $FAILED
}

case "$MODE" in
    track)     track_mode ;;
    staleness) staleness_mode ;;
    *) echo "usage: $0 --track --base <sha> --report <file> | $0 --pin-staleness" >&2; exit 2 ;;
esac
