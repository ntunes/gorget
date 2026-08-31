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
#       ⚠ It presupposes `tests/lints.rs::clone_meter_pins_carry_their_provenance`,
#       which is what asserts the four pins EXIST: on a tree with no provenance
#       lines this mode prints a header, zero rows and exits 0.
#
#   scripts/clone_meter_check.sh --anchor-age
#       ⊕ ITS CALLER IS `tests/lints.rs::clone_band_anchor_is_reseeded_before_
#       work_resumes`, which runs this mode on every `cargo test --test lints`.
#       ⚠ THE LINT NARROWS THE PREDICATE, DELIBERATELY. This mode fails from the
#       moment a round closes until the next one re-anchors, and that window is
#       LEGITIMATE — a raw lint would red the very records commit that closes a
#       round, and a permanently-red ratchet stops being a ratchet. So the lint
#       asks the narrower question: are there commits AFTER the records commit
#       with the anchors still un-reseeded? Sitting on the records commit is
#       green; resuming work on a stale anchor is not.
#       ⚠ RUN THIS AT ROUND OPEN. The ~1% band is anchored at the ROUND-OPEN
#       value, and that anchor is re-seeded by a scheduled human action with no
#       battery gate behind it. Forget the reset and the band silently stops
#       meaning "per round": R48's legitimate +0.9% stacks on R47's +0.5% and
#       reads as +1.4% against a stale anchor — a false owner ask, and the
#       cross-round accumulation the owner explicitly rejected, arrived at by
#       omission. This mode fails once a round has CLOSED since the anchor was
#       set (newest dated `DONE.md` entry newer than `ROUND-OPEN-DATE`), which
#       is the one tree-visible signal that a new round has begun.
#
# ⚠ WHY THE FIRST TWO MODES ARE A SCRIPT AND NOT A LINT — and the claim is now
# scoped, because an earlier revision of this paragraph over-generalised it to
# all three. `--track` and `--pin-staleness` ask about a DIFF, and their failure
# class is an ABSENCE: nobody measured. A provenance line can prove WHO wrote a
# pin; it can never prove that anyone measured. The only tree-adjacent signal
# that can is history against the declared closure, which is what this script
# computes — so those two live here.
# ⊕ `--anchor-age` IS DIFFERENT AND IS NOW LINTED. Its predicate is pure tree
# state (a `DONE.md` date against a `ROUND-OPEN-DATE` line); the objection that
# kept it out of `tests/lints.rs` was about the raw predicate's legitimate RED
# WINDOW, not about tree-visibility, and the lint narrows the predicate instead
# of widening the window.
# ⇒ ALL THREE MODES NOW HAVE A CALLER IN `tests/lints.rs`, which is the only
#   thing that makes any of the paragraphs above a guarantee rather than a hope:
#     --track          clone_meter_check_refuses_an_unattributed_track
#                      (and it DEMONSTRATES the refusal, on every run)
#     --pin-staleness  clone_meter_pin_provenance_shas_resolve
#     --anchor-age     clone_band_anchor_is_reseeded_before_work_resumes
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
        --anchor-age)    MODE=anchor; shift ;;
        --base)          BASE="$2"; shift 2 ;;
        # --tip defaults to HEAD; naming it lets a reviewer check a branch tip
        # without checking it out, and lets the lint below pin an exact range.
        --tip)           TIP="$2"; shift 2 ;;
        --report)        REPORT="$2"; shift 2 ;;
        *) echo "usage: $0 --track --base <sha> [--tip <sha>] --report <file> | $0 --pin-staleness | $0 --anchor-age" >&2; exit 2 ;;
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
    # `CLONE-METER-AT:` is THE COMMIT THE BINARY UNDER MEASUREMENT WAS BUILT
    # FROM, and it must equal the tip under review — a number taken at another
    # commit attributes another commit's clones. (A harness-only track whose
    # diff touches nothing in the closure never reaches this check: it
    # short-circuits above with NO MEASUREMENT REQUIRED, and its report may
    # honestly cite the base, since the base IS the closure state it measured.)
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

anchor_mode() {
    # The band's anchor carries its own date, beside the four ROUND-OPENED-BY
    # lines. A round CLOSES by adding a dated entry at the top of DONE.md, so a
    # DONE.md entry newer than the anchor means a round boundary was crossed
    # without re-seeding it.
    local anchor_date newest_done
    anchor_date=$(awk '/^\/\/ ROUND-OPEN-DATE:/ && !seen { print $3; seen = 1 }' "$GATE_FILE")
    # Plain bracket expressions, not {n} intervals — mawk needs --re-interval.
    newest_done=$(awk '/^- \[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\]/ { print substr($0, 4, 10); exit }' DONE.md)
    echo "clone-meter band anchor"
    echo "  ROUND-OPEN-DATE : ${anchor_date:-<MISSING>}"
    echo "  newest DONE.md  : ${newest_done:-<none>}"
    if [[ -z "$anchor_date" ]]; then
        fail "no '// ROUND-OPEN-DATE: <YYYY-MM-DD>' line beside the anchors — the band's anchor has
   no age, so nothing can tell a fresh anchor from one carried over from a closed round."
        return 1
    fi
    if [[ -n "$newest_done" && "$newest_done" > "$anchor_date" ]]; then
        fail "a round has CLOSED ($newest_done) since this band anchor was set ($anchor_date).
   RE-SEED THE FOUR '..._CLONE_ROUND_OPEN' CONSTANTS from the round-open measurement, and update
   ROUND-OPEN-DATE / ROUND-OPENED-BY. Leaving them is not conservative: the band stops meaning
   'per round' and starts accumulating across rounds, which is the shape the owner rejected."
        return 1
    fi
    echo "  ✅ the anchor is not older than the newest closed round."
    return 0
}

case "$MODE" in
    track)     track_mode ;;
    staleness) staleness_mode ;;
    anchor)    anchor_mode ;;
    *) echo "usage: $0 --track --base <sha> [--tip <sha>] --report <file> | $0 --pin-staleness | $0 --anchor-age" >&2; exit 2 ;;
esac
