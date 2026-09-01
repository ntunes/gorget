#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════
# scripts/figures.sh — the BASH accessor for `scripts/figures.db`.
# ═══════════════════════════════════════════════════════════════════════════
#
# Source this; do not run it. Any shell instrument that needs a pinned figure
# reads it through these functions instead of spelling the number — the same
# Layering-rule-3 shape `scripts/clone_meter.sh` gives the clone meter's
# invocation, generalised from one meter to every figure.
#
# ⛔ THE ONE THING THIS READER DOES DIFFERENTLY FROM ITS PRECEDENT, AND WHY.
# `clone_meter_get` matches its key with an awk REGEX (`$0 ~ "^[[:space:]]*"k`).
# That is safe for the precedent's flat keys, and WRONG here: this file's keys
# are DOTTED, and `.` is a regex wildcard — `figures_get clone.stage1.array.pin.value`
# would happily match `cloneXstage1Yarray...`. Every reader of this file
# compares keys as WHOLE STRINGS. awk's `==` below, Python's dict lookup in
# `scripts/figures.py`, and the Rust `figures_db_field` in `tests/lints.rs` all
# do exactly that, and `figures_db_rows_are_wellformed` refuses a key that
# belongs to no declared row so a typo cannot silently read as a wildcard hit.
#
# ⚠ NO PIPE INTO AN EARLY-CLOSING READER (`sed … | head -1`): under
# `set -o pipefail` the writer's SIGPIPE becomes the pipeline's status, so a
# successful lookup can abort the caller. awk reads its whole input.

FIGURES_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIGURES_DB="$FIGURES_ROOT/scripts/figures.db"

# figures_get <key> — the value of a single-valued key, exact-match.
# Fails loud on a missing key: a silently-empty figure is worse than no figure.
figures_get() {
    local key="$1" val
    val=$(awk -F' = ' -v k="$key" '
        !/^[[:space:]]*#/ && index($0, " = ") > 0 {
            name = $1
            sub(/^[[:space:]]+/, "", name)
            sub(/[[:space:]]+$/, "", name)
            if (name == k && !seen) {
                v = substr($0, index($0, " = ") + 3)
                sub(/^[[:space:]]+/, "", v)
                print v
                seen = 1
            }
        }' "$FIGURES_DB")
    if [[ -z "$val" ]]; then
        echo "figures: no key '$key' in $FIGURES_DB" >&2
        return 1
    fi
    printf '%s' "$val"
}

# figures_list <key> — every value of a repeated key, one per line.
figures_list() {
    local key="$1"
    awk -F' = ' -v k="$key" '
        !/^[[:space:]]*#/ && index($0, " = ") > 0 {
            name = $1
            sub(/^[[:space:]]+/, "", name)
            sub(/[[:space:]]+$/, "", name)
            if (name == k) {
                v = substr($0, index($0, " = ") + 3)
                sub(/^[[:space:]]+/, "", v)
                print v
            }
        }' "$FIGURES_DB"
}

# figures_rows — every declared row id.
figures_rows() { figures_list row; }

# figures_field <row> <field> — one field of one row.
figures_field() { figures_get "$1.$2"; }

# figures_value <row> — the figure itself, separator-normalised so a caller
# never has to know which of the three separator spellings the DB used.
# ⚠ THE EXAMPLE THAT USED TO BE HERE WAS A LIVE FIGURE. This comment spelled a
# real ratchet ceiling three times to illustrate the three forms, and
# `figures.py --scan` reported all three against that ceiling's own row —
# unplanted, on the author, inside the accessor written to retire the class.
# ⚠ NORMALISATION LIVES IN THE READER, not in a per-row list of spellings: both
# the comma and the underscore form are in live use across the declared scan
# roots, so a reader that knows only one of the two is silently blind to the
# other. Regenerate the split with `python3 scripts/figures.py --spellings` and
# do NOT paste its numbers here: the previous version of this comment cited a
# tally a round-open re-seed had already voided, and this file is itself a
# scanned root, so the number would move as you wrote it.
figures_value() { figures_field "$1" value | tr -d '_,'; }
