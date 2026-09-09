#!/usr/bin/env python3
"""LOWER-ONLY `--bless` write-back for the leak-allowlist pins (Core #6).

═══════════════════════════════════════════════════════════════════════════
WHAT IT DOES, AND WHY IT IS NOT A COUNT COMPARISON
═══════════════════════════════════════════════════════════════════════════

Core #6 says an improvement lowers a pin by AUTO-LOWERING WRITE-BACK, never by
hand and never by widening to `<=`. For most pins that is easy: the regen
measures the tree, so a fall in the measurement IS the evidence. The leak pins
are the hard case, and they are also the ones that churn — over one 120-commit
window they were fifty of the sixty-six pin-value edits in `tests/lints.rs`,
each mirrored again in `scripts/figures.db`, so roughly double that by hand.

They are hard because they are `derived` from a HAND-EDITED DECLARATION. A human
edits `tests/sanitize/LEAK_ALLOWLIST.txt` and the pins are recomputed FROM THAT
EDIT, so "the number went down" is evidence of nothing but the edit. Every
magnitude-based safeguard fails on that, and the failure is structural rather
than a matter of picking a better threshold:

  ⛔ A PARTIAL TRUNCATION IS NON-VACUOUS AND PLAUSIBLE UNDER ANY FLOOR. At the
    limit, ONE accidentally-deleted row is indistinguishable from a legitimate
    one-row burn-down by ANY magnitude bound whatsoever, because the inputs
    contain no independent reading of whether that row deserved to go. A floor
    catches the extreme instance and green-lights the class — SIX QUESTIONS #2.

So the decision input is not the count. It is `scripts/sanitize_sweep.sh`'s own
adjudication, which measures REALITY: `fixed_leak` names allowlist rows for
which the sweep saw no leak record of any class, and `shrunk_class` names, per
row, each class that shed records and the exact number it shed to. The bless
requires the sweep to have NAMED every row the edit touches. Truncation then
dies by construction — rows vanished that the sweep never named.

  ⭐ AND THE DIRECTION OF THE CLAIM INVERTS, WHICH IS WHAT BREAKS THE CIRCLE.
    Before: human edits the allowlist, pin blessed from that edit. After: the
    sweep measures reality, the human's edit must match what it measured, and
    the pins are recomputed from the resulting file.

═══════════════════════════════════════════════════════════════════════════
THE SUBJECT SET IS EVERY CHANGED STEM, AND ANY LOOSENING IS AN OUTRIGHT REFUSAL
═══════════════════════════════════════════════════════════════════════════

🚨 SCOPING THE SUBJECT SET TO "REMOVED OR TIGHTENED" IS A HOLE, AND IT WAS
MEASURED. One legitimate removal plus one loosening — a class count raised by
ten on an untouched row — moves ALL THREE blessable pins DOWN. The loosened stem
is neither removed nor tightened, so it is never in the subject set and not one
condition is evaluated for it; the bless accepts and `cargo test --test lints`
goes green over ten newly tolerated leak records, against that guard's own
assert that *"widening a row's count re-opens the exact case the class column
exists to catch"*.

⛔ AND THE RENAME GUARD DOES NOT SAVE IT: `new_class` is written only for stems
the sweep SAW leaking beyond their row, so a padded row that still leaks exactly
as HEAD declares appears in NO output file at all. The refusal has to key on the
EDIT SHAPE, not on the verdict.

⇒ The subject set is EVERY stem whose parsed signature changed IN ANY
  DIRECTION, and a loosening — a stem added, a class added to a row, a count
  raised, a `+` added — is refused outright. A bless is a burn-down instrument;
  a loosening is a hand edit with a justification, which is the review event the
  pin exists to force.

⊕ THE DIFF IS PARSE-BASED, never textual: a commented-out row must read as a
  REMOVED STEM, not as a modification.

═══════════════════════════════════════════════════════════════════════════
WHAT IT WILL NOT DO
═══════════════════════════════════════════════════════════════════════════

* It writes the INTERSECTION of two sets, and both halves are real: the rows
  this tool knows how to compute (`PINS` below) and the rows the DB classifies
  `bless = auto-lower` (`python3 scripts/figures.py family bless`). ⚠ SAY BOTH.
  Flipping a row to `never` in the DB takes it out of the write set with no edit
  here — which is the property that matters, since it means the tool cannot be
  talked into writing a positive control. But flipping one TO `auto-lower` does
  NOT bring it in: this tool must also know what quantity that row records. The
  classifier is a VETO, not the whole selector.
* `UNCITED_LEAK_CLASS_PAIRS` is `bless = never` and is DELIBERATELY out of
  scope, which costs about one edit in seven and removes a whole class of
  vacuous acceptance. It is computed from the CITATION column, not the leak
  column: appending citations moves it down by a quarter with the leak column
  byte-identical, so an edit-shape predicate has an EMPTY subject set and every
  condition passes over nothing. There is no adjudication for it even in
  principle — the citation column feeds only `retire_due` — and adding a
  citation when you file a leak item is the ENCOURAGED workflow, not an attack.
* It refuses a `+` REMOVAL. Dropping a `+` is a tightening in spirit, but the
  sweep skips a loose class in its per-class loop, so the class appears in no
  output file and there is nothing to adjudicate against. Re-arming a count
  check stays a hand edit.
* ⚠ IT CANNOT MAKE THE SUITE GREEN ON ITS OWN, and it says so rather than
  reporting success: the three un-blessable pins move too, and an edit touching
  them leaves `cargo test --test lints` RED until a human re-pins them with a
  justification. It names them and exits non-zero.
* ⚠ LOWER-ONLY CAPS AT ROUGHLY THREE-FIFTHS OF THE CHURN. Over the measured
  window these constants moved DOWN twenty-eight times and UP twenty-one; the
  raises stay hand edits BY DESIGN, because a raise is an admission and an
  admission is the review event.
* ⚠ MA-5, THE SHARED COUNTER: two tracks each removing a row each measure
  base+themselves, so both are right alone and wrong together. THE INTEGRATING
  PARENT RE-RUNS THIS FROM THE MERGED TREE and takes the measured output; a
  track's bump is provisional.

═══════════════════════════════════════════════════════════════════════════
PROVENANCE
═══════════════════════════════════════════════════════════════════════════

The verdict is refused unless its `head_sha` is the current HEAD and its
`allowlist_sha256` is the sha256 of the allowlist AT HEAD. ⚠ Not of the working
file: the adjudication asks what the sweep measured about the PRE-EDIT rows, so
the operational order is SWEEP FIRST, THEN EDIT. ⚠ And do not overclaim what the
hash buys — it is not a signature, any writer sets both fields, and nothing here
authenticates the writer. It closes ACCIDENT, which is the threat that actually
happens: yesterday's verdict, or one from another branch, picked up off a stable
path and acted on.

⭐ ADJUDICATING AGAINST **HEAD** IS LOAD-BEARING. A row already deleted from the
working file is in neither the sweep's `allow` set nor its `seen` set, so the
instrument would say nothing at all about it.

⭐ A PARTIAL SWEEP IS STRUCTURALLY SAFE — AND A PARTIAL *FILE* IS NOT. Say which,
because the first sentence used to be written as if it covered both. The sweep's
three-state sort puts an allowlisted row with no verdict line into `absent`, one
that was reached but not leak-measured into `unmeasured`, and only a genuinely
measured-clean row into `fixed_leak`, so a `FIXLIST` verdict cannot let an
unswept row masquerade as fixed. That is what makes a targeted verdict cheap
enough to be the normal path, and it is a fact about a sweep of FEWER FIXTURES.

🚨 IT SAYS NOTHING ABOUT A TRUNCATED VERDICT FILE, AND THE POLARITY IS WHAT
BITES. Every section this tool reads refuses by ABSENCE — a stem missing from
`measured` or `fixed_leak` is a refusal — except `new_class`, which refuses by
PRESENCE. So an empty `new_class` list is indistinguishable from "the rename
census ran and found nothing", and a verdict truncated before that section
DISARMS THE RENAME GUARD while every other condition still passes. Measured: the
same rename-shaped edit is REFUSED against the complete verdict and ACCEPTED
against one truncated before `[new_class]`.

⇒ The sweep writes an `[end]` sentinel precisely so a consumer can tell a
complete verdict from a torn one. This tool reads it, and requires every declared
section header to be present — naming the missing one — rather than reading an
absent section as an empty one.

⚠ ERRATUM ON THE PREDICATE: an earlier draft also required a removed stem to be
absent from `new_leak`. That condition is VACUOUS here — `new_leak` names stems
with NO row in the allowlist, and every stem examined is by construction in the
HEAD allowlist — so it is dropped rather than kept as decoration.

    python3 scripts/bless_leak_allowlist.py            # adjudicate, write nothing
    python3 scripts/bless_leak_allowlist.py --apply    # write the pins
    python3 scripts/bless_leak_allowlist.py --self-test  # every refusal + every accept

⚠ THE SELF-TEST IS THE COVERAGE, and `tests/lints.rs::bless_leak_allowlist_self_test`
is what runs it on every commit. A refusal demonstrated once by hand is pinned by
nothing.

Exit: 0 accepted (or nothing to do) · 1 REFUSED · 2 instrument error ·
3 accepted, but an un-blessable pin still needs a hand edit.
"""
from __future__ import annotations

import argparse
import hashlib
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import figures  # noqa: E402  (path set above)
import proc_guard  # noqa: E402

ALLOWLIST = "tests/sanitize/LEAK_ALLOWLIST.txt"
GIT_TIMEOUT = 60

# The sections `scripts/sanitize_sweep.sh::publish_verdict` writes, and the
# sentinel it terminates the file with. ⚠ THIS LIST IS A CONTRACT WITH THAT
# FUNCTION: a section added there and not here is simply not required, and a
# section required here and not written there refuses every verdict. The
# self-test builds its fixtures from this tuple, so the two cannot silently
# disagree about the SHAPE — only about the membership, which is one grep:
#     grep -n 'for _sec in' scripts/sanitize_sweep.sh
REQUIRED_SECTIONS = ("measured", "fixed_leak", "shrunk_class", "new_class",
                     "new_leak", "absent", "unmeasured", "retire_due")
END_SENTINEL = "end"

# WHAT THIS SCRIPT KNOWS HOW TO COMPUTE, keyed by the DB row that records it.
# ⚠ THIS TABLE IS NOT THE POLICY. It is an identity mapping — "the quantity I
# recompute for `rows` is recorded by `sanitize.leak.rows.pin`" — and it decides
# nothing. Whether a row may be WRITTEN comes from the DB's typed `bless`
# classifier, so flipping a row to `never` in `scripts/figures.db` takes it out
# of the write set here with no edit to this file.
# ⚠ RESIDUAL, stated rather than papered over: a SEVENTH pin declared
# `bless = auto-lower` and absent from this table is simply not written. It
# cannot go unnoticed — `sanitize_pinned_constants_are_declared_in_figures_db`
# forces it to have a row, `figures.py validate` forces it to declare the
# classifier, and its guard stays RED — but this script will not mention it by
# name, because it does not know it exists.
PINS = {
    "sanitize.leak.rows.pin": "rows",
    "sanitize.leak.class_pairs.pin": "pairs",
    "sanitize.leak.records.pin": "records",
    "sanitize.leak.loose_signatures.pin": "loose",
    "sanitize.leak.uncited_class_pairs.pin": "uncited",
}

# `<class> x<K> (row says x<A>)` and `<class> gone (row says x<A>)`, joined by
# `"; "`. ⚠ BOTH FORMS ARE LIVE and the second has NO K to parse: it is exactly
# the case for DROPPING a class from a row, which is the common tightening.
# ⚠ AND A SUBSTRING "mentions this class" TEST ACCEPTS A TIGHTENING TO THE WRONG
# COUNT, which is why the number is parsed and compared.
SHRUNK = re.compile(r"^(?P<cls>\S+) (?:x(?P<k>\d+)|(?P<gone>gone)) \(row says x(?P<a>\d+)\)$")


class Refusal(Exception):
    pass


# ── the allowlist parse ────────────────────────────────────────────────────
def parse_allowlist(text):
    """{stem: {class: (records, loose)}} — the SIGNATURE, not the text.

    Comment and blank lines are dropped with exactly the filter
    `sanitize_allowlists_shrink_only` uses, so a COMMENTED-OUT row reads as a
    removed stem rather than as a modification — which a textual diff cannot do.
    """
    out = {}
    for n, line in enumerate(text.split("\n"), 1):
        if line.lstrip().startswith("#") or not line.strip():
            continue
        cols = line.split("\t")
        stem = cols[0].strip()
        if not stem:
            continue
        if stem in out:
            raise Refusal(f"{ALLOWLIST}:{n}: two rows for {stem!r} — one row per fixture")
        sig = {}
        col2 = cols[1].strip() if len(cols) > 1 else ""
        for entry in col2.split(","):
            entry = entry.strip()
            if not entry or entry == "-":
                continue
            sym, _, cnt = entry.rpartition("*")
            loose = cnt.endswith("+")
            if loose:
                cnt = cnt[:-1]
            if not sym or not cnt.isdigit():
                raise Refusal(f"{ALLOWLIST}:{n}: signature {entry!r} is not "
                              f"<top-frame>*<records>[+]")
            sig[sym] = (int(cnt), loose)
        out[stem] = sig
    return out


def citations_of(text):
    """{stem: [(class, todo-id)]} from column 3."""
    out = {}
    for line in text.split("\n"):
        if line.lstrip().startswith("#") or not line.strip():
            continue
        cols = line.split("\t")
        stem = cols[0].strip()
        if not stem:
            continue
        pairs = []
        if len(cols) > 2:
            for e in cols[2].strip().split(","):
                e = e.strip()
                if "=" in e:
                    cls, _, tid = e.partition("=")
                    pairs.append((cls.strip(), tid.strip()))
        out[stem] = pairs
    return out


def census(text):
    """The five totals `sanitize_allowlists_shrink_only` pins, from one parse."""
    sig = parse_allowlist(text)
    cites = citations_of(text)
    rows = len(sig)
    pairs = sum(len(s) for s in sig.values())
    records = sum(c for s in sig.values() for c, _ in s.values())
    loose = sum(1 for s in sig.values() for _, lo in s.values() if lo)
    # A pair is CITED only when its column-3 entry names an item that EXISTS and
    # whose body CONTAINS the pair's top-frame symbol — the same two-part test
    # the lint applies, replicated because the number is what a human must
    # hand-edit and a wrong one would send them to the wrong place.
    body_cache = {}

    def covers(tid, sym):
        if tid not in body_cache:
            try:
                with open(os.path.join(ROOT, "todo", f"{tid}.md"), encoding="utf-8") as fh:
                    body_cache[tid] = fh.read()
            except OSError:
                body_cache[tid] = None
        return body_cache[tid] is not None and sym in body_cache[tid]

    uncited = 0
    for stem, classes in sig.items():
        for sym in classes:
            if not any(c == sym and covers(t, sym) for c, t in cites.get(stem, [])):
                uncited += 1
    return {"rows": rows, "pairs": pairs, "records": records,
            "loose": loose, "uncited": uncited}


# ── the verdict ────────────────────────────────────────────────────────────
def parse_verdict_text(text, origin):
    """Parse, and REFUSE a torn file rather than reading it as an empty one."""
    head, sections, cur = {}, {}, None
    for line in text.split("\n"):
        if line.startswith("#") or not line.strip():
            continue
        if line.startswith("[") and line.endswith("]"):
            cur = line[1:-1]
            sections.setdefault(cur, [])
            continue
        if cur is None:
            k, _, v = line.partition("\t")
            head[k.strip()] = v.strip()
        else:
            sections[cur].append(line)
    # 🚨 REFUSE BY ABSENCE IS NOT UNIFORM ACROSS THESE SECTIONS, so a missing one
    # cannot be read as an empty one. `new_class` is the rename guard and it
    # refuses by PRESENCE: truncate the file above it and the guard is disarmed
    # while everything else still looks answerable.
    missing = [s for s in REQUIRED_SECTIONS if s not in sections]
    if END_SENTINEL not in sections or missing:
        raise Refusal(
            f"the verdict at {origin} is INCOMPLETE — "
            + (f"missing section(s) {missing}" if missing else
               f"no `[{END_SENTINEL}]` sentinel")
            + ".\n"
            f"   A torn or truncated verdict is not a smaller verdict. Most sections here\n"
            f"   refuse by ABSENCE, so a shorter file looks stricter — but `new_class` is\n"
            f"   the RENAME GUARD and it refuses by PRESENCE, so losing it silently DISARMS\n"
            f"   the one check that separates a fixed leak from a renamed stack frame.\n"
            f"   The sweep terminates a complete verdict with `[{END_SENTINEL}]`. Re-run it.")
    return head, sections


def parse_verdict(path):
    try:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
    except OSError as e:
        raise Refusal(
            f"no persisted sweep verdict at {path}: {e}\n"
            f"   Run the sweep first — `bash scripts/sanitize_sweep.sh` publishes one at\n"
            f"   exit, and a FIXLIST run over the fixtures you touched is enough (a partial\n"
            f"   verdict is structurally safe; see this script's header).\n"
            f"   ⚠ SWEEP FIRST, THEN EDIT: the verdict must be taken against the allowlist\n"
            f"   in its HEAD state, or the provenance gate below refuses it forever.")
    return parse_verdict_text(text, path)


def git(*args):
    r = proc_guard.run(["git", *args], timeout=GIT_TIMEOUT, cwd=ROOT)
    if r.timed_out or r.returncode != 0:
        raise Refusal(f"`git {' '.join(args)}` failed: {(r.stderr or '').strip()}")
    return r.stdout


def provenance_problem(head, now, at_head):
    """The comparison, with no git in it — so the self-test can drive it.

    Returns a refusal string, or None. ⚠ `tree_dirty` and `sweep_rc` are
    recorded by the sweep and deliberately NOT read here. `tree_dirty` would be
    wrong to gate on: the normal workflow is to FIX a leak (dirtying `src/`),
    sweep, and then edit the allowlist, so a dirty tree at sweep time is the
    expected state — what must match HEAD is the ALLOWLIST's bytes, and that is
    checked exactly. `sweep_rc` is not read because a sweep that exits 1 on an
    unrelated ceiling still measured these rows correctly, and refusing on it
    would make an unrelated regression block a burn-down.
    """
    if head.get("head_sha") != now:
        return (
            f"the verdict was taken at {head.get('head_sha', '<missing>')} and HEAD is {now}.\n"
            f"   A stale-but-complete verdict is accepted WHOLE if nobody checks: a row that\n"
            f"   regressed two rounds ago still reads present + MEASURED + clean. The\n"
            f"   three-state sort protects against INCOMPLETENESS, never against STALENESS.\n"
            f"   Re-run the sweep at HEAD.")
    want = hashlib.sha256(at_head.encode()).hexdigest()
    got = head.get("allowlist_sha256")
    if head.get("allowlist_path") != ALLOWLIST:
        return f"the verdict adjudicated {head.get('allowlist_path')!r}, not {ALLOWLIST!r}"
    if got != want:
        return (
            f"the verdict adjudicated a DIFFERENT allowlist than HEAD's.\n"
            f"     verdict: {got}\n"
            f"     HEAD:    {want}\n"
            f"   The sweep must be run BEFORE the edit, with the allowlist in its HEAD state:\n"
            f"   this bless asks what the instrument measured about the PRE-EDIT rows, and a\n"
            f"   verdict taken over the edited file cannot answer that.")
    return None


def check_provenance(head, path):
    """Refuse a verdict from a different tree. ACCIDENT, not forgery."""
    now = git("rev-parse", "HEAD").strip()
    at_head = git("show", f"HEAD:{ALLOWLIST}")
    problem = provenance_problem(head, now, at_head)
    if problem:
        raise Refusal(problem)
    return at_head


# ── the diff, and the loosening refusal ────────────────────────────────────
def diff_signatures(before, after):
    """(tightenings, loosenings, removed, added) over EVERY changed stem."""
    tight, loose_, removed, added = {}, [], [], []
    for stem in before:
        if stem not in after:
            removed.append(stem)
    for stem in after:
        if stem not in before:
            added.append(stem)
    for stem in sorted(set(before) & set(after)):
        b, a = before[stem], after[stem]
        changes = []
        for cls, (bc, bl) in b.items():
            if cls not in a:
                changes.append(("dropped", cls, None, bc))
                continue
            ac, al = a[cls]
            if al and not bl:
                loose_.append((stem, f"`+` added to `{cls}` — that switches the row's COUNT "
                                     f"CHECK OFF, which is an admission, not a tightening"))
            elif bl and not al:
                loose_.append((stem, f"`+` removed from `{cls}` — re-arming a count check is "
                                     f"not adjudicable: the sweep SKIPS a loose class in its "
                                     f"per-class loop, so it appears in no output file. Hand "
                                     f"edit it, and move LEAK_LOOSE_SIGNATURES with it"))
            elif ac > bc:
                loose_.append((stem, f"`{cls}` raised from x{bc} to x{ac}"))
            elif ac < bc:
                changes.append(("lowered", cls, ac, bc))
        for cls in a:
            if cls not in b:
                loose_.append((stem, f"class `{cls}` added to the row"))
        if changes:
            tight[stem] = changes
    return tight, loose_, removed, added


# ── adjudication ───────────────────────────────────────────────────────────
def adjudicate(sections, removed, tight):
    measured = set(sections.get("measured", []))
    fixed = set(sections.get("fixed_leak", []))
    new_class = {ln.split("\t")[0] for ln in sections.get("new_class", []) if ln.strip()}
    shrunk = {}
    for ln in sections.get("shrunk_class", []):
        stem, _, payload = ln.partition("\t")
        shrunk[stem] = payload

    bad = []
    for stem in sorted(removed):
        # ⚠ `MEASURED` is redundant here — `fixed_leak` is only reachable for a
        # measured stem — but it is kept because the SAME condition is
        # load-bearing on the tightening branch below, and one predicate with
        # two branches is easier to keep true than two predicates.
        if stem not in measured:
            bad.append((stem, "REMOVED, but this run never leak-MEASURED it. The fixture did "
                              "not build, produced no binary, was killed at the timeout, or "
                              "died inside the sanitizer before the at-exit check. Silence "
                              "from a run that never looked is not evidence of a fix."))
        elif stem not in fixed:
            bad.append((stem, "REMOVED, but the sweep did NOT report it clean — it is not in "
                              "`fixed_leak`, so a leak record of some class was still seen. "
                              "This is the case the count delta cannot distinguish from a "
                              "legitimate burn-down."))
        if stem in new_class:
            bad.append((stem, "REMOVED, and the sweep flagged a NEW LEAK CLASS on it. A frame "
                              "RENAME reads exactly like a fix here and fires `new_class` too "
                              "— resolve which it is before deleting the row."))
    for stem, changes in sorted(tight.items()):
        if stem not in measured:
            bad.append((stem, "TIGHTENED, but this run never leak-MEASURED it. ⚠ `measured` "
                              "demands unanimity across reps while the observed count takes "
                              "the MAX, so a partly-dead run can report a spuriously low "
                              "count — which is precisely a tightening to a wrong number."))
            continue
        if stem in new_class:
            bad.append((stem, "TIGHTENED, and the sweep flagged a NEW LEAK CLASS on it — the "
                              "signature of a frame RENAME, which fires `new_class` and "
                              "`shrunk_class` together."))
            continue
        entries = {}
        for part in shrunk.get(stem, "").split("; "):
            m = SHRUNK.match(part.strip())
            if m:
                entries[m.group("cls")] = ("gone" if m.group("gone") else int(m.group("k")))
        for kind, cls, new, old in changes:
            saw = entries.get(cls)
            if kind == "dropped":
                if saw != "gone":
                    bad.append((stem, f"dropped class `{cls}` (row said x{old}), but the sweep "
                                      f"did not report it GONE — it reported {saw!r}."))
            elif saw != new:
                bad.append((stem, f"lowered `{cls}` to x{new} (row said x{old}), but the sweep "
                                  f"measured {saw!r}. The count is COMPARED, not merely "
                                  f"mentioned: a substring test accepts a tightening to the "
                                  f"WRONG number."))
    return bad


def decide(before, after, sections):
    """THE WHOLE DECISION, as a pure function. -> (kind, rows, shape)

    `kind` is one of:
      `nothing`   no parsed signature changed
      `loosened`  the edit widens the allowlist — refused OUTRIGHT, before any
                  verdict is consulted, because the verdict cannot see it
      `refused`   a changed row the verdict does not support
      `ok`        every changed row is named by the verdict

    ⚠ IT IS PURE SO THAT `--self-test` CAN DRIVE IT. A refusal demonstrated once
    by hand is pinned by nothing; this one is exercised on every run of
    `bless_leak_allowlist_self_test`, including its POSITIVE controls — a
    `decide` that refused everything would satisfy every negative case.
    """
    tight, loosened, removed, added = diff_signatures(before, after)
    shape = (tight, loosened, removed, added)
    if added or loosened:
        rows = [(s, "ADDED as a new row — a new leak is not a burn-down")
                for s in sorted(added)] + sorted(loosened)
        return "loosened", rows, shape
    if not removed and not tight:
        return "nothing", [], shape
    bad = adjudicate(sections, removed, tight)
    if bad:
        return "refused", bad, shape
    return "ok", [], shape


# ── the write-back ─────────────────────────────────────────────────────────
def rewrite_literal(path, symbol, new, root=None):
    """Rewrite `const <symbol>...= <digits>;`, asserting exactly one match."""
    full = os.path.join(root if root is not None else ROOT, path)
    with open(full, encoding="utf-8") as fh:
        lines = fh.read().split("\n")
    hits = []
    for i, line in enumerate(lines):
        t = line.strip()
        rest = t[4:].lstrip() if t.startswith("pub ") else t
        rest = rest[6:] if rest.startswith("const ") else rest
        if not rest.startswith(symbol):
            continue
        after = rest[len(symbol):]
        if not after[:1] in (":", " ", "="):
            continue
        if "=" not in t:
            continue
        hits.append(i)
    if len(hits) != 1:
        raise Refusal(f"{path}: {len(hits)} declaration(s) of `{symbol}`, want exactly one")
    i = hits[0]
    head, _, tail = lines[i].rpartition("=")
    digits = re.match(r"\s*[0-9_,]+", tail)
    if not digits:
        raise Refusal(f"{path}:{i + 1}: `{symbol}` carries no integer literal")
    lines[i] = f"{head}= {new}{tail[digits.end():]}"
    with open(full, "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines))


def rewrite_db_value(rid, new, path=None):
    path = path if path is not None else os.path.join(HERE, "figures.db")
    with open(path, encoding="utf-8") as fh:
        lines = fh.read().split("\n")
    pre = f"{rid}.value = "
    hits = [i for i, l in enumerate(lines) if l.startswith(pre)]
    if len(hits) != 1:
        raise Refusal(f"{path}: {len(hits)} `{pre}` line(s), want exactly one")
    lines[hits[0]] = f"{pre}{new}"
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines))


# ── the self-test ──────────────────────────────────────────────────────────
# ⛔ SIX HUNDRED LINES THAT WRITE TWO TRACKED FILES OWE AUTOMATED COVERAGE, and
# a refusal demonstrated once by hand is pinned by NOTHING: delete the loosening
# branch tomorrow and no gate goes red. That is Core #6 (convert the class into
# an executable guard) and Core #12 (a fixture is not coverage until it has been
# seen to FAIL) applied to the most safety-critical artifact here — the one that
# rewrites a debt pin.
#
# The precedent is `scripts/sanitize_sweep.sh`'s own `run_selftest`: watch every
# detector fire, in this invocation, before trusting any verdict.
#
# ⚠ AND THE POSITIVE CONTROLS ARE NOT DECORATION. A `decide` that refused
# everything would satisfy every negative case below — the same trap the sweep's
# self-test names ("a detector that answered UNMEASURED to everything would
# satisfy the second of these and make the whole gate silent"). Three cases here
# must ACCEPT, and one must report nothing to do.

_BASE = "\n".join([
    "# a synthetic allowlist — the SHAPE of the real one, none of its content",
    "",
    "alpha\tfoo*3,bar*2\tfoo=t0001",
    "beta\tbaz*5",
    "gamma\tqux*1+",
    "delta\tzip*4",
    "epsilon\twib*2",
    "",
])


def _sections(new_class=(), measured=None, fixed=("beta",),
              shrunk=("alpha\tfoo x1 (row says x3); bar gone (row says x2)",)):
    """The verdict a sweep of this synthetic corpus would publish.

    `epsilon` is deliberately absent from `measured`: it is the row the sweep
    never looked at, which must never be retirable.
    """
    if measured is None:
        measured = ("alpha", "beta", "gamma", "delta")
    return {
        "measured": list(measured),
        "fixed_leak": list(fixed),
        "shrunk_class": list(shrunk),
        "new_class": list(new_class),
        "new_leak": [],
        "absent": ["epsilon"],
        "unmeasured": [],
        "retire_due": [],
        END_SENTINEL: [],
    }


def _verdict_text(sections, sha="cafe", alsha="beef", path=ALLOWLIST, upto=None):
    """Render a verdict file. `upto` truncates before that section header."""
    out = ["# synthetic verdict", f"head_sha\t{sha}", f"allowlist_path\t{path}",
           f"allowlist_sha256\t{alsha}", "written_at\tnow"]
    for sec in list(REQUIRED_SECTIONS) + [END_SENTINEL]:
        if upto is not None and sec == upto:
            break
        out.append(f"[{sec}]")
        out.extend(sections.get(sec, []))
    return "\n".join(out) + "\n"


def _edit(base, **kw):
    """Apply one named edit to the synthetic allowlist text."""
    lines = base.split("\n")

    def row(stem):
        hits = [i for i, l in enumerate(lines) if l.split("\t")[0] == stem]
        assert len(hits) == 1, f"{stem}: {len(hits)} rows in the synthetic base"
        return hits[0]

    for op, arg in kw.items():
        if op == "remove":
            del lines[row(arg)]
        elif op == "comment_out":
            i = row(arg)
            lines[i] = "#" + lines[i]
        elif op == "set":
            stem, col2 = arg
            i = row(stem)
            cols = lines[i].split("\t")
            cols[1] = col2
            lines[i] = "\t".join(cols)
        elif op == "add":
            lines.insert(len(lines) - 1, arg)
        else:
            raise AssertionError(f"unknown edit {op}")
    return "\n".join(lines)


def self_test():
    fails = []

    def case(name, want, base=None, work=None, sections=None):
        b = parse_allowlist(base if base is not None else _BASE)
        a = parse_allowlist(work)
        kind, rows, _ = decide(b, a, sections if sections is not None else _sections())
        got = kind
        ok = got == want
        detail = "; ".join(f"{s}: {w[:60]}" for s, w in rows) if rows else "-"
        print(f"  [{'ok  ' if ok else 'FAIL'}] {name:38s} want={want:<9s} got={got:<9s} {detail}")
        if not ok:
            fails.append(name)

    def refuses(name, fn, expect):
        try:
            fn()
        except Refusal as e:
            ok = expect in str(e)
            print(f"  [{'ok  ' if ok else 'FAIL'}] {name:38s} REFUSED"
                  f"{'' if ok else ' (wrong reason: ' + str(e)[:80] + ')'}")
            if not ok:
                fails.append(name)
            return
        print(f"  [FAIL] {name:38s} did NOT refuse")
        fails.append(name)

    print("=== bless_leak_allowlist self-test ===")
    print("-- the edit shape (refusals that need no verdict at all) --")
    # 🚨 THE MIXED EDIT. One legitimate removal plus one loosening: the loosened
    # row is neither removed nor tightened, so a subject set scoped to
    # "removed or tightened" evaluates NOTHING for it — while every blessable
    # total still moves DOWN.
    case("mixed edit (removal + raise)", "loosened",
         work=_edit(_BASE, remove="beta", set=("delta", "zip*14")))
    case("count raised", "loosened", work=_edit(_BASE, set=("delta", "zip*14")))
    case("class added to a row", "loosened",
         work=_edit(_BASE, set=("delta", "zip*4,extra*1")))
    case("row added", "loosened", work=_edit(_BASE, add="zeta\tnew*1"))
    case("`+` added (count check OFF)", "loosened",
         work=_edit(_BASE, set=("delta", "zip*4+")))
    # Re-arming a count check is a tightening in spirit and adjudicable by
    # nothing: the sweep SKIPS a loose class, so it appears in no output file.
    case("`+` removed (unadjudicable)", "loosened",
         work=_edit(_BASE, set=("gamma", "qux*1")))

    print("-- the verdict adjudication --")
    case("THE ATTACK: remove a still-leaking row", "refused",
         work=_edit(_BASE, remove="delta"))
    # The same removal spelled as a comment-out. A textual diff reads a
    # modification; the parse-based one reads a REMOVED STEM.
    case("...spelled as a comment-out", "refused",
         work=_edit(_BASE, comment_out="delta"))
    case("remove a row never MEASURED", "refused", work=_edit(_BASE, remove="epsilon"))
    case("partial truncation (3 rows)", "refused",
         work=_edit(_BASE, remove="delta") .replace("epsilon\twib*2\n", "")
              .replace("gamma\tqux*1+\n", ""))
    case("tighten to the WRONG count", "refused",
         work=_edit(_BASE, set=("alpha", "foo*2,bar*2")))
    case("tighten a class the sweep did not shed", "refused",
         work=_edit(_BASE, set=("beta", "baz*1")))
    # A frame RENAME fires `new_class` and `shrunk_class` together, and the
    # tightening branch is where condition 4 is not a no-op.
    case("RENAME (new_class fires)", "refused",
         work=_edit(_BASE, set=("alpha", "foo*1,bar*2")),
         sections=_sections(new_class=["alpha\tfoo_v2 x1 (class not tolerated)"]))

    print("-- the POSITIVE controls (a detector that refuses everything is silent) --")
    case("no edit", "nothing", work=_BASE)
    case("legitimate removal", "ok", work=_edit(_BASE, remove="beta"))
    case("legitimate count tightening", "ok",
         work=_edit(_BASE, set=("alpha", "foo*1,bar*2")))
    case("legitimate class DROP (`gone`)", "ok",
         work=_edit(_BASE, set=("alpha", "foo*3")))

    print("-- the verdict FILE: torn is not smaller --")
    # 🚨 THE POLARITY ASYMMETRY. Every section above refuses by ABSENCE except
    # `new_class`, which refuses by PRESENCE — so a file truncated above it
    # DISARMS the rename guard while everything else still looks answerable.
    for sec in ("new_class", "measured", "shrunk_class"):
        refuses(f"truncated before [{sec}]",
                lambda s=sec: parse_verdict_text(
                    _verdict_text(_sections(), upto=s), "<synthetic>"),
                "INCOMPLETE")
    refuses("no [end] sentinel",
            lambda: parse_verdict_text(
                _verdict_text(_sections(), upto=END_SENTINEL), "<synthetic>"),
            "INCOMPLETE")
    # ...and the complete file must PARSE, or the four above prove nothing.
    _h, _s = parse_verdict_text(_verdict_text(_sections()), "<synthetic>")
    if sorted(_s.get("fixed_leak", [])) != ["beta"]:
        print("  [FAIL] a COMPLETE verdict did not parse — the refusals above are vacuous")
        fails.append("complete verdict parses")
    else:
        print(f"  [ok  ] {'a complete verdict parses':38s} fixed_leak={_s['fixed_leak']}")

    print("-- provenance (accident, not forgery) --")
    good = {"head_sha": "cafe", "allowlist_path": ALLOWLIST,
            "allowlist_sha256": hashlib.sha256(_BASE.encode()).hexdigest()}
    checks = [
        ("verdict from another commit", {**good, "head_sha": "f00d"}, "HEAD is"),
        ("verdict over another allowlist", {**good, "allowlist_sha256": "00"},
         "DIFFERENT allowlist"),
        ("verdict over another file", {**good, "allowlist_path": "other.txt"}, "not "),
    ]
    for name, head, expect in checks:
        got = provenance_problem(head, "cafe", _BASE)
        ok = got is not None and expect in got
        print(f"  [{'ok  ' if ok else 'FAIL'}] {name:38s} {'refused' if got else 'ACCEPTED'}")
        if not ok:
            fails.append(name)
    if provenance_problem(good, "cafe", _BASE) is not None:
        print("  [FAIL] a MATCHING provenance was refused — the three above are vacuous")
        fails.append("provenance positive control")
    else:
        print(f"  [ok  ] {'a matching provenance is accepted':38s} (positive control)")

    print("-- the write-back --")
    import tempfile
    with tempfile.TemporaryDirectory(prefix="bless_selftest_") as d:
        rs = os.path.join(d, "lints.rs")
        with open(rs, "w", encoding="utf-8") as fh:
            fh.write("    const A_PIN: usize = 12;\n    const B_PIN: usize = 12;\n")
        rewrite_literal(rs, "A_PIN", 7, root=d)
        body = open(rs, encoding="utf-8").read()
        ok = "const A_PIN: usize = 7;" in body and "const B_PIN: usize = 12;" in body
        print(f"  [{'ok  ' if ok else 'FAIL'}] {'rewrites ONLY the named constant':38s}")
        if not ok:
            fails.append("rewrite_literal")
        refuses("refuses an absent constant",
                lambda: rewrite_literal(rs, "NO_SUCH_PIN", 1, root=d), "0 declaration")
        with open(rs, "a", encoding="utf-8") as fh:
            fh.write("    const A_PIN: usize = 9;\n")
        refuses("refuses two declarations",
                lambda: rewrite_literal(rs, "A_PIN", 1, root=d), "2 declaration")
        db = os.path.join(d, "figures.db")
        with open(db, "w", encoding="utf-8") as fh:
            fh.write("x.value = 12\ny.value = 12\n")
        rewrite_db_value("x", 7, path=db)
        body = open(db, encoding="utf-8").read()
        ok = "x.value = 7" in body and "y.value = 12" in body
        print(f"  [{'ok  ' if ok else 'FAIL'}] {'rewrites ONLY the named row':38s}")
        if not ok:
            fails.append("rewrite_db_value")
        refuses("refuses an absent row",
                lambda: rewrite_db_value("zzz", 1, path=db), "0 `zzz.value = `")

    print("-- the DB classification this tool reads --")
    _, db = figures.parse()
    for rid in PINS:
        if rid not in figures.rows(db):
            print(f"  [FAIL] {rid} is not a row in scripts/figures.db")
            fails.append(rid)
        elif figures.one(db, f"{rid}.bless") not in figures.CLASSIFIERS["bless"]["values"]:
            print(f"  [FAIL] {rid} declares no legal `bless`")
            fails.append(rid)
    w = sorted(set(figures.classified(db, "bless", "auto-lower")) & set(PINS))
    n = sorted(set(PINS) - set(w))
    print(f"  [ok  ] {'write set is the TYPED field ∩ this table':38s} auto-lower={len(w)} never={len(n)}")
    if not w or not n:
        print("  [FAIL] the write set is all-or-nothing — the classifier is not discriminating")
        fails.append("classifier discriminates")

    if fails:
        print(f"\n❌ SELF-TEST FAILED: {len(fails)} case(s): {fails}")
        return 1
    print("\nself-test: OK — the loosening refusal fired on every widening shape including")
    print("           the MIXED EDIT, the verdict adjudication refused the attack, a")
    print("           comment-out, an unmeasured row, a wrong count and a rename, a TORN")
    print("           verdict was refused rather than read as a smaller one, provenance")
    print("           refused a foreign commit and a foreign allowlist, the write-back")
    print("           touched only what it named — and every positive control still passed.")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--apply", action="store_true",
                    help="write the pins (default: adjudicate and report only)")
    ap.add_argument("--self-test", action="store_true", dest="self_test",
                    help="drive every refusal and every accept against synthetic inputs")
    ap.add_argument("--verdict", default=os.environ.get(
        "VERDICT_FILE", os.path.join(ROOT, "target/sanitize-verdict/verdict.txt")))
    args = ap.parse_args()
    if args.self_test:
        return self_test()

    _, db = figures.parse()
    for rid in PINS:
        if rid not in figures.rows(db):
            print(f"❌ {rid} is not a row in scripts/figures.db", file=sys.stderr)
            return 2
        if not figures.CLASSIFIERS["bless"]["family"](db, rid):
            print(f"❌ {rid} is not in the `bless` family, so its classifier is unread",
                  file=sys.stderr)
            return 2
    writable = set(figures.classified(db, "bless", "auto-lower")) & set(PINS)

    try:
        head, sections = parse_verdict(args.verdict)
        at_head = check_provenance(head, args.verdict)
        with open(os.path.join(ROOT, ALLOWLIST), encoding="utf-8") as fh:
            working = fh.read()
        before, after = parse_allowlist(at_head), parse_allowlist(working)
        kind, rows, (tight, loosened, removed, added) = decide(before, after, sections)

        print("=== bless: the leak-allowlist pins ===")
        print(f"  verdict    : {args.verdict}")
        print(f"  taken at   : {head.get('head_sha')} ({head.get('written_at')}, "
              f"reps={head.get('reps')}, fixlist={head.get('fixlist')})")
        print(f"  edit shape : {len(removed)} removed · {len(tight)} tightened · "
              f"{len(added)} added · {len(loosened)} loosening(s)")
        print(f"  write set  : {len(writable)} of {len(PINS)} known pin(s) — this tool's "
              f"table ∩ `bless = auto-lower`; the classifier is a VETO, not the selector")

        # 🚨 THE LOOSENING REFUSAL COMES FIRST AND IS OUTRIGHT. It is not one
        # condition among four; it is a claim about the whole edit.
        if kind == "loosened":
            print("\n⛔ REFUSED — THE EDIT LOOSENS THE ALLOWLIST, and a bless is a burn-down")
            print("   instrument. A loosening is a hand edit with a justification, which is")
            print("   the review event the pin exists to force.")
            for stem, why in rows:
                print(f"     {stem}: {why}")
            print("\n   ⚠ Note what did NOT catch this: all three blessable pins can move DOWN")
            print("     across such an edit, and a loosened row that still leaks exactly as")
            print("     HEAD declares appears in NO verdict file at all. Only the edit SHAPE")
            print("     sees it.")
            return 1

        if kind == "nothing":
            print("\n  nothing to bless — the allowlist is byte-equivalent to HEAD's on every")
            print("  parsed signature. No file was written.")
            return 0

        if kind == "refused":
            print(f"\n⛔ REFUSED — {len(rows)} row(s) the sweep's verdict does not support:")
            for stem, why in rows:
                print(f"     {stem}: {why}")
            print("\n   Nothing was written. Either the edit is wrong, or the verdict is not")
            print("   the one that measured it — re-run the sweep over these fixtures")
            print("   (FIXLIST is enough) and adjudicate again.")
            return 1

        print("\n✅ ADJUDICATED — every changed row is named by the sweep's own verdict:")
        for stem in sorted(removed):
            print(f"     RETIRED   {stem}  (fixed_leak: no record of any class)")
        for stem, changes in sorted(tight.items()):
            for kind, cls, new, old in changes:
                shown = "gone" if kind == "dropped" else f"x{new}"
                print(f"     TIGHTENED {stem}  {cls} x{old} -> {shown}")

        now = census(working)
        was = census(at_head)
        print("\n  the pins, recomputed from the resulting file:")
        rc = 0
        pending = []
        for rid, key in sorted(PINS.items(), key=lambda kv: kv[1]):
            mirrors = [m for m in db.get(f"{rid}.mirror", []) if m != "none"]
            pinned = int(figures.norm(figures.one(db, f"{rid}.value")))
            new = now[key]
            mark = figures.one(db, f"{rid}.bless")
            if new > pinned:
                raise Refusal(
                    f"{rid} would RISE {pinned} -> {new}. Lower-only: a raise is an "
                    f"admission and stays a hand edit. (No loosening was detected, so "
                    f"this is a defect in the census, not in the edit.)")
            state = "unchanged" if new == pinned else f"{pinned} -> {new}"
            print(f"     {mark:<10s} {key:<8s} {state:<16s} {rid}")
            if new == pinned:
                continue
            if rid in writable:
                if args.apply:
                    for m in mirrors:
                        path, _, sym = m.rpartition(":")
                        rewrite_literal(path, sym, new)
                    rewrite_db_value(rid, new)
            else:
                pending.append((rid, mirrors, pinned, new))

        # ⚠ AND IT MUST NOT EXIT 0 OVER A RED GUARD. The un-blessable pins move
        # too, and reporting success while `cargo test --test lints` is red is
        # the failure this whole apparatus exists to stop.
        if pending:
            rc = 3
            print("\n⚠ AN UN-BLESSABLE PIN MOVED — `cargo test --test lints` IS RED UNTIL YOU")
            print("  RE-PIN IT BY HAND, with a justification. These are `bless = never` for a")
            print("  reason recorded in each row's `caveat`, and no tool may write them:")
            for rid, mirrors, pinned, new in pending:
                print(f"     {rid}: {pinned} -> {new}   ({' '.join(mirrors)})")
                print(f"        why never: {figures.one(db, f'{rid}.caveat').split('⊕ bless')[-1][:200]}")

        if not args.apply:
            print("\n  DRY RUN — nothing was written. Re-run with `--apply`.")
            return rc
        if writable:
            print("\n  WROTE tests/lints.rs AND the scripts/figures.db mirror. Writing one")
            print("  alone reds `figures_db_mirrors_agree`, so they move together or not at all.")
            print("\n  ⚠ STILL OWED BY A HUMAN: the re-seed narrative above each constant. This")
            print("  wrote the NUMBER; it did not write WHY. Paste the RETIRED/TIGHTENED lines")
            print("  above, with what fixed them.")
            print("  ⚠ MA-5: if this ran on a track's worktree, the counts are PROVISIONAL —")
            print("  the integrating parent re-runs this from the MERGED tree and takes the")
            print("  measured output, never a delta added by hand.")
        print(f"\n  debt moved: rows {was['rows']} -> {now['rows']}, "
              f"pairs {was['pairs']} -> {now['pairs']}, "
              f"records {was['records']} -> {now['records']}")
        return rc
    except Refusal as e:
        print(f"\n⛔ REFUSED — {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
