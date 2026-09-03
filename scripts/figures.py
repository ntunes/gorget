#!/usr/bin/env python3
"""scripts/figures.py — the PYTHON reader/runner for `scripts/figures.db`.

═══════════════════════════════════════════════════════════════════════════
THE FIGURES DB IS GENERIC. IT IS NOT A CLONE-METER FEATURE.
═══════════════════════════════════════════════════════════════════════════

A "figure" here is any pinned number a guard, a report or a reader depends on:
a ratchet ceiling, a parity floor, an exact meter pin, a benchmark, a census.
The generalising axis is POLARITY — what a checker must DO with the number —
crossed with PROVENANCE — where the number CAME FROM. Subject ("clones",
"parity", "formatter") is just a name and carries no behaviour.

    polarity      the comparison `check` applies to a fresh measurement
      shrink-only   measured <= value      (a ratchet: may fall, never rise)
      grow-only     measured >= value      (a floor:   may rise, never fall)
      exact-pin     measured == value      (± band_pct against band_anchor)
      informational never red              (a report; see G4/`t0860`)

    provenance    where `value` came from — ORTHOGONAL to polarity
      measured    an instrument RAN and printed it; `regen` observes BEHAVIOUR
      derived     copied/computed from a declared `authority`; `regen` READS
                  a declaration rather than measuring anything
      policy      CHOSEN. `regen = none` is legal ONLY here. `authority`
                  records who chose it and where the choice is recorded.

  ⛔ `policy` is NOT a fifth polarity — and the proof is NOT "it crosses more
    than one polarity". That claim shipped once and was false when made
    (`SHAPE_MAX_DEPTH`, the row it counted on, is correctly excluded); the
    census that replaced it — a claim about how many cells `policy` occupies —
    shipped and was falsified in turn. NO CENSUS HERE: a cell tally is the one
    part of this paragraph no lint can police, and the argument never needed it.
    THE TRUE PROOF IS THAT NEITHER AXIS IS A FUNCTION OF THE OTHER:
    `exact-pin` carries both `measured` and `derived`, `shrink-only` carries
    both `measured` and `policy`, and `measured` carries four different
    polarities. Two independent questions about one row.
    `tests/lints.rs::figures_db_axes_are_occupied` fails if that stops holding —
    it asserts OCCUPANCY and INDEPENDENCE, and deliberately no count, which is
    why the fact is a lint now and not a sentence.

═══════════════════════════════════════════════════════════════════════════
THE VALUE IS THE GUARD, NOT THE FILE
═══════════════════════════════════════════════════════════════════════════

A figures file with no lint forbidding undeclared bare literals of a covered
figure is a sixth spelling with better manners. The enforcement is four lints
in `tests/lints.rs` (`figures_db_*`) plus the two runnable modes here:

    python3 scripts/figures.py --validate     schema contract
    python3 scripts/figures.py --scan         the uniqueness guard
    python3 scripts/figures.py --census       raw duplicate-spelling census
    python3 scripts/figures.py check <id>     run `regen`, apply `polarity`
    python3 scripts/figures.py checkall [pfx] every row, sharing regen runs
    python3 scripts/figures.py report         the informational table (round close)
    python3 scripts/figures.py --list         id · polarity · provenance · value
    python3 scripts/figures.py --where <n>    every unmasked spelling of a value
    python3 scripts/figures.py --spellings    the separator census (see rule 1)

⚠ EVERY SPAWN GOES THROUGH `proc_guard.run` with the row's own `cost_secs` as
  the deadline — a bare `subprocess.run` reddens `process_spawn_deadline_arm_count`.

═══════════════════════════════════════════════════════════════════════════
THE TWO SCANNER RULES THAT WERE BOUGHT WITH MEASUREMENTS
═══════════════════════════════════════════════════════════════════════════

1. **SEPARATOR NORMALISATION IS SCANNER-SIDE, NEVER A PER-ROW LIST.** Both the
   comma and the underscore form are in live use across the declared scan roots,
   so a normaliser that knows only one of the two is blind to the other, and
   blind SILENTLY.
   ⛔ THE SPLIT IS A COMMAND, NEVER A NUMBER HERE:
   `python3 scripts/figures.py --spellings`. Two reasons, and the second is the
   funnier: (a) the first version of this paragraph justified the rule with a
   per-row tally — "29 commas, 19 underscores, 1 bare" — measured against the
   PREVIOUS round's clone values, and the round-open re-seed took the comma
   count to zero inside the same round; (b) this file IS a declared scan root,
   so writing the census into it CHANGES the census. A figure that moves when
   you write it down does not belong written down.
   Both sides (the DB value and the scanned text) are normalised by stripping
   `_` and `,` from digit runs, in ONE place. A per-row `spellings` list would
   be one omission opportunity per row (Layering rule 3).

2. **THE MASK IS THE CITATION FORM, NOT QUOTING.** The obvious reuse —
   `tests/lints.rs::width_ratchet_mask_strings` — masks quoted runs, and the
   false positives this scanner actually hits are backtick-delimited Markdown
   coordinates with no quote in them: `docs/devbook/03-lexer.md` carries
   `1460-1476` and `docs/devbook/09-type-checking.md` carries
   `typecheck.rs:1453-1503`. That masker fixes NEITHER, while swallowing
   15.6% of all 3+ digit figures (much of it by unpaired-apostrophe runaway).
   So the mask is a POSITIVE recogniser of the citation form:
       `<path>.<ext>:NNN`  ·  `<path>.<ext>:NNN:MMM`  ·  `<path>.<ext>:NNN-MMM`
       bare `NNN-MMM`
   minus ISO dates, which are recognised FIRST so `2026-08-31` is never read
   as a range. ⚠ The single `path:NNN` form is the COMMONER of the two and
   collides today on ten distinct covered values.


⛔ WHAT THE SCANNER CANNOT SEE. State the reach; never imply totality. `src/` is
  not a root (a DECLARED `home`/`mirror` path there is always opened, an
  undeclared re-spelling is not); `tests/*.rs` other than `integration.rs` and
  `lints.rs` are unscanned; `docs/` outside `docs/devbook/` is unscanned; and
  the extension filter opens only `.md .rs .py .sh .spec .toml`, so `.gg`,
  `.json`, `.tsv` and extensionless files inside a root are invisible. The full
  list with its per-file counts lives beside `figures.scan_roots` in
  `scripts/figures.db`.

⚠ DELIBERATELY NOT STRIPPED: COMMENTS. The scanner reads comment text, which
  is what lets it catch a live figure planted in the doc comment of the very
  lint that forbids one. `figures_db_scanner_sees_into_doc_comments` is the
  standing test that keeps that property through the next refactor.
"""

import argparse
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import proc_guard  # noqa: E402  (path set above)

# ⚠ THE OVERRIDE EXISTS FOR ONE REASON: so a guard can be SEEN TO FAIL. Core
# #13 — a gate that has never gone red is not evidence — and the only honest way
# to red a schema contract is to hand it a deliberately broken declaration.
# `tests/lints.rs::figures_db_rows_are_wellformed` writes seven broken variants
# to a temp file and points this at each of them, on every run.
DB = os.environ.get("FIGURES_DB") or os.path.join(HERE, "figures.db")

POLARITIES = ("shrink-only", "grow-only", "exact-pin", "informational")
PROVENANCES = ("measured", "derived", "policy")

# Every row owes all of these. `caveat` and `cost_secs` are required WITH AN
# ESCAPE VALUE (`none` / `unmeasured`) so that "this figure has no caveat" is a
# claim someone made, not a field someone forgot.
REQUIRED = (
    "value", "unit", "polarity", "provenance", "regen", "regen_extract",
    "regen_fires", "regen_env", "instrument", "measured_at", "home", "scan",
    "caveat", "cost_secs",
)
# Repeatable / conditional.
MULTI = ("mirror", "waiver", "input")
OPTIONAL = ("authority", "band_pct", "band_anchor", "regen_forbids", "note")

WAIVER_KINDS = ("declared", "debt")


# ── the dialect ────────────────────────────────────────────────────────────
# EXACTLY `scripts/clone_meter.spec`'s restricted dialect: `key = value`, one
# per line, WHOLE-LINE `#` comments only, no trailing whitespace, blank lines
# ignored, a repeated key is a list. ⚠ Keys are compared as WHOLE STRINGS by
# every reader — the dotted keys here would otherwise match `clone_meter_get`'s
# regex spelling, where `.` is a wildcard.
class DialectError(Exception):
    pass


def parse(path=DB):
    """Return (order, {key: [values]}) — keys compared as whole strings."""
    out = {}
    order = []
    with open(path, "r", encoding="utf-8") as fh:
        text = fh.read()
    for n, raw in enumerate(text.splitlines(), 1):
        if raw.strip() == "":
            continue
        if raw.lstrip().startswith("#"):
            if raw != raw.rstrip():
                raise DialectError(f"{path}:{n}: trailing whitespace on a comment line")
            continue
        if raw != raw.rstrip():
            raise DialectError(f"{path}:{n}: trailing whitespace — awk strips only the "
                               f"leading run, so it would reach a shell reader intact")
        if "=" not in raw:
            raise DialectError(f"{path}:{n}: not `key = value` and not a whole-line `#` "
                               f"comment: {raw!r}")
        key, val = raw.split("=", 1)
        key, val = key.strip(), val.strip()
        if not key:
            raise DialectError(f"{path}:{n}: empty key")
        if key not in out:
            out[key] = []
            order.append(key)
        out[key].append(val)
    return order, out


def one(db, key, default=None):
    v = db.get(key)
    if not v:
        return default
    return v[-1]


def rows(db):
    return list(db.get("row", []))


# ── separator normalisation, in ONE place (scanner rule 1) ─────────────────
def norm(tok):
    # ⚠ THE EXAMPLE IS DELIBERATELY NOT A LIVE FIGURE. The first draft of this
    # docstring spelled a real clone pin four times to illustrate the three
    # forms, and `figures.py --census` reported all four against the very row
    # that pin belongs to — unplanted, on the author, in the normaliser's own
    # docstring. The scanner does not strip comments, and that is why.
    """`1_234_567` / `1,234,567` / `1234567` all become `1234567`."""
    return tok.replace("_", "").replace(",", "")


NUM = re.compile(r"[0-9](?:[0-9_,]*[0-9])?")
ISO = re.compile(r"[0-9]{4}-[0-9]{2}-[0-9]{2}")
# A path-ish token ending in an extension, then `:NNN`, `:NNN:MMM` or `:NNN-MMM`.
CITE = re.compile(r"[A-Za-z0-9_./-]+\.[A-Za-z][A-Za-z0-9_]*:([0-9]+)(?::([0-9]+))?(?:-([0-9]+))?")
BARE_RANGE = re.compile(r"(?<![0-9A-Za-z_])([0-9]+)-([0-9]+)(?![0-9])")


def mask_citations(line):
    """Blank out coordinate citations so `lexer.md:1476` is not a spelling of 1476.

    ISO dates are recognised FIRST and never masked, so `2026-08-31` is not
    read as the bare range `2026-08`.
    """
    chars = list(line)
    protected = [False] * len(chars)
    for m in ISO.finditer(line):
        for i in range(m.start(), m.end()):
            protected[i] = True

    def blank(s, e):
        if any(protected[i] for i in range(s, e)):
            return
        for i in range(s, e):
            chars[i] = " "

    for m in CITE.finditer(line):
        for g in (1, 2, 3):
            if m.group(g) is not None:
                blank(*m.span(g))
    for m in BARE_RANGE.finditer(line):
        blank(*m.span())
    return "".join(chars)


def occurrences(text, wanted):
    """{value: count} of unmasked, separator-normalised digit runs in `text`."""
    hits = {v: 0 for v in wanted}
    for line in text.splitlines():
        masked = mask_citations(line)
        for m in NUM.finditer(masked):
            v = norm(m.group(0))
            if v in hits:
                hits[v] += 1
    return hits


def occurrence_lines(text, value):
    out = []
    for n, line in enumerate(text.splitlines(), 1):
        for m in NUM.finditer(mask_citations(line)):
            if norm(m.group(0)) == value:
                out.append((n, line.strip()))
    return out


# ── scan roots ─────────────────────────────────────────────────────────────
def standard_roots(db):
    return one(db, "figures.scan_roots", "").split()


def scan_files(db, spec):
    """Expand a row's `scan` field to a list of repo-relative files."""
    if spec == "none":
        return []
    roots = standard_roots(db) if spec == "standard" else spec.split()
    skip = set(one(db, "figures.scan_skip", "").split())
    files = []
    for root in roots:
        p = os.path.join(ROOT, root)
        if os.path.isfile(p):
            files.append(root)
            continue
        for dirpath, dirnames, filenames in os.walk(p):
            dirnames[:] = [d for d in dirnames if d not in (".git", "target", "node_modules")]
            for fn in sorted(filenames):
                rel = os.path.relpath(os.path.join(dirpath, fn), ROOT)
                if rel in skip or any(rel.startswith(s.rstrip("/") + "/") for s in skip):
                    continue
                if os.path.splitext(fn)[1] in (".md", ".rs", ".py", ".sh", ".spec", ".toml"):
                    files.append(rel)
    return sorted(set(f for f in files if f not in skip))


# ── validation ─────────────────────────────────────────────────────────────
def validate(db, order):
    errs = []
    ids = rows(db)
    if not ids:
        errs.append("no `row = <id>` declarations — the DB declares its own rows")
    if len(set(ids)) != len(ids):
        errs.append(f"duplicate `row =` declarations: {ids}")

    known = set(ids)
    for key in order:
        if key == "row" or key.startswith("figures."):
            continue
        # `<id>.<field>` or `<id>.input.<n>.<sub>`
        matched = [i for i in known if key.startswith(i + ".")]
        if not matched:
            errs.append(f"key `{key}` belongs to no declared row (typo, or a missing `row =`)")
            continue
        rid = max(matched, key=len)
        field = key[len(rid) + 1:]
        if field.startswith("input."):
            parts = field.split(".")
            if len(parts) != 3 or parts[2] not in ("at", "law", "regen"):
                errs.append(f"`{key}`: an input sub-key is `input.<name>.{{at,law,regen}}`")
            continue
        if field not in REQUIRED + MULTI + OPTIONAL:
            errs.append(f"`{key}`: unknown field `{field}`")

    for rid in ids:
        def f(name, default=None):
            return one(db, f"{rid}.{name}", default)

        for name in REQUIRED:
            if f(name) in (None, ""):
                errs.append(f"{rid}: missing mandatory field `{name}`")
        pol, prov = f("polarity"), f("provenance")
        if pol not in POLARITIES:
            errs.append(f"{rid}: polarity `{pol}` not one of {POLARITIES}")
        if prov not in PROVENANCES:
            errs.append(f"{rid}: provenance `{prov}` not one of {PROVENANCES}")
        val = f("value")
        if val is not None and not norm(val).isdigit():
            errs.append(f"{rid}: value `{val}` is not an integer")

        # regen is a (COMMAND, EXTRACTOR) PAIR. "last integer in stdout" is a
        # silent lie: `c_emit_comparison`'s last integer is 0 and
        # `resolver_comparison`'s is 1. So `regen_extract` is MANDATORY with NO
        # default (exactly `proc_guard.run`'s `timeout` precedent) and must
        # carry a named `value` capture.
        regen, extract = f("regen"), f("regen_extract")
        if regen == "none":
            if prov != "policy":
                errs.append(f"{rid}: `regen = none` is legal only under provenance `policy` "
                            f"(this row is `{prov}`) — a figure with no command is a hope")
            if extract != "none":
                errs.append(f"{rid}: `regen = none` requires `regen_extract = none`")
        else:
            if extract in (None, "none"):
                errs.append(f"{rid}: `regen_extract` is mandatory and has NO default — "
                            f"'last integer in stdout' is wrong for a cargo-test regen")
            elif "(?P<value>" not in extract:
                errs.append(f"{rid}: `regen_extract` needs a named `(?P<value>...)` capture")
            else:
                try:
                    re.compile(extract)
                except re.error as e:
                    errs.append(f"{rid}: `regen_extract` does not compile: {e}")
            if f("regen_fires") in (None, "none"):
                errs.append(f"{rid}: `regen_fires` is mandatory for a runnable regen — a "
                            f"measurement with no FIRE COUNT proves nothing ran")
        for name in ("regen_fires", "regen_forbids"):
            pat = f(name)
            if pat not in (None, "none"):
                try:
                    re.compile(pat)
                except re.error as e:
                    errs.append(f"{rid}: `{name}` does not compile: {e}")
        env = f("regen_env")
        if env not in (None, "none"):
            for item in env.split():
                if "=" not in item:
                    errs.append(f"{rid}: `regen_env` item `{item}` is not NAME=value|NAME=unset")

        # `authority` is mandatory for BOTH non-measured provenances: a derived
        # figure that does not name what it derives FROM is a fifth spelling.
        if prov in ("derived", "policy") and f("authority") in (None, ""):
            errs.append(f"{rid}: provenance `{prov}` requires `authority` — the declaration "
                        f"this value is copied from, or the ratification that chose it")
        if prov == "measured" and f("authority") is not None:
            errs.append(f"{rid}: a `measured` row has no `authority` — its authority is the "
                        f"instrument, already named by `instrument`/`regen`")

        cost = f("cost_secs")
        if cost is not None and cost != "unmeasured" and not cost.isdigit():
            errs.append(f"{rid}: `cost_secs` is seconds or `unmeasured`, got `{cost}`")

        band, anchor = f("band_pct"), f("band_anchor")
        if (band is None) != (anchor is None):
            errs.append(f"{rid}: `band_pct` and `band_anchor` come as a pair")
        if anchor is not None and anchor not in known:
            errs.append(f"{rid}: `band_anchor = {anchor}` names no declared row")
        if band is not None and pol != "exact-pin":
            errs.append(f"{rid}: a band belongs to an `exact-pin` row, not `{pol}`")

        # DECLARED INPUTS: naming one without all three sub-keys is the failure
        # this check exists for — `input = root_path_len` with no law is a
        # rumour, not a declaration.
        for inp in db.get(f"{rid}.input", []):
            if inp == "none":
                continue
            for sub in ("at", "law", "regen"):
                if one(db, f"{rid}.input.{inp}.{sub}") in (None, ""):
                    errs.append(f"{rid}: input `{inp}` is named without `input.{inp}.{sub}`")
        if not db.get(f"{rid}.input"):
            errs.append(f"{rid}: missing mandatory field `input` (write `input = none`)")
        # `mirror` is required WITH AN ESCAPE VALUE, for the same reason `input`
        # is: "this row mirrors no literal" must be a claim someone MADE, not a
        # field someone FORGOT. Until R48 close it was merely repeatable, so a
        # row that omitted it was silently never checked by
        # `figures_db_mirrors_agree` — and that lint's own floor was `>=`, so it
        # could not see the omission either. Two guards, one blind spot.
        if not db.get(f"{rid}.mirror"):
            errs.append(f"{rid}: missing mandatory field `mirror` (write `mirror = none` "
                        f"if this row mirrors no literal in code)")

        for w in db.get(f"{rid}.waiver", []):
            parts = w.split(None, 3)
            if len(parts) < 4 or not parts[1].isdigit() or parts[2] not in WAIVER_KINDS:
                errs.append(f"{rid}: waiver `{w}` is not `<path> <count> "
                            f"<{'|'.join(WAIVER_KINDS)}> <reason>`")
        for m in db.get(f"{rid}.mirror", []):
            if m == "none":
                continue
            if ":" not in m:
                errs.append(f"{rid}: mirror `{m}` is not `<path>:<symbol>`")

    # A value shared by two rows is fine (a PIN and its ROUND-OPEN anchor start
    # equal); declaring the same (value, path) waiver twice is not — the scan
    # accounts per VALUE, so the second copy would double-count.
    seen = {}
    for rid in ids:
        val = norm(one(db, f"{rid}.value", ""))
        for w in db.get(f"{rid}.waiver", []):
            parts = w.split(None, 3)
            if len(parts) < 2:
                continue
            k = (val, parts[0])
            if k in seen:
                errs.append(f"{rid}: waiver for ({val}, {parts[0]}) is already declared by "
                            f"`{seen[k]}` — the scan accounts per VALUE, so this double-counts")
            seen[k] = rid
    return errs


# ── the uniqueness guard ───────────────────────────────────────────────────
def scan(db, verbose=False):
    """Return a list of (value, path, actual, expected, rows) discrepancies."""
    ids = rows(db)
    by_value = {}
    for rid in ids:
        by_value.setdefault(norm(one(db, f"{rid}.value", "")), []).append(rid)

    problems = []
    for value, rids in sorted(by_value.items()):
        specs = {one(db, f"{r}.scan") for r in rids}
        files = []
        for spec in specs:
            files.extend(scan_files(db, spec))
        files = sorted(set(files))
        if not files:
            continue
        expected = {}
        for r in rids:
            for m in db.get(f"{r}.mirror", []):
                if m == "none":
                    continue
                path = m.rsplit(":", 1)[0]
                expected[path] = expected.get(path, 0) + 1
            for w in db.get(f"{r}.waiver", []):
                parts = w.split(None, 3)
                expected[parts[0]] = expected.get(parts[0], 0) + int(parts[1])
        # ⛔ A DECLARED PATH IS ALWAYS SCANNED, EVEN OUTSIDE THE SCAN ROOTS.
        # Without this a row whose `home` is in `src/` — which every `src/`-side
        # adopter will have — declared 1 expected spelling at a file the scan
        # never opened, and read as a FALSE RED (`src/formatter/doc.rs has 0
        # unmasked spelling(s), 1 declared`). Declaring a spelling site is a
        # claim that it EXISTS; the scan has to be able to falsify it.
        files = sorted(set(files) | {p for p in expected if os.path.exists(os.path.join(ROOT, p))})
        actual = {}
        for rel in files:
            try:
                with open(os.path.join(ROOT, rel), "r", encoding="utf-8", errors="replace") as fh:
                    text = fh.read()
            except OSError:
                continue
            n = occurrences(text, {value})[value]
            if n:
                actual[rel] = n
        for path in sorted(set(actual) | set(expected)):
            a, e = actual.get(path, 0), expected.get(path, 0)
            if a != e:
                problems.append((value, path, a, e, rids))
            elif verbose and a:
                print(f"  ok   {value:>12} {path}: {a}")
    return problems


def spellings(db):
    """The separator census — the regenerable evidence for scanner rule 1.

    Reports how the declared scan roots actually spell 4+ digit figures, so the
    rule's justification is a COMMAND rather than a tally that a re-seed can
    void — which is exactly what happened to its first version.
    """
    roots = scan_files(db, "standard")
    covered = {norm(one(db, f"{r}.value", "")) for r in rows(db)}
    tally = {"comma": 0, "underscore": 0, "bare": 0}
    cov = {"comma": 0, "underscore": 0, "bare": 0}
    for rel in roots:
        try:
            with open(os.path.join(ROOT, rel), "r", encoding="utf-8", errors="replace") as fh:
                text = fh.read()
        except OSError:
            continue
        for line in text.splitlines():
            for m in NUM.finditer(mask_citations(line)):
                tok = m.group(0)
                n = norm(tok)
                if len(n) < 4:
                    continue
                kind = "comma" if "," in tok else ("underscore" if "_" in tok else "bare")
                tally[kind] += 1
                if n in covered:
                    cov[kind] += 1
    print("=== separator census over the declared scan roots ===")
    print(f"  files scanned: {len(roots)}")
    print(f"  all 4+ digit figures : comma={tally['comma']} "
          f"underscore={tally['underscore']} bare={tally['bare']}")
    print(f"  of the covered values: comma={cov['comma']} "
          f"underscore={cov['underscore']} bare={cov['bare']}")
    print("  => both separator forms are in live use, which is why normalisation is "
          "scanner-side and not a per-row list.")
    return 0


def census(db):
    """Raw duplicate-spelling census: occurrences beyond the declared mirrors."""
    ids = rows(db)
    by_value = {}
    for rid in ids:
        by_value.setdefault(norm(one(db, f"{rid}.value", "")), []).append(rid)
    total = 0
    print("=== figures.db duplicate-spelling census ===")
    for value, rids in sorted(by_value.items(), key=lambda kv: -int(kv[0])):
        specs = {one(db, f"{r}.scan") for r in rids}
        files = sorted({f for s in specs for f in scan_files(db, s)})
        mirrors = sum(1 for r in rids for m in db.get(f"{r}.mirror", []) if m != "none")
        seen = 0
        detail = []
        for rel in files:
            try:
                with open(os.path.join(ROOT, rel), "r", encoding="utf-8", errors="replace") as fh:
                    text = fh.read()
            except OSError:
                continue
            n = occurrences(text, {value})[value]
            if n:
                seen += n
                detail.append(f"{rel}:{n}")
        dup = max(0, seen - mirrors)
        total += dup
        print(f"  {value:>12}  occurrences={seen:<4} mirrors={mirrors:<3} duplicate={dup:<4} "
              f"[{', '.join(rids)}]")
        for d in detail:
            print(f"        {d}")
    print(f"total duplicate spellings beyond the declared mirrors: {total}")
    return total


# ── running a regen ────────────────────────────────────────────────────────
def _env_for(spec):
    env = dict(os.environ)
    conflicts = []
    if spec and spec != "none":
        for item in spec.split():
            name, want = item.split("=", 1)
            if want == "unset":
                if name in env:
                    conflicts.append(f"{name} was {env[name]!r}; the row requires it UNSET, so it is unset FOR THIS RUN")
                    env.pop(name, None)
            else:
                if name in env and env[name] != want:
                    conflicts.append(f"{name} was {env[name]!r}; the row requires {want!r}, overridden FOR THIS RUN")
                env[name] = want
    return env, conflicts


# ⭐ THE SHARED-REGEN MEMO. Four of the clone rows are read off ONE
# `self_host_clone_ceiling` run and four more off ONE `stage1_clone_ceiling`
# run: the axis and the role differ, the COMMAND does not. Keyed on (command,
# env) so a row that differs only in `regen_extract` costs no second spawn —
# without it, `checkall` would be eight full self-compiles at round close.
_REGEN_CACHE = {}


def run_regen(regen, env_spec, timeout):
    key = (regen, env_spec)
    if key in _REGEN_CACHE:
        return _REGEN_CACHE[key], True
    env, conflicts = _env_for(env_spec)
    for c in conflicts:
        print(f"  ⚠ ENV: {c}")
    # ⚠ proc_guard.run, never subprocess.run: the child leads its own process
    # group so a hung grandchild is killed with it (and `process_spawn_
    # deadline_arm_count` refuses a bare spawn).
    r = proc_guard.run(["bash", "-c", regen], timeout=timeout, env=env, cwd=ROOT)
    _REGEN_CACHE[key] = r
    return r, False


def check(db, rid, timeout=None, quiet=False):
    """Run the row's regen, apply its polarity. Returns 0 / 1."""
    if rid not in rows(db):
        print(f"❌ no such row: {rid}", file=sys.stderr)
        return 2

    def f(name, default=None):
        return one(db, f"{rid}.{name}", default)

    value = int(norm(f("value")))
    pol, prov, regen = f("polarity"), f("provenance"), f("regen")
    print(f"=== {rid} ===")
    print(f"  value      : {value} {f('unit')}")
    print(f"  polarity   : {pol}   provenance: {prov}")
    if regen == "none":
        print(f"  regen      : none (policy) — authority: {f('authority')}")
        print("  ⇒ NOT REPRODUCIBLE BY CONSTRUCTION. `figures_db_mirrors_agree` is what "
              "guards this row; nothing else can.")
        return 0

    cost = f("cost_secs")
    if timeout is None:
        if cost == "unmeasured":
            print(f"❌ {rid}: cost_secs is `unmeasured`; pass --timeout <secs> deliberately",
                  file=sys.stderr)
            return 2
        timeout = int(cost)
    print(f"  regen      : {regen}")
    r, cached = run_regen(regen, f("regen_env"), timeout)
    if cached:
        print("  (reused the shared regen run — same command, same env)")
    out = (r.stdout or "") + (r.stderr or "")
    if r.timed_out:
        print(f"❌ {rid}: regen exceeded cost_secs={timeout}", file=sys.stderr)
        return 1

    fires = f("regen_fires")
    n_fires = len(re.findall(fires, out)) if fires and fires != "none" else 0
    print(f"  fire count : {n_fires}  (pattern {fires!r})")
    if fires and fires != "none" and n_fires == 0:
        print(f"❌ {rid}: the regen produced no evidence that the mechanism RAN. A green run "
              f"with a zero fire count is not a measurement.", file=sys.stderr)
        return 1
    forbids = f("regen_forbids")
    if forbids and forbids != "none":
        bad = re.findall(forbids, out)
        if bad:
            print(f"❌ {rid}: the regen printed its own SKIP notice ({bad[0]!r}) — the number "
                  f"was captured with the mechanism disabled.", file=sys.stderr)
            return 1

    m = re.search(f("regen_extract"), out)
    if not m:
        print(f"❌ {rid}: regen_extract matched nothing in {len(out)} bytes of output",
              file=sys.stderr)
        return 1
    measured = int(norm(m.group("value")))
    print(f"  measured   : {measured}")

    if pol == "informational":
        # ⛔ NEVER RED, AND THE DEFINITION IS LOAD-BEARING. The owner ruled slow
        # clone accumulation UNBOUNDED and explicitly REJECTED a cumulative cap,
        # replacing it with a periodic AUDIT; a table that fails the build would
        # re-introduce the cap through the back door. So an informational row
        # returns 0 whatever it measures — including when it measures worse.
        delta = measured - value
        print(f"  ⇒ INFORMATIONAL — never red. prev {value} · now {measured} · "
              f"delta {delta:+d}")
        print(f"     caveat: {f('caveat')}")
        # A report is its OWN output, not a single extracted integer. The row's
        # instrument already prints the table the reader is meant to read.
        body = (r.stdout or "").strip()
        if body:
            print("  ── the instrument's own report ─────────────────────────")
            for line in body.splitlines():
                print(f"  {line}")
        return 0
    if pol == "shrink-only":
        ok = measured <= value
        rel = "<="
    elif pol == "grow-only":
        ok = measured >= value
        rel = ">="
    else:
        band = f("band_pct")
        anchor_id = f("band_anchor")
        if band and anchor_id:
            anchor = int(norm(one(db, f"{anchor_id}.value")))
            hi = anchor + anchor * int(band) // 100
            ok = measured == value or measured <= hi
            rel = f"== (or within +{band}% of {anchor_id} = {hi})"
        else:
            ok = measured == value
            rel = "=="
    verdict = "PASS" if ok else "FAIL"
    relation = f"{rel} {value}" if ok else f"is NOT {rel} {value}"
    print(f"  ⇒ {verdict}: measured {measured} {relation}")
    if not ok and not quiet:
        print(f"     caveat: {f('caveat')}")
    return 0 if ok else 1


def checkall(db, only=None):
    """Every row (or every row whose id starts with `only`), sharing regen runs."""
    rc = 0
    for rid in rows(db):
        if only and not rid.startswith(only):
            continue
        rc |= check(db, rid)
        print()
    print(f"{len(_REGEN_CACHE)} distinct regen run(s) for the rows checked")
    return rc


def report(db):
    """The round-close informational table (`t0860`'s form: now | prev | delta)."""
    rc = 0
    print("=== informational figures (never red — a report, not a gate) ===")
    for rid in rows(db):
        if one(db, f"{rid}.polarity") != "informational":
            continue
        rc |= check(db, rid)
    return rc


def main():
    # ⚠ The commands are spelled BOTH ways (`--scan` and `scan`) because the
    # tree's other census tools are `--`-flagged and a reader will type what
    # they already know. argparse cannot take a `--`-prefixed positional, so
    # the leading run is stripped before it sees argv.
    argv = list(sys.argv[1:])
    for i, a in enumerate(argv[:2]):
        if a.startswith("--") and a[2:] in ("validate", "scan", "census", "list", "where", "spellings"):
            argv[i] = a[2:]
    ap = argparse.ArgumentParser(description="the generic figures DB")
    ap.add_argument("command", nargs="?", default="validate")
    ap.add_argument("row", nargs="?")
    ap.add_argument("--timeout", type=int)
    ap.add_argument("--verbose", action="store_true")
    args = ap.parse_args(argv)
    try:
        order, db = parse()
    except DialectError as e:
        print(f"❌ {e}", file=sys.stderr)
        return 1

    cmd = args.command
    if cmd in ("--validate", "validate"):
        errs = validate(db, order)
        for e in errs:
            print(f"❌ {e}", file=sys.stderr)
        print(f"{len(rows(db))} row(s), {len(errs)} schema error(s)")
        return 1 if errs else 0
    if cmd in ("--scan", "scan"):
        problems = scan(db, verbose=args.verbose)
        for value, path, a, e, rids in problems:
            print(f"❌ {value}: {path} has {a} unmasked spelling(s), {e} declared "
                  f"[{', '.join(rids)}]", file=sys.stderr)
        print(f"{len(problems)} undeclared-spelling discrepancy(ies)")
        return 1 if problems else 0
    if cmd in ("--census", "census"):
        census(db)
        return 0
    if cmd in ("--spellings", "spellings"):
        return spellings(db)
    if cmd in ("--list", "list"):
        for rid in rows(db):
            print(f"{rid:44s} {one(db, rid + '.polarity'):13s} "
                  f"{one(db, rid + '.provenance'):9s} {one(db, rid + '.value')}")
        return 0
    if cmd == "check":
        if not args.row:
            print("usage: figures.py check <row-id>", file=sys.stderr)
            return 2
        return check(db, args.row, timeout=args.timeout)
    if cmd == "checkall":
        return checkall(db, only=args.row)
    if cmd == "report":
        return report(db)
    if cmd in ("--where", "where"):
        if not args.row:
            print("usage: figures.py --where <value>", file=sys.stderr)
            return 2
        want = norm(args.row)
        for rel in scan_files(db, "standard"):
            with open(os.path.join(ROOT, rel), "r", encoding="utf-8", errors="replace") as fh:
                for n, line in occurrence_lines(fh.read(), want):
                    print(f"{rel}:{n}: {line[:140]}")
        return 0
    print(f"unknown command {cmd!r}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main())
