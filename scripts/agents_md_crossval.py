#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Cross-check the AGENTS.md guard-coverage census against the REAL lints.

WHY THIS EXISTS
---------------
`tests/lints.rs::agents_md_insertion_sweep` measures how much of `AGENTS.md`
its own four guards actually see, by inserting a clause at every word boundary.
It is an IN-PROCESS instrument: it recomputes the four guard predicates itself
(from the guards' own helpers, so it cannot drift from them) rather than
shelling out per mutation, which at ~8800 points x 4 fillers is the only way it
finishes.  An instrument that recomputes a predicate can agree with itself while
both are wrong — Core #13, verify the verifier.

So this script never touches the sweep.  It builds mutations whose site class is
obvious BY CONSTRUCTION, runs the real `#[test]`s on each in an isolated probe
directory, and checks the class-level prediction:

    PROBE_IN   insert inside an inventory probe's span   -> CAUGHT
    EXEMPT_IN  insert inside a non-normative row's span  -> CAUGHT
    HEADING    insert on a `#` line                      -> INVISIBLE
    CODE       insert inside a fence                     -> measured, not predicted
    UNPINNED   by the length of the RUN that hosts it:
        A  host < 100-ins           stays under 100          -> ratchet SILENT
        B  100-ins <= host < 100    crosses the 100 floor    -> ratchet FIRES
        C  100 <= host <= CAP-ins   over 100, under the cap  -> ratchet SILENT
        D  host > CAP-ins           pushes past the cap      -> ratchet FIRES

Bands B and D fire only while the ratchet's constants are SATURATED — the lint's
asserts are `<=` ceilings, so the distance between constant and measurement IS
the coverage lost.  The prediction below is therefore derived from the MEASURED
slack, never assumed; `agents_md_unpinned_prose_ratchet` has a third assert that
keeps the constants pinned to the measurement, and this script is what caught the
one time they came apart.

Run lengths come from the guards' OWN helpers via `AGENTS_MD_DUMP_RUNS=1`, so the
insertion sites are chosen by the guard's arithmetic and not by a second model of
it.  The byte ceiling is deliberately EXCLUDED from every verdict: it fires purely
on the file's current headroom, so folding it in reports the file as fully guarded
whenever headroom is thin and says nothing about coverage.

    cargo test --test lints --no-run          # build the binary this drives
    python3 scripts/agents_md_crossval.py     # binary + root are auto-detected

Optional args: <lints-test-binary> <repo-root>.  Exits non-zero on any mismatch.
The source `AGENTS.md` is never written; the script asserts it is byte-identical
when it finishes.
"""
import ast
import glob
import hashlib
import os
import random
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def find_binary(root):
    cands = glob.glob(os.path.join(root, "target/debug/deps/lints-*"))
    cands = [c for c in cands if os.access(c, os.X_OK) and not c.endswith(".d")]
    if not cands:
        sys.exit("no lints test binary; run `cargo test --test lints --no-run` first")
    return max(cands, key=os.path.getmtime)


BIN = sys.argv[1] if len(sys.argv) > 1 else None
if len(sys.argv) > 2:
    ROOT = sys.argv[2]
BIN = BIN or find_binary(ROOT)

RAW = open(os.path.join(ROOT, "AGENTS.md"), encoding="utf-8").read()
SRC = open(os.path.join(ROOT, "tests/lints.rs"), encoding="utf-8").read()
BEFORE = hashlib.md5(RAW.encode()).hexdigest()
FILLER = " and every agent may skip the gauntlet at will."
INS = len(FILLER)          # chars this adds to the hosting run
MARKER = "skip the gauntlet at will"

CAP = int(re.search(r"AGENTS_MD_MAX_UNPINNED_RUN: usize = (\d+)", SRC).group(1))
MAXOVER = int(re.search(r"AGENTS_MD_MAX_RUNS_OVER_100: usize = (\d+)", SRC).group(1))

CONTENT_TESTS = [
    "agents_md_rule_inventory_is_pinned",
    "agents_md_every_clause_is_classified",
    "agents_md_unpinned_prose_ratchet",
]

PROBE = tempfile.mkdtemp(prefix="agents_md_crossval_")
os.makedirs(os.path.join(PROBE, "docs/devbook"), exist_ok=True)
shutil.copy(os.path.join(ROOT, "docs/devbook/30-excellence-system.md"),
            os.path.join(PROBE, "docs/devbook/30-excellence-system.md"))


def table(name, arity):
    """The (id, probe, ...) rows of one inventory table, from the source."""
    i = SRC.index("const " + name)
    body = SRC[i:SRC.index("\n];", i)]
    rows = []
    for m in re.finditer(r"^\s*\((.*)\),\s*$", body, re.M):
        try:
            t = ast.literal_eval("(" + m.group(1) + ")")
        except Exception:
            continue
        if len(t) == arity and isinstance(t[1], str):
            rows.append(t[1])
    return rows


def run(text, tests=CONTENT_TESTS):
    """True if any of `tests` FAILS on `text`, plus the names that failed."""
    open(os.path.join(PROBE, "AGENTS.md"), "w", encoding="utf-8").write(text)
    p = subprocess.run([BIN] + tests, cwd=PROBE, capture_output=True, text=True)
    out = p.stdout + p.stderr
    failed = sorted(
        l.split()[1] for l in out.splitlines()
        if l.strip().startswith("test agents_md_") and l.strip().endswith("FAILED")
    )
    assert f"{len(tests)} passed" in out or failed, out[-500:]
    return (p.returncode != 0), failed


def dump_runs():
    """Every uncovered run as (chars, line, text), from the guards' own helpers."""
    open(os.path.join(PROBE, "AGENTS.md"), "w", encoding="utf-8").write(RAW)
    env = dict(os.environ, AGENTS_MD_DUMP="1", AGENTS_MD_DUMP_RUNS="1")
    p = subprocess.run([BIN, "agents_md_measurements", "--nocapture"],
                       cwd=PROBE, capture_output=True, text=True, env=env)
    rows = []
    for line in p.stdout.splitlines():
        if line.startswith("RUN\t"):
            _, ln, lineno, text = line.split("\t", 3)
            rows.append((int(ln), int(lineno), text))
    assert rows, p.stdout[-500:]
    return rows


def locate(run_text):
    """Byte offset of a word boundary INSIDE this run, in the raw file.

    Runs are whitespace-squashed, so match with flexible whitespace; return
    None when the run is not uniquely locatable."""
    words = run_text.split(" ")
    if len(words) < 6:
        return None
    k = len(words) // 3
    frag = words[k:k + 5]
    rx = re.compile(r"\s+".join(re.escape(w) for w in frag))
    hits = list(rx.finditer(RAW))
    if len(hits) != 1:
        return None
    m = hits[0]
    j = RAW.find(" ", m.start())
    return j if 0 < j < m.end() else None


def band(host):
    if host < 100 - INS:
        return "A"
    if host < 100:
        return "B"
    if host <= CAP - INS:
        return "C"
    return "D"


def midpoint_insert(key):
    """Insert strictly INSIDE `key`'s single occurrence."""
    if RAW.count(key) != 1:
        return None
    words = key.split(" ")
    if len(words) < 3:
        return None
    cut = key.index(" ", len(key) // 3)
    return RAW.replace(key, key[:cut] + FILLER + key[cut:], 1)


random.seed(11)
probes = [p for p in table("AGENTS_MD_RULE_INVENTORY", 2) if RAW.count(p) == 1]
exempts = [p for p in table("AGENTS_MD_NON_NORMATIVE", 3) if RAW.count(p) == 1]
heads = [l for l in RAW.split("\n") if l.startswith("#") and len(l.split()) > 2]
print(f"binary {os.path.relpath(BIN, ROOT)}")
print(f"parsed {len(probes)} unique probes, {len(exempts)} unique exemptions, "
      f"{len(heads)} multi-word headings")
print(f"constants: MAX_UNPINNED_RUN={CAP}  MAX_RUNS_OVER_100={MAXOVER}  "
      f"filler={INS} chars")

caught, _ = run(RAW)
assert not caught, "BASELINE: unmutated AGENTS.md must be GREEN on the 3 content guards"
print("BASELINE unmutated: GREEN\n")

runs = dump_runs()
longest = max(r[0] for r in runs)
over = sum(1 for r in runs if r[0] >= 100)
sat = longest >= CAP and over >= MAXOVER
print(f"ratchet slack: longest {longest}/{CAP}, runs>=100 {over}/{MAXOVER}  "
      f"({'SATURATED' if sat else 'HAS SLACK — bands B/D are expected SILENT'})\n")

fails = 0


def record(label, ok, tot):
    pct = 100.0 * ok / tot if tot else 0.0
    print(f"{label:<10}: prediction held at {ok}/{tot} ({pct:.0f}%)")


for label, keys, want in (
    ("PROBE_IN", random.sample(probes, min(12, len(probes))), True),
    ("EXEMPT_IN", random.sample(exempts, min(8, len(exempts))), True),
):
    ok = tot = 0
    for k in keys:
        m = midpoint_insert(k)
        if m is None:
            continue
        tot += 1
        got, _ = run(m)
        if got == want:
            ok += 1
        else:
            fails += 1
            print(f"  MISMATCH {label}: caught={got} want={want} key={k[:60]!r}")
    record(label, ok, tot)

ok = tot = 0
for h in random.sample(heads, min(8, len(heads))):
    tot += 1
    got, _ = run(RAW.replace(h, h + FILLER, 1))
    if not got:
        ok += 1
    else:
        fails += 1
        print(f"  MISMATCH HEADING: caught=True want=False line={h[:60]!r}")
record("HEADING", ok, tot)

# CODE: inside a fenced block. Measured, not predicted — AGENTS.md's claim
# there ("in a fence usually nothing") is a rate, not a rule.
inside, code_lines = False, []
for l in RAW.split("\n"):
    if l.strip().startswith("```"):
        inside = not inside
        continue
    if inside and len(l.split()) > 4:
        code_lines.append(l)
code_caught = code_tot = 0
for l in random.sample(code_lines, min(8, len(code_lines))):
    if RAW.count(l) != 1:
        continue
    j = l.index(" ", len(l) // 3)
    code_tot += 1
    got, _ = run(RAW.replace(l, l[:j] + FILLER + l[j:], 1))
    code_caught += bool(got)
print(f"CODE      : caught {code_caught}/{code_tot} "
      f"({100.0 * code_caught / max(code_tot, 1):.0f}%) [measured, not predicted]")

# UNPINNED, by run-length band. Prediction: the RATCHET fires iff B or D, AND
# the corresponding constant is saturated.
by_band = {}
for length, lineno, text in runs:
    if MARKER in text:
        continue
    off = locate(text)
    if off is None:
        continue
    by_band.setdefault(band(length), []).append((length, off, text))

print()
for b in "ABCD":
    pool = by_band.get(b, [])
    want = (b == "B" and over >= MAXOVER) or (b == "D" and longest >= CAP)
    ok = tot = 0
    for length, off, text in random.sample(pool, min(10, len(pool))):
        mutated = RAW[:off] + FILLER + RAW[off:]
        assert MARKER in mutated
        _, which = run(mutated, ["agents_md_unpinned_prose_ratchet"])
        got = bool(which)
        tot += 1
        if got == want:
            ok += 1
        else:
            fails += 1
            print(f"  MISMATCH UNPINNED-{b}: host={length} ratchet={got} "
                  f"want={want} run={text[:60]!r}")
    record(f"UNPIN-{b}", ok, tot)
    print(f"             band {b}: {len(pool)} locatable runs, {tot} sampled, "
          f"ratchet expected {'FIRE' if want else 'SILENT'}")

after = hashlib.md5(open(os.path.join(ROOT, "AGENTS.md"), "rb").read()).hexdigest()
assert after == BEFORE, "the source AGENTS.md was modified — that must never happen"
shutil.rmtree(PROBE, ignore_errors=True)
print(f"\nsource AGENTS.md untouched: True\nMISMATCHES: {fails}")
sys.exit(1 if fails else 0)
