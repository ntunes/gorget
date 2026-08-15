#!/bin/bash
# A2 PRE-FLIGHT GATE: does `gg fmt` change a comment's SUBJECT?
#
# Run this BEFORE any bulk `gg fmt` sweep of the tree. A sweep bakes whatever
# the formatter does into the corpus, and a comment that changes block is not a
# reformat — it is an edit to what the source SAYS. `SUBJECT_CHANGED` and
# `CONTINUATION_DRIFT` must both be zero, or every surviving row must be
# classified in the sweep's hunk-classification summary.
#
# Absolute columns move for benign reasons (a body re-indented from 6 spaces to
# 4 carries its comments with it), so this measures each own-line comment
# RELATIVE TO THE CODE IT LEADS: `hash_col - indent(next code line)`, in the
# source and in the formatted output. A change in that delta means the comment
# changed BLOCK — the detach/dedent class — and it is the only instrument that
# tells "the formatter re-indented a block" apart from "the formatter
# re-parented a comment".
#
# Also reports CONTINUATION drift: an own-line comment directly under a trailing
# comment at the same `#` column in the SOURCE that no longer sits under its
# head's `#` in the OUTPUT — the multi-line-trailing-comment run.
#
# ⚠ It measures COLUMNS. A deleted blank line is invisible to it; the corpus
# DIFF (with a per-file line-count comparison) is the detector for that.
#
# usage: scripts/fmt_comment_move_census.sh <gg-binary> [roots...]
#   e.g. scripts/fmt_comment_move_census.sh target/debug/gg lib tests/fixtures \
#          compiler examples spec
GG="$1"; shift
python3 - "$GG" "$@" <<'PY'
import os, subprocess, sys, re

gg = sys.argv[1]
roots = sys.argv[2:] or ["."]

def scan(text):
    """-> (rel, cont) keyed by comment TEXT.
    rel[t]  = hash_col - indent(next code line)  (own-line comments only)
    cont[t] = hash_col - head_hash_col           (continuation lines only)
    """
    lines = text.split("\n")
    info = []
    for ln in lines:
        s = re.sub(r'"(?:[^"\\]|\\.)*"', lambda m: '"'+"_"*(len(m.group(0))-2)+'"', ln)
        s = re.sub(r"'(?:[^'\\]|\\.)*'", lambda m: "'"+"_"*(len(m.group(0))-2)+"'", s)
        h = s.find("#")
        if h < 0:
            info.append(None)
        else:
            info.append((h, ln[:h].strip() == "", ln[h:].strip()))
    rel, cont = {}, {}
    for i, cur in enumerate(info):
        if cur is None:
            continue
        h, own, txt = cur
        if own:
            j = i + 1
            while j < len(lines) and (lines[j].strip() == "" or (info[j] and info[j][1])):
                j += 1
            if j < len(lines):
                rel[txt] = h - (len(lines[j]) - len(lines[j].lstrip(" ")))
            # continuation? previous line is a comment at the same `#` column
            p = info[i-1] if i > 0 else None
            if p and p[0] == h:
                # walk up to the run head
                k = i - 1
                while k > 0 and info[k] and info[k][1] and info[k][0] == h:
                    k -= 1
                if info[k] and not info[k][1] and info[k][0] == h:
                    cont[txt] = h - info[k][0]
    return rel, cont

subj = drift = 0
ok = 0
files = 0
rows = []
for root in roots:
    for dp, dns, fns in os.walk(root):
        dns[:] = [d for d in dns if d not in (".git","target","node_modules",".claude")]
        for fn in sorted(fns):
            if not fn.endswith(".gg"):
                continue
            p = os.path.join(dp, fn)
            try:
                src = open(p, encoding="utf-8").read()
            except Exception:
                continue
            r = subprocess.run([gg, "fmt", p], capture_output=True, text=True)
            if r.returncode != 0:
                rows.append(("PARSE-FAIL", p, "", ""))
                continue
            files += 1
            ra, ca = scan(src)
            rb, cb = scan(r.stdout)
            for t, d in ra.items():
                if t in ca:
                    continue  # continuation lines are adjudicated by CONT-DRIFT
                if t in rb:
                    if rb[t] != d:
                        subj += 1
                        rows.append(("SUBJECT", p, f"rel {d}->{rb[t]}", t[:64]))
                    else:
                        ok += 1
            for t, d in ca.items():
                # a continuation that stopped being one (missing from cb) or drifted
                if cb.get(t, None) != d:
                    drift += 1
                    rows.append(("CONT-DRIFT", p, f"off {d}->{cb.get(t)}", t[:64]))
print(f"files={files} stable={ok} SUBJECT_CHANGED={subj} CONTINUATION_DRIFT={drift}")
byfile = {}
for k, p, d, t in rows:
    byfile.setdefault((k, p), []).append((d, t))
for (k, p), v in sorted(byfile.items()):
    print(f"{k:11} {p}  x{len(v)}")
    for d, t in v[:6]:
        print(f"              {d}   {t}")
PY
