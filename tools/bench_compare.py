#!/usr/bin/env python3
"""Compare two or three benchmark snapshots.

Usage:
    tools/bench_compare.py snapshot_a.txt snapshot_b.txt [snapshot_c.txt]
"""
import re
import sys
from collections import defaultdict

UNIT = {"ns": 1, "us": 1_000, "ms": 1_000_000, "s": 1_000_000_000}

def parse(path):
    """Parse a benchmark snapshot file. Returns {name: (iters, ns_per_iter)}."""
    d = {}
    label = None
    with open(path) as f:
        for line in f:
            m = re.match(r"# Gorget benchmark snapshot: (.+)", line)
            if m:
                label = m.group(1).strip()
            m = re.search(r"bench: (.+?) \.\.\. (\d+) iters, ([\d.]+) (ns|us|ms|s)/iter", line)
            if m:
                name = m.group(1)
                iters = int(m.group(2))
                val = float(m.group(3)) * UNIT[m.group(4)]
                # Keep median if multiple runs
                if name not in d:
                    d[name] = []
                d[name].append((iters, val))
    # Take median by ns/iter
    result = {}
    for k, runs in d.items():
        runs.sort(key=lambda x: x[1])
        result[k] = runs[len(runs) // 2]
    return label or path, result

def parse_allocs(path):
    allocs = 0
    with open(path) as f:
        for line in f:
            m = re.search(r"\[alloc-report\] allocs=(\d+)", line)
            if m:
                allocs += int(m.group(1))
    return allocs

def fmt_ns(ns):
    if ns >= 1_000_000: return f"{ns/1_000_000:.2f}ms"
    if ns >= 1_000:     return f"{ns/1_000:.2f}us"
    return f"{ns:.0f}ns"

def fmt_iters(n):
    if n >= 1_000_000: return f"{n/1_000_000:.0f}M"
    if n >= 1_000:     return f"{n/1_000:.0f}K"
    return str(n)

def pct(old, new):
    if old == 0: return ""
    return f"{(new - old) / old * 100:+.1f}%"

# ── Main ──

files = sys.argv[1:]
if len(files) < 2:
    print(f"Usage: {sys.argv[0]} snapshot_a.txt snapshot_b.txt [snapshot_c.txt]")
    sys.exit(1)

snapshots = [parse(f) for f in files]
allocs = [parse_allocs(f) for f in files]
labels = [s[0] for s in snapshots]
data = [s[1] for s in snapshots]
names = sorted(set().union(*[d.keys() for d in data]))

# Compute total iterations
total_iters = []
for d in data:
    total_iters.append(sum(v[0] for v in d.values()))

# ── Header ──
ncols = len(files)
hdr = f"{'bench':42}"
for l in labels:
    hdr += f" {'iters':>8} {l[:14]:>14}"
if ncols >= 2:
    hdr += f" {'Δ 1→{}'.format(ncols):>10}"
print(hdr)
print("─" * len(hdr))

# ── Rows ──
prev_category = None
for n in names:
    # Category separator based on name patterns
    cat = "String" if "string" in n.lower() or "trim" in n.lower() or "split" in n.lower() \
        or "slice" in n.lower() or "char_at" in n.lower() or "fstring" in n.lower() \
        or "replace" in n.lower() or "upper" in n.lower() or "contains" in n.lower() \
        or "enumerate" in n.lower() or "codepoint" in n.lower() or "concat" in n.lower() \
        or "literal" in n.lower() and "push" not in n.lower() \
        else "Collection" if "vector" in n.lower() or "dict" in n.lower() or "set" in n.lower() \
        else "Compute" if "fib" in n.lower() or "sum" in n.lower() or "range" in n.lower() \
        or "vec2" in n.lower() or "match" in n.lower() or "option" in n.lower() \
        or "closure" in n.lower() \
        else "Other"

    row = f"{n:42}"
    vals = []
    for d in data:
        if n in d:
            iters, ns = d[n]
            row += f" {fmt_iters(iters):>8} {fmt_ns(ns):>14}"
            vals.append(ns)
        else:
            row += f" {'':>8} {'—':>14}"
            vals.append(None)
    # Delta last vs first
    if len(vals) >= 2 and vals[0] is not None and vals[-1] is not None:
        row += f" {pct(vals[0], vals[-1]):>10}"
    print(row)

# ── Summary ──
print("─" * len(hdr))
row = f"{'Total iterations':42}"
for ti in total_iters:
    row += f" {'':>8} {ti:>14,}"
print(row)

row = f"{'Total allocations':42}"
for a in allocs:
    row += f" {'':>8} {a:>14,}"
if len(allocs) >= 2 and allocs[0] > 0:
    row += f" {pct(allocs[0], allocs[-1]):>10}"
print(row)

row = f"{'Allocs per iteration':42}"
for ti, a in zip(total_iters, allocs):
    row += f" {'':>8} {a/ti:>14.4f}"
if len(allocs) >= 2 and total_iters[0] > 0:
    base_api = allocs[0] / total_iters[0]
    curr_api = allocs[-1] / total_iters[-1]
    row += f" {pct(base_api, curr_api):>10}"
print(row)
