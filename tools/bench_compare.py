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
    """Parse alloc-report lines from a snapshot file.
    Returns dict with keys: allocs, frees, live_bytes,
    array_new, array_clone, array_free,
    string_new, string_cat, string_free.
    Sums across all bench runs in the file.
    """
    totals = defaultdict(int)
    with open(path) as f:
        for line in f:
            m = re.search(r"\[alloc-report\] allocs=(\d+) frees=(\d+) live_bytes=(\d+)", line)
            if m:
                totals["allocs"] += int(m.group(1))
                totals["frees"]  += int(m.group(2))
                # live_bytes: skip (should be 0 at exit)
            m = re.search(r"\[alloc-report\] array: new=(\d+) clone=(\d+) free=(\d+)", line)
            if m:
                totals["array_new"]   += int(m.group(1))
                totals["array_clone"] += int(m.group(2))
                totals["array_free"]  += int(m.group(3))
            m = re.search(r"\[alloc-report\] string: new=(\d+) cat=(\d+) free=(\d+)", line)
            if m:
                totals["string_new"] += int(m.group(1))
                totals["string_cat"] += int(m.group(2))
                totals["string_free"] += int(m.group(3))
    return totals

def fmt_ns(ns):
    if ns >= 1_000_000: return f"{ns/1_000_000:.2f}ms"
    if ns >= 1_000:     return f"{ns/1_000:.2f}us"
    return f"{ns:.0f}ns"

def fmt_iters(n):
    if n >= 1_000_000: return f"{n/1_000_000:.0f}M"
    if n >= 1_000:     return f"{n/1_000:.0f}K"
    return str(n)

def fmt_count(n):
    if n >= 1_000_000: return f"{n/1_000_000:.1f}M"
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
alloc_data = [parse_allocs(f) for f in files]
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
for n in names:
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

# ── Timing summary ──
print("─" * len(hdr))
row = f"{'Total iterations':42}"
for ti in total_iters:
    row += f" {'':>8} {ti:>14,}"
print(row)

# ── Alloc summary (only if data present) ──
has_allocs = any(ad["allocs"] > 0 for ad in alloc_data)
if has_allocs:
    print()
    alloc_hdr = f"{'alloc metric':42}"
    for l in labels:
        alloc_hdr += f" {l[:22]:>22}"
    if ncols >= 2:
        alloc_hdr += f"  {'Δ 1→{}'.format(ncols):>10}"
    print(alloc_hdr)
    print("─" * len(alloc_hdr))

    def alloc_row(label, key):
        row = f"  {label:40}"
        vals = []
        for ad in alloc_data:
            v = ad[key]
            row += f" {fmt_count(v):>22}"
            vals.append(v)
        if len(vals) >= 2 and vals[0] > 0:
            row += f"  {pct(vals[0], vals[-1]):>10}"
        print(row)

    alloc_row("total allocs",    "allocs")
    alloc_row("total frees",     "frees")
    alloc_row("array new",       "array_new")
    alloc_row("array clone",     "array_clone")
    alloc_row("string new",      "string_new")
    alloc_row("string cat",      "string_cat")
    alloc_row("string free",     "string_free")

    print()
    row = f"  {'allocs per iteration':40}"
    for ti, ad in zip(total_iters, alloc_data):
        api = ad["allocs"] / ti if ti else 0
        row += f" {api:>22.4f}"
    if len(total_iters) >= 2 and total_iters[0] > 0 and alloc_data[0]["allocs"] > 0:
        base = alloc_data[0]["allocs"] / total_iters[0]
        curr = alloc_data[-1]["allocs"] / total_iters[-1]
        row += f"  {pct(base, curr):>10}"
    print(row)
else:
    row = f"{'Total allocations':42}"
    for _ in alloc_data:
        row += f" {'':>8} {'(no data)':>14}"
    row += f" {'':>10}"
    print(row)
    print("  → Run bench with alloc-report enabled to see allocation counts.")
