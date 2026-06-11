import re, sys
path = sys.argv[1]
lines = open(path, errors='replace').read().splitlines()
params = {}
cur = None; expect_head = False
for ln in lines:
    m = re.match(r'^__bb(\d+):', ln)
    if m:
        cur = int(m.group(1)); params[cur] = []; expect_head = True; continue
    if expect_head:
        m2 = re.match(r'^    __v(\d+) = __bp(\d+);$', ln)
        if m2:
            params[cur].append(int(m2.group(2))); continue
        expect_head = False
pending = []
bad = []; total_edges = 0
for i, ln in enumerate(lines):
    bps = re.findall(r'__bp(\d+) = ', ln)
    if bps:
        pending.extend(int(b) for b in bps)
        if 'goto' not in ln:
            continue
    m = re.search(r'goto __bb(\d+);', ln)
    if m:
        tgt = int(m.group(1)); total_edges += 1
        want = params.get(tgt, [])
        if sorted(pending) != sorted(want):
            bad.append((i+1, tgt, sorted(pending), sorted(want)))
        pending = []
        continue
    s = ln.strip()
    if s and s not in ('} else {','}','{') :
        pending = []
for b in bad[:25]:
    print(f"line {b[0]}: edge -> __bb{b[1]}: assigned {b[2]} != params {b[3]}")
print(f"total edges {total_edges}, mismatched {len(bad)}, blocks {len(params)}, with-params {sum(1 for v in params.values() if v)}")
