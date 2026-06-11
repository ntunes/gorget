import re, sys
from collections import Counter
src_path, out_path = sys.argv[1], sys.argv[2]
lines = open(src_path, errors='replace').read().splitlines(keepends=True)
out = []
i = 0
n = len(lines)
decl_re = re.compile(r'^    \S.*? (__[vs]\d+)( = \{?0\}?)?;$')
fn_start_re = re.compile(r'^[A-Za-z_][^;]*\(.*\{\s*$')
removed = 0
while i < n:
    line = lines[i]
    if fn_start_re.match(line) and not line.startswith(' '):
        # collect function body
        body = [line]; i += 1
        while i < n:
            body.append(lines[i])
            if lines[i].startswith('}'):
                i += 1
                break
            i += 1
        text = ''.join(body)
        cnt = Counter(re.findall(r'__[vs]\d+\b', text))
        newbody = []
        for ln in body:
            m = decl_re.match(ln)
            if m and cnt[m.group(1)] == 1:
                removed += 1
                continue
            newbody.append(ln)
        out.extend(newbody)
    else:
        out.append(line); i += 1
open(out_path,'w').write(''.join(out))
print(f"removed {removed} dead decls", file=sys.stderr)
