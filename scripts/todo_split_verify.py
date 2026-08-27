#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Prove the 2026-08-27 `todo/` split preserved every byte of item prose.

WHY THIS IS COMMITTED AND NOT A /tmp SCRIPT
-------------------------------------------
"The prose survived byte-identically" is an invariant-asserting claim about a
migration that deleted 960 KB of narrative from `TODO.md`.  Core #14 says such a
claim either has an enforcing guard or it is rot.  This is the guard: it
reconstructs the PRE-migration `TODO.md` from the POST-migration `TODO.md` plus
the `todo/` bodies and compares bytes.  Equality proves both halves at once —
every migrated byte is in some `todo/` file unreflowed, AND every un-migrated
byte is still in `TODO.md` unreflowed.  A dropped item, a reflowed line, a lost
heading or a swallowed blank line all surface as a diff.

The reconstruction is exact because the migration replaced each bullet block
with exactly one generated pointer line and touched nothing else.

RE-RUN IT (needs nothing but git):

    python3 scripts/todo_split_verify.py

which defaults to the two pinned commits below, or point it anywhere:

    python3 scripts/todo_split_verify.py <before-rev> <after-rev>

⚠ Only the SPLIT COMMIT can reconstruct the base byte-for-byte.  A later commit
that edits handover prose or files new items will (correctly) not match; that is
not a regression, it is the record moving on.  The claim this script pins is
about the migration, so it pins the migration's own two commits.
"""
import hashlib
import os
import re
import subprocess
import sys
import tempfile

# The R44→R45 interstitial split.
BEFORE = 'fe7f0f09'   # last commit with items as TODO.md bullets
AFTER = 'f5927d99'    # the split: 674 bullets -> 674 todo/<id>.md files

POINTER = re.compile(r'^- \[`(t\d{4})`\]\(todo/(t\d{4})\.md\)')
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def git(*args):
    return subprocess.run(['git', '-C', ROOT] + list(args),
                          check=True, stdout=subprocess.PIPE).stdout


def main(argv):
    before = argv[0] if len(argv) > 0 else BEFORE
    after = argv[1] if len(argv) > 1 else AFTER

    orig = git('show', '%s:TODO.md' % before)
    new = git('show', '%s:TODO.md' % after)
    print('pre-split  %-9s TODO.md : %8d bytes  sha256 %s'
          % (before, len(orig), hashlib.sha256(orig).hexdigest()[:16]))
    print('post-split %-9s TODO.md : %8d bytes  sha256 %s'
          % (after, len(new), hashlib.sha256(new).hexdigest()[:16]))

    tmp = tempfile.mkdtemp(prefix='todo_split_verify.')
    tar = os.path.join(tmp, 'todo.tar')
    with open(tar, 'wb') as f:
        f.write(git('archive', after, 'todo'))
    subprocess.run(['tar', '-xf', tar, '-C', tmp], check=True)
    items = os.path.join(tmp, 'todo')
    names = sorted(f for f in os.listdir(items) if f.endswith('.md'))
    print('todo/ files                    : %8d' % len(names))

    bodies = {}
    for name in names:
        raw = open(os.path.join(items, name), encoding='utf-8').read()
        parts = raw.split('\n+++\n', 1)
        if len(parts) != 2:
            print('FAIL: %s has no `+++` fence' % name)
            return 1
        bodies[name[:-3]] = parts[1]

    out, used = [], []
    for line in new.decode('utf-8').split('\n'):
        m = POINTER.match(line)
        if not m:
            out.append(line + '\n')
            continue
        if m.group(1) != m.group(2):
            print('FAIL: pointer id/href mismatch: %s' % line)
            return 1
        ident = m.group(1)
        if ident not in bodies:
            print('FAIL: pointer to a missing file: %s' % ident)
            return 1
        if ident in used:
            print('FAIL: pointer %s appears twice' % ident)
            return 1
        used.append(ident)
        out.append(bodies[ident])           # the body already ends with \n
    recon = ''.join(out)
    # split('\n') on a file ending in \n yields a trailing '' the loop turned
    # into one extra '\n'.
    if recon.endswith('\n\n') and new.endswith(b'\n'):
        recon = recon[:-1]
    recon = recon.encode('utf-8')

    print('pointers resolved              : %8d' % len(used))
    orphan = [n[:-3] for n in names if n[:-3] not in used]
    print('todo/ files never pointed at   : %s' % (', '.join(orphan) or 'none'))
    print('reconstruction                 : %8d bytes  sha256 %s'
          % (len(recon), hashlib.sha256(recon).hexdigest()[:16]))
    print('')
    if recon == orig and not orphan:
        print('RESULT: BYTE-IDENTICAL — reconstruction == %s:TODO.md' % before)
        return 0
    print('RESULT: *** NOT BYTE-IDENTICAL ***')
    a = os.path.join(tmp, 'orig.md')
    b = os.path.join(tmp, 'recon.md')
    open(a, 'wb').write(orig)
    open(b, 'wb').write(recon)
    subprocess.run(['diff', '-u', a, b])
    return 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
