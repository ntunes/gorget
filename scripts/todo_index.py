#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""TODO index — generate and check the pointer list in TODO.md.

WHERE WORK ITEMS LIVE (owner decision 2026-08-23)
-------------------------------------------------
One FILE PER ITEM under `todo/`.  The filename stem is the item's stable id.
Each file is TOML front matter, then a line containing exactly `+++`, then the
item's markdown body VERBATIM — the same bytes the bullet had in TODO.md,
leading `- ` marker and continuation indentation included.  Nothing reflows an
item body, ever: the bodies carry measurements and refuted remedies, and that
narrative is the point.

    id = "t0123"          stable, == the filename stem, never reused
    mechanism = ""        controlled vocabulary; empty until the census lands
    areas = ["cow"]       the `## ` section the item is filed under
    lane = ""             "rust" | "self-host" | "ggdef", only when the item says so
    severity = "HIGH"     CRITICAL | HIGH | MED | LOW, from the item's own tag
    cites = [...]         repo-relative paths named in the body
    repro = [...]         the durable known_gaps repro(s) this item cites
    filed = "2026-08-23"  from the item's own "filed"/"found" date
    priority = "High"     the `### ` heading the item is filed under
    +++
    - **…the item, byte for byte…**

An EMPTY field is honest; a guessed one is a premise that will mislead someone.
Do not infer a field the item's text does not state.

CLOSURE IS REMOVAL: `git rm todo/<id>.md` + the DONE.md entry.  Never a
`status = "closed"` field — that grows the directory forever and puts the
convergence arbiter back to interpreting field values.

WHY THE INDEX IS GENERATED
--------------------------
TODO.md keeps the handover block, the operating invariants, the whole heading
skeleton and every non-item paragraph, with ONE generated pointer line where
each item's bullet used to stand.  A hand-kept index of 674 rows is the
"parallel lists kept in sync by hand" smell AGENTS.md names, so this script
regenerates it and `--check` fails when it is stale.

    python3 scripts/todo_index.py            # check (what the lint runs)
    python3 scripts/todo_index.py --write    # regenerate TODO.md's index

`--write` reports the tree it LEAVES BEHIND, never the one it replaced: every
condition it repairs is a counted repair on the success line, not a problem.
The repairable set is CLOSED and it is exactly five: a stale pointer line, an
unindexed item, a pointer whose item file is GONE, a DUPLICATE pointer to such
an id, and a pointer whose id and href disagree.  A `--write` that exits
non-zero means it could NOT make the index current — an unloadable item file, a
duplicate pointer to a LIVE item, an item with no matching heading region.
Only the bare invocation is a verdict on the tree as found.

⚠ THE SUPPRESSION IS SCOPED TO WHAT `--write` GENUINELY REPAIRS, never to
`write` alone.  A pointer whose file is missing from `items` is dropped either
way, but that is a repair only when the file is really gone; when it is on disk
and merely failed to load, the drop DESTROYS a live item's pointer and must
stay reported (`tests/lints.rs::todo_index_write_reports_the_tree_it_leaves`
pins all five cells and three negative controls).
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TODO = os.path.join(ROOT, 'TODO.md')
ITEMS = os.path.join(ROOT, 'todo')

# `## ` heading -> area slug.  TOTAL over the categorized sections: a bullet
# section with no mapping is an error, not an empty area.
AREA = {
    'CoW / ownership / materialization': 'cow',
    'Self-host parity': 'self-host',
    'ggdef / define-gorget': 'ggdef',
    'Semantics / reference-grade rejection': 'semantics',
    'Backend / codegen': 'backend',
    'Perf / clone-pressure / compile-time': 'perf',
    'Guards / lints / test-infra': 'guards',
    'Concurrency': 'concurrency',
    'Tooling / CLI / formatter / LSP': 'tooling',
    'Docs / devbook + misc language features': 'docs',
}
PRIORITIES = ('High', 'Medium', 'Low')
ID_RE = re.compile(r'^t\d{4}$')
POINTER_RE = re.compile(r'^- \[`(t\d{4})`\]\(todo/(t\d{4})\.md\)')
SCALARS = ('id', 'mechanism', 'lane', 'severity', 'filed', 'priority')
ARRAYS = ('areas', 'cites', 'repro')


# ── item files ──────────────────────────────────────────────────────────────
def parse_toml_line(line):
    """Only the shapes this format uses: `k = "v"` and `k = ["a", "b"]`."""
    k, _, v = line.partition(' = ')
    k = k.strip()
    v = v.strip()
    if v.startswith('['):
        inner = v[1:-1].strip()
        if not inner:
            return k, []
        return k, [s.strip()[1:-1].replace('\\"', '"').replace('\\\\', '\\')
                   for s in re.findall(r'"(?:[^"\\]|\\.)*"', inner)]
    if v.startswith('"') and v.endswith('"'):
        return k, v[1:-1].replace('\\"', '"').replace('\\\\', '\\')
    raise ValueError('unparseable front-matter line: %r' % line)


def load_items(errors):
    items = {}
    if not os.path.isdir(ITEMS):
        errors.append('todo/ is missing — that is where work items live')
        return items
    for name in sorted(os.listdir(ITEMS)):
        path = os.path.join(ITEMS, name)
        if not name.endswith('.md') or not os.path.isfile(path):
            errors.append('todo/%s: only `<id>.md` item files belong here' % name)
            continue
        stem = name[:-3]
        if not ID_RE.match(stem):
            errors.append('todo/%s: id must match t<4 digits>' % name)
            continue
        raw = open(path, encoding='utf-8').read()
        parts = raw.split('\n+++\n', 1)
        if len(parts) != 2:
            errors.append('todo/%s: no `+++` fence separating front matter from body' % name)
            continue
        fm, body = parts
        fields = {}
        for line in fm.split('\n'):
            if not line.strip():
                continue
            try:
                k, v = parse_toml_line(line)
            except ValueError as e:
                errors.append('todo/%s: %s' % (name, e))
                continue
            fields[k] = v
        for k in SCALARS:
            if not isinstance(fields.get(k), str):
                errors.append('todo/%s: missing string field `%s`' % (name, k))
        for k in ARRAYS:
            if not isinstance(fields.get(k), list):
                errors.append('todo/%s: missing array field `%s`' % (name, k))
        if fields.get('id') != stem:
            errors.append('todo/%s: id=%r does not match the filename'
                          % (name, fields.get('id')))
        if fields.get('severity') not in ('', 'CRITICAL', 'HIGH', 'MED', 'LOW'):
            errors.append('todo/%s: severity=%r is not one of "" / CRITICAL / HIGH / MED / LOW'
                          % (name, fields.get('severity')))
        if fields.get('priority') not in ('',) + PRIORITIES:
            errors.append('todo/%s: priority=%r is not one of "" / %s'
                          % (name, fields.get('priority'), ' / '.join(PRIORITIES)))
        if not body.strip():
            errors.append('todo/%s: empty body' % name)
        fields['_body'] = body
        items[stem] = fields
    return items


# ── the pointer line ────────────────────────────────────────────────────────
def title_of(body):
    t = body.split('\n')[0]
    t = t[2:] if t.startswith('- ') else t
    t = t.replace('**', '').replace('`', '')
    t = re.sub(r'\s+', ' ', t).strip()
    return t[:120].rstrip() + '…' if len(t) > 120 else t


def pointer_for(ident, fields):
    sev = fields.get('severity') or ''
    return '- [`%s`](todo/%s.md)%s — %s' % (
        ident, ident, ' **%s**' % sev if sev else '', title_of(fields['_body']))


# ── TODO.md walk ────────────────────────────────────────────────────────────
def walk(lines):
    """Yield (index, area, priority, pointer_id_or_None) for every line."""
    area = priority = None
    for i, line in enumerate(lines):
        if line.startswith('## '):
            area = AREA.get(line[3:].strip())
            priority = None
        elif line.startswith('### '):
            h = line[4:].strip()
            priority = h if h in PRIORITIES else None
        m = POINTER_RE.match(line)
        yield i, area, priority, (m.group(1) if m else None)


def main(argv):
    write = '--write' in argv
    errors = []
    items = load_items(errors)
    lines = open(TODO, encoding='utf-8').read().split('\n')

    seen = []
    dropped = 0
    for i, area, priority, ident in walk(lines):
        if ident is None:
            continue
        m = POINTER_RE.match(lines[i])
        if m.group(1) != m.group(2) and not write:
            errors.append('TODO.md:%d: pointer id and href disagree' % (i + 1))
        if ident in seen and not (write and ident not in items):
            errors.append('TODO.md:%d: `%s` is pointed at twice' % (i + 1, ident))
        seen.append(ident)
        if ident not in items:
            # SUPPRESS ONLY WHAT `--write` ACTUALLY REPAIRS. The drop filter
            # below removes this row unconditionally, but that is a REPAIR only
            # when the item file is genuinely GONE. When the file is STILL ON
            # DISK and merely failed to load (no `+++` fence, bad id line), the
            # drop DESTROYS a live item's pointer -- so the row must stay
            # reported, or `--write` silently unindexes an item that exists.
            if write and not os.path.exists(os.path.join(ITEMS, ident + '.md')):
                dropped += 1
            else:
                errors.append('TODO.md:%d: pointer to a missing todo/%s.md' % (i + 1, ident))
            continue
        f = items[ident]
        if area is None:
            errors.append('TODO.md:%d: `%s` sits outside a known `## ` section' % (i + 1, ident))
        elif f['areas'] != [area]:
            errors.append('TODO.md:%d: `%s` has areas=%r but is indexed under the %r section'
                          % (i + 1, ident, f['areas'], area))
        if (priority or '') != f['priority']:
            errors.append('TODO.md:%d: `%s` has priority=%r but is indexed under %r'
                          % (i + 1, ident, f['priority'], priority or ''))
        want = pointer_for(ident, f)
        if lines[i] != want:
            if write:
                lines[i] = want
            else:
                errors.append('TODO.md:%d: stale pointer text for `%s`\n'
                              '    have: %s\n    want: %s' % (i + 1, ident, lines[i], want))

    missing = [k for k in sorted(items) if k not in seen]
    inserted = 0
    if missing and not write:
        errors.append('not indexed in TODO.md: %s (run `python3 scripts/todo_index.py --write`)'
                      % ', '.join(missing))
    if write:
        # Drop pointers whose file is gone, then append the unindexed ones at
        # the end of their (area, priority) region.
        # ⚠ A DROP IS A REPAIR, NOT A PROBLEM — and it MUST be named on the
        # success line. It used to be silent there on the argument that a drop
        # "always coincides with an error so the success line never prints":
        # true, and that WAS the defect (todo/t1449). The run that removed the
        # row also reported the row as a problem and exited 1, so `--write`'s rc
        # described the tree it had just replaced. `&&`-chained callers
        # short-circuited on it. Now the drop is counted, reported, and folded
        # into the arithmetic identity below, so `--write` is authoritative
        # about the tree it leaves behind — which is the only tree its caller
        # will ever see.
        lines = [l for l in lines
                 if not (POINTER_RE.match(l) and POINTER_RE.match(l).group(1) not in items)]
        for ident in missing:
            f = items[ident]
            area = f['areas'][0] if f['areas'] else None
            at = None
            for i, a, p, pid in walk(lines):
                if a == area and (p or '') == f['priority']:
                    at = i
            if at is None:
                errors.append('todo/%s.md: no `## `/`### ` region matches areas=%r priority=%r'
                              % (ident, f['areas'], f['priority']))
                continue
            lines.insert(at + 1, pointer_for(ident, f))
            inserted += 1
        open(TODO, 'w', encoding='utf-8').write('\n'.join(lines))

    if errors:
        # NAME THE DROPS ON THE ERROR PATH TOO. `dropped` otherwise lives only
        # on the success line, which never prints at rc 1 — so on a MIXED tree
        # (one genuine drop plus one unrelated problem) the run would remove a
        # row and say nothing about it, which is this script's own class.
        moved = ' (%d pointer(s) dropped)' % dropped if dropped else ''
        sys.stderr.write('todo_index: %d problem(s)%s\n' % (len(errors), moved))
        for e in errors:
            sys.stderr.write('  %s\n' % e)
        return 1
    # ⚠ THE COUNTS DIFFER BY DESIGN, AND THE OLD MESSAGE LET THAT READ AS A
    # NEAR-MISS. `pointer(s) found` is counted BEFORE insertion, so a `--write`
    # that files k new items prints k fewer pointers than items and always did —
    # `OK — N item(s), N-k pointer(s)` is the ROUTINE signature of a normal
    # filing, not a symptom. Printed bare next to the word OK it looks exactly
    # like rows the index lost, and a reader who takes it for one goes hunting
    # for a phantom. Naming the move that explains it closes the arithmetic:
    #     item(s) == pointer(s) found − dropped + inserted
    # (`dropped` is a `--write`-only move and is 0 in check mode; `seen` counts a
    # dropped pointer because the walk records it before the membership test.)
    # On the success path that identity is exact, so it is CHECKED rather than
    # merely claimed: every id in `seen` is distinct and present in `items` (any
    # violation is already an error above), so `missing` is exactly the
    # difference and every one of them is inserted or errors out.
    if len(items) != len(seen) - dropped + inserted:
        sys.stderr.write(
            'todo_index: INTERNAL — %d item(s) but %d pointer(s) found - %d dropped '
            '+ %d inserted. These are equal by construction on the success path; a '
            'mismatch means the index walk and the item loader disagree.\n'
            % (len(items), len(seen), dropped, inserted))
        return 1
    print('todo_index: OK — %d item(s), %d pointer(s) found − %d dropped + %d inserted, '
          'index current' % (len(items), len(seen), dropped, inserted))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
