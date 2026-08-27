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
    for i, area, priority, ident in walk(lines):
        if ident is None:
            continue
        m = POINTER_RE.match(lines[i])
        if m.group(1) != m.group(2):
            errors.append('TODO.md:%d: pointer id and href disagree' % (i + 1))
        if ident in seen:
            errors.append('TODO.md:%d: `%s` is pointed at twice' % (i + 1, ident))
        seen.append(ident)
        if ident not in items:
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
    if missing and not write:
        errors.append('not indexed in TODO.md: %s (run `python3 scripts/todo_index.py --write`)'
                      % ', '.join(missing))
    if write:
        # Drop pointers whose file is gone, then append the unindexed ones at
        # the end of their (area, priority) region.
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
        open(TODO, 'w', encoding='utf-8').write('\n'.join(lines))

    if errors:
        sys.stderr.write('todo_index: %d problem(s)\n' % len(errors))
        for e in errors:
            sys.stderr.write('  %s\n' % e)
        return 1
    print('todo_index: OK — %d item(s), %d pointer(s), index current'
          % (len(items), len(seen)))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
