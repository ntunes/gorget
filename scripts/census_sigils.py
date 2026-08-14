#!/usr/bin/env python3
"""D27 sigil census — classifies every `!` occurrence in the .gg corpus.

Walks each file char-by-char tracking string / comment / f-string state so
that `!` inside a string literal or a `#` comment is classified SKIP rather
than counted as a sigil.  Categories mirror the 2026-08-06 3-scout wave:

  prefix_move     `!name`      -> D27 rewrites to `^name`   (SWEEP)
  move_closure    `!(...)`     -> D27 rewrites to `^(...)`  (SWEEP)
  corner          `!"lit"`, `*!bx`, `![`, `!^`, `!self` ... (SWEEP)
  type_arg_suffix `Vector[T !]`-> D27 Q2 extends            (SWEEP)
  postfix_err     `f()!`, `x!` -> error mark, STAYS
  neq             `!=`                                      STAYS
  in_string       inside a string literal                   SKIP
  in_comment      inside a `#` comment                      SKIP

Usage: census_sigils.py <root> [--per-file] [--list CATEGORY]
"""
import os
import sys
import collections

IDENT = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")


def classify_file(path):
    """Yield (category, lineno, col, line_text) for every `!` in the file."""
    try:
        src = open(path, encoding="utf-8", errors="replace").read()
    except OSError:
        return
    lines = src.split("\n")
    i = 0
    n = len(src)
    line = 1
    col = 0
    # string state
    in_str = None  # None | '"' | "'" | '"""' | "'''"
    in_comment = False
    while i < n:
        c = src[i]
        if c == "\n":
            line += 1
            col = 0
            in_comment = False
            i += 1
            continue
        col += 1
        if in_comment:
            if c == "!":
                yield ("in_comment", line, col, lines[line - 1] if line <= len(lines) else "")
            i += 1
            continue
        if in_str is not None:
            if c == "\\":
                i += 2
                col += 1
                continue
            if src.startswith(in_str, i):
                i += len(in_str)
                col += len(in_str) - 1
                in_str = None
                continue
            if c == "!":
                yield ("in_string", line, col, lines[line - 1] if line <= len(lines) else "")
            i += 1
            continue
        # not in string/comment
        if c == "#":
            in_comment = True
            i += 1
            continue
        if src.startswith('"""', i) or src.startswith("'''", i):
            in_str = src[i:i + 3]
            i += 3
            col += 2
            continue
        if c == '"' or c == "'":
            in_str = c
            i += 1
            continue
        if c == "!":
            nxt = src[i + 1] if i + 1 < n else ""
            # previous non-space char
            j = i - 1
            while j >= 0 and src[j] in " \t":
                j -= 1
            prev = src[j] if j >= 0 else ""
            prev_tight = src[i - 1] if i > 0 else ""
            if nxt == "=":
                cat = "neq"
                i += 2
                col += 1
                yield (cat, line, col - 1, lines[line - 1] if line <= len(lines) else "")
                continue
            elif nxt in IDENT or nxt in "(\"'":
                # could still be postfix if the previous token ends an expression
                # AND there is no space separating... e.g. `f()!` then `!x` is
                # ambiguous only across lines. Prefix wins when prev is one of
                # the "expression cannot end here" set.
                if prev_tight in IDENT or prev_tight in ")]":
                    cat = "postfix_err"
                else:
                    cat = "move_closure" if nxt == "(" else (
                        "corner" if nxt in "\"'" else "prefix_move")
            elif nxt == "]":
                cat = "type_arg_suffix"
            elif prev in IDENT or prev in ")]":
                cat = "postfix_err"
            elif nxt in "*&^":
                cat = "corner"
            else:
                cat = "corner"
            yield (cat, line, col, lines[line - 1] if line <= len(lines) else "")
        i += 1


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    root = args[0] if args else "."
    want_list = None
    per_file = "--per-file" in sys.argv
    if "--list" in sys.argv:
        want_list = sys.argv[sys.argv.index("--list") + 1]
    counts = collections.Counter()
    by_dir = collections.defaultdict(collections.Counter)
    files = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in (".git", "target", "node_modules", ".claude", ".worktrees")]
        for f in sorted(filenames):
            if f.endswith(".gg"):
                files.append(os.path.join(dirpath, f))
    for path in sorted(files):
        rel = os.path.relpath(path, root)
        top = rel.split(os.sep)[0]
        if top == "tests":
            top = os.sep.join(rel.split(os.sep)[:2])
        for cat, ln, col, text in classify_file(path):
            counts[cat] += 1
            by_dir[top][cat] += 1
            if want_list and cat == want_list:
                print(f"{rel}:{ln}:{col}: {text.strip()[:120]}")
    if want_list:
        return
    print(f"files scanned: {len(files)}")
    print("\n== category totals ==")
    sweep_cats = ("prefix_move", "move_closure", "corner", "type_arg_suffix")
    total_sweep = 0
    for cat in ("prefix_move", "move_closure", "corner", "type_arg_suffix",
                "postfix_err", "neq", "in_string", "in_comment"):
        print(f"  {cat:16s} {counts[cat]:6d}")
        if cat in sweep_cats:
            total_sweep += counts[cat]
    print(f"  {'TOTAL SWEEP':16s} {total_sweep:6d}   (prefix_move+move_closure+corner+type_arg_suffix)")
    print(f"  {'TOTAL STAY':16s} {counts['postfix_err'] + counts['neq']:6d}   (postfix_err+neq)")
    print(f"  {'TOTAL SKIP':16s} {counts['in_string'] + counts['in_comment']:6d}   (in_string+in_comment)")
    if per_file:
        print("\n== by top-level corpus ==")
        for d in sorted(by_dir):
            c = by_dir[d]
            s = sum(c[k] for k in sweep_cats)
            print(f"  {d:22s} sweep={s:5d}  stay={c['postfix_err'] + c['neq']:5d}  skip={c['in_string'] + c['in_comment']:5d}")


main()
