#!/usr/bin/env python3
"""fmt_hunk_classify.py — mechanical hunk classifier for the A2 bulk `gg fmt` sweep.

Reads a unified diff (default: `git diff -U0` in the repo) and buckets every
hunk into a class, so the owner can review the one-commit sweep by CLASS
rather than by 25k raw lines.

Classes are tried in order; first match wins.  Every class is decided by a
NORMALISATION: two sides are in class C iff normalising both under C's rule
makes them identical.  That makes the classification mechanical (no taste),
and anything that survives every rule lands in `other` — the bucket that
actually needs a human.

  sigil_move      only `!x` -> `^x` move-sigil rewrites            (D27 INTENT)
  blank_line      only blank lines added/removed
  import_order    same import statements, reordered
  rewrap          identical token stream, different line breaking   (fill-pack)
  trailing_comma  identical modulo trailing `,` before a closer
  comment_gap     identical modulo whitespace before a trailing `#`
  whitespace      identical modulo all whitespace (indent/space)
  paren_drop      `Foo()` -> `Foo` (empty enum-variant paren drop)
  string_escape   a string literal's spelling changed
  numeric         a numeric literal's spelling changed
  comment_move    a comment line moved position
  other           NOT explained by any rule -> REVIEW THESE

Usage:
  fmt_hunk_classify.py [--repo DIR] [--samples N] [--class C] [--md OUT.md]
"""
import argparse
import collections
import re
import subprocess
import sys

# ---------------------------------------------------------------- normalisers

WS = re.compile(r"\s+")
# `!` immediately followed by an identifier start / `(` / `[`, i.e. the move
# sigil in the name slot -- NOT `!=` and NOT a postfix error mark.
SIGIL = re.compile(r"(?<![A-Za-z0-9_)\]])!(?=[A-Za-z_(\[\"'*^])")
CARET = re.compile(r"(?<![A-Za-z0-9_)\]])\^(?=[A-Za-z_(\[\"'*^])")


def strip_ws(s):
    return WS.sub("", s)


def sigil_norm(s):
    """Map both `!x` and `^x` move sigils to a neutral marker."""
    return CARET.sub("\x00", SIGIL.sub("\x00", s))


def drop_trailing_commas(s):
    return re.sub(r",(?=\s*[)\]}])", "", s)


def strip_comments(s):
    """Remove a trailing `#` comment (naive: ignores `#` inside strings)."""
    out, in_str, i = [], None, 0
    while i < len(s):
        c = s[i]
        if in_str:
            if c == "\\":
                out.append(s[i:i + 2]); i += 2; continue
            if c == in_str:
                in_str = None
        elif c in "\"'":
            in_str = c
        elif c == "#":
            break
        out.append(c)
        i += 1
    return "".join(out)


def comment_of(s):
    full, code = s, strip_comments(s)
    return full[len(code):]


STR_LIT = re.compile(r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'')
NUM_LIT = re.compile(r"\b0[xXoObB][0-9a-fA-F_]+\b|\b\d[\d_]*\b")

# ---------------------------------------------------------------- classifier

# NOTE the ORDER inside each normaliser: sigil_norm MUST run BEFORE strip_ws.
# `String !s` collapses to `String!s`, whose `!` then reads as a POSTFIX error
# mark and the lookbehind refuses to match.  (This bug under-counted
# `sigil_move` by ~600 hunks on the first run of this script.)
CLASSES = [
    ("sigil_move", lambda t: strip_ws(sigil_norm(t))),
    ("trailing_comma", lambda t: drop_trailing_commas(strip_ws(sigil_norm(t)))),
    ("whitespace", lambda t: strip_ws(t)),
    ("string_escape", lambda t: STR_LIT.sub("\x01", strip_ws(sigil_norm(t)))),
    ("numeric", lambda t: NUM_LIT.sub("\x02", strip_ws(sigil_norm(t)))),
    ("paren_drop", lambda t: re.sub(r"\(\)", "", strip_ws(sigil_norm(t)))),
]


def classify(removed, added):
    """Return a class name for one hunk given its removed/added line lists."""
    r_nonblank = [l for l in removed if l.strip()]
    a_nonblank = [l for l in added if l.strip()]

    # blank-line-only churn
    if not r_nonblank and not a_nonblank:
        return "blank_line"

    r_join = "\n".join(removed)
    a_join = "\n".join(added)

    # comment-only movement (both sides are comment lines, same multiset)
    if (r_nonblank and a_nonblank
            and all(l.strip().startswith("#") for l in r_nonblank)
            and all(l.strip().startswith("#") for l in a_nonblank)):
        if sorted(l.strip() for l in r_nonblank) == sorted(l.strip() for l in a_nonblank):
            return "comment_move"
        return "comment_edit"

    # imports reordered
    if (r_nonblank and a_nonblank
            and all(re.match(r"\s*(from|import)\b", l) for l in r_nonblank)
            and all(re.match(r"\s*(from|import)\b", l) for l in a_nonblank)):
        if sorted(l.strip() for l in r_nonblank) == sorted(l.strip() for l in a_nonblank):
            return "import_order"
        return "import_edit"

    # trailing-comment gap: code identical, only the run of spaces before `#`
    r_code = strip_ws(sigil_norm("".join(strip_comments(l) for l in removed)))
    a_code = strip_ws(sigil_norm("".join(strip_comments(l) for l in added)))
    r_cmt = [comment_of(l).strip() for l in removed if comment_of(l).strip()]
    a_cmt = [comment_of(l).strip() for l in added if comment_of(l).strip()]
    if r_code == a_code and r_cmt == a_cmt and r_code:
        # whitespace-only difference somewhere; is it the comment gap?
        if any(comment_of(l) for l in removed) or any(comment_of(l) for l in added):
            return "comment_gap"

    for name, norm in CLASSES:
        if norm(r_join) == norm(a_join):
            return name

    # rewrap: same code token stream ignoring comments AND whitespace
    if r_code == a_code and r_code:
        return "rewrap"
    # rewrap with the sigil rewrite folded in (a wrapped line that also moved)
    if sigil_norm(r_code) == sigil_norm(a_code) and r_code:
        return "rewrap+sigil"

    return "other"


# ---------------------------------------------------------------- diff reader

HUNK = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")


def iter_hunks(diff_text):
    path = None
    removed, added, start = [], [], 0
    pending = False
    for line in diff_text.split("\n"):
        if line.startswith("diff --git "):
            if pending:
                yield path, start, removed, added
            pending = False
            removed, added = [], []
            path = line.split(" b/")[-1]
        elif line.startswith("@@"):
            if pending:
                yield path, start, removed, added
            m = HUNK.match(line)
            start = int(m.group(1)) if m else 0
            removed, added = [], []
            pending = True
        elif pending and line.startswith("-") and not line.startswith("---"):
            removed.append(line[1:])
        elif pending and line.startswith("+") and not line.startswith("+++"):
            added.append(line[1:])
    if pending:
        yield path, start, removed, added


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default=".")
    ap.add_argument("--samples", type=int, default=3)
    ap.add_argument("--class", dest="only", default=None)
    ap.add_argument("--md", default=None)
    ap.add_argument("--diff-file", default=None)
    args = ap.parse_args()

    if args.diff_file:
        diff = open(args.diff_file, encoding="utf-8", errors="replace").read()
    else:
        diff = subprocess.run(
            ["git", "-C", args.repo, "--no-pager", "diff", "-U0", "--no-color", "--", "*.gg"],
            capture_output=True, text=True, errors="replace").stdout

    counts = collections.Counter()
    lines_moved = collections.Counter()
    files_by_class = collections.defaultdict(set)
    samples = collections.defaultdict(list)

    for path, start, removed, added in iter_hunks(diff):
        cls = classify(removed, added)
        counts[cls] += 1
        lines_moved[cls] += len(removed) + len(added)
        files_by_class[cls].add(path)
        if args.only and cls != args.only:
            continue
        if len(samples[cls]) < (args.samples if not args.only else 50):
            samples[cls].append((path, start, removed, added))

    total = sum(counts.values())
    out = []
    out.append(f"# A2 bulk `gg fmt` sweep — hunk classification\n")
    out.append(f"Total hunks: **{total}**   ·   files touched: "
               f"**{len(set().union(*files_by_class.values())) if files_by_class else 0}**\n")
    out.append("| class | hunks | % | files | ± lines |")
    out.append("|---|---:|---:|---:|---:|")
    for cls, n in counts.most_common():
        out.append(f"| `{cls}` | {n} | {100 * n / total:.1f}% | "
                   f"{len(files_by_class[cls])} | {lines_moved[cls]} |")
    out.append("")
    for cls, n in counts.most_common():
        out.append(f"\n## `{cls}` — {n} hunks, {len(files_by_class[cls])} files\n")
        for path, start, removed, added in samples[cls]:
            out.append(f"```diff\n--- {path}:{start}")
            for l in removed[:8]:
                out.append(f"-{l}")
            for l in added[:8]:
                out.append(f"+{l}")
            out.append("```")
    text = "\n".join(out)
    if args.md:
        open(args.md, "w").write(text)
        print(f"wrote {args.md}")
    print("\n".join(out[:4 + len(counts)]))


main()
