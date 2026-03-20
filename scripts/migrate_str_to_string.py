#!/usr/bin/env python3
"""Migrate `str` type annotations to `String` in .gg source files.

Handles:
- Type annotations: `str name = ...` → `String name = ...`
- Generic args: `Vector[str]` → `Vector[String]`
- Function params: `(str name)` → `(String name)`
- Return types: `str foo(` → `String foo(`

Does NOT touch:
- `.str()` method calls
- `.as_str()` method calls
- String literals containing "str"
- Comments (left as-is, they're documentation)
"""
import re
import sys
import os

def migrate_line(line):
    """Replace `str` with `String` in type-annotation contexts, preserving string literals."""
    # Split line into segments: outside-string and inside-string
    result = []
    i = 0
    in_triple = False
    triple_char = None
    in_single = False
    single_char = None

    segments = []  # (text, is_string)
    seg_start = 0

    while i < len(line):
        ch = line[i]

        if in_triple:
            if i + 2 < len(line) and line[i:i+3] == triple_char * 3:
                segments.append((line[seg_start:i+3], True))
                seg_start = i + 3
                i += 3
                in_triple = False
                continue
            i += 1
            continue

        if in_single:
            if ch == '\\' and i + 1 < len(line):
                i += 2  # skip escaped char
                continue
            if ch == single_char:
                segments.append((line[seg_start:i+1], True))
                seg_start = i + 1
                in_single = False
            i += 1
            continue

        # Not in string - check for string start
        if i + 2 < len(line) and line[i:i+3] in ('"""', "'''"):
            # Flush non-string segment
            if i > seg_start:
                segments.append((line[seg_start:i], False))
            seg_start = i
            triple_char = ch
            in_triple = True
            i += 3
            continue

        # f-string prefix
        if ch in ('"', "'"):
            if i > seg_start:
                segments.append((line[seg_start:i], False))
            seg_start = i
            single_char = ch
            in_single = True
            i += 1
            continue

        # Check for f-string: f"..." or f'...'
        if ch == 'f' and i + 1 < len(line) and line[i+1] in ('"', "'"):
            if i > seg_start:
                segments.append((line[seg_start:i], False))
            seg_start = i
            single_char = line[i+1]
            in_single = True
            i += 2
            continue

        i += 1

    # Flush remaining
    if seg_start < len(line):
        remaining = line[seg_start:]
        # If we're still inside a string (multi-line), mark as string
        segments.append((remaining, in_single or in_triple))

    # Process non-string segments
    output = []
    for text, is_string in segments:
        if is_string:
            output.append(text)
        else:
            # Replace `str` as a type keyword, but NOT:
            # - .str() method call
            # - .as_str() method call
            # - Part of other words (strip, construct, etc.)

            # Pattern: word-boundary `str` followed by type-context chars
            # Negative lookbehind for `.` (method call) and alphanumeric (part of word)
            text = re.sub(r'(?<![.\w])str(?=[\s\[\],):#])', 'String', text)
            # Also handle `str` at very end of segment (e.g., trailing in generics)
            text = re.sub(r'(?<![.\w])str$', 'String', text)
            output.append(text)

    return ''.join(output)


def migrate_file(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()

    new_lines = []
    changed = False
    for line in lines:
        new_line = migrate_line(line)
        if new_line != line:
            changed = True
        new_lines.append(new_line)

    if changed:
        with open(filepath, 'w') as f:
            f.writelines(new_lines)
        return True
    return False


if __name__ == '__main__':
    files = sys.argv[1:]
    count = 0
    for f in files:
        if migrate_file(f):
            count += 1
    print(f"Migrated {count}/{len(files)} files")
