#!/usr/bin/env python3
"""Census of DUPLICATING STORES in the C runtime, and the guard behind it.

── WHY THIS EXISTS ────────────────────────────────────────────────────────────
`gorget_array_fill` copied ONE source value into N array slots with no per-slot
clone. Every slot aliased a single heap payload and the array's free released it
N times — memory-unsafe from ordinary safe syntax, on both backends, with
`gg check` clean (todo/t1407). The reason nobody noticed for so long is that
`docs/devbook/11-copy-on-write.md` asserted a TOTAL contract over "every
consuming runtime function" and nothing enforced the sentence, so the one writer
that did not obey it drifted for free. Prose rots; guards don't.

The lesson is not "check `fill`". It is that NOTHING forces the NEXT
element-duplicating runtime function through a reviewed path, and the previous
attempt to enumerate the class — the runtime-symbol registry in
`src/lir/runtime.rs` — could not even produce every candidate: it lists declared
symbols, and `gorget_shared_array_set`'s per-type wrapper is emitted as a string
from `src/backend/c_lir/helpers.rs`. An enumerator that cannot produce a row
cannot adjudicate it. THIS census enumerates the C SOURCE ITSELF, so its
population is the thing being judged rather than a description of it.

── THE PREDICATE ──────────────────────────────────────────────────────────────
A row is a STORE STATEMENT, inside a brace-tracked loop, whose

  * SOURCE is loop-INVARIANT  (it mentions no induction variable), and
  * DESTINATION is loop-VARYING (it does),

i.e. exactly "one value written into many slots". Three deliberate choices:

  1. NOT keyed on the token `memcpy`. `memmove` and the runtime's own store
     helpers (`gorget_*_push` / `_push_cloned` / `_set` / `_put` / `_insert` /
     `_add` / `_fill`, `gorget_array_clone_elem_inplace`) count too, because a
     sibling written through a helper is the same defect spelled differently.
  2. BRACE-TRACKED, not `for` plus a line window. A naive window both misses
     stores further down a long body and invents rows across a closing brace.
  3. DEF-TRACKED destinations. Hoisting the slot address into a temp
     (`void* slot = data + i * sz; memcpy(slot, src, sz);`) is the ACTUAL shape
     of the defect, and a purely syntactic destination test does not see it. A
     local assigned from a loop-varying expression is itself loop-varying.

── AND WHY IT CATCHES ITS OWN CLASS ───────────────────────────────────────────
Flagging the SITE would only ever say "someone wrote a loop store" — true of
legitimate code, so the allowlist would absorb the next defect the day it landed
(SIX-Q #2: a guard that green-lights the class it was written to retire is worse
than none). So each row is also CLASSIFIED by whether its ENCLOSING LOOP BODY
produces per-slot independence:

    CLONED   the body calls an independence hook on what it stored
             (`elem_clone`, `*_materialize`, `*_clone_to_owned`, …)
    RAW      it does not

and the allowlist pins the (function, primitive, CATEGORY) SET. Deleting
`gorget_array_fill`'s `arr->elem_clone(slot)` does not remove a row — it FLIPS
that row CLONED → RAW, and the set no longer matches. Measured, by line-anchored
break (Core #13): at pristine HEAD the census is one `gorget_array_fill memcpy
RAW`; with the fix it is `CLONED` plus the trivial-element branch's `RAW`; with
the clone call deleted it is two `RAW`. Three distinct signatures.

⚠ A RAW ROW IS NOT A BUG. Copying raw BYTES into a fresh buffer duplicates
nothing owned — `gorget_str_repeat` and the pad helpers are RAW and correct, and
so is `fill`'s own trivial-element branch. That is why every row carries a
written justification: the guard's signal is "a duplicating store appeared or
changed category — justify it", not "a duplication appeared".

⚠ LINE NUMBERS ARE DELIBERATELY NOT PART OF THE KEY. They rot on every edit
above them, and a guard whose baseline must be re-blessed for unrelated changes
gets re-blessed without reading. The key is (function, primitive, category).

Usage:
  scripts/runtime_duplication_census.py            # TSV census on stdout
  scripts/runtime_duplication_census.py --check    # gate: exit 1 on drift
"""
import os
import re
import sys

FN_DEF = re.compile(r"^[A-Za-z_][A-Za-z0-9_ \*]*?([A-Za-z_][A-Za-z0-9_]*)\s*\([^;{]*\)\s*\{\s*$")
FOR_HDR = re.compile(r"\bfor\s*\(([^;]*);")
WHILE_HDR = re.compile(r"\bwhile\s*\(")
BYTECOPY = re.compile(r"\b(memcpy|memmove)\s*\(")
HELPER = re.compile(r"\b(gorget_[a-z_]*_(?:push|push_cloned|set|put|insert|add|fill)|"
                    r"gorget_array_clone_elem_inplace)\s*\(")
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
INDEP = re.compile(r"(elem_clone|key_clone|val_clone|_materialize|_clone_to_owned|"
                   r"gorget_array_clone_elem_inplace|str_alloc_copy)\s*\(")

TYPE_WORDS = {"size_t", "int", "unsigned", "long", "char", "const", "int64_t", "uint64_t"}


def split_args(s):
    out, depth, cur = [], 0, ""
    for ch in s:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth -= 1
        if ch == "," and depth == 0:
            out.append(cur)
            cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(cur)
    return [a.strip() for a in out]


def call_args(line, match):
    i = line.index("(", match.start())
    depth, j = 0, i
    while j < len(line):
        if line[j] == "(":
            depth += 1
        elif line[j] == ")":
            depth -= 1
            if depth == 0:
                break
        j += 1
    else:
        raise ValueError("unbalanced call")
    return split_args(line[i + 1:j])


def census(root):
    rtdir = os.path.join(root, "src/backend/c/runtime")
    rows = []
    for fname in sorted(os.listdir(rtdir)):
        if not fname.endswith(".c"):
            continue
        path = os.path.join(rtdir, fname)
        rel = os.path.relpath(path, root)
        text = open(path, encoding="utf-8", errors="replace").read()
        lines = text.split("\n")
        depth, cur_fn = 0, ""
        loops = []           # [brace_depth, induction_vars, start_idx]
        pending_loop = None
        in_block_comment = False
        for idx, raw in enumerate(lines):
            line, stripped = raw, raw.strip()
            if in_block_comment:
                if "*/" in line:
                    in_block_comment = False
                    line = line.split("*/", 1)[1]
                else:
                    continue
            if "/*" in line and "*/" not in line:
                in_block_comment = True
                line = line.split("/*", 1)[0]
            if stripped.startswith("//") or stripped.startswith("*"):
                continue
            code = line.split("//", 1)[0]

            if depth == 0:
                m = FN_DEF.match(line)
                if m:
                    cur_fn = m.group(1)
                elif code[:1].isalpha() and "(" in code and not code.rstrip().endswith(";"):
                    # A column-0 signature the regex cannot parse (brace on the
                    # next line). Reset rather than mis-attribute the body that
                    # follows to the previous function.
                    cur_fn = ""

            mfor = FOR_HDR.search(code)
            if mfor:
                pending_loop = set(IDENT.findall(mfor.group(1))) - TYPE_WORDS
            elif WHILE_HDR.search(code):
                pending_loop = set()

            if loops:
                varying = set()
                for lp in loops:
                    varying |= lp[1]
                mdef = re.match(
                    r"\s*(?:[A-Za-z_][A-Za-z0-9_ \*]*\s+)?\*?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^=].*)$",
                    code)
                if mdef and set(IDENT.findall(mdef.group(2))) & varying:
                    loops[-1][1].add(mdef.group(1))
                    varying.add(mdef.group(1))
                for rx, kind in ((BYTECOPY, "bytecopy"), (HELPER, "helper")):
                    for m in rx.finditer(code):
                        try:
                            args = call_args(code, m)
                        except ValueError:
                            continue
                        if len(args) < 2:
                            continue
                        src = args[1] if kind == "bytecopy" else args[-1]
                        dst = args[0]
                        if (set(IDENT.findall(src)) & varying):
                            continue
                        if not (set(IDENT.findall(dst)) & varying):
                            continue
                        rows.append({
                            "loc": "%s:%d" % (rel, idx + 1),
                            "fn": cur_fn or "<unattributed>",
                            "prim": m.group(1) if kind == "bytecopy" else m.group(0).rstrip("( "),
                            "text": stripped[:90],
                            "path": path,
                            "loop_start": loops[-1][2],
                        })

            for _ in range(code.count("{")):
                depth += 1
                if pending_loop is not None:
                    loops.append([depth, pending_loop, idx])
                    pending_loop = None
            for _ in range(code.count("}")):
                while loops and loops[-1][0] >= depth:
                    closed = loops.pop()
                    for r in rows:
                        if (r["path"] == path and r["loop_start"] == closed[2]
                                and "loop_end" not in r):
                            r["loop_end"] = idx
                depth -= 1
                if depth <= 0:
                    depth, cur_fn, loops = 0, "", []

    for r in rows:
        body_lines = open(r["path"], encoding="utf-8", errors="replace").read().split("\n")
        lo = r["loop_start"]
        hi = r.get("loop_end", lo + 1)
        body = "\n".join(body_lines[lo:hi + 1])
        r["cat"] = "CLONED" if INDEP.search(body) else "RAW"
    return rows


def key_set(rows):
    return sorted({(r["fn"], r["prim"], r["cat"]) for r in rows})


def load_allowlist(path):
    keys, reasons = [], {}
    for line in open(path, encoding="utf-8"):
        line = line.rstrip("\n")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) < 4:
            continue
        fn, prim, cat, reason = parts[0], parts[1], parts[2], "\t".join(parts[3:])
        keys.append((fn, prim, cat))
        reasons[(fn, prim, cat)] = reason
    return sorted(set(keys)), reasons


def main():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    allow_path = os.path.join(root, "tests/runtime/DUPLICATING_STORES.txt")
    rows = census(root)
    if "--check" not in sys.argv:
        print("# loc\tfunction\tprimitive\tcategory\tstatement")
        for r in rows:
            print("%s\t%s\t%s\t%s\t%s" % (r["loc"], r["fn"], r["prim"], r["cat"], r["text"]))
        print("# rows %d" % len(rows), file=sys.stderr)
        return 0

    found = key_set(rows)
    declared, _reasons = load_allowlist(allow_path)
    if found == declared:
        print("# runtime duplicating-store census: %d rows, all declared" % len(found))
        return 0

    new = [k for k in found if k not in declared]
    gone = [k for k in declared if k not in found]
    print("DRIFT in the runtime duplicating-store census.\n")
    for k in new:
        locs = [r["loc"] for r in rows if (r["fn"], r["prim"], r["cat"]) == k]
        print("  UNDECLARED  %s\t%s\t%s\tat %s" % (k[0], k[1], k[2], ", ".join(locs)))
    for k in gone:
        print("  DECLARED BUT GONE  %s\t%s\t%s" % k)
    print("""
A row is "a loop store whose source does not vary with the loop" -- one value
written into many slots. CATEGORY says whether the loop body then makes each
slot independent (CLONED) or not (RAW).

  * A row that FLIPPED CLONED -> RAW means a per-slot clone was removed. That
    is the todo/t1407 defect class: N slots aliasing one payload, freed N times.
  * A NEW RAW row duplicating an OWNED element is the same defect in a new
    function. Copying raw bytes into a fresh buffer is not -- justify it.
  * A row that disappeared means the store or its loop is gone; drop the line.

Update tests/runtime/DUPLICATING_STORES.txt, giving every row a REASON.
Regenerate the census with:

    scripts/runtime_duplication_census.py""")
    return 1


if __name__ == "__main__":
    sys.exit(main())
