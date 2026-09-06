#!/usr/bin/env python3
"""D51 / `todo/t0718` — REGENERATE the coercion-identity changed-cell set.

WHY THIS EXISTS, AND WHY IT IS A SCRIPT RATHER THAN A NUMBER IN A DOC
--------------------------------------------------------------------
`unify`'s smart-pointer coercion used to ask
`name == "Mutex" || name == "Shared" || name == "RWLock"` of a resolved
definition; it now asks `DefInfo::is_coercion_transparent()`, the typed builtin
identity. "How many program cells does that change?" was answered by hand four
times, by four different agents, and the answer went **11 → 14 → 18 → 30**.

⚠ NOBODY MISCOUNTED. Each answer enumerated honestly *inside a factorization
somebody had chosen*, and choosing the factorization is the step that fails:
one reader factors by `name`, the next by `name × route`, the next adds the
declaration form, the next the arm. So the load-bearing artifact is this
GENERATOR, not a figure (Core #15(a): cite the thing that regenerates a claim,
never a bare number).

⛔ BUT BE PRECISE ABOUT WHAT THAT BUYS, BECAUSE IT IS EXACTLY ONE OF THE TWO
FAILURES. This script mechanises the CROSS, not the AXIS. Every shape below is
instantiated for every name, and the factorization printed at the top of a run
is DERIVED from the iterated structures — so a name or a shape added here cannot
silently skip a combination, which is the 11 → 14 → 18 failure and it is dead.
⚠ IT CANNOT PREVENT A MISSING SHAPE, and a missing shape is precisely what
produced 18 → 30: `inline × enum × armB` was a real changed cell that no hand
enumeration had a NAME for. Adding a shape is still a human judgement, and this
file is where that judgement is now recorded and reviewable — which is the whole
improvement. If you find a shape that is not here, add it; the count is whatever
the run says afterwards.

THE FACTORIZATION, STATED AS A PRODUCT
--------------------------------------
    names   = {Mutex, Shared, RWLock}          the coercion set: pre-fix these
                                               were three separate string
                                               literals, so every path owes all
                                               three
    verdict = {inline, module} x {struct, enum} x {armA, armB}     -> 8 paths
    message = {inline, module} x struct-ctor                       -> 2 paths

`armA` is `unify`'s first coercion arm (the wrapper is EXPECTED, something else
supplied); `armB` is the second (the wrapper is SUPPLIED where something else is
expected). They are identically spelled and separately guarded, which is why the
arm is an axis and not a detail. `module` means the user's type is declared in
another file and imported — the route D51 exists for, since it is an import that
changes which entity a name denotes.

Controls (`Wrap`, `Weak`, `Box`) run through every shape as well. They must be
UNCHANGED: `Wrap` was never on the name list, and `Weak`/`Box` are builtins that
the identity deliberately answers `false` for on this axis.

Also enumerated, and expected to be UNCHANGED — these are the "named omission"
shapes, run so that the claim they are not cells is regenerated rather than
remembered: `newtype N(int)` (`parse_newtype` parses no generic params, so the
value can never be `ResolvedType::Generic` and the arms' `args.len() == 1` guard
is unreachable), `type N[T] = Inner[T]` (an alias resolves through and the arm
reads the TARGET's name), and the BUILTIN spellings with no user declaration.

⛔ IT COMPARES rc AND stderr, AND THE SECOND HALF IS NOT OPTIONAL. Six of the
cells are MESSAGE cells: rc 1 on both sides, and only the blame moves. An
exit-code sweep is structurally blind to them, and that blindness is exactly how
they went unpinned through three rounds of review.

USAGE
-----
    # matched pair: what did the migration change?
    python3 scripts/coercion_identity_matrix.py --pre <gg-before> --post <gg-after>

    # single binary: just dump the matrix (rc + first diagnostic per cell)
    python3 scripts/coercion_identity_matrix.py --gg target/debug/gg

Build a `<gg-before>` by checking out the parent of the migration commit in a
scratch worktree and `cargo build`; the migration is `src/semantic/{scope,
resolve,typecheck,types}.rs` only, so reverting those four files is enough.

EXIT STATUS
-----------
    0   run completed, controls held        -- the measurement is usable
    3   run completed but a CONTROL MOVED   -- the run is INVALID, not a finding
    1   uncaught error (a missing binary, say)
    2   argparse usage error

`TOTAL CHANGED` is a MEASUREMENT and not a gate — the gate is the fixture set in
`tests/fixtures/coercion_identity/`, whose job is to hold each cell this script
enumerates. ⛔ A MOVED CONTROL IS NOT A MEASUREMENT THOUGH: `Wrap` was never on
the name list and `Weak`/`Box` are deliberately `false` on this axis, so if one
of them moves, the comparison is measuring something other than the migration
and every number in the run is void. This repo reads verdicts off the bare exit
code, so that case gets its own non-zero status rather than a line of stdout
somebody has to notice.
"""

import argparse
import json
import os
import pathlib
import re
import sys
import tempfile

# Every deadline-bearing spawn in this tree goes through `proc_guard.run`, which
# makes the child a process-group LEADER and kills the GROUP on expiry;
# `subprocess.run(timeout=)` kills the direct child only and then blocks in
# `communicate()` on pipes a surviving grandchild still holds open
# (`todo/t0842`, enforced by `process_spawn_deadline_arm_count`).
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import proc_guard  # noqa: E402  (path must be set first)

NAMES = ["Mutex", "Shared", "RWLock"]
CONTROLS = ["Wrap", "Weak", "Box"]

# A name in both lists would be probed twice into one directory AND counted as
# its own control — the subject silently grading itself. Cheap to assert, and
# impossible to notice by reading the output.
assert not (set(NAMES) & set(CONTROLS)), \
    f"NAMES and CONTROLS overlap: {sorted(set(NAMES) & set(CONTROLS))}"

ANSI = re.compile(r"\x1b\[[0-9;]*m")


# --------------------------------------------------------------------------
# Cell sources. One function per SHAPE; every shape is instantiated for every
# name, so adding a shape or a name cannot silently skip a combination.
# --------------------------------------------------------------------------

def v_struct_armA(n):
    """Wrapper EXPECTED, bare int supplied. Two fields, reading the SECOND: a
    one-field version reads offset 0, gets the int back and is ASan-clean."""
    return (f"struct {n}[T]:\n    T first\n    T second\n\n"
            f"int take_w({n}[int] w):\n    return w.second\n\n"
            f"void main():\n    print(take_w(5))\n")


def v_struct_armB(n):
    """int EXPECTED, wrapper supplied."""
    return (f"struct {n}[T]:\n    T value\n\n"
            f"int take_int(int x):\n    return x + 1\n\n"
            f"void main():\n    {n}[int] w = {n}[int](7)\n    print(take_int(w))\n")


def v_enum_armA(n):
    return (f"enum {n}[T]:\n    Held(T)\n\n"
            f"int take_w({n}[int] w):\n    match w:\n"
            f"        case {n}.Held(v): return v\n    return 0\n\n"
            f"void main():\n    print(take_w(5))\n")


def v_enum_armB(n):
    return (f"enum {n}[T]:\n    Held(T)\n\n"
            f"int take_int(int x):\n    return x + 1\n\n"
            f"void main():\n    {n}[int] w = {n}.Held(7)\n    print(take_int(w))\n")


def m_struct_ctor(n):
    """MESSAGE cell: rc 1 on both sides, only the blame moves. The ctor cannot
    infer `T` (pre-existing, name-independent); pre-fix the coercion arm
    unwrapped the ANNOTATION before reporting and named a type nobody wrote."""
    return (f"struct {n}[T]:\n    T value\n\n"
            f"void main():\n    {n}[int] m = {n}(5)\n    print(1)\n")


# --- shapes expected NOT to be cells; enumerated so the claim regenerates ---

def o_newtype_armB(n):
    return (f"newtype {n}(int)\n\n"
            f"int take_int(int x):\n    return x + 1\n\n"
            f"void main():\n    {n} w = {n}(7)\n    print(take_int(w))\n")


def o_alias_armB(n):
    return (f"struct Inner[T]:\n    T value\n\ntype {n}[T] = Inner[T]\n\n"
            f"int take_int(int x):\n    return x + 1\n\n"
            f"void main():\n    {n}[int] w = {n}[int](7)\n    print(take_int(w))\n")


def o_builtin_ctor(n):
    return f"void main():\n    {n}[int] m = {n}(5)\n    print(1)\n"


def o_builtin_armB(n):
    return (f"int take_int(int x):\n    return x + 1\n\n"
            f"void main():\n    {n}[int] m = {n}[int](7)\n    print(take_int(m))\n")


INLINE_SHAPES = {
    "v_struct_armA": v_struct_armA,
    "v_struct_armB": v_struct_armB,
    "v_enum_armA": v_enum_armA,
    "v_enum_armB": v_enum_armB,
    "m_struct_ctor": m_struct_ctor,
    "o_newtype_armB": o_newtype_armB,
    "o_alias_armB": o_alias_armB,
    "o_builtin_ctor": o_builtin_ctor,
    "o_builtin_armB": o_builtin_armB,
}

# (library source, entry source) — the user's type lives in another file.
MODULE_SHAPES = {
    "v_struct_armA": (
        lambda n: f"struct {n}[T]:\n    T first\n    T second\n",
        lambda n: (f"from mylib import {n}\n\nint take_w({n}[int] w):\n"
                   f"    return w.second\n\nvoid main():\n    print(take_w(5))\n")),
    "v_struct_armB": (
        lambda n: f"struct {n}[T]:\n    T value\n",
        lambda n: (f"from mylib import {n}\n\nint take_int(int x):\n    return x + 1\n\n"
                   f"void main():\n    {n}[int] w = {n}[int](7)\n    print(take_int(w))\n")),
    "v_enum_armA": (
        lambda n: f"enum {n}[T]:\n    Held(T)\n",
        lambda n: (f"from mylib import {n}\n\nint take_w({n}[int] w):\n    match w:\n"
                   f"        case {n}.Held(v): return v\n    return 0\n\n"
                   f"void main():\n    print(take_w(5))\n")),
    "v_enum_armB": (
        lambda n: f"enum {n}[T]:\n    Held(T)\n",
        lambda n: (f"from mylib import {n}\n\nint take_int(int x):\n    return x + 1\n\n"
                   f"void main():\n    {n}[int] w = {n}.Held(7)\n    print(take_int(w))\n")),
    "m_struct_ctor": (
        lambda n: f"struct {n}[T]:\n    T value\n",
        lambda n: (f"from mylib import {n}\n\nvoid main():\n"
                   f"    {n}[int] m = {n}(5)\n    print(1)\n")),
}


def build_cases(root):
    """Materialize the full product under `root`; return [(case_id, path)]."""
    cases = []
    for shape, fn in INLINE_SHAPES.items():
        for n in NAMES + CONTROLS:
            d = os.path.join(root, "inline", f"{shape}__{n}")
            os.makedirs(d, exist_ok=True)
            p = os.path.join(d, "main.gg")
            with open(p, "w") as f:
                f.write(fn(n))
            cases.append((f"inline/{shape}/{n}", p))
    for shape, (libf, mainf) in MODULE_SHAPES.items():
        for n in NAMES + CONTROLS:
            d = os.path.join(root, "module", f"{shape}__{n}")
            os.makedirs(d, exist_ok=True)
            with open(os.path.join(d, "mylib.gg"), "w") as f:
                f.write(libf(n))
            p = os.path.join(d, "main.gg")
            with open(p, "w") as f:
                f.write(mainf(n))
            cases.append((f"module/{shape}/{n}", p))
    return cases


def probe(binary, path, root):
    """rc + NORMALISED stderr. The scratch path is stripped so two runs in
    different temp dirs stay comparable, and ANSI colouring is removed so a
    string compare answers the question it looks like it answers."""
    r = proc_guard.run([binary, "check", path], timeout=300)
    err = ANSI.sub("", r.stderr).replace(root, "<ROOT>")
    return r.returncode, err


def first_diag(err):
    for ln in err.splitlines():
        if "error[" in ln:
            return ln.strip()
    for ln in err.splitlines():
        if ln.strip():
            return ln.strip()
    return "(no diagnostic)"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--pre", help="gg binary from BEFORE the migration")
    ap.add_argument("--post", help="gg binary from AFTER the migration")
    ap.add_argument("--gg", help="single gg binary; dump the matrix, no comparison")
    ap.add_argument("--json", metavar="PATH", help="also write the raw rows here")
    args = ap.parse_args()

    if not args.gg and not (args.pre and args.post):
        ap.error("give either --gg, or both --pre and --post")

    with tempfile.TemporaryDirectory(prefix="coercion_identity_matrix_") as root:
        cases = build_cases(root)
        print(f"cells: {len(cases)}  "
              f"= names({len(NAMES)}) + controls({len(CONTROLS)}) "
              f"x shapes(inline {len(INLINE_SHAPES)} + module {len(MODULE_SHAPES)})")

        if args.gg:
            for cid, path in cases:
                rc, err = probe(args.gg, path, root)
                print(f"  {cid:38s} rc={rc}  {first_diag(err)[:100]}")
            return 0

        rows = []
        for cid, path in cases:
            prc, perr = probe(args.pre, path, root)
            orc, oerr = probe(args.post, path, root)
            rows.append({"case": cid, "pre_rc": prc, "post_rc": orc,
                         "pre_err": perr, "post_err": oerr,
                         "is_control": cid.rsplit("/", 1)[1] in CONTROLS})

    if args.json:
        with open(args.json, "w") as f:
            json.dump(rows, f, indent=1)

    verdict = [r for r in rows if r["pre_rc"] != r["post_rc"]]
    message = [r for r in rows if r["pre_rc"] == r["post_rc"] and r["pre_err"] != r["post_err"]]
    same = [r for r in rows if r["pre_rc"] == r["post_rc"] and r["pre_err"] == r["post_err"]]

    print("=" * 78)
    print(f"VERDICT-CHANGED (rc differs)              : {len(verdict)}")
    for r in verdict:
        print(f"  {r['case']:38s} rc {r['pre_rc']} -> {r['post_rc']}")
        print(f"      pre : {first_diag(r['pre_err'])[:96]}")
        print(f"      post: {first_diag(r['post_err'])[:96]}")
    print("-" * 78)
    print(f"MESSAGE-CHANGED (same rc, stderr differs) : {len(message)}")
    for r in message:
        print(f"  {r['case']:38s} rc {r['pre_rc']}")
        print(f"      pre : {first_diag(r['pre_err'])[:96]}")
        print(f"      post: {first_diag(r['post_err'])[:96]}")
    print("-" * 78)
    print(f"UNCHANGED                                 : {len(same)}")
    print("=" * 78)
    print(f"TOTAL CHANGED = {len(verdict) + len(message)}")

    moved_controls = [r["case"] for r in verdict + message if r["is_control"]]
    if moved_controls:
        # ⛔ NOT `return 0`. The line above already calls this an invalidation;
        # a run whose controls moved is measuring something other than the
        # migration, and a reader who checks only the exit code must see that.
        print(f"⛔ CONTROL MOVED (this invalidates the run): {moved_controls}")
        return 3
    print(f"controls held: {len(CONTROLS)} names x "
          f"{len(INLINE_SHAPES) + len(MODULE_SHAPES)} shapes, none changed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
