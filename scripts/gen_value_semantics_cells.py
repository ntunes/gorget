#!/usr/bin/env python3
"""Generate topic 30 of the robustness map: VIEW INVALIDATION UNDER MUTATION.

    python3 scripts/gen_value_semantics_cells.py           # write cells + rows
    python3 scripts/gen_value_semantics_cells.py --check    # verify, write nothing

A cell is one small program in which a value is observed through two PLACES --
one of which was bound from the other -- with a mutation in between. The
question it asks is the oldest question in the CoW design and the one the map
had no cell for: WHEN YOU MUTATE THROUGH ONE PLACE, WHAT DOES THE OTHER READ?

======================================================================
THE EXPECTATION RULE, AND WHY GENERATING IT IS LEGITIMATE HERE
======================================================================

`scripts/robustness_map.py`'s first standing property is that expectations are
HAND-DERIVED, NEVER CAPTURED -- "never 'update' an expectation to match what the
compiler prints, that pins a bug as canonical". A generator is only admissible
under that rule if it knows the right answer WITHOUT RUNNING ANYTHING, and this
one does: it writes the initial value, it writes the mutation, and one sentence
from the language design then fixes the output completely.

    A MUTATION THROUGH ONE PLACE IS OBSERVABLE THROUGH THAT PLACE ONLY;
    EVERY OTHER PLACE READS THE VALUE IT HAD BEFORE.

"Place" and not "name" deliberately: for `field`, `getter`, `getter_elem`,
`getter_deep`, `getvia` and `nested_field` the observation is a place EXPRESSION
(`s.f`, `o.i.f`, a `.get(i).unwrap()` chain), not an identifier. A
`.get(i).unwrap()` chain IS a place here -- it denotes a borrow into the
container, not a copy of it -- and that is why it appears on both sides of the
rule.

Grounding: `docs/language-design.md` section 3.5 (copy-on-write value semantics)
and ratified decision `D52` obligation (ii) in
`docs/define-gorget/decisions.md`, which describes THIS family and says of a
defect in it, verbatim, "a bug there is a UAF, not a wrong answer".

WHAT MAKES THE RULE APPLY UNIFORMLY ACROSS THE SITE AXIS: every site executes
its mutation EXACTLY ONCE -- `while __w < 1`, `for __i in 0..1`, `loop:` + an
immediate `break`, `case 1` of `match 1`, `for __x in [1]`, and the three
if/elif shapes whose conditions are constants. A site therefore changes WHERE
the mutation is written, never HOW MANY TIMES it runs, so "the value it had
before" is a single well-defined value in all eleven.

NOTHING IN THIS FILE RUNS THE COMPILER. There is no `subprocess` import, and
there must never be one: the moment an expectation could be influenced by what
`gg` printed, the map's first property is gone. (`scripts/proc_guard.py` also
counts un-deadlined `subprocess.run` calls in tracked scripts; this file
contributes zero to that census and must keep doing so.)

======================================================================
TWO DIRECTIONS, AND WHY THE SMALLER ONE IS NOT REDUNDANT
======================================================================

FORWARD (693 cells): mutate through the SOURCE place, observe the VIEW.
    Expected = the pre-mutation value, one line.

MIRROR (126 cells): mutate through the VIEW, observe the SOURCE place AND the
    VIEW, in that order. Expected = pre-mutation, then post-mutation
    (`10 / 777` -- the map joins output lines with " / ").

`todo/t0750` names both directions as one filed defect and records that whether
they are one mechanism or two is OPEN. They are not a symmetric re-run: measured
at HEAD, the two directions differ on exactly one cell of the SOURCE axis --
`getvia` is broken on 1 of 7 payloads forward (`string`) but on 7 of 7 in the
mirror, while the other eight sources behave identically. The mirror finds six
payload-cells the forward direction structurally cannot.

The mirror observes BOTH places because a source-only observation is GREEN AT
HEAD at every site: measured, the lost write is in the VIEW, so a source-only
mirror cell would have baselined WORKS across the board and measured nothing.
The forward direction is the opposite case and was also measured: over all 693
forward cells there is NO cell where the source reads its pre-mutation value,
and view-only and both-observed verdicts agree everywhere. Widening the forward
observation would therefore change nothing except the baseline.

======================================================================
!! SCOPE LIMITS -- WHAT THIS TOPIC DOES **NOT** COVER !!
======================================================================

This topic looks like an enumeration of "value semantics under mutation". IT IS
NOT ONE, and the point of writing that here rather than only in a review is that
a round-scoped brief is deleted and this file is not. A previous sweep of this
same bug family (2026-07-06) found four holes and vanished, because nothing
recorded either that it had run or what it had never covered.

SOURCE is a SELECTION, not an enumeration. Its nine kinds are the provenances
the CoW machinery can RECORD (`BorrowOrigin` x `CollectionId`) -- which is not
the same set as the shapes a user can WRITE. UNPROBED, and unreachable by any
widening of SITE or PAYLOAD:

  * closure captures            (a view captured by a closure, mutated outside)
  * trait-default receivers     (a view bound inside a trait default body)
  * comprehension binds         (a view bound by a comprehension's binder)
  * generic-equip receivers     (a view bound through a generic `equip` block)

Filed as `todo/t1384`. A later round that widens SOURCE should start there.

SITE is total for BLOCK-INTRODUCING statements and named where it is not.
Omitted, with reasons: `Select` and `With` are cells with NO SUBJECT -- `with`
needs an allocator binding and `select` needs channels, so the mutation cannot
be written into them without changing what else the program does; `on error`
degenerates into a straight-line mutation after the handler and so is not a
nested-block cell at all; `Snapshot`, `AssertReturn`, the six `Meta*` variants
and `Item` are compile-time or item-level and take no runtime mutation.

PAYLOAD is total on the axis that matters. The TYPE axis is infinite; the DROP
STRATEGY axis is closed (`DropStrategy` = None | Trivial | Recursive | Custom in
`src/ir/types.rs`) and these seven payloads cover all four values. An eighth,
`Vector[Option[String]]`, was dropped: all 99 of its cells fail to BUILD on both
value lanes today (`todo/t0002`), so they would measure nothing at all.

DIRECTION is a two-valued axis and the mirror covers 2 of the 11 sites. That is
not sampling for its own sake: SITE is measurably SATURATED at HEAD -- every
diverging row splits identically, `straight` correct against all ten other sites
broken, in both directions -- so `straight` x `while` carries the axis's entire
signal today. The axis is kept at FULL WIDTH in the forward direction anyway,
because a PARTIAL fix (one that repairs `if` and not `match`) is the one thing
the site axis exists to catch, and it has a single shape today only because
nothing is fixed yet.

======================================================================
BASELINE, AND WHY IT IS NOT `WORKS`
======================================================================

This topic is BASELINED AT THE MEASURED BUCKET. Roughly 210 forward cells and 27
mirror cells land WRONG on the C and LLVM lanes on arrival -- the defect this
corpus exists to pin is live, so a green corpus would have been the surprising
outcome. The numbers going UP before they go down is the intended shape
(`todo/t0956`); a later WRONG -> WORKS flip is PROGRESS and folds through a
reviewed `--accept`.

Seeding a non-good baseline needs `--seed-new`, in ONE five-lane run:

    python3 scripts/robustness_map.py --lanes all --accept --seed-new \\
        --topic "30 view invalidation (9 of N sources, mirror 2/11)"

and drift inside this topic is FATAL rather than report-only
(`FATAL_DRIFT_TOPIC_PREFIX` in `scripts/robustness_map.py`), because a topic born
fully seeded has zero day-one drift and can start at stage 3 of that ratchet.
"""
import argparse
import itertools
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
MAP = ROOT / "tests/fixtures/robustness_map"
CELLS = MAP / "cells"
MANIFEST = MAP / "MANIFEST.tsv"

# Column 1 of every generated row. It is the topic's most durable disclosure:
# it prints on the report's per-topic table row and it is typed into every
# `--topic` invocation, so the scope limit rides along with the name itself
# rather than living only in this docstring.
#
# THE LENGTH CEILING IS REAL: the report field is `{topic:<52}` and the longest
# existing topic string is 51 characters. Regenerate the ceiling with
#   awk -F'\t' 'NR>1{print length($1)}' \
#       tests/fixtures/robustness_map/MANIFEST.tsv | sort -n | tail -1
# `9 of N` rather than `9 sources`, because "there are 9 sources" is exactly the
# false reading the SCOPE LIMITS section above exists to prevent.
TOPIC = "30 view invalidation (9 of N sources, mirror 2/11)"
TOPIC_LEN_CEILING = 51

# Cell-name prefix. This generator OWNS every cell under it: a `.gg` matching the
# prefix that the axes no longer produce is deleted, and a MANIFEST row under
# this topic whose cell is not generated is an error. That is what makes
# "idempotent" checkable rather than aspirational.
PREFIX = "vsm_"
MIRROR_INFIX = "mir_"

NCOLS = 11
COL_TOPIC, COL_CELL, COL_C, COL_EXPECTED, COL_ACTUAL, COL_NOTE = range(6)
COL_LLVM, COL_SELFHOST, COL_DIVERGE, COL_ASAN, COL_GGDEF = 6, 7, 8, 9, 10
# The four columns this generator owns. Everything else -- the five lane
# baselines, the divergence flag, the `actual` column -- belongs to
# `robustness_map.py --accept`, which rewrites MANIFEST.tsv WHOLESALE. If this
# generator rewrote a bucket, regenerating would silently revert every accepted
# baseline in the topic, which is the map's entire progress record.
OWNED = (COL_TOPIC, COL_CELL, COL_EXPECTED, COL_NOTE)

# --------------------------------------------------------------------------
# PAYLOAD axis. Witness: `DropStrategy` (src/ir/types.rs) x container kind.
#   ty   : declared type of the local / field that is VIEWED
#   init : initial value expression
#   mut  : the mutation, "{T}" = the place being mutated
#   obs  : the observation, "{V}" = the place being read
#   pre  : what `obs` prints BEFORE the mutation
#   post : what `obs` prints AFTER a mutation through the observed place
# --------------------------------------------------------------------------
PAYLOADS = {
    "vec_int":  dict(ty="Vector[int]", init="[10, 20]",
                     mut='{T}.set(0, 777)',
                     obs='print(f"{{V}.get(0).unwrap()}")',
                     pre="10", post="777"),
    "vec_str":  dict(ty="Vector[String]", init='["aa", "bb"]',
                     mut='{T}.set(0, "ZZ")',
                     obs='print({V}.get(0).unwrap())',
                     pre="aa", post="ZZ"),
    "string":   dict(ty="String", init='"ab"',
                     mut='{T}.push("Z")',
                     obs='print({V})',
                     pre="ab", post="abZ"),
    "dict":     dict(ty="Dict[String,int]", init='{"k": 10}',
                     mut='{T}.put("k2", 777)',
                     obs='print(f"{{V}.len()}")',
                     pre="1", post="2"),
    "set":      dict(ty="Set[int]", init="{10}",
                     mut='{T}.add(777)',
                     obs='print(f"{{V}.len()}")',
                     pre="1", post="2"),
    "vec_vec":  dict(ty="Vector[Vector[int]]", init="[[10], [20]]",
                     mut='{T}.set(0, [777])',
                     obs='print(f"{{V}.get(0).unwrap().get(0).unwrap()}")',
                     pre="10", post="777"),
    "vec_push": dict(ty="Vector[int]", init="[10, 20]",
                     mut='{T}.push(777)',
                     obs='print(f"{{V}.len()}")',
                     pre="2", post="3"),
}

# --------------------------------------------------------------------------
# SITE axis. Witness: the block-introducing variants of `pub enum Stmt`
# (src/parser/ast.rs), which rustc keeps exhaustive:
#   awk '/^pub enum Stmt/,/^}/' src/parser/ast.rs | grep -E '^    [A-Z]'
# Every arm runs its mutation EXACTLY ONCE -- see the docstring.
# --------------------------------------------------------------------------
SITES = ["straight", "if_then", "if_else", "elif", "while", "for_range",
         "loop_break", "match_arm", "nested_if", "named_scope", "for_in_vec"]


def site_lines(kind, mut, ind):
    i, j, k = ind, ind + "    ", ind + "        "
    if kind == "straight":
        return [i + mut]
    if kind == "if_then":
        return [i + "if 1 == 1:", j + mut]
    if kind == "if_else":
        return [i + "if 1 == 2:", j + "pass", i + "else:", j + mut]
    if kind == "elif":
        return [i + "if 1 == 2:", j + "pass", i + "elif 1 == 1:", j + mut]
    if kind == "while":
        return [i + "int __w = 0", i + "while __w < 1:", j + mut, j + "__w += 1"]
    if kind == "for_range":
        return [i + "for __i in 0..1:", j + mut]
    if kind == "loop_break":
        return [i + "loop:", j + mut, j + "break"]
    if kind == "match_arm":
        return [i + "match 1:", j + "case 1:", k + mut, j + "else:", k + "pass"]
    if kind == "nested_if":
        return [i + "if 1 == 1:", j + "if 1 == 1:", k + mut]
    if kind == "named_scope":
        return [i + "blk:", j + mut]
    if kind == "for_in_vec":
        return [i + "Vector[int] __xs = [1]", i + "for __x in __xs:", j + mut]
    raise KeyError(kind)


# --------------------------------------------------------------------------
# SOURCE axis. Witness: `BorrowOrigin` (src/ir/mod.rs) x `CollectionId`
# (src/ir/lowering/context.rs). SEE THE SCOPE LIMITS SECTION: this is a
# SELECTION, and the four kinds it omits are named there and filed as t1384.
#
# Each entry returns (top-level decls, setup lines, VIEW place, SOURCE place).
# `param` is special-cased in the emitters: its view is bound inside a callee
# from a mutable-borrow parameter, so it has no top-level form.
# --------------------------------------------------------------------------
SOURCES = ["getter", "field", "local", "getter_elem", "getvia", "param",
           "alias2", "nested_field", "getter_deep"]


def source_parts(kind, ty, init):
    if kind == "getter":
        pre = ["struct Store:", f"    {ty} f", "equip Store:",
               f"    {ty} getf(&self):", "        return self.f"]
        setup = [f"    {ty} __p = {init}", "    Store s = Store(__p)",
                 f"    {ty} c = s.getf()"]
        return pre, setup, "c", "s.f"
    if kind == "field":
        pre = ["struct Store:", f"    {ty} f"]
        setup = [f"    {ty} __p = {init}", "    Store s = Store(__p)",
                 f"    {ty} c = s.f"]
        return pre, setup, "c", "s.f"
    if kind == "local":
        setup = [f"    {ty} v = {init}", f"    {ty} c = v"]
        return [], setup, "c", "v"
    if kind == "getter_elem":
        pre = ["struct Store:", f"    Vector[{ty}] f", "equip Store:",
               f"    {ty} getf(&self):", "        return self.f.get(0).unwrap()"]
        setup = [f"    Vector[{ty}] __p = [{init}]", "    Store s = Store(__p)",
                 f"    {ty} c = s.getf()"]
        return pre, setup, "c", "s.f.get(0).unwrap()"
    if kind == "getvia":
        setup = [f"    Vector[{ty}] v = [{init}]",
                 f"    {ty} c = v.get(0).unwrap()"]
        return [], setup, "c", "v.get(0).unwrap()"
    if kind == "alias2":
        setup = [f"    {ty} v = {init}", f"    {ty} b = v", f"    {ty} c = b"]
        return [], setup, "c", "v"
    if kind == "nested_field":
        pre = ["struct Inner:", f"    {ty} f", "struct Outer:", "    Inner i"]
        setup = [f"    {ty} __p = {init}", "    Inner __in = Inner(__p)",
                 "    Outer o = Outer(__in)", f"    {ty} c = o.i.f"]
        return pre, setup, "c", "o.i.f"
    if kind == "getter_deep":
        pre = ["struct Inner:", f"    {ty} f", "struct Outer:", "    Inner i",
               "equip Outer:", f"    {ty} getf(&self):", "        return self.i.f"]
        setup = [f"    {ty} __p = {init}", "    Inner __in = Inner(__p)",
                 "    Outer o = Outer(__in)", f"    {ty} c = o.getf()"]
        return pre, setup, "c", "o.i.f"
    raise KeyError(kind)


def _emit(source, site, payload, mutate_view, observe):
    """Build one cell's source text.

    `mutate_view` picks WHICH of the two places the mutation goes through;
    `observe` is the list of places to print, in order. Both directions come out
    of this one emitter so they cannot drift apart.
    """
    p = PAYLOADS[payload]
    if source == "param":
        pre, setup = [], [f"    {p['ty']} c = v"]
        view, place = "c", "v"
        head = [f"void probe({p['ty']} &v):"]
        tail = ["void main():", f"    {p['ty']} q = {p['init']}", "    probe(&q)"]
    else:
        pre, setup, view, place = source_parts(source, p["ty"], p["init"])
        head, tail = ["void main():"], []
    target = view if mutate_view else place
    lines = list(pre) + head + setup
    lines += site_lines(site, p["mut"].replace("{T}", target), "    ")
    lines += ["    " + p["obs"].replace("{V}", {"view": view, "source": place}[o])
              for o in observe]
    return "\n".join(lines + tail) + "\n"


def forward(source, site, payload):
    """Mutate the SOURCE place; read the VIEW. Expected = pre-mutation."""
    return (_emit(source, site, payload, mutate_view=False, observe=["view"]),
            PAYLOADS[payload]["pre"])


def mirror(source, site, payload):
    """Mutate the VIEW; read the SOURCE place, then the VIEW.

    Expected = pre-mutation, then post-mutation. The map joins output lines with
    " / ", so a two-observation cell's expectation is `10 / 777`.
    """
    p = PAYLOADS[payload]
    return (_emit(source, site, payload, mutate_view=True,
                  observe=["source", "view"]),
            f"{p['pre']} / {p['post']}")


MIRROR_SITES = ["straight", "while"]

# --------------------------------------------------------------------------
# THE TOPIC'S OWN POSITIVE CONTROL.
#
# The map's `_POSITIVE_CONTROL_broken` proves the HARNESS can see a failure. It
# does not prove that THIS topic's comparison fires, and a 819-cell topic that
# cannot demonstrate its own instrument is exactly the kind of thing that
# quietly measures nothing.
#
# The control is one more cell of this corpus with a DELIBERATELY WRONG
# expectation -- the MUTATED value where the rule says the pre-mutation value --
# so every lane must score it non-WORKS. `robustness_map.py` inverts the verdict
# for a row whose C column reads CONTROL: a lane on which it PASSES is reported
# as a regression, "harness is blind".
#
# !! WHICH ROW IT IS DRAWN FROM IS LOAD-BEARING, AND MOST PICKS ARE WRONG. !!
# On the source x payload rows where the compiler is ALREADY BROKEN it already
# prints the mutated value, so a control drawn from one of those MATCHES, scores
# WORKS, and fires "CONTROL PASSED - harness is blind" on arrival: red, and worse
# than red, because the control's meaning is then silently inverted. Measured: 30%
# of naive picks land there. This one is drawn from a row that is GREEN ON EVERY
# LANE at HEAD -- verify with
#   python3 scripts/robustness_map.py --lanes all --topic "30 " --detail
# and re-point it, never widen it, if that row ever stops being green.
CONTROL_SOURCE, CONTROL_SITE, CONTROL_PAYLOAD = "field", "straight", "vec_int"
CONTROL_CELL = f"{PREFIX}POSITIVE_CONTROL_view_reads_pre_mutation"

NOTE_FWD = ("fwd: mutate source place, read view | SCOPE LIMITS + 4 unprobed "
            "source kinds: scripts/gen_value_semantics_cells.py, todo/t1384")
NOTE_MIR = ("mirror: mutate view, read source place then view | SCOPE LIMITS + "
            "4 unprobed source kinds: scripts/gen_value_semantics_cells.py, "
            "todo/t1384")
NOTE_CTL = ("POSITIVE CONTROL for topic 30: deliberately expects the MUTATED "
            "value, so every lane must score it non-WORKS. Drawn from "
            f"{PREFIX}{CONTROL_SOURCE}__{CONTROL_SITE}__{CONTROL_PAYLOAD}, which "
            "is green on every lane. | scripts/gen_value_semantics_cells.py")


def generate():
    """Return {cell_name: (source_text, expected, note, is_control)}.

    Deterministic, and nothing is run.
    """
    out = {}
    for source, site, payload in itertools.product(SOURCES, SITES, PAYLOADS):
        text, expected = forward(source, site, payload)
        out[f"{PREFIX}{source}__{site}__{payload}"] = (text, expected,
                                                       NOTE_FWD, False)
    for source, site, payload in itertools.product(SOURCES, MIRROR_SITES,
                                                   PAYLOADS):
        text, expected = mirror(source, site, payload)
        out[f"{PREFIX}{MIRROR_INFIX}{source}__{site}__{payload}"] = (
            text, expected, NOTE_MIR, False)
    # The control shares its PROGRAM with a forward cell and differs only in the
    # expectation, which is the mutated value instead of the pre-mutation one.
    ctl_text, ctl_pre = forward(CONTROL_SOURCE, CONTROL_SITE, CONTROL_PAYLOAD)
    ctl_wrong = PAYLOADS[CONTROL_PAYLOAD]["post"]
    assert ctl_wrong != ctl_pre, "the control's expectation must be WRONG"
    out[CONTROL_CELL] = (ctl_text, ctl_wrong, NOTE_CTL, True)
    return out


def read_manifest():
    raw = MANIFEST.read_text().splitlines()
    header = raw[0]
    rows = [ln.split("\t") + [""] * (NCOLS - len(ln.split("\t")))
            for ln in raw[1:] if ln.strip()]
    return header, rows


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="verify the committed cells and MANIFEST rows match "
                         "what this generator produces; write nothing, exit 1 "
                         "on any difference")
    ap.add_argument("--force-expectation-change", action="store_true",
                    help="OVERWRITE a committed expectation/note that differs "
                         "from the generated one. Needed only when the RULE "
                         "itself changes, and it invalidates the baseline: "
                         "re-seed with --seed-new in the same commit.")
    args = ap.parse_args()

    assert len(TOPIC) <= TOPIC_LEN_CEILING, (
        f"topic string is {len(TOPIC)} chars, ceiling {TOPIC_LEN_CEILING}: the "
        "report field is {topic:<52} and a longer string breaks the columns")

    cells = generate()
    header, rows = read_manifest()
    by_cell = {r[COL_CELL]: r for r in rows}

    problems, written, removed = [], [], []

    # --- 1. the .gg files -------------------------------------------------
    for name, (text, _, _, _) in sorted(cells.items()):
        path = CELLS / f"{name}.gg"
        if path.exists() and path.read_text() == text:
            continue
        if args.check:
            problems.append(f"cell {name}.gg is "
                            f"{'STALE' if path.exists() else 'MISSING'}")
        else:
            path.write_text(text)
            written.append(name)

    # This generator owns its whole prefix, so a leftover cell from a retired
    # axis value is removed rather than left to rot as an orphan -- an orphan
    # would trip `robustness_map_manifest_and_cells_reconcile` anyway.
    for path in sorted(CELLS.glob(f"{PREFIX}*.gg")):
        if path.stem in cells:
            continue
        if args.check:
            problems.append(f"cell {path.name} is STALE (no longer generated)")
        else:
            path.unlink()
            removed.append(path.stem)

    # --- 2. the MANIFEST rows --------------------------------------------
    # OWNED columns only. An existing row keeps its bucket columns VERBATIM: if
    # this generator rewrote them, a regeneration after any `--accept` would
    # silently revert every reviewed baseline in the topic.
    new_rows = []
    for name, (_, expected, note, is_control) in sorted(cells.items()):
        existing = by_cell.get(name)
        if existing is None:
            row = [""] * NCOLS
            row[COL_TOPIC], row[COL_CELL] = TOPIC, name
            row[COL_EXPECTED], row[COL_NOTE] = expected, note
            if is_control:
                row[COL_C], row[COL_ACTUAL] = "CONTROL", "actual"
            new_rows.append(row)
            continue
        want = {COL_TOPIC: TOPIC, COL_EXPECTED: expected, COL_NOTE: note}
        for col, value in want.items():
            if existing[col] == value:
                continue
            label = {COL_TOPIC: "topic", COL_EXPECTED: "expected",
                     COL_NOTE: "note"}[col]
            if args.force_expectation_change and not args.check:
                existing[col] = value
            else:
                problems.append(
                    f"{name}: committed {label} {existing[col]!r} != generated "
                    f"{value!r}. The generator NEVER silently overwrites a "
                    f"committed expectation (that is how a bug gets pinned as "
                    f"canonical). Either fix the rule, or pass "
                    f"--force-expectation-change and RE-SEED the baseline.")
        if is_control and existing[COL_C] != "CONTROL":
            problems.append(f"{name}: the control row's bucket column must read "
                            f"CONTROL, found {existing[COL_C]!r}")

    stale = [r[COL_CELL] for r in rows
             if r[COL_TOPIC] == TOPIC and r[COL_CELL] not in cells]
    for name in stale:
        problems.append(f"MANIFEST row {name} is under topic 30 but is no "
                        f"longer generated -- delete the row")

    if problems:
        for p in problems:
            print(f"  {p}")
        print(f"\n{len(problems)} problem(s). "
              f"{'Re-run without --check to fix what is fixable.' if args.check else ''}")
        return 1

    if new_rows and not args.check:
        # Topic 30 sorts last, and the file is ordered by (topic, cell), so the
        # new block appends. Sorting the whole file instead would rewrite rows
        # this generator does not own.
        rows = rows + new_rows
        rows.sort(key=lambda r: (r[COL_TOPIC], r[COL_CELL]))
        MANIFEST.write_text(header + "\n"
                            + "\n".join("\t".join(r) for r in rows) + "\n")

    fwd = sum(1 for n in cells if not n.startswith(PREFIX + MIRROR_INFIX)
              and n != CONTROL_CELL)
    mir = sum(1 for n in cells if n.startswith(PREFIX + MIRROR_INFIX))
    if args.check:
        print(f"OK: {fwd} forward + {mir} mirror + 1 control cell, "
              f"{len(cells)} rows, all current.")
    else:
        print(f"{fwd} forward + {mir} mirror + 1 control = {len(cells)} cells; "
              f"{len(written)} written, {len(removed)} removed, "
              f"{len(new_rows)} MANIFEST rows added.")
        if new_rows:
            print("\nSEED THE BASELINE IN ONE FIVE-LANE RUN:\n"
                  f'    python3 scripts/robustness_map.py --lanes all --accept '
                  f'--seed-new --topic "{TOPIC}"')
    return 0


if __name__ == "__main__":
    sys.exit(main())
