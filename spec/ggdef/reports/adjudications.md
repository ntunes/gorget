# Adjudications — RFC §6(c), from the definition

**Purpose.** Phase-0 acceptance (c) (RFC §6): `ggdef` **adjudicates** the two
gorget-smith bugs and the EMove question **from the definition** — i.e. the
correct value is whatever eager value semantics (D1) computes, independent of any
backend. Each verdict below is the `ggdef` outcome plus the `-- trace` excerpt
that justifies it. The definition is authoritative; where production has
converged (EMove) or still diverges (the two smith bugs), that history is cited.

**Regenerate every verdict (never trust the cached values below):**

```
cargo run -p ggdef -- run   tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg   # 9
cargo run -p ggdef -- trace tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg
cargo run -p ggdef -- run   tests/fixtures/move_param_concat.gg            # ablog
cargo run -p ggdef -- trace tests/fixtures/move_param_concat.gg
cargo run -p ggdef -- run   spectests/run/emove_lazy_bind_witness.gg                  # hello
cargo run -p ggdef -- trace spectests/run/emove_lazy_bind_witness.gg
```

The three programs use **no custom-`Drop` type**, so the B2-disclosed transitive
custom-drop gap (`run_custom_drop` is top-level only — a filed phase-1 MUST) does
NOT touch any verdict here.

---

## Adjudication #1 — dead-branch alias bind → `9`

`tests/fixtures/known_gaps/cow_dead_branch_alias_bind.gg` (mirrored as the seed
`spectests/run/smith_dead_branch_alias_bind.gg`):

```gorget
void main():
    Vector[int] v0 = [1, 2, 3]
    if v0.len() < 3:          # 3 < 3 is false — the branch never runs
        Vector[int] v5 = v0
    v0[2] = 9
    print(v0[2])
```

**Verdict: `9`** (exit 0, Value).

**Trace excerpt (justification):**
```
{"event":"move","place":"v0","span":[17,43]}     # fresh-temp move of the [1,2,3] literal into v0
{"event":"write","place":"v0[2]","span":[97,106]} # v0[2] = 9 lands directly on the owned v0
{"event":"drop","place":"v0","span":[17,43]}      # scope exit
```
There is **no `bind_copy` for `v5`**: its guard is false, so the alias bind never
executes. `v0` is never aliased, so the later `v0[2] = 9` writes straight to the
owned vector and prints `9`. This is decisions.md **C1** (dead-branch alias bind =
zero clones, correct output) evaluated by the definition.

**History (invariant #8):** production (Rust gg) currently **SIGSEGVs** on this —
a both-backend bug found by gorget-smith round 1: the mutation-site CoW
materialize walks an alias slot that was never initialised because the bind lives
in a dead branch (ASan near-null SEGV in `gorget_array_clone`, C **and** LLVM);
the self-host prints `9` correctly (`tests/integration.rs:5019-5033`, `#[ignore]`,
filed TODO HIGH). The definition ratifies the language-intended `9`; the fix is
Rust-gg-side.

---

## Adjudication #2 — `String !p` move-param + concat → `ablog`

`tests/fixtures/move_param_concat.gg` (mirrored as the seed
`spectests/run/smith_move_param_concat.gg`):

```gorget
String f(String !p):
    return p + "log"

void main():
    String s = "ab"
    print(f(!s))
```

**Verdict: `ablog`** (exit 0, Value).

**Trace excerpt (justification):**
```
{"event":"move","place":"s","span":[60,75]}   # String s = "ab"  (fresh-temp move of the literal)
{"event":"move","place":"s","span":[80,92]}   # f(!s) moves s into the param p; source s killed
{"event":"drop","place":"p","span":[9,18]}    # p drops at f's scope exit
```
`!s` transfers ownership into the `Move` param `p`; `p + "log"` concatenates the
moved-in `"ab"` with the literal `"log"` → `"ablog"`. Value semantics: a `Move`
carries the value; the concat is an ordinary string operation on it.

**History (invariant #8):** production (Rust gg) currently **fails to compile**
this — it is `gg check`-accepted but the C backend emits `(void*)a + (void*)b` (cc
rejects: invalid operands to binary `+`) and the LLVM backend dies in `llc`; a
check-accepted program that fails at cc/llc means the front and back ends
disagree about validity (invariant #8 adjacent). The self-host prints `ablog`
correctly (`tests/integration.rs:5035-5048`, `#[ignore]`, filed TODO HIGH). The
definition ratifies `ablog`; the fix is Rust-gg-side.

---

## Adjudication #3 — the EMove witness → `hello` (pre-mutation value, D1)

> NOTE (output-review): the trace spans below are PROGRAM-RELATIVE — the seed fixture carries
> ~651 bytes of spectest frontmatter, so file-relative spans from the cited command are offset
> by that amount. Events, places, and the verdict are exact.

The witness, written to the pinned shape (devbook/11:716-733; the name asymmetry
is mandatory) as `spectests/run/emove_lazy_bind_witness.gg`:

```gorget
void main():
    Vector[String] v = ["hello", "world"]
    String s = v.get(0).unwrap()   # bind keyed by the PRE-move name `v`
    Vector[String] w = !v          # move v -> w
    w.set(0, "mutated")            # mutate through the POST-move name `w`
    print(s)
```

**Verdict: `hello`** (exit 0, Value) — the **PRE-mutation** value, per D1.

**Trace excerpt (justification):**
```
{"event":"move","place":"v","span":[17,54]}    # fresh-temp move of the [..] literal into v
{"event":"move","place":"s","span":[59,87]}    # s = v.get(0).unwrap()  (fresh-temp move of the unwrap result — s owns its own "hello")
{"event":"move","place":"v","span":[92,113]}   # w = !v  (explicit move; v killed)
{"event":"write","place":"w","span":[118,137]} # w.set(0,"mutated")  writes on w, NOT on s
{"event":"drop","place":"w","span":[92,113]}   # reverse-order scope drops
{"event":"drop","place":"s","span":[59,87]}
```
The bind of `s` snapshots `v.get(0).unwrap()` **as-of the bind point** (its own
owned `"hello"`); the later mutation goes through `w`, whose `write` event is on
`w` and cannot reach `s`'s independent snapshot. The name asymmetry (bind keyed
by `v`, mutation via `w`) is what makes this a genuine EMove witness rather than a
degenerate same-name read; `ggdef` prints the pre-mutation `hello`. This is D1's
derived consequence: *"`auto e = v.get(0).unwrap(); …` keeps its bind-time
value — a value bind, never an in-place borrow."*

**History (definition adjudicates; production converged):** the EMove class was
"Rust gg VALUE-WRONG on both EMove shapes" (devbook/11) — production's
`Expr::Move` read-through printed the POST-mutation value. That bug was **FIXED**
(Chain C item 1; surfaced/recorded during B1), the reference fixtures
`cow_lazy_move_bind.gg` / `cow_lazy_move_reassign.gg` were updated to expect the
pre-mutation value, and both the B1 and B2 gates now MATCH the committed
`s = hello`. So the definition and production **now AGREE** on the witness: the
definition adjudicated the pre-mutation value; production converged onto it.

---

## Summary

| # | Program | ggdef verdict | Rule | Production status |
|---|---|---|---|---|
| 1 | `cow_dead_branch_alias_bind` | `9` | C1 dead-branch alias = 0 clones | **diverges** — both-backend SIGSEGV (filed, `#[ignore]`) |
| 2 | `move_param_concat` | `ablog` | Move carries the value; concat | **diverges** — check-accepted, cc/llc reject (filed, `#[ignore]`) |
| 3 | EMove witness | `hello` | D1 value-bind snapshot (pre-mutation) | **agrees** — converged after the `Expr::Move` fix |

All three are `Value` outcomes; no `Trap`/`IllFormed`/`FuelExhausted`, no
custom-`Drop`. Acceptance (c) is met from the definition.
