# Scout: compound-assign through a Guard (`g.x += N`)

**Verdict: NO-GO — the bug is ALREADY FIXED on gorget-1. The brief's premise is STALE.**

Scouted on gorget-1 tip `5d6e9261` (worktree fast-forwarded, "Already up to date"). The
miscompile the brief describes — `g.x += 5` on `x=10` silently leaving it 10 — does **not**
reproduce. The exact fix the brief asks me to prototype (mirror the `lower_assign` guard-deref
arm into the compound-assign path) **already landed** in commit `c65e6651`, *261 commits before
HEAD*, on 2026-06-16, including the exact fixture the brief asks me to add.

This is the canonical "re-verify a premise against CURRENT source before acting" case
(CLAUDE.md "Solution Quality"; the resolver-57% / already-shipped-port saga in devbook/29).

---

## 1. Premise check — REPRODUCE (by RUNNING)

Built clean (`cargo build`, finished). Ran the committed fixture and a fresh independent repro
on **both backends**.

### Committed fixture `tests/fixtures/guard_compound_assign.gg` (Mutex Guard + RWLock WriteGuard)

```
# C backend                    # LLVM backend
$ gg run guard_compound_assign.gg     $ gg run --backend=llvm guard_compound_assign.gg
10                             10
15      <- g.x += 5  CORRECT   15
17      <- g.y -= 3  CORRECT   17
8090    <- wg.port += 10       8090
```

`g.x += 5` on `x=10` prints **15**, not 10. The bug is NOT present.

### Fresh independent repro (`+=`, `-=`, `*=`, `/=`, AND plain `=`) — both backends identical

```gorget
struct Counter:
    int x
    int y
void main():
    Mutex[Counter] m = Mutex[Counter](Counter(10, 100))
    Guard[Counter] g = m.lock()
    g.x += 5      print(g.x)   # 15
    g.x -= 2      print(g.x)   # 13
    g.x *= 3      print(g.x)   # 39
    g.y /= 4      print(g.y)   # 25
    g.x = 42      print(g.x)   # 42  (plain assign — proves read+write both route through guard)
```

C and LLVM both print `15 / 13 / 39 / 25 / 42`. All compound ops AND plain assign are correct.

### Bonus: index-compound-assign through a guard field also works

`g.items[1] += 5` on `[10,20,30]` prints **25** (the `Expr::Index` arm resolves its base through
`try_resolve_field_place`, which carries the guard arm). Whole class is healthy.

---

## 2. Root cause of the STALE premise — what the brief got wrong

The brief pins the missing arm at `lower_compound_assign` (`assigns.rs:1010`) vs the template at
`lower_assign` (`assigns.rs:661`). On the CURRENT tree:

- **`lower_compound_assign`'s `Expr::FieldAccess` arm does NOT resolve the field-place inline.**
  It delegates to a shared producer: `try_resolve_field_place(...)` at
  `src/ir/lowering/stmts/assigns.rs:1284`. The guard handling is not "missing from
  `lower_compound_assign`" — it was never *supposed* to live there; it lives in the shared producer.

- **`try_resolve_field_place` ALREADY carries the guard-deref arm**, at
  `src/ir/lowering/exprs/mod.rs:2474-2521`. It reads the typed wrapper name via
  `guard_inner_suffix` (`Guard__`/`ReadGuard__`/`WriteGuard__`), early-outs `None` for read-only
  guards (writes forbidden), and for a writable guard projects through
  `emit_guard_get_ptr(...)` → `(*get_ptr(&g)).field` — the SAME helper the read path
  (`lower_field_access`) and the plain-assign fallback (`lower_assign:666`) use. Its own comment
  (`mod.rs:2477-2484`) names compound-assign (`:1284`) and plain-assign (`:613`) as the consumers
  it centralizes for.

The `lower_assign:661` arm the brief cites as "the template the compound path lacks" is a
*second*, lower-priority copy of the guard arm in the plain-assign FALLBACK path (the inline
`set_field_value`-style branch that runs when `try_resolve_field_place` is not consulted). It is
NOT the only guard arm, and it is NOT what compound-assign relies on. Compound-assign relies on
the producer arm at `mod.rs:2474`.

### How the fix landed (git evidence)

```
$ git show -s --format="%H %ci%n%s" c65e6651
c65e6651969a43e48b83b1b750f552e53725fd62 2026-06-16 20:45:45 +0000
fix(ir): compound-assign through a Guard was a silent no-op (Rust gg)

$ git merge-base --is-ancestor c65e6651 HEAD; echo $?
0     # c65e6651 IS an ancestor of the gorget-1 tip (261 commits back)
```

`c65e6651`'s diff (`git show c65e6651`):
- `+49` lines to `src/ir/lowering/exprs/mod.rs` — the guard arm in `try_resolve_field_place`
  (centralized at the PRODUCER: "fix the class, not the instance" — plain assign, compound
  assign, and index-base resolution all flow through it).
- `+24` lines: `tests/fixtures/guard_compound_assign.gg` — the EXACT fixture the brief asks to add
  (Mutex Guard `+=`/`-=` + RWLock WriteGuard `+=`).
- `+5` lines: `tests/integration.rs` — the lock-in test `guard_compound_assign` asserting
  `"10\n15\n17\n8090"` (`tests/integration.rs:12609-12610`).

The commit body even notes the self-host already compiled it correctly (`634695f2`), so the
fixture is a self-host runtime-parity MATCH (denom +1, num +1).

---

## 3. Self-host status (premise #5) — already correct, confirmed by running

- Self-host carries the same deref at its two compound-assign write sites:
  `tests/fixtures/self_host_lowerer/lower_stmt.gg:1222` (`emit_field_write_from_local`) calls
  `guard_field_deref_base` at `:1248` (and `:1170` for the other write site). Landed `634695f2`
  (2026-06-16; typed `is_guard_struct_type`, mirrors Rust).
- Committed self-host runtime snapshot `tests/fixtures/runtime_snapshots/guard_compound_assign.out`
  = `10\n15\n17\n8090\n` (byte-checked with `od -c`).
- `self_host_runtime` + `self_host_runtime_diff` both PASS (`cargo test --test integration
  self_host_runtime`, 2 passed, 130.94s) — the self-host-emitted C for this fixture runs and
  matches the snapshot. So the self-host produces 15, not 10.

---

## 4. Gate battery run (all green)

- `cargo build` — finished, no errors.
- `gg run guard_compound_assign.gg` (C) → `10 15 17 8090` ✓
- `gg run --backend=llvm guard_compound_assign.gg` → `10 15 17 8090` ✓
- Fresh `+=/-=/*=//=`/plain repro, C + LLVM → `15 13 39 25 42` ✓ (both)
- Index-through-guard `g.items[1] += 5` (C) → `25` ✓
- `cargo test --test integration guard_compound_assign` → `1 passed` ✓
- `cargo test --test integration self_host_runtime` → `2 passed` (runtime + runtime_diff) ✓

---

## 5. Recommendation

**Do NOT launch the implementation track. There is nothing to fix and nothing to add** — the fix,
the fixture, the integration assertion, the LLVM-backend parity, and the self-host snapshot all
already exist and pass on gorget-1. Spending the brief→review→launch→review→integrate cycle on
this would re-land an identical, already-present fix (the already-shipped-port failure mode).

### Where the stale premise likely came from

The brief was written against a pre-`c65e6651` snapshot (before 2026-06-16). The cited line
numbers (`assigns.rs:661` / `:1010`) and the "lacks the guard-deref arm" framing match the world
*before* the producer-centralization landed. The self-host commit `634695f2` it cites as "already
correct" is from the SAME day as the Rust fix — both sides were fixed together. The premise that
"the self-host is NOW correct but Rust is still broken" never held on any committed tree; they
shipped as a pair.

### If anything is owed at all (optional, LOW priority — not part of this scout's mandate)

The current coverage is already strong (Mutex Guard `+=`/`-=`, RWLock WriteGuard `+=`, plus the
self-host parity snapshot). Two *purely additive* hardening ideas, only if a future round wants
them — neither is a bug, neither blocks anything:

1. **Negative test for ReadGuard write rejection.** Both the producer arm (`mod.rs:2488`) and the
   self-host return `None`/skip for read-only guards, with a comment "type checker should reject in
   future." Today a compound-assign *through a ReadGuard* is silently dropped rather than a compile
   error. That is a separate, pre-existing gap (NOT the brief's bug) — file as its own TODO if you
   want the typecheck rejection + negative fixture (per Core invariant #8, the principled fix is to
   REJECT it, not silently no-op). Out of scope here; flagging for the backlog.
2. `*=` / `/=` aren't in the committed fixture (only `+=`/`-=`). I verified them by hand above and
   they work; adding them to `guard_compound_assign.gg` would be a one-line cosmetic strengthening,
   not a correctness fix.

Neither is worth a delegated round on its own.

---

## Blast radius (for the record, had a fix been needed)

The guard arm lives in the shared producer `try_resolve_field_place`
(`src/ir/lowering/exprs/mod.rs:2474`), which feeds plain assign, compound assign, and index-base
resolution. It is backend-agnostic (GIR-level), so C and LLVM are at parity by construction (a
shared-LIR change, confirmed by both backends printing identical output). The arm only fires for
`Guard__`/`ReadGuard__`/`WriteGuard__` wrapper names (typed detection via `guard_inner_suffix`),
so non-guard field-places are untouched.
