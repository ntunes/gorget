# Brief — sec_19 soundness gap: arena-scoped `.get()` borrow-escape UAF accepted by `gg check`

**Track:** SECURITY / soundness (Rust borrow checker). **Scout = the adversarial review `aafd6327`** (RUN-verified the UAF under ASan; root cause + repro + false-positive boundary all pinned).

## The bug (REAL soundness gap — `gg check` accepts a heap-use-after-free)
A `.get()`/`.unwrap()` borrow of an ARENA-scoped, NON-Copy collection, assigned to an OUTER-scope variable, aliases into the arena's backing buffer; `gorget_arena_destroy` frees it at `with`-scope exit → the outer var dangles. `gg check` ACCEPTS it (it should REJECT). ASan: `heap-use-after-free` at `gorget_string_free` (freed by `gorget_arena_destroy`).

Minimal repro (`gg check` → "OK: no semantic errors"; `gg build --sanitize` + run → heap-use-after-free):
```
from std.alloc import Arena
from std.collections import Vector
void main():
    String peek = "init"
    with Arena(4096) as pool:
        Vector[String] v = ["payload-in-arena-memory"]
        peek = v.get(0).unwrap()        # borrow aliases into arena buffer
    print(peek)                          # UAF: arena destroyed at scope exit
```
Also reproduces via a `Dict[String,String].get(...).unwrap()` escape and via pushing the borrowed elem to an outer Vector. The `int`-element (Copy) arena variant is CLEAN (value-copied) — the danger is non-Copy elements only.

## Root cause (file:line)
`src/semantic/safety/check_stmt.rs:419-427` — the `ArenaEscape::AssignOuter` check computes `rhs_def_id` ONLY for a bare `Expr::Identifier` or `Expr::Move { Identifier }` RHS (`_ => None`). A borrow-producing RHS (`.get()`/`.unwrap()`/field/index/method chain) is never traced, so the arena-scoped source is never recognized and no error fires. The sibling **Return** check at `:643-654` (`ArenaEscapeKind::Return`) has the identical bare-identifier-only limitation — `return v.get(0).unwrap()` from an arena scope also dodges it. (The `return s` variant only gets caught because `s`, VarDecl'd inside the arena, is independently tagged `arena_scoped` at `:162-169`.)

This is a "trace the borrow origin, don't shallow-match the RHS" gap: the machinery to trace `.get()` → source already exists and is used 320 lines up.

## The fix (reuse EXISTING machinery; the non-Copy gate already exists)
In BOTH the AssignOuter (`:419-445`) and Return (`:643-654`) arena-escape checks, when the bare-identifier RHS match yields `None`, ALSO try the borrow-producing path:
- Call `self.find_collection_source_with_path(value)` (the SAME fn — `helpers.rs:427-444` — used at `:104` for `index_borrow_sources`; it recurses `unwrap`/`expect`→receiver and resolves `get`/`first`/`last`→`find_root_def_id_with_path`) to get the `root` source def_id of a `.get()`/index/field borrow chain.
- **⚠ R1 — the non-Copy gate applies on BOTH paths, and reads the BOUND VALUE's type, NOT the source collection's.** Only proceed if the bound value's type is non-Copy (`!is_copy_type(tid, self.types, self.scopes)`). The type to read is: **AssignOuter → `target_def_id.type_id`** (`peek`'s type = the element type); **Return → the returned expression's element/result type** (`current_return_type_id` or the `.get()` element type). Do NOT read the SOURCE collection's type — a `Vector[int]` source is non-Copy *as a collection* but its element `int` is Copy; gating on the collection type would false-fire on `int peek = arenaVec.get(0).unwrap()` (a SAFE value-copy, RUN-confirmed accepted today and must stay accepted). `:108-111` reads the *binding's* `def_id.type_id` — map that to `target_def_id` (Assign) / return-element (Return).
- Fire `ArenaEscape` (reuse the existing `AssignOuter{target}`/`Return` kinds — `errors.rs:470`, NO new variant — naming the `root` source) when: `root ∈ self.arena_scoped_vars` ∧ non-Copy-bound-value ∧ **(AssignOuter) target ∉ arena_scoped_vars** / **(Return) the value escapes by definition (no target check) — but the non-Copy gate STILL applies** (a `int first(): return v.get(0).unwrap()` from an arena scope is SAFE and must stay accepted).
- Keep the existing bare-identifier path unchanged (additive).

(Alternative tracer: `self.compute_expr_origin(value)` → a `BorrowOrigin` → check `references_def(arena_var)` for each `arena_scoped_var`. `find_collection_source_with_path` is preferred — it's the proven idiom for this exact "where does this `.get()` borrow from" question and already carries the non-Copy discipline.)

## ⚠ FALSE-POSITIVE BOUNDARY — the crux (a too-strict rule false-positives 145 fixtures)
The fix MUST fire ONLY for the genuinely-dangling case. The boundary (review-delimited):
- **`arena_depth == 0` → check is entirely skipped** (the outer `if self.arena_depth > 0` at `:416`/`:643`). This alone excludes ~all of the **145 `.get()`-binding corpus fixtures** (arena usage is rare) and the **91 passing security tests**.
- **Inner (arena-scoped) target → skipped** (existing `!arena_scoped_vars.contains(target)` for AssignOuter; for Return, the value escapes by definition).
- **Copy element → skipped** (the `elem_is_resource`/`is_copy_type` gate). `int peek = arenaVec.get(0).unwrap()` MUST stay accepted.
- **Non-arena source → skipped** (`root ∉ arena_scoped_vars`). Every normal heap-owned `.get()` binding (CoW-materialized, safe) stays accepted.
So the rule fires only on: arena_depth>0 ∧ borrow-from-arena-scoped-source ∧ non-Copy ∧ outer-escape. Verify NO regression on: the 91 passing security tests, the `int`-arena control (accept), the non-arena `.get()`-into-`Option`/`&` fixtures (accept), attack_03/13/14/17/18 (all `security_safe`, must stay accepted).

## Fixture (per "Don't redesign around compiler gaps" — the test must guard a genuinely-unsafe program)
`attack_19_field_borrow_escape.gg`'s literal body is BENIGN (a plain heap struct-move, CoW-safe — ASan-clean). It under-demonstrates the class. STRENGTHEN it (or add `attack_NN_arena_borrow_escape.gg` + wire a `sec_NN` test): make the fixture the arena-escape shape above, so `security_rejected` guards a program that is genuinely a UAF and that the FIXED `gg check` now rejects. Confirm: pre-fix `gg check` accepts it (the bug); post-fix `gg check` rejects it (sec_19 passes). Keep/adjust the fixture comment to describe the real escape.

## Gate (executor runs targeted; parent runs full corpus + full security suite at integrate)
- `cargo test --lib` (the `src/semantic/safety/` unit tests in `tests.rs` — esp. the origin/arena tests; add a unit test for the new arena-borrow-escape rejection if the harness supports it).
- `cargo test --test security` → ALL pass. Baseline is **92 passed, 1 failed (sec_19)**; post-fix sec_19 flips to PASS (the strengthened fixture is now rejected by `gg check`) and the other 92 stay green. **This is the load-bearing gate.**
- FULL integration corpus (`cargo test --test integration`) BOTH backends → NO new failures (a false-positive rejection would break a `.get()`-binding fixture's build). This is the false-positive net.
- ASan: the strengthened fixture is now REJECTED at `gg check` (no binary to run) — instead, ASan a POSITIVE control: a SAFE arena program that uses `.get()` into an arena-scoped INNER var (must still build + run clean, NOT falsely rejected).
- `self_host_bootstrap_fixed_point` (the self-host driver uses arenas — confirm no false-positive on its own arena `.get()` usage).

## Review notes (pass 1 — folded; N1/N2 are TODO deferrals the executor must REPORT for the parent to file)
- **N1 — `.clone()` is NOT a safe remediation (separate, deeper bug — do NOT imply it fixes this; REPORT for TODO).** RUN-confirmed `peek = v.get(0).unwrap().clone()` is STILL a heap-use-after-free: `gorget_string_clone_to_owned` clones into `__gorget_current_alloc`, which inside the `with`-scope IS the arena → the "owned" copy is arena-allocated and freed at scope exit. The new rule does NOT fire on the `.clone()` form (`find_collection_source_with_path` returns `None` for a `.clone()` outer call). So the natural user response to the new error ("add `.clone()`") yields a program `gg check` ACCEPTS that still UAFs. The honest remediation in the diagnostic/docs is "move the binding inside the scope / don't escape arena-backed non-Copy data," NOT clone. The arena-clone-binds-wrong-allocator bug is a deeper separate soundness gap → TODO.
- **N2 — `push`-to-outer (and any consume position) sibling is OUT OF SCOPE this round; defer to TODO ("fix the class").** `outer.push(v.get(0).unwrap())` is ALSO a real UAF (RUN-confirmed) and is NOT caught by the AssignOuter+Return fix (a method-call arg is neither). Keep this round narrow (AssignOuter + Return) but REPORT the general "borrowed-arena-elem at any consume position (push/put/send/ctor/...)" sibling for a TODO entry.
- **N3 — precision (no action this round):** `find_collection_source_with_path` matches `get`/`first`/`last`/`unwrap`/`expect` by NAME, so a user type with a `get()` method whose receiver is arena-scoped + non-Copy would also fire. In practice those are ALSO genuinely unsafe (the method runs under the arena allocator), so the rule errs toward safety — acceptable. Just don't be surprised by the diagnostic on a user `get()`.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/semantic/safety/check_stmt.rs` (+ `src/semantic/errors.rs` if a new `ArenaEscapeKind` variant), the strengthened/new `tests/fixtures/security/attack_*.gg`, and `tests/security.rs` (the test wiring). No `git add -a`. Do NOT touch `TODO.md`/`DONE.md`/`MEMORY.md`.
