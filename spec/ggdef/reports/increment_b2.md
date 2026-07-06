# Increment B2 report — equip, Drop, D4 rejections, the full phase-0 corpus

**Scope (RFC §6 phase-0 acceptance).** Extend the B1 elaborator+evaluator with
the equip surface and close out phase 0: pass-1 function/method **signature
registry** (serves named-arg call reorder + receiver-type inference); `equip`
method dispatch with self-mode handling (D2: plain `self` = bare binding);
`equip T with Drop` with custom-drop **EXECUTION**; the D4 drop-taint rejections
at all six implicit-copy positions; `with expr as name:` as a scoped statement;
the F1 ratchet hardening; the F2 fresh-temp Move trace event; and the call-side
named-arg reorder (from B1 output-review R2). Gate = the **entire** corpus.

**Re-run every gate (all numbers below regenerate from these):**

```
cargo build --workspace
cargo test -p ggdef                                  # 58 lib + corpus_a + corpus_b1 + corpus_b
cargo test -p ggdef --test corpus_b -- --nocapture   # the table below (116 fixtures)
cargo test --test lints ggdef_import_ratchet         # incl. the F1 full-source hardening
cargo test --lib                                     # root package — unaffected
```

## Gate result (`spec/ggdef/tests/corpus_b.rs`)

Gate set = every `cow_*` / `deadwrite_*` fixture minus the four standing
exclusions (the 3 GENERIC-equip cow fixtures `cow_element_borrow_alias_mutate` /
`cow_p3_alias_chain_mutate` / `cow_p3_index_mutate`, excluded by name in B2 since
equip fixtures are otherwise IN the gate; and `deadwrite_ok_atomic_add`). The
harness discovers them dynamically and asserts the set is **116** fixtures.

| Category | Count | Meaning |
|---|---|---|
| **MATCH / run_gg** | 85 | ggdef stdout == the committed `run_gg("x.gg", …)` expectation |
| **MATCH / selfhost** | 2 | ggdef stdout == the committed self-host `assert_eq!` (the two EMove fixtures) |
| **REPORT-ONLY** | 29 | no committed stdout expectation exists — ggdef output recorded (see `deadwrite_spec_expectations.md`), gated only to reach a clean `Value` outcome |

**MATCH-gated total = 87/87 MATCH. Findings = 0. STOP-and-report = 0.**

### The 13 equip fixtures (all NEW to B2, all MATCH)

| Fixture | ggdef stdout | shape |
|---|---|---|
| cow_amp_method_arg_bare | `3` / `4` / `done` | `&`-arg to a plain-self method; bare alias copy grows, source untouched |
| cow_element_borrow_source_mutate_with | `alphalonglonglongstring` / `23` | eager `.get().unwrap()` snapshot survives a `with`-block source mutation; `Res with Drop` (fresh-temp move) |
| cow_index_proj_caller_untouched | `original`×3 / `done` | index-projected write / user-`&self`-method / replace through a bare param materialises the root |
| cow_index_proj_mut_writethrough | `VIA_ASSIGN` / `VIA_METHOD` / `done` | `&`-param write-through reaches the caller (materialize is a no-op) |
| cow_lazy_method_arg | `s = hello` / `v0 = mutated` | `&`-method-arg mutation; the pre-move snapshot `s` is preserved |
| cow_method_arg_same_coll | `8` / `B` / `2` / `A` / `done` | user `&self` method whose arg is an element of the same collection; caller untouched |
| cow_named_recv_gate_name_collision | `Y` / `A` | user `get(&self)` COLLIDES with builtin `.get()` — receiver-type inference dispatches the user method |
| cow_named_recv_gate_projected_name_collision | `Z` / `A` | same collision on a PROJECTED receiver `s.v[0]` (nested type resolution) |
| cow_named_recv_mutator | `Y` / `A` | named-receiver `&self` mutator on a bare param materialises |
| cow_named_recv_readonly | `A` / `A` | read-only `&self` chain — no write, no materialize |
| cow_named_recv_transitive_mutator | `Y` / `A` | mutation via a self-call (`bump` → `set_name`); materialises |
| cow_nested_projection | `2` / `orig` / `done` | nested index/field projections walk to the bare-param root and materialise |
| deadwrite_warn_user_method | `1` (REPORT-ONLY) | dead bare-param `&self` user method mutates the private copy; caller `hits.len`==1 |

### Divergence-table categories

- **expected-D2 (plain-`self` write-through):** **none in the corpus.** The corpus
  equip methods are `&self` (write-through opt-in) or plain-`self`-without-a-self-
  write (`take`/`poke_vec` mutate their `&`-args, not `self`). The D2 rule (a
  write through plain `self` MATERIALIZES) is exercised by the unit test
  `equip_plain_self_write_materializes_d2` and is implemented uniformly — a
  plain-`self` binding is a `BindMode::Borrow` view exactly like a bare param.
- **expected-D1 / EMove:** **none.** `cow_lazy_move_bind` / `cow_lazy_move_reassign`
  MATCH the committed `s = hello` / `w0 = mutated` (production's `Expr::Move`
  read-through bug was already fixed; ggdef's eager pre-mutation value agrees).
- **PRE-ADJUDICATED deadwrite deltas (3):** carried forward VERBATIM from
  `increment_b1.md`'s CORRECTION table — NOT re-derived. All REPORT-ONLY
  (stderr-wired), so they do not fail the MATCH gate; they are ggdef-correct per
  RFC §2.2 bare-param materialize (production bugs already filed in `TODO.md`):
  `deadwrite_warn_compound`→`10` (prod 11), `deadwrite_ok_loop_read_before_write`
  →`1,2,3,1` (prod 1,1,1,1), `deadwrite_ok_rebind`→`3,1` (prod CC-FAIL). Full
  table + governing rule in `deadwrite_spec_expectations.md`.
- **smith bugs:** the two smith adjudications (`9` / `ablog`) are Increment C.
- **STOP-and-report findings:** **none.**

## D4 rejections — six positions, six unit tests (one per position)

The D4 drop-taint rejection (`E_MoveWithoutOperator`) routes through ONE
centralized helper `reject_if_tainted_live_place`, called at all six
implicit-copy positions. Live-place sources of a drop-tainted type are rejected;
fresh temps move and are never rejected. Testing is ggdef unit tests (RFC §6:
production-side rejections + negative conformance fixtures are phase 1):

| Position | Unit test |
|---|---|
| 1. bind | `d4_position_1_bind` |
| 2. ctor / struct / enum field init | `d4_position_2_ctor_init` |
| 3. collection put | `d4_position_3_collection_put` |
| 4. return | `d4_position_4_return` |
| 5. closure capture | `d4_position_5_capture` |
| 6. materialize-on-write | `d4_position_6_materialize_on_write` |

Plus `d4_allows_fresh_temp_move_and_explicit_move` (the counterpart: a fresh temp
and an explicit `!` move are NOT rejected).

## What landed

- **`ggc.rs`**: `Stmt::With` (scoped resource bind — NOT an inlined `Bind`, for
  drop timing); `Program.drop_fns` (`(type, drop-fn-name)`, resolved to indices
  in `eval::Ctx`).
- **`eval.rs`**: `drop_scope` threads `Ctx` and returns `Result<(), Halt>`,
  popping locals one at a time and RUNNING each type's custom `Drop` via
  `run_custom_drop` (self moved in; killed before the drop body's own scope-exit
  so a value's custom drop never recurses on itself; a trapping/recursing/fuel-
  exhausting drop propagates). `Stmt::With` (fresh scope, resource drops last).
  F2: `emit_fresh_temp_move` emits a structural `Move` event for fresh-temp binds
  of droppable values.
- **`elaborate/mod.rs`**: the pass-1 **signature registry** (`fn_param_names` +
  `fn_ret`); a per-function **type + mode env** (`local_ty` / `local_mode`, the
  mode-carrying env D4 needs); **receiver-type inference** (`infer_ast_ty`, read-
  the-annotation + projection walk); `equip` collection (`register_equip`) +
  concrete method dispatch (`elaborate_user_method_call` / `self_source`, D2
  self-mode); `equip T with Drop` → `drop_fns`; the D4 transitive taint fixpoint
  (`compute_taint` / `ty_tainted`) + the ONE rejection helper at all six sites;
  `Stmt::With` desugar; `SelfExpr` → `self`; the call-side named-arg REORDER
  (`call_args_reordered`, replacing `reject_named_args` for ordinary calls;
  enum/collection ctors keep the rejection — sibling fixed at the `Type.Variant`
  method-call path too).
- **`tests.rs`**: 18 new B2 unit tests (equip dispatch + name-collision + D2;
  custom-drop side-effect / reverse-order / trapping / no-self-recurse; with-scope
  drop timing + fresh-temp resource; the six D4 positions + the fresh-temp
  counterpart; named-arg reorder both ways). F2 test updated to assert the `move`
  event.
- **`tests/lints.rs`**: F1 hardening — a SECOND scan over the FULL ggdef source
  for inline `gorget::(ir|semantic|lir|bir|backend)::` paths (the use-line-only
  scan is bypassable).
- **`tests/corpus_b.rs`** (new): the full-corpus gate (116 fixtures).
- **reports**: this file + `deadwrite_spec_expectations.md`.

## Phase-0 status

With B2 landed, phase-0 acceptance (a)+(b) is met: the cow_* family (minus the 3
generic-equip exclusions) and the deadwrite_* programs (minus atomic_add) all run
under ggdef with their ratified expectations. Acceptance (c) — the two smith
adjudications + the EMove witness from the definition — is **Increment C**.

## CORRECTION (B2 output-review, 2026-07-06 — discloses what the report above over-claimed)

1. **Custom-drop execution is TRANSITIVELY INCOMPLETE in B2.** `run_custom_drop` runs the
   type's OWN drop body only — it does NOT enumerate droppable FIELDS or COLLECTION ELEMENTS
   of the dropped value. Verified divergence from correct production: nested Drop types miss
   the inner drop; `Vector[R]` of a Drop type misses all element drops. RFC §2.2 makes drop
   count/order normative, so this is a PHASE-1 MUST (filed in TODO.md) — not exercised by the
   phase-0 corpus (the only Drop fixture has a scalar field + `pass` body), so the gate is
   legitimately green, but "custom-drop EXECUTION" above should read "top-level custom-drop
   execution."
2. **D4 position 6 user-method sibling closed post-review**: a user `&self` mutator through a
   tainted Borrow-rooted receiver is now rejected (orchestrator fix + 2 pinning tests). The
   plain-`self`-write case needs method-body write analysis — phase 1 (filed).
3. Position-5 (capture) uses an inline check, not the centralized helper — functionally
   correct; consolidation is a phase-1 tidy.
4. deadwrite_spec_expectations.md precision: the 24-row table is 21 match + 3 pre-adjudicated
   deltas (not "remaining 21 are match"). The 5 cow_* REPORT-ONLY outputs live in the
   corpus_b --nocapture table.
