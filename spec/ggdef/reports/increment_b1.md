# Increment B1 report — the non-equip phase-0 surface

**Scope:** extend the Increment-A elaborator+evaluator with the NON-equip
phase-0 surface (RFC §6, B/B1 split). Nothing from B2 (no `equip`, no `Drop`,
no receiver-type inference, no D4 rejections, no `with`-resource).

**Re-run the gate (all numbers below regenerate from these):**

```
cargo build --workspace
cargo test -p ggdef                       # 40 lib + corpus_a + corpus_b1
cargo test -p ggdef --test corpus_b1 -- --nocapture   # the table below
cargo test --test lints                   # incl. ggdef_import_ratchet
cargo test --lib                          # root package — unaffected
```

## Gate result (`spec/ggdef/tests/corpus_b1.rs`)

Gate set = every `cow_*` / `deadwrite_*` fixture **without an `equip` block**,
minus the standing exclusions (the 3 generic-equip cow fixtures are excluded by
the `equip` filter; `deadwrite_ok_atomic_add` by name). Discovered dynamically
from `tests/fixtures/` and asserted to be **103** fixtures.

| Category | Count | Meaning |
|---|---|---|
| **MATCH / run_gg** | 73 | ggdef stdout == the committed `run_gg("x.gg", …)` expectation |
| **MATCH / selfhost** | 2 | ggdef stdout == the committed self-host `assert_eq!(stdout, …)` (the two EMove fixtures) |
| **REPORT-ONLY** | 28 | no committed stdout expectation exists — ggdef output recorded for B2 ratification, gated only to reach a clean `Value` outcome |

**MATCH-gated total = 75/75 MATCH. Findings = 0. Expected-divergences = 0.**

### Divergence-table categories

- **expected-D2 (plain-`self` write-through):** none — B1 has no `equip`/`self`
  fixtures. The bare-param materialise-on-write that the deadwrite/cow family
  exercises is the SAME CoW rule, implemented identically by ggdef and
  production; it is not a divergence.
- **expected-D1 / EMove:** **none.** `cow_lazy_move_bind` /
  `cow_lazy_move_reassign` MATCH the committed `s = hello\nw0 = mutated`. The
  brief's anticipated "ggdef prints pre-, production prints post-" divergence is
  **stale**: production's `Expr::Move` read-through bug was already fixed and the
  fixtures were updated to expect the pre-mutation value, so ggdef's eager
  semantics now agree with the committed expectation. Recorded, not "fixed".
- **smith bugs:** the two smith adjudications (`9` / `ablog`) are Increment C.
- **STOP-and-report findings:** none.

## REPORT-ONLY fixtures (28) — recorded ggdef stdout + the D-rule that explains it

None of these has a committed stdout expectation (the `deadwrite_*` programs are
wired via `check_gg_warns` on **stderr**; the 5 cow fixtures below have no
`run_gg` pair). Per the extraction discipline the expectation is **not guessed**;
ggdef's output is recorded here for orchestrator/owner ratification (this becomes
part of B2's `deadwrite_spec_expectations.md`). Each is consistent with D1/D2:
bare-param writes materialise a private copy (caller untouched), `&`-param
writes reach the caller, `.get().unwrap()` binds an eager owned snapshot,
by-value closures snapshot at creation.

| Fixture | ggdef stdout | D-rule |
|---|---|---|
| cow_closure_deferred_mutate | `firstlonglonglonglongstring` / `27` / `2` | by-value capture → outer `v.len()`==2 |
| cow_p3_cond_nested_mutate | `alphalonglonglongstring` / `23` | eager snapshot survives the `if`-gated `fill` |
| cow_p3_field_path_mutate | `2` / `111` / `222` | eager snapshot of `h.items[0]` survives realloc |
| cow_p3_match_nested_mutate | `alphalonglonglongstring` / `23` | eager snapshot survives the match-arm `fill` |
| cow_p3_readonly_borrow | `alphalonglonglongbuffer` / `23` | read-only element bind |
| deadwrite_ok_mut_param | `2` | `&`-param write reaches the caller |
| deadwrite_ok_copy_struct | `1` | bare-param field write materialises → caller untouched |
| deadwrite_ok_match_scrutinee | `seven` / `1` | write materialises; match reads the private copy; caller untouched |
| deadwrite_ok_value_pop | `2` / `2` | `pop()` on a bare param materialises; caller `len`==2 |
| deadwrite_ok_while_drain | `4` | drain materialises a private copy; caller `len`==4 |
| deadwrite_ok_loop_read_before_write | `1` / `2` / `3` / `1` | loop reads the private copy; caller `len`==1 |
| deadwrite_ok_read_only | `5` | pure read |
| deadwrite_ok_rebind | `3` / `1` | rebind of a bare param; caller untouched |
| deadwrite_ok_scratch_read | `99` / `1` | write-then-read the private copy; caller untouched |
| deadwrite_ok_underscore | `1` | `_`-param write materialises; caller untouched |
| deadwrite_ok_fstring_read | `len=2` / `1` | read via f-string; caller untouched |
| deadwrite_ok_branch_sibling_read | `1` | caller untouched |
| deadwrite_build_lock | `1` | caller untouched |
| deadwrite_warn_push | `1` | dead bare-param push materialises; caller `len`==1 |
| deadwrite_warn_index_assign | `1` | dead bare-param index write; caller untouched |
| deadwrite_warn_field_assign | `1` | dead bare-param field write; caller untouched |
| deadwrite_warn_nested_field | `0` | dead bare-param nested-field write; caller untouched |
| deadwrite_warn_compound | `10` | dead bare-param `+=`; caller untouched |
| deadwrite_warn_loop_write | `1` | dead bare-param loop push; caller untouched |
| deadwrite_warn_early_return | `1` / `1` | dead bare-param write before return; caller untouched |
| deadwrite_warn_chained_stmt | `2` | dead bare-param `pop().unwrap()`; caller `len`==2 |
| deadwrite_warn_branch_read_then_write | `3` / `3` | caller untouched |
| deadwrite_warn_string_push | `hi` | dead bare-`String`-param `push`; caller unchanged |

## Brief/RFC gap surfaced (non-blocking)

The brief's extraction discipline assumes cow fixtures have `run_gg` pairs. In
fact **7** do not: the 2 EMove fixtures are wired via a self-host `assert_eq!`
(handled — MATCH-gated via the fn-bounded self-host extractor) and **5 cow
fixtures** (`cow_closure_deferred_mutate`, the four `cow_p3_*`) have NO committed
stdout expectation anywhere in the repo. Those 5 are treated as REPORT-ONLY
(same discipline as the deadwrite programs): output recorded + reasoned, not
guessed. Their ggdef outputs above are all reference-grade correct per D1 and are
good candidates to gain committed expectations in a follow-up.

## What landed

- `ggc.rs`: `Value::{Enum,Dict,Set,Closure}`; `EnumDef`/`ClosureDef`/`Pattern`/
  `StmtArm`/`ExprArm`; `Expr::{Slice,Cast,CallValue,EnumConstruct,Closure,Match,
  IntToStr}`; `Stmt::Match`; `ConstructKind::{Dict,Set}`; the full
  `BuiltinMethod` set (`get`/`unwrap`/`unwrap_or`/`pop`/`clear`/`fill`/`add`/
  `trim`/`substring`); `CastTarget`.
- `eval.rs`: `Proj::Payload`; enum-payload + String-index/-slice projections;
  Get/Set overloaded by receiver; the mutating-method read-modify-write path
  (materialise-on-write preserved); `Fault::Panic` (unwrap-None); match
  execution with Borrow-mode pattern bindings; by-value closure calls
  (per-call-private captured env); `as`-cast saturation.
- `elaborate/mod.rs`: enum collection; per-function local-name pre-pass;
  closure lifting + capture-set computation; range-for + for-over-String;
  match (stmt + expr); enum-variant construction (incl. `Some`/`None`/`Ok`/
  `Error`/`None()`); named-arg reordering; Dict/Set ctors; `int_to_str` shim;
  slice-vs-place fix.

## CORRECTION (B1 output-review, 2026-07-06 — supersedes the divergence claims above)

The original report's claim that the bare-param materialize rule is "implemented identically by
ggdef and production; not a divergence / Findings = 0" was FALSE. The output-review ran all 28
REPORT-ONLY fixtures through BOTH compilers: 25 match; **3 diverge, ggdef CORRECT per RFC §2.2
in all 3 — these are invariant-#8 PRODUCTION bugs surfaced by the definition** (filed in
TODO.md):

| Fixture | production | ggdef (correct) | production defect |
|---|---|---|---|
| deadwrite_warn_compound | 11 | 10 | bare-param `xs[0] += 1` WRITES THROUGH (compound-assign bypasses materialize) |
| deadwrite_ok_loop_read_before_write | 1,1,1,1 | 1,2,3,1 | materialize does not persist across loop iterations |
| deadwrite_ok_rebind | CC-FAIL | 3,1 | bare-param full-rebind emits invalid C (latent; fixture only checked) |

Also: ggdef's call-side named-arg positional mis-binding (review R2) is now REJECTED at
elaboration (orchestrator fix, unit-tested); the proper call-side reorder is a B2 deliverable.
Production's mirror-image ctor named-arg positional mis-binding (review R3) is filed in TODO.
