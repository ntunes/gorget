# `deadwrite_*` spec expectations — ratified from `ggdef`

**Purpose (RFC §6(b), §4 "expectations flow FROM the definition").** The
`deadwrite_*` corpus programs are wired in `tests/integration.rs` via
`check_gg_warns` — they assert a production **stderr** diagnostic
(`DeadBareParamWrite`), never a **stdout** expectation. Their stdout is therefore
genuinely unextractable, so this file records `ggdef`'s stdout as the newly-
ratified spec expectation (for orchestrator/owner review), each annotated with
the D-decision that governs it. Expectations are **generated, not guessed**.

**Regenerate (never trust the cached values below):**

```
cargo test -p ggdef --test corpus_b -- --nocapture   # the REPORT-ONLY rows
```

All rows below are `ggdef`'s stdout for the program. The governing rule is the
same throughout: **a bare-parameter write MATERIALIZES a private copy (RFC §2.2 /
D2); the caller is untouched; the private copy drops in the callee** — exactly
the CoW rule stated as eager value semantics. `&`-param writes reach the caller;
`.get()`/`.pop()` bind an eager owned snapshot.

| Program | ggdef stdout | Governing rule | vs production |
|---|---|---|---|
| deadwrite_ok_read_only | `5` | pure read, no write | match |
| deadwrite_ok_mut_param | `2` | `&`-param write reaches the caller | match |
| deadwrite_ok_copy_struct | `1` | bare-param field write materialises → caller untouched | match |
| deadwrite_ok_match_scrutinee | `seven` / `1` | write materialises; match reads the private copy; caller untouched | match |
| deadwrite_ok_value_pop | `2` / `2` | `pop()` on a bare param materialises; caller `len`==2 | match |
| deadwrite_ok_while_drain | `4` | drain materialises a private copy; caller `len`==4 | match |
| deadwrite_ok_scratch_read | `99` / `1` | write-then-read the private copy; caller untouched | match |
| deadwrite_ok_underscore | `1` | `_`-param write materialises; caller untouched | match |
| deadwrite_ok_fstring_read | `len=2` / `1` | read via f-string of the private copy; caller untouched | match |
| deadwrite_ok_branch_sibling_read | `1` | write in one branch, read a sibling; caller untouched | match |
| deadwrite_build_lock | `1` | bare-param write materialises; caller untouched | match |
| deadwrite_warn_push | `1` | dead bare-param push materialises; caller `len`==1 | match |
| deadwrite_warn_index_assign | `1` | dead bare-param index write; caller untouched | match |
| deadwrite_warn_field_assign | `1` | dead bare-param field write; caller untouched | match |
| deadwrite_warn_nested_field | `0` | dead bare-param nested-field write; caller untouched | match |
| deadwrite_warn_loop_write | `1` | dead bare-param loop push; caller untouched | match |
| deadwrite_warn_early_return | `1` / `1` | dead bare-param write before return; caller untouched | match |
| deadwrite_warn_chained_stmt | `2` | dead bare-param `pop().unwrap()`; caller `len`==2 | match |
| deadwrite_warn_branch_read_then_write | `3` / `3` | read then dead write; caller untouched | match |
| deadwrite_warn_string_push | `hi` | dead bare-`String`-param `push`; caller unchanged | match |
| deadwrite_warn_user_method | `1` | dead bare-param `&self` user method mutates the private copy; caller `hits.len`==1 | match |
| **deadwrite_warn_compound** | **`10`** | bare-param `xs[0] += 1` materialises (compound-assign is a write) → caller untouched | **DELTA — prod=11 (pre-adjudicated)** |
| **deadwrite_ok_loop_read_before_write** | **`1` / `2` / `3` / `1`** | the private copy PERSISTS across loop iterations; caller `len`==1 | **DELTA — prod=1,1,1,1 (pre-adjudicated)** |
| **deadwrite_ok_rebind** | **`3` / `1`** | bare-param full-rebind materialises; caller untouched | **DELTA — prod CC-FAIL (pre-adjudicated)** |

## The three PRE-ADJUDICATED deltas (production bugs, already filed)

Carried forward VERBATIM from `increment_b1.md`'s CORRECTION table — these are
**not re-derived this increment** (the brief pins them as pre-adjudicated). Each
is a production bug that `ggdef` gets right per RFC §2.2's bare-param materialize
rule; each is filed in `TODO.md`:

| Fixture | production | ggdef (correct) | production defect |
|---|---|---|---|
| deadwrite_warn_compound | 11 | 10 | bare-param `xs[0] += 1` WRITES THROUGH (compound-assign bypasses materialize) |
| deadwrite_ok_loop_read_before_write | 1,1,1,1 | 1,2,3,1 | materialize does not persist across loop iterations |
| deadwrite_ok_rebind | CC-FAIL | 3,1 | bare-param full-rebind emits invalid C (latent; the fixture is stderr-only) |

The remaining 21 `deadwrite_*` programs are `match` (ggdef stdout agrees with
production stdout, per the B1 output-review's both-compiler run). These 24 stdout
values become the ratified spec expectations for the `deadwrite_*` family, to be
carried into `spectests/` frontmatter at migration (phase 1) under
`adjudicator: ggdef`.
