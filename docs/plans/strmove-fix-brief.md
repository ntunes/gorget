# EXECUTOR BRIEF: `String !p` move-param concat fix (conformance 186→187 both backends)

> **STATUS: v1 DRAFT — review passes: (none yet; ≥3 sequential fresh passes required before launch).**
> Scout: full report + measured prototype at `/tmp/recover_strmove/` (FINDINGS.md,
> proto_fix_FINAL.patch, operators.rs.fixed, spec_conf logs). Scout ran the fix end-to-end on
> BOTH backends: 24-shape matrix correct, 12-case ASan clean, no clone regression,
> conformance lanes C 186→187 / LLVM 186→187 / self-host 187 unchanged.

## The bug (verified on main this session)

`String f(String !p): return p + "log"` — `gg check` ACCEPTS; both backends emit invalid
code (C: `(void*)a + (void*)b` → cc "cannot convert to a pointer type"; LLVM: `add ptr` →
llc "invalid operand type"). Boundary (scout-measured): exactly a **`!`-move String param
read directly as a binop operand in a FREE FUNCTION**. Move-param on the RIGHT of `+`,
`==`, `.len()`, f-strings, rebind-then-concat, `&`-borrow params, moved Vectors, and the
same shape inside a method all already work. Conformance seed: `spectests/run/
smith_move_param_concat.gg` (ggdef-adjudicated `ablog`) — the ONE non-MATCH in the C/LLVM
lanes today.

## Root cause (write site) — and the fix the scout ratified

A `!`-move resource param is passed `ByMutPtr` (slot `MutPtr(String)`). The identifier
read auto-derefs only when `is_param_borrow_unique` — but a `!`-move String param carries
`ownership=Owned` (`functions.rs:930-936` `set_owned`, String-specific + free-function-
only), so the read yields the raw `MutPtr(String)`. The GIR binop `is_string` check
(`src/ir/lowering/exprs/operators.rs:68`) compares types EXACTLY (no pointer unwrap) →
`is_string=false` → integer `BinOp::Add` → pointer arithmetic.

**The fix**: extend `cow_deref_if_ptr` (the binop operand-shaping step in
`src/ir/lowering/exprs/operators.rs`) to also `LoadRef`-deref an `is_owning_param`
MutPtr slot (today it derefs only `is_ref_local` CoW aliases). The deref yields a `Str`
value → the existing `is_string` check goes true → the existing `gorget_str_cat` path
fires. `LoadRef` is a shallow borrow-read (no zero, no drop-registration), so `*p` stays
`Owned` and drops exactly once at exit. This matches the SELF-HOST ORACLE's shape
(`lower_expr.gg:2267` + `is_string_type_id` at `lower_types.gg:2744-2757` recursively
unwraps Ptr/MutPtr at the consume site, param ownership untouched).

**FORBIDDEN alternative (measured, do not take):** removing the `set_owned` so the param
becomes `Borrowed` and auto-derefs. The scout prototyped it: fixes concat but REGRESSES a
leak — moved-String `.push()` is ASan-clean pre-fix but leaks 6 bytes through the
shared-mutation Borrowed path (the same path behind the filed `String &p` push leak,
TODO Medium). Keep `Owned`; shape at the binop.

Reference patch: `/tmp/recover_strmove/proto_fix_FINAL.patch` (+27/−3, one file).
**Re-derive it against current main — do not blind-apply**; verify each hunk's context
still holds, and understand the `LoadRef` emission you're adding.

## Deliverables (ONE commit)

1. The operators.rs fix (above).
2. **Conformance ratchet, same commit** (per tests/spec_conformance.rs's own module doc):
   `C_MATCH_FLOOR` 186→187, `LLVM_MATCH_FLOOR` 186→187 (`tests/spec_conformance.rs:63-64`).
3. **Retire the known-gap wiring**: remove the `#[ignore]` on the `move_param_concat`
   integration test (`tests/integration.rs:~5043` — locate by name, line may drift) and
   PROMOTE `tests/fixtures/known_gaps/move_param_concat.gg` into `tests/fixtures/` proper
   (the cow_dead_branch_alias_bind precedent — a fixed gap does not stay in known_gaps/).
4. **Three sibling fixtures** (values scout-verified, ASan-clean): two-move concat →
   `abcd`; self-concat `p + p` → `abab`; chained `p + "a" + "b"` → `Xab`. Wire as run_gg
   pairs; also run each under GG_BACKEND=llvm locally.

## Zone

`src/ir/lowering/exprs/operators.rs` + `tests/spec_conformance.rs` (floors only) +
`tests/integration.rs` (un-ignore + 3 new pairs) + `tests/fixtures/` (promote + 3 new).
**Do NOT touch the self-host (already reference-correct), spec/ggdef, spectests/, or
TODO.md/DONE.md (parent-only).**

## Gates (foreground, tee to /tmp/strmove_*)

- `cargo build` · `cargo test --lib` (1105/0).
- The scout's core slice on BOTH backends: concat / move_param / cow_lazy / leak_string /
  fstring / drop_move / string_owned (~92 tests; `GG_BACKEND=llvm` for the second run).
- `GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1
  --nocapture` → expect **C=187 · LLVM=187 · self-host=187**, floors passing at 187.
- ASan spot: the repro + `move_push` + `two_move` shapes (compile emitted C with
  `-fsanitize=address`; all must be clean).
- `--clones=stats` on the ctor-move probe: `string_clone=1`, identical to pre-fix (no
  clone regression — CLAUDE.md perf rule: memory is measured, not assumed).
- Parent-driven after merge (NOT yours): full integration sweeps both backends +
  `self_host_bootstrap_fixed_point` (no self-host source touched, but Core #7 gates on
  the bootstrap regardless).

## Non-goals

- The `String &p` push-grown-buffer leak (filed Medium — orthogonal; do not attempt here).
- Any `set_owned` / ownership-model redesign.
- ggdef/spectests changes (the seed already pins `ablog` and will simply MATCH).
