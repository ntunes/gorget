# for-over-String-chars — executor brief (self-host `lower_for`)

> Discrete FIDELITY win (scout a16239e0; review pass 2 RUN-verified the CORRECTED version → **PARITY 409→413/941 = +4**, both canaries GREEN, lowerer 972 / c_emit 903). Flipped set (MEASURED): `test_for_loops`, `drop_reassign_after_move`, `string_replace_complex`, `string_struct_complex` (⚠ NOT `async_for_loop_collections` — pass 1 mis-claimed it; it has a SEPARATE Dict-for-loop gap, logged to TODO). TWO-FILE edit: `lower.gg` + `lir_codegen.gg` (the Ptr-source-deref registrations — see Part 2). Oracle: `src/ir/lowering/stmts/for_loops.rs:237-377` (`lower_for_string`) — ⚠ but the oracle's `ch`-drop-registration comment is STALE (the runtime now returns a cap=0 VIEW; see the `ch` note below). Re-verify against CURRENT source before editing.
> ⚠ Review pass 1 found 2 blocking defects in the original draft (now folded): the bound symbol was `gorget_str_len` (DOES NOT EXIST → link error) — use **`gorget_str_byte_len`**; and a Ptr-source `coll` does NOT auto-deref without two `lir_codegen.gg` registrations (Part 2). Both folded below.

## The gap (RUN-verified)
`lower_for` (`lower.gg:9320`) has a Range arm and a Vector/Deque arm, but NO String arm → a `GorgetString` iterable falls through to `lower_for_iterator` (`:9417`), which looks for `GorgetString__iter`/`__next`, finds neither, and hits the silent-drop `lower_fail` (`:9599`) — **the loop body is dropped entirely.** Repro: `String txt="abc"; int n=0; for c in txt: n=n+1; print(n)` → self-host `0`, Rust `3`.

## Oracle (Rust `for_loops.rs:237-377`, `lower_for_string`)
Iterates **UTF-8 CODEPOINTS, not bytes**: loop var is an owned `String` (one codepoint/iter); `byte_pos=0`; bound = the BYTE length; per-iter `cplen = gorget_utf8_codepoint_len_at(s, byte_pos)`, `ch = gorget_str_codepoint_at(s, byte_pos)`, `byte_pos += cplen`. ⚠ Stride by `cplen`, NOT 1 — a byte-per-iter loop would split a multibyte `é`.

## The fix — add a String arm + `lower_for_string`

### (1) Dispatch (in `lower_for`, after the `if coll_is_vec_like:` return at `:9405`, before `if probe_is_enumerate:` at `:9406`):
```gorget
    if coll_is_vec_like:
        lower_for_vector(...)
        return
    if coll_tn == "GorgetString":
        lower_for_string(&ctx, pat, coll_local, body, &gmod, is_main)
        return
```
(`coll_tn == "GorgetString"` also covers a Ptr-to-String param — `type_id_to_name(GtPtr(GorgetString))` collapses to `"GorgetString"`, `lower.gg:877-879`.)

### (2) `lower_for_string` — mirror `lower_for_vector` (`:9422`) / `lower_for_range` (`:9510`) scaffold (header/body/incr/exit blocks, loop_continue/break stacks, per-iteration `push_drop_scope`/`pop_drop_scope`), with these specifics:
```gorget
void lower_for_string(LowerCtx &ctx, SpannedPattern pat, int coll_local, Vector[Stmt] body, GirModule &gmod, bool is_main):
    int gstr_tid = lookup_or_register_named(&gmod, "GorgetString")   # loop-var type
    # Iteration state: byte_pos counter + BYTE-length bound.
    int bytepos_local = add_local(&ctx, I64_TYPE, NO_NAME)
    emit(&ctx, GIAssign(bytepos_local, OpConstI64(0)))
    int len_local = add_local(&ctx, I64_TYPE, NO_NAME)
    emit(&ctx, GICallExtern(len_local, "gorget_str_byte_len", [<coll operand>]))   # BYTE length (runtime_string.c:516; NOT gorget_str_len which doesn't exist, NOT gorget_str_codepoint_count which under-counts)
    # Blocks
    int header_bb = new_block(&ctx); int body_bb = new_block(&ctx)
    int incr_bb = new_block(&ctx);   int exit_bb = new_block(&ctx)
    set_terminator(&ctx, GTJump(header_bb))
    # Header: byte_pos < len ? body : exit
    switch_to(&ctx, header_bb)
    int cond_local = add_local(&ctx, BOOL_TYPE, NO_NAME)
    emit(&ctx, GICmp(cond_local, CMP_LT, I64_TYPE, OpCopy(bytepos_local), OpCopy(len_local)))
    set_terminator(&ctx, GTBranch(OpCopy(cond_local), body_bb, exit_bb))
    # Body: ch = codepoint_at(coll, byte_pos) — a cap=0 VIEW (no heap), bind as borrow-alias.
    ctx.loop_continue_stack.push(incr_bb); ctx.loop_break_stack.push(exit_bb)
    push_drop_scope(&ctx.drop_elab, DSK_LOOP)
    switch_to(&ctx, body_bb)
    int ch_local = add_local_with(&ctx, gstr_tid, NO_NAME, LoBorrowed(), BoCollectionElement(coll_local))
    emit(&ctx, GICallExtern(ch_local, "gorget_str_codepoint_at", [<coll operand>, OpCopy(bytepos_local)]))
    bind_for_local(&ctx, pat, ch_local, gstr_tid, &gmod)
    lower_stmts(&ctx, body, &gmod, is_main)
    # terminator handling EXACTLY like lower_for_vector :9484-9492 (GTNone → pop_drop_scope + GTJump(incr_bb); else pop_drop_scope_no_emit)
    ctx.loop_continue_stack.pop(); ctx.loop_break_stack.pop()
    # Incr: cplen = codepoint_len_at(coll, byte_pos) [byte_pos unchanged since body → same cplen, computed HERE to avoid cross-block liveness]; byte_pos += cplen.
    switch_to(&ctx, incr_bb)
    int cplen_local = add_local(&ctx, I64_TYPE, NO_NAME)
    emit(&ctx, GICallExtern(cplen_local, "gorget_utf8_codepoint_len_at", [<coll operand>, OpCopy(bytepos_local)]))
    int new_pos = add_local(&ctx, I64_TYPE, NO_NAME)
    emit(&ctx, GIBinOp(new_pos, OP_ADD, I64_TYPE, OpCopy(bytepos_local), OpCopy(cplen_local)))
    emit(&ctx, GIAssign(bytepos_local, OpCopy(new_pos)))
    set_terminator(&ctx, GTJump(header_bb))
    switch_to(&ctx, exit_bb)
```

### Load-bearing details (review-pass-1 corrected; RUN-VERIFY each)
- **Bound symbol = `gorget_str_byte_len`** (`runtime_string.c:516` `static inline size_t gorget_str_byte_len(Str s){return s.len;}`, already in `runtime_takes_str_by_value` `lir_codegen.gg:1889`). ⚠ `gorget_str_len` DOES NOT EXIST (link error — only stale dead-entries `lir_codegen.gg:1929,2028`). ⚠ `gorget_str_codepoint_count` is the WRONG symbol (under-counts on multibyte). `gorget_str_byte_len` is correct.
- **`<coll operand>` shape:** the codepoint externs + `gorget_str_byte_len` take `Str` **BY VALUE** (`lir_codegen.gg:6256-6258` `(Str s,...)`). The `*(Str*)` auto-deref for a Ptr-source `coll` is gated by `runtime_takes_str_by_value` (`:1886`) AND `runtime_arg_is_str` (`:1755`) — NOT the by-pointer list. Mirror the operand shape of an existing String `.len()` lowering.
- **⚠ Part 2 (`lir_codegen.gg`) — REQUIRED for Ptr-source params (e.g. `drop_reassign_after_move`'s `for ch in input` where `input: String` → `void*`). Without these the codepoint externs get a raw `void*` → `cc: incompatible type for argument 1`:**
  - (a) add `gorget_str_codepoint_at` + `gorget_utf8_codepoint_len_at` to **`runtime_takes_str_by_value`** (`lir_codegen.gg:1886`).
  - (b) add `gorget_utf8_codepoint_len_at` to **`runtime_arg_is_str`** arg_idx-0 (`:1757` — the existing `starts_with("gorget_str_")` prefix MISSES the `gorget_utf8_` name; `gorget_str_codepoint_at` is already caught by the prefix).
- **`ch` is a cap=0 VIEW** (`gorget_str_codepoint_at` → `gorget_str_view_region` `runtime_string.c:729`, cap=0, free is no-op; self-host emit `lir_codegen.gg:6257`) → bind LoBorrowed/`BoCollectionElement(coll_local)` (like the vector for-element `:9464-9470`); NO owned-drop registration. ⚠ The Rust oracle (`for_loops.rs:348-361`) drop-REGISTERS `ch` ("returns an owned Str… without this every iteration leaked") — that comment is STALE (runtime now returns a view); the cap=0-view design here is correct, do NOT copy the oracle's drop-register.
- **`gorget_utf8_codepoint_len_at` + `gorget_str_codepoint_at` are ALREADY C-emit-defined** (`lir_codegen.gg:6256-6258`, return-type `:2081`) — NO new runtime, NO new C-emit case, NO `CkString` enum variant.

## RUN-verified impact (review pass 2+3 MEASURED + CONFIRMED)
**PARITY 409→413/941 = +4** (pass 2 MEASURED, baseline-stashed `comm`, 0 regressions): `test_for_loops`, `drop_reassign_after_move` [the TODO-cited blocker], `string_replace_complex` [ROT13], `string_struct_complex` all flip to MATCH; `fixed_point` GREEN, `self_host_runtime` 0-regressed, `lowerer` 972, `c_emit` 903. ⚠ `async_for_loop_collections` does NOT flip (pass 1 mis-claimed it) — it has a SEPARATE Dict-for-loop gap (`for (k,v) in <Dict>` → `lower_fail`, drops the `300`); logged to TODO as its own item. RE-CONFIRM the parity number at executor time (denominator 941 per pass 2; the handover cited 940). Other String fixtures stay gated behind SEPARATE String-builtin-C-signature bugs (`gorget_str_is_empty` void* arg, `gorget_str_index_of` return-type, char-method mangling — log as a cluster, NOT this chain).

## Risks
- **Codepoint not byte** (the one correctness trap) — stride by `cplen`.
- **Driver does NOT use `for c in <String>`** (verified across all self_host dirs — the lexer uses index-based `char_at`/`byte_at`) → `bootstrap_fixed_point` SAFE. ⚠ `fixed_point` FLAKED once for the reviewer (the cargo build-race CLAUDE.md warns about — shared artifacts with `lowerer_comparison`); RERUN on a red, don't panic.
- Ptr-source param (`drop_reassign_after_move`, `input: String` → `void*`) — handled by Part 2's two `lir_codegen.gg` registrations (NOT auto-handled — that was the review's blocking finding).

## Snapshot
After the fix, RUN `test_for_loops` + `drop_reassign_after_move` + `string_replace_complex`; for each that MATCHes the oracle, add `tests/fixtures/runtime_snapshots/<stem>.out` (exact `cargo run -q -- run … | od -c` bytes).

## Gates (GG_BUILD_TIMEOUT_SECS=600)
1. `rm -f tests/fixtures/self_host_lowerer/driver{,.c}`
2. `cargo build` + `cargo test --lib` (1072/0).
3. `self_host_bootstrap_fixed_point` GREEN.
4. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → review measured **+4 → 413/941** (`test_for_loops`/`drop_reassign_after_move`/`string_replace_complex`/`string_struct_complex`); report the PARITY line + the flipped set.
5. `self_host_runtime` (+ new snapshots, 0 regressed).
6. `lowerer_comparison` (971) + `c_emit_comparison` (902) — report any delta.
7. A codepoint-semantics RUN: `for c in "aéb"` must yield 3 iterations `a`/`é`/`b` (NOT 4 bytes). Verify against the oracle.
(PARENT runs full `cargo test --test integration`.)

## Worktree discipline
- `pwd` + `git rev-parse --show-toplevel` FIRST; inside YOUR worktree. `git merge --ff-only gorget-1` FIRST.
- Stage ONLY: `git add tests/fixtures/self_host_lowerer/lower.gg tests/fixtures/self_host_lowerer/lir_codegen.gg tests/fixtures/runtime_snapshots/*.out docs/plans/for_over_string_chars_brief.md` — NEVER `git add -a`/`.`. (TWO source files: `lower.gg` + `lir_codegen.gg`.)
- Commit on your branch; do NOT merge to gorget-1.
