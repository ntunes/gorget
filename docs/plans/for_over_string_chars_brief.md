# for-over-String-chars — executor brief (self-host `lower_for`)

> Discrete FIDELITY win (scout a16239e0, RUN-verified). SINGLE-FILE edit (`lower.gg`).
> Oracle: `src/ir/lowering/stmts/for_loops.rs:237-377` (`lower_for_string`). Re-verify against CURRENT source before editing.

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
    emit(&ctx, GICallExtern(len_local, "gorget_str_len", [<coll operand>]))   # ⚠ BYTE length — see note
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

### Load-bearing details (RUN-VERIFY each before/while implementing)
- **`<coll operand>` shape:** the codepoint externs + `gorget_str_len` take `Str` **BY VALUE** (self-host defs `lir_codegen.gg:6230-6233` are `(Str s, ...)`, NOT `Str*`); they are NOT in the by-pointer list (`lir_lower.gg:1908-1990`), so the existing arg coercion auto-derefs a Ptr-source `coll_local` as `*(Str*)` (RUN-verified: `slen(String input): input.len()` emits `gorget_str_codepoint_count(*(Str*)__v0)`). **Mirror the operand shape of an EXISTING String `.len()`/`.codepoint_count()` lowering** (find it in the self-host method-lowering) rather than guessing OpBorrow-vs-OpCopy — match what works.
- **`gorget_str_len` MUST be BYTE length, NOT codepoint count.** `gorget_str_codepoint_count` is the codepoint count — do NOT use it for the bound (would under-count, terminating early on multibyte input). Confirm `gorget_str_len` returns byte length (scout says yes; the `len` field of the 32-byte `Str` struct is the byte length). Alternative if uncertain: terminate the header on `cplen != 0` instead of `byte_pos < len` (the codepoint extern returns 0 past end — but that needs cplen in the header; the byte-length bound is simpler/Rust-faithful).
- **`ch` is a cap=0 VIEW into `coll`'s buffer** (`gorget_str_codepoint_at` returns a non-owning region) → bind LoBorrowed/`BoCollectionElement(coll_local)` (like the vector for-element at `:9464-9470`); its drop is a no-op; body consume/mutate sites clone-on-demand. Do NOT register it as an owned drop.
- **`gorget_utf8_codepoint_len_at` + `gorget_str_codepoint_at` are ALREADY C-emit-defined** (`lir_codegen.gg:6230-6233`, return-type registration `:2081`) — NO new runtime, NO new C-emit case, NO `CkString` enum variant. Only the GIR-lowering arm is missing.

## RUN-verified impact + honest estimate
+3 confident (`test_for_loops`, `drop_reassign_after_move` [the TODO-cited blocker — its tokenize loop], `string_replace_complex` [ROT13 loop]), +1 possible (`async_for_loop_collections`, if async composes). NOT a tokenizer avalanche — other String fixtures are gated behind SEPARATE String-builtin-C-signature bugs (`gorget_str_is_empty` void* arg, `gorget_str_index_of` return-type, char-method mangling — log as a cluster, NOT this chain). `string_indexing`/`str_codepoint_index` improve but don't fully flip (separate `s[i]` indexing bug).

## Risks
- **Codepoint not byte** (the one correctness trap) — stride by `cplen`.
- **Driver does NOT use `for c in <String>`** (verified across all self_host dirs — the lexer uses index-based `char_at`/`byte_at`) → `bootstrap_fixed_point` SAFE (driver output unchanged). Run it to confirm anyway.
- Ptr-source param (`drop_reassign_after_move`'s `for ch in input`, `input: String` → `GtPtr`) — handled by the by-value arg coercion; confirm the codepoint externs are NOT wrongly in the by-pointer list.

## Snapshot
After the fix, RUN `test_for_loops` + `drop_reassign_after_move` + `string_replace_complex`; for each that MATCHes the oracle, add `tests/fixtures/runtime_snapshots/<stem>.out` (exact `cargo run -q -- run … | od -c` bytes).

## Gates (GG_BUILD_TIMEOUT_SECS=600)
1. `rm -f tests/fixtures/self_host_lowerer/driver{,.c}`
2. `cargo build` + `cargo test --lib` (1072/0).
3. `self_host_bootstrap_fixed_point` GREEN.
4. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → expect **412/940** (+3); report the PARITY line.
5. `self_host_runtime` (+ new snapshots, 0 regressed).
6. `lowerer_comparison` (971) + `c_emit_comparison` (902) — expect ≥ (the new arm + `lower_for_string` add fns; if Rust emits the same for-string lowering, fn-count should track — report any delta).
7. A codepoint-semantics RUN: `for c in "aéb"` must yield 3 iterations `a`/`é`/`b` (NOT 4 bytes). Verify against the oracle.
(PARENT runs full `cargo test --test integration`.)

## Worktree discipline
- `pwd` + `git rev-parse --show-toplevel` FIRST; inside YOUR worktree. `git merge --ff-only gorget-1` FIRST.
- Stage ONLY: `git add tests/fixtures/self_host_lowerer/lower.gg tests/fixtures/runtime_snapshots/*.out docs/plans/for_over_string_chars_brief.md` — NEVER `git add -a`/`.`.
- Commit on your branch; do NOT merge to gorget-1.
