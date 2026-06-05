# ③(d)-RSS — register runtime-resource `.get()`-deref-clones for drop (self-host)

> The SAFE, MEASURED slice of the 8 GB self-compile leak. Design+prototype scout a62a214e (RUN-verified). SINGLE-FILE (`lower.gg`). Re-verify against CURRENT source before editing.
> ⚠ This REFUTES the prior "③(d) temp-drop registration / pending_temp_drops / 489 LIR call-arg temps" framing (RUN-disproven — those `OpClone` temps are consumed by `gorget_array_push` which takes ownership → freeing them double-frees). The real leak is ONE deliberate-leak guard; P1 (last-use disentangle) is RSS-NEGATIVE and is NOT part of this fix.

## The leak (RUN-verified root cause)
`lower.gg:7323` (in `lower_stmt`'s `SVarDecl` → `CloneAndMove` arm): `T x = coll.get(i).unwrap()` (the "Branch C-pre" deref-clone) **deep-clones the element** (`resolve_payload_clone_fn` → fresh independent heap) but **skips `register_local_for_drop`** when `source_was_ptr` — a documented leak ("AT THE COST OF LEAKING the deref-clone's heap … to dodge a double-PRINT"). Over a large compile (the self-host compiling itself) these deref-clones pile up: **string_free 2.6M vs Rust 93.8M, peak RSS 9.96 GB vs Rust 1.42 GB.**

Why the blanket-skip exists: blanket-REGISTERING the deref-clone (so it frees) makes a USER struct/enum drop fire TWICE — the oracle BORROWS the element at a read-only `.get()` (no drop), the self-host CLONES it (one drop); registering the clone's drop = a SECOND drop = a double-PRINT for a `Drop` impl with a `printf` (memory-SAFE / ASan-clean, but stdout-wrong, e.g. `drop_struct_collection_fields`).

## The fix (the safe slice — RUN-measured 9.96 → 6.28 GB, −37%)
A **runtime-resource** deref-clone (GorgetString / GorgetArray / Vector__/Dict__/Set__) has **NO observable drop** (its drop is a `gorget_*_free`, no `printf`), so registering it frees the buffer INVISIBLY — no double-print. So: register the deref-clone's drop iff the cloned type is a runtime resource; keep USER struct/enum ptr-clones on the leak-but-stdout-correct path.

At `lower.gg:7323`, replace:
```gorget
                    if not source_was_ptr:
                        register_local_for_drop(&ctx, var_local, owned_type, &gmod)
```
with (the prototype's exact shape — add the `else` branch; keep/extend the explanatory comment):
```gorget
                    if not source_was_ptr:
                        register_local_for_drop(&ctx, var_local, owned_type, &gmod)
                    else:
                        # ③(d)-RSS: a RUNTIME-resource deref-clone (String/Array/
                        # Vector/Dict/Set) has no observable drop, so registering it
                        # frees its fresh heap invisibly (the self-compile RSS leak).
                        # A USER struct/enum ptr-clone's drop may printf → registering
                        # double-PRINTS (it stays on the leak path until Branch C-pre
                        # BORROWS — the root-cause fix below).
                        String owned_tname = type_id_to_name(owned_type, &gmod)
                        bool is_runtime_resource = false
                        match resource_meta_for(&gmod, owned_tname):
                            case Some(_):
                                is_runtime_resource = true
                            case None:
                                pass
                        if is_runtime_resource:
                            register_local_for_drop(&ctx, var_local, owned_type, &gmod)
```
(Reference patch: scout worktree `agent-a62a214e85fc4f5d7`, `/tmp/p3-FINAL-lower.patch`.)

### Why it's correct + safe (RUN-verified by the scout)
- **Runtime-resource drops are stdout-invisible** → registering the clone's drop frees the buffer without any observable side effect → parity-neutral (no double-print). Verified: `self_host_runtime` 413/413, 0 regressed; `self_host_runtime_diff` 413/941 unchanged.
  - ⚠ **LIMITATION (review pass 1, currently unexercised):** the gate is precisely safe for *runtime-resources-of-NON-observable-elements*. A `Vector[Container]`/`Dict[_, Container]` whose ELEMENT is a USER-Drop struct WITH a `printf` would still double-PRINT (`gorget_array_free` calls `elem_drop` per element, `runtime_array.c:250-252`). Corpus grep (review pass 1): the ONLY collection-typed deref-clones are `Vector[int]`/`Vector[String]` (`dict_get_unwrap_push_chain`/`drop_dict_loop`/`drop_struct_collection_fields:72`/etc.) — all primitive/String elements (`elem_drop` NULL or `gorget_string_free`, no printf) → SAFE today. **No `Vector[user-Drop]`/`Dict[_, user-Drop]` deref-clone exists in the corpus.** The Branch-C-pre-BORROW residual TODO is the real backstop; a future collection-of-user-Drop deref-clone would double-print until then (an executor adding such a fixture must check this).
- **No double-free** — the deref-clone is a FRESH INDEPENDENT heap (deep clone), so dropping it is a single, owned free, NOT a double-free of the source. ASan-clean on `drop_struct_collection_fields`/`drop_struct_fields`/`dict_get_unwrap_push_chain`/`cow_collection_element_mutate`/`drop_dict_loop`.
- **`bootstrap_fixed_point` GREEN** (the driver self-emits these deref-clones; re-converges).
- User struct/enum ptr-clones (Container, SpannedExpr, GirType, LirInst — `resource_meta_for` None) stay UNregistered → no double-print → their ~4.9 GB residual leak persists (the deferred Branch-C-pre-BORROW fix below).

## Scope / NOT in scope
- ONLY this `else` branch at `lower.gg:7323`. Do NOT ship the prior P1 (last-use disentangle, `liveness_compute_use_def`) — it's RSS-NEGATIVE (raised array_clone 18.8M→20.8M) and unneeded. Do NOT attempt the LIR call-arg `pending_temp_drops` (RUN-disproven double-free trap).
- The P2 `gorget_map_keys` `elem_clone` fix (`runtime_map.c`) is a SEPARATE genuine correctness fix (latent `.keys().clone()` double-free) — landable standalone, NOT part of this RSS chain; log to TODO.
- **Residual (~4.9 GB) → TODO:** the USER struct/enum `.get()`-deref-clones still leak. The full win (→ ~1.84 GB, near Rust) requires the architecturally-correct root fix the TODO/MEMORY already cite: make Branch C-pre **BORROW instead of deep-clone** for read-only / lifetime-bounded `.get().unwrap()` (the ".get() clone bomb"). That eliminates BOTH the leak AND the double-print and lets this gate (and the whole `source_was_ptr` skip) be removed. It's a CoW-lowering change (last-use/escape analysis on the bound var) — higher-risk, a dedicated future chain.

## Gates (RSS is the headline metric; GG_BUILD_TIMEOUT_SECS=600; ⚠ ~6-10 GB/stage — never 2 fixed_point runs concurrently)
1. `rm -f tests/fixtures/self_host_lowerer/driver{,.c}`
2. `cargo build` + `cargo test --lib` (1072/0).
3. **`self_host_bootstrap_fixed_point` GREEN** (canary 1).
4. **`self_host_runtime` 0-regressed** (canary 2 — REQUIRED alongside fixed_point; the 413 set must hold, esp. `drop_struct_collection_fields`).
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → parity **413/941** unchanged (neutral; any `leak_*` flip is a bonus).
6. `lowerer_comparison` (972) + `c_emit_comparison` (903) — report deltas (more drop calls; should track Rust).
7. **MEASURED RSS — ⚠ MUST measure the STAGE-1 (self-host-codegen'd) binary, NOT the stage-0 driver.** The fix changes the self-host's EMITTED C, so its effect is only visible in a binary built from that emitted C. Review pass 1 mistakenly ran the stage-0 driver directly (`gg build driver.gg` / `tests/fixtures/self_host_lowerer/driver …`) — that's a RUST-codegen'd binary (Rust's drops work → no leak → **1.35 GB FLAT, fix has no effect**). The leak + the win are on the STAGE-1 binary, assembled per `tests/integration.rs:14061-14136`:
   ```
   DRV=tests/fixtures/self_host_lowerer/driver
   "$DRV" tests/fixtures/self_host_lowerer/driver.gg lib --lir-c > /tmp/body.c   # stage-0 emits the SELF-HOST's C (~696K lines)
   # runtime_preamble = the Rust driver.c up to the first "\ntypedef struct __gg_"; assemble stage1.c = preamble + body.c
   cc -O0 -w -o /tmp/stage1 /tmp/stage1.c -lm -lpthread
   # RUN stage1 + sample /proc/<pid>/status VmHWM in a tight loop:
   /tmp/stage1 tests/fixtures/self_host_lowerer/driver.gg lib --lir-c &  PID=$!; while kill -0 $PID 2>/dev/null; do awk '/VmHWM/{print $2}' /proc/$PID/status; sleep 0.3; done | sort -n | tail -1
   ```
   Baseline (edit stashed) ≈ **9.96 GB**; with the fix ≈ **6.28 GB (−37%)** (profiler a0ec9be72 + scout a62a214e + the orchestrator's earlier `bootstrap_fixed_point` monitor all confirm the ~8-10 GB baseline). Alternatively, run `self_host_bootstrap_fixed_point` and monitor the `self_host_stage` process's `/proc/<pid>/status` VmHWM (the actual user-observed balloon). `/usr/bin/time -v` is NOT available on this box — use the `/proc/VmHWM` sampler.
8. ASan on `drop_struct_collection_fields` + a `.get()`-clone-heavy fixture — NO double-free/UAF.
(PARENT runs full `cargo test --test integration`.)

## Worktree discipline
- `pwd` + `git rev-parse --show-toplevel` FIRST; inside YOUR worktree. `git merge --ff-only gorget-1` FIRST.
- Stage ONLY: `git add tests/fixtures/self_host_lowerer/lower.gg docs/plans/dget_deref_clone_rss_brief.md` — NEVER `git add -a`/`.`.
- Commit on your branch; do NOT merge to gorget-1.
