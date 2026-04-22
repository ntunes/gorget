# Self-host memory baselines

Each JSON snapshot captures the output of `scripts/self_host_mem_baseline.sh` at a
specific point in the memory-optimization work. Compare against a baseline with:

```bash
scripts/self_host_mem_baseline.sh --compare scripts/baselines/phase1_pre_cow_fix.json
```

## Baselines

- **phase1_pre_cow_fix.json** (2026-04-21) — initial capture, before any
  Phase 2 / Phase 3 work. Documents the "clone-happy" starting point:
  ~1.3B array clones, ~4 GB peak RSS, ~800M allocations when stage-0
  runs `--lir-c` on its own source. This is the baseline that
  `ensure_owned_at_boundary` + CoW flow-insensitivity fixes must beat.

- **phase3a_prescan_extension.json** (2026-04-21) — after the extended CoW
  mutation-safety prescan landed (Phase 3a). **Zero delta** vs phase1:
  the prescan is infrastructure that tightens safety on the already-active
  Case B path but doesn't enable any new CoW (Case C stays dormant). Useful
  as a checkpoint for the next attempt to activate Case C.

- **phase3b_save_restore_extended.json** (2026-04-21) — after save/restore
  extension covering `local_ownership` + type-flip detection (Phase 3b).
  **+1.4% string_cow (+11,774 clones)** vs phase1. Counter-intuitive
  but correct: the extension stops ownership-state leaks across branch
  boundaries, so some materializations that were being skipped (incorrectly)
  now fire. Zero array_clone change, zero RSS change. Case C still dormant.

- **phase3c_branch_local_clearing.json** (2026-04-21) — after restore_locals
  clears branch-local CollectionRef/CowBorrow states at scope exit
  (Phase 3c). Same numbers as phase3b because Case C still dormant —
  the clearing is pure safety infrastructure that eliminates a class of
  post-scope UAF patterns (e.g. first-loop borrow re-materialised by
  second-loop field mutation on the same struct). Prereq for a safe
  Case C activation.

- **phase3d_case_c_active.json** (2026-04-22) — **THE WIN**. Case C
  (anon recv temp + field-path CoW borrow) activated, paired with the
  Box-deref double-deep-clone fix and the f-string deep-clone fix.
  Dramatic reductions vs phase1:
    - peak_rss_kb: 4,024,136 → 270,552 kB (**-93.3%**, 4 GB → 264 MB)
    - array_clone: 1,328,485,271 → 7,388,593 (**-99.4%**)
    - box_alloc:   1,308,692     → 747,742   (**-42.9%**)
    - total_allocs: 804,008,779  → 21,331,969 (**-97.3%**)
    - live_bytes:  1,240,195,751 → 186,859,368 (**-84.9%**)
  string_cow +1.0% (+8,207 clones) — absorbs the CoW materialization
  that replaces the billions of eager clones. Net: ~99% fewer runtime
  clones, ~93% less RSS. 984 unit + 1020 integration tests pass.
