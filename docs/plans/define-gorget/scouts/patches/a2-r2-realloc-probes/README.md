# A2-R2 M1 — R1 aliasing-window probes (ASan measurement)

The compound-assign ICE fix reads a resource vector/dict element by BORROW
(a `Ptr` aliasing it in place). The self-Ptr is taken before the RHS was
originally lowered, so a RHS that reallocs the SAME collection would dangle it
→ UAF. Resolution taken = **(b) reorder**: the RHS is lowered into an owned
temp BEFORE the `read_place` header capture AND the borrow read (see
`src/ir/lowering/stmts/assigns.rs`, the `rhs_pre` block).

Probes (build with `gg build <f> --sanitize` and run under
`ASAN_OPTIONS=detect_leaks=1`):

- `realloc_v.gg` — `v[0] += grow(&v)` (local vector, RHS pushes 200 → reallocs).
  Post-reorder: ASan-CLEAN, result 6, both backends. Counterfactual (reorder
  disabled): `heap-use-after-free`.
- `realloc_field.gg` — `h.v[0] += grow(&h)` (FIELD-accessed vector; header-copy
  `read_place` path). Post-reorder: ASan-CLEAN, result 6. This case needed the
  reorder to precede the `read_place` capture, not just the borrow read.
- `realloc_d.gg` — `d["a"] += growd(&d)` (dict insert-realloc). No UAF either
  way in this probe; the exit-1 is a pre-existing LeakSanitizer leak from the
  probe's own 200 f-string-keyed inserts (orthogonal to the compound assign).
- `custom_idx.gg` — R2 sibling: custom `Index`-equipped type with a resource
  element + `add` overload. STILL ICEs at `mod.rs:1763` (custom `__get` returns
  owned → shallow-copy path). REACHABLE, filed as a HIGH Core-#4 sibling TODO
  (M1's borrow-in-place is scoped to vector/dict).
