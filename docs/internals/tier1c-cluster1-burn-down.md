# Tier 1c Cluster 1 burn-down — closing the last migration site

**Filed:** 2026-05-11. **Status:** Active. **Estimated effort:** days, not weeks.

This doc anchors the work to close the final Tier 1c migration site:
removing the `monomorphize_enum` Option/Result carve-out at
`src/ir/lowering/generics/mod.rs:2440`.

The other five Tier 1c sites are already shipped — see
`docs/internals/structural-guards.md` §1c and the entries in
`DONE.md` between 2026-05-11 commits `a59faf33` and `ecc780fe`.

## What "closing" means

When the carve-out is removed:

```rust
// generics/mod.rs:2440 — final shape, no Option/Result skip
let (drop_strategy, copy_semantics) = registry.compute_drop_strategy_for_enum(&variants);
let metadata = TypeMetadata {
    enum_category,
    drop_strategy,
    copy_semantics,
    ..Default::default()
};
```

Then `validate_type_metadata_coherence` (Tier 1c env-gated validator) can
be promoted to fatal at zero violations. That's the second of the two
remaining items.

## What surfaces when the carve-out is removed

A 2026-05-11 probe with `validate_resource_moves` env-gated, **after**
adding view-awareness to that validator (this commit's change), shows:

1. **2 genuine Rust GIR validator violations** across the full suite:
   ```
   Describer_for_Widget__describe:bb0:i2: OpCopy(_3) on resource GorgetString
   main:bb0:i4: OpCopy(_1) on resource GorgetString
   ```

2. **17 runtime regressions** (suite passes 1082→1065). These are
   correctness regressions where the runtime now produces wrong
   output — typically a buffer overread / wrong-sized memcpy when a
   cross-type adapter result is byte-copied into a wrongly-sized
   destination slot.

The previously-reported "15,634 violations" turned out to be **mostly
SELF-HOST validator output** (a different format and origin —
self-host's own GIR validator running while compiling self-host
fixtures, part of the existing Phase C self-host validator burn-down
at ~89k baseline). After view-awareness was added to the Rust
validator AND that prior misattribution was corrected, the Cluster 1
burn-down shrank by orders of magnitude:

```
Before:  15,634 GIR violations + 17 runtime regressions  →  1-2 weeks
After:    2 GIR violations + 17 runtime regressions       →  days
```

### View-awareness (shipped 2026-05-11)

`validate_resource_moves` previously flagged every `Assign{Copy}` of
a resource type. But a `Copy` of a `Borrowed` or `View` source is
runtime-safe — both source and copy are non-owning aliases (cap=0
GorgetString views, Ptr-typed borrows), so the resulting byte-copy
creates another non-owning alias whose drop is a no-op. The validator
now mirrors `validate_consume_sites` (`src/ir/validate.rs:2507`) by
skipping Borrowed/View sources at the `assign_read_site` level.

This collapsed false positives. The remaining 2 violations are
genuine Owned-source shallow copies on resource types — likely from
the same root cause as the 17 runtime regressions (cross-type result
adapter destination mis-sizing).

## The 17 runtime regressions

```
coroutine_result_combinators   http_patch
deserializable                  httpserver_e2e
heap_advanced                   httpserver_e2e_extended
httpserver_json                 httpserver_large_body
httpserver_lifecycle            httpserver_parse_request
option_result_combinators       result_map
stdlib_iter_bounds_coverage    stdlib_iter_drain
test_result_advanced            test_result_all
vector_userspace_hofs
```

### Concrete root cause example (result_map.gg)

```gorget
Result[int, int] coded = err.map_err((String e): e.len())
if coded is Error(code):
    print(f"{code}")    # missing — should print "4"
```

Where `err: Result[int, String] = Error("fail")`.

The generated C shows the bug:

```c
__gg_Result__int64_t__GorgetString __s3 = {0};   // declared as OLD type (48 bytes)
__gg_Result__int64_t__int64_t __s6 = {0};        // map_err result (NEW type, 24 bytes)

// ... __s6 receives map_err's output (24 bytes) ...

__v8 = &__s6;
memcpy(&__s3, __v8, sizeof(__gg_Result__int64_t__GorgetString));
//     ^^^^                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//     dst is 48-byte slot    SIZE token is 48 — reads PAST __s6 (24 bytes)!
```

`__s3` is the LIR local for `coded` (`Result[int, int]`, 24 bytes
expected). But it was allocated with the OLD type (`Result[int,
String]`, 48 bytes). The VarDecl's destination type isn't being
propagated from the cross-type adapter's `result_type` (in
`src/ir/lowering/exprs/methods.rs:2625` for `map_err`).

The fix is at the VarDecl-from-adapter-result site: when the RHS is a
cross-type adapter call returning a NEW Result type, the LHS local
must use the NEW type, not the receiver's OLD type. Likely in
`lower_var_decl` or the call-result type inference.

## Migration plan (days, not weeks)

### Phase 1: Fix the cross-type adapter dst type bug (~1 day)

This single bug likely accounts for most of the 17 runtime regressions
— they're all Option/Result combinator fixtures using cross-type
adapters (`map`/`map_err` that change a payload type, `unwrap_or` etc.).

Investigate the destination local allocation path. Likely in
`stmts/mod.rs::lower_var_decl` or its callers — the local's type
should come from the RHS expression's inferred type, not the receiver.

### Phase 2: Fix the 2 remaining GIR violations (~few hours)

`Describer_for_Widget__describe:bb0:i2` and `main:bb0:i4` —
inspect the fixtures, identify the emission site, migrate to
Move/Borrow.

### Phase 3: Promote validator + close (~few hours)

1. Remove the `monomorphize_enum` Option/Result carve-out at
   `generics/mod.rs:2440`.
2. Promote `validate_type_metadata_coherence` to fatal at zero
   violations.
3. Verify full integration suite: should be back to 1082/1082.
4. Update `docs/internals/structural-guards.md` Tier 1c entry to
   COMPLETE.

## How to probe locally

```bash
# Re-apply the carve-out removal:
# Edit src/ir/lowering/generics/mod.rs:2440-2454 — remove the
# Option | Result branch, always use compute_drop_strategy_for_enum.

# Temporarily de-promote the validator to log mode:
# Edit src/ir/lowering/mod.rs:1580-1589 — wrap the panic in
# `if let Ok(_) = env::var("GG_VALIDATE_RESOURCE_MOVES") { ... }`.

# Collect violations + 17 runtime failures:
rm -f /tmp/probe.log
GG_VALIDATE_RESOURCE_MOVES=/tmp/probe.log \
  cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/sweep.log | tail -3
wc -l /tmp/probe.log

# Inspect a runtime regression:
GG_VALIDATE_RESOURCE_MOVES=/tmp/r.log cargo test --test integration result_map -- --test-threads=1 2>&1 | head -30
cargo run -- build /tmp/test.gg -o /tmp/out  # check the emitted .c for memcpy size mismatches
```

## Why this doc exists

A future session picking up Tier 1c needs the corrected scoping to
plan effectively. The original "1-2 weeks" estimate was based on a
miscounted violation set (self-host validator output that was already
on the Phase C burn-down, not new from carve-out removal). With
view-awareness shipped and the runtime-regression root cause
identified (cross-type adapter dst type bug), the work is now
**days**.
