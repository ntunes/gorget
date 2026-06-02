# Brief — delete the divergent enum-own-name drop-collision guard (`lir_lower.gg`)

CLEANUP round (1:1:1:1 cadence). Self-host-dir only, **`lir_lower.gg` ONLY** — runs as a
PARALLEL chain file-disjoint from arena_checkpoint (`loader.gg`) and EMatch-as-value
(`lower.gg`). ⚠ Needs ≥3 fresh sequential reviews before the executor launches.

## ⚠ The ORIGINAL brief was REFUTED — read this
The original cleanup idea ("add the `drop_collision_types` guard to the ENUM EMIT sites in
`lir_codegen.gg`") is **UNSOUND** — it would make the self-host DIVERGE from Rust. Rust leaves
the enum's OWN drop name UNGUARDED at both populate (`src/lir/lower/mod.rs:737`,
`format!("{name}__drop")`) and emit (`src/backend/c_lir/emit_types.rs:1260`). The struct/enum
guard asymmetry is INTENTIONAL in Rust (structs guard at `mod.rs:680-686`; the enum guard
applies ONLY to variant-PAYLOAD field types, `mod.rs:714-719`, never the enum's own name). The
self-host EMIT sites (`lir_codegen.gg:4991` fwd / `:5000` def) are ALREADY unguarded =
already correct = already byte-matching Rust. Do NOT touch them.

## The REAL bug (latent self-host-internal inconsistency, verified against source)
The self-host's enum POPULATE site OVER-applies the guard to the enum's own name, diverging
from Rust. `lir_lower.gg:3634-3636`:
```
String drop_fn_name = type_name + "__drop"
if m.drop_collision_types.contains(type_name):     # ← divergent guard (enum's OWN name)
    drop_fn_name = "__gorget_dtor_" + type_name
```
Rust `mod.rs:737` is just `format!("{name}__drop")` — no guard. This creates a
**seed-vs-emit name mismatch** that, under an enum drop-collision, would DCE-prune the real
drop fn:
- **DCE seed** (`lir_codegen.gg:1082-1084`) reads the **guarded** `type_drop_fns[enum].
  drop_fn_name` → under collision marks `__gorget_dtor_{enum}` reachable.
- **Emit** (`lir_codegen.gg:4991/5000`) emits the **unguarded** `{enum}__drop`.
- → the seed never matches the emitted name → `{enum}__drop` can be pruned as unreachable →
  the enum's drop fn is dropped → leak / UAF.

(`lc_collection_drop_fn` at `lir_codegen.gg:1473-1475` also reads `type_drop_fns[...].
drop_fn_name` for enum element types, so under collision it too would emit a
`__gorget_dtor_{enum}(...)` call that no longer matches the emitted `{enum}__drop` — the same
inconsistency, fixed by the same deletion.)

## Why it's LATENT / output-neutral (confirmed)
`drop_collision_types` is populated ONLY by a `{T}__drop` function with >1 param on a
registered type (`lir_lower.gg:3718-3729`, mirrors Rust `mod.rs:540-546`) — i.e. a user
multi-arg `.drop(...)` method. The ONLY such type in the entire corpus is the builtin
`DataFrame`, which is struct-like (never in `recursive_drop_enums`). So
`m.drop_collision_types.contains(type_name)` at `:3635` is NEVER true for any enum → the
guarded branch is **dead code today**. Deleting it changes ZERO emitted bytes for the current
corpus; it's correctness-hardening + a fidelity (reads-like-Rust) fix.

## Fix — delete the guard (2 lines) + fix the comment
At `lir_lower.gg:3632-3636`, change:
```
            # Also add to type_drop_fns. Collision types take the mangled
            # `__gorget_dtor_{T}` glue name (see the struct branch above).
            String drop_fn_name = type_name + "__drop"
            if m.drop_collision_types.contains(type_name):
                drop_fn_name = "__gorget_dtor_" + type_name
```
to:
```
            # Also add to type_drop_fns. The enum's OWN auto-glue drop name is
            # ALWAYS `{T}__drop` (unguarded) — mirrors Rust `lower/mod.rs:737`
            # and the unguarded enum emit sites (`emit_enum_drops`). Do NOT apply
            # the struct-branch collision guard here: the DCE seed reads this
            # `drop_fn_name` (lir_codegen.gg:1084) while emit uses `{T}__drop`
            # (lir_codegen.gg:4991/5000); guarding here would mismatch them and
            # DCE-prune the real drop fn. The variant-PAYLOAD field types ARE
            # collision-guarded — see `drop_fn_for_type` above (matches Rust
            # mod.rs:714-719) — but the enum's own name is not.
            String drop_fn_name = type_name + "__drop"
```
Do NOT touch the struct branch, the payload-field guard (`drop_fn_for_type`, `:3617`), or
the `drop_collision_types` field/population (still needed by the struct + payload paths).

## Validation gate (self-host-dir only — no `src/`; output-neutral, so byte-identity is the gate)
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild the driver.
3. **`c_emit_comparison`** (`--nocapture`) — matched count UNCHANGED (the real byte gate; this
   is output-neutral, so any delta is a red flag — STOP and report).
4. **`self_host_bootstrap_fixed_point`** GREEN (`--test-threads=1`) — byte-identical reconverge.
5. `self_host_runtime` lock-in net GREEN, unchanged at 248/0 (no parity movement expected).
6. NO new snapshots (output-neutral — nothing moves).
7. The PARENT runs the full integration sweep + `GG_BACKEND=llvm` spot-check at integration
   (the guard/drop path is backend-shared LIR; the executor runs the targeted C gates above).

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lir_lower.gg` ONLY. Do NOT touch `lir_codegen.gg`,
`loader.gg`, `lower.gg`, `src/`, or `TODO.md`/`DONE.md` (the parent owns TODO/DONE across the
parallel chains).
