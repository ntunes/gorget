# Brief — atomics/sync FIDELITY (Option A: extern-ify sync.gg + retire Rust map_stdlib_name sync entries)

OWNER-APPROVED 2026-06-01 (reference-grade, both-sides). Fixes ~13 self-host CC-FAIL fixtures
(onceflag/semaphore/atomic/barrier/condvar/waitgroup/shared_atomic/sync_*/thread_*/stress_*).
⚠ This brief still needs the standard ≥3 fresh sequential reviews before an executor launches.

## Root (scouted + verified 2026-06-01)
`lib/std/sync.gg` declares its equip methods BARE (`int load()`, `bool do_once()`, …). Rust
resolves the mangled call names via a hardcoded ~90-entry `map_stdlib_name` `match` at
`src/backend/mod.rs:11` (`"OnceFlag__do_once" => "gorget_onceflag_do_once"`, …) — INDEPENDENT of
the declarations. The self-host has NO such map → emits the UNDEFINED mangled call
`OnceFlag__do_once(...)`; and because the emitted calls aren't `gorget_*`, `needs_sync_pred`
(`tests/fixtures/self_host_lowerer/lir_codegen.gg:~5901`) doesn't splice `sync_runtime.c` → the
`GorgetOnceFlag`/`GorgetAtomicInt`/… typedefs are missing too. `lib/std/alloc.gg` (Arena, what R10
fixed) does it the clean declarative way: `extern int bytes_used() = "gorget_arena_bytes_used"`.
Fixing the redirect (via extern-ify → the R10 loader path auto-registers it) makes the calls
`gorget_*` → `needs_sync_pred` auto-splices the runtime → BOTH gaps close from ONE fix.

## Edit 1 — `lib/std/sync.gg`: extern-ify the equip methods
For EACH equip method that has a `map_stdlib_name` entry (`src/backend/mod.rs:11-139`), rewrite it
to `extern <ret> <method>(<args>) = "<exact gorget symbol from map_stdlib_name>"`. ⚠ Use the EXACT
symbol from `map_stdlib_name` — DO NOT derive it from the method name (e.g.
`CondVar.wait(Guard[bool] guard)` maps to `gorget_condvar_wait_guard`, NOT `gorget_condvar_wait`).
The sync types + their authoritative symbols (verify against `map_stdlib_name` — this is the source
of truth; reproduce ALL sync entries, not just these):
- `AtomicInt`: load→`gorget_atomic_int_load`, store→`..._store`, add→`..._add`, sub→`..._sub`,
  compare_exchange→`..._compare_exchange`.
- `AtomicBool`: load/store/swap/compare_exchange → `gorget_atomic_bool_*`.
- `Barrier`: wait→`gorget_barrier_wait`.
- `CondVar`: notify_one→`gorget_condvar_notify_one`, notify_all→`..._notify_all`,
  **wait(Guard[bool])→`gorget_condvar_wait_guard`** (note the `_guard` suffix).
- `WaitGroup`: add/done/wait → `gorget_waitgroup_*`.
- `Semaphore`: acquire/release/try_acquire → `gorget_semaphore_*`.
- `OnceFlag`: do_once→`gorget_onceflag_do_once`, is_done→`..._is_done`.
- `RWLock`/`ReadGuard`/`WriteGuard`: check `map_stdlib_name` for their entries + match exactly.
⚠ **`__new`/`__free` are NOT plain equip methods** — `map_stdlib_name` has `AtomicInt__new`/`__free`
etc., but constructors/drops are handled by a different path. INVESTIGATE before touching: do NOT
naively extern-ify a constructor. If `__new`/`__free` are already handled (and only the instance
methods are the gap), extern-ify ONLY the instance methods. Confirm empirically (emit-C before/after).
⚠ Confirm a BARE method with no `map_stdlib_name` entry (if any) is left alone (it's resolved
differently — maybe a real Gorget body or another mechanism).

## Edit 2 — `src/backend/mod.rs` `map_stdlib_name`: retire the now-redundant SYNC entries
Remove ONLY the sync entries that Edit 1's extern declarations now cover (AtomicInt/AtomicBool/
Barrier/CondVar/WaitGroup/Semaphore/OnceFlag/RWLock/guards). KEEP all non-sync entries (conversion
`int_to_str`/`ord`/`chr`, string, etc. — their stdlib files are NOT extern-ified here; retiring the
WHOLE map is the separate R12-adjacent cleanup). ⚠ Only retire an entry once you've confirmed Rust's
extern-equip path produces the IDENTICAL call for it (see the gate).

## ⚠ Both-sides risk — what MUST be verified
- **Rust gg must still compile + RUN the 13 sync fixtures IDENTICALLY** after Edit 1+2 (the
  extern-equip path replaces map_stdlib_name). `alloc.gg`'s `restore(ArenaCheckpoint cp)` proves
  arg-bearing extern equip methods work; VERIFY the tricky shapes: `compare_exchange(2 args)`,
  `CondVar.wait(Guard[bool] guard)` (a GENERIC-typed arg), the bool-returning ones. If Rust's
  extern path can't handle a shape, DON'T retire that map entry (leave it; note it).
- **map_stdlib_name is backend-shared** → the LLVM backend also uses it. Spot-check
  `GG_BACKEND=llvm` on a sync fixture (e.g. onceflag_basic).

## Gate (BIGGER than the self-host-only chains — touches src/ + shared stdlib)
1. `cargo build` + `cargo test --lib` green.
2. **FULL `cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/sync-int-$RANDOM.log`** —
   ALL fixtures (esp. the 13 sync + everything that touches sync), C backend. MUST stay green
   (Rust gg's output for sync fixtures must be unchanged).
3. `GG_BACKEND=llvm cargo test --test integration --release <one sync fixture>` — spot-check LLVM.
4. Force-rebuild the self-host driver; `cargo test --test integration --release self_host_runtime`
   (lock-in 243/0, no regression) + emit-C a sync fixture via the self-host driver and confirm it
   now emits `gorget_onceflag_do_once(...)` + the spliced `GorgetOnceFlag` typedef + cc compiles +
   stdout MATCHes `gg run`. Report which of the 13 become MATCH.
5. `self_host_bootstrap_fixed_point` green.
6. The parent runs the full sweep; the executor runs cargo build + --lib + targeted sync + the
   self-host checks.

## Commit / discipline
- Stage ONLY `lib/std/sync.gg` + `src/backend/mod.rs` (+ any new sync fixtures if added). Never `-a`.
- Output-review MUST `git checkout <executor-commit>` (or build the executor's branch) — NOT ff to
  gorget-1 (the Heap output-review built the wrong tree and returned a false verdict; see DONE.md).
