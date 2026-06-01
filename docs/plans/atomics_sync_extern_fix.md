# Brief v2 — atomics/sync FIDELITY (self-host construction + extern-method dispatch)

OWNER-APPROVED 2026-06-01 ("bundle the complete fix"). Supersedes v1 (Option A:
"extern-ify + retire map_stdlib_name"), whose premise the empirical check below
refuted. **This is the durable spec + record of a fix ALREADY IMPLEMENTED +
EMPIRICALLY VALIDATED inline by the orchestrator** (the inline prototyping WAS the
"confirm empirically (emit-C before/after)" the v1 brief demanded). It now needs
the standard ≥3 fresh sequential REVIEWS of the actual diff + the full gate before
it is committed to `gorget-1`.

## Why v1 was incomplete (empirical findings — ran the self-host driver, did NOT source-read)
Emitting C for `sync_atomics.gg` via the self-host driver (`driver F lib --emit-c
--runtime-dir=…`) showed **THREE independent gaps**, not one:
1. **Construction broken.** `AtomicInt(0)` → `((GorgetAtomicInt *)(__v2))-> = __v0;`
   — a malformed store into a zero-field struct (sync structs are `pass`). Rust gg
   instead special-cases it to `gorget_atomic_int_new(0)` (returns a heap
   `GorgetAtomicInt*`). v1 (extern-ify methods) does NOT touch construction.
2. **Methods undefined.** `AtomicInt__load(__v4)` — non-generic sync types keep
   their bare type name, hit the bare-name branch in `build_resource_metadata`
   (`lir_lower.gg:374`) → `method_prefix=None` → not registered in
   `type_method_prefix_map` → method calls fall through unchanged → undefined.
3. **Typedef/splice missing.** `GorgetAtomicInt` undeclared — `needs_sync_pred`
   (`lir_codegen.gg:5901`) only splices `sync_runtime.c` when a `gorget_*` sync
   call is present; with (2) emitting `AtomicInt__load` (not `gorget_*`), nothing
   triggers the splice.

The cc error chain confirms the layering: `sync_atomics` first cc-fails on
`unknown type name 'GorgetAtomicInt'` (gap 3), then would hit the broken `-> =`
(gap 1), then the undefined methods (gap 2). **Fixing only methods+splice leaves
construction broken → fixtures keep CC-FAILing → net-zero parity.**

## The proven template = `lib/std/alloc.gg` (which v1 itself cited)
`alloc.gg`'s allocator types compile + MATCH (`arena_basic`/`fba_basic`/`tlsf_basic`
are MATCH in the diagnostic) via a **TWO-part** treatment:
- extern equip methods (`extern int bytes_used() = "gorget_arena_bytes_used"`), AND
- an R9 construction special-case `allocator_constructor_runtime_name` +
  call-site branch (`lower.gg:5738-5767`) that emits `gorget_<kind>_new` and
  registers the dst as a **pointer**-to-named slot.
v1 copied only the methods half. The sync types need BOTH halves.

## The fix (self-host only — 3 edits) — IMPLEMENTED + VALIDATED
### Edit 1 — `lib/std/sync.gg`: extern-ify the equip INSTANCE methods
Give each instance method the declarative `extern <ret> m(args) = "gorget_<sym>"`
form, using the EXACT symbol from `map_stdlib_name` (`src/backend/mod.rs:83-119`):
- `AtomicInt`: load/store/add/sub/compare_exchange → `gorget_atomic_int_*`
- `AtomicBool`: load/store/swap/compare_exchange → `gorget_atomic_bool_*`
- `Barrier`: wait → `gorget_barrier_wait`
- `CondVar`: notify_one/notify_all → `gorget_condvar_notify_*`;
  **wait(Guard[bool]) → `gorget_condvar_wait_guard`** (the `_guard` suffix)
- `WaitGroup`: add/done/wait → `gorget_waitgroup_*`
- `Semaphore`: acquire/release/try_acquire → `gorget_semaphore_*`
- `OnceFlag`: do_once/is_done → `gorget_onceflag_*`
- **`RWLock`/`ReadGuard`/`WriteGuard`: LEFT BARE** (no `map_stdlib_name` entry;
  resolved by the guard mechanism; touching them hits the separate, pre-existing
  `gorget_guard_set`/`gorget_write_guard_set` arity bug — out of scope).

The self-host loader's R10 extern-stub path (`loader.gg:788-821`) registers
`call_redirects["AtomicInt__load"] = "gorget_atomic_int_load"` (key =
`equip_target_name + "__" + method`). The method call then resolves to the
`gorget_*` symbol → `needs_sync_pred` auto-splices `sync_runtime.c` (gaps 2+3
close from this one edit).

### Edit 2 — `lower.gg`: sync construction special-case (the half v1 missed)
Add `sync_constructor_runtime_name(name)` (mirrors `allocator_constructor_runtime_name`):
`AtomicInt`→`gorget_atomic_int_new`, `AtomicBool`→`gorget_atomic_bool_new`,
`Barrier`→`gorget_barrier_new`, `CondVar`→`gorget_condvar_new`,
`Semaphore`→`gorget_semaphore_new`, `WaitGroup`→`gorget_waitgroup_new`,
`OnceFlag`→`gorget_onceflag_new`. Add the `elif sync_constructor_runtime_name(fname)
!= ""` branch at the constructor call site (right after the allocator branch,
`lower.gg:~5768`): lower args, register the dst as `register_ptr(lookup_or_register_named(fname))`,
emit `GICallExtern(dst, sync_new_fn, args)`. (RWLock/guards excluded — generic,
construct via method_prefix.) Closes gap 1.

### Edit 3 — `loader.gg`: register the BOOL return type for bool extern stubs
The R10 extern-stub path only pushed an empty-body fn stub (→ fn_sigs return type)
for **void** returns. The bool-returning sync methods (`compare_exchange`, `swap`,
`AtomicBool.load`, `OnceFlag.do_once`/`is_done`, `Semaphore.try_acquire`) otherwise
default to I64 → the formatter prints `1`/`0` instead of `true`/`false` (the R8
class — VALIDATED: `sync_atomics` printed `1` for `compare_exchange` until this
edit). Extend the stub-push guard from `stub_rt == "void"` to
`stub_rt == "void" or stub_rt == "bool"` (`loader.gg:841-851`). int still defaults
to I64 correctly; String/cstr MUST stay unregistered (they rely on the I64 slot +
cstr-coercion). Drop: NO `_free` call is synthesized for the now-heap-pointer
locals (validated — only the inline `static` defs appear, no call) → benign leak,
no broken `AtomicInt__free`/`__drop` call; stdout MATCHes regardless.

## DEFERRED — v1's "retire `map_stdlib_name` sync entries" (PROVEN UNSAFE this round)
Empirically: retiring the sync INSTANCE-method entries from `map_stdlib_name`
**breaks Rust gg** — `sync_condvar`/`shared_atomic`/`shared_atomic_bool` link-fail
with `undefined reference to 'CondVar__notify_one'`/`'AtomicInt__add'`/
`'AtomicBool__store'` **inside SPAWNED functions** (`.text.producer`/`.text.add_n`/
`.text.toggle`). Root: Rust's spawn-wrapper / async-coroutine lowering emits the RAW
mangled method name and resolves it via `map_stdlib_name` at the LIR layer
(`lir/lower/calls.rs:466`), **bypassing** the `extern_bindings` GIR path
(`exprs/methods.rs:2115`) that the non-spawn path uses. So those map rows are NOT
dead. (This also explains why R10 left the alloc map entries intact.) This
map-retirement (v1's separate edit, NOT the v2 "Edit 2" construction special-case
above) has ZERO parity benefit and breaks the build → **deferred**. A future round can route
the Rust spawn-wrapper method-call lowering through `extern_bindings` FIRST, then
retire the now-genuinely-dead rows. Logged to TODO.

## Expected outcome (VALIDATED)
+3 runtime parity: `sync_atomics`, `sync_barrier`, `waitgroup_basic` → MATCH (were
CC-FAIL). Rust gg output-UNCHANGED across all sync fixtures (the lone
`shared_atomic_error` "failure" is a pre-existing NEGATIVE test — `shared(atomic)
float` correctly rejected; identical on the clean tree). Other sync fixtures stay
CC-FAIL on SEPARATE, pre-existing gaps (out of scope): guard-set arity
(sync_condvar/sync_rwlock/mutex_basic/thread_mutex), int-as-Str `current_thread_id`
(thread_basic/thread_atomic/thread_barrier), `gorget_sleep_ms` void-assign +
spawn-execution (semaphore_basic/onceflag_basic), spawn-wrapper undefined refs
(shared_atomic*).

## Gate (parent-run; touches the SHARED stdlib `sync.gg` → full suite)
1. `cargo build` + `cargo test --lib` green.
2. **FULL `cargo test --test integration -- --test-threads=4`** (C backend, ALL
   fixtures — Rust gg's sync output must be unchanged; `sync.gg` is shared).
3. `GG_BACKEND=llvm cargo test --test integration --release <one sync fixture>`
   spot-check (`map_stdlib_name` is backend-shared; though the map-retirement was
   reverted, Edit 1 changes shared `sync.gg`).
4. Force-rebuild the self-host driver; `cargo test --test integration --release
   self_host_runtime` (lock-in ≥243/0, no regression) + re-measure parity via
   `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` (expect ≥246).
5. `self_host_bootstrap_fixed_point` green.

## Files (stage by name only — never `-a`)
`lib/std/sync.gg`, `tests/fixtures/self_host_lowerer/lower.gg`,
`tests/fixtures/self_host_lowerer/loader.gg`. (`src/backend/mod.rs` is NOT touched
in the final scope — the map_stdlib_name retirement is deferred.)
