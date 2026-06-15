# Brief — LLVM residuals: int→bool cast (paren) + cover-struct size (tls + fs_typed)

**Track:** LLVM-green follow-on. Takes the LLVM sweep `1280/3 → 1283/0` (the 3 residuals: `paren_as_and_if_oneliner`, `stdlib_io_tls_writer`, `stdlib_io_fs_typed`). Both fixes RUN-scouted 2026-06-15 (paren `a6e5b95c` measured-green; cover-struct `a5e6e7f1`+`aa4cdd5c`). Two independent fixes, one executor.

## FIX A — `int as bool` lowers as `trunc i64→i1` not `icmp ne` (`paren_as_and_if_oneliner`)
**Root:** `src/backend/llvm/mod.rs` `Inst::IntCast` handler (~`:3496-3511`) casts i64→Bool through the narrowing `else` branch → emits `trunc i64 %v to i1` (keeps only bit 0). So even values (2,4,…) → false. `while (n as bool):` with `n=2` → false → loop body never runs → 2 output lines dropped. C emits `(bool)(x)` (nonzero→1).
**Fix:** in the integer-cast section, add a `to == LirType::Bool` special case **AFTER the `is_float()` check at `mod.rs:3499`, immediately before the `src_bits == to_bits` branch** (covers both the `:3509` trunc branch AND the `src_bits==to_bits` `add i1 0,%v` branch at `:3504`, also wrong for an i8/u8 source). ⚠ **Placement is load-bearing (review):** `float as bool` DOES reach this `Inst::IntCast` handler (Bool ∉ `is_integer()`) and is correctly handled by the in-handler `is_float()` branch (`:3499`, `fptoui … to i1`). If you hoist the Bool arm ABOVE `:3499`, `float as bool` emits invalid `icmp ne double …` → build break. Keep float→bool on the float branch; the new arm is int-sources-only because it sits after the float check.
```rust
} else if matches!(to, LirType::Bool) {
    // int → bool is truthiness (nonzero→true), NOT bit-0 truncation. Matches C's (bool)(x).
    writeln!(out, "  %v{} = icmp ne {src_ty_str} %v{}, 0", dst.0, value.0).unwrap();
} else if src_bits == to_bits {
    ...
```
**Scout measured-green:** patching the emitted `.ll` (`trunc … to i1`→`icmp ne … , 0`) made the full fixture byte-identical to C; review re-confirmed the verbatim diff builds clean and emits `icmp ne i64 …, 0`. Risk very low; doesn't touch genuine narrowing (i64→i32 stays `trunc`) nor float→bool (stays on the `is_float` branch).

## FIX B — cover-struct `computed_c_size` masks `opaque_runtime_size` (`stdlib_io_tls_writer` + `stdlib_io_fs_typed`)
**Root (ONE shared, the `091faaef` TaskGroup pattern — ONE-FIELD-COVER variant):** a Gorget cover struct under-declares its layout — `struct TlsSocket: int _handle` (8B) covers the 24B runtime `GorgetTlsSocket`; `struct File: int handle` (`lib/std/io.gg:23`, 8B) covers the 16B `GorgetFile`. `is_small_aggregate`/`sizeof_lir_type` read `def.computed_c_size` FIRST (`src/lir/lower/types.rs:461`/`765`) → 8, so the correct `opaque_runtime_size` fallback (`:464`/`771`; table at `:383`/`:386`) never fires.
- **tls:** 8≤16 → `needs_sret` FALSE → LLVM emits a register-return (`declare %TlsSocket @gorget_tls_connect`) where the runtime returns 24B by sret; on aarch64 the callee reads garbage `x8` → SIGSEGV. (`needs_sret` decision sites: declare `mod.rs:1622`, call-site `:6076`.)
- **fs_typed:** `File file = !f` move-out `memcpy(_,_,8)` into a 16B `%File` slot (`mod.rs:3086`/`:3509`'s SlotStore) → upper 8B (FILE* high word + `owned`) uninitialized → corrupt File → `buf.len()` reads 0 (expected 9). (`%File = type { ptr, i64 }` is correctly 16B via the `:971` override — only the size READ disagrees.)

**Fix (write-site, "fix the class", typed — NOT name-matching):** at the struct-size registration, make `computed_c_size` reflect the runtime layout when a cover struct's `opaque_runtime_size` exceeds the field-derived sum. ⚠ **Do this at the tail of `compute_struct_sizes` (`src/lir/mod.rs:1852-1874`) — the canonical one-time registration write — NOT inside `c_sizeof_struct_def` (`types.rs:277`), which is called RECURSIVELY for field/union-variant sizing (clamping there applies the inflation on a much broader surface → double-application / wrong-context risk). Review tested the `mod.rs` location green.**
```rust
let field_sum = /* existing field-summed size */;
let sz = match opaque_runtime_size(&def.name) {
    Some(rt) => field_sum.max(rt),   // runtime ABI size wins for a cover struct
    None => field_sum,
};
```
`opaque_runtime_size` returns `None` for ordinary user structs (no change), and `Some(==field_sum)` for the runtime singletons that already agree (no-op) — so ONLY the genuine cover-struct divergence (File 8→16, TlsSocket 8→24) is corrected. This one change flips ALL downstream ABI decisions consistently (declare sret + call-site sret + memcpy size + the type layout's trailing pad) on BOTH backends, and the LLVM `:765` `sizeof_lir_type` then reads the corrected value. Zero C-side risk (C spells `sizeof(GorgetX)` textually).
**ALSO fix the latent table bug:** split `"TlsSocket" => 24, "TlsServerSocket" => 16` at `src/lir/lower/types.rs:383` — `TlsServerSocket` is `{int64_t fd; SSL_CTX* ctx;}` = 16B (`tls_socket_runtime.c:~264` fwd-decl; runtime ctor `tls_server_runtime.c:14`), the table over-sizes it to 24 (review-confirmed `httpserver_tls` builds clean both backends after the split).
**⚠ AudioChunk note (review #3):** the table says `AudioChunk => 16` but the runtime `GorgetAudioChunk` is `{Mix_Chunk*}` = 8B, passed BY VALUE to `gorget_audio_play_channel`. `max(8,16)=16` makes the over-size load-bearing → a 16B load reads 8B past the slot. No running fixture exercises it (no SDL_mixer), harmless today — but DROP `AudioChunk` to 8 in the table (a pre-existing wrong entry), or leave it and file a TODO. Do NOT leave a silent latent over-read the `max()` activates.

**⚠ Blast radius:** Fix B touches `is_small_aggregate` → sret-vs-direct return decisions CORPUS-WIDE on both backends. Scout surveyed the `opaque_runtime_size` registry: only the TLS+File cover-class is affected today (Process/Arena return *pointers* = 8==8 safe; sockets 8==8; multi-field runtime structs already agree); AudioChunk future-proofed. **Gate on the FULL LLVM sweep + FULL C sweep + `fixed_point`** (parent runs these at integration). FALLBACK if the write-site fix regresses the sweep: the narrower LLVM-local guard at `sizeof_lir_type` (`mod.rs:~765`, before the `computed_c_size` read: `if let Some(rt)=opaque_runtime_size(&def.name) { if Some(rt)!=def.computed_c_size { return rt; } }`) — LLVM-only, low-risk, per-instance.

## Gate (executor runs targeted; PARENT runs the full sweeps at integration)
- `cargo build` + `cargo test --lib`.
- `GG_BACKEND=llvm cargo test --test integration --release paren_as_and_if_oneliner` → PASS (build+run binary, not `gg run` — but these are stdout-diff tests so the harness runs them); `stdlib_io_tls_writer` → PASS; `stdlib_io_fs_typed` → PASS (run each separately — one positional filter).
- C-backend no-regression: a slice of `static_*` + `stdlib_io_*` + `tls_*` fixtures on the default backend (Fix B touches shared `is_small_aggregate`).
- A `GG_BACKEND=llvm` regression slice over struct-returning/aggregate-handle fixtures (tls/socket/file/process/arena families) — confirm no new sret-ABI breakage.
- `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point` → GREEN.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/backend/llvm/mod.rs` (Fix A) + `src/lir/lower/types.rs` (and `src/lir/mod.rs` if the size is set there) (Fix B). No `git add -a`. Do NOT touch `TODO.md`/`DONE.md`/`MEMORY.md`. Commit Fix A and Fix B separately (they're independent). Report verbatim gate results + whether you used the write-site or fallback for Fix B.
