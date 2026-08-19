# FIXED (R43 Track C) — `Callable[T(Struct &)]` + Vector-iterator callback → SIGSEGV

**Discovered:** R39 Phase 2e Sub-task 0 probe (2026-08-09). **Fixed:** R43 Track C.

**Fixture:** `repro.gg` (this directory).
**Integration test:** `callable_amp_struct_iterator_body_writes_through`
in `tests/integration.rs` — **LIVE, not `#[ignore]`d**, asserting `1\n2`.
It keeps the `known_gap_` prefix off its name because it no longer describes a gap.

The fixture STAYS in `known_gaps/` on purpose: `runtime_parity_corpus` never
descends subdirectories, so the live test asserts on C AND LLVM with zero
self-host parity-corpus inflow — and the self-host lane still mis-lowers
indirect `&` calls (filed in `TODO.md`).

## Repro

```
./target/debug/gg build tests/fixtures/known_gaps/rust_gg_bug_callable_amp_struct_iterator_segv/repro.gg
./tests/fixtures/known_gaps/rust_gg_bug_callable_amp_struct_iterator_segv/repro
```

- **Before the fix:** rc 139 (SIGSEGV) on C and on `--backend=llvm`, `gg check` clean.
- **After:** prints `1` then `2` on both backends.

## The mechanism — and the three speculations it refuted

This README used to offer three fix directions, all of which are **measurably wrong**;
they are kept here because a stale speculation that reads as plausible costs the next
reader a day.

- ✗ *"the `t` slot's address is stale by the time f is invoked"*
- ✗ *"the fn-pointer wrapper mangles the `Thing &` calling convention"*
- ✗ *"drop-elab frees `t` in the loop body pre-call"*

The GIR passes a **correct pointer** — `gg build --emit-gir` on this repro shows
`_18 = borrow_mut _8` feeding `call @__callable_2(copy _2, copy _18)`. Nothing about
`t`'s lifetime or the shim was wrong. What was missing is the argument's **ABI TAG**:
at an indirect call, each argument's pointer-vs-value ABI used to be GUESSED from the
argument's pointee SHAPE at two independent backend read sites, and this shape's
callable arrives as a `Callable[..]` **PARAMETER**, whose GIR local type is erased to
`unit` (`fn @map_thing(*Vector__Thing, unit)`, `_2: unit ; f`) — so the callee's
declared `&` was unavailable where the decision was made.

R43 Track C writes the tag from the callee's DECLARED parameter ownership at one LIR
site, and publishes the declared ABIs at the GIR call site (`abi::indirect_callee_key`)
for exactly this erased-signature provenance. The iterator has nothing to do with it:
`while`/`get()`/`unwrap()` only supplied the PARAMETER-bound callable that the
annotated-local shape does not.

## Sibling shapes

- `rust_gg_bug_callable_amp_struct_closure_literal/` — a closure literal in the same
  position. STILL A GAP (different root: parameter-type inference), still `#[ignore]`d.
- `tests/fixtures/callable_ref_param.gg` — the direct `with_counter(f, &c)` control,
  green throughout.
- `tests/fixtures/known_gaps/callable_amp_abi_*.gg` — the R43 net, whose
  `callable_amp_abi_param_binding.gg` pins this same provenance alongside the
  `apply(bump, &p)` form.
