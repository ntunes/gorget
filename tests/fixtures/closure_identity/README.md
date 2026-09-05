# `closure_identity/` — why these live in a subdirectory

These are ordinary, PASSING regression fixtures, wired as live `run_gg` tests in
`tests/integration.rs`. Nothing here is a known gap. They pin two families, and
the families share a cause: the compiler decided a callee's IDENTITY from its
SPELLING, in a namespace user code writes into.

**Family 1 — `closure_arg_*_named_call*` (R49 Track A1-IDENTITY).** A user
method named `call` mangles to `Runner__call`, so the LIR call lowering's
`!func.contains("__call")` matched it and left its closure argument unpacked — a
stack-buffer-overflow on a capture-less closure, and a plausible WRONG NUMBER
with a clean exit code, silent under ASan and UBSan, on a closure whose first
capture is itself a `Callable`. Closure identity is now typed metadata
(`ir::Function::takes_env`, `TypeMetadata::closure_call_fn`,
`StructDef::closure_call_fn`).

**Family 2 — `collide_*` / `extern_symbol_*` (R49 Track A2-α).** Every
indirect-dispatch arm MANUFACTURED its callee name — `__callable_<slot>`,
`__gorget_closure_call_<slot>` — emitted a plain `Instruction::Call` with it, and
injected the callable's signature into the module-global `fn_sigs` /
`fn_param_ownerships` tables under it. `__callable_1` is a legal Gorget
identifier and a legal `extern "C"` symbol, so an ordinary program declaring
`int __callable_1(int, int)` had its closure calls dispatched to the user's
function (C: exit 0 with a nondeterministic heap-derived number; LLVM: `llc`
type failure — the backends DISAGREED), its own direct calls re-typed by the
injected `unit` return type so the result was DISCARDED, and, through
`extern "C"`, exit 139. The callee's identity now rides on
`Instruction::CallIndirect` — for these two conventions there is no name to
manufacture, so there is nothing left to collide.

⚠ **SCOPED TO THE TWO RETIRED PREFIXES, NOT TO INDIRECT DISPATCH AS A WHOLE.**
Five of the nine indirect-dispatch arms still pass a NAME (`IndirectCallee::
Named`), because their callee is a genuine emitted symbol: a lifted closure's
`__Closure_N__call` thunk, a trait-object vtable slot, a `Constant::FuncRef`.
`__Closure_N__call` is still MINTED (`src/ir/lowering/closures.rs`), so a user
function spelled `__Closure_0__call` still collides with it — loudly: `gg check`
accepts and `gg build` exits 101 on `duplicate function name`, identically
before and after this work. Loud is not fixed. `todo/t1235`.

They assert **stdout**, not exit codes, and family 2 asserts the VALUE `42`
never a snapshot: its wrong answer is a live pointer plus a constant and differs
between runs. A fix validated on `rc != 139` greens the loud cells of family 1
and every cell of family 2 that exits 0 with the wrong number.

## Why not top-level

MOST of them pass a closure LITERAL at a CALL-ARGUMENT position, because that is
the shape both defects need — and that shape leaks its environment through
`__gorget_closure_env_alloc`. **Measured identically on the PRE-FIX compiler**
(32 bytes in 1 allocation for the free-function control), so it is pre-existing
debt owned by `todo/t0953`, the single largest class in
`tests/sanitize/LEAK_ALLOWLIST.txt` — not inflow from the changes these fixtures
pin.

⛔ **STILL TRUE AFTER `t0953` PARTLY LANDED (R50 Track E), AND THAT IS THE
POINT.** Track E drains the closure-argument temp at the three builtin-HOF
expanders only. Every fixture here passes its literal at a **plain call**, which
is deliberately NOT drained: a plain callee is user code that may RETAIN a
borrowed `Callable` (`lib/std/iter.gg`'s lazy adapters do — `todo/t1349`, write
site `todo/t1350`), and freeing the caller's temp there makes a second owner —
measured as a use-after-free and a double-free. So the leak these fixtures carry
is unchanged: `closure_arg_user_method_named_call.gg` is **96 bytes in 4
allocations on both the pre-fix and the post-fix compiler**. Regenerate with
`gg build --sanitize <f>` under
`ASAN_OPTIONS=detect_leaks=1:exitcode=0 LSAN_OPTIONS=use_stacks=0`.

(`collide_no_closure*` and `extern_symbol*` carry no closure literal and so do
not leak. They stay here anyway: a family split across two directories is a
family whose next reader only finds half of it.)

Top-level `tests/fixtures/*.gg` is what `scripts/sanitize_sweep.sh` sweeps, so
landing them there would admit NEW rows to that allowlist for a PRE-EXISTING
class. The list is shrink-only and its new-inflow case is an explicit owner ask.
`tests/sanitize/CORPUS_MANIFEST.txt`'s `closure_identity` row carries the same
reasoning and the condition that retires it.

## ⚠ WHAT THAT COSTS, so a future reader knows what they gain and what they owe

Out of the top-level scan is also out of **`runtime_parity_corpus`**. The
self-host lane measurement below is therefore a MEASUREMENT TAKEN WHEN THESE
LANDED, **not a continuously enforced gate — nothing will notice if a later
change breaks it.** The C and LLVM lanes stay continuously pinned by the
`run_gg` tests; only the self-host lane is uncovered.

Measured 2026-09-04: all six FAMILY-1 cells COMPILE, RUN and MATCH on the
self-host lowerer lane, including the five Rust gg got wrong — this is the
succession plan's "reference lags the self-host" case.

⛔ **THAT RESULT IS SCOPED TO FAMILY 1 AND DOES NOT GENERALISE.** On family 2 the
self-host is WRONG, and wrong more widely than Rust gg ever was: its arg-ABI
table (`self_host_lowerer/lir_lower.gg`, `needs_ptr_arg`) address-takes argument
0 of any call whose callee NAME carries the prefix, with no `func_index`
precedence check — so a five-line program with NO CLOSURE ANYWHERE is
miscompiled. Filed as `todo/t1055` with four durable repros under
`known_gaps/sh_indirect_callee_name_decode*.gg`. Family 2 is therefore **not**
claimed to match on the self-host lane; the disposition is per-cell.

Reproduce either family with:

```
tests/fixtures/self_host_lowerer/driver <fixture>.gg lib --emit-c \
    --runtime-dir=src/backend/c/runtime > /tmp/x.c
cc -O0 -w -o /tmp/x /tmp/x.c -lm -lpthread && /tmp/x
```

Every expected string was adjudicated against ggdef, except
`closure_arg_user_method_named_call_trait_equip.gg` (`item kind trait is outside
the phase-0 subset`; its oracles are the self-host lane and its non-trait twin)
and `extern_symbol_*.gg` (`item kind other`; those assert a BUILD outcome, not
stdout).

⚠ Family 2's cells use BLOCK bodies (`int f(int a):` then an indented `return`)
rather than expression bodies, deliberately: ggdef's phase-0 subset rejects an
expression body, and a cell ggdef cannot run is a cell with no oracle.

## Moving them top-level

Legitimate the moment either condition holds:

1. **`todo/t0953`'s PLAIN-CALL cell (cell B) lands** — the leak goes away and
   there is nothing to admit. ⚠ Sharpened 2026-09-05: this used to read
   "`todo/t0953` lands", and R50 Track E landed the item's builtin-HOF cell
   without moving these fixtures one byte. The condition is cell B, which is
   gated on `todo/t1349`; or
2. **the owner's ruling on Track A1-M's pending allowlist ask admits rows of
   this shape** — then the move is the CORRECT placement, because it buys back
   `runtime_parity_corpus` coverage of the self-host lane at the price of six
   rows that document already-existing debt.

Whoever moves them owes: one `⚖ ADMITTED` row in `LEAK_ALLOWLIST.txt` per
moved fixture that leaks, citing `todo/t0953`; deletion of the
`closure_identity` row in `CORPUS_MANIFEST.txt`; and the `closure_identity/`
path prefix removed from every `run_gg` call.

⚠ Family 2 has a SECOND reason to stay put, independent of the leak: moving it
top-level puts it in `runtime_parity_corpus`, which gates the SELF-HOST lane —
and the self-host miscompiles those cells (`todo/t1055`). Moving them before
`t1055` closes reds the parity gate on a gap that is already filed. The
`collide_no_closure*` and `collide_slot_id_not_arity` cells do not pass a
closure literal at all and so do not leak; they are still held here, with the
rest of their family, for that reason.
