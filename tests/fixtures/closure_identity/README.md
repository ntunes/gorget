# `closure_identity/` — why these six live in a subdirectory

These are ordinary, PASSING regression fixtures, wired as live `run_gg` tests in
`tests/integration.rs`. Nothing here is a known gap. They pin the R49 Track
A1-IDENTITY miscompile: a user method named `call` mangles to `Runner__call`, so
the LIR call lowering's `!func.contains("__call")` matched it and left its
closure argument unpacked — a stack-buffer-overflow on a capture-less closure,
and a plausible WRONG NUMBER with a clean exit code, silent under ASan and
UBSan, on a closure whose first capture is itself a `Callable`. Closure identity
is now typed metadata (`ir::Function::takes_env`,
`TypeMetadata::closure_call_fn`, `StructDef::closure_call_fn`).

They assert **stdout**, not exit codes. A fix validated on `rc != 139` greens
the two loud cells and leaves the silent one live.

## Why not top-level

Every one of them passes a closure LITERAL at a CALL-ARGUMENT position, because
that is the shape the defect needs — and that shape leaks its environment
through `__gorget_closure_env_alloc`. **Measured identically on the PRE-FIX
compiler** (32 bytes in 1 allocation for the free-function control), so it is
pre-existing debt owned by `todo/t0953`, the single largest class in
`tests/sanitize/LEAK_ALLOWLIST.txt` — not inflow from the change these fixtures
pin.

Top-level `tests/fixtures/*.gg` is what `scripts/sanitize_sweep.sh` sweeps, so
landing them there would admit six NEW rows to that allowlist for a PRE-EXISTING
class. The list is shrink-only and its new-inflow case is an explicit owner ask.
`tests/sanitize/CORPUS_MANIFEST.txt`'s `closure_identity` row carries the same
reasoning and the condition that retires it.

## ⚠ WHAT THAT COSTS, so a future reader knows what they gain and what they owe

Out of the top-level scan is also out of **`runtime_parity_corpus`**. The
self-host lane measurement below is therefore a MEASUREMENT TAKEN WHEN THESE
LANDED, **not a continuously enforced gate — nothing will notice if a later
change breaks it.** The C and LLVM lanes stay continuously pinned by the
`run_gg` tests; only the self-host lane is uncovered.

Measured 2026-09-04: all six COMPILE, RUN and MATCH on the self-host lowerer
lane, including the five Rust gg got wrong — this is the succession plan's
"reference lags the self-host" case. Reproduce with:

```
tests/fixtures/self_host_lowerer/driver <fixture>.gg lib --emit-c \
    --runtime-dir=src/backend/c/runtime > /tmp/x.c
cc -O0 -w -o /tmp/x /tmp/x.c -lm -lpthread && /tmp/x
```

Every expected string was adjudicated against ggdef, except
`closure_arg_user_method_named_call_trait_equip.gg` — `item kind trait is
outside the phase-0 subset`. Its oracles are the self-host lane and its
non-trait twin.

## Moving them top-level

Legitimate the moment either condition holds:

1. **`todo/t0953` lands** — the leak goes away and there is nothing to admit; or
2. **the owner's ruling on Track A1-M's pending allowlist ask admits rows of
   this shape** — then the move is the CORRECT placement, because it buys back
   `runtime_parity_corpus` coverage of the self-host lane at the price of six
   rows that document already-existing debt.

Whoever moves them owes: six `⚖ ADMITTED` rows in `LEAK_ALLOWLIST.txt` citing
`todo/t0953`, deletion of the `closure_identity` row in `CORPUS_MANIFEST.txt`,
and the `closure_identity/` path prefix removed from the six `run_gg` calls.
