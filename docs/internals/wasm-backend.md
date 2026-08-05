# A WASM backend — two routes, not yet chosen

> **Status: `PROPOSED`** — no owner ruling, no code. `src/backend/wasm/` does
> not exist and `--backend=wasm` is not a valid value (see the caveat below).
> This note exists because the design was recorded in two now-deleted planning
> docs that disagreed with each other, and the fork is worth keeping.

WASM is a stated target: `docs/language-design.md` lists browser and edge
deployment among the goals, and `meta.arch()` already enumerates `"wasm32"` in
its documented return set. Nothing implements it.

## The fork

Two routes were proposed independently, in separate documents, and never
reconciled. They are not variations on one plan — they put the WASM work in
different halves of the compiler.

**Route A — a native WASM backend, peer to C and LLVM.** A `src/backend/wasm/`
that consumes BIR directly: `LirType` → WASM value types, `Slot` → WASM locals
or linear-memory offsets, `Inst` → WASM instructions, plus a **Relooper or
Stackifier pass** to restructure the CFG into WASM's structured control flow.
Reached by `gg build --backend=wasm`.

**Route B — WASM as an LLVM target triple.** No new backend at all: emit LLVM
IR as today and hand `wasm32-unknown-unknown` to the existing toolchain, i.e.
`gg build --target=wasm32-…`.

The trade is the usual one and neither doc argued it: Route B is a fraction of
the work and inherits LLVM's optimizer, but it makes WASM support conditional
on the LLVM lane and on an external toolchain, which is the opposite of the
hermetic direction. Route A owns the whole path and can exploit LIR structure
directly, at the cost of writing a code generator and a CFG restructurer.

Choosing between them is the first decision this work needs.

## What already holds, either way

The LIR was shaped with WASM in mind, so several prerequisites are shipped
rather than pending:

- **Reducible control flow is guaranteed, not hoped for.** `check_reducible_cfg`
  and `check_no_critical_edges` are enforced validators (see
  [devbook/25 — Structural guards](../devbook/25-structural-guards.md)). This is
  the single most important precondition: an irreducible CFG cannot be emitted
  as WASM without a relooper, and a relooper over arbitrary CFGs is its own
  correctness hazard. The validators mean Route A's restructuring pass starts
  from a reducible graph.
- **`LirType::FuncRef` exists** (`src/lir/mod.rs`), deliberately distinct from
  `Ptr`, precisely so a WASM backend can lower it to a **table index** with
  `call_indirect` rather than a raw pointer. Chapter 14 records the rationale.
- **Per-value type precision.** WASM's linear-memory loads need exact widths
  (`i32.load8_u` vs `i32.load`); "infer from context" does not work there. The
  typed per-value information the LIR carries is what makes either route
  tractable.

## What each route still owes

- **The LIR optimizer becomes load-bearing.** Cross-block constant propagation,
  GVN and LICM are currently deferred on the reasoning that `clang -O2` and
  LLVM do that work downstream. That reasoning holds for Route B and **fails for
  Route A**, which has no downstream optimizer at all. (It equally fails for any
  other non-LLVM native backend, so this cost is not WASM-specific.)
- **The runtime.** The C runtime cannot be linked into a WASM module as-is.
  Route A needs the runtime surface expressed as `(import …)` declarations
  against the host — which is the same typed-signature problem the runtime ABI
  contract already solves for `extern` declarations, and a reason to drive it
  from the typed registry rather than a second hand-written list.
- **Host boundary semantics.** Neither route has a story yet for the parts of
  the stdlib that assume a POSIX-shaped host (files, sockets, threads). The
  hermetic-core-versus-extended split that a native backend needs applies here
  too, and probably more sharply.

## The flag is reserved, not accepted

`--backend=wasm` is **rejected** with a diagnostic naming the accepted set
(`c`, `c-lir`, `llvm`). It did not always: the dispatch matches `"llvm"` and
falls through to the C backend for everything else, so before the parse-time
check an unrecognised value silently produced a C binary and reported success.
Route A's first milestone is therefore an honest one — add `wasm` to the
accepted set at the same time as a dispatch arm, which
`backend_flag_set_matches_dispatch` requires anyway.
