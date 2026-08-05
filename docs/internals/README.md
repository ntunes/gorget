# Design notes — ruled but not yet built

**This is not the contributor book.** The contributor book is
[`docs/devbook/`](../devbook/README.md): it describes what the compiler does
*today*, in present tense, verifiable against HEAD.

This directory holds the other thing — **designed-but-unbuilt** material that
the devbook's genre cannot carry: owner rulings on work not yet in the tree,
rejected-alternative records, and deferred designs. A file here describes what
has been *decided*, not what the compiler *does*.

Normative language rulings live in
[`docs/define-gorget/decisions.md`](../define-gorget/decisions.md). A design
note may elaborate a ruling; it never *is* the ruling.

## Status vocabulary

Every file opens with one status. There is deliberately **no `SHIPPED` status**:
once the work lands, the content belongs in a devbook chapter and the design
note is deleted. That rule is what keeps this directory from re-accumulating the
superseded tree it used to be.

| status | meaning |
|---|---|
| `RATIFIED-UNBUILT` | an owner has ruled; no code exists yet |
| `PROPOSED` | a design under discussion; not ruled |
| `IN-PROGRESS` | partially landed; the note tracks the remainder |

## Contents

| file | status | covers |
|---|---|---|
| [cow-transient-view-model.md](cow-transient-view-model.md) | `RATIFIED-UNBUILT` | The *legality* axis of CoW: place-gate, typed builtin views, transient/unstorable views, closures as the user mutate-through path. Elaborates **D41** (no stored borrows, no user-visible `Ref[T]`). |
| [cow-cost-contract.md](cow-cost-contract.md) | `RATIFIED-UNBUILT` | The *cost* axis of CoW: the per-signature ownership summary, arg- and return-boundary elision, the guaranteed-elision set. Elaborates **D42** (the `implicit_clones` knob). |
| [wasm-backend.md](wasm-backend.md) | `PROPOSED` | Two unreconciled routes to a WASM target — a native backend with CFG restructuring, or WASM as an LLVM target triple — and what each still owes. |

## What used to be here

This directory once held twenty-odd deep-dives. They were folded into devbook
chapters as those chapters reached reference quality, and the originals are now
retired — the fold is the point, not an accident. Nothing was lost: shipped
behaviour lives in the chapter that absorbed it, ratified rulings live in the
decision ledger, and unbuilt work is filed in `TODO.md` with its findings
inline. Git history has the originals if you need to see how a design was
argued at the time.

## Conventions

- **A design note is not a plan.** Round-scoped scouts, briefs, and censuses are
  `/tmp`-only and are never committed (`docs/plans/` is retired and lint-guarded).
  What lives here is durable design, not the paperwork of a round.
- **No numbers.** A measured figure in a design note is a stale premise with a
  fuse on it. Record the command that regenerates it instead.
- **Cite source, don't transcribe it.** A `file:line` citation is checkable —
  and `doc_source_citations_resolve` checks it. A copied code block silently
  rots.
- **When it ships, delete it.** Move the content into the chapter that now owns
  the behaviour, and remove the note. A design note that survives its own
  implementation is the fossil this directory was cleaned out to prevent.
