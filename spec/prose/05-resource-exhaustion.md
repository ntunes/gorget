# Resource exhaustion is implementation-defined

> **STUB (phase 0).** Rule stated; full prose is phase 1.

**Rule (RFC §2.2, bullet 5).** **Resource exhaustion — stack depth, OOM — is
implementation-defined** and outside output-comparison conformance (places ledger
C11: unbounded recursion → OS-guard SIGSEGV, accepted by design). `ggdef` is
**total via fuel** and never exhausts a real stack: every step decrements a fuel
counter, and reaching the bound is the distinct, swept **`FuelExhausted`**
outcome (RFC §2.3) — never confused with a program's meaning, never undefined
behavior.

This is what lets the definition be **total enough to mechanize** (RFC §2, D3):
`eval(fuel, state, node)` always returns one of the four outcomes.

<!-- cites: eval.rs::run -->
<!-- cites: eval.rs::State::tick -->

**Related:** decisions.md C11 (unbounded recursion → OS-guard SIGSEGV accepted by
design); RFC §2.3 (the four total outcomes) and §2.7 (fuel-indexed eval).
