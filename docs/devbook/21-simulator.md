# Chapter 21 — The simulator / interpreter (`gg sim`)

The simulator is a **tree-walking interpreter over GIR** — it executes a lowered
`crate::ir::Module` directly, instruction by instruction, without ever invoking
the C/LLVM backend or a C compiler. It lives in `src/sim/` (~10K LOC across
`mod.rs`, `dispatch.rs`, `runtime.rs`, `value.rs`, `error.rs`, `config.rs`,
`source_loc.rs`, `crypto.rs`) and is reached through the `gg sim` subcommand.

Its self-described model is "analogous to Rust's miri (which interprets MIR)"
(`src/sim/mod.rs:6`): a reference oracle for testing backends, a fast iteration
path that skips C compilation, and a foundation for undefined-behavior detection
(`src/sim/mod.rs:6-9`). It is **not** a production execution path — anything it
can't model returns `SimError::Unimplemented` with the hint "use `gg run` to
compile and execute natively" (`src/sim/error.rs:51-53`).

This chapter describes how the interpreter executes GIR, how interpreted
execution differs from compiled execution, the optional UB checks, and the
`tests/sim_ub/` differential harness.

---

## Where it sits in the pipeline

`gg sim` runs the full front end and lowering, then branches into the
interpreter instead of the backend. The command handler in `main.rs` does
exactly the same lex → parse → import-load → `semantic::analyze_with_source_dir`
→ `ir::lowering::lower_module` sequence as `gg build`/`gg run`, then calls
`gorget::sim::interpret(&gir_module, filename, &sim_config)` and exits with its
return code (`src/main.rs:3297-3396`). Two facts matter for understanding the
interpreter:

- **It consumes GIR, not LIR.** The interpreter walks `Module::functions`,
  `Block::instructions`, and `Terminator`s from `crate::ir::instructions`
  (`src/sim/dispatch.rs:6`). SSA, drop elaboration, and the LIR layer never run —
  this is GIR semantics, including GIR's `Drop`/`DropIfAlive`/`Dealloc`
  instructions, executed directly.
- **The source filename and concatenated source are attached to the module**
  before interpreting (`src/main.rs:3392-3393`) so error reporting can show
  source snippets (see [Error reporting and backtraces](#error-reporting-and-backtraces)).

The entry point `interpret()` dispatches on what the module contains, in order:
`--many-seeds` (`src/sim/mod.rs:33`), bench functions (`bench_fns`,
`src/sim/mod.rs:51`), test functions / test module (`test_fns`,
`src/sim/mod.rs:56`), hot-reload modules (`init`/`tick`/`reload`,
`src/sim/mod.rs:61`), otherwise plain `main()` (`src/sim/mod.rs:66-72`).

---

## The `Value` model

The interpreter does not work on bytes or a flat stack; it works on a tagged
Rust enum `Value` (`src/sim/value.rs:6-48`). Each Gorget runtime kind maps to a
variant: the scalar ladder (`I8`..`U64`, `F32`/`F64`, `Bool`, `Char`), the three
string flavours (`Str`/`String`/`CStr`), `Struct`/`Enum`/`Tuple`, `FuncRef`, the
pointer family (`Ptr`/`MutPtr`/`Ref`/`Null`), and the two collections
(`Array`/`Dict`).

A few design choices shape everything downstream:

- **Strings are real Rust buffers, not C layouts.** `SimStr` is a shared
  `Rc<Vec<u8>>` view with `start`/`len` (`src/sim/value.rs:52-57`); `SimString`
  is an owned `Vec<u8>` plus a `cap` field that "mirrors GorgetString.cap"
  (`src/sim/value.rs:95-100`). The interpreter does not reproduce the 8-byte
  thin-pointer / 24-byte-header layout of the production runtime — it just holds
  the bytes.
- **Collections are ref-counted and interior-mutable.** `SimArray` wraps
  `Rc<RefCell<Vec<Value>>>` (`src/sim/value.rs:143-144`) and `SimDict` wraps
  `Rc<RefCell<Vec<(Value,Value)>>>` (`src/sim/value.rs:204`). Cloning a handle is
  cheap and *shares* the backing store — mutations are visible through every
  alias. `SimDict` is an **ordered** dict backed by a `Vec` with linear-scan
  lookup via `values_equal` (`src/sim/value.rs:216-229`), preserving insertion
  order; it is not the production hashmap.
- **`Value` carries no static type.** Coercion is done on demand by the
  `as_bool`/`as_i64`/`as_u64`/`as_f64`/`as_char` accessors
  (`src/sim/value.rs:299-434`), which are deliberately *lenient* — e.g. `as_i64`
  returns `0` for `Unit`/`Struct`/`Tuple` rather than panicking
  (`src/sim/value.rs:349-356`) so that an unimplemented runtime function
  returning `Unit` doesn't crash the whole program. This leniency is a recurring
  divergence from compiled code (see below).
- **Zero values are synthesised from the GIR type registry.**
  `Value::zero_for_type(type_id, registry)` (`src/sim/value.rs:513-566`) walks
  the `TypeRegistry`, recursing into struct/enum/alias defs, and falls back to
  name heuristics (`Vector__*` → `Array`, `Dict__*`/`HashMap__*` → `Dict`) for
  named types without a registered def (`src/sim/value.rs:548-559`).

`values_equal` (`src/sim/value.rs:272-295`) provides structural equality used by
the collections, including cross-width integer and cross-flavour string
comparison.

---

## The execution loop

The heart of the interpreter is `Interpreter::call_function` in `dispatch.rs`.
The `Interpreter` struct (`src/sim/dispatch.rs:212-274`) holds the borrowed
`Module`, a `HashMap<usize, Value>` heap with a `heap_next` bump counter, the
global table, captured `stdout`/`stderr` byte buffers, the call stack, plus a
large amount of simulated-subsystem state (tracking allocators, task results,
TCP/UDP sockets, multicast inboxes, compiled regexes, `Shared`/`Weak` refcounts).
Output is buffered into `self.stdout`/`self.stderr` and only flushed to the real
streams at exit / between tests (`src/sim/mod.rs:510-519`) — this is what lets
the test runner interleave per-test result lines cleanly.

`call_function(name, args, depth)` (`src/sim/dispatch.rs:1409`) does, in order:

1. **Depth guard.** If `depth > MAX_DEPTH` (500, `src/sim/dispatch.rs:45`) it
   returns `SimError::StackOverflow` — the interpreter recurses on the Rust call
   stack, so this caps it well below a real overflow.
2. **Name-prefix special cases for concurrency/IO.** Before consulting the
   module, a long ladder of `if name == ...` / `name.strip_prefix(...)` handlers
   intercepts the runtime symbols that the interpreter models specially:
   `__gorget_spawn_FN` runs `FN` *synchronously* and stashes the result under a
   task id (`src/sim/dispatch.rs:1417-1423`); `__gorget_await_FN` retrieves it
   (`:1425-1428`); `gorget_task_group_*`, `__gorget_thread_spawn_FN`
   (eager, sequential, `:1462-1474`), and the TCP/UDP socket family
   (starting at `socket_connect`, `:1481`, through the UDP handlers ending at
   `gorget_udp_set_timeout`, `:1795-1807`) all live here. **All concurrency is collapsed to
   single-threaded eager evaluation** — there is no scheduler, no preemption, no
   real parallelism.
3. **User function execution.** If `module.find_function(name)` succeeds
   (`src/sim/dispatch.rs:2101`), the function body is cloned and run:
   - Locals are a `Vec<Value>` sized to `func.locals.len().max(num_args + 1)`,
     with `_0` reserved as the return slot and `_1..N` filled from the arguments
     (`src/sim/dispatch.rs:2117-2124`).
   - A `StackFrame` is pushed for backtraces (`src/sim/dispatch.rs:2109-2115`).
   - The block loop (`src/sim/dispatch.rs:2151-2269`) keeps a `current_block`
     index, runs every `Instruction` in the block via `execute_instruction`,
     then dispatches on the `Terminator`: `Return` evaluates the operand and
     returns; `Jump` sets the next block; `Branch` evaluates the condition with
     `as_bool`; `Switch` matches on `as_i64`; `Invoke` is simplified to a plain
     call-then-jump-to-normal (the error edge is **not** modeled,
     `src/sim/dispatch.rs:2241-2261`); `Unreachable`/`None` raise errors.
4. **Fallback to runtime/collection/string dispatch** for any name not found in
   the module (`src/sim/dispatch.rs:2270-2278` onward): mutating string ops,
   then `try_collection_dispatch`, then `runtime::call_extern`.

`execute_instruction` (`src/sim/dispatch.rs:799-1403`) is the per-instruction
switch: `Assign`, `FieldLoad`/`IndexLoad`, the arithmetic/cmp/cast family,
`StructInit`/`EnumInit`/`TupleInit`, `Borrow`/`BorrowMut`, `HeapAlloc`/`Dealloc`,
`Drop`/`DropIfAlive`, the three `Call*` variants, and `InlineC`. The destination
local of each instruction is marked initialized at the end via
`mark_instruction_dst` (`src/sim/dispatch.rs:54-88`, called at `:1400-1402`).

### Pointers, borrows and the heap

There is no real memory; pointers are integer indices into the `heap` HashMap
(`src/sim/dispatch.rs:215`, `heap_alloc` at `:357-368`, `heap_next` starts at 1
so 0 is null). `BorrowMut` on a bare local "promotes" the local to a heap slot
and replaces it in-place with `Value::Ref(addr)` (`get_or_alloc_ref`,
`src/sim/dispatch.rs:403-420`); reads/writes through a `Ref` go transparently
through `heap[addr]` (`src/sim/value.rs:39-41`). These ref-promotion slots are
tagged `is_ref_promoted` so leak detection ignores them
(`src/sim/dispatch.rs:352-355`). `FieldLoad` auto-derefs a pointer base
(`src/sim/dispatch.rs:825-830`), mirroring C's `ptr->field`.

### Runtime functions (`CallExtern`)

`CallExtern` (`src/sim/dispatch.rs:1187`) routes, after argument evaluation and
some auto-deref bookkeeping, to one of three handlers: a user function that the
lowering happened to emit as an extern call (`src/sim/dispatch.rs:1266-1267`),
`try_collection_dispatch`, or `runtime::call_extern`
(`src/sim/runtime.rs:594-601`). `runtime.rs` (~2.3K LOC) is dominated by a giant
`match name { ... }` in `call_extern` (`src/sim/runtime.rs:602-2347`) that
hand-reimplements the C runtime in pure Rust: `printf`/`do_printf`
(`src/sim/runtime.rs:629-642`, with its own format-spec engine at `:305`), string
ops, FNV-1a hashing matching the C `__gorget_fnv1a` (`src/sim/runtime.rs:11-18`),
a splitmix64 PRNG matching the C `__gorget_rng_state` (`src/sim/runtime.rs:72-83`),
path helpers, time, and (in `crypto.rs`) real-but-unhardened SHA-256 etc.
intended only for interpreter use (`src/sim/crypto.rs:1-4`). Any unhandled
`gorget_`-prefixed name returns `SimError::Unimplemented`; non-`gorget_` externs
(C stdlib etc.) silently return `Unit` as a safe default
(`src/sim/runtime.rs:2347-2356`).

> **Maintenance note (no-name-matching rule).** The runtime dispatch is, by its
> nature, a giant string switch on runtime-symbol names — exactly the pattern the
> layering discipline (Chapter on layering) forbids for *semantic* decisions.
> That is tolerable here only because the C-emit boundary *is* the runtime-symbol
> contract: the interpreter is re-implementing the same named ABI the C backend
> spells. It is still a standing source of drift — a new runtime function silently
> becomes `Unimplemented` in the sim until someone adds a `match` arm.

---

## How interpreted execution differs from compiled

The interpreter is an *approximation* of the compiled semantics, deliberately so.
The load-bearing divergences a compiler developer must keep in mind:

- **Concurrency is fake.** `spawn`/`await`, `Thread`, and `TaskGroup` all run
  eagerly on one thread in submission order (`src/sim/dispatch.rs:1417-1474`,
  `:1429-1458`). Data races, ordering bugs, and deadlocks that depend on real
  parallelism cannot be reproduced.
- **String/collection layout is abstracted.** No thin-pointer string, no
  GorgetArray header, no real hashing for `Dict` (it is an ordered `Vec` with
  linear lookup, `src/sim/value.rs:204-229`). Programs that depend on hash
  ordering or on the byte layout of a `String` will behave differently.
- **Coercions are lenient.** `as_i64` returns `0` for non-integer kinds
  (`Unit`/`Struct`/`Tuple`/`FuncRef`, `src/sim/value.rs:349-356`) instead of
  panicking; `as_bool` is similarly lenient (`Unit` ⇒ `false`,
  `src/sim/value.rs:317`) though not a blanket `false` — it has a `panic!`
  catch-all for kinds it can't interpret (`src/sim/value.rs:325`). This keeps the
  interpreter running past gaps but means a type confusion that would miscompile
  or trap natively can silently produce `0` here.
- **Overflow/division semantics are always checked.** Integer add/sub/mul use
  `checked_*` and raise `SimError::Overflow` on overflow
  (`src/sim/dispatch.rs`, the signed/unsigned `BinOp::{Add,Sub,Mul}` arms);
  division by zero is always `SimError::DivisionByZero`. The per-operator
  wrapping ops (`BinOp::{AddWrap,SubWrap,MulWrap}`, emitted by `+%`/`-%`/`*%`)
  use `wrapping_*` instead. There is no global wrap mode — plain `+`/`-`/`*`
  always check.
- **Recursion is bounded at 500 frames** (`MAX_DEPTH`,
  `src/sim/dispatch.rs:45`), much shallower than a native stack.
- **`Invoke`'s error edge is ignored** — exceptions are not propagated through
  the catch block in the interpreter (`src/sim/dispatch.rs:2241-2261`).
- **Globals with runtime initializers are zeroed**, not run
  (`GlobalInit::Extern` → `zero_for_type`, `src/sim/dispatch.rs:449-453`).

When parity *does* hold, that is the point: the interpreter can serve as an
oracle to confirm the backend produces the same observable output.

---

## UB detection (`--ub-checks`)

The interpreter doubles as a lightweight memory-safety checker, gated entirely on
`SimConfig::ub_checks` so that `SimConfig::default()` (checks off,
`src/sim/config.rs:29-40`) has zero overhead. When enabled it tracks
per-allocation `HeapMeta { alive, is_ref_promoted, alloc_fn, allocator_id }`
(`src/sim/dispatch.rs:19-29`) and an `initialized: HashSet<u32>` of locals per
frame. The detectable classes (all in `SimError`, `src/sim/error.rs:27-42`):

- **Use-after-free** — `heap_read`/`heap_write` consult `alive`
  (`src/sim/dispatch.rs:370-397`).
- **Double-free** — `Dealloc` errors if the slot is already dead
  (`src/sim/dispatch.rs:1348-1357`).
- **Uninitialized read** — `Assign` checks the source local is in `initialized`
  (`src/sim/dispatch.rs:805-813`).
- **Invalid bool value** — a `Cast` to `BOOL_TYPE` whose raw value is not 0/1
  (`src/sim/dispatch.rs:994-997`).
- **Invalid enum tag** — a tag outside the type's variant range
  (`src/sim/dispatch.rs:1061-1066`).
- **Memory leak** — at clean exit, `report_leaks` reports heap slots still
  `alive && !is_ref_promoted` and forces a non-zero exit
  (`src/sim/mod.rs:77-82`,`:521-535`). Suppressed by `--ignore-leaks`
  (`config.ignore_leaks`, `src/sim/config.rs:13`).
- **Isolation violation** — see below.

`gg sim test` turns `ub_checks` on by default (`src/main.rs:3387-3389`).

### Isolation mode

By default the interpreter runs in **isolation**: real I/O (sockets, filesystem,
wall-clock time) is blocked. `check_isolation(op)` returns
`SimError::IsolationViolation` while the thread-local `ISOLATION` flag is set
(`src/sim/runtime.rs:50-61`); time is served from a monotonic fake counter
(`next_fake_time_ms`, `src/sim/runtime.rs:63-70`). `--disable-isolation` clears
the flag (`src/sim/config.rs:15`,`:75`). The RNG is a thread-local splitmix64 that
can be pinned with `--seed=N` (`src/sim/mod.rs:39-42`, `src/sim/runtime.rs:37-39`)
for deterministic runs, and `--many-seeds=from..to` re-runs the program across a
seed range to hunt non-determinism, reporting only failures
(`src/sim/mod.rs:88-138`).

### Configuration surface

`SimConfig::from_args` parses flags from both argv and the `GGSIMFLAGS` env var
(`src/sim/config.rs:52-90`): `--seed=N`, `--many-seeds=from..to`,
`--ignore-leaks`, `--disable-isolation`, `--backtrace=0|1|full`, `--ub-checks`.

---

## Error reporting and backtraces

Errors are the `SimError` enum (`src/sim/error.rs:3-42`), whose `Display`
(`:46-93`) already produces the `gorget:`/`gg sim:` message prefixes the CLI
expects. The interpreter maintains a `call_stack` of `StackFrame`s
(`src/sim/dispatch.rs:31-42`) carrying the call span, def span, and
currently-executing instruction span; at the *first* error it snapshots the
backtrace and span (`sim_error_return!`, `src/sim/dispatch.rs:2136-2148`).
`source_loc.rs` then renders a rustc/gorget-style report — `--> file:line:col`, a
source snippet with a caret underline, and `= in '...' / = called from '...'`
frames (`src/sim/source_loc.rs:63-146`), pruned to 3 frames at the default
`--backtrace=pruned` level (`src/sim/config.rs:36`, `src/sim/source_loc.rs:105-145`).
`LineIndex` (`src/sim/source_loc.rs:11-49`) does the byte-offset → line/col
mapping over the source attached to the module.

---

## Test/bench/hot-reload modes

Because the interpreter is fast and needs no C compiler, it can run a module's
test or bench suite directly:

- **`run_test_suite`** (`src/sim/mod.rs:141-319`) honours
  `runtime.test_fns` / `is_test_module`, with `__suite_setup`/`__suite_teardown`,
  per-test `skipped`/`should_panic`/`expected_panic_msg`/`timeout_ms`, prints
  `PASS`/`FAIL`/`SKIP` lines with elapsed ms, and exits non-zero if any fail. On
  failure it appends a source-location report unless `--backtrace=off`.
- **`run_bench_suite`** (`src/sim/mod.rs:322-409`) does a 3-iteration warmup then
  auto-calibrates iteration count until ≥1 s, reporting ns/us/ms/s per iter.
- **`run_hot_reload`** (`src/sim/mod.rs:465-508`) calls `init()`, heap-allocates
  the returned state, and loops `tick(&state)` until it returns `false`.

---

## Role as a differential check: `tests/sim_ub/`

The simulator's UB checker is exercised by a standalone Python harness,
`tests/test_sim_ub.py`, over fixtures in `tests/sim_ub/`. This is **not** wired
into the Rust `tests/integration.rs` harness — it is a separate, manually-run
suite (`python3 tests/test_sim_ub.py [substr]`, `tests/test_sim_ub.py:21`).

The harness runs two fixture modes (`tests/test_sim_ub.py:99-138`):

- **CLEAN** (no `.ub_expected` file): runs
  `gg sim --ub-checks --ignore-leaks <fixture>`, asserts exit 0, stdout matches
  the `.expected` file if present, and that **no** UB keyword
  (`use-after-free`, `double-free`, `uninitialized read`, `invalid bool value`,
  `invalid enum tag`, `tests/test_sim_ub.py:34-40`) appears in stderr. This is
  the differential direction that matters: a program the borrow checker accepted
  and the backend would run cleanly must *also* run clean under the
  interpreter's independent dynamic checks. A divergence is a real signal that
  one of the two analyses is wrong.
- **POSITIVE** (`.ub_expected` present): runs `gg sim --ub-checks <fixture>`
  (no `--ignore-leaks`), asserts a non-zero exit and that the `.ub_expected`
  string is a substring of stderr — confirming the checker actually *catches* the
  injected fault.

As of this writing the fixture set is small — 6 `.gg` files: four `clean_*`
fixtures and two positive ones, `ub_double_free.gg` and `ub_leak.gg` (verify with
`ls tests/sim_ub/`). The harness is the live spec for what "clean" and "caught"
mean; run it to see the current pass count rather than trusting a number here.

---

## REPL

`gg sim` with **no file argument** drops into an interpreter-backed REPL
(`run_sim_tui`, `src/main.rs:1949-2065`). It accumulates definitions and
statements, synthesises a source module, and on `/run` interprets it via the same
`gorget::sim::interpret` path with a default `SimConfig` (`src/main.rs:2047-2064`);
`/check` runs only the front end. It is a convenience shell, not a separate
execution engine.

---

## In the self-host

The Gorget self-host (`tests/fixtures/self_host_*`) covers the lexer, parser,
resolver, type checker, and GIR lowerer — it has **no GIR interpreter**. The
simulator is a Rust-only subsystem with no self-hosted counterpart and no
`*_comparison` parity test. There is nothing to measure here; the gap is simply
that the self-host pipeline stops at lowering and hands off to the (Rust) backend
for execution.
