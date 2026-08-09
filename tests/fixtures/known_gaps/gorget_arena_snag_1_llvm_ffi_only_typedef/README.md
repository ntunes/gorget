# Gorget — LLVM backend drops the C typedef for FFI-only-module structs

Minimal, self-contained reproducer for a Gorget compiler bug.

**Compiler:** `gorget` HEAD `913389a7` (`0.1.4-alpha`), Linux x86-64
**Backends:** C backend ✅ · LLVM backend (`--backend=llvm`) ❌

## TL;DR

A plain Gorget `struct` defined in an **FFI-only module** — a module that holds
`struct` defs + an `extern "C"` block and **no Gorget-body functions** (e.g.
`xtd.sdl`'s `SDLEvent`, `xtd.audio`'s `AudioMusic`) — never gets its C typedef
(`GorgetSDLEvent`) emitted unless a *referenced extern's signature* forces it in.

- **LLVM backend:** the generated runtime shim (`__gorget_runtime_*.c`)
  forward-declares functions that take such a struct by value but never emits
  the typedef → `error: unknown type name 'GorgetSDLEvent'`. Happens even when
  an extern *is* referenced (the shim doesn't inherit the typedef).
- **C backend:** works *iff* a referenced extern pulls the typedef in.
  Construct the struct directly in Gorget (no extern referenced) and the C
  backend fails the same way.

## Run

```bash
# gg on PATH:
./run.sh
# or point at a specific build:
GG=/path/to/gorget/target/release/gg ./run.sh
```

## Files & expected results

| File                  | C backend                              | LLVM backend                          |
|-----------------------|----------------------------------------|---------------------------------------|
| `mini.gg`             | ✅ `Built:`                            | ❌ `unknown type name 'GorgetSDLEvent'` |
| `ctor_c_backend.gg`   | ❌ `unknown type name` + `expected expression before ')'` | (same root; C shown) |
| `control_ok.gg`       | ✅ `Built:`                            | ✅ `Built:`                            |

- **`mini.gg`** — the headline bug. `SDLEvent` (from FFI-only `xtd.sdl`) obtained
  via the `sdl_poll_event()` extern and passed **by value** to a user function.
  The C backend builds+links it (the extern reference pulls the typedef in); the
  LLVM shim does not, so it fails to compile. The LLVM error is raised at
  **C-compile of the shim, before linking**, so it reproduces with or without
  SDL installed.

- **`ctor_c_backend.gg`** — same struct, but **constructed directly** in Gorget
  and no extern referenced. Now the C backend also has no path that emits the
  typedef → `unknown type name 'GorgetSDLEvent'` followed by a malformed
  constructor expression (`expected expression before ')'`). Same root cause,
  surfacing on the C backend.

- **`control_ok.gg`** — control that passes on **both** backends. `Vec3` has the
  identical by-value shape, but its module (`xtd.math3d`) carries real
  Gorget-body methods, so its typedef is always gathered. This isolates the
  trigger to *"the defining module contributes no lowered function bodies"*,
  not *"library module"* or *"passed by value"* on their own.

## Likely root cause

Typedef emission for an FFI-only-module struct is driven only by referenced
`extern` signatures (C backend) and is not propagated into the LLVM backend's
runtime C shim at all. The fix is to emit the typedef for any struct actually
used in emitted code — shim signatures included — independent of whether its
defining module emits function bodies or whether an extern happens to reference
it.

## Note

`SDLEvent`/`AudioMusic` are ordinary Gorget structs (scalar `int`/`float`
fields), not opaque C handles — so full struct emission is correct and expected.
Real-world impact: any program that builds on the LLVM backend and passes such a
struct by value (e.g. the Gorget Arena game, which routes SDL input events
through `input_process_event(InputState &, SDLEvent)`) fails to compile on
`--backend=llvm`. The default C backend is unaffected.
