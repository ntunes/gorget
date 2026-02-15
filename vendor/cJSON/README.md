# Vendored cJSON

Source: https://github.com/DaveGamble/cJSON
License: MIT (see LICENSE)

## Files

- `cJSON.h` — header (unmodified from upstream)
- `cJSON.c` — implementation (unmodified from upstream)

## How it's used

Both files are embedded into the Gorget compiler via `include_str!` in
`src/codegen/c_runtime.rs` and inlined directly into the generated C output.

At emit time (`src/codegen/mod.rs`), the `#include "cJSON.h"` line in cJSON.c
is replaced with a comment, since the header is already inlined above it.
No source patches are applied — the vendored files are byte-for-byte identical
to upstream.

## Updating

To resync with upstream:

```
curl -o vendor/cJSON/cJSON.h https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.h
curl -o vendor/cJSON/cJSON.c https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.c
```

No patches to reapply. Just verify `cJSON.c` still contains `#include "cJSON.h"`
(the runtime replacement depends on this).
