#!/usr/bin/env bash
# Reproduces the LLVM-backend FFI-only-module typedef bug (and a related
# C-backend construction defect). Uses `gg` from PATH; override with GG=/path/to/gg.
set -u
GG="${GG:-gg}"
cd "$(dirname "$0")"

hr() { printf '%s\n' "------------------------------------------------------------"; }
run() { echo "\$ $*"; "$@" 2>&1 | grep -iE "unknown type|expected expression|compilation failed|Built:|error:" | grep -v "cc1:" | head -6; }

hr; echo "[1] mini.gg  — struct from FFI-only module, by value"
echo "    EXPECT: C backend Built OK  /  LLVM backend: unknown type name 'GorgetSDLEvent'"; hr
run "$GG" build                mini.gg
echo
run "$GG" build --backend=llvm mini.gg

echo; hr; echo "[2] ctor_c_backend.gg — direct ctor of an FFI-only-module struct (C backend)"
echo "    EXPECT: C backend: expected expression before ')' token"; hr
run "$GG" build                ctor_c_backend.gg

echo; hr; echo "[3] control_ok.gg — Vec3 (module HAS method bodies): passes on both"
echo "    EXPECT: both backends Built OK"; hr
run "$GG" build                control_ok.gg
echo
run "$GG" build --backend=llvm control_ok.gg
hr
