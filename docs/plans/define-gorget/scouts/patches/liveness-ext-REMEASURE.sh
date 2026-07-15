# On pristine main: apply patch, build driver, copy to private path, measure blast + fixtures.
GG=target/release/gg ; PRIV=/tmp/live_ext_re
mkdir -p $PRIV
git apply docs/plans/define-gorget/scouts/patches/<this>.patch   # or proto_core_A_B.patch
cargo build --release
GG_BUILD_TIMEOUT_SECS=900 $GG build tests/fixtures/self_host_lowerer/driver.gg -o $PRIV/driver
# blast (must be 0):
$PRIV/driver tests/fixtures/self_host_lowerer/driver.gg lib --lir-c > $PRIV/all.c 2> $PRIV/all.err; wc -c $PRIV/all.err
# d10b fixtures:
for f in writer_writer_reject writer_subfield_reject read_move_reject move_noncopyread_reject double_move_reject disjoint_siblings_accept writer_copyread_accept; do
  $PRIV/driver tests/fixtures/d10b_place_overlap/$f.gg lib --lir-c >/dev/null 2>$PRIV/d_$f.err; echo "$f: $(grep -aoE 'their places overlap|after it was moved|moved more than once' $PRIV/d_$f.err | head -1) exit=$?"; done
