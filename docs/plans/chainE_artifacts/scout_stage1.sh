#!/bin/bash
# SCOUT helper (Chain E): build a stage-1 binary from a self-host-emitted C body
# and run it on driver.gg. Usage:
#   scout_stage1.sh <body.c> <tag> [env for stage-1 run, e.g. GG_COW_LAZY=1]
# Produces /tmp/stage1_<tag>.c, /tmp/stage1_<tag> and runs it on driver.gg,
# reporting rc + timing. Preamble comes from the Rust-built driver.c.
set -u
WT=/workspace/gorget/.claude/worktrees/agent-a07fcfddc870f61c5
D=$WT/tests/fixtures/self_host_lowerer
BODY=$1
TAG=$2
shift 2
PRE=/tmp/runtime_preamble.c
if [ ! -f $PRE ]; then
  # everything before the first user-type typedef (mirrors tests/integration.rs:14079)
  awk '/^typedef struct __gg_/{exit} {print}' $D/driver.c > $PRE
fi
cat $PRE "$BODY" > /tmp/stage1_$TAG.c
t0=$(date +%s)
cc -O0 -w -o /tmp/stage1_$TAG /tmp/stage1_$TAG.c -lm -lpthread 2>/tmp/stage1_${TAG}_cc.log
rc=$?
echo "cc rc=$rc secs=$(($(date +%s)-t0))"
[ $rc -ne 0 ] && { tail -5 /tmp/stage1_${TAG}_cc.log; exit 1; }
t0=$(date +%s)
env "$@" /tmp/stage1_$TAG $D/driver.gg $WT/lib --lir-c > /tmp/stage2_$TAG.c 2>/tmp/stage2_${TAG}_err.log
rc=$?
echo "stage1-run rc=$rc secs=$(($(date +%s)-t0)) stage2_bytes=$(wc -c < /tmp/stage2_$TAG.c)"
[ -s /tmp/stage2_${TAG}_err.log ] && tail -3 /tmp/stage2_${TAG}_err.log
exit $rc
