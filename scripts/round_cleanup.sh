#!/usr/bin/env bash
# Round-close cleanup: agent worktrees, gg build-scratch, stray stashes.
# Implements AGENTS.md "Multi-agent orchestration" rule 6, which used to carry
# these commands as literal text for each orchestrator to retype. Unpruned
# scratch has filled the disk and killed a session mid-task (devbook/30 §7).
#
# CAPTURE FIRST, PRUNE SECOND is the rule's load-bearing half: committed work
# survives `git worktree remove`, UNCOMMITTED work does not. So this script
# DRY-RUNS by default — it prints what it would remove and what it would
# capture, and changes nothing until you pass --yes.
#
# Usage:
#   scripts/round_cleanup.sh          # dry run (default): report only
#   scripts/round_cleanup.sh --yes    # actually capture + prune
#
# Never touched: /tmp/recover_*  (the capture files themselves), anything
# modified today, and the long-lived dev worktrees (only `agent-*` is pruned).

set -uo pipefail
cd "$(dirname "$0")/.."

DO_IT=0
[ "${1:-}" = "--yes" ] && DO_IT=1
run() { if [ "$DO_IT" = 1 ]; then eval "$@"; else echo "    would run: $*"; fi; }
say() { printf '%s\n' "$*"; }

say "── (a) capture uncommitted agent-worktree work ───────────────────────"
wts=$(git worktree list --porcelain | awk '/^worktree /{print $2}' | grep '/agent-' || true)
if [ -z "$wts" ]; then
  say "    no agent-* worktrees"
else
  for wt in $wts; do
    if [ -n "$(git -C "$wt" status --porcelain 2>/dev/null)" ]; then
      out="/tmp/recover_$(basename "$wt").patch"
      say "  ! $wt has UNCOMMITTED work → capturing to $out"
      run "git -C '$wt' diff > '$out'"
    else
      say "    $wt clean"
    fi
  done
fi

say "── (b) prune agent-* worktrees (long-lived dev worktrees untouched) ──"
for wt in $wts; do
  run "git worktree remove --force '$wt'"
done
run "git worktree prune"

say "── (c) clear stale gg build-scratch (>1 day old, never recover_*) ────"
run "find /tmp /tmp/claude-1000 -maxdepth 1 \\( -name 'tmp.*' -o -name '.tmp*' -o -name 'clone_attr.*' -o -name 'bench_stages.*' \\) -type d -mtime +1 -exec rm -rf {} +"
# Named per-track scratch. clone_attr.*/bench_stages.* above are the measurement
# scripts' work dirs — hundreds of MB each, deliberately kept until stale.
for pat in '/tmp/gg_*' '/tmp/sh_*' '/tmp/gg_runtime_diff_*'; do
  run "find $pat -maxdepth 0 -type d -mtime +1 -exec rm -rf {} + 2>/dev/null"
done

say "── (d) capture + clear stray stashes (the stack is repo-GLOBAL) ──────"
n=$(git stash list | wc -l | tr -d ' ')
if [ "$n" -eq 0 ]; then
  say "    stash stack empty (as it should be — agents NEVER stash, owner 2026-07-03)"
else
  say "  ! $n stash entr(y|ies) present — agents must never stash; capturing all"
  for i in $(seq 0 $((n - 1))); do
    run "git stash show -p 'stash@{$i}' > /tmp/recover_stash_$i.patch"
  done
  run "git stash clear"
fi

say "── verify ────────────────────────────────────────────────────────────"
df -h / | tail -1
say "worktrees: $(git worktree list | wc -l | tr -d ' ')   stashes: $(git stash list | wc -l | tr -d ' ')"
[ "$DO_IT" = 1 ] || say "(DRY RUN — nothing changed. Re-run with --yes to apply.)"
