//! Critical-edge splitting pass for LIR.
//!
//! A *critical edge* is a CFG edge from a block with more than one successor
//! to a block with more than one predecessor. SSA construction (Braun et al.
//! 2013) and structured-control-flow backends (e.g. WASM) both want a CFG
//! that has none of these — without splitting, you can't always place a
//! per-edge action (phi argument resolution, edge-local copies) at a unique
//! program point, because the source block is shared across siblings and the
//! target block is shared across cousins.
//!
//! This pass runs *before* SSA construction and inserts a fresh empty block
//! on every critical edge. The new block has a single unconditional `Jump`
//! to the original target; the source block's terminator is rewritten to
//! point at the new block instead.
//!
//! Post-conditions:
//! - For every CFG edge `s → t`, `succ(s).len() == 1 || pred(t).len() == 1`.
//! - The number of edges in the CFG is unchanged (each split adds one edge
//!   and removes one).
//! - No values, slots, or phis are introduced — the pass runs pre-SSA and
//!   only touches the CFG skeleton.
//!
//! See `docs/devbook/25-structural-guards.md` (the CFG/SSA tier).

use super::*;

/// Split every critical edge in `func` by inserting an empty intermediate
/// block. Pre-SSA pass — runs before `construct_ssa`.
pub fn split_critical_edges(func: &mut LirFunction) {
    if func.blocks.is_empty() {
        return;
    }

    // Compute predecessor counts up-front. A successor of a multi-successor
    // block is "critical" iff it has more than one predecessor.
    let pred_counts = compute_predecessor_counts(func);

    // Snapshot of (source, edge-position, target) for every critical edge,
    // gathered before mutation. Edge-position is opaque — `redirect_edge`
    // uses it to find which slot in the terminator to rewrite.
    let mut criticals: Vec<(BlockId, EdgeSlot, BlockId)> = Vec::new();
    for block in &func.blocks {
        let succs = block.terminator.successors();
        if succs.len() <= 1 {
            continue;
        }
        for (slot, target) in iter_edges(&block.terminator) {
            if pred_counts[target.0 as usize] > 1 {
                criticals.push((block.id, slot, target));
            }
        }
    }

    if criticals.is_empty() {
        return;
    }

    // For each critical edge, allocate a fresh block with a single Jump to
    // the original target, then redirect the source's terminator slot.
    for (src, slot, target) in criticals {
        let new_block = func.add_block();
        // The new block must inherit any block-args the source's terminator
        // was passing on this edge — those args live on the *source*
        // terminator, not on the new block. Pre-SSA there are no block args
        // anyway, but we move them through correctly so the pass is also a
        // no-op idempotent post-SSA (defensive).
        let edge_args = take_edge_args(&mut func.blocks[src.0 as usize].terminator, slot);
        func.blocks[new_block.0 as usize].terminator = Term::Jump(target, edge_args);
        redirect_edge(
            &mut func.blocks[src.0 as usize].terminator,
            slot,
            new_block,
        );
    }
}

/// Module-wide convenience: split critical edges in every function.
pub fn split_critical_edges_module(module: &mut LirModule) {
    for func in &mut module.functions {
        split_critical_edges(func);
    }
}

/// Identifies which outgoing edge of a terminator we mean. Opaque to the
/// caller — produced by `iter_edges` and consumed by `redirect_edge` /
/// `take_edge_args`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EdgeSlot {
    Jump,
    BranchThen,
    BranchElse,
    SwitchCase(usize),
    SwitchDefault,
}

/// Yield `(slot, target)` for every outgoing edge of a terminator.
fn iter_edges(term: &Term) -> Vec<(EdgeSlot, BlockId)> {
    match term {
        Term::Ret(_) | Term::RetVoid | Term::Unreachable => Vec::new(),
        Term::Jump(t, _) => vec![(EdgeSlot::Jump, *t)],
        Term::Branch { then_block, else_block, .. } => {
            vec![
                (EdgeSlot::BranchThen, *then_block),
                (EdgeSlot::BranchElse, *else_block),
            ]
        }
        Term::Switch { cases, default, .. } => {
            let mut v: Vec<(EdgeSlot, BlockId)> = cases
                .iter()
                .enumerate()
                .map(|(i, (_, b, _))| (EdgeSlot::SwitchCase(i), *b))
                .collect();
            v.push((EdgeSlot::SwitchDefault, *default));
            v
        }
    }
}

/// Take the per-edge block arguments off a terminator's slot, replacing
/// them with an empty vector. The new intermediate block re-emits these
/// args on its own jump to the original target.
fn take_edge_args(term: &mut Term, slot: EdgeSlot) -> Vec<ValueId> {
    match (term, slot) {
        (Term::Jump(_, args), EdgeSlot::Jump) => std::mem::take(args),
        (Term::Branch { then_args, .. }, EdgeSlot::BranchThen) => std::mem::take(then_args),
        (Term::Branch { else_args, .. }, EdgeSlot::BranchElse) => std::mem::take(else_args),
        (Term::Switch { cases, .. }, EdgeSlot::SwitchCase(i)) => {
            std::mem::take(&mut cases[i].2)
        }
        (Term::Switch { default_args, .. }, EdgeSlot::SwitchDefault) => {
            std::mem::take(default_args)
        }
        _ => Vec::new(),
    }
}

/// Repoint a terminator's edge slot at `new_target`.
fn redirect_edge(term: &mut Term, slot: EdgeSlot, new_target: BlockId) {
    match (term, slot) {
        (Term::Jump(t, _), EdgeSlot::Jump) => *t = new_target,
        (Term::Branch { then_block, .. }, EdgeSlot::BranchThen) => *then_block = new_target,
        (Term::Branch { else_block, .. }, EdgeSlot::BranchElse) => *else_block = new_target,
        (Term::Switch { cases, .. }, EdgeSlot::SwitchCase(i)) => cases[i].1 = new_target,
        (Term::Switch { default, .. }, EdgeSlot::SwitchDefault) => *default = new_target,
        _ => {}
    }
}

/// Per-block predecessor count for the function's CFG.
fn compute_predecessor_counts(func: &LirFunction) -> Vec<u32> {
    let n = func.blocks.len();
    let mut counts = vec![0u32; n];
    for block in &func.blocks {
        for succ in block.terminator.successors() {
            let i = succ.0 as usize;
            if i < n {
                counts[i] += 1;
            }
        }
    }
    counts
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Construct the canonical critical-edge shape:
    ///
    ///   bb0 (cond branch) ──then──→ bb2 (merge, two preds)
    ///       │                          ▲
    ///       └─── else ── bb1 (jump) ───┘
    ///
    /// Edge bb0 →then→ bb2 is critical: bb0 has two successors, bb2 has two
    /// predecessors. The else edge bb0→bb1 is not critical (bb1 has one pred).
    fn make_critical_edge_func() -> LirFunction {
        let mut func = LirFunction::new("ce".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v_cond = func.next_value();
        func.block_mut(bb0)
            .insts
            .push(Inst::BoolConst { dst: v_cond, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb2,
            then_args: vec![],
            else_block: bb1,
            else_args: vec![],
        };
        func.block_mut(bb1).terminator = Term::Jump(bb2, vec![]);
        func.block_mut(bb2).terminator = Term::RetVoid;
        func
    }

    #[test]
    fn detects_no_criticals_when_clean() {
        // bb0 → bb1 (single succ, single pred) — no critical edges.
        let mut func = LirFunction::new("clean".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![]);
        func.block_mut(bb1).terminator = Term::RetVoid;

        let before = func.blocks.len();
        split_critical_edges(&mut func);
        assert_eq!(func.blocks.len(), before, "no edges should be split");
    }

    #[test]
    fn splits_branch_critical_edge() {
        let mut func = make_critical_edge_func();
        let bb0 = BlockId(0);
        let bb2 = BlockId(2);

        // Sanity: pre-pass, bb0's then_block is bb2 directly.
        if let Term::Branch { then_block, .. } = &func.blocks[0].terminator {
            assert_eq!(*then_block, bb2);
        } else {
            panic!("expected Branch terminator");
        }

        split_critical_edges(&mut func);

        // After split: a new block was inserted; bb0's then_block now points
        // at it, and the new block jumps unconditionally to bb2.
        assert_eq!(func.blocks.len(), 4, "one new block inserted");
        let new_target = match &func.blocks[bb0.0 as usize].terminator {
            Term::Branch { then_block, .. } => *then_block,
            _ => panic!(),
        };
        assert_ne!(new_target, bb2, "then edge should no longer go directly to bb2");
        assert_eq!(new_target, BlockId(3), "new block was appended at index 3");
        match &func.blocks[3].terminator {
            Term::Jump(t, args) => {
                assert_eq!(*t, bb2);
                assert!(args.is_empty());
            }
            _ => panic!("new block should end in Jump(bb2, [])"),
        }

        // The else edge to bb1 is non-critical and untouched.
        if let Term::Branch { else_block, .. } = &func.blocks[bb0.0 as usize].terminator {
            assert_eq!(*else_block, BlockId(1));
        }
    }

    #[test]
    fn split_pass_is_idempotent() {
        let mut func = make_critical_edge_func();
        split_critical_edges(&mut func);
        let after_first = func.blocks.len();
        split_critical_edges(&mut func);
        assert_eq!(func.blocks.len(), after_first, "second pass is a no-op");
    }

    #[test]
    fn switch_critical_edges_split() {
        // bb0 switches into bb1 (one pred — not critical) and bb2 (two preds
        // via fall-through path — critical).
        let mut func = LirFunction::new("sw".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        let v = func.next_value();
        func.block_mut(bb0)
            .insts
            .push(Inst::IConst { dst: v, ty: LirType::I64, value: 0 });
        func.block_mut(bb0).terminator = Term::Switch {
            value: v,
            cases: vec![(1, bb1, vec![]), (2, bb2, vec![])],
            default: bb2, // two paths into bb2 → bb2 has 2 preds
            default_args: vec![],
        };
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![]);
        // bb3 has two preds (bb1, bb2) — none of the edges into bb3 are
        // critical because bb1/bb2 each have a single successor.
        func.block_mut(bb3).terminator = Term::RetVoid;

        split_critical_edges(&mut func);

        // Both edges from bb0 into bb2 are critical (bb0 has 3 succs, bb2 has
        // 2 preds). We expect TWO new blocks inserted.
        assert_eq!(func.blocks.len(), 6, "two new blocks for two critical edges");
        // After splitting, bb2 should have NO critical incoming edges from bb0.
        let preds = compute_predecessor_counts(&func);
        let pred_bb2 = preds[bb2.0 as usize];
        // bb2 still has 2 preds, but they're now from the two new blocks
        // (each with a single successor), so the edges are non-critical.
        assert_eq!(pred_bb2, 2);
        match &func.blocks[bb0.0 as usize].terminator {
            Term::Switch { cases, default, .. } => {
                assert_ne!(cases[1].1, bb2, "case→bb2 redirected");
                assert_ne!(*default, bb2, "default→bb2 redirected");
                assert_eq!(cases[0].1, bb1, "non-critical case untouched");
            }
            _ => panic!(),
        }
    }
}
