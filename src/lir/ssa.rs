//! SSA construction pass for LIR.
//!
//! Promotes scalar slots to SSA values using a simplified version of
//! Braun et al. 2013 ("Simple and Efficient Construction of SSA Form").
//!
//! Algorithm:
//! 1. Identify promotable slots (scalar type, no SlotAddr instruction).
//! 2. Walk blocks in order, tracking the current definition of each slot.
//! 3. At slot stores, record the stored value as the new definition.
//! 4. At slot loads, replace with the reaching definition.
//! 5. At merge points (blocks with multiple predecessors), insert block
//!    parameters and patch predecessor terminators with arguments.
//! 6. Remove dead SlotStore/SlotLoad instructions.

use super::*;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Run SSA construction on a function, promoting scalar slots.
pub fn construct_ssa(func: &mut LirFunction) {
    let promotable = find_promotable_slots(func);
    if promotable.is_empty() {
        return;
    }

    let preds = compute_predecessors(func);
    let mut ctx = SsaBuilder::new(func, &promotable, &preds);
    ctx.run();

    // Debug-only dominance validation after SSA construction.
    #[cfg(debug_assertions)]
    {
        let errors = super::validate::validate_ssa_dominance(func);
        debug_assert!(errors.is_empty(),
            "SSA dominance violation in @{}: {}",
            func.name,
            errors.iter().map(|e| e.message.as_str()).collect::<Vec<_>>().join("; "));
    }
}

/// Synthesize a type-correct zero/default constant for `dst` of type `ty`.
///
/// SSA construction materializes a default value whenever a slot is read with
/// no reaching store (uninitialized variable, or an unresolved block argument at
/// a branch). The constant *must* match the slot's `LirType`: a float slot needs
/// a float const (`FConst`), not an integer `IConst` tagged `f64`. Emitting a
/// type-blind `IConst` is silently masked by the C backend (`(double)0LL` casts
/// to `0.0`) but produces invalid LLVM IR (`add double 0, 0`, which `llc`
/// rejects with "integer constant must have integer type").
///
/// This is the single source of truth for default-synthesis dispatch; all three
/// sites in this pass route through it.
fn zero_const_inst(val: ValueId, ty: LirType) -> Inst {
    match &ty {
        LirType::Bool => Inst::BoolConst { dst: val, value: false },
        LirType::F32 => Inst::FConst { dst: val, ty: LirType::F32, bits: 0 },
        LirType::F64 => Inst::FConst { dst: val, ty: LirType::F64, bits: 0 },
        LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => Inst::NullPtr { dst: val },
        // Item 7e Phase 3: a `Resource` slot is either pointer-shaped
        // (RefCounted handle) — null-init same as Ptr — or aggregate-
        // shaped, in which case the entry-block undefined value is
        // conceptually a zero struct; emitting a NullPtr-like sentinel
        // is the closest analog at SSA time. The aggregate path
        // shouldn't fire in practice (slot types stay `Struct(sid)`
        // under the surgical Phase 2 scope), but is defensive.
        LirType::Resource { .. } => Inst::NullPtr { dst: val },
        _ => Inst::IConst { dst: val, ty, value: 0 },
    }
}

/// A slot is promotable if:
/// - It has scalar type (not aggregate, not void)
/// - No SlotAddr instruction references it
fn find_promotable_slots(func: &LirFunction) -> HashSet<SlotId> {
    let mut candidates: HashSet<SlotId> = HashSet::new();

    // Start with all scalar slots as candidates.
    for (i, slot) in func.slots.iter().enumerate() {
        if slot.ty.is_scalar() {
            candidates.insert(SlotId(i as u32));
        }
    }

    // Remove any slot that has a SlotAddr instruction (address escapes).
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::SlotAddr { slot, .. } = inst {
                candidates.remove(slot);
            }
        }
    }

    candidates
}

/// Compute reverse postorder traversal of the CFG.
/// Ensures dominators are visited before dominated blocks.
fn compute_rpo(func: &LirFunction) -> Vec<BlockId> {
    let n = func.blocks.len();
    let mut visited = vec![false; n];
    let mut postorder = Vec::with_capacity(n);

    fn dfs(bb: BlockId, func: &LirFunction, visited: &mut [bool], postorder: &mut Vec<BlockId>) {
        let idx = bb.0 as usize;
        if idx >= visited.len() || visited[idx] { return; }
        visited[idx] = true;
        for succ in func.blocks[idx].terminator.successors() {
            dfs(succ, func, visited, postorder);
        }
        postorder.push(bb);
    }

    if n > 0 {
        dfs(BlockId(0), func, &mut visited, &mut postorder);
        // Include any unreachable blocks (shouldn't happen in practice)
        for i in 0..n {
            if !visited[i] { postorder.push(BlockId(i as u32)); }
        }
    }
    postorder.reverse();
    postorder
}

/// Compute predecessor blocks for each block.
fn compute_predecessors(func: &LirFunction) -> Vec<Vec<BlockId>> {
    let mut preds = vec![Vec::new(); func.blocks.len()];
    for block in &func.blocks {
        for succ in block.terminator.successors() {
            preds[succ.0 as usize].push(block.id);
        }
    }
    preds
}

struct SsaBuilder<'a> {
    func: &'a mut LirFunction,
    promotable: &'a HashSet<SlotId>,
    preds: &'a Vec<Vec<BlockId>>,
    /// Current definition of each slot at each block: block → slot → value.
    ///
    /// BTreeMap (not HashMap): `patch_terminators` iterates
    /// `incomplete_phis.keys()` inside a fixpoint loop that ALLOCATES fresh
    /// `ValueId`s via `add_block_param` → `func.next_value()`. HashMap's
    /// per-instance randomized bucket order would make ValueId numbering
    /// (and therefore every downstream `__vN`/`__coalK`/`__bpN` identifier
    /// in emitted C) non-deterministic across otherwise-identical runs of
    /// the compiler. See `rust_gg_build_is_deterministic` (integration).
    current_def: BTreeMap<(BlockId, SlotId), ValueId>,
    /// Block parameters we've added: block → slot → param ValueId.
    ///
    /// BTreeMap (see `current_def` above): iterated by `patch_terminators`
    /// alongside `incomplete_phis` during the phi-fixpoint; determinism of
    /// SSA construction requires all three maps have a stable iteration
    /// order.
    block_params: BTreeMap<(BlockId, SlotId), ValueId>,
    /// Track which blocks we've sealed (all predecessors processed).
    /// For simplicity, we process in RPO order and seal blocks lazily.
    ///
    /// BTreeMap (see `current_def` above): the primary non-det write site
    /// was `self.incomplete_phis.keys().copied().collect()` inside a fixpoint
    /// loop that allocates fresh `ValueId`s; sorted iteration is load-bearing.
    incomplete_phis: BTreeMap<(BlockId, SlotId), ValueId>,
    /// Direct value substitution map: old ValueId → reaching ValueId.
    /// Accumulated during process_block for every eliminated SlotLoad.
    ///
    /// BTreeMap (not HashMap): symmetry with the three phi maps above (they
    /// all live on the same SSA builder and can be produced in any order).
    /// Its own `.keys()` iteration in `remove_promoted_instructions` is
    /// order-independent-in-effect (each iteration just re-resolves through
    /// `resolve_value` chains and re-inserts), but keeping it a HashMap here
    /// would leave a recurring-bug-class invitation: a future refactor that
    /// gains an allocating side-effect on that iteration would silently
    /// resurrect the non-det.
    value_subst: BTreeMap<ValueId, ValueId>,
}

impl<'a> SsaBuilder<'a> {
    fn new(
        func: &'a mut LirFunction,
        promotable: &'a HashSet<SlotId>,
        preds: &'a Vec<Vec<BlockId>>,
    ) -> Self {
        Self {
            func,
            promotable,
            preds,
            current_def: BTreeMap::new(),
            block_params: BTreeMap::new(),
            incomplete_phis: BTreeMap::new(),
            value_subst: BTreeMap::new(),
        }
    }

    fn run(&mut self) {
        // Phase 1: Walk blocks in reverse postorder (RPO), replacing SlotLoad
        // with reaching definitions.  RPO ensures dominators are processed before
        // dominated blocks, so read_variable always finds definitions from
        // already-processed predecessors — critical when block-splitting lifts
        // create high-numbered blocks that dominate low-numbered GIR blocks.
        let rpo = compute_rpo(self.func);

        for bb in &rpo {
            self.process_block(*bb);
        }

        // Phase 2: Remove promoted SlotStore/SlotLoad instructions.
        self.remove_promoted_instructions();

        // Phase 3: Patch terminators with block arguments for inserted params.
        self.patch_terminators();
    }

    fn process_block(&mut self, bb: BlockId) {
        let block = &self.func.blocks[bb.0 as usize];
        let insts = block.insts.clone(); // clone to allow mutation
        // Parallel-clone span_map; pre-1b blocks may have an empty span_map,
        // in which case we treat every slot as `None` to keep the invariant.
        let spans: Vec<Option<crate::span::Span>> = if block.span_map.len() == insts.len() {
            block.span_map.clone()
        } else {
            vec![None; insts.len()]
        };

        let mut new_insts = Vec::with_capacity(insts.len());
        let mut new_spans: Vec<Option<crate::span::Span>> = Vec::with_capacity(insts.len());

        for (i, inst) in insts.iter().enumerate() {
            match inst {
                Inst::SlotStore { slot, value, .. } if self.promotable.contains(slot) => {
                    // Resolve through substitution chain so current_def always
                    // holds the canonical (non-eliminated) value.
                    let resolved = self.resolve_value(*value);
                    self.current_def.insert((bb, *slot), resolved);
                    // Don't emit the SlotStore — it's promoted.
                }
                Inst::SlotLoad { dst, slot, .. } if self.promotable.contains(slot) => {
                    // Replace with the reaching definition.
                    let reaching = self.read_variable(*slot, bb);
                    // Update current def so subsequent loads see the same value.
                    self.current_def.insert((bb, *slot), reaching);
                    // Record the substitution: all uses of dst should become reaching.
                    if reaching != *dst {
                        self.value_subst.insert(*dst, reaching);
                    }
                    // Note: do NOT overwrite block_params here — that map stores
                    // the actual param ValueIds created by add_block_param and is
                    // needed by patch_terminators for correct arg ordering.
                }
                other => {
                    // For non-promoted instructions, check if they store to a promoted slot
                    // (they shouldn't, but be safe).
                    new_insts.push(other.clone());
                    new_spans.push(spans.get(i).copied().flatten());
                }
            }
        }

        // Note: `read_variable` may insert a zero-init at index 0 of this
        // block during the loop above. The original code overwrites that
        // by assigning `new_insts` here — we preserve the same semantics
        // (any inserted instructions on `bb` itself are discarded by the
        // assignment). Span_map is rebuilt in parallel so the invariant
        // holds.
        self.func.blocks[bb.0 as usize].insts = new_insts;
        self.func.blocks[bb.0 as usize].span_map = new_spans;
    }

    /// Chase value_subst chain to find the canonical (non-eliminated) value.
    fn resolve_value(&self, mut val: ValueId) -> ValueId {
        let mut visited = std::collections::HashSet::new();
        while let Some(&target) = self.value_subst.get(&val) {
            if !visited.insert(val) {
                break; // cycle detected — return best value so far
            }
            val = target;
        }
        val
    }

    /// Read the current definition of a slot at block entry.
    fn read_variable(&mut self, slot: SlotId, bb: BlockId) -> ValueId {
        // Check if we have a local definition in this block.
        if let Some(&val) = self.current_def.get(&(bb, slot)) {
            return val;
        }

        // Check predecessors.
        let preds = self.preds[bb.0 as usize].clone();

        if preds.is_empty() {
            // Entry block with no definition — use an undefined value.
            // This happens for uninitialized variables. Create a zero constant.
            let val = self.func.next_value();
            let ty = self.func.slots[slot.0 as usize].ty.clone();
            // Insert a type-correct zero constant at the beginning of bb0.
            let zero_inst = zero_const_inst(val, ty);
            self.func.blocks[bb.0 as usize].insert_inst(0, zero_inst, None);
            self.current_def.insert((bb, slot), val);
            val
        } else if preds.len() == 1 {
            // Single predecessor — use its definition.
            let pred = preds[0];
            let val = self.read_variable(slot, pred);
            self.current_def.insert((bb, slot), val);
            val
        } else {
            // Multiple predecessors — need a block parameter (phi).
            self.add_block_param(slot, bb)
        }
    }

    /// Add a block parameter for a slot at a merge point.
    fn add_block_param(&mut self, slot: SlotId, bb: BlockId) -> ValueId {
        // Check if we already created a param for this slot at this block.
        if let Some(&val) = self.block_params.get(&(bb, slot)) {
            return val;
        }

        let ty = self.func.slots[slot.0 as usize].ty.clone();
        let param_val = self.func.next_value();
        self.func.blocks[bb.0 as usize]
            .params
            .push((param_val, ty));
        self.block_params.insert((bb, slot), param_val);
        self.current_def.insert((bb, slot), param_val);

        // Record as incomplete — we need to fill in arguments from predecessors.
        self.incomplete_phis.insert((bb, slot), param_val);

        param_val
    }

    /// Remove promoted SlotStore/SlotLoad instructions (already done in process_block
    /// for SlotStore; SlotLoad is also removed there).
    fn remove_promoted_instructions(&mut self) {
        // The instructions were already filtered during process_block.
        // Apply value substitutions: replace all uses of eliminated SlotLoad
        // dst values with their reaching definitions.
        if self.value_subst.is_empty() {
            return;
        }

        // Resolve any transitive chains in the substitution map.
        let keys: Vec<ValueId> = self.value_subst.keys().copied().collect();
        for k in keys {
            let resolved = self.resolve_value(k);
            self.value_subst.insert(k, resolved);
        }

        // Apply substitutions across all instructions and terminators.
        for block in &mut self.func.blocks {
            for inst in &mut block.insts {
                substitute_inst_values(inst, &self.value_subst);
            }
            substitute_term_values(&mut block.terminator, &self.value_subst);
        }
    }

    /// Patch terminators: for each jump/branch to a block with params,
    /// provide the reaching definition from the predecessor.
    fn patch_terminators(&mut self) {
        // Iteratively resolve reaching definitions and create new phis as needed.
        // When a predecessor block has multiple preds with conflicting definitions,
        // a new block param (phi) must be created there, which may cascade.
        let mut changed = true;
        while changed {
            changed = false;
            let phis: Vec<(BlockId, SlotId)> = self.incomplete_phis.keys().copied().collect();
            for (target_bb, slot) in &phis {
                let preds = self.preds[target_bb.0 as usize].clone();
                for pred_bb in preds {
                    if !self.current_def.contains_key(&(pred_bb, *slot)) {
                        let val = self.resolve_reaching_def(*slot, pred_bb);
                        self.current_def.insert((pred_bb, *slot), val);
                        changed = true;
                    }
                }
            }
        }

        // For each target block with params, determine the slot order from
        // the block params (which were added by add_block_param in discovery order).
        // Then for each predecessor, collect args in that same order.
        // First, build a map: target_bb → [slot in param order]
        let phis: Vec<(BlockId, SlotId)> = self.incomplete_phis.keys().copied().collect();
        // BTreeMap (not HashMap): iterated below in a loop that in its
        // defensive `unwrap_or_else` path allocates fresh `ValueId`s via
        // `self.func.next_value()`. HashMap's per-instance randomized bucket
        // order would resurrect the same class of non-determinism the four
        // SsaBuilder fields fixed above.
        let mut target_slots: BTreeMap<BlockId, Vec<SlotId>> = BTreeMap::new();
        for &(target_bb, slot) in &phis {
            target_slots.entry(target_bb).or_default().push(slot);
        }
        // Sort each target's slots by the position of their block param in the
        // target block's params list. This ensures args match param order.
        for (target_bb, slots) in &mut target_slots {
            let block_params_list: &Vec<(ValueId, LirType)> =
                &self.func.blocks[target_bb.0 as usize].params;
            slots.sort_by_key(|slot| {
                let param_val = self.block_params.get(&(*target_bb, *slot)).copied();
                block_params_list
                    .iter()
                    .position(|(vid, _)| Some(*vid) == param_val)
                    .unwrap_or(usize::MAX)
            });
        }

        // Collect patches: for each (pred, target), build a single args vector
        // with values in the correct param order.
        //
        // HashMap here is safe (order-independent in effect): the apply loop
        // below writes each patch to a distinct terminator field on a distinct
        // block, and the terminator-writer allocates no fresh ValueIds. Kept
        // as HashMap rather than swapped to BTreeMap only because there is no
        // ordering hazard to close — the four SsaBuilder maps above are the
        // load-bearing ones.
        let mut patches: HashMap<(BlockId, BlockId), Vec<ValueId>> = HashMap::new();
        for (&target_bb, slots) in &target_slots {
            let preds = self.preds[target_bb.0 as usize].clone();
            for &pred_bb in &preds {
                let args: Vec<ValueId> = slots.iter().map(|slot| {
                    self.current_def
                        .get(&(pred_bb, *slot))
                        .copied()
                        .unwrap_or_else(|| {
                            let v = self.func.next_value();
                            let ty = self.func.slots[slot.0 as usize].ty.clone();
                            self.func.blocks[pred_bb.0 as usize]
                                .push_synthetic(zero_const_inst(v, ty));
                            v
                        })
                }).collect();
                patches.insert((pred_bb, target_bb), args);
            }
        }

        // Apply patches to terminators.
        for ((pred_bb, target_bb), args) in patches {
            let term = &mut self.func.blocks[pred_bb.0 as usize].terminator;
            add_args_to_terminator(term, target_bb, &args);
        }
    }

    /// Resolve the reaching definition of a slot at a block, creating new
    /// block params (phis) when predecessors disagree.
    fn resolve_reaching_def(&mut self, slot: SlotId, bb: BlockId) -> ValueId {
        if let Some(&val) = self.current_def.get(&(bb, slot)) {
            return val;
        }
        if let Some(&val) = self.block_params.get(&(bb, slot)) {
            return val;
        }
        let preds = self.preds[bb.0 as usize].clone();
        if preds.is_empty() {
            // Entry block — undefined, use a type-correct zero.
            let val = self.func.next_value();
            let ty = self.func.slots[slot.0 as usize].ty.clone();
            self.func.blocks[bb.0 as usize].insert_inst(
                0,
                zero_const_inst(val, ty),
                None,
            );
            self.current_def.insert((bb, slot), val);
            return val;
        }
        if preds.len() == 1 {
            let pred = preds[0];
            let val = self.resolve_reaching_def(slot, pred);
            self.current_def.insert((bb, slot), val);
            return val;
        }
        // Multiple predecessors — check if they all agree.
        let mut vals: Vec<ValueId> = Vec::new();
        let mut all_resolved = true;
        for &pred in &preds {
            if let Some(&val) = self.current_def.get(&(pred, slot)) {
                vals.push(val);
            } else if let Some(&val) = self.block_params.get(&(pred, slot)) {
                vals.push(val);
            } else {
                all_resolved = false;
            }
        }
        if all_resolved && !vals.is_empty() && vals.iter().all(|v| *v == vals[0]) {
            // All predecessors agree — no phi needed.
            self.current_def.insert((bb, slot), vals[0]);
            return vals[0];
        }
        // Predecessors disagree or not all resolved — create a block param (phi).
        self.add_block_param(slot, bb)
    }
}

/// Add arguments to a terminator's jump to a specific target block.
fn add_args_to_terminator(term: &mut Term, target: BlockId, args: &[ValueId]) {
    match term {
        Term::Jump(t, existing_args) if *t == target => {
            existing_args.extend_from_slice(args);
        }
        Term::Branch {
            then_block,
            then_args,
            else_block,
            else_args,
            ..
        } => {
            if *then_block == target {
                then_args.extend_from_slice(args);
            }
            if *else_block == target {
                else_args.extend_from_slice(args);
            }
        }
        Term::Switch {
            cases,
            default,
            default_args,
            ..
        } => {
            for (_, block, case_args) in cases.iter_mut() {
                if *block == target {
                    case_args.extend_from_slice(args);
                }
            }
            if *default == target {
                default_args.extend_from_slice(args);
            }
        }
        _ => {}
    }
}

/// Substitute value references in an instruction.
fn substitute_inst_values(inst: &mut Inst, subst: &BTreeMap<ValueId, ValueId>) {
    let sub = |v: &mut ValueId| {
        if let Some(&replacement) = subst.get(v) {
            *v = replacement;
        }
    };

    match inst {
        Inst::SlotStore { value, .. } => sub(value),
        Inst::ClosurePack { env_ptr, .. } => sub(env_ptr),
        Inst::Add { lhs, rhs, .. }
        | Inst::Sub { lhs, rhs, .. }
        | Inst::Mul { lhs, rhs, .. }
        | Inst::Div { lhs, rhs, .. }
        | Inst::Rem { lhs, rhs, .. }
        | Inst::Mod { lhs, rhs, .. }
        | Inst::BitAnd { lhs, rhs, .. }
        | Inst::BitOr { lhs, rhs, .. }
        | Inst::BitXor { lhs, rhs, .. }
        | Inst::Shl { lhs, rhs, .. }
        | Inst::Shr { lhs, rhs, .. }
        | Inst::FaultCheck { lhs, rhs, .. }
        | Inst::Cmp { lhs, rhs, .. } => {
            sub(lhs);
            sub(rhs);
        }
        Inst::Neg { operand, .. }
        | Inst::BitNot { operand, .. }
        | Inst::Not { operand, .. } => sub(operand),
        Inst::IntCast { value, .. }
        | Inst::FloatCast { value, .. }
        | Inst::IntToFloat { value, .. }
        | Inst::FloatToInt { value, .. }
        | Inst::PtrCast { value, .. }
        | Inst::Bitcast { value, .. } => sub(value),
        Inst::Load { ptr, .. } => sub(ptr),
        Inst::Store { ptr, value } => {
            sub(ptr);
            sub(value);
        }
        Inst::FieldPtr { base, .. } => sub(base),
        Inst::ElemPtr { base, index, .. } => {
            sub(base);
            sub(index);
        }
        Inst::Memset { ptr, byte, size } => {
            sub(ptr);
            sub(byte);
            sub(size);
        }
        Inst::Memcpy {
            dst_ptr,
            src_ptr,
            size,
        } => {
            sub(dst_ptr);
            sub(src_ptr);
            sub(size);
        }
        Inst::Call { args, .. }
        | Inst::CallExtern { args, .. }
        | Inst::CallRuntime { args, .. } => {
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::CollectionCtor { args, .. } => {
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::CallPtr { callee, args, .. } => {
            sub(callee);
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::CallByRef { fref, args, .. } => {
            sub(fref);
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::CallClosure { closure, args, .. } => {
            sub(closure);
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::DropGuardOpen { value, .. } => sub(value),
        Inst::DropGuardClose => {}
        Inst::BoundsCheck { index, len } => {
            sub(index);
            sub(len);
        }
        Inst::DivCheck { divisor } => sub(divisor),
        Inst::Printf { args, .. } => {
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::Fprintf { fd, args, .. } => {
            sub(fd);
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Inst::EnumInit { target, fields, .. } => {
            sub(target);
            for (_, v) in fields.iter_mut() { sub(v); }
        }
        Inst::EnumCheck { value, .. } => sub(value),
        Inst::EnumExtract { value, .. } => sub(value),
        Inst::StructInit { target, fields, .. } => {
            sub(target);
            for (_, v) in fields.iter_mut() { sub(v); }
        }
        Inst::CowClone { src, .. } => sub(src),
        Inst::TraitCall { object, args, .. } => {
            sub(object);
            for a in args.iter_mut() { sub(a); }
        }
        Inst::HofExpand { coll, closure, init, .. } => {
            sub(coll);
            sub(closure);
            if let Some(i) = init { sub(i); }
        }
        Inst::AddressOf { value, .. } => sub(value),
        Inst::BoxAlloc { value, .. } => sub(value),
        Inst::SlotLoad { .. }
        | Inst::SlotAddr { .. }
        | Inst::IConst { .. }
        | Inst::FConst { .. }
        | Inst::BoolConst { .. }
        | Inst::NullPtr { .. }
        | Inst::FuncAddr { .. }
        | Inst::NamedFuncAddr { .. }
        | Inst::GlobalAddr { .. }
        | Inst::StrLit { .. }
        | Inst::ParamRef { .. }
        | Inst::SizeOf { .. }
        | Inst::Trap { .. }
        | Inst::MoveSlot { .. }
        | Inst::Nop | Inst::InlineC { .. } => {}
        Inst::SetCollectionBridge { collection, .. } => sub(collection),
    }
}

/// Substitute value references in a terminator.
fn substitute_term_values(term: &mut Term, subst: &BTreeMap<ValueId, ValueId>) {
    let sub = |v: &mut ValueId| {
        if let Some(&replacement) = subst.get(v) {
            *v = replacement;
        }
    };

    match term {
        Term::Ret(v) => sub(v),
        Term::RetVoid | Term::Unreachable => {}
        Term::Jump(_, args) => {
            for a in args.iter_mut() {
                sub(a);
            }
        }
        Term::Branch {
            cond,
            then_args,
            else_args,
            ..
        } => {
            sub(cond);
            for a in then_args.iter_mut() {
                sub(a);
            }
            for a in else_args.iter_mut() {
                sub(a);
            }
        }
        Term::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            sub(value);
            for (_, _, args) in cases.iter_mut() {
                for a in args.iter_mut() {
                    sub(a);
                }
            }
            for a in default_args.iter_mut() {
                sub(a);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn promote_simple_scalar() {
        // fn test() -> i64:
        //   s0: i64
        //   bb0:
        //     v0: i64 = iconst 42
        //     slot_store s0, v0
        //     v1: i64 = slot_load s0
        //     ret v1
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let s0 = func.add_slot(LirType::I64, Some("x".into()));
        let bb0 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::IConst {
                dst: v0,
                ty: LirType::I64,
                value: 42,
            },
            Inst::SlotStore { slot: s0, value: v0, is_move: false },
            Inst::SlotLoad {
                dst: v1,
                slot: s0,
                ty: LirType::I64,
            },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v1);

        construct_ssa(&mut func);

        // After SSA: SlotStore and SlotLoad should be removed.
        // The ret should use v0 directly (or its alias).
        let block = &func.blocks[0];
        assert!(
            !block.insts.iter().any(|i| matches!(i, Inst::SlotStore { .. })),
            "SlotStore should be removed"
        );
        assert!(
            !block.insts.iter().any(|i| matches!(i, Inst::SlotLoad { .. })),
            "SlotLoad should be removed"
        );
    }

    #[test]
    fn promote_with_branch() {
        // fn test() -> i64:
        //   s0: i64
        //   s1: bool
        //   bb0:
        //     v0: bool = bconst true
        //     slot_store s1, v0
        //     v1: bool = slot_load s1
        //     br v1, bb1, bb2
        //   bb1:
        //     v2: i64 = iconst 10
        //     slot_store s0, v2
        //     jmp bb3
        //   bb2:
        //     v3: i64 = iconst 20
        //     slot_store s0, v3
        //     jmp bb3
        //   bb3:
        //     v4: i64 = slot_load s0
        //     ret v4
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let s0 = func.add_slot(LirType::I64, Some("x".into()));
        let s1 = func.add_slot(LirType::Bool, Some("cond".into()));

        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();
        let v3 = func.next_value();
        let v4 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::BoolConst { dst: v0, value: true },
            Inst::SlotStore { slot: s1, value: v0, is_move: false },
            Inst::SlotLoad {
                dst: v1,
                slot: s1,
                ty: LirType::Bool,
            },
        ];
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v1,
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };

        func.block_mut(bb1).insts = vec![
            Inst::IConst {
                dst: v2,
                ty: LirType::I64,
                value: 10,
            },
            Inst::SlotStore { slot: s0, value: v2, is_move: false },
        ];
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![]);

        func.block_mut(bb2).insts = vec![
            Inst::IConst {
                dst: v3,
                ty: LirType::I64,
                value: 20,
            },
            Inst::SlotStore { slot: s0, value: v3, is_move: false },
        ];
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![]);

        func.block_mut(bb3).insts = vec![Inst::SlotLoad {
            dst: v4,
            slot: s0,
            ty: LirType::I64,
        }];
        func.block_mut(bb3).terminator = Term::Ret(v4);

        construct_ssa(&mut func);

        // After SSA: bb3 should have a block parameter for s0.
        let merge_block = &func.blocks[3];
        assert!(
            !merge_block.params.is_empty(),
            "merge block should have a block parameter"
        );

        // The branch cond should be v0 (not v1 which was a SlotLoad).
        let term = &func.blocks[0].terminator;
        if let Term::Branch { cond, .. } = term {
            assert_eq!(*cond, v0, "branch cond should use v0 directly");
        }
    }

    #[test]
    fn aggregate_slots_not_promoted() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Void);
        let _s0 = func.add_slot(LirType::Struct(StructId(0)), Some("point".into()));
        let bb0 = func.add_block();

        let v0 = func.next_value();
        func.block_mut(bb0).insts = vec![Inst::SlotAddr {
            dst: v0,
            slot: SlotId(0),
        }];
        func.block_mut(bb0).terminator = Term::RetVoid;

        let promotable = find_promotable_slots(&func);
        assert!(
            promotable.is_empty(),
            "aggregate slots should not be promotable"
        );
    }

    #[test]
    fn addressed_slot_not_promoted() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let s0 = func.add_slot(LirType::I64, Some("x".into()));
        let bb0 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::IConst {
                dst: v0,
                ty: LirType::I64,
                value: 42,
            },
            Inst::SlotStore { slot: s0, value: v0, is_move: false },
            Inst::SlotAddr { dst: v1, slot: s0 }, // takes address!
        ];
        func.block_mut(bb0).terminator = Term::Ret(v0);

        let promotable = find_promotable_slots(&func);
        assert!(
            !promotable.contains(&s0),
            "addressed scalar slot should not be promotable"
        );
    }

    #[test]
    fn no_promotable_slots_is_noop() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Void);
        let _s0 = func.add_slot(LirType::Struct(StructId(0)), None);
        let bb0 = func.add_block();
        func.block_mut(bb0).terminator = Term::RetVoid;

        let original_insts = func.blocks[0].insts.len();
        construct_ssa(&mut func);
        assert_eq!(func.blocks[0].insts.len(), original_insts);
    }

    #[test]
    fn predecessors_computed_correctly() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        func.block_mut(bb0).terminator = Term::Branch {
            cond: ValueId(0),
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb3).terminator = Term::RetVoid;

        let preds = compute_predecessors(&func);
        assert!(preds[0].is_empty()); // bb0 has no predecessors
        assert_eq!(preds[1], vec![bb0]); // bb1 ← bb0
        assert_eq!(preds[2], vec![bb0]); // bb2 ← bb0
        assert_eq!(preds[3].len(), 2); // bb3 ← bb1, bb2
        assert!(preds[3].contains(&bb1));
        assert!(preds[3].contains(&bb2));
    }

    #[test]
    fn zero_const_inst_is_type_dispatched() {
        // A float slot's synthesized default must be a float const (FConst),
        // not an integer IConst tagged f64 — the latter emits invalid LLVM IR
        // (`add double 0, 0`). Regression guard for gorget-js snag #12.
        let v = ValueId(0);
        assert!(
            matches!(
                zero_const_inst(v, LirType::F64),
                Inst::FConst { ty: LirType::F64, bits: 0, .. }
            ),
            "F64 default must be an FConst"
        );
        assert!(
            matches!(
                zero_const_inst(v, LirType::F32),
                Inst::FConst { ty: LirType::F32, bits: 0, .. }
            ),
            "F32 default must be an FConst"
        );
        assert!(
            matches!(zero_const_inst(v, LirType::I64), Inst::IConst { value: 0, .. }),
            "integer default must be an IConst"
        );
        assert!(
            matches!(zero_const_inst(v, LirType::Bool), Inst::BoolConst { value: false, .. }),
            "bool default must be a BoolConst"
        );
        assert!(
            matches!(zero_const_inst(v, LirType::Ptr), Inst::NullPtr { .. }),
            "pointer default must be a NullPtr"
        );
    }
}
