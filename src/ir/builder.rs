use super::instructions::*;
use super::types::*;
use super::{BasicBlock, Function, Local, LocalOwnership, SlotKind};
use crate::span::Span;

/// Ergonomic builder for constructing GIR functions.
pub struct FunctionBuilder {
    pub name: String,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    pub current_block: BlockId,
    /// Current source span — attached to every emitted instruction until changed.
    pub current_span: Option<Span>,
    /// `with` refresh pairs for spawned functions: `(binding_local, param_local)`.
    pub with_refresh_pairs: Vec<(LocalId, LocalId)>,
    /// Inner shared spawn metadata for nested spawn propagation.
    pub inner_shared_spawns: Vec<super::InnerSharedSpawn>,
}

impl FunctionBuilder {
    /// Create a new function builder.
    ///
    /// Sets up `_0` as the return place and `_1.._N` as parameters.
    /// Creates the entry block (bb0).
    pub fn new(
        name: impl Into<String>,
        return_type: TypeId,
        params: &[(TypeId, Option<&str>)],
    ) -> Self {
        let mut locals = Vec::new();

        // _0 = return place
        locals.push(Local {
            type_id: return_type,
            name_hint: None,
            ownership: LocalOwnership::default(),
            slot_kind: SlotKind::default(),
            is_owning_param: false,
            deref_of_owning_param: None,
        });

        // _1.._N = parameters
        let mut param_types = Vec::new();
        for (ty, hint) in params {
            param_types.push(*ty);
            locals.push(Local {
                type_id: *ty,
                name_hint: hint.map(|s| s.to_string()),
                ownership: LocalOwnership::default(),
                slot_kind: SlotKind::default(),
                is_owning_param: false,
                deref_of_owning_param: None,
            });
        }

        let blocks = vec![BasicBlock::new()];

        Self {
            name: name.into(),
            params: param_types,
            return_type,
            locals,
            blocks,
            current_block: BlockId(0),
            current_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        }
    }

    // ---- Local management ----

    /// Allocate a new local variable and return its LocalId.
    pub fn add_local(&mut self, type_id: TypeId, name_hint: Option<&str>) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(Local {
            type_id,
            name_hint: name_hint.map(|s| s.to_string()),
            ownership: LocalOwnership::default(),
            slot_kind: SlotKind::default(),
            is_owning_param: false,
            deref_of_owning_param: None,
        });
        id
    }

    /// Get the type of a local with bounds checking.
    pub fn local_type(&self, id: LocalId) -> TypeId {
        let idx = id.0 as usize;
        debug_assert!(idx < self.locals.len(),
            "BUG: LocalId({}) out of range (only {} locals)", id.0, self.locals.len());
        self.locals[idx].type_id
    }

    /// Get the name hint of a local (if any).
    pub fn local_name(&self, id: LocalId) -> Option<&str> {
        self.locals[id.0 as usize].name_hint.as_ref().map(|s| s.as_str())
    }

    /// Attach a name hint to an existing local (e.g. a `Ptr(elem)` for-loop
    /// element materialized via `index_load_borrow`, which creates an anonymous
    /// temp). The hint is what `cow_materialize_collection_ref` rebinds when a
    /// bare for-element is written in place (Track 1A materialize-on-write).
    pub fn set_local_name(&mut self, id: LocalId, name: &str) {
        self.locals[id.0 as usize].name_hint = Some(name.to_string());
    }

    /// Update the type of an existing local (e.g., after discovering the actual
    /// type from lowering a branch of an if-expression).
    pub fn set_local_type(&mut self, id: LocalId, type_id: TypeId) {
        self.locals[id.0 as usize].type_id = type_id;
    }

    // ---- Block management ----

    /// Create a new empty basic block and return its BlockId.
    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock::new());
        id
    }

    /// Switch emission to a different block.
    pub fn switch_to(&mut self, block: BlockId) {
        self.current_block = block;
    }

    /// Finalize and return the completed Function.
    pub fn build(self) -> Function {
        Function {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            locals: self.locals,
            blocks: self.blocks,
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: self.with_refresh_pairs,
            inner_shared_spawns: self.inner_shared_spawns,
            participates_in_fault: false,
        }
    }

    /// Set the source span for all subsequent emitted instructions.
    pub fn set_span(&mut self, span: Span) {
        self.current_span = Some(span);
    }

    // ---- Helpers ----

    fn emit(&mut self, inst: Instruction) {
        let block = &mut self.blocks[self.current_block.0 as usize];
        block.instructions.push(inst);
        block.span_map.push(self.current_span);
    }

    fn set_terminator(&mut self, term: Terminator) {
        let block = &mut self.blocks[self.current_block.0 as usize];
        // Cluster B (Snag #33 + Snag #39 bug 1): no-op when already
        // terminated. The previous behaviour was silent-overwrite, which
        // let post-divergent-subexpression fallthrough `jump`/`ret` /
        // `branch` calls clobber the divergent Return/Throw/Unreachable
        // terminator. Callers no longer need to scatter
        // `is_terminated()` guards — `jump(merge)` / `ret(...)` /
        // `branch(...)` after a divergent statement is now a built-in
        // no-op. Should a future caller genuinely want to overwrite a
        // terminator (none today), introduce `force_set_terminator`
        // explicitly rather than relying on silent overwrite.
        if block.terminator.is_some() {
            return;
        }
        block.terminator = Some(term);
        block.terminator_span = self.current_span;
    }

    /// Returns true if the current block already has a terminator set.
    /// Most callers no longer need this guard after Cluster B —
    /// `jump`/`ret`/`branch` are no-ops on terminated blocks. The
    /// remaining legitimate use cases: skipping result-slot assigns
    /// before `jump(merge)` when the assign would be a wrong-shape
    /// type write (see `lower_catch_expr` / `lower_match_expr`).
    pub fn is_terminated(&self) -> bool {
        self.blocks[self.current_block.0 as usize].terminator.is_some()
    }

    /// Allocate a temp local, emit an instruction targeting it, return the LocalId.
    fn emit_with_temp(&mut self, type_id: TypeId, inst_fn: impl FnOnce(LocalId) -> Instruction) -> LocalId {
        let id = self.add_local(type_id, None);
        self.emit(inst_fn(id));
        id
    }

    // ---- Place / Operand helpers ----

    pub fn local(&self, id: LocalId) -> Place {
        Place::local(id)
    }

    pub fn field(&self, id: LocalId, idx: u32) -> Place {
        Place::field(id, idx)
    }

    pub fn const_bool(b: bool) -> Operand {
        Operand::Constant(Constant::Bool(b))
    }

    pub fn const_i32(n: i32) -> Operand {
        Operand::Constant(Constant::I32(n))
    }

    pub fn const_i64(n: i64) -> Operand {
        Operand::Constant(Constant::I64(n))
    }

    pub fn const_f64(n: f64) -> Operand {
        Operand::Constant(Constant::F64(n))
    }

    pub fn const_str(s: impl Into<String>) -> Operand {
        Operand::Constant(Constant::Str(s.into()))
    }

    pub fn const_unit() -> Operand {
        Operand::Constant(Constant::Unit)
    }

    pub fn const_null() -> Operand {
        Operand::Constant(Constant::Null)
    }

    pub fn copy(id: LocalId) -> Operand {
        Operand::Copy(Place::local(id))
    }

    pub fn mov(id: LocalId) -> Operand {
        Operand::Move(Place::local(id))
    }

    // ---- Instruction emitters ----

    pub fn assign(&mut self, dst: Place, value: Operand) {
        self.emit(Instruction::Assign { mode: AssignMode::Copy, dst, value });
    }

    pub fn assign_mode(&mut self, mode: AssignMode, dst: Place, value: Operand) {
        self.emit(Instruction::Assign { mode, dst, value });
    }

    pub fn global_assign(&mut self, name: String, value: Operand) {
        self.emit(Instruction::GlobalAssign { name, value });
    }

    pub fn field_load(&mut self, base: Place, field: u32, type_id: TypeId) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::FieldLoad { dst, base, field })
    }

    pub fn index_load(&mut self, base: Place, index: Operand, type_id: TypeId) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::IndexLoad {
            dst, base, index, read: ReadMode::Clone,
        })
    }

    pub fn index_load_borrow(&mut self, base: Place, index: Operand, type_id: TypeId) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::IndexLoad {
            dst, base, index, read: ReadMode::Borrow,
        })
    }

    pub fn load_ref(&mut self, src: Place, type_id: TypeId) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::LoadRef { dst, src })
    }

    pub fn store_ref(&mut self, dst: Place, value: Operand) {
        self.emit(Instruction::StoreRef { dst, value });
    }

    pub fn bin_op(&mut self, op: BinOp, type_id: TypeId, lhs: Operand, rhs: Operand) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::BinOp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
        })
    }

    /// Emit a fault-`catch`able arithmetic op (error-model.md §11.2): identical
    /// to [`Self::bin_op`] but on a fault it BRANCHES to `fault_handler` instead
    /// of panicking. Only used inside an active fault-catch for the five integer
    /// faultable ops.
    pub fn bin_op_faultable(
        &mut self,
        op: BinOp,
        type_id: TypeId,
        lhs: Operand,
        rhs: Operand,
        overflow_handler: Option<BlockId>,
        divzero_handler: Option<BlockId>,
    ) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::FaultableBinOp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
            overflow_handler,
            divzero_handler,
        })
    }

    /// Emit a fault-`catch`able array element-READ (error-model.md §11,
    /// `Fault.Bounds`): like [`Self::index_load`] but an out-of-bounds index
    /// BRANCHES to `fault_handler` instead of panicking. Used only inside an
    /// active fault-catch whose `bounds_handler` is set, on array element reads.
    pub fn index_load_faultable(
        &mut self,
        base: Place,
        index: Operand,
        type_id: TypeId,
        read: ReadMode,
        fault_handler: BlockId,
    ) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::FaultableIndexLoad {
            dst,
            base,
            index,
            read,
            fault_handler,
        })
    }

    pub fn un_op(&mut self, op: UnOp, type_id: TypeId, operand: Operand) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::UnOp {
            dst,
            op,
            type_id,
            operand,
        })
    }

    pub fn cmp(&mut self, op: CmpOp, type_id: TypeId, lhs: Operand, rhs: Operand) -> LocalId {
        self.emit_with_temp(BOOL_TYPE, |dst| Instruction::Cmp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
        })
    }

    pub fn cast(&mut self, target_type: TypeId, value: Operand) -> LocalId {
        self.emit_with_temp(target_type, |dst| Instruction::Cast {
            dst,
            target_type,
            value,
        })
    }

    pub fn bitcast(&mut self, target_type: TypeId, value: Operand) -> LocalId {
        self.emit_with_temp(target_type, |dst| Instruction::BitCast {
            dst,
            target_type,
            value,
        })
    }

    pub fn ptr_cast(&mut self, target_type: TypeId, value: Operand) -> LocalId {
        self.emit_with_temp(target_type, |dst| Instruction::PtrCast {
            dst,
            target_type,
            value,
        })
    }

    pub fn call(&mut self, func: impl Into<String>, args: Vec<Operand>, return_type: TypeId) -> LocalId {
        let func = func.into();
        self.emit_with_temp(return_type, |dst| Instruction::Call {
            dst: Some(dst),
            func,
            args,
        })
    }

    pub fn call_void(&mut self, func: impl Into<String>, args: Vec<Operand>) {
        self.emit(Instruction::Call {
            dst: None,
            func: func.into(),
            args,
        });
    }

    /// Emit a fault-`catch`able direct call (error-model.md §11, Increment
    /// 2.1a/2.1c): like [`Self::call`] but the callee participates in cross-frame
    /// fault propagation. `args` must already include the trailing `&fault_slot`
    /// operand as its LAST element; `fault_slot` is the caller's `i32` slot place
    /// (its tag VALUE selects the handler after the call). `overflow_handler` /
    /// `divzero_handler` are the per-category GIR blocks to dispatch to (each the
    /// user's catch entry OR the scope's panic block — always `Some` here). Emits
    /// a `dst`-producing form when `return_type` is not unit. Used ONLY by the
    /// call-site gate inside an active fault scope.
    pub fn fault_call(
        &mut self,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: TypeId,
        fault_slot: Place,
        overflow_handler: Option<BlockId>,
        divzero_handler: Option<BlockId>,
        bounds_handler: Option<BlockId>,
    ) -> LocalId {
        let func = func.into();
        self.emit_with_temp(return_type, |dst| Instruction::FaultableCall {
            dst: Some(dst),
            func,
            args,
            fault_slot,
            overflow_handler,
            divzero_handler,
            bounds_handler,
        })
    }

    /// Void-returning variant of [`Self::fault_call`].
    pub fn fault_call_void(
        &mut self,
        func: impl Into<String>,
        args: Vec<Operand>,
        fault_slot: Place,
        overflow_handler: Option<BlockId>,
        divzero_handler: Option<BlockId>,
        bounds_handler: Option<BlockId>,
    ) {
        self.emit(Instruction::FaultableCall {
            dst: None,
            func: func.into(),
            args,
            fault_slot,
            overflow_handler,
            divzero_handler,
            bounds_handler,
        });
    }

    pub fn call_indirect(
        &mut self,
        callee: Operand,
        args: Vec<Operand>,
        return_type: TypeId,
    ) -> LocalId {
        self.emit_with_temp(return_type, |dst| Instruction::CallIndirect {
            dst: Some(dst),
            callee,
            args,
        })
    }

    pub fn call_extern(
        &mut self,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: TypeId,
    ) -> LocalId {
        let func = func.into();
        self.emit_with_temp(return_type, |dst| Instruction::CallExtern {
            dst: Some(dst),
            func,
            args,
        })
    }

    /// Call an extern function that returns void (output-parameter pattern).
    pub fn call_extern_void(
        &mut self,
        func: impl Into<String>,
        args: Vec<Operand>,
    ) {
        self.emit(Instruction::CallExtern {
            dst: None,
            func: func.into(),
            args,
        });
    }

    pub fn struct_init(
        &mut self,
        type_name: impl Into<String>,
        type_id: TypeId,
        fields: Vec<Operand>,
    ) -> LocalId {
        let type_name = type_name.into();
        self.emit_with_temp(type_id, |dst| Instruction::StructInit {
            dst,
            type_name,
            fields,
        })
    }

    pub fn enum_init(
        &mut self,
        type_name: impl Into<String>,
        variant: impl Into<String>,
        type_id: TypeId,
        fields: Vec<Operand>,
    ) -> LocalId {
        let type_name = type_name.into();
        let variant = variant.into();
        self.emit_with_temp(type_id, |dst| Instruction::EnumInit {
            dst,
            type_name,
            variant,
            fields,
        })
    }

    pub fn tuple_init(&mut self, elements: Vec<Operand>, type_id: TypeId) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::TupleInit { dst, elements })
    }

    pub fn tag_of(&mut self, operand: Operand) -> LocalId {
        self.emit_with_temp(I32_TYPE, |dst| Instruction::TagOf { dst, operand })
    }

    /// Load a field from an enum variant's payload. For resource-type
    /// fields the LIR zeros the source slot after extraction, preventing
    /// shallow-copy double-free when either side drops.
    pub fn enum_field_load_move(
        &mut self,
        base: Place,
        variant: impl Into<String>,
        field: u32,
        type_id: TypeId,
    ) -> LocalId {
        let variant = variant.into();
        self.emit_with_temp(type_id, |dst| Instruction::EnumFieldLoad {
            dst,
            base,
            variant,
            field,
            mode: crate::ir::instructions::EnumFieldLoadMode::Move,
        })
    }

    /// Non-destructive sibling of `enum_field_load_move`. Used by
    /// `lower_pattern_condition` to inspect a nested constructor's payload
    /// without zeroing the source — `emit_pattern_bindings` re-reads the
    /// same source for the actual binding (Snag #34).
    pub fn enum_field_load_borrow(
        &mut self,
        base: Place,
        variant: impl Into<String>,
        field: u32,
        type_id: TypeId,
    ) -> LocalId {
        let variant = variant.into();
        self.emit_with_temp(type_id, |dst| Instruction::EnumFieldLoad {
            dst,
            base,
            variant,
            field,
            mode: crate::ir::instructions::EnumFieldLoadMode::Borrow,
        })
    }

    pub fn heap_alloc(&mut self, type_id: TypeId, allocator: Operand) -> LocalId {
        let ptr_type = type_id; // caller provides the pointer type
        self.emit_with_temp(ptr_type, |dst| Instruction::HeapAlloc {
            dst,
            type_id,
            allocator,
        })
    }

    pub fn heap_alloc_array(
        &mut self,
        type_id: TypeId,
        count: Operand,
        allocator: Operand,
    ) -> LocalId {
        self.emit_with_temp(type_id, |dst| Instruction::HeapAllocArray {
            dst,
            type_id,
            count,
            allocator,
        })
    }

    pub fn dealloc(&mut self, ptr: Operand, allocator: Operand) {
        self.emit(Instruction::Dealloc { ptr, allocator });
    }

    pub fn borrow(&mut self, place: Place, ptr_type: TypeId) -> LocalId {
        self.emit_with_temp(ptr_type, |dst| Instruction::Borrow { dst, place })
    }

    pub fn borrow_mut(&mut self, place: Place, ptr_type: TypeId) -> LocalId {
        self.emit_with_temp(ptr_type, |dst| Instruction::BorrowMut { dst, place })
    }

    /// Emit a Borrow instruction targeting a pre-allocated local.
    pub fn emit_borrow(&mut self, dst: LocalId, place: Place) {
        self.emit(Instruction::Borrow { dst, place });
    }

    /// Emit a BorrowMut instruction targeting a pre-allocated local.
    pub fn emit_borrow_mut(&mut self, dst: LocalId, place: Place) {
        self.emit(Instruction::BorrowMut { dst, place });
    }

    pub fn drop(&mut self, place: Place) {
        self.emit(Instruction::Drop { place });
    }

    pub fn drop_if_alive(&mut self, place: Place) {
        self.emit(Instruction::DropIfAlive { place });
    }

    pub fn move_zero(&mut self, place: Place) {
        self.emit(Instruction::MoveZero { place });
    }

    pub fn load_thread_local(&mut self, name: impl Into<String>, type_id: TypeId) -> LocalId {
        let name = name.into();
        self.emit_with_temp(type_id, |dst| Instruction::LoadThreadLocal { dst, name })
    }

    pub fn push_allocator(&mut self, allocator: Operand) {
        self.emit(Instruction::PushAllocator { allocator });
    }

    pub fn pop_allocator(&mut self) {
        self.emit(Instruction::PopAllocator);
    }

    pub fn inline_c(&mut self, code: String) {
        self.emit(Instruction::InlineC { code });
    }

    pub fn nop(&mut self) {
        self.emit(Instruction::Nop);
    }

    // ---- Terminator emitters ----

    pub fn ret(&mut self, value: Operand) {
        self.set_terminator(Terminator::Return(value));
    }

    pub fn jump(&mut self, target: BlockId) {
        self.set_terminator(Terminator::Jump(target));
    }

    pub fn branch(&mut self, cond: Operand, then_block: BlockId, else_block: BlockId) {
        self.set_terminator(Terminator::Branch {
            cond,
            then_block,
            else_block,
        });
    }

    pub fn switch(&mut self, value: Operand, cases: Vec<(i64, BlockId)>, default: BlockId) {
        self.set_terminator(Terminator::Switch {
            value,
            cases,
            default,
        });
    }

    pub fn invoke(
        &mut self,
        func: impl Into<String>,
        args: Vec<Operand>,
        dst: Option<LocalId>,
        normal: BlockId,
        error: BlockId,
    ) {
        self.set_terminator(Terminator::Invoke {
            func: func.into(),
            args,
            dst,
            normal,
            error,
        });
    }

    pub fn unreachable(&mut self) {
        self.set_terminator(Terminator::Unreachable);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder_creates_return_place() {
        let b = FunctionBuilder::new("test", I32_TYPE, &[]);
        assert_eq!(b.locals.len(), 1);
        assert_eq!(b.locals[0].type_id, I32_TYPE);
    }

    #[test]
    fn builder_params_are_locals() {
        let b = FunctionBuilder::new(
            "add",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );
        assert_eq!(b.locals.len(), 3); // _0=ret, _1=a, _2=b
        assert_eq!(b.locals[1].type_id, I64_TYPE);
        assert_eq!(b.locals[1].name_hint.as_deref(), Some("a"));
        assert_eq!(b.locals[2].name_hint.as_deref(), Some("b"));
        assert_eq!(b.params, vec![I64_TYPE, I64_TYPE]);
    }

    #[test]
    fn builder_add_local() {
        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        let l1 = b.add_local(I64_TYPE, Some("x"));
        let l2 = b.add_local(F64_TYPE, None);
        assert_eq!(l1, LocalId(1));
        assert_eq!(l2, LocalId(2));
        assert_eq!(b.locals.len(), 3);
    }

    #[test]
    fn builder_emit_instructions() {
        let mut b = FunctionBuilder::new(
            "compute",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );

        let sum = b.bin_op(
            BinOp::Add,
            I64_TYPE,
            FunctionBuilder::copy(LocalId(1)),
            FunctionBuilder::copy(LocalId(2)),
        );
        let result = b.call(
            "double",
            vec![FunctionBuilder::copy(sum)],
            I64_TYPE,
        );
        b.assign(
            Place::local(LocalId(0)),
            FunctionBuilder::copy(result),
        );
        b.ret(FunctionBuilder::copy(LocalId(0)));

        let func = b.build();
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.blocks[0].instructions.len(), 3); // binop + call + assign
        assert!(func.blocks[0].terminator.is_some());
    }

    #[test]
    fn builder_multiple_blocks() {
        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        let bb1 = b.new_block();

        // bb0 jumps to bb1
        b.jump(bb1);

        // bb1 returns
        b.switch_to(bb1);
        b.ret(FunctionBuilder::const_unit());

        let func = b.build();
        assert_eq!(func.blocks.len(), 2);
        assert!(matches!(
            func.blocks[0].terminator,
            Some(Terminator::Jump(BlockId(1)))
        ));
        assert!(matches!(
            func.blocks[1].terminator,
            Some(Terminator::Return(_))
        ));
    }

    #[test]
    fn builder_branch_and_merge() {
        let mut b = FunctionBuilder::new(
            "abs",
            I64_TYPE,
            &[(I64_TYPE, Some("x"))],
        );

        let then_bb = b.new_block();  // bb1
        let else_bb = b.new_block();  // bb2
        let merge_bb = b.new_block(); // bb3

        // bb0: branch on x >= 0
        let cond = b.cmp(
            CmpOp::Ge,
            I64_TYPE,
            FunctionBuilder::copy(LocalId(1)),
            FunctionBuilder::const_i64(0),
        );
        b.branch(FunctionBuilder::copy(cond), then_bb, else_bb);

        // bb1 (then): _0 = x, jump merge
        b.switch_to(then_bb);
        b.assign(
            Place::local(LocalId(0)),
            FunctionBuilder::copy(LocalId(1)),
        );
        b.jump(merge_bb);

        // bb2 (else): _0 = -x, jump merge
        b.switch_to(else_bb);
        let neg = b.un_op(UnOp::Neg, I64_TYPE, FunctionBuilder::copy(LocalId(1)));
        b.assign(
            Place::local(LocalId(0)),
            FunctionBuilder::copy(neg),
        );
        b.jump(merge_bb);

        // bb3 (merge): return _0
        b.switch_to(merge_bb);
        b.ret(FunctionBuilder::copy(LocalId(0)));

        let func = b.build();
        assert_eq!(func.blocks.len(), 4);
        assert!(matches!(
            func.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
        assert!(matches!(
            func.blocks[1].terminator,
            Some(Terminator::Jump(BlockId(3)))
        ));
        assert!(matches!(
            func.blocks[2].terminator,
            Some(Terminator::Jump(BlockId(3)))
        ));
        assert!(matches!(
            func.blocks[3].terminator,
            Some(Terminator::Return(_))
        ));
    }
}
