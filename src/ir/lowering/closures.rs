//! Closure lowering for GIR.
//!
//! Transforms `Expr::Closure` into:
//! 1. A `__Closure_N` struct type holding captured variables
//! 2. A `__Closure_N__call` function with env pointer + params
//! 3. A `StructInit` at the creation site to build the env

use rustc_hash::FxHashSet;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::ir::Function;
use crate::parser::ast::{self, ClosureParam, Expr, Ownership, Pattern, Stmt};
use crate::span::Spanned;

use super::context::{LoweringContext, ParamABI};
use super::exprs::{lower_expr, lower_stmt_as_tail_value};

/// How a variable is captured by a closure.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CaptureMode {
    /// Copy the value into the closure struct.
    ByValue,
    /// Store a pointer to the outer variable (for mutation).
    ByMutRef,
}

/// Info about a single captured variable.
#[derive(Debug, Clone)]
pub struct CaptureInfo {
    pub name: String,
    pub type_id: TypeId,
    pub local_id: LocalId,
    pub mode: CaptureMode,
}

/// A lifted closure pending emission as GIR types/functions.
#[derive(Debug)]
pub struct LiftedClosure {
    pub id: usize,
    pub struct_type_name: String,
    pub call_fn_name: String,
    pub captures: Vec<CaptureInfo>,
    pub param_names: Vec<String>,
    pub param_types: Vec<TypeId>,
    pub param_ownerships: Vec<Ownership>,
    pub return_type: TypeId,
    /// The closure body AST (cloned for deferred lowering).
    pub body: Spanned<Expr>,
    /// Expected type context at the point of closure creation (for Ok/Error/Some/None resolution).
    pub expected_type: Option<TypeId>,
}

/// Manages closure lowering state.
pub struct ClosureLowering {
    next_id: usize,
    pub lifted: Vec<LiftedClosure>,
}

impl Default for ClosureLowering {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosureLowering {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            lifted: Vec::new(),
        }
    }

    /// Lower a closure expression, returning an operand for the closure env struct.
    ///
    /// This creates the closure struct type and registers the lifted closure
    /// for later function emission. Returns a StructInit operand.
    pub fn lower_closure(
        &mut self,
        ctx: &mut LoweringContext,
        builder: &mut FunctionBuilder,
        params: &[Spanned<ClosureParam>],
        body: &Spanned<Expr>,
        is_move: bool,
        closure_span: crate::span::Span,
    ) -> Operand {
        let id = self.next_id;
        self.next_id += 1;

        let struct_name = format!("__Closure_{id}");
        let call_fn_name = format!("__Closure_{id}__call");

        // Collect free variables from the closure body
        let param_names: FxHashSet<String> = params.iter()
            .map(|p| p.node.name.node.clone())
            .collect();

        let free_vars = collect_free_vars(ctx, &body.node, &param_names);

        // Detect mutations to determine capture mode
        let mutated = if is_move {
            FxHashSet::default()
        } else {
            detect_mutations(&body.node, &param_names)
        };

        // Build capture info
        let captures: Vec<CaptureInfo> = free_vars.into_iter()
            .map(|(name, type_id, local_id)| {
                let mode = if mutated.contains(&name) {
                    CaptureMode::ByMutRef
                } else {
                    CaptureMode::ByValue
                };
                CaptureInfo { name, type_id, local_id, mode }
            })
            .collect();

        // Create the closure struct TypeDef.
        // CoW Ptr(T) aliases are materialized to owned T — the closure must
        // capture an independent snapshot, not a raw pointer that can dangle
        // if the source is later mutated and the alias is severed.
        let fields: Vec<StructField> = captures.iter()
            .map(|cap| {
                let field_type = match cap.mode {
                    CaptureMode::ByValue => {
                        // Resolve Ptr(T) → T for CoW aliases
                        ctx.pointee_type(cap.type_id).unwrap_or(cap.type_id)
                    }
                    CaptureMode::ByMutRef => ctx.type_registry.insert(GirType::MutPtr(cap.type_id)),
                };
                StructField {
                    name: cap.name.clone(),
                    type_id: field_type,
                }
            })
            .collect();

        let type_def = TypeDef {
            name: struct_name.clone(),
            kind: TypeDefKind::Struct(StructDef { fields }),
            metadata: TypeMetadata {
                // Marks this as a closure-env struct: captured locals at non-last-use
                // are lifetime-tied aliases of outer-scope values (no independent
                // ownership — outer-scope drops handle cleanup). The consume-site
                // validator skips StructInit fields for closure-env destinations so
                // the bitwise-copy alias pattern doesn't fire OwnedLiveSourceConsumed.
                // See validate.rs `validate_consume` and docs/devbook/12-gir-lowering.md (closure lowering and capture).
                is_closure_env: true,
                ..TypeMetadata::default()
            },
        };
        ctx.type_registry.add_type_def(type_def);
        let struct_type_id = ctx.type_registry.insert(GirType::Named(struct_name.clone()));
        ctx.type_mapper.register_named(struct_name.clone(), struct_type_id);

        // Map closure params to GIR types
        let closure_param_names: Vec<String> = params.iter()
            .map(|p| p.node.name.node.clone())
            .collect();
        let closure_param_types: Vec<TypeId> = params.iter()
            .enumerate()
            .map(|(i, p)| {
                if let Some(ref ty) = p.node.type_ {
                    // map_ast_type (immutable) returns UNIT_TYPE for types
                    // that aren't yet registered, which silently breaks
                    // closures whose params reference compound types — most
                    // visibly Tuple/Array/Slice that haven't been monomorphized
                    // yet because the closure is the first place they appear.
                    // Example: `(((int, int) p): p._0 + p._1)((3, 4))` (IIFE)
                    // had no other site introducing `Tuple__int64_t__int64_t`
                    // to the registry before the closure was lowered, so the
                    // param ended up `unit` and the body folded to `add unit
                    // const unit, const unit`. Fall through to the mutable
                    // mapper so the type registers on first use.
                    ctx.type_mapper.map_ast_type_mut(&ty.node, &mut ctx.type_registry)
                } else if i < ctx.func_state.closure_param_type_hints.len() {
                    // Use hint from enclosing higher-order method call (e.g., filter/map/fold)
                    ctx.func_state.closure_param_type_hints[i]
                } else {
                    I64_TYPE // fallback for untyped params
                }
            })
            .collect();

        // Infer return type from body (simplified — use I64 fallback).
        //
        // Tier 1c: temporarily register closure params as locals in ctx
        // so the inference's `Expr::Identifier` lookup resolves param
        // references to their hinted types. Without this, a closure body
        // like `e.to_upper()` (`e: String` from the call site's
        // closure_param_type_hints) fails to resolve `e` and falls back
        // to I64_TYPE → MethodCall recv_type = I64 → return type =
        // I64_TYPE, even though the actual call fn body returns String.
        // The mismatch makes `map_err` build a wrongly-sized
        // `Result__T__int64_t` and the destination memcpy overruns the
        // smaller source slot.
        let saved_params: Vec<(String, Option<(LocalId, TypeId)>)> = params.iter().enumerate()
            .map(|(i, p)| {
                let name = p.node.name.node.clone();
                let prior = ctx.func_state.locals.get(&name).copied();
                ctx.register_local(&name, LocalId(u32::MAX - i as u32), closure_param_types[i]);
                (name, prior)
            })
            .collect();
        let body_return_type = infer_closure_return_type(ctx, body);
        // Round XIX Track N2 cell H: when the ambient expected type is a
        // `Callable[R(...)]` / `FnPtr { return_type: R }`, prefer R over the
        // body-only inference. Body inference of `(): Box.new(Concrete)` yields
        // `Box[Concrete]`; the ambient `Callable[Box[Trait]()]` requires
        // `Box[Trait]` so LocalId(0) is trait-box-sized and
        // `try_trait_object_construct` can fire. Snag #51 still forbids the
        // polluted auto-fallback (I64) path — only peel a real FnPtr.
        let return_type = match ctx.func_state.expected_type.and_then(|et| {
            match ctx.type_registry.get(et) {
                Some(GirType::FnPtr { return_type: ret, .. }) if *ret != UNIT_TYPE => Some(*ret),
                _ => None,
            }
        }) {
            Some(ambient_ret) => ambient_ret,
            None => body_return_type,
        };
        // Restore the prior locals state for these names.
        for (name, prior) in saved_params {
            if let Some(entry) = prior {
                ctx.func_state.locals.insert(name, entry);
            } else {
                ctx.func_state.locals.remove(&name);
            }
        }

        // Collect parameter ownerships for ABI computation
        let closure_param_ownerships: Vec<Ownership> = params.iter()
            .map(|p| p.node.ownership)
            .collect();

        // Register the call function signature (base types — lower_call_arg expects base)
        let env_ptr_type = ctx.type_registry.insert(GirType::Ptr(struct_type_id));
        let mut sig_params = vec![env_ptr_type];
        sig_params.extend_from_slice(&closure_param_types);
        ctx.fn_sigs.insert(call_fn_name.clone(), (sig_params, return_type));

        // Register unified ParamABI: resource params pass by Ptr, like regular functions.
        let param_abis: Vec<ParamABI> = closure_param_types.iter()
            .zip(closure_param_ownerships.iter())
            .map(|(&base, own)| ctx.compute_param_abi(base, *own))
            .collect();
        let mut all_abis = vec![ParamABI::ByPtr]; // env pointer
        all_abis.extend(param_abis);
        ctx.fn_param_abis.insert(call_fn_name.clone(), all_abis);

        // Register this closure's info for call dispatch.
        // Only store ByValue captures — ByMutRef captures cannot be copied across
        // thread boundaries, so they are excluded from the spawn wrapper signature.
        let spawn_captures: Vec<(String, TypeId, u32)> = captures.iter()
            .enumerate()
            .filter(|(_, c)| c.mode == CaptureMode::ByValue)
            .map(|(i, c)| {
                // Resolve Ptr(T) → T for CoW aliases (matches struct field type)
                let ty = ctx.pointee_type(c.type_id).unwrap_or(c.type_id);
                (c.name.clone(), ty, i as u32)
            })
            .collect();
        ctx.register_closure_info(
            struct_name.clone(),
            call_fn_name.clone(),
            struct_type_id,
            spawn_captures,
        );

        // Store the lifted closure for later function emission
        self.lifted.push(LiftedClosure {
            id,
            struct_type_name: struct_name.clone(),
            call_fn_name,
            captures: captures.clone(),
            param_names: closure_param_names,
            param_types: closure_param_types,
            param_ownerships: closure_param_ownerships,
            return_type,
            body: body.clone(),
            expected_type: ctx.func_state.expected_type,
        });

        // Emit the creation-site StructInit.
        // CoW Ptr(T) captures are cloned to produce an independent owned T,
        // preventing stale pointers if the source is mutated after capture.
        //
        // For owned by-value resource-typed locals captured at last-use, MOVE
        // the source into the struct (unregister its drop, MoveZero its slot
        // AFTER the StructInit reads it). Otherwise the source's scope-exit
        // drop fires on a buffer the closure env still aliases → heap-UAF on
        // closure invocation.
        let mut pending_move_zero: Vec<crate::ir::LocalId> = Vec::new();
        let field_operands: Vec<Operand> = captures.iter()
            .map(|cap| {
                match cap.mode {
                    CaptureMode::ByValue => {
                        // If this capture is a CoW Ptr(T) alias, clone through
                        // the Ptr to produce an owned T for the closure struct.
                        // Exception: if the capture is the last use of the variable,
                        // the closure takes ownership via move — no clone needed.
                        if let Some(inner) = ctx.pointee_type(cap.type_id) {
                            let is_last_use = ctx.is_last_use_at(&cap.name, closure_span);
                            if is_last_use {
                                // Last use — auto-deref Ptr(T) to T (move ownership)
                                let deref_local = builder.add_local(inner, None);
                                builder.assign(Place::local(deref_local),
                                    Operand::Move(Place::local(cap.local_id)));
                                ctx.move_zero_and_mark(builder, cap.local_id);
                                return FunctionBuilder::copy(deref_local);
                            }
                        }
                        // Owned by-value resource captured at last-use: defer
                        // MoveZero to after StructInit so the field init can
                        // still read the source.
                        // Only applies to CopySemantics::Resource types.
                        // CopySemantics::Trivial types (Shared, Channel, Weak) are
                        // bitwise-copyable at GIR level; the runtime handles their
                        // refcounts via explicit drops. The consume-site validator
                        // skips Trivial types (see validate.rs).
                        if ctx.pointee_type(cap.type_id).is_none()
                            && ctx.type_registry.is_resource_type(cap.type_id)
                            && ctx.drops.is_registered(cap.local_id)
                            && ctx.is_last_use_at(&cap.name, closure_span)
                        {
                            ctx.drops.unregister(cap.local_id);
                            pending_move_zero.push(cap.local_id);
                            return FunctionBuilder::copy(cap.local_id);
                        }
                        // Unified boundary clone: `ensure_owned_at_boundary`
                        // handles Ptr(T) borrows, ref-state locals, and
                        // Untracked resource locals (via Tier 2a 2B extension).
                        ctx.ensure_owned_at_boundary(
                            builder,
                            FunctionBuilder::copy(cap.local_id),
                            closure_span,
                            crate::ir::ImplicitCloneReason::ClosureCapture,
                        )
                    }
                    CaptureMode::ByMutRef => {
                        // Borrow the captured variable
                        let ptr_type = ctx.type_registry.insert(GirType::MutPtr(cap.type_id));
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow_mut(ptr_local, Place::local(cap.local_id));
                        FunctionBuilder::copy(ptr_local)
                    }
                }
            })
            .collect();

        let dst = builder.struct_init(&struct_name, struct_type_id, field_operands);
        // After StructInit has read the moved sources, MoveZero their slots
        // so the scope-exit drop tracker doesn't free buffers the closure
        // env now owns.
        for local in pending_move_zero {
            ctx.move_zero_and_mark(builder, local);
        }
        FunctionBuilder::copy(dst)
    }
}

/// Emit the `__Closure_N__call` function for a lifted closure. Returns the Function.
pub fn emit_closure_call_function(
    ctx: &mut LoweringContext,
    closure: &LiftedClosure,
) -> Function {
    let struct_type_id = ctx.type_mapper.lookup_named(&closure.struct_type_name)
        .unwrap_or(UNIT_TYPE);
    let env_ptr_type = ctx.type_registry.insert(GirType::Ptr(struct_type_id));

    // Build params: env pointer + closure params (resolved to Ptr for resource types)
    let mut params: Vec<(TypeId, Option<&str>)> = vec![(env_ptr_type, Some("__env"))];
    for (i, (name, type_id)) in closure.param_names.iter().zip(closure.param_types.iter()).enumerate() {
        let ownership = closure.param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
        let resolved = ctx.resolve_param_type(*type_id, ownership);
        params.push((resolved, Some(name.as_str())));
    }

    let mut builder = FunctionBuilder::new(
        &closure.call_fn_name,
        closure.return_type,
        &params,
    );

    // The closure body is a bare `Spanned<Expr>` (`LiftedClosure::body`), so it
    // reaches the prescans as `FnBodyAst::Expr` — an `Expr::Block` body unwraps
    // to its own statements, anything else has none. Before centralisation this
    // path reset per-function state but ran NO prescans, so a CoW view bound
    // inside a closure body was never materialised before a reallocating
    // mutation (see `functions::begin_function_body`).
    super::functions::begin_function_body(
        ctx,
        super::functions::FnBodyAst::Expr(&closure.body.node),
    );

    // Save the outer function's drop state and push a fresh Function scope
    // for the closure body. Without this, locals registered during closure
    // lowering would land in the outer function's scope and not be dropped.
    let saved_drops = std::mem::replace(&mut ctx.drops, super::drops::DropElaborator::new());
    ctx.drops.push_scope(super::drops::DropScopeKind::Function);

    // _1 = __env (pointer to closure struct)
    let env_local = LocalId(1);
    ctx.register_local("__env", env_local, env_ptr_type);

    // Load captures from the env struct
    for (i, cap) in closure.captures.iter().enumerate() {
        match cap.mode {
            CaptureMode::ByValue => {
                // Phase C FieldLoad migration (2026-05-06): resource-typed
                // ByValue captures load as Ptr(cap.type_id) — a borrow into
                // the env's storage. The env owns the data across calls; the
                // closure body reads through the borrow. Auto-clone fires at
                // ownership boundaries (call args via auto_clone_if_ptr,
                // VarDecl Ptr→T via clone_fn_for_ptr, etc.). Mirrors the
                // owned-base lower_field_access path. Non-resource captures
                // (primitives) stay value-typed (bit-copy is correct).
                if ctx.type_registry.is_resource_type(cap.type_id) {
                    let ptr_type = ctx.type_registry.insert(GirType::Ptr(cap.type_id));
                    let dst = builder.field_load(
                        Place::local(env_local),
                        i as u32,
                        ptr_type,
                    );
                    ctx.register_local(&cap.name, dst, ptr_type);
                    ctx.set_field_borrow(&mut builder, dst, env_local, i as u32);
                } else {
                    let dst = builder.field_load(
                        Place::local(env_local),
                        i as u32,
                        cap.type_id,
                    );
                    ctx.register_local(&cap.name, dst, cap.type_id);
                }
            }
            CaptureMode::ByMutRef => {
                // Load pointer field from env struct
                let ptr_type = ctx.type_registry.insert(GirType::MutPtr(cap.type_id));
                let ptr_local = builder.field_load(
                    Place::local(env_local),
                    i as u32,
                    ptr_type,
                );
                // Register with pointer type; reads/writes in the body will
                // go through Deref projections (checked via is_param_borrow_unique).
                ctx.register_local(&cap.name, ptr_local, ptr_type);
                ctx.set_param_borrow_unique(&mut builder, ptr_local);
            }
        }
    }

    // Register closure params as locals with resolved types (Ptr for resource types)
    let param_start = 2u32; // _0=return, _1=env, _2...=params
    for (i, (name, type_id)) in closure.param_names.iter()
        .zip(closure.param_types.iter())
        .enumerate()
    {
        let local_id = LocalId(param_start + i as u32);
        let ownership = closure.param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
        let resolved = ctx.resolve_param_type(*type_id, ownership);
        ctx.register_local(name, local_id, resolved);
        if ctx.is_ref_param(*type_id, ownership) {
            ctx.set_bare_param(&mut builder, local_id);
        } else if ctx.is_mut_ref_param(*type_id, ownership) {
            // & or ! MutPtr param. Per §6.2: typed shape Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, local_id);
            if matches!(ownership, Ownership::Move)
                && ctx.type_registry.is_resource_type(*type_id)
            {
                ctx.set_owning_param(&mut builder, local_id);
                ctx.drops.register_owning_param(local_id, *type_id, &ctx.type_registry);
            }
        }
    }

    // Body-level expected_type is the closure's own return type — this is
    // what the body is contractually producing. Sub-expressions sizing their
    // result slot from expected_type (match-as-expression, if-as-expression,
    // Ok/Error/Some/None constructors) read the closure's return type, not
    // whatever the outer surroundings happened to have on the stack.
    //
    // Snag #51 (2026-05-16) tail of the family: when an `auto`-typed VarDecl
    // wraps a closure (`auto mk = (...)`), the outer var-decl sets
    // expected_type=Some(I64) (the auto fallback for closure-RHS, since the
    // closure's type isn't inferable without lowering it first). Pre-fix,
    // that I64 was captured on the LiftedClosure and restored here as the
    // body's expected_type. The body's match-as-tail then sized its
    // result_local from I64 and short-circuited refinement
    // (`result_type_refined = expected_type.is_some() && !is_result_wrapper`),
    // forcing String/enum-arm values to memcpy into an I64-sized slot.
    // The C codegen surfaced this as `Str = int64_t`.
    //
    // The closure's `return_type` is what we inferred from the body itself
    // (via `infer_closure_return_type`, which is body-driven and free of
    // outer-context pollution). Use that. If the closure was created in a
    // typed target context where Ok/Some need an enclosing Result/Option
    // type to monomorphize (the original motivation for capturing
    // `expected_type`), that information is now carried by `return_type`
    // when the inferer recognises an Ok/Error/Some/None tail.
    let prev_expected = ctx.func_state.expected_type;
    ctx.func_state.expected_type = if closure.return_type == UNIT_TYPE {
        None
    } else {
        Some(closure.return_type)
    };

    // Lower the closure body. Both the expression-body case (`(x): x + 1`)
    // and the block-with-tail-value case (`(x):\n    let y = ...\n    x + y`,
    // `(x):\n    match x: case _: 1`, `(x):\n    if x: 1 else: 0`) funnel
    // through `emit_implicit_return` so the trailing value reaches
    // LocalId(0). A block whose tail isn't a recognised value-producing
    // form falls through to the default `ret` emission below.
    //
    // Snag #51 (2026-05-16): the block-arm originally only recognised
    // `Stmt::Expr` as a tail value, so multi-stmt closure bodies ending
    // in `match`/`if` saw the value silently dropped and `LocalId(0)`
    // returned its zero-init default. The tail-value dispatch lives in
    // `lower_stmt_as_tail_value` and is now shared with `lower_block_expr`
    // so the closure path and the block-as-expression path can't diverge
    // again — new tail-value shapes added there flow to both.
    let mut tail_handled = false;
    match &closure.body.node {
        Expr::Block(block) => {
            let stmts = &block.stmts;
            if !stmts.is_empty() {
                for stmt in &stmts[..stmts.len() - 1] {
                    super::stmts::lower_stmt(ctx, &mut builder, stmt);
                }
                let last = &stmts[stmts.len() - 1];
                if let Some(result) = lower_stmt_as_tail_value(ctx, &mut builder, last) {
                    emit_implicit_return(ctx, &mut builder, closure, result, last.span);
                    tail_handled = true;
                }
            }
        }
        _ => {
            let body_span = closure.body.span;
            let result = lower_expr(ctx, &mut builder, &closure.body);
            emit_implicit_return(ctx, &mut builder, closure, result, body_span);
            tail_handled = true;
        }
    }
    if !tail_handled {
        let last_block_idx = builder.current_block.0 as usize;
        if builder.blocks[last_block_idx].terminator.is_none() {
            if closure.return_type == UNIT_TYPE {
                builder.ret(FunctionBuilder::const_unit());
            } else {
                builder.ret(FunctionBuilder::copy(LocalId(0)));
            }
        }
    }

    ctx.func_state.expected_type = prev_expected;

    // Restore the outer function's drop state
    ctx.drops = saved_drops;

    ctx.flush_ownership_to_locals(&mut builder);
    let mut func = builder.build();
    // Update the function's return_type to match the actual local[0] type —
    // EXCEPT when LocalId(0) was deliberately typed as a Box[Trait] ambient
    // return and the body produced Box[Concrete] that was packed into it.
    // Re-pinning to Concrete would desync the call site (Round XIX N2 cell H).
    let actual_ret = func.locals[0].type_id;
    if actual_ret != func.return_type {
        let declared_is_trait_box = is_trait_box_type(ctx, func.return_type);
        if !declared_is_trait_box {
            func.return_type = actual_ret;
        }
    }
    func
}

/// True when `tid` is a `Box__X` whose inner has a registered `<X>_TraitObj`
/// (i.e. a trait-object box). Used to protect LocalId(0) / return_type from
/// being re-pinned to Box[Concrete] after a pack.
fn is_trait_box_type(ctx: &LoweringContext, tid: TypeId) -> bool {
    let name = match ctx.type_registry.get(tid) {
        Some(GirType::Named(n)) => n,
        _ => return false,
    };
    if !ctx.type_registry.is_box(tid) {
        return false;
    }
    let inner = match name.strip_prefix("Box__") {
        Some(i) => i,
        None => return false,
    };
    ctx.type_registry
        .get_type_def(&format!("{inner}_TraitObj"))
        .is_some()
}

/// Emit ownership-boundary clone, return-type override, move/copy assign
/// to LocalId(0), scope-exit drops, and the terminating `ret`. Shared by
/// both the bare-expression body (`(x): x + 1`) and the block-tail-
/// expression body (`(x):\n    let y = ...\n    x + y`). Without this
/// helper, the block path used to leave LocalId(0) uninitialized and the
/// SSA validator (or downstream codegen) tripped on the bare ret.
fn emit_implicit_return(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    closure: &LiftedClosure,
    result: Operand,
    body_span: crate::span::Span,
) {
    let mut result = ctx.ensure_owned_at_boundary(
        builder,
        result,
        body_span,
        crate::ir::ImplicitCloneReason::ReturnFromBorrow,
    );
    // Round XIX Track N2 cell H Class B: pack Box[Concrete]→Box[Trait] into
    // the declared return type before the return-slot assign, so LIR construct
    // fires on SlotStore into LocalId(0).
    if let Some(GirType::Named(ref n)) = ctx.type_registry.get(closure.return_type).cloned() {
        result = super::exprs::pack_trait_object_for_smart_ptr_ctor(ctx, builder, result, &n);
    }
    let actual_type = super::exprs::infer_operand_type_full(ctx, &result, builder);
    // Do NOT override LocalId(0) to Box[Concrete] when the declared return is
    // a trait-box — that kills try_trait_object_construct (SIGILL cell H).
    let declared_is_trait_box = is_trait_box_type(ctx, closure.return_type);
    let should_override = actual_type != closure.return_type
        && actual_type != UNIT_TYPE
        && !declared_is_trait_box
        && !(actual_type == ctx.type_mapper.owned_string_type
             && closure.return_type == ctx.type_mapper.owned_string_type);
    if should_override {
        builder.locals[0].type_id = actual_type;
    }
    let returned_local = match &result {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
        _ => None,
    };
    let use_move = if let Some(local) = returned_local {
        ctx.type_registry.needs_drop(builder.local_type(local))
    } else { false };
    if use_move {
        let r = std::mem::replace(&mut result, Operand::Constant(Constant::Unit));
        builder.assign_mode(
            crate::ir::instructions::AssignMode::Move,
            Place::local(LocalId(0)),
            r,
        );
        if let Some(local) = returned_local {
            ctx.move_zero_and_mark(builder, local);
        }
    } else {
        builder.assign(Place::local(LocalId(0)), result);
    }
    ctx.drops.emit_early_exit_drops(
        builder,
        &ctx.type_registry,
        super::drops::DropScopeKind::Function,
        returned_local,
    );
    builder.ret(FunctionBuilder::copy(LocalId(0)));
}

/// Collect free variables referenced in a closure body.
/// Returns (name, type_id, local_id) for each free variable.
fn collect_free_vars(
    ctx: &LoweringContext,
    expr: &Expr,
    param_names: &FxHashSet<String>,
) -> Vec<(String, TypeId, LocalId)> {
    let mut collector = FreeVarCollector {
        ctx,
        param_names,
        local_names: FxHashSet::default(),
        found: Vec::new(),
        seen: FxHashSet::default(),
    };
    collector.visit_expr(expr);
    collector.found
}

struct FreeVarCollector<'a> {
    ctx: &'a LoweringContext<'a>,
    param_names: &'a FxHashSet<String>,
    local_names: FxHashSet<String>,
    found: Vec<(String, TypeId, LocalId)>,
    seen: FxHashSet<String>,
}

impl FreeVarCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Identifier(name) => {
                if !self.param_names.contains(name)
                    && !self.local_names.contains(name)
                    && !self.seen.contains(name)
                {
                    if let Some((local_id, type_id)) = self.ctx.lookup_local(name) {
                        self.seen.insert(name.clone());
                        self.found.push((name.clone(), type_id, local_id));
                    }
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.visit_expr(&left.node);
                self.visit_expr(&right.node);
            }
            Expr::UnaryOp { operand, .. } => {
                self.visit_expr(&operand.node);
            }
            Expr::Call { callee, args, .. } => {
                self.visit_expr(&callee.node);
                for arg in args {
                    self.visit_expr(&arg.node.value.node);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.visit_expr(&receiver.node);
                for arg in args {
                    self.visit_expr(&arg.node.value.node);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.visit_expr(&object.node);
            }
            Expr::Index { object, index } => {
                self.visit_expr(&object.node);
                self.visit_expr(&index.node);
            }
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                self.visit_expr(&condition.node);
                self.visit_expr(&then_branch.node);
                for (cond, body) in elif_branches {
                    self.visit_expr(&cond.node);
                    self.visit_expr(&body.node);
                }
                if let Some(eb) = else_branch {
                    self.visit_expr(&eb.node);
                }
            }
            Expr::Block(block) => {
                self.visit_block(block);
            }
            Expr::StructLiteral { args, .. } => {
                for arg in args {
                    self.visit_expr(&arg.node);
                }
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
                for elem in elems {
                    self.visit_expr(&elem.node);
                }
            }
            Expr::Move { expr: inner }
            | Expr::Propagate { expr: inner }
            | Expr::MutableBorrow { expr: inner } => {
                self.visit_expr(&inner.node);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { self.visit_expr(&s.node); }
                if let Some(e) = end { self.visit_expr(&e.node); }
            }
            Expr::StringLiteral(lit, _) => {
                // Visit interpolated variable references in f-strings
                for seg in &lit.segments {
                    if let crate::lexer::token::StringSegment::Interpolation(var_name, _) = seg {
                        // The interpolation text may be a simple identifier or a complex
                        // expression. For simple identifiers, check directly. For complex
                        // expressions (e.g., "x + 1", "obj.field"), extract leading identifier.
                        let ident = var_name.split(|c: char| !c.is_alphanumeric() && c != '_')
                            .next()
                            .unwrap_or("");
                        if !ident.is_empty()
                            && !self.param_names.contains(ident)
                            && !self.local_names.contains(ident)
                            && !self.seen.contains(ident)
                        {
                            if let Some((local_id, type_id)) = self.ctx.lookup_local(ident) {
                                self.seen.insert(ident.to_string());
                                self.found.push((ident.to_string(), type_id, local_id));
                            }
                        }
                    }
                }
            }
            Expr::Closure { body, .. } => {
                // Don't descend into nested closures — they have their own captures
                let _ = body;
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.visit_stmt(&stmt.node);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl { pattern, value, .. } => {
                // Register the new local binding
                if let Pattern::Binding(name) = &pattern.node {
                    self.local_names.insert(name.clone());
                }
                self.visit_expr(&value.node);
            }
            Stmt::Assign { target, value } => {
                self.visit_expr(&target.node);
                self.visit_expr(&value.node);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.visit_expr(&target.node);
                self.visit_expr(&value.node);
            }
            Stmt::Return(Some(expr)) | Stmt::Expr(expr) | Stmt::Throw(expr) => {
                self.visit_expr(&expr.node);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.visit_expr(&condition.node);
                self.visit_block(then_body);
                for (cond, body) in elif_branches {
                    self.visit_expr(&cond.node);
                    self.visit_block(body);
                }
                if let Some(eb) = else_body {
                    self.visit_block(eb);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.visit_expr(&condition.node);
                self.visit_block(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.visit_expr(&iterable.node);
                self.visit_block(body);
            }
            _ => {}
        }
    }
}

/// Detect which free variables are mutated inside the closure body.
fn detect_mutations(
    expr: &Expr,
    param_names: &FxHashSet<String>,
) -> FxHashSet<String> {
    let mut mutated = FxHashSet::default();
    detect_mutations_inner(expr, param_names, &mut mutated);
    mutated
}

fn detect_mutations_inner(
    expr: &Expr,
    param_names: &FxHashSet<String>,
    mutated: &mut FxHashSet<String>,
) {
    match expr {
        Expr::Block(block) => {
            for stmt in &block.stmts {
                detect_mutations_in_stmt(&stmt.node, param_names, mutated);
            }
        }
        _ => {} // Single expression body can't mutate
    }
}

fn detect_mutations_in_stmt(
    stmt: &Stmt,
    param_names: &FxHashSet<String>,
    mutated: &mut FxHashSet<String>,
) {
    match stmt {
        Stmt::Assign { target, value } => {
            if let Expr::Identifier(name) = &target.node {
                if !param_names.contains(name) {
                    mutated.insert(name.clone());
                }
            }
            detect_mutations_in_expr(&value.node, param_names, mutated);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                if !param_names.contains(name) {
                    mutated.insert(name.clone());
                }
            }
            detect_mutations_in_expr(&value.node, param_names, mutated);
        }
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            detect_mutations_in_expr(&condition.node, param_names, mutated);
            for s in &then_body.stmts {
                detect_mutations_in_stmt(&s.node, param_names, mutated);
            }
            for (cond, body) in elif_branches {
                detect_mutations_in_expr(&cond.node, param_names, mutated);
                for s in &body.stmts {
                    detect_mutations_in_stmt(&s.node, param_names, mutated);
                }
            }
            if let Some(eb) = else_body {
                for s in &eb.stmts {
                    detect_mutations_in_stmt(&s.node, param_names, mutated);
                }
            }
        }
        Stmt::While { condition, body, .. } => {
            detect_mutations_in_expr(&condition.node, param_names, mutated);
            for s in &body.stmts {
                detect_mutations_in_stmt(&s.node, param_names, mutated);
            }
        }
        Stmt::For { iterable, body, .. } => {
            detect_mutations_in_expr(&iterable.node, param_names, mutated);
            for s in &body.stmts {
                detect_mutations_in_stmt(&s.node, param_names, mutated);
            }
        }
        Stmt::Expr(expr) | Stmt::Return(Some(expr)) => {
            detect_mutations_in_expr(&expr.node, param_names, mutated);
        }
        _ => {}
    }
}

fn detect_mutations_in_expr(
    expr: &Expr,
    param_names: &FxHashSet<String>,
    mutated: &mut FxHashSet<String>,
) {
    match expr {
        Expr::Block(block) => {
            for s in &block.stmts {
                detect_mutations_in_stmt(&s.node, param_names, mutated);
            }
        }
        Expr::Closure { .. } => {
            // Nested closures have their own scope
        }
        _ => {}
    }
}

/// Walk block statements looking for an explicit `return expr`, recursing into
/// if/match/while/for/loop bodies. Returns the inferred type of the first return found.
fn find_return_type_in_block(ctx: &mut LoweringContext, stmts: &[Spanned<Stmt>]) -> Option<TypeId> {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Return(Some(expr)) => {
                return Some(infer_closure_return_type(ctx, expr));
            }
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                if let Some(t) = find_return_type_in_block(ctx, &then_body.stmts) {
                    return Some(t);
                }
                for (_, elif_body) in elif_branches {
                    if let Some(t) = find_return_type_in_block(ctx, &elif_body.stmts) {
                        return Some(t);
                    }
                }
                if let Some(eb) = else_body {
                    if let Some(t) = find_return_type_in_block(ctx, &eb.stmts) {
                        return Some(t);
                    }
                }
            }
            Stmt::Match { arms, else_arm, .. } => {
                for item in arms {
                    if let ast::MatchItem::Arm(arm) = item {
                        if let Expr::Block(block) = &arm.body.node {
                            if let Some(t) = find_return_type_in_block(ctx, &block.stmts) {
                                return Some(t);
                            }
                        }
                    }
                }
                if let Some(eb) = else_arm {
                    if let Some(t) = find_return_type_in_block(ctx, &eb.stmts) {
                        return Some(t);
                    }
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
                if let Some(t) = find_return_type_in_block(ctx, &body.stmts) {
                    return Some(t);
                }
            }
            Stmt::For { body, .. } => {
                if let Some(t) = find_return_type_in_block(ctx, &body.stmts) {
                    return Some(t);
                }
            }
            _ => {}
        }
    }
    None
}

/// Infer the type of a trailing statement when it appears as the tail
/// value of a closure body's block. Mirrors `lower_stmt_as_tail_value`'s
/// recognised shapes: `Stmt::Expr`, `Stmt::Match`, `Stmt::If`. For
/// match/if statements at the tail, returns the type of the first
/// non-divergent arm/branch body — sufficient for picking the closure's
/// return-slot size; downstream `emit_implicit_return` overrides the
/// slot's actual type when arms produce a more specific result.
///
/// Returns `None` for any other statement form (block-as-Unit fallback).
/// Adding a new tail-value shape: extend this function, `lower_stmt_as_tail_value`,
/// and any sibling dispatcher in lockstep.
fn infer_stmt_tail_type(ctx: &mut LoweringContext, stmt: &Spanned<Stmt>) -> Option<TypeId> {
    match &stmt.node {
        Stmt::Expr(expr) => Some(infer_closure_return_type(ctx, expr)),
        Stmt::Match { arms, else_arm, .. } => {
            for item in arms {
                if let ast::MatchItem::Arm(arm) = item {
                    let ty = infer_closure_return_type(ctx, &arm.body);
                    if ty != UNIT_TYPE {
                        return Some(ty);
                    }
                }
            }
            if let Some(eb) = else_arm {
                // else_arm is a Block; synthesize an Expr::Block at its span and recurse
                let block_expr = Spanned::new(Expr::Block(eb.clone()), eb.span);
                let ty = infer_closure_return_type(ctx, &block_expr);
                if ty != UNIT_TYPE {
                    return Some(ty);
                }
            }
            Some(UNIT_TYPE)
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            let then_expr = Spanned::new(Expr::Block(then_body.clone()), then_body.span);
            let ty = infer_closure_return_type(ctx, &then_expr);
            if ty != UNIT_TYPE {
                return Some(ty);
            }
            for (_, elif_body) in elif_branches {
                let elif_expr = Spanned::new(Expr::Block(elif_body.clone()), elif_body.span);
                let ty = infer_closure_return_type(ctx, &elif_expr);
                if ty != UNIT_TYPE {
                    return Some(ty);
                }
            }
            if let Some(eb) = else_body {
                let else_expr = Spanned::new(Expr::Block(eb.clone()), eb.span);
                let ty = infer_closure_return_type(ctx, &else_expr);
                if ty != UNIT_TYPE {
                    return Some(ty);
                }
            }
            Some(UNIT_TYPE)
        }
        _ => None,
    }
}

/// Infer the return type of a closure body (simplified).
fn infer_closure_return_type(ctx: &mut LoweringContext, body: &Spanned<Expr>) -> TypeId {
    match &body.node {
        Expr::IntLiteral(_) => I64_TYPE,
        Expr::FloatLiteral(_) => F64_TYPE,
        Expr::BoolLiteral(_) => BOOL_TYPE,
        Expr::StringLiteral(_, _) => ctx.type_mapper.owned_string_type,
        // Struct construction: `(int n): Boxed(n * 100)`. Sibling of the
        // enum-variant arms in the `Call` case below — the parser gives struct
        // construction its own node, so it never reached them and fell through
        // to the I64 default. That default is not private to this function:
        // `__Closure_N__call` is registered with it, and every consumer that
        // reads the closure's signature — the Vector-HOF result type, the LIR
        // expander's element size — then agrees on the wrong type. `auto v =
        // nums.map((int n): Boxed(n))` read `.val` back as 0, while the same
        // map with a DECLARED destination was correct, because the declaration
        // supplied the type this channel could not.
        Expr::StructLiteral { name, generic_args, .. } => {
            let mangled = match generic_args {
                Some(gargs) if !gargs.is_empty() => {
                    let mut m = name.node.clone();
                    for g in gargs {
                        let tid = ctx.type_mapper.map_ast_type_mut(&g.node, &mut ctx.type_registry);
                        m.push_str("__");
                        m.push_str(&crate::ir::types::format_type_for_mangle(tid, &ctx.type_registry));
                    }
                    m
                }
                _ => name.node.clone(),
            };
            match ctx.type_mapper.lookup_named(&mangled) {
                Some(tid) => tid,
                // Not registered yet (the closure is the first mention of this
                // instance) — the I64 default is no worse than before, and the
                // typed destination still carries it.
                None => I64_TYPE,
            }
        }
        Expr::BinaryOp { op, left, right } => {
            use crate::parser::ast::BinaryOp;
            match op {
                BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt
                | BinaryOp::LtEq | BinaryOp::GtEq | BinaryOp::And | BinaryOp::Or => BOOL_TYPE,
                BinaryOp::Add => {
                    // For Add, the result type follows the operands —
                    // String + String → String, int + int → int. Recurse
                    // into operands to discover the type. Without this,
                    // string-concatenation closures (e.g.
                    // `((String name): "hi " + name)`) were typed I64
                    // and the IIFE call site ended up with an `i64` dst
                    // memcpy'd into a `GorgetString` slot — a real but
                    // backend-tolerated type mismatch that surfaced as
                    // an `AssignIntoOwnedSlot` Tier 2a violation. The
                    // resulting `_16: i64 = call __Closure_N__call(...);
                    // _11: GorgetString = copy _16` shape is a
                    // GIR-level type bug; recursing here straightens
                    // the typing and removes the violation.
                    let lty = infer_closure_return_type(ctx, left);
                    if lty != I64_TYPE { return lty; }
                    infer_closure_return_type(ctx, right)
                }
                _ => I64_TYPE, // Arithmetic → assume int
            }
        }
        Expr::Identifier(name) => {
            if let Some((_, type_id)) = ctx.lookup_local(name) {
                type_id
            } else {
                I64_TYPE
            }
        }
        Expr::Call { callee, args, .. } => {
            if let Expr::Identifier(name) = &callee.node {
                if let Some((_, ret_type)) = ctx.fn_sigs.get(name.as_str()) {
                    return *ret_type;
                }
                // READER 5 of the erased-Callable class (`t0770`): a closure
                // BODY that calls a `Callable[T]` parameter of the ENCLOSING
                // function (this inference runs in the enclosing function's
                // `func_state`, so the sidecar is in scope). The callee is
                // neither a top-level fn (`fn_sigs`) nor a closure struct —
                // its GIR local type is erased to unit — so the fall-through
                // at the end of this fn typed the whole closure `I64_TYPE`.
                // That value is published into `fn_sigs` for `__Closure_N__call`
                // and is what the combinator adapter's closure-struct arm reads,
                // so a wrong answer here re-opens `t0770` one level in.
                // Read the declared return through the same accessor every
                // other reader uses (devbook/24 rule 4).
                //
                // ⚠ MEASURED SCOPE, do not overclaim: this arm fires (verified
                // by instrumentation on `(String s): f(s)` shapes) but the
                // capture face it serves is still blocked one layer down — a
                // captured `Callable[T]` param becomes a `unit`-typed env field
                // (`void __v5 = *(void *)(__v4);` → `error: void value not
                // ignored as it ought to be`), which is `t0927`, a different
                // class (env-field typing + sidecar propagation into the lifted
                // closure). This read is correct and stays; it is not, on its
                // own, sufficient to compile that shape.
                if let Some((local_id, _)) = ctx.lookup_local(name) {
                    if let Some(ret_type) = ctx.callable_return_type(local_id) {
                        return ret_type;
                    }
                }
                // Enum variant constructors: Some(x), Ok(x), Error(x)
                if name == "Some" && args.len() == 1 {
                    let inner_type = infer_closure_return_type(ctx, &args[0].node.value);
                    let mangled = format!("Option__{}", crate::ir::types::format_type_for_mangle(inner_type, &ctx.type_registry));
                    if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        return tid;
                    }
                }
                if (name == "Ok" || name == "Error") && args.len() == 1 {
                    // Check expected_type from context
                    if let Some(et) = ctx.func_state.expected_type {
                        let is_result = ctx.type_registry.enum_category(et) == Some(EnumCategory::Result);
                        if is_result {
                            return et;
                        }
                    }
                }
                // Check enum_variants for user-defined enums
                if let Some((enum_name, _)) = ctx.enum_variants.get(name.as_str()) {
                    if let Some(type_id) = ctx.type_mapper.lookup_named(enum_name.as_str()) {
                        return type_id;
                    }
                }
            }
            // Qualified enum variant constructor: `EnumName.Variant(args)`.
            // Callee is `Expr::FieldAccess { object: Identifier(enum_name), field: variant }`.
            if let Expr::FieldAccess { object, .. } = &callee.node {
                if let Expr::Identifier(enum_name) = &object.node {
                    if let Some(type_id) = ctx.type_mapper.lookup_named(enum_name.as_str()) {
                        return type_id;
                    }
                }
            }
            if let Expr::Identifier(name) = &callee.node {
                // Known void builtins not in fn_sigs
                if matches!(name.as_str(),
                    "print" | "println" | "eprint" | "eprintln" | "assert"
                    | "panic" | "exit" | "sleep" | "sleep_ms"
                ) {
                    return UNIT_TYPE;
                }
            }
            // None() call
            if matches!(callee.node, Expr::NoneLiteral) {
                if let Some(et) = ctx.func_state.expected_type {
                    let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option);
                    if is_option {
                        return et;
                    }
                }
            }
            I64_TYPE
        }
        Expr::NoneLiteral => {
            // Bare `None` — check expected_type for Option context
            if let Some(et) = ctx.func_state.expected_type {
                let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option);
                if is_option {
                    return et;
                }
            }
            I64_TYPE
        }
        Expr::Block(block) => {
            // Walk block statements for explicit `return expr`
            if let Some(ret_type) = find_return_type_in_block(ctx, &block.stmts) {
                return ret_type;
            }
            // Last statement as implicit return (tail expression).
            // Snag #51 (2026-05-16): keep the recognised tail-value shapes
            // in lockstep with `lower_stmt_as_tail_value` in `exprs/mod.rs`
            // — `Stmt::Expr`, `Stmt::Match`, `Stmt::If` are all tail values
            // here. Pre-fix, only `Stmt::Expr` was recognised, so a
            // multi-stmt closure body ending in `match`/`if` registered as
            // UNIT_TYPE-returning. The caller's `auto x = mk()` slot
            // landed at unit size, and the int that the match arm produced
            // never made it across the call boundary (read back 0).
            if let Some(last) = block.stmts.last() {
                if let Some(ty) = infer_stmt_tail_type(ctx, last) {
                    return ty;
                }
            }
            UNIT_TYPE
        }
        // Tier 1c: previously fell through to I64_TYPE, silently
        // mis-typing closure-body method calls like `e.to_upper()`
        // (returns GorgetString, not int). The fallback was masked
        // pre-Tier-1c because Option/Result weren't Resource so
        // the type-rebuild logic in `map_err`'s
        // `try_lower_option_result_combinator` didn't fire for
        // same-type closures. Once Option/Result became Resource,
        // the cross-type adapter built a wrongly-sized result type
        // (`Result__T__int64_t` for a closure returning String),
        // causing a memcpy buffer overread.
        Expr::MethodCall { receiver, method, .. } => {
            // Qualified enum variant constructor `EnumName.Variant(args)` parses
            // as MethodCall with receiver = Identifier(enum_name). Detect this
            // before the value-method path: if the receiver name is a registered
            // type name, the call constructs that type. Closes the enum half of
            // Snag #51 — without this, a closure body returning
            // `Box.A("payload")` typed as I64 and the C codegen emitted
            // `__gg_Box = int64_t`.
            if let Expr::Identifier(name) = &receiver.node {
                if ctx.lookup_local(name).is_none() {
                    if let Some(type_id) = ctx.type_mapper.lookup_named(name.as_str()) {
                        return type_id;
                    }
                }
            }
            // Resolve receiver type. For `e.to_upper()` where `e` is
            // a closure param, the param type hint sets `e`'s type
            // before this inference runs.
            let recv_type = infer_closure_return_type(ctx, receiver);
            if recv_type == I64_TYPE {
                // No useful type info — fall back.
                return I64_TYPE;
            }
            if let Some(type_name) = ctx.type_name_for_id(recv_type) {
                let type_name = type_name.to_string();
                if let Some(ret) = ctx.resolve_builtin_method_return_type(&type_name, &method.node) {
                    return ret;
                }
            }
            I64_TYPE
        }
        _ => I64_TYPE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::lowering::empty_analysis_for_test;
    use crate::ir::lowering::types::TypeMapper;

    fn make_test_ctx() -> LoweringContext<'static> {
        let analysis = Box::leak(Box::new(empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);
        LoweringContext::new(analysis, mapper, reg)
    }

    #[test]
    fn collect_free_vars_simple() {
        let mut ctx = make_test_ctx();
        ctx.register_local("x", LocalId(1), I64_TYPE);
        ctx.register_local("y", LocalId(2), I64_TYPE);

        let param_names: FxHashSet<String> = ["a".to_string()].into_iter().collect();

        // x + a — x is free, a is a param
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::dummy(Expr::Identifier("x".to_string()))),
            op: crate::parser::ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(Expr::Identifier("a".to_string()))),
        };

        let free = collect_free_vars(&ctx, &expr, &param_names);
        assert_eq!(free.len(), 1);
        assert_eq!(free[0].0, "x");
    }

    #[test]
    fn collect_free_vars_no_captures() {
        let ctx = make_test_ctx();
        let param_names: FxHashSet<String> = ["x".to_string()].into_iter().collect();

        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::dummy(Expr::Identifier("x".to_string()))),
            op: crate::parser::ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(Expr::IntLiteral(1))),
        };

        let free = collect_free_vars(&ctx, &expr, &param_names);
        assert!(free.is_empty(), "No free variables expected");
    }

    #[test]
    fn closure_lowering_creates_struct() {
        let mut ctx = make_test_ctx();
        ctx.register_local("x", LocalId(1), I64_TYPE);

        let mut lowering = ClosureLowering::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let params = vec![Spanned::dummy(ClosureParam {
            type_: Some(Spanned::dummy(crate::parser::ast::Type::Primitive(
                crate::parser::ast::PrimitiveType::Int,
            ))),
            ownership: crate::parser::ast::Ownership::Borrow,
            name: Spanned::dummy("y".to_string()),
            destructure: None,
        })];

        let body = Spanned::dummy(Expr::BinaryOp {
            left: Box::new(Spanned::dummy(Expr::Identifier("x".to_string()))),
            op: crate::parser::ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(Expr::Identifier("y".to_string()))),
        });

        let _operand = lowering.lower_closure(
            &mut ctx, &mut builder,
            &params, &body, false,
            crate::span::Span::new(0, 0),
        );

        assert_eq!(lowering.lifted.len(), 1);
        assert_eq!(lowering.lifted[0].struct_type_name, "__Closure_0");
        assert_eq!(lowering.lifted[0].call_fn_name, "__Closure_0__call");
        assert_eq!(lowering.lifted[0].captures.len(), 1);
        assert_eq!(lowering.lifted[0].captures[0].name, "x");

        // Should have registered the struct type
        let type_def = ctx.type_registry.get_type_def("__Closure_0");
        assert!(type_def.is_some(), "Should have __Closure_0 TypeDef");
    }
}
