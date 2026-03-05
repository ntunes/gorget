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
use crate::parser::ast::{self, ClosureParam, Expr, Pattern, Stmt};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::exprs::lower_expr;
use super::stmts::lower_block;

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
    pub return_type: TypeId,
    /// The closure body AST (cloned for deferred lowering).
    pub body: Spanned<Expr>,
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

        // Create the closure struct TypeDef
        let fields: Vec<StructField> = captures.iter()
            .map(|cap| {
                let field_type = match cap.mode {
                    CaptureMode::ByValue => cap.type_id,
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
            metadata: TypeMetadata::default(),
        };
        ctx.type_registry.add_type_def(type_def);
        let struct_type_id = ctx.type_registry.insert(GirType::Named(struct_name.clone()));
        ctx.type_mapper.named_types.insert(struct_name.clone(), struct_type_id);

        // Map closure params to GIR types
        let closure_param_names: Vec<String> = params.iter()
            .map(|p| p.node.name.node.clone())
            .collect();
        let closure_param_types: Vec<TypeId> = params.iter()
            .map(|p| {
                if let Some(ref ty) = p.node.type_ {
                    ctx.type_mapper.map_ast_type(&ty.node)
                } else {
                    I64_TYPE // fallback for untyped params
                }
            })
            .collect();

        // Infer return type from body (simplified — use I64 fallback)
        let return_type = infer_closure_return_type(ctx, body);

        // Register the call function signature
        let env_ptr_type = ctx.type_registry.insert(GirType::Ptr(struct_type_id));
        let mut sig_params = vec![env_ptr_type];
        sig_params.extend_from_slice(&closure_param_types);
        ctx.fn_sigs.insert(call_fn_name.clone(), (sig_params, return_type));

        // Register this closure's info for call dispatch.
        // Only store ByValue captures — ByMutRef captures cannot be copied across
        // thread boundaries, so they are excluded from the spawn wrapper signature.
        let spawn_captures: Vec<(String, TypeId, u32)> = captures.iter()
            .enumerate()
            .filter(|(_, c)| c.mode == CaptureMode::ByValue)
            .map(|(i, c)| (c.name.clone(), c.type_id, i as u32))
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
            return_type,
            body: body.clone(),
        });

        // Emit the creation-site StructInit
        let field_operands: Vec<Operand> = captures.iter()
            .map(|cap| {
                match cap.mode {
                    CaptureMode::ByValue => FunctionBuilder::copy(cap.local_id),
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

    // Build params: env pointer + closure params
    let mut params: Vec<(TypeId, Option<&str>)> = vec![(env_ptr_type, Some("__env"))];
    for (name, type_id) in closure.param_names.iter().zip(closure.param_types.iter()) {
        params.push((*type_id, Some(name.as_str())));
    }

    let mut builder = FunctionBuilder::new(
        &closure.call_fn_name,
        closure.return_type,
        &params,
    );

    ctx.clear_locals();

    // _1 = __env (pointer to closure struct)
    let env_local = LocalId(1);
    ctx.register_local("__env", env_local, env_ptr_type);

    // Load captures from the env struct
    for (i, cap) in closure.captures.iter().enumerate() {
        match cap.mode {
            CaptureMode::ByValue => {
                // Load field from env struct → local
                let dst = builder.field_load(
                    Place::local(env_local),
                    i as u32,
                    cap.type_id,
                );
                ctx.register_local(&cap.name, dst, cap.type_id);
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
                // go through Deref projections (checked via mut_capture_locals).
                ctx.register_local(&cap.name, ptr_local, ptr_type);
                ctx.mut_capture_locals.insert(ptr_local, cap.type_id);
            }
        }
    }

    // Register closure params as locals
    let param_start = 2u32; // _0=return, _1=env, _2...=params
    for (i, (name, type_id)) in closure.param_names.iter()
        .zip(closure.param_types.iter())
        .enumerate()
    {
        let local_id = LocalId(param_start + i as u32);
        ctx.register_local(name, local_id, *type_id);
    }

    // Lower the closure body
    match &closure.body.node {
        Expr::Block(block) => {
            lower_block(ctx, &mut builder, block);
            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                if closure.return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            }
        }
        _ => {
            // Expression body
            let result = lower_expr(ctx, &mut builder, &closure.body);
            // Re-infer return type from the actual body result (the pre-inference
            // may have returned I64_TYPE for variant constructors like Some(x+1))
            let actual_type = super::exprs::infer_operand_type_full(ctx, &result, &builder);
            if actual_type != closure.return_type && actual_type != UNIT_TYPE {
                builder.locals[0].type_id = actual_type;
            }
            builder.assign(Place::local(LocalId(0)), result);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }
    }

    let mut func = builder.build();
    // Update the function's return_type to match the actual local[0] type
    let actual_ret = func.locals[0].type_id;
    if actual_ret != func.return_type {
        func.return_type = actual_ret;
    }
    func
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
            Expr::If { condition, then_branch, else_branch, .. } => {
                self.visit_expr(&condition.node);
                self.visit_expr(&then_branch.node);
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
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    self.visit_expr(&elem.node);
                }
            }
            Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner } => {
                self.visit_expr(&inner.node);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { self.visit_expr(&s.node); }
                if let Some(e) = end { self.visit_expr(&e.node); }
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

/// Infer the return type of a closure body (simplified).
fn infer_closure_return_type(ctx: &LoweringContext, body: &Spanned<Expr>) -> TypeId {
    match &body.node {
        Expr::IntLiteral(_) => I64_TYPE,
        Expr::FloatLiteral(_) => F64_TYPE,
        Expr::BoolLiteral(_) => BOOL_TYPE,
        Expr::StringLiteral(_) => ctx.type_mapper.str_type,
        Expr::BinaryOp { op, .. } => {
            use crate::parser::ast::BinaryOp;
            match op {
                BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt
                | BinaryOp::LtEq | BinaryOp::GtEq | BinaryOp::And | BinaryOp::Or => BOOL_TYPE,
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
                // Enum variant constructors: Some(x), Ok(x), Error(x)
                if name == "Some" && args.len() == 1 {
                    let inner_type = infer_closure_return_type(ctx, &args[0].node.value);
                    let mangled = format!("Option__{}", crate::ir::types::format_type_for_mangle(inner_type, &ctx.type_registry));
                    if let Some(&tid) = ctx.type_mapper.named_types.get(&mangled) {
                        return tid;
                    }
                }
                if (name == "Ok" || name == "Error") && args.len() == 1 {
                    // Check expected_type from context
                    if let Some(et) = ctx.expected_type {
                        let tn = ctx.type_registry.type_name(et).unwrap_or_default();
                        if tn.starts_with("Result__") {
                            return et;
                        }
                    }
                }
                // Check enum_variants for user-defined enums
                if let Some((enum_name, _)) = ctx.enum_variants.get(name.as_str()) {
                    if let Some(&type_id) = ctx.type_mapper.named_types.get(enum_name.as_str()) {
                        return type_id;
                    }
                }
            }
            // None() call
            if matches!(callee.node, Expr::NoneLiteral) {
                if let Some(et) = ctx.expected_type {
                    let tn = ctx.type_registry.type_name(et).unwrap_or_default();
                    if tn.starts_with("Option__") {
                        return et;
                    }
                }
            }
            I64_TYPE
        }
        Expr::Block(_) => UNIT_TYPE, // Block bodies are void by default
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
        })];

        let body = Spanned::dummy(Expr::BinaryOp {
            left: Box::new(Spanned::dummy(Expr::Identifier("x".to_string()))),
            op: crate::parser::ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(Expr::Identifier("y".to_string()))),
        });

        let _operand = lowering.lower_closure(
            &mut ctx, &mut builder,
            &params, &body, false,
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
