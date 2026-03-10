//! Post-resolution rewrite pass.
//!
//! After name resolution builds the resolution map and struct_fields,
//! this pass walks the AST mutably and converts `Expr::Call` nodes whose
//! callee resolves to `DefKind::Struct` into `Expr::StructLiteral` nodes.
//! This activates the dead `StructLiteral` match arms across the compiler
//! and removes the need for struct-as-Call workarounds in the borrow checker.

use crate::lexer::token::StringSegment;
use crate::parser::ast::*;
use crate::span::Spanned;

use super::resolve::ResolutionMap;
use super::scope::{DefKind, ScopeTable};

/// Rewrite struct constructor calls to `Expr::StructLiteral`.
///
/// For each `Expr::Call` whose callee is an `Expr::Identifier` that resolves
/// to `DefKind::Struct`, replace the node with `Expr::StructLiteral`.
/// Enum variant calls (`DefKind::Variant`) are NOT rewritten.
pub fn rewrite_struct_calls(module: &mut Module, resolution_map: &ResolutionMap, scopes: &ScopeTable) {
    for item in &mut module.items {
        rewrite_item(&mut item.node, resolution_map, scopes);
    }
}

fn rewrite_item(item: &mut Item, res: &ResolutionMap, scopes: &ScopeTable) {
    match item {
        Item::Function(f) => rewrite_function(f, res, scopes),
        Item::Equip(eq) => {
            for method in &mut eq.items {
                rewrite_function(&mut method.node, res, scopes);
            }
        }
        Item::ConstDecl(c) => rewrite_expr(&mut c.value, res, scopes),
        Item::StaticDecl(s) => rewrite_expr(&mut s.value, res, scopes),
        Item::Test(t) => {
            rewrite_block(&mut t.body, res, scopes);
        }
        Item::Bench(b) => {
            rewrite_block(&mut b.body, res, scopes);
        }
        Item::SuiteSetup(s) => rewrite_block(&mut s.body, res, scopes),
        Item::SuiteTeardown(s) => rewrite_block(&mut s.body, res, scopes),
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                if let TraitItem::Method(f) = &mut trait_item.node {
                    rewrite_function(f, res, scopes);
                }
            }
        }
        Item::Struct(_) | Item::Enum(_) | Item::Import(_)
        | Item::TypeAlias(_) | Item::Newtype(_) | Item::ExternBlock(_)
        | Item::Directive(_) | Item::MetaConst(_) | Item::MetaType(_)
        | Item::MetaTypeFunc(_) | Item::MetaAssert(_) | Item::MetaIf(_) | Item::MetaLog(_) => {}
        Item::Module { items, .. } => {
            for si in items {
                rewrite_item(&mut si.node, res, scopes);
            }
        }
    }
}

fn rewrite_function(f: &mut FunctionDef, res: &ResolutionMap, scopes: &ScopeTable) {
    // Rewrite default parameter expressions
    for param in &mut f.params {
        if let Some(default) = &mut param.node.default {
            rewrite_expr(default, res, scopes);
        }
    }
    match &mut f.body {
        FunctionBody::Block(block) => rewrite_block(block, res, scopes),
        FunctionBody::Expression(expr) => rewrite_expr(expr, res, scopes),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn rewrite_block(block: &mut Block, res: &ResolutionMap, scopes: &ScopeTable) {
    for stmt in &mut block.stmts {
        rewrite_stmt(&mut stmt.node, res, scopes);
    }
}

fn rewrite_stmt(stmt: &mut Stmt, res: &ResolutionMap, scopes: &ScopeTable) {
    match stmt {
        Stmt::VarDecl { value, .. } => rewrite_expr(value, res, scopes),
        Stmt::Expr(expr) => {
            rewrite_expr(expr, res, scopes);
            // Rewrite field_set(obj, "field", value) → obj.field = value
            if let Expr::Call { ref callee, ref args, .. } = expr.node {
                if let Expr::Identifier(ref cname) = callee.node {
                    if cname == "field_set" && args.len() == 3 {
                        if let Expr::StringLiteral(ref s) = args[1].node.value.node {
                            if !s.has_interpolation() {
                                let field_name: String = s.segments.iter()
                                    .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                                    .collect();
                                if !field_name.is_empty() {
                                    let obj_expr = args[0].node.value.clone();
                                    let val_expr = args[2].node.value.clone();
                                    let field_span = args[1].node.value.span;
                                    let target = Spanned::new(Expr::FieldAccess {
                                        object: Box::new(obj_expr),
                                        field: Spanned::new(field_name, field_span),
                                    }, expr.span);
                                    *stmt = Stmt::Assign {
                                        target,
                                        value: val_expr,
                                    };
                                }
                            }
                        }
                    }
                }
            }
        }
        Stmt::Assign { target, value } => {
            rewrite_expr(target, res, scopes);
            rewrite_expr(value, res, scopes);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target, res, scopes);
            rewrite_expr(value, res, scopes);
        }
        Stmt::Return(Some(expr)) => rewrite_expr(expr, res, scopes),
        Stmt::Throw(expr) => rewrite_expr(expr, res, scopes),
        Stmt::Break(Some(expr)) => rewrite_expr(expr, res, scopes),
        Stmt::Return(None) | Stmt::Break(None) | Stmt::Continue | Stmt::Pass => {}
        Stmt::For { iterable, body, else_body, .. } => {
            rewrite_expr(iterable, res, scopes);
            rewrite_block(body, res, scopes);
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes); }
        }
        Stmt::While { condition, body, else_body } => {
            rewrite_expr(condition, res, scopes);
            rewrite_block(body, res, scopes);
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes); }
        }
        Stmt::Loop { body } => rewrite_block(body, res, scopes),
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            rewrite_expr(condition, res, scopes);
            rewrite_block(then_body, res, scopes);
            for (cond, body) in elif_branches {
                rewrite_expr(cond, res, scopes);
                rewrite_block(body, res, scopes);
            }
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes); }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            rewrite_expr(scrutinee, res, scopes);
            for arm in arms.iter_mut().filter_map(|i| i.arm_mut()) {
                if let Some(guard) = &mut arm.guard { rewrite_expr(guard, res, scopes); }
                rewrite_expr(&mut arm.body, res, scopes);
            }
            if let Some(ea) = else_arm { rewrite_block(ea, res, scopes); }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                match &mut arm.op {
                    SelectOp::Recv { channel, .. } => rewrite_expr(channel, res, scopes),
                    SelectOp::Send { channel, value } => {
                        rewrite_expr(channel, res, scopes);
                        rewrite_expr(value, res, scopes);
                    }
                }
                rewrite_block(&mut arm.body, res, scopes);
            }
            if let Some(ea) = else_arm { rewrite_block(ea, res, scopes); }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                rewrite_expr(&mut binding.expr, res, scopes);
            }
            rewrite_block(body, res, scopes);
        }
        Stmt::Unsafe { body } => rewrite_block(body, res, scopes),
        Stmt::NamedScope { body, .. } => rewrite_block(body, res, scopes),
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            rewrite_expr(condition, res, scopes);
            if let Some(msg) = message { rewrite_expr(msg, res, scopes); }
        }
        Stmt::Snapshot { value, .. } => {
            rewrite_expr(value, res, scopes);
        }
        Stmt::Item(item) => rewrite_item(item, res, scopes),
        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            // Conditions are meta expressions — skip rewriting them; rewrite the bodies.
            rewrite_block(then_body, res, scopes);
            for (_, body) in elif_branches {
                rewrite_block(body, res, scopes);
            }
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes); }
        }
        Stmt::MetaFor { body, .. } => {
            // Range is a meta expression — skip; rewrite the body.
            rewrite_block(body, res, scopes);
        }
        Stmt::MetaMatch { arms, else_arm, .. } => {
            // Scrutinee and case exprs are meta expressions — skip; rewrite bodies only.
            for (_, body) in arms {
                rewrite_block(body, res, scopes);
            }
            if let Some(eb) = else_arm { rewrite_block(eb, res, scopes); }
        }
        Stmt::MetaWhile { body, .. } => {
            // Condition is a meta expression — skip; rewrite the body.
            rewrite_block(body, res, scopes);
        }

        Stmt::MetaConst { .. } => {
            // Entirely a meta expression — evaluated at monomorphization time; skip.
        }

        Stmt::MetaLog { .. } => {
            // Compile-time diagnostic — removed before GIR lowering; skip.
        }
        Stmt::OnError { body } => {
            rewrite_block(body, res, scopes);
        }
    }
}

fn rewrite_expr(expr: &mut Spanned<Expr>, res: &ResolutionMap, scopes: &ScopeTable) {
    // First, recurse into sub-expressions
    match &mut expr.node {
        Expr::UnaryOp { operand, .. } => rewrite_expr(operand, res, scopes),
        Expr::BinaryOp { left, right, .. } => {
            rewrite_expr(left, res, scopes);
            rewrite_expr(right, res, scopes);
        }
        Expr::Call { callee, args, .. } => {
            rewrite_expr(callee, res, scopes);
            for arg in args {
                rewrite_expr(&mut arg.node.value, res, scopes);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            rewrite_expr(receiver, res, scopes);
            for arg in args {
                rewrite_expr(&mut arg.node.value, res, scopes);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => {
            rewrite_expr(object, res, scopes);
        }
        Expr::Index { object, index } => {
            rewrite_expr(object, res, scopes);
            rewrite_expr(index, res, scopes);
        }
        Expr::DefaultOp { lhs, rhs } => {
            rewrite_expr(lhs, res, scopes);
            rewrite_expr(rhs, res, scopes);
        }
        Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner } | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner } | Expr::Spawn { expr: inner }
        | Expr::SpawnBlocking { expr: inner } | Expr::RawCapture { expr: inner } => {
            rewrite_expr(inner, res, scopes);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            rewrite_expr(condition, res, scopes);
            rewrite_expr(then_branch, res, scopes);
            for (cond, body) in elif_branches {
                rewrite_expr(cond, res, scopes);
                rewrite_expr(body, res, scopes);
            }
            if let Some(eb) = else_branch { rewrite_expr(eb, res, scopes); }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            rewrite_expr(scrutinee, res, scopes);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { rewrite_expr(guard, res, scopes); }
                rewrite_expr(&mut arm.body, res, scopes);
            }
            if let Some(ea) = else_arm { rewrite_expr(ea, res, scopes); }
        }
        Expr::Block(block) | Expr::Do { body: block } => {
            rewrite_block(block, res, scopes);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            rewrite_expr(body, res, scopes);
        }
        Expr::ListComprehension { expr: comp_expr, iterable, condition, .. } => {
            rewrite_expr(comp_expr, res, scopes);
            rewrite_expr(iterable, res, scopes);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            rewrite_expr(key, res, scopes);
            rewrite_expr(value, res, scopes);
            rewrite_expr(iterable, res, scopes);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes); }
        }
        Expr::SetComprehension { expr: comp_expr, iterable, condition, .. } => {
            rewrite_expr(comp_expr, res, scopes);
            rewrite_expr(iterable, res, scopes);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes); }
        }
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            for elem in elems { rewrite_expr(elem, res, scopes); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                rewrite_expr(k, res, scopes);
                rewrite_expr(v, res, scopes);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for arg in args { rewrite_expr(arg, res, scopes); }
        }
        Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
            rewrite_expr(inner, res, scopes);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { rewrite_expr(s, res, scopes); }
            if let Some(e) = end { rewrite_expr(e, res, scopes); }
        }
        // Dot-shorthand: recurse into args
        Expr::DotShorthand { args, .. } => {
            for arg in args.iter_mut() {
                rewrite_expr(&mut arg.node.value, res, scopes);
            }
        }
        Expr::MetaOpInfix { left, right, .. } => {
            rewrite_expr(left, res, scopes);
            rewrite_expr(right, res, scopes);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, transform, .. } => {
            rewrite_expr(expr, res, scopes);
            rewrite_expr(transform, res, scopes);
        }
        Expr::Catch { expr, recovery, .. } => {
            rewrite_expr(expr, res, scopes);
            rewrite_expr(recovery, res, scopes);
        }
        // Leaf nodes
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::StringLiteral(_) | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } | Expr::It => {}
    }

    // Rewrite field_get/field_value(val, "field") → val.field (for literal usage outside meta for)
    if let Expr::Call { ref callee, ref args, .. } = expr.node {
        if let Expr::Identifier(ref cname) = callee.node {
            if (cname == "field_get" || cname == "field_value") && args.len() == 2 {
                if let Expr::StringLiteral(ref s) = args[1].node.value.node {
                    if !s.has_interpolation() {
                        let field_name: String = s.segments.iter()
                            .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                            .collect();
                        if !field_name.is_empty() {
                            let val_expr = args[0].node.value.clone();
                            let field_span = args[1].node.value.span;
                            expr.node = Expr::FieldAccess {
                                object: Box::new(val_expr),
                                field: Spanned::new(field_name, field_span),
                            };
                            return;
                        }
                    }
                }
            }
        }
    }

    // Rewrite make_variant(T, "Variant") → Expr::Path ["T", "Variant"]
    if let Expr::Call { ref callee, ref args, .. } = expr.node {
        if let Expr::Identifier(ref cname) = callee.node {
            if cname == "make_variant" && args.len() == 2 {
                if let Expr::StringLiteral(ref s) = args[1].node.value.node {
                    if !s.has_interpolation() {
                        let variant_name: String = s.segments.iter()
                            .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                            .collect();
                        if !variant_name.is_empty() {
                            if let Expr::Identifier(type_name) = args[0].node.value.node.clone() {
                                let type_span = args[0].node.value.span;
                                let var_span  = args[1].node.value.span;
                                expr.node = Expr::Path {
                                    segments: vec![
                                        Spanned::new(type_name, type_span),
                                        Spanned::new(variant_name, var_span),
                                    ],
                                };
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    // Now check if this expression is a Call that should become a StructLiteral.
    // Skip collection types — they have special C constructors (gorget_array_new,
    // GorgetDict__new, etc.) that don't use compound literal syntax.
    const COLLECTION_TYPES: &[&str] = &[
        "Vector", "Dict", "HashMap", "Set", "HashSet", "Box", "Channel", "Arena", "TrackingAllocator", "PoolAllocator", "TlsfAllocator",
        "FixedBufferAllocator", "FallbackAllocator",
    ];
    if let Expr::Call { callee, .. } = &expr.node {
        if let Expr::Identifier(cname) = &callee.node {
            if COLLECTION_TYPES.contains(&cname.as_str()) {
                return;
            }
            let callee_span_start = callee.span.start;
            if let Some(&def_id) = res.get(&callee_span_start) {
                let def = scopes.get_def(def_id);
                // Verify the definition name matches the callee name.
                // This prevents span collisions from derive-generated code
                // (which has overlapping spans) from causing false rewrites.
                if def.kind == DefKind::Struct && def.name == *cname {
                    // Extract fields from the Call and build StructLiteral
                    let call = std::mem::replace(&mut expr.node, Expr::NoneLiteral);
                    if let Expr::Call { callee, generic_args, args } = call {
                        let name_str = if let Expr::Identifier(n) = callee.node {
                            n
                        } else {
                            unreachable!()
                        };
                        let bare_args: Vec<Spanned<Expr>> = args.into_iter()
                            .map(|a| a.node.value)
                            .collect();
                        expr.node = Expr::StructLiteral {
                            name: Spanned::new(name_str, callee.span),
                            generic_args,
                            args: bare_args,
                        };
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;
    use rustc_hash::FxHashMap;

    fn dummy_span() -> Span {
        Span { start: 0, end: 1 }
    }

    #[test]
    fn rewrite_converts_struct_call_to_struct_literal() {
        // Build a minimal module with a single expression statement: MyStruct(42)
        let callee_span = Span { start: 100, end: 108 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("MyStruct".to_string()), callee_span)),
                generic_args: None,
                args: vec![Spanned::new(
                    CallArg {
                        name: None,
                        ownership: Ownership::Borrow,
                        value: Spanned::new(Expr::IntLiteral(42), dummy_span()),
                    },
                    dummy_span(),
                )],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: None,
                    where_clause: None,
                    body: FunctionBody::Block(Block {
                        stmts: vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        span: dummy_span(),
                    }),
                    doc_comment: None,
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        // Set up scope table with MyStruct as a Struct def
        let mut scopes = ScopeTable::new();
        let struct_def_id = scopes.define("MyStruct".to_string(), DefKind::Struct, callee_span).unwrap();
        let _ = struct_def_id;

        // Set up resolution map: callee span start → struct def id
        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, struct_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify the expression was rewritten
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    match &expr.node {
                        Expr::StructLiteral { name, generic_args, args } => {
                            assert_eq!(name.node, "MyStruct");
                            assert!(generic_args.is_none());
                            assert_eq!(args.len(), 1);
                            assert!(matches!(args[0].node, Expr::IntLiteral(42)));
                        }
                        other => panic!("Expected StructLiteral, got: {:?}", other),
                    }
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }

    #[test]
    fn rewrite_does_not_convert_variant_call() {
        let callee_span = Span { start: 200, end: 204 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("Some".to_string()), callee_span)),
                generic_args: None,
                args: vec![Spanned::new(
                    CallArg {
                        name: None,
                        ownership: Ownership::Borrow,
                        value: Spanned::new(Expr::IntLiteral(1), dummy_span()),
                    },
                    dummy_span(),
                )],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: None,
                    where_clause: None,
                    body: FunctionBody::Block(Block {
                        stmts: vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        span: dummy_span(),
                    }),
                    doc_comment: None,
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        let mut scopes = ScopeTable::new();
        let variant_def_id = scopes.define("Some".to_string(), DefKind::Variant, callee_span).unwrap();

        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, variant_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify it was NOT rewritten (should still be Call)
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    assert!(matches!(expr.node, Expr::Call { .. }), "Expected Call (not rewritten), got: {:?}", expr.node);
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }

    #[test]
    fn rewrite_preserves_generic_args() {
        let callee_span = Span { start: 300, end: 304 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("Pair".to_string()), callee_span)),
                generic_args: Some(vec![
                    Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                    Spanned::new(Type::Primitive(PrimitiveType::Str), dummy_span()),
                ]),
                args: vec![
                    Spanned::new(
                        CallArg {
                            name: None,
                            ownership: Ownership::Borrow,
                            value: Spanned::new(Expr::IntLiteral(10), dummy_span()),
                        },
                        dummy_span(),
                    ),
                    Spanned::new(
                        CallArg {
                            name: None,
                            ownership: Ownership::Borrow,
                            value: Spanned::new(Expr::StringLiteral(
                                crate::lexer::token::StringLiteral {
                                kind: crate::lexer::token::StringKind::Normal,
                                segments: vec![crate::lexer::token::StringSegment::Literal("hi".to_string())],
                            }
                            ), dummy_span()),
                        },
                        dummy_span(),
                    ),
                ],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: None,
                    where_clause: None,
                    body: FunctionBody::Block(Block {
                        stmts: vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        span: dummy_span(),
                    }),
                    doc_comment: None,
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        let mut scopes = ScopeTable::new();
        let struct_def_id = scopes.define("Pair".to_string(), DefKind::Struct, callee_span).unwrap();

        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, struct_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify rewritten with generic_args preserved
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    match &expr.node {
                        Expr::StructLiteral { name, generic_args, args } => {
                            assert_eq!(name.node, "Pair");
                            assert!(generic_args.is_some());
                            assert_eq!(generic_args.as_ref().unwrap().len(), 2);
                            assert_eq!(args.len(), 2);
                        }
                        other => panic!("Expected StructLiteral, got: {:?}", other),
                    }
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }
}
