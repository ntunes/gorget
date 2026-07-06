//! Elaboration: production surface AST → GGC (RFC §2.4), for the Increment-A
//! subset only.
//!
//! This is the spec-owned front half. It shares the production **lexer +
//! parser + AST** (that is the only dependency; the import ratchet forbids
//! `ir`/`semantic`/`lir`/`bir`/`backend`). It performs the desugarings and
//! mode-tag resolution the A fixtures need — and NOTHING it cannot do
//! faithfully: any surface construct outside the A subset is a hard
//! `ElabError`, never a silent approximation (per the brief's stop-and-report
//! rule and "Don't redesign around compiler gaps").
//!
//! A-subset desugarings implemented here:
//!   * collection literals `[a, b, c]` → `Vector` construct with owning elems;
//!   * f-strings → `FString` parts (literal / interpolation);
//!   * `for pat in coll:` → an index `while` loop with a `Borrow`-view var;
//!   * `elif` chains → nested `if`;
//!   * `x += e` → `x = x + e`;
//!   * `print(e)` → the `Print` output effect;
//!   * method calls (`push`/`set`/`len`/`clone`) → GGC `Method`/`Clone`;
//!   * mode tags from syntax (`bare`/`&`/`!`) at every binding/arg position;
//!   * `from std.collections import ...` → a parse-and-DISCARD no-op (the
//!     imported types are prelude-available in A; the full shim list is B).

use std::collections::{HashMap, HashSet};

use gorget::lexer::token::{StringKind, StringSegment};
use gorget::parser::ast;
use gorget::span::{Span, Spanned};

use crate::ggc::{
    BinOp, BuiltinMethod, CastTarget, ClosureDef, ConstructKind, EnumDef, Expr, ExprArm, FPart,
    Function, Mode, Param, Pattern, Program, Source, Stmt, StmtArm, StructDef, UnOp,
};

/// A faithful-elaboration failure: a surface construct outside the A subset,
/// or a shape the elaborator cannot lower without inventing semantics.
#[derive(Debug, Clone)]
pub struct ElabError {
    pub message: String,
    pub span: Span,
}

impl ElabError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        ElabError { message: message.into(), span }
    }
}

type ElabResult<T> = Result<T, ElabError>;

/// Elaborate a parsed surface module into a GGC program.
pub fn elaborate(module: &ast::Module) -> ElabResult<Program> {
    let mut el = Elaborator::default();
    let items = module.all_items();

    // Pass 1: collect struct/enum layouts (+ field/payload TYPES for inference
    // and taint), function + method signatures, `equip` methods, and the
    // `equip T with Drop` registry (the tiny resolver).
    for item in &items {
        match item {
            ast::Item::Struct(sd) => {
                let name = sd.name.node.clone();
                let fields: Vec<String> = sd.fields.iter().map(|f| f.node.name.node.clone()).collect();
                let field_types: Vec<(String, Ty)> = sd
                    .fields
                    .iter()
                    .map(|f| (f.node.name.node.clone(), ty_of_type(&f.node.type_.node)))
                    .collect();
                el.struct_field_types.insert(name.clone(), field_types);
                el.structs.push(StructDef { name: name.clone(), fields });
                el.struct_names.insert(name);
            }
            ast::Item::Enum(ed) => {
                let name = ed.name.node.clone();
                let mut payloads: Vec<Ty> = Vec::new();
                let variants = ed
                    .variants
                    .iter()
                    .map(|v| {
                        let arity = match &v.node.fields {
                            ast::VariantFields::Unit => 0,
                            ast::VariantFields::Tuple(ts) => {
                                for t in ts {
                                    payloads.push(ty_of_type(&t.node));
                                }
                                ts.len()
                            }
                        };
                        (v.node.name.node.clone(), arity)
                    })
                    .collect();
                el.enum_payload_types.insert(name.clone(), payloads);
                el.enums.push(EnumDef { name, variants });
            }
            ast::Item::Function(fd) => {
                let name = fd.name.node.clone();
                el.func_names.insert(name.clone());
                el.fn_param_names.insert(
                    name.clone(),
                    fd.params.iter().map(|p| p.node.name.node.clone()).collect(),
                );
                el.fn_ret.insert(name.clone(), ty_of_type(&fd.return_type.node));
                if fd.throws.is_some() {
                    el.fn_throws.insert(name);
                }
            }
            ast::Item::Equip(eq) => el.register_equip(eq)?,
            // Imports are a discard no-op (types are prelude-available; the
            // `std.conv.int_to_str` shim is handled at the call site).
            ast::Item::Import(_) => {}
            // Nested modules are already flattened by `all_items`.
            ast::Item::Module { .. } => {}
            other => {
                return Err(ElabError::new(
                    format!("item kind {} is outside the phase-0 subset", item_kind(other)),
                    item_span(other),
                ));
            }
        }
    }

    // Pass 1b: the D4 transitive drop-taint fixpoint (seed = types with a
    // custom `Drop`; propagate through struct fields + enum payloads).
    el.compute_taint();

    // Pass 2: elaborate free-function bodies AND `equip` method bodies (the
    // latter as GGC functions named `Type__method`, with `self` in scope).
    let mut functions = Vec::new();
    for item in &items {
        match item {
            ast::Item::Function(fd) => functions.push(el.elaborate_function(fd, None)?),
            ast::Item::Equip(eq) => {
                let type_name = equip_type_name(eq)?;
                for m in &eq.items {
                    functions.push(el.elaborate_equip_method(&type_name, &m.node)?);
                }
            }
            _ => {}
        }
    }

    Ok(Program {
        functions,
        structs: el.structs,
        enums: el.enums,
        closures: el.closures,
        drop_fns: el.drop_fns,
    })
}

/// An elaboration-inferred type, carried in the per-function env so a method
/// call can dispatch a USER `equip` method vs a builtin (the corpus has
/// user `get`/`set_name` colliding with the builtins) and so the D4 taint
/// check can classify a source place. Gorget is type-first, so this is
/// READ-THE-ANNOTATION for decls/params plus small projection inference.
#[derive(Clone, Debug, PartialEq)]
enum Ty {
    /// Any non-collection scalar/primitive (int/bool/float/…).
    Prim,
    Str,
    Vector(Box<Ty>),
    Dict(Box<Ty>, Box<Ty>),
    Set(Box<Ty>),
    Tuple(Vec<Ty>),
    /// A user struct/enum (or `Option`/`Result` — those carry no user methods).
    Named(String),
    /// Not inferable at this position — dispatch falls through to the builtins.
    Unknown,
}

/// A binding's mode in the env — the D4 materialize-on-write check needs to
/// know a write's root local is a BORROW binding (bare param / for-var /
/// match-binding / plain `self`), the only bindings that materialize.
#[derive(Clone, Copy, PartialEq, Debug)]
enum BindMode {
    /// An owned `let` binding — writes land in place, never materialize.
    Owned,
    /// A bare param / for-var / match-binding / plain `self` — a view that
    /// MATERIALIZES a private copy on first write (RFC §2.2).
    Borrow,
    /// A `&` alias — writes reach the owner, no materialize.
    WriteThrough,
    /// A `!` move binding.
    Move,
}

/// A resolved `equip` method: its GGC function name (`Type__method`) and the
/// mode its `self` param binds under (D2: plain `self` = Borrow).
#[derive(Clone, Debug)]
struct MethodInfo {
    mangled: String,
    self_mode: Mode,
}

#[derive(Default)]
struct Elaborator {
    structs: Vec<StructDef>,
    struct_names: HashSet<String>,
    /// `struct-name → [(field-name, field-type)]` — for projection type
    /// inference (`v[0].inner` → the element/field type) and D4 taint.
    struct_field_types: HashMap<String, Vec<(String, Ty)>>,
    enums: Vec<EnumDef>,
    /// `enum-name → [payload-type…]` flattened across variants — D4 taint only.
    enum_payload_types: HashMap<String, Vec<Ty>>,
    func_names: HashSet<String>,
    /// Signature registry (pass 1): free-fn / method name → explicit param
    /// names in decl order — serves the call-side named-arg REORDER.
    fn_param_names: HashMap<String, Vec<String>>,
    /// Signature registry: fn / method name → return type — serves receiver
    /// type inference for method-call-result receivers.
    fn_ret: HashMap<String, Ty>,
    /// Functions / methods (by GGC name) declared `throws E`: their observable
    /// return type is widened to `Result[T, E]` and a call to one auto-
    /// propagates (RFC §2.6 row 1). One source of truth for the throws→Result
    /// desugar; read at call sites to decide propagate-vs-consume.
    fn_throws: HashSet<String>,
    /// `(type-name, method-name) → MethodInfo` for user `equip` methods.
    equip_methods: HashMap<(String, String), MethodInfo>,
    /// `equip T with Drop` registry: `(type-name, drop-fn-name)`.
    drop_fns: Vec<(String, String)>,
    /// Transitively drop-tainted NAMED types (D4): a type with a custom `Drop`
    /// anywhere in its field/payload graph.
    tainted: HashSet<String>,
    closures: Vec<ClosureDef>,
    /// Local names bound anywhere in the CURRENT function (params + var decls +
    /// for-vars). Used to distinguish a closure-value call (`f()`) from an
    /// unknown-function error, and to compute closure capture sets.
    local_names: HashSet<String>,
    /// Per-function type env (built incrementally as bindings are elaborated):
    /// local → its inferred type. Read for method dispatch + D4 taint.
    local_ty: HashMap<String, Ty>,
    /// Per-function mode env: local → its binding mode (D4 materialize check).
    local_mode: HashMap<String, BindMode>,
    /// The type name of `self` while elaborating an `equip` method body.
    current_self_type: Option<String>,
    /// Whether the function CURRENTLY being elaborated is `throws E`: its
    /// `return`/`throw`/fall-off are wrapped `Ok`/`Error`, and a nested throws-
    /// call in a non-Result-consuming position auto-propagates.
    current_fn_throws: bool,
    /// Set for the direct scrutinee of a `match` whose arms consume the
    /// `Result` itself (`case Ok(..)` / `case Error(..)`): a throws-call there
    /// yields the `Result` value rather than auto-propagating. Consumed (reset)
    /// by the outermost call so nested sub-expression calls still propagate.
    autoprop_suppressed: bool,
    gensym: usize,
}

impl Elaborator {
    fn fresh(&mut self, hint: &str) -> String {
        let n = self.gensym;
        self.gensym += 1;
        format!("__{hint}_{n}")
    }

    /// A free function: elaborate its body, keeping its own name.
    fn elaborate_function(&mut self, fd: &ast::FunctionDef, self_type: Option<&str>) -> ElabResult<Function> {
        let (params, body) = self.elaborate_fn_body(fd, self_type)?;
        Ok(Function { name: fd.name.node.clone(), params, body, span: fd.span })
    }

    /// An `equip` method: elaborate its body with `self: type_name` in scope,
    /// naming the GGC function `Type__method` (concrete static dispatch).
    fn elaborate_equip_method(&mut self, type_name: &str, fd: &ast::FunctionDef) -> ElabResult<Function> {
        let (params, body) = self.elaborate_fn_body(fd, Some(type_name))?;
        Ok(Function { name: format!("{type_name}__{}", fd.name.node), params, body, span: fd.span })
    }

    /// Shared body elaboration for free functions and `equip` methods: reset the
    /// per-function name/type/mode env, resolve param modes + types, and lower
    /// the body. `self_type` is `Some(T)` inside an `equip T` method.
    fn elaborate_fn_body(
        &mut self,
        fd: &ast::FunctionDef,
        self_type: Option<&str>,
    ) -> ElabResult<(Vec<Param>, Vec<Stmt>)> {
        // Reset + populate the per-function name/type/mode env (params + all
        // bound names) so closure detection, dispatch, and D4 are scoped.
        self.local_names.clear();
        self.local_ty.clear();
        self.local_mode.clear();
        self.current_self_type = self_type.map(|s| s.to_string());
        // A `throws E` function's return type is widened to `Result[T, E]`:
        // `return`/`throw`/fall-off wrap `Ok`/`Error`, and nested throws-calls
        // auto-propagate (RFC §2.6 row 1). `autoprop_suppressed` starts clear.
        self.current_fn_throws = fd.throws.is_some();
        self.autoprop_suppressed = false;
        for p in &fd.params {
            self.local_names.insert(p.node.name.node.clone());
        }
        if let ast::FunctionBody::Block(block) = &fd.body {
            collect_bound_names(block, &mut self.local_names);
        }

        let mut params = Vec::with_capacity(fd.params.len());
        for p in &fd.params {
            if p.node.is_meta_op {
                return Err(ElabError::new("`meta op` params are phase 2", p.span));
            }
            let name = p.node.name.node.clone();
            let pty = if matches!(p.node.type_.node, ast::Type::SelfType) {
                self_type.map(|s| Ty::Named(s.to_string())).unwrap_or(Ty::Unknown)
            } else {
                ty_of_type(&p.node.type_.node)
            };
            // Params are BORROW-mode views (materialize-on-write) unless `&`
            // (WriteThrough) / `!` (Move). Plain `self` is a bare binding (D2).
            self.local_ty.insert(name.clone(), pty);
            self.local_mode.insert(name.clone(), bindmode_of(p.node.ownership));
            params.push(Param { name, mode: mode_of(p.node.ownership), span: p.span });
        }
        let mut body = match &fd.body {
            ast::FunctionBody::Block(block) => self.elaborate_block(block)?,
            ast::FunctionBody::Expression(e) => {
                // Expression-body function: evaluate and return the value.
                // (D4 position 4 — return of a live tainted place is rejected.)
                self.reject_if_tainted_live_place(&e.node, e.span, "return")?;
                let tail = self.elaborate_expr(e)?;
                // A `throws` expression-body fn wraps its tail in `Ok(...)`
                // exactly once (a throws-call tail already auto-propagated to
                // the bare `T`, which this re-wraps — never a double-`Ok`).
                let value = if self.current_fn_throws { ok_wrap(tail) } else { tail };
                vec![Stmt::Return { value: Some(value), span: e.span }]
            }
            ast::FunctionBody::Declaration | ast::FunctionBody::Extern(_) => {
                return Err(ElabError::new(
                    "extern / declaration-only functions are out of spec v1",
                    fd.span,
                ));
            }
        };
        // Fall-off in a `throws` function returns `Ok(())` (the `void … throws
        // E` success path, and dead-but-harmless after a body that always
        // returns). Block bodies only — an expr body always returns above.
        if self.current_fn_throws && matches!(fd.body, ast::FunctionBody::Block(_)) {
            body.push(Stmt::Return { value: Some(ok_wrap(Expr::Unit)), span: fd.span });
        }
        Ok((params, body))
    }

    // ── Pass-1 collection helpers (equip + taint) ──────────────────────────

    /// Register an `equip` block's methods into the signature/method registries
    /// and (for `equip T with Drop`) the custom-drop registry + taint seed.
    fn register_equip(&mut self, eq: &ast::EquipBlock) -> ElabResult<()> {
        if eq.generic_params.is_some() {
            return Err(ElabError::new(
                "generic `equip` blocks are excluded from phase 0 (the 3 generic-equip cow \
                 fixtures are the standing exclusions)",
                eq.span,
            ));
        }
        let type_name = equip_type_name(eq)?;
        let is_drop = eq.trait_.as_ref().is_some_and(|t| trait_is_drop(&t.trait_name.node));
        for m in &eq.items {
            let mname = m.node.name.node.clone();
            let mangled = format!("{type_name}__{mname}");
            let self_mode = self_param_mode(&m.node)?;
            let param_names: Vec<String> = m
                .node
                .params
                .iter()
                .filter(|p| !is_self_param(&p.node))
                .map(|p| p.node.name.node.clone())
                .collect();
            self.func_names.insert(mangled.clone());
            self.fn_param_names.insert(mangled.clone(), param_names);
            self.fn_ret.insert(mangled.clone(), ty_of_type(&m.node.return_type.node));
            if m.node.throws.is_some() {
                self.fn_throws.insert(mangled.clone());
            }
            self.equip_methods.insert(
                (type_name.clone(), mname.clone()),
                MethodInfo { mangled: mangled.clone(), self_mode },
            );
            if is_drop && mname == "drop" {
                self.drop_fns.push((type_name.clone(), mangled));
            }
        }
        if is_drop {
            // Seed the D4 taint set: a type with a custom `Drop` is tainted.
            self.tainted.insert(type_name);
        }
        Ok(())
    }

    /// The D4 transitive drop-taint fixpoint: a struct/enum is tainted if any
    /// field / variant payload is (transitively) tainted. Seeds are the types
    /// with a custom `Drop` (set in `register_equip`).
    fn compute_taint(&mut self) {
        loop {
            let mut changed = false;
            let struct_names: Vec<String> = self.struct_field_types.keys().cloned().collect();
            for n in struct_names {
                if self.tainted.contains(&n) {
                    continue;
                }
                let fields = self.struct_field_types[&n].clone();
                if fields.iter().any(|(_, t)| self.ty_tainted(t)) {
                    self.tainted.insert(n);
                    changed = true;
                }
            }
            let enum_names: Vec<String> = self.enum_payload_types.keys().cloned().collect();
            for n in enum_names {
                if self.tainted.contains(&n) {
                    continue;
                }
                let payloads = self.enum_payload_types[&n].clone();
                if payloads.iter().any(|t| self.ty_tainted(t)) {
                    self.tainted.insert(n);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    // ── Type inference + D4 (the mode-carrying env in action) ──────────────

    /// Whether a type is (transitively) drop-tainted — a NAMED tainted type, or
    /// a container/tuple carrying one.
    fn ty_tainted(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Named(n) => self.tainted.contains(n),
            Ty::Vector(el) | Ty::Set(el) => self.ty_tainted(el),
            Ty::Dict(k, v) => self.ty_tainted(k) || self.ty_tainted(v),
            Ty::Tuple(ts) => ts.iter().any(|t| self.ty_tainted(t)),
            Ty::Prim | Ty::Str | Ty::Unknown => false,
        }
    }

    /// The declared type of `field` on struct `type_name` (for projection
    /// inference), or `Unknown`.
    fn field_ty(&self, type_name: &str, field: &str) -> Ty {
        self.struct_field_types
            .get(type_name)
            .and_then(|fs| fs.iter().find(|(n, _)| n == field))
            .map(|(_, t)| t.clone())
            .unwrap_or(Ty::Unknown)
    }

    /// Infer the type of a surface expression from the env — enough to dispatch
    /// a user `equip` method (receiver type name) and to classify a D4 source.
    /// Read-the-annotation for locals; small projection inference for the rest.
    fn infer_ast_ty(&self, e: &ast::Expr) -> Ty {
        match e {
            ast::Expr::Identifier(n) => self.local_ty.get(n).cloned().unwrap_or(Ty::Unknown),
            ast::Expr::SelfExpr => {
                self.current_self_type.clone().map(Ty::Named).unwrap_or(Ty::Unknown)
            }
            ast::Expr::FieldAccess { object, field } => match self.infer_ast_ty(&object.node) {
                Ty::Named(t) => self.field_ty(&t, &field.node),
                _ => Ty::Unknown,
            },
            ast::Expr::TupleFieldAccess { object, index } => match self.infer_ast_ty(&object.node) {
                Ty::Tuple(ts) => ts.get(*index).cloned().unwrap_or(Ty::Unknown),
                _ => Ty::Unknown,
            },
            ast::Expr::Index { object, index } => {
                if matches!(index.node, ast::Expr::Range { .. }) {
                    // A slice keeps the container kind (`v[a..b]` → Vector,
                    // `s[a..b]` → Str).
                    return self.infer_ast_ty(&object.node);
                }
                match self.infer_ast_ty(&object.node) {
                    Ty::Vector(el) | Ty::Set(el) => *el,
                    Ty::Dict(_, v) => *v,
                    Ty::Str => Ty::Str,
                    _ => Ty::Unknown,
                }
            }
            ast::Expr::StructLiteral { name, .. } => Ty::Named(name.node.clone()),
            ast::Expr::Call { callee, .. } => {
                if let ast::Expr::Identifier(name) = &callee.node {
                    if self.struct_names.contains(name) {
                        return Ty::Named(name.clone());
                    }
                    if let Some(t) = self.fn_ret.get(name) {
                        return t.clone();
                    }
                }
                Ty::Unknown
            }
            ast::Expr::Move { expr } | ast::Expr::MutableBorrow { expr } => {
                self.infer_ast_ty(&expr.node)
            }
            ast::Expr::IntLiteral(_) | ast::Expr::FloatLiteral(_) | ast::Expr::BoolLiteral(_) => {
                Ty::Prim
            }
            ast::Expr::StringLiteral(..) => Ty::Str,
            _ => Ty::Unknown,
        }
    }

    /// The ONE centralized D4 rejection (RFC §2.2). An implicit copy of a
    /// LIVE-PLACE source of drop-tainted type at any of the six positions
    /// (bind / ctor-init / collection-put / return / capture /
    /// materialize-on-write) is `E_MoveWithoutOperator`. Fresh temps move and
    /// never reach here (their sources are `Value`/`Move`, not places).
    fn reject_if_tainted_live_place(&self, e: &ast::Expr, span: Span, position: &str) -> ElabResult<()> {
        if ast_is_place(e) && self.ty_tainted(&self.infer_ast_ty(e)) {
            return Err(ElabError::new(
                format!(
                    "E_MoveWithoutOperator: implicit copy of a drop-tainted value at {position}; \
                     a type with a custom `Drop` is single-owner — write `!<src>` to move or \
                     `<src>.clone()` to copy"
                ),
                span,
            ));
        }
        Ok(())
    }

    /// D4 position 6 (materialize-on-write): a write whose target roots at a
    /// BORROW-mode binding of a tainted type would privatise (copy) that value.
    /// Routes through the ONE helper on the root-local place.
    fn reject_materialize_on_write(&self, target: &ast::Expr, span: Span) -> ElabResult<()> {
        if let Some(root) = root_local_name(target) {
            if self.local_mode.get(root) == Some(&BindMode::Borrow) {
                let root_expr = ast::Expr::Identifier(root.to_string());
                return self.reject_if_tainted_live_place(&root_expr, span, "materialize-on-write");
            }
        }
        Ok(())
    }

    fn elaborate_block(&mut self, block: &ast::Block) -> ElabResult<Vec<Stmt>> {
        let mut out = Vec::new();
        for stmt in &block.stmts {
            out.extend(self.elaborate_stmt(stmt)?);
        }
        Ok(out)
    }

    /// One surface statement may lower to several GGC statements (the for-loop
    /// desugar), so this returns a vector.
    fn elaborate_stmt(&mut self, stmt: &Spanned<ast::Stmt>) -> ElabResult<Vec<Stmt>> {
        let span = stmt.span;
        match &stmt.node {
            ast::Stmt::VarDecl { pattern, value, type_, .. } => {
                let name = binding_name(pattern)?;
                // D4 position 1 (bind) fires inside `bind_source`'s Copy branch.
                let source = self.bind_source(value)?;
                // Record the binding's type + mode in the env (annotation, or
                // inferred from the initializer for `auto`).
                let ty = match &type_.node {
                    ast::Type::Inferred => self.infer_ast_ty(&value.node),
                    other => ty_of_type(other),
                };
                self.local_ty.insert(name.clone(), ty);
                self.local_mode.insert(name.clone(), bindmode_of_source(&source));
                Ok(vec![Stmt::Bind { name, source, span }])
            }

            ast::Stmt::Assign { target, value } => {
                // D4 position 6 (materialize-on-write): a write rooted at a
                // tainted Borrow binding privatises it — rejected.
                self.reject_materialize_on_write(&target.node, span)?;
                let target_expr = self.elaborate_expr(target)?;
                let value_src = self.owning_source_from_expr(value)?;
                Ok(vec![Stmt::Assign { target: target_expr, value: value_src, span }])
            }

            ast::Stmt::CompoundAssign { target, op, value } => {
                // `x op= e`  →  `x = x op e`
                self.reject_materialize_on_write(&target.node, span)?;
                let target_expr = self.elaborate_expr(target)?;
                let lhs = self.elaborate_expr(target)?;
                let rhs = self.elaborate_expr(value)?;
                let combined = Expr::Binary(map_binop(*op, span)?, Box::new(lhs), Box::new(rhs));
                Ok(vec![Stmt::Assign { target: target_expr, value: Source::Value(combined), span }])
            }

            ast::Stmt::Expr(e) => {
                // `print(...)` is the output effect, not an ordinary call.
                if let Some(arg) = as_print_call(e) {
                    let expr = self.elaborate_expr(arg)?;
                    return Ok(vec![Stmt::Print { expr, span }]);
                }
                Ok(vec![Stmt::Expr { expr: self.elaborate_expr(e)?, span }])
            }

            ast::Stmt::Return(opt) => {
                let value = match opt {
                    Some(e) => {
                        // D4 position 4 (return of a live tainted place).
                        self.reject_if_tainted_live_place(&e.node, e.span, "return")?;
                        let inner = self.elaborate_expr(e)?;
                        // A `throws` fn returns `Result[T, E]`: wrap the value in
                        // `Ok(...)`. A throws-call inside `e` already auto-
                        // propagated to the bare `T`, which this re-wraps once.
                        Some(if self.current_fn_throws { ok_wrap(inner) } else { inner })
                    }
                    // Bare `return`: `Ok(())` in a throws fn, `Unit` otherwise.
                    None if self.current_fn_throws => Some(ok_wrap(Expr::Unit)),
                    None => None,
                };
                Ok(vec![Stmt::Return { value, span }])
            }

            // `throw e` desugars to `return Error(e)` — the throws→Result error
            // path (RFC §2.6 row 1). Only valid inside a `throws` function; a
            // `throw` anywhere else is a LOUD elaboration error (never silently
            // dropped, the flagship silent-wrong bug this closes).
            ast::Stmt::Throw(e) => {
                if !self.current_fn_throws {
                    return Err(ElabError::new(
                        "`throw` outside a `throws` function is ill-formed",
                        span,
                    ));
                }
                let inner = self.elaborate_expr(e)?;
                Ok(vec![Stmt::Return { value: Some(error_wrap(inner)), span }])
            }

            ast::Stmt::With { bindings, body } => self.desugar_with(bindings, body, span),

            ast::Stmt::Break(None) => Ok(vec![Stmt::Break { span }]),
            ast::Stmt::Break(Some(_)) => {
                Err(ElabError::new("`break <value>` is outside the Increment-A subset", span))
            }
            ast::Stmt::Continue => Ok(vec![Stmt::Continue { span }]),
            ast::Stmt::Pass => Ok(vec![]),

            ast::Stmt::If { condition, then_body, elif_branches, else_body } => {
                Ok(vec![self.build_if(condition, then_body, elif_branches, else_body.as_ref())?])
            }

            ast::Stmt::While { condition, body, else_body } => {
                if else_body.is_some() {
                    return Err(ElabError::new("`while ... else` is outside the A subset", span));
                }
                let cond = self.elaborate_expr(condition)?;
                let body = self.elaborate_block(body)?;
                Ok(vec![Stmt::While { cond, body, span }])
            }

            ast::Stmt::Loop { body } => {
                Ok(vec![Stmt::Loop { body: self.elaborate_block(body)?, span }])
            }

            ast::Stmt::For { pattern, ownership, iterable, body, else_body } => {
                self.desugar_for(pattern, *ownership, iterable, body, else_body.as_ref(), span)
            }

            ast::Stmt::Match { scrutinee, arms, else_arm } => {
                Ok(vec![self.elaborate_match_stmt(scrutinee, arms, else_arm.as_ref(), span)?])
            }

            other => Err(ElabError::new(
                format!("statement `{}` is outside the phase-0 subset", stmt_kind(other)),
                span,
            )),
        }
    }

    fn build_if(
        &mut self,
        condition: &Spanned<ast::Expr>,
        then_body: &ast::Block,
        elifs: &[(Spanned<ast::Expr>, ast::Block)],
        else_body: Option<&ast::Block>,
    ) -> ElabResult<Stmt> {
        let cond = self.elaborate_expr(condition)?;
        let then_ = self.elaborate_block(then_body)?;
        let else_ = match elifs.split_first() {
            Some(((elif_cond, elif_body), rest)) => {
                vec![self.build_if(elif_cond, elif_body, rest, else_body)?]
            }
            None => match else_body {
                Some(b) => self.elaborate_block(b)?,
                None => Vec::new(),
            },
        };
        Ok(Stmt::If { cond, then_, else_, span: condition.span })
    }

    /// `for var in coll:` → an index `while` with a `Borrow`-view element var.
    fn desugar_for(
        &mut self,
        pattern: &Spanned<ast::Pattern>,
        ownership: ast::Ownership,
        iterable: &Spanned<ast::Expr>,
        body: &ast::Block,
        else_body: Option<&ast::Block>,
        span: Span,
    ) -> ElabResult<Vec<Stmt>> {
        if else_body.is_some() {
            return Err(ElabError::new("`for ... else` is outside the phase-0 subset", span));
        }
        if ownership != ast::Ownership::Borrow {
            // `for x in &coll` / `for x in !coll` (write-through / draining) is B2.
            return Err(ElabError::new("`for &`/`for !` iteration is Increment B2", span));
        }
        let var = binding_name(pattern)?;

        // `for i in a..b:` → a numeric `while` loop (the loop variable is a
        // fresh int per iteration, not a Borrow view of an element).
        if let ast::Expr::Range { start, end, inclusive } = &iterable.node {
            let start_e = match start {
                Some(e) => self.elaborate_expr(e)?,
                None => Expr::Int(0),
            };
            let end_e = match end {
                Some(e) => self.elaborate_expr(e)?,
                None => return Err(ElabError::new("`for` over an open-ended range is unsupported", span)),
            };
            let cmp = if *inclusive { BinOp::LtEq } else { BinOp::Lt };
            // The range loop var is a fresh int, owned per iteration.
            self.local_ty.insert(var.clone(), Ty::Prim);
            self.local_mode.insert(var.clone(), BindMode::Owned);
            let mut while_body = self.elaborate_block(body)?;
            while_body.push(Stmt::Assign {
                target: Expr::Local(var.clone()),
                value: Source::Value(Expr::Binary(
                    BinOp::Add,
                    Box::new(Expr::Local(var.clone())),
                    Box::new(Expr::Int(1)),
                )),
                span,
            });
            return Ok(vec![
                Stmt::Bind { name: var.clone(), source: Source::Value(start_e), span },
                Stmt::While {
                    cond: Expr::Binary(cmp, Box::new(Expr::Local(var)), Box::new(end_e)),
                    body: while_body,
                    span,
                },
            ]);
        }

        let coll = self.fresh("coll");
        let idx = self.fresh("i");

        // Env: the element var is a Borrow view of the collection's element
        // type; the synthesized `__coll`/`__i` are owned scratch.
        let elem_ty = match self.infer_ast_ty(&iterable.node) {
            Ty::Vector(el) | Ty::Set(el) => *el,
            Ty::Dict(_, v) => *v,
            Ty::Str => Ty::Str,
            _ => Ty::Unknown,
        };
        self.local_ty.insert(var.clone(), elem_ty);
        self.local_mode.insert(var.clone(), BindMode::Borrow);
        self.local_ty.insert(coll.clone(), self.infer_ast_ty(&iterable.node));
        self.local_mode.insert(coll.clone(), BindMode::Owned);
        self.local_ty.insert(idx.clone(), Ty::Prim);
        self.local_mode.insert(idx.clone(), BindMode::Owned);
        self.local_names.insert(coll.clone());
        self.local_names.insert(idx.clone());

        // `__coll = <iterable owning source>`
        let coll_src = self.owning_source_from_expr(iterable)?;
        // `__i = 0`
        let idx_bind = Stmt::Bind {
            name: idx.clone(),
            source: Source::Value(Expr::Int(0)),
            span,
        };
        // condition: `__i < __coll.len()`
        let cond = Expr::Binary(
            BinOp::Lt,
            Box::new(Expr::Local(idx.clone())),
            Box::new(Expr::Method {
                recv: Box::new(Expr::Local(coll.clone())),
                method: BuiltinMethod::Len,
                args: Vec::new(),
            }),
        );
        // loop body: bind the element (Borrow view), run body, `__i += 1`.
        let mut while_body = Vec::new();
        while_body.push(Stmt::Bind {
            name: var,
            source: Source::BorrowView(Expr::Index(
                Box::new(Expr::Local(coll.clone())),
                Box::new(Expr::Local(idx.clone())),
            )),
            span,
        });
        while_body.extend(self.elaborate_block(body)?);
        while_body.push(Stmt::Assign {
            target: Expr::Local(idx.clone()),
            value: Source::Value(Expr::Binary(
                BinOp::Add,
                Box::new(Expr::Local(idx.clone())),
                Box::new(Expr::Int(1)),
            )),
            span,
        });

        Ok(vec![
            Stmt::Bind { name: coll, source: coll_src, span },
            idx_bind,
            Stmt::While { cond, body: while_body, span },
        ])
    }

    /// `with expr as name:` → a NEW scoped `Stmt::With` (RFC §2.6). Multiple
    /// bindings nest outer→inner so each drops at block exit in reverse order.
    /// The resource is NOT inlined as a plain `Bind` — that would drop it at the
    /// enclosing function's exit, not the block's.
    fn desugar_with(
        &mut self,
        bindings: &[ast::WithBinding],
        body: &ast::Block,
        span: Span,
    ) -> ElabResult<Vec<Stmt>> {
        let Some((first, rest)) = bindings.split_first() else {
            return self.elaborate_block(body);
        };
        let name = first.name.node.clone();
        // Same source classification as a `let` bind — `with Res(1) as r` is a
        // fresh-temp Move (constructor temp, not a place ⇒ `Value`, never a D4
        // rejection); `with somePlace as r` would be a live-place copy.
        let source = self.bind_source(&first.expr)?;
        self.local_ty.insert(name.clone(), self.infer_ast_ty(&first.expr.node));
        self.local_mode.insert(name.clone(), bindmode_of_source(&source));
        self.local_names.insert(name.clone());
        let inner = self.desugar_with(rest, body, span)?;
        Ok(vec![Stmt::With { name, source, body: inner, span }])
    }

    // ── Source classification (the copy/move/borrow decision) ──────────────

    /// The RHS of a `let` binding (an implicit-copy position, but `&` makes a
    /// write-through alias and `!` a move). D4 position 1 (bind) lives on the
    /// Copy branch.
    fn bind_source(&mut self, value: &Spanned<ast::Expr>) -> ElabResult<Source> {
        match &value.node {
            ast::Expr::Move { expr } => Ok(Source::Move(self.elaborate_expr(expr)?)),
            ast::Expr::MutableBorrow { expr } => Ok(Source::WriteThrough(self.elaborate_expr(expr)?)),
            _ if is_clone_call(&value.node) => Ok(Source::Value(self.elaborate_expr(value)?)),
            _ if ast_is_place(&value.node) => {
                self.reject_if_tainted_live_place(&value.node, value.span, "bind")?;
                Ok(Source::Copy(self.elaborate_expr(value)?))
            }
            _ => Ok(Source::Value(self.elaborate_expr(value)?)),
        }
    }

    /// A value in an OWNING position from a bare expression (assign RHS, array/
    /// tuple/struct-literal element). No write-through alias is permitted here.
    /// D4 positions 2/3 (ctor-init / collection-put) live on the Copy branch.
    fn owning_source_from_expr(&mut self, value: &Spanned<ast::Expr>) -> ElabResult<Source> {
        match &value.node {
            ast::Expr::Move { expr } => Ok(Source::Move(self.elaborate_expr(expr)?)),
            ast::Expr::MutableBorrow { .. } => {
                Err(ElabError::new("`&`-alias in an owning position is not valid", value.span))
            }
            _ if is_clone_call(&value.node) => Ok(Source::Value(self.elaborate_expr(value)?)),
            _ if ast_is_place(&value.node) => {
                self.reject_if_tainted_live_place(&value.node, value.span, "ctor-init/collection-put")?;
                Ok(Source::Copy(self.elaborate_expr(value)?))
            }
            _ => Ok(Source::Value(self.elaborate_expr(value)?)),
        }
    }

    /// A value in an OWNING position from a call-arg (collection put, struct/
    /// enum field init): the sigil rides `CallArg.ownership`. D4 positions 2/3
    /// live on the Copy branch.
    fn owning_source_from_arg(&mut self, arg: &ast::CallArg) -> ElabResult<Source> {
        match arg.ownership {
            ast::Ownership::Move => Ok(Source::Move(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::MutableBorrow => {
                Err(ElabError::new("`&`-alias into an owning position is not valid", arg.value.span))
            }
            ast::Ownership::Borrow => {
                if is_clone_call(&arg.value.node) {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                } else if ast_is_place(&arg.value.node) {
                    self.reject_if_tainted_live_place(
                        &arg.value.node,
                        arg.value.span,
                        "ctor-init/collection-put",
                    )?;
                    Ok(Source::Copy(self.elaborate_expr(&arg.value)?))
                } else {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                }
            }
        }
    }

    /// A function-call argument: bare params are BORROW **views**, so a bare
    /// place becomes a view (not a copy). `&`/`!` ride `CallArg.ownership`.
    /// Reject named arguments at positions that bind POSITIONALLY (ordinary
    /// calls, enum/collection constructors, closure-value calls). Silently
    /// dropping the name would mis-bind (RFC discipline: never silently
    /// mis-evaluate). Struct construction reorders named args and does not
    /// come through here; call-side named-arg REORDER is an Increment-B2
    /// deliverable.
    fn reject_named_args(&self, args: &[gorget::span::Spanned<ast::CallArg>], what: &str) -> ElabResult<()> {
        for a in args {
            if let Some(n) = &a.node.name {
                return Err(ElabError::new(
                    format!("named argument `{}` is not supported at a {what} in ggdef Increment B1 (positional binding would silently mis-bind; named-arg reorder for calls is Increment B2)", n.node),
                    a.span,
                ));
            }
        }
        Ok(())
    }

    fn call_arg_source(&mut self, arg: &ast::CallArg) -> ElabResult<Source> {
        match arg.ownership {
            ast::Ownership::Move => Ok(Source::Move(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::MutableBorrow => Ok(Source::WriteThrough(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::Borrow => {
                if ast_is_place(&arg.value.node) {
                    Ok(Source::BorrowView(self.elaborate_expr(&arg.value)?))
                } else {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                }
            }
        }
    }

    // ── Expressions ────────────────────────────────────────────────────────

    fn elaborate_expr(&mut self, expr: &Spanned<ast::Expr>) -> ElabResult<Expr> {
        let span = expr.span;
        match &expr.node {
            ast::Expr::IntLiteral(i) => Ok(Expr::Int(*i)),
            ast::Expr::FloatLiteral(f) => Ok(Expr::Float(*f)),
            ast::Expr::BoolLiteral(b) => Ok(Expr::Bool(*b)),
            ast::Expr::StringLiteral(lit, interps) => self.elaborate_string(lit, interps, span),

            ast::Expr::Identifier(name) => Ok(Expr::Local(name.clone())),
            // `self` inside an `equip` method body is the `self` binding.
            ast::Expr::SelfExpr => Ok(Expr::Local("self".to_string())),

            ast::Expr::FieldAccess { object, field } => {
                Ok(Expr::Field(Box::new(self.elaborate_expr(object)?), field.node.clone()))
            }
            ast::Expr::TupleFieldAccess { object, index } => {
                Ok(Expr::TupleField(Box::new(self.elaborate_expr(object)?), *index))
            }
            ast::Expr::Index { object, index } => {
                // `s[a..b]` / `v[a..b]` → a `Slice`; `x[i]` → an `Index`.
                if let ast::Expr::Range { start, end, inclusive } = &index.node {
                    let object = Box::new(self.elaborate_expr(object)?);
                    let start = self.opt_expr(start.as_deref())?;
                    let end = self.opt_expr(end.as_deref())?;
                    Ok(Expr::Slice { object, start, end, inclusive: *inclusive })
                } else {
                    Ok(Expr::Index(
                        Box::new(self.elaborate_expr(object)?),
                        Box::new(self.elaborate_expr(index)?),
                    ))
                }
            }

            ast::Expr::BinaryOp { left, op, right } => Ok(Expr::Binary(
                map_binop(*op, span)?,
                Box::new(self.elaborate_expr(left)?),
                Box::new(self.elaborate_expr(right)?),
            )),
            ast::Expr::UnaryOp { op, operand } => {
                Ok(Expr::Unary(map_unop(*op, span)?, Box::new(self.elaborate_expr(operand)?)))
            }

            ast::Expr::Move { expr } => {
                // Bare `!x` in a read position (e.g. `print(!x)`): read the
                // moved value. Faithful move-kill is applied only at binding /
                // owning positions, which route through the `Source` helpers.
                self.elaborate_expr(expr)
            }

            ast::Expr::Call { callee, generic_args, args } => {
                self.elaborate_call(callee, generic_args.is_some(), args, span)
            }

            ast::Expr::MethodCall { receiver, method, args, .. } => {
                self.elaborate_method(receiver, &method.node, args, span)
            }

            ast::Expr::ArrayLiteral(elems) => {
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Vector, args: out })
            }
            ast::Expr::TupleLiteral(elems) => {
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Tuple, args: out })
            }
            ast::Expr::StructLiteral { name, args, .. } => {
                let mut out = Vec::with_capacity(args.len());
                for e in args {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Struct(name.node.clone()), args: out })
            }

            ast::Expr::NoneLiteral => Ok(enum_construct("Option", "None", Vec::new())),

            ast::Expr::As { expr, type_ } => {
                let inner = self.elaborate_expr(expr)?;
                let target = cast_target(&type_.node, type_.span)?;
                Ok(Expr::Cast { expr: Box::new(inner), target })
            }

            ast::Expr::Closure { is_async, params, body, .. } => {
                self.elaborate_closure(*is_async, params, body, span)
            }

            ast::Expr::Match { scrutinee, arms, else_arm } => {
                // See `elaborate_match_stmt`: suppress auto-prop on the scrutinee
                // iff the arms consume the `Result` itself (`case Ok/Error`).
                let consumes = arms.iter().any(|a| pattern_consumes_result(&a.pattern.node));
                self.autoprop_suppressed = consumes;
                let scrut = self.elaborate_expr(scrutinee)?;
                self.autoprop_suppressed = false;
                let mut ggc_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    if arm.guard.is_some() {
                        return Err(ElabError::new("match guards are outside the phase-0 subset", arm.span));
                    }
                    ggc_arms.push(ExprArm {
                        pattern: self.elaborate_pattern(&arm.pattern)?,
                        body: self.elaborate_expr(&arm.body)?,
                    });
                }
                let else_arm = match else_arm {
                    Some(e) => Some(Box::new(self.elaborate_expr(e)?)),
                    None => None,
                };
                Ok(Expr::Match { scrutinee: Box::new(scrut), arms: ggc_arms, else_arm, span })
            }

            other => Err(ElabError::new(
                format!("expression `{}` is outside the phase-0 subset", expr_kind(other)),
                span,
            )),
        }
    }

    fn elaborate_string(
        &mut self,
        lit: &gorget::lexer::token::StringLiteral,
        interps: &[Spanned<ast::Expr>],
        span: Span,
    ) -> ElabResult<Expr> {
        match lit.kind {
            StringKind::Format => {
                let mut parts = Vec::new();
                let mut next_interp = 0usize;
                for seg in &lit.segments {
                    match seg {
                        StringSegment::Literal(s) => parts.push(FPart::Lit(s.clone())),
                        StringSegment::Interpolation(_, _) => {
                            let e = interps.get(next_interp).ok_or_else(|| {
                                ElabError::new("f-string interpolation without a parsed expr", span)
                            })?;
                            next_interp += 1;
                            parts.push(FPart::Interp(self.elaborate_expr(e)?));
                        }
                    }
                }
                Ok(Expr::FString(parts))
            }
            StringKind::Normal | StringKind::Raw | StringKind::MultiLine => {
                Ok(Expr::Str(lit.as_plain_text()))
            }
            StringKind::Byte | StringKind::CStr => {
                Err(ElabError::new("byte / cstr literals are outside the A subset", span))
            }
        }
    }

    fn elaborate_call(
        &mut self,
        callee: &Spanned<ast::Expr>,
        has_generic_args: bool,
        args: &[Spanned<ast::CallArg>],
        _span: Span,
    ) -> ElabResult<Expr> {
        // `None()` — the callee is a `NoneLiteral`, not an identifier.
        if matches!(callee.node, ast::Expr::NoneLiteral) {
            return Ok(enum_construct("Option", "None", Vec::new()));
        }
        let ast::Expr::Identifier(name) = &callee.node else {
            return Err(ElabError::new("only named callees are supported in phase 0", callee.span));
        };
        // `print(...)` in expression position (e.g. a closure body). In
        // statement position it is lowered to `Stmt::Print` upstream.
        if name == "print" {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::Call { func: "print".to_string(), args: out });
        }
        // Prelude enum constructors: `Some(x)`, `None()`, `Ok(v)`, `Error(e)`.
        if let Some(type_name) = prelude_enum_of(name) {
            self.reject_named_args(args, "prelude enum constructor")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::EnumConstruct { type_name: type_name.to_string(), variant: name.clone(), args: out });
        }
        // The `std.conv.int_to_str` shim intrinsic (§2.6 shim list).
        if name == "int_to_str" && args.len() == 1 {
            return Ok(Expr::IntToStr(Box::new(self.elaborate_expr(&args[0].node.value)?)));
        }
        // Collection constructors: `Vector[T]()`, `Dict[K,V]()`, `Set[T]()`.
        if has_generic_args {
            if let Some(kind) = collection_ctor_kind(name) {
                self.reject_named_args(args, "collection constructor")?;
                let mut out = Vec::with_capacity(args.len());
                for a in args {
                    out.push(self.owning_source_from_arg(&a.node)?);
                }
                return Ok(Expr::Construct { kind, args: out });
            }
        }
        // User-enum variant constructor spelled bare (rare): `Variant(args)`.
        if let Some(type_name) = self.user_enum_of_variant(name) {
            self.reject_named_args(args, "enum variant constructor")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::EnumConstruct { type_name, variant: name.clone(), args: out });
        }
        // Struct construction: `Res("x")`, `Person("Alice", 30)`, `Point(x=1, y=2)`.
        if self.struct_names.contains(name) {
            let out = self.struct_ctor_args(name, args)?;
            return Ok(Expr::Construct { kind: ConstructKind::Struct(name.clone()), args: out });
        }
        // A first-class closure value stored in a local: `f()`, `grow()`.
        if self.local_names.contains(name) && !self.func_names.contains(name) {
            self.reject_named_args(args, "closure-value call")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::CallValue { callee: Box::new(Expr::Local(name.clone())), args: out });
        }
        // Ordinary function call — (B2, from B1 output-review R2) named args
        // are REORDERED to the callee's param order via the pass-1 signature
        // registry, replacing the B1-interim rejection.
        if self.func_names.contains(name) {
            // Consume the Result-consuming flag for THIS (outermost) call before
            // its args elaborate, so a nested throws-call in an arg still
            // auto-propagates.
            let suppressed = std::mem::take(&mut self.autoprop_suppressed);
            let out = self.call_args_reordered(name, args)?;
            let call = Expr::Call { func: name.clone(), args: out };
            return self.maybe_wrap_throws_call(call, name, callee.span, suppressed);
        }
        Err(ElabError::new(
            format!("unresolved callee `{name}` (unknown function/struct/enum; may need Increment B2)"),
            callee.span,
        ))
    }

    /// Apply the throws→Result auto-propagate wrap to a `call` to the GGC
    /// function/method `callee_name` (RFC §2.6 row 1). A call to a NON-throws
    /// callee passes through untouched. For a throws callee:
    ///   * `suppressed` (a Result-consuming match scrutinee, `case Ok/Error`)
    ///     → yield the `Result` value directly (the consumer destructures it);
    ///   * else inside a `throws` fn → wrap in `Propagate` (`?` semantics:
    ///     `Ok(x)` peels to `x`, `Error(e)` early-returns from the caller);
    ///   * else (non-`throws` context, not consumed) → LOUD ElabError. In real
    ///     Gorget this is a type error; ggdef refuses it rather than silently
    ///     mis-evaluating the dropped `throws` effect (the flagship safety bug).
    fn maybe_wrap_throws_call(
        &self,
        call: Expr,
        callee_name: &str,
        span: Span,
        suppressed: bool,
    ) -> ElabResult<Expr> {
        if !self.fn_throws.contains(callee_name) || suppressed {
            return Ok(call);
        }
        if self.current_fn_throws {
            return Ok(Expr::Propagate(Box::new(call)));
        }
        Err(ElabError::new(
            format!(
                "call to `throws` function `{callee_name}` in a non-`throws` context that does \
                 not consume the `Result` (no `match … case Ok/Error`); ggdef does not model \
                 `catch`/`??`/`rethrow` error recovery yet"
            ),
            span,
        ))
    }

    /// Elaborate struct-construction args, honouring named args (`Point(x=1,
    /// y=2)`): reorder to the struct's declaration order.
    fn struct_ctor_args(
        &mut self,
        struct_name: &str,
        args: &[Spanned<ast::CallArg>],
    ) -> ElabResult<Vec<Source>> {
        let any_named = args.iter().any(|a| a.node.name.is_some());
        if !any_named {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(out);
        }
        let field_order: Vec<String> = self
            .structs
            .iter()
            .find(|s| s.name == struct_name)
            .map(|s| s.fields.clone())
            .unwrap_or_default();
        let mut by_name: Vec<(String, Source)> = Vec::with_capacity(args.len());
        for a in args {
            let name = a
                .node
                .name
                .as_ref()
                .ok_or_else(|| ElabError::new("mixed positional/named struct args are unsupported", a.span))?
                .node
                .clone();
            by_name.push((name, self.owning_source_from_arg(&a.node)?));
        }
        let mut out = Vec::with_capacity(field_order.len());
        for f in &field_order {
            let pos = by_name
                .iter()
                .position(|(n, _)| n == f)
                .ok_or_else(|| ElabError::new(format!("missing field `{f}` in `{struct_name}(...)`"), args[0].span))?;
            out.push(by_name.remove(pos).1);
        }
        Ok(out)
    }

    /// Elaborate ordinary function-call args, honouring named args by reordering
    /// them to the callee's declared param order (mirrors `struct_ctor_args`,
    /// but classifies each arg with the borrow-view call rule). Positional args
    /// pass through unchanged.
    fn call_args_reordered(
        &mut self,
        func_name: &str,
        args: &[Spanned<ast::CallArg>],
    ) -> ElabResult<Vec<Source>> {
        let any_named = args.iter().any(|a| a.node.name.is_some());
        if !any_named {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(out);
        }
        let order = self.fn_param_names.get(func_name).cloned().unwrap_or_default();
        let mut by_name: Vec<(String, Source)> = Vec::with_capacity(args.len());
        for a in args {
            let name = a
                .node
                .name
                .as_ref()
                .ok_or_else(|| {
                    ElabError::new("mixed positional/named call args are unsupported", a.span)
                })?
                .node
                .clone();
            by_name.push((name, self.call_arg_source(&a.node)?));
        }
        let mut out = Vec::with_capacity(order.len());
        for p in &order {
            let pos = by_name.iter().position(|(n, _)| n == p).ok_or_else(|| {
                ElabError::new(format!("missing argument `{p}` in `{func_name}(...)`"), args[0].span)
            })?;
            out.push(by_name.remove(pos).1);
        }
        Ok(out)
    }

    /// The enum a user-declared variant belongs to (for bare-spelled ctors).
    fn user_enum_of_variant(&self, variant: &str) -> Option<String> {
        self.enums
            .iter()
            .find(|e| e.variants.iter().any(|(v, _)| v == variant))
            .map(|e| e.name.clone())
    }

    /// The arity of `variant` in enum `type_name`, if that enum exists.
    fn enum_variant_arity(&self, type_name: &str, variant: &str) -> Option<usize> {
        self.enums
            .iter()
            .find(|e| e.name == type_name)?
            .variants
            .iter()
            .find(|(v, _)| v == variant)
            .map(|(_, a)| *a)
    }

    fn elaborate_method(
        &mut self,
        receiver: &Spanned<ast::Expr>,
        method: &str,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        // Enum-variant construction is parsed as a method call on the type name
        // (`Token.Ident("x")` → MethodCall{ recv: Token, method: Ident }).
        if let ast::Expr::Identifier(type_name) = &receiver.node {
            if let Some(arity) = self.enum_variant_arity(type_name, method) {
                if arity != args.len() {
                    return Err(ElabError::new(
                        format!("variant `{type_name}.{method}` expects {arity} field(s), got {}", args.len()),
                        span,
                    ));
                }
                // Enum-variant payloads bind POSITIONALLY — named args would
                // silently mis-bind, so reject (call-side reorder is for
                // ordinary function/method calls only).
                self.reject_named_args(args, "enum variant constructor")?;
                let mut out = Vec::with_capacity(args.len());
                for a in args {
                    out.push(self.owning_source_from_arg(&a.node)?);
                }
                return Ok(Expr::EnumConstruct {
                    type_name: type_name.clone(),
                    variant: method.to_string(),
                    args: out,
                });
            }
        }

        // User `equip` method dispatch — via RECEIVER-TYPE INFERENCE (read the
        // annotation). This precedes the builtin table so a user method whose
        // name COLLIDES with a builtin (`get`, `set`) resolves to the user
        // method — the corpus has exactly this collision (`cow_named_recv_gate_*`),
        // which name-matching cannot disambiguate.
        if let Ty::Named(type_name) = self.infer_ast_ty(&receiver.node) {
            if let Some(minfo) = self.equip_methods.get(&(type_name, method.to_string())).cloned() {
                return self.elaborate_user_method_call(receiver, &minfo, args, span);
            }
        }

        let recv = Box::new(self.elaborate_expr(receiver)?);
        // `(method, expected-arg-count)` for the fixed-arity builtins.
        let (bm, argn): (BuiltinMethod, Option<usize>) = match method {
            "push" => (BuiltinMethod::Push, Some(1)),
            "set" | "put" => (BuiltinMethod::Set, Some(2)),
            "len" => (BuiltinMethod::Len, Some(0)),
            "get" => (BuiltinMethod::Get, Some(1)),
            "unwrap" => (BuiltinMethod::Unwrap, Some(0)),
            "unwrap_or" => (BuiltinMethod::UnwrapOr, Some(1)),
            "pop" => (BuiltinMethod::Pop, Some(0)),
            "clear" => (BuiltinMethod::Clear, Some(0)),
            "fill" => (BuiltinMethod::Fill, Some(2)),
            "add" => (BuiltinMethod::Add, Some(1)),
            "trim" => (BuiltinMethod::Trim, Some(0)),
            "substring" => (BuiltinMethod::Substring, Some(2)),
            "clone" => {
                if !args.is_empty() {
                    return Err(ElabError::new("`.clone` takes no args", span));
                }
                return Ok(Expr::Clone(recv));
            }
            other => {
                return Err(ElabError::new(
                    format!("method `.{other}()` is outside the phase-0 subset (may need Increment B2)"),
                    span,
                ));
            }
        };
        if let Some(n) = argn {
            if args.len() != n {
                return Err(ElabError::new(format!("`.{method}` takes {n} arg(s)"), span));
            }
        }
        // A mutating builtin on a tainted Borrow root would materialize it (D4
        // position 6). Read-only builtins never reach here.
        if matches!(
            bm,
            BuiltinMethod::Push
                | BuiltinMethod::Set
                | BuiltinMethod::Pop
                | BuiltinMethod::Clear
                | BuiltinMethod::Fill
                | BuiltinMethod::Add
        ) {
            self.reject_materialize_on_write(&receiver.node, span)?;
        }
        let mut out = Vec::with_capacity(args.len());
        for a in args {
            out.push(self.owning_source_from_arg(&a.node)?);
        }
        Ok(Expr::Method { recv, method: bm, args: out })
    }

    /// Lower a user `equip` method call to a `Type__method` GGC function call.
    /// The receiver becomes `self`, bound per the method's self-mode (D2: plain
    /// `self` = a bare Borrow view that materializes on write; `&self` =
    /// WriteThrough; `!self` = Move). Non-self args follow the ordinary
    /// borrow-view call rule and honour named-arg reorder.
    fn elaborate_user_method_call(
        &mut self,
        receiver: &Spanned<ast::Expr>,
        minfo: &MethodInfo,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        // D4 position 6, user-method sibling (B2 output-review R2): a `&self`
        // mutator writing through a TAINTED Borrow-rooted receiver would
        // materialize it — same rejection as the builtin-mutator site. (The
        // plain-`self`-write case needs method-body write analysis: phase 1.)
        if minfo.self_mode == Mode::WriteThrough {
            self.reject_materialize_on_write(&receiver.node, receiver.span)?;
        }
        // Consume the Result-consuming flag for THIS call before receiver/args
        // elaborate (a nested throws-call in them still auto-propagates).
        let suppressed = std::mem::take(&mut self.autoprop_suppressed);
        let self_src = self.self_source(receiver, minfo.self_mode)?;
        let mut out = vec![self_src];
        out.extend(self.call_args_reordered(&minfo.mangled, args)?);
        let call = Expr::Call { func: minfo.mangled.clone(), args: out };
        self.maybe_wrap_throws_call(call, &minfo.mangled, span, suppressed)
    }

    /// Bind a method receiver as the `self` source per the resolved self-mode.
    fn self_source(&mut self, receiver: &Spanned<ast::Expr>, self_mode: Mode) -> ElabResult<Source> {
        let is_place = ast_is_place(&receiver.node);
        let e = self.elaborate_expr(receiver)?;
        Ok(match self_mode {
            Mode::WriteThrough => Source::WriteThrough(e),
            Mode::Move => Source::Move(e),
            // Plain `self` is a bare binding: a view of a place, a fresh temp
            // otherwise (a temp receiver is moved into `self`).
            Mode::Borrow if is_place => Source::BorrowView(e),
            Mode::Borrow => Source::Value(e),
        })
    }

    /// Elaborate a `match` in statement position (arm bodies are blocks).
    fn elaborate_match_stmt(
        &mut self,
        scrutinee: &Spanned<ast::Expr>,
        arms: &[ast::MatchItem],
        else_arm: Option<&ast::Block>,
        span: Span,
    ) -> ElabResult<Stmt> {
        // A scrutinee that is a throws-call yields the `Result` value (no auto-
        // propagate) exactly when the arms destructure `Result` (`case
        // Ok/Error`); otherwise the throws-call auto-propagates and the arms see
        // the inner value.
        let consumes = arms
            .iter()
            .filter_map(|i| i.arm())
            .any(|a| pattern_consumes_result(&a.pattern.node));
        self.autoprop_suppressed = consumes;
        let scrut = self.elaborate_expr(scrutinee)?;
        self.autoprop_suppressed = false;
        let mut out_arms = Vec::with_capacity(arms.len());
        for item in arms {
            let arm = item
                .arm()
                .ok_or_else(|| ElabError::new("`meta for` match arms are phase 2", span))?;
            if arm.guard.is_some() {
                return Err(ElabError::new("match guards are outside the phase-0 subset", arm.span));
            }
            out_arms.push(StmtArm {
                pattern: self.elaborate_pattern(&arm.pattern)?,
                body: self.arm_body_block(&arm.body)?,
            });
        }
        let else_ = match else_arm {
            Some(b) => Some(self.elaborate_block(b)?),
            None => None,
        };
        Ok(Stmt::Match { scrutinee: scrut, arms: out_arms, else_arm: else_, span })
    }

    /// A statement-match arm body: a block, or a single expression treated as
    /// a statement (so `case 0: print("x")` lowers to a `Print`).
    fn arm_body_block(&mut self, body: &Spanned<ast::Expr>) -> ElabResult<Vec<Stmt>> {
        if let ast::Expr::Block(b) = &body.node {
            return self.elaborate_block(b);
        }
        if let Some(arg) = as_print_call(body) {
            return Ok(vec![Stmt::Print { expr: self.elaborate_expr(arg)?, span: body.span }]);
        }
        Ok(vec![Stmt::Expr { expr: self.elaborate_expr(body)?, span: body.span }])
    }

    fn elaborate_pattern(&mut self, pat: &Spanned<ast::Pattern>) -> ElabResult<Pattern> {
        match &pat.node {
            ast::Pattern::Wildcard => Ok(Pattern::Wildcard),
            ast::Pattern::Binding(name) => Ok(Pattern::Binding(name.clone())),
            ast::Pattern::Literal(e) => Ok(Pattern::Literal(Box::new(self.elaborate_expr(e)?))),
            ast::Pattern::Constructor { path, fields } => {
                let variant = path
                    .last()
                    .ok_or_else(|| ElabError::new("empty constructor path", pat.span))?
                    .node
                    .clone();
                let mut fs = Vec::with_capacity(fields.len());
                for f in fields {
                    fs.push(self.elaborate_pattern(f)?);
                }
                Ok(Pattern::Variant { variant, fields: fs })
            }
            ast::Pattern::DotShorthand { variant, fields } => {
                let mut fs = Vec::with_capacity(fields.len());
                for f in fields {
                    fs.push(self.elaborate_pattern(f)?);
                }
                Ok(Pattern::Variant { variant: variant.node.clone(), fields: fs })
            }
            _ => Err(ElabError::new("pattern shape is outside the phase-0 subset", pat.span)),
        }
    }

    /// Elaborate a bare (by-value) closure into `Program.closures`, computing
    /// its capture set (free enclosing-locals referenced in the body).
    fn elaborate_closure(
        &mut self,
        is_async: bool,
        params: &[Spanned<ast::ClosureParam>],
        body: &Spanned<ast::Expr>,
        span: Span,
    ) -> ElabResult<Expr> {
        if is_async {
            return Err(ElabError::new("async closures are phase 3", span));
        }
        let mut cparams = Vec::with_capacity(params.len());
        for p in params {
            if p.node.destructure.is_some() {
                return Err(ElabError::new("closure param destructuring is outside phase 0", p.span));
            }
            cparams.push(Param {
                name: p.node.name.node.clone(),
                mode: mode_of(p.node.ownership),
                span: p.span,
            });
        }
        let cbody = self.elaborate_expr(body)?;
        // Capture set: enclosing locals referenced in the body, minus the
        // closure's own params. Deterministic order (sorted).
        let mut used = HashSet::new();
        collect_expr_locals(&cbody, &self.closures, &mut used);
        let cparam_names: HashSet<String> = cparams.iter().map(|p| p.name.clone()).collect();
        let mut captures: Vec<String> = self
            .local_names
            .iter()
            .filter(|n| used.contains(*n) && !cparam_names.contains(*n))
            .cloned()
            .collect();
        captures.sort();
        // D4 position 5 (closure capture): capturing a drop-tainted local by
        // value is an implicit copy — rejected (capture `!name` to move).
        for c in &captures {
            if let Some(ty) = self.local_ty.get(c) {
                if self.ty_tainted(ty) {
                    return Err(ElabError::new(
                        format!(
                            "E_MoveWithoutOperator: closure captures the drop-tainted local `{c}` \
                             by value; a type with a custom `Drop` is single-owner — capture \
                             `!{c}` to move or `{c}.clone()` to copy"
                        ),
                        span,
                    ));
                }
            }
        }
        let id = self.closures.len();
        self.closures.push(ClosureDef { params: cparams, captures, body: cbody, span });
        Ok(Expr::Closure(id))
    }

    /// Elaborate an optional sub-expression (range endpoints), boxing it.
    fn opt_expr(&mut self, e: Option<&Spanned<ast::Expr>>) -> ElabResult<Option<Box<Expr>>> {
        match e {
            Some(e) => Ok(Some(Box::new(self.elaborate_expr(e)?))),
            None => Ok(None),
        }
    }
}

// ── Small helpers ──────────────────────────────────────────────────────────

/// Build an `EnumConstruct` expression.
fn enum_construct(type_name: &str, variant: &str, args: Vec<Source>) -> Expr {
    Expr::EnumConstruct { type_name: type_name.to_string(), variant: variant.to_string(), args }
}

/// `Ok(inner)` — the throws→Result success wrap (RFC §2.6 row 1). `inner` moves
/// into the payload (a fresh value at this owning position).
fn ok_wrap(inner: Expr) -> Expr {
    enum_construct("Result", "Ok", vec![Source::Value(inner)])
}

/// `Error(inner)` — the `throw`-statement desugar (`throw e` → `return
/// Error(e)`).
fn error_wrap(inner: Expr) -> Expr {
    enum_construct("Result", "Error", vec![Source::Value(inner)])
}

/// Whether a `match` arm pattern destructures the `Result` itself (`case
/// Ok(..)` / `case Error(..)`) — the signal that a throws-call scrutinee should
/// yield the `Result` value rather than auto-propagate.
fn pattern_consumes_result(pat: &ast::Pattern) -> bool {
    match pat {
        ast::Pattern::Constructor { path, .. } => {
            path.last().map(|s| s.node == "Ok" || s.node == "Error").unwrap_or(false)
        }
        ast::Pattern::DotShorthand { variant, .. } => {
            variant.node == "Ok" || variant.node == "Error"
        }
        _ => false,
    }
}

/// The prelude enum a bare constructor name belongs to (`Some`/`None` →
/// `Option`, `Ok`/`Error` → `Result`).
fn prelude_enum_of(name: &str) -> Option<&'static str> {
    match name {
        "Some" | "None" => Some("Option"),
        "Ok" | "Error" => Some("Result"),
        _ => None,
    }
}

/// The construct kind for a generic collection constructor call.
fn collection_ctor_kind(name: &str) -> Option<ConstructKind> {
    match name {
        "Vector" => Some(ConstructKind::Vector),
        "Dict" | "HashMap" => Some(ConstructKind::Dict),
        "Set" | "HashSet" => Some(ConstructKind::Set),
        _ => None,
    }
}

/// Map a surface type to an `as`-cast target (unit-tested only).
fn cast_target(ty: &ast::Type, span: Span) -> ElabResult<CastTarget> {
    use gorget::parser::ast::PrimitiveType as P;
    let ast::Type::Primitive(p) = ty else {
        return Err(ElabError::new("`as`-cast target must be a numeric primitive", span));
    };
    Ok(match p {
        P::Int8 => CastTarget::Int { bits: 8, signed: true },
        P::Int16 => CastTarget::Int { bits: 16, signed: true },
        P::Int32 => CastTarget::Int { bits: 32, signed: true },
        P::Int | P::Int64 => CastTarget::Int { bits: 64, signed: true },
        P::Uint8 => CastTarget::Int { bits: 8, signed: false },
        P::Uint16 => CastTarget::Int { bits: 16, signed: false },
        P::Uint32 => CastTarget::Int { bits: 32, signed: false },
        P::Uint | P::Uint64 => CastTarget::Int { bits: 64, signed: false },
        P::Float32 => CastTarget::Float32,
        P::Float | P::Float64 => CastTarget::Float64,
        _ => return Err(ElabError::new("`as`-cast target must be a numeric primitive", span)),
    })
}

/// Pre-pass: collect every name bound (var decls, for-vars) in a block, for the
/// per-function local-name set. Recurses through nested blocks.
fn collect_bound_names(block: &ast::Block, out: &mut HashSet<String>) {
    for stmt in &block.stmts {
        collect_stmt_bound_names(&stmt.node, out);
    }
}

fn collect_stmt_bound_names(stmt: &ast::Stmt, out: &mut HashSet<String>) {
    match stmt {
        ast::Stmt::VarDecl { pattern, .. } => pattern_names(&pattern.node, out),
        ast::Stmt::If { then_body, elif_branches, else_body, .. } => {
            collect_bound_names(then_body, out);
            for (_, b) in elif_branches {
                collect_bound_names(b, out);
            }
            if let Some(b) = else_body {
                collect_bound_names(b, out);
            }
        }
        ast::Stmt::While { body, .. } | ast::Stmt::Loop { body } => collect_bound_names(body, out),
        ast::Stmt::For { pattern, body, .. } => {
            pattern_names(&pattern.node, out);
            collect_bound_names(body, out);
        }
        ast::Stmt::With { bindings, body } => {
            for b in bindings {
                out.insert(b.name.node.clone());
            }
            collect_bound_names(body, out);
        }
        ast::Stmt::Match { arms, else_arm, .. } => {
            for item in arms {
                if let Some(arm) = item.arm() {
                    if let ast::Expr::Block(b) = &arm.body.node {
                        collect_bound_names(b, out);
                    }
                }
            }
            if let Some(b) = else_arm {
                collect_bound_names(b, out);
            }
        }
        _ => {}
    }
}

fn pattern_names(pat: &ast::Pattern, out: &mut HashSet<String>) {
    match pat {
        ast::Pattern::Binding(n) => {
            out.insert(n.clone());
        }
        ast::Pattern::Tuple(ps) => {
            for p in ps {
                pattern_names(&p.node, out);
            }
        }
        ast::Pattern::Constructor { fields, .. } | ast::Pattern::DotShorthand { fields, .. } => {
            for f in fields {
                pattern_names(&f.node, out);
            }
        }
        _ => {}
    }
}

/// Collect the local names referenced in a GGC expression (for closure capture
/// sets). Recurses through sub-expressions and sources; a nested closure
/// contributes its own (already-computed) capture names transitively.
fn collect_expr_locals(e: &Expr, closures: &[ClosureDef], out: &mut HashSet<String>) {
    match e {
        Expr::Local(n) => {
            out.insert(n.clone());
        }
        Expr::Int(_) | Expr::Bool(_) | Expr::Float(_) | Expr::Str(_) | Expr::Unit => {}
        Expr::FString(parts) => {
            for p in parts {
                if let FPart::Interp(e) = p {
                    collect_expr_locals(e, closures, out);
                }
            }
        }
        Expr::Field(o, _) | Expr::TupleField(o, _) => collect_expr_locals(o, closures, out),
        Expr::Index(o, i) => {
            collect_expr_locals(o, closures, out);
            collect_expr_locals(i, closures, out);
        }
        Expr::Slice { object, start, end, .. } => {
            collect_expr_locals(object, closures, out);
            if let Some(s) = start {
                collect_expr_locals(s, closures, out);
            }
            if let Some(en) = end {
                collect_expr_locals(en, closures, out);
            }
        }
        Expr::Binary(_, l, r) => {
            collect_expr_locals(l, closures, out);
            collect_expr_locals(r, closures, out);
        }
        Expr::Unary(_, e)
        | Expr::Cast { expr: e, .. }
        | Expr::IntToStr(e)
        | Expr::Clone(e)
        | Expr::Propagate(e) => collect_expr_locals(e, closures, out),
        Expr::Call { args, .. } | Expr::Construct { args, .. } | Expr::EnumConstruct { args, .. } => {
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::CallValue { callee, args } => {
            collect_expr_locals(callee, closures, out);
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::Method { recv, args, .. } => {
            collect_expr_locals(recv, closures, out);
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::Closure(id) => {
            for c in &closures[*id].captures {
                out.insert(c.clone());
            }
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            collect_expr_locals(scrutinee, closures, out);
            for a in arms {
                collect_expr_locals(&a.body, closures, out);
            }
            if let Some(e) = else_arm {
                collect_expr_locals(e, closures, out);
            }
        }
    }
}

fn collect_source_locals(s: &Source, closures: &[ClosureDef], out: &mut HashSet<String>) {
    match s {
        Source::Copy(e)
        | Source::Move(e)
        | Source::BorrowView(e)
        | Source::WriteThrough(e)
        | Source::Value(e) => collect_expr_locals(e, closures, out),
    }
}

fn mode_of(o: ast::Ownership) -> Mode {
    match o {
        ast::Ownership::Borrow => Mode::Borrow,
        ast::Ownership::MutableBorrow => Mode::WriteThrough,
        ast::Ownership::Move => Mode::Move,
    }
}

/// A param's env BINDING mode: a bare param is a BORROW view (materialize-on-
/// write, D2 for plain `self`); `&` = WriteThrough; `!` = Move.
fn bindmode_of(o: ast::Ownership) -> BindMode {
    match o {
        ast::Ownership::Borrow => BindMode::Borrow,
        ast::Ownership::MutableBorrow => BindMode::WriteThrough,
        ast::Ownership::Move => BindMode::Move,
    }
}

/// A `let` / `with` binding's env mode from its classified source: a Copy/Value/
/// Move binding OWNS its value (never materializes); a `&` binding is a
/// WriteThrough alias. (`BorrowView` is never produced at a `let`.)
fn bindmode_of_source(s: &Source) -> BindMode {
    match s {
        Source::WriteThrough(_) => BindMode::WriteThrough,
        Source::Move(_) => BindMode::Move,
        Source::BorrowView(_) => BindMode::Borrow,
        Source::Copy(_) | Source::Value(_) => BindMode::Owned,
    }
}

/// Map a surface type annotation to an inferred `Ty`. `SelfType` resolves to
/// `Unknown` here (it is only meaningful inside a method, where the caller
/// special-cases it against the current self-type).
fn ty_of_type(t: &ast::Type) -> Ty {
    use gorget::parser::ast::PrimitiveType as P;
    match t {
        ast::Type::Primitive(P::StringType) => Ty::Str,
        ast::Type::Primitive(_) => Ty::Prim,
        ast::Type::Named { name, generic_args } => {
            let arg = |i: usize| generic_args.get(i).map(|t| ty_of_type(&t.node)).unwrap_or(Ty::Unknown);
            match name.node.as_str() {
                "Vector" => Ty::Vector(Box::new(arg(0))),
                "Set" | "HashSet" => Ty::Set(Box::new(arg(0))),
                "Dict" | "HashMap" => Ty::Dict(Box::new(arg(0)), Box::new(arg(1))),
                "String" => Ty::Str,
                // Option/Result carry no user methods — a Named type with no
                // equip entries just falls through dispatch to the builtins.
                other => Ty::Named(other.to_string()),
            }
        }
        ast::Type::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| ty_of_type(&t.node)).collect()),
        ast::Type::Ref(inner) | ast::Type::Owned(inner) => ty_of_type(&inner.node),
        _ => Ty::Unknown,
    }
}

/// The named target of an `equip` block (`equip Res:` / `equip Res with Drop:`).
fn equip_type_name(eq: &ast::EquipBlock) -> ElabResult<String> {
    match &eq.type_.node {
        ast::Type::Named { name, .. } => Ok(name.node.clone()),
        _ => Err(ElabError::new("`equip` target must be a named type in phase 0", eq.type_.span)),
    }
}

/// Whether an `equip … with <trait>` trait is `Drop`.
fn trait_is_drop(t: &ast::Type) -> bool {
    matches!(t, ast::Type::Named { name, .. } if name.node == "Drop")
}

/// Whether a param is the method receiver `self`.
fn is_self_param(p: &ast::Param) -> bool {
    matches!(p.type_.node, ast::Type::SelfType) || p.name.node == "self"
}

/// The self-param's binding mode for an `equip` method (D2: plain `self` =
/// Borrow; `&self` = WriteThrough; `!self`/`consuming self` = Move).
fn self_param_mode(fd: &ast::FunctionDef) -> ElabResult<Mode> {
    for p in &fd.params {
        if is_self_param(&p.node) {
            return Ok(mode_of(p.node.ownership));
        }
    }
    Err(ElabError::new("`equip` method without a `self` param is outside phase 0", fd.span))
}

/// The root local a place expression is rooted at (`v[0].name` → `v`,
/// `self.field` → `self`), or `None` for a non-place.
fn root_local_name(e: &ast::Expr) -> Option<&str> {
    match e {
        ast::Expr::Identifier(n) => Some(n),
        ast::Expr::SelfExpr => Some("self"),
        ast::Expr::FieldAccess { object, .. }
        | ast::Expr::TupleFieldAccess { object, .. }
        | ast::Expr::Index { object, .. } => root_local_name(&object.node),
        _ => None,
    }
}

/// Whether a surface expression denotes a place (a named storage location),
/// so binding/arg positions can pick copy/view rather than fresh-temp.
fn ast_is_place(e: &ast::Expr) -> bool {
    match e {
        ast::Expr::Identifier(_) | ast::Expr::SelfExpr => true,
        ast::Expr::FieldAccess { object, .. } | ast::Expr::TupleFieldAccess { object, .. } => {
            ast_is_place(&object.node)
        }
        // `x[i]` is a place, but `x[a..b]` (a slice) is a fresh value.
        ast::Expr::Index { object, index } => {
            !matches!(index.node, ast::Expr::Range { .. }) && ast_is_place(&object.node)
        }
        _ => false,
    }
}

fn is_clone_call(e: &ast::Expr) -> bool {
    matches!(e, ast::Expr::MethodCall { method, args, .. } if method.node == "clone" && args.is_empty())
}

/// If `e` is a `print(arg)` call, return the single argument expression.
fn as_print_call(e: &Spanned<ast::Expr>) -> Option<&Spanned<ast::Expr>> {
    if let ast::Expr::Call { callee, args, .. } = &e.node {
        if let ast::Expr::Identifier(name) = &callee.node {
            if name == "print" && args.len() == 1 {
                return Some(&args[0].node.value);
            }
        }
    }
    None
}

fn binding_name(pattern: &Spanned<ast::Pattern>) -> ElabResult<String> {
    match &pattern.node {
        ast::Pattern::Binding(name) => Ok(name.clone()),
        _ => Err(ElabError::new("only simple bindings are supported in Increment A", pattern.span)),
    }
}

fn map_binop(op: ast::BinaryOp, span: Span) -> ElabResult<BinOp> {
    use ast::BinaryOp as B;
    Ok(match op {
        B::Add => BinOp::Add,
        B::Sub => BinOp::Sub,
        B::Mul => BinOp::Mul,
        B::Div => BinOp::Div,
        B::Rem | B::Mod => BinOp::Rem,
        B::Eq => BinOp::Eq,
        B::Neq => BinOp::Neq,
        B::Lt => BinOp::Lt,
        B::Gt => BinOp::Gt,
        B::LtEq => BinOp::LtEq,
        B::GtEq => BinOp::GtEq,
        B::And => BinOp::And,
        B::Or => BinOp::Or,
        other => {
            return Err(ElabError::new(
                format!("operator {other:?} is outside the Increment-A subset"),
                span,
            ));
        }
    })
}

fn map_unop(op: ast::UnaryOp, span: Span) -> ElabResult<UnOp> {
    match op {
        ast::UnaryOp::Neg => Ok(UnOp::Neg),
        ast::UnaryOp::Not => Ok(UnOp::Not),
        ast::UnaryOp::BitNot => {
            Err(ElabError::new("bitwise-not is outside the Increment-A subset", span))
        }
    }
}

// ── Diagnostic labels (for stop-and-report clarity) ────────────────────────

fn item_kind(item: &ast::Item) -> &'static str {
    match item {
        ast::Item::Function(_) => "function",
        ast::Item::Struct(_) => "struct",
        ast::Item::Enum(_) => "enum",
        ast::Item::Trait(_) => "trait",
        ast::Item::Equip(_) => "equip",
        ast::Item::Import(_) => "import",
        ast::Item::TypeAlias(_) => "type-alias",
        ast::Item::Newtype(_) => "newtype",
        ast::Item::ConstDecl(_) => "const",
        ast::Item::StaticDecl(_) => "static",
        _ => "other",
    }
}

fn item_span(item: &ast::Item) -> Span {
    match item {
        ast::Item::Function(f) => f.span,
        ast::Item::Struct(s) => s.span,
        ast::Item::Enum(e) => e.span,
        ast::Item::Trait(t) => t.span,
        ast::Item::Equip(e) => e.span,
        ast::Item::Import(i) => i.span(),
        ast::Item::Newtype(n) => n.span,
        ast::Item::ConstDecl(c) => c.span,
        ast::Item::StaticDecl(s) => s.span,
        _ => Span::dummy(),
    }
}

fn stmt_kind(s: &ast::Stmt) -> &'static str {
    match s {
        ast::Stmt::Throw(_) => "throw",
        ast::Stmt::OnError { .. } => "on-error",
        ast::Stmt::Match { .. } => "match",
        ast::Stmt::Select { .. } => "select",
        ast::Stmt::With { .. } => "with",
        ast::Stmt::Unsafe { .. } => "unsafe",
        ast::Stmt::NamedScope { .. } => "named-scope",
        ast::Stmt::Assert { .. } => "assert",
        _ => "unsupported",
    }
}

fn expr_kind(e: &ast::Expr) -> &'static str {
    match e {
        ast::Expr::Match { .. } => "match",
        ast::Expr::If { .. } => "if-expr",
        ast::Expr::Closure { .. } => "closure",
        ast::Expr::Range { .. } => "range",
        ast::Expr::NoneLiteral => "None",
        ast::Expr::DotShorthand { .. } => "dot-shorthand",
        ast::Expr::As { .. } => "as-cast",
        _ => "unsupported",
    }
}
