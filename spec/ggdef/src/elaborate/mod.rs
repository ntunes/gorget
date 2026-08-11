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

mod liveness;
pub(crate) use liveness::check_liveness;

use gorget::lexer::token::{StringKind, StringSegment};
use gorget::parser::ast;
use gorget::span::{Span, Spanned};

use crate::ggc::{
    BinOp, BuiltinMethod, CastTarget, ClosureDef, ConstructKind, D29Reject, EnumDef, Expr, ExprArm,
    FPart, Function, Mode, Param, Pattern, Program, Source, Stmt, StmtArm, StructDef, UnOp,
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
                el.fn_param_tys.insert(
                    name.clone(),
                    fd.params.iter().map(|p| ty_of_type(&p.node.type_.node)).collect(),
                );
                el.fn_param_modes.insert(
                    name.clone(),
                    fd.params.iter().map(|p| mode_of(p.node.ownership)).collect(),
                );
                el.fn_ret.insert(name.clone(), ty_of_type(&fd.return_type.node));
                if fd.throws.declares_throws() {
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
        display_fns: el.display_fns,
        // D29: the first bare-fallible-mark violation (if any), surfaced by `run`
        // as an `IllFormed` + `E_MissingFallibleMark` reject BEFORE eval.
        d29_reject: el.d29_reject,
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
    /// `Option[T]` — a prelude enum carrying `T`. Payload-carrying (unlike the
    /// other `Named` types) so D4 taint sees through it: `Option[R]` (R custom
    /// `Drop`) is drop-tainted, matching production's `is_drop_tainted_type`
    /// recursion over generic args.
    Option(Box<Ty>),
    /// `Result[T, E]` — a prelude enum carrying success `T` and error `E`.
    /// Payload-carrying for the same D4-taint reason; taints if EITHER arm is
    /// tainted (production taints both `Result[R,_]` and `Result[_,R]`).
    Result(Box<Ty>, Box<Ty>),
    /// A user struct/enum (carry no user methods that dispatch differently).
    Named(String),
    /// A first-class callable (`Callable[..]` / `MutCallable[..]` /
    /// `ConsumeCallable[..]`). `consuming` is true ONLY for `ConsumeCallable`:
    /// a single-owner callable whose call consumes it (D5 kind axis). Resolved
    /// once here so the `CallValue` elaboration reads a typed field rather than
    /// name-matching the surface spelling (layering rule 2).
    ///
    /// `param_ownerships` carries the callable's declared per-parameter sigil
    /// modes (Track B3, D31-uniform: the call-site sigil rule applies
    /// UNIFORMLY at every INDIRECT call site whose callee has a resolvable
    /// function type). Empty when the callable's generic arg is not a
    /// `Function` type (e.g. `Callable[Unknown]`), in which case the sigil
    /// check falls back to Unknown just like the direct-call path does.
    Callable { consuming: bool, param_ownerships: Vec<Mode> },
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

/// Round XXIV Track D — mirror of `src/semantic/typecheck.rs:197-208`
/// `ClosureCombinatorCell`. Classifies which payload axis to unify a
/// closure return type against for the 3 unify-eligible closure-returning
/// combinators. See the twin lint
/// `tests/lints.rs::unify_closure_ret_axis_class_enumeration` — it pins
/// this enum's variant count in lockstep with the production twin.
///
/// Deliberately out-of-class (mirroring the production doc-comment):
///   - `.map` / `.map_err` — scalar-returning closures (no axis).
///   - `Result.{flat_map, filter}` + `Option.{map_err, unwrap_error}` —
///     one-sided combinators on the wrong-shape receiver. Ratified
///     Option-only / Result-only per `docs/language-reference.md:3861-3891`.
///     Rejected at `elaborate_method` (Round XXV Track B) with
///     `error[E_NoMethodFound]:` — a "method doesn't exist" reject, not
///     an axis-unify cell (there is no axis to unify when the method is
///     not part of the receiver's protocol). Result.flatten reaches the
///     `other =>` catch-all in the arm-picker (no `BuiltinMethod::Flatten`
///     variant exists).
///   - `Option.and_then` / `Option.flat_map` — legitimate cross-type map.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureCombinatorCell {
    /// `Result[T, E].or_else((E) -> Result[T', E'])` — unify T' == T (Ok-axis).
    /// The Error axis IS the recovery axis; E' ≠ E is legitimate.
    ResultOrElse,
    /// `Result[T, E].and_then((T) -> Result[U, E'])` — unify E' == E (Err-axis).
    /// The Ok axis IS the mapped axis; U ≠ T is legitimate.
    ResultAndThen,
    /// `Option[T].or_else(() -> Option[T'])` — unify T' == T (Some-axis).
    /// Option has one payload; the recovery closure must produce the same T.
    OptionOrElse,
}

/// Render a `Ty` for a diagnostic message. Mirrors production's
/// `describe_resolved_type` shape (`Result[Money, int]`, `Option[int]`, etc.).
/// `Ty::Prim` collapses int/bool/float/unsigned — see
/// `unify_closure_ret_axis`'s precision-gap doc.
fn ty_display(t: &Ty) -> String {
    match t {
        Ty::Prim => "<prim>".to_string(),
        Ty::Str => "String".to_string(),
        Ty::Unknown => "<unknown>".to_string(),
        Ty::Vector(el) => format!("Vector[{}]", ty_display(el)),
        Ty::Set(el) => format!("Set[{}]", ty_display(el)),
        Ty::Dict(k, v) => format!("Dict[{}, {}]", ty_display(k), ty_display(v)),
        Ty::Tuple(ts) => {
            let inner: Vec<_> = ts.iter().map(ty_display).collect();
            format!("({})", inner.join(", "))
        }
        Ty::Option(el) => format!("Option[{}]", ty_display(el)),
        Ty::Result(ok, err) => format!("Result[{}, {}]", ty_display(ok), ty_display(err)),
        Ty::Named(n) => n.clone(),
        Ty::Callable { consuming, .. } => {
            if *consuming { "ConsumeCallable[..]".to_string() } else { "Callable[..]".to_string() }
        }
    }
}

/// A resolved `equip` method: its GGC function name (`Type__method`) and the
/// mode its `self` param binds under (D2: plain `self` = Borrow).
#[derive(Clone, Debug)]
struct MethodInfo {
    mangled: String,
    self_mode: Mode,
}

/// The surface form a `call_args_reordered` call was spelled in. Production's
/// call-site sigil check is ASYMMETRIC (`check_call_ownership` runs on the
/// `Expr::Call` arm only, `check_expr.rs:315`): free-fn call sites require the
/// arg sigil to match the declared param mode; method calls never reach the
/// check, so a bare place arg into a `&` param silently binds the alias and
/// writes through. Typed here so the two behaviors cannot blur.
#[derive(Clone, Copy, PartialEq, Debug)]
enum CallForm {
    FreeFn,
    Method,
}

/// The §10.3 "Type-Directed Result Capture" context of the fallible call
/// currently being elaborated. Set by the destination position JUST before its
/// value expression elaborates; consumed (`mem::take`) by the outermost
/// function/method call so nested calls in args still take the D29 bare-mark
/// path (ground-truthed: production requires a mark on a nested fallible call
/// inside a captured call's args). The `Scrutinee` context was RETIRED by D29
/// (2026-07-17): a bare `match f():` scrutinee is a bare call — bind to a
/// `Result` first, or mark + match the success value.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
enum CaptureCtx {
    /// Not a capturing destination: a bare (unmarked) fallible call here is the
    /// `E_MissingFallibleMark` reject (D29 — propagation needs the `f()!` mark).
    #[default]
    None,
    /// A destination whose DECLARED type is `Result[_,_]` (§10.3): VarDecl,
    /// assign target, return slot, declared fn/method param, struct field.
    /// The call yields its full `Result` (an UNMARKED capture — the amendment),
    /// with the callee-T-is-itself-Result ambiguity rejected loudly (see
    /// `maybe_wrap_throws_call`).
    TypedDest,
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
    /// desugar; read at call sites to decide propagate-vs-capture-vs-consume.
    fn_throws: HashSet<String>,
    /// Signature registry: fn / method name → declared param types, in decl
    /// order (methods exclude `self`, aligning with `fn_param_names`). Serves
    /// §10.3 type-directed capture at call args (a throws-call arg whose param
    /// is declared `Result[_,_]` captures instead of auto-propagating).
    fn_param_tys: HashMap<String, Vec<Ty>>,
    /// Signature registry: fn / method name → declared param MODES, in decl
    /// order (methods exclude `self`, aligning with `fn_param_names`). Serves
    /// the "unbroken `&`-chain" (§3.1): a bare place arg into a `&`
    /// (WriteThrough) param aliases the caller's place — the `&` on the PARAM
    /// declaration drives write-through, no call-site sigil required.
    fn_param_modes: HashMap<String, Vec<Mode>>,
    /// `(type-name, method-name) → MethodInfo` for user `equip` methods.
    equip_methods: HashMap<(String, String), MethodInfo>,
    /// `equip T with Drop` registry: `(type-name, drop-fn-name)`.
    drop_fns: Vec<(String, String)>,
    /// `equip T with Displayable` registry: `(type-name, display-fn-name)`.
    /// Round XXVI Track B: `eval::format_for_print` reads this via
    /// `Ctx::display_fns` and dispatches the user's `display(self) -> String`
    /// instead of the default `Type{k: v}` render.
    display_fns: Vec<(String, String)>,
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
    /// PARAM names of the CURRENT function (params only — NOT for-vars,
    /// match-bindings, or `self`-derived locals). The single-owner callable
    /// bind/reassign gate skips these (production `!def.is_param`,
    /// `check_stmt.rs:1464`); ctor sites do NOT skip them.
    param_names: HashSet<String>,
    /// The type name of `self` while elaborating an `equip` method body.
    current_self_type: Option<String>,
    /// Whether the function CURRENTLY being elaborated is `throws E`: its
    /// `return`/`throw`/fall-off are wrapped `Ok`/`Error`, and a nested throws-
    /// call in a non-capturing position auto-propagates.
    current_fn_throws: bool,
    /// Whether the current function's DECLARED return type is `Result[_,_]`
    /// (for a `throws` fn this is the declared success type `T`, as written) —
    /// drives §10.3 capture at return positions.
    current_fn_ret_is_result: bool,
    /// The capture context for the throws-call currently being elaborated
    /// (see `CaptureCtx`). Set by a destination position just before its value
    /// elaborates; consumed by the outermost call.
    capture_ctx: CaptureCtx,
    /// The DECLARED/INFERRED type of the destination whose value is currently
    /// being elaborated. A brace literal `{a, b, c}` and a bracket literal
    /// `[a, b, c]` share ONE AST node (`ArrayLiteral`) — set vs vector is
    /// DESTINATION-TYPE directed (production disambiguates the same way, both
    /// spellings). Lifecycle: SET at a destination position (VarDecl annotation;
    /// assignment target's inferred type; re-armed per element with the
    /// container's element type inside the `ArrayLiteral` arm), CONSUMED
    /// (`take`) by the `ArrayLiteral` arm, and KILLED at the entry of every
    /// other `elaborate_expr` node — so the hint reaches exactly the literal
    /// that IS the destination's value, never a literal nested in a call arg
    /// (the p5 leak) or any other unrelated position.
    dest_ty_hint: Option<Ty>,
    /// D29: the FIRST bare-fallible-mark violation seen while elaborating (first
    /// wins, mirroring the liveness gate's first-Halt). Carried onto the emitted
    /// `Program` and surfaced by `run` as an `IllFormed` + `E_MissingFallibleMark`
    /// reject code. `None` when the program is D29-clean.
    d29_reject: Option<D29Reject>,
    /// D29 mark one-shot (the ggdef analog of the Rust checker's
    /// `fallible_call_marked`): set true by the `ast::Expr::Propagate` arm just
    /// before its inner elaborates, TAKEN by the DIRECT call it wraps (so the
    /// call is NOT the missing-mark reject). Captured at `elaborate_call` /
    /// method-call entry (via `mem::take`) BEFORE the args elaborate, so a nested
    /// bare fallible call in an arg (`parse(g())!`) still takes the bare-mark
    /// path — the mark binds to the call it directly wraps.
    fallible_marked: bool,
    /// Round XXVIII Track C — D32:1278-1281 `!`-in-operand-position reject
    /// (ggdef lane, sibling of Rust `suppress_move_in_operand_position` and SH
    /// `DkMoveInOperandPosition`). Set to `true` when about to elaborate an
    /// expression at a RESTING / consuming-boundary position where a direct
    /// top-level `Expr::Move` is legit: Stmt::Return value, Stmt::Throw value,
    /// closure body tail (line 523), match arm bodies + else (propagate from
    /// enclosing owning context). The `elaborate_expr::Expr::Move` arm
    /// REJECTS unless this flag is set. Reset to `false` on entry to every
    /// operand-position recursion (binop operands, unop operand, index, as-
    /// cast, propagate inner, match scrutinee, if/while cond, f-string interp,
    /// field access object, method receiver) via `elaborate_expr` bracketing
    /// helpers; match arm bodies / else propagate via `elaborate_expr_ctx`.
    /// This mirrors what the Rust safety pass does with per-arm suppress and
    /// what the SH walker will mirror by threading a bool on ResolveContext.
    in_owning_context: bool,
    gensym: usize,
}

impl Elaborator {
    fn fresh(&mut self, hint: &str) -> String {
        let n = self.gensym;
        self.gensym += 1;
        format!("__{hint}_{n}")
    }

    /// D29: record a bare-fallible-mark violation (first wins). Elaboration
    /// CONTINUES after recording — the reject is surfaced by `run` (as
    /// `IllFormed` + `E_MissingFallibleMark`) which short-circuits before eval,
    /// so the placeholder the recording site returns is never evaluated. This is
    /// the ratified-static-rejection channel (a coded `IllFormed`), distinct from
    /// an `ElabError` (an out-of-subset `FrontendError`).
    fn record_d29_reject(&mut self, message: impl Into<String>, span: Span) {
        if self.d29_reject.is_none() {
            self.d29_reject = Some(D29Reject { message: message.into(), span });
        }
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
        self.param_names.clear();
        self.current_self_type = self_type.map(|s| s.to_string());
        // A `throws E` function's return type is widened to `Result[T, E]`:
        // `return`/`throw`/fall-off wrap `Ok`/`Error`, and nested throws-calls
        // auto-propagate (RFC §2.6 row 1). `capture_ctx` starts clear.
        self.current_fn_throws = fd.throws.declares_throws();
        self.current_fn_ret_is_result = ty_is_result(&ty_of_type(&fd.return_type.node));
        self.capture_ctx = CaptureCtx::None;
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
            self.param_names.insert(name.clone());
            params.push(Param { name, mode: mode_of(p.node.ownership), span: p.span });
        }
        let mut body = match &fd.body {
            ast::FunctionBody::Block(block) => self.elaborate_block(block)?,
            ast::FunctionBody::Expression(e) => {
                // Expression-body function: evaluate and return the value.
                // (D4 position 4 — return of a live tainted place is rejected.)
                self.reject_if_tainted_live_place(&e.node, e.span, "return")?;
                // D29: a fallible expression-body tail is an UNMARKED capture at
                // a `Result`-declared return (the tail slot is the annotated
                // return type) — legal, mirroring the block-body return capture.
                // (The old "production REJECTS this shape" LOUD guard was a
                // pre-D23/D29 fossil; ground-truthed 2026-07-17 that Rust gg
                // accepts+runs `Result[…] f(…) throws E: risky(x)`.) A bare tail
                // that is NOT a capture (a non-`throws`/`Result`-returning fn)
                // still takes the ordinary bare-mark path via `maybe_wrap`.
                if self.current_fn_ret_is_result
                    && (self.root_throws_callee(&e.node).is_some()
                        || self.root_kind2_callee(&e.node).is_some())
                {
                    self.capture_ctx = CaptureCtx::TypedDest;
                }
                // Round XXVIII Track C — an expression-body fn's tail IS its
                // return value (implicit SReturn lowering below). Treat as
                // RESTING so a direct top-level `!x` tail is legit.
                let tail = self.elaborate_expr_direct_move_owning(e)?;
                self.capture_ctx = CaptureCtx::None;
                // A `throws` expression-body fn wraps its captured tail in
                // `Ok(...)` exactly once — the tail captured the callee's full
                // `Result` as the declared success value `T`.
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
        let is_displayable =
            eq.trait_.as_ref().is_some_and(|t| trait_is_displayable(&t.trait_name.node));
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
            let param_tys: Vec<Ty> = m
                .node
                .params
                .iter()
                .filter(|p| !is_self_param(&p.node))
                .map(|p| ty_of_type(&p.node.type_.node))
                .collect();
            let param_modes: Vec<Mode> = m
                .node
                .params
                .iter()
                .filter(|p| !is_self_param(&p.node))
                .map(|p| mode_of(p.node.ownership))
                .collect();
            self.func_names.insert(mangled.clone());
            self.fn_param_names.insert(mangled.clone(), param_names);
            self.fn_param_tys.insert(mangled.clone(), param_tys);
            self.fn_param_modes.insert(mangled.clone(), param_modes);
            self.fn_ret.insert(mangled.clone(), ty_of_type(&m.node.return_type.node));
            if m.node.throws.declares_throws() {
                self.fn_throws.insert(mangled.clone());
            }
            self.equip_methods.insert(
                (type_name.clone(), mname.clone()),
                MethodInfo { mangled: mangled.clone(), self_mode },
            );
            if is_drop && mname == "drop" {
                self.drop_fns.push((type_name.clone(), mangled.clone()));
            }
            if is_displayable && mname == "display" {
                // Round XXVI Track B: register the user's Displayable impl so
                // `format_for_print` dispatches it instead of the default shape.
                self.display_fns.push((type_name.clone(), mangled));
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
            // Prelude enums see through to their payload(s): `Option[R]` /
            // `Result[R,_]` / `Result[_,R]` carry the tainted value, so an
            // implicit copy duplicates R's drop just as a bare `R` would.
            Ty::Option(el) => self.ty_tainted(el),
            Ty::Result(ok, err) => self.ty_tainted(ok) || self.ty_tainted(err),
            // A callable is single-owner-by-design with a PURE drop (D4/D5): it
            // is never drop-TAINTED (that axis is side-effectful custom drops).
            Ty::Callable { .. } => false,
            Ty::Prim | Ty::Str | Ty::Unknown => false,
        }
    }

    /// Whether a type is **Copy** — a value snapshot at a bare read (the D10(b)
    /// place-overlap exemption: a Copy read participates in no overlap). Mirrors
    /// production `is_copy_type` (`src/semantic/safety/type_utils.rs`) within
    /// ggdef's `Ty` vocabulary: a scalar, a tuple of Copy elements, or a
    /// NON-tainted user struct / enum ALL of whose fields / payloads are
    /// (transitively) Copy. `String` owns a heap buffer, collections own a
    /// buffer, callables are single-owner, and the prelude generics
    /// `Option`/`Result` are `Generic` in production — ABSENT from its Copy
    /// whitelist — so none are Copy. A drop-tainted type is NEVER Copy (D4/D12
    /// mutual exclusion: copying it would duplicate a side-effectful drop).
    fn ty_is_copy(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Prim => true,
            Ty::Tuple(ts) => ts.iter().all(|t| self.ty_is_copy(t)),
            Ty::Named(n) => {
                if self.tainted.contains(n) {
                    return false;
                }
                if let Some(fields) = self.struct_field_types.get(n) {
                    return fields.iter().all(|(_, t)| self.ty_is_copy(t));
                }
                if let Some(payloads) = self.enum_payload_types.get(n) {
                    return payloads.iter().all(|t| self.ty_is_copy(t));
                }
                false
            }
            Ty::Str
            | Ty::Vector(_)
            | Ty::Dict(_, _)
            | Ty::Set(_)
            | Ty::Option(_)
            | Ty::Result(_, _)
            | Ty::Callable { .. }
            | Ty::Unknown => false,
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
                    "error[E_MoveWithoutOperator]: implicit copy of a drop-tainted value at {position}; \
                     a type with a custom `Drop` is single-owner — write `!<src>` to move or \
                     `<src>.clone()` to copy"
                ),
                span,
            ));
        }
        Ok(())
    }

    /// The D12 single-owner-**callable** init check (the carve-out family,
    /// mirroring production `require_explicit_move_for_single_owner_init` /
    /// the bare-assign `needs_explicit_move` branch). A `Callable` /
    /// `MutCallable` / `ConsumeCallable` value is single-owner: at an
    /// explicit-move INIT boundary (bare local bind, whole reassign, struct /
    /// enum / prelude-enum variant field init) a bare copy is rejected — write
    /// `!src` to move (or `src.clone()`). This is a SEPARATE axis from
    /// drop-taint (`ty_tainted` stays callable-clean, per D4/D5 pure drop), and
    /// it is IDENTIFIER-gated (a bare local of callable type), matching
    /// production's `needs_explicit_move` identifier branch. It deliberately
    /// does NOT fire at return, collection put/set, collection/array/tuple
    /// literal, or capture positions — a callable moves there at last use
    /// (measured: `return f` / `v.push(f)` / `[f]` accepted on both compilers).
    fn reject_if_single_owner_callable_init(&self, e: &ast::Expr, span: Span, position: &str, exempt_params: bool) -> ElabResult<()> {
        if let ast::Expr::Identifier(n) = e {
            // At a BIND / whole-reassign boundary a bare PARAM callable is
            // accepted (a param is a borrowed view; re-binding copies a pointer,
            // not the owner) — production `!def.is_param`, `check_stmt.rs:1464`.
            // Ctor / struct / enum init sites pass `exempt_params = false`: a
            // param IS rejected there (production `require_explicit_move_for_
            // single_owner_init`, check_expr.rs:42, has NO is_param gate).
            if exempt_params && self.param_names.contains(n) {
                return Ok(());
            }
            if matches!(self.local_ty.get(n), Some(Ty::Callable { .. })) {
                return Err(ElabError::new(
                    format!(
                        "error[E_MoveWithoutOperator]: implicit copy of the single-owner callable `{n}` at \
                         {position}; a callable is single-owner (no implicit copy) — write `!{n}` to \
                         move or `{n}.clone()` to copy"
                    ),
                    span,
                ));
            }
        }
        Ok(())
    }

    /// The `CallArg` form of the single-owner-callable init check: the move
    /// sigil rides `CallArg.ownership` (an `!f` arg has value = bare identifier,
    /// ownership = `Move`), so gate on a BARE (`Borrow`) arg before consulting
    /// the identifier — an explicit `!f` / `&f` is never a bare copy.
    fn reject_if_single_owner_callable_arg(&self, arg: &ast::CallArg, span: Span, position: &str) -> ElabResult<()> {
        if arg.ownership == ast::Ownership::Borrow {
            // Arg form is only wired at ctor / enum-variant init sites → params
            // are NOT exempt here.
            self.reject_if_single_owner_callable_init(&arg.value.node, span, position, false)?;
        }
        Ok(())
    }

    /// D4 position 6 (materialize-on-write): a write whose target roots at a
    /// BORROW-mode binding of a tainted type would privatise (copy) that value.
    /// Routes through the ONE helper on the root-local place. The root resolves
    /// via the free strict `root_local_name` FIRST (a direct `h.field = v`
    /// store), then falls back to the get-chain-descending `get_chain_root_local`
    /// (a builtin-collection getter-chain store `h.items.get(0).unwrap().f = v`,
    /// which materialises the SAME root) — extending the 2T reject to the
    /// get-chain materialize position. Mirrors production's four reject gates,
    /// each fed by `find_root_def_id(t).or_else(|| find_get_chain_taint_root(t))`
    /// (Core #9 — both lanes reject the get-chain shape).
    fn reject_materialize_on_write(&self, target: &ast::Expr, span: Span) -> ElabResult<()> {
        if let Some(root) = root_local_name(target).or_else(|| self.get_chain_root_local(target)) {
            if self.local_mode.get(root) == Some(&BindMode::Borrow) {
                let root_expr = ast::Expr::Identifier(root.to_string());
                return self.reject_if_tainted_live_place(&root_expr, span, "materialize-on-write");
            }
        }
        Ok(())
    }

    /// Like the free `root_local_name`, but ALSO descends a builtin-collection
    /// element-getter chain (`c.get(i).unwrap()`, `c.first()`, `c.last()`) to
    /// the collection's root local — so the 2T materialize-on-write reject sees
    /// that a get-chain store privatises the SAME tainted root the lowering
    /// materialises. `&self` (unlike the free `root_local_name`) because the
    /// KIND gate needs the type env; the free fn stays STRICT for its
    /// return-position caller. Kind-gated on the receiver's inferred type being a
    /// builtin Vector (or Dict — belt-and-suspenders; Dict is out of ggdef's
    /// phase-0 subset) — NEVER Set, mirroring production's
    /// `is_field_addressable_collection` = {Array, OrderedMap}: a Set element is
    /// not field-addressable, and descending it would reject a shape that does
    /// not materialize (reject ⊄ materialize). A USER `get` returning an owned
    /// temp is not descended (its receiver type is a `Named` struct, not Vector).
    fn get_chain_root_local<'a>(&self, e: &'a ast::Expr) -> Option<&'a str> {
        match e {
            ast::Expr::Identifier(n) => Some(n),
            ast::Expr::SelfExpr => Some("self"),
            ast::Expr::FieldAccess { object, .. }
            | ast::Expr::TupleFieldAccess { object, .. }
            | ast::Expr::Index { object, .. } => self.get_chain_root_local(&object.node),
            ast::Expr::MethodCall { receiver, method, .. } => {
                let descend = match method.node.as_str() {
                    // `c.get(i)`/`.first()`/`.last()` — receiver IS the collection.
                    "get" | "first" | "last" => {
                        self.is_field_addressable_collection_ty(&receiver.node)
                    }
                    // `c.get(i).unwrap()` — receiver is the getter over a collection.
                    "unwrap" | "expect" => matches!(
                        &receiver.node,
                        ast::Expr::MethodCall { receiver: inner, method: inner_m, .. }
                            if matches!(inner_m.node.as_str(), "get" | "first" | "last")
                                && self.is_field_addressable_collection_ty(&inner.node)
                    ),
                    _ => false,
                };
                if descend {
                    self.get_chain_root_local(&receiver.node)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// True if `recv`'s inferred type is a builtin Vector (or Dict) — the
    /// field-write-addressable builtin collection kinds. The ggdef mirror of
    /// production's `is_field_addressable_collection`; NEVER Set (a Set element
    /// is not field-addressable, so a Set get-chain does not materialize).
    fn is_field_addressable_collection_ty(&self, recv: &ast::Expr) -> bool {
        matches!(self.infer_ast_ty(recv), Ty::Vector(_) | Ty::Dict(_, _))
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
                // D10(a): a local `&`-bind (`auto r = &b`, projected `&b.data`,
                // or one reached through an if/match/do tail) aliases a second
                // writable path to a place — rejected. Fired FIRST so the
                // subset guard never masks it. Mirrors production check_stmt.
                if expr_is_borrow_bind(&value.node) {
                    return Err(local_borrow_bind_error(value.span));
                }
                let name = binding_name(pattern)?;
                // §10.3 type-directed capture: a throws-call initializer whose
                // DECLARED binding type is `Result[_,_]` captures the full
                // Result instead of auto-propagating (`auto` never captures —
                // there is no declared Result to direct it).
                if ty_is_result(&ty_of_type(&type_.node))
                    && self.root_throws_callee(&value.node).is_some()
                {
                    self.capture_ctx = CaptureCtx::TypedDest;
                }
                // D29 mark+capture: `Result[T,E] r = f()!` — a MARK on a call
                // whose outcome a `Result`-annotated binding already captures is
                // redundant (capture is the UNMARKED spelling). Reject with the
                // remove-the-`!` fix-it (both kinds).
                if ty_is_result(&ty_of_type(&type_.node)) {
                    if let ast::Expr::Propagate { expr } = &value.node {
                        if self.root_throws_callee(&expr.node).is_some()
                            || self.root_kind2_callee(&expr.node).is_some()
                        {
                            self.record_d29_reject(
                                "remove the `!`: a `Result`-annotated binding captures the \
                                 fallible outcome without a mark",
                                value.span,
                            );
                        }
                    }
                }
                // A brace/bracket literal RHS is set-vs-vector directed by the
                // DECLARED type (they share one `ArrayLiteral` node): hand the
                // annotation to the `ArrayLiteral` arm, cleared after the bind.
                if !matches!(type_.node, ast::Type::Inferred) {
                    self.dest_ty_hint = Some(ty_of_type(&type_.node));
                }
                // D4 position 1 (bind) fires inside `bind_source`'s Copy branch.
                let source = self.bind_source(value)?;
                self.dest_ty_hint = None;
                self.capture_ctx = CaptureCtx::None;
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
                // D10(a): `name = &expr` re-binds a mutable borrow to a name —
                // the same class as the VarDecl-init form, same rejection.
                if expr_is_borrow_bind(&value.node) {
                    return Err(local_borrow_bind_error(value.span));
                }
                // D4 position 6 (materialize-on-write): a write rooted at a
                // tainted Borrow binding privatises it — rejected.
                self.reject_materialize_on_write(&target.node, span)?;
                // The target elaborates FIRST (it may itself contain calls,
                // e.g. `v[idx()] = …`, which must not consume the RHS's
                // capture flag).
                let target_expr = self.elaborate_expr(target)?;
                // §10.3 type-directed capture at an assignment: a throws-call
                // RHS whose TARGET's type is `Result[_,_]` captures. A target
                // whose type cannot be resolved is a LOUD error (capture-vs-
                // propagate would be a coin flip — never silent).
                if self.root_throws_callee(&value.node).is_some() {
                    match self.infer_ast_ty(&target.node) {
                        t if ty_is_result(&t) => self.capture_ctx = CaptureCtx::TypedDest,
                        Ty::Unknown => {
                            return Err(ElabError::new(
                                "cannot resolve the assignment target's type for a `throws`-\
                                 call RHS (capture-vs-propagate is type-directed, §10.3); \
                                 bind through an explicitly-typed local instead",
                                span,
                            ));
                        }
                        _ => {}
                    }
                }
                // D12 single-owner: a whole-local reassign `g = f` of a bare
                // callable is a copy at a bind boundary — reject (mirrors the
                // VarDecl bind, production check_stmt bare-assign `:1490`).
                if matches!(target.node, ast::Expr::Identifier(_)) {
                    self.reject_if_single_owner_callable_init(&value.node, value.span, "bind", true)?;
                }
                // Set-vs-vector literal disambiguation at an ASSIGNMENT
                // destination (probe p15: `s = {3,3,4}` re-assign must dedupe
                // exactly like the VarDecl form): hand the target's inferred
                // type to the ArrayLiteral arm. Set AFTER the target elaborated
                // (its own elaboration clears the hint) and cleared right after
                // the value; an unresolvable target type simply leaves no hint.
                match self.infer_ast_ty(&target.node) {
                    Ty::Unknown => {}
                    t => self.dest_ty_hint = Some(t),
                }
                let value_src = self.owning_source_from_expr(value)?;
                self.dest_ty_hint = None;
                self.capture_ctx = CaptureCtx::None;
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
                // D29 kind-2 bare-DISCARD: a bare (unmarked) call whose declared
                // return is `Result[_,_]`, used as an expression statement,
                // silently drops the outcome. (Kind-1 throws calls are caught at
                // `maybe_wrap`; a MARKED call `f()!` is an `ast::Expr::Propagate`,
                // not a bare `Call`, so it is exempt and activates its channel.)
                if self.root_kind2_callee(&e.node).is_some() {
                    self.record_d29_reject(
                        "this fallible call must be marked with `!` — its `Result` outcome is \
                         dropped; mark it to propagate, handle it with `catch`/`rethrow`, or \
                         capture it into a `Result` binding",
                        e.span,
                    );
                }
                Ok(vec![Stmt::Expr { expr: self.elaborate_expr(e)?, span }])
            }

            ast::Stmt::Return(opt) => {
                let value = match opt {
                    Some(e) => {
                        // D4 position 4 (return of a live tainted place).
                        self.reject_if_tainted_live_place(&e.node, e.span, "return")?;
                        // §10.3 capture at the return slot, ground-truthed
                        // against production (probes 2026-07-06):
                        //  * non-throws fn, declared ret Result[_,_], value is
                        //    a throws-call → CAPTURE the full Result (the
                        //    callee-T-itself-Result variant miscompiles in
                        //    production and is rejected loudly by the shared
                        //    guard in `maybe_wrap_throws_call`);
                        //  * throws fn with declared T = Result[_,_]: a throws-
                        //    callee whose OWN declared T is NOT Result captures
                        //    (→ `Ok(<callee's full Result>)`); one whose T IS
                        //    Result keeps auto-prop (peel outer, re-wrap) — the
                        //    inner Result is exactly T.
                        if self.current_fn_ret_is_result {
                            if let Some(callee) = self.root_throws_callee(&e.node) {
                                if !self.current_fn_throws || !self.callee_ret_is_result(&callee)
                                {
                                    self.capture_ctx = CaptureCtx::TypedDest;
                                }
                            }
                        }
                        // Round XXVIII Track C — return value is D32 RESTING /
                        // consuming-boundary; `return !x` legit-moves x at the
                        // return boundary. Use owning-context elaboration so
                        // the `Expr::Move` arm ALLOWs a direct top-level `!x`
                        // (and compound-shape tails via match arm-body
                        // propagation). Nested `!x` inside a binop or scrutinee
                        // still rejects because those recursions reset the flag.
                        let inner = self.elaborate_expr_direct_move_owning(e)?;
                        self.capture_ctx = CaptureCtx::None;
                        // A `throws` fn returns `Result[T, E]`: wrap the value in
                        // `Ok(...)`. A throws-call inside `e` already auto-
                        // propagated (or captured) to the declared `T`, which
                        // this re-wraps once.
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
                // Round XXVIII Track C — throw value is D32 RESTING /
                // consuming-boundary (per scout: handler consumes at boundary).
                // Filed as owner sub-question; default ALLOW.
                let inner = self.elaborate_expr_direct_move_owning(e)?;
                Ok(vec![Stmt::Return { value: Some(error_wrap(inner)), span }])
            }

            ast::Stmt::With { bindings, body } => self.desugar_with(bindings, body, span),

            // `break <value>` no longer reaches elaboration: the parser
            // rejects it (D19 -- loops are not expressions).
            ast::Stmt::Break => Ok(vec![Stmt::Break { span }]),
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

            // `assert cond` / `assert cond, msg` — Traps `T_AssertFailed` at eval
            // when the condition is false (RFC trap registry; §10.9).
            ast::Stmt::Assert { condition, message } => {
                let cond = self.elaborate_expr(condition)?;
                let message = match message {
                    Some(m) => Some(self.elaborate_expr(m)?),
                    None => None,
                };
                Ok(vec![Stmt::Assert { cond, message, span }])
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
        if let ast::Expr::Range { start, end, inclusive, .. } = &iterable.node {
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
            ast::Expr::Move { expr } => self.move_source(expr),
            ast::Expr::MutableBorrow { expr } => Ok(Source::WriteThrough(self.elaborate_expr(expr)?)),
            _ if is_clone_call(&value.node) => Ok(Source::Value(self.elaborate_expr(value)?)),
            _ if ast_is_place(&value.node) => {
                self.reject_if_tainted_live_place(&value.node, value.span, "bind")?;
                self.reject_if_single_owner_callable_init(&value.node, value.span, "bind", true)?;
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
            ast::Expr::Move { expr } => self.move_source(expr),
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
            ast::Ownership::Move => self.move_source(&arg.value),
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

    /// D10(b) place-overlap check, mirroring production `check_call_aliasing`
    /// (src/semantic/safety/helpers.rs). Within a single call, two PLACE args
    /// whose roots+projection paths overlap under conflicting sigils are
    /// rejected at elaboration. A Copy-typed bare read is a value SNAPSHOT (no
    /// live alias) and is exempt. Two axes are deliberately kept OUT and handled
    /// by the interpreter's liveness rule (the `Moved`→IllFormed slot):
    /// `(Move, Move)` overlap and `f(!x, x.copy_field)` (move-then-Copy-read) —
    /// both surface as `read of moved-out value` IllFormed at eval, matching
    /// production's E_DoubleMove / E_UseAfterMove (a LIVENESS reject, one layer
    /// before place-overlap).
    fn check_arg_place_overlap(&self, args: &[Spanned<ast::CallArg>]) -> ElabResult<()> {
        struct P {
            root: String,
            path: Vec<String>,
            own: ast::Ownership,
            is_copy: bool,
            span: Span,
        }
        let mut places: Vec<P> = Vec::new();
        for a in args {
            let Some((root, path)) = ast_place(&a.node.value.node) else {
                continue;
            };
            // Only actual LOCAL bindings (params + var decls + for-vars +
            // match-bindings) and `self` are tracked places — mirrors
            // production's `DefKind::Variable` root filter.
            if root != "self" && !self.local_names.contains(&root) {
                continue;
            }
            // Copy-ness of the arg VALUE via the TYPED axis (Rider 2 — NO
            // name/shape heuristic): scalars, tuples-of-Copy, and all-scalar
            // user struct/enum values are Copy (value snapshots); Str /
            // collections / Option / Result / callables own/alias and are NOT.
            let is_copy = self.ty_is_copy(&self.infer_ast_ty(&a.node.value.node));
            places.push(P {
                root,
                path,
                own: a.node.ownership,
                is_copy,
                span: a.node.value.span,
            });
        }
        for i in 0..places.len() {
            for j in (i + 1)..places.len() {
                let (x, y) = (&places[i], &places[j]);
                if x.root != y.root || !paths_overlap(&x.path, &y.path) {
                    continue;
                }
                // Drop Copy bare readers (value snapshots).
                let x_copy_reader = x.own == ast::Ownership::Borrow && x.is_copy;
                let y_copy_reader = y.own == ast::Ownership::Borrow && y.is_copy;
                if x_copy_reader || y_copy_reader {
                    continue;
                }
                // `(Move, Move)` overlap is a liveness concern (IllFormed at
                // eval), not place-overlap.
                if x.own == ast::Ownership::Move && y.own == ast::Ownership::Move {
                    continue;
                }
                let has_writer_or_mover =
                    matches!(x.own, ast::Ownership::MutableBorrow | ast::Ownership::Move)
                        || matches!(y.own, ast::Ownership::MutableBorrow | ast::Ownership::Move);
                let both_bare =
                    x.own == ast::Ownership::Borrow && y.own == ast::Ownership::Borrow;
                if !has_writer_or_mover || both_bare {
                    continue;
                }
                return Err(place_overlap_error(
                    &sigil_place(x.own, &render_place(&x.root, &x.path)),
                    &sigil_place(y.own, &render_place(&y.root, &y.path)),
                    y.span,
                ));
            }
        }
        Ok(())
    }

    /// Classify a `!expr` move source. `!place` moves OUT of the place (eval
    /// kills the slot). `!temp` (a fresh ctor / call / literal — not a place) is
    /// already an owned rvalue: the `!` marks the already-happening consume, so
    /// there is no place to kill — elaborate it as a plain owned `Value`. D31
    /// ADDENDUM-2 (full strict) requires the `!` at the call site even for a
    /// temporary (`f(!Tok(1))`); production treats it identically (a value move,
    /// not a place move). Without this, eval's `Source::Move` would `eval_place`
    /// the temp and `IllFormed` ("expression is not a place").
    fn move_source(&mut self, value: &Spanned<ast::Expr>) -> ElabResult<Source> {
        if ast_is_place(&value.node) {
            Ok(Source::Move(self.elaborate_expr(value)?))
        } else {
            Ok(Source::Value(self.elaborate_expr(value)?))
        }
    }

    fn call_arg_source(&mut self, arg: &ast::CallArg, _param_mode: Option<Mode>) -> ElabResult<Source> {
        match arg.ownership {
            ast::Ownership::Move => self.move_source(&arg.value),
            ast::Ownership::MutableBorrow => {
                // 2T FORMATION position (materialize-on-write, wave-2): a
                // `&`-arg whose ROOT is a bare BORROW binding of a tainted type
                // materializes a private copy of the ROOT at the `&`-formation —
                // for a drop-tainted root that is a hidden clone → the drop
                // side-effect runs twice (double-close). Covers `&s.field`,
                // `&arr[i]`, whole `&self`, AND whole `&p` on a bare param (all
                // MEASURED double-closing pre-fix, in ggdef AND production).
                // Reject, mirroring production's `reject_tainted_formation_arg`.
                // Only `&*deref` is excluded (write-through to the heap pointee,
                // no root materialize). `reject_materialize_on_write` roots via
                // `root_local_name` and gates on BORROW-mode + tainted — so a
                // WriteThrough (`&`) or Owned root is a no-op, exactly as the
                // typed `is_param + Borrow + tainted` gate does in production.
                if !matches!(&arg.value.node, ast::Expr::Deref { .. }) {
                    self.reject_materialize_on_write(&arg.value.node, arg.value.span)?;
                }
                Ok(Source::WriteThrough(self.elaborate_expr(&arg.value)?))
            }
            ast::Ownership::Borrow => {
                // D31 (`&`-direction): the former "unbroken `&`-chain" leniency
                // — a bare place arg into a `&` (WriteThrough) param writing
                // through silently — is RETIRED. `free_fn_sigil_check` now
                // rejects that bare-into-`&` mismatch at BOTH free-fn and method
                // sites before this point, so a bare arg here is always a read
                // borrow (never a `&`-param write-through). A genuine `&` arg
                // carries the sigil and takes the `MutableBorrow` arm above.
                if ast_is_place(&arg.value.node) {
                    // D10(b) Copy-snapshot: a bare read of a COPY-typed place is
                    // a VALUE SNAPSHOT evaluated eagerly at the call site into an
                    // independent value — NOT a lazy view. This matches
                    // production passing Copy scalars by value, and it makes the
                    // rule evaluation-order-sensitive: `f(s.copy_field, !s)`
                    // (read BEFORE move) is legal — the snapshot is taken before
                    // the move kills the slot. A non-Copy bare read stays a live
                    // `BorrowView` (the CoW-default borrow). Copy spans scalars,
                    // tuples-of-Copy, and all-scalar structs/enums (ty_is_copy,
                    // mirroring production is_copy_type).
                    if self.ty_is_copy(&self.infer_ast_ty(&arg.value.node)) {
                        Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                    } else {
                        Ok(Source::BorrowView(self.elaborate_expr(&arg.value)?))
                    }
                } else {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                }
            }
        }
    }

    // ── Expressions ────────────────────────────────────────────────────────

    /// Round XXVIII Track C helper — elaborate an expression at a RESTING /
    /// consuming-boundary position when the RHS is a DIRECT top-level
    /// `Expr::Move` (mirrors Rust safety `check_stmt`'s direct-Move suppress
    /// bracket at VarDecl/Assign/Return/Throw/Send). Used at Stmt::Return
    /// value, Stmt::Throw value, closure body tail. For a NON-direct RHS
    /// (compound: match/if/binop enclosing the Move), the flag stays false
    /// and any nested `!` in operand position rejects — matching Rust's
    /// behavior (verified: `return match c: case: !y` rejects on Rust).
    fn elaborate_expr_direct_move_owning(&mut self, expr: &Spanned<ast::Expr>) -> ElabResult<Expr> {
        if matches!(&expr.node, ast::Expr::Move { .. }) {
            let prev = self.in_owning_context;
            self.in_owning_context = true;
            let r = self.elaborate_expr(expr);
            self.in_owning_context = prev;
            r
        } else {
            self.elaborate_expr(expr)
        }
    }

    fn elaborate_expr(&mut self, expr: &Spanned<ast::Expr>) -> ElabResult<Expr> {
        let span = expr.span;
        // Round XXVIII Track C — the `in_owning_context` flag PROPAGATES
        // through recursive `elaborate_expr` calls unchanged (mirrors Rust's
        // `suppress_move_in_operand_position` design). Owning-context
        // callers bracket it TRUE around their call: Stmt::Return / Stmt::Throw
        // / closure body tail use `elaborate_expr_direct_move_owning` (direct
        // top-level `Expr::Move` only, matching Rust check_stmt suppress
        // semantics); container-literal walker arms
        // (ArrayLiteral/TupleLiteral/DictLiteral/StructLiteral) set the flag
        // TRUE for the WHOLE element walk (walker-driven blanket suppress,
        // matching Rust R1 pass-2). The `Expr::Move` arm below rejects when
        // the flag is false at Move-arm entry. Enum-init lowers through
        // `owning_source_from_arg` which peels `Move` at the ownership sigil
        // — no arm here needed.
        // `dest_ty_hint` scope rule: the hint applies ONLY when the destination's
        // value IS the literal directly. Any other node between the destination
        // and a literal (a call whose ARG is a literal — the p5 leak —, a binary
        // op, a method call, …) kills it, so a `Set[T]` annotation can never
        // dedupe a literal nested in an unrelated position. `Closure` joins the
        // whitelist so `elaborate_closure` can read the destination's
        // `Ty::Callable { consuming }` and reject an `is_move` mismatch (the
        // production `E_ClosureKindMismatch` shape); the closure arm `.take()`s
        // the hint before recursing into its own body.
        if !matches!(&expr.node, ast::Expr::ArrayLiteral(..) | ast::Expr::Closure { .. }) {
            self.dest_ty_hint = None;
        }
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
                if let ast::Expr::Range { start, end, inclusive, .. } = &index.node {
                    let object = Box::new(self.elaborate_expr(object)?);
                    let start = self.opt_expr(start.as_deref())?;
                    let end = self.opt_expr(end.as_deref())?;
                    Ok(Expr::Slice { object, start, end, inclusive: *inclusive })
                } else {
                    // Round XXIX Track A (Core #9 lane parity with Rust
                    // `E_NotIndexable`): reject `s[i]` on a `Set` (D38 — no
                    // lookup key) and `t[i]` on a `Tuple` (D39 — dot-N is
                    // the accessor). The reject fires at ELABORATE (user
                    // AST), NOT at eval, because `desugar_for` synthesizes
                    // an `Expr::Index(coll, __i)` for `for x in someSet:`
                    // (this file :1319-1323): at eval the AST distinction
                    // between user `s[i]` and desugared `s[__i]` is LOST
                    // (both are `Expr::Index(_, _)`) and rejecting at eval
                    // would break every `for x in Set:`. See Core #15(e)
                    // Q4 — "rule subject": the subject is USER `[]`, not
                    // any `Expr::Index` node in the elaborated tree.
                    match self.infer_ast_ty(&object.node) {
                        Ty::Set(_) => {
                            return Err(ElabError::new(
                                "type `Set` is not indexable — `[]` requires an \
                                 `Index[K,V]` implementation. Iterate with \
                                 `for x in c:` or use the collection's ratified \
                                 named accessors (see D38/D39).",
                                span,
                            ));
                        }
                        Ty::Tuple(_) => {
                            return Err(ElabError::new(
                                "type `Tuple` is not indexable — use dot-N \
                                 field access (`t.0`, `t.1`, ...) — the `[]` \
                                 operator has no meaning on tuples (D39).",
                                span,
                            ));
                        }
                        _ => {}
                    }
                    Ok(Expr::Index(
                        Box::new(self.elaborate_expr(object)?),
                        Box::new(self.elaborate_expr(index)?),
                    ))
                }
            }

            ast::Expr::BinaryOp { left, op, right } => {
                let mapped = map_binop(*op, span)?;
                // D26 (Round XXXIII Batch C1): a fallible-arith BinOp evaluates
                // to `Result[T, ArithError]`. In a propagating context — the
                // enclosing fn is `throws` and the destination is NOT a Result
                // capture — wrap in `Propagate` so an `Error` unwinds via the
                // existing throws-return path (mirrors `f()!` propagation). At a
                // Result-capture destination the raw Result flows through unchanged.
                let dest_is_result = matches!(&self.dest_ty_hint, Some(Ty::Result(_, _)));
                let elab_left = self.elaborate_expr(left)?;
                let elab_right = self.elaborate_expr(right)?;
                let inner = Expr::Binary(mapped, Box::new(elab_left), Box::new(elab_right));
                if mapped.is_fallible_arith() && self.current_fn_throws && !dest_is_result {
                    Ok(Expr::Propagate(Box::new(inner)))
                } else {
                    Ok(inner)
                }
            }
            ast::Expr::UnaryOp { op, operand } => {
                Ok(Expr::Unary(map_unop(*op, span)?, Box::new(self.elaborate_expr(operand)?)))
            }

            ast::Expr::Move { expr } => {
                // Round XXVIII Track C — D32:1278-1281 `!`-in-operand-position
                // reject (ggdef lane; sibling of Rust
                // `E_MoveInOperandPosition` and SH `DkMoveInOperandPosition`).
                // A Move reaching this arm is legit only when
                // `in_owning_context` is true: bracketed TRUE by Stmt::Return /
                // Stmt::Throw / closure body tail (direct top-level Move only,
                // via `elaborate_expr_direct_move_owning`) and by container-
                // literal walker arms (ArrayLiteral/TupleLiteral/DictLiteral/
                // StructLiteral element walks, blanket for the whole subtree).
                // Legit `!` in owning positions that PEEL Move (bind_source /
                // owning_source_from_expr / call_arg_source) never reach this
                // arm at all. A Move here with the flag false is the silent-
                // inert operand-`!` class D32 rejects.
                if !self.in_owning_context {
                    return Err(ElabError::new(
                        "`!` in an operand (read) position is not valid (D32:1278-1281) — \
                         the sigil consumes the source at an ownership boundary and there is \
                         no boundary here. Drop the `!` and read the place directly \
                         (`s + \"b\"`, `if b:`, `match x:`, `v[i]`); use `!` only at a \
                         boundary (RHS of a bind/assign/return/throw/send, call argument \
                         `f(!x)`, container element `[!x]`, iterable `for x in !coll:`)",
                        span,
                    ));
                }
                // Owning context: elaborate the inner. Faithful move-kill is
                // applied only at binding / owning positions, which route
                // through the `Source` helpers.
                self.elaborate_expr(expr)
            }

            // D29 `call()!`: the postfix mark ACTIVATES the error channel. Wrap
            // the elaborated inner call in the GGC `Propagate` node — `Ok(x)`
            // peels to `x`, `Error(e)` early-returns via `Halt::Propagate` (the
            // `?`-operator semantics eval already implements). The mark is NOT a
            // capture (capture is the UNMARKED annotated-dest form); a marked
            // call reaching a capture/scrutinee position is the redundant-mark /
            // peeled-arms reject, recorded at those positions.
            ast::Expr::Propagate { expr } => {
                // Signal the DIRECT inner call that it carries the mark (so its
                // `maybe_wrap` peels+activates instead of recording the bare-mark
                // reject). Save/restore so a nested marked call in an arg
                // (`g(f()!)!`) leaves the outer mark intact for `g`.
                let saved = self.fallible_marked;
                self.fallible_marked = true;
                let inner = self.elaborate_expr(expr)?;
                self.fallible_marked = saved;
                Ok(Expr::Propagate(Box::new(inner)))
            }

            ast::Expr::Call { callee, generic_args, args } => {
                self.elaborate_call(callee, generic_args.is_some(), args, span)
            }

            ast::Expr::MethodCall { receiver, method, args, .. } => {
                self.elaborate_method(receiver, &method.node, args, span)
            }

            ast::Expr::ArrayLiteral(elems, _) => {
                // `{a, b, c}` and `[a, b, c]` are the SAME node; a `Set[T]`
                // destination makes this a set literal (dedup on build), else a
                // vector — matching production, which dedupes BOTH spellings
                // into a Set destination (probe p12). The hint's ELEMENT type
                // re-arms the hint per element, so a nested literal element
                // (`Vector[Set[int]] vs = [{1,1,2}]`) builds by ITS declared
                // element type; a non-literal element clears it on entry.
                let (kind, elem_hint) = match self.dest_ty_hint.take() {
                    Some(Ty::Set(el)) => (ConstructKind::Set, Some(*el)),
                    Some(Ty::Vector(el)) => (ConstructKind::Vector, Some(*el)),
                    _ => (ConstructKind::Vector, None),
                };
                // Round XXVIII Track C R1 — container-literal ELEMENTS are D32
                // consuming init boundaries; suppress `!`-in-operand rejection
                // for the WHOLE element walk (walker-driven blanket, matching
                // Rust `check_expr::Expr::ArrayLiteral` bracket).
                let prev_owning = self.in_owning_context;
                self.in_owning_context = true;
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    self.dest_ty_hint = elem_hint.clone();
                    out.push(self.owning_source_from_expr(e)?);
                }
                self.in_owning_context = prev_owning;
                self.dest_ty_hint = None;
                Ok(Expr::Construct { kind, args: out })
            }
            ast::Expr::TupleLiteral(elems) => {
                // R1: same as ArrayLiteral — tuple elements are consuming init
                // boundaries; blanket-suppress operand-`!` rejection.
                let prev_owning = self.in_owning_context;
                self.in_owning_context = true;
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.owning_source_from_expr(e)?);
                }
                self.in_owning_context = prev_owning;
                Ok(Expr::Construct { kind: ConstructKind::Tuple, args: out })
            }
            ast::Expr::StructLiteral { name, args, .. } => {
                // R1: struct-literal args are consuming init boundaries;
                // blanket-suppress operand-`!` rejection.
                let prev_owning = self.in_owning_context;
                self.in_owning_context = true;
                let mut out = Vec::with_capacity(args.len());
                for e in args {
                    self.reject_if_single_owner_callable_init(&e.node, e.span, "ctor-init", false)?;
                    out.push(self.owning_source_from_expr(e)?);
                }
                self.in_owning_context = prev_owning;
                Ok(Expr::Construct { kind: ConstructKind::Struct(name.node.clone()), args: out })
            }

            ast::Expr::NoneLiteral => Ok(enum_construct("Option", "None", Vec::new())),

            ast::Expr::As { expr, type_ } => {
                let inner = self.elaborate_expr(expr)?;
                let target = cast_target(&type_.node, type_.span)?;
                Ok(Expr::Cast { expr: Box::new(inner), target })
            }

            ast::Expr::Closure { is_move, is_async, params, body } => {
                self.elaborate_closure(*is_move, *is_async, params, body, span)
            }

            ast::Expr::Match { scrutinee, arms, else_arm } => {
                // D29 marked-match peel: `match f()!: case Ok/Error` — the `!`
                // peeled the `Result` to its success `T`, so `Ok`/`Error` arms
                // cannot match (bind the `Result` first). A T-VARIANT marked
                // scrutinee (`match f()!: case UserVariant`) is LEGAL and runs.
                // A BARE throws-call scrutinee (`match f():`) is a bare call —
                // rejected at the scrutinee's `maybe_wrap` (Scrutinee capture is
                // retired). (See `elaborate_match_stmt` for the statement twin.)
                let has_result_arm =
                    arms.iter().any(|a| pattern_consumes_result(&a.pattern.node));
                self.reject_marked_match_result_arms(scrutinee, has_result_arm);
                // Round XXVIII Track C — SCRUTINEE is OPERAND: reset the
                // owning flag around the scrutinee walk so `match !x:` rejects
                // (matches Rust: brief NEG fixture `sound_move_operand_matchsc_
                // error.gg`). Arm bodies + else are elaborated with the flag
                // in its current state — for a boundary-enclosed match this
                // does NOT propagate to arm bodies (matches Rust behavior:
                // `return match c: case: !y` rejects the arm-body `!y`).
                let prev_owning_scrut = self.in_owning_context;
                self.in_owning_context = false;
                let scrut = self.elaborate_expr(scrutinee)?;
                self.in_owning_context = prev_owning_scrut;
                self.capture_ctx = CaptureCtx::None;
                let mut ggc_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    if arm.guard.is_some() {
                        return Err(ElabError::new("match guards are outside the phase-0 subset", arm.span));
                    }
                    // Arm body walked with flag reset (mirrors Rust — a
                    // match arm body is treated as operand for `!` unless the
                    // arm body itself is a direct-Move ROUTED through some
                    // owning bracket at its own site).
                    let prev_owning_body = self.in_owning_context;
                    self.in_owning_context = false;
                    let body_expr = self.elaborate_expr(&arm.body)?;
                    self.in_owning_context = prev_owning_body;
                    ggc_arms.push(ExprArm {
                        pattern: self.elaborate_pattern(&arm.pattern)?,
                        body: body_expr,
                    });
                }
                let else_arm = match else_arm {
                    Some(e) => {
                        let prev_owning_else = self.in_owning_context;
                        self.in_owning_context = false;
                        let r = self.elaborate_expr(e)?;
                        self.in_owning_context = prev_owning_else;
                        Some(Box::new(r))
                    }
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
                        StringSegment::Interpolation(_, Some(_spec)) => {
                            // Round XXV Track E (Class-B close): `{x:b}`,
                            // `{x:#b}`, `{x:.2f}`, etc. — format specs are
                            // ratified surface (docs/language-reference.md
                            // §3171) but the phase-A subset does NOT model
                            // them; silently dropping the spec printed the
                            // wrong value (`fstring_binary_spec_leak`
                            // BOTH-WRONG pre-fix). Reject as a LOUD ElabError,
                            // matching classify.rs invariant #8.
                            return Err(ElabError::new(
                                "f-string format spec is outside the phase-A subset",
                                span,
                            ));
                        }
                        StringSegment::Interpolation(_, None) => {
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
        // D10(b): reject place-overlapping args under conflicting sigils before
        // any callee dispatch (mirrors production `check_call_aliasing`, which
        // runs on every `Expr::Call`).
        self.check_arg_place_overlap(args)?;
        let ast::Expr::Identifier(name) = &callee.node else {
            return Err(ElabError::new("only named callees are supported in phase 0", callee.span));
        };
        // `print(...)` in expression position (e.g. a closure body). In
        // statement position it is lowered to `Stmt::Print` upstream.
        if name == "print" {
            // Round XXV Track E (Class-B close, Core #4 print-dispatch class):
            // `print(x, terminator=..., file=...)` are ratified surface (docs/
            // language-reference.md §3247/3291) — but the phase-A subset does
            // NOT model those kwargs, so silently dropping the name would
            // mis-evaluate (BOTH-WRONG rows `print_builtin` + `print_terminator`
            // pre-fix). Reject as a LOUD ElabError, matching classify.rs
            // invariant #8. Sibling call sites: `as_print_call:3325` (rejects
            // via `args[0].node.name.is_none()` guard → falls through to here);
            // `eval.rs:1072` defensive path — the ElabError here fires BEFORE
            // eval runs, so no runtime observation of a named-arg print.
            self.reject_named_args(args, "print builtin")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node, None)?);
            }
            return Ok(Expr::Call { func: "print".to_string(), args: out });
        }
        // `panic(msg)` — an uncatchable trap (`T_Panic`). Modeled as a dedicated
        // GGC node (not a name-dispatched `Call`) so no name-match leaks into
        // eval's ordinary call path.
        if name == "panic" {
            if args.len() != 1 {
                return Err(ElabError::new("`panic` takes exactly 1 argument (a message)", callee.span));
            }
            self.reject_named_args(args, "panic")?;
            let msg = self.elaborate_expr(&args[0].node.value)?;
            return Ok(Expr::Panic(Box::new(msg)));
        }
        // Prelude enum constructors: `Some(x)`, `None()`, `Ok(v)`, `Error(e)`.
        if let Some(type_name) = prelude_enum_of(name) {
            self.reject_named_args(args, "prelude enum constructor")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                self.reject_if_single_owner_callable_arg(&a.node, a.span, "enum-variant init")?;
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
                self.reject_if_single_owner_callable_arg(&a.node, a.span, "enum-variant init")?;
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
            // Track B3 (D31-uniform): read the callee's declared param sigil
            // modes off its typed `Ty::Callable { param_ownerships }`. When
            // present, enforce the same sigil rule the direct-call path
            // enforces via `free_fn_sigil_check` — the caller's per-arg sigil
            // must EQUAL the declared param mode, else `E_OwnershipMismatch`.
            // When the callable's inner type is not resolvable (`Callable[Unknown]`),
            // `param_ownerships` is empty and the check silently skips.
            let param_modes: Vec<Option<Mode>> = match self.local_ty.get(name) {
                Some(Ty::Callable { param_ownerships, .. }) if !param_ownerships.is_empty() => {
                    param_ownerships.iter().map(|m| Some(*m)).collect()
                }
                _ => (0..args.len()).map(|_| None).collect(),
            };
            let mut out = Vec::with_capacity(args.len());
            for (i, a) in args.iter().enumerate() {
                let pmode = param_modes.get(i).copied().flatten();
                if let Some(pm) = pmode {
                    let found = mode_of(a.node.ownership);
                    if found != pm {
                        let render = |m: Mode| match m {
                            Mode::Borrow => "borrow (bare)",
                            Mode::WriteThrough => "mutable borrow (&)",
                            Mode::Move => "consume (!)",
                        };
                        return Err(ElabError::new(
                            format!(
                                "argument #{i} of indirect call through `{name}` expects {}, \
                                 found {} — the call-site sigil must match the declared param \
                                 at every call site, indirect calls included (production \
                                 E_OwnershipMismatch, D31-uniform, Track B3)",
                                render(pm),
                                render(found)
                            ),
                            a.span,
                        ));
                    }
                }
                out.push(self.call_arg_source(&a.node, pmode)?);
            }
            // Single-owner `ConsumeCallable`: the call consumes the callee (D5
            // kind axis). Read the typed callable classification resolved at
            // `ty_of_type` — never the surface name.
            let consumes_callee =
                matches!(self.local_ty.get(name), Some(Ty::Callable { consuming: true, .. }));
            return Ok(Expr::CallValue {
                callee: Box::new(Expr::Local(name.clone())),
                args: out,
                consumes_callee,
            });
        }
        // Ordinary function call — (B2, from B1 output-review R2) named args
        // are REORDERED to the callee's param order via the pass-1 signature
        // registry, replacing the B1-interim rejection.
        if self.func_names.contains(name) {
            // Consume the capture + mark flags for THIS (outermost) call before
            // its args elaborate, so a nested fallible call in an arg still takes
            // its own bare-mark / capture path (the mark/capture binds to the
            // call it directly wraps, not a nested arg call).
            let capture = std::mem::take(&mut self.capture_ctx);
            let marked = std::mem::take(&mut self.fallible_marked);
            let out = self.call_args_reordered(name, args, CallForm::FreeFn)?;
            let call = Expr::Call { func: name.clone(), args: out };
            return self.maybe_wrap_throws_call(call, name, callee.span, capture, marked);
        }
        Err(ElabError::new(
            format!("unresolved callee `{name}` (unknown function/struct/enum; may need Increment B2)"),
            callee.span,
        ))
    }

    /// Apply the throws→Result treatment to a `call` to the GGC function/
    /// method `callee_name` (RFC §2.6 row 1; language-reference §10.1/§10.3).
    /// A call to a NON-throws callee passes through untouched. For a throws
    /// callee, by capture context:
    ///   * `Scrutinee` (a `match … case Ok/Error` consumer) → yield the full
    ///     widened `Result` (the arms destructure it);
    ///   * `TypedDest` (§10.3: a `Result[_,_]`-declared destination) → yield
    ///     the full `Result` (capture) — UNLESS the callee's own declared T is
    ///     itself `Result`, where name-level types cannot tell capture (outer)
    ///     from propagate (inner) apart AND production miscompiles the shape
    ///     (probes k/c2 2026-07-06 print garbage payloads) → LOUD error;
    ///   * `None` inside a `throws` fn → wrap in `Propagate` (`?` semantics:
    ///     `Ok(x)` peels to `x`, `Error(e)` early-returns from the caller);
    ///   * `None` elsewhere → LOUD ElabError. In real Gorget this is a type
    ///     error; ggdef refuses it rather than silently mis-evaluating the
    ///     dropped `throws` effect (the flagship safety bug).
    fn maybe_wrap_throws_call(
        &mut self,
        call: Expr,
        callee_name: &str,
        span: Span,
        capture: CaptureCtx,
        marked: bool,
    ) -> ElabResult<Expr> {
        if !self.fn_throws.contains(callee_name) {
            return Ok(call);
        }
        // D29: the call carries the `!` mark (it is the inner of an
        // `ast::Expr::Propagate`). The `Propagate` arm wraps it in the GGC
        // `Propagate` node — here we just hand the bare call back (no reject, no
        // double-wrap). Mark + `Result`-capture together is the redundant-mark
        // reject, recorded at the capture position (VarDecl), not here.
        if marked {
            return Ok(call);
        }
        match capture {
            // §10.3 capture (the 2026-07-17 amendment): an explicitly
            // `Result[_,_]`-annotated destination captures the UNMARKED fallible
            // call — legal, the annotation carries the visibility. (D29 RETIRED
            // the old callee-T-is-Result "undecidable" guard: propagation now
            // requires the visible `!` mark, so an UNMARKED capture is
            // unambiguously the full widened `Result` — and Rust gg accepts both
            // the full-widened and the peel-typed capture of a Result-T throws
            // callee. The "production miscompiles" rationale was a pre-D23/D29
            // fossil; ground-truthed 2026-07-17 that Rust gg now accepts+runs it.)
            CaptureCtx::TypedDest => Ok(call),
            // D29 (auto-wrap RETIRED): a BARE throws call — no `!` mark, no
            // `Result[_,_]` capture — silently drops the error. This is the
            // `E_MissingFallibleMark` class (the former auto-propagate site is
            // now a reject: propagation requires the visible `f()!` mark). The
            // (`Scrutinee` context is retired too — a bare `match f():`
            // scrutinee is a bare call.) Record the reject (surfaced as
            // `IllFormed` + `E_MissingFallibleMark`) and return the unwrapped
            // call as an inert placeholder — `run` short-circuits before eval,
            // so it is never evaluated.
            CaptureCtx::None => {
                self.record_d29_reject(
                    format!(
                        "this fallible call to `{callee_name}` must be marked with `!` — mark it \
                         to propagate, handle it with `catch`/`rethrow`, or capture it into a \
                         `Result` binding"
                    ),
                    span,
                );
                Ok(call)
            }
        }
    }

    /// If `e`'s ROOT is a call that will dispatch to a `throws` function or
    /// `equip` method, the resolved GGC callee name. Mirrors the dispatch in
    /// `elaborate_call` (Identifier callee in `fn_throws`) and
    /// `elaborate_method` (receiver-type inference + `equip_methods`) exactly,
    /// so the §10.3 capture decision and the actual call resolution can never
    /// disagree. `None` for non-call roots, non-throws callees, ctors, and
    /// closure-value calls (closure signatures are untyped locals).
    fn root_throws_callee(&self, e: &ast::Expr) -> Option<String> {
        match e {
            ast::Expr::Call { callee, .. } => match &callee.node {
                ast::Expr::Identifier(n) if self.fn_throws.contains(n) => Some(n.clone()),
                _ => None,
            },
            ast::Expr::MethodCall { receiver, method, .. } => {
                if let Ty::Named(t) = self.infer_ast_ty(&receiver.node) {
                    if let Some(mi) = self.equip_methods.get(&(t, method.node.clone())) {
                        if self.fn_throws.contains(&mi.mangled) {
                            return Some(mi.mangled.clone());
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Whether the callee's DECLARED (success) return type is itself
    /// `Result[_,_]` — the §10.3 capture-vs-propagate discriminator at return
    /// positions and the ambiguity guard at typed destinations.
    fn callee_ret_is_result(&self, callee_name: &str) -> bool {
        self.fn_ret.get(callee_name).map(ty_is_result).unwrap_or(false)
    }

    /// D29 kind-2: if `e`'s ROOT is a call to a NON-`throws` function whose
    /// DECLARED return is `Result[_,_]`, its GGC callee name. This is the
    /// one-mark-for-both-kinds companion of `root_throws_callee` (kind-1): a
    /// declared-`Result` return makes a call fallible too, so a bare-DISCARD of
    /// its outcome is `E_MissingFallibleMark`. `None` for throws callees (kind-1,
    /// owned by `root_throws_callee`), non-Result returns, ctors, closure-value
    /// calls, and method calls (conservative — the corpus is throws-free).
    fn root_kind2_callee(&self, e: &ast::Expr) -> Option<String> {
        if let ast::Expr::Call { callee, .. } = e {
            if let ast::Expr::Identifier(n) = &callee.node {
                if !self.fn_throws.contains(n) && self.callee_ret_is_result(n) {
                    return Some(n.clone());
                }
            }
        }
        None
    }

    /// D29 marked-match peel reject: a MARKED fallible-call scrutinee
    /// (`match f()!:`, an `ast::Expr::Propagate` over a throws/Result-return
    /// call) whose arms destructure the `Result` (`case Ok/Error`) — the mark
    /// peeled the `Result` to `T`, so those arms cannot match. Records
    /// `E_MissingFallibleMark`. A bare scrutinee is handled at `maybe_wrap`; a
    /// T-variant marked scrutinee (no Result-consuming arm) is legal and runs.
    fn reject_marked_match_result_arms(
        &mut self,
        scrutinee: &Spanned<ast::Expr>,
        has_result_arm: bool,
    ) {
        if !has_result_arm {
            return;
        }
        if let ast::Expr::Propagate { expr } = &scrutinee.node {
            if self.root_throws_callee(&expr.node).is_some()
                || self.root_kind2_callee(&expr.node).is_some()
            {
                self.record_d29_reject(
                    "`match f()!:` peels the `Result` to its success value — `Ok`/`Error` arms \
                     cannot match here; bind the `Result` first (`Result[T, E] r = f()`), then \
                     match `r`",
                    scrutinee.span,
                );
            }
        }
    }

    /// Shared §10.3 decision for one destination slot with declared type `ty`
    /// (`None` = no registry entry), about to receive a value whose ROOT is a
    /// throws-call. `Result[_,_]` → set `TypedDest` (capture). An unresolvable
    /// declared type is a LOUD error (capture-vs-propagate would be a coin
    /// flip). Anything else → leave `None` (§10.1 auto-propagation default).
    /// Call ONLY when `root_throws_callee` matched the incoming value.
    fn set_typed_dest_capture(&mut self, ty: Option<&Ty>, what: &str, span: Span) -> ElabResult<()> {
        match ty {
            Some(t) if ty_is_result(t) => {
                self.capture_ctx = CaptureCtx::TypedDest;
                Ok(())
            }
            Some(Ty::Unknown) => Err(ElabError::new(
                format!(
                    "cannot resolve the declared type of {what} for a `throws`-call argument \
                     (capture-vs-propagate is type-directed, §10.3); bind through an \
                     explicitly-typed local instead"
                ),
                span,
            )),
            _ => Ok(()),
        }
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
            for (i, a) in args.iter().enumerate() {
                // §10.3 capture at a struct-ctor field: a throws-call into a
                // field DECLARED `Result[_,_]` captures (ground-truthed,
                // probe e — production captures at the field init).
                if self.root_throws_callee(&a.node.value.node).is_some() {
                    let fty = self
                        .struct_field_types
                        .get(struct_name)
                        .and_then(|fs| fs.get(i))
                        .map(|(_, t)| t.clone());
                    self.set_typed_dest_capture(
                        fty.as_ref(),
                        &format!("field {i} of `{struct_name}`"),
                        a.span,
                    )?;
                }
                self.reject_if_single_owner_callable_arg(&a.node, a.span, "ctor-init")?;
                out.push(self.owning_source_from_arg(&a.node)?);
                self.capture_ctx = CaptureCtx::None;
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
            // §10.3 capture, named-field form.
            if self.root_throws_callee(&a.node.value.node).is_some() {
                let fty = self
                    .struct_field_types
                    .get(struct_name)
                    .and_then(|fs| fs.iter().find(|(n, _)| n == &name))
                    .map(|(_, t)| t.clone());
                self.set_typed_dest_capture(
                    fty.as_ref(),
                    &format!("field `{name}` of `{struct_name}`"),
                    a.span,
                )?;
            }
            self.reject_if_single_owner_callable_arg(&a.node, a.span, "ctor-init")?;
            by_name.push((name, self.owning_source_from_arg(&a.node)?));
            self.capture_ctx = CaptureCtx::None;
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
    ///
    /// `form` carries production's ASYMMETRIC call-site sigil discipline
    /// (verified: `check_call_ownership` runs on the `Expr::Call` arm ONLY —
    /// `check_expr.rs:315` — never on `Expr::MethodCall`):
    ///   * `FreeFn`: the call-site sigil must MATCH the declared param mode
    ///     exactly (production `E_OwnershipMismatch`) — a bare place into a `&`
    ///     param is a loud static reject, never a silent write-through.
    ///   * `Method`: production never reaches the check; a bare place arg into
    ///     a `&` param binds the alias and WRITES THROUGH (the ratified fixture
    ///     family — `method_mut_borrow_arg`'s `c.add_all(v)` into `&vals`).
    fn call_args_reordered(
        &mut self,
        func_name: &str,
        args: &[Spanned<ast::CallArg>],
        form: CallForm,
    ) -> ElabResult<Vec<Source>> {
        let any_named = args.iter().any(|a| a.node.name.is_some());
        if !any_named {
            let mut out = Vec::with_capacity(args.len());
            for (i, a) in args.iter().enumerate() {
                // §10.3 capture at a call arg: a throws-call into a param
                // DECLARED `Result[_,_]` captures (ground-truthed, probe d).
                if self.root_throws_callee(&a.node.value.node).is_some() {
                    let pty = self.fn_param_tys.get(func_name).and_then(|v| v.get(i)).cloned();
                    self.set_typed_dest_capture(
                        pty.as_ref(),
                        &format!("parameter {i} of `{func_name}`"),
                        a.span,
                    )?;
                }
                let pmode = self.fn_param_modes.get(func_name).and_then(|v| v.get(i)).copied();
                let effective = self.free_fn_sigil_check(func_name, i, pmode, a, form)?;
                out.push(self.call_arg_source(&a.node, effective)?);
                self.capture_ctx = CaptureCtx::None;
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
            // §10.3 capture, named-arg form: the param index is the name's
            // position in the declared order (a missing name errors below).
            if self.root_throws_callee(&a.node.value.node).is_some() {
                if let Some(i) = order.iter().position(|p| p == &name) {
                    let pty = self.fn_param_tys.get(func_name).and_then(|v| v.get(i)).cloned();
                    self.set_typed_dest_capture(
                        pty.as_ref(),
                        &format!("parameter `{name}` of `{func_name}`"),
                        a.span,
                    )?;
                }
            }
            let pidx = order.iter().position(|p| p == &name);
            let pmode = pidx
                .and_then(|i| self.fn_param_modes.get(func_name).and_then(|v| v.get(i)).copied());
            let effective =
                self.free_fn_sigil_check(func_name, pidx.unwrap_or(usize::MAX), pmode, a, form)?;
            by_name.push((name, self.call_arg_source(&a.node, effective)?));
            self.capture_ctx = CaptureCtx::None;
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

    /// Production's call-site sigil rule (`check_call_ownership` +
    /// `check_method_call_ownership`, `src/semantic/safety/helpers.rs`): the
    /// arg's sigil must EQUAL the declared param mode, else `E_OwnershipMismatch`.
    /// D31 ADDENDUM-2 (2026-07-20) is FULL STRICT — the rule is identical for
    /// free-fn and method calls, named place or temporary (bare = borrow,
    /// `&` = write-through, `!` = consume). `CallForm` no longer distinguishes
    /// behavior (the former "unbroken `&`-chain" method write-through leniency
    /// is retired); it is retained only to key the diagnostic wording. Returns
    /// `None` — the explicit-sigil arms of `call_arg_source` classify every
    /// accepted arg, so no effective mode needs threading.
    fn free_fn_sigil_check(
        &self,
        func_name: &str,
        param_idx: usize,
        pmode: Option<Mode>,
        arg: &Spanned<ast::CallArg>,
        form: CallForm,
    ) -> ElabResult<Option<Mode>> {
        let render = |m: Mode| match m {
            Mode::Borrow => "borrow (bare)",
            Mode::WriteThrough => "mutable borrow (&)",
            Mode::Move => "consume (!)",
        };
        let pname = || {
            self.fn_param_names
                .get(func_name)
                .and_then(|v| v.get(param_idx))
                .cloned()
                .unwrap_or_else(|| format!("#{param_idx}"))
        };
        // FULL STRICT (both forms). `form` only tunes the message tail.
        let site = match form {
            CallForm::Method => "the call-site sigil must match the declared param at every \
                                 call site, method calls included",
            CallForm::FreeFn => "the call-site sigil must match the declared param at a \
                                 function call",
        };
        if let Some(pm) = pmode {
            let found = mode_of(arg.node.ownership);
            if found != pm {
                return Err(ElabError::new(
                    format!(
                        "argument for parameter `{}` of `{func_name}` expects {}, found {} \
                         — {site} (production E_OwnershipMismatch)",
                        pname(),
                        render(pm),
                        render(found)
                    ),
                    arg.span,
                ));
            }
        }
        Ok(None)
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

    /// Round XXIV Track D — mirror of `src/semantic/typecheck.rs:7990-8050`
    /// `unify_closure_ret_axis`. Picks the payload axis per cell; emits
    /// `error[E_TypeMismatch]` on mismatch. Non-Option/Result closure returns,
    /// `Ty::Unknown` (unresolved) payloads, and same-payload cases all no-op
    /// — they either can't cause the cross-type SBO or will surface via a
    /// separate diagnostic upstream.
    ///
    /// **Known precision gap:** ggdef's `Ty::Prim` collapses int/bool/float/
    /// unsigned to a single tag, so a bool-vs-int mismatch in the same axis
    /// slips through this check. This is a shared elaborator limit (all `Ty`
    /// consumers are Prim-blind), orthogonal to the `ClosureCombinatorCell`
    /// class. The three α NEG fixtures contrast struct-vs-Prim (Money vs int),
    /// which `Ty::Named` distinguishes cleanly.
    fn unify_closure_ret_axis(
        &self,
        cell: ClosureCombinatorCell,
        receiver_type: &Ty,
        closure_ret_type: &Ty,
        span: Span,
    ) -> ElabResult<()> {
        // Extract the closure's payload types (must be a concrete Option/Result
        // to unify against). A bare `Ty::Unknown` means the closure's return
        // couldn't be inferred (free-form callee, unregistered fn, etc.); the
        // ill-typed shape would surface as a distinct diagnostic elsewhere, so
        // no-op here rather than cascade a TypeMismatch on top.
        let (closure_ok, closure_err) = match closure_ret_type {
            Ty::Option(t) => (Some((**t).clone()), None),
            Ty::Result(ok, err) => (Some((**ok).clone()), Some((**err).clone())),
            _ => return Ok(()),
        };
        let (recv_ok, recv_err) = match receiver_type {
            Ty::Option(t) => (Some((**t).clone()), None),
            Ty::Result(ok, err) => (Some((**ok).clone()), Some((**err).clone())),
            _ => return Ok(()),
        };
        // Pick the axis per cell (mirrors production's indexed lookup):
        //   ResultOrElse: closure's Ok must match receiver's Ok (recovery is Error-axis).
        //   ResultAndThen: closure's Err must match receiver's Err (map is Ok-axis).
        //   OptionOrElse: closure's Some must match receiver's Some (single payload).
        let (recv_payload, closure_payload) = match cell {
            ClosureCombinatorCell::ResultOrElse => (recv_ok, closure_ok),
            ClosureCombinatorCell::ResultAndThen => (recv_err, closure_err),
            ClosureCombinatorCell::OptionOrElse => (recv_ok, closure_ok),
        };
        let (Some(recv_payload), Some(closure_payload)) = (recv_payload, closure_payload) else {
            return Ok(());
        };
        // Bail on `Unknown` — the mirror of production's `is_fully_concrete`
        // gate. A later inference pass (or an upstream error) will resolve or
        // report the miss.
        if matches!(recv_payload, Ty::Unknown) || matches!(closure_payload, Ty::Unknown) {
            return Ok(());
        }
        if recv_payload == closure_payload {
            return Ok(());
        }
        Err(ElabError::new(
            format!(
                "error[E_TypeMismatch]: type mismatch: expected `{}`, found `{}`",
                ty_display(receiver_type),
                ty_display(closure_ret_type),
            ),
            span,
        ))
    }

    /// Round XXIV Track D — classify the axis-unify cell from `(builtin_method,
    /// receiver_type)`. Returns `None` when the pair is not one of the 3
    /// unify-eligible closure-returning combinators — mirrors production's
    /// per-method arms in `infer_closure_method_type` collapsed into a single
    /// mapping (ggdef consolidates the arms via `elaborate_method`'s single
    /// match).
    fn combinator_cell(
        bm: BuiltinMethod,
        receiver_type: &Ty,
    ) -> Option<ClosureCombinatorCell> {
        match (bm, receiver_type) {
            (BuiltinMethod::OrElse, Ty::Result(_, _)) => Some(ClosureCombinatorCell::ResultOrElse),
            (BuiltinMethod::OrElse, Ty::Option(_)) => Some(ClosureCombinatorCell::OptionOrElse),
            (BuiltinMethod::AndThen, Ty::Result(_, _)) => Some(ClosureCombinatorCell::ResultAndThen),
            _ => None,
        }
    }

    /// Round XXIV Track D — infer a closure's return type from its AST body,
    /// as needed by `unify_closure_ret_axis`. `infer_ast_ty` has no
    /// `Expr::Closure` arm, so we descend to the body and infer that. Returns
    /// `Ty::Unknown` on shapes the inferrer can't classify — the caller then
    /// no-ops (never cascade a false TypeMismatch on top of an inference miss).
    fn infer_closure_arg_ret_ty(&self, arg: &ast::CallArg) -> Ty {
        match &arg.value.node {
            ast::Expr::Closure { body, .. } => self.infer_ast_ty(&body.node),
            _ => Ty::Unknown,
        }
    }

    fn elaborate_method(
        &mut self,
        receiver: &Spanned<ast::Expr>,
        method: &str,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        // D10(b): place-overlap check on the method args (mirrors production
        // `check_call_aliasing`, which runs on every `Expr::MethodCall`). The
        // receiver is not in scope of this check (it binds as `self`), matching
        // production.
        self.check_arg_place_overlap(args)?;
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
                    self.reject_if_single_owner_callable_arg(&a.node, a.span, "enum-variant init")?;
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

        // Round XXVIII Track A ggdef LAG close: tag-checks (is_ok/is_some/
        // is_none/is_error) fall through to the arm-picker's `other =>`
        // catch-all at :2664 with the generic "outside phase-0 subset"
        // message. Rust + SH both reject wrong-cell tag-checks with the
        // more informative `error[E_NoMethodFound]:` (R26A+R27C 9-arm
        // chokepoints). This gate intercepts BEFORE the catch-all so the
        // WRONG-cell case (Result.is_some/is_none Option-only, Option.is_ok/
        // is_error Result-only) rejects with the same `error[E_NoMethodFound]:`
        // shape all 3 lanes agree on. Right-cell (Result.is_ok / Option.is_some)
        // remains outside phase-0 subset and reaches the catch-all — its
        // subset-expansion is separate work (needs BuiltinMethod::IsOk etc.
        // variants + eval-side impls).
        let receiver_ty_for_tagcheck = self.infer_ast_ty(&receiver.node);
        match (method, &receiver_ty_for_tagcheck) {
            ("is_some" | "is_none", Ty::Result(_, _)) => {
                return Err(ElabError::new(
                    format!(
                        "error[E_NoMethodFound]: `.{method}()` on Result is outside the phase-0 subset (Option-only)"
                    ),
                    span,
                ));
            }
            ("is_ok" | "is_error", Ty::Option(_)) => {
                return Err(ElabError::new(
                    format!(
                        "error[E_NoMethodFound]: `.{method}()` on Option is outside the phase-0 subset (Result-only)"
                    ),
                    span,
                ));
            }
            _ => {}
        }

        // `(method, expected-arg-count)` for the fixed-arity builtins.
        let (bm, argn): (BuiltinMethod, Option<usize>) = match method {
            "push" => (BuiltinMethod::Push, Some(1)),
            "set" | "put" => (BuiltinMethod::Set, Some(2)),
            "len" => (BuiltinMethod::Len, Some(0)),
            "get" => (BuiltinMethod::Get, Some(1)),
            "unwrap" => (BuiltinMethod::Unwrap, Some(0)),
            "unwrap_error" => (BuiltinMethod::UnwrapError, Some(0)),
            "unwrap_or" => (BuiltinMethod::UnwrapOr, Some(1)),
            "pop" => (BuiltinMethod::Pop, Some(0)),
            "clear" => (BuiltinMethod::Clear, Some(0)),
            "fill" => (BuiltinMethod::Fill, Some(2)),
            "add" => (BuiltinMethod::Add, Some(1)),
            "trim" => (BuiltinMethod::Trim, Some(0)),
            "substring" => (BuiltinMethod::Substring, Some(2)),
            // Option/Result combinators (Increment B3). All take exactly one
            // closure arg. READ-ONLY on the receiver, so no
            // `reject_materialize_on_write` (unlike push/set/pop/…).
            "map" => (BuiltinMethod::Map, Some(1)),
            "filter" => (BuiltinMethod::Filter, Some(1)),
            "or_else" => (BuiltinMethod::OrElse, Some(1)),
            "and_then" => (BuiltinMethod::AndThen, Some(1)),
            "flat_map" => (BuiltinMethod::FlatMap, Some(1)),
            "unwrap_or_else" => (BuiltinMethod::UnwrapOrElse, Some(1)),
            "map_err" => (BuiltinMethod::MapErr, Some(1)),
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
        let receiver_type = self.infer_ast_ty(&receiver.node);
        // Round XXV Track B — one-sided combinators on the wrong-shape
        // receiver: {FlatMap, Filter} on Result; {MapErr, UnwrapError} on
        // Option. All 4 are ratified Option-only or Result-only per
        // `docs/language-reference.md:3861-3891` (surface method tables)
        // and mirrored by `src/ir/lowering/builtins.rs:915-939` +
        // `:1425-1429` (RESULT protocol has no `flat_map`/`filter`/
        // `flatten`; MAP_ERR/UNWRAP_ERROR are Result-only). ggdef must
        // reject at elaborate (Rust production silently accepts and
        // crashes at C-compile with `incompatible types`; ggdef's eval-side
        // reject fires too late for corpus_b CheckFails adjudication).
        // Result.flatten reaches this path via the `other =>` catch-all
        // in the arm-picker (no BuiltinMethod::Flatten variant exists),
        // so no arm here. Core #4 class-fix at the elaborate chokepoint;
        // Rust-side class-fix is owed follow-up (TODO Round XXV Track B).
        match (bm, &receiver_type) {
            (BuiltinMethod::FlatMap, Ty::Result(_, _))
            | (BuiltinMethod::Filter, Ty::Result(_, _)) => {
                return Err(ElabError::new(
                    format!(
                        "error[E_NoMethodFound]: `.{method}()` on Result is outside the phase-0 subset (Option-only)"
                    ),
                    span,
                ));
            }
            (BuiltinMethod::MapErr, Ty::Option(_)) => {
                return Err(ElabError::new(
                    "error[E_NoMethodFound]: `.map_err()` on Option is outside the phase-0 subset (Result-only)".to_string(),
                    span,
                ));
            }
            (BuiltinMethod::UnwrapError, Ty::Option(_)) => {
                return Err(ElabError::new(
                    "error[E_NoMethodFound]: `.unwrap_error()` on Option is outside the phase-0 subset (Result-only)".to_string(),
                    span,
                ));
            }
            _ => {}
        }
        // Round XXIV Track D — closure-returning combinator axis-unify.
        // Mirror of `src/semantic/typecheck.rs:7583/7610/7633`, collapsed into
        // ONE call site because ggdef's arm-picker consolidates the per-cell
        // arms into a single match (production has one arm per cell). See the
        // twin lint's `EXPECTED_GGDEF_CALLERS = 1` — this is the Core #4
        // chokepoint. `combinator_cell` returns `None` for `.map` / `.map_err`
        // / `flat_map` / `unwrap_or_else` (out-of-class); the check no-ops
        // there. Runs before the write-materialize check because these are
        // read-only combinators (they never trip the D4-6 gate).
        if let Some(cell) = Self::combinator_cell(bm, &receiver_type) {
            if let Some(closure_arg) = args.first() {
                let closure_ret = self.infer_closure_arg_ret_ty(&closure_arg.node);
                self.unify_closure_ret_axis(cell, &receiver_type, &closure_ret, closure_arg.span)?;
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
        // Consume the capture + mark flags for THIS call before receiver/args
        // elaborate (a nested fallible call in them takes its own path).
        let capture = std::mem::take(&mut self.capture_ctx);
        let marked = std::mem::take(&mut self.fallible_marked);
        let self_src = self.self_source(receiver, minfo.self_mode)?;
        let mut out = vec![self_src];
        out.extend(self.call_args_reordered(&minfo.mangled, args, CallForm::Method)?);
        let call = Expr::Call { func: minfo.mangled.clone(), args: out };
        self.maybe_wrap_throws_call(call, &minfo.mangled, span, capture, marked)
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
        // D29 marked-match peel (statement twin of the `ast::Expr::Match` arm):
        // `match f()!: case Ok/Error` peels the `Result` to `T` — the `Ok`/
        // `Error` arms cannot match; reject. A bare `match f():` scrutinee is a
        // bare call, rejected at its `maybe_wrap` (Scrutinee capture retired).
        let has_result_arm = arms
            .iter()
            .filter_map(|i| i.arm())
            .any(|a| pattern_consumes_result(&a.pattern.node));
        self.reject_marked_match_result_arms(scrutinee, has_result_arm);
        let scrut = self.elaborate_expr(scrutinee)?;
        self.capture_ctx = CaptureCtx::None;
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
        is_move: bool,
        is_async: bool,
        params: &[Spanned<ast::ClosureParam>],
        body: &Spanned<ast::Expr>,
        span: Span,
    ) -> ElabResult<Expr> {
        // Take the destination hint set at the enclosing VarDecl/Assign BEFORE
        // recursing into the body — a stale callable-destination hint would
        // only confuse the `ArrayLiteral` set-vs-vector disambiguation nested
        // inside the body.
        let dest_hint = self.dest_ty_hint.take();
        if is_async {
            return Err(ElabError::new("async closures are phase 3", span));
        }
        // A `!(...)` (move-closure) literal bound to a non-consuming
        // `Callable`/`MutCallable` destination is a ratified reject in
        // production (`src/semantic/typecheck.rs:331-338` →
        // `E_ClosureKindMismatch`). ggdef was silently dropping `is_move` and
        // running the program (`fixtures/closure_move_kind_error.gg` printed
        // "should not reach here"); mirror the reject as a coded elaboration
        // error, not a codeless `IllFormed` (contract at `lib.rs:113-124`).
        // `ConsumeCallable[..]` destination (`consuming: true`) still accepts
        // `!(...)` — the ratified ADJ-MATCH shape (`consume_callable_once.gg`).
        // An absent / non-Callable hint stays silent: without the destination
        // in view, ggdef falls back to eval-side defense-in-depth downstream.
        if is_move {
            if let Some(Ty::Callable { consuming: false, .. }) = dest_hint {
                return Err(ElabError::new(
                    "error[E_ClosureKindMismatch]: consume-callable literal `!(...)` bound to \
                     a non-consuming `Callable`/`MutCallable` destination — declare the \
                     destination as `ConsumeCallable[...]` or remove the `!`",
                    span,
                ));
            }
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
                            "error[E_MoveWithoutOperator]: closure captures the drop-tainted local `{c}` \
                             by value; a type with a custom `Drop` is single-owner — capture \
                             `!{c}` to move or `{c}.clone()` to copy"
                        ),
                        span,
                    ));
                }
            }
        }
        // D4 position 4 (closure expression-tail): a closure whose body IS a
        // bare tainted PLACE rooted at a closure PARAM (`(R x): x`) returns a
        // copy at the closure return boundary — the same implicit-copy
        // rejection as a function expr-body tail. CAPTURE-rooted tails
        // (`(): hh.r`) are position 5's domain (already rejected above), so we
        // gate on the root being a closure param — a param-rooted tail cannot
        // be a capture, and this avoids double-reporting the capture case.
        // (Mirrors production's `Expr::Closure` tail arm, which skips
        // capture-rooted places; ggdef reads the param's declared Ty directly
        // rather than scoping it into `local_ty`, since a param-rooted tail
        // needs no body-env change.)
        if ast_is_place(&body.node) {
            if let Some(root) = root_local_name(&body.node) {
                if let Some(pty) = params
                    .iter()
                    .find(|p| p.node.name.node == root)
                    .and_then(|p| p.node.type_.as_ref())
                {
                    if self.ty_tainted(&ty_of_type(&pty.node)) {
                        return Err(ElabError::new(
                            format!(
                                "error[E_MoveWithoutOperator]: implicit copy of the drop-tainted place \
                                 `{root}` at closure-tail; a type with a custom `Drop` is \
                                 single-owner — write `!{root}` to move or `{root}.clone()` to copy"
                            ),
                            body.span,
                        ));
                    }
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

/// Whether an elaboration type is `Result[_,_]` (`Result` is a prelude enum
/// users cannot shadow; the payload arms are carried but irrelevant here).
fn ty_is_result(t: &Ty) -> bool {
    matches!(t, Ty::Result(..))
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
        | Expr::Panic(e)
        | Expr::Propagate(e) => collect_expr_locals(e, closures, out),
        Expr::Call { args, .. } | Expr::Construct { args, .. } | Expr::EnumConstruct { args, .. } => {
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::CallValue { callee, args, .. } => {
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
                // Prelude enums carry their payload(s) so D4 taint sees through
                // them (`Option[R]` / `Result[R,E]`). They still carry no user
                // methods — dispatch falls through to the builtin table.
                "Option" => Ty::Option(Box::new(arg(0))),
                "Result" => Ty::Result(Box::new(arg(0)), Box::new(arg(1))),
                // The callable family (D5 kind axis). ONLY `ConsumeCallable` is
                // single-owner (consumed by its call); `Callable`/`MutCallable`
                // are reusable. Classified once here so `CallValue` reads a
                // typed field, never the surface name.
                //
                // Track B3: extract the declared parameter sigil modes from
                // the inner function type (`Callable[void(&int)]` has an
                // `ast::Type::Function` at generic_args[0]) so the
                // `CallValue` elaboration can enforce the D31-uniform
                // sigil rule at indirect call sites.
                "ConsumeCallable" => Ty::Callable {
                    consuming: true,
                    param_ownerships: callable_param_ownerships(generic_args),
                },
                "Callable" | "MutCallable" => Ty::Callable {
                    consuming: false,
                    param_ownerships: callable_param_ownerships(generic_args),
                },
                other => Ty::Named(other.to_string()),
            }
        }
        ast::Type::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| ty_of_type(&t.node)).collect()),
        ast::Type::Ref(inner) | ast::Type::Owned(inner) => ty_of_type(&inner.node),
        _ => Ty::Unknown,
    }
}

/// Extract the declared per-parameter sigil modes from a callable's inner
/// function type: `Callable[void(&int, int)]` has `param_ownerships` at
/// generic_args[0]'s `ast::Type::Function`. Returns an empty Vec when the
/// generic arg is not a function type (or missing) — the sigil check then
/// falls back to Unknown, i.e. inert.
///
/// Track B3 (D31-uniform, ledger 2026-07-20 D31 ADDENDUM-2, FULL STRICT):
/// the call-site sigil rule applies UNIFORMLY at every indirect call site
/// whose callee has a resolvable function type; this helper is the source
/// where the per-arg param mode is read.
fn callable_param_ownerships(generic_args: &[gorget::span::Spanned<ast::Type>]) -> Vec<Mode> {
    match generic_args.first().map(|t| &t.node) {
        Some(ast::Type::Function { param_ownerships, .. }) => {
            param_ownerships.iter().map(|o| mode_of(*o)).collect()
        }
        _ => Vec::new(),
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

/// Whether an `equip … with <trait>` trait is `Displayable`. Round XXVI Track B:
/// used by `register_equip` to record the user's `display(self) -> String` into
/// `Program::display_fns`, which `eval::format_for_print` dispatches at print /
/// f-string interpolation.
fn trait_is_displayable(t: &ast::Type) -> bool {
    matches!(t, ast::Type::Named { name, .. } if name.node == "Displayable")
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

/// D10(b) place primitive: the (root local name, projection path) of a place
/// expression — mirrors production `find_root_def_id_with_path`
/// (src/semantic/safety/helpers.rs). Field names are outer-to-inner; tuple
/// segments are pre-dotted (`.0`). `Index`/`OptionalChain` COLLAPSE to the root
/// (an index borrow is from the collection itself — conservative). Non-places
/// return `None`.
fn ast_place(e: &ast::Expr) -> Option<(String, Vec<String>)> {
    match e {
        ast::Expr::Identifier(n) => Some((n.clone(), Vec::new())),
        ast::Expr::SelfExpr => Some(("self".to_string(), Vec::new())),
        ast::Expr::FieldAccess { object, field } => {
            let (root, mut path) = ast_place(&object.node)?;
            path.push(field.node.clone());
            Some((root, path))
        }
        ast::Expr::TupleFieldAccess { object, index } => {
            let (root, mut path) = ast_place(&object.node)?;
            path.push(format!(".{index}"));
            Some((root, path))
        }
        ast::Expr::Index { object, .. } | ast::Expr::OptionalChain { object, .. } => {
            ast_place(&object.node)
        }
        _ => None,
    }
}

/// D10(b): two projection paths under the SAME root OVERLAP iff one is a prefix
/// of the other (`zip` stops at the shorter). Disjoint siblings (`["a"]` vs
/// `["b"]`) do NOT overlap; an empty path (the whole binding) overlaps all.
fn paths_overlap(a: &[String], b: &[String]) -> bool {
    a.iter().zip(b.iter()).all(|(x, y)| x == y)
}

/// Render a place for a diagnostic: `root` followed by its projection segments.
fn render_place(root: &str, path: &[String]) -> String {
    let mut s = root.to_string();
    for seg in path {
        s.push('.');
        s.push_str(seg.strip_prefix('.').unwrap_or(seg));
    }
    s
}

/// Prefix a rendered place with its call-arg sigil (`&`/`!`/bare).
fn sigil_place(own: ast::Ownership, place: &str) -> String {
    match own {
        ast::Ownership::Borrow => place.to_string(),
        ast::Ownership::MutableBorrow => format!("&{place}"),
        ast::Ownership::Move => format!("!{place}"),
    }
}

/// The D10(b) place-overlap rejection (`decisions.md` D10 + the 2026-07-12
/// D10(b) ADDENDUM + Rider 1 REVISED). The message carries `error[E_BorrowConflict]`,
/// the code production surfaces from `SemanticErrorKind::BorrowConflict`.
fn place_overlap_error(a: &str, b: &str, span: Span) -> ElabError {
    ElabError::new(
        format!(
            "error[E_BorrowConflict]: cannot pass `{a}` and `{b}` in the same \
             call — their places overlap under conflicting sigils (D10(b) \
             place-overlap: a place has one exclusive writer; a Copy-typed bare \
             read is a value snapshot and is exempt)"
        ),
        span,
    )
}

/// The D10(a) rejection (`docs/define-gorget/decisions.md`, ratified
/// 2026-07-06; move-bind addendum 2026-07-11). A named `&`-binding creates a
/// SECOND live writable path to a place — the exclusivity violation D10 exists
/// to close — so the definition rejects it, mirroring production's
/// `expr_is_borrow_bind` in `src/semantic/typecheck.rs` (landed by `414e652a`).
/// The message carries the `error[E_LocalBorrowBind]` code the corpus
/// expectation greps (production surfaces the same code from
/// `SemanticErrorKind::LocalBorrowBind`).
fn local_borrow_bind_error(span: Span) -> ElabError {
    ElabError::new(
        "error[E_LocalBorrowBind]: cannot bind a mutable borrow (`&`) to a \
         name — a place has one exclusive writer, and a named `&`-binding \
         would alias a second writable path to it. Pass the borrow directly \
         at a call site (`f(&x)`) or mutate the place itself (`x.push(..)`, \
         `x.field = value`)",
        span,
    )
}

/// D10(a): does this initializer / assignment RHS bind a mutable borrow to a
/// name? True for a top-level `&expr` and for any value-position expression
/// whose result IS such a borrow: an if-expression branch, a match-expression
/// arm (or its `else`), and the TAIL of a `do:` / block expression — each is a
/// place where the whole expression's value is the branch/arm/tail value, so a
/// `&expr` there is the same named-`&`-bind. Deliberately NOT a deep walk:
/// `&x` nested in a call (`f(&x)`) is the legal call-arg form and is never
/// visited here — this helper is only invoked on VarDecl inits and assignment
/// RHS. Mirrors production `TypeChecker::expr_is_borrow_bind`.
fn expr_is_borrow_bind(expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::MutableBorrow { .. } => true,
        ast::Expr::If { then_branch, elif_branches, else_branch, .. } => {
            expr_is_borrow_bind(&then_branch.node)
                || elif_branches.iter().any(|(_, b)| expr_is_borrow_bind(&b.node))
                || else_branch.as_ref().is_some_and(|b| expr_is_borrow_bind(&b.node))
        }
        ast::Expr::Match { arms, else_arm, .. } => {
            arms.iter().any(|arm| expr_is_borrow_bind(&arm.body.node))
                || else_arm.as_ref().is_some_and(|b| expr_is_borrow_bind(&b.node))
        }
        ast::Expr::Do { body } => block_tail_is_borrow_bind(body),
        ast::Expr::Block(block) => block_tail_is_borrow_bind(block),
        _ => false,
    }
}

/// The value of a `do:` / block expression is its TAIL statement. A tail whose
/// value is a `&expr` makes the block a borrow-bind. The tail may be a plain
/// expression OR a STATEMENT-FORM `if`/`match` used in value position, so
/// recurse those too. Mirrors production `TypeChecker::block_tail_is_borrow_bind`.
fn block_tail_is_borrow_bind(block: &ast::Block) -> bool {
    match block.stmts.last() {
        Some(last) => match &last.node {
            ast::Stmt::Expr(e) => expr_is_borrow_bind(&e.node),
            ast::Stmt::If { then_body, elif_branches, else_body, .. } => {
                block_tail_is_borrow_bind(then_body)
                    || elif_branches.iter().any(|(_, b)| block_tail_is_borrow_bind(b))
                    || else_body.as_ref().is_some_and(block_tail_is_borrow_bind)
            }
            ast::Stmt::Match { arms, else_arm, .. } => {
                arms.iter().any(|item| {
                    let arm = match item {
                        ast::MatchItem::Arm(a) => a,
                        ast::MatchItem::MetaFor { arm_template, .. } => arm_template,
                    };
                    expr_is_borrow_bind(&arm.body.node)
                }) || else_arm.as_ref().is_some_and(block_tail_is_borrow_bind)
            }
            _ => false,
        },
        None => false,
    }
}

/// If `e` is a `print(arg)` call, return the single POSITIONAL argument
/// expression. A single NAMED arg (e.g. `print(terminator=", ")` — nonsensical
/// but syntactically parseable) is NOT recognized here: returning None lets it
/// fall through to `Stmt::Expr → elaborate_call`, where `:1781`'s
/// `reject_named_args` fires a LOUD ElabError. This is the Core #4 sibling of
/// the print-dispatch class (Round XXV Track E) — the name-guard here + the
/// `reject_named_args` at `:1781` together close the class.
fn as_print_call(e: &Spanned<ast::Expr>) -> Option<&Spanned<ast::Expr>> {
    if let ast::Expr::Call { callee, args, .. } = &e.node {
        if let ast::Expr::Identifier(name) = &callee.node {
            if name == "print" && args.len() == 1 && args[0].node.name.is_none() {
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
        B::Pow => BinOp::Pow,
        B::Div => BinOp::Div,
        B::Rem | B::Mod => BinOp::Rem,
        // D26 (Round XXXIII Batch C1): fallible arithmetic (`+! -! *! /! %!`)
        // maps to the `*Fallible` GGC variants; eval produces Result.Ok / .Error.
        B::AddFallible => BinOp::AddFallible,
        B::SubFallible => BinOp::SubFallible,
        B::MulFallible => BinOp::MulFallible,
        B::DivFallible => BinOp::DivFallible,
        B::RemFallible => BinOp::RemFallible,
        // D26 shift-fallible (`<<! >>!`) is OUT-OF-SUBSET (Increment A). The
        // plain shift ops (`Shl`/`Shr`) are also out-of-subset — kept parallel.
        B::ShlFallible | B::ShrFallible => {
            return Err(ElabError::new(
                format!("operator {op:?} is outside the Increment-A subset (D26 shift-fallible)"),
                span,
            ));
        }
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
