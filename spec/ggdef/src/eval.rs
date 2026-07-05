//! The definitional interpreter — one fuel-indexed functional-big-step
//! evaluator (RFC §2.3/§2.7).
//!
//! "One fuel-indexed function" is realised MiniRust/CakeML-style: a family of
//! mutually-recursive big-step arms (`eval_expr`/`exec_stmt`/`exec_block`/
//! `call_function`) that thread a single linear `State` and a decrementing
//! fuel counter. Fuel makes it **total**: every run ends in exactly one of the
//! four outcomes (Value/Trap/IllFormed/FuelExhausted, RFC §2.3), so GGC has no
//! undefined behaviour. State is *plain data* — no `Rc`/`RefCell`/`unsafe`.
//!
//! ## The store model (how §2.2 is realised)
//!
//! A `Frame` per function call holds a stack of `Local`s. Each local's `Slot`
//! is one of:
//!   * `Owned(Value)`        — this binding owns the value.
//!   * `BorrowView(Place)`   — a non-owning view (bare param / for-var). Reads
//!                             see the aliased place; the **first write
//!                             materialises** a private `Owned` copy, leaving
//!                             the owner untouched (the core CoW rule).
//!   * `WriteThrough(Place)` — a `&` alias: writes land on the aliased place.
//!   * `Moved`               — moved out; any read is `IllFormed`.
//!
//! A `Place` is `(frame, local, projections)`. Because call frames stay live
//! on the stack, a callee's borrow can name a caller place directly — no
//! copy-in/copy-out approximation. Reads clone out (value semantics); implicit
//! copies clone at the binding point (eager — "copy timing is unobservable",
//! D1). Returns copy at the boundary, so no borrow escapes a popped frame.

use std::collections::HashMap;

use gorget::span::Span;

use crate::ggc::{
    BinOp, BuiltinMethod, ConstructKind, Expr, FPart, Function, Program, Source, Stmt, UnOp, Value,
};
use crate::trace::TraceEvent;

// ── Exit codes (deliverable 6) ─────────────────────────────────────────────
// PROVISIONAL until the trap-normalization spec text lands in Increment B.
// They exist so `ggdef run` is scriptable today.
/// Program ran to completion with a value.
pub const EXIT_VALUE: i32 = 0;
/// A catchable fault escaped uncaught (defined panic).
pub const EXIT_TRAP: i32 = 101;
/// A statically-ill-formed program was detected dynamically.
pub const EXIT_ILLFORMED: i32 = 102;
/// The fuel bound was reached (non-termination guard).
pub const EXIT_FUEL: i32 = 103;

/// A catchable fault (RFC §2.3 Trap(Fault)).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fault {
    Overflow,
    DivByZero,
    Bounds,
}

impl Fault {
    pub fn message(&self) -> &'static str {
        match self {
            Fault::Overflow => "arithmetic overflow",
            Fault::DivByZero => "division by zero",
            Fault::Bounds => "index out of bounds",
        }
    }
}

/// The four total outcomes of evaluation (RFC §2.3).
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Value(Value),
    Trap(Fault),
    IllFormed(String),
    FuelExhausted,
}

impl Outcome {
    /// The provisional process exit code for this outcome (deliverable 6).
    pub fn exit_code(&self) -> i32 {
        match self {
            Outcome::Value(_) => EXIT_VALUE,
            Outcome::Trap(_) => EXIT_TRAP,
            Outcome::IllFormed(_) => EXIT_ILLFORMED,
            Outcome::FuelExhausted => EXIT_FUEL,
        }
    }
}

/// Internal short-circuit: a *terminal* result that unwinds the whole eval.
/// Distinct from `Flow` (return/break/continue), which is normal control flow.
#[derive(Debug, Clone)]
enum Halt {
    Trap(Fault),
    IllFormed(String),
    FuelExhausted,
}

/// Normal (non-terminal) control flow within a function body.
#[derive(Debug, Clone)]
enum Flow {
    Normal,
    Return(Value),
    Break,
    Continue,
}

/// A projection step from a root local into a nested value.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Proj {
    Field(String),
    Index(usize),
}

/// A storage location: a local in a frame, plus a projection path.
#[derive(Debug, Clone)]
struct Place {
    frame: usize,
    local: usize,
    proj: Vec<Proj>,
}

impl Place {
    fn extend(&self, extra: &[Proj]) -> Place {
        let mut proj = self.proj.clone();
        proj.extend_from_slice(extra);
        Place { frame: self.frame, local: self.local, proj }
    }
}

/// A binding's storage discipline (see the module doc).
#[derive(Debug, Clone)]
enum Slot {
    Owned(Value),
    BorrowView(Place),
    WriteThrough(Place),
    Moved,
}

/// One binding in a frame.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    slot: Slot,
    /// Declaration span — provenance for the scope-exit `Drop` event.
    span: Span,
}

/// One function activation.
#[derive(Debug, Clone)]
struct Frame {
    locals: Vec<Local>,
}

/// The whole evaluator state: the call stack, the fuel counter, the observable
/// stdout, and the provenance trace.
struct State {
    frames: Vec<Frame>,
    fuel: u64,
    stdout: String,
    trace: Vec<TraceEvent>,
    /// The span of the statement currently executing — stamped onto trace
    /// events emitted deep inside expression evaluation (e.g. a materialize
    /// during a push) without threading spans through every arm.
    cur_span: Span,
}

impl State {
    fn new(fuel: u64) -> Self {
        State { frames: Vec::new(), fuel, stdout: String::new(), trace: Vec::new(), cur_span: Span::dummy() }
    }

    /// Spend one unit of fuel; `FuelExhausted` when the bound is reached.
    fn tick(&mut self) -> Result<(), Halt> {
        if self.fuel == 0 {
            return Err(Halt::FuelExhausted);
        }
        self.fuel -= 1;
        Ok(())
    }

    fn cur_frame(&self) -> usize {
        self.frames.len() - 1
    }
}

/// Immutable per-run context: the program plus name→index lookups.
struct Ctx<'a> {
    prog: &'a Program,
    funcs: HashMap<&'a str, usize>,
    structs: HashMap<&'a str, usize>,
}

impl<'a> Ctx<'a> {
    fn new(prog: &'a Program) -> Self {
        let funcs = prog.functions.iter().enumerate().map(|(i, f)| (f.name.as_str(), i)).collect();
        let structs = prog.structs.iter().enumerate().map(|(i, s)| (s.name.as_str(), i)).collect();
        Ctx { prog, funcs, structs }
    }

    fn struct_fields(&self, name: &str) -> Option<&'a [String]> {
        self.structs.get(name).map(|&i| self.prog.structs[i].fields.as_slice())
    }
}

/// The result of a whole run: the outcome, the observable stdout, and the
/// full provenance trace (present on ALL outcomes, per RFC §2.3).
pub struct Run {
    pub outcome: Outcome,
    pub stdout: String,
    pub trace: Vec<TraceEvent>,
}

/// Evaluate a program from `main`, bounded by `fuel`.
pub fn run(program: &Program, fuel: u64) -> Run {
    let ctx = Ctx::new(program);
    let Some(&main_idx) = ctx.funcs.get("main") else {
        return Run {
            outcome: Outcome::IllFormed("no `main` function".to_string()),
            stdout: String::new(),
            trace: Vec::new(),
        };
    };
    let mut state = State::new(fuel);
    let outcome = match call_function(&ctx, &mut state, main_idx, Vec::new()) {
        Ok(_) => Outcome::Value(Value::Unit),
        Err(Halt::Trap(f)) => Outcome::Trap(f),
        Err(Halt::IllFormed(m)) => Outcome::IllFormed(m),
        Err(Halt::FuelExhausted) => Outcome::FuelExhausted,
    };
    Run { outcome, stdout: state.stdout, trace: state.trace }
}

// ── Function calls ─────────────────────────────────────────────────────────

fn call_function(
    ctx: &Ctx,
    state: &mut State,
    fn_idx: usize,
    param_slots: Vec<Slot>,
) -> Result<Value, Halt> {
    state.tick()?;
    let func: &Function = &ctx.prog.functions[fn_idx];
    // Params occupy the function's root scope; they drop with the top-level
    // body locals in reverse declaration order (params first ⇒ dropped last).
    let mut locals = Vec::with_capacity(param_slots.len());
    for (param, slot) in func.params.iter().zip(param_slots) {
        locals.push(Local { name: param.name.clone(), slot, span: param.span });
    }
    state.frames.push(Frame { locals });

    // The body runs directly in the root scope (mark 0).
    let body = func.body.clone();
    let mut flow = Flow::Normal;
    for stmt in &body {
        flow = exec_stmt(ctx, state, stmt)?;
        if !matches!(flow, Flow::Normal) {
            break;
        }
    }
    let ret = match flow {
        Flow::Return(v) => v,
        Flow::Normal => Value::Unit,
        // break/continue reaching a function top would be ill-formed; the
        // elaborator only emits them inside loops, so this is defensive.
        Flow::Break | Flow::Continue => {
            return Err(Halt::IllFormed("break/continue outside a loop".to_string()));
        }
    };
    drop_scope(state, 0);
    state.frames.pop();
    Ok(ret)
}

/// Drop every local from `mark`.. in reverse declaration order, emitting a
/// `Drop` event for each owned, droppable value, then truncate the frame's
/// locals to `mark`. Scalars are `Copy` and do not drop.
fn drop_scope(state: &mut State, mark: usize) {
    let frame = state.cur_frame();
    let mut i = state.frames[frame].locals.len();
    while i > mark {
        i -= 1;
        let local = &state.frames[frame].locals[i];
        if let Slot::Owned(v) = &local.slot {
            if is_droppable(v) {
                // Drop provenance is the binding's declaration site.
                state.trace.push(TraceEvent::Drop { place: local.name.clone(), span: local.span });
            }
        }
    }
    state.frames[frame].locals.truncate(mark);
}

/// Non-scalar values carry a (possibly custom, in B) drop. Scalars are `Copy`.
fn is_droppable(v: &Value) -> bool {
    !matches!(v, Value::Unit | Value::Int(_) | Value::Bool(_) | Value::Float(_))
}

// ── Statements ─────────────────────────────────────────────────────────────

fn exec_block(ctx: &Ctx, state: &mut State, block: &[Stmt]) -> Result<Flow, Halt> {
    let mark = state.frames[state.cur_frame()].locals.len();
    let mut flow = Flow::Normal;
    for stmt in block {
        flow = exec_stmt(ctx, state, stmt)?;
        if !matches!(flow, Flow::Normal) {
            break;
        }
    }
    // Leaving the block drops its locals even on an early return/break/continue.
    drop_scope(state, mark);
    Ok(flow)
}

fn exec_stmt(ctx: &Ctx, state: &mut State, stmt: &Stmt) -> Result<Flow, Halt> {
    state.tick()?;
    match stmt {
        Stmt::Bind { name, source, span } => {
            state.cur_span = *span;
            let slot = eval_source_to_slot(ctx, state, source, *span)?;
            let frame = state.cur_frame();
            state.frames[frame].locals.push(Local { name: name.clone(), slot, span: *span });
            Ok(Flow::Normal)
        }
        Stmt::Assign { target, value, span } => {
            state.cur_span = *span;
            let v = eval_source_to_value(ctx, state, value, *span)?;
            let place = eval_place(ctx, state, target)?;
            resolve_write(ctx, state, &place, v, *span)?;
            state.trace.push(TraceEvent::Write { place: target.place_str(), span: *span });
            Ok(Flow::Normal)
        }
        Stmt::Expr { expr, span } => {
            state.cur_span = *span;
            eval_expr(ctx, state, expr)?;
            Ok(Flow::Normal)
        }
        Stmt::Print { expr, span } => {
            state.cur_span = *span;
            let v = eval_expr(ctx, state, expr)?;
            state.stdout.push_str(&format_value(&v));
            state.stdout.push('\n');
            Ok(Flow::Normal)
        }
        Stmt::If { cond, then_, else_, span } => {
            state.cur_span = *span;
            if eval_bool(ctx, state, cond)? {
                exec_block(ctx, state, then_)
            } else {
                exec_block(ctx, state, else_)
            }
        }
        Stmt::While { cond, body, span } => {
            loop {
                state.cur_span = *span;
                if !eval_bool(ctx, state, cond)? {
                    break;
                }
                match exec_block(ctx, state, body)? {
                    Flow::Normal | Flow::Continue => {}
                    Flow::Break => break,
                    Flow::Return(v) => return Ok(Flow::Return(v)),
                }
            }
            Ok(Flow::Normal)
        }
        Stmt::Loop { body, span } => loop {
            state.cur_span = *span;
            // Tick per iteration so an empty-body `loop:` still terminates via
            // fuel (`while` ticks through its condition; `loop` has none).
            state.tick()?;
            match exec_block(ctx, state, body)? {
                Flow::Normal | Flow::Continue => {}
                Flow::Break => return Ok(Flow::Normal),
                Flow::Return(v) => return Ok(Flow::Return(v)),
            }
        },
        Stmt::Return { value, span } => {
            state.cur_span = *span;
            let v = match value {
                Some(e) => eval_expr(ctx, state, e)?,
                None => Value::Unit,
            };
            Ok(Flow::Return(v))
        }
        Stmt::Break { .. } => Ok(Flow::Break),
        Stmt::Continue { .. } => Ok(Flow::Continue),
    }
}

// ── Sources (the copy/move/borrow decision, realised) ──────────────────────

/// Resolve a `Source` into a fresh slot (for `let` bindings and param setup).
fn eval_source_to_slot(ctx: &Ctx, state: &mut State, source: &Source, span: Span) -> Result<Slot, Halt> {
    match source {
        Source::Copy(place) => {
            let p = eval_place(ctx, state, place)?;
            let v = resolve_read(ctx, state, &p)?;
            state.trace.push(TraceEvent::BindCopy { place: place.place_str(), span });
            Ok(Slot::Owned(v))
        }
        Source::Move(place) => {
            let p = eval_place(ctx, state, place)?;
            let v = resolve_read(ctx, state, &p)?;
            kill_place(state, &p);
            state.trace.push(TraceEvent::Move { from: place.place_str(), span });
            Ok(Slot::Owned(v))
        }
        Source::BorrowView(place) => {
            let p = eval_place(ctx, state, place)?;
            Ok(Slot::BorrowView(p))
        }
        Source::WriteThrough(place) => {
            let p = eval_place(ctx, state, place)?;
            Ok(Slot::WriteThrough(p))
        }
        Source::Value(expr) => {
            let v = eval_expr(ctx, state, expr)?;
            Ok(Slot::Owned(v))
        }
    }
}

/// Resolve a `Source` into an owned value (for OWNING positions: collection
/// put, struct/enum field init, return, assign RHS). Never a view — the
/// elaborator does not emit `BorrowView`/`WriteThrough` in owning positions.
fn eval_source_to_value(ctx: &Ctx, state: &mut State, source: &Source, span: Span) -> Result<Value, Halt> {
    match source {
        Source::Copy(place) => {
            let p = eval_place(ctx, state, place)?;
            let v = resolve_read(ctx, state, &p)?;
            state.trace.push(TraceEvent::BindCopy { place: place.place_str(), span });
            Ok(v)
        }
        Source::Move(place) => {
            let p = eval_place(ctx, state, place)?;
            let v = resolve_read(ctx, state, &p)?;
            kill_place(state, &p);
            state.trace.push(TraceEvent::Move { from: place.place_str(), span });
            Ok(v)
        }
        Source::Value(expr) => eval_expr(ctx, state, expr),
        Source::BorrowView(_) | Source::WriteThrough(_) => {
            Err(Halt::IllFormed("internal: alias source in an owning position".to_string()))
        }
    }
}

/// Kill a whole-local place (a move source). Projected moves don't occur in
/// the A subset; for those we leave the source live (the read already copied).
fn kill_place(state: &mut State, place: &Place) {
    if place.proj.is_empty() {
        state.frames[place.frame].locals[place.local].slot = Slot::Moved;
    }
}

// ── Places ─────────────────────────────────────────────────────────────────

/// Resolve a place expression to a `Place` (root local + projections),
/// evaluating any index expressions **once** (so `&v[side()]` runs `side`
/// exactly one time).
fn eval_place(ctx: &Ctx, state: &mut State, expr: &Expr) -> Result<Place, Halt> {
    match expr {
        Expr::Local(name) => {
            let frame = state.cur_frame();
            let local = lookup_local(state, frame, name)
                .ok_or_else(|| Halt::IllFormed(format!("unresolved local `{name}`")))?;
            Ok(Place { frame, local, proj: Vec::new() })
        }
        Expr::Field(obj, field) => {
            let mut p = eval_place(ctx, state, obj)?;
            p.proj.push(Proj::Field(field.clone()));
            Ok(p)
        }
        Expr::TupleField(obj, i) => {
            let mut p = eval_place(ctx, state, obj)?;
            p.proj.push(Proj::Index(*i));
            Ok(p)
        }
        Expr::Index(obj, idx) => {
            let i = eval_index(ctx, state, idx)?;
            let mut p = eval_place(ctx, state, obj)?;
            p.proj.push(Proj::Index(i));
            Ok(p)
        }
        _ => Err(Halt::IllFormed("expression is not a place".to_string())),
    }
}

/// Find the most-recently-declared local named `name` in `frame` (shadowing).
fn lookup_local(state: &State, frame: usize, name: &str) -> Option<usize> {
    state.frames[frame].locals.iter().rposition(|l| l.name == name)
}

fn eval_index(ctx: &Ctx, state: &mut State, idx: &Expr) -> Result<usize, Halt> {
    match eval_expr(ctx, state, idx)? {
        Value::Int(i) if i >= 0 => Ok(i as usize),
        Value::Int(_) => Err(Halt::Trap(Fault::Bounds)),
        v => Err(Halt::IllFormed(format!("index must be int, got {}", v.kind_name()))),
    }
}

// ── Read / write through the store (follows borrows) ───────────────────────

fn resolve_read(ctx: &Ctx, state: &State, place: &Place) -> Result<Value, Halt> {
    let follow = match &state.frames[place.frame].locals[place.local].slot {
        Slot::Owned(v) => return navigate_read(v, &place.proj),
        Slot::BorrowView(t) | Slot::WriteThrough(t) => t.clone(),
        Slot::Moved => {
            return Err(Halt::IllFormed(format!(
                "read of moved-out value `{}`",
                state.frames[place.frame].locals[place.local].name
            )));
        }
    };
    let full = follow.extend(&place.proj);
    resolve_read(ctx, state, &full)
}

fn resolve_write(ctx: &Ctx, state: &mut State, place: &Place, newval: Value, span: Span) -> Result<(), Halt> {
    enum Action {
        WriteOwned,
        Follow(Place),
        Materialize(Place, String),
    }
    let action = match &state.frames[place.frame].locals[place.local].slot {
        Slot::Owned(_) => Action::WriteOwned,
        Slot::WriteThrough(t) => Action::Follow(t.clone()),
        Slot::BorrowView(t) => {
            Action::Materialize(t.clone(), state.frames[place.frame].locals[place.local].name.clone())
        }
        Slot::Moved => {
            return Err(Halt::IllFormed(format!(
                "write to moved-out value `{}`",
                state.frames[place.frame].locals[place.local].name
            )));
        }
    };
    match action {
        Action::WriteOwned => {
            if let Slot::Owned(v) = &mut state.frames[place.frame].locals[place.local].slot {
                navigate_write(v, &place.proj, newval)
            } else {
                unreachable!("slot changed under WriteOwned")
            }
        }
        Action::Follow(t) => {
            let full = t.extend(&place.proj);
            resolve_write(ctx, state, &full, newval, span)
        }
        Action::Materialize(t, name) => {
            // First write through a Borrow binding: privatise the whole aliased
            // value into this slot, leaving the owner untouched, then apply the
            // write to the now-owned copy.
            let cur = resolve_read(ctx, state, &t)?;
            state.trace.push(TraceEvent::Materialize { place: name, span });
            state.frames[place.frame].locals[place.local].slot = Slot::Owned(cur);
            if let Slot::Owned(v) = &mut state.frames[place.frame].locals[place.local].slot {
                navigate_write(v, &place.proj, newval)
            } else {
                unreachable!("slot changed under Materialize")
            }
        }
    }
}

fn navigate_read(v: &Value, proj: &[Proj]) -> Result<Value, Halt> {
    let mut cur = v;
    for p in proj {
        cur = match (cur, p) {
            (Value::Struct { fields, .. }, Proj::Field(name)) => fields
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, vv)| vv)
                .ok_or_else(|| Halt::IllFormed(format!("no field `{name}`")))?,
            (Value::Vector(items), Proj::Index(i)) | (Value::Tuple(items), Proj::Index(i)) => {
                items.get(*i).ok_or(Halt::Trap(Fault::Bounds))?
            }
            (other, p) => {
                return Err(Halt::IllFormed(format!(
                    "read projection {p:?} on {}",
                    other.kind_name()
                )));
            }
        };
    }
    Ok(cur.clone())
}

fn navigate_write(v: &mut Value, proj: &[Proj], newval: Value) -> Result<(), Halt> {
    match proj.split_first() {
        None => {
            *v = newval;
            Ok(())
        }
        Some((head, rest)) => {
            let child = match (v, head) {
                (Value::Struct { fields, .. }, Proj::Field(name)) => fields
                    .iter_mut()
                    .find(|(n, _)| n == name)
                    .map(|(_, vv)| vv)
                    .ok_or_else(|| Halt::IllFormed(format!("no field `{name}`")))?,
                (Value::Vector(items), Proj::Index(i)) | (Value::Tuple(items), Proj::Index(i)) => {
                    items.get_mut(*i).ok_or(Halt::Trap(Fault::Bounds))?
                }
                (other, p) => {
                    return Err(Halt::IllFormed(format!(
                        "write projection {p:?} on {}",
                        other.kind_name()
                    )));
                }
            };
            navigate_write(child, rest, newval)
        }
    }
}

// ── Expressions ────────────────────────────────────────────────────────────

fn eval_expr(ctx: &Ctx, state: &mut State, expr: &Expr) -> Result<Value, Halt> {
    state.tick()?;
    match expr {
        Expr::Int(i) => Ok(Value::Int(*i)),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Float(f) => Ok(Value::Float(*f)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::FString(parts) => {
            let mut out = String::new();
            for part in parts {
                match part {
                    FPart::Lit(s) => out.push_str(s),
                    FPart::Interp(e) => {
                        let v = eval_expr(ctx, state, e)?;
                        out.push_str(&format_value(&v));
                    }
                }
            }
            Ok(Value::Str(out))
        }

        // Places in read position: go through the store (follows borrows) when
        // the whole expression is a place; otherwise navigate into a temp.
        Expr::Local(_) => {
            let p = eval_place(ctx, state, expr)?;
            resolve_read(ctx, state, &p)
        }
        Expr::Field(obj, field) => {
            if obj_is_place(obj) {
                let p = eval_place(ctx, state, expr)?;
                resolve_read(ctx, state, &p)
            } else {
                let v = eval_expr(ctx, state, obj)?;
                navigate_read(&v, &[Proj::Field(field.clone())])
            }
        }
        Expr::TupleField(obj, i) => {
            if obj_is_place(obj) {
                let p = eval_place(ctx, state, expr)?;
                resolve_read(ctx, state, &p)
            } else {
                let v = eval_expr(ctx, state, obj)?;
                navigate_read(&v, &[Proj::Index(*i)])
            }
        }
        Expr::Index(obj, idx) => {
            if obj_is_place(obj) {
                let p = eval_place(ctx, state, expr)?;
                resolve_read(ctx, state, &p)
            } else {
                let i = eval_index(ctx, state, idx)?;
                let v = eval_expr(ctx, state, obj)?;
                navigate_read(&v, &[Proj::Index(i)])
            }
        }

        Expr::Binary(op, l, r) => eval_binary(ctx, state, *op, l, r),
        Expr::Unary(op, e) => {
            let v = eval_expr(ctx, state, e)?;
            eval_unary(*op, v)
        }

        Expr::Call { func, args } => {
            if func == "print" {
                // Defensive: `print` is normally lowered to `Stmt::Print`.
                if let Some(a) = args.first() {
                    let v = eval_source_to_value(ctx, state, a, state.cur_span)?;
                    state.stdout.push_str(&format_value(&v));
                    state.stdout.push('\n');
                }
                return Ok(Value::Unit);
            }
            let &idx = ctx
                .funcs
                .get(func.as_str())
                .ok_or_else(|| Halt::IllFormed(format!("unknown function `{func}`")))?;
            let mut slots = Vec::with_capacity(args.len());
            for a in args {
                slots.push(eval_source_to_slot(ctx, state, a, state.cur_span)?);
            }
            call_function(ctx, state, idx, slots)
        }

        Expr::Construct { kind, args } => eval_construct(ctx, state, kind, args),

        Expr::Method { recv, method, args } => eval_method(ctx, state, recv, *method, args),

        Expr::Clone(inner) => {
            // `resolve_read` already deep-copies, so the result is independent.
            let v = eval_expr(ctx, state, inner)?;
            state.trace.push(TraceEvent::ExplicitClone { place: inner.place_str(), span: state.cur_span });
            Ok(v)
        }
    }
}

/// Whether an expression's *base* denotes a place, so a Field/Index read should
/// go through the store (to follow borrows) rather than navigate a temp.
fn obj_is_place(obj: &Expr) -> bool {
    match obj {
        Expr::Local(_) => true,
        Expr::Field(o, _) | Expr::TupleField(o, _) | Expr::Index(o, _) => obj_is_place(o),
        _ => false,
    }
}

fn eval_bool(ctx: &Ctx, state: &mut State, expr: &Expr) -> Result<bool, Halt> {
    match eval_expr(ctx, state, expr)? {
        Value::Bool(b) => Ok(b),
        v => Err(Halt::IllFormed(format!("condition must be bool, got {}", v.kind_name()))),
    }
}

fn eval_construct(ctx: &Ctx, state: &mut State, kind: &ConstructKind, args: &[Source]) -> Result<Value, Halt> {
    match kind {
        ConstructKind::Vector => {
            let mut items = Vec::with_capacity(args.len());
            for a in args {
                items.push(eval_source_to_value(ctx, state, a, state.cur_span)?);
            }
            Ok(Value::Vector(items))
        }
        ConstructKind::Tuple => {
            let mut items = Vec::with_capacity(args.len());
            for a in args {
                items.push(eval_source_to_value(ctx, state, a, state.cur_span)?);
            }
            Ok(Value::Tuple(items))
        }
        ConstructKind::Struct(name) => {
            let field_names: Vec<String> = ctx
                .struct_fields(name)
                .ok_or_else(|| Halt::IllFormed(format!("unknown struct `{name}`")))?
                .to_vec();
            if field_names.len() != args.len() {
                return Err(Halt::IllFormed(format!(
                    "struct `{name}` expects {} fields, got {}",
                    field_names.len(),
                    args.len()
                )));
            }
            let mut fields = Vec::with_capacity(args.len());
            for (fname, a) in field_names.iter().zip(args) {
                let v = eval_source_to_value(ctx, state, a, state.cur_span)?;
                fields.push((fname.clone(), v));
            }
            Ok(Value::Struct { name: name.clone(), fields })
        }
    }
}

fn eval_method(ctx: &Ctx, state: &mut State, recv: &Expr, method: BuiltinMethod, args: &[Source]) -> Result<Value, Halt> {
    match method {
        BuiltinMethod::Len => {
            let p = eval_place(ctx, state, recv)?;
            let v = resolve_read(ctx, state, &p)?;
            match v {
                Value::Vector(items) => Ok(Value::Int(items.len() as i64)),
                other => Err(Halt::IllFormed(format!("`.len()` on {}", other.kind_name()))),
            }
        }
        BuiltinMethod::Push => {
            // Collection put: the arg is an owning copy/move.
            let arg = args
                .first()
                .ok_or_else(|| Halt::IllFormed("push expects 1 arg".to_string()))?;
            let elem = eval_source_to_value(ctx, state, arg, state.cur_span)?;
            let p = eval_place(ctx, state, recv)?;
            let mut vec = match resolve_read(ctx, state, &p)? {
                Value::Vector(items) => items,
                other => return Err(Halt::IllFormed(format!("`.push()` on {}", other.kind_name()))),
            };
            vec.push(elem);
            resolve_write(ctx, state, &p, Value::Vector(vec), state.cur_span)?;
            state.trace.push(TraceEvent::Write { place: recv.place_str(), span: state.cur_span });
            Ok(Value::Unit)
        }
        BuiltinMethod::Set => {
            if args.len() != 2 {
                return Err(Halt::IllFormed("set expects 2 args".to_string()));
            }
            let idx = match &args[0] {
                Source::Value(e) => eval_index(ctx, state, e)?,
                other => eval_index_source(ctx, state, other)?,
            };
            let elem = eval_source_to_value(ctx, state, &args[1], state.cur_span)?;
            let p = eval_place(ctx, state, recv)?;
            let mut vec = match resolve_read(ctx, state, &p)? {
                Value::Vector(items) => items,
                other => return Err(Halt::IllFormed(format!("`.set()` on {}", other.kind_name()))),
            };
            if idx >= vec.len() {
                return Err(Halt::Trap(Fault::Bounds));
            }
            vec[idx] = elem;
            resolve_write(ctx, state, &p, Value::Vector(vec), state.cur_span)?;
            state.trace.push(TraceEvent::Write { place: recv.place_str(), span: state.cur_span });
            Ok(Value::Unit)
        }
    }
}

/// Evaluate a `Source` in index position to a `usize` (for `.set(i, x)`).
fn eval_index_source(ctx: &Ctx, state: &mut State, source: &Source) -> Result<usize, Halt> {
    let v = eval_source_to_value(ctx, state, source, state.cur_span)?;
    match v {
        Value::Int(i) if i >= 0 => Ok(i as usize),
        Value::Int(_) => Err(Halt::Trap(Fault::Bounds)),
        other => Err(Halt::IllFormed(format!("index must be int, got {}", other.kind_name()))),
    }
}

// ── Operators ──────────────────────────────────────────────────────────────

fn eval_binary(ctx: &Ctx, state: &mut State, op: BinOp, l: &Expr, r: &Expr) -> Result<Value, Halt> {
    // Short-circuit boolean operators.
    if matches!(op, BinOp::And | BinOp::Or) {
        let lb = eval_bool(ctx, state, l)?;
        return match op {
            BinOp::And => {
                if !lb {
                    Ok(Value::Bool(false))
                } else {
                    Ok(Value::Bool(eval_bool(ctx, state, r)?))
                }
            }
            BinOp::Or => {
                if lb {
                    Ok(Value::Bool(true))
                } else {
                    Ok(Value::Bool(eval_bool(ctx, state, r)?))
                }
            }
            _ => unreachable!(),
        };
    }

    let lv = eval_expr(ctx, state, l)?;
    let rv = eval_expr(ctx, state, r)?;
    match (op, &lv, &rv) {
        // Integer arithmetic — checked (overflow ⇒ Trap(Overflow)).
        (BinOp::Add, Value::Int(a), Value::Int(b)) => {
            a.checked_add(*b).map(Value::Int).ok_or(Halt::Trap(Fault::Overflow))
        }
        (BinOp::Sub, Value::Int(a), Value::Int(b)) => {
            a.checked_sub(*b).map(Value::Int).ok_or(Halt::Trap(Fault::Overflow))
        }
        (BinOp::Mul, Value::Int(a), Value::Int(b)) => {
            a.checked_mul(*b).map(Value::Int).ok_or(Halt::Trap(Fault::Overflow))
        }
        (BinOp::Div, Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(Halt::Trap(Fault::DivByZero))
            } else {
                a.checked_div(*b).map(Value::Int).ok_or(Halt::Trap(Fault::Overflow))
            }
        }
        (BinOp::Rem, Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(Halt::Trap(Fault::DivByZero))
            } else {
                a.checked_rem(*b).map(Value::Int).ok_or(Halt::Trap(Fault::Overflow))
            }
        }

        // Float arithmetic — IEEE, no trap (matches hardware; D8 governs print).
        (BinOp::Add, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
        (BinOp::Sub, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
        (BinOp::Mul, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
        (BinOp::Div, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),

        // String concatenation.
        (BinOp::Add, Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{a}{b}"))),

        // Comparisons.
        (BinOp::Eq, _, _) => Ok(Value::Bool(values_eq(&lv, &rv))),
        (BinOp::Neq, _, _) => Ok(Value::Bool(!values_eq(&lv, &rv))),
        (BinOp::Lt, _, _) => cmp(&lv, &rv, |o| o.is_lt()),
        (BinOp::Gt, _, _) => cmp(&lv, &rv, |o| o.is_gt()),
        (BinOp::LtEq, _, _) => cmp(&lv, &rv, |o| o.is_le()),
        (BinOp::GtEq, _, _) => cmp(&lv, &rv, |o| o.is_ge()),

        _ => Err(Halt::IllFormed(format!(
            "operator {op:?} on {} and {}",
            lv.kind_name(),
            rv.kind_name()
        ))),
    }
}

fn eval_unary(op: UnOp, v: Value) -> Result<Value, Halt> {
    match (op, v) {
        (UnOp::Neg, Value::Int(i)) => i.checked_neg().map(Value::Int).ok_or(Halt::Trap(Fault::Overflow)),
        (UnOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
        (UnOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
        (op, v) => Err(Halt::IllFormed(format!("unary {op:?} on {}", v.kind_name()))),
    }
}

fn values_eq(a: &Value, b: &Value) -> bool {
    a == b
}

fn cmp(a: &Value, b: &Value, f: impl Fn(std::cmp::Ordering) -> bool) -> Result<Value, Halt> {
    let ord = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        (Value::Float(x), Value::Float(y)) => {
            x.partial_cmp(y).ok_or_else(|| Halt::IllFormed("NaN comparison".to_string()))?
        }
        (Value::Str(x), Value::Str(y)) => x.cmp(y),
        _ => {
            return Err(Halt::IllFormed(format!(
                "ordered comparison of {} and {}",
                a.kind_name(),
                b.kind_name()
            )));
        }
    };
    Ok(Value::Bool(f(ord)))
}

// ── Printing (the observable) ──────────────────────────────────────────────

/// Format a value for `print` / f-string interpolation.
///
/// Float formatting is provisional (D8 mandates shortest round-trip; Rust's
/// `{}` is shortest-round-trip already, and no phase-0 fixture prints a float —
/// RFC §8.2). Composite values are formatted defensively; the A corpus prints
/// only scalars and strings.
fn format_value(v: &Value) -> String {
    match v {
        Value::Unit => "()".to_string(),
        Value::Int(i) => i.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Float(f) => format!("{f}"),
        Value::Str(s) => s.clone(),
        Value::Vector(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("[{}]", inner.join(", "))
        }
        Value::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("({})", inner.join(", "))
        }
        Value::Struct { name, fields } => {
            let inner: Vec<String> = fields.iter().map(|(n, vv)| format!("{n}: {}", format_value(vv))).collect();
            format!("{name}{{{}}}", inner.join(", "))
        }
    }
}
