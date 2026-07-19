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
    BinOp, BuiltinMethod, CastTarget, ClosureDef, ConstructKind, Expr, ExprArm, FPart, Function,
    Pattern, Program, Source, Stmt, StmtArm, UnOp, Value,
};
use crate::trace::TraceEvent;

// ── Exit codes (deliverable 6) ─────────────────────────────────────────────
// The `T_`-code trap format + exit 101 are normative (D11 trap normalization);
// the code→class→catchable registry is `spec/prose/trap-codes.md`.
/// Program ran to completion with a value.
pub const EXIT_VALUE: i32 = 0;
/// An uncaught trap (§10.9 catchable subset OR an uncatchable panic / unwrap /
/// assert). All trap classes exit 101; the `T_` code discriminates them.
pub const EXIT_TRAP: i32 = 101;
/// A statically-rejected program (a static rejection — parse, elaboration, OR
/// may-move `IllFormed`; ONE class per the ratified TOOLCHAIN EXIT-CODE SCHEME
/// (Option A), decisions.md). It NEVER ran, so stdout is exactly empty and the
/// `E_` reject code goes to stderr — mirroring production `gg check`. Distinct
/// from a runtime trap (101), so a crash can't masquerade as a correct reject.
pub const EXIT_ILLFORMED: i32 = 1;
/// The fuel bound was reached (non-termination guard). ggdef-ONLY / out of
/// conformance: fuel is the definitional interpreter's totality device, not a
/// language outcome an implementation reproduces (production does not bound).
pub const EXIT_FUEL: i32 = 103;

/// The closed registry of trap classes (RFC §2.3 `Trap(TrapKind)`; D11 trap
/// normalization). Every variant's stable `T_<VariantName>` code derives
/// mechanically from its identity (`code()`), mirroring `E_<VariantName>`
/// (`SemanticErrorKind::code`). The §10.9 `Fault` catchable subset is exactly
/// `Overflow | DivByZero | Bounds` (`is_catchable`); the rest
/// (unwrap / assert / panic) are uncatchable.
///
/// A variant's detail payload is for the RENDERED human line only — it is
/// NEVER compared by conformance (Q1: `{T_ code, exit 101}` is the contract,
/// the human detail is impl-defined). The unwrap classes need no payload: the
/// variant identity already fixes their message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrapKind {
    /// `T_Overflow` — an overflowing checked `+`/`-`/`*`/`/`/`%`/unary-neg, a
    /// signed `TYPE_MIN / -1`, or an out-of-range shift count (owner ruling
    /// 2026-07-10: shift-out-of-range normalizes to `T_Overflow`). NOTE: ggdef
    /// does not yet MODEL shift operators (filed: the `trap_shift.gg` fixture
    /// is blocked on that) — this doc pins the CLASS so the registry copies
    /// agree; the evaluator arm lands with shift support.
    Overflow,
    /// `T_DivByZero` — a `/` or `%` with a zero divisor.
    DivByZero,
    /// `T_Bounds` — an out-of-bounds index.
    Bounds,
    /// `T_UnwrapNone` — `.unwrap()` on a `None`.
    UnwrapNone,
    /// `T_UnwrapError` — `.unwrap()` on an `Error`.
    UnwrapError,
    /// `T_UnwrapErrorOnOk` — `.unwrap_error()` on an `Ok`.
    UnwrapErrorOnOk,
    /// `T_AssertFailed` — a failing `assert`. Detail = the message if present,
    /// else `"assertion failed"`.
    AssertFailed(String),
    /// `T_Panic` — an explicit `panic(msg)`. Detail = the user message.
    Panic(String),
}

impl TrapKind {
    /// The stable `T_<VariantName>` code — an exhaustive, catch-all-free match
    /// so `rustc`'s exhaustiveness check IS the registry ratchet (mirrors
    /// `SemanticErrorKind::code`, `src/semantic/errors.rs`). Derives from the
    /// variant identity alone, never the detail payload.
    pub fn code(&self) -> &'static str {
        match self {
            TrapKind::Overflow => "T_Overflow",
            TrapKind::DivByZero => "T_DivByZero",
            TrapKind::Bounds => "T_Bounds",
            TrapKind::UnwrapNone => "T_UnwrapNone",
            TrapKind::UnwrapError => "T_UnwrapError",
            TrapKind::UnwrapErrorOnOk => "T_UnwrapErrorOnOk",
            TrapKind::AssertFailed(_) => "T_AssertFailed",
            TrapKind::Panic(_) => "T_Panic",
        }
    }

    /// The §10.9 `Fault` catchable subset — a fault `catch` may recover exactly
    /// these; the rest panic uncatchably. A PURE registry accessor (ggdef models
    /// no `catch`): its consumers are the §10.9 prose subset and the T2a parity
    /// lint.
    pub fn is_catchable(&self) -> bool {
        matches!(self, TrapKind::Overflow | TrapKind::DivByZero | TrapKind::Bounds)
    }

    /// The human-readable detail for the rendered `trap[T_X]: <detail>` line.
    /// NEVER compared by conformance (Q1) — impl-defined.
    pub fn message(&self) -> &str {
        match self {
            TrapKind::Overflow => "arithmetic overflow",
            TrapKind::DivByZero => "division by zero",
            TrapKind::Bounds => "index out of bounds",
            TrapKind::UnwrapNone => "called `unwrap()` on a `None` value",
            TrapKind::UnwrapError => "called `unwrap()` on an `Error` value",
            TrapKind::UnwrapErrorOnOk => "called `unwrap_error()` on an `Ok` value",
            TrapKind::AssertFailed(m) => m,
            TrapKind::Panic(m) => m,
        }
    }
}

/// The four total outcomes of evaluation (RFC §2.3).
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Value(Value),
    Trap(TrapKind),
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
    Trap(TrapKind),
    IllFormed(String),
    FuelExhausted,
    /// Error-propagation from an `Expr::Propagate` (`throws`-call auto-prop /
    /// the `?` operator): unwind the CURRENT function activation and make it
    /// return this `Error(e)` `Result` value. Caught at the `call_function`
    /// boundary (NOT a terminal outcome) — it is function-local control flow
    /// that just happens to originate deep inside expression evaluation, which
    /// `Flow` (statement-level) cannot express.
    Propagate(Value),
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
    /// The i-th payload slot of an `Enum` value (a `match` pattern binding).
    Payload(usize),
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
    /// `type-name → drop-fn index` for `equip T with Drop` custom drops.
    drop_fns: HashMap<&'a str, usize>,
}

impl<'a> Ctx<'a> {
    fn new(prog: &'a Program) -> Self {
        let funcs: HashMap<&'a str, usize> =
            prog.functions.iter().enumerate().map(|(i, f)| (f.name.as_str(), i)).collect();
        let structs = prog.structs.iter().enumerate().map(|(i, s)| (s.name.as_str(), i)).collect();
        let drop_fns = prog
            .drop_fns
            .iter()
            .filter_map(|(ty, fname)| funcs.get(fname.as_str()).map(|&idx| (ty.as_str(), idx)))
            .collect();
        Ctx { prog, funcs, structs, drop_fns }
    }

    fn struct_fields(&self, name: &str) -> Option<&'a [String]> {
        self.structs.get(name).map(|&i| self.prog.structs[i].fields.as_slice())
    }

    fn closure(&self, idx: usize) -> &'a ClosureDef {
        &self.prog.closures[idx]
    }
}

/// The result of a whole run: the outcome, the observable stdout, and the
/// full provenance trace (present on ALL outcomes, per RFC §2.3).
pub struct Run {
    pub outcome: Outcome,
    pub stdout: String,
    pub trace: Vec<TraceEvent>,
    /// The `cur_span` of the trapping statement when `outcome` is a Trap —
    /// RUN PROVENANCE for the rendered ` at file:line:col` suffix (trap-codes.md
    /// "Rendering"), deliberately NOT part of `Outcome` identity: conformance
    /// compares `{T_ code, exit 101}` only (Q1), and two runs trapping on
    /// different lines are the SAME outcome. Statement-granular (the enclosing
    /// statement, not the faulting sub-expression) — an impl-defined detail.
    pub trap_span: Option<Span>,
    /// The ratified `E_` reject code when `outcome` is a static-rejection
    /// `IllFormed` from the may-move gate (`MoveErrorKind::code()`), `None`
    /// otherwise. TYPED METADATA resolved ONCE at the liveness gate and carried
    /// here — the conformance-compared axis of a rejection (pin 3). Deliberately
    /// on `Run`, NOT on `Outcome` (exactly like `trap_span`): conformance keys on
    /// the outcome KIND + this code, and two rejects differing only in prose/span
    /// are the SAME outcome. The eval-internal `IllFormed` cases (no `main`,
    /// unresolved local) carry no ratified code → `None`.
    pub reject_code: Option<&'static str>,
    /// The statement span of a static-rejection `IllFormed`, for the rendered
    /// ` at file:line:col` suffix (mirrors `trap_span`; impl-defined, never
    /// conformance-compared). `None` when the outcome is not a gate rejection or
    /// the violation had no enclosing statement.
    pub illformed_span: Option<Span>,
}

/// Evaluate a program from `main`, bounded by `fuel`.
pub fn run(program: &Program, fuel: u64) -> Run {
    // D29 (visible error propagation): a bare fallible call — unmarked,
    // uncaptured, unhandled — is a ratified static rejection the elaborator
    // recorded as TYPED metadata. Surface it here FIRST (before the may-move
    // gate and eval), on the SAME `IllFormed` + `reject_code` channel the
    // liveness gate uses, so the conformance lane compares the ratified
    // `E_MissingFallibleMark` code. The program never executes → stdout is
    // exactly empty (the verdict IS the empty output).
    if let Some(rej) = &program.d29_reject {
        return Run {
            outcome: Outcome::IllFormed(rej.message.clone()),
            stdout: String::new(),
            trace: Vec::new(),
            trap_span: None,
            reject_code: Some(crate::ggc::D29Reject::CODE),
            illformed_span: Some(rej.span),
        };
    }
    // The STATIC gate (verdict = check_liveness ∘ eval): a flow-sensitive
    // may-move analysis rejects the conditional-move-then-use / double-move /
    // move-in-loop class BEFORE eval runs, so those programs never reach the
    // dynamic per-path interpreter. A statically ill-formed program is rejected
    // with NO observable output (it never executes) — matching production
    // `gg check`, which never runs a rejected program.
    if let Err(err) = crate::elaborate::check_liveness(program) {
        // Verdict-triple render channels (decisions.md THE VERDICT TRIPLE):
        // stdout stays EXACTLY empty (the program never executed — that IS the
        // verdict); the ratified `E_` code + statement span ride on `Run` for the
        // stderr `error[E_Code]: … at span` render and the conformance compare.
        return Run {
            outcome: Outcome::IllFormed(err.message),
            stdout: String::new(),
            trace: Vec::new(),
            trap_span: None,
            reject_code: Some(err.kind.code()),
            illformed_span: err.span,
        };
    }
    // Gate passed → the pure dynamic per-path interpreter (the `eval` half of
    // `verdict = check_liveness ∘ eval`).
    eval_program(program, fuel)
}

/// The `eval` half of `verdict = check_liveness ∘ eval`: the pure dynamic
/// per-path interpreter WITHOUT the static may-move gate. `run` composes it
/// AFTER `check_liveness`. Exposed at crate scope so the transition-table tests
/// can pin eval's PER-PATH verdict (e.g. a conditional-move-then-use where the
/// untaken arm's read is dynamically a Value) — the verdict `run`'s gate shadows
/// by rejecting the whole program up front. eval's own `IllFormed` (a read of a
/// moved slot on the TAKEN path) remains defense-in-depth (RFC §2.3).
pub(crate) fn eval_program(program: &Program, fuel: u64) -> Run {
    let ctx = Ctx::new(program);
    let Some(&main_idx) = ctx.funcs.get("main") else {
        return Run {
            outcome: Outcome::IllFormed("no `main` function".to_string()),
            stdout: String::new(),
            trace: Vec::new(),
            trap_span: None,
            reject_code: None,
            illformed_span: None,
        };
    };
    let mut state = State::new(fuel);
    let outcome = match call_function(&ctx, &mut state, main_idx, Vec::new()) {
        Ok(_) => Outcome::Value(Value::Unit),
        Err(Halt::Trap(f)) => Outcome::Trap(f),
        Err(Halt::IllFormed(m)) => Outcome::IllFormed(m),
        Err(Halt::FuelExhausted) => Outcome::FuelExhausted,
        // A propagate that escapes `main` means the elaborator emitted an
        // `Expr::Propagate` outside any `throws` function — a spec bug, surfaced
        // loudly rather than silently swallowed.
        Err(Halt::Propagate(_)) => {
            Outcome::IllFormed("error-propagation (`?`) outside a `throws` function".to_string())
        }
    };
    // Trap provenance: the span of the statement that was executing when the
    // trap unwound (dummy = never stamped → no location to report).
    let trap_span = match &outcome {
        Outcome::Trap(_) if state.cur_span != Span::dummy() => Some(state.cur_span),
        _ => None,
    };
    Run {
        outcome,
        stdout: state.stdout,
        trace: state.trace,
        trap_span,
        // Dynamic eval never produces a gate rejection (the gate ran first);
        // any eval-internal `IllFormed` here carries no ratified code.
        reject_code: None,
        illformed_span: None,
    }
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
    // A `Halt::Propagate` unwinding out of an `Expr::Propagate` deep in this
    // function's body is caught HERE (the function boundary) and turned into a
    // return of the `Error(e)` value — the `throws`-call auto-propagate / `?`.
    let mut propagated: Option<Value> = None;
    for stmt in &body {
        match exec_stmt(ctx, state, stmt) {
            Ok(f) => {
                flow = f;
                if !matches!(flow, Flow::Normal) {
                    break;
                }
            }
            Err(Halt::Propagate(v)) => {
                propagated = Some(v);
                break;
            }
            Err(h) => return Err(h),
        }
    }
    let ret = match propagated {
        Some(v) => v,
        None => match flow {
            Flow::Return(v) => v,
            Flow::Normal => Value::Unit,
            // break/continue reaching a function top would be ill-formed; the
            // elaborator only emits them inside loops, so this is defensive.
            Flow::Break | Flow::Continue => {
                return Err(Halt::IllFormed("break/continue outside a loop".to_string()));
            }
        },
    };
    drop_scope(ctx, state, 0)?;
    state.frames.pop();
    Ok(ret)
}

/// Drop every local from `mark`.. in reverse declaration order (RFC §2.1),
/// emitting a `Drop` event for each owned, droppable value and RUNNING its
/// custom `Drop` body (D4) when its type has one. Scalars are `Copy` and do not
/// drop. Threads `Ctx` and returns `Result` because a custom drop is arbitrary
/// code — it can Trap, recurse, or exhaust fuel.
///
/// Locals are popped ONE AT A TIME (not bulk-truncated) so a custom drop, which
/// pushes its own frame and may itself drop values, never re-observes the local
/// being dropped.
fn drop_scope(ctx: &Ctx, state: &mut State, mark: usize) -> Result<(), Halt> {
    loop {
        let frame = state.cur_frame();
        if state.frames[frame].locals.len() <= mark {
            break;
        }
        let local = state.frames[frame].locals.pop().expect("locals.len() > mark");
        if let Slot::Owned(v) = local.slot {
            if is_droppable(&v) {
                // Drop provenance is the binding's declaration site.
                state.trace.push(TraceEvent::Drop { place: local.name.clone(), span: local.span });
                run_custom_drop(ctx, state, v, local.span)?;
            }
        }
    }
    Ok(())
}

/// If `val`'s type has an `equip T with Drop` custom drop, run it with `val`
/// moved in as `self` (D4: `drop(!self)`). `self` is killed before the drop
/// body's own scope exits, so a value's custom drop never recurses on itself
/// (phase-0 tainted types have scalar/loop-free fields, so transitive
/// field-drop is a no-op and is left to phase 1). Body-declared locals inside
/// the drop DO drop transitively (they route back through `drop_scope`).
fn run_custom_drop(ctx: &Ctx, state: &mut State, val: Value, span: Span) -> Result<(), Halt> {
    let tyname = match &val {
        Value::Struct { name, .. } => name.clone(),
        Value::Enum { type_name, .. } => type_name.clone(),
        _ => return Ok(()),
    };
    let Some(&fn_idx) = ctx.drop_fns.get(tyname.as_str()) else {
        return Ok(());
    };
    state.tick()?;
    let func: &Function = &ctx.prog.functions[fn_idx];
    let self_span = func.params.first().map(|p| p.span).unwrap_or(span);
    let body = func.body.clone();
    state.frames.push(Frame {
        locals: vec![Local { name: "self".to_string(), slot: Slot::Owned(val), span: self_span }],
    });
    // Run the drop body; a Halt (trap / fuel) propagates AFTER we unwind the
    // frame stack via `?` at the caller (state is discarded on any Halt).
    let mut body_result: Result<(), Halt> = Ok(());
    for stmt in &body {
        match exec_stmt(ctx, state, stmt) {
            Ok(Flow::Normal) => {}
            // `return`/`break`/`continue` in a drop body just ends it.
            Ok(_) => break,
            Err(h) => {
                body_result = Err(h);
                break;
            }
        }
    }
    // Kill `self` so the drop body's scope exit does NOT re-invoke this custom
    // drop, then drop the body's own locals (transitive). On a Halt we skip the
    // remaining drops (consistent with Halt short-circuiting elsewhere).
    let frame = state.cur_frame();
    if let Some(slot0) = state.frames[frame].locals.first_mut() {
        slot0.slot = Slot::Moved;
    }
    body_result?;
    drop_scope(ctx, state, 0)?;
    state.frames.pop();
    Ok(())
}

/// Non-scalar values carry a (possibly custom, in B2) drop. Scalars are
/// `Copy`. A closure's captured environment carries drops too, but no phase-0
/// fixture observes a closure drop, so it is treated uniformly here.
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
    drop_scope(ctx, state, mark)?;
    Ok(flow)
}

fn exec_stmt(ctx: &Ctx, state: &mut State, stmt: &Stmt) -> Result<Flow, Halt> {
    state.tick()?;
    match stmt {
        Stmt::Bind { name, source, span } => {
            state.cur_span = *span;
            let slot = eval_source_to_slot(ctx, state, source, *span)?;
            emit_fresh_temp_move(state, source, &slot, name, *span);
            let frame = state.cur_frame();
            state.frames[frame].locals.push(Local { name: name.clone(), slot, span: *span });
            Ok(Flow::Normal)
        }
        Stmt::With { name, source, body, span } => {
            state.cur_span = *span;
            // The resource lives in a fresh scope: bind it, run the body, then
            // drop the body's locals AND the resource (reverse declaration
            // order ⇒ the resource, bound first, drops LAST — RFC §2.6).
            let mark = state.frames[state.cur_frame()].locals.len();
            let slot = eval_source_to_slot(ctx, state, source, *span)?;
            emit_fresh_temp_move(state, source, &slot, name, *span);
            let frame = state.cur_frame();
            state.frames[frame].locals.push(Local { name: name.clone(), slot, span: *span });
            let mut flow = Flow::Normal;
            for stmt in body {
                flow = exec_stmt(ctx, state, stmt)?;
                if !matches!(flow, Flow::Normal) {
                    break;
                }
            }
            drop_scope(ctx, state, mark)?;
            Ok(flow)
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
        Stmt::Match { scrutinee, arms, else_arm, span } => {
            state.cur_span = *span;
            exec_match_stmt(ctx, state, scrutinee, arms, else_arm.as_deref())
        }
        Stmt::Return { value, span } => {
            state.cur_span = *span;
            let v = match value {
                Some(e) => eval_expr(ctx, state, e)?,
                None => Value::Unit,
            };
            Ok(Flow::Return(v))
        }
        Stmt::Assert { cond, message, span } => {
            state.cur_span = *span;
            if eval_bool(ctx, state, cond)? {
                Ok(Flow::Normal)
            } else {
                // The message is evaluated ONLY on failure (short-circuit).
                let detail = match message {
                    Some(m) => format_value(&eval_expr(ctx, state, m)?),
                    None => "assertion failed".to_string(),
                };
                Err(Halt::Trap(TrapKind::AssertFailed(detail)))
            }
        }
        Stmt::Break { .. } => Ok(Flow::Break),
        Stmt::Continue { .. } => Ok(Flow::Continue),
    }
}

// ── Sources (the copy/move/borrow decision, realised) ──────────────────────

/// (F2) A fresh-temp bind (`S a = S(..)`, `with Res(1) as r`) is a STRUCTURAL
/// MOVE of the temp into the binding — never a live-place copy — so it emits a
/// `Move` provenance event (whose `from` names the destination, per the trace
/// doc) rather than a `BindCopy`. Scalars are `Copy`, so only droppable values
/// carry the event (matching the copy/move axis that structural moves live on).
fn emit_fresh_temp_move(state: &mut State, source: &Source, slot: &Slot, name: &str, span: Span) {
    if matches!(source, Source::Value(_)) {
        if let Slot::Owned(v) = slot {
            if is_droppable(v) {
                state.trace.push(TraceEvent::Move { from: name.to_string(), span });
            }
        }
    }
}

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
        // A borrow/write-through source in a value position is a READ (a
        // copy-out of the aliased place) — this reaches here only from the
        // `print(...)`-as-expression / closure-argument paths, where the arg is
        // formatted/consumed by value.
        Source::BorrowView(place) | Source::WriteThrough(place) => {
            let p = eval_place(ctx, state, place)?;
            resolve_read(ctx, state, &p)
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
        // Auto-borrow-from-get: `coll.get(i).unwrap()` is the place `coll[i]`
        // (`.get()` → `Option[Ref[T]]`, `.unwrap()` → the `Ref[T]` borrow). An
        // out-of-bounds `.get()` is `None`, so `.unwrap()` traps `T_UnwrapNone` —
        // resolved HERE so the write-through place path keeps the ratified trap
        // identity (NOT the `[i]` bounds trap). Phase-0 Vector element only: a
        // Dict entry-by-key is not expressible as a `Proj` (its chain stays
        // out-of-subset / value-copy).
        Expr::Method { recv, method: BuiltinMethod::Unwrap, args }
            if args.is_empty()
                && matches!(&**recv, Expr::Method { method: BuiltinMethod::Get, .. }) =>
        {
            let Expr::Method { recv: coll, args: get_args, .. } = &**recv else {
                unreachable!("guarded by the matches! above")
            };
            let idx_src = get_args.first().ok_or_else(|| illf("`.get()` needs an index"))?;
            let i = as_index(&eval_source_to_value(ctx, state, idx_src, state.cur_span)?)?;
            let base = eval_place(ctx, state, coll)?;
            match resolve_read(ctx, state, &base)? {
                Value::Vector(items) => {
                    if i >= items.len() {
                        return Err(Halt::Trap(TrapKind::UnwrapNone));
                    }
                    Ok(base.extend(&[Proj::Index(i)]))
                }
                other => Err(Halt::IllFormed(format!(
                    "`.get().unwrap()` write-through place on {} is outside the phase-0 subset",
                    other.kind_name()
                ))),
            }
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
        Value::Int(_) => Err(Halt::Trap(TrapKind::Bounds)),
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
        Revive,
    }
    let action = match &state.frames[place.frame].locals[place.local].slot {
        Slot::Owned(_) => Action::WriteOwned,
        Slot::WriteThrough(t) => Action::Follow(t.clone()),
        Slot::BorrowView(t) => {
            Action::Materialize(t.clone(), state.frames[place.frame].locals[place.local].name.clone())
        }
        Slot::Moved => {
            // Re-init makes live: a WHOLE-LOCAL reassignment REVIVES a moved-out
            // slot (it rebinds a fresh owned value), mirroring production's
            // `mark_live` on a whole rebind (`check_stmt.rs` `mark_live`; ratified
            // "move-bind kills, one live name after" — decisions.md D10(a)). A
            // PROJECTED write (`x.f = …`) to a moved root is a genuine
            // use-after-move — the root value is gone, so there is no place to
            // write into; that stays `IllFormed`.
            if place.proj.is_empty() {
                Action::Revive
            } else {
                return Err(Halt::IllFormed(format!(
                    "write to moved-out value `{}`",
                    state.frames[place.frame].locals[place.local].name
                )));
            }
        }
    };
    match action {
        Action::Revive => {
            // Whole-local re-init: the slot becomes freshly owned again.
            state.frames[place.frame].locals[place.local].slot = Slot::Owned(newval);
            Ok(())
        }
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
    for (k, p) in proj.iter().enumerate() {
        cur = match (cur, p) {
            (Value::Struct { fields, .. }, Proj::Field(name)) => fields
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, vv)| vv)
                .ok_or_else(|| Halt::IllFormed(format!("no field `{name}`")))?,
            // A Set is insertion-ordered, so element iteration (`for x in s`
            // desugars to `s[__i]`) reads by position exactly like a Vector.
            (Value::Vector(items), Proj::Index(i))
            | (Value::Tuple(items), Proj::Index(i))
            | (Value::Set(items), Proj::Index(i)) => {
                items.get(*i).ok_or(Halt::Trap(TrapKind::Bounds))?
            }
            (Value::Enum { payload, .. }, Proj::Payload(i)) => {
                payload.get(*i).ok_or_else(|| Halt::IllFormed("bad payload projection".to_string()))?
            }
            // A String codepoint read (`s[i]`, `for c in s`): fresh 1-char
            // value, so it terminates the projection walk (a `str` is not a
            // place you can project further into).
            (Value::Str(s), Proj::Index(i)) => {
                let ch = str_codepoint(s, *i).ok_or(Halt::Trap(TrapKind::Bounds))?;
                return navigate_read(&Value::Str(ch), &proj[k + 1..]);
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

/// The i-th codepoint of `s` as a fresh 1-char `String`, or `None` if out of
/// range (negative or past the end).
fn str_codepoint(s: &str, i: usize) -> Option<String> {
    s.chars().nth(i).map(|c| c.to_string())
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
                    items.get_mut(*i).ok_or(Halt::Trap(TrapKind::Bounds))?
                }
                (Value::Enum { payload, .. }, Proj::Payload(i)) => {
                    payload.get_mut(*i).ok_or_else(|| Halt::IllFormed("bad payload projection".to_string()))?
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
        Expr::Unit => Ok(Value::Unit),
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

        Expr::Panic(msg) => {
            // `panic(msg)` is noreturn: it unwinds as an uncatchable trap and
            // never yields a value. The message is rendered as the human detail.
            let v = eval_expr(ctx, state, msg)?;
            Err(Halt::Trap(TrapKind::Panic(format_value(&v))))
        }

        Expr::Method { recv, method, args } => eval_method(ctx, state, recv, *method, args),

        Expr::Clone(inner) => {
            // `resolve_read` already deep-copies, so the result is independent.
            let v = eval_expr(ctx, state, inner)?;
            state.trace.push(TraceEvent::ExplicitClone { place: inner.place_str(), span: state.cur_span });
            Ok(v)
        }

        Expr::Slice { object, start, end, inclusive } => {
            let base = eval_recv_value(ctx, state, object)?;
            let n = seq_len(&base)?;
            let a = match start {
                Some(e) => as_index(&eval_expr(ctx, state, e)?)?,
                None => 0,
            };
            let mut b = match end {
                Some(e) => as_index(&eval_expr(ctx, state, e)?)?,
                None => n,
            };
            if *inclusive {
                b += 1;
            }
            match base {
                Value::Str(s) => Ok(Value::Str(str_slice(&s, a, b))),
                Value::Vector(items) => {
                    let a = a.min(items.len());
                    let b = b.min(items.len()).max(a);
                    Ok(Value::Vector(items[a..b].to_vec()))
                }
                other => Err(Halt::IllFormed(format!("slice of {}", other.kind_name()))),
            }
        }

        Expr::Cast { expr, target } => {
            let v = eval_expr(ctx, state, expr)?;
            eval_cast(v, *target)
        }

        Expr::EnumConstruct { type_name, variant, args } => {
            let mut payload = Vec::with_capacity(args.len());
            for a in args {
                payload.push(eval_source_to_value(ctx, state, a, state.cur_span)?);
            }
            Ok(Value::Enum { type_name: type_name.clone(), variant: variant.clone(), payload })
        }

        Expr::Closure(idx) => {
            // Capture-by-value at creation (D5): snapshot each free variable
            // from the current frame into the closure's environment record.
            let def: &ClosureDef = ctx.closure(*idx);
            let mut captured = Vec::with_capacity(def.captures.len());
            for name in &def.captures {
                let p = eval_place(ctx, state, &Expr::Local(name.clone()))?;
                let v = resolve_read(ctx, state, &p)?;
                captured.push((name.clone(), v));
            }
            Ok(Value::Closure { def: *idx, captured })
        }

        Expr::CallValue { callee, args, consumes_callee } => {
            let callee_val = eval_expr(ctx, state, callee)?;
            // A `ConsumeCallable` is single-owner (D5 kind axis): the call
            // CONSUMES the callee. Kill its slot (BEFORE the args, matching the
            // ratified step-order where the receiver/callee is consumed first)
            // so a second call reads a moved-out slot → `IllFormed`, mirroring
            // production `check_move` on a ConsumeCallable call. A plain
            // `Callable` leaves `consumes_callee` false and stays reusable.
            if *consumes_callee {
                let p = eval_place(ctx, state, callee)?;
                kill_place(state, &p);
            }
            let (def_idx, captured) = match callee_val {
                Value::Closure { def, captured } => (def, captured),
                other => {
                    return Err(Halt::IllFormed(format!(
                        "call of non-closure value ({})",
                        other.kind_name()
                    )));
                }
            };
            let mut slots = Vec::with_capacity(args.len());
            for a in args {
                slots.push(eval_source_to_slot(ctx, state, a, state.cur_span)?);
            }
            call_closure(ctx, state, def_idx, captured, slots)
        }

        Expr::Match { scrutinee, arms, else_arm, span } => {
            state.cur_span = *span;
            eval_match_expr(ctx, state, scrutinee, arms, else_arm.as_deref())
        }

        Expr::IntToStr(inner) => {
            let v = eval_expr(ctx, state, inner)?;
            match v {
                Value::Int(i) => Ok(Value::Str(i.to_string())),
                other => Err(Halt::IllFormed(format!("`int_to_str` on {}", other.kind_name()))),
            }
        }

        Expr::Propagate(inner) => {
            // `?` / `throws`-call auto-propagate: the inner MUST be a `Result`.
            // `Ok(x)` yields `x`; `Error(e)` unwinds to the enclosing `throws`
            // function's boundary (caught in `call_function`) as its return.
            let v = eval_expr(ctx, state, inner)?;
            match v {
                Value::Enum { type_name, variant, mut payload }
                    if type_name == "Result" && variant == "Ok" =>
                {
                    // `Ok(x)` — a well-formed Result carries exactly one payload.
                    Ok(payload.pop().unwrap_or(Value::Unit))
                }
                v @ Value::Enum { .. } if is_result_error(&v) => Err(Halt::Propagate(v)),
                other => Err(Halt::IllFormed(format!(
                    "error-propagation of a non-Result value ({})",
                    other.kind_name()
                ))),
            }
        }
    }
}

/// Whether `v` is a `Result.Error(_)` value (the propagate-early-return case).
fn is_result_error(v: &Value) -> bool {
    matches!(v, Value::Enum { type_name, variant, .. } if type_name == "Result" && variant == "Error")
}

/// The element/codepoint count of a sliceable value.
fn seq_len(v: &Value) -> Result<usize, Halt> {
    match v {
        Value::Str(s) => Ok(s.chars().count()),
        Value::Vector(items) => Ok(items.len()),
        other => Err(Halt::IllFormed(format!("slice of {}", other.kind_name()))),
    }
}

/// A closure call: push a frame holding a fresh copy of the captured
/// environment (per-call — a bare closure's write is to its own copy, D5) plus
/// the argument slots, then evaluate the single-expression body.
fn call_closure(
    ctx: &Ctx,
    state: &mut State,
    def_idx: usize,
    captured: Vec<(String, Value)>,
    param_slots: Vec<Slot>,
) -> Result<Value, Halt> {
    state.tick()?;
    let def: &ClosureDef = ctx.closure(def_idx);
    let body = def.body.clone();
    let span = def.span;
    let mut locals = Vec::new();
    for (name, v) in captured {
        locals.push(Local { name, slot: Slot::Owned(v), span });
    }
    for (param, slot) in def.params.iter().zip(param_slots) {
        locals.push(Local { name: param.name.clone(), slot, span: param.span });
    }
    state.frames.push(Frame { locals });
    let result = eval_expr(ctx, state, &body)?;
    drop_scope(ctx, state, 0)?;
    state.frames.pop();
    Ok(result)
}

/// The `as`-cast rules (RFC §2.1): float→int **saturates** (the ratified
/// 2026-04-24 both-backend fix), int→int truncates/wraps (two's-complement),
/// widening/int→float is exact-ish. Unit-tested only — no phase-0 fixture.
fn eval_cast(v: Value, target: CastTarget) -> Result<Value, Halt> {
    match (v, target) {
        (Value::Float(f), CastTarget::Int { bits, signed }) => {
            Ok(Value::Int(saturate_float_to_int(f, bits, signed)))
        }
        (Value::Int(i), CastTarget::Int { bits, signed }) => {
            Ok(Value::Int(wrap_int(i, bits, signed)))
        }
        (Value::Int(i), CastTarget::Float32) => Ok(Value::Float(i as f32 as f64)),
        (Value::Int(i), CastTarget::Float64) => Ok(Value::Float(i as f64)),
        (Value::Float(f), CastTarget::Float32) => Ok(Value::Float(f as f32 as f64)),
        (Value::Float(f), CastTarget::Float64) => Ok(Value::Float(f)),
        (other, _) => Err(Halt::IllFormed(format!("cannot cast {}", other.kind_name()))),
    }
}

/// The inclusive range `[min, max]` of a `bits`-wide integer of the sign.
fn int_range(bits: u32, signed: bool) -> (i128, i128) {
    if signed {
        let m = 1i128 << (bits - 1);
        (-m, m - 1)
    } else {
        (0, (1i128 << bits) - 1)
    }
}

/// Saturating float→int (NaN→0), matching the ratified both-backend rule.
fn saturate_float_to_int(f: f64, bits: u32, signed: bool) -> i64 {
    let (lo, hi) = int_range(bits, signed);
    if f.is_nan() {
        return 0;
    }
    let r = f.trunc();
    if r <= lo as f64 {
        lo as i64
    } else if r >= hi as f64 {
        hi as i64
    } else {
        r as i64
    }
}

/// Two's-complement int→int narrowing (Rust `as` semantics), kept in the i64
/// value domain.
fn wrap_int(i: i64, bits: u32, signed: bool) -> i64 {
    if bits >= 64 {
        return i;
    }
    let mask = (1i128 << bits) - 1;
    let m = (i as i128) & mask;
    if signed && (m & (1i128 << (bits - 1))) != 0 {
        (m - (1i128 << bits)) as i64
    } else {
        m as i64
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
        ConstructKind::Dict => {
            // Phase-0 fixtures construct empty dicts (`Dict[K,V]()`); values are
            // populated via `.set()`. A dict literal desugars to pushes too.
            let mut entries = Vec::with_capacity(args.len());
            for a in args {
                if let Value::Tuple(mut kv) = eval_source_to_value(ctx, state, a, state.cur_span)? {
                    if kv.len() == 2 {
                        let v = kv.pop().unwrap();
                        let k = kv.pop().unwrap();
                        entries.push((k, v));
                        continue;
                    }
                }
                return Err(Halt::IllFormed("dict entry must be a (key, value) pair".to_string()));
            }
            Ok(Value::Dict(entries))
        }
        ConstructKind::Set => {
            let mut items: Vec<Value> = Vec::with_capacity(args.len());
            for a in args {
                let v = eval_source_to_value(ctx, state, a, state.cur_span)?;
                if !items.contains(&v) {
                    items.push(v);
                }
            }
            Ok(Value::Set(items))
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
    use BuiltinMethod as M;
    match method {
        // ── Read-only methods: operate on the receiver's VALUE, so they work
        //    on both places (`v.get(0)`) and temps (`v.get(0).unwrap()`). ──
        M::Len => {
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Vector(items) | Value::Set(items) => Ok(Value::Int(items.len() as i64)),
                Value::Dict(entries) => Ok(Value::Int(entries.len() as i64)),
                Value::Str(s) => Ok(Value::Int(s.chars().count() as i64)),
                other => Err(Halt::IllFormed(format!("`.len()` on {}", other.kind_name()))),
            }
        }
        M::Get => {
            let key = eval_source_to_value(ctx, state, arg_at(args, 0)?, state.cur_span)?;
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Vector(items) => {
                    let i = as_index(&key)?;
                    Ok(option_of(items.into_iter().nth(i)))
                }
                Value::Dict(entries) => {
                    Ok(option_of(entries.into_iter().find(|(k, _)| k == &key).map(|(_, val)| val)))
                }
                other => Err(Halt::IllFormed(format!("`.get()` on {}", other.kind_name()))),
            }
        }
        M::Unwrap => {
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Enum { variant, mut payload, .. } if variant == "Some" || variant == "Ok" => {
                    Ok(payload.pop().unwrap_or(Value::Unit))
                }
                // Split by receiver so each unwrap trap gets its own `T_` code
                // (the code derives from variant identity, so a single `Panic`
                // arm would collapse `T_UnwrapNone`/`T_UnwrapError` together).
                Value::Enum { variant, .. } if variant == "None" => {
                    Err(Halt::Trap(TrapKind::UnwrapNone))
                }
                Value::Enum { variant, .. } if variant == "Error" => {
                    Err(Halt::Trap(TrapKind::UnwrapError))
                }
                other => Err(Halt::IllFormed(format!("`.unwrap()` on {}", other.kind_name()))),
            }
        }
        M::UnwrapError => {
            // The dual of `.unwrap()`: extract the `Error` payload; Trap on `Ok`.
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Enum { variant, mut payload, .. } if variant == "Error" => {
                    Ok(payload.pop().unwrap_or(Value::Unit))
                }
                Value::Enum { variant, .. } if variant == "Ok" => {
                    Err(Halt::Trap(TrapKind::UnwrapErrorOnOk))
                }
                other => Err(Halt::IllFormed(format!("`.unwrap_error()` on {}", other.kind_name()))),
            }
        }
        M::UnwrapOr => {
            let default = eval_source_to_value(ctx, state, arg_at(args, 0)?, state.cur_span)?;
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Enum { variant, mut payload, .. } if variant == "Some" || variant == "Ok" => {
                    Ok(payload.pop().unwrap_or(default))
                }
                Value::Enum { .. } => Ok(default),
                other => Err(Halt::IllFormed(format!("`.unwrap_or()` on {}", other.kind_name()))),
            }
        }
        M::Trim => {
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Str(s) => Ok(Value::Str(s.trim().to_string())),
                other => Err(Halt::IllFormed(format!("`.trim()` on {}", other.kind_name()))),
            }
        }
        M::Substring => {
            let a = as_index(&eval_source_to_value(ctx, state, arg_at(args, 0)?, state.cur_span)?)?;
            let b = as_index(&eval_source_to_value(ctx, state, arg_at(args, 1)?, state.cur_span)?)?;
            let v = eval_recv_value(ctx, state, recv)?;
            match v {
                Value::Str(s) => Ok(Value::Str(str_slice(&s, a, b))),
                other => Err(Halt::IllFormed(format!("`.substring()` on {}", other.kind_name()))),
            }
        }
        // ── Mutating methods: read-modify-write through the receiver place
        //    (materialise-on-write if it is a Borrow binding). On a temp
        //    receiver the mutation is simply discarded (only the return used). ─
        M::Push | M::Set | M::Pop | M::Clear | M::Fill | M::Add => {
            eval_mut_method(ctx, state, recv, method, args)
        }
    }
}

/// Read the receiver's value, following borrows when it is a place and
/// evaluating it as a temp otherwise (so chained calls like
/// `v.get(0).unwrap()` work — the middle receiver is a fresh `Option`).
fn eval_recv_value(ctx: &Ctx, state: &mut State, recv: &Expr) -> Result<Value, Halt> {
    if recv.is_place() {
        let p = eval_place(ctx, state, recv)?;
        resolve_read(ctx, state, &p)
    } else {
        eval_expr(ctx, state, recv)
    }
}

/// A mutating collection method. On a place receiver it reads the current
/// collection, applies the mutation, and writes back through the store (so a
/// bare-param Borrow materialises a private copy on the write). The trace
/// records a `Write`, exactly as a direct assignment would.
fn eval_mut_method(ctx: &Ctx, state: &mut State, recv: &Expr, method: BuiltinMethod, args: &[Source]) -> Result<Value, Halt> {
    // Evaluate arguments to values first (element / index / key / value).
    let mut argvals = Vec::with_capacity(args.len());
    for a in args {
        argvals.push(eval_source_to_value(ctx, state, a, state.cur_span)?);
    }
    let is_place = recv.is_place();
    let (mut coll, place) = if is_place {
        let p = eval_place(ctx, state, recv)?;
        (resolve_read(ctx, state, &p)?, Some(p))
    } else {
        (eval_expr(ctx, state, recv)?, None)
    };
    let result = apply_mut(&mut coll, method, argvals)?;
    if let Some(p) = place {
        resolve_write(ctx, state, &p, coll, state.cur_span)?;
        state.trace.push(TraceEvent::Write { place: recv.place_str(), span: state.cur_span });
    }
    Ok(result)
}

/// Apply a mutating method to a collection value in place, returning the
/// method's own result value.
fn apply_mut(coll: &mut Value, method: BuiltinMethod, argvals: Vec<Value>) -> Result<Value, Halt> {
    use BuiltinMethod as M;
    let mut args = argvals.into_iter();
    match (method, coll) {
        (M::Push, Value::Vector(items)) => {
            items.push(args.next().ok_or_else(|| illf("push expects 1 arg"))?);
            Ok(Value::Unit)
        }
        (M::Push, Value::Str(s)) => {
            // `String.push(x)` appends `x`'s text.
            match args.next().ok_or_else(|| illf("push expects 1 arg"))? {
                Value::Str(x) => s.push_str(&x),
                other => return Err(illf(&format!("`String.push` of {}", other.kind_name()))),
            }
            Ok(Value::Unit)
        }
        (M::Add, Value::Set(items)) => {
            let x = args.next().ok_or_else(|| illf("add expects 1 arg"))?;
            if !items.contains(&x) {
                items.push(x);
            }
            Ok(Value::Unit)
        }
        (M::Set, Value::Vector(items)) => {
            let i = as_index(&args.next().ok_or_else(|| illf("set expects 2 args"))?)?;
            let x = args.next().ok_or_else(|| illf("set expects 2 args"))?;
            if i >= items.len() {
                return Err(Halt::Trap(TrapKind::Bounds));
            }
            items[i] = x;
            Ok(Value::Unit)
        }
        (M::Set, Value::Dict(entries)) => {
            let k = args.next().ok_or_else(|| illf("set expects 2 args"))?;
            let v = args.next().ok_or_else(|| illf("set expects 2 args"))?;
            if let Some(slot) = entries.iter_mut().find(|(ek, _)| ek == &k) {
                slot.1 = v; // update preserves insertion order
            } else {
                entries.push((k, v));
            }
            Ok(Value::Unit)
        }
        (M::Pop, Value::Vector(items)) => Ok(option_of(items.pop())),
        (M::Clear, Value::Vector(items)) => {
            items.clear();
            Ok(Value::Unit)
        }
        (M::Clear, Value::Set(items)) => {
            items.clear();
            Ok(Value::Unit)
        }
        (M::Fill, Value::Vector(items)) => {
            let n = as_index(&args.next().ok_or_else(|| illf("fill expects 2 args"))?)?;
            let x = args.next().ok_or_else(|| illf("fill expects 2 args"))?;
            *items = std::iter::repeat_n(x, n).collect();
            Ok(Value::Unit)
        }
        (m, other) => Err(illf(&format!("`.{m:?}()` on {}", other.kind_name()))),
    }
}

fn illf(m: &str) -> Halt {
    Halt::IllFormed(m.to_string())
}

/// Wrap an optional value in `Option`: `Some(v)` or `None`.
fn option_of(v: Option<Value>) -> Value {
    match v {
        Some(v) => Value::Enum { type_name: "Option".to_string(), variant: "Some".to_string(), payload: vec![v] },
        None => Value::Enum { type_name: "Option".to_string(), variant: "None".to_string(), payload: Vec::new() },
    }
}

/// Interpret a value as a non-negative index; a negative int is out-of-bounds.
fn as_index(v: &Value) -> Result<usize, Halt> {
    match v {
        Value::Int(i) if *i >= 0 => Ok(*i as usize),
        Value::Int(_) => Err(Halt::Trap(TrapKind::Bounds)),
        other => Err(Halt::IllFormed(format!("index must be int, got {}", other.kind_name()))),
    }
}

/// A codepoint slice `s[a..b)` (clamped to the string's codepoint length).
fn str_slice(s: &str, a: usize, b: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    let a = a.min(chars.len());
    let b = b.min(chars.len()).max(a);
    chars[a..b].iter().collect()
}

/// The i-th argument source, or an ill-formed error naming the shortfall.
fn arg_at(args: &[Source], i: usize) -> Result<&Source, Halt> {
    args.get(i).ok_or_else(|| illf("missing method argument"))
}

// ── Match (statement + expression) ─────────────────────────────────────────
//
// A `match` scrutinee is a Borrow-mode view (RFC §2.2): pattern bindings are
// `BorrowView`s into the scrutinee's payload (a new `Proj::Payload`), so
// materialise-on-write applies to them exactly as to a bare param. A non-place
// scrutinee is snapshotted into a synthetic local so the bindings still name a
// stable place. All arm-scope locals (bindings + the synthetic scrutinee) drop
// at the match's end, in reverse declaration order.

fn exec_match_stmt(
    ctx: &Ctx,
    state: &mut State,
    scrutinee: &Expr,
    arms: &[StmtArm],
    else_arm: Option<&[Stmt]>,
) -> Result<Flow, Halt> {
    let mark = state.frames[state.cur_frame()].locals.len();
    let scrut_place = scrutinee_place(ctx, state, scrutinee)?;
    let scrut_val = resolve_read(ctx, state, &scrut_place)?;
    for arm in arms {
        if let Some(binds) = match_pattern(ctx, state, &scrut_val, &arm.pattern)? {
            push_pattern_bindings(state, &scrut_place, binds);
            let flow = exec_block(ctx, state, &arm.body)?;
            drop_scope(ctx, state, mark)?;
            return Ok(flow);
        }
    }
    let flow = match else_arm {
        Some(b) => exec_block(ctx, state, b)?,
        // No arm matched and there is no `else`: the scrutinee's value was not
        // covered. In a well-typed program `match` is exhaustive, so reaching
        // here means the program was statically ill-formed (a non-exhaustive
        // match) — detected dynamically (RFC §2.3 `IllFormed`). This must be
        // LOUD, never a silent fall-through: a silent fall-through is exactly
        // how a `match result: case Ok/case Error` on a mis-typed (throws-
        // dropped) scrutinee produced empty output and a bogus `Value`.
        None => {
            drop_scope(ctx, state, mark)?;
            return Err(Halt::IllFormed(
                "no `match` arm matched the scrutinee (non-exhaustive match)".to_string(),
            ));
        }
    };
    drop_scope(ctx, state, mark)?;
    Ok(flow)
}

fn eval_match_expr(
    ctx: &Ctx,
    state: &mut State,
    scrutinee: &Expr,
    arms: &[ExprArm],
    else_arm: Option<&Expr>,
) -> Result<Value, Halt> {
    let mark = state.frames[state.cur_frame()].locals.len();
    let scrut_place = scrutinee_place(ctx, state, scrutinee)?;
    let scrut_val = resolve_read(ctx, state, &scrut_place)?;
    for arm in arms {
        if let Some(binds) = match_pattern(ctx, state, &scrut_val, &arm.pattern)? {
            push_pattern_bindings(state, &scrut_place, binds);
            let result = eval_expr(ctx, state, &arm.body)?;
            drop_scope(ctx, state, mark)?;
            return Ok(result);
        }
    }
    let result = match else_arm {
        Some(e) => eval_expr(ctx, state, e)?,
        // No arm matched, no `else` — a non-exhaustive match reached at runtime:
        // statically ill-formed, detected dynamically (RFC §2.3). Mirrors the
        // statement-`match` case; both are LOUD, never a silent bogus value.
        None => {
            drop_scope(ctx, state, mark)?;
            return Err(Halt::IllFormed(
                "no `match` arm matched the scrutinee (non-exhaustive match)".to_string(),
            ));
        }
    };
    drop_scope(ctx, state, mark)?;
    Ok(result)
}

/// Resolve the scrutinee to a stable place: its own place if it is one,
/// otherwise a synthetic `Owned` local holding the evaluated value.
fn scrutinee_place(ctx: &Ctx, state: &mut State, scrutinee: &Expr) -> Result<Place, Halt> {
    if scrutinee.is_place() {
        eval_place(ctx, state, scrutinee)
    } else {
        let v = eval_expr(ctx, state, scrutinee)?;
        let frame = state.cur_frame();
        let local = state.frames[frame].locals.len();
        state.frames[frame].locals.push(Local {
            name: "__match".to_string(),
            slot: Slot::Owned(v),
            span: state.cur_span,
        });
        Ok(Place { frame, local, proj: Vec::new() })
    }
}

/// Try to match `val` against `pat`; on success, return the pattern's bindings
/// as `(name, projection-path-relative-to-the-scrutinee)` pairs.
#[allow(clippy::type_complexity)]
fn match_pattern(
    ctx: &Ctx,
    state: &mut State,
    val: &Value,
    pat: &Pattern,
) -> Result<Option<Vec<(String, Vec<Proj>)>>, Halt> {
    match pat {
        Pattern::Wildcard => Ok(Some(Vec::new())),
        Pattern::Binding(name) => Ok(Some(vec![(name.clone(), Vec::new())])),
        Pattern::Literal(e) => {
            let lit = eval_expr(ctx, state, e)?;
            Ok(if values_eq(&lit, val) { Some(Vec::new()) } else { None })
        }
        Pattern::Variant { variant, fields } => match val {
            Value::Enum { variant: v, payload, .. } if v == variant && payload.len() == fields.len() => {
                let mut binds = Vec::new();
                for (i, (fp, fv)) in fields.iter().zip(payload).enumerate() {
                    match match_pattern(ctx, state, fv, fp)? {
                        Some(sub) => {
                            for (n, proj) in sub {
                                let mut full = vec![Proj::Payload(i)];
                                full.extend(proj);
                                binds.push((n, full));
                            }
                        }
                        None => return Ok(None),
                    }
                }
                Ok(Some(binds))
            }
            _ => Ok(None),
        },
    }
}

/// Push each pattern binding as a `BorrowView` into the scrutinee's place.
fn push_pattern_bindings(state: &mut State, scrut_place: &Place, binds: Vec<(String, Vec<Proj>)>) {
    let frame = state.cur_frame();
    let span = state.cur_span;
    for (name, proj) in binds {
        let place = scrut_place.extend(&proj);
        state.frames[frame].locals.push(Local { name, slot: Slot::BorrowView(place), span });
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
            a.checked_add(*b).map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow))
        }
        (BinOp::Sub, Value::Int(a), Value::Int(b)) => {
            a.checked_sub(*b).map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow))
        }
        (BinOp::Mul, Value::Int(a), Value::Int(b)) => {
            a.checked_mul(*b).map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow))
        }
        (BinOp::Div, Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(Halt::Trap(TrapKind::DivByZero))
            } else {
                a.checked_div(*b).map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow))
            }
        }
        (BinOp::Rem, Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(Halt::Trap(TrapKind::DivByZero))
            } else {
                a.checked_rem(*b).map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow))
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
        (UnOp::Neg, Value::Int(i)) => i.checked_neg().map(Value::Int).ok_or(Halt::Trap(TrapKind::Overflow)),
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
        Value::Enum { variant, payload, .. } => {
            if payload.is_empty() {
                variant.clone()
            } else {
                let inner: Vec<String> = payload.iter().map(format_value).collect();
                format!("{variant}({})", inner.join(", "))
            }
        }
        Value::Dict(entries) => {
            let inner: Vec<String> =
                entries.iter().map(|(k, v)| format!("{}: {}", format_value(k), format_value(v))).collect();
            format!("{{{}}}", inner.join(", "))
        }
        Value::Set(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("{{{}}}", inner.join(", "))
        }
        Value::Closure { .. } => "<closure>".to_string(),
    }
}
