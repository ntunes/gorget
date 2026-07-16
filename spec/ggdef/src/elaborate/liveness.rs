//! The static may-move liveness pass (RFC §9.5 branch-merge; mirrors production
//! `src/semantic/safety/origins.rs` + the self-host `check_safety_*` walk).
//!
//! `ggdef`'s `eval` models liveness DYNAMICALLY — one path per run, per the
//! concrete branch conditions. That is the correct *dynamic* semantics, but it
//! cannot reject a program that is memory-safe on its sampled path yet
//! statically ill-formed (`if false: sink(!x); print(x)` — `x` is never moved
//! at runtime, so `eval` says `Value`, but the STATIC gate rejects it). This
//! pass is that static gate: a syntax-directed, flow-sensitive move-tracker that
//! runs BEFORE `eval` and rejects the conditional-move-then-use class with an
//! `IllFormed` verdict (exit 1, the static-rejection code per the ratified
//! toolchain exit-code scheme), so `verdict = check_liveness ∘ eval`.
//!
//! It needs NO fuel and NO path exploration — branches are abstracted by
//! unioning the per-arm moved-sets ("moved in ANY arm ⇒ moved after the join",
//! reference `:2390` §9.5), never executed.
//!
//! ## Why a private scope-stack + binding ids (not names)
//!
//! GGC identifies locals by surface NAME (`Expr::Local(String)`). A name-keyed
//! moved-set COLLIDES across sibling scopes and shadows (two `if`-arm-local `x`
//! bindings, a re-`let` of the same name). Production and the self-host key on
//! `DefId` for exactly this reason. So this pass builds its own lexical resolver
//! — a fresh `BindingId` per binding, resolved innermost-first — which gives
//! scope-exit correctness (an out-of-scope id is simply never resolved again)
//! for free.

use std::collections::{HashMap, HashSet};

use gorget::span::Span;

use crate::ggc::{Expr, Pattern, Program, Source, Stmt};

type Id = u32;

/// The closed registry of ratified may-move rejection diagnostics. Each
/// variant's stable `E_<VariantName>` code derives mechanically from its
/// identity (`code()`), mirroring `TrapKind::code` (`eval.rs`) and production's
/// `SemanticErrorKind::code` (`src/semantic/errors.rs`) — an exhaustive,
/// catch-all-free match so `rustc`'s exhaustiveness check IS the registry
/// ratchet. This is the ONE source of truth for the code; it is produced HERE
/// (at the violation site) and carried as typed metadata to the render + the
/// conformance comparison — NEVER re-parsed out of the human message (layering
/// rule 2: typed metadata, not name-matching).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveErrorKind {
    /// `E_UseAfterMove` — a read of a binding that a (possibly conditional) move
    /// killed on some reaching path (`:2390` §9.5 branch-merge union).
    UseAfterMove,
    /// `E_DoubleMove` — a second move of an already-moved binding.
    DoubleMove,
    /// `E_MoveInLoop` — a move of an enclosing-scope local inside a loop body
    /// (a use-after-move on the next iteration).
    MoveInLoop,
}

impl MoveErrorKind {
    /// The stable `E_<VariantName>` diagnostic code — derives from the variant
    /// identity alone. This is the CONFORMANCE-compared axis of a rejection
    /// (pin 3: the `E_` code + exit class, never the prose message or span).
    pub fn code(&self) -> &'static str {
        match self {
            MoveErrorKind::UseAfterMove => "E_UseAfterMove",
            MoveErrorKind::DoubleMove => "E_DoubleMove",
            MoveErrorKind::MoveInLoop => "E_MoveInLoop",
        }
    }
}

/// A structured may-move rejection: the ratified `kind` (typed metadata — the
/// conformance-compared code), the human `message` (impl-defined detail, D11
/// trap precedent), and the statement-granular `span` of the offending site
/// (impl-defined provenance for the ` at file:line:col` render suffix; `None`
/// when the violation is outside any statement, e.g. a closure-body capture).
#[derive(Debug, Clone)]
pub struct LivenessError {
    pub kind: MoveErrorKind,
    pub message: String,
    pub span: Option<Span>,
}

/// The flow-sensitive move-tracker state.
struct Live<'a> {
    /// Per-closure-index capture name lists (`program.closures[i].captures`), so
    /// a closure-creation expression can read (check_use) its captured free vars.
    closure_captures: &'a [Vec<String>],
    /// Lexical scope stack: each frame is `(name, id)` in declaration order.
    /// Resolution scans innermost frame first, latest-declared first (shadowing).
    scopes: Vec<Vec<(String, Id)>>,
    next_id: Id,
    /// The set of binding ids currently moved-out.
    moved: HashSet<Id>,
    /// id → name, for the diagnostic message only (never conformance-compared).
    names: HashMap<Id, String>,
    /// Structural loop nesting. A move of an ENCLOSING-scope local inside a loop
    /// would be a use-after-move on the next iteration → `MoveInLoop`.
    loop_depth: u32,
    /// Loop-local ids: one set per active loop (innermost last). A local
    /// DECLARED inside the loop is re-created each iteration, so moving it is
    /// legal (mirrors production `loop_local_defs.last()`).
    loop_locals: Vec<HashSet<Id>>,
    /// The left-fold rebind target (`acc = bump(!acc)`): the `!acc` move feeds an
    /// immediate re-init, so it is not a cross-iteration re-move.
    rebind: Option<Id>,
    /// The span of the statement currently being checked — stamped onto the
    /// structured error so a violation deep inside expression-checking can name
    /// its offending site WITHOUT threading spans through every arm (the same
    /// device `eval`'s `cur_span` uses for trap provenance). `dummy` = no
    /// enclosing statement (a closure-body capture) → the error carries no span.
    cur_span: Span,
    /// The FIRST violation (we stop at the first, mirroring `eval`'s first Halt).
    error: Option<LivenessError>,
}

/// Run the static may-move gate over a whole program. `Ok(())` if well-formed;
/// `Err(LivenessError)` on the first use-after-move / double-move / move-in-loop
/// — the caller maps its `kind.code()` / `span` onto the `IllFormed` verdict
/// (the reject code + render location) BEFORE `eval` runs.
pub(crate) fn check_liveness(program: &Program) -> Result<(), LivenessError> {
    let captures: Vec<Vec<String>> = program.closures.iter().map(|c| c.captures.clone()).collect();
    // Each function is an independent activation: params are fresh live
    // bindings, the body walks in the params' scope.
    for f in &program.functions {
        let mut live = Live::new(&captures);
        live.push_scope();
        for p in &f.params {
            live.declare(&p.name);
        }
        live.check_block_in_current_scope(&f.body);
        if let Some(e) = live.error.take() {
            return Err(e);
        }
    }
    // Closure bodies are independent activations too: captures are by-value
    // copies (D5), so a closure's move-state is disjoint from its enclosing
    // function. Params + captures are fresh live bindings.
    for c in &program.closures {
        let mut live = Live::new(&captures);
        live.push_scope();
        for name in &c.captures {
            live.declare(name);
        }
        for p in &c.params {
            live.declare(&p.name);
        }
        live.check_expr(&c.body);
        if let Some(e) = live.error.take() {
            return Err(e);
        }
    }
    Ok(())
}

impl<'a> Live<'a> {
    fn new(closure_captures: &'a [Vec<String>]) -> Self {
        Live {
            closure_captures,
            scopes: Vec::new(),
            next_id: 0,
            moved: HashSet::new(),
            names: HashMap::new(),
            loop_depth: 0,
            loop_locals: Vec::new(),
            rebind: None,
            cur_span: Span::dummy(),
            error: None,
        }
    }

    // ── Scope + binding-id machinery ────────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Allocate a fresh live binding for `name` in the current scope. If in a
    /// loop, record it as loop-local (safe to move — re-created each iteration).
    fn declare(&mut self, name: &str) -> Id {
        self.declare_impl(name, true)
    }

    /// Like `declare`, but NEVER records the binding as loop-local even inside a
    /// loop. Used for the `for`-element view, which aliases persistent
    /// collection storage rather than being re-created each iteration — moving
    /// it out is a `MoveInLoop` (production binds the for-var OUTSIDE the
    /// loop-local scope: `check_stmt.rs:968` precedes `:992`).
    fn declare_non_loop_local(&mut self, name: &str) -> Id {
        self.declare_impl(name, false)
    }

    fn declare_impl(&mut self, name: &str, loop_local: bool) -> Id {
        let id = self.next_id;
        self.next_id += 1;
        self.names.insert(id, name.to_string());
        self.scopes.last_mut().expect("a scope is open").push((name.to_string(), id));
        if loop_local {
            if let Some(set) = self.loop_locals.last_mut() {
                set.insert(id);
            }
        }
        id
    }

    /// Resolve `name` to the currently-in-scope binding id (innermost scope,
    /// latest declaration first — matching `eval`'s `rposition` shadowing).
    fn resolve(&self, name: &str) -> Option<Id> {
        for frame in self.scopes.iter().rev() {
            if let Some((_, id)) = frame.iter().rev().find(|(n, _)| n == name) {
                return Some(*id);
            }
        }
        None
    }

    fn is_loop_local(&self, id: Id) -> bool {
        self.loop_locals.last().map_or(false, |s| s.contains(&id))
    }

    /// Record the FIRST violation: the ratified `kind` (typed → the reject code)
    /// plus the human `msg`, stamped with the current statement span (`dummy` →
    /// no span, rendered without a location, like the trap arm).
    fn set_err(&mut self, kind: MoveErrorKind, msg: String) {
        if self.error.is_none() {
            let span = (self.cur_span != Span::dummy()).then_some(self.cur_span);
            self.error = Some(LivenessError { kind, message: msg, span });
        }
    }

    // ── The two primitive checks (mirror origins.rs check_use / check_move) ──

    /// A read of `name`. If its binding is moved-out → use-after-move.
    fn check_use(&mut self, name: &str) {
        if self.error.is_some() {
            return;
        }
        if let Some(id) = self.resolve(name) {
            if self.moved.contains(&id) {
                self.set_err(
                    MoveErrorKind::UseAfterMove,
                    format!("use of moved value `{name}`"),
                );
            }
        }
    }

    /// A move of `name` (kills its binding). Double-move / move-in-loop reject.
    fn check_move(&mut self, name: &str) {
        if self.error.is_some() {
            return;
        }
        let Some(id) = self.resolve(name) else {
            return; // untrackable (e.g. an unresolved place) — never enters `moved`
        };
        if self.moved.contains(&id) {
            self.set_err(
                MoveErrorKind::DoubleMove,
                format!("`{name}` moved more than once (double move)"),
            );
            return;
        }
        if self.loop_depth > 0 && !self.is_loop_local(id) && self.rebind != Some(id) {
            self.set_err(
                MoveErrorKind::MoveInLoop,
                format!("cannot move `{name}` out of an enclosing scope inside a loop"),
            );
            return;
        }
        self.moved.insert(id);
    }

    /// The root local name of a place expression (`v`, `v.f`, `v[i]`, `v.0`), or
    /// `None` for a non-place shape.
    fn place_root<'e>(expr: &'e Expr) -> Option<&'e str> {
        match expr {
            Expr::Local(n) => Some(n),
            Expr::Field(o, _) | Expr::TupleField(o, _) | Expr::Index(o, _) => Live::place_root(o),
            _ => None,
        }
    }

    // ── Sources (the copy/move/borrow decision, as `eval` sees it) ───────────

    fn check_source(&mut self, source: &Source) {
        if self.error.is_some() {
            return;
        }
        match source {
            // A move reads the place then KILLS its root.
            Source::Move(place) => {
                self.check_place_indices(place);
                if let Some(root) = Live::place_root(place) {
                    self.check_move(root);
                }
            }
            // Copy / borrow / write-through all READ the place (root must be live).
            Source::Copy(place) | Source::BorrowView(place) | Source::WriteThrough(place) => {
                self.check_expr(place);
            }
            Source::Value(expr) => self.check_expr(expr),
        }
    }

    /// The index sub-expressions of a place are ordinary reads (`v[side()]`).
    fn check_place_indices(&mut self, place: &Expr) {
        match place {
            Expr::Index(o, i) => {
                self.check_place_indices(o);
                self.check_expr(i);
            }
            Expr::Field(o, _) | Expr::TupleField(o, _) => self.check_place_indices(o),
            _ => {}
        }
    }

    // ── Expressions ──────────────────────────────────────────────────────────

    fn check_expr(&mut self, expr: &Expr) {
        if self.error.is_some() {
            return;
        }
        match expr {
            Expr::Local(name) => self.check_use(name),
            Expr::Field(o, _) | Expr::TupleField(o, _) => self.check_expr(o),
            Expr::Index(o, i) => {
                self.check_expr(o);
                self.check_expr(i);
            }
            Expr::Slice { object, start, end, .. } => {
                self.check_expr(object);
                if let Some(e) = start {
                    self.check_expr(e);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
            }
            Expr::Binary(_, a, b) => {
                self.check_expr(a);
                self.check_expr(b);
            }
            Expr::Unary(_, a) | Expr::Cast { expr: a, .. } => self.check_expr(a),
            Expr::Call { args, .. } => {
                for a in args {
                    self.check_source(a);
                }
            }
            Expr::CallValue { callee, args, consumes_callee } => {
                // eval reads the callee then, when consuming, KILLS it BEFORE the
                // args (a `ConsumeCallable` call is single-use). A second call
                // reads a moved-out slot → DoubleMove.
                if *consumes_callee {
                    if let Some(root) = Live::place_root(callee) {
                        self.check_move(root);
                    } else {
                        self.check_expr(callee);
                    }
                } else {
                    self.check_expr(callee);
                }
                for a in args {
                    self.check_source(a);
                }
            }
            Expr::Construct { args, .. } | Expr::EnumConstruct { args, .. } => {
                for a in args {
                    self.check_source(a);
                }
            }
            Expr::Method { recv, args, .. } => {
                self.check_expr(recv);
                for a in args {
                    self.check_source(a);
                }
            }
            Expr::Closure(idx) => {
                // Capture-by-value at creation READS each free var from the
                // current scope; capturing a moved-out var is a use-after-move.
                if let Some(caps) = self.closure_captures.get(*idx) {
                    for name in caps.clone() {
                        self.check_use(&name);
                    }
                }
            }
            Expr::Match { scrutinee, arms, else_arm, .. } => {
                self.check_expr(scrutinee);
                // Each arm body is an EXPR; branch over them like a stmt match.
                let saved = self.moved.clone();
                let mut ends: Vec<HashSet<Id>> = Vec::new();
                for arm in arms {
                    self.moved = saved.clone();
                    self.push_scope();
                    self.declare_pattern(&arm.pattern);
                    self.check_expr(&arm.body);
                    self.pop_scope();
                    ends.push(self.moved.clone());
                }
                if let Some(e) = else_arm {
                    self.moved = saved.clone();
                    self.check_expr(e);
                    ends.push(self.moved.clone());
                }
                self.moved = union_all(&saved, &ends);
            }
            Expr::Panic(e) | Expr::IntToStr(e) | Expr::Clone(e) | Expr::Propagate(e) => {
                self.check_expr(e)
            }
            Expr::FString(parts) => {
                for p in parts {
                    if let crate::ggc::FPart::Interp(e) = p {
                        self.check_expr(e);
                    }
                }
            }
            // Literals — no reads.
            Expr::Int(_) | Expr::Bool(_) | Expr::Float(_) | Expr::Str(_) | Expr::Unit => {}
        }
    }

    // ── Statements ───────────────────────────────────────────────────────────

    /// Walk a block, introducing a FRESH scope for its locals.
    fn check_block(&mut self, block: &[Stmt]) {
        self.push_scope();
        self.check_block_in_current_scope(block);
        self.pop_scope();
    }

    /// Walk a block's statements WITHOUT opening a new scope (the function root
    /// runs its body directly in the params' scope).
    fn check_block_in_current_scope(&mut self, block: &[Stmt]) {
        for s in block {
            if self.error.is_some() {
                return;
            }
            self.check_stmt(s);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        // Stamp the statement span so a violation surfaced deep inside
        // expression-checking names THIS statement's location (statement-granular
        // provenance, matching `eval`'s trap `cur_span`).
        self.cur_span = stmt_span(stmt);
        match stmt {
            Stmt::Bind { name, source, .. } => {
                self.check_source(source);
                // A `BorrowView`-sourced bind is a per-iteration VIEW of a
                // persistent value — uniquely the `for x in coll` element bind
                // (bind_source never yields BorrowView; only the for-desugar
                // does, `mod.rs:947`). It is NOT re-created each iteration, so a
                // move of it out is a MoveInLoop — do NOT seed it loop-local.
                if matches!(source, Source::BorrowView(_)) {
                    self.declare_non_loop_local(name);
                } else {
                    self.declare(name);
                }
            }
            Stmt::With { name, source, body, .. } => {
                self.check_source(source);
                self.push_scope();
                self.declare(name);
                self.check_block_in_current_scope(body);
                self.pop_scope();
            }
            Stmt::Assign { target, value, .. } => {
                if let Expr::Local(x) = target {
                    // Whole-local reassignment: eval evaluates the RHS first, so
                    // a `!x` inside it is a legal left-fold move (rebind guard);
                    // then the write REVIVES the slot (mark_live).
                    let saved_rebind = self.rebind;
                    self.rebind = self.resolve(x);
                    self.check_source(value);
                    self.rebind = saved_rebind;
                    if let Some(id) = self.resolve(x) {
                        self.moved.remove(&id);
                        // `mark_live` also seeds the innermost loop-local set
                        // (production `origins.rs:18`): a whole-local rebind
                        // inside a loop re-creates the value each iteration, so a
                        // subsequent move is legal (not MoveInLoop).
                        if let Some(set) = self.loop_locals.last_mut() {
                            set.insert(id);
                        }
                    }
                } else {
                    // A projected write (`x.f = …`, `v[i] = …`): the RHS, then the
                    // place — whose ROOT must be live (a moved root has no storage
                    // to write into → use-after-move).
                    self.check_source(value);
                    self.check_expr(target);
                }
            }
            Stmt::Expr { expr, .. } | Stmt::Print { expr, .. } => self.check_expr(expr),
            Stmt::If { cond, then_, else_, .. } => {
                self.check_expr(cond);
                let saved = self.moved.clone();
                self.check_block(then_);
                let then_end = self.moved.clone();
                let then_div = stmts_diverge(then_);
                self.moved = saved.clone();
                self.check_block(else_);
                let else_end = self.moved.clone();
                let else_div = stmts_diverge(else_);
                let mut ends: Vec<HashSet<Id>> = Vec::new();
                if !then_div {
                    ends.push(then_end);
                }
                if !else_div {
                    ends.push(else_end);
                }
                self.moved = union_all(&saved, &ends);
            }
            Stmt::Match { scrutinee, arms, else_arm, .. } => {
                self.check_expr(scrutinee);
                let saved = self.moved.clone();
                let mut ends: Vec<HashSet<Id>> = Vec::new();
                for arm in arms {
                    self.moved = saved.clone();
                    self.push_scope();
                    self.declare_pattern(&arm.pattern);
                    self.check_block_in_current_scope(&arm.body);
                    self.pop_scope();
                    if !stmts_diverge(&arm.body) {
                        ends.push(self.moved.clone());
                    }
                }
                if let Some(body) = else_arm {
                    self.moved = saved.clone();
                    self.check_block(body);
                    if !stmts_diverge(body) {
                        ends.push(self.moved.clone());
                    }
                }
                self.moved = union_all(&saved, &ends);
            }
            Stmt::While { cond, body, .. } => {
                self.check_expr(cond);
                self.check_loop_body(body);
            }
            Stmt::Loop { body, .. } => self.check_loop_body(body),
            Stmt::Return { value, .. } => {
                if let Some(e) = value {
                    self.check_expr(e);
                }
            }
            Stmt::Assert { cond, message, .. } => {
                self.check_expr(cond);
                if let Some(m) = message {
                    self.check_expr(m);
                }
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
        }
    }

    /// Walk a loop body ONCE at `loop_depth+1` with a FRESH innermost loop-local
    /// set. A move of an enclosing (non-loop-local, non-rebind) local is a
    /// `MoveInLoop`. After the body, the loop-merge is `union(before, after)` —
    /// a local moved before OR by the body is moved after (mirrors production
    /// `merge_branch_states([before, after_body])`).
    fn check_loop_body(&mut self, body: &[Stmt]) {
        let before = self.moved.clone();
        self.loop_depth += 1;
        self.loop_locals.push(HashSet::new());
        self.check_block(body);
        self.loop_locals.pop();
        self.loop_depth -= 1;
        // union(before, after): re-add any pre-loop move revived inside the body.
        for id in before {
            self.moved.insert(id);
        }
    }

    /// Declare a match-arm pattern's bindings as fresh live locals (they are
    /// Borrow views into the scrutinee).
    fn declare_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Literal(e) => self.check_expr(e),
            Pattern::Binding(name) => {
                self.declare(name);
            }
            Pattern::Variant { fields, .. } => {
                for f in fields {
                    self.declare_pattern(f);
                }
            }
        }
    }
}

/// The source span of a statement — every `Stmt` variant carries one (`ggc.rs`).
/// Used for the statement-granular provenance of a rejection's render location.
fn stmt_span(stmt: &Stmt) -> Span {
    match stmt {
        Stmt::Bind { span, .. }
        | Stmt::Assign { span, .. }
        | Stmt::Expr { span, .. }
        | Stmt::With { span, .. }
        | Stmt::Print { span, .. }
        | Stmt::If { span, .. }
        | Stmt::While { span, .. }
        | Stmt::Loop { span, .. }
        | Stmt::Match { span, .. }
        | Stmt::Return { span, .. }
        | Stmt::Break { span }
        | Stmt::Continue { span }
        | Stmt::Assert { span, .. } => *span,
    }
}

/// Union the moved-sets of the live (non-diverging) branch arms: "moved in ANY
/// arm ⇒ moved after the join" (`:2390` §9.5). If every arm diverged, the join
/// is unreachable — keep the pre-branch state.
fn union_all(saved: &HashSet<Id>, ends: &[HashSet<Id>]) -> HashSet<Id> {
    if ends.is_empty() {
        return saved.clone();
    }
    let mut out = HashSet::new();
    for e in ends {
        for id in e {
            out.insert(*id);
        }
    }
    out
}

/// Whether a statement block DEFINITELY diverges (its tail transfers control
/// away, so its move-state never reaches the join). Mirrors the self-host
/// `live_stmts_diverge`. Over-reporting divergence only ever UNDER-detects a
/// move (never a false reject).
fn stmts_diverge(block: &[Stmt]) -> bool {
    match block.last() {
        None => false,
        Some(s) => stmt_diverges(s),
    }
}

fn stmt_diverges(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => true,
        Stmt::Expr { expr: Expr::Panic(_), .. } => true,
        Stmt::If { then_, else_, .. } => {
            // An `if` diverges iff BOTH arms diverge (an empty else never does).
            !else_.is_empty() && stmts_diverge(then_) && stmts_diverge(else_)
        }
        Stmt::Match { arms, else_arm, .. } => {
            // A match (exhaustive by typecheck) diverges iff every arm diverges,
            // and the else (if present) diverges too.
            arms.iter().all(|a| stmts_diverge(&a.body))
                && else_arm.as_ref().map_or(true, |b| stmts_diverge(b))
        }
        _ => false,
    }
}
