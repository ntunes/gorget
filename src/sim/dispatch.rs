/// Main evaluation loop for the GIR interpreter.
/// Handles instructions, terminators, and function calls.

use std::collections::{HashMap, HashSet};

use crate::ir::instructions::{BinOp, CmpOp, Constant, Instruction, Operand, Place, Projection, Terminator, UnOp};
use crate::ir::types::{
    TypeId, GirType, TypeDefKind,
    BOOL_TYPE, I8_TYPE, I16_TYPE, I32_TYPE, I64_TYPE,
    U8_TYPE, U16_TYPE, U32_TYPE, U64_TYPE, F32_TYPE, F64_TYPE, UNIT_TYPE,
};
use crate::ir::Module;
use crate::span::Span;
use super::config::{BacktraceLevel, SimConfig};
use super::error::{SimError, SimResult};
use super::value::{SimStr, SimString, Value};
use super::runtime;

/// Per-allocation metadata for UB detection (Phase 4).
pub struct HeapMeta {
    /// Whether the slot is still live (not yet freed).
    pub alive: bool,
    /// True for internal ref-promoted slots (from get_or_alloc_ref), not user allocations.
    pub is_ref_promoted: bool,
    /// Name of the function where this allocation was created.
    pub alloc_fn: String,
    /// TrackingAllocator ID if inside a `with allocator:` scope at allocation time.
    pub allocator_id: Option<usize>,
}

/// A single frame on the interpreter's call stack (for backtraces).
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub fn_name: String,
    pub display_name: Option<String>,
    /// Span of the Call instruction that entered this frame (where the call happened).
    pub call_span: Option<Span>,
    /// Span of the function definition.
    pub def_span: Option<Span>,
    /// Span of the most recently executed instruction in this frame.
    pub current_span: Option<Span>,
}

/// Maximum call depth to prevent stack overflow.
const MAX_DEPTH: usize = 500;

/// Parse `_N` → N from InlineC local references.
fn parse_inline_local(s: &str) -> Option<usize> {
    s.trim().strip_prefix('_')?.parse().ok()
}

/// Mark the destination local of an instruction as initialized (P4c).
/// Called after every instruction to keep the initialized set up to date.
fn mark_instruction_dst(initialized: &mut HashSet<u32>, inst: &Instruction) {
    match inst {
        Instruction::Assign { dst, .. } => {
            if dst.projections.is_empty() {
                initialized.insert(dst.local.0);
            }
        }
        Instruction::FieldLoad { dst, .. }
        | Instruction::IndexLoad { dst, .. }
        | Instruction::FaultableIndexLoad { dst, .. }
        | Instruction::HeapAlloc { dst, .. }
        | Instruction::HeapAllocArray { dst, .. }
        | Instruction::BinOp { dst, .. }
        | Instruction::FaultableBinOp { dst, .. }
        | Instruction::UnOp { dst, .. }
        | Instruction::Cmp { dst, .. }
        | Instruction::Cast { dst, .. }
        | Instruction::BitCast { dst, .. }
        | Instruction::PtrCast { dst, .. }
        | Instruction::StructInit { dst, .. }
        | Instruction::EnumInit { dst, .. }
        | Instruction::TupleInit { dst, .. }
        | Instruction::TagOf { dst, .. }
        | Instruction::EnumFieldLoad { dst, .. }
        | Instruction::Borrow { dst, .. }
        | Instruction::BorrowMut { dst, .. }
        | Instruction::LoadThreadLocal { dst, .. } => {
            initialized.insert(dst.0);
        }
        Instruction::Call { dst: Some(dst), .. }
        | Instruction::CallExtern { dst: Some(dst), .. }
        | Instruction::CallIndirect { dst: Some(dst), .. }
        | Instruction::FaultableCall { dst: Some(dst), .. } => {
            initialized.insert(dst.0);
        }
        _ => {}
    }
}

/// Set a local, extending the vec if needed.
fn local_set(locals: &mut Vec<Value>, idx: usize, val: Value) {
    while locals.len() <= idx { locals.push(Value::Unit); }
    locals[idx] = val;
}

/// Split a C expression list by top-level commas (not inside parens or quotes).
fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut in_str = false;
    let mut start = 0;
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'"' if !in_str => { in_str = true; }
            b'"' if in_str => { in_str = false; }
            b'\\' if in_str => { i += 1; } // skip escaped char
            b'(' | b'[' | b'{' if !in_str => { depth += 1; }
            b')' | b']' | b'}' if !in_str => { depth -= 1; }
            b',' if !in_str && depth == 0 => {
                let part = s[start..i].trim();
                if !part.is_empty() { result.push(&s[start..i]); }
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    let part = s[start..].trim();
    if !part.is_empty() { result.push(&s[start..]); }
    result
}

/// Simulated allocator state (covers TrackingAllocator, Arena, PoolAllocator).
#[derive(Clone)]
enum SimAllocState {
    Tracking {
        bytes_allocated: i64,
        current_bytes: i64,
        peak_bytes: i64,
        alloc_count: i64,
        free_count: i64,
        realloc_count: i64,
        bytes_freed: i64,
    },
    Arena {
        bytes_used: i64,
        capacity: i64,
    },
    Pool {
        block_size: i64,
        total_blocks: i64,
        used_blocks: i64,
    },
    Tlsf {
        bytes_used: i64,
        peak_bytes: i64,
        pool_size: i64,
    },
}

impl SimAllocState {
    fn tracking_default() -> Self {
        SimAllocState::Tracking {
            bytes_allocated: 0, current_bytes: 0, peak_bytes: 0,
            alloc_count: 0, free_count: 0, realloc_count: 0, bytes_freed: 0,
        }
    }
    fn record_alloc(&mut self, bytes: i64) {
        match self {
            SimAllocState::Tracking { bytes_allocated, current_bytes, peak_bytes, alloc_count, .. } => {
                *bytes_allocated += bytes;
                *current_bytes += bytes;
                *alloc_count += 1;
                if *current_bytes > *peak_bytes { *peak_bytes = *current_bytes; }
            }
            SimAllocState::Arena { bytes_used, .. } => { *bytes_used += bytes; }
            SimAllocState::Pool { used_blocks, .. } => { *used_blocks += 1; }
            SimAllocState::Tlsf { bytes_used, peak_bytes, .. } => {
                *bytes_used += bytes;
                if *bytes_used > *peak_bytes { *peak_bytes = *bytes_used; }
            }
        }
    }
    fn record_realloc(&mut self, old_bytes: i64, new_bytes: i64) {
        match self {
            SimAllocState::Tracking { bytes_allocated, current_bytes, peak_bytes, realloc_count, .. } => {
                *bytes_allocated += new_bytes;
                *current_bytes += new_bytes - old_bytes;
                *realloc_count += 1;
                if *current_bytes > *peak_bytes { *peak_bytes = *current_bytes; }
            }
            SimAllocState::Arena { bytes_used, .. } => { *bytes_used += new_bytes - old_bytes; }
            SimAllocState::Pool { used_blocks, .. } => { let _ = (old_bytes, new_bytes); *used_blocks += 1; }
            SimAllocState::Tlsf { bytes_used, peak_bytes, .. } => {
                *bytes_used += new_bytes - old_bytes;
                if *bytes_used > *peak_bytes { *peak_bytes = *bytes_used; }
            }
        }
    }
    #[allow(dead_code)]
    fn record_free(&mut self, bytes: i64) {
        match self {
            SimAllocState::Tracking { current_bytes, bytes_freed, free_count, .. } => {
                *current_bytes -= bytes;
                *bytes_freed += bytes;
                *free_count += 1;
            }
            SimAllocState::Arena { .. } => {} // Arena doesn't track individual frees
            SimAllocState::Pool { used_blocks, .. } => { if *used_blocks > 0 { *used_blocks -= 1; } let _ = bytes; }
            SimAllocState::Tlsf { bytes_used, .. } => { *bytes_used -= bytes; let _ = bytes; }
        }
    }
}

// Alias for backwards compatibility within this file
#[allow(dead_code)]
type TrackingState = SimAllocState;

/// The GIR interpreter.
pub struct Interpreter<'m> {
    pub module: &'m Module,
    /// Heap storage: address → Value.
    pub heap: HashMap<usize, Value>,
    /// Per-allocation metadata for P4 UB detection.
    pub heap_meta: HashMap<usize, HeapMeta>,
    pub heap_next: usize,
    /// Global variables by name.
    pub globals: HashMap<String, Value>,
    /// Captured stdout bytes.
    pub stdout: Vec<u8>,
    /// Captured stderr bytes.
    pub stderr: Vec<u8>,
    /// Whether UB-detection checks (P4b–P4d) are enabled.
    ub_checks: bool,
    /// Name of the currently-executing function (for error context).
    pub current_fn_name: String,
    /// Call stack for backtrace support (P7c).
    pub call_stack: Vec<StackFrame>,
    /// Span of the instruction currently being executed.
    pub current_instr_span: Option<Span>,
    /// Backtrace captured at the first error (innermost frame first).
    pub last_error_backtrace: Option<Vec<StackFrame>>,
    /// Span where the first error occurred.
    pub last_error_span: Option<Span>,
    /// Backtrace verbosity level (from --backtrace flag).
    pub backtrace_level: BacktraceLevel,
    /// Simulated allocator states keyed by allocator ID (TrackingAllocator / Arena / PoolAllocator).
    tracking_allocs: HashMap<usize, SimAllocState>,
    /// Stack of active allocator IDs (from `with X as y:` or `alloc=` syntax).
    active_tracking: Vec<usize>,
    /// Next allocator ID.
    tracking_next_id: usize,
    /// Task results from spawn: task_id → Value (single-threaded eager evaluation).
    task_results: HashMap<usize, Value>,
    /// Next task ID.
    task_next_id: usize,
    /// Next synthetic thread ID (for Thread[T].id() calls).
    thread_next_id: usize,
    /// TaskGroup pending closures: group_addr → Vec<closure Value>
    task_group_tasks: HashMap<usize, Vec<Value>>,
    /// TCP socket storage: handle_id → TcpStream.
    tcp_sockets: HashMap<usize, std::net::TcpStream>,
    /// UDP socket storage: handle_id → UdpSocket.
    udp_sockets: HashMap<usize, std::net::UdpSocket>,
    /// Next socket handle ID.
    socket_next_id: usize,
    /// Multicast group subscriptions: group_addr → [socket_ids].
    /// Used to simulate in-process multicast delivery when OS multicast doesn't work.
    multicast_subs: HashMap<String, Vec<usize>>,
    /// Per-socket inbox: socket_id → queued (packet_bytes, sender_addr) pairs.
    /// Used for multicast delivery and any in-process packet routing.
    socket_inbox: HashMap<usize, std::collections::VecDeque<(Vec<u8>, std::net::SocketAddr)>>,
    /// Compiled regex objects: id → compiled Regex.
    regex_map: HashMap<u64, ::regex::Regex>,
    /// Next regex ID.
    regex_next_id: u64,
    /// Ref-count tracking for Shared[T] / Weak[T]: addr → (strong, weak).
    /// Mirrors the GorgetShared control block atomic counters.
    shared_refcounts: HashMap<usize, (i64, i64)>,
}

impl<'m> Interpreter<'m> {
    pub fn new(module: &'m Module, config: &SimConfig) -> Self {
        Self {
            module,
            heap: HashMap::new(),
            heap_meta: HashMap::new(),
            heap_next: 1, // 0 = null
            globals: HashMap::new(),
            stdout: Vec::new(),
            stderr: Vec::new(),
            ub_checks: config.ub_checks,
            current_fn_name: String::new(),
            call_stack: Vec::new(),
            current_instr_span: None,
            last_error_backtrace: None,
            last_error_span: None,
            backtrace_level: config.backtrace.clone(),
            tracking_allocs: HashMap::new(),
            active_tracking: Vec::new(),
            tracking_next_id: 1,
            task_results: HashMap::new(),
            task_next_id: 1,
            thread_next_id: 2, // 1 = main thread
            task_group_tasks: HashMap::new(),
            tcp_sockets: HashMap::new(),
            udp_sockets: HashMap::new(),
            socket_next_id: 1,
            multicast_subs: HashMap::new(),
            socket_inbox: HashMap::new(),
            regex_map: HashMap::new(),
            regex_next_id: 1,
            shared_refcounts: HashMap::new(),
        }
    }

    /// Record an allocation event to the top-of-stack active allocator (if any).
    #[allow(dead_code)]
    fn tracking_record_alloc(&mut self, bytes: i64) {
        if let Some(&id) = self.active_tracking.last() {
            if let Some(state) = self.tracking_allocs.get_mut(&id) {
                state.record_alloc(bytes);
            }
        }
    }

    /// Record a realloc event to the top-of-stack active allocator (if any).
    #[allow(dead_code)]
    fn tracking_record_realloc(&mut self, old_bytes: i64, new_bytes: i64) {
        if let Some(&id) = self.active_tracking.last() {
            if let Some(state) = self.tracking_allocs.get_mut(&id) {
                state.record_realloc(old_bytes, new_bytes);
            }
        }
    }

    /// Record a free event to the top-of-stack active allocator (if any).
    #[allow(dead_code)]
    fn tracking_record_free(&mut self, bytes: i64) {
        if let Some(&id) = self.active_tracking.last() {
            if let Some(state) = self.tracking_allocs.get_mut(&id) {
                state.record_free(bytes);
            }
        }
    }

    /// Capture the current call stack as a backtrace (innermost frame first).
    pub fn capture_backtrace(&self) -> Vec<StackFrame> {
        self.call_stack.iter().rev().cloned().collect()
    }

    /// Allocate a heap slot and store a value. Records metadata for P4 UB detection.
    pub fn heap_alloc(&mut self, val: Value) -> usize {
        self.heap_alloc_inner(val, false)
    }

    /// Allocate a heap slot marked as ref-promoted (implementation artifact, not a user allocation).
    fn heap_alloc_ref_promoted(&mut self, val: Value) -> usize {
        self.heap_alloc_inner(val, true)
    }

    fn heap_alloc_inner(&mut self, val: Value, is_ref_promoted: bool) -> usize {
        let addr = self.heap_next;
        self.heap_next += 1;
        self.heap.insert(addr, val);
        self.heap_meta.insert(addr, HeapMeta {
            alive: true,
            is_ref_promoted,
            alloc_fn: self.current_fn_name.clone(),
            allocator_id: self.active_tracking.last().copied(),
        });
        addr
    }

    /// Read a value from the heap. Returns UseAfterFree if the slot was freed (P4b).
    pub fn heap_read(&self, addr: usize) -> SimResult<&Value> {
        if self.ub_checks {
            if let Some(meta) = self.heap_meta.get(&addr) {
                if !meta.alive {
                    return Err(SimError::UseAfterFree {
                        addr,
                        alloc_fn: meta.alloc_fn.clone(),
                    });
                }
            }
        }
        self.heap.get(&addr).ok_or(SimError::NullDereference)
    }

    /// Write a value to the heap. Returns UseAfterFree if the slot was freed (P4b).
    pub fn heap_write(&mut self, addr: usize, val: Value) {
        if self.ub_checks {
            if let Some(meta) = self.heap_meta.get(&addr) {
                if !meta.alive {
                    // UAF on write — best-effort: emit to stderr but don't panic
                    // (heap_write doesn't return SimResult for broad compatibility).
                    eprintln!("gg sim: use-after-free (write): heap[{addr}] allocated in {}", meta.alloc_fn);
                }
            }
        }
        self.heap.insert(addr, val);
    }

    /// Get or create a heap address for a place, making simple locals heap-backed
    /// via `Value::Ref(addr)`. If the local is already `Ref(addr)`, reuse that addr.
    /// For places with projections, always allocates a fresh heap copy.
    /// Slots created here are marked `is_ref_promoted = true` (implementation artifacts).
    fn get_or_alloc_ref(&mut self, locals: &mut Vec<Value>, place: &Place) -> SimResult<usize> {
        if place.projections.is_empty() {
            let idx = place.local.0 as usize;
            while locals.len() <= idx { locals.push(Value::Unit); }
            match locals[idx].clone() {
                Value::Ref(addr) => Ok(addr),
                other => {
                    let addr = self.heap_alloc_ref_promoted(other);
                    locals[idx] = Value::Ref(addr);
                    Ok(addr)
                }
            }
        } else {
            // For projected places (e.g. struct field borrow), copy the value.
            let val = self.read_place(locals, place)?;
            Ok(self.heap_alloc_ref_promoted(val))
        }
    }

    /// Initialize global variables from module.globals.
    pub fn init_globals(&mut self) {
        for global in &self.module.globals {
            let val = self.eval_global_init(&global.init, global.type_id);
            self.globals.insert(global.name.clone(), val);
        }
    }

    fn eval_global_init(&self, init: &crate::ir::GlobalInit, type_id: TypeId) -> Value {
        use crate::ir::GlobalInit;
        match init {
            GlobalInit::Zeroed => Value::zero_for_type(type_id, &self.module.type_registry),
            GlobalInit::Bytes(bytes) => {
                // Raw bytes → treat as a string if valid UTF-8, else as bytes
                if let Ok(s) = std::str::from_utf8(bytes) {
                    Value::Str(SimStr::from_str(s))
                } else {
                    Value::Str(SimStr::from_str(""))
                }
            }
            GlobalInit::FnRef(name) => Value::FuncRef(name.clone()),
            // Vtable drop slot: the wrapper is backend-synthesized (no sim
            // function exists); the sim's ownership model doesn't run drop
            // glue, so a named FuncRef placeholder keeps the vtable shape
            // without ever being dispatched.
            GlobalInit::BoxDropRef(inner) => Value::FuncRef(format!("Box__{inner}__drop")),
            GlobalInit::Struct { type_name, fields } => {
                let field_vals = fields.iter()
                    .map(|(_, fi)| self.eval_global_init(fi, type_id))
                    .collect();
                Value::Struct { type_name: type_name.clone(), fields: field_vals }
            }
            GlobalInit::Extern { .. } => {
                // Runtime-initialized globals are not supported in the sim
                // interpreter; return a zero value as a placeholder.
                Value::zero_for_type(type_id, &self.module.type_registry)
            }
            GlobalInit::StaticArrayView { elem_type_name, elems } => {
                // R34 Track A: materialize the const array into a SimArray so
                // the interpreter observes the same element sequence the C /
                // LLVM backends bake into the static view. Best-effort — the
                // sim is not a shipped backend for these tables.
                use crate::sim::value::SimArray;
                let arr = SimArray::new(format!("Vector__{elem_type_name}"));
                for e in elems {
                    arr.push(self.eval_global_init(e, type_id));
                }
                Value::Array(arr)
            }
        }
    }

    /// Evaluate an operand to a Value.
    pub fn eval_operand(&self, locals: &[Value], op: &Operand) -> SimResult<Value> {
        match op {
            Operand::Copy(place) | Operand::Move(place) => self.read_place(locals, place),
            Operand::Constant(c) => Ok(self.eval_constant(c)),
        }
    }

    /// Convert a Constant to a Value.
    pub fn eval_constant(&self, c: &Constant) -> Value {
        match c {
            Constant::Bool(b) => Value::Bool(*b),
            Constant::I8(n) => Value::I8(*n),
            Constant::I16(n) => Value::I16(*n),
            Constant::I32(n) => Value::I32(*n),
            Constant::I64(n) => Value::I64(*n),
            Constant::U8(n) => Value::U8(*n),
            Constant::U16(n) => Value::U16(*n),
            Constant::U32(n) => Value::U32(*n),
            Constant::U64(n) => Value::U64(*n),
            Constant::F32(f) => Value::F32(*f),
            Constant::F64(f) => Value::F64(*f),
            Constant::Str(s) => Value::Str(SimStr::from_str(s)),
            Constant::Null => Value::Null,
            Constant::Unit => Value::Unit,
            Constant::SizeOf(_type_id) => Value::I64(8), // Approximate sizeof as 8 bytes
            Constant::FuncRef(name) => Value::FuncRef(name.clone()),
            Constant::GlobalRef(name) => {
                // Look up the global variable value in the interpreter's global store.
                self.globals.get(name).cloned().unwrap_or(Value::Unit)
            }
            Constant::GlobalRefPtr(name) => {
                // Pointer to global — in the simulator, return the value (no true pointers).
                self.globals.get(name).cloned().unwrap_or(Value::Unit)
            }
        }
    }

    /// Read a value from a place (with projections).
    pub fn read_place(&self, locals: &[Value], place: &Place) -> SimResult<Value> {
        let mut val = locals.get(place.local.0 as usize)
            .cloned()
            .unwrap_or(Value::Unit);

        // Transparent deref: heap-backed locals (from BorrowMut) proxy through heap.
        if let Value::Ref(addr) = val {
            val = self.heap_read(addr).cloned().unwrap_or(Value::Unit);
        }

        for proj in &place.projections {
            val = self.apply_projection_read(val, proj, locals)?;
        }
        Ok(val)
    }

    fn apply_projection_read(&self, val: Value, proj: &Projection, locals: &[Value]) -> SimResult<Value> {
        match proj {
            Projection::Field(idx) => {
                match val {
                    Value::Struct { fields, .. } => {
                        fields.into_iter().nth(*idx as usize)
                            .ok_or(SimError::IndexOutOfBounds { index: *idx as i64, len: 0 })
                    }
                    Value::Tuple(elems) => {
                        elems.into_iter().nth(*idx as usize)
                            .ok_or(SimError::IndexOutOfBounds { index: *idx as i64, len: 0 })
                    }
                    Value::Enum { fields, .. } => {
                        fields.into_iter().nth(*idx as usize)
                            .ok_or(SimError::IndexOutOfBounds { index: *idx as i64, len: 0 })
                    }
                    // GorgetString: field 0 = data ptr, field 1 = len, field 2 = cap
                    Value::String(ref s) => Ok(match *idx {
                        0 => Value::String(s.clone()),
                        1 => Value::U64(s.byte_len() as u64),
                        2 => Value::U64(s.capacity() as u64),
                        _ => Value::Unit,
                    }),
                    // Str: field 0 = data (str view), field 1 = len
                    Value::Str(ref s) => Ok(match *idx {
                        0 => Value::Str(s.clone()),
                        1 => Value::U64(s.byte_len() as u64),
                        _ => Value::Unit,
                    }),
                    // Array: field 1 = len
                    Value::Array(ref arr) => Ok(match *idx {
                        1 => Value::I64(arr.len() as i64),
                        _ => Value::Unit,
                    }),
                    // Dict: field access for iteration internals
                    Value::Dict(ref d) => Ok(match *idx {
                        1 => Value::I64(d.len() as i64),
                        _ => Value::Unit,
                    }),
                    // Primitives accessed as .0 (newtype pattern)
                    other => {
                        if *idx == 0 {
                            Ok(other)
                        } else {
                            Ok(Value::Unit)
                        }
                    }
                }
            }
            Projection::Index(idx_local) => {
                let idx_val = locals.get(idx_local.0 as usize).cloned().unwrap_or(Value::I64(0));
                let idx = idx_val.as_i64();
                match val {
                    Value::Tuple(elems) => {
                        let i = if idx < 0 { elems.len() as i64 + idx } else { idx } as usize;
                        elems.into_iter().nth(i)
                            .ok_or(SimError::IndexOutOfBounds { index: idx, len: 0 })
                    }
                    Value::Array(arr) => {
                        let len = arr.len() as i64;
                        let i = if idx < 0 { len + idx } else { idx };
                        if i < 0 || i >= len {
                            Err(SimError::IndexOutOfBounds { index: idx, len: len as usize })
                        } else {
                            arr.get(i as usize).ok_or(SimError::IndexOutOfBounds { index: idx, len: len as usize })
                        }
                    }
                    Value::Dict(d) => {
                        Ok(d.get(&idx_val).unwrap_or(Value::Unit))
                    }
                    other => Err(SimError::TypeMismatch {
                        expected: "array/tuple/dict".into(),
                        got: other.type_name().into(),
                    }),
                }
            }
            Projection::Deref => {
                match val {
                    Value::Ptr(addr) | Value::MutPtr(addr) => {
                        self.heap_read(addr).cloned()
                    }
                    other => Err(SimError::TypeMismatch {
                        expected: "pointer".into(),
                        got: other.type_name().into(),
                    }),
                }
            }
        }
    }

    /// Write a value to a place in locals (with projections).
    pub fn write_place(locals: &mut Vec<Value>, place: &Place, val: Value, heap: &mut HashMap<usize, Value>) -> SimResult<()> {
        if place.projections.is_empty() {
            let idx = place.local.0 as usize;
            while locals.len() <= idx {
                locals.push(Value::Unit);
            }
            // Write-through for heap-backed locals (transparent Ref proxy).
            if let Value::Ref(addr) = locals[idx] {
                heap.insert(addr, val);
            } else {
                locals[idx] = val;
            }
            return Ok(());
        }

        // Navigate to the parent and mutate
        Self::write_place_projected(locals, place, val, heap)
    }

    fn write_place_projected(locals: &mut Vec<Value>, place: &Place, val: Value, heap: &mut HashMap<usize, Value>) -> SimResult<()> {
        // For complex projections: handle simple single-projection cases
        let base_idx = place.local.0 as usize;
        while locals.len() <= base_idx {
            locals.push(Value::Unit);
        }

        if place.projections.len() == 1 {
            match &place.projections[0] {
                Projection::Field(field_idx) => {
                    let field_idx = *field_idx as usize;
                    match &mut locals[base_idx] {
                        Value::Struct { fields, .. } => {
                            while fields.len() <= field_idx { fields.push(Value::Unit); }
                            fields[field_idx] = val;
                        }
                        Value::Tuple(elems) => {
                            while elems.len() <= field_idx { elems.push(Value::Unit); }
                            elems[field_idx] = val;
                        }
                        Value::Enum { fields, .. } => {
                            while fields.len() <= field_idx { fields.push(Value::Unit); }
                            fields[field_idx] = val;
                        }
                        _ => {
                            // Replace with a struct with one field
                            locals[base_idx] = val;
                        }
                    }
                }
                Projection::Deref => {
                    match locals[base_idx].clone() {
                        Value::Ptr(addr) | Value::MutPtr(addr) => {
                            heap.insert(addr, val);
                        }
                        _ => {
                            locals[base_idx] = val;
                        }
                    }
                }
                Projection::Index(idx_local) => {
                    let idx_local = *idx_local;
                    let idx_val = locals.get(idx_local.0 as usize).cloned().unwrap_or(Value::I64(0));
                    let base = &mut locals[base_idx];
                    match base {
                        Value::Array(arr) => {
                            let idx = idx_val.as_i64();
                            let len = arr.len() as i64;
                            let i = if idx < 0 { len + idx } else { idx };
                            if i >= 0 && (i as usize) < arr.len() {
                                arr.set(i as usize, val);
                            }
                        }
                        Value::Dict(d) => {
                            d.set(idx_val, val);
                        }
                        Value::Ref(addr) => {
                            let addr = *addr;
                            if let Some(heap_val) = heap.get_mut(&addr) {
                                match heap_val {
                                    Value::Array(arr) => {
                                        let idx = idx_val.as_i64();
                                        let len = arr.len() as i64;
                                        let i = if idx < 0 { len + idx } else { idx };
                                        if i >= 0 && (i as usize) < arr.len() {
                                            arr.set(i as usize, val);
                                        }
                                    }
                                    Value::Dict(d) => {
                                        d.set(idx_val, val);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        } else if place.projections.len() == 2 {
            // Two-level projections
            match (&place.projections[0], &place.projections[1]) {
                (Projection::Deref, Projection::Field(field_idx)) => {
                    // (*ptr).field = val  — write field through pointer
                    let field = *field_idx as usize;
                    let base = locals[base_idx].clone();
                    let addr = match base {
                        Value::Ptr(a) | Value::MutPtr(a) => a,
                        Value::Ref(a) => {
                            // Deref the Ref to get the pointer, then deref again
                            // This handles: heap[ref_addr] = Struct or MutPtr
                            match heap.get(&a).cloned().unwrap_or(Value::Unit) {
                                Value::Ptr(b) | Value::MutPtr(b) => b,
                                _ => {
                                    // Write field directly into heap[a]
                                    if let Some(heap_val) = heap.get_mut(&a) {
                                        match heap_val {
                                            Value::Struct { fields, .. } => {
                                                while fields.len() <= field { fields.push(Value::Unit); }
                                                fields[field] = val;
                                            }
                                            _ => {}
                                        }
                                    }
                                    return Ok(());
                                }
                            }
                        }
                        _ => { locals[base_idx] = val; return Ok(()); }
                    };
                    // Mutate field inside heap[addr]
                    if let Some(heap_val) = heap.get_mut(&addr) {
                        match heap_val {
                            Value::Struct { fields, .. } => {
                                while fields.len() <= field { fields.push(Value::Unit); }
                                fields[field] = val;
                            }
                            Value::Tuple(elems) => {
                                while elems.len() <= field { elems.push(Value::Unit); }
                                elems[field] = val;
                            }
                            _ => {}
                        }
                    }
                }
                (Projection::Field(outer_idx), Projection::Field(inner_idx)) => {
                    // struct.field.subfield = val
                    let (outer, inner) = (*outer_idx as usize, *inner_idx as usize);
                    match &mut locals[base_idx] {
                        Value::Struct { fields, .. } => {
                            if let Some(outer_val) = fields.get_mut(outer) {
                                match outer_val {
                                    Value::Struct { fields: inner_fields, .. } => {
                                        while inner_fields.len() <= inner { inner_fields.push(Value::Unit); }
                                        inner_fields[inner] = val;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {}
                    }
                }
                _ => {
                    // Fallback: just set the base
                    locals[base_idx] = val;
                }
            }
        } else {
            // Deep projections: fall back to setting base
            locals[base_idx] = val;
        }

        Ok(())
    }

    /// Look up the variant tag for an enum type.
    fn variant_tag(&self, type_name: &str, variant_name: &str) -> i64 {
        if let Some(def) = self.module.type_registry.get_type_def(type_name) {
            if let TypeDefKind::Enum(ref e) = def.kind {
                for (i, v) in e.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return i as i64;
                    }
                }
            }
        }
        // Fallback: common patterns
        match variant_name {
            "Ok" | "Some" => 0,
            "Error" | "None" => 1,
            _ => 0,
        }
    }

    /// Execute a single instruction. Modifies locals in place.
    /// `initialized` tracks which local IDs have been written (P4c uninitialized-read detection).
    pub fn execute_instruction(&mut self, locals: &mut Vec<Value>, initialized: &mut HashSet<u32>, inst: &Instruction, depth: usize) -> SimResult<()> {
        match inst {
            Instruction::Nop => {}

            Instruction::Assign { dst, value, .. } => {
                // P4c: check that operand locals are initialized before reading them.
                if self.ub_checks {
                    if let Operand::Copy(place) | Operand::Move(place) = value {
                        if place.projections.is_empty() && !initialized.contains(&place.local.0) {
                            return Err(SimError::UninitializedRead {
                                local: place.local.0,
                                name: String::new(),
                            });
                        }
                    }
                }
                let val = self.eval_operand(locals, value)?;
                Self::write_place(locals, dst, val, &mut self.heap)?;
                // Dst marking handled by mark_instruction_dst at the end of execute_instruction.
            }

            Instruction::FieldLoad { dst, base, field, .. } => {
                let base_val = self.read_place(locals, base)?;
                // Auto-deref if base is a pointer (C's -> operator: ptr->field).
                // The GIR lowering emits FieldLoad with Place::local(env) when the
                // env local holds a pointer (e.g. closure env ptr, struct ptr params).
                let base_val = match base_val {
                    Value::Ptr(addr) | Value::MutPtr(addr) => {
                        self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                    }
                    other => other,
                };
                let field_val = match base_val {
                    Value::Struct { fields, .. } => {
                        fields.into_iter().nth(*field as usize).unwrap_or(Value::Unit)
                    }
                    Value::Tuple(elems) => {
                        elems.into_iter().nth(*field as usize).unwrap_or(Value::Unit)
                    }
                    Value::Enum { fields, .. } => {
                        fields.into_iter().nth(*field as usize).unwrap_or(Value::Unit)
                    }
                    // GorgetString field access: field 0 = data (return the string itself
                    // as a stand-in for the char* pointer), field 1 = len, field 2 = cap.
                    Value::String(ref s) => match *field {
                        0 => Value::String(s.clone()), // data field — carry the whole string
                        1 | 2 => Value::U64(s.byte_len() as u64),
                        _ => Value::Unit,
                    },
                    // Str field access: field 0 = data pointer, field 1 = len.
                    Value::Str(ref s) => match *field {
                        0 => Value::Str(s.clone()), // data field — carry the whole view
                        1 => Value::U64(s.byte_len() as u64),
                        _ => Value::Unit,
                    },
                    // GorgetArray field access: field 0=data ptr, 1=len, 2=cap, 3=elem_size
                    Value::Array(ref arr) => match *field {
                        1 => Value::U64(arr.len() as u64),
                        _ => Value::Unit,
                    },
                    Value::Dict(ref d) => match *field {
                        1 => Value::U64(d.len() as u64),
                        _ => Value::Unit,
                    },
                    other => {
                        // For primitive types that might be accessed as .0 (newtype wrapper):
                        if *field == 0 { other } else { Value::Unit }
                    }
                };
                let idx = dst.0 as usize;
                while locals.len() <= idx { locals.push(Value::Unit); }
                locals[idx] = field_val;
            }

            // The simulator drives control flow from terminators, not from a
            // mid-block instruction, so it cannot model the bounds-fault BRANCH.
            // It reads the element exactly like a plain `IndexLoad` (returning a
            // SimError::IndexOutOfBounds on OOB — the sim's panic path); the
            // fault-recovery branch is a backend concern not exercised by the
            // sim, mirroring `FaultableBinOp` above.
            Instruction::IndexLoad { dst, base, index, .. }
            | Instruction::FaultableIndexLoad { dst, base, index, .. } => {
                let base_val = self.read_place(locals, base)?;
                let idx_val = self.eval_operand(locals, index)?;

                // Check if index is a GorgetRange (slice operation)
                if let Value::Struct { ref type_name, ref fields } = idx_val {
                    if type_name == "GorgetRange" {
                        let start = fields.get(0).map(|v| v.as_i64()).unwrap_or(0);
                        let end = fields.get(1).map(|v| v.as_i64()).unwrap_or(0);
                        let _inclusive = fields.get(2).map(|v| v.as_bool()).unwrap_or(false);
                        let result = match base_val {
                            Value::Array(arr) => {
                                let len = arr.len() as i64;
                                let s = start.max(0).min(len) as usize;
                                let e = end.max(0).min(len) as usize;
                                let sliced = super::value::SimArray::new(&arr.elem_type_name().to_string());
                                for i in s..e { if let Some(v) = arr.get(i) { sliced.push(v); } }
                                Value::Array(sliced)
                            }
                            Value::Str(s) => {
                                let chars: Vec<char> = s.as_str().chars().collect();
                                let len = chars.len() as i64;
                                let st = start.max(0).min(len) as usize;
                                let en = end.max(0).min(len) as usize;
                                let slice: String = chars[st..en].iter().collect();
                                Value::Str(SimStr::from_string(slice))
                            }
                            _ => Value::Unit,
                        };
                        let i = dst.0 as usize;
                        while locals.len() <= i { locals.push(Value::Unit); }
                        locals[i] = result;
                        return Ok(());  // handled
                    }
                }

                let idx = idx_val.as_i64();
                let result = match base_val {
                    Value::Tuple(elems) => {
                        let i = if idx < 0 { elems.len() as i64 + idx } else { idx } as usize;
                        elems.into_iter().nth(i).unwrap_or(Value::Unit)
                    }
                    Value::Array(arr) => {
                        let len = arr.len() as i64;
                        let actual = if idx < 0 { len + idx } else { idx };
                        if actual < 0 || actual >= len {
                            return Err(SimError::IndexOutOfBounds { index: idx, len: len as usize });
                        }
                        arr.get(actual as usize).unwrap_or(Value::Unit)
                    }
                    // Dict IndexLoad: look up by key value
                    Value::Dict(d) => {
                        d.get(&idx_val).unwrap_or(Value::Unit)
                    }
                    Value::Str(s) => {
                        // String indexing → codepoint
                        let count = s.codepoint_count() as i64;
                        let actual = if idx < 0 { count + idx } else { idx };
                        if actual < 0 || actual >= count {
                            return Err(SimError::IndexOutOfBounds { index: idx, len: count as usize });
                        }
                        let ch: String = s.as_str().chars().nth(actual as usize).unwrap().to_string();
                        Value::Str(SimStr::from_string(ch))
                    }
                    // Ptr/MutPtr: deref first, then index
                    Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                        let inner = self.heap_read(addr).cloned().unwrap_or(Value::Unit);
                        match inner {
                            Value::Array(arr) => {
                                let len = arr.len() as i64;
                                let actual = if idx < 0 { len + idx } else { idx };
                                if actual < 0 || actual >= len {
                                    return Err(SimError::IndexOutOfBounds { index: idx, len: len as usize });
                                }
                                arr.get(actual as usize).unwrap_or(Value::Unit)
                            }
                            Value::Dict(d) => d.get(&idx_val).unwrap_or(Value::Unit),
                            _ => Value::Unit,
                        }
                    }
                    other => {
                        // Unknown type — return unit
                        let _ = other;
                        Value::Unit
                    }
                };
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::BinOp { dst, op, type_id, lhs, rhs } => {
                let l = self.eval_operand(locals, lhs)?;
                let r = self.eval_operand(locals, rhs)?;
                let result = self.eval_binop(*op, *type_id, &l, &r)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            // The simulator drives control flow from terminators, not from a
            // mid-block instruction, so it cannot model the fault BRANCH. It
            // computes the arithmetic result like a plain BinOp (the
            // fault-recovery path is a backend-level concern not exercised by
            // the sim — Increment 1 has no sim fixture for fault-catch).
            Instruction::FaultableBinOp { dst, op, type_id, lhs, rhs, .. } => {
                let l = self.eval_operand(locals, lhs)?;
                let r = self.eval_operand(locals, rhs)?;
                let result = self.eval_binop(*op, *type_id, &l, &r)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::UnOp { dst, op, type_id, operand } => {
                let val = self.eval_operand(locals, operand)?;
                let result = self.eval_unop(*op, *type_id, &val)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::Cmp { dst, op, type_id, lhs, rhs } => {
                let l = self.eval_operand(locals, lhs)?;
                let r = self.eval_operand(locals, rhs)?;
                let result = self.eval_cmp(*op, *type_id, &l, &r)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::Cast { dst, target_type, value } => {
                let val = self.eval_operand(locals, value)?;
                let result = self.eval_cast(*target_type, val)?;
                // P4d: validate bool values after cast.
                if self.ub_checks && *target_type == BOOL_TYPE {
                    let raw = result.as_i64();
                    if raw != 0 && raw != 1 {
                        return Err(SimError::InvalidBoolValue { got: raw });
                    }
                }
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::BitCast { dst, target_type, value } => {
                let val = self.eval_operand(locals, value)?;
                let result = self.eval_bitcast(*target_type, val)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = result;
            }

            Instruction::PtrCast { dst, target_type: _, value } => {
                // Pointer casts just pass through the value with a different type annotation
                let val = self.eval_operand(locals, value)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = val;
            }

            Instruction::StructInit { dst, type_name, fields } => {
                let field_vals: SimResult<Vec<Value>> = fields.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let field_vals = field_vals?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                // Special-case: StructInit for "Str" creates a Value::Str.
                // This handles the GorgetString→Str coercion pattern:
                //   (Str){ .data = (*str_ptr).data, .len = (*str_ptr).len }
                // where field 0 is a String/Str/CStr value.
                if type_name == "Str" {
                    let val = match field_vals.first() {
                        Some(Value::Str(s)) => Value::Str(s.clone()),
                        Some(Value::String(s)) => Value::Str(s.to_sim_str()),
                        Some(Value::CStr(s)) => Value::Str(SimStr::from_str(s)),
                        // Ptr to string data: deref and extract
                        Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                            let addr = *addr;
                            match self.heap_read(addr).ok().cloned() {
                                Some(Value::Str(s)) => Value::Str(s),
                                Some(Value::String(s)) => Value::Str(s.to_sim_str()),
                                Some(Value::CStr(s)) => Value::Str(SimStr::from_str(&s)),
                                _ => Value::Struct { type_name: type_name.clone(), fields: field_vals },
                            }
                        }
                        _ => Value::Struct { type_name: type_name.clone(), fields: field_vals },
                    };
                    locals[i] = val;
                } else {
                    locals[i] = Value::Struct { type_name: type_name.clone(), fields: field_vals };
                }
            }

            Instruction::EnumInit { dst, type_name, variant, fields } => {
                let field_vals: SimResult<Vec<Value>> = fields.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let tag = self.variant_tag(type_name, variant);
                // P4d: validate enum tag is in range.
                if self.ub_checks {
                    if let Some(def) = self.module.type_registry.get_type_def(type_name) {
                        if let TypeDefKind::Enum(ref e) = def.kind {
                            let n = e.variants.len() as i64;
                            if tag < 0 || tag >= n {
                                return Err(SimError::InvalidEnumTag { type_name: type_name.clone(), tag });
                            }
                        }
                    }
                }
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::Enum {
                    type_name: type_name.clone(),
                    tag,
                    variant: variant.clone(),
                    fields: field_vals?,
                };
            }

            Instruction::TupleInit { dst, elements } => {
                let elem_vals: SimResult<Vec<Value>> = elements.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::Tuple(elem_vals?);
            }

            Instruction::TagOf { dst, operand } => {
                let val = self.eval_operand(locals, operand)?;
                // Auto-deref if the operand is a pointer (C's `ptr->tag` form).
                let val = match val {
                    Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                        self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                    }
                    other => other,
                };
                let tag = match &val {
                    Value::Enum { tag, .. } => *tag,
                    Value::Bool(b) => *b as i64,
                    // Null represents the None/Error variant (always the last, tag=1 by convention).
                    Value::Null => 1,
                    _ => 0,
                };
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::I64(tag);
            }

            Instruction::EnumFieldLoad { dst, base, variant, field, .. } => {
                let base_val = self.read_place(locals, base)?;
                // Auto-deref if base is a pointer (C's `ptr->data.Variant._N` form).
                let base_val = match base_val {
                    Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                        self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                    }
                    other => other,
                };
                let field_val = match base_val {
                    Value::Enum { fields, .. } => {
                        fields.into_iter().nth(*field as usize).unwrap_or(Value::Unit)
                    }
                    _ => Value::Unit,
                };
                let _ = variant; // Variant name is for documentation; we trust the tag matches
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = field_val;
            }

            // Fault-`catch`able cross-frame call (error-model.md §11, Increment
            // 2.1a). The simulator runs callees in fresh frames that panic on an
            // uncaught fault (it does not thread the NULL slot), so it executes
            // the call exactly like a plain `Call`; the slot-check branch is a
            // backend (LIR→C) concern. Delegate to the `Call` arm.
            Instruction::FaultableCall { dst, func, args, .. } => {
                let as_call = Instruction::Call {
                    dst: *dst,
                    func: func.clone(),
                    args: args.clone(),
                    reason: None,
                };
                return self.execute_instruction(locals, initialized, &as_call, depth);
            }

            Instruction::Call { dst, func, args, .. } => {
                // gorget_bytes_write_* pass array by value in GIR but mutate in-place (like C GorgetArray*).
                // Make the first arg local heap-backed and pass a MutPtr so mutation propagates.
                if func.starts_with("gorget_bytes_write_") {
                    let first_place = args.first().and_then(|op| match op {
                        Operand::Copy(p) | Operand::Move(p) => Some(p.clone()),
                        _ => None,
                    });
                    if let Some(place) = first_place {
                        let addr = self.get_or_alloc_ref(locals, &place)?;
                        let mut arg_vals: Vec<Value> = vec![Value::MutPtr(addr)];
                        for op in args.iter().skip(1) {
                            arg_vals.push(self.eval_operand(locals, op)?);
                        }
                        if let Some(result) = self.try_collection_dispatch(func, &arg_vals, depth)? {
                            if let Some(dst_id) = dst {
                                let i = dst_id.0 as usize;
                                while locals.len() <= i { locals.push(Value::Unit); }
                                locals[i] = result;
                            }
                        }
                        return Ok(());
                    }
                }

                let arg_vals: SimResult<Vec<Value>> = args.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let arg_vals = arg_vals?;

                let result = self.call_function(func, arg_vals, depth + 1)?;

                if let Some(dst_id) = dst {
                    let i = dst_id.0 as usize;
                    while locals.len() <= i { locals.push(Value::Unit); }
                    locals[i] = result;
                }
            }

            Instruction::CallIndirect { dst, callee, args } => {
                let callee_val = self.eval_operand(locals, callee)?;
                let arg_vals: SimResult<Vec<Value>> = args.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let arg_vals = arg_vals?;

                let result = self.call_closure_value(callee_val, arg_vals, depth + 1)?;

                if let Some(dst_id) = dst {
                    let i = dst_id.0 as usize;
                    while locals.len() <= i { locals.push(Value::Unit); }
                    locals[i] = result;
                }
            }

            Instruction::CallExtern { dst, func, args } => {
                // gorget_bytes_write_* need the array passed as MutPtr (in C it's GorgetArray*).
                // Special-case: extract the first arg as a heap-backed MutPtr before normal eval.
                if func.starts_with("gorget_bytes_write_") {
                    let first_place = args.first().and_then(|op| match op {
                        Operand::Copy(p) | Operand::Move(p) => Some(p.clone()),
                        _ => None,
                    });
                    if let Some(place) = first_place {
                        let addr = self.get_or_alloc_ref(locals, &place)?;
                        let mut arg_vals: Vec<Value> = vec![Value::MutPtr(addr)];
                        for op in args.iter().skip(1) {
                            arg_vals.push(self.eval_operand(locals, op)?);
                        }
                        if let Some(result) = self.try_collection_dispatch(func, &arg_vals, depth)? {
                            if let Some(dst_id) = dst {
                                let i = dst_id.0 as usize;
                                while locals.len() <= i { locals.push(Value::Unit); }
                                locals[i] = result;
                            }
                        }
                        return Ok(());
                    }
                }

                let arg_vals: SimResult<Vec<Value>> = args.iter()
                    .map(|op| self.eval_operand(locals, op))
                    .collect();
                let mut arg_vals = arg_vals?;

                // Handle in-place mutation of GorgetString (append, push_char, clear, etc.).
                // These take &GorgetString as first arg (a MutPtr) and modify the heap value.
                // We must NOT auto-deref these — we need the address to write back.
                if let Some(result) = self.try_string_mutation(func, &arg_vals)? {
                    if let Some(dst_id) = dst {
                        let i = dst_id.0 as usize;
                        while locals.len() <= i { locals.push(Value::Unit); }
                        locals[i] = result;
                    }
                } else {
                    // For GorgetString/Str query-only method calls, auto-deref the first arg.
                    // The C backend takes &GorgetString and coerces to Str; in the interpreter
                    // we just deref through the heap to get the String/Str value.
                    if self.is_string_method_call(func) {
                        if let Some(first) = arg_vals.first().cloned() {
                            let dereffed = match first {
                                Value::Ptr(addr) | Value::MutPtr(addr) => {
                                    self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                                }
                                other => other,
                            };
                            if let Some(f) = arg_vals.first_mut() { *f = dereffed; }
                        }
                    }

                    // __option_unwrap / __result_unwrap / __option_is_some / __option_is_none:
                    // the first arg is a Ptr/MutPtr to the enum (a borrow of the local).
                    // Deref it so the runtime handler can pattern-match the enum directly.
                    let needs_deref = func.starts_with("__option_unwrap")
                        || func.starts_with("__result_unwrap")
                        || func == "__option_is_some" || func == "__option_is_ok"
                        || func == "__option_is_none" || func == "__option_is_err";
                    if needs_deref {
                        if let Some(first) = arg_vals.first().cloned() {
                            let dereffed = match first {
                                Value::Ptr(addr) | Value::MutPtr(addr) => {
                                    self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                                }
                                other => other,
                            };
                            if let Some(f) = arg_vals.first_mut() { *f = dereffed; }
                        }
                    }

                    // Check for collection method dispatch by function name pattern
                    // Also check module.functions first: GIR lowering uses CallExtern for
                    // user-defined methods like CounterIter__next (via builder.call_extern
                    // in lower_for_iterable). Without this check those calls return Unit,
                    // causing infinite loops.
                    let result = if self.module.find_function(func).is_some() {
                        self.call_function(func, arg_vals, depth + 1)?
                    } else if let Some(result) = self.try_collection_dispatch(func, &arg_vals, depth)? {
                        result
                    } else {
                        runtime::call_extern(
                            func,
                            arg_vals,
                            self.module,
                            &mut self.stdout,
                            &mut self.stderr,
                            depth,
                        )?
                    };

                    if let Some(dst_id) = dst {
                        let i = dst_id.0 as usize;
                        while locals.len() <= i { locals.push(Value::Unit); }
                        locals[i] = result;
                    }
                }
            }

            Instruction::Borrow { dst, place } => {
                // Create an immutable pointer. For simple locals, make them heap-backed
                // so future writes through the pointer are reflected in the local.
                let addr = self.get_or_alloc_ref(locals, place)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::Ptr(addr);
            }

            Instruction::BorrowMut { dst, place } => {
                // Create a mutable pointer. Make the source local heap-backed so that
                // writes through the pointer (e.g. from a capturing closure) propagate
                // back when the outer variable is read after the closure returns.
                let addr = self.get_or_alloc_ref(locals, place)?;
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::MutPtr(addr);
            }

            Instruction::MoveZero { place } => {
                // Zero out the place after a move (Phase 2 ownership tracking)
                let zero = Value::Unit; // simplified zero
                Self::write_place(locals, place, zero, &mut self.heap)?;
            }

            Instruction::Drop { place } => {
                let val = self.read_place(locals, place)?.clone();
                self.run_drop(locals, place, &val, depth)?;
            }
            Instruction::DropIfAlive { place } => {
                let val = self.read_place(locals, place)?.clone();
                // Skip if value is zeroed (already moved/dropped).
                if !matches!(val, Value::Unit) {
                    self.run_drop(locals, place, &val, depth)?;
                }
            }

            Instruction::HeapAlloc { dst, type_id, allocator: _ } => {
                let zero = Value::zero_for_type(*type_id, &self.module.type_registry);
                let addr = self.heap_alloc(zero);
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::MutPtr(addr);
            }

            Instruction::HeapAllocArray { dst, type_id: _, count, allocator: _ } => {
                let n = self.eval_operand(locals, count)?.as_i64() as usize;
                // Allocate an array of n zero values
                let addr = self.heap_alloc(Value::Tuple(vec![Value::Unit; n]));
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = Value::MutPtr(addr);
            }

            Instruction::LoadRef { .. } | Instruction::StoreRef { .. } => { /* TODO: sim interpreter */ }
            Instruction::Dealloc { ptr, allocator: _ } => {
                // P4b: mark the allocation as dead; detect double-free.
                let ptr_val = self.eval_operand(locals, ptr)?;
                if let Value::MutPtr(addr) | Value::Ptr(addr) = ptr_val {
                    if self.ub_checks {
                        if let Some(meta) = self.heap_meta.get_mut(&addr) {
                            if !meta.alive {
                                return Err(SimError::DoubleFree {
                                    addr,
                                    alloc_fn: meta.alloc_fn.clone(),
                                });
                            }
                            meta.alive = false;
                        }
                    } else {
                        // Without UB checks, still mark dead for leak detection if ub_checks later enabled.
                        if let Some(meta) = self.heap_meta.get_mut(&addr) {
                            meta.alive = false;
                        }
                    }
                }
            }

            Instruction::LoadThreadLocal { dst, name } => {
                // Load a thread-local variable (allocator etc.)
                let val = self.globals.get(name.as_str()).cloned().unwrap_or(Value::Unit);
                let i = dst.0 as usize;
                while locals.len() <= i { locals.push(Value::Unit); }
                locals[i] = val;
            }

            Instruction::PushAllocator { allocator } => {
                // If the allocator is a TrackingAllocator (I64 ID), push it to the tracking stack.
                let alloc_val = self.eval_operand(locals, allocator)?;
                match alloc_val {
                    Value::I64(id) if id > 0 && self.tracking_allocs.contains_key(&(id as usize)) => {
                        self.active_tracking.push(id as usize);
                    }
                    _ => {}
                }
            }
            Instruction::PopAllocator => {
                self.active_tracking.pop();
            }

            Instruction::InlineC { code } => {
                // Pattern-match known InlineC patterns for dict/set iteration.
                // All patterns assign to _X from _Y (dict/set local) and optional _Z (index).
                self.eval_inline_c(locals, code)?;
            }

            Instruction::GlobalAssign { .. } => {
                // Global assignments are not executed in the simulator.
            }
        }

        // P4c: mark the instruction's destination local as initialized, regardless of instruction type.
        if self.ub_checks {
            mark_instruction_dst(initialized, inst);
        }

        Ok(())
    }

    /// Execute a function by name with the given arguments.
    pub fn call_function(&mut self, name: &str, args: Vec<Value>, depth: usize) -> SimResult<Value> {
        if depth > MAX_DEPTH {
            return Err(SimError::StackOverflow);
        }

        // ── __gorget_spawn_FN(args...) → call FN synchronously, store result, return task ID ──
        // Gorget `spawn fn(args)` lowers to `__gorget_spawn_fn(args)` + `__gorget_await_fn(task)`.
        // The sim runs them eagerly (single-threaded) to avoid threading complexity.
        if let Some(fn_name) = name.strip_prefix("__gorget_spawn_") {
            let result = self.call_function(fn_name, args, depth + 1)?;
            let task_id = self.task_next_id;
            self.task_next_id += 1;
            self.task_results.insert(task_id, result);
            return Ok(Value::I64(task_id as i64));
        }
        // ── __gorget_await_FN(task_id) → retrieve stored result ──
        if name.starts_with("__gorget_await_") {
            let task_id = args.first().map(|v| v.as_i64()).unwrap_or(0) as usize;
            return Ok(self.task_results.remove(&task_id).unwrap_or(Value::Unit));
        }
        // ── gorget_task_group_* (TaskGroup) ──
        // TaskGroup.spawn(closure) → submit closure; TaskGroup.join() → run all, wait.
        if name == "gorget_task_group_submit" {
            let group = args.get(0).cloned().unwrap_or(Value::Unit);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let addr = match &group {
                Value::MutPtr(a) | Value::Ptr(a) => *a,
                Value::I64(a) => *a as usize,
                _ => 0,
            };
            self.task_group_tasks.entry(addr).or_default().push(closure);
            return Ok(Value::Unit);
        }
        if name == "gorget_task_group_join" || name == "gorget_task_group_wait" {
            let group = args.first().cloned().unwrap_or(Value::Unit);
            let addr = match &group {
                Value::MutPtr(a) | Value::Ptr(a) => *a,
                Value::I64(a) => *a as usize,
                _ => 0,
            };
            let tasks = self.task_group_tasks.remove(&addr).unwrap_or_default();
            for closure in tasks {
                let _ = self.call_closure_value(closure, vec![], depth + 1)?;
            }
            return Ok(Value::Unit);
        }
        if name == "gorget_task_group_new" {
            let addr = self.heap_alloc(Value::I64(0));
            return Ok(Value::I64(addr as i64));
        }

        // ── __gorget_thread_spawn_FN → call FN immediately, return Thread handle (MutPtr to result) ──
        // Threads are simulated sequentially: the spawned function runs eagerly before join().
        if let Some(fn_name) = name.strip_prefix("__gorget_thread_spawn_") {
            let result = self.call_function(fn_name, args, depth + 1)?;
            let tid = self.thread_next_id;
            self.thread_next_id += 1;
            // Store as a Struct { fields: [result, tid] } so join() and id() can both work.
            // fields[0] = return value, fields[1] = thread ID.
            let handle = Value::Struct {
                type_name: "__ThreadHandle".to_string(),
                fields: vec![result, Value::I64(tid as i64)],
            };
            let addr = self.heap_alloc(handle);
            return Ok(Value::MutPtr(addr));
        }
        // ── gorget_current_thread_id → return 1 (main thread; non-zero like pthread_self()) ──
        if name == "gorget_current_thread_id" {
            return Ok(Value::I64(1));
        }

        // ── TCP socket operations ─────────────────────────────────────────────
        if name == "socket_connect" {
            runtime::check_isolation("socket_connect")?;
            let host = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let port = args.get(1).map(|v| v.as_i64() as u16).unwrap_or(0);
            
            let addr_str = format!("{host}:{port}");
            let sock_addr: std::net::SocketAddr = match addr_str.parse() {
                Ok(a) => a,
                Err(_) => {
                    return Ok(Value::Enum {
                        type_name: "Result__Socket__Str".to_string(),
                        tag: 1, variant: "Error".to_string(),
                        fields: vec![Value::Str(super::value::SimStr::from_str("invalid address"))],
                    });
                }
            };
            let timeout = std::time::Duration::from_secs(5);
            return Ok(match std::net::TcpStream::connect_timeout(&sock_addr, timeout) {
                Ok(stream) => {
                    let id = self.socket_next_id;
                    self.socket_next_id += 1;
                    self.tcp_sockets.insert(id, stream);
                    Value::Enum {
                        type_name: "Result__Socket__Str".to_string(),
                        tag: 0, variant: "Ok".to_string(),
                        fields: vec![Value::Struct {
                            type_name: "Socket".to_string(),
                            fields: vec![Value::I64(id as i64)],
                        }],
                    }
                }
                Err(e) => Value::Enum {
                    type_name: "Result__Socket__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(super::value::SimStr::from_string(e.to_string()))],
                },
            });
        }
        if name == "gorget_socket_close" || name == "Socket__close" {
            let id = self.extract_socket_id(&args);
            self.tcp_sockets.remove(&id);
            return Ok(Value::Unit);
        }
        if name == "gorget_socket_write_str" || name == "Socket__write_str" {
            use std::io::Write;
            let id = self.extract_socket_id(&args);
            let s = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(stream) = self.tcp_sockets.get_mut(&id) {
                let _ = stream.write_all(s.as_bytes());
            }
            return Ok(Value::I64(s.len() as i64));
        }
        if name == "gorget_socket_write" || name == "Socket__write" {
            use std::io::Write;
            let id = self.extract_socket_id(&args);
            let bytes = args.get(1)
                .and_then(|v| if let Value::Array(a) = v { Some(a.to_vec()) } else { None })
                .unwrap_or_default();
            let byte_vec: Vec<u8> = bytes.iter().map(|v| v.as_i64() as u8).collect();
            let n = byte_vec.len();
            if let Some(stream) = self.tcp_sockets.get_mut(&id) {
                let _ = stream.write_all(&byte_vec);
            }
            return Ok(Value::I64(n as i64));
        }
        if name == "gorget_socket_read_line" || name == "Socket__read_line" {
            use std::io::BufRead;
            let id = self.extract_socket_id(&args);
            if let Some(stream) = self.tcp_sockets.get_mut(&id) {
                let mut reader = std::io::BufReader::new(stream);
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(0) => return Ok(Value::Enum {
                        type_name: "Result__String__Str".to_string(),
                        tag: 1, variant: "Error".to_string(),
                        fields: vec![Value::Str(super::value::SimStr::from_str("EOF"))],
                    }),
                    Ok(_) => return Ok(Value::Enum {
                        type_name: "Result__String__Str".to_string(),
                        tag: 0, variant: "Ok".to_string(),
                        fields: vec![Value::String(super::value::SimString::from_string(line))],
                    }),
                    Err(e) => return Ok(Value::Enum {
                        type_name: "Result__String__Str".to_string(),
                        tag: 1, variant: "Error".to_string(),
                        fields: vec![Value::Str(super::value::SimStr::from_string(e.to_string()))],
                    }),
                }
            }
            return Ok(Value::Enum {
                type_name: "Result__String__Str".to_string(),
                tag: 1, variant: "Error".to_string(),
                fields: vec![Value::Str(super::value::SimStr::from_str("socket not found"))],
            });
        }
        if name == "gorget_socket_set_timeout" || name == "Socket__set_timeout" {
            let id = self.extract_socket_id(&args);
            let ms = args.get(1).map(|v| v.as_i64() as u64).unwrap_or(0);
            if let Some(stream) = self.tcp_sockets.get_mut(&id) {
                if ms > 0 {
                    let dur = std::time::Duration::from_millis(ms);
                    let _ = stream.set_read_timeout(Some(dur));
                    let _ = stream.set_write_timeout(Some(dur));
                } else {
                    let _ = stream.set_read_timeout(None);
                    let _ = stream.set_write_timeout(None);
                }
            }
            return Ok(Value::Unit);
        }

        // ── UDP socket operations ─────────────────────────────────────────────
        if name == "udp_bind" {
            runtime::check_isolation("udp_bind")?;
            let addr = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let port = args.get(1).map(|v| v.as_i64() as u16).unwrap_or(0);
            let bind_addr = format!("{addr}:{port}");
            // Try the requested port first; fall back to a random port for multicast use-cases
            // where two sockets in the same process need the same port (OS doesn't allow this
            // without SO_REUSEPORT, but in-memory multicast delivery handles it transparently).
            let sock_result = std::net::UdpSocket::bind(&bind_addr)
                .or_else(|_| std::net::UdpSocket::bind(format!("{addr}:0")));
            return Ok(match sock_result {
                Ok(sock) => {
                    let id = self.socket_next_id;
                    self.socket_next_id += 1;
                    self.udp_sockets.insert(id, sock);
                    self.socket_inbox.entry(id).or_default();
                    Value::Enum {
                        type_name: "Result__UdpSocket__Str".to_string(),
                        tag: 0, variant: "Ok".to_string(),
                        fields: vec![Value::Struct {
                            type_name: "UdpSocket".to_string(),
                            fields: vec![Value::I64(id as i64)],
                        }],
                    }
                }
                Err(e) => Value::Enum {
                    type_name: "Result__UdpSocket__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(super::value::SimStr::from_string(e.to_string()))],
                },
            });
        }
        if name == "gorget_udp_close" || name == "UdpSocket__close" {
            let id = self.extract_udp_socket_id(&args);
            self.udp_sockets.remove(&id);
            return Ok(Value::Unit);
        }
        if name == "gorget_udp_send_to" || name == "UdpSocket__send_to" || name == "UdpSocket__sendto" {
            let id = self.extract_udp_socket_id(&args);
            let data = args.get(1)
                .and_then(|v| if let Value::Array(a) = v { Some(a.to_vec()) } else { None })
                .unwrap_or_default();
            let dest_addr = args.get(2).map(|v| v.as_str_lossy()).unwrap_or_default();
            let dest_port = args.get(3).map(|v| v.as_i64() as u16).unwrap_or(0);
            let target = format!("{dest_addr}:{dest_port}");
            let bytes: Vec<u8> = data.iter().map(|v| v.as_i64() as u8).collect();
            let n = bytes.len();
            // Simulate multicast: deliver to all in-process sockets that joined this group.
            let is_multicast = dest_addr.starts_with("224.") || dest_addr.starts_with("239.")
                || dest_addr.starts_with("225.") || dest_addr.starts_with("226.");
            if is_multicast {
                // Get sender's local address for the inbox "from" field
                let sender_addr = self.udp_sockets.get(&id)
                    .and_then(|s| s.local_addr().ok())
                    .unwrap_or_else(|| "127.0.0.1:0".parse().unwrap());
                let subscribers = self.multicast_subs.get(&dest_addr)
                    .cloned().unwrap_or_default();
                for &sub_id in &subscribers {
                    self.socket_inbox.entry(sub_id).or_default()
                        .push_back((bytes.clone(), sender_addr));
                }
            }
            // Also send via real UDP (best-effort — may fail for multicast without OS support)
            if let Some(sock) = self.udp_sockets.get(&id) {
                let _ = sock.send_to(&bytes, &target);
            }
            return Ok(Value::Enum {
                type_name: "Result__int64_t__Str".to_string(),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![Value::I64(n as i64)],
            });
        }
        if name == "gorget_udp_recv_from" || name == "UdpSocket__recv_from" || name == "UdpSocket__recvfrom" {
            let id = self.extract_udp_socket_id(&args);
            let max_len = args.get(1).map(|v| v.as_i64() as usize).unwrap_or(65536);
            // Helper to build a Result[UdpPacket, str] Ok value from bytes + sender addr
            let make_packet = |bytes: &[u8], src: std::net::SocketAddr| -> Value {
                let arr = super::value::SimArray::new("uint8_t");
                let n = bytes.len().min(max_len);
                for &b in &bytes[..n] { arr.push(Value::U8(b)); }
                let sender = Value::Struct {
                    type_name: "UdpAddr".to_string(),
                    fields: vec![
                        Value::Str(super::value::SimStr::from_string(src.ip().to_string())),
                        Value::I64(src.port() as i64),
                    ],
                };
                Value::Enum {
                    type_name: "Result__UdpPacket__Str".to_string(),
                    tag: 0, variant: "Ok".to_string(),
                    fields: vec![Value::Struct {
                        type_name: "UdpPacket".to_string(),
                        fields: vec![Value::Array(arr), sender],
                    }],
                }
            };
            // Check in-process inbox first (multicast-delivered packets)
            if let Some(queue) = self.socket_inbox.get_mut(&id) {
                if let Some((bytes, src)) = queue.pop_front() {
                    return Ok(make_packet(&bytes, src));
                }
            }
            // Fall through to real socket
            if let Some(sock) = self.udp_sockets.get(&id) {
                let mut buf = vec![0u8; max_len];
                match sock.recv_from(&mut buf) {
                    Ok((n, src)) => return Ok(make_packet(&buf[..n], src)),
                    Err(e) => return Ok(Value::Enum {
                        type_name: "Result__UdpPacket__Str".to_string(),
                        tag: 1, variant: "Error".to_string(),
                        fields: vec![Value::Str(super::value::SimStr::from_string(e.to_string()))],
                    }),
                }
            }
            return Ok(Value::Enum {
                type_name: "Result__UdpPacket__Str".to_string(),
                tag: 1, variant: "Error".to_string(),
                fields: vec![Value::Str(super::value::SimStr::from_str("socket not found"))],
            });
        }
        if name == "gorget_udp_poll" {
            // Poll the socket for readability within the given timeout.
            // Check the in-process inbox first (for multicast-delivered packets).
            let id = self.extract_udp_socket_id(&args);
            let timeout_ms = args.get(1).map(|v| v.as_i64() as u64).unwrap_or(0);
            // Inbox check (always non-blocking, returns immediately if data available)
            if self.socket_inbox.get(&id).map_or(false, |q| !q.is_empty()) {
                return Ok(Value::Bool(true));
            }
            if let Some(sock) = self.udp_sockets.get(&id) {
                let dur = if timeout_ms > 0 {
                    std::time::Duration::from_millis(timeout_ms)
                } else {
                    std::time::Duration::from_millis(1)
                };
                let _ = sock.set_read_timeout(Some(dur));
                let mut peek_buf = [0u8; 1];
                let ready = sock.peek_from(&mut peek_buf).is_ok();
                // Restore blocking mode (no timeout)
                let _ = sock.set_read_timeout(None);
                return Ok(Value::Bool(ready));
            }
            return Ok(Value::Bool(false));
        }
        if name == "gorget_udp_set_nonblocking" || name == "UdpSocket__set_nonblocking" {
            let id = self.extract_udp_socket_id(&args);
            let enabled = args.get(1).map(|v| v.as_bool()).unwrap_or(false);
            if let Some(sock) = self.udp_sockets.get(&id) {
                let _ = sock.set_nonblocking(enabled);
            }
            return Ok(Value::Unit);
        }
        if name == "gorget_udp_set_multicast_loopback" {
            let id = self.extract_udp_socket_id(&args);
            let enabled = args.get(1).map(|v| v.as_bool()).unwrap_or(true);
            if let Some(sock) = self.udp_sockets.get(&id) {
                let _ = sock.set_multicast_loop_v4(enabled);
            }
            return Ok(Value::Unit);
        }
        if name == "gorget_udp_leave_multicast" {
            // No-op in sim — just return Unit
            return Ok(Value::Unit);
        }
        if name == "gorget_udp_join_multicast" || name == "UdpSocket__join_multicast" {
            let id = self.extract_udp_socket_id(&args);
            let group_str = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            // Register in in-process multicast subscription table regardless of OS support.
            self.multicast_subs.entry(group_str.clone()).or_default().push(id);
            // Also try to join via OS (best-effort — may fail if socket isn't on multicast port)
            if let Ok(group_addr) = group_str.parse::<std::net::Ipv4Addr>() {
                if let Some(sock) = self.udp_sockets.get(&id) {
                    let _ = sock.join_multicast_v4(&group_addr, &std::net::Ipv4Addr::UNSPECIFIED);
                }
            }
            return Ok(Value::Enum {
                type_name: "Result__bool__Str".to_string(),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![Value::Bool(true)],
            });
        }
        if name == "gorget_udp_local_addr" || name == "UdpSocket__local_addr" {
            let id = self.extract_udp_socket_id(&args);
            if let Some(sock) = self.udp_sockets.get(&id) {
                if let Ok(addr) = sock.local_addr() {
                    return Ok(Value::Struct {
                        type_name: "UdpAddr".to_string(),
                        fields: vec![
                            Value::Str(super::value::SimStr::from_string(addr.ip().to_string())),
                            Value::I64(addr.port() as i64),
                        ],
                    });
                }
            }
            return Ok(Value::Struct {
                type_name: "UdpAddr".to_string(),
                fields: vec![
                    Value::Str(super::value::SimStr::from_str("0.0.0.0")),
                    Value::I64(0),
                ],
            });
        }
        if name == "gorget_udp_set_timeout" || name == "UdpSocket__set_timeout" {
            let id = self.extract_udp_socket_id(&args);
            let ms = args.get(1).map(|v| v.as_i64() as u64).unwrap_or(0);
            if let Some(sock) = self.udp_sockets.get(&id) {
                if ms > 0 {
                    let dur = std::time::Duration::from_millis(ms);
                    let _ = sock.set_read_timeout(Some(dur));
                } else {
                    let _ = sock.set_read_timeout(None);
                }
            }
            return Ok(Value::Unit);
        }

        // ── Crypto operations ─────────────────────────────────────────────────────
        // Helper: extract bytes from a Value::Array argument
        let arg_bytes = |v: Option<&Value>| -> Vec<u8> {
            match v {
                Some(Value::Array(arr)) => arr.to_vec().iter().map(|x| x.as_i64() as u8).collect(),
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    match self.heap_read(*addr) {
                        Ok(Value::Array(arr)) => arr.to_vec().iter().map(|x| x.as_i64() as u8).collect(),
                        _ => vec![],
                    }
                }
                _ => vec![],
            }
        };
        let bytes_to_array_value = |bytes: &[u8]| -> Value {
            use super::value::SimArray;
            let arr = SimArray::new("uint8_t");
            for &b in bytes { arr.push(Value::U8(b)); }
            Value::Array(arr)
        };
        let ok_bytes = |bytes: &[u8], tn: &str| -> Value {
            use super::value::SimArray;
            let arr = SimArray::new("uint8_t");
            for &b in bytes { arr.push(Value::U8(b)); }
            Value::Enum {
                type_name: format!("{tn}__Str"),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![Value::Array(arr)],
            }
        };
        // SHA-256
        if name == "gorget_crypto_sha256" {
            let data = arg_bytes(args.first());
            let hash = super::crypto::sha256(&data);
            return Ok(bytes_to_array_value(&hash));
        }
        // SHA-1
        if name == "gorget_crypto_sha1" {
            let data = arg_bytes(args.first());
            let hash = super::crypto::sha1(&data);
            return Ok(bytes_to_array_value(&hash));
        }
        // crypto_random_bytes(n) -> Result[Vector[uint8], str]
        if name == "crypto_random_bytes" {
            let n = args.first().map(|v| v.as_i64() as usize).unwrap_or(0);
            let bytes = super::crypto::random_bytes(n);
            return Ok(ok_bytes(&bytes, "Result__Vector__uint8_t"));
        }
        // HMAC-SHA256 via crypto_hmac("sha256", key, data) -> Result[Vector[uint8], str]
        if name == "crypto_hmac" {
            let algo = args.first().map(|v| v.as_str_lossy()).unwrap_or_default();
            let key = arg_bytes(args.get(1));
            let data = arg_bytes(args.get(2));
            if algo == "sha256" {
                let mac = super::crypto::hmac_sha256(&key, &data);
                return Ok(ok_bytes(&mac, "Result__Vector__uint8_t"));
            } else {
                return Ok(Value::Enum {
                    type_name: "Result__Vector__uint8_t__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_string(format!("unsupported HMAC algorithm: {algo}")))],
                });
            }
        }
        // HKDF-SHA256 -> Result[Vector[uint8], str]
        if name == "crypto_hkdf_sha256" {
            let salt = arg_bytes(args.first());
            let ikm = arg_bytes(args.get(1));
            let info = arg_bytes(args.get(2));
            let len = args.get(3).map(|v| v.as_i64() as usize).unwrap_or(32);
            let out = super::crypto::hkdf_sha256(&salt, &ikm, &info, len);
            return Ok(ok_bytes(&out, "Result__Vector__uint8_t"));
        }
        // AES-GCM encrypt: (key, nonce, plaintext) -> Result[Vector[uint8], str]
        if name == "crypto_aes_gcm_encrypt" {
            let key = arg_bytes(args.first());
            let nonce = arg_bytes(args.get(1));
            let plaintext = arg_bytes(args.get(2));
            let ct = super::crypto::aes_gcm_encrypt(&key, &nonce, &plaintext);
            return Ok(ok_bytes(&ct, "Result__Vector__uint8_t"));
        }
        // AES-GCM decrypt: (key, ciphertext) -> Result[Vector[uint8], str]
        if name == "crypto_aes_gcm_decrypt" {
            let key = arg_bytes(args.first());
            let ct = arg_bytes(args.get(1));
            return Ok(match super::crypto::aes_gcm_decrypt(&key, &ct) {
                Ok(pt) => ok_bytes(&pt, "Result__Vector__uint8_t"),
                Err(e) => Value::Enum {
                    type_name: "Result__Vector__uint8_t__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_str(e))],
                },
            });
        }
        // AES-CTR cipher context: crypto_aes_ctr_new(key, iv) -> Result[CipherContext, str]
        if name == "crypto_aes_ctr_new" {
            let key = arg_bytes(args.first());
            let iv = arg_bytes(args.get(1));
            if key.len() != 16 && key.len() != 24 && key.len() != 32 {
                return Ok(Value::Enum {
                    type_name: "Result__CipherContext__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_str("invalid AES key size"))],
                });
            }
            // CipherContext = struct with key and iv stored as arrays
            let ctx = Value::Struct {
                type_name: "CipherContext".to_string(),
                fields: vec![
                    bytes_to_array_value(&key),
                    bytes_to_array_value(&iv),
                ],
            };
            return Ok(Value::Enum {
                type_name: "Result__CipherContext__Str".to_string(),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![ctx],
            });
        }
        // CipherContext::encrypt(&self, data) and decrypt(&self, data) — XOR cipher
        if name == "gorget_cipher_encrypt" || name == "gorget_cipher_decrypt" {
            // arg[0] = Ptr/MutPtr to CipherContext, arg[1] = data bytes
            let ctx_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).ok().cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            let (key, iv) = match &ctx_val {
                Value::Struct { fields, .. } if fields.len() >= 2 => {
                    let k = match &fields[0] { Value::Array(a) => a.to_vec().iter().map(|x| x.as_i64() as u8).collect::<Vec<_>>(), _ => vec![] };
                    let i = match &fields[1] { Value::Array(a) => a.to_vec().iter().map(|x| x.as_i64() as u8).collect::<Vec<_>>(), _ => vec![] };
                    (k, i)
                }
                _ => (vec![], vec![]),
            };
            let data = arg_bytes(args.get(1));
            let ks = super::crypto::aes_ctr_keystream(&key, &iv, data.len());
            let out: Vec<u8> = data.iter().zip(ks.iter()).map(|(a, b)| a ^ b).collect();
            return Ok(bytes_to_array_value(&out));
        }
        // Ed25519 keygen -> Result[Ed25519KeyPair, str]
        if name == "crypto_ed25519_keygen" {
            let (priv_arr, pub_arr) = super::crypto::ed25519_keygen();
            let kp = Value::Struct {
                type_name: "Ed25519KeyPair".to_string(),
                fields: vec![
                    bytes_to_array_value(&pub_arr),  // fields[0] = public_key
                    bytes_to_array_value(&priv_arr), // fields[1] = private_key
                ],
            };
            return Ok(Value::Enum {
                type_name: "Result__Ed25519KeyPair__Str".to_string(),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![kp],
            });
        }
        // Ed25519KeyPair::public_key() -> Vector[uint8]
        if name == "gorget_ed25519_public_key" || name == "Ed25519KeyPair__public_key" {
            let kp_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            return Ok(match kp_val {
                Value::Struct { ref fields, .. } => fields.first().cloned().unwrap_or(Value::Unit),
                _ => Value::Unit,
            });
        }
        // Ed25519KeyPair::private_key() -> Vector[uint8]
        if name == "gorget_ed25519_private_key" || name == "Ed25519KeyPair__private_key" {
            let kp_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            return Ok(match kp_val {
                Value::Struct { ref fields, .. } => fields.get(1).cloned().unwrap_or(Value::Unit),
                _ => Value::Unit,
            });
        }
        // crypto_ed25519_sign(priv_key, data) -> Result[Vector[uint8], str]
        if name == "crypto_ed25519_sign" {
            let priv_key = arg_bytes(args.first());
            let data = arg_bytes(args.get(1));
            let sig = super::crypto::ed25519_sign(&priv_key, &data);
            return Ok(ok_bytes(&sig, "Result__Vector__uint8_t"));
        }
        // gorget_crypto_ed25519_verify(pub_key, data, signature) -> bool (always true)
        if name == "gorget_crypto_ed25519_verify" {
            return Ok(Value::Bool(true));
        }
        // X25519 keygen -> Result[X25519KeyPair, str]
        if name == "crypto_x25519_keygen" {
            let (priv_arr, pub_arr) = super::crypto::x25519_keygen();
            let kp = Value::Struct {
                type_name: "X25519KeyPair".to_string(),
                fields: vec![
                    bytes_to_array_value(&pub_arr),  // fields[0] = public_key
                    bytes_to_array_value(&priv_arr), // fields[1] = private_key
                ],
            };
            return Ok(Value::Enum {
                type_name: "Result__X25519KeyPair__Str".to_string(),
                tag: 0, variant: "Ok".to_string(),
                fields: vec![kp],
            });
        }
        // X25519KeyPair::public_key() -> Vector[uint8]
        if name == "gorget_crypto_x25519_public" || name == "X25519KeyPair__public_key" {
            let kp_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            return Ok(match kp_val {
                Value::Struct { ref fields, .. } => fields.first().cloned().unwrap_or(Value::Unit),
                _ => Value::Unit,
            });
        }
        // X25519KeyPair::private_key() -> Vector[uint8]
        if name == "gorget_crypto_x25519_private" || name == "X25519KeyPair__private_key" {
            let kp_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            return Ok(match kp_val {
                Value::Struct { ref fields, .. } => fields.get(1).cloned().unwrap_or(Value::Unit),
                _ => Value::Unit,
            });
        }
        // crypto_x25519_shared_secret(keypair, peer_pub) -> Result[Vector[uint8], str]
        if name == "crypto_x25519_shared_secret" {
            // keypair is an X25519KeyPair struct (fields[0]=pub, fields[1]=priv)
            let kp_val = match args.first() {
                Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                    self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
                }
                Some(v) => v.clone(),
                None => Value::Unit,
            };
            let self_pub = match &kp_val {
                Value::Struct { fields, .. } => match fields.first() {
                    Some(Value::Array(a)) => a.to_vec().iter().map(|x| x.as_i64() as u8).collect::<Vec<_>>(),
                    _ => vec![],
                },
                _ => vec![],
            };
            let peer_pub = arg_bytes(args.get(1));
            let shared = super::crypto::x25519_shared_secret_from_pub(&self_pub, &peer_pub);
            return Ok(ok_bytes(&shared, "Result__Vector__uint8_t"));
        }
        // crypto_x25519_dh(priv_key_bytes, peer_pub) -> Result[Vector[uint8], str]
        if name == "crypto_x25519_dh" {
            let priv_bytes = arg_bytes(args.first());
            let peer_pub = arg_bytes(args.get(1));
            let shared = super::crypto::x25519_dh(&priv_bytes, &peer_pub);
            return Ok(ok_bytes(&shared, "Result__Vector__uint8_t"));
        }

        // bytes_to_str intercept: the C codegen bypasses the Gorget wrapper function and emits
        // UTF-8 validation inline. In the sim we mirror this by intercepting at call time.
        if name == "bytes_to_str" {
            let arr_arg = args.into_iter().next().unwrap_or(Value::Null);
            let bytes: Vec<u8> = if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.to_vec().into_iter().map(|v| v.as_i64() as u8).collect()
            } else { vec![] };
            return match std::str::from_utf8(&bytes) {
                Ok(s) => Ok(Value::Enum {
                    type_name: "Result__Str__Str".to_string(),
                    tag: 0, variant: "Ok".to_string(),
                    fields: vec![Value::Str(SimStr::from_string(s.to_string()))],
                }),
                Err(_) => Ok(Value::Enum {
                    type_name: "Result__Str__Str".to_string(),
                    tag: 1, variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_str("invalid UTF-8 in byte buffer"))],
                }),
            };
        }

        // Find the function in the module
        if let Some(func) = self.module.find_function(name) {
            let func = func.clone(); // Clone to avoid borrow issues
            // Track current function name for error context (P4 heap allocation recording).
            // Restored after the function returns so callers record correct alloc_fn.
            let mut prev_fn_name = std::mem::replace(&mut self.current_fn_name, name.to_string());

            // P7c: push a stack frame; record depth so we can truncate on error.
            let stack_depth_before = self.call_stack.len();
            self.call_stack.push(StackFrame {
                fn_name: name.to_string(),
                display_name: func.display_name.clone(),
                call_span: self.current_instr_span,
                def_span: func.def_span,
                current_span: None,
            });

            // Initialize locals: _0 = return slot (Unit), _1..N = params
            let num_args = args.len();
            let mut locals = vec![Value::Unit; func.locals.len().max(num_args + 1)];
            for (i, arg) in args.into_iter().enumerate() {
                if i + 1 < locals.len() {
                    locals[i + 1] = arg;
                }
            }

            // P4c: track initialized locals. _0 (return slot) and params are pre-initialized.
            let mut initialized: HashSet<u32> = HashSet::new();
            initialized.insert(0); // _0 = return slot
            for i in 1..=(num_args as u32) {
                initialized.insert(i);
            }

            // Helper: capture backtrace on the first error, then restore stack/name.
            // Used at every error exit path inside this function.
            // Uses std::mem::take to avoid a move-out-of-captured-variable issue.
            macro_rules! sim_error_return {
                ($self:expr, $e:expr, $stack_depth:expr, $prev_name:expr) => {{
                    if $self.last_error_backtrace.is_none() {
                        $self.last_error_span = $self.current_instr_span;
                        // Capture backtrace with a shared borrow, then assign with &mut.
                        let _bt = $self.capture_backtrace();
                        $self.last_error_backtrace = Some(_bt);
                    }
                    $self.call_stack.truncate($stack_depth);
                    $self.current_fn_name = std::mem::take(&mut $prev_name);
                    return Err($e);
                }};
            }

            // Execute blocks
            let mut current_block = 0usize;
            loop {
                if current_block >= func.blocks.len() {
                    sim_error_return!(self, SimError::MissingTerminator(current_block), stack_depth_before, prev_fn_name);
                }
                let block = &func.blocks[current_block];
                let instructions = block.instructions.clone();
                let span_map = block.span_map.clone();
                let terminator = block.terminator.clone();

                for (idx, inst) in instructions.iter().enumerate() {
                    // P7c: update current span for error context.
                    let instr_span = span_map.get(idx).copied().flatten();
                    self.current_instr_span = instr_span;
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.current_span = instr_span;
                    }

                    match self.execute_instruction(&mut locals, &mut initialized, inst, depth) {
                        Ok(()) => {
                            // Post-call zero: after consuming an element into a collection,
                            // zero the source local to prevent double-free. Mirrors the C
                            // backend's `memset(&_N, 0, sizeof(Type))` emitted after push/set.
                            if let Instruction::Call { func: call_fn, args, .. }
                                | Instruction::CallExtern { func: call_fn, args, .. } = inst
                            {
                                if let Some(elem_idx) = consuming_collection_arg_idx(call_fn) {
                                    if let Some(Operand::Copy(place) | Operand::Move(place)) = args.get(elem_idx) {
                                        if place.projections.is_empty() {
                                            let lid = place.local.0 as usize;
                                            let droppable = func.locals.get(lid)
                                                .map(|l| self.is_type_id_droppable(l.type_id))
                                                .unwrap_or(false);
                                            if droppable {
                                                local_set(&mut locals, lid, Value::Unit);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Err(SimError::Panic(msg)) => {
                            // Run cleanup (like __gorget_cleanup_run) before propagating panic.
                            // This drops droppable locals registered via cleanup_push in C.
                            self.cleanup_locals_on_panic(&func, &locals, depth);
                            sim_error_return!(self, SimError::Panic(msg), stack_depth_before, prev_fn_name);
                        }
                        Err(e) => {
                            sim_error_return!(self, e, stack_depth_before, prev_fn_name);
                        }
                    }
                }

                match terminator {
                    Some(Terminator::Return(op)) => {
                        match self.eval_operand(&locals, &op) {
                            Ok(ret) => {
                                self.call_stack.truncate(stack_depth_before);
                                self.current_fn_name = prev_fn_name;
                                return Ok(ret);
                            }
                            Err(e) => {
                                sim_error_return!(self, e, stack_depth_before, prev_fn_name);
                            }
                        }
                    }
                    Some(Terminator::Jump(bid)) => {
                        current_block = bid.0 as usize;
                    }
                    Some(Terminator::Branch { cond, then_block, else_block }) => {
                        let cond_val = match self.eval_operand(&locals, &cond) {
                            Ok(v) => v,
                            Err(e) => sim_error_return!(self, e, stack_depth_before, prev_fn_name),
                        };
                        current_block = if cond_val.as_bool() {
                            then_block.0 as usize
                        } else {
                            else_block.0 as usize
                        };
                    }
                    Some(Terminator::Switch { value, cases, default }) => {
                        let switch_val = match self.eval_operand(&locals, &value) {
                            Ok(v) => v.as_i64(),
                            Err(e) => sim_error_return!(self, e, stack_depth_before, prev_fn_name),
                        };
                        current_block = cases.iter()
                            .find(|(k, _)| *k == switch_val)
                            .map(|(_, b)| b.0 as usize)
                            .unwrap_or(default.0 as usize);
                    }
                    Some(Terminator::Invoke { func: inv_func, args: inv_args, dst: inv_dst, normal, error: _ }) => {
                        // Invoke: try to call func, on panic go to error block.
                        // For Phase 0: simplified — just call and jump to normal.
                        let arg_vals: SimResult<Vec<Value>> = inv_args.iter()
                            .map(|op| self.eval_operand(&locals, op))
                            .collect();
                        let arg_vals = match arg_vals {
                            Ok(v) => v,
                            Err(e) => sim_error_return!(self, e, stack_depth_before, prev_fn_name),
                        };
                        let result = match self.call_function(&inv_func, arg_vals, depth + 1) {
                            Ok(v) => v,
                            Err(e) => sim_error_return!(self, e, stack_depth_before, prev_fn_name),
                        };
                        if let Some(dst_id) = inv_dst {
                            let i = dst_id.0 as usize;
                            while locals.len() <= i { locals.push(Value::Unit); }
                            locals[i] = result;
                        }
                        current_block = normal.0 as usize;
                    }
                    Some(Terminator::Unreachable) => {
                        sim_error_return!(self, SimError::Unreachable, stack_depth_before, prev_fn_name);
                    }
                    None => {
                        sim_error_return!(self, SimError::MissingTerminator(current_block), stack_depth_before, prev_fn_name);
                    }
                }
            }
        } else {
            // Function not found in module — check if it's a collection method or runtime function.
            // Handle mutating string operations BEFORE auto-deref (mutation needs the address).
            if let Some(result) = self.try_string_mutation(name, &args)? {
                return Ok(result);
            }
            // Try collection dispatch (Box, Vector, Dict, Set, etc.) before auto-deref.
            if let Some(result) = self.try_collection_dispatch(name, &args, depth)? {
                return Ok(result);
            }
            // Handle __callable_N and __gorget_closure_call_N — indirect calls through
            // Callable[sig] function parameters. args[0] = closure value, args[1..] = actual args.
            if name.starts_with("__callable_") || name.starts_with("__gorget_closure_call_") {
                if let Some(closure) = args.first().cloned() {
                    let rest = args[1..].to_vec();
                    return self.call_closure_value(closure, rest, depth);
                }
                return Ok(Value::Unit);
            }
            // `len(x)` free function — dispatch based on value type.
            // For user structs (Measurable), call TypeName__len(&x).
            if name == "len" {
                if let Some(arg) = args.into_iter().next() {
                    return match &arg {
                        Value::Array(a) => Ok(Value::I64(a.len() as i64)),
                        Value::Dict(d) => Ok(Value::I64(d.len() as i64)),
                        Value::Str(s) => Ok(Value::I64(s.codepoint_count() as i64)),
                        Value::String(s) => Ok(Value::I64(s.as_str().chars().count() as i64)),
                        Value::Struct { type_name, .. } => {
                            let len_fn = format!("{}__len", type_name);
                            let addr = self.heap_alloc(arg.clone());
                            self.call_function(&len_fn, vec![Value::MutPtr(addr)], depth + 1)
                        }
                        Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                            let inner = self.heap_read(*addr).cloned().unwrap_or(Value::Unit);
                            match inner {
                                Value::Array(a) => Ok(Value::I64(a.len() as i64)),
                                Value::Dict(d) => Ok(Value::I64(d.len() as i64)),
                                Value::Str(s) => Ok(Value::I64(s.codepoint_count() as i64)),
                                Value::String(s) => Ok(Value::I64(s.as_str().chars().count() as i64)),
                                Value::Struct { ref type_name, .. } => {
                                    let len_fn = format!("{}__len", type_name);
                                    self.call_function(&len_fn, vec![Value::MutPtr(*addr)], depth + 1)
                                }
                                _ => Ok(Value::I64(0)),
                            }
                        }
                        _ => Ok(Value::I64(0)),
                    };
                }
                return Ok(Value::I64(0));
            }
            // Auto-deref pointer args for string method calls (same as CallExtern path).
            let args = if self.is_string_method_call(name) {
                if let Some(first) = args.first().cloned() {
                    let dereffed = match first {
                        Value::Ptr(addr) | Value::MutPtr(addr) => {
                            self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                        }
                        other => other,
                    };
                    let mut v = args;
                    if let Some(f) = v.first_mut() { *f = dereffed; }
                    v
                } else {
                    args
                }
            } else {
                args
            };
            // Regex dispatch (before runtime fallback)
            if let Some(result) = self.try_regex_dispatch(name, &args)? {
                return Ok(result);
            }
            let result = runtime::call_extern(
                name,
                args,
                self.module,
                &mut self.stdout,
                &mut self.stderr,
                depth,
            )?;
            Ok(result)
        }
    }

    /// Invoke a closure value with the given args.
    /// Handles: Struct { type_name: "__Closure_N", ... } → call __Closure_N__call
    ///          FuncRef(name) → call_function(name, args)
    ///          Ptr/MutPtr/Ref → deref first
    fn call_closure_value(&mut self, closure: Value, args: Vec<Value>, depth: usize) -> SimResult<Value> {
        match closure {
            Value::Struct { ref type_name, .. } => {
                let call_fn = format!("{}__call", type_name);
                // Allocate the closure struct on the heap and pass a MutPtr to __call.
                // __call's signature is: RetType __call(ClosureType* __self, params...)
                let addr = self.heap_alloc(closure.clone());
                let ptr = Value::MutPtr(addr);
                let mut call_args = vec![ptr];
                call_args.extend(args);
                self.call_function(&call_fn, call_args, depth + 1)
            }
            Value::FuncRef(name) => {
                self.call_function(&name, args, depth + 1)
            }
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                let inner = self.heap_read(addr).cloned().unwrap_or(Value::Unit);
                self.call_closure_value(inner, args, depth)
            }
            Value::Tuple(ref elems) if !elems.is_empty() => {
                // [fn_ptr, env_ptr] ABI tuple
                let fn_val = elems[0].clone();
                self.call_closure_value(fn_val, args, depth)
            }
            _ => Ok(Value::Unit),
        }
    }

    /// Execute the drop strategy for a value at the given place.
    /// Handles Struct, Enum, Array (Vector), and MutPtr (Box) types.
    fn run_drop(&mut self, locals: &mut Vec<Value>, place: &Place, val: &Value, depth: usize) -> SimResult<()> {
        // Delegate to the place-free helper, then zero the place.
        let needs_zero = self.run_drop_value(val, depth)?;
        if needs_zero {
            Self::write_place(locals, place, Value::Unit, &mut self.heap)?;
        }
        Ok(())
    }

    /// Drop a value without needing a place (used for array elements, Box inner values, etc.).
    /// Returns true if the caller should zero out the place (i.e., a non-trivial drop fired).
    pub fn run_drop_value(&mut self, val: &Value, depth: usize) -> SimResult<bool> {
        use crate::ir::types::DropStrategy;
        match val {
            // Array (Vector[T]): iterate elements forward and drop each (matches C backend order).
            Value::Array(arr) => {
                let elements = arr.to_vec();
                for elem in elements.iter() {
                    self.run_drop_value(elem, depth)?;
                }
                return Ok(true);
            }
            // Ref: Weak[T] handle. Decrement weak count; remove entry when it hits 0.
            // Weak handles use Value::Ref so they can be distinguished from Shared (Value::Ptr).
            Value::Ref(addr) => {
                let addr = *addr;
                if addr != 0 && self.shared_refcounts.contains_key(&addr) {
                    if let Some(rc) = self.shared_refcounts.get_mut(&addr) {
                        rc.1 -= 1;
                        if rc.1 <= 0 {
                            self.shared_refcounts.remove(&addr);
                        }
                    }
                    return Ok(false);
                }
                // Non-Weak Ref — treat as no-op (Ref is typically a borrow, not owning).
                return Ok(false);
            }
            // MutPtr / Ptr: either Box[T] (free inner data) or Shared[T] (decrement strong ref).
            // Shared[T] handles use Value::Ptr with an entry in shared_refcounts.
            // Box[T] uses this path without a shared_refcounts entry.
            // Guard[T]/Mutex[T] also pass through here, but since they don't own the allocation,
            // double-drop won't naturally occur.
            Value::Ptr(addr) => {
                let addr = *addr;
                if addr != 0 {
                    // Shared[T] drop: decrement strong count; free data+collective-weak at 0.
                    if self.shared_refcounts.contains_key(&addr) {
                        if let Some(rc) = self.shared_refcounts.get_mut(&addr) {
                            rc.0 -= 1;
                            if rc.0 <= 0 {
                                self.heap.remove(&addr);
                                rc.1 -= 1; // release collective weak ref held by all strongs
                                if rc.1 <= 0 {
                                    self.shared_refcounts.remove(&addr);
                                }
                            }
                        }
                        return Ok(false);
                    }
                    // P6b: detect double-free for Box[T].
                    if self.ub_checks {
                        if let Some(meta) = self.heap_meta.get(&addr) {
                            if !meta.alive {
                                return Err(SimError::DoubleFree {
                                    addr,
                                    alloc_fn: meta.alloc_fn.clone(),
                                });
                            }
                        }
                    }
                    // Read inner value (UseAfterFree if dead and ub_checks enabled).
                    let inner = self.heap_read(addr).cloned()?;
                    self.run_drop_value(&inner, depth)?;
                    // Mark heap slot dead after dropping Box contents.
                    if let Some(meta) = self.heap_meta.get_mut(&addr) {
                        meta.alive = false;
                    }
                }
                return Ok(false); // Don't zero the place for ptr drops
            }
            Value::MutPtr(addr) => {
                let addr = *addr;
                if addr != 0 {
                    // P6b: detect double-free — if the slot is already dead, this is a
                    // second drop of the same Box (double-free UB).
                    if self.ub_checks {
                        if let Some(meta) = self.heap_meta.get(&addr) {
                            if !meta.alive {
                                return Err(SimError::DoubleFree {
                                    addr,
                                    alloc_fn: meta.alloc_fn.clone(),
                                });
                            }
                        }
                    }
                    // Read inner value (UseAfterFree if dead and ub_checks enabled).
                    let inner = self.heap_read(addr).cloned()?;
                    self.run_drop_value(&inner, depth)?;
                    // Mark heap slot dead after dropping Box contents.
                    if let Some(meta) = self.heap_meta.get_mut(&addr) {
                        meta.alive = false;
                    }
                }
                return Ok(false); // Don't zero the place for ptr drops
            }
            // Struct / Enum: dispatch on the type's drop strategy.
            Value::Struct { type_name, fields } => {
                let type_name = type_name.clone();
                let fields = fields.clone();
                let strategy = self.module.type_registry.get_type_def(&type_name)
                    .map(|td| td.metadata.drop_strategy.clone())
                    .unwrap_or(DropStrategy::None);
                match strategy {
                    DropStrategy::Custom(fn_name) => {
                        // Use ref-promoted so the temp heap copy is excluded from leak
                        // detection — it's an implementation artifact, not a user alloc.
                        let addr = self.heap_alloc_ref_promoted(val.clone());
                        let ptr = Value::MutPtr(addr);
                        self.call_function(&fn_name, vec![ptr], depth + 1)?;
                        // Drop droppable fields after custom drop.
                        for field_val in fields.iter().rev() {
                            self.run_drop_value(field_val, depth)?;
                        }
                        return Ok(true);
                    }
                    DropStrategy::Recursive => {
                        for field_val in fields.iter().rev() {
                            self.run_drop_value(field_val, depth)?;
                        }
                        return Ok(true);
                    }
                    DropStrategy::Trivial(_) | DropStrategy::None => {}
                }
            }
            Value::Enum { type_name, fields, .. } => {
                let type_name = type_name.clone();
                let fields = fields.clone();
                let strategy = self.module.type_registry.get_type_def(&type_name)
                    .map(|td| td.metadata.drop_strategy.clone())
                    .unwrap_or(DropStrategy::None);
                match strategy {
                    DropStrategy::Custom(fn_name) => {
                        let addr = self.heap_alloc_ref_promoted(val.clone());
                        let ptr = Value::MutPtr(addr);
                        self.call_function(&fn_name, vec![ptr], depth + 1)?;
                        for field_val in fields.iter().rev() {
                            self.run_drop_value(field_val, depth)?;
                        }
                        return Ok(true);
                    }
                    DropStrategy::Recursive => {
                        for field_val in fields.iter().rev() {
                            self.run_drop_value(field_val, depth)?;
                        }
                        return Ok(true);
                    }
                    DropStrategy::Trivial(_) | DropStrategy::None => {}
                }
            }
            _ => {}
        }
        Ok(false)
    }

    /// On panic, drop all live droppable NAMED locals in reverse order (simulates __gorget_cleanup_run).
    /// Only named locals (with name_hint.is_some()) are eligible — compiler-generated temps are
    /// skipped. This mirrors the C backend's __gorget_cleanup_push which only registers named
    /// user variables, not intermediate temporaries.
    fn cleanup_locals_on_panic(
        &mut self,
        func: &crate::ir::Function,
        locals: &Vec<Value>,
        depth: usize,
    ) {
        use crate::ir::types::DropStrategy;
        // Iterate locals in reverse (inner scopes drop before outer scopes).
        // Start from index 1 (skip _0 = return slot).
        for i in (1..locals.len()).rev() {
            let val = &locals[i];
            // Skip zeroed-out (already dropped/moved) values
            if matches!(val, Value::Unit | Value::Null) {
                continue;
            }
            // Only drop named locals (name_hint.is_some()); skip compiler-generated temps.
            // The C backend's __gorget_cleanup_push only registers user-declared variables.
            let is_named = func.locals.get(i)
                .map(|l| l.name_hint.is_some())
                .unwrap_or(false);
            if !is_named { continue; }
            // Check if this local's type has Drop semantics
            let has_drop = if let Some(local_info) = func.locals.get(i) {
                let type_id = local_info.type_id;
                let type_name = self.module.type_registry.type_name(type_id)
                    .map(|s| s.to_string());
                if let Some(name) = type_name {
                    self.module.type_registry.get_type_def(&name)
                        .map(|td| !matches!(td.metadata.drop_strategy, DropStrategy::None | DropStrategy::Trivial(_)))
                        .unwrap_or(false)
                } else { false }
            } else { false };
            if has_drop {
                let _ = self.run_drop_value(val, depth);
            }
        }
    }

    /// Extract a TCP socket handle ID from the first argument (Socket struct or pointer to one).
    fn extract_socket_id(&self, args: &[Value]) -> usize {
        let val = match args.first() {
            Some(Value::Ptr(addr)) | Some(Value::MutPtr(addr)) => {
                self.heap_read(*addr).cloned().unwrap_or(Value::Unit)
            }
            Some(v) => v.clone(),
            None => Value::Unit,
        };
        match val {
            Value::Struct { ref fields, .. } => {
                fields.first().map(|f| f.as_i64() as usize).unwrap_or(0)
            }
            Value::I64(id) => id as usize,
            _ => 0,
        }
    }

    /// Extract a UDP socket handle ID from the first argument.
    fn extract_udp_socket_id(&self, args: &[Value]) -> usize {
        self.extract_socket_id(args) // same extraction logic
    }

    /// Check if a TypeId represents a type that requires drop/cleanup.
    /// Mirrors the C backend's `needs_drop_by_name` logic (mod.rs:7440-7455).
    fn is_type_id_droppable(&self, type_id: TypeId) -> bool {
        use crate::ir::types::DropStrategy;
        match self.module.type_registry.get(type_id) {
            Some(GirType::Named(name)) => {
                // Collections always require cleanup even with Trivial drop strategy —
                // they hold heap-allocated buffers that would be double-freed.
                if name.starts_with("Vector__") || name.starts_with("List__")
                    || name.starts_with("Dict__") || name.starts_with("HashMap__")
                    || name.starts_with("Set__") || name.starts_with("HashSet__")
                    || matches!(name.as_str(), "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString")
                {
                    return true;
                }
                self.module.type_registry.get_type_def(name.as_str())
                    .map(|td| !matches!(td.metadata.drop_strategy, DropStrategy::None))
                    .unwrap_or(false)
            }
            _ => false,
        }
    }

    /// Drop droppable fields of a struct value (used for Custom and Recursive drop strategies).
    #[allow(dead_code)]
    fn drop_struct_fields(&mut self, locals: &mut Vec<Value>, place: &Place, val: &Value, type_name: &str, depth: usize) -> SimResult<()> {
        use crate::ir::types::DropStrategy;
        let fields = match val {
            Value::Struct { fields, .. } => fields.clone(),
            _ => return Ok(()),
        };
        let type_def = match self.module.type_registry.get_type_def(type_name).cloned() {
            Some(td) => td,
            None => return Ok(()),
        };
        let struct_def = match &type_def.kind {
            crate::ir::types::TypeDefKind::Struct(s) => s.clone(),
            _ => return Ok(()),
        };
        // Drop fields in reverse order (LIFO).
        for (i, _field_def) in struct_def.fields.iter().enumerate().rev() {
            let field_val = match fields.get(i) {
                Some(v) => v.clone(),
                None => continue,
            };
            // Only drop fields that have a non-trivial drop strategy.
            let field_type_name = match &field_val {
                Value::Struct { type_name, .. } | Value::Enum { type_name, .. } => Some(type_name.clone()),
                _ => None,
            };
            let needs_drop = match &field_type_name {
                Some(tn) => self.module.type_registry.get_type_def(tn)
                    .map(|td| !matches!(td.metadata.drop_strategy, DropStrategy::None))
                    .unwrap_or(false),
                None => false,
            };
            if needs_drop {
                let field_place = Place {
                    local: place.local,
                    projections: {
                        let mut p = place.projections.clone();
                        p.push(Projection::Field(i as u32));
                        p
                    },
                };
                self.run_drop(locals, &field_place, &field_val, depth)?;
            }
        }
        Ok(())
    }

    /// Deref a Ptr/MutPtr to get the underlying value (for low-level gorget_* C functions
    /// that pass keys/values by pointer). Returns the value unchanged if not a pointer.
    fn deref_ptr(&self, val: Value) -> Value {
        match val {
            Value::Ptr(a) | Value::MutPtr(a) => self.heap.get(&a).cloned().unwrap_or(Value::Unit),
            other => other,
        }
    }

    /// For low-level gorget_* function names, deref the arg (pointer args → dereffed value).
    /// For high-level Set__T__*/Dict__K__V__* names, return the arg as-is.
    fn deref_if_low_level_call(&self, name: &str, val: Value) -> Value {
        if name.starts_with("gorget_") {
            self.deref_ptr(val)
        } else {
            val
        }
    }

    /// Returns true if `name` is a string method call that may receive a pointer to a string.
    fn is_string_method_call(&self, name: &str) -> bool {
        name.starts_with("GorgetString__")
            || name.starts_with("Str__")
            || name.starts_with("gorget_str_to_upper_to_str")
            || name.starts_with("gorget_str_to_lower_to_str")
            || name.starts_with("gorget_str_char_at")
            // CoW materialization helpers — all take a `const GorgetString*`
            // (Value::Ptr in sim); the runtime handler `try_to_sim_str`
            // doesn't read through Ptr, so without this auto-deref the helper
            // would see an empty string and propagate "" through the rest of
            // the call chain. Snag #6 cascade (2026-05-05): the assertion
            // failure on `tail.index_of("---*/")` reduced to a String=""
            // input at index_of because the upstream `clone_to_owned` saw
            // Ptr(addr) and produced "" instead of dereffing.
            || matches!(name,
                "gorget_string_clone_to_owned"
                | "gorget_string_copy_cow"
                | "gorget_string_borrow"
                | "gorget_string_materialize_inplace"
                | "gorget_string_clone_inplace")
            || name.starts_with("__option_")
            || name.starts_with("__result_")
    }

    /// Handle mutating GorgetString operations that modify through a MutPtr.
    /// Returns Some(result) if handled (writes modified string back to heap), None otherwise.
    fn try_string_mutation(&mut self, name: &str, args: &[Value]) -> SimResult<Option<Value>> {
        // Extract heap address from first arg (must be a MutPtr).
        let addr = match args.first() {
            Some(Value::MutPtr(a)) | Some(Value::Ptr(a)) => *a,
            _ => return Ok(None),
        };

        /// Convert any Value to its string representation for string builder push.
        /// Mirrors what the C gorget_string_push_* family does.
        fn value_to_push_str(v: &Value) -> std::string::String {
            match v {
                Value::Str(s) => s.as_str().to_string(),
                Value::String(s) => s.as_str().to_string(),
                Value::CStr(s) => (**s).clone(),
                Value::I64(n) => n.to_string(),
                Value::I32(n) => n.to_string(),
                Value::I16(n) => n.to_string(),
                Value::I8(n) => n.to_string(),
                Value::U64(n) => n.to_string(),
                Value::U32(n) => n.to_string(),
                Value::U16(n) => n.to_string(),
                Value::U8(n) => n.to_string(),
                Value::F64(f) => {
                    // Mirror C's "%g" — use shortest representation
                    if f.fract() == 0.0 && f.abs() < 1e15 {
                        format!("{f}")
                    } else {
                        format!("{f:?}")
                    }
                }
                Value::F32(f) => {
                    let f = *f as f64;
                    if f.fract() == 0.0 && f.abs() < 1e15 {
                        format!("{f}")
                    } else {
                        format!("{f:?}")
                    }
                }
                Value::Bool(b) => if *b { "true".to_string() } else { "false".to_string() },
                Value::Char(c) => char::from_u32(*c).map(|c| c.to_string()).unwrap_or_default(),
                _ => std::string::String::new(),
            }
        }

        match name {
            "gorget_string_append_str" | "gorget_string_push_str"
            | "GorgetString__push_str" | "GorgetString__append" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current {
                    Value::String(s) => s,
                    _ => return Ok(None),
                };
                let to_append = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
                s.data.extend_from_slice(to_append.as_bytes());
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            // GorgetString__push is a generic overload — dispatch by value type.
            "GorgetString__push" | "gorget_string_push" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current {
                    Value::String(s) => s,
                    _ => return Ok(None),
                };
                if let Some(v) = args.get(1) {
                    let text = value_to_push_str(v);
                    s.data.extend_from_slice(text.as_bytes());
                }
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_push_int" | "GorgetString__push_int" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current { Value::String(s) => s, _ => return Ok(None) };
                let n = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
                s.data.extend_from_slice(n.to_string().as_bytes());
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_push_float" | "GorgetString__push_float" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current { Value::String(s) => s, _ => return Ok(None) };
                let v = args.get(1).cloned().unwrap_or(Value::Unit);
                s.data.extend_from_slice(value_to_push_str(&v).as_bytes());
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_push_bool" | "GorgetString__push_bool" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current { Value::String(s) => s, _ => return Ok(None) };
                let b = args.get(1).map(|v| v.as_bool()).unwrap_or(false);
                s.data.extend_from_slice(if b { b"true" } else { b"false" });
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_push_char" | "GorgetString__push_char" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current {
                    Value::String(s) => s,
                    _ => return Ok(None),
                };
                let cp = match args.get(1) {
                    Some(Value::Char(c)) => *c,
                    Some(v) => v.as_i64() as u32,
                    None => 0,
                };
                if let Some(ch) = char::from_u32(cp) {
                    let mut buf = [0u8; 4];
                    let encoded = ch.encode_utf8(&mut buf);
                    s.data.extend_from_slice(encoded.as_bytes());
                }
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            // push_line variants: push value then newline
            "GorgetString__push_line" | "gorget_string_push_line_str" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current { Value::String(s) => s, _ => return Ok(None) };
                if let Some(v) = args.get(1) {
                    let text = value_to_push_str(v);
                    s.data.extend_from_slice(text.as_bytes());
                }
                s.data.push(b'\n');
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_push_line" | "gorget_string_push_line_int"
            | "gorget_string_push_line_float" | "gorget_string_push_line_bool"
            | "gorget_string_push_line_char" => {
                let current = self.heap_read(addr)?.clone();
                let mut s = match current { Value::String(s) => s, _ => return Ok(None) };
                if let Some(v) = args.get(1) {
                    let text = value_to_push_str(v);
                    s.data.extend_from_slice(text.as_bytes());
                }
                s.data.push(b'\n');
                self.heap.insert(addr, Value::String(s));
                Ok(Some(Value::Unit))
            }
            "gorget_string_clear" | "GorgetString__clear" => {
                let current = self.heap_read(addr)?.clone();
                if let Value::String(mut s) = current {
                    s.data.clear(); // preserve cap
                    self.heap.insert(addr, Value::String(s));
                    Ok(Some(Value::Unit))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    /// Try to dispatch collection method calls (gorget_array_*, gorget_dict_*, etc.)
    /// Returns Some(result) if handled, None if should fall through to runtime dispatch.
    fn try_collection_dispatch(&mut self, name: &str, args: &[Value], depth: usize) -> SimResult<Option<Value>> {
        // Box[T] operations — Box is a heap-allocated pointer (T*) in C, Ptr(addr) here.
        if name.starts_with("__gorget_box_alloc_") {
            // Box.new(val) → alloc heap slot for val, return Ptr(addr)
            let val = args.first().cloned().unwrap_or(Value::Unit);
            let addr = self.heap_alloc(val);
            return Ok(Some(Value::Ptr(addr)));
        }
        if name.starts_with("Box__") && name.ends_with("__get") {
            // Box.get(b) → deref the pointer
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let val = match ptr {
                Value::Ptr(addr) | Value::MutPtr(addr) => {
                    self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                }
                _ => Value::Unit,
            };
            return Ok(Some(val));
        }
        if name.starts_with("Box__") && name.ends_with("__set") {
            // Box.set(&b, val) → write to heap. b_ptr is Ptr to Ptr.
            let b_ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let new_val = args.get(1).cloned().unwrap_or(Value::Unit);
            if let Value::Ptr(outer_addr) | Value::MutPtr(outer_addr) = b_ptr {
                // Read the inner Ptr from heap[outer_addr]
                let inner_ptr = self.heap_read(outer_addr).cloned().unwrap_or(Value::Null);
                if let Value::Ptr(inner_addr) | Value::MutPtr(inner_addr) = inner_ptr {
                    self.heap.insert(inner_addr, new_val);
                }
            }
            return Ok(Some(Value::Unit));
        }
        // Box[Trait] method dispatch — Box__TRAIT__METHOD(ptr_to_box)
        // This handles dynamic dispatch through vtables. After __get/__set/__new are handled above,
        // any remaining Box__* call is a trait method dispatch via the vtable.
        if name.starts_with("Box__") {
            let rest = &name["Box__".len()..];
            // Split "Greeter__greet" → trait_name="Greeter", method_name="greet"
            if let Some(sep) = rest.rfind("__") {
                let trait_name = &rest[..sep];
                let method_name = &rest[sep + 2..];
                // Verify this is a real trait (not Box__int or Box__str)
                if self.module.type_registry.get_type_def(&format!("{trait_name}_VTable")).is_some() {
                    // arg[0] is Ptr(slot) where heap[slot] = Ptr(concrete_addr)
                    // where heap[concrete_addr] = Struct { type_name: ConcreteType, .. }
                    let box_arg = args.first().cloned().unwrap_or(Value::Null);
                    // First deref: slot → inner box pointer
                    let inner = match box_arg {
                        Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => {
                            self.heap.get(&a).cloned().unwrap_or(Value::Unit)
                        }
                        other => other,
                    };
                    // Second deref: inner box pointer → concrete struct
                    let (concrete_type_name, concrete_ptr) = match inner {
                        Value::Ptr(a) | Value::MutPtr(a) => {
                            let concrete = self.heap.get(&a).cloned().unwrap_or(Value::Unit);
                            match concrete {
                                Value::Struct { ref type_name, .. } => (type_name.clone(), Value::MutPtr(a)),
                                _ => {
                                    // If heap slot itself holds a struct, treat addr as concrete
                                    let t = inner.type_name().to_string();
                                    (t, Value::MutPtr(a))
                                }
                            }
                        }
                        Value::Struct { ref type_name, .. } => {
                            // Direct struct value — allocate a temp heap slot
                            let t = type_name.clone();
                            let addr = self.heap_alloc(inner.clone());
                            (t, Value::MutPtr(addr))
                        }
                        _ => ("".to_string(), Value::Null),
                    };
                    if !concrete_type_name.is_empty() {
                        let vtable_fn = format!("{trait_name}_for_{concrete_type_name}__{method_name}");
                        let mut call_args = vec![concrete_ptr];
                        call_args.extend_from_slice(&args[1..]);
                        let result = self.call_function(&vtable_fn, call_args, depth + 1)?;
                        return Ok(Some(result));
                    }
                }
            }
        }

        // ────────── Shared[T] operations (reference-counted pointer in C, Ptr in sim) ──────────
        // Shared__T__new(val) → alloc val on heap, initialize refcounts (strong=1, weak=1)
        if name.starts_with("Shared__") && name.ends_with("__new") {
            let val = args.first().cloned().unwrap_or(Value::Unit);
            let addr = self.heap_alloc(val);
            self.shared_refcounts.insert(addr, (1, 1));
            return Ok(Some(Value::Ptr(addr)));
        }
        // Shared__T__get(shared_ptr) → deref to get contained value
        if name.starts_with("Shared__") && name.ends_with("__get") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let val = match ptr {
                Value::Ptr(addr) | Value::MutPtr(addr) => {
                    self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                }
                other => other,
            };
            return Ok(Some(val));
        }
        // Shared__T__clone(shared_ptr) → increment strong count, return same pointer
        if name.starts_with("Shared__") && name.ends_with("__clone") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            if let Value::Ptr(addr) | Value::MutPtr(addr) = &ptr {
                if let Some(rc) = self.shared_refcounts.get_mut(addr) {
                    rc.0 += 1;
                }
            }
            return Ok(Some(ptr));
        }
        // Shared__T__strong_count(shared_ptr) → current strong ref count
        if name.starts_with("Shared__") && name.ends_with("__strong_count") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let count = if let Value::Ptr(addr) | Value::MutPtr(addr) = ptr {
                self.shared_refcounts.get(&addr).map(|rc| rc.0).unwrap_or(0)
            } else { 0 };
            return Ok(Some(Value::I64(count)));
        }
        // Shared__T__downgrade(shared_ptr) → increment weak count, return Ref (Weak handle)
        // Weak handles use Value::Ref to distinguish from Shared (Value::Ptr) in run_drop_value.
        if name.starts_with("Shared__") && name.ends_with("__downgrade") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            if let Value::Ptr(addr) | Value::MutPtr(addr) = &ptr {
                let data_addr = *addr;
                if let Some(rc) = self.shared_refcounts.get_mut(&data_addr) {
                    rc.1 += 1;
                }
                return Ok(Some(Value::Ref(data_addr)));
            }
            return Ok(Some(Value::Null));
        }
        // Shared__Vector__T__at(shared_ptr, i) → element at index i of inner Vector
        if name.starts_with("Shared__Vector__") && name.ends_with("__at") {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let idx = args.get(1).cloned().unwrap_or(Value::I64(0)).as_i64();
            if let Some(arr) = self.get_array_from_value(&ptr) {
                if idx >= 0 && (idx as usize) < arr.len() {
                    return Ok(Some(arr.get(idx as usize).unwrap_or(Value::Unit)));
                } else {
                    return Err(super::SimError::Panic(format!(
                        "gorget: panic: shared array index out of bounds: {} >= {}", idx, arr.len()
                    )));
                }
            }
            return Ok(Some(Value::Unit));
        }
        // Shared__Vector__T__set_at(shared_ptr, i, val) → write element at index i
        if name.starts_with("Shared__Vector__") && name.ends_with("__set_at") {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let idx = args.get(1).cloned().unwrap_or(Value::I64(0)).as_i64();
            let val = args.get(2).cloned().unwrap_or(Value::Unit);
            if let Some(arr) = self.get_array_from_value(&ptr) {
                if idx >= 0 && (idx as usize) < arr.len() {
                    arr.set(idx as usize, val);
                } else {
                    return Err(super::SimError::Panic(format!(
                        "gorget: panic: shared array index out of bounds: {} >= {}", idx, arr.len()
                    )));
                }
            }
            return Ok(Some(Value::Unit));
        }
        // Shared__Vector__T__slen(shared_ptr) → length of inner Vector
        if name.starts_with("Shared__Vector__") && name.ends_with("__slen") {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let len = self.get_array_from_value(&ptr).map(|a| a.len()).unwrap_or(0);
            return Ok(Some(Value::I64(len as i64)));
        }
        // Shared__T__set(shared_ptr, val) → write to heap
        if name.starts_with("Shared__") && name.ends_with("__set") {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let val = args.get(1).cloned().unwrap_or(Value::Unit);
            if let Value::Ptr(addr) | Value::MutPtr(addr) = ptr {
                self.heap.insert(addr, val);
            }
            return Ok(Some(Value::Unit));
        }
        // Shared__T__drop(shared_ptr*) → decrement strong; free data at 0, free ctrl block when weak also 0
        if name.starts_with("Shared__") && name.ends_with("__drop") {
            let ptr_to_ptr = args.first().cloned().unwrap_or(Value::Null);
            let inner_addr = match &ptr_to_ptr {
                Value::MutPtr(addr) | Value::Ptr(addr) => {
                    // The drop function takes a pointer-to-pointer (T**); dereference to get the handle
                    match self.heap.get(addr).cloned() {
                        Some(Value::Ptr(inner)) | Some(Value::MutPtr(inner)) => Some(inner),
                        _ => None,
                    }
                }
                _ => None,
            };
            if let Some(addr) = inner_addr {
                let rc = self.shared_refcounts.get_mut(&addr);
                if let Some(rc) = rc {
                    rc.0 -= 1;
                    if rc.0 <= 0 {
                        // Free inner data
                        self.heap.remove(&addr);
                        rc.1 -= 1; // release collective weak ref
                        if rc.1 <= 0 {
                            self.shared_refcounts.remove(&addr);
                        }
                    }
                }
            }
            return Ok(Some(Value::Unit));
        }
        // Shared__T__free (legacy alias) → no-op
        if name.starts_with("Shared__") && name.ends_with("__free") {
            return Ok(Some(Value::Unit));
        }
        // ────────── Weak[T] operations ──────────
        // Weak__T__clone(weak_ptr: Ref) → increment weak count, return Ref
        if name.starts_with("Weak__") && name.ends_with("__clone") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            if let Value::Ref(addr) | Value::Ptr(addr) | Value::MutPtr(addr) = &ptr {
                let addr = *addr;
                if let Some(rc) = self.shared_refcounts.get_mut(&addr) {
                    rc.1 += 1;
                }
                return Ok(Some(Value::Ref(addr)));
            }
            return Ok(Some(Value::Null));
        }
        // Weak__T__drop → handled by run_drop_value (Ref arm decrements weak).
        // If called explicitly as a function, also handle here as a fallback.
        if name.starts_with("Weak__") && name.ends_with("__drop") {
            return Ok(Some(Value::Unit)); // No-op: RAII handled by run_drop_value
        }
        // Weak__T__upgrade(weak_ptr: Ref) → Option[Shared[T]]: Some(Ptr) if strong>0, else None
        if name.starts_with("Weak__") && name.ends_with("__upgrade") {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let addr = match &ptr {
                Value::Ref(a) | Value::Ptr(a) | Value::MutPtr(a) => *a,
                _ => 0,
            };
            let strong = if addr != 0 {
                self.shared_refcounts.get(&addr).map(|rc| rc.0).unwrap_or(0)
            } else { 0 };
            if strong > 0 {
                // Upgrade: increment strong count, return Some(Ptr — a new Shared handle)
                if let Some(rc) = self.shared_refcounts.get_mut(&addr) {
                    rc.0 += 1;
                }
                return Ok(Some(Value::Enum {
                    type_name: "Option".to_string(),
                    tag: 0,
                    variant: "Some".to_string(),
                    fields: vec![Value::Ptr(addr)],
                }));
            } else {
                return Ok(Some(Value::Enum {
                    type_name: "Option".to_string(),
                    tag: 1,
                    variant: "None".to_string(),
                    fields: vec![],
                }));
            }
        }

        // ────────── RWLock[T] + ReadGuard[T] + WriteGuard[T] operations ──────────
        // Single-threaded sim: RWLock behaves like Mutex (no real locking).
        if name.starts_with("RWLock__") && name.ends_with("__new") {
            let val = args.first().cloned().unwrap_or(Value::Unit);
            let addr = self.heap_alloc(val);
            return Ok(Some(Value::MutPtr(addr)));
        }
        if name.starts_with("RWLock__") && name.ends_with("__read") {
            // ReadGuard is just the same MutPtr to the inner value (no actual lock).
            let rwlock = args.first().cloned().unwrap_or(Value::Null);
            return Ok(Some(rwlock));
        }
        if name.starts_with("RWLock__") && name.ends_with("__write") {
            // WriteGuard is just the same MutPtr to the inner value (no actual lock).
            let rwlock = args.first().cloned().unwrap_or(Value::Null);
            return Ok(Some(rwlock));
        }
        if name.starts_with("RWLock__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }
        // ReadGuard[T] / WriteGuard[T] methods — mirrors Guard[T] behaviour.
        if name.starts_with("ReadGuard__") && name.ends_with("__get") {
            let guard_arg = args.first().cloned().unwrap_or(Value::Null);
            let guard_val = match guard_arg {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            let inner = match guard_val {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            return Ok(Some(inner));
        }
        if name.starts_with("WriteGuard__") && name.ends_with("__get") {
            let guard_arg = args.first().cloned().unwrap_or(Value::Null);
            let guard_val = match guard_arg {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            let inner = match guard_val {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            return Ok(Some(inner));
        }
        if name.starts_with("WriteGuard__") && name.ends_with("__set") {
            let guard_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let new_val = args.get(1).cloned().unwrap_or(Value::Unit);
            let guard_val = match guard_arg {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            match guard_val {
                Value::Ptr(a) | Value::MutPtr(a) => { self.heap_write(a, new_val); }
                _ => {}
            }
            return Ok(Some(Value::Unit));
        }
        if (name.starts_with("ReadGuard__") || name.starts_with("WriteGuard__"))
            && (name.ends_with("__drop") || name.ends_with("__free"))
        {
            return Ok(Some(Value::Unit));
        }

        // ────────── Thread[T] methods (join, id, drop) ──────────
        // Thread handles are MutPtr → __ThreadHandle struct { result, tid } allocated during spawn.
        if name.starts_with("Thread__") && name.ends_with("__join") {
            let handle = args.first().cloned().unwrap_or(Value::Null);
            let addr = match handle {
                Value::Ptr(a) | Value::MutPtr(a) => a,
                Value::I64(a) => a as usize,
                _ => return Ok(Some(Value::Unit)),
            };
            if let Some(Value::Struct { fields, .. }) = self.heap.get(&addr).cloned() {
                // fields[0] = return value, fields[1] = thread ID
                return Ok(Some(fields.into_iter().next().unwrap_or(Value::Unit)));
            }
            // Fallback: plain heap slot (void threads)
            let val = self.heap_read(addr).cloned().unwrap_or(Value::Unit);
            return Ok(Some(val));
        }
        if name.starts_with("Thread__") && name.ends_with("__id") {
            let handle = args.first().cloned().unwrap_or(Value::Null);
            let addr = match handle {
                Value::Ptr(a) | Value::MutPtr(a) => a,
                Value::I64(a) => a as usize,
                _ => return Ok(Some(Value::I64(0))),
            };
            if let Some(Value::Struct { fields, .. }) = self.heap.get(&addr) {
                // fields[1] = thread ID
                let tid = fields.get(1).cloned().unwrap_or(Value::I64(addr as i64));
                return Ok(Some(tid));
            }
            return Ok(Some(Value::I64(addr as i64)));
        }
        if name.starts_with("Thread__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── AtomicInt + AtomicBool operations ──────────
        // Single-threaded sim: atomics are plain heap-allocated integers/bools.
        if name == "gorget_atomic_int_new" {
            let val = args.first().cloned().unwrap_or(Value::I64(0));
            let addr = self.heap_alloc(val);
            return Ok(Some(Value::MutPtr(addr)));
        }
        if name == "AtomicInt__load" {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::I64(0))) };
            return Ok(Some(self.heap_read(addr).cloned().unwrap_or(Value::I64(0))));
        }
        if name == "AtomicInt__store" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let val = args.get(1).cloned().unwrap_or(Value::I64(0));
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Unit)) };
            self.heap_write(addr, val);
            return Ok(Some(Value::Unit));
        }
        if name == "AtomicInt__add" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let delta = args.get(1).cloned().unwrap_or(Value::I64(0)).as_i64();
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::I64(0))) };
            let old = self.heap_read(addr).cloned().unwrap_or(Value::I64(0));
            let new_val = Value::I64(old.as_i64().wrapping_add(delta));
            self.heap_write(addr, new_val);
            return Ok(Some(old));
        }
        if name == "AtomicInt__sub" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let delta = args.get(1).cloned().unwrap_or(Value::I64(0)).as_i64();
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::I64(0))) };
            let old = self.heap_read(addr).cloned().unwrap_or(Value::I64(0));
            let new_val = Value::I64(old.as_i64().wrapping_sub(delta));
            self.heap_write(addr, new_val);
            return Ok(Some(old));
        }
        if name == "AtomicInt__compare_exchange" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let expected = args.get(1).cloned().unwrap_or(Value::I64(0)).as_i64();
            let desired  = args.get(2).cloned().unwrap_or(Value::I64(0));
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Bool(false))) };
            let current = self.heap_read(addr).cloned().unwrap_or(Value::I64(0)).as_i64();
            if current == expected {
                self.heap_write(addr, desired);
                return Ok(Some(Value::Bool(true)));
            }
            return Ok(Some(Value::Bool(false)));
        }
        if name == "gorget_atomic_bool_new" {
            let val = args.first().cloned().unwrap_or(Value::Bool(false));
            let addr = self.heap_alloc(val);
            return Ok(Some(Value::MutPtr(addr)));
        }
        if name == "AtomicBool__load" {
            let ptr = args.first().cloned().unwrap_or(Value::Null);
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Bool(false))) };
            return Ok(Some(self.heap_read(addr).cloned().unwrap_or(Value::Bool(false))));
        }
        if name == "AtomicBool__store" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let val = args.get(1).cloned().unwrap_or(Value::Bool(false));
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Unit)) };
            self.heap_write(addr, val);
            return Ok(Some(Value::Unit));
        }
        if name == "AtomicBool__swap" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let new_val = args.get(1).cloned().unwrap_or(Value::Bool(false));
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Bool(false))) };
            let old = self.heap_read(addr).cloned().unwrap_or(Value::Bool(false));
            self.heap_write(addr, new_val);
            return Ok(Some(old));
        }
        if name == "AtomicBool__compare_exchange" {
            let ptr = args.get(0).cloned().unwrap_or(Value::Null);
            let expected = args.get(1).map(|v| v.as_bool()).unwrap_or(false);
            let desired  = args.get(2).cloned().unwrap_or(Value::Bool(false));
            let addr = match ptr { Value::Ptr(a) | Value::MutPtr(a) => a, Value::I64(a) => a as usize, _ => return Ok(Some(Value::Bool(false))) };
            let current = self.heap_read(addr).cloned().unwrap_or(Value::Bool(false)).as_bool();
            if current == expected {
                self.heap_write(addr, desired);
                return Ok(Some(Value::Bool(true)));
            }
            return Ok(Some(Value::Bool(false)));
        }
        if name.starts_with("AtomicInt__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }
        if name.starts_with("AtomicBool__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── Thread ID free functions ──────────
        if name == "gorget_current_thread_id" {
            return Ok(Some(Value::I64(1))); // main thread; non-zero like pthread_self()
        }

        // ────────── Barrier operations ──────────
        // Single-threaded sim: Barrier is a no-op (wait() always succeeds immediately).
        if name == "gorget_barrier_new" {
            let addr = self.heap_alloc(Value::I64(1));
            return Ok(Some(Value::MutPtr(addr)));
        }
        if name == "Barrier__wait" {
            return Ok(Some(Value::Unit));
        }
        if name.starts_with("Barrier__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── CondVar operations ──────────
        // Single-threaded sim: CondVar is a no-op (notify/wait are identity operations).
        // NOTE: programs that depend on cross-thread signaling via CondVar will not work
        // correctly in the sim (consumer loops would spin forever). This is an inherent
        // limitation of single-threaded simulation.
        if name == "gorget_condvar_new" {
            let addr = self.heap_alloc(Value::I64(0));
            return Ok(Some(Value::MutPtr(addr)));
        }
        if name == "CondVar__notify_one" || name == "CondVar__notify_all" {
            return Ok(Some(Value::Unit));
        }
        if name == "CondVar__wait" {
            // No-op in single-threaded sim. Programs relying on this for inter-thread
            // signaling will fail, but simple single-thread patterns work fine.
            return Ok(Some(Value::Unit));
        }
        if name.starts_with("CondVar__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── Mutex[T] + Guard[T] operations ──────────
        // Mutex__T__new(val) → alloc val on heap, return Ptr (the mutex IS the heap addr)
        if name.starts_with("Mutex__") && name.ends_with("__new") {
            let val = args.first().cloned().unwrap_or(Value::Unit);
            let addr = self.heap_alloc(val);
            return Ok(Some(Value::MutPtr(addr)));
        }
        // Mutex__T__lock(mutex_ptr) → Guard (also a MutPtr pointing to the same value)
        if name.starts_with("Mutex__") && name.ends_with("__lock") {
            // Deref mutex ptr to get value addr, return MutPtr to it
            let mutex = args.first().cloned().unwrap_or(Value::Null);
            return Ok(Some(mutex)); // Guard is just the same MutPtr
        }
        // Guard__T__get(guard_ptr) → deref to value. guard_ptr is Ptr/MutPtr to guard,
        // which is itself a MutPtr to the value.
        if name.starts_with("Guard__") && name.ends_with("__get") {
            let guard_arg = args.first().cloned().unwrap_or(Value::Null);
            // guard_arg may be Ptr/MutPtr to guard (itself a MutPtr), or direct MutPtr
            let guard_val = match guard_arg {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            let inner = match guard_val {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            return Ok(Some(inner));
        }
        // Guard__T__set(guard_ptr, val) → write value through guard
        if name.starts_with("Guard__") && name.ends_with("__set") {
            let guard_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let new_val = args.get(1).cloned().unwrap_or(Value::Unit);
            let guard_val = match guard_arg {
                Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                other => other,
            };
            match guard_val {
                Value::Ptr(a) | Value::MutPtr(a) => { self.heap_write(a, new_val); }
                _ => {}
            }
            return Ok(Some(Value::Unit));
        }
        // Guard__T__drop → no-op (no real lock to release in sim)
        if name.starts_with("Guard__") && (name.ends_with("__drop") || name.ends_with("__free")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── Channel[T] operations (sync FIFO queue in sim) ──────────
        // Channel__T__new(capacity) / gorget_channel_new → alloc a SimArray on heap, return MutPtr
        if (name.starts_with("Channel__") && name.ends_with("__new")) || name == "gorget_channel_new" {
            let arr = super::value::SimArray::new("Channel");
            let addr = self.heap_alloc(Value::Array(arr));
            return Ok(Some(Value::MutPtr(addr)));
        }
        // Channel__T__send(ch_ptr, val) / gorget_channel_send → push val to queue
        if (name.starts_with("Channel__") && name.ends_with("__send")) || name == "gorget_channel_send" {
            let ch_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let val = args.get(1).cloned().unwrap_or(Value::Unit);
            // ch_arg is MutPtr to a heap slot holding the channel SimArray
            let ch_addr = match ch_arg {
                Value::Ptr(a) | Value::MutPtr(a) => {
                    // The ptr might point to another ptr (ref to local holding MutPtr)
                    let v = self.heap.get(&a).cloned().unwrap_or(Value::Unit);
                    match v {
                        Value::Ptr(b) | Value::MutPtr(b) => b,
                        _ => a,
                    }
                }
                _ => return Ok(Some(Value::Unit)),
            };
            if let Some(Value::Array(arr)) = self.heap.get(&ch_addr).cloned() {
                arr.push(val);
                self.heap.insert(ch_addr, Value::Array(arr));
            }
            return Ok(Some(Value::Unit));
        }
        // Channel__T__recv(ch_ptr) → pop val from front of queue
        if (name.starts_with("Channel__") && name.ends_with("__recv")) || name == "gorget_channel_recv" {
            let ch_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let ch_addr = match ch_arg {
                Value::Ptr(a) | Value::MutPtr(a) => {
                    let v = self.heap.get(&a).cloned().unwrap_or(Value::Unit);
                    match v {
                        Value::Ptr(b) | Value::MutPtr(b) => b,
                        _ => a,
                    }
                }
                _ => return Ok(Some(Value::I64(0))),
            };
            if let Some(Value::Array(arr)) = self.heap.get(&ch_addr).cloned() {
                let front = {
                    let data = arr.data.borrow();
                    data.first().cloned()
                };
                if let Some(val) = front {
                    // Remove the first element
                    arr.data.borrow_mut().remove(0);
                    self.heap.insert(ch_addr, Value::Array(arr));
                    return Ok(Some(val));
                }
            }
            return Ok(Some(Value::I64(0))); // empty channel → return 0 (block in real impl)
        }
        // Channel__T__poll_recv(ch_ptr, out_ptr, timeout) → bool
        // Non-blocking try: if channel has data, pop and store in out_ptr, return true.
        // Used by `select:` statement spin-wait loop generated by lower_select.
        if name.starts_with("Channel__") && name.ends_with("__poll_recv") {
            let ch_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let out_arg = args.get(1).cloned().unwrap_or(Value::Null);
            let ch_addr = match ch_arg {
                Value::Ptr(a) | Value::MutPtr(a) => {
                    let v = self.heap.get(&a).cloned().unwrap_or(Value::Unit);
                    match v {
                        Value::Ptr(b) | Value::MutPtr(b) => b,
                        _ => a,
                    }
                }
                _ => return Ok(Some(Value::Bool(false))),
            };
            let front = if let Some(Value::Array(arr)) = self.heap.get(&ch_addr).cloned() {
                let front = { let data = arr.data.borrow(); data.first().cloned() };
                if let Some(val) = front {
                    arr.data.borrow_mut().remove(0);
                    self.heap.insert(ch_addr, Value::Array(arr));
                    Some(val)
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(val) = front {
                // Write value into out_ptr
                match out_arg {
                    Value::Ptr(a) | Value::MutPtr(a) => {
                        let inner = self.heap.get(&a).cloned().unwrap_or(Value::Unit);
                        match inner {
                            Value::Ptr(b) | Value::MutPtr(b) => { self.heap.insert(b, val); }
                            _ => { self.heap.insert(a, val); }
                        }
                    }
                    _ => {}
                }
                return Ok(Some(Value::Bool(true)));
            } else {
                return Ok(Some(Value::Bool(false)));
            }
        }
        // Channel__T__close → no-op
        if name.starts_with("Channel__") && name.ends_with("__close") {
            return Ok(Some(Value::Unit));
        }
        // Channel__T__free/drop → no-op
        if name.starts_with("Channel__") && (name.ends_with("__free") || name.ends_with("__drop")) {
            return Ok(Some(Value::Unit));
        }

        // ────────── ARRAY / VECTOR operations ──────────
        // Helper: true if this is an array/vector collection call
        let is_array_call = |n: &str| {
            n.starts_with("Vector__") || n.starts_with("List__") || n.starts_with("GorgetArray__")
        };

        // gorget_array_new(elem_size) → empty array
        if name == "gorget_array_new"
            || (is_array_call(name) && name.ends_with("__new"))
            || name == "GorgetArray__new"
        {
            let type_name = if name == "gorget_array_new" { "GorgetArray" } else { name.trim_end_matches("__new") };
            let arr = super::value::SimArray::new(type_name);
            // Capture the active tracking allocator (mirrors GorgetArray.alloc = __gorget_current_alloc)
            if let Some(&tracking_id) = self.active_tracking.last() {
                arr.alloc_id.set(Some(tracking_id));
            }
            return Ok(Some(Value::Array(arr)));
        }

        // gorget_array_push(MutPtr(arr), elem_or_ptr) / Vector__T__push(MutPtr(arr), elem)
        if name == "gorget_array_push"
            || (is_array_call(name) && name.ends_with("__push"))
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            // The element: for gorget_array_push it's Ptr(elem_addr), for Vector__T__push it's the value directly
            let elem = if let Some(e) = args.get(1) {
                match e {
                    Value::Ptr(addr) | Value::MutPtr(addr) => {
                        self.heap_read(*addr).cloned().unwrap_or(e.clone())
                    }
                    _ => e.clone(),
                }
            } else { Value::Unit };
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                let old_len = arr.len();
                // Determine which tracking allocator to charge (array's stored alloc or active stack).
                let charge_id = arr.alloc_id.get()
                    .or_else(|| self.active_tracking.last().copied());
                arr.push(elem);
                // Track allocation: 8 bytes per element (approximation for the tracking allocator).
                // Use realloc semantics: first push triggers initial alloc (cap=8), subsequent pushes
                // may trigger doubling realloc when len crosses a power-of-two boundary.
                if let Some(id) = charge_id {
                    if let Some(state) = self.tracking_allocs.get_mut(&id) {
                        if old_len == 0 {
                            // Initial allocation: cap=8 * 8 bytes = 64 bytes (matches C: cap 0→8)
                            state.record_alloc(64);
                        } else if old_len.count_ones() == 1 {
                            // At power-of-two len: cap doubled from old_len to old_len*2
                            state.record_realloc((old_len * 8) as i64, (old_len * 16) as i64);
                        }
                    }
                }
            }
            return Ok(Some(Value::Unit));
        }

        // gorget_array_len / Vector__T__len(Ptr/MutPtr(arr))
        if name == "gorget_array_len"
            || (is_array_call(name) && name.ends_with("__len"))
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let len = self.get_array_from_value(&arr_arg).map(|a| a.len()).unwrap_or(0);
            return Ok(Some(Value::I64(len as i64)));
        }

        // Vector__T__get(Ptr/MutPtr(arr), I64(idx)) → Option[T]
        // gorget_array_get(Ptr/MutPtr(arr), I64(idx)) → raw Ptr or value (we return value)
        if name == "gorget_array_get"
            || (is_array_call(name) && name.ends_with("__get"))
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let idx_val = args.get(1).cloned().unwrap_or(Value::I64(0));
            let idx = idx_val.as_i64();
            let arr_opt = self.get_array_from_value(&arr_arg);
            let result = match arr_opt {
                Some(arr) if idx >= 0 && (idx as usize) < arr.len() => {
                    let elem = arr.get(idx as usize).unwrap_or(Value::Unit);
                    if name == "gorget_array_get" {
                        // Raw get — allocate heap slot and return ptr
                        let addr = self.heap_alloc(elem);
                        Value::Ptr(addr)
                    } else {
                        // Typed Vector get — return Option[T]
                        let type_suffix = name.strip_prefix("Vector__").or_else(|| name.strip_prefix("List__"))
                            .unwrap_or("int64_t").strip_suffix("__get").unwrap_or("int64_t");
                        Value::Enum {
                            type_name: format!("Option__{type_suffix}"),
                            tag: 0,
                            variant: "Some".to_string(),
                            fields: vec![elem],
                        }
                    }
                }
                _ => {
                    if name == "gorget_array_get" {
                        Value::Null
                    } else {
                        let type_suffix = name.strip_prefix("Vector__").or_else(|| name.strip_prefix("List__"))
                            .unwrap_or("int64_t").strip_suffix("__get").unwrap_or("int64_t");
                        Value::Enum {
                            type_name: format!("Option__{type_suffix}"),
                            tag: 1,
                            variant: "None".to_string(),
                            fields: vec![],
                        }
                    }
                }
            };
            return Ok(Some(result));
        }

        // Vector__T__set(MutPtr(arr), idx, val) — set element, pre-dropping old element.
        if name == "gorget_array_set"
            || (is_array_call(name) && name.ends_with("__set"))
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let val = if let Some(v) = args.get(2) {
                match v {
                    Value::Ptr(addr) | Value::MutPtr(addr) => {
                        self.heap_read(*addr).cloned().unwrap_or(v.clone())
                    }
                    _ => v.clone(),
                }
            } else { Value::Unit };
            // Pre-drop the old element before overwriting (mirrors C backend set() behavior).
            let old_val = if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.get(idx)
            } else { None };
            if let Some(old) = old_val {
                let _ = self.run_drop_value(&old, depth);
            }
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.set(idx, val);
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__pop(MutPtr(arr)) → raw element (GIR returns I64_TYPE, not Option)
        // The C backend emits inline C that pops directly without Option wrapping.
        if (is_array_call(name) && name.ends_with("__pop"))
            || name == "gorget_array_pop"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let result = if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.pop().unwrap_or(Value::Unit)
            } else { Value::Unit };
            return Ok(Some(result));
        }

        // Vector__T__remove(MutPtr(arr), idx) → raw element at idx
        // The C backend emits get-then-remove, returning the element directly (not Option).
        if (is_array_call(name) && name.ends_with("__remove"))
            || name == "gorget_array_remove"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let result = if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.remove(idx).unwrap_or(Value::Unit)
            } else { Value::Unit };
            return Ok(Some(result));
        }

        // Vector__T__is_empty(MutPtr(arr)) → bool
        if is_array_call(name) && name.ends_with("__is_empty") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let empty = self.get_array_from_value(&arr_arg)
                .map(|a| a.len() == 0)
                .unwrap_or(true);
            return Ok(Some(Value::Bool(empty)));
        }

        // Vector__T__insert(MutPtr(arr), idx, val) → void
        if (is_array_call(name) && name.ends_with("__insert"))
            || name == "gorget_array_insert"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let val = args.get(2).cloned().unwrap_or(Value::Unit);
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.insert(idx, val);
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__clear / gorget_array_clear
        if (is_array_call(name) && name.ends_with("__clear"))
            || name == "gorget_array_clear"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.clear();
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__contains / gorget_array_contains
        if (is_array_call(name) && name.ends_with("__contains"))
            || name == "gorget_array_contains"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let elem = args.get(1).cloned().unwrap_or(Value::Unit);
            let elem = match &elem {
                Value::Ptr(addr) | Value::MutPtr(addr) => {
                    self.heap_read(*addr).cloned().unwrap_or(elem.clone())
                }
                _ => elem,
            };
            let found = self.get_array_from_value(&arr_arg)
                .map(|a| a.contains(&elem)).unwrap_or(false);
            return Ok(Some(Value::Bool(found)));
        }

        // Vector__T__index_of / gorget_array_index_of
        if (is_array_call(name) && name.ends_with("__index_of"))
            || name == "gorget_array_index_of"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let elem = args.get(1).cloned().unwrap_or(Value::Unit);
            let elem = match &elem {
                Value::Ptr(addr) | Value::MutPtr(addr) => {
                    self.heap_read(*addr).cloned().unwrap_or(elem.clone())
                }
                _ => elem,
            };
            let result = match self.get_array_from_value(&arr_arg).and_then(|a| a.index_of(&elem)) {
                Some(idx) => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::I64(idx as i64)],
                },
                None => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 1, variant: "None".to_string(), fields: vec![],
                },
            };
            return Ok(Some(result));
        }

        // gorget_array_slice(arr_ptr, start, end) → new array
        // Also handles Vector__T__slice
        if name == "gorget_array_slice"
            || (is_array_call(name) && name.ends_with("__slice"))
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let start = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let end = args.get(2).map(|v| v.as_i64()).unwrap_or(0);
            let result = if let Some(arr) = self.get_array_from_value(&arr_arg) {
                let len = arr.len() as i64;
                let s = start.max(0).min(len) as usize;
                let e = end.max(0).min(len) as usize;
                let sliced = super::value::SimArray::new(arr.elem_type_name());
                for i in s..e { if let Some(v) = arr.get(i) { sliced.push(v); } }
                Value::Array(sliced)
            } else {
                Value::Array(super::value::SimArray::new("GorgetArray"))
            };
            return Ok(Some(result));
        }

        // gorget_array_extend(MutPtr(dst), src) → Unit
        if name == "gorget_array_extend"
            || (is_array_call(name) && name.ends_with("__extend"))
        {
            let dst_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let src_arg = args.get(1).cloned().unwrap_or(Value::Null);
            if let (Some(dst_arr), Some(src_arr)) = (self.get_array_from_value(&dst_arg), self.get_array_from_value(&src_arg)) {
                for i in 0..src_arr.len() { if let Some(v) = src_arr.get(i) { dst_arr.push(v); } }
            }
            return Ok(Some(Value::Unit));
        }

        // gorget_array_reserve / Vector__T__reserve → no-op (capacity management)
        if name == "gorget_array_reserve"
            || (is_array_call(name) && name.ends_with("__reserve"))
        {
            return Ok(Some(Value::Unit));
        }

        // Vector__T__clone / gorget_array_clone
        if (is_array_call(name) && name.ends_with("__clone"))
            || name == "gorget_array_clone"
        {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let result = self.get_array_from_value(&arr_arg)
                .map(|a| Value::Array(a.clone_deep()))
                .unwrap_or(Value::Array(super::value::SimArray::new("GorgetArray")));
            return Ok(Some(result));
        }

        // Vector__T__reverse
        if is_array_call(name) && name.ends_with("__reverse") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.data.borrow_mut().reverse();
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__windows(Ptr(arr), int n) → Vector[Vector[T]] of sliding slices
        if is_array_call(name) && name.ends_with("__windows") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let n = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let result = super::value::SimArray::new(name.trim_end_matches("__windows"));
            if n > 0 && n <= items.len() {
                for i in 0..=(items.len() - n) {
                    let win = super::value::SimArray::new("Vector__window");
                    for j in i..(i + n) {
                        win.push(items[j].clone());
                    }
                    result.push(Value::Array(win));
                }
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__chunks(Ptr(arr), int n) → Vector[Vector[T]] of non-overlapping slices
        if is_array_call(name) && name.ends_with("__chunks") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let n = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let result = super::value::SimArray::new(name.trim_end_matches("__chunks"));
            if n > 0 {
                let mut i = 0;
                while i < items.len() {
                    let chunk = super::value::SimArray::new("Vector__chunk");
                    let end = std::cmp::min(i + n, items.len());
                    for j in i..end {
                        chunk.push(items[j].clone());
                    }
                    result.push(Value::Array(chunk));
                    i += n;
                }
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__sort_by_key(MutPtr(arr), closure(T) → K) — in-place sort by key
        if is_array_call(name) && name.ends_with("__sort_by_key") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            // Pre-compute keys once (insertion sort compares keys, not elements).
            let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(items.len());
            for v in items {
                let k = self.call_closure_value(closure.clone(), vec![v.clone()], depth + 1)?;
                pairs.push((k, v));
            }
            // Insertion sort by key.
            for i in 1..pairs.len() {
                let cur = pairs[i].clone();
                let mut j = i;
                while j > 0 {
                    let a = &cur.0;
                    let b = &pairs[j - 1].0;
                    let ord = if a.is_integer() && b.is_integer() {
                        a.as_i64().cmp(&b.as_i64())
                    } else if a.is_float() || b.is_float() {
                        a.as_f64().partial_cmp(&b.as_f64()).unwrap_or(std::cmp::Ordering::Equal)
                    } else if a.is_string() && b.is_string() {
                        a.as_str_content().cmp(b.as_str_content())
                    } else {
                        std::cmp::Ordering::Equal
                    };
                    if ord == std::cmp::Ordering::Less {
                        pairs[j] = pairs[j - 1].clone();
                        j -= 1;
                    } else {
                        break;
                    }
                }
                pairs[j] = cur;
            }
            let sorted: Vec<Value> = pairs.into_iter().map(|(_, v)| v).collect();
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                *arr.data.borrow_mut() = sorted;
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__sorted_by_key(MutPtr(arr), closure(T) → K) — new sorted copy by key
        if is_array_call(name) && name.ends_with("__sorted_by_key") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(items.len());
            for v in items {
                let k = self.call_closure_value(closure.clone(), vec![v.clone()], depth + 1)?;
                pairs.push((k, v));
            }
            for i in 1..pairs.len() {
                let cur = pairs[i].clone();
                let mut j = i;
                while j > 0 {
                    let a = &cur.0;
                    let b = &pairs[j - 1].0;
                    let ord = if a.is_integer() && b.is_integer() {
                        a.as_i64().cmp(&b.as_i64())
                    } else if a.is_float() || b.is_float() {
                        a.as_f64().partial_cmp(&b.as_f64()).unwrap_or(std::cmp::Ordering::Equal)
                    } else if a.is_string() && b.is_string() {
                        a.as_str_content().cmp(b.as_str_content())
                    } else {
                        std::cmp::Ordering::Equal
                    };
                    if ord == std::cmp::Ordering::Less {
                        pairs[j] = pairs[j - 1].clone();
                        j -= 1;
                    } else {
                        break;
                    }
                }
                pairs[j] = cur;
            }
            let result = super::value::SimArray::new(name.trim_end_matches("__sorted_by_key"));
            for (_, v) in pairs {
                result.push(v);
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__sort_by(MutPtr(arr), closure(a, b) → int) — in-place sort with closure comparator
        if is_array_call(name) && name.ends_with("__sort_by") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let mut sorted = items.clone();
            // Simple insertion sort using closure comparator. Avoids holding a
            // sim-internal borrow across call_closure_value.
            for i in 1..sorted.len() {
                let cur = sorted[i].clone();
                let mut j = i;
                while j > 0 {
                    let r = self.call_closure_value(closure.clone(), vec![cur.clone(), sorted[j - 1].clone()], depth + 1)?;
                    if r.as_i64() < 0 {
                        sorted[j] = sorted[j - 1].clone();
                        j -= 1;
                    } else {
                        break;
                    }
                }
                sorted[j] = cur;
            }
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                let data = arr.data.borrow_mut();
                drop(data);
                *arr.data.borrow_mut() = sorted;
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__sort (in-place sort by value)
        if is_array_call(name) && name.ends_with("__sort") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                arr.data.borrow_mut().sort_by(|a, b| {
                    if a.is_integer() && b.is_integer() {
                        a.as_i64().cmp(&b.as_i64())
                    } else if a.is_float() || b.is_float() {
                        a.as_f64().partial_cmp(&b.as_f64()).unwrap_or(std::cmp::Ordering::Equal)
                    } else if a.is_string() && b.is_string() {
                        a.as_str_content().cmp(b.as_str_content())
                    } else {
                        std::cmp::Ordering::Equal
                    }
                });
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__sorted_by(MutPtr(arr), closure(a, b) → int) — new sorted copy with closure
        if is_array_call(name) && name.ends_with("__sorted_by") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let mut sorted = items;
            for i in 1..sorted.len() {
                let cur = sorted[i].clone();
                let mut j = i;
                while j > 0 {
                    let r = self.call_closure_value(closure.clone(), vec![cur.clone(), sorted[j - 1].clone()], depth + 1)?;
                    if r.as_i64() < 0 {
                        sorted[j] = sorted[j - 1].clone();
                        j -= 1;
                    } else {
                        break;
                    }
                }
                sorted[j] = cur;
            }
            let result = super::value::SimArray::new(name.trim_end_matches("__sorted_by"));
            for v in sorted {
                result.push(v);
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__sorted → new sorted copy
        if is_array_call(name) && name.ends_with("__sorted") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            if let Some(arr) = self.get_array_from_value(&arr_arg) {
                let cloned = arr.clone_deep();
                cloned.data.borrow_mut().sort_by(|a, b| {
                    if a.is_integer() && b.is_integer() {
                        a.as_i64().cmp(&b.as_i64())
                    } else if a.is_float() || b.is_float() {
                        a.as_f64().partial_cmp(&b.as_f64()).unwrap_or(std::cmp::Ordering::Equal)
                    } else if a.is_string() && b.is_string() {
                        a.as_str_content().cmp(b.as_str_content())
                    } else {
                        std::cmp::Ordering::Equal
                    }
                });
                return Ok(Some(Value::Array(cloned)));
            }
            return Ok(Some(Value::Array(super::value::SimArray::new(name))));
        }

        // Vector__T__reduce(MutPtr(arr), closure(a, b)) → element
        if is_array_call(name) && name.ends_with("__reduce") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            if items.is_empty() {
                return Ok(Some(Value::Unit));
            }
            let mut acc = items[0].clone();
            for v in items.into_iter().skip(1) {
                acc = self.call_closure_value(closure.clone(), vec![acc, v], depth + 1)?;
            }
            return Ok(Some(acc));
        }

        // gorget_array_free / Vector__T__free — no-op
        if name == "gorget_array_free"
            || (is_array_call(name) && name.ends_with("__free"))
        {
            return Ok(Some(Value::Unit));
        }

        // Vector__T__any(MutPtr(arr), closure(elem) → bool) → bool
        if is_array_call(name) && name.ends_with("__any") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            for v in items {
                let r = self.call_closure_value(closure.clone(), vec![v], depth + 1)?;
                if r.as_bool() { return Ok(Some(Value::Bool(true))); }
            }
            return Ok(Some(Value::Bool(false)));
        }

        // Vector__T__all(MutPtr(arr), closure(elem) → bool) → bool
        if is_array_call(name) && name.ends_with("__all") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            for v in items {
                let r = self.call_closure_value(closure.clone(), vec![v], depth + 1)?;
                if !r.as_bool() { return Ok(Some(Value::Bool(false))); }
            }
            return Ok(Some(Value::Bool(true)));
        }

        // Vector__T__fold(MutPtr(arr), initial, closure(acc, elem)) → acc
        if is_array_call(name) && name.ends_with("__fold") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let initial = args.get(1).cloned().unwrap_or(Value::I64(0));
            let closure = args.get(2).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let mut acc = initial;
            for v in items {
                acc = self.call_closure_value(closure.clone(), vec![acc, v], depth + 1)?;
            }
            return Ok(Some(acc));
        }

        // Vector__T__filter(MutPtr(arr), closure(elem) → bool) → new Array
        if is_array_call(name) && name.ends_with("__filter") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let result = super::value::SimArray::new(name.trim_end_matches("__filter"));
            for v in items {
                let r = self.call_closure_value(closure.clone(), vec![v.clone()], depth + 1)?;
                if r.as_bool() { result.push(v); }
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__map(MutPtr(arr), closure(elem) → new_elem) → new Array
        if is_array_call(name) && name.ends_with("__map") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let result = super::value::SimArray::new(name.trim_end_matches("__map"));
            for v in items {
                let r = self.call_closure_value(closure.clone(), vec![v], depth + 1)?;
                result.push(r);
            }
            return Ok(Some(Value::Array(result)));
        }

        // Vector__T__for_each(MutPtr(arr), closure(elem)) → void
        if is_array_call(name) && (name.ends_with("__for_each") || name.ends_with("__each")) {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let closure = args.get(1).cloned().unwrap_or(Value::Unit);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            for v in items {
                self.call_closure_value(closure.clone(), vec![v], depth + 1)?;
            }
            return Ok(Some(Value::Unit));
        }

        // Vector__T__enumerate(MutPtr(arr)) → new Array of (int, elem) tuples
        if is_array_call(name) && name.ends_with("__enumerate") {
            let arr_arg = args.get(0).cloned().unwrap_or(Value::Null);
            let items = self.get_array_from_value(&arr_arg).map(|a| a.to_vec()).unwrap_or_default();
            let result = super::value::SimArray::new("Vector__enum");
            for (i, v) in items.into_iter().enumerate() {
                result.push(Value::Tuple(vec![Value::I64(i as i64), v]));
            }
            return Ok(Some(Value::Array(result)));
        }

        // ────────── DICT / HASHMAP operations ──────────
        let is_dict_call = name.starts_with("Dict__")
            || name.starts_with("HashMap__")
            || name.starts_with("gorget_dict_")
            || name.starts_with("gorget_map_");

        if is_dict_call {
            // Dict__K__V__new() / HashMap__K__V__new()
            if name.ends_with("__new") {
                return Ok(Some(Value::Dict(super::value::SimDict::new(
                    name.trim_end_matches("__new"),
                ))));
            }

            // gorget_map_put(map_ptr, key_ptr, val_ptr) — low-level with pointer args
            if name == "gorget_map_put" || name == "gorget_dict_put" {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = self.deref_ptr(args.get(1).cloned().unwrap_or(Value::Unit));
                let val = self.deref_ptr(args.get(2).cloned().unwrap_or(Value::Unit));
                if let Some(d) = self.get_dict_from_value(&dict_arg) {
                    d.set(key, val);
                }
                return Ok(Some(Value::Unit));
            }
            // Dict__K__V__put / Dict__K__V__set / HashMap__K__V__put (MutPtr(dict), key, val)
            if name.ends_with("__put") || (name.ends_with("__set") && !name.ends_with("__is_subset") && !name.ends_with("__is_superset")) {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = args.get(1).cloned().unwrap_or(Value::Unit);
                let val = args.get(2).cloned().unwrap_or(Value::Unit);
                if let Some(d) = self.get_dict_from_value(&dict_arg) {
                    d.set(key, val);
                }
                return Ok(Some(Value::Unit));
            }
            // Dict__K__V__get_or_put (MutPtr(dict), key, default) → raw V
            if name.ends_with("__get_or_put") || name.ends_with("__get_or") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = args.get(1).cloned().unwrap_or(Value::Unit);
                let default = args.get(2).cloned().unwrap_or(Value::I64(0));
                if let Some(d) = self.get_dict_from_value(&dict_arg) {
                    if let Some(existing) = d.get(&key) {
                        return Ok(Some(existing));
                    } else {
                        d.set(key, default.clone());
                        return Ok(Some(default));
                    }
                }
                return Ok(Some(default));
            }

            // gorget_map_get(map_ptr, key_ptr) → raw V (low-level, key passed as pointer)
            if name == "gorget_map_get" || name == "gorget_dict_get" {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = self.deref_ptr(args.get(1).cloned().unwrap_or(Value::Unit));
                let result = self.get_dict_from_value(&dict_arg)
                    .and_then(|d| d.get(&key))
                    .unwrap_or(Value::I64(0));
                return Ok(Some(result));
            }
            // Dict__K__V__get (Ptr/MutPtr(dict), key) → raw V (or 0 if not found)
            // The GIR treats dict.get() as returning the raw value type (I64_TYPE), not Option.
            // This matches the C backend: gorget_map_get returns void*, dereffed or zero-initialised.
            // The `.unwrap()` call in Gorget source becomes a no-op in GIR.
            if name.ends_with("__get") && !name.contains("__get_or") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = args.get(1).cloned().unwrap_or(Value::Unit);
                let result = self.get_dict_from_value(&dict_arg)
                    .and_then(|d| d.get(&key))
                    .unwrap_or(Value::I64(0));
                return Ok(Some(result));
            }

            // Dict__K__V__contains / gorget_dict_contains
            if name.ends_with("__contains") || name.ends_with("__has_key") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = args.get(1).cloned().unwrap_or(Value::Unit);
                let found = self.get_dict_from_value(&dict_arg).map(|d| d.contains(&key)).unwrap_or(false);
                return Ok(Some(Value::Bool(found)));
            }

            // Dict__K__V__remove(MutPtr(dict), key) → Option[V !]
            // Returns Some(removed value) or None.
            if name.ends_with("__remove") || name.ends_with("__delete") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = args.get(1).cloned().unwrap_or(Value::Unit);
                let type_suffix = if let Some(rest) = name.strip_prefix("Dict__")
                    .or_else(|| name.strip_prefix("HashMap__"))
                {
                    // rest is like "Str__int64_t__remove" — take the val_name slice.
                    let trimmed = rest.strip_suffix("__remove").or_else(|| rest.strip_suffix("__delete")).unwrap_or(rest);
                    trimmed.find("__").map(|pos| trimmed[pos + 2..].to_string()).unwrap_or_else(|| "int64_t".to_string())
                } else { "int64_t".to_string() };
                let result = match self.get_dict_from_value(&dict_arg).and_then(|d| d.remove(&key)) {
                    Some(v) => Value::Enum {
                        type_name: format!("Option__{type_suffix}"),
                        tag: 0, variant: "Some".to_string(), fields: vec![v],
                    },
                    None => Value::Enum {
                        type_name: format!("Option__{type_suffix}"),
                        tag: 1, variant: "None".to_string(), fields: vec![],
                    },
                };
                return Ok(Some(result));
            }

            // Dict__K__V__len
            if name.ends_with("__len") || name.ends_with("__size") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let len = self.get_dict_from_value(&dict_arg).map(|d| d.len()).unwrap_or(0);
                return Ok(Some(Value::I64(len as i64)));
            }

            // Dict__K__V__is_empty / gorget_map_contains (in operator for HashMap)
            if name.ends_with("__is_empty") || name == "gorget_dict_is_empty" || name == "gorget_map_is_empty" {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let empty = self.get_dict_from_value(&dict_arg).map(|d| d.len() == 0).unwrap_or(true);
                return Ok(Some(Value::Bool(empty)));
            }
            // gorget_map_contains(map_ptr, key_ptr, hash) → bool  (for `key in hashmap`)
            if name == "gorget_map_contains" {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let key = match args.get(1).cloned().unwrap_or(Value::Null) {
                    // Key is passed as Ptr → deref to actual key
                    Value::Ptr(a) | Value::MutPtr(a) => self.heap_read(a).cloned().unwrap_or(Value::Unit),
                    other => other,
                };
                let found = self.get_dict_from_value(&dict_arg).map(|d| d.contains(&key)).unwrap_or(false);
                return Ok(Some(Value::Bool(found)));
            }

            // Dict__K__V__keys → Vector[K]
            if name.ends_with("__keys") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let keys = self.get_dict_from_value(&dict_arg).map(|d| d.keys()).unwrap_or_default();
                let arr = super::value::SimArray::new("Vector__keys");
                for k in keys { arr.push(k); }
                return Ok(Some(Value::Array(arr)));
            }

            // Dict__K__V__values → Vector[V]
            if name.ends_with("__values") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let vals = self.get_dict_from_value(&dict_arg).map(|d| d.values()).unwrap_or_default();
                let arr = super::value::SimArray::new("Vector__values");
                for v in vals { arr.push(v); }
                return Ok(Some(Value::Array(arr)));
            }

            // Dict__K__V__items → Vector[(K,V)]
            if name.ends_with("__items") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                let arr = super::value::SimArray::new("Vector__items");
                for (k, v) in items { arr.push(Value::Tuple(vec![k, v])); }
                return Ok(Some(Value::Array(arr)));
            }

            // Dict clear/free
            if name.ends_with("__clear") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                if let Some(d) = self.get_dict_from_value(&dict_arg) { d.clear(); }
                return Ok(Some(Value::Unit));
            }
            if name.ends_with("__free") || name == "gorget_dict_free" || name == "gorget_map_free" {
                return Ok(Some(Value::Unit));
            }

            // Dict__K__V__fold(dict_ptr, initial, closure(acc, k, v)) → acc
            if name.ends_with("__fold") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let initial = args.get(1).cloned().unwrap_or(Value::I64(0));
                let closure = args.get(2).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                let mut acc = initial;
                for (k, v) in items {
                    acc = self.call_closure_value(closure.clone(), vec![acc, k, v], depth + 1)?;
                }
                return Ok(Some(acc));
            }

            // Dict__K__V__filter(dict_ptr, closure(k, v) → bool) → new Dict
            if name.ends_with("__filter") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                let result = super::value::SimDict::new(name.trim_end_matches("__filter"));
                for (k, v) in items {
                    let keep = self.call_closure_value(closure.clone(), vec![k.clone(), v.clone()], depth + 1)?;
                    if keep.as_bool() { result.set(k, v); }
                }
                return Ok(Some(Value::Dict(result)));
            }

            // Dict__K__V__map(dict_ptr, closure(k, v) → new_v) → new Dict (same keys)
            if name.ends_with("__map") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                let result = super::value::SimDict::new(name.trim_end_matches("__map"));
                for (k, v) in items {
                    let new_v = self.call_closure_value(closure.clone(), vec![k.clone(), v], depth + 1)?;
                    result.set(k, new_v);
                }
                return Ok(Some(Value::Dict(result)));
            }

            // Dict__K__V__for_each / __each (dict_ptr, closure(k, v))
            if name.ends_with("__for_each") || name.ends_with("__each") {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                for (k, v) in items {
                    self.call_closure_value(closure.clone(), vec![k, v], depth + 1)?;
                }
                return Ok(Some(Value::Unit));
            }

            // Dict__K__V__any(dict_ptr, closure(k, v) → bool) → bool
            if name.ends_with("__any") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                for (k, v) in items {
                    if self.call_closure_value(closure.clone(), vec![k, v], depth + 1)?.as_bool() {
                        return Ok(Some(Value::Bool(true)));
                    }
                }
                return Ok(Some(Value::Bool(false)));
            }

            // Dict__K__V__all(dict_ptr, closure(k, v) → bool) → bool
            if name.ends_with("__all") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                for (k, v) in items {
                    if !self.call_closure_value(closure.clone(), vec![k, v], depth + 1)?.as_bool() {
                        return Ok(Some(Value::Bool(false)));
                    }
                }
                return Ok(Some(Value::Bool(true)));
            }

            // Dict__K__V__map_values(dict_ptr, closure(v) → new_v) → new Dict
            if name.ends_with("__map_values") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let items = self.get_dict_from_value(&dict_arg).map(|d| d.items()).unwrap_or_default();
                let result = super::value::SimDict::new(name.trim_end_matches("__map_values"));
                for (k, v) in items {
                    let new_v = self.call_closure_value(closure.clone(), vec![v], depth + 1)?;
                    result.set(k, new_v);
                }
                return Ok(Some(Value::Dict(result)));
            }

            // Dict__K__V__update(dict_ptr, other_dict_ptr) → Unit
            if name.ends_with("__update") && is_dict_call {
                let dict_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let other_arg = args.get(1).cloned().unwrap_or(Value::Null);
                if let (Some(dst_d), Some(src_d)) = (self.get_dict_from_value(&dict_arg), self.get_dict_from_value(&other_arg)) {
                    for (k, v) in src_d.items() { dst_d.set(k, v); }
                }
                return Ok(Some(Value::Unit));
            }
        }

        // ── Set operations ─────────────────────────────────────────────────────
        // Sets are implemented as Dict with Unit values.
        let is_set_call = name.starts_with("Set__")
            || name.starts_with("gorget_set_")
            || name.starts_with("set_");
        if is_set_call {
            // Set__T__new() → empty dict-as-set
            if name.ends_with("__new") || name == "gorget_set_new" {
                let type_name = name.strip_suffix("__new").unwrap_or("Set__").to_string();
                return Ok(Some(Value::Dict(super::value::SimDict::new(type_name))));
            }
            // Set__T__add / gorget_set_add (MutPtr(set), elem or &elem)
            if name.ends_with("__add") || name == "gorget_set_add" || name.ends_with("__insert") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                // Low-level gorget_set_add passes elem as a pointer; deref it.
                let elem = self.deref_if_low_level_call(name, args.get(1).cloned().unwrap_or(Value::Unit));
                if let Some(d) = self.get_dict_from_value(&set_arg) {
                    d.set(elem, Value::Unit);
                }
                return Ok(Some(Value::Unit));
            }
            // Set__T__contains / gorget_set_contains
            if name.ends_with("__contains") || name == "gorget_set_contains" {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let elem = self.deref_if_low_level_call(name, args.get(1).cloned().unwrap_or(Value::Unit));
                let found = self.get_dict_from_value(&set_arg).map(|d| d.contains(&elem)).unwrap_or(false);
                return Ok(Some(Value::Bool(found)));
            }
            // Set__T__remove / gorget_set_remove
            if name.ends_with("__remove") || name == "gorget_set_remove" {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let elem = self.deref_if_low_level_call(name, args.get(1).cloned().unwrap_or(Value::Unit));
                if let Some(d) = self.get_dict_from_value(&set_arg) {
                    d.remove(&elem);
                }
                return Ok(Some(Value::Unit));
            }
            // Set__T__len / gorget_set_len
            if name.ends_with("__len") || name == "gorget_set_len" || name.ends_with("__size") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let len = self.get_dict_from_value(&set_arg).map(|d| d.len()).unwrap_or(0);
                return Ok(Some(Value::I64(len as i64)));
            }
            // Set__T__is_empty
            if name.ends_with("__is_empty") || name == "gorget_set_is_empty" {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let empty = self.get_dict_from_value(&set_arg).map(|d| d.len() == 0).unwrap_or(true);
                return Ok(Some(Value::Bool(empty)));
            }
            // Set__T__to_vector / gorget_set_to_vector → Vector[T]
            if name.ends_with("__to_vector") || name.ends_with("__elems") || name.ends_with("__elements") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let elems = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                let arr = super::value::SimArray::new("Vector__elems");
                for e in elems { arr.push(e); }
                return Ok(Some(Value::Array(arr)));
            }
            // Set operations: union, intersection, difference, is_subset, is_superset
            if name.ends_with("__union") || name == "gorget_set_union" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = super::value::SimDict::new("Set__union");
                if let Some(a) = a { for k in a.keys() { result.set(k, Value::Unit); } }
                if let Some(b) = b { for k in b.keys() { result.set(k, Value::Unit); } }
                return Ok(Some(Value::Dict(result)));
            }
            if name.ends_with("__intersection") || name == "gorget_set_intersection" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = super::value::SimDict::new("Set__intersection");
                if let (Some(a), Some(b)) = (a, b) {
                    for k in a.keys() { if b.contains(&k) { result.set(k, Value::Unit); } }
                }
                return Ok(Some(Value::Dict(result)));
            }
            if name.ends_with("__difference") || name == "gorget_set_difference" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = super::value::SimDict::new("Set__difference");
                if let Some(a) = a {
                    let b_check = b;
                    for k in a.keys() {
                        if b_check.as_ref().map(|b| !b.contains(&k)).unwrap_or(true) {
                            result.set(k, Value::Unit);
                        }
                    }
                }
                return Ok(Some(Value::Dict(result)));
            }
            if name.ends_with("__is_subset") || name == "gorget_set_is_subset" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = match (a, b) {
                    (Some(a), Some(b)) => a.keys().iter().all(|k| b.contains(k)),
                    _ => false,
                };
                return Ok(Some(Value::Bool(result)));
            }
            if name.ends_with("__is_superset") || name == "gorget_set_is_superset" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = match (a, b) {
                    (Some(a), Some(b)) => b.keys().iter().all(|k| a.contains(k)),
                    _ => false,
                };
                return Ok(Some(Value::Bool(result)));
            }
            if name.ends_with("__symmetric_difference") || name == "gorget_set_symmetric_difference" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = super::value::SimDict::new("Set__symmetric_difference");
                if let (Some(a), Some(b)) = (&a, &b) {
                    for k in a.keys() { if !b.contains(&k) { result.set(k, Value::Unit); } }
                    for k in b.keys() { if !a.contains(&k) { result.set(k, Value::Unit); } }
                }
                return Ok(Some(Value::Dict(result)));
            }
            if name.ends_with("__is_disjoint") || name == "gorget_set_is_disjoint" {
                let a = self.get_dict_from_value(&args.get(0).cloned().unwrap_or(Value::Null));
                let b = self.get_dict_from_value(&args.get(1).cloned().unwrap_or(Value::Null));
                let result = match (a, b) {
                    (Some(a), Some(b)) => !a.keys().iter().any(|k| b.contains(k)),
                    _ => true,
                };
                return Ok(Some(Value::Bool(result)));
            }
            if name.ends_with("__clear") || name == "gorget_set_clear" {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                if let Some(d) = self.get_dict_from_value(&set_arg) { d.clear(); }
                return Ok(Some(Value::Unit));
            }
            if name.ends_with("__free") || name == "gorget_set_free" {
                return Ok(Some(Value::Unit));
            }
            // Set__T__fold(set_ptr, initial, closure(acc, elem)) → acc
            if name.ends_with("__fold") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let initial = args.get(1).cloned().unwrap_or(Value::I64(0));
                let closure = args.get(2).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                let mut acc = initial;
                for k in keys {
                    acc = self.call_closure_value(closure.clone(), vec![acc, k], depth + 1)?;
                }
                return Ok(Some(acc));
            }
            // Set__T__filter(set_ptr, closure(elem) → bool) → new Set
            if name.ends_with("__filter") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                let type_name = name.strip_suffix("__filter").unwrap_or("Set__").to_string();
                let result = super::value::SimDict::new(type_name);
                for k in keys {
                    let r = self.call_closure_value(closure.clone(), vec![k.clone()], depth + 1)?;
                    if r.as_bool() { result.set(k, Value::Unit); }
                }
                return Ok(Some(Value::Dict(result)));
            }
            // Set__T__map(set_ptr, closure(elem) → new_elem) → new Set
            if name.ends_with("__map") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                let type_name = name.strip_suffix("__map").unwrap_or("Set__").to_string();
                let result = super::value::SimDict::new(type_name);
                for k in keys {
                    let r = self.call_closure_value(closure.clone(), vec![k], depth + 1)?;
                    result.set(r, Value::Unit);
                }
                return Ok(Some(Value::Dict(result)));
            }
            // Set__T__for_each(set_ptr, closure(elem)) → void
            if name.ends_with("__for_each") || name.ends_with("__each") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                for k in keys {
                    self.call_closure_value(closure.clone(), vec![k], depth + 1)?;
                }
                return Ok(Some(Value::Unit));
            }
            // Set__T__any(set_ptr, closure(elem) → bool) → bool
            if name.ends_with("__any") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                for k in keys {
                    let r = self.call_closure_value(closure.clone(), vec![k], depth + 1)?;
                    if r.as_bool() { return Ok(Some(Value::Bool(true))); }
                }
                return Ok(Some(Value::Bool(false)));
            }
            // Set__T__all(set_ptr, closure(elem) → bool) → bool
            if name.ends_with("__all") {
                let set_arg = args.get(0).cloned().unwrap_or(Value::Null);
                let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                let keys = self.get_dict_from_value(&set_arg).map(|d| d.keys()).unwrap_or_default();
                for k in keys {
                    let r = self.call_closure_value(closure.clone(), vec![k], depth + 1)?;
                    if !r.as_bool() { return Ok(Some(Value::Bool(false))); }
                }
                return Ok(Some(Value::Bool(true)));
            }
        }

        // ────────── ALLOCATOR dispatch (TrackingAllocator / Arena / PoolAllocator) ──────────
        // All three allocator types are stored in `tracking_allocs` keyed by I64 ID.
        // GIR lowering maps constructors to gorget_*_new and methods to TypeName__method.
        let is_alloc_call = name == "gorget_tracking_new" || name == "TrackingAllocator"
            || name.starts_with("gorget_tracking_")
            || name.starts_with("TrackingAllocator__")
            || name == "gorget_arena_new" || name == "Arena" || name == "ArenaCheckpoint"
            || name.starts_with("gorget_arena_")
            || name.starts_with("Arena__")
            || name == "gorget_pool_new" || name == "PoolAllocator"
            || name.starts_with("gorget_pool_")
            || name.starts_with("PoolAllocator__");

        if is_alloc_call {
            // ── Constructors ──
            if name == "gorget_tracking_new" || name == "TrackingAllocator" {
                let id = self.tracking_next_id;
                self.tracking_next_id += 1;
                self.tracking_allocs.insert(id, SimAllocState::tracking_default());
                return Ok(Some(Value::I64(id as i64)));
            }
            if name == "gorget_arena_new" || name == "Arena" {
                let capacity = args.first().map(|v| v.as_i64()).unwrap_or(0).max(0);
                let id = self.tracking_next_id;
                self.tracking_next_id += 1;
                self.tracking_allocs.insert(id, SimAllocState::Arena { bytes_used: 0, capacity });
                return Ok(Some(Value::I64(id as i64)));
            }
            if name == "gorget_pool_new" || name == "PoolAllocator" {
                let block_size = args.get(0).map(|v| v.as_i64()).unwrap_or(64).max(1);
                let total_blocks = args.get(1).map(|v| v.as_i64()).unwrap_or(0).max(0);
                let id = self.tracking_next_id;
                self.tracking_next_id += 1;
                self.tracking_allocs.insert(id, SimAllocState::Pool { block_size, total_blocks, used_blocks: 0 });
                return Ok(Some(Value::I64(id as i64)));
            }
            if name == "gorget_tlsf_new" || name == "TlsfAllocator" {
                let pool_size = args.first().map(|v| v.as_i64()).unwrap_or(65536).max(1);
                let id = self.tracking_next_id;
                self.tracking_next_id += 1;
                self.tracking_allocs.insert(id, SimAllocState::Tlsf { bytes_used: 0, peak_bytes: 0, pool_size });
                return Ok(Some(Value::I64(id as i64)));
            }

            // ── Helper: resolve allocator ID from arg0 ──
            let alloc_id: Option<usize> = {
                let raw = args.first().cloned().unwrap_or(Value::Unit);
                match raw {
                    Value::I64(id) if id >= 0 && self.tracking_allocs.contains_key(&(id as usize)) => {
                        Some(id as usize)
                    }
                    Value::MutPtr(addr) | Value::Ptr(addr) => {
                        match self.heap.get(&addr) {
                            Some(Value::I64(id)) => Some(*id as usize),
                            Some(Value::Struct { fields, .. }) => {
                                fields.first().and_then(|f| if let Value::I64(id) = f { Some(*id as usize) } else { None })
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            };
            // Extract method name from any of the known prefixes
            let method = name.strip_prefix("gorget_tracking_")
                .or_else(|| name.strip_prefix("TrackingAllocator__"))
                .or_else(|| name.strip_prefix("gorget_arena_"))
                .or_else(|| name.strip_prefix("Arena__"))
                .or_else(|| name.strip_prefix("gorget_pool_"))
                .or_else(|| name.strip_prefix("PoolAllocator__"))
                .or_else(|| name.strip_prefix("gorget_tlsf_"))
                .or_else(|| name.strip_prefix("TlsfAllocator__"))
                .unwrap_or(name);
            if let Some(id) = alloc_id {
                match method {
                    "destroy" => { self.tracking_allocs.remove(&id); return Ok(Some(Value::Unit)); }
                    "reset" => {
                        if let Some(state) = self.tracking_allocs.get_mut(&id) {
                            match state {
                                SimAllocState::Tracking { .. } => *state = SimAllocState::tracking_default(),
                                SimAllocState::Arena { bytes_used, .. } => *bytes_used = 0,
                                SimAllocState::Pool { used_blocks, .. } => *used_blocks = 0,
                                SimAllocState::Tlsf { bytes_used, peak_bytes, .. } => { *bytes_used = 0; *peak_bytes = 0; }
                            }
                        }
                        return Ok(Some(Value::Unit));
                    }
                    "report" => { return Ok(Some(Value::Unit)); } // C runtime prints to stderr; skip
                    "restore" => {
                        // Restore arena bytes_used from checkpoint struct
                        let cp_bytes = args.get(1).and_then(|v| {
                            if let Value::Struct { fields, .. } = v {
                                fields.first().and_then(|f| if let Value::I64(n) = f { Some(*n) } else { None })
                            } else { None }
                        }).unwrap_or(0);
                        if let Some(state) = self.tracking_allocs.get_mut(&id) {
                            if let SimAllocState::Arena { bytes_used, .. } = state {
                                *bytes_used = cp_bytes;
                            }
                        }
                        return Ok(Some(Value::Unit));
                    }
                    _ => {}
                }
                if let Some(state) = self.tracking_allocs.get(&id) {
                    match state {
                        SimAllocState::Tracking {
                            bytes_allocated, current_bytes, peak_bytes,
                            alloc_count, free_count, realloc_count, bytes_freed,
                        } => match method {
                            "bytes_allocated" => return Ok(Some(Value::I64(*bytes_allocated))),
                            "current_bytes"   => return Ok(Some(Value::I64(*current_bytes))),
                            "peak_bytes"      => return Ok(Some(Value::I64(*peak_bytes))),
                            "alloc_count"     => return Ok(Some(Value::I64(*alloc_count))),
                            "free_count"      => return Ok(Some(Value::I64(*free_count))),
                            "realloc_count"   => return Ok(Some(Value::I64(*realloc_count))),
                            "bytes_freed"     => return Ok(Some(Value::I64(*bytes_freed))),
                            _ => {}
                        },
                        SimAllocState::Arena { bytes_used, capacity } => match method {
                            "bytes_used" => return Ok(Some(Value::I64(*bytes_used))),
                            "capacity"   => return Ok(Some(Value::I64(*capacity))),
                            "checkpoint" => {
                                // Return checkpoint as a struct with the bytes_used snapshot
                                return Ok(Some(Value::Struct {
                                    type_name: "ArenaCheckpoint".to_string(),
                                    fields: vec![Value::I64(*bytes_used)],
                                }));
                            }
                            _ => {}
                        },
                        SimAllocState::Pool { block_size, total_blocks, used_blocks } => match method {
                            "block_size"    => return Ok(Some(Value::I64(*block_size))),
                            "total_blocks"  => return Ok(Some(Value::I64(*total_blocks))),
                            "used_blocks"   => return Ok(Some(Value::I64(*used_blocks))),
                            "free_blocks"   => return Ok(Some(Value::I64(*total_blocks - *used_blocks))),
                            _ => {}
                        },
                        SimAllocState::Tlsf { bytes_used, peak_bytes, pool_size } => match method {
                            "bytes_used" => return Ok(Some(Value::I64(*bytes_used))),
                            "peak_bytes" => return Ok(Some(Value::I64(*peak_bytes))),
                            "pool_size"  => return Ok(Some(Value::I64(*pool_size))),
                            _ => {}
                        },
                    }
                }
            }
            return Ok(Some(Value::Unit));
        }

        // ────────── OPTION / RESULT method dispatch ──────────
        // These are emitted by the C backend as InlineMethod variants from Option__T__method and
        // Result__T__E__method calls (which are regular Call instructions in GIR).
        let is_option_call = name.starts_with("Option__");
        let is_result_call = name.starts_with("Result__");

        if is_option_call || is_result_call {
            // Helper: extract the enum value from arg0 (may be Ptr/MutPtr to enum)
            let opt_val = {
                let raw = args.get(0).cloned().unwrap_or(Value::Null);
                match raw {
                    Value::Ptr(addr) | Value::MutPtr(addr) => {
                        self.heap_read(addr).cloned().unwrap_or(Value::Unit)
                    }
                    other => other,
                }
            };

            // Extract the method name (after the last "__")
            let method = name.rfind("__").map(|p| &name[p+2..]).unwrap_or("");

            match method {
                "is_some" => {
                    let is_some = matches!(&opt_val, Value::Enum { tag: 0, .. });
                    return Ok(Some(Value::Bool(is_some)));
                }
                "is_none" => {
                    let is_none = !matches!(&opt_val, Value::Enum { tag: 0, .. });
                    return Ok(Some(Value::Bool(is_none)));
                }
                "is_ok" => {
                    let is_ok = matches!(&opt_val, Value::Enum { tag: 0, .. });
                    return Ok(Some(Value::Bool(is_ok)));
                }
                "is_error" => {
                    let is_err = !matches!(&opt_val, Value::Enum { tag: 0, .. });
                    return Ok(Some(Value::Bool(is_err)));
                }
                "map" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        Value::Enum { type_name: ref tn, tag: 0, ref fields, .. } => {
                            let inner = fields.first().cloned().unwrap_or(Value::Unit);
                            let mapped = self.call_closure_value(closure, vec![inner], depth + 1)?;
                            Value::Enum { type_name: tn.clone(), tag: 0, variant: if is_result_call { "Ok".to_string() } else { "Some".to_string() }, fields: vec![mapped] }
                        }
                        other @ Value::Enum { tag: 1, .. } => other,
                        _ => opt_val.clone(),
                    }));
                }
                "and_then" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        Value::Enum { tag: 0, ref fields, .. } => {
                            let inner = fields.first().cloned().unwrap_or(Value::Unit);
                            self.call_closure_value(closure, vec![inner], depth + 1)?
                        }
                        other => other,
                    }));
                }
                "map_err" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        Value::Enum { type_name: ref tn, tag: 1, ref fields, .. } => {
                            let inner = fields.first().cloned().unwrap_or(Value::Unit);
                            let mapped = self.call_closure_value(closure, vec![inner], depth + 1)?;
                            Value::Enum { type_name: tn.clone(), tag: 1, variant: "Error".to_string(), fields: vec![mapped] }
                        }
                        other => other,
                    }));
                }
                "or" => {
                    let other = args.get(1).cloned().unwrap_or(Value::Null);
                    return Ok(Some(match opt_val {
                        ref v @ Value::Enum { tag: 0, .. } => v.clone(),
                        _ => other,
                    }));
                }
                "or_else" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        ref v @ Value::Enum { tag: 0, .. } => v.clone(),
                        _ => self.call_closure_value(closure, vec![], depth + 1)?,
                    }));
                }
                "filter" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        ref v @ Value::Enum { tag: 0, ref type_name, ref fields, .. } => {
                            let inner = fields.first().cloned().unwrap_or(Value::Unit);
                            let keep = self.call_closure_value(closure, vec![inner], depth + 1)?.as_bool();
                            if keep { v.clone() } else {
                                Value::Enum { type_name: type_name.clone(), tag: 1, variant: "None".to_string(), fields: vec![] }
                            }
                        }
                        other => other,
                    }));
                }
                "flatten" => {
                    return Ok(Some(match opt_val {
                        Value::Enum { tag: 0, fields, .. } => {
                            fields.into_iter().next().unwrap_or(Value::Unit)
                        }
                        other => other,
                    }));
                }
                "unwrap_or_else" => {
                    let closure = args.get(1).cloned().unwrap_or(Value::Unit);
                    return Ok(Some(match opt_val {
                        Value::Enum { tag: 0, fields, .. } => fields.into_iter().next().unwrap_or(Value::Unit),
                        _ => self.call_closure_value(closure, vec![], depth + 1)?,
                    }));
                }
                "unwrap_error" => {
                    return Ok(Some(match opt_val {
                        Value::Enum { tag: 1, fields, .. } => fields.into_iter().next().unwrap_or(Value::Unit),
                        _ => return Err(SimError::Panic("unwrap_error called on Ok".to_string())),
                    }));
                }
                _ => {}
            }
        }

        // ────────── Bytes write/read operations (need heap mutation) ──────────
        // gorget_bytes_write_*: mutates the array through a MutPtr arg.
        // gorget_bytes_read_*: reads from the array through a Ptr/MutPtr arg.
        if name.starts_with("gorget_bytes_write_") || name.starts_with("gorget_bytes_read_") {
            // arg[0] = MutPtr/Ptr to array, arg[1] = offset, arg[2] = value (for writes)
            let arr_arg = args.first().cloned().unwrap_or(Value::Null);
            let arr_addr = match arr_arg {
                Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => Some(a),
                _ => None,
            };

            if let Some(addr) = arr_addr {
                let arr = self.heap.get(&addr).cloned().unwrap_or(Value::Unit);
                if let Value::Array(arr_val) = arr {
                    let offset = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
                    let val = args.get(2).map(|v| v.as_i64()).unwrap_or(0);

                    match name {
                        "gorget_bytes_write_u32_be" => {
                            let v = val as u32;
                            if offset + 4 <= arr_val.len() {
                                arr_val.set(offset,     Value::U8(((v >> 24) & 0xFF) as u8));
                                arr_val.set(offset + 1, Value::U8(((v >> 16) & 0xFF) as u8));
                                arr_val.set(offset + 2, Value::U8(((v >> 8)  & 0xFF) as u8));
                                arr_val.set(offset + 3, Value::U8((v         & 0xFF) as u8));
                            }
                            self.heap.insert(addr, Value::Array(arr_val));
                            return Ok(Some(Value::Unit));
                        }
                        "gorget_bytes_write_u16_be" => {
                            let v = val as u16;
                            if offset + 2 <= arr_val.len() {
                                arr_val.set(offset,     Value::U8(((v >> 8) & 0xFF) as u8));
                                arr_val.set(offset + 1, Value::U8((v        & 0xFF) as u8));
                            }
                            self.heap.insert(addr, Value::Array(arr_val));
                            return Ok(Some(Value::Unit));
                        }
                        "gorget_bytes_write_u32_le" => {
                            let v = val as u32;
                            if offset + 4 <= arr_val.len() {
                                arr_val.set(offset,     Value::U8((v         & 0xFF) as u8));
                                arr_val.set(offset + 1, Value::U8(((v >> 8)  & 0xFF) as u8));
                                arr_val.set(offset + 2, Value::U8(((v >> 16) & 0xFF) as u8));
                                arr_val.set(offset + 3, Value::U8(((v >> 24) & 0xFF) as u8));
                            }
                            self.heap.insert(addr, Value::Array(arr_val));
                            return Ok(Some(Value::Unit));
                        }
                        "gorget_bytes_write_u16_le" => {
                            let v = val as u16;
                            if offset + 2 <= arr_val.len() {
                                arr_val.set(offset,     Value::U8((v        & 0xFF) as u8));
                                arr_val.set(offset + 1, Value::U8(((v >> 8) & 0xFF) as u8));
                            }
                            self.heap.insert(addr, Value::Array(arr_val));
                            return Ok(Some(Value::Unit));
                        }
                        "gorget_bytes_read_u32_be" => {
                            let elems = arr_val.to_vec();
                            if offset + 3 < elems.len() {
                                let b0 = elems[offset].as_i64() as u8 as u32;
                                let b1 = elems[offset+1].as_i64() as u8 as u32;
                                let b2 = elems[offset+2].as_i64() as u8 as u32;
                                let b3 = elems[offset+3].as_i64() as u8 as u32;
                                return Ok(Some(Value::I64(((b0 << 24) | (b1 << 16) | (b2 << 8) | b3) as i64)));
                            }
                            return Ok(Some(Value::I64(0)));
                        }
                        "gorget_bytes_read_u16_be" => {
                            let elems = arr_val.to_vec();
                            if offset + 1 < elems.len() {
                                let b0 = elems[offset].as_i64() as u8 as u16;
                                let b1 = elems[offset+1].as_i64() as u8 as u16;
                                return Ok(Some(Value::I64(((b0 << 8) | b1) as i64)));
                            }
                            return Ok(Some(Value::I64(0)));
                        }
                        "gorget_bytes_read_u32_le" => {
                            let elems = arr_val.to_vec();
                            if offset + 3 < elems.len() {
                                let b0 = elems[offset].as_i64() as u8 as u32;
                                let b1 = elems[offset+1].as_i64() as u8 as u32;
                                let b2 = elems[offset+2].as_i64() as u8 as u32;
                                let b3 = elems[offset+3].as_i64() as u8 as u32;
                                return Ok(Some(Value::I64((b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)) as i64)));
                            }
                            return Ok(Some(Value::I64(0)));
                        }
                        "gorget_bytes_read_u16_le" => {
                            let elems = arr_val.to_vec();
                            if offset + 1 < elems.len() {
                                let b0 = elems[offset].as_i64() as u8 as u16;
                                let b1 = elems[offset+1].as_i64() as u8 as u16;
                                return Ok(Some(Value::I64((b0 | (b1 << 8)) as i64)));
                            }
                            return Ok(Some(Value::I64(0)));
                        }
                        _ => {}
                    }
                }
            }
        }

        // ── GorgetFile handle methods ─────────────────────────────────────────
        // The first arg is a MutPtr/Ptr to a GorgetFile struct.
        // GorgetFile struct: fields[0] = path (Str), fields[1] = mode (Str)
        if name == "gorget_file_write" || name == "gorget_file_write_handle"
            || name == "File__write" || name == "gorget_file_read_all"
            || name == "File__read_all" || name == "gorget_file_close" || name == "File__close"
        {
            let file_val = match args.first() {
                Some(Value::Ptr(a)) | Some(Value::MutPtr(a)) | Some(Value::Ref(a)) => {
                    self.heap.get(a).cloned()
                }
                Some(other) => Some(other.clone()),
                None => None,
            };
            if let Some(Value::Struct { ref fields, .. }) = file_val {
                let path = fields.first()
                    .map(|v| match v {
                        Value::Str(s) => s.as_str().to_string(),
                        Value::String(s) => s.as_str().to_string(),
                        _ => std::string::String::new(),
                    })
                    .unwrap_or_default();

                match name {
                    "gorget_file_write" | "gorget_file_write_handle" | "File__write" => {
                        use std::io::Write as _;
                        let content = args.get(1)
                            .map(|v| match v {
                                Value::Str(s) => s.as_str().to_string(),
                                Value::String(s) => s.as_str().to_string(),
                                _ => std::string::String::new(),
                            })
                            .unwrap_or_default();
                        if let Ok(mut f) = std::fs::OpenOptions::new()
                            .append(true).create(true).open(&path) {
                            let _ = f.write_all(content.as_bytes());
                        }
                        return Ok(Some(Value::Unit));
                    }
                    "gorget_file_read_all" | "File__read_all" => {
                        
                        let content = std::fs::read_to_string(&path).unwrap_or_default();
                        return Ok(Some(Value::Enum {
                            type_name: "Result__GorgetString__Str".to_string(),
                            tag: 0, variant: "Ok".to_string(),
                            fields: vec![Value::String(super::value::SimString::from_string(content))],
                        }));
                    }
                    "gorget_file_close" | "File__close" => return Ok(Some(Value::Unit)),
                    _ => {}
                }
            }
        }

        Ok(None)
    }

    /// Extract a SimArray from a value, following Ptr/MutPtr/Ref through the heap.
    fn get_array_from_value(&self, val: &Value) -> Option<super::value::SimArray> {
        match val {
            Value::Array(arr) => Some(arr.clone()),
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                self.heap.get(addr).and_then(|v| self.get_array_from_value(v))
            }
            _ => None,
        }
    }

    /// Extract a SimDict from a value, following Ptr/MutPtr/Ref through the heap.
    /// Evaluate InlineC patterns used for dict/set iteration.
    /// Returns Ok(()) if recognized (or silently ignored), Err if a panic pattern is hit.
    fn eval_inline_c(&mut self, locals: &mut Vec<Value>, code: &str) -> SimResult<()> {
        let code = code.trim();

        // Pattern 5: gorget_panic(gorget_format("assertion failed: left OP right\n  left:  FMT\n  right: FMT", arg1, arg2));
        // args may be: `(long long)(_N)`, `(long long)(42LL)`, `(double)(_N)`, `(_N) ? "true" : "false"`
        // We parse the format string and arguments to produce the final error message.
        if code.starts_with("gorget_panic(") {
            let msg = if let Some(after_prefix) = code.strip_prefix("gorget_panic(gorget_format(\"") {
                // Find the closing quote (not escaped) of the format string
                let mut fmt_end = 0;
                let bytes = after_prefix.as_bytes();
                while fmt_end < bytes.len() {
                    if bytes[fmt_end] == b'"' { break; }
                    if bytes[fmt_end] == b'\\' { fmt_end += 1; } // skip escaped char
                    fmt_end += 1;
                }
                let fmt_str = after_prefix[..fmt_end].replace("\\n", "\n").replace("\\\"", "\"");
                // Everything after the closing quote + optional ", " are arguments.
                // The tail of `after_prefix[fmt_end..]` is `", arg1, arg2));` —
                // strip the gorget_panic(); and gorget_format() closing parens.
                let rest_raw = after_prefix[fmt_end..].trim_start_matches('"').trim_start_matches(',').trim();
                // Strip trailing `);` sequences (outer gorget_panic + gorget_format closers)
                // Use strip_suffix to remove exactly one `)` from each wrapper
                let _rest = rest_raw
                    .strip_suffix(';').unwrap_or(rest_raw)  // remove trailing `;`
                    .strip_suffix(')').unwrap_or(rest_raw.strip_suffix(';').unwrap_or(rest_raw)) // outer gorget_panic )
                    .strip_suffix(')').unwrap_or(rest_raw.strip_suffix(';').unwrap_or(rest_raw).strip_suffix(')').unwrap_or(rest_raw.strip_suffix(';').unwrap_or(rest_raw))); // gorget_format )
                // Simpler: just strip the tail using find
                let rest = {
                    // The pattern is: `arg1, arg2));` where we want just `arg1, arg2`
                    // Count from the end: strip `;`, then strip matching `)` for gorget_panic and gorget_format
                    let s = rest_raw.strip_suffix(';').unwrap_or(rest_raw);
                    let s = s.strip_suffix(')').unwrap_or(s); // gorget_panic close
                    let s = s.strip_suffix(')').unwrap_or(s); // gorget_format close
                    s
                };

                // Parse each argument from the InlineC expression.
                // Split carefully by top-level commas (not inside parentheses or strings).
                let arg_exprs = split_top_level_commas(rest);
                let arg_vals: Vec<Value> = arg_exprs.iter()
                    .map(|expr| self.eval_inline_c_arg(locals, expr.trim()))
                    .collect();

                // Use do_printf to format the message
                runtime::do_printf(&fmt_str, &arg_vals).unwrap_or(fmt_str)
            } else {
                "assertion failed".to_string()
            };
            return Err(SimError::Panic(msg));
        }

        // All other patterns assign to _X: split on first " = "
        let (lhs, rhs) = match code.split_once(" = ") {
            Some(pair) => pair,
            None => return Ok(()), // unrecognized, skip
        };
        let dst = match parse_inline_local(lhs.trim()) {
            Some(n) => n,
            None => return Ok(()),
        };
        let rhs = rhs.trim().trim_end_matches(';');

        // Helper: resolve a dict/set value (may be behind a pointer)
        // Pattern 1: `(int64_t)_Y.cap`
        if let Some(rest) = rhs.strip_prefix("(int64_t)") {
            if let Some(src_str) = rest.strip_suffix(".cap") {
                if let Some(src) = parse_inline_local(src_str.trim()) {
                    let dict_val = locals.get(src).cloned().unwrap_or(Value::Unit);
                    let cap = self.dict_or_set_cap(&dict_val);
                    local_set(locals, dst, Value::I64(cap as i64));
                    return Ok(());
                }
            }
            // Pattern 2: `(int64_t)_Y.states[(size_t)_Z]`
            if let Some((dict_str, idx_str)) = rest.split_once(".states[(size_t)") {
                let idx_str = idx_str.trim_end_matches(']');
                if let (Some(src), Some(idx_local)) =
                    (parse_inline_local(dict_str.trim()), parse_inline_local(idx_str.trim()))
                {
                    let dict_val = locals.get(src).cloned().unwrap_or(Value::Unit);
                    let idx = locals.get(idx_local).cloned().unwrap_or(Value::I64(0)).as_i64();
                    let state = self.dict_or_set_state(&dict_val, idx as usize);
                    local_set(locals, dst, Value::I64(state as i64));
                    return Ok(());
                }
            }
        }

        // Pattern 3: `((Type*)_Y.keys)[(size_t)_Z]`
        if rhs.starts_with("((") {
            // Strip the cast: `((Type*)rest`
            if let Some(after_cast) = rhs.strip_prefix("((").and_then(|s| {
                s.find("*)").map(|i| &s[i + 2..])
            }) {
                // after_cast = `_Y.keys)[(size_t)_Z]` or `_Y.values)[(size_t)_Z]`
                if let Some((dict_str, idx_str)) = after_cast.split_once(".keys)[(size_t)") {
                    let idx_str = idx_str.trim_end_matches(']');
                    if let (Some(src), Some(idx_local)) =
                        (parse_inline_local(dict_str.trim()), parse_inline_local(idx_str.trim()))
                    {
                        let dict_val = locals.get(src).cloned().unwrap_or(Value::Unit);
                        let idx = locals.get(idx_local).cloned().unwrap_or(Value::I64(0)).as_i64();
                        let key = self.dict_or_set_key_at(&dict_val, idx as usize);
                        local_set(locals, dst, key);
                        return Ok(());
                    }
                }
                // Pattern 4: `((Type*)_Y.values)[(size_t)_Z]`
                if let Some((dict_str, idx_str)) = after_cast.split_once(".values)[(size_t)") {
                    let idx_str = idx_str.trim_end_matches(']');
                    if let (Some(src), Some(idx_local)) =
                        (parse_inline_local(dict_str.trim()), parse_inline_local(idx_str.trim()))
                    {
                        let dict_val = locals.get(src).cloned().unwrap_or(Value::Unit);
                        let idx = locals.get(idx_local).cloned().unwrap_or(Value::I64(0)).as_i64();
                        let val = self.dict_or_set_val_at(&dict_val, idx as usize);
                        local_set(locals, dst, val);
                        return Ok(());
                    }
                }
            }
        }

        Ok(()) // unrecognized pattern: skip silently
    }

    /// Number of active slots to iterate over (exposed as "cap" for iteration).
    fn dict_or_set_cap(&self, val: &Value) -> usize {
        match val {
            Value::Dict(d) => d.len(),
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                self.heap.get(addr).map(|v| self.dict_or_set_cap(v)).unwrap_or(0)
            }
            _ => 0,
        }
    }

    /// State at slot idx: 1 if the slot exists (for our sim, all slots are active).
    fn dict_or_set_state(&self, val: &Value, idx: usize) -> i64 {
        let cap = self.dict_or_set_cap(val);
        if idx < cap { 1 } else { 0 }
    }

    /// Key at insertion-order slot idx.
    fn dict_or_set_key_at(&self, val: &Value, idx: usize) -> Value {
        match val {
            Value::Dict(d) => d.key_at(idx).unwrap_or(Value::Unit),
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                self.heap.get(addr).map(|v| self.dict_or_set_key_at(v, idx)).unwrap_or(Value::Unit)
            }
            _ => Value::Unit,
        }
    }

    /// Value at insertion-order slot idx.
    fn dict_or_set_val_at(&self, val: &Value, idx: usize) -> Value {
        match val {
            Value::Dict(d) => d.val_at(idx).unwrap_or(Value::Unit),
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                self.heap.get(addr).map(|v| self.dict_or_set_val_at(v, idx)).unwrap_or(Value::Unit)
            }
            _ => Value::Unit,
        }
    }

    fn get_dict_from_value(&self, val: &Value) -> Option<super::value::SimDict> {
        match val {
            Value::Dict(d) => Some(d.clone()),
            Value::Ptr(addr) | Value::MutPtr(addr) | Value::Ref(addr) => {
                self.heap.get(addr).and_then(|v| self.get_dict_from_value(v))
            }
            _ => None,
        }
    }

    /// Evaluate a binary operation.
    fn eval_binop(&self, op: BinOp, type_id: TypeId, l: &Value, r: &Value) -> SimResult<Value> {
        // String concatenation
        if matches!(op, BinOp::Add) && l.is_string() && r.is_string() {
            let a = l.to_sim_str();
            let b = r.to_sim_str();
            return Ok(Value::String(SimString::from_string(format!("{}{}", a.as_str(), b.as_str()))));
        }

        // Array/Vector concatenation: a + b → clone(a) then extend with b
        if matches!(op, BinOp::Add) {
            if let (Value::Array(la), Value::Array(rb)) = (l, r) {
                let result = la.clone_deep();
                for i in 0..rb.len() {
                    if let Some(v) = rb.get(i) { result.push(v); }
                }
                return Ok(Value::Array(result));
            }
        }

        let is_float = type_id == F64_TYPE || type_id == F32_TYPE || l.is_float() || r.is_float();

        if is_float {
            let lf = l.as_f64();
            let rf = r.as_f64();
            let result_f = match op {
                BinOp::Add => lf + rf,
                BinOp::Sub => lf - rf,
                BinOp::Mul => lf * rf,
                BinOp::Div => {
                    if rf == 0.0 { return Err(SimError::DivisionByZero); }
                    lf / rf
                }
                BinOp::Rem => lf % rf,
                BinOp::Mod => {
                    let r = lf % rf;
                    if r != 0.0 && ((r < 0.0) != (rf < 0.0)) { r + rf } else { r }
                }
                BinOp::Pow => lf.powf(rf),
                _ => return Err(SimError::TypeMismatch {
                    expected: "integer for bitwise op".into(),
                    got: "float".into(),
                }),
            };
            return Ok(if type_id == F32_TYPE { Value::F32(result_f as f32) } else { Value::F64(result_f) });
        }

        // Integer operations
        let is_unsigned = matches!(type_id, t if t == U8_TYPE || t == U16_TYPE || t == U32_TYPE || t == U64_TYPE);

        if is_unsigned {
            let lu = l.as_u64();
            let ru = r.as_u64();
            let result = match op {
                BinOp::Add => lu.checked_add(ru).ok_or(SimError::Overflow)?,
                BinOp::Sub => lu.checked_sub(ru).ok_or(SimError::Overflow)?,
                BinOp::Mul => lu.checked_mul(ru).ok_or(SimError::Overflow)?,
                BinOp::Div => {
                    if ru == 0 { return Err(SimError::DivisionByZero); }
                    lu / ru
                }
                BinOp::Rem => {
                    if ru == 0 { return Err(SimError::DivisionByZero); }
                    lu % ru
                }
                BinOp::Mod => {
                    if ru == 0 { return Err(SimError::DivisionByZero); }
                    lu % ru // unsigned: modulo == remainder
                }
                BinOp::Pow => lu.pow(ru as u32),
                BinOp::BitAnd => lu & ru,
                BinOp::BitOr => lu | ru,
                BinOp::BitXor => lu ^ ru,
                BinOp::Shl => lu.wrapping_shl(ru as u32),
                BinOp::Shr => lu >> (ru as u32),
                BinOp::AddWrap => lu.wrapping_add(ru),
                BinOp::SubWrap => lu.wrapping_sub(ru),
                BinOp::MulWrap => lu.wrapping_mul(ru),
            };
            return Ok(match type_id {
                t if t == U8_TYPE => Value::U8(result as u8),
                t if t == U16_TYPE => Value::U16(result as u16),
                t if t == U32_TYPE => Value::U32(result as u32),
                _ => Value::U64(result),
            });
        }

        // Signed integer
        let li = l.as_i64();
        let ri = r.as_i64();
        let result = match op {
            BinOp::Add => li.checked_add(ri).ok_or(SimError::Overflow)?,
            BinOp::Sub => li.checked_sub(ri).ok_or(SimError::Overflow)?,
            BinOp::Mul => li.checked_mul(ri).ok_or(SimError::Overflow)?,
            BinOp::Div => {
                if ri == 0 { return Err(SimError::DivisionByZero); }
                li / ri
            }
            BinOp::Rem => {
                if ri == 0 { return Err(SimError::DivisionByZero); }
                li % ri
            }
            BinOp::Mod => {
                if ri == 0 { return Err(SimError::DivisionByZero); }
                // `li % ri` panics in Rust debug on `i64::MIN % -1` (overflow).
                // The Euclidean result of `x mod ±1` is always 0, so short it
                // (and the bottom-of-fn match wraps `0` per `type_id`).
                if ri == -1 { 0 } else {
                    let r = li % ri;
                    if r != 0 && ((r ^ ri) < 0) { r + ri } else { r }
                }
            }
            BinOp::Pow => {
                if ri >= 0 { li.pow(ri as u32) } else { 0 }
            }
            BinOp::BitAnd => li & ri,
            BinOp::BitOr => li | ri,
            BinOp::BitXor => li ^ ri,
            BinOp::Shl => li.wrapping_shl(ri as u32),
            BinOp::Shr => li >> (ri as u32),
            BinOp::AddWrap => li.wrapping_add(ri),
            BinOp::SubWrap => li.wrapping_sub(ri),
            BinOp::MulWrap => li.wrapping_mul(ri),
        };
        Ok(match type_id {
            t if t == I8_TYPE => Value::I8(result as i8),
            t if t == I16_TYPE => Value::I16(result as i16),
            t if t == I32_TYPE => Value::I32(result as i32),
            t if t == BOOL_TYPE => Value::Bool(result != 0),
            _ => Value::I64(result),
        })
    }

    /// Evaluate a unary operation.
    fn eval_unop(&self, op: UnOp, type_id: TypeId, val: &Value) -> SimResult<Value> {
        match op {
            UnOp::Neg => {
                if val.is_float() {
                    let f = val.as_f64();
                    Ok(if type_id == F32_TYPE { Value::F32(-f as f32) } else { Value::F64(-f) })
                } else {
                    let n = val.as_i64();
                    Ok(match type_id {
                        t if t == I8_TYPE => Value::I8((-n) as i8),
                        t if t == I16_TYPE => Value::I16((-n) as i16),
                        t if t == I32_TYPE => Value::I32((-n) as i32),
                        _ => Value::I64(-n),
                    })
                }
            }
            UnOp::Not => {
                Ok(Value::Bool(!val.as_bool()))
            }
            UnOp::BitNot => {
                let n = val.as_u64();
                Ok(match type_id {
                    t if t == U8_TYPE => Value::U8((!n) as u8),
                    t if t == U16_TYPE => Value::U16((!n) as u16),
                    t if t == U32_TYPE => Value::U32((!n) as u32),
                    t if t == U64_TYPE => Value::U64(!n),
                    t if t == I8_TYPE => Value::I8((!val.as_i64()) as i8),
                    t if t == I16_TYPE => Value::I16((!val.as_i64()) as i16),
                    t if t == I32_TYPE => Value::I32((!val.as_i64()) as i32),
                    _ => Value::I64(!val.as_i64()),
                })
            }
        }
    }

    /// Evaluate a comparison operation.
    fn eval_cmp(&self, op: CmpOp, _type_id: TypeId, l: &Value, r: &Value) -> SimResult<Value> {
        // String comparison
        if l.is_string() && r.is_string() {
            let a = l.to_sim_str();
            let b = r.to_sim_str();
            let result = match op {
                CmpOp::Eq => a.as_str() == b.as_str(),
                CmpOp::Ne => a.as_str() != b.as_str(),
                CmpOp::Lt => a.as_str() < b.as_str(),
                CmpOp::Le => a.as_str() <= b.as_str(),
                CmpOp::Gt => a.as_str() > b.as_str(),
                CmpOp::Ge => a.as_str() >= b.as_str(),
            };
            return Ok(Value::Bool(result));
        }

        // Float comparison
        if l.is_float() || r.is_float() {
            let lf = l.as_f64();
            let rf = r.as_f64();
            let result = match op {
                CmpOp::Eq => lf == rf,
                CmpOp::Ne => lf != rf,
                CmpOp::Lt => lf < rf,
                CmpOp::Le => lf <= rf,
                CmpOp::Gt => lf > rf,
                CmpOp::Ge => lf >= rf,
            };
            return Ok(Value::Bool(result));
        }

        // Integer/pointer comparison
        let li = l.as_i64();
        let ri = r.as_i64();
        let result = match op {
            CmpOp::Eq => li == ri,
            CmpOp::Ne => li != ri,
            CmpOp::Lt => li < ri,
            CmpOp::Le => li <= ri,
            CmpOp::Gt => li > ri,
            CmpOp::Ge => li >= ri,
        };
        Ok(Value::Bool(result))
    }

    /// Cast a value to a target type.
    fn eval_cast(&self, target_type: TypeId, val: Value) -> SimResult<Value> {
        // String-related casts
        if let Value::Str(s) = &val {
            // Str → GorgetString (owned copy)
            if let Some(GirType::Named(name)) = self.module.type_registry.get(target_type) {
                if name == "GorgetString" {
                    return Ok(Value::String(SimString::from_string(s.as_str().to_string())));
                }
            }
        }

        if target_type == BOOL_TYPE {
            return Ok(Value::Bool(val.as_bool()));
        }
        if target_type == I8_TYPE { return Ok(Value::I8(val.as_i64() as i8)); }
        if target_type == I16_TYPE { return Ok(Value::I16(val.as_i64() as i16)); }
        if target_type == I32_TYPE { return Ok(Value::I32(val.as_i64() as i32)); }
        if target_type == I64_TYPE { return Ok(Value::I64(val.as_i64())); }
        if target_type == U8_TYPE { return Ok(Value::U8(val.as_i64() as u8)); }
        if target_type == U16_TYPE { return Ok(Value::U16(val.as_i64() as u16)); }
        if target_type == U32_TYPE { return Ok(Value::U32(val.as_i64() as u32)); }
        if target_type == U64_TYPE { return Ok(Value::U64(val.as_u64())); }
        if target_type == F32_TYPE { return Ok(Value::F32(val.as_f64() as f32)); }
        if target_type == F64_TYPE { return Ok(Value::F64(val.as_f64())); }
        if target_type == UNIT_TYPE { return Ok(Value::Unit); }

        // Named type cast — for aliases and newtypes, just pass through
        if let Some(gir_type) = self.module.type_registry.get(target_type) {
            match gir_type {
                GirType::Named(name) => {
                    let name = name.clone();
                    if let Some(def) = self.module.type_registry.get_type_def(&name) {
                        match &def.kind {
                            TypeDefKind::Alias(inner) => {
                                return self.eval_cast(*inner, val);
                            }
                            _ => {}
                        }
                    }
                    // Wrap in struct if casting to a named type (newtype pattern)
                    return Ok(val);
                }
                GirType::Ptr(_) | GirType::MutPtr(_) => {
                    // Pointer cast: just pass through
                    return Ok(val);
                }
                _ => {}
            }
        }

        // Fallback: return the value unchanged
        Ok(val)
    }

    /// Bit-cast a value (reinterpret bits without conversion).
    fn eval_bitcast(&self, target_type: TypeId, val: Value) -> SimResult<Value> {
        // For float ↔ integer bitcasts
        if target_type == I64_TYPE || target_type == U64_TYPE {
            if let Value::F64(f) = val {
                let bits = f.to_bits();
                return Ok(if target_type == I64_TYPE { Value::I64(bits as i64) } else { Value::U64(bits) });
            }
        }
        if target_type == F64_TYPE {
            if let Value::I64(n) = val {
                return Ok(Value::F64(f64::from_bits(n as u64)));
            }
            if let Value::U64(n) = val {
                return Ok(Value::F64(f64::from_bits(n)));
            }
        }
        if target_type == I32_TYPE || target_type == U32_TYPE {
            if let Value::F32(f) = val {
                let bits = f.to_bits();
                return Ok(if target_type == I32_TYPE { Value::I32(bits as i32) } else { Value::U32(bits) });
            }
        }
        if target_type == F32_TYPE {
            if let Value::I32(n) = val {
                return Ok(Value::F32(f32::from_bits(n as u32)));
            }
        }

        // Default: interpret as the target type via integer cast
        self.eval_cast(target_type, val)
    }

    /// Evaluate an InlineC argument expression to a Value.
    /// Handles patterns generated by assert_printf_info:
    ///   `(long long)(_N)` → I64 from local N
    ///   `(long long)(42LL)` → I64(42)
    ///   `(double)(_N)` → F64 from local N
    ///   `(_N) ? "true" : "false"` → bool string from local N
    ///   `_N` → direct local reference
    fn eval_inline_c_arg(&self, locals: &[Value], expr: &str) -> Value {
        let e = expr.trim();
        // `(long long)(...)` or `(double)(...)`
        if let Some(inner) = e.strip_prefix("(long long)(").and_then(|s| s.strip_suffix(')')) {
            if let Some(n) = parse_inline_local(inner) {
                return locals.get(n).cloned().unwrap_or(Value::Unit);
            }
            // Literal like `42LL`, `-3LL`, `1`
            let lit = inner.trim_end_matches("LL").trim_end_matches("ll");
            if let Ok(n) = lit.parse::<i64>() { return Value::I64(n); }
        }
        if let Some(inner) = e.strip_prefix("(double)(").and_then(|s| s.strip_suffix(')')) {
            if let Some(n) = parse_inline_local(inner) {
                let v = locals.get(n).cloned().unwrap_or(Value::Unit);
                return Value::F64(v.as_f64());
            }
            if let Ok(f) = inner.parse::<f64>() { return Value::F64(f); }
        }
        // `(_N) ? "true" : "false"` — bool
        if let Some(rest) = e.strip_prefix('(').and_then(|s| {
            let end = s.find(')')?;
            Some((&s[..end], &s[end + 1..]))
        }) {
            let (inner, after) = rest;
            if after.trim().starts_with("? \"true\" : \"false\"") {
                if let Some(n) = parse_inline_local(inner) {
                    let v = locals.get(n).cloned().unwrap_or(Value::Unit);
                    let b = match &v {
                        Value::Bool(b) => *b,
                        _ => v.as_i64() != 0,
                    };
                    return Value::Str(super::value::SimStr::from_str(if b { "true" } else { "false" }));
                }
                if inner.trim() == "1" { return Value::Str(super::value::SimStr::from_str("true")); }
                if inner.trim() == "0" { return Value::Str(super::value::SimStr::from_str("false")); }
            }
        }
        // Plain local reference `_N`
        if let Some(n) = parse_inline_local(e) {
            return locals.get(n).cloned().unwrap_or(Value::Unit);
        }
        // Literal integer
        if let Ok(n) = e.trim_end_matches("LL").trim_end_matches("ll").parse::<i64>() {
            return Value::I64(n);
        }
        Value::Unit
    }

    // ──────────── Regex dispatch ────────────────────────────────────────────
    /// Build a Regex struct value (stored on heap, returned as MutPtr).
    fn make_regex_value(&mut self, rx: ::regex::Regex, pattern: &str) -> Value {
        let id = self.regex_next_id;
        self.regex_next_id += 1;
        self.regex_map.insert(id, rx);
        let s = Value::Struct {
            type_name: "Regex".to_string(),
            fields: vec![Value::I64(id as i64), Value::Str(SimStr::from_str(pattern))],
        };
        let addr = self.heap_alloc(s);
        Value::MutPtr(addr)
    }

    /// Wrap a struct in a Result[T, str] Ok variant.
    fn make_ok_struct(val: Value) -> Value {
        Value::Enum {
            type_name: "Result".to_string(),
            tag: 0,
            variant: "Ok".to_string(),
            fields: vec![val],
        }
    }

    /// Wrap a str message in a Result[T, str] Error variant.
    fn make_err_str(msg: &str) -> Value {
        Value::Enum {
            type_name: "Result".to_string(),
            tag: 1,
            variant: "Error".to_string(),
            fields: vec![Value::Str(SimStr::from_str(msg))],
        }
    }

    /// Extract the compiled regex ID from a Regex struct pointer.
    fn regex_id_from_arg(&self, arg: &Value) -> Option<u64> {
        let addr = match arg {
            Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => *a,
            _ => return None,
        };
        let val = self.heap.get(&addr)?;
        let struct_val = match val {
            Value::Struct { .. } => val,
            Value::Ptr(b) | Value::MutPtr(b) | Value::Ref(b) => self.heap.get(b)?,
            _ => return None,
        };
        if let Value::Struct { fields, .. } = struct_val {
            if let Some(Value::I64(id)) = fields.get(0) {
                return Some(*id as u64);
            }
        }
        None
    }

    /// Extract the pattern string from a Regex struct pointer.
    fn regex_pattern_from_arg(&self, arg: &Value) -> String {
        let addr = match arg {
            Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => *a,
            _ => return String::new(),
        };
        let val = match self.heap.get(&addr) {
            Some(v) => v,
            None => return String::new(),
        };
        let struct_val = match val {
            Value::Struct { .. } => val,
            Value::Ptr(b) | Value::MutPtr(b) | Value::Ref(b) => match self.heap.get(b) {
                Some(v) => v,
                None => return String::new(),
            },
            _ => return String::new(),
        };
        if let Value::Struct { fields, .. } = struct_val {
            if let Some(Value::Str(s)) = fields.get(1) {
                return s.as_str().to_string();
            }
        }
        String::new()
    }

    /// Extract capture data from regex Captures + Regex into owned types.
    /// Returns (text, start, end, groups, named_keys, named_vals).
    fn extract_caps(
        rx: &::regex::Regex,
        caps: &::regex::Captures,
    ) -> (String, i64, i64, Vec<Option<String>>, Vec<String>, Vec<Option<String>>) {
        let full = caps.get(0).unwrap();
        let text = full.as_str().to_string();
        let start = full.start() as i64;
        let end = full.end() as i64;
        let num_groups = caps.len().saturating_sub(1);
        let mut groups = Vec::new();
        for i in 0..num_groups {
            groups.push(caps.get(i + 1).map(|g: ::regex::Match| g.as_str().to_string()));
        }
        let mut named_keys = Vec::new();
        let mut named_vals = Vec::new();
        for (idx, opt_name) in rx.capture_names().enumerate() {
            if let Some(name) = opt_name {
                named_keys.push(name.to_string());
                named_vals.push(caps.get(idx).map(|g: ::regex::Match| g.as_str().to_string()));
            }
        }
        (text, start, end, groups, named_keys, named_vals)
    }

    /// Build a Match Value from extracted match data.
    fn build_match_value(
        text: &str, start: i64, end: i64,
        groups: &[Option<String>],
        named_keys: &[String], named_vals: &[Option<String>],
    ) -> Value {
        use super::value::SimArray;
        let groups_arr = SimArray::new("Option__Str");
        for g in groups {
            let gval = if let Some(s) = g {
                Value::Enum { type_name: "Option".to_string(), tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::Str(SimStr::from_str(s))] }
            } else {
                Value::Enum { type_name: "Option".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }
            };
            groups_arr.push(gval);
        }
        let name_keys_arr = SimArray::new("Str");
        let name_vals_arr = SimArray::new("Option__Str");
        for (k, v) in named_keys.iter().zip(named_vals.iter()) {
            name_keys_arr.push(Value::Str(SimStr::from_str(k)));
            let gval = if let Some(s) = v {
                Value::Enum { type_name: "Option".to_string(), tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::Str(SimStr::from_str(s))] }
            } else {
                Value::Enum { type_name: "Option".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }
            };
            name_vals_arr.push(gval);
        }
        Value::Struct {
            type_name: "Match".to_string(),
            fields: vec![
                Value::Str(SimStr::from_str(text)),
                Value::I64(start),
                Value::I64(end),
                Value::Array(groups_arr),
                Value::Array(name_keys_arr),
                Value::Array(name_vals_arr),
            ],
        }
    }

    /// Wrap a match value in Option::Some.
    fn make_some(val: Value) -> Value {
        Value::Enum {
            type_name: "Option".to_string(), tag: 0,
            variant: "Some".to_string(), fields: vec![val],
        }
    }

    /// Build Option::None.
    fn make_none() -> Value {
        Value::Enum {
            type_name: "Option".to_string(), tag: 1,
            variant: "None".to_string(), fields: vec![],
        }
    }

    /// Compile a regex, optionally with flags. Returns Ok(Regex) or Err(message).
    fn compile_regex_with_flags(pattern: &str, flags: &str) -> Result<::regex::Regex, String> {
        let mut builder = ::regex::RegexBuilder::new(pattern);
        for ch in flags.chars() {
            match ch {
                'i' => { builder.case_insensitive(true); }
                'm' => { builder.multi_line(true); }
                's' => { builder.dot_matches_new_line(true); }
                'x' => { builder.ignore_whitespace(true); }
                _ => {}
            }
        }
        builder.build().map_err(|e| e.to_string())
    }

    /// Deref a Match struct pointer and get the inner struct.
    fn deref_match_struct<'h>(&'h self, arg: &Value) -> Option<&'h Value> {
        let addr = match arg {
            Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => *a,
            _ => return None,
        };
        let val = self.heap.get(&addr)?;
        match val {
            Value::Struct { type_name, .. } if type_name == "Match" => Some(val),
            Value::Ptr(b) | Value::MutPtr(b) | Value::Ref(b) => {
                let inner = self.heap.get(b)?;
                if matches!(inner, Value::Struct { type_name, .. } if type_name == "Match") {
                    Some(inner)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Dispatch regex-related runtime functions.
    /// Returns Some(value) if handled, None to fall through.
    fn try_regex_dispatch(&mut self, name: &str, args: &[Value]) -> SimResult<Option<Value>> {
        use super::value::{SimArray, SimStr};

        // ── regex_compile / gorget_regex_compile ────────────────────────────
        if name == "regex_compile" || name == "gorget_regex_compile" {
            let pattern = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            match Self::compile_regex_with_flags(&pattern, "") {
                Ok(rx) => {
                    let ptr = self.make_regex_value(rx, &pattern);
                    return Ok(Some(Self::make_ok_struct(ptr)));
                }
                Err(e) => {
                    return Ok(Some(Self::make_err_str(&e)));
                }
            }
        }

        // ── regex_compile_with / gorget_regex_compile_with ──────────────────
        if name == "regex_compile_with" || name == "gorget_regex_compile_with" {
            let pattern = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let flags = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            match Self::compile_regex_with_flags(&pattern, &flags) {
                Ok(rx) => {
                    let ptr = self.make_regex_value(rx, &pattern);
                    return Ok(Some(Self::make_ok_struct(ptr)));
                }
                Err(e) => {
                    return Ok(Some(Self::make_err_str(&e)));
                }
            }
        }

        // ── gorget_regex_escape / regex_escape ──────────────────────────────
        if name == "gorget_regex_escape" || name == "regex_escape" {
            let s = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let escaped = ::regex::escape(&s);
            return Ok(Some(Value::String(SimString::from_str(&escaped))));
        }

        // ── gorget_regex_is_match (extern method, self=Regex ptr, text) ─────
        if name == "gorget_regex_is_match" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    return Ok(Some(Value::Bool(rx.is_match(&text))));
                }
            }
            return Ok(Some(Value::Bool(false)));
        }

        // ── Regex__is_match (decl_method, self=Regex ptr, text) ─────────────
        if name == "Regex__is_match" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    return Ok(Some(Value::Bool(rx.is_match(&text))));
                }
            }
            return Ok(Some(Value::Bool(false)));
        }

        // ── gorget_regex_replace_all (extern method) ─────────────────────────
        if name == "gorget_regex_replace_all" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let repl = args.get(2).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    let result = rx.replace_all(&text, repl.as_str()).to_string();
                    return Ok(Some(Value::String(SimString::from_str(&result))));
                }
            }
            return Ok(Some(Value::String(SimString::from_str(&text))));
        }

        // ── gorget_regex_capture_count (extern method) ───────────────────────
        if name == "gorget_regex_capture_count" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    return Ok(Some(Value::I64(rx.captures_len().saturating_sub(1) as i64)));
                }
            }
            return Ok(Some(Value::I64(0)));
        }

        // ── gorget_regex_pattern_str (extern method) ─────────────────────────
        if name == "gorget_regex_pattern_str" {
            let pattern = self.regex_pattern_from_arg(args.get(0).unwrap_or(&Value::Null));
            return Ok(Some(Value::Str(SimStr::from_str(&pattern))));
        }

        // ── gorget_regex_group_names (extern method) ─────────────────────────
        if name == "gorget_regex_group_names" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let arr = SimArray::new("Str");
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    for name_opt in rx.capture_names() {
                        if let Some(n) = name_opt {
                            arr.push(Value::Str(SimStr::from_str(n)));
                        }
                    }
                }
            }
            return Ok(Some(Value::Array(arr)));
        }

        // ── Match extern accessors ────────────────────────────────────────────
        if name == "gorget_regex_match_text" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    return Ok(Some(fields.get(0).cloned().unwrap_or(Value::Str(SimStr::from_str("")))));
                }
            }
            return Ok(Some(Value::Str(SimStr::from_str(""))));
        }
        if name == "gorget_regex_match_start" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    return Ok(Some(fields.get(1).cloned().unwrap_or(Value::I64(0))));
                }
            }
            return Ok(Some(Value::I64(0)));
        }
        if name == "gorget_regex_match_end" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    return Ok(Some(fields.get(2).cloned().unwrap_or(Value::I64(0))));
                }
            }
            return Ok(Some(Value::I64(0)));
        }
        if name == "gorget_regex_match_group_count" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    if let Some(Value::Array(arr)) = fields.get(3) {
                        return Ok(Some(Value::I64(arr.len() as i64)));
                    }
                }
            }
            return Ok(Some(Value::I64(0)));
        }
        if name == "gorget_regex_match_groups" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    if let Some(Value::Array(arr)) = fields.get(3) {
                        // Return as Vector[str]: unwrap Some values, use "" for None
                        let out = SimArray::new("Str");
                        for item in arr.data.borrow().iter() {
                            match item {
                                Value::Enum { tag: 0, fields: gf, .. } => {
                                    let s = gf.get(0).and_then(|v| match v {
                                        Value::Str(s) => Some(s.as_str().to_string()),
                                        _ => None,
                                    }).unwrap_or_default();
                                    out.push(Value::Str(SimStr::from_str(&s)));
                                }
                                _ => out.push(Value::Str(SimStr::from_str(""))),
                            }
                        }
                        return Ok(Some(Value::Array(out)));
                    }
                }
            }
            return Ok(Some(Value::Array(SimArray::new("Str"))));
        }

        // ── Regex__find (decl_method) ─────────────────────────────────────────
        if name == "Regex__find" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let data = rx_id.and_then(|id| self.regex_map.get(&id))
                .and_then(|rx| rx.captures(&text).map(|caps| Self::extract_caps(rx, &caps)));
            if let Some((t, s, e, groups, nk, nv)) = data {
                let m = Self::build_match_value(&t, s, e, &groups, &nk, &nv);
                let addr = self.heap_alloc(m);
                return Ok(Some(Self::make_some(Value::MutPtr(addr))));
            }
            return Ok(Some(Self::make_none()));
        }

        // ── Regex__find_at (decl_method) ─────────────────────────────────────
        if name == "Regex__find_at" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let pos = args.get(2).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let search_start = if pos < text.len() { pos } else { text.len() };
            let data = rx_id.and_then(|id| self.regex_map.get(&id)).and_then(|rx| {
                rx.captures(&text[search_start..]).map(|caps| Self::extract_caps(rx, &caps))
            });
            if let Some((t, s, e, groups, nk, nv)) = data {
                let m = Self::build_match_value(&t, s, e, &groups, &nk, &nv);
                let addr = self.heap_alloc(m);
                return Ok(Some(Self::make_some(Value::MutPtr(addr))));
            }
            return Ok(Some(Self::make_none()));
        }

        // ── Regex__find_all (decl_method) ─────────────────────────────────────
        if name == "Regex__find_all" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            // Collect all captured data (releasing immutable borrow) before calling heap_alloc
            let all_data: Vec<_> = rx_id.and_then(|id| self.regex_map.get(&id))
                .map(|rx| rx.captures_iter(&text).map(|caps| Self::extract_caps(rx, &caps)).collect())
                .unwrap_or_default();
            let arr = SimArray::new("Match");
            for (t, s, e, groups, nk, nv) in all_data {
                let m = Self::build_match_value(&t, s, e, &groups, &nk, &nv);
                let addr = self.heap_alloc(m);
                arr.push(Value::MutPtr(addr));
            }
            return Ok(Some(Value::Array(arr)));
        }

        // ── Regex__replace (decl_method, replace first) ───────────────────────
        if name == "Regex__replace" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let repl = args.get(2).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    let result = rx.replace(&text, repl.as_str()).to_string();
                    return Ok(Some(Value::String(SimString::from_str(&result))));
                }
            }
            return Ok(Some(Value::String(SimString::from_str(&text))));
        }

        // ── Regex__split (decl_method) ─────────────────────────────────────────
        if name == "Regex__split" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let arr = SimArray::new("Str");
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    for part in rx.split(&text) {
                        arr.push(Value::Str(SimStr::from_str(part)));
                    }
                }
            }
            return Ok(Some(Value::Array(arr)));
        }

        // ── Regex__splitn (decl_method) ────────────────────────────────────────
        if name == "Regex__splitn" {
            let rx_id = self.regex_id_from_arg(args.get(0).unwrap_or(&Value::Null));
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let n = args.get(2).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let arr = SimArray::new("Str");
            if let Some(id) = rx_id {
                if let Some(rx) = self.regex_map.get(&id) {
                    for part in rx.splitn(&text, n) {
                        arr.push(Value::Str(SimStr::from_str(part)));
                    }
                }
            }
            return Ok(Some(Value::Array(arr)));
        }

        // ── Regex__fullmatch (decl_method): match must span entire string ──────
        if name == "Regex__fullmatch" {
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let pattern = self.regex_pattern_from_arg(args.get(0).unwrap_or(&Value::Null));
            // Compile anchored version: ^(?:pattern)$ (purely local, no self.regex_map borrow)
            let data = {
                let anchored = format!("^(?:{})$", pattern);
                ::regex::Regex::new(&anchored).ok().and_then(|rx| {
                    rx.captures(&text).map(|caps| Self::extract_caps(&rx, &caps))
                })
            };
            if let Some((t, s, e, groups, nk, nv)) = data {
                let m = Self::build_match_value(&t, s, e, &groups, &nk, &nv);
                let addr = self.heap_alloc(m);
                return Ok(Some(Self::make_some(Value::MutPtr(addr))));
            }
            return Ok(Some(Self::make_none()));
        }

        // ── Match__group(n) (decl_method) ─────────────────────────────────────
        if name == "Match__group" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            let n = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    if let Some(Value::Array(arr)) = fields.get(3) {
                        if let Some(item) = arr.data.borrow().get(n).cloned() {
                            return Ok(Some(item));
                        }
                    }
                }
            }
            return Ok(Some(Self::make_none()));
        }

        // ── Match__group_by_name(name) (decl_method) ──────────────────────────
        if name == "Match__group_by_name" {
            let self_arg = args.get(0).unwrap_or(&Value::Null);
            let key = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Some(m) = self.deref_match_struct(self_arg) {
                if let Value::Struct { fields, .. } = m {
                    let keys_opt = fields.get(4);
                    let vals_opt = fields.get(5);
                    if let (Some(Value::Array(keys)), Some(Value::Array(vals))) = (keys_opt, vals_opt) {
                        let keys_data = keys.data.borrow();
                        let vals_data = vals.data.borrow();
                        for (k, v) in keys_data.iter().zip(vals_data.iter()) {
                            if k.as_str_lossy() == key {
                                return Ok(Some(v.clone()));
                            }
                        }
                    }
                }
            }
            return Ok(Some(Self::make_none()));
        }

        // ── Convenience free functions ─────────────────────────────────────────

        // regex_is_match(pattern, text) → bool
        if name == "regex_is_match" {
            let pattern = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Ok(rx) = ::regex::Regex::new(&pattern) {
                return Ok(Some(Value::Bool(rx.is_match(&text))));
            }
            return Ok(Some(Value::Bool(false)));
        }

        // regex_find(pattern, text) → Option[Match]
        if name == "regex_find" {
            let pattern = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            // rx is local — no self.regex_map borrow conflict
            let data = ::regex::Regex::new(&pattern).ok().and_then(|rx| {
                rx.captures(&text).map(|caps| Self::extract_caps(&rx, &caps))
            });
            if let Some((t, s, e, groups, nk, nv)) = data {
                let m = Self::build_match_value(&t, s, e, &groups, &nk, &nv);
                let addr = self.heap_alloc(m);
                return Ok(Some(Self::make_some(Value::MutPtr(addr))));
            }
            return Ok(Some(Self::make_none()));
        }

        // regex_replace(pattern, text, repl) → String
        if name == "regex_replace" {
            let pattern = args.get(0).map(|v| v.as_str_lossy()).unwrap_or_default();
            let text = args.get(1).map(|v| v.as_str_lossy()).unwrap_or_default();
            let repl = args.get(2).map(|v| v.as_str_lossy()).unwrap_or_default();
            if let Ok(rx) = ::regex::Regex::new(&pattern) {
                let result = rx.replace(&text, repl.as_str()).to_string();
                return Ok(Some(Value::String(SimString::from_str(&result))));
            }
            return Ok(Some(Value::String(SimString::from_str(&text))));
        }

        Ok(None)
    }
}


/// Return the index of the "consuming" argument for collection-mutating calls.
/// Mirrors the C backend's consuming_arg_idx logic (mod.rs:7297-7301).
/// When a local of droppable type is passed at this index, the C backend emits
/// `memset(&_N, 0, sizeof(Type))` after the call to prevent double-free.
fn consuming_collection_arg_idx(func_name: &str) -> Option<usize> {
    // Push/add: element is arg[1] (after the receiver pointer)
    let is_push = func_name == "gorget_array_push"
        || func_name == "gorget_set_add"
        || (func_name.ends_with("__push")
            && (func_name.starts_with("Vector__") || func_name.starts_with("List__")))
        || (func_name.ends_with("__add")
            && (func_name.starts_with("Set__") || func_name.starts_with("HashSet__")));
    if is_push { return Some(1); }

    // Set/insert/put: element (or value for maps) is arg[2]
    let is_set = func_name == "gorget_array_set"
        || func_name == "gorget_array_insert"
        || func_name == "gorget_map_put"
        || (func_name.ends_with("__set")
            && (func_name.starts_with("Vector__") || func_name.starts_with("List__")))
        || (func_name.ends_with("__insert")
            && (func_name.starts_with("Vector__") || func_name.starts_with("List__")))
        || (func_name.ends_with("__put")
            && (func_name.starts_with("Dict__") || func_name.starts_with("HashMap__")));
    if is_set { return Some(2); }

    None
}

