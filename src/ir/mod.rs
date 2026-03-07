pub mod types;
pub mod instructions;
pub mod builder;
pub mod lowering;
pub mod printer;
pub mod transforms;
pub mod validate;

use instructions::{Instruction, Terminator};
use types::{TypeId, TypeRegistry};
use crate::span::Span;

/// Compile-time selectable scheduler backend for `spawn`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SchedulerMode {
    /// M:N thread pool + work-stealing (default).
    #[default]
    Pool,
    /// 1:1 OS thread per spawn.
    Thread,
    /// Synchronous on caller thread.
    Inline,
    /// N:1 cooperative event loop.
    Single,
}

/// A single `with X as y` binding in a test, for the C backend to generate setup code.
#[derive(Debug, Clone)]
pub struct TestWithBinding {
    /// Variable name as used in the test body (e.g. `r`).
    pub var_name: String,
    /// GIR function that creates and returns the binding value (no Drop registered).
    pub init_fn_name: String,
    /// GIR type of the binding.
    pub type_id: TypeId,
}

/// Metadata for a single test function, used by the C backend to generate the test runner.
#[derive(Debug, Clone)]
pub struct TestFnInfo {
    /// GIR function name (e.g. `__test_0`).
    pub fn_name: String,
    /// Human-readable test name (e.g. `"addition works"`).
    pub display_name: String,
    /// True when `@should_panic` attribute is present — panic = PASS, no panic = FAIL.
    pub should_panic: bool,
    /// Expected panic message substring (from `@should_panic("msg")`).
    pub expected_panic_msg: Option<String>,
    /// With-bindings for this test (empty when no `with X as y` clause).
    /// The test function takes pointer parameters for each binding (in order).
    pub with_bindings: Vec<TestWithBinding>,
}

/// Backend-specific runtime feature flags and type lists.
///
/// Separated from `Module` to keep the core IR struct focused on
/// types, functions, globals, and externs.  Everything here is
/// populated during lowering and consumed by the C backend / sim.
#[derive(Debug, Clone, Default)]
pub struct RuntimeFeatures {
    // ── Concurrency ────────────────────────────────────────────────
    /// True if any async function was detected; causes the C backend to emit async runtime.
    pub has_async: bool,
    /// True if any `spawn` expression was found; emits executor runtime.
    pub has_spawn: bool,
    /// Scheduler backend for `spawn` (pool, thread, inline, single).
    pub scheduler_mode: SchedulerMode,
    /// Whether any TaskGroup was used (triggers TaskGroup runtime emission).
    pub has_task_group: bool,
    /// Whether the blocking thread pool is needed (auto-offloaded blocking calls or spawn_blocking).
    pub has_blocking_pool: bool,
    /// Whether any std.sync types are used (AtomicInt, AtomicBool, Barrier, RWLock).
    pub has_sync: bool,
    /// Channel element C type names (e.g., ["int64_t"] for Channel[int]).
    pub channel_types: Vec<String>,
    /// Shared[T] inner C type names (e.g., ["int64_t"] for Shared[int]).
    pub shared_types: Vec<String>,
    /// Weak[T] inner C type names (e.g., ["int64_t"] for Weak[int]).
    pub weak_types: Vec<String>,
    /// Mutex[T] inner C type names (e.g., ["int64_t"] for Mutex[int]).
    pub mutex_types: Vec<String>,
    /// RWLock[T] inner C type names (e.g., ["int64_t"] for RWLock[int]).
    pub rwlock_types: Vec<String>,
    /// Spawned functions: (fn_name, [(param_name, param_type)], return_type).
    pub spawned_fns: Vec<(String, Vec<(String, TypeId)>, TypeId)>,

    // ── Threads / processes ────────────────────────────────────────
    /// Whether std.thread is used.
    pub has_thread: bool,
    /// Whether std.process Process type (fork+exec) is used.
    pub has_process: bool,
    /// Thread[T] return C type names (e.g., ["int64_t"] for Thread[int]).
    pub thread_types: Vec<String>,
    /// Thread-spawned functions: (fn_name, return_type).
    pub thread_spawned_fns: Vec<(String, TypeId)>,

    // ── Test runner ────────────────────────────────────────────────
    /// Test functions registered for the test runner.
    pub test_fns: Vec<TestFnInfo>,
    /// True when lowered in test mode (gg test).
    pub is_test_module: bool,
    /// True when a `suite setup:` block was lowered.
    pub has_suite_setup: bool,
    /// True when a `suite teardown:` block was lowered.
    pub has_suite_teardown: bool,

    // ── Codegen hints ──────────────────────────────────────────────
    /// When true, arithmetic wraps on overflow instead of aborting.
    pub overflow_wrap: bool,
    /// When set, emit trace instrumentation and write events to this file path.
    pub trace_filename: Option<String>,

    // ── Hot reload ─────────────────────────────────────────────────
    /// When true, this module uses `directive hot-reload`.
    pub hot_reload: bool,
    /// Name of the hot-reload State struct (derived from `init()` return type).
    pub hot_reload_state_type: Option<String>,
    /// FNV-1a hash of the State struct's field layout (for change detection).
    pub hot_reload_state_hash: u64,
    /// True when a `reload()` function exists in the module.
    pub hot_reload_has_reload_fn: bool,
}

/// A complete GIR module.
#[derive(Debug, Clone)]
pub struct Module {
    pub type_registry: TypeRegistry,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub externs: Vec<ExternDecl>,
    /// Original .gg filename (for backtrace display).
    pub source_filename: Option<String>,
    /// Concatenated source text (for source-line display in errors).
    pub source_code: Option<String>,
    /// Backend-specific runtime feature flags and type lists.
    pub runtime: RuntimeFeatures,
}

impl Module {
    /// Create an empty module with pre-allocated primitive types.
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            externs: Vec::new(),
            source_filename: None,
            source_code: None,
            runtime: RuntimeFeatures::default(),
        }
    }

    /// Look up a function by name.
    pub fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Look up an extern declaration by name.
    pub fn find_extern(&self, name: &str) -> Option<&ExternDecl> {
        self.externs.iter().find(|e| e.name == name)
    }

    /// Check if a function or extern with the given name exists.
    pub fn has_callable(&self, name: &str) -> bool {
        self.find_function(name).is_some() || self.find_extern(name).is_some()
    }
}

/// A GIR function.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    /// `_0` = return place, `_1.._N` = params, rest = user/temps.
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    /// True for test functions (test "...") — enables cleanup stack registration
    /// for droppable locals so they're cleaned up on panic/longjmp.
    pub is_test_fn: bool,
    /// Human-readable Gorget function name for trace output (e.g. "add", "Point.distance").
    /// None for compiler-generated functions (closures, vtable methods, etc.).
    pub display_name: Option<String>,
    /// Byte-span of the function definition in source (for backtrace display).
    pub def_span: Option<Span>,
}

/// A local variable slot.
#[derive(Debug, Clone)]
pub struct Local {
    pub type_id: TypeId,
    pub name_hint: Option<String>,
}

/// A basic block.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    /// Parallel source span for each instruction (None for compiler-generated instructions).
    pub span_map: Vec<Option<Span>>,
    /// Source span of the terminator instruction.
    pub terminator_span: Option<Span>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            terminator: None,
            span_map: Vec::new(),
            terminator_span: None,
        }
    }
}

/// A global constant or variable.
#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub type_id: TypeId,
    pub init: GlobalInit,
}

#[derive(Debug, Clone)]
pub enum GlobalInit {
    Zeroed,
    Struct {
        type_name: String,
        fields: Vec<(String, GlobalInit)>,
    },
    FnRef(String),
    Bytes(Vec<u8>),
    /// C expression to call at runtime (via __attribute__((constructor))).
    /// Used for module-level variables that need heap allocation (e.g. AtomicInt, Barrier).
    RuntimeCall(String),
}

/// An extern function declaration.
#[derive(Debug, Clone)]
pub struct ExternDecl {
    pub name: String,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub is_variadic: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::{I32_TYPE, I64_TYPE, UNIT_TYPE};

    #[test]
    fn empty_module() {
        let module = Module::new();
        assert!(module.functions.is_empty());
        assert!(module.globals.is_empty());
        assert!(module.externs.is_empty());
        assert_eq!(module.type_registry.len(), 12); // primitives
    }

    #[test]
    fn module_with_function() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![Local {
                type_id: I32_TYPE,
                name_hint: None,
            }],
            blocks: vec![BasicBlock::new()],
            is_test_fn: false,
            display_name: None,
            def_span: None,
        });
        assert_eq!(module.functions.len(), 1);
        let f = module.find_function("main").unwrap();
        assert_eq!(f.name, "main");
        assert_eq!(f.return_type, I32_TYPE);
    }

    #[test]
    fn module_with_global() {
        let mut module = Module::new();
        module.globals.push(Global {
            name: "counter".into(),
            type_id: I64_TYPE,
            init: GlobalInit::Zeroed,
        });
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "counter");

        module.externs.push(ExternDecl {
            name: "printf".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            is_variadic: true,
        });
        assert!(module.has_callable("printf"));
        assert!(!module.has_callable("missing"));
    }
}
