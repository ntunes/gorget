pub mod types;
pub mod instructions;
pub mod builder;
pub mod lowering;
pub mod printer;
pub mod validate;

use instructions::{Instruction, Terminator};
use types::{TypeId, TypeRegistry};

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

/// A complete GIR module.
#[derive(Debug, Clone)]
pub struct Module {
    pub type_registry: TypeRegistry,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub externs: Vec<ExternDecl>,
    /// Test functions registered for the test runner.
    pub test_fns: Vec<TestFnInfo>,
    /// When true, arithmetic wraps on overflow instead of aborting.
    pub overflow_wrap: bool,
    /// Pre-generated C code for async functions (state structs, poll fns, constructors).
    /// Emitted verbatim by the C backend before any GIR-derived functions.
    pub global_inline_c: Vec<String>,
    /// True if any async function was detected; causes the C backend to emit async runtime.
    pub has_async: bool,
    /// True if any `spawn` expression was found; emits executor runtime.
    pub has_spawn: bool,
    /// True if any `sleep()` call was found; emits sleep runtime.
    pub has_sleep: bool,
    /// Channel element C type names found (e.g., ["int64_t"] for Channel[int]).
    /// Used by the C backend to emit Channel__T wrapper structs and functions.
    pub channel_types: Vec<String>,
    /// Spawned functions: (fn_name, [(param_name, param_type)], return_type).
    /// Used by the C backend to emit __SpawnCtx_fn, __gorget_spawn_fn, __gorget_await_fn.
    pub spawned_fns: Vec<(String, Vec<(String, TypeId)>, TypeId)>,
    /// True when a `suite setup:` block was lowered as `__suite_setup()`.
    pub has_suite_setup: bool,
    /// True when a `suite teardown:` block was lowered as `__suite_teardown()`.
    pub has_suite_teardown: bool,
    /// True when the module was lowered in test mode (gg test).
    /// Forces emission of a test runner main() even when test_fns is empty (all filtered).
    pub is_test_module: bool,
    /// When set, emit trace instrumentation and write events to this file path.
    pub trace_filename: Option<String>,
    /// When true, this module uses `directive hot-reload`.
    pub hot_reload: bool,
    /// Name of the hot-reload State struct (derived from `init()` return type).
    pub hot_reload_state_type: Option<String>,
    /// FNV-1a hash of the State struct's field layout (for change detection).
    pub hot_reload_state_hash: u64,
    /// True when a `reload()` function exists in the module.
    pub hot_reload_has_reload_fn: bool,
}

impl Module {
    /// Create an empty module with pre-allocated primitive types.
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            externs: Vec::new(),
            test_fns: Vec::new(),
            overflow_wrap: false,
            global_inline_c: Vec::new(),
            has_async: false,
            has_spawn: false,
            has_sleep: false,
            channel_types: Vec::new(),
            spawned_fns: Vec::new(),
            has_suite_setup: false,
            has_suite_teardown: false,
            is_test_module: false,
            trace_filename: None,
            hot_reload: false,
            hot_reload_state_type: None,
            hot_reload_state_hash: 0,
            hot_reload_has_reload_fn: false,
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
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            terminator: None,
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
        assert_eq!(module.type_registry.len(), 13); // primitives
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
