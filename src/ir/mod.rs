pub mod types;
pub mod instructions;
pub mod builder;
pub mod lowering;
pub mod printer;
pub mod validate;

use instructions::{Instruction, Terminator};
use types::{TypeId, TypeRegistry};

/// A complete GIR module.
#[derive(Debug, Clone)]
pub struct Module {
    pub type_registry: TypeRegistry,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub externs: Vec<ExternDecl>,
}

impl Module {
    /// Create an empty module with pre-allocated primitive types.
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            externs: Vec::new(),
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
