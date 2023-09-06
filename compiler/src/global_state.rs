use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::ast::{
    EnumDefinition, File, FileId, Generic, NewtypeDefinition, PkgImport, Span, StructDefinition,
    TypeAliasDef,
};
use crate::type_::{BoundedType, ModuleId, Symbol, Type};

pub struct GlobalState {
    next_var: i32,
    next_file_id: i32,
    current_module: ModuleId,
    pub files: HashMap<FileId, ModuleId>,
    pub modules: HashMap<ModuleId, Module>,
    pub module_list: ModuleList,
}

impl GlobalState {
    pub fn new() -> Self {
        let std = Module::std();

        let empty_id = ModuleId::empty();
        let empty = Module::new(&empty_id.0, ModuleImport::empty());

        let modules = vec![
            (std.id.clone(), std.clone()),
            (empty_id.clone(), empty.clone()),
        ]
        .into_iter()
        .collect();

        let module_list = ModuleList::parse();

        Self {
            next_var: 0,
            next_file_id: 0,
            current_module: std.id,
            files: Default::default(),
            modules,
            module_list,
        }
    }

    pub fn fresh_var(&mut self) -> i32 {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    pub fn new_module(&mut self, import: &ModuleImport) -> ModuleId {
        let prev = self.current_module.clone();

        let m = Module::new(&import.name, import.clone());

        // add new module
        self.modules.insert(m.id.clone(), m.clone());

        // set as current
        self.set_module(m.id);

        // return previous module so it can be restored
        prev
    }

    pub fn new_file(&mut self) -> FileId {
        let v = self.next_file_id;
        self.next_file_id += 1;

        let id = FileId(v);

        // Assign this file to the current module, so that later on (ie. when looking up errors)
        // the original file can be recovered
        self.files.insert(id.clone(), self.current_module.clone());

        id
    }

    pub fn get_enum(&self, id: &Symbol) -> Option<EnumDefinition> {
        self.get_module(&id.module)?.enums.get(id).cloned()
    }

    pub fn get_struct(&self, id: &Symbol) -> Option<StructDefinition> {
        self.get_module(&id.module)?.structs.get(id).cloned()
    }

    pub fn module(&mut self) -> &mut Module {
        self.modules.get_mut(&self.current_module).unwrap()
    }

    pub fn get_symbol(&self, name: &str, span: &Span) -> Symbol {
        Symbol {
            module: self.current_module.clone(),
            name: name.to_string(),
            span: span.clone(),
        }
    }

    pub fn get_module(&self, module: &ModuleId) -> Option<Module> {
        self.modules.get(module).cloned()
    }

    pub fn set_module(&mut self, module: ModuleId) {
        self.current_module = module;
    }

    // given a module name like net::http, find the corresponding "import"
    // TODO asdf using "import" here is a bit misleading and confusing
    pub fn import_for(&self, name: &str) -> Option<ModuleImport> {
        self.module_list.lookup(name)
    }

    pub fn get_type(&self, sym: &Symbol) -> Option<BoundedType> {
        let m = self.modules.get(&sym.module)?;
        m.types.get(sym).cloned()
    }

    pub fn get_interface(&self, sym: &Symbol) -> Option<Interface> {
        let m = self.modules.get(&sym.module)?;
        m.interfaces.get(sym).cloned()
    }

    pub fn get_methods(&self, sym: &Symbol) -> Option<HashMap<String, BoundedType>> {
        let m = self.modules.get(&sym.module)?;
        m.methods.get(sym).cloned()
    }

    pub fn get_newtype(&self, sym: &Symbol) -> Option<NewtypeDefinition> {
        let m = self.modules.get(&sym.module)?;
        m.newtypes.get(sym).cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<Generic>,
    pub supertraits: Vec<Type>,
    pub methods: HashMap<String, Type>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub id: ModuleId,
    pub import: ModuleImport,
    pub files: HashMap<FileId, File>,
    pub types: HashMap<Symbol, BoundedType>,
    pub values: HashMap<Symbol, BoundedType>,
    pub enums: HashMap<Symbol, EnumDefinition>,
    pub structs: HashMap<Symbol, StructDefinition>,
    pub aliases: HashMap<Symbol, TypeAliasDef>,
    pub newtypes: HashMap<Symbol, NewtypeDefinition>,
    pub methods: HashMap<Symbol, HashMap<String, BoundedType>>, // type => (method => signature)
    pub interfaces: HashMap<Symbol, Interface>,
    pub visibility: HashMap<Symbol, Visibility>,
}

impl Module {
    pub fn new(name: &str, import: ModuleImport) -> Module {
        let id = ModuleId(name.to_string());
        Module {
            id,
            import,
            files: Default::default(),
            types: Default::default(),
            values: Default::default(),
            enums: Default::default(),
            structs: Default::default(),
            aliases: Default::default(),
            newtypes: Default::default(),
            methods: Default::default(),
            interfaces: Default::default(),
            visibility: Default::default(),
        }
    }

    pub fn is_exported(&self, sym: &Symbol) -> bool {
        // Export everything for now :|
        if let Some(vis) = self.visibility.get(sym) {
            return vis == &Visibility::TopLevel;
        }

        false
    }

    pub fn get_file(&self, filename: &str) -> Option<File> {
        self.files
            .values()
            .find_map(|f| if f.name == filename { Some(f) } else { None })
            .cloned()
    }

    fn std() -> Module {
        Module::new(
            "std",
            ModuleImport {
                name: "std".to_string(),
                path: "std".to_string(),
            },
        )
    }

    pub fn user() -> Module {
        Module::new(
            "user",
            ModuleImport {
                name: "user".to_string(),
                path: "***".to_string(),
            },
        )
    }

    pub fn all_imports(&self) -> Vec<PkgImport> {
        self.files.values().flat_map(|f| f.imports()).collect()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    TopLevel,
    Local,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleImport {
    pub path: String, // net/http
    pub name: String, // net::http
                      // pub prefix: Option<String>, // http (only if different from name)
}

impl ModuleImport {
    pub fn prefix(&self) -> String {
        self.name.split(".").last().unwrap().to_string()
    }

    pub fn to_id(&self) -> ModuleId {
        ModuleId::from_str(&self.name)
    }

    pub fn empty() -> ModuleImport {
        // A bit cheesy, this ModuleImport thing needs a bit of thinking
        let empty = ModuleId::empty();
        ModuleImport {
            path: empty.0.to_string(),
            name: empty.0.to_string(),
        }
    }
}

pub struct ModuleList {
    pub entries: Vec<ModuleImport>,
}

impl ModuleList {
    pub fn parse() -> ModuleList {
        let json = include_str!("../../std/modules.json");
        let entries = serde_json::from_str(json).unwrap();
        ModuleList { entries }
    }

    pub fn lookup(&self, name: &str) -> Option<ModuleImport> {
        self.entries.iter().find(|e| e.name == name).cloned()
    }
}

pub enum ModuleExport {
    EnumDef(Symbol),
    StructDef(Symbol),
}
