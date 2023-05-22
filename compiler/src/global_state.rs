use serde::{Deserialize, Serialize};

use crate::ast::{EnumDefinition, File, PkgInfo, Span, StructDefinition};
use crate::project::Package;
use crate::type_::{Bound, BoundedType, Type, TypeId};
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
struct Scope {
    // used for local types ie. generics
    types: HashMap<String, TypeDef>,
    // bindings in the value universe, ie. x => Int
    values: HashMap<String, ValueDef>,
    // assumptions about a type found in trait bounds, ie. T => comparable. Used for constraint resolution.
    assumptions: HashMap<String, Vec<String>>,
    // bindings that were declared with `let mut`
    mutability: HashMap<String, bool>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            types: Default::default(),
            values: Default::default(),
            assumptions: Default::default(),
            mutability: Default::default(),
        }
    }

    fn add_value(&mut self, name: String, ty: BoundedType, decl: Declaration) {
        self.values.insert(name, ValueDef { ty, decl });
    }

    fn get_value(&self, name: &str) -> Option<ValueDef> {
        self.values.get(name).cloned()
    }

    pub fn add_type(&mut self, name: String, ty: BoundedType, decl: Declaration) {
        self.types.insert(name, TypeDef { ty, decl });
    }

    pub fn get_type(&mut self, name: &str) -> Option<TypeDef> {
        self.types.get(name).cloned()
    }
}

#[derive(Clone)]
pub struct GlobalState {
    next_var: i32,
    next_type_id: i32,
    scopes: VecDeque<Scope>,
    types: HashMap<String, TypeDef>,
    enums: HashMap<String, EnumDefinition>,
    structs: HashMap<String, StructDefinition>,
    cons_aliases: HashMap<String, String>, // Ok => Result::Ok
    methods: HashMap<String, HashMap<String, TypeDef>>, // type => (method => signature)
    modules: HashMap<String, Module>,
    interfaces: HashMap<String, Interface>,
    type_aliases: HashMap<String, TypeDef>,
}

impl GlobalState {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            next_type_id: 0,
            scopes: VecDeque::from([Scope::new()]),
            types: Default::default(),
            enums: Default::default(),
            structs: Default::default(),
            cons_aliases: Default::default(), // Ok -> Result::Ok
            modules: Default::default(),
            methods: Default::default(),
            interfaces: Default::default(),
            type_aliases: Default::default(),
        }
    }

    pub fn begin_scope(&mut self) {
        let new_scope = self.current_scope().clone();
        self.scopes.push_front(new_scope);
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop_front();
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.front_mut().unwrap()
    }

    pub fn add_type(&mut self, name: String, ty: BoundedType, decl: Declaration) {
        if !self.types.contains_key(&name) {
            self.types.insert(name, TypeDef { ty, decl });
        }
    }

    pub fn get_type(&mut self, name: &str) -> Option<BoundedType> {
        self.get_global_type(name)
            .or_else(|| self.current_scope().get_type(name).map(|t| t.ty))
    }

    pub fn get_global_type(&self, name: &str) -> Option<BoundedType> {
        self.get_global_type_declaration(name).map(|t| t.ty)
    }

    pub fn get_global_type_declaration(&self, name: &str) -> Option<TypeDef> {
        self.get_type_alias(name)
            .or_else(|| self.types.get(name).cloned())
    }

    pub fn add_type_alias(&mut self, name: String, ty: BoundedType, decl: Declaration) {
        // this is terrible, just like in add_type which gets called multiple times during
        // inference, we want to ensure that once the right type is being registered, we don't
        // override it with a dummy one. The fix is somewhere in inference which is doing one too
        // many passes over the ast, but for now this check is necessary.
        //
        if let Some(existing) = self.type_aliases.get(&name) {
            if existing.ty.ty != Type::dummy() {
                return;
            }
        }

        self.type_aliases.insert(name, TypeDef { ty, decl });
    }

    pub fn get_type_alias(&self, name: &str) -> Option<TypeDef> {
        self.type_aliases.get(name).cloned()
    }

    pub fn add_value(&mut self, name: String, ty: BoundedType, decl: Declaration) {
        self.current_scope().add_value(name, ty, decl);
    }

    pub fn get_value(&self, name: &str) -> Option<ValueDef> {
        let lookup = self.resolve_name(name);

        self.scopes
            .iter()
            .find_map(|scope| scope.get_value(&lookup))
    }

    pub fn add_enum(&mut self, name: String, def: EnumDefinition) {
        self.enums.insert(name, def);
    }

    pub fn add_struct(&mut self, name: String, def: StructDefinition) {
        self.structs.insert(name, def);
    }

    pub fn add_constructor_alias(&mut self, alias: String, source: String) {
        self.cons_aliases.insert(alias, source);
    }

    pub fn get_enum(&self, name: &str) -> Option<EnumDefinition> {
        self.enums.get(name).cloned()
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDefinition> {
        self.structs.get(name).cloned()
    }

    /// Follows an alias or returns the input unchanged
    pub fn resolve_name(&self, name: &str) -> String {
        self.cons_aliases
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_owned())
    }

    pub fn fresh_var(&mut self) -> i32 {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    pub fn add_builtin_type(&mut self, name: String, ty: BoundedType) {
        self.add_type(name, ty, Declaration::dummy());
    }

    pub fn add_builtin_value(&mut self, name: String, ty: BoundedType) {
        self.add_value(name, ty, Declaration::dummy());
    }

    pub fn dump(&self) {
        self.scopes
            .iter()
            .for_each(|scope| eprintln!("{:#?}", scope))
    }

    pub fn put_generics_in_scope(&mut self, generics: &[String], decl: Declaration) {
        generics.iter().for_each(|g| {
            let ty = Type::Con {
                name: g.to_string(),
                args: vec![],
            }
            .to_bounded();

            // Generics are only visible in the current scope
            self.current_scope()
                .add_type(g.to_string(), ty, decl.clone());
        });
    }

    pub fn add_assumption(&mut self, bound: &Bound) {
        self.current_scope()
            .assumptions
            .entry(bound.generic.to_string())
            .or_default()
            .push(bound.ty.get_name().unwrap());
    }

    pub fn add_interface(&mut self, interface: Interface) {
        self.interfaces.insert(interface.name.clone(), interface);
    }

    pub fn get_all_types(&self) -> HashMap<String, BoundedType> {
        self.types
            .iter()
            .map(|(name, t)| (name.clone(), t.ty.clone()))
            .collect()
    }

    pub fn set_mutability(&mut self, ident: &str, mutable: bool) {
        self.current_scope()
            .mutability
            .insert(ident.to_string(), mutable);
    }

    pub fn get_mutability(&mut self, ident: &str) -> bool {
        self.scopes
            .iter()
            .find_map(|scope| scope.mutability.get(ident))
            .cloned()
            .unwrap_or_default()
    }

    pub fn add_method(
        &mut self,
        base_ty: &str,
        method_name: &str,
        ty: BoundedType,
        decl: Declaration,
    ) {
        let methods = self
            .methods
            .entry(base_ty.to_string())
            .or_insert(HashMap::new());

        methods.insert(method_name.to_string(), TypeDef { ty, decl });
    }

    pub fn get_method(&self, base_ty: &Type, method: &str) -> Option<BoundedType> {
        base_ty.remove_references().get_name().and_then(|ty_name| {
            self.get_type_methods(&ty_name)
                .get(method)
                .cloned()
                .map(|t| t.ty)
        })
    }

    pub fn get_type_methods(&self, ty_name: &str) -> HashMap<String, TypeDef> {
        // If it's an interface, return all methods
        if let Some(interface) = self.interfaces.get(ty_name).cloned() {
            let mut ret = HashMap::new();

            for (name, ty) in interface.methods {
                let decl = Declaration::dummy(); // TODO an interface should have a declaration
                ret.insert(
                    name,
                    TypeDef {
                        ty: ty.add_any_receiver().to_bounded(),
                        decl,
                    },
                );
            }

            return ret;
        }

        // If it's a constraint, return all the methods of all the interfaces implied by that
        // constraint
        if let Some(ifaces) = self.get_assumptions(ty_name) {
            return ifaces
                .into_iter()
                .flat_map(|i| self.get_type_methods(&i))
                .collect();
        }

        self.methods.get(ty_name).cloned().unwrap_or_default()
    }

    pub fn generate_type_id(&mut self) -> TypeId {
        self.next_type_id += 1;
        TypeId(self.next_type_id)
    }

    pub fn get_interface(&self, name: &str) -> Option<Interface> {
        self.interfaces.get(name).cloned()
    }

    pub fn extract_module(&mut self, name: &str, pkg: &PkgInfo) -> Module {
        let matches_decl = |decl: &Declaration| decl.file_id.package == name;

        let mut types = HashMap::new();

        for t in &self.types {
            // TODO asdf store the actual generated type names fmt, os...
            // this is excluding stuff like __Packageos created in `use` declarations
            if matches_decl(&t.1.decl) && !t.0.starts_with("__") {
                types.insert(t.0.clone(), t.1.clone());
            }
        }

        let mut enums = HashMap::new();
        for t in &self.enums {
            if types.contains_key(t.0) {
                enums.insert(t.0.clone(), t.1.clone());
            }
        }

        let mut structs = HashMap::new();
        for t in &self.structs {
            if types.contains_key(t.0) {
                structs.insert(t.0.clone(), t.1.clone());
            }
        }

        let mut values = HashMap::new();
        for t in &self.current_scope().values {
            let ty = t.1.ty.ty.get_name();
            // TODO asdf here too
            if let Some(name) = ty {
                if name.starts_with("__") {
                    continue;
                }
            }

            if matches_decl(&t.1.decl) {
                values.insert(t.0.clone(), t.1.clone());
            }
        }

        let mut methods = HashMap::new();
        for t in &types {
            let ty = t.1.ty.ty.get_name();

            if let Some(name) = ty {
                let ty_methods = self.get_type_methods(&name);
                if !ty_methods.is_empty() {
                    methods.insert(name, ty_methods);
                }
            }
        }

        let mut interfaces = HashMap::new();
        for i in self.interfaces.values() {
            if types.contains_key(&i.name) {
                interfaces.insert(i.name.clone(), i.clone());
            }
        }

        Module {
            name: name.to_string(),
            pkg: pkg.clone(),
            types,
            enums,
            structs,
            values,
            methods,
            interfaces,
        }
    }

    pub fn import_module(&mut self, pkg: &str, module: Module) {
        for (name, t) in &module.types {
            let name = module.rewrite(pkg, name);
            let ty = module.rewrite_type(pkg, &t.ty);
            self.add_type(name, ty, t.decl.clone());
        }

        for (name, def) in &module.structs {
            let name = module.rewrite(pkg, name);
            let mut def = def.clone();

            for f in &mut def.fields {
                f.ty = module.rewrite_type(pkg, &f.ty);
            }

            self.add_struct(name, def);
        }

        for (name, methods) in &module.methods {
            let name = module.rewrite(pkg, name);

            for (method_name, def) in methods {
                self.add_method(
                    &name,
                    method_name,
                    module.rewrite_type(pkg, &def.ty),
                    def.decl.clone(),
                );
            }
        }

        for interface in module.interfaces.values() {
            let name = module.rewrite(pkg, &interface.name);

            let mut methods = HashMap::new();
            for (m, ty) in &interface.methods {
                methods.insert(m.to_string(), module.rewrite_type_impl(pkg, ty));
            }

            self.add_interface(Interface {
                name,
                types: interface.types.clone(),
                methods,
            });
        }

        // values are imported in Infer::create_pkg_struct

        // TODO asdf fill the other ones in
    }

    pub fn add_module(&mut self, name: &str, module: Module) {
        self.modules.insert(name.to_string(), module);
    }

    pub fn get_module(&self, name: &str) -> Option<Module> {
        self.modules.get(name).cloned()
    }

    fn get_assumptions(&self, ty_name: &str) -> Option<Vec<String>> {
        self.scopes
            .iter()
            .find_map(|scope| scope.assumptions.get(ty_name))
            .cloned()
    }

    pub fn begin_mod_scope(&mut self) {
        self.begin_scope();
        self.add_value(
            "EXT".to_string(),
            Type::any().to_bounded(),
            Declaration::dummy(),
        );
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FileId {
    pub package: String,
    pub filename: String,
}

impl FileId {
    pub fn create(pkg: &Package, file: &File) -> FileId {
        Self {
            package: pkg.name.clone(),
            filename: file.name.clone(),
        }
    }

    pub fn builtin() -> Self {
        FileId {
            package: "std".to_string(),
            filename: "builtin.brg".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDef {
    pub ty: BoundedType,
    pub decl: Declaration,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueDef {
    pub ty: BoundedType,
    pub decl: Declaration,
}

impl ValueDef {
    pub fn dummy() -> Self {
        Self {
            ty: Type::dummy().to_bounded(),
            decl: Declaration::dummy(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Declaration {
    pub file_id: FileId,
    pub span: Span,
}

impl Declaration {
    pub fn dummy() -> Declaration {
        Self {
            span: Span::dummy(),
            file_id: FileId::builtin(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub pkg: PkgInfo,
    pub types: HashMap<String, TypeDef>,
    pub enums: HashMap<String, EnumDefinition>,
    pub structs: HashMap<String, StructDefinition>,
    pub values: HashMap<String, ValueDef>,
    pub methods: HashMap<String, HashMap<String, TypeDef>>, // type => (method => signature)
    pub interfaces: HashMap<String, Interface>,
}

impl Module {
    pub fn rewrite(&self, pkg: &str, name: &str) -> String {
        if self.types.contains_key(name) {
            format!("{}::{}", pkg, name)
        } else {
            name.to_string()
        }
    }

    pub fn rewrite_type(&self, pkg: &str, ty: &BoundedType) -> BoundedType {
        let new_ty = self.rewrite_type_impl(pkg, &ty.ty);
        BoundedType {
            generics: ty.generics.clone(),
            ty: new_ty,
        }
    }

    pub fn rewrite_type_impl(&self, pkg: &str, ty: &Type) -> Type {
        match ty {
            Type::Con { name, args } => Type::Con {
                name: self.rewrite(pkg, name),
                args: args
                    .iter()
                    .map(|a| self.rewrite_type_impl(pkg, a))
                    .collect(),
            },

            Type::Fun {
                args,
                bounds,
                ret,
                id,
            } => Type::Fun {
                args: args
                    .iter()
                    .map(|a| self.rewrite_type_impl(pkg, a))
                    .collect(),
                bounds: bounds.clone(),
                ret: self.rewrite_type_impl(pkg, ret).into(),
                id: id.clone(),
            },

            Type::Var(_) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Interface {
    pub name: String,
    pub types: Vec<String>,
    pub methods: HashMap<String, Type>,
}
