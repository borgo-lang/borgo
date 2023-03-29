use serde::{Deserialize, Serialize};

use crate::ast::{EnumDefinition, File, Span, StructDefinition};
use crate::project::Package;
use crate::type_::{BoundedType, Type};
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Clone)]
struct Scope {
    // used for local types ie. generics
    types: HashMap<String, TypeDef>,
    // bindings in the value universe, ie. x => Int
    values: HashMap<String, ValueDef>,
    // assumptions about a type found in trait bounds, ie. equals<T>. Used for constraint resolution.
    assumptions: Vec<Type>,
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
    scopes: VecDeque<Scope>,
    types: HashMap<String, TypeDef>,
    enums: HashMap<String, EnumDefinition>,
    structs: HashMap<String, StructDefinition>,
    aliases: HashMap<String, String>,
    effects: HashSet<String>,
    traits: HashSet<String>,
    derived_traits: HashSet<DerivedOverload>,
}

impl GlobalState {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            scopes: VecDeque::from([Scope::new()]),
            types: Default::default(),
            enums: Default::default(),
            structs: Default::default(),
            aliases: Default::default(), // Ok -> Result::Ok
            effects: Default::default(),
            traits: Default::default(),
            derived_traits: Default::default(),
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
        self.types.insert(name, TypeDef { ty, decl });
    }

    pub fn get_type(&mut self, name: &str) -> Option<BoundedType> {
        self.get_global_type(name)
            .or_else(|| self.current_scope().get_type(name).map(|t| t.ty))
    }

    pub fn get_global_type(&self, name: &str) -> Option<BoundedType> {
        self.types.get(name).cloned().map(|t| t.ty)
    }

    pub fn get_global_type_declaration(&self, name: &str) -> Option<TypeDef> {
        self.types.get(name).cloned()
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

    pub fn add_alias(&mut self, alias: String, source: String) {
        self.aliases.insert(alias, source);
    }

    pub fn add_effect(&mut self, effect: String) {
        self.effects.insert(effect);
    }

    pub fn is_effect(&self, effect: &str) -> bool {
        self.effects.contains(effect)
    }

    pub fn get_enum(&self, name: &str) -> Option<EnumDefinition> {
        self.enums.get(name).cloned()
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDefinition> {
        self.structs.get(name).cloned()
    }

    /// Follows an alias or returns the input unchanged
    pub fn resolve_name(&self, name: &str) -> String {
        self.aliases
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

    pub fn lookup_method(
        &mut self,
        method: &str,
        first_arg: &Type,
        _arity: usize,
    ) -> Option<String> {
        // This should be a bit cleverer, but ok for now
        // Aliasing makes this hard to follow
        // Note that this doesn't handle struct fields with functions

        // 1. If we know the first argument, then lookup a fully qualified function ie. List::map
        if let Type::Con { name, .. } = first_arg {
            let checks = vec![name.clone(), format!("{}::{}", name, method)];

            let found = checks
                .into_iter()
                .find_map(|n| self.get_value(&n).map(|_| n));

            if let Some(name) = found {
                return Some(self.resolve_name(&name));
            }
        }

        // 2. The first argument is generic, this could be something like Debug::assert_eq(a, b)
        // For now, scan the whole set of values looking for something that might match
        let lookup = format!("::{}", method);
        let candidate = self.current_scope().values.iter().find_map(|(name, def)| {
            if name.ends_with(&lookup) {
                match &def.get_type() {
                    Type::Fun { args, .. } => {
                        if let Some(first) = args.first() {
                            if def.is_generic(first) {
                                return Some(name);
                            }
                        }
                    }
                    _ => (),
                }
            }

            None
        });

        if let Some(candidate) = candidate {
            return Some(candidate.to_string());
        }

        // 3. If all else fails, look for a function with that name.
        if let Some(_) = self.get_value(method) {
            return Some(self.resolve_name(method));
        }

        None
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

    pub fn add_assumption(&mut self, assumption: &Type) {
        self.current_scope().assumptions.push(assumption.clone());
    }

    pub fn constraint_satisfied(&self, constraint: &Type) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.assumptions.contains(constraint))
    }

    pub fn add_trait(&mut self, name: &str) {
        self.traits.insert(name.to_string());
    }

    pub fn get_all_types(&self) -> HashMap<String, BoundedType> {
        self.types
            .iter()
            .map(|(name, t)| (name.clone(), t.ty.clone()))
            .collect()
    }

    pub fn get_overloads(&self) -> HashSet<String> {
        self.traits.clone()
    }

    pub fn add_derived_overload(&mut self, overload_name: &str, ty_name: &str) {
        let value = DerivedOverload {
            overload: overload_name.to_string(),
            ty: ty_name.to_string(),
        };

        self.derived_traits.insert(value);
    }

    pub fn get_derived_overloads(&self) -> HashSet<DerivedOverload> {
        self.derived_traits.clone()
    }

    pub fn remove_derived_overload(&mut self, name: &str) {
        if !name.contains("::") {
            return;
        }

        let parts: Vec<_> = name.split("::").collect();
        let ty = parts[0].to_string();
        let overload = parts[1].to_string();

        let value = DerivedOverload { overload, ty };
        self.derived_traits.remove(&value);
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

    fn get_type(&self) -> Type {
        self.ty.ty.clone()
    }

    /// Used in lookup_method to find if this type is generic
    fn is_generic(&self, first: &Type) -> bool {
        let name = first.get_name();
        if name.is_none() {
            return false;
        }

        self.ty.generics.contains(&name.unwrap())
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

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DerivedOverload {
    pub overload: String,
    pub ty: String,
}
