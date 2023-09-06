use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::ast::{Generic, Span};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Con {
        id: Symbol,
        args: Vec<Type>,
    },
    Fun {
        args: Vec<Type>,
        bounds: Vec<Bound>,
        ret: Box<Type>,
    },
    Var(i32),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BoundedType {
    pub generics: Vec<String>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Bound {
    pub generic: Type,
    pub ty: Type,
}

impl Type {
    // Build up a type that doesn't exist in any module.
    // This is useful in certain parts of the compiler where we still need to make up types,
    // but can't produce reasonable symbols or spans. Think generating error messages or anywhere
    // where a concrete type is desirable but more as a placeholder than an actual real type.
    pub fn ethereal(name: &str) -> Self {
        Self::Con {
            id: Symbol::ethereal(name),
            args: vec![],
        }
    }

    // AST fields are initially populated with this
    pub fn dummy() -> Self {
        Self::Var(-1)
    }

    // Magic value used during inference to indicate that this type isn't really relevant.
    // This stops unification and skips constraining other types in certain contexts
    // ie. an if expression used as a statement, we don't really need to assign a type.
    pub fn discard() -> Self {
        Self::Var(-333)
    }

    pub fn is_result(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "Result",
            _ => false,
        }
    }

    pub fn is_option(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "Option",
            _ => false,
        }
    }

    pub fn stringify(&self, generics: &[String], skip_generics: bool) -> String {
        match self {
            Type::Con { id, args } => {
                let args_formatted = args
                    .iter()
                    .map(|a| a.stringify(generics, true))
                    .collect::<Vec<_>>()
                    .join(", ");

                let name = id.name.as_str();

                // Special case Unit
                if name == "Unit" {
                    return "()".to_string();
                }

                // Special case slices
                if name == "Slice" {
                    return format!("[{}]", args_formatted);
                }

                // Special case tuples
                if name.starts_with("Tuple") {
                    return format!("({})", args_formatted);
                }

                // Special case Ref
                if name == "Ref" {
                    return format!("*{}", args_formatted);
                }

                // Special case RefMut
                if name == "RefMut" {
                    return format!("&mut {}", args_formatted);
                }

                // All other types

                if args.is_empty() {
                    return name.to_string();
                }

                format!("{}<{}>", name, args_formatted)
            }

            Type::Var(var) => {
                format!("{}", var)
            }

            Type::Fun {
                args, bounds, ret, ..
            } => {
                let args_formatted = args
                    .iter()
                    .map(|a| a.stringify(generics, true))
                    .collect::<Vec<_>>()
                    .join(", ");

                let ret_formatted = (*ret).stringify(generics, true);

                let generics = if skip_generics || generics.is_empty() {
                    "".to_string()
                } else {
                    let mut generics = generics
                        .iter()
                        .map(|g| stringify_bounds(g, bounds))
                        .collect::<Vec<_>>();

                    generics.sort();
                    format!("<{}>", generics.join(", "))
                };

                format!("fn {}({}) -> {}", generics, args_formatted, ret_formatted)
            }
        }
    }

    pub fn get_name(&self) -> Option<String> {
        if self.is_reference() {
            return self.inner().unwrap().get_name();
        }

        match self {
            Type::Con { id, .. } => Some(id.name.clone()),
            _ => None,
        }
    }

    pub fn get_args(&self) -> Option<Vec<Type>> {
        match self {
            Type::Con { args, .. } => Some(args.clone()),
            _ => None,
        }
    }

    pub fn get_function_args(&self) -> Option<Vec<Type>> {
        match self {
            Type::Fun { args, .. } => Some(args.clone()),
            _ => None,
        }
    }

    pub fn get_function_ret(&self) -> Option<Type> {
        match self {
            Type::Fun { ret, .. } => Some(*ret.clone()),
            _ => None,
        }
    }

    pub fn to_bounded(&self) -> BoundedType {
        self.to_bounded_with_generics(vec![])
    }

    pub fn to_bounded_with_generics(&self, generics: Vec<Generic>) -> BoundedType {
        BoundedType {
            generics: generics.iter().map(|g| g.name.to_string()).collect(),
            ty: self.to_owned(),
        }
    }

    pub fn remove_vars(types: Vec<&Type>) -> (Vec<Type>, Vec<String>) {
        let mut vars = HashMap::new();
        let types = types
            .iter()
            .map(|v| Self::remove_vars_impl(v, &mut vars))
            .collect();

        (types, vars.into_values().collect())
    }

    fn remove_vars_impl(ty: &Type, vars: &mut HashMap<i32, String>) -> Type {
        match ty {
            Type::Con { id: name, args } => Type::Con {
                id: name.clone(),
                args: args
                    .iter()
                    .map(|a| Self::remove_vars_impl(a, vars))
                    .collect(),
            },

            Type::Fun { args, bounds, ret } => Type::Fun {
                args: args
                    .iter()
                    .map(|a| Self::remove_vars_impl(a, vars))
                    .collect(),
                bounds: bounds
                    .iter()
                    .map(|b| Bound {
                        generic: Self::remove_vars_impl(&b.generic, vars),
                        ty: Self::remove_vars_impl(&b.ty, vars),
                    })
                    .collect(),
                ret: Self::remove_vars_impl(ret, vars).into(),
            },

            Type::Var(v) => match vars.get(v) {
                Some(g) => Self::ethereal(g),
                None => {
                    let g = char::from_digit((vars.len() + 10).try_into().unwrap(), 16)
                        .unwrap()
                        .to_uppercase()
                        .to_string();

                    vars.insert(v.to_owned(), g.to_string());
                    Self::ethereal(&g)
                }
            },
        }
    }

    pub fn replace_vars_with_type(ty: &Type, vars: &HashMap<i32, Type>) -> Type {
        match ty {
            Type::Con { id: name, args } => Type::Con {
                id: name.clone(),
                args: args
                    .iter()
                    .map(|a| Self::replace_vars_with_type(a, vars))
                    .collect(),
            },

            Type::Fun { args, bounds, ret } => Type::Fun {
                args: args
                    .iter()
                    .map(|a| Self::replace_vars_with_type(a, vars))
                    .collect(),
                bounds: bounds
                    .iter()
                    .map(|b| Bound {
                        generic: Self::replace_vars_with_type(&b.generic, vars),
                        ty: Self::replace_vars_with_type(&b.ty, vars),
                    })
                    .collect(),
                ret: Self::replace_vars_with_type(ret, vars).into(),
            },

            Type::Var(v) => match vars.get(v) {
                Some(ty) => ty.clone(),
                None => {
                    Type::Var(*v)
                    // unreachable!("replacing vars with types, got unexpected variable");
                }
            },
        }
    }

    pub fn get_bounds(&self) -> Vec<Bound> {
        match self {
            Type::Con { .. } => vec![],
            Type::Var(_) => vec![],
            Type::Fun { bounds, .. } => bounds.clone(),
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Type::Con { .. } => false,
            Type::Var(_) => false,
            Type::Fun { .. } => true,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "Unit",
            _ => false,
        }
    }

    pub fn is_reference(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "Ref",
            _ => false,
        }
    }

    pub fn is_mut_reference(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "RefMut",
            _ => false,
        }
    }

    pub fn is_receiver(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "Receiver",
            _ => false,
        }
    }

    pub fn inner(&self) -> Option<Type> {
        self.get_args().and_then(|args| args.get(0).cloned())
    }

    pub fn remove_references(&self) -> Type {
        if self.is_reference() || self.is_mut_reference() {
            return self.inner().unwrap().remove_references();
        }

        self.clone()
    }

    pub fn remove_receiver(&mut self) {
        match self {
            Type::Fun { ref mut args, .. } => {
                args.remove(0);
            }
            _ => unreachable!(),
        }
    }

    pub fn is_discard(&self) -> bool {
        self == &Type::discard()
    }

    pub fn is_variadic(&self) -> Option<Type> {
        let args = self.get_function_args()?;
        let last = args.last()?;

        if last.get_name()? == "VarArgs" {
            return last.inner();
        }

        None
    }

    pub fn is_string(&self) -> bool {
        match self {
            Type::Con { id, .. } => id.name == "string",
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Type::Var(_) => true,
            _ => false,
        }
    }

    pub fn get_symbol(&self) -> Symbol {
        match self.remove_references() {
            Type::Con { id, .. } => id,
            _ => panic!("called get_symbol on {:#?}", self),
        }
    }

    pub fn swap_arg(&self, index: usize, typ: Type) -> Type {
        match self {
            Type::Con { id, args } => {
                let mut new_args = args.clone();
                new_args[index] = typ;
                Type::Con {
                    id: id.clone(),
                    args: new_args,
                }
            }
            _ => panic!("called swap_arg on {:#?}", self),
        }
    }

    // Given a function type, take the T out of Option<T> or Result<T>
    pub fn get_return_inner_arg(&self) -> Type {
        match self {
            Type::Fun { ret, .. } => {
                if ret.is_option() || ret.is_result() {
                    return ret.get_args().unwrap()[0].clone();
                }

                panic!("expected Option<T> or Result<T>")
            }
            _ => (),
        }

        panic!("called get_return_inner_arg() with wrong type {}", self)
    }
}

fn stringify_bounds(g: &str, bounds: &[Bound]) -> String {
    let matching_bounds = bounds
        .iter()
        .filter_map(|b| {
            if b.generic.get_name().unwrap() == g {
                return Some(b.ty.to_string());
            }

            None
        })
        .collect::<Vec<_>>();

    if matching_bounds.is_empty() {
        return g.to_string();
    }

    format!("{g}: {bounds}", bounds = matching_bounds.join(" + "))
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (types, generics) = Self::remove_vars(vec![self]);
        write!(f, "{}", types[0].stringify(&generics, false))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub String);

impl ModuleId {
    pub fn from_str(name: &str) -> ModuleId {
        ModuleId(name.to_string())
    }

    pub fn is_std(&self) -> bool {
        self.0 == "std"
    }

    pub fn is_empty(&self) -> bool {
        self.0 == "**empty"
    }

    pub fn empty() -> ModuleId {
        ModuleId::from_str("**empty")
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Deserialize)]
pub struct Symbol {
    pub module: ModuleId,
    pub name: String,
    pub span: Span,
}

impl Symbol {
    // This is only used from Type::ethereal. See the explanation there.
    pub fn ethereal(name: &str) -> Symbol {
        Symbol {
            module: ModuleId::empty(),
            name: name.to_string(),
            span: Span::dummy(),
        }
    }
}

impl Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.name)
    }
}
