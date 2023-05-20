use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Con {
        name: String,
        args: Vec<Type>,
    },
    Fun {
        args: Vec<Type>,
        bounds: Vec<Bound>,
        ret: Box<Type>,
        id: TypeId,
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
    pub fn unit() -> Self {
        Self::Con {
            name: "Unit".into(),
            args: vec![],
        }
    }

    pub fn bool() -> Self {
        Self::Con {
            name: "bool".into(),
            args: vec![],
        }
    }

    pub fn int() -> Self {
        Self::Con {
            name: "int".into(),
            args: vec![],
        }
    }

    pub fn float() -> Type {
        Self::Con {
            name: "float64".into(),
            args: vec![],
        }
    }

    pub fn string() -> Self {
        Self::Con {
            name: "string".into(),
            args: vec![],
        }
    }

    pub fn char() -> Self {
        Self::Con {
            name: "rune".into(),
            args: vec![],
        }
    }

    pub fn slice(typ: Type) -> Self {
        Self::Con {
            name: "Slice".into(),
            args: vec![typ],
        }
    }

    pub fn tuple2(t1: Type, t2: Type) -> Self {
        Self::Con {
            name: "Tuple2".into(),
            args: vec![t1, t2],
        }
    }

    pub fn never() -> Type {
        Type::Con {
            name: "Never".to_string(),
            args: vec![],
        }
    }

    pub fn any() -> Type {
        Type::Con {
            name: "any".to_string(),
            args: vec![],
        }
    }

    pub fn receiver(typ: &Type) -> Self {
        Self::Con {
            name: "Receiver".into(),
            args: vec![typ.clone()],
        }
    }

    pub fn sender(typ: &Type) -> Self {
        Self::Con {
            name: "Sender".into(),
            args: vec![typ.clone()],
        }
    }

    pub fn option(typ: &Type) -> Self {
        Self::Con {
            name: "Option".into(),
            args: vec![typ.clone()],
        }
    }

    pub fn result(ok: &Type, err: &Type) -> Self {
        Self::Con {
            name: "Result".into(),
            args: vec![ok.clone(), err.clone()],
        }
    }

    pub fn generic(name: &str) -> Self {
        Self::Con {
            name: name.to_string(),
            args: vec![],
        }
    }

    // AST fields are initially populated with this
    pub fn dummy() -> Self {
        Self::Var(-1)
    }

    // Magic value used during inference to evaluate if expressions as statements
    pub fn discard() -> Self {
        Self::Var(-333)
    }

    pub fn is_result(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "Result",
            _ => false,
        }
    }

    pub fn is_option(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "Option",
            _ => false,
        }
    }

    pub fn stringify(&self, generics: &[String], skip_generics: bool) -> String {
        match self {
            Type::Con { name, args } => {
                let args_formatted = args
                    .iter()
                    .map(|a| a.stringify(generics, true))
                    .collect::<Vec<_>>()
                    .join(", ");

                // Special case Unit
                if name == "Unit" {
                    return "()".to_string();
                }

                // Special case slices
                if name.starts_with("Slice") {
                    return format!("[{}]", args_formatted);
                }

                // Special case tuples
                if name.starts_with("Tuple") {
                    return format!("({})", args_formatted);
                }

                // Special case Ref
                if name == "Ref" {
                    return format!("&{}", args_formatted);
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
            Type::Con { name, .. } => Some(name.clone()),
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

    pub fn to_bounded_with_generics(&self, generics: Vec<String>) -> BoundedType {
        BoundedType {
            generics,
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
            Type::Con { name, args } => Type::Con {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| Self::remove_vars_impl(a, vars))
                    .collect(),
            },

            Type::Fun {
                args,
                bounds,
                ret,
                id: fx,
            } => Type::Fun {
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
                id: fx.clone(),
            },

            Type::Var(v) => match vars.get(v) {
                Some(g) => Self::generic(g),
                None => {
                    let g = char::from_digit((vars.len() + 10).try_into().unwrap(), 16)
                        .unwrap()
                        .to_uppercase()
                        .to_string();

                    vars.insert(v.to_owned(), g.to_string());
                    Self::generic(&g)
                }
            },
        }
    }

    pub fn replace_vars_with_type(ty: &Type, vars: &HashMap<i32, Type>) -> Type {
        match ty {
            Type::Con { name, args } => Type::Con {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| Self::replace_vars_with_type(a, vars))
                    .collect(),
            },

            Type::Fun {
                args,
                bounds,
                ret,
                id: fx,
            } => Type::Fun {
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
                id: fx.clone(),
            },

            Type::Var(v) => match vars.get(v) {
                Some(ty) => ty.clone(),
                None => {
                    unreachable!("replacing vars with types, got unexpected variable");
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
        self == &Type::unit()
    }

    pub fn is_reference(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "Ref",
            _ => false,
        }
    }

    pub fn is_mut_reference(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "RefMut",
            _ => false,
        }
    }

    pub fn is_receiver(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "Receiver",
            _ => false,
        }
    }

    pub fn inner(&self) -> Option<Type> {
        self.get_args().and_then(|args| args.get(0).cloned())
    }

    pub fn reference(typ: Type) -> Self {
        Self::Con {
            name: "Ref".into(),
            args: vec![typ],
        }
    }

    pub fn add_any_receiver(&self) -> Type {
        match self {
            Type::Fun {
                args,
                bounds,
                ret,
                id,
            } => {
                let mut new_args = vec![Type::any()];
                new_args.extend_from_slice(args);

                Type::Fun {
                    args: new_args,
                    bounds: bounds.clone(),
                    ret: ret.clone(),
                    id: id.clone(),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn remove_references(&self) -> Type {
        if self.is_reference() || self.is_mut_reference() {
            return self.inner().unwrap().remove_references();
        }

        return self.clone();
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

    // temporary hacky function to gather together all numeric types.
    // conversions between the specific types are left as an exercises to the go compiler.
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Con { name, .. } => NUMERIC_TYPES.contains(&name.as_str()),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Type::Con { name, .. } => name == "string",
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Type::Var(_) => true,
            _ => false,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeId(pub i32);

impl TypeId {
    pub fn unset() -> TypeId {
        TypeId(-1)
    }
}

const NUMERIC_TYPES: &[&str] = &[
    "byte",
    "complex128",
    "complex64",
    "float32",
    "float64",
    "int",
    "int16",
    "int32",
    "int64",
    "int8",
    "rune",
    "uint",
    "uint16",
    "uint32",
    "uint64",
    "uint8",
];
