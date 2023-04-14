use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Con {
        name: String,
        args: Vec<Type>,
    },
    Fun {
        args: Vec<Type>,
        bounds: Vec<Type>,
        ret: Box<Type>,
        #[serde(serialize_with = "ordered_set")]
        fx: HashSet<String>,
    },
    Var(i32),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BoundedType {
    pub generics: Vec<String>,
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
            name: "Bool".into(),
            args: vec![],
        }
    }

    pub fn int() -> Self {
        Self::Con {
            name: "Int".into(),
            args: vec![],
        }
    }

    pub fn float() -> Type {
        Self::Con {
            name: "Float".into(),
            args: vec![],
        }
    }

    pub fn string() -> Self {
        Self::Con {
            name: "String".into(),
            args: vec![],
        }
    }

    pub fn char() -> Self {
        Self::Con {
            name: "Char".into(),
            args: vec![],
        }
    }

    pub fn list(typ: Type) -> Self {
        Self::Con {
            name: "List".into(),
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

                // Special case tuples
                if name.starts_with("Tuple") {
                    return format!("({})", args_formatted);
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
                fx,
            } => Type::Fun {
                args: args
                    .iter()
                    .map(|a| Self::remove_vars_impl(a, vars))
                    .collect(),
                bounds: bounds
                    .iter()
                    .map(|a| Self::remove_vars_impl(a, vars))
                    .collect(),
                ret: Self::remove_vars_impl(ret, vars).into(),
                fx: fx.clone(),
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

    pub fn get_bounds(&self) -> Vec<Type> {
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
}

fn stringify_bounds(g: &str, bounds: &[Type]) -> String {
    let matching_bounds = bounds
        .iter()
        .filter_map(|b| match b {
            Type::Con { name, args } => {
                let type_arg = args.first().unwrap().get_name().unwrap();
                if type_arg == g {
                    return Some(name.to_string());
                }

                None
            }
            Type::Fun { .. } => None,
            Type::Var(_) => None,
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

// used to serialize `fx` so that the order in the expectation files remains stable.
fn ordered_set<S>(value: &HashSet<String>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut ordered = Vec::from_iter(value.iter());
    ordered.sort();
    ordered.serialize(serializer)
}
