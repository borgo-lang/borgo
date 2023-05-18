use std::collections::HashMap;

use crate::ast::{
    Binding, EnumFieldDef, Expr, Function, Operator, Pat, PkgInfo, Result, Span, StructFieldDef,
    TypeAst, UnOp,
};
use crate::type_::Type;

use syn::spanned::Spanned;

pub fn type_from_path(path: &syn::Path) -> TypeAst {
    let last_segment = path.segments.iter().last().unwrap();

    TypeAst::Con {
        name: path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),

        args: match &last_segment.arguments {
            syn::PathArguments::AngleBracketed(a) => a
                .args
                .iter()
                .map(|generic| match generic {
                    syn::GenericArgument::Type(ty) => type_from_expr(ty.to_owned()),
                    _ => panic!("not a generic type"),
                })
                .collect(),

            syn::PathArguments::None => vec![],

            _ => panic!("non bracketed args"),
        },
    }
}

pub fn type_from_expr(ty: syn::Type) -> TypeAst {
    match ty {
        syn::Type::Path(ty) => type_from_path(&ty.path),

        syn::Type::Tuple(t) => {
            if t.elems.is_empty() {
                return TypeAst::Con {
                    name: "Unit".to_string(),
                    args: vec![],
                };
            }

            let name = format!("Tuple{}", t.elems.len());
            let args = t.elems.into_iter().map(type_from_expr).collect();
            TypeAst::Con { name, args }
        }

        syn::Type::BareFn(f) => {
            let args = f.inputs.into_iter().map(|a| type_from_expr(a.ty)).collect();
            let ret = parse_output(f.output, TypeAst::unit());

            TypeAst::Fun {
                args,
                ret: ret.into(),
            }
        }

        syn::Type::Reference(r) => {
            let name = if r.mutability.is_some() {
                "RefMut"
            } else {
                "Ref"
            };

            let args = vec![type_from_expr(*r.elem)];

            TypeAst::Con {
                name: name.to_string(),
                args,
            }
        }

        syn::Type::Slice(t) => {
            let args = vec![type_from_expr(*t.elem)];
            TypeAst::Con {
                name: "Slice".to_string(),
                args,
            }
        }

        _ => todo!("non path type {:#?}", ty),
    }
}

pub fn parse_generics(generics: &syn::Generics) -> Vec<String> {
    generics
        .params
        .iter()
        .map(|generic| match generic {
            syn::GenericParam::Type(ty) => ty.ident.to_string(),
            _ => panic!("not a generic type"),
        })
        .collect()
}

pub enum Fields {
    TupleCons(Vec<EnumFieldDef>),
    StructFields(Vec<StructFieldDef>),
}

pub fn parse_fields(fields: syn::Fields) -> Fields {
    match fields {
        syn::Fields::Unit => Fields::TupleCons(vec![]),

        syn::Fields::Unnamed(u) => {
            let fields = u
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(index, f)| {
                    // let span = Span::make(f.span());

                    EnumFieldDef {
                        name: format!("field{index}"),
                        ann: type_from_expr(f.ty),
                        ty: Type::dummy(),
                    }
                })
                .collect();

            Fields::TupleCons(fields)
        }

        syn::Fields::Named(n) => {
            let fields = n
                .named
                .into_iter()
                .map(|f| {
                    // let span = Span::make(f.span());

                    StructFieldDef {
                        name: f.ident.unwrap().to_string(),
                        ann: type_from_expr(f.ty),
                        ty: Type::dummy().to_bounded(),
                    }
                })
                .collect();

            Fields::StructFields(fields)
        }
    }
}

pub fn parse_input(input: syn::FnArg, receiver: Option<(TypeAst, Span)>) -> Result<Binding> {
    match input {
        syn::FnArg::Typed(arg) => {
            let pat = Pat::from_pat_expr(*arg.pat)?;
            let ann = type_from_expr(*arg.ty);

            Ok(Binding {
                pat,
                ann,
                ty: Type::dummy(),
            })
            // todo!("{:#?}", arg.pat)
        }

        syn::FnArg::Receiver(r) => match receiver {
            Some((ann, span)) => {
                // TODO asdf this logic should be applied to other args too, not just self
                let is_mut = r.mutability.is_some();

                let reference = if r.reference.is_some() {
                    if is_mut {
                        Some("RefMut")
                    } else {
                        Some("Ref")
                    }
                } else {
                    None
                };

                let ann = match reference {
                    Some(name) => TypeAst::Con {
                        name: name.to_string(),
                        args: vec![ann],
                    },
                    None => ann,
                };

                Ok(Binding {
                    pat: Pat::Type {
                        ident: "self".to_string(),
                        is_mut,
                        ann: ann.clone(),
                        span,
                    },
                    ann,
                    ty: Type::dummy(),
                })
            }
            _ => panic!("found receiver but no arg provided"),
        },
    }
}

pub fn parse_output(output: syn::ReturnType, default: TypeAst) -> TypeAst {
    match output {
        syn::ReturnType::Default => default,
        syn::ReturnType::Type(_, ty) => type_from_expr(*ty),
    }
}

pub fn parse_signature(
    sig: syn::Signature,
    receiver: Option<(TypeAst, Span)>,
) -> Result<(Vec<String>, Vec<Binding>, TypeAst)> {
    let generics = parse_generics(&sig.generics);
    let args = sig
        .inputs
        .into_iter()
        .map(|i| parse_input(i, receiver.clone()))
        .collect::<Result<_>>()?;

    let ret = parse_output(sig.output, TypeAst::unit());
    Ok((generics, args, ret))
}

pub fn parse_item_fn(fun: syn::ItemFn) -> Result<Function> {
    let sig = fun.sig;
    let (generics, args, ann) = self::parse_signature(sig.clone(), None)?;
    let bounds = self::parse_bounds(&sig.generics);

    let span = (*fun.block).span();
    let stmts = fun
        .block
        .stmts
        .into_iter()
        .map(Expr::from_statement)
        .collect::<Result<_>>()?;

    let body = Expr::Block {
        stmts,
        ty: Type::dummy(),
        span: Span::make(span),
    };

    Ok(Function {
        name: sig.ident.to_string(),
        generics,
        bounds,
        args,
        ann,
        ret: Type::dummy(),
        body: body.into(),
        bounded_ty: Type::dummy().to_bounded(),
    })
}

pub fn parse_ident_from_path(e: syn::ExprPath) -> String {
    Pat::parse_path(e.path)
}

pub fn parse_member(m: syn::Member) -> Result<String> {
    match m {
        syn::Member::Named(n) => Ok(n.to_string()),
        syn::Member::Unnamed(u) => Expr::tuple_index_string(u.index),
    }
}

pub fn parse_operator(op: syn::BinOp) -> Result<Operator> {
    match op {
        syn::BinOp::Add(_) => Ok(Operator::Add),
        syn::BinOp::Sub(_) => Ok(Operator::Sub),
        syn::BinOp::Mul(_) => Ok(Operator::Mul),
        syn::BinOp::Div(_) => Ok(Operator::Div),
        syn::BinOp::Rem(_) => Ok(Operator::Rem),
        syn::BinOp::Eq(_) => Ok(Operator::Eq),
        syn::BinOp::Ne(_) => Ok(Operator::Ne),
        syn::BinOp::Lt(_) => Ok(Operator::Lt),
        syn::BinOp::Le(_) => Ok(Operator::Le),
        syn::BinOp::Gt(_) => Ok(Operator::Gt),
        syn::BinOp::Ge(_) => Ok(Operator::Ge),
        syn::BinOp::And(_) => Ok(Operator::And),
        syn::BinOp::Or(_) => Ok(Operator::Or),

        _ => panic!("operator not implemented {:?}", op),
    }
}

pub fn parse_unop(op: syn::UnOp) -> Result<UnOp> {
    match op {
        syn::UnOp::Deref(_) => Ok(UnOp::Deref),
        syn::UnOp::Not(_) => Ok(UnOp::Not),
        syn::UnOp::Neg(_) => Ok(UnOp::Neg),
    }
}

pub fn parse_bounds(generics: &syn::Generics) -> Vec<(String, TypeAst)> {
    let mut ret = vec![];

    for x in generics.type_params() {
        let generic_type = x.ident.to_string();

        for b in x.bounds.iter() {
            let b = match b {
                syn::TypeParamBound::Trait(b) => b,
                syn::TypeParamBound::Lifetime(_) => panic!("we don't need no lifetimes"),
            };

            ret.push((generic_type.clone(), type_from_path(&b.path)));
        }
    }

    ret
}

pub fn parse_attrs(attrs: &[syn::Attribute]) -> Vec<String> {
    attrs
        .iter()
        .map(|a| {
            a.path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect::<Vec<_>>()
                .join("::")
        })
        .collect::<Vec<_>>()
}

pub fn parse_use(tree: syn::UseTree) -> String {
    match tree {
        syn::UseTree::Name(n) => n.ident.to_string(),
        syn::UseTree::Path(p) => format!("{}::{}", p.ident.to_string(), parse_use(*p.tree)),

        // syn::UseTree::Rename(_) => todo!(),
        _ => panic!("unsupported UseTree {:?}", tree),
    }
}

pub fn parse_pkgmod_info(attrs: &[syn::Attribute]) -> std::result::Result<PkgInfo, String> {
    use std::result::Result;

    if attrs.first().is_none() {
        return Err("expected attribute #[...] on mod declaration".to_string());
    }

    let mut source = attrs[0].tokens.to_string();
    source.remove(0);
    source.remove(source.len() - 1);

    // parse pkg info like a butcher would
    let parts = source
        .split(",")
        .map(|s| {
            let pair: Vec<_> = s.split("=").collect();
            if pair.len() != 2 {
                return Err("Failed to parse attribute".to_string());
            }

            Ok((
                pair[0].trim().to_string(),
                pair[1].trim().to_string().replace(" ", ""),
            ))
        })
        .collect::<Result<HashMap<_, _>, _>>()?;

    let name = parts
        .get("name")
        .ok_or("Attribute `name` not found".to_string())?;

    let path = parts
        .get("path")
        .ok_or("Attribute `path` not found".to_string())?;

    Ok(PkgInfo {
        name: name.to_string(),
        path: path.to_string(),
    })
}
