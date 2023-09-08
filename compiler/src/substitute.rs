use crate::ast::{
    Arm, Binding, Expr, Function, Literal, Loop, Pat, SelectArm, SelectArmPat, StructField,
    StructFieldPat,
};
use crate::infer;
use crate::type_::Type;

pub fn substitute_expr(expr: Expr, instance: &mut infer::Infer) -> Expr {
    match expr {
        Expr::Literal { lit, ty, span } => match lit {
            Literal::Slice(expr) => {
                let new_expr = expr
                    .iter()
                    .map(|e| substitute_expr(e.clone(), instance))
                    .collect();

                Expr::Literal {
                    lit: Literal::Slice(new_expr),
                    ty: instance.substitute(ty),
                    span,
                }
            }
            _ => Expr::Literal {
                lit,
                ty: instance.substitute(ty),
                span,
            },
        },

        Expr::Closure {
            fun,
            kind,
            ty,
            span,
        } => Expr::Closure {
            fun: Function {
                name: fun.name,
                generics: fun.generics,
                args: fun
                    .args
                    .iter()
                    .map(|b| substitute_binding(b, instance))
                    .collect(),

                body: substitute_expr(*fun.body, instance).into(),
                ret: instance.substitute(fun.ret.clone()),
                ann: fun.ann,
                bounded_ty: fun.bounded_ty,
            },
            ty: instance.substitute(ty),
            kind,
            span,
        },

        Expr::Block { stmts, ty, span } => Expr::Block {
            stmts: stmts
                .iter()
                .map(|e| {
                    let new_expr = substitute_expr(e.clone(), instance);

                    // Statements inside a block are discarded, so they may never end up getting
                    // constrained to anything. It's desirable to have a concrete type for all
                    // expressions, so replace with Unit if a type hasn't been inferred yet.

                    match new_expr.get_type() {
                        Type::Var(_) => new_expr.replace_type(instance.type_unit()),
                        _ => new_expr,
                    }
                })
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Let {
            binding,
            value,
            mutable,
            ty,
            span,
        } => Expr::Let {
            binding: substitute_binding(&binding, instance),
            value: substitute_expr(*value, instance).into(),
            mutable,
            ty: instance.substitute(ty),
            span,
        },

        Expr::Var {
            value,
            decl,
            generics_instantiated,
            ty,
            span,
        } => Expr::Var {
            value,
            decl,
            generics_instantiated: generics_instantiated
                .iter()
                .map(|ty| instance.substitute(ty.clone()))
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Call {
            func,
            args,
            ty,
            span,
        } => Expr::Call {
            func: substitute_expr(*func, instance).into(),
            args: args
                .iter()
                .map(|a| substitute_expr(a.clone(), instance))
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::If {
            cond,
            then,
            els,
            ty,
            span,
        } => Expr::If {
            cond: substitute_expr(*cond, instance).into(),
            then: substitute_expr(*then, instance).into(),
            els: substitute_expr(*els, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::CheckType {
            expr,
            ann,
            ty,
            span,
        } => Expr::CheckType {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            ann,
            span,
        },

        Expr::Match {
            subject,
            arms,
            ty,
            span,
        } => Expr::Match {
            subject: substitute_expr(*subject, instance).into(),
            arms: arms
                .iter()
                .map(|a| Arm {
                    pat: substitute_pat(a.pat.clone(), instance),
                    expr: substitute_expr(a.expr.clone(), instance),
                })
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Tuple { elems, ty, span } => Expr::Tuple {
            elems: elems
                .iter()
                .map(|a| substitute_expr(a.clone(), instance))
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::EnumDef { .. } => expr,
        Expr::StructDef { .. } => expr,

        Expr::StructCall {
            name,
            fields,
            rest,
            ty,
            span,
        } => Expr::StructCall {
            name,
            fields: fields
                .iter()
                .map(|f| StructField {
                    name: f.name.clone(),
                    value: substitute_expr(f.value.clone(), instance),
                })
                .collect(),

            rest: rest.map(|a| substitute_expr(a, instance)).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::FieldAccess {
            expr,
            ty,
            field,
            span,
        } => Expr::FieldAccess {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            field,
            span,
        },

        Expr::VarUpdate {
            target,
            value,
            span,
        } => Expr::VarUpdate {
            target: substitute_expr(*target, instance).into(),
            value: substitute_expr(*value, instance).into(),
            span,
        },

        Expr::MethodCall {
            target,
            args,
            method,
            ty,
            span,
        } => Expr::MethodCall {
            target: substitute_expr(*target, instance).into(),
            args: args
                .iter()
                .map(|e| substitute_expr(e.clone(), instance))
                .collect(),
            method,
            ty: instance.substitute(ty),
            span,
        },

        Expr::Return { expr, ty, span } => Expr::Return {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Try { expr, ty, span } => Expr::Try {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::ImplBlock {
            ann,
            ty,
            self_name,
            generics,
            items,
            span,
        } => Expr::ImplBlock {
            ann,
            generics,
            self_name,
            ty: instance.substitute(ty),
            span,
            items: items
                .iter()
                .map(|e| substitute_expr(e.clone(), instance))
                .collect(),
        },

        Expr::Binary {
            left,
            right,
            op,
            ty,
            span,
        } => Expr::Binary {
            left: substitute_expr(*left, instance).into(),
            right: substitute_expr(*right, instance).into(),
            op,
            ty: instance.substitute(ty),
            span,
        },

        Expr::Paren { expr, ty, span } => Expr::Paren {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Unary { op, expr, ty, span } => Expr::Unary {
            op,
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Const {
            ident,
            expr,
            ann,
            ty,
            span,
        } => Expr::Const {
            ident,
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            ann,
            span,
        },

        Expr::Debug {
            kind,
            expr,
            ty,
            span,
        } => Expr::Debug {
            kind,
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Spawn { expr, ty, span } => Expr::Spawn {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Select { arms, span } => {
            let new_arms = arms
                .iter()
                .map(|a| {
                    let pat = match a.pat.clone() {
                        SelectArmPat::Recv(b, expr) => SelectArmPat::Recv(
                            substitute_binding(&b, instance),
                            substitute_expr(expr, instance),
                        ),
                        SelectArmPat::Send(expr) => {
                            SelectArmPat::Send(substitute_expr(expr, instance))
                        }
                        SelectArmPat::Wildcard => SelectArmPat::Wildcard,
                    };

                    let expr = substitute_expr(a.expr.clone(), instance);

                    SelectArm { pat, expr }
                })
                .collect();

            Expr::Select {
                arms: new_arms,
                span,
            }
        }

        Expr::Defer { expr, ty, span } => Expr::Defer {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Reference {
            expr,
            mutable,
            ty,
            span,
        } => Expr::Reference {
            expr: substitute_expr(*expr, instance).into(),
            mutable,
            ty: instance.substitute(ty),
            span,
        },

        Expr::Index {
            expr,
            index,
            ty,
            span,
        } => Expr::Index {
            expr: substitute_expr(*expr, instance).into(),
            index: substitute_expr(*index, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Loop { kind, body, span } => {
            let new_kind = match kind {
                Loop::NoCondition => kind,
                Loop::WithCondition { binding, expr } => Loop::WithCondition {
                    binding: substitute_binding(&binding, instance),
                    expr: substitute_expr(*expr, instance).into(),
                },
                Loop::While { expr } => Loop::While {
                    expr: substitute_expr(*expr, instance).into(),
                },
            };

            Expr::Loop {
                kind: new_kind,
                body: substitute_expr(*body, instance).into(),
                span,
            }
        }

        Expr::Trait {
            name,
            generics,
            items,
            supertraits,
            span,
        } => Expr::Trait {
            name,
            generics,
            items: items
                .iter()
                .map(|e| substitute_expr(e.clone(), instance))
                .collect(),
            supertraits,
            span,
        },

        Expr::Flow { kind, span } => Expr::Flow { kind, span },
        Expr::TypeAlias { def, span } => Expr::TypeAlias { def, span },
        Expr::NewtypeDef { def, span } => Expr::NewtypeDef { def, span },
        Expr::UsePackage { import, span } => Expr::UsePackage { import, span },
        Expr::Unit { ty, span } => Expr::Unit {
            ty: instance.substitute(ty),
            span,
        },
        Expr::Raw { text } => Expr::Raw { text },
        Expr::Noop => Expr::Noop,
        Expr::Todo => Expr::Todo,
    }
}

fn substitute_binding(b: &Binding, instance: &mut infer::Infer) -> Binding {
    Binding {
        pat: substitute_pat(b.pat.clone(), instance),
        ann: b.ann.clone(),
        ty: instance.substitute(b.ty.clone()),
    }
}

fn substitute_pat(pat: Pat, instance: &mut infer::Infer) -> Pat {
    match pat {
        Pat::Type { .. } => pat,

        Pat::Lit { lit, ty, span } => Pat::Lit {
            lit,
            ty: instance.substitute(ty),
            span,
        },

        Pat::Pat {
            ident,
            elems,
            ty,
            span,
        } => Pat::Pat {
            ident,
            elems: elems
                .into_iter()
                .map(|e| substitute_pat(e, instance))
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Pat::Struct {
            ident,
            fields,
            ty,
            span,
        } => Pat::Struct {
            ident,
            fields: fields
                .into_iter()
                .map(|f| StructFieldPat {
                    value: substitute_pat(f.value, instance),
                    ..f
                })
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Pat::Wild { span } => Pat::Wild { span },
        Pat::Unit { ty, span } => Pat::Unit {
            ty: instance.substitute(ty),
            span,
        },
    }
}
