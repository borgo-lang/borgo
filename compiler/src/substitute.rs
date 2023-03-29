use crate::ast::{Arm, Binding, Expr, Function, Literal, Pat, StructField, StructFieldPat};
use crate::infer;

pub fn substitute_expr(expr: Expr, instance: &mut infer::Infer) -> Expr {
    match expr {
        Expr::Literal { lit, ty, span } => match lit {
            Literal::List(expr) => {
                let new_expr = expr
                    .iter()
                    .map(|e| substitute_expr(e.clone(), instance))
                    .collect();

                Expr::Literal {
                    lit: Literal::List(new_expr),
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
                bounds: fun.bounds,
                args: fun
                    .args
                    .iter()
                    .map(|b| substitute_binding(b, instance))
                    .collect(),

                body: substitute_expr(*fun.body, instance).into(),
                ret: instance.substitute(fun.ret.clone()),
                ann: fun.ann,
            },
            ty: instance.substitute(ty),
            kind,
            span,
        },

        Expr::Block { stmts, ty, span } => Expr::Block {
            stmts: stmts
                .iter()
                .map(|e| substitute_expr(e.clone(), instance))
                .collect(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Let {
            binding,
            value,
            ty,
            span,
        } => Expr::Let {
            binding: substitute_binding(&binding, instance),
            value: substitute_expr(*value, instance).into(),
            ty: instance.substitute(ty),
            span,
        },

        Expr::Var {
            value,
            decl,
            ty,
            span,
        } => Expr::Var {
            value,
            decl,
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

        Expr::StructAccess {
            expr,
            ty,
            field,
            span,
        } => Expr::StructAccess {
            expr: substitute_expr(*expr, instance).into(),
            ty: instance.substitute(ty),
            field,
            span,
        },

        Expr::VarUpdate { .. } => expr,

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

        Expr::ExternDecl {
            name,
            kind,
            items,
            span,
        } => Expr::ExternDecl {
            name,
            kind,
            items: items
                .iter()
                .map(|e| substitute_expr(e.clone(), instance))
                .collect(),
            span,
        },

        Expr::ImplBlock {
            ann,
            ty,
            generics,
            items,
            span,
        } => Expr::ImplBlock {
            ann,
            generics,
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

        Expr::Select { arms, ty, span } => Expr::Select {
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

        Expr::Loop {
            binding,
            expr,
            body,
            span,
        } => Expr::Loop {
            binding: substitute_binding(&binding, instance),
            expr: substitute_expr(*expr, instance).into(),
            body: substitute_expr(*body, instance).into(),
            span,
        },

        Expr::Unit { span } => Expr::Unit { span },
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
        Pat::Type { ident, ann, span } => Pat::Type { ident, ann, span },

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
        Pat::Unit { span } => Pat::Unit { span },
    }
}
