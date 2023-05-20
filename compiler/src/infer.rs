use crate::ast::{
    Arm, Binding, Constructor, EnumDefinition, EnumFieldDef, Expr, File, Function, FunctionKind,
    Literal, Loop, Operator, Pat, PkgImport, Span, StructDefinition, StructField, StructFieldDef,
    StructFieldPat, TypeAst, UnOp,
};
use crate::error::{ArityError, Error, UnificationError};
use crate::exhaustive;
use crate::global_state::{Declaration, FileId, GlobalState, Interface, Module, ValueDef};
use crate::project::Package;
use crate::substitute;
use crate::type_::{Bound, BoundedType, Type, TypeId};

use std::collections::{HashMap, HashSet};

pub struct Infer {
    pub gs: GlobalState,
    substitutions: HashMap<i32, Type>,

    // The return type of the current function being inferred
    current_fn_ret_ty: Option<Type>,

    // The file being processed
    current_file_id: Option<FileId>,

    // accumulate all errors encountered during inference
    errors: Vec<Error>,
}

impl Infer {
    pub fn new() -> Self {
        let mut gs = GlobalState::new();
        Self::init_builtin_types(&mut gs);

        Self {
            gs,
            substitutions: Default::default(),
            current_fn_ret_ty: None,
            current_file_id: None,
            errors: Default::default(),
        }
    }

    pub fn fresh_ty_var(&mut self) -> Type {
        Type::Var(self.gs.fresh_var())
    }

    fn add_constraint(&mut self, t1: &Type, t2: &Type, span: &Span) {
        // eprintln!("t1 {}, t2 {}", t1, t2);
        let res = self.unify(t1, t2, span);

        if let Err(msg) = res {
            let t1 = self.substitute(t1.clone());
            let t2 = self.substitute(t2.clone());
            let (types, _) = Type::remove_vars(vec![&t1, &t2]);

            let err = UnificationError {
                msg,
                expected: types[0].clone(),
                actual: types[1].clone(),
                span: span.clone(),
            };

            let err = Error::Unification(err);
            self.errors.push(err);
        }
    }

    pub fn dump(&self) {
        self.gs.dump();
        eprintln!("SUBS {:#?}", self.substitutions);
    }

    pub fn substitute(&mut self, typ: Type) -> Type {
        match typ {
            Type::Var(i) if self.sub(i) != typ => {
                let s = self.sub(i);
                self.substitute(s)
            }

            Type::Var(_) => typ,

            Type::Con { name, args } => {
                let new_args = args.into_iter().map(|a| self.substitute(a)).collect();
                Type::Con {
                    name,
                    args: new_args,
                }
            }

            Type::Fun {
                args,
                bounds,
                ret,
                id: fx,
            } => {
                let new_args = args.into_iter().map(|a| self.substitute(a)).collect();
                let new_bounds = bounds
                    .into_iter()
                    .map(|b| Bound {
                        generic: self.substitute(b.generic),
                        ty: self.substitute(b.ty),
                    })
                    .collect();
                let new_ret = self.substitute(*ret);

                Type::Fun {
                    args: new_args,
                    bounds: new_bounds,
                    ret: new_ret.into(),
                    id: fx,
                }
            }
        }
    }

    pub fn infer_expr(&mut self, expr: Expr, expected: &Type) -> Expr {
        match expr {
            Expr::Literal { lit, span, .. } => match lit {
                Literal::Bool(b) => {
                    self.add_constraint(expected, &Type::bool(), &span);

                    Expr::Literal {
                        lit: Literal::Bool(b),
                        ty: Type::bool(),
                        span,
                    }
                }

                Literal::Int(i) => {
                    self.add_constraint(expected, &Type::int(), &span);

                    Expr::Literal {
                        lit: Literal::Int(i),
                        ty: Type::int(),
                        span,
                    }
                }

                Literal::Float(i) => {
                    self.add_constraint(expected, &Type::float(), &span);

                    Expr::Literal {
                        lit: Literal::Float(i),
                        ty: Type::float(),
                        span,
                    }
                }

                Literal::String(s, token) => {
                    self.add_constraint(expected, &Type::string(), &span);

                    Expr::Literal {
                        lit: Literal::String(s, token),
                        ty: Type::string(),
                        span,
                    }
                }

                Literal::Char(s) => {
                    self.add_constraint(expected, &Type::char(), &span);

                    Expr::Literal {
                        lit: Literal::Char(s),
                        ty: Type::char(),
                        span,
                    }
                }

                Literal::Slice(elems) => {
                    let ty = self.fresh_ty_var();
                    let new_elems = elems.into_iter().map(|e| self.infer_expr(e, &ty)).collect();

                    let ty = Type::slice(ty);
                    self.add_constraint(expected, &ty, &span);

                    Expr::Literal {
                        lit: Literal::Slice(new_elems),
                        ty,
                        span,
                    }
                }
            },

            Expr::Block {
                ref stmts, span, ..
            } => {
                let mut stmts = stmts.to_vec();
                let was_empty = stmts.is_empty();

                // Take the last statement so we can run inference on it later
                let last = stmts
                    .pop()
                    .unwrap_or_else(|| Expr::Unit { span: span.clone() });

                stmts.iter().for_each(|e| self.declare_type(e));

                let mut new_stmts: Vec<_> = stmts
                    .iter()
                    .map(|e| {
                        let is_if_statement = matches!(e, Expr::If { .. });

                        let var = if is_if_statement {
                            Type::discard()
                        } else {
                            self.fresh_ty_var()
                        };

                        self.infer_expr(e.clone(), &var)
                    })
                    .collect();

                // Finally run inference on the last expression
                let new_last = self.infer_expr(last, expected);
                let last_ty = new_last.get_type();

                if !was_empty {
                    new_stmts.push(new_last);
                }

                Expr::Block {
                    stmts: new_stmts,
                    ty: last_ty,
                    span,
                }
            }

            Expr::Closure {
                ref fun,
                kind,
                span,
                ..
            } => {
                // NEW SCOPE
                self.gs.begin_scope();

                // Bring all generics in scope
                self.gs
                    .put_generics_in_scope(&fun.generics, self.new_declaration(&span));

                // If we're passed down a function, then we need to check the current closure
                // against it. This mostly applies to anonymous lambdas that are inlined into a
                // function call, ie. xs.reduce(|acc, item| item.bar)
                // How do we infer that `item` is a struct with a `bar` field? Without looking at
                // the expected type, we'd have no way of knowing and this will fail to infer.
                // So annotations on arguments take precedence, but then we look into the expected
                // argument flowing into to assign the correct type.

                let expected_args = expected.get_function_args().unwrap_or_default();

                let new_args: Vec<Binding> = fun
                    .args
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(index, binding)| {
                        // With no annotation, take the type of the argument flowing in.
                        let arg_ty_flowing_in = match binding.ann {
                            TypeAst::Unknown => expected_args.get(index).cloned(),
                            _ => None,
                        };

                        // Otherwise fallback to the annotation, or a fresh tyvar
                        let ty =
                            arg_ty_flowing_in.unwrap_or_else(|| self.to_type(&binding.ann, &span));

                        let new_pat = self.infer_pat(binding.pat, ty.clone());

                        Binding {
                            pat: new_pat,
                            ty,
                            ..binding
                        }
                    })
                    .collect();

                // TODO the return type from `expected` should also flow in
                let new_ret = self.to_type(&fun.ann, &span);

                let new_bounds: Vec<_> = fun
                    .bounds
                    .iter()
                    .map(|(g, ann)| Bound {
                        generic: Type::generic(g),
                        ty: self.to_type(ann, &span),
                    })
                    .collect();

                new_bounds.iter().for_each(|b| self.gs.add_assumption(b));

                // set return type as the current function being evaluated (used by Return branch)
                let prev_ret_ty = self.current_fn_ret_ty.clone(); // nested functions, save context
                self.current_fn_ret_ty = Some(new_ret.clone());

                let typ = Type::Fun {
                    args: new_args.iter().map(|b| b.ty.clone()).collect(),
                    bounds: new_bounds,
                    ret: new_ret.clone().into(),
                    id: TypeId::unset(),
                };

                // We don't actually care what the type of the last expression is, if the return
                // type is unit
                let body_ty = if new_ret == Type::unit() {
                    Type::discard()
                } else {
                    new_ret.clone()
                };

                let new_body = self.infer_expr(*fun.body.clone(), &body_ty);

                self.gs.exit_scope();
                // END NEW SCOPE

                self.current_fn_ret_ty = prev_ret_ty; // Reset current function

                let bounded_ty = typ.to_bounded_with_generics(fun.generics.to_owned());

                let typ = self.instantiate(&bounded_ty);

                self.add_constraint(expected, &typ, &span);

                Expr::Closure {
                    fun: Function {
                        name: fun.name.clone(),
                        generics: fun.generics.clone(),
                        bounds: fun.bounds.clone(),
                        args: new_args,
                        body: new_body.into(),
                        ret: new_ret,
                        ann: fun.ann.clone(),
                        bounded_ty,
                    },
                    ty: typ,
                    kind,
                    span,
                }
            }

            Expr::Let {
                binding,
                value,
                span,
                ty: _,
            } => {
                let ty = self.to_type(&binding.ann, &span);
                let new_value = self.infer_expr(*value, &ty);

                let new_binding = Binding {
                    pat: self.infer_pat(binding.pat, ty.clone()),
                    ty: ty.clone(),
                    ..binding
                };

                Expr::Let {
                    binding: new_binding,
                    value: new_value.into(),
                    ty,
                    span,
                }
            }

            Expr::Var {
                ref value, span, ..
            } => {
                let def = self.gs.get_value(value);

                let def = match def {
                    Some(def) => def,
                    None => {
                        self.generic_error(
                            format!("variable {} not found in env", value),
                            span.clone(),
                        );
                        ValueDef::dummy()
                    }
                };

                // Instantiate the type manually so we can collect the types and use them later for
                // codegen.
                let generics = self.collect_generics(&def.ty);
                let ty = self.instantiate_with_vars(&def.ty.ty, &generics);

                let mut instances: Vec<_> = generics.into_values().collect();
                instances.sort();
                let instances = instances.into_iter().map(|v| Type::Var(v)).collect();

                self.add_constraint(expected, &ty, &span);

                Expr::Var {
                    value: value.clone(),
                    decl: def.decl,
                    generics_instantiated: instances,
                    ty,
                    span,
                }
            }

            Expr::Call {
                func,
                args: call_args,
                span,
                ..
            } => {
                let ty = self.fresh_ty_var();
                let new_func = self.infer_expr(*func, &ty);

                let ty = new_func.get_type();
                let bounds = ty.get_bounds();
                let is_variadic = ty.is_variadic();

                let (args_ty, ret_ty) = match ty {
                    Type::Fun { mut args, ret, .. } => match is_variadic {
                        None => (args, *ret),

                        Some(variadic_ty) => {
                            //  We need at least (args - 1) arguments because the last one is
                            //  optional
                            let min_args_size = args.len() - 1;

                            // Remove the var arg
                            args.remove(min_args_size);

                            // At most, we need the arguments supplied by the caller
                            let max_args = std::cmp::max(min_args_size, call_args.len());

                            while args.len() < max_args {
                                // If there are more arguments than we have, then they must be of
                                // the variadic type
                                args.push(variadic_ty.clone());
                            }

                            (args, *ret)
                        }
                    },
                    _ => {
                        // we may get here without a proper function, ie. if a method did not exist
                        // manufacture a function on the fly and keep going
                        let args_ty = call_args.iter().map(|_| self.fresh_ty_var()).collect();
                        let ret = self.fresh_ty_var();
                        (args_ty, ret)
                    }
                };

                // match arguments
                let new_args: Vec<Expr> = call_args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let expected_arg_ty = args_ty
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| self.fresh_ty_var());
                        self.infer_expr(arg.clone(), &expected_arg_ty)
                    })
                    .collect();

                if args_ty.len() != new_args.len() {
                    let err = ArityError {
                        expected: args_ty,
                        actual: new_args.iter().map(|e| e.get_type()).collect(),
                        span: span.clone(),
                    };

                    self.errors.push(Error::WrongArity(err));
                }

                // match return type
                self.add_constraint(expected, &ret_ty, &span);

                // match constraint bounds
                for b in bounds.into_iter() {
                    let generic = self.substitute(b.generic);

                    // Only constraint the type if we know what it is.
                    // In certain situations, like when instantiating a map with Map::new()
                    // we don't know what K is yet, so there's no point constraining it that early.
                    // This is more inline with what Rust does, putting constraints on the single
                    // methods (ie. HashMap::insert) instead of on the whole type.

                    if generic.get_name().is_some() {
                        // TODO bounds should have a span too
                        self.add_constraint(&generic, &b.ty, &span);
                    }
                }

                Expr::Call {
                    func: new_func.into(),
                    args: new_args,
                    ty: ret_ty.clone(),
                    span,
                }
            }

            Expr::CheckType {
                expr, ann, span, ..
            } => {
                if ann == TypeAst::Unknown {
                    panic!("checking type without type?");
                }

                let expr_ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &expr_ty);

                let check_ty = self.to_type(&ann, &span);
                self.add_constraint(&expr_ty, &check_ty, &span);
                // self.add_constraint(expected, &check_ty, &span);

                Expr::CheckType {
                    expr: new_expr.into(),
                    ty: check_ty,
                    ann,
                    span,
                }
            }

            Expr::If {
                cond,
                then,
                els,
                span,
                ..
            } => {
                let then_ty = self.fresh_ty_var();
                let else_ty = self.fresh_ty_var();

                let has_else = match *els {
                    Expr::Block { ref stmts, .. } => !stmts.is_empty(),
                    _ => true,
                };

                let new_cond = self.infer_expr(*cond, &Type::bool());
                let new_then = self.infer_expr(*then, &then_ty);
                let new_els = self.infer_expr(*els, &else_ty);

                // Check if we are in discard mode (ie. block statements, loop bodies)
                let discard_mode = expected.is_discard();

                // In case we are not discarding, then we need an else block
                // and the types of the then and else branches must unify
                if !discard_mode {
                    if !has_else && expected != &Type::unit() {
                        self.generic_error(
                            "If expression must have an else branch".to_string(),
                            span.clone(),
                        );
                    }

                    self.add_constraint(&expected, &then_ty, &new_then.get_span());
                    self.add_constraint(&expected, &else_ty, &new_els.get_span());
                }

                Expr::If {
                    cond: new_cond.into(),
                    then: new_then.into(),
                    els: new_els.into(),
                    ty: then_ty,
                    span,
                }
            }

            Expr::Match {
                subject,
                arms,
                span,
                ..
            } => {
                let ret = self.fresh_ty_var();

                let subject_ty = self.fresh_ty_var();
                let new_subject = self.infer_expr(*subject, &subject_ty);

                // if it's a `match select!()`, then turn it into a Select expr
                if matches!(new_subject, Expr::Select { .. }) {
                    let select = Expr::Select {
                        arms,
                        span,
                        ty: Type::dummy(),
                    };
                    return self.infer_expr(select, expected);
                }

                let new_arms = arms
                    .into_iter()
                    .map(|a| {
                        // NEW SCOPE
                        self.gs.begin_scope();

                        // each arm.pat should unify with subject
                        let expected = self.substitute(subject_ty.clone());
                        let new_pat = self.infer_pat(a.pat, expected);

                        // each arm.expr should unify with ret
                        let new_expr = self.infer_expr(a.expr, &ret);

                        // EXIT SCOPE
                        self.gs.exit_scope();

                        Arm {
                            pat: new_pat,
                            expr: new_expr,
                        }
                    })
                    .collect();

                self.add_constraint(expected, &ret, &span);

                Expr::Match {
                    subject: new_subject.into(),
                    arms: new_arms,
                    ty: ret,
                    span,
                }
            }

            Expr::Unit { span } => {
                self.add_constraint(expected, &Type::unit(), &span);
                Expr::Unit { span }
            }

            Expr::Tuple { elems, span, .. } => {
                // Tuples really are structs, with fields `first`, `second`

                let name = format!("Tuple{}", elems.len());
                let fields = elems
                    .into_iter()
                    .enumerate()
                    .map(|(index, f)| StructField {
                        name: Expr::tuple_index_string(index.try_into().unwrap()).expect(
                            "ok to unwrap here as the parser should have caught invalid indexes",
                        ),
                        value: f,
                    })
                    .collect();

                let new_expr = Expr::StructCall {
                    name,
                    fields,
                    rest: None.into(),
                    ty: Type::dummy(),
                    span,
                };

                self.infer_expr(new_expr, expected)
            }

            // Note: for enums and structs, the declaration is actually redundant.
            // But tests that only deal with expressions still need this logic.
            // Plus this enables users to define a type in a block and use it locally.
            Expr::EnumDef { ref def, ref span } => {
                self.declare_type(&expr);
                self.declare_variants(&expr);

                let actual_def = self.gs.get_enum(&def.name).unwrap();
                Expr::EnumDef {
                    def: actual_def,
                    span: span.clone(),
                }
            }

            Expr::StructDef { ref def, ref span } => {
                self.declare_type(&expr);
                self.declare_variants(&expr);

                let actual_def = self.gs.get_struct(&def.name).unwrap();
                Expr::StructDef {
                    def: actual_def,
                    span: span.clone(),
                }
            }

            Expr::StructCall {
                ref name,
                ref fields,
                ref rest,
                ty: _,
                ref span,
            } => {
                let (def, ty) = match self.get_struct_by_name(name) {
                    Some(ret) => ret,
                    None => {
                        let msg = format!("struct not found {}", name);
                        self.generic_error(msg, span.clone());
                        return expr.clone();
                    }
                };

                let mut matched = HashSet::new();

                // Instantiate the type manually, so that we can use the same vars for fields too.
                let generics = self.collect_generics(&ty);
                let instantiated_ty = self.instantiate_with_vars(&ty.ty, &generics);

                // If I'm spreading in another struct (ie. { a: 1, ..x })
                // then ensure the types match
                let new_rest = match *rest.clone() {
                    Some(r) => {
                        let expr = self.infer_expr(r, &instantiated_ty);
                        Some(expr)
                    }
                    None => None,
                };

                // let typed_fields = self.get_typed_struct_fields(&def, ty);

                // match each field with the definition
                let new_fields = fields
                    .iter()
                    .map(|f| {
                        let found = def.fields.iter().find(|x| x.name == f.name);

                        let target = match found {
                            Some(found) => {
                                matched.insert(f.name.clone());
                                self.instantiate_with_vars(&found.ty.ty, &generics)
                            }
                            None => {
                                self.generic_error(
                                    format!("field not found: `{}`", f.name),
                                    span.clone(),
                                );
                                self.fresh_ty_var()
                            }
                        };

                        let new_value = self.infer_expr(f.value.clone(), &target);
                        StructField {
                            name: f.name.clone(),
                            value: new_value,
                        }
                    })
                    .collect();

                if matched.len() != def.fields.len() && rest.is_none() {
                    let field_names: HashSet<_> =
                        def.fields.iter().map(|f| f.name.clone()).collect();

                    let expected = field_names.difference(&matched);
                    let mut expected: Vec<_> = expected.into_iter().collect();
                    expected.sort();

                    let msg = format!("Missing fields:\n{:#?}", expected);
                    self.generic_error(format!("Wrong arguments \n{}", msg), span.clone());
                }

                self.add_constraint(expected, &instantiated_ty, span);

                Expr::StructCall {
                    name: name.clone(),
                    fields: new_fields,
                    rest: new_rest.into(),
                    ty: instantiated_ty,
                    span: span.clone(),
                }
            }

            Expr::FieldAccess {
                expr,
                field,
                span,
                ty: _,
            } => {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);
                let ty = self.substitute(ty);

                // 1. check if it's a struct
                //      -> yes, check for field
                // 2. If fails, Lookup method

                let def = ty
                    .remove_references()
                    .get_name()
                    .and_then(|name| self.get_struct_by_name(&name));

                if let Some((def, struct_ty)) = def {
                    // Lookup the field
                    let field_ty = def.fields.iter().find_map(|f| {
                        if f.name == field {
                            Some(f.ty.clone())
                        } else {
                            None
                        }
                    });

                    if let Some(field_ty) = field_ty {
                        // Collect all the generics on both the struct and field
                        let mut generics = struct_ty.generics.clone();
                        generics.extend_from_slice(&field_ty.generics);
                        let generics = self.collect_generics_as_vars(&generics);

                        // Instantiate the struct again, so we can get a fresh mapping of generics.
                        let instantiated = self.instantiate_with_vars(&struct_ty.ty, &generics);

                        // The type in the field isn't instantiated yet, so we can use the same
                        // vars we used to instantiate the struct to get all the types to line up.
                        let new_field_ty = self.instantiate_with_vars(&field_ty.ty, &generics);

                        // Constraint the instantiated struct to the expr we inferred before.
                        self.add_constraint(&ty.remove_references(), &instantiated, &span);

                        self.add_constraint(expected, &new_field_ty, &span);

                        return Expr::FieldAccess {
                            expr: new_expr.into(),
                            field,
                            ty: new_field_ty,
                            span,
                        };
                    }
                }

                // TODO asdf this should return Ok | expected Ref<T>, RefMut<T> or something
                let method = self.gs.get_method(&ty, &field);

                if let Some(method) = method {
                    let mut func_ty = self.instantiate(&method);

                    let func_args = match func_ty {
                        Type::Fun { ref mut args, .. } => args,
                        _ => unreachable!(),
                    };

                    // Unify the receiver, then drop it so that the resulting type only has the
                    // rest of the args
                    let receiver = func_args.remove(0);

                    self.add_constraint(&receiver, &ty, &span);

                    return Expr::FieldAccess {
                        expr: new_expr.into(),
                        field,
                        ty: func_ty,
                        span,
                    };
                };

                self.generic_error(
                    format!(
                        "Type:
    {}
has no field or method:
    {}",
                        &ty, &field,
                    ),
                    span.clone(),
                );

                Expr::FieldAccess {
                    expr: new_expr.into(),
                    field,
                    ty: self.fresh_ty_var(),
                    span,
                }
            }

            Expr::VarUpdate {
                target,
                value,
                span,
            } => {
                let target_ty = self.fresh_ty_var();
                let new_target = self.infer_expr(*target.clone(), &target_ty);

                let value_ty = self.fresh_ty_var();
                let new_value = self.infer_expr(*value.clone(), &value_ty);

                if let Some(var_name) = new_target.as_var_name() {
                    // This is not really correct, we should check whether the target var is a RefMut.
                    let is_deref = matches!(new_target, Expr::Unary { .. });
                    if !self.gs.get_mutability(&var_name) && !is_deref {
                        self.generic_error(
                        format!("Variable {var_name} is not declared as mutable. Use `let mut {var_name}`.")
                            ,
                        span.clone(),
                    );
                    }
                } else {
                    self.generic_error(
                        "Assign operator is not supported here".to_string(),
                        span.clone(),
                    );
                }

                // Constraint left = right
                self.add_constraint(&target_ty, &value_ty, &span);

                Expr::VarUpdate {
                    target: new_target.into(),
                    value: new_value.into(),
                    span: span.clone(),
                }
            }

            Expr::MethodCall { .. } => {
                panic!("method calls should have been desugared to FieldAccess during parsing. This is not currently used")
            }

            Expr::Return { expr, span, .. } => {
                let current_fn_ret_ty =
                    self.current_fn_ret_ty.as_ref().cloned().unwrap_or_else(|| {
                        self.generic_error(
                            "return statements can only be used within functions.".to_string(),
                            span.clone(),
                        );
                        self.fresh_ty_var()
                    });

                let new_expr = self.infer_expr(*expr, &current_fn_ret_ty);
                let ty = new_expr.get_type();

                Expr::Return {
                    expr: new_expr.into(),
                    ty,
                    span,
                }
            }

            Expr::Try { expr, span, .. } => {
                // constraint the expression to be a Result
                let ok_ty = self.fresh_ty_var();
                let err_ty = self.fresh_ty_var();
                let inferred = Type::result(&ok_ty, &err_ty);
                let new_expr = self.infer_expr(*expr, &inferred);

                // constraint the function we're currently processing to be a Result
                // with the same error type
                let fn_ret = self.current_fn_ret_ty.clone().unwrap();
                let fn_expected = Type::result(&self.fresh_ty_var(), &err_ty);

                // TODO we need the span of the return type in the function signature,
                // so that the error points at the right location
                self.add_constraint(&fn_expected, &fn_ret, &span);

                self.add_constraint(expected, &ok_ty, &span);

                Expr::Try {
                    expr: new_expr.into(),
                    ty: ok_ty,
                    span,
                }
            }

            Expr::Binary {
                op,
                left,
                right,
                span,
                ..
            } => {
                let left_ty = self.fresh_ty_var();
                let right_ty = self.fresh_ty_var();

                let new_left = self.infer_expr(*left, &left_ty);
                let new_right = self.infer_expr(*right, &right_ty);

                // Checking the type of operands here is a tad complex.
                // For now, rely on Go compiler to do the right thing.

                let target = match &op {
                    Operator::Eq
                    | Operator::Ne
                    | Operator::Lt
                    | Operator::Le
                    | Operator::Gt
                    | Operator::Ge => Type::bool(),

                    _ => left_ty.clone(),
                };

                self.add_constraint(expected, &target, &span);

                match op {
                    Operator::And | Operator::Or => {
                        self.add_constraint(&left_ty, &Type::bool(), &span);
                        self.add_constraint(&right_ty, &Type::bool(), &span);
                    }

                    Operator::Eq | Operator::Ne => {
                        self.add_constraint(&left_ty, &right_ty, &span);
                    }

                    // allow strings to be used with + operator
                    Operator::Add
                        if (new_left.get_type().is_string()
                            || new_right.get_type().is_string()) =>
                    {
                        self.add_constraint(&left_ty, &right_ty, &span);
                    }

                    // in all other cases it must be a numeric type
                    _ => {
                        self.check_numeric(&left_ty, &span);
                        self.check_numeric(&right_ty, &span);
                    }
                }

                Expr::Binary {
                    op,
                    left: new_left.into(),
                    right: new_right.into(),
                    ty: target,
                    span,
                }
            }

            Expr::Paren { expr, span, .. } => {
                let new_expr = self.infer_expr(*expr, expected);
                let new_ty = new_expr.get_type();

                Expr::Paren {
                    expr: new_expr.into(),
                    ty: new_ty,
                    span,
                }
            }

            Expr::Unary { op, expr, span, .. } => {
                let new_ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &new_ty);

                let target = match op {
                    UnOp::Neg => {
                        self.check_numeric(&new_ty, &span);
                        new_ty.clone()
                    }

                    UnOp::Not => Type::bool(),
                    UnOp::Deref => self.fresh_ty_var(),
                };

                self.add_constraint(&target, &new_ty, &new_expr.get_span());
                self.add_constraint(expected, &new_ty, &span);

                Expr::Unary {
                    op,
                    expr: new_expr.into(),
                    ty: new_ty,
                    span,
                }
            }

            Expr::Const {
                ann,
                ty: _,
                expr,
                span,
                ident,
            } => {
                let target = self.gs.get_value(&ident).unwrap().ty;
                let target = self.instantiate(&target);

                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);

                self.add_constraint(&target, &ty, &span);

                Expr::Const {
                    ident,
                    expr: new_expr.into(),
                    ann,
                    ty,
                    span,
                }
            }

            Expr::Debug {
                kind,
                expr,
                ty: _,
                span,
            } => {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);

                self.add_constraint(expected, &ty, &span);

                Expr::Debug {
                    kind,
                    expr: new_expr.into(),
                    ty,
                    span,
                }
            }

            Expr::Spawn { expr, span, .. } => {
                if !matches!(*expr, Expr::Call { .. }) {
                    self.generic_error(
                        "Argument to spawn!() must be a function call".to_string(),
                        span.clone(),
                    );
                }

                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);
                self.add_constraint(expected, &Type::unit(), &span);

                Expr::Spawn {
                    expr: new_expr.into(),
                    ty: expected.clone(),
                    span,
                }
            }

            Expr::Select { arms, ty: _, span } => {
                let ret = Type::unit();

                // Each arm is inferred separately
                // Arm pat must be of type ChannelOp
                // Return type is unit
                let new_arms = arms
                    .into_iter()
                    .map(|a| {
                        // NEW SCOPE
                        self.gs.begin_scope();

                        let var = self.fresh_ty_var();
                        let channel_op = Type::Con {
                            name: "ChannelOp".to_string(),
                            args: vec![var],
                        };

                        // each arm.pat should unify with ChannelOp
                        let new_pat = self.infer_pat(a.pat, channel_op);

                        // each arm.expr should unify with ret
                        let new_expr = self.infer_expr(a.expr, &ret);

                        // EXIT SCOPE
                        self.gs.exit_scope();

                        Arm {
                            pat: new_pat,
                            expr: new_expr,
                        }
                    })
                    .collect();

                self.add_constraint(expected, &ret, &span);

                Expr::Select {
                    arms: new_arms,
                    ty: ret,
                    span,
                }
            }

            Expr::Defer { expr, span, .. } => {
                if !matches!(*expr, Expr::Call { .. }) {
                    self.generic_error(
                        "Argument to defer!() must be a function call".to_string(),
                        span.clone(),
                    );
                }

                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);
                self.add_constraint(expected, &Type::unit(), &span);

                Expr::Defer {
                    expr: new_expr.into(),
                    ty: expected.clone(),
                    span,
                }
            }

            Expr::Reference {
                expr,
                mutable,
                span,
                ..
            } => {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);

                let wrapped = Type::reference(ty);
                self.add_constraint(expected, &wrapped, &span);

                Expr::Reference {
                    expr: new_expr.into(),
                    mutable,
                    ty: wrapped,
                    span,
                }
            }

            Expr::Index {
                expr, index, span, ..
            } => {
                let expr_ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &expr_ty);

                let index_ty = self.fresh_ty_var();
                let new_index = self.infer_expr(*index, &index_ty);
                let expr_ty = self.substitute(expr_ty);

                let ty_name = match expr_ty.get_name() {
                    Some(name) => name,
                    None => {
                        self.generic_error(
                            "Type must be known at this point".to_string(),
                            new_expr.get_span(),
                        );
                        "Slice".to_string()
                    }
                };

                let inner_ty = self.fresh_ty_var();

                let (expected_index, collection_ty) = if ty_name == "Slice" {
                    // slices can be indexed by ints
                    (Type::int(), Type::slice(inner_ty.clone()))
                } else if ty_name == "Map" {
                    // maps can be indexed by whatever the key type is
                    let key_ty = &expr_ty.get_args().unwrap()[0];

                    (
                        key_ty.clone(),
                        Type::Con {
                            name: "Map".to_string(),
                            args: vec![key_ty.clone(), inner_ty.clone()],
                        },
                    )
                } else {
                    self.generic_error(
                        "Only slices and maps can be indexed".to_string(),
                        new_expr.get_span(),
                    );
                    (self.fresh_ty_var(), self.fresh_ty_var())
                };

                self.add_constraint(&collection_ty, &expr_ty, &span);
                self.add_constraint(&index_ty, &expected_index, &span);
                self.add_constraint(expected, &inner_ty, &span);

                Expr::Index {
                    expr: new_expr.into(),
                    index: new_index.into(),
                    ty: inner_ty,
                    span,
                }
            }

            Expr::ImplBlock {
                ann,
                ty: _,
                items,
                generics,
                span,
            } => {
                self.gs.begin_scope();

                self.gs
                    .put_generics_in_scope(&generics, self.new_declaration(&span));

                let existing = self.to_type(&ann, &span);

                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                self.gs.exit_scope();

                Expr::ImplBlock {
                    ann,
                    ty: existing,
                    items: new_items,
                    generics,
                    span,
                }
            }

            // Both this and Trait are sort of redundant.
            // Checks are already performed when declaring types, but this is still needed to
            // update the node in the tree. A better way would be to keep around the result of
            // declare_type
            Expr::ExternDecl { items, span } => {
                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                Expr::ExternDecl {
                    items: new_items,
                    span,
                }
            }

            Expr::Trait {
                name,
                items,
                supertraits,
                types,
                span,
            } => {
                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                Expr::Trait {
                    name,
                    items: new_items,
                    supertraits,
                    types,
                    span,
                }
            }

            Expr::Mod {
                name,
                pkg,
                items,
                span,
            } => {
                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                Expr::Mod {
                    name,
                    pkg,
                    items: new_items,
                    span,
                }
            }

            Expr::Loop { kind, body, span } => {
                match kind {
                    Loop::NoCondition => {
                        let body_ty = Type::discard();
                        let new_body = self.infer_expr(*body, &body_ty);

                        return Expr::Loop {
                            kind,
                            body: new_body.into(),
                            span,
                        };
                    }

                    //
                    // for $binding in $expr { $body }
                    // $expr should be Slice, Map, Channel
                    //
                    Loop::WithCondition { binding, expr } => {
                        let binding_ty = self.to_type(&binding.ann, &span);

                        let new_binding = Binding {
                            pat: self.infer_pat(binding.pat, binding_ty.clone()),
                            ty: binding_ty.clone(),
                            ..binding
                        };

                        let expr_ty = self.fresh_ty_var();
                        let new_expr = self.infer_expr(*expr, &expr_ty);

                        let expr_ty = self.substitute(expr_ty);

                        let ty_name = match expr_ty.get_name() {
                            Some(name) => name,
                            None => {
                                self.generic_error(
                                    "Type must be known at this point".to_string(),
                                    new_expr.get_span(),
                                );
                                "Slice".to_string()
                            }
                        };

                        let expr_args = expr_ty
                            .get_args()
                            .unwrap_or_else(|| vec![self.fresh_ty_var(), self.fresh_ty_var()]);

                        let range_ty = match ty_name.as_str() {
                            "Slice" => expr_args[0].clone(),

                            "EnumerateSlice" => {
                                // (index, value) for slices with .enumerate()
                                Type::tuple2(Type::int(), expr_args[0].clone())
                            }

                            "Map" => {
                                // (key, value) for maps
                                Type::tuple2(expr_args[0].clone(), expr_args[1].clone())
                            }

                            "Receiver" => expr_args[0].clone(),

                            "string" => Type::Con {
                                name: "rune".to_string(),
                                args: vec![],
                            },

                            _ => {
                                self.generic_error(
                                    format!("Can't iterate on type {expr_ty}"),
                                    new_expr.get_span(),
                                );

                                self.fresh_ty_var()
                            }
                        };

                        // If range_ty is a tuple, then the binding must be a literal tuple.
                        // This is because during codegen we're not actually allocating a Tuple,
                        // but directly matching "k, v := range ..."

                        if range_ty.get_name() == Some("Tuple2".to_string()) {
                            match &new_binding.pat {
                                Pat::Struct { .. } => (),
                                Pat::Wild { .. } => (),
                                _ => {
                                    self.generic_error(
                                        format!("Use tuple literals \"(k, v)\" in loops."),
                                        span.clone(),
                                    );
                                }
                            }
                        }

                        self.add_constraint(&range_ty, &binding_ty, &new_expr.get_span());

                        // Inference for body must run after constraining the binding, otherwise there will
                        // be a loose type variable that won't unify.
                        let body_ty = Type::discard();
                        let new_body = self.infer_expr(*body, &body_ty);

                        Expr::Loop {
                            kind: Loop::WithCondition {
                                binding: new_binding,
                                expr: new_expr.into(),
                            },
                            body: new_body.into(),
                            span,
                        }
                    }

                    //
                    // while $expr { $body }
                    //
                    Loop::While { expr } => {
                        let new_expr = self.infer_expr(*expr, &Type::bool());
                        let body_ty = Type::discard();
                        let new_body = self.infer_expr(*body, &body_ty);

                        Expr::Loop {
                            kind: Loop::While {
                                expr: new_expr.into(),
                            },
                            body: new_body.into(),
                            span,
                        }
                    }
                }
            }

            Expr::Flow { kind, span } => Expr::Flow { kind, span },

            Expr::TypeAlias { def, span } => Expr::TypeAlias { def, span },
            Expr::UsePackage { import, span } => Expr::UsePackage { import, span },

            Expr::Raw { text } => Expr::Raw { text },
            Expr::Noop => Expr::Noop,
            Expr::Todo => todo!(),
            // _ => todo!("{:#?}", expr),
        }
    }

    pub fn collect_generics(&mut self, typ: &BoundedType) -> HashMap<String, i32> {
        self.collect_generics_as_vars(&typ.generics)
    }

    pub fn collect_generics_as_vars(&mut self, generics: &[String]) -> HashMap<String, i32> {
        generics
            .iter()
            .map(|g| (g.to_string(), self.gs.fresh_var()))
            .collect()
    }

    /// Instantiate all generics to unbound variables.
    /// f(x: T, b: Y) -> Y
    /// f(x: 0, b: 1) -> 1
    fn instantiate(&mut self, typ: &BoundedType) -> Type {
        // create a mapping generic -> var
        let new_vars = self.collect_generics(typ);
        self.instantiate_with_vars(&typ.ty, &new_vars)
    }

    fn instantiate_with_vars(&mut self, typ: &Type, new_vars: &HashMap<String, i32>) -> Type {
        match typ {
            Type::Var(_) => typ.to_owned(),

            Type::Con { name, args } => match new_vars.get(name) {
                Some(id) => Type::Var(id.to_owned()),
                None => Type::Con {
                    name: name.into(),
                    args: args
                        .iter()
                        .map(|x| self.instantiate_with_vars(x, new_vars))
                        .collect(),
                },
            },

            Type::Fun {
                args,
                bounds,
                ret,
                id: fx,
            } => Type::Fun {
                args: args
                    .iter()
                    .map(|x| self.instantiate_with_vars(x, new_vars))
                    .collect(),
                bounds: bounds
                    .iter()
                    .map(|b| Bound {
                        generic: self.instantiate_with_vars(&b.generic, new_vars),
                        ty: self.instantiate_with_vars(&b.ty, new_vars),
                    })
                    .collect(),
                ret: self.instantiate_with_vars(ret, new_vars).into(),
                id: fx.clone(),
            },
        }
    }

    fn infer_pat(&mut self, pat: Pat, expected: Type) -> Pat {
        match pat {
            Pat::Type {
                ident,
                is_mut,
                ann,
                span,
            } => {
                // Check that the name isn't reserved (like `default`)
                if self.check_reserved_name(&ident) {
                    self.generic_error(format!("Name {ident} is reserved"), span.clone());
                    return Pat::Wild { span };
                }

                // Add ident to the scope
                self.gs.add_value(
                    ident.clone(),
                    expected.to_bounded(),
                    self.new_declaration(&span),
                );

                self.gs.set_mutability(&ident, is_mut);

                Pat::Type {
                    ident,
                    is_mut,
                    ann,
                    span,
                }
            }

            Pat::Lit {
                lit,
                ty: lit_ty,
                span,
            } => {
                let expr = Expr::Literal {
                    lit,
                    ty: lit_ty,
                    span,
                };

                let lit = self.infer_expr(expr, &expected);
                match lit {
                    Expr::Literal { lit, ty, span } => Pat::Lit { lit, ty, span },
                    _ => unreachable!(),
                }
            }

            Pat::Pat {
                ref ident,
                ref elems,
                ref span,
                ..
            } => {
                // If it's a Pat, then we should be able to find a function with this name
                let def = match self.gs.get_value(ident) {
                    Some(def) => def.ty,
                    None => {
                        self.generic_error(
                            format!("constructor {} not found", &ident),
                            span.clone(),
                        );
                        return pat.clone();
                    }
                };

                let def = self.instantiate(&def);

                let (args, ty) = match def {
                    Type::Fun { args, ret, .. } => (args, *ret),
                    Type::Con { .. } => (vec![], def),
                    _ => unreachable!(),
                };

                self.add_constraint(&expected, &ty, span);

                let new_elems: Vec<_> = elems
                    .iter()
                    .enumerate()
                    .map(|(index, e)| {
                        let expected_arg_ty = args
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| self.fresh_ty_var());
                        self.infer_pat(e.clone(), expected_arg_ty)
                    })
                    .collect();

                if args.len() != new_elems.len() {
                    let err = ArityError {
                        expected: args,
                        actual: new_elems
                            .iter()
                            .map(|e| e.get_type().unwrap_or_else(|| self.fresh_ty_var()))
                            .collect(),
                        span: span.clone(),
                    };

                    self.errors.push(Error::WrongArity(err));
                }

                // If the source code contains an alias, make sure we resolve it to the qualified
                // name. ie. Red -> Color::Red This helps with exhaustiveness checking later.
                let new_ident = self.gs.resolve_name(ident);
                Pat::Pat {
                    ident: new_ident,
                    elems: new_elems,
                    ty,
                    span: span.clone(),
                }
            }

            Pat::Struct {
                ref ident,
                ref fields,
                ref span,
                ..
            } => {
                let (def, ty) = match self.get_struct_by_name(ident) {
                    Some(ret) => ret,
                    None => {
                        let msg = format!("struct not found {}", ident);
                        self.generic_error(msg, span.clone());
                        return pat.clone();
                    }
                };

                let generics = self.collect_generics(&ty);
                let instantiated_ty = self.instantiate_with_vars(&ty.ty, &generics);
                self.add_constraint(&expected, &instantiated_ty, span);

                // match each field with the definition
                let new_fields = fields
                    .iter()
                    .map(|f| {
                        let found = def.fields.iter().find(|x| x.name == f.name);

                        let target = match found {
                            Some(found) => self.instantiate_with_vars(&found.ty.ty, &generics),
                            None => {
                                self.generic_error(
                                    format!("field not found: `{}`", f.name),
                                    span.clone(),
                                );
                                self.fresh_ty_var()
                            }
                        };

                        let new_value = self.infer_pat(f.value.clone(), target);

                        StructFieldPat {
                            name: f.name.clone(),
                            value: new_value,
                        }
                    })
                    .collect();

                let new_ident = self.gs.resolve_name(ident);

                Pat::Struct {
                    ident: new_ident,
                    fields: new_fields,
                    ty: instantiated_ty,
                    span: span.clone(),
                }
            }

            Pat::Wild { span } => Pat::Wild { span },
            Pat::Unit { span } => {
                self.add_constraint(&expected, &Type::unit(), &span);
                Pat::Unit { span }
            }
        }
    }

    pub fn infer_package(&mut self, pkg: &Package) -> Package {
        self.declare_symbols(&pkg.files);
        self.declare_modules(&pkg.files);
        self.declare_files(&pkg.files);

        // this is a hack, see below
        let had_errors = self.errors.clone();

        let mut errors = pkg.errors.clone();

        let files: Vec<_> = pkg
            .files
            .iter()
            .map(|f| {
                let (new_file, errs) = self.infer_file(pkg, f);
                errors.entry(f.name.clone()).or_default().extend(errs);
                new_file
            })
            .collect();

        files.iter().for_each(|f| {
            f.decls.iter().for_each(|expr| {
                if let Err(e) = exhaustive::check(expr, self) {
                    errors.entry(f.name.clone()).or_default().push(e);
                }
            })
        });

        if errors.is_empty() && !had_errors.is_empty() {
            // TODO asdf we don't have information about the file that triggered the error here...
            // so the best we can do is to wrap up the error in a random file and call it a day :/
            // this is obviously wrong
            errors.insert(pkg.files.first().unwrap().name.to_string(), had_errors);
        }

        Package {
            name: pkg.name.clone(),
            files,
            errors,
        }
    }

    pub fn infer_file(&mut self, pkg: &Package, file: &File) -> (File, Vec<Error>) {
        self.current_file_id = Some(FileId::create(pkg, file));
        self.errors = vec![];

        let new_decls = file
            .decls
            .iter()
            .map(|e| {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(e.clone(), &ty);
                substitute::substitute_expr(new_expr, self)
            })
            .collect();

        let errors = self.errors.clone();
        self.errors = vec![];

        (
            File {
                name: file.name.clone(),
                decls: new_decls,
                source: file.source.clone(),
            },
            errors,
        )
    }

    /// Prepare a File for inference. Run this with a list of files before running inference.
    /// All types and functions will be in scope.
    pub fn declare_files(&mut self, files: &[File]) {
        // This seems a bit insane, and perhaps it is.
        // BUT:
        //   - First put enum and struct names in scope
        //   - Second declare all the variants for the types we just processed
        //   - Third declare all overloads, because they are used in trait bounds
        //   - Fourth derive implementations for known overloads (eq, hash, display).
        //   - Fifth bring everything else in scope (top level fn, impls, consts)
        //   - Finally declare all enum/struct variants
        //
        //  It's important that all these steps stay separated, otherwise stuff will break
        //  depending on the order of declarations.

        // 1 Process imports, get external modules in scope
        files.iter().for_each(|f| {
            for e in &f.decls {
                if let Expr::UsePackage { import, span } = e {
                    self.import_package(import, span);
                }
            }
        });

        // 2. Declare all the actual variants
        files.iter().for_each(|f| {
            // Now that all types are in scope, actually parse the enum variants and struct fields.
            f.decls.iter().for_each(|e| self.declare_variants(e));
        });

        // 3. Declare Closure, Impl, Extern and Const
        files.iter().for_each(|f| {
            f.decls.iter().for_each(|e| self.declare_type(e));
        });
    }

    fn declare_type(&mut self, e: &Expr) {
        let create_func = |fun: &Function, span: &Span| -> Expr {
            let mut new_func = fun.clone();
            new_func.body = Expr::Noop.into();

            Expr::Closure {
                fun: new_func,
                kind: FunctionKind::TopLevel,
                ty: Type::dummy(),
                span: span.clone(),
            }
        };

        match e {
            Expr::EnumDef { def, span, .. } => {
                let ty = Type::Con {
                    name: def.name.clone(),
                    args: def.generics.iter().map(|g| Type::generic(g)).collect(),
                };

                self.gs.add_type(
                    def.name.clone(),
                    ty.to_bounded_with_generics(def.generics.to_owned()),
                    self.new_declaration(span),
                );
            }

            Expr::StructDef { def, span, .. } => {
                let ty = Type::Con {
                    name: def.name.clone(),
                    args: def.generics.iter().map(|g| Type::generic(g)).collect(),
                };

                self.gs.add_type(
                    def.name.clone(),
                    ty.to_bounded_with_generics(def.generics.to_owned()),
                    self.new_declaration(span),
                );
            }

            Expr::TypeAlias { def, span, .. } => {
                let is_native = def.ann == TypeAst::unit();

                if is_native {
                    let ty = Type::Con {
                        name: def.name.clone(),
                        args: def.generics.iter().map(|g| Type::generic(g)).collect(),
                    };

                    self.gs.add_type(
                        def.name.clone(),
                        ty.to_bounded_with_generics(def.generics.to_owned()),
                        self.new_declaration(span),
                    );

                    return;
                }

                todo!("figure out what to do with type aliases");
            }

            Expr::Closure { fun, span, .. } => {
                let ty = self.fresh_ty_var();

                let new_expr = self.infer_expr(create_func(fun, span), &ty);
                let new_func = new_expr.as_function();

                self.gs.add_value(
                    new_func.name.clone(),
                    new_func.bounded_ty.clone(),
                    self.new_declaration(&span),
                );
            }

            Expr::ImplBlock {
                ann,
                generics,
                items,
                span,
                ..
            } => {
                items.iter().for_each(|e| {
                    self.gs.begin_scope();

                    self.gs
                        .put_generics_in_scope(&generics, self.new_declaration(&span));

                    let base_ty = self.to_type(&ann, &span);

                    let decl = self.new_declaration(&e.get_span());
                    let ty = self.fresh_ty_var();

                    let fun = e.as_function();
                    let new_expr = self.infer_expr(create_func(&fun, span), &ty);
                    let new_func = new_expr.as_function();
                    let method = new_func.as_method();

                    self.gs.exit_scope();

                    match method {
                        Some((method, _)) => {
                            // Add method to type

                            self.gs
                                .add_method(&base_ty, &method.name, method.bounded_ty, decl);
                        }

                        None => {
                            // Add static function to global scope

                            self.gs.add_value(
                                new_func.name,
                                new_func.bounded_ty.clone(),
                                self.new_declaration(&span),
                            );
                        }
                    };
                });
            }

            Expr::Const {
                ident, ann, span, ..
            } => {
                match &ann {
                    TypeAst::Unknown => panic!("checking type without type?"),
                    _ => (),
                }

                let ty = self.to_type(ann, span);
                self.gs
                    .add_value(ident.clone(), ty.to_bounded(), self.new_declaration(span));
            }

            Expr::ExternDecl { items, span, .. } => {
                for e in items {
                    let ty = self.fresh_ty_var();

                    let fun = e.as_function();
                    let new_expr = self.infer_expr(create_func(&fun, span), &ty);
                    let new_func = new_expr.as_function();

                    self.gs.add_value(
                        new_func.name.clone(),
                        new_func.bounded_ty.clone(),
                        self.new_declaration(&span),
                    );
                }
            }

            Expr::Trait {
                name,
                items,
                supertraits,
                types,
                span,
            } => {
                // create a struct that holds one field for each fn
                // This is similar to what ExternDecl does
                let struct_name = name;

                let decl = self.new_declaration(&span);

                let base_ty = Type::Con {
                    name: struct_name.clone(),
                    args: vec![],
                };

                let mut methods: HashMap<String, Type> = HashMap::new();

                // Include all the methods from supertraits
                for s in supertraits {
                    let super_methods = self.gs.get_type_methods(s);

                    for (name, mut method) in super_methods {
                        method.ty.ty.remove_receiver();
                        methods.insert(name, method.ty.ty);
                    }
                }

                for e in items {
                    let fun = e.as_function();

                    let ty = self.fresh_ty_var();
                    let expr = self.infer_expr(e.clone(), &ty);

                    methods.insert(fun.name, expr.get_type());
                }

                self.gs
                    .add_type(struct_name.to_string(), base_ty.to_bounded(), decl.clone());

                self.gs.add_interface(Interface {
                    name: struct_name.to_string(),
                    types: types.to_vec(),
                    methods,
                });
            }

            Expr::Mod { items, .. } => {
                for i in items {
                    self.declare_type(i);
                }
            }

            _ => (),
        }
    }

    fn declare_variants(&mut self, expr: &Expr) {
        match expr {
            Expr::EnumDef { def, span, .. } => {
                let ty = self.gs.get_type(&def.name).unwrap().ty;

                let new_cons = def
                    .cons
                    .iter()
                    .map(|c| {
                        let name = c.name.clone();
                        let qualified = c.to_qualified(&def.name);

                        // Alias each constructor to its full qualified name
                        // Might be a bad idea in the long run.
                        // When this changes, also update eval.ts EnumDef
                        self.gs
                            .add_constructor_alias(name.clone(), qualified.clone());

                        if c.fields.is_empty() {
                            self.gs.add_value(
                                qualified,
                                ty.to_bounded_with_generics(def.generics.to_owned()),
                                self.new_declaration(span),
                            );

                            return Constructor {
                                name,
                                fields: vec![],
                            };
                        }

                        let new_fields: Vec<_> = c
                            .fields
                            .iter()
                            .map(|f| {
                                self.gs.begin_scope();

                                self.gs.put_generics_in_scope(
                                    &def.generics,
                                    self.new_declaration(span),
                                );

                                let typ = self.to_type(&f.ann, span);

                                self.gs.exit_scope();

                                EnumFieldDef {
                                    ty: typ,
                                    ..f.clone()
                                }
                            })
                            .collect();

                        let fun = Type::Fun {
                            args: new_fields.clone().into_iter().map(|f| f.ty).collect(),
                            bounds: Default::default(),
                            ret: ty.clone().into(),
                            id: TypeId::unset(),
                        };

                        // let fun = self.generalize(fun, &def.generics, &span);

                        self.gs.add_value(
                            qualified,
                            fun.to_bounded_with_generics(def.generics.to_owned()),
                            self.new_declaration(span),
                        );

                        Constructor {
                            name,
                            fields: new_fields,
                        }
                    })
                    .collect();

                let def = EnumDefinition {
                    name: def.name.clone(),
                    generics: def.generics.clone(),
                    cons: new_cons,
                };

                self.gs.add_enum(def.name.clone(), def);
            }

            Expr::StructDef { def, span } => {
                let new_fields = def
                    .fields
                    .iter()
                    .map(|f| {
                        self.gs.begin_scope();

                        self.gs
                            .put_generics_in_scope(&def.generics, self.new_declaration(span));

                        let typ = self.to_type(&f.ann, span);

                        self.gs.exit_scope();

                        StructFieldDef {
                            ty: typ.to_bounded(), // should this care about generics?
                            ..f.clone()
                        }
                    })
                    .collect();

                let def = StructDefinition {
                    name: def.name.clone(),
                    generics: def.generics.clone(),
                    fields: new_fields,
                };

                self.gs.add_struct(def.name.clone(), def);
            }

            Expr::Mod { items, .. } => {
                for i in items {
                    self.declare_variants(i);
                }
            }

            _ => (),
        }
    }

    /// This should be the preferred way of running inference from the outside
    pub fn infer_expr_with_error(&mut self, e: &Expr) -> (Expr, Vec<Error>, Type) {
        self.errors = vec![];

        let ty = self.fresh_ty_var();
        let expr = self.infer_expr(e.clone(), &ty);

        let errors = self.errors.clone();
        self.errors = vec![];

        let ty = self.substitute(ty);
        let expr = substitute::substitute_expr(expr, self);
        (expr, errors, ty)
    }

    pub fn sub(&mut self, var: i32) -> Type {
        self.substitutions
            .get(&var)
            .cloned()
            .unwrap_or(Type::Var(var))
    }

    fn occurs_in(&self, var: i32, t: &Type) -> bool {
        match t {
            Type::Con { args, .. } => args.iter().any(|a| self.occurs_in(var, a)),
            Type::Var(v) => v == &var,

            Type::Fun { args, ret, .. } => {
                let in_args = args.iter().any(|a| self.occurs_in(var, a));
                in_args || self.occurs_in(var, ret)
            }
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type, span: &Span) -> Result<(), String> {
        // First unpack references. Type inference doesn't bother with references,
        // it just checks that the underlying types match. A further compiler pass
        // will check that the correct references are being passed around.
        let t1_reference = t1.is_reference() || t1.is_mut_reference();
        let t2_reference = t2.is_reference() || t2.is_mut_reference();

        // if they are both references, unwrap them both and check that the inner type matches
        if t1_reference && t2_reference {
            return self.unify(&t1.remove_references(), &t2.remove_references(), span);
        }

        // Otherwise, prevent substitution of variables too eagerly and only unify if we're dealing
        // with concrete types on both sides
        if t1_reference && !t2.is_var() {
            return self.unify(&t1.remove_references(), t2, span);
        }

        if t2_reference && !t1.is_var() {
            return self.unify(t1, &t2.inner().unwrap(), span);
        }
        // END references stuff

        // If either type can be discarded, then there's nothing to unify.
        if t1.is_discard() || t2.is_discard() {
            return Ok(());
        }

        // Don't bother matching exact numeric types, for now.
        if t1.is_numeric() && t2.is_numeric() {
            return Ok(());
        }

        //
        // Check the actual types
        //
        match (t1, t2) {
            (Type::Var(i1), Type::Var(i2)) if i1 == i2 => {
                Ok(()) /* do nothing if they're equal */
            }

            // Unify substitution when there is one
            (Type::Var(i), _) if &self.sub(*i) != t1 => {
                let s = self.sub(*i);
                self.unify(&s, t2, span)
            }

            // Same as above for t2
            (_, Type::Var(i)) if &self.sub(*i) != t2 => {
                let s = self.sub(*i);
                self.unify(t1, &s, span)
            }

            // Substitute t1 with t2, only if not an infinite type
            (Type::Var(i), _) => {
                if self.occurs_in(*i, t2) {
                    return Err("Occurs check".to_string());
                }

                self.substitutions.insert(*i, t2.clone());
                Ok(())
            }

            // Same as above for t2
            (_, Type::Var(i)) => {
                if self.occurs_in(*i, t1) {
                    return Err("Occurs check".to_string());
                }

                self.substitutions.insert(*i, t1.clone());
                Ok(())
            }

            // They are both concrete types, check names and args match
            (Type::Con { name: n1, args: a1 }, Type::Con { name: n2, args: a2 }) => {
                if n1 == "any" || n2 == "any" {
                    return Ok(());
                }

                if n1 != n2 {
                    match self.check_interface_impl(n1, n2) {
                        CheckInterfaceResult::Ok => return Ok(()),
                        CheckInterfaceResult::NotAnInterface => (),
                        CheckInterfaceResult::Error(e) => return Err(e),
                    }

                    match self.check_interface_impl(n2, n1) {
                        CheckInterfaceResult::Ok => return Ok(()),
                        CheckInterfaceResult::NotAnInterface => (),
                        CheckInterfaceResult::Error(e) => return Err(e),
                    }

                    return Err("Type mismatch".to_string());
                }

                if a1.len() != a2.len() {
                    return Err("Type mismatch".to_string());
                }

                a1.iter()
                    .zip(a2)
                    .try_for_each(|(t1, t2)| self.unify(t1, t2, span))?;

                Ok(())
            }

            // They are both functions, check arity and then unify arguments and return type
            (
                Type::Fun {
                    args: a1, ret: r1, ..
                },
                Type::Fun {
                    args: a2, ret: r2, ..
                },
            ) => {
                if a1.len() != a2.len() {
                    return Err("Functions have different arity".to_string());
                }

                let args = a1
                    .iter()
                    .zip(a2)
                    .try_for_each(|(t1, t2)| self.unify(t1, t2, span));

                let ret = self.unify(r1, r2, span);

                // This seems dumb, but allows the return type to get unified
                // even in the presence of errors in one of the args. In fact,
                // try_for_each might not be the right function as I want to
                // unify all args, not bail out at the first error.
                args?;
                ret?;
                Ok(())

                // TODO need to unify errors? Probably
                // need to unify effects? Not sure
            }

            // Matching functions with constructors is always an error
            (Type::Fun { .. }, Type::Con { .. }) => Err("Type mismatch".to_string()),
            (Type::Con { .. }, Type::Fun { .. }) => Err("Type mismatch".to_string()),
            /*
            _ => {
                self.dump();
                todo!("{:?}, {:?}", t1, t2)
            }
            */
        }
    }

    fn init_builtin_types(gs: &mut GlobalState) {
        // Pretty much all of these could be defined in the stdlib.
        // They're still here because infer-expr tests don't load the stdlib so I'd need to copy
        // pasta this types in the prelude. Maybe one day I'll get rid of the prelude and this
        // function too.
        let gen_t = Type::generic("T");

        gs.add_builtin_type("Unit".into(), Type::unit().to_bounded());
        gs.add_builtin_type(
            "Slice".into(),
            Type::slice(gen_t.clone()).to_bounded_with_generics(vec!["T".to_string()]),
        );
        gs.add_builtin_type("Never".into(), Type::never().to_bounded());
        gs.add_builtin_type(
            "Ref".into(),
            Type::reference(gen_t.clone()).to_bounded_with_generics(vec!["T".to_string()]),
        );
        gs.add_builtin_type(
            "RefMut".into(),
            Type::reference(gen_t).to_bounded_with_generics(vec!["T".to_string()]),
        );
    }

    fn generic_error(&mut self, msg: String, span: Span) {
        let err = Error::Generic(msg, span);
        self.errors.push(err);
    }

    fn get_struct_by_name(&mut self, name: &str) -> Option<(StructDefinition, BoundedType)> {
        let def = self.gs.get_struct(name)?;
        let ty = self.gs.get_type(name)?;
        Some((def, ty))
    }

    fn new_declaration(&self, span: &Span) -> Declaration {
        Declaration {
            file_id: self.current_file_id.clone().unwrap_or_else(|| FileId {
                package: "dummy".to_string(),
                filename: "dummy".to_string(),
            }),
            span: span.clone(),
        }
    }

    // Allow users to omit the error type, ie. Result<T>
    // This function will add the error type back -> Result<T, E>
    fn add_optional_error_to_result(&mut self, ty: Type, _span: &Span) -> Type {
        match &ty {
            Type::Con { name, args } => {
                if name == "Result" && args.len() == 1 {
                    let mut new_args = args.clone();

                    let error_ty = Type::Con {
                        name: "error".to_string(),
                        args: vec![],
                    };
                    new_args.push(error_ty);

                    return Type::Con {
                        name: "Result".to_string(),
                        args: new_args.clone(),
                    };
                }

                ty
            }

            _ => ty,
        }
    }

    fn to_type(&mut self, ann: &TypeAst, span: &Span) -> Type {
        match ann {
            TypeAst::Con { name, args } => {
                let new_args = args.iter().map(|a| self.to_type(a, span)).collect();

                let existing = self.gs.get_type(name);

                if existing.is_none() {
                    self.generic_error(format!("Type not found: {name}"), span.clone());
                    return self.fresh_ty_var();
                }

                let existing = self.instantiate(&existing.unwrap());

                let ty = Type::Con {
                    name: name.into(),
                    args: new_args,
                };

                let ty = self.add_optional_error_to_result(ty, span);

                let expected_args = existing.get_args().unwrap();
                let actual_args = ty.get_args().unwrap();

                if expected_args.len() != actual_args.len() {
                    let err = ArityError {
                        expected: expected_args,
                        actual: actual_args,
                        span: span.clone(),
                    };

                    self.errors.push(Error::WrongArity(err));
                }

                ty
            }

            TypeAst::Fun { args, ret } => {
                let new_args = args.iter().map(|a| self.to_type(a, span)).collect();
                let new_ret = self.to_type(ret, span);

                Type::Fun {
                    args: new_args,
                    bounds: Default::default(),
                    ret: new_ret.into(),
                    id: TypeId::unset(),
                }
            }

            TypeAst::Unknown => self.fresh_ty_var(),
        }
    }

    fn check_reserved_name(&mut self, ident: &str) -> bool {
        RESERVED_WORDS.contains(&ident)
    }

    fn check_interface_impl(&mut self, n1: &str, n2: &str) -> CheckInterfaceResult {
        let interface = self.gs.get_interface(n1);
        if interface.is_none() {
            return CheckInterfaceResult::NotAnInterface;
        }

        let interface = interface.unwrap();
        let methods = self.gs.get_type_methods(n2);

        for (method_name, method_ty) in interface.methods {
            let candidate = methods.get(&method_name);

            if candidate.is_none() {
                return CheckInterfaceResult::Error(format!(
                    "method {} not found on type {}",
                    method_name, n2
                ));
            }

            // Add a self receiver param on the interface method
            let interface_method = method_ty.clone().add_any_receiver();

            // Check the two unify
            if let Err(e) = self.unify(&interface_method, &candidate.unwrap().ty.ty, &Span::dummy())
            {
                return CheckInterfaceResult::Error(format!(
                    "method {} on type {} has wrong type {}",
                    method_name, n2, e
                ));
            }
        }

        return CheckInterfaceResult::Ok;
    }

    fn declare_modules(&mut self, files: &[File]) {
        // There's a lot of repetition in this function, maybe it can be improved a bit.
        //
        // First declare symbols.
        // This is incomplete, modules will be filled out in the next pass below.
        for f in files {
            for e in &f.decls {
                if let Expr::Mod {
                    name, pkg, items, ..
                } = e
                {
                    self.gs.begin_scope();
                    self.current_file_id = Some(FileId {
                        package: name.clone(),
                        filename: f.name.clone(),
                    });

                    let decls = vec![File {
                        decls: items.clone(),
                        ..f.clone()
                    }];
                    self.declare_symbols(&decls);

                    let module = self.gs.extract_module(name, &pkg);
                    self.gs.add_module(&pkg.name, module);

                    self.current_file_id = None;
                    self.gs.exit_scope();
                }
            }
        }

        // Loop over again and declare everything else
        for f in files {
            for e in &f.decls {
                if let Expr::Mod {
                    name, pkg, items, ..
                } = e
                {
                    self.gs.begin_scope();
                    self.current_file_id = Some(FileId {
                        package: name.clone(),
                        filename: f.name.clone(),
                    });

                    let decls = vec![File {
                        decls: items.clone(),
                        ..f.clone()
                    }];
                    self.declare_files(&decls);

                    let module = self.gs.extract_module(name, &pkg);
                    self.gs.add_module(&pkg.name, module);

                    self.current_file_id = None;
                    self.gs.exit_scope();
                }
            }
        }
    }

    fn import_package(&mut self, import: &PkgImport, span: &Span) {
        let module = self.gs.get_module(&import.name);
        if module.is_none() {
            self.generic_error(
                format!("Package {name} not found", name = import.name),
                span.clone(),
            );
            return;
        }

        let module = module.unwrap();
        self.gs.import_module(&module.name, module.clone());
        self.create_pkg_struct(&module.name, &module);
    }

    fn create_pkg_struct(&mut self, name: &str, module: &Module) {
        // create a struct that holds one field for each fn
        // define a global value for the package (ie. fmt)
        // TODO asdf store the names of these packages and their values, so that when extracting
        // modules we can skip them

        let struct_name = format!("__Package{name}");
        let ty = Type::Con {
            name: struct_name.clone(),
            args: vec![],
        };

        let mut fields = vec![];

        for (binding, v) in &module.values {
            fields.push(StructFieldDef {
                name: binding.clone(),
                ann: TypeAst::Unknown,
                ty: module.rewrite_type(name, &v.ty),
            });

            // If this value has a receiver defined in this package, then add it as a method
            if let Some((receiver, decl)) = self.external_func_is_method(&name, &v.ty.ty) {
                self.gs.add_method(
                    &module.rewrite_type_impl(name, &receiver),
                    &binding,
                    module.rewrite_type(name, &v.ty),
                    decl,
                );
            }
        }

        let def = StructDefinition {
            name: struct_name.clone(),
            generics: vec![],
            fields,
        };

        let span = Span::dummy(); // TODO asdf
        let decl = self.new_declaration(&span);

        // Hacky way to register global functions (ie. string(), int() etc.)
        if name == "global" {
            for f in def.fields {
                self.gs.add_value(f.name, f.ty, decl.clone());
            }

            return;
        }

        self.gs
            .add_type(struct_name.to_string(), ty.to_bounded(), decl.clone());
        self.gs.add_struct(struct_name, def);
        self.gs.add_value(name.to_string(), ty.to_bounded(), decl)
    }

    fn declare_symbols(&mut self, files: &[File]) {
        files.iter().for_each(|f| {
            f.decls
                .iter()
                .filter(|e| {
                    matches!(
                        e,
                        Expr::EnumDef { .. }
                            | Expr::StructDef { .. }
                            | Expr::TypeAlias { .. }
                            | Expr::Trait { .. }
                    )
                })
                .for_each(|e| self.declare_type(e));
        });
    }

    fn external_func_is_method(&mut self, name: &str, ty: &Type) -> Option<(Type, Declaration)> {
        let args = ty.get_function_args()?;
        let receiver = args.first()?;
        let ty_name = receiver.remove_references().get_name()?;
        let type_def = self.gs.get_global_type_declaration(&ty_name)?;

        if type_def.decl.file_id.package == name {
            return Some((receiver.remove_references(), type_def.decl));
        }

        None
    }

    fn check_numeric(&mut self, ty: &Type, span: &Span) {
        let ty = self.substitute(ty.clone());
        if !ty.is_numeric() {
            self.generic_error("Expected numeric type".to_string(), span.clone());
        }
    }
}

enum CheckInterfaceResult {
    Ok,
    NotAnInterface,
    Error(String),
}

const RESERVED_WORDS: &[&str] = &["default", "len", "append", "cap"];
