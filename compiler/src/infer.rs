use crate::ast::{
    Arm, Binding, Constructor, DebugKind, EnumDefinition, EnumFieldDef, Expr, ExternKind, File,
    Function, FunctionKind, Literal, Operator, Pat, Span, StructDefinition, StructField,
    StructFieldDef, StructFieldPat, TypeAst, UnOp,
};
use crate::error::{ArityError, Error, UnificationError};
use crate::exhaustive;
use crate::global_state::{Declaration, FileId, GlobalState, ValueDef};
use crate::project::Package;
use crate::substitute;
use crate::type_::{BoundedType, Type};

use std::collections::{HashMap, HashSet};

pub struct Infer {
    pub gs: GlobalState,
    substitutions: HashMap<i32, Type>,

    // The return type of the current function being inferred
    current_fn_ret_ty: Option<Type>,

    // The `Error` type as defined by this package
    package_error_ty: Option<Type>,

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
            package_error_ty: None,
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
                fx,
            } => {
                let new_args = args.into_iter().map(|a| self.substitute(a)).collect();
                let new_bounds = bounds.into_iter().map(|a| self.substitute(a)).collect();
                let new_ret = self.substitute(*ret);

                Type::Fun {
                    args: new_args,
                    bounds: new_bounds,
                    ret: new_ret.into(),
                    fx,
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

                Literal::String(s) => {
                    self.add_constraint(expected, &Type::string(), &span);

                    Expr::Literal {
                        lit: Literal::String(s),
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

                Literal::List(elems) => {
                    let ty = self.fresh_ty_var();
                    let new_elems = elems.into_iter().map(|e| self.infer_expr(e, &ty)).collect();

                    let ty = Type::list(ty);
                    self.add_constraint(expected, &ty, &span);

                    Expr::Literal {
                        lit: Literal::List(new_elems),
                        ty,
                        span,
                    }
                }
            },

            Expr::Block {
                ref stmts, span, ..
            } => {
                stmts.iter().for_each(|e| self.declare_type(e));

                let new_stmts: Vec<_> = stmts
                    .iter()
                    // Generate a new type variable for each statement, as we don't care what the actual type is
                    .map(|e| {
                        let var = self.fresh_ty_var();
                        let expr = self.infer_expr(e.clone(), &var);
                        (var, expr)
                    })
                    .collect();

                let (last_ty, last_span) = new_stmts
                    .last()
                    .map(|(ty, e): &(Type, Expr)| (ty.clone(), e.get_span()))
                    .unwrap_or_else(|| (Type::unit(), span.clone()));

                self.add_constraint(expected, &last_ty, &last_span);

                Expr::Block {
                    stmts: new_stmts.into_iter().map(|(_, e)| e).collect(),
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

                let new_bounds: Vec<_> =
                    fun.bounds.iter().map(|b| self.to_type(b, &span)).collect();

                new_bounds.iter().for_each(|b| self.gs.add_assumption(b));

                // set return type as the current function being evaluated (used by Return branch)
                let prev_ret_ty = self.current_fn_ret_ty.clone(); // nested functions, save context
                self.current_fn_ret_ty = Some(new_ret.clone());

                let typ = Type::Fun {
                    args: new_args.iter().map(|b| b.ty.clone()).collect(),
                    bounds: new_bounds,
                    ret: new_ret.clone().into(),
                    fx: Default::default(),
                };

                let new_body = self.infer_expr(*fun.body.clone(), &new_ret);

                self.gs.exit_scope();
                // END NEW SCOPE

                self.current_fn_ret_ty = prev_ret_ty; // Reset current function

                let bounded_ty = typ.to_bounded_with_generics(fun.generics.to_owned());
                let typ = self.instantiate(&bounded_ty);

                self.add_constraint(expected, &typ, &span);

                match kind {
                    FunctionKind::TopLevel | FunctionKind::Inline => {
                        // TODO for now, out of the builtin overloads only to_string can be overridden.
                        // The reason is buried somewhere in the runtime, it's probably not worth
                        // implementing this right now -- better wait for comptime.
                        // But it means that there should be a check here to prevent users from
                        // overriding these overloads, as their code won't get called if in a
                        // nested struct.

                        self.gs.add_value(
                            fun.name.clone(),
                            bounded_ty.clone(),
                            self.new_declaration(&span),
                        );

                        self.gs.remove_derived_overload(&fun.name);
                    }

                    FunctionKind::Lambda => {}
                };

                Expr::Closure {
                    fun: Function {
                        name: fun.name.clone(),
                        generics: fun.generics.clone(),
                        bounds: fun.bounds.clone(),
                        args: new_args,
                        body: new_body.into(),
                        ret: new_ret,
                        ann: fun.ann.clone(),
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

                let ty = self.instantiate(&def.ty);
                self.add_constraint(expected, &ty, &span);

                Expr::Var {
                    value: value.clone(),
                    decl: def.decl,
                    ty,
                    span,
                }
            }

            Expr::Call {
                func, args, span, ..
            } => {
                let ty = self.fresh_ty_var();
                let new_func = self.infer_expr(*func, &ty);

                let ty = new_func.get_type();
                let bounds = ty.get_bounds();

                let (args_ty, ret_ty) = match ty {
                    Type::Fun { args, ret, .. } => (args, *ret),
                    _ => {
                        // we may get here without a proper function, ie. if a method did not exist
                        // manufacture a function on the fly and keep going
                        let args_ty = args.iter().map(|_| self.fresh_ty_var()).collect();
                        let ret = self.fresh_ty_var();
                        (args_ty, ret)
                    }
                };

                // match arguments
                let new_args: Vec<Expr> = args
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

                // Transform Debug::inspect(x) to Expr::Debug
                if let Some(op) = self.transform_call_expr(&new_func, &new_args, &span) {
                    return self.infer_expr(op, expected);
                }

                // match return type
                self.add_constraint(expected, &ret_ty, &span);

                // match constraint bounds
                bounds.iter().for_each(|b| {
                    let constraint = self.substitute(b.clone());

                    let trait_name = constraint.get_name();
                    if trait_name.is_none() {
                        // There's probably a type error somewhere else, just skip checking.
                        return;
                    }

                    let trait_name = trait_name.unwrap();
                    let inner_type = constraint.get_args().unwrap().first().unwrap().clone();

                    // There can't be any overload for functions, so this is always an error
                    if inner_type.is_function() {
                        // self.check_trait_can_be_derived(&trait_name, &inner_type);
                        self.generic_error(
                            format!("Functions don't support overload `{trait_name}`"),
                            span.clone(),
                        );
                        return;
                    }

                    // Otherwise we need to know if it's a regular type or a generic
                    let type_name = inner_type.get_name();

                    if type_name.is_none() {
                        // Same as above, type error somewhere else
                        return;
                    }

                    let type_name = type_name.unwrap();
                    let ty = self.gs.get_global_type(&type_name);

                    match ty {
                        Some(_) => {
                            // Regular type
                            // Check that an overload exists for this type.
                            let method = format!("{type_name}::{trait_name}");
                            if self.gs.get_value(&method).is_none() {
                                self.generic_error(
                                    format!(
                                        "No overload `{trait_name}` found for type `{type_name}`"
                                    ),
                                    span.clone(),
                                );
                            }
                        }

                        None => {
                            // Generic type
                            if !self.gs.constraint_satisfied(&constraint) {
                                self.generic_error(
                            format!("Constraint {trait_name} not satisfied for type {type_name}"),
                            span.clone(),
                        );
                            }
                        }
                    }
                });

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
                let new_expr = self.infer_expr(*expr, expected);
                match &ann {
                    TypeAst::Unknown => panic!("checking type without type?"),
                    _ => (),
                }

                let new_ty = self.to_type(&ann, &span);

                self.add_constraint(expected, &new_ty, &span);

                Expr::CheckType {
                    expr: new_expr.into(),
                    ty: new_ty,
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
                let ret = self.fresh_ty_var();
                let new_cond = self.infer_expr(*cond, &Type::bool());
                let new_then = self.infer_expr(*then, &ret);
                let new_els = self.infer_expr(*els, &ret);

                self.add_constraint(expected, &ret, &span);

                Expr::If {
                    cond: new_cond.into(),
                    then: new_then.into(),
                    els: new_els.into(),
                    ty: ret,
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
                                self.instantiate_with_vars(&found.ty, &generics)
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

            Expr::StructAccess {
                expr,
                field,
                span,
                ty: _,
            } => {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);
                let ty = self.substitute(ty);

                let field_ty = ty
                    .get_name()
                    .ok_or(format!("was expecting struct, got {}", ty))
                    .and_then(|name| {
                        self.get_struct_by_name(&name)
                            .ok_or_else(|| format!("no struct {}", name))
                    })
                    .and_then(|(def, struct_ty)| {
                        // Instantiate the struct again, so we can get a fresh mapping of generics.
                        let generics = self.collect_generics(&struct_ty);
                        let instantiated = self.instantiate_with_vars(&struct_ty.ty, &generics);

                        // Constraint the new instantiated struct to the expr we inferred before.
                        self.add_constraint(&ty, &instantiated, &span);

                        // Now look for the field in the struct.
                        // The type in the field won't be instantiated yet, so we can use the same
                        // vars we used to instantiate the struct to get all the types to line up.
                        def.fields
                            .iter()
                            .find_map(|f| {
                                if f.name == field {
                                    Some(self.instantiate_with_vars(&f.ty, &generics))
                                } else {
                                    None
                                }
                            })
                            .ok_or_else(|| format!("no struct field {}", field))
                    })
                    .unwrap_or_else(|err| {
                        self.generic_error(err, span.clone());
                        self.fresh_ty_var()
                    });

                self.add_constraint(expected, &field_ty, &span);

                Expr::StructAccess {
                    expr: new_expr.into(),
                    field,
                    ty: field_ty,
                    span,
                }
            }

            Expr::VarUpdate {
                target,
                value,
                span,
            } => {
                // TODO keep this around for named params in function calls
                // ie. f(a = 1, b = false)

                let ty = self.fresh_ty_var();

                let new_target = self.infer_expr(*target.clone(), &ty);

                match &new_target {
                    Expr::Var { value, .. } => {
                        self.generic_error(
                            format!("Can't assign to variable {}. Re-bind instead", value),
                            span.clone(),
                        );
                    }

                    Expr::StructAccess { .. } => {
                        self.generic_error(
                            "updating struct fields is not supported. Re-bind instead".to_string(),
                            span.clone(),
                        );
                    }

                    _ => self.generic_error(
                        "Assign operator is not supported".to_string(),
                        span.clone(),
                    ),
                };

                Expr::VarUpdate {
                    target,
                    value,
                    span,
                }
            }

            Expr::MethodCall {
                ty: _,
                target,
                method,
                args,
                span,
            } => {
                let ty = self.fresh_ty_var();
                let new_target = self.infer_expr(*target, &ty);
                let ty = self.substitute(ty);

                let new_target = new_target.replace_span(&span);

                // If it's a struct, check if there is a field that matches method
                if let Some(func) = self.find_method_in_struct(&ty, &new_target, &method) {
                    let func = Expr::Call {
                        func: func.into(),
                        args,
                        ty: Type::dummy(),
                        span,
                    };

                    return self.infer_expr(func, expected);
                }

                let func = self
                    .gs
                    .lookup_method(&method, &ty, args.len())
                    .unwrap_or_else(|| {
                        self.generic_error(
                            format!(
                                "Type:
    {}
has no method:
    {}",
                                &ty, &method,
                            ),
                            span.clone(),
                        );
                        method.clone()
                    });

                let decl = self
                    .gs
                    .get_value(&func)
                    .map(|def| def.decl)
                    .unwrap_or_else(Declaration::dummy);

                let func = Expr::Var {
                    value: func,
                    decl,
                    ty: Type::dummy(),
                    span: span.clone(),
                };
                let func = Expr::Call {
                    func: func.into(),
                    args: std::iter::once(new_target)
                        .chain(args.into_iter())
                        .collect(),
                    ty: Type::dummy(),
                    span,
                };

                // Transform all MethodCall into Call, no point keeping the extra node around
                self.infer_expr(func, expected)
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

                let ty = self.substitute(left_ty.clone());

                // TODO refactor this mess
                let target = match &op {
                    Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                        if self.check_numeric(&ty, new_left.get_span()) {
                            ty.clone()
                        } else {
                            self.fresh_ty_var()
                        }
                    }

                    Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge => {
                        let _ = self.check_numeric(&ty, new_left.get_span());
                        Type::bool()
                    }

                    Operator::Rem => Type::int(),

                    _ => Type::bool(),
                };

                self.add_constraint(expected, &target, &span);

                if op == Operator::And || op == Operator::Or {
                    self.add_constraint(&left_ty, &Type::bool(), &span);
                    self.add_constraint(&right_ty, &Type::bool(), &span);
                } else {
                    self.add_constraint(&left_ty, &right_ty, &span);
                }

                if op == Operator::Eq || op == Operator::Ne {
                    if !self.check_trait_can_be_derived("equals", &ty) {
                        self.generic_error(format!("Type {} can't be compared", &ty), span.clone());
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
                        let ty = self.substitute(new_expr.get_type());

                        if ty != Type::int() && ty != Type::float() {
                            let msg = format!("Was expecting Int or Float, got {}", ty);
                            self.generic_error(msg, new_expr.get_span());
                            self.fresh_ty_var()
                        } else {
                            ty
                        }
                    }

                    UnOp::Not => Type::bool(),
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

            Expr::Spawn { expr, ty: _, span } => {
                // TODO just check that it's a Call
                let new_expr = self.infer_expr(*expr, expected);

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

            Expr::ExternDecl {
                name,
                kind,
                items,
                span,
            } => {
                if kind == ExternKind::Overload {
                    return Expr::ExternDecl {
                        name,
                        kind,
                        items,
                        span,
                    };
                }

                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                Expr::ExternDecl {
                    name,
                    kind,
                    items: new_items,
                    span,
                }
            }

            Expr::Noop => Expr::Noop,
            Expr::Todo => todo!(),
            // _ => todo!("{:#?}", expr),
        }
    }

    pub fn collect_generics(&mut self, typ: &BoundedType) -> HashMap<String, i32> {
        typ.generics
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
                fx,
            } => Type::Fun {
                args: args
                    .iter()
                    .map(|x| self.instantiate_with_vars(x, new_vars))
                    .collect(),
                bounds: bounds
                    .iter()
                    .map(|x| self.instantiate_with_vars(x, new_vars))
                    .collect(),
                ret: self.instantiate_with_vars(ret, new_vars).into(),
                fx: fx.clone(),
            },
        }
    }

    fn infer_pat(&mut self, pat: Pat, expected: Type) -> Pat {
        match pat {
            Pat::Type { ident, ann, span } => {
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
                Pat::Type { ident, ann, span }
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
                            Some(found) => self.instantiate_with_vars(&found.ty, &generics),
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
        self.declare_files(&pkg.files);

        // Look for the blessed `Error` type.
        // It's fine if we can't find one, it means the package only exports
        // pure functions, or uses Results with explicit errors.
        self.package_error_ty = self.gs.get_type("Error").map(|def| def.ty);

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

        // 1. Declare Enums and structs
        files.iter().for_each(|f| {
            f.decls
                .iter()
                .filter(|e| matches!(e, Expr::EnumDef { .. } | Expr::StructDef { .. }))
                .for_each(|e| self.declare_type(e));
        });

        // 2. Declare all the actual variants
        files.iter().for_each(|f| {
            // Now that all types are in scope, actually parse the enum variants and struct fields.
            f.decls.iter().for_each(|e| self.declare_variants(e));
        });

        // 3. Declare overloads
        files.iter().for_each(|f| {
            f.decls.iter().for_each(|e| match e {
                Expr::ExternDecl { kind, items, .. } if kind == &ExternKind::Overload => {
                    items.iter().for_each(|e| match e {
                        Expr::Closure {
                            ref fun, ref span, ..
                        } => {
                            let ty = self.fresh_ty_var();
                            self.infer_expr(e.clone(), &ty);

                            // Would be nice to just use `ty` here, but infer_expr will always
                            // return an instantiated type, so we can't use it.
                            // Instead, we get the generalized type by looking in GlobalState
                            let def = self.gs.get_value(&fun.name).unwrap();

                            self.check_and_add_overload(fun, &def.ty, &span);
                        }

                        _ => unreachable!(),
                    });
                }

                _ => (),
            });
        });

        // 4. Derive implementation for known overloads
        // (this will be moved to comptime eventually)

        let overloads = self.gs.get_overloads();
        self.gs
            .get_all_types()
            .into_iter()
            .for_each(|(ty_name, ty)| {
                // Skip overloading overloads, bit confusing I guess.
                if overloads.contains(&ty_name) {
                    return;
                }

                overloads.iter().for_each(|overload_name| {
                    let overload_ty = self.gs.get_value(overload_name).unwrap();

                    if self.check_trait_can_be_derived(overload_name, &ty.ty) {
                        let method = format!("{ty_name}::{overload_name}");

                        // Replace T in overload_ty with ty.ty
                        // For example, given a target type Foo<X>
                        // we want to instantiate the overload
                        // equals<T>(x: T, y: T) -> Bool
                        // to
                        // equals<X: equals>(x: Foo<X>, y: Foo<X>) -> Bool
                        let new_fn = self
                            .instantiate_overload(
                                &overload_ty.ty.ty,
                                overload_ty.ty.generics.first().unwrap(),
                                overload_name,
                                &ty,
                            )
                            .to_bounded_with_generics(overload_ty.ty.generics);

                        // Finally add the new function to the global scope.
                        // Note that there's no actual implementation, codegen will take care of it.
                        // Only do this if there's no implementation in scope already.

                        if self.gs.get_value(&method).is_none() {
                            self.gs.add_value(method, new_fn, Declaration::dummy());
                            self.gs.add_derived_overload(&overload_name, &ty_name);
                        }
                    }
                })
            });

        // 5. Declare Closure, Impl, Extern and Const
        files.iter().for_each(|f| {
            f.decls
                .iter()
                .filter(|e| {
                    matches!(
                        e,
                        Expr::Closure { .. }
                            | Expr::ImplBlock { .. }
                            | Expr::ExternDecl { .. }
                            | Expr::Const { .. }
                    )
                })
                .for_each(|e| self.declare_type(e));
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

            Expr::Closure { fun, span, .. } => {
                let new_func = create_func(fun, span);
                let ty = self.fresh_ty_var();
                self.infer_expr(new_func, &ty);
            }

            Expr::ImplBlock { items, .. } => {
                items.iter().for_each(|e| {
                    self.declare_type(e);
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

            Expr::ExternDecl { items, kind, .. } => {
                if kind == &ExternKind::Overload {
                    // overloads need to be declared already, see declare_files
                    return ();
                }

                items.iter().for_each(|fun| {
                    let ty = self.fresh_ty_var();
                    self.infer_expr(fun.clone(), &ty);
                });
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
                        self.gs.add_alias(name.clone(), qualified.clone());

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
                            fx: Default::default(),
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
                            ty: typ,
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

            _ => (),
        }
    }

    /// This should be the preferred way of running inference from the outside
    pub fn infer_expr_with_error(&mut self, e: &Expr, err_ty: Type) -> (Expr, Vec<Error>, Type) {
        self.errors = vec![];

        self.package_error_ty = Some(err_ty);

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
                if n1 != n2 || a1.len() != a2.len() {
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
        let gen_t = Type::generic("T");

        gs.add_builtin_type("Unit".into(), Type::unit().to_bounded());
        gs.add_builtin_type("Int".into(), Type::int().to_bounded());
        gs.add_builtin_type("Float".into(), Type::float().to_bounded());
        gs.add_builtin_type("Bool".into(), Type::bool().to_bounded());
        gs.add_builtin_type("String".into(), Type::string().to_bounded());
        gs.add_builtin_type("Char".into(), Type::char().to_bounded());
        gs.add_builtin_type(
            "List".into(),
            Type::list(gen_t).to_bounded_with_generics(vec!["T".to_string()]),
        );
        gs.add_builtin_type("Never".into(), Type::never().to_bounded());
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

    fn transform_call_expr(&mut self, func: &Expr, args: &[Expr], span: &Span) -> Option<Expr> {
        match &func {
            Expr::Var { value, .. } => {
                if value == "Debug::inspect" {
                    return Some(Expr::Debug {
                        kind: DebugKind::Inspect,
                        expr: args[0].clone().into(),
                        ty: Type::dummy(),
                        span: span.clone(),
                    });
                }

                None
            }

            _ => None,
        }
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

    fn find_method_in_struct(
        &mut self,
        ty: &Type,
        new_target: &Expr,
        method: &str,
    ) -> Option<Expr> {
        let name = ty.get_name()?;
        let def = self.gs.get_struct(&name)?;

        let found = def.fields.iter().any(|f| f.name == method);
        if !found {
            return None;
        }

        Some(Expr::StructAccess {
            expr: new_target.to_owned().into(),
            field: method.to_string(),
            ty: Type::dummy(),
            span: new_target.get_span(),
        })
    }

    // Allow users to omit the error type, ie. Result<T>
    // This function will add the error type back -> Result<T, E>
    fn add_optional_error_to_result(&mut self, ty: Type, span: &Span) -> Type {
        match &ty {
            Type::Con { name, args } => {
                if name == "Result" && args.len() == 1 {
                    let mut new_args = args.clone();

                    let package_err = match &self.package_error_ty {
                        Some(ty) => ty.clone(),
                        None => {
                            self.generic_error(
                                "
     Tried to use Result<T> shorthand syntax,
     but no Error type was found for this package"
                                    .to_string(),
                                span.clone(),
                            );
                            self.fresh_ty_var()
                        }
                    };

                    new_args.push(package_err);

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
                    fx: Default::default(),
                }
            }

            TypeAst::Unknown => self.fresh_ty_var(),
        }
    }

    fn check_and_add_overload(&mut self, fun: &Function, bounded_ty: &BoundedType, span: &Span) {
        // TODO check:
        //   - there is one generic param
        //   - function is of shape T -> A
        //
        let type_param = fun.generics.first().unwrap();

        let bound = Type::Con {
            name: fun.name.clone(),
            args: vec![Type::generic(type_param)],
        };

        let ty = match &bounded_ty.ty {
            Type::Fun { args, ret, .. } => Type::Fun {
                args: args.clone(),
                bounds: vec![bound.clone()],
                ret: ret.clone(),
                fx: Default::default(),
            },
            _ => unreachable!(),
        };

        // Add type (needed when resolving constraints)
        self.gs.add_type(
            fun.name.clone(),
            bound.to_bounded_with_generics(vec![type_param.to_string()]),
            self.new_declaration(span),
        );

        // Add trait ie. equals
        self.gs.add_trait(&fun.name);

        // Add function ie. equals
        // NOTE this overrides the declaration that was just added when inferring the function.
        // It's probably a bit confusing, need to think of how to improve this.
        self.gs.add_value(
            fun.name.clone(),
            ty.to_bounded_with_generics(vec![type_param.to_string()]),
            Declaration::dummy(),
        );
    }

    fn check_trait_can_be_derived(&mut self, trait_name: &str, ty: &Type) -> bool {
        let mut seen = HashSet::new();
        self.check_trait_can_be_derived_impl(&mut seen, trait_name, ty)
    }

    fn check_trait_can_be_derived_impl(
        &mut self,
        seen: &mut HashSet<String>,
        trait_name: &str,
        ty: &Type,
    ) -> bool {
        // All the built-in traits pretty much follow the same logic.
        // Deriving an automatic instance for a trait is allowed, as long as the type doesn't
        // contain functions (which can't be compared, hashed etc.)

        match ty {
            Type::Con { name, .. } => {
                // Extend check to fields in structs and enums

                if seen.contains(name) {
                    return true;
                }

                seen.insert(name.to_string());

                if let Some(def) = self.gs.get_struct(name) {
                    return def
                        .fields
                        .iter()
                        .all(|f| self.check_trait_can_be_derived_impl(seen, trait_name, &f.ty));
                }

                if let Some(def) = self.gs.get_enum(name) {
                    return def.cons.iter().all(|c| {
                        c.fields
                            .iter()
                            .all(|f| self.check_trait_can_be_derived_impl(seen, trait_name, &f.ty))
                    });
                }

                return true;
            }

            Type::Fun { .. } => match trait_name {
                "equals" => false,
                "to_hash" => false,
                "to_string" => true,
                _ => todo!("Do something with trait {}", trait_name),
            },

            Type::Var(_) => {
                return false;
                // self.generic_error("Type must be known at this point".to_string(), span.clone())
            }
        }
    }

    fn check_reserved_name(&mut self, ident: &str) -> bool {
        RESERVED_WORDS.contains(&ident)
    }

    fn check_numeric(&mut self, ty: &Type, span: Span) -> bool {
        if ty != &Type::int() && ty != &Type::float() {
            let msg = format!("Was expecting Int or Float, got {}", &ty);
            self.generic_error(msg, span);
            return false;
        }

        true
    }

    // This is similar to `instantiate_with_vars`, but uses a concrete type instead of type variables.
    // ie. fn to_string<T>(x: T) -> String
    // instantiated with Foo<X, Y> becomes
    // fn to_string<X: to_string, Y: to_string>(x: Foo<X, Y>) -> String
    //
    // `self_ty` is the generic T in the overload
    // `overload_name` is `equals`, `to_string` etc.
    fn instantiate_overload(
        &mut self,
        typ: &Type,
        self_ty: &str,
        overload_name: &str,
        replace_with: &BoundedType,
    ) -> Type {
        match typ {
            Type::Var(_) => unreachable!(),

            Type::Con { name, args } => {
                if name == self_ty {
                    return replace_with.ty.clone();
                }

                Type::Con {
                    name: name.to_string(),
                    args: args
                        .iter()
                        .map(|x| self.instantiate_overload(x, self_ty, overload_name, replace_with))
                        .collect(),
                }
            }

            Type::Fun {
                args,
                bounds: _,
                ret,
                fx,
            } => Type::Fun {
                args: args
                    .iter()
                    .map(|x| self.instantiate_overload(x, self_ty, overload_name, replace_with))
                    .collect(),
                bounds: replace_with
                    .generics
                    .iter()
                    .map(|g| Type::Con {
                        name: overload_name.to_string(),
                        args: vec![Type::generic(g)],
                    })
                    .collect(),

                ret: self
                    .instantiate_overload(ret, self_ty, overload_name, replace_with)
                    .into(),
                fx: fx.clone(),
            },
        }
    }
}

const RESERVED_WORDS: &[&str] = &["default"];
