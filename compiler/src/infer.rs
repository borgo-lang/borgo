use crate::ast::{
    Arm, Binding, Constructor, EnumDefinition, EnumFieldDef, Expr, File, Function, FunctionKind,
    Generic, InterfaceSuperTrait, Literal, Loop, NewtypeDefinition, Operator, Pat, PkgImport,
    SelectArm, SelectArmPat, Span, StructDefinition, StructField, StructFieldDef, StructFieldPat,
    TypeAliasDef, TypeAst, UnOp,
};
use crate::error::{ArityError, Error, UnificationError};
use crate::exhaustive;
use crate::global_state::{GlobalState, Interface, Module, ModuleImport, Visibility};
use crate::parser;
use crate::substitute;
use crate::type_::{Bound, BoundedType, ModuleId, Symbol, Type};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct Infer {
    pub gs: GlobalState,

    scopes: VecDeque<Scope>,

    substitutions: HashMap<i32, Type>,

    // The return type of the current function being inferred
    current_fn_ret_ty: Option<Type>,

    // accumulate all errors encountered during inference
    pub errors: Vec<Error>,

    // all the modules processed during inference
    processed_modules: HashSet<ModuleId>,

    // cache built-in types defined in std
    // we use a RefCell so that type_*() methods only require &self
    cache_builtin_types: RefCell<HashMap<String, Type>>,

    fs: Box<dyn crate::fs::FileSystem>,
}

impl Infer {
    pub fn new(fs: Box<dyn crate::fs::FileSystem>) -> Self {
        let gs = GlobalState::new();

        Self {
            gs,
            scopes: VecDeque::from([Scope::new()]),
            substitutions: Default::default(),
            current_fn_ret_ty: None,
            errors: Default::default(),
            processed_modules: Default::default(),
            cache_builtin_types: Default::default(),
            fs,
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
        // self.gs.dump();
        eprintln!("SUBS {:#?}", self.substitutions);
    }

    pub fn get_errors(&self) -> Vec<Error> {
        self.errors.clone()
    }

    pub fn first_error(&self) -> Option<String> {
        self.get_errors().first().map(|e| {
            let file_id = e.get_span().file;
            let module_id = self
                .gs
                .files
                .get(&file_id)
                .expect("All files should be registered in GlobalState");

            let module = self.gs.get_module(module_id).unwrap();

            let source = &module
                .files
                .get(&file_id)
                .expect("Module must have file")
                .source;

            e.stringify(Some(source))
        })
    }

    pub fn substitute(&mut self, typ: Type) -> Type {
        match typ {
            Type::Var(i) if self.sub(i) != typ => {
                let s = self.sub(i);
                self.substitute(s)
            }

            Type::Var(_) => typ,

            Type::Con { id: name, args } => {
                let new_args = args.into_iter().map(|a| self.substitute(a)).collect();
                Type::Con {
                    id: name,
                    args: new_args,
                }
            }

            Type::Fun { args, bounds, ret } => {
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
                }
            }
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

    pub fn reset_scope(&mut self) {
        self.scopes = VecDeque::from([Scope::new()]);
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        self.scopes[0].types.get(name).cloned()
    }

    pub fn builtin_type(&self, name: &str) -> Type {
        // Check cache first
        if let Some(ty) = self.cache_builtin_types.borrow().get(name).cloned() {
            return ty;
        }

        // Lookup type and populate cache
        let m = ModuleId("std".to_string());
        let sym = &self.scopes[0].types.get(name);

        // If a fatal error occurs when initializing `std`, then we might not find a builtin type.
        if sym.is_none() {
            dbg!(&self.errors);
            panic!("Type {name} not initialized.");
        }

        let sym = sym.unwrap();
        let ty = self.gs.modules[&m].types[&sym].ty.clone();

        // Insert type in cache
        self.cache_builtin_types
            .borrow_mut()
            .insert(name.to_string(), ty.clone());
        ty
    }

    pub fn type_slice(&self, typ: Type) -> Type {
        self.builtin_type("Slice").swap_arg(0, typ)
    }

    pub fn type_map(&self, x: Type, y: Type) -> Type {
        self.builtin_type("Map").swap_arg(0, x).swap_arg(1, y)
    }

    pub fn type_tuple2(&self, x: Type, y: Type) -> Type {
        self.builtin_type("Tuple2").swap_arg(0, x).swap_arg(1, y)
    }

    pub fn type_result(&self, x: Type, y: Type) -> Type {
        self.builtin_type("Result").swap_arg(0, x).swap_arg(1, y)
    }

    pub fn type_reference(&self, typ: Type) -> Type {
        self.builtin_type("Ref").swap_arg(0, typ)
    }

    pub fn type_unit(&self) -> Type {
        self.builtin_type("Unit")
    }
    pub fn type_int(&self) -> Type {
        self.builtin_type("int")
    }
    pub fn type_float(&self) -> Type {
        self.builtin_type("float64")
    }
    pub fn type_string(&self) -> Type {
        self.builtin_type("string")
    }
    pub fn type_char(&self) -> Type {
        self.builtin_type("rune")
    }
    pub fn type_bool(&self) -> Type {
        self.builtin_type("bool")
    }
    pub fn type_any(&self) -> Type {
        self.builtin_type("any")
    }

    pub fn infer_expr(&mut self, expr: Expr, expected: &Type) -> Expr {
        match expr {
            Expr::Literal { lit, span, .. } => match lit {
                Literal::Bool(b) => {
                    self.add_constraint(expected, &self.type_bool(), &span);

                    Expr::Literal {
                        lit: Literal::Bool(b),
                        ty: self.type_bool(),
                        span,
                    }
                }

                Literal::Int(i) => {
                    self.add_constraint(expected, &self.type_int(), &span);

                    Expr::Literal {
                        lit: Literal::Int(i),
                        ty: self.type_int(),
                        span,
                    }
                }

                Literal::Float(i) => {
                    self.add_constraint(expected, &self.type_float(), &span);

                    Expr::Literal {
                        lit: Literal::Float(i),
                        ty: self.type_float(),
                        span,
                    }
                }

                Literal::String(s) => {
                    self.add_constraint(expected, &self.type_string(), &span);

                    Expr::Literal {
                        lit: Literal::String(s),
                        ty: self.type_string(),
                        span,
                    }
                }

                Literal::Char(s) => {
                    self.add_constraint(expected, &self.type_char(), &span);

                    Expr::Literal {
                        lit: Literal::Char(s),
                        ty: self.type_char(),
                        span,
                    }
                }

                Literal::Slice(elems) => {
                    let ty = self.fresh_ty_var();
                    let new_elems = elems.into_iter().map(|e| self.infer_expr(e, &ty)).collect();

                    let ty = self.type_slice(ty);
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
                // NEW SCOPE
                self.begin_scope();

                let mut stmts = stmts.to_vec();
                let was_empty = stmts.is_empty();

                self.declare_stmts(&stmts, &Visibility::Local);

                // Take the last statement so we can run inference on it later
                let last = stmts.pop().unwrap_or_else(|| Expr::Unit {
                    ty: self.type_unit(),
                    span: span.clone(),
                });

                let mut new_stmts: Vec<_> = stmts
                    .iter()
                    .map(|e| {
                        let is_discard_stmt = matches!(e, Expr::If { .. } | Expr::Let { .. });

                        let var = if is_discard_stmt {
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

                self.exit_scope();
                // END NEW SCOPE

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
                self.begin_scope();

                // Bring all generics in scope
                self.put_generics_in_scope(&fun.generics);

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
                        let ty = arg_ty_flowing_in
                            .unwrap_or_else(|| self.to_type(&binding.ann, &binding.pat.get_span()));

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

                let mut new_bounds = vec![];

                for g in &fun.generics {
                    let sym = self.gs.get_symbol(&g.name, &g.span);

                    for b in &g.bounds {
                        let ty = self.to_type(b, &span);
                        self.add_assumption(&sym, &ty);

                        let gen_ty = Type::Con {
                            id: sym.clone(),
                            args: vec![],
                        };

                        new_bounds.push(Bound {
                            generic: gen_ty,
                            ty,
                        });
                    }
                }

                // set return type as the current function being evaluated (used by Return branch)
                let prev_ret_ty = self.current_fn_ret_ty.clone(); // nested functions, save context
                self.current_fn_ret_ty = Some(new_ret.clone());

                let typ = Type::Fun {
                    args: new_args.iter().map(|b| b.ty.clone()).collect(),
                    bounds: new_bounds,
                    ret: new_ret.clone().into(),
                };

                // We don't actually care what the type of the last expression is, if the return
                // type is unit
                let body_ty = if new_ret == self.type_unit() {
                    Type::discard()
                } else {
                    new_ret.clone()
                };

                let new_body = self.infer_expr(*fun.body.clone(), &body_ty);

                self.exit_scope();
                // END NEW SCOPE

                self.current_fn_ret_ty = prev_ret_ty; // Reset current function

                let bounded_ty = typ.to_bounded_with_generics(fun.generics.clone());

                let typ = self.instantiate(&bounded_ty);

                self.add_constraint(expected, &typ, &span);

                Expr::Closure {
                    fun: Function {
                        name: fun.name.clone(),
                        generics: fun.generics.clone(),
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

            Expr::Unit { span, .. } => {
                let new_ty = self.fresh_ty_var();
                self.add_constraint(&new_ty, &self.type_unit(), &span);
                self.add_constraint(expected, &new_ty, &span);
                Expr::Unit { ty: new_ty, span }
            }

            Expr::Var {
                ref value, span, ..
            } => {
                let def = self.get_value(value);

                let def = match def {
                    Some(def) => def,
                    None => {
                        self.generic_error(
                            format!("variable {} not found in env", value),
                            span.clone(),
                        );
                        self.fresh_ty_var().to_bounded()
                    }
                };

                // Instantiate the type manually so we can collect the types and use them later for
                // codegen.
                let generics = self.collect_generics(&def);
                let ty = self.instantiate_with_vars(&def.ty, &generics);

                let mut instances: Vec<_> = generics.into_values().collect();
                instances.sort();
                let instances = instances.into_iter().map(Type::Var).collect();

                self.add_constraint(expected, &ty, &span);

                let decl = self.get_span(value).unwrap_or_else(Span::dummy);

                let resolved = self.resolve_name(&value);
                Expr::Var {
                    value: resolved,
                    decl,
                    generics_instantiated: instances,
                    ty,
                    span,
                }
            }

            Expr::Let {
                binding,
                value,
                mutable,
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

                match &new_binding.pat {
                    Pat::Type { ident, .. } => self.set_mutability(ident, mutable),
                    _ => {
                        if mutable {
                            self.generic_error(
                                "`mut` keyword not supported here".to_string(),
                                span.clone(),
                            );
                        }
                    }
                };

                self.add_constraint(&expected, &self.type_unit(), &span);

                Expr::Let {
                    binding: new_binding,
                    value: new_value.into(),
                    mutable,
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
                    Expr::Noop => false,
                    _ => true,
                };

                let new_cond = self.infer_expr(*cond, &self.type_bool());
                let new_then = self.infer_expr(*then, &then_ty);
                let new_els = self.infer_expr(*els, &else_ty);

                // Check if we are in discard mode (ie. block statements, loop bodies)
                let discard_mode = expected.is_discard();

                // In case we are not discarding, then we need an else block
                // and the types of the then and else branches must unify
                if !discard_mode {
                    if !has_else && expected != &self.type_unit() {
                        self.generic_error(
                            "If expression must have an else branch".to_string(),
                            span.clone(),
                        );
                    }

                    self.add_constraint(expected, &then_ty, &new_then.get_span());
                    self.add_constraint(expected, &else_ty, &new_els.get_span());
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

                let new_arms = arms
                    .into_iter()
                    .map(|a| {
                        // NEW SCOPE
                        self.begin_scope();

                        // each arm.pat should unify with subject
                        let expected = self.substitute(subject_ty.clone());
                        let new_pat = self.infer_pat(a.pat, expected);

                        self.ensure_not_slice_literal(&new_pat);

                        // each arm.expr should unify with ret
                        let new_expr = self.infer_expr(a.expr, &ret);

                        // EXIT SCOPE
                        self.exit_scope();

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
                // resolve types first
                //   - either a constructor on an enum
                //   - a static function on a type
                if let Some(new_expr) =
                    self.infer_field_access_as_constructor(*expr.clone(), &field, &span)
                {
                    return self.infer_expr(new_expr, expected);
                }

                // it's a struct or pkg
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);
                let ty = self.substitute(ty);

                // 1. check if it's a struct
                //      -> yes, check for field
                // 2. check if it's a package
                // 3. otherwise lookup method

                let inner_ty = ty.remove_references();

                let def = inner_ty.get_name().and_then(|name| {
                    // use symbol instead of name so that non-local structs can be resolved
                    self.get_struct(&inner_ty.get_symbol())
                        .or_else(|| self.get_package(&name))
                });

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

                let method = self.get_method(&ty, &field);

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

            Expr::EnumDef { def, span } => {
                let sym = self.gs.get_symbol(&def.name, &span);
                let actual_def = &self.gs.module().enums[&sym];

                Expr::EnumDef {
                    def: actual_def.clone(),
                    span,
                }
            }

            Expr::StructDef { def, span } => {
                let sym = self.gs.get_symbol(&def.name, &span);
                let actual_def = &self.gs.module().structs[&sym];

                Expr::StructDef {
                    def: actual_def.clone(),
                    span,
                }
            }

            Expr::TypeAlias { def, span } => {
                if def.is_native() {
                    return Expr::TypeAlias { def, span };
                }

                let sym = self.gs.get_symbol(&def.name, &span);
                let actual_def = &self.gs.module().aliases[&sym];

                Expr::TypeAlias {
                    def: actual_def.clone(),
                    span,
                }
            }

            Expr::ImplBlock {
                ann,
                ty: _,
                items,
                self_name,
                generics,
                span,
            } => {
                self.begin_scope();

                self.put_generics_in_scope(&generics);

                let existing = self.to_type(&ann, &span);

                // put the name in scope while parsing items
                self.add_value(
                    &self_name,
                    existing.to_bounded_with_generics(generics.clone()),
                    &span,
                );

                // self should be mutable
                self.set_mutability(&self_name, true);

                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                self.exit_scope();

                Expr::ImplBlock {
                    ann,
                    ty: existing,
                    self_name,
                    items: new_items,
                    generics,
                    span,
                }
            }

            Expr::Trait {
                name,
                generics,
                items,
                supertraits,
                span,
            } => {
                let new_items = items
                    .into_iter()
                    .map(|e| {
                        let ty = self.fresh_ty_var();
                        self.infer_expr(e, &ty)
                    })
                    .collect();

                let new_supertraits = supertraits
                    .into_iter()
                    .map(|s| {
                        let ty = self.to_type(&s.ann, &s.span);
                        InterfaceSuperTrait {
                            ann: s.ann,
                            span: s.span,
                            ty,
                        }
                    })
                    .collect();

                Expr::Trait {
                    name,
                    generics,
                    items: new_items,
                    supertraits: new_supertraits,
                    span,
                }
            }

            Expr::VarUpdate {
                target,
                value,
                span,
            } => {
                let target_ty = self.fresh_ty_var();
                let new_target = self.infer_expr(*target, &target_ty);

                let value_ty = self.fresh_ty_var();
                let new_value = self.infer_expr(*value, &value_ty);

                if let Some(var_name) = new_target.as_var_name() {
                    // This is not really correct, we should check whether the target var is a RefMut.
                    let is_deref = matches!(new_target, Expr::Unary { .. });
                    if !self.get_mutability(&var_name) && !is_deref {
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
                let inferred = self.type_result(ok_ty.clone(), err_ty.clone());
                let new_expr = self.infer_expr(*expr, &inferred);

                // constraint the function we're currently processing to be a Result
                // with the same error type
                let fn_ret = self.current_fn_ret_ty.clone().unwrap();
                let expected_ok = self.fresh_ty_var();
                let fn_expected = self.type_result(expected_ok, err_ty);

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
                    | Operator::Ge => self.type_bool(),

                    _ => left_ty.clone(),
                };

                self.add_constraint(expected, &target, &span);

                match op {
                    Operator::And | Operator::Or => {
                        self.add_constraint(&left_ty, &self.type_bool(), &span);
                        self.add_constraint(&right_ty, &self.type_bool(), &span);
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

                    UnOp::Not => self.type_bool(),
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
                let target = self.to_type(&ann, &span);
                // let target = self.instantiate(&target);

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

            Expr::Loop { kind, body, span } => {
                // All loops must unify with Unit
                self.add_constraint(expected, &self.type_unit(), &span);

                match kind {
                    Loop::NoCondition => {
                        let body_ty = Type::discard();
                        let new_body = self.infer_expr(*body, &body_ty);

                        Expr::Loop {
                            kind,
                            body: new_body.into(),
                            span,
                        }
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
                                self.type_tuple2(self.type_int(), expr_args[0].clone())
                            }

                            "Map" => {
                                // (key, value) for maps
                                self.type_tuple2(expr_args[0].clone(), expr_args[1].clone())
                            }

                            "Receiver" => expr_args[0].clone(),

                            "string" => self.type_char(),

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
                                        "Use tuple literals \"(k, v)\" in loops.".to_string(),
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
                        let new_expr = self.infer_expr(*expr, &self.type_bool());
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

            Expr::Reference {
                expr,
                mutable,
                span,
                ..
            } => {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(*expr, &ty);

                let wrapped = self.type_reference(ty);
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
                    (self.type_int(), self.type_slice(inner_ty.clone()))
                } else if ty_name == "Map" {
                    // maps can be indexed by whatever the key type is
                    let key_ty = &expr_ty.get_args().unwrap()[0];

                    (
                        key_ty.clone(),
                        self.type_map(key_ty.clone(), inner_ty.clone()),
                    )
                } else {
                    self.generic_error(
                        format!("Only slices and maps can be indexed. Got {}", expr_ty),
                        new_expr.get_span(),
                    );
                    (self.fresh_ty_var(), self.fresh_ty_var())
                };

                self.add_constraint(&collection_ty, &expr_ty, &span);
                self.add_constraint(&expected_index, &index_ty, &span);
                self.add_constraint(expected, &inner_ty, &span);

                Expr::Index {
                    expr: new_expr.into(),
                    index: new_index.into(),
                    ty: inner_ty,
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

                let new_expr = self.infer_expr(*expr, &Type::discard());

                let ret = self.type_unit();
                self.add_constraint(expected, &ret, &span);

                Expr::Spawn {
                    expr: new_expr.into(),
                    ty: ret,
                    span,
                }
            }

            Expr::Select { arms, span, .. } => {
                // Each arm is inferred separately
                let new_arms = arms
                    .into_iter()
                    .map(|a| {
                        // NEW SCOPE
                        self.begin_scope();

                        let new_pat = match a.pat {
                            SelectArmPat::Recv(binding, expr) => {
                                let new_expr = self.ensure_channel_call("Recv", expr);

                                let ty = self.to_type(&binding.ann, &span);
                                let new_binding = Binding {
                                    pat: self.infer_pat(binding.pat, Type::discard()),
                                    ty: ty.clone(),
                                    ..binding
                                };

                                SelectArmPat::Recv(new_binding, new_expr)
                            }

                            SelectArmPat::Send(expr) => {
                                let new_expr = self.ensure_channel_call("Send", expr);
                                SelectArmPat::Send(new_expr)
                            }

                            SelectArmPat::Wildcard => SelectArmPat::Wildcard,
                        };

                        // the resulting type from arm.expr can be discarded
                        let new_expr = self.infer_expr(a.expr, &Type::discard());

                        // EXIT SCOPE
                        self.exit_scope();

                        SelectArm {
                            pat: new_pat,
                            expr: new_expr,
                        }
                    })
                    .collect();

                Expr::Select {
                    arms: new_arms,
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
                self.add_constraint(expected, &self.type_unit(), &span);

                Expr::Defer {
                    expr: new_expr.into(),
                    ty: expected.clone(),
                    span,
                }
            }

            Expr::UsePackage { import, span } => Expr::UsePackage { import, span },
            Expr::NewtypeDef { def, span } => {
                // let sym = self.gs.get_symbol(&def.name, &span);
                // let actual_def = &self.gs.module().newtypes[&sym];
                // TODO should populate module.newtypes
                let actual_def = def.clone();

                Expr::NewtypeDef {
                    def: actual_def.clone(),
                    span,
                }
            }

            Expr::Flow { kind, span } => Expr::Flow { kind, span },
            Expr::Raw { text } => Expr::Raw { text },
            Expr::Noop => Expr::Noop,
            Expr::Todo => todo!(),

            _ => todo!("uncovered infer_expr {:?}", expr),
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

            Type::Con { id, args } => match new_vars.get(&id.name) {
                Some(id) => Type::Var(id.to_owned()),
                None => Type::Con {
                    id: id.clone(),
                    args: args
                        .iter()
                        .map(|x| self.instantiate_with_vars(x, new_vars))
                        .collect(),
                },
            },

            Type::Fun { args, bounds, ret } => Type::Fun {
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
                self.add_value(&ident, expected.to_bounded(), &span);

                // Tracking mutability is a bit all over the place right now.
                // Always allow mutation of variables, like in Go.
                // self.set_mutability(&ident, is_mut);
                self.set_mutability(&ident, true);

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
                let def = match self.get_value(ident) {
                    Some(def) => def,
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
                let new_ident = self.resolve_name(ident);
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

                let new_ident = self.resolve_name(ident);

                Pat::Struct {
                    ident: new_ident,
                    fields: new_fields,
                    ty: instantiated_ty,
                    span: span.clone(),
                }
            }

            Pat::Wild { span } => Pat::Wild { span },
            Pat::Unit { span, .. } => {
                let new_ty = self.fresh_ty_var();
                self.add_constraint(&new_ty, &self.type_unit(), &span);
                self.add_constraint(&expected, &new_ty, &span);
                Pat::Unit { ty: new_ty, span }
            }
        }
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

        // Don't bother matching exact numeric types.
        // Let the Go compiler do the heavy lifting here, mostly as we don't support type sets.
        if self.is_numeric(t1) && self.is_numeric(t2) {
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
            (Type::Con { id: id1, args: a1 }, Type::Con { id: id2, args: a2 }) => {
                let n1 = id1.name.as_str();
                let n2 = id2.name.as_str();

                if n1 == "any" || n2 == "any" {
                    return Ok(());
                }

                if id1 != id2 {
                    match self.check_interface_impl(id1, id2) {
                        CheckInterfaceResult::Ok => return Ok(()),
                        CheckInterfaceResult::NotAnInterface => (),
                        CheckInterfaceResult::Error(e) => return Err(e),
                    }

                    match self.check_interface_impl(id2, id1) {
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
            }

            // Matching functions with constructors is always an error, unless the type is any
            (Type::Fun { .. }, Type::Con { .. }) | (Type::Con { .. }, Type::Fun { .. }) => {
                if t1 == &self.type_any() || t2 == &self.type_any() {
                    return Ok(());
                }

                Err("Type mismatch".to_string())
            } /*
              _ => {
                  self.dump();
                  todo!("{:?}, {:?}", t1, t2)
              }
              */
        }
    }

    fn generic_error(&mut self, msg: String, span: Span) {
        let err = Error::Generic(msg, span);
        self.errors.push(err);
    }

    // Allow users to omit the error type, ie. Result<T>
    // This function will add the error type back -> Result<T, E>
    fn add_optional_error_to_result(&mut self, ty: &Type, args: &[TypeAst]) -> Vec<TypeAst> {
        if let Type::Con { id, .. } = ty {
            if id.name == "Result" && args.len() == 1 {
                let mut new_args = args.to_vec();

                let error_ty = TypeAst::Con {
                    name: "error".to_string(),
                    args: vec![],
                };
                new_args.push(error_ty);

                return new_args;
            }
        }

        args.to_vec()
    }

    fn check_reserved_name(&mut self, ident: &str) -> bool {
        RESERVED_WORDS.contains(&ident)
    }

    fn check_numeric(&mut self, ty: &Type, span: &Span) {
        if !self.is_numeric(&ty) {
            self.generic_error("Expected numeric type".to_string(), span.clone());
        }
    }

    // temporary hacky function to gather together all numeric types.
    // conversions between the specific types are left as an exercises to the go compiler.
    pub fn is_numeric(&mut self, ty: &Type) -> bool {
        let ty = self.substitute(ty.clone());

        match ty {
            Type::Con { id: sym, .. } => {
                if NUMERIC_TYPES.contains(&sym.name.as_str()) {
                    return true;
                }

                let def = self.gs.get_newtype(&sym);

                // If it's a newtype, check the underlying type
                if let Some(def) = def {
                    return self.is_numeric(&def.fields[0].ty);
                }

                false
            }
            _ => false,
        }
    }

    // This is only used in tests.
    pub fn infer_expr_with_error(&mut self, e: &Expr) -> (Expr, Vec<Error>, Type) {
        let m = Module::user();
        self.gs.set_module(m.id.clone());
        self.gs.modules.insert(m.id.clone(), m);

        let ty = self.fresh_ty_var();
        let expr = self.infer_expr(e.clone(), &ty);

        let ty = self.substitute(ty);
        let expr = substitute::substitute_expr(expr, self);
        let errors = self.errors.clone();

        (expr, errors, ty)
    }

    pub fn declare_stmts(&mut self, stmts: &[Expr], vis: &Visibility) {
        let result = self
            .declare_symbols(stmts, vis)
            .and_then(|_| self.declare_types(stmts))
            .and_then(|_| self.declare_values(stmts, vis));

        if let Err(err) = result {
            self.errors.push(err);
        }
    }

    pub fn declare_symbols(&mut self, decls: &[Expr], vis: &Visibility) -> Result<(), Error> {
        for e in decls.iter() {
            match e {
                Expr::TypeAlias { def, span } => {
                    let sym = self.gs.get_symbol(&def.name, span);
                    let args = self.declare_generics(&def.generics);

                    let ty = Type::Con {
                        id: sym.clone(),
                        args,
                    };

                    self.gs.module().visibility.insert(sym.clone(), vis.clone());

                    // Add as actual type
                    self.gs.module().types.insert(
                        sym.clone(),
                        ty.to_bounded_with_generics(def.generics.to_owned()),
                    );
                }

                Expr::EnumDef { def, span } => {
                    let sym = self.gs.get_symbol(&def.name, span);
                    let args = self.declare_generics(&def.generics);

                    let ty = Type::Con {
                        id: sym.clone(),
                        args,
                    };

                    self.gs.module().types.insert(
                        sym.clone(),
                        ty.to_bounded_with_generics(def.generics.to_owned()),
                    );

                    self.gs.module().visibility.insert(sym.clone(), vis.clone());
                }

                Expr::StructDef { def, span } => {
                    let sym = self.gs.get_symbol(&def.name, span);
                    let args = self.declare_generics(&def.generics);

                    let ty = Type::Con {
                        id: sym.clone(),
                        args,
                    };

                    self.gs.module().types.insert(
                        sym.clone(),
                        ty.to_bounded_with_generics(def.generics.to_owned()),
                    );

                    self.gs
                        .module()
                        .visibility
                        .insert(sym.clone(), Visibility::TopLevel);
                }

                Expr::NewtypeDef { def, span } => {
                    let sym = self.gs.get_symbol(&def.name, span);
                    let args = self.declare_generics(&def.generics);

                    let ty = Type::Con {
                        id: sym.clone(),
                        args,
                    };

                    self.gs.module().types.insert(
                        sym.clone(),
                        ty.to_bounded_with_generics(def.generics.to_owned()),
                    );

                    self.gs
                        .module()
                        .visibility
                        .insert(sym.clone(), Visibility::TopLevel);
                }

                Expr::Trait { name, span, .. } => {
                    let sym = self.gs.get_symbol(&name, span);

                    let ty = Type::Con {
                        id: sym.clone(),
                        args: vec![],
                    };

                    self.gs.module().types.insert(sym.clone(), ty.to_bounded());

                    self.gs
                        .module()
                        .visibility
                        .insert(sym.clone(), Visibility::TopLevel);
                }

                _ => (),
            }
        }

        Ok(())
    }

    pub fn declare_types(&mut self, decls: &[Expr]) -> Result<(), Error> {
        for e in decls.iter() {
            match e {
                Expr::EnumDef { def, span } => self.declare_enum(def, span),
                Expr::StructDef { def, span } => self.declare_struct(def, span),
                Expr::ImplBlock {
                    ann,
                    items,
                    generics,
                    span,
                    ..
                } => self.declare_impl(ann, generics, items, span),
                Expr::Trait {
                    name,
                    generics,
                    supertraits,
                    items,
                    span,
                } => self.declare_trait(name, generics, supertraits, items, span),

                Expr::TypeAlias { def, span } => self.declare_type_alias(def, span),
                Expr::NewtypeDef { def, span } => self.declare_newtype(def, span),
                _ => (),
            }
        }

        Ok(())
    }

    fn declare_enum(&mut self, def: &EnumDefinition, span: &Span) {
        let sym = self.gs.get_symbol(&def.name, span);

        // get the type that was previously declared
        let ty = self.gs.module().types[&sym].clone();

        // declare constructors

        let new_cons = def
            .cons
            .iter()
            .map(|c| {
                let name = c.name.clone();
                let _qualified = c.to_qualified(&def.name);

                if c.fields.is_empty() {
                    return Constructor {
                        name,
                        fields: vec![],
                    };
                }

                let new_fields: Vec<_> = c
                    .fields
                    .iter()
                    .map(|f| {
                        self.begin_scope();

                        self.put_generics_in_scope(&def.generics);

                        let typ = self.to_type(&f.ann, span);

                        self.exit_scope();

                        EnumFieldDef {
                            ty: typ,
                            ..f.clone()
                        }
                    })
                    .collect();

                Constructor {
                    name,
                    fields: new_fields,
                }
            })
            .collect();

        for c in &new_cons {
            let c: &Constructor = c; // cannot infer type, damn

            let mut cons_ty = ty.clone();

            // If there are fields, turn it into a function.
            // TODO this is all the same as declare_enum
            if !c.fields.is_empty() {
                let fun = Type::Fun {
                    args: c.fields.clone().into_iter().map(|f| f.ty).collect(),
                    bounds: Default::default(),
                    ret: ty.ty.clone().into(),
                };

                cons_ty = fun.to_bounded_with_generics(def.generics.to_owned());
            }

            self.add_constructor(&def.name, &c.name, &cons_ty, &sym.span);
        }

        let new_def = EnumDefinition {
            name: def.name.clone(),
            generics: def.generics.clone(),
            cons: new_cons,
        };

        // end constructors

        self.add_type(&def.name, &sym);

        self.gs.module().enums.insert(sym.clone(), new_def);
    }

    fn declare_struct(&mut self, def: &StructDefinition, span: &Span) {
        let sym = self.gs.get_symbol(&def.name, span);

        // Declare fields
        let new_fields = def
            .fields
            .iter()
            .map(|f| {
                self.begin_scope();

                self.put_generics_in_scope(&def.generics);

                let typ = self.to_type(&f.ann, span);

                self.exit_scope();

                StructFieldDef {
                    ty: typ.to_bounded(), // should this care about generics?
                    ..f.clone()
                }
            })
            .collect();

        let new_def = StructDefinition {
            name: def.name.clone(),
            generics: def.generics.clone(),
            fields: new_fields,
        };

        // end fields

        self.add_type(&def.name, &sym);

        self.gs.module().structs.insert(sym.clone(), new_def);
    }

    fn declare_impl(&mut self, ann: &TypeAst, generics: &[Generic], items: &[Expr], span: &Span) {
        self.declare_generics(generics);

        items.iter().for_each(|e| {
            self.begin_scope();

            self.put_generics_in_scope(generics);

            let base_ty = self.to_type(ann, span);

            let ty = self.fresh_ty_var();

            let fun = e.as_function();
            let new_expr = self.infer_expr(self.create_empty_func(&fun, span), &ty);
            let new_func = new_expr.as_function();
            let method_ty = new_func.add_receiver(generics, &base_ty);

            self.exit_scope();

            // Add method to type

            let m = self.gs.module();
            let methods = m
                .methods
                .entry(base_ty.get_symbol())
                .or_insert(HashMap::new());

            methods.insert(fun.name.clone(), method_ty.clone());

            // Add standalone function
            // ie. Option.IsSome(...)
            {
                let span = new_expr.get_span();
                let name = format!("{}.{}", base_ty.get_name().unwrap(), fun.name);
                let sym = self.gs.get_symbol(&name, &span);

                self.add_value(&name, method_ty.clone(), &span);

                self.gs.module().values.insert(sym, method_ty);
            }
        });
    }

    fn declare_trait(
        &mut self,
        name: &str,
        generics: &[Generic],
        supertraits: &[InterfaceSuperTrait],
        items: &[Expr],
        span: &Span,
    ) {
        let mut methods: HashMap<String, Type> = HashMap::new();
        let mut new_supertraits = vec![];

        for s in supertraits {
            // self.begin_scope();
            // generics need to be instantiated
            // self.put_generics_in_scope(generics);
            let ty = self.to_type(&s.ann, &s.span);
            new_supertraits.push(ty);
        }

        // Declare interface methods
        self.begin_scope();
        self.put_generics_in_scope(generics);

        for e in items {
            let fun = e.as_function();

            let ty = self.fresh_ty_var();
            let expr = self.infer_expr(e.clone(), &ty);

            methods.insert(fun.name, expr.get_type());
        }

        self.exit_scope();

        let sym = self.gs.get_symbol(name, span);

        let interface = Interface {
            name: name.to_string(),
            generics: generics.to_owned(),
            supertraits: new_supertraits,
            methods,
        };

        self.add_type(name, &sym);

        self.gs.module().interfaces.insert(sym.clone(), interface);
    }

    fn declare_type_alias(&mut self, def: &TypeAliasDef, span: &Span) {
        let sym = self.gs.get_symbol(&def.name, span);

        if def.is_native() {
            self.add_type(&def.name, &sym);
        } else {
            // it's a type alias

            self.begin_scope();

            self.put_generics_in_scope(&def.generics);

            let ty = self.to_type(&def.ann, span);
            let new_ty = ty.to_bounded_with_generics(def.generics.clone());

            self.exit_scope();

            let new_def = TypeAliasDef {
                ty: new_ty.clone(),
                ..def.clone()
            };

            self.gs.module().aliases.insert(sym.clone(), new_def);

            // We need to register this again: this time, the type is fully resolved from the
            // annotation so it's pointing to the actual aliased type.
            self.gs.module().types.insert(sym.clone(), new_ty);
        }
    }

    fn declare_newtype(&mut self, def: &NewtypeDefinition, span: &Span) {
        let sym = self.gs.get_symbol(&def.name, span);

        let mut func_args = vec![];

        let new_fields = def
            .fields
            .iter()
            .enumerate()
            .map(|(index, f)| {
                self.begin_scope();

                self.put_generics_in_scope(&def.generics);

                let typ = self.to_type(&f.ann, span);

                self.exit_scope();

                let name = Expr::tuple_index_string(index.try_into().unwrap()).unwrap();

                func_args.push(typ.clone());

                StructFieldDef {
                    name,
                    ann: f.ann.clone(),
                    ty: typ.to_bounded(),
                }
            })
            .collect();

        let new_def = StructDefinition {
            name: def.name.clone(),
            generics: def.generics.clone(),
            fields: new_fields,
        };

        let ty = Type::Con {
            id: sym.clone(),
            args: def
                .generics
                .iter()
                .map(|g| {
                    let gen_sym = self.gs.get_symbol(&g.name, &g.span);
                    Type::Con {
                        id: gen_sym,
                        args: vec![],
                    }
                })
                .collect(),
        };

        let constructor = Type::Fun {
            args: func_args,
            bounds: vec![],
            ret: ty.clone().into(),
        }
        .to_bounded_with_generics(def.generics.clone());

        let ty_bounded = ty.to_bounded_with_generics(def.generics.clone());

        // given struct Foo(int, bool)
        // - add value Foo => Fun { args: [int, bool], ret: Foo }
        // - add struct Foo => { first: int, second: bool }

        self.add_type(&def.name, &sym);
        self.add_value(&def.name, constructor, span);

        self.gs
            .module()
            .types
            .insert(sym.clone(), ty_bounded.clone());

        self.gs.module().values.insert(sym.clone(), ty_bounded);

        self.gs
            .module()
            .newtypes
            .insert(sym.clone(), new_def.to_newtype_def());

        self.gs.module().structs.insert(sym.clone(), new_def);

        self.gs
            .module()
            .visibility
            .insert(sym.clone(), Visibility::TopLevel);
    }

    fn create_empty_func(&self, fun: &Function, span: &Span) -> Expr {
        let mut new_func = fun.clone();
        new_func.body = Expr::Noop.into();

        Expr::Closure {
            fun: new_func,
            kind: FunctionKind::TopLevel, // TODO asdf change with visibility
            ty: Type::dummy(),
            span: span.clone(),
        }
    }

    pub fn declare_values(&mut self, decls: &[Expr], _vis: &Visibility) -> Result<(), Error> {
        for e in decls.iter() {
            match e {
                Expr::Closure { fun, span, .. } => {
                    let ty = self.fresh_ty_var();

                    self.declare_generics(&fun.generics);

                    let new_expr = self.infer_expr(self.create_empty_func(fun, span), &ty);
                    let new_func = new_expr.as_function();

                    let sym = self.gs.get_symbol(&fun.name, span);

                    self.add_value(&fun.name, new_func.bounded_ty.clone(), span);

                    self.gs
                        .module()
                        .values
                        .insert(sym.clone(), new_func.bounded_ty.clone());
                }

                Expr::Const {
                    ident, ann, span, ..
                } => {
                    let sym = self.gs.get_symbol(ident, span);
                    let ty = self.to_type(ann, span).to_bounded();

                    self.gs.module().values.insert(sym.clone(), ty);
                }

                _ => (),
            }
        }

        Ok(())
    }

    fn declare_generics(&mut self, generics: &[Generic]) -> Vec<Type> {
        generics
            .iter()
            .map(|g| {
                let sym = self.gs.get_symbol(&g.name, &g.span);
                let ty = Type::Con {
                    id: sym.clone(),
                    args: vec![],
                };

                // Declare the generic type on the module
                self.gs.module().types.insert(sym.clone(), ty.to_bounded());

                ty
            })
            .collect()
    }

    fn add_type(&mut self, name: &str, sym: &Symbol) {
        self.current_scope()
            .types
            .insert(name.to_string(), sym.clone());
    }

    pub fn get_type(&mut self, name: &str) -> Option<BoundedType> {
        let sym = self.resolve(name)?;
        self.gs.get_type(&sym)
    }

    pub fn add_value(&mut self, name: &str, ty: BoundedType, span: &Span) {
        self.current_scope().values.insert(name.to_string(), ty);
        self.current_scope()
            .spans
            .insert(name.to_string(), span.clone());
    }

    fn get_value(&mut self, name: &str) -> Option<BoundedType> {
        let lookup = self.resolve_name(name);
        self.scopes[0].values.get(&lookup).cloned()
    }

    fn get_span(&mut self, name: &str) -> Option<Span> {
        self.current_scope().spans.get(name).cloned()
    }

    fn get_method(&mut self, base_ty: &Type, method: &str) -> Option<BoundedType> {
        match base_ty.remove_references() {
            Type::Con { id, args: _ } => self.get_type_methods(&id).get(method).cloned(),
            _ => None,
        }
    }

    fn get_type_methods(&mut self, sym: &Symbol) -> HashMap<String, BoundedType> {
        // If it's an interface, return all methods
        if let Some(interface) = self.gs.get_interface(&sym) {
            let mut ret = HashMap::new();

            for (name, ty) in interface.methods {
                ret.insert(name, self.add_any_receiver(ty).to_bounded());
            }

            // Also include any methods of supertypes
            for s in interface.supertraits {
                let methods = self.get_type_methods(&s.get_symbol());

                for (name, ty) in methods {
                    ret.insert(name, ty);
                }
            }

            return ret;
        }

        // If it's a constraint, return all the methods of all the interfaces implied by that
        // constraint
        if let Some(ifaces) = self.get_assumptions(sym) {
            return ifaces
                .into_iter()
                .flat_map(|i| self.get_type_methods(&i))
                .collect();
        }

        // Otherwise get all the methods defined on the type
        self.gs.get_methods(&sym).unwrap_or_default()
    }

    pub fn add_assumption(&mut self, sym: &Symbol, bound: &Type) {
        self.current_scope()
            .assumptions
            .entry(sym.clone())
            .or_default()
            .push(bound.get_symbol());
    }

    fn get_assumptions(&self, sym: &Symbol) -> Option<Vec<Symbol>> {
        self.scopes[0].assumptions.get(sym).cloned()
    }

    // shouldn't take a mut reference, but this whole constructors business is probably wrong
    // anyway
    fn resolve_name(&mut self, name: &str) -> String {
        self.current_scope()
            .constructors
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_owned())
    }

    fn set_mutability(&mut self, name: &str, is_mut: bool) {
        self.current_scope()
            .mutability
            .insert(name.to_string(), is_mut);
    }

    fn get_mutability(&mut self, name: &str) -> bool {
        self.current_scope()
            .mutability
            .get(name)
            .cloned()
            .unwrap_or_default()
    }

    fn get_struct(&mut self, sym: &Symbol) -> Option<(StructDefinition, BoundedType)> {
        let def = self.gs.get_struct(&sym)?;
        let ty = self.gs.get_type(&sym)?;

        Some((def.clone(), ty.clone()))
    }

    fn get_struct_by_name(&mut self, name: &str) -> Option<(StructDefinition, BoundedType)> {
        let sym = self.resolve(name)?;
        self.get_struct(&sym)
    }

    pub fn put_generics_in_scope(&mut self, generics: &[Generic]) {
        for g in generics {
            let sym = self.gs.get_symbol(&g.name, &g.span);
            self.add_type(&g.name, &sym);
        }
    }

    // Turn an annotation into a non-instantiated type (generics are still in the type).
    // The caller is responsible to call instantiate().
    fn to_type(&mut self, ann: &TypeAst, span: &Span) -> Type {
        match ann {
            TypeAst::Con { name, args } => {
                let existing = self.get_type(name);

                if existing.is_none() {
                    // dbg!(&self.scopes.front().unwrap().types);
                    // dbg!(&self.gs.modules);
                    self.generic_error(format!("Type not found: {name}"), span.clone());
                    return self.fresh_ty_var();
                }

                let existing = existing.unwrap();

                // This bit is quite involved in order to support type aliases.
                // For example, given an alias Foo<V> = Map<int, V>
                // We'll get as an input something like Con { Foo, [string] } and need to return
                // Map<int, string>
                //
                // In order to do that we:
                // - collect all generics in a hashmap (generic => var) [V => 123]
                // - instantiate the type with those vars Con { Map, [int, 123] }
                // - create a lookup hashmap $vars [123 => V]
                // - for each generic, get the corresponding argument at that index
                //      - var = generics[V] // 123
                //      - args[0] = string
                //      - so we insert: vars[var] = string
                // at this point we need to replace the new vars in the instantiated type
                // We have:
                //  instantiated = Con { Map, [int, 123] }
                //  vars = [ 123 => string ]
                //  therefore the type is Con { Map, [int, string] }
                let generics = self.collect_generics_as_vars(&existing.generics);
                let instantiated = self.instantiate_with_vars(&existing.ty, &generics);
                let new_args = self.add_optional_error_to_result(&existing.ty, args);

                // i32 => Type
                let mut vars: HashMap<i32, Type> = generics
                    .iter()
                    .map(|(gen, var)| (*var, Type::ethereal(gen)))
                    .collect();

                // Restore the types we know about. This is necessary to support type aliases.
                for (index, g) in existing.generics.iter().enumerate() {
                    let var_index = generics[g];

                    // we should always find an argument at this index, unless the type is
                    // malformed
                    if let Some(arg_ann) = new_args.get(index) {
                        vars.insert(var_index, self.to_type(arg_ann, span));
                    }
                }

                let expected_args = existing.generics;

                if expected_args.len() != new_args.len() {
                    let err = ArityError {
                        expected: expected_args.iter().map(|g| Type::ethereal(g)).collect(),
                        actual: new_args.iter().map(|ann| self.to_type(ann, span)).collect(),
                        span: span.clone(),
                    };

                    self.errors.push(Error::WrongArity(err));
                }

                Type::replace_vars_with_type(&instantiated, &vars)
            }

            TypeAst::Fun { args, ret } => {
                let new_args = args.iter().map(|a| self.to_type(a, span)).collect();
                let new_ret = self.to_type(ret, span);

                Type::Fun {
                    args: new_args,
                    bounds: Default::default(),
                    ret: new_ret.into(),
                }
            }

            TypeAst::Unknown => self.fresh_ty_var(),
        }
    }

    pub fn import_module(&mut self, prefix: Option<String>, module: &ModuleId) {
        let m = self.gs.get_module(module).unwrap();

        let to_name = |name: &str| -> String {
            match &prefix {
                Some(prefix) => format!("{}.{}", prefix, name),
                None => name.to_string(),
            }
        };

        for (sym, _ty) in &m.types {
            if m.is_exported(sym) {
                self.add_type(&to_name(&sym.name), sym)
            }
        }

        match &prefix {
            // If we're importing with a prefix, then create a struct
            // to hold all values in a module (ie. fmt.Println)
            Some(prefix) => self.create_pkg_struct(prefix, &m),

            // Otherwise bring in all values as top level bindings
            None => {
                for (sym, ty) in m.values {
                    self.add_value(&to_name(&sym.name), ty, &sym.span);
                }
            }
        }

        for (sym, def) in m.enums {
            let ty = m.types[&sym].clone();

            for c in def.cons {
                let mut cons_ty = ty.clone();

                // If there are fields, turn it into a function.
                // TODO this is all the same as declare_enum
                if !c.fields.is_empty() {
                    let fun = Type::Fun {
                        args: c.fields.clone().into_iter().map(|f| f.ty).collect(),
                        bounds: Default::default(),
                        ret: ty.ty.clone().into(),
                    };

                    cons_ty = fun.to_bounded_with_generics(def.generics.to_owned());
                }

                self.add_constructor(&def.name, &c.name, &cons_ty, &sym.span);
            }
        }
    }

    pub fn add_any_receiver(&self, ty: Type) -> Type {
        match &ty {
            Type::Fun { args, bounds, ret } => {
                let mut new_args = vec![self.type_any()];
                new_args.extend_from_slice(args);

                Type::Fun {
                    args: new_args,
                    bounds: bounds.clone(),
                    ret: ret.clone(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn check_interface_impl(&mut self, id1: &Symbol, id2: &Symbol) -> CheckInterfaceResult {
        let interface = self.gs.get_interface(&id1);

        if interface.is_none() {
            return CheckInterfaceResult::NotAnInterface;
        }

        let interface = interface.unwrap();
        let methods = self.get_type_methods(id2);

        for (method_name, method_ty) in interface.methods {
            let candidate = methods.get(&method_name);

            if candidate.is_none() {
                return CheckInterfaceResult::Error(format!(
                    "method {} not found on type {}",
                    method_name, id2.name
                ));
            }

            // Add a self receiver param on the interface method
            let interface_method = self.add_any_receiver(method_ty.clone());

            // Check the two unify
            if let Err(e) = self.unify(&interface_method, &candidate.unwrap().ty, &Span::dummy()) {
                return CheckInterfaceResult::Error(format!(
                    "method {} on type {} has wrong type {}",
                    method_name, id2.name, e
                ));
            }
        }

        CheckInterfaceResult::Ok
    }

    pub fn file_from_source(&mut self, name: &str, source: &str) {
        let id = self.gs.new_file();
        let res = parser::Parser::lex_and_parse_file(source, id);

        let decls = match res {
            Ok(decls) => decls,

            Err(err) => {
                // Push parse errors
                self.errors.push(Error::Parse(err));
                vec![]
            }
        };

        let file = File {
            id,
            name: name.to_string(),
            source: source.to_string(),
            decls,
        };

        self.gs.module().files.insert(id, file);
    }

    pub fn module_from_folder(&mut self, import: ModuleImport, mode: DeclareMode) {
        // If the module has already been processed, then skip
        if self.processed_modules.contains(&import.to_id()) {
            return;
        }

        // Set this module as processed
        self.processed_modules.insert(import.to_id());

        // Save previous module so we can restore it later
        let prev_mod = self.gs.new_module(&import);

        // First ingest all the files
        let files = self.fs.scan_folder(&import.path);
        for (name, source) in &files {
            self.file_from_source(&name, &source);
        }

        // Then declare and infer module
        let m = ModuleId::from_str(&import.name);
        self.declare_module(&m);

        // Only perform inference for user and std, for now
        if matches!(mode, DeclareMode::Full) {
            self.infer_module(&m);
        }

        // Restore previous module
        self.gs.set_module(prev_mod);
    }

    // Declare all symbols in a module.
    // Doesn't update type information.
    pub fn declare_module(&mut self, id: &ModuleId) {
        self.gs.set_module(id.clone());

        let m = self.gs.get_module(id).unwrap();

        // First declare all symbols
        for f in m.files.values() {
            self.declare_symbols(&f.decls, &Visibility::TopLevel)
                .unwrap_or_else(|e| {
                    self.errors.push(e);
                });
        }

        // Make sure all this module's dependencies are processed
        for i in m.all_imports() {
            let import = self.gs.import_for(&i.name);

            match import {
                Some(import) => {
                    // At the moment, all modules consist of just declarations,
                    // so performing inference would only add extra time to the build
                    let mode = DeclareMode::SkipInference;

                    // Process the module
                    self.module_from_folder(import, mode)
                }
                None => {
                    self.generic_error(format!("Module not found {name}", name = i.name), i.span);
                }
            }
        }

        // Then process types and values
        for f in m.files.values() {
            self.reset_scope();
            self.import_std();

            // Put current module in scope
            self.import_module(None, &m.id);

            // `use` statements
            self.process_imports(&f.imports());

            self.declare_types(&f.decls).unwrap_or_else(|e| {
                self.errors.push(e);
            });

            self.declare_values(&f.decls, &Visibility::TopLevel)
                .unwrap_or_else(|e| {
                    self.errors.push(e);
                });
        }
    }

    // Assumes all symbols have been declared
    pub fn infer_module(&mut self, id: &ModuleId) {
        self.gs.set_module(id.clone());

        let m = self.gs.get_module(id).unwrap();

        let mut new_files = HashMap::new();

        for (k, f) in m.files.iter() {
            self.reset_scope();
            self.import_std();

            // Put current module in scope
            self.import_module(None, &m.id);

            // `use` statements
            self.process_imports(&f.imports());

            // TODO asdf this should be configurable
            self.allow_external_decls();

            let mut new_decls = vec![];

            for e in &f.decls {
                let ty = self.fresh_ty_var();
                let new_expr = self.infer_expr(e.clone(), &ty);
                let new_expr = substitute::substitute_expr(new_expr, self);

                // exhaustivity check
                if let Err(e) = exhaustive::check(&new_expr, self) {
                    self.errors.push(e);
                }

                new_decls.push(new_expr);
            }

            let f = File {
                decls: new_decls,
                ..f.clone()
            };

            new_files.insert(k.clone(), f);
        }

        // Override files in modules, all declarations are now typed
        self.gs.module().files = new_files;
    }

    fn import_std(&mut self) {
        self.import_module(None, &ModuleId("std".to_string()))
    }

    pub fn init_std(&mut self) {
        let import = self.gs.import_for("std").unwrap();
        self.module_from_folder(import, DeclareMode::Full);
    }

    fn process_imports(&mut self, imports: &[PkgImport]) {
        for i in imports {
            let import = self.gs.import_for(&i.name);

            // If the module doesn't exist, an error would have been generated already.
            if let Some(import) = import {
                self.import_module(Some(import.prefix()), &import.to_id())
            }
        }
    }

    fn create_pkg_struct(&mut self, name: &str, module: &Module) {
        // create a struct that holds one field for each fn
        // define a global value for the package (ie. fmt)

        let struct_name = format!("__Package{name}");
        let span = Span::dummy();

        let struct_sym = Symbol {
            module: module.id.clone(),
            name: name.to_string(),
            span: span.clone(),
        };

        let ty = Type::Con {
            id: struct_sym,
            args: vec![],
        };

        let mut fields = vec![];

        for (sym, ty) in &module.values {
            fields.push(StructFieldDef {
                name: sym.name.clone(),
                ann: TypeAst::Unknown,
                ty: ty.clone(),
            });
        }

        let def = StructDefinition {
            name: struct_name,
            generics: vec![],
            fields,
        };

        self.add_value(name, ty.to_bounded(), &span);
        self.add_package(name, def, ty.to_bounded());
    }

    fn add_package(&mut self, name: &str, def: StructDefinition, ty: BoundedType) {
        self.current_scope()
            .packages
            .insert(name.to_string(), (def, ty));
    }

    fn get_package(&mut self, name: &str) -> Option<(StructDefinition, BoundedType)> {
        self.current_scope().packages.get(name).cloned()
    }

    fn allow_external_decls(&mut self) {
        self.add_value("EXT", self.type_any().to_bounded(), &Span::dummy());
    }

    fn add_constructor(
        &mut self,
        enum_name: &str,
        cons_name: &str,
        cons_ty: &BoundedType,
        span: &Span,
    ) {
        // let qualified = format!("{}::{}", enum_name, cons_name);
        let qualified = format!("{}.{}", enum_name, cons_name);
        self.add_value(&qualified, cons_ty.clone(), span);
        self.add_value(&cons_name, cons_ty.clone(), span);

        self.current_scope()
            .constructors
            .insert(cons_name.to_string(), qualified);
    }

    fn ensure_not_slice_literal(&mut self, pat: &Pat) {
        if let Pat::Lit { lit, .. } = pat {
            if let Literal::Slice(_) = lit {
                self.generic_error(
                    "Can't pattern match on slice literals".to_string(),
                    pat.get_span(),
                )
            }
        }
    }

    fn can_resolve_name(&mut self, first: Qualified) -> bool {
        // The first chunk of a qualified name can be:
        // - a package name
        // - a local enum name

        if let Some(_pkg) = self.current_scope().packages.get(&first.ident) {
            return true;
        }

        if let Some(_sym) = self.current_scope().types.get(&first.ident) {
            return true;
        }

        false
    }

    fn infer_field_access_as_constructor(
        &mut self,
        expr: Expr,
        field: &str,
        span: &Span,
    ) -> Option<Expr> {
        let mut qualified = vec![];
        to_qualified(
            &Expr::FieldAccess {
                expr: expr.clone().into(),
                field: field.to_string(),
                ty: Type::dummy(),
                span: Span::dummy(),
            },
            &mut qualified,
        );

        // if this expression can be resolved as a bunch of strings,
        // then transform the nested FieldAccess( FieldAccess (..) )
        // tree into a single Expr::Var, then try to resolve that.
        if qualified.is_empty() {
            return None;
        }

        let first = qualified.first().cloned().unwrap();

        // if it's a package, then skip because a struct with all fields is already in scope
        if self.get_package(&first.ident).is_some() {
            return None;
        }

        // check if it's a known type.
        // Here we only want to resolve actual types ie. Foo.Bar
        // not access to struct fields ie. p.name
        if !self.can_resolve_name(first) {
            return None;
        }

        let new_var = qualified
            .into_iter()
            .map(|q| q.ident)
            .collect::<Vec<_>>()
            .join(".");

        Some(Expr::Var {
            value: new_var,
            decl: span.clone(),
            generics_instantiated: Default::default(),
            ty: Type::dummy(),
            span: span.clone(),
        })
    }

    // In a select {}, we're looking for:
    //   - x.Recv()
    //   - x.Send()
    // anything else is invalid
    fn ensure_channel_call(&mut self, method: &str, expr: Expr) -> Expr {
        let new_expr = self.infer_expr(expr, &Type::discard());

        match &new_expr {
            Expr::Call { func, .. } => match **func {
                Expr::FieldAccess {
                    ref field,
                    ref span,
                    expr: ref method_receiver,
                    ..
                } => {
                    if field != method {
                        self.generic_error(
                            format!("was expecting {method}, got {field}"),
                            span.clone(),
                        );
                    }

                    let expected = if method == "Recv" {
                        self.builtin_type("Receiver")
                    } else {
                        self.builtin_type("Sender")
                    }
                    .swap_arg(0, self.type_any());

                    self.add_constraint(&expected, &method_receiver.get_type(), &span);
                }

                _ => {
                    self.generic_error("invalid call in select".to_string(), func.get_span());
                }
            },

            _ => {
                self.generic_error(
                    "invalid expression in select".to_string(),
                    new_expr.get_span(),
                );
            }
        }

        new_expr
    }
}

#[derive(Debug, Clone)]
struct Scope {
    // bindings in the type universe, ie. bool => Symbol(builtin, bool)
    types: HashMap<String, Symbol>,
    // bindings in the value universe, ie. x => int
    values: HashMap<String, BoundedType>,
    // aliases, ie. Ok => Result::Ok
    constructors: HashMap<String, String>,
    // track where bindings were defined
    spans: HashMap<String, Span>,
    // when importing a package, a corresponding struct is created
    packages: HashMap<String, (StructDefinition, BoundedType)>,

    // assumptions about a type found in trait bounds, ie. T => comparable. Used for constraint resolution.
    assumptions: HashMap<Symbol, Vec<Symbol>>,
    // bindings that were declared with `let mut`
    mutability: HashMap<String, bool>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            types: Default::default(),
            values: Default::default(),
            constructors: Default::default(),
            spans: Default::default(),
            packages: Default::default(),
            assumptions: Default::default(),
            mutability: Default::default(),
        }
    }
}

enum CheckInterfaceResult {
    Ok,
    NotAnInterface,
    Error(String),
}

pub enum DeclareMode {
    Full,
    SkipInference,
}

// A chunk of qualified name ie. "foo" in foo.bar
#[derive(Debug, Clone)]
struct Qualified {
    ident: String,
    #[allow(dead_code)]
    span: Span,
}

impl Qualified {
    fn new(ident: &str, span: &Span) -> Qualified {
        Qualified {
            ident: ident.to_string(),
            span: span.clone(),
        }
    }
}

// return true if the expr can be turned into a qualified path ie. foo.bar.baz
fn to_qualified(e: &Expr, qualified: &mut Vec<Qualified>) -> bool {
    match e {
        Expr::FieldAccess {
            expr, field, span, ..
        } => {
            if !to_qualified(expr, qualified) {
                return false;
            }

            qualified.push(Qualified::new(field, span));
            true
        }

        Expr::Var { value, span, .. } => {
            qualified.push(Qualified::new(value, span));
            true
        }

        _ => false,
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

const RESERVED_WORDS: &[&str] = &["default", "func"];
