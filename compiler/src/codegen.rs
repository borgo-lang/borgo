use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    ast::{
        Arm, Binding, Constructor, DebugKind, EnumDefinition, EnumFieldDef, Expr, File, FileId,
        Function, FunctionKind, Generic, InterfaceSuperTrait, Literal, Loop, LoopFlow, Operator,
        Pat, SelectArm, SelectArmPat, Span, StrType, StructDefinition, StructField, UnOp,
    },
    global_state::Module,
    infer,
    type_::{ModuleId, Symbol, Type},
};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct EmittedFile {
    pub name: String,
    pub source: String,
    pub imports: HashMap<String, String>, // "full/path" => name
}

impl EmittedFile {
    pub fn render_source(&self) -> String {
        let mut out = emitter();

        out.emit("package main".to_string());

        out.emit("import (".to_string());

        for (path, name) in &self.imports {
            out.emit(format!("{name} \"{path}\""));
        }

        out.emit(")".to_string());

        out.emit(self.source.to_string());

        out.render()
    }
}

struct Emitter {
    output: Vec<String>,
}

impl Emitter {
    pub fn render(&self) -> String {
        self.output.join("\n")
    }

    pub fn emit(&mut self, line: String) {
        self.output.push(line)
    }

    pub fn emit_value(&self, value: String) -> EmitResult {
        EmitResult {
            output: self.render(),
            value: Some(value),
        }
    }

    pub fn no_value(&self) -> EmitResult {
        EmitResult {
            output: self.render(),
            value: None,
        }
    }

    fn as_value(&self) -> EmitResult {
        EmitResult {
            output: "".to_string(),
            value: Some(self.render()),
        }
    }

    fn try_emit(&self, result: Option<String>) -> EmitResult {
        match result {
            Some(result) => self.emit_value(result),
            None => self.no_value(),
        }
    }
}

#[derive(Debug)]
struct EmitResult {
    output: String,
    value: Option<String>,
}

impl EmitResult {
    fn empty() -> EmitResult {
        EmitResult {
            output: "".to_string(),
            value: None,
        }
    }

    fn unit() -> EmitResult {
        EmitResult {
            output: "".to_string(),
            value: Some(empty_struct()),
        }
    }

    fn to_statement(&self) -> String {
        let ret = self.output.clone();
        format!(
            "{ret} {value}",
            value = self.value.to_owned().unwrap_or_default()
        )
    }
}

fn empty_struct() -> String {
    "struct{}{}".to_string()
}

#[derive(Debug, Clone)]
enum Ctx {
    Discard,     // dont' care about the result
    Var(String), // emit to this specific var
    Arg,         // emit to a new var and tell me which it is
}

impl Ctx {
    fn to_mode(self) -> EmitMode {
        EmitMode {
            ctx: self,
            should_return: false,
        }
    }

    fn to_var(&self) -> Option<String> {
        match self {
            Ctx::Discard => None,
            Ctx::Var(s) => Some(s.to_string()),
            Ctx::Arg => None,
        }
    }

    fn is_discard(&self) -> bool {
        match self {
            Ctx::Discard => true,
            Ctx::Var(_) => false,
            Ctx::Arg => false,
        }
    }
}

#[derive(Clone)]
struct EmitMode {
    ctx: Ctx,
    should_return: bool,
}

impl EmitMode {
    fn top_level() -> EmitMode {
        EmitMode {
            ctx: Ctx::Discard,
            should_return: false,
        }
    }
}

#[derive(Debug, PartialEq)]
enum CallWrapMode {
    // function returns (ok, err) tuple, so needs to be wrapped in a Result
    Wrapped,
    // just call the function, return type doesn't need modifications
    Unwrapped,
}

// A stack of IDENT => VAR bindings
struct Scope {
    scopes: VecDeque<HashMap<String, String>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            scopes: Default::default(),
        }
    }

    fn reset(&mut self) {
        self.scopes = Default::default();
        self.begin();
    }

    fn begin(&mut self) {
        self.scopes.push_front(Default::default());
    }

    fn add_binding(&mut self, k: String, v: String) {
        self.scopes.front_mut().unwrap().insert(k, v);
    }

    fn exit(&mut self) {
        self.scopes.pop_front();
    }

    fn get_binding(&self, value: &str) -> Option<String> {
        self.scopes
            .iter()
            .find_map(|scope| scope.get(value))
            .cloned()
    }

    fn is_defined_in_current_scope(&self, value: &str) -> bool {
        self.scopes.front().unwrap().get(value).is_some()
    }
}

pub struct Codegen {
    pub next_var: usize,

    // functions used to build enum constructors, ie. make_Option_Some()
    pub make_functions: HashMap<String, String>,

    // resolve type aliases
    pub instance: infer::Infer,

    // let bindings are re-bound to temporary vars
    // ie. let x = 1
    // becomes `var varN any; varN = 1`
    // this allows inner scopes to reference existing bindings
    // IDENT => VAR
    scope: Scope,

    // The return type of the current function being generated
    current_fn_ret_ty: Option<Type>,

    // The module being emitted (this is only here to skip emitting traits in std)
    current_module: ModuleId,

    // Ensure these packages are imported
    ensure_imported: HashSet<ModuleId>,
}

impl Codegen {
    pub fn new(instance: infer::Infer) -> Self {
        Self {
            next_var: 0,
            make_functions: Default::default(),
            instance,
            scope: Scope::new(),
            current_fn_ret_ty: None,
            current_module: ModuleId::empty(),
            ensure_imported: Default::default(),
        }
    }

    pub fn compile_module(&mut self, module: &Module) -> Vec<EmittedFile> {
        self.current_module = module.id.clone();

        // collect make functions first so that all enums are registered
        let make_functions = self.collect_make_functions(&module.files);

        module
            .files
            .values()
            .map(|file| {
                let mut source = emitter();

                // A bit dumb, but std can't have dependencies for now at least
                let imports = if module.id == ModuleId::from_str("std") {
                    self.get_std_imports()
                } else {
                    self.collect_imports(file)
                };

                // emit make functions
                for f in make_functions.get(&file.id).cloned().unwrap_or_default() {
                    source.emit(f);
                }

                file.decls.iter().for_each(|expr| {
                    self.next_var = 0;
                    self.scope.reset();

                    let value = self.emit_expr(EmitMode::top_level(), expr);
                    source.emit(value.to_statement())
                });

                // Extend imports to whatever extra packages were collected during codegen.
                // This is useful if there's an indirect dependency on a package.
                // For example:
                //  - package os: fn Open() -> Option<fs::File>
                //  when calling os::Open() we need to instantiate the generic for Option,
                //  so package "fs" also needs to be in scope.
                let imports = self.extend_imports(&imports, &self.ensure_imported.clone());
                self.ensure_imported = Default::default();

                EmittedFile {
                    name: file.go_filename(),
                    imports,
                    source: source.render(),
                }
            })
            .collect()
    }

    fn fresh_var(&mut self) -> String {
        self.next_var += 1;
        format!("var{next}", next = self.next_var)
    }

    // Most expressions will ignore mode and ctx
    // Only if/match/block have to care about it
    fn emit_expr(&mut self, mode: EmitMode, expr: &Expr) -> EmitResult {
        match expr {
            Expr::Closure { fun, kind, .. } => self.emit_closure(fun, kind, None),
            Expr::Block { stmts, .. } => self.emit_block(mode, stmts, false),
            Expr::Call { func, args, ty, .. } => {
                let call_mode = if mode.ctx.is_discard() && !mode.should_return {
                    // if the result should be discarded, there's no point in wrapping the value.
                    // This is the case when the last expression in a block returns a result (ie.
                    // fmt.Fprintf) but the current function returns unit so the value can be
                    // safely dropped and the call doesn't need to be wrapped.
                    CallWrapMode::Unwrapped
                } else {
                    self.call_wrap_mode(ty)
                };

                self.emit_call(func, args, call_mode)
            }
            Expr::Literal { lit, ty, .. } => self.emit_literal(lit, ty),
            Expr::Debug { kind, expr, ty, .. } => self.emit_debug(kind, expr, ty),
            Expr::Return { expr, .. } => self.emit_return(expr),
            Expr::Var {
                value,
                ty,
                generics_instantiated,
                ..
            } => self.emit_var(value, ty, generics_instantiated),
            Expr::Match {
                subject, arms, ty, ..
            } => self.emit_match(&mode.ctx, subject, arms, ty),
            Expr::Let { binding, value, .. } => self.emit_let(binding, value),
            Expr::Binary {
                op, left, right, ..
            } => self.emit_binary(op, left, right),
            Expr::If {
                cond, then, els, ..
            } => self.emit_if(&mode.ctx, cond, then, els),
            Expr::Unit { .. } => EmitResult::unit(),
            Expr::EnumDef { def, .. } => self.emit_enum(def),
            Expr::StructDef { def, .. } => self.emit_struct(def),
            Expr::ImplBlock {
                self_name,
                ty,
                items,
                generics,
                ..
            } => self.emit_impl(self_name, ty, generics, items),
            Expr::StructCall {
                fields, rest, ty, ..
            } => self.emit_struct_call(fields, rest, ty),
            Expr::FieldAccess { expr, field, .. } => self.emit_struct_access(expr, field),
            Expr::Try { expr, .. } => self.emit_try(&mode.ctx, expr),
            Expr::Unary { op, expr, .. } => self.emit_unary(op, expr),
            Expr::Noop { .. } => EmitResult::unit(),
            Expr::Loop { kind, body, .. } => match kind {
                Loop::NoCondition => self.emit_loop(body),
                Loop::WithCondition { binding, expr } => {
                    self.emit_loop_with_condition(binding, expr, body)
                }
                Loop::While { expr } => self.emit_while_loop(expr, body),
            },

            Expr::Flow { kind, .. } => self.emit_loop_flow(kind),
            Expr::VarUpdate { value, target, .. } => self.emit_var_update(value, target),
            Expr::Const { ident, expr, .. } => self.emit_const(ident, expr),
            Expr::CheckType { .. } => EmitResult::empty(),
            Expr::Paren { expr, .. } => self.emit_paren(expr),
            Expr::Spawn { expr, .. } => self.emit_spawn(expr),
            Expr::Select { arms, .. } => self.emit_select(arms),
            Expr::Defer { expr, .. } => self.emit_defer(expr),
            Expr::Reference { expr, .. } => self.emit_reference(expr),
            Expr::Index { expr, index, .. } => self.emit_index(expr, index),
            Expr::Raw { text } => self.emit_raw(text),
            Expr::TypeAlias { .. } => EmitResult::empty(),
            Expr::NewtypeDef { .. } => EmitResult::empty(), // TODO asdf emit "type Foo int"
            Expr::UsePackage { .. } => EmitResult::empty(),
            Expr::Trait {
                name,
                items,
                supertraits,
                ..
            } => self.emit_interface(name, items, supertraits),

            Expr::Tuple { .. } => unreachable!(),
            Expr::MethodCall { .. } => unreachable!(),
            Expr::Todo { .. } => todo!(),
        }
    }

    fn emit_local(&mut self, expr: &Expr, out: &mut Emitter) -> String {
        let res = self.emit_expr(Ctx::Arg.to_mode(), expr);
        let value = res.value.unwrap_or_default();
        out.emit(res.output);
        value
    }

    fn emit_pattern(&mut self, subject: &str, is_matching: &str, pat: &Pat) -> String {
        let mut out = emitter();

        // In let bindings, we still use emit_pattern to destructure the argument.
        // But in that case, there's no parent matching context that we care about, as is the case
        // in match expressions. So there's no point emitting all the if statements.
        let cares_about_matching = is_matching != "_";

        let new_is_matching = if cares_about_matching {
            self.fresh_var() + "_match_pat"
        } else {
            is_matching.to_string()
        };

        match pat {
            Pat::Lit { lit, ty, span } => {
                let lit = Expr::Literal {
                    lit: lit.clone(),
                    ty: ty.clone(),
                    span: span.clone(),
                };

                let value = self.emit_local(&lit, &mut out);

                out.emit(format!(
                    "
if {is_matching} != 1 && {value} == {subject} {{
    {is_matching} = 2
}} else {{
    {is_matching} = 1
}}"
                ));
            }

            Pat::Type { ident, .. } => {
                out.emit(format!("{ident} := {subject}"));
                self.scope.add_binding(ident.clone(), ident.clone());
            }

            Pat::Pat { ident, elems, .. } => {
                // Introduce a new sentinel matching value for the nested pattern match.
                //
                // For example, if we're trying to match (1, "foo")
                // new_is_matching will be 2 if both fields match
                // And then we can forward the success value to the parent is_matching
                if cares_about_matching {
                    out.emit(format!("{new_is_matching} := 0"));
                }

                let (_, con_name) = ident.split_once(".").unwrap();

                elems.iter().enumerate().for_each(|(index, p)| {
                    let field = constructor_field_name(con_name, elems.len(), index);
                    let new_subject = format!("{subject}.{field}");
                    let pat = self.emit_pattern(&new_subject, &new_is_matching, p);
                    out.emit(pat);
                });

                let name = to_name(ident);

                if cares_about_matching {
                    // Check against new_is_matching but override parent is_matching for final check
                    out.emit(format!(
                        "
if {new_is_matching} != 1 && {subject}.tag == {name} {{
    {is_matching} = 2
}} else {{
    {is_matching} = 1
}}
"
                    ));
                }
            }

            Pat::Struct { fields, .. } => {
                if cares_about_matching {
                    out.emit(format!("{new_is_matching} := 0"));
                }

                fields.iter().for_each(|f| {
                    let pat = self.emit_pattern(
                        &format!("{subject}.{field_name}", field_name = f.name),
                        &new_is_matching,
                        &f.value,
                    );
                    out.emit(pat);
                });

                if cares_about_matching {
                    out.emit(format!(
                        "
if {new_is_matching} != 1 {{
    {is_matching} = 2
}} else {{
    {is_matching} = 1
}}
"
                    ));
                }
            }

            Pat::Wild { .. } => {
                if cares_about_matching {
                    out.emit(format!(
                        "if {is_matching} != 1 {{ {is_matching} = 2 /* wildcard */ }}"
                    ))
                }
            }

            Pat::Unit { .. } => {
                if cares_about_matching {
                    out.emit(format!(
                        "if {is_matching} != 1 {{ {is_matching} = 2 /* Unit */ }}"
                    ))
                }
            }
        };

        out.render()
    }

    // TODO asdf add Method variant to FunctionKind and remove receiver
    fn emit_closure(
        &mut self,
        fun: &Function,
        kind: &FunctionKind,
        receiver: Option<(String, Type)>,
    ) -> EmitResult {
        let mut out = emitter();
        let mut args_destructure = emitter();

        if fun.is_external() {
            return out.no_value();
        }

        let prev_ret_ty = self.current_fn_ret_ty.clone(); // nested functions, save context
        self.current_fn_ret_ty = Some(fun.ret.clone());

        let (fun, receiver) = change_collection_methods(fun, receiver.clone());

        let args = fun
            .args
            .iter()
            .enumerate()
            .map(|(index, a)| {
                let name = match &a.pat {
                    Pat::Type { ident, .. } => {
                        // a binding is still necessary for this
                        self.scope.add_binding(ident.clone(), ident.clone());
                        ident.clone()
                    }
                    Pat::Wild { .. } => "_".to_string(),
                    _ => {
                        let var = format!("arg_{index}");
                        let pat = self.emit_pattern(&var, "_", &a.pat);
                        args_destructure.emit(pat);
                        var
                    }
                };

                name + " " + &self.to_type(&a.ty)
            })
            .collect::<Vec<_>>()
            .join(", ");

        let stmts = wrap_block_and_get_statements(&fun.body);

        let body = self
            .emit_block(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: !fun.ret.is_unit(),
                },
                &stmts,
                true,
            )
            .to_statement();

        // There are 3 cases here:
        // - Top level functions are always emitted as `func $name`
        // - Lambdas are bound to an anonymous function `$name := func`
        // - Inline functions may be recursive, so need a declaration first
        //     `var $name func(..); $name =`

        let name = to_name(&fun.name);
        let ret = if fun.ret.is_unit() {
            "".to_string()
        } else {
            self.return_to_type(&fun.ret)
        };

        let generics = if receiver.is_none() {
            generics_to_string(&fun.generics)
        } else {
            "".to_string()
        };

        let receiver_fmt = if let Some((self_name, ty)) = receiver {
            format!("({self_name} {ty})", ty = self.to_type(&ty))
        } else {
            "".to_string()
        };

        let binding = match kind {
            FunctionKind::TopLevel => {
                format!("func {receiver_fmt} {name} {generics} ({args}) {ret}")
            }

            FunctionKind::Lambda => format!("func ({args}) {ret}"),

            FunctionKind::Inline => {
                let typed_args = fun
                    .args
                    .iter()
                    .map(|t| self.to_type(&t.ty))
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut decl = emitter();

                // needs to be separate declaration to allow recursive functions.
                decl.emit(format!("var {name} func ({typed_args}) {ret}"));
                decl.emit(format!("{name} = func ({args}) {ret}"));

                decl.render()
            }
        };

        out.emit(format!(
            "{binding} {{
{args_destructure}
{body}
}}",
            args_destructure = args_destructure.render()
        ));

        self.current_fn_ret_ty = prev_ret_ty;

        out.as_value()
    }

    fn emit_block(&mut self, mode: EmitMode, stmts: &[Expr], skip_braces: bool) -> EmitResult {
        let mut out = emitter();
        let mut stmts = stmts.to_vec();

        if stmts.is_empty() {
            if !skip_braces {
                out.emit("{}".to_string());
            }
            return out.no_value();
        }

        let unit = Expr::Unit {
            ty: self.instance.type_unit(),
            span: Span::dummy(),
        };

        let mut last = stmts.pop().unwrap_or(unit.clone());

        // The logic here is a bit involved.
        // We're basically trying to emit the cleanest output possible, while allowing for values
        // to be discarded

        if !is_standalone(&last) && !mode.should_return && !mode.ctx.is_discard() {
            stmts.push(last);
            last = unit;
        }

        if mode.should_return {
            last = ensure_wrapped_in_return(&last);
        }

        let needs_braces = !stmts.is_empty() || mode.should_return;

        if !skip_braces && needs_braces {
            out.emit("{".to_string());
        }

        self.scope.begin();

        stmts.iter().for_each(|e| {
            let result = self.emit_expr(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                e,
            );

            out.emit(result.to_statement());
        });

        let result = match mode.ctx {
            Ctx::Discard => {
                let value = self.emit_expr(mode, &last);
                out.emit(value.to_statement());
                None
            }
            Ctx::Var(_) => {
                let result = self.emit_expr(mode.clone(), &last);
                out.emit(result.output);
                let assign = assign_to_result(&mode.ctx, &result.value.unwrap_or_default());
                out.emit(assign);
                None
            }
            Ctx::Arg => {
                let value = self.emit_local(&last, &mut out);
                Some(value)
            }
        };

        self.scope.exit();

        if !skip_braces && needs_braces {
            out.emit("}".to_string());
        }

        out.try_emit(result)
    }

    fn emit_call(&mut self, func: &Expr, args: &[Expr], mode: CallWrapMode) -> EmitResult {
        if let Some(res) = self.emit_collection_method(func, args, &mode) {
            return res;
        }

        let mut out = emitter();

        let new_args = args
            .iter()
            .map(|a| self.emit_local(a, &mut out))
            .collect::<Vec<_>>()
            .join(", ");

        let new_func = self.emit_local(func, &mut out);

        let instantiated = match func {
            Expr::Var {
                generics_instantiated,
                ..
            } => self.render_generics_instantiated(generics_instantiated),

            _ => "".to_string(),
        };

        let call = format!("{new_func}{instantiated}({new_args})");

        return self.emit_wrapped_call(call, func, &mode, &mut out);
    }

    fn emit_wrapped_call(
        &mut self,
        call: String,
        func: &Expr,
        mode: &CallWrapMode,
        out: &mut Emitter,
    ) -> EmitResult {
        // if no wrapping needs to happen, then just return the call
        if mode == &CallWrapMode::Unwrapped {
            return out.emit_value(call);
        }

        // if the function is a call to the constructors Ok/Err or Some/None
        // then we don't need to wrap and can emit the call as is
        if func.as_result_constructor().is_some() || func.as_option_constructor().is_some() {
            return out.emit_value(call);
        }

        // At this point we know that the function either needs to return (ok, err) or (ok, bool)
        // The return type is either Result<T, E> or Option<T>
        let ret = func.get_type().get_function_ret().unwrap();

        let args = ret.get_args().unwrap();

        // Instantiate generics on return type
        let instantiated = self.render_generics_instantiated(&args);

        // Ensure all referenced packages are imported
        self.add_pkg_imports(&args);

        // Handle Result<T, E>
        if ret.is_result() {
            let check = self.fresh_var() + "_check";
            let err = self.fresh_var() + "_err";
            let result = self.fresh_var() + "_result";

            out.emit(format!(
                "{result} := func() Result{instantiated} {{
    {check}, {err} := {call}
    if {err} != nil {{
        return make_Result_Err{instantiated}({err})
    }}
    return make_Result_Ok{instantiated}({check})
}}()
"
            ));

            return out.emit_value(result);
        }

        // Handle Option<T>
        if ret.is_option() {
            let check = self.fresh_var() + "_check";
            let err = self.fresh_var() + "_err";
            let result = self.fresh_var() + "_result";

            out.emit(format!(
                "{result} := func() Option{instantiated} {{
    {check}, {err} := {call}
    if {err} == false {{
        return make_Option_None{instantiated}()
    }}
    return make_Option_Some{instantiated}({check})
}}()
"
            ));

            return out.emit_value(result);
        }

        unreachable!("should be either Result or Option");
    }

    fn emit_literal(&mut self, lit: &Literal, ty: &Type) -> EmitResult {
        let mut out = emitter();

        let value = match lit {
            Literal::Int(n) => n.to_string(),
            Literal::Float(n) => n.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(inner) => match inner {
                StrType::Single(s) => format!("\"{}\"", s).replace('\n', "\\n"),
                StrType::Multi(lines) => {
                    let body = lines.join("\n");
                    format!("`{}`", body)
                }
            },
            Literal::Char(s) => {
                format!("\'{}\'", s)
            }
            Literal::Slice(elems) => {
                let new_elems = elems
                    .iter()
                    .map(|e| self.emit_local(e, &mut out))
                    .collect::<Vec<_>>()
                    .join(", ");

                let instantiated = self.to_type(ty.get_args().unwrap().first().unwrap());
                format!("[]{instantiated} {{ {new_elems} }}")
            }
        };

        out.emit_value(value)
    }

    fn emit_debug(&mut self, kind: &DebugKind, expr: &Expr, ty: &Type) -> EmitResult {
        let mut out = emitter();

        let var = match kind {
            DebugKind::Unreachable => "".to_string(),
            DebugKind::Todo => "".to_string(),
            DebugKind::Inspect => self.emit_local(expr, &mut out),
        };

        let fn_name = format!("{:#?}", kind).to_lowercase();
        let instantiated = self.to_type(&ty);
        out.emit_value(format!("Debug_{fn_name}[{instantiated}]({var})"))
    }

    fn emit_return(&mut self, expr: &Expr) -> EmitResult {
        if let Some(ret) = self.return_value_is_result(expr) {
            return ret;
        }

        if let Some(ret) = self.return_value_is_option(expr) {
            return ret;
        }

        let mut out = emitter();

        let value = if self.current_fn_ret_ty == Some(self.instance.type_unit()) {
            "".to_string()
        } else {
            self.emit_local(expr, &mut out)
        };

        out.emit(format!("return {value}"));
        out.no_value()
    }

    fn emit_match(&mut self, ctx: &Ctx, subject: &Expr, arms: &[Arm], ty: &Type) -> EmitResult {
        let mut out = emitter();

        let subject = self.emit_local(subject, &mut out);

        let subject_var = self.fresh_var() + "_subject";
        let is_matching = self.fresh_var() + "_matches";

        let new_ctx = self.generate_or_reuse(ctx, ty, &mut out);

        // Matching status:
        //   0 -> pattern matching hasn't started
        //   1 -> pattern matching failed
        //   2 -> pattern matching succeeded

        out.emit(format!(
            " {subject_var} := {subject}
{is_matching} := 0

",
        ));

        arms.iter().for_each(|arm| {
            let if_statements = self.emit_pattern(&subject_var, &is_matching, &arm.pat);

            let result = self.emit_expr(new_ctx.clone().to_mode(), &arm.expr);
            let assign = assign_to_result(&new_ctx, &result.value.unwrap_or_default());
            let preamble = result.output;

            out.emit(format!(
                "
if {is_matching} != 2 {{
    {is_matching} = 0

    {if_statements}
    _ = {subject_var}

    if {is_matching} == 2 {{
        {preamble}
        {assign}
    }}
}}
"
            ))
        });

        out.try_emit(new_ctx.to_var())
    }

    fn emit_var(&mut self, value: &str, ty: &Type, generics_instantiated: &[Type]) -> EmitResult {
        let name = self.scope.get_binding(value).unwrap_or_else(|| {
            value.to_string()
            // panic!("failed to resolve {}", value);
        });
        // .unwrap_or_else(|| self.gs.resolve_name(value));

        let make_fn = self.make_functions.get(&name);

        // Check if it's a constructor with no arguments (ie. Option::None)
        // In that case, it's not sufficient to resolve it as a bare function.
        // We need to actually call the function and instantiate the generics.
        if matches!(ty, Type::Con { .. }) && make_fn.is_some() {
            let generics = self.render_generics_instantiated(generics_instantiated);
            let call = format!("{fun}{generics}()", fun = to_name(make_fn.unwrap()));
            return EmitResult {
                output: "".to_string(),
                value: Some(call),
            };
        }

        let name = to_name(make_fn.unwrap_or(&name));

        EmitResult {
            output: "".to_string(),
            value: Some(name),
        }
    }

    fn emit_let(&mut self, binding: &Binding, value: &Expr) -> EmitResult {
        let mut out = emitter();

        let mut needs_declaration = true;
        let mut pat_output = None;

        let ctx = match binding.pat {
            Pat::Type { ref ident, .. } => {
                if self.scope.is_defined_in_current_scope(ident) {
                    needs_declaration = false;
                }

                self.scope.add_binding(ident.clone(), ident.clone());

                Ctx::Var(ident.clone())
            }

            Pat::Wild { .. } => Ctx::Discard,

            _ => {
                let var = self.fresh_var();
                let pat = self.emit_pattern(&var, "_", &binding.pat);
                pat_output = Some(pat);
                Ctx::Var(var)
            }
        };

        // If it's a try expression, then a declaration is never necessary because it will define
        // an ok, err := ... binding when calling the function
        if matches!(value, Expr::Try { .. }) {
            needs_declaration = false;
        }

        let new_expr = self.emit_expr(ctx.clone().to_mode(), value);
        let new_value = new_expr.value.unwrap_or_default();

        // If it's not a standalone expression (if, match) then declare the variable first, then
        // emit the output
        if !is_standalone(value) {
            match ctx {
                Ctx::Discard => {
                    out.emit(new_expr.output);
                    return out.no_value();
                }

                Ctx::Var(var) => {
                    if needs_declaration {
                        let instantiated = self.to_type(&value.get_type());
                        out.emit(format!("var {var} {instantiated}"));
                    };

                    out.emit(new_expr.output);
                    return out.no_value();
                }

                Ctx::Arg => unreachable!(),
            }
        }

        // Otherwise emit the output first and then assign the new value
        out.emit(new_expr.output);

        match ctx {
            Ctx::Discard => out.emit(format!("_ = {new_value}")),
            Ctx::Var(var) => {
                let assign = if needs_declaration { ":=" } else { "=" };
                out.emit(format!("{var} {assign} {new_value}"));
                out.emit(pat_output.unwrap_or_default());
            }
            Ctx::Arg => unreachable!(), // pretty sure this is right
        }

        out.no_value()
    }

    fn emit_binary(&mut self, op: &Operator, left: &Expr, right: &Expr) -> EmitResult {
        let mut out = emitter();
        let left = self.emit_local(left, &mut out);
        let right = self.emit_local(right, &mut out);
        let op = match op {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Lt => "<",
            Operator::Le => "<=",
            Operator::Gt => ">",
            Operator::Ge => ">=",
            Operator::Rem => "%",
            Operator::Eq => "==",
            Operator::Ne => "!=",
            Operator::And => "&&",
            Operator::Or => "||",
        };

        return out.emit_value(format!("{left} {op} {right}"));
    }

    fn emit_unary(&mut self, op: &UnOp, expr: &Expr) -> EmitResult {
        let mut out = emitter();
        let expr = self.emit_local(expr, &mut out);

        let op = match op {
            UnOp::Neg => "-".to_string(),
            UnOp::Not => "!".to_string(),
            UnOp::Deref => "*".to_string(),
        };

        return out.emit_value(format!("{op}{expr}"));
    }

    fn emit_if(&mut self, ctx: &Ctx, cond: &Expr, then: &Expr, els: &Expr) -> EmitResult {
        let mut out = emitter();

        let new_cond = self.emit_local(cond, &mut out);
        let new_ctx = self.generate_or_reuse(ctx, &then.get_type(), &mut out);

        let mut wrap_and_assign = |expr: &Expr| {
            let result = self.emit_expr(new_ctx.clone().to_mode(), &expr);
            let assign = assign_to_result(&new_ctx, &result.value.unwrap_or_default());
            let preamble = result.output;

            format!(
                "{{
    {preamble}
    {assign}
}}"
            )
        };

        let new_then = wrap_and_assign(then);

        // Skip emitting the else branch if it's empty
        let new_else = match els {
            Expr::Block { stmts, .. } if stmts.is_empty() => "".to_string(),
            Expr::Noop => "".to_string(),
            _ => format!("else {}", wrap_and_assign(els)),
        };

        out.emit(format!("if {new_cond} {new_then} {new_else}",));

        out.try_emit(new_ctx.to_var())
    }

    fn emit_enum(&self, def: &EnumDefinition) -> EmitResult {
        let mut out = emitter();

        let name = &def.name;

        // Emit tag
        let tag = format!("{name}Tag");
        out.emit(format!("type {tag} int"));

        // Emit const declaration for tag
        out.emit("const (".to_string());

        let make_cons = |cons: &str| -> String { format!("{name}_{cons}") };

        let first = def.cons.first().unwrap();
        out.emit(format!(
            "{cons} {tag} = iota",
            cons = make_cons(&first.name)
        ));

        for c in &def.cons[1..] {
            out.emit(format!("{cons}", cons = make_cons(&c.name)));
        }

        out.emit(")".to_string());

        // Emit type
        let generics = generics_to_string(&def.generics);

        out.emit(format!(
            "type {name}{generics} struct {{
            tag {tag}"
        ));

        def.cons.iter().for_each(|con| {
            if con.fields.is_empty() {
                return;
            }

            let emit_constructor = |field: &EnumFieldDef, index: Option<usize>| -> String {
                format!(
                    "{field_name}{index} {typ}",
                    field_name = con.name,
                    index = index.map(|i| format!("{i}")).unwrap_or("".to_string()),
                    typ = self.to_type(&field.ty)
                )
            };

            if con.fields.len() == 1 {
                let field = con.fields.first().unwrap();
                out.emit(emit_constructor(field, None));
                return;
            }

            for (index, field) in con.fields.iter().enumerate() {
                out.emit(emit_constructor(field, Some(index)));
            }
        });

        out.emit("}".to_string());

        out.no_value()
    }

    fn emit_struct(&self, def: &StructDefinition) -> EmitResult {
        let mut out = emitter();

        let name = def.name.clone();

        let generics = generics_to_string(&def.generics);
        out.emit(format!("type {name}{generics} struct {{"));

        def.fields.iter().for_each(|f| {
            let field = &f.name;
            let ty = self.to_type(&f.ty.ty);
            out.emit(format!("  {field} {ty}"));
        });

        out.emit("}".to_string());
        out.no_value()
    }

    fn emit_impl(
        &mut self,
        self_name: &str,
        ty: &Type,
        generics: &[Generic],
        items: &[Expr],
    ) -> EmitResult {
        let mut out = emitter();

        items.iter().for_each(|e| {
            self.next_var = 0;
            self.scope.reset();

            let mut fun = e.as_function();
            fun.generics = generics.to_vec();

            // TODO asdf FunctionKind should have extra variant Method
            // remove is_method param
            let value = self.emit_closure(
                &fun,
                &FunctionKind::TopLevel,
                Some((self_name.to_string(), ty.clone())),
            );
            out.emit(value.to_statement())
        });

        out.no_value()
    }

    fn emit_struct_call(
        &mut self,
        fields: &[StructField],
        rest: &Option<Expr>,
        ty: &Type,
    ) -> EmitResult {
        let mut out = emitter();

        let has_rest = rest.is_some();
        let instantiated = self.to_type(ty);

        let new_fields = fields
            .iter()
            .map(|f| {
                let name = &f.name;
                let value = self.emit_local(&f.value, &mut out);

                if has_rest {
                    format!("data.{name} = {value}")
                } else {
                    format!("{name}: {value}, ")
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        if !has_rest {
            return out.emit_value(format!("{instantiated} {{ {new_fields} }}"));
        }

        // Generate a self-invoking function
        let new_rest = self.emit_local(rest.as_ref().unwrap(), &mut out);

        out.emit_value(format!(
            "func (data {instantiated}) {instantiated} {{
    {new_fields}
    return data
}}({new_rest})"
        ))
    }

    fn emit_struct_access(&mut self, expr: &Expr, field: &str) -> EmitResult {
        let mut out = emitter();
        let new_expr = self.emit_local(expr, &mut out);
        out.emit_value(format!("{new_expr}.{field}"))
    }

    fn emit_try(&mut self, ctx: &Ctx, expr: &Expr) -> EmitResult {
        // Check if we're dealing with a wrapped function that returns (ok, err)
        if let Expr::Call { func, args, ty, .. } = expr {
            if self.call_wrap_mode(ty) == CallWrapMode::Wrapped {
                return self.emit_wrapped_try(ctx, func, args);
            }
        }

        // In all other cases we're dealing with a function that will return a Result value
        let mut out = emitter();

        let check = self.fresh_var() + "_check";
        let call = self.emit_local(expr, &mut out);
        // let assign = assign_to_result(&ctx, &format!("{check}.Ok"));

        // The error value we return depends on whether the return type of the current function is
        // wrapped or not. TODO asdf check the jargon here, I think I have wrapped/unwrapped
        // wrong, they mean the opposite of how I'm using them. :/
        let return_error = match self.call_wrap_mode(self.current_fn_ret_ty.as_ref().unwrap()) {
            CallWrapMode::Unwrapped => format!("return {check}"),
            CallWrapMode::Wrapped => {
                format!("return {nil}, {check}.Err", nil = self.current_zero_value())
            }
        };

        out.emit(format!(
            "{check} := {call}
             {var} := {check}.Ok
             if !{check}.IsOk() {{
               {return_error}
             }}",
            var = ctx.to_var().unwrap_or_else(|| "_".to_string())
        ));

        out.try_emit(ctx.to_var())
    }

    fn emit_loop(&mut self, body: &Expr) -> EmitResult {
        let mut out = emitter();

        let stmts = wrap_block_and_get_statements(&body);
        let body = self
            .emit_block(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                &stmts,
                true,
            )
            .to_statement();

        out.emit(format!(
            "for {{
    {body}
}}",
        ));

        out.no_value()
    }

    fn emit_loop_with_condition(
        &mut self,
        binding: &Binding,
        expr: &Expr,
        body: &Expr,
    ) -> EmitResult {
        let mut out = emitter();

        let rangeable = self.emit_local(expr, &mut out);

        let mut extract_binding = |pat: &Pat| match pat {
            Pat::Type { ident, .. } => {
                self.scope.add_binding(ident.clone(), ident.clone());
                ident.clone()
            }

            _ => unreachable!(),
        };

        let mut k = "_".to_string();
        let mut v = "_".to_string();

        match &binding.pat {
            Pat::Wild { .. } => {}
            Pat::Type { .. } => v = extract_binding(&binding.pat),
            Pat::Struct { fields, .. } => {
                // there should be two fields
                k = extract_binding(&fields[0].value);
                v = extract_binding(&fields[1].value);
            }
            _ => {
                panic!("unexpected pat in loop expr {:#?}", binding.pat);
            }
        };

        let stmts = wrap_block_and_get_statements(&body);
        let body = self
            .emit_block(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                &stmts,
                true,
            )
            .to_statement();

        let iteration_vars = if expr.get_type().is_receiver() {
            v
        } else {
            format!("{k}, {v}")
        };

        out.emit(format!(
            "for {iteration_vars} := range {rangeable} {{
    {body}
}}",
        ));

        out.no_value()
    }

    fn emit_while_loop(&mut self, expr: &Expr, body: &Expr) -> EmitResult {
        let mut out = emitter();

        let expr = self.emit_local(expr, &mut out);

        let stmts = wrap_block_and_get_statements(&body);
        let body = self
            .emit_block(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                &stmts,
                true,
            )
            .to_statement();

        out.emit(format!(
            "for ({expr}) {{
    {body}
}}",
        ));

        out.no_value()
    }

    fn emit_loop_flow(&mut self, kind: &LoopFlow) -> EmitResult {
        let mut out = emitter();

        let token = match kind {
            LoopFlow::Break => "break",
            LoopFlow::Continue => "continue",
        };

        out.emit(token.to_string());
        out.no_value()
    }

    fn emit_var_update(&mut self, value: &Expr, target: &Expr) -> EmitResult {
        let mut out = emitter();

        let new_expr = self.emit_local(value, &mut out);

        match target {
            Expr::Var {
                value: var_name, ..
            } => {
                out.emit(format!("{var_name} = {new_expr}"));
            }

            Expr::FieldAccess { expr, field, .. } => {
                let target = self.emit_local(expr, &mut out);
                out.emit(format!("{target}.{field} = {new_expr}"));
            }

            Expr::Index { expr, index, .. } => {
                let target = self.emit_local(expr, &mut out);
                let index = self.emit_local(index, &mut out);
                out.emit(format!("{target}[{index}] = {new_expr}"));
            }

            Expr::Unary { expr, .. } => {
                let target = self.emit_local(expr, &mut out);
                out.emit(format!("*{target} = {new_expr}"));
            }

            e => panic!("unexpected expr in codegen var update {:#?}", e),
        };

        out.no_value()
    }

    fn emit_paren(&mut self, expr: &Expr) -> EmitResult {
        let mut out = emitter();
        let value = self.emit_local(expr, &mut out);
        out.emit_value(format!("({value})"))
    }

    fn emit_spawn(&mut self, expr: &Expr) -> EmitResult {
        let mut out = emitter();

        let call = match expr {
            Expr::Call { func, args, .. } => {
                let new_expr = self.emit_call(func, args, CallWrapMode::Unwrapped);
                new_expr.to_statement()
            }

            _ => unreachable!("expected function call in spawn!()"),
        };

        out.emit(format!("go {call}"));
        out.no_value()
    }

    fn emit_defer(&mut self, expr: &Expr) -> EmitResult {
        let mut out = emitter();

        let call = match expr {
            Expr::Call { func, args, .. } => {
                let new_expr = self.emit_call(func, args, CallWrapMode::Unwrapped);
                new_expr.to_statement()
            }

            _ => unreachable!("expected function call in defer!()"),
        };

        out.emit(format!("defer {call}"));
        out.no_value()
    }

    fn emit_raw(&self, text: &str) -> EmitResult {
        EmitResult {
            output: "".to_string(),
            value: Some(text.to_string()),
        }
    }

    fn emit_select(&mut self, arms: &[SelectArm]) -> EmitResult {
        let mut out = emitter();
        let mut inner = emitter();

        for a in arms {
            match &a.pat {
                SelectArmPat::Recv(binding, expr) => match binding.pat {
                    Pat::Type { ref ident, .. } => {
                        self.scope.add_binding(ident.clone(), ident.clone());

                        let new_expr = self.emit_select_case(&expr, &mut out);
                        inner.emit(format!("case {ident} := {new_expr}:",));
                    }

                    Pat::Wild { .. } => {
                        let new_expr = self.emit_select_case(&expr, &mut out);
                        inner.emit(format!("case {new_expr}:",));
                    }

                    _ => unreachable!(),
                },

                SelectArmPat::Send(expr) => {
                    let new_expr = self.emit_select_case(&expr, &mut out);
                    inner.emit(format!("case {new_expr}:",));
                }

                SelectArmPat::Wildcard => {
                    inner.emit("default:".to_string());
                }
            }

            let body = self.emit_expr(Ctx::Discard.to_mode(), &ensure_wrapped_in_block(&a.expr));
            inner.emit(body.output);
        }

        out.emit("select {".to_string());
        out.emit(inner.render());
        out.emit("}".to_string());

        out.no_value()
    }

    // Unwrap ch.Recv() or ch.Send(x)
    // and emit corresponding <- ch or ch <- x
    fn emit_select_case(&mut self, expr: &Expr, out: &mut Emitter) -> String {
        match expr {
            Expr::Call { func, args, .. } => match **func {
                Expr::FieldAccess {
                    ref field,
                    expr: ref method_receiver,
                    ..
                } => {
                    let new_expr = self.emit_local(&method_receiver, out);

                    if field == "Recv" {
                        format!("<- {new_expr}")
                    } else {
                        let new_arg = self.emit_local(&args[0], out);
                        format!("{new_expr} <- {new_arg}")
                    }
                }

                _ => unreachable!(),
            },

            _ => unreachable!(),
        }
    }

    fn create_make_function(&mut self, def: &EnumDefinition) -> String {
        let mut out = emitter();

        def.cons.iter().for_each(|con| {
            let constructor = def.name.clone() + "." + &con.name;
            let name = to_name(&format!("make_{constructor}"));
            out.emit(self.constructor_make_function(&def, &name, con));
            self.make_functions.insert(constructor, name);
        });

        out.render()
    }

    fn emit_wrapped_try(&mut self, ctx: &Ctx, func: &Expr, args: &[Expr]) -> EmitResult {
        let mut out = emitter();

        // get the T out of Option<T> or Result<T>
        let ret = func.get_type().get_return_inner_arg();
        self.add_pkg_imports(&[ret.clone()]);

        let new_ctx = self.generate_or_reuse(ctx, &ret, &mut out);
        let check = new_ctx.to_var().unwrap_or_else(|| "_".to_string());

        let err = self.fresh_var() + "_err";
        let assign = assign_to_result(&new_ctx, &format!("{check}"));

        // let ret = self.current_fn_ret_ty.as_ref().unwrap();
        // let instantiated = render_generics_instantiated(&ret.get_args().unwrap());

        // Emit function call as if it were unwrapped, so that we can deal with the two returned
        // value appropriately.
        let call = self.emit_call(func, args, CallWrapMode::Unwrapped);
        out.emit(call.output);
        out.emit(format!(
            "{check}, {err} := {call}
    if {err} != nil {{
        return {nil}, {err}
    }}
    {assign}
",
            nil = self.current_zero_value(),
            call = call.value.unwrap()
        ));

        return out.try_emit(new_ctx.to_var());
    }

    fn emit_reference(&mut self, expr: &Expr) -> EmitResult {
        let mut out = emitter();
        let expr = self.emit_local(expr, &mut out);

        return out.emit_value(format!("&{expr}"));
    }

    fn emit_interface(
        &self,
        name: &str,
        items: &[Expr],
        supertraits: &[InterfaceSuperTrait],
    ) -> EmitResult {
        let mut out = emitter();

        // TODO asdf this is a hack to prevent stuff like comparable or any to get emitted
        if self.current_module == ModuleId::from_str("std") {
            return out.no_value();
        }

        out.emit(format!("type {name} interface {{"));

        for s in supertraits {
            out.emit(self.to_type(&s.ty));
        }

        for f in items {
            let name = f.as_function().name;
            let ty = f.get_type();

            let args = ty
                .get_function_args()
                .unwrap()
                .iter()
                .map(|a| self.to_type(a))
                .collect::<Vec<_>>()
                .join(", ");

            let ret = self.to_type(&ty.get_function_ret().unwrap());

            out.emit(format!("{name} ({args}) {ret}"));
        }

        out.emit("}".to_string());
        return out.no_value();
    }

    fn emit_const(&mut self, ident: &str, expr: &Expr) -> EmitResult {
        let mut out = emitter();

        let body = self
            .emit_expr(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: true,
                },
                &ensure_wrapped_in_block(&expr),
            )
            .to_statement();

        let instantiated = self.to_type(&expr.get_type());
        out.emit(format!("var {ident} = func () {instantiated} {body}()"));
        out.no_value()
    }

    // methods on Slice, Maps and Channels are actually function calls.
    // TODO asdf this could actually incline the rawgo!(..) implementation
    // instead of emitting an extra function call.
    fn emit_collection_method(
        &mut self,
        func: &Expr,
        args: &[Expr],
        mode: &CallWrapMode,
    ) -> Option<EmitResult> {
        if let Expr::FieldAccess { expr, field, .. } = func {
            let ty_name = is_collection_type(&expr.get_type())?;

            let mut new_args = vec![*expr.clone()];
            new_args.extend_from_slice(args);

            let mut out = emitter();

            let new_args = new_args
                .iter()
                .map(|a| self.emit_local(a, &mut out))
                .collect::<Vec<_>>()
                .join(", ");

            let instantiated =
                self.render_generics_instantiated(&expr.get_type().get_args().unwrap());

            let fn_name = to_name(&format!("{ty_name}.{field}"));
            let call = format!("{fn_name}{instantiated}({new_args})");

            return Some(self.emit_wrapped_call(call, func, mode, &mut out));
        }

        None
    }

    fn current_zero_value(&self) -> String {
        let ty = &self.current_fn_ret_ty.as_ref().unwrap().get_args().unwrap()[0];

        format!("*new({ty})", ty = self.to_type(&ty))
    }

    fn return_value_is_result(&mut self, expr: &Expr) -> Option<EmitResult> {
        let ret_ty = expr.get_type();
        if !ret_ty.is_result() {
            return None;
        }

        // If the error type is not an interface, then we can't wrap it.
        // See explanation in call_wrap_mode
        if self.call_wrap_mode(&ret_ty) == CallWrapMode::Unwrapped {
            return None;
        }

        let mut out = emitter();

        // Handle cases where return value is a Result

        // 1. Function call to a Result constructor, ie. Ok() or Err()
        // 2. Function call to any other function that produces a Result
        // 3. A value (match expression, variable) that needs to be unwrapped

        if let Expr::Call { func, args, .. } = expr {
            let variant = func.as_result_constructor();

            if variant == Some(Ok(())) {
                // 1a. Function call to Result::Ok
                let arg = self.emit_local(&args[0], &mut out);
                out.emit(format!("return {arg}, nil"));
                //
            } else if variant == Some(Err(())) {
                // 1b. Function call to Result::Err
                let arg = self.emit_local(&args[0], &mut out);
                out.emit(format!(
                    "return {nil}, {arg}",
                    nil = self.current_zero_value(),
                ));
                //
            } else {
                // 2. Function call to any other function that produces a Result
                let call = self.emit_call(func, args, CallWrapMode::Unwrapped);
                out.emit(call.output);
                out.emit(format!("return {value}", value = call.value.unwrap()));
            }

            return Some(out.no_value());
        };

        // 3. A value (match expression, variable) that needs to be unwrapped
        let value = self.emit_local(expr, &mut out);

        out.emit(format!(
            "
        if {value}.IsOk() {{
            return {value}.Ok, nil
        }}
        return {nil}, {value}.Err
            ",
            nil = self.current_zero_value(),
        ));
        return Some(out.no_value());
    }

    fn return_value_is_option(&mut self, expr: &Expr) -> Option<EmitResult> {
        let ret_ty = expr.get_type();
        if !ret_ty.is_option() {
            return None;
        }

        let mut out = emitter();

        // Handle cases where return value is an Option

        // 1. Function call to a Option constructor, ie. Some() or None()
        // 2. Function call to any other function that produces an Option
        // 3. A value (match expression, variable) that needs to be unwrapped

        if let Expr::Call { func, args, .. } = expr {
            let variant = func.as_option_constructor();

            if variant == Some(Ok(())) {
                // 1a. Function call to Option::Some
                let arg = self.emit_local(&args[0], &mut out);
                out.emit(format!("return {arg}, true"));
                //
            } else if variant == Some(Err(())) {
                // 1b. Function call to Option::None
                out.emit(format!(
                    "return {nil}, false",
                    nil = self.current_zero_value(),
                ));
                //
            } else {
                // 2. Function call to any other function that produces an Option
                let call = self.emit_call(func, args, CallWrapMode::Unwrapped);
                out.emit(call.output);
                out.emit(format!("return {value}", value = call.value.unwrap()));
            }

            return Some(out.no_value());
        };

        // 3. A value (match expression, variable) that needs to be unwrapped
        let value = self.emit_local(expr, &mut out);

        out.emit(format!(
            "
        if {value}.IsSome() {{
            return {value}.Some, true
        }}
        return {nil}, false
            ",
            nil = self.current_zero_value(),
        ));
        return Some(out.no_value());
    }

    fn collect_imports(&self, file: &File) -> HashMap<String, String> {
        let mut ret = HashMap::new();

        for e in &file.decls {
            if let Expr::UsePackage { import, .. } = e {
                let m = self
                    .instance
                    .gs
                    .get_module(&ModuleId::from_str(&import.name))
                    .unwrap();

                let name = if m.import.name != m.import.path {
                    m.import.prefix()
                } else {
                    "".to_string()
                };

                ret.insert(m.import.path, name);
            }
        }

        ret
    }

    fn emit_index(&mut self, expr: &Expr, index: &Expr) -> EmitResult {
        let mut out = emitter();
        let new_expr = self.emit_local(expr, &mut out);
        let new_index = self.emit_local(index, &mut out);
        out.emit_value(format!("{new_expr}[{new_index}]"))
    }

    fn generate_or_reuse(&mut self, ctx: &Ctx, ty: &Type, out: &mut Emitter) -> Ctx {
        match ctx {
            Ctx::Discard => Ctx::Discard,
            Ctx::Var(var) => Ctx::Var(var.to_string()),
            Ctx::Arg => {
                let var = self.fresh_var();
                let instantiated = self.to_type(&ty);
                out.emit(format!("var {var} {instantiated}"));
                Ctx::Var(var.to_string())
            }
        }
    }

    // A function call needs to be wrapped when it returns (ok, err) or (ok, bool)
    fn call_wrap_mode(&self, ret: &Type) -> CallWrapMode {
        if ret.is_option() {
            return CallWrapMode::Wrapped;
        }

        if ret.is_result() {
            // A Result<T, E> can only be wrapped if the E is an interface
            // otherwise the err != nil check would fail
            let err_sym = ret.get_args().unwrap()[1].get_symbol();

            // TODO asdf this check is not even correct, it should be any type that implements
            // the error interface or is an interface.
            let m = self.instance.gs.get_module(&err_sym.module).unwrap();
            let is_interface = m.interfaces.get(&err_sym);

            return if is_interface.is_some() {
                CallWrapMode::Wrapped
            } else {
                CallWrapMode::Unwrapped
            };
        }

        CallWrapMode::Unwrapped
    }

    fn return_to_type(&self, ret: &Type) -> String {
        if self.call_wrap_mode(ret) == CallWrapMode::Wrapped {
            if ret.is_result() {
                let args = ret.get_args().unwrap();
                let ok = self.to_type(&args[0]);
                let err = self.to_type(&args[1]);

                return format!("({ok}, {err})");
            }

            if ret.is_option() {
                let args = ret.get_args().unwrap();
                let ok = self.to_type(&args[0]);

                return format!("({ok}, bool)");
            }
        }

        self.to_type(ret)
    }

    fn constructor_make_function(
        &self,
        def: &EnumDefinition,
        name: &str,
        con: &Constructor,
    ) -> String {
        let mut out = emitter();

        let ty_name = &def.name;
        let tag = format!("{ty_name}_{cons}", cons = con.name);

        let (fields, params): (Vec<_>, Vec<_>) = con
            .fields
            .iter()
            .enumerate()
            .map(|(index, f)| {
                let arg = format!("arg_{index}");
                let sig = self.to_type(&f.ty);
                let param = format!("{arg} {sig}");

                let field_name = constructor_field_name(&con.name, con.fields.len(), index);
                let field = format!("{field_name}: {arg}");

                (field, param)
            })
            .unzip();

        let fields = fields.join(", ");
        let params = params.join(", ");

        let (generic_params, generic_args) = if def.generics.is_empty() {
            ("".to_string(), "".to_string())
        } else {
            let args = def
                .generics
                .clone()
                .into_iter()
                .map(|g| g.name)
                .collect::<Vec<_>>()
                .join(", ");
            let generics = generics_to_string(&def.generics);
            (generics, format!("[{args}]"))
        };

        let ret = Type::Con {
            id: Symbol::ethereal(&def.name),
            args: def
                .generics
                .iter()
                .map(|g| Type::Con {
                    id: Symbol::ethereal(&g.name),
                    args: vec![],
                })
                .collect(),
        };

        let ret = self.to_type(&ret);

        out.emit(format!(
            "
func {name} {generic_params} ({params}) {ret} {{
    return {ty_name} {generic_args} {{ tag: {tag}, {fields} }}
}}",
        ));

        out.render()
    }

    fn get_std_imports(&self) -> HashMap<String, String> {
        vec![
            ("fmt".to_string(), "".to_string()),
            ("reflect".to_string(), "".to_string()),
        ]
        .into_iter()
        .collect()
    }

    fn to_type(&self, ty: &Type) -> String {
        match ty {
            Type::Con { id, args } => {
                // TODO asdf this should look at the whole symbol, checking that all these types are
                // actually defined in "std"

                // if the name isn't local and it isn't from std
                // then it's an imported type and we need to render it with the package prefixed
                let name = if !id.module.is_std()
                    && !id.module.is_empty()
                    && id.module != self.current_module
                {
                    let m = self.instance.gs.get_module(&id.module).unwrap();

                    // TODO resolve to the correct import name, if it's been renamed in "use" stmt
                    let import = m.import.prefix();

                    format!("{}.{}", import, id.name)
                } else {
                    id.name.to_string()
                };

                // let name = name.replace("::", ".");

                let new_args = args
                    .iter()
                    .map(|a| self.to_type(a))
                    .collect::<Vec<_>>()
                    .join(", ");

                if name == "Unit" {
                    return "struct{}".to_string();
                }

                if let Some(coll) = is_collection_type(ty) {
                    let new_args = ty.remove_references().get_args().unwrap();

                    if coll == "Slice" {
                        return format!("[]{k}", k = self.to_type(&new_args[0]));
                    }

                    if coll == "Map" {
                        return format!(
                            "map[{k}]{v}",
                            k = self.to_type(&new_args[0]),
                            v = self.to_type(&new_args[1])
                        );
                    }

                    if coll == "Channel" {
                        return format!("chan {k}", k = self.to_type(&new_args[0]));
                    }

                    if coll == "Sender" {
                        return format!("chan<- {k}", k = self.to_type(&new_args[0]));
                    }

                    if coll == "Receiver" {
                        return format!("<-chan {k}", k = self.to_type(&new_args[0]));
                    }
                }

                if name == "Ref" || name == "RefMut" {
                    return format!("*{new_args}");
                }

                if name == "EnumerateSlice" {
                    return format!("[]{new_args}");
                }

                if name == "VarArgs" {
                    return format!("...{new_args}");
                }

                if args.is_empty() {
                    return name.to_string();
                }

                format!("{name}[{new_args}]")
            }

            Type::Fun { args, ret, .. } => {
                let args = args
                    .iter()
                    .map(|a| self.to_type(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.to_type(ret);
                format!("func ({args}) {ret}")
            }

            Type::Var(_) => format!("any"),
            // Type::Var(_) => panic!("unexpected Type::Var in to_type"),
        }
    }

    fn render_generics_instantiated(&self, generics_instantiated: &[Type]) -> String {
        if generics_instantiated.is_empty() {
            "".to_string()
        } else {
            let instantiated = generics_instantiated
                .iter()
                .map(|t| self.to_type(t))
                .collect::<Vec<_>>()
                .join(", ");

            format!("[{instantiated}]")
        }
    }

    fn collect_make_functions(
        &mut self,
        files: &HashMap<FileId, File>,
    ) -> HashMap<FileId, Vec<String>> {
        let mut ret: HashMap<FileId, Vec<String>> = HashMap::new();

        for file in files.values() {
            for expr in &file.decls {
                // TODO asdf use module.enums instead?
                // for def in module.enums.values() {
                //     source.emit(self.create_make_function(&def));
                // }

                if let Expr::EnumDef { def, .. } = expr {
                    ret.entry(file.id.clone())
                        .or_default()
                        .push(self.create_make_function(&def));
                }
            }
        }

        ret
    }

    fn extend_imports(
        &mut self,
        imports: &HashMap<String, String>,
        ensure_imported: &HashSet<ModuleId>,
    ) -> HashMap<String, String> {
        // imports: path => name
        let mut imports = imports.clone();

        for i in ensure_imported {
            let m = self.instance.gs.get_module(i).unwrap();
            if !imports.contains_key(&m.import.path) {
                // TODO asdf this should account for package imported under a different name,
                // when that is supported
                imports.insert(m.import.path, "".to_string());
            }
        }

        imports
    }

    pub fn add_pkg_imports(&mut self, types: &[Type]) {
        for ty in types {
            match ty {
                Type::Con { id, args } => {
                    let m = id.module.clone();
                    if !m.is_std() && !m.is_empty() {
                        self.ensure_imported.insert(m);
                    }

                    self.add_pkg_imports(args);
                }

                _ => unreachable!(),
            }
        }
    }
}

fn wrap_block_and_get_statements(expr: &Expr) -> Vec<Expr> {
    let body = ensure_wrapped_in_block(&expr);
    match body {
        Expr::Block { stmts, .. } => stmts,
        _ => unreachable!(),
    }
}

fn is_standalone(expr: &Expr) -> bool {
    !matches!(
        expr,
        Expr::If { .. }
            | Expr::Match { .. }
            | Expr::Block { .. }
            | Expr::Loop { .. }
            | Expr::Try { .. }
    )
}

fn ensure_wrapped_in_block(expr: &Expr) -> Expr {
    if matches!(expr, Expr::Block { .. }) {
        return expr.clone();
    }

    Expr::Block {
        stmts: vec![expr.clone()],
        ty: Type::dummy(),
        span: Span::dummy(),
    }
}

fn ensure_wrapped_in_return(expr: &Expr) -> Expr {
    if matches!(expr, Expr::Return { .. })
        || expr.get_type().is_unit()
        || expr.get_type().is_discard()
    {
        return expr.clone();
    }

    Expr::Return {
        expr: expr.to_owned().into(),
        ty: Type::dummy(),
        span: Span::dummy(),
    }
}

fn assign_to_result(ctx: &Ctx, value: &str) -> String {
    if value.is_empty() || value == "_" {
        return "".to_string();
    }

    let result = if let Ctx::Var(var) = ctx {
        Some(var.clone())
    } else {
        None
    };

    // this prevents empty blocks from being created
    if result.is_none() && value == empty_struct() {
        return "".to_string();
    }

    let result_assign = match &result {
        Some(var) => {
            // in nested if/match expressions, the final assignment may
            // look like "var1 = var1". Skip emitting in that case.
            if var == value {
                return "".to_string();
            } else {
                format!("{var} = ")
            }
        }
        None => "".to_string(),
    };

    format!("{result_assign} {value}")
}

fn emitter() -> Emitter {
    Emitter { output: vec![] }
}

fn to_name(value: &str) -> String {
    value.replace(".", "_")
}

fn constructor_field_name(con_name: &str, fields_count: usize, index: usize) -> String {
    format!(
        "{field_name}{index}",
        field_name = con_name,
        index = if fields_count <= 1 {
            "".to_string()
        } else {
            format!("{index}")
        }
    )
}

fn generics_to_string(generics: &[Generic]) -> String {
    if generics.is_empty() {
        return "".to_string();
    }

    let generics = generics
        .iter()
        .map(|g| {
            let matching: Vec<_> = g.bounds.iter().map(|ann| ann.get_name().unwrap()).collect();

            let constraint = match matching.len() {
                0 => "any".to_string(),
                1 => matching[0].to_string(),
                _ => {
                    let ifaces = matching.join("; ");
                    format!("interface {{ {ifaces} }}")
                }
            };

            format!("{name} {constraint}", name = g.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!("[{generics}]")
}

fn change_collection_methods(
    fun: &Function,
    receiver: Option<(String, Type)>,
) -> (Function, Option<(String, Type)>) {
    if receiver.is_none() {
        return (fun.clone(), None);
    }

    let receiver = receiver.unwrap();

    if let Some(ty_name) = is_collection_type(&receiver.1) {
        // Use the old function
        let mut updated = fun.clone();

        // Update the function name
        updated.name = format!("{ty_name}.{name}", name = fun.name);

        let self_binding = Binding {
            pat: Pat::Type {
                ident: receiver.0,
                is_mut: false,
                ann: crate::ast::TypeAst::Unknown,
                span: Span::dummy(),
            },
            ann: crate::ast::TypeAst::Unknown,
            ty: receiver.1,
        };

        updated.args.insert(0, self_binding);

        return (updated, None);
    }

    (fun.clone(), Some(receiver))
}

fn is_collection_type(ty: &Type) -> Option<String> {
    let ty_name = ty.remove_references().get_name()?;
    if ["Slice", "Map", "Channel", "Sender", "Receiver", "string"].contains(&ty_name.as_str()) {
        return Some(ty_name);
    }

    None
}
