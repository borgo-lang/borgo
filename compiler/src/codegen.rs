use std::collections::{HashMap, VecDeque};

use crate::{
    ast::{
        Arm, Binding, Constructor, DebugKind, EnumDefinition, Expr, ExternKind, Function,
        FunctionKind, Literal, Loop, LoopFlow, Operator, Pat, Span, StructDefinition, StructField,
        UnOp,
    },
    global_state::{self, Declaration, DerivedOverload},
    project::{Package, Project},
    type_::Type,
};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct EmittedFile {
    pub name: String,
    pub source: String,
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
            value: Some("borgo.Unit".to_string()),
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

#[derive(Debug, Clone)]
enum Ctx {
    Discard,
    Var(String),
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
        }
    }

    fn is_discard(&self) -> bool {
        match self {
            Ctx::Discard => true,
            Ctx::Var(_) => false,
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
    pub gs: global_state::GlobalState,

    // LIFO stack of results of evaluating expressions
    stack: VecDeque<String>,

    // let bindings are re-bound to temporary vars
    // ie. let x = 1
    // becomes `var varN any; varN = 1`
    // this allows inner scopes to reference existing bindings
    // IDENT => VAR
    scope: Scope,

    // For loops can iterate over Seq values
    // `continue` statements need to advance the iterator
    // for the proper loop
    current_loop: Option<String>,
}

enum CallMode {
    Push,
    Bare,
}

impl Codegen {
    pub fn new(gs: &global_state::GlobalState) -> Self {
        Self {
            next_var: 0,
            make_functions: Default::default(),
            gs: gs.clone(),
            stack: Default::default(),
            scope: Scope::new(),
            current_loop: None,
        }
    }

    pub fn compile_package(&mut self, pkg: &Package) -> EmittedFile {
        let mut source = emitter();

        source.emit("package main".to_string());
        source.emit("import borgo \"borgo/runtime\"".to_string());

        // prevent 'imported and not used' errors
        source.emit("var _ = borgo.Unit".to_string());

        // emit make functions first
        pkg.files.iter().for_each(|file| {
            file.decls.iter().for_each(|expr| {
                if let Expr::EnumDef { def, .. } = expr {
                    source.emit(self.create_make_function(def));
                }

                if let Expr::StructDef { def, .. } = expr {
                    source.emit(self.create_struct_make_function(def));
                }

                if let Expr::ExternDecl { items, kind, .. } = expr {
                    if kind == &ExternKind::Overload {
                        return;
                    }

                    items.iter().for_each(|item| match &item {
                        Expr::Closure { fun, ty, .. } => {
                            let mangled = to_name(&fun.name);
                            // TODO note that the type signature here for overloads is wrong.
                            // The dummy type doesn't get replaced during inference.
                            let sig = to_loose_type_signature(ty);
                            source.emit(format!("var {mangled} {sig}"));
                        }

                        _ => unreachable!(),
                    });
                }

                if let Expr::Const { ident, expr, .. } = expr {
                    let body = self
                        .emit_expr(
                            EmitMode {
                                ctx: Ctx::Discard,
                                should_return: true,
                            },
                            &ensure_wrapped_in_block(&expr),
                        )
                        .to_statement();

                    source.emit(format!(
                        "var {ident} any
func init_const_{ident}() any {body}"
                    ));
                }
            });
        });

        pkg.files.iter().for_each(|file| {
            file.decls.iter().for_each(|expr| {
                self.next_var = 0;
                self.stack = Default::default();
                self.scope.reset();

                let value = self.emit_expr(EmitMode::top_level(), expr);
                source.emit(value.to_statement())
            });
        });

        // Generate automatically derived overloads
        source.emit(self.emit_derived_overloads(&pkg.name));

        // emit `init` function to populate known types
        source.emit(self.emit_init_function(pkg));

        // emit blanket implementation for overloads.
        // TODO we need to store the FileId for each overload declaration.
        // For now we do this once in the std package, bit hacky.
        if pkg.name == Project::std() {
            source.emit(self.emit_blanket_overloads());
        }

        EmittedFile {
            name: pkg.name.to_string() + ".go",
            source: source.render(),
        }
    }

    fn emit_init_function(&self, pkg: &Package) -> String {
        let mut out = emitter();

        let register_global_fn = |out: &mut Emitter, expr: &Expr| match expr {
            Expr::Closure { fun, .. } => {
                let name = &fun.name;
                let mangled = to_name(&fun.name);
                out.emit(format!(
                    "borgo.RegisterGlobalFunction(\"{name}\", {mangled})"
                ));
            }

            _ => unreachable!(),
        };

        pkg.files.iter().for_each(|file| {
            file.decls.iter().for_each(|expr| {
                if let Expr::EnumDef { def, .. } = expr {
                    def.cons.iter().for_each(|con| {
                        let constructor = def.name.clone() + "::" + &con.name;
                        let mangled = to_name(&constructor);
                        out.emit(format!(
                            "borgo.RegisterTypeConstructor(\"{constructor}\", \"{mangled}\")"
                        ));

                        let make_fn = "make_".to_string() + &mangled;
                        out.emit(format!(
                            "borgo.RegisterMakeFunction(\"{constructor}\", {make_fn})"
                        ));
                    })
                }

                if let Expr::StructDef { def, .. } = expr {
                    let name = def.name.clone();
                    let mangled = to_name(&name);
                    let fields = def
                        .fields
                        .iter()
                        .map(|f| format!("\"{name}\"", name = f.name))
                        .collect::<Vec<_>>()
                        .join(", ");

                    out.emit(format!(
                        "borgo.RegisterStruct(\"{name}\", \"{mangled}\", []string{{ {fields} }})"
                    ));

                    let make_fn = "make_".to_string() + &mangled;
                    out.emit(format!("borgo.RegisterMakeFunction(\"{name}\", {make_fn})"));
                }

                if let Expr::ExternDecl { items, kind, .. } = expr {
                    if kind == &ExternKind::Overload {
                        return;
                    }

                    items.iter().for_each(|item| match &item {
                        Expr::Closure { fun, ty, .. } => {
                            let name = &fun.name;
                            let mangled = to_name(&fun.name);
                            let arity = ty.get_function_args().unwrap().len();
                            out.emit(format!("{mangled} = borgo.GetNative{arity}(\"{name}\")"));
                        }

                        _ => unreachable!(),
                    });
                }

                if let Expr::ImplBlock { items, .. } = expr {
                    items.iter().for_each(|e| register_global_fn(&mut out, e));
                }

                if let Expr::Closure { .. } = expr {
                    register_global_fn(&mut out, expr);
                }

                if let Expr::Const { ident, .. } = expr {
                    out.emit(format!("{ident} = init_const_{ident}()"));
                }
            });
        });

        format!(
            "func pkg_{name}_init() {{
            {init}
        }}",
            name = pkg.name,
            init = out.render()
        )
    }

    fn fresh_var(&mut self) -> String {
        self.next_var += 1;
        format!("var{next}", next = self.next_var)
    }

    fn create_make_function(&mut self, def: &EnumDefinition) -> String {
        let mut out = emitter();

        def.cons.iter().for_each(|con| {
            let constructor = def.name.clone() + "::" + &con.name;
            let constructor = to_name(&constructor);
            let name = format!("make_{constructor}");
            out.emit(constructor_make_function(&name, &constructor, con));
            self.make_functions.insert(constructor, name);
        });

        out.render()
    }

    fn create_struct_make_function(&mut self, def: &StructDefinition) -> String {
        // Emitting a make function for structs is not strictly necessary because it just forwards
        // all params to the actual struct ie. Option_Some { arg_0 }
        // In fact, these make functions are not used in this module.
        // However they still need to be around for the runtime and eventually the interpreter.
        let struct_name = def.name.clone();
        let name = to_name(&struct_name);

        let params = def
            .fields
            .iter()
            .enumerate()
            .map(|(index, _)| format!("arg_{index} any"))
            .collect::<Vec<_>>()
            .join(", ");

        let call_args = def
            .fields
            .iter()
            .enumerate()
            .map(|(index, field)| {
                format!(
                    "arg_{index}{cast}",
                    cast = cast_to(&to_loose_type_signature(&field.ty)),
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "func make_{name} ({params}) any {{
        return {struct_name} {{ {call_args} }}
        }}"
        )
    }

    // Most expressions will ignore mode and ctx
    // Only if/match/block have to care about it
    fn emit_expr(&mut self, mode: EmitMode, expr: &Expr) -> EmitResult {
        match expr {
            Expr::Closure { fun, kind, .. } => self.emit_closure(fun, kind),
            Expr::Block { stmts, .. } => self.emit_block(mode, stmts),
            Expr::Call { func, args, .. } => self.emit_call(func, args, CallMode::Push),
            Expr::Literal { lit, .. } => self.emit_literal(lit),
            Expr::ExternDecl { .. } => EmitResult::empty(),
            Expr::Debug { kind, expr, .. } => self.emit_debug(kind, expr),
            Expr::Return { expr, .. } => self.emit_return(expr),
            Expr::Var { value, .. } => self.emit_var(value),
            Expr::Match { subject, arms, .. } => self.emit_match(&mode.ctx, subject, arms),
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
            Expr::ImplBlock { items, .. } => self.emit_impl(items),
            Expr::StructCall {
                name, fields, rest, ..
            } => self.emit_struct_call(name, fields, rest),
            Expr::StructAccess { expr, field, .. } => self.emit_struct_access(expr, field),
            Expr::Try { expr, .. } => self.emit_try(&mode.ctx, expr),
            Expr::Unary { op, expr, .. } => self.emit_unary(op, expr),
            Expr::Noop { .. } => EmitResult::unit(),
            Expr::Loop { kind, body, .. } => match kind {
                Loop::NoCondition => self.emit_loop(body),
                Loop::WithCondition { binding, expr } => {
                    self.emit_loop_with_condition(binding, expr, body)
                }
            },

            Expr::Flow { kind, .. } => self.emit_loop_flow(kind),
            Expr::VarUpdate { value, target, .. } => self.emit_var_update(value, target),
            Expr::Const { .. } => EmitResult::empty(),
            Expr::CheckType { .. } => EmitResult::empty(),
            Expr::Paren { expr, .. } => self.emit_paren(expr),
            Expr::Spawn { expr, .. } => self.emit_spawn(expr),
            Expr::Select { arms, .. } => self.emit_select(arms),

            Expr::Tuple { .. } => unreachable!(),
            Expr::MethodCall { .. } => unreachable!(),
            Expr::Todo { .. } => todo!(),
        }
    }

    fn emit_trait_impelementation(&self, def: &DerivedOverload) -> String {
        let trait_fn = format!("{ty}::{overload}", ty = def.ty, overload = def.overload);

        format!(
            "func {name}(values ...any) any {{
return borgo.OverloadImpl(\"{trait_name}\", values)
        }}",
            name = to_name(&trait_fn),
            trait_name = def.overload
        )
    }

    fn emit_derived_overloads(&self, pkg: &str) -> String {
        let mut out = emitter();

        let overloads = self.gs.get_derived_overloads();
        let mut overloads: Vec<_> = overloads.iter().collect();

        overloads.sort();

        overloads.iter().for_each(|d| {
            let typ = self.gs.get_global_type_declaration(&d.ty).unwrap();

            // Make sure type was declared in the current package
            if typ.decl.file_id.package == pkg {
                out.emit(self.emit_trait_impelementation(d));
            }
        });

        out.render()
    }

    fn emit_blanket_overloads(&self) -> String {
        let mut out = emitter();

        let overloads = self.gs.get_overloads();
        let mut overloads: Vec<_> = overloads.iter().collect();

        overloads.sort();

        overloads.iter().for_each(|overload| {
            let mangled = to_name(&overload);

            out.emit(format!(
                "func {mangled} (values ...any) any {{
    return borgo.OverloadImpl(\"{overload}\", values)
}}"
            ));
        });

        out.render()
    }

    fn emit_local(&mut self, expr: &Expr, out: &mut Emitter) -> String {
        let var = self.fresh_var();
        let ctx = Ctx::Var(var.clone());

        let res = self.emit_expr(ctx.to_mode(), expr);

        // Only produce a variable binding if there is an output.
        // An example where this is reduntant is a block like { 5 }
        // which will only emit a value.
        if !res.output.trim().is_empty() && res.value.is_some() {
            // This is a bit of a mess.
            // In theory, it'd be better if emit_local also got a Ctx
            // so that it knows not to output a variable. Probably there's
            // a good fix I just can't think of any right now.
            out.emit(format!(
                "var {var} any
_ = {var}"
            ));
        }

        let value = res.value.unwrap_or_default();

        out.emit(res.output);
        value
    }

    fn emit_pattern(&mut self, subject: &str, is_matching: &str, pat: &Pat) -> String {
        let mut out = emitter();

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
if {is_matching} != borgo.MatchErr && borgo.Ops.Eq({value}, {subject}).(bool) {{
    {is_matching} = borgo.MatchOk
}} else {{
    {is_matching} = borgo.MatchErr
}}"
                ));
            }

            Pat::Type { ident, .. } => {
                out.emit(format!("{ident} := {subject}"));
                self.scope.add_binding(ident.clone(), ident.clone());
            }

            Pat::Pat { ident, elems, .. } => {
                let pat = self.fresh_var() + "_pat";

                // Introduce a new sentinel matching value for the nested pattern match.
                //
                // For example, if we're trying to match (1, "foo")
                // new_is_matching will be 2 if both fields match
                // And then we can forward the success value to the parent is_matching
                let new_is_matching = self.fresh_var() + "_match_pat";
                out.emit(format!("{new_is_matching} := borgo.MatchNone"));

                let new_elems = elems
                    .iter()
                    .enumerate()
                    .map(|(index, p)| {
                        let new_subject = format!("{pat}.Field{index}");
                        self.emit_pattern(&new_subject, &new_is_matching, p)
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                let name = to_name(ident);

                // Check against new_is_matching but override parent is_matching for final check
                out.emit(format!(
                    "
{pat}, constructor_check := {subject}.({name})
_ = {pat}

{new_elems}

if {new_is_matching} != borgo.MatchErr && constructor_check {{
    {is_matching} = borgo.MatchOk
}} else {{
    {is_matching} = borgo.MatchErr
}}
"
                ));
            }

            Pat::Struct { fields, ty, .. } => {
                let ty = to_type(ty);

                let new_is_matching = self.fresh_var() + "_match_pat";
                out.emit(format!("{new_is_matching} := borgo.MatchNone"));

                fields.iter().for_each(|f| {
                    let field_name = to_struct_field(&f.name);
                    let cast = cast_to(&ty);
                    let pat = self.emit_pattern(
                        &format!("{subject}{cast}.{field_name}"),
                        &new_is_matching,
                        &f.value,
                    );
                    out.emit(pat);
                });

                out.emit(format!(
                    "
if {new_is_matching} != borgo.MatchErr {{
    {is_matching} = borgo.MatchOk
}} else {{
    {is_matching} = borgo.MatchErr
}}
"
                ));
            }

            Pat::Wild { .. } => out.emit(format!(
                "if {is_matching} != borgo.MatchErr {{ {is_matching} = borgo.MatchOk /* wildcard */ }}"
            )),
            Pat::Unit { .. } => out.emit(format!(
                "if {is_matching} != borgo.MatchErr {{ {is_matching} = borgo.MatchOk /* Unit */ }}"
            )),
        };

        out.render()
    }

    fn emit_closure(&mut self, fun: &Function, kind: &FunctionKind) -> EmitResult {
        let mut out = emitter();
        let mut args_destructure = emitter();

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

                name + " " + &to_loose_type_signature(&a.ty)
            })
            .collect::<Vec<_>>()
            .join(", ");

        let body = self
            .emit_expr(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: true,
                },
                &ensure_wrapped_in_block(&fun.body),
            )
            .to_statement();

        // If there's something left on the stack, then return it
        // let ret = match self.stack.pop_front() {
        // Some(var) => format!("return {var}"),
        // None => "".to_string(),
        // };

        // There are 3 cases here:
        // - Top level functions are always emitted as `func $name`
        // - Lambdas are bound to an anonymous function `$name := func`
        // - Inline functions may be recursive, so need a declaration first
        //     `var $name func(..); $name =`

        let name = to_name(&fun.name);

        let binding = match kind {
            FunctionKind::TopLevel => format!("func {name} ({args}) any"),

            FunctionKind::Lambda => format!("func ({args}) any"),

            FunctionKind::Inline => {
                let typed_args = fun
                    .args
                    .iter()
                    .map(|_| "any".to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut decl = emitter();
                decl.emit(format!("var {name} func ({typed_args}) any"));
                decl.emit(format!("{name} = func ({args}) any"));

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

        // if let Some(to_push) = to_push {
        // out.emit(self.push(to_push));
        // }

        out.as_value()
    }

    fn emit_block(&mut self, mode: EmitMode, stmts: &[Expr]) -> EmitResult {
        let mut out = emitter();
        let mut stmts = stmts.to_vec();

        let unit = Expr::Unit {
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

        if needs_braces {
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
                let value = self.emit_local(&last, &mut out);
                Some(value)
            }
        };

        self.scope.exit();

        if needs_braces {
            out.emit("}".to_string());
        }

        out.try_emit(result)
    }

    fn emit_call(&mut self, func: &Expr, args: &[Expr], call_mode: CallMode) -> EmitResult {
        let mut output = emitter();

        let new_args = args
            .iter()
            .map(|a| self.emit_local(a, &mut output))
            .collect::<Vec<_>>()
            .join(", ");

        let new_func = self.emit_local(func, &mut output);
        let call = format!("{new_func}({new_args})");

        // Normal function calls are pushed to the stack.
        // In case a bare function call is needed (when spawning a coroutine, which requires an
        // explicit function call) then we only push the function call itself.
        match call_mode {
            CallMode::Push => {
                // output.emit(self.push(call));
            }
            CallMode::Bare => {
                // self.stack.push_front(call);
            }
        };

        output.emit_value(call)
    }

    fn emit_literal(&mut self, lit: &Literal) -> EmitResult {
        let mut out = emitter();

        let value = match lit {
            Literal::Int(n) => n.to_string(),
            Literal::Float(n) => n.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(s) => format!("\"{}\"", s).replace('\n', "\\n"),
            Literal::Char(s) => format!("\'{}\'", s),
            Literal::List(elems) => {
                let new_elems = elems
                    .iter()
                    .map(|e| self.emit_local(e, &mut out))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("borgo.List({new_elems})")
            }
        };

        out.emit_value(value)
    }

    fn emit_debug(&mut self, kind: &DebugKind, expr: &Expr) -> EmitResult {
        let mut out = emitter();

        let var = match kind {
            DebugKind::Unreachable => "".to_string(),
            DebugKind::Todo => "".to_string(),
            DebugKind::Inspect => self.emit_local(expr, &mut out),
        };

        let fn_name = format!("{:#?}", kind).to_lowercase();
        out.emit_value(format!("Debug_{fn_name}({var})"))
    }

    fn emit_return(&mut self, expr: &Expr) -> EmitResult {
        let mut out = emitter();
        let mut value = self.emit_local(expr, &mut out);

        // TODO this sucks
        if value.is_empty() {
            value = "borgo.Unit".to_string()
        }

        out.emit(format!("return {value}"));
        out.no_value()
    }

    fn emit_match(&mut self, ctx: &Ctx, subject: &Expr, arms: &[Arm]) -> EmitResult {
        let mut out = emitter();

        let subject = self.emit_local(subject, &mut out);

        let subject_var = self.fresh_var() + "_subject";
        let is_matching = self.fresh_var() + "_matches";

        // Matching status:
        //   None -> pattern matching hasn't started
        //   Err  -> pattern matching failed
        //   Ok   -> pattern matching succeeded

        out.emit(format!(
            " {subject_var} := {subject}
{is_matching} := borgo.MatchNone

",
        ));

        arms.iter().for_each(|arm| {
            let if_statements = self.emit_pattern(&subject_var, &is_matching, &arm.pat);

            let result = self.emit_expr(ctx.clone().to_mode(), &arm.expr);
            let assign = assign_to_result(&ctx, &result.value.unwrap_or_default());
            let preamble = result.output;

            out.emit(format!(
                "
if {is_matching} != borgo.MatchOk {{
    {is_matching} = borgo.MatchNone

    {if_statements}
    _ = {subject_var}

    if {is_matching} == borgo.MatchOk {{
        {preamble}
        {assign}
    }}
}}
"
            ))
        });

        out.try_emit(ctx.to_var())
    }

    fn emit_var(&mut self, value: &str) -> EmitResult {
        let name = self
            .scope
            .get_binding(value)
            .unwrap_or_else(|| to_name(&self.gs.resolve_name(value)));

        let expr = self.make_functions.get(&name).cloned().unwrap_or(name);

        EmitResult {
            output: "".to_string(),
            value: Some(expr),
        }
    }

    fn emit_let(&mut self, binding: &Binding, value: &Expr) -> EmitResult {
        let mut out = emitter();

        let new_value = self.emit_local(value, &mut out);

        match binding.pat {
            Pat::Type { ref ident, .. } => {
                if self.scope.is_defined_in_current_scope(ident) {
                    out.emit(format!("{ident} = {new_value}"));
                } else {
                    out.emit(format!("var {ident} any = {new_value}"));
                }

                self.scope.add_binding(ident.clone(), ident.clone());
            }

            Pat::Wild { .. } => {
                out.emit(format!("_ = {new_value}"));
            }

            _ => {
                let var = self.fresh_var();
                out.emit(format!("{var} := {new_value}"));

                let pat = self.emit_pattern(&var, "_", &binding.pat);
                out.emit(pat);
            }
        };

        out.no_value()
    }

    fn emit_binary(&mut self, op: &Operator, left: &Expr, right: &Expr) -> EmitResult {
        // Transform in function call

        let prefix = match op {
            Operator::Eq | Operator::Ne | Operator::Or | Operator::And => "".to_string(),
            _ => self.math_op_prefix(left.get_type()),
        };

        let func = Expr::Var {
            value: format!("borgo.Ops.{prefix}{:?}", op),
            decl: Declaration::dummy(),
            ty: Type::dummy(),
            span: Span::dummy(),
        };

        self.emit_call(&func, &[left.clone(), right.clone()], CallMode::Push)
    }

    fn emit_unary(&mut self, op: &UnOp, expr: &Expr) -> EmitResult {
        // Transform in function call

        let prefix = match op {
            UnOp::Neg => self.math_op_prefix(expr.get_type()),
            UnOp::Not => "".to_string(),
        };

        let func = Expr::Var {
            value: format!("borgo.Ops.{prefix}{:?}", op),
            decl: Declaration::dummy(),
            ty: Type::dummy(),
            span: Span::dummy(),
        };

        self.emit_call(&func, &[expr.clone()], CallMode::Push)
    }

    fn math_op_prefix(&self, ty: Type) -> String {
        let ty = ty.get_name().unwrap();
        if ty == "Int" {
            "I".to_string()
        } else if ty == "Float" {
            "F".to_string()
        } else {
            unreachable!()
        }
    }

    fn emit_if(&mut self, ctx: &Ctx, cond: &Expr, then: &Expr, els: &Expr) -> EmitResult {
        let mut out = emitter();

        let new_cond = self.emit_local(cond, &mut out);

        let mut wrap_and_assign = |expr: &Expr| {
            let result = self.emit_expr(ctx.clone().to_mode(), &expr);
            let assign = assign_to_result(&ctx, &result.value.unwrap_or_default());
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
            _ => format!("else {}", wrap_and_assign(els)),
        };

        out.emit(format!(
            "if borgo.Ops.Eq({new_cond}, true).(bool) {new_then} {new_else}",
        ));

        out.try_emit(ctx.to_var())
    }

    fn emit_enum(&self, def: &EnumDefinition) -> EmitResult {
        let mut out = emitter();

        def.cons.iter().for_each(|con| {
            let constructor = def.name.clone() + "_" + &con.name;
            out.emit(enum_constructor(&constructor, con));
        });

        // also emit an alias for the type name
        out.emit(format!("type {name} any", name = def.name));

        out.no_value()
    }

    fn emit_struct(&self, def: &StructDefinition) -> EmitResult {
        let mut out = emitter();

        let name = def.name.clone();
        out.emit(format!("type {name} struct {{"));

        def.fields.iter().for_each(|f| {
            let field = to_struct_field(&f.name);
            let ty = to_loose_type_signature(&f.ty);
            out.emit(format!("  {field} {ty}"));
        });

        out.emit("}".to_string());
        out.no_value()
    }

    fn emit_impl(&mut self, items: &[Expr]) -> EmitResult {
        let mut out = emitter();

        items.iter().for_each(|e| {
            self.next_var = 0;
            self.stack = Default::default();
            self.scope.reset();

            let value = self.emit_expr(EmitMode::top_level(), e);
            out.emit(value.to_statement())
        });

        out.no_value()
    }

    fn emit_struct_call(
        &mut self,
        name: &str,
        fields: &[StructField],
        rest: &Option<Expr>,
    ) -> EmitResult {
        // because of `rest` we can't just inline the call, it needs to be wrapped in a func so that the stuff from `rest` can be copied over. If there's no `rest`, then start with the actual name ie. `Seq_Cons{}`

        let mut out = emitter();

        let ty = to_name(name);

        let new_rest = match rest {
            Some(rest) => self.emit_local(rest, &mut out),
            None => ty.to_string() + "{}",
        };

        let new_fields = fields
            .iter()
            .map(|f| {
                let name = to_struct_field(&f.name);
                let value = self.emit_local(&f.value, &mut out);
                format!("data.{name} = {value}")
            })
            .collect::<Vec<_>>()
            .join("\n");

        out.emit_value(format!(
            "func (base any) any {{
    data := base.({ty})
    {new_fields}
    return data
}}({new_rest})"
        ))
    }

    fn emit_struct_access(&mut self, expr: &Expr, field: &str) -> EmitResult {
        let mut out = emitter();
        let new_expr = self.emit_local(expr, &mut out);
        let cast = cast_to(&to_type(&expr.get_type()));
        let field = to_struct_field(field);
        out.emit_value(format!("{new_expr}{cast}.{field}"))
    }

    fn emit_try(&mut self, ctx: &Ctx, expr: &Expr) -> EmitResult {
        let mut out = emitter();

        let check = self.fresh_var() + "_check";

        let value = self.emit_local(expr, &mut out);
        let assign = assign_to_result(&ctx, &format!("ret.Field0"));

        out.emit(format!(
            "{check} := {value}

  if ret, ok := {check}.(Result_Ok); ok {{
    // $ret = ret.Field0
    {assign}
  }} else {{
    return {check}
  }}
"
        ));

        out.try_emit(ctx.to_var())
    }

    fn emit_loop(&mut self, body: &Expr) -> EmitResult {
        let mut out = emitter();

        let prev_loop_expr = self.current_loop.clone();
        self.current_loop = None;
        let body = self
            .emit_expr(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                &ensure_wrapped_in_block(&body),
            )
            .to_statement();
        self.current_loop = prev_loop_expr;

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

        let expr_var = self.fresh_var();
        let sequence = self.emit_local(expr, &mut out);

        let prev_loop_expr = self.current_loop.clone();
        let current_loop_value = self.fresh_var();

        // Create a fake let binding so that patterns are destructured correctly
        let loop_var = self
            .emit_let(
                &binding,
                &Expr::Var {
                    value: current_loop_value.clone(),
                    decl: Declaration::dummy(),
                    ty: Type::dummy(),
                    span: Span::dummy(),
                },
            )
            .to_statement();

        self.current_loop = Some(expr_var.clone());
        let body = self
            .emit_expr(
                EmitMode {
                    ctx: Ctx::Discard,
                    should_return: false,
                },
                &ensure_wrapped_in_block(&body),
            )
            .to_statement();
        self.current_loop = prev_loop_expr;

        out.emit(format!(
            "
            {expr_var} := {sequence}
            for !borgo.ValuesIsOfType({expr_var}, \"Seq::Nil\") {{
    {current_loop_value} := {expr_var}.(Seq_Cons).Field0
    {loop_var}
    {body}

    {expr_var} = {expr_var}.(Seq_Cons).Field1()
}}",
        ));

        out.no_value()
    }

    fn emit_loop_flow(&mut self, kind: &LoopFlow) -> EmitResult {
        let mut out = emitter();

        let token = match kind {
            LoopFlow::Break => "break".to_string(),
            LoopFlow::Continue => {
                // Make sure to update the loop condition first
                let current_loop = match &self.current_loop {
                    Some(expr_var) => {
                        format!("{expr_var} = {expr_var}.(Seq_Cons).Field1()")
                    }
                    None => "".to_string(),
                };

                format!(
                    "{current_loop}
                continue "
                )
            }
        };

        out.emit(token);

        out.no_value()
    }

    fn emit_var_update(&mut self, value: &Expr, target: &Expr) -> EmitResult {
        let mut out = emitter();

        let new_expr = self.emit_local(value, &mut out);

        match target {
            Expr::Var {
                value: var_name, ..
            } => {
                // let actual_var = self.scope.get_binding(var_name).unwrap();
                out.emit(format!("{var_name} = {new_expr}"));
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
                let new_expr = self.emit_call(func, args, CallMode::Bare);
                new_expr.to_statement()
            }

            _ => unreachable!("expected function call in spawn!()"),
        };

        out.emit(format!("go {call}"));
        out.no_value()
    }

    fn emit_select(&mut self, arms: &[Arm]) -> EmitResult {
        let mut out = emitter();

        out.emit("select {".to_string());

        arms.iter().for_each(|a| {
            match &a.pat {
                Pat::Pat { ident, elems, .. } => {
                    // We're looking for ChannelOp::Recv|Send or _

                    if ident == "ChannelOp::Send" {
                        out.emit(self.emit_select_send(&elems[0], &elems[1]));
                    } else if ident == "ChannelOp::Recv" {
                        out.emit(self.emit_select_recv(&elems[0], &elems[1]));
                    } else {
                        unreachable!()
                    }
                }

                Pat::Wild { .. } => {
                    out.emit("case default:".to_string());
                }

                _ => unreachable!(),
            }

            let result = self.emit_expr(Ctx::Discard.to_mode(), &ensure_wrapped_in_block(&a.expr));
            out.emit(result.to_statement());
        });

        out.emit("}".to_string());
        out.no_value()
    }

    fn emit_select_send(&mut self, chan: &Pat, value: &Pat) -> String {
        let chan = match chan {
            Pat::Type { ident, .. } => self.scope.get_binding(ident).unwrap(),
            _ => unreachable!(),
        };

        let value = match value {
            Pat::Type { ident, .. } => self.scope.get_binding(ident).unwrap(),
            _ => unreachable!(),
        };

        format!("case {chan}.(chan any)<- {value}:")
    }

    fn emit_select_recv(&mut self, chan: &Pat, binding: &Pat) -> String {
        let mut var_binding = "".to_string();
        let chan = match chan {
            Pat::Type { ident, .. } => self.scope.get_binding(ident).unwrap(),
            _ => unreachable!(),
        };

        let binding = match binding {
            Pat::Type { ident, .. } => {
                let b = ident.to_string();
                var_binding = format!("var {b} any");
                self.scope.add_binding(b.clone(), b.clone());
                b
            }
            Pat::Wild { .. } => "_".to_string(),
            _ => unreachable!(),
        };

        let value = self.fresh_var() + "_value";
        let more = self.fresh_var() + "_more";

        format!(
            "case {value}, {more} := <-{chan}.(chan any):
{var_binding}
if {more} {{ {binding} = make_Option_Some({value}) }} else {{ {binding} = make_Option_None }}"
        )
    }
}

fn is_standalone(expr: &Expr) -> bool {
    !matches!(
        expr,
        Expr::If { .. } | Expr::Match { .. } | Expr::Block { .. } | Expr::Loop { .. }
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
    if matches!(expr, Expr::Return { .. }) {
        return expr.clone();
    }

    Expr::Return {
        expr: expr.to_owned().into(),
        ty: Type::dummy(),
        span: Span::dummy(),
    }
}

fn assign_to_result(ctx: &Ctx, value: &str) -> String {
    if value.is_empty() {
        return "".to_string();
    }

    let result = if let Ctx::Var(var) = ctx {
        Some(var.clone())
    } else {
        None
    };

    // this prevents empty blocks from being created
    if result.is_none() && value == "borgo.Unit" {
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

// Reflection in Go only works for exported fields.
fn to_struct_field(name: &str) -> String {
    let mut chars = name.chars();
    let first = chars.next().unwrap().to_uppercase().to_string();
    let rest = chars.collect::<String>();
    first + &rest
}

fn emitter() -> Emitter {
    Emitter { output: vec![] }
}

// TODO this is terrible
// and maybe not even necessary?
fn primitives(name: &str) -> Option<&str> {
    let mut p = HashMap::new();
    p.insert("List", "*immutable.List");
    p.insert("Bool", "bool");
    p.insert("String", "string");
    p.insert("Int", "int");
    p.insert("Float", "float64");

    p.get(name).cloned()
}

fn to_name(value: &str) -> String {
    value.replace("::", "_")
}

fn cast_to(ty: &str) -> String {
    if ty == "any" {
        "".to_string()
    } else {
        format!(".({ty})")
    }
}

fn to_type(ty: &Type) -> String {
    match ty {
        Type::Con { name, .. } => primitives(name).map(|_| "any").unwrap_or(name).to_string(),
        Type::Fun { args, .. } => {
            let args = args.iter().map(to_type).collect::<Vec<_>>().join(", ");

            // let ret = to_type(ret);
            format!("func ({args}) any")
        }
        Type::Var(_) => "any".to_string(),
    }
}

fn func_signature_or_any(ty: &Type) -> String {
    match ty {
        Type::Fun { .. } => to_type(ty),
        _ => "any".to_string(),
    }
}

fn to_loose_type_signature(ty: &Type) -> String {
    match ty {
        Type::Con { .. } => "any".to_string(),
        Type::Fun { args, .. } => {
            let args = args
                .iter()
                .map(|_| "any".to_string())
                .collect::<Vec<_>>()
                .join(", ");

            format!("func ({args}) any")
        }
        Type::Var(_) => "any".to_string(),
    }
}

fn enum_constructor(constructor: &str, con: &Constructor) -> String {
    let mut out = emitter();
    out.emit(format!("type {constructor} struct {{"));

    con.fields.iter().enumerate().for_each(|(index, f)| {
        let ty = func_signature_or_any(&f.ty);
        out.emit(format!("  Field{index} {ty}"));
    });

    out.emit("}".to_string());
    out.render()
}

fn constructor_make_function(name: &str, constructor: &str, con: &Constructor) -> String {
    let mut out = emitter();

    if con.fields.is_empty() {
        // no arguments in constructor, don't emit a func ie. Seq::Nil
        return format!("var {name} = {constructor}{{}}");
    }

    let (args, params): (Vec<_>, Vec<_>) = con
        .fields
        .iter()
        .enumerate()
        .map(|(index, f)| {
            let arg = format!("arg_{index}");
            let sig = "any"; //func_signature_or_any(ty);
            let param = format!("{arg} {sig}");

            let cast = cast_to(&to_loose_type_signature(&f.ty));

            (arg + &cast, param)
        })
        .unzip();

    let args = args.join(", ");
    let params = params.join(", ");

    out.emit(format!(
        "
func {name}({params}) any {{
    return {constructor} {{ {args} }}
}}"
    ));

    out.render()
}
