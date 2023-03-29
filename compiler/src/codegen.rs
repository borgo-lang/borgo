use std::collections::{HashMap, VecDeque};

use crate::{
    ast::{
        Arm, Binding, Constructor, DebugKind, EnumDefinition, Expr, ExternKind, Function,
        FunctionKind, Literal, Operator, Pat, Span, StructDefinition, StructField, UnOp,
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
                    source.emit(format!("var {ident} any"));
                    source.emit(format!("func init_const_{ident}() any {{"));

                    let ret = self.emit_local(expr, &mut source);
                    source.emit(format!("return {ret} }}"));
                }
            });
        });

        pkg.files.iter().for_each(|file| {
            file.decls.iter().for_each(|expr| {
                self.next_var = 0;
                self.stack = Default::default();
                self.scope.reset();

                let value = self.emit_expr(expr);
                source.emit(value)
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

    fn emit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::EnumDef { def, .. } => self.emit_enum(def),
            Expr::StructDef { def, .. } => self.emit_struct(def),
            Expr::ImplBlock { items, .. } => self.emit_impl(items),
            Expr::ExternDecl { .. } => "".to_string(),
            Expr::Unit { .. } => self.push("borgo.Unit".to_string()),
            Expr::Closure { fun, kind, .. } => self.emit_closure(fun, kind),
            Expr::Block { stmts, .. } => self.emit_block(stmts),
            Expr::Debug { kind, expr, .. } => self.emit_debug(kind, expr),
            Expr::Literal { lit, .. } => self.emit_literal(lit),
            Expr::Return { expr, .. } => self.emit_return(expr),
            Expr::Call { func, args, .. } => self.emit_call(func, args, CallMode::Push),
            Expr::Var { value, .. } => self.emit_var(value),
            Expr::Match { subject, arms, .. } => self.emit_match(subject, arms),
            Expr::Let { binding, value, .. } => self.emit_let(binding, value),
            Expr::Binary {
                op, left, right, ..
            } => self.emit_binary(op, left, right),
            Expr::Unary { op, expr, .. } => self.emit_unary(op, expr),
            Expr::If {
                cond, then, els, ..
            } => self.emit_if(cond, then, els),
            Expr::StructCall {
                name, fields, rest, ..
            } => self.emit_struct_call(name, fields, rest),
            Expr::StructAccess { expr, field, .. } => self.emit_struct_access(expr, field),
            Expr::Try { expr, .. } => self.emit_try(expr),
            Expr::Noop { .. } => self.push("borgo.Unit".to_string()),
            Expr::Spawn { expr, .. } => self.emit_spawn(expr),
            Expr::Select { arms, .. } => self.emit_select(arms),
            Expr::CheckType { .. } => self.push("borgo.Unit".to_string()),
            Expr::Const { .. } => "".to_string(),
            Expr::Paren { expr, .. } => self.emit_paren(expr),
            Expr::Tuple { .. } => unreachable!(),
            Expr::VarUpdate { .. } => unreachable!(),
            Expr::MethodCall { .. } => unreachable!(),
            Expr::Todo { .. } => todo!(),
        }
    }

    fn emit_enum(&self, def: &EnumDefinition) -> String {
        let mut out = emitter();

        def.cons.iter().for_each(|con| {
            let constructor = def.name.clone() + "_" + &con.name;
            out.emit(enum_constructor(&constructor, con));
        });

        // also emit an alias for the type name
        out.emit(format!("type {name} any", name = def.name));

        out.render()
    }

    fn emit_struct(&self, def: &StructDefinition) -> String {
        let mut out = emitter();

        let name = def.name.clone();
        out.emit(format!("type {name} struct {{"));

        def.fields.iter().for_each(|f| {
            let field = to_struct_field(&f.name);
            let ty = to_loose_type_signature(&f.ty);
            out.emit(format!("  {field} {ty}"));
        });

        out.emit("}".to_string());
        out.render()
    }

    fn emit_impl(&mut self, items: &[Expr]) -> String {
        let mut out = emitter();

        items.iter().for_each(|e| {
            self.next_var = 0;
            self.stack = Default::default();
            self.scope.reset();

            let closure = self.emit_expr(e);
            out.emit(closure)
        });

        out.render()
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

    fn emit_closure(&mut self, fun: &Function, kind: &FunctionKind) -> String {
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
                        let pat = self.emit_pattern(&var, &a.pat);
                        args_destructure.emit(pat);
                        var
                    }
                };

                name + " " + &to_loose_type_signature(&a.ty)
            })
            .collect::<Vec<_>>()
            .join(", ");

        let body = self.emit_expr(&fun.body);

        // If there's something left on the stack, then return it
        let ret = match self.stack.pop_front() {
            Some(var) => format!("return {var}"),
            None => "".to_string(),
        };

        // There are 3 cases here:
        // - Top level functions are always emitted as `func $name`
        // - Lambdas are bound to an anonymous function `$name := func`
        // - Inline functions may be recursive, so need a declaration first
        //     `var $name func(..); $name =`

        let name = to_name(&fun.name);

        let (binding, to_push) = match kind {
            FunctionKind::TopLevel => (format!("func {name} ({args}) any"), None),

            FunctionKind::Lambda => {
                let var = self.fresh_var();
                (format!("{var} := func ({args}) any"), Some(var))
            }

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

                (decl.render(), Some(name))
            }
        };

        out.emit(format!(
            "{binding} {{
{args_destructure}
{body}
{ret}
}}
",
            args_destructure = args_destructure.render()
        ));

        if let Some(to_push) = to_push {
            out.emit(self.push(to_push));
        }

        out.render()
    }

    fn emit_block(&mut self, stmts: &[Expr]) -> String {
        if stmts.is_empty() {
            // Make sure there's always something on the stack
            return self.push("borgo.Unit".to_string());
        }

        let mut out = emitter();
        let mut stmts = stmts.to_vec();

        let last = stmts.pop().unwrap();

        let result = self.fresh_var() + "_block";
        out.emit(format!("var {result} any"));
        out.emit("{".to_string());

        self.scope.begin();

        stmts.iter().for_each(|e| {
            let value = self.emit_expr(e);
            out.emit(value);

            // always pop the value back
            let var = self.pop();
            out.emit(format!("_ = {var}"));
        });

        let value = self.emit_expr(&last);
        out.emit(value);

        // Get what's on the stack and assign it to the result then push that on the stack.
        // This whole dance is needed to keep variables in the correct scope.
        let var = self.pop();
        out.emit(format!("{result} = {var}"));
        out.emit("}".to_string());

        self.stack.push_front(result);
        self.scope.exit();

        out.render()
    }

    fn emit_match(&mut self, subject: &Expr, arms: &[Arm]) -> String {
        let mut out = emitter();

        let subject = self.emit_local(subject, &mut out);

        let result = self.fresh_var() + "_result";
        let subject_var = self.fresh_var() + "_subject";
        let pattern = self.fresh_var() + "_pattern";

        out.emit(format!(
            "var {result} any
{subject_var} := {subject}

for {pattern} := 0; {pattern} < {arms_len}; {pattern}++ {{",
            arms_len = arms.len()
        ));

        arms.iter().enumerate().for_each(|(index, arm)| {
            let if_statements = self.emit_pattern(&subject_var, &arm.pat);

            let new_expr = self.emit_expr(&arm.expr);
            let value = self.pop();

            out.emit(format!(
                "
if {pattern} == {index} {{
    {if_statements}
    _ = {subject_var}

    {new_expr}
    {result} = {value}
    break;
}}
"
            ))
        });

        // close for loop
        out.emit("}".to_string());

        self.stack.push_front(result);

        out.render()
    }

    fn emit_pattern(&mut self, subject: &str, pat: &Pat) -> String {
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
                    "if (!borgo.Ops.Eq({value}, {subject}).(bool)) {{ continue; }}"
                ));
            }

            Pat::Type { ident, .. } => {
                let var = self.fresh_var();

                out.emit(format!("{var} := {subject}"));
                self.scope.add_binding(ident.clone(), var);
            }

            Pat::Pat { ident, elems, .. } => {
                let pat = self.fresh_var() + "_pat";

                let new_elems = elems
                    .iter()
                    .enumerate()
                    .map(|(index, p)| {
                        let new_subject = format!("{pat}.Field{index}");
                        self.emit_pattern(&new_subject, p)
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                let name = to_name(ident);

                out.emit(format!(
                    "{pat}, ok := {subject}.({name})
_ = {pat}
if !ok {{ continue; }}"
                ));
                out.emit(new_elems);
            }

            Pat::Struct { fields, ty, .. } => {
                let ty = to_type(ty);

                fields.iter().for_each(|f| {
                    let field_name = to_struct_field(&f.name);
                    let cast = cast_to(&ty);
                    let pat = self.emit_pattern(&format!("{subject}{cast}.{field_name}"), &f.value);
                    out.emit(pat);
                });
            }

            Pat::Wild { .. } => out.emit("  // wildcard".to_string()),
            Pat::Unit { .. } => out.emit("  // unit".to_string()),
        };

        out.render()
    }

    fn emit_debug(&mut self, kind: &DebugKind, expr: &Expr) -> String {
        let mut out = emitter();

        let var = match kind {
            DebugKind::Unreachable => "".to_string(),
            DebugKind::Todo => "".to_string(),
            DebugKind::Inspect => self.emit_local(expr, &mut out),
        };

        let fn_name = format!("{:#?}", kind).to_lowercase();
        let new_value = self.push(format!("Debug_{fn_name}({var})"));

        out.emit(new_value);
        out.render()
    }

    fn emit_literal(&mut self, lit: &Literal) -> String {
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

        let var = self.push(value);
        out.emit(var);

        out.render()
    }

    fn emit_return(&mut self, expr: &Expr) -> String {
        let mut out = emitter();
        let value = self.emit_local(expr, &mut out);
        out.emit(format!("return {value}"));

        // Still need to push unit
        // TODO use sentinel value so that it's ignored. Same for Let bindings
        out.emit(self.push("borgo.Unit".to_string()));

        out.render()
    }

    fn emit_call(&mut self, func: &Expr, args: &[Expr], call_mode: CallMode) -> String {
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
                output.emit(self.push(call));
            }
            CallMode::Bare => {
                self.stack.push_front(call);
            }
        };

        output.render()
    }

    fn emit_var(&mut self, value: &str) -> String {
        let name = self
            .scope
            .get_binding(value)
            .unwrap_or_else(|| to_name(&self.gs.resolve_name(value)));

        let expr = self.make_functions.get(&name).cloned().unwrap_or(name);

        self.push(expr)
    }

    fn emit_let(&mut self, binding: &Binding, value: &Expr) -> String {
        let mut out = emitter();

        let new_value = self.emit_local(value, &mut out);

        match binding.pat {
            Pat::Type { ref ident, .. } => {
                let var = self.fresh_var();

                out.emit(format!("{var} := {new_value}"));
                self.scope.add_binding(ident.clone(), var);
            }

            Pat::Wild { .. } => {
                out.emit(format!("_ = {new_value}"));
            }

            _ => {
                let var = self.fresh_var();
                out.emit(format!("{var} := {new_value}"));

                let pat = self.emit_pattern(&var, &binding.pat);
                out.emit(pat);
            }
        };

        // always push unit so that there's something to return
        let unit = self.push("borgo.Unit".to_string());
        out.emit(unit);

        out.render()
    }

    fn emit_binary(&mut self, op: &Operator, left: &Expr, right: &Expr) -> String {
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

    fn emit_unary(&mut self, op: &UnOp, expr: &Expr) -> String {
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

    fn emit_if(&mut self, cond: &Expr, then: &Expr, els: &Expr) -> String {
        let mut out = emitter();

        let result = self.fresh_var() + "_result";

        let new_cond = self.emit_local(cond, &mut out);

        let new_then = self.emit_expr(then);
        let then_res = self.pop();

        let new_else = self.emit_expr(els);
        let else_res = self.pop();

        out.emit(format!(
            "var {result} any

if borgo.Ops.Eq({new_cond}, true).(bool) {{
    {new_then}
    {result} = {then_res}
}} else {{
    {new_else}
    {result} = {else_res}
}}",
        ));

        out.emit(self.push(result));
        out.render()
    }

    fn emit_struct_call(
        &mut self,
        name: &str,
        fields: &[StructField],
        rest: &Option<Expr>,
    ) -> String {
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

        let new_value = self.push(format!(
            "func (base any) any {{
    data := base.({ty})
    {new_fields}
    return data
}}({new_rest})"
        ));

        out.emit(new_value);
        out.render()
    }

    fn emit_struct_access(&mut self, expr: &Expr, field: &str) -> String {
        let mut out = emitter();
        let new_expr = self.emit_local(expr, &mut out);
        let cast = cast_to(&to_type(&expr.get_type()));
        let field = to_struct_field(field);
        let ret = self.push(format!("{new_expr}{cast}.{field}"));

        out.emit(ret);
        out.render()
    }

    fn emit_try(&mut self, expr: &Expr) -> String {
        let mut out = emitter();

        let new_expr = self.emit_local(expr, &mut out);

        let ret = self.fresh_var() + "_ret";
        let check = self.fresh_var() + "_check";

        out.emit(format!(
            "var {ret} any
{check} := {new_expr}

  if ret, ok := {check}.(Result_Ok); ok {{
    {ret} = ret.Field0
  }} else {{
    return {check}
  }}
"
        ));

        out.emit(self.push(ret));
        out.render()
    }

    fn emit_spawn(&mut self, expr: &Expr) -> String {
        let mut out = emitter();

        let call = match expr {
            Expr::Call { func, args, .. } => {
                let new_expr = self.emit_call(func, args, CallMode::Bare);
                out.emit(new_expr);
                self.pop()
            }

            _ => unreachable!("expected function call in spawn!()"),
        };

        out.emit(format!("go {call}"));

        out.emit(self.push("borgo.Unit".to_string()));

        out.render()
    }

    fn emit_select(&mut self, arms: &[Arm]) -> String {
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

            // ignore what's on the stack as we can't use it
            let var = self.emit_local(&a.expr, &mut out);
            out.emit(format!("_ = {var}"));
        });

        out.emit("}".to_string());

        out.emit(self.push("borgo.Unit".to_string()));

        out.render()
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

    fn emit_paren(&mut self, expr: &Expr) -> String {
        let mut out = emitter();
        let new_expr = self.emit_local(expr, &mut out);
        out.emit(self.push(format!("({new_expr})")));
        out.render()
    }

    fn pop(&mut self) -> String {
        self.stack.pop_front().unwrap()
    }

    // Push a value on the stack and return the relevant binding
    fn push(&mut self, value: String) -> String {
        let var = self.fresh_var();
        self.stack.push_front(var.clone());

        format!("{var} := {value}")
    }

    // Emits an expression, pop the value off the stack
    fn emit_local(&mut self, e: &Expr, out: &mut Emitter) -> String {
        let value = self.emit_expr(e);
        out.emit(value);

        self.pop()
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
