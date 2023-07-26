use crate::parse;
use crate::type_::{BoundedType, ModuleId, Symbol, Type};
use proc_macro2::{LineColumn as ProcLine, Span as ProcSpan};
use serde::{Deserialize, Serialize};
use syn;
use syn::spanned::Spanned;

pub type Ident = String;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Binding {
    pub pat: Pat, // contains span
    pub ann: TypeAst,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub name: Ident,
    pub generics: Vec<Generic>,
    pub args: Vec<Binding>,
    pub ret: Type,
    pub ann: TypeAst,
    pub body: Box<Expr>,
    pub bounded_ty: BoundedType,
}

impl Function {
    pub fn as_method(&self) -> Option<(Self, Type)> {
        if let Some(first) = self.args.first() {
            match first.pat {
                // if the first argument is `self` then this is a method
                Pat::Type { ref ident, .. } if ident == "self" => {
                    let mut new_func = self.clone();
                    let receiver = new_func.args.remove(0).ty;
                    return Some((new_func, receiver));
                }
                _ => (),
            }
        }

        None
    }

    pub fn is_external(&self) -> bool {
        if let Expr::Block { ref stmts, .. } = *self.body {
            if let Some(Expr::Var { value, .. }) = stmts.first() {
                if value == "EXT" {
                    return true;
                }
            }
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pat {
    Type {
        ident: String,
        is_mut: bool,
        ann: TypeAst,
        span: Span,
    },
    Lit {
        lit: Literal,
        ty: Type,
        span: Span,
    },
    Pat {
        ident: String,
        elems: Vec<Self>,
        ty: Type,
        span: Span,
    },
    Struct {
        ident: String,
        fields: Vec<StructFieldPat>,
        ty: Type,
        span: Span,
    },
    Wild {
        span: Span,
    },
    Unit {
        ty: Type,
        span: Span,
    },
}

impl Pat {
    fn parse_ident(p: syn::Pat) -> Result<String> {
        match p {
            syn::Pat::Ident(i) => Ok(i.ident.to_string()),
            _ => panic!("was expecting pat ident"),
        }
    }

    pub fn parse_path(path: syn::Path) -> String {
        path.segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn get_span(&self) -> Span {
        match self {
            Pat::Type { span, .. } => span.clone(),
            Pat::Lit { span, .. } => span.clone(),
            Pat::Pat { span, .. } => span.clone(),
            Pat::Struct { span, .. } => span.clone(),
            Pat::Wild { span } => span.clone(),
            Pat::Unit { span, .. } => span.clone(),
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Pat::Type { .. } => None,
            Pat::Lit { ty, .. } => Some(ty.clone()),
            Pat::Pat { ty, .. } => Some(ty.clone()),
            Pat::Struct { ty, .. } => Some(ty.clone()),
            Pat::Wild { .. } => None,
            Pat::Unit { ty, .. } => Some(ty.clone()),
        }
    }

    fn find_at_position(&self, target: &Span) -> Option<Expr> {
        // Nasty hack, build a dummy expr so that the type can be shown
        let make_expr = |ty: Option<Type>, span: Span| Expr::Var {
            value: "not really a var, used in match pat".to_string(),
            decl: Span::dummy(),
            generics_instantiated: vec![],
            ty: ty.unwrap_or_else(Type::discard),
            span,
        };

        match self {
            Pat::Type { span, .. } => {
                if span.contains(target) {
                    Some(make_expr(None, span.clone()))
                } else {
                    None
                }
            }

            Pat::Lit { ty, span, .. } => {
                if span.contains(target) {
                    Some(make_expr(Some(ty.clone()), span.clone()))
                } else {
                    None
                }
            }

            Pat::Pat {
                elems, ty, span, ..
            } => {
                if span.contains(target) {
                    Some(make_expr(Some(ty.clone()), span.clone()))
                } else {
                    elems.iter().find_map(|p| p.find_at_position(target))
                }
            }

            Pat::Struct {
                fields, ty, span, ..
            } => {
                if span.contains(target) {
                    Some(make_expr(Some(ty.clone()), span.clone()))
                } else {
                    fields.iter().find_map(|f| f.value.find_at_position(target))
                }
            }

            Pat::Wild { span } => {
                if span.contains(target) {
                    Some(make_expr(None, span.clone()))
                } else {
                    None
                }
            }

            Pat::Unit { span, .. } => {
                if span.contains(target) {
                    Some(make_expr(None, span.clone()))
                } else {
                    None
                }
            }
        }
    }

    pub fn parse_mutability(pat: &syn::Pat) -> bool {
        match &pat {
            syn::Pat::Ident(i) => i.mutability.is_some(),
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct StructFieldPat {
    pub name: Ident,
    pub value: Pat,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Arm {
    pub pat: Pat,
    pub expr: Expr,
}

// struc access ie. struct.field
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct StructField {
    pub name: Ident,
    pub value: Expr,
    // TODO add span
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumDefinition {
    pub name: Ident,
    pub generics: Vec<Generic>,
    pub cons: Vec<Constructor>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Constructor {
    pub name: String,
    // pub fields: Vec<Binding>, Named fields, skip for now
    // pub fields: Vec<Type>,
    pub fields: Vec<EnumFieldDef>,
}

impl Constructor {
    pub fn to_qualified(&self, enum_name: &str) -> String {
        format!("{}::{}", enum_name, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumFieldDef {
    pub name: Ident,
    pub ann: TypeAst,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructDefinition {
    pub name: Ident,
    pub generics: Vec<Generic>,
    pub fields: Vec<StructFieldDef>,
}
impl StructDefinition {
    pub fn to_newtype_def(&self) -> NewtypeDefinition {
        let fields = self
            .fields
            .iter()
            .map(|f| EnumFieldDef {
                name: f.name.to_string(),
                ann: f.ann.clone(),
                ty: f.ty.ty.clone(),
            })
            .collect();

        NewtypeDefinition {
            name: self.name.to_string(),
            generics: self.generics.to_vec(),
            fields,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructFieldDef {
    pub name: Ident,
    pub ann: TypeAst,
    pub ty: BoundedType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeAliasDef {
    pub name: String,
    pub generics: Vec<Generic>,
    pub ann: TypeAst,
    pub ty: BoundedType,
    pub attrs: Vec<String>,
}

impl TypeAliasDef {
    pub fn is_native(&self) -> bool {
        self.ann == TypeAst::ext()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NewtypeDefinition {
    pub name: Ident,
    pub generics: Vec<Generic>,
    pub fields: Vec<EnumFieldDef>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PkgImport {
    pub name: String,
    // pub rename: String
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopFlow {
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Loop {
    NoCondition,
    WithCondition { binding: Binding, expr: Box<Expr> },
    While { expr: Box<Expr> },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
    pub file: FileId,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LineColumn {
    pub line: usize,
    pub col: usize,
}

impl LineColumn {
    pub fn make(l: ProcLine) -> Self {
        Self {
            line: l.line,
            col: l.column,
        }
    }
}

impl Span {
    pub fn make(s: ProcSpan, file: &FileId) -> Self {
        Self {
            start: LineColumn::make(s.start()),
            end: LineColumn::make(s.end()),
            file: file.clone(),
        }
    }

    pub fn dummy() -> Self {
        let line = LineColumn { line: 0, col: 0 };

        Self {
            start: line.clone(),
            end: line,
            file: FileId::dummy(),
        }
    }

    pub fn contains(&self, target: &Span) -> bool {
        if self.start.line > target.start.line {
            return false;
        }

        if self.end.line < target.end.line {
            return false;
        }

        if self.start.line == target.start.line && self.start.col > target.start.col {
            return false;
        }

        if self.end.line == target.end.line && self.end.col < target.end.col {
            return false;
        }

        true
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Generic {
    pub name: String,
    pub bounds: Vec<TypeAst>,
    pub span: Span,
}

impl Generic {
    pub fn to_type(&self, module: &ModuleId) -> Type {
        Type::Con {
            id: Symbol {
                name: self.name.clone(),
                module: module.clone(),
                span: self.span.clone(),
            },
            args: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DebugKind {
    Todo,
    Unreachable,
    Inspect,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FunctionKind {
    TopLevel, // top level fn ()
    Inline,   // fn () nested in function
    Lambda,   // || {}
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Literal {
        lit: Literal,
        ty: Type,
        span: Span,
    },
    Closure {
        fun: Function,
        kind: FunctionKind,
        ty: Type,
        span: Span,
    },
    Block {
        stmts: Vec<Expr>,
        ty: Type,
        span: Span,
    },
    Let {
        binding: Binding,
        value: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Var {
        value: Ident,
        // TODO asdf should this go? Track spans in Scope::spans?
        decl: Span,
        generics_instantiated: Vec<Type>,
        ty: Type,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Type,
        span: Span,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
        ty: Type,
        span: Span,
    },
    CheckType {
        expr: Box<Expr>,
        ann: TypeAst,
        ty: Type,
        span: Span,
    },
    Match {
        subject: Box<Expr>,
        arms: Vec<Arm>,
        ty: Type,
        span: Span,
    },
    Tuple {
        elems: Vec<Expr>,
        ty: Type,
        span: Span,
    },
    EnumDef {
        def: EnumDefinition,
        span: Span,
    },
    StructDef {
        def: StructDefinition,
        span: Span,
    },
    StructCall {
        name: Ident,
        fields: Vec<StructField>,
        rest: Box<Option<Expr>>,
        ty: Type,
        span: Span,
    },
    FieldAccess {
        expr: Box<Expr>,
        field: Ident,
        ty: Type,
        span: Span,
    },
    VarUpdate {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    MethodCall {
        target: Box<Expr>,
        method: Ident,
        args: Vec<Expr>,
        ty: Type,
        span: Span,
    },
    Return {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Try {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    ImplBlock {
        ann: TypeAst,
        ty: Type,
        items: Vec<Expr>,
        generics: Vec<Generic>,
        span: Span,
    },
    Binary {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Paren {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Const {
        ident: Ident,
        expr: Box<Expr>,
        ann: TypeAst,
        ty: Type,
        span: Span,
    },
    Debug {
        kind: DebugKind,
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Spawn {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Select {
        arms: Vec<Arm>,
        ty: Type, // TODO remove this
        span: Span,
    },
    Defer {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Raw {
        text: String,
    },
    Loop {
        kind: Loop,
        body: Box<Expr>,
        span: Span,
    },
    Flow {
        kind: LoopFlow,
        span: Span,
    },
    TypeAlias {
        def: TypeAliasDef,
        span: Span,
    },
    NewtypeDef {
        def: NewtypeDefinition,
        span: Span,
    },
    UsePackage {
        import: PkgImport,
        span: Span,
    },
    Reference {
        expr: Box<Expr>,
        mutable: bool,
        ty: Type,
        span: Span,
    },
    Trait {
        name: String,
        supertraits: Vec<String>,
        items: Vec<Expr>,
        types: Vec<String>,
        span: Span,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
        ty: Type,
        span: Span,
    },

    Unit {
        ty: Type,
        span: Span,
    },
    Noop,
    Todo,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String, Option<String> /* token like r"asdf" */),
    Char(char),
    Slice(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Gt,
    Ge,
    Rem,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnOp {
    Neg,
    Not,
    Deref,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File {
    pub id: FileId,
    pub name: String,
    pub source: String,
    pub decls: Vec<Expr>,
}

impl File {
    pub fn find_expr_at_position(&self, span: &Span) -> Option<Expr> {
        self.decls.iter().find_map(|e| e.find_at_position(span))
    }

    pub fn go_filename(&self) -> String {
        std::path::Path::new(&self.name)
            .with_extension("go")
            .display()
            .to_string()
    }

    pub fn imports(&self) -> Vec<PkgImport> {
        self.decls
            .iter()
            .filter_map(|e| {
                if let Expr::UsePackage { import, .. } = e {
                    Some(import.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

pub struct Ast {
    current_file: FileId,
}

impl Ast {
    pub fn from_expr(&self, input: syn::Expr) -> Result<Expr> {
        let root_span = self.make_span(input.span());

        match input {
            syn::Expr::Lit(lit) => match lit.lit {
                syn::Lit::Int(i) => {
                    let n = i.base10_parse().unwrap();
                    Ok(Expr::Literal {
                        lit: Literal::Int(n),
                        ty: Type::dummy(),
                        span: root_span,
                    })
                }
                syn::Lit::Float(i) => {
                    let n = i.base10_parse().unwrap();
                    Ok(Expr::Literal {
                        lit: Literal::Float(n),
                        ty: Type::dummy(),
                        span: root_span,
                    })
                }
                syn::Lit::Bool(b) => Ok(Expr::Literal {
                    lit: Literal::Bool(b.value),
                    ty: Type::dummy(),
                    span: root_span,
                }),
                syn::Lit::Str(s) => {
                    let token = if s.token().to_string().starts_with('r') {
                        Some("r".to_string())
                    } else {
                        None
                    };

                    Ok(Expr::Literal {
                        lit: Literal::String(s.value(), token),
                        ty: Type::dummy(),
                        span: root_span,
                    })
                }
                syn::Lit::Char(c) => Ok(Expr::Literal {
                    lit: Literal::Char(c.value()),
                    ty: Type::dummy(),
                    span: root_span,
                }),
                _ => todo!("uncovered literal {:#?}", lit),
            },

            syn::Expr::Block(syn::ExprBlock { block, .. }) => {
                let stmts = block
                    .stmts
                    .into_iter()
                    .map(|e| self.from_statement(e))
                    .collect::<Result<_>>()?;

                Ok(Expr::Block {
                    stmts,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Path(path) => {
                let value = parse::parse_ident_from_path(path);
                Ok(Expr::Var {
                    value,
                    decl: Span::dummy(),
                    generics_instantiated: vec![],
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Call(call) => {
                let func = self.from_expr(*call.func)?;
                let args = call
                    .args
                    .into_iter()
                    .map(|e| self.from_expr(e))
                    .collect::<Result<_>>()?;

                Ok(Expr::Call {
                    func: func.into(),
                    args,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Array(arr) => {
                let elems = arr
                    .elems
                    .into_iter()
                    .map(|e| self.from_expr(e))
                    .collect::<Result<_>>()?;

                Ok(Expr::Literal {
                    lit: Literal::Slice(elems),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Cast(c) => {
                let expr = self.from_expr(*c.expr)?;
                let ann = parse::type_from_expr(*c.ty);

                Ok(Expr::CheckType {
                    expr: expr.into(),
                    ann,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Closure(c) => {
                let args = c
                    .inputs
                    .into_iter()
                    .map(|e| self.from_pat(e))
                    .collect::<Result<_>>()?;

                let body = self.from_expr(*c.body)?;
                let ann = parse::parse_output(c.output, TypeAst::Unknown);

                let fun = Function {
                    name: "__anonymous".into(),
                    generics: vec![],
                    args,
                    body: body.into(),
                    ann,
                    ret: Type::dummy(),
                    bounded_ty: Type::dummy().to_bounded(),
                };

                Ok(Expr::Closure {
                    fun,
                    kind: FunctionKind::Lambda,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::If(i) => {
                let cond = self.from_expr(*i.cond)?;
                let then = self.from_block(i.then_branch)?;
                let els = i
                    .else_branch
                    .map(|(_, block)| self.from_expr(*block))
                    .unwrap_or(Ok(Expr::Block {
                        stmts: vec![],
                        ty: Type::dummy(),
                        span: root_span.clone(),
                    }))?;

                Ok(Expr::If {
                    cond: cond.into(),
                    then: then.into(),
                    els: els.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Match(m) => {
                let subject = self.from_expr(*m.expr)?;
                let arms = m
                    .arms
                    .into_iter()
                    .map(|e| self.parse_arm(e))
                    .collect::<Result<_>>()?;

                Ok(Expr::Match {
                    subject: subject.into(),
                    arms,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Tuple(t) => {
                if t.elems.is_empty() {
                    return Ok(Expr::Unit {
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                let elems = t
                    .elems
                    .into_iter()
                    .map(|e| self.from_expr(e))
                    .collect::<Result<_>>()?;

                Ok(Expr::Tuple {
                    elems,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Struct(s) => {
                let name = Pat::parse_path(s.path);
                let fields = s
                    .fields
                    .into_iter()
                    .map(|f| {
                        Ok(StructField {
                            name: parse::parse_member(f.member)?,
                            value: self.from_expr(f.expr)?,
                        })
                    })
                    .collect::<Result<_>>()?;

                let rest = s.rest.map(|e| self.from_expr(*e)).transpose()?;

                Ok(Expr::StructCall {
                    name,
                    fields,
                    rest: rest.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Field(f) => {
                let expr = self.from_expr(*f.base)?;
                let field = parse::parse_member(f.member)?;

                Ok(Expr::FieldAccess {
                    expr: expr.into(),
                    field,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Assign(a) => {
                let target = self.from_expr(*a.left)?;
                let value = self.from_expr(*a.right)?;

                Ok(Expr::VarUpdate {
                    target: target.into(),
                    value: value.into(),
                    span: root_span,
                })
            }

            syn::Expr::MethodCall(m) => {
                let target = self.from_expr(*m.receiver)?;
                let method = m.method.to_string();
                let args = m
                    .args
                    .into_iter()
                    .map(|e| self.from_expr(e))
                    .collect::<Result<_>>()?;

                let span = self.make_span(m.method.span());

                let func = Expr::FieldAccess {
                    expr: target.into(),
                    field: method,
                    ty: Type::dummy(),
                    span: span.clone(),
                };

                Ok(Expr::Call {
                    func: func.into(),
                    args,
                    ty: Type::dummy(),
                    span,
                })

                /*
                Ok(Expr::MethodCall {
                    target: target.into(),
                    method,
                    args,
                    ty: Type::dummy(),
                    span,
                })
                */
            }

            syn::Expr::Return(r) => {
                let expr = r
                    .expr
                    .map(|e| self.from_expr(*e))
                    .transpose()?
                    .unwrap_or(Expr::Noop);

                Ok(Expr::Return {
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Macro(m) => {
                let name = Pat::parse_path(m.mac.path.clone());
                let expr = m
                    .mac
                    .parse_body::<syn::Expr>()
                    .ok()
                    .map(|e| self.from_expr(e))
                    .unwrap_or_else(|| Ok(Expr::Noop))?;

                if name == "todo" {
                    return Ok(Expr::Debug {
                        kind: DebugKind::Todo,
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "unreachable" {
                    return Ok(Expr::Debug {
                        kind: DebugKind::Unreachable,
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "spawn" {
                    return Ok(Expr::Spawn {
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "select" {
                    return Ok(Expr::Select {
                        arms: vec![], // will be populated in infer
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "defer" {
                    return Ok(Expr::Defer {
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "rawgo" {
                    let text = match expr {
                        Expr::Literal { lit, .. } => match lit {
                            Literal::String(s, _) => s,
                            _ => panic!("was expecting string in rawgo block"),
                        },
                        _ => panic!("was expecting string in rawgo block"),
                    };
                    return Ok(Expr::Raw { text });
                }

                panic!("Unknown macro {}", name);
            }

            syn::Expr::Try(t) => {
                let expr = self.from_expr(*t.expr)?;
                Ok(Expr::Try {
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Binary(b) => {
                let op = parse::parse_operator(b.op)?;
                let left = self.from_expr(*b.left)?;
                let right = self.from_expr(*b.right)?;

                Ok(Expr::Binary {
                    op,
                    left: left.into(),
                    right: right.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Paren(p) => {
                let expr = self.from_expr(*p.expr)?;

                Ok(Expr::Paren {
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Unary(u) => {
                let op = parse::parse_unop(u.op)?;
                let expr = self.from_expr(*u.expr)?;

                Ok(Expr::Unary {
                    op,
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::ForLoop(f) => {
                let binding = self.from_pat(f.pat)?;
                let expr = self.from_expr(*f.expr)?;
                let body = self.from_block(f.body)?;

                Ok(Expr::Loop {
                    kind: Loop::WithCondition {
                        binding,
                        expr: Box::new(expr),
                    },
                    body: Box::new(body),
                    span: root_span,
                })
            }

            syn::Expr::Loop(l) => {
                let body = self.from_block(l.body)?;

                Ok(Expr::Loop {
                    kind: Loop::NoCondition,
                    body: Box::new(body),
                    span: root_span,
                })
            }

            syn::Expr::While(w) => {
                let expr = self.from_expr(*w.cond)?;
                let body = self.from_block(w.body)?;

                Ok(Expr::Loop {
                    kind: Loop::While {
                        expr: Box::new(expr),
                    },
                    body: Box::new(body),
                    span: root_span,
                })
            }

            syn::Expr::Break(_) => Ok(Expr::Flow {
                kind: LoopFlow::Break,
                span: root_span,
            }),

            syn::Expr::Continue(_) => Ok(Expr::Flow {
                kind: LoopFlow::Continue,
                span: root_span,
            }),

            syn::Expr::Reference(r) => {
                let expr = self.from_expr(*r.expr)?;

                Ok(Expr::Reference {
                    expr: expr.into(),
                    mutable: r.mutability.is_some(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Index(i) => {
                let expr = self.from_expr(*i.expr)?;
                let index = self.from_expr(*i.index)?;

                Ok(Expr::Index {
                    expr: expr.into(),
                    index: index.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            _ => todo!("uncovered expr {:#?}", input),
        }
    }

    pub fn from_item(&self, item: syn::Item, kind: FunctionKind) -> Result<Expr> {
        let span = self.make_span(item.span());

        match item {
            syn::Item::Fn(fun) => Ok(Expr::Closure {
                fun: self.parse_item_fn(fun)?,
                ty: Type::dummy(),
                kind,
                span,
            }),

            syn::Item::Enum(e) => {
                let generics = self.parse_generics(&e.generics);
                let cons = e
                    .variants
                    .into_iter()
                    .map(|v| {
                        let fields = parse::parse_fields(v.fields);

                        let fields = match fields {
                            parse::Fields::TupleCons(fields) => fields,
                            _ => panic!("unexpected enum fields"),
                        };

                        Ok(Constructor {
                            name: v.ident.to_string(),
                            // fields: parse::parse_fields(v.fields),
                            fields,
                        })
                    })
                    .collect::<Result<_>>()?;

                let def = EnumDefinition {
                    name: e.ident.to_string(),
                    generics,
                    cons,
                };

                Ok(Expr::EnumDef { def, span })
            }

            syn::Item::Struct(e) => {
                let generics = self.parse_generics(&e.generics);
                let fields = parse::parse_fields(e.fields);

                match fields {
                    parse::Fields::StructFields(fields) => {
                        let def = StructDefinition {
                            name: e.ident.to_string(),
                            generics,
                            fields,
                        };

                        Ok(Expr::StructDef { def, span })
                    }

                    parse::Fields::TupleCons(fields) => {
                        let def = NewtypeDefinition {
                            name: e.ident.to_string(),
                            generics,
                            fields,
                        };

                        Ok(Expr::NewtypeDef { def, span })
                    }
                }
            }

            syn::Item::Impl(i) => {
                let base_ann = parse::type_from_expr(*i.self_ty.clone());
                let impl_generics = self.parse_generics(&i.generics);

                let items = i
                    .items
                    .into_iter()
                    .map(|item| match item {
                        syn::ImplItem::Method(fun) => {
                            let root_span = self.make_span(fun.span());

                            let mut sig = self.parse_signature(
                                fun.sig.clone(),
                                Some((base_ann.clone(), span.clone())),
                            )?;

                            // add generics to signature
                            for g in impl_generics.iter() {
                                if !sig.generics.contains(g) {
                                    sig.generics.push(g.clone());
                                }
                            }

                            // TODO all this stuff is the same as parse_item_fn, refactor
                            let span = fun.block.span();
                            let stmts = fun
                                .block
                                .stmts
                                .into_iter()
                                .map(|e| self.from_statement(e))
                                .collect::<Result<_>>()?;

                            let body = Expr::Block {
                                stmts,
                                ty: Type::dummy(),
                                span: self.make_span(span),
                            };

                            let name = fun.sig.ident.to_string();

                            let mut new_fun = Function {
                                name,
                                generics: sig.generics,
                                args: sig.args,
                                ann: sig.ret,
                                ret: Type::dummy(),
                                body: body.into(),
                                bounded_ty: Type::dummy().to_bounded(),
                            };

                            // If it's a static method, then update the function name to be
                            // qualified
                            if new_fun.as_method().is_none() {
                                new_fun.name = format!(
                                    "{base}::{name}",
                                    base = base_ann.get_name().unwrap(),
                                    name = new_fun.name
                                );
                            }

                            Ok(Expr::Closure {
                                fun: new_fun,
                                kind: FunctionKind::TopLevel,
                                ty: Type::dummy(),
                                span: root_span,
                            })
                        }
                        _ => panic!("can only have methods in impl blocks"),
                    })
                    .collect::<Result<_>>()?;

                let span = self.make_span(i.self_ty.span());

                Ok(Expr::ImplBlock {
                    ann: base_ann,
                    ty: Type::dummy(),
                    generics: impl_generics,
                    items,
                    span,
                })
            }

            syn::Item::Const(c) => {
                let span = self.make_span(c.span());
                let expr = self.from_expr(*c.expr)?;
                let ann = parse::type_from_expr(*c.ty);

                Ok(Expr::Const {
                    ident: c.ident.to_string(),
                    expr: expr.into(),
                    ann,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Item::Macro(m) => {
                // TODO this is duplicated with Expr::Macro

                let name = Pat::parse_path(m.mac.path.clone());
                let expr = m
                    .mac
                    .parse_body::<syn::Expr>()
                    .ok()
                    .map(|e| self.from_expr(e))
                    .unwrap_or_else(|| Ok(Expr::Noop))?;

                if name == "rawgo" {
                    let text = match expr {
                        Expr::Literal { lit, .. } => match lit {
                            Literal::String(s, _) => s,
                            _ => panic!("was expecting string in rawgo block"),
                        },
                        _ => panic!("was expecting string in rawgo block"),
                    };
                    return Ok(Expr::Raw { text });
                }

                panic!("Unknown macro {}", name);
            }

            syn::Item::Type(t) => {
                let span = self.make_span(t.span());
                let ann = parse::type_from_expr(*t.ty);
                let attrs = parse::parse_attrs(&t.attrs);
                let generics = self.parse_generics(&t.generics);

                let def = TypeAliasDef {
                    name: t.ident.to_string(),
                    generics,
                    ann,
                    attrs,
                    ty: BoundedType {
                        generics: vec![],
                        ty: Type::dummy(),
                    },
                };

                Ok(Expr::TypeAlias { def, span })
            }

            syn::Item::Use(u) => {
                let span = self.make_span(u.span());
                let name = parse::parse_use(u.tree);
                let import = PkgImport {
                    name,
                    span: span.clone(),
                };
                Ok(Expr::UsePackage { import, span })
            }

            syn::Item::Trait(t) => {
                let mut items: Vec<Expr> = vec![];
                let mut types: Vec<String> = vec![];

                for i in t.items.iter() {
                    match i {
                        syn::TraitItem::Method(fun) => {
                            let sig = self.parse_signature(fun.sig.clone(), None)?;

                            let fun_name = fun.sig.ident.to_string();

                            let new_func = Function {
                                name: fun_name,
                                args: sig.args,
                                generics: sig.generics,
                                ret: Type::dummy(),
                                ann: sig.ret,
                                body: Expr::Noop.into(),
                                bounded_ty: Type::dummy().to_bounded(),
                            };

                            let span = self.make_span(i.span());

                            items.push(Expr::Closure {
                                fun: new_func,
                                kind: FunctionKind::Inline,
                                ty: Type::dummy(),
                                span,
                            });
                        }

                        syn::TraitItem::Type(t) => {
                            types.push(t.ident.to_string());
                        }

                        _ => panic!("trait item {:#?}", i),
                    }
                }

                let supertraits = t
                    .supertraits
                    .into_iter()
                    .map(|s| match s {
                        syn::TypeParamBound::Trait(s) => Pat::parse_path(s.path),
                        syn::TypeParamBound::Lifetime(_) => panic!("don't be ridicolous"),
                    })
                    .collect();

                Ok(Expr::Trait {
                    name: t.ident.to_string(),
                    supertraits,
                    items,
                    types,
                    span,
                })
            }

            _ => panic!("unimplemented from_item {:?}", item),
        }
    }

    pub fn from_statement(&self, stmt: syn::Stmt) -> Result<Expr> {
        match stmt {
            syn::Stmt::Item(item) => self.from_item(item, FunctionKind::Inline),

            syn::Stmt::Local(syn::Local {
                ref init, ref pat, ..
            }) => {
                let binding = self.from_pat(pat.clone())?;
                let value = init
                    .clone()
                    .map(|(_, e)| self.from_expr(*e))
                    .unwrap_or_else(|| Ok(Expr::Noop))?;

                Ok(Expr::Let {
                    binding,
                    value: Box::new(value),
                    ty: Type::dummy(),
                    span: self.make_span(stmt.span()),
                })
            }

            syn::Stmt::Expr(e) => self.from_expr(e),

            syn::Stmt::Semi(e, _) => self.from_expr(e),
        }
    }

    pub fn from_block(&self, block: syn::Block) -> Result<Expr> {
        let span = self.make_span(block.span());
        let stmts = block
            .stmts
            .into_iter()
            .map(|e| self.from_statement(e))
            .collect::<Result<_>>()?;

        Ok(Expr::Block {
            stmts,
            ty: Type::dummy(),
            span,
        })
    }

    pub fn from_file(&self, name: String, source: String, input: syn::File) -> Result<File> {
        let decls = input
            .items
            .into_iter()
            .map(|e| self.from_item(e, FunctionKind::TopLevel))
            .collect::<Result<Vec<_>>>()?;

        Ok(File {
            id: FileId::dummy(),
            name,
            source,
            decls,
        })
    }

    pub fn from_file_contents(&self, contents: &str) -> Result<Vec<Expr>> {
        let file = syn::parse_str::<syn::File>(contents).map_err(|err| ParseError {
            msg: err.to_string(),
            span: self.make_span(err.span()),
        })?;

        file.items
            .into_iter()
            .map(|e| self.from_item(e, FunctionKind::TopLevel))
            .collect::<Result<Vec<_>>>()
    }

    pub fn from_pat_expr(&self, p: syn::Pat) -> Result<Pat> {
        // TODO fix this
        // Add Pat::Ident for new variables
        // Pat::Path always refers to ctors
        // Basically like TupleStruct but with no args
        let span = self.make_span(p.span());

        match p {
            syn::Pat::Type(ty) => Ok(Pat::Type {
                ident: Pat::parse_ident(*ty.pat.clone())?,
                is_mut: Pat::parse_mutability(&ty.pat),
                ann: parse::type_from_expr(*ty.ty),
                span,
            }),

            syn::Pat::Ident(_) => {
                let ident = Pat::parse_ident(p.clone())?;

                // syn cannot distinguish between Foo (constructor) and foo (binding).
                // So let's use a dumb heuristic:
                //   - if the first letter is uppercase it's a ctor
                //   - otherwise it's a binding

                let first = ident.chars().next().unwrap();
                if first.is_uppercase() {
                    return Ok(Pat::Pat {
                        ident,
                        elems: vec![],
                        ty: Type::dummy(),
                        span,
                    });
                }

                Ok(Pat::Type {
                    ident,
                    is_mut: Pat::parse_mutability(&p),
                    ann: TypeAst::Unknown,
                    span,
                })
            }

            syn::Pat::Lit(l) => {
                let expr = self.from_expr(*l.expr)?;
                match expr {
                    Expr::Literal { lit, ty, span } => Ok(Pat::Lit { lit, ty, span }),
                    _ => panic!("Pat::Lit should parse to a Literal"),
                }
            }

            syn::Pat::Path(p) => {
                let ident = Pat::parse_path(p.path);

                Ok(Pat::Pat {
                    ident,
                    elems: vec![],
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::TupleStruct(t) => {
                let ident = Pat::parse_path(t.path);
                let elems = t
                    .pat
                    .elems
                    .into_iter()
                    .map(|e| self.from_pat_expr(e))
                    .collect::<Result<_>>()?;

                Ok(Pat::Pat {
                    ident,
                    elems,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::Struct(t) => {
                let ident = Pat::parse_path(t.path);
                let fields = t
                    .fields
                    .into_iter()
                    .map(|p| {
                        Ok(StructFieldPat {
                            name: parse::parse_member(p.member)?,
                            value: self.from_pat_expr(*p.pat)?,
                        })
                    })
                    .collect::<Result<_>>()?;

                Ok(Pat::Struct {
                    ident,
                    fields,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::Tuple(t) => {
                if t.elems.is_empty() {
                    return Ok(Pat::Unit {
                        ty: Type::dummy(),
                        span,
                    });
                }

                let name = format!("Tuple{}", t.elems.len());
                let fields = t
                    .elems
                    .into_iter()
                    .enumerate()
                    .map(|(index, p)| {
                        Ok(StructFieldPat {
                            name: Expr::tuple_index_string(index.try_into().unwrap())?,
                            value: self.from_pat_expr(p)?,
                        })
                    })
                    .collect::<Result<_>>()?;

                Ok(Pat::Struct {
                    ident: name,
                    fields,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::Slice(e) => Err(ParseError {
                msg: "Can't pattern match on slice literals".to_string(),
                span: self.make_span(e.span()),
            }),

            syn::Pat::Wild(_) => Ok(Pat::Wild { span }),

            _ => panic!("unexpected pat expression {:#?}", &p),
        }
    }

    pub fn from_pat(&self, pat: syn::Pat) -> Result<Binding> {
        let pat = self.from_pat_expr(pat)?;

        let ann = match pat {
            Pat::Type { ref ann, .. } => ann.clone(),
            _ => TypeAst::Unknown,
        };

        Ok(Binding {
            pat,
            ann,
            ty: Type::dummy(),
        })
    }

    fn parse_arm(&self, input: syn::Arm) -> Result<Arm> {
        let pat = self.from_pat_expr(input.pat)?;
        let expr = self.from_expr(*input.body)?;
        Ok(Arm { pat, expr })
    }

    fn parse_item_fn(&self, fun: syn::ItemFn) -> Result<Function> {
        let sig = self.parse_signature(fun.sig.clone(), None)?;

        let span = (*fun.block).span();
        let stmts = fun
            .block
            .stmts
            .into_iter()
            .map(|e| self.from_statement(e))
            .collect::<Result<_>>()?;

        let body = Expr::Block {
            stmts,
            ty: Type::dummy(),
            span: self.make_span(span),
        };

        Ok(Function {
            name: fun.sig.ident.to_string(),
            generics: sig.generics,
            args: sig.args,
            ann: sig.ret,
            ret: Type::dummy(),
            body: body.into(),
            bounded_ty: Type::dummy().to_bounded(),
        })
    }

    pub fn parse_input(
        &self,
        input: syn::FnArg,
        receiver: Option<(TypeAst, Span)>,
    ) -> Result<Binding> {
        match input {
            syn::FnArg::Typed(arg) => {
                let pat = self.from_pat_expr(*arg.pat)?;
                let ann = parse::type_from_expr(*arg.ty);

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

    pub fn parse_signature(
        &self,
        sig: syn::Signature,
        receiver: Option<(TypeAst, Span)>,
    ) -> Result<Signature> {
        let generics = self.parse_generics(&sig.generics);
        let args = sig
            .inputs
            .into_iter()
            .map(|i| self.parse_input(i, receiver.clone()))
            .collect::<Result<_>>()?;

        let ret = parse::parse_output(sig.output, TypeAst::unit());
        Ok(Signature {
            generics,
            args,
            ret,
        })
    }

    pub fn parse_generics(&self, generics: &syn::Generics) -> Vec<Generic> {
        generics
            .type_params()
            .map(|x| {
                let name = x.ident.to_string();

                let bounds = x
                    .bounds
                    .iter()
                    .map(|b| match b {
                        syn::TypeParamBound::Trait(b) => parse::type_from_path(&b.path),
                        syn::TypeParamBound::Lifetime(_) => panic!("we don't need no lifetimes"),
                    })
                    .collect();

                let span = self.make_span(x.span());
                Generic { name, bounds, span }
            })
            .collect()
    }

    pub fn new() -> Self {
        Ast {
            current_file: FileId::dummy(),
        }
    }

    pub fn make_span(&self, s: ProcSpan) -> Span {
        Span::make(s, &self.current_file)
    }

    pub fn set_file(&mut self, file: &FileId) {
        self.current_file = file.clone();
    }
}

pub struct Signature {
    pub generics: Vec<Generic>,
    pub args: Vec<Binding>,
    pub ret: TypeAst,
}

impl Expr {
    /// Only used in tests
    pub fn from_source(source: &str) -> syn::Expr {
        syn::parse_str::<syn::Expr>(source).unwrap()
    }

    pub fn tuple_index_string(index: u32) -> Result<String> {
        match index {
            0 => Ok("first".to_string()),
            1 => Ok("second".to_string()),
            2 => Ok("third".to_string()),
            3 => Ok("fourth".to_string()),
            4 => Ok("fifth".to_string()),
            n => panic!("unsupported tuple index {}", n),
        }
    }

    pub fn find_at_position(&self, target: &Span) -> Option<Self> {
        let res = match self {
            Self::Closure { fun, .. } => fun.body.find_at_position(target),

            Self::Block { stmts, .. } => stmts.iter().find_map(|e| e.find_at_position(target)),

            Self::Let { value, .. } => value.find_at_position(target),

            Self::Call { func, args, .. } => func
                .find_at_position(target)
                .or_else(|| args.iter().find_map(|a| a.find_at_position(target))),

            Self::If {
                cond, then, els, ..
            } => cond
                .find_at_position(target)
                .or_else(|| then.find_at_position(target))
                .or_else(|| els.find_at_position(target)),

            Self::Match { subject, arms, .. } => subject.find_at_position(target).or_else(|| {
                arms.iter().find_map(|a| {
                    a.pat
                        .find_at_position(target)
                        .or_else(|| a.expr.find_at_position(target))
                })
            }),

            Self::Tuple { elems, .. } => elems.iter().find_map(|e| e.find_at_position(target)),

            Self::Binary { left, right, .. } => left
                .find_at_position(target)
                .or_else(|| right.find_at_position(target)),

            _ => None,
        };

        res.or_else(|| {
            if self.get_span().contains(target) {
                return Some(self.clone());
            }

            None
        })
    }

    pub fn get_type(&self) -> Type {
        match self.clone() {
            Self::Literal { ty, .. } => ty,
            Self::Closure { ty, .. } => ty,
            Self::Block { ty, .. } => ty,
            Self::Let { ty, .. } => ty,
            Self::Var { ty, .. } => ty,
            Self::Call { ty, .. } => ty,
            Self::If { ty, .. } => ty,
            Self::CheckType { ty, .. } => ty,
            Self::Match { ty, .. } => ty,
            Self::Tuple { ty, .. } => ty,
            Self::EnumDef { .. } => Type::discard(),
            Self::StructDef { .. } => Type::discard(),
            Self::StructCall { ty, .. } => ty,
            Self::FieldAccess { ty, .. } => ty,
            Self::VarUpdate { .. } => Type::discard(),
            Self::MethodCall { ty, .. } => ty,
            Self::Return { ty, .. } => ty,
            Self::Try { ty, .. } => ty,
            Self::ImplBlock { .. } => Type::discard(),
            Self::Binary { ty, .. } => ty,
            Self::Paren { ty, .. } => ty,
            Self::Unary { ty, .. } => ty,
            Self::Const { ty, .. } => ty,
            Self::Debug { ty, .. } => ty,
            Self::Spawn { ty, .. } => ty,
            Self::Select { ty, .. } => ty,
            Self::Defer { ty, .. } => ty,
            Self::Reference { ty, .. } => ty,
            Self::Index { ty, .. } => ty,
            Self::Raw { .. } => Type::discard(),
            Self::Loop { .. } => Type::discard(),
            Self::Flow { .. } => Type::discard(),
            Self::TypeAlias { .. } => Type::discard(),
            Self::NewtypeDef { .. } => Type::discard(),
            Self::UsePackage { .. } => Type::discard(),
            Self::Trait { .. } => Type::discard(),

            Self::Unit { ty, .. } => ty,
            Self::Noop => Type::discard(),
            Self::Todo => Type::discard(),
        }
    }

    pub fn replace_type(self, new_ty: Type) -> Self {
        let mut ret = self;
        match ret {
            Self::Literal { ref mut ty, .. } => *ty = new_ty,
            Self::Closure { ref mut ty, .. } => *ty = new_ty,
            Self::Block { ref mut ty, .. } => *ty = new_ty,
            Self::Let { ref mut ty, .. } => *ty = new_ty,
            Self::Var { ref mut ty, .. } => *ty = new_ty,
            Self::Call { ref mut ty, .. } => *ty = new_ty,
            Self::If { ref mut ty, .. } => *ty = new_ty,
            Self::CheckType { ref mut ty, .. } => *ty = new_ty,
            Self::Match { ref mut ty, .. } => *ty = new_ty,
            Self::Tuple { ref mut ty, .. } => *ty = new_ty,
            Self::EnumDef { .. } => (),
            Self::StructDef { .. } => (),
            Self::StructCall { ref mut ty, .. } => *ty = new_ty,
            Self::FieldAccess { ref mut ty, .. } => *ty = new_ty,
            Self::VarUpdate { .. } => (),
            Self::MethodCall { ref mut ty, .. } => *ty = new_ty,
            Self::Return { ref mut ty, .. } => *ty = new_ty,
            Self::Try { ref mut ty, .. } => *ty = new_ty,
            Self::ImplBlock { .. } => (),
            Self::Binary { .. } => (),
            Self::Paren { ref mut ty, .. } => *ty = new_ty,
            Self::Unary { ref mut ty, .. } => *ty = new_ty,
            Self::Const { ref mut ty, .. } => *ty = new_ty,
            Self::Debug { ref mut ty, .. } => *ty = new_ty,
            Self::Spawn { ref mut ty, .. } => *ty = new_ty,
            Self::Select { ref mut ty, .. } => *ty = new_ty,
            Self::Defer { ref mut ty, .. } => *ty = new_ty,
            Self::Reference { ref mut ty, .. } => *ty = new_ty,
            Self::Index { ref mut ty, .. } => *ty = new_ty,
            Self::Raw { .. } => (),
            Self::Loop { .. } => (),
            Self::Flow { .. } => (),
            Self::TypeAlias { .. } => (),
            Self::NewtypeDef { .. } => (),
            Self::UsePackage { .. } => (),
            Self::Trait { .. } => (),

            Self::Unit { .. } => (),
            Self::Noop => (),
            Self::Todo => (),
        };

        ret
    }

    pub fn get_span(&self) -> Span {
        match self.clone() {
            Self::Literal { span, .. } => span,
            Self::Closure { span, .. } => span,
            Self::Block { span, .. } => span,
            Self::Let { span, .. } => span,
            Self::Var { span, .. } => span,
            Self::Call { span, .. } => span,
            Self::If { span, .. } => span,
            Self::CheckType { span, .. } => span,
            Self::Match { span, .. } => span,
            Self::Tuple { span, .. } => span,
            Self::EnumDef { span, .. } => span,
            Self::StructDef { span, .. } => span,
            Self::StructCall { span, .. } => span,
            Self::FieldAccess { span, .. } => span,
            Self::VarUpdate { span, .. } => span,
            Self::MethodCall { span, .. } => span,
            Self::Return { span, .. } => span,
            Self::Try { span, .. } => span,
            Self::ImplBlock { span, .. } => span,
            Self::Binary { span, .. } => span,
            Self::Paren { span, .. } => span,
            Self::Unary { span, .. } => span,
            Self::Const { span, .. } => span,
            Self::Debug { span, .. } => span,
            Self::Spawn { span, .. } => span,
            Self::Select { span, .. } => span,
            Self::Defer { span, .. } => span,
            Self::Reference { span, .. } => span,
            Self::Index { span, .. } => span,
            Self::Loop { span, .. } => span,
            Self::Flow { span, .. } => span,
            Self::TypeAlias { span, .. } => span,
            Self::NewtypeDef { span, .. } => span,
            Self::UsePackage { span, .. } => span,
            Self::Trait { span, .. } => span,

            Self::Unit { span, .. } => span,
            Self::Raw { .. } => Span::dummy(),
            Self::Noop => Span::dummy(),
            Self::Todo => Span::dummy(),
        }
    }

    pub fn replace_span(self, new_span: &Span) -> Self {
        let new_span = new_span.clone();
        let mut ret = self;

        match ret {
            Self::Literal { ref mut span, .. } => *span = new_span,
            Self::Closure { ref mut span, .. } => *span = new_span,
            Self::Block { ref mut span, .. } => *span = new_span,
            Self::Let { ref mut span, .. } => *span = new_span,
            Self::Var { ref mut span, .. } => *span = new_span,
            Self::Call { ref mut span, .. } => *span = new_span,
            Self::If { ref mut span, .. } => *span = new_span,
            Self::CheckType { ref mut span, .. } => *span = new_span,
            Self::Match { ref mut span, .. } => *span = new_span,
            Self::Tuple { ref mut span, .. } => *span = new_span,
            Self::EnumDef { .. } => (),
            Self::StructDef { .. } => (),
            Self::StructCall { ref mut span, .. } => *span = new_span,
            Self::FieldAccess { ref mut span, .. } => *span = new_span,
            Self::VarUpdate { .. } => todo!(),
            Self::MethodCall { ref mut span, .. } => *span = new_span,
            Self::Return { ref mut span, .. } => *span = new_span,
            Self::Try { ref mut span, .. } => *span = new_span,
            Self::ImplBlock { .. } => (),
            Self::Binary { .. } => (),
            Self::Paren { ref mut span, .. } => *span = new_span,
            Self::Unary { ref mut span, .. } => *span = new_span,
            Self::Const { ref mut span, .. } => *span = new_span,
            Self::Debug { ref mut span, .. } => *span = new_span,
            Self::Spawn { ref mut span, .. } => *span = new_span,
            Self::Select { ref mut span, .. } => *span = new_span,
            Self::Defer { ref mut span, .. } => *span = new_span,
            Self::Reference { ref mut span, .. } => *span = new_span,
            Self::Index { ref mut span, .. } => *span = new_span,
            Self::Loop { ref mut span, .. } => *span = new_span,
            Self::Flow { ref mut span, .. } => *span = new_span,
            Self::TypeAlias { ref mut span, .. } => *span = new_span,
            Self::NewtypeDef { ref mut span, .. } => *span = new_span,
            Self::UsePackage { ref mut span, .. } => *span = new_span,
            Self::Trait { ref mut span, .. } => *span = new_span,

            Self::Unit { .. } => (),
            Self::Raw { .. } => (),
            Self::Noop => (),
            Self::Todo => (),
        };

        ret
    }

    pub fn as_function(&self) -> Function {
        match self {
            Expr::Closure { fun, .. } => fun.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_var_name(&self) -> Option<String> {
        match self {
            Expr::Var { value, .. } => Some(value.to_string()),
            Expr::FieldAccess { expr, .. } => expr.as_var_name(),
            Expr::VarUpdate { target, .. } => target.as_var_name(),
            Expr::Index { expr, .. } => expr.as_var_name(),
            Expr::Unary { op, expr, .. } => {
                if op == &UnOp::Deref {
                    expr.as_var_name()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn as_result_constructor(&self) -> Option<std::result::Result<(), ()>> {
        let variant = match self {
            Expr::Var { ref value, .. } => Some(value.as_str()),
            _ => None,
        }?;

        match variant {
            "Result::Ok" => Some(Ok(())),
            "Result::Err" => Some(Err(())),
            _ => None,
        }
    }

    pub fn as_option_constructor(&self) -> Option<std::result::Result<(), ()>> {
        let variant = match self {
            Expr::Var { ref value, .. } => Some(value.as_str()),
            _ => None,
        }?;

        match variant {
            "Option::Some" => Some(Ok(())),
            "Option::None" => Some(Err(())),
            _ => None,
        }
    }
}
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum TypeAst {
    Con { name: String, args: Vec<Self> },
    Fun { args: Vec<Self>, ret: Box<Self> },
    Unknown,
}

impl TypeAst {
    pub fn unit() -> Self {
        Self::Con {
            name: "Unit".into(),
            args: vec![],
        }
    }

    pub fn get_name(&self) -> Option<String> {
        match self {
            Self::Con { name, .. } => Some(name.to_string()),
            _ => None,
        }
    }

    fn ext() -> TypeAst {
        TypeAst::Con {
            name: "EXT".to_string(),
            args: vec![],
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FileId(pub i32);

impl FileId {
    fn dummy() -> FileId {
        FileId(-1)
    }
}
