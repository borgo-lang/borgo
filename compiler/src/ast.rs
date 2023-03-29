use crate::type_::Type;
use crate::{global_state::Declaration, parse};
use proc_macro2::{LineColumn as ProcLine, Span as ProcSpan};
use serde::{Deserialize, Serialize};
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
    pub pat: Pat,
    pub ann: TypeAst,
    pub ty: Type,
    // TODO needs a span
    // span: Span
}

impl Binding {
    pub fn from_pat(pat: syn::Pat) -> Result<Self> {
        let pat = Pat::from_pat_expr(pat)?;

        let ann = match pat {
            Pat::Type { ref ann, .. } => ann.clone(),
            _ => TypeAst::Unknown,
        };

        Ok(Self {
            pat,
            ann,
            ty: Type::dummy(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub name: Ident,
    pub generics: Vec<String>,
    pub bounds: Vec<TypeAst>,
    pub args: Vec<Binding>,
    pub ret: Type,
    pub ann: TypeAst,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pat {
    Type {
        ident: String,
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

    pub fn from_pat_expr(p: syn::Pat) -> Result<Self> {
        // TODO fix this
        // Add Pat::Ident for new variables
        // Pat::Path always refers to ctors
        // Basically like TupleStruct but with no args
        let span = Span::make(p.span());

        match p {
            syn::Pat::Type(ty) => Ok(Pat::Type {
                ident: Self::parse_ident(*ty.pat)?,
                ann: parse::type_from_expr(*ty.ty),
                span,
            }),

            syn::Pat::Ident(_) => {
                let ident = Self::parse_ident(p)?;

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
                    ann: TypeAst::Unknown,
                    span,
                })
            }

            syn::Pat::Lit(l) => {
                let expr = Expr::from_expr(*l.expr)?;
                match expr {
                    Expr::Literal { lit, ty, span } => Ok(Pat::Lit { lit, ty, span }),
                    _ => panic!("Pat::Lit should parse to a Literal"),
                }
            }

            syn::Pat::Path(p) => {
                let ident = Self::parse_path(p.path);

                Ok(Pat::Pat {
                    ident,
                    elems: vec![],
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::TupleStruct(t) => {
                let ident = Self::parse_path(t.path);
                let elems = t
                    .pat
                    .elems
                    .into_iter()
                    .map(Self::from_pat_expr)
                    .collect::<Result<_>>()?;

                Ok(Pat::Pat {
                    ident,
                    elems,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Pat::Struct(t) => {
                let ident = Self::parse_path(t.path);
                let fields = t
                    .fields
                    .into_iter()
                    .map(|p| {
                        Ok(StructFieldPat {
                            name: parse::parse_member(p.member)?,
                            value: Self::from_pat_expr(*p.pat)?,
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
                    return Ok(Pat::Unit { span });
                }

                let name = format!("Tuple{}", t.elems.len());
                let fields = t
                    .elems
                    .into_iter()
                    .enumerate()
                    .map(|(index, p)| {
                        Ok(StructFieldPat {
                            name: Expr::tuple_index_string(index.try_into().unwrap())?,
                            value: Self::from_pat_expr(p)?,
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
                msg: "Can't pattern match on list literals".to_string(),
                span: Span::make(e.span()),
            }),

            syn::Pat::Wild(_) => Ok(Pat::Wild { span }),

            _ => panic!("unexpected pat expression {:#?}", &p),
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
            Pat::Type { span, .. } => span.clone(),
            Pat::Lit { span, .. } => span.clone(),
            Pat::Pat { span, .. } => span.clone(),
            Pat::Struct { span, .. } => span.clone(),
            Pat::Wild { span } => span.clone(),
            Pat::Unit { span } => span.clone(),
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Pat::Type { .. } => None,
            Pat::Lit { ty, .. } => Some(ty.clone()),
            Pat::Pat { ty, .. } => Some(ty.clone()),
            Pat::Struct { ty, .. } => Some(ty.clone()),
            Pat::Wild { .. } => None,
            Pat::Unit { .. } => Some(Type::unit()),
        }
    }

    fn find_at_position(&self, target: &Span) -> Option<Expr> {
        // Nasty hack, build a dummy expr so that the type can be shown
        let make_expr = |ty: Option<Type>, span: Span| Expr::Var {
            value: "not really a var, used in match pat".to_string(),
            decl: Declaration::dummy(),
            ty: ty.unwrap_or_else(|| Type::Con {
                name: "_".to_string(),
                args: vec![],
            }),
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

            Pat::Unit { span } => {
                if span.contains(target) {
                    Some(make_expr(None, span.clone()))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct StructFieldPat {
    pub name: Ident,
    pub value: Pat,
    // TODO add span
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Arm {
    pub pat: Pat,
    pub expr: Expr,
}

impl Arm {
    fn parse(input: syn::Arm) -> Result<Self> {
        let pat = Pat::from_pat_expr(input.pat)?;
        let expr = Expr::from_expr(*input.body)?;
        Ok(Self { pat, expr })
    }
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
    pub generics: Vec<String>,
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
    pub generics: Vec<String>,
    pub fields: Vec<StructFieldDef>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructFieldDef {
    pub name: Ident,
    pub ann: TypeAst,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExternKind {
    Effect,
    Native,
    Overload,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    pub fn make(s: ProcSpan) -> Self {
        Self {
            start: LineColumn::make(s.start()),
            end: LineColumn::make(s.end()),
        }
    }

    pub fn dummy() -> Self {
        let line = LineColumn { line: 0, col: 0 };

        Self {
            start: line.clone(),
            end: line,
        }
    }

    pub fn from_position(line: u32, col: u32) -> Self {
        let l = LineColumn {
            line: line.try_into().unwrap(),
            col: col.try_into().unwrap(),
        };

        Self {
            start: l.clone(),
            end: l,
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
        decl: Declaration,
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
    StructAccess {
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
    ExternDecl {
        name: String,
        kind: ExternKind,
        items: Vec<Expr>,
        span: Span,
    },
    ImplBlock {
        ann: TypeAst,
        ty: Type,
        items: Vec<Expr>,
        generics: Vec<String>,
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
        ty: Type,
        span: Span,
    },

    Unit {
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
    String(String),
    Char(String),
    List(Vec<Expr>),
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
}

#[derive(Deserialize, Clone, Debug)]
pub struct UnparsedFile {
    pub filename: String,
    pub contents: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File {
    pub name: String,
    pub decls: Vec<Expr>,
    pub source: String,
}

impl File {
    pub fn from_file(name: String, source: String, input: syn::File) -> Result<Self> {
        let decls = input
            .items
            .into_iter()
            .map(|e| Expr::from_item(e, FunctionKind::TopLevel))
            .collect::<Result<Vec<_>>>()?;

        Ok(Self {
            name,
            source,
            decls,
        })
    }

    pub fn from_file_contents(filename: &str, contents: &str) -> (Self, Vec<ParseError>) {
        syn::parse_str::<syn::File>(contents)
            .map_err(|err| ParseError {
                msg: err.to_string(),
                span: Span::make(err.span()),
            })
            .and_then(|file| {
                let file = File::from_file(filename.to_string(), contents.to_string(), file)?;
                Ok((file, vec![]))
            })
            .unwrap_or_else(|err| {
                let file = File {
                    name: filename.to_string(),
                    source: contents.to_string(),
                    decls: vec![],
                };

                (file, vec![err])
            })
    }

    pub fn find_expr_at_position(&self, span: &Span) -> Option<Expr> {
        self.decls.iter().find_map(|e| e.find_at_position(span))
    }
}

impl Expr {
    pub fn from_expr(input: syn::Expr) -> Result<Self> {
        let root_span = Span::make(input.span());

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
                syn::Lit::Str(s) => Ok(Expr::Literal {
                    lit: Literal::String(s.value()),
                    ty: Type::dummy(),
                    span: root_span,
                }),
                syn::Lit::Char(c) => Ok(Expr::Literal {
                    lit: Literal::Char(c.value().to_string()),
                    ty: Type::dummy(),
                    span: root_span,
                }),
                _ => todo!("uncovered literal {:#?}", lit),
            },

            syn::Expr::Block(syn::ExprBlock { block, .. }) => {
                let stmts = block
                    .stmts
                    .into_iter()
                    .map(Self::from_statement)
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
                    decl: Declaration::dummy(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Call(call) => {
                let func = Self::from_expr(*call.func)?;
                let args = call
                    .args
                    .into_iter()
                    .map(Self::from_expr)
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
                    .map(Self::from_expr)
                    .collect::<Result<_>>()?;

                Ok(Expr::Literal {
                    lit: Literal::List(elems),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Cast(c) => {
                let expr = Self::from_expr(*c.expr)?;
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
                    .map(Binding::from_pat)
                    .collect::<Result<_>>()?;

                let body = Self::from_expr(*c.body)?;
                let ann = parse::parse_output(c.output, TypeAst::Unknown);

                let fun = Function {
                    name: "__anonymous".into(),
                    generics: vec![],
                    bounds: vec![],
                    args,
                    body: body.into(),
                    ann,
                    ret: Type::dummy(),
                };

                Ok(Expr::Closure {
                    fun,
                    kind: FunctionKind::Lambda,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::If(i) => {
                let cond = Self::from_expr(*i.cond)?;
                let then = Self::from_block(i.then_branch)?;
                let els = i
                    .else_branch
                    .map(|(_, block)| Self::from_expr(*block))
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
                let subject = Self::from_expr(*m.expr)?;
                let arms = m.arms.into_iter().map(Arm::parse).collect::<Result<_>>()?;

                Ok(Expr::Match {
                    subject: subject.into(),
                    arms,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Tuple(t) => {
                if t.elems.is_empty() {
                    return Ok(Expr::Unit { span: root_span });
                }

                let elems = t
                    .elems
                    .into_iter()
                    .map(Self::from_expr)
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
                            value: Self::from_expr(f.expr)?,
                        })
                    })
                    .collect::<Result<_>>()?;

                let rest = s.rest.map(|e| Self::from_expr(*e)).transpose()?;

                Ok(Expr::StructCall {
                    name,
                    fields,
                    rest: rest.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Field(f) => {
                let expr = Self::from_expr(*f.base)?;
                let field = parse::parse_member(f.member)?;

                Ok(Expr::StructAccess {
                    expr: expr.into(),
                    field,
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Assign(a) => {
                let target = Self::from_expr(*a.left)?;
                let value = Self::from_expr(*a.right)?;

                Ok(Expr::VarUpdate {
                    target: target.into(),
                    value: value.into(),
                    span: root_span,
                })
            }

            syn::Expr::MethodCall(m) => {
                let target = Self::from_expr(*m.receiver)?;
                let method = m.method.to_string();
                let args = m
                    .args
                    .into_iter()
                    .map(Self::from_expr)
                    .collect::<Result<_>>()?;

                let span = Span::make(m.method.span());

                Ok(Expr::MethodCall {
                    target: target.into(),
                    method,
                    args,
                    ty: Type::dummy(),
                    span,
                })
            }

            syn::Expr::Return(r) => {
                let expr = r
                    .expr
                    .map(|e| Self::from_expr(*e))
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
                    .map(Self::from_expr)
                    .unwrap_or_else(|| Ok(Expr::Noop))?;

                if name == "todo" {
                    return Ok(Self::Debug {
                        kind: DebugKind::Todo,
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "unreachable" {
                    return Ok(Self::Debug {
                        kind: DebugKind::Unreachable,
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "spawn" {
                    return Ok(Self::Spawn {
                        expr: expr.into(),
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                if name == "select" {
                    return Ok(Self::Select {
                        arms: vec![], // will be populated in infer
                        ty: Type::dummy(),
                        span: root_span,
                    });
                }

                panic!("Unknown macro {}", name);
            }

            syn::Expr::Try(t) => {
                let expr = Self::from_expr(*t.expr)?;
                Ok(Self::Try {
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Binary(b) => {
                let op = parse::parse_operator(b.op)?;
                let left = Self::from_expr(*b.left)?;
                let right = Self::from_expr(*b.right)?;

                Ok(Self::Binary {
                    op,
                    left: left.into(),
                    right: right.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Paren(p) => {
                let expr = Self::from_expr(*p.expr)?;

                Ok(Self::Paren {
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            syn::Expr::Unary(u) => {
                let op = parse::parse_unop(u.op)?;
                let expr = Self::from_expr(*u.expr)?;

                Ok(Self::Unary {
                    op,
                    expr: expr.into(),
                    ty: Type::dummy(),
                    span: root_span,
                })
            }

            _ => todo!("uncovered expr {:#?}", input),
        }
    }

    pub fn from_item(item: syn::Item, kind: FunctionKind) -> Result<Self> {
        let span = Span::make(item.span());

        match item {
            syn::Item::Fn(fun) => Ok(Self::Closure {
                fun: parse::parse_item_fn(fun)?,
                ty: Type::dummy(),
                kind,
                span,
            }),

            syn::Item::Enum(e) => {
                let generics = parse::parse_generics(e.generics);
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
                let generics = parse::parse_generics(e.generics);
                let fields = parse::parse_fields(e.fields);
                let _attrs = e
                    .attrs
                    .iter()
                    .map(|a| {
                        a.path
                            .segments
                            .iter()
                            .map(|s| s.ident.to_string())
                            .collect::<Vec<_>>()
                            .join("::")
                    })
                    .collect::<Vec<_>>();

                match fields {
                    parse::Fields::StructFields(fields) => {
                        let def = StructDefinition {
                            name: e.ident.to_string(),
                            generics,
                            fields,
                        };

                        Ok(Expr::StructDef { def, span })
                    }

                    _ => panic!("wrong fields in struct"),
                }
            }

            syn::Item::ForeignMod(f) => {
                let name = f
                    .abi
                    .name
                    .map(|x| x.value())
                    .unwrap_or_else(|| "effects".to_string());

                let kind = if name.starts_with("native") {
                    ExternKind::Native
                } else if name.starts_with("effect") {
                    ExternKind::Effect
                } else if name == "overload" {
                    ExternKind::Overload
                } else {
                    panic!("unknown extern kind {}", name)
                };

                let module_name: Option<String> = match kind {
                    ExternKind::Native => {
                        Some(name.chars().into_iter().skip("native/".len()).collect())
                    }
                    ExternKind::Effect => None,
                    ExternKind::Overload => None,
                };

                let items = f
                    .items
                    .iter()
                    .map(|i| match i {
                        syn::ForeignItem::Fn(fun) => {
                            let (generics, args, ann) =
                                parse::parse_signature(fun.sig.clone(), None)?;

                            let bounds = parse::parse_bounds(&fun.sig);

                            let fun_name = fun.sig.ident.to_string();

                            let fun_name = match &module_name {
                                Some(name) => format!("{}::{}", name, fun_name),
                                None => fun_name,
                            };

                            let new_func = Function {
                                name: fun_name,
                                args,
                                generics,
                                bounds,
                                ret: Type::dummy(),
                                ann,
                                body: Expr::Noop.into(),
                            };

                            let span = Span::make(i.span());

                            Ok(Expr::Closure {
                                fun: new_func,
                                kind: FunctionKind::TopLevel,
                                ty: Type::dummy(),
                                span,
                            })
                        }

                        _ => panic!("not yet implemented {:#?}", i),
                    })
                    .collect::<Result<_>>()?;

                Ok(Expr::ExternDecl {
                    name,
                    kind,
                    items,
                    span,
                })
            }

            syn::Item::Impl(i) => {
                let ann = parse::type_from_expr(*i.self_ty.clone());
                let ty_name = ann.get_name().unwrap();
                let impl_generics = parse::parse_generics(i.generics);

                let receiver = Binding {
                    pat: Pat::Type {
                        ident: "self".to_string(),
                        ann: ann.clone(),
                        span,
                    },
                    ann: ann.clone(),
                    ty: Type::dummy(),
                };

                let items = i
                    .items
                    .into_iter()
                    .map(|item| match item {
                        syn::ImplItem::Method(fun) => {
                            let root_span = Span::make(fun.span());

                            let (mut generics, args, ann) =
                                parse::parse_signature(fun.sig.clone(), Some(receiver.clone()))?;

                            let bounds = parse::parse_bounds(&fun.sig);

                            // add generics to signature
                            generics.extend(impl_generics.clone());

                            // TODO all this stuff is the same as parse_item_fn, refactor
                            let span = fun.block.span();
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

                            let new_name = format!("{}::{}", ty_name, fun.sig.ident);

                            let new_fun = Function {
                                name: new_name,
                                generics,
                                bounds,
                                args,
                                ann,
                                ret: Type::dummy(),
                                body: body.into(),
                            };

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

                let span = Span::make(i.self_ty.span());

                Ok(Expr::ImplBlock {
                    ann,
                    ty: Type::dummy(),
                    generics: impl_generics,
                    items,
                    span,
                })
            }

            syn::Item::Const(c) => {
                let span = Span::make(c.span());
                let expr = Self::from_expr(*c.expr)?;
                let ann = parse::type_from_expr(*c.ty);

                Ok(Expr::Const {
                    ident: c.ident.to_string(),
                    expr: expr.into(),
                    ann,
                    ty: Type::dummy(),
                    span,
                })
            }

            _ => panic!("unimplemented from_item {:?}", item),
        }
    }

    pub fn from_statement(stmt: syn::Stmt) -> Result<Self> {
        match stmt {
            syn::Stmt::Item(item) => Self::from_item(item, FunctionKind::Inline),

            syn::Stmt::Local(syn::Local {
                ref init, ref pat, ..
            }) => {
                let binding = Binding::from_pat(pat.clone())?;
                let value = init
                    .clone()
                    .map(|(_, e)| Self::from_expr(*e))
                    .unwrap_or_else(|| Ok(Self::Noop))?;

                Ok(Self::Let {
                    binding,
                    value: Box::new(value),
                    ty: Type::dummy(),
                    span: Span::make(stmt.span()),
                })
            }

            syn::Stmt::Expr(e) => Self::from_expr(e),

            syn::Stmt::Semi(e, _) => Self::from_expr(e),
        }
    }

    pub fn from_block(block: syn::Block) -> Result<Self> {
        let span = Span::make(block.span());
        let stmts = block
            .stmts
            .into_iter()
            .map(Self::from_statement)
            .collect::<Result<_>>()?;

        Ok(Expr::Block {
            stmts,
            ty: Type::dummy(),
            span,
        })
    }

    /// Only used in tests
    pub fn from_source(source: &str) -> syn::Expr {
        syn::parse_str::<syn::Expr>(source).unwrap()
    }

    pub fn tuple_index_string(index: u32) -> Result<String> {
        match index {
            0 => Ok("first".to_string()),
            1 => Ok("second".to_string()),
            2 => Ok("third".to_string()),
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
            Self::EnumDef { .. } => Type::unit(),
            Self::StructDef { .. } => Type::unit(),
            Self::StructCall { ty, .. } => ty,
            Self::StructAccess { ty, .. } => ty,
            Self::VarUpdate { .. } => todo!(),
            Self::MethodCall { ty, .. } => ty,
            Self::Return { ty, .. } => ty,
            Self::Try { ty, .. } => ty,
            Self::ExternDecl { .. } => Type::unit(),
            Self::ImplBlock { .. } => Type::unit(),
            Self::Binary { ty, .. } => ty,
            Self::Paren { ty, .. } => ty,
            Self::Unary { ty, .. } => ty,
            Self::Const { ty, .. } => ty,
            Self::Debug { ty, .. } => ty,
            Self::Spawn { ty, .. } => ty,
            Self::Select { ty, .. } => ty,

            Self::Unit { .. } => Type::unit(),
            Self::Noop => Type::unit(),
            Self::Todo => Type::unit(),
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
            Self::StructAccess { ref mut ty, .. } => *ty = new_ty,
            Self::VarUpdate { .. } => todo!(),
            Self::MethodCall { ref mut ty, .. } => *ty = new_ty,
            Self::Return { ref mut ty, .. } => *ty = new_ty,
            Self::Try { ref mut ty, .. } => *ty = new_ty,
            Self::ExternDecl { .. } => (),
            Self::ImplBlock { .. } => (),
            Self::Binary { .. } => (),
            Self::Paren { ref mut ty, .. } => *ty = new_ty,
            Self::Unary { ref mut ty, .. } => *ty = new_ty,
            Self::Const { ref mut ty, .. } => *ty = new_ty,
            Self::Debug { ref mut ty, .. } => *ty = new_ty,
            Self::Spawn { ref mut ty, .. } => *ty = new_ty,
            Self::Select { ref mut ty, .. } => *ty = new_ty,

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
            Self::StructAccess { span, .. } => span,
            Self::VarUpdate { span, .. } => span,
            Self::MethodCall { span, .. } => span,
            Self::Return { span, .. } => span,
            Self::Try { span, .. } => span,
            Self::ExternDecl { span, .. } => span,
            Self::ImplBlock { span, .. } => span,
            Self::Binary { span, .. } => span,
            Self::Paren { span, .. } => span,
            Self::Unary { span, .. } => span,
            Self::Const { span, .. } => span,
            Self::Debug { span, .. } => span,
            Self::Spawn { span, .. } => span,
            Self::Select { span, .. } => span,

            Self::Unit { span, .. } => span,
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
            Self::StructAccess { ref mut span, .. } => *span = new_span,
            Self::VarUpdate { .. } => todo!(),
            Self::MethodCall { ref mut span, .. } => *span = new_span,
            Self::Return { ref mut span, .. } => *span = new_span,
            Self::Try { ref mut span, .. } => *span = new_span,
            Self::ExternDecl { .. } => (),
            Self::ImplBlock { .. } => (),
            Self::Binary { .. } => (),
            Self::Paren { ref mut span, .. } => *span = new_span,
            Self::Unary { ref mut span, .. } => *span = new_span,
            Self::Const { ref mut span, .. } => *span = new_span,
            Self::Debug { ref mut span, .. } => *span = new_span,
            Self::Spawn { ref mut span, .. } => *span = new_span,
            Self::Select { ref mut span, .. } => *span = new_span,

            Self::Unit { .. } => (),
            Self::Noop => (),
            Self::Todo => (),
        };

        ret
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

    fn get_name(&self) -> Option<String> {
        match self {
            Self::Con { name, .. } => Some(name.to_string()),
            _ => None,
        }
    }

    pub fn add_type_argument(&self, typ: &str) -> Self {
        match self {
            TypeAst::Con { name, args } => {
                let mut new_args = args.clone();

                let typ = Self::Con {
                    name: typ.to_string(),
                    args: vec![],
                };
                new_args.push(typ);

                TypeAst::Con {
                    name: name.clone(),
                    args: new_args,
                }
            }

            TypeAst::Fun { .. } => unreachable!(),
            TypeAst::Unknown => unreachable!(),
        }
    }
}
