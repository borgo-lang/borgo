use crate::type_::{BoundedType, ModuleId, Symbol, Type};
use serde::{Deserialize, Serialize};

pub type Ident = String;

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

    pub fn add_receiver(&self, generics: &[Generic], ty: &Type) -> BoundedType {
        let mut ret = self.bounded_ty.clone();

        match ret.ty {
            Type::Fun { ref mut args, .. } => {
                // prepend receiver
                args.insert(0, ty.clone());
            }
            _ => unreachable!(),
        }

        // ensure generics are properly set
        ret.generics = generics.iter().map(|g| g.name.to_string()).collect();
        ret
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
        // format!("{}::{}", enum_name, self.name)
        format!("{}.{}", enum_name, self.name)
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

impl Span {
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

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
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
        mutable: bool,
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
        self_name: String,
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
        arms: Vec<SelectArm>,
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
        generics: Vec<Generic>,
        supertraits: Vec<InterfaceSuperTrait>,
        items: Vec<Expr>,
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
    String(StrType),
    Char(String), // Rust char and Go rune are slightly different, just fallback to a String
    Slice(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StrType {
    Single(String),     // "quoted string"
    Multi(Vec<String>), // multi-line string
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
    pub fn tuple_index_string(index: u32) -> Option<String> {
        match index {
            0 => Some("first".to_string()),
            1 => Some("second".to_string()),
            2 => Some("third".to_string()),
            3 => Some("fourth".to_string()),
            4 => Some("fifth".to_string()),
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
            Self::Select { .. } => Type::discard(),
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
            Self::Select { .. } => (),
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
            "Result.Ok" => Some(Ok(())),
            "Result.Err" => Some(Err(())),
            _ => None,
        }
    }

    pub fn as_option_constructor(&self) -> Option<std::result::Result<(), ()>> {
        let variant = match self {
            Expr::Var { ref value, .. } => Some(value.as_str()),
            _ => None,
        }?;

        match variant {
            "Option.Some" => Some(Ok(())),
            "Option.None" => Some(Err(())),
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FileId(pub i32);

impl FileId {
    fn dummy() -> FileId {
        FileId(-1)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceSuperTrait {
    pub ann: TypeAst,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectArm {
    pub pat: SelectArmPat,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SelectArmPat {
    Recv(Binding, Expr),
    Send(Expr),
    Wildcard,
}
