use crate::{
    ast::{
        Arm, Binding, Constructor, EnumDefinition, EnumFieldDef, Expr, FileId, Function,
        FunctionKind, Generic, InterfaceSuperTrait, LineColumn, Literal, Loop, LoopFlow,
        NewtypeDefinition, Operator, Pat, PkgImport, SelectArm, SelectArmPat, Span, StrType,
        StructDefinition, StructField, StructFieldDef, StructFieldPat, TypeAliasDef, TypeAst, UnOp,
    },
    lexer::{Lexer, Pos, Token, TokenKind, TokenKind::EOF},
    type_::Type,
};

use serde::{Deserialize, Serialize};

pub struct Parser {
    tok: Token,
    tokens: Vec<Token>,
    pub current: usize,
    pub errors: Vec<ParseError>,
    pub file_id: FileId,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file_id: FileId) -> Parser {
        let mut p = Parser {
            tok: token_eof(),
            current: 0,
            tokens,
            errors: Default::default(),
            file_id,
        };

        p.tok = p.nth(0);
        p
    }

    fn next(&mut self) -> Pos {
        let prev = self.tok.pos;

        self.current += 1;
        self.tok = self.nth(self.current);

        self.skip_comments();

        prev
    }

    fn nth(&self, pos: usize) -> Token {
        self.tokens.get(pos).cloned().unwrap_or(token_eof())
    }

    pub fn eof(&self) -> bool {
        self.tok.kind == EOF
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.tok.kind == kind {
            self.next();
            return true;
        }

        false
    }

    fn is_not(&self, kind: TokenKind) -> bool {
        if self.eof() {
            return false;
        }

        return self.tok.kind != kind;
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.tok.kind != kind {
            self.error(format!("expected {} got {}", kind, self.tok.kind));
        }

        // always make progress
        self.next();
    }

    fn expect_semi(&mut self) {
        // semicolon is optional before a closing ')' or '}'
        if matches!(self.tok.kind, TokenKind::RParen | TokenKind::RCurly) {
            return;
        }

        if self.eof() {
            return;
        }

        self.expect(TokenKind::Semicolon);
        // TODO advance
        // self.advance_stmt_start();
    }

    fn expect_comma_or(&mut self, kind: TokenKind) {
        if self.tok.kind == TokenKind::Semicolon {
            let peek = self.nth(self.current + 1).kind;
            if peek == kind || peek == TokenKind::Comma {
                // eat the semicolon if next token is what we're expecting
                self.next();
            }
        }

        if self.tok.kind != kind {
            self.expect(TokenKind::Comma);
        }
    }

    // More lenient version of expect_semi
    // Will ignore ',' if preceeded by ';'
    fn expect_semi_lenient(&mut self) {
        if self.eat(TokenKind::Comma) {
            // optionally try eating ';'
            self.eat(TokenKind::Semicolon);
            return;
        }

        self.expect_semi();
    }

    fn error(&mut self, msg: String) {
        // dbg!(&self.tok, &msg);

        let err = ParseError {
            msg,
            span: self.single_span(self.tok.pos),
        };

        self.errors.push(err);
    }

    fn start(&self) -> Pos {
        self.tok.pos
    }

    fn skip_comments(&mut self) {
        if self.tok.kind == TokenKind::Comment {
            self.next();
        }
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.expr_bp(
            0,
            Restrictions {
                structs_allowed: true,
            },
        )
    }

    pub fn parse_expr_no_struct(&mut self) -> Expr {
        self.expr_bp(
            0,
            Restrictions {
                structs_allowed: false,
            },
        )
    }

    // Algorithm stolen from matklad :)
    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn expr_bp(&mut self, min_bp: u8, restrictions: Restrictions) -> Expr {
        let start = self.start();
        let kind = self.tok.kind;

        // TODO asdf clean this up, move to unary_expr
        let mut lhs = match self.unary_operator() {
            Some(op) => {
                // Consume unary operator
                self.next();

                let ((), r_bp) = prefix_binding_power(kind);
                let rhs = self.expr_bp(r_bp, restrictions);

                Expr::Unary {
                    op,
                    expr: rhs.into(),
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            }

            None => {
                // Handle references &foo, &mut
                if self.eat(TokenKind::Ampersand) {
                    // Eat a 'mut' token if present
                    // For now it doesn't affect anything
                    self.eat(TokenKind::Mut);

                    let rhs = self.parse_expr();

                    return Expr::Reference {
                        expr: rhs.into(),
                        mutable: true,
                        ty: Type::dummy(),
                        span: self.make_span(start),
                    };
                }

                // Otherwise it's a primary expression
                self.primary_expr()
            }
        };

        loop {
            let kind = self.tok.kind;

            if self.eof() {
                break;
            }

            if let Some((l_bp, ())) = postfix_binding_power(kind, restrictions) {
                if l_bp < min_bp {
                    break;
                }

                lhs = self.expr_postfix(lhs);
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(kind) {
                if l_bp < min_bp {
                    break;
                }

                // Consume the binary operator
                let op = self.operator();
                let rhs = self.expr_bp(r_bp, restrictions);

                lhs = Expr::Binary {
                    op,
                    left: lhs.into(),
                    right: rhs.into(),
                    ty: Type::dummy(),
                    span: self.make_span(start),
                };

                continue;
            }

            break;
        }

        lhs
    }

    fn primary_expr(&mut self) -> Expr {
        if lit_token(self.tok.kind) {
            return self.expr_lit();
        }

        match self.tok.kind {
            TokenKind::LParen => self.expr_paren(),

            TokenKind::LCurly => self.expr_block(),

            TokenKind::Ident => self.expr_ident(),

            TokenKind::Fn_ => self.expr_fn(FunctionKind::Inline),

            TokenKind::Match => self.expr_match(),

            TokenKind::If => self.expr_if(),

            TokenKind::Pipe => self.expr_lambda(),
            TokenKind::Pipe2 => self.expr_lambda(),

            TokenKind::MultiString => self.expr_multi_string(),

            TokenKind::Return => self.stmt_return(),
            TokenKind::Support => self.stmt_support(),
            TokenKind::Break | TokenKind::Continue => self.stmt_loop_flow(),

            TokenKind::Error => {
                self.error(self.tok.text.clone());
                self.next();
                Expr::Noop
            }

            _ => self.unhandled_token("expr"),
        }
    }

    fn expr_lit(&mut self) -> Expr {
        assert!(lit_token(self.tok.kind));

        let start = self.start();

        let lit = match self.tok.kind {
            TokenKind::Int => {
                let n = self.tok.text.parse().unwrap();
                Literal::Int(n)
            }

            TokenKind::Float => {
                let n = self.tok.text.parse().unwrap();
                Literal::Float(n)
            }

            TokenKind::Bool => Literal::Bool(self.tok.text == "true"),

            TokenKind::String => Literal::String(StrType::Single(self.tok.text.clone())),

            TokenKind::Char => Literal::Char(self.tok.text.clone()),

            TokenKind::LBrack => {
                let elems = self.parse_paren(ParseParen::Brack);

                return Expr::Literal {
                    lit: Literal::Slice(elems),
                    ty: Type::dummy(),
                    span: self.make_span(start),
                };
            }

            _ => return self.unhandled_token("literal"),
        };

        let span = self.single_span(start);

        // We only get here with primitive types, so always advance.
        // When parsing slices, the next token is already in the right position.
        self.next();

        Expr::Literal {
            lit,
            ty: Type::dummy(),
            span,
        }
    }

    fn expr_ident(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Ident);

        let tok = self.tok.clone();
        self.next();

        Expr::Var {
            value: tok.text,
            decl: Span::dummy(),
            generics_instantiated: Default::default(),
            ty: Type::dummy(),
            span: self.single_span(tok.pos),
        }
    }

    // Look for a single ident
    fn parse_one_ident(&mut self) -> String {
        if self.tok.kind == TokenKind::Ident {
            let ident = self.tok.text.clone();
            self.next();
            ident
        } else {
            self.expect(TokenKind::Ident);
            "_".to_string()
        }
    }

    // Will also eat any dots ie. Option.Some
    fn parse_ident_dots(&mut self) -> String {
        if self.tok.kind == TokenKind::Ident {
            let ident = self.tok.text.clone();
            self.next();

            if self.tok.kind == TokenKind::Dot {
                // consume '.'
                self.next();

                format!("{}.{}", ident, self.parse_ident_dots())
            } else {
                ident
            }
        } else {
            self.expect(TokenKind::Ident);
            "_".to_string()
        }
    }

    fn expr_field_selector(&mut self, lhs: Expr) -> Expr {
        assert!(self.tok.kind == TokenKind::Dot);

        // Skip '.'
        self.next();
        let start = self.start();

        // Pointer dereference
        // a.b.*
        if self.eat(TokenKind::Star) {
            return Expr::Unary {
                op: UnOp::Deref,
                expr: lhs.into(),

                ty: Type::dummy(),
                span: self.make_span(start),
            };
        }

        // Tuple field access
        // a.0
        if self.tok.kind == TokenKind::Int {
            let index = self.tok.text.parse().unwrap();

            // consume 'int'
            self.next();

            return Expr::FieldAccess {
                expr: lhs.into(),
                field: Expr::tuple_index_string(index).unwrap(),

                ty: Type::dummy(),
                span: self.make_span(start),
            };
        }

        let field = self.tok.text.clone();
        self.expect(TokenKind::Ident);

        // Field access
        // a.b.c
        Expr::FieldAccess {
            expr: lhs.into(),
            field,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_postfix(&mut self, lhs: Expr) -> Expr {
        match self.tok.kind {
            TokenKind::LParen => self.expr_call(lhs),
            TokenKind::LBrack => self.expr_index(lhs),
            TokenKind::LCurly => self.expr_struct_call(lhs),
            TokenKind::Question => self.expr_try(lhs),
            TokenKind::Dot => self.expr_field_selector(lhs),
            _ => return self.unhandled_token("postfix"),
        }
    }

    fn expr_call(&mut self, lhs: Expr) -> Expr {
        let start = self.start();
        let args = self.parse_paren(ParseParen::Paren);

        Expr::Call {
            func: lhs.into(),
            args,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_block(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::LCurly);

        // consume '{'
        let start = self.next();

        let mut stmts = vec![];

        while self.is_not(TokenKind::RCurly) {
            let s = self.parse_stmt();
            stmts.push(s);
        }

        self.expect(TokenKind::RCurly);

        Expr::Block {
            stmts,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_fn(&mut self, kind: FunctionKind) -> Expr {
        assert!(self.tok.kind == TokenKind::Fn_);

        // consume 'fn'
        let start = self.next();

        // For top-level functions, allow '.' in the name
        // This is needed for defining static functions
        let name = match kind {
            FunctionKind::TopLevel => self.parse_ident_dots(),
            FunctionKind::Inline | FunctionKind::Lambda => self.parse_one_ident(),
        };

        let generics = self.parse_generics_list();

        let args = self.parse_param_list(ParamsMode::Fn);

        let ann = self.parse_ret_type(kind);

        let body = self.expr_block();

        let fun = Function {
            name,
            generics,
            args,
            ann,
            body: body.into(),
            ret: Type::dummy(),
            bounded_ty: Type::dummy().to_bounded(),
        };

        Expr::Closure {
            fun,
            kind,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_match(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Match);

        // consume 'match'
        let start = self.next();

        let subject = self.parse_expr_no_struct();

        self.expect(TokenKind::LCurly);

        let mut arms = vec![];

        while self.is_not(TokenKind::RCurly) {
            let arm = self.parse_match_arm();
            arms.push(arm);

            self.expect_semi_lenient();
        }

        self.expect(TokenKind::RCurly);

        Expr::Match {
            subject: subject.into(),
            arms,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_if(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::If);

        // consume 'if'
        let start = self.next();

        let cond = self.parse_expr_no_struct();
        let then = self.expr_block();

        let els = if self.tok.kind == TokenKind::Else {
            // consume 'else'
            self.next();

            if self.tok.kind == TokenKind::If {
                // else if
                self.expr_if()
            } else {
                self.expr_block()
            }
        } else {
            Expr::Noop
        };

        Expr::If {
            cond: cond.into(),
            then: then.into(),
            els: els.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_lambda(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Pipe || self.tok.kind == TokenKind::Pipe2);

        let start = self.start();

        let args = if self.tok.kind == TokenKind::Pipe {
            self.parse_param_list(ParamsMode::Lambda)
        } else {
            // if token is '||' we already know there are no params
            self.next();
            vec![]
        };

        let kind = FunctionKind::Lambda;
        let ann = self.parse_ret_type(kind);
        let body = self.parse_expr();

        let fun = Function {
            name: "__anonymous".into(),
            args,
            ann,
            generics: vec![],
            body: body.into(),
            ret: Type::dummy(),
            bounded_ty: Type::dummy().to_bounded(),
        };

        Expr::Closure {
            fun,
            kind,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_multi_string(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::MultiString);

        // TODO asdf Inlining Multistrings in a function call is a bit of a PITA right now.
        // The solution is to exctract them out to a variable, which might not always be desirable.
        // The problem is that a ';' is automatically inserted after each line
        let start = self.start();

        // Accumulate all lines
        let mut lines = vec![];

        loop {
            lines.push(self.tok.text.clone());

            // consume MultiString
            self.next();

            // a semicolon is always inserted afterwards.
            // consume it only if the next token is still a MultiString.
            //
            if self.tok.kind != TokenKind::MultiString {
                break;
            }

            /*
            if self.nth(self.current + 1).kind == TokenKind::MultiString {
                // consume ';'
                self.next();
                continue;
            } else {
                // otherwise break off the loop, MultiString is finished
                break;
            }
            */
        }

        let lit = Literal::String(StrType::Multi(lines));

        Expr::Literal {
            lit,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_try(&mut self, lhs: Expr) -> Expr {
        assert!(self.tok.kind == TokenKind::Question);

        // consume '?'
        let start = self.next();

        Expr::Try {
            expr: lhs.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn expr_struct_call(&mut self, lhs: Expr) -> Expr {
        assert!(self.tok.kind == TokenKind::LCurly);

        let name = self.flatten_expr_to_name(&lhs);

        // consume '{'
        let start = self.next();

        let mut fields = vec![];
        let mut rest = None;

        while self.is_not(TokenKind::RCurly) {
            if rest.is_some() {
                self.error("rest expression (..x) should be last".to_string());
                break;
            }

            // Parse ..rest
            if self.tok.kind == TokenKind::Dot {
                rest = Some(self.expr_rest());
                continue;
            }

            // Parse field: value
            let field = self.parse_one_ident();
            let value = if self.tok.kind == TokenKind::Colon {
                // consume ':'
                self.next();
                self.parse_expr()
            } else {
                // shorthand syntax ie. Foo { x }
                // create a field x => Var("x")
                Expr::Var {
                    value: field.clone(),
                    decl: Span::dummy(),
                    generics_instantiated: Default::default(),
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            };

            fields.push(StructField { name: field, value });

            if self.tok.kind == TokenKind::Comma {
                self.next();
            }
        }

        self.expect(TokenKind::RCurly);

        Expr::StructCall {
            name,
            fields,
            rest: rest.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    // ..rest expression in struct update
    fn expr_rest(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Dot);

        // consume '.'
        self.next();

        // second '.'
        self.expect(TokenKind::Dot);

        self.parse_expr()
    }

    // parse an index expr ie. foo[3]
    fn expr_index(&mut self, lhs: Expr) -> Expr {
        assert!(self.tok.kind == TokenKind::LBrack);

        // consume '['
        let start = self.next();

        let index = self.parse_expr();

        self.expect(TokenKind::RBrack);

        Expr::Index {
            expr: lhs.into(),
            index: index.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn parse_match_arm(&mut self) -> Arm {
        let pat = self.parse_pat();

        self.expect(TokenKind::FatArrow);

        let expr = self.parse_expr();

        Arm {
            pat,
            expr: expr.into(),
        }
    }

    fn parse_stmt(&mut self) -> Expr {
        let e = match self.tok.kind {
            TokenKind::Let => self.stmt_let(),
            TokenKind::Enum => self.item_enum(),
            TokenKind::Struct => self.item_struct(),
            TokenKind::Interface => self.item_interface(),
            TokenKind::Return => self.stmt_return(),
            TokenKind::Defer => self.stmt_defer(),
            TokenKind::Spawn => self.stmt_spawn(),
            TokenKind::Fn_ => self.expr_fn(FunctionKind::Inline),
            TokenKind::For | TokenKind::While | TokenKind::Loop => self.stmt_loop(),
            TokenKind::Break | TokenKind::Continue => self.stmt_loop_flow(),
            TokenKind::Select => self.stmt_select(),
            TokenKind::Support => self.stmt_support(),
            _ => self.stmt_assign(),
        };

        self.expect_semi();

        e
    }

    fn stmt_let(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Let);

        // consume 'let'
        let start = self.next();

        let mut mutable = false;

        if self.eat(TokenKind::Mut) {
            mutable = true;
        }

        let binding = self.parse_binding(&AnnotationMode::Optional);

        self.expect(TokenKind::Eq);

        let expr = self.parse_expr();

        Expr::Let {
            binding,
            value: expr.into(),
            mutable,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn stmt_return(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Return);

        // consume 'return'
        let start = self.next();

        let expr = if self.tok.kind == TokenKind::Semicolon {
            self.next();
            Expr::Noop
        } else {
            self.parse_expr()
        };

        Expr::Return {
            expr: expr.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn stmt_defer(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Defer);

        // consume 'defer'
        let start = self.next();

        let expr = self.parse_expr();

        Expr::Defer {
            expr: expr.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn stmt_spawn(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Spawn);

        // consume 'spawn'
        let start = self.next();

        let expr = self.parse_expr();

        Expr::Spawn {
            expr: expr.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn stmt_loop(&mut self) -> Expr {
        let start = self.start();

        let kind = match self.tok.kind {
            TokenKind::For => {
                // consume 'for'
                self.next();

                let binding = self.parse_binding(&AnnotationMode::Optional);

                self.expect(TokenKind::In_);
                let expr = self.parse_expr_no_struct();

                Loop::WithCondition {
                    binding,
                    expr: expr.into(),
                }
            }

            TokenKind::While => {
                // consume 'while'
                self.next();

                let expr = self.parse_expr_no_struct();
                Loop::While { expr: expr.into() }
            }

            TokenKind::Loop => {
                // consume 'loop'
                self.next();

                Loop::NoCondition
            }

            _ => unreachable!(),
        };

        let body = self.expr_block();

        Expr::Loop {
            kind,
            body: body.into(),
            span: self.make_span(start),
        }
    }

    fn stmt_loop_flow(&mut self) -> Expr {
        let kind = match self.tok.kind {
            TokenKind::Break => LoopFlow::Break,
            TokenKind::Continue => LoopFlow::Continue,
            _ => unreachable!(),
        };

        let start = self.next();

        Expr::Flow {
            kind,
            span: self.make_span(start),
        }
    }

    fn stmt_support(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Support);

        match self.tok.text.as_str() {
            "ensure" => self.stmt_check_expr(),
            "rawgo" => self.stmt_rawgo_expr(),
            "unreachable" => self.expr_debug(),
            _ => {
                self.error(format!("unknown built-in function {}", self.tok.text));
                Expr::Noop
            }
        }
    }

    fn stmt_check_expr(&mut self) -> Expr {
        // consume '@ensure'
        let start = self.next();

        let expr = self.parse_expr();
        self.expect(TokenKind::Comma);
        let ann = self.parse_type();

        Expr::CheckType {
            expr: expr.into(),
            ann,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn stmt_rawgo_expr(&mut self) -> Expr {
        // consume '@rawgo'
        let _start = self.next();

        let args = self.parse_paren(ParseParen::Paren);
        let text = match args.first() {
            Some(Expr::Literal {
                lit: Literal::String(inner),
                ..
            }) => match inner {
                StrType::Single(s) => s.to_string(),
                StrType::Multi(lines) => lines.join("\n"),
            },

            _ => {
                self.error("invalid call to rawgo!".to_string());
                "".to_string()
            }
        };

        Expr::Raw { text }
    }

    fn stmt_select(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Select);

        // consume 'select'
        let start = self.next();

        let mut arms = vec![];

        self.expect(TokenKind::LCurly);

        while self.is_not(TokenKind::RCurly) {
            let pat = match self.tok.kind {
                TokenKind::Let => {
                    //  'let' x = ch.Recv()
                    let stmt = self.stmt_let();

                    match stmt {
                        Expr::Let { binding, value, .. } => SelectArmPat::Recv(binding, *value),
                        _ => unreachable!(),
                    }
                }

                TokenKind::Ident if self.tok.text == "_" => {
                    //  Wildcard
                    self.next();
                    SelectArmPat::Wildcard
                }

                _ => {
                    //  ch.Send()
                    let expr = self.parse_expr_no_struct();
                    SelectArmPat::Send(expr)
                }
            };

            self.expect(TokenKind::FatArrow);

            let expr = self.parse_expr();
            arms.push(SelectArm { pat, expr });

            self.expect_semi_lenient();
        }

        self.expect(TokenKind::RCurly);

        Expr::Select {
            arms,
            span: self.make_span(start),
        }
    }

    fn expr_debug(&mut self) -> Expr {
        let kind = match self.tok.text.as_str() {
            "unreachable" => crate::ast::DebugKind::Unreachable,
            _ => panic!("unexpected debug {}", self.tok.text),
        };

        // consume @unreachable
        let start = self.next();

        let _params = self.parse_param_list(ParamsMode::Fn);

        // TODO don't ignore params, they're useful for @todo()

        Expr::Debug {
            kind,
            expr: Expr::Noop.into(),
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn item_import(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Import);

        // consume 'import'
        let start = self.next();

        let name = self.parse_ident_dots();
        let import = PkgImport {
            name,
            span: self.make_span(start),
        };

        Expr::UsePackage {
            import,
            span: self.make_span(start),
        }
    }

    fn stmt_assign(&mut self) -> Expr {
        let start = self.start();

        let lhs = self.parse_expr();

        if self.tok.kind != TokenKind::Eq {
            return lhs;
        }

        // consume '='
        self.next();

        // TODO validate that LHS is valid target for assignment, or do it in later pass?

        let rhs = self.parse_expr();

        Expr::VarUpdate {
            target: lhs.into(),
            value: rhs.into(),
            span: self.make_span(start),
        }
    }

    fn expr_paren(&mut self) -> Expr {
        let start = self.start();

        let args = self.parse_paren(ParseParen::Paren);

        match args.len() {
            0 => {
                // unit
                Expr::Unit {
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            }
            1 => {
                // (a + b)
                Expr::Paren {
                    expr: args[0].clone().into(),
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            }
            _ => {
                // (a, b, c)
                Expr::Tuple {
                    elems: args,
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            }
        }
    }

    fn parse_paren(&mut self, mode: ParseParen) -> Vec<Expr> {
        let (open, close) = match mode {
            ParseParen::Paren => (TokenKind::LParen, TokenKind::RParen),
            ParseParen::Brack => (TokenKind::LBrack, TokenKind::RBrack),
        };

        self.expect(open);

        let mut elems = vec![];

        while self.is_not(close) {
            let expr = self.parse_expr();
            elems.push(expr);

            // Consume ';' if present.
            // Function calls that span multiple lines are tricky to parse, especially if they
            // involve a MultiString, because a comma can't be put on the same line.
            // So a ';' would be present because inserted automatically,
            // though not visible in source code.
            self.eat(TokenKind::Semicolon);

            if self.tok.kind == TokenKind::Comma {
                self.next();
            }
        }

        self.expect(close);
        elems
    }

    fn operator(&mut self) -> Operator {
        let op = match self.tok.kind {
            TokenKind::Plus => Operator::Add,
            TokenKind::Minus => Operator::Sub,
            TokenKind::Star => Operator::Mul,
            TokenKind::Slash => Operator::Div,
            TokenKind::LAngle => Operator::Lt,
            TokenKind::LTEq => Operator::Le,
            TokenKind::RAngle => Operator::Gt,
            TokenKind::GTEq => Operator::Ge,
            TokenKind::Percent => Operator::Rem,
            TokenKind::Eq2 => Operator::Eq,
            TokenKind::NotEq => Operator::Ne,
            TokenKind::Ampersand2 => Operator::And,
            TokenKind::Pipe2 => Operator::Or,

            _ => panic!("unexpected operator {:#?}", self.tok),
        };

        self.next();
        op
    }

    fn parse_pat(&mut self) -> Pat {
        if lit_token(self.tok.kind) {
            return self.parse_pat_lit();
        }

        match self.tok.kind {
            TokenKind::Ident => self.parse_pat_ident(),
            TokenKind::LParen => self.parse_pat_paren(),

            _ => panic!("unhandled pat {:#?}", self.tok),
        }
    }

    fn parse_pat_ident(&mut self) -> Pat {
        let start = self.start();

        let ident = self.parse_ident_dots();

        if self.tok.kind == TokenKind::LCurly {
            return self.parse_pat_struct(ident);
        }

        // This could be either a binding ie. 'x'
        // Or a match on a specific variant ie. Option.Some(x)
        // Or a match on a struct ie. Foo { a: 1 }

        let mut elems = vec![];

        if self.tok.kind == TokenKind::LParen {
            elems = self.parse_pat_list();
        }

        let has_seen_dots = ident.contains(".");
        let span = self.single_span(start);

        if !elems.is_empty() || has_seen_dots || first_uppercase(&ident) {
            Pat::Pat {
                ident,
                elems,
                ty: Type::dummy(),
                span,
            }
        } else if ident == "_" {
            Pat::Wild { span }
        } else {
            Pat::Type {
                ident,
                is_mut: false,
                ann: TypeAst::Unknown,
                span,
            }
        }
    }

    fn parse_pat_struct(&mut self, ident: String) -> Pat {
        assert!(self.tok.kind == TokenKind::LCurly);

        // consume '{'
        let start = self.next();

        let mut fields = vec![];

        while self.is_not(TokenKind::RCurly) {
            // Parse field: value
            let field = self.parse_one_ident();
            let value = if self.tok.kind == TokenKind::Colon {
                // consume ':'
                self.next();
                self.parse_pat()
            } else {
                Pat::Type {
                    ident: field.clone(),
                    is_mut: false,
                    ann: TypeAst::Unknown,
                    span: self.make_span(start),
                }
            };

            fields.push(StructFieldPat { name: field, value });

            if self.tok.kind == TokenKind::Comma {
                self.next();
            }
        }

        self.expect(TokenKind::RCurly);

        Pat::Struct {
            ident,
            fields,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn parse_pat_lit(&mut self) -> Pat {
        let expr = self.parse_expr();
        match expr {
            Expr::Literal { lit, ty, span } => Pat::Lit { lit, ty, span },
            _ => panic!("expected literal expr"),
        }
    }

    fn parse_pat_paren(&mut self) -> Pat {
        let start = self.start();
        let elems = self.parse_pat_paren_elems();
        let count = elems.len();

        match count {
            0 => Pat::Unit {
                ty: Type::dummy(),
                span: self.make_span(start),
            },

            1 => {
                self.error("unnecessary parentheses".to_string());
                elems[0].clone()
            }

            _ => {
                let name = format!("Tuple{}", count);
                let fields = elems
                    .into_iter()
                    .enumerate()
                    .map(|(index, pat)| StructFieldPat {
                        name: Expr::tuple_index_string(index.try_into().unwrap()).unwrap(),
                        value: pat,
                    })
                    .collect();

                Pat::Struct {
                    ident: name,
                    fields,
                    ty: Type::dummy(),
                    span: self.make_span(start),
                }
            }
        }
    }

    fn parse_pat_paren_elems(&mut self) -> Vec<Pat> {
        assert!(self.tok.kind == TokenKind::LParen);

        // consume '('
        self.next();

        let mut args = vec![];

        while self.is_not(TokenKind::RParen) {
            let expr = self.parse_pat();
            args.push(expr);

            if self.tok.kind == TokenKind::Comma {
                self.next();
            }
        }

        self.expect(TokenKind::RParen);
        args
    }

    // Parse x, y in Option.Some(x, y)
    fn parse_pat_list(&mut self) -> Vec<Pat> {
        assert!(self.tok.kind == TokenKind::LParen);

        // consume '('
        self.next();

        let mut elems = vec![];

        while self.is_not(TokenKind::RParen) {
            let pat = self.parse_pat();
            elems.push(pat);
            self.expect_comma_or(TokenKind::RParen);
        }

        self.expect(TokenKind::RParen);

        elems
    }

    fn parse_ann(&mut self) -> TypeAst {
        assert!(self.tok.kind == TokenKind::Colon);

        // consume ':'
        self.next();

        self.parse_type()
    }

    fn optional_ann(&mut self) -> TypeAst {
        if self.tok.kind == TokenKind::Colon {
            self.parse_ann()
        } else {
            TypeAst::Unknown
        }
    }

    fn parse_binding(&mut self, mode: &AnnotationMode) -> Binding {
        let pat = self.parse_pat();
        let ann = match mode {
            AnnotationMode::Required => self.parse_ann(),
            AnnotationMode::Optional => self.optional_ann(),
        };

        Binding {
            pat,
            ann,
            ty: Type::dummy(),
        }
    }

    fn parse_type(&mut self) -> TypeAst {
        // fn (...) -> x annotation
        if self.tok.kind == TokenKind::Fn_ {
            return self.parse_type_fn();
        }

        // [T] slice
        if self.tok.kind == TokenKind::LBrack {
            return self.parse_type_slice();
        }

        // (T, Y) tuple
        if self.tok.kind == TokenKind::LParen {
            return self.parse_type_tuple();
        }

        // *T pointer
        // could also eat TokenKind::Ampersand + TokenKind::Mut
        if self.eat(TokenKind::Star) {
            return TypeAst::Con {
                name: "Ref".to_string(),
                args: vec![self.parse_type()],
            };
        }

        // Foo.Bar<T> annotation
        let name = self.parse_ident_dots();

        let mut args = vec![];

        if self.tok.kind == TokenKind::LAngle {
            self.expect(TokenKind::LAngle);

            while self.is_not(TokenKind::RAngle) {
                let ty = self.parse_type();
                args.push(ty);

                self.expect_comma_or(TokenKind::RAngle);
            }

            self.expect(TokenKind::RAngle);
        }

        TypeAst::Con { name, args }
    }

    fn parse_type_fn(&mut self) -> TypeAst {
        assert!(self.tok.kind == TokenKind::Fn_);

        // consume 'fn'
        self.next();

        let args = self
            .parse_param_list(ParamsMode::Fn)
            .into_iter()
            .map(|b| b.ann)
            .collect();

        let ret = self.parse_ret_type(FunctionKind::Inline);

        TypeAst::Fun {
            args,
            ret: ret.into(),
        }
    }

    fn parse_type_slice(&mut self) -> TypeAst {
        assert!(self.tok.kind == TokenKind::LBrack);

        // consume '['
        self.next();

        let mut args = vec![];

        while self.is_not(TokenKind::RBrack) {
            let ty = self.parse_type();
            args.push(ty);

            self.expect_comma_or(TokenKind::RBrack);
        }

        self.expect(TokenKind::RBrack);

        TypeAst::Con {
            name: "Slice".to_string(),
            args,
        }
    }

    fn parse_type_tuple(&mut self) -> TypeAst {
        assert!(self.tok.kind == TokenKind::LParen);

        // consume '('
        self.next();

        let mut args = vec![];

        while self.is_not(TokenKind::RParen) {
            let ty = self.parse_type();
            args.push(ty);

            self.expect_comma_or(TokenKind::RParen);
        }

        self.expect(TokenKind::RParen);

        let count = args.len();

        let name = if count > 0 {
            format!("Tuple{}", count)
        } else {
            "Unit".to_string()
        };

        TypeAst::Con { name, args }
    }

    fn parse_generics_list(&mut self) -> Vec<Generic> {
        if self.tok.kind != TokenKind::LAngle {
            return vec![];
        }

        // consume '<'
        self.next();

        let mut generics = vec![];

        while self.is_not(TokenKind::RAngle) {
            let g = self.parse_generic_param();
            generics.push(g);

            self.expect_comma_or(TokenKind::RAngle);
        }

        self.expect(TokenKind::RAngle);

        generics
    }

    fn parse_generic_param(&mut self) -> Generic {
        let start = self.start();
        let name = self.parse_one_ident();
        let bounds = self.parse_bounds_in_type_param(TokenKind::RAngle);

        Generic {
            name,
            bounds,
            span: self.make_span(start),
        }
    }

    fn parse_bounds_in_type_param(&mut self, close: TokenKind) -> Vec<TypeAst> {
        if self.tok.kind != TokenKind::Colon {
            return vec![];
        }

        // consume ':'
        self.next();

        let mut bounds = vec![];

        while ![close, TokenKind::Comma].contains(&self.tok.kind) {
            let ty = self.parse_type();
            bounds.push(ty);

            if self.tok.kind == TokenKind::Plus {
                self.next();
            }
        }

        bounds
    }

    fn parse_param_list(&mut self, mode: ParamsMode) -> Vec<Binding> {
        let (open, close, annotation) = match mode {
            ParamsMode::Fn => (
                TokenKind::LParen,
                TokenKind::RParen,
                AnnotationMode::Required,
            ),
            ParamsMode::Lambda => (TokenKind::Pipe, TokenKind::Pipe, AnnotationMode::Optional),
        };

        self.expect(open);

        let mut params = vec![];

        while self.is_not(close) {
            let binding = self.parse_binding(&annotation);
            params.push(binding);

            self.expect_comma_or(close);
        }

        self.expect(close);

        params
    }

    fn parse_ret_type(&mut self, kind: FunctionKind) -> TypeAst {
        if self.tok.kind == TokenKind::Arrow {
            // consume '->'
            self.next();
            return self.parse_type();
        }

        match kind {
            FunctionKind::TopLevel | FunctionKind::Inline => TypeAst::unit(),
            FunctionKind::Lambda => TypeAst::Unknown,
        }
    }

    fn flatten_expr_to_name(&mut self, expr: &Expr) -> String {
        let mut res = vec![];
        let ok = doit(expr, &mut res);

        if let Err(err) = ok {
            self.error(err);
            return "_".to_string();
        }

        return res.join(".");

        fn doit(expr: &Expr, res: &mut Vec<String>) -> Result<(), String> {
            match expr {
                Expr::Var { value, .. } => {
                    res.push(value.to_string());
                    Ok(())
                }
                Expr::FieldAccess { expr, field, .. } => {
                    doit(expr, res)?;
                    res.push(field.to_string());
                    Ok(())
                }
                _ => Err("unexpected expression".to_string()),
            }
        }
    }

    fn unary_operator(&self) -> Option<UnOp> {
        match self.tok.kind {
            TokenKind::Bang => Some(UnOp::Not),
            TokenKind::Minus => Some(UnOp::Neg),
            // Deref is handled in expr_field_selector
            _ => None,
        }
    }

    fn item_struct(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Struct);

        // consume 'struct'
        let start = self.next();

        let name = self.parse_one_ident();
        let generics = self.parse_generics_list();

        // Newtype
        if self.tok.kind == TokenKind::LParen {
            let fields = self.parse_type_paren_elems();

            if fields.len() == 0 {
                self.error("Specify at least one type".to_string());
            }

            let def = NewtypeDefinition {
                name,
                generics,
                fields,
            };

            return Expr::NewtypeDef {
                def,
                span: self.make_span(start),
            };
        }

        let mut fields = vec![];

        self.expect(TokenKind::LCurly);

        while self.is_not(TokenKind::RCurly) {
            let f = self.parse_struct_field_def();
            fields.push(f);

            self.expect_comma_or(TokenKind::RCurly);
        }

        self.expect(TokenKind::RCurly);

        let def = StructDefinition {
            name,
            generics,
            fields,
        };

        Expr::StructDef {
            def,
            span: self.make_span(start),
        }
    }

    fn item_enum(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Enum);

        // consume 'enum'
        let start = self.next();

        let name = self.parse_one_ident();
        let generics = self.parse_generics_list();

        let mut cons = vec![];

        self.expect(TokenKind::LCurly);

        while self.is_not(TokenKind::RCurly) {
            let f = self.parse_enum_constructor();
            cons.push(f);

            self.expect_comma_or(TokenKind::RCurly);
        }

        self.expect(TokenKind::RCurly);

        let def = EnumDefinition {
            name,
            generics,
            cons,
        };

        Expr::EnumDef {
            def,
            span: self.make_span(start),
        }
    }

    fn parse_enum_constructor(&mut self) -> Constructor {
        let name = self.parse_one_ident();
        let fields = self.parse_type_paren_elems();

        Constructor { name, fields }
    }

    fn parse_type_paren_elems(&mut self) -> Vec<EnumFieldDef> {
        if self.tok.kind != TokenKind::LParen {
            return vec![];
        }

        // consume '('
        self.next();

        let mut args = vec![];

        while self.is_not(TokenKind::RParen) {
            let ann = self.parse_type();

            let def = EnumFieldDef {
                name: format!("field{}", args.len()),
                ann,
                ty: Type::dummy(),
            };

            args.push(def);

            if self.tok.kind == TokenKind::Comma {
                self.next();
            }
        }

        self.expect(TokenKind::RParen);
        args
    }

    fn parse_struct_field_def(&mut self) -> StructFieldDef {
        let name = self.parse_one_ident();
        self.expect(TokenKind::Colon);
        let ann = self.parse_type();

        StructFieldDef {
            name,
            ann,
            ty: Type::dummy().to_bounded(),
        }
    }

    fn item_interface(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Interface);

        // consume 'interface'
        let start = self.next();

        let name = self.parse_one_ident();

        let generics = self.parse_generics_list();

        // TODO asdf supertraits should be added to interface as-is, not as strings, because they
        // can have type parameters.
        let mut supertraits = vec![];
        let mut items = vec![];

        self.expect(TokenKind::LCurly);

        while self.is_not(TokenKind::RCurly) {
            match self.tok.kind {
                TokenKind::Fn_ => {
                    let i = self.parse_interface_method();
                    items.push(i);
                }

                TokenKind::Impl => {
                    // consume 'impl'
                    self.next();

                    let start = self.start();
                    let ann = self.parse_type();
                    let span = self.make_span(start);

                    supertraits.push(InterfaceSuperTrait {
                        ann,
                        span,
                        ty: Type::dummy(),
                    });
                }

                _ => {
                    self.error("unexpected token, expecting `fn` or `impl`".to_string());
                    self.next();
                }
            }

            self.expect_semi();
        }

        self.expect(TokenKind::RCurly);

        Expr::Trait {
            name,
            generics,
            supertraits,
            items,
            span: self.make_span(start),
        }
    }

    fn item_type_alias(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Type);

        // consume 'type'
        let start = self.next();

        let name = self.parse_one_ident();
        let generics = self.parse_generics_list();

        self.expect(TokenKind::Eq);

        let alias = self.parse_type();

        let def = TypeAliasDef {
            name,
            generics,
            ann: alias,
            ty: Type::dummy().to_bounded(),
        };

        Expr::TypeAlias {
            def,
            span: self.make_span(start),
        }
    }

    fn parse_interface_method(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Fn_);

        // consume 'fn'
        self.next();

        let start = self.start();

        let name = self.parse_one_ident();
        let args = self.parse_param_list(ParamsMode::Fn);
        let ann = self.parse_ret_type(FunctionKind::TopLevel);

        let fun = Function {
            name,
            generics: vec![],
            args,
            ann,
            body: Expr::Noop.into(),
            ret: Type::dummy(),
            bounded_ty: Type::dummy().to_bounded(),
        };

        Expr::Closure {
            fun,
            kind: FunctionKind::TopLevel,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn item_const(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Const);

        // consume 'const'
        let start = self.next();

        let ident = self.parse_one_ident();
        let ann = self.optional_ann();

        self.expect(TokenKind::Eq);

        let expr = self.parse_expr();

        Expr::Const {
            ident,
            expr: expr.into(),
            ann,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    fn item_impl(&mut self) -> Expr {
        assert!(self.tok.kind == TokenKind::Impl);

        // consume 'impl'
        let start = self.next();

        let generics = self.parse_generics_list();
        let params = self.parse_param_list(ParamsMode::Fn);

        if params.len() != 1 {
            self.error("Method receiver is a single param".to_string());
        }

        let (self_name, ann) = params
            .first()
            .and_then(|b| match &b.pat {
                Pat::Type { ident, .. } => {
                    let name = ident.to_string();
                    Some((name, b.ann.clone()))
                }
                _ => {
                    self.error("expected name".to_string());
                    None
                }
            })
            .unwrap_or_else(|| ("".to_string(), TypeAst::Unknown));

        let mut items = vec![];

        self.expect(TokenKind::LCurly);

        while self.is_not(TokenKind::RCurly) {
            let i = self.expr_fn(FunctionKind::TopLevel);
            items.push(i);

            self.expect_semi();
        }

        self.expect(TokenKind::RCurly);

        Expr::ImplBlock {
            ann,
            items,
            self_name,
            generics,
            ty: Type::dummy(),
            span: self.make_span(start),
        }
    }

    pub fn parse_item(&mut self) -> Expr {
        match self.tok.kind {
            TokenKind::Enum => self.item_enum(),
            TokenKind::Struct => self.item_struct(),
            TokenKind::Interface => self.item_interface(),
            TokenKind::Fn_ => self.expr_fn(FunctionKind::TopLevel),
            TokenKind::Impl => self.item_impl(),
            TokenKind::Const => self.item_const(),
            TokenKind::Import => self.item_import(),
            TokenKind::Type => self.item_type_alias(),

            TokenKind::Comment => {
                self.skip_comments();
                Expr::Noop
            }

            _ => self.unhandled_token("item"),
        }
    }

    pub fn parse_file(&mut self) -> Vec<Expr> {
        let mut items = vec![];

        self.skip_comments();

        while !self.eof() {
            let item = self.parse_item();
            self.expect_semi();
            items.push(item);
        }

        items
    }

    // Create a Span only looking at the single position.
    // Used by single-token expressions, like primitives
    fn single_span(&self, start: Pos) -> Span {
        let mut end = start;
        end.col += start.len;
        Span {
            start: pos_linecol(start),
            end: pos_linecol(end),
            file: self.file_id.clone(),
        }
    }

    // Create a Span from start to current token's position
    fn make_span(&self, start: Pos) -> Span {
        // `tok` points to start of current token
        // so the span ends one char before that.
        // this yields somewhat incorrect results, because it doesn't take into account whitespace
        // between the previous token and the current one.
        // the end position isn't particularly useful in most situations anyway, so even if
        // it's inaccurate it's not that big of a deal.
        let mut end = self.nth(self.current - 1).pos;
        end.col += end.len;

        // TODO asdf the logic here is messed up.
        // Probably worth also look at self.prev?
        // Sometimes end ends up being less than start

        Span {
            start: pos_linecol(start),
            end: pos_linecol(end),
            file: self.file_id.clone(),
        }
    }

    pub fn lex_and_parse_file(source: &str, file: FileId) -> Result<Vec<Expr>, ParseError> {
        let mut lex = Lexer::new(source);

        let tokens = lex.tokens();

        let mut p = Parser::new(tokens.clone(), file);

        let decls = p.parse_file();

        if let Some(err) = p.errors.first().cloned() {
            return Err(err);
        }

        Ok(decls)
    }

    fn unhandled_token(&mut self, ctx: &str) -> Expr {
        self.error(format!("unhandled {ctx} {:#?}", self.tok));

        // advance past the unexpected token
        self.next();

        Expr::Noop
    }
}

fn pos_linecol(pos: Pos) -> LineColumn {
    LineColumn {
        line: pos.line,
        col: pos.col,
    }
}

fn lit_token(kind: TokenKind) -> bool {
    [
        TokenKind::Int,
        TokenKind::Bool,
        TokenKind::Char,
        TokenKind::String,
        TokenKind::Float,
        TokenKind::LBrack,
    ]
    .contains(&kind)
}

fn first_uppercase(ident: &str) -> bool {
    let first = ident.chars().next().unwrap_or('a');
    first.is_uppercase()
}

fn token_eof() -> Token {
    Token {
        kind: TokenKind::EOF,
        text: "Parser EOF".to_string(),
        pos: Pos {
            line: 990,
            col: 990,
            len: 990,
        },
    }
}

// whether the annotation is required
// when parsing a binding
//   a(: string)
enum AnnotationMode {
    Optional,
    Required,
}

enum ParamsMode {
    Fn,     // look for parens, annotations are required
    Lambda, // look for pipes, annotations are optional
}

// Parse expresions inside (a, b) or [a, b]
#[derive(Clone, Copy)]
enum ParseParen {
    Paren,
    Brack,
}

// Struct calls are allowed pretty much everywhere
// except in control expressions, like: if, match, for, while
// That's because the '{' would be ambiguous: is it the start of the block or is it the start of
// the list of struct fields?
#[derive(Clone, Copy)]
struct Restrictions {
    structs_allowed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseError {
    pub msg: String,
    // tok: Token,
    pub span: Span,
}

fn prefix_binding_power(kind: TokenKind) -> ((), u8) {
    match kind {
        TokenKind::Minus | TokenKind::Bang => ((), 15),
        _ => unreachable!(),
    }
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        TokenKind::Pipe2 => Some((1, 2)),
        TokenKind::Ampersand2 => Some((3, 4)),

        TokenKind::LAngle
        | TokenKind::RAngle
        | TokenKind::LTEq
        | TokenKind::GTEq
        | TokenKind::Eq2
        | TokenKind::NotEq => Some((5, 6)),

        TokenKind::Plus | TokenKind::Minus => Some((7, 8)),

        TokenKind::Star | TokenKind::Slash | TokenKind::Caret | TokenKind::Percent => Some((9, 10)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind, restrictions: Restrictions) -> Option<(u8, ())> {
    if [
        TokenKind::LParen,
        TokenKind::LBrack,
        TokenKind::Question,
        TokenKind::Dot,
    ]
    .contains(&kind)
    {
        return Some((17, ()));
    }

    // Only allow '{' to be a postfix operator if structs are allowed

    if kind == TokenKind::LCurly && restrictions.structs_allowed {
        return Some((17, ()));
    }

    None
}
