use std::fmt;

pub struct Lexer {
    input: Vec<char>,

    ch: char,        // current char
    current: usize,  // index of current char
    prev: TokenKind, // The token emitted in the last call to scan() used to insert semicolons
    line: usize,     // current line
    col: usize,      // current col
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,

    // How many chars the token spans.
    // In most cases this == token.text.len() but ie. strings have 2 extra chars for quotes
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub pos: Pos,
}

impl Token {
    fn match_text(&self) -> bool {
        let kind = format!("{:?}", self);
        kind.to_lowercase() == self.text.to_lowercase()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.match_text() {
            write!(f, "{}", self.text.to_uppercase())?;
        } else {
            write!(f, "{:#?}", self.kind)?;

            if matches!(
                self.kind,
                TokenKind::Ident
                    | TokenKind::String
                    | TokenKind::Char
                    | TokenKind::Int
                    | TokenKind::Float
                    | TokenKind::MultiString
            ) {
                write!(f, "(\"{}\", {})", self.text, self.pos.len)?;
            }
        }

        write!(f, "#{},{}", self.pos.line, self.pos.col)
    }
}

pub fn stringify_tokens(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Int,
    String,
    Char,
    Bool,
    Float,
    Ident,
    Comment,
    MultiString,

    Semicolon,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LCurly,
    RCurly,
    LAngle,
    RAngle,

    Arrow,
    FatArrow,

    Eq,
    Eq2,
    NotEq,
    GTEq,
    LTEq,

    Colon,
    Pipe,
    Pipe2,
    Ampersand,
    Ampersand2,

    Plus,
    Minus,
    Star,
    Slash,
    Backslash,

    Caret,
    Percent,
    Bang,
    Question,
    Dot,
    Comma,

    Fn_,
    Let,
    If,
    Else,
    Match,
    Enum,
    Struct,
    Type,
    Interface,
    Impl,
    Const,
    Return,
    Defer,
    Import,
    Spawn,
    Mut,
    For,
    In_,
    While,
    Loop,
    Break,
    Continue,
    Select,

    Support, // @ensure, @rawgo
    Error,
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

fn get_keyword(s: &str) -> Option<TokenKind> {
    let kind = match s {
        "fn" => TokenKind::Fn_,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "match" => TokenKind::Match,
        "enum" => TokenKind::Enum,
        "struct" => TokenKind::Struct,
        "type" => TokenKind::Type,
        "interface" => TokenKind::Interface,
        "impl" => TokenKind::Impl,
        "const" => TokenKind::Const,
        "return" => TokenKind::Return,
        "defer" => TokenKind::Defer,
        "use" => TokenKind::Import,
        "spawn" => TokenKind::Spawn,
        "mut" => TokenKind::Mut,
        "for" => TokenKind::For,
        "in" => TokenKind::In_,
        "while" => TokenKind::While,
        "loop" => TokenKind::Loop,
        "break" => TokenKind::Break,
        "continue" => TokenKind::Continue,
        "select" => TokenKind::Select,
        _ => TokenKind::Error,
    };

    if kind == TokenKind::Error {
        None
    } else {
        Some(kind)
    }
}

fn get_syntax(s: &str) -> Option<TokenKind> {
    let kind = match s {
        "(" => TokenKind::LParen,
        ")" => TokenKind::RParen,
        "[" => TokenKind::LBrack,
        "]" => TokenKind::RBrack,
        "{" => TokenKind::LCurly,
        "}" => TokenKind::RCurly,
        "<" => TokenKind::LAngle,
        ">" => TokenKind::RAngle,
        "->" => TokenKind::Arrow,
        "=>" => TokenKind::FatArrow,
        "=" => TokenKind::Eq,
        "==" => TokenKind::Eq2,
        "!=" => TokenKind::NotEq,
        ">=" => TokenKind::GTEq,
        "<=" => TokenKind::LTEq,
        ":" => TokenKind::Colon,
        "|" => TokenKind::Pipe,
        "||" => TokenKind::Pipe2,
        "&" => TokenKind::Ampersand,
        "&&" => TokenKind::Ampersand2,
        "+" => TokenKind::Plus,
        "-" => TokenKind::Minus,
        "*" => TokenKind::Star,
        "^" => TokenKind::Caret,
        "%" => TokenKind::Percent,
        "!" => TokenKind::Bang,
        "?" => TokenKind::Question,
        "." => TokenKind::Dot,
        "," => TokenKind::Comma,
        _ => TokenKind::Error,
    };

    if kind == TokenKind::Error {
        None
    } else {
        Some(kind)
    }
}

// Determine if the last token at the end of a line is eligible for semicolon insertion
fn insert_semicolon(kind: TokenKind) -> bool {
    [
        TokenKind::RParen,
        TokenKind::RBrack,
        TokenKind::RCurly,
        //
        // note that Go doesn't insert a semicolon after '>'
        // but we need it otherwise method declarations interfaces would be invalid
        // ie. interface Foo {
        //   fn bar() -> Option<int>
        // }
        // The drawback is that expressions with the math operator greater than '>'
        // can't span newlines. In Go you can have a hanging '>' at the end of a line.
        TokenKind::RAngle,
        //
        TokenKind::Ident,
        TokenKind::Int,
        TokenKind::Float,
        TokenKind::Bool,
        TokenKind::Char,
        TokenKind::String,
        TokenKind::MultiString,
        TokenKind::Break,
        TokenKind::Continue,
        TokenKind::Return,
        TokenKind::Question,
    ]
    .contains(&kind)
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.chars().collect(),
            ch: ' ',
            current: 0,
            prev: TokenKind::Error,
            line: 1,
            col: 1,
        };

        // Explicitely set ch
        l.ch = l.read_char_at(0);

        l
    }

    fn start(&self) -> usize {
        self.col
    }

    fn make_pos(&self, start: usize) -> Pos {
        Pos {
            line: self.line,
            col: start,
            len: self.col - start,
        }
    }

    fn read_char_at(&self, pos: usize) -> char {
        self.input.get(pos).copied().unwrap_or('\0')
    }

    // Returns the original position before advancing
    fn next(&mut self) -> usize {
        let start = self.col;
        self.col += 1;
        self.current += 1;
        self.ch = self.read_char_at(self.current);
        start
    }

    fn peek(&self) -> char {
        self.read_char_at(self.current + 1)
    }

    fn skip(&mut self, mut count: usize) -> usize {
        let start = self.start();

        while count > 0 {
            self.next();
            count -= 1;
        }

        start
    }

    fn once(&mut self, kind: TokenKind) -> Token {
        let text = self.ch.to_string();
        let start = self.next();

        Token {
            kind,
            text,
            pos: self.make_pos(start),
        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while !self.is_eof() {
            let tok = self.scan();
            tokens.push(tok);
        }

        match tokens.last() {
            Some(t) if t.kind != TokenKind::EOF => {
                tokens.push(self.once(TokenKind::EOF));
            }
            _ => (),
        }

        tokens
    }

    fn is_eof(&self) -> bool {
        self.current >= self.input.len()
    }

    fn is_end_of_line(&self) -> bool {
        self.ch == '\n' || self.is_eof()
    }

    fn bump_line(&mut self) {
        // Inc line and reset col
        self.line += 1;
        self.col = 1;
    }

    fn error(&mut self, start: usize, consumed: String) -> Token {
        Token {
            kind: TokenKind::Error,
            text: consumed,
            pos: self.make_pos(start),
        }
    }

    // This is a bit unfortunate, the only usage is in insert_semicolon().
    // There's some backtracking that needs to happen in this function, shouldn't affect
    // performance too much, but worth looking into in future.
    fn next_token_is_dot_or_multistr(&self) -> bool {
        assert!(self.ch == '\n');

        // Start one char after the current one
        let mut pos = self.current + 1;

        // Find the next non whitespace token
        loop {
            let ch = self.read_char_at(pos);
            if ch == '\0' {
                return false;
            }

            if ch.is_ascii_whitespace() {
                pos += 1;
                continue;
            }

            // If we find a comment, we need to go past it and look for a DOT char on the next line
            if ch == '/' && self.read_char_at(pos + 1) == '/' {
                // consume '//'
                pos += 2;

                // keep going until next newline or eof
                while self.read_char_at(pos) != '\n' && pos < self.input.len() {
                    pos += 1;
                }

                // consume '\n' and keep going
                pos += 1;
                continue;
            }

            return ch == '.' || (ch == '\\' && self.read_char_at(pos + 1) == '\\');
        }
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        while self.ch.is_ascii_whitespace() {
            if self.ch != '\n' {
                self.next();
                continue;
            }

            // We're on a newline, trigger automatic semicolon insertion.
            // This is similar to what the Go scanner does.
            // One (big) difference is that we want to allow DOTs on a newline,
            // whereas Go wants you to put a trailing DOT in a chain of method calls.
            // ie.
            //      fmt.
            //          Println()
            // not:
            //      fmt
            //          .Println()
            //
            // Ideally, we support the latter, although it complicates things a bit.
            // We need to scan (indefinitely) ahead to find the next non whitespace token
            // and check if it's a DOT: in that case, we don't insert the semicolon.
            // Note that other postfix operators like '[index]', '?' and '(call)'
            // need to be on the same line to parse correctly.
            if insert_semicolon(self.prev) && !self.next_token_is_dot_or_multistr() {
                // consume '\n'
                let start = self.next();

                // Store the token with '\n' as text, indicating that this semicolon was artificial
                let tok = Token {
                    kind: TokenKind::Semicolon,
                    text: "\n".to_string(),
                    pos: self.make_pos(start),
                };

                // Go to next line
                self.bump_line();

                return Some(tok);
            }

            // consume '\n'
            self.next();
            // And reset line + col
            self.bump_line();
        }

        None
    }

    fn scan_single_token(&mut self) -> Option<Token> {
        // Check the longest token first
        let s = format!("{}{}", self.ch, self.peek());
        let pos = self.make_pos(self.col);

        return check(s, pos).or_else(|| check(self.ch.to_string(), pos));

        fn check(s: String, pos: Pos) -> Option<Token> {
            get_syntax(&s.as_str()).map(|kind| Token { kind, text: s, pos })
        }
    }

    fn digits(&mut self) -> String {
        let mut text = String::new();

        while self.ch.is_ascii_digit() {
            text.push(self.ch);
            self.next();
        }

        text
    }

    fn scan_number(&mut self) -> Token {
        let start = self.start();

        let mut text = self.digits();
        let mut kind = TokenKind::Int;

        if self.ch == '.' {
            text.push('.');
            self.next();

            let decimals = self.digits();

            if decimals.is_empty() {
                kind = TokenKind::Error;
            } else {
                kind = TokenKind::Float;
                text.push_str(&decimals);
            }
        }

        Token {
            kind,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_ident(&mut self) -> Token {
        let mut text = String::new();
        let start = self.start();

        while self.ch.is_ascii_alphanumeric() || self.ch == '_' {
            text.push(self.ch);
            self.next();
        }

        let mut kind = get_keyword(&text.as_str()).unwrap_or(TokenKind::Ident);

        if ["true", "false"].contains(&text.as_str()) {
            kind = TokenKind::Bool;
        }

        Token {
            kind,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_string(&mut self) -> Token {
        let mut text = String::new();

        let start = self.next();
        let mut seen_closing = false;

        // TODO validate escape \"
        loop {
            if self.ch == '"' {
                self.next();
                seen_closing = true;
                break;
            }

            if self.is_end_of_line() {
                break;
            }

            text.push(self.ch);
            self.next();
        }

        if !seen_closing {
            return self.error(start, text);
        }

        Token {
            kind: TokenKind::String,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_char(&mut self) -> Token {
        let mut text = String::new();

        let start = self.next();

        // TODO validate
        while self.ch != '\'' {
            text.push(self.ch);
            self.next();
        }

        self.next();

        Token {
            kind: TokenKind::Char,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_slash_comment(&mut self) -> Token {
        if self.peek() != '/' {
            let start = self.next();
            return Token {
                kind: TokenKind::Slash,
                text: self.ch.to_string(),
                pos: self.make_pos(start),
            };
        }

        // It's a comment
        let start = self.skip(2);

        let mut text = String::new();

        while !self.is_end_of_line() {
            text.push(self.ch);
            self.next();
        }

        Token {
            kind: TokenKind::Comment,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_multi_string(&mut self) -> Token {
        if self.peek() != '\\' {
            let start = self.next();
            return Token {
                kind: TokenKind::Backslash,
                text: self.ch.to_string(),
                pos: self.make_pos(start),
            };
        }

        // It's a multi-line string
        let start = self.skip(2);

        let mut text = String::new();

        while !self.is_end_of_line() {
            text.push(self.ch);
            self.next();
        }

        Token {
            kind: TokenKind::MultiString,
            text,
            pos: self.make_pos(start),
        }
    }

    fn scan_support_tokens(&mut self) -> Token {
        // consume '@'
        let start = self.next();
        let ident = self.scan_ident();

        return Token {
            kind: TokenKind::Support,
            text: ident.text,
            pos: self.make_pos(start),
        };

        // self.error(start, "@".to_owned() + &ident.text)
    }

    pub fn scan(&mut self) -> Token {
        let tok = self.scan_actual();

        // Update previous token.
        // If we just scanned a comment, keep the previous one around so that the rules for
        // semicolon insertion apply to it
        self.prev = if tok.kind == TokenKind::Comment {
            self.prev
        } else {
            tok.kind
        };

        tok
    }

    pub fn scan_actual(&mut self) -> Token {
        if let Some(tok) = self.skip_whitespace() {
            // Whitespace is ignored, but we could get a ';' token at end of new lines
            return tok;
        }

        if self.is_eof() {
            return self.once(TokenKind::EOF);
        }

        if let Some(tok) = self.scan_single_token() {
            self.skip(tok.text.len());
            return tok;
        }

        match self.ch {
            '0'..='9' => self.scan_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.scan_ident(),
            '"' => self.scan_string(),
            '\'' => self.scan_char(),
            '/' => self.scan_slash_comment(),
            '\\' => self.scan_multi_string(),
            ';' => self.once(TokenKind::Semicolon),
            '@' => self.scan_support_tokens(),

            _ => panic!("unhandled char: '{}'", self.ch),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let mut lex = Lexer::new(input);
        let actual = stringify_tokens(&lex.tokens());
        expect.assert_eq(&actual);
    }

    #[test]
    fn lex() {
        check("1", expect![[r#"Int("1", 1)#1,1, EOF#1,2"#]]);
        check("120", expect![[r#"Int("120", 3)#1,1, EOF#1,4"#]]);
        check("false", expect!["Bool#1,1, EOF#1,6"]);
        check("foo", expect![[r#"Ident("foo", 3)#1,1, EOF#1,4"#]]);
        check("\'b\'", expect![[r#"Char("b", 3)#1,1, EOF#1,4"#]]);
        check(
            "\"foo bar\"",
            expect![[r#"String("foo bar", 9)#1,1, EOF#1,10"#]],
        );

        check(
            "= == - -> | ||",
            expect!["Eq#1,1, Eq2#1,3, Minus#1,6, Arrow#1,8, Pipe#1,11, Pipe2#1,13, EOF#1,15"],
        );

        check(
            "let a = 10;",
            expect![[
                r#"Let#1,1, Ident("a", 1)#1,5, Eq#1,7, Int("10", 2)#1,9, Semicolon#1,11, EOF#1,12"#
            ]],
        );

        check(
            "fn foo<T>(a: [T]) {
            a
        }",
            expect![[
                r#"Fn_#1,1, Ident("foo", 3)#1,4, LAngle#1,7, Ident("T", 1)#1,8, RAngle#1,9, LParen#1,10, Ident("a", 1)#1,11, Colon#1,12, LBrack#1,14, Ident("T", 1)#1,15, RBrack#1,16, RParen#1,17, LCurly#1,19, Ident("a", 1)#2,13, Semicolon#2,14, RCurly#3,9, EOF#3,10"#
            ]],
        );

        check(
            "220 // this is -> a comment",
            expect![[r#"Int("220", 3)#1,1, Comment#1,5, EOF#1,28"#]],
        );

        check(
            "\\\\foo
            \\\\ bar
            ",
            expect![[
                r#"MultiString("foo", 5)#1,1, MultiString(" bar", 6)#2,13, Semicolon#2,19, EOF#3,13"#
            ]],
        );

        check(
            "{
            let a = foo()
            let b = bar()
            }",
            expect![[
                r#"LCurly#1,1, Let#2,13, Ident("a", 1)#2,17, Eq#2,19, Ident("foo", 3)#2,21, LParen#2,24, RParen#2,25, Semicolon#2,26, Let#3,13, Ident("b", 1)#3,17, Eq#3,19, Ident("bar", 3)#3,21, LParen#3,24, RParen#3,25, Semicolon#3,26, RCurly#4,13, EOF#4,14"#
            ]],
        );

        check(
            "fn foo() {
                if (bar) {
                    return
                }

                22
            }
            ",
            expect![[
                r#"Fn_#1,1, Ident("foo", 3)#1,4, LParen#1,7, RParen#1,8, LCurly#1,10, If#2,17, LParen#2,20, Ident("bar", 3)#2,21, RParen#2,24, LCurly#2,26, Return#3,21, Semicolon#3,27, RCurly#4,17, Semicolon#4,18, Int("22", 2)#6,17, Semicolon#6,19, RCurly#7,13, Semicolon#7,14, EOF#8,13"#
            ]],
        );

        check(
            "foo()
            .bar()",
            expect![[
                r#"Ident("foo", 3)#1,1, LParen#1,4, RParen#1,5, Dot#2,13, Ident("bar", 3)#2,14, LParen#2,17, RParen#2,18, EOF#2,19"#
            ]],
        );

        // newlines should be correctly accounted for
        check(
            "foo[1]
            .bar()?



            .baz",
            expect![[
                r#"Ident("foo", 3)#1,1, LBrack#1,4, Int("1", 1)#1,5, RBrack#1,6, Dot#2,13, Ident("bar", 3)#2,14, LParen#2,17, RParen#2,18, Question#2,19, Dot#6,13, Ident("baz", 3)#6,14, EOF#6,17"#
            ]],
        );

        // comments in between newlines don't affect semicolon insertion
        check(
            "foo
            // yep
            .baz",
            expect![[
                r#"Ident("foo", 3)#1,1, Comment#2,13, Dot#3,13, Ident("baz", 3)#3,14, EOF#3,17"#
            ]],
        );

        check(
            "fn foo() {}
            // comment at end",
            expect![[
                r#"Fn_#1,1, Ident("foo", 3)#1,4, LParen#1,7, RParen#1,8, LCurly#1,10, RCurly#1,11, Semicolon#1,12, Comment#2,13, EOF#2,30"#
            ]],
        );

        check("1.68", expect![[r#"Float("1.68", 4)#1,1, EOF#1,5"#]]);

        // unterminated strings produce an error
        check(
            "let a = \"foo",
            expect![[r#"Let#1,1, Ident("a", 1)#1,5, Eq#1,7, Error#1,9, EOF#1,13"#]],
        );
    }
}
