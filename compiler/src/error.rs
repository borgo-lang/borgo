use crate::exhaustive;
use crate::type_::Type;
use crate::{ast::Span, parser::ParseError};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Error {
    Unification(UnificationError),

    // f(a,b) called with f(a)
    WrongArity(ArityError),

    VarNotFound(String, Span),
    MethodNotFound(String, Span),

    NotExhaustivePatternMatch(String, Span),
    Generic(String, Span),

    // Move this whole error type out of this module
    Parse(ParseError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnificationError {
    pub msg: String,
    pub expected: Type,
    pub actual: Type,
    pub span: Span,
}

impl UnificationError {
    pub fn stringify(&self) -> String {
        format!(
            "{}
Expected:
    {}
Got:
    {}",
            self.msg, self.expected, self.actual
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArityError {
    pub expected: Vec<Type>,
    pub actual: Vec<Type>,
    pub span: Span,
}

impl ArityError {
    pub fn stringify(&self) -> String {
        let expected = self
            .expected
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<_>>()
            .join(", ");

        let actual = self
            .actual
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "Wrong arity

Expected:
    ({})
Got:
    ({})",
            expected, actual
        )
    }
}

impl Error {
    pub fn from_exhaustive(errors: Vec<exhaustive::Error>, span: Span) -> Self {
        match errors.first().unwrap() {
            exhaustive::Error::Incomplete(_, _, cases) => {
                let case = match cases.first().unwrap() {
                    exhaustive::Pattern::Ctor(_, name, _) => name,
                    exhaustive::Pattern::Literal(_) => todo!(),
                    exhaustive::Pattern::Anything => "_",
                };

                let msg = format!("Pattern match is not exhaustive, missing case: {:#?}", case);
                Self::NotExhaustivePatternMatch(msg, span)
            }
            _ => todo!(),
        }
    }

    pub fn stringify(&self, source_file: Option<&str>) -> String {
        let err = match self.clone() {
            Error::Unification(e) => e.stringify(),
            Error::WrongArity(e) => e.stringify(),
            Error::VarNotFound(_, _) => todo!(),
            Error::MethodNotFound(_, _) => todo!(),
            Error::NotExhaustivePatternMatch(e, _) => e,
            Error::Generic(e, _) => e,
            Error::Parse(e) => e.msg,
        };

        if source_file.is_none() {
            return err;
        }

        let source_file: Vec<_> = source_file.unwrap().split('\n').collect();

        let span = self.get_span();
        let start = span.start.line;
        let hint = format!("{start}|  ");

        let line = source_file.get(start - 1).unwrap();

        let padding: String = " ".repeat(span.start.col + hint.len() - 1);

        let highlight = if span.start.line == span.end.line {
            // single char idents need to repeat at least once
            let count = std::cmp::max(span.end.col - span.start.col, 1);
            "^".repeat(count)
        } else {
            "^--".to_string()
        };

        format!(
            "{err}

{hint}{line}
{padding}{highlight}",
        )
    }

    pub fn get_span(&self) -> Span {
        match self.clone() {
            Error::Unification(e) => e.span,
            Error::WrongArity(e) => e.span,
            Error::VarNotFound(_, _) => todo!(),
            Error::MethodNotFound(_, _) => todo!(),
            Error::NotExhaustivePatternMatch(_, span) => span,
            Error::Generic(_, span) => span,
            Error::Parse(e) => e.span,
        }
    }
}
