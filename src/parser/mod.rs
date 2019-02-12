use super::*;

use crate::lexer::{Span, Token, TokenKind, TokensRef, LexError, TextPos};
use crate::lsp::{Range, Diagnostic};

pub use self::expr::*;
pub use self::pat::*;
pub use self::ident::*;
pub use self::composite_term::*;
pub use self::composite_type::*;
pub use self::struct_term::*;
pub use self::struct_type::*;
pub use self::composite_pat::*;
pub use self::struct_pat::*;
pub use self::enum_type::*;
pub use self::neg_func_term::*;
pub use self::enum_term_or_func::*;

mod ident;
mod expr;
mod pat;
mod composite_term;
mod composite_type;
mod struct_term;
mod struct_type;
mod composite_pat;
mod struct_pat;
mod enum_type;
mod neg_func_term;
mod enum_term_or_func;

static SYMBOL_TABLE: &[&str] = &[";", ":", "#", ",", "=", "..", "=>", "->"];

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "{}", _0)]
    Lex(LexError),
    #[fail(display = "parse error")]
    Parse(Vec<Diagnostic>),
    #[fail(display = "debug: {}", _0)]
    Debug(String),
}

impl From<ParseError> for Result<Vec<Diagnostic>, Error> {
    fn from(parse_error: ParseError) -> Result<Vec<Diagnostic>, Error> {
        match parse_error {
            ParseError::Lex(err) => err.into(),
            ParseError::Parse(diagnostics) => Ok(diagnostics),
            ParseError::Debug(s) => bail!("debug: {}", s),
        }
    }
}

pub fn parse(code: &str) -> Result<Expr, ParseError> {
    let lexed = match lexer::lex(code, SYMBOL_TABLE) {
        Ok(lexed) => lexed,
        Err(e) => return Err(ParseError::Lex(e)),
    };

    debug!("lexed the code. parsing");
    parse_expr(lexed.borrow())
}

