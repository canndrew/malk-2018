use super::*;

pub use self::ident::{Ident, parse_ident};
pub use self::struct_type::{StructType};
pub use self::enum_type::{EnumType};
pub use self::struct_term::{UnitTerm, PairTerm, parse_struct_term_inner};
pub use self::enum_term::{EnumTerm, EnumTermKind, parse_enum_term_inner};
pub use self::composite_type::parse_composite_type_inner;
pub use self::composite_type_elem::{CompositeTypeElem, parse_maybe_composite_type_elem,
                                 parse_composite_type_elem};
pub use self::composite_term_elem::{CompositeTermElem, parse_maybe_composite_term_elem,
                                    parse_composite_term_elem};
pub use self::expr::{Expr, ExprKind, parse_expr};
pub use self::func_term::{FuncTerm, SingularFuncTerm, EnumFuncTerm, EnumFuncTermLeft};
pub use self::func_type::{FuncType, SingularFuncType, EnumFuncType, EnumFuncTypeLeft};
pub use self::func_app::{FuncApp};
pub use self::typed::{Typed};
pub use self::type_literal::TypeLiteral;
pub use self::level_literal::LevelLiteral;
pub use self::let_expr::{LetExpr, parse_let_expr};
pub use self::pattern::*;
pub use self::parens::Parens;

use crate::lexer::{LexError, TextPos, TokenKind, Span, TokensRef};
use crate::lsp::{Diagnostic, Range};

mod parens;
mod ident;
mod composite_term_elem;
mod struct_term;
mod composite_type_elem;
mod composite_type;
mod struct_type;
mod enum_type;
mod enum_term;
mod func_term;
mod func_type;
mod func_app;
mod typed;
mod type_literal;
mod level_literal;
mod let_expr;
mod pattern;
mod expr;

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "{}", _0)]
    Lexer(LexError),
    #[fail(display = "expected expression")]
    ExpectedExpression {
        pos: TextPos,
    },
    #[fail(display = "expected token")]
    ExpectedToken {
        pos: TextPos,
    },
    #[fail(display = "unexpected token")]
    UnexpectedToken {
        pos: TextPos,
    },
    #[fail(display = "expected ident")]
    ExpectedIdent {
        pos: TextPos,
    },
    /*
    #[fail(display = "expected enum contents")]
    ExpectedEnumContents {
        pos: TextPos,
    },
    #[fail(display = "invalid variable bind")]
    InvalidVariableBind {
        pos: TextPos,
    },
    */
    #[fail(display = "invalid pattern")]
    InvalidPattern {
        span: Span,
    }
}

impl From<ParseError> for Result<Vec<Diagnostic>, Error> {
    fn from(parse_error: ParseError) -> Result<Vec<Diagnostic>, Error> {
        match parse_error {
            ParseError::Lexer(lex_error) => lex_error.into(),
            ParseError::ExpectedExpression { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("expected expression"),
                    }
                ])
            },
            ParseError::ExpectedToken { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("expected token"),
                    },
                ])
            },
            ParseError::UnexpectedToken { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("unexpected token"),
                    },
                ])
            },
            ParseError::ExpectedIdent { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("expected ident"),
                    },
                ])
            },
            ParseError::InvalidPattern { span } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(span),
                        msg: String::from("invalid pattern"),
                    },
                ])
            },
        }
    }
}

pub const SYMBOL_TABLE: &'static [&'static str] = &[
    ":", ";", ",", "=", "=>", "#", "->", "+",
];

pub fn parse(code: &str) -> Result<Expr, ParseError> {
    let tokens = lexer::lex(code, SYMBOL_TABLE).map_err(ParseError::Lexer)?;
    parse_expr(tokens.borrow())
}

/*
#[cfg(test)]
mod test {
    use super::*;

    use lexer::lex;

    #[test]
    fn unit_term() {
        let srcs = &["{}", " { } "];
        for src in srcs {
            let ts = lex(src, SYMBOL_TABLE).unwrap();
            let expr = parse_expr(ts.borrow()).unwrap();

            let st = match expr.kind {
                ExprKind::StructTerm(st) => *st,
                _ => panic!(),
            };
            assert_eq!(st.head, None);
            assert_eq!(st.head_elems[..], []);
        }
    }

    /*
    #[test]
    fn struct_term() {
        let srcs = &["{x, y = 23}", "{x  , y=23,}", "{{}; x, y = 23}", "{{x,}; y =23, }"];
        for src in srcs {
            let ts = lex(src, SYMBOL_TABLE).unwrap();
            let expr = parse_expr(ts.borrow()).unwrap();

            let st = match expr.kind {
                ExprKind::StructTerm(st) => *st,
                _ => panic!(),
            };
            assert_
        }
    }
    */
}
*/

