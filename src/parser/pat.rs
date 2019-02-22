use super::*;
use lsp_types::Position;

pub enum Pat {
    Parens(Span, Box<Pat>),
    Var(Ident),
    Unit(Span),
    Pair {
        head: CompositePatElem,
        tail: Box<Pat>,
        span: Span,
    },
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Parens(span, _) => *span,
            Pat::Var(ident) => ident.span(),
            Pat::Unit(span) => *span,
            Pat::Pair { span, .. } => *span,
        }
    }

    pub fn span_of_expr_or_pat_at_position(&self, pos: Position) -> Span {
        match self {
            Pat::Parens(span, pat) => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Pat::Var(ident) => ident.span(),
            Pat::Unit(span) => *span,
            Pat::Pair { head, tail, span } => {
                if head.span().contains_pos(pos) {
                    return head.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
        }
    }

    pub fn respan_bracketed(self, outer_span: Span) -> Pat {
        match self {
            Pat::Parens(_, pat) => Pat::Parens(outer_span, pat),
            Pat::Unit(..) => Pat::Unit(outer_span),
            Pat::Pair { head, tail, .. } => Pat::Pair { head, tail, span: outer_span },
            _ => self,
        }
    }
}

pub fn parse_pat<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Pat, ParseError> {
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();
    
    let token = match tokens.next() {
        Some(token) => token,
        None => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: ts.span().into(),
                message: String::from("expected pattern"),
                code: None,
                severity: None,
                source: None,
                related_information: None,
            },
        ])),
    };

    let span = ts.span_of(0);
    let ret = match &token.kind {
        TokenKind::Ident(name) => Pat::Var(Ident::new(name, span)),
        TokenKind::Whitespace(..) => unreachable!(),
        TokenKind::Symbol(s) => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: span.into(),
                message: format!("unexpected symbol at start of pattern '{}'", s),
                code: None,
                severity: None,
                source: None,
                related_information: None,
            },
        ])),
        TokenKind::Bracket('(', sub_tokens) => {
            Pat::Parens(span, Box::new(parse_pat(sub_tokens.borrow())?))
        },
        TokenKind::Bracket('{', sub_tokens) => {
            parse_struct_pat(sub_tokens.borrow(), span)?
        },
        TokenKind::Bracket(bracket_char, _) => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: Range::from(token.start),
                message: format!("unexpected bracket kind for pattern '{}'", bracket_char),
                code: None,
                severity: None,
                source: None,
                related_information: None,
            },
        ])),
        TokenKind::String(..) => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: span.into(),
                message: format!("strings in patterns not implemented"),
                code: None,
                severity: None,
                source: None,
                related_information: None,
            },
        ])),
        TokenKind::Numeric(..) => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: span.into(),
                message: format!("numbers in patterns not implemented"),
                code: None,
                severity: None,
                source: None,
                related_information: None,
            },
        ])),
    };

    Ok(ret)
}

