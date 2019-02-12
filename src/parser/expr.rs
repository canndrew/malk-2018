use super::*;
use crate::lsp::Position;

pub enum Expr {
    Let {
        span: Span,
        pat: Box<Pat>,
        expr: Box<Expr>,
        body: Box<Expr>,
    },
    Parens(Span, Box<Expr>),
    Var(Ident),
    UnitTerm(Span),
    UnitType(Span),
    PairTerm {
        head: CompositeTermElem,
        tail: Box<Expr>,
        span: Span,
    },
    PairType {
        head: CompositeTypeElem,
        tail: Box<Expr>,
        span: Span,
    },
    NeverType(Span),
    EnumType {
        head: CompositeTypeElem,
        tail: Box<Expr>,
        span: Span,
    },
    NegFuncTerm {
        pat: Box<Pat>,
        body: Box<Expr>,
        span: Span,
    },
    NegFuncType {
        pat: Box<Pat>,
        body: Box<Expr>,
        span: Span,
    },
    Number(Ident),
    String(Ident),
    EnumLeft {
        elem: CompositeTermElem,
        span: Span,
    },
    EnumRight {
        expr: Box<Expr>,
        span: Span,
    },
    EnumFuncTerm {
        pat: Box<CompositePatElem>,
        body: Box<Expr>,
        tail: Box<Expr>,
        span: Span,
    },
    EnumFuncType {
        pat: Box<CompositePatElem>,
        body: Box<Expr>,
        tail: Box<Expr>,
        span: Span,
    },
    NeverFunc(Span),
    App {
        func: Box<Expr>,
        arg: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Let { span, .. } => *span,
            Expr::Parens(span, _) => *span,
            Expr::Var(ident) => ident.span(),
            Expr::UnitTerm(span) => *span,
            Expr::UnitType(span) => *span,
            Expr::PairTerm { span, .. } => *span,
            Expr::PairType { span, .. } => *span,
            Expr::NeverType(span) => *span,
            Expr::EnumType { span, .. } => *span,
            Expr::NegFuncTerm { span, .. } => *span,
            Expr::NegFuncType { span, .. } => *span,
            Expr::Number(ident) => ident.span(),
            Expr::String(ident) => ident.span(),
            Expr::EnumLeft { span, .. } => *span,
            Expr::EnumRight { span, .. } => *span,
            Expr::EnumFuncTerm { span, .. } => *span,
            Expr::EnumFuncType { span, .. } => *span,
            Expr::NeverFunc(span) => *span,
            Expr::App { span, .. } => *span,
        }
    }

    pub fn span_of_expr_or_pat_at_position(&self, pos: Position) -> Span {
        match self {
            Expr::Let { span, pat, expr, body } => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                if expr.span().contains_pos(pos) {
                    return expr.span_of_expr_or_pat_at_position(pos);
                }
                if body.span().contains_pos(pos) {
                    return body.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::Parens(span, expr) => {
                if expr.span().contains_pos(pos) {
                    return expr.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::Var(ident) => {
                ident.span()
            },
            Expr::UnitTerm(span) => *span,
            Expr::UnitType(span) => *span,
            Expr::PairTerm { head, tail, span } => {
                if head.span().contains_pos(pos) {
                    return head.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::PairType { head, tail, span } => {
                if head.span().contains_pos(pos) {
                    return head.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::NeverType(span) => *span,
            Expr::EnumType { head, tail, span } => {
                if head.span().contains_pos(pos) {
                    return head.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::NegFuncTerm { pat, body, span } => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                if body.span().contains_pos(pos) {
                    return body.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::NegFuncType { pat, body, span } => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                if body.span().contains_pos(pos) {
                    return body.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::Number(ident) => ident.span(),
            Expr::String(ident) => ident.span(),
            Expr::EnumLeft { elem, span } => {
                if elem.span().contains_pos(pos) {
                    return elem.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::EnumRight { expr, span } => {
                if expr.span().contains_pos(pos) {
                    return expr.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::EnumFuncTerm { pat, body, tail, span } => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                if body.span().contains_pos(pos) {
                    return body.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::EnumFuncType { pat, body, tail, span } => {
                if pat.span().contains_pos(pos) {
                    return pat.span_of_expr_or_pat_at_position(pos);
                }
                if body.span().contains_pos(pos) {
                    return body.span_of_expr_or_pat_at_position(pos);
                }
                if tail.span().contains_pos(pos) {
                    return tail.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
            Expr::NeverFunc(span) => *span,
            Expr::App { func, arg, span } => {
                if func.span().contains_pos(pos) {
                    return func.span_of_expr_or_pat_at_position(pos);
                }
                if arg.span().contains_pos(pos) {
                    return arg.span_of_expr_or_pat_at_position(pos);
                }
                *span
            },
        }
    }
}

pub fn parse_expr<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    let mut sections = ts.split_symbol(";").rev();
    let last_section = unwrap!(sections.next());

    let mut body = parse_under_let(last_section)?;
    for section in sections {
        let (pat, expr) = parse_let_expr(section)?;
        let span = Span {
            start: section.span().start,
            end: body.span().end,
        };
        body = Expr::Let {
            pat: Box::new(pat),
            expr: Box::new(expr),
            body: Box::new(body),
            span,
        };
    }

    Ok(body)
}

pub fn parse_let_expr<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<(Pat, Expr), ParseError> {
    let start_pos = ts.pos_of(0);

    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => {
            let pat = Pat::Unit(ts.span());
            let expr = Expr::UnitTerm(ts.span());
            return Ok((pat, expr));
        },
    };

    if token.kind == TokenKind::Ident("let") {
        let mut sections = ts.slice_from(1).split_symbol("=");
        let section0 = sections.next();
        let section1 = sections.next();
        let section2 = sections.next();

        match (section0, section1, section2) {
            (None, _, _) |
            (Some(..), None, Some(..)) => unreachable!(),
            (Some(..), None, None) => return Err(ParseError::Parse(vec![
                Diagnostic {
                    range: ts.span().into(),
                    msg: format!("expected '=' after let"),
                },
            ])),
            (Some(..), Some(tokens1), Some(..)) => return Err(ParseError::Parse(vec![
                Diagnostic {
                    range: tokens1.end.into(),
                    msg: format!("unexpected '='"),
                },
            ])),
            (Some(pat_tokens), Some(expr_tokens), None) => {
                let pat = parse_pat(pat_tokens)?;
                let expr = parse_under_let(expr_tokens)?;
                return Ok((pat, expr));
            },
        }
    }

    let pat = Pat::Unit(start_pos.into());
    let expr = parse_under_let(ts)?;
    Ok((pat, expr))
}

pub fn parse_under_let<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    let (mut sections, last_section) = ts.split_symbols_rev(&["=>", "->"]);

    let mut body = parse_under_func(last_section)?;
    for (sym, section) in sections {
        match sym {
            "=>" => {
                let pat = parse_pat(section)?;
                let span = Span {
                    start: pat.span().start,
                    end: body.span().end,
                };
                body = Expr::NegFuncTerm {
                    pat: Box::new(pat),
                    body: Box::new(body),
                    span,
                };
            },
            "->" => {
                let pat = parse_pat(section)?;
                let span = Span {
                    start: pat.span().start,
                    end: body.span().end,
                };
                body = Expr::NegFuncType {
                    pat: Box::new(pat),
                    body: Box::new(body),
                    span,
                };
            },
            _ => unreachable!(),
        }
    }

    Ok(body)
}

pub fn parse_under_func<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    let (mut left, mut ts) = match parse_single_token(ts)? {
        Some(x) => x,
        None => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: ts.span().into(),
                msg: format!("expected expression"),
            },
        ])),
    };

    loop {
        let (right, new_ts) = match parse_single_token(ts)? {
            Some(x) => x,
            None => break,
        };
        let span = Span {
            start: left.span().start,
            end: right.span().end,
        };
        ts = new_ts;
        left = Expr::App {
            func: Box::new(left),
            arg: Box::new(right),
            span,
        };
    }

    Ok(left)
}

pub fn parse_single_token<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<Option<(Expr, TokensRef<'t, 's>)>, ParseError>
{
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => return Ok(None),
    };

    let span = ts.span_of(0);
    let ret = match &token.kind {
        TokenKind::Ident(name) => Expr::Var(Ident::new(name, span)),
        TokenKind::Whitespace(..) => unreachable!(),
        TokenKind::Symbol(s) => {
            match *s {
                "#" => {
                    match tokens.next() {
                        Some(next_token) => match &next_token.kind {
                            TokenKind::Bracket('{', sub_tokens) => {
                                parse_struct_type(sub_tokens.borrow(), span)?
                            },
                            TokenKind::Bracket('[', sub_tokens) => {
                                parse_enum_type(sub_tokens.borrow(), span)?
                            },
                            _ => return Err(ParseError::Parse(vec![
                                Diagnostic {
                                    range: ts.span_of(1).into(),
                                    msg: format!("unexpected token following '#'"),
                                },
                            ])),
                        },
                        None => {
                            return Err(ParseError::Parse(vec![
                                Diagnostic {
                                    range: span.into(),
                                    msg: format!("expected something following '#'"),
                                },
                            ]));
                        },
                    }
                },
                _ => {
                    return Err(ParseError::Parse(vec![
                        Diagnostic {
                            range: span.into(),
                            msg: format!("unexpected symbol at start of expression '{}'", s),
                        },
                    ]))
                },
            }
        },
        TokenKind::Bracket('(', sub_tokens) => {
            Expr::Parens(span, Box::new(parse_expr(sub_tokens.borrow())?))
        },
        TokenKind::Bracket('{', sub_tokens) => {
            parse_struct_term(sub_tokens.borrow(), span)?
        },
        TokenKind::Bracket('[', sub_tokens) => {
            parse_enum_term_or_func(sub_tokens.borrow(), span)?
        },
        TokenKind::Bracket(bracket_char, _) => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: Range::from(token.start),
                msg: format!("unexpected bracket kind for expression '{}'", bracket_char),
            },
        ])),
        TokenKind::String(s) => Expr::String(Ident::new(&s, span)),
        TokenKind::Numeric(s) => Expr::Number(Ident::new(s, span)),
    };

    Ok(Some((ret, ts.slice_from(1))))
}


