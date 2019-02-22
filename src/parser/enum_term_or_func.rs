use super::*;

pub fn parse_enum_term_or_func<'t, 's: 't>(
    ts: TokensRef<'t, 's>,
    outer_span: Span,
) -> Result<Expr, ParseError> {
    let mut comma_sections = ts.split_symbol(",");

    let section0 = comma_sections.next();
    let section1 = comma_sections.next();
    let section2 = comma_sections.next();

    let only_tokens = match (section0, section1, section2) {
        (Some(only_tokens), None, None) => only_tokens,
        (Some(only_tokens), Some(empty_tokens), None)
            if empty_tokens.trim_whitespace().iter().count() == 0 => only_tokens,
        _ => return parse_enum_func(ts, outer_span),
    };
    let only_tokens = only_tokens.trim_whitespace();

    match only_tokens.iter().next() {
        Some(token) => match token.kind {
            TokenKind::Symbol("..") => {
                let expr = parse_under_let(only_tokens.slice_from(1))?;
                return Ok(Expr::EnumRight {
                    expr: Box::new(expr),
                    span: outer_span,
                });
            },
            _ => (),
        },
        None => return parse_enum_func(ts, outer_span),
    }

    let (mut arrow_sections, _) = only_tokens.split_symbols_rev(&["=>", "->"]);
    if arrow_sections.count() != 0 {
        return parse_enum_func(ts, outer_span);
    }

    let elem = parse_composite_term_elem(only_tokens)?;
    Ok(Expr::EnumLeft {
        elem,
        span: outer_span,
    })
}

pub fn parse_enum_func<'t, 's: 't>(
    ts: TokensRef<'t, 's>,
    outer_span: Span,
) -> Result<Expr, ParseError> {
    let mut sections = ts.split_symbol(",").rev();

    let last_section = unwrap!(sections.next());

    let mut tail = parse_enum_func_tail(last_section)?;
    for section in sections {
        tail = parse_enum_func_head(section, Box::new(tail))?;
    }

    Ok(tail.respan_bracketed(outer_span))
}

pub fn parse_enum_func_tail<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<Expr, ParseError>
{
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => return Ok(Expr::NeverFunc(Span::from(ts.end))),
    };

    if token.kind == TokenKind::Symbol("..") {
        let sub_tokens = ts.slice_from(1);
        return parse_expr(sub_tokens);
    }

    parse_enum_func_head(ts, Box::new(Expr::NeverFunc(Span::from(ts.end))))
}

pub fn parse_enum_func_head<'t, 's: 't>(ts: TokensRef<'t, 's>, tail: Box<Expr>)
    -> Result<Expr, ParseError>
{
    match ts.split_next_symbols(&["=>", "->"]) {
        None => return Err(ParseError::Parse(vec![
            Diagnostic {
                range: ts.span().into(),
                message: format!("expected '=>' or '->'"),
                code: None,
                source: None,
                severity: None,
                related_information: None,
            },
        ])),
        Some((pat_tokens, arrow, body_tokens)) => {
            let pat_elem = parse_composite_pat_elem(pat_tokens)?;
            let body = parse_under_let(body_tokens)?;
            let span = Span {
                start: pat_tokens.span().start,
                end: tail.span().end,
            };
            Ok(match arrow {
                "=>" => Expr::EnumFuncTerm {
                    pat: Box::new(pat_elem),
                    body: Box::new(body),
                    tail,
                    span,
                },
                "->" => Expr::EnumFuncType {
                    pat: Box::new(pat_elem),
                    body: Box::new(body),
                    tail,
                    span,
                },
                _ => unreachable!(),
            })
        },
    }
}

