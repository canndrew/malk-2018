use super::*;

pub fn parse_struct_term<'t, 's: 't>(
    ts: TokensRef<'t, 's>,
    outer_span: Span,
) -> Result<Expr, ParseError> {
    let mut sections = ts.split_symbol(",").rev();

    let last_section = unwrap!(sections.next());

    let mut tail = parse_struct_term_tail(last_section)?;
    for section in sections {
        let head = parse_composite_term_elem(section)?;
        let span = Span {
            start: section.span().start,
            end: tail.span().end,
        };
        tail = Expr::PairTerm {
            head,
            tail: Box::new(tail),
            span,
        };
    }
    let ret = tail.respan_bracketed(outer_span);
    Ok(ret)
}

fn parse_struct_term_tail<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => return Ok(Expr::UnitTerm(Span::from(ts.end))),
    };

    if token.kind == TokenKind::Symbol("..") {
        let sub_tokens = ts.slice_from(1);
        return parse_expr(sub_tokens);
    }

    let head = parse_composite_term_elem(ts)?;
    let tail = Box::new(Expr::UnitTerm(Span::from(ts.end)));
    let span = ts.span();
    Ok(Expr::PairTerm { head, tail, span })
}

