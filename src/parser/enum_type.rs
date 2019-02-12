use super::*;

pub fn parse_enum_type<'t, 's: 't>(
    ts: TokensRef<'t, 's>,
    outer_span: Span,
) -> Result<Expr, ParseError> {
    let mut sections = ts.split_symbol(",").rev();

    let last_section = unwrap!(sections.next());

    let mut tail = parse_enum_type_tail(last_section)?;
    for section in sections {
        let head = parse_composite_type_elem(section)?;
        let span = Span {
            start: section.span().start,
            end: tail.span().end,
        };
        tail = Expr::EnumType {
            head,
            tail: Box::new(tail),
            span,
        };
    }
    Ok(tail)
}

fn parse_enum_type_tail<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => return Ok(Expr::NeverType(Span::from(ts.end))),
    };

    if token.kind == TokenKind::Symbol("..") {
        let sub_tokens = ts.slice_from(1);
        return parse_expr(sub_tokens);
    }

    let head = parse_composite_type_elem(ts)?;
    let tail = Box::new(Expr::NeverType(Span::from(ts.end)));
    let span = ts.span();
    Ok(Expr::EnumType { head, tail, span })
}

