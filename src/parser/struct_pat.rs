use super::*;

pub fn parse_struct_pat<'t, 's: 't>(
    ts: TokensRef<'t, 's>,
    outer_span: Span,
) -> Result<Pat, ParseError> {
    let mut sections = ts.split_symbol(",").rev();

    let last_section = unwrap!(sections.next());

    let mut tail = parse_struct_pat_tail(last_section)?;
    for section in sections {
        let head = parse_composite_pat_elem(section)?;
        let span = Span {
            start: section.span().start,
            end: tail.span().end,
        };
        tail = Pat::Pair {
            head,
            tail: Box::new(tail),
            span,
        };
    }
    let ret = tail.respan_bracketed(outer_span);
    Ok(ret)
}

fn parse_struct_pat_tail<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<Pat, ParseError>
{
    let ts = ts.trim_whitespace();
    let mut tokens = ts.iter();

    let token = match tokens.next() {
        Some(token) => token,
        None => return Ok(Pat::Unit(Span::from(ts.end))),
    };

    if token.kind == TokenKind::Symbol("..") {
        let sub_tokens = ts.slice_from(1);
        return parse_pat(sub_tokens);
    }

    let head = parse_composite_pat_elem(ts)?;
    let tail = Box::new(Pat::Unit(Span::from(ts.end)));
    let span = ts.span();
    Ok(Pat::Pair { head, tail, span })
}

