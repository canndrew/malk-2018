use super::*;

pub fn parse_neg_func_term<'t, 's: 't>(
    pat_tokens: TokensRef<'t, 's>,
    body_tokens: TokensRef<'t, 's>,
) -> Result<Expr, ParseError> {
    let pat = Box::new(parse_pat(pat_tokens)?);
    let body = Box::new(parse_expr(body_tokens)?);
    let span = Span {
        start: pat.span().start,
        end: body.span().end,
    };
    Ok(Expr::NegFuncTerm { pat, body, span })
}

