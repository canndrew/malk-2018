use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeTermElem {
    pub ident: Option<Ident>,
    pub expr: Expr,
    pub span: Span,
}

pub fn parse_maybe_composite_term_elem<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Option<CompositeTermElem>, ParseError> {
    if ts.tokens.iter().all(|t| t.is_whitespace()) {
        return Ok(None)
    }

    let composite_term_elem = parse_composite_term_elem(ts)?;
    Ok(Some(composite_term_elem))
}

pub fn parse_composite_term_elem<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<CompositeTermElem, ParseError> {
    for (i, token) in ts.tokens.iter().enumerate() {
        // See if there's a '=' in there.
        match token.kind {
            TokenKind::Symbol("=") => {
                let (ident_tokens, expr_tokens) = ts.split_around(i);

                let ident = parse_ident(ident_tokens)?;
                let expr = parse_expr(expr_tokens)?;
                let span = Span {
                    start: ident.span.start,
                    end: expr.span.end,
                };
                return Ok(CompositeTermElem {
                    ident: Some(ident),
                    expr: expr,
                    span: span,
                });
            },
            _ => (),
        }
    }
    // Didn't find an '=', must just be an expression
    let expr = parse_expr(ts)?;
    let span = expr.span;
    return Ok(CompositeTermElem {
        ident: None,
        expr: expr,
        span: span,
    });
}

impl fmt::Display for CompositeTermElem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref ident) = self.ident {
            write!(f, "{} = ", ident)?;
        }
        write!(f, "{}", self.expr)
    }
}

