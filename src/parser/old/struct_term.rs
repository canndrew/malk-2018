use super::*;

pub struct UnitTerm {
    pub span: Span,
}

pub struct PairTerm {
    pub head: CompositeTermElem,
    pub tail: Expr,
    pub span: Span,
}

pub fn parse_struct_term_tail<'t, 's>(ts: TokensRef<'t, 's>)
    -> Result<Expr, ParseError>
{
    let ts = ts.trim_whitespace();
    {
        let mut tokens = ts.tokens.iter();
        let next_two = (tokens.next().map(|t| t.kind), tokens.next().map(|t| t.kind));
        if let (Some(TokenKind::Symbol(".")), Some(TokenKind::Symbol("."))) = next_two {
            return parse_expr(ts.range_from(2));
        }
    }

    if ts.tokens.len() == 0 {
        return Ok(Expr {
            kind: ExprKind::UnitTerm,
            span: Span { start: ts.end, end: ts.end },
        });
    }

    let head = parse_composite_term_elem(ts)?;
    let span = Span {
        start: head.span.start,
        end: ts.end,
    };
    Ok(Expr {
        kind: ExprKind::PairTerm(PairTerm {
            head: head,
            tail: Expr { kind: ExprKind::UnitTerm, span: Span::from(ts.end) },
            span,
        }),
        span: span,
    })
}

pub fn parse_struct_term_inner<'t, 's>(ts: TokensRef<'t, 's>)
    -> Result<ExprKind, ParseError>
{
    let (mut elems, tail_tokens) = ts.split_rev(",");

    let mut tail = Expr {
        kind: parse_struct_term_tail(tail_tokens)?,
        span: ..,
    };

    for elem_tokens in elems {
        let head = parse_composite_term_elem(elem_tokens)?;
        let span = Span {
            start: head.span.start,
            end: tail.span.end,
        };
        tail = Expr::PairTerm(PairTerm {
            head,
            tail,
            span,
        });
    }

    Ok(tail)
}

/*
#[derive(Debug, Clone, PartialEq)]
pub struct StructTerm {
    pub head_elems: Vec<CompositeTermElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn parse_struct_term_inner<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<(Vec<CompositeTermElem>, Option<Expr>), ParseError> {
    let mut head_elems = Vec::new();
    let mut elem_start = 0;
    for (i, token) in ts.tokens.iter().enumerate() {
        match token.kind {
            TokenKind::Symbol(",") => {
                let head_tokens = ts.range(elem_start, i);
                let head = parse_composite_term_elem(head_tokens)?;
                head_elems.push(head);
                elem_start = i + 1;
            },
            TokenKind::Symbol(";") => {
                let head_tokens = ts.range(elem_start, i);
                let head = parse_maybe_composite_term_elem(head_tokens)?;
                head_elems.extend(head);

                let tail_tokens = ts.range_from(i + 1);
                let tail = parse_expr(tail_tokens)?;
                return Ok((head_elems, Some(tail)));
            },
            _ => (),
        }
    }
    // Didn't find a semicolon. There is no tail elem.
    let head_tokens = ts.range_from(elem_start);
    let head = parse_maybe_composite_term_elem(head_tokens)?;
    head_elems.extend(head);
    Ok((head_elems, None))
}

impl fmt::Display for StructTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        if let Some((first, rest)) = self.head_elems.split_first() {
            write!(f, "{}", first)?;
            for elem in rest {
                write!(f, ", {}", elem)?;
            }
        }
        if let Some(ref tail) = self.tail {
            write!(f, "; {}", tail)?;
        }
        write!(f, "}}")
    }
}
*/

