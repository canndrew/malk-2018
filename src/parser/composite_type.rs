use super::*;
use lsp::Position;

pub struct CompositeTypeElem {
    name: Option<Ident>,
    expr: Box<Expr>,
}

impl CompositeTypeElem {
    pub fn span(&self) -> Span {
        match self.name {
            None => self.expr.span(),
            Some(ref name) => {
                Span {
                    start: name.span().start,
                    end: self.expr.span().end,
                }
            },
        }
    }

    pub fn span_of_expr_or_pat_at_position(&self, pos: Position) -> Span {
        if self.expr.span().contains_pos(pos) {
            return self.expr.span_of_expr_or_pat_at_position(pos);
        }
        self.span()
    }
}

pub fn parse_composite_type_elem<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<CompositeTypeElem, ParseError>
{
    let mut sections = ts.split_symbol(":");
    
    let section0 = sections.next();
    let section1 = sections.next();
    let section2 = sections.next();

    match (section0, section1, section2) {
        (None, Some(..), _) |
        (None, None, Some(..)) |
        (Some(..), None, Some(..)) => unreachable!(),
        (Some(expr_tokens), None, None) => {
            let expr = parse_expr(expr_tokens)?;
            Ok(CompositeTypeElem {
                name: None,
                expr: Box::new(expr),
            })
        },
        (Some(name_tokens), Some(expr_tokens), None) => {
            let name = parse_ident(name_tokens)?;
            let expr = parse_expr(expr_tokens)?;
            Ok(CompositeTypeElem {
                name: Some(name),
                expr: Box::new(expr),
            })
        },
        (None, None, None) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(ts.end),
                    msg: String::from("expected composite type element"),
                },
            ]))
        },
        (Some(tokens0), Some(tokens1), Some(_)) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(tokens1.end),
                    msg: String::from("unexpected second colon symbol in composite type element"),
                },
                Diagnostic {
                    range: Range::from(tokens0.end),
                    msg: String::from("position of first colon"),
                },
            ]))
        },
    }
}


