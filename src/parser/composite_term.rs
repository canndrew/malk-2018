use super::*;
use lsp_types::Position;

pub struct CompositeTermElem {
    pub name: Option<Ident>,
    pub expr: Box<Expr>,
}

impl CompositeTermElem {
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

pub fn parse_composite_term_elem<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<CompositeTermElem, ParseError>
{
    debug!("parsing a composite term elem");
    let mut sections = ts.split_symbol("=");
    
    let section0 = sections.next();
    let section1 = sections.next();
    let section2 = sections.next();

    match (section0, section1, section2) {
        (None, _, _) |
        (Some(..), None, Some(..)) => unreachable!(),
        (Some(expr_tokens), None, None) => {
            let expr = parse_expr(expr_tokens)?;
            Ok(CompositeTermElem {
                name: None,
                expr: Box::new(expr),
            })
        },
        (Some(name_tokens), Some(expr_tokens), None) => {
            debug!("split around the = sign");
            let name = parse_ident(name_tokens)?;
            debug!("parsed the ident");
            let expr = parse_expr(expr_tokens)?;
            debug!("all parsed now");
            Ok(CompositeTermElem {
                name: Some(name),
                expr: Box::new(expr),
            })
        },
        (Some(tokens0), Some(tokens1), Some(_)) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(tokens1.end),
                    message: String::from("unexpected second equals symbol in composite term element"),
                    code: None,
                    source: None,
                    severity: None,
                    related_information: None,
                },
                Diagnostic {
                    range: Range::from(tokens0.end),
                    message: String::from("position of first equals"),
                    code: None,
                    source: None,
                    severity: None,
                    related_information: None,
                },
            ]))
        },
    }
}

