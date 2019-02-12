use super::*;
use crate::lsp::Position;

pub struct CompositePatElem {
    name: Option<Ident>,
    pat: Box<Pat>,
}

impl CompositePatElem {
    pub fn span(&self) -> Span {
        match self.name {
            None => self.pat.span(),
            Some(ref name) => {
                Span {
                    start: name.span().start,
                    end: self.pat.span().end,
                }
            },
        }
    }

    pub fn span_of_expr_or_pat_at_position(&self, pos: Position) -> Span {
        if self.pat.span().contains_pos(pos) {
            return self.pat.span_of_expr_or_pat_at_position(pos);
        }
        self.span()
    }
}

pub fn parse_composite_pat_elem<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<CompositePatElem, ParseError>
{
    let mut sections = ts.split_symbol("=");
    
    let section0 = sections.next();
    let section1 = sections.next();
    let section2 = sections.next();

    match (section0, section1, section2) {
        (None, Some(..), _) |
        (None, None, Some(..)) |
        (Some(..), None, Some(..)) => unreachable!(),
        (Some(pat_tokens), None, None) => {
            let pat = parse_pat(pat_tokens)?;
            Ok(CompositePatElem {
                name: None,
                pat: Box::new(pat),
            })
        },
        (Some(name_tokens), Some(pat_tokens), None) => {
            let name = parse_ident(name_tokens)?;
            let pat = parse_pat(pat_tokens)?;
            Ok(CompositePatElem {
                name: Some(name),
                pat: Box::new(pat),
            })
        },
        (None, None, None) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(ts.end),
                    msg: String::from("expected composite pattern element"),
                },
            ]))
        },
        (Some(tokens0), Some(tokens1), Some(_)) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(tokens1.end),
                    msg: String::from("unexpected second equals symbol in composite pattern element"),
                },
                Diagnostic {
                    range: Range::from(tokens0.end),
                    msg: String::from("position of first equals"),
                },
            ]))
        },
    }
}


