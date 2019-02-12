use super::*;

pub struct Ident {
    name: String,
    span: Span,
}

impl Ident {
    pub fn new(name: &str, span: Span) -> Ident {
        Ident {
            name: name.to_owned(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

pub fn parse_ident<'t, 's: 't>(ts: TokensRef<'t, 's>)
    -> Result<Ident, ParseError>
{
    let mut tokens = ts.trim_whitespace().iter();

    match (tokens.next(), tokens.next()) {
        (None, Some(..)) => unreachable!(),
        (Some(token), None) => {
            let span = ts.span_of(0);
            match token.kind {
                TokenKind::Ident(name) => {
                    Ok(Ident::new(name, span))
                },
                _ => {
                    Err(ParseError::Parse(vec![
                        Diagnostic {
                            range: span.into(),
                            msg: String::from("expected ident, got something else"),
                        },
                    ]))
                }
            }
        },
        (None, None) => {
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: Range::from(ts.end),
                    msg: String::from("missing ident"),
                },
            ]))
        },
        (Some(..), Some(..)) => {
            let span0 = ts.span_of(0);
            let span1 = ts.span_of(1);
            Err(ParseError::Parse(vec![
                Diagnostic {
                    range: span0.into(),
                    msg: format!("expected single ident"),
                },
                Diagnostic {
                    range: span1.into(),
                    msg: format!("unexpected second token"),
                },
            ]))
        },
    }
}

