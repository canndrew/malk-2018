use std::sync::Arc;
use std::ops::Deref;
use lsp_types::{Range, Position, Diagnostic, DiagnosticSeverity, Url};
use crate::syntax::Expr;

grammar_macro::grammar! {
    mod grammar;
}

#[derive(Clone)]
pub enum Origin {
    Document {
        uri: Arc<Url>,
        range: Range,
    },
    Substitute {
        subject: Box<Origin>,
        variable: Box<Origin>,
        value: Box<Origin>,
    },
    TypeOf(Box<Origin>),
    InjLeftOuter {
        depth: u32,
        inner: Box<Origin>,
    },
}

#[derive(Clone)]
pub struct Ast<T: ?Sized> {
    pub node: Box<T>,
    pub origin: Origin,
}

impl<T> Deref for Ast<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T> From<(T, Origin)> for Ast<T> {
    fn from((node, origin): (T, Origin)) -> Ast<T> {
        Ast::new(node, origin)
    }
}

impl<T> From<(Ast<T>, Origin)> for Ast<T> {
    fn from((ast, _origin): (Ast<T>, Origin)) -> Ast<T> {
        ast
    }
}

impl<T> PartialEq<Ast<T>> for Ast<T>
where
    T: PartialEq<T>,
{
    fn eq(&self, other: &Ast<T>) -> bool {
        self.node.eq(&other.node)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Parser<'s> {
    s: &'s str,
    byte_pos: usize,
    line: usize,
    character: usize,
}

impl<T> Ast<T> {
    fn new(node: T, origin: Origin) -> Ast<T> {
        let node = Box::new(node);
        Ast {
            node, origin,
        }
    }

    fn into_box(self) -> Box<T> {
        self.node
    }

    fn into_inner(self) -> T {
        *self.node
    }
}

impl<'s> Parser<'s> {
    fn position(&self) -> Position {
        Position {
            line: self.line as u64,
            character: self.character as u64,
        }
    }

    fn advance(mut self, bytes: usize) -> Parser<'s> {
        log::trace!("advancing {:#?}", self);
        let mut chars = self.s[self.byte_pos .. (self.byte_pos + bytes)].chars().peekable();
        loop {
            match chars.next() {
                None => break,
                Some('\r') => {
                    match chars.peek() {
                        None => {
                            if let Some('\n') = self.s[(self.byte_pos + bytes)..].chars().next() {
                                panic!("can't advance into middle of \"\\r\\n\" line-break.");
                            }
                            break;
                        },
                        Some('\n') => (),
                        _ => {
                            self.line += 1;
                            self.character = 0;
                        },
                    }
                },
                Some('\n') => {
                    self.line += 1;
                    self.character = 0;
                },
                Some(c) => {
                    let mut units = [0; 2];
                    let units = c.encode_utf16(&mut units[..]);
                    self.character += units.len();
                },
            }
        }
        self.byte_pos += bytes;
        self
    }

    fn rest(&self) -> &'s str {
        &self.s[self.byte_pos..]
    }
}

pub fn parse_doc(uri: &Arc<Url>, text: &str) -> Result<Ast<Expr>, Diagnostic> {
    let p = Parser {
        s: text,
        byte_pos: 0,
        line: 0,
        character: 0,
    };

    let (term, p) = grammar::parse_any_term(uri, p)?;
    let p = grammar::skip_whitespace(uri, p);
    if p.byte_pos != text.len() {
        return Err(Diagnostic {
            code: None,
            related_information: None,
            source: None,
            severity: Some(DiagnosticSeverity::Error),
            range: Range {
                start: p.position(),
                end: p.position(),
            },
            message: String::from("expected end of input"),
        });
    }
    Ok(term)
}

