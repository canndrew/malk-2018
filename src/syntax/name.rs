use crate::parser::Ast;
use crate::syntax::Ident;

#[derive(Clone)]
pub struct Name {
    pub ident: Ast<Ident>,
    pub bumps: u32,
}

