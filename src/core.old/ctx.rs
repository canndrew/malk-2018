use super::*;
use crate::parser::{Ast, Origin};

pub enum Ctx<'c> {
    Nil,
    Var {
        name: Ast<String>,
        ty: Ast<Term>,
        tail: &'c Ctx<'c>,
    },
}

impl<'c> Ctx<'c> {
    pub fn lookup(&'c self, ident: Ast<IdentRef<'_>>) -> Option<&'c Ast<Term>> {
        match self {
            Ctx::Nil => None,
            Ctx::Var { name, ty, tail } => {
                if &**name.node == ident.node.name {
                    match ident.unbump() {
                        None => Some(ty),
                        Some(ident) => tail.lookup(ident),
                    }
                } else {
                    tail.lookup(ident)
                }
            },
        }
    }
}

