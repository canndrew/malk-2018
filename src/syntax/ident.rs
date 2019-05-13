use crate::parser::Ast;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Ident(pub String);

#[derive(Clone, Eq, Debug)]
pub enum IdentOpt {
    Real(Ident),
    Fake(&'static str),
}

impl From<Ident> for IdentOpt {
    fn from(ident: Ident) -> IdentOpt {
        IdentOpt::Real(ident)
    }
}

impl IdentOpt {
    pub fn fake(s: &'static str) -> IdentOpt {
        IdentOpt::Fake(s)
    }
}

impl PartialEq for IdentOpt {
    fn eq(&self, other: &IdentOpt) -> bool {
        match (self, other) {
            (IdentOpt::Real(ident0), IdentOpt::Real(ident1)) => ident0 == ident1,
            (IdentOpt::Fake(_), IdentOpt::Fake(_)) => true,
            _ => false,
        }
    }
}

impl Ident {
    pub fn as_str(&self) -> &str {
        let Ident(s) = self;
        s
    }
}

