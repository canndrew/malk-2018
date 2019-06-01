use super::*;
use crate::parser::Ast;
use crate::syntax::Ident;

#[derive(Clone)]
pub struct Name {
    pub ident: Ast<Ident>,
    pub bumps: u32,
}

impl Name {
    /*
    pub fn new(ident: &Ident, bumps: u32) -> Name {
        let ident = ident.clone();
        Name { ident, bumps }
    }
    */

    pub fn ident(&self) -> Ident {
        (*self.ident).clone()
    }

    pub fn bumps(&self) -> u32 {
        self.bumps
    }

    /*
    pub fn bump_over_ident(&self, ident: &Ident) -> Name {
        if self.ident == *ident {
            Name {
                ident: self.ident.clone(),
                bumps: self.bumps + 1,
            }
        } else {
            self.clone()
        }
    }

    pub fn unbump_over_ident(&self, ident: &Ident) -> Name {
        if self.ident == *ident {
            Name {
                ident: self.ident.clone(),
                bumps: self.bumps - 1,
            }
        } else {
            self.clone()
        }
    }

    pub fn unbump(&self) -> Name {
        Name {
            ident: self.ident.clone(),
            bumps: self.bumps - 1,
        }
    }
    */
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.bumps {
            write!(f, "^")?;
        }
        write!(f, "{}", *self.ident)
    }
}

