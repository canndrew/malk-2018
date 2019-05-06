use crate::parser::Ast;
use crate::syntax::Ident;

#[derive(PartialEq, Clone, Debug)]
pub struct Name {
    pub ident: Ident,
    pub bumps: u32,
}

impl Name {
    pub fn new(ident: Ident, bumps: u32) -> Name {
        Name { ident, bumps }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn bumps(&self) -> u32 {
        self.bumps
    }

    pub fn into_ident(self) -> Ident {
        assert_eq!(self.bumps, 0);
        self.ident
    }

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

    pub fn bump_over_ident_opt(&self, ident: Option<&Ident>) -> Name {
        match ident {
            Some(ident) => self.bump_over_ident(ident),
            None => self.clone(),
        }
    }

    pub fn unbump_over_ident_opt(&self, ident: Option<&Ident>) -> Name {
        match ident {
            Some(ident) => self.bump_over_ident(ident),
            None => self.clone(),
        }
    }

    pub fn unbump(&self) -> Name {
        Name {
            ident: self.ident.clone(),
            bumps: self.bumps - 1,
        }
    }
}

/*
pub struct NameOpt {
    inner: Option<Name>,
}

impl NameOpt {
    pub fn ident(&self) -> IdentOpt {
        match self.inner {
            Some(name) => Some(name.ident()),
            None => None,
        }
    }

    pub fn into_ident(self) -> IdentOpt {
        match self.inner {
            Some(name) => Some(name.into_ident()),
            None => None,
        }
    }

    pub fn bump_over_ident(&self, ident: &Ident) -> Option<Name> {
        match self {
            Some(name) => Some(name.bump_over_ident(ident)),
            None => None,
        }
    }

    pub fn bump_over_ident_opt(&self, ident: Option<&Ident>) -> Option<Name> {
        match self {
            Some(name) => Some(name.bump_over_ident_opt(ident)),
            None => None,
        }
    }

    pub fn unbump_over_ident(&self, ident: &Ident) -> Option<Name> {
        match self {
            Some(name) => Some(name.unbump_over_ident(ident)),
            None => None,
        }
    }

    pub fn unbump_over_ident_opt(&self, ident: Option<&Ident>) -> Option<Name> {
        match self {
            Some(name) => Some(name.unbump_over_ident_opt(ident)),
            None => None,
        }
    }
}
*/

