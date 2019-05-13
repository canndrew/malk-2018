use crate::parser::Ast;
use crate::syntax::{Ident, IdentOpt};

#[derive(PartialEq, Clone, Debug)]
pub struct Name {
    pub ident: Ident,
    pub bumps: u32,
}

impl Name {
    pub fn new(ident: &Ident, bumps: u32) -> Name {
        let ident = ident.clone();
        Name { ident, bumps }
    }

    pub fn ident(&self) -> Ident {
        self.ident.clone()
    }

    pub fn bumps(&self) -> u32 {
        self.bumps
    }

    pub fn unwrap_as_ident(self) -> Ident {
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

    pub fn bump_over_ident_opt(&self, ident_opt: &IdentOpt) -> Name {
        match ident_opt {
            IdentOpt::Real(ident) => self.bump_over_ident(ident),
            IdentOpt::Fake(_) => self.clone(),
        }
    }

    pub fn unbump_over_ident_opt(&self, ident_opt: &IdentOpt) -> Name {
        match ident_opt {
            IdentOpt::Real(ident) => self.bump_over_ident(ident),
            IdentOpt::Fake(_) => self.clone(),
        }
    }

    pub fn unbump(&self) -> Name {
        Name {
            ident: self.ident.clone(),
            bumps: self.bumps - 1,
        }
    }
}

#[derive(Clone, Debug)]
pub enum NameOpt {
    Real(Name),
    Fake(&'static str),
}

impl PartialEq for NameOpt {
    fn eq(&self, other: &NameOpt) -> bool {
        match (self, other) {
            (NameOpt::Real(name0), NameOpt::Real(name1)) => name0 == name1,
            (NameOpt::Fake(_), NameOpt::Fake(_)) => true,
            _ => false,
        }
    }
}

impl NameOpt {
    pub fn new(ident_opt: &IdentOpt, bumps: u32) -> NameOpt {
        match ident_opt {
            IdentOpt::Real(ident) => {
                NameOpt::Real(Name::new(ident, bumps))
            },
            IdentOpt::Fake(s) => {
                NameOpt::Fake(s)
            },
        }
    }

    pub fn fake(s: &'static str) -> NameOpt {
        NameOpt::Fake(s)
    }

    pub fn ident(&self) -> IdentOpt {
        match self {
            NameOpt::Real(name) => IdentOpt::from(name.ident()),
            NameOpt::Fake(s) => IdentOpt::fake(s),
        }
    }

    pub fn unwrap_as_ident(self) -> IdentOpt {
        match self {
            NameOpt::Real(name) => IdentOpt::from(name.unwrap_as_ident()),
            NameOpt::Fake(s) => IdentOpt::fake(s),
        }
    }

    pub fn bump_over_ident(&self, ident: &Ident) -> NameOpt {
        match self {
            NameOpt::Real(name) => NameOpt::Real(name.bump_over_ident(ident)),
            NameOpt::Fake(s) => NameOpt::Fake(s),
        }
    }

    pub fn bump_over_ident_opt(&self, ident_opt: &IdentOpt) -> NameOpt {
        match self {
            NameOpt::Real(name) => NameOpt::Real(name.bump_over_ident_opt(ident_opt)),
            NameOpt::Fake(s) => NameOpt::Fake(s),
        }
    }

    pub fn unbump_over_ident(&self, ident: &Ident) -> NameOpt {
        match self {
            NameOpt::Real(name) => NameOpt::Real(name.unbump_over_ident(ident)),
            NameOpt::Fake(s) => NameOpt::Fake(s),
        }
    }

    pub fn unbump_over_ident_opt(&self, ident_opt: &IdentOpt) -> NameOpt {
        match self {
            NameOpt::Real(name) => NameOpt::Real(name.unbump_over_ident_opt(ident_opt)),
            NameOpt::Fake(s) => NameOpt::Fake(s),
        }
    }
}

impl From<Name> for NameOpt {
    fn from(name: Name) -> NameOpt {
        NameOpt::Real(name)
    }
}

