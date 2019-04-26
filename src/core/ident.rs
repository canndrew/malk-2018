use crate::parser::Ast;

#[derive(Clone)]
pub struct Ident {
    pub name: String,
    pub bumps: u32,
}

#[derive(PartialEq, Clone, Copy)]
pub struct IdentRef<'s> {
    pub name: &'s str,
    pub bumps: u32,
}

impl Ident {
    pub fn as_ref(&self) -> IdentRef<'_> {
        IdentRef {
            name: &self.name[..],
            bumps: self.bumps,
        }
    }
}

impl<'s> Ast<IdentRef<'s>> {
    pub fn unbump(mut self) -> Option<Ast<IdentRef<'s>>> {
        if self.bumps == 0 {
            return None;
        }
        
        self.node.bumps -= 1;
        Some(self)
    }
}

impl<'s> IdentRef<'s> {
    pub fn to_ident(self) -> Ident {
        Ident {
            name: self.name.to_owned(),
            bumps: self.bumps,
        }
    }
}

/*
impl<'s> Bump for IdentRef<'s> {
    fn bump_over_name(&self, name: &str) -> IdentRef<'s> {
        if self.name == name {
            return IdentRef {
                name: self.name,
                bumps: self.bumps + 1,
            };
        }
        *self
    }
}

impl<'s> Bump for Ast<IdentRef<'s>> {
    fn bump_over_name(&self, name: &str) -> Ast<IdentRef<'s>> {
        Ast {
            node: Box::new(self.node.bump_over_name(name)),
            origin: self.origin.clone(),
        }
    }
}
*/

