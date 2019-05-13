use crate::core::Ident;
use crate::parser::Ast;

pub trait Bump: Clone {
    fn bump_over_name(&self, name: &str) -> Self;
    
    fn bump_over_name_opt(&self, name: &Option<Ast<String>>) -> Self {
        if let Some(name) = name.as_ref().map(|ast| &ast.node) {
            self.bump_over_name(name)
        } else {
            self.clone()
        }
    }
}

/*
impl<T> Bump for Ast<T>
where
    T: Bump,
{
    fn bump_over_name(&self, name: &Ident) -> Ast<T> {
        Ast {
            node: Box::new(self.node.bump_over_name(name)),
            origin: self.origin.clone(),
        }
    }
}
*/

