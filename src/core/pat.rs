use super::*;
use crate::parser::{Ast, Origin};
use crate::core::{Ident, Term, Bump};
use lsp_types::{Range, Position};

#[derive(Clone)]
pub enum Pat {
    Unit,
    Bind(Ast<String>),
    Pair {
        name: Option<Ast<String>>,
        head: Ast<Pat>,
        tail: Ast<Pat>,
    },
    ProjLeft {
        ident: Option<Ast<Ident>>,
        left: Ast<Pat>,
    },
    ProjRight {
        right: Ast<Pat>,
    },
    Typed {
        pat: Ast<Pat>,
        ty: Ast<Term>,
    },
}

impl Ast<Pat> {
    pub fn fold_mut<A: Bump>(
        &mut self,
        args: &mut A,
        folder: &impl Fn(&mut Ast<Term>, &A, bool) -> bool,
    ) -> bool {
        let mut children_changed = false;
        match &mut *self.node {
            Pat::Unit => (),
            Pat::Bind(name) => {
                *args = args.bump_over_name(&name.node);
            },
            Pat::Pair { head, tail, .. } => {
                children_changed |= head.fold_mut(args, folder);
                children_changed |= tail.fold_mut(args, folder);
            },
            Pat::ProjLeft { left, .. } => {
                children_changed |= left.fold_mut(args, folder);
            },
            Pat::ProjRight { right } => {
                children_changed |= right.fold_mut(args, folder);
            },
            Pat::Typed { pat, ty } => {
                children_changed |= ty.fold_mut(args, folder);
                children_changed |= pat.fold_mut(args, folder);
            },
        }
        children_changed
    }

    pub fn app_at_position(&self, position: Position) -> Option<&Ast<Term>> {
        if let Origin::Document { range, .. } = self.origin {
            if !range.contains(position) {
                return None;
            }
        }

        match &*self.node {
            Pat::Unit |
            Pat::Bind(..) => (),
            Pat::Pair { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Pat::ProjLeft { left, .. } => {
                if let Some(app) = left.app_at_position(position) {
                    return Some(app);
                }
            },
            Pat::ProjRight { right } => {
                if let Some(app) = right.app_at_position(position) {
                    return Some(app);
                }
            },
            Pat::Typed { pat, ty } => {
                if let Some(app) = pat.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = ty.app_at_position(position) {
                    return Some(app);
                }
            },
        }
        None
    }
}

