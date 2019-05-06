use super::*;
use crate::syntax::{Ident, Name};

#[derive(PartialEq, Debug, Clone)]
pub struct Ctx {
    inner: Rc<CtxKind>,
}

#[derive(PartialEq, Debug)]
enum CtxKind {
    Nil,
    Var {
        parent: Ctx,
        ident: Option<Ident>,
        ty: Type,
    },
}

impl Ctx {
    pub fn empty() -> Ctx {
        Ctx {
            inner: Rc::new(CtxKind::Nil),
        }
    }

    pub fn bind(&self, ident: Option<Ident>, ty: Type) -> Ctx {
        assert_eq!(*self, ty.get_ctx());
        let parent = self.clone();
        Ctx {
            inner: Rc::new(CtxKind::Var {
                parent,
                ident,
                ty,
            }),
        }
    }

    pub fn bump(&self, index: u32, name: Option<&Name>, bump_ty: Type) -> Ctx {
        if index == 0 {
            let bump_ident = name.cloned().map(Name::into_ident);
            Ctx::bind(self, bump_ident, bump_ty)
        } else {
            match &*self.inner {
                CtxKind::Nil => panic!("invalid bump"),
                CtxKind::Var { parent, ident, ty } => {
                    let name = name.map(|n| n.unbump_over_ident_opt(ident.as_ref()));
                    let parent = parent.bump(index - 1, name.as_ref(), bump_ty.clone());
                    let ty = ty.bump_ctx(index - 1, name.as_ref(), bump_ty.clone());
                    let ident = ident.clone();
                    Ctx::bind(&parent, ident, ty)
                },
            }
        }
    }

    pub fn substitute(&self, subst_index: u32, subst_var_name: Option<&Name>, subst_value: Term) -> Ctx {
        match &*self.inner {
            CtxKind::Nil => panic!("invalid substitution on context"),
            CtxKind::Var { parent, ident, ty } => {
                if subst_index == 0 {
                    let subst_ident = subst_var_name.cloned().map(|n| n.into_ident());
                    assert_eq!(subst_ident, *ident);
                    assert_eq!(*ty, subst_value.get_type());
                    parent.clone()
                } else {
                    let subst_var_name = subst_var_name.map(|n| n.unbump_over_ident_opt(ident.as_ref()));
                    let parent = parent.substitute(subst_index - 1, subst_var_name.as_ref(), subst_value.clone());
                    let ty = ty.substitute(subst_index - 1, subst_var_name.as_ref(), subst_value.clone());
                    let ident = ident.clone();
                    Ctx::bind(&parent, ident, ty)
                }
            },
        }
    }

    pub fn lookup_name(&self, index: u32) -> (Option<Name>, Type) {
        match &*self.inner {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    match ident {
                        Some(ident) => (Some(Name::new(ident.clone(), 0)), ty.clone()),
                        None => (None, ty.clone()),
                    }
                } else {
                    let (name, ty) = parent.lookup_name(index - 1);
                    let name = match name {
                        Some(name) => Some(name.bump_over_ident_opt(ident.as_ref())),
                        None => None,
                    };
                    (name, ty)
                }
            },
        }
    }

    pub fn lookup(&self, index: u32, name_opt: Option<&Name>) -> Type {
        match &*self.inner {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    let var_ident = name_opt.cloned().map(|n| n.into_ident());
                    assert_eq!(var_ident, *ident);
                    ty.clone()
                } else {
                    let name_opt = name_opt.map(|n| n.unbump_over_ident_opt(ident.as_ref()));
                    parent.lookup(index - 1, name_opt.as_ref())
                }
            },
        }
    }

    pub fn try_lookup(&self, name: &Name) -> Option<(u32, Type)> {
        match &*self.inner {
            CtxKind::Nil => None,
            CtxKind::Var { parent, ident, ty } => {
                if name.bumps() == 0 && Some(name.ident()) == ident.as_ref() {
                    Some((0, ty.clone()))
                } else {
                    let name = name.unbump_over_ident_opt(ident.as_ref());
                    let (index, ret_ty) = parent.try_lookup(&name)?;
                    let bump_name = match ident {
                        Some(ident) => Some(Name::new(ident.clone(), 0)),
                        None => None,
                    };
                    let ret_ty = ret_ty.bump_ctx(0, bump_name.as_ref(), ty.clone());
                    Some((index + 1, ret_ty))
                }
            },
        }
    }
}

