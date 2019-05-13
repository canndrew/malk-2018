use super::*;
use crate::syntax::{Ident, IdentOpt, Name, NameOpt};
use pretty_assertions::{assert_eq, assert_ne};

#[derive(Clone)]
pub struct Ctx {
    inner: Rc<CtxInner>,
}

impl PartialEq for Ctx {
    fn eq(&self, other: &Ctx) -> bool {
        self.inner.kind == other.inner.kind
    }
}

impl fmt::Debug for Ctx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner.kind, f)
    }
}

struct CtxInner {
    kind: CtxKind,
    hash: u64,
}

#[derive(PartialEq, Debug)]
enum CtxKind {
    Nil,
    Var {
        parent: Ctx,
        ident: IdentOpt,
        ty: Type,
    },
}

impl Ctx {
    pub fn debug_summary(&self) -> String {
        let mut ret = self.debug_summary_inner();
        ret.push_str(" ⊦");
        ret
    }

    fn kind(&self) -> &CtxKind {
        &self.inner.kind
    }

    fn debug_summary_inner(&self) -> String {
        match self.kind() {
            CtxKind::Nil => String::from("·"),
            CtxKind::Var { parent, ident, .. } => {
                let mut ret = parent.debug_summary_inner();
                ret.push_str(", ");
                match ident {
                    IdentOpt::Fake(s) => {
                        ret.push_str("(");
                        ret.push_str(s);
                        ret.push_str(")");
                    },
                    IdentOpt::Real(Ident(s)) => {
                        ret.push_str(s);
                    },
                }
                ret
            },
        }
    }

    pub fn empty() -> Ctx {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Nil");
            hasher.finish()
        };
        Ctx {
            inner: Rc::new(CtxInner {
                kind: CtxKind::Nil,
                hash,
            }),
        }
    }

    pub fn bind(&self, ident: &IdentOpt, ty: &Type) -> Ctx {
        assert_eq!(*self, ty.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Var");
            if let IdentOpt::Real(ident) = ident {
                hasher.write(ident.as_str().as_bytes());
            }
            hasher.write_u64(ty.get_hash());
            hasher.finish()
        };
        let parent = self.clone();
        let ident = ident.clone();
        let ty = ty.clone();
        Ctx {
            inner: Rc::new(CtxInner {
                kind: CtxKind::Var { parent, ident, ty },
                hash,
            }),
        }
    }

    pub fn parent(&self) -> Ctx {
        match self.kind() {
            CtxKind::Nil => panic!("ctx has no parent"),
            CtxKind::Var { parent, .. } => {
                parent.clone()
            },
        }
    }

    pub fn unbind(&self) -> (Ctx, IdentOpt, Type) {
        match self.kind() {
            CtxKind::Nil => panic!("ctx is empty!"),
            CtxKind::Var { parent, ident, ty } => {
                (parent.clone(), ident.clone(), ty.clone())
            },
        }
    }

    pub fn bump(&self, index: u32, bump_ident_opt: &IdentOpt, bump_ty: &Type) -> Ctx {
        assert_eq!(self.nth_parent(index), bump_ty.get_ctx());
        if index == 0 {
            Ctx::bind(self, bump_ident_opt, bump_ty)
        } else {
            match self.kind() {
                CtxKind::Nil => panic!("invalid bump"),
                CtxKind::Var { parent, ident, ty } => {
                    let parent = parent.bump(index - 1, bump_ident_opt, bump_ty);
                    let ty = ty.bump_ctx(index - 1, bump_ident_opt, bump_ty);
                    Ctx::bind(&parent, ident, &ty)
                },
            }
        }
    }

    pub fn substitute(
        &self,
        subst_index: u32,
        subst_ident_opt: &IdentOpt,
        subst_value: &Term,
    ) -> Ctx {
        match self.kind() {
            CtxKind::Nil => panic!("invalid substitution on context"),
            CtxKind::Var { parent, ident, ty } => {
                if subst_index == 0 {
                    assert_eq!(subst_ident_opt, ident);
                    assert_eq!(*ty, subst_value.get_type());
                    assert_eq!(*parent, subst_value.get_ctx());
                    parent.clone()
                } else {
                    let parent = parent.substitute(subst_index - 1, subst_ident_opt, subst_value);
                    let ty = ty.substitute(subst_index - 1, subst_ident_opt, subst_value);
                    Ctx::bind(&parent, ident, &ty)
                }
            },
        }
    }

    pub fn try_lift(&self, cutoff: u32, lift_bumps: u32) -> Option<Ctx> {
        if cutoff == 0 {
            Some(self.nth_parent(lift_bumps))
        } else {
            match self.kind() {
                CtxKind::Nil => panic!("invalid lifting operation"),
                CtxKind::Var { parent, ident, ty } => {
                    let parent = parent.try_lift(cutoff - 1, lift_bumps)?;
                    let ty = ty.try_lift_out_of_ctx(cutoff - 1, lift_bumps)?;
                    Some(parent.bind(ident, &ty))
                },
            }
        }
    }

    /*
    pub fn shift_ctx(&self, index: u32, bumps: u32, ident_opt: &IdentOpt, shift_type: &Type) -> Ctx {
        let shifted_arg_type
        match self.kind() {
            CtxKind::Nil => panic!("invalid context shift operation"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    assert_eq!(ident, ident_opt);
                    assert_eq!(ty, shift_type);

                }
            },
        }
    }
    */

    /*
    pub fn lookup_name(&self, index: u32) -> (NameOpt, Type) {
        match self.kind() {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    (NameOpt::new(ident.clone(), 0), ty.clone())
                } else {
                    let (name, ty) = parent.lookup_name(index - 1);
                    let name = name.bump_over_ident_opt(ident);
                    (name, ty)
                }
            },
        }
    }
    */

    // returns the ident and unbumped type at the requested de brujin index
    pub fn lookup_ident_opt(&self, index: u32) -> (IdentOpt, Type) {
        match self.kind() {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    (ident.clone(), ty.clone())
                } else {
                    parent.lookup_ident_opt(index - 1)
                }
            },
        }
    }

    pub fn lookup_and_bump_out(&self, index: u32, name_opt: &NameOpt) -> Type {
        match self.kind() {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, ident, ty } => {
                if index == 0 {
                    let var_ident = name_opt.clone().unwrap_as_ident();
                    assert_eq!(var_ident, *ident);
                    ty.clone()
                } else {
                    let name_opt = name_opt.unbump_over_ident_opt(ident);
                    parent
                    .lookup_and_bump_out(index - 1, &name_opt)
                }
                .bump_ctx(0, ident, ty)
            },
        }
    }

    pub fn try_lookup(&self, name: &Name) -> Option<(u32, Type)> {
        match self.kind() {
            CtxKind::Nil => None,
            CtxKind::Var { parent, ident, ty } => {
                if name.bumps() == 0 && IdentOpt::from(name.ident()) == *ident {
                    Some((0, ty.clone()))
                } else {
                    let name = name.unbump_over_ident_opt(ident);
                    let (index, ret_ty) = parent.try_lookup(&name)?;
                    let ret_ty = ret_ty.bump_ctx(0, ident, ty);
                    Some((index + 1, ret_ty))
                }
            },
        }
    }

    pub fn nth_parent(&self, n: u32) -> Ctx {
        if n == 0 {
            self.clone()
        } else {
            self.parent().nth_parent(n - 1)
        }
    }
}

