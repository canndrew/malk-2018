pub use super::*;
use crate::syntax::{Ident, Name};

#[derive(PartialEq, Clone, Debug)]
pub struct Term {
    inner: Rc<TermInner>,
}

#[derive(PartialEq, Debug)]
struct TermInner {
    kind: TermKind,
    ty: Type,
}

#[derive(PartialEq, Debug)]
pub enum TermKind {
    Type {
        level: u32,
    },
    Var {
        index: u32,
        name_opt: Option<Name>,
    },
    EqualType {
        x0: Term,
        x1: Term,
    },
    UnitType,
    NeverType,
    PairType {
        head_ident_opt: Option<Ident>,
        head_type: Type,
        tail_type: Type,
    },
    FuncType {
        arg_type: Type,
        res_type: Type,
    },
    Refl {
        x: Term,
    },
    J {
        target_type: Type,
        target: Term,
        elim: Term,
    },
    Abort {
        target_type: Type,
        elim: Term,
    },
    Unit,
    Pair {
        head_ident_opt: Option<Ident>,
        head: Term,
        tail: Term,
    },
    Func {
        arg_type: Type,
        res: Term,
    },
    App {
        func: Term,
        arg: Term,
    },
}

impl Term {
    pub fn get_ctx(&self) -> Ctx {
        self.get_type().get_ctx()
    }

    pub fn get_type(&self) -> Type {
        self.inner.ty.clone()
    }

    pub fn kind(&self) -> &TermKind {
        &self.inner.kind
    }

    pub fn bump_ctx(&self, bump_index: u32, bump_name: Option<&Name>, bump_ty: Type) -> Term {
        let ctx = self.get_ctx().bump(bump_index, bump_name, bump_ty.clone());
        match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name_opt } => {
                if *index >= bump_index {
                    let name_opt = name_opt.clone().map(|n| n.bump_over_ident_opt(bump_name.map(Name::ident)));
                    Term::var(&ctx, *index + 1, name_opt)
                } else {
                    Term::var(&ctx, *index, name_opt.clone())
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let x1 = x1.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::equal_type(&ctx, x0, x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                let head_type = head_type.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let bump_name = bump_name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail_type = tail_type.bump_ctx(bump_index + 1, bump_name.as_ref(), bump_ty.clone());
                Term::pair_type(&ctx, head_ident_opt.clone(), head_type, tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                let arg_type = arg_type.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let res_type = res_type.bump_ctx(bump_index + 1, bump_name, bump_ty.clone());
                Term::func_type(&ctx, arg_type, res_type)
            },
            TermKind::Refl { x } => {
                let x = x.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::refl(&ctx, x)
            }
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let target = target.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let elim = elim.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::j(&ctx, target_type, target, elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let elim = elim.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::abort(&ctx, target_type, elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_ident_opt, head, tail } => {
                let head = head.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let bump_name = bump_name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail = tail.bump_ctx(bump_index + 1, bump_name.as_ref(), bump_ty.clone());
                Term::pair(&ctx, head_ident_opt.clone(), head, tail)
            },
            TermKind::Func { arg_type, res } => {
                let arg_type = arg_type.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let res = res.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::func(&ctx, arg_type, res)
            },
            TermKind::App { func, arg } => {
                let func = func.bump_ctx(bump_index, bump_name, bump_ty.clone());
                let arg = arg.bump_ctx(bump_index, bump_name, bump_ty.clone());
                Term::app(&ctx, func, arg)
            },
        }
    }

    pub fn substitute(&self, subst_index: u32, subst_var_name: Option<&Name>, subst_value: Term) -> Term {
        let ctx = self.get_ctx().substitute(subst_index, subst_var_name, subst_value.clone());
        match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name_opt } => {
                if *index == subst_index {
                    assert_eq!(name_opt.as_ref(), subst_var_name);
                    subst_value.clone()
                } else if *index > subst_index {
                    let name_opt = name_opt.clone().map(|n| n.unbump_over_ident_opt(subst_var_name.map(|n| n.ident())));
                    Term::var(&ctx, *index - 1, name_opt)
                } else {
                    Term::var(&ctx, *index, name_opt.clone())
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.substitute(subst_index, subst_var_name, subst_value.clone());
                let x1 = x1.substitute(subst_index, subst_var_name, subst_value.clone());
                Term::equal_type(&ctx, x0, x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                let head_type = head_type.substitute(subst_index, subst_var_name, subst_value.clone());
                let subst_var_name = subst_var_name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail_type = tail_type.substitute(subst_index + 1, subst_var_name.as_ref(), subst_value.clone());
                Term::pair_type(&ctx, head_ident_opt.clone(), head_type, tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                let arg_type = arg_type.substitute(subst_index, subst_var_name, subst_value.clone());
                let res_type = res_type.substitute(subst_index + 1, subst_var_name, subst_value.clone());
                Term::func_type(&ctx, arg_type, res_type)
            },
            TermKind::Refl { x } => {
                let x = x.substitute(subst_index, subst_var_name, subst_value.clone());
                Term::refl(&ctx, x)
            },
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.substitute(subst_index, subst_var_name, subst_value.clone());
                let target = target.substitute(subst_index, subst_var_name, subst_value.clone());
                let elim = elim.substitute(subst_index, subst_var_name, subst_value.clone());
                Term::j(&ctx, target_type, target, elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.substitute(subst_index, subst_var_name, subst_value.clone());
                let elim = elim.substitute(subst_index, subst_var_name, subst_value.clone());
                Term::abort(&ctx, target_type, elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_ident_opt, head, tail } => {
                let head = head.substitute(subst_index, subst_var_name, subst_value.clone());
                let subst_var_name = subst_var_name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail = tail.substitute(subst_index + 1, subst_var_name.as_ref(), subst_value.clone());
                Term::pair(&ctx, head_ident_opt.clone(), head, tail)
            },
            TermKind::Func { arg_type, res } => {
                let arg_type = arg_type.substitute(subst_index, subst_var_name, subst_value.clone());
                let res = res.substitute(subst_index + 1, subst_var_name, subst_value.clone());
                Term::func(&ctx, arg_type, res)
            },
            TermKind::App { func, arg } => {
                let func = func.substitute(subst_index, subst_var_name, subst_value.clone());
                let arg = arg.substitute(subst_index, subst_var_name, subst_value.clone());
                Term::app(&ctx, func, arg)
            },
        }
    }

    pub fn ty(ctx: &Ctx, level: u32) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Type { level },
                ty: Type::ty(ctx, level + 1),
            }),
        }
    }

    pub fn var(ctx: &Ctx, index: u32, name_opt: Option<Name>) -> Term {
        let ty = ctx.lookup(index, name_opt.as_ref());
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Var { index, name_opt },
                ty: ty,
            }),
        }
    }

    pub fn equal_type(ctx: &Ctx, x0: Term, x1: Term) -> Term {
        assert_eq!(x0.get_type(), x1.get_type());
        let level = x0.get_type().get_level();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::EqualType { x0, x1 },
                ty: Type::ty(ctx, level),
            }),
        }
    }

    pub fn unit_type(ctx: &Ctx) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::UnitType,
                ty: Type::ty(ctx, 0),
            }),
        }
    }

    pub fn never_type(ctx: &Ctx) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::NeverType,
                ty: Type::ty(ctx, 0),
            }),
        }
    }

    pub fn pair_type(ctx: &Ctx, head_ident_opt: Option<Ident>, head_type: Type, tail_type: Type) -> Term {
        let head_level = head_type.get_level();
        let tail_level = tail_type.get_level();
        let level = cmp::max(head_level, tail_level);
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::PairType {
                    head_ident_opt,
                    head_type,
                    tail_type,
                },
                ty: Type::ty(ctx, level),
            }),
        }
    }

    pub fn func_type(ctx: &Ctx, arg_type: Type, res_type: Type) -> Term {
        let arg_level = arg_type.get_level();
        let res_level = res_type.get_level();
        let level = cmp::max(arg_level, res_level);
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::FuncType {
                    arg_type,
                    res_type,
                },
                ty: Type::ty(ctx, level),
            }),
        }
    }

    pub fn refl(ctx: &Ctx, x: Term) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Refl { x: x.clone() },
                ty: Type::equal(ctx, x.clone(), x.clone()),
            }),
        }
    }

    pub fn j(ctx: &Ctx, target_type: Type, target: Term, elim: Term) -> Term {
        let (target_arg_name, target_arg_type) = target.get_ctx().lookup_name(0);
        let (target_type_x0_name, _target_type_x0_type) = target_type.get_ctx().lookup_name(2);
        let (target_type_x1_name, _target_type_x1_type) = target_type.get_ctx().lookup_name(1);
        let (target_type_equality_name, _target_type_equality_type) = target_type.get_ctx().lookup_name(0);

        let bumped_target_arg_name = {
            target_arg_name
            .clone()
            .map(|n| n.bump_over_ident_opt(target_type_x0_name.clone().map(|n| n.ident().clone()).as_ref()))
            .map(|n| n.bump_over_ident_opt(target_type_x1_name.clone().map(|n| n.ident().clone()).as_ref()))
            .map(|n| n.bump_over_ident_opt(target_type_equality_name.clone().map(|n| n.ident().clone()).as_ref()))
        };

        let expected_ty = {
            target_type
            .clone()
            .bump_ctx(3, bumped_target_arg_name.as_ref(), target_arg_type)
            .substitute(2, target_type_x0_name.as_ref(), Term::var(ctx, 0, target_arg_name.clone()))
            .substitute(1, target_type_x1_name.as_ref(), Term::var(ctx, 0, target_arg_name.clone()))
            .substitute(0, target_type_equality_name.as_ref(), Term::refl(ctx, Term::var(ctx, 0, target_arg_name)))
        };
        assert_eq!(expected_ty, target.get_type());

        let (a0, a1) = match elim.get_type().kind() {
            TypeKind::Equal { x0, x1 } => (x0.clone(), x1.clone()),
            _ => panic!("elim is not an equality"),
        };
        let final_ty = {
            target_type
            .clone()
            .substitute(2, target_type_x0_name.as_ref(), a0)
            .substitute(1, target_type_x1_name.as_ref(), a1)
            .substitute(0, target_type_equality_name.as_ref(), elim.clone())
        };
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::J { target_type, target, elim },
                ty: final_ty,
            }),
        }
    }

    pub fn abort(ctx: &Ctx, target_type: Type, elim: Term) -> Term {
        assert_eq!(elim.get_type(), Type::never(ctx));
        let (target_type_arg_name, target_type_arg_type) = target_type.get_ctx().lookup_name(0);
        assert_eq!(target_type_arg_type, Type::never(ctx));
        let final_type = target_type.substitute(0, target_type_arg_name.as_ref(), elim.clone());
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Abort { target_type, elim },
                ty: final_type,
            })
        }
    }

    pub fn unit(ctx: &Ctx) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Unit,
                ty: Type::unit(ctx),
            }),
        }
    }

    pub fn pair(ctx: &Ctx, head_ident_opt: Option<Ident>, head: Term, tail: Term) -> Term {
        assert_eq!(*ctx, head.get_ctx());
        assert_eq!(Ctx::bind(ctx, head_ident_opt.clone(), head.get_type()), tail.get_ctx());
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Pair { head_ident_opt: head_ident_opt.clone(), head: head.clone(), tail: tail.clone() },
                ty: Type::pair(ctx, head_ident_opt, head.get_type(), tail.get_type()),
            }),
        }
    }

    pub fn func(ctx: &Ctx, arg_type: Type, res: Term) -> Term {
        assert_eq!(*ctx, arg_type.get_ctx());
        assert_eq!(Ctx::bind(ctx, None, arg_type.clone()), res.get_ctx());
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Func { arg_type: arg_type.clone(), res: res.clone() },
                ty: Type::func(ctx, arg_type, res.get_type()),
            }),
        }
    }

    pub fn app(ctx: &Ctx, func: Term, arg: Term) -> Term {
        assert_eq!(*ctx, func.get_ctx());
        assert_eq!(*ctx, arg.get_ctx());

        let (arg_type, res_type) = match func.get_type().kind() {
            TypeKind::Func { arg, res } => (arg.clone(), res.clone()),
            _ => panic!("invalid type for function application"),
        };

        assert_eq!(arg_type, arg.get_type());
        let final_type = res_type.substitute(0, None, arg.clone());

        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::App { func, arg },
                ty: final_type,
            }),
        }
    }
}

