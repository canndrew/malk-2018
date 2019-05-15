pub use super::*;
use crate::syntax::{Ident, IdentOpt, Name, NameOpt};
use pretty_assertions::{assert_eq, assert_ne};

#[derive(Clone)]
pub struct Term {
    inner: Rc<TermInner>,
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

#[derive(Debug)]
struct TermInner {
    kind: TermKind,
    ty: Type,
    hash: u64,
}

impl Hash for TermInner {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash.hash(hasher)
    }
}

#[derive(Debug)]
pub enum TermKind {
    Type {
        level: u32,
    },
    Var {
        index: u32,
        name_opt: NameOpt,
    },
    EqualType {
        x0: Term,
        x1: Term,
    },
    UnitType,
    NeverType,
    PairType {
        head_ident_opt: IdentOpt,
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
        head_ident_opt: IdentOpt,
        head: Term,
        tail: Term,
    },
    PairSplit {
        target_type: Type,
        target: Term,
        elim: Term,
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

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        match (self.kind(), other.kind()) {
            (TermKind::Type { level: level0 }, TermKind::Type { level: level1 }) => {
                level0 == level1
            },
            (TermKind::Var { index: index0, name_opt: name_opt0 },
             TermKind::Var { index: index1, name_opt: name_opt1 }) => {
                index0 == index1 &&
                name_opt0 == name_opt1
            },
            (TermKind::EqualType { x0: x00, x1: x10 },
             TermKind::EqualType { x0: x01, x1: x11 }) => {
                x00 == x01 &&
                x10 == x11
            },
            (TermKind::UnitType, TermKind::UnitType) => true,
            (TermKind::NeverType, TermKind::NeverType) => true,
            (TermKind::PairType { head_ident_opt: head_ident_opt0, head_type: head_type0, tail_type: tail_type0 },
             TermKind::PairType { head_ident_opt: head_ident_opt1, head_type: head_type1, tail_type: tail_type1 }) => {
                head_ident_opt0 == head_ident_opt1 &&
                head_type0 == head_type1 &&
                tail_type0 == tail_type1
            },
            (TermKind::FuncType { arg_type: arg_type0, res_type: res_type0 },
             TermKind::FuncType { arg_type: arg_type1, res_type: res_type1 }) => {
                arg_type0 == arg_type1 &&
                res_type0 == res_type1
            },
            (TermKind::Refl { x: x0 }, TermKind::Refl { x: x1 }) => {
                x0 == x1
            },
            (TermKind::J { target_type: target_type0, target: target0, elim: elim0 },
             TermKind::J { target_type: target_type1, target: target1, elim: elim1 }) => {
                target_type0 == target_type1 &&
                target0 == target1 &&
                elim0 == elim1
            },
            (TermKind::Abort { target_type: target_type0, elim: elim0 },
             TermKind::Abort { target_type: target_type1, elim: elim1 }) => {
                target_type0 == target_type1 &&
                elim0 == elim1
            },
            (TermKind::Unit, TermKind::Unit) => true,
            (TermKind::Pair { head_ident_opt: head_ident_opt0, head: head0, tail: tail0 },
             TermKind::Pair { head_ident_opt: head_ident_opt1, head: head1, tail: tail1 }) => {
                head_ident_opt0 == head_ident_opt1 &&
                head0 == head1 &&
                tail0 == tail1
            },
            (TermKind::PairSplit { target_type: target_type0, target: target0, elim: elim0 },
             TermKind::PairSplit { target_type: target_type1, target: target1, elim: elim1 }) => {
                target_type0 == target_type1 &&
                target0 == target1 &&
                elim0 == elim1
            },
            (TermKind::Func { arg_type: arg_type0, res: res0 },
             TermKind::Func { arg_type: arg_type1, res: res1 }) => {
                arg_type0 == arg_type1 &&
                res0 == res1
            },
            (TermKind::App { func: func0, arg: arg0 },
             TermKind::App { func: func1, arg: arg1 }) => {
                func0 == func1 &&
                arg0 == arg1
            },
            _ => {
                match (self.reduce_head(), other.reduce_head()) {
                    (None, None) => false,
                    (self_reduced, other_reduced) => {
                        let self_reduced = self_reduced.unwrap_or(self.clone());
                        let other_reduced = other_reduced.unwrap_or(self.clone());
                        self_reduced == other_reduced
                    },
                }
            },
        }
    }
}

impl Term {
    pub fn get_ctx(&self) -> Ctx {
        self.get_type().get_ctx()
    }

    pub fn get_type(&self) -> Type {
        self.inner.ty.clone()
    }

    pub fn get_hash(&self) -> u64 {
        self.inner.hash
    }

    pub fn kind(&self) -> &TermKind {
        &self.inner.kind
    }

    pub fn bump_ctx(&self, bump_index: u32, bump_ident_opt: &IdentOpt, bump_ty: &Type) -> Term {
        assert_eq!(self.get_ctx().nth_parent(bump_index), bump_ty.get_ctx());
        let ctx = self.get_ctx().bump(bump_index, bump_ident_opt, bump_ty);
        match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name_opt } => {
                if *index >= bump_index {
                    let name_opt = name_opt.bump_over_ident_opt(&bump_ident_opt);
                    Term::var(&ctx, *index + 1, &name_opt)
                } else {
                    Term::var(&ctx, *index, name_opt)
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let x1 = x1.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::equal_type(&ctx, &x0, &x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                let head_type = head_type.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let tail_type = tail_type.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                Term::pair_type(&ctx, head_ident_opt, &head_type, &tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                let arg_type = arg_type.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let res_type = res_type.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                Term::func_type(&ctx, &arg_type, &res_type)
            },
            TermKind::Refl { x } => {
                let x = x.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::refl(&ctx, &x)
            }
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 3, bump_ident_opt, bump_ty);
                let target = target.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::j(&ctx, &target_type, &target, &elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::abort(&ctx, &target_type, &elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_ident_opt, head, tail } => {
                let head = head.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let tail = tail.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                Term::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TermKind::PairSplit { target_type, target, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 2, bump_ident_opt, bump_ty);
                let target = target.bump_ctx(bump_index + 2, bump_ident_opt, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::pair_split(&ctx, &target_type, &target, &elim)
            },
            TermKind::Func { arg_type, res } => {
                let arg_type = arg_type.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let res = res.bump_ctx(bump_index + 1, bump_ident_opt, bump_ty);
                Term::func(&ctx, &arg_type, &res)
            },
            TermKind::App { func, arg } => {
                let func = func.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                let arg = arg.bump_ctx(bump_index, bump_ident_opt, bump_ty);
                Term::app(&ctx, &func, &arg)
            },
        }
    }

    pub fn bump_into_ctx(&self, lo_ctx: &Ctx, hi_ctx: &Ctx) -> Term {
        if lo_ctx == hi_ctx {
            return self.clone();
        }

        let (parent, ident_opt, ty) = hi_ctx.unbind();
        
        self
        .bump_into_ctx(lo_ctx, &parent)
        .bump_ctx(0, &ident_opt, &ty)
    }

    pub fn substitute(&self, subst_index: u32, subst_ident_opt: &IdentOpt, subst_value: &Term) -> Term {
        let ctx = self.get_ctx().substitute(subst_index, subst_ident_opt, subst_value);
        match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name_opt } => {
                if *index == subst_index {
                    assert_eq!(name_opt.ident(), *subst_ident_opt);
                    subst_value.bump_into_ctx(&ctx.nth_parent(subst_index), &ctx)
                } else if *index > subst_index {
                    let name_opt = name_opt.unbump_over_ident_opt(subst_ident_opt);
                    Term::var(&ctx, *index - 1, &name_opt)
                } else {
                    Term::var(&ctx, *index, name_opt)
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.substitute(subst_index, subst_ident_opt, subst_value);
                let x1 = x1.substitute(subst_index, subst_ident_opt, subst_value);
                Term::equal_type(&ctx, &x0, &x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                let head_type = head_type.substitute(subst_index, subst_ident_opt, subst_value);
                let tail_type = tail_type.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Term::pair_type(&ctx, head_ident_opt, &head_type, &tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                let arg_type = arg_type.substitute(subst_index, subst_ident_opt, subst_value);
                let res_type = res_type.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Term::func_type(&ctx, &arg_type, &res_type)
            },
            TermKind::Refl { x } => {
                let x = x.substitute(subst_index, subst_ident_opt, subst_value);
                Term::refl(&ctx, &x)
            },
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.substitute(subst_index + 3, subst_ident_opt, subst_value);
                let target = target.substitute(subst_index + 1, subst_ident_opt, subst_value);
                let elim = elim.substitute(subst_index, subst_ident_opt, subst_value);
                Term::j(&ctx, &target_type, &target, &elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.substitute(subst_index + 1, subst_ident_opt, subst_value);
                let elim = elim.substitute(subst_index, subst_ident_opt, subst_value);
                Term::abort(&ctx, &target_type, &elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_ident_opt, head, tail } => {
                let head = head.substitute(subst_index, subst_ident_opt, subst_value);
                let tail = tail.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Term::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TermKind::PairSplit { target_type, target, elim } => {
                let target_type = target_type.substitute(subst_index + 2, subst_ident_opt, subst_value);
                let target = target.substitute(subst_index + 2, subst_ident_opt, subst_value);
                let elim = elim.substitute(subst_index, subst_ident_opt, subst_value);
                Term::pair_split(&ctx, &target_type, &target, &elim)
            },
            TermKind::Func { arg_type, res } => {
                let arg_type = arg_type.substitute(subst_index, subst_ident_opt, subst_value);
                let res = res.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Term::func(&ctx, &arg_type, &res)
            },
            TermKind::App { func, arg } => {
                let func = func.substitute(subst_index, subst_ident_opt, subst_value);
                let arg = arg.substitute(subst_index, subst_ident_opt, subst_value);
                Term::app(&ctx, &func, &arg)
            },
        }
    }

    pub fn reduce_head(&self) -> Option<Term> {
        match self.kind() {
            TermKind::J { target, elim, .. } => {
                let elim = elim.reduce_head().unwrap_or(elim.clone());
                if let TermKind::Refl { x } = elim.kind() {
                    return Some(target.substitute(0, &IdentOpt::fake("refl_x"), x));
                }
            },
            TermKind::PairSplit { target, elim, .. } => {
                let elim = elim.reduce_head().unwrap_or(elim.clone());
                if let TermKind::Pair { head_ident_opt, head, tail } = elim.kind() {
                    return Some({
                        target
                        .substitute(1, head_ident_opt, head)
                        .substitute(0, &IdentOpt::fake("tail"), tail)
                    });
                }
                if let Some(lifted_target) = target.try_lift_out_of_ctx(0, 2) {
                    return Some(lifted_target);
                }
            },
            TermKind::App { func, arg } => {
                let func = func.reduce_head().unwrap_or(func.clone());
                if let TermKind::Func { res, .. } = func.kind() {
                    return Some(res.substitute(0, &IdentOpt::fake("arg"), arg));
                }
            },
            _ => (),
        }
        None
    }

    pub fn try_lift_out_of_ctx(&self, cutoff: u32, lift_bumps: u32) -> Option<Term> {
        let ctx = self.get_ctx().try_lift(cutoff, lift_bumps)?;
        Some(match self.kind() {
            TermKind::Type { level } => {
                Term::ty(&ctx, *level)
            },
            TermKind::Var { index, name_opt } => {
                if *index > cutoff {
                    if *index < cutoff + lift_bumps {
                        return None;
                    } else {
                        let mut unbump_ctx = self.get_ctx();
                        let mut name_opt = name_opt.clone();
                        for _ in 0..lift_bumps {
                            let (parent, ident_opt, _) = unbump_ctx.unbind();
                            unbump_ctx = parent;
                            name_opt = name_opt.unbump_over_ident_opt(&ident_opt);
                        }
                        Term::var(&ctx, *index - lift_bumps, &name_opt)
                    }
                } else {
                    Term::var(&ctx, *index, name_opt)
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let x1 = x1.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::equal_type(&ctx, &x0, &x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                let head_type = head_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let tail_type = tail_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                Term::pair_type(&ctx, head_ident_opt, &head_type, &tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                let arg_type = arg_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let res_type = res_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                Term::func_type(&ctx, &arg_type, &res_type)
            },
            TermKind::Refl { x } => {
                let x = x.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::refl(&ctx, &x)
            },
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.try_lift_out_of_ctx(cutoff + 3, lift_bumps)?;
                let target = target.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                let elim = elim.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::j(&ctx, &target_type, &target, &elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                let elim = elim.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::abort(&ctx, &target_type, &elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_ident_opt, head, tail } => {
                let head = head.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let tail = tail.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TermKind::PairSplit { target_type, target, elim } => {
                let target_type = target_type.try_lift_out_of_ctx(cutoff + 2, lift_bumps)?;
                let target = target.try_lift_out_of_ctx(cutoff + 2, lift_bumps)?;
                let elim = elim.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::pair_split(&ctx, &target_type, &target, &elim)
            },
            TermKind::Func { arg_type, res } => {
                let arg_type = arg_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let res = res.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                Term::func(&ctx, &arg_type, &res)
            },
            TermKind::App { func, arg } => {
                let func = func.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let arg = arg.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Term::app(&ctx, &func, &arg)
            },
        })
    }

    pub fn ty(ctx: &Ctx, level: u32) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Type");
            hasher.write_u32(level);
            hasher.finish()
        };
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Type { level },
                ty: Type::ty(ctx, level + 1),
                hash,
            }),
        }
    }

    pub fn var(ctx: &Ctx, index: u32, name_opt: &NameOpt) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Var");
            hasher.write_u32(index);
            if let NameOpt::Real(name) = name_opt {
                hasher.write(name.ident().as_str().as_bytes());
                hasher.write_u32(name.bumps());
            };
            hasher.finish()
        };
        let ty = ctx.lookup_and_bump_out(index, &name_opt);
        let name_opt = name_opt.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Var { index, name_opt },
                ty,
                hash,
            }),
        }
    }

    pub fn equal_type(ctx: &Ctx, x0: &Term, x1: &Term) -> Term {
        assert_eq!(x0.get_type(), x1.get_type());
        assert_eq!(*ctx, x0.get_ctx());
        assert_eq!(*ctx, x1.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"EqualType");
            hasher.write_u64(x0.get_hash());
            hasher.write_u64(x1.get_hash());
            hasher.finish()
        };
        let level = x0.get_type().get_level();
        let x0 = x0.clone();
        let x1 = x1.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::EqualType { x0, x1 },
                ty: Type::ty(ctx, level),
                hash,
            }),
        }
    }

    pub fn unit_type(ctx: &Ctx) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"UnitType");
            hasher.finish()
        };
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::UnitType,
                ty: Type::ty(ctx, 0),
                hash,
            }),
        }
    }

    pub fn never_type(ctx: &Ctx) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"NeverType");
            hasher.finish()
        };
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::NeverType,
                ty: Type::ty(ctx, 0),
                hash,
            }),
        }
    }

    pub fn pair_type(ctx: &Ctx, head_ident_opt: &IdentOpt, head_type: &Type, tail_type: &Type) -> Term {
        assert_eq!(*ctx, head_type.get_ctx());
        assert_eq!(Ctx::bind(ctx, head_ident_opt, head_type), tail_type.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"PairType");
            if let IdentOpt::Real(ident) = head_ident_opt {
                hasher.write(ident.as_str().as_bytes());
            };
            hasher.write_u64(head_type.get_hash());
            hasher.write_u64(tail_type.get_hash());
            hasher.finish()
        };
        let head_level = head_type.get_level();
        let tail_level = tail_type.get_level();
        let level = cmp::max(head_level, tail_level);
        let head_ident_opt = head_ident_opt.clone();
        let head_type = head_type.clone();
        let tail_type = tail_type.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::PairType { head_ident_opt, head_type, tail_type },
                ty: Type::ty(ctx, level),
                hash,
            }),
        }
    }

    pub fn func_type(ctx: &Ctx, arg_type: &Type, res_type: &Type) -> Term {
        assert_eq!(*ctx, arg_type.get_ctx());
        assert_eq!(ctx.bind(&IdentOpt::fake("arg"), arg_type), res_type.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"FuncType");
            hasher.write_u64(arg_type.get_hash());
            hasher.write_u64(res_type.get_hash());
            hasher.finish()
        };
        let arg_level = arg_type.get_level();
        let res_level = res_type.get_level();
        let level = cmp::max(arg_level, res_level);
        let arg_type = arg_type.clone();
        let res_type = res_type.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::FuncType { arg_type, res_type },
                ty: Type::ty(ctx, level),
                hash,
            }),
        }
    }

    pub fn refl(ctx: &Ctx, x: &Term) -> Term {
        assert_eq!(*ctx, x.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Refl");
            hasher.write_u64(x.get_hash());
            hasher.finish()
        };
        let ty = Type::equal(ctx, x, x);
        let x = x.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Refl { x },
                ty: ty,
                hash,
            }),
        }
    }

    pub fn j(ctx: &Ctx, target_type: &Type, target: &Term, elim: &Term) -> Term {
        assert_eq!(*ctx, target_type.get_ctx().parent().parent().parent());
        assert_eq!(*ctx, target.get_ctx().parent());
        assert_eq!(*ctx, elim.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"J");
            hasher.write_u64(target_type.get_hash());
            hasher.write_u64(target.get_hash());
            hasher.write_u64(elim.get_hash());
            hasher.finish()
        };

        let (a0, a1) = match elim.get_type().kind() {
            TypeKind::Equal { x0, x1 } => (x0.clone(), x1.clone()),
            _ => panic!("elim is not an equality"),
        };

        let (target_type_x0_ident_opt, target_type_x0_type) = target_type.get_ctx().lookup_ident_opt(2);
        let (target_type_x1_ident_opt, target_type_x1_type) = target_type.get_ctx().lookup_ident_opt(1);
        let (target_type_equality_ident_opt, target_type_equality_type) = target_type.get_ctx().lookup_ident_opt(0);

        let target_type_x0_name_opt = NameOpt::new(&target_type_x0_ident_opt, 0).bump_over_ident_opt(&target_type_x1_ident_opt);
        let target_type_x1_name_opt = NameOpt::new(&target_type_x1_ident_opt, 0);

        assert_eq!(target_type_x0_type, a0.get_type());
        assert_eq!(target_type_x1_type, a1.get_type().bump_ctx(0, &target_type_x0_ident_opt, &target_type_x0_type));
        assert_eq!(target_type_equality_type, {
            Type::equal(
                &target_type.get_ctx().parent(),
                &Term::var(&target_type.get_ctx().parent(), 1, &target_type_x0_name_opt),
                &Term::var(&target_type.get_ctx().parent(), 0, &target_type_x1_name_opt),
            )
        });

        let (target_arg_ident_opt, target_arg_type) = target.get_ctx().lookup_ident_opt(0);
        assert_eq!(target_arg_type, a0.get_type());

        let target_arg_name_opt = NameOpt::new(&target_arg_ident_opt, 0);
        let target_arg_var = Term::var(&target.get_ctx(), 0, &target_arg_name_opt);
        let expected_type = {
            target_type
            .bump_ctx(3, &target_arg_ident_opt, &target_arg_type)
            .substitute(2, &target_type_x0_ident_opt, &target_arg_var)
            .substitute(1, &target_type_x1_ident_opt, &target_arg_var)
            .substitute(0, &target_type_equality_ident_opt, &Term::refl(&target.get_ctx(), &target_arg_var))
        };

        assert_eq!(target.get_type(), expected_type);

        let final_ty = {
            target_type
            .substitute(2, &target_type_x0_ident_opt, &a0)
            .substitute(1, &target_type_x1_ident_opt, &a1)
            .substitute(0, &target_type_equality_ident_opt, elim)
        };
        let target_type = target_type.clone();
        let target = target.clone();
        let elim = elim.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::J { target_type, target, elim },
                ty: final_ty,
                hash,
            }),
        }
    }

    pub fn abort(ctx: &Ctx, target_type: &Type, elim: &Term) -> Term {
        assert_eq!(*ctx, elim.get_ctx());
        assert_eq!(*ctx, target_type.get_ctx().parent());
        assert_eq!(elim.get_type(), Type::never(ctx));
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Abort");
            hasher.write_u64(target_type.get_hash());
            hasher.write_u64(elim.get_hash());
            hasher.finish()
        };
        let (target_type_arg_name, target_type_arg_type) = target_type.get_ctx().lookup_ident_opt(0);
        assert_eq!(target_type_arg_type, Type::never(ctx));
        let final_type = target_type.substitute(0, &target_type_arg_name, elim);
        let target_type = target_type.clone();
        let elim = elim.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Abort { target_type, elim },
                ty: final_type,
                hash,
            })
        }
    }

    pub fn unit(ctx: &Ctx) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Unit");
            hasher.finish()
        };
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Unit,
                ty: Type::unit(ctx),
                hash,
            }),
        }
    }

    pub fn pair(ctx: &Ctx, head_ident_opt: &IdentOpt, head: &Term, tail: &Term) -> Term {
        assert_eq!(*ctx, head.get_ctx());
        assert_eq!(Ctx::bind(ctx, head_ident_opt, &head.get_type()), tail.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Pair");
            if let IdentOpt::Real(ident) = head_ident_opt {
                hasher.write(ident.as_str().as_bytes());
            };
            hasher.write_u64(head.get_hash());
            hasher.write_u64(tail.get_hash());
            hasher.finish()
        };
        let ty = Type::pair(ctx, head_ident_opt, &head.get_type(), &tail.get_type());
        let head_ident_opt = head_ident_opt.clone();
        let head = head.clone();
        let tail = tail.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Pair { head_ident_opt, head, tail },
                ty: ty,
                hash,
            }),
        }
    }

    pub fn pair_split(ctx: &Ctx, target_type: &Type, target: &Term, elim: &Term) -> Term {
        let target_type_ctx = target_type.get_ctx();
        let (target_type_tail_ctx, _, target_type_tail_type) = target_type_ctx.unbind();
        let (target_type_head_ctx, _, target_type_head_type) = target_type_tail_ctx.unbind();
        assert_eq!(*ctx, target_type_head_ctx);
        assert_eq!(target_type.get_ctx(), target.get_ctx());
        assert_eq!(*target_type, target.get_type());

        assert_eq!(*ctx, elim.get_ctx());

        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"PairSplit");
            hasher.write_u64(target_type.get_hash());
            hasher.write_u64(target.get_hash());
            hasher.write_u64(elim.get_hash());
            hasher.finish()
        };

        let (head_type, tail_type) = match elim.get_type().kind() {
            TypeKind::Pair { head, tail, .. } => (head.clone(), tail.clone()),
            _ => panic!("argument to pair_split is not a pair"),
        };

        assert_eq!(head_type, target_type_head_type);
        assert_eq!(tail_type, target_type_tail_type);

        let final_type = match target_type.try_lift_out_of_ctx(0, 2) {
            Some(final_type) => final_type,
            None => {
                Type::from_term(Term::pair_split(
                    ctx,
                    &Type::ty(&target_type.get_ctx(), target_type.get_level()),
                    &target_type.into_term(),
                    elim,
                ))
            },
        };

        let target_type = target_type.clone();
        let target = target.clone();
        let elim = elim.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::PairSplit { target_type, target, elim },
                ty: final_type,
                hash,
            })
        }
    }

    pub fn func(ctx: &Ctx, arg_type: &Type, res: &Term) -> Term {
        assert_eq!(*ctx, arg_type.get_ctx());
        assert_eq!(ctx.bind(&IdentOpt::fake("arg"), arg_type), res.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Func");
            hasher.write_u64(arg_type.get_hash());
            hasher.write_u64(res.get_hash());
            hasher.finish()
        };

        let ty = Type::func(ctx, arg_type, &res.get_type());
        let arg_type = arg_type.clone();
        let res = res.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Func { arg_type, res },
                ty: ty,
                hash,
            }),
        }
    }

    pub fn app(ctx: &Ctx, func: &Term, arg: &Term) -> Term {
        assert_eq!(*ctx, func.get_ctx());
        assert_eq!(*ctx, arg.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"App");
            hasher.write_u64(func.get_hash());
            hasher.write_u64(arg.get_hash());
            hasher.finish()
        };

        let (arg_type, res_type) = match func.get_type().kind() {
            TypeKind::Func { arg, res } => (arg.clone(), res.clone()),
            _ => panic!("invalid type for function application"),
        };

        assert_eq!(arg_type, arg.get_type());
        let final_type = res_type.substitute(0, &IdentOpt::fake("arg"), arg);

        let func = func.clone();
        let arg = arg.clone();
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::App { func, arg },
                ty: final_type,
                hash,
            }),
        }
    }
}

