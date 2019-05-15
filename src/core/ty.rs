use super::*;
use crate::syntax::{Ident, IdentOpt, Name, NameOpt};
use pretty_assertions::{assert_eq, assert_ne};

#[derive(Clone)]
pub struct Type {
    inner: Rc<TypeInner>,
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner.kind, f)
    }
}

struct TypeInner {
    kind: TypeKind,
    ctx: Ctx,
    hash: u64,
}

impl Hash for TypeInner {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash.hash(hasher)
    }
}

#[derive(Debug)]
pub enum TypeKind {
    Embed(Term),
    Type {
        level: u32,
    },
    Equal {
        x0: Term,
        x1: Term,
    },
    Never,
    Unit,
    Pair {
        head_ident_opt: IdentOpt,
        head: Type,
        tail: Type,
    },
    Func {
        arg: Type,
        res: Type,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self.kind(), other.kind()) {
            (TypeKind::Embed(term0), TypeKind::Embed(term1)) => {
                term0 == term1
            },
            (TypeKind::Type { level: level0 }, TypeKind::Type { level: level1 }) => {
                level0 == level1
            },
            (TypeKind::Equal { x0: x00, x1: x10 }, TypeKind::Equal { x0: x01, x1: x11 }) => {
                x00 == x01 &&
                x10 == x11
            },
            (TypeKind::Never, TypeKind::Never) => true,
            (TypeKind::Unit, TypeKind::Unit) => true,
            (TypeKind::Pair { head_ident_opt: head_ident_opt0, head: head0, tail: tail0 },
             TypeKind::Pair { head_ident_opt: head_ident_opt1, head: head1, tail: tail1 }) => {
                head_ident_opt0 == head_ident_opt1 &&
                head0 == head1 &&
                tail0 == tail1
            },
            (TypeKind::Func { arg: arg0, res: res0 },
             TypeKind::Func { arg: arg1, res: res1 }) => {
                arg0 == arg1 &&
                res0 == res1
            },
            _ => false,
        }
    }
}

impl Type {
    pub fn from_term(term: Term) -> Type {
        Type::embed(&term.get_ctx(), &term)
    }

    pub fn into_term(&self) -> Term {
        let ctx = self.get_ctx();
        match self.kind() {
            TypeKind::Embed(term) => term.clone(),
            TypeKind::Type { level } => Term::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => Term::equal_type(&ctx, x0, x1),
            TypeKind::Never => Term::never_type(&ctx),
            TypeKind::Unit => Term::unit_type(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => Term::pair_type(&ctx, head_ident_opt, head, tail),
            TypeKind::Func { arg, res } => Term::func_type(&ctx, arg, res),
        }
    }

    pub fn get_ctx(&self) -> Ctx {
        self.inner.ctx.clone()
    }

    pub fn get_level(&self) -> u32 {
        match self.kind() {
            TypeKind::Embed(term) => {
                match term.get_type().kind() {
                    TypeKind::Type { level } => *level,
                    _ => panic!("term embedded as type is not a type"),
                }
            },
            TypeKind::Type { level } => level + 1,
            TypeKind::Equal { x0, .. } => x0.get_type().get_level(),
            TypeKind::Never |
            TypeKind::Unit => 0,
            TypeKind::Pair { head, tail, .. } => cmp::max(head.get_level(), tail.get_level()),
            TypeKind::Func { arg, res } => cmp::max(arg.get_level(), res.get_level()),
        }
    }

    pub fn get_hash(&self) -> u64 {
        self.inner.hash
    }

    pub fn kind(&self) -> &TypeKind {
        &self.inner.kind
    }

    pub fn bump_ctx(&self, index: u32, bump_ident_opt: &IdentOpt, ty: &Type) -> Type {
        assert_eq!(self.get_ctx().nth_parent(index), ty.get_ctx());
        let ctx = self.get_ctx().bump(index, bump_ident_opt, ty);
        match self.kind() {
            TypeKind::Embed(term) => Type::embed(&ctx, &term.bump_ctx(index, bump_ident_opt, ty)),
            TypeKind::Type { level } => Type::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.bump_ctx(index, bump_ident_opt, ty);
                let x1 = x1.bump_ctx(index, bump_ident_opt, ty);
                Type::equal(&ctx, &x0, &x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => {
                let head = head.bump_ctx(index, bump_ident_opt, ty);
                let tail = tail.bump_ctx(index + 1, bump_ident_opt, ty);
                Type::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TypeKind::Func { arg, res } => {
                let arg = arg.bump_ctx(index, bump_ident_opt, ty);
                let res = res.bump_ctx(index + 1, bump_ident_opt, ty);
                Type::func(&ctx, &arg, &res)
            },
        }
    }

    pub fn bump_into_ctx(&self, lo_ctx: &Ctx, hi_ctx: &Ctx) -> Type {
        if lo_ctx == hi_ctx {
            return self.clone();
        }

        let (parent, ident_opt, ty) = hi_ctx.unbind();
        
        self
        .bump_into_ctx(lo_ctx, &parent)
        .bump_ctx(0, &ident_opt, &ty)
    }

    pub fn substitute(&self, subst_index: u32, subst_ident_opt: &IdentOpt, subst_value: &Term) -> Type {
        let trimmed_ctx = self.get_ctx().nth_parent(subst_index);
        let (trimmed_parent, _, trimmed_type) = trimmed_ctx.unbind();
        assert_eq!(trimmed_parent, subst_value.get_ctx());
        assert_eq!(trimmed_type, subst_value.get_type());

        let ctx = self.get_ctx().substitute(subst_index, subst_ident_opt, subst_value);
        match self.kind() {
            TypeKind::Embed(term) => {
                let term = term.substitute(subst_index, subst_ident_opt, subst_value);
                Type::embed(&ctx, &term)
            },
            TypeKind::Type { level } => Type::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.substitute(subst_index, subst_ident_opt, subst_value);
                let x1 = x1.substitute(subst_index, subst_ident_opt, subst_value);
                assert_eq!(x0.get_ctx(), x1.get_ctx());
                Type::equal(&ctx, &x0, &x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => {
                let head = head.substitute(subst_index, subst_ident_opt, subst_value);
                let tail = tail.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Type::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TypeKind::Func { arg, res } => {
                let arg = arg.substitute(subst_index, subst_ident_opt, subst_value);
                let res = res.substitute(subst_index + 1, subst_ident_opt, subst_value);
                Type::func(&ctx, &arg, &res)
            },
        }
    }

    /*
    pub fn reduce_head(&self) -> Option<Type> {
        let ctx = self.get_ctx();
        if let TypeKind::Embed(term) = self.kind() {
            let term = term.reduce_head().unwrap_or(term.clone());
            match term.kind() {
                TermKind::Type { level } => {
                    return Some(Type::ty(&ctx, *level));
                },
                TermKind::EqualType { x0, x1 } => {
                    return Some(Type::equal(&ctx, x0, x1));
                },
                TermKind::UnitType => {
                    return Some(Type::unit(&ctx));
                },
                TermKind::NeverType => {
                    return Some(Type::never(&ctx));
                },
                TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                    return Some(Type::pair(&ctx, head_ident_opt, head_type, tail_type));
                },
                TermKind::FuncType { arg_type, res_type } => {
                    return Some(Type::func(&ctx, arg_type, res_type));
                },
                _ => (),
            }
        }
        None
    }
    */

    pub fn try_lift_out_of_ctx(&self, cutoff: u32, lift_bumps: u32) -> Option<Type> {
        let ctx = self.get_ctx().try_lift(cutoff, lift_bumps)?;
        Some(match self.kind() {
            TypeKind::Embed(term) => {
                Type::embed(&ctx, &term.try_lift_out_of_ctx(cutoff, lift_bumps)?)
            },
            TypeKind::Type { level } => {
                Type::ty(&ctx, *level)
            },
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let x1 = x1.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                Type::equal(&ctx, &x0, &x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => {
                let head = head.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let tail = tail.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                Type::pair(&ctx, head_ident_opt, &head, &tail)
            },
            TypeKind::Func { arg, res } => {
                let arg = arg.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                let res = res.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                Type::func(&ctx, &arg, &res)
            },
        })
    }

    pub fn embed(ctx: &Ctx, term: &Term) -> Type {
        assert_eq!(*ctx, term.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Embed");
            hasher.write_u64(term.get_hash());
            hasher.finish()
        };
        match term.get_type().kind() {
            TypeKind::Type { .. } => (),
            _ => panic!("embedding a term which is not a type"),
        }
        match term.kind() {
            TermKind::Type { level } => {
                Type::ty(&ctx, *level)
            },
            TermKind::EqualType { x0, x1 } => {
                Type::equal(&ctx, x0, x1)
            },
            TermKind::UnitType => {
                Type::unit(&ctx)
            },
            TermKind::NeverType => {
                Type::never(&ctx)
            },
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                Type::pair(&ctx, head_ident_opt, head_type, tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                Type::func(&ctx, arg_type, res_type)
            },
            _ => {
                Type {
                    inner: Rc::new(TypeInner {
                        kind: TypeKind::Embed(term.clone()),
                        ctx: ctx.clone(),
                        hash,
                    }),
                }
            },
        }
    }

    pub fn ty(ctx: &Ctx, level: u32) -> Type {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Type");
            hasher.write_u32(level);
            hasher.finish()
        };
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Type { level },
                ctx: ctx.clone(),
                hash,
            }),
        }
    }

    pub fn never(ctx: &Ctx) -> Type {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Never");
            hasher.finish()
        };
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Never,
                ctx: ctx.clone(),
                hash,
            }),
        }
    }

    pub fn unit(ctx: &Ctx) -> Type {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Unit");
            hasher.finish()
        };
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Unit,
                ctx: ctx.clone(),
                hash,
            }),
        }
    }
    
    pub fn equal(ctx: &Ctx, x0: &Term, x1: &Term) -> Type {
        assert_eq!(*ctx, x0.get_ctx());
        assert_eq!(*ctx, x1.get_ctx());
        assert_eq!(x0.get_type(), x1.get_type());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Equal");
            hasher.write_u64(x0.get_hash());
            hasher.write_u64(x1.get_hash());
            hasher.finish()
        };
        let x0 = x0.clone();
        let x1 = x1.clone();
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Equal { x0, x1 },
                ctx: ctx.clone(),
                hash,
            }),
        }
    }

    pub fn pair(ctx: &Ctx, head_ident_opt: &IdentOpt, head: &Type, tail: &Type) -> Type {
        assert_eq!(*ctx, head.get_ctx());
        assert_eq!(Ctx::bind(ctx, head_ident_opt, head), tail.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Pair");
            if let IdentOpt::Real(ident) = head_ident_opt {
                hasher.write(ident.as_str().as_bytes());
            }
            hasher.write_u64(head.get_hash());
            hasher.write_u64(tail.get_hash());
            hasher.finish()
        };
        let head_ident_opt = head_ident_opt.clone();
        let head = head.clone();
        let tail = tail.clone();
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Pair { head_ident_opt, head, tail },
                ctx: ctx.clone(),
                hash,
            }),
        }
    }

    pub fn func(ctx: &Ctx, arg: &Type, res: &Type) -> Type {
        assert_eq!(*ctx, arg.get_ctx());
        assert_eq!(ctx.bind(&IdentOpt::fake("arg"), arg), res.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Func");
            hasher.write_u64(arg.get_hash());
            hasher.write_u64(res.get_hash());
            hasher.finish()
        };
        let arg = arg.clone();
        let res = res.clone();
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Func { arg, res },
                ctx: ctx.clone(),
                hash,
            }),
        }
    }
}

