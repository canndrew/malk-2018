pub use super::*;
use syntax::Ident;
use pretty_assertions::{assert_eq, assert_ne};
use crate::core::render;

lazy_static! {
    static ref TERMS: Interner<TermInner> = Interner::new();
    static ref BUMP_CACHE: Mutex<HashMap<(Term, u32, StrName, Type), Term>> = Mutex::new(HashMap::new());
    static ref SUBSTITUTE_CACHE: Mutex<HashMap<(Term, u32, StrName, Term), Term>> = Mutex::new(HashMap::new());
    static ref TRY_LIFT_CACHE: Mutex<HashMap<(Term, u32, u32), Option<Term>>> = Mutex::new(HashMap::new());
}

#[derive(Eq, Clone, Hash)]
pub struct Term {
    inner: Arc<TermInner>,
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(PartialEq, Eq, Debug)]
pub enum TermKind {
    Type {
        level: u32,
    },
    Var {
        index: u32,
        name: StrName,
    },
    EqualType {
        x0: Term,
        x1: Term,
    },
    UnitType,
    NeverType,
    StringType,
    PairType {
        head_name: StrName,
        head_type: Type,
        tail_type: Type,
    },
    FuncType {
        arg_name: StrName,
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
    StringLit {
        ident: Ident,
    },
    Pair {
        head_name: StrName,
        head: Term,
        tail_type: Type,
        tail: Term,
    },
    PairSplit {
        target_type: Type,
        target: Term,
        elim: Term,
    },
    Func {
        arg_name: StrName,
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

    pub fn get_hash(&self) -> u64 {
        self.inner.hash
    }

    pub fn kind(&self) -> &TermKind {
        &self.inner.kind
    }

    pub fn bump_ctx(&self, bump_index: u32, bump_name: &StrName, bump_ty: &Type) -> Term {
        let parent_ctx = self.get_ctx().nth_parent(bump_index);
        let bump_ty = &bump_ty.bump_into_ctx(&bump_ty.get_ctx(), &parent_ctx);
        let bump_name = &bump_name.bump_into_ctx(&bump_name.get_ctx(), &parent_ctx);

        let key = (self.clone(), bump_index, bump_name.clone(), bump_ty.clone());
        let cache = unwrap!(BUMP_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().bump(bump_index, bump_name, bump_ty);
        let ret = match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name } => {
                let name = name.bump_ctx(bump_index, bump_name, bump_ty);
                if *index >= bump_index {
                    Term::var(&ctx, *index + 1, &name)
                } else {
                    Term::var(&ctx, *index, &name)
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.bump_ctx(bump_index, bump_name, bump_ty);
                let x1 = x1.bump_ctx(bump_index, bump_name, bump_ty);
                Term::equal_type(&ctx, &x0, &x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::StringType => Term::string_type(&ctx),
            TermKind::StringLit { ident } => Term::string_lit(&ctx, ident),
            TermKind::PairType { head_name, head_type, tail_type } => {
                let head_name = head_name.bump_ctx(bump_index, bump_name, bump_ty);
                let head_type = head_type.bump_ctx(bump_index, bump_name, bump_ty);
                let tail_type = tail_type.bump_ctx(bump_index + 1, bump_name, bump_ty);
                Term::pair_type(&ctx, &head_name, &head_type, &tail_type)
            },
            TermKind::FuncType { arg_name, arg_type, res_type } => {
                let arg_name = arg_name.bump_ctx(bump_index, bump_name, bump_ty);
                let arg_type = arg_type.bump_ctx(bump_index, bump_name, bump_ty);
                let res_type = res_type.bump_ctx(bump_index + 1, bump_name, bump_ty);
                Term::func_type(&ctx, &arg_name, &arg_type, &res_type)
            },
            TermKind::Refl { x } => {
                let x = x.bump_ctx(bump_index, bump_name, bump_ty);
                Term::refl(&ctx, &x)
            }
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 3, bump_name, bump_ty);
                let target = target.bump_ctx(bump_index + 1, bump_name, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_name, bump_ty);
                Term::j(&ctx, &target_type, &target, &elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 1, bump_name, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_name, bump_ty);
                Term::abort(&ctx, &target_type, &elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_name, head, tail_type, tail } => {
                let head_name = head_name.bump_ctx(bump_index, bump_name, bump_ty);
                let head = head.bump_ctx(bump_index, bump_name, bump_ty);
                let tail_type = tail_type.bump_ctx(bump_index + 1, bump_name, bump_ty);
                let tail = tail.bump_ctx(bump_index, bump_name, bump_ty);
                Term::pair(&ctx, &head_name, &head, &tail_type, &tail)
            },
            TermKind::PairSplit { target_type, target, elim } => {
                let target_type = target_type.bump_ctx(bump_index + 2, bump_name, bump_ty);
                let target = target.bump_ctx(bump_index + 2, bump_name, bump_ty);
                let elim = elim.bump_ctx(bump_index, bump_name, bump_ty);
                Term::pair_split(&ctx, &target_type, &target, &elim)
            },
            TermKind::Func { arg_name, arg_type, res } => {
                let arg_name = arg_name.bump_ctx(bump_index, bump_name, bump_ty);
                let arg_type = arg_type.bump_ctx(bump_index, bump_name, bump_ty);
                let res = res.bump_ctx(bump_index + 1, bump_name, bump_ty);
                Term::func(&ctx, &arg_name, &arg_type, &res)
            },
            TermKind::App { func, arg } => {
                let func = func.bump_ctx(bump_index, bump_name, bump_ty);
                let arg = arg.bump_ctx(bump_index, bump_name, bump_ty);
                Term::app(&ctx, &func, &arg)
            },
        };

        let mut cache = unwrap!(BUMP_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    pub fn bump_into_ctx(&self, lo_ctx: &Ctx, hi_ctx: &Ctx) -> Term {
        if lo_ctx == hi_ctx {
            return self.clone();
        }

        let (parent, name, ty) = hi_ctx.unbind();
        
        self
        .bump_into_ctx(lo_ctx, &parent)
        .bump_ctx(0, &name, &ty)
    }

    pub fn substitute(&self, subst_index: u32, subst_name: &StrName, subst_value: &Term) -> Term {
        let parent_ctx = self.get_ctx().nth_parent(subst_index + 1);
        let subst_name = &subst_name.bump_into_ctx(&subst_name.get_ctx(), &parent_ctx);
        assert_eq!(parent_ctx, subst_value.get_ctx());

        let key = (self.clone(), subst_index, subst_name.clone(), subst_value.clone());
        let cache = unwrap!(SUBSTITUTE_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().substitute(subst_index, subst_name, subst_value);
        let ret = match self.kind() {
            TermKind::Type { level } => Term::ty(&ctx, *level),
            TermKind::Var { index, name } => {
                let name = name.substitute(subst_index, subst_name, subst_value);
                if *index == subst_index {
                    let expected_name = subst_name.bump_into_ctx(&parent_ctx, &ctx);
                    assert_eq!(name, expected_name);
                    subst_value.bump_into_ctx(&parent_ctx, &ctx)
                } else if *index > subst_index {
                    Term::var(&ctx, *index - 1, &name)
                } else {
                    Term::var(&ctx, *index, &name)
                }
            },
            TermKind::EqualType { x0, x1 } => {
                let x0 = x0.substitute(subst_index, subst_name, subst_value);
                let x1 = x1.substitute(subst_index, subst_name, subst_value);
                Term::equal_type(&ctx, &x0, &x1)
            },
            TermKind::UnitType => Term::unit_type(&ctx),
            TermKind::NeverType => Term::never_type(&ctx),
            TermKind::StringType => Term::string_type(&ctx),
            TermKind::StringLit { ident } => Term::string_lit(&ctx, ident),
            TermKind::PairType { head_name, head_type, tail_type } => {
                let head_name = head_name.substitute(subst_index, subst_name, subst_value);
                let head_type = head_type.substitute(subst_index, subst_name, subst_value);
                let tail_type = tail_type.substitute(subst_index + 1, subst_name, subst_value);
                Term::pair_type(&ctx, &head_name, &head_type, &tail_type)
            },
            TermKind::FuncType { arg_name, arg_type, res_type } => {
                let arg_name = arg_name.substitute(subst_index, subst_name, subst_value);
                let arg_type = arg_type.substitute(subst_index, subst_name, subst_value);
                let res_type = res_type.substitute(subst_index + 1, subst_name, subst_value);
                Term::func_type(&ctx, &arg_name, &arg_type, &res_type)
            },
            TermKind::Refl { x } => {
                let x = x.substitute(subst_index, subst_name, subst_value);
                Term::refl(&ctx, &x)
            },
            TermKind::J { target_type, target, elim } => {
                let target_type = target_type.substitute(subst_index + 3, subst_name, subst_value);
                let target = target.substitute(subst_index + 1, subst_name, subst_value);
                let elim = elim.substitute(subst_index, subst_name, subst_value);
                Term::j(&ctx, &target_type, &target, &elim)
            },
            TermKind::Abort { target_type, elim } => {
                let target_type = target_type.substitute(subst_index + 1, subst_name, subst_value);
                let elim = elim.substitute(subst_index, subst_name, subst_value);
                Term::abort(&ctx, &target_type, &elim)
            },
            TermKind::Unit => Term::unit(&ctx),
            TermKind::Pair { head_name, head, tail_type, tail } => {
                let head_name = head_name.substitute(subst_index, subst_name, subst_value);
                let head = head.substitute(subst_index, subst_name, subst_value);
                let tail_type = tail_type.substitute(subst_index + 1, subst_name, subst_value);
                let tail = tail.substitute(subst_index, subst_name, subst_value);
                Term::pair(&ctx, &head_name, &head, &tail_type, &tail)
            },
            TermKind::PairSplit { target_type, target, elim } => {
                let target_type = target_type.substitute(subst_index + 2, subst_name, subst_value);
                let target = target.substitute(subst_index + 2, subst_name, subst_value);
                let elim = elim.substitute(subst_index, subst_name, subst_value);
                Term::pair_split(&ctx, &target_type, &target, &elim)
            },
            TermKind::Func { arg_name, arg_type, res } => {
                let arg_name = arg_name.substitute(subst_index, subst_name, subst_value);
                let arg_type = arg_type.substitute(subst_index, subst_name, subst_value);
                let res = res.substitute(subst_index + 1, subst_name, subst_value);
                Term::func(&ctx, &arg_name, &arg_type, &res)
            },
            TermKind::App { func, arg } => {
                let func = func.substitute(subst_index, subst_name, subst_value);
                let arg = arg.substitute(subst_index, subst_name, subst_value);
                Term::app(&ctx, &func, &arg)
            },
        };

        let mut cache = unwrap!(SUBSTITUTE_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    pub fn try_lift_out_of_ctx(&self, cutoff: u32, lift_bumps: u32) -> Option<Term> {

        let key = (self.clone(), cutoff, lift_bumps);
        let cache = unwrap!(TRY_LIFT_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().try_lift(cutoff, lift_bumps)?;
        let ret: Option<Term> = try {
            match self.kind() {
                TermKind::Type { level } => {
                    Term::ty(&ctx, *level)
                },
                TermKind::Var { index, name } => {
                    let name = name.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    if *index > cutoff {
                        if *index < cutoff + lift_bumps {
                            return None;
                        } else {
                            Term::var(&ctx, *index - lift_bumps, &name)
                        }
                    } else {
                        Term::var(&ctx, *index, &name)
                    }
                },
                TermKind::EqualType { x0, x1 } => {
                    let x0 = x0.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let x1 = x1.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    Term::equal_type(&ctx, &x0, &x1)
                },
                TermKind::UnitType => Term::unit_type(&ctx),
                TermKind::NeverType => Term::never_type(&ctx),
                TermKind::StringType => Term::string_type(&ctx),
                TermKind::StringLit { ident } => Term::string_lit(&ctx, ident),
                TermKind::PairType { head_name, head_type, tail_type } => {
                    let head_name = head_name.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let head_type = head_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let tail_type = tail_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                    Term::pair_type(&ctx, &head_name, &head_type, &tail_type)
                },
                TermKind::FuncType { arg_name, arg_type, res_type } => {
                    let arg_name = arg_name.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let arg_type = arg_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let res_type = res_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                    Term::func_type(&ctx, &arg_name, &arg_type, &res_type)
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
                TermKind::Pair { head_name, head, tail_type, tail } => {
                    let head_name = head_name.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let head = head.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let tail_type = tail_type.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                    let tail = tail.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    Term::pair(&ctx, &head_name, &head, &tail_type, &tail)
                },
                TermKind::PairSplit { target_type, target, elim } => {
                    let target_type = target_type.try_lift_out_of_ctx(cutoff + 2, lift_bumps)?;
                    let target = target.try_lift_out_of_ctx(cutoff + 2, lift_bumps)?;
                    let elim = elim.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    Term::pair_split(&ctx, &target_type, &target, &elim)
                },
                TermKind::Func { arg_name, arg_type, res } => {
                    let arg_name = arg_name.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let arg_type = arg_type.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let res = res.try_lift_out_of_ctx(cutoff + 1, lift_bumps)?;
                    Term::func(&ctx, &arg_name, &arg_type, &res)
                },
                TermKind::App { func, arg } => {
                    let func = func.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    let arg = arg.try_lift_out_of_ctx(cutoff, lift_bumps)?;
                    Term::app(&ctx, &func, &arg)
                },
            }
        };

        let mut cache = unwrap!(TRY_LIFT_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    pub fn ty(ctx: &Ctx, level: u32) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Type");
            hasher.write_u32(level);
            hasher.finish()
        };
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::Type { level },
                ty: Type::ty(ctx, level + 1),
                hash,
            }),
        }
    }

    pub fn var(ctx: &Ctx, index: u32, name: &StrName) -> Term {
        let name = name.bump_into_ctx(&name.get_ctx(), ctx);
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Var");
            hasher.write_u32(index);
            hasher.write_u64(name.get_hash());
            hasher.finish()
        };
        let ty = ctx.lookup_and_bump_out(index, &name);
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::Var { index, name },
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
            inner: TERMS.intern(TermInner {
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
            inner: TERMS.intern(TermInner {
                kind: TermKind::UnitType,
                ty: Type::ty(ctx, 0),
                hash,
            }),
        }
    }

    pub fn string_type(ctx: &Ctx) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"StringType");
            hasher.finish()
        };
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::StringType,
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
            inner: TERMS.intern(TermInner {
                kind: TermKind::NeverType,
                ty: Type::ty(ctx, 0),
                hash,
            }),
        }
    }

    pub fn pair_type(ctx: &Ctx, head_name: &StrName, head_type: &Type, tail_type: &Type) -> Term {
        let head_name = head_name.bump_into_ctx(&head_name.get_ctx(), ctx);
        let head_type = head_type.bump_into_ctx(&head_type.get_ctx(), ctx);
        let tail_type = tail_type.bump_into_ctx(&tail_type.get_ctx(), &ctx.bind(&head_name, &head_type));
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"PairType");
            hasher.write_u64(head_name.get_hash());
            hasher.write_u64(head_type.get_hash());
            hasher.write_u64(tail_type.get_hash());
            hasher.finish()
        };
        let head_level = head_type.get_level();
        let tail_level = tail_type.get_level();
        let level = cmp::max(head_level, tail_level);
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::PairType { head_name, head_type, tail_type },
                ty: Type::ty(ctx, level),
                hash,
            }),
        }
    }

    pub fn func_type(ctx: &Ctx, arg_name: &StrName, arg_type: &Type, res_type: &Type) -> Term {
        let arg_name = arg_name.bump_into_ctx(&arg_name.get_ctx(), ctx);
        let arg_type = arg_type.bump_into_ctx(&arg_type.get_ctx(), ctx);
        let res_type = res_type.bump_into_ctx(&res_type.get_ctx(), &ctx.bind(&arg_name, &arg_type));
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"FuncType");
            hasher.write_u64(arg_name.get_hash());
            hasher.write_u64(arg_type.get_hash());
            hasher.write_u64(res_type.get_hash());
            hasher.finish()
        };
        let arg_level = arg_type.get_level();
        let res_level = res_type.get_level();
        let level = cmp::max(arg_level, res_level);
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::FuncType { arg_name, arg_type, res_type },
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
            inner: TERMS.intern(TermInner {
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

        let target_type_ctx_0 = target_type.get_ctx();
        let (target_type_ctx_1, target_type_equality_name, target_type_equality_type) = target_type_ctx_0.unbind();
        let (target_type_ctx_2, target_type_x1_name, target_type_x1_type) = target_type_ctx_1.unbind();
        let (_, target_type_x0_name, target_type_x0_type) = target_type_ctx_2.unbind();

        assert_eq!(target_type_x0_type, a0.get_type());
        assert_eq!(target_type_x1_type, a1.get_type().bump_ctx(0, &target_type_x0_name, &target_type_x0_type));
        assert_eq!(target_type_equality_type, {
            Type::equal(
                &target_type_ctx_1,
                &Term::var(&target_type_ctx_1, 1, &target_type_x0_name),
                &Term::var(&target_type_ctx_1, 0, &target_type_x1_name),
            )
        });

        let target_ctx_0 = target.get_ctx();
        let (_, target_x_name, target_x_type) = target_ctx_0.unbind();
        assert_eq!(target_x_type, a0.get_type());

        let target_x_var = Term::var(&target_ctx_0, 0, &target_x_name);
        let expected_type = {
            let target_type_x0_name = {
                target_type_x0_name
                .bump_ctx(0, &target_x_name, &target_x_type)
            };
            let target_type_x1_name = {
                target_type_x1_name
                .bump_ctx(1, &target_x_name, &target_x_type)
                .substitute(0, &target_type_x0_name, &target_x_var)
            };
            let target_type_equality_name = {
                target_type_equality_name
                .bump_ctx(2, &target_x_name, &target_x_type)
                .substitute(1, &target_type_x0_name, &target_x_var)
                .substitute(0, &target_type_x1_name, &target_x_var)
            };
            target_type
            .bump_ctx(3, &target_x_name, &target_x_type)
            .substitute(2, &target_type_x0_name, &target_x_var)
            .substitute(1, &target_type_x1_name, &target_x_var)
            .substitute(0, &target_type_equality_name, &Term::refl(&target_ctx_0, &target_x_var))
        };
        assert_eq!(target.get_type(), expected_type);

        if let TermKind::Refl { x } = elim.kind() {
            target.substitute(0, &target_x_name, x)
        } else {
            let final_ty = {
                let target_type_x1_name = {
                    target_type_x1_name
                    .substitute(0, &target_type_x0_name, &a0)
                };
                let target_type_equality_name = {
                    target_type_equality_name
                    .substitute(1, &target_type_x0_name, &a0)
                    .substitute(0, &target_type_x1_name, &a1)
                };
                target_type
                .substitute(2, &target_type_x0_name, &a0)
                .substitute(1, &target_type_x1_name, &a1)
                .substitute(0, &target_type_equality_name, elim)
            };

            let target_type = target_type.clone();
            let target = target.clone();
            let elim = elim.clone();
            Term {
                inner: TERMS.intern(TermInner {
                    kind: TermKind::J { target_type, target, elim },
                    ty: final_ty,
                    hash,
                }),
            }
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
        let (_, target_type_arg_name, target_type_arg_type) = target_type.get_ctx().unbind();
        assert_eq!(target_type_arg_type, Type::never(ctx));
        let final_type = target_type.substitute(0, &target_type_arg_name, elim);
        let target_type = target_type.clone();
        let elim = elim.clone();
        Term {
            inner: TERMS.intern(TermInner {
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
            inner: TERMS.intern(TermInner {
                kind: TermKind::Unit,
                ty: Type::unit(ctx),
                hash,
            }),
        }
    }

    pub fn string_lit(ctx: &Ctx, ident: &Ident) -> Term {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"StringLit");
            hasher.write_u64(ident.get_hash());
            hasher.finish()
        };
        let ident = ident.clone();
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::StringLit { ident },
                ty: Type::string(ctx),
                hash,
            }),
        }
    }

    pub fn pair(
        ctx: &Ctx,
        head_name: &StrName,
        head: &Term,
        tail_type: &Type,
        tail: &Term,
    ) -> Term {
        let head_name = head_name.bump_into_ctx(&head_name.get_ctx(), ctx);
        let tail_type = tail_type.bump_into_ctx(&tail_type.get_ctx(), &ctx.bind(&head_name, &head.get_type()));
        assert_eq!(*ctx, head.get_ctx());
        assert_eq!(*ctx, tail.get_ctx());
        assert_eq!(tail_type.substitute(0, &head_name, head), tail.get_type());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Pair");
            hasher.write_u64(head_name.get_hash());
            hasher.write_u64(head.get_hash());
            hasher.write_u64(tail.get_hash());
            hasher.finish()
        };
        let ty = Type::pair(ctx, &head_name, &head.get_type(), &tail_type);
        let head = head.clone();
        let tail = tail.clone();
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::Pair { head_name, head, tail_type, tail },
                ty: ty,
                hash,
            }),
        }
    }

    pub fn pair_split(ctx: &Ctx, target_type: &Type, target: &Term, elim: &Term) -> Term {
        let target_type_ctx = target_type.get_ctx();
        let (target_type_tail_ctx, tail_name, target_type_tail_type) = target_type_ctx.unbind();
        let (target_type_head_ctx, head_name, target_type_head_type) = target_type_tail_ctx.unbind();
        let target_ctx = target.get_ctx();
        assert_eq!(*ctx, target_type_head_ctx);
        assert_eq!(target_type.get_ctx(), target_ctx);
        assert_eq!(*target_type, target.get_type());

        assert_eq!(*ctx, elim.get_ctx());

        let (real_head_name, head_type, tail_type) = match elim.get_type().kind() {
            TypeKind::Pair { head_name, head, tail, } => (head_name.clone(), head.clone(), tail.clone()),
            _ => panic!("argument to pair_split is not a pair"),
        };

        assert_eq!(head_type, target_type_head_type);
        assert_eq!(tail_type, target_type_tail_type);

        if let TermKind::Pair { head, tail, .. } = elim.kind() {
            target
            .substitute(1, &head_name, head)
            .substitute(0, &tail_name, tail)
        } else if target == &Term::pair(
            &target_ctx,
            &real_head_name,
            &Term::var(&target_ctx, 1, &head_name),
            &tail_type,
            &Term::var(&target_ctx, 0, &tail_name),
        ) {
            elim.clone()
        } else {
            let hash = {
                let mut hasher = DefaultHasher::new();
                hasher.write(b"PairSplit");
                hasher.write_u64(target_type.get_hash());
                hasher.write_u64(target.get_hash());
                hasher.write_u64(elim.get_hash());
                hasher.finish()
            };

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
                inner: TERMS.intern(TermInner {
                    kind: TermKind::PairSplit { target_type, target, elim },
                    ty: final_type,
                    hash,
                })
            }
        }
    }

    pub fn func(ctx: &Ctx, arg_name: &StrName, arg_type: &Type, res: &Term) -> Term {
        let arg_name = arg_name.bump_into_ctx(&arg_name.get_ctx(), ctx);
        let arg_type = arg_type.bump_into_ctx(&arg_type.get_ctx(), ctx);
        assert_eq!(ctx.bind(&arg_name, &arg_type), res.get_ctx());
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Func");
            hasher.write_u64(arg_type.get_hash());
            hasher.write_u64(res.get_hash());
            hasher.finish()
        };

        let ty = Type::func(ctx, &arg_name, &arg_type, &res.get_type());
        let res = res.clone();
        Term {
            inner: TERMS.intern(TermInner {
                kind: TermKind::Func { arg_name, arg_type, res },
                ty: ty,
                hash,
            }),
        }
    }

    pub fn app(ctx: &Ctx, func: &Term, arg: &Term) -> Term {
        assert_eq!(*ctx, func.get_ctx());
        assert_eq!(*ctx, arg.get_ctx());

        let (arg_name, arg_type, res_type) = match func.get_type().kind() {
            TypeKind::Func { arg_name, arg, res } => (arg_name.clone(), arg.clone(), res.clone()),
            _ => panic!("invalid type for function application"),
        };

        assert_eq!(arg_type, arg.get_type());

        if let TermKind::Func { res, .. } = func.kind() {
            res.substitute(0, &arg_name, arg)
        } else {
            let hash = {
                let mut hasher = DefaultHasher::new();
                hasher.write(b"App");
                hasher.write_u64(func.get_hash());
                hasher.write_u64(arg.get_hash());
                hasher.finish()
            };
            let final_type = res_type.substitute(0, &arg_name, arg);
            let func = func.clone();
            let arg = arg.clone();
            Term {
                inner: TERMS.intern(TermInner {
                    kind: TermKind::App { func, arg },
                    ty: final_type,
                    hash,
                }),
            }
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        render::render_term(self, f, 0, render::Precedence::Func)
    }
}

