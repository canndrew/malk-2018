use super::*;
use crate::syntax::{Ident, IdentOpt, Name, NameOpt};
use pretty_assertions::{assert_eq, assert_ne};

lazy_static! {
    static ref TYPES: Interner<TypeInner> = Interner::new();
    static ref BUMP_CACHE: Mutex<HashMap<(Type, u32, IdentOpt, Type), Type>> = Mutex::new(HashMap::new());
    static ref SUBSTITUTE_CACHE: Mutex<HashMap<(Type, u32, IdentOpt, Term), Type>> = Mutex::new(HashMap::new());
    static ref TRY_LIFT_CACHE: Mutex<HashMap<(Type, u32, u32), Option<Type>>> = Mutex::new(HashMap::new());
}

#[derive(Eq, Clone, Hash)]
pub struct Type {
    inner: Arc<TypeInner>,
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner.kind, f)
    }
}

#[derive(PartialEq, Eq)]
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

#[derive(PartialEq, Eq, Debug)]
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
    String,
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
            TypeKind::String => Term::string_type(&ctx),
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
            TypeKind::Unit |
            TypeKind::String => 0,
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

        let key = (self.clone(), index, bump_ident_opt.clone(), ty.clone());
        let cache = unwrap!(BUMP_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().bump(index, bump_ident_opt, ty);
        let ret = match self.kind() {
            TypeKind::Embed(term) => Type::embed(&ctx, &term.bump_ctx(index, bump_ident_opt, ty)),
            TypeKind::Type { level } => Type::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.bump_ctx(index, bump_ident_opt, ty);
                let x1 = x1.bump_ctx(index, bump_ident_opt, ty);
                Type::equal(&ctx, &x0, &x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::String => Type::string(&ctx),
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
        };

        let mut cache = unwrap!(BUMP_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
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

        let key = (self.clone(), subst_index, subst_ident_opt.clone(), subst_value.clone());
        let cache = unwrap!(SUBSTITUTE_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().substitute(subst_index, subst_ident_opt, subst_value);
        let ret = match self.kind() {
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
            TypeKind::String => Type::string(&ctx),
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
        };

        let mut cache = unwrap!(SUBSTITUTE_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
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

        let key = (self.clone(), cutoff, lift_bumps);
        let cache = unwrap!(TRY_LIFT_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ctx = self.get_ctx().try_lift(cutoff, lift_bumps)?;
        let ret: Option<Type> = try {
            match self.kind() {
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
                TypeKind::String => Type::string(&ctx),
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
            }
        };

        let mut cache = unwrap!(TRY_LIFT_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
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
            TermKind::StringType => {
                Type::string(&ctx)
            },
            TermKind::PairType { head_ident_opt, head_type, tail_type } => {
                Type::pair(&ctx, head_ident_opt, head_type, tail_type)
            },
            TermKind::FuncType { arg_type, res_type } => {
                Type::func(&ctx, arg_type, res_type)
            },
            _ => {
                Type {
                    inner: TYPES.intern(TypeInner {
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
            inner: TYPES.intern(TypeInner {
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
            inner: TYPES.intern(TypeInner {
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
            inner: TYPES.intern(TypeInner {
                kind: TypeKind::Unit,
                ctx: ctx.clone(),
                hash,
            }),
        }
    }

    pub fn string(ctx: &Ctx) -> Type {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"String");
            hasher.finish()
        };
        Type {
            inner: TYPES.intern(TypeInner {
                kind: TypeKind::String,
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
            inner: TYPES.intern(TypeInner {
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
            inner: TYPES.intern(TypeInner {
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
            inner: TYPES.intern(TypeInner {
                kind: TypeKind::Func { arg, res },
                ctx: ctx.clone(),
                hash,
            }),
        }
    }
}

