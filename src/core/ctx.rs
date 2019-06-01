use super::*;
use pretty_assertions::{assert_eq, assert_ne};

lazy_static! {
    static ref CTXS: Interner<CtxInner> = Interner::new();
    static ref BUMP_CACHE: Mutex<HashMap<(Ctx, u32, StrName, Type), Ctx>> = Mutex::new(HashMap::new());
    static ref SUBSTITUTE_CACHE: Mutex<HashMap<(Ctx, u32, StrName, Term), Ctx>> = Mutex::new(HashMap::new());
    static ref TRY_LIFT_CACHE: Mutex<HashMap<(Ctx, u32, u32), Option<Ctx>>> = Mutex::new(HashMap::new());
}

#[derive(Hash, Eq, Clone)]
pub struct Ctx {
    inner: Arc<CtxInner>,
}

impl PartialEq for Ctx {
    fn eq(&self, other: &Ctx) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl fmt::Debug for Ctx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner.kind, f)
    }
}

#[derive(PartialEq, Eq, Debug)]
struct CtxInner {
    kind: CtxKind,
    hash: u64,
}

impl Hash for CtxInner {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash.hash(hasher)
    }
}

#[derive(PartialEq, Eq, Debug)]
enum CtxKind {
    Nil,
    Var {
        parent: Ctx,
        name: StrName,
        ty: Type,
    },
}

impl Ctx {
    pub fn debug_summary(&self) -> String {
        let mut ret = self.debug_summary_inner();
        ret.push_str(" ⊦");
        ret
    }

    fn debug_summary_inner(&self) -> String {
        match self.kind() {
            CtxKind::Nil => String::from("·"),
            CtxKind::Var { parent, name, ty } => {
                let mut ret = parent.debug_summary_inner();
                ret.push_str(", ");
                if let TermKind::StringLit { ident } = name.kind() {
                    ret.push_str(ident.as_str());
                } else {
                    ret.push_str(&format!("${}", name.into_term()));
                }
                ret.push_str(&format!(": {}", ty.into_term()));
                ret
            },
        }
    }

    fn kind(&self) -> &CtxKind {
        &self.inner.kind
    }

    pub fn empty() -> Ctx {
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Nil");
            hasher.finish()
        };
        Ctx {
            inner: CTXS.intern(CtxInner {
                kind: CtxKind::Nil,
                hash,
            }),
        }
    }

    pub fn bind(&self, name: &StrName, ty: &Type) -> Ctx {
        let name = name.bump_into_ctx(&name.get_ctx(), self);
        let ty = ty.bump_into_ctx(&ty.get_ctx(), self);
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(b"Var");
            hasher.write_u64(name.get_hash());
            hasher.write_u64(ty.get_hash());
            hasher.finish()
        };
        let parent = self.clone();
        Ctx {
            inner: CTXS.intern(CtxInner {
                kind: CtxKind::Var { parent, name, ty },
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

    pub fn unbind(&self) -> (Ctx, StrName, Type) {
        match self.kind() {
            CtxKind::Nil => panic!("ctx is empty!"),
            CtxKind::Var { parent, name, ty } => {
                (parent.clone(), name.clone(), ty.clone())
            },
        }
    }

    pub fn bump(&self, index: u32, bump_name: &StrName, bump_ty: &Type) -> Ctx {
        let parent_ctx = self.nth_parent(index);
        let bump_ty = &bump_ty.bump_into_ctx(&bump_ty.get_ctx(), &parent_ctx);
        let bump_name = &bump_name.bump_into_ctx(&bump_name.get_ctx(), &parent_ctx);

        let key = (self.clone(), index, bump_name.clone(), bump_ty.clone());
        let cache = unwrap!(BUMP_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ret = if index == 0 {
            self.bind(bump_name, bump_ty)
        } else {
            match self.kind() {
                CtxKind::Nil => panic!("invalid bump"),
                CtxKind::Var { parent, name, ty } => {
                    let parent = parent.bump(index - 1, bump_name, bump_ty);
                    let ty = ty.bump_ctx(index - 1, bump_name, bump_ty);
                    let name = name.bump_ctx(index - 1, bump_name, bump_ty);
                    parent.bind(&name, &ty)
                },
            }
        };

        let mut cache = unwrap!(BUMP_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    pub fn substitute(
        &self,
        subst_index: u32,
        subst_name: &StrName,
        subst_value: &Term,
    ) -> Ctx {
        let parent_ctx = self.nth_parent(subst_index + 1);
        let subst_name = &subst_name.bump_into_ctx(&subst_name.get_ctx(), &parent_ctx);

        let key = (self.clone(), subst_index, subst_name.clone(), subst_value.clone());
        let cache = unwrap!(SUBSTITUTE_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ret = match self.kind() {
            CtxKind::Nil => panic!("invalid substitution on context"),
            CtxKind::Var { parent, name, ty } => {
                if subst_index == 0 {
                    assert_eq!(subst_name, name);
                    assert_eq!(*ty, subst_value.get_type());
                    assert_eq!(*parent, subst_value.get_ctx());
                    parent.clone()
                } else {
                    let parent = parent.substitute(subst_index - 1, subst_name, subst_value);
                    let ty = ty.substitute(subst_index - 1, subst_name, subst_value);
                    let name = name.substitute(subst_index - 1, subst_name, subst_value);
                    parent.bind(&name, &ty)
                }
            },
        };

        let mut cache = unwrap!(SUBSTITUTE_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    pub fn try_lift(&self, cutoff: u32, lift_bumps: u32) -> Option<Ctx> {

        let key = (self.clone(), cutoff, lift_bumps);
        let cache = unwrap!(TRY_LIFT_CACHE.lock());
        if let Some(x) = cache.get(&key) {
            return x.clone();
        };
        drop(cache);

        let ret: Option<Ctx> = try {
            if cutoff == 0 {
                self.nth_parent(lift_bumps)
            } else {
                match self.kind() {
                    CtxKind::Nil => panic!("invalid lifting operation"),
                    CtxKind::Var { parent, name, ty } => {
                        let parent = parent.try_lift(cutoff - 1, lift_bumps)?;
                        let ty = ty.try_lift_out_of_ctx(cutoff - 1, lift_bumps)?;
                        let name = name.try_lift_out_of_ctx(cutoff - 1, lift_bumps)?;
                        parent.bind(&name, &ty)
                    },
                }
            }
        };

        let mut cache = unwrap!(TRY_LIFT_CACHE.lock());
        cache.insert(key, ret.clone());
        ret
    }

    // returns the ident and unbumped type at the requested de brujin index
    /*
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
    */

    pub fn lookup_and_bump_out(&self, index: u32, lookup_name: &StrName) -> Type {
        let lookup_name = unwrap!(lookup_name.try_lift_out_of_ctx(0, 1));
        match self.kind() {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { parent, name, ty } => {
                if index == 0 {
                    assert_eq!(lookup_name, *name);
                    ty.clone()
                } else {
                    parent.lookup_and_bump_out(index - 1, &lookup_name)
                }
                .bump_ctx(0, name, ty)
            },
        }
    }

    pub fn try_lookup(&self, bumps: u32, lookup_name: &StrName) -> Option<(u32, Type)> {
        match self.kind() {
            CtxKind::Nil => None,
            CtxKind::Var { parent, name, ty } => {
                let lookup_name_unbumped = lookup_name.try_lift_out_of_ctx(0, 1)?;
                if lookup_name_unbumped == *name {
                    if bumps == 0 {
                        Some((0, ty.clone()))
                    } else {
                        parent.try_lookup(bumps - 1, &lookup_name_unbumped)
                    }
                } else {
                    parent.try_lookup(bumps, &lookup_name_unbumped)
                }
            },
        }
    }

    pub fn lookup_bumps(&self, index: u32, lookup_name: &StrName) -> u32 {
        let lookup_name_unbumped = unwrap!(lookup_name.try_lift_out_of_ctx(0, 1));
        match self.kind() {
            CtxKind::Nil => panic!("invalid call to lookup_bumps"),
            CtxKind::Var { parent, name, .. } => {
                if index == 0 {
                    assert_eq!(*name, lookup_name_unbumped);
                    0
                } else {
                    let parent_bumps = parent.lookup_bumps(index - 1, &lookup_name_unbumped);
                    parent_bumps + if lookup_name_unbumped == *name {
                        1
                    } else {
                        0
                    }
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

