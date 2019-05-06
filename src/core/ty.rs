use super::*;
use crate::syntax::{Ident, Name};

#[derive(PartialEq, Debug, Clone)]
pub struct Type {
    inner: Rc<TypeInner>,
}

#[derive(PartialEq, Debug)]
struct TypeInner {
    kind: TypeKind,
    ctx: Ctx,
}

#[derive(PartialEq, Debug)]
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
        head_ident_opt: Option<Ident>,
        head: Type,
        tail: Type,
    },
    Func {
        arg: Type,
        res: Type,
    },
}

impl Type {
    pub fn into_term(&self) -> Term {
        let ctx = self.get_ctx();
        match self.kind() {
            TypeKind::Embed(term) => term.clone(),
            TypeKind::Type { level } => Term::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => Term::equal_type(&ctx, x0.clone(), x1.clone()),
            TypeKind::Never => Term::never_type(&ctx),
            TypeKind::Unit => Term::unit_type(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => Term::pair_type(&ctx, head_ident_opt.clone(), head.clone(), tail.clone()),
            TypeKind::Func { arg, res } => Term::func_type(&ctx, arg.clone(), res.clone()),
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

    pub fn kind(&self) -> &TypeKind {
        &self.inner.kind
    }

    pub fn bump_ctx(&self, index: u32, name: Option<&Name>, ty: Type) -> Type {
        let ctx = self.get_ctx().bump(index, name, ty.clone());
        match self.kind() {
            TypeKind::Embed(term) => Type::embed(&ctx, term.bump_ctx(index, name, ty)),
            TypeKind::Type { level } => Type::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.bump_ctx(index, name, ty.clone());
                let x1 = x1.bump_ctx(index, name, ty.clone());
                Type::equal(&ctx, x0, x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => {
                let head = head.bump_ctx(index, name, ty.clone());
                let name = name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail = tail.bump_ctx(index + 1, name.as_ref(), ty.clone());
                Type::pair(&ctx, head_ident_opt.clone(), head, tail)
            },
            TypeKind::Func { arg, res } => {
                let arg = arg.bump_ctx(index, name, ty.clone());
                let res = res.bump_ctx(index + 1, name, ty.clone());
                Type::func(&ctx, arg, res)
            },
        }
    }

    pub fn substitute(&self, subst_index: u32, subst_var_name: Option<&Name>, subst_value: Term) -> Type {
        let ctx = self.get_ctx().substitute(subst_index, subst_var_name, subst_value.clone());
        match self.kind() {
            TypeKind::Embed(term) => {
                let term = term.substitute(subst_index, subst_var_name, subst_value.clone());
                Type::embed(&ctx, term)
            },
            TypeKind::Type { level } => Type::ty(&ctx, *level),
            TypeKind::Equal { x0, x1 } => {
                let x0 = x0.substitute(subst_index, subst_var_name, subst_value.clone());
                let x1 = x1.substitute(subst_index, subst_var_name, subst_value.clone());
                Type::equal(&ctx, x0, x1)
            },
            TypeKind::Never => Type::never(&ctx),
            TypeKind::Unit => Type::unit(&ctx),
            TypeKind::Pair { head_ident_opt, head, tail } => {
                let head = head.substitute(subst_index, subst_var_name, subst_value.clone());
                let subst_var_name = subst_var_name.map(|n| n.bump_over_ident_opt(head_ident_opt.as_ref()));
                let tail = tail.substitute(subst_index + 1, subst_var_name.as_ref(), subst_value.clone());
                Type::pair(&ctx, head_ident_opt.clone(), head, tail)
            },
            TypeKind::Func { arg, res } => {
                let arg = arg.substitute(subst_index, subst_var_name, subst_value.clone());
                let res = res.substitute(subst_index + 1, subst_var_name, subst_value.clone());
                Type::func(&ctx, arg, res)
            },
        }
    }

    pub fn embed(ctx: &Ctx, term: Term) -> Type {
        assert_eq!(*ctx, term.get_ctx());
        match term.get_type().kind() {
            TypeKind::Type { .. } => (),
            _ => panic!("embedding a term which is not a type"),
        }
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Embed(term),
                ctx: ctx.clone(),
            }),
        }
    }

    pub fn ty(ctx: &Ctx, level: u32) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Type { level },
                ctx: ctx.clone(),
            }),
        }
    }

    pub fn never(ctx: &Ctx) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Never,
                ctx: ctx.clone(),
            }),
        }
    }

    pub fn unit(ctx: &Ctx) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Unit,
                ctx: ctx.clone(),
            }),
        }
    }
    
    pub fn equal(ctx: &Ctx, x0: Term, x1: Term) -> Type {
        assert_eq!(x0.get_type(), x1.get_type());
        assert_eq!(*ctx, x0.get_type().get_ctx());
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Equal { x0, x1 },
                ctx: ctx.clone(),
            }),
        }
    }

    pub fn pair(ctx: &Ctx, head_ident_opt: Option<Ident>, head: Type, tail: Type) -> Type {
        assert_eq!(*ctx, head.get_ctx());
        assert_eq!(Ctx::bind(ctx, head_ident_opt.clone(), head.clone()), tail.get_ctx());
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Pair { head_ident_opt, head, tail },
                ctx: ctx.clone(),
            }),
        }
    }

    pub fn func(ctx: &Ctx, arg: Type, res: Type) -> Type {
        assert_eq!(*ctx, arg.get_ctx());
        assert_eq!(Ctx::bind(ctx, None, arg.clone()), res.get_ctx());
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Func { arg, res },
                ctx: ctx.clone(),
            }),
        }
    }
}

