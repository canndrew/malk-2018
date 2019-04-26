pub struct Term {
    inner: Rc<TermInner>,
}

struct TermInner {
    kind: TermKind,
    ty: Type,
}

enum TermKind {
    Type {
        bumps: u32,
    },
    Var {
        index: u32,
        name: Name,
    },
    FuncType {
        arg: Term,
        res: Term,
    },
    Abort {
        elim: Term,
    },
    Unit,
    Pair {
        head: Term,
        tail: Term,
    },
    Func {
        res: Term,
    },
}

impl Term {
    pub fn ty(ctx: Ctx) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Type,
                ty: Type::universe(ctx),
            }),
        }
    }

    pub fn var(ctx: Ctx, index: u32, name: Name) -> Term {
        let ty = ctx.lookup_var(index, name);
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Var { index, name },
                ty: ty,
            }),
        }
    }

    pub fn unit(ctx: Ctx) -> Term {
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Unit,
                ty: Type::unit(ctx),
            },
        }
    }

    pub fn pair(ctx: Ctx, head: Term, tail: Term) -> Term {
        assert_eq!(ctx, head.ty.ctx);
        assert_eq!(Ctx::cons(ctx, head.ty), tail.ty.ctx);
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Pair { head, tail },
                ty: Type::pair(head.ty, tail.ty),
            },
        }
    }

    pub fn func(ctx: Ctx, arg_ty: Type, res: Term) -> Term {
        assert_eq!(ctx, arg_ty.ctx);
        assert_eq!(Ctx::cons(ctx, arg_ty), res.ty.ctx);
        Term {
            inner: Rc::new(TermInner {
                kind: TermKind::Func { res },
                ty: Type::func(ctx, arg_ty, res),
            }),
        }
    }
}

