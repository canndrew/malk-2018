pub struct Type {
    inner: Rc<TypeInner>,
}

struct TypeInner {
    kind: TypeKind,
    ctx: Ctx,
}

enum TypeKind {
    Type {
        bumps: u32,
    },
    Equal {
        ty: Type,
        x0: Term,
        x1: Term,
    },
    Never,
    Unit,
    Pair {
        head: Type,
        tail: Type,
    },
    Func {
        arg: Type,
        res: Type,
    },
}

impl Type {
    pub fn universe(ctx: Ctx) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Universe,
                ctx: ctx,
            }),
        }
    }

    pub fn never(ctx: Ctx) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Never,
                ctx: ctx,
            }),
        }
    }

    pub fn unit(ctx: Ctx) -> Type {
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Unit,
                ctx: ctx,
            }),
        }
    }

    pub fn pair(ctx: Ctx, head: Type, tail: Type) -> Type {
        assert_eq!(ctx, head.ctx);
        assert_eq!(Ctx::cons(ctx, head), tail.ctx);
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Pair { head, tail },
                ctx: ctx,
            },
        }
    }

    pub fn func(ctx: Ctx, arg: Type, res: Type) -> Type {
        assert_eq!(ctx, arg.ctx);
        assert_eq!(Ctx::cons(ctx, arg), res.ctx);
        Type {
            inner: Rc::new(TypeInner {
                kind: TypeKind::Func { arg, res },
                ctx: ctx,
            }),
        }
    }
}

