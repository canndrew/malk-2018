pub struct Ctx {
    inner: Rc<CtxKind>,
}

enum CtxKind {
    Nil,
    Var {
        ident: Ident,
        ty: Type,
    },
}

impl Ctx {
    pub fn empty() -> Ctx {
        Ctx {
            inner: Rc::new(CtxKind::Nil),
        }
    }

    pub fn cons(ctx: Ctx, ty: Type) -> Ctx {
        assert_eq!(ctx, ty.ctx);
        Ctx {
            inner: Rc::new(CtxKind::Var {
                ty: ty,
            })
        }
    }

    pub fn lookup(&self, index: usize, name: Name) -> Type {
        match self.inner {
            CtxKind::Nil => panic!("invalid lookup"),
            CtxKind::Var { ident, ty } => {
                if index == 0 {
                    assert_eq!(name.bumps, 0);
                    assert_eq!(name.ident, ident);
                    ty
                }
            },
        }
    }
}

