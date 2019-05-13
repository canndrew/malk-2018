pub struct MetaCtx {
    next_meta_var_id: u32,
    meta_vars: HashMap<u32, MetaVar>,
}

pub struct MetaVar {
    r
}

impl MetaCtx {
    pub fn has_type<'c>(
        &mut self,
        ctx: &'c Ctx<'c>,
        ty: Ast<Term>,
        term: Ast<Term>,
    ) -> Result<(), Error> {
        match &*term.node {
            Term::Var(ident) => {
                match ctx.lookup(ident) {
                    Some(got_ty) => self.is_subtype(got_ty, ty),
                    None => bail!("unknown var"),
                }
            },
            Term::Type { bumps } => {
                let term_ty = Ast {
                    origin: Origin::TypeOf(term.origin.clone()),
                    node: Box::new(Term::Type { bumps: bumps + 1 }),
                };
                self.is_subtype(ctx, term_ty, ty)?;
            },
            Term::Unit => {
                let term_ty = Ast {
                    origin: Origin::TypeOf(term.origin.clone()),
                    node: Box::new(Term::Type { bumps: 0 }),
                };
                self.is_subtype(ctx, term_ty, ty)?;
            },
            Term::Pair { name, head, tail } => {
                let head_ty = self.new_meta_var(ctx);
                let sub_ctx = ctx.push(name, head_ty);
                let tail_ty = self.new_meta_var(&sub_ctx);
                let term_ty = Ast {
                    origin: Origin::TypeOf(term.origin.clone()),
                    node: Box::new(Term::PairType {
                        name: name.clone(),
                        head: head_ty,
                        tail: tail_ty,
                    }),
                };
                self.is_subtype(ctx, term_ty, ty)?;
                self.has_type(ctx, head, head_ty)?;
                self.has_type(&sub_ctx, tail, tail_ty)?;
            },
            Term::InjLeft { ident, left } => {
                let mut head_ty = self.new_meta_var();
                self.has_type(ctx, head_ty, left)?;

                let mut bumps = ident.node.map(|ident| ident.bumps).unwrap_or(0);
                let mut name = ident.node.map(|ident| ident.name);
                let mut tail_ty = self.new_meta_var();
                for depth in 0..bumps {
                    tail_ty = Ast {
                        node: Box::new(Term::EnumType {
                            name: name.clone(),
                            head: head_ty,
                            tail: tail_ty,
                        }),
                        origin: Origin::ImLazy,
                    };
                    head_ty = self.new_meta_var();
                }
                let term_ty = Ast {
                    origin: Origin::TypeOf(term.origin.clone()),
                    node: Box::new(Term::EnumType {
                        name: name.clone(),
                        head: head_ty,
                        tail: tail_ty,
                    }),
                };
                self.is_subtype(ctx, term_ty, ty)?;

            },
        }
    }

    pub fn is_subtype<'c>(&mut self, ctx: &'c Ctx<'c>, ty0: Ast<Term>, ty1: Ast<Term>) -> Result<(), Error> {
        match (&*ty0.node, &*ty1.node) {
            (Term::Var(i0), Term::Var(i1)) => ensure!(i0 == i1),
            (Term::Type { bumps: bumps0 }, Term::Type { bumps: bumps1 }) => ensure!(bumps0 <= bumps1),
            (Term::Level { bumps: bumps0 }, Term::Level { bumps: bumps1 }) => ensure!(bumps0 <= bumps1),
            (Term::UnitType, Term::UnitType) => Ok(()),
            (Term::NeverType, Term::NeverType) => Ok(()),
            (Term::NilType, Term::NilType) => Ok(()),
            (
                Term::PairType { name: name0, head: head0, tail: tail0 },
                Term::PairType { name: name1, head: head1, tail: tail1 },
            ) => {
                match name1 {
                    Some(name1) => {
                        if let Some(name0) = name0 {
                            ensure!(&**name0.node == &**name1.node),
                        }
                        is_subtype(ctx, head0, head1)?;
                        is_subtype(&ctx.push(name1, head0), tail0, tail1)?;
                    },
                    None => {
                        ensure!(name0.is_none());
                        is_subtype(ctx, head0, head1)?;
                        is_subtype(ctx, tail0, tail1)?;
                    },
                }
            },
            (
                Term::FuncType { pat: pat0, body: body0 }
                Term::FuncType { pat: pat1, body: body1 }
            ) => {
                //is_subtype(ctx, head0, head1)?;
                //is_subtype(&ctx.push(name1, head0), tail0, tail1)?;
            },
            (
                Term::EnumType { name: name0, head: head0, tail: tail0 },
                Term::EnumType { name: name1, head: head1, tail: tail1 },
            ) => {
                match name1 {
                    Some(name1) => {
                        if let Some(name0) = name0 {
                            ensure!(&**name0.node == &**name1.node),
                        }
                    },
                    None => ensure!(name0.is_none()),
                }
                is_subtype(ctx, head0, head1)?;
                is_subtype(ctx, tail0, tail1)?;
            },
            (
                Term::CaseType { name: name0, head: head0, tail: tail0 },
                Term::CaseType { name: name1, head: head1, tail: tail1 },
            ) => {
                match name1 {
                    Some(name1) => {
                        if let Some(name0) = name0 {
                            ensure!(&**name0.node == &**name1.node),
                        }
                    },
                    None => ensure!(name0.is_none()),
                }
                is_subtype(ctx, head0, head1)?;
                is_subtype(ctx, tail0, tail1)?;
            },
        }
    }
}

*/

