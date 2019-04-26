pub fn as_sub_type(
    ctx: Ctx,
    hi_type: Type,
    lo_type: Type,
    term: Term,
) -> Result<Term, Error> {
    assert_eq!(ctx, hi_type.ctx());
    assert_eq!(ctx, lo_type.ctx());
    assert_eq!(term.ty(), lo_type);
    unimplemented!();
}

pub fn has_type(ctx: Ctx, ty: Type, expr: Expr) -> Result<Term, Error> {
    assert_eq!(ctx, ty.ctx());
    match expr {
        Expr::Var(name) => {
            let (index, var_ty) = ctx.try_lookup(name)?;
            let term = Term::var(ctx, index, name);
            as_sub_type(ctx, ty, var_ty, term)
        },
        Expr::Type { bumps } => {
            let term = Term::ty(ctx, bumps);
            let term_ty = Type::ty(ctx, bumps + 1);
            as_sub_type(ctx, ty, term_ty, term)
        },
        Expr::Unit => {
            let term = Term::unit(ctx);
            let term_ty = Type::unit(ctx);
            as_sub_type(ctx, ty, unit_ty, term)
        },
        Expr::UnitType => {
            let term = Term::unit_type(ctx);
            let term_ty = Type::ty(ctx, 0);
            as_sub_type(ctx, ty, term_ty, term)
        },
        /*
        Expr::Pair { ident, head, tail } => {
            let (ident, head_ty, tail_ty) = ty.expect_pair(ident)?;
            let head = has_type(ctx, head_ty, head)?;
            let tail = has_type(ctx, tail_ty.substitute(head, 0, ty_ident), tail)?;
            let term = Term::pair(ident, head, tail);
            let term_ty = Type::pair(ident, head_ty, tail_ty);

        },
        */
        Expr::Case { field, head, tail } => {
            match ty.kind() {
                TypeKind::Case { field: ty_field, head: head_ty, tail: tail_ty } => {
                    if field == ty_field || field.is_none() {
                        let head = has_type(ctx, head_ty, head)?;
                        let tail = has_type(ctx, tail_ty, tail)?;
                        Term::case(ctx, ty_field, head, tail)
                    } else {
                        let ident = field.unwrap();
                        let ty_ident = ty_field.unwrap();
                        let mut ty_heads = vec![(ty_ident, head_ty)];
                        let mut remaining = tail_ty;
                        loop {
                            if let TypeKind::Case {
                                field: ty_field,
                                head: head_ty,
                                tail: tail_ty,
                            } = remaining.kind() {
                                if let Some(ty_ident) = ty_field {
                                    if ty_ident == ident {
                                        let head = has_type(ctx, head_ty, head)?;
                                        let mut tail_ty = tail_ty;
                                        for (ty_ident, head_ty) in ty_heads.iter().rev() {
                                            tail_ty = Type::case(ctx, Some(ty_ident), head_ty, tail_ty);
                                        }
                                        let tail = has_type(ctx, tail_ty, tail)?;
                                        let mut term = tail.clone();
                                        for _ in 0..ty_heads.len() {
                                            term = Term::case_tail(ctx, term);
                                        };
                                        term = Term::case(ctx, Some(ident), head, term);
                                        for (i, (ty_ident, _)) in ty_heads.iter().enumerate().rev() {
                                            let mut head = tail.clone();
                                            for _ in 0..i {
                                                head = Term::case_tail(ctx, head);
                                            }
                                            term = Term::case(ctx, Some(ty_ident), head, term);
                                        }
                                        break term;
                                    } else if !ty_heads.any(|(seen, _)| seen == ty_ident) {
                                        ty_heads.push((ty_head, head_ty));
                                        continue;
                                    }
                                }
                            };
                            break {
                                let (head, head_ty) = get_type(ctx, head)?;
                                let tail = has_type(ctx, ty, tail)?;
                                let term = Term::case(ctx, Some(ident), head, tail);
                                let term = Term::case_tail(ctx, term);
                                term
                            };
                        }
                    }
                },
            }
        },
        Expr::App { func, arg } => {
            let (func, func_ty) = get_type(ctx, func)?;
            if let TypeKind::Func { arg: arg_ty, res: res_ty } = func_ty.kind() {
                let arg = has_type(ctx, arg_ty, arg)?;
                let res_ty = res_ty.substitute(arg, 0, None);
                let term = Term::app(ctx, func, arg);
                as_sub_type(ctx, ty, res_ty, term)
            }
        },
    }
}

