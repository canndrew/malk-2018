use super::*;

use crate::syntax::Expr;

pub fn check_doc(expr: &Expr) -> Result<HasType, Error> {
    has_type(&Ctx::empty(), &Type::unit(&Ctx::empty()), expr)
}

// Finding a unique inhabitant of meta_arg_type, and substituting it into term produces a term of
// the required type.
#[derive(Debug)]
pub struct HasType {
    meta_arg_type: Type,
    term: Term,
}

pub fn has_type(ctx: &Ctx, ty: &Type, expr: &Expr) -> Result<HasType, Error> {
    assert_eq!(*ctx, ty.get_ctx());
    match expr {
        Expr::Var(name) => {
            let (index, var_ty) = match ctx.try_lookup(name) {
                Some(x) => x,
                None => bail!("unknown variable"),
            };
            let term = Term::var(ctx, index, Some(name.clone()));
            Ok(as_sub_type(ctx, ty, &var_ty, term))
        },
        Expr::Type { bumps } => {
            let term = Term::ty(ctx, *bumps);
            let term_ty = Type::ty(ctx, *bumps + 1);
            Ok(as_sub_type(ctx, ty, &term_ty, term))
        },
        Expr::Unit => {
            let term = Term::unit(ctx);
            let term_ty = Type::unit(ctx);
            Ok(as_sub_type(ctx, ty, &term_ty, term))
        },
        Expr::UnitType => {
            let term = Term::unit_type(ctx);
            let term_ty = Type::ty(ctx, 0);
            Ok(as_sub_type(ctx, ty, &term_ty, term))
        },
        /*
        Expr::Pair { ident, head, tail } => {
        },
        */
        /*
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
                                let (head, head_ty) = infer_type(ctx, head)?;
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
            // Do some context/type manipulation so that we can unify afterwards.

            // G, Arg: Type, Res: Arg -> Type
            let func_ctx = {
                // G, Arg: Type
                let func_ctx = ctx.bind(None, Type::ty(ctx, 0));
                // G, Arg: Type, arg: Arg
                let res_ctx = func_ctx.bind(None, Type::var(&func_ctx, 0));
                // G, Arg: Type, Res: Arg -> Type
                let func_ctx = ctx.bind(None, Type::func(
                    &func_ctx,
                    Type::var(&func_ctx, 0),
                    Type::ty(&res_ctx, 0),
                ));
                func_ctx
            };
            // G, Arg: Type, Res: Arg -> Type, arg: Arg
            let res_ctx = func_ctx.bind(None, Type::var(&func_ctx, 1));
            // G, Arg: Type, Res: Arg -> Type |- (arg: Arg) -> Res(arg)
            let func_ty = Type::func(
                &func_ctx,
                Type::var(&func_ctx, Type::var(&func_ctx, 1));
                Type::app(
                    &res_ctx,
                    Term::var(&res_ctx, 1),
                    Term::var(&res_ctx, 0),
                ),
            );
            let func_ht = has_type(&func_ctx, &func_ty, func);

            // G, Arg: Type, Res: Arg -> Type |- A
            let arg_ty = Type::var(&func_ctx, Type::var(&func_ctx, 1));
            let arg_ht = has_type(&func_ctx, &arg_ty, arg);

            


        },
        */
        _ => unimplemented!(),
    }
}

fn as_sub_type(
    ctx: &Ctx,
    hi_type: &Type,
    lo_type: &Type,
    term: Term,
) -> HasType {
    assert_eq!(*ctx, hi_type.get_ctx());
    assert_eq!(*ctx, lo_type.get_ctx());
    assert_eq!(term.get_type(), *lo_type);
    if hi_type == lo_type {
        return HasType {
            meta_arg_type: Type::unit(ctx),
            term: term,
        };
    }
    // TODO: add transformations for @[...] and #[...] types
    let hi_type = hi_type.into_term();
    let lo_type = lo_type.into_term();
    let meta_arg_type = Type::equal(ctx, lo_type, hi_type);
    let sub_ctx = Ctx::bind(ctx, None, meta_arg_type.clone());
    let term = transport(&sub_ctx, Term::var(&sub_ctx, 0, None), term);
    HasType { meta_arg_type, term }
}

fn transport(ctx: &Ctx, equality: Term, term: Term) -> Term {
    let (lo_type, hi_type) = match equality.get_type().kind() {
        TypeKind::Equal { x0, x1 } => (x0.clone(), x1.clone()),
        _ => panic!("invalid type for transport"),
    };
    let target_type_ctx = ctx.clone();
    let target_type_ctx = target_type_ctx.bind(None, lo_type.get_type());
    let target_type_ctx = target_type_ctx.bind(None, hi_type.get_type());
    let target_type_ctx = target_type_ctx.bind(None, Type::equal(&target_type_ctx, lo_type.clone(), hi_type.clone()));
    let target_type = Type::func(&target_type_ctx, Type::embed(&target_type_ctx, lo_type.clone()), Type::embed(&target_type_ctx, hi_type));

    let target_ctx = ctx.clone();
    let target_ctx = target_ctx.bind(None, lo_type.get_type());
    let res_ctx = target_ctx.bind(None, lo_type.get_type());
    let target = Term::func(&target_ctx, lo_type.get_type(), Term::var(&res_ctx, 0, None));

    Term::app(ctx,
        Term::j(ctx, target_type, target, equality),
        term,
    )
}


