use super::*;

use crate::syntax::{IdentOpt, NameOpt};

// Tries to turn the type into a simpler type. Produces a term which can be substituted into a term
// predicated on the original type to turn it into a term predicated on the new type.
//
// eg.
//  #(#(), t: T) simplifies to (t: T) with substitution term ((), t)
pub fn simplify(ctx: &Ctx, ty: &Type) -> (Type, Term) {
    assert_eq!(*ctx, ty.get_ctx());
    /*
    match ty.kind() {
        TypeKind::Embed(..) |
        TypeKind::Type { .. } |
        TypeKind::Never |
        TypeKind::Unit => {
        */
            let ret_ty = ty.clone();
            let ret_ctx = ctx.bind(&IdentOpt::fake("meta_arg"), &ret_ty);
            let subst = Term::var(&ret_ctx, 0, &NameOpt::fake("meta_arg"));
            (ret_ty, subst)
        /*
        },
        TypeKind::Equal { x0, x1 } => {
            if x0 == x1 {
                let ret_ty = Type::unit(ctx);
                let ret_ctx = ctx.bind(&IdentOpt::none(), &ret_ty);
                let subst = Term::refl(&ret_ctx, &x0);
                (ret_ty, subst)
            } else {
                let ret_ty = ty.clone();
                let ret_ctx = ctx.bind(&IdentOpt::none(), &ret_ty);
                let subst = Term::var(&ret_ctx, 0, &NameOpt::none());
                (ret_ty, subst)
            }
        },
        TypeKind::Pair { head_ident_opt, head, tail } => {
            let (simplified_head, head_subst) = simplify(ctx, head);

            if let TypeKind::Unit = simplified_head.kind() {
                let substituted_tail = {
                    tail
                    .substitute(0, head_ident_opt, &Term::unit(ctx))
                };

                let (simplified_tail, tail_subst) = simplify(ctx, substituted_tail);

                unimplemented!()
            } else {
                if let TypeKind::Pair {
                    head_ident_opt: head_head_ident_opt,
                    head: head_head,
                    tail: head_tail,
                } = simplified_head.kind() {
                } else {
                    let new_tail = {
                        tail
                        .bump_ctx(1, head_ident_opt, &new_head)
                        .substitute(0, head_ident_opt, &subst_head)
                    };
                    let new_ret_ty = Type::pair(ctx, head_ident_opt, &new_head, &new_tail);
                    let ret_ctx = ctx.bind(&IdentOpt::none(), &new_ret_ty);
                    let bumped_head = head.bump_ctx(0, &IdentOpt::none(), &new_ret_ty);
                    let bumped_tail = tail.bump_ctx(1, &IdentOpt::none(), &new_ret_ty);
                    let pair_ctx = {
                        ret_ctx
                        .bind(head_ident_opt, &bumped_head)
                        .bind(&IdentOpt::none(), &bumped_tail)
                    };
                    let bumped_ret_ty = {
                        ret_ty
                        .bump_ctx(0, &IdentOpt::none(), &new_ret_ty)
                        .bump_ctx(0, head_ident_opt, &bumped_head)
                        .bump_ctx(0, &IdentOpt::none(), &bumped_tail)
                    };
                    let bumped_subst_head = {
                        subst_head
                        .bump_ctx(0, &IdentOpt::none(), &new_ret_ty)
                        .bump_ctx(0, head_ident_opt, &bumped_head)
                        .bump_ctx(0, &IdentOpt::none(), &bumped_tail)
                    };

                    let new_subst = Term::pair_split(
                        &ret_ctx,
                        &bumped_ret_ty,
                        &Term::pair(
                            &pair_ctx,
                            head_ident_opt,
                            &bumped_subst_head,
                            &Term::var(&pair_ctx, 0, &NameOpt::none()),
                        ),
                        &subst,
                    );
                }
            };
        },
        TypeKind::Func { arg, res } => {
            let (simplified_arg, arg_subst) = simplify(ctx, arg);
            match simplified_arg.kind() {
                TypeKind::Unit => {
                    // return simplified res
                },
                TypeKind::Never => {
                    // return unit
                },
                _ => {
                    // check if res is another function.
                    // if so, conglomerate the args into a struct.
                },
            }
        },
    }
    */
}

/*
pub fn unique_inhabitant(ctx: &Ctx, ty: &Type) -> Option<Term> {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Embed(..) => None,
        TypeKind::Type { .. } => None,
        TypeKind::Equal { x0, x1 } => {
            if x0 == x1 {
                Some(Term::refl(ctx, x0))
            } else {
                None
            }
        },
        TypeKind::Never => None,
        TypeKind::Unit => Some(Term::unit(ctx)),
        TypeKind::Pair { head_ident_opt, head, tail } => {
            let head_term = unique_inhabitant(ctx, head)?;
            let sub_ctx = ctx.bind(head_ident_opt, head);
            let tail_term = unique_inhabitant(&sub_ctx, tail)?;
            Some(Term::pair(ctx, head_ident_opt, &head_term, &tail_term))
        },
        TypeKind::Func { arg, res } => {
            match map_to_never(ctx, arg) {
                Some(map) => {
                    Some(Term::func(ctx, arg, &Term::abort(ctx, res, &Term::app(ctx, &map, &Term::var(ctx, 0, &NameOpt::none())))))
                },
                None => {
                    let res_term = unique_inhabitant(ctx, res)?;
                    Some(Term::func(ctx, arg, &res_term))
                },
            }
        },
    }
}

pub fn map_to_never(ctx: &Ctx, ty: &Type) -> Option<Term> {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Embed(..) => None,
        TypeKind::Type { .. } => None,
        TypeKind::Equal { .. } => None,
        TypeKind::Never => Some(Term::func(ctx, ty, &Term::var(ctx, 0, &NameOpt::none()))),
        TypeKind::Unit => None,
        //TypeKind::Pair { head_ident_opt, head, tail } => {
        TypeKind::Pair { .. } => {
            None
            /*
            match map_to_never(ctx, head) {
                Some(map) => {
                    Term::func(ctx, ty, Term::pair_split(
                },
            }
            */
        },
        //TypeKind::Func { arg, res } => None,
        TypeKind::Func { .. } => None,
    }
}
*/

