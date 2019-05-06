use super::*;

pub fn unique_inhabitant(ctx: &Ctx, ty: &Type) -> Option<Term> {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Embed(..) => None,
        TypeKind::Type { .. } => None,
        TypeKind::Equal { x0, x1 } => {
            if x0 == x1 {
                Some(Term::refl(ctx, x0.clone()))
            } else {
                None
            }
        },
        TypeKind::Never => None,
        TypeKind::Unit => Some(Term::unit(ctx)),
        TypeKind::Pair { head_ident_opt, head, tail } => {
            let head_term = unique_inhabitant(ctx, head)?;
            let sub_ctx = ctx.bind(head_ident_opt.clone(), head.clone());
            let tail_term = unique_inhabitant(&sub_ctx, tail)?;
            Some(Term::pair(ctx, head_ident_opt.clone(), head_term, tail_term))
        },
        TypeKind::Func { arg, res } => {
            match map_to_never(ctx, arg) {
                Some(map) => {
                    Some(Term::func(ctx, arg.clone(), Term::abort(ctx, res.clone(), Term::app(ctx, map, Term::var(ctx, 0, None)))))
                },
                None => {
                    let res_term = unique_inhabitant(ctx, res)?;
                    Some(Term::func(ctx, arg.clone(), res_term))
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
        TypeKind::Never => Some(Term::func(ctx, ty.clone(), Term::var(ctx, 0, None))),
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

