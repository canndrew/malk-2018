// Attempts to find the unique inhabitant of type ty, if it exists.
pub fn solve(ctx: &Ctx, ty: &Type) -> Option<Term> {
    match ty.kind() {
        TypeKind::Embed(..) |
        TypeKind::Type { .. } |
        TypeKind::Never => None,

        TypeKind::Equal { ty, x0, x1 } => {
            // FIXME
            None
        },

        TypeKind::Unit => Term::unit(ctx),
        TypeKind::Pair { head, tail } => {
            let head_term = solve(ctx, head)?;
            let sub_ctx = ctx.define(head.clone(), head_term.clone());
            let tail_term = solve(&sub_ctx, tail)?;
            Some(Term::pair(ctx, head_term, tail_term))
        },

        TypeKind::Func { arg, res } => {
            match arg.refute() {
                Some(_refutation) => {
                    // FIXME
                    None
                },
                None => {
                    let sub_ctx = ctx.bind(arg.clone());
                    let res_term = solve(&sub_ctx, res)?;
                    Some(Term::func(ctx, arg, res_term))
                },
            }
        },
    }
}

