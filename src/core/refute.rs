// Attempts to find a function of type ty -> @[], if one exists.
pub fn refute(ctx: &Ctx, ty: &Type) -> Option<Term> {
    match ty.kind() {
        TypeKind::Never => {
            Some(Term::func(ctx, Type::never(), Term::var(0)))
        },
    }
}

