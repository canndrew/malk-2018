use super::*;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct StrName {
    term: Term,
}

impl StrName {
    pub fn from_term(term: Term) -> StrName {
        assert_eq!(term.get_type(), Type::string(&term.get_ctx()));
        StrName { term }
    }

    pub fn into_term(&self) -> Term {
        self.term.clone()
    }

    pub fn get_hash(&self) -> u64 {
        self.term.get_hash()
    }

    pub fn get_ctx(&self) -> Ctx {
        self.term.get_ctx()
    }

    pub fn kind(&self) -> &TermKind {
        self.term.kind()
    }

    pub fn as_term(&self) -> Term {
        self.term.clone()
    }

    pub fn lit<S: Into<String>>(ctx: &Ctx, s: S) -> StrName {
        let ident = Ident::new(s);
        let term = Term::string_lit(ctx, &ident);
        StrName { term }
    }

    pub fn bump_ctx(&self, bump_index: u32, bump_name: &StrName, bump_ty: &Type) -> StrName {
        let term = self.term.bump_ctx(bump_index, bump_name, bump_ty);
        StrName { term }
    }

    pub fn bump_into_ctx(&self, lo_ctx: &Ctx, hi_ctx: &Ctx) -> StrName {
        let term = self.term.bump_into_ctx(lo_ctx, hi_ctx);
        StrName { term }
    }

    pub fn substitute(&self, subst_index: u32, subst_name: &StrName, subst_value: &Term) -> StrName {
        let term = self.term.substitute(subst_index, subst_name, subst_value);
        StrName { term }
    }

    pub fn try_lift_out_of_ctx(&self, cutoff: u32, lift_bumps: u32) -> Option<StrName> {
        let term = self.term.try_lift_out_of_ctx(cutoff, lift_bumps)?;
        Some(StrName { term })
    }
}

impl Display for StrName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        render::render_name(f, self, 0)
    }
}

