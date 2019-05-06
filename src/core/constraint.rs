pub struct Constraint {
    inner: Rc<ConstraintKind>,
}

enum ConstraintKind {
    TypesEqual {
        ty_0: Type,
        ty_1: Type,
    },
    Telescope {
        entries: Vec<TelescopeEntry>,
    },
}

struct TelescopeEntry {
    term_0: Term,
    term_1: Term,
    ty: Type,
}

