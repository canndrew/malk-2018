enum Origin {
    Span(Span),
}

struct Term {
    kind: TermKind,
    origin: Origin,
}

enum TermKind {
    UnitType,
    UnitTerm,
    PairType {
        head: Term,
        tail: Term,
    },
}

