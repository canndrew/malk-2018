enum Term {
    UnitTerm,
    Lambda {
        body: Term,
    },
    App {
        func: Term,
        arg: Term,
    },
    InjLeft {
        term: Term,
    },
    InjRight {
        term: Term,
    },
    Match {
        arg: Term,
        on_left: Term,
        on_right: Term,
    },
}



