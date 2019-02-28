grammar_macro::grammar! {
    mod grammar;
}

pub struct Ast<T> {
    node: T,
}

