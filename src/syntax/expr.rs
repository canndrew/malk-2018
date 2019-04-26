use super::*;
use crate::parser::{Ast, Origin};
use crate::syntax::Ident;
use lsp_types::{Range, Position};

#[derive(Clone)]
pub enum Expr {
    MetaVar,
    Var(Name),
    Type {
        bumps: u32,
    },
    Unit,
    UnitType,
    Pair {
        field: Option<Ast<Ident>>,
        head: Ast<Expr>,
        tail: Ast<Expr>,
    },
    PairType {
        field: Option<Ast<Ident>>,
        head: Ast<Expr>,
        tail: Ast<Expr>,
    },
    InjLeft {
        name: Option<Ast<Name>>,
        left: Ast<Expr>,
    },
    InjRight {
        right: Ast<Expr>,
    },
    EnumType {
        field: Option<Ast<Ident>>,
        head: Ast<Expr>,
        tail: Ast<Expr>,
    },
    NeverType,
    Func {
        pat: Ast<Expr>,
        body: Ast<Expr>,
    },
    FuncType {
        pat: Ast<Expr>,
        body: Ast<Expr>,
    },
    App {
        func: Ast<Expr>,
        arg: Ast<Expr>,
    },
    Case {
        field: Option<Ast<Ident>>,
        head: Ast<Expr>,
        tail: Ast<Expr>,
    },
    CaseType {
        field: Option<Ast<Ident>>,
        head: Ast<Expr>,
        tail: Ast<Expr>,
    },
    Nil,
    NilType,
    Let {
        pat: Ast<Expr>,
        expr: Ast<Expr>,
        body: Ast<Expr>,
    },
    String(String),
    Typed {
        term: Ast<Expr>,
        ty: Ast<Expr>,
    },
}


