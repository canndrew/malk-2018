use super::*;
use syntax::{NameOpt, IdentOpt};

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Enclosed = 0,
    Ident = 1,
    App = 2,
    Equality = 3,
    Func = 4,
}

fn render_indent(f: &mut fmt::Formatter, indent: u32) -> fmt::Result {
    for _ in 0..indent {
        write!(f, "    ")?;
    }
    Ok(())
}

fn term_precedence(term: &Term) -> Precedence {
    match term.kind() {
        TermKind::Unit |
        TermKind::Pair { .. } |
        TermKind::UnitType |
        TermKind::NeverType |
        TermKind::StringType |
        TermKind::PairType { .. } => {
            Precedence::Enclosed
        },

        TermKind::Type { .. } |
        TermKind::Var { .. } => {
            Precedence::Ident
        },

        TermKind::App { .. } => {
            Precedence::App
        },

        TermKind::EqualType { .. } |
        TermKind::Refl { .. } => {
            Precedence::Equality
        },

        TermKind::FuncType { .. } |
        TermKind::Func { .. } => {
            Precedence::Func
        },

        TermKind::J { .. } |
        TermKind::Abort { .. } |
        TermKind::PairSplit { .. } => {
            unimplemented!()
        },
    }
}

fn type_precedence(ty: &Type) -> Precedence {
    match ty.kind() {
        TypeKind::Embed(term) => term_precedence(term),
        TypeKind::Type { .. } => Precedence::Ident,
        TypeKind::Equal { .. } => Precedence::Equality,
        TypeKind::Never |
        TypeKind::Unit |
        TypeKind::String |
        TypeKind::Pair { .. } => Precedence::Enclosed,
        TypeKind::Func { .. } => Precedence::Func,
    }
}

pub fn render_type(
    ty: &Type,
    f: &mut fmt::Formatter,
    indent: u32,
    precedence: Precedence,
) -> fmt::Result {
    let inner_precedence = type_precedence(ty);
    if inner_precedence > precedence {
        write!(f, "{{")?;
    }

    match ty.kind() {
        TypeKind::Embed(term) => {
            render_term(term, f, indent, Precedence::Func)?;
        },
        TypeKind::Type { level } => {
            write!(f, "Type '({}lev)", level)?;
        },
        TypeKind::Equal { x0, x1 } => {
            render_term(x0, f, indent, Precedence::App)?;
            write!(f, " #= ")?;
            render_term(x1, f, indent, Precedence::App)?;
        },
        TypeKind::Unit => {
            write!(f, "#()")?;
        },
        TypeKind::Never => {
            write!(f, "@[]")?;
        },
        TypeKind::String => {
            write!(f, "String")?;
        },
        TypeKind::Pair { head_ident_opt, head, tail } => {
            writeln!(f, "#(")?;
            let mut ident_opt = head_ident_opt.clone();
            let mut head = head.clone();
            let mut tail = tail.clone();
            loop {
                render_indent(f, indent + 1)?;
                match ident_opt {
                    IdentOpt::Real(ident) => write!(f, "{}: ", ident)?,
                    IdentOpt::Fake(s) => write!(f, "<{}>: ", s)?,
                }
                render_type(&head, f, indent + 1, Precedence::Func)?;
                writeln!(f, ",")?;
                match tail.kind() {
                    TypeKind::Pair { head_ident_opt, head: new_head, tail: new_tail } => {
                        ident_opt = head_ident_opt.clone();
                        head = new_head.clone();
                        tail = new_tail.clone();
                    },
                    TypeKind::Unit => break,
                    _ => {
                        render_indent(f, indent + 1)?;
                        write!(f, ".. ")?;
                        render_type(&tail, f, indent + 1, Precedence::Func)?;
                        writeln!(f, "")?;
                        break;
                    },
                }
            }
            write!(f, ")")?;
        },
        TypeKind::Func { arg, res } => {
            write!(f, "_: ")?;
            render_type(arg, f, indent, Precedence::Enclosed)?;
            write!(f, " -> ")?;
            render_type(res, f, indent, Precedence::Func)?;
        },
    }

    if inner_precedence > precedence {
        write!(f, "}}")?;
    }

    Ok(())
}

pub fn render_term(
    term: &Term,
    f: &mut fmt::Formatter,
    indent: u32,
    precedence: Precedence,
) -> fmt::Result {
    let inner_precedence = term_precedence(term);

    if inner_precedence > precedence {
        write!(f, "{{")?;
    }

    match term.kind() {
        TermKind::Type { .. } |
        TermKind::EqualType { .. } |
        TermKind::UnitType { .. } |
        TermKind::NeverType { .. } |
        TermKind::PairType { .. } |
        TermKind::StringType { .. } |
        TermKind::FuncType { .. } => {
            render_type(&Type::from_term(term.clone()), f, indent, Precedence::Func)?;
        },
        TermKind::Var { index, name_opt } => {
            match name_opt {
                NameOpt::Real(name) => write!(f, "{}", name)?,
                NameOpt::Fake(s) => write!(f, "<↑{} {}>", index, s)?,
            }
        },
        TermKind::Refl { x } => {
            render_term(x, f, indent, Precedence::App)?;
            write!(f, " == ")?;
            render_term(x, f, indent, Precedence::App)?;
        },
        TermKind::Unit => {
            write!(f, "()")?;
        },
        TermKind::Pair { head_ident_opt, head, tail, .. } => {
            writeln!(f, "(")?;
            let mut ident_opt = head_ident_opt.clone();
            let mut head = head.clone();
            let mut tail = tail.clone();
            loop {
                render_indent(f, indent + 1)?;
                match ident_opt {
                    IdentOpt::Real(ident) => write!(f, "{}: ", ident)?,
                    IdentOpt::Fake(s) => write!(f, "<{}>: ", s)?,
                }
                render_term(&head, f, indent + 1, Precedence::Func)?;
                writeln!(f, ",")?;
                match tail.kind() {
                    TermKind::Pair { head_ident_opt, head: new_head, tail: new_tail, .. } => {
                        ident_opt = head_ident_opt.clone();
                        head = new_head.clone();
                        tail = new_tail.clone();
                    },
                    TermKind::Unit => break,
                    _ => {
                        render_indent(f, indent + 1)?;
                        write!(f, ".. ")?;
                        render_term(&tail, f, indent + 1, Precedence::Func)?;
                        writeln!(f, "")?;
                        break;
                    },
                }
            }
            write!(f, ")")?;
        },
        TermKind::Func { arg_type, res } => {
            write!(f, "_: ")?;
            render_type(arg_type, f, indent, Precedence::Enclosed)?;
            write!(f, " => ")?;
            render_term(res, f, indent, Precedence::Func)?;
        },
        TermKind::App { func, arg } => {
            render_term(func, f, indent, Precedence::App)?;
            render_term(arg, f, indent, Precedence::Enclosed)?;
        },

        TermKind::J { .. } |
        TermKind::Abort { .. } |
        TermKind::PairSplit { .. } => {
            unimplemented!()
        },
    }

    if inner_precedence > precedence {
        write!(f, "}}")?;
    }

    Ok(())
}

