use super::*;

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Enclosed = 0,
    Ident = 1,
    App = 2,
    Equality = 3,
    Func = 4,
}

pub fn debug_term(term: &Term) -> String {
    use std::fmt::Write;
    struct Ass<'a> {
        term: &'a Term,
    }
    impl<'a> Display for Ass<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            render_term(self.term, f, 0, Precedence::Func)
        }
    }

    let mut ret = String::new();
    write!(&mut ret, "{}", Ass { term });
    ret
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
        TermKind::StringLit { .. } |
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
            Precedence::Func
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
        TypeKind::Pair { head_name, head, tail } => {
            writeln!(f, "#(")?;
            let mut head_name = head_name.clone();
            let mut head = head.clone();
            let mut tail = tail.clone();
            loop {
                render_indent(f, indent + 1)?;
                render_name(f, &head_name, indent)?;
                write!(f, ": ")?;
                render_type(&head, f, indent + 1, Precedence::Func)?;
                writeln!(f, ",")?;
                match tail.kind() {
                    TypeKind::Pair { head_name: next_head_name, head: next_head, tail: next_tail } => {
                        head_name = next_head_name.clone();
                        head = next_head.clone();
                        tail = next_tail.clone();
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
            render_indent(f, indent)?;
            write!(f, ")")?;
        },
        TypeKind::Func { arg_name, arg, res } => {
            write!(f, "{{")?;
            render_name(f, arg_name, indent)?;
            write!(f, ": ")?;
            render_type(arg, f, indent, Precedence::Func)?;
            write!(f, "}} -> ")?;
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
        TermKind::Var { index, name } => {
            let bumps = term.get_ctx().lookup_bumps(*index, name);
            for _ in 0..bumps {
                write!(f, "^")?;
            }
            render_name(f, name, indent)?;
        },
        TermKind::StringLit { ident } => {
            write!(f, "\"{}\"", ident)?;
        },
        TermKind::Refl { x } => {
            render_term(x, f, indent, Precedence::App)?;
            write!(f, " == ")?;
            render_term(x, f, indent, Precedence::App)?;
        },
        TermKind::Unit => {
            write!(f, "()")?;
        },
        TermKind::Pair { head_name, head, tail, .. } => {
            writeln!(f, "(")?;
            let mut head_name = head_name.clone();
            let mut head = head.clone();
            let mut tail = tail.clone();
            loop {
                render_indent(f, indent + 1)?;
                render_name(f, &head_name, indent)?;
                write!(f, " = ")?;
                render_term(&head, f, indent + 1, Precedence::Func)?;
                writeln!(f, ",")?;
                match tail.kind() {
                    TermKind::Pair { head_name: next_head_name, head: next_head, tail: next_tail, .. } => {
                        head_name = next_head_name.clone();
                        head = next_head.clone();
                        tail = next_tail.clone();
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
            render_indent(f, indent)?;
            write!(f, ")")?;
        },
        TermKind::Func { arg_name, arg_type, res } => {
            render_name(f, arg_name, indent)?;
            write!(f, ": ")?;
            render_type(arg_type, f, indent, Precedence::Enclosed)?;
            write!(f, " => ")?;
            render_term(res, f, indent, Precedence::Func)?;
        },
        TermKind::App { func, arg } => {
            render_term(func, f, indent, Precedence::App)?;
            render_term(arg, f, indent, Precedence::Enclosed)?;
        },

        /*
        TermKind::J { target_type, target, elim } => {
            write!(f, "(")?;
            render_term(elim, f, indent, Precedence::App)?;
            write!(f, ") in {('")?;
            let target_type_ctx_0 = target_type.get_ctx();
            let (target_type_ctx_1, equality_name, _) = target_type_ctx_0.unbind();
            let (target_type_ctx_2, target_type_x1_name, _) = target_type_ctx_1.unbind();
            let (target_type_ctx_3, target_type_x0_name, _) = target_type_ctx_2.unbind();
            render_name(f, target_type_x0_name, indent)?;
            write!(f, ", '")?;
            render_name(f, target_type_x1_name, indent)?;
            write!(f, ", '")?;
            render_name(f, equality_name, indent)?;
            write!(f, " =   
        },
        */

        TermKind::J { .. } |
        TermKind::Abort { .. } => unimplemented!(),

        TermKind::PairSplit { target_type, target, elim } => {
            let target_type_ctx_0 = target_type.get_ctx();
            let (target_type_ctx_1, tail_name, _) = target_type_ctx_0.unbind();
            let (target_type_ctx_2, head_name, _) = target_type_ctx_1.unbind();

            render_term(elim, f, indent, Precedence::App)?;
            write!(f, " in (")?;
            render_name(f, &head_name, indent)?;
            write!(f, ", .. ")?;
            render_name(f, &tail_name, indent)?;
            write!(f, ") => ")?;
            render_term(target, f, indent, Precedence::Func)?;
        },
    }

    if inner_precedence > precedence {
        write!(f, "}}")?;
    }

    Ok(())
}

pub fn render_name(
    f: &mut fmt::Formatter,
    name: &StrName,
    indent: u32,
) -> fmt::Result {
    if let TermKind::StringLit { ident } = name.kind() {
        write!(f, "{}", ident.as_str())?;
    } else {
        write!(f, "$")?;
        render_term(&name.as_term(), f, indent, Precedence::Ident)?;
    }
    Ok(())
}

