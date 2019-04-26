use super::*;
use crate::parser::Ast;
use crate::core::{Term, Ident};

const MAX_LINE_WIDTH: usize = 80;

enum Chunk<'s> {
    Str(&'s str),
    String(String),
    Joined(Vec<Chunk<'s>>),
    Delimited {
        seperator: &'s str,
        sections: Vec<Chunk<'s>>,
        tail: Option<Box<Chunk<'s>>>,
    },
    Brackets {
        open: &'s str,
        close: &'s str,
        inner: Box<Chunk<'s>>,
    },
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Statement = 0,
    Operator = 1,
    App = 2,
    Enclosed = 3,
    Brackets = 4,
}

macro_rules! list_cons (
    ($t:ident, $cons:ident, $nil:ident, $name:ident, $head:ident, $tail:ident, $open:literal, $close:literal) => ({
        let mut head = $head;
        let mut tail = $tail;
        let mut name = $name;
        let mut elems = Vec::new();
        let tail = loop {
            let mut joined = Vec::new();
            if let Some(name) = name {
                joined.push(Chunk::Str(&name.node[..]));
                joined.push(Chunk::Str(" = "));
            }
            joined.push(head.prerender(Precedence::Operator));
            elems.push(Chunk::Joined(joined));
            match &*tail.node {
                $t::$cons { name: new_name, head: new_head, tail: new_tail } => {
                    name = new_name;
                    head = new_head;
                    tail = new_tail;
                },
                $t::$nil => break None,
                _ => break Some(Box::new(Chunk::Joined(vec![
                    Chunk::Str(".. "),
                    tail.prerender(Precedence::Operator),
                ]))),
            }
        };
        Chunk::Brackets {
            open: $open,
            close: $close,
            inner: Box::new(Chunk::Delimited {
                seperator: ",",
                sections: elems,
                tail: tail,
            }),
        }
    })
);

macro_rules! list_nil (($open:literal, $close:literal) => ({
    Chunk::Brackets {
        open: $open,
        close: $close,
        inner: Box::new(Chunk::Joined(Vec::new())),
    }
}));

macro_rules! at_left(($ident:ident, $left:ident) => ({
    match $ident {
        Some(name) => Chunk::Joined(vec![
            Chunk::Str("@"),
            name.prerender(),
            Chunk::Str(" = "),
            $left.prerender(Precedence::Operator),
        ]),
        None => Chunk::Joined(vec![
            Chunk::Str("@"),
            $left.prerender(Precedence::Operator),
        ]),
    }
}));

macro_rules! at_right(($right:ident) => ({
    Chunk::Joined(vec![
        Chunk::Str("@"),
        Chunk::Str(".. "),
        $right.prerender(Precedence::Operator),
    ])
}));

impl Term {
    fn prerender(&self, container_precedence: Precedence) -> Chunk<'_> {
        let chunk = match self {
            Term::Pair { name, head, tail } => list_cons!(Term, Pair, Unit, name, head, tail, "{", "}"),
            Term::Unit => list_nil!("{", "}"),
            Term::PairType { name, head, tail } => list_cons!(Term, PairType, UnitType, name, head, tail, "#{", "}"),
            Term::UnitType => list_nil!("#{", "}"),
            Term::EnumType { name, head, tail } => list_cons!(Term, EnumType, NeverType, name, head, tail, "@{", "}"),
            Term::NeverType => list_nil!("@{", "}"),
            Term::Case { name, head, tail } => list_cons!(Term, Case, Nil, name, head, tail, "[", "]"),
            Term::Nil => list_nil!("[", "]"),
            Term::CaseType { name, head, tail } => list_cons!(Term, CaseType, NilType, name, head, tail, "#[", "]"),
            Term::NilType => list_nil!("#[", "]"),
            Term::Var(ident) => ident.prerender(),
            Term::Type { bumps } => {
                let mut s = prerender_bumps(*bumps);
                s.push_str("Type");
                Chunk::String(s)
            },
            Term::MetaVar(i) => {
                Chunk::String(format!("?"))
            },
            /*
            Term::Level { bumps } => {
                let mut s = prerender_bumps(*bumps);
                s.push_str("Level");
                Chunk::String(s)
            },
            */
            Term::InjLeft { ident, left } => at_left!(ident, left),
            Term::InjRight { right } => at_right!(right),
            Term::Func { pat, body } => {
                Chunk::Joined(vec![
                    pat.prerender(Precedence::Enclosed),
                    Chunk::Str(" => "),
                    body.prerender(Precedence::Operator),
                ])
            },
            Term::FuncType { pat, body } => {
                Chunk::Joined(vec![
                    pat.prerender(Precedence::Enclosed),
                    Chunk::Str(" -> "),
                    body.prerender(Precedence::Operator),
                ])
            },
            Term::App { func, arg } => {
                Chunk::Joined(vec![
                    func.prerender(Precedence::App),
                    arg.prerender(Precedence::Brackets),
                ])
            },
            Term::Let { pat, expr, body } => {
                let mut pat = pat;
                let mut expr = expr;
                let mut body = body;
                let mut elems = Vec::new();
                let tail = loop {
                    elems.push(Chunk::Joined(vec![
                        Chunk::Str("let "),
                        pat.prerender(Precedence::Operator),
                        Chunk::Str(" = "),
                        expr.prerender(Precedence::Operator),
                    ]));
                    match &*body.node {
                        Term::Let { pat: new_pat, expr: new_expr, body: new_body } => {
                            pat = new_pat;
                            expr = new_expr;
                            body = new_body;
                        },
                        Term::Unit => break None,
                        body => break Some(Box::new(body.prerender(Precedence::Statement))),
                    }
                };
                Chunk::Delimited {
                    seperator: ";",
                    sections: elems,
                    tail: tail,
                }
            },
            Term::String(s) => {
                Chunk::String(format!("\"{}\"", s))
            },
            Term::Typed { term, ty } => {
                Chunk::Joined(vec![
                    term.prerender(Precedence::App),
                    Chunk::Str(": "),
                    ty.prerender(Precedence::App),
                ])
            },
        };
        let precedence = match self {
            Term::Unit |
            Term::UnitType |
            Term::Pair { .. } |
            Term::PairType { .. } |
            Term::EnumType { .. } |
            Term::NeverType |
            Term::Case { .. } |
            Term::CaseType { .. } |
            Term::Nil |
            Term::NilType => Precedence::Brackets,

            Term::MetaVar(..) |
            Term::Var(..) |
            Term::Type { .. } |
            //Term::Level { .. } |
            Term::String(..) => Precedence::Enclosed,

            Term::Typed { .. } |
            Term::InjLeft { .. } |
            Term::InjRight { .. } |
            Term::Func { .. } |
            Term::FuncType { .. } => Precedence::Operator,

            Term::App { .. } => Precedence::App,
            Term::Let { .. } => Precedence::Statement,
        };
        if precedence < container_precedence {
            Chunk::Brackets {
                open: "(",
                close: ")",
                inner: Box::new(chunk),
            }
        } else {
            chunk
        }
    }

    pub fn render(&self, before: &str, after: &str) -> String {
        self.prerender(Precedence::App).render(before, after)
    }
}

/*
impl Pat {
    fn prerender(&self, container_precedence: Precedence) -> Chunk {
        let chunk = match self {
            Pat::Pair { name, head, tail } => list_cons!(Pat, Pair, Unit, name, head, tail, "{", "}"),
            Pat::Unit => list_nil!("{", "}"),
            Pat::Bind(name) => Chunk::Str(&name.node[..]),
            Pat::ProjLeft { ident, left } => at_left!(ident, left),
            Pat::ProjRight { right } => at_right!(right),
            Pat::Typed { pat, ty } => {
                Chunk::Joined(vec![
                    pat.prerender(Precedence::App),
                    Chunk::Str(": "),
                    ty.prerender(Precedence::App),
                ])
            },
        };
        let precedence = match self {
            Pat::Bind(..) => Precedence::Enclosed,
            Pat::Unit |
            Pat::Pair { .. } => Precedence::Brackets,
            Pat::Typed { .. } |
            Pat::ProjLeft { .. } |
            Pat::ProjRight { .. } => Precedence::Operator,
        };
        if precedence < container_precedence {
            Chunk::Brackets {
                open: "(",
                close: ")",
                inner: Box::new(chunk),
            }
        } else {
            chunk
        }
    }
}
*/

impl Ident {
    fn prerender(&self) -> Chunk {
        let mut s = prerender_bumps(self.bumps);
        s.push_str(&self.name);
        Chunk::String(s)
    }
}

fn prerender_bumps(bumps: u32) -> String {
    let mut s = String::with_capacity(bumps as usize);
    for _ in 0..bumps {
        s.push('^');
    }
    s
}

impl<'s> Chunk<'s> {
    fn render(&self, before: &str, after: &str) -> String {
        match self {
            Chunk::Str(s) => String::from(*s),
            Chunk::String(s) => s.clone(),
            Chunk::Joined(chunks) => {
                let mut before = String::from(before);
                let mut ret = String::new();
                for chunk in chunks {
                    let next = chunk.render(&before, after);
                    let mut lines = next.lines();
                    if let (Some(line), Some(..)) = (lines.next_back(), lines.next_back()) {
                        before = String::from(line);
                    } else {
                        before.push_str(&next);
                    }
                    ret.push_str(&next);
                }
                ret
            },
            Chunk::Delimited { seperator, sections, tail } => {
                if let Some(inline) = self.try_render_inline() {
                    let full_width = before.width() + inline.width() + after.width();
                    if full_width <= MAX_LINE_WIDTH {
                        return inline;
                    }
                }

                let indent = before.len() - before.trim_start().len();
                let next_line_before = String::from(&before[..indent]);
                let mut ret = format!("\n{}", next_line_before);
                for section in sections {
                    let next = section.render(&next_line_before, "");
                    ret.push_str(&next);
                    ret.push_str(seperator);
                    ret.push('\n');
                    ret.push_str(&next_line_before);
                }
                if let Some(tail) = tail {
                    let next = tail.render(&next_line_before, after);
                    ret.push_str(&next);
                }
                ret
            },
            Chunk::Brackets { open, close, inner } => {
                if let Some(inner_inline) = inner.try_render_inline() {
                    let full_width = before.width() + open.width() + inner_inline.width() + close.width() + after.width();
                    if full_width <= MAX_LINE_WIDTH {
                        return format!("{}{}{}", open, inner_inline, close);
                    }
                }
                let indent = before.len() - before.trim_start().len();
                let next_line_before = format!("{}    ", &before[..indent]);
                let inner_rendered = inner.render(&next_line_before, "");
                format!("{}\n{}{}\n{}", open, next_line_before, inner_rendered, close)
            },
        }
    }

    fn try_render_inline(&self) -> Option<String> {
        match self {
            Chunk::Str(s) => {
                if s.width() <= MAX_LINE_WIDTH {
                    Some(String::from(*s))
                } else {
                    None
                }
            },
            Chunk::String(s) => {
                if s.width() <= MAX_LINE_WIDTH {
                    Some(s.clone())
                } else {
                    None
                }
            },
            Chunk::Joined(chunks) => {
                let mut ret = String::new();
                let mut width = 0;
                for chunk in chunks {
                    let next = chunk.try_render_inline()?;
                    width += next.width();
                    if width > MAX_LINE_WIDTH {
                        return None;
                    }
                    ret.push_str(&next);
                }
                Some(ret)
            },
            Chunk::Delimited { seperator, sections, tail } => {
                let mut ret = String::new();
                let mut width = 0;
                let mut iter = sections.iter().peekable();
                while let Some(section) = iter.next() {
                    let mut next = section.try_render_inline()?;
                    if iter.peek().is_some() || tail.is_some() {
                        next.push_str(seperator);
                        next.push(' ');
                    }
                    width += next.width();
                    if width > MAX_LINE_WIDTH {
                        return None;
                    }
                    ret.push_str(&next);
                }
                if let Some(tail) = tail {
                    let next = tail.try_render_inline()?;
                    width += next.width();
                    if width > MAX_LINE_WIDTH {
                        return None;
                    }
                    ret.push_str(&next);
                }
                Some(ret)
            },
            Chunk::Brackets { open, close, inner } => {
                let inner = inner.try_render_inline()?;
                if open.width() + inner.width() + close.width() > MAX_LINE_WIDTH {
                    return None;
                }
                Some(format!("{}{}{}", open, inner, close))
            },
        }
    }
}

