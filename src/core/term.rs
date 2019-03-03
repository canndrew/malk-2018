use super::*;
use crate::parser::{Ast, Origin};
use crate::core::{Ident, IdentRef, Bump, Pat};
use lsp_types::{Range, Position};

#[derive(Clone)]
pub enum Term {
    Var(Ident),
    Type {
        bumps: u32,
    },
    Level {
        bumps: u32,
    },
    Unit,
    UnitType,
    Pair {
        name: Option<Ast<String>>,
        head: Ast<Term>,
        tail: Ast<Term>,
    },
    PairType {
        name: Option<Ast<String>>,
        head: Ast<Term>,
        tail: Ast<Term>,
    },
    InjLeft {
        ident: Option<Ast<Ident>>,
        left: Ast<Term>,
    },
    InjRight {
        right: Ast<Term>,
    },
    EnumType {
        name: Option<Ast<String>>,
        head: Ast<Term>,
        tail: Ast<Term>,
    },
    NeverType,
    Func {
        pat: Ast<Pat>,
        body: Ast<Term>,
    },
    FuncType {
        pat: Ast<Pat>,
        body: Ast<Term>,
    },
    App {
        func: Ast<Term>,
        arg: Ast<Term>,
    },
    Case {
        name: Option<Ast<String>>,
        head: Ast<Term>,
        tail: Ast<Term>,
    },
    CaseType {
        name: Option<Ast<String>>,
        head: Ast<Term>,
        tail: Ast<Term>,
    },
    Nothing,
    NothingType,
    Let {
        pat: Ast<Pat>,
        expr: Ast<Term>,
        body: Ast<Term>,
    },
    String(String),
    Typed {
        term: Ast<Term>,
        ty: Ast<Term>,
    },
}

impl Ast<Term> {
    pub fn fold_mut<A: Bump>(
        &mut self,
        args: &A,
        folder: &impl Fn(&mut Ast<Term>, &A, bool) -> bool,
    ) -> bool {
        let mut children_changed = false;
        match &mut *self.node {
            Term::Type { .. } |
            Term::Level { .. } |
            Term::Unit |
            Term::UnitType |
            Term::NeverType |
            Term::Nothing |
            Term::NothingType |
            Term::Var(..) |
            Term::String(..) => (),

            Term::Pair { name, head, tail } => {
                children_changed |= head.fold_mut(args, folder);
                let args = &args.bump_over_name_opt(name);
                children_changed |= tail.fold_mut(args, folder);
            },
            Term::PairType { name, head, tail } => {
                children_changed |= head.fold_mut(args, folder);
                let args = &args.bump_over_name_opt(name);
                children_changed |= tail.fold_mut(args, folder);
            },
            Term::InjLeft { left, .. } => {
                children_changed |= left.fold_mut(args, folder);
            },
            Term::InjRight { right } => {
                children_changed |= right.fold_mut(args, folder);
            },
            Term::EnumType { head, tail, .. } => {
                children_changed |= head.fold_mut(args, folder);
                children_changed |= tail.fold_mut(args, folder);
            },
            Term::Func { pat, body } => {
                let args = &mut args.clone();
                children_changed |= pat.fold_mut(args, folder);
                children_changed |= body.fold_mut(args, folder);
            },
            Term::FuncType { pat, body } => {
                let args = &mut args.clone();
                children_changed |= pat.fold_mut(args, folder);
                children_changed |= body.fold_mut(args, folder);
            },
            Term::Case { head, tail, .. } => {
                children_changed |= head.fold_mut(args, folder);
                children_changed |= tail.fold_mut(args, folder);
            },
            Term::CaseType { head, tail, .. } => {
                children_changed |= head.fold_mut(args, folder);
                children_changed |= tail.fold_mut(args, folder);
            },
            Term::Let { pat, expr, body } => {
                children_changed |= expr.fold_mut(args, folder);
                let args = &mut args.clone();
                children_changed |= pat.fold_mut(args, folder);
                children_changed |= body.fold_mut(args, folder);
            },
            Term::Typed { term, ty } => {
                children_changed |= term.fold_mut(args, folder);
                children_changed |= ty.fold_mut(args, folder);
            },
            Term::App { func, arg } => {
                children_changed |= func.fold_mut(args, folder);
                children_changed |= arg.fold_mut(args, folder);
            },
        }
        folder(self, args, children_changed)
    }

    pub fn substitute(&mut self, variable: Ast<IdentRef<'_>>, value: Ast<Term>) -> bool {
        let substitution = Substitution { variable, value };
        self.fold_mut(&substitution, &|subject, substitution, children_changed| {
            if let Term::Var(ident) = &mut *subject.node {
                if ident.name == substitution.variable.node.name && ident.bumps >= substitution.variable.node.bumps {
                    if ident.bumps == substitution.variable.node.bumps {
                        *subject = substitution.value.clone();
                    } else {
                        ident.bumps -= 1;
                    }
                    return true;
                }
            }

            if children_changed {
                subject.origin = Origin::Substitute {
                    subject: Box::new(subject.origin.clone()),
                    variable: Box::new(substitution.variable.origin.clone()),
                    value: Box::new(substitution.value.origin.clone()),
                };
                return true;
            }

            false
        })
    }

    pub fn app_at_position(&self, position: Position) -> Option<&Ast<Term>> {
        if let Origin::Document { range, .. } = self.origin {
            if !range.contains(position) {
                return None;
            }
        }

        match &*self.node {
            Term::Type { .. } |
            Term::Level { .. } |
            Term::Unit |
            Term::UnitType |
            Term::NeverType |
            Term::Nothing |
            Term::NothingType |
            Term::Var(..) |
            Term::String(..) => (),

            Term::Pair { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::PairType { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::InjLeft { left, .. } => {
                if let Some(app) = left.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::InjRight { right } => {
                if let Some(app) = right.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::EnumType { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::Func { pat, body } => {
                if let Some(app) = pat.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = body.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::FuncType { pat, body } => {
                if let Some(app) = pat.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = body.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::Case { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::CaseType { head, tail, .. } => {
                if let Some(app) = head.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = tail.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::Let { pat, expr, body } => {
                if let Some(app) = expr.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = body.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = pat.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::Typed { term, ty } => {
                if let Some(app) = term.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = ty.app_at_position(position) {
                    return Some(app);
                }
            },
            Term::App { func, arg } => {
                if let Some(app) = func.app_at_position(position) {
                    return Some(app);
                }
                if let Some(app) = arg.app_at_position(position) {
                    return Some(app);
                }
                return Some(self);
            },
        }
        None
    }

    pub fn reduce_head(&self) -> Ast<Term> {
        match &*self.node {
            Term::App { func, arg } => {
                match &*func.node {
                    Term::Func { pat, body } => {
                        let (new_body, opt) = substitute_pat(body, pat, arg);
                        return match opt {
                            Some((new_pat, new_arg)) => {
                                Ast {
                                    node: Box::new(Term::App {
                                        func: Ast {
                                            node: Box::new(Term::Func {
                                                pat: new_pat,
                                                body: new_body,
                                            }),
                                            origin: func.origin.clone(),
                                        },
                                        arg: new_arg,
                                    }),
                                    origin: self.origin.clone(),
                                }
                            },
                            None => new_body,
                        };
                    },
                    _ => (),
                }
            },
            _ => (),
        }
        self.clone()
    }
}



fn substitute_pat(body: &Ast<Term>, pat: &Ast<Pat>, arg: &Ast<Term>) -> (Ast<Term>, Option<(Ast<Pat>, Ast<Term>)>) {
    match &*pat.node {
        Pat::Unit => {
            match &*arg.node {
                Term::Unit => {
                    return (body.clone(), None);
                },
                _ => (),
            }
        },
        Pat::Bind(name) => {
            let ident = Ast {
                node: Box::new(IdentRef {
                    name: name,
                    bumps: 0,
                }),
                origin: name.origin.clone(),
            };
            let mut body = body.clone();
            let _ = body.substitute(ident, arg.clone());
            return (body, None);
        },
        Pat::Pair { name: p_name, head: p_head, tail: p_tail } => {
            match &*arg.node {
                Term::Pair { name: t_name, head: t_head, tail: t_tail } => {
                    let names_match = match (p_name, t_name) {
                        (Some(a), Some(b)) => a.node == b.node,
                        _ => true,
                    };
                    if names_match {
                        let (body, opt) = substitute_pat(body, p_head, t_head);
                        return match opt {
                            Some((new_pat, new_arg)) => {
                                let new_pat = Ast {
                                    node: Box::new(Pat::Pair {
                                        name: p_name.clone(),
                                        head: new_pat,
                                        tail: p_tail.clone(),
                                    }),
                                    origin: pat.origin.clone(),
                                };
                                let new_arg = Ast {
                                    node: Box::new(Term::Pair {
                                        name: t_name.clone(),
                                        head: new_arg,
                                        tail: t_tail.clone(),
                                    }),
                                    origin: arg.origin.clone(),
                                };
                                (body, Some((new_pat, new_arg)))
                            },
                            None => {
                                substitute_pat(&body, p_tail, t_tail)
                            },
                        }
                    }
                },
                _ => (),
            }
        },
        Pat::ProjLeft { ident, left } => {
            match &*arg.node {
                Term::Case { name, head, tail } => {
                    return match ident {
                        None => substitute_pat(body, left, head),
                        Some(ident) => {
                            if Some(&ident.name) == name.as_ref().map(|ast| &*ast.node) {
                                if ident.bumps == 0 {
                                    substitute_pat(body, left, head)
                                } else {
                                    let new_pat = Ast {
                                        node: Box::new(Pat::ProjLeft {
                                            ident: Some(Ast {
                                                node: Box::new(Ident {
                                                    name: ident.name.clone(),
                                                    bumps: ident.bumps - 1,
                                                }),
                                                origin: ident.origin.clone(),
                                            }),
                                            left: left.clone(),
                                        }),
                                        origin: pat.origin.clone(),
                                    };
                                    substitute_pat(body, &new_pat, tail)
                                }
                            } else {
                                substitute_pat(body, pat, tail)
                            }
                        }
                    };
                },
                _ => (),
            }
        },
        Pat::ProjRight { right } => {
            match &*arg.node {
                Term::Case { tail, .. } => {
                    return substitute_pat(body, right, tail)
                },
                _ => (),
            }
        },
        Pat::Typed { pat, .. } => {
            return substitute_pat(body, pat, arg);
        },
    }
    (body.clone(), Some((pat.clone(), arg.clone())))
}

impl Bump for Ast<Term> {
    fn bump_over_name(&self, name: &str) -> Ast<Term> {
        let mut ret = self.clone();
        let variable = IdentRef {
            name: name,
            bumps: 0,
        };
        let _ = ret.fold_mut(&variable, &|term, variable, children_changed| {
            if let Term::Var(ident) = &mut *term.node {
                if variable.bumps <= ident.bumps {
                    ident.bumps += 1;
                    return true;
                }
            }
            children_changed
        });
        ret
    }
}

#[derive(Clone)]
struct Substitution<'s> {
    variable: Ast<IdentRef<'s>>,
    value: Ast<Term>,
}

impl<'s> Bump for Substitution<'s> {
    fn bump_over_name(&self, name: &str) -> Substitution<'s> {
        Substitution {
            variable: self.variable.bump_over_name(name),
            value: self.value.bump_over_name(name),
        }
    }
}

