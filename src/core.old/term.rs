use super::*;
use crate::parser::{Ast, Origin};
use crate::core::{Ident, IdentRef};
use lsp_types::{Range, Position};

#[derive(Clone)]
pub enum Term {
    MetaVar(u32),
    Var(Ident),
    Type {
        bumps: u32,
    },
    //Level {
        //bumps: u32,
    //},
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
        pat: Ast<Term>,
        body: Ast<Term>,
    },
    FuncType {
        pat: Ast<Term>,
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
    Nil,
    NilType,
    Let {
        pat: Ast<Term>,
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
    /*
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
            Term::Nil |
            Term::NilType |
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
    */

    /*
    pub fn occurances(&mut self, variable: Ast<IdentRef<'_>>) -> Result<Occurances, Error> {
        let ret = match &self.node {
            Term::Var(ident) => {
                if ident == variable {
                    Occurances::One
                } else {
                    Occurances::Zero
                }
            },
            Term::Type { .. } |
            Term::Level { .. }
            Term::Unit |
            Term::UnitType |
            Term::PairType { .. } |
            Term::EnumType { .. } |
            Term::NeverType,
            Term::FuncType { .. } |
            Term::CaseType { .. } |
            Term::NilType |
                => Occurances::Zero,
            Term::Pair { name, head, tail } => {
                let h = head.uses_linearly(variable);
                let t = tail.uses_linearly(variable.bump_name(name));
                h + t
            },
            Term::InjLeft { left, .. } => left.occurances(variable)?,
            Term::InjRight { right } => right.occurances(variable)?,
            Term::Func { body, .. } => body.occurances(variable)?,
            Term::App { func, arg } => {
                let f = func.occurances(variable)?;
                let a = arg.occurances(variable)?;
                f + a
            },
            Term::Case { head, tail, .. } => {
                let h = head.occurances(variable)?;
                let t = tail.occurances(variable)?;
                match (h, t) {
                    (Occurances::Ambiguous, x) => x,
                    (x, Occurances::Ambiguous) => x,
                    (Occurances::Zero, Occurances::Zero) => Occurances::Zero,
                    (Occurances::One, Occurances::One) => Occurances::One,
                    (Occurances::Many, Occurances::Many) => Occurances::Many,
                    _ => return Err(()),
                }
            },
            Term::Nil => Occurances::Ambiguous,
            Term::Let { pat, expr, body } => {
                let e = expr.occurances(variable)?;
                let b = expr.occurances(variable.bump_pat(pat))?;
                e.and(b)
            },
            Term::String(..) => Occurances::Zero,
            Term::Typed { term, .. } => term.occurances(variable)?,
        };
        Ok(ret)
    }
    */

    pub fn substitute(&mut self, variable: Ast<IdentRef<'_>>, value: Ast<Term>) {
        let value = Some(value);
        let mut substitution = Substitution { variable, value };
        substitution.fold_mut_term(self);
    }

    pub fn redex_at_position(&mut self, position: Position) -> Option<Ast<Term>> {
        let mut redex_at_position = RedexAtPosition {
            position,
            found: None,
        };
        redex_at_position.fold_mut_term(self);
        redex_at_position.found
    }

    /*
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
            Term::Nil |
            Term::NilType |
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
    */

    /*
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
    */
}


/*
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
*/

/*
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
*/

/*
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
*/

struct Substitution<'s> {
    variable: Ast<IdentRef<'s>>,
    value: Option<Ast<Term>>,
}

impl<'s> Fold for Substitution<'s> {
    type Scope = u32;

    fn pre_recurse_term(&mut self, term: &mut Ast<Term>) -> bool {
        match &mut *term.node {
            /*
            Term::Case { .. } |
            Term::Nil => {
                *term = Ast {
                    node: Term::Let {
                        pat: Term::Var(self.variable_name),
                        expr: unwrap!(self.value.take()),
                        body: *term,
                    },
                };
                true
            },
            */
            Term::Var(ident) => {
                if ident.name == self.variable.node.name {
                    if ident.bumps == self.variable.node.bumps {
                        *term = unwrap!(self.value.take());
                    } else if ident.bumps > self.variable.node.bumps {
                        ident.bumps -= 1;
                    }
                }
                true
            },
            _ => false,
        }
    }

    fn bump_name(&mut self, name: &str) {
        if name == self.variable.node.name {
            self.variable.node.bumps += 1;
        }
    }

    fn save_scope(&mut self) -> u32 {
        self.variable.node.bumps
    }

    fn restore_scope(&mut self, scope: u32) {
        self.variable.node.bumps = scope;
    }
}

struct RedexAtPosition {
    position: Position,
    found: Option<Ast<Term>>,
}

impl Fold for RedexAtPosition {
    type Scope = ();

    fn save_scope(&mut self) {
    }

    fn restore_scope(&mut self, (): ()) {
    }

    fn pre_recurse_term(&mut self, term: &mut Ast<Term>) -> bool {
        let range = match term.origin {
            Origin::Document { range, .. } => range,
            _ => panic!("wtf"),
        };
        !range.contains(self.position)
    }

    fn post_recurse_term(&mut self, term: &mut Ast<Term>) {
        if self.found.is_some() {
            return;
        }

        match &*term.node {
            Term::App { func, .. } => {
                match &*func.node {
                    Term::Func { .. } => {
                        self.found = Some(term.clone());
                    },
                    _ => (),
                }
            },
            _ => (),
        }
    }
}

trait Fold: Sized {
    type Scope;

    fn pre_recurse_term(&mut self, _term: &mut Ast<Term>) -> bool {
        false
    }

    fn post_recurse_term(&mut self, _term: &mut Ast<Term>) {
    }

    fn pre_recurse_pat(&mut self, _term: &mut Ast<Term>) -> bool {
        false
    }

    fn post_recurse_pat(&mut self, _term: &mut Ast<Term>) {
    }

    fn bump_name(&mut self, _name: &str) {
    }

    fn bump_name_opt(&mut self, name: &Option<Ast<String>>) {
        if let Some(ast) = name.as_ref() {
            self.bump_name(&*ast.node);
        }
    }

    fn visit_mut_name(&mut self, _name: &mut Option<Ast<String>>) {
    }

    fn visit_mut_ident(&mut self, _ident: &mut Option<Ast<Ident>>) {
    }

    fn save_scope(&mut self) -> Self::Scope;
    fn restore_scope(&mut self, scope: Self::Scope);

    fn fold_mut_term(&mut self, term: &mut Ast<Term>) {
        if self.pre_recurse_term(term) {
            return;
        };

        match &mut *term.node {
            Term::MetaVar(..) |
            Term::Var(..) |
            Term::Type { .. } |
            //Term::Level { .. } |
            Term::Unit |
            Term::UnitType |
            Term::NeverType |
            Term::Nil |
            Term::NilType |
            Term::String(..) => (),
            Term::Pair { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_term(head);
                let scope = self.save_scope();
                self.bump_name_opt(name);
                self.fold_mut_term(tail);
                self.restore_scope(scope);
            },
            Term::PairType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_term(head);
                let scope = self.save_scope();
                self.bump_name_opt(name);
                self.fold_mut_term(tail);
                self.restore_scope(scope);
            },
            Term::InjLeft { ident, left } => {
                self.visit_mut_ident(ident);
                self.fold_mut_term(left);
            },
            Term::InjRight { right } => {
                self.fold_mut_term(right);
            },
            Term::EnumType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_term(head);
                self.fold_mut_term(tail);
            },
            Term::Func { pat, body } => {
                let scope = self.save_scope();
                self.fold_mut_pat(pat);
                self.fold_mut_term(body);
                self.restore_scope(scope);
            },
            Term::FuncType { pat, body } => {
                let scope = self.save_scope();
                self.fold_mut_pat(pat);
                self.fold_mut_term(body);
                self.restore_scope(scope);
            },
            Term::App { func, arg } => {
                self.fold_mut_term(func);
                self.fold_mut_term(arg);
            },
            Term::Case { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_term(head);
                self.fold_mut_term(tail);
            },
            Term::CaseType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_term(head);
                self.fold_mut_term(tail);
            },
            Term::Let { pat, expr, body } => {
                let scope = self.save_scope();
                self.fold_mut_term(expr);
                self.fold_mut_pat(pat);
                self.fold_mut_term(body);
                self.restore_scope(scope);
            },
            Term::Typed { term, ty } => {
                self.fold_mut_term(term);
                self.fold_mut_term(ty);
            },
        }

        self.post_recurse_term(term);
    }

    fn fold_mut_pat(&mut self, pat: &mut Ast<Term>) {
        if self.pre_recurse_pat(pat) {
            return;
        };

        match &mut *pat.node {
            Term::MetaVar(..) |
            Term::Type { .. } |
            //Term::Level { .. } |
            Term::Unit |
            Term::UnitType |
            Term::NeverType |
            Term::Nil |
            Term::NilType |
            Term::String(..) => (),
            Term::Var(ident) => {
                self.bump_name(&ident.name);
            },
            Term::Pair { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_pat(head);
                let scope = self.save_scope();
                self.bump_name_opt(name);
                self.fold_mut_pat(tail);
                self.restore_scope(scope);
            },
            Term::PairType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_pat(head);
                let scope = self.save_scope();
                self.bump_name_opt(name);
                self.fold_mut_pat(tail);
                self.restore_scope(scope);
            },
            Term::InjLeft { ident, left } => {
                self.visit_mut_ident(ident);
                self.fold_mut_pat(left);
            },
            Term::InjRight { right } => {
                self.fold_mut_pat(right);
            },
            Term::EnumType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_pat(head);
                self.fold_mut_pat(tail);
            },
            Term::Func { pat, body } => {
                let scope = self.save_scope();
                self.fold_mut_term(pat);
                self.fold_mut_pat(body);
                self.restore_scope(scope);
            },
            Term::FuncType { pat, body } => {
                let scope = self.save_scope();
                self.fold_mut_term(pat);
                self.fold_mut_pat(body);
                self.restore_scope(scope);
            },
            Term::App { func, arg } => {
                self.fold_mut_pat(func);
                self.fold_mut_pat(arg);
            },
            Term::Case { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_pat(head);
                self.fold_mut_pat(tail);
            },
            Term::CaseType { name, head, tail } => {
                self.visit_mut_name(name);
                self.fold_mut_pat(head);
                self.fold_mut_pat(tail);
            },
            Term::Let { pat, expr, body } => {
                let scope = self.save_scope();
                self.fold_mut_pat(expr);
                self.fold_mut_term(pat);
                self.fold_mut_pat(body);
                self.restore_scope(scope);
            },
            Term::Typed { term, ty } => {
                self.fold_mut_pat(term);
                self.fold_mut_term(ty);
            },
        }

        self.post_recurse_pat(pat);
    }
}

