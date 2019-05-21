use super::*;

use crate::core::render;
use crate::syntax::{Expr, NameOpt, IdentOpt};

pub fn check_doc(expr: &Expr) -> Result<Term, Error> {
    let ctx = Ctx::empty();
    let expected_type = Type::unit(&ctx);
    println!("checking doc");
    let ht = has_type(&ctx, &expected_type, expr)?;
    println!("checked!");
    if let TypeKind::Unit = ht.meta_arg_type().kind() {
        return Ok(ht.term().substitute(0, &IdentOpt::fake("meta_arg"), &Term::unit(&ctx)));
    }

    struct Error { ht: HasType };

    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "unable to solve requirement: ")?;
            render::render_type(&self.ht.meta_arg_type(), f, 0, render::Precedence::Enclosed)
        }
    }

    bail!("{}", Error { ht })
}

// Finding a unique inhabitant of meta_arg_type, and substituting it into term produces a term of
// the required type.
#[derive(Debug)]
pub struct HasType {
    meta_arg_type: Type,
    term: Term,
}

impl HasType {
    pub fn meta_arg_type(&self) -> Type {
        self.meta_arg_type.clone()
    }

    pub fn term(&self) -> Term {
        self.term.clone()
    }

    pub fn new_from_split_ctx(ctx: &Ctx, index: u32, term: &Term) -> HasType {
        assert_eq!(*ctx, term.get_ctx());

        println!("entered new_from_split_ctx");
        println!("ctx == {:#?}", ctx.debug_summary());

        let mut ctx = ctx.clone();
        let mut meta_arg_type = Type::unit(&ctx);
        let mut term = term.bump_ctx(0, &IdentOpt::fake("meta_arg"), &meta_arg_type);
        for _ in 0..index {

            let (parent, ident_opt, ty) = ctx.unbind();
            let next_ctx = parent;
            let next_meta_arg_type = Type::pair(&next_ctx, &ident_opt, &ty, &meta_arg_type);
            let term_ctx = next_ctx.bind(&IdentOpt::fake("meta_arg"), &next_meta_arg_type);
            let bumped_term = term.bump_ctx(2, &IdentOpt::fake("meta_arg"), &next_meta_arg_type);
            term = Term::pair_split(
                &term_ctx,
                &bumped_term.get_type(),
                &bumped_term,
                &Term::var(&term_ctx, 0, &NameOpt::fake("meta_arg"))
            );
            ctx = next_ctx;
            meta_arg_type = next_meta_arg_type;
        }
        HasType::new(&ctx, &meta_arg_type, &term)
    }

    pub fn new(ctx: &Ctx, meta_arg_type: &Type, term: &Term) -> HasType {
        let (meta_arg_type, subst, _) = simplify(ctx, meta_arg_type);
        let term = {
            term
            .bump_ctx(1, &IdentOpt::fake("meta_arg"), &meta_arg_type)
            .substitute(0, &IdentOpt::fake("meta_arg"), &subst)
        };
        HasType { meta_arg_type, term }
    }

    /*
    pub fn unbump_out_of_ctx(&self, lo_ctx: &Ctx, hi_ctx: &Ctx) -> (Type, Term) {
        let mut hi_ctx = hi_ctx.clone();
        let mut meta_arg_type = self.meta_arg_type.clone();
        let mut term = self.term.clone();
        let mut bumps = 0;
        while *lo_ctx != hi_ctx {
            let (parent, ident_opt, bump_ty) = hi_ctx.unbind();
            meta_arg_type = Type::func(&parent, &bump_ty, &meta_arg_type);
            let new_term_ctx = hi_ctx.bump(1, &IdentOpt::fake("meta_arg"), &meta_arg_type);
            term = {
                term
                .bump_ctx(bumps + 2, &IdentOpt::fake("meta_arg"), &meta_arg_type)
                .substitute(bumps, &IdentOpt::fake("meta_arg"), &Term::app(
                    &new_term_ctx,
                    &Term::var(&new_term_ctx, 1, &NameOpt::fake("meta_arg")),
                    &Term::var(&new_term_ctx, 0, &NameOpt::new(&ident_opt, 0)),
                ))
            };
            hi_ctx = parent;
            bumps += 1;
        }
        (meta_arg_type, term)
    }
    */
}

pub fn has_type(ctx: &Ctx, ty: &Type, expr: &Expr) -> Result<HasType, Error> {
    assert_eq!(*ctx, ty.get_ctx());
    match expr {
        Expr::Var(name) => {
            let (index, var_ty) = match ctx.try_lookup(name) {
                Some(x) => x,
                None => bail!("unknown variable"),
            };
            let term = Term::var(ctx, index, &NameOpt::from(name.clone()));
            Ok(as_sub_type(ctx, ty, &var_ty, &term))
        },
        Expr::Type { bumps } => {
            let term = Term::ty(ctx, *bumps);
            let term_ty = Type::ty(ctx, *bumps + 1);
            Ok(as_sub_type(ctx, ty, &term_ty, &term))
        },
        Expr::Unit => {
            let term = Term::unit(ctx);
            let term_ty = Type::unit(ctx);
            Ok(as_sub_type(ctx, ty, &term_ty, &term))
        },
        Expr::UnitType => {
            let term = Term::unit_type(ctx);
            let term_ty = Type::ty(ctx, 0);
            Ok(as_sub_type(ctx, ty, &term_ty, &term))
        },
        Expr::Pair { field, head, tail } => {
            let ident_opt = match field {
                Some(ident) => IdentOpt::Real(ident.clone().into_inner()),
                None => IdentOpt::fake("anon_struct_field"),
            };

            let head_type_type = Type::ty(ctx, 0);
            let tail_type_ctx = ctx.bind(&IdentOpt::fake("Head"), &head_type_type);

            let tail_type_type = {
                let res_ty_ctx = tail_type_ctx.bind(&ident_opt, &Type::from_term(Term::var(&tail_type_ctx, 0, &NameOpt::fake("Head"))));
                Type::func(&tail_type_ctx, &Type::from_term(Term::var(&tail_type_ctx, 0, &NameOpt::fake("Head"))), &Type::ty(&res_ty_ctx, 0))
            };

            let head_ctx = {
                ctx
                .bind(&IdentOpt::fake("Head"), &head_type_type)
                .bind(&IdentOpt::fake("Tail"), &tail_type_type)
            };

            let head_ht = has_type(&head_ctx, &Type::from_term(Term::var(&head_ctx, 1, &NameOpt::fake("Head"))), head)?;

            let tail_ctx = head_ctx.bind(&IdentOpt::fake("meta_arg"), &head_ht.meta_arg_type());
            let tail_ht = has_type(&tail_ctx, &Type::from_term(Term::app(&tail_ctx, &Term::var(&tail_ctx, 1, &NameOpt::fake("Tail")), &head_ht.term())), tail)?;

            let pair_ctx = tail_ctx.bind(&IdentOpt::fake("meta_arg"), &tail_ht.meta_arg_type());
            let pair_tail_type_ctx = pair_ctx.bind(&ident_opt, &Type::from_term(Term::var(&pair_ctx, 3, &NameOpt::fake("Head"))));
            let pair_tail_type = Type::from_term(Term::app(
                &pair_tail_type_ctx,
                &Term::var(&pair_tail_type_ctx, 3, &NameOpt::fake("Tail")),
                &Term::var(&pair_tail_type_ctx, 0, &NameOpt::new(&ident_opt, 0)),
            ));
            let pair_type = {
                Type::pair(
                    &pair_ctx,
                    &ident_opt,
                    &Type::from_term(Term::var(&pair_ctx, 3, &NameOpt::fake("Head"))),
                    &pair_tail_type,
                )
            };
            let pair_term = {
                Term::pair(
                    &pair_ctx,
                    &ident_opt,
                    &head_ht.term().bump_ctx(0, &IdentOpt::fake("meta_arg"), &tail_ht.meta_arg_type()),
                    &pair_tail_type,
                    &tail_ht.term(),
                )
            };
            let pair_ht = as_sub_type(&pair_ctx, ty, &pair_type, &pair_term);
            let final_ctx = pair_ctx.bind(&IdentOpt::fake("meta_arg"), &pair_ht.meta_arg_type());
            let final_term = pair_ht.term();

            Ok(HasType::new_from_split_ctx(&final_ctx, 5, &final_term))
        },
        /*
        Expr::Case { field, head, tail } => {
            match ty.kind() {
                TypeKind::Case { field: ty_field, head: head_ty, tail: tail_ty } => {
                    if field == ty_field || field.is_none() {
                        let head = has_type(ctx, head_ty, head)?;
                        let tail = has_type(ctx, tail_ty, tail)?;
                        Term::case(ctx, ty_field, head, tail)
                    } else {
                        let ident = field.unwrap();
                        let ty_ident = ty_field.unwrap();
                        let mut ty_heads = vec![(ty_ident, head_ty)];
                        let mut remaining = tail_ty;
                        loop {
                            if let TypeKind::Case {
                                field: ty_field,
                                head: head_ty,
                                tail: tail_ty,
                            } = remaining.kind() {
                                if let Some(ty_ident) = ty_field {
                                    if ty_ident == ident {
                                        let head = has_type(ctx, head_ty, head)?;
                                        let mut tail_ty = tail_ty;
                                        for (ty_ident, head_ty) in ty_heads.iter().rev() {
                                            tail_ty = Type::case(ctx, Some(ty_ident), head_ty, tail_ty);
                                        }
                                        let tail = has_type(ctx, tail_ty, tail)?;
                                        let mut term = tail.clone();
                                        for _ in 0..ty_heads.len() {
                                            term = Term::case_tail(ctx, term);
                                        };
                                        term = Term::case(ctx, Some(ident), head, term);
                                        for (i, (ty_ident, _)) in ty_heads.iter().enumerate().rev() {
                                            let mut head = tail.clone();
                                            for _ in 0..i {
                                                head = Term::case_tail(ctx, head);
                                            }
                                            term = Term::case(ctx, Some(ty_ident), head, term);
                                        }
                                        break term;
                                    } else if !ty_heads.any(|(seen, _)| seen == ty_ident) {
                                        ty_heads.push((ty_head, head_ty));
                                        continue;
                                    }
                                }
                            };
                            break {
                                let (head, head_ty) = infer_type(ctx, head)?;
                                let tail = has_type(ctx, ty, tail)?;
                                let term = Term::case(ctx, Some(ident), head, tail);
                                let term = Term::case_tail(ctx, term);
                                term
                            };
                        }
                    }
                },
            }
        },
        */
        Expr::App { func, arg } => {
            // G |- Type: Type
            let arg_type_type = Type::ty(ctx, 0);

            // G, Arg: Type
            let res_type_ctx = ctx.bind(&IdentOpt::fake("Arg"), &arg_type_type);
            // G, Arg: Type, arg: Arg
            let res_type_res_ctx = res_type_ctx.bind(&IdentOpt::fake("arg"), &Type::from_term(Term::var(&res_type_ctx, 0, &NameOpt::fake("Arg"))));
            // G, Arg: Type |- (Arg -> Type): Type
            let res_type_type = Type::func(
                &res_type_ctx,
                &Type::from_term(Term::var(&res_type_ctx, 0, &NameOpt::fake("Arg"))),
                &Type::ty(&res_type_res_ctx, 0),
            );

            // G, Arg: Type, Res: Arg -> Type
            let func_ctx = res_type_ctx.bind(&IdentOpt::fake("Res"), &res_type_type);

            // G, Arg: Type, Res: Arg -> Type |- Arg: Type
            let arg_type = Type::from_term(Term::var(&func_ctx, 1, &NameOpt::fake("Arg")));
            // G, Arg: Type, Res: Arg -> Type, arg: Arg
            let res_type_ctx = func_ctx.bind(&IdentOpt::fake("arg"), &arg_type);
            // G, Arg: Type, Res: Arg -> Type, arg: Arg |- Res(arg): Type
            let res_type = Type::from_term(Term::app(
                &res_type_ctx,
                &Term::var(&res_type_ctx, 1, &NameOpt::fake("Res")),
                &Term::var(&res_type_ctx, 0, &NameOpt::fake("arg")),
            ));

            // G, Arg: Type, Res: Arg -> Type |- ((arg: Arg) -> Res(arg)): Type
            let func_type = Type::func(&func_ctx, &arg_type, &res_type);

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM |- func : (arg: Arg) -> Res(arg)
            let func_ht = has_type(&func_ctx, &func_type, func)?;

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM
            let arg_ctx = func_ctx.bind(&IdentOpt::fake("meta_arg"), &func_ht.meta_arg_type());

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM |- Arg: Type
            let arg_type = Type::from_term(Term::var(&arg_ctx, 2, &NameOpt::fake("Arg")));

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM |- arg: Arg
            let arg_ht = has_type(&arg_ctx, &arg_type, arg)?;

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM |- func : (arg: Arg) -> Res(arg)
            let func_term = func_ht.term().bump_ctx(0, &IdentOpt::fake("meta_arg"), &arg_ht.meta_arg_type());

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM
            let app_ctx = arg_ctx.bind(&IdentOpt::fake("meta_arg"), &arg_ht.meta_arg_type());

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM |- func(arg) : Res(arg)
            let app_term = Term::app(&app_ctx, &func_term, &arg_ht.term());

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM |- Res(arg): Type
            let app_type = Type::from_term(Term::app(
                &app_ctx,
                &Term::var(&app_ctx, 2, &NameOpt::fake("Res")),
                &arg_ht.term(),
            ));

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM, app_m: AppM |- app : T
            let app_ht = as_sub_type(&app_ctx, ty, &app_type, &app_term);

            // G, Arg: Type, Res: Arg -> Type, func_m: FuncM, arg_m: ArgM, app_m: AppM
            let final_ctx = app_ctx.bind(&IdentOpt::fake("meta_arg"), &app_ht.meta_arg_type());

            Ok(HasType::new_from_split_ctx(&final_ctx, 5, &app_ht.term()))
        },
        Expr::Func { pat, body } => {
            // G |- Arg: Type
            let arg_type = Type::ty(ctx, 0);

            // G, Arg: Type
            let res_type_ctx = ctx.bind(&IdentOpt::fake("Arg"), &arg_type);

            // G, Arg: Type, arg: Arg
            let res_type_res_type_ctx = res_type_ctx.bind(
                &IdentOpt::fake("arg"),
                &Type::from_term(Term::var(&res_type_ctx, 0, &NameOpt::fake("Arg"))),
            );

            // G, Arg: Type |- Res : Arg -> Type
            let res_type = Type::func(
                &res_type_ctx,
                &Type::from_term(Term::var(&res_type_ctx, 0, &NameOpt::fake("Arg"))),
                &Type::ty(&res_type_res_type_ctx, 0),
            );

            // G, Arg: Type, Res: Arg -> Type
            let outer_ctx = res_type_ctx.bind(&IdentOpt::fake("Res"), &res_type);

            // G, Arg: Type, Res: Arg -> Type, arg: Arg
            let pat_ctx = outer_ctx.bind(
                &IdentOpt::fake("arg"),
                &Type::from_term(Term::var(&outer_ctx, 1, &NameOpt::fake("Arg"))),
            );

            let pat_mp = check_pat(
                &pat_ctx,
                &Type::from_term(Term::var(&pat_ctx, 2, &NameOpt::fake("Arg"))),
                &Term::var(&pat_ctx, 0, &NameOpt::fake("arg")),
                pat,
            )?;

            let ext_ctx = pat_mp.ext_ctx();

            let body_type = {
                Type::from_term(Term::app(
                    &pat_ctx,
                    &Term::var(&pat_ctx, 1, &NameOpt::fake("Res")),
                    &Term::var(&pat_ctx, 0, &NameOpt::fake("arg")),
                ))
                .bump_into_ctx(&pat_ctx, &ext_ctx)
            };

            let body_ht = pat_mp.construct(has_type(&ext_ctx, &body_type, body)?);

            let pat_meta_shifted = Type::func(
                &outer_ctx,
                &Type::from_term(Term::var(&outer_ctx, 1, &NameOpt::fake("Arg"))),
                &pat_mp.meta_arg_type(),
            );

            let outer_ctx_with_pat_meta = outer_ctx.bind(&IdentOpt::fake("meta_arg"), &pat_meta_shifted);
            let pat_ctx_with_pat_meta = {
                outer_ctx_with_pat_meta
                .bind(&IdentOpt::fake("arg"), &Type::from_term(
                    Term::var(&outer_ctx_with_pat_meta, 2, &NameOpt::fake("Arg"))
                ))
            };

            let body_meta_shifted = Type::func(
                &outer_ctx_with_pat_meta,
                &Type::from_term(Term::var(&outer_ctx_with_pat_meta, 2, &NameOpt::fake("Arg"))),
                &{
                    body_ht
                    .meta_arg_type()
                    .bump_ctx(2, &IdentOpt::fake("meta_arg"), &pat_meta_shifted)
                    .substitute(0, &IdentOpt::fake("meta_arg"), &Term::app(
                        &pat_ctx_with_pat_meta,
                        &Term::var(&pat_ctx_with_pat_meta, 1, &NameOpt::fake("meta_arg")),
                        &Term::var(&pat_ctx_with_pat_meta, 0, &NameOpt::fake("arg")),
                    ))
                },
            );

            let outer_ctx_with_body_meta = outer_ctx_with_pat_meta.bind(&IdentOpt::fake("meta_arg"), &body_meta_shifted);
            let pat_ctx_with_body_meta = {
                outer_ctx_with_body_meta
                .bind(&IdentOpt::fake("arg"), &Type::from_term(
                    Term::var(&outer_ctx_with_body_meta, 3, &NameOpt::fake("Arg"))
                ))
            };

            let res = {
                body_ht
                .term()
                .bump_ctx(3, &IdentOpt::fake("meta_arg"), &pat_meta_shifted)
                .substitute(1, &IdentOpt::fake("meta_arg"), &Term::app(
                    &pat_ctx_with_pat_meta,
                    &Term::var(&pat_ctx_with_pat_meta, 1, &NameOpt::fake("meta_arg")),
                    &Term::var(&pat_ctx_with_pat_meta, 0, &NameOpt::fake("arg")),
                ))
                .bump_ctx(2, &IdentOpt::fake("meta_arg"), &body_meta_shifted)
                .substitute(0, &IdentOpt::fake("meta_arg"), &Term::app(
                    &pat_ctx_with_body_meta,
                    &Term::var(&pat_ctx_with_body_meta, 1, &NameOpt::fake("meta_arg")),
                    &Term::var(&pat_ctx_with_body_meta, 0, &NameOpt::fake("arg")),
                ))
            };

            let func = Term::func(
                &outer_ctx_with_body_meta,
                &Type::from_term(Term::var(&outer_ctx_with_body_meta, 3, &NameOpt::fake("Arg"))),
                &res,
            );

            let func_type = Type::func(
                &outer_ctx_with_body_meta,
                &Type::from_term(Term::var(&outer_ctx_with_body_meta, 3, &NameOpt::fake("Arg"))),
                &Type::from_term(Term::app(
                    &pat_ctx_with_body_meta,
                    &Term::var(&pat_ctx_with_body_meta, 3, &NameOpt::fake("Res")),
                    &Term::var(&pat_ctx_with_body_meta, 0, &NameOpt::fake("arg")),
                )),
            );

            let expected_type = ty.bump_into_ctx(ctx, &outer_ctx_with_body_meta);
            let func_ht = as_sub_type(&outer_ctx_with_body_meta, &expected_type, &func_type, &func);

            let final_ctx = outer_ctx_with_body_meta.bind(&IdentOpt::fake("meta_arg"), &func_ht.meta_arg_type());

            Ok(HasType::new_from_split_ctx(&final_ctx, 5, &func_ht.term()))
        },
        _ => unimplemented!(),
    }
}

pub struct MetaPat {
    meta_arg_type: Type,
    ext_ctx: Ctx,
    constructor: Box<Fn(HasType) -> HasType>,
}

impl MetaPat {
    pub fn ext_ctx(&self) -> Ctx {
        self.ext_ctx.clone()
    }

    pub fn meta_arg_type(&self) -> Type {
        self.meta_arg_type.clone()
    }

    pub fn construct(&self, ht: HasType) -> HasType {
        (self.constructor)(ht)
    }
}

// Takes a context and a type and term under that context. The pattern is used to eliminate the
// term. The MetaPat which is returned has a meta_arg_type under the original context, a
// constructor which embeds a meta-term under a pile of eliminators, and an extended context which
// is an extension of the original context, includes the meta_arg_type, and is the context under
// the eliminators. The constructor takes a meta-term under the extended context and returns a
// meta-term under the original context extended with just the pattern's meta_arg_type.
pub fn check_pat(
    ctx: &Ctx,
    elim_type: &Type,
    elim: &Term,
    pat: &Expr,
) -> Result<MetaPat, Error> {
    match pat {
        Expr::Var(name) => {
            if name.bumps() != 0 {
                bail!("variable references cannot appear in pattern position");
            }
            let ident = name.ident();
            let ident_opt = IdentOpt::from(ident);

            let meta_arg_type = Type::unit(ctx);
            let elim_type = elim_type.bump_ctx(0, &IdentOpt::fake("meta_arg"), &meta_arg_type);
            let ctx_with_meta = ctx.bind(&IdentOpt::fake("meta_arg"), &meta_arg_type);
            let ext_ctx = ctx_with_meta.bind(&ident_opt, &elim_type);

            let elim = elim.bump_ctx(0, &IdentOpt::fake("meta_arg"), &meta_arg_type);
            let constructor = {
                let ext_ctx = ext_ctx.clone();
                Box::new(move |ht: HasType| {
                    let term_meta = ht.meta_arg_type();
                    assert_eq!(ext_ctx, term_meta.get_ctx());
                    let constructed_meta = Type::func(
                        &ctx_with_meta,
                        &elim_type,
                        &term_meta,
                    );
                    let ctx_with_term_meta = ctx_with_meta.bind(&IdentOpt::fake("meta_arg"), &constructed_meta);
                    let elim_type = elim_type.bump_ctx(0, &IdentOpt::fake("meta_arg"), &constructed_meta);
                    let elim = elim.bump_ctx(0, &IdentOpt::fake("meta_arg"), &constructed_meta);
                    let ctx_under_func = ctx_with_term_meta.bind(&ident_opt, &elim_type);
                    let res = {
                        ht
                        .term()
                        .bump_ctx(2, &IdentOpt::fake("meta_arg"), &constructed_meta)
                        .substitute(0, &IdentOpt::fake("meta_arg"), &Term::app(
                            &ctx_under_func,
                            &Term::var(&ctx_under_func, 1, &NameOpt::fake("meta_arg")),
                            &Term::var(&ctx_under_func, 0, &NameOpt::new(&ident_opt, 0)),
                        ))
                    };
                    let term = Term::app(
                        &ctx_with_term_meta,
                        &Term::func(
                            &ctx_with_term_meta,
                            &elim_type,
                            &res,
                        ),
                        &elim,
                    );

                    HasType::new(&ctx_with_meta, &constructed_meta, &term)
                })
            };

            Ok(MetaPat { meta_arg_type, ext_ctx, constructor })
        },
        Expr::Unit => {
            let meta_arg_type = Type::equal(ctx, &elim_type.into_term(), &Term::unit_type(ctx));
            let ctx_with_meta = ctx.bind(&IdentOpt::fake("meta_arg"), &meta_arg_type);
            let ext_ctx = ctx_with_meta.bind(&IdentOpt::fake("unit"), &Type::unit(&ctx_with_meta));
            let elim = elim.bump_ctx(0, &IdentOpt::fake("meta_arg"), &meta_arg_type);
            let constructor = {
                let ext_ctx = ext_ctx.clone();
                Box::new(move |ht: HasType| {
                    let term_meta = ht.meta_arg_type();
                    assert_eq!(ext_ctx, term_meta.get_ctx());
                    let constructed_meta = term_meta.substitute(0, &IdentOpt::fake("unit"), &Term::unit(&ctx_with_meta));
                    let ctx_with_term_meta = ctx_with_meta.bind(&IdentOpt::fake("meta_arg"), &constructed_meta);
                    let ctx_under_func = ctx_with_term_meta.bind(&IdentOpt::fake("unit"), &Type::unit(&ctx_with_term_meta));
                    let elim = elim.bump_ctx(0, &IdentOpt::fake("meta_arg"), &constructed_meta);
                    let term = &Term::app(
                        &ctx_with_term_meta,
                        &Term::func(
                            &ctx_with_term_meta,
                            &Type::unit(&ctx_with_term_meta),
                            &{
                                ht
                                .term()
                                .bump_ctx(2, &IdentOpt::fake("meta_arg"), &constructed_meta)
                                .substitute(0, &IdentOpt::fake("meta_arg"), &Term::var(&ctx_under_func, 1, &NameOpt::fake("meta_arg")))
                            },
                        ),
                        &transport(
                            &ctx_with_term_meta,
                            &Term::var(&ctx_with_term_meta, 1, &NameOpt::fake("meta_arg")),
                            &elim,
                        ),
                    );
                    HasType::new(&ctx_with_meta, &constructed_meta, &term)
                })
            };
            Ok(MetaPat { meta_arg_type, ext_ctx, constructor })
        },
        _ => {
            unimplemented!()
        },
    }
}

fn as_sub_type(
    ctx: &Ctx,
    hi_type: &Type,
    lo_type: &Type,
    term: &Term,
) -> HasType {
    assert_eq!(*ctx, hi_type.get_ctx());
    assert_eq!(*ctx, lo_type.get_ctx());
    assert_eq!(term.get_type(), *lo_type);
    if hi_type == lo_type {
        return HasType {
            meta_arg_type: Type::unit(ctx),
            term: term.clone(),
        };
    }
    // TODO: add transformations for @[...] and #[...] types
    let hi_type = hi_type.into_term();
    let lo_type = lo_type.into_term();
    let meta_arg_type = Type::equal(ctx, &lo_type, &hi_type);
    let sub_ctx = Ctx::bind(ctx, &IdentOpt::fake("meta_arg"), &meta_arg_type);
    let term = transport(
        &sub_ctx,
        &Term::var(&sub_ctx, 0, &NameOpt::fake("meta_arg")),
        &term.bump_ctx(0, &IdentOpt::fake("meta_arg"), &meta_arg_type),
    );
    HasType::new(ctx, &meta_arg_type, &term)
}

fn transport(ctx: &Ctx, equality: &Term, term: &Term) -> Term {
    assert_eq!(*ctx, equality.get_ctx());
    assert_eq!(*ctx, term.get_ctx());

    let (lo_type, hi_type) = match equality.get_type().kind() {
        TypeKind::Equal { x0, x1 } => (x0.clone(), x1.clone()),
        _ => panic!("invalid type for transport"),
    };

    assert_eq!(lo_type, term.get_type().into_term());

    let target_type_ctx = ctx;
    let target_type_ctx = target_type_ctx.bind(&IdentOpt::fake("LoType"), &lo_type.get_type());
    let target_type_ctx = target_type_ctx.bind(&IdentOpt::fake("HiType"), &hi_type.get_type().bump_ctx(0, &IdentOpt::fake("LoType"), &lo_type.get_type()));
    let target_type_ctx = target_type_ctx.bind(&IdentOpt::fake("types_equal"), &Type::equal(
        &target_type_ctx,
        &Term::var(&target_type_ctx, 1, &NameOpt::fake("LoType")),
        &Term::var(&target_type_ctx, 0, &NameOpt::fake("HiType")),
    ));
    let target_type_res_ctx = {
        target_type_ctx
        .bind(&IdentOpt::fake("transport_arg"), &Type::from_term(Term::var(&target_type_ctx, 2, &NameOpt::fake("LoType"))))
    };
    let target_type = Type::func(
        &target_type_ctx,
        &Type::from_term(Term::var(&target_type_ctx, 2, &NameOpt::fake("LoType"))),
        &Type::from_term(Term::var(&target_type_res_ctx, 2, &NameOpt::fake("HiType"))),
    );

    let target_ctx = ctx;
    let target_ctx = target_ctx.bind(&IdentOpt::fake("EqType"), &lo_type.get_type());
    let arg_type = Type::from_term(Term::var(&target_ctx, 0, &NameOpt::fake("EqType")));
    let target_res_ctx = target_ctx.bind(&IdentOpt::fake("transport_arg"), &arg_type);
    let target = Term::func(&target_ctx, &arg_type, &Term::var(&target_res_ctx, 0, &NameOpt::fake("transport_arg")));

    Term::app(ctx,
        &Term::j(ctx, &target_type, &target, &equality),
        &term,
    )
}


