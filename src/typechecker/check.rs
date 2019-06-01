use super::*;

use crate::core::render;
use crate::syntax::Expr;
use pretty_assertions::{assert_eq, assert_ne};

pub fn check_doc(expr: &Expr) -> Result<Term, Error> {
    let ctx = Ctx::empty();
    let expected_type = Type::unit(&ctx);
    let ht = has_type(&ctx, &StrName::lit(&ctx, "doc_unit_constraint"), &expected_type, expr)?;
    if let TypeKind::Unit = ht.meta_arg_type().kind() {
        return Ok(ht.term().substitute(0, &ht.meta_arg_name(), &Term::unit(&ctx)));
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
    meta_arg_name: StrName,
    meta_arg_type: Type,
    term: Term,
}

impl HasType {
    pub fn meta_arg_type(&self) -> Type {
        self.meta_arg_type.clone()
    }

    pub fn meta_arg_name(&self) -> StrName {
        self.meta_arg_name.clone()
    }

    pub fn term(&self) -> Term {
        self.term.clone()
    }

    pub fn new_from_split_ctx(ctx: &Ctx, meta_arg_name: &StrName, index: u32, term: &Term) -> HasType {
        assert_eq!(*ctx, term.get_ctx());
        let mut ctx = ctx.clone();
        let mut meta_arg_type = Type::unit(&ctx);
        let terminator_name = StrName::lit(&ctx, "split_ctx_terminator");
        let mut term = term.bump_ctx(0, &terminator_name, &meta_arg_type);
        for _ in 0..index {
            let (parent, ident_opt, ty) = ctx.unbind();
            let next_ctx = parent;
            let next_meta_arg_type = Type::pair(&next_ctx, &ident_opt, &ty, &meta_arg_type);
            //let next_meta_arg_name = StrName::lit(&next_ctx, "split_ctx_tail");
            //let next_meta_arg_name = meta_arg_name.bump_into_ctx(&ctx, &next_ctx);
            let term_ctx = next_ctx.bind(meta_arg_name, &next_meta_arg_type);
            let bumped_term = term.bump_ctx(2, meta_arg_name, &next_meta_arg_type);
            term = Term::pair_split(
                &term_ctx,
                &bumped_term.get_type(),
                &bumped_term,
                /*
                &Term::var(&term_ctx, 0, &{
                    next_meta_arg_name
                    .bump_ctx(0, &next_meta_arg_name, &next_meta_arg_type)
                }),
                */
                &Term::var(&term_ctx, 0, meta_arg_name),
            );
            ctx = next_ctx;
            meta_arg_type = next_meta_arg_type;
        }
        HasType::new(&ctx, meta_arg_name, &meta_arg_type, &term)
    }

    pub fn new(ctx: &Ctx, meta_arg_name: &StrName, meta_arg_type: &Type, term: &Term) -> HasType {
        assert_eq!(ctx.bind(meta_arg_name, meta_arg_type), term.get_ctx());

        let (meta_arg_type, subst, _) = simplify(ctx, meta_arg_name, meta_arg_type);
        let term = {
            term
            .bump_ctx(1, meta_arg_name, &meta_arg_type)
            .substitute(0, &meta_arg_name.bump_ctx(0, &meta_arg_name, &meta_arg_type), &subst)
        };
        let meta_arg_name = meta_arg_name.clone();
        HasType { meta_arg_name, meta_arg_type, term }
    }
}

pub fn has_type(ctx: &Ctx, meta_arg_name: &StrName, ty: &Type, expr: &Expr) -> Result<HasType, Error> {
    assert_eq!(*ctx, ty.get_ctx());
    match expr {
        Expr::Var(name) => {
            let bumps = name.bumps();
            let name = StrName::from_term(Term::string_lit(ctx, &name.ident()));
            let (index, var_ty) = match ctx.try_lookup(bumps, &name) {
                Some(x) => x,
                None => bail!("unknown variable"),
            };
            let term = Term::var(ctx, index, &name);
            Ok(as_sub_type(ctx, meta_arg_name, ty, &var_ty, &term))
        },
        Expr::Type { bumps } => {
            let term = Term::ty(ctx, *bumps);
            let term_ty = Type::ty(ctx, *bumps + 1);
            Ok(as_sub_type(ctx, meta_arg_name, ty, &term_ty, &term))
        },
        Expr::Unit => {
            let term = Term::unit(ctx);
            let term_ty = Type::unit(ctx);
            Ok(as_sub_type(ctx, meta_arg_name, ty, &term_ty, &term))
        },
        Expr::UnitType => {
            let term = Term::unit_type(ctx);
            let term_ty = Type::ty(ctx, 0);
            Ok(as_sub_type(ctx, meta_arg_name, ty, &term_ty, &term))
        },
        Expr::Pair { field, head, tail } => {
            let head_name_name = StrName::lit(ctx, "head_name");
            let ctx_head_name = ctx.bind(&head_name_name, &Type::string(ctx));
            // ctx..
            // head_name: String

            let head_type_name = StrName::lit(&ctx_head_name, "HeadType");
            let head_type_type = Type::ty(&ctx_head_name, 0);
            let ctx_head_type = ctx_head_name.bind(&head_type_name, &head_type_type);
            // ctx..
            // head_name: String
            // HeadType: Type

            let tail_type_name = StrName::lit(&ctx_head_type, "TailType");
            let tail_type_type = {
                let head_name = StrName::from_term(Term::var(&ctx_head_type, 1, &head_name_name));
                let head_type = Type::from_term(Term::var(&ctx_head_type, 0, &head_type_name));
                let ctx_head = ctx_head_type.bind(&head_name, &head_type);
                // ctx..
                // head_name: String
                // HeadType: Type
                // $head_name: HeadType
                Type::func(&ctx_head_type, &head_name, &head_type, &Type::ty(&ctx_head, 0))
            };
            let ctx_tail_type = ctx_head_type.bind(&tail_type_name, &tail_type_type);
            // ctx..
            // head_name: String
            // HeadType: Type
            // TailType: ($head_name: HeadType) -> Type

            let head_name_meta_name = StrName::lit(&ctx_tail_type, "head_name_constraint");
            let head_name_meta_type = match field {
                Some(ident) => {
                    Type::equal(
                        &ctx_tail_type,
                        &Term::var(&ctx_tail_type, 2, &head_name_name),
                        &Term::string_lit(&ctx_tail_type, ident),
                    )
                },
                None => Type::unit(&ctx_tail_type),
            };
            let ctx_head_name_meta = ctx_tail_type.bind(&head_name_meta_name, &head_name_meta_type);
            // ctx..
            // head_name: String
            // HeadType: Type
            // TailType: ($head_name: HeadType) -> Type
            // head_name_constraint: HeadNameMeta

            let head_ht = {
                let head_meta_name = StrName::lit(&ctx_head_name_meta, "head_constraint");
                let head_type = Type::from_term(Term::var(&ctx_head_name_meta, 2, &head_type_name));
                has_type(&ctx_head_name_meta, &head_meta_name, &head_type, head)?
            };

            let head_meta_name = head_ht.meta_arg_name();
            let head_meta_type = head_ht.meta_arg_type();
            let head_term = head_ht.term();
            let ctx_head_meta = ctx_head_name_meta.bind(&head_meta_name, &head_meta_type);
            // ctx..
            // head_name: String
            // HeadType: Type
            // TailType: ($head_name: HeadType) -> Type
            // head_name_constraint: HeadNameMeta
            // head_constraint: HeadMeta

            let tail_ht = {
                let tail_meta_name = StrName::lit(&ctx_head_meta, "tail_constraint");
                let tail_type = Type::from_term(Term::app(
                    &ctx_head_meta,
                    &Term::var(&ctx_head_meta, 2, &tail_type_name),
                    &head_term,
                ));
                has_type(&ctx_head_meta, &tail_meta_name, &tail_type, tail)?
            };

            let tail_meta_name = tail_ht.meta_arg_name();
            let tail_meta_type = tail_ht.meta_arg_type();
            let tail_term = tail_ht.term();
            let ctx_tail_meta = ctx_head_meta.bind(&tail_meta_name, &tail_meta_type);
            // ctx..
            // head_name: String
            // HeadType: Type
            // TailType: ($head_name: HeadType) -> Type
            // head_name_constraint: HeadNameMeta
            // head_constraint: HeadMeta
            // tail_constraint: TailMeta

            let pair_ht = {
                let pair_meta_name = StrName::lit(&ctx_head_meta, "pair_subtype_constraint");
                let head_name = StrName::from_term(Term::var(&ctx_tail_meta, 5, &head_name_name));
                let head_type = Type::from_term(Term::var(&ctx_tail_meta, 4, &head_type_name));
                let ctx_head = ctx_tail_meta.bind(&head_name, &head_type);
                let tail_type = Type::from_term(Term::app(
                    &ctx_head,
                    &Term::var(&ctx_head, 4, &tail_type_name),
                    &Term::var(&ctx_head, 0, &head_name),
                ));
                let pair_type = Type::pair(&ctx_tail_meta, &head_name, &head_type, &tail_type);
                let pair_term = Term::pair(&ctx_tail_meta, &head_name, &head_term, &tail_type, &tail_term);
                as_sub_type(&ctx_tail_meta, &pair_meta_name, ty, &pair_type, &pair_term)
            };

            let pair_meta_name = pair_ht.meta_arg_name();
            let pair_meta_type = pair_ht.meta_arg_type();
            let pair_term = pair_ht.term();
            let pair_meta_ctx = ctx_tail_meta.bind(&pair_meta_name, &pair_meta_type);
            // ctx..
            // head_name: String
            // HeadType: Type
            // TailType: ($head_name: HeadType) -> Type
            // head_name_constraint: HeadNameMetaType
            // head_constraint: HeadMetaType
            // tail_constraint: TailMetaType
            // pair_subtype_constraint: PairMetaType

            Ok(HasType::new_from_split_ctx(&pair_meta_ctx, meta_arg_name, 7, &pair_term))
        },
        Expr::App { func, arg } => {
            let arg_name_name = StrName::lit(ctx, "arg_name");
            let ctx_arg_name = ctx.bind(&arg_name_name, &Type::string(ctx));
            // ctx..
            // arg_name: String

            let arg_type_name = StrName::lit(&ctx_arg_name, "ArgType");
            let arg_type_type = Type::ty(&ctx_arg_name, 0);
            let ctx_arg_type = ctx_arg_name.bind(&arg_type_name, &arg_type_type);
            // ctx..
            // arg_name: String
            // ArgType: Type

            let res_type_name = StrName::lit(&ctx_arg_type, "ResType");
            let res_type_type = {
                let arg_name = StrName::from_term(Term::var(&ctx_arg_type, 1, &arg_name_name));
                let arg_type = Type::from_term(Term::var(&ctx_arg_type, 0, &arg_type_name));
                let ctx_arg = ctx_arg_type.bind(&arg_name, &arg_type);
                Type::func(&ctx_arg_type, &arg_name, &arg_type, &Type::ty(&ctx_arg, 0))
            };
            let ctx_res_type = ctx_arg_type.bind(&res_type_name, &res_type_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type

            let func_meta_name = StrName::lit(&ctx_res_type, "func_constraint");
            let func_ht = {
                let arg_name = StrName::from_term(Term::var(&ctx_res_type, 2, &arg_name_name));
                let arg_type = Type::from_term(Term::var(&ctx_res_type, 1, &arg_type_name));
                let ctx_arg = ctx_arg_type.bind(&arg_name, &arg_type);
                let res_type = Type::from_term(Term::app(
                    &ctx_arg,
                    &Term::var(&ctx_arg, 1, &res_type_name),
                    &Term::var(&ctx_arg, 0, &arg_name),
                ));
                let func_type = Type::func(&ctx_res_type, &arg_name, &arg_type, &res_type);
                has_type(&ctx_res_type, &func_meta_name, &func_type, func)?
            };
            let func_meta_name = func_ht.meta_arg_name();
            let func_meta_type = func_ht.meta_arg_type();
            let func_term = func_ht.term();
            let ctx_func_meta = ctx_res_type.bind(&func_meta_name, &func_meta_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type
            // func_constraint: FuncMeta

            let arg_meta_name = StrName::lit(&ctx_func_meta, "arg_constraint");
            let arg_ht = {
                let arg_type = Type::from_term(Term::var(&ctx_func_meta, 2, &arg_type_name));
                has_type(&ctx_func_meta, &arg_meta_name, &arg_type, arg)?
            };
            let arg_meta_name = arg_ht.meta_arg_name();
            let arg_meta_type = arg_ht.meta_arg_type();
            let arg_term = arg_ht.term();
            let ctx_arg_meta = ctx_func_meta.bind(&arg_meta_name, &arg_meta_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type
            // func_constraint: FuncMeta
            // arg_constraint: ArgMeta

            let app_ht = {
                let app_meta_name = StrName::lit(&ctx_arg_meta, "app_subtype_constraint");
                let app_type = Type::from_term(Term::app(
                    &ctx_arg_meta,
                    &Term::var(&ctx_arg_meta, 2, &res_type_name),
                    &arg_term,
                ));
                let app_term = Term::app(
                    &ctx_arg_meta,
                    &func_term,
                    &arg_term,
                );
                as_sub_type(&ctx_arg_meta, &app_meta_name, ty, &app_type, &app_term)
            };
            let app_meta_name = app_ht.meta_arg_name();
            let app_meta_type = app_ht.meta_arg_type();
            let app_term = app_ht.term();
            let ctx_app_meta = ctx_arg_meta.bind(&app_meta_name, &app_meta_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type
            // func_constraint: FuncMeta
            // arg_constraint: ArgMeta
            // app_subtype_constraint: AppMeta

            Ok(HasType::new_from_split_ctx(&ctx_app_meta, meta_arg_name, 6, &app_term))
        },
        Expr::Func { pat, body } => {
            let arg_name_name = StrName::lit(ctx, "arg_name");
            let ctx_arg_name = ctx.bind(&arg_name_name, &Type::string(ctx));
            // ctx..
            // arg_name: String

            let arg_type_name = StrName::lit(&ctx_arg_name, "ArgType");
            let arg_type_type = Type::ty(&ctx_arg_name, 0);
            let ctx_arg_type = ctx_arg_name.bind(&arg_type_name, &arg_type_type);
            // ctx..
            // arg_name: String
            // ArgType: Type

            let res_type_name = StrName::lit(&ctx_arg_type, "ResType");
            let res_type_type = {
                let arg_name = StrName::from_term(Term::var(&ctx_arg_type, 1, &arg_name_name));
                let arg_type = Type::from_term(Term::var(&ctx_arg_type, 0, &arg_type_name));
                let ctx_arg = ctx_arg_type.bind(&arg_name, &arg_type);
                Type::func(&ctx_arg_type, &arg_name, &arg_type, &Type::ty(&ctx_arg, 0))
            };
            let ctx_res_type = ctx_arg_type.bind(&res_type_name, &res_type_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type

            let arg_pht = {
                let arg_meta_name = StrName::lit(&ctx_res_type, "function_arg_subtype_constraint");
                let arg_name = StrName::from_term(Term::var(&ctx_res_type, 2, &arg_name_name));
                let arg_type = Type::from_term(Term::var(&ctx_res_type, 1, &arg_type_name));
                check_pat(&ctx_res_type, &arg_meta_name, &arg_name, &arg_type, pat)?
            };
            let arg_ext_ctx = arg_pht.ext_ctx();
            let arg_func_arg = arg_pht.func_arg();

            let res_type = Type::from_term(Term::app(
                &arg_ext_ctx,
                &{
                    Term::var(&ctx_res_type, 0, &res_type_name)
                    .bump_into_ctx(&ctx_res_type, &arg_ext_ctx)
                },
                &arg_func_arg,
            ));
            let res_meta_name = StrName::lit(&arg_ext_ctx, "function_body_subtype_constraint");
            let res_ht = has_type(&arg_ext_ctx, &res_meta_name, &res_type, body)?;
            let constructed_ht = arg_pht.construct(res_ht);
            let constructed_meta_name = constructed_ht.meta_arg_name();
            let constructed_meta_type = constructed_ht.meta_arg_type();
            let constructed_term = constructed_ht.term();
            let ctx_constructed_meta = ctx_res_type.bind(&constructed_meta_name, &constructed_meta_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type
            // _: ConstructedMeta

            let func_ht = {
                let func_meta_name = StrName::lit(&ctx_constructed_meta, "function_subtype_constraint");
                let arg_name = StrName::from_term(Term::var(&ctx_constructed_meta, 3, &arg_name_name));
                let arg_type = Type::from_term(Term::var(&ctx_constructed_meta, 2, &arg_type_name));
                let ctx_arg = ctx_constructed_meta.bind(&arg_name, &arg_type);
                let func_type = Type::func(
                    &ctx_constructed_meta,
                    &arg_name,
                    &arg_type,
                    &Type::from_term(Term::app(
                        &ctx_arg,
                        &Term::var(&ctx_arg, 2, &res_type_name),
                        &Term::var(&ctx_arg, 0, &arg_name),
                    )),
                );
                as_sub_type(&ctx_constructed_meta, &func_meta_name, ty, &func_type, &constructed_term)
            };
            let func_meta_name = func_ht.meta_arg_name();
            let func_meta_type = func_ht.meta_arg_type();
            let func_term = func_ht.term();
            let ctx_func_meta = ctx_constructed_meta.bind(&func_meta_name, &func_meta_type);
            // ctx..
            // arg_name: String
            // ArgType: Type
            // ResType: ($arg_name: ArgType) -> Type
            // _: ConstructedMeta
            // function_subtype_constraint: FuncMeta

            Ok(HasType::new_from_split_ctx(&ctx_func_meta, meta_arg_name, 5, &func_term))
        },
        _ => unimplemented!(),
    }
}

pub struct PatHasType {
    ext_ctx: Ctx,
    func_arg: Term,
    constructor: Box<Fn(HasType) -> HasType>,
}

impl PatHasType {
    pub fn ext_ctx(&self) -> Ctx {
        self.ext_ctx.clone()
    }

    pub fn func_arg(&self) -> Term {
        self.func_arg.clone()
    }
    
    pub fn construct(&self, ht: HasType) -> HasType {
        (self.constructor)(ht)
    }
}

pub fn check_pat(
    ctx: &Ctx,
    meta_arg_name: &StrName,
    arg_name: &StrName,
    ty: &Type,
    pat: &Expr,
) -> Result<PatHasType, Error> {
    assert_eq!(*ctx, ty.get_ctx());
    match pat {
        Expr::Unit => {
            let arg_meta_name = StrName::lit(&ctx, "unit_arg_subtype_constraint");
            let arg_meta_type = Type::equal(
                ctx,
                &ty.into_term(),
                &Term::unit_type(ctx),
            );
            let ctx_arg_meta = ctx.bind(&arg_meta_name, &arg_meta_type);
            // ctx..
            // unit_arg_subtype_constraint: Ty #= #()

            let ctx_arg = ctx_arg_meta.bind(arg_name, ty);
            // ctx..
            // unit_arg_subtype_constraint: Ty #= #()
            // $arg_name: Ty

            let ctx_unit_arg = ctx_arg.bind(arg_name, &Type::unit(&ctx_arg));
            // ctx..
            // unit_arg_subtype_constraint: Ty #= #()
            // $arg_name: Ty
            // $arg_name: #()

            let ext_ctx = ctx_unit_arg.clone();
            let func_arg = Term::var(&ext_ctx, 1, arg_name);
            let meta_arg_name = meta_arg_name.clone();
            let arg_name = arg_name.clone();
            let ty = ty.clone();
            let constructor = Box::new(move |res_ht: HasType| {
                let res_meta_name = res_ht.meta_arg_name();
                let res_meta_type = res_ht.meta_arg_type();
                let res_term = res_ht.term();

                let lifted_res_meta_name = unwrap!(res_meta_name.try_lift_out_of_ctx(0, 2));
                let lifted_res_meta_type = Type::func(
                    &ctx_arg_meta,
                    &arg_name,
                    &ty,
                    &Type::from_term(Term::app(
                        &ctx_arg,
                        &Term::func(
                            &ctx_arg,
                            &arg_name,
                            &Type::unit(&ctx_arg),
                            &res_meta_type.into_term(),
                        ),
                        &transport(
                            &ctx_arg,
                            &Term::var(&ctx_arg, 1, &arg_meta_name),
                            &Term::var(&ctx_arg, 0, &arg_name),
                        ),
                    )),
                );
                let ctx_lifted_res_meta = ctx_arg_meta.bind(&lifted_res_meta_name, &lifted_res_meta_type);
                // ctx..
                // unit_arg_subtype_constraint: Ty #= #()
                // res_subtype_constraint: Ty -> ResMeta

                let func_term = {
                    let ctx_arg = ctx_arg.bump(1, &lifted_res_meta_name, &lifted_res_meta_type);
                    // ctx..
                    // unit_arg_subtype_constraint: Ty #= #()
                    // res_meta: {$arg_name: ArgType} -> ResMeta
                    // $arg_name: Ty

                    let ctx_unit_arg = ctx_unit_arg.bump(2, &lifted_res_meta_name, &lifted_res_meta_type);
                    // ctx..
                    // unit_arg_subtype_constraint: Ty #= #()
                    // res_meta: {$arg_name: ArgType} -> ResMeta
                    // $arg_name: Ty
                    // $arg_name: #()

                    let lifted_res_term = {
                        res_term
                        .bump_ctx(3, &lifted_res_meta_name, &lifted_res_meta_type)
                        .substitute(
                            0, 
                            &res_meta_name.bump_ctx(
                                2,
                                &lifted_res_meta_name,
                                &lifted_res_meta_type,
                            ),
                            &Term::app(
                                &ctx_unit_arg,
                                &Term::var(&ctx_unit_arg, 2, &lifted_res_meta_name),
                                &Term::var(&ctx_unit_arg, 1, &arg_name),
                            ),
                        )
                    };
                    Term::func(
                        &ctx_lifted_res_meta,
                        &arg_name,
                        &ty,
                        &Term::app(
                            &ctx_arg,
                            &Term::func(
                                &ctx_arg,
                                &arg_name,
                                &Type::unit(&ctx_arg),
                                &lifted_res_term,
                            ),
                            &transport(
                                &ctx_arg,
                                &Term::var(&ctx_arg, 2, &arg_meta_name),
                                &Term::var(&ctx_arg, 0, &arg_name),
                            ),
                        )
                    )
                };

                HasType::new_from_split_ctx(&ctx_lifted_res_meta, &meta_arg_name, 2, &func_term)
            });

            Ok(PatHasType { ext_ctx, func_arg, constructor })
        },
        _ => unimplemented!(),
    }
}


/*
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
// the eliminators. The constructor takes a meta-term (HasType) under the extended context and
// returns a meta-term under the original context extended with just the pattern's meta_arg_type.
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
*/

fn as_sub_type(
    ctx: &Ctx,
    meta_arg_name: &StrName,
    hi_type: &Type,
    lo_type: &Type,
    term: &Term,
) -> HasType {
    let meta_arg_name = meta_arg_name.bump_into_ctx(&meta_arg_name.get_ctx(), ctx);
    let hi_type = hi_type.bump_into_ctx(&hi_type.get_ctx(), ctx);
    let lo_type = lo_type.bump_into_ctx(&lo_type.get_ctx(), ctx);

    assert_eq!(term.get_type(), lo_type);
    if hi_type == lo_type {
        return HasType {
            meta_arg_name: meta_arg_name.clone(),
            meta_arg_type: Type::unit(ctx),
            term: term.clone(),
        };
    }
    // TODO: add transformations for @[...] and #[...] types
    let hi_type = hi_type.into_term();
    let lo_type = lo_type.into_term();
    let meta_arg_type = Type::equal(ctx, &lo_type, &hi_type);
    let sub_ctx = ctx.bind(&meta_arg_name, &meta_arg_type);
    let term = transport(
        &sub_ctx,
        &Term::var(&sub_ctx, 0, &meta_arg_name),
        &term.bump_ctx(0, &meta_arg_name, &meta_arg_type),
    );
    HasType::new(ctx, &meta_arg_name, &meta_arg_type, &term)
}

fn transport(ctx: &Ctx, equality: &Term, term: &Term) -> Term {
    assert_eq!(*ctx, equality.get_ctx());
    assert_eq!(*ctx, term.get_ctx());

    let (lo_type, hi_type) = match equality.get_type().kind() {
        TypeKind::Equal { x0, x1 } => (x0.clone(), x1.clone()),
        _ => panic!("invalid type for transport"),
    };

    assert_eq!(lo_type, term.get_type().into_term());

    let arg_name = StrName::lit(&ctx, "transport_arg");
    let lo_type_name = StrName::lit(ctx, "LoType");
    let ctx_lo_type = ctx.bind(&lo_type_name, &lo_type.get_type());
    let hi_type_name = StrName::lit(&ctx_lo_type, "HiType");
    let ctx_hi_type = ctx_lo_type.bind(&hi_type_name, &hi_type.get_type().bump_into_ctx(ctx, &ctx_lo_type));
    let types_equal_name = StrName::lit(&ctx_hi_type, "types_equal");
    let ctx_types_equal = ctx_hi_type.bind(&types_equal_name, &Type::equal(
        &ctx_hi_type,
        &Term::var(&ctx_hi_type, 1, &lo_type_name),
        &Term::var(&ctx_hi_type, 0, &hi_type_name),
    ));
    let target_type = {
        let arg_type = Type::from_term(Term::var(&ctx_types_equal, 2, &lo_type_name));
        let ctx_arg = ctx_types_equal.bind(&arg_name, &arg_type);
        Type::func(&ctx_types_equal, &arg_name, &arg_type, &Type::from_term(Term::var(&ctx_arg, 2, &hi_type_name)))
    };

    let eq_type_name = StrName::lit(ctx, "EqType");
    let ctx_eq_type = ctx.bind(&eq_type_name, &lo_type.get_type());
    let target = {
        let arg_type = Type::from_term(Term::var(&ctx_eq_type, 0, &eq_type_name));
        let ctx_arg = ctx_eq_type.bind(&arg_name, &arg_type);
        Term::func(&ctx_eq_type, &arg_name, &arg_type, &Term::var(&ctx_arg, 0, &arg_name))
    };

    Term::app(
        ctx,
        &Term::j(ctx, &target_type, &target, &equality),
        term,
    )
}


