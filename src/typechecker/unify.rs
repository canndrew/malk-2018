use super::*;

use crate::syntax::{IdentOpt, NameOpt};

// Tries to turn the type into a simpler type. Produces two terms which map between the old and the
// new type in both directions. The forward term is term of the old type, predicated on the new
// type, the back term is a term of the new type predicated on the old type. Together they form an
// isomorphism.
//
// eg. for
//
//      G |- #(#(), t: T)
// 
// we simplify this type to
//
//      G |- T
// 
// and we get two morphisms:
//
//      G, t: T |- ((), t)         -- forward
//      G, p: #(#(), t: T) |- p.t  -- back
//
pub fn simplify(ctx: &Ctx, ty: &Type) -> (Type, Term, Term) {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Equal { x0, x1 } => {
            if x0 == x1 {
                let ret_ty = Type::unit(ctx);
                let ctx_forward = ctx.bind(&IdentOpt::fake("eliminating_equality"), &ret_ty);
                let ctx_back = ctx.bind(&IdentOpt::fake("equality"), &ty);

                let subst_forward = Term::refl(&ctx_forward, x0);
                let subst_back = Term::unit(&ctx_back);

                return (ret_ty, subst_forward, subst_back);
            }
        },
        TypeKind::Pair { head_ident_opt, head, tail } => return {
            // initial state
            //
            // G |- A type
            // G, a: A |- B type
            // G |- #(A, B) type
            //
            // simplifying A gives us..
            //
            // G |- A' type
            // G, a': A' |- a_for: A[^0 A']
            // G, a: A |- a_bak: A'[^0 A]
            let (simplified_head, head_subst_forward, head_subst_back) = simplify(ctx, head);

            if let TypeKind::Unit = simplified_head.kind() {
                // G |- BBumped type
                let tail_bumped = {
                    // G, a: A |- B type
                    tail
                    // G, #(), a: A |- B[^1 #()] type
                    .bump_ctx(1, head_ident_opt, &Type::unit(ctx))
                    // G, #() |- B[^1 #()][a / a_for] type
                    .substitute(0, head_ident_opt, &head_subst_forward)
                    // G |- B[^1 #()][a / a_for][() / ()] type
                    .substitute(0, head_ident_opt, &Term::unit(ctx))
                };

                let (simplified_tail, tail_subst_forward, tail_subst_back) = simplify(ctx, &tail_bumped);

                let ret_ty = simplified_tail;

                let ctx_forward = ctx.bind(&IdentOpt::fake("simplified_pair"), &ret_ty);
                let ctx_back = ctx.bind(&IdentOpt::fake("pair"), ty);

                let tail_type_bumped = {
                    tail
                    .bump_ctx(1, &IdentOpt::fake("simplified_pair"), &ret_ty)
                };
                let tail_subst_forward_bumped = {
                    tail_subst_forward
                    .bump_ctx(1, head_ident_opt, head)
                };
                let pair_subst_forward = Term::pair(
                    &ctx_forward,
                    head_ident_opt,
                    &Term::unit(&ctx_forward),
                    &tail_type_bumped,
                    &tail_subst_forward_bumped,
                );
                let tail_subst_back_bumped = {
                    tail_subst_back
                    .bump_ctx(1, head_ident_opt, head)
                    .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                };
                let ret_ty_bumped = ret_ty.bump_ctx(0, &IdentOpt::fake("pair"), ty);
                let pair_subst_back = Term::pair_split(
                    &ctx_back,
                    &ret_ty_bumped,
                    &tail_subst_back_bumped,
                    &Term::var(&ctx_back, 0, &NameOpt::fake("pair")),
                );

                (ret_ty, pair_subst_forward, pair_subst_back)
            } else {
                // G, a': A' |- BBumped type
                let tail_bumped = {
                    // G, a: A |- B type
                    tail
                    // G, a': A', a: A[^0 A'] |- B[^1 A'] type
                    .bump_ctx(1, head_ident_opt, &simplified_head)
                    // G, a': A' |- B[^1 A'][a / a_for] type
                    .substitute(0, head_ident_opt, &head_subst_forward)
                };

                let tail_ctx = ctx.bind(head_ident_opt, &simplified_head);

                // simplify BBumped to get
                //
                // G, a': A' |- B' type
                // G, a': A', b': B' |- b_for: BBumped[^0 B']
                // G, a': A', b: BBumped |- b_bak: B'[^0 BBumped]
                let (simplified_tail, tail_subst_forward, tail_subst_back) = simplify(&tail_ctx, &tail_bumped);

                if let TypeKind::Unit = simplified_tail.kind() {
                    let ret_ty = simplified_head;
                    let ctx_forward = {
                        ctx
                        .bind(&IdentOpt::fake("simplified_pair"), &ret_ty)
                    };
                    let ctx_back = {
                        ctx
                        .bind(&IdentOpt::fake("pair"), ty)
                    };

                    let tail_type_bumped = {
                        tail
                        .bump_ctx(1, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };
                    let tail_subst_forward_bumped = {
                        tail_subst_forward
                        .substitute(0, &IdentOpt::fake("simplified_tail"), &Term::unit(&ctx_forward))
                    };

                    let pair_subst_forward = Term::pair(
                        &ctx_forward,
                        head_ident_opt,
                        &head_subst_forward,
                        &tail_type_bumped,
                        &tail_subst_forward_bumped,
                    );

                    let head_subst_back_bumped = {
                        head_subst_back
                        .bump_ctx(0, &IdentOpt::fake("tail"), tail)
                        .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                    };

                    let ret_ty_bumped = {
                        ret_ty
                        .bump_ctx(0, head_ident_opt, head)
                        .bump_ctx(0, &IdentOpt::fake("tail"), tail)
                        .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                    };

                    let pair_subst_back = Term::pair_split(
                        &ctx_back,
                        &ret_ty_bumped,
                        &head_subst_back_bumped,
                        &Term::var(&ctx_back, 0, &NameOpt::fake("pair")),
                    );

                    (ret_ty, pair_subst_forward, pair_subst_back)
                } else {
                    // G |- #(A', B') type
                    let ret_ty = Type::pair(ctx, head_ident_opt, &simplified_head, &simplified_tail);

                    // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 #(A', B')] |- a_for_bumped : A[^0 A'][^0 B'][^2 #(A', B')]
                    let head_subst_forward_bumped = {
                        // G, a': A' |- a_for: A[^0 A']
                        head_subst_forward
                        // G, a': A', b': B' |- a_for[^0 B']: A[^0 A'][^0 B']
                        .bump_ctx(0, &IdentOpt::fake("simplified_tail"), &simplified_tail)
                        // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 #(A', B')] |- a_for[^0 B'][^2 #(A', B')]: A[^0 A'][^0 B'][^2 #(A', B')]
                        .bump_ctx(2, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    // G, a': A' b': B'[^1 A][a' / a_bak][^1 A'][a / a_for] |- b_for_bumped: B[^0 B'][^2 A'][a / a_for]
                    let tail_subst_forward_bumped = {
                        // G, a': A', b': B' |- b_for: BBumped[^0 B']
                        tail_subst_forward
                        // G, a: A, a': A'[^0 A], b': B'[^1 A] |- b_for[^2 A]: BBumped[^0 B'][^2 A]
                        .bump_ctx(2, head_ident_opt, head)
                        // G, a: A, b': B'[^1 A][a' / a_bak] |- b_for[^2 A][a' / a_bak]: BBumped[^0 B'][^2 A]
                        // G, a: A, b': B'[^1 A][a' / a_bak] |- b_for[^2 A][a' / a_bak]: B[^0 B']
                        .substitute(1, head_ident_opt, &head_subst_back)
                        // G, a': A', a: A[^0 A'], b': B'[^1 A][a' / a_bak][^1 A'] |- b_for[^2 A][a' / a_bak][^2 A']: B[^0 B'][^2 A']
                        .bump_ctx(2, head_ident_opt, &simplified_head)
                        // G, a': A', b': B'[^1 A][a' / a_bak][^1 A'][a / a_for] |- b_for[^2 A][a' / a_bak][^2 A'][a / a_for]: B[^0 B'][^2 A'][a / a_for]
                        .substitute(1, head_ident_opt, &head_subst_forward)
                        // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 A][a' / a_bak][^1 A'][a / a_for][^1 #(A', B')]
                        //      |- b_for[^2 A][a' / a_bak][^2 A'][a / a_for][^2 #(A', B')]: B[^0 B'][^2 A'][a / a_for][^2 #(A', B')]
                        .bump_ctx(2, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    let ctx_forward = {
                        ctx
                        .bind(&IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    let ctx_back = {
                        ctx
                        .bind(&IdentOpt::fake("pair"), ty)
                    };

                    // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 #(A', B')]
                    let pair_forward_ctx = {
                        ctx
                        .bind(head_ident_opt, &simplified_head)
                        .bind(&IdentOpt::fake("simplified_tail"), &simplified_tail)
                        .bump(2, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 #(A', B')] |- #(A, B)[^0 A'][^0 B'][^2 #(A', B')] type
                    let ty_bumped = {
                        ty
                        .bump_ctx(0, head_ident_opt, &simplified_head)
                        .bump_ctx(0, &IdentOpt::fake("simplified_tail"), &simplified_tail)
                        .bump_ctx(2, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    let tail_type_bumped = {
                        tail
                        .bump_ctx(1, head_ident_opt, &simplified_head)
                        .bump_ctx(1, &IdentOpt::fake("simplified_tail"), &simplified_tail)
                        .bump_ctx(3, &IdentOpt::fake("simplified_pair"), &ret_ty)
                    };

                    // G, p': #(A', B') |- p_for: #(A, B)[^0 #(A', B')]
                    let pair_subst_forward = {
                        // G, p': #(A', B'), a': A'[^0 #(A', B')], b': B'[^1 #(A', B')] |- (a_for_bumped, b_for_bumped): #(A, B)[^0 A'][^0 B'][^2 #(A', B')]
                        let pair = &Term::pair(&pair_forward_ctx, head_ident_opt, &head_subst_forward_bumped, &tail_type_bumped, &tail_subst_forward_bumped);
                        // G, p': #(A', B') |- split(p' => (a_for_bumped, b_for_bumped)): #(A, B)[^0 #(A', B')]
                        Term::pair_split(&ctx_forward, &ty_bumped, &pair, &Term::var(&ctx_forward, 0, &NameOpt::fake("simplified_pair")))
                    };

                    // G, p: #(A, B), a: A, b: B |- a_bak_bumped: A'[^0 A][^0 B][^2 #(A, B)]
                    let head_subst_back_bumped = {
                        // G, a: A |- a_bak: A'[^0 A]
                        head_subst_back
                        // G, a: A, b: B |- a_bak[^0 B]: A'[^0 A][^0 B]
                        .bump_ctx(0, &IdentOpt::fake("tail"), tail)
                        // G, p: #(A, B), a: A[^0 #(A, B)], b: B[^1 #(A, B)] |- a_bak[^0 B][^2 #(A, B)]: A'[^0 A][^0 B][^2 #(A, B)]
                        .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                    };

                    // G, p: #(A, B), a: A, b: B |- b_bak_bumped: B'[^1 A][^1 B][^2 #(A, B)][a' / a_bak]
                    let tail_subst_back_bumped = {
                        // G, a': A', b: B[^1 A'][a / a_for] |- b_bak: B'[^0 B[^1 A'][a / a_for]]
                        tail_subst_back
                        // G, a: A, a': A'[^0 A], b: B[^1 A'][a / a_for][^1 A] |- b_bak[^2 A]: B'[^0 B[^1 A'][a / a_for]][^2 A]
                        .bump_ctx(2, head_ident_opt, head)
                        // G, a: A, b: B |- b_bak[^2 A][a' / a_bak] : B'[^0 B[^1 A'][a / a_for]][^2 A][a' / a_bak]
                        .substitute(1, head_ident_opt, &head_subst_back)
                        // G, p: #(A, B), a: A, b: B |- b_bak[^2 A][a' / a_bak][^2 #(A, B)] : B'[^0 B[^1 A'][a / a_for]][^2 A][a' / a_bak][^2 #(A, B)]
                        .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                    };

                    // G, p: #(A, B), a: A[^0 #(A, B)], b: B[^1 #(A, B)]
                    let pair_back_ctx = {
                        ctx
                        .bind(head_ident_opt, head)
                        .bind(&IdentOpt::fake("tail"), tail)
                        .bump(2, &IdentOpt::fake("pair"), ty)
                    };

                    let ret_ty_bumped = {
                        ret_ty
                        .bump_ctx(0, head_ident_opt, head)
                        .bump_ctx(0, &IdentOpt::fake("tail"), tail)
                        .bump_ctx(2, &IdentOpt::fake("pair"), ty)
                    };

                    let ret_tail_type_bumped = {
                        simplified_tail
                        .bump_ctx(1, head_ident_opt, head)
                        .bump_ctx(1, &IdentOpt::fake("tail"), tail)
                        .bump_ctx(3, &IdentOpt::fake("pair"), ty)
                    };

                    let pair_subst_back = {
                        // G, p: #(A, B), a: A, b: B |- (a_bak_bumped, b_bak_bumped): #(A', B')[^0 A][^0 B][^2 #(A, B)]
                        let pair = &Term::pair(&pair_back_ctx, head_ident_opt, &head_subst_back_bumped, &ret_tail_type_bumped, &tail_subst_back_bumped);
                        // G, p: #(A, B) |- split(p => (a_bak_bumped, b_bak_bumped)): #(A', B')[^0 #(A, B)]
                        Term::pair_split(&ctx_back, &ret_ty_bumped, &pair, &Term::var(&ctx_back, 0, &NameOpt::fake("pair")))
                    };

                    (ret_ty, pair_subst_forward, pair_subst_back)
                }
            }
        },
        /*
        TypeKind::Func { arg, res } => {
            let (simplified_arg, arg_subst_forward, arg_subst_back) = simplify(ctx, arg);
            match simplified_arg.kind() {
                TypeKind::Unit => {
                    // return res
                },
                TypeKind::Never => {
                    // return unit
                },
                _ => {
                    // check if res is another function.
                    // if so, conglomerate the args into a struct.
                },
            }
            unimplemented!()
        },
        */
        _ => (),
    }

    let ret_ty = ty.clone();
    let ctx_forward = ctx.bind(&IdentOpt::fake("new_meta"), &ret_ty);
    let ctx_back = ctx.bind(&IdentOpt::fake("old_meta"), &ret_ty);

    let subst_forward = Term::var(&ctx_forward, 0, &NameOpt::fake("new_meta"));
    let subst_back = Term::var(&ctx_back, 0, &NameOpt::fake("old_meta"));

    (ret_ty, subst_forward, subst_back)
}

/*
pub fn unique_inhabitant(ctx: &Ctx, ty: &Type) -> Option<Term> {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Embed(..) => None,
        TypeKind::Type { .. } => None,
        TypeKind::Equal { x0, x1 } => {
            if x0 == x1 {
                Some(Term::refl(ctx, x0))
            } else {
                None
            }
        },
        TypeKind::Never => None,
        TypeKind::Unit => Some(Term::unit(ctx)),
        TypeKind::Pair { head_ident_opt, head, tail } => {
            let head_term = unique_inhabitant(ctx, head)?;
            let sub_ctx = ctx.bind(head_ident_opt, head);
            let tail_term = unique_inhabitant(&sub_ctx, tail)?;
            Some(Term::pair(ctx, head_ident_opt, &head_term, &tail_term))
        },
        TypeKind::Func { arg, res } => {
            match map_to_never(ctx, arg) {
                Some(map) => {
                    Some(Term::func(ctx, arg, &Term::abort(ctx, res, &Term::app(ctx, &map, &Term::var(ctx, 0, &NameOpt::none())))))
                },
                None => {
                    let res_term = unique_inhabitant(ctx, res)?;
                    Some(Term::func(ctx, arg, &res_term))
                },
            }
        },
    }
}

pub fn map_to_never(ctx: &Ctx, ty: &Type) -> Option<Term> {
    assert_eq!(*ctx, ty.get_ctx());
    match ty.kind() {
        TypeKind::Embed(..) => None,
        TypeKind::Type { .. } => None,
        TypeKind::Equal { .. } => None,
        TypeKind::Never => Some(Term::func(ctx, ty, &Term::var(ctx, 0, &NameOpt::none()))),
        TypeKind::Unit => None,
        //TypeKind::Pair { head_ident_opt, head, tail } => {
        TypeKind::Pair { .. } => {
            None
            /*
            match map_to_never(ctx, head) {
                Some(map) => {
                    Term::func(ctx, ty, Term::pair_split(
                },
            }
            */
        },
        //TypeKind::Func { arg, res } => None,
        TypeKind::Func { .. } => None,
    }
}
*/

