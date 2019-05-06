pub struct MetaId(pub u64);

pub struct MetaCtx {
    next_meta_id: MetaId,
    metas: HashMap<MetaId, MetaVarState>,
}

struct MetaVarState {
    ty: Type,

}

enum MetaVarState {
    Const {
        term: Type,
        ty: Type,
    },
    Solved {
        term: Term,
        ty: Type,
    },
    Constrained {
        ty: Type,
        constraint: Constraint,
    },
}

impl MetaCtx {
    pub fn lookup(&self, meta_id: MetaId) -> Type {
        self.metas[&meta_id].ty()
    }

    pub fn new_meta(&mut self, ty: Type) -> MetaId {
        let meta_id = self.next_meta_id;
        self.next_meta_id = MetaId(self.next_meta_id.0 + 1);
        self.metas.insert(meta_id, MetaVarState::Unsolved { ty });
        meta_id
    }

    pub fn new_meta_constrained(&mut self, ty: Type, constraint: Constraint) -> MetaId {
        let meta_id = self.next_meta_id;
        self.next_meta_id = MetaId(self.next_meta_id.0 + 1);
        self.metas.insert(meta_id, MetaVarState::Constrained { ty, constraint });
        meta_id
    }

    pub fn solve(&mut self, meta_id: MetaId, term: Term) {
        let meta_var_state = &mut self.metas[&meta_id];
        match *meta_var_state {
            MetaVarState::Unsolved { ty } => {
                assert_eq!(term.ty(), ty);
                *meta_var_state = MetaVarState::Solved { term, ty };
            },
            MetaVarState::Solved { term: current, ty } => {
                assert_eq!(term.ty(), ty);
                assert_eq!(current, term);
            },
            MetaVarState::Constrained { ty, constraint } => {
                panic!("need to check constraints somehow");
                assert_eq!(term.ty(), ty);
                *meta_var_state = MetaVarState::Solved { term, ty };
            },
        }
    }
}

impl MetaVarState {
    fn ty(&self) -> Type {
        match self {
            MetaVarState::Unsolved { ty } => ty.clone(),
            MetaVarState::Solved { ty, .. } => ty.clone(),
            MetaVarState::Constrained { ty, .. } => ty.clone(),
        }
    }
}


