use super::*;

// here's how it works. for now.
//
//      local(0) always points to the start of the value at the top of our mem stack.
//      other locals can be used for temporary variables.

struct Build {
    next_local: LocalIdx,
    instructions: Vec<Instruction>,
}

struct FuncDef {
    ty: FuncType,
    body: Expr,
}

pub fn build_module(expr: &parser::Expr) -> Module {
    let mut build = Build {
        instructions: Vec::new(),
    };

    build.fold_expr(expr);
    let func_def = build.finish();

    Module {
        types: vec![func_def.ty],
        funcs: vec![
            Func {
                type_: TypeIdx(0),
                locals: Vec::new(),
                body: func_def.body,
            },
        ],
        tables: Vec::new(),
        mems: Vec::new(),
        globals: Vec::new(),
        elem: Vec::new(),
        data: Vec::new(),
        start: Some(FuncIdx(0)),
        imports: Vec::new(),
        exports: Vec::new(),
    }
}

impl Build {
    pub fn new_func(
        _arg_type: &parser::Expr,
        _ret_type: &parser::Expr,
    ) -> Build {
        Build {
            instructions: Vec::new(),
        }
    }

    pub fn finish(self) -> FuncDef {
        FuncDef {
            ty: FuncType {
                params: Vec::new(),
                result: Vec::new(),
            },
            body: Expr {
                instructions: self.instructions,
            },
        }
    }

    pub fn get_local(&mut self) -> LocalIdx {
        let next_local = self.next_local;
        let LocalIdx(local) = next_local;
        self.next_local = LocalIdx(local + 1);
        self.max_locals = cmp::max(max_locals, local);
        next_local
    }

    pub fn fold_expr(&mut self, expr: &parser::Expr) {
        match expr {
            parser::Expr::UnitTerm(..) => (),
            parser::Expr::App { func, arg, .. } => {
                self.fold_expr(func);

                let local = self.get_local();

                self.instructions.push(Instruction::GetLocal(LocalIdx(0)));
                self.instructions.push(Instruction::SetLocal(local));

                self.fold_expr(arg);

                self.instructions.push(Instruction::GetLocal(local));
                self.instructions.push(Instruction::I32Const(4));
                self.instructions.push(Instruction::I32Add);
                self.instructions.push(Instruction::GetLocal(local));
                self.instructions.push(Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 1,
                }));

                self.next_local = local;
            },
            /*
            parser::Expr::Let { pat, expr, body, .. } => {
                self.fold_expr(expr);
                self.fold_pat(pat);
                self.fold_expr(body);
            },
            parser::Expr::Var(ident) => {
                for (name, instructions) in &self.vars {
                    if name == ident.name {
                        self.instructions.extend(instructions);
                        break;
                    }
                }
            },
            parser::Expr::UnitTerm => (),
            parser::Expr::PairTerm { head, tail, .. } => {
                self.fold_expr(head);
                self.fold_expr(tail);
            },
            parser::Expr::NegFuncTerm { pat, body, .. } => {
                let mut sub_build = Build {
                    funcs: Vec::new(),
                    vars: self.vars.clone(),
                    instructions: Vec::new(),
                };
                sub_build.fold_pat(pat);
                sub_build.fold_body(body);
            },

            parser::Expr::UnitType |
            parser::Expr::PairType { .. } |
            parser::Expr::NeverType |
            parser::Expr::EnumType { .. } |
            parser::Expr::NegFuncType { .. } |
            parser::Expr::EnumFuncType { .. } |
            parser::Expr::NeverFunc => panic!("tried to build erased term"),
            */
            _ => unimplemented!(),
        }
    }
}

