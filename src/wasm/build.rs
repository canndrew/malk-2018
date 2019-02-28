use super::*;

pub struct BuilderFunc {
    num_upvars: usize,
    instructions: Vec<Instruction>,
}

pub fn build_expr(expr: &parser::Expr) -> Module {
    let mut builder = Builder::new();
    builder.push_expr(expr);
    builder.into_module()
}

pub struct Builder {
    // the vars that are currently on the wasm value-stack, sans the top of the stack (which points
    // to the top of the data stack).
    ctx: Vec<Option<String>>,
    funcs: Vec<BuilderFunc>,
    stacked_instructions: Vec<Vec<Instruction>>,
    instructions: Vec<Instruction>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            ctx: Vec::new(),
            funcs: Vec::new(),
            stacked_instructions: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn into_module(self) -> Module {
        assert!(self.ctx == &[None]);
        assert!(self.stacked_instructions.is_empty());
        let mut types = Vec::new();
        let mut funcs = Vec::new();
        for func in self.funcs {
            funcs.push(Func {
                type_: TypeIdx(types.len() as u32),
                locals: vec![ValType::I32],
                body: Expr {
                    instructions: func.instructions,
                },
            });
            types.push(FuncType {
                params: vec![ValType::I32; func.num_upvars + 2],
                result: vec![ValType::I32; func.num_upvars + 2],
            });
        }
        let start_idx = funcs.len() as u32;
        funcs.push(Func {
            type_: TypeIdx(start_idx),
            locals: vec![ValType::I32],
            body: Expr {
                instructions: {
                    let mut instructions = Vec::new();
                    instructions.push(Instruction::I32Const(0));
                    instructions.extend(self.instructions);
                    instructions.push(Instruction::Drop);
                    instructions.push(Instruction::Drop);
                    instructions
                },
            },
        });
        types.push(FuncType {
            params: vec![],
            result: vec![],
        });
        Module {
            types,
            funcs,
            tables: vec![],
            mems: vec![MemType {
                limits: Limits {
                    min: 1024,
                    max: None,
                },
            }],
            globals: vec![],
            elem: vec![],
            data: vec![],
            start: Some(FuncIdx(start_idx)),
            imports: vec![],
            exports: vec![],
        }
    }

    /*
    pub fn push_pat(&mut self, pat: &parser::Pat) {
        match pat {
            parser::Pat::Var(ident) => {
                let name = ident.name();
                
            },
        }
    }
    */

    pub fn push_expr(&mut self, expr: &parser::Expr) {
        match expr {
            parser::Expr::UnitTerm(..) => {
                self.instructions.extend(vec![
                    Instruction::TeeLocal(LocalIdx(0)),
                    Instruction::GetLocal(LocalIdx(0)),
                ]);
            },
            parser::Expr::PairTerm { head, tail, .. } => {
                self.push_expr(&head.expr);
                self.push_expr(tail);
                self.instructions.extend(vec![
                    Instruction::SetLocal(LocalIdx(0)),
                    Instruction::Drop,
                    Instruction::GetLocal(LocalIdx(0)),
                ]);
            },
            parser::Expr::String(ident) => {
                let string = ident.name();
                self.instructions.extend(vec![
                    Instruction::TeeLocal(LocalIdx(0)),
                    Instruction::I32Const(string.len() as i32),
                    Instruction::I32Store(MemArg { offset: 0, align: 0 }),
                ]);
                let mut offset = 4;
                for chunk in string.as_bytes().chunks(8) {
                    let mut val = 0u64;
                    for (i, byte) in chunk.iter().enumerate() {
                        val |= (*byte as u64) << (i * 8);
                    }
                    self.instructions.extend(vec![
                        Instruction::GetLocal(LocalIdx(0)),
                        Instruction::I64Const(val as i64),
                        Instruction::I64Store(MemArg { offset, align: 0 }),
                    ]);
                    offset += 8;
                }
                self.instructions.extend(vec![
                    Instruction::GetLocal(LocalIdx(0)),
                    Instruction::GetLocal(LocalIdx(0)),
                    Instruction::I32Const(string.len() as i32 + 4),
                    Instruction::I32Add,
                ]);
            },
            /*
            parser::Expr::Let { pat, expr, body, .. } => {
                self.push_expr(expr);
                let num_vars = self.push_pat(pat);
                self.push_expr(body);
                self.instructions.extend(vec![
                    Instruction::SetLocal(LocalIdx(0)),
                    Instruction::SetLocal(LocalIdx(1)),
                    Instruction::GetLocal(LocalIdx(0)),
                    Instruction::GetLocal(LocalIdx(1)),
                    Instruction::I32Sub,
                    Instruction::SetLocal(LocalIdx(0)),
                ]);
                // start of return data is at local(1), len is local(0)
                for _ in 0..num_vars {
                    self.instructions.push(Instruction::Drop);
                }

                self.instructions.push(vec![
                    Instruction::I32Const(0),
                    Instruction::SetLocal(LocalIdx(2)),
                    Instruction::TeeLocal(LocalIdx(3)),
                    Instruction::Loop(Vec::new(), vec![
                        Instruction::GetLocal(LocalIdx(3)),
                        Instruction::GetLocal(LocalIdx(2)),
                        Instruction::I32Add,
                        Instruction::GetLocal(LocalIdx(1)),
                        Instruction::GetLocal(LocalIdx(2)),
                        Instruction::I32Add,
                        Instruction::I32Load8U(MemArg { offset: 0, align: 0 }),
                        Instruction::I32Store8(MemArg { offset: 0, align: 0 }),
                        Instruction::GetLocal(LocalIdx(2)),
                        Instruction::I32Const(1),
                        Instruction::I32Add,
                        Instruction::TeeLocal(LocalIdx(2)),
                        Instruction::GetLocal(LocalIdx(0)),
                        Instruction::I32Eq,
                        Instruction::BrIf(LabelIdx(0)),
                    ]),
                ]);
            },
            */
    /*
    Parens(Span, Box<Expr>),
    Var(Ident),
    NegFuncTerm {
        pat: Box<Pat>,
        body: Box<Expr>,
        span: Span,
    },
    Number(Ident),
    EnumLeft {
        elem: CompositeTermElem,
        span: Span,
    },
    EnumRight {
        expr: Box<Expr>,
        span: Span,
    },
    EnumFuncTerm {
        pat: Box<CompositePatElem>,
        body: Box<Expr>,
        tail: Box<Expr>,
        span: Span,
    },
    NeverFunc(Span),
    App {
        func: Box<Expr>,
        arg: Box<Expr>,
        span: Span,
    },
    */
            _ => panic!("I don't know how to handle that expr type"),
        };
        self.ctx.push(None);
    }
}

