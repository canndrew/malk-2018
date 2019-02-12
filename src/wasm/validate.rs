use super::*;

struct Context {
    types: Vec<FuncType>,
    funcs: Vec<FuncType>,
    tables: Vec<TableType>,
    mems: Vec<MemType>,
    globals: Vec<GlobalType>,
    locals: Vec<ValType>,
    labels: Vec<Option<ValType>>,
    return_: Option<Option<ValType>>,
}

impl Limits {
    pub fn check(&self) -> Result<(), Error> {
        if let Some(max) = self.max {
            if max < self.min {
                bail!("malformed limits, max is less than min");
            }
        }

        Ok(())
    }

    pub fn check_with_range(&self, range: u32) -> Result<(), Error> {
        if self.min > range {
            bail!("limits min out of range");
        }

        if let Some(max) = self.max {
            if max > range {
                bail!("limits max out of range");
            }
        }

        self.check()?;

        Ok(())
    }
}

impl FuncType {
    pub fn check(&self) -> Result<(), Error> {
        if self.result.len() > 1 {
            bail!("functions may not return more than one result");
        }

        Ok(())
    }
}

impl TableType {
    pub fn check(&self) -> Result<(), Error> {
        self.limits.check()
    }
}

impl MemType {
    pub fn check(&self) -> Result<(), Error> {
        self.limits.check_with_range(1 << 16)
    }
}

struct Frame<'f> {
    label_types: &'f [ValType],
    end_types: &'f [ValType],
    height: usize,
    unreachable: bool,
    prev: Option<&'f Frame<'f>>,
}

impl<'f> Frame<'f> {
    pub fn get(&'f self, index: LabelIdx) -> Option<&'f Frame> {
        let LabelIdx(mut index) = index;
        let mut ret = self;
        while index > 0 {
            ret = match ret.prev {
                Some(ret) => ret,
                None => return None,
            };
            index -= 1;
        }
        Some(ret)
    }
}

struct Stack {
    tys: Vec<Option<ValType>>,
}

impl Stack {
    pub fn push(&mut self, ty: Option<ValType>) {
        self.tys.push(ty);
    }

    pub fn pop_check(&mut self, check: Option<ValType>) -> Result<Option<ValType>, Error> {
        let actual = match self.tys.pop() {
            Some(actual) => actual,
            None => bail!("stack is empty"),
        };
        if actual.is_none() {
            return Ok(check);
        }
        if check.is_none() {
            return Ok(actual);
        }
        if check == actual {
            return Ok(actual);
        }
        bail!("mismatched types");
    }

    pub fn truncate(&mut self, size: usize) -> Result<(), Error> {
        if self.tys.len() < size {
            bail!("stack underflow");
        }
        self.tys.truncate(size);
        Ok(())
    }

    pub fn len(&self) -> usize {
        self.tys.len()
    }
}

impl Instruction {
    fn check<'f>(
        &self,
        ctx: &mut Context,
        opds: &mut Stack,
        frame: &mut Frame<'f>,
    ) -> Result<(), Error> {
        use super::ValType::*;
        use super::Instruction::*;
        match self {
            I32Const(..) => opds.push(Some(I32)),
            I64Const(..) => opds.push(Some(I64)),
            F32Const(..) => opds.push(Some(F32)),
            F64Const(..) => opds.push(Some(F64)),
            I32Clz |
            I32Ctz |
            I32Popcnt => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Clz |
            I64Ctz |
            I64Popcnt => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(I64));
            },
            F32Abs |
            F32Neg |
            F32Sqrt |
            F32Ceil |
            F32Floor |
            F32Trunc |
            F32Nearest => {
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(F32));
            },
            F64Abs |
            F64Neg |
            F64Sqrt |
            F64Ceil |
            F64Floor |
            F64Trunc |
            F64Nearest => {
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(F64));
            },
            I32Add |
            I32Sub |
            I32Mul |
            I32DivU |
            I32DivS |
            I32RemU |
            I32RemS |
            I32And |
            I32Or |
            I32Xor |
            I32Shl |
            I32ShrU |
            I32ShrS |
            I32Rotl |
            I32Rotr => {
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Add |
            I64Sub |
            I64Mul |
            I64DivU |
            I64DivS |
            I64RemU |
            I64RemS |
            I64And |
            I64Or |
            I64Xor |
            I64Shl |
            I64ShrU |
            I64ShrS |
            I64Rotl |
            I64Rotr => {
                let _ = opds.pop_check(Some(I64))?;
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(I64));
            },
            F32Add |
            F32Sub |
            F32Mul |
            F32Div |
            F32Min |
            F32Max |
            F32Copysign => {
                let _ = opds.pop_check(Some(F32))?;
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(F32));
            },
            F64Add |
            F64Sub |
            F64Mul |
            F64Div |
            F64Min |
            F64Max |
            F64Copysign => {
                let _ = opds.pop_check(Some(F64))?;
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(F64));
            },
            I32Eqz => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Eqz => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(I32));
            },
            I32Eq |
            I32Ne |
            I32LtU |
            I32LtS |
            I32GtU |
            I32GtS |
            I32LeU |
            I32LeS |
            I32GeU |
            I32GeS => {
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Eq |
            I64Ne |
            I64LtU |
            I64LtS |
            I64GtU |
            I64GtS |
            I64LeU |
            I64LeS |
            I64GeU |
            I64GeS => {
                let _ = opds.pop_check(Some(I64))?;
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(I32));
            },
            F32Eq |
            F32Ne |
            F32Lt |
            F32Gt |
            F32Le |
            F32Ge => {
                let _ = opds.pop_check(Some(F32))?;
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(I32));
            },
            F64Eq |
            F64Ne |
            F64Lt |
            F64Gt |
            F64Le |
            F64Ge => {
                let _ = opds.pop_check(Some(F64))?;
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(I32));
            },
            I32Wrap => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(I32));
            },
            I64ExtendU |
            I64ExtendS => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I32TruncUF32 |
            I32TruncSF32 => {
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(I32));
            },
            I64TruncUF32 |
            I64TruncSF32 => {
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(I64));
            },
            I32TruncUF64 |
            I32TruncSF64 => {
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(I32));
            },
            I64TruncUF64 |
            I64TruncSF64 => {
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(I64));
            },
            F32Demote => {
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(F32));
            },
            F64Promote => {
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(F64));
            },
            F32ConvertUI32 |
            F32ConvertSI32 => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(F32));
            },
            F64ConvertUI32 |
            F64ConvertSI32 => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(F64));
            },
            F32ConvertUI64 |
            F32ConvertSI64 => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(F32));
            },
            F64ConvertUI64 |
            F64ConvertSI64 => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(F64));
            },
            F32Reinterpret => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(F32));
            },
            F64Reinterpret => {
                let _ = opds.pop_check(Some(I64))?;
                opds.push(Some(F64));
            },
            I32Reinterpret => {
                let _ = opds.pop_check(Some(F32))?;
                opds.push(Some(I32));
            },
            I64Reinterpret => {
                let _ = opds.pop_check(Some(F64))?;
                opds.push(Some(I64));
            },
            Drop => {
                let _ = opds.pop_check(None)?;
            },
            Select => {
                let t0 = opds.pop_check(None)?;
                let t1 = opds.pop_check(t0)?;
                opds.push(t1);
            },
            GetLocal(LocalIdx(idx)) => {
                match ctx.locals.get(*idx as usize) {
                    Some(t) => opds.push(Some(*t)),
                    None => bail!("invalid local index"),
                }
            },
            SetLocal(LocalIdx(idx)) => {
                match ctx.locals.get(*idx as usize) {
                    Some(t) => {
                        let _ = opds.pop_check(Some(*t))?;
                    },
                    None => bail!("invalid local index"),
                }
            },
            TeeLocal(LocalIdx(idx)) => {
                match ctx.locals.get(*idx as usize) {
                    Some(t) => {
                        let _ = opds.pop_check(Some(*t))?;
                        opds.push(Some(*t));
                    },
                    None => bail!("invalid local index"),
                }
            },
            GetGlobal(GlobalIdx(idx)) => {
                match ctx.globals.get(*idx as usize) {
                    Some(global) => opds.push(Some(global.val_type)),
                    None => bail!("invalid global index"),
                }
            },
            SetGlobal(GlobalIdx(idx)) => {
                match ctx.globals.get(*idx as usize) {
                    Some(global) => {
                        if !global.mutable {
                            bail!("cannot set global variable");
                        }
                        let _ = opds.pop_check(Some(global.val_type))?;
                    },
                    None => bail!("invalid global index"),
                }
            },
            I32Load(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Load(mem_arg) => {
                mem_arg.check(8)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            F32Load(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(F32));
            },
            F64Load(mem_arg) => {
                mem_arg.check(8)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(F64));
            },
            I32Store(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I32))?;
            },
            I64Store(mem_arg) => {
                mem_arg.check(8)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I64))?;
            },
            F32Store(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(F32))?;
            },
            F64Store(mem_arg) => {
                mem_arg.check(8)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(F64))?;
            },
            I32Load8U(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I32Load8S(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Load8U(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I64Load8S(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I32Load16U(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I32Load16S(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            I64Load16U(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I64Load16S(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I64Load32U(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I64Load32S(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I64));
            },
            I32Store8(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I32))?;
            },
            I64Store8(mem_arg) => {
                mem_arg.check(1)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I64))?;
            },
            I32Store16(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I32))?;
            },
            I64Store16(mem_arg) => {
                mem_arg.check(2)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I64))?;
            },
            I64Store32(mem_arg) => {
                mem_arg.check(4)?;
                let _ = opds.pop_check(Some(I32))?;
                let _ = opds.pop_check(Some(I64))?;
            },
            MemorySize => {
                opds.push(Some(I32));
            },
            MemoryGrow => {
                let _ = opds.pop_check(Some(I32))?;
                opds.push(Some(I32));
            },
            Nop => (),
            Unreachable => {
                opds.truncate(frame.height)?;
                frame.unreachable = true;
            },
            Block(val_types, instructions) => {
                instructions.check(ctx, opds, &val_types[..], &val_types[..], Some(frame))?;
            },
            Loop(val_types, instructions) => {
                instructions.check(ctx, opds, &[], &val_types[..], Some(frame))?;
            },
            If(val_types, then_instructions, else_instructions) => {
                let _ = opds.pop_check(Some(I32))?;
                then_instructions.check(ctx, opds, &val_types[..], &val_types[..], Some(frame))?;
                else_instructions.check(ctx, opds, &val_types[..], &val_types[..], Some(frame))?;
            },
            Br(idx) => {
                let up_frame = match frame.get(*idx) {
                    Some(up_frame) => up_frame,
                    None => bail!("no such label"),
                };
                for ty in up_frame.label_types.iter().rev() {
                    let _ = opds.pop_check(Some(*ty))?;
                }
                opds.truncate(frame.height)?;
                frame.unreachable = true;
            },
            BrIf(idx) => {
                let up_frame = match frame.get(*idx) {
                    Some(up_frame) => up_frame,
                    None => bail!("no such label"),
                };
                let _ = opds.pop_check(Some(I32))?;
                for ty in up_frame.label_types.iter().rev() {
                    let _ = opds.pop_check(Some(*ty))?;
                }
                for ty in up_frame.label_types {
                    opds.push(Some(*ty));
                }
            },
            BrTable(idxs, idx) => {
                let up_frame = match frame.get(*idx) {
                    Some(up_frame) => up_frame,
                    None => bail!("no such label"),
                };
                for other_idx in idxs {
                    let other_frame = match frame.get(*other_idx) {
                        Some(other_frame) => other_frame,
                        None => bail!("no such frame"),
                    };
                    if other_frame.label_types != up_frame.label_types {
                        bail!("mismatched label types");
                    }
                }
                let _ = opds.pop_check(Some(I32))?;
                for ty in up_frame.label_types.iter().rev() {
                    let _ = opds.pop_check(Some(*ty))?;
                }
                opds.truncate(frame.height)?;
                frame.unreachable = true;
            },
            Return => {
                if let Some(ret_type_opt) = ctx.return_ {
                    if let Some(ret_type) = ret_type_opt {
                        let _ = opds.pop_check(Some(ret_type))?;
                    }
                    opds.truncate(frame.height)?;
                    frame.unreachable = true;
                } else {
                    bail!("return used outside of function");
                }
            },
            Call(FuncIdx(func_idx)) => {
                let func_type = match ctx.funcs.get(*func_idx as usize) {
                    Some(func_type) => func_type,
                    None => bail!("invalid function index"),
                };
                for param in &func_type.params {
                    let _ = opds.pop_check(Some(*param))?;
                }
                for result in &func_type.result {
                    opds.push(Some(*result));
                }
            },
            CallIndirect(TypeIdx(type_idx)) => {
                let func_type = match ctx.types.get(*type_idx as usize) {
                    Some(func_type) => func_type,
                    None => bail!("invalid function index"),
                };
                let _ = opds.pop_check(Some(I32))?;
                for param in &func_type.params {
                    let _ = opds.pop_check(Some(*param))?;
                }
                for result in &func_type.result {
                    opds.push(Some(*result));
                }
            },
        };
        Ok(())
    }
}

impl MemArg {
    pub fn check(&self, max_align: u32) -> Result<(), Error> {
        if self.align > max_align {
            bail!("invalid alignment");
        }
        Ok(())
    }
}

trait CheckVecInstruction {
    fn check<'f>(
        &self,
        ctx: &mut Context,
        opds: &mut Stack,
        label_types: &[ValType],
        end_types: &[ValType],
        prev: Option<&Frame<'f>>,
    ) -> Result<(), Error>;
}

impl CheckVecInstruction for Vec<Instruction> {
    fn check<'f>(
        &self,
        ctx: &mut Context,
        opds: &mut Stack,
        label_types: &[ValType],
        end_types: &[ValType],
        prev: Option<&Frame<'f>>,
    ) -> Result<(), Error> {
        let mut frame = Frame {
            label_types,
            end_types,
            height: opds.len(),
            prev,
            unreachable: false,
        };
        for instruction in self {
            instruction.check(ctx, opds, &mut frame)?;
        }
        for end_type in frame.end_types.iter().rev() {
            opds.pop_check(Some(*end_type))?;
        }
        for end_type in frame.end_types {
            opds.push(Some(*end_type));
        }
        Ok(())
    }
}
