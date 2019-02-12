use super::*;

pub trait Encode {
    fn encode(&self, w: &mut Vec<u8>);
}

impl<T: Encode> Encode for [T] {
    fn encode(&self, w: &mut Vec<u8>) {
        (self.len() as u32).encode(w);
        for val in self {
            val.encode(w);
        }
    }
}

impl Encode for str {
    fn encode(&self, w: &mut Vec<u8>) {
        w.extend(self.as_bytes());
    }
}

impl Encode for u32 {
    fn encode(&self, w: &mut Vec<u8>) {
        let mut x = *self;
        loop {
            let is_last = x & !0x7f == 0;
            let b = ((x & 0x7f) as u8) | if is_last { 0x00 } else { 0x80 };
            w.push(b);
            if is_last {
                break;
            }
            x = x >> 7;
        }
    }
}

impl Encode for u64 {
    fn encode(&self, w: &mut Vec<u8>) {
        let mut x = *self;
        loop {
            let is_last = x & !0x7f == 0;
            let b = ((x & 0x7f) as u8) | if is_last { 0x00 } else { 0x80 };
            w.push(b);
            if is_last {
                break;
            }
            x = x >> 7;
        }
    }
}

impl Encode for i32 {
    fn encode(&self, w: &mut Vec<u8>) {
        let mut x = *self;
        loop {
            let b = (x & 0x7f) as u8;
            x = x >> 7;
            if (x == 0 && b & 0x40 == 0) || (x == -1 && b & 0x40 == 0x40) {
                w.push(b);
                break;
            }
            w.push(b | 0x80);
        }
    }
}

impl Encode for i64 {
    fn encode(&self, w: &mut Vec<u8>) {
        let mut x = *self;
        loop {
            let b = (x & 0x7f) as u8;
            x = x >> 7;
            if (x == 0 && b & 0x40 == 0) || (x == -1 && b & 0x40 == 0x40) {
                w.push(b);
                break;
            }
            w.push(b | 0x80);
        }
    }
}

impl Encode for f32 {
    fn encode(&self, w: &mut Vec<u8>) {
        let x = self.to_bits();
        for i in 0..4 {
            w.push(((x >> (i * 8)) & 0xff) as u8);
        }
    }
}

impl Encode for f64 {
    fn encode(&self, w: &mut Vec<u8>) {
        let x = self.to_bits();
        for i in 0..8 {
            w.push(((x >> (i * 8)) & 0xff) as u8);
        }
    }
}
impl Encode for ValType {
    fn encode(&self, w: &mut Vec<u8>) {
        let b = match *self {
            ValType::I32 => 0x7f,
            ValType::I64 => 0x7e,
            ValType::F32 => 0x7d,
            ValType::F64 => 0x7c,
        };
        w.push(b);
    }
}

impl Encode for Option<ValType> {
    fn encode(&self, w: &mut Vec<u8>) {
        match self {
            Some(val_type) => val_type.encode(w),
            None => w.push(0x40),
        }
    }
}

impl Encode for FuncType {
    fn encode(&self, w: &mut Vec<u8>) {
        w.push(0x60);
        self.params.encode(w);
        self.result.encode(w);
    }
}

impl Encode for Limits {
    fn encode(&self, w: &mut Vec<u8>) {
        match self.max {
            None => {
                w.push(0x00);
                self.min.encode(w);
            },
            Some(max) => {
                w.push(0x01);
                self.min.encode(w);
                max.encode(w);
            },
        }
    }
}

impl Encode for MemType {
    fn encode(&self, w: &mut Vec<u8>) {
        self.limits.encode(w);
    }
}

impl Encode for GlobalType {
    fn encode(&self, w: &mut Vec<u8>) {
        self.val_type.encode(w);
        match self.mutable {
            false => w.push(0x00),
            true => w.push(0x01),
        }
    }
}

impl Encode for Instruction {
    fn encode(&self, w: &mut Vec<u8>) {
        use super::Instruction::*;

        match self {
            I32Const(x) => {
                w.push(0x41);
                x.encode(w);
            },
            I64Const(x) => {
                w.push(0x42);
                x.encode(w);
            },
            F32Const(x) => {
                w.push(0x43);
                x.encode(w);
            },
            F64Const(x) => {
                w.push(0x44);
                x.encode(w);
            },
            I32Eqz => w.push(0x45),
            I32Eq => w.push(0x46),
            I32Ne => w.push(0x47),
            I32LtS => w.push(0x48),
            I32LtU => w.push(0x49),
            I32GtS => w.push(0x4a),
            I32GtU => w.push(0x4b),
            I32LeS => w.push(0x4c),
            I32LeU => w.push(0x4d),
            I32GeS => w.push(0x4e),
            I32GeU => w.push(0x4f),
            I64Eqz => w.push(0x50),
            I64Eq => w.push(0x51),
            I64Ne => w.push(0x52),
            I64LtS => w.push(0x53),
            I64LtU => w.push(0x54),
            I64GtS => w.push(0x55),
            I64GtU => w.push(0x56),
            I64LeS => w.push(0x57),
            I64LeU => w.push(0x58),
            I64GeS => w.push(0x59),
            I64GeU => w.push(0x5a),
            F32Eq => w.push(0x5b),
            F32Ne => w.push(0x5c),
            F32Lt => w.push(0x5d),
            F32Gt => w.push(0x5e),
            F32Le => w.push(0x5f),
            F32Ge => w.push(0x60),
            F64Eq => w.push(0x61),
            F64Ne => w.push(0x62),
            F64Lt => w.push(0x63),
            F64Gt => w.push(0x64),
            F64Le => w.push(0x65),
            F64Ge => w.push(0x66),
            I32Clz => w.push(0x67),
            I32Ctz => w.push(0x68),
            I32Popcnt => w.push(0x69),
            I32Add => w.push(0x6a),
            I32Sub => w.push(0x6b),
            I32Mul => w.push(0x6c),
            I32DivS => w.push(0x6d),
            I32DivU => w.push(0x6e),
            I32RemS => w.push(0x6f),
            I32RemU => w.push(0x70),
            I32And => w.push(0x71),
            I32Or => w.push(0x72),
            I32Xor => w.push(0x73),
            I32Shl => w.push(0x74),
            I32ShrS => w.push(0x75),
            I32ShrU => w.push(0x76),
            I32Rotl => w.push(0x77),
            I32Rotr => w.push(0x78),
            I64Clz => w.push(0x79),
            I64Ctz => w.push(0x7a),
            I64Popcnt => w.push(0x7b),
            I64Add => w.push(0x7c),
            I64Sub => w.push(0x7d),
            I64Mul => w.push(0x7e),
            I64DivS => w.push(0x7f),
            I64DivU => w.push(0x80),
            I64RemS => w.push(0x81),
            I64RemU => w.push(0x82),
            I64And => w.push(0x83),
            I64Or => w.push(0x84),
            I64Xor => w.push(0x85),
            I64Shl => w.push(0x86),
            I64ShrS => w.push(0x87),
            I64ShrU => w.push(0x88),
            I64Rotl => w.push(0x89),
            I64Rotr => w.push(0x8a),
            F32Abs => w.push(0x8b),
            F32Neg => w.push(0x8c),
            F32Ceil => w.push(0x8d),
            F32Floor => w.push(0x8e),
            F32Trunc => w.push(0x8f),
            F32Nearest => w.push(0x90),
            F32Sqrt => w.push(0x91),
            F32Add => w.push(0x92),
            F32Sub => w.push(0x93),
            F32Mul => w.push(0x94),
            F32Div => w.push(0x95),
            F32Min => w.push(0x96),
            F32Max => w.push(0x97),
            F32Copysign => w.push(0x98),
            F64Abs => w.push(0x99),
            F64Neg => w.push(0x9a),
            F64Ceil => w.push(0x9b),
            F64Floor => w.push(0x9c),
            F64Trunc => w.push(0x9d),
            F64Nearest => w.push(0x9e),
            F64Sqrt => w.push(0x9f),
            F64Add => w.push(0xa0),
            F64Sub => w.push(0xa1),
            F64Mul => w.push(0xa2),
            F64Div => w.push(0xa3),
            F64Min => w.push(0xa4),
            F64Max => w.push(0xa5),
            F64Copysign => w.push(0xa6),
            I32Wrap => w.push(0xa7),
            I32TruncSF32 => w.push(0xa8),
            I32TruncUF32 => w.push(0xa9),
            I32TruncSF64 => w.push(0xaa),
            I32TruncUF64 => w.push(0xab),
            I64ExtendS => w.push(0xac),
            I64ExtendU => w.push(0xad),
            I64TruncSF32 => w.push(0xae),
            I64TruncUF32 => w.push(0xaf),
            I64TruncSF64 => w.push(0xb0),
            I64TruncUF64 => w.push(0xb1),
            F32ConvertSI32 => w.push(0xb2),
            F32ConvertUI32 => w.push(0xb3),
            F32ConvertSI64 => w.push(0xb4),
            F32ConvertUI64 => w.push(0xb5),
            F32Demote => w.push(0xb6),
            F64ConvertSI32 => w.push(0xb7),
            F64ConvertUI32 => w.push(0xb8),
            F64ConvertSI64 => w.push(0xb9),
            F64ConvertUI64 => w.push(0xba),
            F64Promote => w.push(0xbb),
            I32Reinterpret => w.push(0xbc),
            I64Reinterpret => w.push(0xbd),
            F32Reinterpret => w.push(0xbe),
            F64Reinterpret => w.push(0xbf),
            Drop => w.push(0x1a),
            Select => w.push(0x1b),
            GetLocal(local_idx) => {
                w.push(0x20);
                local_idx.encode(w);
            },
            SetLocal(local_idx) => {
                w.push(0x21);
                local_idx.encode(w);
            },
            TeeLocal(local_idx) => {
                w.push(0x22);
                local_idx.encode(w);
            },
            GetGlobal(global_idx) => {
                w.push(0x23);
                global_idx.encode(w);
            },
            SetGlobal(global_idx) => {
                w.push(0x24);
                global_idx.encode(w);
            },
            I32Load(mem_arg) => {
                w.push(0x28);
                mem_arg.encode(w);
            },
            I64Load(mem_arg) => {
                w.push(0x29);
                mem_arg.encode(w);
            },
            F32Load(mem_arg) => {
                w.push(0x2a);
                mem_arg.encode(w);
            },
            F64Load(mem_arg) => {
                w.push(0x2b);
                mem_arg.encode(w);
            },
            I32Load8S(mem_arg) => {
                w.push(0x2c);
                mem_arg.encode(w);
            },
            I32Load8U(mem_arg) => {
                w.push(0x2d);
                mem_arg.encode(w);
            },
            I32Load16S(mem_arg) => {
                w.push(0x2e);
                mem_arg.encode(w);
            },
            I32Load16U(mem_arg) => {
                w.push(0x2f);
                mem_arg.encode(w);
            },
            I64Load8S(mem_arg) => {
                w.push(0x30);
                mem_arg.encode(w);
            },
            I64Load8U(mem_arg) => {
                w.push(0x31);
                mem_arg.encode(w);
            },
            I64Load16S(mem_arg) => {
                w.push(0x32);
                mem_arg.encode(w);
            },
            I64Load16U(mem_arg) => {
                w.push(0x33);
                mem_arg.encode(w);
            },
            I64Load32S(mem_arg) => {
                w.push(0x34);
                mem_arg.encode(w);
            },
            I64Load32U(mem_arg) => {
                w.push(0x35);
                mem_arg.encode(w);
            },
            I32Store(mem_arg) => {
                w.push(0x36);
                mem_arg.encode(w);
            },
            I64Store(mem_arg) => {
                w.push(0x37);
                mem_arg.encode(w);
            },
            F32Store(mem_arg) => {
                w.push(0x38);
                mem_arg.encode(w);
            },
            F64Store(mem_arg) => {
                w.push(0x39);
                mem_arg.encode(w);
            },
            I32Store8(mem_arg) => {
                w.push(0x3a);
                mem_arg.encode(w);
            },
            I32Store16(mem_arg) => {
                w.push(0x3b);
                mem_arg.encode(w);
            },
            I64Store8(mem_arg) => {
                w.push(0x3c);
                mem_arg.encode(w);
            },
            I64Store16(mem_arg) => {
                w.push(0x3d);
                mem_arg.encode(w);
            },
            I64Store32(mem_arg) => {
                w.push(0x3e);
                mem_arg.encode(w);
            },
            MemorySize => {
                w.push(0x3f);
                w.push(0x00);
            },
            MemoryGrow => {
                w.push(0x40);
                w.push(0x00);
            },
            Nop => w.push(0x01),
            Unreachable => w.push(0x00),
            Block(return_type, instructions) => {
                w.push(0x02);
                return_type.encode(w);
                instructions.encode(w);
                w.push(0x0b);
            },
            Loop(return_type, instructions) => {
                w.push(0x03);
                return_type.encode(w);
                instructions.encode(w);
                w.push(0x0b);
            },
            If(return_type, then_instructions, else_instructions) => {
                w.push(0x04);
                return_type.encode(w);
                then_instructions.encode(w);
                if !else_instructions.is_empty() {
                    w.push(0x05);
                    else_instructions.encode(w);
                }
                w.push(0x0b);
            },
            Br(label_idx) => {
                w.push(0x0c);
                label_idx.encode(w);
            },
            BrIf(label_idx) => {
                w.push(0x0d);
                label_idx.encode(w);
            },
            BrTable(label_idxs, label_idx) => {
                w.push(0x0e);
                label_idxs.encode(w);
                label_idx.encode(w);
            },
            Return => w.push(0x0f),
            Call(func_idx) => {
                w.push(0x10);
                func_idx.encode(w);
            },
            CallIndirect(type_idx) => {
                w.push(0x11);
                type_idx.encode(w);
                w.push(0x00);
            },
        }
    }
}

impl Encode for Expr {
    fn encode(&self, w: &mut Vec<u8>) {
        self.instructions.encode(w);
        w.push(0x0b);
    }
}

impl Encode for TypeIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let TypeIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for FuncIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let FuncIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for TableIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let TableIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for MemIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let MemIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for GlobalIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let GlobalIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for LocalIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let LocalIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for LabelIdx {
    fn encode(&self, w: &mut Vec<u8>) {
        let LabelIdx(x) = self;
        x.encode(w);
    }
}

impl Encode for MemArg {
    fn encode(&self, w: &mut Vec<u8>) {
        self.align.encode(w);
        self.offset.encode(w);
    }
}

impl Encode for Module {
    fn encode(&self, w: &mut Vec<u8>) {
        fn encode_section<T: Encode + ?Sized>(id: u8, section: &T, w: &mut Vec<u8>, scratch: &mut Vec<u8>) {
            scratch.clear();
            section.encode(scratch);

            w.push(id);
            (scratch.len() as u32).encode(w);
            w.extend(&scratch[..])
        }

        let mut scratch = Vec::new();

        w.extend(&[0x00, 0x61, 0x73, 0x6d]);
        w.extend(&[0x01, 0x00, 0x00, 0x00]);

        if !self.types.is_empty() {
            encode_section(1, &self.types[..], w, &mut scratch);
        }
        
        if !self.imports.is_empty() {
            encode_section(2, &self.imports[..], w, &mut scratch);
        }

        if !self.funcs.is_empty() {
            let mut func_types = Vec::with_capacity(self.funcs.len());
            for func in &self.funcs {
                func_types.push(func.type_);
            }
            encode_section(3, &func_types[..], w, &mut scratch);
        }

        if !self.tables.is_empty() {
            encode_section(4, &self.tables[..], w, &mut scratch);
        }

        if !self.mems.is_empty() {
            encode_section(5, &self.mems[..], w, &mut scratch);
        }

        if !self.globals.is_empty() {
            encode_section(6, &self.globals[..], w, &mut scratch);
        }

        if !self.exports.is_empty() {
            encode_section(7, &self.exports[..], w, &mut scratch);
        }

        if let Some(ref start) = self.start {
            encode_section(9, start, w, &mut scratch);
        }

        if !self.funcs.is_empty() {
            let mut code_entries = Vec::with_capacity(self.funcs.len());
            for func in &self.funcs {
                let code_entry = CodeEntry {
                    locals: &func.locals[..],
                    body: &func.body,
                };
                code_entries.push(code_entry);
            }
            encode_section(10, &code_entries[..], w, &mut scratch);
        }

        if !self.data.is_empty() {
            encode_section(11, &self.data[..], w, &mut scratch);
        }
    }
}

struct CodeEntry<'a> {
    locals: &'a [ValType],
    body: &'a Expr,
}

impl<'a> Encode for CodeEntry<'a> {
    fn encode(&self, w: &mut Vec<u8>) {
        let mut function_code = Vec::new();

        let mut locals = self.locals.iter();
        let mut local_decls = Vec::new();
        if let Some(local_type) = locals.next() {
            let mut prev_local_type = local_type;
            let mut count = 1u32;
            for local_type in locals {
                if local_type == prev_local_type {
                    count += 1;
                } else {
                    local_decls.push((prev_local_type, count));
                    prev_local_type = local_type;
                    count = 1;
                }
            }
            local_decls.push((prev_local_type, count));
        }

        (local_decls.len() as u32).encode(&mut function_code);
        for (local_type, count) in local_decls {
            count.encode(&mut function_code);
            local_type.encode(&mut function_code);
        }
        self.body.encode(&mut function_code);

        (function_code.len() as u32).encode(w);
        w.extend(&function_code[..]);
    }
}

impl Encode for Import {
    fn encode(&self, w: &mut Vec<u8>) {
        self.module.encode(w);
        self.name.encode(w);
        self.desc.encode(w);
    }
}

impl Encode for ImportDesc {
    fn encode(&self, w: &mut Vec<u8>) {
        match self {
            ImportDesc::Func(type_idx) => {
                w.push(0x00);
                type_idx.encode(w);
            },
            ImportDesc::Table(table_type) => {
                w.push(0x01);
                table_type.encode(w);
            },
            ImportDesc::Mem(mem_type) => {
                w.push(0x02);
                mem_type.encode(w);
            },
            ImportDesc::Global(global_type) => {
                w.push(0x03);
                global_type.encode(w);
            },
        }
    }
}

impl Encode for TableType {
    fn encode(&self, w: &mut Vec<u8>) {
        w.push(0x70);
        self.limits.encode(w);
    }
}

impl Encode for Global {
    fn encode(&self, w: &mut Vec<u8>) {
        self.type_.encode(w);
        self.init.encode(w);
    }
}

impl Encode for Export {
    fn encode(&self, w: &mut Vec<u8>) {
        self.name.encode(w);
        self.desc.encode(w);
    }
}

impl Encode for Data {
    fn encode(&self, w: &mut Vec<u8>) {
        self.data.encode(w);
        self.offset.encode(w);
        w.extend(&self.init);
    }
}

impl Encode for ExportDesc {
    fn encode(&self, w: &mut Vec<u8>) {
        match self {
            ExportDesc::Func(func_idx) => {
                w.push(0x00);
                func_idx.encode(w);
            },
            ExportDesc::Table(table_idx) => {
                w.push(0x01);
                table_idx.encode(w);
            },
            ExportDesc::Mem(mem_idx) => {
                w.push(0x02);
                mem_idx.encode(w);
            },
            ExportDesc::Global(global_idx) => {
                w.push(0x03);
                global_idx.encode(w);
            },
        }
    }
}

pub fn write_to_file<P: AsRef<Path>>(module: &Module, path: P) -> io::Result<()> {
    let mut bytes = Vec::new();
    module.encode(&mut bytes);
    fs::write(path, &bytes)
}

