#[derive(Debug)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<TableType>,
    pub mems: Vec<MemType>,
    pub globals: Vec<Global>,
    pub elem: Vec<Elem>,
    pub data: Vec<Data>,
    pub start: Option<FuncIdx>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

#[derive(Debug)]
pub struct TableType {
    pub limits: Limits,
}

#[derive(Debug)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug)]
pub struct MemType {
    pub limits: Limits,
}

#[derive(Debug)]
pub struct Global {
    pub type_: GlobalType,
    pub init: Expr,
}

#[derive(Debug)]
pub struct GlobalType {
    pub val_type: ValType,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Vec<FuncIdx>,
}

#[derive(Debug)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Vec<u8>,
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Debug)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<ValType>,
    pub result: Vec<ValType>,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct Func {
    pub type_: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    F32Abs,
    F32Neg,
    F32Sqrt,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F64Abs,
    F64Neg,
    F64Sqrt,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivU,
    I32DivS,
    I32RemU,
    I32RemS,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrU,
    I32ShrS,
    I32Rotl,
    I32Rotr,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivU,
    I64DivS,
    I64RemU,
    I64RemS,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrU,
    I64ShrS,
    I64Rotl,
    I64Rotr,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    I32Eqz,
    I64Eqz,
    I32Eq,
    I32Ne,
    I32LtU,
    I32LtS,
    I32GtU,
    I32GtS,
    I32LeU,
    I32LeS,
    I32GeU,
    I32GeS,
    I64Eq,
    I64Ne,
    I64LtU,
    I64LtS,
    I64GtU,
    I64GtS,
    I64LeU,
    I64LeS,
    I64GeU,
    I64GeS,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    I32Wrap,
    I64ExtendU,
    I64ExtendS,
    I32TruncUF32,
    I32TruncSF32,
    I64TruncUF32,
    I64TruncSF32,
    I32TruncUF64,
    I32TruncSF64,
    I64TruncUF64,
    I64TruncSF64,
    F32Demote,
    F64Promote,
    F32ConvertUI32,
    F32ConvertSI32,
    F64ConvertUI32,
    F64ConvertSI32,
    F32ConvertUI64,
    F32ConvertSI64,
    F64ConvertUI64,
    F64ConvertSI64,
    F32Reinterpret,
    F64Reinterpret,
    I32Reinterpret,
    I64Reinterpret,
    Drop,
    Select,
    GetLocal(LocalIdx),
    SetLocal(LocalIdx),
    TeeLocal(LocalIdx),
    GetGlobal(GlobalIdx),
    SetGlobal(GlobalIdx),
    I32Load(MemArg),
    I64Load(MemArg),
    F32Load(MemArg),
    F64Load(MemArg),
    I32Store(MemArg),
    I64Store(MemArg),
    F32Store(MemArg),
    F64Store(MemArg),
    I32Load8U(MemArg),
    I32Load8S(MemArg),
    I64Load8U(MemArg),
    I64Load8S(MemArg),
    I32Load16U(MemArg),
    I32Load16S(MemArg),
    I64Load16U(MemArg),
    I64Load16S(MemArg),
    I64Load32U(MemArg),
    I64Load32S(MemArg),
    I32Store8(MemArg),
    I64Store8(MemArg),
    I32Store16(MemArg),
    I64Store16(MemArg),
    I64Store32(MemArg),
    MemorySize,
    MemoryGrow,
    Nop,
    Unreachable,
    Block(Vec<ValType>, Vec<Instruction>),
    Loop(Vec<ValType>, Vec<Instruction>),
    If(Vec<ValType>, Vec<Instruction>, Vec<Instruction>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Vec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),
}

#[derive(Debug)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct TypeIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct FuncIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct TableIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct MemIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct GlobalIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct LocalIdx(pub u32);
#[derive(Clone, Copy, Debug)]
pub struct LabelIdx(pub u32);

