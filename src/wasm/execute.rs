use super::*;

pub trait WasmValType {
}

pub trait WasmValTypeList {
}

impl WasmValType for u32 {
}

pub struct ModInstance {
}

impl Module {
    pub fn instantiate<I>(&self, imports: I) -> Result<ModInstance, InstantiateError>
    where
        I: FnMut(&str) -> Option<&mut ModInstance>
    {

    }
}

