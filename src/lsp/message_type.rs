use super::*;

#[repr(i32)]
pub enum MessageType {
    Error = 1,
    Warning = 2,
    Info = 3,
    Log = 4,
}

impl ToJson for MessageType {
    fn to_json(self) -> Value {
        Value::from(self as i32)
    }
}

