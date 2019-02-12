use super::*;

pub struct ExecuteCommandParams {}

impl FromJson for ExecuteCommandParams {
    fn from_json(_json: Value) -> Result<ExecuteCommandParams, Error> {
        Ok(ExecuteCommandParams {})
    }
}

impl ToJson for ExecuteCommandParams {
    fn to_json(self) -> Value {
        Value::Object(Map::new())
    }
}

