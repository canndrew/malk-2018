use super::*;

pub struct InitializeParams {}

pub struct InitializeResponse {
    pub capabilities: ServerCapabilities,
}

pub struct InitializeError;

pub struct ServerCapabilities;

impl FromJson for InitializeParams {
    fn from_json(_json: Value) -> Result<InitializeParams, Error> {
        Ok(InitializeParams {})
    }
}

impl ToJson for InitializeParams {
    fn to_json(self) -> Value {
        Value::Object(Map::new())
    }
}

impl ToJson for InitializeResponse {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("capabilities"), self.capabilities.to_json());
        Value::Object(map)
    }
}

impl ToErrorKind for InitializeError {
    fn to_error_kind(self) -> Option<ErrorKind> {
        None
    }
}

impl ToJson for ServerCapabilities {
    fn to_json(self) -> Value {
        let mut caps = Map::new();
        caps.insert(String::from("documentHighlightProvider"), Value::Bool(true));
        Value::Object(caps)
    }
}

