use super::*;

#[derive(Serialize, Deserialize)]
#[repr(i32)]
pub enum ErrorCode {
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,
    ServerErrorStart= -32099,
    ServerErrorEnd = -32000,
    ServerNotInitialized = -32002,
    UnknownErrorCode = -32001,
    RequestCancelled = -32800,
}

pub struct RequestMessage {
    pub id: u64,
    pub method: String,
    pub params: Value,
}

pub struct ResponseMessage {
    pub id: u64,
    pub response: Result<Value, ResponseError>,
}

#[derive(Serialize, Deserialize)]
pub struct ResponseError {
    pub code: ErrorCode,
    pub message: String,
    pub data: Value,
}

pub struct NotificationMessage {
    pub method: String,
    pub params: Value,
}

pub enum Message {
    Request(RequestMessage),
    Response(ResponseMessage),
    Notification(NotificationMessage),
}

impl Message {
    pub fn from_json(json: Value) -> Result<Message, Error> {
        let mut object = match json {
            Value::Object(object) => object,
            _ => bail!("json value is not an object"),
        };
        let id = match object.get("id") {
            Some(id) => match id.as_u64() {
                Some(id) => id,
                None => bail!("json object's id field is not a valid integer"),
            },
            None => {
                let method = match object.remove("method") {
                    Some(method) => match method {
                        Value::String(method) => method,
                        _ => bail!("json object's method field is not a string"),
                    },
                    None => bail!("json object is not a valid lsp message. it contains neither an id field or a method field"),
                };
                let params = object.remove("params").unwrap_or(Value::Null);
                return Ok(Message::Notification(NotificationMessage { method, params }));
            },
        };
        if let Some(method) = object.remove("method") {
            let method = match method {
                Value::String(method) => method,
                _ => bail!("json object's method field is not a string"),
            };
            let params = object.remove("params").unwrap_or(Value::Null);
            return Ok(Message::Request(RequestMessage { id, method, params }));
        }
        if let Some(result) = object.remove("result") {
            let response = Ok(result);
            return Ok(Message::Response(ResponseMessage { id, response }));
        }
        if let Some(error) = object.remove("error") {
            let error = match serde_json::from_value(error) {
                Ok(error) => error,
                Err(e) => bail!("json object's error field is malformatted: {}", e),
            };
            let response = Err(error);
            return Ok(Message::Response(ResponseMessage { id, response }));
        }
        bail!("json object is not a valid lsp message. It does not contain a method, result or error field");
    }

    pub fn to_json(self) -> Value {
        match self {
            Message::Request(request) => {
                json!({
                    "jsonrpc": "2.0",
                    "id": request.id,
                    "method": request.method,
                    "params": request.params,
                })
            },
            Message::Response(response) => {
                match response.response {
                    Ok(result) => {
                        json!({
                            "jsonrpc": "2.0",
                            "id": response.id,
                            "result": result,
                        })
                    },
                    Err(error) => {
                        json!({
                            "jsonrpc": "2.0",
                            "id": response.id,
                            "error": error,
                        })
                    },
                }
            },
            Message::Notification(notification) => {
                json!({
                    "jsonrpc": "2.0",
                    "method": notification.method,
                    "params": notification.params,
                })
            },
        }
    }
}

