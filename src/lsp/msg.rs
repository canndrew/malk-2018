use super::*;

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

pub enum RequestMsg {
    Initialize(InitializeParams),
    ExecuteCommand(ExecuteCommandParams),
    DocumentHighlight(TextDocumentPositionParams),
}

pub enum ResponseMsg {
    Initialize(InitializeResponse),
    ExecuteCommand,
    DocumentHighlight(Option<Vec<DocumentHighlight>>),
}

pub enum NotificationMsg {
    Initialized,
    TextDocumentDidOpen(DidOpenTextDocumentParams),
    TextDocumentDidSave,
    TextDocumentDidChange(DidChangeTextDocumentParams),
    WindowShowMessage(MessageType, String),
    TextDocumentPublishDiagnostics(PublishDiagnosticsParams),
}

pub struct ErrorResponse<E> {
    pub code: ErrorCode,
    pub msg: String,
    pub data: E,
}

pub struct ErrorMsg {
    pub code: ErrorCode,
    pub msg: String,
    pub data: Option<ErrorKind>,
}

pub enum ErrorKind {}

pub enum Msg {
    Request(u64, RequestMsg),
    Response(u64, ResponseMsg),
    Error(u64, ErrorMsg),
    Notification(NotificationMsg),
}

impl FromJson for Msg {
    fn from_json(json: Value) -> Result<Msg, Error> {
        let object_spec = (
            want::<u64>("id"),
            want::<String>("method"),
            want::<Value>("params"),
            want::<Value>("result"),
            want::<Value>("error"),
        );

        let parsed_msg = object_spec.parse_object(json)?;
        let msg = match parsed_msg {
            (Some(id), Some(method), Some(params), None, None) => {
                let request = match &method[..] {
                    "initialize" => {
                        let params = InitializeParams::from_json(params)?;
                        RequestMsg::Initialize(params)
                    },
                    "workspace/executeCommand" => {
                        let params = ExecuteCommandParams::from_json(params)?;
                        RequestMsg::ExecuteCommand(params)
                    },
                    "textDocument/documentHighlight" => {
                        let params = TextDocumentPositionParams::from_json(params)?;
                        RequestMsg::DocumentHighlight(params)
                    },
                    _ => bail!("unknown request method: {}", method),
                };
                Msg::Request(id, request)
            },
            (Some(_id), Some(method), None, None, None) => {
                let _request = match method {
                    _ => bail!("unknown request method: {}", method),
                };
                //Msg::Request(id, request)
            },
            (None, Some(method), Some(params), None, None) => {
                let notification = match &method[..] {
                    "initialized" => NotificationMsg::Initialized,
                    "textDocument/didOpen" => {
                        let params = DidOpenTextDocumentParams::from_json(params)?;
                        NotificationMsg::TextDocumentDidOpen(params)
                    },
                    "textDocument/didChange" => {
                        let params = DidChangeTextDocumentParams::from_json(params)?;
                        NotificationMsg::TextDocumentDidChange(params)
                    },
                    "textDocument/didSave" => {
                        NotificationMsg::TextDocumentDidSave
                    },
                    _ => bail!("unknown notification method: {}", method),
                };
                Msg::Notification(notification)
            },
            (None, Some(method), None, None, None) => {
                bail!("unknown notification method: {}", method)
            },
            _ => bail!("invalid message fields"),
        };

        Ok(msg)
    }
}

impl ToJson for Msg {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("jsonrpc"), Value::from("2.0"));
        match self {
            Msg::Request(id, request_msg) => {
                map.insert(String::from("id"), Value::from(id));
                map.insert(String::from("method"), Value::from(request_msg.method()));
                if let Some(params) = request_msg.params() {
                    map.insert(String::from("params"), params);
                }
            },
            Msg::Response(id, response_msg) => {
                map.insert(String::from("id"), Value::from(id));
                map.insert(String::from("result"), response_msg.to_json());
            },
            Msg::Error(id, error_msg) => {
                map.insert(String::from("id"), Value::from(id));
                map.insert(String::from("error"), error_msg.to_json());
            },
            Msg::Notification(notification_msg) => {
                map.insert(String::from("method"), Value::from(notification_msg.method()));
                if let Some(params) = notification_msg.params() {
                    map.insert(String::from("params"), params);
                }
            },
        }
        Value::from(map)
    }
}

impl RequestMsg {
    pub fn method(&self) -> &'static str {
        match self {
            RequestMsg::Initialize(..) => "initialize",
            RequestMsg::ExecuteCommand(..) => "workspace/executeCommand",
            RequestMsg::DocumentHighlight(..) => "textDocument/documentHighlight",
        }
    }

    pub fn params(self) -> Option<Value> {
        match self {
            RequestMsg::Initialize(initialize_params) => {
                Some(initialize_params.to_json())
            },
            RequestMsg::ExecuteCommand(params) => {
                Some(params.to_json())
            },
            RequestMsg::DocumentHighlight(params) => {
                Some(params.to_json())
            },
        }
    }
}

impl NotificationMsg {
    pub fn method(&self) -> &'static str {
        match self {
            NotificationMsg::Initialized => "initialized",
            NotificationMsg::TextDocumentDidOpen(..) => "textDocument/didOpen",
            NotificationMsg::TextDocumentDidSave => "textDocument/didSave",
            NotificationMsg::TextDocumentDidChange(..) => "textDocument/didChange",
            NotificationMsg::WindowShowMessage(..) => "window/showMessage",
            NotificationMsg::TextDocumentPublishDiagnostics(..) => "textDocument/publishDiagnostics",
        }
    }

    pub fn params(self) -> Option<Value> {
        match self {
            NotificationMsg::Initialized => None,
            NotificationMsg::TextDocumentDidOpen(params) => Some(params.to_json()),
            NotificationMsg::TextDocumentDidSave => None,
            NotificationMsg::TextDocumentDidChange(params) => Some(params.to_json()),
            NotificationMsg::WindowShowMessage(ty, message) => {
                let mut map = Map::new();
                map.insert(String::from("type"), ty.to_json());
                map.insert(String::from("message"), Value::from(message));
                Some(Value::Object(map))
            },
            NotificationMsg::TextDocumentPublishDiagnostics(params) => Some(params.to_json()),
        }
    }
}

impl ToJson for ResponseMsg {
    fn to_json(self) -> Value {
        match self {
            ResponseMsg::Initialize(response) => response.to_json(),
            ResponseMsg::ExecuteCommand => Value::Null,
            ResponseMsg::DocumentHighlight(response) => match response {
                Some(document_highlights) => document_highlights.to_json(),
                None => Value::Null,
            }
        }
    }
}

impl ToJson for ErrorMsg {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("code"), Value::from(self.code as u32));
        map.insert(String::from("message"), Value::from(self.msg));
        /*
        if let Some(kind) = self.data {
            map.insert(String::from("data"), kind.to_json());
        }
        */
        Value::from(map)
    }
}

pub trait ToErrorKind {
    fn to_error_kind(self) -> Option<ErrorKind>;
}

impl<E> ErrorResponse<E>
where
    E: ToErrorKind,
{
    pub fn to_error_msg(self) -> ErrorMsg {
        ErrorMsg {
            code: self.code,
            msg: self.msg,
            data: self.data.to_error_kind(),
        }
    }
}

impl ToErrorKind for () {
    fn to_error_kind(self) -> Option<ErrorKind> {
        None
    }
}

impl ToJson for ErrorKind {
    fn to_json(self) -> Value {
        match self {}
    }
}

