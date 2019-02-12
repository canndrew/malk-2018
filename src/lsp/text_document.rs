use super::*;

pub struct TextDocumentItem {
    pub uri: DocumentUri,
    pub text: String,
}

impl FromJson for TextDocumentItem {
    fn from_json(json: Value) -> Result<TextDocumentItem, Error> {
        let (uri,text,) = (
            need::<DocumentUri>("uri"),
            need::<String>("text"),
        ).parse_object(json)?;
        Ok(TextDocumentItem {
            uri,
            text,
        })
    }
}

pub struct DidOpenTextDocumentParams {
    pub text_document: TextDocumentItem,
}

impl FromJson for DidOpenTextDocumentParams {
    fn from_json(json: Value) -> Result<DidOpenTextDocumentParams, Error> {
        let (text_document,) = (need::<TextDocumentItem>("textDocument"),).parse_object(json)?;
        Ok(DidOpenTextDocumentParams {
            text_document,
        })
    }
}

impl ToJson for DidOpenTextDocumentParams {
    fn to_json(self) -> Value {
        Value::Object(Map::new())
    }
}

pub struct DidChangeTextDocumentParams {
    pub text_document: VersionedTextDocumentIdentifier,
    pub content_changes: Vec<TextDocumentContentChangeEvent>,
}

impl ToJson for DidChangeTextDocumentParams {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("contentChanges"), self.content_changes.to_json());
        Value::Object(map)
    }
}

impl FromJson for DidChangeTextDocumentParams {
    fn from_json(json: Value) -> Result<DidChangeTextDocumentParams, Error> {
        let (text_document, content_changes,) = (
            need::<VersionedTextDocumentIdentifier>("textDocument"),
            need::<Vec<TextDocumentContentChangeEvent>>("contentChanges"),
        ).parse_object(json)?;
        Ok(DidChangeTextDocumentParams {
            text_document,
            content_changes,
        })
    }
}

pub struct TextDocumentContentChangeEvent {
    pub text: String,
}

impl ToJson for TextDocumentContentChangeEvent {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("text"), Value::from(self.text));
        Value::Object(map)
    }
}

impl FromJson for TextDocumentContentChangeEvent {
    fn from_json(json: Value) -> Result<TextDocumentContentChangeEvent, Error> {
        let (text,) = (need::<String>("text"),).parse_object(json)?;
        Ok(TextDocumentContentChangeEvent {
            text,
        })
    }
}

pub struct TextDocumentIdentifier {
    uri: DocumentUri,
}

impl ToJson for TextDocumentIdentifier {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("uri"), self.uri.to_json());
        Value::Object(map)
    }
}

impl FromJson for TextDocumentIdentifier {
    fn from_json(json: Value) -> Result<TextDocumentIdentifier, Error> {
        let (uri,) = (need::<DocumentUri>("uri"),).parse_object(json)?;
        Ok(TextDocumentIdentifier {
            uri,
        })
    }
}

pub struct TextDocumentPositionParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

impl ToJson for TextDocumentPositionParams {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("text_document"), self.text_document.to_json());
        map.insert(String::from("position"), self.position.to_json());
        Value::Object(map)
    }
}

impl FromJson for TextDocumentPositionParams {
    fn from_json(json: Value) -> Result<TextDocumentPositionParams, Error> {
        let (text_document, position) = (
            need::<TextDocumentIdentifier>("textDocument"),
            need::<Position>("position"),
        ).parse_object(json)?;
        Ok(TextDocumentPositionParams {
            text_document, position,
        })
    }
}

pub enum DocumentHighlightKind {
    Text,
    Read,
    Write,
}

impl ToJson for DocumentHighlightKind {
    fn to_json(self) -> Value {
        match self {
            DocumentHighlightKind::Text => Value::from(1),
            DocumentHighlightKind::Read => Value::from(2),
            DocumentHighlightKind::Write => Value::from(3),
        }
    }
}

pub struct DocumentHighlight {
    pub range: Range,
    pub kind: DocumentHighlightKind,
}

impl ToJson for DocumentHighlight {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("range"), self.range.to_json());
        map.insert(String::from("kind"), self.kind.to_json());
        Value::Object(map)
    }
}

