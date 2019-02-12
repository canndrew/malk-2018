use super::*;

pub struct DocumentUri(pub String);

pub struct VersionedTextDocumentIdentifier {
    pub uri: DocumentUri,
}

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub range: Range,
    pub msg: String,
}

pub struct PublishDiagnosticsParams {
    pub uri: DocumentUri,
    pub diagnostics: Vec<Diagnostic>,
}

impl FromJson for DocumentUri {
    fn from_json(json: Value) -> Result<DocumentUri, Error> {
        match json {
            Value::String(s) => Ok(DocumentUri(s)),
            _ => bail!("expected string for DocumentUri"),
        }
    }
}

impl ToJson for DocumentUri {
    fn to_json(self) -> Value {
        Value::String(self.0)
    }
}

impl FromJson for Diagnostic {
    fn from_json(json: Value) -> Result<Diagnostic, Error> {
        let (range, msg) = (
            need::<Range>("range"),
            need::<String>("message"),
        ).parse_object(json)?;
        Ok(Diagnostic {
            range,
            msg,
        })
    }
}

impl ToJson for Diagnostic {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("range"), self.range.to_json());
        map.insert(String::from("message"), Value::from(self.msg));
        Value::Object(map)
    }
}

impl ToJson for PublishDiagnosticsParams {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("uri"), self.uri.to_json());
        map.insert(String::from("diagnostics"), self.diagnostics.to_json());
        Value::Object(map)
    }
}

impl ToJson for Range {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("start"), self.start.to_json());
        map.insert(String::from("end"), self.end.to_json());
        Value::Object(map)
    }
}

impl ToJson for Position {
    fn to_json(self) -> Value {
        let mut map = Map::new();
        map.insert(String::from("line"), Value::from(self.line));
        map.insert(String::from("character"), Value::from(self.character));
        Value::Object(map)
    }
}

impl FromJson for Range {
    fn from_json(json: Value) -> Result<Range, Error> {
        let (start, end) = (
            need::<Position>("start"),
            need::<Position>("end"),
        ).parse_object(json)?;
        Ok(Range {
            start,
            end,
        })
    }
}

impl FromJson for Position {
    fn from_json(json: Value) -> Result<Position, Error> {
        let (line, character) = (
            need::<usize>("line"),
            need::<usize>("character"),
        ).parse_object(json)?;
        Ok(Position {
            line,
            character,
        })
    }
}

impl FromJson for VersionedTextDocumentIdentifier {
    fn from_json(json: Value) -> Result<VersionedTextDocumentIdentifier, Error> {
        let (uri,) = (
            need::<DocumentUri>("uri"),
        ).parse_object(json)?;
        Ok(VersionedTextDocumentIdentifier {
            uri,
        })
    }
}

