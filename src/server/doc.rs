use super::*;
use std::sync::Arc;
use crate::parser::{self, Ast};
use crate::core::Term;
use lsp_types::{Diagnostic, Url, TextDocumentContentChangeEvent, Position};

pub struct Doc {
    uri: Arc<Url>,
    text: String,
    parsed: Option<Ast<Term>>,
    diagnostics: Vec<Diagnostic>,
}

impl Doc {
    fn parse(&mut self) {
        trace!("parsing doc");
        match parser::parse_doc(&self.uri, &self.text) {
            Ok(term) => {
                self.parsed = Some(term);
                self.diagnostics = vec![];
            },
            Err(e) => {
                self.parsed = None;
                self.diagnostics = vec![e];
            },
        }
    }

    pub fn new(uri: Url, text: String) -> Doc {
        let uri = Arc::new(uri);
        let mut ret = Doc {
            uri,
            text,
            parsed: None,
            diagnostics: vec![],
        };
        ret.parse();
        ret
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn change_content(&mut self, change: TextDocumentContentChangeEvent) {
        match change.range {
            Some(range) => {
                let start = self.find_byte_pos(range.start);
                let end = self.find_byte_pos(range.end);
                self.text.replace_range(start..end, &change.text);
            },
            None => {
                self.text = change.text;
            },
        }
        self.parse();
    }

    pub fn parsed(&self) -> Option<&Ast<Term>> {
        self.parsed.as_ref()
    }

    fn find_byte_pos(&self, position: Position) -> usize {
        let mut char_indices = self.text.char_indices().peekable();
        let mut line = 0;
        let mut character = 0;
        loop {
            if line == position.line && character == position.character {
                let ret = char_indices.peek().map(|(i, _)| *i).unwrap_or(self.text.len());
                return ret;
            }

            if line > position.line || (line == position.line && character > position.character) {
                panic!("invalid position");
            }

            let (_, c) = unwrap!(char_indices.next());
            if c == '\n' {
                line += 1;
                character = 0;
                continue;
            }
            if c == '\r' {
                if let Some((_, '\n')) = char_indices.peek() {
                    continue;
                }
                line += 1;
                character = 0;
                continue;
            }
            let mut units = [0; 2];
            character += c.encode_utf16(&mut units).len() as u64;
        }
    }

    pub fn highlight(&self, position: Position) -> Vec<DocumentHighlight> {
        let parsed = match self.parsed {
            Some(ref parsed) => parsed,
            None => return Vec::new(),
        };

        if let Some(app) = parsed.app_at_position(position) {
            if let Origin::Document { range, .. } = app.origin {
                return vec![DocumentHighlight {
                    range,
                    kind: Some(DocumentHighlightKind::Write),
                }];
            }
        };

        Vec::new()
    }
}

