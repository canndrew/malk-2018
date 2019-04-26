use super::*;
use crate::lsp::*;
use parser::Origin;

use serde_json::Value;
use lsp_types::{
    request::{
        Request,
        Initialize,
        ExecuteCommand,
        DocumentHighlightRequest,
    },
    notification::{
        Initialized,
        DidOpenTextDocument,
        DidChangeTextDocument,
        ShowMessage,
        PublishDiagnostics,
    },
    VersionedTextDocumentIdentifier,
    TextDocumentEdit,
    TextEdit,
    DocumentChanges,
    WorkspaceEdit,
    ExecuteCommandOptions,
    ExecuteCommandParams,
    ServerCapabilities,
    MessageType,
    TextDocumentPositionParams,
    TextDocumentContentChangeEvent,
    DocumentHighlightKind,
    ShowMessageParams,
    Url,
    Diagnostic,
    PublishDiagnosticsParams,
    InitializeParams,
    InitializeResult,
    DocumentHighlight,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
};
use self::doc::Doc;

mod doc;

pub struct Server {
    client: LspClient,
    open_docs: HashMap<Url, Doc>,
    cursor_position: Option<TextDocumentPositionParams>,
}

impl Server {
    pub fn new(client: LspClient) -> Server {
        Server {
            client,
            open_docs: HashMap::new(),
            cursor_position: None,
        }
    }
}

impl LspServer for Server {
    fn initialize(&mut self, _initialize_params: InitializeParams)
        -> BoxSendFuture<InitializeResult, ResponseError>
    {
        let response = InitializeResult {
            capabilities: ServerCapabilities {
                document_highlight_provider: Some(true),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![String::from("normalise")],
                }),
                .. ServerCapabilities::default()
            }
        };
        future::ok(response).into_send_boxed()
    }

    fn initialized(&mut self) {}

    fn did_open_text_document(&mut self, params: DidOpenTextDocumentParams) {
        trace!("in did_open_text_document");
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let doc = Doc::new(uri.clone());
        trace!("created doc");
        let doc = self.open_docs.entry(uri.clone()).or_insert(doc);
        doc.change_content(TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: text,
        });
        self.client.publish_diagnostics(uri, doc.diagnostics());
        trace!("published diagnostics");
    }

    fn did_change_text_document(&mut self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = unwrap!(self.open_docs.get_mut(&uri));
        for change in params.content_changes {
            doc.change_content(change);
        }
        self.client.publish_diagnostics(uri, doc.diagnostics());

        /*
        let uri = params.text_document.uri;
        let changes = params.content_changes;
        let expr_opt = match self.open_uris.get_mut(&uri) {
            Some(expr_opt) => expr_opt,
            None => return,
        };
        debug!("processing {} changes", changes.len());
        match changes.last() {
            Some(change) => {
                *expr_opt = None;
                match compile::check(&change.text[..]) {
                    Ok(Ok(expr)) => {
                        *expr_opt = Some(expr);
                        self.client.publish_diagnostics(uri, Vec::new());
                    },
                    Ok(Err(diagnostics)) => self.client.publish_diagnostics(uri, diagnostics),
                    Err(error) => self.client.show_message(MessageType::Error, error.to_string()),
                }
            },
            None => (),
        }
        */
    }

    fn document_highlight(&mut self, params: TextDocumentPositionParams)
        -> BoxSendFuture<Option<Vec<DocumentHighlight>>, ResponseError>
    {
        let uri = &params.text_document.uri;
        let doc = unwrap!(self.open_docs.get_mut(&uri));
        let highlights = doc.highlight(params.position);
        self.cursor_position = Some(params);
        future::ok(Some(highlights)).into_send_boxed()
    }

    fn execute_command(&mut self, params: ExecuteCommandParams)
        -> BoxSendFuture<Option<Value>, ResponseError>
    {
        trace!("in execute_command");
        /*
        if params.command == "normalise" {
            trace!("its a normalise");
            let cursor_position = match &self.cursor_position {
                Some(cursor_position) => cursor_position,
                None => return future::ok(None).into_send_boxed(),
            };
            let doc = match self.open_docs.get(&cursor_position.text_document.uri) {
                Some(doc) => doc,
                None => return future::ok(None).into_send_boxed(),
            };
            let term = match doc.parsed() {
                Some(term) => term,
                None => return future::ok(None).into_send_boxed(),
            };
            let redex = match term.redex_at_position(cursor_position.position) {
                Some(redex) => redex,
                None => return future::ok(None).into_send_boxed(),
            };
            let (uri, range) = match &redex.origin {
                Origin::Document { uri, range } => (uri, range),
                _ => return future::ok(None).into_send_boxed(),
            };
            let before = {
                unwrap!(doc.text().lines().nth(range.start.line as usize))
                .split_to_lsp_character_pos(range.start.character)
            };
            let after = {
                unwrap!(doc.text().lines().nth(range.end.line as usize))
                .split_from_lsp_character_pos(range.end.character)
            };
            let reduced = redex.reduce_head();
            let rendered = reduced.render(before, after);
            let edit = WorkspaceEdit {
                changes: None,
                document_changes: Some(DocumentChanges::Edits(vec![
                    TextDocumentEdit {
                        text_document: VersionedTextDocumentIdentifier {
                            uri: (**uri).clone(),
                            version: None,
                        },
                        edits: vec![
                            TextEdit {
                                range: *range,
                                new_text: rendered,
                            },
                        ],
                    },
                ])),
            };
            tokio::spawn(
                self
                .client
                .apply_edit(edit)
                .then(|_res| Ok(()))
            );
            return future::ok(None).into_send_boxed();
        }
        */
        future::ok(None).into_send_boxed()
    }
}

