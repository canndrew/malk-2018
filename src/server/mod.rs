use super::*;
use crate::lsp::*;
use parser::Expr;

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


pub struct Server {
    client: LspClient,
    open_uris: HashMap<Url, Option<Expr>>,
}

impl Server {
    pub fn new(client: LspClient) -> Server {
        Server {
            client,
            open_uris: HashMap::new(),
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
        trace!("in Server::_did_open");
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let expr_opt = self.open_uris.entry(uri.clone()).or_default();
        match compile::check(&text[..]) {
            Ok(Ok(expr)) => {
                *expr_opt = Some(expr);
                self.client.publish_diagnostics(uri, Vec::new());
            },
            Ok(Err(diagnostics)) => self.client.publish_diagnostics(uri, diagnostics),
            Err(error) => self.client.show_message(MessageType::Error, error.to_string()),
        }
    }

    fn did_change_text_document(&mut self, params: DidChangeTextDocumentParams) {
        debug!("in Server::text_document_did_change");
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
    }

    fn document_highlight(&mut self, params: TextDocumentPositionParams)
        -> BoxSendFuture<Option<Vec<DocumentHighlight>>, ResponseError>
    {
        println!("WAZZZUUHHHHH!!!");
        let expr_opt = match self.open_uris.get_mut(&params.text_document.uri) {
            Some(expr_opt) => expr_opt,
            None => return future::ok(None).into_send_boxed(),
        };
        match expr_opt {
            Some(expr) => {
                future::ok(Some(vec![DocumentHighlight {
                    range: expr.span_of_expr_or_pat_at_position(params.position).into(),
                    kind: Some(DocumentHighlightKind::Text),
                }])).into_send_boxed()
            },
            None => {
                future::ok(None).into_send_boxed()
            },
        }
    }

    fn execute_command(&mut self, params: ExecuteCommandParams)
        -> BoxSendFuture<Option<Value>, ResponseError>
    {
        if params.command == "normalise" {
            for (uri, expr_opt) in &self.open_uris {
                if let Some(expr) = expr_opt {
                    use wasmer_runtime::{imports, instantiate};
                    use crate::wasm::Encode;

                    let module = crate::wasm::build_expr(expr);
                    debug!("module == {:#?}", module);
                    let mut bytes = Vec::new();
                    module.encode(&mut bytes);

                    let imports = imports! {};
                    debug!("about to instantiate");
                    let instance = match instantiate(&bytes, &imports) {
                        Ok(instance) => instance,
                        Err(e) => {
                            debug!("instantiation failed: {}", e);
                            continue
                        },
                    };
                    debug!("instantiated");

                    let wow = &instance.context().memory(0).view::<u8>()[..];
                    let bang = unsafe { std::slice::from_raw_parts(wow.as_ptr() as *const u8, wow.len()) };

                    let new_expr = compile::interpret_value(
                        bang,
                        &crate::parser::Expr::Var(crate::parser::Ident::new("String", crate::lexer::Span {
                            start: crate::lexer::TextPos {
                                line: 0,
                                col: 0,
                                byte: 0,
                            },
                            end: crate::lexer::TextPos {
                                line: 0,
                                col: 0,
                                byte: 0,
                            },
                        },
                    )));
                    let text = format!("{}", new_expr);
                    let edit = WorkspaceEdit {
                        changes: None,
                        document_changes: Some(DocumentChanges::Edits(vec![
                            TextDocumentEdit {
                                text_document: VersionedTextDocumentIdentifier {
                                    uri: uri.to_owned(),
                                    version: None,
                                },
                                edits: vec![
                                    TextEdit {
                                        range: expr.span().into(),
                                        new_text: text,
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
                }
            }
        }
        future::ok(None).into_send_boxed()
    }
}

