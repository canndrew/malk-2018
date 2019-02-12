use super::*;
use crate::lsp::*;
use parser::Expr;

pub struct Server {
    client: LspClient,
    expr: Option<Expr>,
}

impl Server {
    pub fn new(client: LspClient) -> Server {
        Server {
            client,
            expr: None,
        }
    }
}

impl LspServer for Server {
    fn initialize(&mut self, _initialize_params: InitializeParams)
        -> BoxSendFuture<InitializeResponse, ErrorResponse<InitializeError>>
    {
        let response = InitializeResponse {
            capabilities: ServerCapabilities,
        };
        future::ok(response).into_send_boxed()
    }

    fn initialized(&mut self) {}

    fn execute_command(&mut self) -> BoxSendFuture<(), ErrorResponse<()>> {
        future::ok(()).into_send_boxed()
    }

    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        trace!("in Server::_did_open");
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.expr = None;
        match compile::check(&text[..]) {
            Ok(Ok(expr)) => {
                self.expr = Some(expr);
                self.client.text_document_publish_diagnostics(uri, Vec::new());
            },
            Ok(Err(diagnostics)) => self.client.text_document_publish_diagnostics(uri, diagnostics),
            Err(error) => self.client.show_message(MessageType::Error, error.to_string()),
        }
    }

    fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) {
        debug!("in Server::text_document_did_change");
        let uri = params.text_document.uri;
        let changes = params.content_changes;
        debug!("processing {} changes", changes.len());
        match changes.last() {
            Some(change) => {
                self.expr = None;
                match compile::check(&change.text[..]) {
                    Ok(Ok(expr)) => {
                        self.expr = Some(expr);
                        self.client.text_document_publish_diagnostics(uri, Vec::new());
                    },
                    Ok(Err(diagnostics)) => self.client.text_document_publish_diagnostics(uri, diagnostics),
                    Err(error) => self.client.show_message(MessageType::Error, error.to_string()),
                }
            },
            None => (),
        }
    }

    fn document_highlight(&mut self, params: TextDocumentPositionParams)
        -> BoxSendFuture<Option<Vec<DocumentHighlight>>, ErrorResponse<()>>
    {
        match self.expr {
            Some(ref expr) => {
                future::ok(Some(vec![DocumentHighlight {
                    range: expr.span_of_expr_or_pat_at_position(params.position).into(),
                    kind: DocumentHighlightKind::Text,
                }])).into_send_boxed()
            },
            None => {
                future::ok(None).into_send_boxed()
            },
        }
    }
}

