use super::*;

pub type ResultSender = oneshot::Sender<Result<ResponseMsg, ErrorMsg>>;

#[derive(Clone)]
pub struct LspClient {
    pub(crate) client_requests_tx: UnboundedSender<(u64, RequestMsg, ResultSender)>,
    pub(crate) client_notifications_tx: UnboundedSender<NotificationMsg>,
}

impl LspClient {
    pub fn show_message(&self, ty: MessageType, message: String) {
        let _ = {
            self
            .client_notifications_tx
            .unbounded_send(NotificationMsg::WindowShowMessage(ty, message))
        };
        trace!("sent message on client_tx");
    }

    pub fn text_document_publish_diagnostics(&self, uri: DocumentUri, diagnostics: Vec<Diagnostic>) {
        let msg = PublishDiagnosticsParams {
            uri, diagnostics,
        };
        let _ = {
            self
            .client_notifications_tx
            .unbounded_send(NotificationMsg::TextDocumentPublishDiagnostics(msg))
        };
    }

    pub fn with_diagnostics<F>(&self, uri: DocumentUri, f: F)
    where
        F: Future<Item = Vec<Diagnostic>, Error = Error> + Send + 'static,
    {
        let client = self.clone();
        tokio::spawn(
            f
            .then(move |result| {
                match result {
                    Ok(diagnostics) => {
                        debug!("sending {} diagnostics", diagnostics.len()); 
                        client.text_document_publish_diagnostics(uri, diagnostics);
                    },
                    Err(error) => {
                        client.show_message(MessageType::Error, error.to_string());
                    },
                }
                Ok(())
            })
        );
    }
}


