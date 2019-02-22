use super::*;

use lsp_types::{request::Request, notification::Notification};

pub struct LspClient {
    pub(crate) next_request_id: AtomicU64,
    pub(crate) client_requests_tx: UnboundedSender<(RequestMessage, oneshot::Sender<Result<Value, ResponseError>>)>,
    pub(crate) client_notifications_tx: UnboundedSender<NotificationMessage>,
}

impl LspClient {
    fn send_notification<N>(&self, params: N::Params)
    where
        N: Notification,
        N::Params: Serialize,
    {
        let notification_msg = NotificationMessage {
            method: String::from(N::METHOD),
            params: json!(params),
        };
        self.client_notifications_tx.unbounded_send(notification_msg);
    }

    fn send_request<R>(&self, params: R::Params)
        -> BoxSendFuture<R::Result, ResponseError>
    where
        R: Request,
        R::Params: Serialize,
        R::Result: DeserializeOwned + Send + 'static,
    {
        let next_request_id = self.next_request_id.fetch_add(1, atomic::Ordering::Relaxed);
        let request_msg = RequestMessage {
            id: next_request_id,
            method: String::from(R::METHOD),
            params: json!(params),
        };
        let (response_tx, response_rx) = oneshot::channel();
        self.client_requests_tx.unbounded_send((request_msg, response_tx));

        response_rx
        .map_err(|_| panic!("client side destroyed response sender"))
        .and_then(|res| match res {
            Ok(result) => unwrap!(serde_json::from_value(result)),
            Err(err) => Err(err),
        })
        .into_send_boxed()
    }

    pub fn show_message(&self, typ: MessageType, message: String) {
        self.send_notification::<ShowMessage>(ShowMessageParams { typ, message });
    }

    pub fn publish_diagnostics(&self, uri: Url, diagnostics: Vec<Diagnostic>) {
        self.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams { uri, diagnostics });
    }

    pub fn apply_edit(&self, edit: WorkspaceEdit)
        -> BoxSendFuture<bool, ResponseError>
    {
        self
        .send_request::<ApplyWorkspaceEdit>(ApplyWorkspaceEditParams { edit })
        .map(|response| response.applied)
        .into_send_boxed()
    }

    /*
    pub fn with_diagnostics<F>(&self, uri: Url, f: F)
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
                        client.publish_diagnostics(uri, diagnostics);
                    },
                    Err(error) => {
                        client.show_message(MessageType::Error, error.to_string());
                    },
                }
                Ok(())
            })
        );
    }
    */
}


