use super::*;

pub trait LspServer {
    fn initialize(&mut self, initialize_params: InitializeParams)
        -> BoxSendFuture<InitializeResponse, ErrorResponse<InitializeError>>;
    fn initialized(&mut self);
    fn execute_command(&mut self) -> BoxSendFuture<(), ErrorResponse<()>>;
    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams);
    fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams);
    fn document_highlight(&mut self, params: TextDocumentPositionParams)
        -> BoxSendFuture<Option<Vec<DocumentHighlight>>, ErrorResponse<()>>;
}

pub fn run<F, S>(f: F) -> Result<(), Error>
where
    F: FnOnce(LspClient) -> S,
    S: LspServer,
    S: Send + 'static,
{
    /*
    let stdin = Stdin;
    let stdout = Stdout;

    set_nonblocking(&stdin);
    set_nonblocking(&stdout);
    */

    let stdin = Stdin::new()?;
    let stdout = Stdout::new()?;

    let stdin = BufReader::new(stdin);
    
    log::init()?;

    let (client_requests_tx, client_requests_rx) = mpsc::unbounded();
    let (client_notifications_tx, client_notifications_rx) = mpsc::unbounded();
    
    let client = LspClient {
        client_requests_tx,
        client_notifications_tx,
    };
    let mut server = f(client);

    let client_notifications = {
        client_notifications_rx
        .map(Msg::Notification)
    };

    let client_requests = {
        client_requests_rx
        .map(Either::B)
    };

    let mut result_senders: HashMap<u64, ResultSender> = HashMap::new();
    //let stdin = BufReader::new(tokio::io::stdin());
    //let stdout = tokio::io::stdout();

    trace!("starting lsp server");
    let unparsed_msgs_from_client = stream::unfold(stdin, |stdin| {
        Some(future::loop_fn((None::<usize>, stdin), move |(content_length_opt, stdin)| {
            trace!("reading header line from stdin");
            tokio::io::read_until(stdin, '\n' as u8, Vec::new())
            .compat_context("error reading header from stdin")
            .and_then(move |(stdin, line)| {
                let line = {
                    String::from_utf8(line)
                    .compat_context("invalid utf8 in header")?
                };
                let seek = "Content-Length:";
                if line.starts_with(seek) {
                    let content_length_str = line[seek.len()..].trim();
                    let content_length = {
                        usize::from_str(content_length_str)
                        .compat_context("invalid content length")?
                    };
                    if content_length_opt.is_some() {
                        bail!("content length header repeated");
                    }
                    return Ok(Loop::Continue((Some(content_length), stdin)));
                } else if line.trim() == "" {
                    let content_length = match content_length_opt {
                        Some(content_length) => content_length,
                        None => bail!("missing content length header"),
                    };
                    return Ok(Loop::Break((content_length, stdin)));
                }
                Ok(Loop::Continue((content_length_opt, stdin)))
            })
        })
        .and_then(|(content_length, stdin)| {
            let content = Vec::zeros(content_length);
            tokio::io::read_exact(stdin, content)
            .compat_context("error reading header from stdin")
            .and_then(|(stdin, content)| {
                let content = {
                    String::from_utf8(content)
                    .compat_context("invalid utf8 in received json")?
                };
                trace!("from client: {}", content);
                Ok((content, stdin))
            })
        }))
    });

    let msgs_to_client = {
        unparsed_msgs_from_client
        .and_then(|msg: String| {
            let json: Value = {
                serde_json::from_str(&msg)
                .compat_context("invalid json received on stdin")?
            };

            Ok(Either::A(Msg::from_json(json)?))
        })
        .select(client_requests.infallible())
        .and_then(move |either| {
            trace!("got either or");
            match either {
                Either::A(msg) => match msg {
                    Msg::Request(id, request_msg) => {
                        Ok(Some(match request_msg {
                            RequestMsg::Initialize(params) => {
                                server
                                .initialize(params)
                                .map(|wow| {
                                    trace!("initialize returned");
                                    wow
                                })
                                .then(move |result| future::ok(match result {
                                    Ok(response) => 
                                        Msg::Response(id, ResponseMsg::Initialize(response)),
                                    Err(error) => 
                                        Msg::Error(id, error.to_error_msg()),
                                }))
                                .into_send_boxed()
                            },
                            RequestMsg::ExecuteCommand(params) => {
                                server
                                .execute_command()
                                .then(move |result| future::ok(match result {
                                    Ok(()) => 
                                        Msg::Response(id, ResponseMsg::ExecuteCommand),
                                    Err(error) =>
                                        Msg::Error(id, error.to_error_msg())
                                }))
                                .into_send_boxed()
                            },
                            RequestMsg::DocumentHighlight(params) => {
                                server
                                .document_highlight(params)
                                .then(move |result| future::ok(match result {
                                    Ok(response) => 
                                        Msg::Response(id, ResponseMsg::DocumentHighlight(response)),
                                    Err(error) =>
                                        Msg::Error(id, error.to_error_msg())
                                }))
                                .into_send_boxed()
                            },
                        }))
                    },
                    Msg::Response(id, response_msg) => {
                        let result_sender = match result_senders.remove(&id) {
                            Some(result_sender) => result_sender,
                            None => bail!("invalid response id"),
                        };
                        result_sender.send(Ok(response_msg));

                        Ok(None)
                    },
                    Msg::Error(id, error_msg) => {
                        let result_sender = match result_senders.remove(&id) {
                            Some(result_sender) => result_sender,
                            None => bail!("invalid response id"),
                        };
                        result_sender.send(Err(error_msg));

                        Ok(None)
                    },
                    Msg::Notification(notification_msg) => {
                        trace!("got notification");
                        match notification_msg {
                            NotificationMsg::Initialized => {
                                server.initialized();
                            },
                            NotificationMsg::TextDocumentDidOpen(params) => {
                                trace!("got text document did open");
                                server.text_document_did_open(params);
                            },
                            NotificationMsg::TextDocumentDidChange(params) => {
                                trace!("got text document did change");
                                server.text_document_did_change(params);
                            },
                            _ => {
                                warn!(
                                    "unrecognised notification from client: {}",
                                    notification_msg.method(),
                                );
                            },
                        }
                        Ok(None)
                    },
                },
                Either::B((id, request, result_sender)) => {
                    result_senders.insert(id, result_sender);

                    Ok(Some(future::ok(Msg::Request(id, request)).into_send_boxed()))
                },
            }
        })
        .filter_map(|opt| opt)
        .map(|wow| {
            trace!("got a future to wait on");
            wow
        })
        //.and_then(|f| f)
        .buffer_unordered(1024)
        .map(|wow| {
            trace!("got a message to print");
            wow
        })
        .select(client_notifications.infallible())
    };

    let fut = {
        msgs_to_client
        .fold(stdout, |stdout, msg| {
            let json = msg.to_json();
            let s = json.to_string();
            trace!("dispatching message to client: {}", s);

            let output = format!("Content-Length: {}\r\n\r\n{}", s.len(), s);
            let output = output.into_bytes();
            tokio::io::write_all(stdout, output)
            .compat_context("error writing to stdout")
            .map(|(stdout, _output)| stdout)
            .map(|wow| {
                trace!("wrote to stdout");
                wow
            })
            .and_then(|stdout| tokio::io::flush(stdout).compat_context("error flushing stdout"))
        })
        .map(|_stdout| ())
        .map_err(|err| {
            error!("lsp server exiting with error: {}", err);
            err
        })
    };

    let mut runtime = {
        tokio::runtime::Runtime::new()
        .compat_context("failed to start tokio runtime")?
    };

    runtime.block_on(fut)
}

