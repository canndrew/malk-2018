use super::*;

use std::panic;
use std::any::Any;

use serde_json::json;

pub trait LspServer {
    fn initialize(&mut self, initialize_params: InitializeParams)
        -> BoxSendFuture<InitializeResult, ResponseError>;
    fn initialized(&mut self);
    fn did_open_text_document(&mut self, params: DidOpenTextDocumentParams);
    fn did_change_text_document(&mut self, params: DidChangeTextDocumentParams);
    fn document_highlight(&mut self, params: TextDocumentPositionParams)
        -> BoxSendFuture<Option<Vec<DocumentHighlight>>, ResponseError>;
    fn execute_command(&mut self, params: ExecuteCommandParams)
        -> BoxSendFuture<Option<Value>, ResponseError>;
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

    //std::thread::sleep(std::time::Duration::from_secs(10));

    let stdin = Stdin::new()?;
    let stdout = Stdout::new()?;

    let stdin = BufReader::new(stdin);
    
    log::init()?;

    let (client_requests_tx, client_requests_rx) = mpsc::unbounded();
    let (client_notifications_tx, client_notifications_rx) = mpsc::unbounded();
    
    let client = LspClient {
        next_request_id: AtomicU64::new(0),
        client_requests_tx,
        client_notifications_tx,
    };
    let mut server = f(client);

    let mut result_senders: HashMap<u64, oneshot::Sender<Result<Value, ResponseError>>> = HashMap::new();
    //let stdin = BufReader::new(tokio::io::stdin());
    //let stdout = tokio::io::stdout();

    panic::set_hook(Box::new(|_panic_info| {
        let message = format!("{:?}", backtrace::Backtrace::new());
        *unwrap!(PANIC_INFO.lock()) = Some(message);
    }));

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

    let msgs_from_client = {
        unparsed_msgs_from_client
        .and_then(|msg: String| {
            let json: Value = {
                serde_json::from_str(&msg)
                .compat_context("invalid json received on stdin")?
            };

            Message::from_json(json)
        })
    };

    let msgs_to_client = {
        msgs_from_client
        .map(Either::A)
        .select(client_requests_rx.map(Either::B).infallible())
        //.and_then(|either| match either {
        .and_then(move |either| match either {
            Either::A(message_from_client) => match message_from_client {
                Message::Notification(notification) => {
                    let params = notification.params;
                    match &notification.method[..] {
                        Initialized::METHOD => {
                            do_notification::<Initialized, _, _>(|server, _| server.initialized(), &mut server, params)
                        },
                        DidOpenTextDocument::METHOD => {
                            do_notification::<DidOpenTextDocument, _, _>(S::did_open_text_document, &mut server, params)
                        },
                        DidChangeTextDocument::METHOD => {
                            do_notification::<DidChangeTextDocument, _, _>(S::did_change_text_document, &mut server, params)
                        },
                        _ => {
                            trace!("unrecognised notification method '{}'", notification.method);
                            future::ok(None).into_send_boxed()
                        },
                    }
                },
                Message::Request(request) => {
                    debug!("Got a request yo!: {}", request.method);
                    let id = request.id;
                    let params = request.params;
                    match &request.method[..] {
                        Initialize::METHOD => {
                            do_request::<Initialize, _, _>(S::initialize, &mut server, id, params)
                        },
                        DocumentHighlightRequest::METHOD => {
                            do_request::<DocumentHighlightRequest, _, _>(S::document_highlight, &mut server, id, params)
                        },
                        ExecuteCommand::METHOD => {
                            do_request::<ExecuteCommand, _, _>(S::execute_command, &mut server, id, params)
                        },
                        _ => {
                            trace!("unrecognised request method '{}'", request.method);
                            future::ok(None).into_send_boxed()
                        }
                    }
                },
                Message::Response(response) => {
                    let id = response.id;
                    let result_sender = match result_senders.remove(&id) {
                        Some(result_sender) => result_sender,
                        None => future_bail!("invalid response id"),
                    };
                    let _ = result_sender.send(response.response);
                    future::ok(None).into_send_boxed()
                },
            },
            Either::B((request_to_client, result_sender)) => {
                result_senders.insert(request_to_client.id, result_sender);
                future::ok(Some(Message::Request(request_to_client))).into_send_boxed()
            },
        })
        .filter_map(|opt| opt)
        .select(client_notifications_rx.map(Message::Notification).infallible())
    };

    let print_message = |stdout, message| {
        let json = Message::to_json(message);
        let s = json.to_string();
        trace!("dispatching message to client: {}", s);

        let output = format!("Content-Length: {}\r\n\r\n{}", s.len(), s);
        let output = output.into_bytes();
        tokio::io::write_all(stdout, output)
        .compat_context("error writing to stdout")
        .map(|(stdout, _output)| stdout)
        .and_then(|stdout| tokio::io::flush(stdout).compat_context("error flushing stdout"))
    };

    let fut = {
        msgs_to_client
        .map(Ok)
        .fold(stdout, move |stdout, message_res| match message_res {
            Ok(message) => print_message(stdout, message).into_send_boxed(),
            Err(e) => {
                let message = Message::Notification(NotificationMessage {
                    method: String::from("window/showMessage"),
                    params: json!(ShowMessageParams {
                        typ: MessageType::Error,
                        message: format!("malk server aborting: {}", e),
                    }),
                });
                print_message(stdout, message)
                .and_then(|_stdout| Err(e))
                .into_send_boxed()
            },
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

    let res = runtime.block_on(fut);
    trace!("exiting normally: {:?}", res);
    res
}

fn do_request<R, F, S>(f: F, server: &mut S, id: u64, params: Value) -> BoxSendFuture<Option<Message>, Error>
where
    R: Request,
    R::Params: DeserializeOwned,
    R::Result: Serialize + 'static,
    F: FnOnce(&mut S, R::Params) -> BoxSendFuture<R::Result, ResponseError>,
    S: LspServer + Send + 'static,
{
    let params = match serde_json::from_value(params) {
        Ok(params) => params,
        Err(e) => future_bail!("error parsing params: {}", e), 
    };
    panic::AssertUnwindSafe(f(server, params))
    .catch_unwind()
    .then(move |res| match res {
        Ok(res) => {
            let response = match res {
                Ok(result) => Ok(unwrap!(serde_json::to_value(result))),
                Err(err) => Err(err),
            };
            Ok(Some(Message::Response(ResponseMessage {
                id: id,
                response: response,
            })))
        },
        Err(_e) => Err(get_panic_info()),
    })
    .into_send_boxed()
}

fn do_notification<N, F, S>(f: F, server: &mut S, params: Value) -> BoxSendFuture<Option<Message>, Error>
where
    N: Notification,
    N::Params: DeserializeOwned + panic::UnwindSafe,
    F: FnOnce(&mut S, N::Params) + panic::UnwindSafe,
    S: LspServer + Send + 'static,
{
    let params = match serde_json::from_value(params) {
        Ok(params) => params,
        Err(e) => future_bail!("error parsing params: {}", e), 
    };
    let server = panic::AssertUnwindSafe(server);
    let res = std::panic::catch_unwind(|| {
        let panic::AssertUnwindSafe(server) = server;
        f(server, params);
    });
    match res {
        Ok(()) => future::ok(None).into_send_boxed(),
        Err(_e) => future::err(get_panic_info()).into_send_boxed(),
    }
}

lazy_static! {
    static ref PANIC_INFO: Mutex<Option<String>> = Mutex::new(None);
}

fn get_panic_info() -> Error {
    let message = match unwrap!(PANIC_INFO.lock()).take() {
        Some(panic_info) => panic_info,
        None => String::from("(no stack trace available)"),
    };
    Error::from(failure::err_msg(message))
}

