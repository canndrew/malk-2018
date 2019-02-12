#![feature(pattern)]
#![feature(never_type)]
#![feature(exhaustive_patterns)]
#![feature(underscore_imports)]
#![feature(try_from)]
#![feature(nll)]
#![feature(label_break_value)]

#![allow(unused_imports)] // workaround spurious warnings

use futures::{future, stream, Future, Stream, Sink, Async};
use futures::future::{Loop, Either};
use futures::sync::oneshot;
use failure::{Context, Error, Fail, bail};
use future_utils::{FutureExt as _, StreamExt as _, BoxSendFuture, mpsc};
use future_utils::mpsc::{UnboundedSender, UnboundedReceiver};
use canndrews_misc_ext_traits::VecBytesExt;
use maplit::hashmap;
use lazy_static::lazy_static;
use unwrap::unwrap;
use log::{debug, trace, warn, error};
use tokio::io::{AsyncRead, AsyncWrite};
use clap::{Arg, App, AppSettings, SubCommand};

use std::convert::TryFrom;
use std::borrow::Cow;
use std::{fmt, io, slice, fs};
use std::fmt::Display;
use std::io::{Read, Write, BufReader};
use std::str::FromStr;
use std::collections::HashMap;
use std::char;
use std::marker::PhantomData;
use std::path::Path;

use self::result_ext::ResultExt;
use self::future_ext::FutureExt;

mod lsp;
mod result_ext;
mod future_ext;
mod server;
mod lexer;
mod compile;
mod render;
mod parser;
mod wasm;

fn main() -> Result<(), Error> {
    let matches = {
        App::new("malk")
        .version("0.0.1")
        .author("canndrew")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand({
            SubCommand::with_name("mls")
            .about("run the malk language server")
        })
        .subcommand({
            SubCommand::with_name("run")
            .about("run a malk script")
            .arg({
                Arg::with_name("file")
                .required(true)
            })
        })
        .get_matches()
    };

    match unwrap!(matches.subcommand_name()) {
        "mls" => lsp::run(|client| server::Server::new(client)),
        "run" => {
            let run_matches = unwrap!(matches.subcommand_matches("run"));
            let filename = unwrap!(run_matches.value_of("file"));
            let code = match fs::read_to_string(filename) {
                Ok(code) => code,
                Err(e) => bail!("error opening file: {}", e),
            };
            compile::run(&code)?;

            Ok(())
        },
        _ => unreachable!(),
    }
}

