#![feature(pattern)]
#![feature(map_get_key_value)]
#![feature(never_type)]
#![feature(exhaustive_patterns)]
#![feature(nll)]
#![feature(label_break_value)]
#![feature(integer_atomics)]
#![feature(proc_macro_hygiene)]

#![allow(unused_imports)] // workaround spurious warnings

use futures::{future, stream, Future, Stream, Sink, Async};
use futures::future::{Loop, Either};
use futures::sync::oneshot;
use failure::{Context, Error, Fail, bail, format_err};
use future_utils::{FutureExt as _, StreamExt as _, BoxSendFuture, mpsc};
use future_utils::mpsc::{UnboundedSender, UnboundedReceiver};
use canndrews_misc_ext_traits::VecBytesExt;
use maplit::hashmap;
use lazy_static::lazy_static;
use unwrap::unwrap;
use log::{debug, trace, warn, error};
use tokio::io::{AsyncRead, AsyncWrite};
use clap::{Arg, App, AppSettings, SubCommand};
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use unicode_width::UnicodeWidthStr;

use std::hash::{Hash, Hasher};
use std::sync::{atomic, Mutex};
use std::sync::atomic::AtomicU64;
use std::convert::TryFrom;
use std::borrow::Cow;
use std::{fmt, io, slice, fs, str, mem};
use std::fmt::Display;
use std::io::{Read, Write, BufReader};
use std::str::FromStr;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::char;
use std::marker::PhantomData;
use std::path::Path;
use std::sync::Arc;
use std::rc::Rc;
use std::cmp;
use lsp_types::Url;

use self::result_ext::ResultExt;
use self::future_ext::FutureExt;
use self::lsp_types_ext::*;
use self::core::{Ctx, Type, TypeKind, Term, TermKind};
use self::interner::Interner;

/*
macro_rules! future_bail {
    ($($t:expr),*) => ({
        return futures::future::err(failure::format_err!($($t),*)).into_send_boxed();
    })
}
*/

//mod lsp;
mod result_ext;
mod future_ext;
mod lsp_types_ext;
//mod server;
//mod render;
//mod lexer;
//mod compile;
//mod render;
//pub mod core;
pub mod core;
pub mod typechecker;
pub mod syntax;
pub mod parser;
pub mod interner;
//mod wasm;

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
        //"mls" => lsp::run(|client| server::Server::new(client)),
        "run" => {
            let run_matches = unwrap!(matches.subcommand_matches("run"));
            let filename = unwrap!(run_matches.value_of("file"));
            unwrap!(run(filename));

            Ok(())
        },
        _ => unreachable!(),
    }
}

fn run(filename: &str) -> Result<(), Error> {
    let code = match fs::read_to_string(filename) {
        Ok(code) => code,
        Err(e) => bail!("error opening file: {}", e),
    };
    let uri = Arc::new(Url::parse(&format!("file://{}", filename))?);
    let term = unwrap!(parser::parse_doc(&uri, &code));
    let ht = typechecker::check_doc(&term);
    println!("ht == {:?}", ht);
    //let rendered = term.render("", "");
    //println!("rendered: {}", rendered);
    Ok(())
}

