use super::*;

use std::sync::Mutex;
use std::net::TcpStream;
use ::log::{Log, Record, Level, Metadata, SetLoggerError, LevelFilter};

struct TcpLogger {
    stream: Option<Mutex<TcpStream>>,
}

lazy_static! {
    static ref LOGGER: TcpLogger = TcpLogger {
        stream: TcpStream::connect("0.0.0.0:45666").ok().map(Mutex::new),
    };
}

impl Log for TcpLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.stream.is_some() && metadata.target().starts_with("malk")
    }

    fn log(&self, record: &Record) {
        if !record.target().starts_with("malk") {
            return;
        }

        if let Some(ref stream) = self.stream {
            let mut stream = unwrap!(stream.lock());
            let _ = writeln!(stream, "{} - {}", record.level(), record.args());
            let _ = stream.flush();
        }
    }

    fn flush(&self) {}
}

pub fn init() -> Result<(), SetLoggerError> {
    ::log::set_logger(&*LOGGER)?;
    ::log::set_max_level(LevelFilter::Trace);
    Ok(())
}

