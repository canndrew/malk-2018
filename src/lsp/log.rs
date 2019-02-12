use super::*;

use std::sync::Mutex;
use std::fs::File;
use ::log::{Log, Record, Level, Metadata, SetLoggerError, LevelFilter};

struct FileLogger {
    file: Option<Mutex<File>>,
}

/*
impl Drop for FileLogger {
    fn drop(&mut self) {
        if let Some(ref file) = self.file {
            if let Ok(mut file) = file.lock() {
                let _ = file.flush();
            }
        }
    }
}
*/

lazy_static! {
    static ref LOGGER: FileLogger = FileLogger {
        file: File::create("/home/shum/malk-lsp.log").ok().map(Mutex::new),
    };
}

impl Log for FileLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.file.is_some() && metadata.target().starts_with("malk")
    }

    fn log(&self, record: &Record) {
        if !record.target().starts_with("malk") {
            return;
        }

        if let Some(ref file) = self.file {
            let mut file = unwrap!(file.lock());
            let _ = writeln!(file, "{} - {}", record.level(), record.args());
            let _ = file.flush();
        }
    }

    fn flush(&self) {}
}

pub fn init() -> Result<(), SetLoggerError> {
    ::log::set_logger(&*LOGGER)?;
    ::log::set_max_level(LevelFilter::Trace);
    Ok(())
}

