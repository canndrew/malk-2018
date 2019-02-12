use super::*;

use std::os::unix::io::{AsRawFd, RawFd};
use tokio::reactor::PollEvented2;

pub struct Stdin {
    inner: PollEvented2<AsyncFd>,
}

impl Stdin {
    pub fn new() -> io::Result<Stdin> {
        let inner = PollEvented2::new(AsyncFd::new(0)?);
        Ok(Stdin {
            inner,
        })
    }
}

pub struct Stdout {
    inner: PollEvented2<AsyncFd>,
}

impl Stdout {
    pub fn new() -> io::Result<Stdout> {
        let inner = PollEvented2::new(AsyncFd::new(1)?);
        Ok(Stdout {
            inner,
        })
    }
}

impl AsRawFd for Stdin {
    fn as_raw_fd(&self) -> RawFd {
        0
    }
}

impl Read for Stdin {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buffer)
    }
}

impl Write for Stdout {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.inner.write(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl AsyncRead for Stdin {}

impl AsyncWrite for Stdout {
    fn shutdown(&mut self) -> io::Result<Async<()>> {
        Ok(Async::Ready(()))
    }
}

impl AsRawFd for Stdout {
    fn as_raw_fd(&self) -> RawFd {
        1
    }
}

