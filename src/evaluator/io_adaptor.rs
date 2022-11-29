use std::{
    collections::VecDeque,
    io::{self, IoSlice, Read, Stderr, Stdin, Stdout, Write},
    str::from_utf8,
};

struct ChunkWriter(Vec<Box<[u8]>>);

pub struct TestingAdaptor {
    in_queue: VecDeque<u8>,
    out: ChunkWriter,
    err: ChunkWriter,
}

impl Read for TestingAdaptor {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.in_queue.read(buf)
    }
}
impl Write for TestingAdaptor {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.out.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        /*intentional blank */
        Ok(())
    }
}
impl Write for ChunkWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.push(buf.into());
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        /*intentional blank */
        Ok(())
    }
}

impl IoAdaptor for TestingAdaptor {
    fn err(&mut self) -> &mut dyn Write {
        &mut self.err
    }
}
pub struct StdIOAdaptor {
    stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
}
impl StdIOAdaptor {
    pub(crate) fn new() -> Self {
        StdIOAdaptor {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
        }
    }
}
impl Read for StdIOAdaptor {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.stdin.read(buf)
    }
}
impl Write for StdIOAdaptor {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let yellow = b"\x1B[33m";
        let reset = b"\x1B[0m";

        // eprintln!("[IO ADAPTER] buf: {final_buf:?}");
        self.stdout.write_all(&[yellow, buf, reset].concat())?;

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}
impl IoAdaptor for StdIOAdaptor {
    fn err(&mut self) -> &mut dyn Write {
        &mut self.stderr as &mut dyn Write
    }
}

pub trait IoAdaptor: Read + Write {
    fn err(&mut self) -> &mut dyn Write;
}
