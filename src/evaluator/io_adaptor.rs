use std::io::{self, Read, Stderr, Stdin, Stdout, Write};

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
        self.stdout.write(buf)
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
