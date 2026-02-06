use std::io::{self, Read};
use std::process::Command;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

use game_core::{InputSource, Key};

pub struct MacOsInput {
    _raw: RawModeGuard,
    rx: Receiver<Key>,
    _reader_handle: JoinHandle<()>,
}

impl MacOsInput {
    pub fn new() -> io::Result<Self> {
        let raw = RawModeGuard::new()?;
        let (tx, rx) = mpsc::channel();

        let handle = thread::spawn(move || {
            let stdin = io::stdin();
            let mut lock = stdin.lock();
            let mut byte = [0u8; 1];

            loop {
                match lock.read(&mut byte) {
                    Ok(0) => continue,
                    Ok(_) => {
                        if let Some(key) = decode_key(byte[0], &mut lock) {
                            if tx.send(key).is_err() {
                                break;
                            }
                        }
                    }
                    Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                    Err(_) => break,
                }
            }
        });

        Ok(Self {
            _raw: raw,
            rx,
            _reader_handle: handle,
        })
    }
}

impl InputSource for MacOsInput {
    fn poll_key(&mut self, timeout: Duration) -> io::Result<Option<Key>> {
        match self.rx.recv_timeout(timeout) {
            Ok(key) => Ok(Some(key)),
            Err(RecvTimeoutError::Timeout) => Ok(None),
            Err(RecvTimeoutError::Disconnected) => Ok(None),
        }
    }
}

fn decode_key(first: u8, lock: &mut dyn Read) -> Option<Key> {
    if first == 0x03 {
        return Some(Key::CtrlC);
    }
    if first == b'\n' || first == b'\r' {
        return Some(Key::Enter);
    }
    if first == 0x7f {
        return Some(Key::Backspace);
    }
    if first == 0x1b {
        let mut second = [0u8; 1];
        let mut third = [0u8; 1];

        if lock.read(&mut second).ok()? == 1
            && second[0] == b'['
            && lock.read(&mut third).ok()? == 1
        {
            return match third[0] {
                b'A' => Some(Key::Up),
                b'B' => Some(Key::Down),
                b'C' => Some(Key::Right),
                b'D' => Some(Key::Left),
                _ => Some(Key::Escape),
            };
        }
        return Some(Key::Escape);
    }

    if first.is_ascii() {
        return Some(Key::Char(first as char));
    }
    None
}

struct RawModeGuard {
    original_state: String,
}

impl RawModeGuard {
    fn new() -> io::Result<Self> {
        let original = run_stty(&["-f", "/dev/tty", "-g"])?;
        run_stty(&[
            "-f", "/dev/tty", "-echo", "-icanon", "min", "0", "time", "1",
        ])?;
        Ok(Self {
            original_state: original.trim().to_string(),
        })
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        let _ = run_stty(&["-f", "/dev/tty", &self.original_state]);
    }
}

fn run_stty(args: &[&str]) -> io::Result<String> {
    let output = Command::new("stty").args(args).output()?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(io::Error::other(format!(
            "stty failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )))
    }
}
