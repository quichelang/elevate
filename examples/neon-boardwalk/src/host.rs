use std::io::{self, Read, Write};
use std::process::Command;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

#[derive(Debug)]
struct TerminalState {
    _raw: RawModeGuard,
    rx: Receiver<KeyEvent>,
    _reader_handle: JoinHandle<()>,
}

#[derive(Debug, Default)]
struct TerminalRegistry {
    next_id: i64,
    terminals: std::collections::HashMap<i64, TerminalState>,
}

static TERMINALS: OnceLock<Mutex<TerminalRegistry>> = OnceLock::new();

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyEvent {
    Up,
    Down,
    Left,
    Right,
    Check,
    Next,
    Reset,
    ToggleRenderer,
    Hint,
    Save,
    Load,
    Clear,
    Digit(i64),
    Quit,
}

pub fn runtime_start() -> i64 {
    let mut out = io::stdout();
    let _ = write!(out, "\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l");
    let _ = out.flush();

    let raw = RawModeGuard::new().expect("failed to enter raw mode");
    let (tx, rx) = mpsc::channel::<KeyEvent>();

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

    let mut registry = terminals().lock().expect("terminal registry mutex poisoned");
    registry.next_id += 1;
    let id = registry.next_id;
    registry.terminals.insert(
        id,
        TerminalState {
            _raw: raw,
            rx,
            _reader_handle: handle,
        },
    );
    id
}

pub fn runtime_poll_key(handle: i64) -> Option<KeyEvent> {
    with_terminal_mut(
        handle,
        |terminal| match terminal.rx.recv_timeout(Duration::from_millis(33)) {
            Ok(key) => Some(key),
            Err(RecvTimeoutError::Timeout) => None,
            Err(RecvTimeoutError::Disconnected) => Some(KeyEvent::Quit),
        },
    )
    .flatten()
}

pub fn runtime_shutdown(handle: i64) {
    let mut registry = terminals().lock().expect("terminal registry mutex poisoned");
    registry.terminals.remove(&handle);

    let mut out = io::stdout();
    let _ = write!(out, "\x1b[0m\x1b[?25h\x1b[?1049l");
    let _ = out.flush();
}

pub fn runtime_draw(frame: String) {
    let mut out = io::stdout();
    let _ = write!(out, "{frame}");
    let _ = out.flush();
}

fn decode_key(first: u8, lock: &mut dyn Read) -> Option<KeyEvent> {
    if first == 0x03 {
        return Some(KeyEvent::Quit);
    }
    if first == 0x7f {
        return Some(KeyEvent::Clear);
    }
    if first == 0x1b {
        let mut second = [0u8; 1];
        let mut third = [0u8; 1];

        if lock.read(&mut second).ok()? == 1
            && second[0] == b'['
            && lock.read(&mut third).ok()? == 1
        {
            return match third[0] {
                b'A' => Some(KeyEvent::Up),
                b'B' => Some(KeyEvent::Down),
                b'C' => Some(KeyEvent::Right),
                b'D' => Some(KeyEvent::Left),
                _ => Some(KeyEvent::Quit),
            };
        }
        return Some(KeyEvent::Quit);
    }

    if first.is_ascii_digit() {
        if first == b'0' {
            return Some(KeyEvent::Clear);
        }
        return Some(KeyEvent::Digit(i64::from(first - b'0')));
    }

    match first as char {
        'w' | 'k' => Some(KeyEvent::Up),
        's' | 'j' => Some(KeyEvent::Down),
        'a' | 'h' => Some(KeyEvent::Left),
        'd' | 'l' => Some(KeyEvent::Right),
        ' ' => Some(KeyEvent::Clear),
        'c' => Some(KeyEvent::Check),
        'n' => Some(KeyEvent::Next),
        'r' => Some(KeyEvent::Reset),
        'v' => Some(KeyEvent::ToggleRenderer),
        'q' => Some(KeyEvent::Quit),
        'H' => Some(KeyEvent::Hint),
        'p' => Some(KeyEvent::Save),
        'o' => Some(KeyEvent::Load),
        _ => None,
    }
}

#[derive(Debug)]
struct RawModeGuard {
    original_state: String,
}

impl RawModeGuard {
    fn new() -> io::Result<Self> {
        let original = run_stty(&["-f", "/dev/tty", "-g"])?;
        run_stty(&["-f", "/dev/tty", "-echo", "-icanon", "min", "0", "time", "1"])?;
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

fn with_terminal_mut<T>(handle: i64, f: impl FnOnce(&mut TerminalState) -> T) -> Option<T> {
    let mut registry = terminals().lock().expect("terminal registry mutex poisoned");
    let terminal = registry.terminals.get_mut(&handle)?;
    Some(f(terminal))
}

fn terminals() -> &'static Mutex<TerminalRegistry> {
    TERMINALS.get_or_init(|| Mutex::new(TerminalRegistry::default()))
}
