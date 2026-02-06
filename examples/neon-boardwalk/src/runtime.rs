use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::process::Command;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

const PUZZLES: [&str; 3] = [
    "530070000600195000098000060800060003400803001700020006060000280000419005000080079",
    "009000000080605020501078000000000700706040102004000000000720903090301080000000600",
    "000260701680070090190004500820100040004602900050003028009300074040050036703018000",
];

#[derive(Debug, Clone)]
struct BoardState {
    cells: [u8; 81],
    fixed: [bool; 81],
}

#[derive(Debug, Default)]
struct BoardRegistry {
    next_id: i64,
    boards: HashMap<i64, BoardState>,
}

static BOARDS: OnceLock<Mutex<BoardRegistry>> = OnceLock::new();

#[derive(Debug)]
struct TerminalState {
    _raw: RawModeGuard,
    rx: Receiver<KeyEvent>,
    _reader_handle: JoinHandle<()>,
}

#[derive(Debug, Default)]
struct TerminalRegistry {
    next_id: i64,
    terminals: HashMap<i64, TerminalState>,
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
    Hint,
    Save,
    Load,
    Clear,
    Digit(i64),
    Quit,
}

pub fn puzzle_count() -> i64 {
    PUZZLES.len() as i64
}

pub fn board_from_puzzle(index: i64) -> i64 {
    let idx = normalize_puzzle_index(index);
    let board = board_from_text(PUZZLES[idx]);

    let mut registry = boards().lock().expect("board registry mutex poisoned");
    registry.next_id += 1;
    let id = registry.next_id;
    registry.boards.insert(id, board);
    id
}

pub fn board_get(handle: i64, row: i64, col: i64) -> i64 {
    with_board(handle, |board| {
        let Some(idx) = board_index(row, col) else {
            return 0;
        };
        i64::from(board.cells[idx])
    })
    .unwrap_or(0)
}

pub fn board_is_fixed(handle: i64, row: i64, col: i64) -> bool {
    with_board(handle, |board| {
        let Some(idx) = board_index(row, col) else {
            return false;
        };
        board.fixed[idx]
    })
    .unwrap_or(false)
}

pub fn board_set(handle: i64, row: i64, col: i64, value: i64) -> bool {
    with_board_mut(handle, |board| {
        let Some(idx) = board_index(row, col) else {
            return false;
        };
        if board.fixed[idx] {
            return false;
        }
        if !(1..=9).contains(&value) {
            return false;
        }
        board.cells[idx] = value as u8;
        true
    })
    .unwrap_or(false)
}

pub fn board_clear(handle: i64, row: i64, col: i64) -> bool {
    with_board_mut(handle, |board| {
        let Some(idx) = board_index(row, col) else {
            return false;
        };
        if board.fixed[idx] {
            return false;
        }
        board.cells[idx] = 0;
        true
    })
    .unwrap_or(false)
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
    with_terminal_mut(handle, |terminal| match terminal.rx.recv_timeout(Duration::from_millis(33)) {
        Ok(key) => Some(key),
        Err(RecvTimeoutError::Timeout) => None,
        Err(RecvTimeoutError::Disconnected) => Some(KeyEvent::Quit),
    })
    .flatten()
}

pub fn runtime_shutdown(handle: i64) {
    let mut registry = terminals().lock().expect("terminal registry mutex poisoned");
    registry.terminals.remove(&handle);

    let mut out = io::stdout();
    let _ = write!(out, "\x1b[0m\x1b[?25h\x1b[?1049l");
    let _ = out.flush();
}

pub fn runtime_render(board_handle: i64, cursor_row: i64, cursor_col: i64, message: String) {
    let mut out = io::stdout();
    let _ = write!(out, "\x1b[H");

    let _ = writeln!(out, "Neon Boardwalk :: Sudoku");
    let _ = writeln!(out, "");

    for row in 0..9 {
        if row % 3 == 0 {
            let _ = writeln!(out, "+-------+-------+-------+");
        }

        let _ = write!(out, "|");
        for col in 0..9 {
            let value = board_get(board_handle, row, col);
            let ch = if value == 0 {
                '.'
            } else {
                char::from_digit(value as u32, 10).unwrap_or('?')
            };

            let is_cursor = row == cursor_row && col == cursor_col;
            let is_fixed = board_is_fixed(board_handle, row, col);

            if is_cursor {
                let _ = write!(out, "\x1b[7m");
            } else if is_fixed {
                let _ = write!(out, "\x1b[1m");
            }

            let _ = write!(out, " {}", ch);
            let _ = write!(out, "\x1b[0m");

            if col % 3 == 2 {
                let _ = write!(out, " |",);
            }
        }
        let _ = writeln!(out);
    }
    let _ = writeln!(out, "+-------+-------+-------+");
    let _ = writeln!(out);
    let _ = writeln!(out, "Controls: arrows/WASD move | 1-9 set | 0/space/backspace clear");
    let _ = writeln!(out, "          c check | n next | r reset | q quit");
    let _ = writeln!(out, "Status: {message}");

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

fn normalize_puzzle_index(index: i64) -> usize {
    let len = PUZZLES.len() as i64;
    if len == 0 {
        return 0;
    }
    let mut idx = index % len;
    if idx < 0 {
        idx += len;
    }
    idx as usize
}

fn board_from_text(puzzle: &str) -> BoardState {
    let mut cells = [0u8; 81];
    let mut fixed = [false; 81];

    for (idx, ch) in puzzle.chars().take(81).enumerate() {
        let value = ch.to_digit(10).unwrap_or(0) as u8;
        cells[idx] = value;
        fixed[idx] = value != 0;
    }

    BoardState { cells, fixed }
}

fn board_index(row: i64, col: i64) -> Option<usize> {
    if !(0..=8).contains(&row) || !(0..=8).contains(&col) {
        return None;
    }
    Some((row as usize) * 9 + (col as usize))
}

fn with_board<T>(handle: i64, f: impl FnOnce(&BoardState) -> T) -> Option<T> {
    let registry = boards().lock().expect("board registry mutex poisoned");
    let board = registry.boards.get(&handle)?;
    Some(f(board))
}

fn with_board_mut<T>(handle: i64, f: impl FnOnce(&mut BoardState) -> T) -> Option<T> {
    let mut registry = boards().lock().expect("board registry mutex poisoned");
    let board = registry.boards.get_mut(&handle)?;
    Some(f(board))
}

fn boards() -> &'static Mutex<BoardRegistry> {
    BOARDS.get_or_init(|| Mutex::new(BoardRegistry::default()))
}

fn with_terminal_mut<T>(handle: i64, f: impl FnOnce(&mut TerminalState) -> T) -> Option<T> {
    let mut registry = terminals().lock().expect("terminal registry mutex poisoned");
    let terminal = registry.terminals.get_mut(&handle)?;
    Some(f(terminal))
}

fn terminals() -> &'static Mutex<TerminalRegistry> {
    TERMINALS.get_or_init(|| Mutex::new(TerminalRegistry::default()))
}
