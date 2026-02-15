use std::io::{self, Read, Write};
#[cfg(not(windows))]
use std::process::Command;

#[cfg(windows)]
use windows_sys::Win32::Foundation::INVALID_HANDLE_VALUE;
#[cfg(windows)]
use windows_sys::Win32::System::Console::{
    CONSOLE_SCREEN_BUFFER_INFO, ENABLE_ECHO_INPUT, ENABLE_LINE_INPUT, ENABLE_PROCESSED_INPUT,
    ENABLE_VIRTUAL_TERMINAL_PROCESSING, GetConsoleMode, GetConsoleScreenBufferInfo, GetStdHandle,
    STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, SetConsoleMode,
};

/// Enter raw mode: disable echo/canonical, switch to alternate screen, hide cursor.
pub fn term_init() -> String {
    #[cfg(windows)]
    {
        let saved = save_windows_console_modes().unwrap_or_default();
        let mut out = io::stdout();
        let _ = write!(out, "\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l");
        let _ = out.flush();
        return saved;
    }

    #[cfg(not(windows))]
    {
        let saved = run_stty(&["-g"]).unwrap_or_default();
        let _ = run_stty(&["-echo", "-icanon", "min", "1", "time", "0"]);
        let mut out = io::stdout();
        let _ = write!(out, "\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l");
        let _ = out.flush();
        return saved.trim().to_string();
    }
}

/// Restore terminal state.
pub fn term_cleanup(saved: &str) {
    let mut out = io::stdout();
    let _ = write!(out, "\x1b[?25h\x1b[?1049l");
    let _ = out.flush();

    #[cfg(windows)]
    {
        let _ = restore_windows_console_modes(saved);
        return;
    }

    #[cfg(not(windows))]
    {
        if !saved.trim().is_empty() {
            let _ = run_stty(&[saved]);
        }
    }
}

/// Read a single key event. Returns a tag string:
/// "up", "down", "left", "right", "escape", "enter", "backspace", "char:x"
pub fn term_read_key() -> String {
    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut byte = [0u8; 1];

    match lock.read(&mut byte) {
        Ok(0) | Err(_) => return String::new(),
        Ok(_) => {}
    }

    let ch = byte[0];

    #[cfg(windows)]
    {
        if ch == 0x00 || ch == 0xE0 {
            let mut ext = [0u8; 1];
            if lock.read(&mut ext).unwrap_or(0) == 1 {
                return match ext[0] {
                    b'H' => "up".to_string(),
                    b'P' => "down".to_string(),
                    b'M' => "right".to_string(),
                    b'K' => "left".to_string(),
                    _ => String::new(),
                };
            }
            return String::new();
        }
    }

    // Escape sequence
    if ch == 0x1b {
        let mut seq = [0u8; 2];
        if lock.read(&mut seq).unwrap_or(0) == 2 && seq[0] == b'[' {
            return match seq[1] {
                b'A' => "up".to_string(),
                b'B' => "down".to_string(),
                b'C' => "right".to_string(),
                b'D' => "left".to_string(),
                _ => "escape".to_string(),
            };
        }
        return "escape".to_string();
    }

    // Ctrl-C
    if ch == 0x03 {
        return "escape".to_string();
    }

    // Backspace
    if ch == 0x7f || ch == 0x08 {
        return "backspace".to_string();
    }

    // Enter
    if ch == 0x0d || ch == 0x0a {
        return "enter".to_string();
    }

    // Printable ASCII
    if (0x20..0x7f).contains(&ch) {
        return format!("char:{}", ch as char);
    }

    String::new()
}

/// Get terminal height.
pub fn term_height() -> i64 {
    if let Some((_, rows)) = term_size() {
        return rows as i64;
    }
    24
}

/// Write a frame string to stdout.
pub fn term_draw(frame: &str) {
    let mut out = io::stdout();
    let _ = out.write_all(frame.as_bytes());
    let _ = out.flush();
}

/// Recursively scan a directory for file paths (relative to root).
pub fn scan_dir(root: &str) -> Vec<String> {
    let mut files = Vec::new();
    collect_files(root, root, &mut files);
    files.sort();
    files
}

fn collect_files(base: &str, dir: &str, out: &mut Vec<String>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<_> = entries.filter_map(|e| e.ok()).collect();
    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') || name == "node_modules" || name == "target" {
            continue;
        }
        let path = entry.path();
        if path.is_dir() {
            collect_files(base, path.to_str().unwrap_or(""), out);
        } else if let Ok(rel) = path.strip_prefix(base) {
            out.push(rel.to_string_lossy().to_string());
        }
    }
}

/// Read script events from a file, one per line.
pub fn read_script_events(path: &str) -> Vec<String> {
    let Ok(content) = std::fs::read_to_string(path) else {
        return Vec::new();
    };
    content
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .collect()
}

/// Get command-line arguments (skipping the program name).
pub fn get_args() -> Vec<String> {
    std::env::args().skip(1).collect()
}

/// Vec::len as i64 (Elevate integers are i64).
pub fn vec_len(v: &Vec<String>) -> i64 {
    v.len() as i64
}

/// Clone a Vec<String> — needed when .ers code needs to use the same
/// vector value in multiple consuming positions.
pub fn clone_vec(v: &Vec<String>) -> Vec<String> {
    v.clone()
}

/// String length as i64.
pub fn string_len(s: &str) -> i64 {
    s.len() as i64
}

/// If event starts with "char:", return the payload; otherwise return "".
pub fn extract_char_payload(event: &str) -> String {
    if let Some(rest) = event.strip_prefix("char:") {
        rest.to_string()
    } else {
        String::new()
    }
}

/// If event starts with "type:", return the payload; otherwise return "".
pub fn extract_type_payload(event: &str) -> String {
    if let Some(rest) = event.strip_prefix("type:") {
        rest.to_string()
    } else {
        String::new()
    }
}

/// Remove the last character from a string and return the new string.
pub fn string_drop_last(s: &str) -> String {
    let mut chars: Vec<char> = s.chars().collect();
    if !chars.is_empty() {
        chars.pop();
    }
    chars.into_iter().collect()
}

/// Print a line to stdout.
pub fn println(s: &str) {
    println!("{s}");
}

#[cfg(not(windows))]
fn run_stty(args: &[&str]) -> io::Result<String> {
    let mut prefixed_args = vec!["-f", "/dev/tty"];
    prefixed_args.extend_from_slice(args);

    for candidate in [prefixed_args.as_slice(), args] {
        let output = Command::new("stty").args(candidate).output();
        let Ok(output) = output else {
            continue;
        };
        if output.status.success() {
            return Ok(String::from_utf8_lossy(&output.stdout).to_string());
        }
    }

    Err(io::Error::other("stty failed"))
}

fn term_size() -> Option<(u16, u16)> {
    #[cfg(windows)]
    {
        unsafe {
            let out = GetStdHandle(STD_OUTPUT_HANDLE);
            if out.is_null() || (out as isize) == (INVALID_HANDLE_VALUE as isize) {
                return None;
            }
            let mut info: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
            if GetConsoleScreenBufferInfo(out, &mut info) == 0 {
                return None;
            }
            let cols = (info.srWindow.Right - info.srWindow.Left + 1) as u16;
            let rows = (info.srWindow.Bottom - info.srWindow.Top + 1) as u16;
            return Some((cols, rows));
        }
    }

    #[cfg(not(windows))]
    {
        let output = Command::new("stty")
            .args(["-f", "/dev/tty", "size"])
            .output()
            .ok()
            .or_else(|| Command::new("stty").arg("size").output().ok())?;
        let text = String::from_utf8_lossy(&output.stdout);
        let mut parts = text.trim().split_whitespace();
        let rows: u16 = parts.next()?.parse().ok()?;
        let cols: u16 = parts.next()?.parse().ok()?;
        return Some((cols, rows));
    }
}

#[cfg(windows)]
fn save_windows_console_modes() -> Option<String> {
    unsafe {
        let input = GetStdHandle(STD_INPUT_HANDLE);
        let output = GetStdHandle(STD_OUTPUT_HANDLE);
        if input.is_null()
            || output.is_null()
            || (input as isize) == (INVALID_HANDLE_VALUE as isize)
            || (output as isize) == (INVALID_HANDLE_VALUE as isize)
        {
            return None;
        }

        let mut in_mode = 0u32;
        let mut out_mode = 0u32;
        if GetConsoleMode(input, &mut in_mode) == 0 || GetConsoleMode(output, &mut out_mode) == 0 {
            return None;
        }

        let new_in_mode = (in_mode | ENABLE_PROCESSED_INPUT) & !(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
        let new_out_mode = out_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
        let _ = SetConsoleMode(input, new_in_mode);
        let _ = SetConsoleMode(output, new_out_mode);

        Some(format!("{in_mode}:{out_mode}"))
    }
}

#[cfg(windows)]
fn restore_windows_console_modes(saved: &str) -> io::Result<()> {
    let (in_raw, out_raw) = saved
        .split_once(':')
        .ok_or_else(|| io::Error::other("invalid saved console modes"))?;
    let in_mode = in_raw
        .parse::<u32>()
        .map_err(|_| io::Error::other("invalid saved input mode"))?;
    let out_mode = out_raw
        .parse::<u32>()
        .map_err(|_| io::Error::other("invalid saved output mode"))?;

    unsafe {
        let input = GetStdHandle(STD_INPUT_HANDLE);
        let output = GetStdHandle(STD_OUTPUT_HANDLE);
        if input.is_null()
            || output.is_null()
            || (input as isize) == (INVALID_HANDLE_VALUE as isize)
            || (output as isize) == (INVALID_HANDLE_VALUE as isize)
        {
            return Err(io::Error::other("invalid console handle"));
        }
        let _ = SetConsoleMode(input, in_mode);
        let _ = SetConsoleMode(output, out_mode);
    }

    Ok(())
}
