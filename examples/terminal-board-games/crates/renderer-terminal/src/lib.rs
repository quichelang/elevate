use std::env;
use std::io::{self, Stdout, Write};

use game_core::{Frame, Renderer, Style};

pub struct TerminalRenderer {
    out: Stdout,
    width: usize,
    height: usize,
}

impl TerminalRenderer {
    pub fn new() -> io::Result<Self> {
        let mut out = io::stdout();
        write!(out, "\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l")?;
        out.flush()?;

        let (width, height) = terminal_size_from_env();
        Ok(Self { out, width, height })
    }
}

impl Drop for TerminalRenderer {
    fn drop(&mut self) {
        let _ = write!(self.out, "\x1b[0m\x1b[?25h\x1b[?1049l");
        let _ = self.out.flush();
    }
}

impl Renderer for TerminalRenderer {
    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    fn render(&mut self, frame: &Frame) -> io::Result<()> {
        write!(self.out, "\x1b[H")?;

        let mut current_style = Style::default();
        for y in 0..frame.height() {
            if let Some(row) = frame.row(y) {
                for cell in row {
                    if cell.style != current_style {
                        write!(self.out, "{}", style_escape(cell.style))?;
                        current_style = cell.style;
                    }
                    write!(self.out, "{}", cell.ch)?;
                }
            }
            if y + 1 < frame.height() {
                write!(self.out, "\r\n")?;
            }
        }

        if current_style != Style::default() {
            write!(self.out, "\x1b[0m")?;
        }
        self.out.flush()
    }
}

fn terminal_size_from_env() -> (usize, usize) {
    let width = env::var("COLUMNS")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .filter(|v| *v >= 40)
        .unwrap_or(100);
    let height = env::var("LINES")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .filter(|v| *v >= 20)
        .unwrap_or(36);
    (width, height)
}

fn style_escape(style: Style) -> &'static str {
    match (style.bold, style.inverted) {
        (false, false) => "\x1b[0m",
        (true, false) => "\x1b[1m",
        (false, true) => "\x1b[7m",
        (true, true) => "\x1b[1;7m",
    }
}
