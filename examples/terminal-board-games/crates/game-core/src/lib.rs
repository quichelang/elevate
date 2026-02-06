use std::io;
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key {
    Up,
    Down,
    Left,
    Right,
    Enter,
    Backspace,
    Escape,
    CtrlC,
    Char(char),
}

pub trait InputSource {
    fn poll_key(&mut self, timeout: Duration) -> io::Result<Option<Key>>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Style {
    pub inverted: bool,
    pub bold: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cell {
    pub ch: char,
    pub style: Style,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            ch: ' ',
            style: Style::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
}

impl Frame {
    pub fn new(width: usize, height: usize) -> Self {
        let cells = vec![Cell::default(); width.saturating_mul(height)];
        Self {
            width,
            height,
            cells,
        }
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn resize(&mut self, width: usize, height: usize) {
        self.width = width;
        self.height = height;
        self.cells = vec![Cell::default(); width.saturating_mul(height)];
    }

    pub fn clear(&mut self) {
        self.cells.fill(Cell::default());
    }

    pub fn set(&mut self, x: usize, y: usize, ch: char, style: Style) {
        if x >= self.width || y >= self.height {
            return;
        }
        let idx = y * self.width + x;
        self.cells[idx] = Cell { ch, style };
    }

    pub fn write_text(&mut self, x: usize, y: usize, text: &str, style: Style) {
        for (offset, ch) in text.chars().enumerate() {
            self.set(x + offset, y, ch, style);
        }
    }

    pub fn row(&self, y: usize) -> Option<&[Cell]> {
        if y >= self.height {
            return None;
        }
        let start = y * self.width;
        let end = start + self.width;
        Some(&self.cells[start..end])
    }
}

pub trait Renderer {
    fn size(&self) -> (usize, usize);
    fn render(&mut self, frame: &Frame) -> io::Result<()>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppCommand {
    Continue,
    Quit,
}

pub trait App {
    fn title(&self) -> &str;
    fn on_key(&mut self, key: Key) -> AppCommand;
    fn render(&self, frame: &mut Frame);
}

pub struct Runtime<R, I> {
    renderer: R,
    input: I,
    tick: Duration,
    frame: Frame,
}

impl<R, I> Runtime<R, I>
where
    R: Renderer,
    I: InputSource,
{
    pub fn new(renderer: R, input: I, tick: Duration) -> Self {
        let (width, height) = renderer.size();
        Self {
            renderer,
            input,
            tick,
            frame: Frame::new(width, height),
        }
    }

    pub fn run<A: App>(&mut self, app: &mut A) -> io::Result<()> {
        loop {
            let (width, height) = self.renderer.size();
            if width != self.frame.width() || height != self.frame.height() {
                self.frame.resize(width, height);
            }

            self.frame.clear();
            app.render(&mut self.frame);
            self.renderer.render(&self.frame)?;

            if let Some(key) = self.input.poll_key(self.tick)? {
                if app.on_key(key) == AppCommand::Quit {
                    return Ok(());
                }
            }
        }
    }
}
