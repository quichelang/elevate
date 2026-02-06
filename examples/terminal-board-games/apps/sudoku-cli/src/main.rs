use std::io;
use std::time::Duration;

use game_core::Runtime;
use game_sudoku::SudokuGame;
use platform_macos::MacOsInput;
use renderer_terminal::TerminalRenderer;

fn main() -> io::Result<()> {
    let renderer = TerminalRenderer::new()?;
    let input = MacOsInput::new()?;

    let mut runtime = Runtime::new(renderer, input, Duration::from_millis(33));
    let mut game = SudokuGame::new();
    runtime.run(&mut game)
}
