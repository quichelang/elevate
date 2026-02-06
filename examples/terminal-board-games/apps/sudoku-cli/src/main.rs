use std::io;
use std::time::Duration;

use game_core::Runtime;
use game_sudoku::SudokuGame;
use platform_host::{HostInput, platform_name};
use renderer_terminal::TerminalRenderer;

fn main() -> io::Result<()> {
    let renderer = TerminalRenderer::new()?;
    let input = HostInput::new().map_err(|err| {
        io::Error::new(
            err.kind(),
            format!(
                "unable to initialize host input backend ({}): {}",
                platform_name(),
                err
            ),
        )
    })?;

    let mut runtime = Runtime::new(renderer, input, Duration::from_millis(33));
    let mut game = SudokuGame::new();
    runtime.run(&mut game)
}
