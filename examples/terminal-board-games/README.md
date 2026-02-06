# Terminal Board Games Workspace

A purpose-built, no-external-dependencies game engine for terminal board games.

## Goals

- Keyboard-controlled board games in terminal.
- Renderer abstraction so output targets can evolve (terminal now, others later).
- Platform abstraction so input/backend can expand beyond macOS.
- Start with Sudoku as the first fully playable game.

## Workspace Layout

- `crates/game-core`
  - Runtime loop, app trait, input trait, frame buffer, renderer trait.
- `crates/game-loop`
  - Fixed-step planning for deterministic update loops.
- `crates/game-ui-term`
  - Reusable terminal widgets (panels, labels, menus).
- `crates/game-save`
  - Snapshot and replay serialization utilities.
- `crates/game-ai`
  - Hint/solver traits and engine-facing AI primitives.
- `crates/platform-host`
  - Host-level input backend abstraction with macOS implementation selected by `cfg`.
- `crates/renderer-terminal`
  - ANSI terminal renderer (alternate screen + styled frame rendering).
- `crates/platform-macos`
  - macOS keyboard input and raw mode management (`stty` based).
- `crates/game-sudoku`
  - Sudoku board model + game logic + terminal view composition + snapshots + hints.
- `apps/sudoku-cli`
  - Executable wiring all crates together.

## Run

```bash
cd examples/terminal-board-games
cargo run -p sudoku-cli
```

## Controls

- Move: arrow keys or `WASD` or `HJKL`
- Set cell: `1`..`9`
- Clear cell: `0`, `Space`, `Backspace`
- Check status: `c`
- Hint: `Shift+H`
- Reset puzzle: `r`
- Next puzzle: `n`
- Save snapshot: `p`
- Load snapshot: `o`
- Quit: `q`, `Esc`, `Ctrl+C`

## Notes

- Current host input backend is macOS-first (`platform-host` -> `platform-macos`) and uses `/dev/tty` + `stty`.
- Core engine abstractions are platform/renderer-neutral for future Linux/Windows and non-terminal render targets.
