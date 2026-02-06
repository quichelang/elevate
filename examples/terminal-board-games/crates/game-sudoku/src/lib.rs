use std::env;
use std::path::PathBuf;

use game_core::{App, AppCommand, Frame, Key, Style};
use game_save::{Snapshot, read_snapshot, write_snapshot};
use game_ui_term::{Rect, draw_panel, draw_text};

const PUZZLES: [&str; 3] = [
    "530070000600195000098000060800060003400803001700020006060000280000419005000080079",
    "009000000080605020501078000000000700706040102004000000000720903090301080000000600",
    "000260701680070090190004500820100040004602900050003028009300074040050036703018000",
];

#[derive(Debug, Clone)]
pub struct SudokuBoard {
    cells: [u8; 81],
    fixed: [bool; 81],
}

impl SudokuBoard {
    pub fn from_puzzle(puzzle: &str) -> Self {
        let mut cells = [0u8; 81];
        let mut fixed = [false; 81];

        for (idx, ch) in puzzle.chars().take(81).enumerate() {
            let value = ch.to_digit(10).unwrap_or(0) as u8;
            cells[idx] = value;
            fixed[idx] = value != 0;
        }

        Self { cells, fixed }
    }

    pub fn get(&self, row: usize, col: usize) -> u8 {
        self.cells[row * 9 + col]
    }

    pub fn is_fixed(&self, row: usize, col: usize) -> bool {
        self.fixed[row * 9 + col]
    }

    pub fn clear(&mut self, row: usize, col: usize) -> bool {
        if self.is_fixed(row, col) {
            return false;
        }
        self.cells[row * 9 + col] = 0;
        true
    }

    pub fn set(&mut self, row: usize, col: usize, value: u8) -> bool {
        if !(1..=9).contains(&value) || self.is_fixed(row, col) {
            return false;
        }
        self.cells[row * 9 + col] = value;
        true
    }

    pub fn has_conflict(&self, row: usize, col: usize) -> bool {
        let value = self.get(row, col);
        if value == 0 {
            return false;
        }

        for c in 0..9 {
            if c != col && self.get(row, c) == value {
                return true;
            }
        }
        for r in 0..9 {
            if r != row && self.get(r, col) == value {
                return true;
            }
        }

        let box_row = (row / 3) * 3;
        let box_col = (col / 3) * 3;
        for r in box_row..box_row + 3 {
            for c in box_col..box_col + 3 {
                if (r != row || c != col) && self.get(r, c) == value {
                    return true;
                }
            }
        }

        false
    }

    pub fn is_complete(&self) -> bool {
        for row in 0..9 {
            for col in 0..9 {
                if self.get(row, col) == 0 || self.has_conflict(row, col) {
                    return false;
                }
            }
        }
        true
    }

    pub fn to_compact_string(&self) -> String {
        let mut out = String::with_capacity(81);
        for value in self.cells {
            out.push(char::from_digit(u32::from(value), 10).unwrap_or('0'));
        }
        out
    }

    pub fn apply_compact_string(&mut self, compact: &str) -> Result<(), String> {
        let mut chars = compact.chars();
        for idx in 0..81 {
            let ch = chars
                .next()
                .ok_or_else(|| "compact board state is too short".to_string())?;
            let value = ch
                .to_digit(10)
                .ok_or_else(|| format!("invalid compact board digit '{ch}'"))?
                as u8;

            if !self.fixed[idx] {
                self.cells[idx] = value;
            }
        }

        if chars.next().is_some() {
            return Err("compact board state is too long".to_string());
        }
        Ok(())
    }
}

pub struct SudokuGame {
    puzzle_index: usize,
    board: SudokuBoard,
    cursor_row: usize,
    cursor_col: usize,
    message: String,
}

impl Default for SudokuGame {
    fn default() -> Self {
        Self::new()
    }
}

impl SudokuGame {
    pub fn new() -> Self {
        let board = SudokuBoard::from_puzzle(PUZZLES[0]);
        Self {
            puzzle_index: 0,
            board,
            cursor_row: 0,
            cursor_col: 0,
            message: "Arrows/WASD/HJKL move, 1-9 set, 0 clear, c check, n next, r reset, p save, o load, q quit"
                .to_string(),
        }
    }

    fn save_path() -> PathBuf {
        env::temp_dir().join("elevate-sudoku.save")
    }

    fn load_current_puzzle(&mut self) {
        self.board = SudokuBoard::from_puzzle(PUZZLES[self.puzzle_index]);
        self.cursor_row = 0;
        self.cursor_col = 0;
    }

    fn move_cursor(&mut self, dr: isize, dc: isize) {
        self.cursor_row = ((self.cursor_row as isize + dr).rem_euclid(9)) as usize;
        self.cursor_col = ((self.cursor_col as isize + dc).rem_euclid(9)) as usize;
    }

    fn apply_digit(&mut self, digit: u8) {
        if self.board.set(self.cursor_row, self.cursor_col, digit) {
            self.message.clear();
        } else {
            self.message = "Cannot edit a fixed cell".to_string();
        }
    }

    fn clear_cell(&mut self) {
        if self.board.clear(self.cursor_row, self.cursor_col) {
            self.message.clear();
        } else {
            self.message = "Cannot clear a fixed cell".to_string();
        }
    }

    fn check_status(&mut self) {
        if self.board.is_complete() {
            self.message = "Solved! Press n for a new puzzle or q to quit".to_string();
            return;
        }
        if self.board.has_conflict(self.cursor_row, self.cursor_col) {
            self.message = "Current cell conflicts with row/column/box".to_string();
            return;
        }
        self.message = "No conflict in current cell. Keep going.".to_string();
    }

    fn make_snapshot(&self) -> Snapshot {
        let mut snapshot = Snapshot::new("sudoku-game", 1);
        snapshot.insert("puzzle_index", self.puzzle_index.to_string());
        snapshot.insert("cursor_row", self.cursor_row.to_string());
        snapshot.insert("cursor_col", self.cursor_col.to_string());
        snapshot.insert("cells", self.board.to_compact_string());
        snapshot
    }

    fn restore_snapshot(&mut self, snapshot: Snapshot) -> Result<(), String> {
        if snapshot.kind != "sudoku-game" {
            return Err(format!("unsupported snapshot kind '{}'", snapshot.kind));
        }
        if snapshot.version != 1 {
            return Err(format!(
                "unsupported snapshot version '{}'",
                snapshot.version
            ));
        }

        let puzzle_index = parse_usize_field(&snapshot, "puzzle_index")?;
        if puzzle_index >= PUZZLES.len() {
            return Err(format!(
                "snapshot puzzle_index {} out of range",
                puzzle_index
            ));
        }
        let cursor_row = parse_usize_field(&snapshot, "cursor_row")?.min(8);
        let cursor_col = parse_usize_field(&snapshot, "cursor_col")?.min(8);
        let cells = snapshot
            .get("cells")
            .ok_or_else(|| "snapshot missing 'cells'".to_string())?;

        self.puzzle_index = puzzle_index;
        self.load_current_puzzle();
        self.board.apply_compact_string(cells)?;
        self.cursor_row = cursor_row;
        self.cursor_col = cursor_col;
        Ok(())
    }

    fn draw_board(&self, frame: &mut Frame, start_x: usize, start_y: usize) {
        let normal = Style::default();
        let bold = Style {
            bold: true,
            inverted: false,
        };

        for row in 0..9 {
            if row % 3 == 0 {
                frame.write_text(
                    start_x,
                    start_y + row * 2,
                    "+-------+-------+-------+",
                    normal,
                );
            }

            let y = start_y + row * 2 + 1;
            frame.write_text(start_x, y, "|", normal);

            for col in 0..9 {
                let x = start_x + 1 + col * 2 + (col / 3);
                let value = self.board.get(row, col);
                let ch = if value == 0 {
                    '.'
                } else {
                    char::from_digit(u32::from(value), 10).unwrap_or('?')
                };

                let mut style = if self.board.is_fixed(row, col) {
                    bold
                } else {
                    normal
                };

                if row == self.cursor_row && col == self.cursor_col {
                    style.inverted = true;
                }

                if self.board.has_conflict(row, col) {
                    style.bold = true;
                }

                frame.set(x, y, ch, style);
            }

            frame.write_text(start_x + 8, y, "|", normal);
            frame.write_text(start_x + 16, y, "|", normal);
            frame.write_text(start_x + 24, y, "|", normal);
        }

        frame.write_text(start_x, start_y + 18, "+-------+-------+-------+", normal);
    }
}

impl App for SudokuGame {
    fn title(&self) -> &str {
        "Sudoku"
    }

    fn on_key(&mut self, key: Key) -> AppCommand {
        match key {
            Key::CtrlC => return AppCommand::Quit,
            Key::Char('q') | Key::Escape => return AppCommand::Quit,
            Key::Up | Key::Char('w') | Key::Char('k') => self.move_cursor(-1, 0),
            Key::Down | Key::Char('s') | Key::Char('j') => self.move_cursor(1, 0),
            Key::Left | Key::Char('a') | Key::Char('h') => self.move_cursor(0, -1),
            Key::Right | Key::Char('d') | Key::Char('l') => self.move_cursor(0, 1),
            Key::Char('n') => {
                self.puzzle_index = (self.puzzle_index + 1) % PUZZLES.len();
                self.load_current_puzzle();
                self.message = format!("Loaded puzzle {}", self.puzzle_index + 1);
            }
            Key::Char('r') => {
                self.load_current_puzzle();
                self.message = "Puzzle reset".to_string();
            }
            Key::Char('c') => self.check_status(),
            Key::Char('p') => {
                let path = Self::save_path();
                let snapshot = self.make_snapshot();
                match write_snapshot(&path, &snapshot) {
                    Ok(()) => self.message = format!("Saved game to {}", path.display()),
                    Err(err) => self.message = format!("Save failed: {err}"),
                }
            }
            Key::Char('o') => {
                let path = Self::save_path();
                match read_snapshot(&path) {
                    Ok(snapshot) => match self.restore_snapshot(snapshot) {
                        Ok(()) => self.message = format!("Loaded game from {}", path.display()),
                        Err(err) => self.message = format!("Load failed: {err}"),
                    },
                    Err(err) => self.message = format!("Load failed: {err}"),
                }
            }
            Key::Char('0') | Key::Char(' ') | Key::Backspace => self.clear_cell(),
            Key::Char(ch) if ('1'..='9').contains(&ch) => {
                self.apply_digit(ch.to_digit(10).unwrap_or(0) as u8)
            }
            _ => {}
        }

        AppCommand::Continue
    }

    fn render(&self, frame: &mut Frame) {
        let title_style = Style {
            bold: true,
            inverted: false,
        };
        draw_text(
            frame,
            2,
            1,
            "Elevate Terminal Board Engine :: Sudoku",
            title_style,
        );

        draw_panel(
            frame,
            Rect {
                x: 1,
                y: 2,
                width: 27,
                height: 21,
            },
            Some("Grid"),
            Style::default(),
        );
        self.draw_board(frame, 2, 3);

        let side_width = frame.width().saturating_sub(30);
        if side_width >= 24 {
            draw_panel(
                frame,
                Rect {
                    x: 30,
                    y: 2,
                    width: side_width,
                    height: 12,
                },
                Some("Controls"),
                Style::default(),
            );
            draw_text(frame, 32, 4, "Move: arrows / WASD / HJKL", Style::default());
            draw_text(frame, 32, 5, "Set: 1..9", Style::default());
            draw_text(
                frame,
                32,
                6,
                "Clear: 0 / Space / Backspace",
                Style::default(),
            );
            draw_text(frame, 32, 7, "Check: c", Style::default());
            draw_text(frame, 32, 8, "Next puzzle: n", Style::default());
            draw_text(frame, 32, 9, "Reset puzzle: r", Style::default());
            draw_text(frame, 32, 10, "Save: p   Load: o", Style::default());
            draw_text(frame, 32, 11, "Quit: q / Esc / Ctrl+C", Style::default());
        }

        draw_panel(
            frame,
            Rect {
                x: 1,
                y: 23,
                width: frame.width().saturating_sub(2),
                height: 3,
            },
            Some("Status"),
            Style::default(),
        );
        draw_text(frame, 3, 24, &self.message, Style::default());
    }
}

fn parse_usize_field(snapshot: &Snapshot, key: &str) -> Result<usize, String> {
    let raw = snapshot
        .get(key)
        .ok_or_else(|| format!("snapshot missing '{key}'"))?;
    raw.parse::<usize>()
        .map_err(|_| format!("snapshot field '{key}' has invalid integer '{raw}'"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn board_marks_fixed_cells() {
        let board = SudokuBoard::from_puzzle(PUZZLES[0]);
        assert!(board.is_fixed(0, 0));
        assert!(!board.is_fixed(0, 2));
    }

    #[test]
    fn board_detects_conflict() {
        let mut board = SudokuBoard::from_puzzle(PUZZLES[0]);
        assert!(board.set(0, 2, 5));
        assert!(board.has_conflict(0, 2));
    }

    #[test]
    fn fixed_cells_cannot_be_changed() {
        let mut board = SudokuBoard::from_puzzle(PUZZLES[0]);
        assert!(!board.set(0, 0, 9));
        assert_eq!(board.get(0, 0), 5);
    }

    #[test]
    fn complete_board_is_detected() {
        let solved =
            "534678912672195348198342567859761423426853791713924856961537284287419635345286179";
        let board = SudokuBoard::from_puzzle(solved);
        assert!(board.is_complete());
    }

    #[test]
    fn compact_board_roundtrip_preserves_values() {
        let mut board = SudokuBoard::from_puzzle(PUZZLES[1]);
        assert!(board.set(0, 0, 3));
        assert!(board.set(0, 1, 4));

        let compact = board.to_compact_string();
        let mut restored = SudokuBoard::from_puzzle(PUZZLES[1]);
        restored
            .apply_compact_string(&compact)
            .expect("apply compact state");

        assert_eq!(restored.to_compact_string(), compact);
    }

    #[test]
    fn snapshot_roundtrip_restores_game_state() {
        let mut original = SudokuGame::new();
        original.cursor_row = 0;
        original.cursor_col = 2;
        original.apply_digit(7);

        let snapshot = original.make_snapshot();
        let mut restored = SudokuGame::new();
        restored.restore_snapshot(snapshot).expect("restore");

        assert_eq!(restored.puzzle_index, original.puzzle_index);
        assert_eq!(restored.cursor_row, original.cursor_row);
        assert_eq!(restored.cursor_col, original.cursor_col);
        assert_eq!(
            restored.board.to_compact_string(),
            original.board.to_compact_string()
        );
    }
}
