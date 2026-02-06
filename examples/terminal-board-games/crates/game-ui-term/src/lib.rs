use game_core::{Frame, Key, Style};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rect {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize,
}

impl Rect {
    pub fn inset(&self, pad: usize) -> Rect {
        let width = self.width.saturating_sub(pad * 2);
        let height = self.height.saturating_sub(pad * 2);
        Rect {
            x: self.x + pad,
            y: self.y + pad,
            width,
            height,
        }
    }
}

pub fn draw_panel(frame: &mut Frame, rect: Rect, title: Option<&str>, style: Style) {
    if rect.width < 2 || rect.height < 2 {
        return;
    }

    let x2 = rect.x + rect.width - 1;
    let y2 = rect.y + rect.height - 1;

    frame.set(rect.x, rect.y, '+', style);
    frame.set(x2, rect.y, '+', style);
    frame.set(rect.x, y2, '+', style);
    frame.set(x2, y2, '+', style);

    for x in rect.x + 1..x2 {
        frame.set(x, rect.y, '-', style);
        frame.set(x, y2, '-', style);
    }
    for y in rect.y + 1..y2 {
        frame.set(rect.x, y, '|', style);
        frame.set(x2, y, '|', style);
    }

    if let Some(title) = title {
        let available = rect.width.saturating_sub(4);
        if available > 0 {
            let clipped = clip_text(title, available);
            frame.write_text(rect.x + 2, rect.y, &clipped, style);
        }
    }
}

pub fn draw_text(frame: &mut Frame, x: usize, y: usize, text: &str, style: Style) {
    frame.write_text(x, y, text, style);
}

#[derive(Debug, Clone)]
pub struct Menu {
    entries: Vec<String>,
    selected: usize,
}

impl Menu {
    pub fn new(entries: Vec<String>) -> Self {
        Self {
            entries,
            selected: 0,
        }
    }

    pub fn selected_index(&self) -> usize {
        self.selected
    }

    pub fn selected_entry(&self) -> Option<&str> {
        self.entries.get(self.selected).map(String::as_str)
    }

    pub fn on_key(&mut self, key: Key) {
        if self.entries.is_empty() {
            return;
        }

        match key {
            Key::Up | Key::Char('k') | Key::Char('w') => {
                self.selected = (self.selected + self.entries.len() - 1) % self.entries.len();
            }
            Key::Down | Key::Char('j') | Key::Char('s') => {
                self.selected = (self.selected + 1) % self.entries.len();
            }
            _ => {}
        }
    }

    pub fn render(&self, frame: &mut Frame, area: Rect, style: Style, selected_style: Style) {
        let max_rows = area.height;
        for (idx, entry) in self.entries.iter().take(max_rows).enumerate() {
            let marker = if idx == self.selected { '>' } else { ' ' };
            let rendered = format!("{} {}", marker, entry);
            let clipped = clip_text(&rendered, area.width);
            let row_style = if idx == self.selected {
                selected_style
            } else {
                style
            };
            frame.write_text(area.x, area.y + idx, &clipped, row_style);
        }
    }
}

fn clip_text(text: &str, max_chars: usize) -> String {
    text.chars().take(max_chars).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn panel_draws_corners() {
        let mut frame = Frame::new(20, 10);
        draw_panel(
            &mut frame,
            Rect {
                x: 1,
                y: 1,
                width: 10,
                height: 5,
            },
            Some("T"),
            Style::default(),
        );

        let top_left = frame.row(1).expect("row")[1];
        let bottom_right = frame.row(5).expect("row")[10];
        assert_eq!(top_left.ch, '+');
        assert_eq!(bottom_right.ch, '+');
    }

    #[test]
    fn menu_selection_wraps() {
        let mut menu = Menu::new(vec!["A".into(), "B".into(), "C".into()]);
        menu.on_key(Key::Up);
        assert_eq!(menu.selected_index(), 2);
        menu.on_key(Key::Down);
        assert_eq!(menu.selected_index(), 0);
    }
}
