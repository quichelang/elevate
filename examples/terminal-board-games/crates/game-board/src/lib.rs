#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Grid<T> {
    width: usize,
    height: usize,
    cells: Vec<T>,
}

impl<T: Clone> Grid<T> {
    pub fn new(width: usize, height: usize, fill: T) -> Self {
        Self {
            width,
            height,
            cells: vec![fill; width.saturating_mul(height)],
        }
    }
}

impl<T> Grid<T> {
    pub fn from_vec(width: usize, height: usize, cells: Vec<T>) -> Result<Self, String> {
        let expected = width.saturating_mul(height);
        if cells.len() != expected {
            return Err(format!(
                "invalid cell count: expected {expected}, got {}",
                cells.len()
            ));
        }
        Ok(Self {
            width,
            height,
            cells,
        })
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn in_bounds(&self, pos: Position) -> bool {
        pos.row < self.height && pos.col < self.width
    }

    pub fn index_of(&self, pos: Position) -> Option<usize> {
        if !self.in_bounds(pos) {
            return None;
        }
        Some(pos.row * self.width + pos.col)
    }

    pub fn get(&self, pos: Position) -> Option<&T> {
        let idx = self.index_of(pos)?;
        self.cells.get(idx)
    }

    pub fn get_mut(&mut self, pos: Position) -> Option<&mut T> {
        let idx = self.index_of(pos)?;
        self.cells.get_mut(idx)
    }

    pub fn set(&mut self, pos: Position, value: T) -> Result<(), String> {
        let Some(slot) = self.get_mut(pos) else {
            return Err(format!(
                "position out of bounds: row={}, col={}",
                pos.row, pos.col
            ));
        };
        *slot = value;
        Ok(())
    }

    pub fn row_slice(&self, row: usize) -> Option<&[T]> {
        if row >= self.height {
            return None;
        }
        let start = row * self.width;
        let end = start + self.width;
        self.cells.get(start..end)
    }

    pub fn column_refs(&self, col: usize) -> Option<Vec<&T>> {
        if col >= self.width {
            return None;
        }

        let mut out = Vec::with_capacity(self.height);
        for row in 0..self.height {
            let pos = Position { row, col };
            if let Some(value) = self.get(pos) {
                out.push(value);
            }
        }
        Some(out)
    }

    pub fn positions(&self) -> impl Iterator<Item = Position> + '_ {
        (0..self.height).flat_map(move |row| (0..self.width).map(move |col| Position { row, col }))
    }

    pub fn neighbors4(&self, pos: Position) -> [Option<Position>; 4] {
        let up = (pos.row > 0).then_some(Position {
            row: pos.row.saturating_sub(1),
            col: pos.col,
        });
        let down = (pos.row + 1 < self.height).then_some(Position {
            row: pos.row + 1,
            col: pos.col,
        });
        let left = (pos.col > 0).then_some(Position {
            row: pos.row,
            col: pos.col.saturating_sub(1),
        });
        let right = (pos.col + 1 < self.width).then_some(Position {
            row: pos.row,
            col: pos.col + 1,
        });

        [up, down, left, right]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_and_get_cell() {
        let mut grid = Grid::new(3, 2, 0u8);
        grid.set(Position { row: 1, col: 2 }, 9).expect("set");
        assert_eq!(grid.get(Position { row: 1, col: 2 }), Some(&9));
    }

    #[test]
    fn row_and_column_access() {
        let grid = Grid::from_vec(3, 2, vec![1, 2, 3, 4, 5, 6]).expect("grid");
        assert_eq!(grid.row_slice(0), Some(&[1, 2, 3][..]));

        let column = grid.column_refs(1).expect("column");
        assert_eq!(*column[0], 2);
        assert_eq!(*column[1], 5);
    }

    #[test]
    fn neighbors_clip_to_bounds() {
        let grid = Grid::new(2, 2, 0u8);
        let neighbors = grid.neighbors4(Position { row: 0, col: 0 });
        assert_eq!(neighbors[0], None);
        assert_eq!(neighbors[2], None);
        assert_eq!(neighbors[1], Some(Position { row: 1, col: 0 }));
        assert_eq!(neighbors[3], Some(Position { row: 0, col: 1 }));
    }
}
