use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Pos {
    row: usize,
    col: usize,
}

impl Pos {
    pub fn newline(&mut self) {
        self.row = 0;
        self.col += 1;
    }

    pub fn right_shift(&mut self) {
        self.row += 1;
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.col, self.row)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Span {
    path: String,
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn new(path: &str, start: Pos, end: Pos) -> Self {
        Self {
            path: path.into(),
            start,
            end,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.path, self.start)
    }
}
