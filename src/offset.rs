use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
    pub start_offset: u32,
    pub end_offset: u32,
}

impl Offset {
    pub fn new(start_offset: u32, end_offset: u32) -> Self {
        Self { start_offset, end_offset }
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start_offset, self.end_offset)
    }
}