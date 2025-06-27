#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub file: String, // This is dumb, we're wasting memory by storing the file name for each location
    pub start_offset: usize,
    pub end_offset: usize,
}


impl Location {
    pub fn new(file: String, start_offset: usize, end_offset: usize) -> Self {
        Self { file, start_offset, end_offset }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}-{}", self.file, self.start_offset, self.end_offset)
    }
}

