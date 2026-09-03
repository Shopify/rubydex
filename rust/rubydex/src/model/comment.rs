use crate::assert_mem_size;
use crate::offset::Offset;

#[derive(Debug, Clone)]
pub struct Comment {
    offset: Offset,
    string: Box<str>,
}
assert_mem_size!(Comment, 24);

impl Comment {
    #[must_use]
    pub fn new(offset: Offset, string: Box<str>) -> Self {
        Self { offset, string }
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn string(&self) -> &str {
        &self.string
    }
}
