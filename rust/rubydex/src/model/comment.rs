use crate::offset::Offset;

#[derive(Debug, Clone)]
pub struct Comment {
    offset: Offset,
    string: String,
}

impl Comment {
    #[must_use]
    pub fn new(offset: Offset, string: String) -> Self {
        Self { offset, string }
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn string(&self) -> &String {
        &self.string
    }
}
