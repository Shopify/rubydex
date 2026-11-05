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

/// Consecutive source comments that are not attached to a definition.
#[derive(Debug)]
pub struct CommentGroup {
    comments: Box<[Comment]>,
}
assert_mem_size!(CommentGroup, 16);

impl CommentGroup {
    #[must_use]
    pub fn new(comments: Box<[Comment]>) -> Self {
        Self { comments }
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }
}
