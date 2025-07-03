use crate::pool::PoolId;
use crate::tables::{FileId, GlobalTables};

#[derive(Debug, PartialEq)]
pub struct Location {
    pub file_id: PoolId<FileId>,
    pub start_offset: usize,
    pub end_offset: usize,
}

impl Location {
    pub fn new(file_id: PoolId<FileId>, start_offset: usize, end_offset: usize) -> Self {
        Self { file_id, start_offset, end_offset }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{}:{}-{}", tables.files.get(&self.file_id).unwrap(), self.start_offset, self.end_offset)
    }
}

