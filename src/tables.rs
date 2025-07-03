use super::pool::Pool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId;

/// Global manager for all string tables
#[derive(Debug)]
pub struct GlobalTables {
    /// File path strings table
    pub files: Pool<FileId, String>,
    /// Symbol name strings table
    pub names: Pool<NameId, String>,
}

impl GlobalTables {
    /// Create new empty global tables
    pub fn new() -> Self {
        Self {
            files: Pool::new(),
            names: Pool::new(),
        }
    }

    // /// Get statistics about memory usage
    // pub fn stats(&self) -> GlobalStats {
    //     GlobalStats {
    //         file_count: self.files.size(),
    //         name_count: self.names.size(),
    //         total_strings: self.files.size() + self.names.size(),
    //     }
    // }
}

impl Default for GlobalTables {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about global table usage
#[derive(Debug, Clone)]
pub struct GlobalStats {
    pub file_count: usize,
    pub name_count: usize,
    pub total_strings: usize,
}

impl std::fmt::Display for GlobalStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Global Tables Statistics:")?;
        writeln!(f, "  File paths: {}", self.file_count)?;
        writeln!(f, "  Symbol names: {}", self.name_count)?;
        writeln!(f, "  Total strings: {}", self.total_strings)?;
        Ok(())
    }
}