use crate::tables::string_pool::{PoolId, StringPool};
use std::sync::{Arc, Mutex, OnceLock};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl PoolId for FileId {
    fn from_index(index: u32) -> Self { Self(index) }
    fn to_index(self) -> u32 { self.0 }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId(u32);

impl PoolId for NameId {
    fn from_index(index: u32) -> Self { Self(index) }
    fn to_index(self) -> u32 { self.0 }
}


/// Global manager for all string tables
#[derive(Debug)]
pub struct GlobalTables {
    /// File path strings table
    pub files: StringPool<FileId>,
    /// Symbol name strings table
    pub names: StringPool<NameId>,
}

impl GlobalTables {
    /// Create new empty global tables
    pub fn new() -> Self {
        Self {
            files: StringPool::new(),
            names: StringPool::new(),
        }
    }

    /// Intern a file path, returning its ID
    pub fn intern_file(&mut self, path: &str) -> FileId {
        self.files.intern(path)
    }

    /// Intern a symbol name, returning its ID
    pub fn intern_name(&mut self, name: &str) -> NameId {
        self.names.intern(name)
    }

    /// Get a file path by ID
    pub fn get_file(&self, id: FileId) -> Option<&str> {
        self.files.get(id)
    }

    /// Get a symbol name by ID
    pub fn get_name(&self, id: NameId) -> Option<&str> {
        self.names.get(id)
    }

    /// Get statistics about memory usage
    pub fn stats(&self) -> GlobalStats {
        GlobalStats {
            file_count: self.files.len(),
            name_count: self.names.len(),
            total_strings: self.files.len() + self.names.len(),
        }
    }

    /// Clear all tables
    pub fn clear(&mut self) {
        self.files.clear();
        self.names.clear();
    }
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

/// Global tables instance
static GLOBAL: OnceLock<Arc<Mutex<GlobalTables>>> = OnceLock::new();

/// Initialize the global tables
pub fn init_global_tables() {
    GLOBAL
        .set(Arc::new(Mutex::new(GlobalTables::new())))
        .expect("Global tables already initialized");
}

/// Execute a function with access to the global tables
pub fn with_global_tables<R>(f: impl FnOnce(&mut GlobalTables) -> R) -> R {
    let tables = GLOBAL.get_or_init(|| Arc::new(Mutex::new(GlobalTables::new())));
    let mut guard = tables.lock().unwrap();
    f(&mut *guard)
}

/// Intern a file path using the global tables
pub fn intern_file(path: &str) -> FileId {
    with_global_tables(|tables| tables.intern_file(path))
}

/// Intern a symbol name using the global tables
pub fn intern_name(name: &str) -> NameId {
    with_global_tables(|tables| tables.intern_name(name))
}

/// Get a file path from the global tables
pub fn get_file(id: FileId) -> Option<String> {
    with_global_tables(|tables| tables.get_file(id).map(|s| s.to_string()))
}

/// Get a symbol name from the global tables
pub fn get_name(id: NameId) -> Option<String> {
    with_global_tables(|tables| tables.get_name(id).map(|s| s.to_string()))
}

/// Execute a function with a file path from the global tables
pub fn with_file<R>(id: FileId, f: impl FnOnce(&str) -> R) -> Option<R> {
    with_global_tables(|tables| tables.get_file(id).map(f))
}

/// Execute a function with a symbol name from the global tables
pub fn with_name<R>(id: NameId, f: impl FnOnce(&str) -> R) -> Option<R> {
    with_global_tables(|tables| tables.get_name(id).map(f))
}

/// Get global tables statistics
pub fn global_stats() -> GlobalStats {
    with_global_tables(|tables| tables.stats())
}
