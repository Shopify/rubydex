use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

/// Trait for string pool ID types
pub trait PoolId: Debug + Clone + Copy + PartialEq + Eq + Hash {
    /// Create a new ID from a u32 index
    fn from_index(index: u32) -> Self;

    /// Get the u32 index from this ID
    fn to_index(self) -> u32;
}

/// Generic string pool for memory-efficient string storage with deduplication
#[derive(Debug)]
pub struct StringPool<Id: PoolId> {
    /// All stored strings, indexed by ID
    strings: Vec<String>,
    /// Map from string content to its ID for fast lookup
    lookup: HashMap<String, Id>,
}

impl<Id: PoolId> StringPool<Id> {
    /// Create a new empty string pool
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    /// Add a string to the pool, returning its ID
    /// If the string already exists, returns the existing ID
    pub fn intern(&mut self, string: &str) -> Id {
        if let Some(&id) = self.lookup.get(string) {
            return id;
        }

        let id = Id::from_index(self.strings.len() as u32);
        self.strings.push(string.to_string());
        self.lookup.insert(string.to_string(), id);
        id
    }

    /// Add a String to the pool, returning its ID
    /// More efficient than intern() when you already have a String
    pub fn intern_owned(&mut self, string: String) -> Id {
        if let Some(&id) = self.lookup.get(&string) {
            return id;
        }

        let id = Id::from_index(self.strings.len() as u32);
        self.lookup.insert(string.clone(), id);
        self.strings.push(string);
        id
    }

    /// Get the string for a given ID
    pub fn get(&self, id: Id) -> Option<&str> {
        self.strings.get(id.to_index() as usize).map(|s| s.as_str())
    }

    /// Get the string for a given ID (panics if invalid)
    pub fn get_unchecked(&self, id: Id) -> &str {
        &self.strings[id.to_index() as usize]
    }

    /// Check if a string is already in the pool
    pub fn contains(&self, string: &str) -> bool {
        self.lookup.contains_key(string)
    }

    /// Get the ID for a string if it exists
    pub fn find(&self, string: &str) -> Option<Id> {
        self.lookup.get(string).copied()
    }

    /// Number of unique strings in the pool
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Check if the pool is empty
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Clear all strings from the pool
    pub fn clear(&mut self) {
        self.strings.clear();
        self.lookup.clear();
    }
}

impl<Id: PoolId> Default for StringPool<Id> {
    fn default() -> Self {
        Self::new()
    }
}
