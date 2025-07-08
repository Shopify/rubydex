use std::collections::HashMap;

pub struct Repository {
    pub entries: HashMap<String, Entry>,
}

impl Default for Repository {
    fn default() -> Self {
        Self::new()
    }
}

impl Repository {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    pub fn add_entry(&mut self, entry: Entry) {
        self.entries.insert(entry.name.clone(), entry);
    }

    pub fn get_entry(&self, key: &str) -> Option<&Entry> {
        self.entries.get(key)
    }
}

#[derive(Clone)]
pub struct Entry {
    pub name: String,
    pub value: String,
}

impl Entry {
    pub fn new(name: String, value: String) -> Self {
        Self { name, value }
    }
}

// Simple Point struct for benchmarking object creation
#[derive(Clone)]
pub struct Point {
    pub x: u32,
    pub y: u32,
}

impl Point {
    pub fn new(x: u32, y: u32) -> Self {
        Self { x, y }
    }
}
