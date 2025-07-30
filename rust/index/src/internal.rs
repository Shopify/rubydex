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
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    pub fn add_entry(&mut self, entry: Entry) {
        self.entries.insert(entry.name.clone(), entry);
    }

    #[must_use]
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
    #[must_use]
    pub fn new(name: String, value: String) -> Self {
        Self { name, value }
    }
}
