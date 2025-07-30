use rusqlite::{Connection, params};
use std::{collections::HashMap, process::exit};

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

    #[allow(clippy::missing_panics_doc)]
    pub fn dump_to_cache(&self) {
        let mut conn = Connection::open("tmp/entries.db").unwrap();

        // Use transaction for batch insert
        let tx = conn.transaction().unwrap();
        {
            let mut stmt = tx
                .prepare("INSERT OR REPLACE INTO entries (name, value) VALUES (?1, ?2)")
                .unwrap();

            for entry in self.entries.values() {
                stmt.execute(params![entry.name, entry.value]).unwrap();
            }
        }
        tx.commit().unwrap();

        exit(0)
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
