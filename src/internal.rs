use std::collections::HashMap;

pub struct Repository {
    pub entries: HashMap<String, Entry>,
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
    pub member: Member,
}

impl Entry {
    pub fn new(name: String, value: String) -> Self {
        Self {
            name,
            value,
            member: Member::new(String::from("member_value")),
        }
    }
}

#[derive(Clone)]
pub struct Member {
    pub value: String,
    pub nested_member: NestedMember,
}

impl Member {
    pub fn new(value: String) -> Self {
        Self {
            value,
            nested_member: NestedMember::new("nested_value".to_string()),
        }
    }
}

#[derive(Clone)]
pub struct NestedMember {
    pub value: String,
}

impl NestedMember {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}
