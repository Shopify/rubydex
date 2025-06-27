use std::collections::HashMap;
use std::rc::Rc;

/// A pool of unique items stored by indexed to avoid duplication. Pools can be used to store unique names, like
/// `Foo::Bar#method`, file paths or any unique value we need to store avoiding cloning and minimizing memory usage.
///
/// We use two hashes with reference counters to guarantee uniqueness without cloning and fast lookups both ways
pub struct Pool<T: Eq + std::hash::Hash> {
    id_to_entry: HashMap<usize, Rc<T>>,
    entry_to_id: HashMap<Rc<T>, usize>,
    next_id: usize,
}

impl<T: Eq + std::hash::Hash> Pool<T> {
    pub fn new() -> Self {
        Pool {
            id_to_entry: HashMap::new(),
            entry_to_id: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn add(&mut self, item: T) -> usize {
        // If the item already exists, skip adding it and just return its ID
        if let Some(&id) = self.entry_to_id.get(&item) {
            return id;
        }

        let item_rc = Rc::new(item);

        let id = self.next_id;
        self.id_to_entry.insert(id, item_rc.clone());
        self.entry_to_id.insert(item_rc, id);
        self.next_id += 1;
        id
    }

    pub fn get(&self, id: usize) -> Option<&T> {
        self.id_to_entry.get(&id).map(|rc| rc.as_ref())
    }

    pub fn remove(&mut self, id: usize) {
        if let Some(entry) = self.id_to_entry.remove(&id) {
            self.entry_to_id.remove(&entry);
        }
    }

    pub fn size(&self) -> usize {
        self.id_to_entry.len()
    }
}

impl<T: Eq + std::hash::Hash> Default for Pool<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adding_and_fetching_entries() {
        let mut pool: Pool<String> = Pool::new();
        let id = pool.add("Foo::Bar#method".into());
        let item = pool.get(id).unwrap();
        assert_eq!(*item, "Foo::Bar#method".to_string());
    }

    #[test]
    fn test_adding_the_same_entry_just_returns_id() {
        let mut pool: Pool<String> = Pool::new();
        let id = pool.add("Foo::Bar#method".into());
        assert_eq!(1, pool.size());
        let other_id = pool.add("Foo::Bar#method".into());
        assert_eq!(1, pool.size());
        assert_eq!(id, other_id);
    }

    #[test]
    fn test_removing_entries() {
        let mut pool: Pool<String> = Pool::new();
        let id = pool.add("Foo::Bar#method".into());
        pool.remove(id);
        assert_eq!(pool.get(id), None);
    }
}
