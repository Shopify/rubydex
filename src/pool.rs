use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;

/// A pool of unique items stored by indexed to avoid duplication. Pools can be used to store unique names, like
/// `Foo::Bar#method`, file paths or any unique value we need to store avoiding cloning and minimizing memory usage.
///
/// We use two hashes with reference counters to guarantee uniqueness without cloning and fast lookups both ways. We
/// also require defining an ID type for the pool, so that IDs belonging to different pools cannot be mixed up and a
/// type error will be thrown during compilation.
///
/// ```
/// # #[derive(PartialEq, Debug)]
/// # pub struct NameId;
/// # #[derive(PartialEq, Debug)]
/// # pub struct UriId;
/// #
/// # let mut name_pool = Pool::<NameId, String>::new();
/// # let mut uri_pool = Pool::<UriId, String>::new();
/// # let name_id = name_pool.add("Foo::Bar#method".into());
/// # uri_pool.get(&name_id) // Not possible. This will fail to compile because of the ID types
/// ```
#[derive(Debug)]
pub struct Pool<I, T: Eq + std::hash::Hash> {
    id_to_entry: HashMap<usize, Rc<T>>,
    entry_to_id: HashMap<Rc<T>, usize>,
    next_id: usize,
    _phantom: PhantomData<I>,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub struct PoolId<I> {
    value: usize,
    _phantom: PhantomData<I>,
}

impl<I: PartialEq> From<usize> for PoolId<I> {
    fn from(id: usize) -> Self {
        Self {
            value: id,
            _phantom: PhantomData,
        }
    }
}

impl<I: PartialEq> From<PoolId<I>> for usize {
    fn from(id: PoolId<I>) -> Self {
        id.value
    }
}

impl<I: PartialEq, T: Eq + std::hash::Hash> Pool<I, T> {
    pub fn new() -> Self {
        Pool {
            id_to_entry: HashMap::new(),
            entry_to_id: HashMap::new(),
            next_id: 0,
            _phantom: PhantomData,
        }
    }

    pub fn add(&mut self, item: T) -> PoolId<I> {
        // If the item already exists, skip adding it and just return its ID
        if let Some(&id) = self.entry_to_id.get(&item) {
            return id.into();
        }

        let item_rc = Rc::new(item);

        let id = self.next_id;
        self.id_to_entry.insert(id, item_rc.clone());
        self.entry_to_id.insert(item_rc, id);
        self.next_id += 1;
        id.into()
    }

    pub fn get(&self, id: &PoolId<I>) -> Option<&T> {
        self.id_to_entry.get(&id.value).map(|rc| rc.as_ref())
    }

    // pub fn remove(&mut self, id: &PoolId<I>) {
    //     if let Some(entry) = self.id_to_entry.remove(&id.value) {
    //         self.entry_to_id.remove(&entry);
    //     }
    // }

    // pub fn size(&self) -> usize {
    //     self.id_to_entry.len()
    // }
}

impl<I: PartialEq, T: Eq + std::hash::Hash> Default for Pool<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Debug)]
    pub struct UniqueNameId;

    #[derive(PartialEq, Debug)]
    pub struct OtherId;

    #[test]
    fn test_adding_and_fetching_entries() {
        let mut pool: Pool<UniqueNameId, String> = Pool::new();
        let id = pool.add("Foo::Bar#method".into());
        let item = pool.get(&id).unwrap();
        assert_eq!(*item, "Foo::Bar#method".to_string());
    }

    // #[test]
    // fn test_adding_the_same_entry_just_returns_id() {
    //     let mut pool: Pool<UniqueNameId, String> = Pool::new();
    //     let id = pool.add("Foo::Bar#method".into());
    //     assert_eq!(1, pool.size());
    //     let other_id = pool.add("Foo::Bar#method".into());
    //     assert_eq!(1, pool.size());
    //     assert_eq!(id, other_id);
    // }

    // #[test]
    // fn test_removing_entries() {
    //     let mut pool: Pool<UniqueNameId, String> = Pool::new();
    //     let id = pool.add("Foo::Bar#method".into());
    //     pool.remove(&id);
    //     assert_eq!(pool.get(&id), None);
    // }
}
