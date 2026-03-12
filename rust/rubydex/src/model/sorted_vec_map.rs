use std::cell::UnsafeCell;

/// A sorted Vec-based map for compact storage of bulk-loaded key-value pairs.
///
/// Uses a flat `Vec<(K, V)>` sorted by key for O(log n) lookups.
/// Much more memory-efficient than `HashMap` for large collections that are
/// bulk-loaded then mostly read, with occasional removals.
///
/// # When to use
///
/// Use this instead of `HashMap` when **all** of the following are true:
/// - The collection is large (millions of entries) — small maps don't benefit
/// - Entries are bulk-loaded (e.g., during indexing), not inserted one-at-a-time in hot loops
/// - Lookups are infrequent or off the critical path (O(log n) binary search, not O(1))
///
/// **Do not use** for collections accessed in tight loops during resolution or
/// other hot paths. Binary search on large arrays causes cache misses that
/// make it slower than `HashMap` despite the memory savings. For example,
/// `constant_references` (5.64M entries, queried millions of times during
/// resolution) showed a 24% time regression when switched to `SortedVecMap`.
///
/// # Implementation
///
/// Uses interior mutability for lazy sorting: `get()` takes `&self` and sorts
/// on first access if needed. This is safe because sorting is idempotent and
/// the map is only accessed from a single thread at a time.
pub struct SortedVecMap<K, V> {
    // UnsafeCell allows mutation through &self for lazy sorting.
    // Safety: rubydex's Graph is single-threaded for mutations.
    inner: UnsafeCell<SortedVecMapInner<K, V>>,
}

struct SortedVecMapInner<K, V> {
    entries: Vec<(K, V)>,
    sorted: bool,
}

// SAFETY: SortedVecMap is only used within Graph which is single-threaded for mutations.
// The UnsafeCell is only mutated during lazy sorting which is idempotent.
// Sync is needed because Graph is stored in Arc<Mutex<>> in the MCP server.
unsafe impl<K: Send, V: Send> Send for SortedVecMap<K, V> {}
unsafe impl<K: Send + Sync, V: Send + Sync> Sync for SortedVecMap<K, V> {}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for SortedVecMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = unsafe { &*self.inner.get() };
        f.debug_struct("SortedVecMap")
            .field("entries", &inner.entries)
            .field("sorted", &inner.sorted)
            .finish()
    }
}

impl<K, V> Default for SortedVecMap<K, V> {
    fn default() -> Self {
        Self {
            inner: UnsafeCell::new(SortedVecMapInner {
                entries: Vec::new(),
                sorted: true,
            }),
        }
    }
}

impl<K: Ord + Copy, V> SortedVecMap<K, V> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Ensure the entries are sorted.
    fn ensure_sorted(&self) {
        let inner = unsafe { &mut *self.inner.get() };
        if !inner.sorted {
            inner.entries.sort_unstable_by_key(|(k, _)| *k);
            inner.sorted = true;
        }
    }

    fn inner(&self) -> &SortedVecMapInner<K, V> {
        unsafe { &*self.inner.get() }
    }

    /// Push a new entry without maintaining sort order.
    pub fn push(&mut self, key: K, value: V) {
        let inner = self.inner.get_mut();
        inner.entries.push((key, value));
        inner.sorted = false;
    }

    /// Insert a key-value pair, returning the previous value if the key already existed.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.ensure_sorted();
        let inner = self.inner.get_mut();
        match inner.entries.binary_search_by_key(&key, |(k, _)| *k) {
            Ok(index) => {
                let old = std::mem::replace(&mut inner.entries[index].1, value);
                Some(old)
            }
            Err(index) => {
                inner.entries.insert(index, (key, value));
                None
            }
        }
    }

    /// Look up a value by key. Lazily sorts if needed.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.ensure_sorted();
        let inner = self.inner();
        inner
            .entries
            .binary_search_by_key(key, |(k, _)| *k)
            .ok()
            .map(|index| &inner.entries[index].1)
    }

    /// Remove an entry by key, returning the value if found.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.ensure_sorted();
        let inner = self.inner.get_mut();
        match inner.entries.binary_search_by_key(key, |(k, _)| *k) {
            Ok(index) => Some(inner.entries.remove(index).1),
            Err(_) => None,
        }
    }

    pub fn len(&self) -> usize {
        self.inner().entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner().entries.is_empty()
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.inner().entries.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.inner().entries.iter().map(|(_, v)| v)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner().entries.iter().map(|(k, v)| (k, v))
    }

    pub fn shrink_to_fit(&mut self) {
        self.inner.get_mut().entries.shrink_to_fit();
    }

    /// Sort the map if not already sorted.
    pub fn sort(&mut self) {
        self.ensure_sorted();
    }
}
