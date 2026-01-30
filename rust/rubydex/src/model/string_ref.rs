use std::ops::Deref;

/// A reference-counted string used in the graph.
///
/// This struct wraps a `String` with a reference count to track how many times
/// the string is used across the graph. When a document is removed, we decrement
/// the reference count for each string it uses, and remove the string from the
/// graph when its count reaches zero.
#[derive(Debug)]
pub struct StringRef {
    value: String,
    ref_count: u32,
}

impl StringRef {
    #[must_use]
    pub fn new(value: String) -> Self {
        Self { value, ref_count: 1 }
    }

    #[must_use]
    pub fn ref_count(&self) -> u32 {
        self.ref_count
    }

    /// # Panics
    ///
    /// This function will panic if the reference count would exceed `u32::MAX`
    pub fn increment_ref_count(&mut self, count: u32) {
        self.ref_count = self
            .ref_count
            .checked_add(count)
            .expect("Should not exceed maximum string ref count");
    }

    #[must_use]
    pub fn decrement_ref_count(&mut self) -> bool {
        debug_assert!(self.ref_count > 0);
        self.ref_count -= 1;
        self.ref_count > 0
    }
}

impl Deref for StringRef {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
